use std::cmp;
use std::mem::size_of;
use std::io::IoResult;

use super::{FlacResult, FramingError, IoError};

/// Bitstream unpacker.
///
/// Unpacks bits from a byte stream, MSb-first (big-endian).
pub trait BitReader: Reader {
    /// Read `n` bits.
    ///
    /// `n` must be less than or equal to the bit width of `T`.
    fn read_bits<T: Primitive>(&mut self, n: u8) -> IoResult<T>;

    /// Realign the underlying stream to the next byte boundary.
    /// 
    /// Returns the number of bits discarded, (which will always be
    /// between 0 and 7, inclusive), and the values taken, right-aligned.
    fn align(&mut self) -> (uint, u8);
}

/// A `Reader` wrapper to provide a `BitReader`.
pub struct BitSource<R> {
    src: R,
    // Must be wide enough to allow read() to return 32 bits at a time.
    cache: u64,
    cache_avail: u8,
}

impl<R: Reader> BitSource<R> {
    fn fill_buffer(&mut self) -> IoResult<()> {
        let b = try!(self.src.read_u8());
        self.cache <<= 8;
        self.cache |= b as u64;
        self.cache_avail += 8;
        Ok(())
    }

    /// Construct a `BitSource` wrapping a `Reader`.
    pub fn new(src: R) -> BitSource<R> {
        BitSource {
            src: src,
            cache: 0,
            cache_avail: 0
        }
    }
}

impl<R: Reader> BitReader for BitSource<R> {
    fn align(&mut self) -> (uint, u8) {
        let padding = self.cache_avail % 8;
        let values = self.cache & ((1 << padding) - 1);
        self.cache >>= padding;
        self.cache_avail -= padding;

        (padding as uint, values as u8)
    }

    #[inline]
    fn read_bits<T: Primitive>(&mut self, n: u8) -> IoResult<T> {
        let t_width = 8 * size_of::<T>();
        // T must be at least n bits wide
        assert!(t_width >= (n as uint));
        // Current implementation may only return up to 56 bits.
        assert!(n <= 56);

        // Fill buffer
        while self.cache_avail < n {
            try!(self.fill_buffer());
        }
        // Get top n bits of cache
        let mask = (1u64 << n) - 1;
        let shift = self.cache_avail - n;
        let out = (self.cache >> shift) & mask;
        self.cache_avail -= n;
        self.cache &= !(mask << shift);

        let out: T = NumCast::from(out).unwrap();
        Ok(out)
    }
}

impl<R: Reader> Reader for BitSource<R> {
    fn read(&mut self, buf: &mut [u8]) -> IoResult<uint> {
        if self.cache_avail % 8 != 0 {
            // Can't force alignment. This is going to be slow.
            // TODO when len(buf) is large, we can do larger reads and buffer
            // more. Limitation is that we must not read more than requested,
            // to avoid blocking/sudden EOF.
            let mut i = 0;
            while i < buf.len() {
                let x: u8 = try!(self.read_bits(8));
                buf[i] = x as u8;
                i += 1;
            }
            return Ok(i);
        }

        // Flush the cache
        let flushCount = cmp::min(buf.len(), self.cache_avail as uint / 8);
        let mut n = 0;
        while n < flushCount {
            buf[n] = self.cache as u8;
            n += 1;
            self.cache >>= 8;
        }
        // Read what's left
        let out = self.src.read(buf.mut_slice_from(n));
        out.map(|count| count + n)
    }
}

/// Reads a variable-length value
/// 
/// The encoding is an extended form of UTF-8, capable of representing
/// up to 36 bit values in between 1 and 7 bytes.
pub fn utf8_uint_from<R: Reader>(src: &mut R) -> FlacResult<u64> {
    let first = flac_io!(src.read_byte());
    let nlo = (!first).leading_zeros();

    // Encodings may be up to 7 bytes long, providing up to a 36-bit value:
    //  0xxxxxxx
    //  110xxxxx 10xxxxxx
    //  1110xxxx 10xxxxxx 10xxxxxx
    //  ...
    //  11111110 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    //
    //  Length of the entire codepoint is thus the number of leading ones in
    //  the first byte (`nlo`), or 1 if nlo == 0.
    let len = if nlo == 0 {
        1
    } else {
        assert_sync!(nlo > 1, "Invalid UTF-8 first byte")
        nlo
    };
    // Will take the low-order 7-nlo bits of `first`
    let first_mask = (1 << (7 - nlo)) - 1;

    let mut out: u64 = (first & first_mask) as u64;
    for _ in ::std::iter::range(1, len) {
        let b = flac_io!(src.read_byte());
        assert_sync!(b & 0xC0 == 0x80, "Invalid UTF-8 sequence")
        out <<= 6;
        out |= (b & 0x3F) as u64;
    }

    Ok(out)
}

#[cfg(test)]
fn utf8(x: &[u8]) -> FlacResult<u64> {
    utf8_uint_from(&mut ::std::io::BufReader::new(x))
}

#[test]
fn utf8_uint_valid() {
    assert_eq!(utf8([64]), Ok(64));
    assert_eq!(utf8([0xC3, 0x91]), Ok(209));
    assert_eq!(utf8([0xE7, 0xB0, 0x8C]), Ok(31756));
    assert_eq!(utf8([0xF2, 0xA4, 0xB9, 0x81]), Ok(675393));
    assert_eq!(utf8([0xF9, 0x8F, 0x93, 0x88, 0xBC]), Ok(20787772));
    assert_eq!(utf8([0xFD, 0xA7, 0xAA, 0xAF, 0x84, 0xB2]), Ok(1739256114));
    assert_eq!(utf8([0xFE, 0x88, 0x89, 0xBE, 0x92, 0xA7, 0x80]), Ok(8757258688));
}

#[test]
fn utf8_uint_invalid() {
    let bad = [
        [0x80, 0, 0],
        [0xBF, 0, 0],
        [0xFF, 0, 0],
        [0xF0, 0xFF, 0],
        [0xF0, 0xC0, 0],
        [0xEF, 0x81, 0x3F]
    ];

    for &t in bad.iter() {
        match utf8(t) {
            Err(FramingError(_)) => (),
            x => fail!("Expected Err(FramingError()), got {}", x)
        }
    }
}
