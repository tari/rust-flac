use std::io::IoResult;
use super::bitstream::BitReader;

// x^8 + x^2 + x^1 + x^0
#[allow(dead_code)]
static POLYNOMIAL_8: u16 = 0x0107;

pub fn filter_for_frame_hdr<'a, S: BitReader>(src: &'a mut S) -> Filter<'a, S, Crc8> {
    Filter {
        src: src,
        state: Crc8 { result: 0 }
    }
}

pub struct Filter<'a, S, T> {
    src: &'a mut S,
    state: T
}

impl<'a, S, D, T: CrcHasher<D>> Filter<'a, S, T> {
    pub fn digest(self) -> D {
        self.state.digest()
    }
}

impl<'a, S: Reader, D, T: CrcHasher<D>> Reader for Filter<'a, S, T> {
    fn read(&mut self, buf: &mut [u8]) -> IoResult<uint> {
        let count = try!(self.src.read(buf));
        for i in range(0, count) {
            self.state.update(buf[i], 8);
        };
        Ok(count)
    }
}

impl<'a, S: BitReader, D, T: CrcHasher<D>> BitReader for Filter<'a, S, T> {
    fn read_bits<T: Primitive>(&mut self, n: u8) -> IoResult<T> {
        warn!("crc::Filter BitReader is incomplete");
        self.src.read_bits(n)
    }

    fn align(&mut self) -> (uint, u8) {
        warn!("crc::Filter BitReader is incomplete");
        self.src.align()
    }
}

/// CRC-8 with POLYNOMIAL_8
pub struct Crc8 {
    result: u8
}

/// Something that can compute a CRC over a bitstream
pub trait CrcHasher<T> {
    /// Take `nbits` bits from `x` and accumulate to the state vector.
    fn update(&mut self, x: u8, nbits: uint);
    /// Return the final CRC, consuming self.
    fn digest(self) -> T;
}

impl CrcHasher<u8> for Crc8 {
    #[allow(unused_variable)]
    fn update(&mut self, x: u8, nbits: uint) {
        // POLYNOMIAL_8
        warn!("Crc8::update not implemented");
    }

    fn digest(self) -> u8 {
        warn!("Crc8::digest not implemented");
        self.result
    }
}

