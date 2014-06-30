use std::fmt;
use std::iter;
use std::mem::size_of;
use std::str;
use super::{FlacResult, FramingError, IoError, CrcError};
use super::audio::{Audio, SubFrame, FixedBlock, VariableBlock};
use super::audio::{Predictor, Residual};
use super::audio::{Constant, Fixed, Lpc, Verbatim};
use super::audio::{NullResidual, CodedResidual};
use super::audio::{Independent, LeftSide, RightSide, MidSide};
use super::bitstream::{BitReader, BitSource, utf8_uint_from};
use super::crc;

/// Stream frame
#[deriving(Show)]
pub enum Frame {
    /// Stream metadata block
    MetadataBlock(Metadata),
    /// Frame of audio data
    AudioFrame(Audio)
}

/// Metadata block
#[deriving(Show)]
pub enum Metadata {
    /// METADATA_BLOCK_STREAMINFO
    MStreamInfo(StreamInfo),
    /// A block of zeroes
    ///
    /// Length in the stream is specified by the member.
    Padding(u32),
    /// Application-specific data block
    ///
    /// Refer to the [FLAC application registry](https://xiph.org/flac/id.html)
    /// for field meanings.
    Application {
        /// Application ID, as defined in the FLAC registry
        id: u32,
        /// Data block with application-defined meaning
        data: Vec<u8>
    },
    /// Set of stream pointers to simplify seeking
    SeekTable(Vec<SeekPoint>),
    /// Human-readable metadata
    MComment(Comment),
    /// Embedded cuesheet
    MCuesheet(Cuesheet),
    /// Embedded picture
    Picture {
        /// Code 0-20 specifying the type of picture
        picture_type: u32,
        /// MIME type of the `data`
        mime: Vec<Ascii>,
        /// Human-readable description of the picture
        description: String,
        /// Width in pixels
        width: u32,
        /// Height in pixels
        height: u32,
        /// Color depth in bits per pixel
        depth: u32,
        /// Number of colors, for indexed-color pictures such as GIF.
        /// 0 for non-indexed formats.
        num_colors: u32,
        /// Raw picture data
        data: Vec<u8>
    }
}

/// Core stream parameters
pub struct StreamInfo {
    /// Minimum size of a block (in samples) used in the stream
    pub block_min: u16,
    /// Maximum size of a block (in samples) used in the stream
    pub block_max: u16,
    /// Minimum frame size (in bytes) used, 24-bit value.
    /// 0 indicates unknown.
    pub frame_min: u32,
    /// Maximum frame size (in bytes) used, 24-bit value.
    /// 0 indicates unknown.
    pub frame_max: u32,
    /// Sample rate in Hz. May not be zero or greater than 655350 Hz.
    pub sample_rate: u32,
    /// Number of channels
    ///
    /// Must be between 1 and 8, inclusive.
    pub channels: u8,
    /// Bits per sample
    ///
    /// Must be between 3 and 32, inclusive. The reference FLAC
    /// implementation only supports up to 24 bits per sample.
    pub bps: u8,
    /// Total stream length in samples, 36 bits. 0 is unknown.
    ///
    /// Stream length in seconds is thus `samples / sample_rate`.
    pub samples: u64,
    /// MD5 signature of unencoded audio data
    ///
    /// This value allows decoders to detect errors even when the
    /// bitstream is valid.
    pub signature: [u8, ..16]
}

impl StreamInfo {
    fn read_from<R: BitReader>(src: &mut R, len: u32) -> FlacResult<StreamInfo> {
        // StreamInfo must be 34 bytes
        assert_sync!(len == 34, "StreamInfo block is not the correct length");

        let mut out = StreamInfo {
            block_min: flac_io!(src.read_be_u16()),
            block_max: flac_io!(src.read_be_u16()),
            frame_min: flac_io!(src.read_be_uint_n(3)) as u32,
            frame_max: flac_io!(src.read_be_uint_n(3)) as u32,
            sample_rate: flac_io!(src.read_bits(20)),
            channels: flac_io!(src.read_bits::<u8>(3)) + 1,
            bps: flac_io!(src.read_bits::<u8>(5)) + 1,
            samples: flac_io!(src.read_bits::<u64>(36)),
            signature: [0, ..16]
        };
        flac_io!(src.read_at_least(out.signature.len(),
                                   out.signature.as_mut_slice()));

        Ok(out)
    }
}

impl fmt::Show for StreamInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, r"StreamInfo{{blocks=[{}-{}]s, frames=[{}-{}]B,
                         sample_rate={}, channels={}, bps={}, samples={}, md5=",
               self.block_min, self.block_max, self.frame_min, self.frame_max,
               self.sample_rate, self.channels, self.bps, self.samples));

        for &x in self.signature.iter() {
            try!(write!(f, "{:02X}", x));
        }
        write!(f, "}}")
    }
}

fn read_padding_from<R: BitReader>(src: &mut R, len: u32) -> FlacResult<Metadata> {
    flac_io!(src.read_exact(len as uint));
    Ok(Padding(len))
}

/// Vorbis comments
#[deriving(Show)]
pub struct Comment {
    /// Name of the vendor or tool which created the stream
    pub vendor: String,
    /// `=`-delimited set of key-value pairs.
    ///
    /// For example, `["ARTIST=Comaduster","TITLE=Hollow Worlds"]`.
    /// There are no strict limits on fields. Refer to the
    /// [Vorbis comment specification](https://www.xiph.org/vorbis/doc/v-comment.html)
    /// for suggested common field names.
    pub comments: Vec<String>
}

impl Comment {
    fn read_from<R: BitReader>(src: &mut R, len: u32) -> FlacResult<Comment> {
        // Despite every other part of FLAC being big-endian, the Vorbis
        // comment block is exactly the same as it would be in a Vorbis stream,
        // meaning the multibyte values here are little-endian.
        let mut byte_count = 0;

        let vendor_len = flac_io!(src.read_le_u32());
        let vendor_bytes = flac_io!(src.read_exact(vendor_len as uint));
        let vendor = str::from_utf8_lossy(vendor_bytes.as_slice()).into_string();
        byte_count += 4 + vendor_len;

        let count = flac_io!(src.read_le_u32()) as uint;
        byte_count += 4;

        let mut comments = Vec::with_capacity(count);
        for _ in iter::range(0, count) {
            let l = flac_io!(src.read_le_u32());
            let bytes = flac_io!(src.read_exact(l as uint));
            let s = str::from_utf8_lossy(bytes.as_slice()).into_string();
            comments.push(s);
            byte_count += l + 4;
        }

        assert_sync!(len == byte_count, "Comment block length did not match block header");
        Ok(Comment {
            vendor: vendor,
            comments: comments
        })
    }
}

/// CD metadata and physical structure
pub struct Cuesheet {
        /// Media catalog number
        ///
        /// For CD-DA, this is a 13-digit number padded with NUL
        catalog_number: [Ascii, ..128],
        /// Number of lead-in samples. Only meaningful for CD-DA.
        lead_in: u64,
        /// `true` if this cuesheet describes a CD
        is_CD: bool,
        /// One or more tracks.
        ///
        /// There must always be a lead-out, which has number 170 for CD-DA
        /// or 255 otherwise.
        tracks: Vec<Track>
}

impl fmt::Show for Cuesheet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Cuesheet{{catalog_number=\"{}\", lead_in={}, is_CD={}, tracks={}}}",
                      self.catalog_number.as_str_ascii(),
                      self.lead_in, self.is_CD, self.tracks)
    }
}

/// Stream seek point
///
/// Points to an arbitrary frame within the stream.
#[deriving(Show)]
pub struct SeekPoint {
    /// Absolute sample number of the first sample in the target frame
    sample: u64,
    /// Byte offset from the first frame's header to the target frame's header
    offset: u64,
    /// Number of samples in the target frame
    num_samples: u16
}

/// Cuesheet track
pub struct Track {
    /// Track offset in samples within FLAC stream
    offset: u64,
    number: u8,
    /// International Standard Recording Code
    ///
    /// Code is 12-character alphanumeric. 12 NUL characters indicates there is
    /// no ISRC for this track.
    isrc: [Ascii, ..12],
    is_audio: bool,
    pre_emphasis: bool,
    indexes: Vec<Index>
}

impl fmt::Show for Track {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Track{{offset={}, number={}, isrc={}, is_audio={}
                   pre_emphasis={}, indexes={}}}",
                      self.offset, self.number,
                      self.isrc.as_str_ascii(),
                      self.is_audio, self.pre_emphasis, self.indexes)
    }
}

/// Cuesheet track index
#[deriving(Show)]
pub struct Index {
    /// Offset of index point in samples, relative to track offset
    offset: u64,
    /// Index point number
    ///
    /// Point 0 is a pre-track gap. The first point in a track must be either
    /// 0 or 1, and subsequent indexes monotonically increase.
    num: u8
}

#[allow(non_camel_case_types)]
#[deriving(Show)]
enum DecoderState {
    // "fLaC" marker
    S_Marker,
    // STREAMINFO
    S_StreamInfo,
    // metadata ..
    S_Metadata(StreamInfo),
    // audio
    S_Audio(StreamInfo)
}

/// FLAC stream unpacker
pub struct Stream<R> {
    src: BitSource<R>,
    state: DecoderState,
}

impl<R: Reader> Stream<R> {
    /// Initializes a new decoder.
    pub fn new(src: R) -> Stream<R> {
        Stream {
            src: BitSource::new(src),
            state: S_Marker
        }
    }

    fn read_metadata(&mut self) -> FlacResult<(bool, Metadata)> {
        let last = flac_io!(self.src.read_bits::<uint>(1)) == 1;
        let ty: u8 = flac_io!(self.src.read_bits(7));
        let len: u32 = flac_io!(self.src.read_be_uint_n(3)) as u32;

        debug!("Stream::read_metadata type={}, len={:#08x}", ty, len);

        let value = match ty {
            0 => MStreamInfo(try!(StreamInfo::read_from(&mut self.src, len))),
            1 => try!(read_padding_from(&mut self.src, len)),
            4 => MComment(try!(Comment::read_from(&mut self.src, len))),
            x => fail!("Don't know what to do with metadata block type {}", x)
        };

        debug!("Stream::read_metadata => {}", value);
        Ok((last, value))
    }

    fn read_subframe(&mut self, bps: u8, n_samples: u32) -> FlacResult<SubFrame> {
        let pad = flac_io!(self.src.read_bits::<u8>(1));
        assert_sync!(pad == 0, "Invalid subframe header padding");

        let s_type = flac_io!(self.src.read_bits::<u8>(6));
        let wasted = if flac_io!(self.src.read_bits::<uint>(1)) == 0 {
                         0
                     } else {
                         let mut count = 1;
                         while flac_io!(self.src.read_bits::<uint>(1)) == 0 {
                             count += 1;
                         }
                         count
                     };
        let predictor = try!(read_predictor(s_type, &mut self.src, bps, n_samples));
        let residual = match predictor {
            Constant(_) | Verbatim(_) => NullResidual,
            Fixed(..) | Lpc(..) => try!(read_residual(&mut self.src, bps, n_samples))
        };
        Ok(SubFrame(predictor, wasted, residual))
    }

    fn read_frame_header(&mut self, si: &StreamInfo) -> FlacResult<Audio> {
        // Running CRC-8 computation over the frame header
        let mut filter = crc::filter_for_frame_hdr(&mut self.src);

        let sync = flac_io!(filter.read_bits::<u16>(14));
        assert_sync!(sync == 0x3FFE, "Frame sync code mismatch");
        let reserved1 = flac_io!(filter.read_bits::<u8>(1));
        assert_sync!(reserved1 == 0, "Invalid reserved1");

        let blocking_strategy = flac_io!(filter.read_bits::<u8>(1));
        let blocksize_spec = flac_io!(filter.read_bits::<u8>(4));
        assert_sync!(blocksize_spec != 0, "Invalid block size value");
        let samplerate_spec = flac_io!(filter.read_bits::<u8>(4));
        assert_sync!(samplerate_spec != 0xF, "Invalid sample rate value");
        let channels_spec = flac_io!(filter.read_bits::<u8>(4));
        assert_sync!(channels_spec <= 10, "Invalid channel assignment value");
        let samplesize_spec = flac_io!(filter.read_bits::<u8>(3));
        assert_sync!(samplesize_spec & 3 != 3, "Invalid sample size value");
        let reserved2 = flac_io!(filter.read_bits::<u8>(1));
        assert_sync!(reserved2 == 0, "Invalid reserved2");

        let location = if blocking_strategy == 0 {
            FixedBlock(try!(utf8_uint_from(&mut filter)))
        } else {
            VariableBlock(try!(utf8_uint_from(&mut filter)) as u32)
        };

        let blocksize = match blocksize_spec {
            1 => 192,
            n @ 2..5 => 576 * (1 << (n - 2) as uint),
            6 => flac_io!(filter.read_byte()) as u32 + 1,
            7 => flac_io!(filter.read_be_u16()) as u32 + 1,
            n @ 8..15 => 256 * (1 << (n - 8) as uint),
            _ => unreachable!()
        };

        let samplerate = match samplerate_spec {
            0 => si.sample_rate,
            n @ 1..11 => [88200, 176400, 192000, 8000, 16000,
                          22050, 24000, 32000, 44100, 48000,
                          96000][n as uint - 1],
            12 => flac_io!(filter.read_byte()) as u32 * 1000,
            13 => flac_io!(filter.read_be_u16()) as u32,
            14 => flac_io!(filter.read_be_u16()) as u32 * 10,
            _ => unreachable!()
        };

        let channels = match channels_spec {
            n @ 0..7 => Independent(n + 1),
            8 => LeftSide,
            9 => RightSide,
            10 => MidSide,
            _ => unreachable!()
        };

        let samplesize = match samplesize_spec {
            0 => si.bps,
            1 => 8, 2 => 12, 4 => 16,
            5 => 20, 6 => 24,
            _ => unreachable!()
        };

        // Validate header CRC
        flac_io!(filter.read_byte());
        if filter.digest() != 0 {
            return Err(CrcError);
        }

        Ok(Audio {
            location: location,
            size: blocksize,
            sample_rate: samplerate,
            channels: channels,
            sample_size: samplesize,
            subframes: Vec::with_capacity(channels.count())
        })
    }

    fn read_audio(&mut self, si: &StreamInfo) -> FlacResult<Audio> {
        let mut a = try!(self.read_frame_header(si));
        for _ in iter::range(0, a.channels.count()) {
            a.subframes.push(try!(self.read_subframe(a.sample_size, a.size)));
        }
        Ok(a)
    }

    /// Decodes the next frame from the stream.
    pub fn next(&mut self) -> FlacResult<Frame> {
        debug!("Stream::next at {}", self.state);
        match self.state {
            S_Marker => {
                let sig = flac_io!(self.src.read_exact(4));
                assert_sync!(sig.as_slice() == b"fLaC", "Invalid stream marker");

                self.state = S_StreamInfo;
                self.next()
            }

            S_StreamInfo => {
                let (final, block) = try!(self.read_metadata());
                assert_sync!(block.is_streaminfo(), "Expected StreamInfo");
                let streaminfo = match block {
                    MStreamInfo(x) => x,
                    _ => unreachable!()
                };

                self.state = if final {
                    S_Audio(streaminfo)
                } else {
                    S_Metadata(streaminfo)
                };
                Ok(MetadataBlock(block))
            }

            S_Metadata(si) => {
                let (final, block) = try!(self.read_metadata());
                assert_sync!(!block.is_streaminfo(), "Expected non-StreamInfo metadata block");

                if final {
                    self.state = S_Audio(si);
                }
                Ok(MetadataBlock(block))
            }

            S_Audio(si) => {
                let frame = try!(self.read_audio(&si));
                Ok(AudioFrame(frame))
            }
        }
    }
}

impl Metadata {
    fn is_streaminfo(&self) -> bool {
        match *self {
            MStreamInfo(_) => true,
            _ => false
        }
    }
}

/// Sign-extend `x` from `from` to 32 bits.
#[inline]
fn sext(x: uint, from: u8) -> i32 {
    // This implementation can't expand width
    assert!(size_of::<uint>() >= size_of::<i32>());
    let shift = (size_of::<i32>() * 8) - (from as uint);

    ((x << shift) as i32) >> shift
}

#[test]
fn test_sext() {
    assert_eq!(sext(1, 1), -1);
    assert_eq!(sext(16, 10), 16);
    assert_eq!(sext(0xFEB, 12), -21);
    assert_eq!(sext(0x7FFFFFFF, 32), 2147483647);
    assert_eq!(sext(0xFFFFFFFF, 32), -1);
}


fn read_raw_samples<R: BitReader>(src: &mut R, bps: u8, count: u32) -> FlacResult<Vec<i32>> {
    let mut samples = Vec::<i32>::with_capacity(count as uint);
    for _ in range(0, count) {
        let s = flac_io!(src.read_bits::<u32>(bps));
        samples.push(sext(s as uint, bps));
    }

    Ok(samples)
}

fn read_predictor<R: BitReader>(s_type: u8,
                                src: &mut R,
                                bps: u8,
                                blocksize: u32) -> FlacResult<Predictor> {
    let pred = match s_type {
        0 => Constant(sext(flac_io!(src.read_bits(bps)), bps)),
        1 => Verbatim(try!(read_raw_samples(src, bps, blocksize))),
        x @ 8..15 => {
            let order = x & 7;
            assert_sync!(order <= 4, "Invalid fixed predictor order");
            let warmup = try!(read_raw_samples(src, bps, order as u32));
            Fixed(order, warmup)
        },
        x @ 32..63 => {
            let order = (x & 0x1F) + 1;
            let warmup = try!(read_raw_samples(src, bps, order as u32));

            let precision = flac_io!(src.read_bits::<u8>(4)) + 1;
            assert_sync!(precision < 16, "Invalid LPC precision");

            let shift = sext(flac_io!(src.read_bits(5)), 5) as i8;

            // `order` signed `precision`-bit values
            let mut coefficients = Vec::<i16>::with_capacity(order as uint);
            for _ in range(0, order) {
                let unsigned = flac_io!(src.read_bits(order));
                coefficients.push(sext(unsigned, precision) as i16);
            }

            Lpc(order, warmup, precision, shift, coefficients)
        }
        _ => return Err(FramingError("Invalid subframe type"))
    };

    Ok(pred)
}

#[allow(unused_variable,unreachable_code)]
fn read_residual<R: BitReader>(src: &mut R, bps: u8, blocksize: u32) -> FlacResult<Residual> {
    fail!();
    Ok(CodedResidual)
}
