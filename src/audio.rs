//! Audio data structure and compression.
//!
//! Audio signals are a pair of predictor and residual, where the actual
//! signal is the sum of values yielded by the predictor and the residual.

/// Signal predictor
#[deriving(Show, PartialEq, Eq)]
pub enum Predictor {
    /// DC, "digital silence"
    Constant(u32),
    /// Fixed linear predictor of order 0-3
    /// 
    /// (order, warmup_samples)
    Fixed(u8, Vec<i32>),
    /// FIR linear predictor of up to order 32
    /// 
    /// (order, warmup_samples, precision, shift, coefficients)
    Lpc(u8, Vec<i32>, u8, i8, Vec<i16>),
    /// Zero-order; raw signal
    Verbatim(Vec<i32>)
}

/*impl Iterator<u32> for Predictor {
    #[inline]
    fn next(&mut self) -> Option<u32> {
        match *self {
            Constant(n) => Some(n),
        }
    }
}*/

/// Difference between signal and predictor
#[deriving(Show, PartialEq, Eq)]
pub enum Residual {
    /// Zero (entire signal is in the predictor)
    NullResidual,
    /// Nonzero
    CodedResidual
}

/// A block of audio data, containing subframes
#[deriving(Show)]
pub struct Audio {
    /// Location within the stream of this frame
    pub location: FrameIndex,
    /// Block size in (inter-channel) samples
    pub size: u32,
    /// Sample rate in Hz
    pub sample_rate: u32,
    /// Channel mapping
    pub channels: ChannelSpec,
    /// Sample width in bits
    pub sample_size: u8,
    /// One subframe per channel of audio data
    pub subframes: Vec<SubFrame>
}

impl Audio {
    /// Decode the frame into raw samples
    ///
    /// `data` is a vector of channels, each of which must be at least
    /// `self.size` items large. Items beyond the frame's length in each
    /// channel are not modified.
    pub fn decode_into(&self, data: &mut Vec<Vec<i32>>) {
        for (i, channel) in data.mut_iter().enumerate() {
            self.subframes.get(i).decode_into(channel.as_mut_slice());
        }
    }

    /// Same as `decode_into`, but allocates new storage.
    pub fn decode(&self) -> Vec<Vec<i32>> {
        let mut v = Vec::from_fn(self.channels.count(), |_| {
            Vec::from_fn(self.size as uint, |_| 0)
        });
        self.decode_into(&mut v);
        v
    }
}

/// (Predictor, wasted bits-per-sample, Residual)
#[deriving(Show)]
pub struct SubFrame(pub Predictor, pub u8, pub Residual);

impl SubFrame {
    /// Decompresses audio data into 32-bit samples.
    /// 
    /// `data` must be large enough to hold the entire subframe.
    #[allow(unused_variable)]
    pub fn decode_into(&self, data: &mut [i32]) {

    }
}

/// Indicates the stream position of an audio frame
#[deriving(Show, PartialEq, Eq)]
pub enum FrameIndex {
    /// Fixed blocksize stream
    ///
    /// Value is the sample number of the beginning of this frame
    FixedBlock(u64),
    /// Variable blocksize stream
    ///
    /// Value is the frame number from beginning of stream. Sample number
    /// may be computed via multiplication by the block size in samples.
    VariableBlock(u32)
}

/// Expression of channels in a block
/// 
/// The difference encodings represent a single complete channel and the
/// difference between that channel and the other. Mid/Side assignment
/// is similar, but provides the average and difference of the pair
/// (left, right).
///
/// For example, Given a Left/Side encoding of `(54, -4)`, the left channel's
/// value is 54 and the right is 50. With Mid/Side, the left would be 52 and
/// right 56 given the same values.
#[deriving(Show, PartialEq, Eq)]
pub enum ChannelSpec {
    /// Channels are encoded independently
    ///
    /// Channel mapping is as follows:
    /// 
    ///  * 1 channel: mono
    ///  * 2 channels: left, right
    ///  * 3 channels: left, right, center
    ///  * 4 channels: front left, front right, back left, back right
    ///  * 5 channels: front left, front right, front center, back/surround left, back/surround right
    ///  * 6 channels: front left, front right, front center, LFE, back/surround left, back/surround right
    ///  * 7 channels: front left, front right, front center, LFE, back center, side left, side right
    ///  * 8 channels: front left, front right, front center, LFE, back left, back right, side left, side right
    Independent(u8),
    /// Left/Side difference
    LeftSide,
    /// Right/Side difference
    RightSide,
    /// Mid/Side difference
    MidSide
}

impl ChannelSpec {
    /// Gets the number of channels represented
    pub fn count(&self) -> uint {
        match *self {
            Independent(x) => x as uint,
            _ => 2
        }
    }
}

