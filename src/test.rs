#![feature(phase)]
#[phase(plugin,link)] extern crate log;

extern crate flac;
use flac::audio;
use flac::stream::Stream;
use flac::stream::{MetadataBlock, AudioFrame};
use flac::stream::MStreamInfo;
use std::io::File;

#[test]
fn decode_silence() {
    // Pure silence, 16 bit 44.1 kHz, 1 second.
    // Encoding is 4-kibisample frames, all with SUBFRAME_CONSTANT
    let f = File::open(&Path::new("../tests/silence.flac"));
    let mut s = Stream::new(f);

    let streaminfo = match s.next() {
        Ok(MetadataBlock(MStreamInfo(si))) => si,
        Ok(x) => fail!("Expected StreamInfo, got {}", x),
        Err(x) => fail!("{}", x)
    };
    assert_eq!(streaminfo.sample_rate, 44100);
    assert_eq!(streaminfo.channels, 1);
    assert_eq!(streaminfo.bps, 16);

    let audio = match s.next() {
        Ok(AudioFrame(x)) => x,
        Ok(x) => fail!("Expected AudioFrame, got {}", x),
        Err(x) => fail!("{}", x)
    };
    assert_eq!(audio.size, 4096);
    assert_eq!(audio.sample_rate, 44100);
    assert_eq!(audio.channels, audio::Independent(1));
    assert_eq!(audio.channels.count(), 1);
    assert_eq!(audio.sample_size, 16);

    assert_eq!(audio.subframes.len(), 1);
    let &audio::SubFrame(ref predictor, wasted, ref residual) = audio.subframes.get(0);
    assert_eq!(*predictor, audio::Constant(0));
    assert_eq!(wasted, 0);
    assert_eq!(*residual, audio::NullResidual);
}
