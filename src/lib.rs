#![crate_type="lib"]
#![crate_id = "flac#$0.1"]
#![deny(missing_doc,dead_code)]
#![feature(macro_rules)]
#![feature(struct_variant)]

//! FLAC decoder
//! 
//! 

#![feature(phase)]
#[phase(plugin,link)] extern crate log;

use std::io;

/// flac_io! and assert_sync!
mod macros;

/// Bitstream unpacking
mod bitstream;

/// FLAC stream format
pub mod stream;

/// Audio data
pub mod audio;

mod crc;

#[cfg(test)]
mod test;

/// Error/Ok indicator
pub type FlacResult<T> = Result<T, FlacErr>;

/// Decoder errors
#[deriving(Clone, PartialEq, Show)]
pub enum FlacErr {
    /// An expected stream structure was not found or was invalid.
    /// The contained string provides a human-readable error message.
    FramingError(&'static str),
    /// I/O error.
    IoError(io::IoError),
    /// Invalid frame header CRC.
    CrcError
}

/// A simple FLAC decoder
pub struct Decoder<R> {
    stream: stream::Stream<R>,
    buffers: Vec<Vec<i32>>
}

impl<R: Reader> Decoder<R> {
    /// Construct a Decoder from a Reader.
    pub fn new(reader: R) -> Decoder<R> {
        Decoder {
            stream: stream::Stream::new(reader),
            buffers: Vec::new()
        }
    }

    /// Decode the next frame of audio samples from the stream.
    pub fn next<'a>(&'a mut self) -> FlacResult<&'a Vec<Vec<i32>>> {
        let frame = try!(self.stream.next());
        match frame {
            stream::MetadataBlock(_) => self.next(),
            stream::AudioFrame(a) => {
                a.decode_into(&mut self.buffers);
                Ok(&self.buffers)
            }
        }
    }
}
