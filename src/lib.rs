#![crate_type="lib"]
#![crate_id = "flac#$0.1"]
#![deny(missing_doc,dead_code)]
#![feature(macro_rules)]
#![feature(struct_variant)]

//! FLAC decoder
//! 
//! 

#![feature(phase)]
#[phase(syntax,link)] extern crate log;

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

/// Error/Ok indicator
pub type FlacResult<T> = Result<T, FlacErr>;

/// Decoder errors
#[deriving(Clone, Eq, Show)]
pub enum FlacErr {
    /// An expected stream structure was not found or was invalid.
    /// The contained string provides a human-readable error message.
    FramingError(&'static str),
    /// I/O error.
    IoError(io::IoError),
    /// Invalid frame header CRC.
    CrcError
}

