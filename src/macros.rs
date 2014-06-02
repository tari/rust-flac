#![macro_escape]

/// try!-like overload to turn IoResult into FlacResult
macro_rules! flac_io(
    ($e:expr) => (
        match $e {
            Ok(r) => r,
            Err(e) => return Err(IoError(e))
        }
    );
)

/// "soft" assertion returning FramingError on failure
macro_rules! assert_sync(
    ($e:expr, $s:expr) => (
        if $e == false {
            return Err(FramingError($s));
        }
    );
)
