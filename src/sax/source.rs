use std::io::Read;

use crate::encoding::Decoder;

const INPUT_CHUNK: usize = 4096;

pub struct InputSource<'a> {
    source: Box<dyn Read + 'a>,
    buffer: [u8; INPUT_CHUNK],
    decoder: Box<dyn Decoder>,
    decoded: String,
    /// Start position of the undecoded range of `buffer`
    buffer_next: usize,
    /// End position of data read into `buffer`
    buffer_end: usize,
    /// Start position of unused data in `decoded`
    decoded_next: usize,
    /// Total number of bytes read from `source`
    total_read: usize,
}
