use std::io::Read;

use crate::{
    encoding::{Decoder, UTF8Decoder},
    error::XMLError,
};

const INPUT_CHUNK: usize = 4096;
const GROW_THRESHOLD: usize = 16;

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
    /// Whether `source` has reached EOF
    eof: bool,
}

impl<'a> InputSource<'a> {
    pub fn from_reader(reader: impl Read + 'a, encoding: Option<&str>) -> Self {
        todo!()
    }

    pub fn from_content(str: &str) -> Self {
        Self {
            source: Box::new(std::io::empty()),
            buffer: [0; _],
            decoder: Box::new(UTF8Decoder),
            decoded: str.to_owned(),
            buffer_next: 0,
            buffer_end: 0,
            decoded_next: 0,
            total_read: str.len(),
            eof: true,
        }
    }

    pub fn grow(&mut self) -> Result<(), XMLError> {
        if !self.eof {
            let rem = self.buffer_end - self.buffer_next;
            if rem < GROW_THRESHOLD {
                self.buffer
                    .copy_within(self.buffer_next..self.buffer_end, 0);
                self.buffer_next = 0;
                self.buffer_end = rem;
                let read = self.source.read(&mut self.buffer[self.buffer_end..])?;
                self.eof = read == 0;
                self.buffer_end += read;
                self.total_read += read;
            }
        }

        let rem = self.buffer_end - self.buffer_next;
        if rem > 0 {
            let cap = self.decoded.capacity() - self.decoded_next;
            if cap < GROW_THRESHOLD {
                self.decoded.drain(..self.decoded_next);
                self.decoded.shrink_to(INPUT_CHUNK);
                self.decoded_next = 0;
            }
            let (read, _) = self.decoder.decode(
                &self.buffer[self.buffer_next..self.buffer_end],
                &mut self.decoded,
                self.eof,
            )?;
            self.buffer_next += read;
        }
        Ok(())
    }

    pub fn content_bytes(&self) -> &[u8] {
        &self.decoded.as_bytes()[self.decoded_next..]
    }

    pub fn next_char(&mut self) -> Result<Option<char>, XMLError> {
        Ok(self
            .peek_char()?
            .inspect(|c| self.decoded_next += c.len_utf8()))
    }

    pub fn peek_char(&mut self) -> Result<Option<char>, XMLError> {
        self.grow()?;
        Ok(self.decoded.chars().next())
    }
}

impl Default for InputSource<'_> {
    fn default() -> Self {
        Self {
            source: Box::new(std::io::empty()),
            buffer: [0; INPUT_CHUNK],
            decoder: Box::new(UTF8Decoder),
            decoded: String::new(),
            buffer_next: 0,
            buffer_end: 0,
            decoded_next: 0,
            total_read: 0,
            eof: true,
        }
    }
}
