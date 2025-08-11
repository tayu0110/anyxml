use std::io::Read;

use crate::{
    encoding::{DecodeError, Decoder, UTF8Decoder, UTF16BEDecoder, UTF16LEDecoder},
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
    pub fn from_reader(reader: impl Read + 'a, encoding: Option<&str>) -> Result<Self, XMLError> {
        let mut ret = Self::default();
        ret.decoded
            .reserve(INPUT_CHUNK.saturating_sub(ret.decoded.capacity()));
        ret.source = Box::new(reader);

        if let Some(encoding) = encoding {
            todo!()
        } else {
            // Handling strange implementations that write only one byte per read
            for _ in 0..INPUT_CHUNK {
                let read = ret.source.read(&mut ret.buffer[ret.buffer_end..])?;
                ret.buffer_end += read;
                if read == 0 || ret.buffer_end == INPUT_CHUNK {
                    break;
                }
            }
            if ret.buffer_end < 4 {
                // The minimum byte count for well-formed XML is 4 bytes
                // (a document containing only an empty tag with a length of 1),
                // so if the number of bytes read is less than 4 bytes,
                // encoding detection is not possible.
                return Ok(ret);
            }

            match ret.buffer[..4] {
                // Cases where BOM was found:
                // UCS-4, big-endian machine (1234 order)
                [0x00, 0x00, 0xFE, 0xFF] => return Err(XMLError::ParserUnsupportedEncoding),
                // UCS-4, little-endian machine (4321 order)
                [0xFF, 0xFE, 0x00, 0x00] => return Err(XMLError::ParserUnsupportedEncoding),
                // UCS-4, unusual octet order (2143)
                [0x00, 0x00, 0xFF, 0xFE] => return Err(XMLError::ParserUnsupportedEncoding),
                // UCS-4, unusual octet order (3412)
                [0xFE, 0xFF, 0x00, 0x00] => return Err(XMLError::ParserUnsupportedEncoding),
                // UTF-16, big-endian
                [0xFE, 0xFF, ..] => {
                    ret.buffer_next = 2;
                    ret.decoder = Box::new(UTF16BEDecoder);
                }
                // UTF-16, little-endian
                [0xFF, 0xFF, ..] => {
                    ret.buffer_next = 2;
                    ret.decoder = Box::new(UTF16LEDecoder);
                }
                // UTF-8
                [0xEF, 0xBB, 0xBF, ..] => {
                    ret.buffer_next = 3;
                    ret.decoder = Box::new(UTF8Decoder);
                }
                // Cases where BOM was not found:
                // UCS-4 or other 32-bit encoding, big-endian machine (1234 order)
                [0x00, 0x00, 0x00, 0x3C] => return Err(XMLError::ParserUnsupportedEncoding),
                // UCS-4 or other 32-bit encoding, little-endian machine (4321 order)
                [0x3C, 0x00, 0x00, 0x00] => return Err(XMLError::ParserUnsupportedEncoding),
                // UCS-4 or other 32-bit encoding, unusual octet order (2143)
                [0x00, 0x00, 0x3C, 0x00] => return Err(XMLError::ParserUnsupportedEncoding),
                // UCS-4 or other 32-bit encoding, unusual octet order (3412)
                [0x00, 0x3C, 0x00, 0x00] => return Err(XMLError::ParserUnsupportedEncoding),
                // UTF-16BE or big-endian ISO-10646-UCS-2 or other encoding  with a 16-bit
                // code unit in big-endian order and ASCII characters encoded as ASCII values
                // (the encoding declaration must be read to determine which)
                [0x00, 0x3C, 0x00, 0x3F] => todo!(),
                // UTF-16LE or little-endian ISO-10646-UCS-2 or other encoding with a 16-bit
                // code unit in little-endian order and ASCII characters encoded as ASCII values
                // (the encoding declaration must be read to determine which)
                [0x3C, 0x00, 0x3F, 0x00] => todo!(),
                // UTF-8, ISO 646, ASCII, some part of ISO 8859, Shift-JIS, EUC, or any other 7-bit,
                // 8-bit, or mixed-width encoding which ensures that the characters of ASCII have
                // their normal positions, width, and values; the actual encoding declaration must
                // be read to detect which of these applies, but since all of these encodings use
                // the same bit patterns for the relevant ASCII characters, the encoding declaration
                // itself may be read reliably
                [0x3C, 0x3F, 0x78, 0x6D] => todo!(),
                // EBCDIC (in some flavor; the full encoding declaration must be read to tell
                // which code page is in use)
                [0x4C, 0x6F, 0xA7, 0x94] => return Err(XMLError::ParserUnsupportedEncoding),
                // cannot detect the specific encoding from the head of the content.
                // In this case, we assume that it is a UTF-8 document without an XML declaration.
                _ => {
                    ret.decoder = Box::new(UTF8Decoder);
                }
            };
        }
        Ok(ret)
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
                let mut read = 1;
                while self.buffer_end < INPUT_CHUNK && read != 0 {
                    read = self.source.read(&mut self.buffer[self.buffer_end..])?;
                    self.buffer_end += read;
                    self.total_read += read;
                }
                self.eof = read == 0;
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
            match self.decoder.decode(
                &self.buffer[self.buffer_next..self.buffer_end],
                &mut self.decoded,
                self.eof,
            ) {
                Ok((read, _)) => {
                    self.buffer_next += read;
                }
                Err(e) => match e {
                    DecodeError::Malformed {
                        read,
                        write: _,
                        length,
                        offset,
                    } => {
                        let actual_read = read - offset - length;
                        // Since it may not be possible to set the decoder appropriately
                        // from the BOM or external encoding, no error is returned as long as
                        // some data can be decoded.
                        if actual_read > 0 {
                            self.buffer_next += actual_read;
                        } else {
                            return Err(From::from(e));
                        }
                    }
                    _ => return Err(From::from(e)),
                },
            }
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
        if let Some(c) = self.decoded.chars().next() {
            return Ok(Some(c));
        }
        self.grow()?;
        Ok(self.decoded.chars().next())
    }

    pub fn advance(&mut self, mut len: usize) -> Result<(), XMLError> {
        while len > 0 {
            self.grow()?;
            let l = len.min(self.decoded.len() - self.decoded_next);
            assert!(l > 0);
            assert!(self.decoded.is_char_boundary(self.decoded_next + l));
            self.decoded_next += l;
            len -= l;
        }
        Ok(())
    }

    /// Returns `true` if both the decoded but unused string
    /// and the read but undecoded data are 0 bytes.
    ///
    /// # Note
    /// Returning `true` does not mean that EOF has been reached.  
    /// If all of the read data has been decoded and you continue to consume the decoded strings
    /// without explicitly calling `grow`, this function may return `true` before reaching EOF.
    pub fn is_empty(&self) -> bool {
        self.decoded.len() - self.decoded_next == 0 && self.buffer_end - self.buffer_next == 0
    }

    pub fn encoding_name(&self) -> &'static str {
        self.decoder.name()
    }

    pub(crate) fn switch_encoding(&mut self, to: &str) -> Result<(), XMLError> {
        todo!()
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
