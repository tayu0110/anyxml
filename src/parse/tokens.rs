use crate::{
    ParserSpec,
    error::XMLError,
    sax::{parser::XMLReader, source::InputSource},
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>> XMLReader<Spec> {
    pub fn is_char(&self, c: char) -> bool {
        self.version.is_char(c)
    }

    pub fn is_whitespace(&self, c: char) -> bool {
        self.version.is_whitespace(c)
    }

    pub fn skip_whitespaces(&mut self) -> Result<usize, XMLError> {
        let mut skipped = 0;
        while let Some(w) = self.source.peek_char()? {
            if !self.is_whitespace(w) {
                break;
            }
            self.source.next_char()?;

            match w {
                '\x20' | '\t' => self.locator.update_line(|c| c + 1),
                '\n' => {
                    self.locator.set_column(1);
                    self.locator.update_line(|l| l + 1);
                }
                '\r' => {
                    if self.source.peek_char()?.is_some_and(|c| c == '\n') {
                        self.source.next_char()?;
                    }
                    self.locator.set_column(1);
                    self.locator.update_line(|l| l + 1);
                }
                _ => unimplemented!(),
            }
            skipped += 1;
        }

        Ok(skipped)
    }
}
