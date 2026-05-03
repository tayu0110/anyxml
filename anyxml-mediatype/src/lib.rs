//! Provide data structures to support media type identification.
//!
//! While the primary focus is on identifying XML-related media types, all media types listed
//! in [IANA Media Types](https://www.iana.org/assignments/media-types/media-types.xhtml) are supported.
//!
//! Since the scope is limited to media type identification, parameters related to media
//! types—such as charset—are not processed.
//!
//! # Reference
//! - [IANA Media Types](https://www.iana.org/assignments/media-types/media-types.xhtml)
//! - [RFC2045 Multipurpose Internet Mail Extensions (MIME) Part One: Format of Internet Message Bodies](https://datatracker.ietf.org/doc/html/rfc2045)
//! - [RFC6839 Additional Media Type Structured Syntax Suffixes](https://datatracker.ietf.org/doc/html/rfc6839)
//! - [RFC7303 XML Media Types](https://datatracker.ietf.org/doc/html/rfc7303)

mod subtypes;

use std::str::FromStr;

pub use subtypes::*;

/// Media type parse error
#[derive(Debug, Clone, Copy)]
pub enum MediaTypeError {
    InvalidTopLevelType,
    InvalidSubtype,
    SyntaxError,
}

impl std::fmt::Display for MediaTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for MediaTypeError {}

/// Check if `s` is a token defined in [RFC2045 Section 5.1](https://datatracker.ietf.org/doc/html/rfc2045#section-5.1).
///
/// ```text
/// token := 1*<any (US-ASCII) CHAR except SPACE, CTLs, or tspecials>
/// tspecials :=  "(" / ")" / "<" / ">" / "@" /
///               "," / ";" / ":" / "\" / <">
///               "/" / "[" / "]" / "?" / "="
///               ; Must be in quoted-string,
///               ; to use within parameter values
/// ```
fn is_token(s: &str) -> bool {
    !s.is_empty()
        && s.bytes().all(|b| {
            b.is_ascii_graphic()
                && !matches!(
                    b,
                    b'(' | b')'
                        | b'<'
                        | b'>'
                        | b'@'
                        | b','
                        | b';'
                        | b':'
                        | b'\\'
                        | b'"'
                        | b'/'
                        | b'['
                        | b']'
                        | b'?'
                        | b'='
                )
        })
}

/// Check if `s` is a x-token defined in [RFC2045 Section 5.1](https://datatracker.ietf.org/doc/html/rfc2045#section-5.1).
///
/// ```text
/// x-token := <The two characters "X-" or "x-" followed, with
///             no intervening white space, by any token>
/// ```
fn is_x_token(s: &str) -> bool {
    (s.starts_with("X-") || s.starts_with("x-")) && is_token(&s[2..])
}

/// Top-Level Media Types.
///
/// # Reference
/// - [Top-Level Media Types](https://www.iana.org/assignments/top-level-media-types/top-level-media-types.xhtml)
pub enum TopLevelMediaType {
    Application,
    Audio,
    Example,
    Font,
    Haptics,
    Image,
    Message,
    Model,
    Multipart,
    Text,
    Video,
    Private(String),
}

impl TopLevelMediaType {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Application => "application",
            Self::Audio => "audio",
            Self::Example => "example",
            Self::Font => "font",
            Self::Haptics => "haptics",
            Self::Image => "image",
            Self::Message => "message",
            Self::Model => "model",
            Self::Multipart => "multipart",
            Self::Text => "text",
            Self::Video => "video",
            Self::Private(value) => value,
        }
    }
}

impl FromStr for TopLevelMediaType {
    type Err = MediaTypeError;

    fn from_str(s: &str) -> Result<Self, MediaTypeError> {
        if s.eq_ignore_ascii_case("application") {
            Ok(Self::Application)
        } else if s.eq_ignore_ascii_case("audio") {
            Ok(Self::Audio)
        } else if s.eq_ignore_ascii_case("example") {
            Ok(Self::Example)
        } else if s.eq_ignore_ascii_case("font") {
            Ok(Self::Font)
        } else if s.eq_ignore_ascii_case("haptics") {
            Ok(Self::Haptics)
        } else if s.eq_ignore_ascii_case("image") {
            Ok(Self::Image)
        } else if s.eq_ignore_ascii_case("message") {
            Ok(Self::Message)
        } else if s.eq_ignore_ascii_case("model") {
            Ok(Self::Model)
        } else if s.eq_ignore_ascii_case("multipart") {
            Ok(Self::Multipart)
        } else if s.eq_ignore_ascii_case("text") {
            Ok(Self::Text)
        } else if s.eq_ignore_ascii_case("video") {
            Ok(Self::Video)
        } else if is_x_token(s) {
            Ok(Self::Private(s.to_owned()))
        } else {
            Err(MediaTypeError::InvalidTopLevelType)
        }
    }
}

/// Media type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MediaType {
    Application(ApplicationSubtype),
    Audio(AudioSubtype),
    Example(ExampleSubtype),
    Font(FontSubtype),
    Haptics(HapticsSubtype),
    Image(ImageSubtype),
    Message(MessageSubtype),
    Model(ModelSubtype),
    Multipart(MultipartSubtype),
    Text(TextSubtype),
    Video(VideoSubtype),
    Private(PrivateMediaType),
}

impl MediaType {
    /// Return the string preceding the "/".
    pub fn top_level_type(&self) -> &str {
        match self {
            Self::Application(_) => "application",
            Self::Audio(_) => "audio",
            Self::Example(_) => "example",
            Self::Font(_) => "font",
            Self::Haptics(_) => "haptics",
            Self::Image(_) => "image",
            Self::Message(_) => "message",
            Self::Model(_) => "model",
            Self::Multipart(_) => "multipart",
            Self::Text(_) => "text",
            Self::Video(_) => "video",
            Self::Private(value) => value.top_level_type(),
        }
    }

    /// Return the string following the "/".
    pub fn sub_type(&self) -> &str {
        match self {
            Self::Application(sub) => sub.to_str(),
            Self::Audio(sub) => sub.to_str(),
            Self::Example(sub) => sub.to_str(),
            Self::Font(sub) => sub.to_str(),
            Self::Haptics(sub) => sub.to_str(),
            Self::Image(sub) => sub.to_str(),
            Self::Message(sub) => sub.to_str(),
            Self::Model(sub) => sub.to_str(),
            Self::Multipart(sub) => sub.to_str(),
            Self::Text(sub) => sub.to_str(),
            Self::Video(sub) => sub.to_str(),
            Self::Private(value) => value.sub_type(),
        }
    }

    /// If the subtype has a suffix starting with "+", return the string following the "+".
    pub fn suffix(&self) -> Option<&str> {
        self.sub_type().rsplit_once('+').map(|s| s.1)
    }

    /// Return whether the media type indicates an XML resource.
    ///
    /// Specifically, it returns `true` for the two media types `application/xml` and
    /// `text/xml`, or for any media type with the `+xml` suffix.
    ///
    /// # Note
    /// Since external parsed entities and DTDs are not in XML format themselves,
    /// this method returns `false`.
    ///
    /// # Reference
    /// - [RFC7303 XML Media Types](https://datatracker.ietf.org/doc/html/rfc7303)
    pub fn is_xml(&self) -> bool {
        matches!(
            self,
            Self::Application(ApplicationSubtype::Xml) | Self::Text(TextSubtype::Xml)
        ) || self
            .suffix()
            .is_some_and(|suf| suf.eq_ignore_ascii_case("xml"))
    }
}

impl std::fmt::Display for MediaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Application(sub) => write!(f, "application/{}", sub.to_str()),
            Self::Audio(sub) => write!(f, "audio/{}", sub.to_str()),
            Self::Example(sub) => write!(f, "example/{}", sub.to_str()),
            Self::Font(sub) => write!(f, "font/{}", sub.to_str()),
            Self::Haptics(sub) => write!(f, "haptics/{}", sub.to_str()),
            Self::Image(sub) => write!(f, "image/{}", sub.to_str()),
            Self::Message(sub) => write!(f, "message/{}", sub.to_str()),
            Self::Model(sub) => write!(f, "model/{}", sub.to_str()),
            Self::Multipart(sub) => write!(f, "multipart/{}", sub.to_str()),
            Self::Text(sub) => write!(f, "text/{}", sub.to_str()),
            Self::Video(sub) => write!(f, "video/{}", sub.to_str()),
            Self::Private(ty) => write!(f, "{}", ty),
        }
    }
}

impl FromStr for MediaType {
    type Err = MediaTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (sup, sub) = s.split_once('/').ok_or(MediaTypeError::SyntaxError)?;
        match sup.parse::<TopLevelMediaType>()? {
            TopLevelMediaType::Application => Ok(Self::Application(sub.parse()?)),
            TopLevelMediaType::Audio => Ok(Self::Audio(sub.parse()?)),
            TopLevelMediaType::Example => Ok(Self::Example(sub.parse()?)),
            TopLevelMediaType::Font => Ok(Self::Font(sub.parse()?)),
            TopLevelMediaType::Haptics => Ok(Self::Haptics(sub.parse()?)),
            TopLevelMediaType::Image => Ok(Self::Image(sub.parse()?)),
            TopLevelMediaType::Message => Ok(Self::Message(sub.parse()?)),
            TopLevelMediaType::Model => Ok(Self::Model(sub.parse()?)),
            TopLevelMediaType::Multipart => Ok(Self::Multipart(sub.parse()?)),
            TopLevelMediaType::Text => Ok(Self::Text(sub.parse()?)),
            TopLevelMediaType::Video => Ok(Self::Video(sub.parse()?)),
            TopLevelMediaType::Private(_) => {
                let _ = sub.parse::<PrivateSubtype>()?;
                Ok(Self::Private(PrivateMediaType { inner: s.into() }))
            }
        }
    }
}

/// Private media type prefixed by "x-" or "X-"
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrivateMediaType {
    inner: Box<str>,
}

impl PrivateMediaType {
    /// Return the string preceding the "/".
    pub fn top_level_type(&self) -> &str {
        self.inner.split_once('/').unwrap().0
    }

    /// Return the string following the "/".
    pub fn sub_type(&self) -> &str {
        self.inner.split_once('/').unwrap().1
    }
}

impl std::fmt::Display for PrivateMediaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

/// Private sub type prefixed by "x-" or "X-"
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrivateSubtype {
    inner: Box<str>,
}
impl PrivateSubtype {
    pub fn to_str(&self) -> &str {
        &self.inner
    }
}

impl FromStr for PrivateSubtype {
    type Err = MediaTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if is_x_token(s) {
            Ok(Self { inner: s.into() })
        } else {
            Err(MediaTypeError::InvalidSubtype)
        }
    }
}
