//! A simple implementation of datetime format as defined in
//! [XML Schema Part 2: Datatypes Second Edition Appendix D ISO 8601 Date and Time Formats](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#isoformats).
//!
//! I do not target full ISO 8601 format support.

use std::{num::ParseIntError, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSegment {
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Second,
    TimeZone,
}

impl std::fmt::Display for ErrorSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Year => write!(f, "year"),
            Self::Month => write!(f, "month"),
            Self::Day => write!(f, "day"),
            Self::Hour => write!(f, "hour"),
            Self::Minute => write!(f, "minute"),
            Self::Second => write!(f, "second"),
            Self::TimeZone => write!(f, "timezone"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    NotEnoughDigits,
    FailedToParseInt(ParseIntError),
    TooLarge,
    TooSmall,
    InvalidFormat,
}

pub struct DateTimeError {
    segment: ErrorSegment,
    kind: ErrorKind,
}

impl std::fmt::Display for DateTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorKind::*;

        match self.kind {
            NotEnoughDigits => write!(f, "Not enough digits in the {}.", self.segment),
            FailedToParseInt(ref err) => {
                write!(f, "Failed to parse {} because '{err}'.", self.segment)
            }
            TooLarge => write!(f, "The {} is too large.", self.segment),
            TooSmall => write!(f, "The {} is too small.", self.segment),
            InvalidFormat => write!(f, "The format of {} is invalid.", self.segment),
        }
    }
}

macro_rules! datetime_error {
    ( $segment:ident, $kind:expr ) => {{
        use ErrorKind::*;
        use ErrorSegment::*;
        DateTimeError {
            segment: $segment,
            kind: $kind,
        }
    }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NaiveYear(i128);

impl std::fmt::Display for NaiveYear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04}", self.0)
    }
}

impl FromStr for NaiveYear {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 4 + s.starts_with('-') as usize {
            return Err(datetime_error!(Year, NotEnoughDigits));
        }

        s.parse()
            .map(Self)
            .map_err(|err| datetime_error!(Year, FailedToParseInt(err)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TimeZone(i8, u8);

impl PartialOrd for TimeZone {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimeZone {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Reverse((self.0, self.1)).cmp(&std::cmp::Reverse((other.0, other.1)))
    }
}

impl std::fmt::Display for TimeZone {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0 && self.1 == 0 {
            write!(f, "Z")
        } else {
            write!(f, "{:+02}:{:02}", self.0, self.1)
        }
    }
}

impl FromStr for TimeZone {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "Z" {
            return Ok(Self(0, 0));
        }

        let (hour, minute) = s
            .split_once(':')
            .ok_or(datetime_error!(TimeZone, InvalidFormat))?;
        if !hour.starts_with(['+', '-']) || hour.len() != 3 || minute.len() != 2 {
            return Err(datetime_error!(TimeZone, InvalidFormat))?;
        }
        let hour = hour
            .parse()
            .map_err(|err| datetime_error!(TimeZone, FailedToParseInt(err)))?;
        let minute = minute
            .parse()
            .map_err(|err| datetime_error!(TimeZone, FailedToParseInt(err)))?;

        if minute >= 60 {
            return Err(datetime_error!(TimeZone, TooLarge));
        }

        if hour < -14 || (hour == -14 && minute != 0) {
            Err(datetime_error!(TimeZone, TooSmall))
        } else if hour > 14 || (hour == 14 && minute != 0) {
            Err(datetime_error!(TimeZone, TooLarge))
        } else {
            Ok(Self(hour, minute))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GYear {
    year: NaiveYear,
    tz: Option<TimeZone>,
}

impl PartialOrd for GYear {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.tz, other.tz) {
            (Some(stz), Some(otz)) => match self.year.cmp(&other.year) {
                std::cmp::Ordering::Equal => Some(stz.cmp(&otz)),
                cmp => Some(cmp),
            },
            (None, None) => self.year.partial_cmp(&other.year),
            _ => None,
        }
    }
}

impl std::fmt::Display for GYear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.year)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for GYear {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let base = s.starts_with('-') as usize;
        if let Some(sep) = s[base..].bytes().position(|b| !b.is_ascii_digit()) {
            let (year, tz) = s.split_at(base + sep);
            Ok(Self {
                year: year.parse()?,
                tz: Some(tz.parse()?),
            })
        } else {
            Ok(Self {
                year: s.parse()?,
                tz: None,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gyear_parse_test() {
        assert!("1999".parse::<GYear>().is_ok());
        assert!("-1999".parse::<GYear>().is_ok());
        assert!("1999+09:00".parse::<GYear>().is_ok());
        assert!("1999-09:00".parse::<GYear>().is_ok());
        assert!("-1999+09:00".parse::<GYear>().is_ok());
        assert!("-1999-09:00".parse::<GYear>().is_ok());
        // edge case
        assert!("-1999+14:00".parse::<GYear>().is_ok());
        // edge case
        assert!("-1999-14:00".parse::<GYear>().is_ok());
        // edge case
        assert!("1999+09:59".parse::<GYear>().is_ok());

        assert!("231999".parse::<GYear>().is_ok());
        assert!("-231999".parse::<GYear>().is_ok());
        assert!("231999+09:00".parse::<GYear>().is_ok());
        assert!("231999-09:00".parse::<GYear>().is_ok());
        assert!("-231999+09:00".parse::<GYear>().is_ok());
        assert!("-231999-09:00".parse::<GYear>().is_ok());

        // Too large year
        assert!(
            "1000000000000000000000000000000000000000"
                .parse::<GYear>()
                .is_err()
        );
        // Too small year
        assert!(
            "-1000000000000000000000000000000000000000"
                .parse::<GYear>()
                .is_err()
        );
        // invalid timezone
        assert!("1999+12:60".parse::<GYear>().is_err());
        assert!("1999+12:000".parse::<GYear>().is_err());
        assert!("1999+012:00".parse::<GYear>().is_err());
        // unallowed positive sign
        assert!("+1999+09:00".parse::<GYear>().is_err());
        // Too large timezone
        assert!("1999+15:00".parse::<GYear>().is_err());
        // Too small timezone
        assert!("1999-15:00".parse::<GYear>().is_err());
        // Too large timezone (edge case)
        assert!("1999+14:01".parse::<GYear>().is_err());
        // Too small timezone (edge case)
        assert!("1999-14:01".parse::<GYear>().is_err());
    }
}
