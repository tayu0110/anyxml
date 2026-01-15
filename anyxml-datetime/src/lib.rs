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
    OutOfRange,
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
            OutOfRange => write!(
                f,
                "The value of {} is outside the range of the domain.",
                self.segment
            ),
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

        let ret = s
            .parse()
            .map(Self)
            .map_err(|err| datetime_error!(Year, FailedToParseInt(err)))?;
        if ret.0 == 0 {
            // '0000' is not allowed in XML Schema v1.0.
            // '0000' is allowed in v1.1, but since only v1.0 is supported currently,
            // this is not an issue.
            return Err(datetime_error!(Year, OutOfRange));
        }
        Ok(ret)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NaiveMonth(u8);

impl std::fmt::Display for NaiveMonth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02}", self.0)
    }
}

impl FromStr for NaiveMonth {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ret = s
            .parse()
            .map(Self)
            .map_err(|err| datetime_error!(Month, FailedToParseInt(err)))?;
        if s.starts_with('+') {
            Err(datetime_error!(Month, InvalidFormat))
        } else if s.len() < 2 {
            Err(datetime_error!(Month, NotEnoughDigits))
        } else if !(1..=12).contains(&ret.0) {
            Err(datetime_error!(Month, OutOfRange))
        } else {
            Ok(ret)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GMonth {
    month: NaiveMonth,
    tz: Option<TimeZone>,
}

impl PartialOrd for GMonth {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.tz, other.tz) {
            (Some(stz), Some(otz)) => match self.month.cmp(&other.month) {
                std::cmp::Ordering::Equal => Some(stz.cmp(&otz)),
                cmp => Some(cmp),
            },
            (None, None) => self.month.partial_cmp(&other.month),
            _ => None,
        }
    }
}

impl std::fmt::Display for GMonth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.month)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for GMonth {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(sep) = s.bytes().position(|b| !b.is_ascii_digit()) {
            let (month, tz) = s.split_at(sep);
            Ok(Self {
                month: month.parse()?,
                tz: Some(tz.parse()?),
            })
        } else {
            Ok(Self {
                month: s.parse()?,
                tz: None,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NaiveDay(u8);

impl std::fmt::Display for NaiveDay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02}", self.0)
    }
}

impl FromStr for NaiveDay {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ret = s
            .parse()
            .map(Self)
            .map_err(|err| datetime_error!(Day, FailedToParseInt(err)))?;
        if s.starts_with('+') {
            Err(datetime_error!(Day, InvalidFormat))
        } else if s.len() < 2 {
            Err(datetime_error!(Day, NotEnoughDigits))
        } else if !(1..=31).contains(&ret.0) {
            Err(datetime_error!(Day, OutOfRange))
        } else {
            Ok(ret)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GDay {
    day: NaiveDay,
    tz: Option<TimeZone>,
}

impl PartialOrd for GDay {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.tz, other.tz) {
            (Some(stz), Some(otz)) => match self.day.cmp(&other.day) {
                std::cmp::Ordering::Equal => Some(stz.cmp(&otz)),
                cmp => Some(cmp),
            },
            (None, None) => self.day.partial_cmp(&other.day),
            _ => None,
        }
    }
}

impl std::fmt::Display for GDay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.day)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for GDay {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(sep) = s.bytes().position(|b| !b.is_ascii_digit()) {
            let (day, tz) = s.split_at(sep);
            Ok(Self {
                day: day.parse()?,
                tz: Some(tz.parse()?),
            })
        } else {
            Ok(Self {
                day: s.parse()?,
                tz: None,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NaiveYearMonth {
    year: NaiveYear,
    month: NaiveMonth,
}

impl std::fmt::Display for NaiveYearMonth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.year, self.month)
    }
}

impl FromStr for NaiveYearMonth {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (year, month) = s
            .rsplit_once('-')
            .ok_or(datetime_error!(Year, InvalidFormat))?;
        Ok(Self {
            year: year.parse()?,
            month: month.parse()?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GYearMonth {
    ym: NaiveYearMonth,
    tz: Option<TimeZone>,
}

impl PartialOrd for GYearMonth {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.tz, other.tz) {
            (Some(stz), Some(otz)) => match self.ym.cmp(&other.ym) {
                std::cmp::Ordering::Equal => Some(stz.cmp(&otz)),
                cmp => Some(cmp),
            },
            (None, None) => self.ym.partial_cmp(&other.ym),
            _ => None,
        }
    }
}

impl std::fmt::Display for GYearMonth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ym)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for GYearMonth {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 7 {
            // CCYY-MM
            Err(datetime_error!(Year, InvalidFormat))
        } else if s.ends_with('Z') {
            let (ym, tz) = s.split_at(s.len() - 1);
            Ok(Self {
                ym: ym.parse()?,
                tz: Some(tz.parse()?),
            })
        } else if let (ym, tz) = s.split_at(s.len() - 6)
            && let Ok(tz) = tz.parse()
            && let Ok(ym) = ym.parse()
        {
            Ok(Self { ym, tz: Some(tz) })
        } else {
            Ok(Self {
                ym: s.parse()?,
                tz: None,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NaiveMonthDay {
    month: NaiveMonth,
    day: NaiveDay,
}

impl std::fmt::Display for NaiveMonthDay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.month, self.day)
    }
}

impl FromStr for NaiveMonthDay {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (month, day) = s
            .split_once('-')
            .ok_or(datetime_error!(Month, InvalidFormat))?;
        Ok(Self {
            month: month.parse()?,
            day: day.parse()?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GMonthDay {
    md: NaiveMonthDay,
    tz: Option<TimeZone>,
}

impl PartialOrd for GMonthDay {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.tz, other.tz) {
            (Some(stz), Some(otz)) => match self.md.cmp(&other.md) {
                std::cmp::Ordering::Equal => Some(stz.cmp(&otz)),
                cmp => Some(cmp),
            },
            (None, None) => self.md.partial_cmp(&other.md),
            _ => None,
        }
    }
}

impl std::fmt::Display for GMonthDay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.md)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for GMonthDay {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 5 {
            // MM-DD
            Err(datetime_error!(Month, InvalidFormat))
        } else if let Some(md) = s.strip_suffix('Z') {
            Ok(Self {
                md: md.parse()?,
                tz: Some("Z".parse()?),
            })
        } else if s.len() >= 6
            && let (md, tz) = s.split_at(s.len() - 6)
            && let Ok(tz) = tz.parse()
            && let Ok(md) = md.parse()
        {
            Ok(Self { md, tz: Some(tz) })
        } else {
            Ok(Self {
                md: s.parse()?,
                tz: None,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NaiveDate {
    year: NaiveYear,
    month: NaiveMonth,
    day: NaiveDay,
}

impl std::fmt::Display for NaiveDate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}-{}", self.year, self.month, self.day)
    }
}

impl FromStr for NaiveDate {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (ym, day) = s
            .rsplit_once('-')
            .ok_or(datetime_error!(Month, InvalidFormat))?;
        let (year, month) = ym
            .rsplit_once('-')
            .ok_or(datetime_error!(Year, InvalidFormat))?;
        Ok(Self {
            year: year.parse()?,
            month: month.parse()?,
            day: day.parse()?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Date {
    ymd: NaiveDate,
    tz: Option<TimeZone>,
}

impl PartialOrd for Date {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.tz, other.tz) {
            (Some(stz), Some(otz)) => match self.ymd.cmp(&other.ymd) {
                std::cmp::Ordering::Equal => Some(stz.cmp(&otz)),
                cmp => Some(cmp),
            },
            (None, None) => self.ymd.partial_cmp(&other.ymd),
            _ => None,
        }
    }
}

impl std::fmt::Display for Date {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ymd)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for Date {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 10 {
            // CCYY-MM-DD
            Err(datetime_error!(Month, InvalidFormat))
        } else if let Some(ymd) = s.strip_suffix('Z') {
            Ok(Self {
                ymd: ymd.parse()?,
                tz: Some("Z".parse()?),
            })
        } else if let (ymd, tz) = s.split_at(s.len() - 6)
            && let Ok(tz) = tz.parse()
            && let Ok(ymd) = ymd.parse()
        {
            Ok(Self { ymd, tz: Some(tz) })
        } else {
            Ok(Self {
                ymd: s.parse()?,
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

        // '0000' is not allowed.
        assert!("0000".parse::<GYear>().is_err());
        assert!("0000+09:00".parse::<GYear>().is_err());
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
