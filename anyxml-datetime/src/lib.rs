//! A simple implementation of datetime format as defined in
//! [XML Schema Part 2: Datatypes Second Edition Appendix D ISO 8601 Date and Time Formats](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#isoformats).
//!
//! I do not target full ISO 8601 format support.
//!
//! # BCE year notation
//! This crate follows the older ISO 8601 referenced by XML Schema 1.0, which
//! diverges from the BCE notation in the current ISO 8601 standard.  \
//! In the current ISO 8601 standard, "0000" denotes the year 1 BC, and "-0001"
//! denotes the year 2 BC. However, in the older ISO 8601 referenced by the schema
//! specification this crate follows, "0000" is an invalid year notation, and "-0001"
//! indicates the year 1 BC.  \
//! Since XML Schema 1.1 now conforms to the current ISO 8601, this crate may follow
//! the new notation in the future.

use std::{
    num::{NonZeroU64, ParseIntError},
    ops::{Add, AddAssign, Sub, SubAssign},
    str::FromStr,
};

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
    ParseIntError(ParseIntError),
    TooLarge,
    TooSmall,
    OutOfRange,
    InvalidFormat,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DateTimeError {
    segment: ErrorSegment,
    kind: ErrorKind,
}

impl std::fmt::Display for DateTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorKind::*;

        match self.kind {
            NotEnoughDigits => write!(f, "Not enough digits in the {}.", self.segment),
            ParseIntError(ref err) => {
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

impl std::error::Error for DateTimeError {}

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
pub struct TimeZone(i16);

impl TimeZone {
    const MIN: Self = Self(-14 * 60);
    const MAX: Self = Self(14 * 60);
    const UTC: Self = Self(0);
}

impl std::fmt::Display for TimeZone {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0 {
            write!(f, "Z")
        } else {
            let h = self.0 / 60;
            let m = self.0 % 60;
            write!(f, "{:+02}:{:02}", h, m.abs())
        }
    }
}

impl FromStr for TimeZone {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "Z" {
            return Ok(Self(0));
        }

        let (hour, minute) = s
            .split_once(':')
            .ok_or(datetime_error!(TimeZone, InvalidFormat))?;
        if !hour.starts_with(['+', '-'])
            || hour.len() != 3
            || minute.len() != 2
            || minute.starts_with(['+', '-'])
        {
            return Err(datetime_error!(TimeZone, InvalidFormat))?;
        }
        let hour = hour
            .parse::<i16>()
            .map_err(|err| datetime_error!(TimeZone, ParseIntError(err)))?;
        let minute = minute
            .parse::<i16>()
            .map_err(|err| datetime_error!(TimeZone, ParseIntError(err)))?;

        if minute >= 60 {
            return Err(datetime_error!(TimeZone, TooLarge));
        }

        if hour < -14 || (hour == -14 && minute != 0) {
            Err(datetime_error!(TimeZone, TooSmall))
        } else if hour > 14 || (hour == 14 && minute != 0) {
            Err(datetime_error!(TimeZone, TooLarge))
        } else {
            Ok(Self(hour * 60 + minute * hour.signum()))
        }
    }
}

const NUM_OF_DAYS_IN_A_MONTH: [u8; 13] = [0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NaiveYear(i128);

impl NaiveYear {
    fn checked_add(&self, rhs: i128) -> Option<Self> {
        let mut ret = self.0.checked_add(rhs)?;
        if ret == 0 {
            ret = 1;
        }
        Some(Self(ret))
    }

    fn checked_sub(&self, rhs: i128) -> Option<Self> {
        let mut ret = self.0.checked_sub(rhs)?;
        if ret == 0 {
            ret = -1;
        }
        Some(Self(ret))
    }
}

impl std::fmt::Display for NaiveYear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04}", self.0)
    }
}

impl FromStr for NaiveYear {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('+') {
            return Err(datetime_error!(Year, InvalidFormat));
        }
        if s.len() < 4 + s.starts_with('-') as usize {
            return Err(datetime_error!(Year, NotEnoughDigits));
        }

        let ret = s
            .parse()
            .map(Self)
            .map_err(|err| datetime_error!(Year, ParseIntError(err)))?;
        if ret.0 == 0 {
            // '0000' is not allowed in XML Schema v1.0.
            // '0000' is allowed in v1.1, but since only v1.0 is supported currently,
            // this is not an issue.
            return Err(datetime_error!(Year, OutOfRange));
        }
        Ok(ret)
    }
}

macro_rules! impl_add_for_naive_year {
    ( $( $t:ty ),* ) => {
        $(
            impl Add<$t> for NaiveYear {
                type Output = NaiveYear;

                fn add(self, rhs: $t) -> Self::Output {
                    let mut ret = self.0 + rhs as i128;
                    if ret == 0 {
                        ret += 1;
                    }
                    Self(ret)
                }
            }
        )*
    };
}
impl_add_for_naive_year!(i8, u8, i16, u16, i32, u32, i64, u64, i128);
macro_rules! impl_sub_for_naive_year {
    ( $( $t:ty ),* ) => {
        $(
            impl Sub<$t> for NaiveYear {
                type Output = NaiveYear;

                fn sub(self, rhs: $t) -> Self::Output {
                    let mut ret = self.0 - rhs as i128;
                    if ret == 0 {
                        ret -= 1;
                    }
                    Self(ret)
                }
            }
        )*
    };
}
impl_sub_for_naive_year!(i8, u8, i16, u16, i32, u32, i64, u64, i128);

impl Default for NaiveYear {
    fn default() -> Self {
        Self(1)
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
            .map_err(|err| datetime_error!(Month, ParseIntError(err)))?;
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

impl Default for NaiveMonth {
    fn default() -> Self {
        Self(1)
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
            .map_err(|err| datetime_error!(Day, ParseIntError(err)))?;
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

impl Default for NaiveDay {
    fn default() -> Self {
        Self(1)
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
        let ret = Self {
            month: month.parse()?,
            day: day.parse()?,
        };
        if NUM_OF_DAYS_IN_A_MONTH[ret.month.0 as usize] < ret.day.0 {
            return Err(datetime_error!(Day, TooLarge));
        }
        Ok(ret)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
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
        let len = s.len();
        if len < 10 {
            // CCYY-MM-DD
            return Err(datetime_error!(Year, InvalidFormat));
        }
        let (year, md) = s.split_at(len - 5);
        let md = md.parse::<NaiveMonthDay>()?;
        let year = year
            .strip_suffix('-')
            .ok_or(datetime_error!(Year, InvalidFormat))?;
        let year = year.parse::<NaiveYear>()?;
        if !is_leap(year) && md.month.0 == 2 && md.day.0 == 29 {
            return Err(datetime_error!(Day, TooLarge));
        }
        Ok(Self {
            year,
            month: md.month,
            day: md.day,
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

/// generated by anyxml/resources/generate-leapsecond-list.ts
///
/// # Reference
/// https://data.iana.org/time-zones/tzdb/leap-seconds.list
const INSERTED_LEAPSECONDS: &[(u16, u8, u8)] = include!("../resources/inserted-leapseconds.rs");
const REMOVED_LEAPSECONDS: &[(u16, u8, u8)] = include!("../resources/removed-leapseconds.rs");

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
struct NaiveTime {
    hour: u8,
    minute: u8,
    // support down to the nanosecond level
    nanosecond: u64,
}

impl std::fmt::Display for NaiveTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let second = self.nanosecond / NANOSECONDS_PER_SECOND;
        let mut nano = self.nanosecond % NANOSECONDS_PER_SECOND;
        write!(f, "{:02}:{:02}:{:02}", self.hour, self.minute, second)?;
        if nano != 0 {
            while nano % 10 == 0 {
                nano /= 10;
            }
            write!(f, ".{}", nano)?;
        }
        Ok(())
    }
}

impl FromStr for NaiveTime {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 8 {
            // hh:mm:ss
            return Err(datetime_error!(Hour, InvalidFormat));
        }

        macro_rules! parse_number {
            ( $s:expr, $seg:ident, $max:expr ) => {{
                let num = $s
                    .parse()
                    .map_err(|err| datetime_error!($seg, ParseIntError(err)))?;
                if $s.starts_with('+') {
                    return Err(datetime_error!($seg, InvalidFormat));
                }
                if num > $max {
                    return Err(datetime_error!($seg, TooLarge));
                }
                num
            }};
        }

        let (sh, ms) = s.split_at(2);
        let hour = parse_number!(sh, Hour, 24);
        let (sm, second) = ms
            .strip_prefix(':')
            .ok_or(datetime_error!(Hour, InvalidFormat))?
            .split_at(2);
        let minute = parse_number!(sm, Minute, 59);
        let (ss, nano) = second
            .strip_prefix(':')
            .ok_or(datetime_error!(Minute, InvalidFormat))?
            .split_at(2);
        let second: u64 = parse_number!(ss, Second, 60);
        let ret = if nano.is_empty() {
            Self {
                hour,
                minute,
                nanosecond: second * NANOSECONDS_PER_SECOND,
            }
        } else if nano.len() == 1 {
            // If a decimal point is present, at least one digit must follow it;
            // lengths less than 4 are invalid.
            return Err(datetime_error!(Second, InvalidFormat));
        } else {
            let sn = nano
                .strip_prefix('.')
                .ok_or(datetime_error!(Second, InvalidFormat))?;
            // It only supports up to nanoseconds,
            // so it does not handle values with more than 9 digits.
            let mut nano: u64 = parse_number!(sn, Second, 999_999_999);
            let base = 9 - sn.len();
            nano *= 10u64.pow(base as u32);
            Self {
                hour,
                minute,
                nanosecond: second * NANOSECONDS_PER_SECOND + nano,
            }
        };

        if ret.hour == 24 && (ret.minute != 0 || ret.nanosecond != 0) {
            return Err(datetime_error!(Hour, OutOfRange));
        }
        if ret.nanosecond == 60 * NANOSECONDS_PER_SECOND && (ret.hour != 23 || ret.minute != 59) {
            return Err(datetime_error!(Second, OutOfRange));
        }

        Ok(ret)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Time {
    time: NaiveTime,
    tz: Option<TimeZone>,
}

impl PartialOrd for Time {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.tz, other.tz) {
            (Some(stz), Some(otz)) => match self.time.cmp(&other.time) {
                std::cmp::Ordering::Equal => Some(stz.cmp(&otz)),
                cmp => Some(cmp),
            },
            (None, None) => self.time.partial_cmp(&other.time),
            _ => None,
        }
    }
}

impl std::fmt::Display for Time {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.time)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for Time {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(time) = s.strip_suffix('Z') {
            Ok(Self {
                time: time.parse()?,
                tz: Some("Z".parse()?),
            })
        } else if s.len() >= 14 && matches!(s.as_bytes()[s.len() - 6], b'+' | b'-') {
            // hh:mm:ss+zz:zz
            let (time, tz) = s.split_at(s.len() - 6);
            Ok(Self {
                time: time.parse()?,
                tz: Some(tz.parse()?),
            })
        } else {
            Ok(Self {
                time: s.parse()?,
                tz: None,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
struct NaiveDateTime {
    date: NaiveDate,
    time: NaiveTime,
}

impl std::fmt::Display for NaiveDateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}T{}", self.date, self.time)
    }
}

impl FromStr for NaiveDateTime {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 19 {
            // CCYY-MM-DDThh:mm:ss
            return Err(datetime_error!(Year, InvalidFormat));
        }

        let (date, time) = s
            .split_once('T')
            .ok_or(datetime_error!(Year, InvalidFormat))?;
        let ret = Self {
            date: date.parse()?,
            time: time.parse()?,
        };

        if ret.time.nanosecond == 60 * NANOSECONDS_PER_SECOND {
            // leap second (insertion)
            if let Ok(year) = u16::try_from(ret.date.year.0)
                && INSERTED_LEAPSECONDS
                    .binary_search(&(year, ret.date.month.0, ret.date.day.0))
                    .is_err()
            {
                return Err(datetime_error!(Second, OutOfRange));
            }
        } else if ret.time.nanosecond == 59 * NANOSECONDS_PER_SECOND {
            // leap second (removal)
            if let Ok(year) = u16::try_from(ret.date.year.0)
                && REMOVED_LEAPSECONDS
                    .binary_search(&(year, ret.date.month.0, ret.date.day.0))
                    .is_ok()
            {
                return Err(datetime_error!(Second, OutOfRange));
            }
        }

        Ok(ret)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DateTime {
    datetime: NaiveDateTime,
    tz: Option<TimeZone>,
}

impl DateTime {
    /// # Safety
    /// The datetime must be valid.
    ///
    /// Note that some values may be calculated assuming a strictly limited range,
    /// which could lead to unexpected results.
    pub const unsafe fn new_unchecked(
        year: i128,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        nanosecond: u64,
        tz: Option<TimeZone>,
    ) -> Self {
        DateTime {
            datetime: NaiveDateTime {
                date: NaiveDate {
                    year: NaiveYear(year),
                    month: NaiveMonth(month),
                    day: NaiveDay(day),
                },
                time: NaiveTime {
                    hour,
                    minute,
                    nanosecond,
                },
            },
            tz,
        }
    }

    /// # Reference
    /// - [E Adding durations to dateTimes](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#adding-durations-to-dateTimes)
    pub fn checked_add(&self, rhs: Duration) -> Option<DateTime> {
        if rhs.neg {
            return self.checked_sub(Duration { neg: false, ..rhs });
        }

        let mut ret = DateTime {
            datetime: NaiveDateTime::default(),
            tz: None,
        };

        // Months (may be modified additionally below)
        //  - temp      := S[month] + D[month]
        //  - E[month]  := modulo(temp, 1, 13)
        //  - carry     := fQuotient(temp, 1, 13)
        let temp = rhs
            .month
            .map(|m| m.get())
            .unwrap_or(0)
            .checked_add(self.datetime.date.month.0 as u64)?;
        ret.datetime.date.month.0 = ((temp - 1) % 12 + 1) as u8;
        let carry = (temp - 1) / 12;

        // Years (may be modified additionally below)
        //  - E[year]   := S[year] + D[year] + carry
        ret.datetime.date.year = self
            .datetime
            .date
            .year
            .checked_add(rhs.year.map(|y| y.get()).unwrap_or(0) as i128)?
            .checked_add(carry as i128)?;

        // Zone
        //  - E[zone]   := S[zone]
        ret.tz = self.tz;

        // Seconds
        //  - temp      := S[second] + D[second]
        //  - E[second] := modulo(temp, 60)
        //  - carry     := fQuotient(temp, 60)
        let temp = rhs
            .nanosecond
            .map(|n| n.get())
            .unwrap_or(0)
            .checked_add(self.datetime.time.nanosecond)?;
        ret.datetime.time.nanosecond = temp % (60 * NANOSECONDS_PER_SECOND);
        let carry = temp / (60 * NANOSECONDS_PER_SECOND);

        // Minutes
        //  - temp      := S[minute] + D[minute] + carry
        //  - E[minute] := modulo(temp, 60)
        //  - carry     := fQuotient(temp, 60)
        let temp = rhs
            .minute
            .map(|m| m.get())
            .unwrap_or(0)
            .checked_add(self.datetime.time.minute as u64)?
            .checked_add(carry)?;
        ret.datetime.time.minute = (temp % 60) as u8;
        let carry = temp / 60;

        // Hours
        //  - temp      := S[hour] + D[hour] + carry
        //  - E[hour]   := modulo(temp, 24)
        //  - carry     := fQuotient(temp, 24)
        let temp = rhs
            .hour
            .map(|h| h.get())
            .unwrap_or(0)
            .checked_add(self.datetime.time.hour as u64)?
            .checked_add(carry)?;
        ret.datetime.time.hour = (temp % 24) as u8;
        let carry = temp / 24;

        // Days
        //  - if S[day] > maximumDayInMonthFor(E[year], E[month])
        //      - tempDays  := maximumDayInMonthFor(E[year], E[month])
        //  - else if S[day] < 1
        //      - tempDays  := 1
        //  - else
        //      - tempDays  := S[day]
        let temp_days = self.datetime.date.day.0.clamp(
            1,
            maximum_day_in_month_for(ret.datetime.date.year, ret.datetime.date.month.0 as i8)?,
        );
        //  - E[day]    := tempDays + D[day] + carry
        let mut day = rhs
            .day
            .map(|d| d.get())
            .unwrap_or(0)
            .checked_add(temp_days as u64)?
            .checked_add(carry)?;
        //  - START LOOP
        //      - IF E[day] < 1
        //            E[day]        := E[day] + maximumDayInMonthFor(E[year], E[month] - 1)
        //            carry         := -1
        //      - ELSE IF E[day] > maximumDayInMonthFor(E[year], E[month])
        //            E[day]        := E[day] - maximumDayInMonthFor(E[year], E[month])
        //            carry         := 1
        //      - ELSE EXIT LOOP
        //      - temp      := E[month] + carry
        //      - E[month]  := modulo(temp, 1, 13)
        //      - E[year]   := E[year] + fQuotient(temp, 1, 13)
        //      - GOTO START LOOP

        // The original loop-based calculation is too slow, so I will change the approach.
        //
        // First, advance the year in units of 365*400+100-4+1 days.
        const NUM_OF_DAYS_OVER_400YEARS: u64 = 365 * 400 + 100 - 4 + 1;
        ret.datetime.date.year = ret
            .datetime
            .date
            .year
            .checked_add((day / NUM_OF_DAYS_OVER_400YEARS) as i128)?;
        day %= NUM_OF_DAYS_OVER_400YEARS;
        // Next, advance the year in units of 100 years.
        const NUM_OF_DAYS_OVER_100YEARS: u64 = 365 * 100 + 25 - 1;
        ret.datetime.date.year = ret
            .datetime
            .date
            .year
            .checked_add((day / NUM_OF_DAYS_OVER_100YEARS) as i128)?;
        day %= NUM_OF_DAYS_OVER_100YEARS;
        // Next, advance the year in units of 4 years.
        const NUM_OF_DAYS_OVER_4YEARS: u64 = 365 * 4 + 1;
        ret.datetime.date.year = ret
            .datetime
            .date
            .year
            .checked_add((day / NUM_OF_DAYS_OVER_4YEARS) as i128)?;
        day %= NUM_OF_DAYS_OVER_4YEARS;
        // Finally, it is determined using a loop method.It should take no more than 50 iterations.
        // While advancing one year at a time is possible, handling boundary cases is complex,
        // so I determine the last four years using a loop.
        loop {
            let temp = if day < 1 {
                let month = ret.datetime.date.month.0 as i8 - 1;
                day = day
                    .checked_add(maximum_day_in_month_for(ret.datetime.date.year, month)? as u64)?;
                month
            } else if let max_days =
                maximum_day_in_month_for(ret.datetime.date.year, ret.datetime.date.month.0 as i8)?
                    as u64
                && day > max_days
            {
                day -= max_days;
                ret.datetime.date.month.0 as i8 + 1
            } else {
                break;
            };

            ret.datetime.date.month.0 = (temp - 1).rem_euclid(12) as u8 + 1;
            ret.datetime.date.year = ret
                .datetime
                .date
                .year
                .checked_add((temp - 1).div_euclid(12) as i128)?;
        }
        ret.datetime.date.day.0 = day as u8;

        Some(ret)
    }

    /// # Reference
    /// - [E Adding durations to dateTimes](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#adding-durations-to-dateTimes)
    pub fn checked_sub(&self, rhs: Duration) -> Option<DateTime> {
        if rhs.neg {
            return self.checked_add(Duration { neg: false, ..rhs });
        }

        let mut ret = DateTime {
            datetime: NaiveDateTime::default(),
            tz: None,
        };

        // Months (may be modified additionally below)
        //  - temp      := S[month] + D[month]
        //  - E[month]  := modulo(temp, 1, 13)
        //  - carry     := fQuotient(temp, 1, 13)
        let temp = (self.datetime.date.month.0 as i128)
            .checked_sub(rhs.month.map(|m| m.get() as i128).unwrap_or(0))?;
        ret.datetime.date.month.0 = temp.checked_sub(1)?.rem_euclid(12) as u8 + 1;
        let carry = (temp - 1).div_euclid(12);

        // Years (may be modified additionally below)
        //  - E[year]   := S[year] + D[year] + carry
        ret.datetime.date.year = self
            .datetime
            .date
            .year
            .checked_sub(rhs.year.map(|y| y.get()).unwrap_or(0) as i128)?
            .checked_add(carry)?;

        // Zone
        //  - E[zone]   := S[zone]
        ret.tz = self.tz;

        // Seconds
        //  - temp      := S[second] + D[second]
        //  - E[second] := modulo(temp, 60)
        //  - carry     := fQuotient(temp, 60)
        let temp = (self.datetime.time.nanosecond as i128)
            .checked_sub(rhs.nanosecond.map(|n| n.get()).unwrap_or(0) as i128)?;
        ret.datetime.time.nanosecond =
            temp.rem_euclid((60 * NANOSECONDS_PER_SECOND) as i128) as u64;
        let carry = temp.div_euclid((60 * NANOSECONDS_PER_SECOND) as i128);

        // Minutes
        //  - temp      := S[minute] + D[minute] + carry
        //  - E[minute] := modulo(temp, 60)
        //  - carry     := fQuotient(temp, 60)
        let temp = (self.datetime.time.minute as i128)
            .checked_sub(rhs.minute.map(|m| m.get()).unwrap_or(0) as i128)?
            .checked_add(carry)?;
        ret.datetime.time.minute = temp.rem_euclid(60) as u8;
        let carry = temp.div_euclid(60);

        // Hours
        //  - temp      := S[hour] + D[hour] + carry
        //  - E[hour]   := modulo(temp, 24)
        //  - carry     := fQuotient(temp, 24)
        let temp = (self.datetime.time.hour as i128)
            .checked_sub(rhs.hour.map(|h| h.get()).unwrap_or(0) as i128)?
            .checked_add(carry)?;
        ret.datetime.time.hour = temp.rem_euclid(24) as u8;
        let carry = temp.div_euclid(24);

        // Days
        //  - if S[day] > maximumDayInMonthFor(E[year], E[month])
        //      - tempDays  := maximumDayInMonthFor(E[year], E[month])
        //  - else if S[day] < 1
        //      - tempDays  := 1
        //  - else
        //      - tempDays  := S[day]
        let temp_days = self.datetime.date.day.0.clamp(
            1,
            maximum_day_in_month_for(ret.datetime.date.year, ret.datetime.date.month.0 as i8)?,
        );
        //  - E[day]    := tempDays + D[day] + carry
        let mut day = (temp_days as i128)
            .checked_sub(rhs.day.map(|d| d.get()).unwrap_or(0) as i128)?
            .checked_add(carry)?;
        //  - START LOOP
        //      - IF E[day] < 1
        //            E[day]        := E[day] + maximumDayInMonthFor(E[year], E[month] - 1)
        //            carry         := -1
        //      - ELSE IF E[day] > maximumDayInMonthFor(E[year], E[month])
        //            E[day]        := E[day] - maximumDayInMonthFor(E[year], E[month])
        //            carry         := 1
        //      - ELSE EXIT LOOP
        //      - temp      := E[month] + carry
        //      - E[month]  := modulo(temp, 1, 13)
        //      - E[year]   := E[year] + fQuotient(temp, 1, 13)
        //      - GOTO START LOOP

        // The original loop-based calculation is too slow, so I will change the approach.
        //
        // First, advance the year in units of 365*400+100-4+1 days.
        const NUM_OF_DAYS_OVER_400YEARS: i128 = 365 * 400 + 100 - 4 + 1;
        ret.datetime.date.year = ret
            .datetime
            .date
            .year
            .checked_add(day / NUM_OF_DAYS_OVER_400YEARS)?;
        day %= NUM_OF_DAYS_OVER_400YEARS;
        // Next, advance the year in units of 100 years.
        const NUM_OF_DAYS_OVER_100YEARS: i128 = 365 * 100 + 25 - 1;
        ret.datetime.date.year = ret
            .datetime
            .date
            .year
            .checked_add(day / NUM_OF_DAYS_OVER_100YEARS)?;
        day %= NUM_OF_DAYS_OVER_100YEARS;
        // Next, advance the year in units of 4 years.
        const NUM_OF_DAYS_OVER_4YEARS: i128 = 365 * 4 + 1;
        ret.datetime.date.year = ret
            .datetime
            .date
            .year
            .checked_add(day / NUM_OF_DAYS_OVER_4YEARS)?;
        day %= NUM_OF_DAYS_OVER_4YEARS;
        // Finally, it is determined using a loop method.It should take no more than 50 iterations.
        // While advancing one year at a time is possible, handling boundary cases is complex,
        // so I determine the last four years using a loop.
        loop {
            let temp = if day < 1 {
                let month = ret.datetime.date.month.0 as i8 - 1;
                day = day
                    .checked_add(maximum_day_in_month_for(ret.datetime.date.year, month)? as i128)?;
                month
            } else if let max_days =
                maximum_day_in_month_for(ret.datetime.date.year, ret.datetime.date.month.0 as i8)?
                    as i128
                && day > max_days
            {
                day -= max_days;
                ret.datetime.date.month.0 as i8 + 1
            } else {
                break;
            };

            ret.datetime.date.month.0 = (temp - 1).rem_euclid(12) as u8 + 1;
            ret.datetime.date.year = ret
                .datetime
                .date
                .year
                .checked_add((temp - 1).div_euclid(12) as i128)?;
        }
        ret.datetime.date.day.0 = day as u8;

        Some(ret)
    }

    /// If a time zone is set, change it to UTC. If no time zone is set, do nothing.
    pub fn to_utc(&self) -> DateTime {
        let Some(tz) = self.tz else {
            return *self;
        };

        // self is already set to UTC
        if tz.0 == 0 {
            return *self;
        }

        let h = tz.0 / 60;
        let m = tz.0 % 60;
        let duration = Duration {
            neg: tz.0 > 0,
            hour: NonZeroU64::new(h.unsigned_abs() as u64),
            minute: NonZeroU64::new(m.unsigned_abs() as u64),
            ..Default::default()
        };

        DateTime {
            tz: Some(TimeZone(0)),
            ..*self + duration
        }
    }
}

impl PartialOrd for DateTime {
    /// # Reference
    /// - [3.2.7.4 Order relation on dateTime](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#dateTime)
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let lhs = self.to_utc();
        let rhs = other.to_utc();

        if lhs.tz.is_some() == rhs.tz.is_some() {
            Some(lhs.datetime.cmp(&rhs.datetime))
        } else if lhs.tz.is_some() {
            let min = DateTime {
                datetime: rhs.datetime,
                tz: Some(TimeZone::MAX),
            };
            let max = DateTime {
                datetime: rhs.datetime,
                tz: Some(TimeZone::MIN),
            };
            if lhs < min {
                Some(std::cmp::Ordering::Less)
            } else if max < lhs {
                Some(std::cmp::Ordering::Greater)
            } else {
                None
            }
        } else {
            let min = DateTime {
                datetime: lhs.datetime,
                tz: Some(TimeZone::MAX),
            };
            let max = DateTime {
                datetime: lhs.datetime,
                tz: Some(TimeZone::MIN),
            };
            if max < rhs {
                Some(std::cmp::Ordering::Less)
            } else if rhs < min {
                Some(std::cmp::Ordering::Greater)
            } else {
                None
            }
        }
    }
}

impl PartialEq for DateTime {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other).is_some_and(|ret| ret.is_eq())
    }
}

impl std::fmt::Display for DateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.datetime)?;
        if let Some(tz) = self.tz.as_ref() {
            write!(f, "{}", tz)?;
        }
        Ok(())
    }
}

impl FromStr for DateTime {
    type Err = DateTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(datetime) = s.strip_suffix('Z') {
            Ok(Self {
                datetime: datetime.parse()?,
                tz: Some("Z".parse()?),
            })
        } else if s.len() >= 25 && matches!(s.as_bytes()[s.len() - 6], b'+' | b'-') {
            // CCYY-MM-DDThh:mm:ss+zz:zz
            let (datetime, tz) = s.split_at(s.len() - 6);
            Ok(Self {
                datetime: datetime.parse()?,
                tz: Some(tz.parse()?),
            })
        } else {
            Ok(Self {
                datetime: s.parse()?,
                tz: None,
            })
        }
    }
}

impl Add<Duration> for DateTime {
    type Output = DateTime;

    fn add(self, rhs: Duration) -> Self::Output {
        self.checked_add(rhs).expect("attempt to add with overflow")
    }
}

impl AddAssign<Duration> for DateTime {
    fn add_assign(&mut self, rhs: Duration) {
        *self = *self + rhs;
    }
}

impl Sub<Duration> for DateTime {
    type Output = DateTime;

    fn sub(self, rhs: Duration) -> Self::Output {
        self.checked_sub(rhs)
            .expect("attempt to subtract with overflow")
    }
}

impl SubAssign<Duration> for DateTime {
    fn sub_assign(&mut self, rhs: Duration) {
        *self = *self - rhs;
    }
}

const NANOSECONDS_PER_SECOND: u64 = 1_000_000_000;

#[derive(Debug, Clone, Copy, Default)]
pub struct Duration {
    neg: bool,
    year: Option<NonZeroU64>,
    month: Option<NonZeroU64>,
    day: Option<NonZeroU64>,
    hour: Option<NonZeroU64>,
    minute: Option<NonZeroU64>,
    nanosecond: Option<NonZeroU64>,
}

impl PartialOrd for Duration {
    /// # Reference
    /// - [3.2.6.2 Order relation on duration](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#duration)
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // Since the suggested optimization method is not well understood,
        // comparisons will be made based on the baseline dates provided
        // in the specifications unless critical performance degradation occurs.

        // 1696-09-01T00:00:00Z
        // 1697-02-01T00:00:00Z
        // 1903-03-01T00:00:00Z
        // 1903-07-01T00:00:00Z
        const BASELINE: [DateTime; 4] = unsafe {
            // # Safety
            // The datetimes specified in the specification,
            // and also datetimes whose existence is obvious.
            [
                DateTime::new_unchecked(1696, 9, 1, 0, 0, 0, Some(TimeZone::UTC)),
                DateTime::new_unchecked(1697, 2, 1, 0, 0, 0, Some(TimeZone::UTC)),
                DateTime::new_unchecked(1903, 3, 1, 0, 0, 0, Some(TimeZone::UTC)),
                DateTime::new_unchecked(1903, 7, 1, 0, 0, 0, Some(TimeZone::UTC)),
            ]
        };

        let ret0 = (BASELINE[0] + *self).partial_cmp(&(BASELINE[0] + *other))?;
        let ret1 = (BASELINE[1] + *self).partial_cmp(&(BASELINE[1] + *other))?;
        let ret2 = (BASELINE[2] + *self).partial_cmp(&(BASELINE[2] + *other))?;
        let ret3 = (BASELINE[3] + *self).partial_cmp(&(BASELINE[3] + *other))?;
        (ret0 == ret1 && ret0 == ret2 && ret0 == ret3).then_some(ret0)
    }
}

impl PartialEq for Duration {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other).is_some_and(|ret| ret.is_eq())
    }
}

impl std::fmt::Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.neg {
            write!(f, "-")?;
        }
        write!(f, "P")?;
        if self.year.is_none()
            && self.month.is_none()
            && self.day.is_none()
            && self.hour.is_none()
            && self.minute.is_none()
            && self.nanosecond.is_none()
        {
            write!(f, "0Y")?;
            return Ok(());
        }
        if let Some(year) = self.year {
            write!(f, "{}Y", year)?;
        }
        if let Some(month) = self.month {
            write!(f, "{}M", month)?;
        }
        if let Some(day) = self.day {
            write!(f, "{}D", day)?;
        }

        match (self.hour, self.minute, self.nanosecond) {
            (None, None, None) => {}
            (hour, minute, nanosecond) => {
                write!(f, "T")?;
                if let Some(hour) = hour {
                    write!(f, "{}H", hour)?;
                }
                if let Some(minute) = minute {
                    write!(f, "{}M", minute)?;
                }
                if let Some(nanosecond) = nanosecond {
                    let sec = nanosecond.get() / NANOSECONDS_PER_SECOND;
                    let mut nano = nanosecond.get() % NANOSECONDS_PER_SECOND;
                    if nano == 0 {
                        write!(f, "{}S", sec)?;
                    } else {
                        while nano % 10 == 0 {
                            nano /= 10;
                        }
                        write!(f, "{}.{}S", sec, nano)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl FromStr for Duration {
    type Err = DateTimeError;

    fn from_str(mut s: &str) -> Result<Self, Self::Err> {
        let mut neg = false;
        if let Some(rem) = s.strip_prefix('-') {
            neg = true;
            s = rem;
        }

        s = s
            .strip_prefix('P')
            .ok_or(datetime_error!(Year, InvalidFormat))?;
        if s.is_empty() {
            return Err(datetime_error!(Year, InvalidFormat))?;
        }

        let mut year = None;
        let mut month = None;
        let mut day = None;
        let mut hour = None;
        let mut minute = None;
        let mut nanosecond = None;

        macro_rules! parse_number {
            ( $s:expr, $ind:literal, $seg:ident, $res:expr ) => {
                if let Some((seg, rem)) = $s.split_once($ind) {
                    let seg = seg
                        .parse::<u64>()
                        .map_err(|err| datetime_error!($seg, ParseIntError(err)))?;
                    $res = NonZeroU64::new(seg);
                    $s = rem;
                }
            };
        }
        parse_number!(s, 'Y', Year, year);
        parse_number!(s, 'M', Month, month);
        parse_number!(s, 'D', Day, day);
        if let Some(rem) = s.strip_prefix('T') {
            s = rem;
            if s.is_empty() {
                return Err(datetime_error!(Hour, InvalidFormat));
            }
            parse_number!(s, 'H', Hour, hour);
            parse_number!(s, 'M', Minute, minute);
            if let Some((sec, rem)) = s.split_once('S') {
                s = rem;

                if let Some((sec, frac)) = sec.split_once('.') {
                    let sec = sec
                        .parse::<u64>()
                        .map_err(|err| datetime_error!(Second, ParseIntError(err)))?
                        .checked_mul(NANOSECONDS_PER_SECOND)
                        .ok_or(datetime_error!(Second, TooLarge))?;
                    let base = 10u64.pow(9 - frac.len() as u32);
                    let frac = frac
                        .parse::<u64>()
                        .map_err(|err| datetime_error!(Second, ParseIntError(err)))?
                        .checked_mul(base)
                        .ok_or(datetime_error!(Second, TooLarge))?;
                    let nano = sec
                        .checked_add(frac)
                        .ok_or(datetime_error!(Second, TooLarge))?;
                    nanosecond = NonZeroU64::new(nano);
                } else {
                    let sec = sec
                        .parse::<u64>()
                        .map_err(|err| datetime_error!(Second, ParseIntError(err)))?;
                    let nano = sec
                        .checked_mul(NANOSECONDS_PER_SECOND)
                        .ok_or(datetime_error!(Second, TooLarge))?;
                    nanosecond = NonZeroU64::new(nano);
                }
            }
        }

        if !s.is_empty() {
            return Err(datetime_error!(Second, InvalidFormat));
        }

        Ok(Self {
            neg,
            year,
            month,
            day,
            hour,
            minute,
            nanosecond,
        })
    }
}

fn is_leap(year: NaiveYear) -> bool {
    year.0 % 400 == 0 || (year.0 % 100 != 0 && year.0 % 4 == 0)
}

/// If overflow occurs, return [`None`].
///
/// # Reference
/// - [E.1 Algorithm](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#adding-durations-to-dateTimes)
fn maximum_day_in_month_for(year: NaiveYear, month: i8) -> Option<u8> {
    let m = (month - 1).rem_euclid(12) + 1;
    let mut y = year.0.checked_add((month - 1).div_euclid(12) as i128)?;
    if y == 0 {
        y = 1;
    }
    if m == 2 {
        Some(28 + is_leap(NaiveYear(y)) as u8)
    } else {
        Some(NUM_OF_DAYS_IN_A_MONTH[m as usize])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gyear_parse_test() {
        assert!("1999".parse::<GYear>().is_ok());
        assert!("-1999".parse::<GYear>().is_ok());
        assert!("1999Z".parse::<GYear>().is_ok());
        assert!("-1999Z".parse::<GYear>().is_ok());
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
        assert!("+1999".parse::<GYear>().is_err());
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

    #[test]
    fn gmonth_parse_test() {
        assert!("12".parse::<GMonth>().is_ok());
        assert!("12Z".parse::<GMonth>().is_ok());
        assert!("12+09:00".parse::<GMonth>().is_ok());
        assert!("12-09:00".parse::<GMonth>().is_ok());

        // '00' is not allowed.
        assert!("00".parse::<GMonth>().is_err());
        assert!("00+09:00".parse::<GMonth>().is_err());
        // Too large month
        assert!("13".parse::<GMonth>().is_err());
        // invalid timezone
        assert!("12+12:60".parse::<GMonth>().is_err());
        assert!("12+12:000".parse::<GMonth>().is_err());
        assert!("12+012:00".parse::<GMonth>().is_err());
        // unallowed positive sign
        assert!("+12+09:00".parse::<GMonth>().is_err());
        // unallowed negative sign
        assert!("-12+09:00".parse::<GMonth>().is_err());
    }

    #[test]
    fn gday_parse_test() {
        assert!("08".parse::<GDay>().is_ok());
        assert!("08Z".parse::<GDay>().is_ok());
        assert!("08+09:00".parse::<GDay>().is_ok());
        assert!("08-09:00".parse::<GDay>().is_ok());

        // '00' is not allowed.
        assert!("00".parse::<GDay>().is_err());
        assert!("00+09:00".parse::<GDay>().is_err());
        // Too large day
        assert!("32".parse::<GDay>().is_err());
        // invalid timezone
        assert!("12+12:60".parse::<GDay>().is_err());
        assert!("12+12:000".parse::<GDay>().is_err());
        assert!("12+012:00".parse::<GDay>().is_err());
        // unallowed positive sign
        assert!("+12+09:00".parse::<GDay>().is_err());
        // unallowed negative sign
        assert!("-12+09:00".parse::<GDay>().is_err());
    }

    #[test]
    fn gyearmonth_parse_test() {
        assert!("2015-05".parse::<GYearMonth>().is_ok());
        assert!("2015-05Z".parse::<GYearMonth>().is_ok());
        assert!("2015-05+09:00".parse::<GYearMonth>().is_ok());
        assert!("2015-05-09:00".parse::<GYearMonth>().is_ok());
        assert!("-0660-02+09:00".parse::<GYearMonth>().is_ok());
        assert!("-0660-02-09:00".parse::<GYearMonth>().is_ok());

        // invalid timezone
        assert!("2015-05+12:60".parse::<GYearMonth>().is_err());
        assert!("2015-05+12:000".parse::<GYearMonth>().is_err());
        assert!("2025-05+012:00".parse::<GYearMonth>().is_err());
        // unallowed positive sign
        assert!("+2015-05+09:00".parse::<GYearMonth>().is_err());
    }

    #[test]
    fn gmonthday_parse_test() {
        assert!("05-15".parse::<GMonthDay>().is_ok());
        assert!("05-15Z".parse::<GMonthDay>().is_ok());
        assert!("05-15+09:00".parse::<GMonthDay>().is_ok());
        assert!("05-15-09:00".parse::<GMonthDay>().is_ok());
        assert!("02-11+09:00".parse::<GMonthDay>().is_ok());
        assert!("02-11-09:00".parse::<GMonthDay>().is_ok());
        // edge case
        assert!("01-31".parse::<GMonthDay>().is_ok());
        assert!("02-29".parse::<GMonthDay>().is_ok());
        assert!("03-31".parse::<GMonthDay>().is_ok());
        assert!("04-30".parse::<GMonthDay>().is_ok());
        assert!("05-31".parse::<GMonthDay>().is_ok());
        assert!("06-30".parse::<GMonthDay>().is_ok());
        assert!("07-31".parse::<GMonthDay>().is_ok());
        assert!("08-31".parse::<GMonthDay>().is_ok());
        assert!("09-30".parse::<GMonthDay>().is_ok());
        assert!("10-31".parse::<GMonthDay>().is_ok());
        assert!("11-30".parse::<GMonthDay>().is_ok());
        assert!("12-31".parse::<GMonthDay>().is_ok());

        // invalid timezone
        assert!("05-15+12:60".parse::<GMonthDay>().is_err());
        assert!("05-15+12:000".parse::<GMonthDay>().is_err());
        assert!("05-15+012:00".parse::<GMonthDay>().is_err());
        // unallowed positive sign
        assert!("+05-15+09:00".parse::<GMonthDay>().is_err());
        // out of range
        assert!("01-32".parse::<GMonthDay>().is_err());
        assert!("02-30".parse::<GMonthDay>().is_err());
        assert!("03-32".parse::<GMonthDay>().is_err());
        assert!("04-31".parse::<GMonthDay>().is_err());
        assert!("05-32".parse::<GMonthDay>().is_err());
        assert!("06-31".parse::<GMonthDay>().is_err());
        assert!("07-32".parse::<GMonthDay>().is_err());
        assert!("08-32".parse::<GMonthDay>().is_err());
        assert!("09-31".parse::<GMonthDay>().is_err());
        assert!("10-32".parse::<GMonthDay>().is_err());
        assert!("11-31".parse::<GMonthDay>().is_err());
        assert!("12-32".parse::<GMonthDay>().is_err());
    }

    #[test]
    fn date_parse_test() {
        assert!("2015-05-15".parse::<Date>().is_ok());
        assert!("2015-05-15Z".parse::<Date>().is_ok());
        assert!("2015-05-15+09:00".parse::<Date>().is_ok());
        assert!("2015-05-15-09:00".parse::<Date>().is_ok());
        assert!("-0660-02-11+09:00".parse::<Date>().is_ok());
        assert!("-0660-02-11-09:00".parse::<Date>().is_ok());
        // leap year
        assert!("2024-02-29".parse::<Date>().is_ok());
        assert!("2000-02-29".parse::<Date>().is_ok());

        // invalid timezone
        assert!("2015-05-15+12:60".parse::<Date>().is_err());
        assert!("2015-05-15+12:000".parse::<Date>().is_err());
        assert!("2015-05-15+012:00".parse::<Date>().is_err());
        // unallowed positive sign
        assert!("+2015-05-15+09:00".parse::<Date>().is_err());
        // non-leap year
        assert!("2015-02-29".parse::<Date>().is_err());
        assert!("1900-02-29".parse::<Date>().is_err());
    }

    #[test]
    fn time_parse_test() {
        assert!("00:00:00".parse::<Time>().is_ok());
        assert!("12:00:00".parse::<Time>().is_ok());
        // 24:00:00 is allowed
        assert!("24:00:00".parse::<Time>().is_ok());
        // leap second (inserted)
        assert!("23:59:60".parse::<Time>().is_ok());
        assert!("12:30:00Z".parse::<Time>().is_ok());
        assert!("12:00:30+09:00".parse::<Time>().is_ok());
        assert!("12:15:15-09:00".parse::<Time>().is_ok());

        assert!("25:00:00".parse::<Time>().is_err());
        assert!("12:60:00".parse::<Time>().is_err());
        assert!("09:00:60".parse::<Time>().is_err());
        assert!("9:00:00".parse::<Time>().is_err());
        assert!("+09:00:00".parse::<Time>().is_err());
        assert!("-09:00:00".parse::<Time>().is_err());
        assert!("+9:00:00".parse::<Time>().is_err());
        assert!("-9:00:00".parse::<Time>().is_err());
        assert!("09:+0:00".parse::<Time>().is_err());
        assert!("09:-0:00".parse::<Time>().is_err());
        assert!("09:00:+0".parse::<Time>().is_err());
        assert!("09:00:-0".parse::<Time>().is_err());
        assert!("09:00:00++1:00".parse::<Time>().is_err());
        assert!("09:00:00+-1:00".parse::<Time>().is_err());
        assert!("09:00:00-+1:00".parse::<Time>().is_err());
        assert!("09:00:00--1:00".parse::<Time>().is_err());
        assert!("09:00:00+01:+0".parse::<Time>().is_err());
        assert!("09:00:00+01:-0".parse::<Time>().is_err());
        // unallowed positive sign
        assert!("+09:00:10".parse::<Time>().is_err());
        // unallowed negative sign
        assert!("-09:00:10".parse::<Time>().is_err());
        // invalid leap second
        assert!("23:00:60".parse::<Time>().is_err());
        // '24' is not allowed as hour other than '24:00:00'
        assert!("24:00:01".parse::<Time>().is_err());
    }

    #[test]
    fn datetime_parse_test() {
        assert!("2000-01-20T12:00:00-13:00".parse::<DateTime>().is_ok());
        assert!("2000-01-20T12:00:00Z".parse::<DateTime>().is_ok());
        assert!("2000-01-12T12:13:14Z".parse::<DateTime>().is_ok());
        assert!("-0660-02-11T00:00:00+09:00".parse::<DateTime>().is_ok());

        assert!("+2000-01-20T12:00:00-13:00".parse::<DateTime>().is_err());
        assert!("2000-01-20t12:00:00-13:00".parse::<DateTime>().is_err());
        assert!("2000-+1-20T12:00:00-13:00".parse::<DateTime>().is_err());
        assert!("+000-01-20T12:00:00-13:00".parse::<DateTime>().is_err());
        assert!("0000-01-20T12:00:00-13:00".parse::<DateTime>().is_err());
        assert!("2000-01-2012:00:00-13:00".parse::<DateTime>().is_err());
        assert!("2000001-20T12:00:00-13:00".parse::<DateTime>().is_err());
        assert!("2000-01-20T-12:00".parse::<DateTime>().is_err());
    }

    #[test]
    fn duration_parse_test() {
        assert!("P1347Y".parse::<Duration>().is_ok());
        assert!("P1347M".parse::<Duration>().is_ok());
        assert!("P1Y2MT2H".parse::<Duration>().is_ok());
        assert!("P0Y1347M".parse::<Duration>().is_ok());
        assert!("P0Y1347M0D".parse::<Duration>().is_ok());
        assert!("-P1347M".parse::<Duration>().is_ok());

        assert!("P-1347M".parse::<Duration>().is_err());
        assert!("P1Y2MT".parse::<Duration>().is_err());
    }

    #[test]
    fn datetime_addition_test() {
        let datetime = "2000-01-12T12:13:14Z".parse::<DateTime>().unwrap();
        let duration = "P1Y3M5DT7H10M3.3S".parse::<Duration>().unwrap();
        let ret = datetime + duration;
        assert_eq!(ret.to_string(), "2001-04-17T19:23:17.3Z");

        let datetime = "2000-01-01T00:00:00Z".parse::<DateTime>().unwrap();
        let duration = "-P3M".parse::<Duration>().unwrap();
        let ret = datetime + duration;
        assert_eq!(ret.to_string(), "1999-10-01T00:00:00Z");

        let datetime = "2000-01-12T00:00:00Z".parse::<DateTime>().unwrap();
        let duration = "PT33H".parse::<Duration>().unwrap();
        let ret = datetime + duration;
        assert_eq!(ret.to_string(), "2000-01-13T09:00:00Z");

        let datetime = "2000-03-04T23:00:00+03:00".parse::<DateTime>().unwrap();
        let utc = datetime.to_utc();
        assert_eq!(utc.to_string(), "2000-03-04T20:00:00Z");
    }

    #[test]
    fn datetime_comparison_test() {
        // Determinate
        assert!(
            "2000-01-15T00:00:00".parse::<DateTime>().unwrap()
                < "2000-02-15T00:00:00".parse::<DateTime>().unwrap()
        );
        assert!(
            "2000-01-15T12:00:00".parse::<DateTime>().unwrap()
                < "2000-01-16T12:00:00Z".parse::<DateTime>().unwrap()
        );

        // Indeterminate
        assert!(
            "2000-01-01T12:00:00"
                .parse::<DateTime>()
                .unwrap()
                .partial_cmp(&"1999-12-31T23:00:00Z".parse().unwrap())
                .is_none()
        );
        assert!(
            "2000-01-16T12:00:00"
                .parse::<DateTime>()
                .unwrap()
                .partial_cmp(&"2000-01-16T12:00:00Z".parse().unwrap())
                .is_none()
        );
        assert!(
            "2000-01-16T00:00:00"
                .parse::<DateTime>()
                .unwrap()
                .partial_cmp(&"2000-01-16T12:00:00Z".parse().unwrap())
                .is_none()
        );
    }

    #[test]
    fn duration_comparison_test() {
        let p1y = "P1Y".parse::<Duration>().unwrap();
        assert!(p1y > "P364D".parse().unwrap());
        assert!(p1y < "P367D".parse().unwrap());
        assert!(p1y.partial_cmp(&"P365D".parse().unwrap()).is_none());
        assert!(p1y.partial_cmp(&"P366D".parse().unwrap()).is_none());

        let p1m = "P1M".parse::<Duration>().unwrap();
        assert!(p1m > "P27D".parse().unwrap());
        assert!(p1m < "P32D".parse().unwrap());
        assert!(p1m.partial_cmp(&"P28D".parse().unwrap()).is_none());
        assert!(p1m.partial_cmp(&"P29D".parse().unwrap()).is_none());
        assert!(p1m.partial_cmp(&"P30D".parse().unwrap()).is_none());
        assert!(p1m.partial_cmp(&"P31D".parse().unwrap()).is_none());

        let p5m = "P5M".parse::<Duration>().unwrap();
        assert!(p5m > "P149D".parse().unwrap());
        assert!(p5m < "P154D".parse().unwrap());
        assert!(p5m.partial_cmp(&"P150D".parse().unwrap()).is_none());
        assert!(p5m.partial_cmp(&"P151D".parse().unwrap()).is_none());
        assert!(p5m.partial_cmp(&"P152D".parse().unwrap()).is_none());
        assert!(p5m.partial_cmp(&"P153D".parse().unwrap()).is_none());
    }
}
