//! Date patterns with the semantics of Java's `SimpleDateFormat` (lenient, English names).

use chrono::{DateTime, Datelike, NaiveDate, Timelike, Utc};

use super::zones::{match_zone_name, ParsedZone, Zone};

const PATTERN_LETTERS: &str = "GyMdkHmsSEDFwWahKzZYuXL";

const MONTHS: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];
const WEEKDAYS: [&str; 7] =
    ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Literal(String),
    Field(char, usize),
}

impl Token {
    fn is_numeric(&self) -> bool {
        match self {
            Token::Field('M' | 'L', n) => *n <= 2,
            Token::Field(c, _) => "yYdHkKhmsSDFwWu".contains(*c),
            Token::Literal(_) => false,
        }
    }
}

/// A compiled date pattern.
#[derive(Debug, Clone)]
pub struct JavaPattern {
    tokens: Vec<Token>,
}

impl JavaPattern {
    /// Compiles a pattern. The error text matches Java's `IllegalArgumentException` message.
    pub fn compile(pattern: &str) -> Result<JavaPattern, String> {
        let mut tokens: Vec<Token> = Vec::new();
        let push_literal = |tokens: &mut Vec<Token>, c: char| match tokens.last_mut() {
            Some(Token::Literal(s)) => s.push(c),
            _ => tokens.push(Token::Literal(c.to_string())),
        };
        let chars: Vec<char> = pattern.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            let c = chars[i];
            if c == '\'' {
                if chars.get(i + 1) == Some(&'\'') {
                    push_literal(&mut tokens, '\'');
                    i += 2;
                    continue;
                }
                i += 1;
                loop {
                    match chars.get(i) {
                        None => return Err("Unterminated quote".into()),
                        Some('\'') if chars.get(i + 1) == Some(&'\'') => {
                            push_literal(&mut tokens, '\'');
                            i += 2;
                        }
                        Some('\'') => {
                            i += 1;
                            break;
                        }
                        Some(&ch) => {
                            push_literal(&mut tokens, ch);
                            i += 1;
                        }
                    }
                }
            } else if c.is_ascii_alphabetic() {
                if !PATTERN_LETTERS.contains(c) {
                    return Err(format!("Illegal pattern character '{c}'"));
                }
                let start = i;
                while chars.get(i) == Some(&c) {
                    i += 1;
                }
                let count = i - start;
                if c == 'X' && count > 3 {
                    return Err(format!("invalid ISO 8601 format: length={count}"));
                }
                tokens.push(Token::Field(c, count));
            } else {
                push_literal(&mut tokens, c);
                i += 1;
            }
        }
        Ok(JavaPattern { tokens })
    }

    /// Formats epoch milliseconds in `zone`.
    pub fn format(&self, epoch_ms: i64, zone: &Zone) -> Result<String, String> {
        let utc = DateTime::<Utc>::from_timestamp_millis(epoch_ms)
            .ok_or_else(|| "timestamp out of range".to_string())?;
        let zi = zone.info_at(&utc);
        let local = utc.naive_utc() + chrono::Duration::seconds(zi.offset_secs as i64);
        let date = local.date();
        let (bc, era_year) =
            if date.year() <= 0 { (true, 1 - date.year()) } else { (false, date.year()) };
        let hour = local.hour() as i64;
        let dow = date.weekday().num_days_from_sunday() as usize;

        let mut out = String::new();
        for tok in &self.tokens {
            let (c, n) = match tok {
                Token::Literal(s) => {
                    out.push_str(s);
                    continue;
                }
                Token::Field(c, n) => (*c, *n),
            };
            let num = |out: &mut String, v: i64| out.push_str(&format!("{v:0n$}"));
            match c {
                'G' => out.push_str(if bc { "BC" } else { "AD" }),
                'y' => format_year(&mut out, era_year as i64, n),
                'Y' => format_year(&mut out, week_of_year(date).1 as i64, n),
                'M' | 'L' => {
                    let m = date.month0() as usize;
                    match n {
                        1 | 2 => num(&mut out, m as i64 + 1),
                        3 => out.push_str(&MONTHS[m][..3]),
                        _ => out.push_str(MONTHS[m]),
                    }
                }
                'd' => num(&mut out, date.day() as i64),
                'D' => num(&mut out, date.ordinal() as i64),
                'E' => out.push_str(if n >= 4 { WEEKDAYS[dow] } else { &WEEKDAYS[dow][..3] }),
                'u' => num(&mut out, date.weekday().number_from_monday() as i64),
                'F' => num(&mut out, (date.day() as i64 - 1) / 7 + 1),
                'w' => num(&mut out, week_of_year(date).0 as i64),
                'W' => {
                    let first_dow =
                        date.with_day(1).map_or(0, |d| d.weekday().num_days_from_sunday());
                    num(&mut out, (date.day() as i64 - 1 + first_dow as i64) / 7 + 1)
                }
                'a' => out.push_str(if hour < 12 { "AM" } else { "PM" }),
                'H' => num(&mut out, hour),
                'k' => num(&mut out, if hour == 0 { 24 } else { hour }),
                'K' => num(&mut out, hour % 12),
                'h' => num(&mut out, if hour % 12 == 0 { 12 } else { hour % 12 }),
                'm' => num(&mut out, local.minute() as i64),
                's' => num(&mut out, local.second() as i64),
                'S' => num(&mut out, epoch_ms.rem_euclid(1000)),
                'z' => out.push_str(if n >= 4 { &zi.long_name } else { &zi.short_name }),
                'Z' => out.push_str(&format_offset(zi.offset_secs, 2)),
                'X' if zi.offset_secs == 0 => out.push('Z'),
                'X' => out.push_str(&format_offset(zi.offset_secs, n)),
                _ => unreachable!("pattern letters are validated by compile"),
            }
        }
        Ok(out)
    }

    /// Parses `text` in UTC unless it carries a zone, returning epoch milliseconds.
    /// Text after the end of the pattern is ignored, as in Java. Two-digit years resolve to
    /// the 100 years starting at `century_start_ms`.
    pub fn parse(&self, text: &str, century_start_ms: i64) -> Option<i64> {
        let text: Vec<char> = text.chars().collect();
        let mut pos = 0;
        let mut f = Fields::default();

        for (idx, tok) in self.tokens.iter().enumerate() {
            let (c, n) = match tok {
                Token::Literal(s) => {
                    for ch in s.chars() {
                        if text.get(pos) != Some(&ch) {
                            return None;
                        }
                        pos += 1;
                    }
                    continue;
                }
                Token::Field(c, n) => (*c, *n),
            };
            while text.get(pos).is_some_and(|ch| ch.is_whitespace()) {
                pos += 1;
            }
            if pos >= text.len() {
                return None;
            }
            if tok.is_numeric() {
                // A field followed by another numeric field reads exactly `n` chars.
                let end = if self.tokens.get(idx + 1).is_some_and(Token::is_numeric) {
                    if pos + n > text.len() {
                        return None;
                    }
                    pos + n
                } else {
                    text.len()
                };
                let window = &text[pos..end];
                let neg = window.first() == Some(&'-');
                let sign_len = usize::from(neg);
                let digits = window[sign_len..].iter().take_while(|c| c.is_ascii_digit()).count();
                if digits == 0 {
                    return None;
                }
                let s: String = window[..sign_len + digits].iter().collect();
                let mut v: i64 = s.parse().ok()?;
                pos += sign_len + digits;
                match c {
                    'y' | 'Y' => {
                        let ambiguous = n <= 2 && !neg && digits == 2;
                        if ambiguous {
                            let start_year =
                                DateTime::<Utc>::from_timestamp_millis(century_start_ms)?.year()
                                    as i64;
                            v += start_year.div_euclid(100) * 100;
                            if v < start_year {
                                v += 100;
                            }
                        }
                        f.set(if c == 'y' { F::Year } else { F::WeekYear }, v);
                        f.ambiguous_year = ambiguous;
                    }
                    'M' | 'L' => f.set(F::Month, v),
                    'd' => f.set(F::Day, v),
                    'D' => f.set(F::DayOfYear, v),
                    'F' => f.set(F::DowInMonth, v),
                    'W' => f.set(F::WeekOfMonth, v),
                    'w' => f.set(F::WeekOfYear, v),
                    'u' => f.set(F::DayOfWeek, if (1..=7).contains(&v) { v % 7 + 1 } else { v }),
                    'H' => f.set(F::HourOfDay, v),
                    'k' => f.set(F::HourOfDay, if v == 24 { 0 } else { v }),
                    'K' => f.set(F::Hour, v),
                    'h' => f.set(F::Hour, if v == 12 { 0 } else { v }),
                    'm' => f.set(F::Minute, v),
                    's' => f.set(F::Second, v),
                    'S' => f.set(F::Millis, v),
                    _ => unreachable!("numeric pattern letters are listed in is_numeric"),
                }
                continue;
            }
            let rest = &text[pos..];
            let len = match c {
                'M' | 'L' => {
                    let (i, len) = match_name(rest, &MONTHS)?;
                    f.set(F::Month, i as i64 + 1);
                    len
                }
                'E' => {
                    let (i, len) = match_name(rest, &WEEKDAYS)?;
                    f.set(F::DayOfWeek, i as i64 + 1);
                    len
                }
                'a' => {
                    let (i, len) = match_name(rest, &["AM", "PM"])?;
                    f.set(F::AmPm, i as i64);
                    len
                }
                'G' => {
                    let (i, len) = match_name(rest, &["BC", "AD"])?;
                    f.set(F::Era, i as i64);
                    len
                }
                'z' | 'Z' => {
                    let (zone, len) = parse_general_zone(rest)?;
                    f.zone = zone;
                    len
                }
                'X' => {
                    let (off, len) = parse_iso_zone(rest, n)?;
                    f.zone = ParsedZone::Offset(off);
                    len
                }
                _ => unreachable!("pattern letters are validated by compile"),
            };
            pos += len;
        }
        if pos == 0 {
            return None;
        }
        let ms = f.resolve()?;
        if f.ambiguous_year && ms < century_start_ms {
            for y in [F::Year, F::WeekYear] {
                f.vals[y as usize] = f.vals[y as usize].saturating_add(100);
            }
            return f.resolve();
        }
        Some(ms)
    }
}

/// Calendar fields set while parsing.
#[derive(Clone, Copy)]
enum F {
    Era,
    Year,
    WeekYear,
    /// 1 is January.
    Month,
    Day,
    DayOfYear,
    /// Java's `Calendar` numbering: 1 is Sunday. Out-of-range values roll over.
    DayOfWeek,
    DowInMonth,
    WeekOfMonth,
    WeekOfYear,
    HourOfDay,
    Hour,
    /// 1 is PM.
    AmPm,
    Minute,
    Second,
    Millis,
}

const FIELD_COUNT: usize = F::Millis as usize + 1;

/// Field values with the order they were set in (0 is unset), which Java uses to choose
/// between competing ways of expressing a date.
struct Fields {
    vals: [i64; FIELD_COUNT],
    stamps: [u32; FIELD_COUNT],
    next_stamp: u32,
    ambiguous_year: bool,
    zone: ParsedZone,
}

impl Default for Fields {
    fn default() -> Self {
        Fields {
            vals: [0; FIELD_COUNT],
            stamps: [0; FIELD_COUNT],
            next_stamp: 0,
            ambiguous_year: false,
            zone: ParsedZone::Offset(0),
        }
    }
}

impl Fields {
    fn set(&mut self, f: F, v: i64) {
        self.next_stamp += 1;
        self.vals[f as usize] = v;
        self.stamps[f as usize] = self.next_stamp;
    }

    fn stamp(&self, f: F) -> u32 {
        self.stamps[f as usize]
    }

    fn get(&self, f: F, default: i64) -> i128 {
        if self.stamp(f) == 0 {
            default as i128
        } else {
            self.vals[f as usize] as i128
        }
    }

    /// Resolves the fields the way a lenient `GregorianCalendar` does: out-of-range values
    /// roll over, and the most recently set date fields win. Weeks start on Sunday and
    /// week 1 holds January 1.
    fn resolve(&self) -> Option<i64> {
        let bc = self.stamp(F::Era) != 0 && self.vals[F::Era as usize] == 0;
        let era_year = |y: i128| if bc { 1 - y } else { y };
        let year = era_year(self.get(F::Year, 1970));
        let days = if self.stamp(F::WeekYear) > self.stamp(F::Year) {
            // Java's CalendarBuilder moves an out-of-range weekday into the next or previous
            // week, then reads the result as an ISO day number.
            let mut week = self.get(F::WeekOfYear, 1);
            let mut dow = self.get(F::DayOfWeek, 1);
            if !(1..=7).contains(&dow) {
                if dow >= 8 {
                    dow -= 1;
                    week += dow / 7;
                    dow = dow % 7 + 1;
                } else {
                    let k = (-dow).div_euclid(7) + 1;
                    week -= k;
                    dow += 7 * k;
                }
                dow = dow % 7 + 1;
            }
            week1_start(era_year(self.get(F::WeekYear, 0))) + 7 * (week - 1) + (dow - 1)
        } else {
            self.resolve_date(year, (self.get(F::DayOfWeek, 1) - 1).rem_euclid(7))
        };

        let s = |f| self.stamp(f);
        let hour_of_day = s(F::HourOfDay);
        let mut best = hour_of_day.max(aggregate(s(F::Hour), s(F::AmPm)));
        if best == 0 {
            best = s(F::Hour).max(s(F::AmPm));
        }
        let hour = if best == 0 {
            0
        } else if best == hour_of_day {
            self.get(F::HourOfDay, 0)
        } else {
            self.get(F::Hour, 0) + 12 * self.get(F::AmPm, 0)
        };

        let ms = days * 86_400_000
            + hour * 3_600_000
            + self.get(F::Minute, 0) * 60_000
            + self.get(F::Second, 0) * 1000
            + self.get(F::Millis, 0);
        let local = i64::try_from(ms).ok()?;
        local.checked_sub(self.zone.offset_for_local(local)? as i64 * 1000)
    }

    /// Port of `Calendar.selectFields` and `GregorianCalendar.getFixedDate`.
    fn resolve_date(&self, year: i128, dow: i128) -> i128 {
        let s = |f| self.stamp(f);
        let dow_s = s(F::DayOfWeek);
        let mut dom_s = s(F::Day);
        let mut wom_s = aggregate(s(F::WeekOfMonth), dow_s);
        let mut dowim_s = aggregate(s(F::DowInMonth), dow_s);
        let doy_s = s(F::DayOfYear);
        let mut woy_s = aggregate(s(F::WeekOfYear), dow_s);
        let mut best = dom_s.max(wom_s).max(dowim_s).max(doy_s).max(woy_s);
        if best == 0 {
            wom_s = s(F::WeekOfMonth);
            dowim_s = s(F::DowInMonth).max(dow_s);
            woy_s = s(F::WeekOfYear);
            best = wom_s.max(dowim_s).max(woy_s);
            if best == 0 {
                dom_s = s(F::Month);
                best = dom_s;
            }
        }
        let first_week_start = |first: i128| {
            let start = on_or_before(first + 6, 0);
            if start > first {
                start - 7
            } else {
                start
            }
        };

        let by_month = best == dom_s
            || (best == wom_s && s(F::WeekOfMonth) >= s(F::WeekOfYear))
            || (best == dowim_s && s(F::DowInMonth) >= s(F::WeekOfYear));
        if by_month {
            let m0 = self.get(F::Month, 1) - 1;
            let (y, m) = (year + m0.div_euclid(12), m0.rem_euclid(12) + 1);
            let first = days_from_civil(y, m, 1);
            if best == dom_s {
                return first + self.get(F::Day, 1) - 1;
            }
            let use_wom = if wom_s == dowim_s {
                s(F::WeekOfMonth) >= s(F::DowInMonth)
            } else {
                best == wom_s
            };
            if use_wom {
                let mut start = first_week_start(first);
                if dow_s != 0 {
                    start = on_or_before(start + 6, dow);
                }
                return start + 7 * (self.get(F::WeekOfMonth, 1) - 1);
            }
            let dowim = self.get(F::DowInMonth, 1);
            if dowim >= 0 {
                on_or_before(first + 7 * dowim - 1, dow)
            } else {
                let next = if m == 12 {
                    days_from_civil(y + 1, 1, 1)
                } else {
                    days_from_civil(y, m + 1, 1)
                };
                on_or_before(next + 7 * (dowim + 1) - 1, dow)
            }
        } else if best == doy_s {
            days_from_civil(year, 1, 1) + self.get(F::DayOfYear, 1) - 1
        } else {
            let mut start = first_week_start(days_from_civil(year, 1, 1));
            if dow_s != 0 && dow != 0 {
                start = on_or_before(start + 6, dow);
            }
            start + 7 * (self.get(F::WeekOfYear, 1) - 1)
        }
    }
}

/// Java's `Calendar.aggregateStamp`: unset unless both fields are set.
fn aggregate(a: u32, b: u32) -> u32 {
    if a == 0 || b == 0 {
        0
    } else {
        a.max(b)
    }
}

/// The latest day on or before `day` that falls on `dow` (0 is Sunday).
fn on_or_before(day: i128, dow: i128) -> i128 {
    // 1970-01-01 was a Thursday.
    day - (day + 4 - dow).rem_euclid(7)
}

/// First day of week 1 of `year`.
fn week1_start(year: i128) -> i128 {
    let jan1 = days_from_civil(year, 1, 1);
    jan1 - (jan1 + 4).rem_euclid(7)
}

/// Days since 1970-01-01 in the proleptic Gregorian calendar.
fn days_from_civil(y: i128, m: i128, d: i128) -> i128 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

/// Week of year and week year, with weeks starting on Sunday and week 1 holding January 1.
fn week_of_year(date: NaiveDate) -> (u32, i32) {
    let dow = date.weekday().num_days_from_sunday();
    let days_in_year = if date.leap_year() { 366 } else { 365 };
    if date.ordinal() + (6 - dow) > days_in_year {
        return (1, date.year() + 1);
    }
    let jan1_dow = date.with_ordinal(1).map_or(0, |d| d.weekday().num_days_from_sunday());
    ((date.ordinal() - 1 + jan1_dow) / 7 + 1, date.year())
}

fn format_year(out: &mut String, year: i64, n: usize) {
    if n == 2 {
        out.push_str(&format!("{:02}", year % 100));
    } else {
        out.push_str(&format!("{year:0n$}"));
    }
}

/// `+hh` (width 1), `+hhmm` (width 2) or `+hh:mm` (width 3).
fn format_offset(offset_secs: i32, width: usize) -> String {
    let sign = if offset_secs < 0 { '-' } else { '+' };
    let mins = offset_secs.abs() / 60;
    match width {
        1 => format!("{sign}{:02}", mins / 60),
        3 => format!("{sign}{:02}:{:02}", mins / 60, mins % 60),
        _ => format!("{sign}{:02}{:02}", mins / 60, mins % 60),
    }
}

/// Case-insensitive match of full or three-letter names; the longest match wins.
fn match_name(text: &[char], names: &[&str]) -> Option<(usize, usize)> {
    let mut best: Option<(usize, usize)> = None;
    for (i, name) in names.iter().enumerate() {
        for cand in [*name, &name[..3.min(name.len())]] {
            let len = cand.chars().count();
            if len <= text.len()
                && best.is_none_or(|(_, b)| len > b)
                && cand.chars().zip(text).all(|(a, b)| a.eq_ignore_ascii_case(b))
            {
                best = Some((i, len));
            }
        }
    }
    best
}

fn digit(text: &[char], i: usize) -> Option<i32> {
    text.get(i).and_then(|c| c.to_digit(10)).map(|d| d as i32)
}

/// Parses `hh[mm]` after a sign. `count` 1 reads hours only; `colon` requires `hh:mm`.
/// Without a colon the hour must have two digits.
fn parse_numeric_zone(text: &[char], count: usize, colon: bool) -> Option<(i32, usize)> {
    let mut i = 0;
    let mut hours = digit(text, i)?;
    i += 1;
    match digit(text, i) {
        Some(d) => {
            hours = hours * 10 + d;
            i += 1;
        }
        None if count > 0 || !colon => return None,
        None => {}
    }
    if hours > 23 {
        return None;
    }
    let mut minutes = 0;
    if count != 1 {
        if colon {
            if text.get(i) != Some(&':') {
                return None;
            }
            i += 1;
        }
        minutes = digit(text, i)? * 10 + digit(text, i + 1)?;
        i += 2;
        if minutes > 59 {
            return None;
        }
    }
    Some(((hours * 60 + minutes) * 60, i))
}

fn sign_of(c: Option<&char>) -> Option<i32> {
    match c {
        Some('+') => Some(1),
        Some('-') => Some(-1),
        _ => None,
    }
}

/// `z`/`Z`: `GMT[+hh:mm]`, RFC 822 `+hhmm`, or a zone name.
fn parse_general_zone(text: &[char]) -> Option<(ParsedZone, usize)> {
    let is_gmt = text.len() >= 3
        && text[..3].iter().zip("GMT".chars()).all(|(a, b)| a.eq_ignore_ascii_case(&b));
    if is_gmt {
        return match sign_of(text.get(3)) {
            Some(sign) => {
                let (off, len) = parse_numeric_zone(&text[4..], 0, true)?;
                Some((ParsedZone::Offset(sign * off), 4 + len))
            }
            None => Some((ParsedZone::Offset(0), 3)),
        };
    }
    if let Some(sign) = sign_of(text.first()) {
        let (off, len) = parse_numeric_zone(&text[1..], 0, false)?;
        return Some((ParsedZone::Offset(sign * off), 1 + len));
    }
    match_zone_name(text)
}

/// `X`: `Z` or an ISO 8601 offset of the pattern's width.
fn parse_iso_zone(text: &[char], count: usize) -> Option<(i32, usize)> {
    if text.first() == Some(&'Z') {
        return Some((0, 1));
    }
    let sign = sign_of(text.first())?;
    let (off, len) = parse_numeric_zone(&text[1..], count, count == 3)?;
    Some((sign * off, 1 + len))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(p: &str, ms: i64) -> String {
        JavaPattern::compile(p).unwrap().format(ms, &Zone::Utc).unwrap()
    }

    /// 1946-11-11, 80 years before 2026-11-11.
    const CENTURY_START: i64 = -730_252_800_000;

    fn parse(p: &str, text: &str) -> Option<i64> {
        JavaPattern::compile(p).unwrap().parse(text, CENTURY_START)
    }

    #[test]
    fn compile_rejects_unknown_letters_and_bad_quotes() {
        assert_eq!(JavaPattern::compile("yyyy q").unwrap_err(), "Illegal pattern character 'q'");
        assert_eq!(JavaPattern::compile("'abc").unwrap_err(), "Unterminated quote");
        assert_eq!(JavaPattern::compile("XXXX").unwrap_err(), "invalid ISO 8601 format: length=4");
        assert_eq!(fmt("'it''s' ''", 0), "it's '");
    }

    #[test]
    fn formats_fields_like_java() {
        let ms = 1_514_764_800_005;
        assert_eq!(fmt("y yy yyy yyyy yyyyy G", ms), "2018 18 2018 2018 02018 AD");
        assert_eq!(fmt("M MM MMM MMMM L LLL", ms), "1 01 Jan January 1 Jan");
        assert_eq!(
            fmt("d dd D DDDD E EEEE u F w W Y YY", ms),
            "1 01 1 0001 Mon Monday 1 1 1 1 2018 18"
        );
        assert_eq!(fmt("H HH k K h hh a m s S SSS SSSS", ms), "0 00 24 0 12 12 AM 0 0 5 005 0005");
        assert_eq!(fmt("z zzzz Z X XX XXX", ms), "UTC Coordinated Universal Time +0000 Z Z Z");
    }

    #[test]
    fn week_year_rolls_over_at_year_end() {
        // 2018-12-30 is a Sunday; its week holds 2019-01-01.
        assert_eq!(fmt("w YYYY", 1_546_128_000_000), "1 2019");
    }

    #[test]
    fn parses_leniently_like_java() {
        assert_eq!(parse("yyyy-MM-dd HH:mm:ss", "2018-13-32 25:61:61"), Some(1_549_072_921_000));
        assert_eq!(parse("yyyyMMdd", "20180102"), Some(1_514_851_200_000));
        assert_eq!(parse("yy-MM-dd", "18-01-01"), Some(1_514_764_800_000));
        assert_eq!(parse("yy-MM-dd", "70-01-01"), Some(0));
        assert_eq!(parse("yy-MM-dd", "46-01-01"), Some(2_398_377_600_000));
        assert_eq!(parse("yy-MM-dd", "46-12-01"), Some(-728_524_800_000));
        assert_eq!(parse("w yyyy", "12 2018"), Some(1_521_331_200_000));
        assert_eq!(parse("YYYY-MM-dd", "2018-01-02"), Some(1_514_678_400_000));
        assert_eq!(parse("yy-MM-dd", "2018-01-01"), Some(1_514_764_800_000));
        assert_eq!(parse("yyyy-MMMM-dd", "2018-jan-02"), Some(1_514_851_200_000));
        assert_eq!(parse("yyyy-MM-dd hh:mm a", "2018-01-02 01:00 PM"), Some(1_514_898_000_000));
        assert_eq!(parse("yyyy-MM-dd kk:mm", "2018-01-02 24:00"), Some(1_514_851_200_000));
        assert_eq!(parse("yyyy D", "2018 032"), Some(1_517_443_200_000));
        assert_eq!(parse("yyyy", " 2018trailing"), Some(1_514_764_800_000));
        assert_eq!(parse("ss.SSS", "00.1234"), Some(1234));
        assert_eq!(parse(" yyyy", "2018"), None);
        assert_eq!(parse("", "not a date"), None);
        assert_eq!(parse("yyyy", "9223372036854775807"), None);
        assert_eq!(parse("yyyy-MM-dd HH", "2018-01-02"), None);
    }

    #[test]
    fn resolves_competing_fields_like_java() {
        let jan = |d: i64| Some(1_514_764_800_000 + (d - 1) * 86_400_000);
        assert_eq!(parse("yyyy-MM F E", "2018-01 2 Mon"), jan(8));
        assert_eq!(parse("yyyy-MM W E", "2018-01 2 Mon"), jan(8));
        assert_eq!(parse("yyyy-MM W", "2018-01 2"), jan(7));
        assert_eq!(parse("yyyy-MM F", "2018-01 2"), jan(14));
        assert_eq!(parse("yyyy-MM-dd D", "2018-01-01 032"), jan(32));
        assert_eq!(parse("D yyyy-MM-dd", "032 2018-01-05"), jan(5));
        assert_eq!(parse("yyyy-MM-dd E", "2018-01-10 Mon"), jan(10));
        assert_eq!(parse("yyyy w u", "2018 12 3"), Some(1_521_590_400_000));
        // Out-of-range ISO day numbers roll into the next or previous week.
        assert_eq!(parse("YYYY w u", "2018 1 8"), Some(1_515_369_600_000));
        assert_eq!(parse("YYYY w u", "2018 1 0"), Some(1_514_073_600_000));
        assert_eq!(parse("yyyy w u", "2018 1 8"), Some(1_514_678_400_000));
        assert_eq!(parse("yyyy w u", "2018 1 0"), Some(1_515_196_800_000));
        assert_eq!(parse("yyyy-MM F u", "2018-01 1 0"), Some(1_515_196_800_000));
        assert_eq!(parse("YY w u", "46 1 1"), Some(2_398_377_600_000));
        assert_eq!(parse("yyyy-MM-dd hh a HH", "2018-01-02 03 PM 15"), Some(1_514_905_200_000));
        assert_eq!(parse("yyyy-MM-dd HH hh a", "2018-01-02 15 03 PM"), Some(1_514_905_200_000));
    }

    #[test]
    fn accepts_negative_fields() {
        assert_eq!(parse("yyyy-MM-dd HH:mm:ss", "2018-01-01 00:00:-1"), Some(1_514_764_799_000));
        assert_eq!(parse("yyyy-MM", "2018--01"), Some(1_509_494_400_000));
        assert_eq!(parse("yyyyMM", "2018-1"), Some(1_509_494_400_000));
        assert_eq!(parse("yyyy-MM", "2018-+1"), None);
        assert_eq!(parse("yyyy-MM-dd", "2018--9223372036854775808-01"), None);
    }

    #[test]
    fn parses_zones_like_java() {
        let base = 1_514_851_200_000;
        assert_eq!(parse("yyyy-MM-dd HH:mm Z", "2018-01-02 00:00 +0100"), Some(base - 3_600_000));
        assert_eq!(parse("yyyy-MM-dd HH:mm Z", "2018-01-02 00:00 +01:00"), None);
        assert_eq!(parse("yyyy-MM-dd HH:mm Z", "2018-01-02 00:00 Z"), None);
        assert_eq!(
            parse("yyyy-MM-dd HH:mm z", "2018-01-02 00:00 GMT+01:00"),
            Some(base - 3_600_000)
        );
        assert_eq!(parse("yyyy-MM-dd HH:mm z", "2018-01-02 00:00 CEST"), Some(base - 7_200_000));
        assert_eq!(
            parse("yyyy-MM-dd HH:mm z", "2018-01-02 00:00 Central European Standard Time"),
            Some(base - 3_600_000)
        );
        assert_eq!(parse("yyyy-MM-dd HH:mm z", "2018-01-02 00:00 PST"), Some(base + 28_800_000));
        // Named zones use their offset on the parsed date: Moscow was UTC+4 in 2012.
        assert_eq!(parse("yyyy-MM-dd z", "2012-01-01 MSK"), Some(1_325_361_600_000));
        assert_eq!(parse("yyyy-MM-dd z", "1990-07-01 MSK"), Some(646_776_000_000));
        assert_eq!(parse("yyyy-MM-dd z", "2018-07-01 CET"), Some(1_530_399_600_000));
        assert_eq!(parse("yyyy-MM-dd z", "1950-07-01 CEST"), Some(-615_520_800_000));
        // Long names force standard time; China and Japan had DST in these years.
        assert_eq!(parse("yyyy-MM-dd z", "1988-07-01 China Standard Time"), Some(583_689_600_000));
        assert_eq!(parse("yyyy-MM-dd z", "1990-07-01 Moscow Standard Time"), Some(646_779_600_000));
        assert_eq!(parse("yyyy-MM-dd z", "1948-07-01 JST"), Some(-678_618_000_000));
        assert_eq!(parse("yyyy-MM-dd HH:mm X", "2018-01-02 00:00 Z"), Some(base));
        assert_eq!(parse("yyyy-MM-dd HH:mm X", "2018-01-02 00:00 +0130"), Some(base - 3_600_000));
        assert_eq!(
            parse("yyyy-MM-dd HH:mm XXX", "2018-01-02 00:00 +01:30"),
            Some(base - 5_400_000)
        );
    }
}
