//! Date patterns with the semantics of Java's `SimpleDateFormat` (lenient, English names).

use chrono::{DateTime, Datelike, NaiveDate, Timelike, Utc};

use super::zones::{match_zone_name, Zone};

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
                let max_digits = if self.tokens.get(idx + 1).is_some_and(Token::is_numeric) {
                    n
                } else {
                    usize::MAX
                };
                let digits =
                    text[pos..].iter().take(max_digits).take_while(|c| c.is_ascii_digit()).count();
                if digits == 0 {
                    return None;
                }
                let s: String = text[pos..pos + digits].iter().collect();
                let mut v: i64 = s.parse().ok()?;
                pos += digits;
                match c {
                    'y' | 'Y' => {
                        let ambiguous = n <= 2 && digits == 2;
                        if ambiguous {
                            let start_year =
                                DateTime::<Utc>::from_timestamp_millis(century_start_ms)?.year()
                                    as i64;
                            v += start_year.div_euclid(100) * 100;
                            if v < start_year {
                                v += 100;
                            }
                        }
                        if c == 'y' {
                            f.year = Some(v);
                            f.ambiguous_year = ambiguous;
                        } else {
                            f.week_year = Some(v);
                        }
                    }
                    'M' | 'L' => f.month = Some(v),
                    'd' => f.day = Some(v),
                    'D' => f.day_of_year = Some(v),
                    'H' => f.hour24 = Some(v),
                    'k' => f.hour24 = Some(if v == 24 { 0 } else { v }),
                    'K' => f.hour12 = Some(v),
                    'h' => f.hour12 = Some(if v == 12 { 0 } else { v }),
                    'm' => f.minute = v,
                    's' => f.second = v,
                    'S' => f.millis = v,
                    'w' => f.week = Some(v),
                    'u' => f.weekday = Some(v % 7),
                    _ => {}
                }
                continue;
            }
            let rest = &text[pos..];
            let len = match c {
                'M' | 'L' => {
                    let (i, len) = match_name(rest, &MONTHS)?;
                    f.month = Some(i as i64 + 1);
                    len
                }
                'E' => {
                    let (i, len) = match_name(rest, &WEEKDAYS)?;
                    f.weekday = Some(i as i64);
                    len
                }
                'a' => {
                    let (i, len) = match_name(rest, &["AM", "PM"])?;
                    f.pm = Some(i == 1);
                    len
                }
                'G' => {
                    let (i, len) = match_name(rest, &["BC", "AD"])?;
                    f.bc = i == 0;
                    len
                }
                'z' | 'Z' => {
                    let (off, len) = parse_general_zone(rest)?;
                    f.offset_secs = off;
                    len
                }
                'X' => {
                    let (off, len) = parse_iso_zone(rest, n)?;
                    f.offset_secs = off;
                    len
                }
                _ => unreachable!("pattern letters are validated by compile"),
            };
            pos += len;
        }
        let ms = f.resolve()?;
        if f.ambiguous_year && ms < century_start_ms {
            f.year = f.year.map(|y| y + 100);
            return f.resolve();
        }
        Some(ms)
    }
}

#[derive(Default)]
struct Fields {
    year: Option<i64>,
    ambiguous_year: bool,
    week_year: Option<i64>,
    week: Option<i64>,
    /// 0 is Sunday.
    weekday: Option<i64>,
    bc: bool,
    month: Option<i64>,
    day: Option<i64>,
    day_of_year: Option<i64>,
    hour24: Option<i64>,
    hour12: Option<i64>,
    pm: Option<bool>,
    minute: i64,
    second: i64,
    millis: i64,
    offset_secs: i32,
}

impl Fields {
    /// Out-of-range values roll over into the next unit, like a lenient Java calendar.
    /// A week year, or a week without a month or day, selects a week date (as in Java,
    /// `YYYY-MM-dd` ignores the month and day).
    fn resolve(&self) -> Option<i64> {
        let mut year = self.year.unwrap_or(1970);
        if self.bc {
            year = 1 - year;
        }
        let week_date = self.week_year.is_some()
            || (self.week.is_some()
                && self.month.is_none()
                && self.day.is_none()
                && self.day_of_year.is_none());
        let days = match (self.day_of_year, self.month, self.day) {
            _ if week_date => {
                let jan1 = days_from_civil(self.week_year.unwrap_or(year), 1, 1);
                let week1_start = jan1 - (jan1 + 4).rem_euclid(7);
                let week = self.week.unwrap_or(1) - 1;
                week1_start
                    .checked_add(week.checked_mul(7)?)?
                    .checked_add(self.weekday.unwrap_or(0))?
            }
            (Some(doy), None, None) => days_from_civil(year, 1, 1).checked_add(doy - 1)?,
            (_, month, day) => {
                let m0 = month.unwrap_or(1) - 1;
                let first =
                    days_from_civil(year.checked_add(m0.div_euclid(12))?, m0.rem_euclid(12) + 1, 1);
                first.checked_add(day.unwrap_or(1) - 1)?
            }
        };
        let pm_hours = if self.pm == Some(true) { 12 } else { 0 };
        let hour = self.hour24.unwrap_or_else(|| self.hour12.unwrap_or(0) + pm_hours);
        let ms = days
            .checked_mul(86_400_000)?
            .checked_add(hour.checked_mul(3_600_000)?)?
            .checked_add(self.minute.checked_mul(60_000)?)?
            .checked_add(self.second.checked_mul(1000)?)?
            .checked_add(self.millis)?;
        ms.checked_sub(self.offset_secs as i64 * 1000)
    }
}

/// Days since 1970-01-01 in the proleptic Gregorian calendar.
fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
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
fn parse_general_zone(text: &[char]) -> Option<(i32, usize)> {
    let is_gmt = text.len() >= 3
        && text[..3].iter().zip("GMT".chars()).all(|(a, b)| a.eq_ignore_ascii_case(&b));
    if is_gmt {
        return match sign_of(text.get(3)) {
            Some(sign) => {
                let (off, len) = parse_numeric_zone(&text[4..], 0, true)?;
                Some((sign * off, 4 + len))
            }
            None => Some((0, 3)),
        };
    }
    if let Some(sign) = sign_of(text.first()) {
        let (off, len) = parse_numeric_zone(&text[1..], 0, false)?;
        return Some((sign * off, 1 + len));
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
        assert_eq!(parse("yyyy-MM-dd HH", "2018-01-02"), None);
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
        assert_eq!(parse("yyyy-MM-dd HH:mm X", "2018-01-02 00:00 Z"), Some(base));
        assert_eq!(parse("yyyy-MM-dd HH:mm X", "2018-01-02 00:00 +0130"), Some(base - 3_600_000));
        assert_eq!(
            parse("yyyy-MM-dd HH:mm XXX", "2018-01-02 00:00 +01:30"),
            Some(base - 5_400_000)
        );
    }
}
