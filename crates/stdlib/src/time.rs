#[cfg(feature = "chrono")]
use chrono::{FixedOffset, TimeZone};

pub fn now_seconds_portable() -> Result<f64, String> {
    #[cfg(target_arch = "wasm32")]
    {
        let millis = js_sys::Date::now();
        Ok(millis / 1000.0)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        use std::time::{SystemTime, UNIX_EPOCH};
        let dur = SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| e.to_string())?;
        Ok(dur.as_secs() as f64 + dur.subsec_nanos() as f64 / 1_000_000_000.0)
    }
}

#[derive(Debug)]
pub enum ParseTimeErr {
    BadFormat(String),
    ParseFailed(String),
}

// Parse with chrono, return seconds since epoch (UTC) as f64.
// Behavior:
// - If input has an offset → convert to UTC.
// - If no offset → interpret as UTC.
// - Bad format pattern → BadFormat; parse mismatch → ParseFailed.
#[cfg(feature = "chrono")]
pub fn parse_time_utc_seconds(text: &str, fmt_java: &str) -> Result<f64, ParseTimeErr> {
    use chrono::{DateTime, NaiveDate, NaiveDateTime, Utc};

    let fmt = java_to_chrono_format(fmt_java);
    let text_norm = normalize_z_for_offset(text, &fmt);

    // 1) Offset-aware
    match DateTime::parse_from_str(&text_norm, &fmt) {
        Ok(dt_fixed) => {
            let dt_utc = dt_fixed.with_timezone(&Utc);
            Ok(dt_utc.timestamp() as f64 + dt_utc.timestamp_subsec_nanos() as f64 / 1e9)
        }
        Err(e1) => {
            // 2) Naive datetime (assume UTC)
            match NaiveDateTime::parse_from_str(&text_norm, &fmt) {
                Ok(naive) => {
                    let dt_utc = DateTime::<Utc>::from_naive_utc_and_offset(naive, Utc);
                    Ok(dt_utc.timestamp() as f64 + dt_utc.timestamp_subsec_nanos() as f64 / 1e9)
                }
                Err(e2) => {
                    // 3) Naive date only (midnight UTC)
                    match NaiveDate::parse_from_str(&text_norm, &fmt) {
                        Ok(date) => {
                            let naive = date.and_hms_opt(0, 0, 0).ok_or_else(|| {
                                ParseTimeErr::ParseFailed("invalid time components".into())
                            })?;
                            let dt_utc = DateTime::<Utc>::from_naive_utc_and_offset(naive, Utc);
                            Ok(dt_utc.timestamp() as f64
                                + dt_utc.timestamp_subsec_nanos() as f64 / 1e9)
                        }
                        Err(e3) => {
                            let msg = format!("{}; {}; {}", e1, e2, e3);
                            if msg.contains("invalid format")
                                || msg.contains("unknown")
                                || msg.contains("bad format")
                            {
                                return Err(ParseTimeErr::BadFormat(msg));
                            }
                            Err(ParseTimeErr::ParseFailed(msg))
                        }
                    }
                }
            }
        }
    }
}

// Normalize ISO-8601 trailing 'Z' to +0000 if the pattern expects an offset.
// This makes "Z" acceptable for %z parsing.
#[cfg(feature = "chrono")]
fn normalize_z_for_offset<'a>(text: &'a str, chrono_fmt: &str) -> std::borrow::Cow<'a, str> {
    use std::borrow::Cow;
    if chrono_fmt.contains("%z") && text.ends_with('Z') {
        // Convert "...Z" to "...+0000"
        let mut s = String::with_capacity(text.len() + 5);
        s.push_str(&text[..text.len() - 1]);
        s.push_str("+0000");
        Cow::Owned(s)
    } else {
        Cow::Borrowed(text)
    }
}

// Format seconds=since-epoch using chrono with java-like patterns.
// - Default timezone is UTC.
// - If tz_str is provided, it must be a numeric offset like "+0200", "-0530", or "Z" for UTC.
#[cfg(feature = "chrono")]
pub fn format_time_with_chrono(
    secs_since_epoch: f64,
    fmt_java: &str,
    tz_str: Option<&str>,
) -> Result<String, String> {
    use chrono::Utc;

    // Translate Java pattern to chrono pattern
    let fmt = java_to_chrono_format(fmt_java);

    // Split into seconds + nanos (handle negative and fractional)
    let whole = secs_since_epoch.trunc() as i64;
    let mut nanos = ((secs_since_epoch - whole as f64) * 1_000_000_000.0).round() as i64;
    let mut secs = whole;
    if nanos >= 1_000_000_000 {
        secs += 1;
        nanos -= 1_000_000_000;
    }
    if nanos < 0 {
        secs -= 1;
        nanos += 1_000_000_000;
    }

    let base_utc = Utc
        .timestamp_opt(secs, nanos as u32)
        .single()
        .ok_or_else(|| "timestamp out of range".to_string())?;

    let formatted = if let Some(tz) = tz_str {
        let offset =
            parse_fixed_offset(tz).ok_or_else(|| format!("unsupported timezone '{}'", tz))?;
        base_utc.with_timezone(&offset).format(&fmt).to_string()
    } else {
        base_utc.format(&fmt).to_string()
    };

    Ok(formatted)
}

#[cfg(feature = "chrono")]
fn parse_fixed_offset(s: &str) -> Option<FixedOffset> {
    if s == "Z"
        || s == "+00:00"
        || s == "+0000"
        || s == "+00"
        || s == "-00:00"
        || s == "-0000"
        || s == "-00"
    {
        return FixedOffset::east_opt(0);
    }
    // Accept "+HH", "+HHMM", "+HH:MM", and negative variants
    let clean = s.replace(':', "");
    let (sign, rest) = match clean.as_bytes().first()? {
        b'+' => (1, &clean[1..]),
        b'-' => (-1, &clean[1..]),
        _ => return None,
    };
    let (hh, mm) = match rest.len() {
        2 => (rest.parse::<i32>().ok()?, 0),
        4 => {
            let (h, m) = rest.split_at(2);
            (h.parse::<i32>().ok()?, m.parse::<i32>().ok()?)
        }
        _ => return None,
    };
    let secs = sign * (hh * 3600 + mm * 60);
    if secs.abs() > 24 * 3600 {
        return None;
    }
    if secs >= 0 {
        FixedOffset::east_opt(secs)
    } else {
        FixedOffset::west_opt(-secs)
    }
}

#[cfg(feature = "chrono")]
fn java_to_chrono_format(java: &str) -> String {
    // Minimal token translator covering common symbols we use in tests/spec:
    // y -> %Y or truncated; M -> %m; d -> %d; H -> %H; m -> %M; s -> %S;
    // 'literal' -> literal (strip single quotes); X -> ISO8601 offset -> %z or 'Z'
    // Anything else is passed through as-is.
    let mut out = String::with_capacity(java.len() + 8);
    let mut chars = java.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            // quoted literal
            '\'' => {
                for ch in chars.by_ref() {
                    if ch == '\'' {
                        break;
                    }
                    out.push(ch);
                }
            }
            // years (yyyy or yy)
            'y' => {
                // count consecutive y
                while matches!(chars.peek(), Some('y')) {
                    chars.next();
                }
                // Map yyyy/yyy/yy/y -> %Y (Chrono prints full year; yy behavior not strictly emulated)
                out.push_str("%Y");
            }
            // month number
            'M' => {
                // collapse consecutive M as numeric month
                while matches!(chars.peek(), Some('M')) {
                    chars.next();
                }
                out.push_str("%m");
            }
            // day of month
            'd' => {
                while matches!(chars.peek(), Some('d')) {
                    chars.next();
                }
                out.push_str("%d");
            }
            // hour (0-23)
            'H' => {
                while matches!(chars.peek(), Some('H')) {
                    chars.next();
                }
                out.push_str("%H");
            }
            // minute
            'm' => {
                while matches!(chars.peek(), Some('m')) {
                    chars.next();
                }
                out.push_str("%M");
            }
            // second
            's' => {
                while matches!(chars.peek(), Some('s')) {
                    chars.next();
                }
                out.push_str("%S");
            }
            // timezone X (ISO 8601) → %z. We don't distinguish 1/2/3 X’s variants here.
            'X' => {
                while matches!(chars.peek(), Some('X')) {
                    chars.next();
                }
                out.push_str("%z");
            }
            // literal T (common in ISO)
            'T' => {
                out.push('T');
            }
            // everything else passthrough
            other => out.push(other),
        }
    }
    out
}
