use chrono::{DateTime, Offset, TimeZone, Utc};
use chrono_tz::{OffsetComponents, OffsetName, Tz};

/// Time zone used by `format-time`. `Utc` is Java's `SimpleTimeZone(0, "UTC")` default.
#[derive(Debug, Clone, Copy)]
pub enum Zone {
    Utc,
    Named(Tz),
}

/// Java's legacy three-letter IDs that are not IANA names (`ZoneId.SHORT_IDS`).
const JAVA_SHORT_IDS: &[(&str, &str)] = &[
    ("ACT", "Australia/Darwin"),
    ("AET", "Australia/Sydney"),
    ("AGT", "America/Argentina/Buenos_Aires"),
    ("ART", "Africa/Cairo"),
    ("AST", "America/Anchorage"),
    ("BET", "America/Sao_Paulo"),
    ("BST", "Asia/Dhaka"),
    ("CAT", "Africa/Harare"),
    ("CNT", "America/St_Johns"),
    ("CST", "America/Chicago"),
    ("CTT", "Asia/Shanghai"),
    ("EAT", "Africa/Addis_Ababa"),
    ("ECT", "Europe/Paris"),
    ("IET", "America/Indiana/Indianapolis"),
    ("IST", "Asia/Kolkata"),
    ("JST", "Asia/Tokyo"),
    ("MIT", "Pacific/Apia"),
    ("NET", "Asia/Yerevan"),
    ("NST", "Pacific/Auckland"),
    ("PLT", "Asia/Karachi"),
    ("PNT", "America/Phoenix"),
    ("PRT", "America/Puerto_Rico"),
    ("PST", "America/Los_Angeles"),
    ("SST", "Pacific/Guadalcanal"),
    ("VST", "Asia/Ho_Chi_Minh"),
];

/// Zone names: (short standard, long standard, short daylight, long daylight, standard
/// offset in minutes). Daylight time is one hour ahead of standard time.
const ZONE_NAMES: &[(&str, &str, &str, &str, i32)] = &[
    ("UTC", "Coordinated Universal Time", "", "", 0),
    ("GMT", "Greenwich Mean Time", "BST", "British Summer Time", 0),
    ("WET", "Western European Standard Time", "WEST", "Western European Summer Time", 0),
    ("CET", "Central European Standard Time", "CEST", "Central European Summer Time", 60),
    ("EET", "Eastern European Standard Time", "EEST", "Eastern European Summer Time", 120),
    ("MSK", "Moscow Standard Time", "", "", 180),
    ("IST", "India Standard Time", "", "", 330),
    ("HKT", "Hong Kong Standard Time", "", "", 480),
    ("AWST", "Australian Western Standard Time", "", "", 480),
    ("JST", "Japan Standard Time", "", "", 540),
    ("KST", "Korean Standard Time", "", "", 540),
    ("ACST", "Australian Central Standard Time", "ACDT", "Australian Central Daylight Time", 570),
    ("AEST", "Australian Eastern Standard Time", "AEDT", "Australian Eastern Daylight Time", 600),
    ("NZST", "New Zealand Standard Time", "NZDT", "New Zealand Daylight Time", 720),
    ("HST", "Hawaii-Aleutian Standard Time", "HDT", "Hawaii-Aleutian Daylight Time", -600),
    ("AKST", "Alaska Standard Time", "AKDT", "Alaska Daylight Time", -540),
    ("PST", "Pacific Standard Time", "PDT", "Pacific Daylight Time", -480),
    ("MST", "Mountain Standard Time", "MDT", "Mountain Daylight Time", -420),
    ("CST", "Central Standard Time", "CDT", "Central Daylight Time", -360),
    ("EST", "Eastern Standard Time", "EDT", "Eastern Daylight Time", -300),
    ("AST", "Atlantic Standard Time", "ADT", "Atlantic Daylight Time", -240),
];

/// Offset and names of a zone at one instant.
pub(crate) struct ZoneInfo {
    pub offset_secs: i32,
    pub short_name: String,
    pub long_name: String,
}

impl Zone {
    /// Looks up a zone by the IDs Java's `TimeZone.getAvailableIDs()` accepts.
    pub fn from_id(id: &str) -> Option<Zone> {
        let id = JAVA_SHORT_IDS.iter().find(|(k, _)| *k == id).map_or(id, |(_, v)| v);
        id.parse::<Tz>().ok().map(Zone::Named)
    }

    pub(crate) fn info_at(&self, utc: &DateTime<Utc>) -> ZoneInfo {
        let (offset_secs, abbr, dst) = match self {
            Zone::Utc => (0, "UTC".to_string(), false),
            Zone::Named(tz) => {
                let off = tz.offset_from_utc_datetime(&utc.naive_utc());
                let abbr = off.abbreviation().unwrap_or("").to_string();
                let dst = !off.dst_offset().is_zero();
                (off.fix().local_minus_utc(), abbr, dst)
            }
        };
        let known = ZONE_NAMES.iter().find_map(|(ss, sl, ds, dl, _)| {
            if !dst && *ss == abbr {
                Some(*sl)
            } else if dst && !ds.is_empty() && *ds == abbr {
                Some(*dl)
            } else {
                None
            }
        });
        let alphabetic = !abbr.is_empty() && abbr.chars().all(|c| c.is_ascii_alphabetic());
        let gmt = gmt_name(offset_secs);
        ZoneInfo {
            offset_secs,
            short_name: if alphabetic { abbr } else { gmt.clone() },
            long_name: known.map_or(gmt, str::to_string),
        }
    }
}

/// Java's fallback zone name, e.g. `GMT+05:30`.
fn gmt_name(offset_secs: i32) -> String {
    let sign = if offset_secs < 0 { '-' } else { '+' };
    let mins = offset_secs.abs() / 60;
    format!("GMT{sign}{:02}:{:02}", mins / 60, mins % 60)
}

/// Matches a zone name at the start of `text` (case-insensitive, longest match wins).
/// Returns the offset in seconds and the matched length in chars.
pub(crate) fn match_zone_name(text: &[char]) -> Option<(i32, usize)> {
    let mut best: Option<(i32, usize)> = None;
    for (ss, sl, ds, dl, std_min) in ZONE_NAMES {
        let std_off = std_min * 60;
        for (name, off) in
            [(ss, std_off), (sl, std_off), (ds, std_off + 3600), (dl, std_off + 3600)]
        {
            let len = name.chars().count();
            if len == 0 || len > text.len() || best.is_some_and(|(_, b)| b >= len) {
                continue;
            }
            if name.chars().zip(text).all(|(a, b)| a.eq_ignore_ascii_case(b)) {
                best = Some((off, len));
            }
        }
    }
    best
}
