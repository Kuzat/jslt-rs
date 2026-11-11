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

/// Names Java uses for a group of zones. An empty short name means Java has none and
/// prints the `GMT+hh:mm` form. Some zones use one short name for both standard and
/// daylight time.
struct ZoneNames {
    short_std: &'static str,
    long_std: &'static str,
    short_dst: &'static str,
    long_dst: &'static str,
    zones: &'static [&'static str],
}

const fn names(
    short_std: &'static str,
    long_std: &'static str,
    short_dst: &'static str,
    long_dst: &'static str,
    zones: &'static [&'static str],
) -> ZoneNames {
    ZoneNames { short_std, long_std, short_dst, long_dst, zones }
}

/// When parsing, the first entry with a matching name wins (`IST` is Israel, `CST` is
/// US Central), as in Java, and the entry's first zone gives the offset.
const ZONE_NAMES: &[ZoneNames] = &[
    names(
        "UTC",
        "Coordinated Universal Time",
        "",
        "",
        &["UTC", "Etc/UTC", "Etc/UCT", "UCT", "Etc/Universal", "Universal", "Etc/Zulu", "Zulu"],
    ),
    names(
        "GMT",
        "Greenwich Mean Time",
        "BST",
        "British Summer Time",
        &[
            "GMT",
            "Etc/GMT",
            "Etc/GMT0",
            "Etc/GMT+0",
            "Etc/GMT-0",
            "Etc/Greenwich",
            "Greenwich",
            "Europe/London",
            "GB",
            "Europe/Belfast",
            "Europe/Guernsey",
            "Europe/Isle_of_Man",
            "Europe/Jersey",
            "Africa/Abidjan",
            "Africa/Accra",
            "Africa/Dakar",
            "Atlantic/Reykjavik",
            "Iceland",
        ],
    ),
    names(
        "WET",
        "Western European Standard Time",
        "WEST",
        "Western European Summer Time",
        &[
            "WET",
            "Europe/Lisbon",
            "Portugal",
            "Atlantic/Canary",
            "Atlantic/Madeira",
            "Atlantic/Faroe",
        ],
    ),
    names(
        "CET",
        "Central European Standard Time",
        "CEST",
        "Central European Summer Time",
        &[
            "CET",
            "MET",
            "Europe/Oslo",
            "Europe/Stockholm",
            "Europe/Copenhagen",
            "Europe/Berlin",
            "Europe/Paris",
            "Europe/Madrid",
            "Europe/Rome",
            "Europe/Amsterdam",
            "Europe/Brussels",
            "Europe/Luxembourg",
            "Europe/Vienna",
            "Europe/Zurich",
            "Europe/Prague",
            "Europe/Warsaw",
            "Poland",
            "Europe/Budapest",
            "Europe/Belgrade",
            "Europe/Zagreb",
            "Europe/Ljubljana",
            "Europe/Bratislava",
            "Europe/Sarajevo",
            "Europe/Skopje",
            "Europe/Podgorica",
            "Europe/Tirane",
            "Europe/Monaco",
            "Europe/Malta",
            "Europe/Andorra",
            "Europe/Gibraltar",
            "Europe/Vaduz",
            "Europe/San_Marino",
            "Europe/Vatican",
            "Europe/Busingen",
            "Arctic/Longyearbyen",
            "Atlantic/Jan_Mayen",
            "Africa/Ceuta",
            "Africa/Tunis",
            "Africa/Algiers",
        ],
    ),
    names(
        "EET",
        "Eastern European Standard Time",
        "EEST",
        "Eastern European Summer Time",
        &[
            "EET",
            "Europe/Helsinki",
            "Europe/Mariehamn",
            "Europe/Tallinn",
            "Europe/Riga",
            "Europe/Vilnius",
            "Europe/Kiev",
            "Europe/Kyiv",
            "Europe/Uzhgorod",
            "Europe/Zaporozhye",
            "Europe/Chisinau",
            "Europe/Bucharest",
            "Europe/Sofia",
            "Europe/Athens",
            "Asia/Nicosia",
            "Europe/Nicosia",
            "Asia/Famagusta",
            "Asia/Beirut",
            "Africa/Cairo",
            "Egypt",
            "Africa/Tripoli",
            "Libya",
            "Europe/Kaliningrad",
        ],
    ),
    names(
        "IST",
        "Israel Standard Time",
        "IDT",
        "Israel Daylight Time",
        &["Asia/Jerusalem", "Asia/Tel_Aviv", "Israel"],
    ),
    // tzdata models Irish summer time as standard time with negative DST in winter. The
    // offsets come from Europe/London, which has Java's positive DST and the same clock
    // (London itself matches the entry above first).
    names(
        "GMT",
        "Greenwich Mean Time",
        "IST",
        "Irish Standard Time",
        &["Europe/London", "Europe/Dublin", "Eire"],
    ),
    names(
        "MSK",
        "Moscow Standard Time",
        "MSK",
        "Moscow Summer Time",
        &["Europe/Moscow", "W-SU", "Europe/Simferopol", "Europe/Kirov", "Europe/Volgograd"],
    ),
    names("", "Gulf Standard Time", "", "", &["Asia/Dubai", "Asia/Muscat"]),
    names(
        "IST",
        "India Standard Time",
        "IST",
        "Kolkata Daylight Time",
        &["Asia/Kolkata", "Asia/Calcutta"],
    ),
    names("", "Singapore Standard Time", "", "", &["Asia/Singapore", "Singapore"]),
    names(
        "HKT",
        "Hong Kong Standard Time",
        "HKST",
        "Hong Kong Summer Time",
        &["Asia/Hong_Kong", "Hongkong"],
    ),
    names(
        "AWST",
        "Australian Western Standard Time",
        "AWDT",
        "Australian Western Daylight Time",
        &["Australia/Perth", "Australia/West"],
    ),
    names("JST", "Japan Standard Time", "JDT", "Japan Daylight Time", &["Asia/Tokyo", "Japan"]),
    names("KST", "Korean Standard Time", "KST", "Korean Daylight Time", &["Asia/Seoul", "ROK"]),
    names(
        "ACST",
        "Australian Central Standard Time",
        "ACDT",
        "Australian Central Daylight Time",
        &[
            "Australia/Adelaide",
            "Australia/South",
            "Australia/Darwin",
            "Australia/North",
            "Australia/Broken_Hill",
        ],
    ),
    names(
        "AEST",
        "Australian Eastern Standard Time",
        "AEDT",
        "Australian Eastern Daylight Time",
        &[
            "Australia/Sydney",
            "Australia/NSW",
            "Australia/ACT",
            "Australia/Canberra",
            "Australia/Melbourne",
            "Australia/Victoria",
            "Australia/Hobart",
            "Australia/Tasmania",
            "Australia/Brisbane",
            "Australia/Queensland",
            "Australia/Lindeman",
        ],
    ),
    names(
        "NZST",
        "New Zealand Standard Time",
        "NZDT",
        "New Zealand Daylight Time",
        &["Pacific/Auckland", "NZ"],
    ),
    names(
        "HST",
        "Hawaii-Aleutian Standard Time",
        "HDT",
        "Hawaii-Aleutian Daylight Time",
        &["HST", "Pacific/Honolulu", "US/Hawaii", "America/Adak", "US/Aleutian"],
    ),
    names(
        "AKST",
        "Alaska Standard Time",
        "AKDT",
        "Alaska Daylight Time",
        &[
            "America/Anchorage",
            "US/Alaska",
            "America/Juneau",
            "America/Sitka",
            "America/Nome",
            "America/Yakutat",
            "America/Metlakatla",
        ],
    ),
    names(
        "PST",
        "Pacific Standard Time",
        "PDT",
        "Pacific Daylight Time",
        &[
            "America/Los_Angeles",
            "US/Pacific",
            "PST8PDT",
            "America/Vancouver",
            "Canada/Pacific",
            "America/Tijuana",
        ],
    ),
    names(
        "MST",
        "Mountain Standard Time",
        "MDT",
        "Mountain Daylight Time",
        &[
            "MST",
            "MST7MDT",
            "America/Denver",
            "US/Mountain",
            "America/Phoenix",
            "US/Arizona",
            "America/Boise",
            "America/Edmonton",
            "Canada/Mountain",
        ],
    ),
    names(
        "CST",
        "Central Standard Time",
        "CDT",
        "Central Daylight Time",
        &[
            "CST6CDT",
            "America/Chicago",
            "US/Central",
            "America/Winnipeg",
            "Canada/Central",
            "America/Regina",
            "America/Mexico_City",
            "America/Indiana/Knox",
            "America/Menominee",
            "America/North_Dakota/Center",
        ],
    ),
    names(
        "EST",
        "Eastern Standard Time",
        "EDT",
        "Eastern Daylight Time",
        &[
            "EST",
            "EST5EDT",
            "America/New_York",
            "US/Eastern",
            "America/Toronto",
            "Canada/Eastern",
            "America/Detroit",
            "US/Michigan",
            "America/Indiana/Indianapolis",
            "America/Indianapolis",
            "America/Kentucky/Louisville",
            "America/Louisville",
            "America/Panama",
            "America/Jamaica",
        ],
    ),
    names(
        "AST",
        "Atlantic Standard Time",
        "ADT",
        "Atlantic Daylight Time",
        &[
            "America/Halifax",
            "Canada/Atlantic",
            "Atlantic/Bermuda",
            "America/Puerto_Rico",
            "America/Barbados",
        ],
    ),
    names(
        "CST",
        "China Standard Time",
        "CDT",
        "China Daylight Time",
        &["Asia/Shanghai", "PRC", "Asia/Chongqing", "Asia/Harbin"],
    ),
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
        let names = match self {
            Zone::Utc => ZONE_NAMES.first(),
            Zone::Named(tz) => ZONE_NAMES.iter().find(|n| n.zones.contains(&tz.name())),
        };
        // The tzdata abbreviation says which half applies when it is distinct; otherwise
        // the DST flag decides.
        let (short, long) = match names {
            Some(n) => {
                let distinct = n.short_std != n.short_dst;
                let is_dst = if distinct && !n.short_dst.is_empty() && n.short_dst == abbr {
                    true
                } else if distinct && !n.short_std.is_empty() && n.short_std == abbr {
                    false
                } else {
                    dst
                };
                if is_dst {
                    (n.short_dst, n.long_dst)
                } else {
                    (n.short_std, n.long_std)
                }
            }
            None => ("", ""),
        };
        let alphabetic = !abbr.is_empty() && abbr.chars().all(|c| c.is_ascii_alphabetic());
        let gmt = gmt_name(offset_secs);
        let short_name = if !short.is_empty() {
            short.to_string()
        } else if alphabetic && names.is_none() {
            abbr
        } else {
            gmt.clone()
        };
        ZoneInfo {
            offset_secs,
            short_name,
            long_name: if long.is_empty() { gmt } else { long.to_string() },
        }
    }
}

/// Java's fallback zone name, e.g. `GMT+05:30`.
fn gmt_name(offset_secs: i32) -> String {
    let sign = if offset_secs < 0 { '-' } else { '+' };
    let mins = offset_secs.abs() / 60;
    format!("GMT{sign}{:02}:{:02}", mins / 60, mins % 60)
}

/// A zone read by the `z`/`Z`/`X` parsers.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ParsedZone {
    Offset(i32),
    /// A named zone. `dst` is `Some` when the name says standard or daylight time, and
    /// `None` when the zone uses one name for both.
    Named {
        tz: Tz,
        dst: Option<bool>,
    },
}

impl ParsedZone {
    /// Offset in seconds for a local time given as milliseconds since 1970-01-01. Named
    /// zones use their standard offset on that date, like Java.
    pub(crate) fn offset_for_local(&self, local_ms: i64) -> Option<i32> {
        let (tz, dst) = match *self {
            ParsedZone::Offset(secs) => return Some(secs),
            ParsedZone::Named { tz, dst } => (tz, dst),
        };
        let offset_at = |utc_ms: i64| -> Option<i32> {
            let utc = DateTime::<Utc>::from_timestamp_millis(utc_ms)?;
            let off = tz.offset_from_utc_datetime(&utc.naive_utc());
            let base = off.base_utc_offset().num_seconds() as i32;
            let actual_dst = off.dst_offset().num_seconds() as i32;
            Some(base + dst.map_or(actual_dst, |d| if d { 3600 } else { 0 }))
        };
        let guess = local_ms.checked_sub(offset_at(local_ms)? as i64 * 1000)?;
        offset_at(guess)
    }
}

/// Matches a zone name at the start of `text` (case-insensitive, longest match wins).
/// Returns the zone and the matched length in chars.
pub(crate) fn match_zone_name(text: &[char]) -> Option<(ParsedZone, usize)> {
    let mut best: Option<(ParsedZone, usize)> = None;
    for n in ZONE_NAMES {
        let Some(tz) = n.zones.first().and_then(|z| z.parse::<Tz>().ok()) else {
            continue;
        };
        // Long names always say standard or daylight time; a short name shared by both
        // leaves the zone's own DST in effect.
        let shared_short = n.short_std == n.short_dst;
        for (name, is_dst) in
            [(n.short_std, false), (n.long_std, false), (n.short_dst, true), (n.long_dst, true)]
        {
            let len = name.chars().count();
            if len == 0 || len > text.len() || best.is_some_and(|(_, b)| b >= len) {
                continue;
            }
            if name.chars().zip(text).all(|(a, b)| a.eq_ignore_ascii_case(b)) {
                let is_short = name == n.short_std || name == n.short_dst;
                let dst = if is_short && shared_short { None } else { Some(is_dst) };
                best = Some((ParsedZone::Named { tz, dst }, len));
            }
        }
    }
    best
}
