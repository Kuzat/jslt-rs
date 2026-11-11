use serde_json::{Map, Value};

// Very small RFC3986-ish parser sufficient for stdlib semantics and the conformance tests.
// scheme ":" "//" authority path? query? fragment?
// authority = [ userinfo "@" ] host [ ":" port ]
pub fn parse_url(url: &str) -> Result<Map<String, Value>, String> {
    let mut out = Map::new();

    // scheme
    let (scheme_opt, rest_after_scheme) = match url.find(':') {
        Some(idx) if idx > 0 => {
            let scheme = &url[..idx];
            (Some(scheme), &url[idx + 1..])
        }
        _ => (None, url),
    };

    let mut rest = rest_after_scheme;

    // skip leading "//" only if present
    let mut userinfo: Option<String> = None;
    let mut host: Option<String> = None;
    let mut port_num: Option<u16> = None;

    if rest.starts_with("//") {
        rest = &rest[2..];

        // Split off path-start (first '/' or '?' or '#' ends authority)
        let auth_end = rest.find(['/', '?', '#']).unwrap_or(rest.len());
        let authority = &rest[..auth_end];
        rest = &rest[auth_end..];

        // userinfo@
        let (maybe_userinfo, hostport) = match authority.rsplit_once('@') {
            Some((ui, hp)) => (Some(ui.to_string()), hp),
            None => (None, authority),
        };
        if let Some(ui) = maybe_userinfo {
            if !ui.is_empty() {
                userinfo = Some(ui);
            }
        }

        // host[:port]
        if let Some((h, p)) = hostport.rsplit_once(':') {
            // Only treat as port if part after ':' is an integer (no spaces)
            if !p.is_empty() && p.chars().all(|c| c.is_ascii_digit()) {
                host = Some(h.to_string());
                // safe: ASCII digits and length limited by input
                let pn: u16 = p.parse().unwrap_or(0);
                port_num = Some(pn);
            } else {
                host = Some(hostport.to_string());
            }
        } else if !hostport.is_empty() {
            host = Some(hostport.to_string());
        }
    }

    // path, query, fragment
    let mut query: Option<String> = None;
    let mut fragment: Option<String> = None;

    // fragment start at '#'
    if let Some(hash_pos) = rest.find('#') {
        fragment = Some(rest[hash_pos + 1..].to_string());
        rest = &rest[..hash_pos];
    }

    // query start at '?'
    if let Some(q_pos) = rest.find('?') {
        query = Some(rest[q_pos + 1..].to_string());
        rest = &rest[..q_pos];
    }

    // Like Java's URL: only present, non-empty parts are included (the fragment may be empty).
    if let Some(ui) = userinfo {
        out.insert("userinfo".to_string(), Value::String(ui));
    }
    if let Some(h) = host.filter(|h| !h.is_empty()) {
        out.insert("host".to_string(), Value::String(h));
    }
    if let Some(p) = port_num {
        out.insert("port".to_string(), Value::Number(p.into()));
    }
    if !rest.is_empty() {
        out.insert("path".to_string(), Value::String(rest.to_string()));
    }
    if let Some(s) = scheme_opt.filter(|s| !s.is_empty()) {
        out.insert("scheme".to_string(), Value::String(s.to_string()));
    }
    if let Some(qs) = query.filter(|q| !q.is_empty()) {
        out.insert("parameters".to_string(), Value::Object(parse_query(&qs)?));
        out.insert("query".to_string(), Value::String(qs));
    }
    if let Some(f) = fragment {
        out.insert("fragment".to_string(), Value::String(f));
    }

    Ok(out)
}

/// Splits a query into `name -> [values]` like Java JSLT. A pair without `=` (or with nothing
/// after it) adds `null`. Names and values are URL-decoded unless the pair starts with `=`.
fn parse_query(qs: &str) -> Result<Map<String, Value>, String> {
    let mut params = Map::new();
    for pair in qs.split('&') {
        let (name, value) = match pair.find('=') {
            Some(idx) if idx > 0 => {
                let v = &pair[idx + 1..];
                let value = if v.is_empty() { None } else { Some(url_decode(v)?) };
                (url_decode(&pair[..idx])?, value)
            }
            _ => (pair.to_string(), None),
        };
        let entry = params.entry(name).or_insert_with(|| Value::Array(Vec::new()));
        if let Value::Array(values) = entry {
            values.push(value.map_or(Value::Null, Value::String));
        }
    }
    Ok(params)
}

/// `application/x-www-form-urlencoded` decoding, like Java's `URLDecoder` with UTF-8.
fn url_decode(s: &str) -> Result<String, String> {
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'+' => out.push(b' '),
            b'%' => {
                let hex = bytes
                    .get(i + 1..i + 3)
                    .and_then(|h| std::str::from_utf8(h).ok())
                    .and_then(|h| u8::from_str_radix(h, 16).ok())
                    .ok_or_else(|| format!("invalid escape in query: {s}"))?;
                out.push(hex);
                i += 2;
            }
            b => out.push(b),
        }
        i += 1;
    }
    Ok(String::from_utf8_lossy(&out).into_owned())
}
