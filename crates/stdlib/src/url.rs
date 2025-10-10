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
        let auth_end =
            rest.find(['/', '?', '#']).unwrap_or(rest.len());
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
    let mut path: Option<String> = None;
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

    // path is what's left; empty string => null. "/" should be kept
    if !rest.is_empty() {
        path = Some(rest.to_string());
    }

    // parameters object from query per spec
    let mut params_obj = Map::new();
    if let Some(qs) = &query {
        if !qs.is_empty() {
            for pair in qs.split('&') {
                // name[=value], empty value -> null in array; missing '=' -> [null]
                let (name, value_opt) = match pair.split_once('=') {
                    Some((n, v)) => (n, Some(v)),
                    None => (pair, None),
                };
                if name.is_empty() {
                    continue;
                }
                let arr =
                    params_obj.entry(name.to_string()).or_insert_with(|| Value::Array(Vec::new()));
                match arr {
                    Value::Array(vec) => {
                        if let Some(v) = value_opt {
                            if v.is_empty() {
                                vec.push(Value::Null);
                            } else {
                                vec.push(Value::String(v.to_string()));
                            }
                        } else {
                            vec.push(Value::Null);
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    if let Some(ui) = userinfo {
        out.insert("userinfo".to_string(), Value::String(ui));
    }

    if let Some(h) = host {
        out.insert("host".to_string(), Value::String(h));
    }

    if let Some(p) = port_num {
        out.insert("port".to_string(), Value::Number(p.into()));
    }

    if let Some(p) = path {
        out.insert("path".to_string(), Value::String(p));
    } else {
        out.insert("path".to_string(), Value::Null);
    }

    if let Some(s) = scheme_opt {
        out.insert("scheme".to_string(), Value::String(s.to_string()));
    }

    if let Some(q) = query {
        out.insert("query".to_string(), Value::String(q));
    }
    out.insert("parameters".to_string(), Value::Object(params_obj));

    if let Some(f) = fragment {
        out.insert("fragment".to_string(), Value::String(f));
    }

    Ok(out)
}
