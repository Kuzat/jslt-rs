#[cfg(feature = "chrono")]
mod pattern;
#[cfg(feature = "chrono")]
mod zones;

#[cfg(feature = "chrono")]
pub use pattern::JavaPattern;
#[cfg(feature = "chrono")]
pub use zones::Zone;

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
