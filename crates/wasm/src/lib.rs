use serde::Serialize;
use serde_json::Value;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn init_panic_hook() {
    // Better error message in console on panic
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub struct Program {
    inner: engine::CompiledProgram,
}

#[wasm_bindgen]
impl Program {
    /// Compile a JSLT source string into an executable Program
    #[wasm_bindgen(js_name = compile)]
    pub fn compile(src: &str) -> Result<Program, JsValue> {
        engine::compile(src)
            .map(|inner| Program { inner })
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    /// Apply the compiled program to input JSON.
    /// input: any JS value convertible to JSON (object, array, string, number, boolean, null)
    /// returns: JSON value as a JS value
    #[wasm_bindgen]
    pub fn apply(&self, input: JsValue) -> Result<JsValue, JsValue> {
        // use web_sys::console;

        let v: Value = serde_wasm_bindgen::from_value(input)
            .map_err(|e| JsValue::from_str(&format!("Input JSON parse error: {}", e)))?;

        // console::log_1(&JsValue::from_str(&format!("input: {:?}", v)));

        // Optional: allow passing an eval config later; for now use default.
        let out = self
            .inner
            .apply(&v, None)
            .map_err(|e| JsValue::from_str(&format!("Program error: {}", e)))?;

        // console::log_1(&JsValue::from_str(&format!("output: {:?}", out)));

        let serializer = serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
        let js_out = out
            .serialize(&serializer)
            .map_err(|e| JsValue::from_str(&format!("Output JSON serialization error: {}", e)))?;
        Ok(js_out)
    }

    /// Returns the version of the JSLT engine.
    #[wasm_bindgen(js_name = version)]
    pub fn version() -> String {
        env!("CARGO_PKG_VERSION").to_string()
    }
}
