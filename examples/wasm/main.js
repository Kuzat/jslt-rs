import init, { Program } from "./pkg/jslt_wasm.js";

const status = document.getElementById("status");
const progEl = document.getElementById("program");
const inputEl = document.getElementById("input");
const outputEl = document.getElementById("output");
const errorEl = document.getElementById("error");
const runBtn = document.getElementById("run");

let ready = false;

async function bootstrap() {
    status.textContent = "Loading WASM...";
    await init(); // initializes the wasm module
    ready = true;
    status.textContent = "Ready";
}

runBtn.addEventListener("click", () => {
    errorEl.textContent = "";
    outputEl.value = "";

    if (!ready) {
        errorEl.textContent = "WASM not loaded yet.";
        return;
    }

    const src = progEl.value;
    let input;
    try {
        input = JSON.parse(inputEl.value);
        console.log(`parsed input: ${JSON.stringify(input)}`);
    } catch (e) {
        errorEl.textContent = `Invalid input JSON: ${e}`;
        return;
    }

    try {
        const program = Program.compile(src); // may throw on parse/bind errors
        const result = program.apply(input);  // may throw on runtime errors
        console.log(`result: ${JSON.stringify(result)}`);
        console.log("typeof result:", typeof result);
        console.log("proto:", result && Object.getPrototypeOf(result)?.constructor?.name);
        console.log("keys:", result && Object.keys(result));

        outputEl.value = JSON.stringify(result, null, 2);
    } catch (e) {
        errorEl.textContent = String(e);
    }
});

bootstrap().catch(e => {
    errorEl.textContent = `Failed to initialize: ${e}`;
    status.textContent = "Error";
});