use formatter::format_source;

fn main() {
    let source = r#"def  add(x,y)    x+y

let   foo=1

{ "result": .data }"#;

    match format_source(source) {
        Ok(formatted) => {
            println!("Input:");
            println!("{}", source);
            println!("\nFormatted:");
            println!("{}", formatted);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
}
