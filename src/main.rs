use jslt_rs::Jslt;

fn main() {
    let test_jslt = r#"
    {
        "foo": "bar",
        "baz": 1,
        "qux": true,
        "quux": null,
        "corge": [1, 2, 3],
        "grault": {
            "garply": "waldo"
        },
        "user": .name
    }
    "#;

    let test_json = "{\"user\": {\"name\": \"John Doe\"}}";

    let mut jslt = Jslt::compile(test_jslt);
    let result = jslt.apply(test_json).unwrap();
    println!("{}", result)
}
