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
        "name": .user.name,
        "age": .age,
        "newPhonePrice": .device.new.price,
        "oldPhonePrice": .device.old.price
    }
    "#;

    let test_json = r#"
    {
      "user": { "name": "John Doe" },
      "age": 22,
      "isStudent": true,
      "device": {
        "new": {
          "name": "iPhone 6",
          "price": 699
        },
        "old": {
          "name": "iPhone 5",
          "price": 399
        }
      }
    }
    "#;

    let mut jslt = Jslt::compile(test_jslt);
    let result = jslt.apply(test_json).unwrap();
    println!("{}", result)
}
