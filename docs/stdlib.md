# JSLT standard library specification

This document specifies the functions provided by the JSLT standard library (stdlib) implemented by this project. It is adapted from the original JSLT documentation and kept compatible unless otherwise stated.

Conventions:
- Types are JSON types unless otherwise noted: null, boolean, number (integer or decimal), string, array, object.
- "True iff" means true if and only if.
- Unless a function explicitly states otherwise, passing `null` returns `null` and passing a value of an unexpected type is an error.


## General functions

### contains(element, sequence) -> boolean
True if element is contained in sequence. sequence can be an array, a string, or an object.
- If sequence is an array: element must be an array item.
- If sequence is a string: element is stringified; must be a substring. If element is null the result is false.
- If sequence is an object: element is stringified; must be a key in the object.

Examples:
- contains(null, [1, 2, 3]) => false
- contains(1, [1, 2, 3]) => true
- contains(0, [1, 2, 3]) => false
- contains("no", {"no": false}) => true
- contains(1, {"1": false}) => true
- contains("ab", "abc") => true

### size(sequence) -> integer
Returns the number of elements in the sequence, which can be an array, an object, or a string. If sequence is null it returns null.

Examples:
- size([1, 2, 3]) => 3
- size({"1": 3}) => 1
- size("abcdef") => 6
- size(null) => null

### error(message)
Halts the transform with an error carrying the given message.

Example:
- if (not(is-array(.things))) error("'things' is not an array")

### fallback(arg1, arg2, ...) -> value
Returns the first argument that has a value (i.e. not null, not [], not {}).

Examples:
- fallback(.not_existing_key, .another_not_existing, 1) => 1
- fallback(null, [], {}, "value") => "value"

### min(a, b) -> value
Returns the argument that compares as the smallest. If either argument is null the result is null.

Examples:
- min(10, 1) => 1
- min("a", "b") => "a"
- min(10, null) => null

### max(a, b) -> value
Returns the argument that compares as the largest. If either argument is null the result is null.

Examples:
- max(10, 1) => 10
- max("a", "b") => "b"
- max(10, null) => null


## Numeric functions

### is-number(value) -> boolean
True iff the argument is a number.

Examples:
- is-number(null) => false
- is-number(1) => true
- is-number(1.0) => true
- is-number("1") => false

### is-integer(value) -> boolean
True iff the argument is an integral number.

Examples:
- is-integer(null) => false
- is-integer(1) => true
- is-integer(1.0) => false
- is-integer("1") => false

### is-decimal(value) -> boolean
True iff the argument is a floating-point number. 1.0 is considered decimal; 1 is not.

Examples:
- is-decimal(null) => false
- is-decimal(1) => false
- is-decimal(1.0) => true
- is-decimal("1.0") => false

### number(value, fallback?) -> integer|float
Converts the argument into a number, if possible.
- Numbers are returned as-is.
- Strings are parsed into numbers. Supported format equals JSON number literals, except leading zeroes are allowed and the zero before the period may be omitted (".23").
- null returns null.
- Other types cause an error unless fallback is specified. If fallback is specified, it is returned when value is of the wrong type or a string that cannot be parsed.

Examples:
- number(23) => 23
- number("23") => 23
- number("023") => 23
- number(23.0) => 23.0
- number(".23") => 0.23
- number("-.23") => -0.23
- number(null) => null
- number("ab") => error
- number("ab", 0) => 0

### round(x) -> integer
Rounds its argument to the nearest integer. Integers and null are returned untouched. Other types are errors.

Examples:
- round(1) => 1
- round(1.0) => 1
- round(1.51) => 2
- round(null) => null

### floor(x) -> integer
Rounds its argument down to the nearest integer <= x. Integers and null returned untouched. Other types are errors.

Examples:
- floor(1) => 1
- floor(1.0) => 1
- floor(1.51) => 1
- floor(null) => null

### ceiling(x) -> integer
Rounds its argument up to the nearest integer >= x. Integers and null returned untouched. Other types are errors.

Examples:
- ceiling(1) => 1
- ceiling(1.0) => 1
- ceiling(1.51) => 2
- ceiling(null) => null

### random() -> float
Returns a random number between 0.0 and 1.0.

Example: random() => 0.24712712424

### sum(array) -> number
Returns the sum of all the numbers in the array. The parameter must be an array; all values in it must be numbers.

Examples:
- sum([1,2,3]) => 6
- sum([1]) => 1
- sum([1.0, 2.0]) => 3.0
- sum([]) => 0
- sum(null) => null

### mod(a, d) -> integer
Euclidean modulus. Returns a modulo d with result in range 0..abs(d).
Mathematically: a = d * floor(a / d) + mod(a, d) using Euclidean division.

Examples:
- mod(10, 2) => 0
- mod(10, 3) => 1
- mod(10, 4) => 2
- mod(-10, 3) => 2
- mod(-10, -3) => 2
- mod(10, -3) => 1
- mod(null, 2) => null
- mod(10, null) => null
- mod(10.5, 2) => error
- mod(10, 2.1) => error
- mod(10, "2") => error

### hash-int(value) -> integer
Returns an integral hash of the given value. Unlike sha256-hex, this returns a number. There is no guarantee that the same input yields the same output across environments (e.g., different JVM versions).

Examples:
- hash-int("test") => 3556808
- hash-int("") => 310
- hash-int({}) => 8
- hash-int([]) => 1
- hash-int([1,2]) => 8928
- hash-int([2,1]) => 9858
- hash-int([1,2]) != hash-int([2,1]) => true
- hash-int(1) => 248
- hash-int(null) => 6
- hash-int({"a":1,"b":2}) => 10519540
- hash-int({"b":2,"a":1}) => 10519540
- hash-int({"a":1,"b":2}) == hash-int({"b":2,"a":1}) => true


## String functions

### is-string(value) -> boolean
True iff the argument is a string.

Examples:
- is-string(null) => false
- is-string("123") => true
- is-string(123) => false

### string(value) -> string
Converts value to a string representation. Numbers, null, and booleans become their JSON representation inside a string.

Examples:
- string(null) => "null"
- string(123) => "123"
- string("123") => "123"

### test(input, regexp) -> boolean
True iff input matches the regexp (Java regular expressions). Matching a substring is sufficient unless ^ and $ anchors are used. If input is null returns false.

Examples:
- test("123", "\\d+") => true
- test("abc123", "\\d+") => true
- test("abc123", "^\\d+$") => false

Note: When writing regex in JSLT strings, escape backslashes accordingly.

### capture(input, regexp) -> object
If input matches the regexp, returns an object with a key for every named group (?<name>...). If input is null returns null. If the regexp doesn't match an empty object is returned.

Example input:
- {"schema": "http://schemas.schibsted.io/thing/pulse-simple.json#1.json"}
Expression:
- capture(.schema, "http://(?<host>[^/]+)/(?<rest>.+)")
Output:
- {"host": "schemas.schibsted.io", "rest": "thing/pulse-simple.json#1.json"}

### split(input, regexp) -> array
Splits the input string by regexp, returns array of strings. If input is null returns null.

Examples:
- split("1,2,3,4,5", ",") => ["1", "2", "3", "4", "5"]
- split("1,2,3,4,5", ";") => ["1,2,3,4,5"]
- split(null, ";") => null
- split(",2", ",") => ["", "2"]
- split("2,", ",") => ["2"]

### join(array, separator) -> string
Concatenates array elements (stringified via string()) with separator between. If array is null returns null.

Examples:
- join(["a", "b", "c"], " ") => "a b c"
- join(["a"], " ") => "a"
- join(null, "-") => null
- join([1], "-") => "1"

### lowercase(string) -> string
Converts string to lowercase. Naive; does not handle all special Unicode cases.

Examples:
- lowercase("ABCÆØÅ") => "abcæøå"
- lowercase(null) => null

### uppercase(string) -> string
Converts string to uppercase. Naive; only ASCII is fully handled.

Examples:
- uppercase("abcæøå") => "ABCÆØÅ"
- uppercase(null) => null

### sha256-hex(value) -> string
Hexadecimal SHA-256 hash of the input string. Non-string inputs are stringified. null returns null.

Examples:
- sha256-hex("foo") => "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
- sha256-hex("42") => "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"
- sha256-hex(42) => same as above
- sha256-hex(null) => null

### starts-with(tested, prefix) -> boolean
True iff tested starts with prefix. Null tested yields false.

Examples:
- starts-with("prohibition", "pro") => true
- starts-with("prohibition", "pre") => false
- starts-with(null, "pre") => false

### ends-with(tested, suffix) -> boolean
True iff tested ends with suffix. Null tested yields false.

Examples:
- ends-with("prohibition", "pro") => false
- ends-with("prohibition", "ion") => true
- ends-with(null, "ion") => false

### from-json(string, fallback?) -> value
Parses string as JSON and returns the value. If string is null returns null.
- If fallback is omitted: parse error => error.
- If fallback is provided: returned on parse error or wrong type.

Examples:
- from-json("[1,2]") => [1, 2]
- from-json("[1,2", "BAD") => "BAD"
- from-json("[1,2") => error
- from-json(null) => null

### to-json(value) -> string
Serializes any JSON value to a string (JSON text).

Examples:
- to-json([1,2]) => "[1, 2]"
- to-json(1) => "1"
- to-json("foo") => "\"foo\""
- to-json(null) => "null"

### replace(value, regexp, out) -> string
Replaces every substring in value that matches regexp with out. If value is not a string it is stringified except if it is null. regexp and out must be strings. It is an error for regexp ever to match an empty string.

Examples:
- replace("abc def ghi", " ", "-") => "abc-def-ghi"
- replace("abc def ghi", "\\s+", "-") => "abc-def-ghi"
- replace(null, "\\s+", "-") => null
- replace("   whoah", "^\\s+", "") => "whoah"
- replace("abc def ghi", "[a-z]", "x") => "xxx xxx xxx"
- replace("abc def ghi", "[a-z]+", "x") => "x x x"

### trim(value) -> string
Removes leading and trailing whitespace in the input. If input is null, output is null. Non-string inputs are stringified.

Examples:
- trim("  abc  ") => "abc"
- trim("abc") => "abc"
- trim("abc \t\r\n") => "abc"
- trim(false) => "false"
- trim(null) => null

### uuid(msb?, lsb?) -> string
Generates a formatted UUID string with dashes.
- No parameters: random UUID v4.
- Two parameters: UUID v1 based on two long values: first is MSB, second is LSB.
- Both parameters null: NIL UUID 00000000-0000-0000-0000-000000000000

Examples:
- uuid() => "b02c39c0-6f8f-4250-97cd-78500af36e27"
- uuid(1234567890, 1234567890) => "00000000-4996-102d-8000-0000499602d2"
- uuid(0, 0) => "00000000-0000-1000-8000-000000000000"
- uuid(null, null) => "00000000-0000-0000-0000-000000000000"


## Boolean functions

### boolean(value) -> boolean
Converts value to boolean. Considered false: null, [], {}, "", false, 0. Everything else is true.

Examples:
- boolean(null) => false
- boolean("") => false
- boolean(" ") => true
- boolean(0) => false
- boolean(1) => true
- boolean(true) => true
- boolean(false) => false
- boolean([]) => false
- boolean([1]) => true

### not(value) -> boolean
Returns logical negation of boolean(value).

Examples:
- not(null) => true
- not("") => true
- not(" ") => false
- not(0) => true
- not(1) => false
- not(true) => false
- not(false) => true
- not([]) => true
- not([1]) => false

### is-boolean(value) -> boolean
True iff value is a boolean.

Examples:
- is-boolean(null) => false
- is-boolean(true) => true
- is-boolean(false) => true
- is-boolean("") => false
- is-boolean(" ") => false


## Object functions

### is-object(value) -> boolean
True iff value is an object.

Examples:
- is-object(null) => false
- is-object({}) => true
- is-object([]) => false
- is-object("") => false

### get-key(object, key, fallback?) -> value
Equivalent to object.key except key is dynamic.
- If key does not exist and no fallback: returns null.
- If fallback is provided: returns fallback when key does not exist.

Example:
- let lookup = { "no": "Norway", "se": "Sweden" }
- get-key($lookup, "no") => "Norway"
- get-key($lookup, "dk", "<unknown>") => "<unknown>"


## Array functions

### array(value) -> array
Converts the input value to an array. Numbers, booleans, and strings cannot be converted and cause errors.
- Objects are converted to arrays of {"key": <key>, "value": <value>} objects in iteration order.

Examples:
- array(null) => null
- array([1, 2]) => [1, 2]
- array("123") => error
- array({"a": 1, "b": 2}) => [{"key": "a", "value": 1}, {"key": "b", "value": 2}]

### is-array(value) -> boolean
True iff value is an array.

Examples:
- is-array(null) => false
- is-array([1, 2]) => true
- is-array("123") => false

### flatten(array) -> array
Flattens nested arrays at any depth; non-arrays are left as-is. null => null.

Examples:
- flatten([[1,2], [3,4]]) => [1,2,3,4]
- flatten([1, 2, 3, 4]) => [1,2,3,4]
- flatten([1, [2, [3, [4, []]]]]) => [1,2,3,4]
- flatten(null) => null

### all(array) -> boolean
True iff all elements of array evaluate to true. null => null. [] => true. Non-array input => error.

Examples:
- all([true, true, true]) => true
- all([true, true, false]) => false
- all(null) => null
- all([]) => true
- all("") => error

### any(array) -> boolean
True iff any element of array evaluates to true. null => null. [] => false. Non-array input => error.

Examples:
- any([false, false, false]) => false
- any([false, false, true]) => true
- any(null) => null
- any([]) => false
- any("") => error

### zip(array1, array2) -> array
Pairs elements from array1 and array2 into 2-element arrays. Error if lengths differ. null if either input is null.

Examples:
- zip(["a", "b", "c"], [1, 2, 3]) => [["a", 1], ["b", 2], ["c", 3]]
- zip(["a", "b", "c"], null) => null
- zip(null, [1, 2, 3]) => null
- zip([], []) => []
- zip([1], []) => error

### zip-with-index(array) -> array
Maps array elements to objects {"value": <element>, "index": <index>} with index starting at 0. Error if input is not array (or null).

Examples:
- zip-with-index(["a", "b", "c"]) => [{"value": "a", "index": 0}, {"value": "b", "index": 1}, {"value": "c", "index": 2}]
- zip-with-index([]) => []
- zip-with-index(null) => null
- zip-with-index("abc") => error

### index-of(array, value) -> integer
Returns index of value within array, or -1 if not found. Error if array is not an array (or null).

Examples:
- index-of([], 1) => -1
- index-of([0, 1, 2], 1) => 1
- index-of([0, 1, 2, null], null) => 3
- index-of([0, 1, 2], null) => -1
- index-of(null, 1) => null
- index-of(1, 1) => error


## Time functions

### now() -> number
Returns the number of seconds since midnight, January 1, 1970 UTC. Milliseconds are returned as decimals of the number.

Examples:
- now() -> 1.529677371698E9
- round(now()) -> 1529677391

### parse-time(time, format, fallback?) -> number
Parses time with format (Java date/time format) and returns the number of seconds since epoch in UTC. If no timezone is specified in time, UTC is assumed.
- If fallback is omitted: wrong type or mismatch => error.
- If fallback is provided: returned on error.
- If time is null: returns null.

Examples:
- parse-time("2018-05-30T11:46:37Z", "yyyy-MM-dd'T'HH:mm:ssX") => 1.527680797E9
- parse-time("2018-05-30T11:46:37", "yyyy-MM-dd'T'HH:mm:ssX") => error
- parse-time("2018-05-30T11:46:37", "yyyy-MM-dd'T'HH:mm:ssX", null) => null
- parse-time(null, "yyyy-MM-dd'T'HH:mm:ssX") => null

### format-time(timestamp, format, timezone?) -> string
Formats timestamp (seconds since epoch) with format and returns string.
- Default timezone is UTC; can be overridden by timezone argument.
- null timestamp => null.

Examples:
- format-time(1529677391, "yyyy-MM-dd'T'HH:mm:ss") => "2018-06-22T14:23:11"
- format-time(0, "yyyy-MM-dd") => "1970-01-01"
- format-time(null, "yyyy-MM-dd") => null


## Miscellaneous functions

### parse-url(url) -> object
Parses url and returns an object with keys: scheme, userinfo, host, port, path, query, parameters, fragment.
- parameters is an object mapping parameter names to arrays of values; parameters with no value become [null].

Examples:
- parse-url("http://example.com").scheme => "http"
- parse-url("http://example.com").host => "example.com"
- parse-url("http://example.com").path => null
- parse-url("http://example.com/").path => "/"
- parse-url("https://www.example.com/?aa=1&aa=2&bb=&cc").query => "aa=1&aa=2&bb=&cc"
- parse-url("https://www.example.com/?aa=1&aa=2&bb=&cc").parameters.aa => ["1", "2"]
- parse-url("https://www.example.com/?aa=1&aa=2&bb=&cc").parameters.bb => [null]
- parse-url("https://www.example.com/?aa=1&aa=2&bb=&cc").parameters.cc => [null]
- parse-url("ftp://username:password@host.com/").userinfo => "username:password"
- parse-url("https://example.com:8443").port => 8443
