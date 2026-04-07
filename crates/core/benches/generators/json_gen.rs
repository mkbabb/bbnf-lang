/// Deeply nested objects: {"a":{"a":{"a":...}}} to depth N
pub fn deeply_nested_objects(depth: usize) -> String {
    let mut s = String::with_capacity(depth * 6 + 10);
    for _ in 0..depth {
        s.push_str("{\"a\":");
    }
    s.push_str("null");
    for _ in 0..depth {
        s.push('}');
    }
    s
}

/// Deeply nested arrays: [[[...]]] to depth N
pub fn deeply_nested_arrays(depth: usize) -> String {
    let mut s = String::with_capacity(depth * 2 + 4);
    for _ in 0..depth {
        s.push('[');
    }
    s.push_str("null");
    for _ in 0..depth {
        s.push(']');
    }
    s
}

/// Wide array with N integer elements: [1,2,3,...,N]
pub fn wide_array(count: usize) -> String {
    let mut s = String::from("[");
    for i in 0..count {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&i.to_string());
    }
    s.push(']');
    s
}

/// Wide object with N key-value pairs: {"k0":0,"k1":1,...}
pub fn wide_object(count: usize) -> String {
    let mut s = String::from("{");
    for i in 0..count {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!("\"k{}\":{}", i, i));
    }
    s.push('}');
    s
}

/// Array of N strings each `len` chars long
pub fn long_strings(count: usize, len: usize) -> String {
    let word: String = "abcdefgh".chars().cycle().take(len).collect();
    let mut s = String::from("[");
    for i in 0..count {
        if i > 0 {
            s.push(',');
        }
        s.push('"');
        s.push_str(&word);
        s.push('"');
    }
    s.push(']');
    s
}

/// Array of strings with extensive \u escapes
pub fn escape_heavy(count: usize) -> String {
    let escaped = r#"\u0041\u0042\u0043\u0044\u0045\u0046"#;
    let mut s = String::from("[");
    for i in 0..count {
        if i > 0 {
            s.push(',');
        }
        s.push('"');
        s.push_str(escaped);
        s.push('"');
    }
    s.push(']');
    s
}

/// Alternating types: [null,true,42,"str",[],{},...] repeated
pub fn mixed_types(count: usize) -> String {
    let types = [
        "null",
        "true",
        "false",
        "42",
        "3.14",
        "\"hello\"",
        "[]",
        "{}",
    ];
    let mut s = String::from("[");
    for i in 0..count {
        if i > 0 {
            s.push(',');
        }
        s.push_str(types[i % types.len()]);
    }
    s.push(']');
    s
}
