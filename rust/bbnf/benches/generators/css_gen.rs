/// N separate CSS rules with single declarations
pub fn many_rules(count: usize) -> String {
    let mut s = String::with_capacity(count * 30);
    for i in 0..count {
        s.push_str(&format!(".class-{} {{ color: red; }}\n", i));
    }
    s
}

/// Single rule with N property declarations
pub fn many_declarations(count: usize) -> String {
    let props = [
        "color",
        "margin",
        "padding",
        "font-size",
        "display",
        "width",
        "height",
        "border",
        "background",
        "opacity",
    ];
    let vals = [
        "red",
        "10px",
        "20px",
        "16px",
        "block",
        "100%",
        "auto",
        "1px solid",
        "#fff",
        "0.5",
    ];
    let mut s = String::from(".test {\n");
    for i in 0..count {
        let p = props[i % props.len()];
        let v = vals[i % vals.len()];
        s.push_str(&format!("  {}: {};\n", p, v));
    }
    s.push_str("}\n");
    s
}

/// Wide selector list: .a, .b, .c, ..., .N { }
pub fn wide_selector_list(count: usize) -> String {
    let mut s = String::new();
    for i in 0..count {
        if i > 0 {
            s.push_str(", ");
        }
        s.push_str(&format!(".sel-{}", i));
    }
    s.push_str(" { color: red; }\n");
    s
}

/// Deeply nested selectors: .a .b .c ... { }
pub fn deeply_nested_selectors(depth: usize) -> String {
    let mut s = String::new();
    for i in 0..depth {
        if i > 0 {
            s.push(' ');
        }
        s.push_str(&format!(".level-{}", i));
    }
    s.push_str(" { color: red; }\n");
    s
}

/// Nested calc() values: calc(1px + calc(2px + calc(...)))
pub fn complex_values(depth: usize) -> String {
    let mut s = String::from(".test { width: ");
    for i in 0..depth {
        s.push_str(&format!("calc({}px + ", i + 1));
    }
    s.push_str("0px");
    for _ in 0..depth {
        s.push(')');
    }
    s.push_str("; }\n");
    s
}

/// @media query nesting to depth N
pub fn media_query_nesting(depth: usize) -> String {
    let mut s = String::new();
    for i in 0..depth {
        s.push_str(&format!(
            "@media (min-width: {}px) {{\n",
            (i + 1) * 100
        ));
    }
    s.push_str(".inner { color: red; }\n");
    for _ in 0..depth {
        s.push_str("}\n");
    }
    s
}

/// Identifiers that stress scan_ident: long, hyphenated, vendor-prefixed
pub fn adversarial_identifiers() -> String {
    let mut s = String::new();
    // Long custom properties
    for i in 0..100 {
        let name: String = (0..50)
            .map(|j| {
                if j % 5 == 0 {
                    '-'
                } else {
                    (b'a' + (j % 26) as u8) as char
                }
            })
            .collect();
        s.push_str(&format!(".cls {{ --{}-{}: value; }}\n", name, i));
    }
    // Long class names with hyphens
    for i in 0..100 {
        let name: String = (0..40)
            .map(|j| {
                if j % 3 == 0 {
                    '-'
                } else {
                    (b'a' + ((j + i) % 26) as u8) as char
                }
            })
            .collect();
        s.push_str(&format!(".{} {{ display: block; }}\n", name));
    }
    s
}
