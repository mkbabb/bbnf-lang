/// Grammar with N alternation branches testing dispatch table limits.
pub fn wide_alternation_grammar(branches: usize) -> String {
    let mut s = String::from("root = ");
    for i in 0..branches {
        if i > 0 {
            s.push_str(" | ");
        }
        s.push_str(&format!("\"kw{}\"", i));
    }
    s.push_str(" ;\n");
    s
}

/// Chain of N rules each referencing the next.
pub fn deep_recursion_grammar(depth: usize) -> String {
    let mut s = String::new();
    for i in 0..depth {
        if i < depth - 1 {
            s.push_str(&format!("rule{} = \"(\" , rule{} , \")\" ;\n", i, i + 1));
        } else {
            s.push_str(&format!("rule{} = \"x\" ;\n", i));
        }
    }
    // Root
    s.insert_str(0, "root = rule0 ;\n");
    s
}

/// Grammar with N rules having overlapping FIRST sets.
pub fn ambiguous_first_sets(count: usize) -> String {
    let mut s = String::from("root = ");
    for i in 0..count {
        if i > 0 {
            s.push_str(" | ");
        }
        // All start with "a" but differ after
        s.push_str(&format!("\"a\" , \"{}\"", (b'a' + (i % 26) as u8) as char));
    }
    s.push_str(" ;\n");
    s
}

/// Grammar with N distinct regex patterns.
pub fn many_regex_patterns(count: usize) -> String {
    let mut s = String::from("root = ");
    for i in 0..count {
        if i > 0 {
            s.push_str(" | ");
        }
        s.push_str(&format!("r{}", i));
    }
    s.push_str(" ;\n");
    for i in 0..count {
        // Each rule has a unique regex
        let chars: String = (0..3)
            .map(|j| (b'a' + ((i * 3 + j) % 26) as u8) as char)
            .collect();
        s.push_str(&format!("r{} = /[{}]+/ ;\n", i, chars));
    }
    s
}

/// Nested repetitions: a = (b)*; b = (c)*; ...
pub fn nested_repeat_grammar(depth: usize) -> String {
    let mut s = String::new();
    for i in 0..depth {
        if i < depth - 1 {
            s.push_str(&format!("rule{} = rule{}* ;\n", i, i + 1));
        } else {
            s.push_str(&format!("rule{} = /[a-z]/ ;\n", i));
        }
    }
    s.insert_str(0, "root = rule0 ;\n");
    s
}
