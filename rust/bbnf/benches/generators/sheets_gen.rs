/// Deeply nested functions: SUM(SUM(SUM(...)))
pub fn deeply_nested_functions(depth: usize) -> String {
    let mut s = String::with_capacity(depth * 6);
    for _ in 0..depth {
        s.push_str("SUM(");
    }
    s.push('1');
    for _ in 0..depth {
        s.push(')');
    }
    s
}

/// Wide function args: SUM(1,2,3,...,N)
pub fn wide_function_args(count: usize) -> String {
    let mut s = String::from("SUM(");
    for i in 0..count {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&i.to_string());
    }
    s.push(')');
    s
}

/// Nested LET bindings: LET(a,1,LET(b,2,LET(...,result)))
pub fn nested_let_bindings(depth: usize) -> String {
    let mut s = String::with_capacity(depth * 20);
    for i in 0..depth {
        s.push_str(&format!("LET(v{},{},", i, i));
    }
    s.push_str("v0");
    for _ in 0..depth {
        s.push(')');
    }
    s
}

/// Complex LAMBDA chain: MAP(A1:A10,LAMBDA(x,MAP(B1:B10,LAMBDA(y,...))))
pub fn complex_lambda_chain(depth: usize) -> String {
    let mut s = String::with_capacity(depth * 40);
    for i in 0..depth {
        let col = (b'A' + (i % 26) as u8) as char;
        s.push_str(&format!("MAP({}1:{}10,LAMBDA(v{},", col, col, i));
    }
    s.push_str("v0+1");
    for _ in 0..depth {
        s.push_str("))");
    }
    s
}

/// Large array literal: {1,2,3,...,N;1,2,...}
pub fn large_array_literal(count: usize) -> String {
    let row_size = (count as f64).sqrt() as usize;
    let rows = count / row_size.max(1);
    let mut s = String::from("{");
    for r in 0..rows.max(1) {
        if r > 0 {
            s.push(';');
        }
        for c in 0..row_size.max(1) {
            if c > 0 {
                s.push(',');
            }
            s.push_str(&(r * row_size + c).to_string());
        }
    }
    s.push('}');
    s
}
