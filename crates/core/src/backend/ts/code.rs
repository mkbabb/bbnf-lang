//! TsCode output type + TsEmitter / TsEmitCtx structs.

/// A TS code fragment: setup statements + result expression.
#[derive(Clone, Default)]
pub struct TsCode {
    pub stmts: String,
    pub expr: String,
}

impl TsCode {
    pub fn expr(e: impl Into<String>) -> Self {
        Self { stmts: String::new(), expr: e.into() }
    }

    pub fn new(stmts: String, expr: impl Into<String>) -> Self {
        Self { stmts, expr: expr.into() }
    }

    pub fn dissolve(self, parent: &mut String) -> String {
        if !self.stmts.is_empty() {
            parent.push_str(&self.stmts);
        }
        self.expr
    }

    pub fn bind_checked(self, var: &str, parent: &mut String) -> String {
        if !self.stmts.is_empty() {
            parent.push_str(&self.stmts);
        }
        parent.push_str(&format!(
            "const {var} = {expr};\nif ({var} === null) return null;\n",
            expr = self.expr
        ));
        var.to_string()
    }

    pub fn as_expr(&self) -> String {
        if self.stmts.is_empty() {
            self.expr.clone()
        } else {
            format!("((() => {{ {}return {}; }})())", self.stmts, self.expr)
        }
    }
}

/// TS emitter state.
pub struct TsEmitter {
    pub enum_name: String,
}

/// Mutable context for TS emission.
pub struct TsEmitCtx {
    counter: usize,
    pub hoisted_regexes: Vec<String>,
}

impl Default for TsEmitCtx {
    fn default() -> Self {
        Self { counter: 0, hoisted_regexes: Vec::new() }
    }
}

impl TsEmitCtx {
    pub fn fresh(&mut self, prefix: &str) -> String {
        let id = self.counter;
        self.counter += 1;
        format!("__{prefix}{id}")
    }
}
