use ir::{AltMode, BackendExpr, BackendRule};
use std::fmt::Write;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum TapeFlavor {
    Eager,
    Offset,
}

impl TapeFlavor {
    fn runtime_type(self) -> &'static str {
        match self {
            Self::Eager => "EagerTapeRule",
            Self::Offset => "OffsetTapeRule",
        }
    }

    fn prefix(self) -> &'static str {
        match self {
            Self::Eager => "eager",
            Self::Offset => "offset",
        }
    }

    fn span_op(self) -> &'static str {
        match self {
            Self::Eager => "capture_span_value",
            Self::Offset => "record_span_offsets",
        }
    }

    fn tape_op(self) -> &'static str {
        match self {
            Self::Eager => "write_tape_value",
            Self::Offset => "write_tape_offset",
        }
    }
}

pub(super) fn render_rule(rule: &BackendRule, flavor: TapeFlavor) -> String {
    let mut ops = Vec::new();
    ops.push(format!("enter_rule({})", rule.name));
    render_expr(&rule.expr, flavor, &mut ops);
    ops.push("finish_rule".to_string());

    let mut out = format!(
        "runtime_plan::{} generated_runtime=ParserState+TapeBuilder rule={} ops={}\n",
        flavor.runtime_type(),
        rule.name,
        ops.len()
    );
    for op in ops {
        out.push_str("  ");
        out.push_str(&op);
        out.push('\n');
    }
    out
}

fn render_expr(expr: &BackendExpr, flavor: TapeFlavor, ops: &mut Vec<String>) {
    match expr {
        BackendExpr::Entry(inner) => {
            ops.push("entry_begin".to_string());
            render_expr(inner, flavor, ops);
            ops.push("entry_end".to_string());
        }
        BackendExpr::Seq(children) => {
            ops.push(format!("seq_begin({})", children.len()));
            for child in children {
                render_expr(child, flavor, ops);
            }
            ops.push("seq_end".to_string());
        }
        BackendExpr::Alt { mode, branches } => {
            ops.push(format!("{}_begin({})", alt_label(*mode), branches.len()));
            for branch in branches {
                render_expr(branch, flavor, ops);
            }
            ops.push(format!("{}_end", alt_label(*mode)));
        }
        BackendExpr::RepeatLoop { body, min } => {
            ops.push(format!("repeat_begin(min={min})"));
            render_expr(body, flavor, ops);
            ops.push("repeat_end".to_string());
        }
        BackendExpr::OptionalBranch(inner) => {
            ops.push("optional_begin".to_string());
            render_expr(inner, flavor, ops);
            ops.push("optional_end".to_string());
        }
        BackendExpr::ByteLiteral(bytes) => {
            ops.push(format!(
                "{}_match_literal_hex({}) -> ParserState::match_literal",
                flavor.prefix(),
                literal_hex(bytes)
            ));
        }
        BackendExpr::RegexProgram { pattern, span_kind } => {
            ops.push(format!(
                "{}_{}({span_kind:?}, pattern={}) -> ParserState::scan_span",
                flavor.prefix(),
                flavor.span_op(),
                pattern
            ));
        }
        BackendExpr::CallRule { callee } => {
            ops.push(format!("{}_call_rule({callee})", flavor.prefix()));
        }
        BackendExpr::SpanMark { kind, label } => {
            ops.push(format!(
                "{}_span_mark({kind:?}, label={label})",
                flavor.prefix()
            ));
        }
        BackendExpr::TapeEmit { kind } => {
            ops.push(format!(
                "{}_{}({kind:?}) -> ParserState::emit_plain_offset",
                flavor.prefix(),
                flavor.tape_op()
            ));
        }
        BackendExpr::DirectBuild { shape, fields } => {
            let field_names = fields
                .iter()
                .map(|field| field.name.as_str())
                .collect::<Vec<_>>()
                .join(",");
            ops.push(format!(
                "{}_direct_build(shape={shape}, fields=[{field_names}])",
                flavor.prefix()
            ));
        }
        BackendExpr::ValueProject { projection } => {
            ops.push(format!("{}_project_value({projection})", flavor.prefix()));
        }
        BackendExpr::Return => {
            ops.push(format!("{}_return_success", flavor.prefix()));
        }
    }
}

fn alt_label(mode: AltMode) -> &'static str {
    match mode {
        AltMode::Dispatch => "alt_dispatch",
        AltMode::Speculative => "alt_speculative",
    }
}

fn literal_hex(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len().saturating_mul(2));
    for byte in bytes {
        write!(&mut out, "{byte:02x}").expect("write to string");
    }
    out
}
