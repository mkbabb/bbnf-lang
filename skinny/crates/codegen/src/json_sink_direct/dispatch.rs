//! Derive + emit the value dispatch — the 91.5% hot leaf.
//!
//! `derive_arms` walks the `value` rule's `Alt{Dispatch}` branches and resolves
//! each branch (a `CallRule`) into a [`DispatchArm`] carrying its first-byte
//! keys + structural disposition. The arm SET and KEYS come from the tree, so
//! dropping a rule from the `.bbnf` makes its arms vanish (the falsifier).
//!
//! The dispatch is emitted in three sink-prefix variants ([`super::PREFIXES`])
//! — `parse_value_direct` / `parse_object_value_at_direct` /
//! `parse_array_element_at_direct` — which differ ONLY by sink prefix and the
//! leading whitespace skip. They are NOT LCD-unified (3 monomorphized fns).

use super::first_set::first_bytes;
use super::{ArmKind, DispatchArm, LiteralArm, SinkWalk, INLINE_CFG};
use crate::lower::sink_only::{SinkOnlyExpr, SinkOnlyProgram, SinkOnlyRule};
use ir::AltMode;
use std::fmt::Write;

/// One sink-prefix variant of the value dispatch.
pub(crate) struct SinkPrefix {
    /// The emitted function name.
    pub function: &'static str,
    /// The sink call prefix (`sink.` / `sink.object_` / `sink.array_`).
    pub sink: &'static str,
    /// The number callee for this variant.
    pub number_fn: &'static str,
    /// Whether this variant skips leading whitespace (only the top-level `value`).
    pub leading_ws: bool,
}

/// Derive the dispatch arms from the `value` rule's `Alt{Dispatch}` branches.
pub(crate) fn derive_arms(
    program: &SinkOnlyProgram,
    value_rule: &SinkOnlyRule,
) -> Result<Vec<DispatchArm>, String> {
    let alt = find_dispatch_alt(&value_rule.expr)
        .ok_or_else(|| "sink-only renderer: `value` rule has no dispatch Alt".to_string())?;

    let mut arms = Vec::with_capacity(alt.len());
    for branch in alt {
        let SinkOnlyExpr::CallRule { callee } = branch else {
            return Err(format!(
                "sink-only renderer: unexpected value-dispatch branch {:?}",
                branch
            ));
        };
        let callee_rule = program
            .rules
            .iter()
            .find(|rule| &rule.name == callee)
            .ok_or_else(|| format!("sink-only renderer: missing dispatch callee `{callee}`"))?;
        let first = first_bytes(program, &callee_rule.expr);
        let keys = first.bytes.iter().copied().collect::<Vec<_>>();
        let kind = arm_kind(callee, callee_rule)?;
        arms.push(DispatchArm { keys, kind });
    }
    Ok(arms)
}

/// Find the `Alt{Dispatch}` inside a (possibly `Seq`-wrapped) value expr.
fn find_dispatch_alt(expr: &SinkOnlyExpr) -> Option<&[SinkOnlyExpr]> {
    match expr {
        SinkOnlyExpr::Alt {
            mode: AltMode::Dispatch,
            branches,
        } => Some(branches),
        SinkOnlyExpr::Seq(children) => children.iter().find_map(find_dispatch_alt),
        SinkOnlyExpr::Entry(inner) | SinkOnlyExpr::OptionalBranch(inner) => find_dispatch_alt(inner),
        _ => None,
    }
}

/// Classify a dispatch arm by walking its callee rule's tree.
fn arm_kind(callee: &str, callee_rule: &SinkOnlyRule) -> Result<ArmKind, String> {
    // Container / sequence rules carry both open+close ByteLiterals.
    if rule_has_byte(&callee_rule.expr, b'{') && rule_has_byte(&callee_rule.expr, b'}') {
        return Ok(ArmKind::Container {
            parse_fn: format!("parse_{callee}_direct"),
        });
    }
    if rule_has_byte(&callee_rule.expr, b'[') && rule_has_byte(&callee_rule.expr, b']') {
        return Ok(ArmKind::Sequence {
            parse_fn: format!("parse_{callee}_direct"),
        });
    }
    // String / number rules carry a leaf RegexProgram (span_kind selects the scanner).
    match span_kind(&callee_rule.expr) {
        Some(crate::lower::sink_only::SinkOnlySpanKind::String) => return Ok(ArmKind::String),
        Some(crate::lower::sink_only::SinkOnlySpanKind::Number) => return Ok(ArmKind::Number),
        _ => {}
    }
    // Literal-valued rules (bool / null): one literal arm per leading byte.
    let literals = literal_arms(callee_rule)?;
    if literals.is_empty() {
        return Err(format!(
            "sink-only renderer: dispatch callee `{callee}` has no recognizable shape"
        ));
    }
    Ok(ArmKind::Literal { literals })
}

/// Derive the per-byte literal arms for a bool/null rule from its ByteLiterals.
fn literal_arms(rule: &SinkOnlyRule) -> Result<Vec<LiteralArm>, String> {
    let mut literals = Vec::new();
    collect_literals(&rule.expr, &mut literals);
    literals
        .into_iter()
        .map(|bytes| literal_arm(&bytes))
        .collect::<Result<Vec<_>, _>>()
}

/// Map a literal byte-string to its (config const, sink verb) — derived from
/// the literal value, so the `.bbnf` literal drives the emitted arm.
fn literal_arm(bytes: &[u8]) -> Result<LiteralArm, String> {
    let key = bytes
        .first()
        .copied()
        .ok_or_else(|| "sink-only renderer: empty literal arm".to_string())?;
    let (literal_const, verb) = match bytes {
        b"true" => ("TRUE_LITERAL", "bool(true)"),
        b"false" => ("FALSE_LITERAL", "bool(false)"),
        b"null" => ("NULL_LITERAL", "null()"),
        other => {
            return Err(format!(
                "sink-only renderer: unsupported value literal {:?}",
                String::from_utf8_lossy(other)
            ))
        }
    };
    Ok(LiteralArm {
        key,
        literal_const: literal_const.to_string(),
        verb: verb.to_string(),
    })
}

fn collect_literals(expr: &SinkOnlyExpr, out: &mut Vec<Vec<u8>>) {
    match expr {
        SinkOnlyExpr::ByteLiteral(bytes) => out.push(bytes.clone()),
        SinkOnlyExpr::Seq(children) => children.iter().for_each(|c| collect_literals(c, out)),
        SinkOnlyExpr::Alt { branches, .. } => {
            branches.iter().for_each(|b| collect_literals(b, out))
        }
        SinkOnlyExpr::Entry(inner)
        | SinkOnlyExpr::OptionalBranch(inner)
        | SinkOnlyExpr::RepeatLoop { body: inner, .. } => collect_literals(inner, out),
        _ => {}
    }
}

fn rule_has_byte(expr: &SinkOnlyExpr, byte: u8) -> bool {
    match expr {
        SinkOnlyExpr::ByteLiteral(bytes) => bytes.first() == Some(&byte),
        SinkOnlyExpr::Seq(children) => children.iter().any(|c| rule_has_byte(c, byte)),
        SinkOnlyExpr::Alt { branches, .. } => branches.iter().any(|b| rule_has_byte(b, byte)),
        SinkOnlyExpr::Entry(inner)
        | SinkOnlyExpr::OptionalBranch(inner)
        | SinkOnlyExpr::RepeatLoop { body: inner, .. } => rule_has_byte(inner, byte),
        _ => false,
    }
}

fn span_kind(expr: &SinkOnlyExpr) -> Option<crate::lower::sink_only::SinkOnlySpanKind> {
    match expr {
        SinkOnlyExpr::RegexProgram { span_kind, .. } => Some(*span_kind),
        SinkOnlyExpr::Seq(children) => children.iter().find_map(span_kind),
        SinkOnlyExpr::Alt { branches, .. } => branches.iter().find_map(span_kind),
        SinkOnlyExpr::Entry(inner)
        | SinkOnlyExpr::OptionalBranch(inner)
        | SinkOnlyExpr::RepeatLoop { body: inner, .. } => span_kind(inner),
        _ => None,
    }
}

impl SinkWalk<'_> {
    /// Emit the 3 sink-prefix value-dispatch variants (the hot leaf).
    pub(crate) fn render_value_dispatch(&self, out: &mut String) {
        for (index, prefix) in super::PREFIXES.iter().enumerate() {
            self.render_dispatch_fn(out, prefix);
            if index + 1 < super::PREFIXES.len() {
                out.push('\n');
            }
        }
        out.push('\n');
    }

    fn render_dispatch_fn(&self, out: &mut String, prefix: &SinkPrefix) {
        out.push_str(INLINE_CFG);
        let _ = writeln!(
            out,
            "fn {}<'i, S: JsonSink>(",
            prefix.function
        );
        out.push_str(
            "    input: &'i str,\n    bytes: &'i [u8],\n    cursor: &mut usize,\n    sink: &mut S,\n) -> Result<(), ParseError<'i>> {\n",
        );
        if prefix.leading_ws {
            out.push_str("    *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        }
        out.push_str(
            "    let Some(byte) = bytes.get(*cursor).copied() else {\n        return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));\n    };\n    match byte {\n",
        );
        for arm in &self.arms {
            self.render_arm(out, arm, prefix);
        }
        out.push_str(
            "        _ => Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue)),\n    }\n}\n",
        );
    }

    fn render_arm(&self, out: &mut String, arm: &DispatchArm, prefix: &SinkPrefix) {
        match &arm.kind {
            ArmKind::Container { parse_fn } => {
                let key = byte_key(arm.keys[0]);
                let _ = writeln!(
                    out,
                    "        {key} => {parse_fn}(input, bytes, cursor, sink),"
                );
            }
            ArmKind::Sequence { parse_fn } => {
                let key = byte_key(arm.keys[0]);
                let _ = writeln!(
                    out,
                    "        {key} => {parse_fn}(input, bytes, cursor, sink),"
                );
            }
            ArmKind::String => {
                let key = byte_key(b'"');
                let _ = writeln!(out, "        {key} => {{");
                out.push_str("            let value = parse_string_direct(input, bytes, cursor)?;\n");
                let _ = writeln!(
                    out,
                    "            {sink}string_source(value.raw, value.needs_unescape)",
                    sink = prefix.sink
                );
                out.push_str("                .map_err(|err| string_error(input, err))?;\n");
                out.push_str("            Ok(())\n        }\n");
            }
            ArmKind::Number => {
                let key = number_arm_key(&arm.keys);
                let _ = writeln!(
                    out,
                    "        {key} => {number_fn}(input, bytes, cursor, sink, byte),",
                    number_fn = prefix.number_fn
                );
            }
            ArmKind::Literal { literals } => {
                for literal in literals {
                    let key = byte_key(literal.key);
                    let _ = writeln!(out, "        {key} => {{");
                    let _ = writeln!(
                        out,
                        "            consume_literal_direct(input, bytes, cursor, config::{})?;",
                        literal.literal_const
                    );
                    let _ = writeln!(out, "            {sink}{verb};", sink = prefix.sink, verb = literal.verb);
                    out.push_str("            Ok(())\n        }\n");
                }
            }
        }
    }
}

/// Render a single byte as a Rust byte literal (`b'{'`).
fn byte_key(byte: u8) -> String {
    super::byte_literal(byte)
}

/// Render the number dispatch key from its derived first-byte set
/// (`b'-' | b'0'..=b'9'`), tracking the grammar's leading number class.
pub(crate) fn number_arm_key(keys: &[u8]) -> String {
    let has_sign = keys.contains(&b'-');
    number_range(keys, has_sign)
}

/// Render the number `matches!(byte, ...)` pattern for the array fast-path —
/// the SAME derived first-byte set as the dispatch arm.
pub(crate) fn number_match_pattern(keys: &[u8]) -> String {
    number_arm_key(keys)
}

fn number_range(keys: &[u8], has_sign: bool) -> String {
    let digits = keys
        .iter()
        .copied()
        .filter(|b| b.is_ascii_digit())
        .collect::<Vec<_>>();
    let lo = digits.iter().copied().min().unwrap_or(b'0');
    let hi = digits.iter().copied().max().unwrap_or(b'9');
    let range = format!("{}..={}", super::byte_literal(lo), super::byte_literal(hi));
    if has_sign {
        format!("b'-' | {range}")
    } else {
        range
    }
}

/// Collect the ordered structural ByteLiterals of a container rule
/// (the open + close delimiters), in tree order.
pub(crate) fn collect_container_literals(expr: &SinkOnlyExpr, out: &mut Vec<u8>) {
    match expr {
        SinkOnlyExpr::ByteLiteral(bytes) => {
            if let Some(byte) = bytes.first().copied() {
                out.push(byte);
            }
        }
        SinkOnlyExpr::Seq(children) => children
            .iter()
            .for_each(|c| collect_container_literals(c, out)),
        SinkOnlyExpr::Entry(inner)
        | SinkOnlyExpr::OptionalBranch(inner)
        | SinkOnlyExpr::RepeatLoop { body: inner, .. } => collect_container_literals(inner, out),
        SinkOnlyExpr::Alt { branches, .. } => branches
            .iter()
            .for_each(|b| collect_container_literals(b, out)),
        _ => {}
    }
}
