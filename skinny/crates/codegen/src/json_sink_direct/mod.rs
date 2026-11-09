//! Recursive emitter over the `SinkOnlyExpr` tree.
//!
//! This module walks the `SinkOnlyProgram` produced by `lower::sink_only` and
//! derives the JSON direct-to-sink runtime FROM the grammar tree + the `.bbnf`
//! rules — it is NOT a fixed-literal stringifier. The structural skeleton
//! (the value dispatch arm set + keys, the container loops, the literal arms,
//! and the value/object/array sink-prefix variants) is computed by walking the
//! tree; only the per-leaf scanner kernels stay as named external callees
//! (string: `match_tiny_plain_string_direct` + `match_string_at_quote_trusted_utf8`;
//! number: `match_number_span_from_first` + `materialize_*`) selected from the
//! rule's `RegexProgram` span_kind.
//!
//! The walk emits BOTH emit-policies of the same grammar:
//! - [`EmitPolicy::Sink`] — recursive descent with monomorphized `sink.*` calls
//!   (the 91.5% hot leaf), in three sink-prefix variants (`""`/`object_`/`array_`).
//! - [`EmitPolicy::Recognize`] — the same recursive-descent walk with the sink
//!   calls suppressed (the `parse_only` recognizer).

use crate::lower::sink_only::{SinkOnlyProgram, SinkOnlyRule, SinkOnlySpanKind};
use std::fmt::Write;

mod dispatch;
mod first_set;
mod recognize;
mod skeleton;

use dispatch::SinkPrefix;

/// Render the JSON direct-to-sink runtime + recognizer for `program`.
pub fn render(program: &SinkOnlyProgram) -> Result<String, String> {
    validate(program)?;
    let walk = SinkWalk::new(program)?;

    let mut out = String::new();
    walk.render_parse_only(&mut out);
    walk.render_header(&mut out);
    walk.render_entry(&mut out);
    walk.render_value_dispatch(&mut out);
    walk.render_container_rules(&mut out);
    walk.render_string_rule(&mut out);
    walk.render_number_rules(&mut out);
    walk.render_utility_rules(&mut out);
    Ok(out)
}

/// The semantic role a `value`-Alt branch resolves to (derived from the
/// callee rule's tree: its first-byte set + span_kind + literal shape).
///
/// Both emit-policies of the walk consume this: the [`Sink`](super) policy
/// (recursive descent + `sink.*` calls — the direct path) and the recognize
/// policy (the same shape, sink calls suppressed — `parse_only`).
#[derive(Clone)]
pub(crate) struct DispatchArm {
    /// The first-byte set keying this arm, in grammar order (e.g. `[b'-', b'0'..=b'9']`).
    pub keys: Vec<u8>,
    /// The structural disposition of this arm, derived from the callee rule.
    pub kind: ArmKind,
}

/// What the dispatch arm body emits, derived from the callee rule's tree.
#[derive(Clone)]
pub(crate) enum ArmKind {
    /// `object` rule: recurse into the `{`-container parser.
    Container { parse_fn: String },
    /// `array` rule: recurse into the `[`-sequence parser.
    Sequence { parse_fn: String },
    /// `string` rule (RegexProgram{String}): scan + `sink.{prefix}string_source`.
    String,
    /// `number` rule (RegexProgram{Number}): scan + `parse_number_{prefix}direct`.
    Number,
    /// `bool`/`null` rule: a literal arm per branch — `consume_literal_direct` + `sink.{prefix}<verb>`.
    Literal { literals: Vec<LiteralArm> },
}

/// One literal value branch (e.g. `true` -> `sink.bool(true)`).
#[derive(Clone)]
pub(crate) struct LiteralArm {
    /// The first byte keying this literal (e.g. `b't'`).
    pub key: u8,
    /// The config literal constant name (`TRUE_LITERAL`/`FALSE_LITERAL`/`NULL_LITERAL`).
    pub literal_const: String,
    /// The sink verb suffix (`bool(true)`, `bool(false)`, `null()`).
    pub verb: String,
}

/// The walk state: the program + a by-name rule index + the derived dispatch.
pub(crate) struct SinkWalk<'p> {
    program: &'p SinkOnlyProgram,
    /// The `value` rule's dispatch arms, in grammar order, derived from its Alt branches.
    arms: Vec<DispatchArm>,
}

impl<'p> SinkWalk<'p> {
    fn new(program: &'p SinkOnlyProgram) -> Result<Self, String> {
        let value_rule = program
            .rules
            .iter()
            .find(|rule| rule.name == "value")
            .ok_or_else(|| "sink-only renderer missing `value` rule".to_string())?;
        let arms = dispatch::derive_arms(program, value_rule)?;
        Ok(Self { program, arms })
    }

    pub(crate) fn rule(&self, name: &str) -> Option<&'p SinkOnlyRule> {
        self.program.rules.iter().find(|rule| rule.name == name)
    }

    pub(crate) fn arms(&self) -> &[DispatchArm] {
        &self.arms
    }
}

/// Render a structural byte as a Rust byte literal token (`b'{'`), escaping
/// only what a Rust char literal requires.
pub(crate) fn byte_literal(byte: u8) -> String {
    let inner = match byte {
        b'\\' => "\\\\".to_string(),
        b'\'' => "\\'".to_string(),
        0x20..=0x7e => (byte as char).to_string(),
        other => format!("\\x{other:02x}"),
    };
    format!("b'{inner}'")
}

fn validate(program: &SinkOnlyProgram) -> Result<(), String> {
    if !program.has_rule(&program.entry_rule) {
        return Err(format!(
            "sink-only renderer missing entry BIR rule `{}`",
            program.entry_rule
        ));
    }

    if program.direct_shapes.is_empty() {
        return Err("sink-only renderer requires DirectBuild shapes".to_string());
    }

    if program.literals.is_empty() {
        return Err("sink-only renderer requires literal recognizers".to_string());
    }

    for span in [
        SinkOnlySpanKind::String,
        SinkOnlySpanKind::Number,
        SinkOnlySpanKind::Whitespace,
    ] {
        if !program.span_kinds.contains(&span) {
            return Err(format!(
                "sink-only renderer missing {:?} RegexProgram in BIR",
                span
            ));
        }
    }

    for shape in program
        .rules
        .iter()
        .filter_map(|rule| rule.direct_shape.as_ref())
    {
        if shape.shape.is_empty() {
            return Err("sink-only renderer found empty DirectBuild shape".to_string());
        }
        for field in &shape.fields {
            if field.name.is_empty() {
                return Err(format!(
                    "sink-only renderer found empty DirectBuild field in `{}`",
                    shape.shape
                ));
            }
        }
    }

    Ok(())
}

impl SinkWalk<'_> {
    fn render_header(&self, out: &mut String) {
        let shapes = self
            .program
            .direct_shapes
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>()
            .join(",");
        let _ = writeln!(
            out,
            "// sink-only lowered from BackendIr: entry={} direct_shapes={} dispatch_alt_count={}",
            self.program.entry_rule, shapes, self.program.dispatch_alt_count
        );
        out.push_str(
            r#"
use super::sink::JsonSink;
use parse_that_regex::number::{
    materialize_f64, materialize_i64, materialize_u64,
};

struct ParsedString<'i> {
    raw: &'i str,
    needs_unescape: bool,
}

"#,
        );
    }
}

/// The `#[cfg_attr]` inline-policy pair stamped on every emitted parser fn —
/// the hot-leaf gate requires these byte-identical, so this is a fixed convention.
pub(crate) const INLINE_CFG: &str = "#[cfg_attr(feature = \"parse-attribution\", inline(never))]\n#[cfg_attr(not(feature = \"parse-attribution\"), inline(always))]\n";

/// The ordered sink-prefix variants of the value dispatch (do NOT LCD-unify).
pub(crate) const PREFIXES: [SinkPrefix; 3] = [
    SinkPrefix {
        function: "parse_value_direct",
        sink: "sink.",
        number_fn: "parse_number_direct",
        leading_ws: true,
    },
    SinkPrefix {
        function: "parse_object_value_at_direct",
        sink: "sink.object_",
        number_fn: "parse_number_object_direct",
        leading_ws: false,
    },
    SinkPrefix {
        function: "parse_array_element_at_direct",
        sink: "sink.array_",
        number_fn: "parse_number_array_direct",
        leading_ws: false,
    },
];
