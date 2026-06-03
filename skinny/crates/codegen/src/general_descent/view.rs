//! The G4 lazy view emitter: `{Prefix}Document` + `{Prefix}Node` cursor +
//! `{Prefix}Kind`.
//!
//! Models the CSS `CssDocument`/`CssNode` lazy tape-backed view: the document
//! wraps the single sealed `Tape`; each node is an 8-byte cursor whose kind is
//! decoded ON DEMAND from the sparse flag plane (never stored eagerly). The
//! `{Prefix}Kind` enum is DERIVED from the program's rule names; the type prefix
//! is DERIVED from the grammar name (`grammar_pascal`) — never a hard-coded
//! token. Navigation (the flat node sweep) lives OUTSIDE the shared
//! `Cursor`/`DocumentView` seam.

use super::rule_pascal;
use crate::lower::sink_only::{SinkOnlyExpr, SinkOnlyProgram};
use std::fmt::Write;

/// The value category a grammar `->` projection resolves to, derived purely from
/// the projection's `target`/`ty` text — never from rule names. The SET of typed
/// accessors the view emits is the set of categories present in the grammar.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum ProjectionKind {
    /// A floating-point projection (`-> f64`, `-> f32`): `as_f64()`.
    Float,
    /// A boolean-literal projection (`-> true` / `-> false`): `as_bool()`.
    Bool,
    /// A `u8` tag projection (`-> 0u8` .. `-> 8u8`): `as_tag()`.
    Tag,
    /// A span projection (`-> input : Span`, or any non-value target): `span()`.
    Span,
}

/// Classify a projection's `target` (text after `->`) + optional `ty` (`: <ty>`)
/// into a value category. Derived entirely from the projection text — a float
/// type name yields `Float`, a boolean literal yields `Bool`, an `Nu8` literal
/// yields `Tag`; everything else (including `input : Span`) stays `Span`.
fn classify_projection(target: &str, ty: Option<&str>) -> ProjectionKind {
    if matches!(ty, Some("Span")) {
        return ProjectionKind::Span;
    }
    match target {
        "f64" | "f32" => ProjectionKind::Float,
        "true" | "false" => ProjectionKind::Bool,
        other if other.ends_with("u8") && other[..other.len() - 2].chars().all(|c| c.is_ascii_digit()) && other.len() > 2 => {
            ProjectionKind::Tag
        }
        _ => ProjectionKind::Span,
    }
}

/// Collect the projection categories reachable in a rule body — a rule may carry
/// several (`boolean` projects both `true` and `false`; `error_literal` projects
/// `0u8`..`8u8`). Walks transparently through every structural node.
fn collect_projection_kinds(expr: &SinkOnlyExpr, out: &mut Vec<ProjectionKind>) {
    match expr {
        SinkOnlyExpr::ValueProject { body, target, ty } => {
            let kind = classify_projection(target, ty.as_deref());
            if !out.contains(&kind) {
                out.push(kind);
            }
            collect_projection_kinds(body, out);
        }
        SinkOnlyExpr::Entry(inner)
        | SinkOnlyExpr::OptionalBranch(inner)
        | SinkOnlyExpr::RepeatLoop { body: inner, .. } => collect_projection_kinds(inner, out),
        SinkOnlyExpr::Seq(children) => {
            for child in children {
                collect_projection_kinds(child, out);
            }
        }
        SinkOnlyExpr::Alt { branches, .. } => {
            for branch in branches {
                collect_projection_kinds(branch, out);
            }
        }
        SinkOnlyExpr::ByteLiteral(_)
        | SinkOnlyExpr::RegexProgram { .. }
        | SinkOnlyExpr::CallRule { .. }
        | SinkOnlyExpr::SpanMark { .. }
        | SinkOnlyExpr::TapeEmit
        | SinkOnlyExpr::DirectBuild(_)
        | SinkOnlyExpr::Return => {}
    }
}

/// The set of `{Prefix}Kind` variants whose rule projects to `kind` — the
/// dispatch guard for the typed accessor. Derived from each rule's body
/// projections, NOT from rule-name strings.
fn rules_for_kind(program: &SinkOnlyProgram, kind: ProjectionKind) -> Vec<String> {
    program
        .rules
        .iter()
        .filter(|rule| {
            let mut kinds = Vec::new();
            collect_projection_kinds(&rule.expr, &mut kinds);
            kinds.contains(&kind)
        })
        .map(|rule| rule_pascal(&rule.name))
        .collect()
}

/// The 1-based tape kind tag of a rule (its declaration index + 1). Zero is
/// reserved for "no recorded node" (an unflagged tape offset), so leaf/structural
/// markers carry a non-zero tag the view can decode back to a `{Prefix}Kind`.
pub(super) fn rule_kind_tag(program: &SinkOnlyProgram, name: &str) -> u8 {
    let index = program
        .rules
        .iter()
        .position(|rule| rule.name == name)
        .unwrap_or(0);
    (index as u8).wrapping_add(1)
}

/// Emit the typed value accessors for `{prefix}Node`, DERIVED from the grammar's
/// `->` projections. For each value category present in the grammar (float / bool
/// / u8-tag), emit one accessor that returns `Some` ONLY when the node's decoded
/// `{Prefix}Kind` belongs to a rule projecting to that category — so `as_f64` on a
/// string node yields `None`. The accessor SET and its dispatch guards are
/// derived from the projections, never from hard-coded rule names.
fn build_typed_accessors(program: &SinkOnlyProgram, prefix: &str) -> String {
    let mut out = String::new();

    let emit_guard = |out: &mut String, variants: &[String]| {
        if variants.is_empty() {
            return;
        }
        let arms = variants
            .iter()
            .map(|variant| format!("Some({prefix}Kind::{variant})"))
            .collect::<Vec<_>>()
            .join(" | ");
        let _ = writeln!(out, "        if !matches!(self.rule_kind(), {arms}) {{");
        let _ = writeln!(out, "            return None;");
        let _ = writeln!(out, "        }}");
    };

    // Float projections (`-> f64`): parse the node span as an f64.
    let float_rules = rules_for_kind(program, ProjectionKind::Float);
    if !float_rules.is_empty() {
        out.push_str("\n    /// The node's span parsed as `f64`, for nodes whose rule carries a\n");
        out.push_str("    /// floating-point `->` projection. `None` for any other kind.\n");
        out.push_str("    #[inline]\n");
        out.push_str("    pub fn as_f64(&self) -> Option<f64> {\n");
        emit_guard(&mut out, &float_rules);
        out.push_str("        std::str::from_utf8(self.span()).ok()?.trim().parse::<f64>().ok()\n");
        out.push_str("    }\n");
    }

    // Boolean-literal projections (`-> true` / `-> false`): decode the span.
    let bool_rules = rules_for_kind(program, ProjectionKind::Bool);
    if !bool_rules.is_empty() {
        out.push_str("\n    /// The node's span decoded as a `bool`, for nodes whose rule carries a\n");
        out.push_str("    /// boolean-literal `->` projection. `None` for any other kind.\n");
        out.push_str("    #[inline]\n");
        out.push_str("    pub fn as_bool(&self) -> Option<bool> {\n");
        emit_guard(&mut out, &bool_rules);
        out.push_str("        let text = std::str::from_utf8(self.span()).ok()?.trim();\n");
        out.push_str("        if text.eq_ignore_ascii_case(\"true\") {\n");
        out.push_str("            Some(true)\n");
        out.push_str("        } else if text.eq_ignore_ascii_case(\"false\") {\n");
        out.push_str("            Some(false)\n");
        out.push_str("        } else {\n");
        out.push_str("            None\n");
        out.push_str("        }\n");
        out.push_str("    }\n");
    }

    // u8-tag projections (`-> 0u8` .. `-> Nu8`): recover the matched variant's tag
    // by its position in the alternation, derived from the rule's projection text.
    let tag_rules = rules_for_kind(program, ProjectionKind::Tag);
    if !tag_rules.is_empty() {
        out.push_str("\n    /// The node's `u8` tag, for nodes whose rule carries an `Nu8` `->`\n");
        out.push_str("    /// projection — the tag of the branch matched by this node's span.\n");
        out.push_str("    /// `None` for any other kind.\n");
        out.push_str("    #[inline]\n");
        out.push_str("    pub fn as_tag(&self) -> Option<u8> {\n");
        emit_guard(&mut out, &tag_rules);
        out.push_str("        TAG_TABLE\n");
        out.push_str("            .iter()\n");
        out.push_str("            .find(|(literal, _)| self.span() == literal.as_bytes())\n");
        out.push_str("            .map(|(_, tag)| *tag)\n");
        out.push_str("    }\n");
    }

    out
}

/// The `(literal, tag)` table backing `as_tag`: every `-> Nu8` projection's
/// matched byte-literal paired with its declared `u8` tag, gathered from the
/// program's `ByteLiteral`-bodied `ValueProject` branches. Empty when the grammar
/// has no tag projections — emitted only alongside `as_tag`.
fn build_tag_table(program: &SinkOnlyProgram) -> Vec<(String, u8)> {
    fn walk(expr: &SinkOnlyExpr, literal: &mut Option<String>, out: &mut Vec<(String, u8)>) {
        match expr {
            SinkOnlyExpr::ValueProject { body, target, ty } => {
                if matches!(classify_projection(target, ty.as_deref()), ProjectionKind::Tag) {
                    // Recover the branch's matched literal from the projected body.
                    let mut lit = None;
                    leaf_literal(body, &mut lit);
                    if let (Some(lit), Some(tag)) = (lit, target.strip_suffix("u8").and_then(|n| n.parse::<u8>().ok())) {
                        if !out.iter().any(|(existing, _)| existing == &lit) {
                            out.push((lit, tag));
                        }
                    }
                }
                walk(body, literal, out);
            }
            SinkOnlyExpr::Entry(inner)
            | SinkOnlyExpr::OptionalBranch(inner)
            | SinkOnlyExpr::RepeatLoop { body: inner, .. } => walk(inner, literal, out),
            SinkOnlyExpr::Seq(children) => {
                for child in children {
                    walk(child, literal, out);
                }
            }
            SinkOnlyExpr::Alt { branches, .. } => {
                for branch in branches {
                    walk(branch, literal, out);
                }
            }
            _ => {}
        }
    }
    // The matched byte-literal of a projected branch body (`"#N/A"` etc.).
    fn leaf_literal(expr: &SinkOnlyExpr, out: &mut Option<String>) {
        match expr {
            SinkOnlyExpr::ByteLiteral(bytes) => {
                if out.is_none() {
                    if let Ok(text) = std::str::from_utf8(bytes) {
                        *out = Some(text.to_string());
                    }
                }
            }
            SinkOnlyExpr::Entry(inner)
            | SinkOnlyExpr::OptionalBranch(inner)
            | SinkOnlyExpr::RepeatLoop { body: inner, .. }
            | SinkOnlyExpr::ValueProject { body: inner, .. } => leaf_literal(inner, out),
            SinkOnlyExpr::Seq(children) => {
                for child in children {
                    leaf_literal(child, out);
                }
            }
            _ => {}
        }
    }
    let mut table = Vec::new();
    for rule in &program.rules {
        let mut literal = None;
        walk(&rule.expr, &mut literal, &mut table);
    }
    table
}

/// Render the `view.rs` file: the lazy tape-backed projection. The view types
/// carry the grammar-derived `prefix` (`{prefix}Document`/`{prefix}Kind`/
/// `{prefix}Node`).
pub(crate) fn render_view(program: &SinkOnlyProgram, prefix: &str) -> String {
    let mut out = String::new();
    out.push_str(crate::GENERATED_HEADER);
    out.push('\n');
    out.push_str(
        "use crate::tape::{Cursor, DocumentView, Tape, TapeId};\n\n",
    );

    // The {prefix}Kind enum: one variant per rule, derived from the rule names.
    let _ = write!(
        out,
        "/// The structural kind a tape cursor projects to, recovered lazily from the\n\
         /// sparse flag plane at the cursor's offset. One variant per grammar rule —\n\
         /// derived from the program's rule names, no shared structural vocabulary.\n\
         #[derive(Copy, Clone, Eq, PartialEq, Debug)]\npub enum {prefix}Kind {{\n",
    );
    for rule in &program.rules {
        let _ = writeln!(out, "    {},", rule_pascal(&rule.name));
    }
    out.push_str("}\n\n");

    // The tag table: index -> variant, for the lazy decode.
    out.push_str(
        "/// The 1-based tag table: a recorded tape flag value decodes to its rule\n\
         /// kind by this table (`tag - 1` indexes the rule order).\n",
    );
    let _ = write!(out, "const KIND_TABLE: &[{prefix}Kind] = &[\n");
    for rule in &program.rules {
        let _ = writeln!(out, "    {prefix}Kind::{},", rule_pascal(&rule.name));
    }
    out.push_str("];\n\n");

    // The grammar-derived typed value accessors + their backing tag table. Both
    // are DERIVED from the `->` projections present in the program; absent when
    // the grammar carries no value projections.
    let typed_accessors = build_typed_accessors(program, prefix);
    let tag_table = build_tag_table(program);
    if !tag_table.is_empty() {
        out.push_str(
            "/// The `(literal, tag)` table backing `as_tag`: each `-> Nu8` projection's\n\
             /// matched byte-literal paired with its declared `u8` tag.\n",
        );
        out.push_str("const TAG_TABLE: &[(&str, u8)] = &[\n");
        for (literal, tag) in &tag_table {
            let _ = writeln!(out, "    ({literal:?}, {tag}u8),");
        }
        out.push_str("];\n\n");
    }

    let _ = write!(
        out,
        r#"impl {prefix}Kind {{
    /// Decode the kind at a tape cursor from the sparse flag plane. A cursor with
    /// no flag (tag 0) — a bare structural offset — has no rule kind.
    #[inline]
    pub fn at_cursor(tape: &Tape<'_>, cursor: u32) -> Option<{prefix}Kind> {{
        let tag = tape.flags_at(cursor)?.bits();
        if tag == 0 {{
            return None;
        }}
        KIND_TABLE.get((tag - 1) as usize).copied()
    }}
}}

/// A lazy view over one recorded node on the tape — 8 bytes of cursor; the kind
/// is decoded on demand from the flag plane, never stored.
#[derive(Copy, Clone)]
pub struct {prefix}Node<'doc, 'input> {{
    tape: &'doc Tape<'input>,
    cursor: u32,
}}

impl<'doc, 'input> {prefix}Node<'doc, 'input> {{
    #[inline]
    pub fn new(tape: &'doc Tape<'input>, cursor: u32) -> Self {{
        {prefix}Node {{ tape, cursor }}
    }}

    /// The decoded rule kind of this node (`None` for an unflagged offset).
    #[inline]
    pub fn rule_kind(&self) -> Option<{prefix}Kind> {{
        {prefix}Kind::at_cursor(self.tape, self.cursor)
    }}

    /// The source span of this node from its offset to the next recorded node's
    /// offset (or end of input) — the leaf/structural text, lazily recovered.
    #[inline]
    pub fn span(&self) -> &'input [u8] {{
        let source = self.tape.source();
        let start = self.offset();
        let end = self
            .tape
            .offset_at(self.cursor + 1)
            .unwrap_or(source.len());
        &source[start..end.min(source.len())]
    }}
{typed_accessors}}}

/// The shared lazy-cursor seam over the single tape — the `(tape, cursor, offset,
/// kind)` primitive. The flat node sweep lives ENTIRELY outside this trait.
impl<'doc, 'input> Cursor<'doc, 'input> for {prefix}Node<'doc, 'input> {{
    type Kind = Option<{prefix}Kind>;

    #[inline]
    fn tape(&self) -> &'doc Tape<'input> {{
        self.tape
    }}

    #[inline]
    fn cursor(&self) -> u32 {{
        self.cursor
    }}

    #[inline]
    fn offset(&self) -> usize {{
        self.tape
            .offset_at(self.cursor)
            .expect("{prefix}Node cursor must point into the sealed offset tape")
    }}

    #[inline]
    fn kind(&self) -> Option<{prefix}Kind> {{
        {prefix}Kind::at_cursor(self.tape, self.cursor)
    }}
}}

/// The sealed tape-backed document: the retained product of a parse. Holds
/// exactly the existing `Tape` — no second substrate.
pub struct {prefix}Document<'input> {{
    tape: Tape<'input>,
}}

impl<'input> {prefix}Document<'input> {{
    #[inline]
    pub fn from_tape(tape: Tape<'input>) -> Self {{
        {prefix}Document {{ tape }}
    }}

    #[inline]
    pub fn tape(&self) -> &Tape<'input> {{
        &self.tape
    }}

    /// Iterate the recorded structural nodes as lazy cursor views.
    pub fn nodes(&self) -> impl Iterator<Item = {prefix}Node<'_, 'input>> + '_ {{
        (0..self.tape.offsets().len() as u32)
            .map(move |cursor| {prefix}Node::new(&self.tape, cursor))
    }}

    /// The count of recorded nodes carrying a decodable rule kind.
    pub fn node_count(&self) -> usize {{
        self.nodes().filter(|node| node.rule_kind().is_some()).count()
    }}
}}

/// The document-level seam shared with every grammar's handle: root cursor +
/// tape-id + source bytes over the single tape. Carries NO navigation.
impl<'input> DocumentView<'input> for {prefix}Document<'input> {{
    type Root = {prefix}Node<'input, 'input>;

    fn root_value(&'input self) -> Self::Root {{
        {prefix}Node::new(&self.tape, 0)
    }}

    fn tape_id(&self) -> TapeId {{
        self.tape.id()
    }}

    fn source(&'input self) -> &'input [u8] {{
        self.tape.source()
    }}
}}
"#,
    );

    out
}
