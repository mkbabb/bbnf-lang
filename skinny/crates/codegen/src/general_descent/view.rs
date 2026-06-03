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
use crate::lower::sink_only::SinkOnlyProgram;
use std::fmt::Write;

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
}}

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
