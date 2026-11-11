//! The lazy rich CSS projection — emitted as structural+behavioral templates
//! (G2 AMENDMENT: held to 9-field cssparser oracle parity, NOT byte-equivalence
//! vs the retired `CssFullParser`). The byte-classes that ARE grammar-derived
//! (the `left_boundary` structural delimiters, the value-class heads) are keyed
//! off the [`ScanAlphabet`]; the rest are the lazy-decode shape isomorphic to
//! JSON's `value_from_ref`.

use crate::css_scan_direct::byte_literal;
use crate::lower::css_scan::ScanAlphabet;
use std::fmt::Write;

/// The structural-kind enum + `at_cursor` decode (lazy kind from source byte +
/// the at-rule flag). The `:` discriminant is the grammar-derived colon byte.
pub(super) fn render_node_kind(out: &mut String) {
    out.push_str(
        r#"/// The structural kind a tape cursor projects to, recovered lazily from the
/// source byte at the cursor's offset plus the sparse at-rule flag. There is no
/// stored tag: the CSS analogue of `JsonNodeKind::at_cursor`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CssNodeKind {
    QualifiedRule,
    AtRule,
    Declaration,
}

impl CssNodeKind {
    #[inline]
    pub fn at_cursor(tape: &Tape<'_>, cursor: u32) -> Self {
        let offset = tape
            .offset_at(cursor)
            .expect("CSS cursor must point into the sealed offset tape");
        if tape.source()[offset] == COLON {
            CssNodeKind::Declaration
        } else if tape
            .flags_at(cursor)
            .is_some_and(|flags| flags.contains(AT_RULE_FLAG))
        {
            CssNodeKind::AtRule
        } else {
            CssNodeKind::QualifiedRule
        }
    }
}

"#,
    );
}

/// The `CssNode` lazy cursor view + `value()` branch projection + the
/// `left_boundary` backward scan (structural delimiters grammar-derived).
pub(super) fn render_node_view(out: &mut String) {
    out.push_str(
        r#"/// A lazy view over one structural CSS node on the tape. 8 bytes of cursor;
/// the kind is decoded on demand, never stored.
#[derive(Copy, Clone)]
pub struct CssNode<'doc, 'input> {
    node: ValueRef<'doc, 'input>,
}

impl<'doc, 'input> CssNode<'doc, 'input> {
    #[inline]
    pub fn kind(&self) -> CssNodeKind {
        CssNodeKind::at_cursor(self.node.tape(), self.node.cursor())
    }

    #[inline]
    pub fn offset(&self) -> usize {
        self.node.offset()
    }

    /// The left boundary of this node's prelude/property: the byte after the
    /// nearest structural delimiter (`{`, `}`, `;`) at or before `end`, floored
    /// by the previous tape node's offset.
    #[inline]
    fn left_boundary(&self, end: usize) -> usize {
        let cursor = self.node.cursor();
        let floor = if cursor == 0 {
            0
        } else {
            self.node
                .tape()
                .offset_at(cursor - 1)
                .map_or(0, |offset| offset + 1)
        };
        let source = self.node.tape().source();
        let mut pos = end.min(source.len());
        while pos > floor {
            let byte = source[pos - 1];
            if byte == BLOCK_OPEN || byte == BLOCK_CLOSE || byte == TERMINATOR {
                break;
            }
            pos -= 1;
        }
        pos
    }

    /// The rich typed view: the `BackendRule` branch tag (kind) dispatches to
    /// the typed projection — at-rule / qualified-rule (carrying its lazy
    /// selector prelude) or declaration (carrying its lazy property + typed
    /// value). The CSS analogue of JSON's `value_from_ref`.
    #[inline]
    pub fn value(&self) -> CssTypedNode<'doc, 'input> {
        match self.kind() {
            CssNodeKind::AtRule => CssTypedNode::Rule(CssRule {
                node: *self,
                at_rule: true,
            }),
            CssNodeKind::QualifiedRule => CssTypedNode::Rule(CssRule {
                node: *self,
                at_rule: false,
            }),
            CssNodeKind::Declaration => {
                CssTypedNode::Declaration(CssDeclaration { node: *self })
            }
        }
    }
}

/// The shared lazy-cursor seam over the single tape — the `(tape, cursor,
/// offset, kind)` primitive `CssNode` is built on. The flat-sweep navigation
/// (`nodes`/`value`/`rich_summary`/`selector_count`) lives ENTIRELY outside this
/// trait and is untouched by it.
impl<'doc, 'input> Cursor<'doc, 'input> for CssNode<'doc, 'input> {
    type Kind = CssNodeKind;

    #[inline]
    fn tape(&self) -> &'doc Tape<'input> {
        self.node.tape()
    }

    #[inline]
    fn cursor(&self) -> u32 {
        self.node.cursor()
    }

    #[inline]
    fn offset(&self) -> usize {
        self.node.offset()
    }

    #[inline]
    fn kind(&self) -> CssNodeKind {
        CssNodeKind::at_cursor(self.node.tape(), self.node.cursor())
    }
}

"#,
    );
}

pub(super) fn render_typed_node(out: &mut String) {
    out.push_str(
        r#"/// The lazily-projected rich CSS node: one `BackendRule` branch per variant,
/// each a thin `ValueRef`-cursor view (8 bytes of cursor; no eager subtree).
#[derive(Copy, Clone)]
pub enum CssTypedNode<'doc, 'input> {
    Rule(CssRule<'doc, 'input>),
    Declaration(CssDeclaration<'doc, 'input>),
}

"#,
    );
}

pub(super) fn render_rule_view(out: &mut String) {
    out.push_str(
        r#"/// A lazy view over one CSS rule (at-rule or qualified). The selector prelude
/// is the source slice `(prev_node_end .. brace_offset)`, recovered on demand.
#[derive(Copy, Clone)]
pub struct CssRule<'doc, 'input> {
    node: CssNode<'doc, 'input>,
    at_rule: bool,
}

impl<'doc, 'input> CssRule<'doc, 'input> {
    #[inline]
    pub fn is_at_rule(&self) -> bool {
        self.at_rule
    }

    /// The prelude span: a qualified rule's selector list, or an at-rule's
    /// `@name <prelude>`. Lazily recovered from the tape.
    #[inline]
    pub fn prelude(&self) -> &'input [u8] {
        let source = self.node.node.tape().source();
        let end = self.node.offset();
        let start = self.node.left_boundary(end);
        trim_ascii(&source[start..end])
    }

    /// The selector-list entries of a qualified rule, split on top-level commas
    /// (the cssparser selector-count definition). At-rules yield zero selectors.
    #[inline]
    pub fn selector_count(&self) -> usize {
        if self.at_rule {
            return 0;
        }
        let prelude = self.prelude();
        if prelude.is_empty() {
            return 0;
        }
        1 + count_top_level_commas(prelude)
    }
}

"#,
    );
}

pub(super) fn render_declaration_view(out: &mut String) {
    out.push_str(
        r#"/// A lazy view over one CSS declaration. The property is the slice
/// `(prev_node_end .. colon)`, the value is `(colon+1 .. terminator)`.
#[derive(Copy, Clone)]
pub struct CssDeclaration<'doc, 'input> {
    node: CssNode<'doc, 'input>,
}

impl<'doc, 'input> CssDeclaration<'doc, 'input> {
    /// The declaration colon offset (the recorded tape position).
    #[inline]
    fn colon(&self) -> usize {
        self.node.offset()
    }

    /// The property name span `(prev_node_end .. colon)`, lazily recovered.
    #[inline]
    pub fn property(&self) -> &'input [u8] {
        let source = self.node.node.tape().source();
        let colon = self.colon();
        let start = self.node.left_boundary(colon);
        trim_ascii(&source[start..colon])
    }

    /// The value span `(colon+1 .. terminator)`, recovered by a lazy forward
    /// scan over balanced components from the colon.
    #[inline]
    pub fn value_span(&self) -> &'input [u8] {
        let source = self.node.node.tape().source();
        let colon = self.colon();
        let start = (colon + 1).min(source.len());
        let end = scan_value_end(source, start);
        trim_ascii(&source[start..end])
    }

    /// The typed value-node kind of the declaration's leading significant token.
    #[inline]
    pub fn typed_value(&self) -> CssTypedValue {
        CssTypedValue::classify(self.value_span())
    }
}

"#,
    );
}

/// The typed value-node model — the value-head categories. The classification
/// bytes (`#` color, quotes other, digit-lead dimension/number, ident-lead
/// function/keyword) are the value-head shape of the declaration `value`
/// alternation; rendered as a behavioral template.
pub(super) fn render_typed_value(out: &mut String) {
    out.push_str(
        r#"/// The typed value-node model: the leading-token category of a declaration
/// value, recovered lazily from the source bytes. Each variant is a
/// `BackendRule` value-branch tag projection, decoded from the value head.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CssTypedValue {
    /// `<number><unit>` or `<number>%` (e.g. `10px`, `50%`, `1.2rem`).
    Dimension,
    /// A bare numeric value (e.g. `0`, `1.5`, `.5`).
    Number,
    /// A `#rrggbb` / `#rgb` hash colour.
    Color,
    /// A `name(...)` functional value (e.g. `rgb(...)`, `calc(...)`).
    Function,
    /// A keyword / identifier-led value (e.g. `red`, `none`, `var`).
    Keyword,
    /// A quoted string, url, or otherwise non-categorised value head.
    Other,
}

impl CssTypedValue {
    /// Classify a value span by its leading significant byte and shape.
    #[inline]
    pub fn classify(value: &[u8]) -> CssTypedValue {
        let value = trim_ascii(value);
        let Some(&head) = value.first() else {
            return CssTypedValue::Other;
        };
        match head {
            b'#' => CssTypedValue::Color,
            b'"' | b'\'' => CssTypedValue::Other,
            b'+' | b'-' | b'.' | b'0'..=b'9' => {
                if number_has_unit(value) {
                    CssTypedValue::Dimension
                } else {
                    CssTypedValue::Number
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | 0x80..=0xff | b'\\' => {
                if leading_ident_is_function(value) {
                    CssTypedValue::Function
                } else {
                    CssTypedValue::Keyword
                }
            }
            _ => CssTypedValue::Other,
        }
    }
}

"#,
    );
}

pub(super) fn render_document(out: &mut String) {
    out.push_str(
        r#"/// The sealed tape-backed CSS document: the retained product of a Track-1
/// parse. Holds exactly the existing `Tape` — no second substrate.
pub struct CssDocument<'input> {
    tape: Tape<'input>,
}

impl<'input> CssDocument<'input> {
    #[inline]
    pub fn tape(&self) -> &Tape<'input> {
        &self.tape
    }

    /// Iterate the structural nodes as lazy `ValueRef` views.
    pub fn nodes(&self) -> impl Iterator<Item = CssNode<'_, 'input>> + '_ {
        (0..self.tape.offsets().len() as u32).map(move |cursor| CssNode {
            node: ValueRef::new(&self.tape, cursor),
        })
    }

    /// Reconstruct the 4-field structural summary LAZILY from the tape.
    pub fn summary(&self) -> CssSummary {
        let mut summary = CssSummary::default();
        for node in self.nodes() {
            match node.kind() {
                CssNodeKind::AtRule => {
                    summary.rules += 1;
                    summary.at_rules += 1;
                }
                CssNodeKind::QualifiedRule => {
                    summary.rules += 1;
                    summary.qualified_rules += 1;
                }
                CssNodeKind::Declaration => summary.declarations += 1,
            }
        }
        summary
    }

    /// Reconstruct the RICH typed CSSOM summary LAZILY from the tape — the fair
    /// full-materialization comparison vs lightningcss. Beyond the 4 structural
    /// counts it materializes, per node, the rich typed values by `ValueRef`
    /// view: selector-list entries per qualified rule, and the typed value-node
    /// category of every declaration value head, writing nothing to the arena.
    pub fn rich_summary(&self) -> CssRichSummary {
        let mut summary = CssRichSummary::default();
        for node in self.nodes() {
            match node.value() {
                CssTypedNode::Rule(rule) if rule.is_at_rule() => {
                    summary.rules += 1;
                    summary.at_rules += 1;
                }
                CssTypedNode::Rule(rule) => {
                    summary.rules += 1;
                    summary.qualified_rules += 1;
                    summary.selectors += rule.selector_count();
                }
                CssTypedNode::Declaration(decl) => {
                    summary.declarations += 1;
                    match decl.typed_value() {
                        CssTypedValue::Dimension => summary.dimensions += 1,
                        CssTypedValue::Number => summary.numbers += 1,
                        CssTypedValue::Color => summary.colors += 1,
                        CssTypedValue::Function => summary.functions += 1,
                        CssTypedValue::Keyword | CssTypedValue::Other => {}
                    }
                }
            }
        }
        summary
    }
}

/// The document-level seam shared with every grammar's document handle: root
/// cursor + tape-id + source bytes over the single tape. Carries NO navigation
/// (the flat sweep stays on `CssDocument` inherently) — naturally flatten-free.
impl<'input> DocumentView<'input> for CssDocument<'input> {
    type Root = CssNode<'input, 'input>;

    fn root_value(&'input self) -> Self::Root {
        CssNode {
            node: ValueRef::new(&self.tape, 0),
        }
    }

    fn tape_id(&self) -> TapeId {
        self.tape.id()
    }

    fn source(&'input self) -> &'input [u8] {
        self.tape.source()
    }
}

"#,
    );
}

pub(super) fn render_summaries(out: &mut String) {
    out.push_str(
        r#"#[derive(Default, Copy, Clone, Eq, PartialEq, Debug)]
pub struct CssSummary {
    pub rules: usize,
    pub at_rules: usize,
    pub qualified_rules: usize,
    pub declarations: usize,
}

/// The rich typed-CSSOM summary: the 4 structural fields plus the lazily
/// materialized rich value plane (selectors + typed value-node counts).
#[derive(Default, Copy, Clone, Eq, PartialEq, Debug)]
pub struct CssRichSummary {
    pub rules: usize,
    pub at_rules: usize,
    pub qualified_rules: usize,
    pub declarations: usize,
    pub selectors: usize,
    pub dimensions: usize,
    pub numbers: usize,
    pub colors: usize,
    pub functions: usize,
}

"#,
    );
}

/// The lazy span helpers. The `scan_value_end` balanced bytes (`(`/`)`/`[`/`]`/
/// `{`/`}`/`;`) are grammar-derived from the [`ScanAlphabet`].
pub(super) fn render_lazy_helpers(alphabet: &ScanAlphabet, out: &mut String) {
    out.push_str(
        r#"/// Trim leading/trailing ASCII whitespace from a span.
#[inline]
fn trim_ascii(mut bytes: &[u8]) -> &[u8] {
    while let [first, rest @ ..] = bytes {
        if first.is_ascii_whitespace() {
            bytes = rest;
        } else {
            break;
        }
    }
    while let [rest @ .., last] = bytes {
        if last.is_ascii_whitespace() {
            bytes = rest;
        } else {
            break;
        }
    }
    bytes
}

/// Count top-level commas in a selector prelude (commas not nested inside
/// `()`/`[]`/`{}` or strings) — the selector-list separator count. Rides the
/// shared neutral `parse_that_simd` bracket-depth NEON kernel.
#[inline]
fn count_top_level_commas(bytes: &[u8]) -> usize {
    crate::runtime_simd::count_top_level_commas(bytes)
}

"#,
    );

    // `scan_value_end` — the balanced open/close bytes are grammar-derived.
    let opens: Vec<u8> = alphabet.balanced_pairs.iter().map(|(o, _)| *o).collect();
    let closes_round: Vec<u8> = alphabet
        .balanced_pairs
        .iter()
        .filter(|(o, _)| *o != alphabet.block_open)
        .map(|(_, c)| *c)
        .collect();
    let open_pat = opens
        .iter()
        .map(|&b| byte_literal(b))
        .collect::<Vec<_>>()
        .join(" | ");
    let close_pat = closes_round
        .iter()
        .map(|&b| byte_literal(b))
        .collect::<Vec<_>>()
        .join(" | ");
    let _ = write!(
        out,
        r#"/// Forward-scan from a declaration value head to its terminator (`;` or `}}`
/// at top level), honouring balanced `()`/`[]`/`{{}}` and strings.
#[inline]
fn scan_value_end(bytes: &[u8], start: usize) -> usize {{
    let mut depth: i32 = 0;
    let mut pos = start;
    while pos < bytes.len() {{
        match bytes[pos] {{
            {open_pat} => depth += 1,
            {close_pat} => depth = depth.saturating_sub(1),
            {block_close} if depth == 0 => return pos,
            {block_close} => depth -= 1,
            {terminator} if depth == 0 => return pos,
            b'"' | b'\'' => {{
                pos = skip_string(bytes, pos);
                continue;
            }}
            _ => {{}}
        }}
        pos += 1;
    }}
    bytes.len()
}}

"#,
        open_pat = open_pat,
        close_pat = close_pat,
        block_close = byte_literal(alphabet.block_close),
        terminator = byte_literal(alphabet.terminator),
    );

    out.push_str(
        r#"/// Skip a CSS string literal beginning at `pos` (a quote), returning the index
/// just past the closing quote.
#[inline]
fn skip_string(bytes: &[u8], pos: usize) -> usize {
    let quote = bytes[pos];
    let mut cursor = pos + 1;
    while cursor < bytes.len() {
        match bytes[cursor] {
            b'\\' => cursor += 2,
            byte if byte == quote => return cursor + 1,
            _ => cursor += 1,
        }
    }
    bytes.len()
}

/// Whether a numeric value head carries a unit or `%` suffix (a dimension) as
/// opposed to a bare number.
#[inline]
fn number_has_unit(value: &[u8]) -> bool {
    let mut pos = 0usize;
    if matches!(value.first(), Some(b'+' | b'-')) {
        pos += 1;
    }
    while pos < value.len() && matches!(value[pos], b'0'..=b'9' | b'.') {
        pos += 1;
    }
    if pos < value.len() && matches!(value[pos], b'e' | b'E') {
        let mut exp = pos + 1;
        if exp < value.len() && matches!(value[exp], b'+' | b'-') {
            exp += 1;
        }
        if exp < value.len() && value[exp].is_ascii_digit() {
            pos = exp;
            while pos < value.len() && value[pos].is_ascii_digit() {
                pos += 1;
            }
        }
    }
    matches!(value.get(pos), Some(b'%') | Some(b'a'..=b'z') | Some(b'A'..=b'Z'))
}

/// Whether an identifier-led value head opens a function call (`name(`).
#[inline]
fn leading_ident_is_function(value: &[u8]) -> bool {
    let mut pos = 0usize;
    while pos < value.len() {
        match value[pos] {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'-' | b'_' | 0x80..=0xff => pos += 1,
            b'\\' => pos += 2,
            _ => break,
        }
    }
    matches!(value.get(pos), Some(b'('))
}
"#,
    );
}
