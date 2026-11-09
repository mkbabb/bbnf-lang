//! BBNF-specific shape template recognition — AV.5.6 / AV.6.1–6.2.
//!
//! Mines two BBNF rule-body patterns into `ShapeTemplate` records that
//! the emitter collapses into `ShapeRef` tape records:
//!
//! 1. **`big_comment`**: `( "/*" , /[^\*]*/ , "*/" ) ?w -> Span`
//!    Three tape records (Rule → Repeat → Span leaf) collapse to one
//!    `ShapeRef` + an 8-byte `(u32 lo, u32 hi)` payload blob.
//!    Subsumes AU.6.9.
//!
//! 2. **`mapped_factor` empty branch**: `factor , ( "->" ... ) ?`
//!    When the optional `-> value_expr type_annotation?` branch is
//!    absent (~40% of factor calls per profiling-2.md), the rule body
//!    is structurally just `factor`. Template captures the `__factor`
//!    tape offset; payload blob is empty.
//!
//! These are rule-level patterns: the miner walks each rule body (not
//! per-node), recognizing the shape purely STRUCTURALLY — by the body's
//! structure and its defining concrete-syntax literals (the block-comment
//! delimiters `/*`/`*/`, the map arrow `->`), never by the rule name. The
//! templates feed into `GrammarProfile::shape_dict` for the emitter to lower.

use std::hash::{Hash, Hasher};

use rustc_hash::FxHasher;

use crate::{GrammarIR, IrNode};

// ── Shape template descriptor ──────────────────────────────────────

/// A compile-time shape template for a rule body that admits collapse
/// to a single ShapeRef tape record.
///
/// Each template records:
/// - `rule_name` — the grammar rule this template applies to.
/// - `kind` — which structural pattern was recognized.
/// - `shape_hash` — canonical 64-bit hash for dedup/matching.
/// - `payload_bytes` — size of the packed payload blob (0 for
///   empty-branch templates).
#[derive(Clone, Debug)]
pub struct BbnfShapeTemplate {
    /// Grammar rule name (e.g. "big_comment", "mapped_factor").
    pub rule_name: String,
    /// Which structural pattern this template captures.
    pub kind: BbnfShapeKind,
    /// Canonical 64-bit hash for matching at emit time.
    pub shape_hash: u64,
    /// Byte size of the packed payload blob that accompanies the
    /// ShapeRef record. 0 for empty-branch templates; 8 for
    /// single-Span templates (u32 lo + u32 hi).
    pub payload_bytes: u16,
}

/// Structural pattern kind for BBNF shape templates.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BbnfShapeKind {
    /// `big_comment`: single-hole Span template.
    /// Three-record `Rule → Repeat → Span` wrap collapses to one
    /// ShapeRef record. Payload is the inner Span's (lo, hi) pair
    /// (8 bytes).
    BigComment,
    /// `mapped_factor` empty branch: the optional `-> value_expr`
    /// mapping is absent. Template captures the `__factor` child
    /// tape offset; payload blob is empty.
    MappedFactorEmpty,
}

// ── Mining ─────────────────────────────────────────────────────────

/// Mine BBNF-specific shape templates from a compiled grammar IR.
///
/// Returns templates for rules whose body structure matches the known
/// BBNF patterns. The caller populates `GrammarProfile::shape_dict`
/// from these results.
pub fn mine_bbnf_shape_templates(ir: &GrammarIR) -> Vec<BbnfShapeTemplate> {
    let mut templates = Vec::new();

    // Recognition is purely STRUCTURAL: a rule earns a template when its
    // body matches the (tightened) shape — keyed on the defining
    // concrete-syntax literals (block-comment delimiters / the map arrow),
    // not on the rule name. The rule name is recorded as data only.
    for rule in &ir.rules {
        if is_big_comment_shape(ir, &rule.body) {
            templates.push(BbnfShapeTemplate {
                rule_name: ir.get_string(rule.name).to_string(),
                kind: BbnfShapeKind::BigComment,
                shape_hash: compute_bbnf_shape_hash(BbnfShapeKind::BigComment),
                payload_bytes: 8, // u32 lo + u32 hi
            });
        } else if is_mapped_factor_empty_shape(ir, &rule.body) {
            templates.push(BbnfShapeTemplate {
                rule_name: ir.get_string(rule.name).to_string(),
                kind: BbnfShapeKind::MappedFactorEmpty,
                shape_hash: compute_bbnf_shape_hash(BbnfShapeKind::MappedFactorEmpty),
                payload_bytes: 0, // empty payload
            });
        }
    }

    templates
}

/// Recognize the `big_comment` body shape:
///
/// ```text
/// big_comment = ( "/*" , /[^\*]*/ , "*/" ) ?w -> Span ;
/// ```
///
/// In the IR this is a Seq of three literals-and-regex wrapped in a
/// whitespace modifier, with a `-> Span` map. The structural shape is
/// fixed AND keyed on the defining concrete-syntax delimiters:
/// literal("/*"), regex(body), literal("*/"). The `/*` and `*/` tokens
/// are grammar-neutral block-comment syntax — not rule-name tokens — so
/// matching on them keeps recognition structural while preventing any
/// generic `Seq[literal, regex, literal]` from false-matching.
fn is_big_comment_shape(ir: &GrammarIR, body: &IrNode) -> bool {
    // The body may be wrapped in Map (for `-> Span`) and/or
    // OptionalWhitespace (for `?w`). Peel all transparent layers.
    let inner = peel_transparent(body);

    match inner {
        IrNode::Seq(children) if children.len() == 3 => {
            // Child 0: literal "/*"  (the opening delimiter)
            // Child 1: regex (the body scanner)
            // Child 2: literal "*/"  (the closing delimiter)
            is_literal_value(ir, children.first().unwrap(), "/*")
                && is_regex(children.get(1).unwrap())
                && is_literal_value(ir, children.get(2).unwrap(), "*/")
        }
        _ => false,
    }
}

/// Recognize the `mapped_factor` body shape with an empty optional branch:
///
/// ```text
/// mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ? ;
/// ```
///
/// In the IR this is Seq([ Ref(factor), Repeat { inner, lo: 0, hi: 1 } ]).
/// The empty-branch template fires when the Repeat(0,1) body LEADS with
/// the map-arrow literal "->". The arrow is grammar-neutral map syntax —
/// not a rule-name token — so keying on it keeps recognition structural
/// while preventing a generic `Seq[Ref, Repeat(0,1)]` (any optional
/// suffix) from false-matching.
fn is_mapped_factor_empty_shape(ir: &GrammarIR, body: &IrNode) -> bool {
    let inner = peel_transparent(body);
    match inner {
        IrNode::Seq(children) if children.len() == 2 => {
            // Child 0: Ref to factor (possibly wrapped in OptionalWhitespace)
            let has_factor_ref = matches!(peel_transparent(&children[0]), IrNode::Ref(_));
            // Child 1: Repeat { lo: 0, hi: 1, inner } — the optional `-> ...`
            // branch, whose body must lead with the map-arrow literal.
            let has_optional_map = match peel_transparent(&children[1]) {
                IrNode::Repeat {
                    inner,
                    lo: 0,
                    hi: 1,
                } => repeat_body_leads_with_arrow(ir, inner),
                _ => false,
            };
            has_factor_ref && has_optional_map
        }
        _ => false,
    }
}

/// True when the `Repeat(0,1)` body's first matched element is the
/// map-arrow literal `->`. The body is either the arrow literal directly
/// or a `Seq` whose first child is the arrow literal (after peeling
/// transparent wrappers).
fn repeat_body_leads_with_arrow(ir: &GrammarIR, body: &IrNode) -> bool {
    match peel_transparent(body) {
        IrNode::Seq(children) => children
            .first()
            .is_some_and(|first| is_literal_value(ir, first, "->")),
        other => is_literal_value(ir, other, "->"),
    }
}

// ── Shape matching helpers ─────────────────────────────────────────

/// Peel all transparent wrappers (Map, OptionalWhitespace) returning
/// the structural inner node.
fn peel_transparent(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => peel_transparent(inner),
        other => other,
    }
}

/// True when `node` peels to a `Literal` whose interned string equals
/// `expected`. This is the structural key that ties a shape to its
/// defining concrete-syntax token (e.g. `/*`, `*/`, `->`) without
/// referencing any grammar rule name.
fn is_literal_value(ir: &GrammarIR, node: &IrNode, expected: &str) -> bool {
    matches!(peel_transparent(node), IrNode::Literal(sid) if ir.get_string(*sid) == expected)
}

fn is_regex(node: &IrNode) -> bool {
    matches!(peel_transparent(node), IrNode::Regex(_))
}

// ── Hashing ────────────────────────────────────────────────────────

/// Compute a stable 64-bit hash for a BBNF shape kind.
///
/// Discriminant-based: each `BbnfShapeKind` variant gets a unique hash.
/// The hash is stable across compile sessions (no pointer/address data).
pub fn compute_bbnf_shape_hash(kind: BbnfShapeKind) -> u64 {
    let mut hasher = FxHasher::default();
    // Use a domain separator to avoid collisions with the general
    // RecognizerSignature hash space.
    b"bbnf_shape_template".hash(&mut hasher);
    kind.hash(&mut hasher);
    hasher.finish()
}
