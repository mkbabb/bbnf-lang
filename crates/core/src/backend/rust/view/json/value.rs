//! `Value` — first-class JSON value tree, isomorphic to
//! `sonic_rs::Value`.
//!
//! AX.W1.A — the consumer-facing projection of a BBNF-parsed JSON
//! document. Six variants mirror the JSON data model and the sonic-rs
//! `ValueRef` shape one-for-one: `Null`, `Bool`, `Number`, `String`,
//! `Array`, `Object`. String storage is `Cow<'input, str>` — escape-
//! free spans borrow from the parser's input directly (zero-copy);
//! escape-bearing spans copy into an owned `String` at projection
//! time. Object keys follow the same discipline.
//!
//! Invariant 18 — field-complete on day one. Every variant lands
//! populated from the tape walker in one shot; no `todo!()`, no `_`
//! catch-alls, no `#[allow(dead_code)]` placeholders.
//!
//! Invariant 5 — binary parity with no tolerances. [`Value::eq`]
//! decides by the same algebraic structure `sonic_rs::Value::as_ref()`
//! exposes (`Null | Bool | Number | String | Array | Object`); the
//! `PartialEq<sonic_rs::Value>` impl routes through the same
//! discriminator so BBNF's Value `eq` a sonic_rs Value iff they
//! project the same six-variant tree with the same leaves.
//!
//! # Tape walker
//!
//! [`Value::from_tape`] and [`Value::from_cursor`] walk the
//! shape-emission-authoritative tape (invariant 20) to project a
//! typed value tree. The walker dispatches on `(kind, variant_idx)`
//! where `variant_idx` identifies the grammar rule that produced the
//! tape record; a [`JsonRuleIds`] struct carries the rule-id
//! resolution so the walker does not hard-code numeric discriminants
//! (those shift with every IR prune-pass reordering).
//!
//! The JSON grammar declares six value-producing rules:
//!
//! ```text
//! null   = "null"            -> 0u8                          (InlineScalar u8)
//! bool   = "true" -> true | "false" -> false                 (InlineScalar u8)
//! number = /-?.../           -> f64                          (WideScalar f64)
//! string = /"..."/           -> decode_json_string_to_arena  (Bytes)
//! array  = "[" >> ( value << comma ? ) * ?w << "]"           (compound)
//! object = "{" >> ( pair << comma ? ) * ?w << "}"            (compound)
//! pair   = string, colon >> value                            (compound / kv)
//! ```
//!
//! The walker extracts Null/Bool/Number/String directly from leaf
//! payloads; Array/Object recurse through `children_zero_alloc`;
//! pair records produce an (`&str`, [`Value`]) tuple consumed by the
//! Object accumulator.

use core::fmt;
use indexmap::IndexMap;
use std::borrow::Cow;

use tape::{Tape, TapeCursor, TapeKind, TapeOffset};

use super::number::Number;

/// Six-variant JSON value tree — isomorphic to `sonic_rs::Value`.
///
/// String storage is `Cow<'input, str>`: borrow-safe leaves (no
/// `\` escapes in the source span) slice the input directly; escape-
/// bearing leaves copy into an owned `String` during projection.
/// Object keys follow the same discipline. `Array` and `Object`
/// preserve source order — `Vec` for arrays, [`IndexMap`] for objects
/// (JSON preserves key insertion order per RFC 8259 §4; sonic-rs
/// itself preserves order when iterating `Object`).
#[derive(Clone, Debug)]
pub enum Value<'input> {
    /// JSON `null`.
    Null,
    /// JSON `true` / `false`.
    Bool(bool),
    /// JSON number — integer or floating-point, see [`Number`].
    Number(Number),
    /// JSON string — borrowed slice for escape-free leaves, owned
    /// `String` for escape-bearing leaves.
    String(Cow<'input, str>),
    /// JSON array — source-order vector of child values.
    Array(Vec<Value<'input>>),
    /// JSON object — source-order map of string-keyed child values.
    Object(IndexMap<Cow<'input, str>, Value<'input>>),
}

impl<'input> PartialEq for Value<'input> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Deep structural equality over the six-variant partition.
        // Invariant 18 discipline — the three-stage decomposition
        // (tag-equal, same-tag intra-eq, cross-tag always-false) is
        // explicit, not a `_ => todo!()` placeholder. Cross-tag
        // pairs (30 of 36) collapse through `variant_tag`.
        if variant_tag(self) != variant_tag(other) {
            return false;
        }
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a.as_ref() == b.as_ref(),
            (Value::Array(a), Value::Array(b)) => a == b,
            // IndexMap `PartialEq` is order-insensitive; sonic-rs's
            // object equality is order-insensitive too.
            (Value::Object(a), Value::Object(b)) => a == b,
            // The six `(Variant, Variant)` pairs above exhaust the
            // same-tag case (variant_tag equality gates entry).
            // `unreachable_unchecked`-like branches would be UB-
            // adjacent; this arm uses the infallible-refutable
            // pattern via the tag check's logical contract.
            (Value::Null, _)
            | (Value::Bool(_), _)
            | (Value::Number(_), _)
            | (Value::String(_), _)
            | (Value::Array(_), _)
            | (Value::Object(_), _) => false,
        }
    }
}

/// Six-variant tag extractor — a `u8` proxy for the `Value`
/// variant. Used by `PartialEq` to decide cross-variant
/// inequality in one compare before recursing into same-tag
/// payload equality.
#[inline]
fn variant_tag(v: &Value<'_>) -> u8 {
    match v {
        Value::Null => 0,
        Value::Bool(_) => 1,
        Value::Number(_) => 2,
        Value::String(_) => 3,
        Value::Array(_) => 4,
        Value::Object(_) => 5,
    }
}

impl<'input> Eq for Value<'input> {}

// ── IR rule-id context ────────────────────────────────────────────

/// Identifiers of the JSON grammar's value-producing rules as they
/// appear in the freshly-compiled IR.
///
/// Rule ids are assigned by the IR pipeline's prune pass (AW-III
/// reorders rules) and reach the tape via the 8-bit `variant_idx`
/// slot on every record (widened from 6 bits in AW-III.W1.A). The
/// walker matches incoming `variant_idx` against these ids to
/// identify which grammar rule produced each record; hard-coded
/// numeric constants drift across IR revisions, so every consumer
/// constructs this struct once per parser instance via the
/// compiled IR.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct JsonRuleIds {
    /// `null` rule id (produces `Value::Null`).
    pub null: u8,
    /// `bool` rule id (produces `Value::Bool`).
    pub bool_: u8,
    /// `number` rule id (produces `Value::Number`).
    pub number: u8,
    /// `string` rule id (produces `Value::String`).
    pub string: u8,
    /// `array` rule id (produces `Value::Array`).
    pub array: u8,
    /// `object` rule id (produces `Value::Object`).
    pub object: u8,
    /// `pair` rule id (object key-value entry).
    pub pair: u8,
    /// `value` rule id (the entry alt dispatcher).
    pub value: u8,
}

impl JsonRuleIds {
    /// Resolve the JSON grammar's rule ids from a freshly-compiled
    /// IR. Used by tests and benches that have a `GrammarIR` handle.
    ///
    /// Panics if any of the seven declared rules are missing — that
    /// condition only reaches this function if the BBNF lowering
    /// silently dropped a rule, which is itself a bug the caller
    /// wants surfaced loudly rather than hidden.
    pub fn from_ir(ir: &bbnf_ir::GrammarIR) -> Self {
        let lookup = |name: &str| -> u8 {
            let rule = ir
                .rules
                .iter()
                .find(|r| ir.get_string(r.name) == name)
                .unwrap_or_else(|| panic!("JSON grammar rule `{name}` missing"));
            (rule.id & 0xFF) as u8
        };
        Self {
            null: lookup("null"),
            bool_: lookup("bool"),
            number: lookup("number"),
            string: lookup("string"),
            array: lookup("array"),
            object: lookup("object"),
            pair: lookup("pair"),
            value: lookup("value"),
        }
    }
}

// ── Tape walker ──────────────────────────────────────────────────

impl<'input> Value<'input> {
    /// Project a `Value` tree from a parsed JSON tape, starting at
    /// `root`. The resulting tree borrows string leaves from `input`
    /// when the source bytes are escape-free; escape-bearing leaves
    /// copy into an owned `String`.
    pub fn from_tape(
        tape: &'input Tape,
        input: &'input str,
        root: TapeOffset,
        ids: &JsonRuleIds,
    ) -> Self {
        let cursor = TapeCursor::new(tape, root);
        Self::from_cursor(cursor, input, ids)
    }

    /// Project a `Value` from a tape cursor positioned on a value-
    /// producing record. Walks children recursively as needed.
    ///
    /// Dispatch key is `(kind, variant_idx)` because AW-III.W1.A
    /// wires structural literals (`[`, `,`, `]`) to inherit their
    /// enclosing rule's variant_idx — so `variant_idx == ids.array`
    /// alone matches both the array compound and every structural
    /// `[` / `,` / `]` under it. The kind discriminator rules those
    /// out: value-producing leaves are always `TapeKind::Span`,
    /// value-producing compounds are always `is_compound()`.
    pub fn from_cursor<'tape>(
        cursor: TapeCursor<'tape>,
        input: &'input str,
        ids: &JsonRuleIds,
    ) -> Self
    where
        'tape: 'input,
    {
        let variant = cursor.variant_idx();
        let kind = cursor.kind();

        // Typed leaves — the four `->`-annotated rules land on
        // `TapeKind::Span` records. Routing keys on both kind and
        // variant_idx so a structural `[` literal with `variant_idx
        // == ids.array` (AW-III.W1.A variant-inheritance) doesn't
        // hijack the leaf path.
        if kind == TapeKind::Span {
            if variant == ids.null {
                return Self::decode_null(&cursor);
            }
            if variant == ids.bool_ {
                return Self::decode_bool(&cursor);
            }
            if variant == ids.number {
                return Self::decode_number(&cursor);
            }
            if variant == ids.string {
                return Self::decode_string(&cursor, input);
            }
        }

        // Compound rules — Rule/Seq/Alt compounds whose variant_idx
        // identifies the `array` / `object` / `value` rule. `pair`
        // compounds are consumed inside `decode_object`; they don't
        // reach this top-level dispatch except via recursion.
        if kind.is_compound() {
            if variant == ids.array {
                return Self::decode_array(cursor, input, ids);
            }
            if variant == ids.object {
                return Self::decode_object(cursor, input, ids);
            }
            // `value` Alt, any structural Seq/Rule wrapper: peel
            // to the first value-producing descendant.
            return Self::project_first_matching_child(cursor, input, ids);
        }

        // Leaf record with a variant_idx that doesn't match any
        // value-producing rule — reaches the walker from structural
        // literals (`[`, `,`, `:`, `]`) or epsilon-whitespace
        // records. Callers filter these via `is_value_producing`
        // before recursing, so this path is defensive; return Null
        // as the neutral element when encountered at the top level.
        Value::Null
    }

    /// Walk the cursor's children in source order and project the
    /// first child whose `variant_idx` or `kind` maps to a value-
    /// producing rule. Used for the `value` Alt dispatcher whose
    /// chosen branch is wrapped in a `Rule` compound that sits above
    /// the actual typed leaf.
    fn project_first_matching_child<'tape>(
        cursor: TapeCursor<'tape>,
        input: &'input str,
        ids: &JsonRuleIds,
    ) -> Self
    where
        'tape: 'input,
    {
        for child in cursor.children_zero_alloc() {
            if is_value_producing(&child, ids) {
                return Self::from_cursor(child, input, ids);
            }
        }
        // Empty or all-structural compound — no typed value here.
        // Only reachable for degenerate inputs (empty document);
        // return Null as the neutral element.
        Value::Null
    }

    // ── Leaf decoders ────────────────────────────────────────

    /// Project a `null` leaf. The `null = "null" -> 0u8` grammar
    /// annotation pushes a `TapeKind::Span` with an InlineScalar u8
    /// payload of `0`; we decode conservatively (any `null`-variant
    /// span collapses to `Value::Null` regardless of the payload
    /// content, since the grammar only ever emits `0u8`).
    #[inline]
    fn decode_null<'tape>(_cursor: &TapeCursor<'tape>) -> Self {
        Value::Null
    }

    /// Project a `bool` leaf. The `bool` rule's AV.0.1 aggregate-
    /// payload layout stores 1 byte in the arena; `payload_bytes(1)`
    /// reads that byte. Post-close-out both branches of the Alt
    /// write their payload byte (`true` → 1, `false` → 0).
    #[inline]
    fn decode_bool<'tape>(cursor: &TapeCursor<'tape>) -> Self {
        let rec = cursor.record();
        let tape = cursor.tape();
        let bits = tape
            .payload_bytes(rec, 1)
            .map(|b| b[0])
            // Defensive fallback: an empty 1-byte payload (pre-
            // AV.0.1 build) decodes via the inline `payload_bool`
            // path, which masked the two-branch gap. After AV.0.1
            // close-out this fallback is never exercised; its
            // presence keeps the walker total across tape revisions.
            .or_else(|| tape.payload_bool(rec).map(|b| b as u8))
            .unwrap_or(0);
        Value::Bool(bits != 0)
    }

    /// Project a `number` leaf via `payload_f64`. The JSON grammar's
    /// `-> f64` annotation lands every number in a WideScalar f64
    /// slot; `payload_f64(rec)` is the typed wide reader.
    #[inline]
    fn decode_number<'tape>(cursor: &TapeCursor<'tape>) -> Self {
        let rec = cursor.record();
        let tape = cursor.tape();
        let raw = tape.payload_f64(rec).unwrap_or(0.0);
        // Canonicalise: non-finite doubles (`NaN`, `±∞`) are not JSON
        // numbers. The `number` regex never matches a non-finite
        // literal, so in practice `raw.is_finite()` always holds.
        // The `unwrap_or(0.0)` fallback handles a degenerate tape
        // that lost its payload; we still produce a valid `Number`.
        let n = Number::from_f64_finite(raw).unwrap_or(Number::from_u64(0));
        Value::Number(n)
    }

    /// Project a `string` leaf. `payload_string_with_source` returns
    /// `Some(&str)` for both borrow-safe (slices from `source`) and
    /// arena-decoded (reads from the tape's arena) paths; we promote
    /// the borrow-safe path to `Cow::Borrowed` (points at `input`)
    /// and the arena path to `Cow::Owned` (copy out of the arena so
    /// the resulting `Value` is decoupled from the tape's lifetime).
    #[inline]
    fn decode_string<'tape>(cursor: &TapeCursor<'tape>, input: &'input str) -> Self
    where
        'tape: 'input,
    {
        let rec = cursor.record();
        let tape = cursor.tape();
        // Fast path: the record is flagged as borrowed (no escapes in
        // the source span). `rec.is_string_borrowed()` is the decoder
        // kernel's signal; we slice `input` directly — no arena
        // involvement, no allocation.
        if rec.is_string_borrowed() {
            let lo = rec.span_lo as usize + 1; // skip opening `"`
            let hi = (rec.span_hi as usize).saturating_sub(1); // skip closing `"`
            if lo <= hi && hi <= input.len() {
                return Value::String(Cow::Borrowed(&input[lo..hi]));
            }
        }
        // Escape-bearing path: decoded bytes live in the arena.
        // `payload_string` returns the decoded UTF-8 slice; copy to
        // owned so the `Cow::Owned` variant's lifetime is independent
        // of the tape's arena.
        if let Some(decoded) = tape.payload_string(rec) {
            return Value::String(Cow::Owned(decoded.to_string()));
        }
        // Fallback: neither borrow flag nor arena payload — decode
        // directly from the source span. The grammar's `string`
        // regex ensures the span is wrapped in `"..."`; slice inside
        // the quotes.
        let lo = rec.span_lo as usize;
        let hi = rec.span_hi as usize;
        let raw = input.get(lo..hi).unwrap_or("");
        let inner = raw.strip_prefix('"').and_then(|s| s.strip_suffix('"')).unwrap_or(raw);
        Value::String(Cow::Borrowed(inner))
    }

    // ── Compound decoders ────────────────────────────────────

    /// Project an `array` record — collect every value-producing
    /// descendant into a `Vec`, preserving source order. Structural
    /// literals (`[`, `,`, `]`) and the optional whitespace filler
    /// reach the walker as leaves with non-value `variant_idx`;
    /// [`is_value_producing`] rejects them.
    fn decode_array<'tape>(
        cursor: TapeCursor<'tape>,
        input: &'input str,
        ids: &JsonRuleIds,
    ) -> Self
    where
        'tape: 'input,
    {
        let mut out: Vec<Value<'input>> = Vec::new();
        collect_value_children(cursor, input, ids, &mut out);
        Value::Array(out)
    }

    /// Project an `object` record — collect every `pair` descendant
    /// into an `IndexMap`, preserving insertion order. Each `pair`
    /// record carries a key string (first value-producing child)
    /// and a value (second value-producing child).
    fn decode_object<'tape>(
        cursor: TapeCursor<'tape>,
        input: &'input str,
        ids: &JsonRuleIds,
    ) -> Self
    where
        'tape: 'input,
    {
        let mut out: IndexMap<Cow<'input, str>, Value<'input>> = IndexMap::new();
        visit_pairs(cursor, input, ids, &mut |key, value| {
            out.insert(key, value);
        });
        Value::Object(out)
    }
}

/// True when the cursor's record produces a `Value` node (rather
/// than a structural filler).
///
/// Six value-producing rules exist — four typed leaves on
/// `TapeKind::Span` records (`null`, `bool`, `number`, `string`)
/// plus two compounds (`array`, `object`). A `value` Alt compound
/// wraps the chosen branch and also counts as value-producing
/// (the walker peels it to the inner branch).
///
/// AW-III.W1.A variant inheritance: structural literals (`[`, `,`,
/// `]`) within an array rule carry `variant_idx == ids.array`, so
/// the kind discriminator matters. Typed leaves are always
/// `TapeKind::Span`; value-producing compounds are always
/// `is_compound()`. The pair rule is handled separately by
/// [`visit_pairs`].
#[inline]
fn is_value_producing<'tape>(cursor: &TapeCursor<'tape>, ids: &JsonRuleIds) -> bool {
    let v = cursor.variant_idx();
    let kind = cursor.kind();
    // Typed leaves on Span records.
    if kind == TapeKind::Span
        && (v == ids.null || v == ids.bool_ || v == ids.number || v == ids.string)
    {
        return true;
    }
    // Compounds — array / object / value (the entry-alt wrapper).
    if kind.is_compound() && (v == ids.array || v == ids.object || v == ids.value) {
        return true;
    }
    false
}

/// Recurse into `cursor`'s subtree, appending every value-producing
/// descendant to `out` in source order. Skips structural leaves
/// (literals for `[`/`,`/`]`/quotes) and the optional-whitespace
/// wrappers that the JSON grammar places around the array body.
fn collect_value_children<'tape, 'input>(
    cursor: TapeCursor<'tape>,
    input: &'input str,
    ids: &JsonRuleIds,
    out: &mut Vec<Value<'input>>,
) where
    'tape: 'input,
{
    for child in cursor.children_zero_alloc() {
        if is_value_producing(&child, ids) {
            out.push(Value::from_cursor(child, input, ids));
        } else if child.kind().is_compound() {
            // Structural compound (e.g. the repeat's `Seq` wrapper
            // or the `?w` optional-whitespace envelope) — recurse
            // into it so the contained values reach `out`.
            collect_value_children(child, input, ids, out);
        }
        // Else: pure structural leaf (literal `[`, `,`, `]`, epsilon
        // whitespace). Skip.
    }
}

/// Walk `cursor`'s subtree looking for `pair` compounds. For each
/// pair encountered, extract its key + value and invoke `f`.
fn visit_pairs<'tape, 'input, F>(
    cursor: TapeCursor<'tape>,
    input: &'input str,
    ids: &JsonRuleIds,
    f: &mut F,
) where
    'tape: 'input,
    F: FnMut(Cow<'input, str>, Value<'input>),
{
    for child in cursor.children_zero_alloc() {
        if child.variant_idx() == ids.pair && child.kind().is_compound() {
            if let Some((key, value)) = extract_pair(child, input, ids) {
                f(key, value);
            }
        } else if child.kind().is_compound() {
            visit_pairs(child, input, ids, f);
        }
    }
}

/// Extract a `pair` compound's key (first string child) and value
/// (second value-producing child).
fn extract_pair<'tape, 'input>(
    cursor: TapeCursor<'tape>,
    input: &'input str,
    ids: &JsonRuleIds,
) -> Option<(Cow<'input, str>, Value<'input>)>
where
    'tape: 'input,
{
    // `pair = string, colon >> value`. The pair compound's children
    // run: string-leaf, colon-literal (skipped by structural filter),
    // value-compound. We collect the value-producing children in
    // source order and pick the first two.
    //
    // KvPair (tape-flattened pair) path: when the grammar emits the
    // pair through the `TapeKind::KvPair` flattening — currently
    // JSON's `pair` does not reach that shape (value is a full Alt,
    // not a scalar), but the branch is present so the walker remains
    // total against future tape revisions.
    if cursor.kind() == TapeKind::KvPair {
        // `payload_bytes(16)` reads the flattened scalar payload;
        // the key span is `(span_lo, span_hi)` of the pair record
        // itself. The flattened shape never arrives for JSON
        // (value is an alt, not a scalar), but the decode remains
        // structurally sound if it ever does.
        let (lo, hi) = cursor.span();
        let key_raw = input.get(lo as usize..hi as usize).unwrap_or("");
        let key = strip_json_quotes(key_raw).map(Cow::Borrowed).unwrap_or_else(|| Cow::Borrowed(key_raw));
        // No embedded value representation — fall back to Null.
        return Some((key, Value::Null));
    }

    let mut found: Vec<TapeCursor<'tape>> = Vec::with_capacity(2);
    gather_pair_children(cursor, ids, &mut found);
    if found.len() < 2 {
        return None;
    }
    let key_cursor = found[0];
    let value_cursor = found[1];
    let key = extract_string_value(key_cursor, input);
    let value = Value::from_cursor(value_cursor, input, ids);
    Some((key, value))
}

/// Collect every value-producing descendant of a `pair` compound in
/// source order — the first entry is the key string, the second is
/// the value. The pair body `string, colon >> value` nests the value
/// inside the Seq's compound wrapper; the recursion peels both
/// sides.
fn gather_pair_children<'tape>(
    cursor: TapeCursor<'tape>,
    ids: &JsonRuleIds,
    out: &mut Vec<TapeCursor<'tape>>,
) {
    for child in cursor.children_zero_alloc() {
        if is_value_producing(&child, ids) {
            out.push(child);
        } else if child.kind().is_compound() {
            gather_pair_children(child, ids, out);
        }
    }
}

/// Extract the key-string value out of a cursor known to point at a
/// `string`-rule record. Mirrors [`Value::decode_string`] but
/// returns the inner `Cow` directly (the map key type).
fn extract_string_value<'tape, 'input>(
    cursor: TapeCursor<'tape>,
    input: &'input str,
) -> Cow<'input, str>
where
    'tape: 'input,
{
    let rec = cursor.record();
    let tape = cursor.tape();
    if rec.is_string_borrowed() {
        let lo = rec.span_lo as usize + 1;
        let hi = (rec.span_hi as usize).saturating_sub(1);
        if lo <= hi && hi <= input.len() {
            return Cow::Borrowed(&input[lo..hi]);
        }
    }
    if let Some(decoded) = tape.payload_string(rec) {
        return Cow::Owned(decoded.to_string());
    }
    let lo = rec.span_lo as usize;
    let hi = rec.span_hi as usize;
    let raw = input.get(lo..hi).unwrap_or("");
    strip_json_quotes(raw).map(Cow::Borrowed).unwrap_or(Cow::Borrowed(raw))
}

/// Strip surrounding `"..."` quotes from a raw JSON string source
/// span. Returns `None` when the quotes are missing (defensive; the
/// grammar only emits string records from the quoted regex, so the
/// quotes are always present on real inputs).
#[inline]
fn strip_json_quotes(raw: &str) -> Option<&str> {
    raw.strip_prefix('"').and_then(|s| s.strip_suffix('"'))
}

// ── sonic-rs comparator bridge ────────────────────────────────────

impl<'input> PartialEq<sonic_rs::Value> for Value<'input> {
    #[inline]
    fn eq(&self, other: &sonic_rs::Value) -> bool {
        eq_against_sonic(self, other)
    }
}

impl<'input> PartialEq<Value<'input>> for sonic_rs::Value {
    #[inline]
    fn eq(&self, other: &Value<'input>) -> bool {
        eq_against_sonic(other, self)
    }
}

/// Deep equality between a BBNF `Value` and a sonic-rs `Value`.
///
/// Both sides decompose through the six-variant discriminator —
/// BBNF's own enum on one side, `sonic_rs::Value::as_ref()` on the
/// other. Arrays and objects recurse. Objects compare
/// order-insensitively (matching both sides' native behaviour —
/// BBNF's `IndexMap` `PartialEq` is order-insensitive; sonic-rs
/// materialises objects through ordered pair slices or hash maps
/// depending on storage, but every public comparison goes through
/// key-wise membership).
fn eq_against_sonic(lhs: &Value<'_>, rhs: &sonic_rs::Value) -> bool {
    use sonic_rs::JsonContainerTrait;
    use sonic_rs::ValueRef;
    // Invariant 18 — cross-variant non-equality is decided by tag
    // comparison before the per-variant payload match; no bare
    // `_ => false` placeholder.
    let rhs_ref = rhs.as_ref();
    if variant_tag(lhs) != sonic_variant_tag(&rhs_ref) {
        return false;
    }
    match (lhs, rhs_ref) {
        (Value::Null, ValueRef::Null) => true,
        (Value::Bool(a), ValueRef::Bool(b)) => *a == b,
        (Value::Number(a), ValueRef::Number(b)) => *a == b,
        (Value::String(a), ValueRef::String(b)) => a.as_ref() == b,
        (Value::Array(a), ValueRef::Array(_)) => {
            let Some(b) = rhs.as_array() else {
                return false;
            };
            if a.len() != b.len() {
                return false;
            }
            for (ai, bi) in a.iter().zip(b.iter()) {
                if !eq_against_sonic(ai, bi) {
                    return false;
                }
            }
            true
        }
        (Value::Object(a), ValueRef::Object(_)) => {
            let Some(b) = rhs.as_object() else {
                return false;
            };
            if a.len() != b.len() {
                return false;
            }
            // Order-insensitive: every BBNF key must match a sonic-rs
            // entry, and vice versa. We only iterate one side (len
            // already matches).
            for (ka, va) in a.iter() {
                let Some(vb) = b.get(ka) else {
                    return false;
                };
                if !eq_against_sonic(va, vb) {
                    return false;
                }
            }
            true
        }
        // The six same-tag arms above exhaust the match — the tag
        // guard above ensures only same-tag pairs reach this point.
        // Cross-tag pairs are listed explicitly so the match is
        // total without a bare wildcard arm (invariant 18).
        (Value::Null, _)
        | (Value::Bool(_), _)
        | (Value::Number(_), _)
        | (Value::String(_), _)
        | (Value::Array(_), _)
        | (Value::Object(_), _) => false,
    }
}

/// Six-variant tag extractor for `sonic_rs::ValueRef` — mirrors
/// [`variant_tag`] one-to-one so the two discriminator checks
/// yield the same `u8` for the same JSON variant. Used by
/// [`eq_against_sonic`] to decide cross-variant inequality before
/// the payload-equality dispatch.
#[inline]
fn sonic_variant_tag(v: &sonic_rs::ValueRef<'_>) -> u8 {
    use sonic_rs::ValueRef;
    match v {
        ValueRef::Null => 0,
        ValueRef::Bool(_) => 1,
        ValueRef::Number(_) => 2,
        ValueRef::String(_) => 3,
        ValueRef::Array(_) => 4,
        ValueRef::Object(_) => 5,
    }
}

// ── sonic-rs → Value conversion ──────────────────────────────────

/// Construct a BBNF `Value` from a sonic-rs `Value`. The resulting
/// tree is fully owned (`Cow::Owned` for every string, heap-
/// allocated `Vec` / `IndexMap`) — sonic-rs's internal storage
/// representation isn't exposable as borrowed `&str` without
/// touching its private API, so conversion materialises owned
/// copies. This is acceptable for the parity-harness use case where
/// the BBNF value tree needs to outlive the sonic-rs parse.
impl From<sonic_rs::Value> for Value<'static> {
    fn from(v: sonic_rs::Value) -> Self {
        (&v).into()
    }
}

impl From<&sonic_rs::Value> for Value<'static> {
    fn from(v: &sonic_rs::Value) -> Self {
        use sonic_rs::JsonContainerTrait;
        use sonic_rs::ValueRef;
        match v.as_ref() {
            ValueRef::Null => Value::Null,
            ValueRef::Bool(b) => Value::Bool(b),
            ValueRef::Number(n) => Value::Number(Number::from(n)),
            ValueRef::String(s) => Value::String(Cow::Owned(s.to_string())),
            ValueRef::Array(_) => {
                let arr = v.as_array().expect("as_ref reports Array");
                let mut out: Vec<Value<'static>> = Vec::with_capacity(arr.len());
                for item in arr.iter() {
                    out.push(Value::from(item));
                }
                Value::Array(out)
            }
            ValueRef::Object(_) => {
                let obj = v.as_object().expect("as_ref reports Object");
                let mut out: IndexMap<Cow<'static, str>, Value<'static>> =
                    IndexMap::with_capacity(obj.len());
                for (key, value) in obj.iter() {
                    out.insert(Cow::Owned(key.to_string()), Value::from(value));
                }
                Value::Object(out)
            }
        }
    }
}

// ── Display (debug aid) ──────────────────────────────────────────

impl<'input> fmt::Display for Value<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => f.write_str("null"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{}\"", s.as_ref()),
            Value::Array(arr) => {
                f.write_str("[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        f.write_str(",")?;
                    }
                    write!(f, "{v}")?;
                }
                f.write_str("]")
            }
            Value::Object(obj) => {
                f.write_str("{")?;
                for (i, (k, v)) in obj.iter().enumerate() {
                    if i > 0 {
                        f.write_str(",")?;
                    }
                    write!(f, "\"{}\":{}", k.as_ref(), v)?;
                }
                f.write_str("}")
            }
        }
    }
}

