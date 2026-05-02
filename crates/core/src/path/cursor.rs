//! Path cursor — state machine threaded through generated parse
//! functions during a lazy bail-out parse.
//!
//! The cursor lives on the recursive-descent stack: every generated
//! `parse_<rule>` function receives `&mut PathCursor` and consults it
//! before descending into a child. Three decisions:
//!
//! - [`Decision::ParseFully`] — the path does not bear on this rule's
//!   subtree (the rule is past the path's last interesting segment, or
//!   the path simply does not visit this branch). Parse the body as
//!   eager mode would.
//! - [`Decision::ParseUntil(child_index)`] — the path visits child
//!   `child_index` of this rule but no later child. Parse children
//!   `0..=child_index` and skip the rest. The child's parse function
//!   receives the cursor with `advance()` already called past the
//!   step that pointed at it.
//! - [`Decision::Skip`] — the path does not enter this rule's
//!   subtree at all. The recognizer skips the input range the rule
//!   covers (the codegen-emitted plan tells it the byte length to
//!   advance) and returns the cursor unchanged.
//!
//! ## Decision cache
//!
//! The cursor caches the most recent `(rule_id, segment_kind)` lookup.
//! Recursive-descent grammars typically ask the same `(rule, segment)`
//! pair multiple times under one rule body (e.g. a `Vec<Element>`
//! body asks once before the loop, again per iteration). The cache
//! holds one slot — minimal allocation, zero hashing — because the
//! lookup is one indirection through the codegen-emitted plan and
//! not worth a real cache layer.
//!
//! ## Ascent integration
//!
//! When a path step is [`PathSegment::Wildcard`] and the cursor is
//! asked to re-anchor to a sibling node (`.with_anchors()` or a
//! follow-up sibling lookup), the cursor delegates to the W2-landed
//! [`AscentStrategy`] supplied at construction. Eager paths and lazy
//! paths without wildcard re-anchoring never invoke ascent, so the
//! cursor only stores an `Option<&dyn AscentStrategy>`.

use super::ascent::AscentStrategy;
use super::ir::PathSegment;
use super::schema::PathSchema;

/// Classification of a path segment for decision-table lookup.
///
/// The decision table is keyed `(rule_id, SegmentKind)` rather than
/// `(rule_id, exact_segment)` because the executor's choice depends
/// only on the segment's *shape*, not its payload — a `Field("a")`
/// and a `Field("b")` produce the same `ParseUntil(child_index)`
/// decision once the rule's body has been threaded against the
/// segment-kind alphabet.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SegmentKind {
    /// Step into a named field of a struct / object key.
    Field,
    /// Step into a positional index of a list / array / tuple.
    Index,
    /// Match every element under a list (lazy iteration).
    Wildcard,
    /// Select a tagged-enum variant by name.
    VariantName,
    /// No more segments — the cursor is at the path's terminal.
    Terminal,
}

impl SegmentKind {
    /// Classify a borrowed segment into its kind.
    #[inline]
    pub fn of(segment: &PathSegment<'_>) -> Self {
        match segment {
            PathSegment::Field(_) => SegmentKind::Field,
            PathSegment::Index(_) => SegmentKind::Index,
            PathSegment::Wildcard => SegmentKind::Wildcard,
            PathSegment::VariantName(_) => SegmentKind::VariantName,
        }
    }

    /// `true` iff the kind is the terminal sentinel.
    #[inline]
    pub fn is_terminal(self) -> bool {
        matches!(self, SegmentKind::Terminal)
    }
}

/// Decision the cursor returns for a given `(rule_id, segment_kind)`
/// pair. Codegen (W3.3) emits a static decision table per grammar; the
/// cursor reads the table and folds the path's shape against the
/// table to pick one of these three outcomes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Decision {
    /// Parse the rule's full body as eager mode would.
    ParseFully,
    /// Parse children up to and including `child_index`, then skip
    /// the rest. `child_index` is zero-based against the rule's
    /// emitted child enumeration.
    ParseUntil(u16),
    /// Do not enter the rule's subtree. The recognizer skips the
    /// input range the rule covers.
    Skip,
}

/// One decision-cache slot. Holds the most recent `(rule_id, kind)`
/// pair the cursor was asked about plus the [`Decision`] computed for
/// it. The cursor consults the slot before re-running the lookup
/// closure; a hit bypasses the closure entirely.
#[derive(Debug, Clone, Copy)]
struct CacheSlot {
    rule_id: u32,
    kind: SegmentKind,
    decision: Decision,
}

/// Decision lookup signature. The cursor stores a boxed
/// `dyn Fn(...) -> Decision` matching this shape; implementors map
/// `(rule_id, segment_kind, current_segment_index)` → [`Decision`].
///
/// In production, the codegen-emitted decision table backs this
/// closure: `parse_with` constructs a [`PathCursor`] holding a
/// closure that reads the static table for the calling grammar.
/// Tests build hand-rolled closures over hand-rolled tables.
type DecisionFn<'p> = dyn Fn(u32, SegmentKind, usize) -> Decision + 'p;

/// Path cursor — the W3 lazy bail-out parser's path-state thread.
///
/// `'p` is the lifetime of the [`PathSchema`] reference and the
/// decision-lookup closure. Both share `'p` because the executor
/// scopes them to the same `parse_with` call frame.
///
/// ## Invariants
///
/// 1. `index` is monotone non-decreasing — calling [`PathCursor::advance`]
///    reduces neither the segment index nor the cursor's reachability.
/// 2. `index` never exceeds `schema.segment_count()`. When the cursor
///    has consumed every segment, [`PathCursor::peek`] returns `None`
///    and [`PathCursor::current_kind`] returns [`SegmentKind::Terminal`].
/// 3. The decision cache holds at most one slot. A miss recomputes
///    through `decision_fn`; a hit short-circuits.
pub struct PathCursor<'p, P>
where
    P: PathSchema<'p>,
{
    schema: &'p P,
    index: usize,
    cache: Option<CacheSlot>,
    decision_fn: Box<DecisionFn<'p>>,
    ascent: Option<&'p dyn AscentStrategy>,
}

impl<'p, P> PathCursor<'p, P>
where
    P: PathSchema<'p>,
{
    /// Build a cursor at index 0 with the supplied decision-lookup
    /// closure. The cursor borrows `schema` for its lifetime and
    /// owns the closure as a boxed trait object.
    ///
    /// `ascent` is `None` when the path does not need sibling-wildcard
    /// re-anchoring; the W2 [`AscentStrategy`] only fires for that
    /// narrow case (per the W3 spec §6).
    pub fn new<F>(schema: &'p P, decision_fn: F) -> Self
    where
        F: Fn(u32, SegmentKind, usize) -> Decision + 'p,
    {
        Self {
            schema,
            index: 0,
            cache: None,
            decision_fn: Box::new(decision_fn),
            ascent: None,
        }
    }

    /// Builder — attach an ascent strategy. Call before threading the
    /// cursor into a generated parse function. Wildcard-with-anchor
    /// sibling lookups consult this; eager and non-wildcard paths
    /// never reach it.
    pub fn with_ascent(mut self, ascent: &'p dyn AscentStrategy) -> Self {
        self.ascent = Some(ascent);
        self
    }

    /// Borrow the underlying schema. Generated parse functions read
    /// segments through this when they need the segment payload
    /// (`Field`'s `&str`, `Index`'s `usize`, etc.).
    #[inline]
    pub fn schema(&self) -> &'p P {
        self.schema
    }

    /// Current segment index. Zero when the cursor was just built.
    #[inline]
    pub fn index(&self) -> usize {
        self.index
    }

    /// True iff every segment has been consumed.
    #[inline]
    pub fn is_terminal(&self) -> bool {
        self.index >= self.schema.segment_count()
    }

    /// Peek at the current segment without advancing. Returns `None`
    /// once the cursor reaches its terminal.
    #[inline]
    pub fn peek(&self) -> Option<PathSegment<'p>> {
        self.schema.segment_at(self.index)
    }

    /// Classify the current segment. Returns
    /// [`SegmentKind::Terminal`] once the path has been fully walked.
    #[inline]
    pub fn current_kind(&self) -> SegmentKind {
        match self.peek() {
            Some(seg) => SegmentKind::of(&seg),
            None => SegmentKind::Terminal,
        }
    }

    /// Advance one segment. Returns the segment that was consumed,
    /// or `None` if the cursor was already at the terminal.
    pub fn advance(&mut self) -> Option<PathSegment<'p>> {
        let seg = self.schema.segment_at(self.index)?;
        self.index += 1;
        // Advancing invalidates the cache slot — the rule context
        // that produced the cached decision is the *previous*
        // segment, not the new one.
        self.cache = None;
        Some(seg)
    }

    /// AZ-IV.W3-DYNAMIC — per-iteration field match.
    ///
    /// Compare the parsed object-key against the cursor's current
    /// segment. Returns `true` (and advances the cursor one step) when
    /// the current segment is `Field(key)` and `key == parsed_key`.
    /// Returns `false` (cursor unchanged) on mismatch or when the
    /// current segment is not a `Field`. The lazy lane uses the
    /// `true`/`false` outcome to decide between "parse this value
    /// fully then break" and "byte-skip this value and continue".
    #[inline]
    pub fn match_field(&mut self, parsed_key: &str) -> bool {
        match self.peek() {
            Some(PathSegment::Field(name)) if name == parsed_key => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// AZ-IV.W3-DYNAMIC — per-iteration index match.
    ///
    /// Counterpart of [`match_field`] for positional array steps.
    /// Returns `true` (and advances) when the current segment is
    /// `Index(parsed_idx)`; `false` otherwise.
    #[inline]
    pub fn match_index(&mut self, parsed_idx: usize) -> bool {
        match self.peek() {
            Some(PathSegment::Index(n)) if n == parsed_idx => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// Decide what to do with the rule keyed by `rule_id` given the
    /// cursor's current segment kind. Reads through the decision
    /// cache; misses recompute through the construction-supplied
    /// lookup closure.
    pub fn decide(&mut self, rule_id: u32) -> Decision {
        let kind = self.current_kind();
        if let Some(slot) = self.cache
            && slot.rule_id == rule_id
            && slot.kind == kind
        {
            return slot.decision;
        }

        let decision = (self.decision_fn)(rule_id, kind, self.index);
        self.cache = Some(CacheSlot {
            rule_id,
            kind,
            decision,
        });
        decision
    }

    /// Borrow the configured ascent strategy, if any. The cursor only
    /// reaches for it under wildcard-with-anchor sibling re-anchoring
    /// (W3 spec §6); other paths never call this.
    #[inline]
    pub fn ascent(&self) -> Option<&'p dyn AscentStrategy> {
        self.ascent
    }
}

impl<'p, P> core::fmt::Debug for PathCursor<'p, P>
where
    P: PathSchema<'p>,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PathCursor")
            .field("index", &self.index)
            .field("segment_count", &self.schema.segment_count())
            .field("cache", &self.cache)
            .field("ascent", &self.ascent.map(|a| a.name()))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::{OwnedPathSegment, TypedPath};
    use crate::path::markers::Json;

    /// Build a hand-rolled JSON-shaped path
    /// `["statuses", 0, "text"]`.
    fn fixture_path() -> TypedPath<Json, &'static str> {
        TypedPath::from_owned(vec![
            OwnedPathSegment::Field("statuses".to_owned()),
            OwnedPathSegment::Index(0),
            OwnedPathSegment::Field("text".to_owned()),
        ])
    }

    /// Hand-rolled decision plan: rule 1 (root) parses fully under a
    /// `Field` segment; rule 2 (statuses array) parses-until-0 under
    /// an `Index` segment; rule 3 (status object) parses-fully under a
    /// `Field` segment; everything else skips.
    fn fixture_plan(rule_id: u32, kind: SegmentKind, _idx: usize) -> Decision {
        match (rule_id, kind) {
            (1, SegmentKind::Field) => Decision::ParseFully,
            (2, SegmentKind::Index) => Decision::ParseUntil(0),
            (3, SegmentKind::Field) => Decision::ParseFully,
            (_, SegmentKind::Terminal) => Decision::ParseFully,
            _ => Decision::Skip,
        }
    }

    #[test]
    fn cursor_advance_consumes_segments() {
        let path = fixture_path();
        let mut cursor: PathCursor<'_, TypedPath<Json, &'static str>> =
            PathCursor::new(&path, fixture_plan);

        assert_eq!(cursor.index(), 0);
        assert!(!cursor.is_terminal());
        assert_eq!(cursor.current_kind(), SegmentKind::Field);

        let s0 = cursor.advance().expect("first segment");
        assert!(matches!(s0, PathSegment::Field("statuses")));
        assert_eq!(cursor.index(), 1);
        assert_eq!(cursor.current_kind(), SegmentKind::Index);

        let s1 = cursor.advance().expect("second segment");
        assert!(matches!(s1, PathSegment::Index(0)));

        let s2 = cursor.advance().expect("third segment");
        assert!(matches!(s2, PathSegment::Field("text")));

        assert!(cursor.is_terminal());
        assert_eq!(cursor.current_kind(), SegmentKind::Terminal);
        assert!(cursor.advance().is_none());
    }

    #[test]
    fn cursor_decide_walks_decision_plan() {
        let path = fixture_path();
        let mut cursor: PathCursor<'_, TypedPath<Json, &'static str>> =
            PathCursor::new(&path, fixture_plan);

        // At index 0, segment is Field; rule 1 should ParseFully.
        assert_eq!(cursor.decide(1), Decision::ParseFully);
        // Same query hits the cache — same answer.
        assert_eq!(cursor.decide(1), Decision::ParseFully);
        // A different rule under the same segment recomputes.
        assert_eq!(cursor.decide(99), Decision::Skip);

        cursor.advance();
        // Index segment now; rule 2 should ParseUntil(0).
        assert_eq!(cursor.decide(2), Decision::ParseUntil(0));

        cursor.advance();
        // Field segment again; rule 3 should ParseFully.
        assert_eq!(cursor.decide(3), Decision::ParseFully);

        cursor.advance();
        // Terminal — every rule reports ParseFully under our plan.
        assert_eq!(cursor.decide(7), Decision::ParseFully);
    }

    #[test]
    fn cursor_peek_does_not_advance() {
        let path = fixture_path();
        let cursor: PathCursor<'_, TypedPath<Json, &'static str>> =
            PathCursor::new(&path, fixture_plan);

        let peeked = cursor.peek().expect("peek");
        assert!(matches!(peeked, PathSegment::Field("statuses")));
        assert_eq!(cursor.index(), 0);
    }

    #[test]
    fn cursor_cache_invalidated_by_advance() {
        let path = fixture_path();
        let mut cursor: PathCursor<'_, TypedPath<Json, &'static str>> =
            PathCursor::new(&path, fixture_plan);

        // Prime the cache for rule 1 at index 0 (Field).
        assert_eq!(cursor.decide(1), Decision::ParseFully);
        cursor.advance();

        // After advance, the cache is invalidated; rule 1 under
        // Index segment now resolves to Skip per our plan.
        assert_eq!(cursor.decide(1), Decision::Skip);
    }

    #[test]
    fn empty_path_terminal_immediately() {
        let path: TypedPath<Json, &'static str> = TypedPath::from_owned(Vec::new());
        let cursor: PathCursor<'_, TypedPath<Json, &'static str>> =
            PathCursor::new(&path, fixture_plan);

        assert!(cursor.is_terminal());
        assert_eq!(cursor.current_kind(), SegmentKind::Terminal);
        assert!(cursor.peek().is_none());
    }
}
