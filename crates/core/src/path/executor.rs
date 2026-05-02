//! Path executor — top-level orchestrator for a lazy bail-out parse.
//!
//! [`PathExecutor::execute`] is the entry point: it builds a
//! [`PathCursor`], threads it into the supplied parse callback, and
//! returns whatever leaf the cursor extracted (or `None` if the
//! callback bailed). Per the W3 spec §2, the per-grammar `parse_with`
//! routes for each of {JSON, CSS L4, Sheets, BBNF} land in W3.2 and
//! call into this executor with their grammar's generated entry rule
//! as the callback body.
//!
//! The executor itself is grammar-agnostic. Three obligations:
//!
//! - Construct a [`PathCursor`] over the supplied schema and decision
//!   table.
//! - Hand the cursor to the parse callback exactly once.
//! - Surface the callback's `Option<P::Output>` verbatim.
//!
//! The decision table is closure-shaped (`DecisionFn`) so the
//! W3.3 codegen-emitted static tables can be wrapped in a thin
//! reader closure without coupling the executor to codegen layout.
//! Tests build hand-rolled tables directly.

use super::cursor::{Decision, PathCursor, SegmentKind};
use super::schema::PathSchema;

/// Top-level path-driven parse orchestrator.
///
/// This struct is a marker — every method is associated rather than
/// instance-bound. It exists so consumers see a single entry-point
/// type; future tranches can attach configuration (timeouts, memory
/// limits, profile knobs) here without touching call sites.
pub struct PathExecutor;

impl PathExecutor {
    /// Run a path-driven parse.
    ///
    /// `input` is the source the parse callback consumes. `schema`
    /// is the path the cursor walks. `decision_fn` answers
    /// `(rule_id, segment_kind, segment_index) → Decision`; in
    /// production W3.3 emits this as a static table per grammar and
    /// the per-grammar `parse_with` wraps the table in a closure.
    /// `parse_fn` is the grammar's generated entry-rule recogniser
    /// reformulated to accept a [`PathCursor`].
    ///
    /// Returns `Some(output)` when the parse succeeds and the cursor
    /// reaches its terminal (or returns the leaf at the path's
    /// reach); `None` when the parse bails or the path falls outside
    /// the document's reach.
    pub fn execute<'p, P, F, D>(
        input: &str,
        schema: &'p P,
        decision_fn: D,
        parse_fn: F,
    ) -> Option<P::Output>
    where
        P: PathSchema<'p>,
        D: Fn(u32, SegmentKind, usize) -> Decision + 'p,
        F: FnOnce(&str, &mut PathCursor<'p, P>) -> Option<P::Output>,
    {
        let mut cursor = PathCursor::new(schema, decision_fn);
        parse_fn(input, &mut cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::{OwnedPathSegment, PathSegment, TypedPath};
    use crate::path::markers::Json;

    fn fixture_path<T>() -> TypedPath<Json, T> {
        TypedPath::from_owned(vec![
            OwnedPathSegment::Field("statuses".to_owned()),
            OwnedPathSegment::Index(0),
            OwnedPathSegment::Field("text".to_owned()),
        ])
    }

    fn always_parse_fully(_rule_id: u32, _kind: SegmentKind, _idx: usize) -> Decision {
        Decision::ParseFully
    }

    /// Successful traversal — the parse callback walks the cursor to
    /// its terminal and emits a synthetic leaf string.
    #[test]
    fn executor_returns_leaf_on_success() {
        let path: TypedPath<Json, String> = fixture_path();
        let out = PathExecutor::execute(
            "{\"statuses\":[{\"text\":\"hi\"}]}",
            &path,
            always_parse_fully,
            |_input, cursor| {
                let mut walked = String::new();
                while let Some(seg) = cursor.advance() {
                    match seg {
                        PathSegment::Field(name) => {
                            walked.push_str(name);
                            walked.push('.');
                        }
                        PathSegment::Index(i) => {
                            walked.push_str(&i.to_string());
                            walked.push('.');
                        }
                        _ => {}
                    }
                }
                if cursor.is_terminal() {
                    Some(walked)
                } else {
                    None
                }
            },
        );

        assert_eq!(out.as_deref(), Some("statuses.0.text."));
    }

    /// Path falls past the document's reach — the callback bails by
    /// returning `None`. The executor surfaces it verbatim.
    #[test]
    fn executor_returns_none_on_out_of_bounds() {
        let path: TypedPath<Json, String> = fixture_path();
        let out: Option<String> = PathExecutor::execute(
            "{\"statuses\":[]}",
            &path,
            always_parse_fully,
            |_input, cursor| {
                // Simulate: the parser advances one segment, then
                // discovers the second segment cannot be satisfied
                // (the array is empty) and bails.
                cursor.advance();
                None
            },
        );

        assert!(out.is_none());
    }

    /// The executor threads the decision table into the cursor: a
    /// callback that asks the cursor decisions sees the table's
    /// answers verbatim.
    #[test]
    fn executor_threads_decision_table() {
        let path: TypedPath<Json, Vec<Decision>> = fixture_path();
        let plan = |rule_id: u32, kind: SegmentKind, _idx: usize| match (rule_id, kind) {
            (1, SegmentKind::Field) => Decision::ParseFully,
            (2, SegmentKind::Index) => Decision::ParseUntil(0),
            _ => Decision::Skip,
        };

        let trace: Option<Vec<Decision>> =
            PathExecutor::execute("ignored", &path, plan, |_input, cursor| {
                let mut out = Vec::new();
                out.push(cursor.decide(1));
                cursor.advance();
                out.push(cursor.decide(2));
                cursor.advance();
                out.push(cursor.decide(99));
                Some(out)
            });

        assert_eq!(
            trace.expect("trace"),
            vec![
                Decision::ParseFully,
                Decision::ParseUntil(0),
                Decision::Skip,
            ]
        );
    }
}
