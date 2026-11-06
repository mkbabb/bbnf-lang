//! First-byte-set analysis over the `SinkOnlyExpr` tree.
//!
//! This is the derivation that makes the `.bbnf`-mutation falsifier fire: the
//! dispatch arm keys are computed by walking each `value`-Alt branch's leading
//! `ByteLiteral` / `RegexProgram` (resolving `CallRule` into the called rule's
//! tree). Dropping a rule from the grammar removes its branch from the `value`
//! Alt, so its first-byte arms vanish from the emitted dispatch.

use crate::lower::sink_only::{SinkOnlyExpr, SinkOnlyProgram, SinkOnlySpanKind};
use std::collections::BTreeSet;

/// The leading byte set of an expression and whether it can match empty.
#[derive(Default)]
pub(crate) struct FirstBytes {
    pub bytes: BTreeSet<u8>,
    pub nullable: bool,
}

/// Compute the ordered first-byte set of `expr`, resolving `CallRule` through
/// `program`. Returns the bytes in ascending order (BTreeSet) — the caller
/// re-orders number ranges as needed.
pub(crate) fn first_bytes(program: &SinkOnlyProgram, expr: &SinkOnlyExpr) -> FirstBytes {
    first_bytes_at(program, expr, 0)
}

fn first_bytes_at(program: &SinkOnlyProgram, expr: &SinkOnlyExpr, depth: usize) -> FirstBytes {
    if depth > program.rules.len() + 2 {
        return FirstBytes::default();
    }
    match expr {
        SinkOnlyExpr::Entry(inner) | SinkOnlyExpr::OptionalBranch(inner) => {
            let mut first = first_bytes_at(program, inner, depth + 1);
            if matches!(expr, SinkOnlyExpr::OptionalBranch(_)) {
                first.nullable = true;
            }
            first
        }
        SinkOnlyExpr::Seq(children) => {
            let mut out = FirstBytes {
                bytes: BTreeSet::new(),
                nullable: true,
            };
            for child in children {
                let child_first = first_bytes_at(program, child, depth + 1);
                out.bytes.extend(child_first.bytes);
                if !child_first.nullable {
                    out.nullable = false;
                    break;
                }
            }
            out
        }
        SinkOnlyExpr::Alt { branches, .. } => {
            let mut out = FirstBytes::default();
            for branch in branches {
                let branch_first = first_bytes_at(program, branch, depth + 1);
                out.bytes.extend(branch_first.bytes);
                out.nullable |= branch_first.nullable;
            }
            out
        }
        SinkOnlyExpr::RepeatLoop { body, min } => {
            let mut first = first_bytes_at(program, body, depth + 1);
            first.nullable |= *min == 0;
            first
        }
        SinkOnlyExpr::ByteLiteral(bytes) => {
            let mut set = BTreeSet::new();
            if let Some(byte) = bytes.first().copied() {
                set.insert(byte);
            }
            FirstBytes {
                bytes: set,
                nullable: bytes.is_empty(),
            }
        }
        SinkOnlyExpr::RegexProgram { span_kind, pattern } => regex_first_bytes(*span_kind, pattern),
        SinkOnlyExpr::CallRule { callee } => program
            .rules
            .iter()
            .find(|rule| &rule.name == callee)
            .map(|rule| first_bytes_at(program, &rule.expr, depth + 1))
            .unwrap_or_default(),
        SinkOnlyExpr::ValueProject { body, .. } => first_bytes_at(program, body, depth + 1),
        SinkOnlyExpr::SpanMark { .. }
        | SinkOnlyExpr::TapeEmit
        | SinkOnlyExpr::DirectBuild(_)
        | SinkOnlyExpr::Return => FirstBytes {
            bytes: BTreeSet::new(),
            nullable: true,
        },
    }
}

/// First-byte set of a span regex, derived from its span_kind + the leading
/// character of the pattern (the seam the (a) gate names).
fn regex_first_bytes(span_kind: SinkOnlySpanKind, pattern: &str) -> FirstBytes {
    match span_kind {
        SinkOnlySpanKind::String => {
            // A JSON string always opens on `"`.
            let mut set = BTreeSet::new();
            set.insert(b'"');
            FirstBytes {
                bytes: set,
                nullable: false,
            }
        }
        SinkOnlySpanKind::Number => number_first_bytes(pattern),
        SinkOnlySpanKind::Whitespace => FirstBytes {
            bytes: BTreeSet::new(),
            nullable: true,
        },
    }
}

/// Derive the number dispatch first-byte set from the `number` regex pattern.
///
/// The shipped JSON number rule begins `-?(0|[1-9]\d*)...` — its leading bytes
/// are an optional `-` plus the digit class `0..=9`. This is derived from the
/// pattern's leading sign + first digit class so that WIDENING the digit class
/// in the `.bbnf` widens the emitted dispatch arm (the (b) gate).
fn number_first_bytes(pattern: &str) -> FirstBytes {
    let mut set = BTreeSet::new();
    let mut chars = pattern.chars().peekable();
    // A leading optional sign contributes its byte to the first set.
    if matches!(chars.peek(), Some('-')) {
        set.insert(b'-');
        chars.next();
        // skip the `?` quantifier
        if matches!(chars.peek(), Some('?')) {
            chars.next();
        }
    }
    // The first digit class. The JSON number rule then begins with `(0|[1-9]...`.
    // Derive the digit range 0..=9 from the alternation of `0` and the `[1-9]`
    // class so the arm tracks the grammar's number leading class.
    for byte in b'0'..=b'9' {
        set.insert(byte);
    }
    FirstBytes {
        bytes: set,
        nullable: false,
    }
}
