//! Tranche AF.3 Wave 5 — `EmissionTier` lattice contract.
//!
//! Pins the algebraic invariants of the Tranche AF.4
//! [`EmissionTier`](bbnf_ir::passes::EmissionTier) lattice — the
//! orthogonal axis alongside `MaterializationClass` that decides what
//! return type a rule's parse function projects to (tape-only,
//! direct-to-struct, or lazy view-layer hybrid).
//!
//! The lattice is totally ordered `Direct < Lazy < Tape`:
//!
//! - `top() = Tape` — universal, always-legal.
//! - `bottom() = Direct` — tightest-constraint, requires FixedShape.
//! - `tier_join` widens toward `Tape` (the absorbing top).
//! - `tier_meet` narrows toward `Direct` (the absorbing bottom).
//!
//! These tests depend ONLY on the committed AF.4 lattice, not on the
//! AF.3 cross-rule CSP solve. They activate at the moment the enum is
//! committed and run on every `cargo test -p bbnf-ir --test
//! emission_tier_lattice` invocation.

use bbnf_ir::passes::materialization::{EmissionTier, tier_join, tier_meet};

/// Every variant of the lattice — the enumeration that powers the
/// exhaustive property checks below. Keep this list in sync with the
/// `EmissionTier` enum; a missing variant silently weakens every
/// property test in this file.
const ALL_TIERS: [EmissionTier; 3] =
    [EmissionTier::Direct, EmissionTier::Lazy, EmissionTier::Tape];

// ── Bounds (top / bottom) ────────────────────────────────────────────

/// The top of the lattice is `Tape` — the universal, always-legal
/// tier. Every rule in every grammar can fall back to `Tape`; the
/// CSP only moves down the lattice when a rule's materialization
/// class and parent-compatibility constraints permit.
#[test]
fn top_is_tape() {
    assert_eq!(EmissionTier::top(), EmissionTier::Tape);
}

/// The bottom of the lattice is `Direct` — the tightest-constraint
/// tier. Eligibility requires a `FixedShape` materialization class,
/// a closure-free body, and a single-site consumer model.
#[test]
fn bottom_is_direct() {
    assert_eq!(EmissionTier::bottom(), EmissionTier::Direct);
}

/// `top` and `bottom` are NEVER equal — the lattice has at least two
/// distinct elements. This pins the shape against a degenerate single-
/// element lattice.
#[test]
fn top_and_bottom_distinct() {
    assert_ne!(EmissionTier::top(), EmissionTier::bottom());
}

// ── Rank total order ─────────────────────────────────────────────────

/// The ranks give the total order `Direct < Lazy < Tape`. The rank
/// function is the engine that drives `tier_join` / `tier_meet` — if
/// any variant's rank drifts, both the join and meet contract break
/// in lockstep, so pin all three here.
#[test]
fn rank_order() {
    assert!(EmissionTier::Direct.rank() < EmissionTier::Lazy.rank());
    assert!(EmissionTier::Lazy.rank() < EmissionTier::Tape.rank());
}

/// Ranks are dense at the bottom — `Direct` is rank zero. This is a
/// load-bearing invariant for any caller that uses `rank() == 0` as a
/// "narrowest tier" test.
#[test]
fn direct_rank_is_zero() {
    assert_eq!(EmissionTier::Direct.rank(), 0);
}

/// Ranks match the documented values — `Direct=0, Lazy=1, Tape=2`.
/// This pins the absolute numerical ranks so any caller that reads
/// `rank()` as a numeric tier identifier stays correct.
#[test]
fn rank_values_are_stable() {
    assert_eq!(EmissionTier::Direct.rank(), 0);
    assert_eq!(EmissionTier::Lazy.rank(), 1);
    assert_eq!(EmissionTier::Tape.rank(), 2);
}

/// Ranks are pairwise distinct — no two variants collide. A collision
/// would silently merge the tiers under join/meet and make the
/// lattice degenerate.
#[test]
fn ranks_are_pairwise_distinct() {
    for (i, a) in ALL_TIERS.iter().enumerate() {
        for b in ALL_TIERS.iter().skip(i + 1) {
            assert_ne!(
                a.rank(),
                b.rank(),
                "ranks collide: {:?} vs {:?}",
                a,
                b
            );
        }
    }
}

// ── Join (widen to Tape) ─────────────────────────────────────────────

/// `tier_join` is idempotent: joining a tier with itself is a no-op.
/// `join(a, a) == a` for every variant.
#[test]
fn join_idempotent() {
    for &a in &ALL_TIERS {
        assert_eq!(tier_join(a, a), a, "join not idempotent at {:?}", a);
    }
}

/// `tier_join` is commutative: the order of arguments does not
/// matter. Every pair `(a, b)` in the variant set obeys
/// `join(a, b) == join(b, a)`.
#[test]
fn join_commutative() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            assert_eq!(
                tier_join(a, b),
                tier_join(b, a),
                "join not commutative: {:?}, {:?}",
                a,
                b
            );
        }
    }
}

/// `tier_join` is associative: parenthesization does not matter.
/// `join(join(a, b), c) == join(a, join(b, c))` for every triple.
#[test]
fn join_associative() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            for &c in &ALL_TIERS {
                assert_eq!(
                    tier_join(tier_join(a, b), c),
                    tier_join(a, tier_join(b, c)),
                    "join not associative: {:?}, {:?}, {:?}",
                    a,
                    b,
                    c
                );
            }
        }
    }
}

/// `tier_join` absorbs toward `Tape` — the universal top. Joining
/// anything with `Tape` yields `Tape`. This is what makes `Tape` the
/// absorbing element of the lattice.
#[test]
fn join_monotone_toward_tape() {
    for &a in &ALL_TIERS {
        assert_eq!(
            tier_join(a, EmissionTier::Tape),
            EmissionTier::Tape,
            "join absorbed to Tape failed at {:?}",
            a
        );
        assert_eq!(
            tier_join(EmissionTier::Tape, a),
            EmissionTier::Tape,
            "join absorbed to Tape failed at {:?} (swapped)",
            a
        );
    }
}

/// `Direct` is the identity element for `tier_join` — joining
/// anything with `Direct` leaves it unchanged. Equivalently, `Direct`
/// is the bottom of the lattice and every other tier dominates it.
#[test]
fn join_neutral_with_bottom() {
    for &a in &ALL_TIERS {
        assert_eq!(
            tier_join(a, EmissionTier::Direct),
            a,
            "join with Direct should be identity at {:?}",
            a
        );
        assert_eq!(
            tier_join(EmissionTier::Direct, a),
            a,
            "join with Direct should be identity at {:?} (swapped)",
            a
        );
    }
}

/// `tier_join` is monotone with respect to rank: the result always
/// has rank at least as large as each input. This is the fundamental
/// property that makes the join a legitimate lattice operation.
#[test]
fn join_is_rank_monotone() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            let joined = tier_join(a, b);
            assert!(
                joined.rank() >= a.rank(),
                "join dropped below a: {:?} vs {:?}",
                joined,
                a
            );
            assert!(
                joined.rank() >= b.rank(),
                "join dropped below b: {:?} vs {:?}",
                joined,
                b
            );
        }
    }
}

/// `tier_join` always picks the max-rank input. This is the direct
/// algorithmic contract on top of the lattice semantics.
#[test]
fn join_picks_max_rank() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            let joined = tier_join(a, b);
            let max_rank = a.rank().max(b.rank());
            assert_eq!(
                joined.rank(),
                max_rank,
                "join did not pick max rank at {:?}, {:?}",
                a,
                b
            );
        }
    }
}

// ── Meet (narrow to Direct) ──────────────────────────────────────────

/// `tier_meet` is idempotent: meeting a tier with itself is a no-op.
/// `meet(a, a) == a` for every variant.
#[test]
fn meet_idempotent() {
    for &a in &ALL_TIERS {
        assert_eq!(tier_meet(a, a), a, "meet not idempotent at {:?}", a);
    }
}

/// `tier_meet` is commutative — the order of arguments does not
/// matter. Every pair obeys `meet(a, b) == meet(b, a)`.
#[test]
fn meet_commutative() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            assert_eq!(
                tier_meet(a, b),
                tier_meet(b, a),
                "meet not commutative: {:?}, {:?}",
                a,
                b
            );
        }
    }
}

/// `tier_meet` is associative — parenthesization does not matter.
/// `meet(meet(a, b), c) == meet(a, meet(b, c))` for every triple.
#[test]
fn meet_associative() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            for &c in &ALL_TIERS {
                assert_eq!(
                    tier_meet(tier_meet(a, b), c),
                    tier_meet(a, tier_meet(b, c)),
                    "meet not associative: {:?}, {:?}, {:?}",
                    a,
                    b,
                    c
                );
            }
        }
    }
}

/// `tier_meet` absorbs toward `Direct` — the lattice bottom. Meeting
/// anything with `Direct` yields `Direct`.
#[test]
fn meet_monotone_toward_direct() {
    for &a in &ALL_TIERS {
        assert_eq!(
            tier_meet(a, EmissionTier::Direct),
            EmissionTier::Direct,
            "meet absorbed to Direct failed at {:?}",
            a
        );
        assert_eq!(
            tier_meet(EmissionTier::Direct, a),
            EmissionTier::Direct,
            "meet absorbed to Direct failed at {:?} (swapped)",
            a
        );
    }
}

/// `Tape` is the identity element for `tier_meet` — meeting anything
/// with `Tape` leaves it unchanged. Equivalently, `Tape` is the top
/// of the lattice and every other tier is dominated by it.
#[test]
fn meet_neutral_with_top() {
    for &a in &ALL_TIERS {
        assert_eq!(
            tier_meet(a, EmissionTier::Tape),
            a,
            "meet with Tape should be identity at {:?}",
            a
        );
        assert_eq!(
            tier_meet(EmissionTier::Tape, a),
            a,
            "meet with Tape should be identity at {:?} (swapped)",
            a
        );
    }
}

/// `tier_meet` is monotone with respect to rank: the result always
/// has rank at most as large as each input. Mirror of the join
/// rank-monotonicity property.
#[test]
fn meet_is_rank_monotone() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            let met = tier_meet(a, b);
            assert!(
                met.rank() <= a.rank(),
                "meet rose above a: {:?} vs {:?}",
                met,
                a
            );
            assert!(
                met.rank() <= b.rank(),
                "meet rose above b: {:?} vs {:?}",
                met,
                b
            );
        }
    }
}

/// `tier_meet` always picks the min-rank input.
#[test]
fn meet_picks_min_rank() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            let met = tier_meet(a, b);
            let min_rank = a.rank().min(b.rank());
            assert_eq!(
                met.rank(),
                min_rank,
                "meet did not pick min rank at {:?}, {:?}",
                a,
                b
            );
        }
    }
}

// ── Absorption (join / meet interplay) ───────────────────────────────

/// The absorption laws connect `tier_join` and `tier_meet`:
///
/// - `join(a, meet(a, b)) == a`
/// - `meet(a, join(a, b)) == a`
///
/// These pin the two ops as a consistent algebraic pair — not just
/// independent monotone functions. Any lattice that satisfies the
/// pairwise distributive properties also satisfies absorption.
#[test]
fn join_meet_absorption() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            assert_eq!(
                tier_join(a, tier_meet(a, b)),
                a,
                "join-of-meet absorption failed: {:?}, {:?}",
                a,
                b
            );
            assert_eq!(
                tier_meet(a, tier_join(a, b)),
                a,
                "meet-of-join absorption failed: {:?}, {:?}",
                a,
                b
            );
        }
    }
}

/// The distributive laws — stronger than absorption. A totally
/// ordered lattice is always distributive, so this is a natural
/// consequence of the `Direct < Lazy < Tape` chain shape.
#[test]
fn join_meet_distributive() {
    for &a in &ALL_TIERS {
        for &b in &ALL_TIERS {
            for &c in &ALL_TIERS {
                // join distributes over meet
                assert_eq!(
                    tier_join(a, tier_meet(b, c)),
                    tier_meet(tier_join(a, b), tier_join(a, c)),
                    "join does not distribute over meet: {:?}, {:?}, {:?}",
                    a,
                    b,
                    c
                );
                // meet distributes over join
                assert_eq!(
                    tier_meet(a, tier_join(b, c)),
                    tier_join(tier_meet(a, b), tier_meet(a, c)),
                    "meet does not distribute over join: {:?}, {:?}, {:?}",
                    a,
                    b,
                    c
                );
            }
        }
    }
}

// ── Shape accessors ──────────────────────────────────────────────────

/// `emits_tape()` is the predicate read by AF.6's rule emitter to
/// decide whether to generate the tape push function. `Tape` and
/// `Lazy` both emit the tape function (Lazy uses it for the walk-on-
/// demand path); only `Direct` skips it.
#[test]
fn emits_tape_only_direct_skips() {
    assert!(EmissionTier::Tape.emits_tape());
    assert!(EmissionTier::Lazy.emits_tape());
    assert!(!EmissionTier::Direct.emits_tape());
}

/// `emits_direct_shim()` decides whether a rule emits the secondary
/// `__<rule>_direct` function. `Direct` and `Lazy` both emit the shim
/// (Lazy uses it for cache-hit fast path); only `Tape` skips it.
#[test]
fn emits_direct_shim_lazy_direct_yes_tape_no() {
    assert!(!EmissionTier::Tape.emits_direct_shim());
    assert!(EmissionTier::Lazy.emits_direct_shim());
    assert!(EmissionTier::Direct.emits_direct_shim());
}

/// Every non-`Direct` tier emits the tape record — the contrapositive
/// of `emits_tape_only_direct_skips`. Pins the invariant "`Direct`
/// is the only tier that breaks the tape push contract".
#[test]
fn all_non_direct_emit_tape() {
    for &t in &ALL_TIERS {
        if t != EmissionTier::Direct {
            assert!(
                t.emits_tape(),
                "non-Direct tier {:?} must emit tape",
                t
            );
        }
    }
}

/// Every non-`Tape` tier emits the direct shim — the contrapositive
/// of the Tape-only-skips property. Pins "`Tape` is the only tier
/// that skips the direct shim".
#[test]
fn all_non_tape_emit_direct_shim() {
    for &t in &ALL_TIERS {
        if t != EmissionTier::Tape {
            assert!(
                t.emits_direct_shim(),
                "non-Tape tier {:?} must emit direct shim",
                t
            );
        }
    }
}

/// `Lazy` is the only tier that emits BOTH the tape push and the
/// direct shim — it is the hybrid view-layer tier. This is the
/// defining property of Tier C.
#[test]
fn lazy_emits_both_tape_and_direct_shim() {
    assert!(EmissionTier::Lazy.emits_tape());
    assert!(EmissionTier::Lazy.emits_direct_shim());
}

// ── Default + labels ─────────────────────────────────────────────────

/// `EmissionTier::default()` is `Tape` — the universal, always-legal
/// top. This is load-bearing: every test fixture and every
/// `HashMap::default`-initialized `ir.emission_tier` entry starts at
/// `Tape` and only narrows after the AF.5 decoder runs.
#[test]
fn default_is_tape() {
    assert_eq!(EmissionTier::default(), EmissionTier::Tape);
    assert_eq!(EmissionTier::default(), EmissionTier::top());
}

/// `as_str()` labels are stable — they appear in diagnostics, CSP
/// reports, and debug output. Any rename here is a user-visible
/// breaking change.
#[test]
fn as_str_labels_are_stable() {
    assert_eq!(EmissionTier::Tape.as_str(), "tape");
    assert_eq!(EmissionTier::Lazy.as_str(), "lazy");
    assert_eq!(EmissionTier::Direct.as_str(), "direct");
}

/// `as_str()` is injective — no two tiers share a label. A collision
/// would produce ambiguous diagnostics.
#[test]
fn as_str_labels_are_distinct() {
    for (i, a) in ALL_TIERS.iter().enumerate() {
        for b in ALL_TIERS.iter().skip(i + 1) {
            assert_ne!(
                a.as_str(),
                b.as_str(),
                "as_str collision: {:?} vs {:?}",
                a,
                b
            );
        }
    }
}
