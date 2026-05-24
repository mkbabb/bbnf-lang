# SK-V14 W1A: Comparator Rebind

Date: 2026-05-24.
Scope: JSON parse_only/direct/typed comparator binding for W1 R1.
Output: this file.

## §1 — Findings

- `restart/skinny/tranches/sk-v14/SPEC.md:379` defines W1 as comparator rebind + per-iteration equality + PRUNE-1. The load-bearing R1 lines are `SPEC.md:408-413`: delete the single-lane `sonic_rs_anchor`, bind parse_only to a Skipper-class sonic comparator, direct to per-corpus strict struct deserialization, and typed to per-corpus typed strict deserialization.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` still times `sonic_rs_anchor` as `sonic_rs::from_slice::<sonic_rs::Value>`, with metadata materialisation `eager_typed`; this is strict eager DOM, not parse_only.
- Track 1 and Track 2 are timed in separate closures (`json_parity.rs:43-70`, `181-224`, `261-310`), while parity is asserted before measurement at `json_parity.rs:15-26`. This leaves R2 startup-only.
- `skinny/crates/bbnf-bench/src/report.rs:3666` and `skinny/crates/bbnf-bench/src/bin/gate.rs:2876` already name the intended W1 comparator planes, but the surrounding evidence still uses legacy sources.
- `report.rs:3657` and `gate.rs:2867` mislabel direct/typed Track 2 entry points as sonic comparator paths. Actual Track 2 entry points are `direct_struct::track2_digest` at `skinny/crates/bbnf-bench/src/direct_struct.rs:419` and `real_typed_struct::track2_typed` at `skinny/crates/bbnf-bench/src/real_typed_struct.rs:640`.
- `gate.rs:2760` and `report.rs:5629` still emit parse_only sonic evidence sourced from `sonic_rs_anchor`; W1 must retire that evidence source.

## §2 — Recommendations

- Replace `sonic_rs_anchor` with `sonic_rs_skipper` or equivalent, backed by a local Skipper-class wrapper around public sonic-rs deserialization into `serde::de::IgnoredAny`.
- Keep direct timing as `sonic_rs_direct_to_struct` only if the plan explicitly treats `JsonDirectDigest` as the direct row's strict struct target; otherwise add per-corpus direct target wrappers before claiming `<corpus>::strict_struct_deser`.
- Promote the existing typed sonic switch in `real_typed_struct.rs:690-730` as the typed comparator source for supported typed corpora.
- Fix manifest entry-point fields so Track 1/Track 2 identify generated and independent implementation paths, and comparator evidence identifies the comparator path separately.

## §3 — Risks

- A false parse_only close can recur if any DOM `Value` source remains accepted as the parse_only comparator.
- Direct comparator semantics are not fully aligned with the SPEC wording until the plan decides whether `JsonDirectDigest` is an accepted strict struct target.
- Current validators reject a few bad strings but do not enforce an exact comparator registry or W1 per-iteration equality.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:379-467`
- `skinny/crates/bbnf-bench/benches/json_parity.rs:43-310`
- `skinny/crates/bbnf-bench/src/report.rs:3657-3666`
- `skinny/crates/bbnf-bench/src/bin/gate.rs:2760-2876`
- `skinny/crates/bbnf-bench/src/direct_struct.rs:419-428`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:640-730`
