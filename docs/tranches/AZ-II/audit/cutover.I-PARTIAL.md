# cutover.I — PARTIAL CLOSE

**Status**: PARTIAL — Phase 5 LANDED; Phase 2 substrate authored but BLOCKED by wave-scale regressions; Phase 3 / 4 / 6 cap-deferred.
**Date**: 2026-04-28
**Master HEAD**: `98008086`

**2026-04-29 supersession note**: this file is historical evidence for
the cutover.I halt. Later cutover.K/L/M closed much of the Phase 2
fleet activation path, and the live remaining work is now cutover.O in
`../waves/cutover.md`, beginning with builder transactions before EBNF
activation. Do not dispatch from this file's "Next-tranche scope"
section.

cutover.I closed AZ-II's Phase 5 deferral (BBNF compact-source serializer + bbnf_rule un-ignore) and discovered two wave-scale blockers that gate Phase 2's full-fleet StructDirect activation. The cutover.I.2 emitter substrate (`shapes/transparent.rs`) was authored in a worktree but did NOT land — its activation requires the blockers below to close first.

## Phase 5 — LANDED

`crates/core/src/runtime/bbnf/serialize.rs` (~430 LOC) authors `serialize_compact_doc(doc: &BbnfDocument<'_>) -> String` — a typed walker over `BbnfDocument` keyed on `BbnfCompoundKind`. Each compound shape (Rule, Alternation, Concatenation, MappedFactor, Closure, ImportDirective, PrettyDirective, …) emits its required structural literals (`=`, `;`, `,`, `|`, `->`, `from`, `@import`, …) in grammar order. Borrowed Span leaves emit verbatim; non-Span literals are injected by the walker per the rule's grammar shape.

`crates/core/tests/serialize_roundtrip.rs::bbnf_rule` un-ignored. Round-trip idempotence asserted: `parse → serialize → reparse → serialize` reproduces the first emission byte-for-byte. 19/19 serialize_roundtrip tests pass; 1/1 bbnf_bootstrap_reproducibility passes (CI gate intact).

Commit: `98008086 feat(cutover.I.5): BbnfBootstrap serialize_compact_doc + un-ignore bbnf_rule`

## Phase 2 — SUBSTRATE AUTHORED BUT UN-LANDED

The plan: extend the cutover.H Phase 1 transparent-rule emission (Wrap-classified only) to ALL classified shapes (Object / Array / Keyword / Number / Pratt / Unordered / ArgList / Flat / HRegex). The substrate at `crates/core/src/backend/rust/emitter/shapes/transparent.rs` (~430 LOC) emits per-shape transparent passthrough fns for every `ShapeTag`, with the right per-shape signature (Keyword's `first_byte`, Number's stripped `state`, String's `is_key`).

The activation also requires resolver-arm flips for csv / math / bnf / ebnf / css_pretty in `crates/ir/src/registry/strategy.rs`.

### Blocker 1 — JSON regen reproducibility break (pre-existing)

eprintln-instrumentation of `keyword/struct_direct.rs::rule_type_desc` confirms:

```
[diag] rule_type_desc(bool_lit) = Some(Span)
[diag] rule_type_desc(null) = Some(Span)
[diag] rule_type_desc(bool) = Some(Span)
```

The `bool` rule (declared in `grammar/json/json.bbnf` as `bool = "true" -> true | "false" -> false ;`) should resolve to `TypeDesc::Bool` per its `-> true` / `-> false` annotations. The current regen pipeline produces `TypeDesc::Span` instead, causing the keyword struct_direct emitter to emit `builder.push_leaf_with_unit()` for both branches — runtime then admits both `"true"` and `"false"` as null/unit, and `JsonValue::Bool` is never constructed.

The committed `crates/core/src/grammar/generated/json.rs` (last regen at commit `af1c5b13`, AZ-I.W2-act flip) has the correct `push_leaf_with_bool(...)` emission. Subsequent regens silently diverge. The `bbnf_bootstrap_reproducibility` CI gate covers BBNF only; JSON / CSS L4 / Sheets / CSS pretty have committed output but no enforcement.

Diagnosing the bool-type loss requires walking the AST → IR → types pipeline; the gate-shape suggests a regression in `crates/ir/src/passes/types/` between `af1c5b13` and HEAD. Outside cutover.I.2's scope.

### Blocker 2 — open-compound leak on per-shape `Err` paths

Activating csv / math / bnf / ebnf / css_pretty fleet to StructDirect surfaces panics:

```
thread 'bnf_rule' panicked at crates/core/src/runtime/bnf/builder.rs:54:9:
BnfStructBuilder::finalise called with 7 open frame(s)
```

The flat / array / object struct_direct emitters (`shapes/flat/struct_direct.rs:454`, etc.) emit `return ::core::result::Result::Err(...)` on inner-literal mismatches WITHOUT closing the open compound (the surrounding `begin_compound` precedes the first `Err` path; no `end_compound` runs).

The currently-active StructDirect grammars (JSON / Sheets / CSS L4 / BBNF) do not trip this because their top-level shapes recover from inner errors before the leak escapes the rule boundary; the new fleet's recursive grammars (BNF's `<expr> ::= <term> | <expr> "+" <term>`, CSV's `escaped = DQUOTE >> /[^"]*/ << DQUOTE`) bubble inner errors up and trip `StructBuilder::finalise`'s open-frame assertion.

The fix is a per-shape `Err`-cleanup audit: every `return Err(...)` inside a `begin_compound` ... `end_compound` pair must close the open frame before returning. Outside cutover.I.2's scope; routes to follow-on tranche.

## Phase 3 / 4 — CAP-DEFERRED

`Parsed<R>` deletion (Option B): 42 cross-crate references (re-counted via `rg "Parsed<" crates/`; the brief's 126 figure was over-counted). `Parsed<R>` carries `Tape<R>` — its deletion is gated on Phase 2 (every grammar must reach StructDirect first). Wave-scale.

`crates/tape/` deletion: 13874 references workspace-wide. Gated on Phase 2 + Phase 3 closing first.

## Phase 6 — CAP-DEFERRED

17-entry bench refresh. Bench compile under fat-LTO is >10 min; the post-cutover-I state's bench delta is meaningful only after Phase 2 closes (the new struct-direct grammars' parse path is what we'd be measuring). Routes to follow-on tranche after Phase 2.

## What landed

Single commit:
- `98008086` — `feat(cutover.I.5): BbnfBootstrap serialize_compact_doc + un-ignore bbnf_rule`

Files touched:
- `crates/core/src/runtime/bbnf/serialize.rs` (NEW, ~430 LOC)
- `crates/core/src/runtime/bbnf/mod.rs` (export)
- `crates/core/tests/serialize_roundtrip.rs` (un-ignore + route)

Decay reclaim: 0 LOC (Phase 5 is additive — substrate authored, no deletions).

Workspace test posture: 19/19 serialize_roundtrip + 1/1 bbnf_bootstrap_reproducibility green. The cutover.H +16 net-improved test count carries forward; cutover.I.5 adds +1 (bbnf_rule un-ignored).

## Hard-gate ledger updates per `waves/cutover.md` §Hard gate

| # | Gate | cutover.I.5 status | Notes |
|---|---|---|---|
| 1 | `crates/tape/` deleted | DEFERRED | Unchanged from cutover.H. Routes to follow-on tranche. |
| 2 | Stage A / B byte-equal across BBNF | MET | `bbnf_bootstrap_reproducibility` gate intact. |
| 3 | IR audit pass reports 100% `->` coverage fleet-wide | NOT VERIFIED | Phase 2 gating. |
| 4 | `StructRegistry` non-empty for every Named rule | MET | Unchanged. |
| 5 | Parity harnesses recoded to struct-vs-external | MET | Unchanged. |
| 6 | 17-entry matrix at AU floor | DEFERRED | Phase 6 cap-deferred. |
| 7 | AZ-II FINAL.md + post-AZ-II.json on master | MET | FINAL.md updated at cutover.I.5; bench archive carries cutover.E placeholder. |
| 8 | Decay sweep | PARTIAL | Unchanged from cutover.H (no decay reclaim under cutover.I). |

## BA handoff verification per AZ-II.md §Handoff contract

| # | Point | cutover.I.5 status | Notes |
|---|---|---|---|
| 1 | All four grammars on direct-to-struct | PARTIAL | Unchanged. JSON / CSS L4 / Sheets / BBNF active; new fleet (csv/math/bnf/ebnf/css_pretty) BLOCKED on Phase 2 blockers. |
| 2 | `crates/tape/` deleted | DEFERRED | |
| 3 | StructRegistry closed fleet-wide | MET | |
| 4 | Parity harnesses on struct comparisons | MET | |
| 5 | 17-entry matrix at AU parity | DEFERRED | |
| 6 | BBNF self-parse byte-reproducible | MET | Plus Phase 5: BBNF compact-source idempotent under serialize_compact_doc. |
| 7 | Parent-pointer decision surface for BA.W0 | DEFERRED | Post-tape-deletion. |

## Next-tranche scope

1. Diagnose JSON regen `bool` TypeDesc loss — walk AST → IR → types pipeline; track regression between `af1c5b13` and HEAD.
2. Per-shape `Err`-cleanup audit on flat / array / object struct_direct emitters — every return-Err inside a begin_compound/end_compound pair must close the frame.
3. Land cutover.I.2 substrate (`shapes/transparent.rs`) once Blockers 1+2 close.
4. `Parsed<R>` deletion (Option B) post-Phase 2 close.
5. `crates/tape/` deletion post-Phase 3 close.
6. 17-entry bench refresh + close-matrix archive.

The cutover.I.5 landing is a productive partial close: Phase 5's deferred deliverable (BBNF compact-source serializer) is in master; the cutover.I.2 substrate is authored and worktree-resident pending Blockers 1+2 closure. The bootstrap reproducibility CI gate remains green; net workspace test posture improves by +1.
