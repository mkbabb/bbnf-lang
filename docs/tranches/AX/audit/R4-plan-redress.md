# R4 — AX Plan Redress

## Summary

W0a.1–W0a.2.c landed the shape-emission substrate cleanly. W0a.2.d–g
burned five sub-waves chasing **walker-parity of tape records** as
the correctness oracle — every widening attempt reverted. Four
blockers remain open per `post-AX-W0a2g-progress.md`
§Remaining-blockers: Flat Next/Skip flatten, Ref→HRegex Rule wrap,
Repeat `iter_count < lo` leak, zero-length Seq elision. None are
isolated; each is a shape-emission / walker divergence on record
count that downstream consumers do not demand.

The W0a.2.h pivot retires walker-parity: **shape emission is
authoritative; the `*_parity.rs` semantic harnesses
(`json_value_parity`, `bbnf_ast_parity`, `sheets_expr_parity`,
`css_l4_parity`, `sonic_rs_parity`, `lightningcss_parity`) verify
correctness end-to-end.** Walker tape is a historical scaffold;
consumers read through `Root::View` + `TapeCursor`, not raw record
counts. Per R2 §A4, the walker-parity invariant was inherited from a
retrospective that never priced in the scaffold distinction.

The plan needs four surgical edits. No wave gets a full rewrite; the
pivot removes a false constraint, it does not invalidate the thesis.

## P1. Preserved landings (what worked)

| Sub-wave | Commit(s) | Deliverable | Status |
|---|---|---|---|
| W0a.1 | `9f8aed90`, `af8f6840` | `has_w4_classified` narrowed to `Pratt\|Unordered` at `dispatcher.rs:836`; bootstrap regen idempotent | LOAD-BEARING. Re-activates JSON visitor-path; referenced by W1's visitor monomorphisation |
| W0a.2 | `9b1b54e2`, `69d28f56` | `has_shape_dispatcher_entrypoint` BFS-narrowed to docstring intent; `gate_predicate_wire_contract.rs` freezes 7×3 matrix | LOAD-BEARING. Invariant 14's per-grammar anchor; W0b, W2, W12 all read it |
| W0a.2.a | `ee7f81da` | Array emitter split into `emit_parse_array_wrapped` (Shape 1 `"["…"]"`) + `emit_parse_array_list` (Shape 2 entry-list CSS/BBNF) | LOAD-BEARING. Foundational for CSS `stylesheet` + BBNF `grammar` routing; W4.2 and W9's `fork_cut_byte` mining depend |
| W0a.2.b | `517be13c`→`7f3dbafb`, `610928a6` | `ShapeTag::AltDispatch` + fixed-point detector; Flat/Scalar widened; `shapes/alt_dispatch.rs` emitter. 43→0 entry-reachable unclassified Refs | LOAD-BEARING. AltDispatch is W12's extraction target; detector widening makes CSS `value` / BBNF `alternation` classifiable |
| W0a.2.e infra | `8048fb41` | `tape_parity` split into 6 per-grammar binaries; `CARGO_BUILD_JOBS=4` policy | LOAD-BEARING as infrastructure. The 26 GB RSS OOM is permanent operational learning (promote to README per R2 §A5) |
| W0a.2.e correctness | `cee21ddf` | `shapes/inline.rs` per-Ref-branch `Columns::truncate(attempt_len)` on failure | LOAD-BEARING under pivot. Speculative-branch rollback is emitter-internal correctness, NOT walker-parity; committed writes on failed branches corrupt downstream visitor calls |
| W0a.2.f | `9ffe50db`, `4a3b60bf` | `#[inline(always)]`→`#[inline]` on compound shape fns; leaf shape fns retain aggressive inlining | LOAD-BEARING. Breaks LLVM inliner SIGBUS cycle; precondition for W0a.2.h admission widening |
| W0a.2.g | `63845c68`, `8400d6af`, `e9b580f1`, `d48c1108`, `35042584`, `0618dfc3` | Keyword Ref-led Alt + state-threaded sig; inline Alt ByteDispatch vs AltLinear split; Flat Repeat column-truncation + Seq-inner flatten; Array-list `variant=0` + `has_iter_ow` widening | PARTIAL. Keyword, inline Alt split, Flat truncation = correctness (emitter speculative-rollback + compound-count self-consistency). Array-list `variant=0` + `has_iter_ow` are walker-shape matching — SUBSUMED by pivot but harmless; leave in place |

## P2. Retired chase (what to stop doing)

| Sub-wave | Commit(s) | Why retired |
|---|---|---|
| W0a.2.f admission widening | `f6e1ecb5` → reverted `63895dee` | Deleted `body_has_dispatcher_fallback_position`; reverted when BNF 40-vs-34 record divergence surfaced. Under pivot, BNF's 40 records are a different shape-emission projection — not a bug |
| W0a.2.g admission probe | Reverted (uncommitted) | Same predicate deletion; probe surfaced cascading walker-parity deltas across every non-JSON grammar. Under pivot: deletion is W0a.2.h work |
| Record-count parity chasing | W0a.2.d–g shape fixes aligning counts with walker | Walker-parity is a snapshot oracle. Downstream consumers read `Root::View` / `TapeCursor` by `TapeKind` + semantic cursor API |
| W0a.2.d inline substrate | `1e603586` | 1239 LOC substrate landed; consumers reverted (26 GB RSS + SIGBUS). Survived as `pub(crate)`; post-pivot consumers wire in W0a.2.h with shape-emission semantics, not walker-identical emission |

## P3. W0b redress post-pivot

W0b becomes **simpler, not harder.** Three axes:

1. **Deletion is unambiguous.** Pre-pivot: W0b gated on "walker no longer reached from `parse()`." Post-pivot: W0a.2.h closes admission-widened (every grammar's `parse()` tail-calls `parse_<shape>_<grammar>_<entry>`, end of story). W0b opens after W0a.2.h close.

2. **`body_has_dispatcher_fallback_position` retires in W0b, not W0a.2.h.** W0a.2.h closes admission-widened with predicate still present (narrow-but-consistent with shape-emission semantics). W0b retires alongside `has_w4_classified`, `has_full_shape_coverage`, and `has_shape_dispatcher_entrypoint` when universal admission is verified.

3. **Agent count stays 4.** Six agents not supported: Agent A's deletion is serial (walker files reference DTA types; order matters). Agents B/C/D already disjoint (emit/ purge, renames, tests). `tape_parity_*.rs` (6 files) + `tape_parity_common/` retire under pivot (walker-parity harnesses); Agent D absorbs that scope.

**Hard gate redress.** One addition: **gate 10 — `tape_parity_*.rs` (6 files) + `tape_parity_common/mod.rs` deleted; semantic `*_parity.rs` AST-level harnesses survive.**

## P4. W0c redress

- Scope §4 (walker mentions): **augment** with "reframe walker role as historical scaffold retired W0a/W0b; AST-parity harnesses are the forward correctness oracle."
- Add scope §8: §Pivot subsection citing `post-AX-W0a2g-progress.md`.
- Agent count 1 serial unchanged.

## P5. W1–W15 redress

- **W1.** Walker-agnostic at scope line 9. No changes.
- **W2.** Scope SHRINKS. Existing `*_parity.rs` harnesses inherit from W0a; W2's work is corpus expansion (≥ 200 per grammar) + three new comparators (simdjson OnDemand, serde_json, cssparser). No new shape-parity harness (that was `tape_parity_*.rs` — retired). Agent count 3 unchanged.
- **W3.** Ignore-ridden test surface, walker-independent. Unchanged.
- **W4.** Miner inheritance L1/L2 targets shape emitters (walker dies W0b). No walker-shape deps. Unchanged.
- **W5.** ShapeRef consumer uses compile-time shape hashes. Unchanged.
- **W6.** Shape emitter CTNS consumer reads `last_byte_set` mining. Unchanged.
- **W7.** `seek_matching_close` uses W5 kind streams / per-shape bracket-balance. Unchanged.
- **W8.** W8.4 fuzz framing: "tape-identical" means shape-emitter-identical — both runs traverse the same shape emitter. No text edit needed.
- **W9.** `fork_cut_byte` mines from `list_rules.rs`; `TapeBuilder::merge_from` on SoA columns. Unchanged.
- **W10/W11.** E-graph reads IR; extraction produces `ShapeTag`s shape emitter consumes. W11.3 fuzz: both runs are shape-emitter output. Unchanged.
- **W12.** Pre-retirement oracle (W12.2) is the hand-coded detector's output pre-retirement — already pivot-compatible phrasing.
- **W13/W14.** Unchanged.
- **W15.** AY handoff condition 1 (grep zero walker symbols) unchanged. Condition 3 ("`parse()` routes through shape dispatcher") IS the pivot statement. Unchanged.

## P6. Missing wave proposal

**Add `W0a.close`** as a close sub-phase of W0a, BEFORE W0b. The 17-entry bench is at `docs/benchmarks/post-AW-V.json` (pre-AX). W0b deletes ~78,500 LOC including walker; post-W0b deltas will conflate walker-death-cost with subsequent-lever-wins. Pre-W0b baseline lets each downstream wave attribute its delta.

Scope:
- Single agent, serial.
- Deliverable: `docs/benchmarks/archive/post-AX-W0a-close.json` — 17-entry matrix on master immediately post-W0a.2.h close, pre-W0b.
- Hard gate: matrix coverage ≥ 17 entries (JSON × 5, CSS × 3, Sheets × 3, BBNF × 6).
- Mid-wave bench diffs (invariant 10) reference THIS baseline, not post-AW-V.

Insert in AX.md wave summary table between W0a and W0b.

Authoring deferred to restart — SPEC.md §Wave stipulation compliance + bench-target allow-list decisions outside R4's scope.

## P7. Invariant deltas

Three proposed edits to AX.md §Invariants:

**Invariant 11 — retain verbatim.** Pivot strengthens: shape emission IS the generalization mechanism; walker parity is not proof of generality; external parity harnesses are.

**New invariant 20 — Shape-emission authority.** Propose:

> **Tape shape is shape-emission-authoritative.** Downstream correctness is
> asserted by AST-level `*_parity.rs` harnesses (JSON/CSS/Sheets/BBNF
> against sonic-rs / lightningcss / simdjson-OnDemand + self-parity),
> not by record-count or column-layout equivalence against the walker.
> Walker tape is a historical scaffold retired in W0b; the shape
> emitter's own output is the one source of truth for `TapeCursor` +
> `Root::View` consumers.

Insert after invariant 7 (wire-contract end-to-end), before invariant 8 (no legacy code).

**Invariant 18 augmentation.** Append: "Shape-emission output is field-complete — no emitted parse fn emits a placeholder compound awaiting walker-parity later; the shape emitter decides the tape shape, ships it, and downstream consumers project from it." Prose-only clarification.

## Edit manifest

| File | Status | Edits |
|---|---|---|
| `docs/tranches/AX/audit/R4-plan-redress.md` | CREATE | This document |
| `docs/tranches/AX/AX.md` | SURGICAL | Insert invariant 20 after line 30; augment invariant 18 at line 41; add `W0a.close` row in wave summary table |
| `docs/tranches/AX/waves/W0b.md` | SURGICAL | Gate 10 addition; `tape_parity_*.rs` + `tape_parity_common/` to deletion list; hard gate text |
| `docs/tranches/AX/waves/W0c.md` | SURGICAL | Scope §4 augment; add §8 §Pivot subsection scope bullet |
| `docs/tranches/AX/waves/W2.md` | SURGICAL | One-line pivot note at scope opening; semantic parity inheritance clarified |
| `docs/tranches/AX/waves/W0a.md` | DO NOT EDIT | Per prompt; W0a closes with W0a.2.h |
| `W1.md`, `W3.md`–`W15.md` | DO NOT EDIT | No pivot-driven scope shifts |

**Deferred to restart:**
- `W0a.bench.md` wave spec authoring — flagged in §P6; requires SPEC.md compliance + bench-target allow-list outside R4 scope.
- AY.md invariant alignment — follows W15 close, not R4.
