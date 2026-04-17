# AW-III R6 — Path B: Abandon the DTA

**Thesis**: the DTA `dispatch_one` 24% self-time floor is
architectural, not amortisable. Restore the fn-per-rule emitter,
layered with a grammar-parameterised stage-1 SIMD structural bitmap
and codegen-time fused SoA tape writes. The 400+ commits of AW-I/II
substrate are not wasted — they are reframed from "parse driver" to
"substrate that survives the surgery" (payload layouts, Columns SoA,
GrammarProfile, SHAPE_DICT, PSI rayon, snapshot/decision-log hooks).

## 1. The case against DTA

- `dispatch_one` holds **20-35% self-time across every grammar and
  every input size** (SYNTHESIS §regression). No AW-IV lever touches
  the tagged-union match over 20+ `DtaState` variants; it is the
  canonical state-machine-interpreter ceiling.
- Sheets: full W5 scope (Pratt + scanner closure + ShapeRef) leaves
  **~13 MB/s vs post-AU 121** — 8-9× residual (perf-03-sheets §8).
  Path A's own authors write: *"`dispatch_one`'s tagged-union match
  is inherent to the walker. No lever directly flattens it."*
- AW-III declares the escape clause: *"AW-III declares DTA
  non-viable and escalates an architecture decision back to the
  user"* (AW-III.md §scope triad). That clause was written because
  the authors foresaw this.
- arch-comparison's projected Sheets post-AW-IV is **46 MB/s**, 2.6×
  behind post-AU — admitted by Path A.

The user's framing is correct: the DTA as shaped today is starkly
behind RD, and AW-IV's inventory cannot close the gap.

## 2. Path B defined

### Delete

| File | LOC | Fate |
|------|----:|------|
| `crates/bbnf-tape/src/driver.rs` | 1,777 | Delete |
| `crates/bbnf-tape/src/dta.rs` | 355 | Delete |
| `crates/core/src/backend/rust/emitter/dta.rs` | ~600 | Delete |
| `crates/ir/src/passes/recognizers/dta.rs` lifter | ~900 | Delete (keep Pratt, Alt mining, payload inference passes) |
| **Total deleted** | **~3,600** | |

### Kept (reframed from parse-driver to substrate)

- `crates/bbnf-tape/src/{columns,builder,finaliser,cursor,kind,tape,shape_dict,profile}.rs` — SoA Columns consumed by the stage-2 emitter and view layer.
- `crates/bbnf-tape/src/psi.rs` — rayon payload-fill post-pass, unchanged.
- `GrammarProfile`, `SHAPE_DICT`, payload layouts, `PayloadData` arena — untouched.
- All AW-I/II correctness work (payload wiring, type inference, materialization passes) — untouched; it feeds the new emitter.

### Emit fresh, per grammar

1. **Stage-1 SIMD structural-bitmap kernel** (`parse_stage1_<grammar>`):
   per-grammar codegen reads the structural alphabet from the `.bbnf`
   file; emits `#[target_feature(enable = "avx2")]` producing a
   64-bit bit per 64-byte block via `_mm256_cmpeq_epi8` + OR-reduce +
   `_pdep_u64`. NEON fallback via `vceqq_u8`. Output: `Vec<u32>` of
   structural positions.
2. **Stage-2 fn-per-rule emitter** (`parse_<grammar>`):
   `emitter/grammar.rs` re-emits `fn parse_<rule>(state, builder)`
   per rule — pre-AW-I shape — but with codegen-time **fused SoA
   tape writes** (one bounds-check + seven unchecked stores against
   Columns).
3. **Inline codegen specialisation**: stage-2 inlines ShapeRef probe,
   PHF keyword probe, ClassifyByte LUT, Pratt reducer — all as
   const-folded call-site data, not runtime indirection. LLVM inlines
   through the call graph; there is no `dispatch_one` because there
   is no interpreter.

## 3. Path B gains over Path A

1. **No dispatch_one ceiling.** `parse_value` is a tight
   `match peek_byte()`; LLVM inlines through. The 24% floor is gone
   because the interpreter is gone.
2. **Less code.** ~3,600 LOC deleted, ~1,500-2,000 LOC emitted.
3. **Hand-tuned-quality codegen.** arch-comparison §4 admits it:
   *"LLVM inlines aggressively. Each function is a tight loop over
   input bytes."* The emitter's job is to replicate sonic-rs's
   hand-tuned `parse_object`/`parse_array` body shape *per grammar*,
   driven by the IR.
4. **Per-grammar tuning is local.** Tweaking JSON's loop doesn't
   have to fit a multi-grammar wire contract.
5. **Direct lineage to sonic-rs/simdjson.** Stage 1 is literally
   what simdjson does (arch-comparison §1). Stage 2 is sonic-rs's
   tape builder. We stop approximating them through an interpreter.

## 4. Path B losses vs Path A

1. **400+ commits of substrate need re-narration** — not deletion.
   The Columns SoA layout, PayloadData arena, GrammarProfile
   constants, type-inference passes, SHAPE_DICT mining — every one
   becomes input to the fn-per-rule emitter. Only `dta.rs` +
   `driver.rs` get deleted.
2. **"Uniform shape" motivation** is gone. That was aesthetic; it
   never paid its performance rent.
3. **Replay/recovery/incremental-reparse** lose the DTA-snapshot
   freebie. Re-architect onto RD call-stack snapshots + structural-
   index checkpoints (§6).
4. **Six tranches (AS-AW) need re-narrating** as "substrate
   tranches; AW-III' is the parse-driver tranche."

## 5. Can emitted RD beat sonic-rs?

**Yes, conditionally.** sonic-rs's hand-tuned advantage is the body
shape of `parse_object`/`parse_array` — inlined Rust LLVM vectorises.
The emitter **has the IR**; it knows the grammar. Emitting
`fn parse_json_object(state, builder)` with sonic-shape is
template-emit — we already emit 21,000 lines of generated.rs per
grammar; this is picking the right templates.

The IR carries exactly what's needed: structural alphabet (stage 1),
Alt FIRST-sets (byte dispatch), keyword shape (PHF), operator
precedence (Pratt), payload types (fused writes).

Edge over hand-tuning: we emit **all four grammars** from one
emitter. sonic-rs only parses JSON; lightningcss only CSS. Universal
optimisations (PHF frequency ordering, ShapeRef dedup) amortise
across grammars; the hand-tuned competitors re-hand-tune per target.

**Sheets/BBNF**: no upstream comparator exists. Sheets's deep
operator chains lift to the Pratt reducer (mined from the IR;
already works on Sheets per perf-06 §2). BBNF's structural alphabet
is dense (`;=|,()[]{}@`) — stage-1 SIMD payoff is high. Both use
the same stage-1 + stage-2 architecture; no grammar-specific
machinery needed.

## 6. AX impact

Path B requires AX re-architecture. It does **not block AX**.

- **Decision-log replay**: the RD call stack *is* a decision log.
  Instrument the emitter to optionally emit `record_decision(rule_id,
  branch_idx)` at each Alt; replay re-walks by *calling* functions
  with recorded hints. No DtaSnapshot; the log is a call trace.
- **Snapshot**: the stage-1 structural index *is* the checkpoint
  substrate. Every top-level list item (JSON array comma, CSS
  ruleset, BBNF rule, Sheets formula line) is an index entry.
  Snapshot = `(offset, columns_len, frame_depth)` at a checkpoint.
  Resume: call `parse_<rule>(state_from_snapshot, builder)`.
- **Incremental reparse**: tree-sitter is the model — and
  tree-sitter is **RD-based**. AX.2's thesis (locate covering record
  via span binary search → snapshot at prior checkpoint → re-call
  from that offset → splice Columns) still holds. The DtaSnapshot
  plumbing becomes RD-checkpoint plumbing. Same cost.
- **Recovery**: structural-default recovery walks the frame stack
  upward for a rule with a sync byte, skips, resumes. RD has a
  frame stack (the Rust call stack); tree-sitter already models
  this.

AX loses ~200 LOC of "DtaSnapshot serde" and gains ~200 LOC of
"checkpoint-entry serde + function-resume entry points." Net: zero.

## 7. The riskiest Path-A hypothesis, confronted

*"The DTA work was right; we just haven't activated the levers yet."*

**Counter**: SYNTHESIS.md — written by Path A's authors — says full
W5 leaves Sheets at 8-9× residual; `dispatch_one` is not a lever
target. W5.6 "codegen-specialised walkers" was added **after**
SYNTHESIS as a last-resort escape; arch-comparison admits it is the
only thing that could close the gap. W5.6 emits `dta_run_<grammar>`
with inlined arms. The difference between "inline the DtaState match
per grammar" (W5.6) and "emit a function per rule" (Path B) is
semantic, not architectural. **Path A's W5.6 is admitting Path B; it
keeps the DTA branding for continuity.**

If W5.6 is the right answer, Path B is the cleaner formulation: skip
the tagged union, skip the `dta_run` shell, emit honest RD.

## 8. Path B wave plan

Approximately 4-5 tranches — substantial, bounded. Per-wave bench
checkpoint per AW discipline.

| Tranche | Scope | Bench gate |
|---------|-------|-----------|
| **AW-III'** | Stage-1 SIMD kernel (per-grammar); fn-per-rule emitter skeleton with fused SoA writes; JSON end-to-end. | JSON twitter ≥ 2500 MB/s (exceed post-AU 1967); sonic-parity ratio ≥ 0.85 |
| **AW-IV'** | Port CSS + BBNF + Sheets; ShapeRef + PHF + ClassifyByte + Pratt inlined as codegen. | CSS bootstrap ≥ 900 MB/s; Sheets parse_simple ≥ 200 MB/s; BBNF ≥ 500 MB/s |
| **AW-V'** | Document-parallel fork over stage-1 index; bloom dedup on Columns. | canada 4c ≥ 5000 MB/s; tailwind 4c ≥ 1.5 GB/s |
| **AW-VI'** | AX re-architected onto RD (checkpoint + replay + incremental). | incremental-edit ≤ 200 µs on 100 KB CSS |
| **AW-VII'** | sonic-rs + lightningcss parity harnesses; FINAL. | zero divergence on corpus |

## 9. When Path A wins

Path A is correct iff W5.6 codegen-specialisation closes the
dispatch_one floor **within the existing DtaState shell**.
Concretely: if `dta_run_json` with inlined arms benches within 10%
of sonic-rs on twitter, the shell paid no rent; "inlined DtaState
match" and "fn-per-rule" vanish in LLVM's optimiser.

Shift-to-Path-A indicators:

- W5.6 prototype JSON ≥ 1800 MB/s twitter.
- Sheets parse_simple under W5.6 ≥ 50 MB/s (within 2× post-AU).
- Symbol attribution shows `dta_run_<g>` inlined away entirely;
  only `parse_*` frames remain.

If any hold, the DTA substrate paid for itself. Ship Path A.

If all three fail, Path B is the honest conclusion.

## Summary

Path B: admit the DTA as shipped is a state-machine-interpreter;
admit `dispatch_one` is a canonical ceiling no lever amortises;
admit sonic-rs/simdjson are two-stage (structural-SIMD + inlined
tape), not tagged-union-per-byte; ship the honest path —
fn-per-rule + stage-1 SIMD + fused SoA writes. The 400+ commits
are foundation that survives the surgery. Delete 3,600 LOC of
interpreter, emit ~2,000 LOC of grammar-parameterised RD, exceed
sonic-rs (JSON) and lightningcss (CSS) with one emitter driven by
the IR.
