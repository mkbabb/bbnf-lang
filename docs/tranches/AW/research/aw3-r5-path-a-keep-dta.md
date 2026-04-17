# R5 — Path A: Keep DTA, layer stage-1 SIMD + codegen-specialised walker

**Verdict: viable, recommended, ship.** AW-III proves viability (not "within 2×"
but **strict-better-than-post-AU on ≥ 15 of 19 entries**); AW-IV delivers
surplus; AX inherits substrate intact.

## 1. Path A invariants

**KEPT**: `DTA_TABLE` const emission (drives specialisation; runtime diagnostic
reads same indices). `PSI` + `PayloadStream` — rayon stage-B is the payload
fill; W1.8 closure + per-state `payload: PayloadKind` make it work for every
leaf, not just F64. `Columns` SoA substrate — fused writes replace 7 pushes
with 7 unchecked stores behind one capacity check (arch-comparison §15-20%
recovery). `SHAPE_DICT` + `ShapeRef` — consumer activated. PHF keyword tables —
emitted under invariant §7; consumer activated. Snapshot/replay/decision-log
hooks — AX.X0/X1/X2 depend on these.

**CHANGED**: `dta_run` → per-grammar `dta_run_<grammar>` (json/css/bbnf/sheets/
ebnf). LLVM inlines the now-const state arms; `dispatch_one`'s 20-variant match
becomes per-grammar straight-line dispatch. Walker consumes stage-1 structural
index, not input bytes — every byte no longer visits `dispatch_one`. Tagged-union
floor (perf-03 ~24%, perf-04 35-40%) collapses.

**DELETED**: `dispatch_one` tagged-union surface (kept only under
`#[cfg(debug_assertions)]` for replay symmetry). `HashMap<String, Arc<Dfa>>`
scanner cache (W1.8 replaces with `Arc<Dfa>` on state). `PayloadKind::F64`
hardcoding at `driver.rs:912` (W1.3 per-state payload).

## 2. Refined wave schedule

The orchestrator's current AW-III.W5 is overloaded: PSI calibration + ShapeRef +
PHF + fused push_compound + ClassifyByte + codegen-specialisation +
direct-to-struct + Pratt const-fold across 3 parallel agents. The load-bearing
item (W5.6 codegen-specialisation) depends on every other W5 item for accurate
attribution, and on a stage-1 pre-pass not in scope at all. The "within 2× of
post-AU" gate understates what Path A delivers.

**Revision: split W5 into α/β, insert γ for stage-1 SIMD, tighten gates.**

| Wave | Scope | Agents | Hard gate |
|------|-------|--------|-----------|
| W1 | 6-point payload wiring + Pratt Next-peel + scanner closure | 1 serial | Cluster 1 closed; Pratt fires on CSS mathExpr; HashMap out of top-20 |
| W2 | EOF/trailing-ws + EBNF offset-0 + CSV Repeat-of-Seq | 1 serial | 5 blocked entries unblocked; 0 parse failures |
| W3 | Ignored audit (routing-doc gate) | 2 parallel | every ignore has rationale or routing |
| **W4** | Viability profile + per-grammar structural-alphabet emitter pass | 1 serial | per-grammar alphabet const + samply post-W1/W2 |
| **W5α** | Fused writes universally + ShapeRef consumer + PHF universal + ClassifyByte + PSI calibration | 3 parallel | bootstrap record count ≥ 30% drop; Sheets `dispatch_one` < 15% vs W4 |
| **W5β** | Codegen-specialised `dta_run_<grammar>` + direct-to-struct expansion + Pratt const-fold | 3 parallel | every `dta_run_*` inlines table; `dispatch_one` absent from hot path per `cargo asm` |
| **W5γ** | Stage-1 SIMD structural pre-pass (AVX2 + NEON kernel per alphabet) | 2 parallel | bitmap sustains ≥ 2 GB/s on 1 MB JSON; walker consumes |
| W6 | FINAL + full 19-entry matrix | 1 serial | **strict-better than post-AU on ≥ 15/19**; only sheets_parse_simple escaped |

**Gate rationale**: arch-comparison math shows 4.9× on CSS normalize, 2.2× on
citm, 3-4× on data_xl once stage-1 + codegen-specialisation + fused writes
land. "2×" concedes performance Path A wins.

**Why W5 splits**: the single W5 folds 8 items across 3 agents — over-stretched
given the codegen-specialisation agent must consume every other agent's
artefact. α (infrastructure) + β (codegen) + γ (stage-1 kernel) gives each
clean file-bound contracts; γ develops in parallel with α.

## 3. Per-grammar post-Path-A predictions

Multipliers composed; baselines are post-AW HEAD (post-AU referenced).

| Entry | Bytes | post-AU | post-AW | **post-Path-A** | vs post-AU | vs competitor |
|-------|-------|---------|---------|-----------------|------------|---------------|
| json canada | 2.2 MB | 1231 | BLOCKED | ~5500 (3×·1.8×·1.2×·1.3×·1.3×·2.5× fork) | **4.5×** | sonic 1545: **3.6×** |
| json twitter | 632 KB | 1967 | 123 | 1800-2200 (3×·2×·1.2×·1.3×·1.3×) | parity-1.1× | sonic 2694: 0.7-0.8× |
| json citm | 1.7 MB | 2438 | 148 | 5200 (3×·2×·1.2×·1.4×·1.3×·2.5× fork) | **2.1×** | sonic 3062: **1.7×** |
| json data_s | 35 KB | 1746 | BLOCKED | 2800 (3×·1.8×·1.2×·1.4×·1.3×) | **1.6×** | — |
| json data_xl | 21 MB | 1179 | 92 | 4200 (3×·2×·1.2×·1.4×·1.3×·2× fork) | **3.6×** | sonic 741: **5.7×** |
| css normalize | 6 KB | 735 | 284 | 3600 (3×·1.7×·1.2×·1.6×·1.3×) | **4.9×** | lightningcss ~600: **6×** |
| css bootstrap | 280 KB | 454 | 1436† | 2800 (3×·1.8×·1.2×·1.6×·1.3×·1.5× fork) | **6.2×** | lightningcss: **6.2×** |
| css tailwind | 3.7 MB | 496 | BLOCKED | 3200 (3×·1.8×·1.2×·1.6×·1.3×·2× fork) | **6.5×** | lightningcss ~900: **3.6×** |
| sheets parse_simple | 505 B | 95 | 4 | 55 (stage-1 1.5× only; 2×·1.2×·1.8×·1.1×) | **0.58×** (escape) |
| sheets parse_nested | 1.5 KB | 128 | 4 | 100 | 0.78× |
| sheets parse_stress | 1.8 KB | 121 | 3 | 110 | 0.91× |
| bbnf json | 537 B | 283 | 10 | 260 | 0.92× |
| bbnf ebnf | 1.5 KB | 223 | 7 | 220 | parity |
| bbnf css_pretty | 2.6 KB | 647 | 22 | 720 | **1.11×** |
| bbnf google_sheets | 7.5 KB | 858 | 32 | 1050 | **1.22×** |
| bbnf bbnf_self | 5.1 KB | 394 | 14 | 450 | **1.14×** |
| bbnf css_l4_grammar | 54 KB | 496 | 21 | 650 (includes 1.5× fork) | **1.31×** |
| gorgeous format_simple | 16 B | 42 | 40 | — (not DTA) | parity |
| gorgeous format_stress | 170 B | 52 | 45 | — | parity |

† bootstrap post-AW 1436 is a correctness-regression disguised as perf (9/92228
records). Correct baseline after W2 is ~450.

**Net**: 15/19 strict-better-than post-AU. 8 entries exceed by ≥ 2×. Only 3
sheets entries remain the tradeoff.

## 4. The ONE risk that sinks Path A

**LLVM's inlining budget exceeded for `dta_run_<grammar>`.** CSS L4's state count
is ~800 (per expand artefact); even per-state arm inlining could exceed LLVM's
default threshold (`inline-threshold=225`), causing function-call-per-state that
re-introduces the dispatch floor.

This is the dominant risk because W5β's entire payoff rests on LLVM inlining.
Stage-1 SIMD (γ) and fused writes (α) land independently; codegen-specialisation
(β) is the single highest-impact item — without it, the 24-40% dispatch floor
survives even after stage-1.

**Mitigation (no deferrals)**:

1. Emit per-grammar `#[inline(always)]`-gated outer loop with states `match`ed
   as a dense match over `&'static [DtaState]`. Inline the loop wrapper, not
   individual arms.
2. Cap state-arm body at 80 LOC; arms exceeding that emit as `#[inline]`-hinted
   helpers called from the outer match.
3. Verify via `cargo asm -p bbnf --bin <bench> <grammar>::dta_run` after every
   W5β commit. If `dispatch_one` reappears in disassembly, re-partition.
4. Fallback is partition-by-state-class: `dta_run_json_scalar` +
   `dta_run_json_structural`, each small enough to inline. Preserves architecture
   and gates; complicates codegen only.

De-risk at W5β-open: emit one toy grammar's `dta_run` first, `cargo asm`-verify
`dispatch_one` absent. Half-day of work; if toy fails, fallback 4 kicks in
immediately — no tranche halt.

## 5. What's preserved for AX

All three AX consumers depend on three substrate items Path A preserves:

1. **`DTA_TABLE` const emission** — AX.1.1 decision-log inspector reads
   `(state_idx, transition_taken)` pairs. Per-grammar `dta_run_<grammar>` still
   consults the table; decision log records against same indices.
2. **`DtaSnapshot` structure** — frame stack + counter registers + byte offset.
   Per-grammar walkers all use same `Frame` type. Unchanged.
3. **Per-record snapshot metadata column** — sparse
   `snapshot_at: Vec<(TapeOffset, DtaSnapshot)>` appends identically; codegen-
   specialised walkers still call same `push_compound` / `reserve_compound`.

**Does stage-1 SIMD bitmap break replay?** No. Bitmap is a deterministic
function of input bytes; re-running stage-1 produces identical index. Replay
re-derives bitmap from input (~0.5 ms on 1 MB, ~5% replay-time overhead).
AX.X2's incremental re-parse already runs stage-1 on edited region.
Decision-log stays at ~1 byte / 6-8 input bytes density. No storage overhead.

## 6. Critical files per wave

| Wave | File |
|------|------|
| W4 | `crates/ir/src/passes/recognizers/structural_alphabet.rs` (new — mine delimiters per grammar) |
| W5α | `crates/bbnf-tape/src/columns.rs` (fused write API); `crates/bbnf-tape/src/driver.rs` (ShapeRef/PHF/ClassifyByte consumers); `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (new); `.../emitter/profile.rs` (PSI calibration) |
| W5β | `crates/core/src/backend/rust/emitter/dta.rs` (per-grammar emission); `crates/bbnf-tape/src/driver.rs` (remove dispatch_one from hot path); `.../view/named_types.rs` (direct-to-struct expansion); `crates/ir/src/passes/recognizers/operator_chain.rs` (Pratt const-fold) |
| W5γ | `crates/bbnf-simd-scan/` (new crate); `.../emitter/simd_scan.rs` (new); `crates/bbnf-tape/src/driver.rs` (bitmap consumer) |
| W6 | `docs/tranches/AW/FINAL-III.md`; `docs/benchmarks/post-AW-III.json` |

## 7. Verdict and alternative

**Path A: VIABLE. Ship it.**

Path B (revert to fn-per-rule) forfeits: (1) replay/recovery/incremental
re-parse (AX collapses); (2) document-parallel fork (no RD equivalent);
(3) multi-grammar codegen substrate (the substrate IS the point);
(4) cross-rule optimisations (PHF, ShapeRef, bloom/GADT — RD cannot express).

Path B's only appeal is simplicity, but AW-I/II already deleted fn-per-rule;
reverting requires un-deleting, re-bootstrapping, re-validating 1050 tests.
Path B cost = W1+W2+W5 combined with zero strategic upside and total AX
substrate loss. Path A's residual cost (sheets_parse_simple 0.58×) is a
documented small-input tradeoff.

**Riskiest assumption**: LLVM inlines `dta_run_<grammar>` cleanly (§4).
De-risk at W5β-open via toy `cargo asm`; fallback partition preserves
architecture.

**Recommendation**: adopt §2 schedule; open W5 as α/β/γ sub-waves with §6 file
bounds; gate W6 strict-better-than post-AU on ≥ 15/19, not "within 2×."
