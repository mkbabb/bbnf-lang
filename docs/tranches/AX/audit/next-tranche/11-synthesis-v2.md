# AY Planning — Synthesis v2 (post A7-A10)

Integrates A7 (AX W2-W15 absorption), A8 (legacy prune + DTA cert),
A9 (structural scan + Pratt), A10 (Value API refine) into the v1
synthesis's priority matrix. Drives the revised AY tranche plan.

## 1. New cross-cutting facts

### 1.1 DTA fully retired at runtime; legacy persists at emit + type surface

A8 certification (§1): zero DTA/walker nm symbols + zero DTA frames in
samply profile.json.syms.json across all 4 fresh bench binaries.
`DtaStateId::NONE` references in generated.rs are compile-time
sentinels, never constructed with non-NONE variant at runtime. Benches
are definitively on the RD path.

Legacy pruning scope **~2,300 LOC** concrete:
- `crates/tape/src/dta.rs` kernel-dead carve: 458 LOC of 550 (retain only `DtaRuleId` + `DtaAssociativity` + `DtaPrecedenceEntry` + `DtaStateId` sentinel).
- 5 stale wire-contract tests: 1,390 LOC.
- `GrammarProfile` dead-substrate (list_rules, shape_dict, push_*_count): 150 LOC.
- `crates/tape/src/shape_dict.rs`: 79 LOC.
- 2 emitter-shape tests (classify_byte_dispatch, ctns_lifter): 220 LOC.

### 1.2 Structural alphabet mined but INACTIVE

A9 confirms: `structural_alphabet` mining fires (`crates/ir/src/passes/sets/structural_alphabet.rs:182`), emits to `generated.rs` (28 singletons + 17 digraphs for BBNF), populates `GrammarProfile` slots — **but zero downstream consumers**. `nm` on 4 bench binaries returns 0 hits for `simd_scan` / `scan_structural` / `StructuralIndex` / `KernelShape` / `from_profile`. CTNS similarly inactive (`lower/mod.rs:236` hard-codes `ctns_lifts: HashSet::new()`).

This is a **substrate-without-consumer anti-pattern** (invariant 2). The walker retirement at W0b removed the intended reader. simd-scan kernels compile as dead weight.

**Activate in AY.W1** (after tape inline; mi_heap page sharing constraint per A9 §b). Expected CSS L4 -10 to -13 pp self-time (40-50% of the 26% regex_scan share); Sheets -4 to -6 pp.

### 1.3 Pratt Option C

A9 verdict: Option A rejected (hybrid header; invariant 20 exposure), Option B rejected (flat tape + consumer rewrite; W0a.2.k regression archaeology is dispositive — CSS 11/16 + Sheets 14/25 parity failed). **Option C**: preserve reducer-compound tape; inline `push_leaf_with_arena_payload` per call site; hoist op_stack capacity from mined per-rule chain-depth (miner data at `operator_chain.rs`); write op_discriminant directly to payload column, drop `arena_mut().push` round-trip. Zero consumer changes, zero invariant exposure.

Pratt cost is 8-9% combined Sheets stress; 1.62% BBNF avg; absent from CSS+JSON top-10 — **not W1-priority standalone, folds into W1 as incremental under tape inline umbrella**.

### 1.4 Value API: TypeDesc-collapse + Handle-into-Tape

A10 design verdicts:
- **Shape**: per-rule variants with TypeDesc-equivalence-class collapse (`FxHashMap<TypeDesc, usize>`). Rules with identical `TypeDesc` collapse into a single variant; no variant-explosion on large grammars (CSS L4's 200+ rules collapse to ~20 unique TypeDesc-classes).
- **Materialization**: Handle-into-Document reusing existing `Tape` + AoS `packed_cache` sidecar. 24-32 byte enum. Compound children in `Vec<Value>`, no second arena.
- **Bench lanes**: canonical / lazy-get / eager (all three, not two).
- **Split**: W3a (runtime substrate handle.rs + path.rs) + W3b (emitter view/value.rs + to_value) + W3c (bench lanes + round-trip parity).

My v1 W3.md draft had 7 concrete errors (per A10 §d). Corrections integrate into revised W3.

### 1.5 AX W2-W15 absorption verdicts

A7 bucketed 14 AX waves:
- **FOLD into AY**: 6 (W2 parity CI, W3 subsystem closures, W4 L1/L2 miner + scanner gen, W6 CTNS, W9 parallel fork → AY.W8, W15 FINAL → AY.W0+W7).
- **NEW AY wave**: 2 (W7 LazyRef → AY.W3 expansion; W9 → AY.W8).
- **RETIRE obsolete**: 3 (W4 SIMD micro-kernels, W5 CSS SIMD cluster, W8 speculative parsing).
- **Defer AZ**: 1 (W13 CPU autotune + PMC).
- **Defer post-AZ**: 4 (W5 ShapeRef consumer, W10-12 e-graph, W14 multi-visitor).

Retirement rationales (per A7):
- W4 SIMD micro-kernels: JSON `__regex_scan_JsonParser` is ~0% self-time; scanner-amortising kernels can't reclaim sub-1% symbol.
- W5 CSS SIMD cluster: bbnf already beats lightningcss 0.60×-0.81× at scale — optimizing what we already outpace violates AX invariant 6.
- W8 speculative: dispatcher self-time capped at ≤12% on every grammar/fixture; speculation amortises a compound-boundary dispatch whose ceiling is below the probability-of-hit threshold, with rollback-fuzz correctness risk.

## 2. Revised AY wave schedule (with A7's ordering + A9/A10 additions)

Nine waves. Chain:
**W0 → W1 → {W2 ∥ W4} → W3 → W5 → W6 → W8 → W7.**

| Wave | Absorbed items | Headline |
|------|----------------|----------|
| **W0** | stale tests retirement + ebnf_prettify + AX.FINAL + **DTA kernel-dead prune (~2,300 LOC)** + **housekeeping** (orphan worktrees, stale profiles) | legacy pruning + AX close |
| **W1** | tape inline + finalise fusion + **Pratt Option C incremental** + **structural alphabet activation** | universal hot-path substrate |
| **W2** | Named preservation + direct-to-struct + wire-contract | invariants 20/21 discharge |
| **W3** | split into **W3a runtime substrate** (handle.rs + path.rs) + **W3b Value emitter** + **W3c bench lanes** | apples-to-apples Value API |
| **W4** | regex specialisation + BoundedRegex (AX.W6) + L1/L2 miner inheritance (AX.W4) + scanner generalization | token-heavy grammar wins |
| **W5** | CSS L4 @import split + DFA hoist + shared PHF (compile-time A/B/D) | CSS compile cost |
| **W6** | parse_that de-generic + ax-iter tuning (compile-time C/E) | workspace compile |
| **W8** | Document-parallel fork (AX.W9 demoted) | amortisation multiplier |
| **W7** | FINAL + bench matrix + FINAL.md + CI-gate activation + cssparser parity + AZ handoff | tranche close |

W8 sits between W6 and W7 per A7's chain proposal. Parallel fork is independent of the substrate/Value/regex work but needs W1 tape inline before profiling shows benefit.

## 3. Invariants — no new additions beyond v1's 22-24

The A7-A10 findings are EXECUTION concerns + lever activations, not invariant additions. v1's proposed invariants 22 (tape substrate inline), 23 (Named preservation end-to-end), 24 (Value API apples-to-apples) remain the only AY additions to AX's 1-21.

A9's "structural alphabet activation" discharges **AX invariant 2** (substrate-with-consumer) retroactively — mining infrastructure has existed since AU; the walker retirement orphaned the reader; AY.W1 restores it. Not a new invariant; a discharge of existing.

A8's pruning scope discharges **AX invariant 14** (gate-predicate symmetry) retroactively — the 5 stale tests correspond to predicates retired in W0a.2.j + fields carved in W0b.A. Retirement of the tests completes the invariant-14 cycle.

## 4. Defensible floor (revised)

Per v1's floor (5 items), revised for A7-A10:

1. **W0** stale test retirement + DTA kernel-dead prune — file deletions, no architectural risk.
2. **W1 tape inline** — `#[inline(always)]` + call-site monomorphisation; substrate-only.
3. **W1 Pratt Option C** — inline `push_leaf_with_arena_payload` + op_stack hoist; zero consumer exposure.
4. **W2 Named preservation** — single-pass guard in metadata.rs or egraph cost; wire-contract test.
5. **W3a+W3b Value emitter** — TypeDesc-collapse variants + Handle-into-Tape materialization; codegen-only.
6. **W7 FINAL** — bench + FINAL.md + handoff.

Six items. Adds W1's Pratt Option C + upgrades W3 to pair (W3a + W3b) from v1's single W3.

## 5. What's been tried that needs improvement

### 5.1 Structural alphabet — tried, abandoned by W0b, reviving

Mining landed pre-AU; walker consumed; W0b deleted walker; no new consumer landed. A9 is the first audit to note the orphan state. AY.W1 revives.

### 5.2 Flat Pratt tape — tried, reverted

AX.W0a.2.k attempted flat Pratt tape; CSS + Sheets parity regressed 11/16 + 14/25 per PROGRESS.md. W0a.2.l preserved reducer-compound tree. A9 Option C is the successor: inline within-compound + op_stack hoist; NO tape restructure.

### 5.3 Hand-coded Value — tried, reverted

W1.A/B hand-coded `bbnf::json::Value` + `bbnf::css::StyleSheet` reverted at W1r.0 (-6128 LOC) for invariant 4/11/18 violations. A10's TypeDesc-collapse + grammar-emission addresses the invariant-21 concern: the emitted enum is grammar-derived (variants enumerate rules, not comparator shape).

### 5.4 Direct-to-struct projection — tried repeatedly, upstream collapse

AS.2.3 → AU.4.2 → AW.0.5 → AW-III.W6.4 → AW-IV.W3.5a → AX.W1r.1 all landed substrate; Named collapses before Rust emit. A6 identified two hypotheses (egraph extraction; alias/transparent stamping). AY.W2 empirically discriminates + surgically fixes.

## 6. What user asked to "ensure perfected"

### 6.1 Structural scan with dense alphabets

**Status**: substrate mined but inactive (A9). **AY.W1** activates via dense-alphabet pre-pass: emit `scan_structural(input, &alphabet)` at every `<Parser>::parse` entry, feed result into `ScanState` so (1) CTNS-admitted regex patterns jump via `idx.positions[slot]`, (2) `skip_space` reads from the index. Expected CSS L4 -10 to -13pp.

### 6.2 Pratt flattening

**Status**: reducer-compound tree preserved per W0a.2.l. Option C within-compound optimization, no tape restructure. **AY.W1 incremental** under tape inline umbrella.

### 6.3 Value API

**Status**: lazy cursor (NodeView) + serialize_compact only; no materialized tree. **AY.W3 three-part** split: W3a (handle+path substrate) → W3b (grammar-emitted Value via TypeDesc-collapse) → W3c (three bench lanes: canonical / lazy-get / eager).

## 7. Commits

v2 synthesis commits as `docs(next-tranche): synthesis v2 integrating A7-A10 (AY.planning)`. Revised AY.md + affected wave specs commit as `docs(AY): integrate A7-A10 — DTA prune, structural activation, Pratt Option C, Value W3-split`.
