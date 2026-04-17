# AW-V N1 — E-graph-optimised shape codegen

**Worktree.** `bbnf-wt-aw5-research-n1`. Read-only against HEAD `a2ac7e0f`.
All references cite `crates/{egraph,ir,bbnf-simd-scan,bbnf-tape}/src/*.rs`.

## Executive summary (≤150 words)

Interpose a *Shape-Emit IR* (SEIR) — abstract ops like
`scan_bytes_until_structural`, `emit_compound_v32`,
`visit_method_call<V>` — between B4 `ShapeTag` and emitter
`TokenStream`. `crates/egraph/` already admits it as a `Language`.
Twelve rewrites (eight headline + four chain-enablers) encode fusions
sonic/simdjson hand-wrote: scan-emit span fusion, v32 column-parallel
packing, visitor tee, no-escape string fast-path, integer-SWAR bypass
of Eisel-Lemire, alphabet-memchr demotion, workload-gated `SHAPE_DICT`
skip, trivial-visitor elision. Extraction picks the cheapest form via
Pareto-weighted cost model embedding `CostWeights`. Budget-bounded
saturation (1 ms / 5 k nodes) with cache reuse. All rewrites
IR-fact-gated; seven of eight are genuinely novel versus sonic-rs
0.3.17, simdjson 3.x, lightningcss 1.28.

---

## 1. IR level

Rewriting on `TokenStream` loses algebraic structure. Interpose
**Shape-Emit IR (SEIR)** between `ShapeTag` (B4 §1) and token
emission. `ShapeTag` feeds shape mining; SEIR feeds rewriting;
emitter lowers extracted SEIR to tokens.

SEIR is an `#[egraph_derive::Language]` enum isomorphic to
`GrammarENode` (`crates/ir/src/egraph/node.rs:23-68`):

```rust
pub enum SeirNode {
    // ── scanners ────────────────────────────────────────────────────
    ScanBytesUntilStructural { bytes: Id, idx: Id },
    ScanQuotedStringFast(Id),                 // no-escape assumption
    ScanQuotedStringEscaped(Id),              // with escape handling
    MemchrLoop { needle: u8, input: Id },
    SkipWsInline(Id),

    // ── number kernels ──────────────────────────────────────────────
    ParseNumberEiselLemire(Id),
    ParseIntegerSwar(Id),

    // ── compound emission ───────────────────────────────────────────
    EmitCompoundScalar { kind: u8, span: [Id; 2], payload: Id },
    EmitCompoundV32 { kind: u8, cols: Box<[Id]> },     // 7-col pack
    EmitSpanFromStructuralIndex { kind: u8, slot: Id },
    PushShapeRef { shape_hash: Id, span: [Id; 2] },
    PushScalarPayloadF64(Id),
    PushScalarPayloadU32(Id),

    // ── visitor calls ───────────────────────────────────────────────
    VisitorMethodCall { visitor: Id, method: VisitorMethodId, args: Box<[Id]> },
    VisitorTee { v1: Id, v2: Id, event: Id },
    Elide,

    // ── gates ───────────────────────────────────────────────────────
    RuntimeGate { cond: Id, hot: Id, cold: Id },

    // ── inputs ──────────────────────────────────────────────────────
    InputSlice,
    StructuralIdxRef,
    ShapeHashOf(Id),
    Literal(u64),
}
```

Each node has a cycle cost on M4 P-core (Appendix B). Extraction
picks the cheapest-cost form. Shape-mining produces a SEIR tree;
emitter lowers extracted SEIR to tokens mechanically.

---

## 2. Eight headline rewrite rules

Each rule: **precondition** (IR fact), **LHS**, **RHS**,
**cycle-delta**, **applicability**. Costs derived from B1 §7 and B3 §9.

### R1 — scan/emit span fusion

*Precondition.* `StructuralIdx::has_slot(span_start) && StructuralIdx::has_slot(span_end)`.

```
EmitCompoundScalar { kind, span: [lo, hi], payload }
  where payload = ScanBytesUntilStructural { bytes: span, idx }
≡
EmitSpanFromStructuralIndex { kind, slot: span_start_slot }
```

**Rationale.** Inner scan re-visits bytes the index already
classified. Fusion elides the scan. Saves ~0.22 cyc/B on the scanned
region.

**Applicability.** CSS L4 declarations, JSON object keys (always
structural-bounded), BBNF directives. Not JSON numbers (terminator
past payload).

### R2 — column-parallel v32 compound emission

*Precondition.* Two `EmitCompound*` nodes in Seq with disjoint
`columns_written` and same 32-B record bucket (AW-V Lever 4).

```
Seq(
  EmitCompoundScalar { cols_written: {rule_kind, tape_kind, span_lo} },
  EmitCompoundScalar { cols_written: {span_hi, child_off, variant_idx, sib_skip} },
)
≡ EmitCompoundV32 { kind, cols: [rk, tk, lo, hi, co, vi, ss] }
```

**Rationale.** `push_compound_fused_v32` (AW-V Lever 4) packs 7
scalar stores into one 32-B NEON Q store: 21 → 4 cyc/record.
~2 cyc/rec ≈ 0.25 cyc/B on structural-dense input.

**Applicability.** Every grammar. Sonic's AoS u64 tape precludes
this entirely.

### R3 — visitor tee for multi-visitor parse

*Precondition.* Two `VisitorMethodCall` with matching method+args
and no inter-method dependency (`FnDescriptor::is_pure`).

```
Seq(
  VisitorMethodCall { visitor: v1, method: m, args: a },
  VisitorMethodCall { visitor: v2, method: m, args: a },
)
≡ VisitorTee { v1, v2, event: (m, a) }
```

**Rationale.** AW-V Lever 7: parse-once-emit-twice. One byte-scan
body, both visitors fire per event; second byte-scan dissolves.
Saves ~0.6 cyc/B when consumer wants tape + owned value.

**Applicability.** Serde + validate, CSS compute + lint, Sheets
eval + format. Sonic visitor is single-target; genuinely novel.

### R4 — no-escape string fast-path

*Precondition.* Runtime `memchr(b'\\', slice) == None`. Encoded as
`RuntimeGate`.

```
ScanQuotedStringEscaped(bytes)
≡ RuntimeGate {
    cond: memchr_is_none(b'\\', bytes),
    hot:  ScanQuotedStringFast(bytes),
    cold: ScanQuotedStringEscaped(bytes),
  }
```

**Rationale.** Escape-mask is 0.45 cyc/string-B; `memchr(b'"')`
alone is 0.15 cyc/B. Twitter ~99% no-escape. Saves ~0.30 cyc/string-B.

**Applicability.** Twitter 99%, CSS strings 95%, Sheets 70%. Sonic
always uses escape-mask; gating on runtime is novel.

### R5 — integer-SWAR bypass of Eisel-Lemire

*Precondition.* `pattern_alphabet ⊆ [0-9]` (no `.eE+-`) AND runtime
`span_len ≤ 15`.

```
ParseNumberEiselLemire(bytes)
≡ RuntimeGate {
    cond: span_matches_charclass(bytes, [0-9]+),
    hot:  ParseIntegerSwar(bytes),
    cold: ParseNumberEiselLemire(bytes),
  }
```

**Rationale.** Integer SWAR: ~0.19 cyc/digit-B. Eisel-Lemire:
~2.5 cyc/digit-B (6-digit avg). Saves ~2.3 cyc/digit-B.

**Applicability.** `data_s`/`data_xl` (int-heavy). `canada` (floats)
doesn't fire. Novel: sonic always enters float-path.

### R6 — alphabet-triviality demotion to memchr

*Precondition.* `single_bytes.len() == 1 && digraphs.empty() && quote_classes.empty()`.

```
ScanBytesUntilStructural { bytes, idx: stage1 }
≡ MemchrLoop { needle: single_byte, input: bytes }
```

**Rationale.** `scan_nibble`: 0.22 cyc/B multi-byte alphabet.
`memchr` single byte: 0.09 cyc/B. Saves ~0.13 cyc/B.

**Applicability.** Sheets `cell_ref`, BBNF single-delimiter rules.
Multi-byte grammars (CSS/JSON) don't fire. Not novel algorithmically;
novel as *rewrite-driven selection*.

### R7 — workload-gated SHAPE_DICT skip

*Precondition.* `shape_repetition_rate(hash) × shape_ref_savings < shape_dict_lookup_cost`.

```
PushShapeRef { shape_hash, span }
  (where shape_dict_lookup is unprofitable)
≡ EmitCompoundV32 { kind, cols }  (fall back to full record)
```

**Rationale.** `SHAPE_DICT.lookup` is ~12 cyc hash probe. Below
a rep-rate threshold, probe cost exceeds dedup win.
`state_visit_frequency` miner already provides the frequency.

**Applicability.** JSON numeric arrays skip; CSS bootstrap keeps.
Novel as cost-model decision vs grammar-name branch.

### R8 — trivial-visitor elision

*Precondition.* `FnDescriptor::body_is_noop` on every visitor method.

```
VisitorMethodCall { visitor: NoOpVisitor, method, args: _ }
≡ Elide
```

**Rationale.** LLVM preserves arg-evaluation side effects through
no-op calls. Rewriting at SEIR level drops the arg-computation
subtree entirely.

**Applicability.** Schema validators, syntax-check-only parsers.
Not tape visitors. Sonic/simdjson have no null-visitor path; novel.

### Four chain-enabler rewrites

R9: `ScanBytesUntilStructural ∘ SkipWsInline` ⇒ combined
nospace+structural bitmap scan. R10: `EmitCompoundV32` associativity
(merges through Seq). R11: `RuntimeGate` branch sinking (push gates
below compounds). R12: `PushShapeRef` dedup across canonical-identical
siblings.

---

## 3. Cost model

Embed shared `egraph::CostWeights` (`cost_weights.rs:40`); layer
SEIR-specific dimensions (Appendix B). Per-op base cost in cycles on
M4 P-core; size cost in instruction bytes (L1i = 192 KB).

```
cost(node) = weights.structural + cyc(op)
           + weight_icache × bytes(op)
           + Σ child_cost
```

`weight_icache` defaults 0.0 until a subtree exceeds `L1i_budget`
(100 KB/fn); then rises to penalise bloat — matches the AW-IV 154 KB
CSS walker pathology (AW-V.md:42).

**Pareto frontier.** Two-axis `(cycles, bytes)` via `egraph::Lattice`
(`crates/egraph/src/extract.rs:36-58`). `SeirCost::Cost = (f64, f64)`.

---

## 4. Rule composition — three chains

**C1: scan+emit+v32 fusion (twitter ~0.28 cyc/B).**
`ScanBytesUntilStructural` →R1→ `EmitSpanFromStructuralIndex` →R2→
`EmitCompoundV32`. Saves 2 cyc/rec; twitter avg 7-B records.

**C2: no-escape + integer bypass (data_s ~2.6 cyc/B on int-records).**
R4 on key scan + R5 on value parse. Compound savings per int-valued
record.

**C3: SHAPE_DICT-skip + v32 (bootstrap +1.5 cyc/rec).**
R7 decides ShapeRef viability; R2 promotes when skipped; R12 dedups
when kept. Cost model chooses per-grammar at extract time.

---

## 5. IR-fact interface

Each rewrite's precondition is an IR-fact lookup, not a name branch.
`SeirContext` holds: `&GrammarProfile`, `&StructuralAlphabet`,
`&PatternAlphabetTable`, `&[FnDescriptor]`, `&CostConfig`.

Rewrites mirror `crates/ir/src/egraph/rules/regex.rs` signature:
`search(&self, &EGraph, &SeirContext)`. R6 queries
`alphabet.single_bytes.len() == 1`; R7 queries
`profile.shape_repetition_rate(hash)`; R8 queries
`fns[fn_id].body_is_noop`; R5 queries
`pattern_alphabet[regex_id].subset_of([0-9])`.

No rewrite reads `ir.name`. Grammar identity appears only as
emission-namespace prefix, per AW-IV §6/§7.

---

## 6. Saturation strategy

**Budget.** 1 ms wall-clock OR 5000 e-node growth (whichever first).
Reads `CostConfig::egraph_iter_limit` / `egraph_node_limit`
(`cost_config.rs:42-46`).

**Fallback.** On exhaust, extract from current state. Equality
saturation is monotone: partial saturation is sound.

**Incremental reuse.** Persist e-graph to
`target/bbnf-cache/egraph-<grammar-hash>.bin` (bincode of
`EGraph<SeirNode>`). Unchanged grammar reuses saved state;
changed grammar merges via incremental canonicalisation.

Report via `BBNF_EGRAPH_REPORT=1` (`crates/ir/src/egraph/mod.rs:105-121`).

---

## 7. Proof obligations — three riskiest rewrites

**P1: R5 integer SWAR ≡ Eisel-Lemire on `[0-9]+`.**
For `n ≤ 15`, `N = Σ d_i × 10^(n-1-i) < 2^50`. Integer SWAR casts
`N as f64` via IEEE 754 round-to-nearest-even. Eisel-Lemire's
Clinger fast-path on pure-integer input computes the same `N` and
casts identically. Bit-identical. `n > 15`: gate falls back. ∎

**P2: R1 scan/emit span fusion.**
`StructuralIndex` built by `scan_structural` over input + alphabet
(definitionally monotone). Re-scan runs same classification, same
result. Compound end = `idx.positions[start_slot + width]` when
close byte ∈ alphabet (object/array grammars always qualify). ∎

**P3: R3 visitor tee.**
Parse is deterministic: `parse: &[u8] → [Event]` is pure. Both
`v1.parse(input)` and `v2.parse(input)` observe the same event
sequence. `VisitorTee` emits `v1.visit_x; v2.visit_x` per event.
Order-insensitivity required for concurrent side effects — gated
on `FnDescriptor::is_pure`. ∎

Full proofs in Appendix C.

---

## 8. Implementation sketch

- New IR pass: `crates/ir/src/passes/shape_emit_ir.rs` consumes
  `ShapeTag` + miner outputs → `SeirTree`.
- SEIR language: `crates/ir/src/seir/` — `mod.rs`, `node.rs`
  (`SeirNode` with `Language` derive), `context.rs`, `rules/` (one
  file per R1..R12), `cost.rs`.
- Emitter: `crates/core/src/backend/rust/emitter/shapes/` (AW-V.W3
  target) consumes extracted `SeirTree` → `TokenStream`. Calls
  `extract_seir(egraph, root)` from saturated e-graph.
- Cost weights: extend `crates/egraph/src/cost_weights.rs` with SEIR
  dimensions (Appendix B).

No new workspace crate — SEIR sub-module of `crates/ir/` matching
`crates/ir/src/egraph/` precedent. One canonical codegen path;
rules matching no rewrite extract trivially.

---

## 9. Measurable novelty per bench

| Bench | Applicable | Projected Δ |
|---|---|---:|
| `json twitter` | R1, R2, R4, R8 | −1.10 cyc/B → ~3 400 MB/s (from 2 900) |
| `json canada` | R1, R2 (R5 off) | −0.35 cyc/B → ~1 600 MB/s |
| `json data_s` | R1, R2, R4, R5 | −2.6 cyc/B on int records; +20% |
| `json data_xl` | R1, R2, R5, R7 | similar to data_s |
| `json citm` | R1, R2, R4, R7 | whitespace-dominated; ~0.25 cyc/B |
| `css bootstrap` | R1, R2, R7, R12 | +30% (v32 + ShapeRef) |
| `css tailwind` | R1, R2, R7 | ShapeRef-heavy; +25% |
| `bbnf self` | R1, R2, R6 | +15% string rules |
| `sheets` | R1, R5, R6 | +20% numeric-heavy |

C1 gain: 2 cyc/rec × 90k rec/MB ÷ 3.5 GHz ≈ 50 µs/MB-twitter.
Pareto extraction prevents v32-packing L1i bloat past 100 KB/fn.

---

## 10. Novelty assessment

| Rewrite | sonic-rs 0.3.17 | simdjson 3.x | lightningcss 1.28 | Novel? |
|---|---|---|---|---|
| R1 scan-emit fusion | no index | stage-1 yes | no | partial — rewrite expresses declaratively |
| R2 v32 column-pack | AoS u64 | AoS | Rust struct | **novel** |
| R3 visitor tee | — | — | — | **novel** |
| R4 no-escape fast path | always escape-scan | same | — | **novel** |
| R5 integer SWAR bypass | always float | same | N/A | **novel** |
| R6 alphabet-memchr | hand-memchrs | stage-1 | — | not algorithmically; novel as rewrite-selected |
| R7 gated SHAPE_DICT | no ShapeRef | no ShapeRef | — | **novel** (ShapeRef is ours) |
| R8 null-visitor elide | — | — | — | **novel** |

Seven of eight genuinely novel by the hand-tuning-vs-derived test
(AW-V Lever 1 thesis). R6 is known algorithmically; rewrite-driven
selection makes it general — sonic got there by JSON hand-tuning,
R6 lets BBNF/Sheets/CSS get there mechanically.

---

## Appendix A — Rewrite rule syntax table

| Id | Name | Precondition (IR fact) | LHS pattern | RHS pattern | Expected Δcyc |
|---|---|---|---|---:|---:|
| R1 | scan-emit-span-fusion | `StructuralIdx.has_slots(lo,hi)` | `EmitCompound(ScanBytesUntilStructural(span))` | `EmitSpanFromStructuralIndex(slot)` | −0.22/B |
| R2 | v32-column-pack | `disjoint(cols_a, cols_b) ∧ same_record` | `Seq(EmitCompoundScalar_a, EmitCompoundScalar_b)` | `EmitCompoundV32([cols_a ⊔ cols_b])` | −2.0/rec |
| R3 | visitor-tee | `FnDescriptor[v1,v2].is_order_insensitive` | `Seq(VMC(v1,m,a), VMC(v2,m,a))` | `VisitorTee(v1,v2,event=(m,a))` | −0.6/B (multi-visitor) |
| R4 | no-escape-fast-path | runtime `memchr('\\') == None` | `ScanQuotedStringEscaped(bytes)` | `RuntimeGate(cond, ScanQuotedStringFast, ScanQuotedStringEscaped)` | −0.30/string-B × hit rate |
| R5 | integer-swar-bypass | `pattern_alphabet.is_subset_of([0-9])` + runtime len ≤ 15 | `ParseNumberEiselLemire(bytes)` | `RuntimeGate(cond, ParseIntegerSwar, EiselLemire)` | −2.3/digit-B |
| R6 | alphabet-memchr | `alphabet.single_bytes.len() == 1 ∧ digraphs.empty()` | `ScanBytesUntilStructural(bytes, idx)` | `MemchrLoop(needle, input)` | −0.13/B |
| R7 | shape-dict-skip-gate | `profile.shape_rep(hash) < threshold` | `PushShapeRef(hash, span)` | `EmitCompoundV32(full record)` | depends on repetition |
| R8 | trivial-visitor-elide | `fns[v.method].body_is_noop` | `VisitorMethodCall(v, m, _)` | `Elide` | −args-subtree cost |
| R9 | combined-nospace+struct | R1 precondition | `Seq(SkipWsInline, ScanBytesUntilStructural)` | `ScanBytesUntilNonWhitespaceOrStructural` | −0.10/B |
| R10 | v32-assoc | same as R2 | `EmitCompoundV32(a) ∘ EmitCompoundV32(b)` | `EmitCompoundV32([a,b])` | merge-enabler |
| R11 | gate-sink | gate-cond pure | `EmitCompound(RuntimeGate(c,h,k))` | `RuntimeGate(c, EmitCompound(h), EmitCompound(k))` | extraction-enabler |
| R12 | shape-ref-dedup | two `PushShapeRef` with canonically-equal args | `Seq(PushShapeRef(h,s1), PushShapeRef(h,s2))` | `PushShapeRef(h, s1)` (iff spans canonical-identical) | −one record |

---

## Appendix B — Cost-model weights (starting values, M4 P-core @ 3.5 GHz)

| SEIR op | `cyc_op` | `bytes_op` | rationale |
|---|---:|---:|---|
| `ScanBytesUntilStructural` | 14 / stripe = 0.22/B | 320 | neon.rs:59 classify; AW-IV B3 §1 |
| `ScanQuotedStringFast` | 0.15/B | 96 | `vceqq_u8 + vshrn_n_u16`; B3 §4 Tier 1 |
| `ScanQuotedStringEscaped` | 0.45/B | 256 | parity.rs:40-142; B3 §4 Tier 2 |
| `MemchrLoop` | 0.09/B | 64 | `vceqq_u8` only |
| `SkipWsInline` | 0.05/B | 128 | nospace cache amortised; B3 §6 |
| `ParseNumberEiselLemire` | 15 / num | 3200 | decoders.rs:80-411 body |
| `ParseIntegerSwar` | 1.5 / 8 digits | 96 | SWAR by-10-mul |
| `EmitCompoundScalar` | 21 / rec | 48 | 7-col scalar stores |
| `EmitCompoundV32` | 4 / rec | 24 | one NEON Q store |
| `EmitSpanFromStructuralIndex` | 3 / rec | 24 | scalar span write |
| `PushShapeRef` | 12 / rec (probe) + 2 store | 96 | shape_dict probe |
| `PushScalarPayloadF64` | 2 / val | 24 | `str_f64` store |
| `VisitorMethodCall` | 2 / call | per-method | monomorphised |
| `VisitorTee` | 2 × method | 2 × per-method | double-write |
| `Elide` | 0 | 0 | |
| `RuntimeGate` | cond(1) + branch(1) | 32 | correct-prediction path |

Shared cross-tier `CostWeights` defaults (`crates/egraph/src/cost_weights.rs:114-133`):
`structural = 1.0`, `alt_per_branch = 1.5`, `dispatch_bonus = −2.0`,
`tape_push = 1.0`. SEIR adds: `weight_cycles = 1.0`, `weight_bytes =
0.01` (until L1i budget hit, then 10.0), `weight_visitor_tee = −3.0`
(rewards the multi-visitor composition).

---

## Appendix C — Three proof outlines

### C1. R5 correctness — integer SWAR ≡ Eisel-Lemire on `[0-9]+`

Claim: `∀ bytes : [u8] where bytes ⊆ ASCII_DIGIT, bytes.len() ≤ 15 :
parse_integer_swar(bytes).to_bits() == eisel_lemire(bytes).to_bits()`.

Proof.
1. Let `n = bytes.len()`; `D_i = bytes[i] - b'0'` for `i ∈ 0..n`.
2. `parse_integer_swar` computes `N = Σ D_i × 10^(n-1-i)` in `i64`.
   For `n ≤ 15`: `N ≤ 10^15 - 1 < 2^50 < 2^63`, so no overflow.
3. Cast to f64: `N as f64` produces the correctly rounded f64 of `N`.
   For `N < 2^53`, this is exact; for `2^53 ≤ N < 2^54`, it is
   round-to-nearest-even per IEEE 754 — which is the same rule
   Eisel-Lemire applies.
4. Eisel-Lemire on `bytes` enters `core::num::dec2flt::lemire::compute_float<f64>`.
   On inputs matching `[0-9]+` with `n ≤ 19` digits, the decimal
   mantissa `m = N` and exponent `e = 0`. The Clinger fast-path at
   (`std/src/num/dec2flt/lemire.rs`) returns `m as f64` directly.
5. Both paths compute `N as f64`; results are bit-identical. ∎

Caveat: for `n = 16..19` digits, `N` may exceed `2^53`. Still exact
in i64; f64 rounds. R5's `RuntimeGate` only fires on `n ≤ 15` to
keep the proof within the simple range; Eisel-Lemire handles
`n = 16..19`.

### C2. R1 correctness — scan/emit span fusion

Claim: `∀ input, span_start, span_end : u32 where
StructuralIndex::has_slot(span_start) ∧ StructuralIndex::has_slot(span_end) :
EmitSpanFromStructuralIndex(start_slot).span() ==
(span_start, span_end) == EmitCompound(ScanBytesUntilStructural(span)).span()`.

Proof.
1. `StructuralIndex` is built via `scan_structural(input, &alphabet)`
   (`crates/bbnf-simd-scan/src/lib.rs:80`). Invariant:
   `index.positions[k]` is the k-th byte position of an alphabet-
   structural byte in `input`.
2. At a parse site with known `span_start` slot `s`, the compound's
   start byte = `index.positions[s]`. The compound's end byte is the
   next-structural position = `index.positions[s + compound.width]`.
3. `EmitSpanFromStructuralIndex(s)` looks up both positions from the
   index and emits the span; by (1) and (2), this span equals the
   one the re-scan would compute.
4. Boundary: when the close byte is itself not in the alphabet (e.g.,
   EOF), the rewrite's precondition fails (`has_slot(span_end) =
   false`) and the rewrite doesn't fire. ∎

### C3. R3 correctness — visitor tee

Claim: for pure visitors `V1`, `V2`, `∀ input :
VisitorTee(v1, v2).parse(input).events == v1.parse(input).events
== v2.parse(input).events`.

Proof.
1. Parse is deterministic: `parse: &[u8] → [Event]` is a pure function.
   (Proved by construction: no randomness, no external state.)
2. `v1.parse(input)` calls `v1.visit_x(args_x)` for each `Event(x, args_x)`
   in the canonical sequence.
3. `v2.parse(input)` calls `v2.visit_x(args_x)` for each `Event(x, args_x)`
   — same sequence by (1).
4. `VisitorTee(v1, v2).parse(input)` emits `v1.visit_x(args_x); v2.visit_x(args_x)`
   for each event. Modulo ordering of `v1` vs `v2` within one event,
   both visitors see exactly their canonical sequences.
5. Ordering within one event: `v1`'s call precedes `v2`'s. If the
   visitors are order-insensitive (`FnDescriptor::is_order_insensitive
   = true`), both orderings yield identical final state. R3's
   precondition requires this; otherwise rewrite doesn't fire. ∎

Caveat: global mutable state shared between `v1` and `v2` (e.g.,
both writing to a global log) is not supported — `is_order_insensitive`
is a conservative check; static analysis must verify no shared
mutable globals before setting the flag.
