# PSI / DTA / OpenFrame Failure-Mode Anatomy — and SK-V3 Correspondence Audit

Date: 2026-05-12. Read-only investigation. Every claim cites `path:line`.

This document assembles the failure anatomy of the prior 1000+ commit DTA/PSI/OpenFrame era, taxonomises the named failure modes, then audits whether the current SK-V3 plan ("structural-index-driven typed parse" + asmjson-shaped 9-state FSM + 5-variant `BackendShape`) is structurally different — or is the same architecture under different names.

---

## §A — Verbatim Quotes from the Governing Documents

### A.1 Lock 1 — the canonical 5-failure-mode statement

From `/Users/mkbabb/Programming/bbnf-lang/restart/locks/LOCKS.md:34`:

> "The 2,000-commit prior failure was implementation, not concept: orthogonal codepaths (the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology); type ambivalence (tape and OpenFrame and direct-to-struct competing for the same role); substrate-first/consumer-later (Era V failure mode); columnar SoA designed in AV.04 archaeology but never activated. The greenfield's tape lives at `runtime/src/tape/`; typed-value records borrow into it; per-grammar runtime modules (template-emitted at `runtime/src/grammars/<name>/`) emit accessors; one materialisation surface; one Visitor pattern; no parallel substrate."

### A.2 Lock 14 — the per-grammar god-module statement

From `/Users/mkbabb/Programming/bbnf-lang/restart/locks/LOCKS.md:60`:

> "The current overfitting mess — CSS L4 14-variant `OpenFrame`; BBNF aggregator `pub use bbnf::*`; Sheets arena fallbacks; per-grammar registry arms in `bbnf-ir`; `shape_dict_bbnf.rs`; `crates/core/src/css_types.rs`; per-grammar runtime/<g>/ hand-written modules — is the failure mode this lock prevents from recurring."

### A.3 The Era V post-mortem (canonical) — `era-V-dta-psi-rut.md`

From `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:1-15`:

> "Era V is the hard era. In five calendar days, seven tranche surfaces (AV, AW-I, AW-II, AW-III, AW-IV, AW-V, AX) ship ~600 tranche-tagged commits. At Era V's close, every bench entry is *below* the AU-baseline — JSON twitter 486 MB/s (24.7% of AU), CSS / Sheets / BBNF 3–7% of AU — despite a 400-commit substrate build. Era V's signature failure mode is **substrate-first-consumer-later**: every tranche ships the compile-time emission of constants, tables, and shape dictionaries; no tranche fully activates the runtime consumer that reads them."

From `era-V-dta-psi-rut.md:30-47` — the seven substrate pieces AV+AW shipped:

> "1. **DTA (Dispatch Table Automaton)** — a grammar-derived table-driven parser avoiding the recursive `fn __<rule>` descent.
> 2. **PSI (Parallel Structural Index)** — a pre-computed index allowing document-level parallel parse.
> 3. **Columnar tape** (Era IV's columns made first-class).
> 4. **ShapeRef** — compile-time shape dictionary dispatched at the cursor.
> 5. **PHF + SIMD keyword classifiers** — compile-time perfect-hash keyword tables.
> 6. **Bloom + GADT runtime dedup** — shared-substring dedup.
> 7. **Shape emitter** — the unifying substrate. Auto-derives the sonic-rs-class inner loop from any BBNF grammar.
>
> Each of the seven is shipped. None reach break-even parse throughput with the AU baseline before Era V ends."

From `era-V-dta-psi-rut.md:181-188` — AW-V's thesis lost in its own tranche:

> "AW-V closed with 0/17 parse entries exceeding post-AU (JSON twitter 486 MB/s = 24.7% of baseline; CSS/Sheets/BBNF at 3-7% of baseline). Shape-emitter substrate landed for all grammars but only JSON's `parse()` routes through it at runtime... AW-V's thesis — 'auto-derive the sonic-rs-class inner loop from any BBNF grammar' — was demonstrated exactly once, on JSON, at W3 close (commit `c1e86ab3`), and lost by W6.
>
> **AW-V demonstrated the thesis and lost it within its own tranche.** The 'exactly once' at W3 is the peak of the Era V arc."

From `era-V-dta-psi-rut.md:310-314` — the hardest lesson:

> "Novel levers compound only when they share a substrate AND a demonstrable floor. V's substrate-first-consumer-later anti-pattern must not recur." — AX.md proposition 4.

### A.4 The MIGRATION-doc verbatim 86.07% citation

From `/Users/mkbabb/Programming/bbnf-lang/restart/MIGRATION.md:344-349`:

> "OpenFrame/checkpoint-heavy fallback logic | ABROGATE-REPLACE | Tape builder with bounded checkpoints. ... The restart sketch measured `Vec<OpenFrame>::clone` at 86.07 percent inclusive samples in the current path (`restart/corpora/RESTART-SKETCH.md:154-184`). The new runtime must prove OpenFrame clone stacks are gone."

### A.5 The AY-I.W1 column revert — Era IV "durable" decision was not durable

From `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-VI-restart.md:76-82`:

> "**The W1 column revert is the most important architectural reversal of Era VI.** Tranche Y split the tape into 7 columns; AU baked the split into its measurement floor; AY-I.W1 reverts to a single `Vec<TapeRec>` + `sib_skip` because the 7-column AoS lost to cache-locality of a single AoS record. This is direct evidence that an Era IV 'durable' decision (columnar split) was not durable."

### A.6 LESSONS-LEARNED (the rule for orchestrator discipline)

From `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md:17-26`:

> "## 2026-04-29 - Substrate Without Consumer Is Not Progress
>
> - **Source**: bbnf-lang AZ-I/AZ-II plans; speedtest and glass-ui activation gates.
> - **Failure**: reusable substrate landed before the consuming path proved it was live.
> - **Rule**: every substrate change must land with a same-wave consumer or an explicitly declared brittleness window and restoration wave.
> - **Check**: wave hard gate cites a runtime call site, test, benchmark, or deletion proof."

### A.7 Cumulative cost statement

From `era-V-dta-psi-rut.md:303-308`:

> "Roughly 600 tranche-tagged commits (AV 53 + AW 45 + AW-II 40 + AW-III 93 + AW-IV 92 + AW-V 80 + AX 169 = 572) across five calendar days."

(Cross-reference: the user's "1000 commits" framing aggregates Era IV+V; `era-V-dta-psi-rut.md:12-15` reads "the '1000-commit near-implementation of the fault DTA/PSI interpreter' the user framed.")

---

## §B — Failure-Mode Taxonomy (5+1)

### B.1 OpenFrame Clone Parallel Substrate — the 86.07% pathology

**What OpenFrame was.** A speculative-branch checkpoint frame in the prior runtime's `JsonStructBuilder` (and equivalents per-grammar). When a parse rule entered a speculative branch (Alt with shared first-set, error-recovery probe, etc.), the builder *deep-cloned its in-flight stack of OpenFrame records*, attempted the branch, and either committed the clone or discarded it on failure. CSS L4 reached **14 variants** of OpenFrame (per Lock 14 at `restart/locks/LOCKS.md:60`: "CSS L4 14-variant `OpenFrame`").

**Why it cloned.** Because the tape was *write-forward* and the speculative-rollback discipline was implemented as "save the stack, retry, swap back if failed." There was no bounded-rollback primitive on the tape itself; checkpointing was implemented at the builder layer above, by `Vec<OpenFrame>::clone()`. Speculative branches occur often (Alts with shared first-sets in CSS L4 colour functions, etc.).

**Why it ate 86%.** Each speculative checkpoint deep-cloned a `Vec<OpenFrame>` that contained per-frame partially-built compound payload — so the clone cost was proportional to depth × per-frame payload. The samply profile measured this as inclusive 86.07% of `bbnf_value_twitter`. Source: `restart/MIGRATION.md:344-349` cites `restart/corpora/RESTART-SKETCH.md:154-184`. The DEEP-SYNTHESIS post-mortem corroborates per `/Users/mkbabb/Programming/bbnf-lang/docs/GESTALT.md:13`: "86.07% of inclusive samples on `bbnf_value_twitter` are `Vec<OpenFrame>::clone` from `JsonStructBuilder::checkpoint`, the speculative-branch deep-clone discipline that was needed because compile-time-resolved direct projection isn't emitted."

**Residual evidence in current source.** `rg -l OpenFrame crates/core/` returns 10+ matches still (per the prior tool run): generated parsers (`json.rs`, `bbnf.rs`, `css_l4.rs`, `google_sheets.rs`, `css_pretty.rs`), `builder_template.rs`, `google_sheets/arena.rs`. The OpenFrame substrate is **physically present in master** — the greenfield restart promises to retire it on landing.

### B.2 Type Ambivalence — three representations for one role

**Lock 1's named ambivalence.** From `restart/locks/LOCKS.md:34`: "tape and OpenFrame and direct-to-struct competing for the same role."

**The three representations** (concretely):

1. **Tape** — `crates/tape/src/` (Era IV). The columnar substrate Era IV invested in. Stores `TapeRec` + payload arenas + `FusedBuilder` API. Per `era-IV-tape-first.md:88-97` it was the durable surface that survived through every era.
2. **OpenFrame** — the per-grammar speculative builder frame. Per `restart/locks/LOCKS.md:60`: CSS L4 carried 14 OpenFrame variants. Born from "the tape can't roll back, so the builder above it tracks the rollback shape."
3. **Direct-to-struct** — hand-written-per-grammar `bbnf::json::Value` / `bbnf::css::StyleSheet` etc. Per `era-V-dta-psi-rut.md:277-279`: "Hand-coded `bbnf::json::Value` / `bbnf::css::StyleSheet` — briefly landed in AX.W1.A/W1.B; deleted at W1r.0 (`3429aaba`) `Revert W1.A/W1.B (−6,128 LOC); sonic-rs runtime → dev-dep`."

**Why pathological.** Three representations of "what the parse output is" with no single arbiter. The tape carried the structural projection; OpenFrame carried the speculative-rollback view; direct structs carried the typed-API view. Each had its own materialisation path; each path conflicted with the others. No code path could "trust" one without re-deriving from the others. Cost: indirect cloning, double materialisation, dead writes.

### B.3 Substrate-First / Consumer-Later — the Era V signature

**Concrete definition.** Per `era-V-dta-psi-rut.md:1-10`: every tranche ships compile-time emission of constants, tables, shape dictionaries, classifier kernels — and "no tranche fully activates the runtime consumer that reads them."

**The seven dead substrate pieces** per `era-V-dta-psi-rut.md:30-47`: DTA, PSI, columnar tape, ShapeRef, PHF+SIMD classifiers, Bloom+GADT dedup, Shape emitter. **All seven shipped. None reached break-even at Era V close.**

**The empirical record.** Era V closed at `era-V-dta-psi-rut.md:176-181`: "0/17 parse entries exceeding post-AU... CSS/Sheets/BBNF at 3-7% of baseline. Shape-emitter substrate landed for all grammars but only JSON's `parse()` routes through it at runtime."

**The lesson** at `era-V-dta-psi-rut.md:310-314`: "Novel levers compound only when they share a substrate AND a demonstrable floor. V's substrate-first-consumer-later anti-pattern must not recur."

**The orchestrator rule** at `LESSONS-LEARNED.md:17-26`: "every substrate change must land with a same-wave consumer or an explicitly declared brittleness window and restoration wave."

### B.4 Columnar SoA — Designed in AV.04, Never Activated

**The design.** Per `restart/locks/LOCKS.md:259` (Reading list): "`docs/tranches/AV/research/04-columnar-soa.md` — kind-partitioned columnar SoA spec (designed, never activated; cited so the auditor can verify Lock 1 is honoured)."

**The activation.** Era IV's Tranche Y *split* the tape into 7 structural Vecs (per-kind columns: open-object, close-object, open-array, close-array, key, scalar, etc.). Per `era-IV-tape-first.md:39-40`: "Y | 13 | Tape column splits (first columnar substrate) | Worked — columns survive into AU then revert in AY-I.W1."

**The revert.** Per `era-VI-restart.md:64-71`: "**Columns reverted from 7 structural Vecs to 1 `Vec<TapeRec>` + parallel `sib_skip`.** This is the direct revert of Era IV / Tranche Y's column split... AY-I.W1 reverts to a single `Vec<TapeRec>` + `sib_skip` because the 7-column AoS lost to cache-locality of a single AoS record. This is direct evidence that an Era IV 'durable' decision (columnar split) was not durable."

**Why it failed.** Cache locality. Walking 7 parallel Vecs cost more L1/L2 misses than walking one AoS record. The AV.04 "kind-partitioned" variant was an attempted refinement that never made it past spec. Lock 1 keeps columnar SoA **buried**.

### B.5 The 2,000-commit Prior Failure — what shape it took

**The arc.** Era IV (~185 tranche-tagged commits) + Era V (~572) + Era VI (~130) + infill (~300) = **~945 unpushed commits** at the 2026-04-22 archaeology close per `era-VI-restart.md:208-228`.

**The architecture.** Compile-time-emitted dispatch automaton (DTA) + parallel structural index (PSI) + columnar tape (Era IV's split, made first-class) + PHF+SIMD keyword classifiers + shape dictionary (ShapeRef) + bloom+GADT runtime dedup + shape emitter. Per `era-V-dta-psi-rut.md:30-47`.

**The thesis** (from AV.md preamble cited at `era-V-dta-psi-rut.md:19-28`): "ships the dispatch automaton + PSI pipeline + columnar substrate as one coherent architecture, and drives the sonic-rs and lightningcss parity gates."

**The outcome.** 0/17 parse entries at AW-V close exceeded the post-AU baseline. The interpreter was deleted in AX.W0b (per `era-V-dta-psi-rut.md:210-228`). The column split was reverted in AY-I.W1. The shape emitter survived as JSON-only.

### B.6 Per-Grammar God-Modules — the Lock 14 enumerated list

Per `restart/locks/LOCKS.md:60`, the canonical list of overfitting violations:

- **CSS L4 14-variant `OpenFrame`** — per-grammar speculative-builder variants accumulating in `crates/core/src/runtime/`
- **BBNF aggregator `pub use bbnf::*`** — re-export that hides per-grammar entanglement
- **Sheets arena fallbacks** — per-grammar runtime fallback in `crates/core/src/runtime/google_sheets/arena.rs`
- **Per-grammar registry arms in `bbnf-ir`** — `match grammar { Json => ..., CssL4 => ... }` in generic crate
- **`shape_dict_bbnf.rs`** — per-grammar shape dictionary in generic crate
- **`crates/core/src/css_types.rs`** — per-grammar type module in generic crate
- **Per-grammar `runtime/<g>/` hand-written modules** — hand-coded files where template-emission is required

The current grep proves several still exist: `runtime/builder_template.rs` and `runtime/google_sheets/arena.rs` carry OpenFrame; generated parsers carry OpenFrame; `css_types.rs` is on the kill list per `CENSUS-2026-05-03.md`.

---

## §C — SK-V3 → Prior-Failure Correspondence Audit

### C.1 Is asmjson's 9-state FSM the same as PSI's structural index?

**Verdict: structurally different in seven ways; same family of technique.**

**asmjson's 9-state FSM** (per `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:277-299`):
- 9 states: V (value), O (object body), K (key), D (colon), C (comma/close), S (string), F (false), R (true), A (null)
- PC-as-state — `r10` indirect-jump target carries the next-state across chunk boundaries
- Each state has its own classifier mask set (vpcmpeqb / vpcmpub against state-specific byte set)
- Single-pass mask/state walk across the input
- 64-byte chunked classification; tzcnt-driven seek to next interesting byte

**PSI** (per `era-V-dta-psi-rut.md:34-35`): "**PSI (Parallel Structural Index)** — a pre-computed index allowing document-level parallel parse."

**The differences:**

| Axis | PSI (Era V) | asmjson 9-state FSM (SK-V3 §5) |
|---|---|---|
| Number of passes | 2 (prepass produces index; parse consumes it) | 1 (FSM walks once with mask-classify per chunk) |
| Substrate retained | structural index as separate substrate | mask stream is transient producer (Lock 1 clarification: `restart/locks/LOCKS.md:34` "A SIMD mask stream is a transient producer, not a retained sidecar") |
| State carrier | data structure (the index) | program counter (the jump target in `r10`) |
| Parallelism | document-level parallel parse | single-thread; single pass |
| Consumer wiring | "later wave" (the Era V failure) | within the parse function itself (the FSM is the parser) |
| Hardware gate | none (always-on) | CPUID gates `CollapsedStage` only on AVX-512 VBMI2 hosts |
| Grammar generalisation | grammar-derived index but JSON-only activated | per-rule `backend_shape` selection; auto-detected via 8-step cost-model algorithm |

**Critical distinction** at `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:3`: "The sidecar structural-index *prepass* shape is rejected; the retained tape projection IS the structural index (per SK-V3 §3 and Lock 1 clarification)."

This is the architecturally crucial line. SK-V3 explicitly rejects the PSI-shaped sidecar prepass. The structural offsets ARE the tape — there is one substrate, not two.

### C.2 Is `CollapsedStage` BackendShape a re-introduction of the failed substrate?

**Verdict: at risk; gated to AVX-512 VBMI2 only; cost-model selected per-rule.**

`CollapsedStage` is one of five `BackendShape` variants per `restart/ARCHITECTURE.md:1045-1082`:

```rust
EagerTape,       // default; reads source[pos]
OffsetTape,      // typed event cursor over retained offsets
EventTape,       // typed event cursor over event cells with payload
SinkOnly,        // direct-to-struct; no queryable document
CollapsedStage,  // AVX-512-class FSM with mask-held parser state
```

The 8-step derivation algorithm at `restart/ARCHITECTURE.md:1075-1082` gates `CollapsedStage` strictly: "step 6: Else if target features admit AND rule is a hub with ≥ 4 byte-disjoint arms ⇒ `CollapsedStage`."

**Failure-mode correspondence:**

- ✓ **Same hardware concept** as the failed DTA — a state-table-driven automaton over chunked classify.
- ✗ **Different architectural commitment** — `CollapsedStage` is one of five shapes; the cost model picks per-rule from existing Grammar IR facts (Lock 10 auto-detect). DTA tried to be universal; `CollapsedStage` is a single shape in a per-rule taxonomy.
- ✗ **Gated to AVX-512 VBMI2 only** per `SOTA-BEAT-DESIGN.md:265-275`: "feature flag `bbnf-runtime/avx512vbmi2`... grammar-opt-in via metadata."
- ✗ **No retained substrate sidecar** — the FSM is the parse; the mask stream is transient (Lock 1 clarification).

**The corresponding risk.** `CollapsedStage` is the only variant that doesn't retain a queryable tape — it's a SAX-style streaming sink. This is structurally similar to the AW-V "auto-derive the sonic-rs-class inner loop" thesis that demonstrated once on JSON at W3 and was lost by W6. The path to recurrence: if `CollapsedStage` lands first on JSON, becomes the only working shape, and the other 4 shapes drift behind it.

### C.3 Is the typed event cursor over offset tape a re-introduction of the OpenFrame parallel substrate?

**Verdict: structurally different; Lock 1 explicitly addresses this exact question.**

The typed event cursor lives at `restart/README.md:300-318`:

```rust
struct JsonValue<'i> {
    kind: TokenKind,
    span: (u32, u32),
    tape: &'i Tape<'i>,
    idx: u32,
}
```

**Why this is NOT OpenFrame** per `restart/README.md:291`:
- OpenFrame was a *speculative-rollback frame* that cloned on every checkpoint.
- The typed event cursor is a *borrow into one substrate*; it doesn't clone; it doesn't carry rollback state.

**Why this is NOT a parallel substrate** per `restart/locks/LOCKS.md:34`: "tape and direct-to-struct typed values that borrow into it (`&'i Tape<'i>` + cursor)". The cursor IS the borrow into the tape, not a second representation alongside it.

**The risk.** If the speculative-branch discipline returns (Alt with shared first-set producing checkpoints), and the rollback primitive is **not** built into the tape itself, the OpenFrame pattern recurs at the cursor layer. Per `restart/ARCHITECTURE.md:1465`: "Rollback is bounded and does not clone OpenFrame stacks." — this is gated as a tape-level invariant. The gate's verification path is named per `restart/MASTER-PLAN.md:307`: `rg "OpenFrame|Vec<OpenFrame>|ParseStream" crates/runtime/src crates/codegen/src` must return zero.

### C.4 What is structurally different about SK-V3?

Six structural differences from the Era V architecture:

1. **One substrate, not seven.** Era V shipped 7 distinct substrates (DTA, PSI, columnar tape, ShapeRef, PHF, Bloom+GADT, Shape emitter); SK-V3 has one tape ∪ direct-to-struct union with five access shapes (`BackendShape`). Per `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:55`: "No new substrate variant. No parallel substrate. Lock 1 stands."
2. **Cost-model selects per-rule, not per-grammar.** Era V picked DTA-or-recursive per grammar; SK-V3's 8-step `derive_backend_shape` operates per-rule via existing Grammar IR facts (`restart/ARCHITECTURE.md:1075-1082`).
3. **No new BBNF directive; no new BIR variant.** Per `restart/HANDOFF.md:84` ("20-variant BIR alphabet (no new variant; `Alt { Dispatch }` lowers to multiple access patterns)"). Era V proposed `@simd`, `@pratt`, and `@phf` directives.
4. **Consumer wiring is mandatory same-wave** per `LESSONS-LEARNED.md:17-26` (the "Substrate Without Consumer Is Not Progress" rule, dated 2026-04-29 — born of the Era V post-mortem). The Era V failure was substrate-then-consumer-later; SK-V3 dispatches Wave 0 P0.2 (correctness fix) → Wave 1a (NEON kernel + lowerer emission) → Wave 1b (kernel + force-inline) in sequence, each with same-wave consumer per `restart/HANDOFF.md:182-194`.
5. **Empirical credible-win column already exists.** Per `restart/HANDOFF.md:104-114`: "skinny v3 already wins on 4 of 17 corpora against simdjson C++ / yyjson on M5 Max — citm (+43% vs yyjson), canada (+22% vs simdjson), mesh (+6% vs simdjson), unicode_mixed (+10%)." Era V's AW-V demonstrated the thesis once and lost it; SK-V3 has already demonstrated the substrate beats one or both SOTA peers on 4 corpora.
6. **Falsifiability gates per phase** per `SOTA-BEAT-DESIGN.md:346-353` — every phase has a LOC budget, throughput gate, hot-leaf-count gate, and c/B gate. Era V had hard gates ("strict-better-than post-AU on ≥ 15/19 entries" at AW-III; "every entry exceeds post-AU" at AW-IV) but missed every gate. The differential: Wave 2's per-corpus asm pathology + native-sidecar profile provide empirical evidence for the gates, not aspirational targets.

---

## §D — Honest Assessment

### D.1 The user's "1000 commits of attempts" — proximate cause

Per `era-V-dta-psi-rut.md:303-308` the actual count is **572 tranche-tagged commits** across Era V (AV → AX). Aggregated with Era IV (185) + Era VI (~130) + infill (~300) = 945 unpushed commits at the 2026-04-22 close.

**The proximate cause** is named in three places:

1. `era-V-dta-psi-rut.md:310-314` (cited as AX proposition 4): "Novel levers compound only when they share a substrate AND a demonstrable floor. V's substrate-first-consumer-later anti-pattern must not recur."
2. `LESSONS-LEARNED.md:17-26`: "reusable substrate landed before the consuming path proved it was live."
3. `restart/locks/LOCKS.md:34`: "orthogonal codepaths (the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology); type ambivalence (tape and OpenFrame and direct-to-struct competing for the same role); substrate-first/consumer-later (Era V failure mode); columnar SoA designed in AV.04 archaeology but never activated."

The post-mortems converge on: **the architecture was speculative-substrate-first; the consumer never caught up; orthogonal codepaths multiplied; one substrate would have been correct.**

### D.2 Implementation, architectural, or measurement?

**Implementation primarily; architectural secondarily; measurement was not the cause.**

The author's reframe at `restart/README.md:291`: "The user's deep concern: the failure was **implementation**, not naming." And `restart/locks/LOCKS.md:34`: "The 2,000-commit prior failure was implementation, not concept."

But the implementation faults were **systemic** (orthogonal codepaths, parallel substrates, consumer-later sequencing) rather than per-site bugs. The architectural commitment to a separate PSI sidecar + DTA dispatch table created the conditions where multiple representations had to coexist; the implementation faithfully realised those representations and they ate each other.

So: an architectural choice (multiple substrates with separate consumers) constrained the implementation into a shape where no implementation could succeed cleanly. Better implementation could not have rescued PSI-as-sidecar without first collapsing it into the tape. Which is exactly what SK-V3 does at `SOTA-BEAT-DESIGN.md:3`.

Measurement was not the cause. The bench harness was correct. AW-V's failure (`era-V-dta-psi-rut.md:171-189`) was 0/17 corpora exceeding the baseline. The benches told the truth; the architecture didn't make the numbers.

### D.3 Does SK-V3 share any named pathologies?

**Three named risks; each has explicit mitigation:**

| Era V pathology | SK-V3 surface | Mitigation in SK-V3 |
|---|---|---|
| Parallel substrate (PSI sidecar) | Risk: `OffsetTape` + `flags` array + parallel `offsets[]` could become a sidecar | Mitigated per `restart/locks/LOCKS.md:34`: "if structural offsets are retained, the structural projection IS the tape." The offsets array IS the tape; not a sidecar. |
| OpenFrame clone (Vec<OpenFrame>::clone) | Risk: speculative branches (Alt with shared first-set) need rollback | Mitigated by 8-step derivation step 4 (`ARCHITECTURE.md:1078`): "Else if rule's `Alt` first-set has overlap ⇒ `EagerTape` (lowers `Alt` as `Speculative`, not `Dispatch`)." Speculative branches stay on `EagerTape`; rollback is a tape primitive, not a builder primitive. |
| Type ambivalence (tape vs OpenFrame vs direct-struct) | Risk: 5 `BackendShape` variants could become 5 type-ambivalent representations | Mitigated by Lock 1 union: "tape ∪ direct-to-struct." `SinkOnly` is the only direct-only shape; the other 4 all retain `(TapeId, cursor, event_kind_or_payload_class)` identity per `ARCHITECTURE.md:1489-1497`. |
| Substrate-first / consumer-later | Risk: NEON kernels in `bbnf-simd` could land before lowerer emits them | Mitigated per `LESSONS-LEARNED.md:17-26` (the canonical 2026-04-29 rule) + the wave dispatch order in `HANDOFF.md:182-194` which interleaves kernel landing with lowerer emission in the same wave. |
| Per-grammar god-modules | Risk: `CollapsedStage` could become JSON-only and stay JSON-only | Mitigated by per-rule `derive_backend_shape` (every grammar's rules go through the same 8-step algorithm) + Lock 14 verification commands (`rg JsonParser|CssL4Parser|... crates/{ir,parse,...}` must return ZERO). Risk remains: if `CollapsedStage` only ever fires on JSON, the per-grammar drift recurs in the cost model rather than in source code. |

### D.4 If FSM/DTA is wrong in this project's history, what IS right?

The current spec at `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:24-55` calls itself "structural-index-driven typed parse" — a codegen template, not an FSM-shaped substrate. The structural index IS the offset array on the tape; the typed parse cursors over that array; the source is read only inside primitives (parse_string, parse_number).

**Prior-art success in the project archive:**

- The **eager-tape canonical** with `FusedBuilder` API survived Era IV's AU peak and is what JSON twitter measured 1967 MB/s on (per `era-IV-tape-first.md:8-9`). Source: `era-IV-tape-first.md:88-97` ("**Columnar tape** (`crates/tape/`) — the `TapeRec` + payload arenas + `FusedBuilder` API survive through every later era.").
- The **single-Vec + sib_skip** AY-I.W1 revert (per `era-VI-restart.md:64-71`) recovered twitter to 688 MB/s after the column-split regression. This is the substrate AY-I committed to before B0/B1 paused execution.
- The **shape emitter on JSON only** at AW-V W3 (commit `c1e86ab3`) demonstrated the thesis for one grammar — per `era-V-dta-psi-rut.md:175-189`. SK-V3's structural-index-driven template is the architectural generalisation of that one-grammar demonstration.
- The **interpreter deletion at AX.W0b** (per `era-V-dta-psi-rut.md:210-228`) removed the DTA walker entirely. The current `crates/core/` has no `__dta_walker_inline::run` call sites — the dispatch automaton's substrate is already gone. The skinny doesn't resurrect it.

**The shape that has prior-art success:** single-Vec tape + bounded-checkpoint rollback + per-rule access shape + cost-model-derived materialisation plan. SK-V3 is this shape, expanded with `BackendShape` taxonomy that's never been activated.

**The shape that is genuinely untested:** the 5-variant `BackendShape` cost-model auto-detection at the lowerer. The 8-step `derive_backend_shape` algorithm is a 2026-05-12 design (per `restart/HANDOFF.md:51`); no implementation has measured it. The risk it carries is the standard Era V risk: the cost-model side-table is the substrate, the lowerer's consumption is the consumer, and the consumer must land same-wave per `LESSONS-LEARNED.md:17-26`.

### D.5 Genuinely different — or rebrand?

**Verdict: genuinely different in five enumerable axes, but two specific risks recurrence-shaped.**

**Genuine differences:**

1. **One substrate, not seven** — Lock 1 unioned tape with direct-to-struct; `BackendShape` is access pattern, not substrate count.
2. **No PSI sidecar prepass** — `SOTA-BEAT-DESIGN.md:3` explicitly rejects it; offsets ARE the tape.
3. **No new BBNF directive; no new BIR variant** — `HANDOFF.md:84`. Compositional, not additive.
4. **Empirical credible-win column on 4/17 corpora** — `HANDOFF.md:104-114`. Not aspirational.
5. **Per-rule cost-model auto-detection** — `ARCHITECTURE.md:1075-1082`. Not per-grammar bifurcation.

**Recurrence-shaped risks:**

1. **`CollapsedStage` is the closest analogue to the failed DTA** — 9-state FSM with PC-as-state. Gated to AVX-512 VBMI2 hardware + ≥4 byte-disjoint arms. If `CollapsedStage` only ever activates on JSON on Zen 4, the JSON-only god-module pattern recurs at the cost-model level rather than the source level. Mitigation: Lock 14 verification commands check zero per-grammar arms in generic crates; `derive_backend_shape` runs on every grammar's rules. But the empirical question — does `CollapsedStage` actually fire on CSS, BBNF-self, Sheets rules? — is unanswered (the per-grammar matrix at `HANDOFF.md:155-169` shows `CollapsedStage` selected for NONE of the listed grammars; only `OffsetTape` / `EagerTape` / `EventTape`).
2. **The lowerer's `BackendShape` consumption is consumer-side and unmeasured** — `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:351` budgets ~470 LOC for "Phase 2 codegen template" with gate "T1 ≥ 2375 MiB/s (BEAT sonic-rs Value-DOM 2438 MiB/s)." This is the SAME shape as Era V's "substrate ships first, consumer ships later, gate misses" pattern — except the wave dispatch order at `HANDOFF.md:182-194` puts the lowerer emission in the same wave as the NEON kernels (Wave 1a). If the wave dispatch holds to same-wave consumer, the Era V failure does not recur. If the wave dispatch splits substrate from consumer, it does.

**The rebrand check.** Three names changed: "DTA dispatch table" → "`Alt { Dispatch }` BIR payload"; "PSI parallel structural index" → "offset tape projection"; "Shape emitter auto-derive sonic-rs-class inner loop" → "structural-index-driven codegen template." The architectural commitment underlying each rename has changed materially:

- DTA was a runtime walker over a compile-time-emitted dispatch table; `Alt { Dispatch }` is an IR variant that lowers to a `match b` on `source[offsets[cursor]]`. The walker substrate is gone.
- PSI was a separate index; offset tape IS the tape. The sidecar is gone.
- Shape emitter was a generic auto-derive that only ever worked on JSON; structural-index-driven template is per-rule with explicit `BackendShape` selection. The "auto-derive any grammar" claim is unmade; the per-rule explicit shape is made instead.

**The conclusion.** SK-V3 is genuinely different from PSI/DTA, not a rebrand. The specific risk is that `CollapsedStage` resurrects the DTA failure mode if it activates JSON-only; and the lowerer consumer must land same-wave with the substrate kernels per the `LESSONS-LEARNED.md:17-26` rule that the Era V post-mortem produced. Both risks are *named* in the current spec; both have *explicit mitigation*; neither is yet *empirically validated*.

The architecture is sound; the risk is execution discipline. Which is exactly the failure mode Era V's substrate-first/consumer-later was, and which `LESSONS-LEARNED.md` codified as a rule. The rule must hold this time.

---

## §E — File Citations Index

Primary post-mortems:
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` (330 lines — Era V failure-mode anatomy)
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-VI-restart.md` (243 lines — column revert; B0/B1 dev-loop pause)
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-IV-tape-first.md` (171 lines — Tape peak; column split origin)

Governing locks:
- `/Users/mkbabb/Programming/bbnf-lang/restart/locks/LOCKS.md:34` (Lock 1: 5 failure modes named)
- `/Users/mkbabb/Programming/bbnf-lang/restart/locks/LOCKS.md:60` (Lock 14: per-grammar god-modules list)
- `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md:17-26` (Substrate Without Consumer Is Not Progress rule)

Current architecture:
- `/Users/mkbabb/Programming/bbnf-lang/restart/README.md:291-318` (Tape ∪ direct-to-struct union)
- `/Users/mkbabb/Programming/bbnf-lang/restart/ARCHITECTURE.md:1045-1082` (BackendShape 5 variants + 8-step derivation)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:3` (sidecar prepass rejected)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:265-332` (CollapsedStage / 9-state FSM)
- `/Users/mkbabb/Programming/bbnf-lang/restart/HANDOFF.md:104-194` (Cross-parser landscape + wave dispatch)

Migration evidence:
- `/Users/mkbabb/Programming/bbnf-lang/restart/MIGRATION.md:344-349` (86.07% pathology citation)
- `/Users/mkbabb/Programming/bbnf-lang/restart/MASTER-PLAN.md:307` (verification grep: zero OpenFrame/ParseStream)

OpenFrame physical residue in master (still present 2026-05-12):
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/builder_template.rs`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/json.rs`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/bbnf.rs`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/css_l4.rs`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/css_pretty.rs`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/google_sheets.rs`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/google_sheets/arena.rs`
