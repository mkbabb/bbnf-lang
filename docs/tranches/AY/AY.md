# Tranche AY — Beat Sonic-rs: AU Substrate Restoration + Direct-to-Struct + Value API + Legacy Prune

AY is the AX continuation: performance + projection + closure. Successor to AX; predecessor to AZ (AZ carries replay/recovery/incremental tooling, `docs/tranches/AZ/AZ.md`). AX closed the interpreter at runtime (W0b), landed the shape-emission-authoritative tape (W0a), reverted hand-coded Value duplicates (W1r.0), proved grammar-derived view surface via canonical-serialization byte equality (W1r.2/3a/4a/5), audited the typed-accessor surface (W1r.6). Eight W1r sub-waves landed (`3429aaba`..`ab7c218d`), 13 parity + canonical harnesses green on master (247/1 ignored).

**AU archaeology revealed a ~4.5× regression from post-AU to current master.** Post-AU (`3b8b757`, Apr 15) shipped twitter at **1,967 MB/s = 0.615 bytes/cyc = 76% of sonic-rs**; citm at **94% of sonic-rs**. Current master (411eabfd): twitter at 437 MB/s = 0.137 bytes/cyc = 17% of sonic-rs. The regression chain: SoA 7-column write pivot (AV/AW-I) stacked atop compound-wrap tax introduced by AW-V.W6's gate over-admission. Both landed without perf gating; parity tests pass but the speed ceiling collapsed. `json-prototype` (AW-V.W2.1) demonstrated the underlying substrate is still capable of sonic parity (0.89-0.94× of sonic on 5 JSON fixtures); the regression is entirely emitter-shape + tape-substrate-layer.

**AY targets BEATING sonic-rs by 20-40% on twitter + canada + citm.** Not approaching, not matching — beating. The path is AU substrate restoration + e-graph G3 wrap-elision + json-prototype per-shape inline emission pattern + SIMD unescape + Eisel-Lemire direct-to-column. Each is load-bearing; each has archaeology as proof of feasibility.

Ten fresh audits (A1–A10) at master HEAD `411eabfd` establish the performance floor + absorption scope:

- **Parse throughput**: JSON 5.5-8.2× slower than sonic-rs (4.5× of that gap is AU regression, per AU FINAL bench table); CSS L4 **beats lightningcss 19-40%** at scale; Sheets/BBNF stable (+2-5% vs W0a.close).
- **Universal hot-path**: `Columns::push_structural` (23-43%) + `tape::finaliser::finalise` (12-27%) = **50-70% of every grammar's self-time**. Both symbols were introduced post-AU; both are direct regressions from AU's flat AoS + unified-arena substrate.
- **DTA at runtime**: fully retired (A8 cert). Zero DTA symbols in 4 bench binaries; zero DTA frames in samply profiles.
- **DTA at source**: ~2,300 LOC of kernel-dead legacy. AY.W0 prunes.
- **Structural alphabet**: mined but orphaned; walker retirement broke the reader chain. AY.W1 revives.
- **Named-type collapse**: `TypeDesc::Named(_)` drops before Rust emit. AY.W2 fixes.
- **AX W2-W15 absorption**: per A7 — 6 fold, 2 new, 3 RETIRE-as-obsolete, 1 defer AZ, 4 defer post-AZ.
- **AU regression triad**: (a) SoA 7-column write adds 7× bookkeeping per push vs AU's flat `Vec<TapeRec>`; (b) compound-wrap emission on every value/alt rule doubles the tape record count; (c) `push_structural` cross-crate call boundary not inlined despite `#[inline]` hint.

AY executes. No deferrals. No hand-coded Value duplicates. No substrate-without-consumer landings. No new grammar directives (invariant 4). Every wave ships substrate + consumer (invariant 2). All AY invariants are AX's 1-21 plus three new architectural gates: tape substrate inline (22), Named preservation end-to-end (23), Value API apples-to-apples work-matching (24).

## Architectural thesis

Six propositions:

1. **AU's substrate was near-optimal; restore it.** Post-AU shipped twitter at 1,967 MB/s = 76% of sonic-rs with flat `Vec<TapeRec>` + unified `push_leaf_with(kind, PayloadData)` + per-grammar push_fingerprint. The SoA 7-column pivot (AV/AW-I) was gated on a read-side microbench (1.94× canada f64-sum) without measuring write-side cost. Write-side cost dominated the parse path: 7 Vec pushes per record vs 1. The AoS sidecar we landed in W1.D is orthogonal — it's a read-side cache, not a write-side gain. AY.W1 reverts the write path to AoS `Vec<TapeRec>`, keeps the AoS sidecar as the read cache. This alone is a 2× recovery on twitter.

2. **Compound-wrap emission doubles the tape.** Current emitter wraps every `value` rule in a compound record: `mark_children` + N child pushes + `push_compound`. For JSON: 100K scalars → ~200K tape records (100K leaves + 100K wraps). Sonic-rs has no wrap — `Value::Number(f)` IS the node. E-graph G3 (Wrap-of-epsilon elision) + wrap-elision classification: single-variant Alt or scalar-projecting rules don't emit wrap compounds. Cuts twitter record count 50%.

3. **json-prototype is the speed-ceiling oracle; emit its shape.** AW-V.W2.1 (`4fdef7c3`, `0dcf9743`) hand-tuned JSON parser hit 0.89-0.94× of sonic across 5 fixtures. Five `#[inline(always)]` per-shape fns, no walker residue, inline SIMD + Eisel-Lemire. This shape is grammar-derivable: the shape emitter produces the equivalent layout when Wrap-of-epsilon elides + push_structural inlines + payload-data writes bypass arena. AY.W3 makes this the emission contract.

4. **Direct-to-struct must reach emit.** AU's invariant 2 (every `->` reaches tape) failed on Bugs 1 + 2. AY.W2 merges Named-preservation (A6) with e-graph G1-G9 cost-model repair (A7 absorbed) — they're the same work surface (both touch `crates/ir/src/egraph/` + `crates/ir/src/passes/metadata.rs`).

5. **Three-consumer surface, two perf benches + one correctness test.** NodeView is lazy, serialize_compact is text, `to_value` is eager. The API is ONE substrate (tape) with three consumer patterns. Benches measure **lazy-get-by-path** + **eager-materialized** (matched-work vs sonic-rs `get_by_path` + `from_str::<Value>` respectively). Canonical-serialize is the round-trip correctness gate (already landed W1r.2), NOT a perf lane.

6. **Legacy prune is precondition housekeeping.** ~2,300 LOC of DTA kernel-dead + stale wire-contract tests + dead GrammarProfile fields pollute both compile-time and intellectual scope. Retire in W0 before any substrate work.

## Invariants

All AX invariants 1-21 carry forward verbatim. AY adds three:

22. **Tape substrate inline + AoS primary.** Write path is flat `Vec<TapeRec>` — 16 bytes per record, one `Vec::push` per structural record. `push_structural` (if retained for back-compat) is `#[inline(always)]` + LTO-verified inlined at every emit site. `push_leaf_with(kind, PayloadData)` is the sole payload-bearing leaf entry point (restoration of AU.6.7). Finalise fusion: sib_skip + child_off + span_hi back-patches at `close_compound` rather than a post-pass sweep. Wave close verifies via `nm` (zero cross-crate tape push exports) + samply self-time shifting from `tape` crate into per-rule `parse_*` functions.

23. **Named preservation end-to-end + wrap-compound elision.** Every grammar-declared `-> input : <Name>` annotation (non-scalar per scalar-name table) reaches the Rust tape emitter as `TypeDesc::Named(sid)`. `emit_direct_to_struct_projection` admits the rule and emits the runtime shim + aggregate payload pathway. Additionally: e-graph G3 (Wrap-of-epsilon elision) + G1-G2 (Alt/Repeat-of-single) + G4 (adjacent-literal concat) fire as IR-canonicalisation passes before emit; rules whose body collapses to a single scalar-projecting form don't emit a wrap compound. Enforced by `named_type_preservation.rs` + `wrap_compound_elision.rs` wire-contract tests.

24. **Value API apples-to-apples.** Bench comparisons vs external comparators are work-matched along one of three surfaces: (a) canonical-serialize text equality (W1r.2 landed; correctness test, not perf bench), (b) lazy-get-by-path (bbnf `Parsed::get<T>(path)` vs sonic-rs `get_by_path`), (c) eager-to-eager (bbnf `parsed.to_value::<T>()` vs sonic-rs `from_str::<Value>`). Mixed-work comparisons forbidden in reported headline ratios.

Invariant 2 (substrate-with-consumer) discharges retroactively for `structural_alphabet` mining that shipped without reader since W0b — AY.W1 activation closes the cycle. Invariant 14 (gate-predicate symmetry) discharges retroactively for 5 stale wire-contract tests whose predicates retired in W0a.2.j / fields carved in W0b.A.

## Operational posture

1. **Bench-checkpoint every wave** with bytes/cyc attribution, not just ns/iter. Saved to `docs/benchmarks/post-AY-W<N>.json`. Regression ≥ 5% triggers re-plan per AX invariant 10. Additionally: BEAT-sonic target is a hard gate at W7 close.

2. **Fresh profile-prepare at mid-tranche** — re-run `scripts/prepare-profile-wave.sh` at W3 close for W4+ agents. Prevents stale-profile recurrence.

3. **Wire-contract tests for every new invariant.** Invariant 22 → `post-AY-W1-close-nm.txt` + samply shift. Invariant 23 → `named_type_preservation.rs` + `wrap_compound_elision.rs`. Invariant 24 → `value_api_apples_to_apples.rs`.

4. **Scope-reveal Absorb** per SPEC. Wave scope-reveal reopens as sub-wave in place; no letter pivot mid-execution. Exception: if any wave's scope-reveal threatens the BEAT-sonic target, orchestrator flags to user immediately.

5. **No grammar DSL additions.** Invariant 4 preserved. Every change is IR pipeline / tape substrate / codegen / bench-harness / IR-consumer wiring.

6. **AU archaeology drives every lever.** AU FINAL bench numbers are the restoration target for Phase 1 (W1 + W2); json-prototype numbers are the ceiling target for Phase 2 (W3 + W4). Every wave close cites AU delta.

## Wave summary

Nine waves. Dependency chain: **W0 → W1 → {W2 ∥ W4} → W3 → W5 → W6 → W8 → W7**. W2 and W4 parallelize after W1; W8 (parallel fork) sits after W6.

| Wave | Spec | Headline | AU-delta target (twitter bytes/cyc) |
|------|------|----------|-----------:|
| **AY.W0** | [waves/W0.md](waves/W0.md) | Legacy prune (~2,300 LOC) + stale tests + ebnf_prettify + housekeeping + AX.FINAL | — (no perf) |
| **AY.W1** | [waves/W1.md](waves/W1.md) | **AU AoS substrate revert** + finalise fusion + structural-alphabet activation + Pratt Option C + unified `push_leaf_with` | 0.137 → **~0.45** (AU restoration floor; matches AU citm) |
| **AY.W2** | [waves/W2.md](waves/W2.md) | Named preservation + **E-graph G1-G9 canonicalisation** + **Wrap-compound elision** + detector retirement | 0.45 → **~0.85** (match sonic) |
| **AY.W3** | [waves/W3.md](waves/W3.md) | Value API: runtime substrate (W3a) + `<Grammar>Value` + **json-prototype per-shape inline fn pattern** (W3b) + two bench lanes + correctness test (W3c) | 0.85 → **~1.00** (beat sonic on eager lane) |
| **AY.W4** | [waves/W4.md](waves/W4.md) | Regex-scan specialisation + **SIMD unescape for strings** + **Eisel-Lemire direct-to-column** + BoundedRegex | 1.00 → **~1.15-1.40** (BEAT sonic by 20-40%) |
| **AY.W5** | [waves/W5.md](waves/W5.md) | CSS L4 @import split + DFA hoist + shared PHF (compile-time A/B/D) | — |
| **AY.W6** | [waves/W6.md](waves/W6.md) | parse_that de-generic + ax-iter profile tuning (compile-time C/E) | — |
| **AY.W8** | [waves/W8.md](waves/W8.md) | Document-parallel fork (AX.W9 demoted) | 1.40 → **2.0+** with parallelism multiplier on ≥ 1 MB inputs |
| **AY.W7** | [waves/W7.md](waves/W7.md) | FINAL — bench matrix + FINAL.md + cssparser parity + CI-gate activation + AZ handoff + BEAT-sonic declaration | — |

## AY → AZ handoff contract

Eight conditions must verify clean before AZ opens:

1. `cargo test --workspace` green (AY.W0 retires 5 stale tests + fixes ebnf_prettify; all subsequent waves maintain).
2. `post-AY.json` bench matrix captured over 18 parse entries + 12 Value-API-lane entries (2 lanes × 5 fixtures + 2 lazy-get spot).
3. **BEAT-sonic**: `bbnf_value_twitter / sonic_value_twitter ≤ 0.85` (bbnf at least 15% faster than sonic eager). Ideal: ≤ 0.75 (25% faster).
4. **BEAT-sonic (multiple)**: same ratio holds on ≥ 3 of the 5 JSON fixtures.
5. `named_type_preservation.rs` + `wrap_compound_elision.rs` + `value_api_apples_to_apples.rs` all green.
6. `nm` on all 4 prebuilt bench binaries: zero `push_structural` / `finalise::finalise` cross-crate exports (invariant 22); zero `DtaState::` constructions (DTA prune verified).
7. `PROJECTION_DIRECT_TO_STRUCT` const has ≥ 4 entries (invariant 23).
8. `structural_alphabet` has a runtime consumer (`nm` shows `scan_structural` symbol).

## Defensible floor

Per A7-A10 integrated findings + AU archaeology, the floor is **seven items**:

1. **W0 legacy prune** — ~2,300 LOC deletions; no architectural risk.
2. **W1 AoS revert** — flat `Vec<TapeRec>` write path restoration. Single 16-byte store per record. Closes 2× of the AU regression.
3. **W1 finalise fusion + Pratt Option C + structural-scan activation** — substrate-only; matches AU's flat-tape write hot path.
4. **W2 Named preservation + e-graph G1-G9** — single IR-passes fix; wire-contract tested.
5. **W2 wrap-compound elision (G3)** — IR canonicalisation cuts tape record count 50%.
6. **W3a + W3b Value emitter + json-prototype shape** — runtime handle substrate + TypeDesc-collapse `<Grammar>Value` emission + per-shape inline fn pattern.
7. **W7 FINAL** — bench matrix + BEAT-sonic declaration + FINAL.md + AZ handoff.

These seven land even under scope-reveals in W4/W5/W6/W8. Items 1-6 deliver bytes/cyc ≥ 0.85 on twitter (match sonic). Item 4's SIMD unescape + Eisel-Lemire direct-to-column in W4 is the BEAT margin.

## Post-tranche review candidates

Decision at AY.W7 close, not mid-wave:

- **CSS `calc()` semantic evaluator** (W1r.3a scope-reveal) — bootstrap/tailwind byte-parity against lightningcss requires calc() arithmetic canonicalization. Dedicated workstream OR drop to AZ tooling scope.
- **Shape-dispatch detector retirement** (AX.W12) — folds into AY.W2 e-graph work; detectors retire as e-graph G5-G9 subsume their logic.
- **Multi-visitor pairs + multi-key SIMD compare** (AX.W14) — small LOC, small gain; bench-gated at W7 close.

## Indefatigability

When AY closes:

- **BEAT sonic-rs by 15-40% on twitter + citm + canada eager-materialised bench.** First Rust JSON parser to beat sonic-rs at its own benchmark.
- Tape hot-path is flat AoS `Vec<TapeRec>` write + lazy AoS sidecar read; no cross-crate call boundary on emit.
- Structural alphabet + CTNS reactivated; dense-alphabet pre-scan consumed at `parse()` entry.
- Pratt Option C within-compound inline; reducer-compound tree preserved (parity unbroken).
- Direct-to-struct projection fires for every grammar-declared `-> input : <Name>`; `PROJECTION_DIRECT_TO_STRUCT` const has ≥ 4 entries.
- E-graph G1-G9 universal + per-shape rewrites active; wrap-compound emission eliminated; shape-dispatch detectors retired (~1,676 → ~150 LOC classifier cleanup).
- Value API apples-to-apples bench matrix populated across canonical-serialize (correctness) + lazy-get + eager-materialized (2 perf lanes).
- CSS L4 compile-time + cache-size reduction on top of W1r.3a's 69% drop.
- Legacy pruning: ~2,300 LOC deleted (dta.rs kernel-dead 458, 5 stale tests 1,390, dead GrammarProfile 150, shape_dict 79, 2 emitter tests 220).
- Document-parallel fork delivers amortisation multiplier on ≥ 1 MB inputs — combined with single-thread beat, total twitter throughput ≥ 5-7 GB/s on multi-core (surpasses simdjson's multi-core record).
- Five stale wire-contract tests retired; ebnf_prettify recognizer green; `cargo test --workspace` clean.
- AX FINAL.md written; AZ handoff artefacts ready.
- No hand-coded Value duplicates; no third-party comparator bridges; no substrate-without-consumer landings; no placeholder variants.
- Invariants 22-24 + AX 1-21 all gated by per-wave wire-contract tests and samply attribution.
