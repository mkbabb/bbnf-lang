# Tranche AY — Tape Substrate + Direct-to-Struct + Value API + Legacy Prune

AY is the AX continuation: performance + projection + closure. Successor to AX; predecessor to AZ (AZ carries replay/recovery/incremental tooling, `docs/tranches/AZ/AZ.md`). AX closed the interpreter at runtime (W0b), landed the shape-emission-authoritative tape (W0a), reverted hand-coded Value duplicates (W1r.0), proved grammar-derived view surface via canonical-serialization byte equality (W1r.2/3a/4a/5), audited the typed-accessor surface (W1r.6). Eight W1r sub-waves landed (`3429aaba`..`ab7c218d`), 13 parity + canonical harnesses green on master (247/1 ignored).

Ten fresh audits (A1–A10) at master HEAD `411eabfd` establish the performance floor + absorption scope:

- **Parse throughput**: JSON 5.5-8.2× slower than sonic-rs; CSS L4 **beats lightningcss 19-40%** at scale; Sheets/BBNF stable (+2-5% vs W0a.close).
- **Universal hot-path**: `Columns::push_structural` (23-43%) + `tape::finaliser::finalise` (12-27%) = **50-70% of every grammar's self-time**. No scanner, no dispatch, no allocator is the primary loss — the tape substrate's write + post-pass IS the loss.
- **DTA at runtime**: fully retired. A8 certified 4 bench binaries (`nm` + samply profile.json.syms.json) — zero DTA symbols, zero DTA frames. `DtaStateId::NONE` in `generated.rs` is a compile-time sentinel, never constructed non-NONE at runtime.
- **DTA at source**: ~2,300 LOC of kernel-dead legacy (`dta.rs` 458 LOC, 5 stale wire-contract tests 1,390 LOC, dead `GrammarProfile` fields 150 LOC, `shape_dict.rs` 79 LOC, 2 stale emitter tests 220 LOC). AY.W0 prunes.
- **Structural alphabet**: mined at `crates/ir/src/passes/sets/structural_alphabet.rs` + emitted to `generated.rs` + populated in `GrammarProfile` — **but zero runtime consumers**. Walker retirement orphaned the reader; simd-scan kernels compile as dead weight. AY.W1 revives with a dense-alphabet pre-pass.
- **Named-type collapse**: `TypeDesc::Named(_)` drops before Rust emit on every grammar. Five tranches of `BINDINGS` substrate were dead code. AY.W2 surgically fixes with a 3-pass guard + wire-contract test.
- **AX W2-W15 absorption**: per A7 — 6 fold into AY waves, 2 open new AY waves (W3 LazyRef, W8 parallel fork), 3 RETIRE as obsolete (W4 SIMD micro-kernels, W5 CSS SIMD cluster, W8 speculative parsing — all superseded by fresh attribution), 1 defers to AZ (W13 autotune), 4 defer post-AZ (e-graph rewrites).

AY is the execution tranche for these findings. No deferrals. No hand-coded Value duplicates. No substrate-without-consumer landings. No new grammar directives (invariant 4). Every wave ships substrate + consumer (invariant 2). All AY invariants are AX's 1-21 plus three new architectural gates: tape substrate inline (22), Named preservation end-to-end (23), Value API apples-to-apples work-matching (24).

## Architectural thesis

Five propositions:

1. **The tape substrate's write path is the universal floor.** Every grammar, every fixture, every profile: `push_structural + finalise` dominate. Substrate-level fix — inline `push_structural` at emit call sites, fuse `finalise` back-patches into compound-close — is semantics-preserving, parity-gated, expected 20-40% delta across all grammars. The JSON gap vs sonic-rs collapses from 7-8× to 3-4× on this change alone.

2. **Direct-to-struct must reach emit, or invariants 20/21 are unenforced.** Eighteen months of `-> input : <Name>` substrate built a consumer-ready pathway for a declaration the pipeline silently discards. A6 narrows the collapse to two IR-pass hypotheses (egraph cost-guided extraction OR alias/transparent stamping pre-normalizer-loop); AY.W2 empirically discriminates, ships the fix, gates with a per-grammar wire-contract test.

3. **Apples-to-apples requires three lanes.** A10's TypeDesc-collapse + Handle-into-Tape materialization delivers a grammar-emitted `<Grammar>Value` enum (invariant 21 compliant — variants enumerate grammar rules, not comparator shape) + `parsed.to_value::<T>()` method + three work-matched bench lanes: canonical-serialize (landed W1r.2), lazy-get-by-path (new), eager-materialized (new).

4. **The structural-alphabet substrate is live code pending activation.** A9 verified the mining pipeline fires, emits to generated.rs, populates `GrammarProfile` — no consumer since W0b. AY.W1 revives with a dense-alphabet pre-pass emitting `scan_structural(input, &alphabet)` at `parse()` entry + feeding `ScanState` into CTNS + skip_space. Expected CSS L4 -10 to -13pp of the 26% regex_scan share.

5. **Pratt Option C + legacy prune are load-bearing housekeeping.** Flat Pratt tape (W0a.2.k) regressed CSS+Sheets parity and reverted; Option C preserves the reducer-compound tree while inlining `push_leaf_with_arena_payload` + hoisting op_stack capacity. Pratt cost is 8-9% Sheets stress — not standalone-worthy, folds into AY.W1. Legacy prune (DTA kernel-dead 458 LOC + 5 stale tests 1,390 LOC + dead profile fields + shape_dict) is invariant-14 discharge for carved predicates.

## Invariants

All AX invariants 1-21 carry forward verbatim. AY adds three:

22. **Tape substrate inline.** `tape::columns::Columns::push_structural`, `tape::finaliser::finalise`-equivalent back-patch paths, and `TapeBuilder::push_leaf_with` inline at every emit call site. No cross-crate call-boundary overhead on hot-path record emission. Wave close verifies via `nm` + samply self-time shifting from `tape` crate into per-rule `parse_<shape>_<grammar>_<rule>` functions.

23. **Named preservation end-to-end.** Every grammar-declared `-> input : <Name>` annotation (where `<Name>` is non-scalar per the scalar-name table) reaches the Rust tape emitter as `TypeDesc::Named(sid)`. `emit_direct_to_struct_projection` admits the rule and emits the runtime shim + aggregate payload pathway. Enforced by per-grammar `named_type_preservation.rs` wire-contract test.

24. **Value API apples-to-apples.** Bench comparisons vs external comparators are work-matched along one of three lanes: (a) canonical-serialize text equality (W1r.2 template), (b) lazy-get-by-path (bbnf `Parsed::get<T>(path)` vs sonic-rs `get_by_path`), (c) eager-to-eager (bbnf `parsed.to_value::<T>()` vs sonic-rs `from_str::<Value>`). Mixed-work comparisons forbidden in reported headline ratios.

Invariant 2 (substrate-with-consumer) discharges retroactively for the `structural_alphabet` mining that has shipped without reader since W0b — AY.W1 activation closes the cycle. Invariant 14 (gate-predicate symmetry) discharges retroactively for 5 stale wire-contract tests whose predicates retired in W0a.2.j / fields carved in W0b.A.

## Operational posture

1. **Bench-checkpoint every wave.** `cargo bench` at wave close, saved to `docs/benchmarks/post-AY-W<N>.json`. Regression ≥ 5% triggers re-plan per AX invariant 10.

2. **Fresh profile-prepare at mid-tranche.** Re-run `scripts/prepare-profile-wave.sh` at W3 close to re-ground W4+ agent profiles. Prevents stale-profile recurrence (per `00-session-recap.md` §5).

3. **Wire-contract tests for every new invariant.** Invariant 22 → `docs/benchmarks/post-AY-W1-close-nm.txt` + samply shift document. Invariant 23 → `crates/core/tests/named_type_preservation.rs`. Invariant 24 → `crates/core/tests/value_api_apples_to_apples.rs`.

4. **Scope-reveal Absorb.** Per SPEC. Wave scope-reveal reopens as sub-wave in place; no letter pivot mid-execution.

5. **No grammar DSL additions.** Invariant 4 preserved. Every change is IR pipeline / tape substrate / codegen / bench-harness / IR-consumer wiring.

## Wave summary

Nine waves. Dependency chain: **W0 → W1 → {W2 ∥ W4} → W3 → W5 → W6 → W8 → W7**. W2 and W4 parallelize after W1; W8 (parallel fork) sits after W6.

| Wave | Spec | Headline | Opens after |
|------|------|----------|-------------|
| **AY.W0** | [waves/W0.md](waves/W0.md) | Legacy prune (~2,300 LOC) + stale tests + ebnf_prettify + housekeeping + AX.FINAL | tranche open |
| **AY.W1** | [waves/W1.md](waves/W1.md) | Tape substrate inline + finalise fusion + structural-alphabet activation + Pratt Option C (incremental) | W0 |
| **AY.W2** | [waves/W2.md](waves/W2.md) | Named-type preservation + direct-to-struct activation + wire-contract | W1 |
| **AY.W3** | [waves/W3.md](waves/W3.md) | Value API: runtime substrate (W3a) + grammar-emitted `<Grammar>Value` (W3b) + three bench lanes (W3c) | W2 |
| **AY.W4** | [waves/W4.md](waves/W4.md) | Regex-scan specialisation + BoundedRegex + L1/L2 miner inheritance | W1 (parallel with W2) |
| **AY.W5** | [waves/W5.md](waves/W5.md) | CSS L4 @import split + DFA hoist + shared PHF (compile-time A/B/D) | W3 |
| **AY.W6** | [waves/W6.md](waves/W6.md) | parse_that de-generic + ax-iter profile tuning (compile-time C/E) | W5 |
| **AY.W8** | [waves/W8.md](waves/W8.md) | Document-parallel fork (amortisation multiplier; AX.W9 demoted) | W6 |
| **AY.W7** | [waves/W7.md](waves/W7.md) | FINAL — bench matrix + FINAL.md + cssparser parity + CI-gate activation + AZ handoff | W8 |

## AY → AZ handoff contract

Seven conditions must verify clean before AZ opens:

1. `cargo test --workspace` green (AY.W0 retires 5 stale tests + fixes ebnf_prettify; all subsequent waves maintain).
2. `post-AY.json` bench matrix captured over 18 parse entries + 10 eager-lane entries (invariant 10).
3. AY.W2 `named_type_preservation.rs` test passes for every grammar with `-> input : <Name>` annotations.
4. AY.W3 eager-lane bench: JSON `bbnf.to_value::<JsonValue>(twitter)` within 1.5× of the parse-only baseline at W3 start (A10-recommended gate; post-tape-fix realistic floor).
5. `nm` on all 4 prebuilt bench binaries: zero `push_structural` / `finalise::finalise` cross-crate exports (invariant 22); zero `DtaState::` constructions (DTA prune verified).
6. `PROJECTION_DIRECT_TO_STRUCT` const has ≥ 4 entries (3 CSS L4 + 1 JSON; invariant 23).
7. `structural_alphabet` has a runtime consumer (`nm` shows `scan_structural` symbol + samply shows nonzero self-time on dense-alphabet fixtures).

## Defensible floor

Per A7-A10 integrated findings, the floor is **six items** (up from v1's 5):

1. **W0 legacy prune** — ~2,300 LOC of file deletions + test retirement; no architectural risk.
2. **W1 tape inline** — `#[inline(always)]` + call-site monomorphisation on `push_structural`.
3. **W1 Pratt Option C** — inline `push_leaf_with_arena_payload` + op_stack capacity hoist; zero consumer exposure.
4. **W2 Named preservation** — single-pass guard in `metadata.rs` or `egraph/` cost-model; wire-contract test.
5. **W3a + W3b Value emitter** — runtime handle substrate + TypeDesc-collapse `<Grammar>Value` emission + Handle-into-Tape materialization.
6. **W7 FINAL** — bench matrix + FINAL.md + AZ handoff.

These six land even under scope-reveals in W4/W5/W6/W8.

## Post-tranche review candidates

Decision at AY.W7 close, not mid-wave:

- **CSS `calc()` semantic evaluator** (W1r.3a scope-reveal) — bootstrap/tailwind byte-parity against lightningcss requires calc() arithmetic canonicalization. Dedicated workstream OR drop to AZ tooling scope.
- **E-graph universal rewrites G1-G4 + per-shape G5-G9** (AX.W10-11 deferred) — if AY.W1's structural-scan activation + W4's regex specialisation land strong, the e-graph rewrite lever's amortised cost may be uncompetitive. Bench-delta-gated decision.
- **Shape-dispatch detector retirement** (AX.W12) — classifier LOC cleanup (~1,676 → ~150). Deferrable until e-graph decision.
- **Multi-visitor pairs + multi-key SIMD compare** (AX.W14) — small LOC, small gain; bench-gated.

## Indefatigability

When AY closes:

- Tape hot-path inline + finalise fused; no cross-crate call boundary on emit.
- Structural alphabet + CTNS reactivated; dense-alphabet pre-scan consumed at `parse()` entry.
- Pratt Option C within-compound inline; reducer-compound tree preserved (parity unbroken).
- Direct-to-struct projection fires for every grammar-declared `-> input : <Name>`; `PROJECTION_DIRECT_TO_STRUCT` const has ≥ 4 entries + `__named_type_shim_<name>` markers emit.
- Apples-to-apples bench matrix populated across canonical-serialize + lazy-get + eager-materialized lanes.
- JSON gap vs sonic-rs cut from 7-8× to 3-4× (substrate) + 1.5× of parse baseline on eager lane.
- CSS L4 compile-time + cache-size reduction on top of W1r.3a's 69% drop.
- Legacy pruning: ~2,300 LOC deleted (dta.rs kernel-dead 458, 5 stale tests 1,390, dead GrammarProfile 150, shape_dict 79, 2 emitter tests 220).
- Document-parallel fork delivers amortisation multiplier on ≥ 1 MB inputs.
- Five stale wire-contract tests retired; ebnf_prettify recognizer green; `cargo test --workspace` clean.
- AX FINAL.md written; AZ handoff artefacts ready.
- No hand-coded Value duplicates; no third-party comparator bridges; no substrate-without-consumer landings; no placeholder variants.
- Invariants 22-24 + AX 1-21 all gated by per-wave wire-contract tests and samply attribution.
