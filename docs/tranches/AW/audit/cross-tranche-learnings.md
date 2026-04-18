# Cross-Tranche Learnings — W through AW-V (24 tranches, ~1500 commits)

## 1. Angle headline

Across the W→AW-V arc the codebase iterated on the same four architectural questions — *where does dispatch live, where does the payload live, where does the optimizer live, and how is activation verified* — and answered them with ~19 substrates that were built, inhabited briefly, and then rebuilt on adjacent foundations. The arc's meta-lessons are not about individual levers; they are about the orchestration contract that lets substrates land **with their consumers**. Every regression in the arc traces to a single root pattern — **substrate without consumer**, canonised as Anti-Pattern #1 in `docs/tranches/AW/audit/SYNTHESIS.md:27` — and every success (AP PHF, AU arena, AW-IV.W4.4 parallel fork, AW-V.W2.1 prototype) traces to the inverse: substrate shipped with its firing consumer in the same wave. AX's carry-forward constants are not the specific substrates; they are the orchestration contract.

## 2. Architectural genealogy — shifts that stuck

| Substrate | Origin | Survived to V.W6 master? | Why it stuck |
|---|---|---|---|
| **Fn-per-rule RD** | pre-W (AK-era `__branch_idx` / Vec tape) | Yes (as `shapes/` emitter at V.W3) | Demonstrated monomorphic; matches what LLVM will inline (V.W2.1 prototype proof, `AW-V-W2-close.md`) |
| **CSP solver** | W.Phase 3b (`csp_strategy.rs`, `W.md:219`) | Yes, as substrate — still feasibility-only per `SYNTHESIS.md:64` | Correct abstraction; optimisation consumer never closed |
| **E-graph substrate** | AL prototype; cross-rule normalise deleted `bfa50f25` (2026-04-08); CSS suffix-factor preserved | Yes (`crates/egraph/`, `crates/ir/src/egraph/`, `write_back_optimized` live per `egraph-substrate-audit.md:65`) | Bounded saturation + leaf-predicate guardrail survived the cross-rule cascade failure |
| **StructRegistry** | AS.2.3 | Deleted AU.4.2; re-emerged as backend-type-tables in AW-I.W0.5 (`SYNTHESIS.md` Chronic-deferrals row) | Named-type projection is mandatory; "registry" shape was wrong |
| **Tape SoA + PSI** | AV (V0-V5); PSI at V4 | Partially — SoA tape survives; PSI live only on walker path (`psi-and-dead-substrate.md:26`) | SoA projection is the invariant; PSI is the walker-specific form that dies with walker |
| **DTA interpreter** | AV substrate → AW-I/II/III activation → AW-V.W6 reckoning | No — queued for delete at AX.W0b per `dead-code-manifest.md` Tier-1 (~12K LOC) | Proven un-inlinable (non-uniform state bodies, `r3-codegen-walker-proof.md`) |
| **Shape classification** | AW-V.W3.1 (`7d1dc9fb`) | Yes — IR pass + 12-variant ShapeTag (`FINAL-V.md:49`) | Mechanically generalises fn-per-rule; JSON proof-point genuine |
| **Parity harnesses (sonic-rs, lightningcss)** | AW-IV.W5.2, CI-gated `95b819f0` | Yes — 5/5 + 4/4 preserved to V.W6 (`FINAL-V.md:97`) | Correctness invariant paid directly against competitors |
| **GRAMMAR_PROFILE wire-contract** | AW-IV.W1.δ + V.W5.1 (`53da1bb9..98edad19`) | Yes — populated for every slot with an IR miner (`FINAL-V.md:86`) | The one substrate that consistently crosses mine→emit→read correctly |
| **Bootstrap regen + idempotency** | AC-era debt; closed in AR audit (`0c6e011`); CI-gated AW.0.7 | Yes — byte-identical across consecutive cache clears | Eliminated Agent-B-style hand-patch regressions |

The architectural constant is not any one substrate; it is **"mined IR fact → pluggable cost model → emitter selects body shape → consumer reads the emitted const"** as a pipeline. When all four hops close in one wave, the substrate sticks (V.W5.1, AW-IV.W4.4, AP PHF). When any hop is deferred, the substrate rots.

## 3. Process discipline evolution

| Discipline | First codified | Forced by | Status at V.W6 |
|---|---|---|---|
| **Per-tranche directory structure** (`{LETTER}.md` + PROGRESS + FINAL) | `36945f60` (2026-04-15, post-AW) | AO/AP/AS ceremonial-plans anti-pattern | Enforced; pre-W tranches retro'd, not retroactively conformed |
| **Wave verification ledger** (samply + `nm` + wire-contract) | README §197-232 | AT Phase-1 grep-gate passing with dead stores | Enforced per TRANCHE_SPEC §hard-gates |
| **Substrate-with-consumer invariant** | README §79-92 | AK/AM/AO/AP.1/AQ.6/AS.2.3/AT.1/AV.0.5/AV.2.5/AW-III.W6/AW-IV.W3/AW-V.W4-W5 — ten-tranche chronic | Codified; still partially violated at V.W6 per `V-audit-overfit.md:70` |
| **Mid-wave bench checkpoints** | TRANCHE_SPEC §Bench-contract | AV V10-only bench hiding 2.5-4.5× regression; AW-V W4 widening hidden across 3 wave boundaries | Declared for AX; not yet enforced |
| **Wire-contract tests** (mining→emit→runtime) | README §527-538 | AT branch_pushes_children dead store; V.W4 detector widening | Exists as pattern; per-gate enforcement pending AX.W0a |
| **Scope-reveal triggers re-plan** | TRANCHE_SPEC §Scope-reveal-protocol | AU Session 2 as "bright spot"; AV V5 scope cut ("per user direction"); AW-I/III mid-execution split (`ff0b7fe7`) | Codified both absorb / new-letter modes |
| **Tranche FINAL.md + post-{L}.json required** | README §254-280 | AK/AL/AM/AO/AP/AS/AT missing close artefacts | Enforced; V close adds FINAL despite gate miss |
| **Gate-off commits forbidden** | TRANCHE_SPEC §195-203 | AP.1 `structural_mode = false` deleted at AQ.5 (~400 LOC) | Enforced; AW-V `has_w4_classified` surfaced as shim-masquerade |
| **Hard-gate floor-check at plan time** | TRANCHE_SPEC §166-181 | AW-I gate 9 ("≤ 12000 lines") unreachable from W3 alone | Codified |
| **Isolated worktrees + seed-worktree.sh** | README §132-150 | W2.1 agent reporting 24 "failures" that were missing data/ | `scripts/seed-worktree.sh` exists; enforced |
| **Self-host escape recipe** | README §411-451 | AW-I.W3 circular bootstrap (commits `87f65214`, `49656fd4`) | Documented as legitimate orchestrator move |

## 4. Success vs failure meta-patterns

**Closed-successfully waves share four traits** (AU Session 2 as template per `AU-retro.md:28`; AW-IV.W4.4 parallel fork; AW-V.W2.1 prototype; V.W5.1 wire-contract fix):

1. A single named substrate with a single named consumer in the same wave.
2. Bench/samply attribution cited with artefact path, not paraphrase.
3. Plan-time floor analysis on any numeric gate.
4. Re-plan-not-defer on scope reveal.

**Ledger-only closes share three traits** (AK EmissionTier leftovers; AM.2/AM.5; AO full chain; AP.1; AQ.6; AS.2.3; AT.1; AV V0-V5; AW-III.W6; AW-IV.W3; AW-V.W4-W5):

1. "Substrate exists, consumer in follow-on" framing at wave close.
2. Grep gate instead of runtime gate.
3. FINAL absorbs the gap as "compounding engagement" (V.W6 `FINAL-V.md:168` — explicitly called misleading in `V-audit-overfit.md:63`).

**Regression waves share one pattern**: predicate widening without wire-contract (AW-V.W4-fix-rest `569c17e4`/`ce2fd9f6` widened Flat/Wrap, broke JSON W3 bench hidden for 3 waves per `last10-slowdown-census.md:37`). AP.1b cursor-desync bugs were surface-discoverable via `cargo expand` but shipped because expand inspection was not a named pre-landing gate.

## 5. Orchestration + tooling maturity assessment

Orchestration capability grew along three axes:

- **Parallelism**: W-AS largely solo-sequential; AU introduced samply-trio (shared `CARGO_TARGET_DIR` + `wave.tsv` + `profile-bench-headless.sh`); AW-I onward enforces 6-agent waves with disjoint file bounds.
- **Worktree discipline**: Enforced from AV regressions (3 V0 API-termination losses); `scripts/seed-worktree.sh` closes the data/ gap.
- **Cherry-pick protocol**: AR exposed worktree racing (duplicate `b0e4534`/`6c889d5`); TRANCHE_SPEC §119-134 now requires master-clean-before-dispatch + named consolidator for N-agent shared-file waves.

**Tooling that landed and stuck** (`scripts/` inventory): bench corpora (`data/{json,css,sheets}`), `bootstrap-bbnf.sh` + `check-bootstrap-clean.sh`, `prepare-profile-wave.sh` + `profile-bench-headless.sh` + `verify-w2-asm.sh` + `verify-w2-symbols.sh`, `seed-worktree.sh`, `cost-grid-sweep.sh`, `bisect-fastpath.sh`, `extract_hotspots.py`. AU's samply trio is the template.

**Missing for AX**:
1. `gate_predicate_wire_contract.rs` harness — one-test-per-predicate-per-grammar.
2. Bench-delta-gate at wave close (numeric threshold against post-AW-V.json, reject on miss).

## 6. Governance rule provenance

Every rule in README §code-discipline was paid for:

- "NO workarounds" — AP structural_mode, AQ.1 span-text fallback.
- "NO legacy" — W.md §Phase 1/2 e-graph substrate leftovers; AK EmissionTier survival until AM.1 (-2306 LOC).
- "NO backward-compat" — AS.2.3 StructRegistry dead field surviving two tranches.
- "NO deferrals" — AN five silent items; AP ~10 sub-phases; AQ.1/7/9.4/9.5; AR.6→AS.5 defer-then-retire.
- "Execute the plan" — AO plan's "six phases" collapsed to Phase 0 only.
- "Substrate-with-consumer" — the chronic #1 pattern enumerated eleven-fold.
- "NO god modules" — emerged from analysis/helpers accumulation pre-AR.
- "Generated files are output of fresh regen" — Agent-B hand-patch at AV V0.
- "Tests live in tests/" — codified during AU refactor.
- "One codegen path" — AW-V.W2.1 prototype + shape emitter co-residence explicitly called fork surface.
- "Commit frequently" — AV's 3 API-termination losses.

**Missing rule AX should add**: *"Gate predicates freeze after their introducing wave. Downstream widening requires explicit re-plan with bench recheck."* Provenance: AW-V.W4 widened `has_w4_classified` silently; regression hidden for 3 waves.

## 7. Carry-forward constants

The substrate the arc keeps rebuilding because the next tranche keeps needing it:

1. **GRAMMAR_PROFILE as the mine→emit→read wire contract** — every tranche since AV has added slots; the contract-test discipline (V.W5.1) is the single invariant that distinguishes living slots from dead ones.
2. **fn-per-rule over IR-selected body shapes** — AK's `__branch_idx`, AU's inlined-Ref, AV's SoA-writer, AW-V's `shapes/` are the same answer.
3. **E-graph with bounded saturation + leaf-predicate guardrail** — substrate alive from AL to V; `write_back_optimized` is the live consumer.
4. **Samply-attribution trio** — shared target + wave.tsv + headless script is AX's template.
5. **Parity harnesses** — sonic-rs + lightningcss CI-gated from AW-IV.W5.2 through V.W6, zero divergence.
6. **Bootstrap self-host escape recipe** — README §411-451 with commit templates `87f65214` + `49656fd4`.

## 8. Formally-retired narratives

These framings have been disproved and should be laid to rest:

- **"DTA interpreter with cold-path hedge"** — the hedge kept interpreter paths reachable for 5 tranches; Path B per `SYNTHESIS-5-AW-V-RECKONING.md` was right.
- **"Architectural transposition complete; throughput in next wave"** — invoked in AW-III/IV/V FINALs; the invariant already forbids it; elevate to AX refusal condition.
- **"Universal stage-1 SIMD structural-bitmap pre-pass"** — tried six times, regressed six times. PER-RULE bounded-regex form (per `structural-scan-working-approach.md`) is alive; universal form is dead.
- **"Compile DTA into hot-path code"** — the prototype is RD; call it fn-per-rule.
- **"Lever 4 `push_compound_fused_v32`"** — self-aliased LLVM tautology.
- **"17-digit NEON lever"** — never shipped, projection artefact; strike from ledger.
- **"Substrate-only wave close"** — the README explicitly rejects it (§79-92) but `FINAL-V.md:168` shipped one; AX must refuse at wave-close mechanically.

## 9. Missing rules / tools AX should add

1. **Gate-predicate freeze + wire-contract** (§6 missing rule) — one test per predicate per grammar asserting gate output before/after every downstream wave.
2. **Mid-wave bench checkpoint** — elevated to invariant with re-plan trigger on regression (>5% vs previous wave).
3. **Predicate-widening requires re-bench** — any detector/admission widening runs the full bench matrix at commit time.
4. **Small-input amortisation declared at plan time** — Sheets sub-KB inputs cannot break even on SIMD+walker-specialised paths; plan must declare per-entry break-even bytes.
5. **Consolidator-named at plan time** for N-agent shared-file waves — TRANCHE_SPEC §128-134 as invariant, not commentary.
6. **Ledger-only wave = re-plan trigger** — if close ledger cannot cite runtime evidence for every substrate, wave reopens.

The arc's meta-lesson: orchestration discipline compounds. AU's bright spot and V.W5.1's wire-contract fix were possible because ~6 tranches of retros had already paid the cost of each anti-pattern. AX should inherit the contract — not re-derive it.
