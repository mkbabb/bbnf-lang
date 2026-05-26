# HARDENING-SKINNY — Per-Target Audit Specification

You are the hardening agent for the **skinny implementation spec**. The skinny exists at `restart/skinny/` as a five-quadrant prior-validation device: one grammar (JSON) end-to-end through ~10 partial crates, dual-track measured against sonic-rs / simd-json, before tranches A-J commit (`restart/skinny/INDEX.md`).

This prompt is parameterised by target. It composes with the existing greenfield-restart hardening apparatus rather than duplicating it.

**Pass Omega V2 / SK-V14 receiver (2026-05-24; post-CRUD-3 LOCKS v+1 at
`85a043224`, 779 lines).** The active skinny cycle is SK-V14, with full
cohort §3Z LOCK convergence achieved across all 5 cohorts: S-P2 `4c70b6f193`
+ T-P1 `0a9c0fe65d` + S-P3 `626cb06cc1` + T-P2 `34a28f5c15` + T-P3
`69eea1c5c`. The T-P3 7-lens CHALLENGE waves (CH1 CORRECTNESS, CH2
GENERALITY, CH3 REGRESSION, CH4 COST, CH5 HIDDEN COUPLING, CH6
ANTI-PAPER-CLOSE, CH7 OVERFIT-PRUNE) converged at V4 with the §3Z cohort
LOCK criterion satisfied (≥95% × 2 consecutive cycles NO CAVEAT; zero
orphan REVISEs; V≤5 ceiling consumed at V4 = 4/5 with 1-cycle margin).

The §3Z convergence model: per-lens LOCK depth (4-cycle extensions for
CH1/CH3/CH5; 3-cycle LOCK trigger for CH2; 3-cycle extension for CH6;
4-cycle extension for CH4; 2-cycle LOCK trigger for CH7) feeds the cohort
LOCK declaration when 7/7 lenses ACCEPT at 100% NO CAVEAT for two
consecutive cycles. CH7 specifically scans for fabricated baselines,
cited-but-absent surface text, counter-surface fabrication
(meta-CH7 collision pattern), and the SK-V14 cohort 32:69 = 31.7%
refutation density preservation. The Lock 1 v+1 substrate-ceiling fold +
LAC-2F-V5-02 substrate-union ELEVATION + LAC-1E-14 FactStream 5th
SUBSTRATE-manifest category are the cohort-discipline invariants that all
T-P3 lens artefacts now cite verbatim.

A hardening run now reads the SK-V14 `SPEC.md`, the cohort hardening
consolidators (`HARDENING-S-P2-V2-CONSOLIDATED.md`,
`HARDENING-T-P1-V5-CONSOLIDATED.md`, `HARDENING-S-P3-V3-CONSOLIDATED.md`,
`HARDENING-T-P2-V3-CONSOLIDATED.md`,
`HARDENING-T-P3-V4-CONSOLIDATED.md`), the G3 packet (3A through 3F per
PASS-3-SYNTHESIS.md), and the G-Omega sign-off before it treats any
skinny surface as dispatch authority. Refuse W0-before-G-Omega, missing
parse_only admission coverage, weak strict-vs-strict comparator anchors,
support-only primitives, P1-P8 silent fallback, source-present SIMD/ASM
orphans, Lock 14 generic crate branches, missing audit-overlay column
(LAC-1E-16), and `RESULTS.md` rows lacking executable verification
(LAC-1E-12 mandate). NEW-CH2-V3-02 orphan-cell propagation guard requires
pre/post-grep evidence on every cite-bearing micro-fold. This prompt
also refuses any W3-or-later SK-V14 plan before Pass Omega V3 W2R CRUD has
landed and amended W2 has admitted under the skinny-only `regen-css` gate.
Reject any W2 plan that touches or claims closure over
`crates/core/src/runtime/css_l4/`; W6.0 owns that tree. Pass Omega V4 W4R
adds the next refusal: reject any W4/W5 plan before V4 G-Omega + SPEC/HANDOFF/
INDEX/WORKSPACE patching lands; reject any W4 plan that deletes CSS provider,
template, runtime, source, or generator paths; reject any W5 plan that deletes
CSS provider/template paths without landing the generic provider replacement
and `regen_css.rs` migration in the same source slice. This prompt still
defines the lenses; it does not authorize source, gate, `RESULTS.md`, or
`REDRESS.md` edits by itself.

## §1 — Purpose: why skinny hardening differs from V1 hardening

The V1 corpus hardening (`restart/prompts/audit-specs/HARDENING-LENS-SET.md`) audits whether the V1 architecture is internally coherent and load-bearing. The skinny hardening audits something different: **whether the skinny faithfully tests the V1 premise, while remaining buildable in 2-4 weeks and graduating mechanically**.

Three axes the V1 hardening does not cover:

1. **Premise fidelity** — every skinny scope cut (DK13, GADT, CSP, e-graph, recovery, multi-grammar) must be genuinely orthogonal to the SOTA-throughput axis the bench measures. A cut that masks a V1 cost the bench cannot recover is a false-positive risk: skinny lands SOTA → V1 dispatches → V1 misses SOTA because of the masked cost.
2. **Falsifiability** — the go/no-go threshold matrix must include honest NO-GO branches. A bench framework that cannot return NO-GO is a confirmation-bias engine, not a prior-validation device.
3. **Graduation mechanicality** — each documented skinny-vs-V1 deviation must have a mechanical V1 closure (additive code, not re-architecture). A deviation that requires rewriting the substrate at graduation invalidates the skinny as a prior.

These three axes become **Lenses L, M, N** below and are mandatory at every skinny hardening cycle.

The full lens stack (A-N) is the V1 set A-K (`restart/prompts/audit-specs/HARDENING-LENS-SET.md` §Lanes + §Lens F + §Lens G + §Lens H + §Lens I + §Lens J + §Lens K) plus the three skinny-specific lenses.

## §2 — Target selection

The user invokes you with one of these targets:

| Target | Path | When applied |
|---|---|---|
| **SUBSTRATE** | `restart/skinny/SUBSTRATE.md` | After substrate spec lands (mandatory for any cycle that audits substrate edits) |
| **COMPILER** | `restart/skinny/COMPILER.md` | After compiler spec lands |
| **BENCH** | `restart/skinny/BENCH.md` | After bench / parity-harness spec lands |
| **WORKSPACE** | `restart/skinny/WORKSPACE.md` | After workspace + LOC budget spec lands |
| **INDEX** | `restart/skinny/INDEX.md` | When cross-quadrant invariants change |
| **SKINNY-SUITE** | All five together | The pre-implementation gate (mandatory at SK-V1 dispatch) |

The mandatory invocation is `target=SKINNY-SUITE` at the SK-V1 cycle. Per-quadrant invocations are optional and used when one quadrant has had targeted edits since the last SUITE pass.

## §3 — Required reading (mandatory; in order)

Per-quadrant authority sources are colocated; orchestration sources are inherited from the main restart corpus.

1. `restart/skinny/INDEX.md` — cross-quadrant invariants + decision protocol + flagged contradictions.
2. The target file(s) per §2.
3. `restart/prompts/audit-specs/HARDENING-LENS-SET.md` — the V1 lens contract (Lenses A-K). **Read in full**; this skinny prompt does not duplicate the lens definitions.
4. `restart/prompts/ORCHESTRATOR.md` — dispatch protocol; phase-type table; cycle naming canon.
5. `restart/ARCHITECTURE.md` — V1 architecture authority (the skinny is a subset of this; deviations must be documented).
6. `restart/MASTER-PLAN.md` §4 (Hard Architectural Gates, lines 108-169) — V1 SOTA gate definitions.
7. `restart/locks/LOCKS.md` — settled commitments; particularly Lock 1 (tape substrate), Lock 5 (Backend trait), Lock 8 (SOTA anchors), Lock 13 (directory discipline), Lock 14 (Lock 14 onboarding).
8. `restart/corpora/SOTA.md` lines 50-89 + 130-136 — competitor baselines.
9. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` — voice + discipline.

The agent does NOT read pass-1/pass-2/pass-3 V1 audits unless a contradiction in the skinny target cites them. The skinny is graphically separate; over-reading the V1 corpus dilutes the skinny-specific signal.

## §4 — Lens registry

The skinny audit applies **Lenses A-K from `restart/prompts/audit-specs/HARDENING-LENS-SET.md` plus Lenses L-N below**.

| Lens family | Source | Adapted for skinny? |
|---|---|---|
| Lanes 1-9 (lock adherence, sequencing, cohesion, SOTA, grammar-authoritative, generated-LOC, friction, carry, greenfield) | V1 `HARDENING.md` §Lanes | Lane 2 (sequencing) is N/A — skinny is single-wave. Lane 4 (SOTA) sharpens — it is THE load-bearing lens here. Lane 6 (LOC budget) sharpens — the WORKSPACE.md handwritten LOC ceiling is binding (32,000 after SK-V3 BENCH-side redress). Lane 8 (carry) sharpens — every skinny omission must name V1 receiver. |
| F (LLM bias) | V1 `HARDENING.md` §Lens F | Identical. |
| G (overfitting) | V1 `HARDENING.md` §Lens G | Sharpens — does the skinny over-fit to JSON in ways CSS/Sheets won't tolerate? |
| H (hallucination + provenance) | V1 `HARDENING.md` §Lens H | Identical. |
| I (contrivance) | V1 `HARDENING.md` §Lens I | Inverted — for V1, contrivance is "too much apparatus"; for skinny, contrivance is "too little cut, more than needed for the test". A skinny with cargo-culted V1 apparatus that doesn't move the SOTA needle is over-engineered for its purpose. |
| J (host-language leverage) | V1 `HARDENING.md` §Lens J | Identical. |
| K (meta-grammar discipline) | V1 `HARDENING.md` §Lens K | Identical. |
| **L (premise fidelity)** | **§5 below** | **Skinny-specific.** |
| **M (falsifiability)** | **§5 below** | **Skinny-specific.** |
| **N (graduation mechanicality)** | **§5 below** | **Skinny-specific.** |

The verdict classes are inherited from V1 `HARDENING.md` (KEEP / REINVENT / DISCARD plus the V8+ classes SIMPLIFY / CONSOLIDATE / LEVERAGE / HYBRID / LOAD-BEARING / ASPIRATIONAL / SPECULATIVE) plus three skinny-specific verdicts:

- **FAITHFUL** (Lens L) — the cut is genuinely orthogonal to the SOTA axis; the bench result will predict V1 outcome on this axis.
- **MASKING** (Lens L) — the cut hides a V1 cost the bench cannot recover; the skinny would over-predict V1 success.
- **MECHANICAL** (Lens N) — the deviation closes by additive code; graduation is verified mechanical.

## §5 — Skinny-specific lenses

### Lens L — Premise fidelity

The skinny's promise is: "if JSON SOTA falls out of the substrate + extraction shape, the V1 premise is validated for JSON-class grammars" (`restart/skinny/INDEX.md`). Lens L tests whether each scope cut honors that promise.

For each documented skinny omission (`SUBSTRATE.md` §7; `COMPILER.md` §2.2 + §3.2 + §4.2 + §7; `WORKSPACE.md` §10), classify:

- **Genuinely orthogonal** — the omitted mechanism does not interact with parse throughput. Example: `path-core` is post-parse cursor traversal; cutting it cannot change throughput. Verdict: **FAITHFUL**.
- **Conditionally orthogonal** — orthogonal *for JSON*, possibly load-bearing for other V1 grammars. Example: GADT branch-local equality is orthogonal for monomorphic JSON; for CSS L4 with generic colour-function chains, it may bind. Verdict: **FAITHFUL with V1-grammar caveat** — must name the grammar(s) where the cut becomes load-bearing.
- **Throughput-coupled** — the omitted mechanism interacts with parse throughput in a way the skinny bench cannot recover. Example: if `cost-driven-rewrites` would have selected a different plan that the SIMD+Pratt bench measures more favorably, then cutting cost-driven-rewrites masks a V1 perf cost. Verdict: **MASKING** — must propose a substitute measurement (e.g., bench the hand-curated plan against an alternative-plan stub to bound the rewrite-budget tail).

For each instance: cite path:line + the omission + the orthogonality classification + the bench-recoverable signal (or absence). Honest MASKING verdicts are not faults — they update the V1 SOTA-beat probability *correctly*. Faults are MASKING verdicts that the spec calls FAITHFUL.

Particular foci:

- The HM-only type system (`COMPILER.md` §4.2). DK13 and GADT cuts are FAITHFUL for JSON; verify the spec names them as JSON-FAITHFUL not skinny-FAITHFUL.
- The single-plan extraction (`COMPILER.md` §5.3). The cost-driven-rewrites cut is the most adversarial — the spec claims FAITHFUL by ARCH §10.1 ASPIRATIONAL classification; verify the claim survives steelman.
- The host-fn-free JSON grammar (`COMPILER.md` §1.3). This is a grammar-source deviation; verify the throughput cost of `CallHost` registry dispatch vs direct call is bench-recoverable (e.g., a microbench of a one-host-fn grammar variant).
- The empty payload arena (`SUBSTRATE.md` §2). The "zero arena allocations on the JSON hot path" claim must hold under bench measurement, not assertion.

### Lens M — Falsifiability

The skinny is a falsifiable prior-validation device or it is theatre. Lens M audits the bench harness for honest NO-GO branches.

For each row in `BENCH.md` §6 (the threshold matrix), test:

- **Threshold defensibility** — the threshold cites a competitor baseline + a specific multiplier (e.g., `Track 2 ≤ sonic-rs * 0.95`). The multiplier must be defensible — neither set so loose that the test trivially passes, nor so tight that a viable substrate fails noise. Verify the multipliers against the SOTA-parity-vs-beat calibration in `MASTER-PLAN.md:140-154`.
- **NO-GO branch presence** — the matrix must contain at least one NO-GO outcome that the skinny could plausibly land in. A matrix where every outcome routes to "GO with some focus" is confirmation-biased.
- **Track 1 vs Track 2 separation** — the dual-track design must be empirically separable. If the bench harness cannot distinguish "substrate ceiling viable, codegen overhead at 1.20×" from "substrate gap, codegen artificially fast", the diagnosis is broken.
- **Reproducibility schema enforcement** — `BENCH.md` requires every bench row to carry the 8-field reproducibility schema (`MASTER-PLAN.md:160-168`). Verify the harness fails the gate on missing fields, not just warns.

For each instance: cite path:line + the threshold or claim + the falsifiability test (does it actually fail under plausible adversarial input?) + the surgery (tighten threshold, add NO-GO branch, surface schema fail).

Lens M outputs no-go branch evidence. A skinny that cannot return NO-GO is the most dangerous outcome possible: it commits tranches A-J on a false positive. Lens M is the load-bearing audit lens for skinny dispatch.

### Lens N — Graduation mechanicality

`INDEX.md` §"Open contradictions" lists the deliberate skinny-vs-V1 deviations. `WORKSPACE.md` §8 sketches the migration parity matrix. Lens N audits whether each deviation closes by additive code (mechanical) vs requires re-architecture (anti-mechanical).

For each documented deviation:

- **Additive closure** — the V1 graduation adds code; the skinny code does not move. Example: adding `@layout` support adds the layout-policy lowering pass alongside the HM pass; the HM-as-top-level skinny code becomes a `passes::layout` subroutine via wrapper, not rewrite. Verdict: **MECHANICAL**.
- **Subroutine-inversion closure** — the V1 graduation inverts a hierarchy that the skinny pinned the wrong way. Example: `COMPILER.md` §9.1 inverts ARCH §8.2 + Lock 2's HM-as-layout-subroutine into HM-as-top-level. At graduation, the inversion reverses. Verify the inversion is reversible without rewriting the HM checker — i.e., the wrapper move is small and named. Verdict: **MECHANICAL with named inversion**.
- **Architectural rewrite** — the V1 graduation requires the skinny code to be torn down and rebuilt. Example: if the skinny `SUBSTRATE.md` §1.1 16-byte token were not extensible to V1's larger kind table without changing every consumer, that would be anti-mechanical. Verdict: **ANTI-MECHANICAL** — must propose a skinny-side change that pre-empts the rewrite.

For each instance: cite path:line + the deviation + the V1 closure path (additive / inversion / rewrite) + the mechanical-closure cost in LOC. A graduation that costs more LOC than the skinny itself spent is anti-mechanical by definition.

The five deviations the skinny currently lists (per `INDEX.md` §"Open contradictions"):

1. HM hierarchy inversion (COMPILER.md §9.1) — Lens N must verify reversibility.
2. JSON host-fn-free (COMPILER.md §1.3) — Lens N must verify `@host fn` decode-string add is additive.
3. `parse-that-regex` directory layout (WORKSPACE.md §4.7) — Lens N notes this is a one-time directory promotion that V1 inherits unchanged. Verdict trivially MECHANICAL.
4. `passes` HM-only constraint (WORKSPACE.md §2.1) — Lens N must verify DK13/GADT/CSP additions extend `passes::layout/` without touching `passes::types/algorithm_w.rs`.
5. `wasm = false` metadata flag (WORKSPACE.md §11) — Lens N notes this is a V2 flag flip.

Additional deviations Lens N may surface beyond the five enumerated.

## §6 — Cycle naming

The skinny hardening cycles are named **SK-V1**, **SK-V2**, etc. — namespaced to prevent collision with the V1 corpus's V1-V9.1 cycle stream.

| Cycle | Predecessor | Trigger |
|---|---|---|
| SK-V1 | (initial) | First-pass after `restart/skinny/` lands the five quadrants |
| SK-V2 | SK-V1 | If SK-V1 returns AMENDMENT-REQUIRED-NARROW; verify-then-rerun |
| SK-V3+ | SK-V2 | Subsequent measurement-driven amendments (e.g., bench-result feedback into SUBSTRATE token layout) |

Cycle outputs land at `restart/skinny/tranches/HARDENING-CONSOLIDATED-SK-V{N}.md` (the consolidation) plus per-target reports `HARDENING-{TARGET}-SK-V{N}.md`. The audit subdirectory is created at SK-V1 dispatch.

## §7 — Per-item discipline

Inherited verbatim from V1 `restart/prompts/audit-specs/HARDENING-LENS-SET.md` §"Per-Item Discipline". Every claim, gate, decision, surgery, verdict, and proposal in the target carries the four-part shape:

- **Explication** — what the item means
- **Pros** — why it earns its place
- **Cons** — what it strains
- **Challenge** — the steelman counter

Verdict classes (full set for SK-V1+):

- KEEP / REINVENT / DISCARD (V1 base)
- SIMPLIFY / CONSOLIDATE / LEVERAGE / HYBRID / LOAD-BEARING / ASPIRATIONAL / SPECULATIVE (V1 V8+)
- **FAITHFUL / MASKING** (Lens L; skinny-specific)
- **MECHANICAL / ANTI-MECHANICAL** (Lens N; skinny-specific)

A target where every item lands KEEP / FAITHFUL / MECHANICAL without challenge is fault — the audit failed to challenge.

## §8 — Output contract

Per-target file: `restart/skinny/tranches/HARDENING-{TARGET}-SK-V{N}.md`, ~600-1,200 lines (SKINNY-SUITE consolidated may extend to ~1,500-2,000), structured per V1 `HARDENING.md` §"Output Contract" §1-§13. Adaptations:

- §2 cohort verdict adds three rows (Lens L, M, N) to the lane-verdict table.
- §3-§11 inherits V1 lane sections; lane 2 (sequencing) reports N/A for skinny single-wave.
- §12 punch list per V1 contract.
- §13 readiness verdict adopts the V8+ classes plus skinny-specific FAITHFUL / MASKING / MECHANICAL signals.

Final readiness verdict for skinny:

> **Decision: {SK-READY / SK-AMENDMENT-REQUIRED-NARROW / SK-AMENDMENT-REQUIRED-BROAD / SK-RE-DRAFT}**
>
> {summary in 3-5 sentences identifying the dominant Lens result and the implication for skinny dispatch}
>
> Hereupon: {dispatch skinny implementation per `INDEX.md` §"Decision protocol" / amendment-dispatch agent / re-author the failing quadrant}.

## §9 — Hard cap

| Target | Wall budget |
|---|---|
| Per-quadrant (SUBSTRATE / COMPILER / BENCH / WORKSPACE / INDEX) | 45 minutes |
| SKINNY-SUITE consolidation | 75 minutes |
| Cohort dispatch (5 parallel + consolidation) | ~90-120 minutes total |

At 0.9 × cap, commit the report as-written. At 1.0 × cap, halt. Empty return is not scope-reveal — return whatever exists and flag the contradiction that blocked completion.

## §10 — Cross-tranche scope boundary

Touch ONLY `restart/skinny/tranches/HARDENING-{TARGET}-SK-V{N}.md`. Do NOT modify the skinny target files. Do NOT modify `restart/audit/`, `restart/prompts/`, `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, or any V1-corpus surface. Do NOT execute git operations beyond the single commit at completion.

The skinny audit lives in its own subdirectory (`restart/skinny/tranches/`) parallel to the V1 audit dir (`restart/audit/`). The two streams do not share state.

## §11 — Closing posture

The skinny hardening prompt composes with the V1 hardening prompt by reference, not by duplication. Lenses A-K live at `restart/prompts/audit-specs/HARDENING-LENS-SET.md`; Lenses L-N live here. Cycle dispatch lives at `restart/prompts/sub-orchestrators/HARDENING.md`; skinny target table + cycle namespace live here. The skinny is a graphically separate corpus with its own dispatch graph; this prompt is the single skinny-specific contract the hardening agents read.

After SK-V1 returns SK-READY (or SK-V2+ does, post-amendment), the skinny dispatches per `INDEX.md` §"Decision protocol" — implementation begins; the bench harness adjudicates the V1 SOTA-viability premise; the V1 tranches A-J wait on the bench verdict.

The skinny is buildable, measurable, and falsifiable. This prompt verifies that all three claims survive an independent audit before any LOC is written.
