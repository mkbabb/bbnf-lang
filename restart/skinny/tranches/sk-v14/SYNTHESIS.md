# SK-V14 Grand Synthesis

Date: 2026-05-22.

Status: Pass Alpha α-F contract draft for SK-V14. This file is the master
synthesis and close contract. It does not author `SPEC.md` or
`DISPATCH-PROMPT.md`; skinny pass S-P3 derives those downstream from this
goalset after G-Omega and S-P0 convergence.

## Authority

Read in this order; later entries override where they conflict:

- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v{1..6}-*.md`
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/locks/LOCKS.md` (V1.1; Lock 14 + CH7 Overfit-Prune lens binding)
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md` (requires honest re-baseline)

The 2026-05-21 user pin addendum together with the SK-V14 fresh-session
orchestrator prompt control conflicts. SK-V14 is a **prune-then-rebuild
tranche**. No new admit attempt is authorised until the PRUNE waves
converge and the honest baseline is restated in `RESULTS.md` +
`ROLLING-SOTA-DELTA.md`.

## Section 0 — Close Condition And Goalset

### 0.1 Close condition (R10 verbatim)

SK-V14 closes only when **every JSON cell (51 = 17 corpora × 3 planes) and
every CSS L4 feature (24 non-OUT_OF_SCOPE) ADMITs > strict-vs-strict on the
same plane, same corpus, same equality semantics — OR carries an
architectural-level intrinsic-block proof per row family**. Implementation-
limited misses are reopens, not closes. If any goal below remains unmet
without architectural-block proof at tranche close, Pass Alpha brackets
SK-V15 immediately under the same pinned bar (per addendum A4 +
`[execute-planned-architecture]`).

The campaign is indefatigable. Successive tranches roll automatically
until full ADMIT or per-row / per-feature intrinsic-block proofs cover
everything.

### 0.2 Goalset row enumeration (current state: AUDIT-ZERO)

| Surface | Population | SK-V13 nominal | Audit-corrected | SK-V14 obligation |
|---|---|---|---|---|
| JSON `parse_only` | 17 | 5 ADMITTED (W14.1–.5) | 0 ADMITTED | all 17 reopen; distinct parse_only path + Skipper-class comparator |
| JSON `direct_to_struct` | 17 | 4 ADMITTED carry-over | 0 ADMITTED | all 17 reopen; sonic-rs strict per-corpus struct deser comparator |
| JSON `real_typed_struct` | 17 | 7 ADMITTED carry-over | 0 ADMITTED | all 17 reopen; per-corpus typed struct deser comparator |
| CSS L4 features | 24 | 24 ADMITTED (incl. SK-V12 W1b) | 0 ADMITTED | all 24 reopen; grammar-derived parsers + production corpora + work-equivalent lightningcss/cssparser |

**Per cell** the bar is `Track 1 > comparator strict + 1` and feature-
coverage match (every variant the comparator accepts, the row accepts;
every variant it rejects, the row rejects) and same-plane / same-corpus /
same-equality semantics. Anything less is REOPEN.

This honest baseline restates the §1 bind of `DISPATCH-CONTEXT.md` and
the §4 baseline of `audit-overfit/validation/v5-cross-tranche-stability.md`.
The 25 CSS rows + 5 parse_only admits + 4 direct admits + 7 typed admits
recorded in `restart/skinny/ROLLING-SOTA-DELTA.md` at commit
`653cdf795+w15.1-redress` are FALSIFIED by the audit pack. PRUNE-1 + PRUNE-2
revert them.

**Numeric-divergence reconciliation (per CH6 §2.2 REJ-2).** The dispatch
context §1 cites 4 direct + 7 typed admits; α-A and α-D peer-measure
6 direct + 11 typed under the broader `ROLLING-SOTA-DELTA.md:13-93`
ledger (α-A:117-122 + :161-169; α-D:281-291 + :353-368). The +2 direct
extension rows are **marine_ik** and **instruments**; the +4 typed
extension rows are **random** (W13.3), **instruments** (W13.4),
**numbers** (W13.1), **unicode_basic** (W13.2), with **update_center**
adjusted under W15.1. Both populations reclassify AUDIT-FALSIFIED under
v6 §1 rows 3-4 (same comparator-misbinding pattern:
`sonic_rs::from_slice::<Value>` eager DOM, not strict per-corpus
struct deser). The PRUNE-1 ledger revert binds the wider 6+11
population — not the narrower 4+7 the dispatch summarises.

### 0.3 R-target goalset (the load-bearing layer)

Pass Alpha binds the SK-V14 R-targets verbatim from
`restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`:

| R | Scope | Acceptance |
|---|---|---|
| **R1** | comparator rebind: 3 plane-correct strict comparators | parse_only → sonic-rs Skipper-class (structural skip); direct → sonic-rs strict struct deser per corpus; typed → per-corpus typed struct deser. No row admits until its plane's comparator is strict-vs-strict. |
| **R2** | per-iteration equality oracle | equality on EACH bench iter inside the timing region, not startup-only. The harness emits an equality-pass column per iter; `xtask gate-json` rejects rows whose equality column is empty. |
| **R3** | PRUNE waves before any new admit attempt | PRUNE-1 revert W14.1–.5; PRUNE-2 reverts the 24 CSS L4 admit claims in rolling delta and REDRESS; PRUNE-3 splits into W5A request-boundary capability, W5B-FRONTEND frontend/import/IR closure, W5C-GEN provider-free generator body, and W5D-DELETE provider/template deletion. W5A proves source-consuming runtime generation at the request boundary; W5B-FRONTEND lowers compatibility constructs such as `@ws` into canonical IR; W5C-GEN replaces live provider-backed runtime emission; W5D-DELETE deletes provider/template clusters only after the replacement generator is load-bearing. Static centralization of hand-written CSS runtime bodies is rejected as P-6 recurrence. PRUNE-4 67 hand-written per-grammar runtime files in `crates/core/src/runtime/{grammar}/` refactored to emitted output (9 sub-waves, W6.0..W6.8); PRUNE-5 wire W8 + W9 from SCAFFOLD to LOAD-BEARING. |
| **R4** | `cargo xtask regen-css` pipeline (first instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator) | consumes 15 `.bbnf` files at `/grammar/css/l4/`; emits skinny CSS L4 runtime modules; seven exact `check-css-l4-*` companions pass; skinny-side round-trip clean (`delete generated → run xtask regen-css → diff empty`). Root `crates/core/src/runtime/css_l4/` is W6.0 work. |
| **R5** | production corpora `skinny/corpora/css-l4-sk-v14/` | Bootstrap + Tailwind + Material + Animate, ~960 KB. Tiny embedded fixtures unacceptable for admit. |
| **R6** | CSS L4 re-admit (honest) | after R3+R4+R5, each CSS L4 row attempted via grammar-derived pipeline, real corpora, work-equivalent comparator (lightningcss full-parse; cssparser full-parse; no fact-stream vs full-AST asymmetry). |
| **R7** | JSON direct + typed re-admit | after R1+R2, every JSON direct + typed row re-baselined against rebound strict comparators. Cells previously HOLDING under the misbound comparator hold again under the right comparator, or are reverted. |
| **R8** | JSON `parse_only` distinct path | stand up a distinct parse_only code path in `generated_json` (no full-tape build). Wire to Skipper-class comparator. Then attempt admit. |
| **R9** | carried pillars unchanged | W5A / W5B-FRONTEND / W5C-GEN / W5D-DELETE / W6 / W7 / bbnf-simd / OffsetFlags / Tape stand; R3 must not regress them. |
| **R10** | indefatigable close | as §0.1 above. |

### 0.4 Pre-blocked routes (pattern-level; SK-V14 P-list)

Pattern-level pre-blocks the audit pack discloses. Every wave consumes
this list; any SPEC clause that re-opens these patterns is REVISE:

- **P-1 — Fake `@generated` header on hand-written templates.** Per
  `audit-overfit/validation/v1-css-l4-validation.md §1` Claim 1: all 7 CSS
  L4 providers were `include_str!()` of hand-written templates carrying a
  fabricated `@generated` marker. The recurrence vector for the entire CSS
  L4 fake-admit cluster. SK-V14 generated files post-PRUNE must round-trip
  through `cargo xtask regen-css` (R4) — hand-patching is forbidden per
  `[clean-regen-discipline]`. Per α-C §4, W10.3 nested_layout (124×
  anomaly) carries a preemptive round-trip-rule trigger: any
  second-in-tranche reopen of nested_layout requires user re-pin with
  intrinsic-block evidence; any future CSS feature whose claimed Mbps
  exceeds the same-plane SOTA comparator by ≥ 50× inherits the same
  trigger.
- **P-2 — `sonic_rs::from_slice::<Value>` mislabelled as strict
  comparator.** Per `v6-comparator-integrity.md §1 + §3`: a single
  eager-DOM API was bound for all three planes. SK-V14 binds three
  plane-correct comparators (R1).
- **P-3 — Tiny-fixture Criterion-overhead Mbps inflation.** Per
  `v1-css-l4-validation.md §1` Claim 4 + §3 Claim 5: 85–357-byte fixtures
  embedded in bench source produce ~54 ns/parse measurements dominated by
  Criterion harness overhead. SK-V14 corpora pin (R5) is ≥800 KB working
  set; rows measured on <1 KB fixtures cannot admit.
- **P-4 — Gate-relabel as admit.** Per `v2-json-validation.md §1`:
  W14.1–.5 source diffs touched only `gate.rs` / `report.rs` /
  `lock14_baseline.rs`; the parser was unchanged. SK-V14 admit requires a
  parser/codegen source delta cited per row + measurement evidence per
  REDRESS.
- **P-5 — Scaffold-research counted as load-bearing.** Per
  `v4-decision-engine-trace.md §4 + §5`: W8 + W9 documented facts without
  any runtime consumer. SK-V14 PRUNE-5 wires both end-to-end; no row admit
  may cite W8 / W9 as evidence until the runtime consumer is measured.
- **P-6 — Per-grammar provider modules in generic codegen.** Per
  `v3-lock14-deep-scan.md §1` (3 CRITICAL + 4 HIGH): 8 hand-written
  per-grammar provider modules under `skinny/crates/codegen/` are the
  Lock-14 recurrence vector. SK-V14 PRUNE-3 splits into W5A source-consuming
  request boundary, W5B-FRONTEND frontend/import/IR closure, W5C-GEN
  provider-free generator body, and W5D-DELETE provider/template deletion.
  W5C-GEN proves ONE grammar-agnostic generator body consuming grammar source +
  workspace metadata through W5A's request and W5B-FRONTEND IR; W5D-DELETE
  deletes provider/template clusters only after the replacement path is
  load-bearing. Static centralization of hand-written CSS runtime bodies is
  P-6 recurrence, not redress.
- **P-7 — Track 1 ≡ Track 2 dishonesty.** Per cross-reference to prior
  Lock 1 violations + `PASS-ALPHA.md §3W` CH5 lens. SK-V14 bench harness
  must keep Track 1 (generated) structurally distinct from Track 2
  (independent oracle); any plane collapse fails gate.

Per memory `[abrogate-before-patch]`: any row family whose REDRESS
history shows two-or-more reopen attempts against the same fake-pattern
DELETEs rather than patches.

### 0.5 Wave-by-wave gates (deferred)

The §4.4 wave-by-wave falsifiability gate layer per
`PASS-ALPHA.md §4.4` is authored downstream by skinny pass S-P3 in
`sk-v14/SPEC.md`, consuming the goalset above. Pass Alpha sets §0.1
through §0.4 + §2 telemetry binding; S-P3 owns owner-paths, entry/exit
gates, hard caps, revert protocol, same-wave consumers, and per-wave
pre-blocked routes. The PRUNE waves (R3 PRUNE-1 … PRUNE-5 + R4 + R5)
run FIRST; new-admit waves (R6 + R7 + R8) only after PRUNE converges and
the honest baseline is restated.

## Section 1 — Corrected Diagnosis

SK-V13 was bracketed as PASS-ADMIT-PENDING under the prior synthesis. The
six-agent audit pack reverses that disposition. SK-V14 starts from the
honest baseline.

### 1.1 Survives — architectural pillars carry forward

These eight pillars hold under audit and continue into SK-V14 (per
`DISPATCH-CONTEXT.md §1 + audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
"Decision-engine fold" + "Honest patterns left clean"`):

| Pillar | Status | Citation |
|---|---|---|
| W5 bbnf-regex extraction | LOAD-BEARING | `audit-overfit/validation/v4-decision-engine-trace.md §1` |
| W6 e-graph Language + cost | LOAD-BEARING (extraction-only) | `v4 §1` |
| W7 CSP solver, 5 constraints, fail-closed | LOAD-BEARING | `v4 §1 + §2`; `skinny/crates/passes/lib.rs:476–478` |
| `bbnf-simd` (52 files) | grammar-neutral | `v3 §4` |
| OffsetFlags + Tape | grammar-neutral | `v3 §2` |
| `generated_json::parse_direct` | real codegen from grammar | `v2 §3.1 + §4.1` |
| `generated_real_typed::parse_*` | real codegen from grammar | `v2 §4.1` |
| 15 CSS `.bbnf` grammars at `/grammar/css/l4/` | present, unwired | (R4 makes load-bearing) |

### 1.2 Does not survive — falsified or downgraded

| Item | Audit verdict | Citation |
|---|---|---|
| 25 CSS L4 admitted rows (incl. SK-V12 W1b 2.54× headline) | hand-written templates with fake `@generated`; no `regen-css` xtask | `v1 §1 + §2 + §5` |
| 5 JSON `parse_only` admits (W14.1–.5) | gate-relabel only; parser unchanged; comparator misnamed | `v2 §1 + §2` |
| 4 JSON direct admits (dispatch §1) / 6 under broader ledger | REAL parsers, comparator misbinding (eager DOM, not strict per-corpus struct deser) | `v2 §3.2 + v6 §3` |
| 7 JSON typed admits (dispatch §1) / 11 under broader ledger | REAL parsers, comparator misbinding (eager DOM, not per-corpus typed deser) | `v2 §4.2 + v6 §3` |
| W8 per-grammar policy | COSMETIC; zero runtime consumption | `v4 §4 + §6` |
| W9 same-substrate union | COSMETIC; hardcoded constants | `v4 §5 + §6` |
| 30 Lock 14 violations (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW) | 8 hand-written per-grammar provider modules under `skinny/crates/codegen/` are the recurrence vector | `v3 §1` + Lock 14 (`LOCKS.md:220–238`) |

**Reconciliation (per CH6 §2.2 REJ-2 + §0.2 above).** The direct + typed
counts in the table reflect both the dispatch §1 bind (4 + 7) and the
broader ROLLING-SOTA-DELTA ledger peer-measured by α-A and α-D
(6 + 11). All 17 rows under the wider ledger reclassify
AUDIT-FALSIFIED under v6 §1 rows 3-4; PRUNE-1 binds the wider 6+11
population so the revert covers every comparator-misbinding row, not
just the dispatch-narrowed 11. Per-row reconciliation sits at
α-A:117-122 (direct +2: marine_ik, instruments) + α-A:161-169 (typed
+4: random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4
plus update_center W15.1 adjusted) + α-D:281-291 + α-D:353-368.

### 1.3 Honest rolling delta (the SK-V14 starting baseline)

```
JSON parse_only: 0 / 17  (all OPEN; needs distinct parse_only path + Skipper-class comparator)
JSON direct:     0 / 17  (4 cells need comparator rebind; 13 reopen fresh)
JSON typed:      0 / 17  (7 cells need comparator rebind; 10 reopen fresh)
CSS L4:          0 / 24  (all OPEN; templates pending PRUNE-2; amended skinny-side regen-css pending W2 rerun)
```

Campaign at zero on numbers; non-zero on architecture. `[no-deferrals]`
applies — every SK-V14 wave integrates its optimisation in-pass; no
"future wave / future tranche" deferral without architectural-block
evidence.

## Section 2 — Telemetry Binding

The SK-V14 `skinny/RESULTS.md` schema extends the SK-V13 binding (per
`sk-v13/SYNTHESIS.md §2` and `PASS-ALPHA.md §4.3`) with two
audit-overlay columns. The bench harness must emit the schema verbatim;
`cargo xtask gate-json` rejects any row missing required columns.

| Column | Required rule |
|---|---|
| `row` / `Corpus` / `Workload` | required; JSON rows cover 17 corpora × 3 planes (51 cells); CSS rows cover 24 features |
| `Outcome` / `Verdict` | required; `A/GO` only after strict equality + SOTA margin |
| `Strictness` | required; SOTA anchor must be strict |
| `parse_utf8` / `escape_complete` / `flaw_probe` | required for JSON; CSS analogue names strictness + recovery mode |
| `Output plane` | required; comparisons only count on same plane |
| `Track 1 Mbps` / `Track 2 Mbps` | required for every row |
| `track2_entry_point` | **NEW (CH5)** — symbol path of the Track 2 oracle entry point; `xtask gate-json` rejects any row where the Track 1 and Track 2 entry-point symbol paths share a common ancestor in `runtime::tape::` beyond the public `Tape` / `OffsetFlags` types |
| `comparator_plane` | **NEW (R1)** — names the strict-mode comparator used per plane (parse_only/direct/typed); rejects any row whose comparator does work asymmetric to Track 1 |
| `per_iter_equality` | **NEW (R2)** — boolean column emitted per iteration; PASS only if equality verified inside the timing region |
| `sonic-rs strict Mbps` | required for every JSON row × plane (per R1) |
| `sonic-rs lossy Mbps` | optional flaw probe only; never SOTA anchor |
| `simdjson DOM Mbps` / `simdjson On Demand Mbps` | required when runnable; plane disclosed |
| `yyjson default Mbps` | required when runnable; strictness disclosed |
| `asmjson SWAR Mbps` / `asmjson AVX-512 Mbps` | optional flaw probes unless same-plane strict runnable |
| `RapidJSON default Mbps` | optional flaw probe only |
| `serde_json Mbps` | required JSON strict baseline |
| `lightningcss Mbps` | required for every CSS parity row |
| `cssparser_oracle Mbps` or golden oracle | required for every CSS parity row |
| `Δ vs SK-V13` | required for every carried row |
| `Δ vs SOTA` | required; sonic-rs strict for JSON, lightningcss for CSS |
| `Hot leaf` | required; stale inherited profile names fail S-P1 |
| `audit_overlay_verdict` | **NEW (audit overlay)** — enum (AUDIT-FALSIFIED / AUDIT-SUSTAINED / AUDIT-PENDING) per row; falsified rows cite the validation-pack §reference that falsified |
| `Signal` | required; PASS or NO-GO with reason |
| `REDRESS id` / `wave id` / `run id` / `host` | required for admission |

The rolling delta at `restart/skinny/ROLLING-SOTA-DELTA.md` must be
re-baselined post-PRUNE-1 + PRUNE-2 to the §1.3 honest delta. Subsequent
admits append per row; demotions are the bracket failure under G7.

## Section 3 — Candidate Shortlist (PRUNE-first)

Five candidate slots map naturally onto the SK-V14 R-targets per
`DISPATCH-CONTEXT.md §α-E`. These are the load-bearing candidates the
skinny S-P3 wave plan consumes:

| # | Scope | Same-wave consumer | Falsifiability gate | LOC budget | Risk |
|---|---|---|---|---:|---|
| **C-1** | R3 PRUNE-3A + PRUNE-3B + PRUNE-3C + PRUNE-3D + PRUNE-4 (Lock-14 refactor cluster). W5A has closed a grammar-neutral source-consuming request boundary that passes grammar source + workspace metadata into codegen and parses required V1 grammar-source constructs without grammar-id branches. W5B-FRONTEND builds the generic BBNF grammar-source frontend/import/IR closure and lowers CSS L4 compatibility constructs into canonical IR. W5C-GEN builds the provider-free runtime generator body and removes live provider-backed production dispatch without deleting provider/template residue. W5D-DELETE deletes provider/template clusters and closes the Lock 14 baseline only after W5C-GEN is load-bearing. W6 refactors 67 hand-written per-grammar files in `crates/core/src/runtime/{grammar}/` into emitted output (9 sub-waves, W6.0..W6.8). | regen-derived runtime for every grammar emitted in the same waves; W5A same-wave consumers are `regen-css` plus seven CSS companions with JSON/Sheets/BBNF-self proof; W5B-FRONTEND same-wave consumers are frontend compatibility-lowering coverage plus JSON/Sheets/BBNF-self proof; W5C-GEN same-wave consumers are `regen-css`, companions, `check-json`, and provider-reachability grep; W5D-DELETE same-wave consumers are the deletion gate, `regen-css`, companions, `check-json`, and Lock 14 baseline; W6 per-sub-wave gate runs before commit. | W5A: source/metadata consumed by codegen; all seven CSS L4 profiles through the source-consuming path; JSON unchanged-output proof; Sheets/BBNF-self fail-closed or generated-role witnesses; no provider/template deletion. W5B-FRONTEND: compatibility constructs such as `@ws`, `@pretty`, `?w`, `>>`, `<<`, span capture, typed host projections, and import graph lower into canonical IR without new public syntax or grammar-name branches. W5C-GEN: production entrypoints have no live `render_runtime_profile` / `RuntimeProvider` / `GrammarProfile` / provider dispatch and generic production code has no grammar-name match arms; provider/template files may remain only as unreachable W5D residue. W5D-DELETE: `find skinny/crates/codegen/src -name '*_provider.rs' \! -name 'grammar_provider.rs'` and `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates'` return ZERO; Lock 14 baseline gate passes. W6: `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO per-grammar dirs. Forward invariant (post-redress, permanent): any new grammar added under `workspace.metadata.bbnf.grammars.{name}` produces ZERO new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf, grammar}/src/` and ZERO new directories in `crates/core/src/runtime/`; the Lock 14 baseline gate rejects any commit that violates this. | W5A closed 921 + W5B-FRONTEND ≤1.0k + W5C-GEN ≤1.0k + W5D-DELETE ≤400 + W6 ≤2.0k; total C-1 ≤5.321k actual/cap envelope | VERY HIGH (architectural; multi-wave) |
| **C-2** | R1 + R2 (comparator rebind + per-iter equality oracle). Three plane-correct strict comparators; per-iter equality inside the timing region. | bench harness consumes the rebound comparators on every named JSON row; `xtask gate-json` enforces the schema. | bench harness emits an equality-pass column per iter; `xtask gate-json` rejects any row whose equality column is empty. | 600 – 1.08k | HIGH (harness + comparator surface) |
| **C-3** | R4 + R5 (regen-css pipeline + production corpora; first instance of the `regen-{grammar}` family — the xtask binary parametrises a grammar-neutral generator). `cargo xtask regen-css` consuming the 15 `.bbnf` files at `/grammar/css/l4/`; `skinny/corpora/css-l4-sk-v14/` with Bootstrap + Tailwind + Material + Animate (~960 KB). | skinny runtime regenerated from the 15 `.bbnf` files in W2; bench rows wired to the new corpora in W3. | W2 round-trip xtask check returns clean on the skinny runtime tree (`rm -rf skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask regen-css && git diff --exit-code -- skinny/crates/runtime/src/grammars`) + seven exact `check-css-l4-*` companions pass + bypass-header detector traces every skinny match to a registered W2 emission; root `crates/core/src/runtime/css_l4/` is excluded from W2 and owned by W6.0. `du -sh skinny/corpora/css-l4-sk-v14` > 800 KB in W3; see §5 + hardening V1 CH7 §3.1. | 1.2k – 2.0k | HIGH (xtask + corpora + skinny-side round-trip) |
| **C-4** | R3 PRUNE-5 (W8 + W9 scaffold → load-bearing). CSP-chosen shape produces measurable runtime divergence on a named pre-wave row. | CSP-selected shape produces measurable runtime divergence on at least one named pre-wave row in the same wave. | named pre-wave row `json/numbers/direct_to_struct/main`: pre-wave hot leaf `parse_value_at`, post-wave hot leaf names the W11.1 number-specialised symbol explicitly in the samply trace; row hot leaf attribution changes in `RESULTS.md`; per-shape Lock-1 triad (`substrate_target`, `retention_lifetime`, `policy_owner`) declared in REDRESS; no row admit cites W8/W9 without measured runtime consumption. | 800 – 1.4k | VERY HIGH (Lock-1 substrate-ceiling surface) |
| **C-5** | R3 PRUNE-1 + PRUNE-2 (clean revert of fake admits). PRUNE-1: revert W14.1–W14.5 in `RESULTS.md` + `ROLLING-SOTA-DELTA.md`; REDRESS per row cites `v2 §1–4`. PRUNE-2: revert 24 CSS L4 admitted rows in `restart/skinny/ROLLING-SOTA-DELTA.md`; REDRESS per row cites `v1 §1–6`. CSS provider/template deletion moves to PRUNE-3D and lands only after W5C-GEN proves the provider-free replacement generator body. | REDRESS per row cites the validation §reference; ROLLING-SOTA-DELTA rebases to the audit-zero baseline in the same commit set. | `restart/skinny/ROLLING-SOTA-DELTA.md` shows CSS L4 0/24 + parse_only 0/17; `skinny/REDRESS.md` carries 29 new row-keyed entries; W4 proves no CSS source/generator/provider/template deletion. | 250 – 500 | MED-LOW (revert + REDRESS scribe) |

Total envelope ≈ 5.65k – 9.90k across the five candidates per α-E §2 after
V7 W5B-GENR;
C-1 dominates, C-5 is mostly deletion. Risk-weighted: C-1 + C-4 carry
the architectural risk; C-2 + C-3 carry the throughput / reproducibility
risk; C-5 carries audit-trail risk only.

R6 / R7 / R8 (re-admit waves) are downstream CONSUMERS of C-1 through C-5
and belong in the SK-V14 wave program after these candidates land; they
are not standalone Pass Alpha candidates. PRUNE waves execute FIRST per
`ORCHESTRATOR-PROMPT.md` R3.

## Section 4 — S-P3 Constraints

S-P3 owns the detailed wave plan; it is constrained by this contract:

- the wave plan targets R1–R10 in full, not a shorter shortlist;
- the wave program executes PRUNE waves (C-5 → C-1 → C-2 → C-3 → C-4)
  BEFORE any new-admit wave; no R6/R7/R8 wave dispatches until the
  honest baseline is restated in `RESULTS.md` + `ROLLING-SOTA-DELTA.md`;
- every behaviour wave moves at least one row toward SOTA OR records an
  architectural-level intrinsic-block proof for the row family it
  touched;
- support-only landings are invalid; every primitive lands WITH its
  hot-path consumer in the same commit per `[execute-planned-architecture]`;
- independent waves dispatch concurrently only when file domains do not
  overlap and after required gates close; commit before parallelising;
  worktrees for overlap per `[agent-orchestration]`;
- no SPEC clause may inherit weaker scoping labels (`optional`,
  `fallback`, `diagnostic`, `support-only`, `scaffold-only`,
  `future-tranche`) for pinned R1–R10 work; those items become admitted
  row targets, architectural-block proofs, or user re-pin issues;
- no SPEC clause may authorise a new directive, BIR variant,
  `BackendShape`, public substrate API, or grammar-specific generic
  behaviour (Lock 14 binding); union variants admit only as same-tape,
  codegen-private, row-consumed shapes;
- any SPEC wave that wires `bbnf-simd` into CSS, union, JSON
  `parse_only`, or shared generated code carries `G-SIMD-GRAMMAR-POLICY`
  per `sk-v13/SYNTHESIS.md §4`: consuming-grammar quote/escape/control
  policy or no-string policy, scalar parity, checkasm/differential
  coverage, same-wave measured row consumption, no public substrate API,
  no retained sidecar classifier state. Every SIMD consumer wired by
  C-4 declares `substrate_target`, `retention_lifetime`, and
  `policy_owner` per `LOCKS.md:73-82`; `xtask gate-json` rejects any
  row whose REDRESS lacks the triple.
- after the decision-engine resolver lands (R3 PRUNE-5), the hardcoded
  P1–P8 cascade fails closed for JSON / CSS / Sheets / BBNF-self rows;
  silent fallback to the old cascade is not admission evidence;
- the §1 audit overlay (`audit_overlay_verdict` column) is gate-enforced
  per row; any row currently AUDIT-FALSIFIED requires fresh material
  differential evidence to re-admit, cited per REDRESS;
- S-P3 wave manifest inherits per-candidate LOC envelopes from α-E §2
  (C-1 2.8k–3.4k; C-2 600–1.08k; C-3 1.2k–2.0k; C-4 800–1.4k; C-5
  250–500; total ≈ 5.65k–8.38k); any wave exceeding its envelope by
  > 20 % escalates per `[generated-size-budget]`;
- C-1's forward invariant (no new `.rs` files in generic crates; no new
  directories in `crates/core/src/runtime/`; Lock 14 baseline gate
  rejects any commit that violates this) is permanent; S-P3 wave plans
  MUST cite it as the pre-condition for any new grammar admission wave
  (BBNF-self, Sheets, future grammars);
- the C-4 shape consumer is exercised across at least two grammar
  families before any C-4 admit cites runtime divergence as
  load-bearing; one-grammar runtime divergence is wave evidence, not
  admit evidence; the shape consumer in
  `skinny/crates/codegen/src/lib.rs` MUST dispatch on the CSP-emitted
  `BackendShape` enum alone — no `match grammar { Json => ..., CssL4
  => ... }` arm may appear in the dispatch path;
- every wave fans out as research → plan → redress in distinct commits
  per `[triumvirate-discipline]` + ORCHESTRATOR §8; a wave that lands
  a single research-plan-redress mega-commit fails the gate at S-P3.

## Section 5 — Pre-Blocked And Unblocked Routes

Pre-blocked (carrying SK-V13 list forward, plus SK-V14 P-1 … P-7):

- claiming SK-V14 close from any subset of R1–R10 without architectural-
  block proof for the remainder;
- using lossy sonic-rs, permissive RapidJSON, or different output planes
  as a SOTA anchor;
- treating `parse_only` as diagnostic-only;
- closing a JSON row through REDRESS-119 / REDRESS-120 history without
  fresh SK-V14 evidence (both LIFTED per addendum; HISTORY only);
- producer-only SIMD, union, resolver, or codegen artefacts without
  same-wave consumer measurement;
- non-JSON or shared consumers of `bbnf-simd` alphabet-only classifier
  dispatch unless `G-SIMD-GRAMMAR-POLICY` proves the selected path
  cannot inherit JSON quote/escape/control constants;
- grammar-name branches in generic crates, parser-owned sidecars, hidden
  Track 1 ≡ Track 2 coupling, stale comparator sidecars;
- new admit attempt before R3 PRUNE waves converge and honest baseline
  restated;
- patterns P-1 through P-7 of §0.4 (fake `@generated` header; mislabelled
  eager-DOM comparator; tiny-fixture Mbps inflation; gate-relabel as
  admit; scaffold-only as load-bearing; per-grammar provider modules in
  generic codegen; Track 1 ≡ Track 2 dishonesty);
- dispatching any implementation wave before G-Omega closes and S-P0
  Overfit Audit Pass converges.

Unblocked:

- all 51 JSON cells × 3 planes, including REDRESS-119 residuals and all
  `parse_only` rows (addendum A2 + A3 binding);
- the full 24-feature CSS L4 parity matrix under strict lightningcss
  equality (addendum A1);
- union-substrate category attempts with fresh material differential
  through R3 PRUNE-5 (W8 + W9 wired);
- SIMD / ASM attempts after scalar reference, checkasm/parity, Lock 16,
  and same-wave consumer evidence;
- decision-engine consumption of CSP solver shape selections under
  bounded abrogate criteria (e-graph OOM; CSP > 1 s per grammar; stale
  cost > 30 % of candidate expressions; order-dependent rewrites > 10 %
  variance).

## Section 6 — Close Posture

SK-V14 opens prune-first. The audit pack has reduced the campaign to
the honest baseline: 0 ADMITTED across 51 JSON cells × 3 planes + 24
CSS L4 features. The architectural skeleton (W5A / W5B-FRONTEND / W5C-GEN / W5D-DELETE / W6 / W7 / `bbnf-simd` /
OffsetFlags / Tape / `generated_json::parse_direct` / `generated_real_typed::parse_*` /
15 CSS `.bbnf` grammars) holds and is reusable.

The SK-V14 contract is intentionally aggressive in obligation and
intentionally surgical in sequencing. PRUNE waves restore baseline
honesty; R1 + R2 restore comparator integrity; R4 + R5 establish CSS L4
test infrastructure (grammar-derived pipeline + real corpora); R3
PRUNE-5 wires W8 + W9 into the decision engine; R6 + R7 + R8 re-admit
under the rebound bar. The close is the pinned bar of §0.1: full ADMIT
or per-row architectural-block proof across the 51 JSON cells + 24 CSS
features.

The indefatigability clause carries — SK-V14 is one bracket in a
campaign that closes only at full admit. The work between here and that
close is what SK-V14, SK-V15, … own.
