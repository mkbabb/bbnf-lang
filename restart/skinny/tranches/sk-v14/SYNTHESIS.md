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

### 0.3 R-target goalset (the load-bearing layer)

Pass Alpha binds the SK-V14 R-targets verbatim from
`restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`:

| R | Scope | Acceptance |
|---|---|---|
| **R1** | comparator rebind: 3 plane-correct strict comparators | parse_only → sonic-rs Skipper-class (structural skip); direct → sonic-rs strict struct deser per corpus; typed → per-corpus typed struct deser. No row admits until its plane's comparator is strict-vs-strict. |
| **R2** | per-iteration equality oracle | equality on EACH bench iter inside the timing region, not startup-only. The harness emits an equality-pass column per iter; `xtask gate-json` rejects rows whose equality column is empty. |
| **R3** | PRUNE waves before any new admit attempt | PRUNE-1 revert W14.1–.5; PRUNE-2 delete 7 CSS templates + revert 24 CSS rows; PRUNE-3 trait-dispatch + grammar-agnostic codegen template; PRUNE-4 64 hand-written per-grammar runtime files in `crates/core/src/runtime/{grammar}/` refactored to emitted output (8 sub-waves); PRUNE-5 wire W8 + W9 from SCAFFOLD to LOAD-BEARING. |
| **R4** | `cargo xtask regen-css` pipeline | consumes 15 `.bbnf` files at `/grammar/css/l4/`; emits CSS L4 runtime modules; round-trip clean (`delete generated → run xtask regen-css → diff empty`). |
| **R5** | production corpora `skinny/corpora/css-l4-sk-v14/` | Bootstrap + Tailwind + Material + Animate, ~960 KB. Tiny embedded fixtures unacceptable for admit. |
| **R6** | CSS L4 re-admit (honest) | after R3+R4+R5, each CSS L4 row attempted via grammar-derived pipeline, real corpora, work-equivalent comparator (lightningcss full-parse; cssparser full-parse; no fact-stream vs full-AST asymmetry). |
| **R7** | JSON direct + typed re-admit | after R1+R2, every JSON direct + typed row re-baselined against rebound strict comparators. Cells previously HOLDING under the misbound comparator hold again under the right comparator, or are reverted. |
| **R8** | JSON `parse_only` distinct path | stand up a distinct parse_only code path in `generated_json` (no full-tape build). Wire to Skipper-class comparator. Then attempt admit. |
| **R9** | carried pillars unchanged | W5 / W6 / W7 / bbnf-simd / OffsetFlags / Tape stand; R3 must not regress them. |
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
  `[clean-regen-discipline]`.
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
  Lock-14 recurrence vector. SK-V14 PRUNE-3 collapses these to ONE
  grammar-agnostic generator template consuming grammar source + workspace
  metadata.
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
| 4 JSON direct admits | REAL parsers, comparator misbinding (eager DOM, not strict per-corpus struct deser) | `v2 §3.2 + v6 §3` |
| 7 JSON typed admits | REAL parsers, comparator misbinding (eager DOM, not per-corpus typed deser) | `v2 §4.2 + v6 §3` |
| W8 per-grammar policy | COSMETIC; zero runtime consumption | `v4 §4 + §6` |
| W9 same-substrate union | COSMETIC; hardcoded constants | `v4 §5 + §6` |
| 30 Lock 14 violations (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW) | 8 hand-written per-grammar provider modules under `skinny/crates/codegen/` are the recurrence vector | `v3 §1` + Lock 14 (`LOCKS.md:220–238`) |

### 1.3 Honest rolling delta (the SK-V14 starting baseline)

```
JSON parse_only: 0 / 17  (all OPEN; needs distinct parse_only path + Skipper-class comparator)
JSON direct:     0 / 17  (4 cells need comparator rebind; 13 reopen fresh)
JSON typed:      0 / 17  (7 cells need comparator rebind; 10 reopen fresh)
CSS L4:          0 / 24  (all OPEN; templates deleted + regen-css pipeline built)
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

| # | Scope | Falsifiability gate | Risk |
|---|---|---|---|
| **C-1** | R3 PRUNE-3 + PRUNE-4 (Lock-14 refactor cluster). Replace `RuntimeProvider` enum with trait-based dispatch in `skinny/crates/`. Collapse 8 per-grammar provider modules under `codegen/` into ONE grammar-agnostic generator template consuming grammar source + workspace metadata. Refactor 64 hand-written per-grammar files in `crates/core/src/runtime/{grammar}/` into emitted output (8 sub-waves). | `find skinny/crates -name '*.rs' \| xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO post-redress; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO per-grammar dirs. | HIGH (architectural; multi-wave) |
| **C-2** | R1 + R2 (comparator rebind + per-iter equality oracle). Three plane-correct strict comparators; per-iter equality inside the timing region. | bench harness emits an equality-pass column per iter; `xtask gate-json` rejects any row whose equality column is empty. | MED (harness-local) |
| **C-3** | R4 + R5 (regen-css pipeline + production corpora). `cargo xtask regen-css` consuming the 15 `.bbnf` files at `/grammar/css/l4/`; `skinny/corpora/css-l4-sk-v14/` with Bootstrap + Tailwind + Material + Animate (~960 KB). | round-trip xtask check returns clean; corpora dir > 800 KB. | MED (xtask + corpora) |
| **C-4** | R3 PRUNE-5 (W8 + W9 scaffold → load-bearing). CSP-chosen shape produces measurable runtime divergence on a named pre-wave row. | named row measurement shows runtime divergence keyed on CSP shape decision; no row admit cites W8/W9 without measured runtime consumption. | MED (wires existing scaffold) |
| **C-5** | R3 PRUNE-1 + PRUNE-2 (clean revert of fake admits). PRUNE-1: revert W14.1–W14.5 in `RESULTS.md` + `ROLLING-SOTA-DELTA.md`; REDRESS per row cites `v2 §1–4`. PRUNE-2: delete 7 CSS hand-written template files + their `include_str!`'d `generated.rs`; revert 24 CSS L4 admitted rows; REDRESS per row cites `v1 §1–6`. | post-redress `git grep -l '@generated' crates/core/src/runtime` excludes any file produced by hand; `ROLLING-SOTA-DELTA.md` shows CSS L4 0/24 + parse_only 0/17. | LOW (revert + REDRESS scribe) |

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
  no retained sidecar classifier state;
- after the decision-engine resolver lands (R3 PRUNE-5), the hardcoded
  P1–P8 cascade fails closed for JSON / CSS / Sheets / BBNF-self rows;
  silent fallback to the old cascade is not admission evidence;
- the §1 audit overlay (`audit_overlay_verdict` column) is gate-enforced
  per row; any row currently AUDIT-FALSIFIED requires fresh material
  differential evidence to re-admit, cited per REDRESS.

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
CSS L4 features. The architectural skeleton (W5 / W6 / W7 / `bbnf-simd` /
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
