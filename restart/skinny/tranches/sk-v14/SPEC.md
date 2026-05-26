# SK-V14 SPEC — S-P3 Wave Plan

Date: 2026-05-23.

Status: S-P3 V1 planning packet. This file is not an implementation
dispatch. It folds Pass Alpha's SYNTHESIS.md goalset, the S-P0
audit-overfit synthesis (74 findings; 3 architectural sequencing
constraints), the S-P2 §3Z COHORT-LOCKED candidate pool (V3 HEAD
`ebe84954b`), the S-P3 P3-A..P3-E parallel outputs (V1 dispatched
concurrently with this file), and the existing alpha packet into a
conditional W0-W11 wave plan. The shape mirrors `restart/skinny/
tranches/sk-v8/SPEC.md` verbatim.

Authority:

- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (Pass Alpha goalset; §0 close-condition + R1-R10 + P-1..P-7 pre-blocks)
- `restart/skinny/tranches/sk-v14/HANDOFF.md` (tranche handoff)
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` (R-target acceptance criteria)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (74 findings; §2 sequencing constraints; §3 PRUNE-list)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md` (S-P1 LOCKED profile)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (S-P2 §3Z COHORT LOCK; §6 carry-forward packets)
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (S-P3 shortlist; ≤8 candidates)
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (W0..W{n≤12} sequencing)
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (per-wave gates + Mbps thresholds)
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (24-column schema + SK-V14 additions)
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (per-wave REDRESS pre-block list)
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (this file's drafting notes + provenance)
- `restart/skinny/tranches/sk-v8/SPEC.md` (the SPEC shape mirrored verbatim)
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (wave-execution contract)
- `restart/locks/LOCKS.md` (16 locks; Lock 1 + Lock 14 + Lock 16 v+1 load-bearing)
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/ROLLING-SOTA-DELTA.md`

Dispatch lock:

- No SK-V14 implementation wave dispatches from S-P3 itself.
- G-Omega user gate (per `ORCHESTRATOR-PROMPT.md:204`) is the only mandatory relinquish; S-P3 + T-P3 LOCK precedes.
- Current dispatch authority covers W0 only.
- W1-W11 are conditionally gated by this packet, but each remains blocked until W0 closes, the wave plan names exact owner paths and row gates, required CHALLENGE accepts, and the orchestrator/user dispatches that wave.
- PRUNE waves (W1 PRUNE-1, W4 PRUNE-2, W5 PRUNE-3, W6 PRUNE-4, W7 PRUNE-5) dispatch BEFORE any new-admit wave (W8 R6, W9 R7, W10 R8) per `ORCHESTRATOR-PROMPT.md:110` R3 + SYNTHESIS §4 constraint chain.

## Section 0 — Close Condition And Goalset

### Section 0.1 — Global Close Condition (R10 verbatim)

SK-V14 closes only when all of these are true (folding `SYNTHESIS.md:39-50`
R10 verbatim into the SK-V8 §0.1 ten-clause shape):

1. W0 creates a checked `SK-V14-open` baseline with no placeholder hot leaves, no stale-comparator residue, and the AUDIT-ZERO baseline (`0/17 parse_only / 0/17 direct / 0/17 typed / 0/24 CSS L4`) honestly captured.
2. Every current main row carries required profile, comparator, run, host, build, cost, freshness, audit-overlay, and delta telemetry per Section 0.4.
3. `xtask gate-json` rejects rows missing required SK-V14 telemetry (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`).
4. W1 makes plane-correct strict comparator evidence + per-iter equality oracle gate-consumed before any behavior wave can admit row quality.
5. W2 + W3 stand up the CSS L4 grammar-derived pipeline (`cargo xtask regen-css` round-trip clean + `skinny/corpora/css-l4-sk-v14/` ≥800 KB) BEFORE W4 (PRUNE-2) deletes hand-written CSS templates.
6. W5 + W6 (C-1 PRUNE-3 + PRUNE-4) collapse the 8 per-grammar provider modules and 67 hand-written per-grammar runtime files into ONE generic generator template BEFORE W7 (C-4 PRUNE-5) wires W8 + W9 from SCAFFOLD-ONLY to LOAD-BEARING.
7. Every JSON cell (51 = 17 corpora × 3 planes) and every CSS L4 feature (24 non-OUT_OF_SCOPE) either ADMITs > strict-vs-strict on the same plane / corpus / equality semantics OR carries an architectural-level intrinsic-block proof per row family. Implementation-limited misses are REOPEN, not close.
8. No pre-blocked route (P-1..P-7 per §15 + REDRESS watch-list) reopens without fresh W0 evidence, same-wave consumer, REDRESS citation, no-regression gate, and CHALLENGE acceptance.
9. Lock 1 (v+1 substrate-target/retention-lifetime/policy-owner triple), Lock 14 (v+1 generated-output allowance), and Lock 16 (v+1 primitive-manifest gating) gates pass at every wave close.
10. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/ROLLING-SOTA-DELTA.md`, and `restart/skinny/tranches/sk-v14/HANDOFF.md` agree at close.

Per `SYNTHESIS.md:47-50`: the campaign is **indefatigable**. Successive
tranches roll automatically until full ADMIT or per-row / per-feature
intrinsic-block proofs cover everything; SK-V14 close without full
ADMIT brackets SK-V15 immediately under the same pinned bar.

### Section 0.2 — Comparator Classes (R1: three plane-correct strict comparators)

SK-V14 uses three comparator classes plus the R1-mandated plane-correct
strict comparator triad (per `SYNTHESIS.md:91-94` R1):

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor (plane-correct triad per R1) | parse_only → sonic_rs Skipper (structural-skip-only); direct → sonic_rs strict struct deser per corpus; typed → per-corpus typed struct deser; CSS rows → lightningcss full-parse + cssparser full-parse | Strict admission only when plane / corpus / equality semantics match the candidate row AND `comparator_plane` column populated AND `per_iter_equality` column PASS per iteration AND validation occurs inside the measured row |
| Same-run flaw probe | sonic-rs lossy; permissive RapidJSON; serde_json with relaxed UTF-8 | Planning only; never strict admission |
| Sidecar planning signal | simdjson, yyjson, RapidJSON, asmjson when not refreshed under same-run rules; pre-W1 stale sonic-rs single-lane fan-out evidence | Planning only until freshness, strictness, and output-plane rules are satisfied |

Strict admission is executable, not prose-only: `xtask gate-json` MUST
reject strict admission unless the comparator plane matches the row
output plane, `comparator_strictness=strict`, the comparator is the
SK-V14 R1 plane-correct anchor admitted by id, the `per_iter_equality`
column is PASS for every iter, and UTF-8/control/escape validation
occurs inside the measured row.

`Strictness=deferred`, `parse_utf8=view-boundary`, stale sidecars,
sidecar-only evidence, historical deltas, and plane mismatch are
guard telemetry only.

The current pre-W1 single-lane `sonic_rs_anchor` at `skinny/crates/
bbnf-bench/benches/json_parity.rs:87-102` (per S-P0 A2 F8) is the
structural cause of the 11 JSON misadmits the audit pack falsified;
W1 deletes it and wires the three plane-correct anchors.

### Section 0.3 — Outcome Enum

The current schema supports (SK-V8 §0.3 verbatim):

```text
A
C
G
K
L
N-direct
S
```

`L`, `N-direct`, and `S` remain valid SK-V14 W0 outcomes. `S` is the
explicit substrate-guard / non-SOTA spelling for admission-capable
parse outcomes; hard parse failures remain specific (`L`) instead of
being demoted. Neither hard-failure outcomes nor `S` may support strict
SOTA admission.

The new `audit_overlay_verdict` column (per §0.4) is enum
{AUDIT-FALSIFIED, AUDIT-SUSTAINED, AUDIT-PENDING}; this is NOT an
outcome column but a per-row audit-overlay disposition column gated
by `xtask gate-json`.

### Section 0.4 — Required Telemetry

The rendered `skinny/RESULTS.md` table may keep the existing 26-column
schema surface. SK-V14 adds required report/gate fields after W0;
they may be rendered as columns, a gate-consumed manifest, or a
gate-consumed JSON payload, but they must be consumed by `xtask
gate-json` in the same wave.

Required fields (SK-V8 §0.4 + SK-V14 SYNTHESIS §2 additions):

```text
row / Corpus / Workload                            (required)
Outcome / Verdict                                  (required; A/GO only after strict equality + SOTA margin)
Strictness                                         (required; SOTA anchor must be strict)
parse_utf8 / escape_complete / flaw_probe          (required for JSON; CSS analogue names strictness + recovery mode)
Output plane                                       (required; comparisons only count on same plane)
Track 1 Mbps / Track 2 Mbps                        (required for every row)
track2_entry_point                                 (NEW — CH5; symbol path of Track 2 oracle; gate-json rejects rows where Track 1 and Track 2 entry-point symbols share a common ancestor in runtime::tape:: beyond public Tape / OffsetFlags types)
comparator_plane                                   (NEW — R1; names the strict-mode comparator per plane; rejects rows with asymmetric work)
per_iter_equality                                  (NEW — R2; boolean per iter; PASS only if equality verified inside timing region)
audit_overlay_verdict                              (NEW — audit overlay; enum AUDIT-FALSIFIED / AUDIT-SUSTAINED / AUDIT-PENDING; falsified rows cite the validation-pack §reference)
sonic-rs strict Mbps                               (required for every JSON row × plane per R1)
sonic-rs lossy Mbps                                (optional flaw probe only; never SOTA anchor)
simdjson DOM Mbps / simdjson On Demand Mbps        (required when runnable; plane disclosed)
yyjson default Mbps                                (required when runnable; strictness disclosed)
asmjson SWAR Mbps / asmjson AVX-512 Mbps           (optional flaw probes unless same-plane strict runnable)
RapidJSON default Mbps                             (optional flaw probe only)
serde_json Mbps                                    (required JSON strict baseline)
lightningcss Mbps                                  (required for every CSS parity row)
cssparser_oracle Mbps or golden oracle             (required for every CSS parity row)
Δ vs SK-V13                                        (required for every carried row)
Δ vs SOTA                                          (required; sonic-rs strict for JSON, lightningcss for CSS)
Hot leaf                                           (required; stale inherited profile names fail S-P1)
Signal                                             (required; PASS or NO-GO with reason)
REDRESS id / wave id / run id / host               (required for admission)
substrate_target / retention_lifetime / policy_owner  (required per Lock 1 v+1 for any wave admitting a SIMD/union/cost-shape consumer; allowed values per LOCKS.md:76-82)
comparator_id / comparator_strictness / comparator_freshness / measured_validation_path  (required per SK-V8 W1 binding)
CostFacts rule id / chosen shape / rejected alternative ids  (required per W7 PRUNE-5 + SK-V8 W1)
substrate_surface / structural_projection_status / substrate_cardinality / same_wave_consumer_class  (required per SK-V8 W2 binding)
track2_independence_status                         (required for every row claiming Track 1 + Track 2 ADMIT)
Sidecar freshness                                  (required when sidecar Mbps cited; explicit absent:<reason> when absent)
SK-V14-open delta                                  (required for every row; throughput cells stay within ±1.0% at W0 close)
```

Every emitted field must be consumed by `xtask gate-json` in the same
wave. Missing required fields, unsupported outcome, strictness
mismatch, stale sidecar, producer-only telemetry, W0 behavior drift,
missing W1 strict comparator binding, missing W7 CostFacts shape
choice, missing W4 audit_overlay_verdict on a previously-FALSIFIED
row's revert, Lock 14 generic leak, or cap overflow rejects the wave.

### Section 0.5 — Opening Row Goalset (AUDIT-ZERO baseline)

Per `SYNTHESIS.md:54-71` §0.2 + ORCHESTRATOR-PROMPT.md:71-77 honest
baseline, current main-table state at SK-V14 starting point HEAD
`12ff0744e` after PRUNE-1 + PRUNE-2 reverts:

| Family | Pre-PRUNE state | Audit-corrected | SK-V14 posture |
|---|---|---|---|
| JSON `parse_only` | 5 ADMITTED (W14.1-.5; gate-relabel) | 0 ADMITTED | all 17 reopen; W10 stands up distinct parse_only path + Skipper-class comparator |
| JSON `direct_to_struct` | 4-6 ADMITTED (comparator-misbound) | 0 ADMITTED | all 17 reopen; W9 re-attempts under R1 sonic-rs strict struct deser per corpus |
| JSON `real_typed_struct` | 7-11 ADMITTED (comparator-misbound) | 0 ADMITTED | all 17 reopen; W9 re-attempts under R1 per-corpus typed struct deser |
| CSS L4 features | 24 ADMITTED (incl. SK-V12 W1b 2.54× headline; hand-written templates with fake `@generated`) | 0 ADMITTED | all 24 reopen; W8 re-attempts via grammar-derived pipeline + production corpora + work-equivalent comparator (lightningcss full-parse; cssparser full-parse) |

**Per cell** the bar is `Track 1 > comparator strict + 1` AND
feature-coverage match (every variant the comparator accepts, the row
accepts; every variant it rejects, the row rejects) AND same-plane /
same-corpus / same-equality semantics. Anything less is REOPEN.

W0 target for all 51 JSON cells + 24 CSS L4 features: capture
`SK-V14-open`, populate required SK-V14 telemetry (per §0.4), and
keep every throughput cell within ±1.0% of the captured seed. The
honest baseline restates the SYNTHESIS §1 binding; the 25 CSS rows +
5 parse_only admits + 4-6 direct admits + 7-11 typed admits recorded
in `restart/skinny/ROLLING-SOTA-DELTA.md` at commit `653cdf795+w15.1-
redress` are FALSIFIED by the audit pack. W1 (PRUNE-1) + W4 (PRUNE-2)
revert them.

The post-W0 carried-pillar floors (R9 per SYNTHESIS §0.3): W5 +
W6 + W7 + `bbnf-simd` + OffsetFlags + Tape stand. C-1 (W5 + W6
PRUNE-3 + PRUNE-4) must not regress them.

## Section 1 — Non-Negotiables

Inheriting SK-V8 §1 verbatim + SK-V14 SYNTHESIS §4 + S-P2 §6
carry-forward additions + Lock 1 v+1 + Lock 14 v+1 + Lock 16 v+1:

- No new BBNF directives.
- No new BIR variant.
- No new `BackendShape` variant.
- No `UnionTape`.
- No new substrate surface; W7's CSP-shape consumption is representation replacement inside the singular retained `Tape`, not a new substrate.
- No new public substrate API.
- No parser-owned structural cursor / facts / aux table / density cache / sidecar event vector.
- No parallel or sidecar substrate.
- No JSON policy in generic crates (Lock 14 binding).
- No strict admission except strict-vs-strict on a matching output plane.
- No stale sidecar, permissive, lossy, historical, or view-boundary evidence as strict admission.
- No primitive, kernel, generated path, or substrate representation without a same-wave hot-path consumer (per S-P2 V3 §6.1 CF-3 3-gate cell: scalar-reference status / checkasm-parity expectation / same-wave-consumer NAMED).
- Scalar reference and checkasm parity (per Lock 16 v+1 BBNF_SIMD_STRICT=1) are required before primitive wiring.
- Research, plan, CHALLENGE when required, and redress remain distinct phases per `SKINNY-TRIUMVIRATE.md §9` triumvirate-role-separation.
- Every miss becomes REDRESS evidence or an explicit routed residual.
- No deferrals: a wave cannot close on "wired", "advisory", "future consumer", "integrated", or "paper close" language without measured evidence (per `[no-deferrals]`).
- Any wave admitting any dispatch-envelope-internal primitive ships F-V2-P1ABC-RERECORD as Stage 0 of the same wave per S-P2 V3 §6.3 — cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites; consumers (must-bind): P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13.
- Any wave admitting the long-string-body SIMD scan primitive (the three convergent identifiers `long_string_body_simd_scan` / `scan_string_special_block_sweep_64` / quote-aware classifier composition per S-P2 V3 §6.2) MUST commit to ONE canonical primitive name + ONE canonical scalar-ref function — three orthogonal SIMD bodies for one primitive is REJECT per Lock 14 v+1.
- Lock 1 v+1: every e-graph candidate, backend rewrite, imported scanner plan, union candidate, and SIMD consumer declares `substrate_target` (∈ `local_temp_only`, `existing_tape`, `direct_sink`, `admitted_fact_output`), `retention_lifetime` (∈ `local_loop`, `generated_function`, `output_row`), and `policy_owner` (∈ `generated_grammar`, `caller_data`, `none`) per `LOCKS.md:76-82`; `xtask gate-json` rejects any row whose REDRESS lacks the triple.
- Lock 14 v+1: generic crates carry ZERO `match grammar { Json => ..., CssL4 => ... }` arms; ZERO grammar-named modules; ZERO grammar-specific types in public APIs; ZERO per-grammar feature flags; ZERO hand-written per-grammar runtime files (post-W6); per-grammar runtime is emitted from ONE grammar-agnostic generator template consuming grammar source + workspace metadata; `xtask gate-json` rejects any commit that introduces grammar-specific code in a generic crate.
- Lock 16 v+1: every `core::arch::*`, `target_feature`, and `asm!` use-site in `bbnf-simd`, parse-that facades, generated scanners, or collapsed-stage code maps to a manifest row containing stable primitive id, abstract primitive name, primary ISA/library citation, hardware gate, scalar reference, strict checkasm/parity command (`BBNF_SIMD_STRICT=1`), corpus/equality parity, grammar policy source, substrate target, retention lifetime, policy owner, same-wave production consumer, expected row/feature gate, LOC/risk, rollback path, abrogate threshold, and final disposition.
- **Executable verification mandate (CH7 V2 lesson + LAC-1E-12 procedural addendum):** any cited path:line in any wave's plan or redress MUST be re-executed at HEAD before commit; absence claims without captured command output are UNKNOWN verification actions, not gate closure (per Lock 3 v+1 verification clause + S-P2 dispatch-context §2).
- **CH7-V2 procedural addendum:** any past-perfect verb-tense claim ("landed", "delivered", "shipped") on a function body whose path:line returns NOT-PRESENT at the cycle HEAD is paper-close even if the cite chain is otherwise complete; the orchestrator loses the wave-slot truth. The discipline must be `ls`-existence-verified at cycle HEAD before adopting past-perfect tense (S-P2 V3 §3.2 CH6-E).
- Any SPEC wave that wires `bbnf-simd` into CSS, union, JSON `parse_only`, or shared generated code carries `G-SIMD-GRAMMAR-POLICY` per `sk-v13/SYNTHESIS.md §4`: consuming-grammar quote/escape/control policy or no-string policy, scalar parity, checkasm/differential coverage, same-wave measured row consumption, no public substrate API, no retained sidecar classifier state.
- After the W7 decision-engine resolver lands, the hardcoded P1-P8 cascade fails closed for JSON / CSS / Sheets / BBNF-self rows; silent fallback to the old cascade is not admission evidence.
- The §1 audit overlay (`audit_overlay_verdict` column) is gate-enforced per row; any row currently AUDIT-FALSIFIED requires fresh material differential evidence to re-admit, cited per REDRESS.
- aarch64 / Apple M5 Max binding per user pin (`ORCHESTRATOR-PROMPT.md:183`); x86 OUT.

## Section 2 — Wave Manifest, Caps, And Reruns

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Implementation/redress cap |
|---|---|---|---|---|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock (SK-V14-open) | Dispatchable only after G-Omega | 0 production behavior LOC; reauthorized telemetry gate/report/Lock14 scope per Section 3 accounting; ≤250 report/gate/test/doc LOC | ≤90 min |
| W1 | Section 4 | Comparator Rebind + Per-Iter Equality + PRUNE-1 (R1 + R2 + R3 PRUNE-1) | Conditional on W0 close | ≤1.08k C-2 source/test LOC + ≤500 C-5 part-A revert (delete-heavy); total ≤1.58k | ≤90 min |
| W2 | Section 5 | regen-css xtask (R4 — first instance of regen-{grammar} family; skinny runtime tree only after Pass Omega V3 W2R) | Conditional on W1 close | ≤2.0k C-3 part-A source/test LOC; generated output named separately | ≤90 min |
| W3 | Section 6 | Production CSS Corpora (R5; ~960 KB) | Conditional on W2 close | ≤200 corpora-staging LOC; corpora files are bytes-only, not source LOC | ≤90 min |
| W4 | Section 7 | PRUNE-2 — delete 7 CSS templates + revert 24 CSS admits (R3 PRUNE-2) | Conditional on W2 + W3 close (R4 MUST precede per S-P0 §2.1) | ≤500 C-5 part-B revert + 7-template delete; LOC delta is negative | ≤90 min |
| W5 | Section 8 | PRUNE-3 — Lock-14 refactor: trait dispatch + grammar-agnostic generator template (R3 PRUNE-3; C-1 part-A) | Conditional on W4 close | ≤1.4k C-1 part-A source/test LOC | ≤90 min |
| W6 | Section 9 | PRUNE-4 — 9 sub-waves: W6.0 CSS L4 root-runtime collapse, then remaining per-grammar runtime collapses (R3 PRUNE-4; C-1 part-B) | Conditional on W5 close | ≤2.0k C-1 part-B aggregate across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted) | ≤90 min per sub-wave (W6.0..W6.8); aggregate ≤810 min |
| W7 | Section 10 | PRUNE-5 — wire W8 policy + W9 union from SCAFFOLD to LOAD-BEARING (R3 PRUNE-5; C-4) | Conditional on W6 close (C-1 MUST precede C-4 per S-P0 §2.2) | ≤1.4k C-4 source/test LOC | ≤90 min |
| W8 | Section 11 | CSS L4 Re-Admit (R6; grammar-derived pipeline + production corpora + work-equivalent comparator) | Conditional on W7 close | ≤650 source/test LOC; rows named in wave plan | ≤90 min |
| W9 | Section 12 | JSON Direct + Typed Re-Admit (R7; under rebound R1 comparators) | Conditional on W1 close locally; globally blocked until PRUNE-1..PRUNE-5 close | ≤450 source/test LOC; rows named in wave plan | ≤90 min |
| W10 | Section 13 | JSON parse_only Distinct Path + Re-Admit (R8) | Conditional on W1 + W9 close locally; globally blocked until PRUNE-1..PRUNE-5 close | ≤650 source/test LOC; new generated_json parse_only path named separately | ≤90 min |
| W11 | Section 14 | Close And Alpha Feedback | Conditional on W0-W10 dispositions | 0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only | ≤90 min |

LOC budgets are conjunctive with the 90-minute cap and rerun ceilings.
They count hand-edited source, tests, gate/report/schema code, and
hand-written doc or result edits named by the row. Generated outputs
do not consume the source LOC budget, but every generated file must
be named, diff-audited, and included in the revert slice. A wave plan
that exceeds either its LOC budget or the 90-minute implementation /
redress cap must split before dispatch or return REVISE.

Total envelope across all candidates (per SYNTHESIS §3): C-1 2.8k-3.4k;
C-2 600-1.08k; C-3 1.2k-2.0k; C-4 800-1.4k; C-5 250-500. Aggregate
~5.65k-8.38k. Any wave exceeding its envelope by >20% escalates per
`[generated-size-budget]`.

Phase caps (per `SKINNY-TRIUMVIRATE.md §7`):

| Phase | Cap |
|---|---:|
| Research | 30 min per agent, max 6 agents |
| Plan | 30 min |
| CHALLENGE | 60-90 min when first-of-class, substrate-touching, primitive, or high-risk |
| Implementation/redress | 60 impl + 15 measure = 75 min; 90 min hard ceiling including source edits, generation, verification, RESULTS/REDRESS updates, and rollback |

If a planned implementation cannot fit the 90-min redress cap, the
plan must split before dispatch or return REVISE.

Rerun ceilings:

| Wave | Focused verification | Rerun ceiling |
|---|---|---|
| W0 | report/gate tests, malformed sidecar-evidence rejection, full-table schema validation incl. 4 SK-V14 columns | one gate refresh plus one confirm rerun if variance invalidates telemetry |
| W1 | strict-comparator tests, per-iter-equality oracle test, revert audit, full-table maintain | one gate refresh |
| W2 | `xtask regen-css` skinny runtime round-trip test (rm + regen + diff), generated-output diff audit, seven exact companion checks | one gate refresh |
| W3 | corpora-size validation (`du -sh ≥ 800 KB`), source-URL provenance check | no performance rerun |
| W4 | post-PRUNE-2 gate: 24 CSS L4 rows = 0 ADMITTED; 7 template directories deleted; CSS L4 generated_css_l4.rs round-trip clean | one gate refresh |
| W5 | trait-dispatch tests, generic-generator template tests, Lock-14 baseline grep (returns ZERO for grammar-named matches) | one gate refresh |
| W6.0..W6.8 | per-grammar runtime collapse test, regen check, per-grammar parser tests, Lock-14 grep | one gate refresh per sub-wave |
| W7 | CSP-shape consumer test, named pre-wave row hot-leaf attribution shift in samply trace, Lock-1 triad declaration | one gate refresh; second rerun requires REDRESS cost note |
| W8 | CSS L4 re-admit row tests, lightningcss equality, production-corpora parse tests, generated diff audit, full-table maintain | one full gate refresh; second rerun requires REDRESS cost note |
| W9 | JSON direct/typed re-admit row tests, plane-correct strict comparator parity, Track 1/2 independence, full-table maintain | one full gate refresh; second rerun requires REDRESS cost note |
| W10 | distinct parse_only path tests, Skipper-class equality, Track 1/2 independence, full-table maintain | one full gate refresh |
| W11 | close-honesty checklist and document reconciliation | no performance rerun |

Extra reruns beyond the ceiling are REDRESS cost evidence, not retry room.

### Section 2.1 — Generality And Lock 14 Gate

Every wave has this exit gate, with extra checks when generic crates
are edited (SK-V8 §2.1 verbatim + SK-V14 R3 PRUNE-3 generic-generator
extension):

- **Public API scan**: no new public grammar-named API appears in generic crates.
- **Grammar branch scan**: no generic branch selects behavior by grammar name, corpus name, object/array role, field name, string role, layout role, or any per-grammar identifier.
- **Primitive/table scan**: no generic primitive, SIMD table, or classifier embeds JSON/CSS structural policy unless it is generated byte-set data plus opaque class ordinals with scalar reference and same-wave consumer.
- **Role/fact boundary**: generic code may store and search generated structural class ordinals or opaque fact ids, but event-role, recovery, layout, record-boundary, indentation, and reused-punctuation meaning live only inside generated grammar modules keyed by parser state plus class/byte.
- **Template/provider boundary** (POST-W5): no per-grammar templates or providers remain. The single generic generator template consumes grammar source + workspace metadata; per-grammar deviations encode in grammar metadata + source, NOT in branching code in any other crate.
- **Non-JSON proof**: CSS L4, Sheets, and BBNF-self must compile, lower, cost, or run without JSON structural roles for any generic CostFacts, codegen, runtime, SIMD, or parser-template edit. Acceptable proof is a named no-op dry run, focused test, or unchanged-output audit.
- **Forward invariant** (POST-W5, permanent): any new grammar added under `workspace.metadata.bbnf.grammars.{name}` produces ZERO new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf, grammar}/src/` and ZERO new directories in `crates/core/src/runtime/`; the Lock 14 baseline gate rejects any commit that violates this.

Allowed grammar-specific surfaces are grammar inputs (`.bbnf` files),
generated grammar output (under `runtime/src/grammars/<name>/`, emitted
from the rostered generator using grammar source plus workspace
metadata), tests, and host/API schema facts. The audit must cover
REDRESS 36, 37, 38, 85, 86 residue clusters and renamed JSON / CSS
policy.

## Section 3 — W0 Baseline Profile And Telemetry Lock

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v14/research/` using the `wave-0-<topic>.md` naming pattern.
- `skinny/REDRESS.md` only if W0 rejects.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`,
`skinny/RESULTS.md`, `skinny/REDRESS.md`,
`restart/skinny/tranches/sk-v14/HANDOFF.md`,
`restart/skinny/tranches/sk-v14/SYNTHESIS.md §0 + §2`.

Entry gate:

- G-Omega is closed by the user (per `ORCHESTRATOR-PROMPT.md:204` — the only mandatory relinquish).
- S-P3 + T-P3 LOCK declared.
- `skinny/RESULTS.md` is the SK-V13 close baseline (pre-audit-revert).
- W0 plan names the `SK-V14-open` capture method and the no-behavior-change proof.
- S-P0 + S-P1 + S-P2 §3Z LOCK declared.

Tasks:

1. Capture the current report as `SK-V14-open`.
2. Add SK-V14 telemetry fields per Section 0.4: `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`, plus the Lock 1 v+1 triple (`substrate_target` / `retention_lifetime` / `policy_owner`).
3. Populate hot leaf, profile artifact, run id, host/build metadata, feature mask, sample cost, and `SK-V14-open` delta for every current main row.
4. Pre-populate the `audit_overlay_verdict` column for every row per the SYNTHESIS §0.2 + SYNTHESIS §1.2 audit-overlay disposition table (24 CSS L4 + 5 parse_only + 4-6 direct + 7-11 typed = AUDIT-FALSIFIED; the rest = AUDIT-PENDING until W1 reverts land).
5. Add sidecar freshness/source validation and reject any same-run sidecar claim until a structured sidecar manifest parser exists.
6. Make `xtask gate-json` reject unsupported outcomes, missing required SK-V14 fields, stale sidecar strict claims, strict admission failing Section 0.2, and rows missing `per_iter_equality=PASS` per-iter or `comparator_plane` matching `Output plane`.
7. Create the Lock 14 baseline allowlist; capture the current 8-provider mesh + 67-runtime-file Pattern H census as the pre-PRUNE baseline.
8. Wire `xtask gate-json` Lock-14-companion lint (per S-P0 §2.4 recommendation): REJECTS any new `// @generated by skinny bbnf-codegen` header in `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` unless the matching path appears in a recognized regen subcommand's emission roster.

Exit gate:

- All 51 JSON cells + 24 CSS L4 features (75 main rows) satisfy Section 0.4.
- Throughput cells stay within ±1.0% of `SK-V14-open`.
- Every JSON `parse_only` row reports substrate-guard non-admission (`S`) or a preserved hard-failure outcome (`L`); none ADMITTED.
- Every CSS L4 row carries `audit_overlay_verdict=AUDIT-FALSIFIED` with the validation-pack §reference; none ADMITTED.
- Missing sidecar values have explicit `sidecar_freshness=absent:<reason>`.
- Populated sidecar values are historical non-manifest planning signals with source/freshness coverage; W0 admits no sidecar same-run manifest.
- `xtask gate-json` rejects malformed sidecar evidence, any `sidecar-same-run` claim without a structured manifest, and any new fake-`@generated` header per the Lock-14-companion lint.
- No parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated parser output change lands.

Same-wave consumer: `xtask gate-json` consumes every emitted telemetry
field and rejects malformed/missing evidence + new fake-`@generated`
header introduction in the same W0 slice.

Pre-blocked routes: all behavior routes, all `skinny/` parser/codegen/
runtime changes, stale sidecars as anchors, row-close claims from
schema completion, P-1..P-7 patterns (SYNTHESIS §0.4), REDRESS watch-
list (§15), and any source edit not required for telemetry/gate/
report validation.

Revert protocol: revert the W0 implementation commits together,
restore the opening RESULTS schema, and record a W0 REDRESS rejection
naming the missing profiler, gate, or row.

Downstream effect: W0 rejection blocks W1-W11.

## Section 4 — W1 Comparator Rebind + Per-Iter Equality + PRUNE-1 (R1 + R2 + R3 PRUNE-1)

Owner paths:

- `skinny/crates/bbnf-bench/benches/json_parity.rs` (delete single-lane `sonic_rs_anchor` at lines 87-102 per S-P0 A2 F8; wire three plane-correct anchors)
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs` (lines 695-727 per-corpus typed bindings: promote from parity-assertion-only to anchor-row consumption)
- `skinny/crates/bbnf-bench/src/`
- `skinny/xtask/src/main.rs` + `skinny/xtask/src/gate.rs` (gate-json rejection rules for `per_iter_equality` empty / `comparator_plane` mismatch)
- `skinny/RESULTS.md` (PRUNE-1: revert W14.1-.5 parse_only admits + 4-6 direct admits + 7-11 typed admits)
- `skinny/ROLLING-SOTA-DELTA.md` (PRUNE-1: revert per-row, cite v2 §1-4 + v6 §1 per REDRESS)
- `skinny/REDRESS.md` (22 new row-keyed entries; one per reverted JSON admit row)

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (C-2 + C-5 part-A),
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-admit-mechanism.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v2-json-validation.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v6-comparator-integrity.md`,
`skinny/REDRESS.md` item 87 (CostFacts/comparator binding history).

Entry gate:

- W0 admitted.
- `SK-V14-open` telemetry exists for every current main row.
- Three plane-correct strict comparators identified per R1: `sonic_rs::Skipper` (parse_only); `sonic_rs::Deserialize` strict per corpus (direct); per-corpus typed struct deser via per-corpus binding (typed).

Tasks:

**R1 (comparator rebind):**

1. Delete the single-lane `sonic_rs_anchor` at `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102`.
2. Wire three plane-correct strict anchors: parse_only → `sonic_rs::Skipper` (structural-skip-only API); direct → `sonic_rs::from_slice::<TargetStruct>()` per corpus with strict mode; typed → per-corpus typed struct deser via existing `real_typed_struct.rs:695-727` bindings promoted from parity-assertion to anchor-row.
3. Populate the `comparator_plane` column per row.

**R2 (per-iter equality oracle):**

4. Wire equality verification inside the timing region per iter (not startup-only); current startup-only checksum parity fails the addendum's strict admit rule per ORCHESTRATOR-PROMPT.md:107-108.
5. Emit `per_iter_equality` column per iter; gate-json rejects rows whose equality column is empty.

**PRUNE-1 (revert audit-falsified JSON admits):**

6. Revert W14.1-.5 (5 parse_only) + the 4-6 direct + 7-11 typed audit-falsified admits in `skinny/RESULTS.md` + `skinny/ROLLING-SOTA-DELTA.md`. The wider 6+11 population per SYNTHESIS §0.2 numeric-divergence-reconciliation (NOT the narrower 4+7).
6a. Enumerated 22-row revert manifest by REDRESS item id (per CH3 V1 §2.REVISE-3 prescription; discharges P3-E §3 per-wave audit-trail by-number requirement):
   - **parse_only (5 items)**: REDRESS 154, 155, 156, 157, 158.
   - **direct (6 items)**: REDRESS 131, 132, 133, 134, 135 + 141. Items 131-135 are the 5 SK-V13-W14 admit rows; item 141 is the broader-ledger direct admit row.
   - **typed (11 items)**: REDRESS 143 + 145, 146, 147, 148, 149, 150, 151, 152, 153 + 160. Items 145-153 are 9 SK-V13-W14 typed admit rows; items 143 + 160 are 2 broader-ledger typed admit rows.
   Total = 5 + 6 + 11 = 22 rows; matches the 22 REDRESS entries committed at exit gate.
7. REDRESS per row cites the validation-pack §reference (`v2 §1-4` for parse_only; `v6 §1 + §3` for direct/typed misbindings).
8. Set `audit_overlay_verdict=AUDIT-FALSIFIED` for all 22 reverted rows.

Exit gate:

- Single-lane `sonic_rs_anchor` deleted (grep returns 0 hits in benches/).
- Three plane-correct strict anchors wired; `comparator_plane` populated per row.
- `per_iter_equality` PASS per iter inside the timing region; gate-json rejects empty.
- 22 JSON admit rows reverted in RESULTS + ROLLING-SOTA-DELTA; 22 REDRESS entries land.
- ROLLING-SOTA-DELTA shows JSON parse_only 0/17 + JSON direct 0/17 + JSON typed 0/17 post-revert.
- Full-table maintain: every non-target row stays within ±1.0% of `SK-V14-open`; no correctness or verdict downgrade on non-target rows.

Same-wave consumer: `xtask gate-json` consumes the rebound comparator
columns + per-iter equality column; the revert is its own consumer for
PRUNE-1.

Pre-blocked routes: behavior changes outside `bbnf-bench/`, harness-
only hardening as performance proof, treating PRUNE-1 revert as an
admit, comparator-as-performance claims, P-2 (sonic_rs::from_slice
mislabelled as strict), P-4 (gate-relabel as admit), REDRESS 119/120
LIFTED-only patterns (re-opening requires fresh SK-V14 evidence per
SYNTHESIS §5).

Revert protocol: revert comparator/oracle/PRUNE-1 changes together;
preserve audit-trail in research artefact; add REDRESS naming the
missing strict-comparator binding or oracle path.

Downstream effect: W1 rejection blocks W7 (PRUNE-5 needs comparator
integrity), W9 (JSON re-admit needs rebound comparators), W10 (parse_
only needs Skipper-class anchor). W2 + W3 + W4 + W5 + W6 may proceed
independently (CSS work does not depend on JSON comparator rebind).

## Section 5 — W2 regen-css xtask (R4)

Owner paths:

- `skinny/xtask/src/main.rs` (add `regen-css` subcommand)
- `skinny/xtask/src/regen_css.rs` (new file; grammar-agnostic xtask generator parametrised by grammar name — first instance of `regen-{grammar}` family)
- `skinny/xtask/src/regen.rs` (refactor: extract shared regen-{grammar} machinery; the xtask binary parametrises a grammar-neutral generator)
- `skinny/crates/runtime/src/grammars/css_l4_*/` (generated output destination)
- `crates/core/src/runtime/css_l4/` is excluded from W2 and owned by W6.0
  after W5.
- `skinny/RESULTS.md` (W2 row attribution)
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (C-3 part-A),
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-generator-truth.md`,
`grammar/css/l4/*.bbnf` (15 files; the source of truth).

Entry gate:

- W1 admitted.
- 15 `.bbnf` files at `grammar/css/l4/` present (14 of 15 currently orphan per S-P0 A4 NEW-3; only `stylesheet.bbnf` cited by totality `Cargo.toml:22`).
- W2 plan names the `regen-css` USAGE entry; the parametrised generator's input contract (grammar-name + grammar-source + workspace-metadata); the skinny runtime output destination; the seven exact `check-css-l4-*` companions. The plan explicitly states that root `crates/core/src/runtime/css_l4/` is W6.0 work.

Tasks:

1. Add `regen-css` subcommand to `skinny/xtask/src/main.rs:8` USAGE line.
2. Author `regen_css.rs` consuming the 15 `.bbnf` files at `/grammar/css/l4/` + workspace metadata; emit CSS L4 runtime modules under `skinny/crates/runtime/src/grammars/css_l4_*/` only.
3. The xtask binary parametrises a grammar-neutral generator per SYNTHESIS §0.3 R4 — `regen-css` is the first instance of the `regen-{grammar}` family. The generator's input contract is grammar-name + grammar-source + workspace-metadata; the output is byte-deterministic typed Rust under `runtime/src/grammars/<name>/`.
4. Add the exact companion invocations per S-P0 §2.4(1): `cargo xtask check-css-l4-at-rules-and-media`, `cargo xtask check-css-l4-declaration-values`, `cargo xtask check-css-l4-declaration-values-extended`, `cargo xtask check-css-l4-nested-layout`, `cargo xtask check-css-l4-stylesheet-selectors`, `cargo xtask check-css-l4-vendor-and-custom-atrules`, and `cargo xtask check-css-l4-visual-functions`. Each companion reads the emitted bytes, re-runs `regen-css` for its covered profile, and diffs.

Exit gate:

- `cargo xtask regen-css` skinny-side round-trip clean:
  `rm -rf skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask regen-css && git diff --exit-code -- skinny/crates/runtime/src/grammars`.
- `find skinny/xtask/src -name '*.rs' | xargs grep -l regen-css | wc -l > 0`.
- All seven exact `check-css-l4-*` companions exist and pass.
- Bypass-header detector empty for W2-owned output: `git grep -l '@generated by skinny bbnf-codegen' -- skinny/crates/runtime crates/core/src/runtime` traces every skinny runtime match to a registered W2 xtask emission; root runtime matches fail this gate unless W6.0 owns them.
- Lock 14 baseline gate: zero grammar-named branches in xtask itself; the `regen_css.rs` module name is the only css-named identifier in the xtask binary (per the first-instance discipline).
- Full-table maintain: ±1.0% on JSON rows.

Same-wave consumer: `cargo xtask regen-css` itself + the seven exact
`check-css-l4-*` CI invocations consume the emitted skinny CSS L4 trees in
W2's test suite.

Pre-blocked routes: hand-patching generated output (per `[clean-regen-
discipline]`); shipping `regen-css` without a parametrised
`regen-{grammar}` family contract (P-6 recurrence vector); CSS L4
SOTA claim from W2 alone (xtask correctness is gate evidence, not
admit evidence); touching or claiming closure over
`crates/core/src/runtime/css_l4/`; fake `@generated` header (P-1).

Revert protocol: revert xtask/regen_css.rs changes + delete emitted skinny
output trees; add REDRESS naming the failing round-trip case or missing
grammar-derived emission path.

Downstream effect: W2 rejection blocks W3 and all later waves by hard entry
gate. W4 needs W2 + W3; W8 needs the grammar-derived pipeline; W9/W10 remain
globally blocked by PRUNE-before-new-admit until PRUNE-1..PRUNE-5 close.

## Section 6 — W3 Production CSS Corpora (R5)

Owner paths:

- `skinny/corpora/css-l4-sk-v14/` (new directory; ~960 KB target per `SYNTHESIS.md:97` R5 + `ORCHESTRATOR-PROMPT.md:133-135`)
- `skinny/corpora/css-l4-sk-v14/manifest.md` (source URLs + freshness stamps)
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs` (corpus loader pointing at new directory)
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (C-3 part-B),
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-css-measurement.md`.

Entry gate:

- W2 admitted under the amended skinny-only `regen-css` gate.
- W3 plan names the four production sources (Bootstrap + Tailwind + Material + Animate) with stable source URLs + commit/version pins; ~960 KB target binding.

Tasks:

1. Stage `skinny/corpora/css-l4-sk-v14/` containing Bootstrap + Tailwind + Material + Animate at ~960 KB total per ORCHESTRATOR-PROMPT.md:133-135.
2. Author `manifest.md` with source URLs, commit/version pins, file checksums, freshness stamps.
3. Wire `skinny/crates/bbnf-bench/src/css_l4_corpus.rs` to load from the new directory.
4. Capture per-file size + checksum + freshness telemetry in W3's wave research artefact.

Exit gate:

- `du -sh skinny/corpora/css-l4-sk-v14 ≥ 800 KB` (per `SYNTHESIS.md:128` P-3: rows measured on <1 KB fixtures cannot admit; the corpora pin is ≥800 KB working set).
- `manifest.md` cites four production sources with source URLs + commit/version pins.
- Per-corpus checksum + size telemetry captured.
- Loader resolves all four corpora at runtime.
- Full-table maintain: ±1.0% on JSON rows (no behavior change).

Same-wave consumer: the loader at `bbnf-bench/src/css_l4_corpus.rs`
consumes the new corpora in W3's test suite + parity assertion.

Pre-blocked routes: tiny embedded fixtures (`SYNTHESIS.md:127-129` P-3
recurrence); corpus inflation past ~960 KB without justification;
P-3 (Criterion-overhead Mbps on <1 KB fixtures); CANONICAL_FIXTURE /
CAPTURED_W2_INPUT byte-equality short-circuit at the corpus loader
(per S-P0 A4 NEW-2).

Revert protocol: delete `skinny/corpora/css-l4-sk-v14/` + revert
loader changes; add REDRESS naming the unattainable corpus target.

Downstream effect: W3 rejection blocks W8 (CSS L4 re-admit needs
production corpora), and blocks W4/W5/W6/W7 by the PRUNE chain. W9/W10 remain
globally blocked until PRUNE-1..PRUNE-5 close.

## Section 7 — W4 PRUNE-2 (Delete 7 CSS Templates + Revert 24 CSS L4 Admits)

Owner paths:

- `skinny/crates/codegen/src/css_l4_*_templates/` (delete 7 template directories per `SYNTHESIS-AUDIT-OVERFIT.md §1.2` NEW-1 + NEW-2)
- `skinny/crates/codegen/src/css_l4_*_provider.rs` (delete 7 provider modules)
- `skinny/crates/runtime/src/grammars/css_l4_*/` (delete 7 runtime twins; will be re-emitted by W2's regen-css)
- `skinny/RESULTS.md` (revert 24 CSS L4 admitted rows)
- `skinny/ROLLING-SOTA-DELTA.md` (revert 24 CSS L4 + W2/W3/W4/W10.1-3/W1b admits)
- `skinny/REDRESS.md` (24 new row-keyed entries; one per reverted CSS L4 admit row)

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (C-5 part-B),
`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-css-measurement.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-generator-truth.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md`.

Entry gate:

- W2 admitted under the amended skinny-only R4 `regen-css` gate; gates W4 per S-P0 §2.1 sequencing constraint.
- W3 admitted (production corpora exist; sets up post-PRUNE re-admit path).
- W4 plan names exact files to delete (7 template directories + 7 provider modules + 7 runtime twins per S-P0 A4 NEW-1 + NEW-2 + finding 1-7) + the 24 CSS L4 row keys to revert + the validation-pack §reference per row.

Tasks:

1. Delete the 7 CSS L4 hand-written template directories under `skinny/crates/codegen/src/css_l4_*_templates/`.
2. Delete the 7 CSS L4 provider modules under `skinny/crates/codegen/src/css_l4_*_provider.rs`.
3. Delete the 7 CSS L4 runtime twins under `skinny/crates/runtime/src/grammars/css_l4_*/` (these are regenerated by `cargo xtask regen-css` immediately after).
4. Run `cargo xtask regen-css` to re-emit byte-deterministic generated output from the 15 `.bbnf` files via W2's pipeline.
5. Revert the 24 CSS L4 ADMITTED rows in `RESULTS.md` + `ROLLING-SOTA-DELTA.md`.
6. REDRESS per row cites `v1 §1-6` (CSS L4 validation pack — fake `@generated` header on hand-written templates; no regen-css xtask; CSS scanners as fixture lookups).
7. Set `audit_overlay_verdict=AUDIT-FALSIFIED` for all 24 reverted rows.

Exit gate:

- `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/css_l4_*_templates/ | wc -l == 0` (7 template directories deleted).
- `find skinny/crates/codegen/src -name 'css_l4_*_provider.rs' | wc -l == 0` (7 provider modules deleted).
- Post-PRUNE-2: re-running `cargo xtask regen-css` produces byte-deterministic output identical to the post-W2 emission; `git diff` empty.
- `ROLLING-SOTA-DELTA.md` shows CSS L4 0/24 + JSON parse_only 0/17 (post-W1) + JSON direct 0/17 (post-W1) + JSON typed 0/17 (post-W1) per SYNTHESIS §1.3 honest baseline.
- 24 REDRESS entries land; each cites `v1 §1-6` + names the deleted template + the regenerated replacement.
- Full-table maintain: ±1.0% on JSON rows.

Same-wave consumer: `cargo xtask regen-css` re-emission of the
deleted runtime twins is the consumer; the W2-emitted generated trees
replace the hand-written ones in the same commit.

Pre-blocked routes: deleting without re-emission (substrate-then-
substrate; bricks 24 CSS rows permanently); preserving the fake
`@generated` header on the regenerated output (P-1 recurrence vector;
the W0 Lock-14-companion lint blocks); admitting any CSS L4 row in
this wave (CSS L4 re-admit is W8 work).

Revert protocol: revert the template + provider + runtime-twin
deletions + RESULTS / DELTA reverts as one slice; add REDRESS naming
the failing emission path.

Downstream effect: W4 rejection blocks W8 (CSS L4 re-admit needs the
hand-written templates GONE so the grammar-derived pipeline is the
only emission path), and blocks W5/W6/W7 by the PRUNE chain. W9/W10 remain
globally blocked until PRUNE-1..PRUNE-5 close.

## Section 8 — W5 PRUNE-3 (Lock-14 Refactor: Trait Dispatch + Grammar-Agnostic Generator)

Owner paths:

- `skinny/crates/passes/src/lib.rs` (replace `RuntimeProvider` enum with trait-based dispatch)
- `skinny/crates/codegen/src/lib.rs:167-209` (replace 8 per-grammar `RuntimeProvider::*` match arms with trait-dispatch dispatcher)
- `skinny/crates/codegen/src/grammar_provider.rs` (new; trait + per-grammar facade backing)
- `skinny/crates/codegen/src/{json,css_l4_*,bbnf,google_sheets,csv,ebnf,math,bnf,css_pretty}_provider.rs` (replace 8 hand-written per-grammar provider modules with ONE generic generator template consuming grammar source + workspace metadata)
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` (extend Lock 14 baseline gate to include the post-PRUNE-3 forward invariant)
- `skinny/RESULTS.md` (W5 row attribution)
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (C-1 part-A),
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v3-lock14-deep-scan.md`.

Entry gate:

- W4 admitted (CSS templates deleted; the post-PRUNE-3 generic generator can be exercised on the grammar-derived CSS L4 path immediately).
- W5 plan names the trait surface signature, the 8 per-grammar modules slated for deletion, the generic-generator-template input contract (grammar source + workspace metadata → typed Rust output), the Lock 14 baseline gate's post-W5 forward invariant.
- W5 plan does NOT touch `runtime/` (W6's scope).

Tasks:

1. Replace `RuntimeProvider` enum at `skinny/crates/passes/src/lib.rs` with trait-based dispatch (`trait GrammarProvider` consuming grammar source + workspace metadata).
2. Replace the 8 per-grammar `RuntimeProvider::Json` / `RuntimeProvider::CssL4DeclarationValues` / etc. match arms at `skinny/crates/codegen/src/lib.rs:167-209` with trait-dispatch dispatcher.
3. Collapse the 8 per-grammar provider modules under `skinny/crates/codegen/src/` into ONE grammar-agnostic generator template at `skinny/crates/codegen/src/grammar_provider.rs` consuming (grammar source + workspace metadata) per `LOCKS.md:220` Lock 14 binding.
4. Extend `skinny/crates/bbnf-bench/src/lock14_baseline.rs` to enforce the post-W5 forward invariant: any new grammar produces ZERO new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf, grammar}/src/` AND ZERO new directories in `crates/core/src/runtime/`.
5. Migrate W2's `regen_css.rs` to consume the new `GrammarProvider` trait dispatch — `regen-{grammar}` family becomes the production binding of the new dispatch.

Exit gate:

- `find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar' | wc -l == 0` (per `SYNTHESIS.md:271` C-1 falsifiability gate).
- `find skinny/crates/codegen/src -name '*_provider.rs' \! -name 'grammar_provider.rs' | wc -l == 0` (8 per-grammar providers collapsed).
- Lock 14 baseline gate passes: grammar-name scan returns ZERO matches in generic crates per `LOCKS.md:220-238`.
- Generic-crate Lock 14 grep returns ZERO matches per `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' crates/`.
- Non-JSON proof per §2.1: CSS L4 + Sheets + BBNF-self compile via the new trait dispatch without grammar-name branches.
- `cargo xtask regen-css` continues to produce byte-deterministic output via the new dispatch path.
- Full-table maintain: ±1.0% on all rows.

Same-wave consumer: `cargo xtask regen-css` (W2-emitted) becomes the
production consumer of the new `GrammarProvider` trait dispatch in
W5's commit.

Pre-blocked routes: grammar-name branches in generic crates (Lock 14
v+1 binding); per-grammar provider modules in generic codegen (P-6
recurrence); JSON policy in generic crates; renamed JSON helpers
(REDRESS 36-38, 85-86 watch-list); preserving the 8 provider modules
"for compatibility" (per `[no-backward-compat]`).

Revert protocol: revert trait-dispatch + provider-collapse changes as
one slice; restore the 8 per-grammar provider modules; add REDRESS
naming the failing dispatch path or missing input-contract field.

Downstream effect: W5 rejection blocks W6 (PRUNE-4 needs the generic
generator template to collapse the 67 per-grammar runtime files onto)
AND W7 (PRUNE-5 needs the generic dispatcher to wire W8 + W9 against
per S-P0 §2.2 sequencing constraint). W8 + W9 + W10 may proceed
independently of W5.

## Section 9 — W6 PRUNE-4 (9 Sub-Waves: Per-Grammar Runtime Collapse)

Owner paths:

- `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/` (9 per-grammar directories; collapse onto template emitted output)
- `skinny/crates/runtime/src/grammars/{json, css_l4_*, sheets_witness}/` (skinny-side mirror; partially collapsed already)
- `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/parse_with.rs` (rewrite `LegacyPath` shim per S-P0 A6 NEW-HIGH-1; folded as PRUNE-4 sub-task)
- `crates/core/src/runtime/builder_template.rs` + `crates/core/src/runtime/arena_template.rs` (rewrite Pattern H opt-out enshrinement per S-P0 A6 NEW-HIGH-2; either become genuine codegen output or rewrite with deletion plan)
- `crates/core/src/runtime/google_sheets/document/canonical.rs:13-17` (remove pre-restart-API documentary carry per S-P0 A6 NEW-MED)
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (C-1 part-B),
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1.3 + §3.3` (Pattern H 64 → 67; PRUNE-4 = 9 sub-waves).

Entry gate:

- W5 admitted (generic generator template exists; provides the collapse target).
- W6 plan enumerates 9 sub-waves W6.0..W6.8 by grammar name: `css_l4, math, csv, bnf, ebnf, css_pretty, google_sheets, bbnf, json`. PRUNE-4 sub-wave count is 9 NOT 8 per S-P0 §2.3 (`css_pretty` is the +1 over the SK-V13 baseline's 8). Pass Omega V3 W2R assigns CSS L4 root-runtime collapse to W6.0 after W5.
- W6 plan names the per-grammar runtime file inventory: bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7 (total = 67 files per `SYNTHESIS-AUDIT-OVERFIT.md §1.3`).

### W6 sub-wave order (per substrate-before-consumer + guard-rows-before-risk-rows discipline):

> **Cap footnote (per §2 manifest restated for dispatch-time clarity):** Each W6.N sub-wave carries the ≤90-min implementation/redress cap; the W6 aggregate cumulative cap across W6.0..W6.8 is ≤810 min per `SPEC.md:243`. Any sub-wave or aggregate overflow returns REVISE per `[generated-size-budget]`.

| Sub-wave | Grammar | File count | Risk | Notes |
|---|---|---:|---|---|
| W6.0 | `css_l4` | 7 | MED-HIGH | W2R-owned root-runtime collapse; proves W5 can emit or collapse `crates/core/src/runtime/css_l4/` without W2 touching that tree |
| W6.1 | `math` | 7 | LOW | smallest non-trivial grammar after CSS L4 root handoff; proves the general collapse contract beyond W2R |
| W6.2 | `csv` | 7 | LOW | second-smallest; minimal cross-dependency |
| W6.3 | `bnf` | 7 | LOW-MED | grammar peer to ebnf; subset of bbnf |
| W6.4 | `ebnf` | 7 | LOW-MED | grammar peer to bnf; superset coverage |
| W6.5 | `css_pretty` | 7 | MED | the +1 over SK-V13 baseline; co-derived with CSS L4 but independent of W2's skinny runtime tree |
| W6.6 | `google_sheets` | 10 | HIGH | largest per-grammar (10 files); pre-restart-API carry at `document/canonical.rs:13-17` (S-P0 A6 NEW-MED) removed in this sub-wave |
| W6.7 | `bbnf` | 8 | HIGH | self-hosting grammar; LegacyPath shim removal (S-P0 A6 NEW-HIGH-1) |
| W6.8 | `json` | 7 | VERY HIGH | highest-throughput hot-path; collapsed last so the W6.0-.7 collapses establish the regression budget; also: builder_template.rs + arena_template.rs Pattern H opt-out enshrinement rewrite (S-P0 A6 NEW-HIGH-2) happens at this sub-wave |

Each sub-wave is ONE triumvirate (research + plan + redress); 9 sub-
waves × ~3 commits = ~27 commits within W6.

### Per-sub-wave tasks (W6.N for grammar `<G>`):

1. Refactor the per-grammar runtime files under `crates/core/src/runtime/<G>/` into emitted output via the W5 grammar-agnostic generator template consuming `<G>.bbnf` + workspace metadata.
2. Remove the `LegacyPath` / `LegacySegment` rename shim at `<G>/parse_with.rs` (S-P0 A6 NEW-HIGH-1 fold).
3. Validate the regen check: `cargo xtask regen-<G>` round-trip clean.
4. Run the Lock 14 baseline gate: `find crates/core/src/runtime/<G> -name '*.rs' | wc -l == 0` post-collapse (all generated).
5. Run the per-grammar parser test suite; full-table maintain ±1.0%.

W6.0 additionally runs the CSS L4 root-runtime destructive gate:
`rm -rf crates/core/src/runtime/css_l4 && cargo xtask regen-css && git diff --exit-code -- crates/core/src/runtime/css_l4`.

### Per-sub-wave exit gate (W6.N):

- `find crates/core/src/runtime/<G> -mindepth 1 -maxdepth 1 -type d | wc -l == 0` (per-grammar dir collapsed).
- Generated output regen check returns empty diff.
- Per-grammar parser tests pass.
- Lock 14 baseline gate passes for grammar `<G>` scope.
- Full-table maintain: ±1.0% on all rows.

### W6 aggregate exit gate:

- `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l == 0` (per `SYNTHESIS.md:271` C-1 falsifiability gate); 9 per-grammar dirs collapsed.
- 67 hand-written per-grammar runtime files become 0 hand-written + 67 generated.
- `crates/core/src/runtime/builder_template.rs` + `crates/core/src/runtime/arena_template.rs` Pattern H opt-out enshrinement rewritten (S-P0 A6 NEW-HIGH-2 discharged).
- `crates/core/src/runtime/google_sheets/document/canonical.rs:13-17` pre-restart-API carry removed (S-P0 A6 NEW-MED discharged).
- 4 `LegacyPath` shim files removed (S-P0 A6 NEW-HIGH-1 discharged).
- Forward invariant active: any new grammar produces ZERO new `.rs` files in generic crates AND ZERO new directories in `crates/core/src/runtime/`.

Same-wave consumer: per-sub-wave: the per-grammar parser test suite +
the per-grammar bench rows consume the newly-emitted runtime in the
same commit.

Pre-blocked routes: hand-patching emitted output (per `[clean-regen-
discipline]`); orphaning the `css_pretty` sub-wave with an 8-sub-wave
plan (S-P0 §2.3 binding); preserving Pattern H opt-out language in
builder_template.rs / arena_template.rs as "design-of-record"
(S-P0 A6 NEW-HIGH-2 substrate-doc opt-out enshrinement); preserving
the `LegacyPath` rename shim as a "bridge between transitional
representations"; per-grammar feature flags; grammar-name branches.

Revert protocol: per-sub-wave: revert the collapse + restore the
hand-written `<G>/` files; record per-sub-wave REDRESS naming the
failing emission contract or test gap. A W6 aggregate revert undoes
all 9 sub-waves; sub-wave-granular revert is the default.

Downstream effect: W6 rejection blocks W7 (PRUNE-5 needs the collapsed
runtime to consume W8 policy + W9 union shapes without re-deepening
the Lock 14 violation per S-P0 §2.2 sequencing constraint). W8 + W9 +
W10 remain globally blocked until PRUNE-1..PRUNE-5 close. No new-admit wave
may cite W6 output before the relevant PRUNE chain closes.

## Section 10 — W7 PRUNE-5 (Wire W8 + W9 from SCAFFOLD to LOAD-BEARING)

Owner paths:

- `skinny/crates/passes/src/` (wire `per_grammar_policy` + `same_substrate_union` into compile + lower paths; currently gate-layer-only per S-P0 A5 NEW-MED)
- `skinny/crates/codegen/src/` (consume CSP-selected shape; load-bearing W8 policy + W9 union)
- `skinny/crates/runtime/src/` (runtime honors CSP-selected shape per ORCHESTRATOR-PROMPT.md:124-126)
- `skinny/crates/bbnf-bench/src/bin/gate.rs` + `skinny/crates/bbnf-bench/src/lock14_baseline.rs` + `skinny/crates/bbnf-bench/src/report.rs` (extend gate enforcement; per S-P0 A5 NEW-MED these are the only 3 current consumers)
- `skinny/RESULTS.md` (W7 named pre-wave row + post-wave hot-leaf attribution)
- `skinny/REDRESS.md` (CSP-shape choice cited per row; Lock-1 triad declared per shape)

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (C-4),
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-decision-engine.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v4-decision-engine-trace.md`,
`restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (substrate-union YES preservation per S-P2 V3 §4.5).

Entry gate:

- W5 + W6 admitted (C-1 PRUNE-3 + PRUNE-4 BEFORE C-4 per S-P0 §2.2 sequencing constraint; the generic dispatcher + per-grammar runtime collapse must exist before W7 wires W8/W9 against the post-collapse mesh).
- W7 plan names ONE pre-wave row whose hot-leaf attribution will shift in samply trace (per `SYNTHESIS.md:274` C-4 falsifiability gate: `json/numbers/direct_to_struct/main` pre-wave hot leaf `parse_value_at` → post-wave hot leaf the W11.1 number-specialised symbol per the samply trace).
- W7 plan declares per-shape Lock-1 triad (`substrate_target` ∈ `local_temp_only`/`existing_tape`/`direct_sink`/`admitted_fact_output`; `retention_lifetime` ∈ `local_loop`/`generated_function`/`output_row`; `policy_owner` ∈ `generated_grammar`/`caller_data`/`none`) per `LOCKS.md:76-82`.
- W7 plan exercises the shape consumer across at least two grammar families per SYNTHESIS §4 verbatim — one-grammar runtime divergence is wave evidence, not admit evidence.

Tasks:

1. Wire `per_grammar_policy` (W8 SCAFFOLD per ORCHESTRATOR-PROMPT.md:124) into compile + lower + runtime paths beyond the current 3 gate-layer-only consumers.
2. Wire `same_substrate_union` (W9 SCAFFOLD per ORCHESTRATOR-PROMPT.md:124-126) into compile + lower + runtime paths. The W7 `same_substrate_union` module is an ENFORCEMENT-LAYER pass that proves substrate-union compliance (every shape consumer reuses the existing `Tape` substrate — zero new retained surface); it is NOT the SK-V9 W3 retired retained-class-column-union data structure (PERMANENT-PRE-BLOCK per REDRESS 96/97/98). The naming proximity is incidental; the W7 module is a gate-pass over the W8/W9 emissions, not a runtime substrate.
3. Make the CSP solver's selected shape produce measurable runtime divergence on the named pre-wave row; the resolver fails closed on stale cost > 30% / candidate expressions / order-dependent rewrites > 10% variance per `LOCKS.md:184-188` + ORCHESTRATOR-PROMPT.md:124-126.
4. Hardcoded P1-P8 cascade must fail closed for JSON / CSS / Sheets / BBNF-self after the resolver lands; silent fallback to the old cascade is not admission evidence (per SYNTHESIS §4).
5. The shape consumer in `skinny/crates/codegen/src/lib.rs` MUST dispatch on the CSP-emitted `BackendShape` enum alone — no `match grammar { Json => ..., CssL4 => ... }` arm may appear in the dispatch path per SYNTHESIS §4.

Exit gate:

- Named pre-wave row hot-leaf attribution shifts in samply trace (W7 names exact symbol path of pre-wave vs post-wave hot leaf; samply diff is gate evidence).
- Per-shape Lock-1 triad declared in REDRESS for every shape emitted; `xtask gate-json` rejects any row whose REDRESS lacks the triple.
- CSP-shape consumer exercised on at least two grammar families.
- Hardcoded P1-P8 cascade fails closed for JSON / CSS / Sheets / BBNF-self post-W7.
- The shape consumer dispatches on `BackendShape` alone — Lock 14 grep `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>' skinny/crates/codegen/src/lib.rs` returns ZERO matches.
- Full-table maintain: ±1.0% on non-target rows.

Same-wave consumer: the runtime divergence on the named pre-wave row
is the consumer; the samply trace + REDRESS hot-leaf attribution
shift is the gate evidence.

Pre-blocked routes:
- Silent fallback to the old P1-P8 cascade (P-5 SCAFFOLD-as-load-bearing recurrence).
- Grammar-name branches in the shape dispatcher (P-6 recurrence).
- Telemetry-only row counts as W7 admit evidence.
- One-grammar runtime divergence as admit evidence (SYNTHESIS §4 binding — two-grammar minimum).
- **REDRESS 96-98 PERMANENT-PRE-BLOCK** — full class-column vectors, streaming structural cursors, class-lane-only replays, parser-owned sidecars, UnionTape-style retained structures per Lock 1 v+1 substrate-ceiling history. The W7 `same_substrate_union` ENFORCEMENT module is NOT a re-opening of REDRESS 96/97/98; the SK-V9 W3 retired retained-class-column-union DATA STRUCTURE remains permanently blocked. Naming proximity to the W7 enforcement-pass module name is incidental.

Revert protocol: revert PRUNE-5 wire-up changes + restore the
SCAFFOLD-only state of W8 + W9; add REDRESS naming the failing CSP-
shape consumer or missing Lock-1 triad slot.

Downstream effect: W7 rejection blocks W8 / W9 / W10 admit claims
that cite W8/W9 runtime consumption (per P-5 pre-block: no row admit
may cite W8 / W9 as evidence until the runtime consumer is measured).
W8 / W9 / W10 are new-admit waves and remain globally blocked until
PRUNE-1..PRUNE-5 close.

## Section 11 — W8 CSS L4 Re-Admit (R6)

Owner paths:

- W8 is not pre-authorized beyond its plan. The W8 plan must name exact files before implementation and start from the post-W7 codegen + runtime trees. Expected owner families:
- `skinny/crates/codegen/` (grammar-derived emission for CSS L4 declaration-values, selectors, at-rules, nested layout, vendor & custom at-rules, stylesheet, etc.)
- `skinny/crates/runtime/src/grammars/css_l4_*/` (generated output)
- `skinny/crates/bbnf-bench/src/css_l4_bench.rs` (re-admit bench wiring)
- generated CSS L4 output named by the plan
- `skinny/RESULTS.md` (W8 row attribution)
- `skinny/ROLLING-SOTA-DELTA.md` (W8 admits per row)
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md`,
`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` (CSS L4 SOTA binding).

Entry gate:

- W2 + W3 + W4 + W5 + W6 + W7 admitted (the full PRUNE chain + CSS L4 infrastructure).
- W8 plan names exact CSS L4 feature rows + lightningcss strict-mode comparator + cssparser oracle + production-corpus parse path + Track 1 generated path + Track 2 oracle path + rollback boundaries.
- W8 plan does NOT carry Stage-0 F-V2-P1ABC-RERECORD: Stage-0 binds UNCONDITIONALLY to W10 (per p3a:180 — first wave admitting any of {P3-A C1 long-string-body SIMD scan, C3 digit_block_simd_accumulate, C7 …} — resolves to W10 parse_only distinct path per R8). W8 admits CSS L4 grammar-derived rows; CSS L4 does NOT admit C1/C3/C7, therefore W8 inherits no Stage-0 obligation. Stage-0 inheritance chain (5-step): (1) Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim; (2) C1 = long-string-body SIMD scan primitive (queued for S-P3 same-wave admission per S-P2 V3 §6.2); (3) W10 is first wave consuming C1 via the parse_only distinct path per R8 (the parse_only-distinct-path admission is the first dispatch-envelope behavioral edit); (4) therefore W10 carries Stage-0 unconditionally; (5) W8 + W9 do NOT admit C1/C3/C7 → no Stage-0 obligation there.

Tasks:

1. Re-attempt each CSS L4 feature row via the grammar-derived pipeline (W2 + W6 emitted output) against the production corpora (W3-staged ~960 KB).
2. Work-equivalent comparator: lightningcss full-parse + cssparser full-parse — no fact-stream vs full-AST asymmetry per ORCHESTRATOR-PROMPT.md:139-140.
3. Per row: capture per-iter equality oracle (W1's R2 binding), populate audit-overlay-verdict transition AUDIT-FALSIFIED → AUDIT-SUSTAINED upon ADMIT.
4. Honor the round-trip-rule trigger per SYNTHESIS §0.4 P-1: any second-in-tranche reopen of `nested_layout` requires user re-pin with intrinsic-block evidence; any CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥50× inherits the same trigger.
5. NO F-V2-P1ABC-RERECORD Stage-0 work in W8: Stage-0 binds unconditionally to W10 per §11 entry-gate inheritance chain; CSS L4 admits do not exercise C1/C3/C7 and therefore inherit no Stage-0 obligation.

Exit gate:

- At least one CSS L4 feature ADMITs > strict-vs-strict against lightningcss full-parse on production corpora (≥800 KB) on same plane / same equality.
- Per-iter equality oracle PASS for every iter on every admitted row.
- `audit_overlay_verdict` column shifts to AUDIT-SUSTAINED for every ADMIT row.
- Lock 14 + non-JSON proof pass; no grammar-name branches reintroduced.
- Full-table maintain: ±1.0% on non-target rows; JSON cells stable; no GO row regression.
- F-V2-P1ABC-RERECORD Stage-0 is NOT a W8 obligation (binds unconditionally to W10 per §11 entry-gate inheritance chain); CSS L4 admits do not exercise C1/C3/C7.

Same-wave consumer: every admitted CSS L4 row has a generated Track 1
grammar-derived consumer + an independent lightningcss/cssparser oracle
in the same wave.

Pre-blocked routes: fake `@generated` header on hand-written templates
(P-1 recurrence); CANONICAL_FIXTURE / CAPTURED_W2_INPUT byte-equality
short-circuit at corpus loader (P-3 + S-P0 A4 NEW-2); fact-stream vs
full-AST asymmetry; re-admitting any row at a different plane than
the SK-V13 audit pack falsified it; closing a CSS L4 row through
REDRESS 119/120 history without fresh SK-V14 evidence; admitting via
tiny-fixture <1 KB corpora (P-3); single-quartet Unicode classifier
+ StringBlock16 tiny probe pre-blocks (REDRESS 82, 83).

Revert protocol: revert row admits + REDRESS / RESULTS changes + bench
wiring as one slice; add REDRESS naming the failed comparator parity
or missing production-corpus path.

Downstream effect: W8 disposition informs W11 close ceremony.

## Section 12 — W9 JSON Direct + Typed Re-Admit (R7)

Owner paths:

- W9 is not pre-authorized beyond its plan. Expected owner families:
- `skinny/crates/bbnf-bench/benches/json_parity.rs` (re-admit row wiring against rebound R1 comparators)
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs` (typed re-admit per corpus)
- `skinny/crates/codegen/` (only if re-admit requires generated path changes per W9 plan)
- `skinny/RESULTS.md` (W9 row attribution)
- `skinny/ROLLING-SOTA-DELTA.md` (W9 admits per row)
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v2-json-validation.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v6-comparator-integrity.md`.

Entry gate:

- W1 admitted (R1 + R2 strict-comparator binding + per-iter equality oracle).
- W9 plan selects 1-N JSON direct + typed rows for re-admit; names per-row plane-correct comparator instance + per-row Mbps threshold + Track 1 + Track 2 paths.
- W9 plan does NOT carry Stage-0 F-V2-P1ABC-RERECORD: Stage-0 binds UNCONDITIONALLY to W10 (per p3a:180 — first wave admitting any of {C1, C3, C7}). W9 admits JSON direct + typed rows under the rebound R1 comparators; the JSON direct + typed planes do NOT admit C1/C3/C7 (C1 = long-string-body SIMD scan; the direct + typed planes consume full-tape parse, not the dispatch-envelope parse_only scan). Stage-0 inheritance chain (5-step): (1) Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim; (2) C1 = long-string-body SIMD scan primitive (queued for S-P3 same-wave admission); (3) W10 is first wave consuming C1 via parse_only distinct path per R8; (4) therefore W10 carries Stage-0 unconditionally; (5) W8 + W9 do NOT admit C1/C3/C7 → no Stage-0 obligation there.

Tasks:

1. Re-baseline every JSON direct + typed row against the rebound strict comparators per ORCHESTRATOR-PROMPT.md:143-145.
2. Cells previously HOLDING under the misbound comparator either hold again under the right comparator (re-ADMIT) or are reverted (REDRESS + AUDIT-FALSIFIED carried).
3. Per row: `comparator_plane=direct` or `comparator_plane=typed`; `per_iter_equality=PASS`; `audit_overlay_verdict` transition AUDIT-FALSIFIED → AUDIT-SUSTAINED upon ADMIT.
4. Honor pre-block REDRESS 49-55, 60-72, 80, 82-84, 88, 89: no re-opening these without fresh material differential evidence.
5. NO F-V2-P1ABC-RERECORD Stage-0 work in W9: Stage-0 binds unconditionally to W10 per §12 entry-gate inheritance chain; JSON direct + typed planes do not exercise C1/C3/C7 (those consume full-tape parse, not the dispatch-envelope parse_only scan).

Exit gate:

- Every selected JSON direct + typed row meets Track 1 + Track 2 floors from the W9 plan.
- Correctness parity, plane-correct strict comparator (R1 binding), and per-iter equality (R2 binding) present for each selected row.
- Track 2 does not call generated SinkOnly, generated typed helpers, generated Track 1, or a shared benchmark-private parser; `track2_entry_point` column verifies symbol-path divergence.
- All non-target rows are no worse than -2.0% Track 1 + Track 2 vs `SK-V14-open`.
- Lock 14 + non-JSON proof pass if generic code changed.
- F-V2-P1ABC-RERECORD Stage-0 is NOT a W9 obligation (binds unconditionally to W10 per §12 entry-gate inheritance chain); JSON direct + typed planes do not exercise C1/C3/C7.

Same-wave consumer: selected JSON direct + typed rows consume generated
Track 1 direct or typed work + independent Track 2 proof in the same
wave.

Pre-blocked routes: sink-local decoded stats, quote-source streaming
hash, direct source-hook folding, parser-owned scratch, byte-output
unescape, semantic string fact hashing, raw f64 shortcut, stale canada
mantissa widening, Track 2 coupling, direct cap-16 reruns, digest as
typed product proof (REDRESS 66-72, 80 watch-list); REDRESS 119/120
LIFTED-only without fresh material differential; admitting three
orthogonal SIMD bodies for the long-string-body scan primitive
(S-P2 V3 §6.2 binding); REDRESS 126 carry-through.

Revert protocol: revert row admits + REDRESS / RESULTS changes + bench
wiring as one slice; add REDRESS naming the failed comparator parity
or missing per-corpus binding.

Downstream effect: W9 disposition feeds W11 close ceremony.

## Section 13 — W10 JSON parse_only Distinct Path + Re-Admit (R8)

Owner paths:

- `skinny/crates/runtime/src/grammars/json/parser.rs` (stand up distinct parse_only path — no full-tape build per ORCHESTRATOR-PROMPT.md:147-149)
- `skinny/crates/codegen/src/` (emit parse_only path via the W5 grammar-agnostic generator template; per workspace metadata `parse_only` shape)
- `skinny/crates/bbnf-bench/benches/json_parity.rs` (parse_only row wiring; wire to `sonic_rs::Skipper` comparator)
- generated JSON `parse_only` output named by the plan
- `skinny/RESULTS.md` (W10 row attribution)
- `skinny/ROLLING-SOTA-DELTA.md` (W10 admits per row)
- `skinny/REDRESS.md` if rejected.

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` A2 + A3 (parse_only unblocked).

Entry gate:

- W1 + W9 admitted (R1 sonic_rs::Skipper comparator wired; W9's re-admit path exercises the comparator infrastructure).
- W10 plan names: exact distinct parse_only path location in `generated_json`; the parse_only row threshold per corpus; Track 1 / Track 2 paths.
- W10 plan MUST include Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 verbatim: W10 is the bound wave for Stage-0 (per p3a:180 — first wave admitting any of {C1 long-string-body SIMD scan, C3 digit_block_simd_accumulate, C7 …} resolves to W10 because the parse_only distinct path per R8 is the first dispatch-envelope behavioral edit that admits C1). Stage-0 = cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites (per `SPEC.md:221` non-negotiable). Stage-0 inheritance chain (5-step): (1) Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim; (2) C1 = long-string-body SIMD scan primitive (queued for S-P3 same-wave admission per S-P2 V3 §6.2); (3) W10 is first wave consuming C1 via parse_only distinct path per R8 (W8 + W9 do not admit C1/C3/C7); (4) therefore W10 carries Stage-0 unconditionally; (5) consumers (must-bind per `SPEC.md:221`): P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13.

Tasks:

1. Stand up a distinct `parse_only` code path in `generated_json` (no full-tape build) per ORCHESTRATOR-PROMPT.md:147.
2. Emit the parse_only path via the W5 grammar-agnostic generator template + workspace metadata `parse_only=true` shape (per `[no-orthogonal-codepaths]` — one collection strategy).
3. Wire to `sonic_rs::Skipper`-class strict comparator (R1 binding).
4. Re-attempt parse_only ADMIT per corpus: Track 1 > Skipper strict + 1 + same-plane / same-corpus / same-equality.
5. Ship F-V2-P1ABC-RERECORD Stage-0 UNCONDITIONALLY per S-P2 V3 §6.3 (W10 is the bound wave per §13 entry-gate inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands. Consumer manifest verified: P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13.

Exit gate:

- Distinct parse_only code path exists in `generated_json`; no full-tape build.
- At least one JSON parse_only row ADMITs > `sonic_rs::Skipper` on same plane / same corpus / same equality.
- `comparator_plane=parse_only` populated; `per_iter_equality=PASS` per iter.
- `audit_overlay_verdict` transition AUDIT-FALSIFIED → AUDIT-SUSTAINED upon ADMIT.
- Track 1 / Track 2 structural independence proven; `track2_entry_point` populated.
- Full-table maintain: ±1.0% on non-target rows.
- F-V2-P1ABC-RERECORD Stage-0 SHIPPED UNCONDITIONALLY per S-P2 V3 §6.3 (W10 is the bound wave per the §13 entry-gate inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites landed in this wave's commit slice; consumer manifest (P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13) verified.

Same-wave consumer: the distinct parse_only path + the
`sonic_rs::Skipper` comparator both consume the same generated_json
emission in the same commit.

Pre-blocked routes: treating parse_only as diagnostic-only (per
SYNTHESIS §5); gate-relabel as admit (P-4 recurrence); REDRESS 119/120
LIFTED-only without fresh material differential; admitting via
StringBlock16 tiny probe / object-pair value-byte control compaction
(REDRESS 82-84); orthogonal codepaths (conditional Vec-vs-scratch
branching per `[no-orthogonal-codepaths]`).

Revert protocol: revert distinct path + bench wiring + RESULTS /
DELTA changes as one slice; add REDRESS naming the failed Skipper
parity or missing distinct emission path.

Downstream effect: W10 disposition feeds W11 close ceremony.

## Section 14 — W11 Close And Alpha Feedback

Owner paths:

- `restart/skinny/tranches/sk-v14/HANDOFF.md`
- a future wave-11 close artifact under `restart/skinny/tranches/sk-v14/research/`
- `skinny/REDRESS.md` only if close reconciliation needs a redress entry.
- `skinny/RESULTS.md` only if reconciling a documented mismatch without source behavior change.
- `skinny/ROLLING-SOTA-DELTA.md` (close-state snapshot).

Doc links: `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`,
`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`,
`restart/skinny/tranches/sk-v14/HANDOFF.md`,
`restart/skinny/tranches/sk-v14/SYNTHESIS.md §0.1 + §6` (close posture + indefatigability clause).

Entry gate:

- W0-W10 each have admitted, rejected, or routed status.
- Their REDRESS / RESULTS / HANDOFF updates are present.

Tasks:

1. Reconcile every wave disposition.
2. Ensure `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/ROLLING-SOTA-DELTA.md`, and `restart/skinny/tranches/sk-v14/HANDOFF.md` agree.
3. Compute the close-state per family: count ADMITTED rows per (JSON parse_only / direct / typed / CSS L4) and per architectural-block-proof family.
4. Route residuals to SK-V15 per indefatigability clause (SYNTHESIS §6); the SK-V14 close brackets SK-V15 immediately under the same pinned bar if any goal remains unmet without architectural-block proof.
5. Feed Pass Alpha / S-P3 lessons back into the close note.

Exit gate:

- Every SK-V14 wave has admitted, rejected, or routed status.
- Final row/status artifacts match latest wave evidence and `SK-V14-open` deltas.
- Per family: count ADMITTED rows; cite per-row architectural-block proof for any non-admitted row.
- No accepted source change lacks profile artifact, row threshold, REDRESS id, Lock 14 proof, Lock 1 triad declaration (if SIMD/union/cost-shape consumer), or same-wave consumer proof.
- SK-V15 bracket dispatched per indefatigability clause if any goal remains unmet.

Same-wave consumer: close checklist and document reconciliation.

Pre-blocked routes: paper close (W11 must close on measurement, not
promise); missing REDRESS; missing RESULTS rows; strict admission from
sidecar/permissive evidence; PMULL/CTZ/B6 canary as performance
evidence (REDRESS 88-90); architecture analogy without row data;
dropping falsifier rows; declaring close without per-row architectural-
block proof for non-admits (per SYNTHESIS §0.1 R10).

Revert protocol: no source revert by default. Reopen the producing
wave or mark close blocked with a mismatch list naming file paths,
rows, and missing evidence.

## Section 15 — Pre-Blocked Routes

Every wave inherits this route ledger. A route may reopen only with
fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant,
no-regression gate, REDRESS citation, and CHALLENGE acceptance.

### Pattern-level pre-blocks (SYNTHESIS §0.4 P-1..P-7 verbatim binding):

- **P-1** — Fake `@generated` header on hand-written templates. The recurrence vector for the CSS L4 fake-admit cluster. SK-V14 generated files post-PRUNE must round-trip through `cargo xtask regen-css` (R4) — hand-patching is forbidden per `[clean-regen-discipline]`. Per α-C §4, W10.3 `nested_layout` (124× anomaly) carries a preemptive round-trip-rule trigger: any second-in-tranche reopen of nested_layout requires user re-pin with intrinsic-block evidence; any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥50× inherits the same trigger.
- **P-2** — `sonic_rs::from_slice::<Value>` mislabelled as strict comparator. A single eager-DOM API was bound for all three planes. SK-V14 binds three plane-correct comparators per R1 (W1).
- **P-3** — Tiny-fixture Criterion-overhead Mbps inflation. SK-V14 corpora pin (R5; W3) is ≥800 KB working set; rows measured on <1 KB fixtures cannot admit.
- **P-4** — Gate-relabel as admit. W14.1-.5 source diffs touched only `gate.rs` / `report.rs` / `lock14_baseline.rs`; the parser was unchanged. SK-V14 admit requires a parser/codegen source delta cited per row + measurement evidence per REDRESS.
- **P-5** — Scaffold-research counted as load-bearing. SK-V14 PRUNE-5 (W7) wires W8 + W9 end-to-end; no row admit may cite W8 / W9 as evidence until the runtime consumer is measured.
- **P-6** — Per-grammar provider modules in generic codegen. SK-V14 PRUNE-3 (W5) collapses these to ONE grammar-agnostic generator template consuming grammar source + workspace metadata.
- **P-7** — Track 1 ≡ Track 2 dishonesty. SK-V14 bench harness must keep Track 1 (generated) structurally distinct from Track 2 (independent oracle); any plane collapse fails gate. The `track2_entry_point` column (per §0.4) is the gate enforcement.

Per memory `[abrogate-before-patch]`: any row family whose REDRESS
history shows two-or-more reopen attempts against the same fake-pattern
DELETEs rather than patches.

### Global blocks (SK-V8 §10 verbatim inheritance):

- New directive, BIR variant, substrate surface, `BackendShape`, `UnionTape`, public substrate API, parser-owned cursor/facts, sidecar substrate, and parallel substrate.
- Generic JSON / CSS policy in generic crates, including renamed helper policy (REDRESS 36-38, 85-86 cluster).
- Sidecar/permissive/lossy/stale comparator evidence as strict admission.
- `tape_vs_tape`, `parse_only`, or telemetry rows as W7 production consumer.
- Orphan primitives, checkasm-only admission, and harness-only hardening as performance proof.
- Track 1/Track 2 coupling or benchmark-private parsers.
- Automatic implementation dispatch.

### Specific REDRESS and Alpha blocks (REDRESS watch-list per dispatch-context §4 + SYNTHESIS §5):

- REDRESS 16, 17, 18, 25: pair-token fusion, function-pointer dispatch, skipless/12-byte width churn, separator/generic alternates as-is.
- REDRESS 28+33: Class A NEON/TBL tiny-string wiring as parse close.
- REDRESS 36-38, 85-86: Lock 14 residue, old JSON helpers, generic JSON branches, `StructuralAlphabet::json`.
- REDRESS 49-55: no-allocation visitor, parse-time aux side tables, EventCursor, parser-local structural-mask cursor, decoded stats sink, quote-source fused string materializer.
- REDRESS 59-65, 72/83: retained string-boundary collapse, always-wide or delayed-wide scanning, Unicode validator/classifier retries, object/key carry, global/direct/Track 2 cap-16, generated-retained StringBlock16 tiny probe.
- REDRESS 66-72, 80: direct source-hook/materialization families, parser-owned scratch, byte-output unescape, semantic string facts, hand typed sinks as proof, stale mantissa widening, raw f64 shortcut.
- REDRESS 74-79, 81, 87: architecture/comparator/CostFacts evidence may be cited only under their admitted boundaries; they do not authorize behavior by analogy.
- REDRESS 82-84: single-quartet Unicode classifier, StringBlock16 tiny probe, object-pair value-byte control compaction.
- REDRESS 88-90: PMULL prefix-XOR default hot body, CTZ/bulk production consumer, B6 canary hardening as performance evidence.
- REDRESS 96-98: full class-column vectors, streaming structural cursors, class-lane-only replays, parser-owned sidecars, UnionTape-style retained structures per Lock 1 v+1 substrate-ceiling history.
- **REDRESS 102/103/106/108 PERMANENT-PRE-BLOCK (SK-V10 measured-rejected items)**: REDRESS 102 (parse_only fact-stream-as-admit) PERMANENT-PRE-BLOCK per P3-E §2.1; binding wave W10 (R8) — any parse_only re-admit through a fact-stream surface requires fresh material differential evidence. REDRESS 103/106/108 PERMANENT-PRE-BLOCK per P3-E §2.1; binding wave W9 (R7) for direct/typed re-admit — these are measured-rejected (NOT AUDIT-FALSIFIED), and the audit-overlay pre-block at §15 ("Audit-overlay pre-block") does NOT bind them; their PERMANENT status follows from measured-rejection history per SK-V10 close, not from audit-overlay.
- REDRESS 119/120: LIFTED per addendum; HISTORY only — closing a JSON row through 119/120 history requires fresh SK-V14 evidence per SYNTHESIS §5.
- REDRESS 126: per V3 §1.4 CH3 NF-CH6-3 C2 scalar-ref evidence upgrade carry-through; primitive consumption requires post-W7 runtime divergence.
- Alpha-E bitmap density-gated route remains reserve research only; it is not in W0-W11 unless a future plan challenges it.
- Tier B string-boundary / quote-backslash-parity / CostFacts-template union is blocked from W7 Tier A by default.

### S-P2 carry-forward pre-blocks (per S-P2 V3 §6 packets):

- **Three orthogonal SIMD bodies for one primitive** per S-P2 V3 §6.2 / P2-F §2.Y. The three convergent identifiers `long_string_body_simd_scan` / `scan_string_special_block_sweep_64` / quote-aware classifier composition admit under ONE canonical primitive name + ONE canonical scalar-ref function at admission time.
- **Past-perfect verb-tense claims on NOT-PRESENT path:line** per S-P2 V3 §3.2 CH6-E. Stage-A authoring targets `byte_context_64.rs` + `bcax_64.rs` must surface in present-future tense ("queued for", "lands same-commit at S-P3", "to be authored under Lock 16 same-commit") until the function body lands.
- **Missing 3-gate CH4 cell on admission manifest** per S-P2 V3 §6.1 CF-3. Every shortlisted candidate's admission manifest carries the scalar-ref status / checkasm-parity expectation / same-wave-consumer NAMED cell.

### AUDIT-FALSIFIED admit-row revert ledger (22 JSON items + 24 CSS L4 items = 46 by-number; dispatch headcount references the 22 JSON revert manifest):

The SK-V13-cycle admit rows falsified by the audit-overlay pack are
enumerated below by REDRESS item id with their binding R-target
NAMED framing-change requirement (per CH3 V1 §2.REVISE-1 prescription;
discharges P3-E §3 per-wave-falsifiability-gate `git grep -n
"REDRESS-{N}"` requirement). The dispatch-referenced 22-item JSON
revert manifest (131-135 + 141 + 143 + 145-153 + 154-158 + 160 = 22)
binds the W1 PRUNE-1 revert; the 24 CSS L4 revert binds W4 PRUNE-2.

- **JSON parse_only (5 items; W1 PRUNE-1 reverts; W10 R8 re-admit framing)**:
  REDRESS 154, 155, 156, 157, 158 — single-lane `sonic_rs_anchor`
  mislabelled as strict parse_only anchor; W1 (R1) deletes the
  anchor; W10 (R8) re-admit requires `sonic_rs::Skipper` plane-correct
  strict anchor + distinct parse_only path + Track 1/2 independence.
- **JSON direct (6 items per SYNTHESIS §0.2 wider 6+11 count; W1 PRUNE-1 reverts; W9 R7 re-admit framing)**:
  REDRESS 131, 132, 133, 134, 135 + 141 — `sonic_rs::from_slice::<Value>`
  mislabelled as strict direct anchor (P-2 recurrence); W1 (R1)
  rebinds direct → `sonic_rs::from_slice::<TargetStruct>()` per
  corpus with strict mode; W9 (R7) re-admit requires per-corpus
  strict struct deser + per-iter equality oracle.
- **JSON typed (11 items per SYNTHESIS §0.2 wider 6+11 count; W1 PRUNE-1 reverts; W9 R7 re-admit framing)**:
  REDRESS 143 + 145, 146, 147, 148, 149, 150, 151, 152, 153 + 160 —
  comparator mislabelled as strict typed anchor; W1 (R1) rebinds
  typed → per-corpus typed struct deser via existing
  `real_typed_struct.rs:695-727` bindings promoted from parity-
  assertion to anchor-row; W9 (R7) re-admit requires per-corpus
  typed struct deser + per-iter equality oracle.
- **CSS L4 features (binding to W8 R6 re-admit framing per SPEC §11)**:
  the 24 CSS L4 audit-falsified admit rows revert at W4 PRUNE-2; the
  validation-pack §reference `v1 §1-6` (fake `@generated` header on
  hand-written templates; no regen-css xtask; CSS scanners as
  fixture lookups) cites per row. W8 R6 re-admit requires grammar-
  derived pipeline (W2 R4) + production corpora (W3 R5) + lightningcss
  full-parse + cssparser full-parse comparator + per-iter equality
  oracle.

The 22 JSON revert rows (5 parse_only + 6 direct + 11 typed) +
24 CSS L4 revert rows are gate-enforced by the audit-overlay column
below; each row carries `audit_overlay_verdict=AUDIT-FALSIFIED` post-
W1 / post-W4 with the validation-pack §reference cited.

### Audit-overlay pre-block:

- Any row currently AUDIT-FALSIFIED requires fresh material differential evidence to re-admit, cited per REDRESS. The `audit_overlay_verdict` column is gate-enforced per row.

## Section 16 — G-Alpha And Dispatch Scope

S-P3 ships this SPEC at V1 commit alongside the six P3 artefacts under
`restart/skinny/tranches/sk-v14/research/p3/`. The CHALLENGE V1 cycle
follows per `PASS-3-SYNTHESIS-PLAN.md §3` (six lens agents + one
aggregator). The §3Z convergence rule per `ORCHESTRATOR.md §3Z` (≥95%
ACCEPT × 2 cycles + zero orphan REVISE + V ≤ 5 ceiling) gates S-P3
LOCK. After S-P3 LOCK + T-P3 LOCK, **G-Omega user gate** per
`ORCHESTRATOR-PROMPT.md:204` is the only mandatory relinquish before
W0 dispatches.

Dispatch scope:

- W0 is authorized after G-Omega closes.
- W1-W11 remain conditional. They require W0 closure + S-P3 LOCK + each preceding wave's closure per the sequencing constraints (R4 → PRUNE-2; C-1 → C-4; per-wave triumvirate per `SKINNY-TRIUMVIRATE.md §1-§3`) + required CHALLENGE acceptance + orchestrator/user dispatch before redress.
- PRUNE waves (W1, W4, W5, W6, W7) dispatch BEFORE any new-admit wave (W8, W9, W10) per `ORCHESTRATOR-PROMPT.md:110` R3 binding.
- No W7 implementation dispatches from S-P2 or S-P3 alone.

The orchestrator's per-wave dispatch contract lives at
`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`.
