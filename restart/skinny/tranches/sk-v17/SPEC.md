# SK-V17 SPEC — S-P3 Wave Plan (CSS L4 Typed > lightningcss via Unified Tape/Layout/Projection + aarch64 NEON)

Date: 2026-05-29.

Status: S-P3 Synthesis-Plan packet, cycle V3 (folds the V2 CHALLENGE disposition: the W2 exit-gate maintain budget at §5 is REVISEd from a bare 0% "no regression" floor to the bench-falsifiable -2.0% median band vs the W1 typed-tape baseline, reconciling SPEC↔P3-C §2.2/§3; all V1 dispositions carried forward). This file is NOT an implementation
dispatch. It folds Pass Alpha's SK-V17 goalset (`SYNTHESIS.md` §0, 6496fecae), the
S-P1 N=200 profile (`HARDENING-S-P1-V4-CONSOLIDATED.md`, 0ae1caa52), and the S-P2
locked candidate pool L1–L9 (`HARDENING-S-P2-V3-CONSOLIDATED.md` §3, f87ee713a) into a
PRUNE-before-rebuild W0–W5 wave plan. Master HEAD `f87ee713a`.

Authority:

- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` (the goalset — §0.1 close condition,
  §0.5 per-corpus targets, §0.6 comparator gate, Section 2 telemetry, §0.4 pre-blocks).
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  (the profile — §3.1 bench, §3.3 hot leaves, §3.4 antecedents).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  (the candidate pool — §3 L1–L9, §4 REJECTed set, §6 binding conditions).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` (the S-P3 cohort).
- `restart/skinny/tranches/sk-v8/SPEC.md` (the SPEC shape mirrored).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (the per-wave triumvirate
  contract this SPEC's waves conform to).
- `skinny/RESULTS.md`, `skinny/REDRESS.md`.

Dispatch lock:

- No SK-V17 implementation wave dispatches from S-P3 itself.
- Current dispatch authority covers W0 only.
- W1–W5 are conditionally gated by this packet; each remains blocked until its prior
  wave closes, the orchestrator/user dispatches the wave triumvirate, and the wave's
  entry gate holds.

## Section 0 — Close Condition And Goalset

### 0.1 — Global Close Condition (folded from SYNTHESIS §0.1)

SK-V17 closes only when all of these are true. Every surface citation is the benched
skinny tree (`skinny/crates/`); each tape/layout gate is verifiable by grepping
`skinny/crates/`, NOT `crates/core/` (the wrong-tree-dishonesty REJECT, SYNTHESIS
benched-surface note).

1. **JSON guard.** 51/51 JSON rows remain admitted, strict, same-plane on aarch64
   (Apple M5 Max); touched rows re-run cold; tape activation moves no JSON row out of
   A/GO. JSON already rides the lazy-offset tape — it is the >SOTA proof and the
   regression tripwire.
2. **Tape activation (not dead code).** The shared flat-tape substrate
   (`skinny/crates/runtime/src/tape/`) becomes the LIVE parse substrate for benched CSS
   Track 1 (`track1::parser::parse`, reached `nonjson_css_l4.rs:596`).
   `Tape`/`ValueRef`/`TapeBuilder` appear in the CSS parse path and in
   `grammars/css_l4_*/`; grep returns non-zero; `PayloadArena` write/alloc counters
   confirm tape emission, not a fact-stream String. Track 1 stops returning `String`
   (`emit_fact_stream` retired, `generated.rs:5`). NO new cursor/builder type — the
   EXISTING `Tape`/`ValueRef`/`TapeBuilder` is the only substrate (Lock 1).
3. **Layout-driven projection.** ONE `BackendRule`-walking accessor generator exists in
   `skinny/crates/codegen/` and emits `document/value/view/visitor` per grammar — JSON the
   existing witness (its hand-written `value_from_ref` rider re-emitted byte-equal THROUGH
   the new generator), CSS L4 the first-mover rider — by walking the SAME `BackendRule`
   shape the parser emits, lowered via `lower/{tape_plan,offset_tape,event_tape}.rs`,
   isomorphic to JSON's `value_from_ref` (`json/value.rs:143`) over the existing
   `Tape`/`ValueRef`. The mechanism is grammar-generic (one generator walking
   `BackendRule`), NOT a CSS-pinned emitter; a generator that leaves JSON's hand-written
   path untouched is the generic-named-CSS-generator failure mode (CH2) and FAILS. The CSS
   routing today
   encoded by the hand-coded `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`) is
   RETIRED and DERIVED from the `.bbnf`/`BackendRule` shape, preserved as DATA in the
   tape-plan lowering, NOT lost, NOT re-hardcoded (Lock 14). Close gate keyed strictly
   to skinny-greppable facts: `W5C_REQUEST_FACT_PROFILES` retired; no per-rule-id match
   arms in skinny generic crates that JSON does not need; every residual CSS routing
   entry names the `.bbnf` rule it derives from; the CSS regen profile array
   (`regen_css.rs:45-153`) trends toward the JSON emitter shape. (The `css_l4.toml`
   594-vs-34-line asymmetry is a TOTALITY artefact — grep-clean-absent from `skinny/` —
   INFORMATIONAL only, NOT an SK-V17 close gate.)
4. **CSS typed equality (gate before speed).** Track 1 typed CSS summary equals
   cssparser same-workload typed summary BEFORE any speed counts — EXACT 8-field
   structural equality (`rules=10136, style=9561, sel=9561, decls=20043`,
   `track1_errors=0`, `cssparser_errors=0`, 4/4 corpora, banked `1c5bd7a25`). Re-proven
   after the tape conversion on the NEW typed path, not assumed.
5. **preserve-rich-ast.** The typed CSSOM (`CssColor`, `CssDimension`, `CssLength`,
   `CssFunction`, `Selector`, `CssRule`, `CssTypedValue`) is produced by lazy
   `ValueRef`-view projection over the tape, NOT flattened to spans, NOT materialized
   eagerly (no per-leaf `Box::new`, no eager value tree). Value-plane population parity
   holds (dimensions/colors/functions/lists counts match the eager-tree baseline).
   Non-negotiable.
6. **CSS >SOTA on regular corpora.** On a per-corpus basis at N≥50 cold samples +
   median, CSS L4 typed Track 1 BEATS lightningcss full-CSSOM on the structurally-regular
   benched corpora (animate, bootstrap). Per-corpus median-Mbps thresholds in §0.5.
   lightningcss is the materializing comparator (full-CSSOM build), re-baselined
   same-run, NOT cssparser token-scan.
7. **Honest tailwind handling.** tailwindcss benched cold adversarially with N≥50 +
   median. If it crosses the lightningcss bar, it admits; if it lands short, the
   residual gap is REPORTED honestly with the hot-leaf attribution and recorded in
   REDRESS, NOT paper-closed, NOT hidden behind a corpus average. No corpus-average
   claim substitutes for per-corpus medians.
8. **Telemetry honesty (N≥50).** The W6 single-sample harness is retired; the SK-V17
   harness (`css_canon_bench.rs`, code-asserted N≥50 `:250`) takes N≥50 cold samples per
   corpus per workload and reports the MEDIAN Mbps. lightningcss is wired same-run,
   same-plane, full-CSSOM-materializing, re-baselined this run. The fact-stream
   comparator (`assert_lightningcss_strict_equality` against a fact stream,
   `nonjson_css_l4.rs:776`) is retired; the comparator must build CSSOM.
9. **NEON hot-leaf union.** Any SIMD is profile-first (RE-PROFILED on the benched tape
   path, not inherited from the core-tree profile), scalar-referenced, checkasm/parity
   verified, same-wave consumed, aarch64-only (NEON + optional dotprod/i8mm; NO x86, NO
   SVE). The grammar-general leaf routes through `dispatch.rs select_classifier`
   (`:42`), produces only a `Vec<u32>` structural index, and the tape consumes it. The
   NEON leaf exercises at least one non-JSON grammar (`css_l4`). NEON is gated behind
   tape activation.
10. **Generated-state cleanliness.** The 8 git-dirty generated CSS / real-typed files
    are cleanly regenerated as fresh generator output (`cargo xtask regen --check` 9/9,
    exit 0). Generated files are output of fresh regen, never hand-patched (Lock 6/14).
11. **Foldable into TOTALITY.** The unified tape/layout/projection model + NEON leaf set
    are structured so the TOTALITY tree (`crates/core/src/runtime/tape/`) can adopt them
    in SK-V18. Projection generality exercised by-construction on JSON + CSS only;
    non-CSS-non-JSON (Sheets/BBNF-self) is the SK-V18 proof (`sheets_witness` has no
    `BackendRule` shape and cannot serve as an SK-V17 projection exercise, §0.4).
12. **PASS-IMPL close audit.** The close audit accepts every axis or records row-level
    intrinsic-block proof with measurement.

### 0.2 — Comparator Classes (SYNTHESIS §0.6, CSS framing)

| Comparator | Plane | Admission role |
|---|---|---|
| lightningcss full-CSSOM | full CSSOM build | THE fair >SOTA bar; re-baselined same-run at N≥50; the only strict admission anchor for the CSS >SOTA gate |
| cssparser token-scan | none (tokens only) | flaw probe; beating it is NOT a SOTA claim; planning signal only |
| Track 1 typed (bbnf, post-tape) | typed direct via lazy `ValueRef` projection | the subject |
| Track 2 / oracle | independent reference | equality anchor, structurally distinct from Track 1 (Lock 1, CH5) |

Strict admission is executable, not prose: `gate-json` rejects a CSS >SOTA admission
unless `css_comparator_plane=full-cssom` for the lightningcss bar, the comparator is the
same-run re-baselined lightningcss median, `css_typed_summary_equal=true` (gate before
speed), and `css_rich_ast_preserved=true`. cssparser/fact-stream/historical/broadcast
evidence is planning-only, never strict admission.

### 0.3 — Outcome Enum

```text
A   admit-shaped (> bar)
C   correctness
G   GO
K   pre-blocked (AZ-IV eager-value-tree class)
L   loss
N-direct
S   substrate-guard / non-SOTA
```

`A`/`L`/`N-direct`/`S` are valid CSS outcomes. `S` is the substrate-guard spelling for
admission-capable parse outcomes that do not cross the SOTA bar. The recognition-only
plane (`track1_full_parse`, A-shaped but 4-field, NOT preserve-rich-ast) does NOT by
itself discharge the SK-V17 typed gate (S-P1 §3.2): a recognition-only A may not admit
as a typed >SOTA close.

### 0.4 — Required Telemetry (SYNTHESIS Section 2)

The harness MUST emit N≥50 cold samples and report the MEDIAN per row. The
`W6_SAMPLE_COUNT=1` harness is retired; the gate rejects any CSS row with
`css_sample_count<50` or `css_sample_statistic≠median`.

Required CSS columns (SK-V17 additions to the SK-V15/16 JSON schema):

```text
css_corpus                                  (bootstrap|tailwindcss|material-components-web|animate; css_l4_corpus.rs:22-54)
css_sample_count                            (>=50)
css_sample_statistic                        (median)
css_sample_mode                             (cold)
css_track1_typed_median_mbps
css_lightningcss_full_cssom_median_mbps     (same-run re-baseline; the materializing bar)
css_cssparser_tokenscan_median_mbps         (optional; flaw probe only)
css_comparator_plane                        (full-cssom|typed-direct|token-scan|none)
delta_vs_lightningcss                       (per-corpus, same-run median)
css_track1_typed_passes
css_cssparser_typed_passes
css_typed_summary_equal                     (EXACT 8-field; gate before speed)
css_rich_ast_preserved                      (lazy ValueRef projection, not flattened, not eager)
css_provider_source                         (grammar source path)
tape_activated                              (PayloadArena write/alloc counters; NOT a crates/core grep)
lazy_view_generated                         (accessor generator emits document/value/view/visitor over BackendRule)
projection_generality_exercise              (json|css_l4; sheets_witness INVALID)
w5c_profile_array_retired                   (W5C_REQUEST_FACT_PROFILES deleted; routing grammar-derived)
dirty_generated_state                       (clean|retired|routed-intrinsic-block)
native_simd_status                          (scalar|parity-pass|checkasm-pass|not-applicable)
simd_non_json_exercise                      (css_l4 — the dischargeable non-JSON SIMD exercise)
```

The retained JSON schema (Track 1 Mbps, Track 2 Mbps, sonic-rs strict, simdjson DOM,
yyjson, serde_json, Δ columns, Hot leaf, Signal) stays in force for the JSON guard rows.

Gate consumers bound by this SPEC:
`(cd skinny && cargo xtask gate-json --check-results --skv17-css-sota-report <path>)`
consumes per-corpus median, N≥50, full-CSSOM comparator, equality boolean,
preserve-rich-ast boolean, tape-activated boolean, `w5c_profile_array_retired` boolean,
per-corpus delta-vs-lightningcss, hot leaf, admitted-row count; re-uses the SK-V16
dirty-generated + native-simd report consumers. The gate REJECTS any CSS row missing
`css_sample_count>=50`, `css_sample_statistic==median`, `css_sample_mode==cold`,
`css_comparator_plane==full-cssom` for the lightningcss bar, `css_typed_summary_equal==true`
before any speed admission, or `css_rich_ast_preserved==true`; any row whose `css_corpus`
is not in the benched set (no phantom `normalize`); and any single-tuple broadcast
(`sample_count==1` or one tuple across multiple corpus rows — the W8R tripwire).
Every emitted field must be consumed by `gate-json` in the same wave; a producer-only
field fails the wave (typed-materialization-invariant).

### 0.5 — Opening Row Goalset (SYNTHESIS §0.5)

The >SOTA bar is per-corpus, against lightningcss full-CSSOM, N≥50 cold, MEDIAN. The
benched corpus set is fixed in `css_l4_corpus.rs:22-54`:
`{bootstrap, tailwindcss, material-components-web, animate}`. `normalize` is NOT in this
set; no gate names it. The lightningcss bar is the per-corpus measured median
re-baselined in W0 — the prior numbers (793/833/929/974, run-dependent, SYNTHESIS §0.2)
are NOT the gate. ALL per-corpus lightningcss endpoints are UNMEASURED-PENDING: no wave
exit-gate may key on an inferred per-corpus endpoint until the W0 N≥50 harness emits the
per-corpus split.

| Corpus | Current (fact-stream path) | Target close state | Expected intervention | Fallback if not moved |
|---|---|---|---|---|
| animate (regular) | tape decodes no CSS; Track 1 is a fact-stream String | median Track 1 typed > median lightningcss full-CSSOM same run (>1.0×), N≥50 | W1 tape activation + W2 layout projection + W3 NEON structural index + W4 commit-by-construction (conditional) | if <1.0× after W3/W4: REJECT row, record residual gap + hot leaf in REDRESS; do NOT paper-close |
| bootstrap (regular) | tape decodes no CSS; A-series recognition-only hit 454 Mbps (`3b8b757d`) | same as animate | same stack | same as animate |
| tailwindcss (hardest) | A-series recognition-only 496 Mbps; eager path WATCHDOG'd 10583× under AZ-IV | benched cold N≥50; ADMIT if >lightningcss, else REPORT honest residual + hot leaf | W3 adversarial delimiter-table tuning; udot/i8mm REJECTed (no antecedent, §9) | allowed to land short on first pass; record gap honestly in REDRESS; NOT tranche-blocking provided ≥1 regular corpus crosses |
| material-components-web (full, 495454 B) | Track 1 is a fact-stream String; broadcast tuple ~2319 (NOT typed, falsified) | per-corpus median reported; cross-bar admit OR honest residual | same stack | report median delta; the full-corpus row is the integration check, not a single-corpus gate |

JSON guard floor: all 51 JSON rows maintain A/GO strict same-plane, throughput cells
within ±1.0% of `SK-V17-open`, across every behavior wave.

Tranche-level success criterion: **≥1 regular corpus (animate OR bootstrap) crosses the
lightningcss full-CSSOM bar at N≥50 median**, preserve-rich-ast intact, EXACT cssparser
equality re-proven, JSON 51/51 held. tailwind crossing is a stretch; honest residual
acceptable and recorded. If NO regular corpus crosses after the four-lever stack, the
tranche records the honest residual and escalates per PASS-ALPHA §8 (`WARN`).

## Section 1 — Non-Negotiables

- **Lock 1 — one substrate.** Exactly ONE retained tape (the existing skinny
  `Tape`/`ValueRef`/`TapeBuilder`). No second tape, no `StructLayout`/`TapeStructBuilder`/
  `TapeCursor`, no `UnionTape`, no public substrate API, no parser-owned structural
  cursor/facts, no sidecar event vector, no aux density/projection table, no retained
  cursor/list, no parallel source pass, no cross-call classifier-state retention. A
  SIMD mask stream is a transient producer, not a retained sidecar; if structural
  offsets are retained, the structural projection IS the tape.
- **Lock 14 — grammar-neutral.** No JSON policy in generic crates. The CSS routing is
  DERIVED from the `.bbnf`/`BackendRule` shape, never hand-curated; relocating per-rule
  branching into projection DATA or flag form is the Lock-14-phrase-#1 re-entry seam and
  is FORBIDDEN. Every generic-crate edit carries a non-JSON proof (§2.1).
- **No eager value tree** (AZ-IV K-block). Materialization stays lazy-by-default; the
  tape appends offsets; typed values are reconstructed on demand via `ValueRef`. No
  per-leaf `Box::new`, no f64-alloc-per-number, no per-color `Box<CssColor>`.
- **No fact-stream String as admission plane.** `emit_fact_stream`/`emit_full_parse`/
  `CssFullParseSummary` are diagnostic-only, never an admission surface.
- **Scalar reference + checkasm parity required before primitive wiring.** Per SIMD
  primitive: scalar twin + checkasm differential, both pre-wiring.
- **Same-wave consumer per primitive.** Every wave that lands a primitive lands its
  hot-path consumer in the same commit. No orphan kernel ships (SK-V5 failure).
- **No deferrals.** A wave cannot close on "wired", "advisory", "future consumer",
  "integrated", or "paper close" language without a measured bench row. No wave closes
  on a future-phase promise.
- **preserve-rich-ast.** The typed CSSOM is never flattened for speed.
- **Strict comparator gate.** No CSS >SOTA admission except against the same-run
  re-baselined lightningcss full-CSSOM on a matching (full-cssom) plane, with
  `css_typed_summary_equal=true` proven before speed.
- **Every miss becomes REDRESS evidence or an explicit routed residual** — never a
  silent retreat.
- **aarch64 only.** No x86, no AVX-512, no SVE.

## Section 2 — Wave Manifest, Caps, And Reruns

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Impl/redress cap |
|---|---|---|---|---|---:|
| W0 | §3 | Baseline Profile + Telemetry Lock + lightningcss CSSOM re-baseline | Dispatchable now | 0 behavior LOC; ≤300 harness/gate/report/test LOC (the N≥50 + full-CSSOM comparator wiring + SK-V17 column gate) | ≤90 min |
| W1 | §4 | PRUNE: retire fact-stream + W5C array → tape activation | Conditional on W0 close | ≤450 source/test LOC; generated output named separately + diff-audited | ≤90 min |
| W2 | §5 | Layout-driven lazy projection generator | Conditional on W0/W1 close + CHALLENGE | ≤450 source/test LOC default; ≤650 only with an accepted pre-redress fit proof (defined §5) | ≤90 min |
| W3 | §6 | NEON structural index (re-profiled) | Conditional on W0/W1/W2 close + CHALLENGE | ≤450 source/test LOC; generated SIMD named separately | ≤90 min |
| W4 | §7 | Commit-by-construction Alt-mode (CONDITIONAL) | Conditional on W1 re-profile firing the L9 gate | ≤300 source/test LOC | ≤90 min |
| W5 | §8 | Close, clean regen, Lock-14 audit, Alpha feedback | Conditional on W0–W4 dispositions | 0 source LOC default; ≤150 named Lock-14 cleanup LOC; docs/RESULTS/REDRESS/HANDOFF reconciliation | ≤90 min |

Wave count = 6 (≤12 skinny ceiling). Active shortlist = 8 (L1,L2,L3,L4,L5,L6,L7,L8); L9
conditional, not active until its re-profile gate fires.

LOC budgets are conjunctive with the 90-minute cap and rerun ceilings. They count
hand-edited source, tests, gate/report/schema code, and hand-written doc/result edits.
Generated outputs do not consume the source LOC budget, but every generated file must be
named, diff-audited, and in the revert slice. A plan exceeding its LOC budget or the
90-minute cap MUST split before dispatch or return REVISE.

Phase caps (per `SKINNY-TRIUMVIRATE.md`):

| Phase | Cap (dispatch-hard-cap) |
|---|---:|
| Research | 20 min per agent, max 6 agents |
| Plan | 15 min |
| CHALLENGE | 90 min when first-of-class, substrate-touching, primitive, or high-risk (W1/W2/W3 are all first-of-class) |
| Implementation/redress | 30 min per dispatch, ≤90 min wave wall incl. source edits, generation, verification, RESULTS/REDRESS, rollback |

Per dispatch-hard-cap: every dispatch carries "HARD CAP: N min. At 0.9N commit, at N
halt" (research 20, plan 15, redress 30). These match the P3-B per-wave breakdown.

Rerun ceilings:

| Wave | Focused verification | Rerun ceiling |
|---|---|---|
| W0 | harness N≥50 assert, lightningcss CSSOM comparator wiring, SK-V17 column gate, full-table schema | one gate refresh + one confirm rerun if variance invalidates telemetry |
| W1 | tape-activation grep + PayloadArena counters, W5C retirement, EXACT 8-field equality re-proof, JSON 51/51 maintain, generated diff | one full gate refresh; second requires REDRESS cost note |
| W2 | projection generator tests, rich-ast parity, per-corpus typed median N≥50, generated diff, JSON 51/51 maintain | one full gate refresh; second requires REDRESS cost note |
| W3 | scalar/checkasm per primitive, NEON parity, per-corpus >SOTA gate N≥50, generated SIMD diff, JSON 51/51 maintain | one full gate refresh; second requires REDRESS cost note |
| W4 | byte-identical tape with/without Alt-mode, gated-corpus lift N≥50 | one full gate refresh |
| W5 | `regen --check` 9/9, Lock-14 grep/audit, RESULTS/REDRESS/HANDOFF zero-drift | no performance rerun unless source moved |

Extra reruns beyond the ceiling are REDRESS cost evidence, not retry room.

### Section 2.1 — Generality And Lock 14 Gate

Every wave has this exit gate, with extra checks when generic crates are edited:

- **Public API scan:** no new public JSON-named (or CSS-named) API in generic crates.
- **Grammar-branch scan:** no generic branch selects behavior by grammar name, corpus
  name, rule role, object/array role, field name, or layout role.
- **Primitive/table scan:** no generic primitive, SIMD table, or classifier embeds CSS
  (or JSON) structural policy unless it is generated byte-set data plus opaque class
  ordinals with a scalar reference and a same-wave consumer. The L1 classifier's only
  grammar datum is the `alphabet: &[u8;64]` passed to `select_classifier` (Lock-14
  vehicle); the CSS `;{` pair uses the eq-set fan, NOT the lo6 table (the `& 0x3f`
  slot-59 collision).
- **Role/fact boundary:** generic code may store/search generated structural class
  ordinals or opaque fact ids, but rule-role/recovery/layout/record-boundary meaning
  lives only inside generated grammar modules keyed by parser state + class/byte.
- **Template/provider boundary:** CSS-specific templates/providers remain per-grammar
  surfaces; generic codegen consumes grammar-derived facts, not hand-coded CSS policy
  under neutral names. `W5C_REQUEST_FACT_PROFILES` is RETIRED, not relocated.
- **Non-JSON proof (the SK-V17 first-mover):** CSS L4 is the SK-V17 exercised non-JSON
  rider — it must compile, lower, project, and run via the new tape/projection path for
  every generic codegen/runtime/SIMD/projection edit. The projection-generality riders
  exercised are JSON + CSS only (`projection_generality_exercise ∈ {json, css_l4}`);
  the NEON SIMD non-JSON exercise is `css_l4` (`simd_non_json_exercise=css_l4`).
  Sheets/BBNF-self generality is asserted-by-construction with proof DEFERRED to SK-V18
  (`sheets_witness` has no `.bbnf`/`BackendRule` shape and CANNOT serve as a projection
  exercise; SYNTHESIS §0.4 generality clause). A wave that lets CSS or JSON policy into
  a generic crate fails CH2.

Allowed CSS-specific surfaces: grammar inputs (`.bbnf`), generated CSS output,
per-grammar providers/templates, tests, host/API schema facts.

## Section 3 — W0 Baseline Profile + Telemetry Lock + lightningcss CSSOM Re-baseline

Owner paths:

- `skinny/crates/bbnf-bench/` (the `css_canon_bench.rs` harness, the lightningcss
  full-CSSOM comparator wiring, the SK-V17 column emission)
- `skinny/xtask/src/` (the `gate-json --skv17-css-sota-report` consumer)
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v17/research/` using the `wave-0-<topic>.md` pattern
- `skinny/REDRESS.md` only if W0 rejects

Entry gate:

- `skinny/RESULTS.md` is the SK-V16 close baseline.
- W0 plan names the `SK-V17-open` capture method and the no-behavior-change proof.

Tasks:

1. Capture the current report as `SK-V17-open`.
2. Wire lightningcss as a same-run, same-plane, full-CSSOM-materializing comparator
   re-baselined this run (NOT a fact-stream); retire the fact-stream comparator
   (`assert_lightningcss_strict_equality` against a fact stream, `nonjson_css_l4.rs:776`).
3. Take N≥50 cold samples + median per corpus per workload (the harness asserts N≥50 at
   `css_canon_bench.rs:250`; confirm it fires); emit the per-corpus lightningcss
   full-CSSOM median for `{bootstrap, tailwindcss, material-components-web, animate}` —
   the ONLY measured per-corpus endpoint that downstream gates may key on.
4. Add the SK-V17 telemetry columns (§0.4); populate the JSON guard row telemetry.
5. Make `gate-json` reject `css_sample_count<50`, `css_sample_statistic≠median`,
   `css_sample_mode≠cold`, non-full-cssom lightningcss plane, phantom `normalize`, and
   any single-tuple broadcast (the W8R tripwire).

Exit gate (MEASURABLE):

- `SK-V17-open` captured; per-corpus lightningcss full-CSSOM median Mbps emitted at N≥50
  cold for all four benched corpora.
- All 51 JSON rows + the CSS guard rows satisfy §0.4; throughput cells within ±1.0% of
  `SK-V17-open`.
- `gate-json` rejects the four malformed-row classes above (proven by a fixture row).
- NO parser/scanner/SIMD/codegen behavior or generated parser output change lands.

Same-wave consumer: `gate-json` consumes every emitted telemetry field and rejects
malformed/missing evidence in the same W0 slice.

Pre-blocked routes: all behavior routes; the 24-row broadcast (one tuple → N rows); a
fact-stream comparator; `css_sample_count==1`; phantom `normalize`; any source edit not
required for telemetry/gate/comparator wiring.

Revert protocol: revert the W0 harness/gate/comparator commits together, restore the
opening `skinny/RESULTS.md`, record a W0 REDRESS rejection naming the missing
comparator, gate, or row. Hardening docs remain as history.

Downstream effect: W0 rejection blocks W1–W5.

## Section 4 — W1 PRUNE: Retire Fact-Stream + W5C Array → Tape Activation

This is the PRUNE-before-rebuild wave. It DELETES the fact-stream String plane and the
hand-coded routing array, THEN routes CSS into the existing tape — it does not add a
parallel tape path beside the String. Levers 1+2 (SYNTHESIS §3).

Candidates: **L2** (`push_plain_offset` tape append) + **L7** (one-shot SIMD reserve,
gated behind L2) + **L3** (lazy `ValueRef` projection — the same-wave consumer; the
minimal cursor read sufficient to re-prove equality; the full rich rider generalizes in
W2). L2's same-wave consumer is L3 (P2 §L3); they land together or neither.

Owner paths:

- `skinny/crates/codegen/src/lib.rs` (DELETE `W5C_REQUEST_FACT_PROFILES` `:336`,
  consumed `:567,:611`, selected `:299`)
- `skinny/crates/codegen/src/lower/{tape_plan,offset_tape,event_tape,eager_tape}.rs`
  (the routing derived from `BackendRule` shape, preserved as DATA)
- `skinny/crates/codegen/src/runtime_generator.rs` (route CSS off
  `RuntimeEmitterKind::RequestFacts`)
- `skinny/xtask/src/regen_css.rs:45,63,81,99,117,135,153` (the seven
  `RequestFactsProfile` literals flipped off the fact stream; regenerated via
  `regen_css` fn `:164`)
- `skinny/crates/runtime/src/tape/assembler.rs:42,71,89` (`TapeBuilder`,
  `push_plain_offset`, `reserve_offsets_cold` — used, not modified to add a second tape)
- `skinny/crates/runtime/src/grammars/css_l4_*/parser.rs:6` (the SEVEN live consumers —
  each `generated::emit_fact_stream(input)` entry re-pointed to the tape plane)
- `skinny/crates/runtime/src/lib.rs:76,91,108,126,143,162,434` (the SEVEN
  `css_l4_*_emit_fact_stream` round-trip test consumers — migrated to the tape plane or
  deleted in the W1 commit; no dangling `emit_fact_stream` round-trip assertion survives)
- `skinny/crates/codegen/src/lib.rs:581,1001,1035` (the generator-output-string assertions
  that `.contains("emit_fact_stream")` — migrated to assert the tape-emitting generator
  output, or deleted, in the W1 commit)
- `skinny/crates/codegen/src/runtime_generator.rs:621,666,694` (the `emit_fact_stream`
  generator template — re-pointed to emit the tape append, not a fact-stream `String`)
- generated CSS output (`grammars/css_l4_*/generated.rs` — named, diff-audited)
- `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected

Entry gate:

- W0 admitted; `SK-V17-open` telemetry exists.
- W1 plan names the seam-flip site (`regen_css.rs:45-153`), the W5C deletion site, the
  `BackendRule`-derived routing data, the checkpoint (`offsets.len()` marker) / rollback
  (truncate) mechanism, and the revert slice. No `split_off`, no `Vec<Vec>` arena, no
  per-leaf eager payload.
- CHALLENGE accepts that W1 is not a renamed REDRESS 50-55/60-72 route and introduces no
  second substrate (Lock 1).

Tasks:

1. DELETE `W5C_REQUEST_FACT_PROFILES`; derive CSS routing from the `.bbnf`/`BackendRule`
   shape, preserved as DATA in the tape-plan lowering (every residual routing entry
   names its `.bbnf` rule). Flip the seven `regen_css.rs` literals off
   `RuntimeEmitterKind::RequestFacts`.
2. Route benched CSS Track 1 from `emit_fact_stream` String emission to `TapeBuilder`
   Open/Close/Leaf appends (`push_plain_offset` = one branchless u32 write into the
   EXISTING `offsets`); L7 sizes `offsets` from the W3 scan count in one cold reserve
   (L7 is gated behind the tape; if W3 has not landed the index, L7 sizes from a
   conservative byte-proportional bound — never a per-corpus literal).
3. Land L3's minimal `ValueRef` cursor read sufficient to re-prove the 8-field equality.
4. Re-prove EXACT 8-field structural equality vs cssparser on the NEW typed-tape path
   BEFORE any speed counts.
5. Regenerate the 8 dirty files as fresh generator output.
6. Migrate or delete EVERY `emit_fact_stream` round-trip consumer in the same W1 commit
   (the seven `runtime/src/lib.rs` test fns `:76,91,108,126,143,162,434`; the seven
   `grammars/css_l4_*/parser.rs:6` live entries; the three `codegen/src/lib.rs`
   generator-output `.contains("emit_fact_stream")` assertions `:581,1001,1035`). Per the
   same-wave-consumer non-negotiable, NO `emit_fact_stream` round-trip assertion may dangle
   after the wave: a surviving consumer asserting a String round-trip strands a retired
   plane and FAILS the wave.

Exit gate (MEASURABLE):

- `tape_activated=true`: grep of `TapeBuilder|ValueRef|PayloadArena|crate::tape` over
  `grammars/css_l4_*/` returns NON-ZERO; `PayloadArena` write/alloc counters confirm the
  parse emits into the tape, not a String.
- `emit_fact_stream` retired as the live plane (returns no `String` for the admission
  path); `w5c_profile_array_retired=true` (grep of `W5C_REQUEST_FACT_PROFILES` over
  `skinny/crates/` returns ZERO). NO `emit_fact_stream` round-trip assertion dangles: grep
  of `emit_fact_stream` round-trip asserts over `runtime/src/lib.rs` + `codegen/src/lib.rs`
  returns ZERO surviving String-round-trip consumers (the seven test fns + three codegen
  asserts are migrated to the tape plane or deleted in this commit).
- `css_typed_summary_equal=true` EXACT 8-field (`rules=10136, style=9561, sel=9561,
  decls=20043`, `track1_errors=0`, `cssparser_errors=0`, 4/4 corpora).
- JSON 51/51 A/GO maintained within ±1.0% of `SK-V17-open`.
- NO speed admission this wave (equality is the gate before speed). No second substrate;
  no parser-owned cursor; exactly one retained tape survives.

Same-wave consumer: L3's `ValueRef` cursor read is L2's consumer in the same commit; the
generated CSS retained parser is the production consumer of the tape.

Pre-blocked routes: AZ-IV eager value tree; the fact-stream String as an admission
plane; the W5C array (RETIRED, not extended, not relocated into projection DATA);
StructRegistry/Arena<G> hot-path indirection; the 24-row broadcast; a second substrate
(skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`); REDRESS 50-55, 60-72;
`split_off`/`Vec<Vec>` arena.

Revert protocol: revert the codegen/lower/regen/runtime-wiring commits + generated CSS
output as one slice; restore the opening RESULTS; add REDRESS naming the seam that
failed and the equality delta.

Downstream effect: W1 rejection blocks W2–W5. **Post-W1 obligation:** a typed-tape
re-profile (N≥50) on the new path determines whether the W4 L9 gate fires (§7 entry).

## Section 5 — W2 Layout-Driven Lazy Projection Generator

The rebuild wave: the full rich rider generalizes the W1 minimal cursor. Layout-driven
projection (SYNTHESIS §0.1 row 3). Candidates: **L3** (full rider) + **L8** (sparse-flag
side-table — the kind-disambiguation mechanism) + **L4** (tokenize-once reuse — consume
W3's structural index ONCE; if W3 has not landed, L4 reuses the W1 single-walk).

**Fit proof (the ≤650 LOC escape hatch, CH4-5).** The ≤450 source/test LOC default binds
unless the W2 plan presents, BEFORE redress, a pre-redress fit proof accepted by CHALLENGE:
a per-artefact LOC accounting (the four generated artefacts `document/value/view/visitor`
named separately from hand-edited source) demonstrating that the hand-edited generator +
tests cannot fit ≤450 because the `BackendRule`-walk recipe is intrinsically four
projection methods over N typed leaf kinds, with the over-450 lines attributed to a named
intrinsic cause (not scope creep). Without an accepted fit proof, ≤450 binds and a plan
exceeding it MUST split or return REVISE. The fit proof is itself ≤5 LOC of plan prose.

Owner paths:

- `skinny/crates/codegen/src/grammar_provider.rs` + `lower/{tape_plan,offset_tape,
  event_tape}.rs` (the accessor generator walking the `BackendRule` shape)
- generated `document/value/view/visitor` for the CSS grammars (named, diff-audited)
- `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}` (the existing `flag_cursors`/
  `flag_values` sparse pair `:93-113`, `flags_at` `:144-150` — USED for L8, not widened)
- `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected

Entry gate:

- W0/W1 admitted; tape activated; equality holds.
- W2 plan names the `BackendRule`-walk recipe (child-position → `ValueRef` child, branch
  tag → meta dispatch, typed leaf → decode by type, rule reference → child + recurse),
  the four generated artefacts, and the eager-tree population-parity baseline.
- CHALLENGE accepts: L8 flag bits are `BackendRule` branch-tag projections, NOT a
  hand-curated per-rule catalogue (§9 condition); the L1/L4 index, when consumed, IS the
  tape's `offsets`, never a parallel retained vector (§9 condition).

Tasks:

1. Write the layout-walk accessor generator emitting `document/value/view/visitor` per
   grammar from ONE `BackendRule`-walking generator — JSON the witness, CSS the first-mover
   rider — isomorphic to JSON's `value_from_ref` (`json/value.rs:143`), over the EXISTING
   `Tape`/`ValueRef`. The JSON rider's hand-written `value_from_ref` is RE-EMITTED through
   this generator and must stay byte-equal; the generator is grammar-generic, NOT a
   CSS-pinned emitter. NO new cursor/builder type.
2. Reconstruct the typed CSSOM (`CssColor/CssDimension/CssLength/CssFunction/Selector/
   CssRule/CssTypedValue`) by lazy `ValueRef` view — node kind recovered from the source
   byte at the offset (no stored tag), `PayloadArena` the bounded escape hatch for
   irreducible scalars only.
3. Store kind-disambiguation flag bits in the EXISTING sparse `flag_cursors`/`flag_values`
   pair (paid only where non-zero), each bit a `BackendRule` branch-tag projection.
4. Consume the structural index ONCE (L4); the index IS the tape (no parser-local second
   cursor).

Exit gate (MEASURABLE):

- `lazy_view_generated=true` (the generator emits the four artefacts over `BackendRule`).
- **JSON rider re-emits byte-equal THROUGH the new generator (R-CH2-1, load-bearing).** The
  JSON `value_from_ref` rider, regenerated by the new `BackendRule`-walking generator, is
  byte-equal to its hand-written form (diff of the regenerated JSON `value_from_ref` vs the
  committed file = empty), AND the JSON 51/51 behaviour is byte-identical. If the JSON
  rider's generated output changes — or if the JSON path is left untouched by a CSS-only
  generator (the generic-named-CSS-generator failure mode, CH2) — W2 FAILS. A
  `projection_generality_exercise ∈ {json, css_l4}` satisfied by a CSS-only generator that
  never re-emits JSON does NOT pass this gate.
- `css_rich_ast_preserved=true`: CSSOM via lazy `ValueRef`, value-plane population parity
  (dimensions/colors/functions/lists counts match the eager-tree baseline), NOT flattened,
  NOT eager (no per-leaf `Box::new`).
- `projection_generality_exercise ∈ {json, css_l4}` (both riders exercised; JSON exercised
  by the byte-equal re-emission above, CSS by the 8-field-equal typed CSSOM).
- Per-corpus typed-median Mbps emitted at N≥50 cold for all four corpora; the typed plane
  is **no worse than -2.0% median vs the W1 typed-tape baseline** (`track1_typed@W2(c) ≥
  -2.0%` vs `track1_typed@W1(c)`, N≥50 cold, all four corpora — the bench-falsifiable
  maintain band, P3-C §2.2/§3; W2 is a codegen-generality refactor, not a speedup wave, so
  the cross-bar threshold is deferred to W3, but the typed plane must not regress below the
  -2.0% band against W1). A bare 0% "no regression" floor does not bind — the -2.0% band is
  falsifiable against N≥50 median variance where a 0% floor is not.
- `css_typed_summary_equal=true` re-proven; JSON 51/51 maintained ±1.0%.
- Lock 14 + non-JSON (CSS L4) proof pass.

Same-wave consumer: the generated CSS projection (`value_from_ref`-isomorphic) is the
production reader of the W1 tape; L8 flags are read by L3 in the same wave.

Pre-blocked routes: eager materialization; L8 flag as a hand-curated per-rule catalogue
(the relocated-W5C overfit); the L1/L4 index retained as a parallel vector (REDRESS-53);
retained cursor / aux density / sidecar event vector; a second substrate; relocating
per-rule branching into projection DATA.

Revert protocol: revert the generator + generated `document/value/view/visitor` + flag
wiring as one slice; restore RESULTS; add REDRESS naming the projection axis that failed
parity.

Downstream effect: W2 rejection blocks W3 (no typed plane to accelerate). W2 disposition
informs the W3 NEON gate.

## Section 6 — W3 NEON Structural Index (Re-profiled)

Lever 3 (SYNTHESIS §3). **NEON is gated behind tape activation** — there is no structural
index to pre-scan into until the tape decodes CSS (SYNTHESIS §0.1 NEON gate). The wave
RE-PROFILES the benched tape path first; it does NOT inherit the core-tree
`find_component_delim ~56%` figure. Candidates: **L1** (eq-set byte-class classifier via
`select_classifier`) + **L5** (`comment_body_mask_64`, net-new) + **L6**
(`bracket_depth_mask_64`, net-new, scalar-balance default).

Owner paths:

- `skinny/crates/bbnf-simd/src/dispatch.rs:42,101` (`select_classifier`,
  `lo6_table_admissible` — the single neutral entry)
- `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33` (the eq-set fan
  — the CSS-admissible backend; NOT the lo6 `classify_tbl4`)
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs` (scalar twin)
- net-new `aarch64/comment_body_mask_64.rs` + `scalar/comment_body_mask_64.rs` +
  `tests/checkasm_comment_body_mask_64.rs` (L5)
- net-new `aarch64/bracket_depth_mask_64.rs` + `scalar/bracket_depth_mask_64.rs` +
  `tests/checkasm_bracket_depth_mask_64.rs` (L6)
- `skinny/crates/runtime/src/grammars/css_l4_*/` (the tape consumer of the `Vec<u32>`
  index)
- `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected

Entry gate:

- W0/W1/W2 admitted; typed plane exists.
- W3 plan presents the RE-PROFILE (N≥50) on the benched tape path naming the surviving
  scan leaf as top-N self-time AFTER the alloc floor fell — if no scan leaf survives as
  top-N, W3 does NOT land a NEON kernel (no orphan kernel) and the >SOTA gate is
  evaluated on the W2 plane.
- Per primitive: scalar reference + checkasm differential present BEFORE wiring.
- CHALLENGE accepts: the `Vec<u32>` index IS the tape's `offsets` (§9 condition); L6
  ships the scalar running-balance body (CTZ is consumer-only + parity-gated, NOT the
  default — REDRESS-89, §9 condition); the CSS path uses the eq-set fan, NOT lo6 (the
  `;{`→slot-59 `& 0x3f` collision); no udot/i8mm digit kernel (no CSS antecedent, §9).

Tasks:

1. Route the CSS structural scan through `select_classifier(alphabet)` producing a
   `Vec<u32>` structural index isomorphic to JSON's `scan_structurals` (`json/scan.rs:22`),
   backed by the eq-set fan; alphabet is the only grammar datum.
2. Land L5 (comment-region suppressor mask, digraph-parameterised, `overflowing_add`
   carry idiom — NOT PMULL) AND-NOTed into the index; L6 (bracket-depth balance, scalar
   running balance over the precomputed open/close masks, i32 `depth_carry` threaded
   WITHIN a single `scan_components_to_index` call, init-0-per-parse, never retained).
3. The tape consumes the index ONCE (L4 reuse, same substrate).

Exit gate (MEASURABLE — THE >SOTA gate):

- **≥1 regular corpus (animate OR bootstrap) crosses `delta_vs_lightningcss > 1.0×` at
  N≥50 cold median on the typed plane** (`css_comparator_plane=full-cssom`), with
  `css_rich_ast_preserved=true` and `css_typed_summary_equal=true` re-proven.
- tailwindcss benched cold N≥50: ADMIT if `delta_vs_lightningcss > 1.0×`, else REPORT the
  honest residual gap + the hot leaf in REDRESS (NOT tranche-blocking provided a regular
  corpus crosses).
- material-components-web per-corpus median reported (integration check).
- `native_simd_status ∈ {parity-pass, checkasm-pass}` per landed primitive;
  `simd_non_json_exercise=css_l4`.
- JSON 51/51 maintained ±1.0%; no second substrate; the index IS the tape.

Same-wave consumer: the tape's structural decode consumes the `Vec<u32>` index in the
same commit; scan + tape land together or neither.

Pre-blocked routes: x86/AVX/SVE; lo6 `classify_tbl4` on the CSS alphabet (silent-scalar
SIMD-win claim); PMULL default hot body (REDRESS-88); CTZ as the L6 default body
(REDRESS-89); the orphan udot/i8mm digit kernel (no benched CSS antecedent, §9);
FNV/hex as a primitive; cross-call classifier-state retention; a retained index vector
parallel to the tape (REDRESS-53).

Revert protocol: revert the SIMD kernels + scalar twins + checkasm + the tape-consumer
wiring + generated SIMD output as one slice; restore RESULTS; add REDRESS naming the
corpus that did not cross + the surviving hot leaf.

Downstream effect: if a regular corpus crosses, the tranche success criterion is met and
W4 is evaluated for whether it can lift a still-short corpus. If NO regular corpus
crosses, the tranche records the honest residual and escalates per PASS-ALPHA §8 (WARN);
W4 may still attempt the conditional commit-by-construction if its gate fired.

## Section 7 — W4 Commit-By-Construction Alt-Mode (CONDITIONAL)

Lever 4 (SYNTHESIS §3). **CONDITIONAL: this wave dispatches ONLY if the post-W1
typed-tape re-profile (N≥50) surfaces the recognition-control loop (un-masked by the
retired alloc floor) or a speculative-rollback leaf as top-N self-time** (L9 hard gate,
P2 §6 + §3 L9 condition; HARDENING-S-P2-V3 §3 L9). The re-profile is keyed to **post-W1**,
NOT post-W3: the S-P2 antecedent (HARDENING-S-P2-V3 §3 L9) is unmasked by the retired
alloc floor — which falls in W1 — NOT by the W3 scan collapse; so the L9 gate fires (if at
all) on the W1 typed-tape plane and W4 is conditional on W1, not W3. The LOCKED
28.87%+2.45% recognition-control figures are NOT a measured rollback antecedent — P1-E
measured ZERO speculative checkpoint/rollback self-time on either benched plane. If the
re-profile does NOT surface a rollback leaf, W4 does NOT dispatch; L9 is recorded as
not-needed (NOT a failure).

Candidate: **L9** (commit-by-construction Alt-mode codegen property), riding the
SK-V16-banked O(1) `offsets.len()` checkpoint / truncate rollback (no `split_off`, no
`Vec<Vec>` arena).

Owner paths:

- `skinny/crates/codegen/src/lower/tape_plan.rs` (the emitter Alt-mode property)
- `skinny/crates/runtime/src/grammars/css_l4_*/` (the recognizer spine — the consumer)
- `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`
- `skinny/REDRESS.md` if rejected

Entry gate:

- W1 admitted; the post-W1 typed-tape re-profile (N≥50) HAS surfaced the
  recognition-control loop or a speculative-rollback leaf as top-N self-time. If it has
  NOT, W4 does not dispatch.
- W4 plan names the Alt shape (pure-lexical keyword-dispatch Alts that deposit nothing
  structural), the O(1) checkpoint/truncate mechanism, and the byte-identical-tape parity
  proof.

Tasks:

1. Emit NO speculative checkpoint for pure-lexical keyword-dispatch Alts that deposit
   nothing structural; the spine commits as it scans, driven by the L1 index;
   backtracking survives only on true ambiguous leaves.

Exit gate (MEASURABLE):

- Byte-identical tape with/without the Alt-mode pass (recognizer-output equality).
- A measured lift of **≥ +5%** (N≥50 cold median, `track1_typed@W4 ≥ +5%` vs the W3 plane)
  on the gated corpus (the corpus the re-profile identified the recognition-control loop as
  hot on), AND any regular corpus that crossed at W3 stays crossed. A lift below +5%
  disposes L9 as NOT-WARRANTED (recorded measurably, not a failure) — a noise-band close on
  the conditional wave does not admit (P3-C §2.4/§3).
- JSON 51/51 maintained ±1.0%; `css_typed_summary_equal=true` held.

Same-wave consumer: the post-W1 CSS recognizer spine (the corpus the re-profile
identified) — the live consumer on the post-W1 profile, not a promised future consumer.

Pre-blocked routes: speculative-rollback admission without the re-profile antecedent;
`split_off`/`Vec<Vec>` arena; a non-byte-identical tape (a behavior change masquerading
as a control-flow optimization).

Revert protocol: revert the `tape_plan.rs` Alt-mode commit + generated output as one
slice; restore RESULTS; add REDRESS naming the corpus that did not lift.

Downstream effect: W4 disposition feeds W5 close.

## Section 8 — W5 Close, Clean Regen, Lock-14 Audit, Alpha Feedback

Owner paths:

- `restart/skinny/tranches/sk-v17/HANDOFF.md`
- a W5 close artefact under `restart/skinny/tranches/sk-v17/research/`
- `skinny/REDRESS.md` (close reconciliation)
- `skinny/RESULTS.md` only for documented-mismatch reconciliation without behavior change
- source only if a named Lock-14 cleanup (≤150 LOC) is in scope
- the 8 dirty generated files (clean regen)

Entry gate:

- W0–W4 each have admitted/rejected/routed status; their REDRESS/RESULTS/HANDOFF updates
  are present.

Tasks:

1. Cleanly regenerate the 8 dirty files; `cargo xtask regen --check` 9/9 exit 0.
2. Lock-14 audit of generic crates: no CSS/JSON policy, no renamed residue, no relocated
   `W5C_REQUEST_FACT_PROFILES`; CSS L4 non-JSON proof passes.
3. Reconcile every wave disposition; ensure RESULTS/REDRESS/HANDOFF agree.
4. Route residuals (tailwind gap if short; Sheets/BBNF-self projection generality) to
   SK-V18 / Pass Omega. Feed S-P3 lessons into the close note.

Exit gate (MEASURABLE):

- `dirty_generated_state=clean` (`regen --check` 9/9 exit 0).
- No CSS/JSON policy in generic crates; Lock-14 grep/audit clean; CSS L4 non-JSON proof
  passes.
- Every SK-V17 wave has admitted/rejected/routed status; final row/status artefacts match
  the latest wave evidence and `SK-V17-open` deltas.
- JSON 51/51 GO held; preserve-rich-ast intact; EXACT cssparser equality re-proven.
- The tranche success criterion (≥1 regular corpus crosses) is recorded TRUE, or the
  honest residual is recorded and escalated per PASS-ALPHA §8 (WARN) — NOT paper-closed.
- No accepted source change lacks a profile artefact, row threshold, REDRESS id, Lock-14
  proof, or same-wave-consumer proof.

Same-wave consumer: the close checklist + the `regen --check` gate + document
reconciliation.

Pre-blocked routes: paper close; missing REDRESS; missing RESULTS rows; deleting legacy
CSS generated/runtime shims before replacement proof landed; full-codegen close claims
while dirty generated CSS files remain; architecture analogy without row data; dropping
falsifier rows; corpus-average claim substituting for per-corpus medians.

Revert protocol: no source revert by default. Reopen the producing wave or mark close
blocked with a mismatch list naming file paths, rows, and missing evidence.

Downstream effect: on close, Pass Alpha dispatches the SK-V17→SK-V18 synthesis
(Sheets/BBNF-self tape-conversion + TOTALITY-fold) per PASS-ALPHA.

## Section 9 — Pre-Blocked Routes (the route ledger, per-wave-attributed)

Every wave inherits this ledger (SYNTHESIS §0.4). A route may reopen ONLY with fresh W0
evidence, a same-wave consumer, scalar/checkasm where relevant, a no-regression gate, a
REDRESS citation, and CHALLENGE acceptance. Hidden-coupling escapes reopen only through
Pass Omega + G-Omega (Lock 1).

Global blocks (all waves):

- **AZ-IV eager value tree** (the 118× regression): eager per-leaf payload,
  f64-alloc-per-number, per-color `Box<CssColor>`. Materialization stays lazy-by-default.
- **StructRegistry / Arena<G> / Builder<G> hot-path indirection** (28-65× bbnf/sheets,
  983× css bootstrap, 10583× WATCHDOG tailwind). No registry lookup in the per-leaf hot
  path.
- **CSS fact-stream String** as a live admission plane (`emit_fact_stream`/
  `emit_full_parse`/`CssFullParseSummary`, `generated.rs:5`): diagnostic-only.
- **The hand-coded `W5C_REQUEST_FACT_PROFILES` array** (`codegen/src/lib.rs:336`): the
  Lock-14-phrase-#1 construct. RETIRE it (derive routing from grammar/`BackendRule`),
  not extend, not relocate per-rule branching into projection DATA or flag form.
- **The 24-row broadcast** (one CSS timing tuple → N conceptual admits, RESULTS lines
  112-135): pre-blocked.
- **Fixture / FNV contrivances:** per-corpus hand-coded `real_typed.rs` fixtures,
  hand-tuned per-corpus capacity constants, FNV production selector/arbiter/correctness
  proof, FNV closed-enum production migration. FNV stays bench-only.
- **x86 / AVX-512 / SVE:** aarch64 only (Apple cores have no SVE).
- **Second substrate:** skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, public
  `UnionTape`, new substrate APIs, sixth `BackendShape`, retained sidecars, sidecar event
  vectors, retained cursor/list, cursor streams, aux density/projection tables,
  parser-owned structural projections/streams, parallel source passes, cross-call
  classifier-state retention.
- **Wrong-plane comparator admission:** cssparser token-scan as a SOTA bar; lightningcss
  comparison before Track 1 emits comparable CSSOM; brace-counter proof as CSS admission.
- **Wrong-tree dishonesty:** keying a tape/layout gate on `crates/core/` rather than
  `skinny/crates/`.
- **Paper close:** "wired"/"integrated" without a bench-row threshold; deleting legacy
  CSS shims before replacement proof; full-codegen close while dirty generated files
  remain.

Per-wave attributions:

| Wave | Must NOT re-open (load-bearing) |
|---|---|
| W1 | AZ-IV eager; fact-stream as admission; W5C array (RETIRE not extend/relocate); StructRegistry indirection; 24-row broadcast; second substrate; REDRESS 50-55, 60-72 |
| W2 | eager materialization; L8 flag as hand-curated per-rule catalogue; L1/L4 index as parallel retained vector (REDRESS-53); retained cursor / aux density / sidecar event vector |
| W3 | x86/AVX/SVE; lo6 on CSS alphabet; PMULL default body (REDRESS-88); CTZ as L6 default (REDRESS-89); orphan udot/i8mm digit (no antecedent); FNV/hex primitive |
| W4 | speculative-rollback admission without the re-profile antecedent; `split_off`/`Vec<Vec>`; non-byte-identical tape |
| W5 | paper close; missing REDRESS/RESULTS; legacy-shim deletion before proof; corpus-average substituting for per-corpus medians |

Inherited REDRESS pre-block families (semantics carried, not just ids): 28+33, 50-55,
60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum
production migration.

Binding shortlist conditions (P2 §6, carry verbatim — a shortlisted candidate that
violates one CH-REJECTs at the wave):

1. **L1/L4 index == tape-offsets identity.** The produced `Vec<u32>` IS the tape's
   `offsets`; carry/depth threads WITHIN a single `scan_components_to_index` call, reset
   per parse. A retained parallel index collapses into REDRESS-53.
2. **L8 flag bit = `BackendRule` branch-tag projection.** A hand-curated per-rule
   catalogue is the relocated-W5C overfit (CH2 REVISE).
3. **L2/L3 routing derived-from-grammar.** `W5C_REQUEST_FACT_PROFILES` RETIRED; every
   residual CSS routing entry names its `.bbnf` rule; relocating per-rule branching into
   projection DATA is the Lock-14-phrase-#1 re-entry seam (FORBIDDEN).
4. **L6 scalar-balance default.** The CTZ-ranges path is consumer-only + parity-gated;
   promotion to the default body re-opens REDRESS-89.
5. **L9 hard post-W1 re-profile obligation.** Admit L9 as active ONLY if a post-W1
   typed-tape re-profile (N≥50) surfaces the recognition-control loop or a
   speculative-rollback leaf as top-N; the LOCKED 28.87%+2.45% recognition-control
   figures are NOT a measured rollback antecedent.

REJECTed candidates barred from the active shortlist (P2 §4): CF-4a/C5/C-B3/G4 orphan
udot digit · CF-4b/C6 net-new i8mm digit · FNV/hex · asmjson collapsed-stage FSM (x86,
host-blocked) · lo6 `classify_tbl4` on the CSS alphabet · D6 second substrate.

## Section 10 — G-Alpha And Dispatch Scope

Per SYNTHESIS authority: only G-Omega is mandatory during this execution; G-Alpha
auto-passes. Dispatch scope:

- **W0 is authorized** (baseline + telemetry + lightningcss CSSOM re-baseline; 0 behavior
  LOC).
- **W1–W5 remain conditional.** Each requires its prior wave's closure, this packet's
  per-wave entry gate, the CHALLENGE acceptance named in the wave's entry gate (W1/W2/W3
  are first-of-class and substrate/primitive-touching → CHALLENGE required), and
  orchestrator/user dispatch of the wave triumvirate before redress.
- **W4 is doubly-conditional:** on W1 close AND on the post-W1 re-profile firing the L9
  gate. If the re-profile does not surface a rollback leaf, W4 does not dispatch.
- No SK-V17 implementation wave dispatches from S-P3 or S-P2 alone.

Each wave is executed by the wave triumvirate (research → plan → redress, distinct
commits) per `SKINNY-TRIUMVIRATE.md`, with the six-lens CHALLENGE interposed for the
first-of-class interventions (W1 tape activation, W2 projection generator, W3 NEON
primitives). S-P3 produces this SPEC; it does not run the waves. On close, Pass Alpha
dispatches the SK-V17→SK-V18 synthesis.

The work is bounded by the bench. The plan is bounded by the goalset. The waves are
bounded by this SPEC. The SPEC is the contract.
