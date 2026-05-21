# SK-V13 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-21.
Scope: Measurable falsifiability gates for the SK-V13 wave families.
Output: this file.
Pass Alpha goalset: G1 full CSS L4 lightningcss strict parity, G2 decision-engine fold with JSON regression-free cascade retirement, G3 at least one union variant admitted or architecturally blocked, G4 zero aarch64 orphans, G5 all 51 JSON rows above sonic-rs strict on their own plane, G6 totality V1.1 ratified before Wave 0, and G7 no silent demotion with rolling SOTA delta.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

P3-C binds gates, not implementation scope. The S-P3 prompt requires every
wave gate to be measurable from bench rows, comparator Mbps, strict equality,
full-table maintain budgets, and a revert protocol. The SK-V13 SYNTHESIS §0
overrides weaker earlier concessions: every CSS non-OUT_OF_SCOPE feature must
be `ADMITTED-PARITY` or architecturally blocked, every JSON corpus/plane must
beat sonic-rs strict by more than 1 Mbps, and no wave can close on support-only
or future-consumer work.

The current profile facts stay non-admissive. S-P1 marks every row as
`profile_signal_not_gate_admission`; parse/direct hot leaves are mostly JSON
generated envelopes, typed coverage is only 7/17, CSS declaration-values is
timer/fact-sink dominated, and structural SIMD scan is a scanner micro-result
only. S-P2 therefore allows primitives only when they carry scalar reference,
checkasm/parity where relevant, grammar-neutral policy, a same-wave production
consumer, and named row movement.

The strict comparator plane is the control surface:

- CSS admissions compare Track 1 against lightningcss strict on the same
  `css_l4_*_fact_stream` plane with strict equality and an independent
  cssparser or golden oracle. REDRESS-125/127 prove the one existing admitted
  row `css_l4/declaration_values/direct_to_struct/main`; they do not cover the
  remaining feature matrix.
- JSON admissions compare Track 1 against sonic-rs strict on the same corpus
  and output plane. Lossy sonic-rs, RapidJSON, historical C++ sidecars, and
  different output planes are planning signals only.
- JSON Track 2 is correctness/independence evidence. It cannot be used as the
  SOTA anchor, cannot call Track 1, and cannot hide Track 1 demotion.
- `skinny/RESULTS.md` is the current source for measured JSON and the admitted
  CSS row. Missing typed rows and new CSS rows have no current Mbps in checked
  authority; their first strict same-run wave report becomes the source only
  after the report includes run id, host, equality artifact, comparator Mbps,
  and gate-consumed provenance.

The candidate pool that survives S-P2 maps to these gate families:

| Gate family | Candidate evidence carried forward |
|---|---|
| CSS parity | P2-F CSS row scopes: `stylesheet_and_selectors`, `declaration_values_extended`, `visual_functions`, `at_rules_and_media`, `nested_rules_and_queries`, `vendor_and_custom_atrules`; P2-A/P2-B/P2-C/P2-E allow `ByteSetRunSkip64` only with same-wave CSS scan-block measurement. |
| JSON strict +1 | P2-A sonic-rs strict comparator teardown; P1-A/P1-B/P1-F row inventory; REDRESS-119/120 are history only under the addendum. |
| Decision engine | Decision scoping W5-W9 sequence: `bbnf-regex`, e-graph Language, active `egg::CostFunction`, CSP resolver, cascade deletion; P2-F accepts only output-piped, grammar-neutral resolver pieces. |
| Union | P2-D D2/D3/D4 and value-union C1/C2/C3, conditional on REDRESS 96/97/98 material differential and single-substrate proof. |
| SIMD/ASM | P2-B/P2-C/P2-E candidates: `ByteSetRunSkip64`, `unescape_uxxxx_x4`, string-special 64, digit-run UDOT, grammar structural scan; Lock 16 and REDRESS-126 zero-orphan evidence are binding. |
| Telemetry/rolling delta | SK-V13 SYNTHESIS §2-§3 and USER PIN rolling status require full schema consumption and `restart/skinny/ROLLING-SOTA-DELTA.md` for all JSON rows and CSS features. |

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

### Global gate constants

These predicates apply to every behavior wave unless a stricter per-wave gate
is named:

| Symbol | Formula |
|---|---|
| `json_admit(row, plane)` | `Track1_mbps(row, plane) > sonic_rs_strict_mbps(row, plane) + 1.0` with `Strictness=strict`, `parse_utf8=measured-row`, `escape_complete=yes`, same corpus, same output plane, same host, same run family. |
| `css_admit(feature)` | `Track1_mbps(feature) > lightningcss_strict_mbps(feature) + 1.0` with strict equality against lightningcss and independent cssparser/golden oracle on the same fact stream. |
| `row_move_toward_sota(row)` | `margin_after > margin_before`, where `margin = Track1_mbps - SOTA_strict_mbps`; minimum measurable movement is `max(1.0 Mbps, 0.01 * T1_before)` unless the row reaches `json_admit` or `css_admit`. |
| `json_full_table_maintain` | Every non-target JSON row with a current value in `skinny/RESULTS.md` keeps `Track1_after >= 0.98 * Track1_before` and `Track2_after >= 0.98 * Track2_before`, with no verdict downgrade, no strictness downgrade, and no `measured-row -> view-boundary` demotion. |
| `css_admitted_maintain` | Every previously admitted CSS row keeps strict equality and `Track1_after >= max(lightningcss_after + 1.0, 0.98 * Track1_before)`. |
| `strict_guard` | A strict admission rejects if comparator strictness, output plane, run id, host, feature mask, equality artifact, or measured validation path is missing or mismatched. |
| `support_only_reject` | A primitive, resolver, union, or codegen path with no same-wave measured row consumer is rejected even if tests pass. |

`before` is the current SK-V13-open row captured by W0. Until W0 captures
`SK-V13-open`, use `skinny/RESULTS.md` only as seed authority. If W0 changes
any Track 1, Track 2, sonic-rs, lightningcss, or oracle value, every downstream
threshold recomputes from W0; copying pre-W0 numbers is a gate failure.

### W0 — G-Omega, SK-V13-open, and telemetry lock

Entry gate:

- G-Omega is closed; no implementation wave may start before totality V1.1
  ratification.
- S-P3 has converged and the SPEC names exact owner paths for W0.

Named rows/features:

- All 51 JSON conceptual rows: 17 corpora x `parse_only`,
  `direct_to_struct`, `real_typed_struct`.
- The admitted CSS row:
  `css_l4/declaration_values/direct_to_struct/main`.
- Every non-OUT_OF_SCOPE CSS feature row to be produced later by W10.N.

Exit gate:

- `SK-V13-open` captures Track 1, Track 2, strict comparator Mbps, strictness,
  output plane, equality artifact, profile/hot-leaf artifact, run id, host,
  build flags, feature mask, wave id, and REDRESS id/status for every existing
  row in `skinny/RESULTS.md`.
- Missing rows are explicit: the ten currently missing typed product surfaces
  from P1-F must be rendered as `missing-product-surface`, not silently absent.
- `gate-json` rejects missing required schema fields, stale run ids, lossy or
  permissive SOTA anchors, sidecar-only comparators, producer-only telemetry,
  and strict admissions with view-boundary validation.
- Behavior drift is zero by default: any current throughput cell changes by
  more than +/-1.0% without an explicit W0 measurement explanation rejects W0.

Revert protocol: revert W0 report/gate/schema/RESULTS/rolling-delta changes as
one slice, restore the pre-W0 results surface, and record a W0 REDRESS
rejection naming the missing field, stale comparator, or row. W0 rejection
blocks all behavior waves.

### W10.N — CSS lightningcss+1 parity gates

Each CSS row-production wave selects exactly one row family unless P3-B proves
the selected feature group fits in one redress cap with one fact stream,
one comparator lane, and one revert slice.

Named row families and features:

| Wave row | Feature names covered | Required source for current values |
|---|---|---|
| `css_l4/stylesheet_and_selectors/direct_to_struct/main` | stylesheet root, selectors, pseudo-classes, pseudo-elements, attribute selectors | New wave report; no current Mbps exists. |
| `css_l4/declaration_values_extended/direct_to_struct/main` | declarations, CSS variables, calc expressions, var/url functions, color functions | Existing admitted seed only covers token values in `skinny/RESULTS.md`; extension Mbps must come from new wave report. |
| `css_l4/visual_functions/direct_to_struct/main` | gradients, transforms, filters, easing functions | New wave report; no current Mbps exists. |
| `css_l4/at_rules_and_media/direct_to_struct/main` | at-rules, media queries, keyframes/support/import taxonomy selected by the wave | New wave report; no current Mbps exists. |
| `css_l4/nested_rules_and_queries/direct_to_struct/main` | nested rules; container/scope only if P3-F marks them non-OUT_OF_SCOPE | New wave report; no current Mbps exists. |
| `css_l4/vendor_and_custom_atrules/direct_to_struct/main` | vendor prefixes, custom at-rules | New wave report; no current Mbps exists. |

CSS admission gate:

- Strict equality: generated Track 1 fact stream equals lightningcss strict
  fact stream byte-for-byte; equality artifact includes stream hash and input
  checksum.
- Independent oracle: cssparser oracle or hand-checked golden table for
  productions cssparser cannot cover.
- Throughput: `Track1_mbps > lightningcss_strict_mbps + 1.0`.
- Coverage parity: for the feature variants selected by the row, every
  variant accepted by lightningcss is accepted and every variant rejected by
  lightningcss is rejected under the same strict mode.
- JSON guard: `json_full_table_maintain` holds whenever the wave touches
  generic runtime, codegen, generated output, gate/report code, or SIMD.
- Existing CSS maintain: `css_l4/declaration_values/direct_to_struct/main`
  remains strict-equal and above `lightningcss + 1`.

SIMD inside CSS rows inherits `G-SIMD-GRAMMAR-POLICY`: generated delimiter,
quote, escape, control, and no-string policy must be supplied by the CSS
consumer; scalar/checkasm and same-wave row measurement are mandatory.

Revert protocol: revert generated CSS outputs, codegen templates, runtime
modules, comparator/oracle code, gate/report changes, RESULTS/rolling-delta
edits, and SIMD consumer wiring as one slice. Preserve equality artifacts and
failed feature table in research; append REDRESS with the exact feature row and
the first missed predicate. No CSS feature may remain `PARTIAL` at close.

### W11.N / W14.N / typed reopen — JSON sonic-rs strict +1 gates

The JSON gate is all-plane, not limited to historic N-direct rows.

Corpus set:

`twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`,
`update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`,
`numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`,
`distinct_values`, `y_string_unicode`.

Plane set:

- `parse_only` -> DOM/borrowed-view parse plane, re-pinned as admission
  eligible.
- `direct_to_struct` -> digest/direct plane, including the REDRESS-119
  historic residual family.
- `real_typed_struct` -> typed product plane, including ten currently missing
  generated product surfaces.

Named historic direct reopen rows:

- Prior 3-row shortlist carried by the addendum:
  `twitter/direct_to_struct`, `github_events/direct_to_struct`,
  `update_center/direct_to_struct`.
- Ten equally reopen-eligible rows from the addendum:
  `canada/direct_to_struct`, `mesh/direct_to_struct`,
  `random/direct_to_struct`, `gsoc-2018/direct_to_struct`,
  `instruments/direct_to_struct`, `numbers/direct_to_struct`,
  `unicode_mixed/direct_to_struct`, `unicode_escapes/direct_to_struct`,
  `distinct_values/direct_to_struct`, `y_string_unicode/direct_to_struct`.
- Current legacy `A/GO` direct rows are still re-evaluated under G5; a legacy
  GO label is not close evidence unless the row satisfies strict +1 now.

Admission gate per JSON row:

- `json_admit(corpus, plane)` passes.
- Strict validation happens inside the measured row, not at view boundary.
- Track 1 is the generated/runtime path being admitted.
- Track 2/oracle is structurally independent: it cannot call generated Track 1,
  generated SinkOnly helpers, generated typed helpers, or a shared
  benchmark-private parser.
- For direct rows, REDRESS-119 is cited as history and the wave names the fresh
  material differential: resolver-selected shape, generated direct sink/event
  adapter, unicode/string/number policy primitive, fact-stream digest, or other
  S-P2-accepted differential.
- For typed rows absent from `skinny/RESULTS.md`, exact current Mbps is
  unavailable. Required source is the first strict same-run typed report that
  adds the row, with formula `Track1_mbps > sonic_rs_strict_typed_mbps + 1.0`
  and independent Track 2/oracle proof.
- Full-table maintain: `json_full_table_maintain` and `css_admitted_maintain`
  hold.

Revert protocol: revert JSON runtime/codegen/generated outputs, parse-that or
bbnf-simd consumers, bench/report/gate changes, RESULTS/rolling-delta edits,
and any row tables as one slice. Failed generated rows may remain only if
disabled and explicitly marked rejected with no production consumer. Append
REDRESS with before/after Track 1, Track 2, sonic strict, equality status, and
the missed threshold.

### W5-W9 — decision-engine JSON regression-free and cascade retirement gates

The decision-engine fold is a sequence; no subwave may close on "wired" or
"integrated." Each subwave must either have no behavior drift or produce a
measured row movement under the JSON/CSS gates above.

Named subwave gates:

| Subwave | Gate |
|---|---|
| `bbnf-regex` extraction | `cargo test -p bbnf-regex` or SPEC-named equivalent passes; IR/passes/codegen have zero local hardcoded regex predicate helpers replacing `bbnf_regex::*`; JSON and CSS maintain gates pass. |
| E-graph Language + rewrites | `saturate_and_extract` produces candidates on the named JSON and CSS grammars; each rewrite has an equivalence test; e-graph memory stays below the SPEC wave budget; OOM is abrogate-before-patch. |
| Active cost function | `egg::CostFunction` is used for extraction; `missing_or_stale_cost_exprs / candidate_exprs <= 0.30`; deterministic winner on repeated runs; stale-cost ratio over 30% rejects or abrogates the cost subwave. |
| CSP resolver | CSP solve time is `<= 1.0s` per named grammar; timeout or UNSAT produces visible rejection/non-admission, never silent fallback; CSP and e-graph compose by output-piping, not a fused hidden solver. |
| Cascade retirement | `choose_backend_shape`, `PRIORITY_TABLE`, and hardcoded P1-P8 fallback are deleted or fail closed; after retirement, JSON/CSS/Sheets/BBNF-self cannot silently route through the old cascade. |

JSON regression-free gate:

- Every subwave runs `json_full_table_maintain` over all current JSON rows.
- Any row the resolver claims to improve must pass `json_admit` or
  `row_move_toward_sota`; otherwise the subwave is a measured reject.
- No legacy cascade result may be presented as resolver admission after the
  cascade retirement subwave.
- Lock 14 proof is mandatory for generic crate edits. Fleet-wide
  grammar-neutral claims require CSS L4 plus both Sheets and BBNF-self
  fail-closed, compile/lower/cost, unchanged-output, or generated-role fact-row
  witnesses. CSS L4 plus only one of Sheets or BBNF-self is scoped non-JSON
  evidence and cannot close a fleet-wide grammar-neutral claim.

Abrogate-before-patch criteria:

- E-graph OOM on named grammar.
- CSP solve time over 1.0s per grammar after timeout/fallback attempt.
- Stale or missing cost evidence over 30% of candidate expressions.
- Rewrite order changes final extraction cost by more than 10%.

Revert protocol: keep old cascade feature-gated until the cascade-retirement
gate passes. If a subwave fails before retirement, revert the subwave's
resolver module and report/gate changes only. If cascade retirement fails,
revert the retirement slice and record REDRESS; do not admit fallback behavior
as a success path. If JSON or CSS maintain fails, revert the whole producing
decision-engine slice and append REDRESS naming the first regressed row.

### W8/W12 — union material-differential gates

Union is unblocked only as a same-substrate, row-consumed attempt. REDRESS
96/97/98 are mandatory citations and not category blockers.

Allowed material differentials:

| Differential | Gate name | Rejected history it must distinguish |
|---|---|---|
| Codegen-time per-rule same-tape event projection | `union-c1-per-rule-same-tape` | Not REDRESS-96 class column, not REDRESS-97 streaming cursor, not REDRESS-98 class-lane paper close. |
| E-graph equivalence-class shape selection | `union-c2-egraph-shape` | Not ad hoc per-rule shape; must include equivalence proof and bounded resolver cost. |
| SIMD-first mask-to-tape writer | `union-c3-simd-mask-to-tape` | Not PMULL default hot-body REDRESS-88 or CTZ/bulk REDRESS-89; SIMD writes the active tape/sink projection directly. |

Union admission gate:

- Exactly one retained substrate survives: `Tape`/`ValueRef` or sink-only output
  as currently authorized. No `UnionTape`, public substrate API, parser-owned
  cursor/list, aux density table, sidecar event vector, retained class vector,
  or second source scan.
- Same-wave consumer: a generated CSS row, JSON parse/direct/typed row, or
  other SPEC-named row consumes the union projection in production.
- Row movement: the consumer row reaches `css_admit`/`json_admit` or satisfies
  `row_move_toward_sota` with no guard regression. G3 close requires at least
  one union variant admitted or an architectural-level intrinsic block; a mere
  positive microbench is not enough.
- If SIMD feeds the projection, the SIMD gate below passes first.
- Full-table maintain: `json_full_table_maintain` and `css_admitted_maintain`
  hold.

Revert protocol: revert union runtime/codegen/generated outputs, SIMD producer
if included, gate/report changes, RESULTS/rolling-delta edits, and any new
union policy tables as one slice. Delete or demote any orphaned primitive.
Append REDRESS citing 96/97/98 and the failed material differential.

### SIMD/ASM — zero-orphan and checkasm gates

Every SIMD or ASM attempt, including CSS `a64_ascii_set_run_skip` production
wiring, must pass this gate before row admission.

Primitive gate:

- Scalar reference exists and is executable.
- Candidate-specific checkasm/differential tests run with
  `BBNF_SIMD_STRICT=1`.
- Required common coverage: empty/single/tail windows, alignments 0-15, dense
  and sparse masks, all-zero/all-one, invalid/error cases, grammar-specific
  byte policies, feature-mask disclosure, stack/callee-saved/fault checks where
  the local harness supports them.
- Corpus parity covers the target grammar policy: JSON quote/escape/control,
  CSS delimiter/comment/string policy, Sheets/BBNF policy if named.
- Same-wave production consumer is named and measured.
- `samply` or equivalent symbol-path evidence shows the consumer calls the
  candidate on the affected row when the wave claims hot-path movement.

Named checkasm surfaces from S-P2:

- `checkasm_escape_mask_64` and `checkasm_parity` for escape/string scanner
  prerequisites.
- `checkasm_ascii_set_member_find_64` for `a64_ascii_set_run_skip` /
  `ByteSetRunSkip64`.
- `checkasm_byte_class_from_eq_set_64` for byte-set classifier consumers.
- Candidate-specific suites for `unescape_uxxxx_x4`, string-special 64,
  digit-run UDOT, PMULL/CSSC structural union, prefix/next/bulk bitmap routes.

Zero-orphan gate:

- `orphan_count_after = 0`, where an orphan is any aarch64 production primitive
  or generated SIMD path with no production consumer and no explicit REDRESS
  demotion/deletion evidence.
- The five REDRESS-126 demoted inventory rows remain history only:
  `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
  `byte_context`, `cache_hints`.
- `a64_ascii_set_run_skip` has a one-time SK-V13 production split allowance.
  It must wire into a CSS scan-block consumer and meet CSS row/equality gates
  or be rejected; no second production-split deferral is permitted.

Production throughput gate:

- A primitive cannot admit on microbench speed alone.
- For CSS production wiring, target row must hold strict equality and
  `Track1_mbps > lightningcss_strict_mbps + 1.0`; if the wave's only claimed
  value is replacing an already admitted scalar CSS path, it must additionally
  show `Track1_after >= 1.01 * Track1_before` or record measured reject.
- For JSON production wiring, target row must satisfy `json_admit` or
  `row_move_toward_sota` with `json_full_table_maintain`.

Revert protocol: revert primitive body, dispatch table, generated consumer,
tests that only support the failed primitive, bench/report/gate changes, and
RESULTS/rolling-delta edits together. If the primitive file remains, it must
be demoted/deleted with REDRESS evidence so `orphan_count_after` is still zero.

### Telemetry and rolling delta gate

Every wave that touches source, generated output, comparator, report/gate code,
or row data must refresh telemetry.

Required artifacts:

- `skinny/RESULTS.md` or gate-consumed report payload with the SK-V13 schema.
- `restart/skinny/ROLLING-SOTA-DELTA.md` with one row per 51 JSON row/plane and
  every CSS feature:
  `| row | plane | T1_current | T1_sota | margin | tranche_admitted |`.
- Wave REDRESS entry on admit or reject, except pure planning waves.

Gate formulas:

- JSON rolling `T1_sota = sonic_rs_strict_mbps` on same corpus/plane.
- CSS rolling `T1_sota = lightningcss_strict_mbps` on same feature/fact plane.
- `margin = T1_current - T1_sota`.
- `tranche_admitted = true` only if the row passes `json_admit` or `css_admit`
  and equality/provenance gates.
- Any row whose `margin_after < margin_before` fails G7 unless the wave records
  architectural-level intrinsic block evidence and user re-pin.
- Missing rows fail close unless marked with architectural block; missing typed
  rows cannot disappear from the rolling table.

Revert protocol: if telemetry schema, gate consumption, or rolling delta is
wrong, revert telemetry/report/RESULTS/rolling edits and block downstream
admission. Do not preserve source behavior changes whose only evidence is a
malformed report.

### Wn — close and bracket gate

SK-V13 close is a measurement gate, not a synthesis assertion.

Close passes only when:

- G1: every non-OUT_OF_SCOPE CSS feature is `ADMITTED-PARITY` by `css_admit`
  or carries architectural-level intrinsic-block proof.
- G2: decision-engine fold has landed, the old cascade is retired or fails
  closed, and JSON/CSS guard rows are regression-free.
- G3: at least one union variant is admitted by the union gate or the tranche
  records architectural block.
- G4: `orphan_count_after = 0`.
- G5: all 51 JSON rows pass `json_admit` or carry per-row architectural block.
- G6: totality V1.1 / G-Omega is ratified.
- G7: rolling delta shows no silent demotion.

If any item remains unmet without architectural-block proof, close is REJECT
and Pass Alpha brackets SK-V14 immediately under the same pinned bar. The close
wave may reconcile docs and evidence, but it may not invent a post-hoc
admission or modify source to make a close table pass.

Revert protocol: no source revert by default in close. Reopen the producing
wave if a mismatch is discovered; otherwise close REJECT and bracket forward.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

The gates are falsifiable because every admission reduces to a named row and a
strict Mbps formula.

### JSON binding

Named corpus rows:

| Plane | Rows |
|---|---|
| `parse_only` | `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode` |
| `direct_to_struct` | same 17 corpus names; REDRESS-119 direct residual history must be cited for historic residuals. |
| `real_typed_struct` | same 17 corpus names; ten rows currently lack generated typed product surfaces and must use the first strict same-run typed report as current-value source. |

Threshold:

```text
json_admit(corpus, plane):
  Track1_mbps(corpus, plane, strict, same-run)
    > sonic_rs_strict_mbps(corpus, plane, same-run) + 1.0
```

Required source for current numbers:

- Existing rows: `skinny/RESULTS.md` until W0 captures `SK-V13-open`.
- Missing typed rows: P1-F `missing-product-surface` is the current status;
  exact Mbps is unavailable until the wave adds the strict same-run typed row.
- Post-W0 rows: `SK-V13-open` and the wave's gate-consumed report, not copied
  pre-W0 numbers.

### CSS binding

Named CSS feature rows:

| Row | Admission formula |
|---|---|
| `css_l4/declaration_values/direct_to_struct/main` | Already admitted from SK-V12; maintain `Track1 >= max(lightningcss + 1.0, 0.98 * SK-V13-open Track1)` and strict equality. |
| `css_l4/stylesheet_and_selectors/direct_to_struct/main` | `Track1 > lightningcss + 1.0` plus strict equality and cssparser/golden oracle. |
| `css_l4/declaration_values_extended/direct_to_struct/main` | Same. |
| `css_l4/visual_functions/direct_to_struct/main` | Same. |
| `css_l4/at_rules_and_media/direct_to_struct/main` | Same. |
| `css_l4/nested_rules_and_queries/direct_to_struct/main` | Same, or architectural block for any SPEC-declared out-of-scope subfeature. |
| `css_l4/vendor_and_custom_atrules/direct_to_struct/main` | Same. |

Exact current Mbps is unavailable for new CSS feature rows because they do not
exist in `skinny/RESULTS.md`. Required source is the wave's strict same-run
report with generated Track 1, lightningcss strict, cssparser/golden oracle,
fixture hash, stream hash, run id, host, and REDRESS id.

### Maintain budgets by wave family

| Wave family | Full-table maintain budget |
|---|---|
| W0 telemetry | Existing throughput cells within +/-1.0%; no behavior/source drift. |
| CSS W10.N | All admitted CSS rows keep strict equality and `lightningcss + 1`; JSON maintain budget applies if generic/runtime/codegen/report paths move. |
| JSON W11/W14/typed | Non-target JSON rows no worse than -2.0% Track 1/Track 2; no verdict, strictness, UTF-8, equality, or Track 2 independence downgrade. |
| Decision-engine W5-W9 | No behavior drift for extraction/scaffold subwaves; behavior subwaves use JSON/CSS gates and full-table maintain. Cascade retirement requires no old fallback. |
| Union W8/W12 | Target row reaches SOTA or improves margin; all non-target JSON/CSS admitted rows maintain. |
| SIMD/ASM | Target row reaches SOTA or improves margin; zero orphan; all non-target rows maintain. |
| Telemetry/rolling | No admission if schema/rolling table is missing or stale. |
| Close | No performance rerun unless mismatch found; mismatch reopens producing wave or brackets forward. |

Variance rule: a row below floor fails. One confirm rerun is allowed only inside
the wave rerun ceiling and hard cap. Extra reruns are REDRESS cost evidence,
not retry room.

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

Every wave inherits the REDRESS guardrails below. A wave can reopen a family
only with a named material differential, same-wave consumer, strict comparator
gate, and challenge acceptance.

| Wave family | Pre-blocks |
|---|---|
| CSS | Claiming SK-V13 close from the single SK-V12 declaration-values row; microbench-only CSS delimiter proof; CSS parser hot-leaf claims from timer/fact-sink P1 profile; generic JSON policy in CSS runtime/codegen. |
| JSON | REDRESS-119/120 as close authority; lossy/permissive comparators; direct digest as typed proof; Track 2 coupling; view-boundary strictness; support-only generated rows. |
| Decision engine | Hardcoded P1-P8 fallback after resolver retirement; fused CSP/e-graph hidden solver; stale cost over 30%; resolver scaffold with no row consumer; grammar-name branches in generic crates. |
| Union | REDRESS 96/97/98 class column, streaming cursor, and class-lane-only variants; parser-owned cursor/list/facts; `UnionTape`; sidecar event vectors; aux density tables; second source scan. |
| SIMD/ASM | REDRESS 28/33 tiny-string NEON replay; REDRESS 82-84 unicode/StringBlock/object-pair proof-only replays; REDRESS 88 PMULL default hot body; REDRESS 89 CTZ/bulk standalone route; REDRESS-126 orphan retention. |
| Telemetry | Producer-only telemetry, stale sidecars as anchors, missing rolling row, missing REDRESS id, and historical deltas as admission evidence. |
| Close | Paper close, partial CSS, implementation-limited misses, missing architectural-block proof, or deferral to SK-V14 without immediate Pass Alpha bracket. |

Specific REDRESS anchors carried into every relevant plan:

- REDRESS 28 and 33: Class A/tiny-string NEON paths are not parser-close
  evidence.
- REDRESS 50-55 and 60-72: no parser-local side tables, event cursors,
  decoded-string stats sinks, quote-source streaming hashers, or direct
  materialization shortcuts under new names.
- REDRESS 80: no one-row `canada` mantissa-widen/raw float shortcut.
- REDRESS 82-84: no single-quartet unicode, StringBlock16 tiny probe, or
  object-pair value-byte control compaction replay.
- REDRESS 88/89/90: PMULL/CSSC/bulk routes require a fresh consumer and row
  movement; support bodies alone are rejected.
- REDRESS 96/97/98: union category is unblocked only with material
  differential and one-substrate proof.
- REDRESS 119/120: direct fixpoint history is lifted under the user pin, but
  prior failed-route evidence remains mandatory context.
- REDRESS 121-127: GrammarConfig legality, escape-mask proof, CSS comparator,
  CSS admit, zero-orphan, and SK-V12 close evidence remain guardrails.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/prompts/ORCHESTRATOR.md` §3, §3W, §3Z
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
