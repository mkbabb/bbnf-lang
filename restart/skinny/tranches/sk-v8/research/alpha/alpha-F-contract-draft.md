# Alpha-F Contract Draft: SK-V7 to SK-V8

Date: 2026-05-16.

Scope: alpha-F draft only. This is a content outline for the SK-V8 tranche
documents under the current tranche layout:

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`

This file is not the final SK-V8 contract and must not be used to dispatch
SK-V8 directly. It records the contract shape that the final Alpha output
should materialize after Alpha A-E and CHALLENGE convergence.

## 1. Authority Read

This draft is bound to these inputs:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `docs/precepts/instructions/tranche/SPEC.md`
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/HANDOFF.md`
- `restart/skinny/tranches/sk-v7/DISPATCH-PROMPT.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- latest git log through `56e66ef5 feat(sk-v7-wave10c): admit B6 stack-canary Stage 1`

Latest log posture:

| Commit | Disposition |
|---|---|
| `56e66ef5` | W10c admitted B6 stack-canary Stage 1 only. |
| `0cd00886` | W10b rejected CTZ bulk consumer plus B6 canary fold. |
| `db913136` | W10 rejected consumed aarch64 bitmap bodies plus B6 canary fold. |
| `51d8c8be` | W9 admitted CostFacts substrate projection. |
| `7c6837b8` | W8 admitted Lock 14 phase C+D shell neutralization. |
| `f786e597` | W7 admitted Lock 14 phase A+B neutralization. |
| `58479e29` | W6 rejected object-pair value-byte control compaction. |

## 2. Corrected SK-V7 Diagnosis

SK-V8 must open from the measured SK-V7 close, not from the original SK-V7
forecast.

Measured SK-V7 row state from `skinny/RESULTS.md`:

| Workload | Current count | Close implication |
|---|---:|---|
| `parse_only` | 17 `K / NO-GO` | Parse remains wholly open under current classifier. |
| `direct_to_struct` | 6 `A / GO`, 11 `N-direct / NO-GO` | Digest stressor remains a guard, not product-plane proof. |
| `real_typed_struct` | 4 `A / GO` | Product-plane typed direct is the strongest validated line. |

Mandatory corrections:

1. W0 strict sonic repair was right, but the row-flip forecast was wrong.
   REDRESS item 77 says `instruments` and `unicode_basic` did not flip.
2. Schema-v3 telemetry landed in W0b, but the current `Hot leaf` cells still
   say unprofiled in W0b and `Delta vs SK-V6` is explicitly `n/a`. SK-V8 must
   bind profile paths and delta baselines before prescribing new kernels.
3. W2 mantissa widening was rejected because the canada fallback pool did not
   exist on the current baseline. No SK-V8 numeric wave may assume stale f64
   fallback cost without fresh attribution.
4. W3 capacity-hinted numeric Vec real typed expansion was admitted and is a
   real product-plane win for `mesh` and `marine_ik`. This should carry
   forward as validated `DirectBuild` schema work.
5. W4 per-quartet Unicode escape classifier was correctness-green but
   falsified by row thresholds. SK-V8 must not reopen that materializer helper
   shape.
6. W5 StringBlock16 tiny probe was correctness-green but regressed every named
   parse row. The existing aarch64 string-block wrapper is too expensive for
   the generated retained tiny probe.
7. W6 object-pair value-byte control compaction was correctness-green but too
   small economically and regressed `citm_catalog` Track 1.
8. W7 and W8 materially improved Lock 14 posture. SK-V8 must preserve the
   grammar-neutral generic-crate boundary and must not reintroduce JSON-named
   policy outside grammar inputs, generated output, or host/API schema facts.
9. W9 CostFacts substrate is now available. SK-V8 should consume it as a gate
   and feedback surface, not treat it as already producing route quality.
10. The original W10 bitmap body gate was not green. REDRESS item 88 rejected
    PMULL prefix-XOR as a default hot body after JSON parse regressions.
    REDRESS item 89 rejected CSSC CTZ/bulk production consumption after six
    Track 1/2 row drops over the maintain invariant. REDRESS item 90 admitted
    only B6 stack-canary Stage 1 with zero production and `RESULTS.md` diff.

The SK-V8 thesis should therefore be conservative:

- First repair observability: hot leaves, profile artefact paths, c/B, and
  baseline deltas must be real, not placeholders.
- Prefer product-plane `real_typed_struct` expansion over treating the
  synthetic digest stressor as the product close.
- Use CostFacts to select and reject route shapes with evidence.
- Treat parse-only closure as profile-driven and row-specific. No broad
  string, Unicode, control, bitmap, or asm body route carries over by default.
- Keep bitmap PMULL and CTZ/bulk production consumption routed but pre-blocked
  until a fresh same-row consumer proves non-regression.

## 3. SK-V8 Document Shape

### 3.1 `SYNTHESIS.md` outline

Required sections:

1. Header and authority.
   - State that this is the post-SK-V7 Pass Alpha synthesis.
   - Cite `skinny/RESULTS.md`, `skinny/REDRESS.md` items 77-90, and the latest
     SK-V7 commit chain.
   - State that W10 original bitmap body fills were rejected; B6-only W10c was
     admitted.

2. Current measured state.
   - Summarize 17 parse `K / NO-GO`, 11 direct `N-direct / NO-GO`, 6 direct
     `A / GO`, and 4 real typed `A / GO`.
   - Name the partial telemetry caveats: unprofiled hot leaves, no machine
     readable SK-V6 delta baseline, partial sidecar provenance.

3. Corrected diagnosis.
   - Strict comparator repair is done, but comparator repair did not produce
     row closes.
   - Product typed direct work is validated.
   - Digest stressor is a guard.
   - Parse remains unknown until profile binding is repaired.
   - Bitmap body fills are rejected for the current production paths.

4. Validated ledger.
   - W0/W0b strict and schema-v3 telemetry admitted.
   - W1 TapeKind rename admitted.
   - W3 capacity-hinted numeric Vec typed expansion admitted.
   - W7/W8 Lock 14 neutralization admitted.
   - W9 CostFacts substrate admitted.
   - W10c B6 Stage 1 admitted as harness hardening only.

5. Invalidated ledger.
   - W2 mantissa-widen fallback assumption.
   - W4 per-quartet Unicode escape classifier route.
   - W5 generated-retained StringBlock16 tiny probe.
   - W6 object-pair value-byte control compaction.
   - W10 PMULL prefix-XOR default hot body.
   - W10b CSSC CTZ/bulk production consumer.

6. SK-V8 thesis.
   - SK-V8 is an observability-bound and product-plane tranche.
   - The first wave must make row diagnosis executable.
   - Later waves must be selected from the current profile and CostFacts
     evidence, with no hypothesis transfer.

7. SOTA posture.
   - Strict-vs-strict only.
   - Permissive or lossy rows remain flaw probes.
   - `real_typed_struct` claims are product-plane claims only when the host/API
     schema is explicit and Track 1 is generated.

8. Pass Omega posture.
   - Route any lock amendments, bench-honesty lock work, or path cleanup to
     Omega. Do not mix those with SK-V8 performance waves unless SPEC makes a
     docs-only wave with explicit owner paths.

### 3.2 `SPEC.md` outline

Required sections:

1. Close condition and goalset.
2. Non-negotiables.
3. Telemetry binding.
4. Wave manifest.
5. Per-wave specifications.
6. Pre-blocked routes.
7. Challenge and G-Alpha gates.
8. Revert protocol and REDRESS rules.

The SPEC must follow the tranche precept:

- max 6 parallel agents;
- every wave has a number and name;
- every wave closes on artefacts, measurement, or explicit rejection;
- substrate must land with its consumer or be deleted;
- docs-only waves run document checks plus `git diff --check`;
- implementation waves run focused checks and the declared bench gate.

### 3.3 `HANDOFF.md` outline

Required sections:

1. Status line: SK-V8 spec materialized, awaiting G-Alpha or ready for W0 only
   after G-Alpha, depending on final signoff.
2. Reading order.
3. Current measured state.
4. Wave dispatch posture.
5. Pre-blocked routes.
6. Entry gates per wave.
7. Exit condition for SK-V8 close.
8. W10 honesty note.
9. Pass Omega trigger note.
10. Triumvirate and status discipline.

The handoff must say explicitly:

- SK-V7 W10 original bitmap body-fill gate was not green.
- W10c admitted B6-only Stage 1.
- PMULL prefix-XOR and CSSC CTZ/bulk consumer are rejected for SK-V7 and only
  enter SK-V8 through fresh measured gates.

### 3.4 `DISPATCH-PROMPT.md` outline

Required sections:

1. Purpose and scope.
2. Required reading in order.
3. Wave manifest with SPEC section links and hard caps.
4. Per-wave dispatch protocol:
   - research;
   - plan;
   - redress implementation or rejection;
   - REDRESS entry;
   - no commit role merger.
5. Falsifiability gates by reference to SPEC.
6. Pre-blocked routes by reference to HANDOFF and REDRESS.
7. Non-negotiables.
8. Status discipline.
9. Convergence and escalation.
10. Sub-wave naming.
11. Entry condition.
12. Initial dispatch command after G-Alpha.

The dispatch prompt must not restate all wave details. It should cite SPEC by
section and tell implementation agents to execute the next wave only after
reading SPEC, HANDOFF, RESULTS, and REDRESS.

## 4. Goalset Shape

The final SK-V8 SPEC should make the goalset precise. This alpha-F draft
proposes the shape and minimum targets; Alpha A-E and CHALLENGE must fill or
revise exact row thresholds before G-Alpha.

### 4.1 Global close condition

SK-V8 closes only when all of the following are true:

1. `skinny/RESULTS.md` remains schema-v3 compatible and adds SK-V8-required
   profile bindings.
2. Every row has a measured hot leaf, profile artefact path, c/B or equivalent
   sample cost, and delta versus the SK-V7 opening baseline.
3. `gate-json` rejects any row missing required telemetry.
4. Strict comparator anchors remain strict; lossy or permissive comparators
   remain flaw probes.
5. At least one product-plane improvement wave admits with generated Track 1,
   independent Track 2 or oracle parity, and same-plane comparator evidence.
6. Any parse or direct digest performance wave either admits on its named rows
   or is rejected with REDRESS evidence and the candidate removed.
7. No previously passing row regresses by more than the wave guard budget.
8. No pre-blocked route is reopened without fresh PC-level evidence,
   same-wave consumer, and explicit CHALLENGE acceptance.

### 4.2 Per-row goalset draft

The final packet should include a table with one row per corpus and workload.
Minimum starting targets:

| Row class | Current state | SK-V8 target posture |
|---|---|---|
| All 17 `parse_only` rows | `K / NO-GO`; hot leaf unprofiled in W0b | W0 must produce hot leaf, c/B, profile path, and SK-V7 delta before any parser kernel is proposed. |
| `twitter` parse | 15752 Mbps Track 1, -25.1% vs sonic strict, -49.1% vs yyjson | Treat as hard residual; target profile-bound fusion-quality diagnosis before any implementation. |
| `citm_catalog`, `canada`, `mesh`, `marine_ik`, `numbers` parse | Track 1 faster than sonic on some rows but still `K / NO-GO` | Diagnose classifier/Track 2/output-plane reason for `K`; do not claim SOTA until gate semantics are explicit. |
| 11 `direct_to_struct` misses | `N-direct / NO-GO` digest plane | Keep as guard; W0 must name hot leaves and W3 must decide which rows remain product-relevant. |
| 4 `real_typed_struct` passes | `twitter`, `update_center`, `mesh`, `marine_ik` are `A / GO` | Maintain all four; expand product-plane rows only through host/API schema facts. |
| Bitmap body routes | PMULL and CTZ/bulk rejected | No performance target until a fresh row profile names bitmap prefix/next-bit as a hot owner. |
| B6 hardening | Stage 1 admitted | Preserve and optionally extend harness coverage without production throughput claims. |

### 4.3 Candidate close targets for final Alpha to confirm

These are draft candidates, not final admissions:

1. Observability target:
   - 100 percent of `RESULTS.md` rows carry non-placeholder `Hot leaf`.
   - 100 percent carry `Profile artifact`.
   - `Delta vs SK-V7-open` is non-`n/a`.

2. Typed product target:
   - Maintain the 4 current `real_typed_struct` GO rows.
   - Add at least 2 new generated typed rows from host/API schema facts.
   - Candidate rows for Alpha E to confirm: `canada`, `numbers`,
     `unicode_basic`, or one escape-heavy row.

3. Direct guard target:
   - Select at most 3 `N-direct` rows after W0 profiling.
   - Admit only if the selected rows cross the declared sonic slack with no
     Track 2/oracle dishonesty.
   - Otherwise classify as guard residual and route explicitly.

4. Parse target:
   - Do not promise a broad parse close before W0 profiling.
   - After W0, select at most 3 parse rows for one candidate wave.
   - Every parse candidate must name the exact hot leaf and a no-regression
     guard across all rows.

## 5. Telemetry Binding

SK-V8 must preserve the PASS-ALPHA schema-v3 surface and add binding fields
needed to prevent another unprofiled prescription.

Required existing columns:

```text
Corpus
Workload
Outcome
Verdict
Strictness
parse_utf8
escape_complete
flaw_probe
Output plane
Track 1 Mbps
Track 2 Mbps
sonic-rs strict Mbps
sonic-rs lossy Mbps
simdjson DOM Mbps
simdjson On Demand Mbps
yyjson default Mbps
asmjson SWAR Mbps
asmjson AVX-512 Mbps
RapidJSON default Mbps
serde_json Mbps
Delta vs previous SK
Delta vs sonic-strict
Delta vs simdjson DOM
Delta vs yyjson
Hot leaf
Signal
```

Required SK-V8 additions:

```text
Profile artifact
Cycles per byte
Sample count
Build flags
Host triple
Feature mask
CostFacts rule id
CostFacts chosen shape
CostFacts rejected alternative ids
Redress entry
Wave id
Run id
Sidecar freshness
```

`gate-json` requirements:

- reject placeholder hot leaves such as `unprofiled in W0b` after W0;
- reject missing profile artefact paths after W0;
- reject `Delta vs previous SK = n/a` after the SK-V8 opening baseline is
  captured;
- reject strict claims that use lossy/permissive comparators;
- reject admitted wave rows without a REDRESS entry or wave id;
- reject CostFacts-selected shapes with no evidence source.

## 6. Draft Wave Structure

The final SPEC should use the smallest wave count that closes honestly. This
draft proposes six waves plus close.

| Wave | Name | Purpose | Hard cap |
|---|---|---|---:|
| W0 | Baseline Profile And Telemetry Lock | Make every row profile-bound and delta-bound. No performance patch. | 180 min |
| W1 | Typed Product Plane Expansion | Expand generated `real_typed_struct` rows through host/API schema facts. | 300 min |
| W2 | Parse Candidate From Fresh Profiles | Select one parse intervention after W0 only; admit or reject by row gates. | 300 min |
| W3 | Direct Guard Triage | Profile and either close or explicitly route selected digest guard rows. | 240 min |
| W4 | CostFacts Gate Integration | Bind CostFacts evidence to `RESULTS.md` and `gate-json`. | 240 min |
| W5 | Grammar-Neutral Audit And Lock 14 Preservation | Audit that W1-W4 did not reintroduce generic-crate JSON policy. | 180 min |
| W6 | Close, Redress Reconciliation, And Alpha Feedback | Docs-only close report, REDRESS reconciliation, Pass Alpha feedback. | 120 min |

### W0 - Baseline Profile And Telemetry Lock

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/crates/xtask/`
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-*.md`

Entry gate:

- SK-V7 W10c is closed honestly.
- Current `skinny/RESULTS.md` is the opening baseline.

Exit gate:

- all rows have hot leaf, profile path, c/B or equivalent, run id, and
  `Delta vs SK-V7-open`;
- sidecar provenance is explicit;
- no source-performance patch lands;
- `gate-json --advisory` shows the new schema and rejects missing fields.

Revert protocol:

- If profiling cannot populate the schema, revert schema changes and record a
  REDRESS item naming the missing profiler or bench path.

### W1 - Typed Product Plane Expansion

Owner paths:

- `skinny/crates/codegen/`
- `skinny/crates/bbnf-bench/`
- `skinny/crates/xtask/`
- generated real typed bench outputs
- `skinny/RESULTS.md`

Entry gate:

- W0 telemetry lock complete.
- Alpha E confirms the two or more selected rows and host/API schema facts.

Exit gate:

- the four existing real typed GO rows maintain GO;
- at least two new generated typed rows are added and pass their declared
  same-plane slack;
- Track 1 is generated and Track 2/oracle is structurally different;
- no bench-private parser, hand-authored proof sink, or skip-only dishonesty.

Revert protocol:

- Failed row additions are reverted or left disabled only if the REDRESS entry
  names them as rejected and the gate has no hidden product claim.

### W2 - Parse Candidate From Fresh Profiles

Owner paths:

- selected only after W0 profile evidence;
- likely under `skinny/crates/runtime/`, `skinny/crates/parse-that-regex/`,
  `skinny/crates/bbnf-simd/`, or generated runtime output, but the final SPEC
  must name exact paths.

Entry gate:

- W0 has named hot leaves for all parse rows.
- CHALLENGE accepts that the candidate is not a pre-blocked route.

Exit gate:

- selected parse rows cross the declared threshold;
- no guard row regresses beyond the declared budget;
- scalar reference, checkasm, and same-wave consumer exist for any primitive;
- candidate is rejected with REDRESS if thresholds fail.

Hard rule:

- W2 must not reopen W4/W5/W6 SK-V7 shapes by renaming them.

### W3 - Direct Guard Triage

Owner paths:

- selected after W0 direct profiles;
- final SPEC must distinguish digest guard from product typed rows.

Entry gate:

- W0 direct profiles name hot owners.
- W1 product-plane expansion has landed or been rejected.

Exit gate:

- either selected `N-direct` rows close under digest guard rules, or the final
  HANDOFF explicitly routes them as guard residuals;
- no direct row claim is used as product-plane SOTA unless it is a generated
  typed output row.

### W4 - CostFacts Gate Integration

Owner paths:

- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/`
- `skinny/crates/codegen/`
- `skinny/crates/xtask/`
- `skinny/RESULTS.md`

Entry gate:

- W9 CostFacts substrate from SK-V7 is present.
- W0 has row telemetry to bind evidence.

Exit gate:

- every materialized JSON rule reports chosen shape, rejected alternatives,
  evidence source, REDRESS references, and wave id;
- `gate-json --with-cost-facts` rejects missing evidence after W4;
- generic CostFacts types remain grammar-neutral.

### W5 - Grammar-Neutral Audit And Lock 14 Preservation

Owner paths:

- audit files under `restart/skinny/tranches/sk-v8/research/`
- no source path unless SPEC explicitly names a small cleanup

Entry gate:

- W1-W4 closed.

Exit gate:

- grep/audit confirms no JSON policy re-entered generic crates;
- generated JSON output and grammar input exceptions are documented;
- any discovered Lock 14 drift is either fixed in the wave or routed with a
  named owner and REDRESS note.

### W6 - Close, Redress Reconciliation, And Alpha Feedback

Owner paths:

- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/wave-6-close.md`
- `skinny/REDRESS.md` only if final redress reconciliation is needed by SPEC

Entry gate:

- all implementation waves admitted or rejected.

Exit gate:

- every wave has a measured disposition;
- `RESULTS.md` and `REDRESS.md` agree;
- final HANDOFF names residuals for SK-V9 or Pass Omega;
- no open brittleness window remains.

## 7. Hard Caps And Role Discipline

Default phase caps per implementation wave:

| Phase | Cap |
|---|---:|
| Research agents | 30 min each |
| Plan synthesis | 30 min |
| Redress implementation | 75 min default, or wave-specific cap above |
| CHALLENGE for high-risk wave | 90 min |

High-risk waves:

- W1 typed product expansion.
- W2 parse candidate.
- W3 direct guard triage if it changes output semantics.
- W4 CostFacts gate if it changes admission behavior.

No wave should exceed six parallel agents. No wave may merge research, plan,
and redress into one commit in the final dispatch discipline. This alpha-F file
does not commit and does not stage anything.

## 8. Pre-Blocked Routes

The final HANDOFF and DISPATCH-PROMPT must carry these as explicit
pre-blocked clusters:

1. REDRESS 28+33: active Class A NEON tiny-string wiring as a parse close.
2. REDRESS 50-55: parse-time retained projection, byte-class whitespace
   cursor, parser-local structural-mask cursor, exact decoded-string stats
   sink, and quote-source streaming materializer.
3. REDRESS 60-65: retained trusted-string boundary collapse, wide/delayed
   string scans, four-unit Unicode escape validator, and object next-key carry.
4. REDRESS 66-69: direct source-hook field-layout materializer,
   parser-owned decoded scratch, byte-output unescape materializer, and
   semantic string fact hashing under the current digest workload.
5. REDRESS 70: hand-authored typed sink as SOTA proof.
6. REDRESS 72: global cap-16 policy, direct cap-16 policy, and Track 2 cap-16
   policy. Only generated retained cap-16 remains admitted.
7. REDRESS 80: mantissa-widen/fallback-elimination assumption for canada.
8. REDRESS 82: single-quartet Unicode escape classifier.
9. REDRESS 83: generated-retained StringBlock16 tiny probe.
10. REDRESS 84: object-pair value-byte control compaction.
11. REDRESS 88: PMULL prefix-XOR as default hot production body.
12. REDRESS 89: CSSC CTZ next-bit plus bulk production consumer.
13. Historical blocked routes: function-pointer dispatch table, pair-token
    fusion, 12-byte token churn, separator elision, generic SWAR whitespace,
    capacity prescan, EventCursor or other parallel prepass sidecar, raw f64
    shortcut, and orphan primitive admission.

Reopen rule:

- Fresh PC-level evidence on the SK-V8 baseline.
- Same-wave consumer.
- Scalar reference and checkasm parity for primitives.
- Explicit no-regression budget.
- REDRESS citation explaining why the new shape is not the rejected shape.
- CHALLENGE acceptance before redress.

## 9. G-Alpha Signoff Posture

This alpha-F draft cannot close G-Alpha.

G-Alpha can be presented only after:

1. Alpha A-E artefacts exist and are folded.
2. The final SK-V8 `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, and
   `DISPATCH-PROMPT.md` exist.
3. CHALLENGE returns at least 95 percent ACCEPT, zero critical defects, and no
   orphan REVISE.
4. The user receives the SK-V8 summary: targeted rows, candidate
   interventions, hard caps, telemetry schema, pre-blocked routes, and predicted
   close state.
5. The summary states plainly that SK-V7 W10 original bitmap body fills were
   not admitted. Only B6 stack-canary Stage 1 was admitted in W10c.

Signoff options:

- `G-Alpha closed`: SK-V8 W0 may dispatch.
- `G-Alpha revise`: named revisions return to Alpha hardening.

No SK-V8 implementation wave should dispatch from this draft alone.

## 10. Alpha-F Open Items For Finalization

Alpha A-E or CHALLENGE must still fill:

1. Exact row thresholds for W1-W3.
2. Which two or more typed product rows W1 will add.
3. Which parse rows, if any, W2 may target after W0 profiles.
4. Whether W3 keeps all digest `N-direct` rows as guard residuals or selects a
   small close set.
5. Exact `gate-json` schema field names for the SK-V8 additions.
6. Exact owner paths for W2 and W3 after W0 evidence exists.
7. Whether any Pass Omega lock amendment must block G-Alpha or can route after
   SK-V8 opens.

The final contract should be smaller than this draft where possible. The
load-bearing points are the corrected W10 honesty, telemetry binding,
pre-blocked route list, and profile-first SK-V8 entry gate.
