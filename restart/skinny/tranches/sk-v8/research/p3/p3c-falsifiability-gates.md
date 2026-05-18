# SK-V8 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V4 exact traceability fold.
Date: 2026-05-18.
Scope: Per-wave falsifiability gates for the SK-V8 W0-W6 wave plan.
Output: this file.
Pass Alpha goalset: SK-V8 closes only after W0 creates `SK-V8-open`, every main row has required telemetry, `gate-json` rejects missing evidence, CostFacts are gate-consumed, current typed GO rows maintain, new typed rows use independent oracle proof, behavior waves hit named row gates or reject with REDRESS, pre-blocks stay closed, Lock 14/15 pass, and RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`/named REDRESS id(s) in `skinny/REDRESS.md`/`HANDOFF Section 6 - Exit Condition` agree.
Candidate pool: research/p2/ post-CHALLENGE survivors.
Traceability note: inline citations use exact SPEC/HANDOFF section labels or current file:line anchors. RESULTS row claims resolve to `skinny/RESULTS.md:3-42`; Track 2 independence resolves to `skinny/RESULTS.md:217-218`; named REDRESS ids resolve to `skinny/REDRESS.md`, with cited live spans `skinny/REDRESS.md:1214-1219`, `skinny/REDRESS.md:1301-1312`, and `skinny/REDRESS.md:1331-2605`.


## §1 - Synthesis

P3-C is a gate-binding artifact, not an implementation dispatch. The controlling S-P3 prompt assigns P3-C the named-row Mbps thresholds, full-table maintain budgets, exit gates, and revert protocols; unmeasurable gates are rejected (`PASS-3 Synthesis-Plan role and gate sections`). The same prompt requires W0 to lock `SK-V8-open`, same-row falsifiability, same-wave consumers, and no hypothesis transfer (`PASS-3 Synthesis-Plan role and gate sections`).

The current authority is the SK-V8 wave manifest W0-W6 in `SPEC Section 2 - Wave Manifest, Caps, And Reruns`, `SPEC Section 3 - W0 Baseline Profile And Telemetry Lock`, `SPEC Section 4 - W1 CostFacts And Comparator Gate Binding`, `SPEC Section 5 - W2 Typed Product Plane Expansion`, `SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`, `SPEC Section 7 - W4 Direct Guard Triage`, `SPEC Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation`, `SPEC Section 9 - W6 Close And Alpha Feedback`, `SPEC Section 10 - Pre-Blocked Routes`, and `SPEC Section 11 - G-Alpha And Dispatch Scope`; HANDOFF mirrors dispatch in `HANDOFF Section 4 - Dispatch Posture`, entry in `HANDOFF Section 5 - Entry Gates`, and close in `HANDOFF Section 6 - Exit Condition`. The adjacent P3-B wave sequencing artifact matches that seven-wave shape and orders W4 after W2/W3 disposition or explicit routing. This file binds that full W0-W6 plan for P3-C; if P3-F later changes the wave set, it must reconcile this file before dispatch.

S-P2 governance is closed only for synthesis planning. V6 and V7 are the two consecutive qualifying ACCEPT cycles; V7 explicitly authorizes S-P3 synthesis but no SK-V8 implementation wave, W3 redress, or G-Alpha close (`S-P2 V7 consolidated verdict and preserved boundaries`). The accepted boundaries that carry into these gates are: strict-vs-strict admission only, Tier A structural-class cursor migration only, Tier B separate, Lock 14 grammar-neutrality, no new directive/BIR/BackendShape/UnionTape/public substrate API/parser-owned cursor/facts/parallel substrate, and `tape_vs_tape` as telemetry rather than W3 production consumer (`S-P2 V7 consolidated verdict and preserved boundaries`).

Current opening rows are still `N-direct / NoGo`: 17 `parse_only` `K/NO-GO`, 6 `direct_to_struct` `A/GO` plus 11 `N-direct/NO-GO`, and 4 `real_typed_struct` `A/GO` (`SYNTHESIS opening state and S-P2/W3 finding sections`). All current rows have deferred strictness and unprofiled hot leaves, so W0 is observability, not a parser prescription. Current row values in RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218` are the seed values below; post-W0, `SK-V8-open` replaces them as the authoritative before value.

Implementation cap normalization: every W0-W6 redress/implementation dispatch is capped at 90 minutes wall clock inclusive of source edits, generation, bench, RESULTS/REDRESS updates, and rollback. Research, plan, and challenge phases keep their pass-level caps, but no implementation slice may rely on any older 120-300 minute wave cap copied from a pre-P3 SPEC. A candidate that cannot be implemented, measured, and either admitted or reverted inside 90 minutes must be split in plan or marked REVISE before redress.

Global gate predicates for every wave:

- Strict admission uses same-run strict anchors only: sonic-rs strict or serde_json where output planes match. Sonic lossy, permissive rows, C++ sidecars, historical deltas, and stale sidecars are planning signals only (`SPEC Section 0.2 - Comparator Classes` and `SPEC Section 0.4 - Required Telemetry`).
- `parse_only` rows remain substrate-guard non-admission rows (`K`, or `S` if W0 amends schema). They cannot support strict SOTA admission while strictness is deferred or UTF-8 validation is view-boundary (`SPEC Section 0.2 - Comparator Classes`, `SPEC Section 0.3 - Outcome Enum`, and `SPEC Section 3 - W0 Baseline Profile And Telemetry Lock`; SC-5 at `SC-5 parse_only demotion and tape_vs_tape limits`).
- Any emitted telemetry field must be consumed by `gate-json` in the same wave; producer-only profile paths, freshness fields, or CostFacts ids fail (`SPEC Section 0.4 - Required Telemetry` and `SPEC Section 3 - W0 Baseline Profile And Telemetry Lock`).
- Lock 14 passes every wave: no generic crate gains JSON-named public API, JSON grammar/corpus/role branches, JSON structural policy, or hand-coded per-grammar runtime behavior; generic edits need CSS L4, Sheets, and BBNF-self proof (`SPEC Section 2.1 - Generality And Lock 14 Gate` and `SPEC Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation`).
- No new directive, BIR variant, substrate, public `UnionTape`, `BackendShape` variant, sidecar substrate, parser-owned structural cursor/facts, or consumer-later primitive is admitted (`SPEC Section 1 - Non-Negotiables` and `SPEC Section 10 - Pre-Blocked Routes`; `SC-3 Tier A owner/cost table and one-Tape constraints`).
- Any primitive or kernel requires scalar reference, checkasm parity, same-wave hot-path consumer, named bench rows, and `samply` symbol-path proof. Missing consumer is an orphan kernel and must be rejected (`SKINNY-TRIUMVIRATE role separation and redress contract`).
- A miss creates REDRESS evidence. No wave closes on "wired", "integrated", "advisory only", or future-wave promise without a measured row threshold and revert protocol (`PASS-3 Synthesis-Plan role and gate sections`).

## §2 - Deliverable

### W0 - Baseline Profile And Telemetry Lock

Entry gate: G-Alpha is closed by the user; RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218` form the SK-V7 close baseline; no behavior wave is dispatchable before W0 admits (`HANDOFF Section 10 - G-Alpha Decision`).

Hard cap: 90 minutes implementation plus measurement and rollback.

Named rows and thresholds:

| Row set | Gate |
|---|---|
| All 38 current main rows in RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218` | W0 captures `SK-V8-open`; every throughput cell must stay within +/-1.0% of the captured seed value. |
| 17 `parse_only` rows | Must report substrate-guard non-admission (`K`, or `S` if W0 amends schema), not strict SOTA GO. |
| Four current `real_typed_struct` GO rows | `twitter`, `update_center`, `mesh`, `marine_ik` retain GO status and telemetry; no row may lose independent oracle proof. |
| Sidecar cells | Populated values require manifest/freshness coverage; missing values require explicit `sidecar_freshness=absent:<reason>`. |

Exit gate:

- All 38 rows have `grammar_id`, `domain`, comparator id/plane/strictness, profile artifact, sample cost or cycles/byte, sample count, build flags, host triple, feature mask, run id, wave id, sidecar freshness, and `SK-V8-open` delta (`SPEC Section 0.2 - Comparator Classes` and `SPEC Section 0.4 - Required Telemetry`).
- `gate-json` rejects placeholder hot leaves, missing profile artifacts, missing run ids, missing deltas, malformed sidecar manifest, unsupported outcome, and any strict-admission row whose comparator plane/strictness/freshness/measured-row validation fails.
- Lock 14 baseline allowlist is created.
- No parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated parser output change lands.

Negative gates and aborts:

- Any source behavior change, generated parser behavior change, product-plane behavior change, or throughput movement beyond +/-1.0% rejects W0.
- Any row without profile artifact, run id, host/build metadata, feature mask, sample cost, or `SK-V8-open` delta rejects W0.
- Any `parse_only` strict SOTA admission rejects W0.

Revert protocol: revert W0 report/gate/schema/RESULTS changes as one slice, restore the opening RESULTS schema, and record a W0 REDRESS rejection naming the missing profiler, gate, or row. W0 rejection blocks W1-W6.

### W1 - CostFacts Gate Binding

Entry gate: W0 admitted and every current main row has `SK-V8-open` telemetry (`SPEC Section 0.4 - Required Telemetry` and `SPEC Section 3 - W0 Baseline Profile And Telemetry Lock`).

Hard cap: 90 minutes implementation plus measurement and rollback.

Named rows and thresholds:

| Row set | Gate |
|---|---|
| All 38 current main rows | Full-table maintain within +/-1.0% of `SK-V8-open`; no verdict downgrade. |
| Every materialized JSON rule | Reports CostFacts rule id, chosen shape, rejected alternative ids, evidence source, REDRESS reference, and wave id. |
| Non-JSON proof rows | CSS L4, Sheets, and BBNF-self compile/lower/cost without generic JSON roles. |

Exit gate:

- `gate-json --with-cost-facts` rejects missing CostFacts evidence.
- Generic CostFacts/report fields use grammar-aware comparator fields; no generic CostFacts path contains JSON policy (`SPEC Section 2.1 - Generality And Lock 14 Gate` and `SPEC Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation`).
- Generated JSON output and parser behavior remain unchanged unless a separate W1 behavior consumer was explicitly challenged and accepted. Default W1 is no behavior change.

Negative gates and aborts:

- Any CostFacts id or profile field emitted but not consumed by the gate rejects W1.
- Any generic JSON policy, JSON branch, JSON public API leak, or non-JSON proof failure rejects W1.
- Any parser behavior or generated-output diff without explicit accepted behavior-consumer challenge rejects W1.

Revert protocol: revert CostFacts report/gate changes together, keep read-only audit evidence in the wave research artifact, and add REDRESS naming the missing or non-neutral fact class. W1 rejection blocks W2-W6 behavior waves.

### W2 - Typed Product Plane Expansion

Entry gate: W0 and W1 admitted; W2 plan names exact typed rows, host/API schema facts, owner paths, Track 1 generated path, independent Track 2/oracle path, thresholds, and rollback boundaries (`SPEC Section 0.5 - Opening Row Goalset` and `SPEC Section 5 - W2 Typed Product Plane Expansion`).

Hard cap: 90 minutes implementation plus measurement and rollback. A W2 plan selecting more rows than can be generated, independently verified, and benched in 90 minutes must split.

Strict threshold rule: a selected typed/product row must be within 1.10x sonic-rs strict time, expressed as `Track 1 Mbps >= ceil(sonic-rs strict Mbps / 1.10)`. If the W0 sonic strict anchor changes, recompute from W0; the seed floors below are from RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`.

Existing real-typed GO maintain floors. Both floors apply: the sonic floor preserves GO status, and the no-regression floor preserves the W2 full-table maintain budget for current GO rows.

| Row | Current Track 1 | sonic strict | Sonic GO floor | No-regression floor |
|---|---:|---:|---:|---:|
| `twitter/real_typed_struct` | 18513 | 15486 | 14079 | 18143 |
| `update_center/real_typed_struct` | 11879 | 12627 | 11480 | 11642 |
| `mesh/real_typed_struct` | 9466 | 8696 | 7906 | 9277 |
| `marine_ik/real_typed_struct` | 12020 | 8750 | 7955 | 11780 |

Existing direct GO guard floors:

| Row | Minimum Track 1 Mbps | Minimum Track 2 Mbps |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18151 | 18151 |
| `apache_builds/direct_to_struct` | 10111 | 10111 |
| `mesh/direct_to_struct` | 7990 | 7990 |
| `marine_ik/direct_to_struct` | 7407 | 7407 |
| `numbers/direct_to_struct` | 11671 | 11671 |
| `unicode_basic/direct_to_struct` | 7730 | 7730 |

Candidate typed seed floors for at least two new generated typed rows:

| Candidate row | sonic strict | Minimum Track 1 Mbps |
|---|---:|---:|
| `canada/real_typed_struct` | 12421 | 11292 |
| `numbers/real_typed_struct` | 12838 | 11671 |
| `unicode_basic/real_typed_struct` | 8502 | 7730 |
| `citm_catalog/real_typed_struct` | 19966 | 18151 |
| `apache_builds/real_typed_struct` | 11122 | 10111 |

Exit gate:

- At least two new generated typed rows pass their declared same-plane gate, or W2 is rejected with REDRESS evidence.
- Existing `twitter`, `update_center`, `mesh`, and `marine_ik` real-typed rows maintain GO and the floors above.
- Existing direct GO rows maintain GO and the floors above.
- Full-table maintain: every non-target parse/direct/typed row remains no worse than -2.0% Track 1 and Track 2 versus `SK-V8-open`, with no correctness or verdict downgrade.
- Track 1 is generated from grammar facts plus explicit host/API schema facts. Track 2/oracle is structurally independent and does not call generated Track 1, generated SinkOnly, generated typed helpers, or benchmark-private shared parser.
- Direct digest rows remain guard rows, not typed product proof.

Negative gates and aborts:

- One new row passing is not partial admission; W2 requires at least two or rejects.
- Any Track 2/oracle coupling to generated Track 1 or generated typed helpers rejects W2.
- Any host/API schema fact hidden in generic crates, any JSON policy leak, or any non-JSON proof failure rejects W2.
- Any full-table row outside the -2.0% maintain budget rejects W2 even if target typed rows pass.

Revert protocol: revert row additions, generated outputs, host/API schema facts, gate changes, RESULTS changes, and bench wiring as one slice unless the failed rows are left explicitly disabled with rejected status. Preserve generated diff audit and row table in research, and add REDRESS.

### W3 - Profile-Selected Parse Candidate

Entry gate: W0 and W1 admitted; W3 plan names one parse candidate, exact owner files, exact rows, same-wave production consumer, revert protocol, Lock 1 fork, scalar/checkasm requirements, and challenge acceptance proving it is not a renamed REDRESS 82/83/84/88/89 route (`SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`).

Hard cap: 90 minutes implementation plus measurement and rollback. Tier A may proceed only if the selected slice fits 90 minutes. Tier B string-boundary/parity/CostFacts-template work is not folded into Tier A.

Candidate boundary: the only S-P2-ready W3 shape is Tier A structural-class cursor migration: retain the stage-1 index inside one `Tape`, add scan-written opaque class ordinals, migrate generated retained JSON Track 1 parsing plus retained view/`ValueRef`, and delete scalar structural rediscovery. Tier A does not claim string-boundary closure (`SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`; `SC-3 Tier A owner/cost table and one-Tape constraints`).

Primary row threshold rule: the W3 plan must select at least two structural-heavy parse rows from the table below. Each selected row must hit `Track 1 Mbps >= ceil(SK-V8-open Track 1 * 1.03)`, show the scalar structural rediscovery hot leaf removed or below the W3 plan's declared sample-cost floor, and keep strict validation inside the measured row. Current seed floors:

| Candidate parse row | Current Track 1 | Minimum Track 1 Mbps |
|---|---:|---:|
| `twitter/parse_only` | 15752 | 16225 |
| `apache_builds/parse_only` | 12482 | 12857 |
| `update_center/parse_only` | 11193 | 11529 |
| `github_events/parse_only` | 15198 | 15654 |
| `gsoc-2018/parse_only` | 23026 | 23717 |
| `distinct_values/parse_only` | 6655 | 6855 |
| `y_string_unicode/parse_only` | 6216 | 6403 |

Guard row floors for number-heavy and currently positive substrate-guard rows:

| Guard row | Minimum Track 1 Mbps | Minimum Track 2 Mbps |
|---|---:|---:|
| `canada/parse_only` | 17410 | 16729 |
| `mesh/parse_only` | 13980 | 13022 |
| `numbers/parse_only` | 20197 | 18144 |
| `marine_ik/parse_only` | 13522 | 12137 |

Exit gate:

- Selected parse rows cross their declared thresholds.
- All 38 current main rows respect the W3 maintain budget: default no Track 1 or Track 2 regression worse than -2.0% versus `SK-V8-open`.
- The measured rows prove strict validation, comparator evidence, structural cursor work, and admitted tape facts occurred inside the measured row, not in view-boundary, post-parse, sidecar, or comparator-only paths (`SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`).
- Exactly one retained tape survives. A structural projection passes only if it replaces the offset-tape append path; it fails if retained beside old offset construction. The old offset append API, parser-owned cursor/fact slots, and surviving `StructuralIndex` query API must be absent (`SC-6 Lock 1/14 and one-substrate constraints`).
- Generated JSON retained parser is the Tier A production consumer. `tape_vs_tape`, direct/SinkOnly rows, `path!`, and Track 2 are touched/proven-untouched audit rows, not Tier A production consumers (`SC-3 Tier A owner/cost table and one-Tape constraints`).
- Scalar oracle and checkasm parity pass before primitive wiring.
- Retained view/`ValueRef` parity and Track 2 independence proof pass.
- Lock 14 and non-JSON proofs pass.
- `parse_only` row status remains substrate-guard non-admission unless W0/W3 gate schema has independently proven same-plane strict admission eligibility. A W3 substrate admit does not by itself turn `parse_only` into SOTA GO.

Negative gates and aborts:

- Any source-byte second scan, retained cursor, aux table, density cache, sidecar event vector, parser-owned class/fact slot, old offset append path, or parallel producer rejects W3.
- Any `UnionTape`, new `BackendShape`, new BIR variant, new BBNF directive, public substrate API, or generic grammar API rejects W3.
- Any use of `tape_vs_tape` as the production consumer rejects W3.
- Any string-boundary, quote/backslash/parity, density-policy, or CostFacts-template claim inside Tier A rejects W3 unless W3 has been explicitly expanded and re-challenged.
- Any selected row below threshold or any guard/full-table row beyond -2.0% rejects W3.

Revert protocol: revert runtime/tape, SIMD, codegen templates, generated JSON output, retained view/value, gate, RESULTS, and REDRESS changes as one slice. Save the rejected patch under the wave research directory and add REDRESS naming target rows and guard rows. W3 rejection blocks further parse candidates until challenge accepts a new frame.

### W4 - Direct Guard Triage

Entry gate: W0 and W1 admitted; W2 and W3 have admitted, rejected, or been explicitly routed, or W3 is explicitly blocked before W4; W4 plan names one to three selected `N-direct` rows, exact owner paths, strict direct thresholds, Track 1/Track 2 independence proof, and residual routing (`SPEC Section 7 - W4 Direct Guard Triage`). More than three selected rows exceeds the 90-minute implementation cap and must split.

Hard cap: 90 minutes implementation plus measurement and rollback.

Strict threshold rule: a selected direct row must be within 1.10x sonic-rs strict time on both bbnf tracks, expressed as `Track 1 Mbps >= ceil(sonic-rs strict Mbps / 1.10)` and `Track 2 Mbps >= ceil(sonic-rs strict Mbps / 1.10)`. Current seed floors:

| Candidate `N-direct` row | sonic strict | Minimum Track 1 Mbps | Minimum Track 2 Mbps |
|---|---:|---:|---:|
| `twitter/direct_to_struct` | 14885 | 13532 | 13532 |
| `canada/direct_to_struct` | 12421 | 11292 | 11292 |
| `github_events/direct_to_struct` | 16041 | 14583 | 14583 |
| `update_center/direct_to_struct` | 11081 | 10074 | 10074 |
| `random/direct_to_struct` | 8936 | 8124 | 8124 |
| `gsoc-2018/direct_to_struct` | 23407 | 21280 | 21280 |
| `instruments/direct_to_struct` | 12673 | 11521 | 11521 |
| `unicode_mixed/direct_to_struct` | 9679 | 8800 | 8800 |
| `unicode_escapes/direct_to_struct` | 14028 | 12753 | 12753 |
| `distinct_values/direct_to_struct` | 11344 | 10313 | 10313 |
| `y_string_unicode/direct_to_struct` | 9019 | 8200 | 8200 |

Exit gate:

- Every selected row closes under direct digest guard rules: correctness parity, Track 1 floor, Track 2 floor, sonic-rs strict same-run anchor, and no view-boundary strict-admission claim.
- Track 1 and Track 2 remain structurally independent; Track 2 must not call generated SinkOnly, generated typed helpers, generated Track 1, or a shared benchmark-private parser.
- Full-table maintain: all non-target rows remain no worse than -2.0% Track 1 and Track 2 versus `SK-V8-open`; existing direct GO and real-typed GO rows maintain GO.
- No direct digest result is presented as product-plane SOTA proof; direct rows are guard rows unless a typed product-plane row is generated and independently verified.
- Lock 14 and generic non-JSON proofs pass.

Negative gates and aborts:

- If any selected row misses either Track 1 or Track 2 floor, the behavior candidate rejects; it cannot admit on "route residual" after source changes.
- Sink-local decoded stats, quote-source streaming hash, raw `f64` shortcut, parser-owned scratch/direct sidecar, digest cap-16 rerun, or Track 2 coupling rejects W4 (REDRESS 54/55 and 60-72 class).
- Any full-table row beyond -2.0%, existing GO downgrade, or generic JSON leak rejects W4.

Revert protocol: revert behavior changes, generated outputs, bench wiring, RESULTS, and gate changes as one slice. Keep a direct-triage report that routes residuals and add REDRESS for failed behavior attempts.

### W5 - Grammar-Neutral Audit And Lock 14 Preservation

Entry gate: W1-W4 have admitted, rejected, or been explicitly routed (`SPEC Section 8 - W5 Grammar-Neutral Audit And Lock 14 Preservation`).

Hard cap: 90 minutes implementation plus measurement and rollback. Default source LOC cap is 0; a W5 plan may allow up to 150 source LOC only for a named Lock 14 fix.

Named rows and thresholds:

| Row set | Gate |
|---|---|
| RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218` | Zero behavior drift by default; no RESULTS diff unless W5 explicitly fixes a routed drift. |
| Generated JSON output | Zero diff by default; any diff requires named Lock 14 fix and unchanged behavior proof. |
| Non-JSON proof | CSS L4, Sheets, and BBNF-self compile/lower/cost/run without generic JSON roles. |

Exit gate:

- No JSON policy enters generic crates. Allowed JSON surfaces remain grammar inputs, generated JSON output, per-grammar templates/providers, tests, and host/API schema facts.
- Grep/audit covers renamed JSON policy, not only old names. It includes REDRESS 36, 37, and 38 Lock 14 residue clusters.
- Generic edits satisfy public API scan, grammar-branch scan, primitive/table scan, role/fact interpretation boundary, template/provider boundary, and non-JSON proof from SPEC Section 2.1.
- Generated JSON output and RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218` have zero behavior drift unless W5 explicitly fixed prior routed drift and recorded it.

Negative gates and aborts:

- Any new generic JSON public API, grammar-name branch, JSON object/array/field role in generic code, JSON structural policy in generic primitive/table, or hand-written per-grammar runtime file rejects W5.
- Any generated-output or RESULTS drift without an explicit W5 fix record rejects W5.
- Any source edit beyond the named Lock 14 fix scope or beyond 90 minutes rejects W5.

Revert protocol: fix drift inside W5 only if still inside the named scope and cap. Otherwise revert the offending wave slice or mark close blocked with exact owner paths. W5 rejection blocks W6 close.

### W6 - Close And Alpha Feedback

Entry gate: W0-W5 each have admitted, rejected, or routed status and their REDRESS/RESULTS/HANDOFF updates are present (`SPEC Section 10 - Pre-Blocked Routes`).

Hard cap: 90 minutes close reconciliation. No source edits by default.

Named rows and thresholds:

| Row set | Gate |
|---|---|
| All 38 current main rows | Final status agrees with latest admitted/rejected wave measurements and `SK-V8-open` deltas. |
| Four current real-typed GO rows | `twitter`, `update_center`, `mesh`, `marine_ik` still maintain GO. |
| Any W2-added typed rows | Status agrees with W2 REDRESS and RESULTS. |
| Any W3/W4 behavior targets | Admit/reject/routed status agrees across REDRESS, RESULTS, and HANDOFF. |

Exit gate:

- Every SK-V8 wave has admitted, rejected, or routed status.
- RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`, named REDRESS id(s) in `skinny/REDRESS.md`, and `HANDOFF Section 6 - Exit Condition` agree.
- No open brittleness window remains: no accepted source change lacks profile artifact, row threshold, REDRESS id, Lock 14 proof, or same-wave consumer proof.
- Residuals name SK-V9 or Pass Omega destinations. The SC-6-L1-R1 Lock 1 refinement is an Omega residual unless W3 waited for ratification or proved Lock 1 as written.
- No performance rerun is required unless W6 discovers a source/report inconsistency; if a rerun is required, W6 cannot close until the producing wave is reopened or routed.

Negative gates and aborts:

- Any wave without REDRESS evidence, any admitted behavior without current RESULTS rows, any unresolved W3 Lock 1 fork, any strict admission using sidecar/permissive evidence, or any typed/direct status mismatch rejects W6 close.
- Any source edit outside documentation/REDRESS/HANDOFF/SPEC reconciliation rejects W6.

Revert protocol: no source revert by default. Reopen the producing wave or mark close blocked with a close-honesty mismatch list naming file paths, rows, and missing evidence.

## §3 - Falsifiability Binding

The gates above are measurable from the bench because every numeric rule is either row-specific or formula-bound to `SK-V8-open`:

| Wave | Numeric gate | Full-table maintain |
|---|---|---|
| W0 | All 38 throughput cells within +/-1.0% of captured `SK-V8-open`; telemetry complete on all rows. | +/-1.0%; zero behavior change. |
| W1 | CostFacts evidence present and gate-consumed for every materialized JSON rule. | +/-1.0%; zero behavior change by default. |
| W2 | Existing real typed GO floors; existing direct GO floors; at least two new typed rows with `Track 1 >= ceil(sonic strict / 1.10)`. | Non-target rows no worse than -2.0% Track 1/Track 2; no correctness or verdict downgrade. |
| W3 | At least two selected structural-heavy parse rows at `Track 1 >= ceil(SK-V8-open Track 1 * 1.03)` plus measured structural-rediscovery deletion. | All 38 rows no worse than -2.0% Track 1/Track 2. |
| W4 | Every selected `N-direct` row has Track 1 and Track 2 `>= ceil(sonic strict / 1.10)`. | All non-target rows no worse than -2.0%; existing GO rows maintain GO. |
| W5 | Zero generated-output and RESULTS behavior drift unless a named Lock 14 fix is recorded. | No performance rerun unless source moved; then W0/W1 maintain rule applies to moved rows unless plan sets stricter. |
| W6 | Agreement gate: rows/status artifacts match latest wave evidence. | No source/perf change; close rejects on mismatch. |

Post-W0 recalculation rule: if W0 refreshes any same-run strict anchor or bbnf Track 1/Track 2 value, P3-F or the per-wave plan must recompute thresholds from `SK-V8-open` before redress. Copying seed thresholds after W0 changed the baseline is a gate failure.

Variance rule: a row below floor fails. A second confirm rerun is allowed only within the wave's rerun ceiling and 90-minute implementation cap. Extra reruns are REDRESS cost evidence, not retry room (`SPEC Section 10 - Pre-Blocked Routes`).

Artifact binding:

- Current row seed values: RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`.
- Current direct NO-GO rule: direct rows require both bbnf tracks within 1.10x sonic-rs time (RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`).
- Track 2 independence authority: RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`.
- Sidecar planning-only authority: RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`.
- Lazy tape/materialization and structural scan notes: RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`.

## §4 - Pre-Blocked Routes

Every wave inherits the SK-V8 pre-block list in `SPEC Section 10 - Pre-Blocked Routes`, `HANDOFF Section 7 - Pre-Blocked Routes`, and the adjacent P3-E pre-block ledger when P3-F folds the packet. A wave may reopen a listed route only with fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance. Default is closed.

Per-wave pre-blocks:

| Wave | Pre-blocks |
|---|---|
| W0 | All behavior routes. W0 cannot reopen parser/scanner/SIMD/asm/codegen/product changes. |
| W1 | Behavior routes, generic JSON policy, producer-only CostFacts, non-consumed telemetry. |
| W2 | Direct digest as typed proof; Track 2 coupling; generic JSON schema facts; REDRESS 60-72 direct-materialization coupling. |
| W3 | REDRESS 28+33, 50-55, 60-72, 82, 83, 84, 88, 89; parser-owned cursor/facts; sidecar producer; source-byte second scanner; `tape_vs_tape` as consumer; Tier B folded into Tier A. |
| W4 | REDRESS 54/55 string stats/source-hook failures; REDRESS 60-72 direct cap/sink variants; REDRESS 80 raw float shortcut; Track 2 coupling. |
| W5 | Lock 14 residues from REDRESS 36/37/38; any generic JSON branch or public API leak; generated behavior drift disguised as audit. |
| W6 | Paper close: missing REDRESS, missing RESULTS row, unresolved Lock 1/Omega fork, undocumented residual, or artifact disagreement. |

Specific REDRESS anchors:

- REDRESS 28 and 33 keep Class A tiny-string NEON wiring out of parse close (named REDRESS id(s) in `skinny/REDRESS.md`).
- REDRESS 50-55 reject parse-time aux side tables, event cursors, parser-local structural cursors, decoded-string stats sinks, and quote-source fused string materializers (named REDRESS id(s) in `skinny/REDRESS.md`).
- REDRESS 80 rejects stale canada mantissa-widen/fallback assumptions; W4/W2 may not claim raw float shortcut closure without fresh row evidence (named REDRESS id(s) in `skinny/REDRESS.md`).
- REDRESS 82-84 reject single-quartet unicode, generated StringBlock16 tiny probe, and object-pair value-byte control compaction (named REDRESS id(s) in `skinny/REDRESS.md`).
- REDRESS 88 and 89 reject PMULL prefix-XOR default hot body and CTZ/bulk production consumer after parse-row regressions (named REDRESS id(s) in `skinny/REDRESS.md`).
- REDRESS 90 admits B6 stack-canary Stage 1 only and keeps bitmap bodies rejected (named REDRESS id(s) in `skinny/REDRESS.md`).

## §5 - Sources

- `restart/prompts/ORCHESTRATOR.md` - CH1-CH6/§3W, §3Z convergence, strict profile-first non-negotiables.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` - P3-C scope, challenge lenses, SK-V8 axes.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` - wave plan/redress, challenge, same-wave consumer, role separation.
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md` - adjacent P3-B W0-W6 ordering and W4 disposition dependency.
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md` - adjacent P3-E route ledger to be folded by P3-F.
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md` - opening state, S-P2 convergence, candidate posture.
- `restart/skinny/tranches/sk-v8/SPEC.md` - close condition, comparator/telemetry, wave gates, pre-blocks, G-Alpha.
- `restart/skinny/tranches/sk-v8/HANDOFF.md` - S-P3 readiness, W0-only dispatch, substrate finding, entry/exit/pre-blocks.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md` - offset tape is not the ceiling; structural rediscovery is the defect.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md` - Tier A/Tier B split and strict row posture.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md` - Tier A owner/cost/consumer/risk gates.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md` - string-plane evidence remains diagnostic until separately gated.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md` - parse_only demotion and strict-admission refusal.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` - Lock 1 fork, sidecar drift, grammar-neutral ordinals, Omega residual.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md` - V6/V7 governance and preserved S-P3 boundaries.
- `restart/skinny/tranches/sk-v8/research/alpha/` - Alpha row extraction, comparator matrix, REDRESS digest, candidate posture, draft wave contract.
- `skinny/RESULTS.md` - authoritative row values, direct threshold rule, Track 2 independence, sidecar provenance.
- `skinny/REDRESS.md` - pre-blocked routes and rejected/admitted SK-V7 evidence.

## §6 - Self-Verdict

Verdict: ACCEPT.

Confidence: 92%.

Blockers: none for P3-C gate production. Implementation remains blocked until G-Alpha/W0 and the per-wave entry gates above. Coordination residual: if P3-F changes the P3-B W0-W6 wave set during fold, it must reconcile this artifact before dispatch.

Disposition of V6/V7 governance: preserved. V6 and V7 are treated as the two qualifying S-P2 ACCEPT cycles authorizing S-P3 only. They do not authorize W3, implementation, G-Alpha close, new substrate, relaxed strictness, or automatic wave dispatch.

Residual non-blocking risks:

- W3 seed +3.0% parse floors are deliberately conservative substrate-admission thresholds, not SOTA admission thresholds; W0 profiles may require P3-F to tighten them.
- The 90-minute implementation cap may force P3-B/P3-F to split W2/W3 if their owner-path plans are broader than this gate permits.
- `parse_only` enum cleanup (`K` to `S`) is optional for W0, but strict-admission refusal is not optional.

Required folds if REVISE:

- Add P3-B wave deltas if P3-F changes the current W0-W6 manifest during fold.
- Recompute every seed Mbps floor from W0 `SK-V8-open` if W0 updates same-run strict anchors or current bbnf row values.
- Tighten W3 selected-row thresholds with W0 hot-leaf sample-cost data if +3.0% no longer falsifies structural rediscovery deletion.
