# SK-V12 Grand Synthesis

Date: 2026-05-20.

Status: Pass Alpha SK-V11 -> SK-V12 alpha-F draft. This file is the SK-V12
opening synthesis and goalset. It is not behavior implementation authority and
does not create `SPEC.md` or `DISPATCH-PROMPT.md`; S-P3 owns the later wave
plan after S-P1 and S-P2 converge.

## Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`

## Section 0 - Close Condition And Goalset

SK-V12 closes only when all of these are true:

1. Pass Alpha SK-V11 -> SK-V12 is presented at G-Alpha or pinned by the user,
   then SK-V12 S-P1 Profile, S-P2 Research, and S-P3 Synthesis-Plan converge
   under `restart/prompts/ORCHESTRATOR.md` Section 3Z.
2. S-P1 freezes a coherent SK-V12-open baseline from the SK-V11 measured close.
   The seed authority is REDRESS 120: unchanged `N-direct / NoGo`, no
   `skinny/RESULTS.md` row movement, no behavior source change, and no
   generated non-JSON admission (`skinny/REDRESS.md:3531`,
   `skinny/REDRESS.md:3535`, `skinny/REDRESS.md:3545`).
3. Generated non-JSON baseline comes first. SK-V12 must stand up exactly one
   generated non-JSON direct or typed parser baseline before any JSON-only
   micro-wave dispatches. Preferred order: CSS L4 declaration values, Sheets,
   then BBNF-self. The baseline must have generated Track 1, independent Track 2
   or oracle, strict output equality, finite same-run throughput, generated
   input provenance, run/build/host/sample telemetry, gate/report consumption,
   and no JSON policy leak into generic crates or runtime outside generated
   per-grammar modules.
4. Grammar-generalized measured intervention comes second. At least one
   generated non-JSON direct or typed parser intervention must consume the
   baseline row and clear `ceil(baseline_mbps * 1.01)` on the same output plane,
   unless S-P3 sets a stricter threshold. The independent oracle must remain
   independent, strict equality must pass, and the gate must consume the
   baseline-to-intervention delta.
5. Existing admitted JSON product rows are guarded. The 4
   `direct_to_struct A / GO` rows and 7 `real_typed_struct A / GO` rows remain
   admitted unless a same-wave gate records a measured maintain, lift, or
   demotion disposition. Guard floors seeded from SK-V11 remain active until
   S-P1/S-P3 refresh them.
6. `parse_only` is diagnostic only. The 16 `S / NO-GO` rows and 1 `L / NO-GO`
   row may guide profiles, compatibility checks, and parser health, but they
   cannot count as SOTA admission or close SK-V12.
7. JSON direct residual rows are pre-blocked by REDRESS 119. A residual direct
   row may reopen only when S-P1/S-P2 name fresh material evidence beyond
   REDRESS 114-119: new hot-leaf evidence, a source delta materially different
   from W3-W7, scalar/oracle proof, same-host microbench, independent Track 2,
   strict same-run sonic-rs direct floor, and same-wave gate consumption.
8. W3 and parse-only routes stay closed. No union/event/class-column/streaming
   cursor/class-lane/sidecar substrate, `UnionTape`, retained structural vector,
   W4-through-W3 cascade, parse-only SOTA claim, or renamed route may dispatch.
9. Telemetry is bound before behavior work. Every new non-JSON row, field, or
   companion report must be consumed by a same-wave gate. Producer-only fields,
   stale run ids, oracle coupling, and JSON policy leaks fail closed.
10. Close docs agree at close: `skinny/RESULTS.md`, `skinny/REDRESS.md`,
    `SYNTHESIS.md`, `HANDOFF.md`, and the later S-P3-authored implementation
    packet must record the same result surface and routed remainder.

Close target: SK-V12 is not a JSON direct retry. It closes by admitting one
generated non-JSON baseline and one measured grammar-generalized intervention
while preserving JSON guards, or by recording a measured `BLOCKED` route that
proves the generated non-JSON baseline cannot be created inside the accepted
SK-V12 owner surface. It may not spend another JSON-only cycle first.

### 0.1 Current Result Surface

The current seed result surface is SK-V11 close:

| Family | SK-V11 close state | SK-V12 role |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | guards plus pre-blocked residual fixpoint |
| `real_typed_struct` | 7 `A / GO` | product-plane guard surface |
| non-JSON generated parser | no admitted generated baseline | first material target |
| Overall | `N-direct / NoGo` | seed outcome |

### 0.2 Direct Residual Pre-Block Surface

The table below is carried from REDRESS 119. It is not the SK-V12 first target;
it is the reopen ledger.

| Row | Track 1 | Track 2 | sonic direct | floor | SK-V12 state |
|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | pre-blocked by W5/W7/W8 fixpoint |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | pre-blocked by W3/W8 fixpoint |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | pre-blocked by W5/W7/W8 fixpoint |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | pre-blocked by W5/W7/W8 fixpoint |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | pre-blocked by REDRESS 114 |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | pre-blocked by REDRESS 115 |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | pre-blocked by W5/W7/W8 fixpoint |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 | W0-clamped; docs-only admission pre-blocked |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 | W0-clamped; W3 route rejected |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | W0-clamped; W6 route blocked |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | pre-blocked by W5/W6 and prior proof-only limits |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | pre-blocked by W5/W7/W8 fixpoint |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | pre-blocked by W5/W6/W8 fixpoint |

### 0.3 Guard Rows

Seed direct guard floors:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Seed typed guard floors:

| Row | Track 1 maintain | Track 2 oracle guard |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

S-P1 may refresh these floors for the SK-V12-open host run, but no admitted
row may silently disappear from the guard set.

## Section 1 - Corrected Diagnosis

SK-V11 proved the direct residual surface exhausted inside its bracket.
REDRESS 114 measured the numeric slot route below the `mesh` floor; REDRESS
115 measured the container-tail route below the `random` floor; REDRESS 116,
117, and 118 blocked the string, escaped-segment, and output-digest routes
before source redress; REDRESS 119 then closed all 13 residual rows as a
measured direct fixpoint (`skinny/REDRESS.md:3375`,
`skinny/REDRESS.md:3402`, `skinny/REDRESS.md:3413`,
`skinny/REDRESS.md:3436`, `skinny/REDRESS.md:3464`,
`skinny/REDRESS.md:3497`).

The unresolved thesis is no longer "try another JSON direct primitive." It is
the generator thesis: SK-V11 could consume a non-JSON report lane but could not
create a generated non-JSON baseline or a measured grammar-generalized
intervention (`skinny/REDRESS.md:3545`). SK-V12 therefore starts at the
codegen/runtime/harness boundary that REDRESS 112 exposed.

## Section 2 - Candidate Space For S-P1/S-P2

Alpha-F does not choose waves. It names the candidate space S-P1 must profile
and S-P2 must ground:

| Candidate family | Required first proof before S-P3 |
|---|---|
| Generated CSS L4 declaration-value baseline | Generated Track 1 direct/typed parser, independent oracle, strict equality, gate-consumed finite Mbps, no JSON policy leak |
| Generated Sheets baseline | Same proof surface if CSS L4 remains blocked by owner or harness absence |
| Generated BBNF-self baseline | Same proof surface if it is the smallest authoritative generated grammar |
| Grammar-generalized intervention | Consumes the generated baseline and clears at least `ceil(baseline_mbps * 1.01)` with the same oracle and output plane |
| JSON direct residual reopen | Only after fresh material evidence beyond REDRESS 114-119 and only after the non-JSON baseline/intervention priority is satisfied or explicitly blocked |
| Aarch64 SIMD/helper support | Only if tied to the generated non-JSON row or a reopened JSON row, with scalar/oracle proof, same-host microbench, feature/fallback, and same-wave consumer |

## Section 3 - Telemetry Binding

SK-V12 inherits the SK-V11 schema-v3 discipline and the outcome enum
`A C G I J K L M N-direct S`. The gate may render fewer physical columns only
when the folded evidence remains reconstructable and validator-consumed.

The generated non-JSON baseline and intervention telemetry must bind:

- grammar id, domain, corpus or workload, row id, output plane, and workload
  class;
- generated Track 1 source path, generated input provenance, and generated
  runtime path;
- independent Track 2 or oracle source path and independence status;
- strict output equality result and comparator/oracle status;
- Track 1 Mbps, Track 2/oracle Mbps, run id, host triple, feature mask, build
  flags, sample count, sample cost, and benchmark artifact path;
- baseline row id and threshold for the intervention row;
- JSON guard state when JSON reports are refreshed;
- `wave_id`, `redress_entry`, same-wave consumer class, and fail-closed gate
  status.

The gate must reject missing required fields, unsupported outcomes, stale or
non-uniform run ids, producer-only telemetry, oracle coupling, generated Track
1 / Track 2 dishonesty, parse-only SOTA claims, W3 reopen claims, direct digest
as typed proof, and JSON policy leakage into generic crates or runtime outside
generated per-grammar modules.

## Section 4 - Pre-Blocked Routes

- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate
  family, including `UnionTape`, retained structural vectors, parser-owned
  projections, and W4-through-W3 cascade-lock.
- Parse-only SOTA close or parse-only row admission.
- JSON direct residual row movement without fresh material evidence beyond
  REDRESS 114-119.
- W0-clamped direct admission by docs-only accounting.
- Direct digest evidence as typed product proof.
- Direct or typed row admission by analogy from another row.
- Replays of `number_span_emit_slot`, `container_tail_next`, bounded string
  span, decoded-byte source folds, or output-digest host-sink without material
  differential and new row/oracle evidence.
- PMU, cycles, structural-scan, masking probes, Criterion slope, sidecar
  freshness, or parser inventory as behavior producers.
- JSON policy in generic crates or runtime outside generated per-grammar code.
- New directive, BIR variant, `BackendShape`, public substrate API, parser-owned
  sidecar/fact slot, or second retained substrate.
- x86 implementation work.

## Section 5 - Refusal Conditions

Refuse any dispatch that:

- asks Alpha-F to create `SPEC.md` or `DISPATCH-PROMPT.md`;
- edits source before the selected S-P3 wave entry gate exists and passes;
- schedules JSON-only direct work before the generated non-JSON baseline and
  measured grammar-generalized intervention priority is satisfied or explicitly
  blocked;
- reopens W3, parse-only SOTA, or a direct residual route without the evidence
  required in Section 0;
- admits a direct row without strict same-run sonic-rs direct evidence,
  generated Track 1, independent Track 2, output-plane match, provenance, and
  gate consumption;
- admits a typed row from direct digest evidence;
- claims grammar generalization by prose, hand-only parser code, stale witness
  modules, or producer-only telemetry;
- weakens admitted typed/direct guard rows without a measured gate disposition;
- emits new telemetry fields without a same-wave gate consumer;
- adds a directive, BIR variant, public substrate, parser-owned sidecar, second
  retained substrate, or generic-crate JSON policy;
- targets x86 implementation work.

## Section 6 - S-P1 Dispatch Requirements

S-P1 opens from this contract and must produce fresh evidence, not inherited
SK-V11 explanations:

- freeze the SK-V12-open JSON result surface and verify that SK-V11 close
  remains unchanged unless the profile explicitly records a new measured
  surface;
- profile admitted direct and typed guard rows and propose maintain floors;
- treat `parse_only` as diagnostic only;
- inventory the generated non-JSON codegen/runtime blocker from REDRESS 112,
  including the current `json_provider` path and the absence or presence of
  generated CSS L4, Sheets, and BBNF-self runtimes;
- audit the accepted non-JSON gate/report lane from REDRESS 111 and identify
  the first gate command S-P3 can bind to a generated baseline;
- identify the smallest generated non-JSON baseline candidate with an
  independent oracle and runnable benchmark path;
- record JSON direct hot-leaf data only as diagnostic unless a row passes the
  REDRESS 114-119 material-reopen rule;
- separate microbench inventory from dispatch authority.

## Section 7 - Dispatch Boundary

Next move: S-P1 Profile for SK-V12 after G-Alpha presentation or user pin.

This file authorizes no source work. S-P3 later authors `SPEC.md` and
`DISPATCH-PROMPT.md`; no implementation wave exists until that packet
converges.
