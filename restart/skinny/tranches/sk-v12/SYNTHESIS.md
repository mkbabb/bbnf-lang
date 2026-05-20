# SK-V12 Grand Synthesis

Date: 2026-05-20.

Status: Pass Alpha SK-V11 -> SK-V12 is re-bracketed under
`USER-PIN-W1-CSS-L4-SOTA.md`. This file is the pin-aware SK-V12 opening
synthesis and goalset. It is not behavior implementation authority and does
not create or edit `SPEC.md` or `DISPATCH-PROMPT.md`. After G-Alpha
authorization, SK-V12 runs S-P1 Profile, S-P2 Research, and S-P3
Synthesis-Plan under the user pin; only the downstream S-P3 packet may
materialize replacement implementation authority.

## Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/SPEC.md` as pre-pin context only where it
  does not conflict with the user pin
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`
- The six 2026-05-20 audits in `restart/skinny/tranches/sk-v12/research/`:
  `skv12-W1-A7-sheets-execution-scout.md`,
  `skv12-aarch64-simd-coverage-audit.md`,
  `skv12-profile-truth-audit.md`, `skv12-value-api-audit.md`,
  `skv12-decision-engine-audit.md`, and
  `skv12-totality-fold-scout.md`.

## Section 0 - Close Condition And Goalset

SK-V12 closes by ADMIT or FIXPOINT. No other close shape is valid.

### 0.1 ADMIT

ADMIT requires all of the following:

1. G-Alpha authorizes the pin-aware contract, then SK-V12 S-P1, S-P2, and S-P3
   reconverge under the user pin with CSS L4 as the authoritative first
   generated grammar target.
2. A generated CSS L4 row admits with generated Track 1 throughput strictly
   greater than `lightningcss_mbps + 1` on the same corpus, same output plane,
   same host, and strict equality semantics.
   The selected plane is represented by one canonical CSS fact stream shared
   symmetrically by generated Track 1, independent Track 2/oracle, and
   lightningcss.
3. Strict equality passes against an independent oracle or Track 2, and the
   gate consumes provenance for the generated source, generated runtime,
   grammar/input checksums, oracle path, lightningcss comparator artifact, run
   id, host, build flags, feature mask, sample count, Track 1 Mbps, oracle
   Mbps, equality result, profile artifact, JSON guard state, wave id, and
   REDRESS id.
4. Lock 14 passes by executable evidence. The generic-crate leaks identified in
   `skv12-value-api-audit.md` are resolved through `GrammarConfig` or an
   equivalent grammar-derived metadata surface before CSS L4 emission is legal.
5. Lock 16 passes for any SIMD/ASM admission. The `escape_mask_64` NEON
   correctness bug identified by `skv12-totality-fold-scout.md` is verified
   and resolved before a new SIMD row admission. Every admitted primitive has
   scalar reference, checkasm/parity, micro-proof, and same-wave consumer.
6. The carried aarch64 orphan set named by
   `skv12-aarch64-simd-coverage-audit.md` is zero by admission, removal, or
   inventory demotion with evidence. The set is `bitmap_prefix_xor_64`,
   `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_context`, and
   `cache_hints`.
7. JSON guard floors hold or any miss is recorded as a measured disposition in
   REDRESS. `parse_only` remains diagnostic-only and cannot supply SOTA
   admission.
8. Close docs agree: `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SYNTHESIS.md`,
   `HANDOFF.md`, and the later S-P3-authored packet carry the same close state.

### 0.2 FIXPOINT

FIXPOINT requires a full Pass Alpha bracket proving ADMIT uncloseable and all
of the following:

1. CSS L4 has a measured redress attempt. Sheets and BBNF-self are fallback
   candidates only after that CSS attempt records BLOCKED or REJECTED evidence.
2. The closing tranche records a new measured union-substrate implementation
   attempt. It must cite REDRESS 96/97/98, name the material differential, pass
   CHALLENGE, and include fresh profile, microbench, equality/parity, and
   same-wave consumer evidence.
3. The closing tranche records a new measured ASM-gen implementation attempt.
   It must cite REDRESS 88/89/90 when adjacent, name the material differential,
   pass CHALLENGE, and include scalar reference, checkasm/parity, microbench,
   equality, and same-wave consumer evidence.
4. The aarch64 orphan set from `skv12-aarch64-simd-coverage-audit.md` is zero
   at close by admission, removal, or explicit inventory demotion with evidence.
   Orphan production primitives make FIXPOINT invalid.
5. REDRESS names every measured miss and the routed remainder is explicit
   enough for SK-V13 if the campaign continues.

Close target: generated CSS L4 > lightningcss. The previous
`ceil(baseline_mbps * 1.01)` target is not sufficient for CSS L4 admission.

### 0.3 Current Result Surface

The current seed result surface is SK-V11 close, carried in
`skinny/RESULTS.md` and REDRESS 120:

| Family | SK-V11 close state | SK-V12 role |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | JSON guard and routed ledger |
| `real_typed_struct` | 7 `A / GO` | JSON typed guard surface |
| CSS L4 generated parser | no admitted row | authoritative first target |
| Sheets / BBNF-self | no admitted row | post-CSS-redress fallback only |
| Overall | `N-direct / NoGo` | seed outcome |

Local citations: the live JSON rows are rendered in `skinny/RESULTS.md:5-45`,
the unchanged overall outcome and Track 2 independence notes are at
`skinny/RESULTS.md:143-146`, and REDRESS 120 records the SK-V11 close/fixpoint
surface at `skinny/REDRESS.md:3531-3553`.

### 0.4 Guard Rows

Seed direct guard floors remain active until S-P3 refreshes them under the pin:

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

Guard citations: admitted direct/typed Mbps are sourced from
`skinny/RESULTS.md:6-45`; the direct residual fixpoint table is REDRESS 119 at
`skinny/REDRESS.md:3495-3527`; the pre-pin SPEC seed posture appears in
`restart/skinny/tranches/sk-v12/SPEC.md:169-180` and is retained only where it
does not conflict with the user pin.

Guard refresh rule: if a wave changes generic runtime, codegen,
generated-output, benchmark, report, or gate paths that can produce JSON, it
must refresh the JSON guard run or record a measured REDRESS demotion. The
no-refresh shortcut is legal only when no JSON-producing path moved and
`skinny/RESULTS.md` is proven unchanged.

## Section 1 - Corrected Diagnosis

SK-V11 closed the JSON direct residual surface as a measured fixpoint, but the
user pin changes the SK-V12 target. The route is no longer "try Sheets because
CSS preflight is hard" and no longer "avoid union/ASM-gen categories because
older implementations failed." The route is:

- create generated CSS L4 Track 1;
- compare against lightningcss on the same row and output plane;
- resolve generic JSON policy leaks before CSS L4 emission;
- resolve the known SIMD correctness blocker before SIMD admission;
- use union and ASM-gen categories when a new plan can cite prior REDRESS,
  name material differential, and measure a same-wave consumer.

The profile truth audit is binding against narrative reuse. S-P3 must use fresh
TSVs and gate-consumed artifacts, not inherited prose claims about hot leaves
or PMU behavior.

## Section 2 - Candidate Space For S-P3

Alpha-F does not choose the implementation wave plan. It constrains the
candidate space S-P3 must re-derive:

| Candidate family | Pin-aware requirement |
|---|---|
| CSS L4 generated baseline/admission | First target; generated Track 1 > `lightningcss_mbps + 1`; strict equality; gate-consumed provenance |
| Sheets baseline | Fallback only after CSS L4 redress attempt records measured BLOCKED/REJECTED evidence |
| BBNF-self baseline | Fallback only after CSS L4 redress attempt and after Sheets is either rejected or explicitly routed |
| GrammarConfig / generated metadata | Required before CSS L4 emission can legally escape generic JSON policy |
| Union substrate | Category unblocked; must cite REDRESS 96/97/98 and provide new measured material differential |
| ASM-gen / ARMv9.2-A | Category unblocked; must cite REDRESS 88/89/90 where adjacent, fix `escape_mask_64` before SIMD admission, and wire same-wave consumer |
| JSON direct residual | Guard/routed ledger unless a pin-relevant CSS or guard-row consumer supplies fresh material evidence |

## Section 3 - Telemetry Binding

The CSS L4 row may be rendered in `skinny/RESULTS.md` or a same-wave
gate-consumed companion report. The gate must reject missing or stale:

- `schema_id`, `row_id`, `grammar_id`, `domain`, `workload`, `output_plane`,
  and `strictness`;
- generated Track 1 source path and generated runtime path;
- grammar source or generated metadata checksum;
- fixture/input provenance and byte count;
- independent oracle or Track 2 source path and independence status;
- lightningcss comparator command, artifact, strictness, output plane, and
  Mbps;
- strict equality result and measured validation path;
- Track 1 Mbps, oracle Mbps, sample count, sample cost, run id, host triple,
  feature mask, build flags, profile artifact, and benchmark artifact;
- Lock 14 status, Lock 16 status where applicable, same-wave consumer class,
  JSON guard state, gate status, wave id, and REDRESS id.

Producer-only fields, stale run ids, oracle coupling, grammar-name branches in
generic crates, missing lightningcss comparator evidence, parse-only admission,
or orphan SIMD primitives reject the wave.

## Section 4 - Pre-Blocked And Unblocked Routes

Still blocked:

- `parse_only` SOTA admission.
- Sheets or BBNF-self before a CSS L4 redress attempt.
- CSS admission against `ceil(baseline_mbps * 1.01)` instead of
  `lightningcss_mbps + 1`.
- Generic-crate JSON policy, hand-only non-JSON witnesses, stale report lanes,
  or producer-only telemetry as grammar-generalization proof.
- New directive, BIR variant, BackendShape variant, public substrate API,
  parser-owned sidecar, or x86 implementation work.

Unblocked at category level by the user pin:

- Rust union substrate routes adjacent to REDRESS 96/97/98.
- ASM-gen routes adjacent to REDRESS 88/89/90.

Specific historical implementations remain measured evidence. Reopening a
category requires REDRESS citation, material differential, CHALLENGE, scalar or
reference proof, microbench, equality/parity, and same-wave consumer.

## Section 5 - W0 Revalidation

W0 telemetry/gate lock at commit `f788eb97` is revalidated, not redone. S-P3
must treat W0 as live unless the revalidation command proves drift in the
gate/report surface, source baseline, or JSON result surface. A revalidation
miss returns to S-P3; it does not authorize rewriting W0 inside Alpha-F.

## Section 6 - Pass Dispatch Requirements

Next move after this Alpha re-bracket converges: present G-Alpha, then run
SK-V12 S-P1 Profile, S-P2 Research, and S-P3 Synthesis-Plan under the user pin.
Pre-pin S-P1/S-P2 evidence may be referenced only after measured revalidation;
fresh TSV/profile and research convergence remain required before any wave
scope is dispatch authority.

S-P3 must:

- select CSS L4 first and specify the exact row, output plane, lightningcss
  comparator, oracle, benchmark, gate, generated paths, and rollback slice;
- require `GrammarConfig` or equivalent generated metadata before CSS L4
  emission;
- carry Sheets/BBNF-self only as post-CSS-redress fallbacks;
- treat union and ASM-gen categories as admissible under pin rules;
- verify and resolve the `escape_mask_64` SIMD correctness blocker before any
  SIMD admission;
- preserve W0 as revalidated unless drift is measured;
- leave `SPEC.md` and `DISPATCH-PROMPT.md` for the downstream S-P3 packet.

## Section 7 - G-Alpha Summary

Pin-aware G-Alpha asks for this pass sequence and wave seed:

| Seed | Target row / role | Hand LOC cap | Minute caps | REDRESS adjacency | Close contribution | Failure action |
|---|---|---:|---|---|---|---|
| S-P1 | Fresh JSON 17-corpus plus CSS L4 target profile | docs/profiling | pass prompt | profile-truth audit | establishes open baseline | rerun until §3Z or BLOCKED |
| S-P2 | CSS/lightningcss, Lock 14/16, union, ASM-gen research | docs | pass prompt | REDRESS 88/89/90, 96/97/98, 112/113, 119/120 | names legal routes | rerun until §3Z or BLOCKED |
| S-P3 | SPEC + DISPATCH-PROMPT under pin | docs | pass prompt | all carried | materializes wave authority | REVISE on stale pre-pin gate |
| W0 | Revalidate `f788eb97` telemetry/gate lock | docs-only | 20/15/30 | W0 lock | preserves open surface | return to S-P3 on drift |
| W1a | `GrammarConfig` legality / JSON parity | <=360 | 20/15/30 | Lock 14 leaks | legalizes CSS emission | save `/tmp/skv12-waveW1a-rejected.patch` |
| W1b | CSS L4 generated baseline + lightningcss comparator | <=620 | 20/15/30 | REDRESS 112/113 | possible ADMIT | save `/tmp/skv12-waveW1b-rejected.patch` |
| W2 | `escape_mask_64` correctness | <=180 | 20/15/30 | Lock 16 bug | unblocks SIMD | save `/tmp/skv12-waveW2-rejected.patch` |
| W3 | CSS-local same-tape union attempt | <=420 | 20/15/30 | REDRESS 96/97/98 | ADMIT lift or FIXPOINT union evidence | save `/tmp/skv12-waveW3-rejected.patch` |
| W4 | ARMv9.2 TBL/TBX or selected ASM-gen consumer | <=430 | 20/15/30 | REDRESS 88/89/90 | ADMIT lift or FIXPOINT ASM evidence, zero-orphan disposition | save `/tmp/skv12-waveW4-rejected.patch` |
| W5 | Close / G-Alpha feedback | docs-only | 20/15/30 | REDRESS close | ADMIT or measured FIXPOINT | synthesize SK-V13 if close unmet |

ADMIT remains generated CSS L4 Track 1 strictly
`> lightningcss_mbps + 1`. FIXPOINT requires CSS L4 measured uncloseable, plus
new measured union-substrate and ASM-gen attempts in the closing tranche, with
the carried orphan set admitted, removed, or inventory-demoted.

This file authorizes no behavior source work. It replaces only the Alpha
contract surface owned by this lane.
