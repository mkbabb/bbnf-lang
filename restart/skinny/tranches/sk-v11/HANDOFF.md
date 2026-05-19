# Handoff SK-V11

Date: 2026-05-19.

Status: G-Alpha is presented and W0 refreshed the SK-V11-open native baseline
from HEAD `3ce75df4`. SK-V11 is ready for S-P1 Profile dispatch under
`restart/prompts/skinny/PASS-1-PROFILE.md`. This handoff does not authorize
source work and does not create `SPEC.md` or `DISPATCH-PROMPT.md`; S-P3 owns
those files after S-P1 and S-P2 converge.

## 1. Read First

1. `restart/prompts/ORCHESTRATOR.md`
2. `restart/prompts/pass-contracts/PASS-ALPHA.md`
3. `restart/prompts/skinny/PASS-1-PROFILE.md`
4. `restart/prompts/skinny/PASS-2-RESEARCH.md`
5. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
6. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
7. `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
8. `restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md`
9. `restart/skinny/tranches/sk-v10/research/close/close-redress.md`
10. `skinny/RESULTS.md`
11. `skinny/REDRESS.md` through REDRESS 110
12. `restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md`
13. `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
14. `restart/skinny/tranches/sk-v10/HANDOFF.md`
15. `restart/skinny/tranches/sk-v10/SPEC.md`
16. `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`

## 2. Current State

SK-V11-open is the measured authority:

| Family | State | Role in SK-V11 |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic concession; no SOTA target |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | primary JSON closure target |
| `real_typed_struct` | 7 `A / GO` | guard and product-plane credibility surface |

Overall remains `N-direct / NoGo` at W0
(`sk-v9-open:criterion-fnv64-c8d7e0468358f98c`).

The 13 direct residual rows to profile first are:

| Row | Track 1 | Track 2 | sonic direct | Seed floor |
|---|---:|---:|---:|---:|
| `twitter` | 11613 | 10816 | 15113 | 13740 |
| `canada` | 10316 | 9819 | 11700 | 10637 |
| `github_events` | 11918 | 10596 | 14743 | 13403 |
| `update_center` | 8187 | 7474 | 11064 | 10059 |
| `mesh` | 8561 | 8652 | 9542 | 8675 |
| `random` | 7693 | 6949 | 8665 | 7878 |
| `gsoc-2018` | 2665 | 2578 | 4110 | 3737 |
| `instruments` | 11569 | 10736 | 9865 | 8969 |
| `numbers` | 4479 | 2366 | 2667 | 2425 |
| `unicode_mixed` | 3753 | 2427 | 2846 | 2588 |
| `unicode_escapes` | 1345 | 1341 | 3785 | 3441 |
| `distinct_values` | 1750 | 1625 | 2923 | 2658 |
| `y_string_unicode` | 1983 | 1029 | 4344 | 3950 |

`instruments`, `numbers`, and `unicode_mixed` are W0-clamped planning rows:
fresh numbers may clear one or both floors, but admission still requires a
behavior wave with measured provenance.

## 3. Bound Axes

SK-V11 advances three axes together:

1. Direct plane closure or fixpoint. Every residual direct row clears the
   strict same-run sonic-rs 1.10x digest gate on Track 1 and Track 2, or
   records a per-row uncloseable REDRESS proof.
2. Grammar generalization. At least one non-JSON grammar carries an admitted,
   benchmarked SK-V11 intervention through a generated direct or typed parser.
   Preferred order: CSS L4 declaration values, Sheets, BBNF-self.
3. Aarch64-only SIMD/ASM. Apple Silicon only; no x86 implementation target.
   Every kernel candidate needs scalar reference, differential/checkasm where
   applicable, same-host microbench, representative slices, feature gate, and
   same-wave consumer.

These axes are not alternatives. A row-moving direct intervention should be
preferred when it also supplies a grammar-neutral primitive or a non-JSON
consumer. A non-JSON grammar wave should exercise the same primitive family
that S-P1/S-P2 identify on the JSON direct residual surface.

## 4. Next Move

Next move: ready-for-S-P1.

S-P1 must run full orchestration per `PASS-1-PROFILE.md`:

- P1-A samply mode I over all 17 JSON corpora;
- P1-B samply mode II over direct and typed rows, with direct residual rows
  isolated;
- P1-C masking probes and structural-scan-only paths, diagnostic only;
- P1-D xctrace CPU Counters plus Time Profiler PMU and cycles-per-byte;
- P1-E hot-leaf attribution for every residual direct row and guard row;
- P1-F `RESULTS.md` extraction and delta from SK-V10 close;
- CHALLENGE CH1-CH6, folded until convergence per ORCHESTRATOR Section 3Z.

S-P1 writes under `restart/skinny/tranches/sk-v11/research/p1/` and edits no
source.

## 5. S-P2/S-P3 Expectations

S-P2 consumes only fresh S-P1 evidence. It must ground:

- SOTA comparator teardown for strict direct and typed comparators;
- dav1d/FFmpeg-style scalar-reference and checkasm discipline;
- aarch64 instruction candidates only, with x86 marked out of scope;
- substrate/tape audit that preserves the single substrate and refuses W3;
- parse-that/bbnf-simd primitive gaps tied to direct residual hot leaves;
- grammar-neutral abstraction for CSS L4 / Sheets / BBNF-self.

Pre-S-P3 micro-proofs are research artifacts, throwaway `/tmp` benches, or
existing bench invocations. Durable micro-proof harness or production changes
land only in the S-P3-authored wave packet.

S-P3 then writes the SK-V11 `SPEC.md` and `DISPATCH-PROMPT.md`. It must not
derive a wave plan directly from this Alpha-F draft without S-P1 and S-P2
convergence.

## 6. Refusal Conditions

Refuse any dispatch that:

- edits source before S-P3 converges;
- asks Alpha-F to create `SPEC.md` or `DISPATCH-PROMPT.md`;
- reopens W3 union/event/class-column/streaming-cursor/class-lane/sidecar
  substrate;
- treats parse-only `S / NO-GO` rows as SOTA admissions;
- scopes SIMD/ASM without same-host micro-proof-first;
- targets x86 implementation work;
- claims grammar generalization by prose only;
- admits a direct row without strict same-run sonic-rs direct evidence,
  generated Track 1, independent Track 2, output-plane match, provenance, and
  gate consumption;
- admits a typed row from direct digest evidence;
- emits new telemetry fields without a same-wave gate consumer;
- adds a directive, BIR variant, public substrate, parser-owned sidecar, or
  JSON policy in a generic crate.

## 7. Close Posture

SK-V11 is born from the SK-V10 measured close, not from an untested W3 repair.
The close target is direct plane `GO` or measured direct fixpoint plus one
admitted non-JSON grammar intervention. The implementation packet does not
exist yet; the immediate work is S-P1 Profile with full orchestration.
