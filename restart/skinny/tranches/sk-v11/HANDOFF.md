# Handoff SK-V11

Date: 2026-05-20.

Status: G-Alpha is presented, W0 refreshed the SK-V11-open native baseline
from HEAD `3ce75df4`, S-P1 Profile converged under
`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`,
S-P2 Research converged under
`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`,
and S-P3 Synthesis-Plan converged under
`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md`.
`SPEC.md` and `DISPATCH-PROMPT.md` are active wave authority. Behavior source
work may land only through the selected wave's entry gate and owner paths.

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
11. `skinny/REDRESS.md` through REDRESS 115
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

Next move: ready-for-wave-W5-research.

W0 is closed by S-P1/W0 authority in SPEC §3. W1a is admitted by REDRESS 111:
the companion non-JSON gate/report lane exists and does not move JSON rows or
claim generated non-JSON baseline authority. W1b is rejected by REDRESS 112:
the accepted CSS L4 direct baseline target could not produce generated non-JSON
Track 1 inside the W1b owner surface because skinny codegen/runtime remains
JSON-profiled and no generated CSS L4 runtime exists. W2 may not create the
first measurable non-JSON baseline row. W2 is therefore BLOCKED by REDRESS 113:
the non-JSON generated-intervention axis has no measurable baseline in SK-V11,
and direct-plane waves may continue only with that blocked route carried
explicitly into W8/W9 close. W3 is rejected by REDRESS 114: the scalar
`number_span_emit_slot` implementation passed semantic/gate checks but
Criterion measured `mesh/direct_to_struct` at 3835 Mbps Track 1 and 3614 Mbps
Track 2 versus the 8675 Mbps W3 floor. The rejected source patch is saved at
`/tmp/skv11-waveW3-rejected.patch`, no `RESULTS.md` row moved, and W4 carried a
measured W3 disposition into its entry gate. W4 is rejected by REDRESS 115: the
scalar `container_tail_next` implementation passed malformed-tail, parity,
gate/report, and compile checks, but probe-first measurement falsified
`random/direct_to_struct` before Criterion at 3518 Mbps Track 1 and 3498 Mbps
Track 2 versus the 7878 Mbps W4 floor. The rejected source patch is saved at
`/tmp/skv11-waveW4-rejected.patch`, no `RESULTS.md` row moved, and W5 now has a
measured W4 disposition for its entry gate.

## 5. Active Wave Packet

The active S-P3 packet consumes only fresh S-P1 and converged S-P2 evidence. The
accepted S-P2 pool is:

- C1-C7 parser primitive pool: byte-set/class-table masking, bounded
  special-byte string scan, escape/hex segment decode, digit span/accumulate,
  byte-set layout skip, generated FIRST/prefix/lookahead dispatch, and
  movemask/bitmap support only with a same-wave C1/C2/C6 consumer.
- C8 output digest/hash oracle or per-product host sink only.
- C9 Lock-1/output-plane accounting only.
- `HEX_QUARTET_X4_PROOF`, PRFM/STNP/cache hints, PMULL/CTZ, and EOR3/BCAX as
  proof/support/inventory only until a wave names a full source delta, scalar
  oracle, strict parity/checkasm, feature/fallback, same-wave consumer, and row
  gate.

Pre-wave micro-proofs are research artifacts, throwaway `/tmp` benches, or
existing bench invocations. Durable micro-proof harnesses or production changes
land only in the selected S-P3-authored wave.

No wave may reopen W3 union/event/class-column/streaming-cursor repair,
parse-only SOTA movement, x86 implementation work, JSON-only generic policy, or
non-JSON generality by prose. The live `json_provider` codegen path is a Lock 14
gate before any CSS L4 / Sheets / BBNF-self generated-parser proof can admit.

## 6. Refusal Conditions

Refuse any dispatch that:

- edits source before the selected wave entry gate passes;
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
admitted non-JSON grammar intervention. The implementation packet is active;
the immediate work is W1a with full triumvirate discipline.
