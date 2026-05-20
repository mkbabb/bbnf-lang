# SK-V12 P3-F: SPEC + Dispatch Draft

Pass: S-P3 Synthesis-Plan. Cycle: PIN-V1.
Date: 2026-05-20.
Scope: regenerate the SK-V12 wave SPEC and per-wave dispatch prompt under the
2026-05-20 USER PIN, superseding the stale pre-pin V5 packet.
Output: this file + SPEC.md + DISPATCH-PROMPT.md.
Pass Alpha goalset: ADMIT when a generated CSS L4 row has Track 1 throughput
strictly greater than `lightningcss_mbps + 1` on the same corpus, same output
plane, same host, strict equality, independent oracle/Track 2, gate-consumed
provenance, Lock 14/Lock 16 clean, JSON guards held or measured-disposition
demoted, and zero orphan aarch64 production primitives. FIXPOINT requires a
measured CSS redress attempt plus at least one new union-substrate attempt and
one new ASM-gen attempt in the closing tranche.
Candidate pool: research/p2/ post-CHALLENGE survivors, with S-P2 convergence
recorded in `research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.

## §1 - Synthesis

The user pin changes the packet authority. CSS L4 is the authoritative first
target, not Sheets. The close bar is not a generated-baseline existence floor
and not `ceil(baseline_mbps * 1.01)`: the admitting row must beat
`lightningcss_mbps + 1` on the same corpus, same output plane, same host, and
strict equality. Sheets and BBNF-self are fallbacks only after a CSS L4 redress
attempt records measured BLOCKED or REJECTED evidence.

Pin-aware S-P1 and S-P2 have converged. S-P1 gives the fresh profile authority
from `/tmp/skv12-pin-p1` and names hot primitive families, but explicitly does
not prove CSS behavior. S-P2 narrows the selectable implementation pool:

| Family | P2 status | S-P3 consequence |
|---|---|---|
| CSS L4 generated baseline/comparator | Required first target | W1b must create generated CSS Track 1, independent oracle/Track 2, and lightningcss same-plane comparator. |
| `GrammarConfig` / generated metadata | Required legality precondition | W1a lands before CSS emission can be legal. |
| `escape_mask_64` correctness | Required before SIMD admission | W2 verifies and resolves the xorshift falsifier before W4 can admit SIMD/ASM. |
| `class_mask64_transient`, generated FIRST/follow dispatch, bounded string, digit run, hex quartet | Conditional primitive candidates | May dispatch only with scalar reference, parity/checkasm where applicable, micro-proof, and same-wave CSS consumer. |
| Same-tape union/substrate route | Category unblocked, no current ready primitive | W3 is conditional on CSS baseline + hot-leaf evidence + material differential beyond REDRESS 96/97/98. |
| ASM-gen / ARMv9.2 route | Category unblocked | W4 must attempt a CSS or JSON-guard consumed ASM-gen candidate if ADMIT is not already closed, and any close needs zero orphans. |

The stale V5 packet treated non-JSON baseline admission and a 1% lift as the
campaign close. PIN-V1 replaces that with an ADMIT/FIXPOINT close:

- ADMIT: generated CSS L4 Track 1 > `lightningcss_mbps + 1`, strict and
  same-plane, with Lock 14/16, JSON guards, and zero orphan primitives.
- FIXPOINT: CSS redress attempted, CSS uncloseable by measurement, a new
  union-substrate implementation attempted, a new ASM-gen implementation
  attempted, orphan production primitives zero, and REDRESS carries the
  material differential evidence.

## §2 - Deliverable

This P3-F cycle writes:

- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`

The SPEC mirrors the SK-V8/SK-V11 shape: close condition, comparator classes,
outcome enum, telemetry, opening goalset, non-negotiables, wave manifest,
generality gate, one section per wave, pre-blocked and reopened routes, and
dispatch/convergence scope.

Draft wave manifest:

| Wave | SPEC section | Title | Dispatch status | LOC cap | Risk | Wall cap |
|---|---|---|---|---:|---|---:|
| W0 | Section 3 | Pin Telemetry And Gate Revalidation | Dispatchable after S-P3 convergence | <=160 docs/gate/test; 0 behavior | medium | <=30 min |
| W1a | Section 4 | GrammarConfig + Lock 14 Legality Gate | Conditional on W0 close | <=360 hand + generated named separately | high | <=30 min redress |
| W1b | Section 5 | CSS L4 Generated Track 1 + Lightningcss Comparator | Conditional on W1a close | <=620 hand + generated named separately | high | <=30 min redress |
| W2 | Section 6 | `escape_mask_64` Correctness Prerequisite | Conditional on W1a close; before any SIMD admit | <=180 hand/test | high | <=30 min redress |
| W3 | Section 7 | CSS-Local Same-Tape Union Attempt | Conditional on W1b measured CSS row + CHALLENGE | <=420 hand + generated named separately | high | <=30 min redress |
| W4 | Section 8 | ASM-Gen CSS Consumer + AArch64 Orphan Disposition | Conditional on W1b + W2 + CHALLENGE | <=430 hand/test/gate | high | <=30 min redress |
| W5 | Section 9 | Close And Alpha Feedback | Conditional on W0, W1a, W1b, W2, W3, and W4 disposition | <=140 docs/report/gate | medium | <=30 min |

The wave count stays below the skinny ceiling. W3 and W4 are not optional for a
FIXPOINT close. If W1b or another wave already reaches ADMIT, W4/W5 still must
dispose the orphan set before close unless every production orphan is already
gone by admission/removal/demotion evidence.

## §3 - Falsifiability Binding

`G-W0-PIN-TELEMETRY` revalidates the pin-aware profile, W0 gate/report surface,
and JSON result seed without behavior changes.

`G-W1a-GRAMMARCONFIG-LOCK14` passes only when the seven generic JSON leaks in
`skv12-value-api-audit.md` are removed from generic code through
`GrammarConfig` or an equivalent generated metadata surface, JSON guards pass
or are measured-disposition demoted, and a gate scan proves CSS emission is
legal.

`G-W1b-CSS-L4-COMPARATOR` passes only when generated CSS L4 Track 1, independent
oracle/Track 2, and lightningcss all emit the same canonical CSS fact stream
for the selected corpus. The gate records generated Track 1 Mbps,
oracle/Track 2 Mbps, `lightningcss_mbps`, strict equality, provenance, sample
count, run id, host, build flags, generated LOC/size, JSON guard state, and
whether `track1_mbps > lightningcss_mbps + 1`. A measurable CSS row below that
bar is a measured baseline, not close.

`G-W2-ESCAPE-MASK-CORRECTNESS` passes only when the `escape_mask_64` scalar
reference, NEON body, and checkasm/corpus parity cover the xorshift falsifier
`0xCAFEF00DBAADF00D` and boundary carry cases. Until this gate passes, no new
SIMD/ASM admission is legal.

`G-W3-CSS-UNION-ATTEMPT` passes only with CSS baseline evidence, a fresh profile
or microbench naming a CSS hot leaf, CHALLENGE acceptance, REDRESS 96/97/98
citations, a material differential from the class-column/streaming-cursor/
class-lane historical attempts, single-substrate same-tape semantics, strict
CSS equality, and JSON guards. The wave may admit if it beats lightningcss; it
may reject honestly and still count as the required union attempt for FIXPOINT.

`G-W4-ASM-GEN-CONSUMER` passes only with W2 correctness complete, CHALLENGE
acceptance, a same-host microbench proving the selected ASM-gen candidate on a
CSS or JSON-guard hot leaf, scalar reference, checkasm/parity, same-wave
consumer, strict CSS equality when CSS is touched, JSON guards, and zero orphan
production primitives by the end of the wave or an explicit W5 close-block.

`G-W5-CLOSE` requires all prior waves admitted/rejected/routed with REDRESS
evidence, the ADMIT or FIXPOINT close clause satisfied, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, `SYNTHESIS.md`, `HANDOFF.md`, SPEC, and dispatch prompt
agreeing, and any SK-V13 routed remainder explicit.

## §4 - Pre-Blocked And Reopened Routes

Still blocked:

- `parse_only` SOTA admission.
- Sheets or BBNF-self before a measured CSS L4 redress attempt.
- CSS close on generated baseline existence, `>= 1 Mbps`, or
  `ceil(baseline_mbps * 1.01)`.
- Hand-only CSS parsers, report-only lanes, stale witness modules, producer-only
  telemetry, and generic-crate JSON/CSS/Sheets policy branches.
- New directive, BIR variant, `BackendShape` variant, public substrate API,
  parser-owned sidecar, decoded-byte sidecar, hidden host schema, benchmark-only
  Track 1 parser, digest-only proof, x86 implementation work, and permissive or
  stale comparator admission.
- Replays of REDRESS 111-120 unless the SPEC section names the material
  differential and the wave CHALLENGE accepts it.

Reopened at category level by the user pin:

- Union / event-model / class-column / streaming-cursor / retained structural
  family adjacent to REDRESS 96/97/98. Specific historical implementations
  remain rejected evidence; a new W3 attempt must materially differ and measure.
- ASM-gen routes adjacent to REDRESS 88/89/90. PMULL, CSSC CTZ, EOR3/BCAX,
  UDOT, TBL/TBX, or another ARMv9.2 primitive may dispatch only when scalar
  reference, checkasm/parity, micro-proof, and same-wave consumer are present.

## §5 - Sources

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
