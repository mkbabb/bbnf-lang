# SK-V12 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V12 under `USER-PIN-W1-CSS-L4-SOTA.md` (2026-05-20). It binds to the packet
at `restart/skinny/tranches/sk-v12/`. Each wave is executed by one triumvirate
per `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

Status: S-P3 CONVERGED. Dispatch authority exists for W0 first; later waves
dispatch only after their SPEC entry gates pass.

## Required Reading

Read in order:

1. `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
2. `restart/prompts/ORCHESTRATOR.md`.
3. `restart/prompts/skinny/PASS-1-PROFILE.md`.
4. `restart/prompts/skinny/PASS-2-RESEARCH.md`.
5. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
6. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
7. `restart/prompts/pass-contracts/PASS-ALPHA.md`.
8. `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
9. `restart/skinny/tranches/sk-v12/SPEC.md`.
10. `restart/skinny/tranches/sk-v12/HANDOFF.md`.
11. `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
12. `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.
13. The accepted S-P2 cohort:
    - `research/p2/p2a-sota-teardown.md`
    - `research/p2/p2b-dav1d-process.md`
    - `research/p2/p2c-arch-esoterica.md`
    - `research/p2/p2d-substrate-tape.md`
    - `research/p2/p2e-parse-that-gaps.md`
    - `research/p2/p2f-grammar-neutral.md`
14. The six 2026-05-20 audits in `restart/skinny/tranches/sk-v12/research/`:
    `skv12-W1-A7-sheets-execution-scout.md`,
    `skv12-aarch64-simd-coverage-audit.md`,
    `skv12-profile-truth-audit.md`,
    `skv12-value-api-audit.md`,
    `skv12-decision-engine-audit.md`, and
    `skv12-totality-fold-scout.md`.
15. `skinny/RESULTS.md`.
16. `skinny/REDRESS.md`.

## Pin Summary

Load-bearing user-pin rules:

- CSS L4 is authoritative. Sheets and BBNF-self are fallback-only after a
  measured CSS L4 redress attempt.
- ADMIT requires generated CSS L4 Track 1 strictly greater than
  `lightningcss_mbps + 1` on the same corpus, same output plane, same host, and
  strict equality. `>= 1 Mbps` and `ceil(baseline_mbps * 1.01)` are not close
  bars.
- Union and ASM-gen categories are unblocked at category level. REDRESS
  96/97/98 and 88/89/90 remain historical measured implementations that new
  plans must cite and materially differentiate.
- The `escape_mask_64` NEON correctness bug is resolved before any SIMD/ASM
  admission.
- The seven Lock 14 leaks resolve through `GrammarConfig` or equivalent before
  CSS emission is legal.
- Zero orphan aarch64 production primitives at close:
  `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_context`, `cache_hints`.
- JSON guards are second priority; `parse_only` is diagnostic-only; x86 is out
  of scope.

## Wave Manifest

| Wave | SPEC section | Title | Dispatch status | LOC / risk | Cap |
|---|---|---|---|---:|---:|
| W0 | Section 3 | Pin Telemetry And Gate Revalidation | First after S-P3 convergence | <=160 docs/gate/test; medium | 20m research / 15m plan / 30m redress |
| W1a | Section 4 | GrammarConfig + Lock 14 Legality Gate | Conditional on W0 | <=360 hand + generated named; high | 20m / 15m / 30m |
| W2 | Section 5 | `escape_mask_64` Correctness Prerequisite | Conditional on W1a; before SIMD admit | <=180 hand/test; high | 20m / 15m / 30m |
| W1b-1 | Section 6 | CSS L4 Generated Track 1 + Independent Oracle Scaffold | Conditional on W1a; scalar-only unless W2 passed | <=360 hand + generated named; high | 20m / 15m / 30m |
| W1b-2 | Section 7 | CSS L4 Lightningcss Comparator + Admission Gate | Conditional on W1b-1 | <=300 hand/gate + generated named; high | 20m / 15m / 30m |
| W3 | Section 8 | CSS-Local Same-Tape Union Attempt | Conditional on W1b-2 measured CSS row + CHALLENGE | <=420 hand + generated named; high | 20m / 15m / 30m |
| W4 | Section 9 | ASM-Gen CSS Consumer + AArch64 Orphan Disposition | Conditional on W1b-2 + W2 + CHALLENGE | <=430 hand/test/gate; high | 20m / 15m / 30m |
| W5 | Section 10 | Close And Alpha Feedback | Conditional on W0, W1a, W2, W1b-1, W1b-2, W4, and conditional W3 disposition | <=140 docs/report/gate; medium | 20m / 15m / 30m |

The order is firm. W2 is a correctness prerequisite after W1a and before any
new SIMD/ASM admission; W1b-1 may run before W2 only if its accepted plan proves
the CSS scaffold is scalar-only. W1b-1 and W1b-2 cannot select Sheets or
BBNF-self before a measured CSS redress attempt. W3 and W4 are required for a
FIXPOINT close. If ADMIT is achieved before W3, close may route W3 as
not-required, but W4 must still produce zero-orphan disposition and all close
docs must agree.

## Per-Wave Triumvirate Protocol

Every wave runs research, plan, CHALLENGE when required, and redress as
distinct dispatches and distinct commits. The redress agent is a single
implementation thread and implements only the owner paths named in the SPEC
section.

### Phase 1 - Research

- Up to six parallel research agents on disjoint scope rows.
- 20 min cap.
- Research writes only under `restart/skinny/tranches/sk-v12/research/` and
  edits no source.
- Commit: `docs(sk-v12-wave{W}-research): archive {scope} cohort`.

### Phase 2 - Plan

- One or two plan agents.
- 15 min cap.
- The plan names owner paths, entry gate, exit gate, scalar reference,
  checkasm/parity status, micro-proof, same-wave consumer, LOC budget, risk,
  JSON guard treatment, generated-size budget, pre-blocks/reopened routes, and
  revert protocol.
- Commit: `docs(sk-v12-wave{W}-plan): select {intervention}`.

### Phase 2.5 - CHALLENGE

Mandatory for W1a, W2, W1b-1, W1b-2, W3, W4, and any W0/W5 plan that changes
gate semantics. Six lenses review correctness, generality/Lock 14, regression
and REDRESS, cost, hidden coupling, and anti-paper-close. REJECT or unresolved
REVISE returns to plan.

W3 CHALLENGE must explicitly adjudicate the material differential from REDRESS
96/97/98. W4 CHALLENGE must explicitly adjudicate REDRESS 88/89/90 adjacency,
Lock 16, and orphan disposition.

### Phase 3 - Redress

- One redress agent.
- 30 min cap.
- Implements only SPEC owner paths. Any other source path returns REVISE before
  editing.
- Produces measurement with `RUSTFLAGS="-C target-cpu=native"` unless the SPEC
  section says the wave is docs/gate-only.
- Every primitive or generated parser path must include the same-wave consumer
  and same-wave gate.
- Commit on PASS: `feat(sk-v12-wave{W}): admit {intervention}`.
- Commit on FAIL/BLOCKED: `docs(sk-v12-wave{W}-redress): reject {intervention}`
  or `docs(sk-v12-wave{W}-redress): block {intervention}`, with REDRESS
  evidence and `/tmp/skv12-wave{W}-rejected.patch` if a patch was attempted.

## Falsifiability Gates

The SPEC is the single source for detailed gates:

- `G-W0-PIN-TELEMETRY` (SPEC Section 3)
- `G-W1a-GRAMMARCONFIG-LOCK14` (SPEC Section 4)
- `G-W2-ESCAPE-MASK-CORRECTNESS` (SPEC Section 5)
- `G-W1b-1-CSS-L4-ORACLE` (SPEC Section 6)
- `G-W1b-2-CSS-L4-LIGHTNINGCSS` (SPEC Section 7)
- `G-W3-CSS-UNION-ATTEMPT` (SPEC Section 8)
- `G-W4-ASM-GEN-CONSUMER` (SPEC Section 9)
- `G-W5-CLOSE` (SPEC Section 10)

Load-bearing facts:

- W1a is legality, not performance close.
- W1b-1 creates the exact CSS L4 generated row:
  `css_l4/declaration_values/direct_to_struct/main`, output plane
  `css_l4_declaration_value_fact_stream`, runtime path
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`.
- W1b-2 adds lightningcss measurement and the admission gate. A strict-equal CSS
  row below `lightningcss_mbps + 1` is a measured baseline, not ADMIT.
- W2 is a correctness prerequisite. It moves no row by itself.
- W3 may admit or reject, but a measured, materially differentiated reject
  counts as the union attempt required for FIXPOINT.
- W4 may admit or reject, but a measured, same-wave-consumed reject counts as
  the ASM-gen attempt required for FIXPOINT if REDRESS evidence is complete.
- Any behavior wave touching JSON-producing paths must rerun JSON guards or
  prove no JSON path moved. Guard misses require measured REDRESS disposition.

## Telemetry

CSS/non-JSON gates must consume: schema id, row id, grammar id, domain,
workload, output plane, strictness, generated source/runtime paths, grammar and
input checksums, input bytes, Track 1 Mbps, Track 2/oracle Mbps,
lightningcss Mbps, threshold Mbps, strict equality, oracle independence,
lightningcss command/artifact, run id, host, feature mask, build flags, sample
count/cost, benchmark artifact, profile artifact, generated LOC/module bytes,
O(N) size guard, Lock 14 status, Lock 16 status, same-wave consumer, scalar
reference, checkasm/parity, JSON guard state, gate status, wave id, and REDRESS
id.

Producer-only fields, stale run ids, missing lightningcss evidence, missing
independent oracle, unsupported outcomes, generic policy leaks, parse-only
admission, and orphan SIMD primitives reject the wave.

## Pre-Blocked And Reopened Routes

Still blocked:

- `parse_only` SOTA admission.
- Sheets or BBNF-self before a measured CSS L4 redress attempt.
- CSS close by generated baseline existence, `>= 1 Mbps`, or
  `ceil(baseline_mbps * 1.01)`.
- Hand-only CSS parser, report-only row, stale witness module, stale sidecar,
  producer-only telemetry, permissive comparator admission.
- New directive, BIR variant, `BackendShape` variant, public substrate API,
  parser-owned sidecar, decoded-byte sidecar, hidden host schema,
  benchmark-private parser, digest-only proof, x86 implementation work.
- Generic-crate JSON/CSS/Sheets policy branches.
- Replays of REDRESS 111-120 without material differential and CHALLENGE.
- Orphan SIMD/ASM admission or checkasm-only performance admission.

Unblocked at category level:

- Union/event/class-column/streaming-cursor/retained-structural routes
  adjacent to REDRESS 96/97/98. Specific historical implementations remain
  rejected evidence. New plans must cite, differentiate, and measure.
- ASM-gen routes adjacent to REDRESS 88/89/90. PMULL, CSSC CTZ, EOR3/BCAX,
  UDOT, TBL/TBX, or another ARMv9.2 primitive may dispatch only with scalar
  reference, checkasm/parity, micro-proof, and same-wave consumer.

## Non-Negotiables

- No CSS admission without strict same-plane lightningcss comparison.
- No SIMD/ASM admission before `escape_mask_64` correctness is resolved.
- No behavior source change without a named row gate.
- No primitive or generated parser path without scalar/reference proof,
  parity/checkasm where applicable, micro-proof, and same-wave consumer.
- No generic-crate grammar policy leak.
- No orphan production aarch64 primitive at close.
- No future-phase promise close.
- No new outcome enum value.
- Research, plan, CHALLENGE, redress, and close remain separate commits.

## Status Discipline

Before any status reply, reconcile agent status, running cargo/rustc/xctrace/
samply processes, artifact mtimes, and dirty worktree state. Stage only the
intended wave slice and preserve unrelated dirty or staged work.

Every dispatch carries a cap. At 0.9x the cap, commit or record the blocking
state. At the cap, halt and surface the decision point.

## Convergence And Escalation

SK-V12 converges when W0, W1a, W2, W1b-1, W1b-2, W4, W5, and W3 when required
for FIXPOINT have admitted, rejected, routed, or blocked with evidence, and
SPEC Section 0 ADMIT or FIXPOINT holds. If neither
holds, W5 routes a Pass Alpha SK-V12 -> SK-V13 packet and the campaign
continues.

Escalate immediately if a USER PIN clause needs amendment, lightningcss cannot
be made a same-plane strict comparator, CSS L4 requires a directive/BIR/
BackendShape expansion, x86 must re-enter scope, an admitted JSON guard
regression cannot be recovered in tranche, or the measured fixpoint requires
user choice between re-pin and honest close.
