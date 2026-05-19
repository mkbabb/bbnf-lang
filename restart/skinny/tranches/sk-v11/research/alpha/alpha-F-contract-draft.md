# SK-V11 Alpha-F Contract Draft

Pass: Pass Alpha. Cycle: V1.
Date: 2026-05-19.
Agent: alpha-F.
Scope: SK-V11 contract draft from SK-V10 close evidence.
Output: this file, plus `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
and `restart/skinny/tranches/sk-v11/HANDOFF.md`.

This draft does not create `SPEC.md` or `DISPATCH-PROMPT.md`. Per
`restart/prompts/pass-contracts/PASS-ALPHA.md`, S-P3 owns the detailed
wave plan after S-P1 and S-P2 converge.

## 1. Authorities Read

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v10/research/close/close-redress.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 110
- `restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- User SK-V11 dispatch request in this thread.

## 2. Load-Bearing Diagnosis

SK-V10 closed cleanly, but not green overall. The close authority records
17 `parse_only` rows at `S / NO-GO`, `direct_to_struct` at 6 `A / GO`
and 11 `N-direct / NO-GO`, `real_typed_struct` at 7 `A / GO`, and
overall `N-direct / NoGo` (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:39`,
`restart/skinny/tranches/sk-v10/research/close/close-redress.md:45`).

The direct plane is therefore the JSON close frontier. The eleven residual
direct rows are not a vague pool; they are named rows with current Track 1,
Track 2, sonic-rs direct, and a strict 1.10x digest floor seeded from
`skinny/RESULTS.md`.

| Row | Track 1 | Track 2 | sonic direct | seeded floor | Track 1 gap | Track 2 gap | Source |
|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | 11905 | 10968 | 15244 | 13859 | 1954 | 2891 | `skinny/RESULTS.md:6` |
| `canada` | 10590 | 10286 | 12157 | 11052 | 462 | 766 | `skinny/RESULTS.md:12` |
| `github_events` | 12439 | 11430 | 16206 | 14733 | 2294 | 3303 | `skinny/RESULTS.md:17` |
| `update_center` | 8425 | 7620 | 11186 | 10170 | 1745 | 2550 | `skinny/RESULTS.md:20` |
| `mesh` | 8562 | 8596 | 9422 | 8566 | 4 | -30 | `skinny/RESULTS.md:23` |
| `random` | 7887 | 7132 | 8948 | 8135 | 248 | 1003 | `skinny/RESULTS.md:26` |
| `gsoc-2018` | 15056 | 14534 | 23437 | 21307 | 6251 | 6773 | `skinny/RESULTS.md:28` |
| `unicode_mixed` | 4700 | 4556 | 10480 | 9528 | 4828 | 4972 | `skinny/RESULTS.md:37` |
| `unicode_escapes` | 5069 | 5222 | 14147 | 12861 | 7792 | 7639 | `skinny/RESULTS.md:39` |
| `distinct_values` | 6303 | 5654 | 11978 | 10890 | 4587 | 5236 | `skinny/RESULTS.md:43` |
| `y_string_unicode` | 5067 | 3746 | 9211 | 8374 | 3307 | 4628 | `skinny/RESULTS.md:45` |

The typed product plane is a guard and credibility surface for SK-V11, not
the only frontier. Seven typed rows are already `A / GO`; five of the seven
beat sonic-rs strict outright, while `update_center` remains admitted under
the existing 1.10x slack gate (`skinny/RESULTS.md:7`,
`skinny/RESULTS.md:10`, `skinny/RESULTS.md:15`,
`skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`,
`skinny/RESULTS.md:24`, `skinny/RESULTS.md:31`).

The parse-only plane is closed as a concession. It remains diagnostic only,
because the final SK-V10 close preserves all 17 rows as `S / NO-GO`
(`restart/skinny/tranches/sk-v10/research/close/close-redress.md:41`).

The SK-V9 W3 union substrate is not a candidate. REDRESS 98 retires
`G-W3-UNION-SUBSTRATE`; REDRESS 96 and 97 are recorded as two faithful,
correctness-green implementations that both regressed uniformly
(`skinny/REDRESS.md:2910`, `skinny/REDRESS.md:2916`). The empirical
finding is that materializing or streaming the SIMD structural cursor costs
more than the current scalar delimiter path on the M5 Max
(`skinny/REDRESS.md:2928`).

SK-V10 also sharpened the SIMD rule. W7 proved that scalar and checkasm
parity are insufficient when the caller microbench regresses
(`skinny/REDRESS.md:3152`, `skinny/REDRESS.md:3159`). W8 proved a hex
escape primitive at the caller microbench level (`skinny/REDRESS.md:3174`,
`skinny/REDRESS.md:3185`), while W9 rejected production because the exact
caller was already wired and the row floors failed (`skinny/REDRESS.md:3200`,
`skinny/REDRESS.md:3212`). SK-V11 therefore needs micro-prove-first plus a
real same-wave consumer, not proof-only assembly inventory.

## 3. Contract Decisions

SK-V11 advances three axes together:

1. Direct plane closure or measured fixpoint. The eleven residual
   `direct_to_struct` rows above must either clear the strict same-run
   sonic-rs 1.10x digest gate on both Track 1 and independent Track 2, or
   receive per-row uncloseable REDRESS evidence.
2. Grammar generalization by execution. At least one non-JSON grammar,
   preferred order CSS L4 declaration values then Sheets then BBNF-self, must
   carry an admitted and benchmarked SK-V11 intervention. A Lock 14 prose
   statement is not enough.
3. Aarch64-only SIMD/ASM utilization. SK-V11 may target Apple Silicon
   aarch64 only. NEON TBL/TBX, CSSC CTZ, PMULL, UDOT, SHA3 EOR3, and
   related ARMv9.2 surface are eligible only after a same-host isolated
   micro-benchmark proves gain against representative slices and names the
   same-wave production consumer.

The axes are coupled. A direct-plane kernel is not admissible unless S-P2
can express it as a grammar-neutral primitive and S-P3 can name the
non-JSON proof wave or shared primitive consumer. A grammar-generalization
wave cannot count unless it benchmarks a real generated direct or typed
parser. A SIMD wave cannot count unless it moves a direct row, preserves
typed/direct guards, or admits the non-JSON grammar intervention by
measurement.

## 4. Pre-Blocked Routes For SK-V11

- W3 union/event substrate, class-column substrate, streaming cursor,
  class-lane-only route, `UnionTape`, sidecar producer, retained structural
  cursor, W4-through-W3 cascade-lock, or any rename of that family
  (`skinny/REDRESS.md:2934`).
- Parse-only SOTA admission while rows remain `S / NO-GO`
  (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:41`).
- Direct digest evidence relabeled as typed product proof.
- Apache/CITM, W2, W6, or W10 row admission by analogy; each row needs its
  own same-run strict comparator evidence and Track 2 proof.
- W7 full-string primitive route without a new S-P1 hot-leaf antecedent and a
  new caller-level micro-proof (`skinny/REDRESS.md:3159`).
- W9 "already wired" production claim for an existing caller
  (`skinny/REDRESS.md:3200`).
- JSON-only policy in generic crates, codegen, runtime-outside-JSON,
  `bbnf-simd`, or `parse-that-regex`.
- x86 work, including asmjson AVX-512 as an implementation target. It may be
  cited only as comparator context when S-P2 records strictness and output
  plane.

## 5. S-P1 Readiness

`HANDOFF.md` is ready for S-P1 dispatch after G-Alpha is presented or pinned
by the orchestrator. S-P1 must open with a fresh SK-V11 profile of the
SK-V10-close baseline, not inherit SK-V10 hot-leaf conclusions. The profile
must cover:

- all 17 JSON corpora for `parse_only`, `direct_to_struct`, and
  `real_typed_struct`;
- samply and xctrace CPU Counters plus Time Profiler PMU on this host;
- the 11 direct residual rows as first-class rows, not averages;
- the 7 typed and 6 direct `A / GO` rows as maintain guards;
- at least one non-JSON grammar candidate path, with its benchmark harness
  gap called out if it is not yet runnable;
- microbench candidates only as measured primitives, never as profile-derived
  dispatch authority.

## 6. CHALLENGE Notes

Alpha CHALLENGE should reject this draft if it finds:

- any SK-V11 source dispatch authority before S-P3;
- any SPEC or DISPATCH-PROMPT created by Alpha-F;
- any direct close target missing Track 1, Track 2, sonic-rs direct, and a
  numeric floor;
- any non-JSON axis reduced to a prose Lock 14 proof;
- any aarch64 kernel candidate allowed into S-P3 without micro-proof-first;
- any x86 implementation target;
- any W3 substrate family reopened;
- any parse-only row counted as SOTA admission;
- any telemetry field emitted without a same-wave gate consumer.
