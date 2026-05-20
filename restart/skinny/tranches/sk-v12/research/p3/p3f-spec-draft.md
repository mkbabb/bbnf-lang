# SK-V12 P3-F: SPEC + Dispatch Draft

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-20.
Scope: draft the SK-V12 wave SPEC and per-wave dispatch prompt from the
converged S-P1/S-P2 evidence.
Output: this file + SPEC.md + DISPATCH-PROMPT.md.
Pass Alpha goalset: admit one generated non-JSON direct or typed baseline,
then admit one measured grammar-generalized intervention on that same baseline
at >= `ceil(baseline_mbps * 1.01)`, while preserving 4 direct JSON guards and 7
typed JSON guards; parse_only stays diagnostic and JSON direct residual rows
stay pre-blocked unless a fresh material reopen gate passes.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 - Synthesis

SK-V12 is not a JSON-direct retry. The opening result surface remains
`N-direct / NoGo`: 17 parse rows are diagnostic, direct has 4 `A / GO` rows and
13 `N-direct / NO-GO` residuals, and typed has 7 `A / GO` rows
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:86`;
`skinny/RESULTS.md:5`; `skinny/RESULTS.md:143`). REDRESS 119 is the direct
residual fixpoint authority and REDRESS 120 routes the next bracket to the
generated non-JSON baseline first (`skinny/REDRESS.md:3497`;
`skinny/REDRESS.md:3531`; `skinny/REDRESS.md:3545`).

S-P1 gives dispatchable profile authority but no behavior permission. The
accepted SK-V12-open profile is source baseline `50bd1648`, capture root
`/tmp/skv12-p1`, replay ledger
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`, and
self-time authority `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` plus
`/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`
(`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:31`).
It names ten hot families, but explicitly says JSON-only telemetry does not
prove CSS L4, Sheets, or BBNF-self behavior
(`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:47`;
`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:60`).

S-P2 narrows the implementation pool. P2-A's C1-C7 comparator shapes are
conditional candidates only when scalar references, parity, same-wave
consumers, and grammar-neutral gates exist. P2-B supplies the scalar-oracle
first and checkasm process. P2-C contributes six current AArch64 candidates,
with LD4 and SHA3 ternary boolean demoted to ISA inventory. P2-D contributes
zero selectable tape-substrate candidates and keeps structural class lanes
rejected. P2-E contributes five parse-that vocabulary gaps. P2-F maps the
pool to six conditional parser/support families plus oracle/accounting support
(`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:18`).

The draft packet therefore sequences five waves:

| Wave | Purpose | Reason |
|---|---|---|
| W0 | SK-V12-open baseline and telemetry lock | P3 requires W0 before behavior work. |
| W1 | Selected generated non-JSON baseline | Pass Alpha close condition 3 is first material target. |
| W2 | Selected-baseline measured intervention | Pass Alpha close condition 4 requires >= 1% measured lift. |
| W3 | Conditional JSON direct companion/reopen adjudication | JSON residuals stay pre-blocked unless W1/W2 are resolved and fresh material evidence exists. |
| W4 | Close and Alpha feedback | Reconcile admitted/rejected/routed waves and present G-Alpha. |

W1 selects exactly one baseline target in the Alpha order: CSS L4 declaration
values first, Sheets second, BBNF-self third. The W1 plan must state why any
earlier target in that order is not implementable inside the owner surface
before it may choose the next target. W2 consumes the row W1 admitted. W3 is
conditional and has no current S-P2 row-moving authority; if its entry gate
does not pass, it closes as a measured/routed block rather than a JSON-only
implementation wave.

## §2 - Deliverable

This P3-F cycle writes:

- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`

The SPEC mirrors the SK-V8 shape: close condition, comparator classes, outcome
enum, telemetry, opening goalset, non-negotiables, wave manifest, generality
gate, one section per wave, pre-blocked routes, and G-Alpha/dispatch scope.

The draft wave manifest is:

| Wave | SPEC section | Title | Status | LOC cap | Redress cap |
|---|---|---|---|---:|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock | Dispatchable after S-P3 convergence | <=180 report/gate/test/doc LOC, 0 behavior LOC | <=90 min |
| W1 | Section 4 | Generated Non-JSON Baseline | Conditional on W0 | <=520 CSS, <=480 Sheets, <=460 BBNF-self | <=75 min |
| W2 | Section 5 | Selected-Baseline Measured Intervention | Conditional on W1 admit | <=430 | <=75 min |
| W3 | Section 6 | Conditional JSON Direct Companion | Conditional on W1/W2 disposition plus material reopen gate | <=300 | <=75 min |
| W4 | Section 7 | Close And Alpha Feedback | Conditional on W0-W3 disposition | <=120 docs/gate/report LOC, 0 behavior LOC | <=90 min |

## §3 - Falsifiability binding

W0 gate `G-W0-SK-V12-OPEN` binds the existing SK-V12-open profile and telemetry:
the 41 current JSON main rows must keep their opening outcomes, run/build/host
metadata, profile artifact pointers, comparator strictness, and guard/advisory
status. Behavior source, generated output, SIMD bodies, parser runtime, and
benchmark bodies must not change.

W1 gate `G-W1-GENERATED-NONJSON-BASELINE` admits exactly one selected generated
non-JSON baseline row. The row id is one of
`css_l4/declaration_values/direct/main`, `sheets/formula/direct/main`, or
`bbnf_self/grammar/direct/main`, or the corresponding typed row if the W1 plan
proves typed is the smaller generated product. Track 1 and oracle/Track 2 must
be finite and positive, strict output equality must pass, generated source and
runtime provenance must be gate-consumed, and JSON guard floors must hold.

W2 gate `G-W2-SELECTED-NONJSON-INTERVENTION` admits one measured intervention
on the W1 row. Track 1 must be >= `ceil(W1_baseline_track1_mbps * 1.01)`;
oracle/Track 2 must remain finite, independent, and equal; all JSON guard
floors must hold; every primitive used by the intervention must have scalar
reference, parity/checkasm where applicable, and a same-wave generated
consumer.

W3 gate `G-W3-CONDITIONAL-JSON-COMPANION` is closed by default. To dispatch
behavior, the W3 plan must name a residual direct row from REDRESS 119, a fresh
material differential beyond REDRESS 114-119, scalar/oracle proof, same-host
microbench, independent Track 2, strict same-run sonic direct floor, and
same-wave gate consumption. Current S-P2 contributes no selectable W3
substrate candidate; absent a passing entry gate, W3 records a routed block.

W4 gate `G-W4-CLOSE` requires every prior wave to admit, reject, or route with
REDRESS evidence; `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SYNTHESIS.md`,
`HANDOFF.md`, SPEC, and dispatch prompt must agree.

JSON guard floors carried into W1/W2/W3 are seeded from SK-V12 Alpha:

| Guard row | Track 1 floor | Track 2/oracle floor |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

## §4 - Pre-blocked routes

The SPEC carries these binding blocks:

- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate,
  including `UnionTape`, retained structural vectors, parser-owned
  projections, and W4-through-W3 cascade-lock routes; REDRESS 96/97/98 retire
  the family.
- parse_only SOTA admission or parse-only row movement.
- JSON direct residual work before W1/W2 generated non-JSON priority closes or
  blocks, and any residual reopen without fresh material evidence beyond
  REDRESS 114-119.
- REDRESS 111 report-lane evidence as a generated baseline.
- REDRESS 112/113 CSS/non-JSON baseline blocker as a future-phase promise.
- Number slot, container-tail, bounded string span, escaped segment, and output
  digest host-sink replays from REDRESS 114-118.
- Sidecars, second retained substrates, new directives, new BIR variants,
  new `BackendShape` variants, public substrate APIs, parser-owned scratch,
  generic-crate JSON policy, x86 implementation work, stale sidecars, and
  strict admission from permissive/flaw-probe comparators.

## §5 - Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
