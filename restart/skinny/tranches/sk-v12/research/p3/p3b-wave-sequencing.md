# SK-V12 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-20.
Scope: sequence the converged SK-V12 S-P2 survivor pool into a topological W0..Wn wave manifest.
Output: this file.
Pass Alpha goalset: generated non-JSON baseline first; one measured grammar-generalized intervention second; preserve the 4 direct and 7 typed JSON guard rows; keep `parse_only` diagnostic; carry JSON direct residual and W3 pre-blocks.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis

P3-B sequencing is constrained by three accepted facts.

First, W0 is mandatory and first. PASS-3 defines P3-B as the wave-ordering
agent and states that W0 is the baseline-profile / telemetry-lock wave before
behavior waves (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:59`,
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:245`). SK-V12 S-P1 has
already converged on the current surface: overall `N-direct / NoGo`,
`parse_only` diagnostic, 4 direct `A / GO`, 13 direct residual `N-direct /
NO-GO`, and 7 typed `A / GO` rows
(`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`).

Second, SK-V12 cannot start with another JSON direct retry. The goalset says
the generated non-JSON baseline comes first, the grammar-generalized measured
intervention comes second, direct/typed JSON product rows are guards, parse
only is diagnostic, and JSON residual rows are pre-blocked by REDRESS 119
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:50`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:62`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:65`). The handoff repeats the
same priority order and names the preferred grammar order: CSS L4 declaration
values, then Sheets, then BBNF-self
(`restart/skinny/tranches/sk-v12/HANDOFF.md:60`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:66`).

Third, there is no substrate prerequisite wave. S-P2 converged with no current
selectable tape-substrate candidate: P2-D marks same-tape capacity, sparse
flag, and cursor-skip ideas diagnostic/ineligible, and rejects
`structural_class_lane_union` under REDRESS 96/97/98
(`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:30`,
`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:69`,
`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:79`).
Therefore W1 can go directly to the generated non-JSON baseline owner surface.

The S-P2 survivor pool is support for W1/W2, not independent wave authority.
P2-A C1-C7 are only selectable under scalar-reference, parity, and same-wave
consumer floors (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:31`).
P2-B makes same-wave consumption mandatory for every primitive
(`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:33`).
P2-C lists six current AArch64 candidates and demotes LD4 and SHA3 ternary
boolean fold to inventory (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:38`).
P2-E contributes five parse-that primitive gaps, but explicitly says they do
not authorize JSON-only direct wave selection before the generated non-JSON
priority (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:46`,
`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:376`).
P2-F maps the candidate surface to six conditional parser/support families;
the digest and tape/accounting families are not parser wave candidates
(`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:25`,
`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`,
`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:40`).

## §2 — Deliverable

### Wave Manifest

| Wave | Title | Entry gate | Owner-path family | Dispatch status | Hard cap | LOC / risk | Same-wave consumer requirement | Dependency |
|---|---|---|---|---|---:|---:|---|---|
| W0 | SK-V12-open telemetry and guard-floor lock | S-P3 converged; clean SK-V12 packet; `skinny/RESULTS.md` and gate/report lanes readable | `skinny/RESULTS.md`; `skinny/crates/bbnf-bench/src/report.rs`; `skinny/crates/bbnf-bench/src/bin/gate.rs`; SK-V12 research/w0 artifacts | First, unconditional | <=90 min wall, redress <=75 min | <=160 LOC / low-medium | `gate-json`/report consumer must consume every required SK-V12 field; no producer-only telemetry | none |
| W1 | Generated non-JSON baseline | W0 PASS; selected grammar pre-gate passes for CSS L4 declaration values, else Sheets, else BBNF-self; generated emission seam or per-grammar runtime path named; independent oracle and fixtures named | `skinny/crates/codegen/src/lib.rs`; `skinny/crates/codegen/src/json_provider.rs` only to remove the JSON-only gate, or a grammar-neutral profile provider fed solely by grammar source/workspace metadata; `skinny/crates/runtime/src/grammars/{css_l4,sheets,bbnf_self}/`; selected grammar fixture/bench/report/gate owner paths; `skinny/RESULTS.md` or companion gate report | Dispatchable only after W0 | <=90 min wall, redress <=75 min | <=520 LOC / high | Generated Track 1 direct or typed parser must be consumed by the same-wave benchmark/report gate with independent Track 2 or oracle and strict output equality | W0 |
| W2 | Selected-baseline measured grammar-generalized intervention | W1 admits a baseline row with Track 1 >= 1 Mbps, oracle/Track 2 >= 1 Mbps, sample count >= 30, and recorded `baseline_mbps`; P3-A/P3-C select one S-P2 survivor matching that row; scalar reference and parity status named | Selected slice from `parse-that-regex`, `bbnf-simd`, `codegen`, generated runtime, fixture/bench/report/gate, and result/report output; exact files narrowed by W2 plan | Conditional on W1 admit | <=90 min wall, redress <=75 min | <=430 LOC / high | Same generated row from W1 must consume the intervention in the same commit; threshold is at least `ceil(W1_baseline_mbps * 1.01)` on the same output plane | W1 |
| W3 | Conditional JSON direct companion or residual fixpoint | W1+W2 priority admitted, or W1/W2 records a measured BLOCKED route; fresh material evidence beyond REDRESS 114-119 named for any JSON direct row reopen | JSON direct owner slice only if the entry gate names a legal row; otherwise `skinny/RESULTS.md`, `skinny/REDRESS.md`, and close/research accounting | Conditional; reject before non-JSON priority resolves | <=90 min wall, redress <=75 min | <=300 LOC / high | A reopened JSON row needs generated Track 1, independent Track 2, strict same-run sonic direct comparator, and gate/report consumption in the same wave; no docs-only admission | W1/W2 disposition |
| W4 | Close and Alpha feedback | W0-W3 have admitted, rejected, or blocked with measurement; guard rows reconciled; close docs agree | `restart/skinny/tranches/sk-v12/{SYNTHESIS,HANDOFF,SPEC,DISPATCH-PROMPT}.md`; `skinny/RESULTS.md`; `skinny/REDRESS.md`; close research artifact | Final | <=90 min wall, redress <=75 min | <=220 LOC / medium | Close report/gate must consume W1/W2 result or measured BLOCKED route and guard-row disposition | W0-W3 |

### Topological Order

W0 locks the measured opening surface and guard-floor schema. W1 owns the first
material behavior: exactly one generated non-JSON baseline. W2 is the first
allowed primitive/intervention wave and must consume W1's selected baseline
row. W3 is deliberately late and conditional: it is either a JSON direct
companion after the generated priority succeeds or a residual-fixpoint record
after the generated priority blocks. W4 closes the bracket and routes Alpha
feedback. The bracket is five waves, below the <=12 skinny ceiling
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:130`).

The W1 fallback order is plan-time only, not three redress attempts hidden
inside one wave. The W1 plan may evaluate CSS L4 declaration values first,
then Sheets, then BBNF-self, and must cite a concrete executable pre-gate
failure before selecting the next target. Redress attempts exactly one selected
target. If that selected target fails, W1 records a measured generated-baseline
BLOCKED or REJECTED route and does not fall through to the next grammar inside
the same redress. This preserves the goalset order without spending the bracket
on JSON work
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:80`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:184`).

### Candidate-to-Wave Binding

| Candidate family | First eligible wave | Sequencing reason |
|---|---|---|
| Generated baseline emission/runtime/report (`E1`/`E2`/`E3`) | W1 | SK-V12 starts at the REDRESS 112 codegen/runtime blocker; no generated non-JSON baseline exists yet (`skinny/REDRESS.md:3313`, `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:302`). |
| Byte-set/classifier/layout/FIRST dispatch (`P2-A C1/C5/C7`, `P2-B BYTE_CLASS*`, `P2-C C1/C6/C7`, `P2-E pt_byte_set_run_skip`, `P2-F F1/F5/F6`) | W2 | These require generated byte-set/layout/dispatch consumers and cannot stand alone (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:38`, `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:33`). |
| Bounded string span and escape/hex segments (`P2-A C2/C3`, `P2-C C4/C5`, `P2-E pt_bounded/pt_hex/pt_escaped`, `P2-F F2/F3`) | W2 | Legal only with a generated string/literal/escape consumer; JSON-only residual reuse remains pre-blocked (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:147`, `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:278`, `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:354`). |
| Digit-run/UDOT number span (`P2-A C4`, `P2-B DIGIT_SPAN`, `P2-C C3`, `P2-E pt_digit`, `P2-F F4`) | W2 | Eligible only through the selected non-JSON number/literal consumer; REDRESS 114 blocks JSON numeric-slot reuse (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:216`). |
| Output-plane/digest oracle (`P2-A C6`, `P2-B OUTPUT_DIGEST`, `P2-F F7`) | W1 or W2 only as oracle/report support | Not a parser primitive; REDRESS 118 blocks typed/direct proof by digest alone (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:39`). |
| Tape/direct accounting (`P2-D diagnostics`, `P2-F F8`) | none unless fresh profile appears in later bracket | Current S-P1 gives no selectable tape candidate and no retained-view hot leaf (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:81`). |

## §3 — Falsifiability binding

W0 binds the opening JSON guard surface and telemetry schema. It must keep the
current result family counts coherent: 16 parse-only `S / NO-GO`, 1 parse-only
`L / NO-GO`, 4 direct `A / GO`, 13 direct `N-direct / NO-GO`, and 7 typed
`A / GO` (`skinny/RESULTS.md:143`,
`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`).

W1's measurable rows are one of:

- `css_l4/declaration_values/{direct_to_struct|real_typed_struct}/main`;
- `sheets/formula/{direct_to_struct|real_typed_struct}/main`;
- `bbnf_self/grammar/{direct_to_struct|real_typed_struct}/main`.

W1 must record generated Track 1 Mbps, independent Track 2 or oracle Mbps,
strict output equality, sample count >= 30, run/build/host context, and gate
consumption. No numeric SOTA floor exists before W1; the W1 floor is Track 1 >=
1 Mbps, oracle/Track 2 >= 1 Mbps, sample count >= 30, strict equality, and
independent oracle acceptance, because the row does not exist before this wave
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:73`).

W2 consumes W1's same row. Its minimum throughput threshold is
`ceil(W1_baseline_mbps * 1.01)` on the same output plane, with the same
independent oracle and strict equality (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:50`).

Every behavior wave that touches runtime, generated parser, SIMD, parse-that,
bench, gate, or report code must maintain the existing JSON guard rows unless
it records a measured demotion. Direct guard floors are:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guard floors are:

| Row | Track 1 maintain | Track 2/oracle maintain |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

These floors are copied from the SK-V12 goalset guard section
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:121`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:134`).

W3 may reopen a JSON direct row only after W1/W2 resolves the non-JSON
priority and only with fresh material evidence beyond REDRESS 114-119. If it
does, both Track 1 and Track 2 must clear the named direct floor:

| Row | Direct floor |
|---|---:|
| `twitter/direct_to_struct` | 13740 |
| `canada/direct_to_struct` | 10637 |
| `github_events/direct_to_struct` | 13403 |
| `update_center/direct_to_struct` | 10059 |
| `mesh/direct_to_struct` | 8675 |
| `random/direct_to_struct` | 7878 |
| `gsoc-2018/direct_to_struct` | 3737 |
| `instruments/direct_to_struct` | 8969 |
| `numbers/direct_to_struct` | 2425 |
| `unicode_mixed/direct_to_struct` | 2588 |
| `unicode_escapes/direct_to_struct` | 3441 |
| `distinct_values/direct_to_struct` | 2658 |
| `y_string_unicode/direct_to_struct` | 3950 |

Those residual rows are presently fixpoint/pre-block rows, not W3 dispatch
targets (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:65`,
`skinny/REDRESS.md:3506`). W3 may also close as "no legal JSON companion"
with no source movement if no row clears the entry evidence bar.

## §4 — Pre-blocked routes

All waves inherit these global pre-blocks:

- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate,
  including `UnionTape`, retained structural vectors, parser-owned structural
  projections, and W4-through-W3 cascade-lock
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:70`,
  `skinny/REDRESS.md:2910`).
- Parse-only SOTA close or parse-only row admission
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:62`).
- JSON direct residual row movement before the generated non-JSON baseline and
  intervention priority resolves, or without fresh material evidence beyond
  REDRESS 114-119 (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:65`).
- Generic-crate JSON policy, new directives, new BIR variants, public
  substrate API, parser-owned sidecars, second retained substrates, or x86
  implementation targets (`restart/skinny/tranches/sk-v12/HANDOFF.md:117`).

Per-wave pre-block map:

| Wave | Pre-blocked routes |
|---|---|
| W0 | No row movement by telemetry/accounting alone; no W0-clamped direct admission; no producer-only telemetry. REDRESS 119 and 120 keep the unchanged `N-direct / NoGo` surface authoritative (`skinny/REDRESS.md:3497`, `skinny/REDRESS.md:3531`). |
| W1 | REDRESS 111 report lane cannot be treated as generated baseline; REDRESS 112 rejected generated CSS L4 baseline under JSON-only runtime emission; REDRESS 113 blocks an intervention without a baseline; REDRESS 70/71 bind typed-equivalent baselines to generated DirectBuild/schema-source facts with independent oracle equality and no hand-authored typed sink (`skinny/REDRESS.md:3284`, `skinny/REDRESS.md:3313`, `skinny/REDRESS.md:3342`). |
| W2 | No primitive without scalar reference, parity where applicable, and same-wave generated consumer. Do not reopen REDRESS 28/33 active TBL/NEON tiny-string dispatch, REDRESS 54/55/60-69/72/82/83 string-materialization routes, REDRESS 80/114 numeric routes, REDRESS 88/89/90 bitmap routes, REDRESS 106-108 proof-only SIMD/string routes, REDRESS 116/117 blocked string/escape plans, or REDRESS 118 digest host-sink (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:376`, `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:57`). |
| W3 | REDRESS 114 numeric, 115 container-tail, 116 bounded string, 117 escaped segment, 118 digest host-sink, and 119 direct fixpoint all remain closed unless the entry packet names fresh material evidence beyond those records (`skinny/REDRESS.md:3359`, `skinny/REDRESS.md:3385`, `skinny/REDRESS.md:3413`, `skinny/REDRESS.md:3436`, `skinny/REDRESS.md:3464`, `skinny/REDRESS.md:3497`). |
| W4 | No close on future-phase promises. W4 must reconcile W1/W2 evidence or measured BLOCKED route with `RESULTS.md`, `REDRESS.md`, `SYNTHESIS.md`, `HANDOFF.md`, `SPEC.md`, and `DISPATCH-PROMPT.md`; otherwise close is a paper-close. |

## §5 — Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
