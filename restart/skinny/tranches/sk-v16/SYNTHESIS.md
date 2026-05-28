# SK-V16 Grand Synthesis

Date: 2026-05-28.

Status: Pass Alpha V1 contract draft for SK-V16. SK-V15 closes as
`ADMIT-W11` with routed blocks. PASS-IMPL V2 accepts the close packet but
states that CSS L4 has no admission and that the grammar-driven inflection point
has not been reached.

## Authority

- `restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md`
- `restart/skinny/tranches/sk-v15/research/w11/skv15-W11-close-dependency-checklist.json`
- `restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md`
- `restart/skinny/tranches/sk-v15/HANDOFF.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`
- `restart/prompts/ORCHESTRATOR.md`

The active user pin controls gate conflicts: only G-Omega is mandatory during
this execution; G-Alpha auto-passes.

## Section 0 - Close Condition And Goalset

### 0.1 Close condition

SK-V16 closes only when the following are all true:

| Gate | Close condition |
|---|---|
| JSON guard | 51 / 51 JSON rows remain admitted, strict, and same-plane; touched rows are re-run |
| CSS provider | CSS L4 provider is grammar-derived from `grammar/css/l4/*.bbnf`; `CSS_GENERATED_RS` is not live proof |
| CSS typed API | CSS exposes typed document/value/view/visitor surfaces |
| CSS equality | Track 1 typed CSS summary equals cssparser same-workload typed summary before speed counts |
| CSS SOTA | Track 1 beats cssparser on the same typed workload on Apple M5 Max / aarch64 |
| Dirty generated state | pre-existing dirty generated CSS and real-typed files are retired, cleanly regenerated, or intrinsically blocked with manifest-backed row-level proof |
| Pattern H | count remains 67 and provenance advances to generator-owned collapse, not header-only status |
| Lock 14 / 16 | gates report their own exclusions and reject silent self-exemption |
| Decision Engine | W7-W9 proof remains load-bearing and grammar-neutral |
| Native SIMD | any SIMD work is profile-first, scalar-referenced, checkasm/parity verified, same-wave consumed, and aarch64-only |
| FNV | FNV remains bench-only; production migration stays blocked without a new typed-semantic contract |
| PASS-IMPL V3 | close audit accepts every axis or records row-level intrinsic-block proof |

### 0.2 Starting state

| Surface | SK-V15 close | SK-V16 bracket |
|---|---:|---|
| JSON parse_only | 17 / 17 admitted | guard baseline |
| JSON direct_to_struct | 17 / 17 admitted | guard baseline |
| JSON real_typed_struct | 17 / 17 admitted | guard baseline |
| CSS L4 | 0 / 24 admitted | primary rebuild target |
| Pattern H | 67 files with line-1 provenance | collapse target |
| BackendShape | 5 / 5 lowerer proofs | preserve canon |

### 0.3 Receiver goalset

| Receiver | Obligation |
|---|---|
| CSS grammar provider | derive CSS L4 typed provider from grammar sources and remove live reliance on string-literal generated proof |
| CSS typed equality and SOTA | prove typed equality against cssparser before measuring speed, then beat cssparser on M5 Max |
| Dirty generated retirement | make broad generated checks usable again by cleaning or intrinsically blocking dirty generated state |
| Pattern H collapse | move from provenance-only to generator-owned grammar-id template collapse |
| Native aarch64 SIMD | conditional profile candidate only; S-P1 must nominate a fresh hot leaf before S-P3 may scope work |

### 0.4 Pre-blocks

SK-V16 must not reopen CSS broadcast admission, brace-counter proof,
fact-stream proof, string-literal generated proof, FNV production migration,
dirty generated files as close proof, or x86 implementation scope.

The inherited REDRESS pre-block families are:
`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum production migration`.

Hidden-coupling escapes are also pre-blocked unless routed through Pass Omega
and G-Omega: retained sidecars, sidecar event vectors, parallel source passes,
second tapes, public `UnionTape`, new substrate APIs, sixth `BackendShape`,
production FNV arbiters, and production hash-correctness proof.

The REDRESS family meanings are carried forward, not just their ids: no
tiny-string/StringBlock replay, retained parse shortcuts, retained class
columns, Track 1 == Track 2 sidecars, global direct/Track 2 cap changes,
numeric/digit route without fresh P1 BBNF hot-leaf evidence, one-quartet
Unicode/object-pair promotion, PMULL/CSSC production promotion from ISA or
checkasm alone, or decoded-string/structural-stream/string64/fixed-shape
Unicode retry under old framing.

## Section 1 - Validated And Invalidated Ledger

JSON, Lock gates, Decision Engine, all five BackendShape lowerers, Pattern H
provenance, and FNV quarantine are validated as SK-V15 close evidence. CSS SOTA,
CSS typed equality, full Pattern H collapse, and grammar-driven generalization
are invalidated or still open.

## Section 2 - Telemetry Binding

SK-V16 inherits SK-V15 telemetry and adds:

| Column | Type | Required |
|---|---|---|
| `css_track1_typed_passes` | integer | yes for CSS |
| `css_cssparser_typed_passes` | integer | yes for CSS |
| `css_typed_summary_equal` | boolean | yes for CSS admission |
| `css_provider_source` | string | yes for CSS |
| `dirty_generated_state` | enum | yes for generated checks |
| `native_simd_status` | enum | yes for SIMD claims |

CSS admission also requires a gate-consumed typed-equality report with corpus
manifest, pass/error counts, typed summaries, equality boolean, Track 1 Mbps,
cssparser Mbps, threshold, margin, admitted row count, live admission sources,
and retired legacy proof count.

S-P3 must bind executable consumers for `(cd skinny && cargo xtask gate-json --check-results --skv16-css-typed-report <path>)`,
`(cd skinny && cargo xtask gate-json --check-results --skv16-dirty-generated-report <path>)`,
`(cd skinny && cargo xtask gate-json --check-results --skv16-pattern-h-roundtrip-report <path>)`,
and `(cd skinny && cargo xtask gate-json --check-results --skv16-native-simd-report <path>)`
when native SIMD is in scope. Dirty generated state proof must include an exact
dirty-file manifest, `git status --short`, a broad command result, and
owner/disposition per file.

## Section 3 - Trajectory

SK-V16 is the grammar-derived CSS and Pattern H collapse tranche unless S-P0
finds a new contrivance that must be pruned first. If SK-V16 closes with CSS
typed equality and >SOTA, SK-V17 can become the broader grammar-driven
generalization tranche. Any contrivance that affects SK-V16 close criteria
blocks or redresses inside SK-V16; only unrelated broader generalization may
route to SK-V17.
