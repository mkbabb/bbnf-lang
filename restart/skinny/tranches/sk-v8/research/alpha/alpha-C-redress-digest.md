# SK-V7 -> SK-V8 Alpha-C Redress Digest

Authored: 2026-05-17.
Source window: SK-V7 commits and artifacts dated 2026-05-16.

Scope: digest of the SK-V7 REDRESS ledger, SK-V7 wave research/plan/redress
artifacts, and the git chain from W0 through W10c. This file is read-only
analysis for SK-V8 planning. It does not amend `skinny/REDRESS.md`.

Current authority after SK-V7:

- `skinny/RESULTS.md` remains overall `N-direct / NoGo`.
- Schema-v3 telemetry is present, but `Delta vs SK-V6` is explicitly `n/a`
  because W0b did not have a machine-readable SK-V6 baseline binding.
- Strict sonic-rs is the governing comparator; lossy sonic-rs is retained only
  as same-run flaw-probe provenance.
- Parse rows remain `K / NO-GO` in the final checked-in table. Product-plane
  `real_typed_struct` rows pass for `twitter`, `update_center`, `mesh`, and
  `marine_ik`.
- `direct_to_struct` digest remains the main N-direct blocker. Passing digest
  rows in the final table include `citm_catalog`, `apache_builds`, `mesh`,
  `marine_ik`, `numbers`, and `unicode_basic`; remaining digest rows are still
  below the sonic-rs slack gate.

## Commit Chain Read

| Wave | Research | Plan | Redress / close |
|---|---|---|---|
| W0 | `a5cf0969` | `df8beb58` | `ed923615` |
| W0b | `9ddae991` | `7a3e4126` | `0d2fab3f` |
| W1 | `980469b1` | `a2403144` | `89f29768` |
| W2 | `c3d6e102` | `3d8bb04a` | `78d83497` |
| W3 | `0f3e0579` | `3c2f9854` | `41ecf187` |
| W4 | `3f1828e6` | `25fc2b79` | `17bd39b1` |
| W5 | `893ce6fb` | `21e6c66f` | `db761873` |
| W6 | `bfa2f9e3` | `e045e008` | `58479e29` |
| W7 | `4be402b6` | `36bb9df5` | `f786e597` |
| W8 | `7c5e8ad6` | `ddab18a8` | `7c6837b8` |
| W9 | `ae063b43` | `457bc7a8` | `51d8c8be` |
| W10 | `814118fd` | `ae6ebd79` | `db913136` |
| W10b | `508dfd16` | `7f3200df` | `0cd00886` |
| W10c | `b99cf338` | `0fc24d1a` | `56e66ef5` |

## SK-V7 Routes

| Item | Wave | SHA | Status | Route | Evidence and routing |
|---:|---|---|---|---|---|
| 77 | W0 | `ed923615` | Partial | sonic-rs strict comparator repair | Admitted removal of `utf8_lossy`; rejected row-flip forecast. `cargo bench -p bbnf-bench --bench json_parity` completed and gate refreshed results, but sonic deltas ranged -14.6% to +18.5%; `instruments` stayed 91.6% Track 1/S and `unicode_basic` stayed 76.2%. Keep strict repair; do not claim throughput. |
| 78 | W0b | `0d2fab3f` | Admitted | schema-v3 telemetry row builder | `cargo test -p bbnf-bench`, full `json_parity`, gate, and `xtask gate-json` reached schema-v3 authority. Same-run sonic strict/lossy provenance is explicit. No parser/runtime performance claim. |
| 79 | W1 | `89f29768` | Admitted | descriptor-preserving TapeKind rename | Old JSON-shaped `TapeKind`/`DirectBuildDecode` symbols and `materialization_for_rule` were absent by grep. `passes`, `check-json`, `check-real-typed`, and workspace tests passed. `RESULTS.md` and generated JSON files had no diff. |
| 80 | W2 | `78d83497` | Rejected | zero-fallback mantissa-widen redress | Fresh canada attribution found 111080 f64 candidates, zero mantissa overflow, zero ambiguous EL, zero `str::parse::<f64>()` fallback. Direct rows stayed canada 10773/10296/12421 Mbps `N-direct`, numbers 12615/12362/12838 Mbps `A`. No source patch was causal. |
| 81 | W3 | `41ecf187` | Admitted | capacity-hinted numeric Vec real-typed expansion | `DirectTypeRef::Vec` gained `capacity_hint`, helper identity includes the hint, Vec helpers use `Vec::with_capacity`, and `U32` direct scalar lowering exists. Mesh and marine_ik real typed rows passed: mesh 9466/8089/8696 Mbps, marine_ik 12020/9630/8750 Mbps. |
| 82 | W4 | `17bd39b1` | Rejected | single-quartet Unicode escape classifier | Correctness and checkasm passed, but `unicode_escapes` parse reached only 14516/14535/17671 Mbps, 82.1% of sonic vs 95% target; `y_string_unicode` parse stayed 49.9%; direct rows missed and Track 2 regressed 6.6% on `y_string_unicode`. |
| 83 | W5 | `db761873` | Rejected | generated-retained StringBlock16 tiny probe | Correctness/checkasm passed, but zero of six rows crossed threshold and every named Track 1 row regressed more than 3%: twitter 49.0% of sonic, update_center 38.1%, unicode_basic 45.6%, random 38.9%, unicode_mixed 43.0%, distinct_values 34.7%. |
| 84 | W6 | `58479e29` | Rejected | object-pair value-byte control compaction | Correctness passed, but `citm_catalog` parse Track 2 reached only 84.9% of sonic, `instruments` parse Track 1 92.9%, and `instruments` direct 94.4%; `citm_catalog` Track 1 also violated no-regression. |
| 85 | W7 | `f786e597` | Admitted | Lock 14 Phase A+B neutralization | parse-that public JSON matcher APIs and passes JSON binding helpers were removed; renamed-rule coverage proved grammar-derived DirectBuild roles. Tests, `check-json`, `check-real-typed`, `check-conformance`, workspace tests, and greps passed. No `RESULTS.md` diff. |
| 86 | W8 | `7c6837b8` | Admitted | Lock 14 Phase C+D codegen shell neutralization | Generic codegen APIs/modules and direct-schema key naming were neutralized; JSON allowlists were removed from generic lower paths; IR JSON alphabet and whitespace-special case were removed. Generated JSON outputs and `RESULTS.md` stayed unchanged. |
| 87 | W9 | `51d8c8be` | Admitted | CostFacts substrate projection | Added `ir::cost`, `LayoutFacts.cost_facts`, CostFacts-selected lowerers, diagnostics, and `xtask gate-json --with-cost-facts`. jq verified schema `sk-v7-costfacts-v1`, 15 entries, rejected alternatives, REDRESS 72 backfill, and diagnostics. Generated output and `RESULTS.md` unchanged. |
| 88 | W10 | `db913136` | Rejected | consumed aarch64 bitmap bodies plus B6 canary fold | Correctness, checkasm, canary reach, and explicit `pmull`/`ctz` asm proof passed. JSON parse falsified PMULL default prefix-XOR: instruments Track 1 -4.62%, Track 2 -4.19%; numbers Track 1 -10.04%; unicode_escapes Track 1 -12.66%, Track 2 -15.52%. |
| 89 | W10b | `0cd00886` | Rejected | CTZ bulk consumer plus B6 canary fold | Correctness, primitive-checkasm, workspace tests, negative canary, `ctz` asm proof, and simd_scan smoke passed. Refreshed RESULTS comparison failed maintain invariant: canada Track 1 -3.11%, Track 2 -4.14%; citm Track 1 -7.36%; instruments -3.96%; marine_ik -5.68%; mesh Track 1 -8.07%, Track 2 -7.46%; numbers -6.44%. |
| 90 | W10c | `56e66ef5` | Admitted | B6 stack-canary Stage 1 only | Shared randomized XOR-fold canary plus byte-exact backstop replaced fixed 0xDE volatile probes in checkasm harnesses. Release checkasm, primitive-checkasm, workspace tests, static audits, and negative canary controls passed. Production bitmap/runtime/generated paths and `RESULTS.md` have zero diff. PMULL and CTZ body fills remain rejected. |

## Admitted Route Boundaries

- Comparator honesty is admitted, not comparator-based closure. Item 77 keeps
  strict sonic-rs because reverting would restore a known flaw, but no W0 parse
  row reclassified.
- Schema-v3 reporting is admitted as gate hygiene. Item 78 is required before
  SK-V8 row comparisons, but it is not a throughput intervention.
- Lock 14 cleanup is admitted across W1, W7, and W8. These were neutralization
  waves; they must not be cited as parse or direct performance work.
- Product-plane typed output is admitted for host/API schemas. Items 71 and 81
  prove generated typed `DirectBuild` from explicit host/API schema facts, not
  synthetic digest-stressor closure.
- CostFacts is admitted as the evidence substrate. Item 87 records choices and
  rejected alternatives; it does not authorize retrying a rejected hot-path
  route without fresh evidence.
- B6 Stage 1 hardening is admitted as test-harness hardening only. Item 90 has
  no production or `RESULTS.md` diff.

## SK-V8 Pre-Block List

### Prior clusters required by SK-V7 handoff

| Cluster | Blocked as-is | Binding evidence | Reopen only if... |
|---|---|---|---|
| REDRESS 28+33 | Class A `match_tiny_plain_string` NEON/TBL wiring as a JSON parse-G fix | Earlier active 16-byte tiny helper wiring regressed twitter about 25%; later audit found the kernel targeted the wrong boundary. W5 item 83 also rejected a StringBlock16 wrapper for the generated-retained tiny leaf. | The frame is not "wire the old TBL tiny matcher." A future route must name a different current hot boundary on the current Track 1 baseline, provide scalar/checkasm parity, wire a same-wave consumer, and show same-row parse improvements without Track 2/direct guard regressions. |
| REDRESS 50 | Retained parse-time aux/projection side tables | Dense and sparse aux columns improved retained traversal probes but regressed governing Track 1 parse by double-digit percentages on twitter/citm/canada. | Reframe as typed event consumption over the single existing tape/event substrate, not parser-owned side tables. Evidence must show parse-plane non-regression plus row closure. |
| REDRESS 51+53 | EventCursor, byte-class whitespace cursor, or parser-local structural-mask cursor | Whitespace cursor and stricter structural-mask cursor both turned into extra parse-time scanning and decisively regressed retained parse. | Reframe as the parser's single substrate: scanner writes/feeds the tape/event stream in the same loop, or a CollapsedStage/SinkOnly lowering consumes live masks directly. Evidence must include invalid-byte cross-checks and full retained row measurements. |
| REDRESS 54+55 | Sink-local decoded stats or quote-source streaming hash | Exact decoded stats and one-pass quote-source streaming hash both regressed escaped-string direct rows versus allocate-then-contiguous-hash baseline. | Reframe around a different consumer representation, such as typed field layout that avoids the digest hash shape entirely. Evidence must beat the current allocate-then-contiguous-hash baseline on escaped-string direct rows. |
| REDRESS 60 | Retained trusted-string boundary collapse by deleting the tiny probe | Regressed every measured retained row; the tiny probe is not redundant. | Only reopen with a split short/long-string plan that preserves the short-string early-out and profiles the second boundary separately. |
| REDRESS 61+62 | Always-wide or delayed-wide retained trusted full-string scan | Full matrix or smoke gates failed; wide scanners improved isolated rows but regressed sentinel rows. | Only reopen with a non-sidecar string primitive whose own symbol boundary is measured and whose row thresholds and no-regression guards are met in full gate context. |
| REDRESS 64 | Retained Unicode-escape run validator | Dense `unicode_escapes` improved, but companion rows did not; the route failed its same-row gate. W4 item 82 repeated the per-quartet family and still missed. | Only reopen if fresh profiles show a broader local fact than contiguous or single-quartet `\uXXXX` decode, and both parse and direct rows meet named thresholds. |
| REDRESS 65 | Object next-key carry | Object carry failed its gate; W6 item 84 later rejected the narrower value-byte control compaction. | Only reopen with new PC-level evidence naming a different control owner. Do not retry key/next-byte carry under a new helper name. |
| REDRESS 66+67+68+69 | Direct string/materialization family: source-hook folding, parser-owned decoded scratch, byte-output unescape, DirectBuild semantic string facts | Each failed on escaped-string/direct rows; semantic string fact hashing repeated the rejected sink-local cost class. | Only reopen if the output contract changes from synthetic digest stressor to a real typed field representation, or a grammar-neutral decoded-string primitive beats `unescape_string` standalone and through a generated same-wave consumer. |
| REDRESS 70+71 | Hand-authored typed sink as proof of DirectBuild vs host/API schema DirectBuild | Item 70 rejected hand-authored typed sinks as proof; item 71 admitted generated typed output only when host/API schema facts are explicit. | SK-V8 may extend host/API schema typed output. It must not use benchmark-private hand parsers or hidden BBNF directives as proof. |
| REDRESS 72 | Global cap-16 tiny-string policy | Generated-retained cap 16 was admitted, but direct SinkOnly and hand Track 2 cap-16 regressed; CostFacts item 87 records the split. | Only generated-retained OffsetTape may use the cap-16 fact by default. Any new plane needs CostFacts evidence for that plane, including rejected alternatives and guard-row measurements. |

### SK-V7-specific new blocks

| Item | Blocked as-is for SK-V8 | Required changed framing and evidence |
|---:|---|---|
| 80 | Table-only or zero-fallback Eisel-Lemire mantissa widening for canada | Reopen only if current-head attribution shows a material f64 fallback pool on the target row. Required evidence: counters for mantissa overflow/ambiguous EL/fallback, bit-parity tests, direct row Criterion data, `numbers` no-regression, and no raw `parse::<f64>()` shortcut. |
| 82 | Single-quartet Unicode escape classifier as parse/direct close | Reopen only as a materially different string/Unicode consumer, not another per-quartet helper. Required evidence: profile showing decode is the named hot leaf, parse and direct threshold tables for `unicode_escapes` and `y_string_unicode`, and Track 2 guard evidence. |
| 83 | `string_block` wrapper for generated-retained cap-16 tiny probe | Reopen only as a lower-overhead inline or asm first-special extractor proven cheaper than the scalar leaf before wiring. Required evidence: PC-level attribution on current generated Track 1, scalar/checkasm parity, six-row W5 threshold table, and no Track 1/Track 2 guard regressions. |
| 84 | Object-pair value-byte return / array-next mirroring as W6 close | Reopen only with a different same-row hot owner. Required evidence: fresh profiles for `citm_catalog` and `instruments`, focused parse/direct measurements, and no repetition of object next-key carry, separator elision, dispatch-table, whitespace, or EventCursor sidecar routes. |
| 88 | PMULL as default hot `bitmap_prefix_xor_64` body | Reopen only for a narrow consumer that is not the default JSON parse hot path, or after proving PMULL non-regression on current rows. Required evidence: scalar reference, checkasm, explicit `pmull` asm, simd_scan, full `bench-json`/`gate-json`, and no Track 1/Track 2 Mbps drop above the written maintain budget. |
| 89 | CTZ next-bit body consumed by AArch64 `bulk_emit_positions_64` in production scan | Reopen only if CTZ is isolated behind a proven beneficial consumer or the bulk-emitter interaction changes. Required evidence: explicit `ctz` asm under feature flags, no PMULL reintroduction, simd_scan stability, and full RESULTS comparison with no >2% maintained-row drops. |
| 90 | Treating B6 canary hardening as primitive/body admission | Not reopenable as performance evidence. Item 90 only admits checkasm hardening. Any SK-V8 primitive body still needs its own scalar reference, checkasm, same-wave production consumer, asm proof, and row-level performance gate. |

## Additional standing blocks

These are still blocked by the pre-SK-V7 ledger and were repeatedly cited in
SK-V7 plans:

- 12-byte token width churn or skipless token perturbation unless a fresh
  before/after row shows a clean throughput win.
- Pair-token fusion unless it beats current parse throughput; prior reduction
  in token count regressed or failed to improve key rows.
- Function-pointer dispatch table; a real implementation regressed important
  rows and the old probe was invalid.
- Capacity prescan; do not scan input to discover capacity. Schema-provided
  capacity hints are admissible only when carried as host/API facts.
- Generic SWAR whitespace skipper and separator elision; both are blocked
  without new profile evidence and same-row gates.
- Raw f64 shortcut; exact float parity forbids replacing the EL path with an
  unchecked `parse::<f64>()` shortcut.
- PSI/DTA Rust-codegen automata and EventCursor parallel prepasses; both are
  side-substrate patterns unless reframed through CostFacts and a same-wave
  single-substrate consumer.
- Orphan primitive bodies, including blocked `bbnf.asm` bodies such as
  `BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and
  `FSM_DISPATCH_THREADED`, remain inadmissible without same-wave consumers.

## SK-V8 Routing Implications

1. SK-V8 should treat twitter parse and the yyjson gap as a fusion-quality
   driver problem, not another local tiny-string, per-quartet, or sidecar
   retry.
2. Direct digest failures should not be attacked by sink-local decoded hashing
   or parser-owned scratch. Either change the output/product framing to real
   host/API typed schemas, or produce a standalone decoded-string primitive
   that beats the current materializer before generated integration.
3. CostFacts must be the gate for any route-fact change. If a candidate differs
   by workload plane, CostFacts must record the chosen plane and the rejected
   alternatives instead of globalizing the policy.
4. Primitive body fills must be separated from harness hardening. B6 canary
   work can continue as process hardening, but PMULL/CTZ body admission remains
   rejected until a production consumer passes full row-level maintain gates.
5. Lock 14 cleanup is no longer the dominant blocker after W7/W8, but SK-V8
   should preserve the grep gates so generic crates do not reacquire JSON
   policy while pursuing fusion-quality work.
