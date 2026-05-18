# SK-V8 -> SK-V9 Alpha-C Redress Digest

Authored: 2026-05-18.

Scope: PASS-ALPHA alpha-C digest for the SK-V8 close. This artifact reads the
SK-V8 REDRESS ledger and close dispositions, classifies admitted, rejected, and
partial routes, and identifies routes to pre-block or reframe before SK-V9
planning. It does not amend `skinny/REDRESS.md`, dispatch SK-V9 waves, or
authorize implementation work.

Primary sources:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-reconciliation-research.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-plan.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
- `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V1/HARDENING-W6-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`
- Prior pre-block authority from `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md` and `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`.

## Close Authority

SK-V8 closed by W6 V1+V2 hardening convergence. The W6 close admits no source,
generated-output, benchmark-row, `skinny/RESULTS.md`, or `skinny/REDRESS.md`
change. `skinny/RESULTS.md` remains the W0-rendered authority with 38
`SK-V8-open` manifest rows, four measured `real_typed_struct A / GO` rows, and
overall `N-direct / NoGo` (`restart/skinny/tranches/sk-v8/HANDOFF.md:250-260`;
`skinny/RESULTS.md:3-40`).

The SK-V8 behavior-wave REDRESS ledger is:

- REDRESS 91: W2 typed product-plane source/product parity admitted; benchmark
  row-table admission and `canada/real_typed_struct` rejected/routed
  (`skinny/REDRESS.md:2620-2659`).
- REDRESS 92: W3 Tier A tape plus structural projection rejected/routed before
  source redress (`skinny/REDRESS.md:2661-2690`).
- REDRESS 93: W4 Track 2 scalar-parent fold rejected/routed after selected-row
  falsification (`skinny/REDRESS.md:2692-2729`).

W5 needs no REDRESS entry because it admitted only a named Lock 14
provider-boundary cleanup with no generated-output, performance, row-table, or
`RESULTS.md` claim (`restart/skinny/tranches/sk-v8/HANDOFF.md:235-248`).

## SK-V8 Disposition Classification

| Route | Status | Authority | Classification | Carry-forward boundary |
|---|---|---|---|---|
| W0 telemetry/report gate | Closed | W0 V11+V12 hardening and SK-V8 HANDOFF | Admitted | Telemetry and report authority only. It does not admit parser, scanner, SIMD, asm, codegen, product-plane behavior, or throughput movement. |
| W1 CostFacts and strict comparator gate binding | Closed | commit `c6345e4d`; SK-V8 HANDOFF | Admitted | Gate/evidence substrate only. CostFacts and comparator ids may bind later gates, but are not performance proof and do not reopen rejected routes by bookkeeping. |
| W2 typed product-plane source slice | Closed | commit `12aff1e4`; REDRESS 91; W2 V4+V5 hardening | Partial | Apache and CITM source/product parity are admitted through existing real typed schema/generator paths. `skinny/RESULTS.md` remains unchanged; Apache/CITM are not measured SK-V8 rows. |
| W2 `canada/real_typed_struct` and row-table admission | Closed | REDRESS 91 | Rejected/routed | Canada failed full-fixture DirectBuild-vs-serde checksum parity on long decimal coordinates. Benchmark row-table admission is rejected for SK-V8 because W0 run-id metadata drift already invalidated local Criterion refresh. |
| W3 Tier A tape plus structural projection | Closed | REDRESS 92; W3 V1 hardening | Rejected/routed | The scanner structural index and retained tape event stream are not isomorphic. No source patch, rejected patch artifact, row-table admission, or `RESULTS.md` change exists. |
| W4 hand Track 2 scalar-parent fold | Closed | REDRESS 93; W4 V3+V4 hardening | Rejected/routed | Correctness passed, Apache cleared the selected floor, but random missed and numbers regressed by +6.3287% Track 2 time. Patch is archived at `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`. |
| W5 provider-boundary cleanup | Closed | commit `6e159f5c`; W5 V4+V5 hardening; SK-V8 HANDOFF | Admitted | Named Lock 14 cleanup only: moved provider material from generic `codegen/src/lib.rs` to `codegen/src/json_provider.rs` and added the `per_grammar_provider` allowlist class. No performance or row-table claim. |
| W6 close reconciliation | Closed | W6 V1+V2 hardening; SK-V8 HANDOFF | Admitted | Close-only reconciliation. It routes residuals to SK-V9 Alpha or Pass Omega and explicitly does not dispatch SK-V9 implementation. |

## REDRESS 91 Detail

REDRESS 91 is partial, not a broad typed-row admission.

Admitted:

- Commit `12aff1e4` adds `apache_builds/real_typed_struct` and
  `citm_catalog/real_typed_struct` as source/product parity rows.
- The source stays inside existing typed schema and generated DirectBuild paths:
  `skinny/xtask/src/real_typed_schema.rs`,
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, and generated typed
  output.
- The proof uses generated Track 1 DirectBuild, serde_json as Track 2/oracle,
  sonic-rs checksum parity, and full fixture parity tests.

Rejected/routed:

- `canada/real_typed_struct` is rejected for W2 after checksum mismatch on long
  decimal coordinate payloads. It must not be admitted by weakening proof to
  length-only, digest-only, or partial fixture evidence.
- Apache/CITM benchmark row-table admission is rejected for SK-V8. They are not
  measured `skinny/RESULTS.md` rows and must not be counted as the six-row typed
  close state.
- The W2 report-gate fold is narrow: source-only typed fixtures no longer make
  the W0 checked report require unadmitted Criterion metadata rows. The W0
  run-id validator remains intact.

SK-V9 framing that may admit:

- A dedicated typed benchmark row-table tranche may admit Apache/CITM only if it
  owns run-id/metadata validation, produces fresh measured rows, keeps the four
  existing typed GO rows as guards, and preserves independent Track 2/oracle
  proof.
- Canada remains blocked until the long-decimal checksum mismatch is explained
  and fixed under full-fixture parity.

## REDRESS 92 Detail

REDRESS 92 is rejected/routed before source redress.

Rejected/routed:

- The stage-1 scanner retains structural punctuation plus real quotes, while the
  current retained tape is a generated parser event stream with container
  opens/closes, opening quotes, number starts, and literal starts.
- Retained view and `ValueRef` traversal depend on the current event stream.
- The accepted plan exceeded the W3 fit gate once it included SIMD, JSON scan,
  tape layout, generated retained parser, retained view/value, codegen
  templates, bench parity/materialization/gate code, and row reporting.
- No source patch or rejected patch artifact exists because the route failed the
  pre-redress fit gate.

Pre-blocked as-is:

- Sidecar producers, parser-owned structural cursors or facts, aux density
  tables, EventCursor variants, `tape_vs_tape` as production consumer,
  `UnionTape`, a new `BackendShape`, a new BIR variant, a new directive, or a
  public substrate API.
- Tier B string-boundary, quote-backslash, and parity work under the Tier A
  name.

SK-V9 or Pass Omega framing that may admit:

- A later route must first define the retained class/event grammar, including
  numbers, literals, container events, and string quote ownership.
- It must prove the retained `ValueRef` cursor contract over that grammar.
- It must replace scalar structural rediscovery inside one retained tape rather
  than adding a parallel sidecar.
- It must provide a same-wave generated retained parser consumer, full row
  measurement on structural-heavy parse rows, strict comparator metadata,
  no-regression guards, Lock 14 proof, and challenge acceptance.
- SC-6-L1-R1 remains a Pass Omega residual unless SK-V9 proves Lock 1 as written
  and routes the Omega residual explicitly.

## REDRESS 93 Detail

REDRESS 93 is rejected/routed after implementation falsification.

Rejected/routed:

- The candidate changed only `skinny/crates/bbnf-bench/src/direct_struct.rs`,
  folding hand Track 2 scalar object/array values directly into the parent
  digest instead of constructing temporary scalar child digests.
- Correctness passed, and directional probes improved Apache and numbers.
- Binding Criterion falsified the selected-row gate: Apache cleared
  `sonic-rs / 1.10`, random still missed, and numbers regressed by +6.3287%
  Track 2 time.
- No Lock 14 W4 parent-diff allowance or W4-aware checked report path was added
  because source admission was already falsified. `skinny/RESULTS.md` remains
  unchanged.

Pre-blocked as-is:

- Scalar parent folding under a new name.
- Digest-only local arithmetic as product proof.
- Track 2 candidate admission without full-table maintain measurement, a
  W4/SK-V9-aware checked report path, and an independent digest-arithmetic
  backstop.

SK-V9 framing that may admit:

- A direct output-contract tranche may admit if it changes the output/product
  contract instead of continuing digest guard-plane optimization.
- A direct control-path tranche may admit if fresh profiles identify a different
  same-row owner and the plan keeps direct digest rows as guards, not product
  proof.

## W5 No-REDRESS Provider-Boundary Cleanup

W5 is admitted, but only as a named Lock 14 cleanup.

Admitted:

- Commit `6e159f5c` removed a live provider-boundary residue from generic
  codegen by moving JSON profile guard and provider material from
  `skinny/crates/codegen/src/lib.rs` to
  `skinny/crates/codegen/src/json_provider.rs`.
- The cleanup added the `per_grammar_provider` allowlist class and authorized
  only the named W5 owner-path parent diffs.
- W5 hardening V4+V5 accepted with no source drift after V4, no generated output
  drift, no row-table refresh, no performance claim, and clean Lock 14 scans.

Pre-blocked as-is:

- Treating provider-boundary cleanup as permission to reintroduce JSON policy in
  generic crates.
- Treating a Lock 14 cleanup as parse, direct, typed, or benchmark movement.
- Generalizing the W5 allowlist beyond the named provider boundary without a new
  Lock 14 proof and non-JSON proof.

SK-V9 framing that may admit:

- Further boundary cleanup may admit only as named grammar-neutral audit work
  with zero behavior/performance claim unless a separate challenged behavior
  wave owns generated output, row evidence, and revert scope.

## W6 Close And Alpha Feedback

W6 is admitted only as close reconciliation.

Admitted:

- W6 V1 and V2 each accepted 6/6 with minimum confidence 96%.
- V2 found no drift from V1 across the W6 packet, `skinny/RESULTS.md`,
  `skinny/REDRESS.md`, and SK-V8 HANDOFF.
- The close keeps W2 Apache/CITM source/product rows out of measured
  `RESULTS.md`, keeps W3/W4 rejected/routed, keeps W5 non-performance, and
  routes SC-6-L1-R1 to Pass Omega rather than ratifying it.

Pre-blocked as-is:

- Any SK-V9 implementation dispatch from SK-V8 close.
- Any W6 source, generated-output, benchmark, `RESULTS.md`, or `REDRESS.md`
  movement without a mismatch-specific plan.
- Any silent ratification of SC-6-L1-R1 or broad Lock 1/Lock 14 amendments.

## Prior Pre-Blocks That Still Matter

These blocks remain active for SK-V9 unless a later plan supplies fresh
baseline evidence, exact owner paths, same-wave production consumer,
no-regression gate, REDRESS citation, and challenge acceptance.

| Cluster | Block as-is in SK-V9 | May reopen only under this changed framing |
|---|---|---|
| REDRESS 16/17/18/25 | Pair-token fusion, function-pointer dispatch, 12-byte or skipless token churn, structural-index typed parser prepass, separator elision, generic SWAR whitespace. | Fresh SK-V9 baseline proves the route's owner is hot, same-row strict gates beat baseline, and guard rows hold. No sidecar or second scanner. |
| REDRESS 28+33 and 72/83 | Old Class A tiny-string NEON/TBL wiring, global cap-16 policy, generated-retained `StringBlock16` wrapper. | Different current hot boundary, scalar/checkasm parity, same-wave generated consumer, plane-specific CostFacts, and no Track 1/Track 2 guard regressions. |
| REDRESS 50-55 | Parse-time aux/projection side tables, EventCursor/byte-class/structural-mask cursors, sink-local decoded stats, quote-source streaming hash. | Single-substrate scanner/tape/event consumption or a materially different product representation; no parser-owned side tables or digest-only retries. |
| REDRESS 60-72 | Retained string boundary collapse, wide/delayed-wide scanners, Unicode validator, object next-key carry, direct source-hook folding, parser-owned decoded scratch, byte-output unescape, semantic string facts, hand-authored typed sinks, global cap-16. | Reopen only with fresh profile ownership and a different output/materialization contract. Host/API typed schemas may extend product-plane work; benchmark-private hand parsers and hidden directives remain invalid proof. |
| REDRESS 73 | Generated retained array next-byte helper-shape transfer to hand Track 2. | Future Track 2, retained-control, or direct-control work must profile the hand parser's code layout directly. Do not assume generated helper shape transfers monotonically to the hand comparator. |
| REDRESS 80 | Zero-fallback Eisel-Lemire mantissa widening or raw `parse::<f64>()` shortcuts. | Current-head numeric attribution shows a material fallback/overflow/ambiguous pool, exact parity holds, and numeric direct guard rows hold. |
| REDRESS 82 | Single-quartet Unicode escape classifier. | Broader Unicode/string fact with hot-leaf proof, parse and direct threshold tables, and Track 2 guard evidence. |
| REDRESS 84 and REDRESS 65 | Object key/value-byte carry and object-pair control compaction. | Fresh PC-level evidence names a different same-row control owner; do not retry key/next-byte carry under a helper rename. |
| REDRESS 88/89/90 | PMULL prefix-XOR as default body, CTZ/bulk production consumer, B6 canary hardening as performance proof. | Narrow measured primitive consumer with scalar reference, checkasm, asm proof, same-wave production consumer, full row maintain, and retained falsifier rows. B6 remains harness hardening only. |
| REDRESS 36-38 and 85-86 plus W5 | Generic JSON policy leakage, JSON-hardcoded SIMD/codegen residues, renamed generic helpers that encode JSON semantics. | Named Lock 14 cleanup with non-JSON proof and zero behavior claim unless split into a separate challenged behavior wave. |
| SC-6-L1-R1 / substrate ceiling | Lock 1 amendment, `UnionTape`, new `BackendShape`, new BIR variant, new directive, public substrate API, sidecar cardinality. | Pass Omega ratifies the amendment, or SK-V9 proves Lock 1 as written while preserving one substrate, opaque generated ordinals, `ValueRef` contract, and same-wave production consumption. |
| Strictness and telemetry | Lossy/permissive/sidecar comparator rows, telemetry-only rows, CostFacts-only evidence, `tape_vs_tape` as production consumer. | Same-run strict comparator on the same output plane, gate-consumed telemetry, CostFacts chosen/rejected alternatives, and actual production consumer evidence. |

## Routes That May Admit Under Different Framing

1. Typed row-table expansion can be a SK-V9 candidate, but only as a measured
   row-table tranche that owns run-id/metadata validation. Apache/CITM source
   parity is a seed, not an SK-V8 measured close.
2. Retained structural parse work can be a SK-V9 or Pass Omega candidate only
   after retained class/event grammar and `ValueRef` contract proof. The route
   must replace scalar rediscovery inside one tape and cannot ship as sidecar
   projection.
3. Direct work can continue only through a direct output-contract or control
   path tranche. Digest rows remain guard-plane evidence unless the product
   contract changes.
4. Lock 14 provider-boundary cleanup can continue as named audit work. It cannot
   claim throughput and cannot weaken generic-crate grammar neutrality.
5. Bitmap or primitive work remains reserve research. It may admit only with a
   narrowed consumer, not by default body replacement or correctness/checkasm
   evidence alone.

## Pass Omega Routes

Route these outside SK-V9 implementation unless a later SK-V9 plan explicitly
proves the relevant lock as written:

- SC-6-L1-R1 Lock 1 amendment/generalisation.
- Broad lock amendments.
- Canonical path cleanup.
- Top-level CRUD and surface refresh.
- Any governance change that would authorize new directive, BIR, substrate,
  `BackendShape`, or public substrate API surfaces.

## SK-V9 Alpha Guidance

- Do not dispatch SK-V9 implementation waves from this digest.
- Preserve SK-V8 close status: W2 is partial, W3/W4 are rejected/routed, W5 is
  no-REDRESS cleanup, and W6 is close-only.
- Treat the prior pre-block ledger as binding and additive. A renamed rejected
  route remains rejected unless the new plan states exactly why it is different
  and supplies fresh evidence before redress.
- Any SK-V9 candidate should name target rows, guard rows, strict comparator
  plane, output plane, owner paths, same-wave consumer, revert protocol, and
  REDRESS fallback before implementation.
