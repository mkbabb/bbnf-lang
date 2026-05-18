# SK-V8 P3-E: Pre-Blocked Route Ledger

Pass: S-P3 Synthesis-Plan. Cycle: V4 exact traceability fold.
Date: 2026-05-18.
Scope: enumerate every SK-V8 pre-blocked or disallowed route from Alpha, S-P2, RESULTS, and REDRESS, with the evidence required before any wave may reopen it.
Output: this file.
Pass Alpha goalset: SK-V8 may open only through a measured `SK-V8-open` baseline, CostFacts-bound gates, behavior waves with named row thresholds or REDRESS rejection, preserved typed GO rows, direct digest guard rows, and Lock 14/15 closure (`SPEC Section 0.1 - Global Close Condition` and `SPEC Section 10 - Pre-Blocked Routes`, `HANDOFF Section 6 - Exit Condition`).
Candidate pool: research/p2-substrate-ceiling/ post-CHALLENGE survivors, with S-P2 V7 converged only for S-P3 dispatch and not for any implementation wave (`S-P2 V7 consolidated verdict and preserved boundaries`).
Traceability note: inline citations use exact SPEC/HANDOFF section labels or current file:line anchors. RESULTS row claims resolve to `skinny/RESULTS.md:3-42`; Track 2 independence resolves to `skinny/RESULTS.md:217-218`; named REDRESS ids resolve to `skinny/REDRESS.md`, with cited live spans `skinny/REDRESS.md:1214-1219`, `skinny/REDRESS.md:1301-1312`, and `skinny/REDRESS.md:1331-2605`.


## §1 - Synthesis

S-P2 V6 and V7 form the required consecutive ACCEPT pair, so S-P3 is authorized. That governance result is narrow: it does not authorize W3, G-Alpha close, or any implementation dispatch (`S-P2 V7 consolidated verdict and preserved boundaries`, `SYNTHESIS opening state and S-P2/W3 finding sections`). S-P3 must produce the wave plan, exact owner paths, measurable gates, revert protocol, and same-wave consumers before any redress work can start (`PASS-3 Synthesis-Plan role and gate sections`, `SKINNY-TRIUMVIRATE role separation and redress contract`).

This ledger is a blocking surface for P3-B/P3-C/P3-F and for later CH3 review. The S-P3 prompt requires P3-E to walk REDRESS, identify the per-wave routes that must not be reopened, and distinguish routes that may admit only under a changed framing with fresh P1/W0 evidence (`PASS-3 Synthesis-Plan role and gate sections`). The V8 SPEC minimum list is binding but not exhaustive: REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, plus historical function-pointer, pair-token, token-width, separator, generic SWAR, capacity prescan, EventCursor/sidecar, raw f64, and orphan primitive routes are blocked unless the full reopen package is present (`SPEC Section 10 - Pre-Blocked Routes`, `HANDOFF Section 7 - Pre-Blocked Routes`).

No-deferral rule: a route is either still blocked or is reopened by same-wave evidence. A wave may not close by promising W0 profiles, comparator repair, CostFacts, scalar/checkasm, a production consumer, REDRESS accounting, or non-regression measurement in a later phase. Missing evidence means the route is not in scope; a failed implementation reverts and records REDRESS (`ORCHESTRATOR Sections 3W/3Z`, `SKINNY-TRIUMVIRATE role separation and redress contract`).

The implementation-wave cap for any later redress is hard-capped at 90 minutes. The triumvirate default is 60 minutes implementation plus 15 minutes measurement; SK-V8 plans may use less, but no route in this ledger may be opened by a wave that needs more than 90 minutes of implementation/measurement to reach its falsifiability gate (`SKINNY-TRIUMVIRATE role separation and redress contract`).

## §2 - Deliverable

### Wave Ownership Map

| Owner wave/gate | Evidence owned | Routes it can unblock |
|---|---|---|
| W0 baseline/profile/telemetry gate | `SK-V8-open` row table, same-run strict comparator metadata, hot leaves, profile artifacts, sidecar freshness, run ids, sidecar rejection rules | Evidence only. W0 may identify candidate owners; it must not reopen behavior routes or claim throughput wins (`SYNTHESIS opening state and S-P2/W3 finding sections`). |
| W1 CostFacts gate | CostFacts bound into `gate-json`, rejected alternatives, chosen/rejected plane accounting, zero behavior drift | Route-fact changes only after W0. W1 cannot use CostFacts as a performance result or reopen REDRESS 50-72/28+33/83/84 by evidence bookkeeping alone (named REDRESS id(s) in `skinny/REDRESS.md`). |
| W2 typed product-plane wave | Host/API schema facts, generated `real_typed_struct` consumer, strict typed comparator rows, direct digest guards | May extend generated typed output when schemas are real. It cannot use hand-authored typed sinks, benchmark-private parsers, or hidden directives as proof (named REDRESS id(s) in `skinny/REDRESS.md`). |
| W3 retained parse/substrate behavior wave | Fresh hot-leaf owner path, same-wave retained parse consumer, strict-vs-strict row gate, no sidecar/cursor/aux table, scalar/checkasm for new primitive | May reopen a retained parse family only if the changed framing avoids the blocked route and meets same-row thresholds. `tape_vs_tape` is telemetry, not the W3 production consumer (`SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`, `S-P2 V7 consolidated verdict and preserved boundaries`). |
| W4 direct/materialization behavior wave | Generated SinkOnly or real typed consumer, direct rows versus same-run strict sonic/serde, digest rows as guards, no parser-owned scratch/sidecar | May reopen direct work only when it changes the product/materialization plane or proves a primitive through a generated same-wave consumer. Digest-only local hashing retries stay blocked (RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`). |
| W5 Lock 14/generality audit | Grammar-neutral proof for generic crates, non-JSON CSS L4/Sheets/BBNF-self proof where relevant, grep gates for JSON leakage | May close boundary residue only. It cannot reland JSON policy under renamed generic helpers or claim performance (named REDRESS id(s) in `skinny/REDRESS.md`, `SYNTHESIS opening state and S-P2/W3 finding sections`). |
| Reserve primitive/bitmap gate, not selected in W0-W6 by P3-F | Scalar reference, checkasm, asm proof, same-wave production consumer, density predicate if bitmap, full RESULTS maintain gate | May test a narrowed primitive consumer only after W0/W1 and a future accepted plan. PMULL/CTZ default body fills remain blocked (named REDRESS id(s) in `skinny/REDRESS.md`, `Alpha-E candidate shortlist and pre-blocks`). |

### Global Disallowed Surfaces

These are blocked in every wave. They have no default reopening path.

| Route | Why blocked | Evidence that could unblock | Owner wave/gate |
|---|---|---|---|
| New BBNF directive, new BIR variant, new `BackendShape`, `UnionTape`, public substrate API, or parallel substrate | ORCHESTRATOR and S-P2 V7 preserve the substrate union and forbid new directive/BIR/substrate surfaces (`ORCHESTRATOR Sections 3W/3Z`, `S-P2 V7 consolidated verdict and preserved boundaries`). | None inside S-P3. This would require a separate authorized governance change outside this ledger. | Blocked globally. |
| Generic-crate JSON policy leakage, including renamed generic helpers that encode JSON roles | Lock 14 requires grammar-neutral generic code, generated byte sets, opaque ordinals/fact ids, and non-JSON proof (`S-P2 V7 consolidated verdict and preserved boundaries`, `ORCHESTRATOR Sections 3W/3Z`). | Generic-crate edit passes grep gates and proves CSS L4, Sheets, and BBNF-self do not depend on JSON roles (`SYNTHESIS opening state and S-P2/W3 finding sections`). | W5 Lock 14/generality gate; per-wave for any generic edit. |
| Sidecar producer, parser-owned structural projection, retained cursor, aux density table, or sidecar event vector | S-P3 CH5 explicitly forbids hidden coupling through parallel substrate and parser-owned projection/cursor/sidecars (`PASS-3 Synthesis-Plan role and gate sections`). REDRESS 50/51/53 measured parser-local versions as regressions (named REDRESS id(s) in `skinny/REDRESS.md`). | Only a single-substrate production path where scanner writes/feeds the tape/event stream or a same-loop consumer uses live masks, with invalid-byte cross-checks and full row measurement. | W3 retained parse/substrate gate. |
| Lossy/permissive/sidecar comparator as strict admission evidence | Strict-vs-strict gate is mandatory; sidecar C++ and permissive rows are planning signals or flaw probes unless same-run strict/output-plane rules are met (`ORCHESTRATOR Sections 3W/3Z`, named REDRESS id(s) in `skinny/REDRESS.md`, RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`). | Same-run strict comparator on the same output plane, with corpus/build/hardware/run provenance and in-row validation. | W0 telemetry/schema gate, then each behavior gate. |
| `tape_vs_tape`, `parse_only`, or telemetry-only rows as W3 production consumer | S-P2 V7 preserves `tape_vs_tape` as residual telemetry, not a production same-wave consumer (`S-P2 V7 consolidated verdict and preserved boundaries`). V8 SPEC keeps `tape_vs_tape` outside default W0/W1/W3 closure (`SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`). | A later plan must name an actual production retained parser, direct, or typed consumer. Telemetry alone never unblocks. | W3/W4 behavior gate, after W0/W1. |
| Orphan primitives, checkasm-only body fills, or harness hardening as performance admission | No primitive ships without scalar reference, checkasm, and same-wave consumer (`SKINNY-TRIUMVIRATE role separation and redress contract`). REDRESS 90 admits only B6 canary hardening, not bitmap body fills (named REDRESS id(s) in `skinny/REDRESS.md`). | Scalar reference, checkasm parity, asm proof where relevant, production consumer in the same wave, and row-level gate. | W6 primitive gate or candidate-specific behavior wave. |
| Track 1 == Track 2 dishonesty, benchmark-private parser, hidden hand sink, or shared source masquerading as independent comparator | REDRESS 34/35/48 corrected bench-private direct parser dishonesty and moved direct source under generated/BIR authority (named REDRESS id(s) in `skinny/REDRESS.md`). RESULTS records Track 2 as independent and not calling Track 1 (RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`). | Independent Track 2 proof plus source audit and same-output parity. No throughput proof from a shared hidden parser. | W0 comparator/telemetry gate and any W2/W4 direct gate. |
| Automatic implementation dispatch from S-P2 convergence or from a single S-P3 artifact | V7 convergence authorizes S-P3 only; G-Alpha/W0 dispatch still requires S-P3 plan and challenge flow (`S-P2 V7 consolidated verdict and preserved boundaries`, `ORCHESTRATOR Sections 3W/3Z`). | Completed S-P3 packet, CHALLENGE convergence or user pin at the right gate, and explicit dispatch. | Orchestrator/G-Alpha, not an implementation wave. |

### Route Ledger

No-deferral treatment for every row: if the owning evidence is missing at wave entry, the wave must not include the route. If a route is tried and misses its gate, source is reverted, the rejected patch is preserved, and REDRESS records the failure in the same wave.

| REDRESS / source | Blocked or disallowed route | Why blocked | Evidence that would unblock | Owner wave/gate |
|---|---|---|---|---|
| 16 | Pair-token fusion / pair-token-free object projection | Token count reduction regressed or failed key parse rows; canonical JSON tape keeps explicit key/value cursor pairing (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh W0 profile showing pair cursor cost is current hot owner, before/after retained rows beating SK-V8-open, Track 2/direct guards maintained. | W3 retained parse gate. |
| 17 | 256-entry function-pointer dispatch table or dispatch-table alternate | Old probe was invalid; real implementation in Track 1 and Track 2 regressed key corpora and was reverted (named REDRESS id(s) in `skinny/REDRESS.md`). | Current profile identifies Rust `match` dispatch as hot owner and same-row strict gate beats baseline without guard regressions. | W3 or W4 behavior gate. |
| 18, 25 | 12-byte/skipless token width churn, generic width perturbation | Reduced tape bytes but produced mixed/regressed throughput; measured alternates remain rejected (named REDRESS id(s) in `skinny/REDRESS.md`). | A lazy-offset replacement that beats named parse/direct rows and proves view traversal parity without sidecar. | W3 retained substrate gate. |
| 25 | Structural-index typed parser prepass | Listed among measured rejected alternates; it duplicates/side-steps canonical source signals (named REDRESS id(s) in `skinny/REDRESS.md`). | Reframed as generated same-loop consumer over the single substrate, not a prepass, with full strict row gate. | W3/W4 behavior gate. |
| 25 | NEON no-escape string matcher as close route | Measured/audited and not retained; later string routes repeatedly missed row gates (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh hot-leaf proof that the matcher owns the current row and scalar/checkasm plus generated consumer clear guard rows. | W3 retained or W4 direct string gate. |
| 25 | Separator elision | Measured rejected alternate and repeatedly named as not compensating for later failures (named REDRESS id(s) in `skinny/REDRESS.md`). | Same-row profile showing separator work is current hot owner and a no-regression parse/direct gate. | W3 behavior gate. |
| 25 | Generic SWAR whitespace skipper | Measured rejected alternate and forbidden as compensation for W6 failure (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh row profile plus grammar-neutral primitive proof and same-wave generated consumer; must not hide EventCursor/sidecar. | W3 retained parse gate. |
| 28+33 | Class A NEON/TBL tiny-string wiring as JSON parse close | Active 16-byte helper regressed twitter about 25%; refined audit found the primitive targeted the wrong boundary (named REDRESS id(s) in `skinny/REDRESS.md`). | Changed framing with a different current hot boundary, scalar/checkasm, same-wave generated consumer, same-row parse improvement, and Track 2/direct guard stability. | W3 retained string gate. |
| 31, 80 | Raw `parse::<f64>()`, stale f64 fallback elimination, table-only mantissa widen | Raw shortcut failed parity; SK-V7 W2 measured zero canada f64 fallback pool, so no consumer remained (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh numeric-array attribution with nonzero overflow/ambiguous/fallback pool, bit parity, strict direct rows, and no raw shortcut. | W4 numeric direct gate. |
| 36-38, 85-86 | Reintroducing JSON-hardcoded SIMD, `bbnf-simd` god module, `simd-scan` fossil, or generic JSON residue | Historical Lock 14 violations were identified; SK-V7 W7/W8 admitted neutralization and no throughput claim (named REDRESS id(s) in `skinny/REDRESS.md`). | Only grammar-neutral relocation/audit with zero RESULTS diff and non-JSON proof. No performance route. | W5 Lock 14 gate. |
| 49 | No-allocation decoded-string visitor layered on `unescape_json_string` | Source hooks admitted, but visitor path regressed escaped-string direct rows and was reverted (named REDRESS id(s) in `skinny/REDRESS.md`). | Different materialization representation or primitive that beats baseline both standalone and through generated direct consumer. | W4 direct string gate. |
| 50 | Dense/sparse parse-time aux projection side tables | Traversal probes improved but governing Track 1 parse regressed severely; side tables were reverted (named REDRESS id(s) in `skinny/REDRESS.md`). | Single-substrate event/tape consumption with parse-plane non-regression and row closure; no parser-owned aux table. | W3 retained substrate gate. |
| 51 | `JsonEventCursor` byte-class whitespace cursor | Correctness-green cursor centralized whitespace but regressed retained parse far below baseline; no `StructuralIndex`, `Vec<JsonEvent>`, whitespace sidecar, or aux column is admissible (named REDRESS id(s) in `skinny/REDRESS.md`). | Live mask/tape event stream consumed as the parser substrate, with invalid-byte cross-checks and full retained rows. | W3 retained substrate gate. |
| 53 | Parser-local structural-mask cursor / second scanner | Stricter cursor still performed a second source scan and cut Track 1 roughly in half on target rows (named REDRESS id(s) in `skinny/REDRESS.md`). | Same-loop scanner/tape/event consumer or CollapsedStage/SinkOnly live mask path; no parser-owned second scanner. | W3 retained substrate gate. |
| 54 | Sink-local exact decoded-string stats/hash helper | Two-pass exact stats regressed escaped direct rows; allocation removal did not cross sonic slack (named REDRESS id(s) in `skinny/REDRESS.md`). | A new output/product representation or one-pass primitive that beats allocate-then-contiguous-hash baseline on strict direct rows. | W4 direct string gate. |
| 55 | Quote-source fused streaming hash materializer | One-pass sink-local hash still underperformed the checked-in allocate-then-hash baseline (named REDRESS id(s) in `skinny/REDRESS.md`). | Field-layout/typed materializer or grammar-neutral decoded primitive with generated same-wave consumer; must beat current direct baseline. | W4 direct/product gate. |
| 59 | Re-prescribing SK-V5 UTF-8 fusion class without fresh row gates | Entries 50-55 refute the fusion class on generated Track 1; future work must name row/profile/hot boundary before implementation (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh W0 profiles, named c/B or Mbps deltas, hot symbol boundary, and same-row falsification. | W3/W4 behavior gate. |
| 60 | Retained trusted-string boundary collapse by deleting tiny probe | Regressed every measured retained row; tiny probe is not redundant (named REDRESS id(s) in `skinny/REDRESS.md`). | Split short/long string plan preserving short early-out and measuring second boundary separately. | W3 retained string gate. |
| 61 | Always-wide retained trusted string scan | Plausible focused wins but full advisory gate failed and guard rows regressed (named REDRESS id(s) in `skinny/REDRESS.md`). | Non-sidecar primitive with own symbol boundary, full row thresholds, and guard stability. | W3 retained string gate. |
| 62 | Delayed-wide retained trusted string scan | Correctness green, but production smoke regressed multiple sentinel rows; delayed 64-byte scanner is blocked on this baseline (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh profile naming a non-wide, non-sidecar string boundary. | W3 retained string gate. |
| 63 | Treating admitted `ContainerNext` / array next-byte carry as SK-V8 close | The route was admitted but did not close parse-G; remaining work is string/Unicode or direct bridge (named REDRESS id(s) in `skinny/REDRESS.md`). | Current W0 proof that the same owner remains hot and a new same-row gate beyond the already-admitted shape. | W3 retained parse gate. |
| 64, 82 | Retained Unicode-escape run validator or single-quartet classifier as close route | Four-unit validator helped only dense `unicode_escapes`; W4 single-quartet also missed parse/direct thresholds (named REDRESS id(s) in `skinny/REDRESS.md`). | Broader local fact than contiguous/single `\uXXXX`, profile showing decode as hot leaf, parse/direct thresholds for `unicode_escapes` and `y_string_unicode`, Track 2 guards. | W3/W4 string-Unicode gate. |
| 65, 84 | Object next-key carry or object-pair value-byte return/control compaction | Object key carry failed; value-byte compaction missed citm/instruments thresholds and regressed generated layout (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh PC-level evidence for a different same-row hot owner, not key/value-byte carry under a new name. | W3 retained control gate. |
| 66 | Direct source-hook field-layout materializer / receiver shortcut | Correctness green but produced no required lift; receiver/closure removal was too small (named REDRESS id(s) in `skinny/REDRESS.md`). | Materially different escaped-string materialization or typed product plane; must not repeat source-hook folding. | W4 direct materialization gate. |
| 67 | Parser-owned decoded scratch for generated direct escaped strings | Regressed primary escaped row by 44%; allocation reuse was not the limiting factor (named REDRESS id(s) in `skinny/REDRESS.md`). | Standalone grammar-neutral decoded primitive or DirectBuild field-fact plan, preserving strict semantic equality. | W4 direct string gate. |
| 68 | Byte-output `unescape_json_string` materialization | Rejected direct materialization family; byte-output rewrite did not close escaped-string/direct gate (named REDRESS id(s) in `skinny/REDRESS.md`). | New decoded primitive or product-plane materializer with generated consumer and strict direct thresholds. | W4 direct string gate. |
| 69 | DirectBuild semantic string facts for current digest workload | Rejected for digest workload; repeated sink-local string fact cost class (named REDRESS id(s) in `skinny/REDRESS.md`). | Product-plane change to real typed output or field representation, not digest stressor hashing. | W2 typed or W4 direct product gate. |
| 70 | Hand-authored typed sink, bench-private schema, or hidden typed proof | Hand-authored typed sink is rejected as proof; schema must come from host/API facts, not hidden directives or bench-private parser (named REDRESS id(s) in `skinny/REDRESS.md`). | Host/API schema source, generated typed DirectBuild, strict typed comparator, and visible generated consumer. | W2 typed product gate. |
| 71 | Using admitted generated typed DirectBuild to reopen rejected digest/materializer routes | Generated typed from host/API schema is admitted, but it is a product-plane premise and does not excuse digest-route retries (named REDRESS id(s) in `skinny/REDRESS.md`). | New typed schemas may extend W2; digest/direct materializer routes still need their own W4 evidence. | W2 for typed, W4 for digest. |
| 72, 83 | Global cap-16 policy or generated-retained `StringBlock16` wrapper | Cap16 is admitted only for generated retained OffsetTape; direct/Track2 cap16 regressed. W5 `StringBlock16` wrapper regressed six parse rows (named REDRESS id(s) in `skinny/REDRESS.md`). | Plane-specific CostFacts plus fresh PC-level proof of a lower-overhead extractor before wiring. | W1 CostFacts plus W3 retained string gate. |
| 73 | Track 2 array next-byte parity repair by copying generated helper shape | Helped one row but regressed guard rows; generated helper shape does not transfer monotonically to hand Track 2 (named REDRESS id(s) in `skinny/REDRESS.md`). | Direct profile of hand parser layout and a Track 2-specific gate, without assuming Track 1 transfer. | W0/W3 comparator and retained gate. |
| 74, SC6 | asmjson/DAV1D as new directive, permissive anchor, `CollapsedStage` shortcut, `UnionTape`, or new substrate | asmjson is a reference, not a directive; CollapsedStage requires CostFacts, same-plane strictness, admitted primitives, and consumers (named REDRESS id(s) in `skinny/REDRESS.md`). S-P2 forbids UnionTape/BackendShape/public substrate confusion (`S-P2 V7 consolidated verdict and preserved boundaries`). | CostFacts-selected grammar-neutral stage with strict comparator, emitted tables, scalar/checkasm primitives, and same-wave consumer. | W1 CostFacts, then W3/W4 behavior gate. |
| 75, 77 | Lossy sonic/asmjson/permissive rows as S anchor, or row-flip forecast from strict repair | Sonic lossy cannot anchor strict rows; W0 strict feature repair admitted but flipped no rows (named REDRESS id(s) in `skinny/REDRESS.md`). | Same-run strict row on same output plane, feature proof, and in-row validation. | W0 comparator gate and every row gate. |
| 76 | PMULL/CSSC/DotProd/SVE/SME/AVX-512 route by architecture analogy | Architecture features remain unadmitted until exact profiles point there (named REDRESS id(s) in `skinny/REDRESS.md`). | Fresh hot-leaf profile plus scalar/checkasm/asm proof and production row gate. | W6 primitive gate or behavior wave. |
| 78 | Schema-v3 telemetry as performance improvement | Schema v3 admitted reporting/provenance only; overall gate remains `N-direct / NoGo`, and delta caveat is honest reporting limitation (named REDRESS id(s) in `skinny/REDRESS.md`). | None as performance. Telemetry can only supply W0 evidence for later waves. | W0 telemetry gate. |
| 79 | TapeKind rename as performance or route reopen | Descriptor-preserving rename had zero RESULTS/generated diff and did not reopen prior routes (named REDRESS id(s) in `skinny/REDRESS.md`). | None as performance. Only Lock 14/generality accounting. | W5 Lock 14 gate. |
| 81 | Typed Vec admission as proof for retained/direct rejected routes | Capacity-hinted Vec rows admitted generated `real_typed_struct`, but do not reopen retained materializer, hand typed, or capacity prescan routes (named REDRESS id(s) in `skinny/REDRESS.md`). | Additional host/API typed schemas may extend product plane; rejected parse/direct routes need separate evidence. | W2 typed product gate. |
| 87 | CostFacts evidence as hot path, parser change, or automatic reopen | W9 CostFacts is evidence only, generated output and RESULTS had no diff, and it does not reopen pre-blocked routes (named REDRESS id(s) in `skinny/REDRESS.md`). | Later wave consumes CostFacts in a measured behavior gate with chosen/rejected alternatives and no-regression rows. | W1 CostFacts, then behavior wave. |
| 88 | PMULL `bitmap_prefix_xor_64` as default hot production body | Correct and visible in asm, but JSON parse rows regressed severely; default PMULL is rejected (named REDRESS id(s) in `skinny/REDRESS.md`). | Narrow measured consumer, not default hot path, with scalar/checkasm, explicit `pmull`, simd_scan, full RESULTS maintain gate. | W6 primitive/bitmap gate. |
| 89 | CSSC CTZ next-bit plus bulk production consumer | Correctness/integrity gates passed, but refreshed RESULTS dropped multiple Track 1/2 rows beyond 2% (named REDRESS id(s) in `skinny/REDRESS.md`). | Isolated beneficial consumer or changed bulk interaction, explicit `ctz`, no PMULL reintroduction, simd_scan stability, full row maintain. | W6 primitive/bitmap gate. |
| 90 | B6 canary Stage 1 as primitive body or performance admission | W10c admits test-harness hardening only; no production or RESULTS diff (named REDRESS id(s) in `skinny/REDRESS.md`). | Primitive body admission still needs independent scalar/checkasm/same-wave consumer/row gate. | W6 primitive gate. |
| Alpha-E 1 | Twitter retained parser fusion through old tiny-string/EventCursor/separator/function-pointer/SWAR/sidecar/PMULL/CTZ route | Alpha shortlisted twitter fusion but explicitly pre-blocked those routes (`Alpha-E candidate shortlist and pre-blocks`). | W0 hot-leaf attribution, exact runtime/template owner paths, retained same-wave consumer, strict twitter gate, and guard rows. | W3 retained parse gate. |
| Alpha-E 2 | Telemetry completion used to relabel stale sidecars or modify parser/codegen | Alpha telemetry candidate is schema/freshness only and pre-blocks parser/codegen changes (`Alpha-E candidate shortlist and pre-blocks`). | Same-run manifest and gate validation; parser changes split to later behavior wave. | W0 telemetry gate. |
| Alpha-E 3 | Lock 14 audit as generic JSON reintroduction under renamed helpers | Alpha Lock 14 candidate pre-blocks JSON semantic passes, old public JSON APIs, and renamed generic helpers (`Alpha-E candidate shortlist and pre-blocks`). | Zero generic JSON policy, byte-identical generated output unless split, non-JSON proof. | W5 Lock 14 gate. |
| Alpha-E 4 | Bitmap asm body reland by correctness/checkasm alone or by dropping falsifier rows | Alpha permits only changed density-gated framing and blocks unconditional PMULL/CTZ reland and correctness-only evidence (`Alpha-E candidate shortlist and pre-blocks`). | Density predicate, scalar fallback proof, primitive and full JSON row gates, falsifier rows retained. | W6 primitive/bitmap gate. |

## §3 - Falsifiability Binding

Any route above can be considered reopened only when its owning wave writes all of the following into its plan before redress starts:

| Requirement | Binding |
|---|---|
| Fresh baseline | Compare against `SK-V8-open`, not stale SK-V6/SK-V7 or sidecar-only measurements. W0 owns hot leaf, profile artifact, run id, and comparator metadata (`SYNTHESIS opening state and S-P2/W3 finding sections`). |
| Strict comparator | Admission rows must use same-run strict anchors. sonic lossy/permissive and C++ sidecars are flaw probes/planning signals unless refreshed under same-run strict/output-plane rules (`SYNTHESIS opening state and S-P2/W3 finding sections`, RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`). |
| Named row thresholds | The plan names corpus rows and Mbps or c/B thresholds. Unmeasurable prose gates are rejected (`PASS-3 Synthesis-Plan role and gate sections`). |
| Full-table maintain | Target rows plus guard rows remain in the gate; dropping historical falsifier rows is not allowed, especially for bitmap routes (`Alpha-E candidate shortlist and pre-blocks`). |
| Same-wave consumer | Primitive, substrate, or materializer must be consumed by the production parser/direct/typed path in the same wave. No orphan kernel or telemetry-only close (`SKINNY-TRIUMVIRATE role separation and redress contract`). |
| Scalar/checkasm where relevant | SIMD/ASM work requires scalar reference, checkasm parity, and asm proof before production wiring (`ORCHESTRATOR Sections 3W/3Z`). |
| Revert and REDRESS | Failed gate reverts source and records REDRESS with the failed rows and rejected patch path (`SKINNY-TRIUMVIRATE role separation and redress contract`). |
| Hard cap | Implementation plus measurement must fit within 90 minutes for the redress wave. Work requiring more must be split before dispatch. |

Current row surfaces that should seed gates:

| Route family | Required guard/target rows |
|---|---|
| Retained parse/twitter fusion | `twitter parse_only` is visible residual at 15752 Track 1 vs 21020 sonic strict and 30931 yyjson, but hot leaf is unprofiled, so it is post-W0 only (RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`, `SYNTHESIS opening state and S-P2/W3 finding sections`). Guard `update_center`, `apache_builds`, `github_events`, `unicode_escapes`, `numbers`, and `citm_catalog` parse rows per Alpha-E (`Alpha-E candidate shortlist and pre-blocks`). |
| Direct string/materialization | Direct NO-GO rows include `twitter`, `canada`, `github_events`, `update_center`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, and `y_string_unicode` with sonic strict slack failures (RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`). |
| Typed product-plane expansion | Existing `real_typed_struct` GO rows (`twitter`, `update_center`, `mesh`, `marine_ik`) are maintain guards and may seed host/API schema expansion, not digest proof (RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`, named REDRESS id(s) in `skinny/REDRESS.md`). |
| Numeric/direct | `canada`, `numbers`, `mesh`, and `marine_ik` direct rows were the mantissa-widen guard surface; zero fallback means no W2 source edit without fresh attribution (named REDRESS id(s) in `skinny/REDRESS.md`). |
| Bitmap/primitive | Prior falsifier rows include `instruments`, `numbers`, `unicode_escapes`, `canada`, `citm_catalog`, `marine_ik`, and `mesh`; they must remain in any density-gated bitmap plan (named REDRESS id(s) in `skinny/REDRESS.md`, `Alpha-E candidate shortlist and pre-blocks`). |
| Telemetry/schema | All 38 current main rows must receive W0 schema/freshness treatment; no throughput-cell movement beyond telemetry-only budget may be hidden (RESULTS rows `skinny/RESULTS.md:3-42` and Track 2 independence `skinny/RESULTS.md:217-218`, `HANDOFF Section 2 - Current Measured State`). |

## §4 - Pre-Blocked Routes

The ledger in S2 is the full S-P3 pre-block list. P3-B and P3-F should fold it as follows:

| Wave | Must pre-block explicitly |
|---|---|
| W0 | All behavior routes, parser/codegen changes, stale sidecars as anchors, row-close claims from schema completion, and any `skinny/` source edit. |
| W1 | Behavior changes, CostFacts-as-performance, global route-fact policies that ignore rejected alternatives, and any generated output drift unless the wave is split. |
| W2 | Hand-authored typed sinks, hidden schema/directives, direct digest rows as product proof, capacity prescan, and reuse of typed Vec admission to reopen retained/direct rejected routes. |
| W3 | Pair-token, function-pointer, 12-byte token churn, separator elision, generic SWAR whitespace, Class A tiny-string, EventCursor/sidecar/aux cursor, retained wide-string scanners, Unicode escape validator/classifier retries, object key/value-byte carry, `tape_vs_tape` consumer, and unconditional PMULL/CTZ. |
| W4 | No-allocation visitor, sink-local decoded stats, quote-source streaming hash, direct source-hook folding, parser-owned decoded scratch, byte-output unescape, semantic string facts for digest workload, raw f64 shortcut, and stale mantissa-widen assumptions. |
| W5 | JSON policy leakage in generic crates, old public JSON helpers, `StructuralAlphabet::json`, `skip_json`/`match_json`/`unescape_json`/`StrictJson` style public APIs, renamed generic JSON helpers, and performance claims from boundary cleanup. |
| W6 | Paper close, missing REDRESS/RESULTS evidence, PMULL default prefix-XOR, CTZ/bulk default consumer, correctness/checkasm-only primitive admission, B6 canary-as-performance, feature-architecture analogy without profiles, and dropping W10/W10b falsifier rows. W6 is close/reconciliation in the folded SPEC, not a primitive implementation wave. |

## §5 - Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## §6 - Residual Risks

- REDRESS is long and some rejected routes are clustered by family. The ledger names each blocked family and every S-P3-required item, but CH3 should still diff P3-F's SPEC against REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, and the historical list (`PASS-3 Synthesis-Plan role and gate sections`).
- P3-B may choose different wave numbers than the W0-W6 ownership map above. If so, the owning evidence must move with the candidate; the block itself does not relax.
- Bitmap work has a possible changed density-gated framing from Alpha-E, but the default PMULL/CTZ production body routes remain rejected until the full primitive plus JSON row gate passes.
- Strictness remains the highest-risk accounting boundary: current C++ sidecar rows and any permissive rows are not strict admission evidence unless W0/P3-D binds same-run output-plane provenance.

## §7 - Self-Verdict

Verdict: ACCEPT.

Confidence: 94%.

Blockers: none.

Rationale: the ledger preserves V6/V7 governance, forbids hidden sidecars and substrate expansion, maintains strict-vs-strict comparator discipline, preserves Lock 14 grammar-neutrality, blocks new directive/BIR/substrate routes, rejects deferral/paper-close, and requires same-wave measurable evidence for every changed framing.

Required folds if CHALLENGE returns REVISE: name the missing REDRESS item or route family, add the exact blocked/unblock/owner row to S2, and mirror the new route in the S4 per-wave pre-block checklist before P3-F drafts SPEC.
