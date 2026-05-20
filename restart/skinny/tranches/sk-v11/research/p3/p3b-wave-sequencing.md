# SK-V11 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-20.
Scope: order the accepted SK-V11 S-P2 candidate pool into a topological W0-W9 wave plan.
Output: this file.
Pass Alpha goalset: close the 13 residual `direct_to_struct N-direct / NO-GO` rows or record per-row measured uncloseable REDRESS proofs; maintain the 7 typed and 4 direct `A / GO` rows; admit at least one benchmarked non-JSON generated direct/typed intervention; keep SK-V11 AArch64-only and micro-prove-first; do not reopen parse-only SOTA or the SK-V9 W3 union/substrate family.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

S-P3 has a converged candidate pool, not a blank design surface. S-P2 converged
with two consecutive 6/6 ACCEPT challenge cycles and names C1-C7 as the parser
primitive pool, C8 as output-oracle/per-product host sink only, C9 as Lock
1/output-plane accounting only, and W3 union repair as REDRESS-closed
(`research/p2/hardening/HARDENING-S-P2-CONVERGED.md:7-32`). The direct close
surface is the SK-V11-open 13-row residual table, with each row required to
clear the same-run sonic-rs direct 1.10x digest floor on generated Track 1 and
independent Track 2 or receive a measured uncloseable proof
(`SYNTHESIS.md:41-44`, `SYNTHESIS.md:106-124`).

The sequence therefore has four ordering constraints:

1. W0 must run first. PASS-3 makes W0 the baseline-profile / telemetry-lock
   wave, and SK-V11 requires telemetry binding before behavior work
   (`PASS-3-SYNTHESIS-PLAN.md:58-60`, `SYNTHESIS.md:76-78`).
2. Non-JSON proof must precede generic parser primitive claims. P2-F says a
   generated non-JSON benchmark is required before any C1-C7 S-P3 shortlist
   admission, and the live `json_provider` emission path is a Lock 14 blocker
   (`p2f-grammar-neutral.md:72-91`, `p2f-grammar-neutral.md:113`).
3. AArch64/SIMD work is micro-prove-first and same-wave-consumer-bound. P2-B
   requires scalar oracle, differential/checkasm where applicable, feature gate,
   caller microbench, and a same-wave consumer before production
   (`p2b-dav1d-process.md:247-264`); the wave contract rejects orphan kernels
   (`SKINNY-TRIUMVIRATE.md:177-186`).
4. W3/substrate repair is not a dependency. REDRESS 96/97 falsified the union
   substrate implementations, REDRESS 98 retired `G-W3-UNION-SUBSTRATE`, and
   REDRESS 102 firewall-closed parse-only row movement (`REDRESS.md:2797-2848`,
   `REDRESS.md:2852-2906`, `REDRESS.md:2910-2940`,
   `REDRESS.md:3042-3058`).

The resulting bracket is W0-W9: 10 waves, inside the <=12 skinny ceiling
(`SKINNY-TRIUMVIRATE.md:102-110`). It consumes at most eight candidate surfaces:
C1-C7 plus C8. C9 is carried in every wave as output-plane accounting, not a
row-moving candidate. `HEX_QUARTET_X4_PROOF`, movemask, PMULL/CTZ, EOR3/BCAX,
PRFM/STNP, and cache hints remain support or inventory unless a later wave
names a complete source delta, scalar oracle, strict parity/checkasm,
feature/fallback, same-wave consumer, and measured row gate
(`HANDOFF.md:118-127`, `p2f-grammar-neutral.md:58-68`).

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

Wave phases inherit the triumvirate caps: research 30 min per agent, plan 30
min, redress 75 min unless the SPEC narrows it, and CHALLENGE 60-90 min for
first-of-class or high-risk plans (`SKINNY-TRIUMVIRATE.md:165-173`). Every
behavior wave below should be CHALLENGE-gated before redress except W0 and W9.

| Wave | Title | Candidate surfaces | Owner path family | Entry gate | Dispatch status / dependencies | LOC budget + hard cap | Why order is valid |
|---|---|---|---|---|---|---:|---|
| W0 | SK-V11-open Telemetry And Gate Lock | none; telemetry/schema only | `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/crates/bbnf-bench/src/*gate*`, report/render/gate-json manifests | G-Alpha presented; S-P1/S-P2 converged; no behavior source changes in this wave | Dispatchable first | 0 behavior LOC; <=180 report/gate/test/doc LOC; <=90 min | W0 is required before behavior waves; it freezes direct/typed guard floors, strict comparator freshness, Track 1/Track 2 independence, and any non-JSON telemetry rows. |
| W1 | Grammar-Agnostic Generated Runtime And Non-JSON Harness Gate | C9 accounting; Lock 14 enabling surface, not a parser row mover | `skinny/crates/codegen/src/*`, `skinny/crates/runtime/src/grammars/*`, `skinny/crates/bbnf-bench/src/*`, `grammar/css/l4/*`, gate/report schema | W0 closes with gate-json consuming all SK-V11-open fields | Conditional on W0; blocks W2 and any later generic-crate primitive wave | <=450 source/test LOC plus generated diff audit; <=90 min | The live JSON-provider path is a blocker before CSS/Sheets/BBNF-self proof. This wave creates the grammar-neutral generated-runtime/bench lane without claiming direct row movement. |
| W2 | CSS L4 Generated Direct/Typed Intervention Proof | C1/C2/C4/C5/C6 as scalar-first generated-parser intervention; C7 support only | CSS L4 grammar templates, generated runtime, generated direct/typed benchmark, non-JSON oracle/Track 2, gate-json non-JSON consumer | W1 closes; CSS L4 declaration-values baseline exists; scalar references and throwaway microproofs are recorded | Conditional on W1; blocks JSON generic primitive waves that touch `parse-that-regex`, `bbnf-simd`, or codegen-generic surfaces | <=650 source/test LOC plus generated outputs; <=90 min | Non-JSON generality must be exercised before generic C1-C7 claims. CSS L4 is first because P2-F says it exercises dispatch, string, escape/hex, numeric, layout, and typed/direct output together. |
| W3 | Numeric Direct Closure Slice | C4 digit span/accumulate; D4 number-slot emit shape; optional C1/C5 support | `skinny/crates/parse-that-regex/src/number/*`, `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`, generated JSON direct/typed numeric callers, direct Track 2/oracle benches | W2 closes or rejects with non-JSON route preserved; numeric scalar oracle and same-host caller microbench pass | Conditional on W2 disposition; first JSON direct behavior wave | <=500 source/test LOC plus generated output; <=90 min | Numeric/near-floor rows are low-order risk and give fast falsification: `mesh` and `canada` are near floor, `numbers` is W0-clamped and Track 2-near, and `instruments` needs behavior provenance. This guards the direct plane before string/escape risk. |
| W4 | Generated Dispatch And Byte-Set Control Slice | C1 byte-set/class masks, C5 byte-set layout skip, C6 FIRST/prefix/lookahead, C7 movemask support; D1/D2 consumer shape | generated JSON direct dispatch, codegen lowering for `SinkOnly`, optional `bbnf-simd` TBL/TBX classifier only inside same-loop consumer, direct Track 2/oracle benches | W3 admits or rejects with REDRESS; W2 non-JSON proof remains valid for any generic edit; same-host classifier/dispatch microbench passes | Conditional on W3 disposition | <=650 source/test LOC plus generated output; <=90 min | Dispatch/control follows numeric because it can affect more rows and must not be confused with W3 structural substrate. It consumes transient masks only in same-loop direct/typed callers. |
| W5 | Bounded String Span And Special-Byte Scan Slice | C2 bounded string scan; C7 movemask support; D3 borrowed string span | `skinny/crates/parse-that-regex/src/lib.rs`, `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`, generated JSON direct/typed string callers, direct Track 2/oracle benches | W4 admits or rejects; caller microbench passes on named string rows; strict checkasm passes if any native body is used | Conditional on W4 disposition | <=550 source/test LOC plus generated output; <=90 min | String scan work waits until dispatch/control settles so row movement is attributable and the same-wave consumer can be named at the actual string/key call site. |
| W6 | Escaped Segment And Hex Decode Slice | C3 escape segment / hex decode; `HEX_QUARTET_X4_PROOF` support only if new segment source delta exists | `skinny/crates/parse-that-regex/src/lib.rs`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`, generated direct/typed escaped-string callers, CSS/BBNF escaped-string or host hex consumer if used | W5 admits or rejects; new escaped-segment consumer is named; x4 scalar oracle/checkasm pass before any native body | Conditional on W5 disposition | <=550 source/test LOC plus generated output; <=90 min | Escape work depends on the string span seam. It may not reuse the already-consuming `unescape_string` path as production; a real source delta and row gate must land same-wave. |
| W7 | Output Digest Host-Sink Fold | C8 per-product host sink/oracle only; C9 accounting | `skinny/crates/bbnf-bench/src/direct_struct.rs`, generated direct/typed output sinks, report/gate oracle paths | W3-W6 have dispositions; fresh post-intervention profile still names `output_digest_hash` as limiting on residual rows | Conditional on W3-W6 dispositions | <=350 source/test LOC; <=90 min | Digest/hash is sequenced after parser primitives so it cannot masquerade as parser semantics. It can only move rows as a product output sink with strict Track 1/Track 2-or-oracle parity. |
| W8 | Direct Residual Fixpoint And Row Reclamation | no new primitive by default; consumes any remaining C1-C8 measured residual route | direct row benches, `skinny/RESULTS.md`, `skinny/REDRESS.md`, gate-json row-state updates | W3-W7 all admitted or rejected; W2 non-JSON intervention admitted or explicitly routed | Conditional on all behavior dispositions | <=250 docs/gate/result LOC plus optional <=250 narrowly scoped source LOC if P3-C names one remaining candidate; <=90 min | This wave prevents paper close. Every remaining direct row either becomes `A / GO` under strict measured evidence or receives a per-row REDRESS uncloseable proof naming exhausted candidates. |
| W9 | Close And Alpha Feedback | none; close docs only | `SYNTHESIS.md`, `HANDOFF.md`, `SPEC.md`, `DISPATCH-PROMPT.md`, `skinny/RESULTS.md`, `skinny/REDRESS.md`, Alpha handoff docs | W8 closes; at least one non-JSON intervention is admitted or the bracket is BLOCKED by the close condition | Conditional on W8 | 0 source LOC; <=90 min | Closes only after the direct and non-JSON axes have measured dispositions, then presents G-Alpha for SK-V11 -> SK-V12. |

Topological summary:

- W0 blocks all behavior because telemetry and gate consumers are entry facts.
- W1 blocks W2 because non-JSON generated-parser proof cannot run through the
  current JSON-provider-only emission path.
- W2 blocks generic C1-C7 behavior waves because SK-V11 requires exercised
  non-JSON generality, not prose.
- W3-W7 are ordered by dependency and attribution: numeric/control,
  dispatch/byte-set, string span, escape segment, output sink.
- W8 runs only after row-moving candidates have dispositions; W9 runs only
  after W8 has either closed every row or recorded fixpoint REDRESS evidence.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

P3-C owns final gate prose, but P3-B binds the rows each wave is allowed to
move. JSON direct thresholds below are the SK-V11-open seed floors from
`SYNTHESIS.md:106-124`; both generated Track 1 and independent Track 2 must
meet or exceed the floor for admission. Existing typed and direct `A / GO` rows
from `SYNTHESIS.md:130-147` are full-table maintain guards on every behavior
wave, with exact maintain floors to be set by P3-C from W0.

| Wave | Required measurable gate |
|---|---|
| W0 | Reproduce SK-V11-open authority and make `gate-json --with-cost-facts --check-results` consume every required field. No behavior row admission. |
| W1 | Non-JSON telemetry/gate harness exists and rejects missing grammar domain, comparator/oracle, output plane, Track 2/oracle, run id, feature mask, and same-wave consumer fields. No behavior row admission. |
| W2 | CSS L4 declaration-values generated direct/typed row: W1 baseline Mbps becomes the seed; exit requires strict semantic equality, generated Track 1 and independent Track 2/oracle, at least `ceil(W1_css_baseline_mbps * 1.01)` on one admitted non-JSON intervention row, and no named JSON guard regression beyond P3-C's maintain floor. If W1 cannot produce the baseline row, W2 is REVISE before redress. |
| W3 | Numeric/near-floor direct targets: `canada >= 10637`, `mesh >= 8675`, `numbers >= 2425`, `instruments >= 8969`; optional numeric guard/target `marine_ik` direct stays admitted and typed numeric guards stay within maintain floors. |
| W4 | Dispatch/control direct targets: `twitter >= 13740`, `github_events >= 13403`, `update_center >= 10059`, `random >= 7878`, plus any W3 residual among `canada`, `mesh`, `numbers`, `instruments` only if the plan names a single same-wave consumer. |
| W5 | String-heavy direct targets: `twitter >= 13740`, `github_events >= 13403`, `update_center >= 10059`, `random >= 7878`, `gsoc-2018 >= 3737`, `distinct_values >= 2658`, `y_string_unicode >= 3950`; Unicode rows are guards unless the plan also names escape-segment consumer work. |
| W6 | Escape/Unicode direct targets: `unicode_escapes >= 3441`, `unicode_mixed >= 2588`, `y_string_unicode >= 3950`; optional CSS/BBNF escaped-string proof row uses W2/W1 non-JSON baseline formula. Existing `unescape_string` reuse alone is a gate failure. |
| W7 | Residual output-sink targets: every post-W6 `N-direct` row still below its seed floor from the 13-row table, with the plan required to prove `output_digest_hash` remains limiting before redress. C8 cannot close a parser row by itself. |
| W8 | All 13 direct residual rows are either `A / GO` under their seed floors or have REDRESS uncloseable proofs: `twitter >= 13740`, `canada >= 10637`, `github_events >= 13403`, `update_center >= 10059`, `mesh >= 8675`, `random >= 7878`, `gsoc-2018 >= 3737`, `instruments >= 8969`, `numbers >= 2425`, `unicode_mixed >= 2588`, `unicode_escapes >= 3441`, `distinct_values >= 2658`, `y_string_unicode >= 3950`. |
| W9 | Close documents agree with `RESULTS.md` and `REDRESS.md`; no new row movement. If W8 leaves a direct row without `A / GO` or REDRESS proof, W9 is BLOCKED. |

Every wave also inherits micro-prove-first when it lands a kernel or primitive:
scalar reference, strict checkasm/differential where applicable, feature gate,
fallback, same-host caller microbench, same-wave hot-path consumer, and
samply-visible consumer path. Missing consumer means REJECT, not deferral
(`SKINNY-TRIUMVIRATE.md:177-186`).

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

Global pre-blocks for all waves:

- Parse-only SOTA movement is closed. W0/W8/W9 may report parse-only rows only
  as diagnostics; no behavior wave may count them as admission
  (`SYNTHESIS.md:51-53`, `REDRESS.md:3042-3058`).
- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate is
  retired. No wave may introduce a retained class column, structural-position
  vector, parser-owned projection, streaming cursor, aux density table, or
  sidecar event vector (`REDRESS.md:2797-2940`).
- Generic-crate JSON policy, new directives, new BIR or `BackendShape`
  variants, public substrates, and hidden schema facts are blocked
  (`SYNTHESIS.md:72-75`, `HANDOFF.md:133-136`).
- PMULL prefix-XOR and CSSC CTZ/bulk production rewires stay blocked unless a
  later accepted plan supplies a materially different product-plane packet
  (`p2f-grammar-neutral.md:107`).
- x86 implementation is out of scope (`SYNTHESIS.md:62-67`).

Wave-specific pre-block map:

| Wave | Routes not to reopen |
|---|---|
| W0 | Schema completion as row admission; stale/historical sidecars as strict anchors; producer-only telemetry. |
| W1 | JSON-provider emission as non-JSON proof; hidden directives/BIR variants; old non-JSON hand runtimes as generated-parser proof. |
| W2 | Prose-only Lock 14 proof; generic CSS/Sheets/BBNF policy branches; non-JSON benchmark without generated Track 1 and independent Track 2/oracle. |
| W3 | REDRESS 80 numeric fallback/mantissa/table-only/f64 rewrite; direct admission from parse-only or retained-materialization counters. |
| W4 | REDRESS 50/51/53/92/96/97/98/102 sidecars, cursors, class columns, W3 substrate repair; REDRESS 63/65/84 object/key/value-byte carry beyond local same-loop probes. |
| W5 | REDRESS 54/55/60/61/62/67/68/69/72/83/106 string materialization, retained wide scans, `StringBlock16` retained wrappers, primitive-parity-only production. |
| W6 | REDRESS 64/82/107/108 Unicode/x4 proof-to-production overfit, single-quartet materializer, and reuse of the already-consuming `unescape_string` caller as a new production admit. |
| W7 | Digest/hash as parser semantics; semantic string facts or hash side tables; cache hints/prefetch without fresh output-sink hot-leaf evidence. |
| W8 | Paper-close by "routed residual" without per-row measurement; W0-clamped rows admitted without behavior provenance. |
| W9 | Close document drift, future-phase promises, or G-Alpha presentation while any W0-W8 wave lacks admitted/rejected/measured status. |

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:50-63`,
  `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:73-145`.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:100-125`,
  `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:165-186`.
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md:31-85`,
  `restart/skinny/tranches/sk-v11/SYNTHESIS.md:93-150`.
- `restart/skinny/tranches/sk-v11/HANDOFF.md:37-88`,
  `restart/skinny/tranches/sk-v11/HANDOFF.md:115-136`.
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:32-55`.
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:7-34`.
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:43-63`.
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:52-224`.
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:247-288`.
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:24-82`.
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:28-58`.
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:21-95`.
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:35-113`.
- `restart/skinny/tranches/sk-v8/SPEC.md:214-257` for the manifest/cap shape
  mirrored by SK-V11.
- `skinny/RESULTS.md:1-146` for SK-V11-open row states, Track 1/Track 2
  independence, strict comparator fields, and direct residual notes.
- `skinny/REDRESS.md:2797-2940`, `skinny/REDRESS.md:3042-3058`,
  `skinny/REDRESS.md:3152-3222`.
