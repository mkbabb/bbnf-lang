# SK-V13 Alpha-E Candidate Shortlist

Pass: Alpha SK-V12 -> SK-V13, lane alpha-E.
Date: 2026-05-21.
Scope: candidate intervention families for SK-V13 S-P3 wave planning.

## Authority Read

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- SK-V12 close evidence: `css_l4/declaration_values/direct_to_struct/main` at
  429.34 Mbps vs lightningcss 168.93 Mbps, strict same-plane equality, and
  JSON guard floors held.

## Binding Reconciliation

PASS-ALPHA nominally asks alpha-E for <=5 candidates. The 2026-05-21 addendum
requires extended wave families: full CSS L4 lightningcss parity, every JSON
row and plane above sonic-rs strict, `parse_only` admission eligibility, and no
fixpoint close except architectural intrinsic-block. This shortlist therefore
contains exactly five candidate families. Each family is allowed to fan out in
S-P3 into the addendum-required W10/W11/W14 subwaves.

Global gates for every family:

- **CSS:** every non-OUT_OF_SCOPE CSS L4 feature must be either
  ADMITTED-PARITY with Track 1 > lightningcss + 1 Mbps on the same strict
  output plane, or carry an architectural intrinsic-block proof.
- **JSON:** all 51 JSON rows, including the 13 REDRESS-119 N-direct rows and
  all `parse_only` rows, are admission-eligible and must beat sonic-rs strict
  by satisfying `Track 1 > sonic-rs strict Mbps + 1` on the same plane, or
  carry architectural intrinsic-block proof.
- **Comparator plane:** sonic-rs strict is the JSON SOTA comparator. Lossy or
  permissive comparators are flaw probes only.
- **Behavior-wave rule:** every behavior wave must move at least one row toward
  SOTA or prove intrinsic-block for the touched row family. Support-only,
  checkasm-only, parse_only-demoted, and future-consumer landings reject.
- **Same-wave consumer:** every primitive, union route, resolver rule, or
  generated CSS production lands with its hot-path consumer and row gate in the
  same wave.

## Shortlist

| ID | Candidate family | Required fanout | Primary rows | LOC envelope | Risk |
|---|---|---|---|---:|---|
| E1 | CSS L4 lightningcss parity expansion | W3/W4 + W10.{1..N} | 23 remaining CSS L4 parity features | 8.0k-21.9k source/test upper envelope across one-wave-per-feature fanout; generated LOC separately accounted | high |
| E2 | Per-grammar value/config/sink expansion | Consumed by E1 and optional Sheets fallback | CSS stylesheet/value rows; future Sheets/BBNF-self legality | 1.5k-2.1k | high |
| E3 | Decision-engine fold: bbnf-regex + egraph + active cost + CSP + cascade deletion | W5-W9, feeds W11/W14 | JSON direct/typed/parse_only route generation | 2.3k-3.6k | very high |
| E4 | Legal same-tape union substrate | W8/W12, feeds W11 and CSS rows | JSON structural/projection rows; CSS selector/context rows | 550-1.4k | high |
| E5 | SIMD/ASM consumed kernels | W4b/W12 + W11.{1..13}/W14.{1..K} consumers | CSS delimiter/number/string rows; JSON structural/string/number/parse_only rows | 800-1.6k | very high |

## E1 - CSS L4 Lightningcss Parity Expansion

Purpose: turn the SK-V12 declaration-value admission into full CSS L4 semantic
parity against lightningcss. The current admitted row covers only a tiny
declaration-value fixture and 8 token kinds; the scoping matrix records 7
partial and 16 missing feature families. E1 owns the generated CSS behavior
rows and the W10.{1..N} fanout.

Owner paths:

- `grammar/css/l4/`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/css_*`
- `skinny/crates/codegen/src/*css_l4*`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- future `skinny/crates/runtime/src/grammars/css_l4_stylesheet/`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v13/research/w*/css-l4/`

Wave fanout:

- W3/W4: stylesheet root, selector framework, and declaration-value expansion.
- W10.{1..N}: one wave per non-OUT_OF_SCOPE CSS parity feature not admitted by
  W3/W4. Expected fanout includes variables/calc/color functions, visual
  functions, at-rules/media/keyframes, nesting, vendor/custom at-rules, and any
  remaining non-OUT_OF_SCOPE parity matrix row.

Scalar/checkasm status:

- Scalar/oracle reference is lightningcss 1.0.0-alpha.71 with error recovery
  disabled plus independent cssparser or hand-checked golden facts where
  cssparser lacks coverage.
- checkasm is N/A for scalar-generated CSS rows unless E5 SIMD kernels are
  consumed. If E5 is consumed, checkasm must be same-wave and row-bound.

Same-wave consumer plan:

- Each W10.N lands one generated CSS parser production family plus the
  same-plane lightningcss fact extractor and Criterion/gate row for that exact
  feature.
- Producer-only grammar/codegen support rejects unless the same wave emits and
  measures a named CSS row.

Falsifiability gate:

- `G-W10-N-CSS-L4-PARITY-<feature>` passes only if Track 1 > lightningcss + 1
  Mbps, strict equality passes, feature acceptance/rejection matches
  lightningcss, independent oracle passes, and no previously admitted CSS row
  silently demotes.
- Failure either records a measured implementation REDRESS and continues to the
  next W10 fanout, or records architectural intrinsic-block evidence for the
  feature. Implementation-limited blockers are not closes.

LOC/risk:

- 350-950 LOC per CSS feature family, depending on selector/at-rule/function
  complexity; 8.0k-21.9k source/test upper envelope across 23 feature rows if
  S-P3 keeps one wave per feature. S-P3 may bundle features only when one
  measured row/gate covers the bundle; generated LOC is separately accounted.
- Risk high: selector recursion, media/nesting grammar, and lightningcss
  semantic projection are broad. The material risk is oracle mismatch, not
  pure throughput.

Dependencies:

- E2 for grammar-specific dispatch/sink/value policy.
- E5 only when a CSS row chooses SIMD layout/string/number acceleration.

Material differential:

- SK-V12 proved only declaration-value token facts over a 187-byte fixture.
  E1 moves to full lightningcss semantic parity, feature by feature, on
  real CSS corpora such as Bootstrap, Tailwind, Material Design, and Animate.css.

## E2 - Per-Grammar Value/Config/Sink Expansion

Purpose: remove the remaining JSON policy leaks from generated runtime
behavior without adding a public `GrammarConfig` trait or new substrate API.
E2 is a legality and row-consumer family, not a paper abstraction family.

Owner paths:

- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- future `skinny/crates/runtime/src/grammars/css_l4_stylesheet/`
- future `skinny/crates/runtime/src/grammars/sheets/` only if CSS blocks and
  S-P3 authorizes fallback
- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`

Scalar/checkasm status:

- Scalar reference is byte-for-byte JSON generated parity plus CSS strict
  fact-stream parity against lightningcss for the row consuming each new policy.
- checkasm N/A unless the config expansion routes into E5 SIMD string/number
  helpers.

Same-wave consumer plan:

- Each policy expansion must be consumed by a generated grammar row in the same
  wave: CSS dispatch table, CSS whitespace/comment skipping, CSS number/string
  policy, CSS `DirectSink`, or grammar-specific value wrappers.
- A generic config field with no CSS/generated consumer rejects.

Falsifiability gate:

- `G-WX-GRAMMAR-POLICY-CONSUMED` passes only if the consuming CSS row holds
  strict lightningcss parity, JSON guard rows do not demote, and Lock 14 shows
  no generic branch on grammar name, corpus name, JSON object/array role,
  field name, string role, or layout role.
- Any public trait, new directive, new BIR variant, new `BackendShape`, public
  `UnionTape`-style substrate, or grammar-specific generic behavior is REJECT.
  S-P3 may narrow owner paths and gates; it cannot authorize those surfaces.
  Only user re-pin outside SPEC can change this scope.

LOC/risk:

- Per-grammar config expansion: 600-900 LOC.
- Grammar-specific view/sink emission: 900-1200 LOC.
- Risk high because it touches generic codegen/runtime and JSON guard rows.

Dependencies:

- Precedes most of E1.
- Feeds E4 by providing codegen-private per-rule policy tables.

Material differential:

- W1a resolved only the structural alphabet fully. E2 moves dispatch, string,
  number, flag interpretation, and sink shape out of JSON-specific generated
  paths and into generated per-grammar modules consumed by CSS rows.

## E3 - Decision-Engine Fold

Purpose: replace the hardcoded P1-P8 backend cascade with a resolver that can
generate fresh material routes for every JSON row/plane, including the rows
previously frozen under REDRESS-119 and now reopened by the addendum.

Owner paths:

- new `skinny/crates/bbnf-regex/`
- optional new `skinny/crates/wasm-bbnf-regex/`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/ir/src/lib.rs`
- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/src/lib.rs`
- new `skinny/crates/passes/src/egraph/`
- `skinny/crates/passes/Cargo.toml`
- `skinny/crates/codegen/src/lower/`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`

Wave fanout:

- W5: extract `bbnf-regex`; remove hardcoded regex nullability/first-set
  predicates from IR.
- W6: egraph Language and bounded rewrite set.
- W7: active cost function from measured CostFacts.
- W8/W9: CSP integration and P1-P8 cascade deletion.
- W11.{1..13}: one JSON N-direct residual row per fanout wave if not admitted
  earlier by W5-W9.
- W14.{1..K}: parse_only rows reopened using resolver-produced routes.

Scalar/checkasm status:

- Scalar reference is the current generated JSON/CSS behavior before resolver
  extraction. Resolver changes must preserve strict output equality.
- checkasm N/A unless the extracted plan selects E5 primitives; then E5 gates
  become same-wave prerequisites.

Same-wave consumer plan:

- Every resolver rewrite or CSP constraint must be consumed by generated code
  for at least one named JSON or CSS row in the same behavior wave.
- If W5 is a non-behavior prerequisite, S-P3 must bind it to W6/W7 in the same
  committed plan and it cannot be a close-bearing wave. The first behavior
  closure must show row movement or intrinsic-block proof. No orphan
  `bbnf-regex`/resolver stage is admissible.

Falsifiability gate:

- `G-W5-W9-RESOLVER-FOLD` passes only if `choose_backend_shape()` is no longer
  the live selection path, the resolver produces deterministic candidates, the
  selected row beats its strict comparator or improves measured margin, and all
  existing admitted rows stay admitted.
- After the resolver wave, the hardcoded P1-P8 cascade is not an admissible
  production fallback for JSON, CSS, Sheets, or BBNF-self rows. Any retained
  compatibility path must fail closed with visible row rejection/non-admission
  rather than silently winning through the old cascade.
- Abrogate criteria: egraph OOM, CSP >1s per grammar, stale cost data on >30%
  of expressions, or any rewrite that drops required output fields. Abrogation
  must name architectural or implementation reason; implementation abrogation
  continues in the next wave/fanout.

LOC/risk:

- `bbnf-regex`: 210-330 LOC.
- egraph language/rewrites/cost: 850-1250 LOC.
- active cost and evidence wiring: 500-800 LOC.
- CSP and cascade deletion: 790-1210 LOC.
- Risk very high: solver blowup, stale costs, and hidden coupling to codegen
  internals.

Dependencies:

- E2 for grammar-neutral policy data.
- E4/E5 can be selected by the resolver, but cannot land without their own
  parity/checkasm/consumer gates.

Material differential:

- Prior JSON residual attempts were hardcoded route probes. E3 changes the
  route generator: regex facts, egraph alternatives, measured cost, and CSP
  constraints create new candidate kernels for all JSON rows and planes rather
  than reusing the REDRESS-119 failed route set.

## E4 - Legal Same-Tape Union Substrate

Purpose: exercise the user-unblocked union category with a legal same-tape
implementation. REDRESS 96/97/98 remain historical measured rejections; E4 must
name a material differential and cannot introduce a sidecar substrate.

Owner paths:

- `skinny/crates/codegen/src/lower/`
- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/runtime/src/tape/event_grammar.rs`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- future `skinny/crates/runtime/src/grammars/css_l4_stylesheet/`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

Candidate variants:

- C1: GrammarConfig/per-rule shape selection, codegen-private and monomorphic.
- C2: egraph-selected union shape per equivalence class, conditional on E3.
- C3: SIMD-first PMULL+CSSC lane index, conditional on E5.

Scalar/checkasm status:

- Scalar reference is the current single-tape OffsetTape/EventTape behavior for
  the same row.
- checkasm N/A for C1/C2 if scalar-only. C3 inherits E5 PMULL/CTZ checkasm
  and corpus parity.

Same-wave consumer plan:

- C1 must be consumed by a CSS selector/context row or JSON structural row in
  the same wave.
- C2 must show measured delta over C1 or the scalar baseline, not just a solver
  proof.
- C3 must wire the SIMD lane index into JSON structural or parse_only scanning
  in the same wave.

Falsifiability gate:

- `G-W8-W12-SAME-TAPE-UNION` passes only if a named row improves toward SOTA or
  admits, strict equality holds, no public substrate API is added, no sidecar
  class column/vector/list/cursor is retained, no parse_only demotion occurs,
  and JSON/CSS guard rows do not silently demote.
- If union fails twice on the same row family, escalate only with
  architectural intrinsic-block evidence or continue under a new material
  differential per the round-trip rule.

LOC/risk:

- C1: 150-250 LOC.
- C2: 400-800 LOC, depending on E3 reuse.
- C3: 430 LOC plus E5 test cost.
- Risk high to very high: prior union attempts regressed; hidden sidecar
  coupling is the primary CH5 risk.

Dependencies:

- E2 required.
- C2 depends on E3.
- C3 depends on E5.

Material differential:

- REDRESS 96/97/98 used fixed/global class-column or streaming-cursor event
  models. E4 is codegen-private, per-rule/per-grammar, same-tape, and
  row-consumed. No parallel `UnionTape`, retained structural vector, or
  parser-owned cursor/list is allowed.

## E5 - SIMD/ASM Consumed Kernels

Purpose: turn the unblocked ARMv9.2/SIMD category into row-moving kernels with
scalar references, checkasm, and same-wave consumers. The zero-orphan rule is
strict: no primitive lands unless its production row consumer lands and is
measured in the same wave.

Owner paths:

- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/scalar/`
- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/bbnf-simd/src/dispatch.rs`
- `skinny/crates/bbnf-simd/tests/`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- future `skinny/crates/runtime/src/grammars/css_l4_stylesheet/`
- `skinny/crates/parse-that-regex/src/lib.rs` only for grammar-neutral scalar
  byte/number helpers
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

Candidate kernels:

- W4b `a64_ascii_set_run_skip` production split for CSS delimiter/layout scan.
- PMULL+CSSC CTZ structural-position extraction for JSON structural/direct or
  parse_only rows.
- 64-byte string-special scan for JSON string-heavy rows and CSS escaped
  identifiers if those become W10 rows.
- UDOT digit-run span only if fresh profile names numeric density high enough;
  otherwise it should become measured-reject evidence, not a default primitive.

Scalar/checkasm status:

- `escape_mask_64` implementation is currently parity-green at caller level,
  but direct differential tests and adversarial scanner windows remain required
  before new string/escape SIMD admission.
- `a64_ascii_set_run_skip` has W4 microbench parity and 4.72x scalar speedup,
  but lacks production wiring.
- PMULL/CTZ/UDOT require scalar references and expanded checkasm matrices
  before row claims.

Same-wave consumer plan:

- CSS SIMD consumers: generated CSS scan-block layout skip, delimiter dispatch,
  string-interesting scan, or number-token span.
- JSON SIMD consumers: structural scan, direct projection, string-special scan,
  number projection, or parse_only scanner.
- W11.{1..13} must bind one reopened N-direct row per fanout wave unless the
  row admits earlier.
- W14.{1..K} must bind parse_only rows directly; parse_only is no longer
  diagnostic.

Falsifiability gate:

- `G-WX-SIMD-CONSUMED-KERNEL` passes only if scalar differential/checkasm pass,
  the production consumer executes in the measured row, Track 1 beats strict
  comparator or moves measurably toward it, and zero aarch64 orphans remain.
- Row thresholds: CSS rows Track 1 > lightningcss + 1 Mbps; JSON direct/typed
  and parse_only rows Track 1 > sonic-rs strict + 1 Mbps.
- A parity pass with no row movement must remove the primitive or demote it
  with REDRESS evidence; it cannot remain as support-only inventory.

LOC/risk:

- W4b production split: about 120 LOC.
- PMULL+CTZ consumed structural path: about 430 LOC.
- 64-byte string-special scan: about 280 LOC.
- UDOT digit-run span: about 280 LOC.
- Aggregate E5 envelope 800-1600 LOC across selected kernels and tests.
- Risk very high for PMULL/CTZ and string cross-chunk correctness; medium for
  W4b due to existing microbench evidence.

Dependencies:

- E2 for grammar-neutral consumer policy.
- E3 may select row/kernel pairings.
- E4 C3 depends on E5.

Material differential:

- REDRESS 88/89 rejected broad PMULL/CTZ default bodies and REDRESS 126
  demoted five orphans. E5 only admits narrow, row-consumed kernels with
  scalar reference, checkasm, and same-wave row movement. W4b is specifically
  differentiated by a proved CSS delimiter-run microbench and a required CSS
  production consumer.

## Cost, Caps, And Concurrency Fold

Hard caps:

| Family / waves | Research | Plan | Redress | Notes |
|---|---:|---:|---:|---|
| E1 CSS W3/W4/W10.N | 20 min | 15 min | 30 min | One feature row per behavior gate unless S-P3 bundles rows under one measured gate. |
| E2 GrammarConfig/value/sink | 20 min | 15 min | 30 min | Mostly precondition work; close-bearing only when consumed by CSS/JSON row gate. |
| E3 decision-engine W5-W9 | 20 min | 15 min | 45 min | Addendum redress-cap amendment applies. W5 cannot be close-bearing infrastructure. |
| E4 union C1/C2 | 20 min | 15 min | 30 min | Same-tape, codegen-private, row-consumed only. |
| E4 C3 / W12 union-SIMD | 20 min | 15 min | 45 min | Uses the decision-engine/union-SIMD extended cap. |
| E5 SIMD W4b/W11/W14 kernels | 20 min | 15 min | 30 min | Each primitive needs scalar reference, checkasm/parity, and same-wave row consumer. |

Concurrency and conflict matrix:

| Domain | Can parallelize with | Must serialize with |
|---|---|---|
| E1 CSS feature waves | Other CSS feature waves only when runtime, codegen, comparator artifacts, and gates are disjoint | RESULTS/REDRESS writes; shared CSS tokenizer or fact-stream schema edits; E2 prerequisite edits |
| E2 GrammarConfig/value/sink | Read-only pass work | Most E1/E4 behavior waves until the exact policy surface is consumed and stable |
| E3 W5-W9 decision fold | Non-overlapping CSS feature implementation after committed interfaces | W5-W9 internally unless S-P3 proves disjoint owner paths; `choose_backend_shape`/lowering/cost/CSP edits |
| E4 union | CSS/JSON rows only after C1/C2/C3 owner paths are isolated | E2 policy tables; E3-selected shape ownership; public substrate-adjacent files |
| E5 SIMD/ASM | CSS or JSON row consumers with disjoint kernel modules | Shared `bbnf-simd` dispatch, checkasm reports, RESULTS/REDRESS writes |

G-Omega closes before any source implementation wave. RESULTS and REDRESS are
single-writer ledgers even when redress worktrees run in parallel.

## Non-Shortlisted Items

- Standalone profiling/telemetry is required entry evidence, but not a
  behavior candidate. It belongs in S-P1/W0 capture and cannot close a behavior
  wave.
- Source maps, comments, and whitespace facts are out of the parity-critical
  close unless S-P3 reclassifies them as non-OUT_OF_SCOPE CSS features with a
  lightningcss same-plane row.
- Sheets and BBNF-self are not primary candidates while CSS L4 remains
  authoritative. They are fallback or generality probes only after CSS row
  redress blocks, or as E2 legality consumers if S-P3 needs a second grammar.
