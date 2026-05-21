# SK-V13 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-21.
Scope: order the converged S-P2 candidate families into SK-V13 implementation waves, including dependencies, gates, caps, concurrency, and ledger serialization.
Output: this file.
Pass Alpha goalset: G1 full CSS L4 parity, G2 decision-engine fold, G3 one union variant or architectural block, G4 zero aarch64 orphans, G5 all 51 JSON rows above sonic-rs strict including `parse_only`, G6 totality V1.1/G-Omega before W0, and G7 no demotion with immediate bracket if unmet.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

The SK-V13 candidate pool is bigger than a literal 12-wave skinny bracket if
each row/feature becomes its own triumvirate. The addendum names W10-style CSS
feature subwaves, W11.{1..13} JSON direct residual subwaves, and W14-style
`parse_only` subwaves, while the orchestrator still escalates a skinny bracket
that exceeds 12 waves. P3-B therefore sequences a 12-slot top-level bracket
(`W0` through `W11`) with explicit packed subwaves. Packed subwaves are legal
only when the plan keeps file domains disjoint, fits the redress hard cap, and
serializes ledger writes. If any packed subwave must dispatch as its own
triumvirate, it burns a real bracket wave; more than one such split likely
exceeds the ceiling and must either escalate or close/reject into an immediate
SK-V14 bracket. This is not a scope retreat: the full pinned campaign remains
open until every row/feature admits or carries architectural-block evidence.

`W0` is still blocked on G-Omega for behavior/source/report-result work. The
SK-V13 SYNTHESIS §0.6 and HANDOFF §3 allow S-P1/S-P2/S-P3 planning before
G-Omega, but no Wave 0 implementation, source edit, generated runtime change,
gate/report change, or `skinny/RESULTS.md` / `skinny/REDRESS.md` append may
start before G-Omega closes. P3-B can sequence W0; it cannot make W0
dispatchable.

The ordering follows the converged P1/P2 facts. P1 converged with JSON parse
and direct hot leaves classified as generated envelopes, one clean direct
unicode primitive (`parse_that_regex::unescape_string`), structural SIMD as a
scanner micro-result, 10 missing typed product surfaces, and CSS
declaration-values profiling dominated by timer/fact-sink overhead. P2
converged with CSS rows 1-6 as conditional row-production scopes, not primitive
admissions; with grammar-neutral candidates limited to generated policy,
byte-set, string/escape/number, regex-analysis, resolver, and same-substrate
boundaries; and with support-only SIMD/orphan inventory excluded unless a
same-wave consumer moves a row. REDRESS 119/120 are history under the pin, but
their direct-row evidence still requires a fresh material differential for
every reopen.

The topological constraints are:

- Telemetry and rolling-delta production precede every behavior wave.
- CSS foundation/policy precedes full CSS parity rows and any shared
  `bbnf-simd` use in CSS.
- Decision-engine extraction/fold precedes JSON residual sweeps because the
  addendum expects W5-W9-style resolver outputs to create fresh row routes.
- Union attempts follow the resolver and policy gates unless they use the
  simpler codegen-private C1 material differential; every union attempt must
  avoid REDRESS 96/97/98 sidecar/class-column/cursor repeats.
- JSON direct/typed/parse sweeps follow decision/union availability and must
  guard all 51 rows, not only the prior 13 direct residuals.
- Close cannot be a paper close: if any pinned row/feature remains below SOTA
  without architectural-block evidence, W11 closes SK-V13 as REJECT and
  brackets SK-V14 immediately.

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

### §2.1 Bracket Manifest

Shared phase caps for every wave are the SKINNY-TRIUMVIRATE caps unless a row
below states a narrower cap: research 30 min per agent, plan 30 min, redress 60
min implementation plus 15 min measurement. For decision-engine and union/SIMD
waves, the addendum's 45 min source-edit cap is binding inside the 75 min
redress wall. First-of-class CSS, resolver, union, or SIMD waves require a
60-90 min CHALLENGE before redress.

| Wave | Name | Dispatch status | Entry gate | Owner path families | Hard cap / LOC envelope | Concurrency |
|---|---|---|---|---|---|---|
| W0 | Baseline Telemetry + Rolling Delta Lock | `BLOCKED-PRE-G-OMEGA` for source/report/results work; planning only before G-Omega | G-Omega closed; S-P3 converged; no dirty behavior diff in owner paths | `skinny/crates/bbnf-bench/`, `skinny/xtask/src/`, `skinny/RESULTS.md`, `skinny/REDRESS.md` only on reject, `restart/skinny/ROLLING-SOTA-DELTA.md`, wave artifacts | 0 behavior LOC; <=300 gate/report/test/doc LOC; redress <=75 min | Not parallel. Ledger writer must be single-threaded. |
| W1 | CSS Foundation, Generated Policy, and Row-Production Spine | Conditional on W0 close | CSS W1b row maintained; Lock 14 policy plan accepted; same-wave strict CSS row target named | `skinny/crates/runtime/src/grammars/css_l4_*`, `skinny/crates/codegen/src/css_*`, `skinny/crates/parse-that-regex/`, CSS bench/oracle paths | <=650 hand source/test LOC per packed lane; split before dispatch if exceeded | CSS-only worktree eligible after W0, but not with decision waves touching shared `parse-that-regex` or codegen policy. |
| W2 | CSS Full-Parity Completion Pack + W4 Run-Skip Production Split | Conditional on W1 close | W1 stylesheet/selector row admitted or architecturally blocked; strict lightningcss/cssparser gates ready | Same CSS runtime/codegen/bench paths plus `skinny/crates/bbnf-simd/` for `ByteSetRunSkip64` only | Packed subwaves W2.1-W2.5, each <=650 LOC and <=75 min; SIMD lane requires strict checkasm | Worktree eligible only by disjoint CSS module. `bbnf-simd` lane serializes with any JSON/union SIMD lane. |
| W3 | Decision Fold 1: `bbnf-regex` + FIRST/Follow Facts | Conditional on W0 and W1 policy gate | Regex extraction consumer named; no support-only crate extraction | `skinny/crates/parse-that-regex/`, new or existing `skinny/crates/bbnf-regex/`, `skinny/crates/ir/`, `skinny/crates/passes/`, generated dispatch tests | 45 min source-edit cap; <=450 LOC selected slice; broader 210-330 LOC scoping envelope allowed only if it fits | Serialized with W4-W7; may not overlap CSS W1 if both touch `parse-that-regex`. |
| W4 | Decision Fold 2: E-Graph Language + Bounded Rewrites | Conditional on W3 close | `bbnf-regex` facts consumed; e-graph OOM guard accepted | `skinny/crates/passes/src/egraph*`, `skinny/crates/ir/`, codegen lowerer tests | 45 min source-edit cap; plan must choose a bounded rewrite subset; split if saturation/cost exceeds guard | Serialized decision wave. No parallel shared `passes/` worktree. |
| W5 | Decision Fold 3: Active CostFunction | Conditional on W4 close | E-graph extraction emits candidates; stale-cost rate <30% on sample | `skinny/crates/ir/src/cost.rs`, `skinny/crates/passes/src/egraph/cost*`, diagnostics/report tests | 45 min source-edit cap; <=500 LOC selected slice | Serialized decision wave. |
| W6 | Decision Fold 4: CSP Resolver | Conditional on W5 close | Costed candidate set exists; CSP timeout guard accepted | `skinny/crates/passes/src/egraph/csp_resolver*`, `skinny/crates/passes/Cargo.toml`, resolver tests | 45 min source-edit cap; <=650 LOC selected slice; solve time <=1 s per grammar, prefer <=200 ms | Serialized decision wave. |
| W7 | Decision Fold 5: P1-P8 Cascade Deletion / Fail-Closed Retirement | Conditional on W6 close | Resolver SAT on JSON + CSS samples; no fallback-to-old-cascade ambiguity | `skinny/crates/passes/src/lib.rs`, codegen lowerer, resolver diagnostics, generated-output audits | 45 min source-edit cap; <=300 LOC plus deletion; full JSON/CSS/Sheets/BBNF-self fail-closed proof | Serialized decision wave. |
| W8 | Union Variant + Zero AArch64 Orphans | Conditional on W7 close unless C1-only plan proves independence | Accepted material differential vs REDRESS 96/97/98; `G-SIMD-GRAMMAR-POLICY`; scalar/checkasm before SIMD | `skinny/crates/runtime/src/tape/`, `skinny/crates/runtime/src/grammars/{json,css_l4_*}/`, `skinny/crates/codegen/src/*templates`, `skinny/crates/bbnf-simd/` | 45 min source-edit cap for SIMD/union; <=650 LOC selected slice; no orphan retained | Not parallel with JSON or CSS runtime waves. May run in its own worktree after W7 but ledger close serialized. |
| W9 | JSON Product Sweep: Direct + Typed Rows | Conditional on W7 and relevant W8 disposition | All direct/typed strict comparators same-run; REDRESS 119 material differential named per direct residual | `skinny/crates/runtime/src/grammars/json/`, `skinny/crates/codegen/src/json_templates/`, `skinny/crates/bbnf-bench/src/{direct_struct,generated_real_typed,json_parity*}`, `skinny/crates/bbnf-simd/` consumers | Packed subwaves W9.1-W9.2; <=650 LOC per lane; <=75 min redress each | Source lanes mostly serialized due generated JSON overlap; measurement lanes may parallelize; ledger single-writer. |
| W10 | JSON `parse_only` Sweep | Conditional on W7 and W8 disposition | `parse_only` comparator re-pinned; no diagnostic-only classification remains; parse rows measured strict-vs-strict | JSON runtime/codegen/parse-that/SIMD consumer paths; `bbnf-bench` parse lanes; gate/report | Packed subwaves W10.1-W10.17 by corpus family; <=75 min per lane; split if row family needs source divergence | Measurement can parallelize by corpus; source edits serialize unless isolated in generated row modules. |
| W11 | Rolling Delta, Close, and Alpha Bracket | Conditional on W0-W10 dispositions | Every row/feature has ADMIT or architectural-block evidence; G-Omega closed; no silent demotion | `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, `restart/skinny/tranches/sk-v13/{SYNTHESIS,HANDOFF,SPEC,DISPATCH-PROMPT}` only if authorized later | 0 behavior LOC; close docs/gates <=75 min | Not parallel. Final ledger/doc reconciler only. |

### §2.2 Explicit Packed Subwaves

Packed subwaves are planning labels inside the top-level wave. They are not
permission to exceed the hard cap. If a subwave needs a separate research/plan/
redress triumvirate, it counts as a real wave for the 12-wave ceiling.

| Parent | Subwave | Scope | Primary rows/features |
|---|---|---|---|
| W1 | W1.1 | Stylesheet root, rule list, selector list, pseudo-classes/elements, attribute selectors | `css_l4/stylesheet_and_selectors/direct_to_struct/main` |
| W1 | W1.2 | Generated per-grammar dispatch, whitespace/comment, string, number, direct sink, and sparse flag policy consumed by W1.1 | Same W1.1 row plus SK-V12 declaration-values guard |
| W2 | W2.1 | Declarations, declaration blocks, custom properties, `var()`, `url()`, `calc()/min()/max()/clamp()` | `css_l4/declaration_values_extended/direct_to_struct/main` |
| W2 | W2.2 | Color functions and typed color normalization | `css_l4/color_functions/direct_to_struct/main` |
| W2 | W2.3 | Gradients, transforms, filters, easing functions | `css_l4/visual_functions/direct_to_struct/main` |
| W2 | W2.4 | At-rule dispatch, media queries, keyframes, supports/import/font-face where not OUT_OF_SCOPE | `css_l4/at_rules_and_media/direct_to_struct/main` |
| W2 | W2.5 | Nesting plus vendor/custom at-rule taxonomy; source/comment/whitespace only if required for parity, otherwise diagnostic/out-of-scope | `css_l4/nested_rules_and_queries/direct_to_struct/main`, `css_l4/vendor_and_custom_atrules/direct_to_struct/main` |
| W2 | W2.6 | `a64_ascii_set_run_skip` production split into CSS scanner, if a CSS scan-block row consumes it | CSS delimiter/layout row plus `css_l4/declaration_values/direct_to_struct/main` guard |
| W9 | W9.1 | All 17 `direct_to_struct` rows, with the 13 REDRESS-119 residuals named and the prior A/GO rows guarded | `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode` |
| W9 | W9.2 | All 17 `real_typed_struct` rows; ten missing generated typed product surfaces must be created or architecturally blocked | Same 17 corpora; missing surfaces: `canada`, `random`, `gsoc-2018`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode` |
| W10 | W10.1-W10.17 | One `parse_only` admission/architectural-block lane per corpus | Same 17 corpora, strict sonic-rs `parse_only` anchor |

### §2.3 Dependency Edges

1. `W0 -> all`: no behavior wave starts without `SK-V13-open` telemetry,
   gate-json/schema acceptance, and rolling-delta baseline.
2. `G-Omega -> W0`: W0 source/report/results work remains blocked until
   totality V1.1 is ratified.
3. `W1 -> W2`: full CSS parity depends on stylesheet/selectors and generated
   policy/sink foundations.
4. `W1 -> W3/W7`: resolver facts may consume generated per-grammar policy, but
   decision waves must not encode CSS/JSON branches in generic crates.
5. `W3 -> W4 -> W5 -> W6 -> W7`: the decision-engine fold is serialized:
   regex facts, e-graph language/rewrites, cost extraction, CSP assignment,
   cascade deletion/fail-closed retirement.
6. `W7 -> W9/W10`: JSON residual sweeps get fresh material differential from
   resolver selection or record that the resolver cannot create a row route.
7. `W7 -> W8` by default. A C1 codegen-private union attempt may request
   earlier dispatch only if it proves no dependency on e-graph/CSP and still
   carries REDRESS 96/97/98 material differential.
8. `W8 -> W9/W10` when a JSON/parse row claims union or SIMD benefit. If W8
   rejects or architecturally blocks union, W9/W10 continue under non-union
   row routes but cannot claim G3 admission.
9. `W0-W10 -> W11`: close/bracket reads all dispositions and rolling delta.

### §2.4 Bracket-Ceiling Consequence

The full addendum's literal expansion is at least:

- 23 remaining CSS feature families if each feature is a wave;
- 13 direct residual JSON waves under the W11.{1..13} convention;
- up to 17 `parse_only` waves under the W14 convention;
- 10 missing typed product rows if typed surfaces are split by corpus;
- 5 decision-engine fold waves;
- plus W0, union/zero-orphan, rolling-delta, and close.

That literal plan is over 60 triumvirate-sized dispatches and is not an SK-V13
12-wave bracket. The realistic SK-V13 plan is therefore a packed, row-family
sequence with hard caps and immediate bracket-forward semantics. If W1/W2 CSS,
W9 direct/typed, or W10 parse cannot pack without violating caps or owner-path
isolation, W11 must close SK-V13 as `REJECT-BRACKET` with a complete rolling
delta, then Pass Alpha opens SK-V14 carrying the remaining explicit subwaves.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

All thresholds are instantiated from W0 same-run telemetry unless an existing
admitted CSS row supplies a stricter fixed authority. JSON admission threshold
is `Track 1 Mbps > sonic-rs strict Mbps + 1` on the same corpus, same output
plane, and strict equality. CSS admission threshold is `Track 1 Mbps >
lightningcss strict Mbps + 1` on the same output plane with cssparser/golden
oracle agreement. Maintain threshold is no silent demotion: every previously
admitted row must stay at or above its W0 margin, or the wave records
architectural-block/user re-pin evidence.

| Wave | Must-improve / must-admit rows | Maintain rows | Threshold binding |
|---|---|---|---|
| W0 | No behavior row. Baseline captures all 51 JSON rows plus every CSS feature row in the rolling table. | Current JSON rows, the SK-V12 CSS declaration-values row, zero-orphan inventory state. | `SK-V13-open` run id emitted; all required schema/rolling fields present; missing field fails closed. |
| W1 | `css_l4/stylesheet_and_selectors/direct_to_struct/main`; selector, pseudo-class, pseudo-element, attribute selector coverage. | `css_l4/declaration_values/direct_to_struct/main` at Track 1 429.34420791225705 Mbps vs lightningcss 168.92962215656692 Mbps unless W0 refresh supersedes both. | Strict equality vs lightningcss and cssparser/golden; Track 1 > lightningcss + 1; feature accept/reject matrix matches lightningcss. |
| W2 | CSS feature rows W2.1-W2.6: declaration expansion, vars/calc/url, colors, visual functions, at-rules/media/keyframes, nesting/vendor/custom, and optional CSS scan-block SIMD row. | W1 CSS row and SK-V12 declaration-values row; JSON full-table guard if shared codegen/SIMD changes. | Same CSS strict threshold; SIMD subwave also needs scalar reference, `BBNF_SIMD_STRICT=1` checkasm, and same-wave CSS consumer profile. |
| W3 | At least one generated dispatch/fact consumer that proves `bbnf-regex` facts are used, not support-only. | All W0 JSON/CSS guards. | Consumer row must preserve equality and improve or unblock a named CSS/JSON row; otherwise W3 rejects extraction as support-only. |
| W4 | Resolver sample row selected by e-graph with bounded rewrite set. | W3 consumer, all W0 JSON/CSS guards. | E-graph saturation bounded; no OOM; selected row maintains equality and records candidate cost facts. |
| W5 | Active cost selection changes or validates one generated backend choice with measured row movement or architectural-block evidence. | W4 sample row, all W0 guards. | Stale-cost evidence <=30% of candidate expressions; deterministic ranking; row threshold same as JSON/CSS domain. |
| W6 | CSP assignment on at least one multi-objective grammar sample. | W5 ranking row and W0 guards. | Solve time <=1 s per grammar, target <=200 ms; UNSAT routes to REDRESS; no fused solver hidden coupling. |
| W7 | Old P1-P8 cascade deleted or fail-closed; JSON/CSS/Sheets/BBNF-self samples do not silently fall back. | W3-W6 rows and full W0 guard table. | Generated output accepts resolver-selected paths only; old cascade cannot produce admission evidence. |
| W8 | One union row admits or records architectural-block evidence: preferred rows are `css_l4/stylesheet_and_selectors/direct_to_struct/main`, `json/{canada,mesh,numbers}/parse_only/main`, or a direct projection row selected by W7. | Full JSON/CSS guard table; zero aarch64 orphans. | Same-row strict threshold; no `UnionTape`, sidecar vector, parser-owned cursor/list, aux density table, or retained class sidecar; every SIMD primitive wired or deleted/demoted. |
| W9 | All 17 `direct_to_struct` and all 17 `real_typed_struct` rows. | W1/W2 CSS rows; W8 union/zero-orphan disposition; prior JSON admits. | Each product row Track 1 > sonic-rs strict + 1 or architectural-block; Track 2/oracle structurally independent; REDRESS-119 cited for direct residuals. |
| W10 | All 17 `parse_only` rows: `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode`. | W9 product rows, W1/W2 CSS rows, W8 zero-orphan state. | Each parse row Track 1 > sonic-rs strict `parse_only` + 1; no diagnostic-only outcome; strict equality in measured row. |
| W11 | No new behavior row. Rolling table covers every JSON row/plane and every CSS feature with `T1_current`, `T1_sota`, `margin`, `tranche_admitted`. | Entire bracket. | Full ADMIT or architectural-block for G1-G6 and no demotion for G7; otherwise close is REJECT and Alpha brackets SK-V14. |

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

Global pre-blocks for every wave:

- REDRESS 28 and 33: no naive NEON tiny-string / Class A replay.
- REDRESS 50-55: no parse-time aux side tables, parser-local cursors, decoded
  string stats sinks, quote-source streaming hashers, or sidecar event vectors.
- REDRESS 60-72: no direct materialization/source-hook/string fact family
  replay without a new output-contract material differential.
- REDRESS 80: no one-row Canada mantissa/number patch.
- REDRESS 82-84: no single-quartet unicode helper, generated-retained
  StringBlock16 tiny probe, or object-pair value-byte compaction replay.
- REDRESS 88-90: PMULL/CTZ/bulk body fills remain rejected except behind a
  same-wave material-differential consumer; B6 canary hardening is history.
- REDRESS 96-98: no class-column retained vector, streaming cursor, class-lane
  paper-close, parser-owned structural projection, or sidecar union substrate.
- REDRESS 119/120: prior direct fixpoint and SK-V11 close are history only;
  every JSON row is reopen-eligible but needs fresh material differential.
- REDRESS 121-127: W1a/W2/W1b/W4/W5 SK-V12 records are authority, not shortcuts;
  CSS declaration-values is one admitted row, W4 run-skip is a production split
  not an admission, and zero-orphan close must be revalidated if new SIMD lands.

Wave-specific pre-block emphasis:

| Wave | Additional pre-block emphasis |
|---|---|
| W0 | No behavior/source edits before G-Omega; no stale run id or missing schema field. |
| W1-W2 | CSS row-production scopes are not primitive admissions; no generic-crate CSS/JSON branches; no support-only `ByteSetRunSkip64`. |
| W3-W7 | No support-only resolver scaffolding; no fused e-graph/CSP solver; no silent fallback to P1-P8 after W7. |
| W8 | No `UnionTape`, public substrate API, parser-owned cursor/list, retained class sidecar, or orphan SIMD primitive. |
| W9-W10 | No REDRESS-119 history close; no lossy/permissive/different-plane SOTA anchor; no `parse_only` diagnostic concession. |
| W11 | No implementation-limited miss counted as close; bracket forward if any row/feature lacks ADMIT or architectural-block evidence. |

### Ledger Serialization Discipline

Parallel worktrees may compile, test, benchmark, and emit per-wave artifacts,
but they must not append `skinny/RESULTS.md`, `skinny/REDRESS.md`, or
`restart/skinny/ROLLING-SOTA-DELTA.md` concurrently. Each wave has one ledger
finalizer. The finalizer orders entries by wave/subwave id, refreshes
`RESULTS.md` from gate-consumed reports, appends one REDRESS admit/reject/block
entry per wave decision, updates the rolling delta, runs the gate check, and
then lets downstream worktrees rebase or replay onto the new ledger state.
Failed lanes save rejected patches under `/tmp/` as the triumvirate contract
requires; they do not leave partial ledger edits.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
