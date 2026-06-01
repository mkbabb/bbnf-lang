---
agent: 1D
pass: T-P1-excavation
cycle: V5-SKV18-totality
cycle_self_label: SK-V18-totality
generated_at: 2026-06-01T00:00:00Z
scope: 1D — skinny-track lessons digest (SK-V1..SK-V18); SK-V18 GENERALIZATION lens
spec_surfaces_audited:
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
  - restart/skinny/tranches/sk-v18/SPEC.md
  - restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md
  - restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
  - skinny/REDRESS.md
  - skinny/RESULTS.md
files_audited_count: 18
live_truth_method: "wc -l preflight on REDRESS/RESULTS/ARCHITECTURE/MASTER-PLAN/LOCKS/SPEC; rg + sed on each cited path:line in skinny/crates/ and crates/core/; md5 on 7 css_l4 generated.rs to re-prove replica identity; find -type f for x86 + Pattern-H census; grep for RuntimeEmitterKind/CSS_GENERATED_RS/parse_w11_1_number/phantom-<G>/find_component_delim live witnesses at HEAD"
divergence_count:
  spec_claims_implemented: 1
  spec_claims_unimplemented: 10
  impl_exceeds_spec: 3
  unknown: 5
locks_amendment_candidates: 0
---

## Executive Summary

SK-V18 is the INFLECTION cycle. The durable skinny lesson the V1 totality spec
must absorb is the inversion of SK-V13: the >SOTA is now MEASUREMENT-VALID for the
JSON half (JSON 51/51 strict cold beats sonic-rs — `RESULTS.md:5-25` bench-row-backed);
the CSS half (CSS canonical cold beats lightningcss 1.9–3.3×) is newly measurement-valid
(NOT fake) but DIRECTIONAL, NOT yet re-locked (loadavg 4.35; H1 `css_canon_bench`
re-lock gate per U-4 — split per CH6-V3-F1 so the headline does not carry the
un-caveated closure word its own body retired). The implementation that produced it
is hand-written, FORKED, and
REPLICATED under a "grammar-driven" banner — the generator does not exist
(`SYNTHESIS-AUDIT-OVERFIT.md:36-39,141`). The empirical floor proved across
SK-V1..V14 (single tape/`ValueRef` substrate, aarch64-only admission,
scalar→checkasm→same-wave-consumer SIMD discipline, FNV-as-non-equality-arbiter
(the production `emit_full_parse input_fnv64` is live telemetry, NOT bench-only — see G-5
split per CH5-V3-005), no retained sidecar) HOLDS and is grammar-neutral. What is DISPROVED is the
emission story: 7 css_l4 `generated.rs` are byte-identical (md5 `b654562c…`,
re-verified live), `RuntimeEmitterKind` forks on grammar family, JSON renders 7×
`push_str` fixed literals, the phantom `<G>` is test-only, x86 lingers crate-wide,
and the Lock-14 gate is green-by-exclusion. The SK-V18 finding the totality spec
must reflect: a naive grammar-walk regresses to lightningcss's architecture
(94.1% `find_component_delim` scan), so >SOTA survives ONLY via named,
`.bbnf`-invoked, grammar-derived, (a)-(d)-gated primitives — never a paper-close.

## Spec-Claim ↔ Implementation Table

The "spec claim" column is the SK-V18 GENERALIZATION goalset (the V1 greater-spec
surface the totality cycle absorbs): `restart/skinny/tranches/sk-v18/SPEC.md`
§0.1 close conditions 1–12 + the binding architecture surfaces (`ARCHITECTURE.md`
5-shape `BackendShape`, `MASTER-PLAN.md` one-generator goal). Verdicts:
implemented / unimplemented / impl_exceeds_spec / unknown.

| spec claim (path:line) | impl evidence (path:line) | verdict | note |
|---|---|---|---|
| ONE generator emits JSON+CSS+Sheets from `.bbnf`, 3 NON-IDENTICAL grammar-DERIVED parsers (`SPEC.md:63-69`) | `skinny/crates/codegen/src/runtime_generator.rs:16` forks `match request.profile_contract.emitter`; CSS arm is `normalize(CSS_GENERATED_RS)` const at `skinny/crates/codegen/src/runtime_generator.rs:91,701`; JSON arm is `json_sink_direct.rs` 7× `push_str` (root-relative paths per CH1-V4-F10) | unimplemented | the generator DOES NOT EXIST; both paths are grammar-specialized couriers (`SYNTHESIS-AUDIT-OVERFIT.md:141`, A4). This is the SK-V18 inflection: round-trip passes over hand-written content. |
| One un-forked emitter, dispatched on LOWERED `BackendShape`, not grammar tag (`SPEC.md:71-78`) | `skinny/crates/codegen/src/grammar_provider.rs:33` `pub emitter: RuntimeEmitterKind` (root-relative per CH1-V4-F10); `:40` `enum RuntimeEmitterKind{CompiledLowering,RequestFacts}`; `:110` gates on `!= RuntimeEmitterKind::RequestFacts` | unimplemented | R3, addendum-3 (single-emitter-path). The fork is a live grammar-family discriminator; G3 deletes it. |
| 7 byte-identical css_l4 replicas collapse to ONE; R16 row-collapse (`SPEC.md:80-85`) | 7× `css_l4_*/generated.rs` all md5 `b654562ccff46ed62dd48e9ace325830` (re-verified live); `find_component_delim` at offset 657 in every replica | unimplemented | R4, addendum-2. md5 identity re-proven this pass across all 7 modules; the per-`RuntimeTarget` row-collapse (`skinny/xtask/src/regen.rs:5` derives `Clone,Copy,Debug` only, over `pub(crate) struct RuntimeTarget` at `:6`) is the R16 +1-line `PartialEq` target, not yet landed. |
| Grammar-NEUTRAL 5-shape `BackendShape{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` over `select_lowering` (`ARCHITECTURE.md` §7.3; `SPEC.md:74-76`) | `lower/mod.rs:18` `fn select_lowering(cost:&CostFacts)->&'static dyn ShapeLowering`; `:20-24` match arms over exactly the 5 shapes | impl_exceeds_spec | the 5-shape canon is real, grammar-neutral, and Lock-14-clean; SK-V15 W8/W9 admitted all five lowerers as operation-plan renderers (`REDRESS.md:6356-6414`). This is the seam G3 dispatches on — present at greater fidelity than the spec asserts. |
| Shared value-API trait, JSON+CSS+Sheets instantiate ONE `Cursor`/`DocumentView` seam (`SPEC.md:87-97`) | `tape/mod.rs:227` `pub trait DocumentView<'a>` exists (latent); JSON `ValueRef<K>` real; CSS returns `Result<String,CssFactError>` fact-stream, no value API | unimplemented | R6 LCD-flatten hazard; the CSS value API is absent (carried from SK-V15 REBUILD-WAVE-E). G4 must extend the seam WITHOUT flattening JSON rich nav. |
| Phantom `<G>` resolved by DELETE (`SPEC.md:99-102`) | `tape/mod.rs:175` `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>`; `:179` `_grammar:PhantomData<fn()->G>`; `:197` `_grammar:PhantomData` | unimplemented | R5, addendum-4. The `<G>` axis is test-only (`_proof_compiles` census excl. `_tests.rs` is EMPTY per `SYNTHESIS-RESEARCH.md:26-30`); G4 DELETEs it, preserving REAL `K=Kind` axis. |
| >SOTA preserved HONESTLY — JSON beats sonic-rs strict, CSS beats lightningcss (`SPEC.md:104-128`) | JSON: `RESULTS.md:5-25` cold Track 1 > sonic-rs strict per row (twitter 8349>4913, citm 9079>8335, canada 16709>12970), per-iter equality PASS — bench-row-backed. CSS: SK-V18 headline canonical cold 1.9–3.3× is a synthesis-doc ASSERTION (`SYNTHESIS-AUDIT-OVERFIT.md:36-37`), NOT a bench-row table | impl_exceeds_spec (JSON) / directional, not re-locked (CSS) | SPLIT per CH6-F2. JSON: 51/51 strict-vs-sonic cold admitted, same-plane, per-iter equality — legitimately exceeds spec. CSS: >SOTA is MEASUREMENT-VALID (NOT fake like SK-V13) but the 1.9–3.3× ratios (bootstrap 2.190, tailwind 3.375, material 1.658, animate 2.101, `SPEC.md:113-118`) ran under loadavg 4.35 and are DIRECTIONAL, NOT re-locked; the H1 `css_canon_bench` re-lock gate (U-4 `1D:199-203,228-232`) is the falsifier. The CSS half rides hand-written content; preservation through the generator is the open burden. Do NOT carry the un-caveated "MEASUREMENT-VALID" closure word on the CSS half. |
| x86 gone, aarch64-only (`SPEC.md:130-133`) | `skinny/crates/bbnf-simd/src/x86_64/` (24 files incl. `byte_class_from_eq_set_64.{rs,asm}`) + `ext/x86/` vendored ASM live at HEAD | unimplemented | R8, A6. Both surfaces present; P1 deletes crate-wide (≈−4500 LOC). aarch64-only admission discipline is PROVED policy (SK-V15 G-2) but the x86 tree is not yet excised. |
| Lock-14 gate MEANINGFUL, no green-by-exclusion (`SPEC.md:135-141`) | `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2409` `GENERIC_SCAN_ROOTS`; `:2442` `SKV15_W2_EXTRA_COVERAGE_ROOTS` holds `runtime_generator.rs`,`grammar_provider.rs`,`json_sink_direct.rs`,`json_typed_direct.rs`,`json_templates`; `:2463` `diagnostic-x86` exclusion live | unimplemented | R9, A3. The gate reads CLEAN only because the leak surfaces sit in the WEAK extra-coverage roots, not strict `GENERIC_SCAN_ROOTS`; P4 promotes them BEFORE G2/G3. (Bare `lock14_baseline.rs` prefixed to `skinny/crates/bbnf-bench/src/` per CH1-V2-F4 — the file is NOT under `codegen/`.) |
| Sheets proves generalization REAL — pratt precedence tower via generator ONLY (`SPEC.md:143-152`) | `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf:36-51` 7-level left-assoc tower (comparison→concat→add→mul→exp→unary→postfix); `:67` cyclic `paren_expr="(",expression,")"` (the canonical root copy `grammar/google-sheets/google-sheets.bbnf` carries the SAME tower at `:103-121` and paren_expr at `:137`); `sheets_witness/event_grammar_witness.rs` exists as DEFINITION only | unimplemented | R-E PROVE make-or-break. The precedence tower is the SOLE Sheets-distinctive construct JSON+CSS structurally lack; it stresses G3 generality. Witness is consumed only by `_tests.rs` `_proof_compiles` (defined, never animated). |
| NEON reaches hot path AT ADMISSION (`SPEC.md:154-165`) | `find_component_delim` SCALAR in every css_l4 `generated.rs:657`; `byte_class_from_eq_set_64`/`find_ascii_set_member64` kernel EXISTS in `bbnf-simd/src/{aarch64,scalar,x86_64}`; `find_css_significant` lives in `runtime/src/runtime_simd.rs`+`lib.rs` (caller-side), dead at admission | unimplemented | R7, addendum-6. The 94.1% CSS hot leaf is scalar; the eq-set kernel is checkasm-gated but not retargeted onto the recursive shell. G6 wires it; G5 retires zero-sampled `json/scan.rs`. |
| Generated-state cleanliness — no metalang leak, regen --check exit 0 (`SPEC.md:167-169`) | `skinny/crates/runtime/src/grammars/json/generated.rs:801,841,881` (root-relative per CH1-V4-F10) call `parse_w11_1_number_direct/_object_direct/_array_direct` (7 occurrences) | unimplemented | R15, A1/regen. Bench-wave-id `w11_1` leaked into shipped runtime symbol names; P5 renames `parse_w11_1_number_*`→`parse_number_*` at source. |
| Tape ∪ direct-to-struct UNION substrate; `ValueRef` is a cursor into the tape (Lock 1; `MASTER-PLAN` substrate goal) | `tape/mod.rs:175` `ValueRef` is `&Tape + cursor`; SK-V15 P2-D confirmed single `Tape` owns source/offsets/sparse-flags/payload-arena; REDRESS records direct view as typed projection over sealed offsets (`REDRESS.md:126-132`) | impl_exceeds_spec | the substrate is the genuine CLEAN foundation S-P0 KEEPs (`SYNTHESIS-AUDIT-OVERFIT.md:109`). Lock 1 holds; no second substrate, no retained sidecar. |
| Decision Engine load-bearing (e-graph cost / CSP / 5 lowerers drive selection) (`MASTER-PLAN` Lock 10/Lock 5) | SK-V15 W7 admitted decision spine (`REDRESS.md:6326-6354`); W8/W9 admitted all-five lowerers as operation-plan renderers (`REDRESS.md:6356-6414`) | implemented | the SK-V15 cycle moved this from scaffold (prior-cycle G-5 disproved) to load-bearing; the e-graph records `egraph_rewrite_count`, CSP capacity is falsifiable. Carried forward as PROVED. |

## Divergences Catalogued

Severity inherits `SYNTHESIS-AUDIT-OVERFIT.md §2` (HIGH = falsifies a campaign
claim; MEDIUM = honesty/discipline residual). Every row re-verified LIVE this
pass at the cited path:line.

- **D-1 (HIGH, unimplemented) — the generator does not exist (A4).** Spec claims
  ONE grammar-driven generator (`SPEC.md:63`); disk shows two forked couriers
  (`skinny/crates/codegen/src/runtime_generator.rs:16,91,701`; `json_sink_direct.rs` 7× push_str; root-relative per CH1-V4-F10). The
  round-trip (`delete + regen ⇒ byte-equivalent`) passes BUT over hand-written
  content — the SK-V18 headline divergence. Disposition: G1+G2+G3.
- **D-2 (HIGH, unimplemented) — 7 byte-identical css_l4 replicas.** All 7
  `css_l4_*/generated.rs` share md5 `b654562c…` (re-verified). The "9-grammar
  census" the prior totality cycle cited counts 7 css_l4 + json + sheets_witness
  in the skinny runtime tree; the totality `crates/core/` tree mirrors this with
  one css_l4 dir but per-grammar hand-written runtime modules (7–10 files each).
  P3 collapses to ONE CSS config. loc_delta ≈ −5460 (G-13 P3, `1D` G-13 below;
  source `SYNTHESIS-AUDIT-OVERFIT.md:153`).
- **D-3 (HIGH, unimplemented) — RuntimeEmitterKind grammar-family fork** at
  `skinny/crates/codegen/src/grammar_provider.rs:33,40,110` (root-relative per CH1-V4-F10). The discriminator IS the relocated-seam risk:
  un-forking the visible enum while leaving a per-grammar branch in a neutral
  data table is caught ONLY by the structural `runtime_target_rows_collapsed`
  co-gate (R16), never by arm-grep (`SYNTHESIS-RESEARCH.md:272-279`).
- **D-4 (HIGH, unimplemented) — x86 two surfaces live** in `bbnf-simd/src/x86_64/`
  + `ext/x86/`. Wrong-arch under an aarch64-only admission platform (G-2 PROVED).
  P1 deletes ≈−4500 LOC (loc_delta = G-13 P1, `SYNTHESIS-AUDIT-OVERFIT.md:153`;
  disk: 28 files / 4401 LOC).
- **D-5 (HIGH, unimplemented) — phantom `<G>` test-only** at `tape/mod.rs:175,179,197`.
  Decorative generic; instantiate-or-DELETE (DELETE default, preserve `K` axis).
- **D-6 (HIGH, unimplemented) — CSS NEON dead at admission.** `find_component_delim`
  is scalar in all 7 replicas (`:657`); the eq-set kernel exists checkasm-gated but
  is not retargeted onto the recursive shell. The 94.1% G6=WIRE profile finding.
- **D-7 (MEDIUM, unimplemented) — Lock-14 green-by-exclusion** (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2442`
  `SKV15_W2_EXTRA_COVERAGE_ROOTS` + `:2463` `diagnostic-x86`; bare path prefixed
  per CH1-V2-F4 — home is `bbnf-bench`, not `codegen`). The gate is not
  meaningful until P4 promotes the leak surfaces into strict scan roots.
- **D-8 (MEDIUM, unimplemented) — metalang leak** `parse_w11_1_number` ×7 in
  `skinny/crates/runtime/src/grammars/json/generated.rs:801,841,881` (root-relative per CH1-V4-F10). Bench-wave-id contaminates shipped symbols. P5.
  loc_delta ≈0 — rename-only (G-13 P5, `SYNTHESIS-AUDIT-OVERFIT.md:153`).
- **D-9 (impl_exceeds_spec) — JSON 51/51 strict cold > sonic-rs**, same-plane,
  per-iter equality (`RESULTS.md:5-25`). The guard baseline EXCEEDS a "documentation
  clarification" — it is the load-bearing proof the substrate + projection CAN beat
  SOTA honestly; the burden is preserving it THROUGH the generator (G1 91.5% leaf).
- **D-10 (impl_exceeds_spec — at admitted scope; selection DEPTH pending) — the
  5-shape `BackendShape` + decision spine are load-bearing**, not scaffold. SK-V15
  W7/W8/W9 converted the prior-cycle scaffold finding into operation-plan renderers
  (`REDRESS.md:6326-6414`). This is the seam G3 dispatches on — the architecture the
  spec claims already exists. CAVEAT (CH3-V1-004 reconcile with 1E-L10): the
  decision-engine SELECTION DEPTH under the Sheets R-E precedence tower is the open
  L10 stressor (`1E-locks-evidence.md:90` — the L10 row; re-anchored from `:89` (the L09 row) per CH1-V4-F9), un-tested until G3 exists.
- **D-11 (impl_exceeds_spec) — the unified Tape/ValueRef/PayloadArena substrate is
  CLEAN** (`SYNTHESIS-AUDIT-OVERFIT.md:109`). Lock 1 holds at greater fidelity than
  the spec asserts; it is the genuine foundation S-P0 explicitly KEEPs.

## Gaps / Missing Primitives

- **The grammar-derived CSS balanced scan (PRIMARY §6 finding).** The 94.1% hot
  leaf (`find_component_delim`+`consume_balanced_at`) has a delimiter alphabet
  (`{}:;`) and structural-byte dispatch (`' " / ( [ {`) EMERGENT from rule shapes,
  modeled by NO `SinkOnlyExpr` node (`SYNTHESIS-RESEARCH.md:222-237`). Missing:
  the `css_balanced_component_scan` named primitive — grammar-INVOKED, taking
  grammar-DERIVED byte-set args, (a)-(d)-gated, doubling as the G6 NEON call site.
  NEUTRALITY obligation: a non-CSS invoker (JSON `{}/[]` or Sheets `paren_expr`)
  must exercise the SAME shell, else demote to `css_`-scoped name.
- **JSON string/number leaf scanners (SECONDARY §6 finding).** The 91.5% leaf's
  micro-opts (`b'-'|b'0'..=b'9'` fast-path, `match_tiny_plain_string_direct`) must
  survive the AST-walk byte-exact as named primitives invoked by `.bbnf` `string`/
  `number` rules, each with the (b) byte-set-mutation falsifier (`SYNTHESIS-RESEARCH.md:238-248`).
- **The CSS Value API** is absent (carried from SK-V15 REBUILD-WAVE-E): CSS returns
  `Result<String,CssFactError>`, JSON has `JsonValue`+visitor. G4 must build a lazy
  `CssNode` over the existing tape WITHOUT a second substrate or eager tree.
- **The Sheets precedence-tower lowering** is unrealized — the 7-level right-iterated
  EBNF (`A=B(op B)*`) lowering to `Seq`+`RepeatLoop`+`Alt{Dispatch}`+`CallRule` needs
  NO new IR primitive but stresses G3's generality; if G3 cannot render recursive
  `CallRule`/`RepeatLoop` chains from grammar structure, the tower breaks first.
- **The R16 row-collapse mechanism** — `RuntimeTarget` derives only `Clone,Copy,Debug`
  (`skinny/xtask/src/regen.rs:5`, over `pub(crate) struct RuntimeTarget` at `:6`); the
  +1-line `PartialEq` full-row derive that recurses into both
  `frontend_requirements` (#11) and `output_labels` (#12) is not yet present.

## Rejected-Route Pre-Block (REDRESS items the SK-V18 G6/G2/G4 moves abut)

The three highest-regression SK-V18 moves each ADJOIN a route already REJECTED in
`skinny/REDRESS.md`; this pre-block enumerates the specific items by id+line and
states the FALSIFYING distinction (admissible = retarget/decorate the existing
in-loop single-substrate leaf; rejected = add a second scanner / structural-stream
driver / bespoke per-grammar mask / parser-local cursor). Routing this list at
path:line keeps a downstream wave from re-implementing a measured-and-reverted
shape (CH3-V1-002).

| REDRESS item | line | rejected shape | SK-V18 wave it bounds | admissible vs rejected distinction |
|---|---|---|---|---|
| Item 246 — W11T parse-only structural stream | `skinny/REDRESS.md:6184-6219` (REJECT) | a structural-stream parse_only DRIVER (second substrate over a retained structural stream) | G4 lazy `Cursor`/`CssNode` | ADMISSIBLE: G4's `Cursor` is a VIEW over the EXISTING `Tape`/`ValueRef`/`PayloadArena` (`SPEC.md:87-97`). REJECTED: a structural-stream driver = a second substrate. |
| Item 247 — W11V parse-only string64 mask | `skinny/REDRESS.md:6230-6260` (REJECT) | a bespoke per-grammar 64-byte string-special mask | G2 `css_balanced_component_scan` | ADMISSIBLE: G2 is a SHARED grammar-neutral primitive over a grammar-DERIVED byte set. REJECTED: a bespoke per-grammar 64-byte mask re-emitted per grammar. |
| Item 51 — SK-V5 event-cursor (`JsonEventCursor`) | `skinny/REDRESS.md:742-768` (REJECT) | a parser-local transient event-cursor wrapper in the retained parser | G6 NEON retarget (EventCursor-adjacent) | ADMISSIBLE: the EventTape lowering consumes the single substrate's event stream IN-LOOP. REJECTED: a retained/parser-local second cursor (cf. 1A-SUB-012, 1B EventCursor rows). (Span narrowed `742-783`→`742-768` per CH3-V2-005 — item 51 prose ends at `:767`; `:768` is the blank separator before item 52 (`:769`), so the block span `742-768` includes that separator (off-by-one corrected per CH3-V3-004); `:769-783` is item 52 "SK-V5 baseline reassay", a profiling re-measurement, NOT a rejected route. Reconciles with `1B:55` CH3-V2-004: 1B widened the 51∪53 PAIR span to cover both rejects; this row narrows the item-51 SUB-span to exclude item 52 — both land 51=`742-768`, 53=`784-813` (CH3-V3-006).) |
| Item 53 — SK-V5 structural-mask parser-local cursor (`JsonStructuralCursor`) | `skinny/REDRESS.md:784-813` (REJECT) | a second retained-parser cursor over a per-64-byte emit mask | G6 NEON retarget onto the scalar shell | ADMISSIBLE: G6 RETARGETS NEON onto the EXISTING in-loop `find_component_delim` shell as a shared primitive the generated scan CALLS (`SPEC.md:154-165`). REJECTED: a parser-local second scanner over a retained mask. |

Note: the SK-V18 SPEC itself does NOT cite these items (`rg 'W11T|W11V|structural.stream|event.cursor' SPEC.md` = 0), so this pre-block discharges the burden the dispatch chain otherwise leaves unmet. COMPLETENESS CAVEAT (per CH3-V4-006): coverage is scoped to the committed ledger (which ends at SK-V15 W11, `skinny/REDRESS.md:6446`); SK-V16/V17 rejected routes are NOT yet captured in `skinny/REDRESS.md` (both tranches exist on disk — `restart/skinny/tranches/sk-v16/{HANDOFF,SPEC,SYNTHESIS}.md`, `…/sk-v17/…`) — see U-5. The pre-block is complete for the captured ledger, NOT for the full skinny history; any SK-V16/V17 rejected route is structurally invisible to this four-item table.

## Proved / Disproved / Pending Digest

Separated per Lock 14: JSON/CSS-empirical (grammar-specific) vs grammar-NEUTRAL
(generalizes across the 9 totality grammars). The SK-V18 lens column flags the
inflection finding each row feeds.

### JSON / CSS-Empirical Findings

| id | status | lesson V1 must reflect | citations |
|---|---|---|---|
| J-1 | PROVED | JSON has a validated 51-row guard baseline (parse_only/direct/typed) beating sonic-rs strict cold, same-plane, per-iter equality. | `RESULTS.md:5-25`; `SYNTHESIS-AUDIT-OVERFIT.md:113,139` |
| J-2 | PROVED | W11W parse_only memchr trusted-string split + W11A direct strict-product are the accepted JSON close routes. | `REDRESS.md:6262-6294`; `REDRESS.md:5861-5881` |
| J-3 | DIRECTIONAL / not-re-locked (newly measurement-valid, NOT yet bench-re-locked) | CSS canonical cold N≥200 real-corpus beats lightningcss 1.9–3.3× — NOT a fake admit (unlike SK-V13); the residual is hand-written content, not a fabricated number. INLINE CAVEAT (CH6-F3, per U-4 `1D:199-203`): the S-P1 absolute ratios (bootstrap 2.190, tailwind 3.375, material 1.658, animate 2.101, `SPEC.md:113-118`) ran under loadavg 4.35 and are DIRECTIONAL, NOT re-locked. The `SYNTHESIS-AUDIT-OVERFIT.md:36` cite is the synthesis doc ASSERTING the ratio, not a bench-row table (contrast J-1's `RESULTS.md:5-25` measured rows); a quiet re-capture on `css_canon_bench` is the H1 re-lock gate. Grade CSS >SOTA `directional`, not `PROVED`, until that row is cited. | `SYNTHESIS-AUDIT-OVERFIT.md:36-37,138`; directional caveat `SPEC.md:113-118`; re-lock gate U-4 `1D:199-203` |
| C-1 | DISPROVED | CSS live `generated.rs` is NOT grammar-derived — `CSS_GENERATED_RS` const courier copied into 7 byte-identical replicas (md5 `b654562c…`). | `skinny/crates/codegen/src/runtime_generator.rs:701,91` (root-relative per CH1-V4-F10); 7× live md5; `SYNTHESIS-AUDIT-OVERFIT.md:50,88` |
| C-2 | DISPROVED | The CSS Value API is absent; CSS returns `Result<String,CssFactError>` fact-stream, not a typed value/document/visitor surface. | `tape/mod.rs:227` (latent DocumentView only); `SYNTHESIS-AUDIT-OVERFIT.md:90` (R6) |
| C-3 | PENDING | CSS >SOTA must survive re-emission via the generator without regressing the 94.1% scan to a tree-walk (lightningcss's own architecture). | `SYNTHESIS-RESEARCH.md:222-237`; `SPEC.md:104-128` |
| C-4 | PENDING | The metalang `parse_w11_1_number` leak must be purged at source so shipped JSON runtime carries no bench-wave id. | `skinny/crates/runtime/src/grammars/json/generated.rs:801,841,881` (root-relative per CH1-V4-F10); `SYNTHESIS-AUDIT-OVERFIT.md:99` (R15) |

### Grammar-Neutral Findings

| id | status | lesson V1 must reflect | citations |
|---|---|---|---|
| G-1 | PROVED | Structural projection and tape are ONE substrate; `ValueRef` is a cursor into the tape (Lock 1 holds, CLEAN). | `tape/mod.rs:175`; `REDRESS.md:126-132`; `SYNTHESIS-AUDIT-OVERFIT.md:109` |
| G-2 | PROVED | aarch64 / Apple M5 Max is the SOLE admission platform; x86/AVX-512 is diagnostic only — the prune target. | `SYNTHESIS-RESEARCH.md:9`; `SPEC.md:130-133`; x86 surfaces live in `bbnf-simd` |
| G-3 | PROVED (at admitted scope; selection DEPTH pending) | The 5-shape `BackendShape` + decision spine are LOAD-BEARING (SK-V15 W7/W8/W9 admitted operation-plan renderers + e-graph rewrite count + falsifiable CSP). RECONCILE with 1E-L10 (CH3-V1-004): the W7/W8/W9 lowerers are admitted-as-operation-plan-renderers at the admitted scope, BUT the decision-engine SELECTION DEPTH under the Sheets R-E precedence tower remains the open L10 stressor (`1E-locks-evidence.md:90` — the L10 row; re-anchored from `:89` (L09) per CH1-V4-F9; `SYNTHESIS-RESEARCH.md:249-255`) — not "fully proved load-bearing" without that caveat. | `lower/mod.rs:18-24`; `REDRESS.md:6326-6414`; depth caveat `1E-locks-evidence.md:90` |
| G-4 | PROVED | SIMD/ASM admission discipline = scalar oracle → strict checkasm → same-wave consumer (no orphan kernel); the eq-set kernel is already checkasm-gated. | `SYNTHESIS-RESEARCH.md:43`; `SYNTHESIS-AUDIT-OVERFIT.md:70,113` |
| G-5 | PROVED (split per CH5-V3-005) | FNV / closed-enum products are NEVER a runtime equality arbiter (SK-V15 W10 quarantine). TWO surfaces: (a) the BENCH-side FNV quarantine (`fnv_quarantine.rs`) is clean/KEEP — genuinely bench-only; (b) the PRODUCTION `emit_full_parse` `input_fnv64`/`fnv64` is LIVE telemetry-output on the Track-1 recognition path (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:393,:394,:899`; template `runtime_generator.rs:1093,:1599`), CALLED by the production parser (`parser.rs:42 generated::emit_full_parse`) and the MEASURED Track-1 plane (`skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:131 fn track1_full`; bench dispatch `:306`/`:345`) — it is non-equality / non-substrate / non-document-identity telemetry, NOT "bench-quarantined". (Harness cite repaired per CH3-V4-007 / CH6-V4-005: added the dropped `/bin/` segment; `fn track1_full` is at `:131` — `:130` is the `// ---- track1_full` comment.) | `REDRESS.md:6416-6444`; `SYNTHESIS-AUDIT-OVERFIT.md:113`; production path:line above |
| G-6 | DISPROVED | The single grammar-agnostic emitter does NOT exist — `RuntimeEmitterKind` forks on grammar family; the generator is two couriers + 7 replicas. | `skinny/crates/codegen/src/grammar_provider.rs:40,110`; `skinny/crates/codegen/src/runtime_generator.rs:16` (root-relative per CH1-V4-F10); `SYNTHESIS-AUDIT-OVERFIT.md:141` |
| G-7 | DISPROVED | The Lock-14 gate is green-by-EXCLUSION — leak surfaces sit in weak extra-coverage roots + `diagnostic-x86` exclusion. | `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2442,2463` (bare path prefixed per CH1-V2-F4); `SYNTHESIS-AUDIT-OVERFIT.md:93,140` |
| G-8 | DISPROVED | The phantom `<G:EventGrammar>` axis is decorative — zero non-test production consumers; witnesses defined, never animated. | `tape/mod.rs:175,179`; `SYNTHESIS-RESEARCH.md:26-30` |
| G-9 | PROVED (inflection; CSS ratio DIRECTIONAL not re-locked) | We are AT the inflection point: >SOTA is real, the impl is hand-written/forked/replicated; a naive grammar-walk regresses to lightningcss — >SOTA survives ONLY via (a)-(d)-gated named primitives, never a paper-close. INLINE CAVEAT (CH6-F3): the JSON half is bench-row-backed (`RESULTS.md:5-25`); the CSS 1.9–3.3× half is DIRECTIONAL (loadavg 4.35, NOT re-locked, U-4 `1D:199-203`) — the inflection thesis stands, but the CSS ratio is not a re-locked bench row. | `SYNTHESIS-AUDIT-OVERFIT.md:23,36-39`; `SYNTHESIS-RESEARCH.md:215-266`; CSS directional caveat U-4 `1D:199-203` |
| G-10 | PROVED (profile) | The G6=WIRE decision RULE is the grammar-NEUTRAL lesson: a hot leaf with a measured profile share above the WIRE threshold warrants NEON retarget over honest-retire. The supporting 94.1%/79.5% figures are CSS-EMPIRICAL, not fleet-neutral (re-scoped per CH2-V3-009): `find_component_delim` is the 94.1% CSS hot leaf / 79.5% of the CSS path, and the leaf has ZERO non-CSS caller on disk (`rg find_component_delim skinny/crates/runtime/src \| grep -v css` = empty). Keep the decision-rule as the neutral lesson; the ratio is a CSS profile measurement. | `SYNTHESIS-RESEARCH.md:43,222`; `SPEC.md:154-165`; CSS-only leaf census (`grep -v css` = empty) |
| G-11 | PENDING | The §6 named-primitive escape is the single largest paper-close surface — admissible ONLY under (a) grammar-INVOKED-by-name ∧ (b) output VARIES under invoking-rule mutation ∧ (c) `verbatim_blob_present==false` ∧ (d) PROFILE-PROVEN-NARROW-LEAF. | `SYNTHESIS-RESEARCH.md:257-266`; `SYNTHESIS-AUDIT-OVERFIT.md:103,247-251` |
| G-12 | PENDING | The relocated-seam risk: un-forking the visible enum while leaving a per-grammar branch in a neutral data table is caught ONLY by the structural row-collapse co-gate (R16 `PartialEq` full-row), never by arm-grep. | `SYNTHESIS-RESEARCH.md:272-279`; `SYNTHESIS-AUDIT-OVERFIT.md:59-63,100` |
| G-13 | PROVED (prune) | The prune list deletes net ≈−10800 LOC: P1 x86 (−4500), P2 warm bench (−700), P3 7 css_l4 replicas (−5460), P4 gate-fix, P5 metalang. PRUNE-before-GENERALIZE is the standing order. | `SYNTHESIS-AUDIT-OVERFIT.md:153-179`; `SPEC.md:551-770` |

## Open Questions (UNKNOWN → verify_action)

- **U-1 — totality `crates/core/` Pattern-H carry.** The skinny runtime tree has 40
  hand-written per-grammar files (7 css_l4 + json + sheets_witness); the totality tree
  `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf,csv,ebnf,bnf,math,css_pretty}`
  carries 7–10 hand-written `.rs` per grammar (≈70 files; the hand-written Pattern-H
  carry surface is **67 files / 6867 LOC** — `find crates/core/src/runtime -mindepth 2
  -name '*.rs' -not -path '*tape*'`). The 169956-LOC figure is the GENERATED
  `crates/core/src/grammar/generated/*.rs` recognizer plane (a DIFFERENT tree, ~25× the
  hand-written carry — `wc -l grammar/generated/*.rs = 169956`), detached here from the
  runtime-file clause per CH4-V2-010; the 9×-scale carry-cost is the ≈6867-LOC hand-written
  surface, not the generated plane. Whether SK-V19's totality fold inherits the SAME forked-emitter problem at
  9× scale is UNKNOWN. **verify_action:** at SK-V19 entry, census `crates/core/src/runtime`
  for line-1 `@generated` provenance + md5-distinctness across the 9 grammars, mirroring
  the skinny P3 falsifier.
- **U-2 — Sheets precedence-tower emits via the generator ONLY.** The grammar
  (`crates/gorgeous/grammar/google-sheets/google-sheets.bbnf:36-67`; canonical root
  copy `grammar/google-sheets/google-sheets.bbnf:103-137`) is structurally distinct
  (7-level tower + cyclic `paren_expr`); whether G3 can render recursive
  `CallRule`/`RepeatLoop` chains from
  grammar structure is the PROVE make-or-break and is UNKNOWN until G3 exists.
  **verify_action:** at PROVE, assert Sheets `generated.rs` md5-distinct from JSON∧CSS +
  `sheets_grammar_shape==pratt-operator` + no `const.*_RS.*r#` blob.
- **U-3 — the named-primitive neutrality proof.** Whether `css_balanced_component_scan`'s
  recognizer SHELL gets a genuine non-CSS invoker (JSON `{}/[]` or Sheets `paren_expr`) in
  THIS campaign, or is honestly demoted to `css_`-scoped, is UNKNOWN.
  **verify_action:** at G2, grep the `.bbnf`-invocation census for a non-CSS caller of the
  shell; absent one, require the `css_` rename.
- **U-4 — load-depressed absolute Mbps re-lock.** The S-P1 CSS absolute ratios (bootstrap
  2.190, tailwind 3.375, material 1.658, animate 2.101) ran under loadavg 4.35 and are
  DIRECTIONAL, NOT re-locked (`SPEC.md:113-118`). Whether a quiet re-capture preserves the
  crossing is UNKNOWN. **verify_action:** at H1, re-measure on the P2-survivor `css_canon_bench`
  in a quiet plane; require ≥1 regular corpus (animate OR bootstrap) crossing >1.0× same-run.
- **U-5 — REDRESS.md ledger coverage of SK-V16/V17.** The committed ledger ends at SK-V15 W11
  (`REDRESS.md:6446-6465`); SK-V16/V17 redress is not in this file (their lessons reach this
  digest only via the SK-V18 synthesis docs). Whether SK-V16/V17 carry an uncaptured
  empirical reversal is UNKNOWN. **verify_action:** locate the SK-V16/V17 tranche REDRESS/HANDOFF
  and reconcile against the SK-V18 S-P0 residual census before Pass Omega ratification.

## LOCKS-Amendment Candidates

NONE (0). 1D surfaces zero LOCKS-amendment candidate this pass. Every SK-V18 divergence
is dispositioned by an existing SK-V18 wave (P1–P5 / G1–G6 / PROVE / H1) under the
already-ratified 16 locks; the §6 named-primitive escape and the R16 row-collapse recipe
are gate-mechanism precision pins (T-P3 disposition), not lock amendments. The 1E agent
holds amendment-candidate authority; 1D defers.
