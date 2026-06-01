# CH5 HIDDEN-COUPLING — SK-V18 T-P2 V1

Pass: SK-V18 Totality T-P2 (totality research) hardening.
Cycle: V1.
Lens: CH5 HIDDEN-COUPLING. No grounded design may imply a parallel substrate /
sidecar producer / Lock-1 violation; the FSM/CollapsedStage research must keep
the mask stream transient; Layer-0/Layer-1 must stay a clean one-directional
dependency.
Target packet: the six SK-V18 dossiers `2A`-`2F` under
`restart/audit/totality/p2/` (generated 2026-06-01 18:34-18:37).

Disposition: REVISE.

Tally basis: 16 enumerated CH5-scope items. accept=10, revise=5, reject=1.
REVISE rate = 5/16 = 31.25% (meets the cycle-V1 ≥30% expectation).

## Spot-Verification Ledger (load-bearing citations under the lens)

Every load-bearing CH5 citation was independently checked; all verified.

| citation | dossier | check | result |
|---|---|---|---|
| `find_css_significant` two-fan OR-reduce, `runtime_simd.rs:169`-`216`, `mask_a \| mask_b`, `trailing_zeros` | 2B/2E/2F | read source | VERIFIED — set_a[8]+set_b two-fan, transient per-call mask, scalar tail |
| `count_top_level_commas` LIVE consumer of `bracket_depth_mask_64`+eq-set, i32 depth carry "threads across blocks within this one call" | 2F PTG-2F-10/11 | read `runtime_simd.rs:25`-`55` | VERIFIED — depth carry is per-call transient, doc-stated; no retained stream |
| five-shape `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`, `select_lowering(cost.chosen)` | 2C/2D | read `lower/mod.rs:18`-`24` | VERIFIED exactly five; dispatch on `cost.chosen` |
| `CollapsedStage` delegates to `tape_plan::render_rule(.., Collapsed)`, no marker | 2D | read `collapsed_stage.rs:16` | VERIFIED — delegates, not a `format!` marker |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork | 2D | read `grammar_provider.rs:39`-`43` | VERIFIED — the anomalous second discriminator exists |
| `NormalizeDirectSinkCost` rewrite live, `BackoffScheduler`, `Extractor` | 2D | read `backend_egraph.rs:40`-`90` | VERIFIED — one asserted rewrite under `enable_rewrites` |
| `runtime_target_rows_collapsed` PLANNED-not-live (==0) | 2C/2D | `rg` skinny/crates skinny/xtask | VERIFIED — ZERO hits, co-gate not yet live |
| 9-ident `ManifestStrategyEntry` table in generic `ir` crate | 2C | read `strategy.rs:137`-`185` | VERIFIED — 9 grammar-named idents rows in a neutral data table |
| self-gate falsified: 4-name leak regex asserts ZERO, returns 13 | 2C | `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir crates/analysis` | VERIFIED — 13 sites |
| upstream `parse-that` `scan_balanced`, `<=8` nibble-LUT cap | 2F | read `.../scan/balanced.rs:26`,`:44` | VERIFIED — exists, `<=8` debug_assert |
| `checkasm_parity.rs:3` "Modelled on FFmpeg's tests/checkasm/checkasm.h" | 2A | read `checkasm_parity.rs:1`-`6` | VERIFIED verbatim; `classify_reference`/`scan_scalar` at `:129` |
| `css_types.rs` in GENERIC `crates/core/`, not `crates/<grammar>/` | 2C | `ls`+head | VERIFIED — generic-crate host shim leak |
| `EventGrammar` phantom axis, ZERO non-test production animator | 2C | `rg EventGrammar` runtime | VERIFIED — `G: EventGrammar = AnyGrammar` type bound, no per-grammar producer |
| Lemire 2026-04-19 "fastest way to match characters on ARM" (SVE2 match vs NEON eq-fan) | 2E | WebFetch | VERIFIED REAL — title/date exact; SVE2 `match` "might be fastest"; NEON `vceqq_u8` in comments |
| Validark "Use interleaved vectors for parsing on ARM" 2024-09-03 | 2E/2B | WebFetch | VERIFIED REAL — Niles Salter; ld4/movemask/element-shift |
| Mison VLDB 2017 (consumer-known projection, structural index) | 2D | WebSearch | VERIFIED REAL — Li et al., MSR; DOI 10.14778/3115404.3115416 |
| iburg LOPLAS 1992 DOI 10.1145/151640.151642 (dispatch-on-selected-pattern) | 2D | WebSearch | VERIFIED REAL — DOI exact; tree-pattern+DP description matches R-A grounding |

No confabulated or unverifiable citation was found under this lens. The dossiers
are unusually well-grounded on the verifiable surface; the REVISE findings below
are about under-discharged hidden-coupling seams, not fabrication.

## Enumerated CH5 Items

### ACCEPT (10)

- **CH5-V1-A01 (2A T2A-V1-SOTA-JSON-001 / refuted-#5): simdjson stage-1 grounded as TRANSIENT projection, retained class/cursor streams explicitly refuted.** `transfer_reason=transient structural projection ... consumed by one DOM/tape builder`; refuted-assertion-5 rejects "retained parallel class/cursor streams." Matches the published stage-1→stage-2 architecture; matches the lens fence exactly. ACCEPT.
- **CH5-V1-A02 (2A LAC-04): Lock-1 substrate-union fence is correct and explicit.** "Retained cursor/list/class-column/sidecar routes are REDRESS-refuted ... only allowed shape is transient same-loop masks consumed into the existing substrate, or generated single-substrate consumption; any retained sidecar-like route requires a new Alpha/P1/SPEC contract." Correctly pre-blocks before any SIMD parser primitive wave. ACCEPT.
- **CH5-V1-A03 (2B A1 / SKV18-A4): Layer-0/Layer-1 one-directional dependency grounded.** `bbnf.asm` includes Layer-0 vendored macros one way (`:47`-`48`), per-grammar data kept outside the macro library (`:55`-`60`); x86 Layer-0 is diagnostic-only and a P1 deletion target. The one-way dependency is preserved and the close-route correction (Layer-0 is x86-only diagnostic, cannot close M5 Max rows) is honest. ACCEPT.
- **CH5-V1-A04 (2B SKV18-A1, 2F PTG-2F-10): eq-set inner sub-kernel is genuinely neutral; mask is transient.** Caller-supplied byte set (`set:&[u8]`), kernel names no grammar; LIVE-consumed by both JSON `find_ascii_set_member64` and CSS `count_top_level_commas`. The 64-bit mask is a per-block transient consumed in-loop. No retained sidecar. ACCEPT.
- **CH5-V1-A05 (2F PTG-2F-11 + verified `count_top_level_commas`): the FSM/balanced mask stream is TRANSIENT — the central lens fence.** `bracket_depth_mask_64` takes an i32 depth carry in, returns mask + next depth; the carry "threads across blocks within this one call." The bracket-depth bitmap is computed and consumed inside one call, never written to a retained side array. This is the exact "keep the mask stream transient" property the lens demands. ACCEPT.
- **CH5-V1-A06 (2C SK-V15-2C-CSS-GENERATOR-SIDECAR refuted): the `CSS_GENERATED_RS` generator-sidecar is refuted, not admitted.** Hand-written CSS parser body in a string literal correctly classified as a generator-sidecar and blocked until a same-wave generated typed provider replaces it; fact streams "cannot serve as a retained EventTape." Correct hidden-coupling refutation. ACCEPT.
- **CH5-V1-A07 (2C SK-V18-2C-TOTALITY-TREE-9-IDENT-LEAK): the relocated-seam analog is correctly named a leak, with the self-gate falsification proven.** The 9-ident table in the generic `ir` crate is a per-grammar identity riding a neutral data column the arm-census grep cannot see — the canonical CH5 hidden-coupling shape. The self-gate (asserts ZERO, returns 13) is verified RED on disk. Correctly refuted; routed to SK-V19 R16 structural row-collapse. ACCEPT.
- **CH5-V1-A08 (2D R-A / `emit_shape_source==lowered_program`): the un-fork firewall correctly identifies the relocated-seam as the CH5 risk.** The render body must read shape ONLY from `program.policy_summary.backend_shape`, never `target.profile/emitter/output_labels/profile_contract`. md5-distinctness declared NECESSARY-NOT-SUFFICIENT; the structural `runtime_target_rows_collapsed` co-gate is the only check that catches a fork relocated into data. The `RuntimeEmitterKind` fork (verified) is named as the anomalous second discriminator to DELETE. Architecturally correct and on-disk-grounded. ACCEPT.
- **CH5-V1-A09 (2D five-shape canon, no sixth): no public substrate/BIR/shape expansion.** Verified exactly five `BackendShape` variants; 2D's no-candidates axis explicitly forbids a sixth. CollapsedStage stays an aarch64-gated diagnostic, not a new public substrate. ACCEPT.
- **CH5-V1-A10 (2E refuted-svmatch / two-fan grounding): no parallel SVE2 substrate; the deployable plane is the single transient eq-fan.** SVE2 `match` host-absent (FEAT_SVE2 absent on M5 Max), so no scalable-vector dispatch family is opened; the two-fan OR-reduce is one transient mask, not two retained streams. Lemire-2026 verified real and correctly read (SVE2 fastest but host-absent → NEON eq-fan is the deployable route). ACCEPT.

### REVISE (5)

- **CH5-V1-R01 (2F LAC-2F-V3-01 / PTG-2F-09): the two-SIMD-substrate parallel-substrate risk is correctly NAMED but the lock text under-binds the transient-vs-retained dimension.** Disposition under the lens: the upstream `parse-that` `scan/` substrate and skinny `bbnf-simd` are genuinely two parallel SIMD scan substrates (verified: upstream `scan_balanced` exists with its own `find_first_of_nibble_lut`/`build_nibble_luts` classifier distinct from `byte_class_from_eq_set_64`). 2F recommends the vendor route and binds a SINGLE-SIMD-SUBSTRATE lock — good — but the lock's close test (`rg parse_that:: == 0`) only proves the upstream crate is not a *dependency*; it does NOT prove the vendored `scan_balanced` shell does not introduce a *second mask convention* inside `bbnf-simd` (the upstream uses an 8-byte nibble-LUT `structural` array + `PaddedView`; skinny uses the two-fan eq-set + `bracket_depth_mask_64`). **Correction:** the SINGLE-SIMD-SUBSTRATE lock must additionally assert mask-representation unification — the vendored shell must consume the EXISTING `byte_class_from_eq_set_64`/`bracket_depth_mask_64` kernels and the project's one canonical SHRN movemask (`movemask.rs:5`), with a close test that greps for any new `build_nibble_luts`/`find_first_of_nibble_lut` symbol landing in `bbnf-simd` (`== 0`), not merely the absence of a path-dep. As written, a vendor that copies the upstream nibble-LUT classifier verbatim passes the V3-01 close test while planting a second parallel mask substrate. REVISE 2F.

- **CH5-V1-R02 (2C SK-V18-2C-WITNESS-EMISSION-NEUTRALITY-COUPLING): the `EventGrammar` phantom axis is a latent per-grammar coupling threaded through the PUBLIC tape substrate, and the dossier under-states its CH5 standing.** Verified on disk: `ValueRef<'doc,'input,K,G: EventGrammar = AnyGrammar>` at `tape/mod.rs:175` is a per-`<G>` type axis woven through the public `ValueRef` substrate with ZERO non-test production animator. 2C classifies it `partial` and routes the *literal-token* leak to the P4 `FORBIDDEN_GENERIC_TOKENS` gate — but the lens concern is structural, not lexical: a phantom `G` parameter on a PUBLIC substrate type is exactly a hidden-coupling axis that the P4 token-grep cannot see (the same blindness the relocated-seam firewall exists to cover). **Correction:** 2C must either (a) cite the R-D DELETE-default as the binding disposition (delete the phantom `<G>` axis from the public `ValueRef` before G2/G3, not merely token-scan for `SheetsEventGrammar` literals), or (b) add a structural co-gate (`event_grammar_phantom_axis_animated == false` OR `value_ref_grammar_param_deleted`) analogous to `runtime_target_rows_collapsed`. A token grep is necessary-not-sufficient against a type-level phantom coupling on a public substrate. REVISE 2C.

- **CH5-V1-R03 (2D UNKNOWN-2D-V3-04 / 2D CollapsedStage row): the CollapsedStage staged-FSM lowerer is admitted into the five-shape canon without a discharged transient-mask proof, and the dossier leaves it as an open question rather than a fence.** Verified: `collapsed_stage.rs:16` delegates to `tape_plan::render_rule(.., Collapsed)` whose flavor strings (`fuse_span_stage`, `commit_fused_stage`, `ParserState+CollapsedStagePlan`) describe a STAGED fusion. The lens fence requires the FSM/CollapsedStage research to keep the mask stream transient. 2D's own UNKNOWN-2D-V3-04 asks whether the `Collapsed` flavor "emits a body with no Lock-16 admissibility evidence" and whether it "emits an undocumented hand-tuned loop" — i.e. the transient-mask property of CollapsedStage is *open*, not grounded. Leaving a shape in the admitted canon while its mask-transience is an open question is a CH5 gap. **Correction:** 2D must add an explicit fence: CollapsedStage is admitted to the canon as a SHAPE SLOT only; its lowerer body is `diagnostic-only / author-declared` until a transient-mask proof (the `ParserState+CollapsedStagePlan` carries no retained per-grammar side stream, only a per-call staged FSM state) plus the 2E TBL-classify scalar oracle land. Phrase it as a binding admission gate, not an UNKNOWN. REVISE 2D.

- **CH5-V1-R04 (2C SK-V18-2C-CSS-TYPES-HOST-SHIM-LEAK + SK-V18-2C-CSS-VALUE-API-SURFACE): the CSS typed value/document surface in generic `crates/core/` is a side-channel provider, and the firewall against re-coupling it to the un-forked emitter is under-specified.** Verified: `css_types.rs` is in GENERIC `crates/core/src/` (Lock-14-(c) admits only `crates/<grammar>/`), and the typed `CssDocument`/`CssTypedValue` surface lives at `crates/core/src/runtime/css_l4/` (also generic). 2C correctly refutes `css_types.rs` in-place and marks the CSS typed surface `partial`. But under CH5 the risk is sharper: when G2 builds the grammar-derived CSS typed provider, the un-forked `render(program)` must not reach into the hand-owned `crates/core/src/runtime/css_l4/` surface as a *side channel* (the CSS analog of reading `target.profile`). **Correction:** extend the `emit_shape_source==lowered_program` firewall (currently scoped to `RuntimeTarget` fields in 2D) into 2C's CSS-provider rows: add `css_provider_source==generated` with a close test that the G2 emitter reads zero symbols from the generic `crates/core/src/runtime/css_l4/` hand-owned surface. As written, the firewall fences the `RuntimeTarget` data-column seam but not the CSS-typed-surface side-channel seam. REVISE 2C.

- **CH5-V1-R05 (2B FSM_DISPATCH_THREADED / frame-stack macro-family, A3a row + OQ-2B-05): the frame-stack/FSM macro contracts are correctly refuted, but the only named rebuild route is "a same-wave CollapsedStage consumer," which silently reintroduces the retained-frame-stack coupling the dossier elsewhere blocks.** 2B refutes `FSM_DISPATCH_THREADED`/`FRAME_PUSH_BOUNDED`/`FRAME_POP_BOUNDED` as source-only (`bbnf.asm:317`-`473`, no scalar/checkasm/consumer) — correct. But the rebuild route (`close_status=source-present-unwired; W8/W9 may reopen only with a same-wave CollapsedStage consumer`) names a FRAME STACK (push/pop bounded) as the rebuild vehicle, while the 2B refuted-assertions table separately blocks "retained frame/open stacks" as sidecar substrate expansion (SPEC `:147`-`153`). These two rows are in tension: a `FRAME_PUSH/POP_BOUNDED` macro IS a retained stack. **Correction:** 2B must reconcile — either the FSM/frame-stack rebuild route is DELETE-only (the recursive scalar shell + transient eq-set skip already cover the balanced-nesting need, per SKV18-A2/2F, with NO frame stack), or any CollapsedStage frame-stack rebuild must carry the Lock-1 "fold into existing substrate / transient per-call FSM state, not a retained frame array" proof inline. As written, the rebuild route is a latent sidecar reintroduction. REVISE 2B.

### REJECT (1)

- **CH5-V1-X01 (2C SK-V18-2C-CSS-BALANCED-SCAN-NEUTRALITY-PROOF-FORCED-DEMOTION, inner-kernel neutrality leg as stated): the claim that the eq-set INNER sub-kernel is "already proven neutral by JSON's own `find_ascii_set_member64` consumer" is over-stated for the CSS balanced-scan seam and must be narrowed.** The forced-demotion of the SHELL to `css_balanced_component_scan` is correct and well-grounded (ACCEPT-adjacent). The defect is the specific neutrality discharge for the inner kernel *as the CSS balanced scan uses it*. Verified on disk: the CSS live consumer of `byte_class_from_eq_set_64` is `count_top_level_commas` (a top-level-comma counter), and the JSON consumer is `find_ascii_set_member64` — but the SK-V18 G6 retarget needs the inner kernel inside the *two-fan OR-reduce over a ≤13-byte significant set* (`find_css_significant`), which is a DIFFERENT invocation shape than either live consumer (single ≤8-byte set). The dossier's neutrality proof ("JSON's `find_ascii_set_member64` consumer proves the inner kernel neutral") does not cover the two-fan ≤13-byte composition the CSS retarget actually instantiates: the two-fan *composition* (set-split into `set_a`/`set_b`, OR-reduce) is exercised ONLY by the dead CSS-shaped `find_css_significant`, with zero live non-CSS caller. So the *composition* inherits the same single-grammar-exercise problem as the shell. **Falsifying evidence:** `rg` shows `find_css_significant` (the two-fan composition) has only `#[cfg(test)]` callers (`lib.rs:574`), and no JSON/Sheets call site invokes the two-fan ≤13-byte OR-reduce. The neutrality of the *base eq-set primitive* (≤8-byte, caller data) is genuine (that earns ACCEPT-A04); but the dossier conflates "base eq-set kernel is neutral" with "the two-fan ≤13-byte composition is neutral," and the latter is exercised by CSS only. As written this is a confabulated neutrality discharge for the composition. REJECT the inner-composition-neutrality claim; the dossier must split the row: base eq-set kernel = neutral (caller data); two-fan ≤13-byte OR-reduce composition = CSS-exercised-only, subject to the SAME neutrality-proof obligation as the shell (forced-demote or prove a non-CSS two-fan caller). REJECT 2C (this leg only).

## Non-Regression Guards Held (CH5 V1)

- No retained sidecar admitted in any dossier; all sidecar/parallel-substrate routes are refuted or flagged (2A LAC-04, 2B retained-frame refutation, 2F SINGLE-SIMD-SUBSTRATE lock).
- The FSM/CollapsedStage and balanced-scan mask streams are transient per-call (verified `count_top_level_commas` i32 depth carry; `find_css_significant` per-block mask) — the central lens fence holds, modulo R03's open CollapsedStage-lowerer proof.
- Layer-0/Layer-1 stays one-directional (2B A1, verified `bbnf.asm` one-way include).
- No new BIR variant, no sixth `BackendShape`, no public substrate expansion (verified exactly five shapes).
- No broadcast-admission laundering (carried from V2; 2A/2C reject the 24-row CSS broadcast).
- The relocated-seam (`RuntimeTarget` data column) and the 9-ident generic-crate leak are NAMED as hidden coupling and routed to structural co-gates, not admitted.

## Fold Requirements for V2

1. 2F LAC-2F-V3-01: add mask-representation unification to the SINGLE-SIMD-SUBSTRATE lock; close test greps for new `build_nibble_luts`/`find_first_of_nibble_lut` symbols in `bbnf-simd == 0`, not only path-dep absence (R01).
2. 2C: bind the `EventGrammar` phantom-axis as a structural co-gate or R-D DELETE-default on the public `ValueRef` substrate, not only a P4 token grep (R02).
3. 2D: convert UNKNOWN-2D-V3-04 into a binding CollapsedStage admission fence (diagnostic-only/author-declared until transient-mask + scalar-oracle proof) (R03).
4. 2C: extend the `emit_shape_source==lowered_program` firewall to CSS-provider rows (`css_provider_source==generated`; emitter reads zero hand-owned `crates/core/src/runtime/css_l4/` symbols) (R04).
5. 2B: reconcile the FSM/frame-stack rebuild route against the retained-frame-stack refutation — DELETE-only or transient-per-call-FSM-state proof inline (R05).
6. 2C: split the eq-set neutrality row — base ≤8-byte kernel neutral; two-fan ≤13-byte OR-reduce composition CSS-exercised-only, subject to the neutrality-proof obligation (X01).

## Convergence Impact

REVISE blocks CH5 V1 clean convergence. The dossiers are well-grounded and
contain no confabulated citation; the findings are under-discharged
hidden-coupling seams (two parallel mask substrates, a public phantom-grammar
axis, an open CollapsedStage transient-mask proof, a CSS-provider side channel, a
latent frame-stack reintroduction) plus one over-stated inner-kernel neutrality
discharge. Fold the six items into V2; the lens re-arms on any V2 fold that
admits a retained sidecar, a second SIMD mask substrate, a phantom-grammar public
axis, a non-transient FSM mask stream, or a Layer-0/Layer-1 back-edge.

TALLY accept=10 revise=5 reject=1
