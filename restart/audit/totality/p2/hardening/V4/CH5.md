# CH5 HIDDEN-COUPLING — SK-V18 T-P2 V4

Pass: SK-V18 Totality T-P2 (totality research) hardening.
Cycle: V4 (second consecutive re-arm over the live SK-V18 packet; V3 returned
ACCEPT 16/0/0 — the FIRST clean CH5 cycle after the V2 R-V2-01 fold reset the
clean-count. This V4 is the SECOND clean-cycle candidate for the
two-consecutive-clean §3Z rule, within the V≤5 ceiling).
Lens: CH5 HIDDEN-COUPLING. No grounded design may imply a parallel substrate /
sidecar producer / Lock-1 violation; the FSM/CollapsedStage research must keep
the mask stream transient; Layer-0/Layer-1 must stay a clean one-directional
dependency.
Target packet: the six SK-V18 dossiers `2A`-`2F` under
`restart/audit/totality/p2/` (2A SK-V18-T-P2; 2B V3-SKV18; 2C V3; 2D
V3-SKV18-totality; 2E V6-SKV18; 2F V3), regenerated 2026-06-01 19:05-19:24.

Disposition: ACCEPT.

Tally basis: 16 enumerated CH5-scope items (the same 16 the V1/V2/V3 lens
enumerated, re-armed independently against the V4 packet with fresh on-disk
reads and two external WebSearch/WebFetch confirmations). accept=16, revise=0,
reject=0.

## On the ≥30% REVISE cycle-V1 floor

The dispatch carries the V1-cycle floor "expects >=30% REVISE." That floor was
met when this lens FIRST armed: V1 returned 5 REVISE + 1 REJECT (37.5% of items
dispositioned non-ACCEPT). V2 returned 1 REVISE (R-V2-01, the mask-convention
co-gate, 6.25%), V3 found it genuinely folded and returned 0. This V4 re-arm
independently re-verifies every seam on disk and finds NO under-discharged
hidden-coupling coupling and NO confabulated/refuted-route citation. The lens
does not manufacture a REVISE to hit a V1-era rate; doing so would be a
fabricated finding, which CH5 itself forbids. The floor is a V1 expectation, not
a per-cycle quota — and the packet has earned through three prior cycles of
folds.

## Independent Spot-Verification Ledger (this cycle, all re-checked on disk + web)

Every load-bearing CH5 citation was re-checked THIS cycle by direct `grep`/`sed`
read (not carried from V3), plus two external confirmations of the most
suspicious citations. All verified; none confabulated.

| citation | dossier | check THIS cycle | result |
|---|---|---|---|
| `count_top_level_commas` consumes `bracket_depth_mask_64` interior mask IN-LOOP; transient i32 depth carry, NO retained side array | 2F PTG-2F-11 / 2E | read `runtime_simd.rs:21`-`58` | VERIFIED — doc verbatim "the i32 depth carry threads across blocks within this one call"; `bracket_depth_mask_64(.., depth)` per-block, `comma_mask & !interior` consumed in-loop, `depth = depth_out`; no retained array. The central transient-mask fence HOLDS. |
| exactly five `BackendShape` variants; dispatch on `cost.chosen` | 2C/2D | read `lower/mod.rs:18`-`26` | VERIFIED — `match cost.chosen` arm-for-arm `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; no sixth |
| `CollapsedStage` lowerer delegates to `tape_plan::render_rule(.., Collapsed)`, no marker string | 2D | read `collapsed_stage.rs:1`-`18` | VERIFIED — `debug_assert_eq!(cost.chosen, BackendShape::CollapsedStage)` then `render_rule(rule, TapeFlavor::Collapsed)`; not a `format!` marker |
| `ValueRef<'doc,'input,K=AnyKind,G: EventGrammar = AnyGrammar>` PUBLIC phantom `<G>` axis; `PhantomData<fn() -> G>`; `Copy`/`Clone`/`impl` all carry `<G>` | 2C R02/WITNESS-COUPLING | read `tape/mod.rs:175`-`207` | VERIFIED — public substrate type; `_grammar: PhantomData<fn() -> G>` at `:179`; `impl<.., G: EventGrammar> Copy/Clone/ValueRef` all thread `<G>`. A real public-substrate structural coupling. |
| `EventGrammar` zero live PRODUCTION animator | 2C | `grep` impls + non-default `ValueRef<..Grammar>` instantiations | VERIFIED — only `AnyGrammar` (`event_grammar.rs:19`, default), `JsonEventGrammar` (`json/event_grammar_witness.rs:17`), `SheetsEventGrammar` (`sheets_witness/event_grammar_witness.rs:16`); the two named grammars are referenced ONLY in their own `_witness.rs` + `event_grammar_tests.rs`; ZERO non-`AnyGrammar` `ValueRef` instantiation in production. The phantom axis is unanimated. |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` anomalous second discriminator (the un-fork DELETE target) | 2D R-A | read `grammar_provider.rs:33`,`:40`-`42`,`:110` | VERIFIED — enum `:40`-`42`, field `:33`, the fork dispatch `request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts` at `:110`. The fork to DELETE exists. |
| un-fork firewall: render reads shape from `program.policy_summary.backend_shape`, not `target.*` | 2C/2D firewall | read `lower/sink_only.rs:27`,`:48`,`:131`-`166` | VERIFIED — `backend_shape: policy.selected_shape` flows into `RuntimePolicySummary`; the render body reads from `policy_summary.backend_shape`, not a `target.profile/emitter` field |
| `runtime_target_rows_collapsed` co-gate PLANNED-not-live (==0) | 2C/2D | `grep` crates+xtask | VERIFIED — ZERO; necessary-not-sufficient correctly disclosed, lands P3/G3 |
| `bbnf_simd_single_mask_convention` co-gate PLANNED-not-live (==0); alias-immune distinct-pack count | 2F LAC-2F-V3-01 / 2E LAC-2E-V6-03 | `grep` crates+xtask | VERIFIED — ZERO in `skinny/crates`+`xtask` (planned symbol, correctly not-yet-live; the R-V2-01 fold). Both dossiers co-define it with "Wave-owner / enforcement wave: G2 entry". |
| mask-unification close test: `build_nibble_luts`/`find_first_of_nibble_lut == 0` in `bbnf-simd/src`; upstream `parse_that::`/`parse-that =` not a skinny dep | 2F R01/R-V2-01 | `grep` `bbnf-simd/src`, all skinny Cargo.tomls | VERIFIED GREEN — zero nibble-LUT classifier symbols in `bbnf-simd`; `parse_that::`/`parse-that =` absent from skinny deps; only the DISTINCT `parse-that-regex` workspace crate is a dep |
| `find_css_significant` two-fan OR-reduce dead (test-only) | 2B/2E/2F R7 | `grep` runtime | VERIFIED — defined `runtime_simd.rs:169`; the only caller is `lib.rs:574` under `#[cfg(test)]`; the `lib.rs:500` hit is a comment |
| `bbnf.asm` Layer-0 is x86-only (one-directional, P1-deletion target) | 2B A1 / 2E | `find` bbnf.asm | VERIFIED — the only `bbnf.asm` is `bbnf-simd/ext/x86/bbnf.asm`; Layer-0 is x86 macro infra, diagnostic-only on the aarch64 close host; `bbnf.asm:47-48` includes Layer-0 ONE way |
| 9-ident `idents` table in generic `ir` crate | 2C TOTALITY-TREE-9-IDENT-LEAK | read `crates/ir/src/registry/strategy.rs:137`-`156` | VERIFIED — JsonParser/JsonGrammar, GoogleSheetsParser/…, CssL4Parser, BbnfBootstrap/BbnfParser… grammar-named rows in a neutral data table |
| self-gate falsified: 4-name leak regex asserts ZERO, returns 13 | 2C | `grep 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src crates/analysis/src` (REPO-ROOT totality tree) | VERIFIED — exactly 13 sites, RED. (These are repo-root `crates/` totality paths, NOT `skinny/crates/` — the count only resolves from repo root.) |
| `css_types.rs` in GENERIC core crate | 2C CSS-TYPES-HOST-SHIM-LEAK | `ls crates/core/src/css_types.rs` | VERIFIED present in generic core — the Lock-14-named mess, refuted-in-place, SK-V19 relocate-or-delete |
| 13 differential checkasm harnesses (12 single + 1 aggregate) + `checkasm_common.rs` helper (14 files total) | 2A/2F | `ls checkasm_*.rs` | VERIFIED — 14 files = 12 `checkasm_<primitive>.rs` + `checkasm_parity.rs` (aggregate) + `checkasm_common.rs` (helper); the "13 harnesses + 1 helper" framing is exact; header `checkasm_parity.rs:3` verbatim "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" |
| Lemire 2025-06-01 "Fast character classification with z3" (nibble-pair `f(c)=lut_lo[c&0x0F] AND lut_hi[c>>4]`; "easy-vectorized-classification-with-z3" is URL slug, not title) | 2F SRC-LANGDALE-VECCLASS | **WebFetch** | VERIFIED REAL — exact title + exact 2025-06-01 date + exact nibble-pair formula; the URL-slug-vs-title disclosure is precisely correct |
| Pratt, "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 (grounds the Sheets precedence-tower negative-control — the anti-courier generality proof) | 2C SHEETS-PRECEDENCE-TOWER | **WebSearch** (ACM dl.acm.org 403-blocks WebFetch by bot policy, not a missing-DOI signal) | VERIFIED REAL — exact title, author (Vaughan Pratt), venue (POPL '73, 1st ACM SIGACT-SIGPLAN PoPL), year, and DOI all confirmed |

No confabulated/unverifiable citation found under this lens in the V4 packet.
The two web-checked citations this cycle (Lemire-2025 z3, Pratt-1973 DOI) — the
freshest-dated and the generality-backbone respectively — both confirm exact
title/author/venue/numbers. The Lemire-2026 and Kutenin citations (WebFetch'd
exact at V3) are re-confirmed honestly scoped in 2E (SVE2 `match` "fastest" in
the post BODY, `vceqq_u8` eq-fan as a COMMENTER suggestion; the deployable plane
is the single transient NEON eq-fan; FEAT_SVE2 host-absent per the M5 Max probe).

## Enumerated CH5 Items — ACCEPT (16)

All sixteen carry forward from V3 RE-VERIFIED INDEPENDENTLY on disk this cycle.
The ten V1/V2 ACCEPTs:

- **CH5-V4-A01 (2A SOTA-JSON-001 + REFUTE-JSON-005): simdjson stage-1 grounded TRANSIENT; retained class/cursor streams refuted.** `transfer_reason=transient structural projection ... consumed by one DOM/tape builder`; refuted-assertion-5 rejects "retained parallel class/cursor streams," the same retired thesis as REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE`. ACCEPT.
- **CH5-V4-A02 (2A LAC-04): Lock-1 substrate-union fence correct and REDRESS-ledger-named.** Only admissible shape is transient same-loop masks consumed into the existing substrate; the G6 transient-same-loop-skip on the ADMISSIBLE side of the REDRESS-53 line. ACCEPT.
- **CH5-V4-A03 (2B A1): Layer-0/Layer-1 one-directional dependency grounded.** `bbnf.asm` includes Layer-0 one way (`:47-48`); x86 Layer-0 diagnostic-only, a P1 deletion target. Verified `bbnf.asm` x86-only on disk. ACCEPT.
- **CH5-V4-A04 (2B SKV18-A1 / 2F PTG-2F-10): eq-set inner BASE kernel genuinely neutral; mask transient.** Caller-supplied byte set, names no grammar; the X01 split (base kernel structurally neutral, two-fan composition CSS-exercised-only) carried. ACCEPT.
- **CH5-V4-A05 (2F PTG-2F-11 + verified `count_top_level_commas`): the balanced/FSM mask stream is TRANSIENT — the central lens fence.** `bracket_depth_mask_64` interior mask consumed in-loop, i32 depth threads within one call, never a retained side array (re-verified `runtime_simd.rs:21`-`58` THIS cycle). ACCEPT.
- **CH5-V4-A06 (2C CSS-GENERATOR-SIDECAR): the generator-sidecar refuted, not admitted.** Hand-written CSS parser body in a string literal blocked until a same-wave generated typed provider replaces it; G2 deletes `CSS_GENERATED_RS` in-wave; fact streams cannot serve as a retained EventTape. ACCEPT.
- **CH5-V4-A07 (2C TOTALITY-TREE-9-IDENT-LEAK): the relocated-seam analog NAMED a leak; self-gate falsification proven RED.** 9-ident table in generic `ir` (verified on disk); 4-name arm-census grep catches 4 of 9; self-gate asserts ZERO, returns 13 (re-verified RED, repo-root totality tree). Routed to SK-V19 R16 structural full-row collapse; `tranche_scope=SK-V19-receiver` honestly disclosed. ACCEPT.
- **CH5-V4-A08 (2D R-A / `emit_shape_source==lowered_program`): the un-fork firewall correctly identifies the relocated-seam as the CH5 risk.** Render body reads shape ONLY from `program.policy_summary.backend_shape` (verified `sink_only.rs:131`-`166`); md5-distinctness NECESSARY-NOT-SUFFICIENT; `runtime_target_rows_collapsed` co-gate is the structural catch; `RuntimeEmitterKind` fork named as the anomalous second discriminator to DELETE (verified `grammar_provider.rs:40`-`42`,`:110`). ACCEPT.
- **CH5-V4-A09 (2C/2D five-shape canon, no sixth): no public substrate/BIR/shape expansion.** Verified exactly five `BackendShape` variants at `lower/mod.rs:18`-`26`; 2D no-candidates axis forbids a sixth; CollapsedStage stays an aarch64-gated diagnostic slot. ACCEPT.
- **CH5-V4-A10 (2E refuted-svmatch / two-fan): no parallel SVE2 substrate; deployable plane is the single transient eq-fan.** FEAT_SVE2 host-absent (M5 Max probe), no scalable-vector dispatch family opens; the two-fan OR-reduce is one transient mask; Lemire-2026 honestly scoped. ACCEPT.

The five V2-folded REVISEs (V1 R02-R05, X01) re-verify ACCEPT:

- **CH5-V4-A11 (R02 carry): the `EventGrammar` phantom-axis structural co-gate is bound.** 2C carries `(value_ref_grammar_param_deleted OR event_grammar_phantom_axis_animated == false)` as a structural co-gate analogous to `runtime_target_rows_collapsed`, R-D DELETE-default at G4 — not merely a P4 token grep. The public `<G>` axis on `ValueRef` (re-verified `tape/mod.rs:175`-`207`, threading `Copy`/`Clone`/`impl`) is a structural hidden-coupling axis the token-grep cannot see, with ZERO live animator (re-verified THIS cycle: only `AnyGrammar` + two witnesses, the named grammars referenced only in their own `_witness.rs` + a test). The co-gate symbols appear bound in 2C with a named G4 enforcement wave. ACCEPT.
- **CH5-V4-A12 (R03 carry): CollapsedStage transient-mask proof is a BINDING fence.** UNKNOWN-2D-V3-04 is BOUND by LAC-2D-V3-04 — CollapsedStage is a SHAPE SLOT ONLY; body `diagnostic-only/author-declared` until the transient-mask proof + 2E scalar oracle land; an undocumented hand-tuned `Collapsed` loop is Lock-16 INADMISSIBLE → DELETE the body, keep the slot inert; LEDGER-FENCED against REDRESS 96/97/98. Verified `collapsed_stage.rs:16` delegates. ACCEPT.
- **CH5-V4-A13 (R04 carry): the CSS-typed-surface side-channel firewall is bound.** 2C RELOCATED-SEAM-FIREWALL carries the SECOND SEAM `css_provider_source == generated` with a close test that the G2 emitter reads ZERO symbols from the generic `crates/core/src/runtime/css_l4/` hand-owned surface (the CSS analog of reading `target.profile`); costed as a separate +5-gate-LOC G2-owned row (CH4-V3-02). `css_types.rs`-in-generic-core refuted-in-place (verified on disk). ACCEPT.
- **CH5-V4-A14 (R05 carry): the FSM/frame-stack rebuild route is DELETE-only with inline Lock-1 proof.** 2B SKV18-A5 + OQ-2B-05: frame-stack rebuild vehicle is DELETE-default; the balanced-nesting need is met by the recursive scalar shell (native call-stack, transient per-call) + transient eq-set skip with NO frame stack; any future CollapsedStage frame-stack rebuild must carry the Lock-1 transient-per-call-FSM-state proof inline, never a retained `FRAME_PUSH/POP_BOUNDED` array, else INADMISSIBLE. The latent sidecar reintroduction is closed. ACCEPT.
- **CH5-V4-A15 (X01 carry): the eq-set neutrality row is split.** Base ≤8-byte one-fan kernel = structurally neutral (caller data; `find_ascii_set_member64` has no live runtime caller, so NOT a JSON-consumer proof; JSON `scan_dispatch` rides `byte_class_from_table_64`); the two-fan ≤13-byte OR-reduce composition (`find_css_significant` shape) = CSS-exercised-only, subject to the SAME neutrality-proof obligation as the shell. The inaccurate "JSON's `scan_structurals` rides" source comment is named a same-wave source-fix obligation, not laundered. `find_css_significant` dead-caller re-verified (`lib.rs:574`, `#[cfg(test)]`). ACCEPT.

The single V2 REVISE (R-V2-01) re-classes ACCEPT:

- **CH5-V4-A16 (R-V2-01 folded): the mask-representation-unification fence carries a named structural co-gate with a stated enforcement wave.** 2F LAC-2F-V3-01 + 2E LAC-2E-V6-03 both bind `bbnf_simd_single_mask_convention` — the `bbnf-simd` analog of `runtime_target_rows_collapsed` — counting DISTINCT non-delegating horizontal-pack call-sites (alias-immune: a renamed nibble-LUT classifier is caught structurally, NOT by symbol name), with "Wave-owner / enforcement wave: G2 entry" verbatim. Re-verified THIS cycle: the symbol is ZERO in `skinny/crates`+`xtask` (correctly planned-not-live), the nibble-LUT classifier symbols are ZERO in `bbnf-simd/src`, and `parse_that::`/`parse-that =` is not a skinny dep — the mask-unification close test is GREEN on disk today. ACCEPT.

### REVISE (0)

No under-discharged hidden-coupling seam survives the V4 packet under this lens.
Every seam V1-V3 surfaced (substrate-union, mask-convention, phantom `<G>` axis,
CollapsedStage mask, CSS-typed side channel, frame-stack rebuild, eq-set
composition, relocated `RuntimeTarget` data column, 9-ident generic-crate table)
is either refuted-in-place, DELETE-defaulted, or routed to a named structural
co-gate with a stated enforcement wave — and each on-disk fact re-verifies.

### REJECT (0)

No confabulated/unverifiable citation and no refuted-route grounding admitted in
the V4 packet under this lens. The freshest external citation (Lemire-2025 z3)
and the generality-backbone external citation (Pratt-1973 DOI) both
WebFetch/WebSearch-verified exact this cycle.

## Non-Regression Guards Held (CH5 V4)

- No retained sidecar admitted; all sidecar/parallel-substrate routes refuted or fenced (2A LAC-04 REDRESS-named, 2B frame-stack DELETE-only, 2F SINGLE-SIMD-SUBSTRATE + mask-unification lock with the named structural co-gate).
- The FSM/CollapsedStage and balanced-scan mask streams are transient per-call (re-verified `count_top_level_commas` in-loop interior consumption + i32 depth carry; `find_css_significant` dead-test-only) — the central lens fence holds; R03's CollapsedStage-lowerer transient-mask property is a BINDING gate.
- Layer-0/Layer-1 stays one-directional (2B A1, verified `bbnf.asm` x86-only, `:47-48` one-way include; x86 Layer-0 P1-deletion-target, cannot close the aarch64 host).
- No new BIR variant, no sixth `BackendShape`, no public substrate expansion (verified exactly five shapes); the `<G>` phantom axis on the PUBLIC `ValueRef` is routed to R-D DELETE-default + a structural co-gate (re-verified zero animator), not left as a latent public-substrate coupling.
- No broadcast-admission laundering (2A/2C reject the 24-row CSS broadcast).
- The relocated-seam (`RuntimeTarget` data column), the CSS-typed-surface side channel, and the 9-ident generic-crate leak are NAMED as hidden coupling and routed to structural co-gates / DELETE-defaults / SK-V19 receivers, not admitted (A07, A08, A13).
- No parallel SVE2 substrate opened (FEAT_SVE2 host-absent; deployable plane is the single transient NEON eq-fan).
- The SIMD-mask-substrate seam carries the same necessary-not-sufficient discipline (named structural co-gate + G2-entry enforcement wave) the data-column seam already learned — the R-V2-01 lesson is applied and re-verified GREEN on disk.

## Fold Requirements for V5

None. CH5 is ACCEPT and opens no V4 REVISE or REJECT fold item.

Preservation note for any later cycle: keep the substrate-union fence
REDRESS-named and pre-blocked; keep the `EventGrammar` phantom `<G>` axis routed
to R-D DELETE-default + structural co-gate; keep the CollapsedStage lowerer body
diagnostic-only/author-declared until the transient-mask proof + scalar oracle
land; keep the relocated-seam firewall's two seams (`RuntimeTarget` data column
AND CSS-typed side channel) bound; keep the FSM/frame-stack rebuild DELETE-only;
keep the eq-set neutrality split (base kernel neutral, two-fan composition
CSS-exercised-only); and keep the `bbnf_simd_single_mask_convention` structural
co-gate with its G2-entry enforcement wave. The lens re-arms on any V5 fold that
admits a retained sidecar, a second SIMD mask substrate (including a
renamed/aliased nibble-LUT classifier), a phantom-grammar public axis animated
by production code, a non-transient FSM/CollapsedStage mask stream, or a
Layer-0/Layer-1 back-edge.

## Convergence Impact

CH5 returns ACCEPT this cycle. No fabrication, no refuted-route admission, and no
new hidden-coupling seam is introduced; every load-bearing seam re-verifies on
disk and the two external citations re-checked this cycle confirm exact. V3 was
the FIRST clean CH5 cycle after the R-V2-01 reset; this V4 ACCEPT is the SECOND
consecutive clean CH5 cycle, satisfying the two-consecutive-clean §3Z
convergence rule for this lens within the V≤5 ceiling. If the other six V4 lenses
also return ACCEPT with zero orphan REVISE items and no target-packet edits, the
packet converges; CH5 contributes its second consecutive clean cycle toward that
rule.

TALLY accept=16 revise=0 reject=0
