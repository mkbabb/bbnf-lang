# CH5 HIDDEN-COUPLING — SK-V18 T-P2 V2

Pass: SK-V18 Totality T-P2 (totality research) hardening.
Cycle: V2.
Lens: CH5 HIDDEN-COUPLING. No grounded design may imply a parallel substrate /
sidecar producer / Lock-1 violation; the FSM/CollapsedStage research must keep
the mask stream transient; Layer-0/Layer-1 must stay a clean one-directional
dependency.
Target packet: the six SK-V18 dossiers `2A`-`2F` under
`restart/audit/totality/p2/` at cycle V3/V6 (2A SK-V18-T-P2, 2B V3-SKV18,
2C V3, 2D V3-SKV18, 2E V6-SKV18, 2F V3), regenerated 2026-06-01 19:00-19:10
after the V1 CH5 fold.

Disposition: REVISE.

Tally basis: 16 enumerated CH5-scope items (the same 16 the V1 lens enumerated,
re-armed against the V2 fold). accept=15, revise=1, reject=0.
REVISE rate = 1/16 = 6.25%. V1 returned 5 REVISE + 1 REJECT (31.25% / 6.25%);
this V2 cycle consumes all six V1 fold obligations and finds them genuinely
discharged on disk, leaving one residual seam (R01's surviving live-symbol gap)
that is a *liveness/structural-co-gate* under-binding, not a fabrication or a
refuted-route admission. The V1 ≥30% REVISE expectation is a cycle-V1 floor;
this is cycle V2, and the fold cleared the bar — the lens does not manufacture
REVISEs to hit a V1-era rate.

## V1 Fold Consumption Ledger (the six V1 CH5 obligations)

| V1 finding | V1 disp | V2 fold check (on disk) | V2 disp |
|---|---|---|---|
| R01 — 2F SINGLE-SIMD-SUBSTRATE lock under-binds mask-representation; close test only proves path-dep absence, not absence of a second mask convention | REVISE | LAC-2F-V3-01 now adds MASK-REPRESENTATION UNIFICATION: close test (2) greps `build_nibble_luts\|find_first_of_nibble_lut` in `bbnf-simd/src == 0`, close test (3) gates that G2 reuses `byte_class_from_eq_set_64`/`bracket_depth_mask_64` + `movemask.rs:5`. VERIFIED: both symbols ZERO in `bbnf-simd` today; upstream `parse_that::` is NOT a skinny dep (only the distinct `parse-that-regex` crate is) | folded; residual liveness gap → R-V2-01 |
| R02 — 2C `EventGrammar` phantom axis routed only to a P4 token grep; a `<G>` axis on a PUBLIC substrate is structural, the token-grep cannot see it | REVISE | 2C SK-V18-2C-WITNESS-EMISSION-NEUTRALITY-COUPLING now binds the structural co-gate `(value_ref_grammar_param_deleted OR event_grammar_phantom_axis_animated == false)` analogous to `runtime_target_rows_collapsed`, with R-D DELETE-default at G4. VERIFIED on disk: `ValueRef<'doc,'input,K,G: EventGrammar = AnyGrammar>` at `tape/mod.rs:175` is a public substrate type; the only `EventGrammar` impls are `json/event_grammar_witness.rs` + `sheets_witness/event_grammar_witness.rs` (witness/test-consumed, ZERO live production animator) | ACCEPT |
| R03 — 2D CollapsedStage admitted to the five-shape canon while its transient-mask property is an open UNKNOWN | REVISE | UNKNOWN-2D-V3-04 is now explicitly `BOUND by LAC-2D-V3-04 — a fence, not an open net-win`: CollapsedStage occupies the canon as a SHAPE SLOT ONLY; the body is `diagnostic-only / author-declared` until BOTH a transient-mask proof (`ParserState + CollapsedStagePlan` carries no retained per-grammar side stream, only per-call staged FSM state) AND the 2E TBL-classify scalar oracle land; an undocumented hand-tuned `Collapsed` loop is Lock-16 INADMISSIBLE → DELETE the body, keep the slot inert. VERIFIED: `collapsed_stage.rs:16` delegates to `tape_plan::render_rule(.., Collapsed)`, admitted only under `collapsed_stage_author_declared` | ACCEPT |
| R04 — 2C relocated-seam firewall fenced only `RuntimeTarget` fields, not the CSS-typed-surface side channel | REVISE | 2C SK-V18-2C-RELOCATED-SEAM-FIREWALL now carries a `SECOND SEAM (CSS side channel)`: `css_provider_source == generated` with a close test that the G2 emitter reads ZERO symbols from the generic `crates/core/src/runtime/css_l4/` hand-owned surface. VERIFIED on disk: `css_types.rs` is in generic `crates/core/src/` ("Host shims for the CSS L4 grammar's `-> parse_hex_color(...)`"), correctly refuted-in-place | ACCEPT |
| R05 — 2B FSM/frame-stack rebuild route names a FRAME_PUSH/POP_BOUNDED stack as the rebuild vehicle, silently reintroducing the retained-stack coupling it elsewhere blocks | REVISE | 2B SKV18-A5 + OQ-2B-05 now RECONCILED: the FSM/frame-stack rebuild route is `DELETE-only`; the balanced-nesting need is already met by the recursive scalar shell (native call-stack nesting, transient per-call) + the transient eq-set skip with NO frame stack; any future CollapsedStage rebuild MUST carry the Lock-1 transient-per-call-FSM-state proof INLINE (per 2D LAC-2D-V3-04), never a retained `FRAME_PUSH/POP_BOUNDED` array, else INADMISSIBLE. The A3a manifest row and the refuted-assertions row (line 188) are now in agreement | ACCEPT |
| X01 — 2C eq-set neutrality discharge over-stated: the two-fan ≤13-byte OR-reduce composition is CSS-exercised-only, not covered by the base-kernel JSON discharge | REJECT | 2C SK-V18-2C-CSS-BALANCED-SCAN-NEUTRALITY-PROOF-FORCED-DEMOTION now SPLITS the neutrality claim into (i) BASE ≤8-byte one-fan kernel = STRUCTURALLY neutral (caller DATA, names no grammar; the `find_ascii_set_member64` wrapper has zero live runtime caller, so NOT a JSON-consumer proof; JSON `scan_dispatch` rides `byte_class_from_table_64`) and (ii) TWO-FAN ≤13-byte OR-reduce composition (`find_css_significant` shape) = CSS-exercised-ONLY, subject to the SAME neutrality-proof obligation as the shell. 2B SKV18-A1/A2 and 2E carry the identical split. VERIFIED: `find_css_significant` has only `#[cfg(test)]` caller at `lib.rs:574` | ACCEPT |

All six V1 obligations are folded and on-disk-verified. The single residual is a
liveness/structural-co-gate under-binding inside the otherwise-correct R01 fold
(R-V2-01 below).

## Spot-Verification Ledger (load-bearing CH5 citations, V2 re-check)

Every load-bearing CH5 citation was independently re-checked; all verified.

| citation | dossier | check | result |
|---|---|---|---|
| `ValueRef<'doc,'input,K=AnyKind,G: EventGrammar = AnyGrammar>` PUBLIC phantom axis | 2C R02 fold | read `tape/mod.rs:175`,`:11`,`:183`-`221` | VERIFIED — public substrate type; `Copy`/`Clone`/methods all carry `<G>`; witnesses test-only |
| `EventGrammar` zero live production animator | 2C | `rg` runtime impls | VERIFIED — only `json/event_grammar_witness.rs` + `sheets_witness/event_grammar_witness.rs` (witness) + `tape/event_grammar.rs` (`AnyGrammar`) |
| five-shape `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`, dispatch on `cost.chosen` | 2C/2D | read `lower/mod.rs:18`-`25` | VERIFIED exactly five; no sixth |
| `CollapsedStage` delegates to `tape_plan::render_rule(.., Collapsed)`, no marker | 2D R03 fold | read `collapsed_stage.rs:1`-`18` | VERIFIED — `debug_assert_eq!(cost.chosen, CollapsedStage)` then `render_rule(.., Collapsed)` |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` anomalous second discriminator | 2D R-A | read `grammar_provider.rs:33`,`:40`-`42`,`:110` | VERIFIED — the fork to DELETE exists |
| `runtime_target_rows_collapsed` PLANNED-not-live (==0) | 2C/2D | `rg` skinny/crates skinny/xtask | VERIFIED — ZERO; co-gate not yet live (necessary-not-sufficient correctly disclosed) |
| 9-ident `idents` table in generic `ir` crate (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty) | 2C | read `strategy.rs:137`-`185` | VERIFIED — 9 grammar-named idents rows in a neutral data table |
| self-gate falsified: 4-name leak regex asserts ZERO, returns 13 | 2C | `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src crates/analysis/src` | VERIFIED — 13 sites, RED |
| `count_top_level_commas` consumes `bracket_depth_mask_64` interior mask IN-LOOP, transient i32 depth carry | 2F/2E | read `runtime_simd.rs:29`-`73` | VERIFIED — `commas += (comma_mask & !interior).count_ones()`, depth carry per-call, NO retained side array |
| `find_css_significant` two-fan OR-reduce dead (test-only) | 2B/2E/2F | `rg` runtime | VERIFIED — only `#[cfg(test)]` caller `lib.rs:574` |
| canonical `movemask.rs:5` `vshrn_n_u16::<4>` SHRN movemask (one convention) | 2E | read `movemask.rs:1`-`8` | VERIFIED — the one canonical pack the kernel-internal swap targets |
| upstream `parse-that` `scan_balanced` second substrate (`build_nibble_luts`, `<=8` cap) NOT in `bbnf-simd`; upstream NOT a skinny dep | 2F R01 fold | read `.../scan/balanced.rs:26`,`:44`; `rg` skinny Cargo.toml + `bbnf-simd/src` | VERIFIED — upstream exists with `<=8` debug_assert + distinct classifier; `build_nibble_luts`/`find_first_of_nibble_lut == 0` in `bbnf-simd`; only `parse-that-regex` (distinct crate) is a dep |
| `css_types.rs` in GENERIC `crates/core/`, not `crates/<grammar>/` | 2C R04 fold | `ls`+head | VERIFIED — generic-crate host shim, refuted-in-place |
| Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" (SVE2 `match` fastest; NEON `vceqq_u8` eq-fan in comments) | 2E | WebFetch | VERIFIED REAL — title/date exact; SVE2 `match` "might be the fastest" (16.0 vs 15.5 GB/s); `vceqq_u8` is in the COMMENTS, which is exactly how 2E cites it ("the deployable NEON route is the `vceqq_u8` eq-fan (comments)") — honestly scoped |

No confabulated or unverifiable citation was found under this lens in the V2
packet. The dossiers remain well-grounded on the verifiable surface.

## Enumerated CH5 Items

### ACCEPT (15)

The ten V1 ACCEPTs (A01-A10) carry forward unweakened and re-verified:

- **CH5-V2-A01 (2A T2A-V1-SOTA-JSON-001 + REFUTE-JSON-005): simdjson stage-1 grounded TRANSIENT, retained class/cursor streams refuted.** `transfer_reason=transient structural projection ... consumed by one DOM/tape builder`; refuted-assertion-5 rejects "retained parallel class/cursor streams." Unweakened. ACCEPT.
- **CH5-V2-A02 (2A LAC-04 / T2A-V1-LAC-04): Lock-1 substrate-union fence correct and now ledger-named.** "Retained cursor/list/class-column/sidecar routes are REDRESS-refuted ... only allowed shape is transient same-loop masks consumed into the existing substrate"; the fold added the REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` retirement and REDRESS 50/51/53 line as falsifying evidence, and correctly places the G6 transient-same-loop-skip on the ADMISSIBLE side. ACCEPT.
- **CH5-V2-A03 (2B A1 / SKV18-A4): Layer-0/Layer-1 one-directional dependency grounded.** `bbnf.asm` includes Layer-0 one way (`:47`-`48`), per-grammar data outside the macro library (`:55`-`60`); x86 Layer-0 is diagnostic-only and a P1 deletion target. One-way dependency preserved; close-route correction honest. ACCEPT.
- **CH5-V2-A04 (2B SKV18-A1, 2F PTG-2F-10): eq-set inner BASE kernel genuinely neutral; mask transient.** Caller-supplied byte set, names no grammar; the base ≤8-byte one-fan kernel STRUCTURALLY neutral; the 64-bit mask per-block transient. The V2 fold correctly narrowed this to the BASE kernel only (X01 split). ACCEPT.
- **CH5-V2-A05 (2F PTG-2F-11 + verified `count_top_level_commas`): the balanced/FSM mask stream is TRANSIENT — the central lens fence.** `bracket_depth_mask_64` interior mask consumed in-loop (`comma_mask & !interior`), i32 depth carry threads within one call, never a retained side array. Re-verified on disk. ACCEPT — and R03's fold now extends the same transient-mask discipline to the CollapsedStage lowerer body as a binding admission gate.
- **CH5-V2-A06 (2C SK-V15-2C-CSS-GENERATOR-SIDECAR + SK-V18 `CSS_GENERATED_RS`): the generator-sidecar refuted, not admitted.** Hand-written CSS parser body in a string literal blocked until a same-wave generated typed provider replaces it; G2 deletes `CSS_GENERATED_RS` in-wave. Correct hidden-coupling refutation. ACCEPT.
- **CH5-V2-A07 (2C SK-V18-2C-TOTALITY-TREE-9-IDENT-LEAK): the relocated-seam analog correctly named a leak; self-gate falsification proven RED.** 9-ident table in the generic `ir` crate is a per-grammar identity riding a neutral data column the 4-name arm-census grep cannot fully see (catches 4 of 9). Self-gate asserts ZERO, returns 13 — verified RED on disk. Routed to SK-V19 R16 structural full-row collapse; `tranche_scope=SK-V19-receiver` honestly disclosed. ACCEPT.
- **CH5-V2-A08 (2D R-A / `emit_shape_source==lowered_program`): the un-fork firewall correctly identifies the relocated-seam as the CH5 risk.** Render body reads shape ONLY from `program.policy_summary.backend_shape`, never `target.profile/emitter/output_labels/profile_contract`; md5-distinctness declared NECESSARY-NOT-SUFFICIENT; `runtime_target_rows_collapsed` co-gate is the structural catch. The `RuntimeEmitterKind` fork named as the anomalous second discriminator to DELETE. On-disk-grounded. ACCEPT.
- **CH5-V2-A09 (2C/2D five-shape canon, no sixth): no public substrate/BIR/shape expansion.** Verified exactly five `BackendShape` variants; 2D no-candidates axis explicitly forbids a sixth; CollapsedStage stays an aarch64-gated diagnostic slot. ACCEPT.
- **CH5-V2-A10 (2E refuted-svmatch / two-fan grounding): no parallel SVE2 substrate; deployable plane is the single transient eq-fan.** FEAT_SVE2 host-absent (M5 Max probe), so no scalable-vector dispatch family opens; the two-fan OR-reduce is one transient mask. Lemire-2026 re-verified real and honestly scoped (SVE2 fastest in body, `vceqq_u8` eq-fan in comments). ACCEPT.

The five V1 REVISEs are now folded and re-classed ACCEPT (R02-R05, X01):

- **CH5-V2-A11 (R02 folded): the `EventGrammar` phantom-axis structural co-gate is bound.** 2C now carries `(value_ref_grammar_param_deleted OR event_grammar_phantom_axis_animated == false)` as a structural co-gate analogous to `runtime_target_rows_collapsed`, with R-D DELETE-default at G4 — not merely the P4 token grep. The public `<G>` axis on `ValueRef` (verified `tape/mod.rs:175`) is correctly classed a structural hidden-coupling axis the token-grep cannot see, with zero live animator. The fold is genuine: the binding disposition is the structural delete, not the lexical scan. ACCEPT.
- **CH5-V2-A12 (R03 folded): CollapsedStage transient-mask proof is now a binding fence, not an open question.** LAC-2D-V3-04 binds CollapsedStage as a SHAPE SLOT ONLY; body `diagnostic-only/author-declared` until the transient-mask proof + 2E scalar oracle land; an undocumented hand-tuned `Collapsed` loop is Lock-16 INADMISSIBLE → DELETE the body, keep the slot inert. This is exactly the "keep the mask stream transient" fence the lens demands, now stated as a gate. ACCEPT.
- **CH5-V2-A13 (R04 folded): the CSS-typed-surface side-channel firewall is bound.** 2C extends the relocated-seam firewall with `css_provider_source == generated` and a close test that the G2 emitter reads ZERO symbols from the generic `crates/core/src/runtime/css_l4/` hand-owned surface — the CSS analog of reading `target.profile`. `css_types.rs`-in-generic-core verified and refuted-in-place. ACCEPT.
- **CH5-V2-A14 (R05 folded): the FSM/frame-stack rebuild route is DELETE-only with inline Lock-1 proof.** 2B SKV18-A5 reconciles the prior tension: the frame-stack rebuild vehicle is DELETE-default; the balanced-nesting need is met by the recursive scalar shell (native call-stack, transient per-call) + transient eq-set skip with NO frame stack; any future CollapsedStage frame-stack rebuild must carry the Lock-1 transient-per-call-FSM-state proof inline, never a retained `FRAME_PUSH/POP_BOUNDED` array. The latent sidecar reintroduction is closed. ACCEPT.
- **CH5-V2-A15 (X01 folded): the eq-set neutrality row is split.** The base ≤8-byte one-fan kernel = structurally neutral (caller data); the two-fan ≤13-byte OR-reduce composition (`find_css_significant` shape) = CSS-exercised-only, subject to the SAME neutrality-proof obligation as the shell (forced-demote or prove a non-CSS two-fan caller). 2C, 2B, and 2E all carry the identical split. The over-stated discharge is corrected. ACCEPT.

### REVISE (1)

- **CH5-V2-R-V2-01 (2F LAC-2F-V3-01 + 2E LAC-2E-V6-03, the mask-unification fold): the R01 fold correctly binds the close test but the singular-mask-convention guard is not yet a live symbol, and the V2 packet does not state the gate-liveness wave-owner with the same explicitness it gives the substrate-union/relocated-seam co-gates.** The fold is substantively correct — LAC-2F-V3-01 close tests (2)/(3) grep `build_nibble_luts\|find_first_of_nibble_lut == 0` in `bbnf-simd` and gate G2 reuse of `byte_class_from_eq_set_64`/`bracket_depth_mask_64` + the canonical `movemask.rs:5` SHRN; verified GREEN today (both upstream-classifier symbols ZERO, upstream `parse_that::` not a dep). 2E LAC-2E-V6-03 independently binds the singular-movemask-convention rule (`vshrn_n_u16::<4>` only, no per-kernel `vaddv_u8` re-roll). **The residual seam under the lens:** these are *manual `rg` close tests asserted in the dossier*, not yet a live gate symbol — and unlike the relocated-seam firewall, which the packet at least names as a PLANNED symbol (`runtime_target_rows_collapsed`) with a landing wave (P3/G3), the mask-representation-unification guard has no named gate symbol and no stated wave-owner for *when the grep becomes an enforced xtask/CI check*. So a G2 author who vendors `scan_balanced` and *renames* the upstream classifier (e.g. `build_luts`/`first_of_lut`) plants a second mask convention that BOTH the symbol-name grep AND the `movemask.rs:5`-reuse gate pass by alias, with no structural co-gate (the SIMD-mask analog of the `RuntimeEmitterKind` data-column relocation the firewall exists to catch) to detect it. **Correction:** 2F (co-binding 2E LAC-2E-V6-03) must (a) name a structural mask-convention co-gate symbol with a wave-owner — the bbnf-simd analog of `runtime_target_rows_collapsed`, e.g. `bbnf_simd_single_mask_convention` asserting that every 64-byte→64-bit pack in the crate routes through the one canonical `movemask::movemask_u8x16` (counting *distinct pack implementations*, alias-immune, not *symbol names*); and (b) state the wave at which the grep-or-symbol guard is enforced (G2 entry), the way 2D names P3/G3 for the relocated-seam co-gate. As written, the mask-unification fence is a name-grep, and a name-grep is necessary-not-sufficient against a renamed/aliased second nibble-LUT classifier — the exact necessary-not-sufficient lesson the firewall rows (2C/2D) already learned for the data-column seam, not yet applied to the SIMD-mask-substrate seam. REVISE 2F (co-binds 2E).

### REJECT (0)

No confabulated/unverifiable citation and no refuted-route grounding admitted in
the V2 packet under this lens. The V1 X01 REJECT (over-stated eq-set composition
neutrality) is folded and re-verified ACCEPT (CH5-V2-A15).

## Non-Regression Guards Held (CH5 V2)

- No retained sidecar admitted in any dossier; all sidecar/parallel-substrate routes refuted or fenced (2A LAC-04 + REDRESS-named, 2B frame-stack DELETE-only, 2F SINGLE-SIMD-SUBSTRATE + mask-unification lock).
- The FSM/CollapsedStage and balanced-scan mask streams are transient per-call (verified `count_top_level_commas` in-loop interior consumption + i32 depth carry; `find_css_significant` per-block mask) — the central lens fence holds, and R03's fold makes the CollapsedStage-lowerer transient-mask property a BINDING gate rather than the V1 open question.
- Layer-0/Layer-1 stays one-directional (2B A1, verified `bbnf.asm` one-way include; x86 Layer-0 P1-deletion-target).
- No new BIR variant, no sixth `BackendShape`, no public substrate expansion (verified exactly five shapes); the `<G>` phantom axis on the PUBLIC `ValueRef` substrate is now routed to R-D DELETE-default + structural co-gate, not left as a latent public-substrate coupling (R02 fold).
- No broadcast-admission laundering (2A/2C reject the 24-row CSS broadcast; carried).
- The relocated-seam (`RuntimeTarget` data column), the CSS-typed-surface side channel, and the 9-ident generic-crate leak are NAMED as hidden coupling and routed to structural co-gates / DELETE-defaults, not admitted (R04 fold; A07; A08).
- No parallel SVE2 substrate opened (FEAT_SVE2 host-absent; deployable plane is the single transient NEON eq-fan).

## Convergence Impact

CH5 returns REVISE on one residual seam (R-V2-01) and therefore does NOT
clean-converge this cycle. The packet is materially stronger than V1: all six V1
CH5 fold obligations are discharged and on-disk-verified, and no fabrication or
refuted-route admission survives. The single REVISE is a *liveness/structural-co-gate*
under-binding of the otherwise-correct mask-representation-unification fold — the
name-grep close test should be hardened to a distinct-pack-implementation
structural co-gate with a named wave-owner, mirroring the `runtime_target_rows_collapsed`
discipline the other firewalls already carry. Fold R-V2-01 into V3; the lens
re-arms on any V3 fold that admits a retained sidecar, a second SIMD mask
substrate (including a renamed/aliased nibble-LUT classifier), a phantom-grammar
public axis, a non-transient FSM/CollapsedStage mask stream, or a Layer-0/Layer-1
back-edge. A second consecutive clean CH5 cycle is still required for §3Z
convergence; this cycle is not clean, so the clean-cycle count resets.

TALLY accept=15 revise=1 reject=0
