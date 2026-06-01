# CH5 HIDDEN-COUPLING — SK-V18 T-P2 V5

Pass: SK-V18 Totality T-P2 (totality research) hardening.
Cycle: V5 (third consecutive re-arm over the live SK-V18 packet; V3 returned
ACCEPT 16/0/0 — the FIRST clean CH5 cycle after the V2 R-V2-01 fold reset the
clean-count — and V4 returned ACCEPT 16/0/0, the SECOND consecutive clean cycle
that already satisfied the two-consecutive-clean §3Z rule for this lens. This V5
re-arms independently at the V≤5 ceiling, re-verifying every seam on disk + web
and re-checking that no V5-regeneration edit reintroduced a hidden-coupling
seam).
Lens: CH5 HIDDEN-COUPLING. No grounded design may imply a parallel substrate /
sidecar producer / Lock-1 violation; the FSM/CollapsedStage research must keep
the mask stream transient; Layer-0/Layer-1 must stay a clean one-directional
dependency.
Target packet: the six SK-V18 dossiers `2A`-`2F` under
`restart/audit/totality/p2/` (2A SK-V18-T-P2; 2B V3-SKV18-totality; 2C V3; 2D
V3-SKV18-totality; 2E V6-SKV18-totality; 2F V3), regenerated 2026-06-01
19:05-19:35, working-tree dossiers (the committed HEAD is the SK-V15 V1 packet;
the SK-V18 re-run lives in the working tree, the V1-V4 hardening verdicts and
this V5 alongside it).

Disposition: ACCEPT.

Tally basis: 16 enumerated CH5-scope items (the same 16 the V1/V2/V3/V4 lens
enumerated, re-armed independently against the V5 packet with fresh on-disk reads
and four external WebFetch/WebSearch confirmations). accept=16, revise=0,
reject=0.

## On the ≥30% REVISE cycle-V1 floor

The dispatch carries the V1-cycle floor "Cycle V1 expects >=30% REVISE." That
floor was met when this lens FIRST armed: V1 returned 5 REVISE + 1 REJECT (6 of
16 items dispositioned non-ACCEPT = 37.5%). V2 returned 1 REVISE (R-V2-01, the
mask-convention co-gate, 6.25%); V3 found it genuinely folded and returned 0; V4
re-verified independently and returned 0. This V5 re-arm re-verifies every seam
on disk and on the web and finds NO under-discharged hidden-coupling coupling and
NO confabulated/refuted-route citation. The floor is a V1-CYCLE expectation, not
a per-cycle quota — and manufacturing a REVISE to hit a V1-era rate would itself
be a fabricated finding, which CH5 (the anti-hidden-coupling, anti-confabulation
lens) forbids. The packet has earned ACCEPT through four prior cycles of folds;
the V5 re-arm confirms the fold held across the V5 regeneration.

## Independent Spot-Verification Ledger (this cycle, all re-checked on disk + web)

Every load-bearing CH5 citation was re-checked THIS cycle by direct `grep`/`sed`
read (not carried from V4), plus four external confirmations of the most
suspicious/load-bearing citations. All verified; none confabulated.

| citation | dossier | check THIS cycle | result |
|---|---|---|---|
| `count_top_level_commas` consumes `bracket_depth_mask_64` interior mask IN-LOOP; transient i32 depth carry, NO retained side array | 2F PTG-2F-11 / 2E | read `runtime_simd.rs:20`-`60` | VERIFIED — doc verbatim "the i32 depth carry threads across blocks within this one call"; `bracket_depth_mask_64(.., depth)` per-block, `comma_mask & !interior` consumed in-loop, `depth = depth_out`; no retained array. The CENTRAL transient-mask fence HOLDS. |
| exactly five `BackendShape` variants; dispatch on `cost.chosen` | 2C/2D | read `lower/mod.rs` `select_lowering` | VERIFIED — `match cost.chosen` arm-for-arm `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; no sixth |
| `CollapsedStage` lowerer delegates to `tape_plan::render_rule(.., Collapsed)`, no marker string | 2D | read `collapsed_stage.rs:1`-`18` | VERIFIED — `debug_assert_eq!(cost.chosen, BackendShape::CollapsedStage)` then `super::tape_plan::render_rule(rule, TapeFlavor::Collapsed)`; not a `format!` marker |
| `ValueRef<'doc,'input,K=AnyKind,G: EventGrammar = AnyGrammar>` PUBLIC phantom `<G>` axis; `PhantomData<fn() -> G>`; `Copy`/`Clone`/`impl` all carry `<G>` | 2C R02/WITNESS-COUPLING | read `tape/mod.rs:175`-`210` | VERIFIED — public substrate type; `_grammar: PhantomData<fn() -> G>`; `impl<.., G: EventGrammar> Copy/Clone/ValueRef` all thread `<G>`. A real public-substrate structural coupling. |
| `EventGrammar` zero live PRODUCTION animator | 2C | `grep` all `EventGrammar for` impls | VERIFIED — exactly three: `AnyGrammar` (`tape/event_grammar.rs:19`, default), `JsonEventGrammar` (`grammars/json/event_grammar_witness.rs:17`, witness), `SheetsEventGrammar` (`grammars/sheets_witness/event_grammar_witness.rs:16`, witness). The two named grammars live ONLY in `_witness.rs`; ZERO non-`AnyGrammar` production animator. The phantom axis is unanimated. |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` anomalous second discriminator (the un-fork DELETE target) | 2D R-A | read `grammar_provider.rs:33`,`:40`-`42`,`:110` | VERIFIED — field `:33`, enum `:40`-`42`, fork dispatch `request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts` at `:110`. The fork to DELETE exists. |
| `runtime_target_rows_collapsed` co-gate PLANNED-not-live (==0) | 2C/2D | `grep` skinny/crates+xtask | VERIFIED — ZERO; necessary-not-sufficient correctly disclosed, lands P3/G3 |
| `bbnf_simd_single_mask_convention` co-gate PLANNED-not-live (==0); alias-immune distinct-pack count, G2-entry wave-owner | 2F LAC-2F-V3-01 / 2E LAC-2E-V6-03 | `grep` skinny/crates+xtask | VERIFIED — ZERO in `skinny/crates`+`xtask` (planned symbol, correctly not-yet-live; the R-V2-01 fold). BOTH dossiers co-define it with alias-immune distinct-pack counting + "Wave-owner / enforcement wave: G2 entry" |
| mask-unification close test: `build_nibble_luts`/`find_first_of_nibble_lut == 0` in `bbnf-simd/src`; upstream `parse_that::`/`parse-that` (bare) not a skinny dep | 2F R01/R-V2-01 | `grep` `bbnf-simd/src`, all skinny Cargo.tomls | VERIFIED GREEN — zero nibble-LUT classifier symbols in `bbnf-simd/src`; bare `parse-that`/`parse_that::` absent from skinny deps; only the DISTINCT `parse-that-regex` workspace crate is a dep (runtime/bbnf-bench Cargo.toml) |
| `find_css_significant` two-fan OR-reduce dead (test-only) | 2B/2E/2F R7 | `grep` runtime + caller cfg | VERIFIED — defined `runtime_simd.rs:169`; the only non-comment caller is `lib.rs:574` under `#[cfg(test)]` (nearest `#[cfg(test)]` at `:551`, test fn `neon_significant_skip_matches_scalar`); the `lib.rs:500` hit is a comment |
| `bbnf.asm` Layer-0 is x86-only (one-directional include, P1-deletion target) | 2B A1 / 2E | `find bbnf-simd/ext` + read `:44`-`62` | VERIFIED — the only `bbnf.asm` is `bbnf-simd/ext/x86/bbnf.asm`; `%include "x86inc.asm"`/`"x86util.asm"` one-way; per-grammar `.data` (class LUTs/FSM tables/frame-close-bracket maps) kept OUTSIDE the macro library; `ext/` holds only x86 files. Layer-0 is x86 macro infra, diagnostic-only on the aarch64 close host. One-directional dependency PRESERVED. |
| `movemask_u8x16` canonical SHRN pack `vshrn_n_u16::<4>` at `bbnf-simd/src/aarch64/movemask.rs:5` | 2E SRC-LOCAL-MOVEMASK | read `aarch64/movemask.rs:1`-`12` | VERIFIED — `vshrn_n_u16::<4>(vreinterpretq_u16_u8(value))` then nibble-bit fold; the ONE canonical pack the `bbnf_simd_single_mask_convention` co-gate targets |
| 9-ident `idents` table in generic `ir` crate (repo-root totality tree) | 2C TOTALITY-TREE-9-IDENT-LEAK | read `crates/ir/src/registry/strategy.rs:137`-`185` | VERIFIED — JsonParser/JsonGrammar (`:137`), GoogleSheetsParser/Grammar (`:143`), CssL4Parser (`:149`), BbnfBootstrap/BbnfParser (`:155`), Csv/Math/Bnf/Ebnf/CssPretty (`:161`-`:185`): 9 grammar-named rows in a neutral data table, live-consumed via `for_grammar_with_manifest` |
| self-gate falsified: 4-name leak regex asserts ZERO, returns 13 | 2C | `grep 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src crates/analysis/src` (REPO-ROOT totality tree) | VERIFIED — exactly 13 sites, RED. (Caution preserved: these are repo-root `crates/` totality paths, NOT `skinny/crates/` — the count only resolves from repo root.) |
| `css_types.rs` in GENERIC repo-root `crates/core/` | 2C CSS-TYPES-HOST-SHIM-LEAK | `ls crates/core/src/css_types.rs` | VERIFIED present in generic core (`crates/core/src/css_types.rs`) — the Lock-14-named host shim, refuted-in-place, SK-V19 relocate-or-delete |
| FEAT_SVE2 ABSENT on this M5 Max (the svmatch refutation host gate); FEAT_PMULL/DotProd/I8MM/SHA3 present | 2E SRC-HOST-PROBE | `sysctl machdep.cpu.brand_string` + `sysctl hw.optional.arm.FEAT_*` ON THIS MACHINE | VERIFIED — `machdep.cpu.brand_string = Apple M5 Max`; FEAT_PMULL/DotProd/I8MM/SHA3 = 1; `sysctl hw.optional.arm.FEAT_SVE2` returns `unknown oid` (ABSENT). The svmatch refutation is host-grounded on the actual close host. |
| Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" (SVE2 `match` "might be the fastest" 16.0 vs 15.5 GB/s, 25% fewer instr; `vceqq_u8` eq-fan in COMMENTS only) | 2E SRC-LEMIRE-2026-MATCH | **WebFetch** | VERIFIED REAL — exact title/date; "It might be that the SVE2 function `match` is the fastest" (tentative); 16.0 vs 15.5 GB/s; "25% fewer instructions"; `vceqq_u8` is a COMMENTER ("-.-") suggestion in the comment section, NOT the body — precisely how 2E scopes it ("the `vceqq_u8` eq-fan as the deployable route is a COMMENTER suggestion, not the post's benchmark"). Honestly scoped. |
| Kutenin "Porting x86 vector bitmask optimizations to Arm NEON" — `shrn` movemask, NEON has no PMOVMSKB, strlen/SPEC CPU lineage | 2E SRC-KUTENIN-NEON | **WebSearch** | VERIFIED REAL — Danila Kutenin (Google), Arm Developer/Community blog, slug `porting-x86-vector-bitmask-optimizations-to-arm-neon`; `shrn` (shift-right-and-narrow) movemask, "Arm NEON does not have a PMOVMSKB equivalent", strlen/SPEC CPU lineage confirmed. (The dossier's rendered title "Bit twiddling with Arm Neon: beating SSE movemasks…" and the search's "Porting x86…" title BOTH resolve to the same real URL/slug — the developer.arm.com vs community.arm.com rendered titles of one post, not a confabulation.) |
| simdjson stage-1/stage-2 separation at pinned commit `79bbba3e3e…` `doc/parse_many.md` | 2A T2A-V1-SOTA-JSON-001 + REFUTE-JSON-005 | **WebFetch** | VERIFIED REAL — file exists at the pinned commit; verbatim "in stage 1, we parse the document and find all the structural indexes … validate UTF8. Then, in stage 2, we go through the document again and build the tape using structural indexes found during stage 1." The published architecture is transient structural projection consumed by ONE tape builder — exactly the no-retained-sidecar grounding 2A cites. |
| Fraser/Hanson/Proebsting iburg, LOPLAS 1992, DOI 10.1145/151640.151642 (dispatch-on-selected-pattern; the R-A un-fork grounding) | 2D R-A / LAC-2D-V3-01 | **WebSearch** | VERIFIED REAL — exact authors/title; ACM LOPLAS Vol 1 Issue 3, pp 213-226, DOI exact; tree-pattern matcher dispatches emission on the COST-SELECTED rule, never on a front-end tag — exactly the dispatch-on-`cost.chosen`-not-`RuntimeEmitterKind` architecture 2D grounds. |

No confabulated/unverifiable citation found under this lens in the V5 packet. The
four web-checked citations this cycle (Lemire-2026 — future-dated relative to the
knowledge cutoff and therefore the single most suspicious; Kutenin — the
canonical-movemask grounding the R-V2-01 mask-convention co-gate rests on;
simdjson-pinned-commit — the no-retained-sidecar JSON backbone; iburg-DOI — the
un-fork dispatch backbone) all confirm exact title/author/venue/date/numbers, and
the FEAT_SVE2-absent host gate is re-confirmed on THIS actual M5 Max.

## Enumerated CH5 Items — ACCEPT (16)

All sixteen carry forward from V4 RE-VERIFIED INDEPENDENTLY on disk this cycle,
and re-confirmed present (un-regressed) in the V5-regenerated dossier text.

The ten V1/V2 ACCEPTs:

- **CH5-V5-A01 (2A SOTA-JSON-001 + REFUTE-JSON-005): simdjson stage-1 grounded TRANSIENT; retained class/cursor streams refuted.** `transfer_reason=transient structural projection … consumed by one DOM/tape builder`; refuted-assertion-5 rejects "retained parallel class/cursor streams," the same retired thesis as REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE`. The pinned-commit `parse_many.md` stage-1/stage-2 text WebFetch-verified verbatim THIS cycle. ACCEPT.
- **CH5-V5-A02 (2A LAC-04 / T2A-V1-LAC-04): Lock-1 substrate-union fence correct and REDRESS-ledger-named.** "Retained cursor/list/class-column/sidecar routes are REDRESS-refuted … the only allowed shape is transient same-loop masks consumed into the existing substrate"; the G6 transient-same-loop-skip is on the ADMISSIBLE side of the REDRESS-53 line; the retired prior is REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE`. ACCEPT.
- **CH5-V5-A03 (2B A1 / SKV18-A4): Layer-0/Layer-1 one-directional dependency grounded.** `bbnf.asm` includes Layer-0 one way (`%include`); per-grammar `.data` kept OUTSIDE the macro library; x86 Layer-0 diagnostic-only, a P1 deletion target. Re-verified `bbnf.asm` x86-only on disk (`ext/x86/` holds only x86 files). One-way dependency preserved; the close-route correction (Layer-0 cannot close M5 Max rows) is honest. ACCEPT.
- **CH5-V5-A04 (2B SKV18-A1 / 2F PTG-2F-10): eq-set inner BASE kernel genuinely neutral; mask transient.** Caller-supplied byte set, names no grammar; the X01 split (base ≤8-byte one-fan kernel structurally neutral, two-fan ≤13-byte composition CSS-exercised-only) carried. ACCEPT.
- **CH5-V5-A05 (2F PTG-2F-11 + verified `count_top_level_commas`): the balanced/FSM mask stream is TRANSIENT — the CENTRAL lens fence.** `bracket_depth_mask_64` interior mask consumed in-loop (`comma_mask & !interior`), i32 depth carry threads within one call, never a retained side array (re-verified `runtime_simd.rs:20`-`60` THIS cycle). ACCEPT.
- **CH5-V5-A06 (2C CSS-GENERATOR-SIDECAR): the generator-sidecar refuted, not admitted.** Hand-written CSS parser body in a string literal blocked until a same-wave generated typed provider replaces it; G2 deletes `CSS_GENERATED_RS` in-wave; fact streams cannot serve as a retained EventTape. ACCEPT.
- **CH5-V5-A07 (2C TOTALITY-TREE-9-IDENT-LEAK): the relocated-seam analog NAMED a leak; self-gate falsification proven RED.** 9-ident table in generic `ir` (re-verified `strategy.rs:137`-`185`); 4-name arm-census grep catches 4 of 9; self-gate asserts ZERO, returns 13 (re-verified RED, repo-root totality tree). Routed to SK-V19 R16 structural full-row collapse; `tranche_scope=SK-V19-receiver` honestly disclosed. ACCEPT.
- **CH5-V5-A08 (2D R-A / `emit_shape_source==lowered_program`): the un-fork firewall correctly identifies the relocated-seam as the CH5 risk.** Render body reads shape ONLY from `program.policy_summary.backend_shape`, never `target.profile/emitter/output_labels/profile_contract`; md5-distinctness NECESSARY-NOT-SUFFICIENT; `runtime_target_rows_collapsed` co-gate is the structural catch; `RuntimeEmitterKind` fork named as the anomalous second discriminator to DELETE (re-verified `grammar_provider.rs:40`-`42`,`:110`). iburg DOI WebSearch-verified THIS cycle. ACCEPT.
- **CH5-V5-A09 (2C/2D five-shape canon, no sixth): no public substrate/BIR/shape expansion.** Re-verified exactly five `BackendShape` variants at `lower/mod.rs` `select_lowering`; 2D no-candidates axis forbids a sixth (hard REJECT); CollapsedStage stays an aarch64-gated diagnostic slot. ACCEPT.
- **CH5-V5-A10 (2E refuted-svmatch / two-fan): no parallel SVE2 substrate; deployable plane is the single transient eq-fan.** FEAT_SVE2 host-absent (re-probed on THIS M5 Max THIS cycle: `unknown oid`), no scalable-vector dispatch family opens; the two-fan OR-reduce is one transient mask; Lemire-2026 WebFetch-verified real and honestly scoped (SVE2 `match` "might be fastest" in the BODY, `vceqq_u8` eq-fan as a COMMENTER suggestion). ACCEPT.

The five V2-folded REVISEs (V1 R02-R05, X01) re-verify ACCEPT:

- **CH5-V5-A11 (R02 carry): the `EventGrammar` phantom-axis structural co-gate is bound.** 2C carries `(value_ref_grammar_param_deleted OR event_grammar_phantom_axis_animated == false)` as a structural co-gate analogous to `runtime_target_rows_collapsed`, R-D DELETE-default at G4 — not merely a P4 token grep. The public `<G>` axis on `ValueRef` (re-verified `tape/mod.rs:175`-`210`, `PhantomData<fn() -> G>`, `Copy`/`Clone`/`impl` all threading `<G>`) is a structural hidden-coupling axis the token-grep cannot see, with ZERO live animator (re-verified THIS cycle: exactly `AnyGrammar` + two witnesses). ACCEPT.
- **CH5-V5-A12 (R03 carry): CollapsedStage transient-mask proof is a BINDING fence.** UNKNOWN-2D-V3-04 is BOUND by LAC-2D-V3-04 — CollapsedStage is a SHAPE SLOT ONLY; body `diagnostic-only/author-declared` until the transient-mask proof + 2E TBL-classify scalar oracle land; an undocumented hand-tuned `Collapsed` loop is Lock-16 INADMISSIBLE → DELETE the body, keep the slot inert; LEDGER-FENCED against REDRESS 96/97/98 (a per-call FSM mask threaded through retained parsing IS the streamed-cursor class that retired). Re-verified `collapsed_stage.rs:16` delegates; admitted only under `collapsed_stage_author_declared` (`passes/src/lib.rs:658`). The cost is split COMMITTED-inert-slot (≈0 LOC) vs CONDITIONAL-rebuild. ACCEPT.
- **CH5-V5-A13 (R04 carry): the CSS-typed-surface side-channel firewall is bound.** 2C RELOCATED-SEAM-FIREWALL carries the SECOND SEAM `css_provider_source == generated` with a close test that the G2 emitter reads ZERO symbols from the generic `crates/core/src/runtime/css_l4/` hand-owned surface (the CSS analog of reading `target.profile`); costed as a separate +5-gate-LOC G2-owned row (CH4-V3-02). `css_types.rs`-in-generic-core re-verified on disk. ACCEPT.
- **CH5-V5-A14 (R05 carry): the FSM/frame-stack rebuild route is DELETE-only with inline Lock-1 proof.** 2B SKV18-A5 + OQ-2B-05: frame-stack rebuild vehicle is DELETE-default; the balanced-nesting need is met by the recursive scalar shell (native call-stack, transient per-call) + transient eq-set skip with NO frame stack; a `FRAME_PUSH/POP_BOUNDED` macro IS a retained stack (the refuted sidecar shape, line 188); any future CollapsedStage frame-stack rebuild must carry the Lock-1 transient-per-call-FSM-state proof inline, never a retained frame array, else INADMISSIBLE. The latent sidecar reintroduction is closed. ACCEPT.
- **CH5-V5-A15 (X01 carry): the eq-set neutrality row is split.** Base ≤8-byte one-fan kernel = structurally neutral (caller data; `find_ascii_set_member64` has no live runtime caller, so NOT a JSON-consumer proof; JSON `scan_dispatch` rides `byte_class_from_table_64`); the two-fan ≤13-byte OR-reduce composition (`find_css_significant` shape) = CSS-exercised-only, subject to the SAME neutrality-proof obligation as the shell. `find_css_significant` dead-caller re-verified (`lib.rs:574` under `#[cfg(test)]`). 2C/2B/2E all carry the identical split. ACCEPT.

The single V2 REVISE (R-V2-01) re-classes ACCEPT:

- **CH5-V5-A16 (R-V2-01 folded): the mask-representation-unification fence carries a named structural co-gate with a stated enforcement wave.** 2F LAC-2F-V3-01 + 2E LAC-2E-V6-03 BOTH bind `bbnf_simd_single_mask_convention` — "the `bbnf-simd` analog of `runtime_target_rows_collapsed`" — asserting every 64-byte→64-bit pack DELEGATES to the one canonical `movemask::movemask_u8x16`, counting DISTINCT non-delegating horizontal-pack call-sites (alias-immune: a renamed/aliased nibble-LUT classifier is caught STRUCTURALLY, NOT by symbol name), with "Wave-owner / enforcement wave: G2 entry" verbatim (mirroring 2D's P3/G3 naming). Re-verified THIS cycle: the symbol is ZERO in `skinny/crates`+`xtask` (correctly planned-not-live), the nibble-LUT classifier symbols are ZERO in `bbnf-simd/src`, bare `parse-that`/`parse_that::` is not a skinny dep (only `parse-that-regex`), and the canonical `vshrn_n_u16::<4>` pack is re-verified at `aarch64/movemask.rs:5` — the mask-unification close test is GREEN on disk today. ACCEPT.

### REVISE (0)

No under-discharged hidden-coupling seam survives the V5 packet under this lens.
Every seam V1-V4 surfaced (substrate-union, mask-convention, phantom `<G>` axis,
CollapsedStage mask, CSS-typed side channel, frame-stack rebuild, eq-set
composition, relocated `RuntimeTarget` data column, 9-ident generic-crate table)
is either refuted-in-place, DELETE-defaulted, or routed to a named structural
co-gate with a stated enforcement wave — and each on-disk fact re-verifies in the
V5-regenerated dossier text with no regression.

### REJECT (0)

No confabulated/unverifiable citation and no refuted-route grounding admitted in
the V5 packet under this lens. The four external citations re-checked this cycle
(Lemire-2026 — the future-dated, most-suspicious; Kutenin — the movemask-co-gate
backbone; simdjson-pinned-commit — the no-retained-sidecar JSON backbone; iburg
DOI — the un-fork dispatch backbone) all WebFetch/WebSearch-verify exact. The V1
X01 REJECT (over-stated eq-set composition neutrality) remains folded and
re-verified ACCEPT (CH5-V5-A15).

## Non-Regression Guards Held (CH5 V5)

- No retained sidecar admitted; all sidecar/parallel-substrate routes refuted or fenced (2A LAC-04 REDRESS-named, 2B frame-stack DELETE-only, 2F SINGLE-SIMD-SUBSTRATE + mask-unification lock with the named structural co-gate).
- The FSM/CollapsedStage and balanced-scan mask streams are transient per-call (re-verified `count_top_level_commas` in-loop interior consumption + i32 depth carry; `find_css_significant` dead-test-only) — the central lens fence holds; LAC-2D-V3-04 makes the CollapsedStage-lowerer transient-mask property a BINDING gate, LEDGER-FENCED against REDRESS 96/97/98.
- Layer-0/Layer-1 stays one-directional (2B A1, re-verified `bbnf.asm` x86-only, `%include` one-way, per-grammar `.data` outside the macro library; x86 Layer-0 P1-deletion-target, cannot close the aarch64 host).
- No new BIR variant, no sixth `BackendShape`, no public substrate expansion (re-verified exactly five shapes); the `<G>` phantom axis on the PUBLIC `ValueRef` is routed to R-D DELETE-default + a structural co-gate (re-verified zero animator), not left as a latent public-substrate coupling.
- No broadcast-admission laundering (2A/2C reject the 24-row CSS broadcast).
- The relocated-seam (`RuntimeTarget` data column), the CSS-typed-surface side channel, and the 9-ident generic-crate leak are NAMED as hidden coupling and routed to structural co-gates / DELETE-defaults / SK-V19 receivers, not admitted (A07, A08, A13).
- No parallel SVE2 substrate opened (FEAT_SVE2 host-absent, re-probed on THIS M5 Max; deployable plane is the single transient NEON eq-fan).
- The SIMD-mask-substrate seam carries the same necessary-not-sufficient discipline (named structural co-gate + G2-entry enforcement wave) the data-column seam already learned — the R-V2-01 lesson is applied and re-verified GREEN on disk.

## Fold Requirements for any later cycle

None. CH5 is ACCEPT and opens no V5 REVISE or REJECT fold item.

Preservation note: keep the substrate-union fence REDRESS-named and pre-blocked;
keep the `EventGrammar` phantom `<G>` axis routed to R-D DELETE-default +
structural co-gate; keep the CollapsedStage lowerer body diagnostic-only/
author-declared until the transient-mask proof + scalar oracle land; keep the
relocated-seam firewall's two seams (`RuntimeTarget` data column AND CSS-typed
side channel) bound; keep the FSM/frame-stack rebuild DELETE-only; keep the
eq-set neutrality split (base kernel neutral, two-fan composition CSS-exercised-
only); and keep the `bbnf_simd_single_mask_convention` structural co-gate with
its G2-entry enforcement wave. The lens re-arms on any later fold that admits a
retained sidecar, a second SIMD mask substrate (including a renamed/aliased
nibble-LUT classifier), a phantom-grammar public axis animated by production
code, a non-transient FSM/CollapsedStage mask stream, or a Layer-0/Layer-1
back-edge.

## Convergence Impact

CH5 returns ACCEPT this cycle — the THIRD consecutive clean CH5 cycle (V3, V4,
V5). No fabrication, no refuted-route admission, and no new hidden-coupling seam
is introduced; every load-bearing seam re-verifies on disk and the four external
citations re-checked this cycle (Lemire-2026, Kutenin, simdjson-pinned-commit,
iburg-DOI) confirm exact, with the FEAT_SVE2-absent host gate re-confirmed on the
actual M5 Max close host. The two-consecutive-clean §3Z convergence rule for this
lens was already satisfied at V4; this V5 (the V≤5 ceiling) confirms the fold held
across the V5 regeneration. If the other six V5 lenses also return ACCEPT with
zero orphan REVISE items and no target-packet edits, the packet converges at the
ceiling; CH5 contributes its third consecutive clean cycle.

TALLY accept=16 revise=0 reject=0
