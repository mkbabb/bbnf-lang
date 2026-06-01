# CH5 HIDDEN-COUPLING — SK-V18 T-P2 V3

Pass: SK-V18 Totality T-P2 (totality research) hardening.
Cycle: V3 (re-armed over the V2 fold; V2 returned one REVISE — R-V2-01 — so the
clean-cycle count had reset, and this V3 re-arms the lens against the V2 fold).
Lens: CH5 HIDDEN-COUPLING. No grounded design may imply a parallel substrate /
sidecar producer / Lock-1 violation; the FSM/CollapsedStage research must keep
the mask stream transient; Layer-0/Layer-1 must stay a clean one-directional
dependency.
Target packet: the six SK-V18 dossiers `2A`-`2F` under
`restart/audit/totality/p2/` (2A SK-V18-T-P2; 2B V3-SKV18; 2C V3; 2D
V3-SKV18; 2E V6-SKV18; 2F V3), regenerated 2026-06-01 19:00-19:10 after the V2
CH5 fold.

NOTE on the on-disk V3/CHALLENGE-CONTEXT.md and the V3/CH*.md siblings: those
files (dated May 28) are the STALE SK-V15 cycle's V3 confirmation packet, not
the SK-V18 re-run. The live SK-V18 CH5 predecessor is `../V2/CH5.md` (dated
2026-06-01 19:03). This verdict re-arms the SK-V18 lens over the live dossiers,
not the SK-V15 confirmation contract.

Disposition: ACCEPT.

Tally basis: 16 enumerated CH5-scope items (the same 16 the V1/V2 lens
enumerated, re-armed against the V3 packet). accept=16, revise=0, reject=0.
REVISE rate = 0/16 = 0%. The ≥30% REVISE expectation is a cycle-V1 floor; V1
returned 5 REVISE + 1 REJECT (37.5% of items dispositioned non-ACCEPT), V2
returned 1 REVISE (6.25%), and this V3 cycle finds the single surviving V2
obligation (R-V2-01) genuinely discharged on disk with no new hidden-coupling
seam admitted. The lens does not manufacture a REVISE to hit a V1-era rate; the
fold cleared the bar and no confabulation or refuted-route grounding survives.

## V2 Fold Consumption Ledger (the single V2 CH5 obligation)

| V2 finding | V2 disp | V3 fold check (on disk) | V3 disp |
|---|---|---|---|
| R-V2-01 — the R01 mask-representation-unification fold correctly binds the close test, but the singular-mask-convention guard is a manual `rg` close test, not a named structural co-gate symbol, and the packet does not state the gate-liveness wave-owner with the explicitness it gives the substrate-union/relocated-seam co-gates. A G2 author who vendors `scan_balanced` and RENAMES the upstream classifier passes both the symbol-name grep AND the `movemask.rs:5`-reuse gate by alias, planting a second mask convention with no structural co-gate to catch it. | REVISE | FOLDED. 2F LAC-2F-V3-01 now binds a **named structural co-gate symbol** `bbnf_simd_single_mask_convention` — "the `bbnf-simd` analog of `runtime_target_rows_collapsed`" — asserting that EVERY 64-byte→64-bit pack in the crate routes through the ONE canonical `movemask::movemask_u8x16`, **counting DISTINCT pack implementations (alias-immune: counts `vshrn_n_u16`/`vaddv_u8` horizontal-pack call-sites that do not delegate to the canonical pack, NOT symbol names)**, with **"Wave-owner / enforcement wave: G2 entry"** stated verbatim (the way 2D names P3/G3 for the relocated-seam co-gate). 2E LAC-2E-V6-03 carries the IDENTICAL co-gate symbol co-defined with 2F, same alias-immune counting, same "Wave-owner / enforcement wave: G2 entry". VERIFIED on disk: `bbnf_simd_single_mask_convention` appears ONLY in the two dossiers (planned symbol, correctly so — `rg` returns zero hits in `skinny/crates`/`skinny/xtask`); the nibble-LUT classifier symbols (`build_nibble_luts`/`find_first_of_nibble_lut`) are ZERO in `bbnf-simd/src`; `parse_that::`/`parse-that =` is NOT a skinny dep. Both (a) and (b) of the V2 correction are present: (a) a structural co-gate symbol with alias-immune distinct-pack counting, (b) the stated enforcement wave (G2 entry). | ACCEPT |

The single residual V2 obligation is folded and on-disk-verified. No new
hidden-coupling seam is admitted in the V3 packet under this lens.

## Spot-Verification Ledger (load-bearing CH5 citations, V3 re-check)

Every load-bearing CH5 citation was independently re-checked this cycle (on-disk
reads + two WebFetch confirmations of the most suspicious citations). All
verified; none confabulated.

| citation | dossier | check | result |
|---|---|---|---|
| five-shape `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`, dispatch on `cost.chosen` | 2C/2D | read `lower/mod.rs:18`-`26` | VERIFIED exactly five; `match cost.chosen` arm-for-arm; no sixth |
| `CollapsedStage` delegates to `tape_plan::render_rule(.., Collapsed)`, no marker | 2D R03 carry | read `collapsed_stage.rs:1`-`18` | VERIFIED — `debug_assert_eq!(cost.chosen, BackendShape::CollapsedStage)` then `render_rule(.., Collapsed)`; not a `format!` marker |
| `ValueRef<'doc,'input,K=AnyKind,G: EventGrammar = AnyGrammar>` PUBLIC phantom axis | 2C R02 carry | read `tape/mod.rs:175`-`185` | VERIFIED — public substrate type; `_grammar: PhantomData<fn() -> G>`; `Copy`/`Clone` impls carry `<G>` |
| `EventGrammar` zero live production animator | 2C | `rg` runtime impls | VERIFIED — only `AnyGrammar` (default, `event_grammar.rs:19`), `JsonEventGrammar` (`json/event_grammar_witness.rs:17`, witness), `SheetsEventGrammar` (`sheets_witness/event_grammar_witness.rs:16`, witness); no live production animator |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` anomalous second discriminator | 2D R-A | read `grammar_provider.rs:33`,`:40`-`42`,`:110` | VERIFIED — the fork to DELETE exists; `request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts` at `:110` |
| mask-unification close test: `build_nibble_luts`/`find_first_of_nibble_lut == 0` in `bbnf-simd`; upstream `parse_that::` not a dep | 2F R01/R-V2-01 fold | `rg` `bbnf-simd/src`, skinny Cargo.tomls | VERIFIED GREEN — zero nibble-LUT classifier symbols in `bbnf-simd`; `parse_that::`/`parse-that =` absent from skinny deps (only the distinct `parse-that-regex` crate is a dep) |
| `bbnf_simd_single_mask_convention` named co-gate, G2-entry wave-owner, alias-immune | 2F/2E R-V2-01 fold | `rg` crates+xtask+restart | VERIFIED — appears ONLY in 2F LAC-2F-V3-01 + 2E LAC-2E-V6-03 (planned symbol, correctly not-yet-live); both carry the alias-immune distinct-pack counting + "Wave-owner / enforcement wave: G2 entry" |
| `runtime_target_rows_collapsed` PLANNED-not-live (==0) | 2C/2D | `rg` crates+xtask | VERIFIED — ZERO; co-gate not yet live (necessary-not-sufficient correctly disclosed, lands P3/G3) |
| 9-ident `idents` table in generic `ir` crate (`strategy.rs:137`-`185`) | 2C | read `crates/ir/src/registry/strategy.rs:132`-`155` | VERIFIED — idents rows JsonParser/JsonGrammar (`:137`), GoogleSheetsParser (`:143`), CssL4Parser (`:149`), BbnfBootstrap (`:155`)… 9 grammar-named rows in a neutral data table |
| self-gate falsified: 4-name leak regex asserts ZERO, returns 13 | 2C | `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src crates/analysis/src` (REPO-ROOT totality tree) | VERIFIED — exactly 13 sites, RED. (Caution: these are REPO-ROOT `crates/` totality paths, NOT `skinny/crates/`; the count only resolves from repo root.) |
| `count_top_level_commas` consumes `bracket_depth_mask_64` interior mask IN-LOOP, transient i32 depth carry | 2F/2E | read `runtime_simd.rs:25`-`55` | VERIFIED — doc states "the i32 depth carry threads across blocks within this one call"; `bracket_depth_mask_64(.., depth)` per-block, NO retained side array |
| `find_css_significant` two-fan OR-reduce dead (test-only) | 2B/2E/2F | `rg` runtime | VERIFIED — defined `runtime_simd.rs:169`; only caller is `#[cfg(test)]` at `lib.rs:574` (the `:500` hit is a comment) |
| `bbnf.asm` Layer-0 is x86-only (one-directional, P1-deletion target) | 2B/2E | `find` + path | VERIFIED — the only `bbnf.asm` is `crates/bbnf-simd/ext/x86/bbnf.asm`; Layer-0 is x86 macro infra, diagnostic-only on the aarch64 close host |
| REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` retired (the substrate-union retired prior) | 2A LAC-04, 2D R03, 2E/2F | `grep` `skinny/REDRESS.md` | VERIFIED — item 98 retires `G-W3-UNION-SUBSTRATE` (`:2910`,`:2934`) |
| REDRESS 144 `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT, +109.87%, 444.208 Mbps (CSS-positive precedent) | 2E/2F | `grep` `skinny/REDRESS.md` | VERIFIED — item 144 (`:4420`), Track 1 `444.208` Mbps, `+109.87%` (`:4434`-`:4436`) |
| Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" (SVE2 `match` "fastest" 16.0 vs 15.5 GB/s NEON; `vceqq_u8` eq-fan in comments) | 2E | **WebFetch** | VERIFIED REAL — title/date exact; SVE2 `match` "might be the fastest" (16.0 vs 15.5 GB/s, "25% fewer instructions"); `vceqq_u8` comparison is in the COMMENTS, exactly how 2E scopes it |
| Kutenin "porting x86 vector bitmask optimizations to ARM NEON" — `vshrn_n_u16(.., 4)` SHRN-by-4 movemask, 10-15% SPEC CPU 2017 (strlen/memchr) | 2E | **WebFetch** | VERIFIED REAL — author Danila Kutenin (Google Cloud); SHRN by 4 as ~PMOVMSKB one-instruction substitute; "10-15 percent improvements on a strlen distribution extracted from the SPEC CPU 2017 benchmark" |

No confabulated/unverifiable citation was found under this lens in the V3 packet.
The two web-checked citations (Lemire-2026, Kutenin) — the most suspicious
(a 2026-dated post and the canonical-movemask grounding that R-V2-01 hardened) —
both confirm exact title/author/numbers.

## Enumerated CH5 Items

### ACCEPT (16)

The ten V1/V2 ACCEPTs (A01-A10) carry forward unweakened and re-verified:

- **CH5-V3-A01 (2A T2A-V1-SOTA-JSON-001 + REFUTE-JSON-005): simdjson stage-1 grounded TRANSIENT, retained class/cursor streams refuted.** `transfer_reason=transient structural projection ... consumed by one DOM/tape builder`; refuted-assertion-5 rejects "retained parallel class/cursor streams," naming the same retired thesis as REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` (verified retired on disk). Unweakened. ACCEPT.
- **CH5-V3-A02 (2A T2A-V1-LAC-04): Lock-1 substrate-union fence correct and ledger-named.** "Retained cursor/list/class-column/sidecar routes are REDRESS-refuted ... only allowed shape is transient same-loop masks consumed into the existing substrate"; the G6 transient-same-loop-skip is placed on the ADMISSIBLE side of the REDRESS-53 line. REDRESS 96/97/98 + 50/51/53 verified on disk. ACCEPT.
- **CH5-V3-A03 (2B A1 / SKV18-A4): Layer-0/Layer-1 one-directional dependency grounded.** `bbnf.asm` includes Layer-0 one way; per-grammar data outside the macro library; x86 Layer-0 diagnostic-only and a P1 deletion target. Verified `bbnf.asm` is x86-only (`ext/x86/`). One-way dependency preserved; the close-route correction (Layer-0 cannot close M5 Max rows) is honest. ACCEPT.
- **CH5-V3-A04 (2B SKV18-A1, 2F PTG-2F-10): eq-set inner BASE kernel genuinely neutral; mask transient.** Caller-supplied byte set, names no grammar; base ≤8-byte one-fan kernel STRUCTURALLY neutral; 64-bit mask per-block transient. The X01 split (base kernel neutral, two-fan composition CSS-exercised-only) carried. ACCEPT.
- **CH5-V3-A05 (2F PTG-2F-11 + verified `count_top_level_commas`): the balanced/FSM mask stream is TRANSIENT — the central lens fence.** `bracket_depth_mask_64` interior mask consumed in-loop, i32 depth carry threads within one call, never a retained side array (re-verified `runtime_simd.rs:25`-`55`). The central "keep the mask stream transient" fence holds. ACCEPT.
- **CH5-V3-A06 (2C SK-V15-2C-CSS-GENERATOR-SIDECAR + SK-V18 `CSS_GENERATED_RS`): the generator-sidecar refuted, not admitted.** Hand-written CSS parser body in a string literal blocked until a same-wave generated typed provider replaces it; G2 deletes `CSS_GENERATED_RS` in-wave; fact streams "cannot serve as a retained EventTape." Correct hidden-coupling refutation. ACCEPT.
- **CH5-V3-A07 (2C SK-V18-2C-TOTALITY-TREE-9-IDENT-LEAK): the relocated-seam analog correctly named a leak; self-gate falsification proven RED.** 9-ident table in the generic `ir` crate is a per-grammar identity riding a neutral data column the 4-name arm-census grep catches only 4 of 9; self-gate asserts ZERO, returns 13 — verified RED on disk (13 sites, repo-root totality tree). Routed to SK-V19 R16 structural full-row collapse; `tranche_scope=SK-V19-receiver` honestly disclosed. ACCEPT.
- **CH5-V3-A08 (2D R-A / `emit_shape_source==lowered_program`): the un-fork firewall correctly identifies the relocated-seam as the CH5 risk.** Render body reads shape ONLY from `program.policy_summary.backend_shape`, never `target.profile/emitter/output_labels/profile_contract`; md5-distinctness NECESSARY-NOT-SUFFICIENT; `runtime_target_rows_collapsed` co-gate is the structural catch. `RuntimeEmitterKind` fork named as the anomalous second discriminator to DELETE (verified `grammar_provider.rs:40`-`42`,`:110`). ACCEPT.
- **CH5-V3-A09 (2C/2D five-shape canon, no sixth): no public substrate/BIR/shape expansion.** Verified exactly five `BackendShape` variants at `lower/mod.rs:18`-`26`; 2D no-candidates axis explicitly forbids a sixth; CollapsedStage stays an aarch64-gated diagnostic slot. ACCEPT.
- **CH5-V3-A10 (2E refuted-svmatch / two-fan grounding): no parallel SVE2 substrate; deployable plane is the single transient eq-fan.** FEAT_SVE2 host-absent (M5 Max probe), so no scalable-vector dispatch family opens; the two-fan OR-reduce is one transient mask. Lemire-2026 WebFetch-verified real and honestly scoped (SVE2 `match` "fastest" in body, `vceqq_u8` eq-fan in comments). ACCEPT.

The five V2-folded REVISEs (formerly V1 R02-R05, X01) re-verify ACCEPT:

- **CH5-V3-A11 (R02 carry): the `EventGrammar` phantom-axis structural co-gate is bound.** 2C carries `(value_ref_grammar_param_deleted OR event_grammar_phantom_axis_animated == false)` as a structural co-gate analogous to `runtime_target_rows_collapsed`, with R-D DELETE-default at G4 — not merely the P4 token grep. The public `<G>` axis on `ValueRef` (verified `tape/mod.rs:175`) is correctly classed a structural hidden-coupling axis the token-grep cannot see, with zero live animator (verified: only `AnyGrammar` + two witnesses). ACCEPT.
- **CH5-V3-A12 (R03 carry): CollapsedStage transient-mask proof is a binding fence, not an open question.** UNKNOWN-2D-V3-04 is "BOUND by LAC-2D-V3-04 — a fence, not an open net-win": CollapsedStage is a SHAPE SLOT ONLY; body `diagnostic-only/author-declared` until the transient-mask proof + 2E scalar oracle land; an undocumented hand-tuned `Collapsed` loop is Lock-16 INADMISSIBLE → DELETE the body, keep the slot inert. Additionally LEDGER-FENCED against REDRESS 96/97/98 (the streamed-cursor retired prior). The "keep the mask stream transient" fence is a gate. Verified `collapsed_stage.rs:16` delegates; admitted only under `collapsed_stage_author_declared`. ACCEPT.
- **CH5-V3-A13 (R04 carry): the CSS-typed-surface side-channel firewall is bound.** 2C SK-V18-2C-RELOCATED-SEAM-FIREWALL carries a SECOND SEAM: `css_provider_source == generated` with a close test that the G2 emitter reads ZERO symbols from the generic `crates/core/src/runtime/css_l4/` hand-owned surface (the CSS analog of reading `target.profile`). `css_types.rs`-in-generic-core refuted-in-place. ACCEPT.
- **CH5-V3-A14 (R05 carry): the FSM/frame-stack rebuild route is DELETE-only with inline Lock-1 proof.** 2B SKV18-A5 + OQ-2B-05 reconcile: the frame-stack rebuild vehicle is DELETE-default; the balanced-nesting need is met by the recursive scalar shell (native call-stack, transient per-call) + transient eq-set skip with NO frame stack; any future CollapsedStage frame-stack rebuild must carry the Lock-1 transient-per-call-FSM-state proof inline, never a retained `FRAME_PUSH/POP_BOUNDED` array, else INADMISSIBLE. The A3a manifest row and the refuted-assertions row (line 188) are in agreement. The latent sidecar reintroduction is closed. ACCEPT.
- **CH5-V3-A15 (X01 carry): the eq-set neutrality row is split.** The base ≤8-byte one-fan kernel = structurally neutral (caller data; `find_ascii_set_member64` has no live runtime caller, so NOT a JSON-consumer proof; JSON `scan_dispatch` rides `byte_class_from_table_64`); the two-fan ≤13-byte OR-reduce composition (`find_css_significant` shape) = CSS-exercised-only, subject to the SAME neutrality-proof obligation as the shell. 2C, 2B, and 2E all carry the identical split; `find_css_significant` dead-caller re-verified (`lib.rs:574`, `#[cfg(test)]` only). The inaccurate "JSON's `scan_structurals` rides" source comment is named as a same-wave source-fix obligation, not laundered. ACCEPT.

The single V2 REVISE (R-V2-01) re-classes ACCEPT this cycle:

- **CH5-V3-A16 (R-V2-01 folded): the mask-representation-unification fence now carries a named structural co-gate symbol with a stated enforcement wave.** 2F LAC-2F-V3-01 and 2E LAC-2E-V6-03 both bind `bbnf_simd_single_mask_convention` — "the `bbnf-simd` analog of `runtime_target_rows_collapsed`" — counting DISTINCT pack implementations (alias-immune: a renamed/aliased second nibble-LUT classifier is caught because the gate counts non-delegating horizontal-pack call-sites, NOT symbol names), with "Wave-owner / enforcement wave: G2 entry" stated explicitly (mirroring 2D's P3/G3 naming for the relocated-seam co-gate). This is exactly the (a)+(b) correction R-V2-01 demanded: (a) a structural co-gate symbol that defeats the rename/alias bypass, (b) a named enforcement wave. The name-grep is now correctly disclosed as necessary-not-sufficient and the structural co-gate is the sufficient catch. The fold is genuine, the symbol is correctly planned-not-live (it is a gate for the not-yet-existent vendored shell), and the upstream-classifier-absence + mask-unification close tests verify GREEN on disk today. ACCEPT.

### REVISE (0)

No under-discharged hidden-coupling seam survives the V3 packet under this lens.
The V2 R-V2-01 mask-convention co-gate is folded with a named alias-immune
structural symbol and a stated G2-entry enforcement wave.

### REJECT (0)

No confabulated/unverifiable citation and no refuted-route grounding admitted in
the V3 packet under this lens. The two most suspicious citations (Lemire-2026,
Kutenin) WebFetch-verified exact. The V1 X01 REJECT (over-stated eq-set
composition neutrality) remains folded and re-verified ACCEPT (CH5-V3-A15).

## Non-Regression Guards Held (CH5 V3)

- No retained sidecar admitted in any dossier; all sidecar/parallel-substrate routes refuted or fenced (2A LAC-04 + REDRESS-named, 2B frame-stack DELETE-only, 2F SINGLE-SIMD-SUBSTRATE + mask-unification lock with the now-named structural co-gate).
- The FSM/CollapsedStage and balanced-scan mask streams are transient per-call (verified `count_top_level_commas` in-loop interior consumption + i32 depth carry; `find_css_significant` per-block mask, dead-test-only) — the central lens fence holds; R03's CollapsedStage-lowerer transient-mask property is a BINDING gate.
- Layer-0/Layer-1 stays one-directional (2B A1, verified `bbnf.asm` x86-only; x86 Layer-0 P1-deletion-target, cannot close the aarch64 host).
- No new BIR variant, no sixth `BackendShape`, no public substrate expansion (verified exactly five shapes); the `<G>` phantom axis on the PUBLIC `ValueRef` substrate is routed to R-D DELETE-default + structural co-gate (R02 fold), not left as a latent public-substrate coupling.
- No broadcast-admission laundering (2A/2C reject the 24-row CSS broadcast; carried).
- The relocated-seam (`RuntimeTarget` data column), the CSS-typed-surface side channel, and the 9-ident generic-crate leak are NAMED as hidden coupling and routed to structural co-gates / DELETE-defaults, not admitted (A07; A08; A13).
- No parallel SVE2 substrate opened (FEAT_SVE2 host-absent; deployable plane is the single transient NEON eq-fan; Lemire-2026 honestly scoped).
- The SIMD-mask-substrate seam now carries the same necessary-not-sufficient discipline (named structural co-gate + enforcement wave) the data-column seam (2C/2D) already learned — the R-V2-01 lesson is applied.

## Fold Requirements for V4

None. CH5 is ACCEPT and opens no V3 REVISE or REJECT fold item.

Preservation note for any later cycle: keep the substrate-union fence
REDRESS-named and pre-blocked; keep the `EventGrammar` phantom `<G>` axis routed
to R-D DELETE-default + structural co-gate; keep the CollapsedStage lowerer body
diagnostic-only/author-declared until the transient-mask proof + scalar oracle
land; keep the relocated-seam firewall's two seams (`RuntimeTarget` data column
AND CSS-typed side channel) bound; keep the FSM/frame-stack rebuild DELETE-only;
keep the eq-set neutrality split (base kernel neutral, two-fan composition
CSS-exercised-only); and keep the `bbnf_simd_single_mask_convention` structural
co-gate with its G2-entry enforcement wave. The lens re-arms on any V4 fold that
admits a retained sidecar, a second SIMD mask substrate (including a
renamed/aliased nibble-LUT classifier), a phantom-grammar public axis, a
non-transient FSM/CollapsedStage mask stream, or a Layer-0/Layer-1 back-edge.

## Convergence Impact

CH5 returns ACCEPT this cycle. The single V2 CH5 obligation (R-V2-01) is
discharged and on-disk-verified, no fabrication or refuted-route admission
survives, and no new hidden-coupling seam is introduced. Because V2 returned
REVISE on R-V2-01, the clean-cycle count had reset; this V3 ACCEPT is the FIRST
clean CH5 cycle of the new run. A second consecutive clean CH5 cycle (V4) is
still required for the two-consecutive-clean §3Z convergence rule, within the
V≤5 ceiling. If the other six V3 lenses also return ACCEPT with zero orphan
REVISE items and no target-packet edits, the packet enters a confirmation V4;
CH5 contributes its first clean cycle toward that rule.

TALLY accept=16 revise=0 reject=0
