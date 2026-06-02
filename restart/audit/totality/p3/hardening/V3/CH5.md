# CH5 HIDDEN-COUPLING — T-P3 SK-V18 cycle (hardening V3)

Verdict: REVISE (two carried evidence-anchor mis-cites, UNFOLDED from V2, on the
two surfaces my lens is most responsible for — the second-`vaddv_u8`-pack
convention anchor and the second-classifier `NibbleLut`/`WideLut` export anchor;
both mechanical citation faults on the EVIDENCE lines of the single-substrate
rule, neither a substantive coupling violation). The substantive hidden-coupling
posture is SOUND and UNCHANGED from V2: no parallel substrate / sidecar producer
/ renamed-scanner Lock-1 violation / Track1≡Track2 dishonesty / FactStream-as-
shape / runtime-regex-or-DFA substrate / x86-as-aarch64-close is admitted; the
substrate union holds across every 3A delta; 3C's accepted amendments introduce
no coupling. The ONE active parallel-scanner risk (totality
`OnceCell<StructuralIndex>` 8/9 + `simd-scan` `next_structural_at_or_after` probe
API) is carried as ONE priced SK-V19 DEFER at every surface, never a silent drop.

The two REVISEs are CARRIED from V2 (DEFECT-CH5-V2-01 and DEFECT-CH5-V2-02): the
V3 fold was scoped to the CH4 cost-field gap (`CH4-V2-001`/`CH4-V2-002`) and
heavily rewrote 3C (188 insertions / 133 deletions across the two 3C files), but
the V2 CH5 anchor corrections did NOT land — both wrong cites survive verbatim in
the v+1 diff and (for V2-02) the crystallisation cost matrix.

Target packet: working-tree SK-V18 V3-fold extensions over committed `3f6eb603d`
(`research(sk-v18-T-P2): …ready-for-T-P3`); `restart/audit/totality/p3/{3A,3B,
3C-crystallisation,3C-v+1-diff,3D,3E,3F}` all `M` (mtimes 20:16–20:27). This
review judges the working-tree content, the live cycle artefact. NOTE: the
`hardening/V3/CHALLENGE-CONTEXT.md` and `HARDENING-T-P3-V2-CONSOLIDATED.md` files
in this tree are STALE — they describe the prior SK-V15 cycle (packet
`e6c1c2a84`/`7885b29ab`, "67 Pattern H runtime files", CH5 ACCEPT). The live
SK-V18 invariant is 71 runtime files (recorded honestly under D-SKV18-D12 / L13)
and the live prior-cycle CH5 verdict is the SK-V18 V2/CH5.md REVISE.

## Lens scope reminder

CH5 = no delta implies a parallel substrate / sidecar producer / renamed-scanner
Lock-1 violation / Track1≡Track2 dishonesty; the substrate union must hold across
every 3A delta; 3C's accepted amendments must introduce no coupling.

## Required local checks (run at target context)

| check | result |
|---|---|
| `git show --stat` packet | working-tree fold over `3f6eb603d`; the seven T-P3 proposal artefacts `M`, no live-spec edit. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16` — 16 numbered locks intact. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `71` — live SK-V18 census (stale context expects 67; recorded honestly under L13). |
| five `BackendShape` variants | `lower/mod.rs:18`-`24` = exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` (verified verbatim); no sixth shape, no FactStream-as-shape. |
| extract + `git apply --check /tmp/tp3-locks-v3-cur.diff` | **exit 0 CLEAN**; `--recount` exit 0; `--stat` = `LOCKS.md \| 27 +++`, 1 file, 27 insertions only. Hunk header `@@ -622,6 +622,33 @@` arithmetically correct (5 ctx + 28 added). DEFECT-CH5-V1-01 (V1 broken-diff) STAYS RESOLVED. |
| stale-pattern `rg` scan | ONE match: `3C-crystallisation.md:142` `re-entry trigger` — a LEGITIMATE DEFER-row re-entry trigger required by CH3/CH6 for the LAC-2F-V3-03 open item, NOT a CH5 coupling regression (the scan list is from the stale SK-V15 context). Out-of-lens; noted for the orchestrator, no CH5 defect. |

## Spot-verification of the most load-bearing deltas

| check | required by lens | result |
|---|---|---|
| Cited finding-id resolves (relocated-seam) | yes | `LAC-1E-V5-02` at `1E:148` resolves (relocated-seam firewall, `emit_shape_source==lowered_program`, `runtime_target_rows_collapsed` PLANNED co-gate). |
| Cited finding-id resolves (parallel scanner) | yes | `COH18-015` at `1F-coherence:104` resolves, richly developed; crate attribution FIXED (`OnceCell` in `crates/core` consumer `json.rs:701`, NOT `simd-scan`; `simd-scan` OnceCell count = **0** verified). `1E:159` totality carry + `1F-anti-pattern:44` per-parse row resolve. |
| Cited LOCKS section exists | yes | 16 numbered locks; Lock-1 v+1 ELEVATION `LOCKS.md:137`-`149`; SK-V17 Lock-16 anchor `:610`-`622`; governance boundary `:625` present. |
| **The v+1 diff applies** | **yes — PASSES** | `git apply --check` = exit 0; `--recount` = exit 0; 27 insertions, `LOCKS.md` only. |
| `RuntimeEmitterKind` fork real (D03/L05-L10) | spot | `grammar_provider.rs:40` `pub enum RuntimeEmitterKind {…}` — real; the un-fork DELETEs it and routes dispatch through cost-selected `BackendShape` (`select_lowering(cost.chosen)` `lower/mod.rs:18`-`24`). |
| `CSS_GENERATED_RS` verbatim courier real (L06) | spot | `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` — real; the verbatim-blob-courier clause REJECTs it as "grammar-driven". |
| OnceCell is per-parse, not a retained sidecar | spot | `1F-anti-pattern:44` Lock-1-classifies it `retention_lifetime = generated_function` (per-parse `&mut ScanState`, NOT `retained-across-call-boundary` per `LOCKS.md:139`-`149`); emitted 8/9 (`math` inert). ADMISSIBLE class, fenced, NOT a violation. |
| simd-scan OnceCell absent (skinny single-substrate honest) | spot | `grep -rl OnceCell crates/simd-scan/src = 0`; skinny `bbnf-simd` carries ZERO `OnceCell<StructuralIndex>` and ZERO `next_structural_at_or_after` (COH18-015). |
| `tape/` +4 is the ONE substrate (D12/L13) | spot | `find … = 71` confirmed; +4 = `tape/{mod,cursor,arena,record}.rs` tape-fold trace; canonical substrate, not parallel. |
| Five `BackendShape`, no FactStream-as-shape (3A-D06 / 3E-D05/D16) | spot | five variants exact at `lower/mod.rs:18`-`24`; 3A-D03 classifies `admitted_fact_output` as `SubstrateTarget`/output-plane, never a `BackendShape`; 3E keeps fact streams DIAGNOSTIC; "a sixth shape added to admit a grammar = overfit." |

## Per-delta disposition under the CH5 lens

Non-coupling deltas (D02 named-primitive (a)-(d) gate, D07 aarch64-only/x86-
delete, D11 leak-census, D13 Sheets negative control, D14 skinny-scope authority)
are CH5-neutral ACCEPT pass-through. The substantive coupling posture is unchanged
from V2 ACCEPT; the V3 cost-field fold introduced no new coupling. The two carried
V2 evidence-anchor mis-cites remain the only REVISEs.

| delta (artefact) | CH5 disposition |
|---|---|
| `ARCH-3A-V4-SK18-D01` phantom `<G>` strike (3A) | ACCEPT. `<G>` DELETE re-anchored on the `Cursor` micro-trait + config-breadth classifier, both VIEWs over the existing `Tape`/`ValueRef`. No new substrate. |
| `ARCH-3A-V4-SK18-D03` un-fork DELETE `RuntimeEmitterKind` (3A) | ACCEPT. Removes the SECOND (source-family) discriminator; routes dispatch through cost-selected `BackendShape`; `generator_grammar_type_count==0` falsifier. Collapses coupling. Fork real at `grammar_provider.rs:40`. |
| `ARCH-3A-V4-SK18-D04` relocated-seam firewall (3A:207) | ACCEPT — DEFECT-CH5-V1-02 STAYS RESOLVED. `emit_shape_source==lowered_program` core firewall intact; the SECOND SEAM scans the SKINNY CSS surface only (`skinny/crates/runtime/src/grammars/css_l4_*` + `runtime_simd.rs`); the totality `crates/core/src/runtime/css_l4/` surface is the SK-V19-adoption seam under D-SKV18-L01/MP.SK19. Scope conflation removed. |
| `ARCH-3A-V4-SK18-D06` 5-shape positive dispatch axis (3A:209) | ACCEPT. "A sixth shape added to admit a grammar is overfit." No FactStream-as-shape; five preserved. |
| `ARCH-3A-V4-SK18-D08` CollapsedStage inert slot (3A:211) | ACCEPT. Bars a `Collapsed` lowerer threading a per-call FSM mask through RETAINED parsing = the streamed-cursor REDRESS 96/97/98 RETIRED prior. Anti-sidecar; aarch64-gated diagnostic-only. |
| `ARCH-3A-V4-SK18-D09` G6 retarget + single-movemask (3A) | ACCEPT (substance). Retarget-the-live-shell, caller-data byte set keeps inner kernel neutral; canonical SHRN movemask. The second-pack EVIDENCE-ANCHOR wording is the DEFECT-CH5-V2-01 carrier (REVISE on the 3C v+1 Evidence line, NOT on this 3A delta). |
| `ARCH-3A-V4-SK18-D10` `css_balanced_component_scan` forced demotion (3A) | ACCEPT. Honestly grammar-scoped SHELL name; inner caller-data kernel stays neutral. Defeats neutral-name coupling. |
| `ARCH-3A-V4-SK18-D12` Pattern-H 71-baseline + `tape/` +4 trace (3A) | ACCEPT. The +4 is the ONE shared substrate (`tape/`), recorded as a roster trace; "+N MUST trace or open O(N) scan." 71 confirmed. |
| `ARCH-3A` Open-Q CH5 parallel-scanner (3A:270) | ACCEPT. Names the totality `OnceCell<StructuralIndex>` (8/9) + `crates/simd-scan` random-access probe API as FUNCTIONALLY PARALLEL with divergent APIs + a skinny-only `parity_hash`; routes to the SK-V19 scanner-unification owner + Lock-1 substrate-manifest owner with the ≈ +20..+217 LOC single priced disposition. Honest open question, not a silent drop. |
| `D-SKV18-L05-L10-unfork` (3C v+1) | ACCEPT. Un-fork well-locked; the relocated-seam second seam scans the skinny CSS surface, totality `css_l4/` marked SK-V19 seam (CH5-DEFECT-V1-02 carry). DELETE-the-un-fork-and-carry-unworkability discipline, no shim. |
| `D-SKV18-L16-single-substrate-movemask` (3C v+1:73) | **REVISE — DEFECT-CH5-V3-01 (CARRIED from V2-01) + DEFECT-CH5-V3-02 (CARRIED from V2-02).** The single-substrate POSTURE is coupling-clean and the SKINNY-scope note (CH5-DEFECT-V1-03) is present, but the clause's two EVIDENCE ANCHORS still mis-cite: (a) `scalar/byte_class_from_eq_set_64.rs:79`-`87` — `scalar/` is 38 LOC, `:79-87` is out-of-bounds; the `vaddv_u8` shift-add pack lives in `aarch64/`; (b) `crates/simd-scan/src/lib.rs:68` for `NibbleLut`/`WideLut` — that export is `:67`; `:68` is `{StructuralIndex, next_structural_at_or_after}` (the probe API). |
| `D-SKV18-L16-retarget-not-author` (3C v+1:75) | ACCEPT. Retargets the checkasm-gated `byte_class_from_eq_set_64` onto the live shell; byte-set as caller data; `runtime_simd.rs:169`,`:180`-`204` cites resolve. Single substrate preserved. |
| `D-SKV18-L10-collapsed-slot` (3C v+1) | ACCEPT. Shape-slot only; transient-mask (per-call, no retained side stream) + REDRESS 96/97/98 clearance; `collapsed_stage.rs:16` resolves. Anti-sidecar. |
| `D-SKV18-L01-cursor-generality` (3C v+1) | ACCEPT — the ACTIVE parallel-scanner risk is correctly DEFERRED. The totality `OnceCell<StructuralIndex>` (8/9 per-parse) + `simd-scan` `next_structural_at_or_after` probe API are named FUNCTIONALLY PARALLEL, routed to ONE priced SK-V19 disposition; Decision Engine keeps ≥1 e-graph rewrite + feasibility CSP (regression guard) — no zero-rule scaffold. |
| `D-SKV18-L13-pattern-h-recensus` (3C v+1) | ACCEPT. 71-vs-67 honest; +4 traced to the one substrate; the 9-ident `strategy.rs` leak + `css_types.rs` RED self-gate carried as SK-V19 close, not silently green. |
| `D-SKV18-L14-named-primitive-abcd` / `L16-aarch64-only` / `L06-verbatim-blob` (3C v+1) | ACCEPT. (a)-(d) four-conjunct gate rejects a relabeled blob; aarch64-ONLY (x86 a DELETE target, x86/AVX-512 literature is non-closing architecture pressure — no x86-as-aarch64-close); verbatim `&str` courier REJECT. No coupling. |
| `MP.SK19.SCANNER-UNIFY` / `MP-3B-SKV18-D07` DEFER (3B:177,:216) | ACCEPT. "the renamed/parallel-scanner risk is ACTIVE … Decide UNIFY vs renamed-parallel-scanner"; ≈+217 reconcile + 8/9 OnceCell re-route; explicit, priced, DEFERRED to SK-V19, never bolted into an SK-V18 gate. |
| `3D-D08-substrate-sidecar-lock` (3D:128) | ACCEPT. Retained structural sidecars, parser-local event/structural cursors, public `UnionTape`, sixth shape/substrate all blocked; G6 retarget never parser-local; REDRESS 51/53/246/247 fenced; substrate union Lock-1-authoritative; "wire as-is" REFUTED (T-P2). |
| `3E-D02/D03/D05/D09/D16` CSS-typed / 5-shape fleet matrix (3E) | ACCEPT. Fact streams stay DIAGNOSTIC (no Track1≡Track2 CSS-fact-as-Value-API); 3E-D03 broadcast negative control (no 24-row one-measurement broadcast); five shapes preserved; `BackendShape` grammar-DERIVED not a tag; future-grammar onboarding source/metadata-only with empty generic-owner diff. |
| `3F` §13.6 SK-V19 tee-up + CH3 pre-block carry (3F:253,:265) | ACCEPT. Carries the OnceCell emission re-route (COH18-015), REDRESS 51/53/246/247, and the SK-V16/V17 reconcile as a Pass-Omega-V6 / pre-W-PRUNE blocker into the SK-V19 GENERALIZE plan; runtime regex/DFA not admitted. |

## Defects

### DEFECT-CH5-V3-01 (REVISE) — second-pack evidence anchor mis-pathed (`scalar/` → `aarch64/`); cited range out-of-bounds — CARRIED UNFOLDED from V2-01

- Target: `restart/audit/totality/p3/3C-locks-v+1-diff.md:73` (`D-SKV18-L16-single-substrate-movemask`, Evidence line), citing `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:79`-`87`.
- Conflicting evidence: `wc -l skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs` = **38** (lines 79-87 do NOT exist). The `vaddv_u8` shift-add `movemask_u8x16` pack the clause characterizes as the second-pack convention lives at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:79`-`87` (87 LOC; `:79` `unsafe fn movemask_u8x16`, `:83`-`84` `vaddv_u8(vget_low_u8(bits))`/`vaddv_u8(vget_high_u8(bits))` — verified verbatim). The crystallisation `3C-locks-crystallisation.md:139` cites it correctly with the bare path `byte_class_from_eq_set_64.rs:79`-`87` (resolves to aarch64); the v+1 diff ALONE adds the wrong `scalar/` prefix. The `:54` SK-V17-context manifest line uses bare `scalar/byte_class_from_eq_set_64.rs` (no range) for the scalar REFERENCE — that usage is correct and is NOT part of this defect.
- Why CH5, not merely CH1: this is the EVIDENCE ANCHOR for the live second-`vaddv_u8`-pack convention — the exact second-mask substrate the L16 single-movemask rule exists to forbid. Spot-verifying it surfaces a genuine coupling fact the clause half-states: there are multiple private `movemask_u8x16` definitions across skinny `bbnf-simd/aarch64/` (`movemask.rs` canonical SHRN `vshrn_n_u16::<4>`; `byte_class_from_eq_set_64.rs:79` a `vaddv_u8` shift-add pack). The clause's "the project's ONE canonical SHRN movemask pack" is the PLANNED `bbnf_simd_single_mask_convention` END STATE, not a current invariant — which is precisely why the census is correctly declared PLANNED. The mis-path makes the one cite that would let a reader SEE the live second pack un-resolvable.
- Repair: in `3C-locks-v+1-diff.md:73`, change `scalar/byte_class_from_eq_set_64.rs:79`-`87` → `aarch64/byte_class_from_eq_set_64.rs:79`-`87`. After fold, re-verify `git apply --check` stays exit 0.
- Owner: 3C author. Severity: medium (the single un-resolvable cite is the second-pack-convention anchor; the substantive single-substrate posture is otherwise honest via the PLANNED census).

### DEFECT-CH5-V3-02 (REVISE) — NibbleLut/WideLut second-classifier anchor off-by-one (`:68` → `:67`) — CARRIED UNFOLDED from V2-02 (originally V1's own conflation)

- Target: `restart/audit/totality/p3/3C-locks-v+1-diff.md:73` (the SCOPE NOTE: "the totality `crates/simd-scan` exports a second `NibbleLut`/`WideLut` classifier convention (`crates/simd-scan/src/lib.rs:68`)") and the matching cost-matrix cell `restart/audit/totality/p3/3C-locks-crystallisation.md:88`.
- Conflicting evidence: `crates/simd-scan/src/lib.rs:67` = `pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};` — the second-classifier export (verified verbatim). `:68` = `pub use index::{StructuralIndex, next_structural_at_or_after};` — the probe API, a DIFFERENT export. The controlling evidence confirms `:68` is the probe API: `COH18-015` (`1F-coherence:104`) and `1E:159` both cite `:68` ONLY for `{StructuralIndex, next_structural_at_or_after}`. The off-by-one is inherited from V1's DEFECT-CH5-V1-03 repair text and survived the V2 and V3 folds.
- Why CH5, not merely CH1: `NibbleLut`/`WideLut` is the totality second-classifier CONVENTION whose existence the scope note exists to disclose (so the skinny `rg = 0` green is never read as a totality single-substrate proof). Citing the probe-API line instead of the classifier-export line points an SK-V19 scanner-unification owner at the wrong symbol when reconciling the two conventions, and risks the two `:68` cites (one correct for the probe API in COH18-015, one wrong for the classifier here) being read as the same export — collapsing two distinct coupling surfaces.
- Repair: change `crates/simd-scan/src/lib.rs:68` → `:67` in both `3C-locks-v+1-diff.md:73` (scope note) and `3C-locks-crystallisation.md:88` (cost-matrix cell). Keep `:68` only where the cited symbol is the probe API. After fold, re-verify `git apply --check` stays exit 0.
- Owner: 3C author (carried from V1/V2 CH5). Severity: low (scope-honesty sharpening; the totality `NibbleLut`/`WideLut` is correctly deferred to SK-V19 — this binds the cite to the right line so the classifier and the probe API are not read as one export).

## Residual risk (out-of-packet notes, no in-packet REVISE)

- The canonical-movemask cite `aarch64/movemask.rs:5` names the `vshrn_n_u16::<4>` BODY line; the `pub unsafe fn movemask_u8x16` DECLARATION is `:4`. Body-vs-signature line choice, consistent across the diff and crystallisation, and the cited line IS the characterizing intrinsic — NOT a defect.
- The ACTIVE parallel-scanner risk (COH18-015) remains correctly deferred but UN-CLOSED until SK-V19 adoption decides UNIFY vs renamed-parallel-scanner. CH5 is satisfied for SK-V18 because the SK-V18 SKINNY tree holds one substrate (one `Tape`/`ValueRef`/`PayloadArena`, one `bbnf-simd` scanner with its own `StructuralIndex`/`scan_dispatch`, no `OnceCell` retention, no `next_structural_at_or_after`); the risk is a classification carry, named at every surface (3A:270, 3C D-SKV18-L01, 3B:177/:216, 3D-D08, 3E, 3F:253 + `1F-anti-pattern:44`).
- The stale `hardening/V3/CHALLENGE-CONTEXT.md` + `HARDENING-T-P3-V2-CONSOLIDATED.md` describe the prior SK-V15 cycle (67 Pattern H files, CH5 ACCEPT). Live SK-V18 is 71, recorded honestly (D-SKV18-D12 / L13). Pass Omega should refresh the invariant to 71 to avoid a future-cycle false REJECT.
- The stale-pattern `rg` match at `3C-crystallisation.md:142` (`re-entry trigger`) is a LEGITIMATE DEFER-row re-entry trigger for LAC-2F-V3-03, required by CH3/CH6 for an open item — NOT a CH5 coupling regression. Out-of-lens; flagged for the orchestrator only.

TALLY accept=20 revise=2 reject=0
