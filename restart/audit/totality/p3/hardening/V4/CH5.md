# CH5 HIDDEN-COUPLING — T-P3 SK-V18 cycle (hardening V4)

Verdict: ACCEPT (no in-packet REVISE). The two REVISEs that survived V1→V2→V3
(DEFECT-CH5-V3-01 second-`vaddv_u8`-pack anchor mis-pathed `scalar/`→should-be
`aarch64/`; DEFECT-CH5-V3-02 `NibbleLut`/`WideLut` second-classifier anchor
off-by-one `:68`→should-be `:67`) are BOTH FOLDED in the live working tree and
both corrections independently verify against source. The substantive
hidden-coupling posture is SOUND and UNCHANGED from V2/V3: no delta implies a
parallel substrate / sidecar producer / renamed-scanner Lock-1 violation /
Track1(skinny)≡Track2(totality) dishonesty / FactStream-as-shape /
runtime-regex-or-DFA substrate / x86-as-aarch64-close. The substrate union holds
across every 3A delta; 3C's accepted amendments introduce no coupling. The one
ACTIVE parallel-scanner risk (totality `OnceCell<StructuralIndex>` 8/9 +
`simd-scan` `next_structural_at_or_after` probe API) is carried as ONE priced
SK-V19 DEFER at all six surfaces (3A:3, 3B:1, 3C:4, 3D:4, 3E:3, 3F:1 references),
never a silent drop.

This is the honest convergence read. Cycle V1 expects ≥30% REVISE; the prior
REVISEs were real and have now been folded out — fabricating a fresh REVISE to
hit a quota would itself be the dishonesty this lens exists to catch. The two
load-bearing mechanical defects this lens is most responsible for are resolved
AND re-verified; the residual is a single out-of-packet Pass-Omega hygiene note,
not an in-packet defect.

Target packet: working-tree SK-V18 V4-fold extensions over committed `e6c1c2a84`
(`restart/audit/totality/p3/{3A,3B,3C-crystallisation,3C-v+1-diff,3D,3E,3F}` all
`M` per `git status`). The stale `hardening/V4/`+`V5/` and
`HARDENING-T-P3-V*-CONSOLIDATED.md` files (mtimes May 28) describe the prior
SK-V15 cycle (packet `e6c1c2a84`/`7885b29ab`, "67 Pattern H runtime files", CH5
ACCEPT) — the live SK-V18 invariant is 71 runtime files, recorded honestly under
D-SKV18-D12 / L13. This review judges the live working-tree content and
OVERWRITES the stale May-28 `V4/CH5.md`.

## Lens scope reminder

CH5 = no delta implies a parallel substrate / sidecar producer / renamed-scanner
Lock-1 violation / Track1≡Track2 dishonesty; the substrate union must hold across
every 3A delta; 3C's accepted amendments must introduce no coupling.

## Carried-defect fold verification (the two surviving V1→V3 REVISEs)

| carried defect | folded? | independent source verify |
|---|---|---|
| **DEFECT-CH5-V3-01** (`scalar/`→`aarch64/` second-pack anchor) | **YES** | `3C-locks-v+1-diff.md:76` + `:78` now cite `aarch64/byte_class_from_eq_set_64.rs:79`-`87`; V3-FOLD changelog at `:27`. Source: `wc -l scalar/byte_class_from_eq_set_64.rs = 38` (confirms `:79-87` out-of-bounds for the scalar twin); `aarch64/byte_class_from_eq_set_64.rs` = 87 LOC, `:79` `unsafe fn movemask_u8x16`, `:83`-`84` `vaddv_u8(vget_low_u8(bits))`/`vaddv_u8(vget_high_u8(bits))` — the live second `vaddv_u8` shift-add pack the one-movemask rule forbids, verified verbatim. Corrected cite RESOLVES. |
| **DEFECT-CH5-V3-02** (`:68`→`:67` second-classifier anchor) | **YES** | `3C-locks-v+1-diff.md:76` SCOPE NOTE and `3C-locks-crystallisation.md:90` cost-matrix cell now cite `crates/simd-scan/src/lib.rs:67` for `{KernelShape, NibbleLut, StructuralAlphabet, WideLut}`, explicitly distinguished from `:68` `{StructuralIndex, next_structural_at_or_after}` (the probe API); V3-FOLD changelog at `3C-v+1:28`, `3C-cryst:31`. Source: `lib.rs:67` = `pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};`; `:68` = `pub use index::{StructuralIndex, next_structural_at_or_after};` — verified verbatim. Both corrected cites RESOLVE; the two distinct coupling surfaces are no longer collapsed. |

Cross-consistency: the evidence-base COH18-015 (`1F-coherence:104`) cites
`simd-scan/src/lib.rs:68` ONLY for the probe API `{StructuralIndex,
next_structural_at_or_after}` (matches source `:68`); the v+1 diff cites `:67` for
the `NibbleLut`/`WideLut` classifier (matches source `:67`). The evidence base and
the proposal are now internally consistent — `:67`=classifier, `:68`=probe API
everywhere.

## Spot-verification of the most load-bearing deltas

| check | required by lens | result |
|---|---|---|
| Cited finding-id resolves (relocated-seam) | yes | `LAC-1E-V5-02` at `1E:148` resolves (relocated-seam firewall, `emit_shape_source==lowered_program`, `runtime_target_rows_collapsed` PLANNED co-gate). |
| Cited finding-id resolves (parallel scanner) | yes | `COH18-015` at `1F-coherence:104` resolves, richly developed; crate attribution FIXED (`OnceCell` in `crates/core` consumer `json.rs:701`, NOT `simd-scan`; `simd-scan` OnceCell count = 0 verified). `1F-anti-pattern:44` per-parse `generated_function` row resolves. |
| Cited LOCKS section exists | yes | `grep -cE '^[0-9]+\. \*\*' LOCKS.md = 16`; SK-V17 Lock-16 anchor `:620`-`622`; governance boundary `## v+1 Governance Boundary` at `:625` present and verbatim. |
| **The v+1 diff applies** | **yes — PASSES** | extracted fenced diff (37 lines) → `git apply --check` = exit 0; `--recount` = exit 0; `--stat` = `LOCKS.md \| 27 +++`, 1 file, 27 insertions only. Hunk header `@@ -622,6 +622,33 @@` arithmetically correct. DEFECT-CH5-V1-01 (V1 broken diff) STAYS RESOLVED. |
| `RuntimeEmitterKind` fork real (D03/L05-L10) | spot | `skinny/crates/codegen/src/grammar_provider.rs:40`-`43` `pub enum RuntimeEmitterKind {CompiledLowering, RequestFacts}`; dispatch `:110` `if request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts`; live source-family fork driven from `skinny/xtask/src/{main,regen_css}.rs`. Real; the un-fork DELETEs it and routes through cost-selected `BackendShape`. |
| `CSS_GENERATED_RS` verbatim courier real (L06) | spot | `skinny/crates/codegen/src/runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` — real; the verbatim-blob-courier clause REJECTs it as not grammar-driven. |
| Five `BackendShape`, no FactStream-as-shape | spot | `skinny/crates/ir/src/lib.rs:340` `pub enum BackendShape { EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage }` — exactly five, verified verbatim; no sixth, no FactStream. 3E-D05 cites P1-1B "exactly five … no sixth live IR variant"; 3E-D09 future-grammar onboarding "no … sixth shape". |
| OnceCell is per-parse, not a retained sidecar | spot | `1F-anti-pattern:44` Lock-1-classifies it `retention_lifetime = generated_function` (per-parse `&mut ScanState`, NOT `retained-across-call-boundary` per `LOCKS.md:139`-`149`); emitted 8/9 (`math` inert). Source: consumer `OnceCell<::simd_scan::StructuralIndex>` lives in `crates/core/src/grammar/generated/{json,bnf,...}.rs:686+`, NOT in `simd-scan`. ADMISSIBLE class, fenced, NOT a violation. |
| simd-scan OnceCell absent (skinny single-substrate honest) | spot | `rg -l OnceCell crates/simd-scan/src = 0`; skinny `rg 'OnceCell<.*StructuralIndex>' bbnf-simd/src = 0`. Skinny holds one substrate; the totality OnceCell is the deferred SK-V19 reconcile, not a skinny-tree violation. |
| `tape/` +4 is the ONE substrate (D12/L13) | spot | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l = 71` confirmed; +4 = `tape/{mod,cursor,arena,record}.rs` tape-fold trace. Canonical substrate, not parallel. |
| D04 second-seam re-pathed off the totality tree | spot | `3A:210` SECOND SEAM reads ZERO from SKINNY `skinny/crates/runtime/src/grammars/css_l4_*` (7 dirs exist) + `runtime_simd.rs` (exists); totality `crates/core/src/runtime/css_l4/` (exists, 7 files) marked the SK-V19-adoption seam under D-SKV18-L01/MP.SK19. Scope conflation stays RESOLVED (DEFECT-CH5-V1-02). |
| 3B SK-V19 SCANNER-UNIFY DEFER real | spot | `3B:177` `MP.SK19.SCANNER-UNIFY … The renamed/parallel-scanner risk is ACTIVE: skinny bbnf-simd and totality simd-scan are functionally parallel … Decide UNIFY vs renamed-parallel-scanner; ≈+217 reconcile + 8/9 OnceCell re-route`. Explicit, priced, deferred — not bolted into an SK-V18 gate. |
| 3D-D08 REDRESS 51/53/246/247 fence | spot | `3D:130` enumerates retained structural sidecars, parser-local event/structural cursors, public `UnionTape`, sixth `BackendShape`/substrate all blocked; G6 retarget never parser-local; REDRESS 51/53/246/247 fenced; substrate union Lock-1-authoritative; "wire as-is" REFUTED (T-P2). |

## Per-delta disposition under the CH5 lens

3A delta roster is complete and contiguous (D01–D14, no gaps — verified by id
enumeration). Non-coupling deltas (D02 named-primitive (a)-(d) gate, D05 verbatim
courier, D07 aarch64-only/x86-delete, D11 leak-census, D13 Sheets negative
control, D14 skinny-scope authority) are CH5-neutral ACCEPT pass-through. The
substantive coupling posture is unchanged from V2/V3 ACCEPT; the V4 fold's only
CH5-relevant change was landing the two carried V3 anchor corrections (no new
delta, no new coupling). All coupling-touching deltas now ACCEPT.

| delta (artefact) | CH5 disposition |
|---|---|
| `ARCH-3A-V4-SK18-D01` phantom `<G>` strike (3A) | ACCEPT. `<G>` axis DELETE re-anchored on the `Cursor` micro-trait + config-breadth classifier, both VIEWs over the existing `Tape`/`ValueRef`. No new substrate. |
| `ARCH-3A-V4-SK18-D03` un-fork DELETE `RuntimeEmitterKind` (3A) | ACCEPT. Removes the SECOND (source-family) discriminator (`grammar_provider.rs:40`-`43`, real); routes dispatch through cost-selected `BackendShape`; `generator_grammar_type_count==0` falsifier. Collapses coupling. |
| `ARCH-3A-V4-SK18-D04` relocated-seam firewall (3A:210) | ACCEPT. `emit_shape_source==lowered_program` core firewall intact (md5 NECESSARY-NOT-SUFFICIENT; `runtime_target_rows_collapsed` PLANNED full-row co-gate catches a branch relocated into a neutral data table); SECOND SEAM scans the SKINNY CSS surface only, totality `crates/core/src/runtime/css_l4/` explicitly the SK-V19-adoption seam under the same DEFER bundle. DEFECT-CH5-V1-02 STAYS RESOLVED. |
| `ARCH-3A-V4-SK18-D06` 5-shape positive dispatch axis (3A) | ACCEPT. "A sixth shape added to admit a grammar is overfit." No FactStream-as-shape; five preserved (`ir/src/lib.rs:340`). |
| `ARCH-3A-V4-SK18-D08` CollapsedStage inert slot (3A) | ACCEPT. Bars a `Collapsed` lowerer threading a per-call FSM mask through RETAINED parsing = the streamed-cursor REDRESS 96/97/98 RETIRED prior. Anti-sidecar; aarch64-gated diagnostic-only. |
| `ARCH-3A-V4-SK18-D09` G6 retarget + single-movemask (3A) | ACCEPT — DEFECT-CH5-V3-01 carrier RESOLVED. Retarget-the-live-shell, caller-data byte set keeps the inner eq-set kernel neutral; canonical SHRN movemask (`aarch64/movemask.rs:4`-`5` `vshrn_n_u16::<4>`). The second-pack EVIDENCE ANCHOR now correctly cites `aarch64/byte_class_from_eq_set_64.rs:79`-`87` (the live `vaddv_u8` pack), verified verbatim. |
| `ARCH-3A-V4-SK18-D10` `css_balanced_component_scan` forced demotion (3A) | ACCEPT. Honestly grammar-scoped SHELL name; inner caller-data kernel stays neutral. Defeats neutral-name coupling. |
| `ARCH-3A-V4-SK18-D12` Pattern-H 71-baseline + `tape/` +4 trace (3A) | ACCEPT. The +4 is the ONE shared substrate (`tape/`), recorded as a roster trace; "+N MUST trace or open O(N) scan." 71 confirmed. |
| `ARCH-3A` Open-Q CH5 parallel-scanner (3A) | ACCEPT. Names the totality `OnceCell<StructuralIndex>` (8/9) + `crates/simd-scan` random-access probe API as FUNCTIONALLY PARALLEL with divergent APIs + a skinny-only `parity_hash`; routes to the SK-V19 owner with the ≈+20..+217 LOC single priced disposition. Honest open question, not a silent drop. |
| `D-SKV18-L05-L10-unfork` (3C v+1) | ACCEPT. Un-fork well-locked (stage-separation Aho Ch.8 cite, `RuntimeEmitterKind` DELETED, PLANNED co-gate disclosed); relocated-seam second seam scans the skinny CSS surface, totality `css_l4/` marked SK-V19 seam. DELETE-the-un-fork-and-carry discipline, no shim. |
| `D-SKV18-L16-single-substrate-movemask` (3C v+1:76) | ACCEPT — DEFECT-CH5-V3-01 + V3-02 BOTH FOLDED. Single-substrate posture coupling-clean; SKINNY-scope note (DEFECT-V1-03) present; the second-`vaddv_u8`-pack anchor now `aarch64/byte_class_from_eq_set_64.rs:79`-`87` (resolves) and the `NibbleLut`/`WideLut` second-classifier anchor now `simd-scan/src/lib.rs:67` (resolves, distinguished from `:68` probe API). The "one canonical SHRN movemask pack" assertion is honestly the PLANNED `bbnf_simd_single_mask_convention` END STATE, not a current invariant — correctly declared PLANNED. |
| `D-SKV18-L16-retarget-not-author` (3C v+1:78) | ACCEPT. Retargets the checkasm-gated `byte_class_from_eq_set_64` onto the live shell; byte-set as caller data; `runtime_simd.rs:169`,`:180`-`204` cites resolve. Single substrate preserved. |
| `D-SKV18-L10-collapsed-slot` (3C v+1) | ACCEPT. Shape-slot only; transient-mask (per-call, no retained side stream) + REDRESS 96/97/98 clearance. Anti-sidecar. |
| `D-SKV18-L01-cursor-generality` (3C v+1) | ACCEPT — the ACTIVE parallel-scanner risk is correctly DEFERRED. The totality `OnceCell<StructuralIndex>` (8/9 per-parse) + `simd-scan` probe API named FUNCTIONALLY PARALLEL, routed to ONE priced SK-V19 disposition; Decision Engine keeps ≥1 e-graph rewrite + feasibility CSP — no zero-rule scaffold. Honest handling CH5 demands. |
| `D-SKV18-L13-pattern-h-recensus` (3C v+1) | ACCEPT. 71-vs-67 honest; +4 traced to the one substrate; the `strategy.rs` leak + `css_types.rs` RED self-gate carried as SK-V19 close, not silently green. |
| `D-SKV18-L14-named-primitive-abcd` / `L16-aarch64-only` / `L06-verbatim-blob` (3C v+1) | ACCEPT. (a)-(d) four-conjunct gate rejects a relabeled blob; aarch64-ONLY (x86 a DELETE target, x86/AVX-512 literature non-closing — no x86-as-aarch64-close); verbatim `&str` courier REJECT. No coupling. |
| `MP.SK19.SCANNER-UNIFY` / `MP-3B-SKV18-D07` DEFER (3B:177) | ACCEPT. "renamed/parallel-scanner risk is ACTIVE … Decide UNIFY vs renamed-parallel-scanner"; ≈+217 reconcile + 8/9 OnceCell re-route; explicit, priced, DEFERRED, never bolted into an SK-V18 gate. |
| `3D-D08-substrate-sidecar-lock` (3D:130) | ACCEPT. Retained structural sidecars, parser-local cursors, public `UnionTape`, sixth shape/substrate all blocked; G6 retarget never parser-local; REDRESS 51/53/246/247 fenced; substrate union Lock-1-authoritative; "wire as-is" REFUTED. |
| `3E-D02/D05/D09` CSS-typed / 5-shape fleet matrix (3E) | ACCEPT. Fact streams stay DIAGNOSTIC (no Track1≡Track2 CSS-fact-as-Value-API); five shapes preserved; `BackendShape` grammar-DERIVED not a tag; future-grammar onboarding source/metadata-only with empty generic-owner diff and no sixth shape/substrate/BIR/public API. |
| `3F` §13.6 SK-V19 tee-up + CH3 pre-block carry (3F:264) | ACCEPT. Carries the `OnceCell<StructuralIndex>` emission re-route (COH18-015), REDRESS 51/53/246/247, and the SK-V16/V17 reconcile as a Pass-Omega / pre-W-PRUNE blocker into the SK-V19 GENERALIZE plan; runtime regex/DFA not admitted. |

## Residual risk (out-of-packet notes, no in-packet REVISE)

- The ACTIVE parallel-scanner risk (COH18-015) remains correctly deferred but
  UN-CLOSED until SK-V19 adoption decides UNIFY vs renamed-parallel-scanner. CH5
  is satisfied for SK-V18 because the SK-V18 SKINNY tree holds ONE substrate (one
  `Tape`/`ValueRef`/`PayloadArena`, one `bbnf-simd` scanner with its own
  `StructuralIndex`/`scan_dispatch`, ZERO `OnceCell` retention, ZERO
  `next_structural_at_or_after`); the risk is a classification carry, not a
  violation, named at all six proposal surfaces + `1F-anti-pattern:44`.
- The stale `hardening/V4/`+`V5/` packets and `HARDENING-T-P3-V*-CONSOLIDATED.md`
  describe the prior SK-V15 cycle (67 Pattern H files, CH5 ACCEPT). Live SK-V18 is
  71, recorded honestly (D-SKV18-D12 / L13). Pass Omega should refresh the
  CHALLENGE-CONTEXT invariant to 71 to avoid a future-cycle false REJECT. This
  CH5/V4 verdict OVERWRITES the stale May-28 `V4/CH5.md`.
- The canonical-movemask cite `aarch64/movemask.rs:5` names the `vshrn_n_u16::<4>`
  BODY line; the `pub unsafe fn movemask_u8x16` DECLARATION is `:4`. Body-vs-
  signature line choice, consistent across the diff and crystallisation, and the
  cited line IS the characterizing intrinsic — NOT a defect.

TALLY accept=18 revise=0 reject=0
