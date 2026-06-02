# CH5 HIDDEN-COUPLING — T-P3 SK-V18 cycle (hardening V5)

Verdict: ACCEPT (no in-packet REVISE). This is the honest convergence read at the
V≤5 ceiling. The substantive hidden-coupling posture is SOUND and UNCHANGED from
the V4 converged state (18/0/0): no delta implies a parallel substrate / sidecar
producer / renamed-scanner Lock-1 violation / Track1(skinny)≡Track2(totality)
dishonesty / FactStream-as-shape / runtime-regex-or-DFA substrate /
x86-as-aarch64-close. The substrate union holds across every 3A delta (D01–D14,
contiguous, no gaps); 3C's accepted amendments introduce no coupling; the single
ACTIVE parallel-scanner risk is carried as ONE priced SK-V19 DEFER at all six
proposal surfaces, never a silent drop. The two carried V1→V3 mechanical anchor
corrections (DEFECT-CH5-V3-01 `aarch64/` re-path; DEFECT-CH5-V3-02 `:67` classifier
vs `:68` probe key) remain folded and independently re-verify verbatim against live
source this cycle. The V1 broken-diff defect (DEFECT-CH5-V1-01) stays resolved:
the v+1 diff applies clean. The V1 second-seam (V1-02) and NibbleLut-scope (V1-03)
Track1≡Track2 conflations stay folded at every surface.

Cycle V1 expects ≥30% REVISE; the prior CH5 REVISEs (V1×3, V3×2) were REAL and
have all been folded out and re-verified. Fabricating a fresh REVISE to hit a quota
at the converged ceiling would itself be the Track1≡Track2 / paper-close dishonesty
this lens exists to catch — the >=30% figure is a V1-cycle expectation, not a binding
floor at a clean V5 confirmation. This verdict OVERWRITES the stale May-28 SK-V15
`V5/CH5.md` (which described commit `77b6e9fd7`, "67 Pattern H runtime files"); the
live SK-V18 cycle judges the working-tree content (71 runtime files, recorded
honestly under D-SKV18-D12 / L13).

Target packet: working-tree SK-V18 V5-fold over the SK-V18 T-P3 artefacts
(`restart/audit/totality/p3/{3A,3B,3C-crystallisation,3C-v+1-diff,3D,3E,3F}` all `M`
per `git status`, mtimes Jun 1). The `hardening/V*/` and `HARDENING-T-P3-V*-CONSOLIDATED`
files describe the prior SK-V15 cohort and are stale carriers.

## Lens scope reminder

CH5 = no delta implies a parallel substrate / sidecar producer / renamed-scanner
Lock-1 violation / Track1≡Track2 dishonesty; the substrate union must hold across
every 3A delta; 3C's accepted amendments must introduce no coupling.

## Required local checks (independently re-run this cycle)

| check | result |
|---|---|
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16` — invariant holds. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `71` — Pattern H census; +4 = `tape/{mod,cursor,arena,record}.rs` tape-fold trace (D-SKV18-D12/L13). Live, NOT the stale `67`. |
| `BackendShape` variant census (`skinny/crates/ir/src/lib.rs:340`) | EXACTLY five `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, verified verbatim; no sixth, no FactStream-as-shape. |
| Extract fenced diff from `3C-locks-v+1-diff.md` → `git apply --check` | 37-line diff; `git apply --check` exit 0; `--recount` exit 0; `--stat` = `LOCKS.md \| 27 +++`, 1 file, 27 insertions only. **DEFECT-CH5-V1-01 STAYS RESOLVED.** |
| `rg -l OnceCell crates/simd-scan/src \| wc -l` | `0` — no retained-sidecar producer in the scanner crate; the `OnceCell<StructuralIndex>` is the `crates/core` per-parse CONSUMER (`json.rs:701`,`:711`), an admissible per-parse `generated_function` class, NOT cross-call retention. |
| `rg 'OnceCell<.*StructuralIndex>' skinny/crates/bbnf-simd/src \| wc -l` | `0` — the SK-V18 SKINNY scanner holds ONE substrate, zero `OnceCell` retention. |
| Required stale-pattern `rg` scan | ONE match (`3C-crystallisation.md:144`, `re-entry trigger`) — a LEGITIMATE DEFER-row re-entry trigger for LAC-2F-V3-03 (required by CH3/CH6 for the open audit-scope DEFER), NOT a CH5 coupling regression. Out-of-lens; orchestrator note only. |

## Spot-verification of the most load-bearing deltas (a cited finding-id resolves; a cited LOCKS section exists; the v+1 diff applies)

| check | required by lens | result |
|---|---|---|
| Cited finding-id resolves (relocated-seam) | yes | `LAC-1E-V5-02` resolves at `1E-locks-evidence.md:148` (relocated-seam firewall, `emit_shape_source==lowered_program`, `runtime_target_rows_collapsed` PLANNED co-gate honestly declared a non-live symbol). |
| Cited finding-id resolves (parallel scanner) | yes | `COH18-015` resolves richly at `1F-coherence-scan.md:104`; crate attribution CORRECT (`OnceCell` in `crates/core` consumer `json.rs:701`, NOT `simd-scan`; `simd-scan/src/lib.rs:68` exports ONLY `{StructuralIndex, next_structural_at_or_after}`). |
| Cited LOCKS section exists | yes | `grep -cE '^[0-9]+\. \*\*' LOCKS.md = 16`; SK-V17 Lock-16 clause tail at `:622`; `## v+1 Governance Boundary` present at `:625`. |
| **The v+1 diff applies** | **yes — PASSES** | extracted fenced diff → `git apply --check` exit 0; header `@@ -622,6 +622,33 @@` arithmetically correct; 27 insertions only. |
| DEFECT-CH5-V3-01 anchor re-verifies (second `vaddv_u8` pack) | spot | `wc -l scalar/byte_class_from_eq_set_64.rs = 38` (out-of-bounds for `:79-87`, zero `vaddv_u8`); `aarch64/byte_class_from_eq_set_64.rs = 87` LOC; `:83`-`:84` = `vaddv_u8(vget_low_u8(bits))`/`vaddv_u8(vget_high_u8(bits))` — the live second shift-add pack the one-movemask rule forbids, WITHIN the cited `:79`-`87` range. RESOLVES. |
| DEFECT-CH5-V3-02 anchor re-verifies (`:67` classifier vs `:68` probe) | spot | `simd-scan/src/lib.rs:67` = `pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};`; `:68` = `pub use index::{StructuralIndex, next_structural_at_or_after};` — verified verbatim; the two distinct coupling surfaces are no longer collapsed. |
| canonical SHRN movemask line choice | spot | `aarch64/movemask.rs:4` = `pub unsafe fn movemask_u8x16` (signature); `:5` = `vshrn_n_u16::<4>` (body, the characterizing intrinsic). The diff/matrix cite `:5` consistently — body-vs-signature choice, the cited line IS the intrinsic, NOT a defect (carried from V4). |
| `RuntimeEmitterKind` fork real (D03/L05-L10) | spot | `grammar_provider.rs:40`-`43` `pub enum RuntimeEmitterKind {CompiledLowering, RequestFacts}`; dispatch `:110` `if request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts`. Real; the un-fork DELETEs it and routes through cost-selected `BackendShape`. Collapses coupling. |
| `CSS_GENERATED_RS` verbatim courier real (L06) | spot | `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` — real; the verbatim-blob-courier clause REJECTs it as not grammar-driven. |
| second-seam re-path off the totality tree | spot | 3A:D04 SECOND SEAM (`3A:178`/`:210` region) scans the SKINNY CSS surface (`skinny/crates/runtime/src/grammars/css_l4_*` 7 dirs + `runtime_simd.rs`, all exist); totality `crates/core/src/runtime/css_l4/` (exists, 7 files) marked the SK-V19-adoption seam under the same DEFER bundle (D-SKV18-L01 / MP.SK19.SCANNER-UNIFY). DEFECT-CH5-V1-02 STAYS RESOLVED. |
| parallel-scanner DEFER carried at all six surfaces | spot | reference counts (COH18-015 / SCANNER-UNIFY / SK-V19 / OnceCell / functionally-parallel): 3A=24, 3B=44, 3C-cryst=14, 3C-v+1=7, 3D=11, 3E=20, 3F=28. The risk is NAMED, priced (≈+20..+217 LOC), and routed to the SK-V19 owner at every surface; never silently dropped. 3A V4 Open-Q CH5 (`3A:273`) routes it to the SK-V19 scanner-unification + Lock-1 substrate-manifest owner. |
| renamed-scanner Lock-1 owner split honest | spot | 3A-D10 names `parse-that-regex` the canonical compile-time regex/HIR owner and `skinny/crates/bbnf-regex` "only a temporary legacy path... not an admissible future owner"; runtime regex/DFA engines REJECTED as runtime substrate unless a prior G-Omega Lock-1 amendment — manifest+consumer proof necessary but NEVER sufficient. No renamed-scanner Lock-1 violation. |
| 3C matrix DEFER census (no silent drop) | spot | exactly TWO DEFER rows: D-SKV18-L01 (scanner-unification, `3C-cryst:77`) and LAC-2F-V3-03 (audit-scope, `3C-cryst:144`); both carry named re-entry triggers; LAC-2F-V3-03 folded into D-SKV18-L16 as a one-line audit-scope note. Zero silent drops. |

## Per-delta disposition under the CH5 lens

3A delta roster is complete and contiguous (D01–D14, verified by id enumeration).
Non-coupling deltas (D02 named-primitive (a)-(d) gate, D05 verbatim courier, D07
aarch64-only/x86-delete, D11 leak-census, D13 Sheets negative control, D14
skinny-scope authority) are CH5-neutral ACCEPT pass-through. The V5 fold introduces
no new delta and no new coupling; every coupling-touching delta is ACCEPT, unchanged
from the V4 converged 18/0/0.

| delta (artefact) | CH5 disposition |
|---|---|
| `ARCH-3A-V4-SK18-D01` phantom `<G>` strike (3A) | ACCEPT. `<G>` axis DELETE re-anchored on the `Cursor` micro-trait + config-breadth classifier, both VIEWs over the existing `Tape`/`ValueRef`/`PayloadArena` (REDRESS-fenced 51/53). `<G>` non-test census EMPTY (1A-SUB-023). No new substrate. |
| `ARCH-3A-V4-SK18-D03` un-fork DELETE `RuntimeEmitterKind` (3A) | ACCEPT. Removes the SECOND (source-family) discriminator (`grammar_provider.rs:40`-`43`, real); routes dispatch through cost-selected `BackendShape`; `generator_grammar_type_count==0` falsifier. Collapses coupling. |
| `ARCH-3A-V4-SK18-D04` relocated-seam firewall (3A:178/210) | ACCEPT. `emit_shape_source==lowered_program` core firewall intact (md5 NECESSARY-NOT-SUFFICIENT; `runtime_target_rows_collapsed` PLANNED full-row co-gate catches a branch relocated into a neutral data table); SECOND SEAM scans the SKINNY CSS surface only; totality `crates/core/src/runtime/css_l4/` explicitly the SK-V19-adoption seam under the same DEFER bundle. DEFECT-CH5-V1-02 STAYS RESOLVED. |
| `ARCH-3A-V4-SK18-D06` 5-shape positive dispatch axis (3A) | ACCEPT. "A sixth shape added to admit a grammar is overfit." No FactStream-as-shape; five preserved (`ir/src/lib.rs:340`). |
| `ARCH-3A-V4-SK18-D08` CollapsedStage inert slot (3A) | ACCEPT. Bars a `Collapsed` lowerer threading a per-call FSM mask through RETAINED parsing = the streamed-cursor REDRESS 96/97/98 RETIRED prior. Anti-sidecar; aarch64-gated diagnostic-only. |
| `ARCH-3A-V4-SK18-D09` G6 retarget + single-movemask (3A) | ACCEPT — DEFECT-CH5-V3-01 carrier RESOLVED. Retarget-the-live-shell, caller-data byte set keeps the inner eq-set kernel neutral; canonical SHRN movemask (`aarch64/movemask.rs:5` `vshrn_n_u16::<4>`). The second-`vaddv_u8`-pack EVIDENCE ANCHOR correctly cites `aarch64/byte_class_from_eq_set_64.rs:79`-`87` (the live `:83`-`:84` pack), re-verified verbatim. |
| `ARCH-3A-V4-SK18-D10` `css_balanced_component_scan` forced demotion + parse-that owner split (3A) | ACCEPT. Honestly grammar-scoped SHELL name; inner caller-data kernel stays neutral. `parse-that-regex` canonical / `bbnf-regex` legacy-only; runtime regex/DFA rejected as substrate without prior G-Omega Lock-1 amendment. Defeats neutral-name + renamed-owner coupling. |
| `ARCH-3A-V4-SK18-D12` Pattern-H 71-baseline + `tape/` +4 trace (3A) | ACCEPT. The +4 is the ONE shared substrate (`tape/`), recorded as a roster trace; "+N MUST trace or open O(N) scan." 71 confirmed. |
| `ARCH-3A` V4 Open-Q CH5 parallel-scanner (3A:273) | ACCEPT. Names the totality `OnceCell<StructuralIndex>` (8/9) + `crates/simd-scan` random-access probe API FUNCTIONALLY PARALLEL with divergent APIs + a skinny-only `parity_hash`; routes to the SK-V19 scanner-unification + Lock-1 owner with the ≈+20..+217 LOC single priced disposition. Honest open question, not a silent drop. |
| `D-SKV18-L05-L10-unfork` (3C v+1) | ACCEPT. Un-fork well-locked (stage-separation Aho Ch.8 cite, `RuntimeEmitterKind` DELETED, PLANNED co-gate disclosed); relocated-seam second seam scans the skinny CSS surface, totality `css_l4/` marked SK-V19 seam. DELETE-the-un-fork-and-carry discipline, no shim. |
| `D-SKV18-L16-single-substrate-movemask` (3C v+1:76) | ACCEPT — DEFECT-CH5-V3-01 + V3-02 BOTH FOLDED + DEFECT-CH5-V1-03 scope note present. Single-substrate posture coupling-clean; the second-`vaddv_u8`-pack anchor is `aarch64/byte_class_from_eq_set_64.rs:79`-`87` (resolves) and the `NibbleLut`/`WideLut` second-classifier anchor is `simd-scan/src/lib.rs:67` (resolves, distinguished from `:68` probe API). The "one canonical SHRN movemask pack" assertion is the PLANNED `bbnf_simd_single_mask_convention` END STATE, correctly declared PLANNED, never a current invariant. The skinny `rg=0` falsifier is SKINNY-scoped; the totality `simd-scan` NibbleLut is folded into the SK-V19 disposition — not read as a totality single-substrate proof. |
| `D-SKV18-L16-retarget-not-author` (3C v+1:78) | ACCEPT. Retargets the checkasm-gated `byte_class_from_eq_set_64` onto the live shell; byte-set as caller data; `runtime_simd.rs:169`,`:180`-`204` cites resolve. Single substrate preserved. |
| `D-SKV18-L10-collapsed-slot` (3C v+1) | ACCEPT. Shape-slot only; transient-mask (per-call, no retained side stream) + REDRESS 96/97/98 clearance. Anti-sidecar. |
| `D-SKV18-L01-cursor-generality` (3C v+1) | ACCEPT — the ACTIVE parallel-scanner risk is correctly DEFERRED. The totality `OnceCell<StructuralIndex>` (8/9 per-parse) + `simd-scan` probe API named FUNCTIONALLY PARALLEL, routed to ONE priced SK-V19 disposition; Decision Engine keeps ≥1 e-graph rewrite (`NormalizeDirectSinkCost`, live) + feasibility CSP — no zero-rule scaffold. Honest handling CH5 demands. |
| `D-SKV18-L13-pattern-h-recensus` (3C v+1) | ACCEPT. 71-vs-67 honest; +4 traced to the one substrate; the `strategy.rs` 9-ident leak + `css_types.rs` RED self-gate carried as SK-V19 close, not silently green. |
| `D-SKV18-L14-named-primitive-abcd` / `L16-aarch64-only` / `L06-verbatim-blob` (3C v+1) | ACCEPT. (a)-(d) four-conjunct gate rejects a relabeled blob; aarch64-ONLY (x86 a DELETE target, x86/AVX-512 literature non-closing — no x86-as-aarch64-close); verbatim `&str` courier REJECT. No coupling. |
| `MP.SK19.SCANNER-UNIFY` / `MP-3B-SKV18-D07` DEFER (3B) | ACCEPT. "renamed/parallel-scanner risk is ACTIVE … Decide UNIFY vs renamed-parallel-scanner"; ≈+217 reconcile + 8/9 OnceCell re-route; explicit, priced, DEFERRED, never bolted into an SK-V18 gate. |
| `3D-D08-substrate-sidecar-lock` (3D:130) | ACCEPT. Retained structural sidecars, parser-local cursors, public `UnionTape`, sixth shape/substrate all blocked; G6 retarget never parser-local; REDRESS 51/53/246/247 fenced; substrate union Lock-1-authoritative; "wire as-is" REFUTED (T-P2). |
| `3E-D02/D05/D08/D09` CSS-typed / 5-shape fleet matrix (3E) | ACCEPT. Fact streams stay DIAGNOSTIC (no Track1≡Track2 CSS-fact-as-Value-API; 2A refutes fact-stream/four-counter as Value API proof); five shapes preserved (no FactStream); `BackendShape` grammar-DERIVED not a tag; BBNF-self/Sheets negative controls require no generic-owner diff, no sixth shape/substrate/BIR/public API/directive. |
| `3F` §13.6 SK-V19 tee-up + CH3 pre-block carry (3F) | ACCEPT. Carries the `OnceCell<StructuralIndex>` emission re-route (COH18-015), REDRESS 51/53/246/247, `css_types.rs` SK-V19 relocate-or-delete, and the SK-V16/V17 reconcile as a Pass-Omega-V6 / pre-W-PRUNE blocker into the SK-V19 GENERALIZE plan; runtime regex/DFA not admitted. |

## Residual risk (out-of-packet notes, no in-packet REVISE)

- The ACTIVE parallel-scanner risk (COH18-015 / D-SKV18-L01) remains correctly
  deferred but UN-CLOSED until SK-V19 adoption decides UNIFY vs
  renamed-parallel-scanner. CH5 is satisfied for SK-V18 because the SK-V18 SKINNY
  tree holds ONE substrate (one `Tape`/`ValueRef`/`PayloadArena`, one `bbnf-simd`
  scanner with its own `StructuralIndex`/`scan_dispatch`, ZERO `OnceCell`
  retention, ZERO `next_structural_at_or_after`); the risk is a classification
  carry, not a violation, named at all six proposal surfaces + `1F-anti-pattern:44`.
- The stale `hardening/V*/` packets and `HARDENING-T-P3-V*-CONSOLIDATED.md` describe
  the prior SK-V15 cohort (67 Pattern H files, commit `77b6e9fd7`, CH5 ACCEPT).
  Live SK-V18 is 71, recorded honestly (D-SKV18-D12 / L13). Pass Omega should
  refresh the stale CHALLENGE-CONTEXT invariant (and the stale `V5/CH5.md` this
  verdict OVERWRITES) to 71 to avoid a future-cycle false REJECT.
- The one stale-pattern `rg` match (`3C-crystallisation.md:144`, `re-entry
  trigger`) is the LEGITIMATE DEFER re-entry trigger for LAC-2F-V3-03 required by
  CH3/CH6 for an open audit-scope DEFER — NOT a CH5 coupling regression.
  Out-of-lens; flagged for the orchestrator only.

TALLY accept=20 revise=0 reject=0
