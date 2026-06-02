# CH5 HIDDEN-COUPLING — T-P3 SK-V18 cycle (hardening V1)

Verdict: REVISE (three findings: one binding mechanical defect on the v+1 diff, one
Track1(skinny)≡Track2(totality) second-seam path conflation, one single-substrate
falsifier scope note). The substantive hidden-coupling posture is otherwise sound: the
substrate union holds across every 3A delta, no parallel substrate / sidecar producer /
FactStream-as-shape / runtime-regex substrate / x86-as-aarch64-close is admitted, and the
one ACTIVE parallel-scanner risk (totality `OnceCell<StructuralIndex>` + `simd-scan` probe
API) is carried as an explicit single-priced SK-V19 DEFER, never a silent drop.

Target packet: working-tree SK-V18 V4 extensions over committed `e6c1c2a84`
(`restart/audit/totality/p3/{3A,3B,3C-crystallisation,3C-v+1-diff,3D,3E,3F}` all `M`).
This review judges the working-tree content, the live cycle artefact.

## Lens scope reminder

CH5 = no delta implies a parallel substrate / sidecar producer / renamed-scanner Lock-1
violation / Track1≡Track2 dishonesty; the substrate union must hold across every 3A
delta; 3C's accepted amendments must introduce no coupling.

## Spot-verification of the most load-bearing deltas

| check | required by lens | result |
|---|---|---|
| Cited finding-id resolves (relocated-seam) | yes | `LAC-1E-V5-02` at `1E:148`; `LAC-2D-V3-01/02` at `2D:95-96` — resolve. |
| Cited finding-id resolves (parallel scanner) | yes | `COH18-015` at `1F-coherence:104`; `1E:159` totality-tree CH5-V1-03 carry — resolve, richly developed. |
| Cited LOCKS section exists | yes | 16 numbered locks (`grep -cE '^[0-9]+\. \*\*' = 16`); five `BackendShape` `{Eager/Offset/Event/SinkOnly/Collapsed}` at `lower/mod.rs:18-24`; SK-V17 anchor clause `LOCKS.md:620,622`; governance boundary `:625`. |
| The v+1 diff applies | **yes — FAILS** | `git apply --check` = "corrupt patch at line 38"; `--recount` = "patch failed: LOCKS.md:622 — does not apply." See DEFECT-CH5-V1-01. |
| `RuntimeEmitterKind` fork real (D03/L05-L10) | spot | `grammar_provider.rs:39-43` `enum RuntimeEmitterKind {CompiledLowering, RequestFacts}`; dispatch `runtime_generator.rs:17-25` — real. |
| `CSS_GENERATED_RS` verbatim courier real (D05/L06) | spot | `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` — real. |
| `simd-scan` exports probe API, `OnceCell` absent (COH18-015) | spot | `simd-scan/src/lib.rs:68` exports `{StructuralIndex, next_structural_at_or_after}`; `rg OnceCell simd-scan = 0`; `OnceCell<::simd_scan::StructuralIndex>` lives in the `crates/core` consumer `json.rs:701`. Crate attribution correct. |
| OnceCell is per-parse, not a retained sidecar | spot | `ScanState::new()` builds a fresh empty `OnceCell` at every parse entry (`json.rs:711`, instantiated `:3442`); `ensure_structural_index` is `&mut ScanState` `get_or_init` (idempotent, intra-parse). NOT cross-call. The "admissible `generated_function` class, not a violation" classification is accurate. |
| Single-substrate falsifier honest (L16) | spot | `rg build_nibble_luts\|find_first_of_nibble_lut skinny/crates/bbnf-simd = 0` (true for skinny); canonical SHRN movemask `vshrn_n_u16::<4>` at `bbnf-simd/src/aarch64/movemask.rs:5`. BUT totality `simd-scan/src/lib.rs:68` exports `{NibbleLut, WideLut}` — see DEFECT-CH5-V1-03. |
| `tape/` +4 is the ONE substrate, not a second (D12/L13) | spot | `crates/core/src/runtime/tape/mod.rs:1` = "Shared flat-tape runtime substrate … replaces the per-grammar `OpenFrame` builder and `Vec<Vec<_>>` arena slabs" — canonical substrate, not parallel. |
| REDRESS pre-block 51/53/246/247 second-scanner fence (3D-D08) | spot | `1D:166-171` enumerates all four with the admissible (VIEW over existing tape) vs rejected (second scanner / structural-stream driver / parser-local cursor) distinction — exactly the CH5 discrimination. |

## Per-delta disposition under the CH5 lens

Enumerating the deltas that touch hidden coupling. Non-coupling deltas (D02 named-primitive
gate, D07 aarch64-delete, D11 leak-census, D13 Sheets, D14 authority) are CH5-neutral
ACCEPT pass-through.

| delta (artefact) | CH5 disposition |
|---|---|
| `ARCH-3A-V4-SK18-D01` phantom `<G>` strike (3A) | ACCEPT. Re-anchors generality on the `Cursor` micro-trait declared a VIEW over the existing `Tape`/`ValueRef`/`PayloadArena` (REDRESS-fenced 51/53). No new substrate; `<G>` census empty (`1A-SUB-023`). |
| `ARCH-3A-V4-SK18-D03` un-fork DELETE `RuntimeEmitterKind` (3A) | ACCEPT. Removes the anomalous SECOND discriminator (source-family fork), routes dispatch through cost-selected `BackendShape` — collapses coupling. Fork real at `grammar_provider.rs:39-43`. Post-conditions `generator_grammar_type_count==0` honestly stated as falsifiers, not asserted-live. |
| `ARCH-3A-V4-SK18-D04` relocated-seam firewall (3A) | REVISE — DEFECT-CH5-V1-02. The `emit_shape_source==lowered_program` core firewall is excellent (md5 NECESSARY-NOT-SUFFICIENT, `runtime_target_rows_collapsed` PLANNED full-row co-gate catches a branch relocated into a neutral data table). But the SECOND seam targets `crates/core/src/runtime/css_l4/` — a TOTALITY-tree path — inside a SKINNY-cycle delta. Scope conflation. |
| `ARCH-3A-V4-SK18-D06` 5-shape positive dispatch axis (3A) | ACCEPT. "A sixth shape added to admit a grammar is overfit." No FactStream-as-shape; five preserved. |
| `ARCH-3A-V4-SK18-D08` CollapsedStage inert slot (3A) | ACCEPT. Bars a `Collapsed` lowerer threading a per-call FSM mask through RETAINED parsing = the streamed-cursor shape REDRESS 96/97/98 RETIRED. Anti-sidecar. |
| `ARCH-3A-V4-SK18-D09` G6 retarget + single-movemask (3A) | ACCEPT. One canonical SHRN movemask, no second `vaddv_u8` pack; retarget-the-live-shell not a dead/flat sibling; caller-data byte set keeps the inner kernel neutral. |
| `ARCH-3A-V4-SK18-D10` `css_balanced_component_scan` forced demotion (3A) | ACCEPT. Honestly grammar-scoped name when the SHELL is one-grammar-exercised; inner caller-data kernel stays neutral. Defeats neutral-name coupling. |
| `ARCH-3A-V4-SK18-D12` Pattern-H 71-baseline + `tape/` +4 trace (3A) | ACCEPT. The +4 is the ONE shared substrate (verified `tape/mod.rs:1`), recorded as a roster trace, "+N MUST trace or open O(N) scan." |
| `D-SKV18-L05-L10-unfork` (3C) | REVISE — DEFECT-CH5-V1-02. Locks the un-fork well (stage-separation Aho Ch.8 cite, `RuntimeEmitterKind` DELETED, PLANNED co-gate disclosed), but folds the same `crates/core/src/runtime/css_l4/` totality second-seam into a skinny-scope lock clause. |
| `D-SKV18-L16-single-substrate-movemask` (3C) | REVISE — DEFECT-CH5-V1-03. Clause is coupling-clean and the skinny falsifier `rg ... bbnf-simd = 0` is honest, but it omits that totality `simd-scan` exports a `NibbleLut`/`WideLut` second classifier convention; a reader could treat the skinny-scoped green as a fleet-wide single-substrate proof. |
| `D-SKV18-L16-retarget-not-author` (3C) | ACCEPT. Retargets an already-checkasm-gated kernel onto the live shell, no author-from-scratch loop, byte-set as caller data. Single substrate preserved. |
| `D-SKV18-L10-collapsed-slot` (3C) | ACCEPT. Shape-slot only; transient-mask (per-call, no retained per-grammar side stream) proof + REDRESS 96/97/98 clearance. Anti-sidecar. |
| `D-SKV18-L01-cursor-generality` (3C) | ACCEPT — the ACTIVE parallel-scanner risk is correctly DEFERRED here. The totality `OnceCell<StructuralIndex>` (8/9 grammars, verified per-parse) + `simd-scan` `next_structural_at_or_after` probe API are named FUNCTIONALLY PARALLEL to skinny's scanner (divergent APIs + a skinny-only `parity_hash`) and routed to ONE priced SK-V19 scanner-unification disposition (≈+20..+217 LOC), NOT silently carried. Precisely the honest handling CH5 demands. |
| `D-SKV18-L13-pattern-h-recensus` (3C) | ACCEPT. 71-vs-67 drift honest; +4 traced to the one substrate. |
| `MP.SK19.SCANNER-UNIFY` / `MP-3B-SKV18-D07` DEFER (3B) | ACCEPT. "renamed/parallel-scanner risk is ACTIVE … Decide UNIFY vs renamed-parallel-scanner" — explicit, priced, deferred, not bolted into an SK-V18 gate. `B.W0..B.W4`: "Lock 1 one-substrate forbids the parallel substrate." |
| `3D-D08-substrate-sidecar-lock` (3D) | ACCEPT. Retained structural sidecars, parser-local cursors, public `UnionTape`, sixth shape/substrate all blocked; G6 retarget onto-the-live-shell never parser-local; REDRESS 51/53/246/247 fenced; substrate union cited Lock-1-authoritative. |
| `3E-D02/D05/D16` CSS-typed / 5-shape fleet matrix (3E) | ACCEPT. Fact streams stay diagnostic (no Track1≡Track2 CSS-fact-as-Value-API), five shapes preserved, no FactStream added, `BackendShape` grammar-DERIVED not a tag. |
| `3F` §13.6 SK-V19 tee-up + CH3 pre-block carry (3F) | ACCEPT. Carries scanner asymmetry (COH18-015), the OnceCell re-route, and REDRESS 51/53/246/247 into the SK-V19 GENERALIZE plan. |

## Defects

### DEFECT-CH5-V1-01 (REVISE) — the v+1 diff does not apply

- Target: `restart/audit/totality/p3/3C-locks-v+1-diff.md:47` (`@@ -622,6 +622,38 @@`) and the leading-context block of the fenced diff.
- Conflicting evidence: live `restart/locks/LOCKS.md:622-625`; `git apply --check`.
- Symptom: `git apply --check` = "corrupt patch at line 38"; `git apply --check --recount` = "patch failed: restart/locks/LOCKS.md:622 — patch does not apply."
- Root cause (two compounding faults):
  1. Hunk header arithmetic wrong. Header declares old=6 / new=38, but the body has 5 context lines + 28 added + 0 deleted → actual old=5, new=33. The 5-line over-count makes git read past the hunk and reject it as corrupt.
  2. Leading context missing one blank line. Live `LOCKS.md` has TWO blank lines between the SK-V17 Lock-16 clause (`:622`) and `## v+1 Governance Boundary` (`:625`): `:623` blank, `:624` blank, `:625` heading. The diff supplies only the clause + ONE blank line before the additions, so context fails at the second line even under `--recount`.
- Why CH5, not merely CH1: my spot-verify mandate is "the v+1 diff applies." A LOCKS amendment that cannot be applied is a latent-coupling vector — the addendum text (which binds the relocated-seam firewall, the single-substrate rule, the CollapsedStage anti-sidecar slot, and the parallel-scanner DEFER) drifts against the real LOCKS surface, and the firewall it asserts cannot be CRUD-landed as authored. The amendment CONTENT is coupling-clean; the carrier is broken.
- Repair: in `3C-locks-v+1-diff.md`, (a) add the missing second blank-line context at `:624` so the leading context is `Lock-16 clause` + blank(`:623`) + blank(`:624`); (b) recount old/new from the corrected body (`@@ -622,7 +622,35 @@`); (c) re-verify `git apply --check` is clean.
- Owner: 3C author. Severity: medium (mechanical; a non-applying LOCKS amendment blocks Pass Omega CRUD).

### DEFECT-CH5-V1-02 (REVISE) — relocated-seam second seam reaches into the totality tree (Track1≡Track2 path conflation)

- Target: `restart/audit/totality/p3/3A-architecture-synthesis.md:178` (ARCH-3A-V4-SK18-D04 SECOND SEAM) and `restart/audit/totality/p3/3C-locks-v+1-diff.md:57` (D-SKV18-L05-L10 "extends to the CSS-typed side channel via `css_provider_source == generated` … hand-owned generic `crates/core/src/runtime/css_l4/` surface").
- Conflicting evidence: `crates/core/src/runtime/css_l4/` is a TOTALITY-tree path (it exists; the SKINNY CSS surface is `skinny/crates/runtime/src/grammars/css_l4_*` + `runtime_simd.rs`, differently named). D14 (`3A:188`) itself rules that SK-V18 = skinny generalization "verifiable by grepping `skinny/crates/`" and `crates/core/` adoption is SK-V19 — "the single most material drift."
- Symptom: the relocated-seam firewall is an SK-V18 SKINNY-cycle delta whose un-fork is skinny-scoped, yet its second seam binds a `css_provider_source==generated` firewall against the totality `crates/core/` tree. As written it couples a skinny-cycle gate to the totality tree, which is the exact Track1(skinny)≡Track2(totality) scope conflation D14 forbids. A G2 wave owner reading the firewall would scan the wrong tree (or assume the two trees are interchangeable).
- Repair: re-path the D04/L05-L10 second seam to the SKINNY CSS surface for the SK-V18 G2 firewall (the generated CSS grammars + `runtime_simd.rs`), OR explicitly mark the `crates/core/src/runtime/css_l4/` second seam as the SK-V19-totality-adoption seam carried under the same DEFER bundle as D-SKV18-L01 / MP.SK19.SCANNER-UNIFY / COH18-015. Do not leave a skinny-cycle firewall silently targeting the totality tree.
- Owner: 3A + 3C authors. Severity: medium (scope conflation in the packet's strongest anti-coupling firewall; misroutes the G2 neutrality scan).

### DEFECT-CH5-V1-03 (REVISE) — single-substrate falsifier scoped to skinny without naming the totality NibbleLut

- Target: `restart/audit/totality/p3/3C-locks-v+1-diff.md:67` (D-SKV18-L16-single-substrate-movemask) and `3C-locks-crystallisation.md:102` (cost-matrix gate cell `rg build_nibble_luts/find_first_of_nibble_lut bbnf-simd == 0`).
- Conflicting evidence: totality `crates/simd-scan/src/lib.rs:68` exports `{KernelShape, NibbleLut, StructuralAlphabet, WideLut}` — a nibble-LUT classifier convention live in the parallel `simd-scan` crate.
- Symptom: the structural single-substrate falsifier is SKINNY-scoped and genuinely 0 in `bbnf-simd`, but the clause does not state the totality `simd-scan` carries a `NibbleLut`/`WideLut` second classification convention. A reader could treat the green `rg = 0` as a fleet-wide single-substrate proof when a second classifier convention is live in the un-deferred totality tree — a Track1(skinny)≡Track2(totality) scoping ambiguity adjacent to the very parallel-scanner risk D-SKV18-L01 correctly DEFERs.
- Repair: add a one-clause scope note to D-SKV18-L16 (and the matching cost-matrix cell) marking the single-movemask falsifier SKINNY-scoped and folding the totality `simd-scan` `NibbleLut`/`WideLut` classifier into the SK-V19 scanner-unification single-priced disposition (the COH18-015 / D-SKV18-L01 / MP.SK19.SCANNER-UNIFY DEFER carrier) — so the skinny `rg = 0` is never read as a totality single-substrate proof.
- Owner: 3C author. Severity: low (scope-honesty sharpening; the totality nibble-LUT is already deferred elsewhere — this binds the cross-reference so the two are not read inconsistently).

## Residual risk (out-of-packet notes, no in-packet REVISE)

- The stale V1 CHALLENGE-CONTEXT expects "67 Pattern H runtime files" (`CHALLENGE-CONTEXT.md:97`); live SK-V18 count is 71 (`find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l = 71`). The packet records this honestly (D-SKV18-D12 / L13), so it is NOT a defect — but Pass Omega should refresh the CHALLENGE-CONTEXT invariant to 71 to avoid a future-cycle false REJECT.
- The ACTIVE parallel-scanner risk (COH18-015) is correctly deferred but UN-CLOSED until SK-V19 adoption decides UNIFY vs renamed-parallel-scanner. CH5 is satisfied for SK-V18 because the SK-V18 skinny tree holds one substrate (one `Tape`/`ValueRef`/`PayloadArena`, one `bbnf-simd` scanner, per-parse `OnceCell`); the risk is a classification carry, not a violation, and it is named at every surface (3A Open-Q CH5, 3C D-SKV18-L01 DEFER, 3B MP.SK19, 3D-D08, 3E Open-Q CH5, 3F §13.6).

TALLY accept=15 revise=3 reject=0
