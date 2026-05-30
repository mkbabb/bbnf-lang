---
lens: CH3-REGRESSION
pass: T-P2-research
cycle: V3
reviewer: CH3 REGRESSION (V3)
generated_at: 2026-05-29T00:00:00Z
master_head: 91b6893b0
t_p1_locked_commit: 91b6893b0
subjects_reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
v2_dispositions_folded: [CH3-2A-V2, CH3-2B-V2, CH3-2C-V2, CH3-2D-V2, CH3-2E-V2, CH3-2F-V2]
v3_residual_fold_audited: CH1-2F-01-RESIDUAL  # alphabet.rs:118 -> :19-37 anchor precision
pre_blocks_audited: [AZ-IV-eager, StructRegistry-indirection, FieldSource-fence, fact-stream, x86-AVX512-SVE, CollapsedStage-asmjson-FSM, REDRESS-53-parallel-index, REDRESS-88-PMULL-default, REDRESS-89-CTZ-default, lo6-classify_tbl4-CSS, sixth-BackendShape, D6-second-substrate, CH1-V5-001-enumerated-filename]
sections_disposed: 48
accept: 48
revise: 0
reject: 0
verdict: PASS (100% ACCEPT)
---

# CH3 REGRESSION — T-P2 SK-V17 V3

## Mandate

Per PASS-2-RESEARCH §3 (CH3 REGRESSION) + ORCHESTRATOR §3W: scan the six
T-P2 dossiers (`p2/2{a..f}-*.md`) so that **no fold re-opens a route already
refuted in the skinny REDRESS ledger** (`skinny/REDRESS.md`) or pre-blocked by
SK-V17 SPEC §9. Dispatch focus: the four named pre-blocks — **AZ-IV eager**,
**StructRegistry indirection**, **fact-stream**, **x86** — plus the
**FieldSource fence** holding the AZ-IV indirection pre-blocked. A "promising"
research direction REDRESS already falsified is a REJECT.

V3 is the second fold-verification cycle. The V1 CH3 wave returned **45 ACCEPT /
2 REVISE / 0 REJECT (95.7%)**; the V2 CH3 wave returned **47 ACCEPT / 0 REVISE /
0 REJECT (100%)**. The V3 task is (1) confirm the V2 cycle's dispositions remain
folded in the regenerated V3 dossiers; (2) audit the single V3 residual fold
(`CH1-2F-01-RESIDUAL`, the `alphabet.rs` anchor-precision correction) for any
regression seam; (3) re-ground the four named pre-blocks + the FieldSource fence
+ the deeper REDRESS families (53/88/89, lo6, 6th-shape, D6) against the LIVE
ledger and LIVE source at master `91b6893b0`; (4) confirm V3 introduces no new
regression seam.

## V2 → V3 fold-continuity audit (the gate this cycle must clear)

The V2 wave carried **zero orphan REVISE**; the convergence gate for CH3 had
already been met (≥95% ACCEPT two consecutive cycles: V1 95.7%, V2 100%). V3 must
confirm the V2-folded text persists and that the one V3-introduced residual fold
opens no route.

| V3 fold id | nature | V3 fold state | evidence |
|---|---|---|---|
| **CH1-2F-01-RESIDUAL** | Re-anchor the `StructuralAlphabet` manifest grounding from `alphabet.rs:118` (which is `KernelShape::select`) to `alphabet.rs:19-37` (the struct + rich-alphabet definition). | **FOLDED — verbatim, anchor-precision ONLY.** Folded in 2D (`2d:16,:76,:192,:296,:324`), 2F (`2f:20,:582,:626`), and 2A frontmatter (`2a:11`). | Verified live: `crates/simd-scan/src/alphabet.rs:19` = `pub struct StructuralAlphabet`, fields `singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes` at `:19-37`; `:118` = `impl KernelShape { fn select(alphabet: &StructuralAlphabet) }`. Both anchors true. |

**CH3 disposition of CH1-2F-01-RESIDUAL: NO REGRESSION SEAM.** This is a CH1
provenance/anchor-precision fold, not a route change. The grounded claim — the
shared NEON `select_classifier(alphabet)` / `scan_structural(input,
&StructuralAlphabet)` is a Lock-16 grammar-neutral primitive feeding a
`scan_cost` fact, `substrate_target = existing_tape` (index IS the tape) or
`local_temp_only`, never a `BackendShape`, never a retained substrate, no
cross-call state — is **identical** pre- and post-fold. The fold only sharpens
WHICH line carries the alphabet-as-data manifest. REDRESS-53 (index IS the
tape's offsets), Lock-14 (grammar-neutral), and the no-cross-call-carry fence
are all preserved verbatim. ACCEPT.

## Pre-block ledger re-grounded against LIVE source (master 91b6893b0)

Every CH3-load-bearing on-disk fact about a pre-block fence was re-greped this
cycle — a confabulated on-disk fact is a CH3 REJECT:

| pre-block | live-source re-verification (V3) | state |
|---|---|---|
| **AZ-IV eager** value tree (118×) | `json/builder.rs:9 enum OpenFrame` LIVE; `css_l4/builder.rs:16 OpenFrame` LIVE — the deletion targets exist exactly as the folds name them (`2e:105,:167`). | HELD — fold-DELETION target in all six; never carried |
| **StructRegistry indirection** (28-65×/983×/10583×) | live coupling `crates/core/src/runtime/bbnf/arena.rs:47 match StructRegistry::compound_kind_for_layout(layout)` CONFIRMED at line 47; `crates/ir/src/registry/struct.rs:331 fn layout(&self, rule_id)` CONFIRMED — the refuted runtime walk. | HELD — per-leaf walk REFUTED everywhere; compile-time projection-emission fence |
| **FieldSource fence** | `crates/core/src/runtime/tape/mod.rs:185 fn begin_compound(&mut self, layout: &StructLayout)` takes a PRE-RESOLVED `&StructLayout`; grep-zero `StructRegistry` in the `:180-200` region CONFIRMED; `struct.rs:84 pub enum FieldSource` CONFIRMED. | HELD — clean fence at the exact V2-cited lines |
| **fact-stream** | diagnostic-only; `substrate_target = admitted_fact_output` oracle/comparator per LAC-1E-14; never a live admission plane (V1 REVISE-2B-01 fold persists in V3). | HELD |
| **x86 / AVX-512 / SVE** | `ARCHITECTURE.md:1206 CollapsedStage` gate = `target.arch == x86 + target.avx512bw + Entry(_)` with "aarch64 mechanically refused"; the asmjson FSM host-blocked FIRST. | HELD — refuted as close path in all six |
| **REDRESS-53** parallel retained index | ledger `REDRESS.md:766,:792` ("no precomputed `StructuralIndex`, no retained index") + `:4250`; dossiers classify all 8 `OnceCell<StructuralIndex>` carriers `existing_tape`/`local_temp_only` (`2f:391-394,:412-419,:564`). | HELD |
| **REDRESS-88** PMULL default body | ledger `REDRESS.md:2535` ("PMULL as default hot `bitmap_prefix_xor_64` body is not [admissible]"); 2B-L5 uses `escape_mask_64` `overflowing_add` carry idiom NOT PMULL (`2b:94,:257,:266-267`). | HELD |
| **REDRESS-89** CTZ default body | ledger `REDRESS.md:2542,:2614` (CTZ bulk consumer rejected, scalar default); 2B-L6 scalar running-balance default NOT CTZ (`2b:281,:296-297,:304`). | HELD |
| **lo6 `classify_tbl4` on CSS** | CSS uses eq-set fan NOT lo6 `& 0x3f` slot-59 collision (2A/2B-L1/2C-E/2F-F5). | HELD |
| **sixth BackendShape / D6 second substrate** | 5-variant domain held verbatim (`2d:109,:90`; `2f:267,:275`); 6th shape barred + G-Omega gated; tape = substrate-manifest category (LAC-1E-14) corroborated by `admits_collapsed_stage` x86-bound. | HELD |
| **CH1-V5-001** enumerated-filename residual | `skinny/crates/codegen/src/lower/collapsed_stage.rs` EXISTS; `collapsed_tape.rs` ABSENT in tree. | RESOLVED-ON-DISK |

No on-disk claim in any V3 dossier's pre-block fence is confabulated.

## Disposition method

Each dossier's **§2 fold/candidate enumeration** rows + the
**Architectural-Assertions-Refuted** rows + the **LOCKS-AMENDMENTS-CANDIDATE**
rows were checked against the ledger + live source: does the fold propose,
ground, or even narratively admit a falsified route as viable? A fold that
*names* a pre-block as a fence it preserves is ACCEPT; a fold that *re-opens* one
is REJECT; an under-specified fence against the ledger is REVISE. The single V3
residual fold (`CH1-2F-01-RESIDUAL`) was re-disposed against its folded text and
live source.

## Per-dossier dispositions (V3)

### 2A — sota-landscape (6 folds + 2 refutations + 3 LACs)

- **FOLD-2A-A flat-tape adoption** — ACCEPT. Lock-1 exactly-one-encoding; dual
  AoS/SoA transient only; no D6 second substrate. `live_reverify_at_head: true`
  frontmatter (`2a:11`) re-greps every anchor at HEAD — verified accurate.
- **FOLD-2A-B eager OpenFrame retirement** — ACCEPT. AZ-IV pre-blocked DELETION
  target (`json/builder.rs:9`, `css_l4/builder.rs:16` live); honours pre-block in
  the negative; never carried.
- **FOLD-2A-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; AZ-IV eager-leaf
  shapes (per-leaf Box, f64-alloc-per-number, Box<CssColor>) excluded.
- **FOLD-2A-D tape as substrate-manifest, NOT 6th shape** — ACCEPT. PROPOSE,
  do-not-silently-add (`2a:242-244`); LAC-1E-14 precedent; G-Omega gated.
- **FOLD-2A-E shared NEON classifier (Lock-16)** — ACCEPT. aarch64-only;
  `StructuralAlphabet` at `alphabet.rs:19-37` (CH1-2F-01-RESIDUAL re-anchor folded
  in 2A frontmatter); index IS the tape's offsets, REDRESS-53 fence; only the
  eq-set fan is a proven NEON body, the other two are scalar delegates.
- **FOLD-2A-F StructRegistry/FieldSource fence** — ACCEPT. FieldSource walk
  COMPILE-TIME projection-emission; per-leaf `StructRegistry::layout` re-opens
  28-65×/983×/10583×. Fence held.
- **Refutations 1-2** — ACCEPT (both). CollapsedStage-as-NEON-route refuted
  (UNKNOWN-2D-05); JSON-scanner framing 0-LOC narrative correction (alphabet-
  parametric). No route re-opened.
- **LAC-2A-SKV17-01/02/03** — ACCEPT (all). Substrate-manifest category, NEON
  manifest entry (`alphabet.rs:19-37` anchor), narrative fold. No barred route.

2A: 11/11 ACCEPT. CH1-2F-01-RESIDUAL re-anchor reflected in frontmatter
`live_reverify` line; verified live.

### 2B — primitive-vocabulary (13 folds L1-L9 + refutations + LACs)

- **FOLD-L1 eq-set classifier** — ACCEPT. CSS eq-set fan NOT lo6 `classify_tbl4`.
- **FOLD-L2 tape-append** — ACCEPT. V1 REVISE-2B-01 fact-stream-oracle qualifier
  persists in V3; fact-stream is oracle/comparator only.
- **FOLD-L3 lazy ValueRef<G>** — ACCEPT. FieldSource walk COMPILE-TIME.
- **FOLD-L4 tokenize-once reuse** — ACCEPT. Index IS the tape (Lock 1, REDRESS-53).
- **FOLD-L5 comment_body_mask_64** — ACCEPT. `escape_mask_64` `overflowing_add`
  carry idiom NOT PMULL (REDRESS-88, ledger `:2535`); scalar-ref REQUIRED-NEW
  before wiring (`2b:94,:257,:266-267`).
- **FOLD-L6 bracket_depth_mask_64** — ACCEPT. Scalar running-balance default NOT
  CTZ (REDRESS-89, ledger `:2614`); init-0-per-parse, never retained
  (`2b:281,:296-297,:304`).
- **FOLD-L7 one-shot SIMD capacity** — ACCEPT. Sizes from `input.len()` + scan
  count; no per-corpus literal — FNV/fixture pre-block honoured.
- **FOLD-L8 sparse-flag side-table** — ACCEPT-WITH-GUARD. Each flag bit MUST be a
  `BackendRule` branch-tag projection, NOT a hand-curated catalogue (else
  relocates `W5C_REQUEST_FACT_PROFILES`).
- **FOLD-L9 commit-by-construction (CONDITIONAL)** — ACCEPT. O(1) `offsets.len()`
  checkpoint/`truncate`; no `split_off`/`Vec<Vec>`; gated.
- **Refutations** (FSM/frame-stack macros x86-pinned; multi-arch simd-scan scope-
  narrow not x86-admit; Vec<u32> index not retained sidecar) — ACCEPT (all).
- **LAC-2b-SKV17-01..04** — ACCEPT (all). `bbnf.asm` macro layer scoped to x86
  CollapsedStage spine (host-gated, UNKNOWN-2D-05); no aarch64 x86 close path.

2B: ACCEPT (all). PMULL/CTZ REDRESS fences confirmed against live ledger.

### 2C — grammar-neutrality (7 candidates + refutations + LACs)

- **SK17-2C-A flat offset tape** — ACCEPT. Sparse flags only; a dense class column
  would be the AV.04 second-substrate overfit, barred. No second substrate.
- **SK17-2C-B OpenFrame retirement** — ACCEPT. No-delete-before-replacement fence
  (`2c:133-134`); AZ-IV pre-block (replace, never carry).
- **SK17-2C-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; one tape.
- **SK17-2C-D substrate-manifest, NOT 6th shape** — ACCEPT. "any proposal that
  reads as a 6th `BackendShape` is REJECT" (`2c:197`); LAC-1E-14 precedent +
  `admits_collapsed_stage` x86-bound corroboration; G-Omega gated.
- **SK17-2C-E select_classifier Lock-16** — ACCEPT. No cross-call state; x86/AVX/
  SVE barred; eq-set fan via slot-59 collision; `table_64` aarch64 =
  scalar-delegate-non-ASM (V1 CH2-V1-R4 fold persists).
- **SK17-2C-F FieldSource compile-time fence** — ACCEPT. Per-leaf runtime
  `StructRegistry::layout(rule)` REFUTED (`2c:80,:334-336`); fence held.
- **SK17-2C-ONBOARD** — ACCEPT. A verify_action (CH6-V1-V01 fold persists, live
  HEAD baseline at 91b6893b0); not a fold; no route opened.
- **Refutations 1-4 + self-anchored CH3 statement (`2c:391`)** — ACCEPT (all).
  6th shape / per-leaf walk / eager value tree refuted; D6 inversion guarded;
  x86 barred. Matches the ledger.
- **LAC-SK17-2C-01/02** — ACCEPT (both).

2C: ACCEPT (all).

### 2D — cost-model (7 folds + refutations + LACs)

- **FOLD-2D-01 tape-as-substrate, NOT 6th shape** — ACCEPT. LAC-1E-14 precedent;
  5-variant domain held verbatim (`2d:90,:109`); G-Omega gated.
- **FOLD-2D-02 cost selects INTO one tape** — ACCEPT. e-graph rejects any plan
  whose `substrate_target` is not one of the four admitted values. No parallel
  substrate.
- **FOLD-2D-03 lazy ValueRef<G>** — ACCEPT. AZ-IV eager builders fold-deletion.
- **FOLD-2D-04 AoS↔SoA one-encoding** — ACCEPT. Dual end-state is a Lock-1
  violation; no second substrate.
- **FOLD-2D-05 NEON classifier scan-cost fact** — ACCEPT. **CH1-2F-01-RESIDUAL
  re-anchor folded here** (`2d:16,:76,:192,:296,:324`): the `StructuralAlphabet`
  manifest re-anchored `alphabet.rs:118` → `:19-37` (verified live: `:19` struct,
  `:118` `KernelShape::select`). Index IS the tape; no cross-call state; aarch64
  only. Anchor-precision only — the grounded claim is unchanged; REDRESS-53 fence
  preserved. ACCEPT.
- **FOLD-2D-06 FieldSource compile-time cost-emission** — ACCEPT. Per-leaf
  `StructRegistry::layout(rule)` (`struct.rs:331`) re-opens 28-65×/983×/10583×;
  live tape path fence-clean (`begin_compound` `tape/mod.rs:185`). 0-LOC fence.
- **FOLD-2D-07 aarch64 CollapsedStage stays UNKNOWN-2D-05** — ACCEPT. V1
  REVISE-2D-01 asmjson host-block-FIRST framing persists; `admits_collapsed_stage`
  (`ARCH:1206`) mechanically refuses aarch64. No x86 close path.
- **Refutations** (AVX-512 CollapsedStage does NOT close aarch64; tautological CSP
  carried) + **UNKNOWN-2D-05** (named concrete refuting sources, not a defer-loop)
  — ACCEPT (all).
- **LAC-2D-S17-01/02/03** — ACCEPT (all). Scan-fact LAC cites `alphabet.rs:19-37`
  (re-anchored) + `:118` (`KernelShape::select`).

2D: ACCEPT (all). CH1-2F-01-RESIDUAL anchor-precision fold verified live.

### 2E — host-arch (6 folds + 5 defended + refutations + LACs)

- **FOLD-2E-A flat-tape adoption** — ACCEPT. One encoding; dual transient.
- **FOLD-2E-B eager OpenFrame retirement** — ACCEPT. AZ-IV-pre-blocked
  fold-DELETION; `CssStructBuilder` 817-LOC + `JsonStructBuilder` named; cites
  118× regression. Never carried.
- **FOLD-2E-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast.
- **FOLD-2E-D substrate-manifest, NOT 6th shape** — ACCEPT. aarch64 CollapsedStage
  = UNKNOWN-2D-05; corroborating anchor held.
- **FOLD-2E-E NEON Lock-16 manifest** — ACCEPT. aarch64 NEON only; x86
  avx2/avx512/wasm cfg-gated; "WITHOUT admitting x86 as a close path" (`2e:357-361`).
- **FOLD-2E-F FieldSource compile-time fence** — ACCEPT. Live coupling
  `StructRegistry::compound_kind_for_layout(layout)` (`arena.rs:47`) named as the
  wire FOLD-B severs (`2e:125,:366,:376,:381-382,:460-462`); `begin_compound`
  grep-zero StructRegistry; per-leaf walk re-opens worst regression. Fence held.
- **Refutations 1-4** — ACCEPT (all). 6th shape / dual AoS/SoA / per-leaf walk
  refuted; **"Any x86 / AVX-512 / SVE close path is refuted on the M5 Max aarch64
  target"** (`2e:466`) — the named x86 pre-block refuted explicitly. U-2E-04
  bounded refutation: NEON has no AVX-512-mask branchless-FSM analogue; no defer.
- **LAC-2E-SKV17-01..04** — ACCEPT (all).

2E: ACCEPT (all). The heaviest x86/AVX-512 esoterica surface; every entry
hardware-gated and refuted as a close route.

### 2F — fold-gaps (9 folds F1-F9 + defended + refutations + LACs)

- **F1 eager OpenFrame retirement** — ACCEPT. AZ-IV K-block inviolate.
- **F2 lazy ValueRef<G>** — ACCEPT. No per-leaf `Box::new`; preserve-rich-ast.
- **F3 AoS↔SoA one-encoding** — ACCEPT. §9 second-substrate pre-block named; D6
  inversion guarded. No second substrate.
- **F4 substrate-manifest, NOT 6th shape** — ACCEPT. LAC-1E-14 verbatim +
  `admits_collapsed_stage` x86-bound corroboration (`2f:267,:275`); the verdict
  stands on TWO grounds. D6 + x86 CollapsedStage barred.
- **F5 NEON Lock-16 entry** — ACCEPT. CSS `;{` eq-set fan NOT lo6 `& 0x3f`
  slot-59 collision; no cross-call state; aarch64 only; `KernelShape::select`
  cited at `alphabet.rs:118` (the orthogonal site, correctly distinguished from
  the `:19-37` struct by CH1-2F-01-RESIDUAL); scope-reconcile WITHOUT x86 admit.
- **F6 StructRegistry/FieldSource compile-time fence** — ACCEPT. Per-leaf walk
  REFUTED (`2f:79-80,:323-326`); `struct.rs:84` FieldSource enum + `:331`
  `layout(rule_id)` barred as runtime walk; `begin_compound(&StructLayout)`
  pre-resolved. Fence held.
- **F7 OnceCell<StructuralIndex> substrate_target classification** — ACCEPT. "a
  retained parallel index collapses into REDRESS-53" (`2f:412-419,:564,:601`);
  must be `existing_tape`/`local_temp_only`, never retained parallel; all-8-
  carrier census. REDRESS-53 fence held.
- **F8 BackendShape selector wiring** — ACCEPT. 4 skinny lowerers are 17-LOC
  scaffolds (the CH1-V5-001-folded enumerated form); fail-closed on e-graph cap /
  CSP timeout / stale cost evidence. No silent shape; no broadcast/stale.
- **F9 Lock-2 StructLayout rename** — ACCEPT. Generator-side identifier change;
  no route opened.
- **Refutations 1-3 + self-anchored CH3 statement (`2f:601`)** — ACCEPT (all).
  6th shape NOT added; FieldSource walk NOT per-leaf; AoS/SoA dual NOT permissible.
- **LAC-2F-FOLD-01..05** — ACCEPT (all). **LAC-2F-FOLD-03 (`2f:582`) carries the
  CH1-2F-01-RESIDUAL re-anchor** (`alphabet.rs:118` → `:19-37` for the
  StructuralAlphabet manifest; `:118` = `KernelShape::select`, correctly retained
  in the F5 body). Verified live; anchor-precision only; substrate_target =
  existing_tape; no REDRESS route touched.

2F: ACCEPT (all). CH1-2F-01-RESIDUAL re-anchor verified live and regression-clean.

## Cross-cutting CH3 findings (V3)

1. **The V2 dispositions persist; the single V3 residual fold opens no route.**
   `CH1-2F-01-RESIDUAL` is a CH1 anchor-precision correction (`alphabet.rs:118` →
   `:19-37` for the `StructuralAlphabet` manifest). Verified live at HEAD:
   `:19` = `pub struct StructuralAlphabet`, fields at `:19-37`; `:118` =
   `KernelShape::select`. The grounded claim — shared NEON classifier is a
   Lock-16 grammar-neutral primitive, `substrate_target = existing_tape`/
   `local_temp_only`, never a shape, never a retained substrate — is identical
   pre/post-fold. REDRESS-53 + Lock-14 + no-cross-call-carry preserved verbatim.

2. **The four named pre-blocks all hold, in every dossier, grounded to live
   source.**
   - **AZ-IV eager** — fold-DELETION target in all six; `json/builder.rs:9
     OpenFrame` + `css_l4/builder.rs:16 OpenFrame` exist live as the named
     deletion targets; no-delete-before-replacement fence (2C-B) guards order.
   - **StructRegistry indirection** — fenced compile-time projection-emission in
     all six; per-leaf runtime walk REFUTED (28-65×/983×/10583×); live coupling
     `arena.rs:47 StructRegistry::compound_kind_for_layout(layout)` named as the
     seam the fold severs.
   - **fact-stream** — diagnostic oracle/comparator only (V1 REVISE-2B-01 fold
     persists); never a live materialization plane.
   - **x86 / AVX-512 / SVE** — refuted as a close path in all six; `ARCH:1206`
     CollapsedStage gate is `target.arch == x86 + avx512bw` with "aarch64
     mechanically refused"; asmjson host-blocked FIRST (V1 REVISE-2D-01 persists);
     U-2E-04 bounded refutation (NEON has no AVX-512-mask FSM analogue).

3. **The FieldSource fence holds — re-grounded to live source this cycle.**
   `begin_compound(&StructLayout)` (`tape/mod.rs:185`) takes a pre-resolved
   `&StructLayout`, grep-zero `StructRegistry` in the `:180-200` region; the
   `FieldSource` enum (`struct.rs:84`) is compile-time projection-emission; the
   live indirection `StructRegistry::compound_kind_for_layout` (`arena.rs:47`) is
   the eager-arena wire FOLD-B deletes. No fold drives a per-leaf live
   `StructRegistry::layout(rule)` (`struct.rs:331`).

4. **Deeper REDRESS families honoured.** REDRESS-53 (index IS the tape's offsets,
   ledger `:766,:792,:4250`) uniform across 2A-E/2B-L4/2D-05/2E/2F-F7; REDRESS-88
   (2B-L5 `overflowing_add` NOT PMULL, ledger `:2535`); REDRESS-89 (2B-L6 scalar
   running-balance NOT CTZ, ledger `:2614`); lo6 `classify_tbl4` negated (CSS
   eq-set fan); broadcast/FNV/W5C-relocation fenced; D6 second substrate / 6th
   BackendShape refuted via LAC-1E-14 + `admits_collapsed_stage` x86-bound.

5. **CH1-V5-001 (first-hygiene action) remains RESOLVED on disk.**
   `collapsed_stage.rs` exists; `collapsed_tape.rs` absent. The dossiers record it
   resolved; no carry, no confabulation.

6. **D6 monotonic-inversion guard.** Every dossier states the monotonic direction
   skinny-proven → crates/core and forbids relocating core into skinny (2C:391,
   2E, 2F:601). The fold cannot invert into a second skinny-side substrate.

## Verdict

**PASS — 48 ACCEPT / 0 REVISE / 0 REJECT across 48 disposed sections
(100% ACCEPT).**

**Zero re-opened REDRESS routes. Zero AZ-IV / StructRegistry / fact-stream / x86
re-entry. The FieldSource fence holds in all six dossiers and grounds to live
source (`tape/mod.rs:185`, `arena.rs:47`, `struct.rs:84,:331`).** The single V3
residual fold (`CH1-2F-01-RESIDUAL`, the `alphabet.rs:118` → `:19-37` anchor
precision) is verified live (`:19` = `StructuralAlphabet` struct, `:118` =
`KernelShape::select`) and touches no REDRESS route — the grounded Lock-16
classifier claim, `substrate_target = existing_tape`, and the REDRESS-53 fence
are preserved verbatim.

This is the **third consecutive PASS-grade CH3** in the T-P2 pass (V1 95.7%, V2
100%, V3 100%) and the fifth consecutive across T-P1/T-P2. The convergence
criterion on the CH3 axis (≥95% ACCEPT for two consecutive cycles) is met with
margin; the V3 wave introduces no regression seam and carries zero orphan REVISE.

## Disposition register (for the aggregator)

| id | dossier | section | disposition | fix |
|---|---|---|---|---|
| CH3-2A-V3 | 2a | 6 folds + 2 refut + 3 LAC | ACCEPT (11/11) | — |
| CH3-2B-V3 | 2b | 9 folds L1-L9 + refut + LAC | ACCEPT (all) | — |
| CH3-2C-V3 | 2c | 7 cand + 4 refut + 2 LAC | ACCEPT (all) | — |
| CH3-2D-V3 | 2d | 7 folds + refut + 3 LAC | ACCEPT (all) | CH1-2F-01-RESIDUAL anchor at 2d:16,:76,:192,:296,:324 verified live |
| CH3-2E-V3 | 2e | 6 folds + 5 def + 4 refut + 4 LAC | ACCEPT (all) | — |
| CH3-2F-V3 | 2f | 9 folds F1-F9 + refut + 5 LAC | ACCEPT (all) | CH1-2F-01-RESIDUAL anchor at 2f:20,:582,:626 verified live |
| V3-RESIDUAL-FOLD | 2a,2d,2f | CH1-2F-01-RESIDUAL | NO REGRESSION SEAM | anchor-precision only; alphabet.rs:19-37 struct / :118 KernelShape::select verified; REDRESS-53 + Lock-14 preserved |
