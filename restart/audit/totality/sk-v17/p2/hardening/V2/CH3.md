---
lens: CH3-REGRESSION
pass: T-P2-research
cycle: V2
reviewer: CH3 REGRESSION (V2)
generated_at: 2026-05-29T00:00:00Z
master_head: 91b6893b0
t_p1_locked_commit: 91b6893b0
t_p1_source_sha: 445925167
subjects_reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
v1_revises_folded: [REVISE-2B-01, REVISE-2D-01]
pre_blocks_audited: [AZ-IV-eager, StructRegistry-indirection, FieldSource-fence, fact-stream, x86-AVX512-SVE, CollapsedStage-asmjson-FSM, broadcast-24-row, FNV-fixture, REDRESS-53-parallel-index, REDRESS-88-PMULL-default, REDRESS-89-CTZ-default, lo6-classify_tbl4-CSS, sixth-BackendShape, D6-second-substrate, W5C-array-relocation, orphan-udot-i8mm, CH1-V5-001-enumerated-filename]
sections_disposed: 47
accept: 47
revise: 0
reject: 0
verdict: PASS (100% ACCEPT)
---

# CH3 REGRESSION — T-P2 SK-V17 V2

## Mandate

Per PASS-2-RESEARCH §3 (CH3 REGRESSION) + ORCHESTRATOR §3W: scan the six
T-P2 dossiers (`p2/2{a..f}-*.md`) so that **no fold re-opens a route already
refuted in the skinny REDRESS ledger** (`skinny/REDRESS.md`) or pre-blocked by
SK-V17 SPEC §9. Dispatch focus: the four named pre-blocks — **AZ-IV eager**,
**StructRegistry indirection**, **fact-stream**, **x86** — plus the
**FieldSource fence** holding the AZ-IV indirection pre-blocked. A "promising"
research direction REDRESS already falsified is a REJECT.

V2 is a fold-verification cycle. The V1 CH3 wave returned **45 ACCEPT / 2 REVISE
/ 0 REJECT (95.7%)**; both REVISEs were non-load-bearing provenance/framing
hardenings of routes the dossiers *already* correctly refute. The V2 task is (1)
confirm both V1 REVISEs are folded by the regenerated V2 dossiers; (2)
re-verify the four named pre-blocks + the FieldSource fence + the deeper REDRESS
families against the live ledger and live source; (3) confirm the V2 cycle
introduces no new regression seam.

## V1 disposition fold-audit (the gate this cycle must clear)

| V1 id | V1-prescribed fix | V2 fold state | evidence |
|---|---|---|---|
| **REVISE-2B-01** | qualify `2b:183` checkasm parity to "tape ↔ **diagnostic** fact-stream corpus-parity (fact-stream as oracle/comparator only, `substrate_target = admitted_fact_output` per LAC-1E-14 `LOCKS.md:102-105`; NOT a live admission plane, `SPEC.md:799-800`)" | **FOLDED — verbatim.** `2b:196-198` now reads "checkasm: tape ↔ **diagnostic** fact-stream corpus-parity (the fact-stream as oracle/comparator ONLY — `substrate_target = admitted_fact_output` per LAC-1E-14, `LOCKS.md:102-105`; the diagnostic …)". Frontmatter `2b:17` records the fold id `REVISE-2B-01-FOLD-L2-checkasm-diagnostic-fact-stream-oracle-comparator-only`. | `2b:17,:196-198` |
| **REVISE-2D-01** | prepend SPEC `:854` barred-candidate cite to the asmjson grounding row so the host-block frames it BEFORE the Lemire-2023 literature cite | **FOLDED — verbatim.** `2d:74` (row `T2D17-AARCH64-COLLAPSEDSTAGE-UNKNOWN-2D-05`) now opens "**PRIMARY FRAME**: asmjson is host-blocked per the SPEC §9 barred-candidate list — 'asmjson collapsed-stage FSM (x86, host-blocked)' (`SPEC.md:851-852`, in §9 `:782` the route ledger): a barred, refuted x86-only route, never a latent aarch64 candidate. The literature cite is diagnostic x86 architecture-pressure ONLY: asmjson AVX-512 (Lemire 2023 ICPP), Sneller …". The host-block is now the load-bearing frame. Frontmatter `2d:15-16` records the fold. | `2d:15-16,:74,:242-243` |

Both V1 REVISEs are folded exactly as prescribed; **zero orphan REVISE carried
into V2.** (Note the SPEC barred-candidate line moved from `:854` in the V1
disposition wording to `:851-852` in the V2 fold — the dossier cites the live
line; CH1 owns the exact-line provenance. Either way the route is framed
host-blocked-first, which is the CH3-load-bearing property.)

## Pre-block ledger reconstructed (the do-not-redrive surface)

The CH3 surface is the SK-V17 §9 "Global blocks" + per-wave attributions + the
inherited REDRESS families, each cross-checked against the live ledger AND live
source at master `91b6893b0`:

| pre-block | binding evidence | V2 confirmation |
|---|---|---|
| AZ-IV eager value tree (118×) | `SPEC.md:791`; `css_l4/builder.rs`,`json/builder.rs` | fold-DELETION target in all six; `CssStructBuilder` 817-LOC god-module named as deletion target (`2e:105,:167,:447`); no carry-forward |
| StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65×/983×/10583×) | `SPEC.md:793-795`; `struct.rs:84,313,331`; live coupling `arena.rs:47` | per-leaf runtime `StructRegistry::layout(rule)` REFUTED everywhere; the live wire `arena.rs:47 StructRegistry::compound_kind_for_layout(layout)` named as the seam FOLD-B severs (`2e:83,:328`, `2f:23`) |
| CSS fact-stream String as live admission plane | `SPEC.md:799-800` | diagnostic-only; fact-stream admitted as **oracle/comparator** (`admitted_fact_output`), NEVER a live materialization plane (`2b:196-198`, the V1-REVISE fold) |
| W5C_REQUEST_FACT_PROFILES array (Lock-14-phrase-#1) | `SPEC.md:801-803` | RETIRE not relocate; sparse-flag side-table must be a `BackendRule` branch-tag projection, NOT a hand-curated catalogue (2b-L8 guard) |
| 24-row broadcast | `SPEC.md:804-805` | cost model fail-closes on stale/broadcast evidence (2d) |
| FNV / fixture contrivances | `SPEC.md:806-808` | 2b-L7 sizes capacity from `input.len()` + scan count, no per-corpus literal |
| x86 / AVX-512 / SVE | `SPEC.md:806`,`:851-852` | refuted as a close path in all six; CollapsedStage x86-pinned, aarch64 mechanically refused (`admits_collapsed_stage` x86-bound) |
| REDRESS-53 parallel retained index | `REDRESS.md:784-810` | "index IS the tape's offsets" identity uniform (`2a:262,:267`, `2b:89`, `2d:189`, `2f:391,:410,:562,:599`) |
| REDRESS-88 PMULL default body | `REDRESS.md:2510-2540` | 2b-L5 uses `overflowing_add` carry idiom NOT PMULL (`2b:85,:258`) |
| REDRESS-89 CTZ default body | `REDRESS.md:2542-2595` | 2b-L6 scalar running-balance default NOT CTZ (`2b:85,:276`) |
| lo6 `classify_tbl4` on the CSS alphabet | `SPEC.md:854`,`:316` | CSS uses eq-set fan NOT lo6 `& 0x3f` slot-59 collision (2a/2b-L1/2f) |
| sixth BackendShape / D6 second substrate | `SPEC.md:808` | refuted via LAC-1E-14 substrate-manifest precedent + the independent `admits_collapsed_stage` x86-bound corroboration; G-Omega gated |
| CH1-V5-001 enumerated-filename residual (T-P1 carry) | dispatch first-hygiene action | RESOLVED-ON-DISK, verified this cycle (see below) |

## On-disk verification (live source at master 91b6893b0)

Every CH3-load-bearing claim was re-grounded against live source this cycle — a
confabulated on-disk fact about a pre-block fence is a CH3 REJECT:

1. **FieldSource fence-clean `begin_compound`.** `crates/core/src/runtime/tape/mod.rs:185`
   — `fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle` takes
   a PRE-RESOLVED `&StructLayout`; grep-zero `StructRegistry` inside. Confirmed.
2. **Live coupling-site (the seam FOLD-B/F severs).**
   `crates/core/src/runtime/bbnf/arena.rs:47` —
   `match StructRegistry::compound_kind_for_layout(layout)`. Exists exactly as the
   dossiers name it (`2e:83,:328,:388`; `2f:23` CH5-V1-003). This is the eager-arena
   wire FOLD-B deletes; the dossiers name the EXACT live indirection they sever,
   which strengthens (not weakens) the fence grounding.
3. **FieldSource enum + layout accessor.** `crates/ir/src/registry/struct.rs:84`
   (`pub enum FieldSource`), `:331` (`fn layout(&self, rule_id: RuleId)`), `:337`
   (`fn layout_by_name`). All match the cited lines; the per-leaf
   `StructRegistry::layout(rule)` (`:331`) is the refuted runtime walk.
4. **CH1-V5-001 enumerated-filename hygiene (first dispatch action).** Verified:
   `skinny/crates/codegen/src/lower/collapsed_stage.rs` EXISTS;
   `collapsed_tape.rs` ABSENT (no match anywhere in tree);
   `grep -c ',collapsed}'` in {1a,1b,1e} = **0/0/0**;
   `grep -c 'collapsed_stage}.rs'` in 1b = **3**. The defect lived only in 1b and
   was already corrected at master; no 1a/1e fold required. The residual T-P1
   REVISE is genuinely discharged on disk — NOT carried, NOT confabulated. The
   dossier assertions (`2a:30`, `2e:32,:92-95`, `2f:27,:42`) are accurate.

No on-disk claim in any dossier's pre-block fence is confabulated.

## Disposition method

Each dossier's **§2 fold-candidate enumeration** rows + the
**Architectural-Assertions-Refuted** rows + the **LOCKS-AMENDMENTS-CANDIDATE**
rows were checked against the ledger + live source: does the fold propose,
ground, or even narratively admit a falsified route as viable? A fold that
*names* a pre-block as a fence it preserves is ACCEPT; a fold that *re-opens* one
is REJECT; a fold whose fence wording is *under-specified* against the ledger is
REVISE. The two V1 REVISE sites were re-disposed against their folded text.

## Per-dossier dispositions

### 2A — sota-landscape (8 rows + 6 folds + 2 refutations + 3 LACs)

- **FOLD-2A-A flat-tape adoption** — ACCEPT. Lock-1 exactly-one-encoding; dual
  AoS/SoA transient only. No D6 second substrate.
- **FOLD-2A-B eager OpenFrame retirement** — ACCEPT. AZ-IV pre-blocked DELETION
  target; names the clone pathology Lock 1 forbids; honours the pre-block in the
  negative.
- **FOLD-2A-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; no per-leaf
  `Box::new`, no f64-alloc-per-number, no per-color `Box<CssColor>` — the AZ-IV
  eager-leaf shapes excluded.
- **FOLD-2A-D tape as substrate-manifest, NOT 6th shape** — ACCEPT. PROPOSE,
  do-not-silently-add; LAC-1E-14 precedent; sixth shape G-Omega gated.
- **FOLD-2A-E shared NEON classifier (Lock-16 entry)** — ACCEPT. aarch64-only;
  the multi-arch `crates/simd-scan` (neon/avx2/avx512/wasm) is a fold-SCOPE
  reconcile "narrow-to-aarch64 vs retain, NOT an x86 close path" (`2a:292,:397`);
  index becomes the tape's offsets `substrate_target=existing_tape`, REDRESS-53
  fence (`2a:262,:267`); only the eq-set fan is a proven NEON body, the other
  two primitives are live scalar delegates (no over-admission — the CH4-2a-001
  fold). No retained cross-call classifier state.
- **FOLD-2A-F StructRegistry/FieldSource fence** — ACCEPT. FieldSource walk
  COMPILE-TIME projection-emission; per-leaf `StructRegistry::layout` re-opens
  28-65×/983×/10583×. The fence the dispatch names, held.
- **Refutation 1 — CollapsedStage-as-NEON-route** — ACCEPT. aarch64-NEON not
  routed through CollapsedStage; UNKNOWN-2D-05; x86 close path refused.
- **Refutation 2 — JSON-scanner framing** — ACCEPT. 0-LOC narrative correction;
  classifier alphabet-parametric; no route re-opened.
- **LAC-2A-SKV17-01/02/03** — ACCEPT (all). Substrate-manifest category, NEON
  manifest entry, narrative fold. No barred route admitted.

2A: 0 REJECT, 0 REVISE. CH1-V5-001 confirmed resolved-on-disk in the frontmatter.

### 2B — primitive-vocabulary (11 rows + 9 folds L1-L9 + 6 refutations + 4 LACs)

- **FOLD-L1 eq-set classifier** — ACCEPT. CSS uses eq-set fan NOT lo6
  `classify_tbl4` — the SPEC barred route negated.
- **FOLD-L2 tape-append** — ACCEPT. **REVISE-2B-01 FOLDED.** The checkasm parity
  now reads "tape ↔ **diagnostic** fact-stream corpus-parity (the fact-stream as
  oracle/comparator ONLY — `substrate_target = admitted_fact_output` per
  LAC-1E-14, `LOCKS.md:102-105`)" (`2b:196-198`). The fact-stream is now
  unambiguously the oracle/comparator, NOT a live admission plane; the cosmetic
  re-admission risk V1 flagged is closed. No StructRegistry indirection; retires
  the eager OpenFrame/fact-stream plane.
- **FOLD-L3 lazy ValueRef<G>** — ACCEPT. FieldSource walk COMPILE-TIME, never
  per-leaf.
- **FOLD-L4 tokenize-once reuse** — ACCEPT. Index IS the tape (Lock 1); the
  classifier's `Vec<u32>` output IS the tape's offsets, NOT a parallel retained
  vector — REDRESS-53 cited (`2b:89,:236-239`).
- **FOLD-L5 comment_body_mask_64** — ACCEPT. Uses `escape_mask_64`
  `overflowing_add` carry idiom NOT PMULL (REDRESS-88) (`2b:85,:258`); ledger
  `REDRESS.md:2535` confirms. REQUIRED-NEW scalar-ref + checkasm before wiring.
- **FOLD-L6 bracket_depth_mask_64** — ACCEPT. Scalar running-balance default NOT
  CTZ (REDRESS-89) (`2b:85,:276`); ledger `REDRESS.md:2614` confirms; init-0-
  per-parse, NEVER retained across calls.
- **FOLD-L7 one-shot SIMD capacity** — ACCEPT. Sizes from `input.len()` + scan
  count; no per-corpus capacity literal — FNV/fixture pre-block honoured.
- **FOLD-L8 sparse-flag side-table** — ACCEPT-WITH-GUARD. Each flag bit MUST be a
  `BackendRule` branch-tag projection, NOT a hand-curated catalogue — else it
  relocates `W5C_REQUEST_FACT_PROFILES` into flag form. The pre-block named as
  the failure mode the guard prevents.
- **FOLD-L9 commit-by-construction (CONDITIONAL)** — ACCEPT. Rides D3 O(1)
  `offsets.len()` checkpoint/`truncate` rollback; no `split_off`/`Vec<Vec>`; gated
  on a post-CF-1 re-profile, not asserted active.
- **Refutation: FSM/frame-stack macros** — ACCEPT. `CollapsedStage` spine,
  x86-pinned + aarch64-refused (`ARCHITECTURE.md:1206`); close-state
  `source-present-unwired`/`architectural-block-with-REDRESS`. No aarch64
  admission.
- **Refutation: multi-arch simd-scan as Layer 1** — ACCEPT. The fold adopts the
  PROVEN aarch64-narrow `PrimitiveKernels`; the multi-arch kernels are a
  scope-narrowing decision NOT an x86/avx512 close-path admission (`2b:152`).
- **Refutation: Vec<u32> index as retained sidecar** — ACCEPT. Index IS the
  tape's offsets, REDRESS-53 cited.
- **LAC-2b-SKV17-01..04** — ACCEPT (all). Layer-1 reconcile, Layer-0 re-anchor
  (`x86inc.asm` lives at the skinny path, totality path ABSENT — re-anchor not
  re-admit), scalar-delegate close-state, L1-L9 manifest carry. The `bbnf.asm`
  macro layer scoped to the x86 CollapsedStage spine only (host-gated,
  UNKNOWN-2D-05) — no aarch64 x86 close path.

2B: 0 REJECT, 0 REVISE. (V1 REVISE-2B-01 folded.)

### 2C — grammar-neutrality (8 rows + 7 candidates + 4 refutations + 2 LACs)

- **SK17-2C-A flat offset tape** — ACCEPT. Sparse flags only; a kind-partitioned
  dense class column would be the AV.04 second-substrate overfit, barred. No
  second substrate.
- **SK17-2C-B OpenFrame retirement** — ACCEPT. **No-delete-before-replacement
  fence** (`2c:133-134`): the same-wave replacement (tape consumer) must be
  proven BEFORE deletion; AZ-IV pre-block (replace, never carry).
- **SK17-2C-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; one tape, no second
  value tree.
- **SK17-2C-D substrate-manifest, NOT 6th shape** — ACCEPT. "any proposal that
  reads as a 6th `BackendShape` is REJECT" (`2c:187,:326-328`); the LAC-1E-14
  `FactStream` precedent (plane-table ordinal (3) ≠ substrate-manifest 5th, both
  distinct, neither a shape); independently corroborated by `ARCH:1206`
  CollapsedStage x86-only ⇒ aarch64 needs no 6th-shape route. G-Omega gated.
- **SK17-2C-E select_classifier Lock-16** — ACCEPT. No retained cross-call
  classifier state; x86/AVX/SVE barred, aarch64 only.
- **SK17-2C-F FieldSource compile-time fence** — ACCEPT. Per-leaf runtime
  `StructRegistry::layout(rule)` is REFUTED (28-65×/983×/10583×, `2c:80,:334-336`);
  grammar-neutral ONLY as compile-time projection-emission. The fence, held.
- **SK17-2C-ONBOARD future-grammar onboarding** — ACCEPT. A verification gate,
  not a fold; no route opened.
- **Refutations 1-4** — ACCEPT (all). 6th shape refuted; per-leaf walk refuted;
  fleet-wide claim scoped to JSON+CSS; eager value tree refuted (AZ-IV shape).
  Each matches the ledger.
- **Self-anchored CH3 statement** (`2c:391`) — ACCEPT. D6 second-substrate
  inversion guarded (monotonic skinny→core); x86 barred. Confirmed accurate.
- **LAC-SK17-2C-01/02** — ACCEPT (both). Substrate-manifest category; NEON
  manifest row.

2C: 0 REJECT, 0 REVISE.

### 2D — cost-model (7 rows + 7 folds + 4 refutations + 3 LACs)

- **FOLD-2D-01 tape-as-substrate, NOT 6th shape** — ACCEPT. LAC-1E-14 precedent;
  5-variant domain `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` held
  verbatim (`2d:90`); 6th shape G-Omega gated.
- **FOLD-2D-02 cost selects INTO one tape** — ACCEPT. e-graph rejects any plan
  whose `substrate_target` is not one of the four admitted values; a shape
  selecting a NEW substrate is the Lock-1 violation the CSP must reject; fences
  the prior `csp_named_grammars` tautology. No parallel substrate.
- **FOLD-2D-03 lazy ValueRef<G>** — ACCEPT. AZ-IV eager builders are
  fold-deletion, never carry-forward; FieldSource walk compile-time (FOLD-2D-06
  fence).
- **FOLD-2D-04 AoS↔SoA one-encoding** — ACCEPT. Dual end-state is a Lock-1
  violation; no second substrate.
- **FOLD-2D-05 NEON classifier scan-cost fact** — ACCEPT. Index IS the tape; NO
  cross-call classifier-state retention; aarch64 only, no x86/AVX/SVE.
- **FOLD-2D-06 FieldSource compile-time cost-emission** — ACCEPT. Per-leaf
  `StructRegistry::layout(rule)` (`struct.rs:331`) re-opens 28-65×/983×/10583×
  (`2d:73,:205-213,:262`). The LIVE tape path is fence-clean today
  (`begin_compound` reads `layout.rule_id & 0x1F` only, `tape/mod.rs:185-186`).
  Fence held; 0-LOC fence, CRITICAL if violated.
- **FOLD-2D-07 aarch64 CollapsedStage stays UNKNOWN-2D-05** — ACCEPT.
  **REVISE-2D-01 FOLDED.** The grounding row `2d:74` and FOLD body `2d:242-243`
  now open with "**PRIMARY FRAME**: asmjson is host-blocked per the SPEC §9
  barred-candidate list" BEFORE the Lemire-2023 literature cite. The host-block
  is the load-bearing frame; no T-P3 reader can treat asmjson as a latent aarch64
  candidate. `admits_collapsed_stage` mechanically refuses aarch64. AVX-512
  literature is x86 architecture-pressure ONLY. No x86 close path.
- **Refutation: AVX-512 CollapsedStage closes aarch64** — ACCEPT. asmjson +
  Sneller refuted as aarch64-incapable; the most regression-load-bearing
  refutation in the pass, correct against the ledger.
- **Refutation: zero-rule e-graph / tautological CSP** — ACCEPT. Carried forward;
  the inert decision engine named, not paper-closed.
- **UNKNOWN-2D-05 (carried)** — ACCEPT. NOT a defer-loop (CH6-V1-R01 reconciled):
  the `verify_action` names concrete refuting sources (Arm A64 ISA FSM-dispatch,
  Lemire 2026 `svmatch_u8`, Validark 2024; 2E U-2E-04 NEON-has-no-mask-FSM-analogue)
  — it does not punt to "a future cycle". CollapsedStage stays a 5-shape candidate
  with x86 diagnostic evidence only; aarch64 admission mechanically refused.
- **LAC-2D-S17-01/02/03** — ACCEPT (all). Substrate-manifest carrier;
  substrate-target gate; NEON scan-fact (no cross-call state).

2D: 0 REJECT, 0 REVISE. (V1 REVISE-2D-01 folded.)

### 2E — host-arch (10 rows + 6 folds + 5 defended + 4 refutations + 4 LACs)

- **FOLD-2E-A flat-tape adoption** — ACCEPT. One encoding; dual transient.
- **FOLD-2E-B eager OpenFrame retirement** — ACCEPT. AZ-IV-pre-blocked
  fold-DELETION target (`2e:105,:167-176`); `CssStructBuilder` 817-LOC god-module
  + JSON `JsonStructBuilder` 231-LOC named as deletion targets; cites the
  1.83ms→215.7ms (118×) regression. Never carried.
- **FOLD-2E-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; CH2 firewall (JSON
  byte-equal re-emit).
- **FOLD-2E-D substrate-manifest, NOT 6th shape** — ACCEPT. "propose, do NOT
  silently add a 6th"; aarch64 CollapsedStage = UNKNOWN-2D-05; NEON under the four
  LLVM shapes' scan-leaf FFI.
- **FOLD-2E-E NEON Lock-16 manifest** — ACCEPT. aarch64 NEON only; x86
  avx2/avx512/wasm cfg-gated non-aarch64; no x86 admission (`2e:243-244`);
  scope-reconcile WITHOUT admitting x86 as a close path.
- **FOLD-2E-F FieldSource compile-time fence** — ACCEPT. The live coupling
  `StructRegistry::compound_kind_for_layout(layout)` (`arena.rs:47`) is the wire
  FOLD-B deletion severs (`2e:83,:328,:388,:457`); `begin_compound` is grep-zero
  `StructRegistry` (`2e:333-334`); a naive per-leaf `StructRegistry::layout(rule)`
  re-opens 28-65×/983×/10583× (`2e:339-340`). "the AZ-IV/StructRegistry pre-blocks
  are not re-openable" (`2e:351`). Fence held — and strengthened by naming the
  exact live seam.
- **Refutations 1-4** — ACCEPT (all). 6th shape refuted; dual AoS/SoA refuted;
  per-leaf walk refuted; **"Any x86 / AVX-512 / SVE close path is refuted on the
  M5 Max aarch64 target"** — the named x86 pre-block, refuted explicitly;
  CollapsedStage mechanically refused.
- **Fold Coherence Note (CH5 pre-empt)** — ACCEPT, CH3-relevant: the
  `StructuralIndex` mask stream is a transient producer (`local_temp_only`), never
  a retained sidecar; `OnceCell<StructuralIndex>` becomes the tape's offsets or
  `local_temp_only` — REDRESS-53 pre-empted.
- **LAC-2E-SKV17-01..04** — ACCEPT (all). Substrate-manifest precedent; NEON
  manifest rows (aarch64 only); single ValueRef<G> plane; AoS/SoA closure.

2E: 0 REJECT, 0 REVISE. (The heaviest x86/AVX-512 esoterica surface; cleanest on
CH3 — every esoterica entry hardware-gated and refuted as a close route. The
CH5-V1-003 fold naming the live `arena.rs:47` wire is a net CH3 *improvement*.)

### 2F — fold-gaps (8 rows + 9 folds F1-F9 + 4 defended + 3 refutations + 5 LACs)

- **F1 eager OpenFrame retirement** — ACCEPT. AZ-IV K-block inviolate; deletes
  the eager builder, does not extend it.
- **F2 lazy ValueRef<G>** — ACCEPT. AZ-IV K-block (no per-leaf `Box::new`);
  preserve-rich-ast.
- **F3 AoS↔SoA one-encoding** — ACCEPT. §9 second-substrate pre-block named; the
  fold adopts the PROVEN skinny `Tape`/`ValueRef` INTO core, never relocating core
  into skinny (D6 inversion guarded). No second substrate.
- **F4 substrate-manifest, NOT 6th shape** — ACCEPT. LAC-1E-14 verbatim; "the
  dispatch's 'propose, do NOT silently add a 6th' discharged in the negative";
  CH6-V1-V03 fold adds the independent corroborating anchor —
  `admits_collapsed_stage` x86-bound (`ARCH:1151/:1282`) mechanically refuses on
  aarch64 ⇒ no 6th-shape mechanism; the verdict stands on TWO grounds (`2f:26`).
  D6 second substrate + x86 CollapsedStage barred.
- **F5 NEON Lock-16 entry** — ACCEPT. CSS `;{` uses eq-set fan NOT lo6 `& 0x3f`
  slot-59 collision; no cross-call retained classifier state; aarch64 only;
  scope-reconcile WITHOUT admitting x86 as a close path.
- **F6 StructRegistry/FieldSource compile-time fence** — ACCEPT. Per-leaf walk
  REFUTED (28-65×/983×/10583×, `2f:79-80,:323-326`); the live `struct.rs:84`
  FieldSource enum + `:331` `layout(rule_id)` named as the runtime walk barred;
  `begin_compound(&StructLayout)` pre-resolved. The fence the dispatch names.
- **F7 OnceCell<StructuralIndex> substrate_target classification** — ACCEPT. "a
  retained parallel index collapses into REDRESS-53" (`2f:410,:414-415,:562`);
  must be `existing_tape` (index IS the tape) or `local_temp_only`, never a
  retained parallel index; all-8-carrier census (the COH-014 4-grammar undercount
  corrected). REDRESS-53 fence held.
- **F8 BackendShape selector wiring** — ACCEPT. The 4 skinny lowerers are 17-LOC
  scaffolds (`{eager_tape,offset_tape,event_tape,collapsed_stage}.rs` — the
  CH1-V5-001-folded enumerated form, `2f:439-440`); `substrate_target` binding on
  every `BackendExpr`; fail-closed on e-graph cap / CSP timeout / stale cost
  evidence. No silent shape; no broadcast/stale evidence.
- **F9 Lock-2 StructLayout rename** — ACCEPT. Generator-side identifier change; no
  route opened.
- **Refutations 1-3** — ACCEPT (all). 6th shape NOT added; FieldSource walk NOT
  per-leaf; AoS/SoA dual NOT a permissible closure. Each matches the ledger.
- **Self-anchored CH3 statement** (`2f:599,:621`) — ACCEPT. F1 honours AZ-IV; F6
  keeps StructRegistry indirection pre-blocked; F4 keeps D6 + x86 CollapsedStage
  barred; mask stream stays a transient producer (no retained parallel index,
  REDRESS-53 pre-blocked). CH1-V5-001 folded. Confirmed accurate.
- **LAC-2F-FOLD-01..05** — ACCEPT (all). One-substrate closure; substrate-manifest
  category; NEON manifest row (no x86 close path); no-per-leaf-registry fence;
  Lock-2 rename re-price.

2F: 0 REJECT, 0 REVISE.

## Cross-cutting CH3 findings

1. **Both V1 REVISEs are folded; zero orphan REVISE.** REVISE-2B-01 (fact-stream
   parity-oracle qualifier) and REVISE-2D-01 (asmjson host-block framing
   precedence) are folded verbatim into the regenerated V2 dossiers. Neither ever
   re-opened a falsified route; both were wording/framing hardenings of routes the
   dossiers already correctly refuted. The fold-audit gate is clear.

2. **The dispatch's four named pre-blocks all hold, in every dossier.**
   - **AZ-IV eager** — fold-DELETION target in all six (2A-B, 2C-B, 2D-03, 2E-B,
     2F-F1/F2); the `CssStructBuilder` 817-LOC god-module + `JsonStructBuilder`
     named as deletion targets; the no-delete-before-replacement fence (2C-B)
     guards the order. None extends or carries the eager tree.
   - **StructRegistry indirection** — fenced as compile-time projection-emission
     in all six (2A-F, 2B-L3, 2C-F, 2D-06, 2E-F, 2F-F6); the per-leaf runtime walk
     REFUTED with the 28-65×/983×/10583× citation everywhere; the V2 dossiers now
     name the EXACT live coupling-site `arena.rs:47` the fold severs.
   - **fact-stream** — diagnostic-only; admitted as oracle/comparator
     (`admitted_fact_output`), NEVER a live materialization plane; the V1 cosmetic
     risk at 2b:183 is closed by REVISE-2B-01.
   - **x86 / AVX-512 / SVE** — refuted as a close path in all six; CollapsedStage
     x86-only / UNKNOWN-2D-05, mechanically refused on aarch64; the asmjson FSM is
     host-blocked FIRST (REVISE-2D-01 framing fold).

3. **The FieldSource fence holds — and is grounded against live source this
   cycle.** `begin_compound(&StructLayout)` (`tape/mod.rs:185`) reads
   `layout.rule_id & 0x1F` only, grep-zero `StructRegistry`; the `FieldSource`
   walk (`struct.rs:84`) is compile-time projection-emission resolved once at
   codegen; the live indirection `StructRegistry::compound_kind_for_layout`
   (`arena.rs:47`) is the eager-arena wire FOLD-B deletes. No fold drives a
   per-leaf live `StructRegistry::layout(rule)` (`struct.rs:331`).

4. **Deeper REDRESS families honoured (beyond the four named).** REDRESS-53
   (index IS the tape's offsets) uniform across 2A-E/2B-L4/2D-05/2E-coherence/2F-F7;
   REDRESS-88 (2B-L5 `overflowing_add` NOT PMULL, ledger `:2535`); REDRESS-89
   (2B-L6 scalar running-balance NOT CTZ, ledger `:2614`); lo6 `classify_tbl4`
   negated (CSS eq-set fan); broadcast/FNV/W5C-relocation fenced; D6 second
   substrate / 6th BackendShape refuted via LAC-1E-14 + the independent
   `admits_collapsed_stage` x86-bound corroboration (the CH6-V1-V03 two-grounds
   fold).

5. **CH1-V5-001 (the first-hygiene action) is RESOLVED on disk, not carried.**
   `collapsed_stage.rs` exists; `collapsed_tape.rs` absent; `grep ',collapsed}'`
   {1a,1b,1e} = 0; `'collapsed_stage}.rs'` 1b = 3. Verified against live tree;
   no confabulation. The dossiers correctly record it as resolved.

6. **D6 monotonic-inversion guard.** Every dossier states the monotonic direction
   skinny-proven → crates/core and forbids relocating core constructs into skinny
   (2C:391, 2E:367, 2F:215). The fold cannot invert into a second skinny-side
   substrate.

## Verdict

**PASS — 47 ACCEPT / 0 REVISE / 0 REJECT across 47 disposed sections
(100% ACCEPT).**

**Zero re-opened REDRESS routes. Zero AZ-IV / StructRegistry / fact-stream / x86
re-entry. The FieldSource fence holds in all six dossiers and grounds to live
source (`tape/mod.rs:185`, `arena.rs:47`, `struct.rs:84,:331`).** Both V1 REVISEs
are folded verbatim; zero orphan REVISE. The V2 dossiers improve the CH3 surface
over V1 by naming the exact live `arena.rs:47` coupling-site the fold severs
(CH5-V1-003 fold) and by carrying the independent `admits_collapsed_stage`
x86-bound corroboration for the 6th-shape refutation (CH6-V1-V03 fold).

This is the second consecutive PASS-grade CH3 in the T-P2 pass (V1 95.7%, V2
100%) and the fourth consecutive across T-P1/T-P2; the V2 wave introduces no
regression seam and clears the V1 REVISE set with no residual. The convergence
criterion (≥95% ACCEPT two consecutive cycles) is met on the CH3 axis.

## Disposition register (for the aggregator)

| id | dossier | section | disposition | fix |
|---|---|---|---|---|
| CH3-2A-V2 | 2a | 8 rows + 6 folds + 2 refut + 3 LAC | ACCEPT (19/19) | — |
| CH3-2B-V2 | 2b | 11 rows + 9 folds + 6 refut + 4 LAC | ACCEPT (30/30) | REVISE-2B-01 folded at 2b:196-198 |
| CH3-2C-V2 | 2c | 8 rows + 7 cand + 4 refut + 2 LAC | ACCEPT (21/21) | — |
| CH3-2D-V2 | 2d | 7 rows + 7 folds + 4 refut + 3 LAC | ACCEPT (21/21) | REVISE-2D-01 folded at 2d:74,:242-243 |
| CH3-2E-V2 | 2e | 10 rows + 6 folds + 4 refut + 4 LAC | ACCEPT (24/24) | — |
| CH3-2F-V2 | 2f | 8 rows + 9 folds F1-F9 + 3 refut + 5 LAC | ACCEPT (25/25) | — |
| V1-FOLD-AUDIT | 2b,2d | REVISE-2B-01, REVISE-2D-01 | BOTH FOLDED | zero orphan REVISE carried into V2 |
