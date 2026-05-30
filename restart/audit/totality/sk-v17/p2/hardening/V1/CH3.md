---
lens: CH3-REGRESSION
pass: T-P2-research
cycle: V1
reviewer: CH3 REGRESSION (V1)
generated_at: 2026-05-29T00:00:00Z
master_head: 91b6893b0
t_p1_locked_commit: 91b6893b0
t_p1_source_sha: 445925167
subjects_reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
pre_blocks_audited: [AZ-IV-eager, StructRegistry-indirection, FieldSource-fence, fact-stream, x86-AVX512-SVE, CollapsedStage-asmjson-FSM, broadcast-24-row, FNV-fixture, REDRESS-53-parallel-index, REDRESS-88-PMULL-default, REDRESS-89-CTZ-default, lo6-classify_tbl4-CSS, sixth-BackendShape, D6-second-substrate, W5C-array-relocation, orphan-udot-i8mm]
sections_disposed: 47
accept: 45
revise: 2
reject: 0
verdict: PASS (95.7% ACCEPT)
---

# CH3 REGRESSION — T-P2 SK-V17 V1

## Mandate

Per PASS-2-RESEARCH §3 (CH3 REGRESSION) + ORCHESTRATOR §3W: scan the six
T-P2 dossiers (`p2/2{a..f}-*.md`) so that **no fold re-opens a route already
refuted in the skinny REDRESS ledger** (`skinny/REDRESS.md`) or pre-blocked by
SK-V17 SPEC §9 (`restart/skinny/tranches/sk-v17/SPEC.md:785-857`). Focus per
dispatch: the four named pre-blocks — **AZ-IV eager**, **StructRegistry
indirection**, **fact-stream**, **x86** — plus the **FieldSource fence**. A
"promising" research direction REDRESS already falsified is a REJECT.

## Pre-block ledger reconstructed (the do-not-redrive surface)

The CH3 surface is the SK-V17 §9 "Global blocks" (`SPEC.md:789-811`) +
per-wave attributions (`:824-840`) + the inherited REDRESS families, each
cross-checked against the live ledger:

| pre-block | binding evidence | live ledger confirmation |
|---|---|---|
| AZ-IV eager value tree (118×) | `SPEC.md:791`; `css_l4/builder.rs:71-79`,`json/builder.rs:9` | eager-decode masking ledger `REDRESS.md:131,165,191,200,205,1075-1076`; "eager parse-time string decode is too expensive" `:965,:1155` |
| StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65×/983×/10583×) | `SPEC.md:793-795`; `struct.rs:84,313,331`; `tape/mod.rs:185-186` | `SPEC.md:824` W1 row; T-P1 CH3 do-not-redrive ledger `1e:133-149`, `1a:120,133` |
| CSS fact-stream String as live admission plane | `SPEC.md:799-800` (`emit_fact_stream`/`generated.rs:5`) | diagnostic-only; not a substrate |
| W5C_REQUEST_FACT_PROFILES array (Lock-14-phrase-#1) | `SPEC.md:801-803` | RETIRE not relocate into flag/projection DATA `:836-839` |
| 24-row broadcast | `SPEC.md:804-805` (RESULTS 112-135) | pre-blocked |
| FNV / fixture contrivances | `SPEC.md:806-808` | FNV bench-only; `REDRESS.md:1972-1974` per-corpus byte-dispatch rejected |
| x86 / AVX-512 / SVE | `SPEC.md:806`,`:854` ("asmjson collapsed-stage FSM (x86, host-blocked)") | aarch64 only; Apple cores no SVE |
| REDRESS-53 parallel retained index | `REDRESS.md:784-810` | "structural projection must be the parser's single substrate, not a second scanner" `:806-810` |
| REDRESS-88 PMULL default body | `REDRESS.md:2510-2540` | "PMULL as the default hot `bitmap_prefix_xor_64` body is not admissible" `:2535` |
| REDRESS-89 CTZ default body | `REDRESS.md:2542-2595` | W10b CTZ bulk consumer rejected; prefix-XOR stays scalar `:2614` |
| lo6 `classify_tbl4` on the CSS alphabet | `SPEC.md:854`,`:316` | `;{`→slot-59 `& 0x3f` collision; CSS uses eq-set fan |
| sixth BackendShape / D6 second substrate | `SPEC.md:808`,`:854` | G-Omega gated; LAC-1E-14 substrate-manifest is the home |

## Disposition method

Each dossier's **§2 fold-candidate enumeration** rows + the
**Architectural-Assertions-Refuted** rows + the **LOCKS-AMENDMENTS-CANDIDATE**
rows were checked against the ledger above: does the fold propose, ground, or
even narratively admit a falsified route as viable? A fold that *names* a
pre-block as a fence it preserves is ACCEPT; a fold that *re-opens* one is
REJECT; a fold whose fence wording is *under-specified* against the ledger is
REVISE.

## Per-dossier dispositions

### 2A — sota-landscape (8 grounding rows + 6 folds + 2 refutations + 3 LACs)

- **FOLD-2A-A flat-tape adoption** — ACCEPT. Names Lock-1 exactly-one-encoding;
  dual AoS/SoA is a transient fold-state only (`2a:101-103`). No second
  substrate; D6 not re-opened.
- **FOLD-2A-B eager OpenFrame retirement** — ACCEPT. Treats the eager builders
  as the **AZ-IV-pre-blocked DELETION target** (`2a:112-117,:125,:129`), never
  a carry-forward. Names the `Vec<OpenFrame>::clone` 86.07% pathology Lock 1
  forbids. Honours the pre-block in the negative.
- **FOLD-2A-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; no per-leaf
  `Box::new`, no f64-alloc-per-number, no per-color `Box<CssColor>` (`2a:155`)
  — the exact AZ-IV eager-leaf shapes, explicitly excluded.
- **FOLD-2A-D tape as substrate-manifest, NOT 6th shape** — ACCEPT. PROPOSE,
  do-not-silently-add (`2a:185-189,:207`); LAC-1E-14 precedent; sixth shape
  G-Omega gated. No silent shape.
- **FOLD-2A-E shared NEON classifier (Lock-16 entry)** — ACCEPT. aarch64-only;
  x86 kernels in `crates/simd-scan` are "architecture-pressure, not a close
  path" (`2a:245`); index IS the tape's offsets, not a parallel retained vector
  — REDRESS-53 fence cited (`2a:216`); no cross-call classifier state
  (`2a:226`).
- **FOLD-2A-F StructRegistry/FieldSource fence** — ACCEPT. FieldSource walk
  COMPILE-TIME projection-emission; per-leaf `StructRegistry::layout` re-opens
  28-65×/983×/10583× (`2a:259-262`). The fence the dispatch names, held.
- **Refutation 1 — CollapsedStage-as-NEON-route** — ACCEPT. Refutes routing
  aarch64-NEON through `CollapsedStage`; keeps it the spec-named UNKNOWN-2D-05,
  mechanically refused on aarch64 (`admits_collapsed_stage` co-requires x86,
  `2a:309-316`). No x86 close path.
- **Refutation 2 — JSON-scanner framing** — ACCEPT. 0-LOC narrative correction;
  classifier already alphabet-parametric; no route re-opened.
- **LAC-2A-SKV17-01/02/03** — ACCEPT (all three). Refinements; substrate-manifest
  category (not 6th shape), NEON manifest entry (aarch64, no cross-call state),
  narrative fold. No barred route admitted.

2A: 0 REJECT, 0 REVISE.

### 2B — primitive-vocabulary (11 rows + 9 folds L1-L9 + 6 refutations + 4 LACs)

- **FOLD-L1 eq-set classifier** — ACCEPT. CSS uses eq-set fan **NOT** lo6
  `classify_tbl4` (`2b:73,:158`) — the SPEC `:854`/`:316` barred route
  explicitly negated.
- **FOLD-L2 tape-append** — ACCEPT. "no StructRegistry indirection"
  (`2b:174`); retires the eager OpenFrame/fact-stream plane; per-leaf hot path
  must NOT re-enter `StructRegistry::layout` (`2b:178`).
- **FOLD-L3 lazy ValueRef<G>** — ACCEPT. FieldSource walk COMPILE-TIME, NEVER
  per-leaf (`2b:200-201`).
- **FOLD-L4 tokenize-once reuse** — ACCEPT. Index IS the tape (Lock 1), never a
  retained parallel vector — **REDRESS-53** cited verbatim (`2b:208-211`).
- **FOLD-L5 comment_body_mask_64** — ACCEPT. Uses `escape_mask_64`
  `overflowing_add` carry idiom **NOT PMULL (REDRESS-88)** (`2b:75,:227`).
  Ledger `REDRESS.md:2535` confirms PMULL default body is rejected. REQUIRED-NEW
  scalar-ref + checkasm before wiring.
- **FOLD-L6 bracket_depth_mask_64** — ACCEPT. Scalar running-balance default
  **NOT CTZ (REDRESS-89)** (`2b:75,:243-244`). Ledger `REDRESS.md:2542,:2614`
  confirms. Sees only masks (no literal bracket bytes); within-call carry, no
  cross-call retention.
- **FOLD-L7 one-shot SIMD capacity** — ACCEPT. Sizes from `input.len()` + scan
  count, "no per-corpus capacity literal" (`2b:249,:258`) — the FNV/fixture
  pre-block (`SPEC.md:806-808`) honoured.
- **FOLD-L8 sparse-flag side-table** — ACCEPT-WITH-GUARD (no regression). Each
  flag bit MUST be a `BackendRule` branch-tag projection, **NOT** a hand-curated
  per-rule catalogue — "else it relocates `W5C_REQUEST_FACT_PROFILES` into flag
  form" (`2b:267`). The W5C-relocation pre-block (`SPEC.md:801-803`) is named as
  the failure mode the guard prevents. Clean.
- **FOLD-L9 commit-by-construction Alt-mode (CONDITIONAL)** — ACCEPT. Rides D3's
  O(1) `offsets.len()` checkpoint / `truncate` rollback — "no `split_off`, no
  `Vec<Vec>`" (`2b:278`) — the W4 pre-block (`SPEC.md:835`) honoured. Gated on a
  post-CF-1 re-profile (the SPEC L9 §6 condition), not asserted active.
- **Refutation: FSM/frame-stack macros** — ACCEPT. Files them as
  `CollapsedStage` spine, **x86/AVX-512-pinned, mechanically refused on
  aarch64** (`2b:78,:145`). No aarch64 admission.
- **Refutation: udot/i8mm digit MAC** — ACCEPT. "no benched CSS antecedent"
  (`2b:146`) — the SPEC `:855` orphan-udot bar honoured; admissible only with a
  profiled same-wave consumer.
- **Refutation: Vec<u32> index as retained sidecar** — ACCEPT. Index IS the
  tape's offsets, REDRESS-53 cited (`2b:79`).
- **LAC-2b-SKV17-01..04** — ACCEPT (all). Layer-1-medium reconcile, Layer-0
  re-anchor, scalar-delegate close-state, L1-L9 manifest carry. The
  `bbnf.asm`-macro layer scoped to the x86 CollapsedStage spine only
  (host-gated, UNKNOWN-2D-05) — no aarch64 x86 close path.

  **REVISE-2B-01 (non-load-bearing wording).** `2b:183-184` cites the checkasm
  parity reference as "tape↔fact-stream corpus-parity". The fact-stream is a
  SPEC `:799-800` global-block construct (diagnostic-only, never a live
  admission plane). Using it as the *oracle* for a tape-parity differential is
  legitimate (the diagnostic fact-stream is precisely the comparator/oracle the
  LAC-1E-14 manifest admits, `admitted_fact_output`), but the bare phrase risks
  reading as re-admitting the fact-stream into the live materialization path.
  **Concrete fix:** at `2b:183` qualify to "tape ↔ **diagnostic** fact-stream
  corpus-parity (fact-stream as oracle/comparator only, `substrate_target =
  admitted_fact_output` per LAC-1E-14 `LOCKS.md:102-105`; NOT a live admission
  plane, `SPEC.md:799-800`)". No route re-opened; wording hardening only.

2B: 0 REJECT, 1 REVISE (REVISE-2B-01, cosmetic provenance qualifier).

### 2C — grammar-neutrality (8 rows + 7 candidates + 4 refutations + 2 LACs)

- **SK17-2C-A flat offset tape** — ACCEPT. Sparse flags only; a kind-partitioned
  dense class column would be "the AV.04 overfit, barred" (`2c:105-108`) —
  cites the buried AV.04 second-substrate bar (`LOCKS.md:784`). No second
  substrate.
- **SK17-2C-B OpenFrame retirement** — ACCEPT. No-delete-before-replacement
  fence (`2c:129-130`); AZ-IV pre-block (replace, never carry).
- **SK17-2C-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; one tape, no
  second value tree.
- **SK17-2C-D substrate-manifest, NOT 6th shape** — ACCEPT. "any proposal that
  reads as a 6th `BackendShape` is REJECT" (`2c:176`); G-Omega gated.
- **SK17-2C-E select_classifier Lock-16** — ACCEPT. No retained cross-call
  classifier state (Lock 1 v+1 ELEVATION, `2c:201-202`); x86/AVX/SVE barred,
  aarch64 only.
- **SK17-2C-F FieldSource compile-time fence** — ACCEPT. Per-leaf runtime
  `StructRegistry::layout`/`Arena<G>`/`Builder<G>` is REJECT (`2c:223-224`) —
  re-opens the worst measured regression. The fence, held.
- **SK17-2C-ONBOARD future-grammar onboarding** — ACCEPT. A verification gate,
  not a fold; no route opened.
- **Refutations 1-4** — ACCEPT (all). 6th shape refuted; per-leaf walk refuted;
  fleet-wide claim refuted (scoped to JSON+CSS); eager value tree refuted (AZ-IV
  shape). Each matches the ledger's actual position.
- **Self-anchored CH3 statement** (`2c:327-329`) — ACCEPT. Explicitly states no
  fold re-opens AZ-IV / StructRegistry / fact-stream / x86; D6 second-substrate
  inversion guarded (monotonic skinny→core). Confirmed accurate.
- **LAC-SK17-2C-01/02** — ACCEPT (both). Substrate-manifest category; NEON
  manifest row (no cross-call state, x86/AVX/SVE barred).

2C: 0 REJECT, 0 REVISE.

### 2D — cost-model (7 rows + 7 folds + 4 refutations + 3 LACs)

- **FOLD-2D-01 tape-as-substrate, NOT 6th shape** — ACCEPT. LAC-1E-14
  precedent; 5-variant domain held verbatim; 6th shape G-Omega gated
  (`2d:97-102`).
- **FOLD-2D-02 cost selects INTO one tape** — ACCEPT. e-graph rejects any plan
  whose `substrate_target` is not one of the four admitted values
  (`2d:110-112`); a shape selecting a NEW substrate is the Lock-1 violation the
  CSP must reject. No parallel substrate. Also fences the prior-2D
  `csp_named_grammars` tautology (grammar-named facts forbidden, `2d:120-121`).
- **FOLD-2D-03 lazy ValueRef<G>** — ACCEPT. AZ-IV eager builders are
  fold-deletion, never carry-forward (`2d:151-152`); FieldSource walk
  compile-time (FOLD-2D-06 fence).
- **FOLD-2D-04 AoS↔SoA one-encoding** — ACCEPT. Dual end-state is a Lock-1
  violation (`2d:161-162`); no second substrate.
- **FOLD-2D-05 NEON classifier scan-cost fact** — ACCEPT. Index IS the tape;
  NO cross-call classifier-state retention —
  `retention_lifetime=retained-across-call-boundary` is "the REJECT class"
  (`2d:196-197`); aarch64 only, no x86/AVX/SVE.
- **FOLD-2D-06 FieldSource compile-time cost-emission** — ACCEPT. Per-leaf
  `StructRegistry::layout` re-opens 28-65×/983×/10583× (`2d:206-207`). Fence held.
- **FOLD-2D-07 aarch64 CollapsedStage stays UNKNOWN-2D-05** — ACCEPT.
  `admits_collapsed_stage` mechanically refuses aarch64; **REDRESS: "asmjson
  collapsed-stage FSM (x86, host-blocked)" barred, SPEC `:854`** cited verbatim
  (`2d:231`). AVX-512 literature is x86 architecture-pressure ONLY. No x86 close
  path.
- **Refutation: AVX-512 CollapsedStage closes aarch64** — ACCEPT. asmjson +
  Sneller refuted as aarch64-incapable (`2d:258`); cited as architecture-pressure
  diagnostic only. This is the most regression-load-bearing refutation in the
  pass and it is correct against the ledger.
- **Refutation: zero-rule e-graph / tautological CSP proves selection** —
  ACCEPT. Carries the prior-2D refutation forward (`2d:257`); the inert decision
  engine is named, not paper-closed. No regression.
- **LAC-2D-S17-01/02/03** — ACCEPT (all). Substrate-manifest carrier;
  substrate-target gate; NEON scan-fact (no cross-call state).

  **REVISE-2D-01 (provenance precision).** `2d:69` and the source index cite
  "asmjson AVX-512 (Lemire 2023 ICPP)". CH1 (CORRECTNESS) owns citation
  provenance, but CH3 flags the *regression-adjacent* risk: the dossier grounds
  asmjson **only to refute it** on aarch64, which is the correct CH3 posture —
  however the SPEC barred-list phrase is "asmjson collapsed-stage FSM (x86,
  host-blocked)" and the dossier should make the host-block the *primary*
  framing wherever asmjson appears, not the secondary. **Concrete fix:** at
  `2d:69` (Technique Grounding row `T2D17-AARCH64-COLLAPSEDSTAGE`) prepend the
  SPEC `:854` barred-candidate citation so the asmjson row reads "host-blocked
  per SPEC §9 barred-candidate list (`:854`) — diagnostic x86 pressure only,
  never an aarch64 admission" BEFORE the literature citation. This makes the
  pre-block the load-bearing frame, foreclosing any T-P3 reader treating asmjson
  as a latent aarch64 candidate. (Cross-lens: CH1 should verify the Lemire-2023
  ICPP citation independently.)

2D: 0 REJECT, 1 REVISE (REVISE-2D-01, framing-precedence of an already-refuted
x86 route).

### 2E — host-arch (10 rows + 6 folds + 5 defended + 4 refutations + 4 LACs)

- **FOLD-2E-A flat-tape adoption** — ACCEPT. One encoding; dual is transient
  (`2e:110-111`).
- **FOLD-2E-B eager OpenFrame retirement** — ACCEPT. AZ-IV-pre-blocked
  fold-DELETION target, never carried (`2e:137-139`); cites the 118× canada
  regression.
- **FOLD-2E-C lazy ValueRef<G>** — ACCEPT. preserve-rich-ast; CH2 firewall
  (JSON byte-equal re-emit).
- **FOLD-2E-D substrate-manifest, NOT 6th shape** — ACCEPT. "propose, do NOT
  silently add a 6th" (`2e:194`); aarch64 CollapsedStage = UNKNOWN-2D-05; NEON
  under the four LLVM shapes' scan-leaf FFI.
- **FOLD-2E-E NEON Lock-16 manifest** — ACCEPT. aarch64 NEON only; "x86
  avx2/avx512/wasm cfg-gated non-aarch64; no x86 admission, SPEC `:806`"
  (`2e:243-244`); scope-reconcile WITHOUT admitting x86 as a close path.
- **FOLD-2E-F FieldSource compile-time fence** — ACCEPT. Per-leaf
  `StructRegistry::layout` re-opens 28-65×/983×/10583× (`2e:261-264`); "the
  AZ-IV/StructRegistry pre-blocks are not re-openable" (`2e:274-275`). Fence held.
- **Refutations 1-4** — ACCEPT (all). 6th shape refuted; dual AoS/SoA refuted;
  per-leaf walk refuted; **"Any x86 / AVX-512 / SVE close path is refuted on the
  M5 Max aarch64 target"** (`2e:325-329`) — the named x86 pre-block, refuted
  explicitly; CollapsedStage mechanically refused. Each matches the ledger.
- **Fold Coherence Note (CH5 pre-empt)** (`2e:356-368`) — ACCEPT, CH3-relevant:
  the `StructuralIndex` mask stream is a transient producer (`local_temp_only`),
  never a retained sidecar; the `OnceCell<StructuralIndex>` becomes the tape's
  offsets or `local_temp_only` — REDRESS-53 pre-empted.
- **LAC-2E-SKV17-01..04** — ACCEPT (all). Substrate-manifest precedent; NEON
  manifest rows (aarch64 only); single ValueRef<G> plane; AoS/SoA closure.

2E: 0 REJECT, 0 REVISE. (The dossier with the heaviest x86/AVX-512 esoterica
surface is the cleanest on CH3 — every esoterica entry is hardware-gated and
refuted as a close route.)

### 2F — fold-gaps (8 rows + 9 folds F1-F9 + 4 defended + 3 refutations + 5 LACs)

- **F1 eager OpenFrame retirement** — ACCEPT. AZ-IV K-block inviolate
  (`2f:142-143`); deletes the eager builder, does not extend it.
- **F2 lazy ValueRef<G>** — ACCEPT. AZ-IV K-block (no per-leaf `Box::new`,
  `2f:179`); preserve-rich-ast.
- **F3 AoS↔SoA one-encoding** — ACCEPT. §9 second-substrate pre-block named
  (`2f:215-216`); the fold adopts the PROVEN skinny `Tape`/`ValueRef` INTO core,
  never relocating core constructs into skinny (D6 inversion guarded). No second
  substrate.
- **F4 substrate-manifest, NOT 6th shape** — ACCEPT. LAC-1E-14 verbatim; "the
  dispatch's 'propose, do NOT silently add a 6th' discharged in the negative"
  (`2f:240-241,:253-255`); D6 second substrate + x86 CollapsedStage barred.
- **F5 NEON Lock-16 entry** — ACCEPT. CSS `;{` uses eq-set fan NOT lo6
  `& 0x3f` slot-59 collision (`2f:282-283`); no cross-call retained classifier
  state; aarch64 only; scope-reconcile WITHOUT admitting x86 as a close path.
- **F6 StructRegistry/FieldSource compile-time fence** — ACCEPT. Per-leaf walk
  REFUTED (28-65×/983×/10583×, `2f:323-326`). The fence the dispatch names.
- **F7 OnceCell<StructuralIndex> substrate_target classification** — ACCEPT.
  "a retained parallel index collapses into **REDRESS-53**" (`2f:357-358`);
  must be `existing_tape` (index IS the tape) or `local_temp_only`, never a
  retained parallel index. REDRESS-53 fence held; all-8-carrier census (the
  COH-014 4-grammar undercount corrected).
- **F8 BackendShape selector wiring** — ACCEPT. The 4 skinny lowerers are
  17-LOC scaffolds (`{eager_tape,offset_tape,event_tape,collapsed_stage}.rs` —
  the CH1-V5-001-folded enumerated form); `substrate_target` binding on every
  `BackendExpr` node; fail-closed on e-graph cap / CSP timeout / stale cost
  evidence (`2f:393-397`). No silent shape; no broadcast/stale evidence.
- **F9 Lock-2 StructLayout rename** — ACCEPT. Generator-side identifier change;
  no route opened.
- **Refutations 1-3** — ACCEPT (all). 6th shape NOT added; FieldSource walk NOT
  per-leaf; AoS/SoA dual NOT a permissible closure. Each matches the ledger.
- **Self-anchored CH3 statement** (`2f:498-501`) — ACCEPT. F1 honours AZ-IV
  (deletes, does not extend); F6 keeps StructRegistry indirection pre-blocked;
  F4 keeps D6 second substrate + x86 CollapsedStage barred. Confirmed accurate.
- **LAC-2F-FOLD-01..05** — ACCEPT (all). One-substrate closure; substrate-manifest
  category; NEON manifest row (no x86 close path); no-per-leaf-registry fence;
  Lock-2 rename re-price.

2F: 0 REJECT, 0 REVISE.

## Cross-cutting CH3 findings

1. **The dispatch's four named pre-blocks all hold, in every dossier.**
   - **AZ-IV eager** is treated as the fold-DELETION target by all six (2A-B,
     2C-B, 2D-03, 2E-B, 2F-F1/F2); none extends or carries the eager tree.
   - **StructRegistry indirection** is fenced as compile-time projection-emission
     in all six (2A-F, 2B-L3, 2C-F, 2D-06, 2E-F, 2F-F6); the per-leaf runtime
     walk is REFUTED with the 28-65×/983×/10583× citation everywhere.
   - **fact-stream** is correctly diagnostic-only; never re-admitted as a live
     materialization plane (one cosmetic wording risk at 2B:183 — REVISE-2B-01).
   - **x86 / AVX-512 / SVE** is refuted as a close path in all six; CollapsedStage
     kept x86-only / UNKNOWN-2D-05, mechanically refused on aarch64; the asmjson
     FSM is barred (one framing-precedence improvement at 2D:69 — REVISE-2D-01).

2. **The FieldSource fence holds.** `begin_compound(&StructLayout)` reads
   `layout.rule_id & 0x1F` only; the `FieldSource{TypedLeaf, BranchTag,
   SeqPosition, RepeatElement, RuleReference}` walk is compile-time
   projection-emission resolved once at codegen across every dossier's value-API
   fold. No fold drives a per-leaf live `StructRegistry::layout`.

3. **Deeper REDRESS families correctly honoured (beyond the four named):**
   - **REDRESS-53** (parallel retained index) — fenced in 2A-E, 2B-L4, 2D-05,
     2E-coherence, 2F-F7. The "index IS the tape's offsets" identity is the
     load-bearing fence and it is stated uniformly.
   - **REDRESS-88** (PMULL default body) — 2B-L5 uses the `overflowing_add`
     carry idiom explicitly NOT PMULL; ledger `REDRESS.md:2535` confirms the
     rejection. Correct.
   - **REDRESS-89** (CTZ default body) — 2B-L6 uses scalar running-balance
     default explicitly NOT CTZ; ledger `REDRESS.md:2614` confirms. Correct.
   - **lo6 `classify_tbl4` on CSS** — negated in 2A/2B/2F (CSS uses eq-set fan).
   - **broadcast / FNV / W5C-relocation** — 2D fail-closes on broadcast/stale
     evidence; 2B-L7 bars per-corpus capacity literals; 2B-L8 names W5C-into-flag
     relocation as the failure mode the branch-tag guard prevents.
   - **D6 second substrate / 6th BackendShape** — refuted in all six via the
     LAC-1E-14 substrate-manifest precedent; none silently adds a 6th shape.

4. **D6 monotonic-inversion guard.** Every dossier states the monotonic
   direction skinny-proven → crates/core and forbids relocating core constructs
   into skinny (2C:329, 2E:367-368, 2F:215-216). The fold cannot invert into a
   second skinny-side substrate.

## Verdict

**PASS — 45 ACCEPT / 2 REVISE / 0 REJECT across 47 disposed sections
(95.7% ACCEPT).**

**Zero re-opened REDRESS routes. Zero AZ-IV / StructRegistry / fact-stream /
x86 re-entry. The FieldSource fence holds in all six dossiers.** Both REVISEs
are non-load-bearing provenance/framing hardenings of routes the dossiers
*already* correctly refute (the fact-stream parity-oracle qualifier; the asmjson
host-block framing-precedence) — neither re-opens a falsified route, neither
blocks convergence. No orphan REVISE: each carries a concrete file:line fix.

This is the third consecutive 100%-on-substance CH3 (T-P1 V4/V5/V6 all 100%);
the V1 T-P2 wave introduces no regression seam. The two REVISEs are recorded for
the V2 author fold and do not gate the pass.

## Disposition register (for the aggregator)

| id | dossier | section | disposition | fix |
|---|---|---|---|---|
| CH3-2A-* | 2a | 8 rows + 6 folds + 2 refut + 3 LAC | ACCEPT (19/19) | — |
| CH3-2B-* | 2b | 11 rows + 9 folds + 6 refut + 4 LAC | ACCEPT (29/30) | — |
| REVISE-2B-01 | 2b | FOLD-L2 checkasm `2b:183` | REVISE | qualify "tape↔**diagnostic** fact-stream corpus-parity (oracle only, `admitted_fact_output`; NOT a live admission plane, SPEC `:799-800`)" |
| CH3-2C-* | 2c | 8 rows + 7 cand + 4 refut + 2 LAC | ACCEPT (21/21) | — |
| CH3-2D-* | 2d | 7 rows + 7 folds + 4 refut + 3 LAC | ACCEPT (20/21) | — |
| REVISE-2D-01 | 2d | grounding row `2d:69` | REVISE | prepend SPEC `:854` barred-candidate cite so the host-block frames the asmjson row BEFORE the Lemire-2023 literature cite |
| CH3-2E-* | 2e | 10 rows + 6 folds + 4 refut + 4 LAC | ACCEPT (24/24) | — |
| CH3-2F-* | 2f | 8 rows + 9 folds F1-F9 + 3 refut + 5 LAC | ACCEPT (25/25) | — |
