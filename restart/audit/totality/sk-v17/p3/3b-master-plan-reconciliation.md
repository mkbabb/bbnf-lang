---
agent: 3B
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-30T00:04:02Z
revised_at: 2026-05-31T00:00:00Z
master_head: 2a76916ac
t_p2_locked_sha: 2a76916ac
t_p1_excavation_sha: 445925167154de73540e3ea3283d0170371de790
t_p2_consolidated_sha: 91b6893b0
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: MASTER-PLAN.md
proposed_deltas_count: 9
delta_summary:
  carried_from_prior_cycle: [MP-3B-SKV17-D01, MP-3B-SKV17-D02, MP-3B-SKV17-D03, MP-3B-SKV17-D04, MP-3B-SKV17-D05, MP-3B-SKV17-D06, MP-3B-SKV17-D07, MP-3B-SKV17-D09]
  removed: []
  answered: [MP-3B-SKV17-D08]
  newly_added: []
prior_cycle_dispositions_folded:
  accepted:
    - CH4-ACCEPT-3B-same-wave-consumer   # 3B SK-V18 receiver rows each carry same-wave consumer; W0 pre-gate gated to co-waved W1 (CH4.md:75-97)
    - CH7-ACCEPT-16-lock-5-shape         # 16-lock count + 5-shape canon preserved verbatim across 3B
    - CH4-02-FOLDED                      # V2 D07 wired-target sizing (crates/egraph 1885 / crates/csp-solver 5882) re-verified, stable; no further revision
  rejected: []
  revised:
    - CH4-V2-01   # D08 row (MP.SK18.W1 receiver, 3b:140): eager-OpenFrame retirement propagation priced '22+ files' — V1 CH4-01 corrected this to 40 in 3A but the fold was not propagated to 3B's parallel W1 wave row. FOLDED V3: replaced with 'blast radius = 40 files via grep -rl JsonStructBuilder|CssStructBuilder crates/ (=40, master HEAD 2a76916ac)'; LOC band 300-700 + HIGH risk unaffected (re-verified grep -rl ... crates/ = 40)
---

## Executive Summary

`restart/MASTER-PLAN.md`'s active dispatch block is §13.5 SK-V15 W0-W11
(`restart/MASTER-PLAN.md:889-949`) — the PRUNE-then-REBUILD CSS-honesty repair.
SK-V17 sits *downstream* of SK-V15: it is the skinny tranche that EMPIRICALLY
PROVES the unified-tape / lazy-`ValueRef<G>` / NEON model in skinny, and the
five LOCKED T-P2 fold designs (LAC-2F-FOLD-01..05) crystallise those proven
wins into the **SK-V18 crates/core adoption waves** the MASTER plan must now
direct. SK-V17 is the proving engine (skinny W0-W5,
`restart/skinny/tranches/sk-v17/SPEC.md:264-269`); SK-V18 is the totality fold
that adopts the proven `Tape`/`ValueRef`/`select_classifier` INTO crates/core
and retires the eager-`OpenFrame` / AoS-`TapeRec` / per-leaf-`StructRegistry`
fold-targets. The reconciliation: classify the existing H/MP.NW/SK-V15 waves
against the fold (most are *pending* receivers that the fold's substrate
adoption feeds; none is *refuted* by the fold; the fold REINFORCES the
pre-blocks REDRESS already strengthened); and propose ONE new SK-V18 fold
receiver block (MP.SK18.W0..W6) implementing F1-F9 under the five LACs. The
monotonic invariant holds: SK-V17/SK-V18 skinny wins become V1-authoritative;
the MASTER plan never dictates back to the live skinny iteration
(`restart/skinny/tranches/sk-v17/SPEC.md:110-114`). T-P3 only proposes these
MASTER deltas; Pass Omega CRUD applies any accepted text post-G-Omega.

## V3 Delta Summary

V3 folds the V2 CHALLENGE wave. The single 3B-targeted disposition is **CH4-V2-01
REVISE** against the MP.SK18.W1 receiver row (`3b:140`, F1/LAC-2F-FOLD-01): the
eager-`OpenFrame` retirement propagation was priced "22+ files". V1 CH4-01 had
already corrected this exact figure to 40 in 3A (`restart/audit/totality/sk-v17/p3/3a-architecture-synthesis.md:56,75`),
but the fold was applied only to 3A's parallel D01 row and not propagated to 3B's
W1 wave row. Re-verified `grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/` =
**40** (master HEAD 2a76916ac, `2026-05-29`). The understatement (40 ≈ 1.8× the
stated 22+) is material on a HIGH-risk wave whose single-wave-vs-split allocation
3B governs. FOLDED: the W1 row now reads "blast radius = 40 files via `grep -rl
'JsonStructBuilder\|CssStructBuilder' crates/`" — phrased isomorphically to 3A's
fold so the blast-radius figure is single-sourced across both surfaces. The
300-700 LOC band (the fold-edit envelope, not the touched-file count) and the HIGH
risk class are unaffected. No 3B delta dropped, removed, or added; the nine-delta
set is preserved. The V2 D07 wired-target sizing (`crates/egraph` 1885 /
`crates/csp-solver` 5882) re-verified stable, no further revision.

| bucket | ids | note |
|---|---|---|
| carried | MP-3B-SKV17-D01..D07, D09 | Eight deltas carried verbatim — none drew a V2 CHALLENGE finding requiring re-text; D07 (V2-corrected) re-verified stable; the 3B SK-V18 receiver rows each passed the CH4 same-wave-consumer charge (`restart/audit/totality/sk-v17/p3/hardening/V1/CH4.md:75-97`). |
| removed | none | No 3B delta removed. The nine-delta set is integrity-preserved. |
| answered | MP-3B-SKV17-D08 | CH4-V2-01 REVISE folded: the W1 (D08-allocated) eager-retirement blast radius corrected from "22+ files" to the verified 40-file figure, isomorphic to 3A's D01 fold; LOC band and risk class unchanged. |
| newly added | none | V3 adds no 3B delta; the V1 nine-delta enumeration of the five LOCKED LACs + F1-F9 stands. |

## Wave Classification Ledger

Every existing MASTER wave is classified **landed / refuted / pending / new**
against the five LOCKED fold designs. The fold designs are a substrate-adoption
move (skinny-proven `Tape`/`ValueRef`/`select_classifier` → crates/core); they
do not revive any refuted route. The classification confirms: the fold's
proven substrate is the SAME substrate the SK-V15/H pending receivers already
target — the fold gives those receivers their proven implementation, not a new
direction.

### A-J Tranche Set

The A-J set remains the V1 implementation skeleton (59 stub waves,
`restart/MASTER-PLAN.md:204-223`). The fold touches three tranche surfaces:

| MASTER tranche group | V1 classification | evidence | SK-V18 fold allocation |
|---|---|---|---|
| B (Runtime Substrate) | pending; substrate is the fold's primary target | B close gate is "Tape/direct `DocumentView` works for one generated grammar shell" (`restart/MASTER-PLAN.md:192`); B forbids "ParseStream runtime rename or parallel substrate" (`restart/MASTER-PLAN.md:248`). | The SK-V18 fold (F1 eager-OpenFrame retirement + F3 SoA `Tape` convergence) IS the proven realisation of B's tape substrate; B's gate is met by adopting the skinny `Tape<'input>` (`skinny/.../tape/mod.rs:94-100`). No parallel substrate — the eager `OpenFrame` is the fold-DELETION target, not a sibling. |
| F (Rust Lowerer And Runtime Template) | pending; the value-projection generator is the fold's F2 target | F emits "generated runtime for seed grammars" (`restart/MASTER-PLAN.md:196`); forbids "proc-macro codegen facade or unbudgeted generated churn" (`restart/MASTER-PLAN.md:252`). | F2's ONE grammar-parametric `ValueRef<G>` projection generator (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:158-192`) is the F-tranche generator emitting lazy value/view/document per grammar; the eager per-grammar value enums are the regen retarget, not new hand-written files (Lock 14 honoured, `restart/locks/LOCKS.md:349`). |
| G (Path, Value, Visitor) | pending; consumes the F2 lazy value plane | G close gate is "`path!`, `select!`, visitor mutation, and future grammar gate pass" (`restart/MASTER-PLAN.md:197`). | G's value/visitor surfaces ride the F2 `ValueRef<G>` plane; preserve-rich-ast holds — the lazy view reconstructs typed CSSOM, never flattens (`restart/skinny/tranches/sk-v17/SPEC.md:252`; `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:512-514`). |

### H-Tranche Rows (§13)

The H tranche carries the SIMD/BackendShape/typed-event surfaces the fold's F4,
F5, F8 designs reconcile.

| MASTER wave | V1 classification | evidence | SK-V18 fold allocation |
|---|---|---|---|
| H.W1 (typed event cursor over tape projection) | landed-scoped / pending; the fold supplies its proven substrate | MASTER: "must make structural projection the single parse substrate; no new BIR variant and no new BBNF directive" (`restart/MASTER-PLAN.md:620`). | F1/F3/F7 adopt the proven SoA tape as that single substrate; F7's `OnceCell<StructuralIndex>` substrate_target pre-gate (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:385-420`) enforces "the structural projection IS the tape" (`restart/locks/LOCKS.md:75`). No new BIR variant, no new directive. |
| H.W4 (5-shape backend_shape per-rule selection) | pending; F8 wires the selector | MASTER: SinkOnly correctness-green, all-five gate open (`restart/MASTER-PLAN.md:624`); 5-shape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` (`restart/MASTER-PLAN.md:594`). | F8 wires skinny `derive_backend_shape` into core atop `EmitStrategy::StructDirect` (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:424-456`); the 5-shape canon is PRESERVED — F4 disposes the tape as a substrate-manifest CATEGORY, NOT a 6th shape (`restart/locks/LOCKS.md:107-108`). |
| H.W4.LOCK14 | pending; the fold is grammar-neutral by construction | MASTER: GrammarConfig legality is evidence, not fleet-wide Lock 14 closure (`restart/MASTER-PLAN.md:583`). | Every fold design is grammar-neutral (Lock 14): tape members are grammar-blind (`begin_compound` reads `layout.rule_id & 0x1F` only, `crates/core/src/runtime/tape/mod.rs:185-186`); `select_classifier(alphabet)` is config-breadth (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:284-327`). Sheets/BBNF-self projection generality stays SK-V18 proof, not SK-V17 claim (`restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:96`). |
| H.W2 / H.W2.5 (bbnf-simd primitive vocabulary) | partial; F5 adds ONE manifest row | MASTER: consumed primitive subset admitted, new primitives gated (`restart/MASTER-PLAN.md:579-580,621-622`); Lock 16 allowlist verbatim (`restart/MASTER-PLAN.md:669-738`). | F5 registers `select_classifier(alphabet)` / `scan_structural(input,&StructuralAlphabet)` as a Lock-16 manifest ROW with scalar-ref + checkasm + same-wave consumer (the tape) (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:284-327`). aarch64-only; the eq-set fan is the one real NEON Layer-1 body; no source-inventory admission. |

### MP.NW + SK-V14 + SK-V15 Receiver Blocks

| MASTER receiver | V1 classification | reason | SK-V18 fold disposition |
|---|---|---|---|
| §13.5 SK-V15 W0-W11 | pending / ACTIVE; precedes SK-V17/SK-V18 | SK-V15 is the active dispatch contract (`restart/MASTER-PLAN.md:889-905`); CSS-honesty PRUNE-then-REBUILD. | NOT touched by the fold. SK-V17 proves the tape model in skinny AFTER SK-V15 closes; SK-V18 adopts it. The SK-V15 5-shape lowerer gate (W8/W9, `restart/MASTER-PLAN.md:938-939`) is the precise consumer of F8's selector wiring. |
| §13.5 MP.SK15.W5/W6 (CSS typed Value provider) | pending; F2 supplies the lazy plane | SK-V15 W5 requires "typed CSS value/document/view/visitor provider" (`restart/MASTER-PLAN.md:935`). | F2's lazy `ValueRef<G>` projection is the proven shape of that provider; the eager `CssTypedValue` (`crates/core/src/runtime/css_l4/value.rs:414`) is the fold-deletion target (F1, `restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:90`). |
| §13.2 MP.NW6 / §13.4 MP-NW-07 (FactStream 5th category) | pending; F4 reuses its precedent verbatim | MP.NW6 lands "FactStream 5th substrate category … the 5-shape BackendShape canon STAYS UNCHANGED — FactStream is a substrate-target classification, NOT a 6th BackendShape variant" (`restart/MASTER-PLAN.md:640`). | F4 applies the LAC-1E-14 FactStream precedent VERBATIM to the tape (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:233-280`): the tape is the substrate the 5 shapes project from, recorded at the substrate manifest, NOT a 6th shape. Coherent with §13.2's own canon statement. |
| §13.3 SK-V14 W0..W11 / §13.4 MP-NW-01..14 | historical / superseded (already) | MASTER already marks these "historical/pre-block; superseded for active dispatch by §13.5" (`restart/MASTER-PLAN.md:751-762,847-855`). | No change. The fold's pre-blocks (AZ-IV eager, StructRegistry indirection, fact-stream-String, x86) inherit the same superseded-but-binding status. |

### Refuted-Route Confirmation (CH3 firewall)

The fold REINFORCES — never re-opens — these REDRESS-strengthened pre-blocks.
Each is a fold FENCE, not a revived wave (`restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:119-128`):

| pre-block | refutation | fold disposition |
|---|---|---|
| AZ-IV eager value tree (118×) | `restart/skinny/tranches/sk-v17/SPEC.md:791` | F1/F3 DELETE the eager `OpenFrame`/`CssTypedValue` shape; the fold replaces it with lazy projection, never carries it forward. |
| StructRegistry/Arena/Builder per-leaf indirection (28-65×/983×/10583×) | `restart/skinny/tranches/sk-v17/SPEC.md:793-795` | F6 fences it: `StructRegistry::layout(rule)` resolved ONCE at codegen, never per-leaf; live coupling at `crates/core/src/runtime/bbnf/arena.rs:47` severed by F1 (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:331-381`). |
| CSS fact-stream String as admission plane (~34% self-time) | `restart/skinny/tranches/sk-v17/SPEC.md:796-797` | F1 retires `W5C_REQUEST_FACT_PROFILES` to diagnostic-only; the V1 FactStream category survives ONLY for typed-schema output planes (`restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:124`). |
| x86/AVX-512/SVE close route | `restart/skinny/tranches/sk-v17/SPEC.md:806` | aarch64-only; the §7.3 x86 CollapsedStage is UNKNOWN-2D-05, NOT the SK-V18 target (`restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:127`); F4 verdict stands on `admits_collapsed_stage` x86-binding mechanically refusing on aarch64. |
| Second substrate (D6) / dual AoS-SoA end-state | `restart/skinny/tranches/sk-v17/SPEC.md:854` | F3 closes to EXACTLY ONE encoding (`restart/locks/LOCKS.md:75`); AoS→SoA coexistence is admissible ONLY as a transient fold-state, never a Lock-1 closure. |

## SK-V18 Fold Receiver Block (proposed)

The five LOCKED fold designs (LAC-2F-FOLD-01..05) realise as F1-F9 and group
into SK-V18 adoption waves. These are PROPOSED MASTER receiver rows — not
implementation dispatch. Each carries the F-candidate it implements, the LAC it
crystallises, LOC/risk, the same-wave consumer (no orphan pre-gate), the
5-shape-canon coherence note, and the cap-fit/fail route. The fold is dispatched
only AFTER SK-V17 skinny W0-W5 close proves the model and Pass Omega/G-Omega
authorise. There is no implicit 6th wave or challenge-time overflow.

| SK-V18 receiver | F-candidate / LAC | manual LOC / generated / risk | MASTER alignment | same-wave consumer / gate | cap-fit and fail route |
|---|---|---:|---|---|---|
| MP.SK18.W0 OnceCell substrate_target classification (pre-gate) | F7 / LAC-2F-FOLD-01 | 0 behavior LOC; classification report; HIGH (REDRESS-53 re-entry) | H.W1 single-substrate; B substrate | Co-waved W1 tape-wiring IS the consumer; classification GATES the wiring (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:406-414`). All 8 carriers (json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf), not a 4-grammar sample. | Fits as classification only; a mis-declared `existing_tape` re-opens REDRESS-53 (`restart/skinny/tranches/sk-v17/SPEC.md:837-839`) → REDRESS/intrinsic block, no implicit overflow. |
| MP.SK18.W1 Eager-OpenFrame retire → flat-tape commit-by-construction | F1 / LAC-2F-FOLD-01 | 300-700 generator-side + per-grammar regen ×8; blast radius = 40 files via `grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/` (=40, master HEAD 2a76916ac; the 300-700 LOC band is the fold-edit envelope, not the touched-file count); HIGH | B "Tape/direct works"; B forbids parallel substrate (`restart/MASTER-PLAN.md:192,248`) | The flat-tape commit (`push_plain_offset`, `restart/skinny/tranches/sk-v17/SPEC.md:446`) is the same-wave consumer of the retired builders; revert slice `grep -rln JsonStructBuilder\|CssStructBuilder`. | Fits as eager-deletion + tape-wiring; delete-before-tape-wired is reverted (no orphan deletion). Severs the F6 `arena.rs:47` coupling. |
| MP.SK18.W2 AoS `TapeRec` → SoA `Tape` exactly-one-encoding closure | F3 / LAC-2F-FOLD-01 | 200-600; MEDIUM | B substrate; Lock 1 one-encoding (`restart/locks/LOCKS.md:75`) | The W1 commit-by-construction path consumes the converged SoA encoding; AoS→SoA is the transient fold-state. | Fits as encoding convergence; a dual AoS/SoA END-state is a Lock-1 violation → REDRESS. SoA is the proven anchor (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:208-211`). |
| MP.SK18.W3 Lazy `ValueRef<G>` projection generator | F2 / LAC-2F-FOLD-03 (value-plane home) | 300-700 generator-LOC + per-grammar regen value/view/document ×8; HIGH | F runtime template (`restart/MASTER-PLAN.md:196`); MP.SK15.W5 CSS provider | JSON `value_from_ref` byte-equal re-emission is the W3 gate consumer (`restart/skinny/tranches/sk-v17/SPEC.md:550-557`); a CSS-only generator that never re-emits JSON FAILS CH2. | Fits as ONE grammar-agnostic accessor generator (Lock 14, `restart/locks/LOCKS.md:349`); JSON+CSS-exercised only — Sheets/BBNF-self by-construction (`restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:96`). preserve-rich-ast. |
| MP.SK18.W4 StructRegistry/FieldSource compile-time projection fence | F6 / LAC-2F-FOLD-04 | 0 LOC (fence); HIGH (regression class) | Lock 1; AZ-IV pre-block | The W3 generator IS the consumer — it resolves the layout ONCE at codegen, never per-leaf (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:331-356`). | Fits as a fence on W3 emission; ANY per-leaf runtime `StructRegistry::layout(rule)` re-opens 28-65×/983×/10583× → REJECT (`restart/skinny/tranches/sk-v17/SPEC.md:793-795`). |
| MP.SK18.W5 Shared NEON classifier Lock-16 manifest row + BackendShape selector wiring | F5 + F8 / LAC-2F-FOLD-03 (NEON) + LAC-2F-FOLD-02 | F5 0-LOC narrative + manifest row, 100-400 scope reconcile; F8 60-200 selector + 600-1400 joint decision-engine wiring; MEDIUM | H.W2/H.W2.5 Lock 16; H.W4 5-shape selection; §13.5 W8/W9 lowerer gate | F5 same-wave consumer = the tape; F8 consumer = the 5 real lowerers (the SK-V15 W8/W9 all-five gate, `restart/MASTER-PLAN.md:938-939`). | Fits as manifest row + selector wiring atop existing `crates/egraph`+`crates/csp-solver` (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:431-437`); 5-shape canon PRESERVED — F4 disposes the tape as substrate-manifest category, not a 6th shape. |
| MP.SK18.W6 Lock-2 `StructLayout` canonical-name reconcile | F9 / LAC-2F-FOLD-05 | path-(a) 960-site rename (regen 8 parsers + ~16 tests) MEDIUM; path-(b) text-only re-scope LOW, core materialisation UNKNOWN→bounded | Lock 2 name-retirement (`restart/locks/LOCKS.md:160`) | The regen of all 8 parsers is the same-wave consumer of the rename; path-(b) side-table is sized as the 0→N introduce-site delta (`grep StructLayout crates/`=960 vs `grep backend_shape\|LayoutFacts crates/`=0). | Fits as a Lock-2 sub-surface reconcile, generator-side; the path-(a)-vs-(b) choice is a T-P3 3C/Pass-Omega call. Not one of the five core fold designs. |

The F4 BackendShape-canon disposition (LAC-2F-FOLD-02) is a CANON/PRECEDENT
delta, not an implementation wave — it carries 0 LOC and is realised as the
MASTER cross-reference that the tape is a substrate-manifest category (folded
into MP.SK18.W5's manifest row plus the D04 canon note below). It touches no
new wave LOC; it is a coherence assertion across §13 H.W4, §13.5, and §13.1.

## Proposed Delta Table

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1-surface section | rationale, LOC/risk/wave alignment |
|---|---|---|---|
| MP-3B-SKV17-D01: Add an SK-V17/SK-V18 downstream-of-SK-V15 authority note before the SK-V18 fold block. | T-P1 1D monotonic clause (`restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:71-76`); SK-V17 SPEC foldable-into-totality (`restart/skinny/tranches/sk-v17/SPEC.md:110-114`). | `restart/MASTER-PLAN.md` §13 preamble + §25 Implementation Order. | 40-100 doc LOC, medium routing risk, Pass Omega CRUD only. States SK-V15 closes first, SK-V17 proves the tape model in skinny, SK-V18 adopts it into crates/core. Monotonic: MASTER never dictates back to live skinny. |
| MP-3B-SKV17-D02: Add the SK-V18 fold receiver block MP.SK18.W0..W6 with F-candidate/LAC/LOC/risk/consumer columns. | The five LOCKED LACs (`restart/audit/totality/sk-v17/p2/hardening/HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:85-221`); F1-F9 enumeration (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:115-503`). | New §13.6 after §13.5 SK-V15 block. | 250-450 doc LOC, high routing risk, new pending waves MP.SK18.W0..W6. Every row carries a same-wave consumer; no orphan pre-gate. |
| MP-3B-SKV17-D03: Record the tape as a Lock-1 substrate-manifest CATEGORY (the substrate the 5 shapes project from), explicitly NOT a 6th BackendShape, in §13 H.W4 and §13.1. | F4 / LAC-2F-FOLD-02 (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:233-280`); LAC-1E-14 FactStream precedent (`restart/locks/LOCKS.md:100-116`); `admits_collapsed_stage` x86-binding (`restart/ARCHITECTURE.md:1151`). | §13 H.W4 (`restart/MASTER-PLAN.md:594,624`), §13.1, §13.2 MP.NW6. | 0 LOC implementation + 60-120 doc LOC, medium prose risk. 5-shape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` STAYS UNCHANGED across §13/§13.5/§13.1 — coherence delta. A 6th variant is G-Omega-gated (`restart/locks/LOCKS.md:107-109`). |
| MP-3B-SKV17-D04: Mark MASTER's B/F/G tranche substrate, runtime-template, and value/visitor close gates as fed by the SK-V18 fold's proven implementation. | F1/F2/F3 (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:125-229`); B/F/G close gates (`restart/MASTER-PLAN.md:192,196,197`). | §5 Tranche Set, §5.3 YAML trajectory B/F/G rows. | 80-160 doc LOC, medium regression risk, no implementation wave. The fold realises B's "Tape works" / F's "generated runtime" / G's "value/visitor" gates with the proven shape; no parallel substrate, no hand-written per-grammar runtime. |
| MP-3B-SKV17-D05: Register `select_classifier(alphabet)` / `scan_structural(input,&StructuralAlphabet)` as a Lock-16 manifest ROW in §13.1 with scalar-ref + checkasm + same-wave consumer (the tape). | F5 / LAC-2F-FOLD-03 NEON home (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:284-327`); Lock 16 manifest (`restart/locks/LOCKS.md:506-533`). | §13.1 arm64 NEON allowlist + §13 H.W2.5. | 0-LOC narrative + 1 manifest row + 100-400 scope reconcile; medium risk. aarch64-only; the eq-set fan is the one real NEON Layer-1 body; table/prefix are honest scalar passthroughs. No source-inventory admission. |
| MP-3B-SKV17-D06: Add the StructRegistry/FieldSource compile-time projection fence as a MASTER substrate-manifest obligation (MP.SK18.W4). | F6 / LAC-2F-FOLD-04 (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:331-381`); 28-65×/983×/10583× pre-block (`restart/skinny/tranches/sk-v17/SPEC.md:793-795`). | §13.6 MP.SK18.W4, §23 Risk Register, §24 Carry Ledger. | 0 LOC (fence) + 40-80 doc LOC, high regression-class risk. The W3 generator resolves the layout once at codegen; live coupling at `crates/core/src/runtime/bbnf/arena.rs:47` severed by F1. AZ-IV indirection pre-blocked. |
| MP-3B-SKV17-D07: Wire skinny `derive_backend_shape` 5-shape selector into core (MP.SK18.W5) atop the existing decision engine, preserving the all-five gate. | F8 / LAC-2F-FOLD-02 (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:424-456`); 4 skinny lowerers 17-LOC scaffolds (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:441-442`). | §13 H.W4/H.W7, §13.5 MP.SK15.W8/W9 lowerer gate. | 60-200 selector + 600-1400 joint decision-engine wiring; medium risk. WIRES — does not build — the existing decision-engine crates (`crates/egraph` 1885 LOC + `crates/csp-solver` 5882 LOC, `find crates/{egraph,csp-solver}/src -name '*.rs' \| xargs wc -l`); the skinny lowerer scaffolds it consumes are `backend_egraph` 311 LOC + `decision_csp` 273 LOC (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:433-434`). The 600-1400 LOC envelope sizes the WIRING, not the engine. The SK-V15 W8/W9 all-five gate is the consumer. No new BackendShape, no surface annotation (`backend_shape` is a side-table field, `restart/locks/LOCKS.md:269`). |
| MP-3B-SKV17-D08: Add the F3 exactly-one-encoding closure (MP.SK18.W2) and the F7 substrate_target pre-gate (MP.SK18.W0) as Lock-1 one-substrate obligations across all 8 carriers. | F3 + F7 / LAC-2F-FOLD-01 (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:196-229,385-420`); Lock 1 one-encoding (`restart/locks/LOCKS.md:75`). | §13.6 MP.SK18.W0/W2, §13 H.W1, §24 Carry Ledger. | 0 LOC (W0 classification) + 200-600 LOC (W2 closure); medium-high risk. A dual AoS/SoA end-state or a retained index parallel to a wired tape re-opens REDRESS-53 / the second-substrate block. SoA is the proven convergence anchor. |
| MP-3B-SKV17-D09: Add the Lock-2 `StructLayout` canonical-name reconcile (MP.SK18.W6) with the two-path (rename-vs-side-table) disposition routed to 3C/Pass Omega. | F9 / LAC-2F-FOLD-05 (`restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:460-503`); Lock 2 name-retirement (`restart/locks/LOCKS.md:160`). | §13.6 MP.SK18.W6, §21 Lock Ownership. | path-(a) 960-site rename medium; path-(b) text-only re-scope low + core materialisation UNKNOWN→bounded-by-introduce-delta. Generator-side; regen-gated. Not one of the five core fold designs; a Lock-2 sub-surface reconcile. |

## Consequences

| delta | LOC | propagation (surfaces touched) | risk | wave alignment | consumer / gate | cap-fit | fail action |
|---|---:|---:|---|---|---|---|---|
| D01 | 40-100 doc | 2 (§13 preamble, §25) | medium routing | Pass Omega/G-Omega before SK-V18 W0 | Pass Omega CRUD + G-Omega acceptance | doc-only authority repair | Reject CRUD or REDRESS; block SK-V18 W0 until authority is current. |
| D02 | 250-450 doc | 1 (new §13.6) | high routing | MP.SK18.W0..W6 | Pass Omega CRUD + each MP.SK18 same-wave gate | receiver-map doc; per-wave cap holds | Missing same-wave consumer column blocks CRUD; non-fit wave routes intrinsic block/REDRESS, no overflow. |
| D03 | 60-120 doc | 3 (§13 H.W4, §13.1, §13.2) | medium prose | MP.SK18.W5 manifest + canon | Pass Omega CRUD; 5-shape coherence gate across §13/§13.5/§13.1 | canon/precedent assertion | Any text adding a 6th shape is a CH3 coherence REJECT; 6th variant stays G-Omega-gated. |
| D04 | 80-160 doc | 3 (§5, §5.3, B/F/G gates) | medium regression | B/F/G tranche gates | Pass Omega CRUD + tranche close-gate reference | classification text only | No scoped fold landing becomes V1/root close; no parallel substrate or hand-written per-grammar runtime. |
| D05 | 0 + 100-400 reconcile | 2 (§13.1, §13 H.W2.5) | medium | MP.SK18.W5 / H.W2.5 | the tape (same-wave); checkasm parity; scalar oracle | manifest row + aarch64 scope | Source-present-without-consumer is a manifest entry, not an admit; x86/SVE remain diagnostic. |
| D06 | 0 + 40-80 doc | 3 (§13.6, §23, §24) | high regression-class | MP.SK18.W4 | the W3 generator (resolves layout once at codegen) | fence on W3 emission | Per-leaf runtime `StructRegistry::layout(rule)` is REJECT; revert/REDRESS. |
| D07 | 60-200 + 600-1400 | 3 (§13 H.W4/H.W7, §13.5 W8/W9) | medium | MP.SK18.W5 / SK-V15 W8/W9 | the 5 real lowerers / all-five gate | selector + decision-engine wiring | Fail-closed on e-graph cap / CSP timeout / stale cost (`restart/locks/LOCKS.md:290-293`); no new shape. |
| D08 | 0 (W0) + 200-600 (W2) | 3 (§13.6 W0/W2, §13 H.W1, §24) | medium-high | MP.SK18.W0/W2 / H.W1 | co-waved W1 tape-wiring | classification + encoding closure | Dual AoS/SoA end-state or parallel retained index re-opens REDRESS-53 → REDRESS. |
| D09 | path-(a) 960-site / path-(b) low+UNKNOWN | 2 (§13.6 W6, §21) | medium | MP.SK18.W6 | regen of 8 parsers (path-a) / side-table introduce-delta (path-b) | generator-side rename, regen-gated | path choice is a 3C/Pass-Omega call; Lock 2 closure not claimed by `LayoutFacts` alone (`restart/locks/LOCKS.md:162-166`). |

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 CORRECTNESS | Should Pass Omega add a new §13.6 SK-V18 fold block, or fold MP.SK18.W0..W6 into the existing §13.5 SK-V15 receiver structure? | Pass Omega CRUD owner for `restart/MASTER-PLAN.md`. | 3A/3F cross-surface wording and the §13.5-vs-new-§13.6 placement are not selected; SK-V18 is downstream of SK-V15, arguing for a sibling §13.6. | Pass Omega CRUD acceptance before G-Omega authorization. |
| CH2 GENERALITY | The F2 `ValueRef<G>` value-plane fold is JSON+CSS-exercised only; what exact non-JSON onboarding test does MP.SK18.W3 require before Lock 14 claims fleet-wide closure? | 3E grammar-generalisation + SK-V18 W3/W5 owner. | Sheets/BBNF-self are by-construction under SK-V18, not by-exercise (`sheets_witness` 24-LOC stub, `restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:96`). | The future-grammar onboarding test (3E deliverable) + W3 byte-equal JSON re-emission gate. |
| CH3 REGRESSION | Does any proposed MP.SK18 row imply reviving the eager-OpenFrame, fact-stream-String, or per-leaf-StructRegistry route the fold deletes/fences? | SK-V18 W1/W4 owner + 3C LOCKS crystallisation. | Confirm F1 deletes (not carries) the eager builders and F6 fences the registry lookup; the live coupling at `crates/core/src/runtime/bbnf/arena.rs:47` must be severed in the same wave. | W1 eager-deletion proof + W4 fence (no per-leaf runtime lookup); CH3 firewall. |
| CH4 COST | If MP.SK18.W3 (lazy projection generator) or W5 (decision-engine wiring) exceeds its band, is the route intrinsic block or a G-Omega wave-graph amendment? | SK-V18 wave governance + the affected wave owner. | Cap evidence from SK-V17 W0-W5 skinny proof; the generator-LOC vs regen-LOC split must be measured before sizing. | Plan/redress cap gate: row-level intrinsic block, REDRESS/revert, or G-Omega amendment before redress. |
| CH5 HIDDEN COUPLING | Which exact 3C Lock 1/10/14/16 v+1 text classifies the tape-as-substrate-manifest-category and the `substrate_target` declarations so no MASTER cross-reference reads as a 6th shape or a parallel substrate? | 3C LOCKS crystallisation + Pass Omega LOCKS CRUD owner. | Accepted v+1 Lock 1/10 text (tape category, not 6th shape) is not yet selected; 3C owns the crystallisation. | LOCKS v+1 acceptance gate + MASTER cross-reference CRUD gate; substrate-union holds. |
| CH6 ANTI-PAPER-CLOSE | What exact SK-V17 skinny W0-W5 close evidence must exist before SK-V18 W0 dispatches, so the fold is not deferred-without-receiver? | SK-V18 W0 owner + 3F next-cycle directive. | SK-V17 skinny proof (W0-W5, `restart/skinny/tranches/sk-v17/SPEC.md:264-269`) + Pass Omega/G-Omega authorization are the named receiver+blocker+gate; no engineered-defer. | SK-V17 skinny close gate + Pass Omega G-Omega CRUD authorization before SK-V18 W0 entry. |
