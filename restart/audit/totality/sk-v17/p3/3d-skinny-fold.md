---
agent: 3D
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-29T20:45:00Z
master_head: 2a76916ac
t_p1_locked_sha: 445925167154de73540e3ea3283d0170371de790
t_p2_locked_sha: 91b6893b0
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: n/a
proposed_deltas_count: 8
delta_summary:
  carried_from_prior_cycle:
    - 3D-SK17-D01-tape-soa-authoritative
    - 3D-SK17-D02-valueref-lazy-authoritative
    - 3D-SK17-D03-neon-classifier-authoritative
    - 3D-SK17-D04-eager-tree-locks-strengthening
    - 3D-SK17-D05-registry-fence-locks-strengthening
    - 3D-SK17-D06-factstream-string-locks-strengthening
    - 3D-SK17-D07-sheets-bbnf-generality-gap-to-3E
    - 3D-SK17-D08-monotonic-direction-clause
  removed: []
  answered: []
  newly_added: []
prior_cycle_dispositions_folded:
  accepted:
    - CH3-V2-3D-R1-eager-tree-locks-strengthening          # V2/CH3.md:72 ACCEPT
    - CH3-V2-3D-R2-registry-fence-locks-strengthening       # V2/CH3.md:73 ACCEPT
    - CH3-V2-3D-R3-factstream-locks-strengthening           # V2/CH3.md:74 ACCEPT
    - CH3-V2-3D-W1W2W3-wins-authoritative                   # V2/CH3.md:75 ACCEPT
    - CH3-V2-3D-G1-generality-gap-to-3E                     # V2/CH3.md:76 ACCEPT
    - CH3-V2-3D-D08-monotonic-direction                     # V2/CH3.md:77 ACCEPT
    - CH3-V2-3D-fold-disposition-coherence                  # V2/CH3.md:78 ACCEPT
    - CH4-V2-3D-full-coverage-costs-to-SKV18                # V2/CH4.md:136-141 ACCEPT
    - CH5-V2-3D-no-parallel-substrate                       # V2/CH5.md:118 ACCEPT
    - CH6-V2-3D-monotonic-named-gate-not-prose              # V2/CH6.md:110-113,:130 ACCEPT (CONSOLIDATED row 5)
    - CH7-V2-3D-skinny-fold-section                         # V2/CH7.md:94-98 ACCEPT (banner folded)
  rejected: []
  revised: []   # no 3D-touching REVISE landed in CHALLENGE V2; the two open V2 REVISEs (CH5-V2-R02→3E, CH6-REVISE#9→3C frontmatter) and CH7-S17-V2-R1→3F do not touch 3D
---

## Executive Summary

> **SCOPE-HONESTY BANNER.** The tape/`ValueRef`/NEON fold is PROVEN by-EXERCISE on
> **JSON** (51/51 strict A/GO Track 1 > sonic same-plane, `skinny/RESULTS.md:5-55`) and
> first-moved on **CSS** (SK-V17); it is BY-CONSTRUCTION-NOT-BY-EXERCISE on
> **Sheets/BBNF-self** (`sheets_witness` carries no `BackendRule`, `restart/skinny/tranches/sk-v17/SPEC.md:110`-`114`).
> The CSS `>SOTA` bar is **UNMEASURED-PENDING — NOT met — an SK-V18 proof obligation**
> (`restart/skinny/tranches/sk-v17/HANDOFF.md:44`-`45`; `SPEC.md:207`); SK-V17 proved
> JSON `>SOTA` and CONVERGED on the contract whose CSS `>SOTA` bar SK-V18 must prove.
> A G3 skim must read every Sheets/BBNF-self claim AND every CSS-`>SOTA` claim herein
> as *predicted / SK-V18-pending*, not proven. The NEON classifier is generality-by-CONFIG
> (alphabet-as-data, wired across 8 grammars) but the tape CONSUMER is JSON-only-wired
> today (SK17L-008; cross-ref 3E D07).

3D folds the SK-V1…SK-V17 skinny track into the totality V1 spec in ONE direction
only: SKINNY-proven wins become V1-spec-authoritative, SKINNY rejections become
locks-strengthening evidence (cross-ref 3C), non-JSON generalisation gaps route to
3E — and the totality spec never dictates back to a live skinny iteration
(`restart/skinny/tranches/sk-v17/SPEC.md:110`-`114`; SK17L-007/008/009 monotonic
clauses, `restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:88`,`:128`). The ONE
load-bearing skinny WIN is the flat lazy-offset SoA `Tape<'input>` + lazy
`ValueRef<G>` projection (JSON 51/51 strict A/GO Track 1 > sonic same-plane,
`skinny/RESULTS.md:5-55`): this is the >SOTA carrier and the regression tripwire, and
it becomes V1-authoritative as the single substrate the 5 `BackendShape` shapes
project from. The durable skinny REJECTIONS — AZ-IV eager value tree (118×),
StructRegistry/Arena/Builder per-leaf indirection (28-65×/983×/10583×), CSS
fact-stream String as admission plane, x86/AVX/SVE — become locks-strengthening
fences (Lock 1/10/14/16), each pre-blocked as a fold-target SHAPE in crates/core, NOT
re-derived. The non-JSON gap is explicit-and-monotonic: JSON is the only tape WITNESS
today, CSS is the SK-V17 first-mover (its `>SOTA` bar UNMEASURED-PENDING, an SK-V18
obligation), Sheets/BBNF-self projection generality is by-construction-not-by-exercise
(SK-V18). Eight deltas; all proposal-only; no V1 surface touched by 3D (§8.6).
preserve-rich-ast and N≥50-cold-median telemetry honesty are non-negotiable inputs to
every fold.

## V3 Delta Summary

| bucket | disposition |
|---|---|
| carried from prior cycle | `3D-SK17-D01`…`3D-SK17-D08` — all eight V2 deltas carried verbatim. CHALLENGE V2 returned ACCEPT on every 3D-touching lens (CH3 rows R1/R2/R3/W1W2W3/G1/D08/coherence `V2/CH3.md:72`-`78`; CH4 `V2/CH4.md:136`-`141`; CH5 `V2/CH5.md:118`; CH6 CONSOLIDATED row 5 `V2/CH6.md:130`; CH7 §3D `V2/CH7.md:94`-`98`); no 3D delta required a corrected disposition. |
| removed | None. |
| answered | None outstanding. The CH7/CH4 V1 open question (proven-vs-by-construction banner) was ANSWERED in V2 by the SCOPE-HONESTY BANNER and remains in place; V3 EXTENDS that banner with the CSS-`>SOTA`-UNMEASURED-PENDING clause so the same G3-skim firewall covers the CSS speed axis (consistency with 3E P5b and the V2 CH7-S17-V2-R1 disposition that landed on 3F, `V2/CH7.md:153`-`180`). This is a coherence-only sharpening at exec-summary altitude; no delta is added, removed, or re-dispositioned. |
| newly added | None — V3 folds the V2 ACCEPT wave; it does not add deltas. The eight-delta register is stable across V1→V2→V3. |

**Cross-artefact integrity note (V3).** The load-bearing CH1 REVISE on 3C's gate object
was RESOLVED in V2 and remains clean in V3: `3c-locks-v+1-diff.md:49` reads
`@@ -606,7 +606,22 @@` (verified at this HEAD) and `git apply --check` returns EXIT 0
against `LOCKS.md` at master HEAD `2a76916ac`. Every 3D delta that cross-refs 3C
(D01/D04/D05/D06 → Lock 1; D03 → Lock 16; D07 → Lock 14) therefore points at a gate
object that applies clean. The two open CHALLENGE-V2 REVISEs — CH5-V2-R02 (the
`strategy.rs` leak-census receiver, routed to 3E, `V2/CH5.md:157`-`177`) and the CH6
frontmatter-truth REVISE on 3C (`V2/CH6.md:134`) — and CH7-S17-V2-R1 (the 3F CSS-speed
over-claim, `V2/CH7.md:153`-`180`) are 3E / 3C / 3F obligations; none touches a 3D
delta. The 16-lock count is preserved; no lock is renumbered by 3D.

## Monotonic Fold Register (the load-bearing classification)

Every SK-V17 lesson sorts into exactly one of three monotonic buckets. The direction
is invariant: skinny informs totality, totality never dictates back to a live skinny
iteration (`restart/skinny/tranches/sk-v17/SPEC.md:110`-`114`; SK17L row-7 fold fence
`1d:128` — "SK-V18 adopts the PROVEN skinny `Tape`/`ValueRef` INTO crates/core, never
relocates crates/core `TapeStructBuilder` into skinny").

| # | fold class | SK-V17 lesson (path:line) | totality treatment (proposal-only) |
|---|---|---|---|
| W1 | WIN → V1-authoritative | SoA `Tape<'input>` (`offsets:Vec<u32>` + sparse `flag_cursors`/`flag_values` + `PayloadArena`, `skinny/crates/runtime/src/tape/mod.rs:94`,`:96-99`,`:38`); `push_plain_offset` = one branchless u32 write (`assembler.rs:42`,`:71`); JSON rides it >SOTA (`skinny/RESULTS.md:5-55`) | V1 names the proven SoA `Tape` the single post-fold substrate; AoS `TapeRec` (`crates/core/.../tape/record.rs:103`) converges onto it; a dual end-state is transient-only, not a Lock-1 closure (SK17L-001, `1d:88`; LAC-2F-FOLD-01; cross-ref 3A/3C). |
| W2 | WIN → V1-authoritative | Lazy grammar-parametric `ValueRef<'doc,'input,K,G>` (`tape/mod.rs:175`) read by `value_from_ref` per grammar, zero per-node heap alloc (JSON witness `json/value.rs:143`, `alphaC:69-72`) | V1 names ONE `BackendRule`-walking generator emitting document/value/view/visitor; the existing `@generated` per-grammar value path is RETARGETED to emit lazy `ValueRef<G>` (SK17L-002, `1d:89`; LAC-2F-FOLD-03; cross-ref 3A/3C). |
| W3 | WIN → V1-authoritative | Shared `select_classifier(alphabet:&[u8;64])` → `Vec<u32>` structural index (`skinny/crates/bbnf-simd/src/dispatch.rs:42`); alphabet is the only grammar datum; aarch64-only NEON, scalar-ref + checkasm (SPEC §0.1 row 9 `:99-106`) | V1 registers the shared classifier as a Lock-16 primitive-manifest ROW; the NEON narrative folds from JSON-first to alphabet-as-data; x86/AVX/SVE permanently pre-blocked this pass (SK17L-006/008, `1d:93`,`:95`; LAC-2F-FOLD-03; cross-ref 3A/3C/3E). |
| R1 | REJECTION → locks-strengthening | AZ-IV eager value tree REFUTED 118× (canada 1.83ms→215.7ms, `cb14970f`; `alphaC:50-66`) | Lock 1/10 keep lazy-by-default; the crates/core `CssTypedValue` eager enum + six `pending_*` Vecs (`crates/core/.../css_l4/builder.rs:71-79`) is a fold-DELETION target, never carried forward (SK17L-003, `1d:90`; L-SK17-01; cross-ref 3C). |
| R2 | REJECTION → locks-strengthening | StructRegistry/Arena<G>/Builder<G> per-leaf hot-path indirection REFUTED 28-65×/983×/10583× (`alphaC:98-118`; SPEC §9 `:793-795`) | Lock 1 carries the no-per-leaf-registry-lookup fence; `StructRegistry::layout(rule_id)` (`crates/ir/src/registry/struct.rs:313`,`:331`) resolved ONCE at codegen, never per-leaf (SK17L-004, `1d:91`; L-SK17-02/02b; cross-ref 3C). |
| R3 | REJECTION → locks-strengthening | CSS fact-stream String REFUTED as admission plane (~34% self-time `emit_*`; benched String not typed CSSOM, `alphaC:162-211`) | Lock 1 v+1 FactStream category survives ONLY for typed-schema/provenance output planes; the String CSS-admission plane retires to diagnostic-only (SK17L-005, `1d:92`; L-SK17-03; cross-ref 3C). |
| G1 | GENERALITY GAP → 3E | Projection generality exercised by-construction on JSON + CSS ONLY; `sheets_witness` 24-LOC `EventGrammar` stub has NO `.bbnf`/`BackendRule`; BBNF-self no tape witness (SPEC §0.1.11 `:110-114`, `alphaD:53-65`,`:151`) | 3D routes the Sheets/BBNF-self generality story to 3E; Lock 14 stays grammar-neutral; fleet-wide proof is SK-V18 by-exercise (SK17L-009, `1d:96`; cross-ref 3E/3C). |

## Proposed Delta Table

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1-surface section | rationale |
|---|---|---|---|
| `3D-SK17-D01-tape-soa-authoritative`: V1 absorbs the SoA `Tape<'input>` as the V1-AUTHORITATIVE single post-fold substrate (the proven-and-benched encoding); AoS `TapeRec` converges onto it under SK-V18; a dual AoS/SoA end-state is a transient fold-state only, NOT a permissible Lock-1 closure. | T-P1 SK17L-001 (`restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md:88`); T-P1 U-SK17L-001/002 (`1d:155`-`156`); T-P2 LAC-2F-FOLD-01 + 2F-FOLD-U1 (`restart/audit/totality/sk-v17/p2/hardening/HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:85`-`108`,`:307`-`309`). | `ARCHITECTURE.md` substrate-union / value-plane (`restart/ARCHITECTURE.md:1088`); `LOCKS.md` Lock 1 substrate-union (`restart/locks/LOCKS.md:75`,`:118`-`127`); SK-V17 monotonic clause (`restart/skinny/tranches/sk-v17/SPEC.md:110`-`114`). | The single load-bearing skinny WIN: JSON 51/51 strict A/GO Track 1 > sonic same-plane (`skinny/RESULTS.md:5-55`) is the standing >SOTA proof and the regression tripwire. The proven SoA encoding is V1-authoritative; 3A carries the surface delta, 3C the Lock-1 one-substrate closure. ORQ 2F-FOLD-U1 (SoA-adopt vs AoS-keep-and-prove-parity) is a T-P3 call — 3D affirms SoA-adopt as the proven encoding; parity-keep would re-open the dual-substrate Lock-1 risk. |
| `3D-SK17-D02-valueref-lazy-authoritative`: V1 absorbs the lazy grammar-parametric `ValueRef<G>` as the V1-AUTHORITATIVE materialization plane; ONE `BackendRule`-walking generator emits document/value/view/visitor; the existing `@generated` per-grammar value path is RETARGETED to emit lazy `ValueRef<G>` (JSON+CSS-exercised; Sheets/BBNF by-construction). | T-P1 SK17L-002 (`1d:89`,`:108`); T-P1 Gaps-row no-ValueRef-generator (`1d:147`); T-P2 LAC-2F-FOLD-03/F2 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:110`-`130`). | `ARCHITECTURE.md` value/output-plane union (`restart/ARCHITECTURE.md:1088`); `LOCKS.md` Lock 14 single-generator (`restart/locks/LOCKS.md:349`,`:603`); SK-V17 §0.1 row 3 layout-driven projection (`restart/skinny/tranches/sk-v17/SPEC.md:54`-`72`). | The skinny lazy projection is the byte-equal re-emission target (W2). The divergence is EAGER-generated vs LAZY-generated, NOT generated-vs-hand-written (Lock 14 HONOURED — both core value.rs carry `@generated by xtask regen-*`, `1d:89`,`:147`); the fold retargets the existing regen path. preserve-rich-ast: the lazy view is the materialization plane, never a typed-AST flattening (`restart/skinny/tranches/sk-v17/SPEC.md:78`-`83`). Scope-honest: JSON+CSS only (a CSS-only generator that never re-emits JSON FAILS CH2, `1d:108`). |
| `3D-SK17-D03-neon-classifier-authoritative`: V1 absorbs the shared `select_classifier(alphabet)` / `scan_structural` as a V1-AUTHORITATIVE Lock-16 primitive-manifest ROW (abstract primitive = alphabet-parametrised byte classification); the NEON narrative folds from JSON-first to alphabet-as-data; aarch64-only, scalar-ref + checkasm; x86/AVX-512/SVE pre-blocked this pass. | T-P1 SK17L-006/008 (`1d:93`,`:95`); T-P1 Structural-scan generality row (`1d:109`); T-P2 LAC-2F-FOLD-03/F5 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:132`-`160`). | `LOCKS.md` Lock 16 primitive manifest (`restart/locks/LOCKS.md:453`,`:607`), Lock 14 alphabet-as-config (`:603`); `ARCHITECTURE.md` §7.3 scan-leaf FFI under the 4 LLVM shapes; SK-V17 §0.1 row 9 (`restart/skinny/tranches/sk-v17/SPEC.md:99`-`106`). | Generality is CONFIG-breadth — `StructuralAlphabet` is config DATA, not grammar branches (Lock-14 vehicle). Core scan is already WIRED across 8 generated grammars (SK17L-008, `1d:95`); the residual gap is the missing TAPE CONSUMER, not the scan. The eq-set fan is the one real NEON Layer-1 body; table/prefix are honest scalar passthroughs (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:150`-`154`). NEON sits under the 4 LLVM shapes' scan-leaf FFI — NOT CollapsedStage (aarch64 CollapsedStage is UNKNOWN-2D-05, `1d:93`). |
| `3D-SK17-D04-eager-tree-locks-strengthening`: The SK-V17 AZ-IV REFUTATION (118×) becomes locks-strengthening evidence: V1 keeps materialization lazy-by-default; the crates/core eager value tree (`CssTypedValue` + six `pending_*` Vecs) is a fold-DELETION target, never carried forward into either tree. (cross-ref 3C ACCEPT.) | T-P1 SK17L-003 (`1d:90`); T-P1 L-SK17-01 do-not-redrive (`1d:121`); T-P2 REJECT-row "AoS/SoA dual end-state" + eager-OpenFrame retirement (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:238`-`240`,`:291`-`293`). | `LOCKS.md` Lock 1 (lazy view over sealed tape, `restart/locks/LOCKS.md:75`); Lock 10 5-shape EagerTape semantics (`:269`-`318`); SK-V17 preserve-rich-ast (`restart/skinny/tranches/sk-v17/SPEC.md:78`-`83`). | This is a skinny REJECTION → lock strength, NOT an implementation closure. The pre-block is anchored to the construct (per-leaf typed/f64/Box heap alloc), not a symbol list (`alphaC:78-82`, `1d:90`). The fold REPLACES the eager tree with lazy projection (D02), never carries it forward. 3C dispositions this ACCEPT under LAC-2F-FOLD-01's eager-OpenFrame retirement. |
| `3D-SK17-D05-registry-fence-locks-strengthening`: The SK-V17 StructRegistry/Arena/Builder REFUTATION (28-65×/983×/10583×) becomes the locks-strengthening no-per-leaf-registry-lookup FENCE: `StructRegistry::layout(rule_id)` resolved ONCE at codegen/emission, never per-leaf; the `FieldSource` walk is compile-time projection-emission. (cross-ref 3C ACCEPT.) | T-P1 SK17L-004 (`1d:91`); T-P1 L-SK17-02/02b fences (`1d:122`-`123`); T-P2 LAC-2F-FOLD-04/F6 + REJECT-row per-leaf-walk (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:193`-`214`,`:234`-`237`). | `LOCKS.md` Lock 1 substrate manifest fence (`restart/locks/LOCKS.md:118`-`127`); SK-V17 §9 global block (`restart/skinny/tranches/sk-v17/SPEC.md:793`-`795`). | The regression firewall. `begin_compound` already takes a resolved `&StructLayout` (reads only `layout.rule_id & 0x1F`, grep-zero StructRegistry, `1d:91`,`:122`); the fold inherits the correct shape ONLY if the caller resolves the layout once. Live coupling site is `crates/core/.../bbnf/arena.rs:47` inside the eager arena path (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:207`-`210`); the eager-OpenFrame retirement (D04/F1) severs it. A naive per-leaf walk re-opens the AZ-IV regression class. 3C dispositions ACCEPT under LAC-2F-FOLD-04. |
| `3D-SK17-D06-factstream-string-locks-strengthening`: The SK-V17 CSS fact-stream-String REFUTATION becomes locks-strengthening: the V1 Lock-1 FactStream category survives ONLY as a typed-schema/provenance OUTPUT plane; the String CSS-ADMISSION plane retires to diagnostic-only. NOT a Lock-1 contradiction — the V1 FactStream is output-plane, SK-V17 bars fact-stream-String-as-CSS-admission. (cross-ref 3C MODIFY/clarify.) | T-P1 SK17L-005 (`1d:92`); T-P1 L-SK17-03 (`1d:124`); T-P1 U-SK17L-002 one-encoding closure (`1d:156`); T-P2 fact-stream framing (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:256`). | `LOCKS.md` Lock 1 v+1 FactStream 5th category (`restart/locks/LOCKS.md:100`-`116`); SK-V17 §0.1 row 2 (Track 1 stops returning String, `restart/skinny/tranches/sk-v17/SPEC.md:46`-`53`). | The reconcilable framing: V1's FactStream `substrate_target = admitted_fact_output` is a typed output-plane (`LOCKS.md:103`), distinct from the SK-V17-barred String-as-CSS-admission plane. The live skinny route retires `W5C_REQUEST_FACT_PROFILES` (`skinny/crates/codegen/src/lib.rs:336`) to diagnostic-only. 3C clarifies (MODIFY) that the two FactStream senses do not collide; no Lock-1 narrowing. |
| `3D-SK17-D07-sheets-bbnf-generality-gap-to-3E`: V1 records that tape/ValueRef/NEON generality is exercised by-CONSTRUCTION on JSON+CSS only; Sheets/BBNF-self projection generality is by-construction-not-by-exercise and is the SK-V18 proof target. 3D routes the concrete non-JSON onboarding story to 3E; Lock 14 stays grammar-neutral; no JSON-narrowing. | T-P1 SK17L-009 (`1d:96`); T-P1 JSON-empirical-vs-grammar-neutral split (`1d:105`-`111`); T-P2 fleet-wide-proof REFUTED row (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:247`-`248`); SK-V17 §0.1.11 (`restart/skinny/tranches/sk-v17/SPEC.md:110`-`114`). | `LOCKS.md` Lock 14 grammar-generalisation + future-grammar onboarding (`restart/locks/LOCKS.md:349`,`:603`); 3E grammar-generalisation artefact (`restart/audit/totality/sk-v17/p3/3e-grammar-generalisation.md`). | The monotonic generality bridge (CH2 firewall): scan/projection generality is breadth-of-CONFIG (alphabet-as-data), NOT breadth-of-PROOF (`1d:105`,`:109`). `sheets_witness` is a 24-LOC `EventGrammar` stub with no `BackendRule` and CANNOT serve as a projection exercise (`1d:96`). 3D preserves the win and routes the gap; 3E carries the per-grammar `BackendShape` matrix + future-grammar onboarding test. 3C accepts no Lock-14 JSON-narrowing amendment. |
| `3D-SK17-D08-monotonic-direction-clause`: V1 records the monotonic skinny→totality direction as a governance invariant: SK-V18 adopts the PROVEN skinny `Tape`/`ValueRef` INTO crates/core; it NEVER relocates crates/core `TapeStructBuilder`/`StructLayout`/`TapeCursor` into skinny; the totality spec never dictates back to a live skinny iteration. | T-P1 SK17L-007 + L-SK17-07 second-substrate block (`1d:94`,`:128`); SK-V17 §0.1.11 + §9 forbidden vocabulary (`restart/skinny/tranches/sk-v17/SPEC.md:110`-`114`,`:807`-`811`); ORCHESTRATOR monotonic discipline (PASS-3 §8.4). | `LOCKS.md` Lock 2 `StructLayout`-retired (`restart/locks/LOCKS.md:160`); `MIGRATION.md` rename/abrogate surface (cross-ref 3F); `MASTER-PLAN.md` SK-V18 adoption waves (cross-ref 3B). | The fold direction is the discipline itself. The §9 names `StructLayout`/`TapeStructBuilder`/`TapeCursor` are FORBIDDEN-IN-SKINNY (`1d:128`); the totality fold-target carries the disproved-route SHAPES as UNWIRED dead code awaiting SK-V18 fold — they are the fold TARGET, not skinny artefacts (`1d:72`-`77`). D08 makes the direction explicit so neither 3B's waves nor 3F's migration inverts it. |

## Consequences And CH4 Coverage

| delta | LOC (proposal-only) | propagation count | risk | wave alignment | consumer / gate | hard-cap fit | fail action |
|---|---:|---:|---|---|---|---|---|
| `3D-SK17-D01` | 200-700 SK-V18 fold (D01 itself 0 doc) | 3 (ARCH §7.3, LOCKS Lock 1, SK-V17 SPEC monotonic) | high (eager retirement) + medium (encoding) | SK-V18 tape-adoption wave (3B) | 3A surface delta + 3C Lock-1 one-substrate closure; tape consumer same-wave | Fits doc-only T-P3; the 200-700 LOC is SK-V18, not T-P3. | Pass Omega blocks any dual-encoding closure wording; G-Omega gates the Lock-1 amendment. |
| `3D-SK17-D02` | 300-700 SK-V18 projection generator (D02 itself 0 doc) | 2 (ARCH value-plane, LOCKS Lock 14) | high | SK-V18 ValueRef-generator wave (3B) | 3A surface delta + 3C Lock-14; per-grammar value.rs/view.rs regen consumer | Fits doc-only T-P3; generator LOC is SK-V18. | Block any fleet-wide value-plane proof claim; JSON+CSS only; intrinsic-block Sheets/BBNF assertion. |
| `3D-SK17-D03` | 0-400 scope-reconcile (D03 itself 0 doc) | 3 (LOCKS Lock 16 + Lock 14, ARCH scan-leaf) | medium | SK-V18 NEON-manifest wave (3B/3E) | Lock-16 manifest row + checkasm parity; same-wave tape consumer | Fits doc-only T-P3 + manifest row. | aarch64-only; no x86/SVE close path; scalar-ref + checkasm mandatory or REJECT. |
| `3D-SK17-D04` | 0 (fence/lock prose) | 2 (LOCKS Lock 1 + Lock 10) | high (regression class) | SK-V18 eager-deletion wave (3B) | 3C ACCEPT under LAC-2F-FOLD-01; fold-deletion of `CssTypedValue`+pending Vecs | Fits doc-only T-P3. | Re-derivation of the eager tree in either tree REJECTS at CH3; intrinsic-block. |
| `3D-SK17-D05` | 0 (fence) | 2 (LOCKS Lock 1, SK-V17 §9 block) | high (28-65×/983×/10583× class) | SK-V18 fold gate (3B) | 3C ACCEPT under LAC-2F-FOLD-04; codegen-resolved-once gate | Fits doc-only T-P3. | Per-leaf `StructRegistry::layout` lookup REJECTS; REDRESS/revert any live per-leaf walk. |
| `3D-SK17-D06` | 0 (clarify) | 2 (LOCKS Lock 1 v+1 FactStream, SK-V17 §0.1 row 2) | medium | SK-V18 CSS-rebuild wave (3B) | 3C MODIFY/clarify; typed-output-plane survives, String-admission retires | Fits doc-only T-P3. | Block fact-stream-String CSS admission; diagnostic-only demotion. |
| `3D-SK17-D07` | 0 T-P3 (SK-V18 exercise) | 2 (LOCKS Lock 14, 3E artefact) | medium | SK-V18 Sheets/BBNF-self proof (3B/3E) | 3E future-grammar onboarding gate; Lock-14 grammar-neutral | Fits doc-only T-P3; proof is SK-V18 by-exercise. | Fleet-wide generality wording without a non-JSON witness REJECTS at CH2; route to 3E. |
| `3D-SK17-D08` | 0 (governance clause) | 3 (LOCKS Lock 2, MIGRATION, MASTER-PLAN) | high (direction-inversion regression) | All SK-V18 waves | 3B wave order + 3F migration; monotonic direction gate | Fits doc-only T-P3. | Any totality→skinny dictation or §9-name-into-skinny relocation REJECTS at CH3/CH5. |

## Cost And Non-Fit Fold

| receiver | cost posture | non-fit disposition |
|---|---|---|
| SK-V18 tape adoption (D01/D04/D05) | T-P2 LOC band: 200-700 LOC eager-OpenFrame retirement + AoS→SoA single-encoding closure + all-8 `OnceCell` substrate_target pre-gate (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:105`); the registry fence (D05) is 0 LOC but high regression-class risk. 3B owns the wave allocation. | A dual AoS/SoA end-state is a transient fold-state only; it is NOT a permissible Lock-1 closure (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:238`-`240`). If the SoA convergence does not fit one wave, parity-prove transiently then converge — never ship a parallel substrate. |
| SK-V18 ValueRef generator (D02) | 300-700 LOC projection generator retargeting the existing `@generated` regen path (`1d:135`); JSON+CSS-exercised. 3B owns the wave. | Sheets/BBNF-self value-plane proof is by-construction (SK-V18 by-exercise), not asserted at fold time. A CSS-only generator that never re-emits JSON FAILS CH2 and is intrinsic-block, not paper-close. |
| SK-V18 NEON manifest (D03) | 0-LOC narrative + 1 manifest row + 100-400 LOC scope-reconcile of multi-arch `crates/simd-scan` to the aarch64 set (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:155`). ORQ 2F-FOLD-U3 is the open scope decision. | If multi-arch kernels are retained as a fold-scope decision, x86 is STILL barred as a close path (no SVE, `SPEC:806`). Retention is an architecture-scope choice (U-SK17L-003), not an admission. |
| SK-V18 CSS `>SOTA` proof (D02/D06 consumer) | CSS speed is the SK-V18 PROOF OBLIGATION, not an SK-V17 result: `HANDOFF.md:44`-`45` "the >SOTA bar is NOT met and nothing on the CSS path moved"; `SPEC.md:207` "ALL per-corpus lightningcss endpoints are UNMEASURED-PENDING"; 3E P5b CSS tape-consumer NOT measured. 3B owns the SK-V18 CSS-rebuild wave. | No 3D delta asserts CSS `>SOTA` met. The fold absorbs the CONVERGED CONTRACT (lazy ValueRef + retired String-admission) whose CSS `>SOTA` bar SK-V18 must measure-prove against lightningcss; an artefact asserting CSS `>SOTA` proven FAILS CH7 (cf. the V2 CH7-S17-V2-R1 disposition that landed on 3F, `V2/CH7.md:153`-`180`). |

## Open Questions

| challenge lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 correctness | Does the SoA `Tape` shape replace the AoS `TapeRec` in SK-V18 (D01), or does crates/core keep AoS and prove parity? 3D affirms SoA-adopt as the proven-and-benched encoding. | T-P3 3C (Lock-1 one-substrate closure) + 3A surface delta; SK-V18 tape wave. | The convergence-target encoding must be NAMED before SK-V18 wiring; ORQ 2F-FOLD-U1 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:307`-`308`). | 3C asserts the Lock-1 post-fold one-substrate closure (`LOCKS.md:75` "parallel substrates are dead") as a CATALOGUED divergence; a dual encoding is NOT a permissible end-state (U-SK17L-002, `1d:156`). |
| CH2 generality | Which non-CSS witness — Sheets or BBNF-self — does SK-V18 exercise first to close the by-construction generality gap (D07)? | 3E grammar-generalisation + Lock 14 future-grammar gate. | `sheets_witness` is a 24-LOC stub with no `BackendRule` (`1d:96`); BBNF-self has no tape witness. The first-witness choice cannot weaken the CSS+Sheets/BBNF-self requirement. | Lock-14 future-grammar onboarding test (3E): fleet wording requires JSON+CSS proven AND a non-JSON-non-CSS witness by-exercise, else intrinsic-block (`1d:96`,`:109`). |
| CH3 regression | Are the 8 `OnceCell<StructuralIndex>` carriers each classified `existing_tape` vs `local_temp_only` BEFORE wiring (D01/D03)? | 3C Lock-1 substrate manifest + SK-V18 tape wave. | A retained index that runs parallel to the tape re-enters REDRESS-53; ORQ 2F-FOLD-U2 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:309`-`310`). | The F7 substrate_target classification GATES the F1/F3 tape-wiring it is co-waved with (`2f-fold-gaps.md:407`-`413`); a retained sidecar REJECTS at Lock-1. |
| CH4 cost | Does the 200-700 LOC eager-retirement + SoA closure fit one SK-V18 wave, or split (D01)? | 3B MASTER-PLAN wave reconciliation. | The eager-OpenFrame retirement (high risk) + AoS→SoA encoding (medium) may exceed one wave's cap. | 3B names the wave allocation with per-wave LOC, same-wave tape consumer, and hard-cap fit; non-fit is a split, never a dual-substrate transient shipped as closure. |
| CH5 hidden coupling | How does SK-V18 prove the live `arena.rs:47` `StructRegistry::compound_kind_for_layout` coupling is SEVERED, not relocated (D05)? | 3C Lock-1 fence + SK-V18 eager-retirement wave. | The coupling sits inside the eager arena path (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:207`-`210`); the eager-OpenFrame retirement (D04/F1) must sever it, not move it. | Lock-1 fence: `begin_compound` reads `layout.rule_id & 0x1F` only (grep-zero StructRegistry, fence-clean); a relocated per-leaf walk REJECTS. |
| CH6 anti-paper-close | Is the monotonic direction (D08) enforced by a named gate, or merely asserted prose? | 3B wave order + 3F migration + Pass Omega CRUD. | A totality→skinny dictation or §9-name-into-skinny relocation must REJECT, not be flagged-and-deferred. | The monotonic direction gate: SK-V18 adopts skinny `Tape`/`ValueRef` into crates/core; the §9 names are FORBIDDEN-IN-SKINNY (`1d:128`); any inversion is a CH3/CH5 REJECT with a named receiver, not a future-cycle defer. |
| CH7 overfit/scope-honesty | Does any 3D claim read the CSS `>SOTA` bar as MET rather than UNMEASURED-PENDING? 3D affirms it does not — the SCOPE-HONESTY BANNER + the SK-V18 CSS-`>SOTA` non-fit row mark CSS speed as an SK-V18 obligation. | SK-V18 CSS-rebuild wave (3B) + lightningcss endpoint measurement; coherent with 3E P5b and the V2 CH7-S17-V2-R1 disposition on 3F. | CSS endpoints are UNMEASURED-PENDING (`HANDOFF.md:44`-`45`, `SPEC.md:207`); the SK-V17 CONTRACT converged but the CSS speed bar is not met. | A 3D assertion that CSS `>SOTA` is proven REJECTS at CH7; the banner forces a G3 skim to read CSS speed as predicted/SK-V18-pending, not proven. |
