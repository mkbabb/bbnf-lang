# Pass Omega V10 - Omega-A V1 Spec Coherence Audit

Date: 2026-06-01.
Worker: Omega-A V1 spec coherence (SK-V18 generalization cycle, astral V10).
Scope: SK-V18 totality closure (T-P1/T-P2/T-P3 converged + skinny S-P0..S-P3 certified) against the live V1 governance surfaces.
Write path: `restart/audit/totality/astral/V10/ΩA-coherence-audit.md`.

## Verdict

REVISE REQUIRED before SK-V18 CRUD merge.

The 5-shape `BackendShape` canon is COHERENT and load-bearing-intact across every
surface (verified byte-identical below); the staged locks diff (the V10-hardened
consolidation OF the converged 3C diff — diff body differs from 3C by the V1–V3
hardening deltas, not byte-identical) applies cleanly
(`git apply --check` exit 0); every cited SHA and every spot-checked delta anchor
resolves at the live file:line. The corpus is NOT internally consistent on the one
material axis the SK-V18 generalization pivot turns on: the live HANDOFF (`:17`-`19`),
the live MASTER-PLAN §13.6, the live ARCHITECTURE §0 authority block, and the live
ARCHITECTURE §7.4 status title all carry an OBSOLETE definition — they call the
`crates/core/` tape-fold adoption "SK-V18", and they route from SK-V15/SK-V17
authority text. The CERTIFIED SK-V18 (`restart/skinny/tranches/sk-v18/SPEC.md:19`-`21`,
`:58`-`61`) is a DIFFERENT tranche: the GENERALIZATION cycle on the SKINNY tree
(un-fork JSON+CSS into ONE `.bbnf`-driven generator emitting JSON+CSS+Sheets,
aarch64-only, net ≈ −10800 LOC, 12-wave PRUNE→G1..G6→PROVE→H1). The `crates/core/`
adoption the live surfaces label SK-V18 is now SK-V19.

The T-P3 3A/3B/3F deltas are MUTUALLY COHERENT — all three name this single
drift (COH18-001) identically and none would create a NEW cross-document
inconsistency when merged; they CORRECT the existing one. No live surface should
continue to assert SK-V18 = `crates/core/` adoption, the phantom `G:EventGrammar`
generality vehicle, the x86-pinned CollapsedStage lowering text, the CSS courier
as generated provenance, or the falsified Lock-14 self-gate once the CRUD lands.

## Source Map

| Source | Coherence role |
|---|---|
| `restart/audit/totality/p3/3A-architecture-synthesis.md:159`-`275` | The 14 SK-V18 ARCHITECTURE deltas (V4 EXTENSION) audited here. |
| `restart/audit/totality/p3/3B-master-plan-reconciliation.md:27`-`55` | The §13.6→SK-V19 re-key + §13.7 SK-V18 GENERALIZATION block. |
| `restart/audit/totality/p3/3F-migration-handoff.md:35`-`50` | The MIGRATION/HANDOFF scope-pivot delta (3F-MH-001/008..013). |
| `restart/audit/totality/p3/3C-locks-v+1-diff.md:52`-`90` | The staged locks diff (`git apply --check` exit 0). |
| `restart/skinny/tranches/sk-v18/SPEC.md:19`-`21`,`:58`-`61` | Certified SK-V18 = skinny generalization; `skinny/crates/` benched, `crates/core/` is SK-V19. |
| `restart/skinny/tranches/sk-v18/SPEC.md:431`-`447` | Certified 12-wave manifest (5 PRUNE + G1..G6 + PROVE + H1). |
| `restart/ARCHITECTURE.md:19`-`37` | Live §0 authority block — still "SK-V15 current authority (2026-05-28, G-Omega V9 CRUD-1)". |
| `restart/ARCHITECTURE.md:1088`-`1116`,`:1178`-`1206` | Live §7.3 `BackendShape` enum + per-shape lowering table + admission ledger. |
| `restart/ARCHITECTURE.md:1371` | Live §7.4 title "SK-V5 Through SK-V15 Implementation Status". |
| `restart/ARCHITECTURE.md:1990`-`2008` | Live §9.2 phantom `G:EventGrammar` "generality vehicle" prose. |
| `restart/MASTER-PLAN.md:974`-`1027` | Live §13.6 "SK-V18 Tape-Fold Adoption Receiver Block" (MP.SK18.W0-W6). |
| `restart/HANDOFF.md:3`-`23` | Live override — Pass Omega V5 SK-V17; SK-V18 defined as `crates/core/` adoption. |
| `restart/MIGRATION.md:30`-`40` | Live §0.0 SK-V17 tape-fold migration receiver. |
| `restart/locks/LOCKS.md:106`-`109` | FactStream-not-6th-shape; 5-shape Lock-10 search domain. |
| `restart/locks/LOCKS.md:610`-`618` | SK-V17 T-P3 crystallisation addendum; 16-lock count, 5-shape canon verbatim. |
| `skinny/COMPILER.md:53`-`55`,`:115`-`118`,`:334`-`344` | 5-shape canon + per-shape lowering table (the skinny COMPILER spec). |

Live verification (commands run at HEAD):

```text
git rev-parse: 33b51d8f4 2a76916ac 7157be073 1c5bd7a25 91b6893b0 139ab1e4a 83b66db42 85a043224  -> ALL RESOLVE
lock_count (grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md)             = 16
backend_shape_variants (skinny/crates/ir/src/lib.rs:341-345)             = 5  {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}
3C-locks-v+1-diff git apply --check (extracted :53-89 -> live LOCKS.md)  = EXIT 0 (APPLIES CLEANLY)
ARCH §0 authority line 19                                                = "SK-V15 current authority (2026-05-28, G-Omega V9 CRUD-1)"  [STALE]
ARCH RuntimeEmitterKind count                                            = 0  (un-fork gap: live in skinny codegen, absent in ARCH)
skinny/crates/codegen/src/runtime_generator.rs:1,17,25                   = RuntimeEmitterKind{CompiledLowering,RequestFacts} fork present
crates/ir/src/registry/strategy.rs:137,143,149,155                       = 9-ident grammar-named table present (JsonParser/JsonGrammar, …)
crates/core/src/css_types.rs                                             = present (2373 bytes, generic core)
ARCHITECTURE.md:1186,:1151,:1171,:1206                                   = x86 NASM path + target.arch==x86 hard-coded (stale vs aarch64-only)
ARCHITECTURE.md:1998                                                     = "The G:EventGrammar type parameter is the generality vehicle"  [phantom]
```

## 5-Shape BackendShape Canon Coherence (PASS)

The single most safety-critical cross-document invariant. Verified byte-identical
`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` across all four
surfaces the dispatch task names plus LOCKS:

- ARCHITECTURE §7.3 — `restart/ARCHITECTURE.md:1088`,`:1091`-`1114` (enum), `:1237` (Lock-10 domain), `:1428`.
- MASTER-PLAN §H — `restart/MASTER-PLAN.md:616` (Lock 10 row), `:945`,`:996`-`997`.
- skinny COMPILER.md — `restart/skinny/COMPILER.md:54`,`:116`,`:338`-`342` (per-shape table).
- LOCKS — `restart/locks/LOCKS.md:108`,`:599`,`:618`.

The skinny `BackendShape` enum (`skinny/crates/ir/src/lib.rs:341`-`345`) and
`cost.rs:121`-`131` carry exactly five variants. The 3A V4 deltas D06 (5-shape
re-frame as POSITIVE neutral dispatch axis) and D08 (CollapsedStage diagnostic-only
slot) ADD NO sixth shape and RENUMBER nothing; D06's re-frame is coherent with 3E's
per-grammar shape matrix (`ARCHITECTURE.md:1293`-`1311`) and 3B's §13.6/§13.7 wave
map. CANON HOLDS POST-DELTA. The only canon-adjacent text that is STALE is the
x86-pinning of the `CollapsedStage` ROW (not the variant) — see OA-V10-05.

## Findings

### OA-V10-01 - HANDOFF defines SK-V18 as the WRONG tranche (the load-bearing drift)

Target surface: `restart/HANDOFF.md`. Owner: CRUD-4 (HANDOFF+MIGRATION).

Evidence:
- `restart/HANDOFF.md:17`-`19` says "The next IMPLEMENTATION tranche is **SK-V18**: it adopts the SKINNY-proven unified-tape / lazy-`ValueRef` / shared-NEON model into the totality `crates/core/` tree."
- Certified SK-V18 is the GENERALIZATION cycle on the SKINNY tree, verifiable by grepping `skinny/crates/`, NOT `crates/core/` (`restart/skinny/tranches/sk-v18/SPEC.md:19`-`21`,`:58`-`61`).
- 3A-D14, 3B exec-summary, and 3F-MH-001 all flag this single drift as COH18-001 (`restart/audit/totality/p3/3A-architecture-synthesis.md:220`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:29`-`37`; `restart/audit/totality/p3/3F-migration-handoff.md:43`-`47`).

Disposition: update required. The HANDOFF top-level state must name SK-V18 = skinny generalization (12-wave PRUNE→G1..G6→PROVE→H1) and tee `crates/core/` adoption to SK-V19. Routing note, not a closure.

### OA-V10-02 - HANDOFF override is Pass Omega V5 / SK-V17, not V10 / SK-V18

Target surface: `restart/HANDOFF.md`. Owner: CRUD-4.

Evidence:
- `restart/HANDOFF.md:3`-`14` says "Status: **Pass Omega V5 SK-V17 tape-fold G-Omega is CLOSED**" and routes current skinny authority through `restart/skinny/tranches/sk-v17/DISPATCH-PROMPT.md`.
- The current astral pass is V10 (`restart/audit/totality/astral/V10/`), consuming the converged SK-V18 T-P1/T-P2/T-P3 + certified skinny S-P0..S-P3; G-Omega is PENDING, not closed.
- 3F re-roots the override carrier onto the SK-V18 generalization receiver and the W-PRUNE-first dispatch lock (`restart/audit/totality/p3/3F-migration-handoff.md:25`,`:28`).

Disposition: update required. CRUD-4 (HANDOFF) and Ω-F own the replacement state block.

### OA-V10-03 - HANDOFF/3F carry the stale "Pass Omega V6" index against the live V10 pass

Target surfaces: `restart/HANDOFF.md`, `restart/audit/totality/p3/3F-migration-handoff.md`; audit citation hygiene.

Evidence:
- 3F frontmatter and exec-summary call the next astral pass "Pass Omega V6" (`restart/audit/totality/p3/3F-migration-handoff.md:6`,`:22`,`:42`), reasoning V5 closed for SK-V17 at `33b51d8f4`.
- The astral directory lineage is sequential V1..V10; V9 closed (`restart/audit/totality/astral/V9/G-OMEGA-SIGNOFF.md:1`-`6`, CLOSED 2026-05-28), and the active pass is V10. The "V6" label is a T-P3 cycle-local artefact, not the astral index.

Disposition: citation harmonization required. Ω-F next-cycle directive and the CRUD-4 HANDOFF block must name **Pass Omega V10** as the active pass; do NOT carry "V6" into a live surface. Not a content amendment.

### OA-V10-04 - ARCHITECTURE §0 authority block is still "SK-V15 current authority"

Target surface: `restart/ARCHITECTURE.md`. Owner: CRUD-1 (ARCHITECTURE).

Evidence:
- `restart/ARCHITECTURE.md:19` declares "SK-V15 current authority (2026-05-28, G-Omega V9 CRUD-1)" with T-P3 V5 final-convergence and SK-V15 W0-W11 as the implementation contract.
- 3A-D01/D14 require the §0/§7.4 authority text to route from the certified SK-V18 skinny-generalization scope + SK-V19 totality-fold tee-up (`restart/audit/totality/p3/3A-architecture-synthesis.md:207`,`:220`).
- Current totality inputs are SK-V18 T-P1/T-P2 near-converged NON-normal-§3Z + T-P3 in-cycle hardening (per 3F V3-FOLD CH1-V3-C5, `restart/audit/totality/p3/3F-migration-handoff.md:27`), not the SK-V15 frame.

Disposition: update required. Replace the SK-V15 authority block with the SK-V18 generalization authority; preserve the 16-lock / 5-shape canon statement verbatim.

### OA-V10-05 - ARCHITECTURE §7.3 CollapsedStage text is x86-pinned against the aarch64-only plane

Target surface: `restart/ARCHITECTURE.md`. Owner: CRUD-1.

Evidence:
- `restart/ARCHITECTURE.md:1186` hard-codes the per-grammar NASM file `skinny/crates/bbnf-simd/src/x86_64/{grammar}_collapsed.asm`; `:1151`,`:1171`,`:1206` hard-code `target.arch == x86` as the CollapsedStage co-require.
- Certified SK-V18 is aarch64 / Apple M5 Max ONLY; the whole x86 surface is a PRUNE target (P1, ≈ −4500 LOC), not a measured plane (`restart/skinny/tranches/sk-v18/SPEC.md:42`-`43`; `restart/audit/totality/p3/3A-architecture-synthesis.md:213` = 3A-D07).
- 3A-D08 keeps the SHAPE SLOT in the canon (G-Omega-gated, no retirement) but marks the lowerer body diagnostic-only / aarch64-gated (`restart/audit/totality/p3/3A-architecture-synthesis.md:214`).

Disposition: update required. Demote the x86-pinned lowering text to a diagnostic/historical footnote; keep CollapsedStage IN the 5-shape canon. NOT a canon retirement — the SHAPE persists; the x86-NASM artefact text is what is stale.

### OA-V10-06 - ARCHITECTURE §9.2 leans on the phantom `G:EventGrammar` generality vehicle

Target surface: `restart/ARCHITECTURE.md`. Owner: CRUD-1.

Evidence:
- `restart/ARCHITECTURE.md:1998` asserts "The `G:EventGrammar` type parameter is the generality vehicle."
- The certified plan DELETES the `<G>` axis (G4: `Cursor` micro-trait replaces it); the on-disk non-test instantiation census of `<G>` is EMPTY (`1A-SUB-023`/`025`/`026`; `restart/audit/totality/p3/3A-architecture-synthesis.md:207` = 3A-D01).
- The generality claim survives intact on the two axes the clause already names: the R-D `Cursor` micro-trait (a VIEW, REDRESS-fenced) and the config-breadth classifier (alphabet-as-data across 8/9 grammars, live at `:2005`).

Disposition: update required. Strike the one phantom sentence; re-anchor on `Cursor` + config-breadth. The §9.2 "G-Omega CLOSED" annotation on the lazy-`ValueRef` plane (`:1990`) re-opens as a NEW SK-V18 obligation (3A V4 Open Question CH1; the phantom delete is in-flight). Sibling to the 3C Lock-14 ValueRef-clause strike (`1A-LOCK1-AMEND-001`).

### OA-V10-07 - ARCHITECTURE §7.4 title + content is "SK-V5 Through SK-V15", CSS audit-demotion stale

Target surface: `restart/ARCHITECTURE.md`. Owner: CRUD-1.

Evidence:
- `restart/ARCHITECTURE.md:1371` titles §7.4 "SK-V5 Through SK-V15 Implementation Status" — no SK-V16/V17/V18.
- The §7.3 admission ledger (`:1205`) and the grammar-generality matrix (`:1307`) still mark CSS SinkOnly "audit-demoted by PASS-IMPL V1 pending SK-V15 W5/W6"; CSS >SOTA is now skinny-PROVEN honestly (CSS beats lightningcss 1.66–3.38×, `restart/skinny/tranches/sk-v18/SPEC.md:21`), with the residual being the verbatim-blob courier, not broadcast.
- 3A-D05 records the CSS `generated.rs` as a `CSS_GENERATED_RS` verbatim `&str` courier (the CSS generator does not yet exist), a Lock-6-v+1 violation, NOT broadcast/wrong-plane (`restart/audit/totality/p3/3A-architecture-synthesis.md:211`).

Disposition: update required. Re-title §7.4 to span through SK-V18; replace the SK-V15-W5/W6 CSS-broadcast demotion frame with the SK-V18 courier-prohibition frame (round-trip byte-equivalence against the deletable oracle = binding proof; courier deleted at G2 after the same-wave provider).

### OA-V10-08 - MASTER-PLAN §13.6 "SK-V18 Tape-Fold Adoption" must re-key to SK-V19; §13.7 SK-V18 GENERALIZATION block is absent

Target surface: `restart/MASTER-PLAN.md`. Owner: CRUD-2 (MASTER-PLAN).

Evidence:
- `restart/MASTER-PLAN.md:974` titles §13.6 "SK-V18 Tape-Fold Adoption Receiver Block" with waves MP.SK18.W0-W6 — the `crates/core/` adoption (`:1018`,`:1020`).
- The H.W1 row routes "MP.SK18.W0 GATES MP.SK18.W2 AoS→SoA" (`:642`) and H.W4 routes "MP.SK18.W5 WIRES the skinny selector into core" (`:646`) — all the OLD SK-V18 = core-adoption frame.
- 3B re-keys §13.6 to a SK-V19 block, adds a NEW §13.7 SK-V18 GENERALIZATION receiver mapping the certified 12 waves with same-wave consumers + exit-gate falsifiers, and tees SK-V19 with the three carried totality leaks (the 9-ident `strategy.rs` table, `css_types.rs`, the `simd-scan` scanner asymmetry) (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:37`-`44`).

Disposition: update required. Ω-D owns the staged master-plan diff (`master-plan-diff.md`). CRUD-2 must NOT re-number §13.6's MP.SK18.* IDs in place silently — re-key the BLOCK to SK-V19 and add §13.7 as the SK-V18 receiver; preserve the 5-shape canon row at `:616`/`:945`/`:996` verbatim.

### OA-V10-09 - MIGRATION §0.0 receiver is SK-V17 tape-fold; SK-V18 generalization receiver absent

Target surface: `restart/MIGRATION.md`. Owner: CRUD-4.

Evidence:
- `restart/MIGRATION.md:30`-`40` defines the current receiver as "Pass Omega V5 SK-V17 tape-fold G-Omega CLOSED 2026-05-30", with CSS >SOTA as "the SK-V18 proof obligation, bar NOT yet met".
- SK-V18 now routes five concrete migration actions: x86 crate-wide DELETE (P1, ≈ −4500), `CSS_GENERATED_RS` courier RETIRE (G2), 7-replica + 7-`RuntimeTarget`-row COLLAPSE (P3, ≈ −5500), phantom `<G>` DELETE (G4), `css_types.rs` relocation (3F-MH-008..013; `restart/audit/totality/p3/3F-migration-handoff.md:48`-`50`).

Disposition: update required. Ω-F owns the staged MIGRATION delta. Add the SK-V18 generalization migration receiver ABOVE the historical SK-V17/SK-V15 records; CSS >SOTA is skinny-proven (the obligation is now courier-deletion + honesty close, not a parity bar).

### OA-V10-10 - Lock-14 self-gate is FALSIFIED/RED on the totality tree (the 9-ident leak)

Target surfaces: `restart/ARCHITECTURE.md` (§7.4/§13.1), `restart/locks/LOCKS.md` (via 3C green-by-exclusion). Owner: CRUD-1 + CRUD-3 (LOCKS).

Evidence:
- The live Lock-14 self-gate asserts its command returns ZERO but returns 13 live grammar-named sites; the 9-row grammar-named `idents` table is at `crates/ir/src/registry/strategy.rs:137`-`185` (verified: `JsonParser`/`JsonGrammar`, `GoogleSheetsParser`/`GoogleSheetsGrammar`, `CssL4Parser`, `BbnfBootstrap`/`BbnfParser`, …), consumed via `for_grammar_with_manifest`.
- `crates/core/src/css_types.rs` (verified present, 2373 bytes) is the lock-NAMED mess in a generic crate.
- 3A-D11 SPLITS this: D11a = the SK-V18 skinny P4 green-by-exclusion gate (≈ +15, promote leak roots into strict `GENERIC_SCAN_ROOTS`, extend `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}`, drop `diagnostic-x86`); D11b = the SK-V19 totality R16 9-ident structural row-collapse (≈ +217, SK-V19-owned, NOT laundered into +15) (`restart/audit/totality/p3/3A-architecture-synthesis.md:217`,`:260`).

Disposition: update required. The SK-V18 P4 green-by-exclusion fix (D11a) lands in the skinny tree BEFORE G2/G3; the totality 9-ident collapse (D11b) tees to SK-V19. The CRUD must NOT bolt a 9-name regex widen as an SK-V18 patch. The `FORBIDDEN_GENERIC_TOKENS` set must be written BYTE-IDENTICAL across 3A-D11 / 3C green-by-exclusion / 3B-P4 / 3D-D04 / the v+1 diff (verified converged in the T-P3 packet, `restart/audit/totality/p3/3C-locks-v+1-diff.md:26`).

### OA-V10-11 - Un-fork seam: `RuntimeEmitterKind` is an undocumented Lock-5 fork (live in skinny, absent in ARCH)

Target surface: `restart/ARCHITECTURE.md` (§10/§7.3). Owner: CRUD-1.

Evidence:
- `skinny/crates/codegen/src/runtime_generator.rs:1`,`:17`,`:25` carry `RuntimeEmitterKind{CompiledLowering, RequestFacts}` as a grammar-family emission fork; ARCHITECTURE references it ZERO times (verified `grep -c == 0`).
- 3A-D03/D04 require §10/§7.3 to record the single `render(program)` emitter dispatching on the cost-DERIVED `BackendShape` (`cost.chosen`), with the `RuntimeEmitterKind` fork DELETED (a PATH change, not a new primitive) and the `emit_shape_source==lowered_program` relocated-seam firewall + the `runtime_target_rows_collapsed` full-row `PartialEq` co-gate MANDATORY (`restart/audit/totality/p3/3A-architecture-synthesis.md:209`-`210`).
- The SECOND-SEAM re-path (CH5-DEFECT-V1-02): the SK-V18 G2 SKINNY firewall scans the SKINNY CSS surface only; the totality `crates/core/src/runtime/css_l4/` surface is the SK-V19-adoption seam (same DEFER bundle as the cursor-generality/scanner-unification reconcile), NOT a skinny-cycle scan target.

Disposition: update required. The ARCH un-fork text (D03/D04) is coherent with 3D-D04 and the 3B G3 wave; the firewall must read shape ONLY from `program.policy_summary.backend_shape`. The CRUD must keep the skinny-vs-totality firewall scope distinct (a skinny-cycle firewall must not silently target the totality tree).

### OA-V10-12 - T-P3 3A/3B/3F cross-coherence: NO new inconsistency introduced (PASS, with one harmonization)

Target surfaces: the three staged delta packets. Owner: Ω-C/Ω-D/Ω-F authors.

Evidence:
- 3A (ARCHITECTURE), 3B (MASTER-PLAN), 3F (MIGRATION/HANDOFF) name the SK-V18 scope pivot (COH18-001) IDENTICALLY: `crates/core/` adoption is SK-V19; SK-V18 = skinny generalization (`restart/audit/totality/p3/3A-architecture-synthesis.md:220`; `:3B:37`; `:3F:43`-`47`).
- The 5-shape canon is preserved by addition in all three (3A-D06/D08 add no shape; 3B preserves the canon row; 3C adds no sixth shape, `restart/audit/totality/p3/3C-locks-v+1-diff.md:99`).
- The net-LOC figure is harmonized: the per-wave SPEC sum P1 −4500 + P2 −700 + P3 −5500 + P4 +15 + P5 0 ≈ −10685 (3B V1-FOLD CH4-V1, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:23`); the SPEC top-line states ≈ −10800 (`restart/skinny/tranches/sk-v18/SPEC.md:22`). The ≈115-LOC gap is the rounding band between the wave-sum and the campaign top-line — both figures should be cited together, never one as a contradiction of the other.

Disposition: PASS for cross-document coherence; one harmonization — Ω-D and Ω-F should cite the net-LOC figure as "≈ −10800 campaign (per-wave SPEC sum ≈ −10685)" so the two figures never read as a discrepancy. The Pass-Omega index label ("V6" in 3F vs the live "V10") is the OA-V10-03 harmonization.

## Cohesion Fixes The CRUD Must Apply

| # | Cohesion fix | Surface(s) | Owner |
|---|---|---|---|
| CF-01 | Replace SK-V18 = `crates/core/` adoption with SK-V18 = skinny generalization (12-wave PRUNE→G1..G6→PROVE→H1); tee `crates/core/` adoption to SK-V19. | HANDOFF, MASTER-PLAN §13.6, MIGRATION §0.0, ARCH §0/§7.4 | CRUD-1/2/4 |
| CF-02 | Update active-pass label to **Pass Omega V10** (G-Omega pending); strike the "Pass Omega V5/V6/SK-V17 CLOSED" current-state. | HANDOFF, MIGRATION; Ω-F directive | CRUD-4 / Ω-F |
| CF-03 | Replace ARCH §0 "SK-V15 current authority" block with the SK-V18 generalization authority (T-P1/T-P2 near-converged NON-normal-§3Z + T-P3 in-cycle hardening). | ARCHITECTURE §0 | CRUD-1 |
| CF-04 | Re-title §7.4 to span through SK-V18; swap the CSS-broadcast/W5-W6 demotion frame for the courier-prohibition frame (byte-equivalence oracle proof). | ARCHITECTURE §7.4, §7.3 ledger | CRUD-1 |
| CF-05 | Demote the x86-pinned `CollapsedStage` lowering/`target.arch==x86` text to diagnostic/historical; KEEP CollapsedStage in the 5-shape canon (no retirement). | ARCHITECTURE §7.3 (:1186/:1151/:1171/:1206) | CRUD-1 |
| CF-06 | Strike the phantom `G:EventGrammar` "generality vehicle" sentence (:1998); re-anchor on `Cursor` micro-trait + config-breadth; re-open the §9.2 "G-Omega CLOSED" lazy-`ValueRef` annotation as an in-flight SK-V18 obligation. | ARCHITECTURE §9.2 | CRUD-1 |
| CF-07 | Re-key MASTER-PLAN §13.6 block to SK-V19; add §13.7 SK-V18 GENERALIZATION 12-wave receiver with same-wave consumers + exit-gate falsifiers; preserve canon rows verbatim. | MASTER-PLAN §13.6/§13.7 | CRUD-2 (Ω-D diff) |
| CF-08 | Add the SK-V18 generalization MIGRATION receiver (x86 DELETE, courier RETIRE, 7-replica COLLAPSE, phantom `<G>` DELETE, `css_types.rs` relocation) above the historical SK-V17/V15 records. | MIGRATION | CRUD-4 (Ω-F diff) |
| CF-09 | Land the P4 green-by-exclusion Lock-14 fix (D11a) with the converged byte-identical `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}`; tee the totality 9-ident R16 collapse (D11b) to SK-V19 — do NOT bolt a 9-name regex widen as an SK-V18 patch. | ARCH §7.4/§13.1, LOCKS (3C green-by-exclusion) | CRUD-1/3 |
| CF-10 | Record the `RuntimeEmitterKind` un-fork (DELETE the fork, dispatch on lowered `BackendShape`) + the `emit_shape_source==lowered_program` firewall + the `runtime_target_rows_collapsed` `PartialEq` co-gate in §10/§7.3; keep the skinny-vs-totality firewall scope distinct. | ARCHITECTURE §10/§7.3 | CRUD-1 |
| CF-11 | Cite net LOC as "≈ −10800 campaign (per-wave SPEC sum ≈ −10685)" everywhere; never present one figure as a contradiction of the other. | MASTER-PLAN, MIGRATION, HANDOFF | CRUD-2/4 (Ω-D/Ω-F) |
| CF-12 | Preserve the 16-lock count + the 5-shape `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` canon BYTE-VERBATIM across ARCH §7.3 / MASTER §H / COMPILER.md / LOCKS in every CRUD edit; no renumber, no sixth shape. | ALL | CRUD-1/2/3/5 |

## CRUD Routing Summary

| Owner | Required operations |
|---|---|
| CRUD-1 ARCHITECTURE | CF-03..CF-06, CF-10; apply the companion `architecture-delta.staged.md` (two `git apply`-gated hunks — §7.4 title, §9.2 phantom strike — exit 0; four anchored re-grep-HALT splices); record the 14 SK-V18 deltas (D01-D14) per 3A V4; preserve 16 locks + 5-shape canon. |
| CRUD-2 MASTER-PLAN | CF-07, CF-11; apply Ω-D `master-plan-diff.md` (re-key §13.6→SK-V19, add §13.7); preserve canon rows. |
| CRUD-3 LOCKS | Apply the Ω-C `locks-diff.md` (the V10-hardened consolidation OF the 3C-locks-v+1-diff — diff body differs from 3C by the V1–V3 hardening deltas; `git apply --check` exit 0 confirmed); 16-lock count + 5-shape canon preserved, no renumber; CF-09 green-by-exclusion token set byte-identical. |
| CRUD-4 HANDOFF + MIGRATION | CF-01, CF-02, CF-08; apply Ω-F staged HANDOFF + MIGRATION deltas; route from SK-V18 generalization + SK-V19 tee-up; name Pass Omega V10. |
| CRUD-5 SKINNY CORPUS | Sync `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md` to SK-V18 anchors (one-generator architecture, §6 (a)-(d) discipline, G6=WIRE, track1_rich bit-rot fix); preserve the COMPILER.md 5-shape canon verbatim. Ω-E owns the per-surface staged diff. |
| CRUD-6 AUDIT + CLEANUP | Run the citation scrub: confirm all cited SHAs resolve, the 3C diff applies, the 5-shape canon byte-identical post-merge, and the "Pass Omega V6"→"V10" + net-LOC harmonization (CF-11) carried no stale label into a V1 surface. |

## CRUD-1 ARCHITECTURE Staged-Edit Budget (per-finding LOC + propagation)

The ARCHITECTURE leg now carries a companion `git apply`-gated delta file
`restart/audit/totality/astral/V10/architecture-delta.staged.md` (R9 fix): the two
cleanest edits (the §7.4 title re-key and the §9.2 phantom-vehicle strike) are
emitted there as byte-exact unified hunks that `git apply --check` exit 0 against
live `restart/ARCHITECTURE.md` at HEAD, and the remaining four multi-span edits
carry byte-exact re-grep-HALT anchor strings — so the CRUD-1 operator can verify
every edit lands where stated BEFORE applying, the same bounded guarantee
`locks-diff.md` gives LOCKS. To carry the same per-edit LOC budget + propagation
cost the other surfaces carry (the CH4 requirement that every staged amendment be
budgeted + bounded), each OA-V10 ARCHITECTURE finding is line-anchored and budgeted
below; the four anchored-splice edits apply against the cited spans (re-grep the
first-line anchor string before each edit — these are line-range prose
replacements, so a moved anchor HALTS the edit, it does not force-apply).

| Finding | Target span | Edit kind | LOC budget | Propagation (sites) |
|---|---|---|---:|---|
| OA-V10-04 (CF-03) | §0 authority block `ARCHITECTURE.md:19`-`37` | block replace (SK-V15→SK-V18 authority) | ≈ +12 / −18 (net ≈ −6) | 1 site (§0) |
| OA-V10-05 (CF-05) | §7.3 CollapsedStage x86-pin `:1151`/`:1171`/`:1186`/`:1206` | demote-to-diagnostic (4 in-body splices; SHAPE SLOT retained) | ≈ +8 / −10 | 4 sites |
| OA-V10-06 (CF-06) | §9.2 phantom vehicle `:1998` + `:1990` annotation | strike 1 sentence + re-anchor on `Cursor`/config-breadth; re-open `:1990` lazy-`ValueRef` as in-flight | ≈ +6 / −3 | 2 sites (§9.2) |
| OA-V10-07 (CF-04) | §7.4 title `:1371` + CSS demotion frame `:1205`/`:1307` | re-title (SK-V5→SK-V18) + swap demotion frame for courier-prohibition | ≈ +10 / −8 | 3 sites |
| OA-V10-10 (CF-09) | §7.4 Pattern-H census row `:1398` + §13.1 fence-canon lint `:2402` (both byte-exact ADD-after-anchor; see architecture-delta rows 5a/5b) | record D11a skinny `+15` (P4 green-by-exclusion) inline; tee D11b (≈+217) to SK-V19; do NOT bolt the 9-name widen | ≈ +6 (ARCH prose; the +15/+217 are the LOCKS/`strategy.rs` code costs, NOT this prose edit) | 2 sites (§7.4 `:1398`, §13.1 `:2402`) |
| OA-V10-11 (CF-10) | §7.3 un-fork render `:1274` (byte-exact anchor) + §10 named ADD `:2146` (end of §10.1; see architecture-delta rows 6a/6b) | add `render(program)` un-fork + `emit_shape_source==lowered_program` firewall + the PLANNED `runtime_target_rows_collapsed` co-gate; keep skinny-vs-totality firewall scope distinct | ≈ +14 | 2 sites (§7.3 `:1274` anchored, §10 `:2146` named ADD) |

ARCHITECTURE CRUD-1 staged-edit total: ≈ +56 / −39 prose LOC across 14 sites
(net ≈ +17; per-row deletions: −18 + −10 + −3 + −8 = −39), no canon retirement, no 6th shape, 16-lock count preserved verbatim.
The +15 (skinny D11a) and +217 (SK-V19 D11b) are CODE budgets owned by CRUD-3/SK-V19,
NOT this ARCHITECTURE prose edit, and are not double-counted here.

## No Live Edits

This audit proposes CRUD ownership + cohesion fixes only. It does NOT edit
`restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`,
`restart/HANDOFF.md`, `restart/MIGRATION.md`, the skinny corpus surfaces, or any
generated source. The staged locks diff (hardened-FROM-3C), the Ω-D master-plan diff, and the Ω-F
migration/handoff deltas are STAGED ONLY under `restart/audit/totality/astral/V10/`;
the actual CRUD merge executes POST-G-Omega after user sign-off.
