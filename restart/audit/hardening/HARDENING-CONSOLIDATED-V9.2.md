# HARDENING-CONSOLIDATED-V9.2 — Lazy-Tape Lock 1 Amendment Cohort

V9.2 consolidates the four-target V1-corpus hardening audit of the lazy-offset tape amendment proposal at `restart/skinny/audit/LAZY-TAPE-DESIGN.md`. The audit was dispatched in parallel with the SK-V2 amendment cohort that applies the per-quadrant skinny text-propagation punch list.

## §1 Cycle identification

| Field | Value |
|---|---|
| Cycle | V9.2 (V1-corpus hardening cycle dispatched against the LAZY-TAPE-DESIGN.md proposal) |
| Trigger | SK-V2 returned `SK-AMENDMENT-REQUIRED-NARROW` + 6th-agent LAZY-TAPE-DESIGN.md proposal identified the lazy-offset tape as the architectural amendment surface for Lock 1 |
| Cohort size | 4 V1 hardeners (PASS-1, PASS-2, PASS-3, MASTER-PLAN trio) — Plan agents, read-only audit |
| Co-dispatched | 5 SK-V2 amendment agents (general-purpose, Edit access) applied 60+ item skinny text-propagation in parallel |
| Reports landed | `HARDENING-{PASS-1,PASS-2,PASS-3,MASTER-PLAN}-V9.2.md` |
| Predecessor verdict | V9.1 closed READY-with-narrow-residue across the four targets |
| Audit posture | Steelman the design; produce per-target absorption punch list; do NOT amend the V1 corpus pre-implementation |
| Lens set | A-K (V1 HARDENING.md V8+ contract) |

## §2 Cohort verdict matrix

| Target | Final decision | Punch-list size | Load-bearing item |
|---|---|---:|---|
| PASS-1 | AMENDMENT-REQUIRED-NARROW | 11 surgical (P1-P11) | Lock 1 amendment (P1 + P5 + P6); REINVENT for Lock 1 verbatim text per design §4.1 |
| PASS-2 | AMENDMENT-REQUIRED-NARROW | (see PASS-2 V9.2 §12) | BIR `TapeEmit`/`DirectBuild` payload mode-branching; codegen template branches on `tape_mode` |
| PASS-3 | AMENDMENT-REQUIRED-NARROW | (see PASS-3 V9.2 §12) | ValueRef `cursor: u32` semantics; identity invariant restatement; `ReparsePlan` per-mode reuse-range |
| MASTER-PLAN trio | AMENDMENT-REQUIRED-NARROW (conditional on re-bench A/B/C) | 11 sites across Lock 1 / ARCH / MASTER-PLAN / MIGRATION | Lock 1 verbatim amendment text; trio absorption staged in two waves |
| **Cohort** | **AMENDMENT-REQUIRED-NARROW (CONDITIONAL)** | — | — |

All four targets returned `AMENDMENT-REQUIRED-NARROW`. None returned `RE-DRAFT` or `AMENDMENT-REQUIRED-BROAD`. The dual-mode admission is mechanically absorbable into the V1 corpus.

## §3 Cohort lens disposition

| Lens | Cohort verdict | Notes |
|---|---|---|
| A — Coherence | AMENDMENT-REQUIRED-NARROW | Trio presents tape monomorphically; dual-mode admission requires explicit mode-branching at four cite sites |
| B — Vocabulary | AMENDMENT-REQUIRED-NARROW | Three new terms (`tape_mode`, `TapeAssembler`, `kind_at_cursor`) localized to substrate; no leakage into BIR/Grammar IR/public API |
| C — Coverage | AMENDMENT-REQUIRED-NARROW | Proposal covers skinny surface; V9.2 fills trio coverage gap |
| D — Lock adherence | AMENDMENT-REQUIRED-NARROW | 14 of 14 locks honored; Lock 1 amendment is the single REINVENT; Lock 5 (BIR-only lowerer) survives because metadata is BIR-adjacent not Grammar-IR |
| E — Axiom consistency | READY | BIR cardinality preserved (20 variants); V2 backend deferral preserved; SOTA anchor plane preserved |
| F — LLM bias | READY | Quantitative predictions in proposal cite empirical anchors; no pseudo-precision |
| G — Overfitting | READY | Amendment generic at substrate level; grammar-specific code in template output |
| H — Provenance | READY | All cited path:line resolves; trio-side cites verified |
| I — Contrivance | READY | Two-mode cardinality is the minimum; lazy-only is unsound; eager-only is empirically refuted |
| J — Host-language leverage | READY | LLVM compiles byte-discriminator to jump table; V2 backends unconstrained |
| K — Meta-grammar | READY (LOAD-BEARING for V1 H-gate close; ASPIRATIONAL for V1 correctness) | Meta-grammar correctness floor preserved; lazy mode is route to SOTA-beat, not precondition |
| N — Graduation mechanicality | MECHANICAL with single Lock-level amendment | V1 graduation: 8 V1-crate edits are additive; no fate changes; no rewrite |

## §4 Cross-target punch list (deduplicated)

The four per-target punch lists deduplicate to ~20 surgical edits across the V1 corpus. Most touch a single file; a few (Lock 1, ARCH §9.1) require synchronized edits across PASS-1 + PASS-3 + ARC trio.

| Item | Targets | Surgery class | Load-bearing? |
|---|---|---|---|
| Lock 1 verbatim amendment | `restart/locks/14-LOCKS.md:34` | Wholesale replacement per LAZY-TAPE-DESIGN.md §4.1 | **YES — single load-bearing edit** |
| ARCH §7.2 BIR `TapeEmit`/`DirectBuild` mode-branching | `restart/ARCHITECTURE.md:927-959` | Per-row payload extension | YES |
| ARCH §9.1 dual-mode tape invariants | `restart/ARCHITECTURE.md:1388-1409` | Module table + invariants table rewrite | YES |
| ARCH §3.1 parse API mode-monomorphic note | `restart/ARCHITECTURE.md:218-223` | Paragraph append | no |
| ARCH §5 `tape_mode` metadata schema | `restart/ARCHITECTURE.md:696-733` | Metadata key add + validation rule | YES |
| ARCH §12.2 per-grammar `tape_mode` column | `restart/ARCHITECTURE.md:1616-1627` | Matrix column add + LOC reprojection footnote | no |
| PASS-1 §2 Backend-IR ownership row (Tape/direct/value) | `restart/audit/pass-1-substrate/PASS-1.md:53-55` | Cell extension | no |
| PASS-1 §2 builder-frame replacement paragraph | `restart/audit/pass-1-substrate/PASS-1.md:59` | Paragraph extension | no |
| PASS-1:187 identity statement (mirror at ARC §9.1 + PASS-3 §1) | three-way synchronized edit | Identity invariant rewrite | YES |
| PASS-1 §4/§5 hand-off rows | `restart/audit/pass-1-substrate/PASS-1.md:197, :206` | Blocker cell extension | no |
| PASS-1:278-283 onboarding proof | `restart/audit/pass-1-substrate/PASS-1.md:278-283` | Clarifying clause | no |
| PASS-1:212 V9.1 residue carry | `restart/audit/pass-1-substrate/PASS-1.md:212` | V9.1 closure | no |
| `BBNF-TAPE-MODE-CONFLICT` diagnostic | PASS-1 §2 + ARC §7.4 | New diagnostic row | no |
| PASS-3 §1 `ReparsePlan` per-mode reuse-range | `restart/audit/pass-3-runtime/PASS-3.md:208-220` | Enum payload widening | no |
| MASTER-PLAN §4 SOTA-per-mode disposition | `restart/MASTER-PLAN.md:145-154` | Paragraph insertion | no |
| MASTER-PLAN §7 B.W4 dual-mode gate | `restart/MASTER-PLAN.md:299` | Row extension | no |
| MASTER-PLAN §11 F.W1 mode-branching emit | `restart/MASTER-PLAN.md:431` | Row extension | no |
| MASTER-PLAN §14 I.W1 ReparsePlan offset-range | `restart/MASTER-PLAN.md:537` | Row extension | no |
| MASTER-PLAN H tranche body cost-model `tape_mode` work-item | MASTER-PLAN H section | Work-item insertion | no |
| MIGRATION §17 B-row metadata-read | `restart/MIGRATION.md` §17 B-row | One-line clarification | no |
| `restart/skinny/WORKSPACE.md` `tape_mode` schema | WORKSPACE.md | Metadata-schema enumeration | no |
| PASS-2 BIR + codegen template mode-branching | (see PASS-2 V9.2 §12) | Per-row codegen template branches | YES |
| PASS-3 runtime ValueRef cursor semantics | (see PASS-3 V9.2 §12) | Identity + LSP/DAP per-mode carry | YES |

**Total deduplicated**: ~22 cross-quadrant surgical items. Of these, ~6 are load-bearing (Lock 1, ARCH §7.2 + §9.1, ARCH §5, PASS-1:187 identity, PASS-2 codegen template, PASS-3 ValueRef); the remaining ~16 are mechanical row / paragraph / matrix-cell amendments.

## §5 The conditional staging protocol

The V9.2 cohort's most important architectural commitment is the **two-wave staging**:

**Wave 1 (pre-implementation, NOW)**: V9.2 punch list is drafted and ratified but **not committed to the V1 corpus**. The audit reports land as historical record. The skinny implementation dispatch happens against LAZY-TAPE-DESIGN.md.

**Wave 2 (post-implementation, after measurement)**: skinny v2 lazy-mode lands; re-bench measures outcome class.
- **If outcome A/B/C** (T1 ≥ 14K Mbps on twitter, validation): Wave 2 commits the V9.2 punch list to the V1 corpus verbatim. Lock 1 amendment lands at `14-LOCKS.md:34`. ARCH § 7.2/§9.1/§3.1/§5/§12.2 land. MASTER-PLAN §4/§7/§11/§14 land. MIGRATION §17 lands. PASS-1/2/3 punch items land. Then dispatch **V9.3 verification cycle** to confirm post-amendment trio coherence.
- **If outcome G** (T1 < 13K Mbps, refutation): Wave 2 **discards** the V9.2 punch list. The trio reverts to eager-only canonical. SOTA-beat work routes to V1 H tranche body as ASPIRATIONAL without architectural amendment. The skinny implementation work persists as evidence; the V1 corpus is unchanged.

This protocol preserves the V1 corpus integrity against unmeasured speculative amendments while admitting the architectural move under measurement validation.

## §6 Implementation sequence

Per LAZY-TAPE-DESIGN.md §10 and §11, the implementation work:

1. **Skinny v2 lazy-mode implementation** (1-2 weeks; ~860 LOC net):
   - `runtime/src/tape/{offsets,assembler}.rs` (substrate ~+400 LOC)
   - `codegen/src/lower/rust.rs` mode-branching (~+100 LOC)
   - `runtime/src/grammars/json/view.rs` lazy-mode kind discriminator (~+200 LOC)
   - `runtime/src/grammars/json/generated.rs` shrinks (~-150 LOC)
   - Bench harness already supports outcome A/B/C/G classification per SK-V2

2. **Re-bench against three corpora** (1 day):
   - twitter, citm, canada
   - Predicted T1 14-16K Mbps on twitter (validation band)
   - Predicted T1 ≥ 17K Mbps on twitter (SOTA-beat target)

3. **If validation**: V9.2 punch list applies to V1 corpus; V9.3 verification cycle dispatches.

4. **If refutation**: V9.2 punch list discarded; eager-only V1 plan stands.

## §7 Co-dispatched SK-V2 amendment cohort summary

In parallel with V9.2 V1-corpus audit, 5 SK-V2 amendment agents applied the per-quadrant skinny text-propagation punch list:

| Quadrant | Items applied | File size delta |
|---|---|---:|
| SUBSTRATE | 20/20 — CLEAN | 594 → 652 (+58) |
| COMPILER | 15/15 — CLEAN | 723 → 763 (+40) |
| BENCH | 16/17 (1 cross-quadrant deferred) | 1,796 → 1,875 (+79) |
| WORKSPACE | 16 applied + 1 verified + 4 folded/deferred | 614 → 673 (+59) |
| INDEX | 5/5 — CLEAN | 85 → 89 (+4) |

Total: 72 of 73 SK-V2 items closed; +240 lines across skinny corpus. The SK-V2 amendment cohort delivers the skinny corpus to SK-V2-CLOSED at the text-propagation level.

## §8 Final readiness

> **Decision: V9.2 AMENDMENT-REQUIRED-NARROW (CONDITIONAL).**
>
> The lazy-tape Lock 1 amendment proposal is coherent with the V1 corpus across all four target surfaces. Locks 2-14 survive verbatim; Lock 1 is the single REINVENT. ~22 deduplicated surgical edits absorb the proposal into PASS-1, PASS-2, PASS-3, MASTER-PLAN trio, and Lock 1.
>
> The V9.2 punch list is staged for **post-implementation conditional commit**: it lands in the V1 corpus only if the skinny v2 lazy-mode implementation produces outcome A/B/C on re-bench. If outcome G repeats, the amendment text is discarded and the trio reverts to eager-only canonical.
>
> The proposal is steelmanned. The Lock 1 amendment text preserves the spirit (no parallel substrate; no OpenFrame clone) verbatim while admitting per-grammar `tape_mode` materialization. The dispatch is on `tape_mode` metadata, not on grammar name; Lock 14 holds. The dual-mode admission is the minimum surgical surface that admits the architectural move; lazy-only is unsound (CSS L4 / BBNF-self / Sheets need stored payload classes); eager-only is empirically refuted (outcome G three times running per RESULTS.md).
>
> Hereupon:
> 1. SK-V2 amendment cohort commits the skinny text-propagation amendments (72 items already landed in the working tree).
> 2. User authorizes skinny v2 lazy-mode implementation per LAZY-TAPE-DESIGN.md §10.3 sequence.
> 3. Re-bench measures outcome class.
> 4. **If A/B/C**: V9.2 punch list commits to V1 corpus; V9.3 verification cycle dispatches.
> 5. **If G**: V9.2 punch list archives; eager-only V1 plan stands; SOTA-beat routes to H tranche body.
>
> The architectural decision is now political (which V1 SOTA commitment ships) gated on a single measurement.

---

**V9.2 cohort totals**: 4 per-target audit reports (PASS-1 ~290 lines; PASS-2 ~616 lines; PASS-3 ~387 lines; MASTER-PLAN ~150 lines synthesis) + this consolidation = ~1,650 lines independent V1-corpus audit content. ~22 cross-quadrant surgical items. One architectural amendment proposal (Lock 1) staged for conditional commit pending skinny implementation outcome.
