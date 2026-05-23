# HARDENING ALPHA V1 — CONSOLIDATED (Pass Alpha SK-V13 → SK-V14)

Aggregator: SK-V14 CHALLENGE V1 over the eight Pass Alpha artefacts
(SYNTHESIS, HANDOFF, α-A, α-B, α-C, α-D, α-E, DISPATCH-CONTEXT). Seven
lenses dispatched (CH1 CORRECTNESS, CH2 GENERALITY, CH3 REGRESSION,
CH4 COST, CH5 HIDDEN COUPLING, CH6 ANTI-PAPER-CLOSE, CH7 OVERFIT-PRUNE)
per `restart/prompts/ORCHESTRATOR.md §3W` (CH1–CH6) plus the new
CH7 binding per `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`.

This consolidated authors the V1 verdict and the V2 fold dispositions
per `ORCHESTRATOR.md §3Z step 4`.

## §0 — V1 cycle verdict

### §0.1 — Per-lens dispositions

| Lens | ACCEPT | Total | Rate | REJECT | REVISE |
|---|---|---|---|---|---|
| CH1 CORRECTNESS | 50 | 53 | 94.34 % | 0 | 3 |
| CH2 GENERALITY | 29 | 33 | 87.88 % | 0 | 4 |
| CH3 REGRESSION | 28 | 30 | 93.33 % | 0 | 2 |
| CH4 COST | 24 | 34 | 70.59 % | 3 | 7 |
| CH5 HIDDEN COUPLING | 40 | 46 | 86.96 % | 1 | 5 |
| CH6 ANTI-PAPER-CLOSE | 37 | 42 | 88.10 % | 2 | 3 |
| CH7 OVERFIT-PRUNE | 30 | 36 | 83.33 % | 1 | 5 |
| **Aggregate** | **238** | **274** | **86.86 %** | **7** | **29** |

### §0.2 — Convergence test

Per `ORCHESTRATOR.md §3Z`, convergence requires ≥ 95 % ACCEPT for **two
consecutive cycles**. V1 aggregate is **86.86 %**, ~8.14 pp below the
single-cycle floor and not in convergence-eligible territory until V2
folds land. CH7's REJECT on C-3's round-trip gate is binding per
`PASS-0-OVERFIT-AUDIT.md §CH7` (final paragraph) — "CH7 REJECT triggers
plan-revise OR redress-revert" — and forecloses V1 convergence
regardless of the aggregate count.

**Verdict: PENDING-V2.** V1 does not converge. V2 fold required per §2
below; V3 confirming pass required thereafter per §3Z's two-cycle rule.

### §0.3 — REJECT list (verbatim, 7 total across all lenses)

#### From CH4 COST (3 REJECTs)

> **R1.** SYNTHESIS §3 risk column under-states C-1, C-3, C-4 vs α-E
> §3-6 (`SYNTHESIS.md:241,243,244` vs
> `alpha-E-candidate-shortlist.md:179,353,456`).
> (CH4 §0 / §2 REJECT R1.)

> **R2.** SYNTHESIS §0.5 and §3 elide per-candidate LOC envelopes that
> α-E §2 binds (`SYNTHESIS.md:136-145, 233-250` vs
> `alpha-E-candidate-shortlist.md:81-92`).
> (CH4 §0 / §2 REJECT R2.)

> **R3.** α-E §10 prescribes a 45-min redress cap for C-1 and C-3
> without routing those waves through the addendum's
> decision-engine-fold eligibility clause
> (`alpha-E-candidate-shortlist.md:617-626` vs
> `USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129-134`).
> (CH4 §0 / §2 REJECT R3.)

#### From CH5 HIDDEN COUPLING (1 REJECT)

> **REJECT — alpha-E §6 C-4 owner-paths admit tape-surface edits
> without Lock-1 triad.** `alpha-E-candidate-shortlist.md:392-411`
> enumerates owner paths for C-4 including
> `skinny/crates/runtime/src/tape/{mod,assembler,event_grammar}.rs —
> same-tape union variants (NOT a new sidecar; per pre-block P-7)`. The
> parenthetical disclaimer is the right intent, but Lock 1's
> 2026-05-21 substrate-ceiling fold (`LOCKS.md:73-82`) demands per-shape
> declaration of `substrate_target`, `retention_lifetime`,
> `policy_owner`. The line authorises codegen-pipeline + lowering-pass
> + runtime tape edits in ONE wave without the triad.
> (CH5 §2 REJECT.)

#### From CH6 ANTI-PAPER-CLOSE (2 REJECTs)

> **REJ-1.** `HANDOFF.md:110-114` declares "α-F synthesised directly
> from raw sources per `DISPATCH-CONTEXT.md §α-F` fall-through clause"
> — and the git history confirms this verbatim: only α-B (`e4870b201`)
> and α-E (`86dbd6b09`) carry α-tagged commits. α-A, α-C, α-D, α-F have
> no attribution commits, yet HANDOFF §2 and SYNTHESIS §1 cite them as
> authority. CH6 binds: self-reported "synthesised from raw" without a
> per-agent committed artefact at dispatch time is paper-close on the
> Pass Alpha §2 scope-matrix six-agent fan-out contract.
> (CH6 §0 / §2 REJ-1.)

> **REJ-2.** SYNTHESIS §1.2 row "11 JSON direct + typed admits (4 + 7)"
> / HANDOFF §3 same numbers — α-A §6 audit verdict table tallies
> 6 direct + 11 typed = 17 (not 4 + 7 = 11). α-D §3 I-3/I-4 explicitly
> surfaces "4–5" / "7–10" with a discrepancy paragraph
> (`alpha-D-validated-invalidated.md:281-291, :353-368`). SYNTHESIS
> silently picks the lower count from the dispatch-context bind without
> flagging that α-A and α-D measured the higher count. This is
> paper-closure of a numeric divergence by citation laundering.
> (CH6 §0 / §2 REJ-2.)

#### From CH7 OVERFIT-PRUNE (1 REJECT, BINDING per PASS-0-OVERFIT-AUDIT §CH7)

> **C-3 round-trip gate is CH7-1-blind to Pattern H.** C-3's
> falsifiability gate per `alpha-E.md:332-336` reads:
> `rm -rf skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask
> regen-css && git diff -- skinny/crates/runtime/src/grammars/css_l4_*
> produces empty output`. The gate is sound for the skinny-side
> runtime tree. It is silent on the Pattern H runtime tree under
> `crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv,
> ebnf, bnf, math}/` flagged at `alpha-D.md:486-495` + enumerated at
> `alpha-E.md:133-134` as 64 hand-written per-grammar files. The gate
> is also silent on the audit-confirmed CSS bypass-header pattern. A
> gate that does not falsify the recurrence vector it nominally
> addresses is not a gate — it is paper.
> (CH7 §0 / §2.1 REJECT — **BINDING**.)

### §0.4 — REVISE list (verbatim, 29 total across all lenses)

#### CH1 CORRECTNESS (3 REVISEs)

> **REVISE-1 — α-A §2 direct-admit row-count drift.** Add a 2-line
> reconciliation table in α-A §2 mapping ROLLING-SOTA-DELTA's 6
> ADMITTED rows to the dispatch §1 "4" — name the +2 (marine_ik,
> instruments) explicitly with their v6 §1 row 3 binding so PRUNE-1's
> revert scope is unambiguous.
> (CH1 §2 REVISE-1.)

> **REVISE-2 — α-A §3 typed-admit row-count drift.** Annotate the +4
> extension rows in α-A §3's table (random, instruments, numbers,
> unicode_basic via W13.1/.2/.3/.4 + W15.1 update_center adjusted) so a
> future S-P3 wave dispatching PRUNE-1's revert scope reads one number.
> (CH1 §2 REVISE-2.)

> **REVISE-3 — α-E §6 C-4 falsifiability gate under-specified.**
> Rebind the pre-wave hot-leaf citation to one of: (a) the
> `RESULTS.md` Hot-leaf column for the named row at HEAD, or (b) v2
> §3.1's numeric-array dispatch trace + W11.1 commit SHA. Either
> anchors the pre-wave baseline so the post-wave hot-leaf assertion is
> binary.
> (CH1 §2 REVISE-3.)

#### CH2 GENERALITY (4 REVISEs)

> **Finding 1 — C-3 `regen-css` xtask must be presented as the CSS
> instance of a `regen-{grammar}` family.** α-F SYNTHESIS §3 C-3 row
> appends "(first instance of the `regen-{grammar}` family; the xtask
> binary parametrises a grammar-neutral generator)". α-E §5 ¶1
> (Purpose) appends the same.
> (CH2 §2 Finding 1.)

> **Finding 2 — C-1 owner-path text should bind the FUTURE invariant
> alongside the CURRENT recurrence-vector enumeration.** α-E §3 C-1
> §"Falsifiability gate" appends: "Forward invariant (post-redress,
> permanent): any new grammar added under
> `workspace.metadata.bbnf.grammars.{name}` produces ZERO new `.rs`
> files in `skinny/crates/{codegen, runtime, passes, bbnf,
> grammar}/src/` and ZERO new directories in `crates/core/src/runtime/`.
> The Lock 14 baseline gate rejects any commit that violates this."
> (CH2 §2 Finding 2.)

> **Finding 3 — C-4 must forbid grammar-branched dispatch inside the
> CSP shape consumer.** α-E §6 C-4 §"Pre-blocked routes" appends: "The
> shape consumer in `skinny/crates/codegen/src/lib.rs` MUST dispatch on
> the CSP-emitted `BackendShape` enum alone; no `match grammar { Json
> => ..., CssL4 => ... }` arm may appear in the dispatch path."
> (CH2 §2 Finding 3.)

> **Finding 4 — α-E §10 telemetry-tagging clarification.** α-E §10
> telemetry sentence appends: "The hot-leaf column reads as a
> grammar-keyed symbol path (`{grammar}::parse_*` or equivalent); a
> stale inherited symbol name on a non-JSON row fails the per-row gate
> the same way it fails S-P1."
> (CH2 §2 Finding 4.)

#### CH3 REGRESSION (2 REVISEs)

> **REVISE-1 — SYNTHESIS §0.4 P-1 paragraph does not lift α-C's
> per-entry round-trip rule trigger flag on W10.3 nested_layout.**
> SYNTHESIS §0.4 P-1 closing sentence adds: "Per α-C §4, W10.3
> nested_layout (124× anomaly) carries a preemptive round-trip-rule
> trigger: any second-in-tranche reopen of nested_layout requires user
> re-pin with intrinsic-block evidence. Any future CSS feature whose
> claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50×
> inherits the same trigger." HANDOFF §7 adds a matching
> refusal-condition bullet for the same trigger.
> (CH3 §2 REVISE-1.)

> **REVISE-2 — α-E §7 C-5 REDRESS scribe contract count ambiguity.**
> α-E §7 owner-paths clarifies the scribe contract as "29 row-keyed
> REDRESS entries" — one entry per reverted row, naming the row key +
> the validation §reference. C-5's falsifiability gate then reads
> "`skinny/REDRESS.md` carries 29 new row-keyed entries" rather than
> the current ambiguous wording.
> (CH3 §2 REVISE-2.)

#### CH4 COST (7 REVISEs)

> **V1.** α-E C-1 LOC envelope (2.1k–3.4k source) under-states 64-file
> refactor + 8 grammar sub-waves
> (`alpha-E-candidate-shortlist.md:168`).
> (CH4 §0 V1.)

> **V2.** α-E C-2 LOC envelope omits in-tree Skipper-class fallback
> path flagged by α-B §316-320 (~80 additional LOC).
> (CH4 §0 V2.)

> **V3.** α-E §3 C-1 declares dependency on C-3 + C-5 but §9
> concurrency matrix permits C-1 JSON sub-waves to parallelise C-4
> (post-C-1 surface), creating a sequencing under-specification
> (`alpha-E-candidate-shortlist.md:599-609`).
> (CH4 §0 V3.)

> **V4.** SYNTHESIS §3 candidate table omits the same-wave consumer
> column entirely (`SYNTHESIS.md:233-250`).
> (CH4 §0 V4.)

> **V5.** HANDOFF §6 next-move chain elides hard caps for the
> CHALLENGE pass itself (`HANDOFF.md:144-161`).
> (CH4 §0 V5.)

> **V6.** α-A §5 c/B telemetry surface remains schema-debt with no
> LOC budget assigned for the c/B column add
> (`alpha-A-results-extraction.md:244-261`).
> (CH4 §0 V6.)

> **V7.** SYNTHESIS §4 S-P3 constraints invoke the same-wave consumer
> rule but omit any LOC budget ceiling per wave
> (`SYNTHESIS.md:252-287`).
> (CH4 §0 V7.)

#### CH5 HIDDEN COUPLING (5 REVISEs)

> **REVISE — SYNTHESIS §4 G-SIMD-GRAMMAR-POLICY clause omits Lock-1
> triad.** Append to §4 clause — "every SIMD consumer wired by C-4
> declares `substrate_target`, `retention_lifetime`, and `policy_owner`
> per `LOCKS.md:73-82`; xtask gate-json rejects any row whose REDRESS
> lacks the triple."
> (CH5 §2 REVISE #1.)

> **REVISE — SYNTHESIS §2 telemetry needs explicit
> Track-2-entry-point column.** Append a `track2_entry_point` column
> to the SK-V14 RESULTS schema; `xtask gate-json` rejects any row
> where the Track 1 and Track 2 entry-point symbol paths share a
> common ancestor in `runtime::tape::` beyond the public `Tape` /
> `OffsetFlags` types.
> (CH5 §2 REVISE #2.)

> **REVISE — HANDOFF §7 refusal list lacks verbatim `UnionTape` /
> second-tape clause.** Append to HANDOFF §7 verbatim Lock 1 clause —
> "REVISE any plan that proposes `UnionTape`, a second tape, a public
> substrate API, a retained class/mask stream, or parser-owned
> cursor/list state; only G-Omega may amend Lock 1's substrate-union
> closure."
> (CH5 §2 REVISE #3.)

> **REVISE — alpha-C §2 P-7 falsifiability gate weak on buffer
> ownership.** Strengthen the falsifiability gate to a triple check —
> (a) distinct symbol paths (current), (b) distinct concrete `Sink`
> types (compile-time `TypeId` inequality), (c) distinct buffer
> addresses at the first bench iter (runtime address inequality
> recorded into the per-iter equality column).
> (CH5 §2 REVISE #4.)

> **REVISE — alpha-E §6 C-4 same-wave consumer plan permits
> attribution-only proof.** Append to the C-4 same-wave consumer plan
> — "the post-wave hot leaf's module path traces to `runtime::tape::`
> (an existing same-tape variant) or to a generator-emitted module
> whose template provenance is named in REDRESS; module paths under
> `runtime::ext::`, `runtime::sidecar::`, `runtime::union::`, or
> `runtime::cursor::` are REJECT pre-emptively."
> (CH5 §2 REVISE #5.)

#### CH6 ANTI-PAPER-CLOSE (3 REVISEs)

> **REV-1.** C-4 (W8+W9 scaffold→load-bearing) falsifiability gate
> names "hot leaf attribution differs from pre-wave value — proof of
> runtime divergence" (`alpha-E-candidate-shortlist.md:439-444`) but
> the gate is enumerated on ONE suggested row
> (`json/numbers/direct_to_struct/main`); the gate is measured, but it
> does not bind to a NAMED pre-wave hot-leaf symbol the lens can
> verify in advance. Tighten to "pre-wave hot leaf `parse_value_at`;
> post-wave hot leaf names the W11.1 number-specialised symbol
> explicitly in the samply trace".
> (CH6 §0 REV-1.)

> **REV-2.** HANDOFF.md:145 next-move chain
> `ready-for-CHALLENGE-V1 → G-Alpha → S-P0` skips G-Omega entirely
> from the one-line summary, though §5 step 6 includes G-Omega between
> S-P1 and the wave program. PASS-ALPHA.md:14 binds Pass Omega has run
> as entry condition; the next-move line elides this binding gate.
> Restate as `ready-for-CHALLENGE-V1 → G-Alpha → S-P0 →
> S-P1/S-P2/S-P3 ∥ Pass Omega → G-Omega → Wave 0 (PRUNE-1)`.
> (CH6 §0 REV-2.)

> **REV-3.** SYNTHESIS §4 S-P3 constraints do not encode the
> triumvirate role-separation (research / plan / redress in distinct
> commits) per memory `[triumvirate-discipline]` +
> `ORCHESTRATOR.md:209` non-negotiable row. Append a thirteenth
> constraint to SYNTHESIS §4: "every wave fans out as research → plan
> → redress in distinct commits per `[triumvirate-discipline]` +
> ORCHESTRATOR §8; a wave that lands a single
> research-plan-redress mega-commit fails the gate at S-P3."
> (CH6 §0 REV-3.)

#### CH7 OVERFIT-PRUNE (5 REVISEs)

> **CH7 §1 SYNTHESIS §3 row REVISE.** Table-row gates for C-3 + C-4
> compress the CH7 surface: C-3's gate states "round-trip xtask check
> returns clean" without naming the byte-equivalent diff target;
> C-4's gate states "measurable runtime divergence on a named
> pre-wave row" without naming the row in this table. Lift α-E §5
> "delete generated → cargo xtask regen-css → diff empty" verbatim
> into `SYNTHESIS.md:243`; lift α-E §6 named row
> "`json/numbers/direct_to_struct/main`" into `SYNTHESIS.md:244`.
> (CH7 §1.)

> **CH7 §1 α-E §2 shortlist table REVISE.** C-3 falsifiability gate
> (`alpha-E.md:85`) cites round-trip but the C-3 §5 expansion
> (`alpha-E.md:332-336`) is where the exact `rm -rf … && cargo xtask
> regen-css && git diff` command lives. Lift the explicit command
> into the table-row or add a "see §N" pointer. Same for C-4's named
> pre-wave row.
> (CH7 §1.)

> **CH7 §1 α-E §10 cost + caps REVISE.** C-1 sub-waves are 45-min
> redress (correctly amended per CH7 view, contradicted by CH4 R3),
> but the C-1 redress cap applies per sub-wave × 8 sub-waves. The
> table reads ambiguously — clarify whether the 45-min cap is per
> sub-wave or per cluster.
> (CH7 §1.)

> **CH7 §3.2 V2-DISP-α-E-C3-table REVISE.** Lift the explicit
> round-trip command + bypass-header detector from §3.1 into the
> SYNTHESIS §3 table-row for C-3 (or add a "see α-E §5 + hardening V1
> CH7 §3.1" pointer in the gate cell). Same patch for C-4's named
> pre-wave row (`json/numbers/direct_to_struct/main`) in SYNTHESIS §3
> row C-4 (`SYNTHESIS.md:244`).
> (CH7 §3.2.)

> **CH7 §3.4 V2-DISP-SYNTHESIS-§3-C3-C4 REVISE.** Apply §3.2 to
> SYNTHESIS §3 directly: C-3 row gate clarification + C-4 row
> named-row insertion. SYNTHESIS is the artefact S-P3 consumes
> verbatim; the compressed gate phrasing risks losing the CH7-4
> binding in downstream consumption.
> (CH7 §3.4.)

### §0.5 — Reconciliation note: contradictory dispositions between lenses

CH4 REJECT R3 declares the 45-min redress cap on C-1 / C-3 illegitimate
under the addendum (`USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129-134`
limits the 30→45-min uplift to W5–W9 decision-engine + W12 union-SIMD
waves). CH7 §1 (α-E §10 row) and CH7 §3.3 (V2-DISP-α-E-cost-table)
treat the per-sub-wave 45-min cap as "correctly amended" and only ask
for clarification of per-sub-wave vs per-cluster scope. **The CH4
reading is authoritative** under cap discipline — the addendum is the
campaign-wide bar and CH4 reads it verbatim. The V2 fold per §2 takes
CH4 R3 as binding: C-1, C-3 default to 30 min; only C-4 (W8 + W9)
inherits the 45-min amendment. The CH7 §3.3 clarification then applies
to C-4's per-sub-wave cap.

## §1 — Per-artefact convergence digest

Eight Pass Alpha artefacts; per-artefact cross-lens disposition counts
and V2-redispatch status.

### §1.1 — SYNTHESIS.md (350 lines, α-F author)

| Lens | Disposition pressure |
|---|---|
| CH1 | All 11 sections ACCEPT. |
| CH2 | §3 C-3 row REVISE (Finding 1); rest ACCEPT. |
| CH3 | §0.4 P-1 REVISE (REVISE-1); rest ACCEPT. |
| CH4 | §3 REJECT (R1+R2; V4), §4 REVISE (V7); rest ACCEPT. |
| CH5 | §2 REVISE (telemetry track2_entry_point); §4 REVISE (G-SIMD-GRAMMAR-POLICY triad). |
| CH6 | §0.2 + §1.2 REJECT (REJ-2); §4 REVISE (REV-3 triumvirate). |
| CH7 | §3 row C-3 / C-4 REVISE; rest ACCEPT (P-1..P-7 mapping verified bijective). |

**Verdict: V2 REDISPATCH (α-F).** Concentrated pressure on §3 candidate
table (CH4 R1+R2, V4; CH7 row REVISE; CH2 Finding 1), §1.2 + §0.2 (CH6
REJ-2), §4 constraint set (CH4 V7; CH5 G-SIMD triad; CH6 REV-3); and
§0.4 (CH3 REVISE-1), §2 telemetry (CH5).

### §1.2 — HANDOFF.md (213 lines, α-F author)

| Lens | Disposition pressure |
|---|---|
| CH1 | All 8 sections ACCEPT. |
| CH2 | All ACCEPT. |
| CH3 | All ACCEPT (§7 carries P-1..P-7 verbatim). |
| CH4 | §6 REVISE (V5 next-move caps). |
| CH5 | §7 REVISE (UnionTape clause missing). |
| CH6 | §4 REJECT (REJ-1 α-phase staging race); §3 REJECT (REJ-2 inherited); §6 REVISE (REV-2 G-Omega elision). |
| CH7 | All ACCEPT. |

**Verdict: V2 REDISPATCH (α-F).** §4 + §3 carry the CH6 REJECTs
(REJ-1 + REJ-2); §6 + §7 carry CH4 V5 + CH5 + CH6 REV-2 REVISEs.

### §1.3 — α-A results-extraction (362 lines)

| Lens | Disposition pressure |
|---|---|
| CH1 | §2 REVISE (REVISE-1 direct row-count drift); §3 REVISE (REVISE-2 typed row-count drift). |
| CH2 | All ACCEPT. |
| CH3 | §306-329 ACCEPT-WITH-NOTE; rest ACCEPT. |
| CH4 | §5 REVISE (V6 c/B schema debt LOC budget). |
| CH5 | All ACCEPT. |
| CH6 | All ACCEPT (the reconciliation paragraphs at §117-122 + §161-169 are credited as honest disclosure). |
| CH7 | All ACCEPT; §3.5 informational fold for citation spot-check expansion. |

**Verdict: V2 REDISPATCH (α-A).** Three folds: CH1 REVISE-1 + REVISE-2
(reconciliation table at §2 + extension-row annotation at §3); CH4 V6
(c/B LOC budget). Low surface area — α-A may execute via narrow line
edits.

### §1.4 — α-B competitor-deltas (328 lines)

| Lens | Disposition pressure |
|---|---|
| CH1 | All ACCEPT. |
| CH2 | All ACCEPT. |
| CH3 | All ACCEPT. |
| CH4 | All ACCEPT (the in-tree Skipper estimate is correctly named even when α-E under-counts). |
| CH5 | All ACCEPT. |
| CH6 | All ACCEPT. |
| CH7 | All ACCEPT. |

**Verdict: STANDS.** No V2 redispatch required. α-B is the only
artefact with zero cross-lens findings. (Note: α-B is also the only
artefact with a per-agent commit attribution `e4870b201` — this
correlates and informs the CH6 REJ-1 prescription.)

### §1.5 — α-C REDRESS-digest (428 lines)

| Lens | Disposition pressure |
|---|---|
| CH1 | All ACCEPT. |
| CH2 | All ACCEPT. |
| CH3 | All ACCEPT (per-entry dispositions and pre-blocks both verified). |
| CH4 | All ACCEPT. |
| CH5 | §2 P-7 REVISE (falsifiability gate weak on buffer ownership). |
| CH6 | All ACCEPT. |
| CH7 | All ACCEPT. |

**Verdict: V2 REDISPATCH (α-C, narrow).** Single fold: CH5 P-7 gate
triple-check strengthening (distinct symbol + distinct `Sink` type +
distinct buffer address).

### §1.6 — α-D validated-invalidated (545 lines)

| Lens | Disposition pressure |
|---|---|
| CH1 | All ACCEPT. |
| CH2 | All ACCEPT. |
| CH3 | All ACCEPT. |
| CH4 | All ACCEPT. |
| CH5 | All ACCEPT. |
| CH6 | All ACCEPT (the reconciliation paragraphs at §281-291 + §353-368 are credited as honest disclosure). |
| CH7 | All ACCEPT. |

**Verdict: STANDS.** No V2 redispatch required.

### §1.7 — α-E candidate-shortlist (660 lines)

| Lens | Disposition pressure |
|---|---|
| CH1 | §6 REVISE (REVISE-3 C-4 pre-wave hot-leaf citation). |
| CH2 | §3 C-1 REVISE (Finding 2 forward invariant); §5 C-3 REVISE (Finding 1 family shape); §6 C-4 REVISE (Finding 3 no grammar-branch); §10 REVISE (Finding 4 grammar-keyed hot-leaf). |
| CH3 | §7 REVISE (REVISE-2 scribe-contract count ambiguity). |
| CH4 | §10 REJECT (R3 cap miscategorisation); §2 REVISE (V1 C-1 LOC); §4 REVISE (V2 C-2 LOC + Skipper); §9 REVISE (V3 C-1 ↔ C-4 sequencing). |
| CH5 | §6 C-4 REJECT (owner-paths admit tape-surface edits without Lock-1 triad); §6 C-4 REVISE (same-wave consumer attribution-only proof). |
| CH6 | §6 REVISE (REV-1 C-4 pre/post symbol naming loose). |
| CH7 | §5 C-3 REJECT (round-trip gate CH7-1-blind to Pattern H) — BINDING; §2 + §10 REVISE. |

**Verdict: V2 REDISPATCH (α-E, CONCENTRATED).** Most cross-lens
pressure in the slate. C-3 carries the binding CH7 REJECT; C-4 carries
the CH5 REJECT and CH4 R3; the four CH2 findings span C-1, C-3, C-4,
§10; CH4 V1+V2+V3 are LOC/sequencing realism; CH3 REVISE-2 is the
scribe-count clarification.

### §1.8 — DISPATCH-CONTEXT.md (206 lines)

| Lens | Disposition pressure |
|---|---|
| CH1 | ACCEPT. |
| CH2 | ACCEPT. |
| CH3 | ACCEPT. |
| CH4 | ACCEPT. |
| CH5 | ACCEPT. |
| CH6 | ACCEPT (the §α-F fall-through clause is the source of REJ-1 but the dispatch context itself merely permits it — the discipline failure is downstream). |
| CH7 | ACCEPT. |

**Verdict: STANDS.** DISPATCH-CONTEXT is the spec the α-agents
executed against; it is not the target of any lens finding.

### §1.9 — Cross-artefact roll-up

- **STAND (no V2 redispatch):** α-B, α-D, DISPATCH-CONTEXT (3 of 8).
- **V2 REDISPATCH (narrow):** α-C (CH5 P-7 gate), α-A (3 folds).
- **V2 REDISPATCH (concentrated):** α-E (10+ folds, includes binding CH7 REJECT), α-F SYNTHESIS (8+ folds), α-F HANDOFF (5+ folds).

Total V2 redispatch surface: 4 of 8 artefacts; the α-F double (SYNTHESIS
+ HANDOFF) and α-E carry the bulk of the fold surface.

## §2 — V2 fold dispositions (the binding output)

Per `[agent-orchestration]`, the three V2 dispatches (α-F, α-E, α-A,
α-C) may parallel-redispatch into separate worktrees, but the
α-F/α-E folds must be **sequence-aware** — α-F SYNTHESIS §3 inherits
α-E §10 cap discipline, α-E §10 inherits CH4 R3, and the CH7
REJECT remediation in α-E §5 cross-binds α-F SYNTHESIS §3 row C-3
(per CH7 §3.4). The aggregator commits all V2 outputs in a single
atomic V2 commit per the SK-V14 protocol.

### §2.1 — α-F V2 fold packet (SYNTHESIS + HANDOFF; HIGH priority)

α-F owns the V2 redispatch on SYNTHESIS + HANDOFF. Required folds:

#### F-1 (binding) — REJ-2 numeric divergence reconciliation

Add a discrepancy paragraph to SYNTHESIS §0.2 and §1.2 + HANDOFF §3
citing α-A:117-122 + :161-169 + α-D:281-291 + :353-368. The PRUNE-1
ledger revert must cover the wider 6+11 population, not just the
narrower 4+7 the synthesis cites. Verbatim per CH6 §2.2:

> "dispatch §1 cites 4 + 7; α-A / α-D measure 6 + 11 under the broader
> ROLLING-SOTA-DELTA ledger; both populations reclassify
> AUDIT-FALSIFIED under v6 §1 rows 3-4; reconciliation captured in
> α-A:117-122 + :161-169."

#### F-2 (binding) — REJ-1 α-phase staging race remediation

Per CH6 §3 fold (a): commit α-A / α-C / α-D / α-F with their own
`docs(sk-v14-alpha): <tag>` subjects and 5–15 line bodies per
DISPATCH-CONTEXT.md §3 discipline. **Aggregator note:** Because
α-A/α-C/α-D/α-F files are already in tree at the same SHA as the
α-F commit and have not been re-authored, the V2 dispatch should
either (a) instruct α-F to retroactively partition the synthesis
authorship trail across α-A/α-C/α-D (re-commit each at the prior
SHA), or (b) edit HANDOFF §4 to declare α-F as the sole author of
all four artefacts and remove the "α-A through α-E remain
outstanding" framing. Pick (b) — the cleaner posture, given the V2
α-A and α-C and α-E redispatches in §2.2–§2.4 will generate fresh
per-agent commits naturally.

#### F-3 — REJECT R1 SYNTHESIS §3 risk column rebind

Per CH4 §2 REJECT R1: revise §3 candidate-table risk column to carry
the α-E narrative classification: **C-1 VERY HIGH, C-2 HIGH, C-3 HIGH,
C-4 VERY HIGH, C-5 MED-LOW.** This contradicts the dispatch context's
§CH-4 endorsed classification (HIGH / MED / MED / MED) — the dispatch
endorsement was wrong and is itself REVISE per CH1 reconciliation.

#### F-4 — REJECT R2 SYNTHESIS §3 LOC budget column add

Per CH4 §2 REJECT R2: SYNTHESIS §3 candidate table gains a `LOC
budget` column. Values copied verbatim from
`alpha-E-candidate-shortlist.md:83-87`: C-1 2.1k-3.4k; C-2 600-1.0k;
C-3 1.2k-2.0k; C-4 800-1.4k; C-5 250-500. SYNTHESIS §0 optionally
carries a summary cost envelope ≈ 4.95k-8.3k as the SK-V14 PRUNE-phase
ceiling.

#### F-5 — V4 SYNTHESIS §3 same-wave consumer column add

Per CH4 V4: SYNTHESIS §3 candidate table gains a same-wave consumer
column with values lifted from α-E §3-7 (one-line per candidate).

#### F-6 — V7 SYNTHESIS §4 per-wave LOC ceiling

Per CH4 V7: SYNTHESIS §4 S-P3 constraints append a per-wave LOC
ceiling clause: "S-P3 wave manifest inherits per-candidate LOC
envelopes from α-E §2; any wave exceeding its envelope by > 20 %
escalates per `[generated-size-budget]`."

#### F-7 — V5 HANDOFF §6 hard-cap echo

Per CH4 V5: HANDOFF §6 next-move chain echoes the CHALLENGE-pass
hard caps (30-min lens-agent cap; 20/15/30-or-45 research/plan/redress
caps per `[dispatch-hard-cap]`).

#### F-8 — REV-2 HANDOFF §6 G-Omega restoration

Per CH6 REV-2: restate HANDOFF §6 next-move line as:
`ready-for-CHALLENGE-V1 → G-Alpha → S-P0 → S-P1/S-P2/S-P3 ∥ Pass Omega
→ G-Omega → Wave 0 (PRUNE-1)`.

#### F-9 — REV-3 SYNTHESIS §4 triumvirate discipline

Per CH6 REV-3: append a thirteenth constraint to SYNTHESIS §4: "every
wave fans out as research → plan → redress in distinct commits per
`[triumvirate-discipline]` + ORCHESTRATOR §8; a wave that lands a
single research-plan-redress mega-commit fails the gate at S-P3."

#### F-10 — CH3 REVISE-1 SYNTHESIS §0.4 P-1 round-trip trigger

Per CH3 REVISE-1: SYNTHESIS §0.4 P-1 closing sentence adds the W10.3
nested_layout preemptive round-trip-rule trigger; HANDOFF §7 adds a
matching refusal-condition bullet (text quoted in §0.4 above).

#### F-11 — CH2 Finding 1 mirror SYNTHESIS §3 + §0.3 R4

Per CH2 Finding 1: α-F SYNTHESIS §3 C-3 row + §0.3 R4 text append
"(first instance of the `regen-{grammar}` family; the xtask binary
parametrises a grammar-neutral generator)".

#### F-12 — CH2 Finding 2 mirror SYNTHESIS §4 forward invariant

Per CH2 Finding 2: SYNTHESIS §4 S-P3 constraint list appends the
C-1 forward-invariant clause: "C-1's forward invariant is permanent;
S-P3 wave plans MUST cite it as the pre-condition for any new
grammar admission wave (BBNF-self, Sheets, future grammars)."

#### F-13 — CH2 Finding 3 mirror SYNTHESIS §4 dispatch discipline

Per CH2 Finding 3: SYNTHESIS §4 S-P3 constraint list appends the
C-4 dispatch-discipline clause: "The C-4 shape consumer is exercised
across at least two grammar families before any C-4 admit cites
runtime divergence as load-bearing; one-grammar runtime divergence is
wave evidence, not admit evidence."

#### F-14 — CH5 SYNTHESIS §4 G-SIMD-GRAMMAR-POLICY triad

Per CH5 REVISE: append to SYNTHESIS §4 G-SIMD-GRAMMAR-POLICY clause:
"every SIMD consumer wired by C-4 declares `substrate_target`,
`retention_lifetime`, and `policy_owner` per `LOCKS.md:73-82`; xtask
gate-json rejects any row whose REDRESS lacks the triple."

#### F-15 — CH5 SYNTHESIS §2 track2_entry_point column

Per CH5 REVISE: append a `track2_entry_point` column to the SK-V14
RESULTS schema; `xtask gate-json` rejects any row where Track 1 and
Track 2 entry-point symbol paths share a common ancestor in
`runtime::tape::` beyond the public `Tape` / `OffsetFlags` types.

#### F-16 — CH5 HANDOFF §7 UnionTape verbatim refusal

Per CH5 REVISE: append to HANDOFF §7 the verbatim Lock 1 clause:
"REVISE any plan that proposes `UnionTape`, a second tape, a public
substrate API, a retained class/mask stream, or parser-owned
cursor/list state; only G-Omega may amend Lock 1's substrate-union
closure."

#### F-17 — CH7 SYNTHESIS §3 C-3 + C-4 row clarifications

Per CH7 §3.2 + §3.4: SYNTHESIS §3 row C-3 reads "round-trip xtask
check returns clean on both runtime trees + bypass-header detector
empty" (per CH7 §3.1 binding REJECT remediation in §2.2 below).
SYNTHESIS §3 row C-4 inserts the named pre-wave row
`json/numbers/direct_to_struct/main`.

**α-F V2 hard cap: 45 min** (17 folds across two artefacts; the bulk
is line-level edits to SYNTHESIS §3 + §4 + HANDOFF §6 + §7).

### §2.2 — α-E V2 fold packet (candidate-shortlist; HIGH priority)

α-E owns the V2 redispatch on the candidate-shortlist. Required folds:

#### E-1 (BINDING, CH7 §3.1) — C-3 round-trip gate scope extension

Per CH7 §3.1 — **the binding REJECT remediation**. Replace C-3's
falsifiability gate at `alpha-E.md:332-336` with the three-part
expansion:

> **Round-trip (skinny tree).** `rm -rf
> skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask regen-css
> && git diff -- skinny/crates/runtime/src/grammars/css_l4_*` produces
> empty output.
>
> **Round-trip (core tree, all 8 grammars).** For each of `{json,
> css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}`: `rm -rf
> crates/core/src/runtime/<grammar>/ && cargo xtask regen-<grammar> &&
> git diff -- crates/core/src/runtime/<grammar>/` produces empty
> output. (C-1's sub-wave structure owns the per-grammar xtask
> emission; C-3's round-trip gate consumes those xtasks for CSS.)
>
> **Bypass-header detector.** Every file matching `git grep -l
> '@generated by skinny bbnf-codegen' -- skinny/crates/runtime
> crates/core/src/runtime` must be the byte-for-byte output of a
> registered xtask emission; the round-trip succeeds on every such
> file. Files asserting the header outside the registered xtask scope
> are CH7-1 violations and reject the wave.

#### E-2 (binding) — REJECT R3 cap miscategorisation

Per CH4 R3 (and CH5/CH7 reconciliation per §0.5): revise α-E §10 cap
table to default C-1 30 min, C-3 30 min, C-2 30 min, C-5 30 min;
only C-4 inherits the 45-min addendum amendment. CH7 §3.3's
clarification (per-sub-wave vs per-cluster) now applies to C-4 alone:
the 45-min cap is per sub-wave; the C-4 cluster total is bounded by
the number of CSP-selectable shapes the wave addresses.

#### E-3 (binding) — CH5 REJECT C-4 owner-paths Lock-1 triad

Per CH5 §2 REJECT: α-E §6 C-4 owner-paths section appends per-shape
triad-declaration discipline. The same-wave consumer plan adds a
`substrate_target=existing_tape | retention_lifetime=generated_function
| policy_owner=generated_grammar` triple as a required column in the
wave's REDRESS entry for every CSP-selectable shape. Any shape whose
triple cannot be declared at wave-plan time abrogates per
`[abrogate-before-patch]` and falls under C-4's architectural-block
escalation path (`alpha-E §11:649-651`).

#### E-4 — CH5 REVISE C-4 same-wave consumer module-path discipline

Per CH5 §2 REVISE #5: append to the C-4 same-wave consumer plan: "the
post-wave hot leaf's module path traces to `runtime::tape::` (an
existing same-tape variant) or to a generator-emitted module whose
template provenance is named in REDRESS; module paths under
`runtime::ext::`, `runtime::sidecar::`, `runtime::union::`, or
`runtime::cursor::` are REJECT pre-emptively."

#### E-5 — CH1 REVISE-3 + CH6 REV-1 C-4 pre-wave hot-leaf citation

Per CH1 REVISE-3 + CH6 REV-1: rebind the C-4 pre-wave hot-leaf
citation to one of (a) `RESULTS.md` Hot-leaf column for
`json/numbers/direct_to_struct/main` at HEAD, or (b) v2 §3.1
numeric-array dispatch trace + W11.1 commit SHA. Name the specific
post-wave symbol the samply trace must show (CH6 REV-1 — e.g.
`parse_value_at` → `parse_number_array_specialised` or the actual
W11.1 emitted symbol).

#### E-6 — CH2 Finding 1 C-3 family-shape binding

Per CH2 Finding 1: α-E §5 ¶1 (Purpose) appends "(first instance of
the `regen-{grammar}` family; the xtask binary parametrises a
grammar-neutral generator; the generic codegen entry it invokes is
the same surface a future `regen-sheets` / `regen-bbnf-self` /
`regen-{new}` binary will invoke)."

#### E-7 — CH2 Finding 2 C-1 forward invariant

Per CH2 Finding 2: α-E §3 C-1 §"Falsifiability gate" appends the
forward-invariant clause (text quoted in §0.4 above): no new `.rs`
files in generic crates, no new dirs in `crates/core/src/runtime/`,
Lock 14 baseline gate rejects.

#### E-8 — CH2 Finding 3 C-4 no grammar-branch

Per CH2 Finding 3: α-E §6 C-4 §"Pre-blocked routes" appends the
no-`match grammar` dispatch clause + the two-grammar-family
exercise requirement.

#### E-9 — CH2 Finding 4 §10 grammar-keyed hot-leaf

Per CH2 Finding 4: α-E §10 telemetry sentence appends: "The hot-leaf
column reads as a grammar-keyed symbol path (`{grammar}::parse_*` or
equivalent); a stale inherited symbol name on a non-JSON row fails
the per-row gate the same way it fails S-P1."

#### E-10 — CH3 REVISE-2 C-5 scribe-contract count

Per CH3 REVISE-2: α-E §7 C-5 owner-paths block clarifies "29
row-keyed REDRESS entries" — one entry per reverted row (5 W14 row
keys + 23 SK-V13 CSS row keys + 1 SK-V12 W1b row key); the C-5
falsifiability gate reads "`skinny/REDRESS.md` carries 29 new
row-keyed entries."

#### E-11 — CH4 V1 C-1 LOC lower bound

Per CH4 V1: raise C-1 LOC lower bound to ≈ 2.8k to reflect 64-file
refactor reality; `[generated-size-budget]` is a separate axis from
net source LOC.

#### E-12 — CH4 V2 C-2 envelope ceiling

Per CH4 V2: expand C-2 envelope ceiling by ≈ 80 LOC to cover in-tree
Skipper-class fallback per α-B §316.

#### E-13 — CH4 V3 §9 vs §6 dependency-matrix resolution

Per CH4 V3: resolve §9 (concurrency matrix permits C-1 JSON
sub-waves to parallelise C-4) vs §6 (C-4 declares C-1 as
"must-serialise-after" dependency) internal inconsistency.
Authoritative reading: C-4 strictly serialises after C-1 finishes
ALL sub-waves; §9 matrix updates to reflect this.

#### E-14 — CH7 §1 + §3.2 C-3 / C-4 table-row lifting

Per CH7 §1 + §3.2: α-E §2 shortlist-table row for C-3 lifts the
explicit `rm -rf ... && cargo xtask regen-css && git diff` round-trip
command (or adds "see §5 + hardening V1 CH7 §3.1" pointer); C-4 row
inserts the named pre-wave row `json/numbers/direct_to_struct/main`
(or adds "see §6" pointer).

**α-E V2 hard cap: 45 min** (14 folds; most are line-level edits;
E-1 + E-3 are the architecturally heavy edits — C-3 gate expansion
and C-4 owner-paths triad discipline).

### §2.3 — α-A V2 fold packet (results-extraction; LOW priority)

α-A owns the V2 redispatch on the results-extraction. Required folds:

#### A-1 — CH1 REVISE-1 direct row-count reconciliation table

Per CH1 REVISE-1: add a 2-line reconciliation table in α-A §2
mapping ROLLING-SOTA-DELTA's 6 ADMITTED direct rows to the dispatch
§1 "4" — name the +2 (marine_ik, instruments) explicitly with their
v6 §1 row 3 binding.

#### A-2 — CH1 REVISE-2 typed extension-row annotation

Per CH1 REVISE-2: annotate the +4 extension rows in α-A §3's table
(random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 +
W15.1 update_center adjusted) with a footnote or "wave id" column.

#### A-3 — CH4 V6 c/B telemetry LOC budget

Per CH4 V6: add explicit LOC budget for c/B column addition; route
through C-2's harness scope (the comparator rebind wave touches
`report.rs` and can include c/B in the same commit).

**α-A V2 hard cap: 15 min** (3 narrow line edits).

### §2.4 — α-C V2 fold packet (REDRESS-digest; LOW priority)

α-C owns the V2 redispatch on the REDRESS-digest. Required folds:

#### C-1 — CH5 §2 P-7 triple-check gate

Per CH5 §2 REVISE #4: strengthen P-7 falsifiability gate at
`alpha-C-redress-digest.md:348-352` to a triple check — (a) distinct
symbol paths (current), (b) distinct concrete `Sink` types
(compile-time `TypeId` inequality), (c) distinct buffer addresses at
the first bench iter (runtime address inequality recorded into the
per-iter equality column).

**α-C V2 hard cap: 15 min** (1 narrow line edit).

### §2.5 — Recurrence-pattern grouping (V2 dispatch posture)

Cross-lens recurrence patterns (high concentration → high V2 priority):

#### Pattern P-α-E-C3-C4 — "α-E candidate table compresses CH7 gates"

Cited by CH7 §1 + §3.2 + §3.4 + CH4 R1 + R2 + V4 + CH1 REVISE-3 +
CH6 REV-1. Fold via E-14 + F-3 + F-4 + F-5 + F-17 + E-5 + E-1.

#### Pattern P-α-F-§3-condensation — "SYNTHESIS §3 elides α-E detail"

Cited by CH4 R1 + R2 + V4 + CH7 §3.4. Fold via F-3 + F-4 + F-5
+ F-17.

#### Pattern P-α-E-C4-substrate-laxity — "C-4 owner-paths + consumer plan permit hidden coupling slip"

Cited by CH5 REJECT + CH5 REVISE #5 + CH2 Finding 3 + CH1 REVISE-3 +
CH6 REV-1. Fold via E-3 + E-4 + E-5 + E-8 + F-13 + F-14.

#### Pattern P-α-F-staging-race — "α-A/α-C/α-D/α-F lack per-agent commits"

Cited by CH6 REJ-1. Fold via F-2 (declare α-F sole author per HANDOFF
§4 edit) — supported by the α-A/α-C/α-E V2 redispatches in §2.3/§2.4/§2.2
generating fresh per-agent commits for the V2 cycle.

#### Pattern P-numeric-divergence-4+7-vs-6+11 — "SYNTHESIS launders dispatch over peer-measured ledger"

Cited by CH6 REJ-2 + CH1 REVISE-1 + CH1 REVISE-2. Fold via F-1 +
A-1 + A-2.

#### Pattern P-cap-discipline — "α-E §10 widens 45-min outside addendum"

Cited by CH4 R3 (binding). Reconciliation note at §0.5 above. Fold
via E-2.

#### Pattern P-Lock-14-forward-invariant — "C-1 demolition without forward-invariant binding"

Cited by CH2 Finding 2 + CH5 G-SIMD triad. Fold via E-7 + F-12 + F-14.

#### Pattern P-triumvirate-not-encoded — "S-P3 constraints omit research/plan/redress separation"

Cited by CH6 REV-3 + correlates with REJ-1's staging race. Fold via F-9.

## §3 — Convergence forecast

### §3.1 — V2 fold accept-rate forecast

If the 36 V2 fold dispositions in §2 (17 F + 14 E + 3 A + 1 C + the
HANDOFF F-folds which are α-F-internal) land cleanly, the V2 cycle's
ACCEPT-rate over the same per-§ disposition population should rise to:

- CH1 53 sections; 3 REVISE → 0 (REVISE-1, REVISE-2, REVISE-3 all
  folded). Forecast: 53/53 = 100 %.
- CH2 33 sections; 4 REVISE → 0 (Findings 1–4 all folded). Forecast:
  33/33 = 100 %.
- CH3 30 sections; 2 REVISE → 0 (REVISE-1, REVISE-2 folded).
  Forecast: 30/30 = 100 %.
- CH4 34 sections; 3 REJECT + 7 REVISE → 0 if F-3 + F-4 + F-5 + F-6 +
  F-7 + E-2 + E-11 + E-12 + E-13 + A-3 + V4 land. Forecast: 34/34 =
  100 %.
- CH5 46 sections; 1 REJECT + 5 REVISE → 0 if E-3 + F-14 + F-15 +
  F-16 + C-1 + E-4 land. Forecast: 46/46 = 100 %.
- CH6 42 sections; 2 REJECT + 3 REVISE → 0 if F-1 + F-2 + F-8 + F-9 +
  E-5 land. Forecast: 42/42 = 100 %.
- CH7 36 sections; 1 REJECT + 5 REVISE → 0 if E-1 + E-2 + E-14 +
  F-17 + the §3.5 informational fold land. Forecast: 36/36 = 100 %.

Aggregate V2 forecast: **274/274 = 100 %** (subject to V2-cycle
re-disposition surfacing new findings that V1 missed; historical
SK-tranche evidence suggests V2 cycles typically surface 0–3 new
findings per lens, putting the realistic V2 aggregate at ≈ 95–98 %).

### §3.2 — Convergence path to V3

Per `ORCHESTRATOR.md §3Z`, convergence requires ≥ 95 % ACCEPT on **two
consecutive cycles**. V1 at 86.86 % does not converge. V2 with the §2
folds landed should converge at ≥ 95 % (forecast 95–98 % realistic
floor); V3 confirming pass at ≥ 95 % then locks the bracket.

**Critical path:** V2 must land E-1 (CH7 binding REJECT), E-3 (CH5
REJECT), F-1 + F-2 (CH6 REJECTs), F-3 + F-4 + E-2 (CH4 REJECTs). Each
is an architectural-language tightening, not a research re-extraction;
no source change is implicated by any V1 finding. The V2 redispatch
envelope is doc-only and should fit inside the 45-min hard caps named
in §2.

### §3.3 — Structural concerns surviving V2 fold (none)

No CH lens issues an escalation flag (CH4 §4 explicit; CH7 issues an
escalation-class REJECT but the §3.1 fold resolves it inside the V2
envelope). The architectural intent of every candidate (C-1 through
C-5) is sound under every lens; only the wording of gates, the column
set of telemetry, and the LOC/cap metadata require correction.

### §3.4 — V2 dispatch sequencing recommendation

Per `[agent-orchestration]`: avoid sub-agent races on shared files.
Recommended V2 sequencing:

1. **Parallel α-A + α-C V2 redispatches** (15 min each; touch
   different files: `alpha-A-results-extraction.md` and
   `alpha-C-redress-digest.md`). No file overlap.
2. **α-E V2 redispatch** (45 min; touches only
   `alpha-E-candidate-shortlist.md`). May parallelise with step 1.
3. **α-F V2 redispatch** (45 min; touches `SYNTHESIS.md` and
   `HANDOFF.md`). Must SERIALISE AFTER α-E because F-3, F-4, F-17
   inherit α-E values (risk class, LOC budget, table-row text).
4. **CHALLENGE V2 aggregator** dispatches CH1–CH7 fresh; V2 outputs
   commit atomically per the V1 protocol.
5. **CHALLENGE V3 confirming pass** if V2 converges.

Total V2 wall-clock: ≈ 90 min for the four α-redispatches +
≈ 30 min × 7 = 210 min CH lens-agent work (parallelisable to
~30-45 min) + aggregator. V2 cycle should close inside one orchestrator
session.

## §4 — Final aggregator verdict

V1 aggregate ACCEPT-rate **86.86 %** across 274 per-§ dispositions; 7
REJECT (3 CH4, 1 CH5, 2 CH6, 1 CH7-BINDING); 29 REVISE.

**Cycle verdict: PENDING-V2.** V1 does not converge per
`ORCHESTRATOR.md §3Z`. The CH7 BINDING REJECT on C-3's round-trip gate
(scope blind to Pattern H + bypass-header pattern) forecloses
convergence regardless of aggregate count, per
`PASS-0-OVERFIT-AUDIT.md §CH7` final paragraph.

V2 dispatch must redispatch α-F (17 folds across SYNTHESIS + HANDOFF),
α-E (14 folds in candidate-shortlist; includes binding CH7 + CH5
REJECTs), α-A (3 narrow folds), α-C (1 narrow fold). α-B and α-D
STAND; DISPATCH-CONTEXT STANDS.

V2 forecast: ≥ 95 % under realistic 0–3-new-findings-per-lens
assumption (point forecast 100 % if §2 folds land verbatim). V3
confirming pass then locks the bracket per the two-consecutive-cycle
§3Z rule.

No architectural-block surfaces. No source-side fold implicated. V2
envelope is docs-only.
