# CORPUS-AUDIT-SYNTHESIS — Restart Directory Inventory + Cleanup Ledger

This synthesises the four parallel corpus audits at `restart/research/CORPUS-AUDIT-{1,2,3,4}-*.md` (commits `56610d55` / `c9f35afb` / `f3ffe523` / `124b0068`). The user mandates: explicate every file in `restart/`, retire V2 deferrals, reconcile the three orchestrator-class prompts, and document the independent-Codex-hardening protocol.

## §1 — Inventory at audit time

| Directory | Files | Size | Cohort role |
|---|---:|---:|---|
| `restart/` (top-level) | 5 | 280K | gestalt + executable trio |
| `restart/prompts/` | 5 | 92K | orchestrator + sub-orchestrator + per-target spec |
| `restart/locks/` | 1 | 24K | 14 settled commitments |
| `restart/inheritance/` | 1 | 12K | BA-BD legacy survival map |
| `restart/corpora/` | 4 | 220K | frozen 2026-05-03 snapshots |
| `restart/audit/pass-1-substrate/` | 7 | 96K | substrate synthesis + 6 sub-agents |
| `restart/audit/pass-2-codegen/` | 7 | 144K | codegen synthesis + 6 sub-agents |
| `restart/audit/pass-3-runtime/` | 7 | 128K | runtime synthesis + 6 sub-agents |
| `restart/audit/hardening/` | 48 | 1.8M | V1-V8 hardening cycles + V1 reviewer cohort |
| `restart/research/` | 26 | 920K | INDEX + 8 topics + 4 folds + 8 deferral audits + V1-FOLD-CANDIDATES + 4 corpus audits + this synthesis |
| **Total** | **111** | **3.6M** | |

## §2 — Top-level docs reconciliation

Per audit #1: 5 files; partition cleanly; no merge required.

| Doc | Authoritative role | Disposition |
|---|---|---|
| `restart/HANDOFF.md` | Single source of truth for cold-start orientation | UPDATE — V7.1→V8 currency stale (Phase 8.4 not yet decided; addressed below) |
| `restart/README.md` | Gestalt synthesis + 14 locks anchor + SOTA synthesis | UPDATE — §12 describes retired 6-prompt PASS/SYNTHESIS suite |
| `restart/ARCHITECTURE.md` | Executable architectural specification | EXPLICATE — V8 SIMPLIFY candidates pending absorption decision |
| `restart/MASTER-PLAN.md` | Executable tranche plan (A-J + carry ledger §24 + cookbook §25) | EXPLICATE — V8 SIMPLIFY candidates pending |
| `restart/MIGRATION.md` | Per-file disposition for legacy code | EXPLICATE |

The trio (ARCHITECTURE + MASTER-PLAN + MIGRATION) is the executable authority; HANDOFF + README sit above as orientation. No overlap; each role distinct.

## §3 — Orchestrator reconciliation

Per audit #1 §4: **three orchestrators is correct, post-cleanup**.

| Prompt | Role | Genuine? | Action |
|---|---|---|---|
| `restart/prompts/ORCHESTRATOR.md` | Main entry; phase-identification protocol; phase-type fan-out; hardening-cycle naming canon | **GENUINE** main entry, not ceremonial | KEEP |
| `restart/prompts/HARDENING-ORCHESTRATOR.md` | Sub-orchestrator for hardening cycles V1-V8+ | GENUINE — but Phase 1/2 historical PASS dispatch + SYNTHESIS sections retired Phase 8.0 | UPDATE — surgical PRUNE of historical Phase 1/2 |
| `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | Sub-orchestrator for research deep-dives + fold cycles | GENUINE — clean | KEEP |
| `restart/prompts/AMENDMENT-DISPATCH.md` | Sub-orchestrator for verify-then-patch amendment cycles | GENUINE — but §3 Wave 1/2/3/4 plan is V1-historical | UPDATE — hoist §3 to schematic; preserve historical V1 plan in appendix |
| `restart/prompts/HARDENING.md` | Per-target audit specification (lenses A-K post-Phase-8.1) | GENUINE — load-bearing | KEEP — drop SPECULATIVE verdict class per V2-deferral retirement |

**ORCHESTRATOR.md is the cold-start entry**. It carries phase-identification protocol that the sub-orchestrators do not duplicate. Sub-orchestrators are invoked by ORCHESTRATOR per phase type. The user's concern (which is the right one?) resolves: ORCHESTRATOR.md.

## §4 — V2-deferral retirement ledger

The user mandates: no V2 deferrals. Items that were marked ASPIRATIONAL/SPECULATIVE-routed-to-V2 must either fold V1 or be deleted.

Cumulative ledger across the four audits:

### §4.1 — Class (a) — User-adjudicated scope partitions (~24 occurrences; **KEEP**)

These are NOT deferrals; they are user-adjudicated scope commitments. The V2 in this class is "items that compose into V1 architecturally but ship V2 via the Backend trait at ARCH §7.5":

- TS backend (`TsBackend: Backend` impl) — Lock 5 + ARCH §7.5 + MASTER-PLAN J.W3
- WASM backend (`WasmBackend: Backend` impl) — same
- `path-ts` crate — Lock 7 + Lock 11
- TS-native parse+runtime — principled fork

These class-(a) commitments stay. The Backend trait absorbs the cross-host story; V2 is naming the future impl, not deferring open architecture.

### §4.2 — Class (b) — Genuine V2 deferrals to retire (~6 occurrences)

Per audit #1 + #3 + #2:

| Occurrence | Path:line | Action |
|---|---|---|
| 1 | `restart/MIGRATION.md:803` | Drop "V2 amendment receiver" pattern; route to tranche body or delete the row |
| 2 | `restart/MASTER-PLAN.md:781` | Same (carry-ledger row) |
| 3 | `restart/MASTER-PLAN.md:787` | Same |
| 4 | `restart/MASTER-PLAN.md:788` | Same |
| 5 | `restart/MASTER-PLAN.md:790` | Same |
| 6 | `restart/prompts/HARDENING.md:53` + `:186` | Drop SPECULATIVE verdict class from V8+ verdict set (ASPIRATIONAL stays — those route to tranche bodies, not V2) |

### §4.3 — Class (c) — User-decision items (1 case)

| Item | Path:line | Decision |
|---|---|---|
| GADT / `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved | `Lock 4` (`restart/locks/14-LOCKS.md:40`) + `restart/ARCHITECTURE.md:1293` + `restart/audit/pass-1-substrate/PASS-1.md:117` + `restart/audit/pass-3-runtime/PASS-3.md:468` | **ASK** — fold V1 (full GADT surface) or delete the reserved code (no GADT, no V2 amendment) |

The reserved-without-emission `BBNF-LOCAL-EQUALITY-ANNOTATION` at PASS-1:117 + PASS-3:468 is a V8 simplification candidate (β2 — drop reserved emission infrastructure for cookbook-only). Coupled with audit #2's must-delete classification.

### §4.4 — Class (d) — V8 Tier δ items needing retirement-under-mandate (~10)

Per V8 §3 Tier δ, these were routed to "V2 amendment" or "tranche body":

| V8 Tier δ item | Original receiver | Under user mandate |
|---|---|---|
| δ1 — DK13 rank-N body | D.W3 / D.W6 (tranche body) | KEEP — tranche body is V1, not V2 |
| δ2 — Schema-mining miner telemetry refinement | D body (tranche body) | KEEP — tranche body |
| δ3 — CHR-improvement layer body | V2 amendment | **MUST RETIRE** — fold V1 or delete |
| δ4 — GADT V2 amendment | V2 amendment | **MUST RETIRE** — fold V1 or delete (couples to class-(c)) |
| δ5 — DAP integration body | Tranche I body | KEEP — tranche body |
| δ6 — LSP completion / semantic-tokens / imports | Tranche I body | KEEP — tranche body |
| δ7 — Incremental + reuse-map cookbook content | Tranche I/J body | KEEP — tranche body |
| δ8 — SOTA-throughput body | Tranche H body | KEEP — tranche body |
| δ9 — Function composition library | V2 amendment | **MUST RETIRE** — fold V1 or delete |
| δ10 — CHR-improvement | V2 amendment | duplicates δ3; **MUST RETIRE** |

So: **δ3 + δ4 + δ9 must retire** — fold V1 or delete. The user must decide each.

## §5 — Pruning ledger

Per audits #1-#4 cumulative recommendation:

| Cohort | Files | Size | Recommendation | Rationale |
|---|---:|---:|---|---|
| Top-level + prompts | 0 | 0 | UPDATE-only (no PRUNE) | All 5 top + 5 prompts load-bearing |
| Locks + inheritance + corpora | 0 | 0 | KEEP all | All cited; small surface |
| Pass dirs sub-agents | 0 | 0 | KEEP all 18 | Sealed Wave-1 evidence |
| **Hardening dir V5.1 intermediates** | 4 | 60K | **MINIMAL PRUNE** | `HARDENING-PASS-1-PASS-2-V5.1.md`, `V5.1A.md`, `HARDENING-PASS-3-V5.1.md`, `HARDENING-SYNTHESIS-V5.1.md` — never CONSOLIDATED-V5.1; V6 §10 cites cycle-name "V5.1" generically; architectural content fully absorbed |
| Hardening dir V2/V3 per-target | 0 | 0 | KEEP — sealed audit trail | AGGRESSIVE prune erases V3 four-agent re-audit + V5 metahardening evidence |
| Research fold reports | 0 | 0 | KEEP — sealed research | Cited by V6 verification trail (~25 cites); pruning orphans V6 |
| Research deferral audits | 0 | 0 | KEEP — sealed inputs | Underwrite V1-FOLD-CANDIDATES with per-cohort detail |

**Total prune target**: 4 files / 60K (~3% of hardening dir). Conservative; preserves audit trail.

## §6 — Independent-Codex-hardening protocol

Per audit #1 §6: the cold-start protocol is implicit across `HANDOFF.md` + `ORCHESTRATOR.md`; explication suffices.

### §6.1 — Cold-start reading order

Any cold-start agent (Codex, Claude, etc) reads in order:

1. `restart/HANDOFF.md` — orientation; current verdict; next move
2. `restart/prompts/ORCHESTRATOR.md` — phase-identification + dispatch protocol
3. `restart/README.md` — gestalt + 14 locks anchor
4. `restart/locks/14-LOCKS.md` — settled commitments
5. `restart/audit/hardening/HARDENING-CONSOLIDATED-V{N}.md` (most recent; currently V8) — operating verdict
6. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md`

### §6.2 — Verification block (the "git clone + cargo build" equivalent)

`HANDOFF.md` §6 carries the verification rituals (`rg` greps for the post-amendment state). Pre-dispatch, the cold-start agent:

```bash
git log --oneline -10
git status --short
# verify the most recent commit matches HANDOFF.md §3 current state
```

Then the lens-specific verification block per HANDOFF.md §6 (Lock 4 amendment landed; 6-directive grammar; Backend trait at ARCH §7.5; path! macro canonical; parse-that-regex canonical).

### §6.3 — Phase identification

Per `ORCHESTRATOR.md` §2: cold-start agent identifies the current phase from git log + HANDOFF.md §3 + most recent CONSOLIDATED-V{N}.md. If ambiguous, the orchestrator-prompt itself names the protocol.

### §6.4 — Sub-orchestrator dispatch

Per `ORCHESTRATOR.md` §3: phase-type table maps phase to sub-orchestrator. Cold-start agent reads ORCHESTRATOR's phase-type row + the named sub-orchestrator + dispatches per its protocol.

### §6.5 — Phase 8.1 additions needed

Audit #1 surfaces 3 cold-start signaling additions for ORCHESTRATOR.md or HANDOFF.md:

1. **A standing "current-phase" pointer**: HANDOFF.md §3 names it but the path is implicit; explicit `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` cite would help.
2. **A verification-block runner**: a single shell command that runs all HANDOFF.md §6 greps and reports per-grep status. Minor; `xtask` could carry it.
3. **A phase-table cross-reference in HANDOFF**: HANDOFF.md §7 lists Phase 8 sub-phases; the table would benefit from a current-status column linking to phase commit.

### §6.6 — Independent-Codex-hardening dispatch

For an independent Codex agent to harden the entire corpus from cold-start:

1. Codex reads `restart/HANDOFF.md` → identifies current state (V7.1 READY post-Phase-8.3; V8 SIMPLIFY-AVAILABLE pending Phase 8.4 decision).
2. Codex reads `restart/prompts/ORCHESTRATOR.md` → identifies the phase-type table.
3. Codex selects the hardening phase type → reads `restart/prompts/HARDENING-ORCHESTRATOR.md` + `restart/prompts/HARDENING.md`.
4. Codex dispatches 4 parallel hardener agents per HARDENING-ORCHESTRATOR Phase 3 protocol.
5. Codex consolidates per HARDENING-ORCHESTRATOR Phase 6.

This works today. No additional doc required. The Phase 8.1 additions above improve UX but are non-blocking.

## §7 — Recommended Phase 8.4 simplification fold (revised under user mandate)

The user has retired V2 deferrals. V8 §3 Tier δ items δ3, δ4, δ9 must retire. Three options:

**Option A — Fold V1**:
- δ3 CHR-improvement: fold the layer V1 into PASS-1 §3 type-system algorithm.
- δ4 GADT: fold the surface V1 into PASS-1 + ARCH §8.2 + Lock 4.
- δ9 Function composition library: fold V1 into PASS-1 §3 + ARCH §8.4.

**Option B — Delete**:
- δ3 CHR-improvement: drop the "CHR-shaped where applicable" V1-surface clause at `PASS-1.md:73`. Cite no future amendment.
- δ4 GADT: delete `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved code at `PASS-1.md:117` + `PASS-3.md:468`. No GADT V2 amendment.
- δ9 Function composition library: drop the V2 amendment carry. Function composition expressible via existing function-value surface (no library needed).

**Option C — Mixed**: fold some V1; delete others.

Recommendation: **Option B (delete)** for δ3 + δ9 (both are speculative aspirational; no V1 use case); **Option A (fold V1)** for δ4 GADT only if the user accepts the additional V1 scope (substantial). Otherwise **Option B (delete)** for δ4 too.

## §8 — Phase 8.4 + 8.5 dispatch (post-user-decision)

After user decides on δ3/δ4/δ9, Phase 8.4 dispatches 4 parallel fold agents per V8 §8 routing:

- **Agent A — PASS-1 fold** (~75 min): V8 tier α + β + γ + δ retirement edits per user decision
- **Agent B — PASS-2 fold** (~60 min): V8 tier α + β + γ
- **Agent C — PASS-3 fold** (~75 min): V8 tier β + γ + δ retirement edits
- **Agent D — SYNTHESIS trio fold** (~75 min): V8 tier α + β + δ + ε

Then **Phase 8.5 V8.1 verification rerun** (4 parallel verification agents + consolidation; ~120 min wall).

After Phase 8.5 V8.1 READY: per-tranche full-spec drafting (Wave 9+) unblocks.

## §9 — Open questions for user adjudication

1. **GADT V1 fold or delete?** Per §4.3 + §7. If FOLD: substantial V1 scope (~600 LOC at PASS-1 + ARCH §8.2 + Lock 4 amendment); if DELETE: drop `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved + delete GADT-substrate language at Lock 4.
2. **CHR-improvement V1 fold or delete?** Per §7. Recommendation: DELETE (no V1 use case; speculative aspirational).
3. **Function composition library V1 fold or delete?** Per §7. Recommendation: DELETE (function-value surface absorbs).
4. **V5.1 intermediate prune (4 files / 60K)?** Per §5. Recommendation: PRUNE (sealed evidence absorbed by V6).
5. **HARDENING-ORCHESTRATOR Phase 1/2 historical PASS-dispatch + SYNTHESIS prune**? Per §3 + audit #1 §4. Recommendation: PRUNE (Phase 1/2 retired Phase 8.0; pruning the obsolete sections cleans the sub-orchestrator).
6. **AMENDMENT-DISPATCH §3 Wave 1/2/3/4 schematic hoist**? Per §3 + audit #1 §4. Recommendation: HOIST (preserve V1-historical plan in appendix; main §3 becomes schematic).
7. **README §12 update** (describes retired 6-prompt PASS/SYNTHESIS suite). Recommendation: UPDATE (point at ORCHESTRATOR.md instead).
8. **Phase 8.1 cold-start signaling additions** (3 minor; per §6.5). Recommendation: ADD (low cost; clarifies cold-start UX).

## §10 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct, archaic-permissive. No metalanguage. Path:line citations on every concrete claim.

## §11 — Closing posture

The corpus has 111 files / 3.6M / 35,788 lines. Of these:
- **Live authoritative surface**: 11 files (~3% of file count; ~30% of line count) — README, ARCHITECTURE, MASTER-PLAN, MIGRATION, HANDOFF, 14-LOCKS, INDEX (research), V1-FOLD-CANDIDATES, PASS-1/2/3, ORCHESTRATOR.
- **Sealed reference**: ~80 files — sub-agent reports, V1-V8 hardening reports per-target, fold reports, deferral audits, topic deep-dives, V1 reviewer reports, frozen corpora.
- **Prune candidates**: 4 V5.1 intermediate files (60K).
- **V2-deferral occurrences requiring retirement**: 6 class-(b) edits + 3 user-decision items (δ3/δ4/δ9) + 1 user-decision GADT case.

The corpus is **healthy**. Pruning is conservative (4 files); architectural decisions concentrate on 4 user adjudications (δ3/δ4/δ9 + GADT). The orchestrator structure (3 prompts + 1 main + 1 audit-spec) holds; cold-start protocol is implicit-but-sufficient.

Hereupon: user adjudication on §9 questions, then Phase 8.4 + 8.5 dispatch.
