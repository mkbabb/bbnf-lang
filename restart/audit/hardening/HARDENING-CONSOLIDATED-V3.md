# HARDENING-CONSOLIDATED-V3

## §1 Target identifications

| Target | Target output audited | Hardening report | Hardening commit | Verdict | KEEP | REINVENT | DISCARD | Punch list |
|---|---|---|---|---|---:|---:|---:|---:|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` (post-Wave-1.1 + post-Wave-2; commits `f08c75a4` + `cd3441e7`) + 6 sub-agent reports + correction notes | `restart/audit/hardening/HARDENING-PASS-1-V3.md` | `396b23f8` | READY | 53 | 1 | 2 | 0 |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` (post-Wave-1.2 + post-Wave-2; commits `2778f34d` + `d206b895`) + 6 sub-agent reports + correction notes | `restart/audit/hardening/HARDENING-PASS-2-V3.md` | `77af6d51` | AMENDMENT-REQUIRED | 39 | 14 | 0 | 8+1 |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` (post-Wave-2 + post-Wave-3; commits `dceeaf32` + `70378e46`) + 6 sub-agent reports + correction notes | `restart/audit/hardening/HARDENING-PASS-3-V3.md` | `32126868` | AMENDMENT-REQUIRED | 66 | 4 | 0 | 2+2 |
| MASTER-PLAN | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` (post-Wave-2 + Wave-3; commits `3a73f212` + `70378e46`) | `restart/audit/hardening/HARDENING-MASTER-PLAN-V3.md` | `c38883bf` | AMENDMENT-REQUIRED | 63 | 9 | 0 | 6+3 |

| Cohort | KEEP | REINVENT | DISCARD | Punch-list rows before dedupe | Final verdict |
|---|---:|---:|---:|---:|---|
| Four-target V3 hardening cohort | 221 | 28 | 2 | 24 | AMENDMENT-REQUIRED |

V2 (single-agent serial author) returned READY across all four targets. V3 (four independent parallel auditors) returned AMENDMENT-REQUIRED on three of four. The divergence is structural rather than architectural: V3 surfaced cross-document conflicts, verbatim-string obligations, baseline-anchor anchoring, and Lock-clause precision that a single serial author lacked the adversarial pressure to catch. Every V3 punch item is calibration of an existing surface — not re-architecture.

The cohort cumulative KEEP fraction climbs from V1 (46% — 117 of 252) through V2 (92% — 244 of 265) to V3 (88% — 221 of 251). V3's lower KEEP fraction reflects independent challenge surface, not regression. Comparison to V1: 24 V3 items collapse from 47 V1 items, all surgeries narrowed and architectural conflicts vanished.

## §2 Cohort verdict — per-lane consolidated table

| Lane | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cumulative |
|---|---|---|---|---|---|
| 1 Lock-Adherence | KEEP 9 / REINVENT 0 / DISCARD 1 | AMEND-REQ; KEEP 5 / REINVENT 4 / DISCARD 0 | KEEP 12 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 6 / REINVENT 4 / DISCARD 0 | AMEND-REQ; KEEP 32 / REINVENT 8 / DISCARD 1 |
| 2 Sequencing | N/A | N/A | N/A | KEEP 6 / REINVENT 1 / DISCARD 0 | KEEP 6 / REINVENT 1 / DISCARD 0 |
| 3 Cohesion | KEEP 7 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 4 / REINVENT 3 / DISCARD 0 | KEEP 8 / REINVENT 1 / DISCARD 0 | AMEND-REQ; KEEP 4 / REINVENT 3 / DISCARD 0 | AMEND-REQ; KEEP 23 / REINVENT 7 / DISCARD 0 |
| 4 SOTA-Anchoring | KEEP 5 / REINVENT 0 / DISCARD 0 | KEEP 7 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 5 / REINVENT 1 / DISCARD 0 | AMEND-REQ; KEEP 4 / REINVENT 2 / DISCARD 0 | AMEND-REQ; KEEP 21 / REINVENT 3 / DISCARD 0 |
| 5 Grammar-Authoritative | KEEP 9 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 6 / REINVENT 1 / DISCARD 0 | KEEP 7 / REINVENT 0 / DISCARD 0 | KEEP 5 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 27 / REINVENT 1 / DISCARD 0 |
| 6 Generated-Code-Budget | KEEP 4 / REINVENT 0 / DISCARD 0 | KEEP 6 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 7 / REINVENT 1 / DISCARD 0 | KEEP 4 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 21 / REINVENT 1 / DISCARD 0 |
| 7 Friction-Forecast | KEEP 6 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 3 / REINVENT 4 / DISCARD 0 | KEEP 9 / REINVENT 0 / DISCARD 0 | KEEP 5 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 23 / REINVENT 4 / DISCARD 0 |
| 8 Carry-Deferral | KEEP 6 / REINVENT 0 / DISCARD 0 | KEEP 4 / REINVENT 1 / DISCARD 0 | KEEP 11 / REINVENT 0 / DISCARD 0 | AMEND-REQ; KEEP 5 / REINVENT 1 / DISCARD 0 | AMEND-REQ; KEEP 26 / REINVENT 2 / DISCARD 0 |
| 9 Greenfield-Discipline | KEEP 7 / REINVENT 1 / DISCARD 1 | KEEP 4 / REINVENT 1 / DISCARD 0 | KEEP 7 / REINVENT 1 / DISCARD 0 | KEEP 4 / REINVENT 0 / DISCARD 0 | KEEP 22 / REINVENT 3 / DISCARD 1 |

| Verdict class | Count | Meaning |
|---|---:|---|
| KEEP | 221 | Ratified architecture surviving independent challenge. |
| REINVENT | 28 | Surgical amendment required; surface present, calibration absent. |
| DISCARD | 2 | Specific clauses to retire (PASS-1's confirmed deletions). |

The cohort verdict is **AMENDMENT-REQUIRED**, not RE-DRAFT. Every V3 punch item is calibration; no architectural conflict surfaced.

## §3 Cross-target conflicts (post-amendment)

V1 carried 14 cross-target conflicts. V2 ratified 14-of-14 closures via single-agent verification. V3 independent audit confirms 11 of those 14 closures held; 3 reopen as cross-document calibration:

| V3 Conflict | Sources | Per-target verdicts | Resolution |
|---|---|---|---|
| Path canonicalisation `passes::types` vs `passes::layout` | `restart/ARCHITECTURE.md:435-442` carries `types/`; §6/§7.3/§8.2 narrative + Lock 2 + MASTER-PLAN C.W1 use `passes::layout`. | MASTER-PLAN AMEND-REQ. | Replace `types/` with `layout/` in §4.2's `passes/src/` tree. Lock 2 names `passes::layout`; the §4.2 tree must reflect it. |
| bbnf-bench publication status | `restart/README.md:38` "workspace-internal"; `restart/ARCHITECTURE.md:43` "Public/dev"; `restart/MASTER-PLAN.md:524` includes bbnf-bench in publish dry-run. | MASTER-PLAN AMEND-REQ. | bbnf-bench publishes as the SOTA gate runner consumed by integrators reproducing benchmarks. Amend `restart/README.md:38` to "crates.io" via SYNTHESIS narrow-amendment scope (touched as exception only — see §4 routing). |
| Per-grammar baseline source-of-truth | `restart/ARCHITECTURE.md:1273-1281` carries firm numerics; `restart/MASTER-PLAN.md:638-649` says "baseline recorded at A.W2." | MASTER-PLAN AMEND-REQ. | ARCHITECTURE §12.1 is the firm baseline; amend MASTER-PLAN §20 to drop "recorded at A.W2"; reference ARCHITECTURE §12.1. |

The remaining 11 V1 conflicts (Backend IR ownership, path crate names, `pointer!`/`select!`, layout terminology overall, BBNF extension surface, Lock 14 yaml proof, per-X tables, generated budget, SOTA close, OpenFrame residue, package-name) all stay closed.

## §4 Punch list consolidation

Twenty-four V3 surgeries collapse to twenty discrete punch items (4 cross-target dedupes between PASS-2 lookbehind and SYNTHESIS LookbehindWidth diagnostic; PASS-2 BBNF-OPT + SYNTHESIS BBNF-OPT vocabulary; PASS-3 visitor diagnostics + SYNTHESIS BBNF-VISIT-* table; PASS-3 baseline + SYNTHESIS firm baseline reconciliation).

Routing matrix (3 narrow-amendment agents on non-overlapping write surfaces):

| Route | Primary surface | Items | Estimated wall |
|---|---|---|---|
| PASS-2 narrow | `restart/audit/pass-2-codegen/PASS-2.md` | P2-1: Lock 2 layout-canon clause; P2-2: Lock 3 unified cursor obligation; P2-3: deny-gate path widening; P2-4: 3-row template schema (visitor_bitflags / bump_arena / incremental_marker); P2-5: yaml two-surface invariant; P2-6: 6 verbatim diagnostic strings (BBNF-GEN001/014, BBNF-CODEGEN021/033, BBNF-LIFE009, BBNF-SEM040); P2-7: BBNF-OPT001/002 Pratt/SIMD misfire codes; P2-8: swc rustdoc URL → corpus path:line; P2-9 (optional): yaml smoke regen Tranche-G receiver pin. | 30 min |
| PASS-3 narrow | `restart/audit/pass-3-runtime/PASS-3.md` | P3-1: bench-row table competitor + platform columns (binding); P3-2: generated-API budget W3 baseline anchors (binding); P3-3: yaml-row host-route cell explication (non-blocking); P3-4: visitor cookbook routing into §6b ledger (non-blocking). | 25 min |
| SYNTHESIS narrow | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` (+ `restart/README.md` exception for bbnf-bench publication-status reconciliation) | M1: `passes::types/` → `passes::layout/` at ARCH §4.2 line 435-442 + propagation; M2: Lock 11 incubation vs publication split at MASTER-PLAN J.W3; M3: bbnf-bench publication conflict resolution (README §2 line 38 + ARCH §1 line 43); M4: H.W3 WASM gate competitor anchor; M5: per-grammar baseline source-of-truth (drop "recorded at A.W2" at MASTER-PLAN §20); M6: ARCHITECTURE diagnostic-vocabulary table (new §7.4 with 12 codes); M7: carry-ledger asymmetry consolidation (MIGRATION §20 → MASTER-PLAN §24 with `Source: migration` tag); M8 (polish): H.W2 "skipped metadata" CI-readable skip-marker rewrite; M9 (polish): json/canada simd-json verbatim from `restart/corpora/SOTA.md`. | 45 min |

Total estimated wall-time at 3 parallel agents: 45 minutes (longest serializes the cohort).

## §5 Final readiness verdict

**AMENDMENT-REQUIRED**.

V3 cohort returns one READY (PASS-1) and three AMENDMENT-REQUIRED (PASS-2, PASS-3, MASTER-PLAN trio). Surgeries are surgical, not architectural. The narrow-amendment cycle dispatches 3 parallel agents on non-overlapping surfaces; estimated wall ~45 min serialized at the longest. After the narrow amendments commit, a V4 hardening rerun (3 agents on the 3 amended targets; PASS-1 carries V3-READY) gates per-tranche full-spec drafting.

Re-draft thresholds (from `HARDENING-CONSOLIDATED.md` §5): none currently met. Tape/direct union holds. Backend IR ownership holds. yaml two-surface proof holds. Numeric SOTA gates hold. B/C and C/E/H sequencing hold. Generated-code budgets hold. Carry ledgers hold. `pointer!`/`select!` hold. `@error(recover)` holds. OpenFrame archaeology holds. The V3 verdict is calibration of existing surfaces; no architectural posture is contested.

Decision rule applied: AMENDMENT-REQUIRED-RERUN with multi-agent narrow cycle (24 items > 5; 3 non-overlapping surfaces).

## §6 Voice + discipline locks

The amended documents must preserve the voice and discipline locks from `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage. Path:line citations on concrete claims. Per-X tables for "all grammars" claims. Receiver/blocker/receiving-gate triple on every carry. No quick solutions. No legacy code uncontested. No overfitting. Idiomatic gestalt.

## §7 Closing posture

Hereupon the next step is a narrow-scope amendment dispatch — three parallel agents covering PASS-2 (8 items), PASS-3 (2 binding + 2 non-blocking), and the SYNTHESIS trio (5 material + 2 polish + 1 cross-document README exception) — followed by a V4 hardening rerun on the three amended targets. PASS-1 carries V3-READY and bypasses the rerun. After V4 returns READY, per-tranche full-spec drafting is unblocked.
