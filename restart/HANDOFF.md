# Handoff — bbnf-lang Greenfield Restart

Date: 2026-05-12
Status: V9.2 AMENDMENT-REQUIRED-NARROW (CONDITIONAL) — Lock 1 lazy-tape amendment staged for post-implementation commit; skinny v2 lazy-mode implementation pending; SK-V2 closed at amendment level.
Audience: the next agent or human picking up this work.

This document is the single source of truth for orienting cold. Read it end-to-end before reading anything else; it tells you what the project is, where the work has been, where it is now, and what the next move is. Every claim cites a path so you can verify.

---

## §1 — What this project is

bbnf-lang is a **grammar-driven, multi-backend parser generator** producing SOTA-class typed parsers from `.bbnf` grammar files. The user-facing API is familiar (sonic-rs lazy-value idioms; lightning-css visitor idioms; jq-style path access); the internals are the apotheosis (CSP-backed bidirectional type system; e-graph-driven rewrite engine; shape miner that auto-detects Pratt and SIMD opportunities; cost model unified across the parser and the regex engine; IR + per-backend lowerer).

The anthem: **everything is grammar-derived.** Every grammar plugs into the fleet via two declarative surfaces — (a) a grammar source file `<name>.bbnf` and (b) a workspace metadata block `[workspace.metadata.bbnf.grammars.<name>]` in the root `Cargo.toml`. Adding a 10th grammar requires nothing else: no new crate, no per-grammar match arm in any generic crate, no per-grammar hand-written runtime module. This is Lock 14 — full grammar generalisation; zero overfitting — and it is the single most consequential discipline of the restart.

bbnf is a **meta-grammar**: it generates parsers for extant target languages (Rust V1; WASM + TS deferred V2). bbnf is not itself a runtime; it banks on the host language's facilities (Rust borrow checker + lifetime system; WASM linear memory; TS GC) at the pre-lower layer where appropriate.

Read in order:

1. `restart/README.md` — gestalt synthesis. The architectural commitments, the BBNF extensions, the optimization apotheosis, the type system, the value API, the SOTA synthesis, the 14 locks, the process.
2. `restart/locks/14-LOCKS.md` — the 14 architectural commitments. Lock 1 carries a CONDITIONAL 2026-05-12 amendment (dual-mode `tape_mode ∈ {"eager", "lazy"}`); the amendment text is drafted at `restart/skinny/audit/LAZY-TAPE-DESIGN.md` §4.1 but is not committed until skinny v2 lazy-mode implementation re-benches at outcome A/B/C.
3. `docs/precepts/instructions/STYLE.md` — voice + discipline (governs all writing).
4. `docs/precepts/instructions/LESSONS-LEARNED.md` — failure-mode anatomy (governs all decisions).

---

## §2 — Where the work has been

Single-round greenfield restart began 2026-05-04 after a compounded-contrivance archive of the prior restart. Waves landed through V9.2:

| Wave | Phases | Outcome |
|---|---|---|
| 1-7 | PASS-1/2/3 dispatch + SYNTHESIS trio + V1-V7.1 hardening | V7.1 READY (99% KEEP fraction) |
| 8 | Lenses I/J/K simplification audit + V8/V8.1 cycles | READY-WITH-NARROW-RESIDUE |
| Codex V9 | V9 + V9.1 verification + corpus amendments | V9.1 READY after narrow amendments |
| **Skinny SK-V1** | Five-quadrant skinny implementation spec + SK-V1 audit | SK-AMENDMENT-REQUIRED-NARROW; 20-item C1-C20 cross-quadrant punch list |
| **Skinny implementation + user iteration** | User landed runnable prototype at `skinny/`; 19-item REDRESS; two false routes (dispatch-table-as-canonical; 12-byte token) measured and rejected; outcome G three times running | Empirical ceiling at ~12.5K Mbps T1 vs sonic-rs ~21K Mbps |
| **SK-V2 audit + amendment cohort** | 5-target SK-V2 audit + 6th-agent LAZY-TAPE-DESIGN proposal + 5 SK-V2 amendment agents applied 72/73 text-propagation items | SK-V2 closed at amendment level; skinny corpus text-clean |
| **V9.2 V1-corpus audit** | 4-target V9.2 hardening cohort audited LAZY-TAPE-DESIGN.md against V1 PASS-1/2/3 + MASTER-PLAN trio | AMENDMENT-REQUIRED-NARROW (CONDITIONAL); ~22 deduplicated cross-quadrant punch items; Lock 1 amendment text drafted |

Cumulative commit count: ~80+ across all cycles.

---

## §3 — Current state (post-V9.2; pre-implementation)

**Current operating verdict: `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.2.md` (AMENDMENT-REQUIRED-NARROW, CONDITIONAL).** V9.2 audited the lazy-tape Lock 1 amendment proposal against the V1 corpus; all 4 targets returned AMENDMENT-REQUIRED-NARROW; ~22 surgical edits staged for post-implementation conditional commit.

**The conditional staging protocol** (V9.2 CONSOLIDATED §5):

- **Wave 1 (now, pre-implementation)**: V9.2 punch list is drafted and ratified but **not committed to the V1 corpus**. Audit reports land as historical record.
- **Wave 2 (post-implementation, after measurement)**:
  - **If outcome A/B/C** (T1 ≥ 14K Mbps on twitter): commits V9.2 punch list to V1 corpus verbatim; Lock 1 lands at `14-LOCKS.md:34`; ARCH §7.2/§9.1/§3.1/§5/§12.2 land; MASTER-PLAN §4/§7/§11/§14 land; MIGRATION §17 lands; PASS-1/2/3 punch items land; **V9.3 verification cycle dispatches.**
  - **If outcome G** (T1 < 13K Mbps): discards V9.2 punch list; trio reverts to eager-only canonical; SOTA-beat routes to V1 H tranche body as ASPIRATIONAL.

**Empirical diagnosis (load-bearing)** (from `skinny/RESULTS.md` and `skinny/REDRESS.md`):

| Corpus | T1 Mbps | T2 Mbps | sonic-rs Mbps | T1/sonic | Outcome |
|---|---:|---:|---:|---:|---|
| twitter | 12470 | 10063 | 18440 | 67.6% | G/NO-GO |
| citm | 12246 | 11547 | 23075 | 53.1% | G/NO-GO |
| canada | 8895 | 8177 | 12021 | 74.0% | G/NO-GO |

**What three iterations conclusively closed**:
- T1 ≈ T2 (1.005-1.055 ratio): codegen is NOT the bottleneck.
- Structural scan well above floor (66565 Mbps vs 40000 floor): scan is NOT the bottleneck.
- `alternate_dispatch_table_plan`: INVALIDATED — REDRESS item 17 (duplicate probe; real function-pointer table regressed; canonical Rust `match` is load-bearing).
- 12-byte skipless token: INVALIDATED — REDRESS item 18 (mixed parse results; canonical stays 16-byte aligned).
- Host-call dispatch overhead: PASS (~0.7 ns/call ≤ 50 ns target).
- Host-call eager string decode: MASKING signal (64.9-83% T1 ratio). **V1 JSON must keep lazy string decode**.
- Substrate materialisation is the bottleneck — per-token write bandwidth at ~16 bytes/token across 40K-167K offsets per corpus.

**The remaining honest route**: lazy-offset tape (the structural index IS the tape; node kind computed from `source[offsets[cursor]]`; no separate token stream). Proposal at `restart/skinny/audit/LAZY-TAPE-DESIGN.md` (~860 lines). Lock 1 amendment text preserves spirit (no parallel substrate; no OpenFrame clone) verbatim while admitting per-grammar `tape_mode` materialization.

**What has settled** (do not relitigate):
- 14 architectural locks (post-Phase-7.1 amendments + conditional Lock 1 2026-05-12 amendment).
- 35-question architectural interrogation.
- Backend trait at ARCH §7.5 (V1 RustBackend; V2 WasmBackend + TsBackend deferred).
- 6-directive grammar: `@import`, `@host fn`, `@error(recover)`, `@layout`, `@pretty`, `@token`.
- `path!` macro canonical; `parse-that-regex` canonical; `regex-automata` retired.
- The skinny exists; its purpose is prior-validation of substrate viability.
- Eager-tape substrate has a structural ceiling at ~1.6× sonic-rs time on JSON (3-iteration empirical conclusion).

---

## §4 — Prompt structure

Six prompts at `restart/prompts/` + `restart/skinny/HARDENING.md`:

1. `prompts/ORCHESTRATOR.md` — main entry; fans out to encapsulated sub-orchestrators per phase type.
2. `prompts/HARDENING-ORCHESTRATOR.md` — dispatches hardening cycles (V1 through V9.x).
3. `prompts/RESEARCH-FOLD-ORCHESTRATOR.md` — research deep-dives + fold cycles.
4. `prompts/AMENDMENT-DISPATCH.md` — verify-then-patch amendment cycles.
5. `prompts/HARDENING.md` — per-target audit specification (lenses A-K).
6. `skinny/HARDENING.md` — skinny per-target audit specification (lenses A-K + L premise fidelity + M falsifiability + N graduation mechanicality; SK cycle namespace).

---

## §5 — File map (post-V9.2)

| Path | Status | Purpose |
|---|---|---|
| `restart/README.md` | Live | Gestalt anchor; 14 locks; SOTA synthesis. |
| `restart/ARCHITECTURE.md` | Live | Executable architectural spec; carries user iteration amendments at §1433 area pending Wave-2 conditional Lock 1 amendment. |
| `restart/MASTER-PLAN.md` | Live | Tranche A-J; SOTA close gates. |
| `restart/MIGRATION.md` | Live | Per-file disposition. |
| `restart/locks/14-LOCKS.md` | Live (Lock 1 amendment pending Wave 2) | 14 architectural commitments. |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.2.md` | Live | Current verdict: AMENDMENT-REQUIRED-NARROW (CONDITIONAL). |
| `restart/audit/hardening/HARDENING-{PASS-1,PASS-2,PASS-3,MASTER-PLAN}-V9.2.md` | Live | V9.2 per-target absorption audits of lazy-tape proposal. |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.1.md` | Reference | Prior V9.1 READY verdict. |
| `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE,HARDENING}.md` | Live (post-SK-V2 amendments) | Skinny spec corpus. |
| `restart/skinny/audit/HARDENING-{SUBSTRATE,COMPILER,BENCH,WORKSPACE,INDEX,CONSOLIDATED}-SK-V2.md` | Live | SK-V2 cohort audit reports. |
| `restart/skinny/audit/LAZY-TAPE-DESIGN.md` | Live | Lock 1 amendment proposal (845 lines); architectural design for dual-mode tape. |
| `restart/skinny/audit/HARDENING-*-SK-V1.md` | Reference | SK-V1 cycle history. |
| `skinny/` | Live | Runnable JSON skinny prototype workspace (Cargo + crates + grammars + xtask + bench harness); contains RESULTS.md + REDRESS.md empirical evidence. |

---

## §6 — Next move

**Skinny v2 lazy-mode implementation** per `restart/skinny/audit/LAZY-TAPE-DESIGN.md` §10:

1. `skinny/crates/runtime/src/tape/` — add `offsets.rs` + `assembler.rs` (~+400 LOC); preserve eager-mode `token.rs` + `builder.rs` for non-JSON grammars.
2. `skinny/crates/codegen/src/lower/rust.rs` — mode-branching emit (~+100 LOC).
3. `skinny/crates/runtime/src/grammars/json/view.rs` — lazy-mode kind discriminator `fn kind_at_cursor(...)` template (~+200 LOC).
4. `skinny/crates/runtime/src/grammars/json/generated.rs` — shrinks under lazy mode (~-150 LOC).
5. `skinny/grammars/json.bbnf` workspace metadata: set `[workspace.metadata.bbnf.grammars.json.runtime] tape_mode = "lazy"`.
6. **Re-bench**: `cargo run -p xtask -- bench-json` against twitter / citm / canada.
7. Classify outcome per BENCH §6 matrix.

| Outcome | Action |
|---|---|
| A/B/C (T1 ≥ 14K Mbps twitter) | Apply V9.2 punch list to V1 corpus verbatim; Lock 1 amendment lands; dispatch V9.3 verification cycle. |
| G (T1 < 13K Mbps twitter) | Discard V9.2 punch list; revert to eager-only canonical; SOTA-BEAT routes to H tranche body as ASPIRATIONAL. |

Expected wall: 1-2 weeks of focused implementation + 1 day re-bench + 1 day conditional Wave-2 commit cycle.

---

## §7 — Verification rituals

Before any phase dispatch:

```bash
git log --oneline -10
git status --short
```

Commit head should reflect V9.2 cohort closure. For per-target verification:

```bash
# V9.2 audit reports landed
ls restart/audit/hardening/HARDENING-*-V9.2.md
cat restart/audit/hardening/HARDENING-CONSOLIDATED-V9.2.md | head -50

# Skinny corpus post-SK-V2 state
wc -l restart/skinny/*.md
grep -l "BEAT_BOUND\|FAITHFUL\|MASKING\|MECHANICAL" restart/skinny/*.md

# Lazy-tape proposal
wc -l restart/skinny/audit/LAZY-TAPE-DESIGN.md  # ~845 lines

# Skinny prototype + bench results
cat skinny/RESULTS.md  # outcome G three times running
cat skinny/REDRESS.md | head -50

# Lock 1 NOT YET AMENDED (conditional pending re-bench)
rg -n 'tape_mode|lazy-mode' restart/locks/14-LOCKS.md  # should return zero
```

---

## §8 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage; never cite "the prompt said" or "the user asked". Path:line citations on every concrete claim. Per-X tables for "all grammars" / "all backends" / "all topics" claims. Receiver / blocker / receiving-gate triple on every carry. No quick solutions. No legacy code uncontested. Lock 14 is the binding discipline — full grammar generalisation; zero overfitting.

---

## §9 — Closing posture

Hereupon the next move is skinny v2 lazy-mode implementation against `LAZY-TAPE-DESIGN.md`. The V9.2 cohort has staged the V1 corpus amendments conditionally; they commit only if the re-bench validates the architectural premise. If outcome G repeats, the eager-only canonical survives and SOTA-BEAT becomes ASPIRATIONAL.

The 14 locks govern. The precepts speak. The greenfield holds. The empirical falsifier has fired honestly three times.

Read `restart/prompts/ORCHESTRATOR.md` end-to-end. Then dispatch the skinny v2 lazy-mode implementation per `restart/skinny/audit/LAZY-TAPE-DESIGN.md` §10.
