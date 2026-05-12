# Handoff — bbnf-lang Greenfield Restart

Date: 2026-05-12 (post-V9.2-refutation + six-agent comparative-profile cohort + grand-synthesis spec amendment)
Status: **V9.3-PENDING — SOTA-BEAT-DESIGN ratified; implementation packet ready for dispatch.**
Audience: the next agent or human picking up this work.

This document is the single source of truth for orienting cold. Read it end-to-end before reading anything else; it tells you what the project is, where the work has been, where it is now, and what the next move is. Every claim cites a path so you can verify.

---

## §1 — What this project is

bbnf-lang is a **grammar-driven, multi-backend parser generator** producing SOTA-class typed parsers from `.bbnf` grammar files. The user-facing API is familiar (sonic-rs lazy-value idioms; lightning-css visitor idioms; jq-style path access); the internals are the apotheosis (CSP-backed bidirectional type system; e-graph-driven rewrite engine; shape miner that auto-detects Pratt and SIMD opportunities; cost model unified across the parser and the regex engine; IR + per-backend lowerer).

The anthem: **everything is grammar-derived.** Every grammar plugs into the fleet via two declarative surfaces — (a) a grammar source file `<name>.bbnf` and (b) a workspace metadata block `[workspace.metadata.bbnf.grammars.<name>]` in the root `Cargo.toml`. Adding a 10th grammar requires nothing else: no new crate, no per-grammar match arm in any generic crate, no per-grammar hand-written runtime module. This is Lock 14 — full grammar generalisation; zero overfitting — and it is the single most consequential discipline of the restart.

bbnf is a **meta-grammar**: it generates parsers for extant target languages (Rust V1; WASM + TS deferred V2). bbnf is not itself a runtime; it banks on the host language's facilities (Rust borrow checker + lifetime system; WASM linear memory; TS GC) at the pre-lower layer where appropriate.

Read in order:

1. `restart/README.md` — gestalt synthesis. The architectural commitments, the BBNF extensions, the optimization apotheosis, the type system, the value API, the SOTA synthesis, the 16 locks, the process.
2. `restart/locks/14-LOCKS.md` — the sixteen architectural commitments (Locks 1-14 original; Locks 15-16 land 2026-05-12 per the grand-synthesis spec amendment).
3. `restart/skinny/audit/SOTA-BEAT-DESIGN.md` — the empirical synthesis + three-phase plan + per-target SIMD primitive selection. **Read this third; it is the executable architectural target post-V9.2.**
4. `docs/precepts/instructions/STYLE.md` — voice + discipline (governs all writing).
5. `docs/precepts/instructions/LESSONS-LEARNED.md` — failure-mode anatomy (governs all decisions).

---

## §2 — Where the work has been

Single-round greenfield restart began 2026-05-04 after a compounded-contrivance archive of the prior restart. Waves landed through V9.2 + six-agent comparative-profile cohort:

| Wave | Phases | Outcome |
|---|---|---|
| 1-7 | PASS-1/2/3 dispatch + SYNTHESIS trio + V1-V7.1 hardening | V7.1 READY (99% KEEP fraction) |
| 8 | Lenses I/J/K simplification audit + V8/V8.1 cycles | READY-WITH-NARROW-RESIDUE |
| Codex V9 | V9 + V9.1 verification + corpus amendments | V9.1 READY after narrow amendments |
| **Skinny SK-V1** | Five-quadrant skinny implementation spec + SK-V1 audit | SK-AMENDMENT-REQUIRED-NARROW; 20-item C1-C20 cross-quadrant punch list |
| **Skinny implementation + user iteration** | User landed runnable prototype at `skinny/`; REDRESS items 1-19; three false routes (dispatch-table REDRESS-17, 12-byte token REDRESS-18, pair-token fusion REDRESS-16) measured-and-rejected; outcome G three times | Empirical eager-tape ceiling at ~12.5K Mbps T1 vs sonic-rs ~21K Mbps reference |
| **SK-V2 audit + amendment cohort** | 5-target SK-V2 audit + 6th-agent LAZY-TAPE-DESIGN proposal + 5 amendment agents applied 72/73 text-propagation items | SK-V2 closed at amendment level |
| **V9.2 V1-corpus audit** | 4-target V9.2 hardening cohort audited LAZY-TAPE-DESIGN.md against V1 PASS-1/2/3 + MASTER-PLAN trio | AMENDMENT-REQUIRED-NARROW (CONDITIONAL); ~22 deduplicated cross-quadrant punch items; Lock 1 amendment text drafted but conditional |
| **Skinny v2 lazy-mode implementation** | User implemented lazy-offset tape per `LAZY-TAPE-DESIGN.md` §10 (~860 LOC); re-benched | **REFUTED at outcome G** (twitter T1 = 11780 Mbps < 13K threshold; `skinny/RESULTS.md:5-7`); materialisation bytes fell 67% but throughput did not validate |
| **Six-agent comparative-profile cohort 2026-05-12** | Three samply profiles (skinny lazy-mode + sonic-rs no-LTO + simdjson stage1/stage2) + DAVID/asmjson research + handwritten ASM catalog + SIMD intrinsics catalog beyond sonic-rs | Architectural lever identified: codegen template shape (structural-index-driven typed parse), not substrate representation. `restart/skinny/audit/SOTA-BEAT-DESIGN.md` synthesises. |
| **Grand-synthesis spec amendment 2026-05-12** | New `SOTA-BEAT-DESIGN.md` master document + Lock 15 (build-profile discipline) + Lock 16 (SIMD/ASM admissibility allowlist) + SUBSTRATE.md §1.5→REFUTED + §1.6 structural-index-driven canonical + COMPILER.md §3.3 `CursorDispatch` BIR + §3.4 normative codegen contract + BENCH.md §6 `G-fusion-quality` outcome class + §7.9 comparative-profile primitive + ARCH §7.2 `CursorDispatch` BIR variant + MASTER-PLAN §13 H tranche converted concrete + §13.1 admissible SIMD primitives table | **V9.3-PENDING — implementation packet ready** |

Cumulative commit count: ~90+ across all cycles.

---

## §3 — Current state (post-grand-synthesis amendment; pre-implementation)

**Current operating verdict: `restart/skinny/audit/SOTA-BEAT-DESIGN.md` (ACTIVE proposal).** The V9.2 lazy-tape Lock 1 amendment is **discarded** per the V9.2 conditional staging protocol's outcome-G clause (`HARDENING-MASTER-PLAN-V9.2.md` §18). Lock 1 stands verbatim. Two new locks land (Lock 15 build-profile discipline; Lock 16 SIMD/ASM admissibility allowlist).

**Empirical diagnosis (load-bearing; cross-comparison cycle budget verified)** (cite: `skinny/profile/{PROFILE-REPORT,ASM-REPORT,sonic-rs-v2/PROFILE-REPORT,simdjson-v2/PROFILE-REPORT}.md`):

| Parser | Twitter c/B | Wall-clock Mbps | Hot-leaf count | Architectural shape |
|---|---:|---:|---:|---|
| simdjson DOM | 1.142 | ~24500 (3.06 GB/s) | 2 | Stage1 writes structural index; stage2 reads `tape[i]` and dispatches; **never re-scans source for whitespace/delimiters** |
| sonic-rs LazyValue (the 18552 Mbps anchor) | ~1.5 | 18552 (2.32 GB/s) | n/a (LazyValue path; reference) | Direct-to-struct; SIMD primitives + prefix-XOR via CLMUL |
| sonic-rs typed-Value DOM (measured M5 Max) | ~1.5 | 2782 (0.35 GB/s) | **1** | LTO + codegen-units=1 fuses entire SIMD kernel into `parse_object`/`parse_array` |
| **Skinny lazy-mode (current; outcome G)** | **~2.5** | **11780 (1.47 GB/s)** | **5+** | Sidecar SIMD scan + recursive descent that ignores the index and re-scans bytes via `skip_ws`+`peek` |
| asmjson DOM (Zen 4 AVX-512BW; published) | ~0.4 | 10930 MiB/s ≈ 87K Mbps | (collapsed-stage; 1 fused) | 9-state FSM with PC-as-state direct threading; classifier IS the parser |

**Architectural verdict**: the substrate is not the bottleneck (four perturbation routes measured-and-rejected). The codegen template shape is. Our `parse_value` ignores the structural index it computes; the comparative profiles verify that simdjson stage2 reads `&buf[*(next_structural++)]` and never re-scans source bytes outside `parse_string`/`parse_number` primitives. The architectural lever is the **structural-index-driven typed-parse template** (`SOTA-BEAT-DESIGN.md` §2).

**Unit calibration (load-bearing)**: "Mbps" in skinny benchmarks is **megabits per second** (formula `bytes × 8000 / ns` at `skinny/crates/bbnf-bench/src/report.rs:192`). The 7K Mbps gap to sonic-rs LazyValue (18552 Mbps) = 875 MB/s; the 13K Mbps gap to simdjson DOM = 1.6 GB/s. T_README target = 380 µs ≈ 13.3K Mbps. The cycle-per-byte (c/B) gate is host-clock-invariant and comparator-anchored per `BENCH.md` §7.9.

**Two-target SOTA-BEAT** (per user mandate 2026-05-12):

| Target | Anchor | Required closure | Mechanism |
|---|---:|---:|---|
| Sonic-rs LazyValue | 18552 Mbps twitter | ≥ 17000 Mbps (5% margin) | Phase 1 (NEON intrinsic upgrade `bbnf-simd/aarch64/`) + Phase 2 (structural-index-driven codegen template `BirNode::CursorDispatch`) |
| Simdjson DOM | ~24500 Mbps twitter | ≥ 25000 Mbps | Phase 3 (x86_64 AVX-512 VBMI2 backend `bbnf-simd/x86_64/avx512_vbmi2/`: `_mm512_mask_compressstoreu_epi8` + `_mm512_ternarylogic_epi64` + `vpermi2b`) |

Beat-both on x86_64 hardware requires Phase 1 + 2 + 3; beat-sonic-rs alone on arm64 host requires Phase 1 + 2. Phase 4 (collapsed-stage asmjson-class AVX-512 backend) is aspirational at ~50K Mbps target; not on the V1 close gate.

**What has settled** (do not relitigate):
- 16 architectural locks post-2026-05-12 (Locks 1-14 + Locks 15-16 grand-synthesis additions).
- 35-question architectural interrogation.
- Backend trait at ARCH §7.5 (V1 RustBackend; V2 WasmBackend + TsBackend deferred).
- 6-directive grammar: `@import`, `@host fn`, `@error(recover)`, `@layout`, `@pretty`, `@token`.
- `path!` macro canonical; `parse-that-regex` canonical; `regex-automata` retired.
- The skinny exists; its purpose is prior-validation of substrate viability — that purpose is **discharged** with the structural-index-driven design surviving and the lazy-tape design refuted.
- Eager-tape `Tape<'input>` substrate is the V1 canonical (Lock 1 verbatim; no amendment).
- Four substrate-amendment routes measured-and-rejected: dispatch-table, 12-byte skipless token, pair-token fusion, lazy-offset tape.
- `Mbps` unit = megabits per second per the bench formula.

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

## §5 — File map (post-grand-synthesis amendment)

| Path | Status | Purpose |
|---|---|---|
| `restart/README.md` | Live | Gestalt anchor; 14 locks; SOTA synthesis. README header references 16 locks pending the README amendment pass. |
| `restart/ARCHITECTURE.md` | Live (post-2026-05-12) | Executable architectural spec; §7.2 BIR alphabet carries `CursorDispatch` (21-variant shape). |
| `restart/MASTER-PLAN.md` | Live (post-2026-05-12) | Tranche A-J; §13 Tranche H converted from aspirational to concrete with H.W1-H.W6 waves; §13.1 admissible SIMD primitives allowlist. |
| `restart/MIGRATION.md` | Live | Per-file disposition. |
| `restart/locks/14-LOCKS.md` | Live (Locks 15-16 added 2026-05-12) | Sixteen architectural commitments. |
| `restart/skinny/audit/SOTA-BEAT-DESIGN.md` | **Active proposal** | Empirical synthesis + three-phase plan + per-target SIMD primitive selection + falsifiability gates + implementation sequence. Reads as the executable target. |
| `restart/skinny/audit/LAZY-TAPE-DESIGN.md` | **SUPERSEDED** | Historical record of the lazy-tape amendment; refuted at outcome G; preserved for audit trail. |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.2.md` | Live (DISCARDED at Wave 2 outcome G) | V9.2 conditional staging punch list; outcome-G clause fires; trio reverts to eager-only Lock 1 canonical. |
| `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE,HARDENING}.md` | Live (post-2026-05-12) | Skinny spec corpus; deviation ledger carries 5 new rows post-refutation; §1.6 + §3.3 + §3.4 + §6 + §7.9 land. |
| `restart/skinny/audit/HARDENING-{SUBSTRATE,COMPILER,BENCH,WORKSPACE,INDEX,CONSOLIDATED}-SK-V2.md` | Live | SK-V2 cohort audit reports. |
| `restart/skinny/audit/HARDENING-*-SK-V1.md` | Reference | SK-V1 cycle history. |
| `skinny/` | Live | Runnable JSON skinny prototype workspace (Cargo + crates + grammars + xtask + bench harness); contains RESULTS.md + REDRESS.md empirical evidence; profile artefacts at `skinny/profile/`. |
| `skinny/profile/{PROFILE-REPORT,ASM-REPORT,sonic-rs-v2/PROFILE-REPORT,simdjson-v2/PROFILE-REPORT}.md` | Live | Six-agent comparative-profile cohort outputs 2026-05-12. |

---

## §6 — Next move

**Phase 1 + Phase 2 implementation against the skinny workspace** per `restart/skinny/audit/SOTA-BEAT-DESIGN.md` §7. The detailed implementation packet (`IMPLEMENTATION-PACKET-SOTA-BEAT.md` at `restart/skinny/audit/`, forthcoming under separate dispatch) carries the verbatim edicts in execution order. Headline steps:

1. **Step 0 Lock 15 enforcement** (~5 LOC, 2 min): `skinny/Cargo.toml [profile.release] lto=true codegen-units=1 panic="abort" debug=true`. Re-bench baseline first; record delta in `RESULTS.md` under "build-profile-only".
2. **Step 1 micro-cleanups** (~8 LOC, 10 min): pre-size offset Vec at `input.len()/4`; delete `TapeAssembler::finish` shrink-to-fit pair (two `__rust_realloc` per parse, zero benefit).
3. **Step 2 inline + fuse** (~25 LOC, 20 min): `#[inline(always)]` on `JsonNodeKind::at_cursor`; verify byte→kind dispatch fuses with iterator `next` in `cargo asm`.
4. **Step 3 Phase 1 NEON intrinsic upgrade** (~70 LOC, 4-6 hr): new `crates/bbnf-simd/` crate; `aarch64/` submodule with `vqtbl4q_u8` classifier, `vshrn_n_u16`+`vsri`+`zip1` Validark movemask, `vld1q_u8_x4` quad-load. Exhaustive 256-byte parity tests.
5. **Step 4 Phase 2 codegen template inversion** (~50 LOC IR + 350 LOC codegen + 80 LOC HasEsc + 20 LOC drop bypass, 2-3 days): `BirNode::CursorDispatch` variant; rust template generator rewrite at `crates/codegen/src/lower/rust.rs`; HasEsc flag at scan time + lazy borrow in `parse_string`; set_len(0) drop bypass.
6. **Step 5 comparative re-profile** (~1 hr): produce `skinny/profile/skinny-v3-implemented/`; compute hot-leaf count + class attribution + cycle-per-byte; write `skinny/profile/COMPARISON-v3.md` against `sonic-rs-v2` + `simdjson-v2`.
7. **Step 6 Phase 3 AVX-512 VBMI2 conditional** (~200 LOC, x86_64 only): `bbnf-simd/x86_64/avx512_vbmi2/`.
8. **Step 7 Phase 4 collapsed-stage backend conditional** (~600 LOC, x86_64 AVX-512BW only; aspirational): `crates/runtime/src/backends/collapsed_stage_avx512/`.

| Outcome | Action |
|---|---|
| Steps 0-3 land Phase 1 gate (T1 ≥ 14K Mbps twitter) | Phase 1 validates; proceed to Step 4 |
| Steps 0-4 land Phase 2 gate (T1 ≥ 17K Mbps twitter; ≤ 3 hot leaves; ≤ 1.4 c/B) | **SOTA-BEAT sonic-rs achieved on arm64 host**; proceed to Step 5 comparative re-profile; document V1 SOTA-BEAT verdict |
| Step 4 misses Phase 2 gate (T1 < 14K Mbps) | Re-profile and re-attribute; **do not amend Lock 1**; the architectural inversion is the lever, not the substrate |
| Steps 0-6 land Phase 3 gate (T1 ≥ 25K Mbps on x86_64 AVX-512 VBMI2) | **SOTA-BEAT simdjson achieved on x86_64**; both-target close documented |

Expected wall: Steps 0-3 ~1 day; Step 4 ~2-3 days; Step 5 ~1 day; Steps 6-7 conditional and parallelizable on x86_64 hardware availability.

---

## §7 — Verification rituals

Before any phase dispatch:

```bash
git log --oneline -10
git status --short
```

Commit head should reflect the grand-synthesis spec amendment. For per-target verification:

```bash
# Grand-synthesis amendments landed
head -30 restart/skinny/audit/SOTA-BEAT-DESIGN.md
grep -c "^15\.\|^16\." restart/locks/14-LOCKS.md  # should return at least 2 (Locks 15, 16)
grep "G-fusion-quality" restart/skinny/BENCH.md   # should return one match

# Lazy-tape design SUPERSEDED
head -5 restart/skinny/audit/LAZY-TAPE-DESIGN.md  # should carry [SUPERSEDED — 2026-05-12]

# V9.2 punch list discarded per outcome-G clause
grep -n "DISCARDED\|outcome G" restart/HANDOFF.md  # this file confirms

# Skinny corpus + bench results + profile artefacts
cat skinny/RESULTS.md     # outcome G on all three corpora
ls skinny/profile/        # PROFILE-REPORT.md + ASM-REPORT.md + sonic-rs-v2/ + simdjson-v2/
ls skinny/profile/sonic-rs-v2/    # PROFILE-REPORT.md + 12 profile artefacts + noinline.patch + throughput.json
ls skinny/profile/simdjson-v2/    # PROFILE-REPORT.md + 12 profile artefacts

# Lock 1 NOT AMENDED (V9.2 conditional discarded)
rg -n 'tape_mode|lazy-mode' restart/locks/14-LOCKS.md  # should return zero matches

# Locks 15 + 16 landed
rg -n 'lto = true|codegen-units' restart/locks/14-LOCKS.md          # Lock 15 evidence
rg -n 'admissibility allowlist|vqtbl4q_u8|vpcompressb' restart/locks/14-LOCKS.md  # Lock 16 evidence
```

---

## §8 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage; never cite "the prompt said" or "the user asked". Path:line citations on every concrete claim. Per-X tables for "all grammars" / "all backends" / "all topics" claims. Receiver / blocker / receiving-gate triple on every carry. No quick solutions. No legacy code uncontested. Lock 14 is the binding generalisation discipline — full grammar generalisation; zero overfitting. Lock 15 + Lock 16 are the new 2026-05-12 disciplines for build-profile fusion + SIMD/ASM admissibility.

---

## §9 — Closing posture

Hereupon the next move is Phase 1 + Phase 2 implementation against the skinny workspace per `SOTA-BEAT-DESIGN.md` §7. The lazy-tape route is closed (refuted at outcome G); the structural-index-driven codegen template is the surviving architectural lever (per `skinny/profile/simdjson-v2/PROFILE-REPORT.md` architectural verification + `skinny/profile/sonic-rs-v2/PROFILE-REPORT.md` fusion-quality evidence). The 16 locks govern. The precepts speak. The greenfield holds. The two-target SOTA-BEAT (sonic-rs LazyValue + simdjson DOM) is achievable in arm64 + x86_64 phases per the grand-synthesis spec.

Read `restart/skinny/audit/SOTA-BEAT-DESIGN.md` end-to-end. Then dispatch the implementation packet against the skinny workspace.
