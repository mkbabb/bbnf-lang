# Handoff — bbnf-lang Greenfield Restart

Date: 2026-05-12 (post-V9.3 corpus-expansion + dav1d/asm-monolith lift + yyjson lever discovery + contrivance redress)
Status: **V9.4-READY — multi-wave implementation packet at `IMPLEMENTATION-PACKET-V2.md` ready for dispatch.**
Audience: the next agent or human picking up this work.

This document is the single source of truth for orienting cold. Read it end-to-end before reading anything else; it tells you what the project is, where the work has been, where it is now, and what the next move is. Every claim cites a path so you can verify.

---

## §1 — What this project is

bbnf-lang is a **grammar-driven, multi-backend parser generator** producing SOTA-class typed parsers from `.bbnf` grammar files. The user-facing API is familiar (sonic-rs lazy-value idioms; lightning-css visitor idioms; jq-style path access); the internals are the apotheosis (CSP-backed bidirectional type system; e-graph-driven rewrite engine; shape miner that auto-detects Pratt, SIMD, and **backend shape** opportunities; cost model unified across the parser and the regex engine; IR + per-backend lowerer).

The anthem: **everything is grammar-derived.** Every grammar plugs into the fleet via two declarative surfaces — (a) a grammar source file `<name>.bbnf` and (b) a workspace metadata block `[workspace.metadata.bbnf.grammars.<name>]` in the root `Cargo.toml`. Adding a 10th grammar requires nothing else: no new crate, no per-grammar match arm in any generic crate, no per-grammar hand-written runtime module. This is Lock 14 — full grammar generalisation; zero overfitting — and it is the single most consequential discipline of the restart.

bbnf is a **meta-grammar**: it generates parsers for extant target languages (Rust V1; WASM + TS deferred V2). bbnf is not itself a runtime; it banks on the host language's facilities (Rust borrow checker + lifetime system; WASM linear memory; TS GC) at the pre-lower layer where appropriate.

Read in order:

1. `restart/README.md` — gestalt synthesis. The architectural commitments, the BBNF extensions, the optimization apotheosis, the type system, the value API, the SOTA synthesis, the 16 locks, the process.
2. `restart/locks/14-LOCKS.md` — the sixteen architectural commitments. Locks 1-14 original; Locks 15 (build-profile + force-inline + i-cache budget) and 16 (SIMD/ASM admissibility allowlist + abstract primitive lifts from dav1d/ffmpeg) land 2026-05-12.
3. `restart/skinny/audit/SOTA-BEAT-DESIGN.md` — empirical synthesis + five-phase plan + per-target SIMD primitive selection. **Read this third; it is the executable architectural target.**
4. `restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` — five-wave implementation packet with falsifiability gates, force-inline discipline, asm-visible cleanup items, corpus expansion verification.
5. `docs/precepts/instructions/STYLE.md` — voice + discipline (governs all writing).
6. `docs/precepts/instructions/LESSONS-LEARNED.md` — failure-mode anatomy (governs all decisions).

---

## §2 — Where the work has been

Single-round greenfield restart began 2026-05-04 after a compounded-contrivance archive of the prior restart. Waves landed through V9.4:

| Wave | Phases | Outcome |
|---|---|---|
| 1-7 | PASS-1/2/3 dispatch + SYNTHESIS trio + V1-V7.1 hardening | V7.1 READY (99% KEEP fraction) |
| 8 | Lenses I/J/K simplification audit + V8/V8.1 cycles | READY-WITH-NARROW-RESIDUE |
| Codex V9 | V9 + V9.1 verification + corpus amendments | V9.1 READY |
| Skinny SK-V1 | Five-quadrant skinny implementation spec + SK-V1 audit | SK-AMENDMENT-REQUIRED-NARROW |
| Skinny implementation + user iteration | Runnable prototype at `skinny/`; three false routes (dispatch-table, 12-byte token, pair-fusion) measured-and-rejected | Empirical eager-tape ceiling |
| SK-V2 audit + amendment cohort | 5-target SK-V2 + 6th-agent LAZY-TAPE-DESIGN proposal + 5 amendment agents | SK-V2 closed at amendment level |
| V9.2 V1-corpus audit | 4-target V9.2 hardening against LAZY-TAPE-DESIGN proposal | AMENDMENT-REQUIRED-NARROW (CONDITIONAL) |
| Skinny v2 lazy-mode implementation | Lazy-offset tape per `LAZY-TAPE-DESIGN.md` §10 (~860 LOC); re-benched | **REFUTED at outcome G**; lazy-tape Lock 1 amendment discarded |
| V9.3 six-agent comparative-profile cohort 2026-05-12 | samply skinny + sonic-rs + simdjson; asm dump; DAVID/asmjson research; handwritten ASM catalog; SIMD intrinsics catalog | Architectural lever identified: codegen template shape, not substrate |
| **V9.4 grand-synthesis cohort 2026-05-12** | **6 research agents** (dav1d/ffmpeg/VLC ASM lift, deadweight-intrinsic re-examination, tape-union design, JSON corpora + Unicode torture tests, parse-that audit, greater-arch generalization) + **6 profile agents** (skinny expanded 14-corpus, sonic-rs expanded 9-corpus × Value/LazyValue × inlined/noinline, simdjson expanded 13-corpus stage-decomposition, yyjson 7-corpus no-SIMD reference, RapidJSON + serde_json floor, cargo-asm string/Unicode paths) | **V9.4 READY** — comprehensive spec amendment landed without new directives, without new BIR variants, without contrivances |

Cumulative commit count: ~95+ across all cycles.

---

## §3 — Current state (post-V9.4 grand synthesis)

**Current operating verdict: `restart/skinny/audit/SOTA-BEAT-DESIGN.md` + `restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` (ready for dispatch).** Lock 1 stands verbatim (with one-sentence clarification appended for the tape-union per task #100). Locks 15 + 16 carry their 2026-05-12 amendments (Lock 15: LTO + force-inline + i-cache ≤20 KiB; Lock 16: extended SIMD allowlist with abstract primitive lifts from dav1d/ffmpeg). No new BBNF directives. No new BIR variants. `LayoutFacts.backend_shape` is the cost-model-derived per-rule decision (auto-detect per Lock 10 mandate); the codegen lowerer at `crates/codegen/src/lower/rust.rs` emits different access patterns for `Alt { Dispatch }` based on this fact.

**Empirical diagnosis (DOM-class comparison on M5 Max twitter; cycles/byte at 3.5 GHz)**:

| Parser | c/B | MiB/s | Lever |
|---|---:|---:|---|
| **yyjson** | **0.91** | **3687** | No SIMD; force-inline everywhere; ~18 KiB hot function i-cache resident; single-pass forward `read_num` |
| **simdjson DOM** | 1.142 | 2923 | Two-stage SIMD (stage1 structural index + stage2 typed-tape walk); NEON `tbl` classifier + UTF-8 validate concurrent with stage1 |
| **sonic-rs Value-DOM** | ~2.3 | 2438 | LTO fusion + NEON StringBlock + CLMUL prefix-XOR (LazyValue path slower per sonic-rs-expanded; ref 18552 Mbps is a different from_str-with-field-elision path) |
| **bbnf-skinny (current; `lto=thin` regression)** | 5.07 | 658 (= 5521 Mbps) | sidecar scan + recursive descent re-scanning bytes; three-Vec parallel-buffer pathology |
| **RapidJSON floor** | 7.30 | 479 | textbook recursive descent + memmove |
| **serde_json floor** | 7.80 | 449 | textbook RD + BTreeMap + Value enum allocations |

**Closure cost**: 5.6× cycle reduction to beat yyjson on twitter; 4.4× to beat simdjson DOM; 2.2× to beat sonic-rs Value-DOM. Levers stack independently and are falsifiable per phase per `SOTA-BEAT-DESIGN.md` §6.

**Critical findings from the V9.4 cohort** (load-bearing for the spec):

1. **dav1d ASM monolith pattern lift**: `x86inc.asm` vendored into `crates/bbnf-simd/ext/x86/`; `<family>[16]_<isa>.{asm|S}` directory cohesion; nasm-rs for x86 + cc-rs for arm64. The msac entropy decoder's `cnt/buf/end` cross-chunk refill is the one transferable algorithmic insight beyond simdjson/sonic-rs/yyjson. Source: `skinny/profile/.../dav1d-research` (cohort task #98).

2. **Deadweight-intrinsic re-examination** identified 5 Lock 16 additions: GFNI `vgf2p8affineqb` (2× over PSHUFB classifier on x86_64), arm64 `STNP` non-temporal pair-store (3-8% cold-cache gain on tape), arm64 `PRFM PLDL2STRM` ahead-of-cursor prefetch, `ahash` crate for `path!` dict lookups, SVE2 `svmatch_u8` (cloud-arm64 only). AMX/SME/AVX-IFMA/MOVDIR64B/BITALG reconfirmed EXCLUDE. Source: cohort task #99.

3. **Tape ∪ structural-projection union** (load-bearing CLARIFICATION not amendment): the current skinny carries THREE parallel Vec<u32> (`ParserState.structural_offsets` + `TapeAssembler.offsets` + `Tape.offsets`); only simdjson among five comparators keeps two buffers post-parse; asmjson/yyjson/RapidJSON/sonic-rs-LazyValue all use one. Lock 1 verbatim never required two; the implementation drifted. Migration: delete `ParserState.structural_offsets` + dead `TapeBuilder` (eager-era) + dead `TapeToken`/PAYLOAD_CLASS; fold three `Box<[u32]>` into one + packed `flags: Box<[u8]>`. Net −180 LOC delete + 30 LOC fold + 20 LOC write-through. Source: cohort task #100.

4. **JSON corpus expansion 3 → 14 corpora + 2 UTF-8 correctness gaps**: corpus expansion at BENCH.md §3.1 (twitter + citm + canada + apache_builds + github_events + update_center + mesh + random + gsoc-2018 + marine_ik + instruments + numbers + unicode_mixed + unicode_escapes; 95-file JSONTestSuite conformance bundle); two correctness gaps in current skinny require fixes: `view.rs:203, 229` panics on invalid UTF-8 (move validation to scan stage via `simdutf8` wrap), `parse-that-regex/src/lib.rs:352` over-rejects non-character codepoints (admit per RFC 8259). Source: cohort task #101.

5. **parse-that gap closure**: new `parse-that/{string,float,unicode,integration}/` submodules; `simd-scan` → `bbnf-simd` rename + de-JSON refactor (current skinny `simd-scan` carries `scan_json_*` symbols violating Lock 14). ~700 LOC packet. Source: cohort task #102.

6. **Greater-arch generalization**: per-grammar `LayoutFacts.backend_shape` matrix — JSON pure `structural-index`; CSS L4 hybrid (hubs `structural-index`, `value` layer `eager-tape` for recovery); BBNF-self hybrid + auto-detected `PrattSpine`; Sheets hybrid + `PrattSpine`. Cost-model auto-derives; **no `@runtime` directive needed**. New `DispatchByteSet` payload extends the existing `Alt { Dispatch }` payload (not a new variant) to admit prefix-DFA discriminators for multi-byte tokens (CSS `@-rules`, Sheets `LET(`/`LAMBDA(`). Source: cohort task #103.

7. **yyjson lever discovery**: yyjson beats simdjson on twitter (3687 vs 2923 MiB/s) **without SIMD** via `always_inline` everywhere + ~18 KiB hot function fitting in i-cache + single-pass forward `read_num`. Lock 15 extension: codegen template emits `#[inline(always)]` on hot-path rules (cost-model-derived from `LayoutFacts.hot_call_graph`); target ≤ 20 KiB post-LTO. Source: cohort task #88 (yyjson agent).

8. **simdjson escape-pathology**: simdjson's textbook stage1-dominant signature inverts on escape-heavy corpora — `unicode_escapes` at 4.97 c/B is 2× worse than canada (2.44) and 6.3× worse than github_events (0.71). The escape-pathology affects ALL parsers (sonic-rs LazyValue collapses to 364 Mbps); corpus expansion is the empirical guard. Source: cohort task #90 + #89.

9. **asm-string-unicode findings**: skinny's `parse_string` no-escape happy path is ~110 instructions vs sonic-rs's ~22 (5× heavier due to triple-walk on `structural_offsets`); `parse_value` dispatch is 7-way `cmp/b.eq` cascade (should be jump table; `at_cursor` already emits one); `unescape_json_string` has 8 redundant allocator call sites (single grow-trampoline closes this); `read_hex_unit` is wrong inline boundary. Source: cohort `skinny/profile/asm-string-unicode/`.

**What has settled** (do not relitigate):
- 16 architectural locks post-2026-05-12 (Locks 1-14 + Locks 15-16).
- `LayoutFacts.backend_shape` cost-model derivation per Lock 10 auto-detect (no new directive).
- 20-variant BIR alphabet (no new variant; `Alt { Dispatch }` lowers to multiple access patterns).
- Tape ∪ structural-projection union as Lock 1 clarification (not amendment); three-Vec pathology deletion mechanical.
- Six-agent comparative-profile cohort + six-agent V9.4 research cohort empirically anchored.
- Eager-tape `Tape<'input>` substrate is V1 canonical (Lock 1 verbatim + 2026-05-12 clarification).
- Four substrate-amendment routes measured-and-rejected: dispatch-table, 12-byte token, pair-fusion, lazy-offset.
- Mbps unit calibration: skinny formula `bytes * 8000 / ns` = megabits/s; multiply by 0.119 to get MiB/s for cross-parser comparison.

---

## §4 — Prompt structure

Six prompts at `restart/prompts/` + `restart/skinny/HARDENING.md`:

1. `prompts/ORCHESTRATOR.md` — main entry; fans out to sub-orchestrators.
2. `prompts/HARDENING-ORCHESTRATOR.md` — dispatches hardening cycles.
3. `prompts/RESEARCH-FOLD-ORCHESTRATOR.md` — research deep-dives + fold cycles.
4. `prompts/AMENDMENT-DISPATCH.md` — verify-then-patch amendment cycles.
5. `prompts/HARDENING.md` — per-target audit specification (lenses A-K).
6. `skinny/HARDENING.md` — skinny audit specification (A-K + L + M + N; SK cycle namespace).

---

## §5 — File map (post-V9.4)

| Path | Status | Purpose |
|---|---|---|
| `restart/README.md` | Live | Gestalt anchor; 14 locks (pre-amendment); README amendment pass pending. |
| `restart/ARCHITECTURE.md` | Live (post-2026-05-12) | §7.2 20-variant BIR alphabet preserved; §7.2 lowering amendment note; §7.3 `LayoutFacts.backend_shape` field + derivation algorithm; §7.4 5 new diagnostic codes. |
| `restart/MASTER-PLAN.md` | Live (post-2026-05-12) | §13 Tranche H concrete with 7 waves; §13.1 admissible SIMD primitives allowlist (Lock 16 verbatim). |
| `restart/MIGRATION.md` | Live | Per-file disposition. |
| `restart/locks/14-LOCKS.md` | Live (Locks 15-16 added + extended 2026-05-12) | Sixteen architectural commitments. |
| `restart/skinny/audit/SOTA-BEAT-DESIGN.md` | **Active proposal** | Empirical synthesis + five-phase plan + per-target SIMD primitive selection + falsifiability gates + abstract dav1d primitive lifts; rewritten 2026-05-12 to remove BIR-variant contrivances. |
| `restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` | **Active dispatch packet** | Five-wave implementation with falsifiability gates + force-inline discipline + corpus expansion + UTF-8 correctness fixes; supersedes V1 packet. |
| `restart/skinny/audit/LAZY-TAPE-DESIGN.md` | **SUPERSEDED** | Historical record; refuted at outcome G; preserved for audit. |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.2.md` | Live (DISCARDED at outcome G) | V9.2 conditional staging punch list. |
| `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE,HARDENING}.md` | Live (post-2026-05-12) | INDEX: 5 new deviation rows. SUBSTRATE: §1.5 union clarification + §1.6 structural-index-driven canonical. COMPILER: §3.3 lowering contract (no new BIR). BENCH: §3.1 14-corpus expansion + §7.9 UTF-8 correctness gates + §7.10 comparative-profile primitive. |
| `skinny/` | Live | Runnable JSON skinny prototype workspace; lazy-mode implementation committed; profile artefacts. |
| `skinny/profile/{skinny-expanded,sonic-rs-expanded,simdjson-expanded,yyjson,rapidjson,serde_json,asm-string-unicode}/PROFILE-REPORT.md` | Live | V9.4 comparative-profile cohort outputs. |

---

## §6 — Next move

**Dispatch `restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` as five waves**:

| Wave | Phase | LOC budget | Cap | Twitter gate (M5 Max DOM-class) | Beats |
|---|---|---:|---|---|---|
| **Wave 1** | Phase 0 (Lock 15 enforcement: `lto=fat` + force-inline + tape-union migration) | ~255 net delta (−180 delete + 75 add + 5 Cargo.toml) | 4 hr | T1 ≥ 950 MiB/s; ≤ 4 hot leaves; c/B ≤ 3.5 | catches lto=thin regression; yyjson lever (force-inline) |
| **Wave 2** | Phase 1 (`bbnf-simd/aarch64/`: NEON intrinsic upgrade + abstract dav1d primitive lifts) | ~150 | 8 hr | T1 ≥ 1330 MiB/s; ≤ 3 hot leaves; c/B ≤ 2.5 | approaches sonic-rs Value-DOM |
| **Wave 3** | Phase 2 (`LayoutFacts.backend_shape` cost-model + `Alt { Dispatch }` two-access-pattern lowerer + corpus expansion + UTF-8 correctness gates) | ~470 + corpus + ~80 fix | 3 days | T1 ≥ 2375 MiB/s; ≤ 2 hot leaves; c/B ≤ 1.4 | **SOTA-BEAT sonic-rs Value-DOM 2438 MiB/s**; approaches simdjson 1.142 c/B |
| **Wave 4** | Phase 3 (`bbnf-simd/x86_64/avx512_vbmi2/`: VBMI2 + GFNI; conditional on hardware) | ~200 | 1-2 days | T1 ≥ 3325 MiB/s on x86_64; ≤ 2 hot leaves; c/B ≤ 1.0 | **SOTA-BEAT simdjson DOM 2923 MiB/s on Intel/Zen** |
| **Wave 5** | Phase 4 (collapsed-stage AVX-512BW backend with PC-as-state `r10` direct threading) | ~600 | 3-5 days | T1 ≥ 7400 MiB/s on x86_64; 1 hot leaf; c/B ≤ 0.45 | **asmjson 10.93 GiB/s parity territory** |

Expected total wall: Waves 1-3 ~1 week (Phase 0 + Phase 1 + Phase 2; closes SOTA-BEAT on arm64 host); Waves 4-5 ~1 week on x86_64 hardware (Phases 3 + 4; closes simdjson + asmjson territory).

| Outcome | Action |
|---|---|
| Waves 1-3 land all three gates on arm64 | **SOTA-BEAT sonic-rs achieved**; document V1 SOTA-BEAT verdict; proceed to Wave 4 conditional dispatch |
| Wave 3 misses gate (T1 < 2375 MiB/s on twitter, c/B > 1.4) | Re-profile + re-attribute per `feedback_no_workarounds`; do NOT amend Lock 1; substrate is bounded by 4-perturbation rejection cluster |
| Waves 1-4 land x86_64 gates | **SOTA-BEAT simdjson achieved on Intel/Zen**; both-target close documented |
| Wave 5 lands asmjson-parity gate | **>SOTA achieved on x86_64**; aspirational close documented |

---

## §7 — Verification rituals

Before any wave dispatch:

```bash
git log --oneline -10
git status --short
```

Commit head should reflect the V9.4 grand-synthesis amendment. Per-target verification:

```bash
# V9.4 amendments landed
head -30 restart/skinny/audit/SOTA-BEAT-DESIGN.md
head -30 restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md
grep -c "^15\.\|^16\." restart/locks/14-LOCKS.md  # ≥ 2 (Locks 15, 16)
grep -E "BBNF-BACKEND-SHAPE-INCONSISTENT|BBNF-FORCE-INLINE-MISSED|BBNF-ICACHE-BUDGET-EXCEEDED|BBNF-UTF8-INVALID-AT-PARSE|BBNF-UNICODE-NONCHAR-CODEPOINT" restart/ARCHITECTURE.md | wc -l  # 5

# Tape-union clarification + LayoutFacts.backend_shape
grep -A 5 "backend_shape" restart/ARCHITECTURE.md | head -25
grep -A 3 "tape ≡ structural projection union" restart/skinny/SUBSTRATE.md | head -10

# Corpus expansion + correctness gates
grep -c "twitterescaped\|unicode_mixed\|unicode_escapes\|marine_ik" restart/skinny/BENCH.md  # ≥ 4
grep "Gate [1-4]" restart/skinny/BENCH.md | head -8

# Profile cohort artefacts present
ls skinny/profile/{skinny-expanded,sonic-rs-expanded,simdjson-expanded,yyjson,rapidjson,serde_json,asm-string-unicode}/

# Lock 1 NOT amended (verbatim)
rg -n 'tape_mode|lazy-mode|dual-mode' restart/locks/14-LOCKS.md  # 0 matches
# Lock 1 clarification (one-sentence append)
rg -n 'structural projection IS the tape' restart/skinny/SUBSTRATE.md  # ≥ 1
```

Per-corpus reproduction:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo build --release -v 2>&1 | grep -E '\-C lto=fat' | wc -l  # ≥ 1 per workspace member
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- check-conformance
samply record --save-only --unstable-presymbolicate -o /tmp/skinny-twitter.json.gz -- ./target/release/profile-lazy twitter
```

---

## §8 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage; never cite "the prompt said" or "the user asked". Path:line citations on every concrete claim. Per-X tables for "all grammars" / "all backends" / "all topics" claims. Receiver / blocker / receiving-gate triple on every carry. **No quick solutions. No legacy code uncontested. No contrivances. No new directives. No new BIR variants. No deferrals.** Lock 14 is the binding generalisation discipline — full grammar generalisation; zero overfitting. Lock 15 + Lock 16 are the 2026-05-12 disciplines for build-profile fusion + SIMD/ASM admissibility.

---

## §9 — Closing posture

Hereupon the next move is dispatching `IMPLEMENTATION-PACKET-V2.md` Wave 1 against `skinny/`. Lazy-tape route is closed (refuted at outcome G); structural-index-driven codegen template is the surviving architectural lever (per `skinny/profile/simdjson-v2/PROFILE-REPORT.md` architectural verification + `skinny/profile/sonic-rs-v2/PROFILE-REPORT.md` fusion-quality evidence + `skinny/profile/yyjson/PROFILE-REPORT.md` no-SIMD force-inline lever). The 16 locks govern. The precepts speak. The greenfield holds. The two-target SOTA-BEAT (sonic-rs Value-DOM + simdjson DOM + asmjson aspirational) is achievable in arm64 + x86_64 phases per the grand-synthesis spec, with the cost-model auto-deriving per-rule `backend_shape` from existing Grammar IR facts.

Read `restart/skinny/audit/SOTA-BEAT-DESIGN.md` end-to-end. Then read `restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` end-to-end. Then dispatch Wave 1.
