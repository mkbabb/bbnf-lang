# Handoff — bbnf-lang Greenfield Restart

## Current Totality Override - 2026-05-28

Status: **G-Omega V9 is authorized and V9 CRUD is being applied. Current
implementation authority is SK-V15 W0-W11.** The locked SK-V15
PRUNE-then-REBUILD contract is `restart/skinny/tranches/sk-v15/SPEC.md`
plus `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`. SK-V14 W5B /
Pass Omega V8 remains historical/pre-block evidence only.

Next implementation dispatch after authorized CRUD is **SK-V15 W0 Baseline
and telemetry lock** through
`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`. Continue W1 through
W11 in strict `SPEC.md` order. Do not run routine Alpha/Omega churn before
W0; another Alpha/Omega loop is justified only by a concrete unrepaired
invariant or REDRESS route requiring a V1 surface, LOCKS, or wave-graph
amendment.

Sources of authority for this cycle:

- Pass Omega V9 G-Omega packet:
  `restart/audit/totality/astral/V9/G-OMEGA-PACKET.md`.
- Pass Omega V9 coherence and migration handoff:
  `restart/audit/totality/astral/V9/ΩA-coherence-audit.md` and
  `restart/audit/totality/astral/V9/ΩF-migration-handoff.md`.
- Pass Omega V9 master-plan operation list:
  `restart/audit/totality/astral/V9/master-plan-diff.md`.
- SK-V15 locked skinny surfaces:
  `restart/skinny/tranches/sk-v15/SPEC.md`,
  `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`, and
  `restart/skinny/tranches/sk-v15/HANDOFF.md`.
- T-P1 current authority:
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`
  (clean-final / G1-auto-pinned, not normal two-clean-cycle 3Z).
- T-P2 current authority:
  `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`
  (normal 3Z LOCK / G2-auto-passed).
- T-P3 current authority:
  `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md`
  (final convergence / G3 auto-passed).
- PASS-IMPL V1 blocker map:
  `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
- Historical Pass Omega V2..V8 correction packets remain provenance only;
  they no longer define current dispatch.

Read in order for current work:

1. `restart/prompts/ORCHESTRATOR.md`.
2. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
3. `restart/prompts/pass-contracts/PASS-OMEGA.md`.
4. `restart/audit/totality/astral/V9/G-OMEGA-PACKET.md`.
5. `restart/audit/totality/astral/V9/ΩA-coherence-audit.md`.
6. `restart/audit/totality/astral/V9/ΩF-migration-handoff.md`.
7. `restart/audit/totality/astral/V9/master-plan-diff.md`.
8. `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`.
9. `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`.
10. `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md`.
11. `restart/skinny/tranches/sk-v15/SPEC.md`.
12. `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`.
13. `restart/skinny/tranches/sk-v15/HANDOFF.md`.
14. `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
15. `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
    `restart/locks/LOCKS.md`, `restart/MIGRATION.md`, and this file
    (current V1 surfaces).
16. `skinny/RESULTS.md` and `skinny/REDRESS.md`.

Grammar onboarding remains three declarative surfaces only: grammar
source `.bbnf`, workspace metadata, and an optional per-grammar
declaration crate for host functions. Generated per-grammar names are
allowed only as generator output from the rostered metadata per LAC-1E-08
V+1 generated-output allowance. Generic crates must not grow grammar
switches, grammar-named public APIs, or hand-written per-grammar runtime
files per LAC-1E-15 Pattern H census + substrate-doc cleanup (live
Pattern H count = 67 hand-written runtime files under
`crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf,
google_sheets, json, math}/`; `find crates/core/src/runtime -mindepth 2
-type f -name '*.rs' | wc -l` → 67).

Dispatch rule: SK-V15 source/generated/gate/RESULTS/REDRESS edits remain
blocked until V9 CRUD completes and SK-V15 W0 dispatches through the skinny
wave-triumvirate. Every SK-V15 wave runs research -> plan -> redress under
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`; delete, retirement,
diagnostic demotion, or neutralization work must cite the matching SK-V15
dependency row before redress.

## Pass Omega V9 SK-V15 dispatch directive

Per `restart/audit/totality/astral/V9/G-OMEGA-PACKET.md`, the sequence after
G-Omega V9 authorization is:

(a) apply authorized V9 CRUD only on the approved restart surfaces;
(b) keep source, generated output, gates, `skinny/RESULTS.md`,
    `skinny/REDRESS.md`, and SK-V15 SPEC/DISPATCH read-only during CRUD;
(c) stop routine Omega/Alpha churn for this SK-V15 implementation authority;
(d) dispatch SK-V15 W0 through
    `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`;
(e) continue W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10
    -> W11 in strict `SPEC.md` order;
(f) do not close SK-V15 with doc-only proof, implementation-limited misses,
    stale CSS broadcast evidence, Pattern H deletion without provenance proof,
    scaffold Decision/lowerer claims, production FNV, or planned SK-V16
    handoff.

The current dispatch checklist:

| gate | measurable condition | source |
|---|---|---|
| G-Omega V9 | Authorized; V9 CRUD is being applied before SK-V15 W0. | `restart/audit/totality/astral/V9/G-OMEGA-PACKET.md` |
| T-P1 | CLEAN-FINAL / G1-AUTO-PINNED; not rewritten as normal two-clean-cycle 3Z. | `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md` |
| T-P2 | Normal 3Z LOCK / G2-AUTO-PASSED from V2 and V3 clean cycles. | `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md` |
| T-P3 | Final V5 convergence; G3 auto-passes. | `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md` |
| SK-V15 W0 dispatch | V9 CRUD complete; dispatch W0 through SK-V15 `DISPATCH-PROMPT.md`. | `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` |
| SK-V15 close | W11 plus PASS-IMPL V2 accepts every axis or records row-level intrinsic-block proof at HEAD. | `restart/skinny/tranches/sk-v15/SPEC.md §14` |

Gate posture: G-Omega is the only mandatory user gate under the active pin.
G1, G2, G3, G-Alpha, challenge confirmation gates, and wave-internal gates
auto-pass unless an unrepaired invariant violation or REDRESS route requires
G-Omega.

## Historical Pass Omega V1.1 close (not current authority)

Pass Omega V1.1 closed at user sign-off `2026-05-22T03:52:18Z`; record at
`restart/audit/totality/astral/V1/G-OMEGA-SIGNOFF.md`; CRUD-LOG at
`restart/audit/totality/astral/V1/CRUD-LOG.md`. The V1.1 surface state
seeded the SK-V14 reopening, which produced the audit-zero baseline that
is now current authority. SK-V13 packet is historical lineage.

## Historical SK-V6 Handoff Body (not current authority)

Date: 2026-05-15 (SK-V6 SOTA recovery active)
Status: **SK-V6 same-plane SOTA recovery is current. Current measured authority is `skinny/RESULTS.md`: full gate `N-direct / NoGo`; retained parse has 13 G rows, with `canada`, `mesh`, `marine_ik`, and `numbers` A / GO. Canada structural scan is green in the refreshed full matrix at 69075 Mbps against the 40000 Mbps NEON floor. Direct correctness is green and four direct rows pass the sonic-rs 1.10x direct slack (`citm_catalog`, `apache_builds`, `github_events`, `instruments`); 13 direct rows remain red. Strictness/output-plane columns are disclosed and must be promoted to schema v3 before any SOTA claim. SK-V5 Waves 0-5 landed the Rust-state substrate and measured redress, but the Wave 3 UTF-8 fusion prescription is refuted by REDRESS 50-55. The SK-V6 asmjson/DAV1D synthesis adds the current route: strict comparator planes, DAV1D-grade primitive admission, retained string/materialization recovery, generated DirectFieldFacts, and optional x86 CollapsedStage after the arm64 matrix closes.**
Audience: the next agent or human picking up this work.

This document is the single source of truth for orienting cold. Read it end-to-end before reading anything else; it tells you what the project is, where the work has been, where it is now, where the M5 Max comparator landscape sits, how the SK-V5 substrate generalises across the V1 grammar fleet, and what the next move is. Every claim cites a path so you can verify.

**Reading order for the next implementation agent**: `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` (dispatch authority) → `restart/skinny/tranches/sk-v6/SYNTHESIS.md` → `restart/skinny/tranches/sk-v6/SPEC.md` → `restart/skinny/tranches/sk-v6/HANDOFF.md` → `skinny/RESULTS.md` → `skinny/REDRESS.md` → `restart/skinny/tranches/sk-v6/research/`. `GRAND-SYNTHESIS-SK-V5.md`, `HANDOFF-SK-V5.md`, `NUKE-PLAN-SK-V5.md`, and `IMPLEMENTATION-PACKET-SK-V5.md` remain substrate-history inputs; they are no longer sufficient dispatch authority on their own.

---

## §1 — What this project is

bbnf-lang is a **grammar-driven, multi-backend parser generator** producing SOTA-class typed parsers from `.bbnf` grammar files. The user-facing API is familiar (sonic-rs lazy-value idioms; lightning-css visitor idioms; jq-style path access); the internals are the apotheosis (CSP-backed bidirectional type system; e-graph-driven rewrite engine; shape miner that auto-detects Pratt, SIMD, and **backend shape** opportunities; cost model unified across the parser and the regex engine; IR + per-backend lowerer).

The anthem: **everything is grammar-derived.** Every grammar plugs into the fleet via two declarative surfaces — (a) a grammar source file `<name>.bbnf` and (b) a workspace metadata block `[workspace.metadata.bbnf.grammars.<name>]` in the root `Cargo.toml`. Adding a 10th grammar requires nothing else: no new crate, no per-grammar match arm in any generic crate, no per-grammar hand-written runtime module. This is Lock 14 — full grammar generalisation; zero overfitting — and it is the single most consequential discipline of the restart.

bbnf is a **meta-grammar**: it generates parsers for extant target languages (Rust V1; WASM + TS deferred V2). bbnf is not itself a runtime; it banks on the host language's facilities (Rust borrow checker + lifetime system; WASM linear memory; TS GC) at the pre-lower layer where appropriate.

Read in order:

1. `restart/README.md` — gestalt synthesis. The architectural commitments, the BBNF extensions, the optimization apotheosis, the type system, the value API, the SOTA synthesis, the 16 locks, the process.
2. `restart/locks/LOCKS.md` — the sixteen architectural commitments. Locks 1-14 original; Lock 15 (build-profile + force-inline + i-cache budget) lands and extends 2026-05-12; Lock 16 (SIMD/ASM admissibility allowlist + abstract primitive lifts from dav1d/ffmpeg) lands 2026-05-12 and extends the same day with the post-Wave-1 5-pack of AVX-512 esoterica (k-mask arithmetic family, VPCLMULQDQ at 512-bit, AVX-IFMA `vpmadd52`, AVX-512 VNNI `vpdpbusd`, AVX-512 BITALG `vpshufbitqmb` + `vpopcntb`) and 3-pack of M5 Max NEON esoterica (LD4-interleaved 4-channel classifier, NEON ternary bitwise `BCAX`/`EOR3`, NEON port of SVE2 `svmatch_u8`).
3. `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` — **SK-V6 dispatch authority**; supersedes SK-V5 dispatch, forbids hypothesis transfer from the refuted Wave 3 UTF-8 fusion prescription, and requires Wave 1 fresh profiles before any new kernel prescription.
4. `restart/skinny/tranches/sk-v5/SYNTHESIS.md` — **SK-V5 substrate-history authority**; 417 LOC; corrected SK-V5 audit diagnoses plus the 2026-05-14 post-assay header. Its original UTF-8-fusion close language is superseded by SK-V6 and REDRESS 50-55.
5. `restart/skinny/tranches/sk-v5/research/` — 15 cohort reports (5,559 LOC): A1 comparative, A2 dav1d-process, A3 parse-that-gaps, A4 tape-union-audit, A5 grammar-generalization, A6 research-ledger, B1 parse-attribution, B2 direct-attribution, B3 native-sidecars, D1 eisel-novelty, D2 utf8-novelty, D3 derive-shape-novelty, D4 simd-split-novelty, D5 sinkonly-novelty, D6 class-ab-novelty.
6. `restart/skinny/tranches/sk-v5/HANDOFF.md`, `restart/skinny/tranches/sk-v5/NUKE-PLAN.md`, and `restart/skinny/tranches/sk-v5/SPEC.md` — SK-V5 packet history and partial landed state.
7. `skinny/RESULTS.md` — current measured skinny gate: original triad passes historically; expanded parse has 13 G rows plus four A / GO guard rows (`canada`, `mesh`, `marine_ik`, `numbers`); direct workload is correctness-green but `N-direct / NoGo` with four direct passes and 13 direct misses; strictness/output-plane columns are disclosed.
8. `skinny/REDRESS.md` — accepted wins and rejected-route ledger; items 50-55 reject the failed SK-V5 UTF-8/direct-string family, item 56 records the admitted structural-scan floor fix, and item 57 records direct receiver/source-shape redress and full-matrix refresh.
9. `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE}.md` — live skinny/V1 amendment surfaces.
10. `docs/precepts/instructions/STYLE.md` and `docs/precepts/instructions/LESSONS-LEARNED.md` — voice, discipline, and failure-mode anatomy.

---

## §2 — Where the work has been

Single-round greenfield restart began 2026-05-04 after a compounded-contrivance archive of the prior restart. Waves landed through V9.4 and then through the SK-V3 grand synthesis (Wave 1, 2026-05-12 morning) and the SK-V3 profiling cohort (Wave 2, 2026-05-12 evening):

| Wave | Phases | Outcome |
|---|---|---|
| 1-7 | PASS-1/2/3 dispatch + SYNTHESIS trio + V1-V7.1 hardening | V7.1 READY (99% KEEP fraction) |
| 8 | Lenses I/J/K simplification audit + V8/V8.1 cycles | READY-WITH-NARROW-RESIDUE |
| Codex V9 | V9 + V9.1 verification + corpus amendments | V9.1 READY |
| Skinny SK-V1 | Five-quadrant skinny implementation spec + SK-V1 audit | SK-AMENDMENT-REQUIRED-NARROW |
| Skinny implementation + user iteration | Runnable prototype at `skinny/`; three false routes (dispatch-table, 12-byte token, pair-fusion) measured-and-rejected | Empirical eager-tape ceiling |
| SK-V2 audit + amendment cohort | 5-target SK-V2 + 6th-agent lazy-tape proposal + 5 amendment agents | SK-V2 closed at amendment level; the old design memo is purged in SK-V6 |
| V9.2 V1-corpus audit | 4-target V9.2 hardening against the lazy-tape proposal | AMENDMENT-REQUIRED-NARROW (CONDITIONAL) |
| Skinny v2 lazy-mode implementation | Lazy-offset tape implementation (~860 LOC); re-benched | Intermediate outcome G; not final disposition |
| Skinny triad gate | Lazy-offset tape plus sparse flags, direct spare-capacity offset writes, cold errors, SWAR digit/plain-string runs, delimiter fusion, newline-indent space skipper, `parse_value_at`, short plain-string fast path, Track 2 inline parity | Original twitter / citm_catalog / canada gate passed; substrate/direct projection validated for the narrow skinny triad |
| Skinny expanded gate | The same implementation measured across 17 rows, including Unicode and shape-stress corpora | Current full report has 13 parse/tape G rows and four A / GO retained rows (`canada`, `mesh`, `marine_ik`, `numbers`); Canada scan floor is green at 69075 Mbps; overall gate remains `N-direct / NoGo` because direct throughput is red on 13 rows |
| V9.3 six-agent comparative-profile cohort 2026-05-12 | samply skinny + sonic-rs + simdjson; asm dump; DAVID/asmjson research; handwritten ASM catalog; SIMD intrinsics catalog | Architectural lever identified: codegen template shape, not substrate |
| V9.4 grand-synthesis cohort 2026-05-12 | 6 research agents (dav1d/ffmpeg/VLC ASM lift, deadweight-intrinsic re-examination, tape-union design, JSON corpora + Unicode torture tests, parse-that audit, greater-arch generalization) + 6 profile agents (skinny expanded 14-corpus, sonic-rs expanded 9-corpus × Value/LazyValue × inlined/noinline, simdjson expanded 13-corpus stage-decomposition, yyjson 7-corpus no-SIMD reference, RapidJSON + serde_json floor, cargo-asm string/Unicode paths) | V9.4 READY — comprehensive spec amendment landed without new directives, without new BIR variants, without contrivances |
| **SK-V3 Wave 1 research cohort (2026-05-12 morning, 6 agents)** | (a) asmjson deep dive — only 6× `vpcmpeqb` + 10× `kmovq` + 2× `vpcmpub` + 6× `korq` + 2× `vmovdqu8` + 18× `tzcnt`; no esoterica in the asmjson kernel itself; (b) dav1d/FFmpeg ASM patterns — `x86inc.asm` macro corpus vendored at `crates/bbnf-simd/ext/x86/`; per-target file naming `<family>[16]_<isa>.{asm\|S}`; nasm-rs + cc-rs build wiring; msac cross-chunk refill is the one transferable algorithmic insight beyond simdjson/sonic-rs/yyjson; (c) AVX-512 esoterica beyond catalog — 5-pack added to Lock 16; (d) M5 Max NEON + Apple esoterica — 3-pack added to Lock 16; (e) `parse_value_at` structural analysis; (f) 5-shape `BackendShape` concretization. | Findings are folded into the live SK-V5/SK-V6 authority set; the old SK-V3 packet files are purged in SK-V6 Wave 0. |
| **SK-V3 Wave 2 profiling cohort (2026-05-12 evening, 6 agents)** | (a) native sidecars (`skinny/profile/native-sidecars/`) — skinny v3 ALREADY beats yyjson + simdjson C++ on 4 of 17 corpora on M5 Max: citm +43% vs yyjson, canada +22% vs simdjson, mesh +6% vs simdjson, unicode_mixed +10% vs simdjson; (b) multi-corpus asm pathology (`skinny/profile/wave2-asm/`) — two-pathology-class diagnosis: `tiny_string_loop` dominates github_events/update-center/random (~32% self-time in `match_tiny_plain_string` scalar loop at PCs `0x2734`/`0x3158`); `hex_decode` dominates unicode_escapes/y_string_unicode (~70% self-time in inlined `unescape_json_string` hex normalisation); no single fix closes all five; (c) PMU profile (`skinny/profile/wave2-pmu/`) — `parse_value_at` is a single 7304-byte hot function (RVA `0x2460..0x40e8`, 1826 mnemonics) under workspace `lto=thin codegen-units=1 debug=true` — i-cache budget per Lock 15 is already met; the `match_tiny_plain_string` scalar loop is duplicated at PCs `0x2734` (key) and `0x3158` (value) via inliner duplication, which is the intended LTO-fat shape; (d) checkasm harness (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md`) — caught `escape_mask_64` NEON correctness bug on first run; minimal adversarial repro `xorshift seed 0xCAFEF00DBAADF00D, iter 0, 128-byte JSON-pool buffer`; root cause is state-handoff confusion between `escape_mask_64`'s `new_carry` and `scan_json_tail`'s `escaped` arg; (e) capacity-plan probes (`skinny/profile/wave2-capacity/CAPACITY-REPORT.md`) — Plan D (`Vec::with_capacity(256)` + geometric grow) wins +4.8% on `random`, +10.2% on `github_events`, with 23-64% capacity reclamation; sampled heuristic over-reserves by 2.53× and is overfit to `update-center`'s 4 KiB prefix; Plan D lands at 1.87×; pre-scan cost (1.4% self-time on Plan A) disappears; (f) eventcursor prototype (`skinny/profile/wave2-prototype/`) — refuted: 0.18-0.22 c/B projection regressed 22-37% across six corpora at the implementation level; the prototype path duplicates whitespace-bitmap allocation before parse without proportionate hot-loop savings. | Wave 2 re-baselines `skinny/RESULTS.md`; Plan D promoted to production default at packet §4 P0.1; `escape_mask_64` fix blocks all SOTA-BEAT bench claims per packet §4 P0.2. |
| **SK-V5 audit cohort (2026-05-13, 15 agents)** | 9 deep cohort reports (A1 comparative, A2 dav1d-process, A3 parse-that-gaps, A4 tape-union-audit, A5 grammar-generalization, A6 research-ledger, B1 parse-attribution, B2 direct-attribution, B3 native-sidecars) + 6 novelty-challenge reports (D1 eisel-novelty, D2 utf8-novelty, D3 derive-shape-novelty, D4 simd-split-novelty, D5 sinkonly-novelty, D6 class-ab-novelty). 5,559 LOC under `restart/skinny/tranches/sk-v5/research/`. Findings: (a) Class A `match_tiny_plain_string` was wrong-layer; (b) the UTF-8 hot-boundary diagnosis required implementation evidence and SK-V6 now refutes the Wave 3 close; (c) Eisel-Lemire was vendorable and is now wired; (d) Track 1/Track 2 bench-private dishonesty was real and is now corrected; (e) codegen was decorative and `codegen/src/lower/` now exists; (f) 5-shape BackendShape Rust state is now landed; (g) strictness disclosure is now in `RESULTS.md`; (h) only consumed `bbnf.asm` primitive bodies are admitted; (i) the fossil `simd-scan` crate is now purged. | SK-V5 grand synthesis, packet, nuke plan, and handoff remain substrate-history authority; SK-V6 prompt is current dispatch authority. |

Cumulative commit count: ~95+ across all cycles.

---

## §3 — Current state (post-SK-V5 audit cohort)

**Current operating verdict: `skinny/RESULTS.md` is the measured authority for the runnable skinny.** It records full-gate **`N-direct / NoGo`**. The original triad still passes as historical substrate evidence. The checked-in expanded parse/tape plane has 13 hard G rows and four A / GO guard rows: `canada`, `mesh`, `marine_ik`, and `numbers`. Canada structural scan is no longer the report-level blocker: the full matrix reports 69075 Mbps against the 40000 Mbps NEON floor. The direct workload is correctness-green with generated Track 1 `SinkOnly` and structurally different hand Track 2; `citm_catalog`, `apache_builds`, `github_events`, and `instruments` pass the 1.10 sonic-rs time slack, while 13 rows remain the binding direct-typed-emission block. Strictness and output-plane columns are disclosed.

Lock 1 stands verbatim with the tape-union clarification: the structural projection is the tape, not a sidecar. Locks 15 + 16 carry their 2026-05-12 amendments AND the Wave 1 strict additions (5-pack AVX-512 + 3-pack NEON). No new BBNF directives. No new BIR variants. SK-V5 filled in prior-declared Rust state; SK-V6 owns the current re-profile and throughput-recovery line.

The SK-V5 close criterion is the expanded SOTA-BEAT gate, not the historical triad. The close condition is fixed in §6 below and at `HANDOFF-SK-V5.md` lines 132-147.

**What SK-V5 established (the five corrected diagnoses, all empirical):**

1. **Class A `match_tiny_plain_string` is wrong-layer, and the later UTF-8 fusion prescription is now refuted as a close.** The Class A kernel was previously wired, regressed twitter ~25%, and was reverted per `skinny/REDRESS.md:301-313`. SK-V5 then hypothesized that folding UTF-8 validation into the NEON 16-byte string-body scan would close parse-G; REDRESS 50-55 show the prescription did not survive measurement on the generated Track 1 baseline. SK-V6 therefore reopens the diagnosis through fresh `parse-attribution` profiles rather than carrying this kernel forward.

2. **Number lever is vendor-and-wire, not research.** Eisel-Lemire is fully implemented, bit-parity-tested, and consumed in production at `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`; `mod.rs:147` exposes `compute_f64(i64, u64, bool) -> Option<f64>` with full Clinger fast-path + Eisel-Lemire slow path + ambiguous-rounding `None` sentinel. Vendor into skinny `parse-that-regex/src/number/`. The integer materializer is real and correct but misplaced at `skinny/crates/bbnf-bench/src/direct_struct.rs:501-528` (`parse_integer_digest` with proper `i64::MIN` handling); move to `parse-that-regex/src/number/integer.rs`. This closes the numbers/canada/mesh/marine_ik direct rows (D1).

3. **Bench-private SinkParser dishonesty was real and is now corrected.** Both `track1_digest` and `track2_digest` previously called the same private `sink_only_digest`. SK-V5 Wave 2 rewired Track 1 to generated runtime and made Track 2 structurally different, so the remaining `N-direct` result is a real generated-runtime/materialization gap.

4. **Codegen was decorative and is now corrected for generated direct.** The previous `crates/codegen/src/lib.rs` discarded `&BackendIr` and spliced static JSON templates. SK-V5 Wave 1 created `codegen/src/lower/`; later redress renders generated `SinkOnly` from a BIR-derived `SinkOnlyProgram`. Retained parser/view scaffolding still carries template history, but the direct gate is no longer a codegen-attribution gap.

5. **5-shape BackendShape Rust state exists.** SK-V5 Wave 1 landed the `BackendShape` enum, `LayoutFacts.backend_shape`, `derive_backend_shape`, and the `codegen/src/lower/` hierarchy. Remaining work is measured selection/materialization quality and Lock 14 cleanup, not symbol absence.

**Historical findings from the Wave 2 cohort** (still load-bearing for the spec, all measured 2026-05-12 evening; preserved here since they predate SK-V5 but remain materially correct):

1. **Plan D (grow-only capacity) wins on the cross-corpus probe.** Plan A (sampled heuristic) is overfit to `update-center`'s 4 KiB prefix and over-reserves by 2.53×; Plan D's geometric grow lands at 1.87×, beats Plan A by +4.8% on `random` and +10.2% on `github_events`, and eliminates the 1.4% pre-scan self-time. Promotion is the SK-V3 §4 P0.1 preflight. Source: `skinny/profile/wave2-capacity/CAPACITY-REPORT.md` §3.

2. **`escape_mask_64` NEON has a correctness divergence vs the scalar tail on adversarial inputs.** Synthetic xorshift inputs with backslashes immediately before stripe boundaries land on the divergence; real-world corpora never trigger it, which is why expanded-gate parity held. The fix is either consume the trailing 16-byte 4×chunk under the SIMD branch (so the tail never sees mid-escape state) or translate `bs_carry → escaped` correctly by walking the residual mask. This blocks all SOTA-BEAT bench claims per packet §4 P0.2. Source: `skinny/crates/bbnf-simd/CHECKASM-REPORT.md` §d.

3. **i-cache budget per Lock 15 is already met.** `parse_value_at` is a single 7304-byte (~7.1 KiB) hot function under workspace `lto=thin codegen-units=1 debug=true` — well within the Lock 15 ~20 KiB ceiling. The `match_tiny_plain_string` scalar loop duplication at PCs `0x2734` and `0x3158` is intended LTO-fat shape and precondition for the single-source NEON kernel to land both call sites at once. Source: `skinny/profile/wave2-asm/PROFILE-REPORT.md` Appendix C + `skinny/profile/wave2-pmu/PMU-REPORT.md` §1.

4. **The Wave 2 pathology classes remain diagnostic, not sufficient.** The Class A tiny-string and Class B `\uXXXX` kernels have strict checkasm coverage, but the current gate proves primitive admission is not enough: generated parser overhead, event/tape consumption, and generated `SinkOnly` exact float/string/Unicode materialization remain blockers. Source: `skinny/RESULTS.md`, SK-V5 cohort B reports, and `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md` §1.

5. **Eventcursor prototype refuted at the V9.4 baseline.** A mask-driven whitespace LUT prototype on top of an unchanged `parse_value_at` is +40% instructions and +35% branch instructions in the hot function. The eventcursor lift is real, but it must land **as a lowerer-template change** that consumes the existing tape projection, not as a sidecar mask producer in front of unchanged source-byte dispatch. Source: `skinny/profile/wave2-prototype/PROTOTYPE-REPORT.md` §verdict.

6. **Native sidecar cross-comparator landscape on M5 Max:** skinny v3 already leads citm/canada/mesh/unicode_mixed against simdjson C++ + yyjson; remaining losses concentrate on small object-heavy ASCII (apache_builds/github_events/update_center, where simdjson stage1 amortisation has the most to give) and string-decode-heavy Unicode (random/unicode_escapes/y_string_unicode). asmjson's 10.93 GiB/s anchor is x86_64 AVX-512 only; on M5 Max its native arm64 SWAR path measures 3315/2447/669 MiB/s — comparable to skinny v3, which beats the asmjson mixed-corpus row by 167%. Source: `skinny/profile/native-sidecars/PROFILE-REPORT.md` §a-g.

**What has settled** (do not relitigate):
- 16 architectural locks post-2026-05-12 (Locks 1-14 + Locks 15-16, with Wave 1 strict-additive amendments to Lock 16).
- `LayoutFacts.backend_shape` cost-model derivation per Lock 10 auto-detect, with 5 concrete shapes and an 8-step derivation algorithm (no new directive).
- 20-variant BIR alphabet (no new variant; `Alt { Dispatch }` lowers to multiple access patterns).
- Tape ∪ structural-projection union as Lock 1 clarification (not amendment).
- 12-agent comparative-profile cohort (V9.4 6+6) + 12-agent SK-V3 cohort (Wave 1 6 + Wave 2 6) empirically anchored.
- Lazy-offset tape with sparse flags is the measured JSON skinny substrate for the historical triad; the refreshed full matrix clears the Canada structural scan floor; expanded SOTA-BEAT remains blocked on retained G rows plus `N-direct`.
- Rejected routes remain non-canonical: dispatch-table/function-pointer alternates, 12-byte/width churn, pair-fusion, structural-index typed-parser prepass, NEON no-escape matcher, separator elision, generic SWAR whitespace skipper, sidecar mask-producer prototypes, Plan A sampled-heuristic capacity, Plan B exact-pre-scan, Plan C oneshot-SIMD pre-scan.
- Mbps unit calibration: skinny formula `bytes * 8000 / ns` = megabits/s; multiply by 0.119 to get MiB/s for cross-parser comparison.

---

## §4 — Cross-parser landscape on M5 Max

Throughput is **MiB/s** (binary mebibytes/sec). Conversion: `skinny Mbps × 0.1192 = MiB/s`. All measurements 2026-05-12 unless noted. Source: `skinny/profile/native-sidecars/PROFILE-REPORT.md` §a.

| Corpus | skinny v3 (MiB/s) | yyjson (MiB/s) | simdjson C++ (MiB/s) | sonic-rs (MiB/s) | asmjson SWAR (M5 Max native, MiB/s) | asmjson Zen 4 AVX-512 anchor (MiB/s) | sidecar comparator posture |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | :---: |
| twitter          | **2631** | 3687 | 2923 | 2415 | 3315† | 11192‡ | A / GO |
| citm             | **3571** | 2498 | 4270 | — | 2447† | 11192‡ | A / GO |
| canada           | **1675** | 1550 | 1370 | — | — | — | A / GO |
| apache_builds    | 1850 | 1940 | **4293** | — | — | — | A / GO |
| github_events    | 2267 | 2554 | **4725** | — | — | — | G / NO-GO |
| update_center    | 1763 | 2210 | **3647** | — | — | — | G / NO-GO |
| mesh             | **1194** | — | 1122 | — | 669† | — | A / GO |
| random           | 1117 | — | **2460** | — | 669† | — | G / NO-GO |
| gsoc-2018        | 3521 | — | — | — | — | — | C / GO |
| marine_ik        | 1076 | — | — | — | — | — | A / GO |
| instruments      | 1920 | — | — | — | — | — | A / GO |
| numbers          | 1942 | — | — | — | — | — | A / GO |
| distinct_values  | 1927 | — | 2721 | — | — | — | C / GO |
| unicode_basic    | 1731 | — | 1940 | — | — | — | C / GO |
| unicode_mixed    | **1719** | 1228 | 1568 | — | — | — | C / GO |
| unicode_escapes  | 587 | — | **672** | — | — | — | G / NO-GO |
| y_string_unicode | 865 | — | **1624** | — | — | — | G / NO-GO |

† asmjson M5 Max native SWAR measured against asmjson-shipped synthetic corpora (`string_array`/`string_object`/`mixed`), not the 14-corpus skinny set; rows shown are nearest-shape analogues. The AVX-512 path does not compile on M5 Max. Source: `skinny/profile/native-sidecars/asmjson/NOTE.md`.

‡ asmjson published anchor 10.93 GiB/s = 11192 MiB/s on Zen 4 AVX-512 DOM; cross-architecture target, not an M5 Max measurement. Source: `https://docs.rs/asmjson/`.

The final column is the sidecar profile posture recorded in that comparator
pass, not the current `skinny/RESULTS.md` gate verdict. Current authority is
still §3: parse-G rows are `twitter`, `random`, `unicode_mixed`, and
`unicode_basic`, and the full gate is `N-direct / NoGo`.

**Bold cells are corpora where skinny v3 leads the comparator. Skinny v3 already wins on 4 of 17 corpora against simdjson C++ / yyjson on M5 Max** — citm (+43% vs yyjson), canada (+22% vs simdjson + 8% vs yyjson), mesh (+6% vs simdjson), unicode_mixed (+10% vs simdjson). On twitter, skinny is +9% vs sonic-rs and −10% vs simdjson C++; the remaining gap to simdjson on twitter is the cost of staying in safe Rust with lazy-decode shape.

The **asmjson 10.93 GiB/s SOTA-BEAT anchor is architecturally x86_64 AVX-512 only**; the algorithmic intent it represents is already within reach on M5 Max when arm64 NEON lands on the classifier critical path. Note from `native-sidecars/PROFILE-REPORT.md` §g: native asmjson SWAR on M5 Max measures 1.03 c/B on string_array, which is essentially where skinny v3 already sits on its best corpora (twitter 1.30 c/B, citm 0.96 c/B).

---

## §5 — The greater-arch generalization

The user mandate: the skinny spec is a **subset** of the greater V1 spec, and the feedback loop folds back. The 5-shape `BackendShape` is not JSON-specific; it applies to **every V1 grammar** (Lock 14 — full grammar generalisation; zero overfitting).

The 5 shapes (`restart/ARCHITECTURE.md` §7.3 + Wave 1 agent #114):

```rust
pub enum BackendShape {
    EagerTape,       // Alt { Dispatch } reads source[pos]; default; selected for @error(recover) / @host decoded-at-parse / @layout / first-set overlap
    OffsetTape,      // Alt { Dispatch } reads source[offsets[cursor]]; typed event cursor over retained offsets; lazy scalar spans
    EventTape,       // typed event cursor over event cells with stored payload classes or recovery/layout side facts
    SinkOnly,        // direct-to-struct sink; retains no queryable document identity
    CollapsedStage,  // AVX-512-class FSM with mask-held parser state; CPUID/cost-model gated; strict single-pass mask/state walk
}
```

The 8-step derivation algorithm at `passes::recognizers::derive_backend_shape(grammar_ir, rule_id) -> BackendShape` (no new directives; all inputs are existing Grammar IR facts):

1. If transitive uses include any `ErrorDirective` ⇒ `EagerTape`
2. Else if rule body contains `Call { kind: Host }` decoded-at-parse ⇒ `EagerTape`
3. Else if rule body contains `LayoutDirective` ⇒ `EagerTape`
4. Else if rule's `Alt` first-set has overlap ⇒ `EagerTape` (lowers `Alt` as `Speculative`, not `Dispatch`)
5. Else if the public output mode is direct-only and no post-parse path/value traversal is required ⇒ `SinkOnly`
6. Else if target features admit AND rule is a hub with ≥ 4 byte-disjoint arms ⇒ `CollapsedStage`
7. Else if payload/recovery/layout side facts must be retained per cursor ⇒ `EventTape`
8. Else ⇒ `OffsetTape`

**Per-grammar matrix** (Wave 1 agent #114 derivation against V1 grammar fleet at `restart/grammars/` + `crates/<grammar>/`):

| Grammar | Rule | Backend shape | Rationale |
|---|---|---|---|
| **JSON** | `value` | `OffsetTape` | byte-disjoint dispatch alphabet ({`{`, `[`, `"`, `t`, `f`, `n`, `-`, digit}); lazy scalar spans; no recovery; no layout |
| **JSON** | `string` (skinny) | `OffsetTape` | quote-bounded; HAS_ESC flag captured at scan; raw span emitted to tape; decode is view-time |
| **JSON** | `string` (V1) | `EagerTape` | V1 default if `@host fn decode` activates at parse; reverts to `OffsetTape` under skinny opt-in |
| **CSS L4** | `ruleItem` | `OffsetTape` | byte-disjoint dispatch ({`@`, ident-start, `.`, `#`, `*`}); no recovery on the hot ruleItem dispatch hub |
| **CSS L4** | `value` | `EagerTape` | dimension/percentage/function-call distinction needs source-byte lookahead inside the type-system arm |
| **CSS L4** | `declaration` | `EventTape` | payload class (property name → typed-property enum) MUST be retained for visitor traversal |
| **CSS L4** | `selector` | `EagerTape` | complex/compound/simple selector boundary needs source-byte lookahead; recovery scope |
| **BBNF-self** | `grammar` / `declaration` / `term` | `OffsetTape` | byte-disjoint top-level dispatch ({`@`, ident, `(`}); lazy span over rule body |
| **BBNF-self** | `expression` | `EagerTape` | Pratt operator chain requires precedence climbing on source bytes; auto-detected per Lock 10 |
| **BBNF-self** | `directive` | `EventTape` | payload class (directive kind → enum variant) carries through to LayoutFacts consumption |
| **Sheets** | `formula` / `cellRef` / `primary` | `OffsetTape` | A1-notation byte-disjoint dispatch; lazy span over reference body |
| **Sheets** | `function` / `arrayLiteral` | `EventTape` | function-name payload (`LET(`/`LAMBDA(` prefix-DFA discriminator) must reach the typed AST |
| **Sheets** | `expression` | `EagerTape` | infix-operator precedence requires source-byte lookahead |

**Implications for the codegen lowerer** (per Lock 10 auto-detect + Lock 5 IR-contract):

- The codegen template at `crates/codegen/src/lower/rust.rs` emits **typed event cursor consumption** for `OffsetTape`/`EventTape`, **direct field writes** for `SinkOnly`, **byte-position fallback** for `EagerTape`, **CPUID-gated FSM** for `CollapsedStage`.
- No new BIR variant; no new BBNF directive; no new grammar surface.
- The `Alt { Dispatch }` BIR variant's payload extends with `DispatchByteSet` to admit prefix-DFA discriminators for multi-byte tokens (CSS `@-rules`, Sheets `LET(`/`LAMBDA(`) — this is a payload extension to the existing variant, not a new variant.
- The skinny SOTA-BEAT close on JSON `value`+`string` validates the `OffsetTape` lowering pattern; the same pattern lowers CSS `ruleItem`, BBNF-self `grammar`/`declaration`/`term`, and Sheets `formula`/`cellRef`/`primary` without per-grammar code in any generic crate.

**Cross-grammar feedback loop**: a NEON kernel that closes JSON `match_tiny_plain_string` IS the kernel that closes CSS L4 ident-token scan, BBNF-self ident-token scan, and Sheets cell-reference scan. The `bbnf-simd` primitive crate exposes grammar-neutral `StringMode`/`NumberScan`/`ByteClassPlan`/`KernelSet` per the Wave 1 parse-that audit. Grammar names appear only in generated runtime modules; the primitive crate carries ZERO grammar-specific code per Lock 14.

---

## §6 — Wave dispatch posture (SK-V6)

SK-V6 supersedes SK-V5 dispatch. SK-V5 remains implementation history:
Waves 0-5 landed the Rust substrate, generated `SinkOnly`, strictness
columns, consumed primitives, and scan-floor redress; Wave 3's UTF-8 fusion
close is refuted. The current order is SK-V6 Wave 0 purge/fold-back, Wave 1
fresh profile research, then one-intervention-at-a-time redress with
same-row falsifiability gates.

| Step | Wave | Scope | Owner paths | Exit gate | Source |
|---|---|---|---|---|---|
| 1 | Wave 0 | Strictness columns + diagnostic infra + nuke decisions | `skinny/RESULTS.md`, `runtime/Cargo.toml`, `runtime/src/grammars/json/generated.rs`, `NUKE-PLAN-SK-V5.md` | Strictness disclosed honestly; `parse-attribution` feature build green; nuke targets enumerated | SK-V5 packet §2 |
| 2 | Wave 1 | Substrate authoring: BackendShape enum + LayoutFacts.backend_shape field + derive_backend_shape + codegen/src/lower/ hierarchy | `ir/src/`, `passes/src/`, `codegen/src/lib.rs`, `codegen/src/lower/` | Substrate plumbing complete; codegen no longer discards `&BackendIr`; regression-free transition | SK-V5 packet §3 |
| 3 | Wave 2 | Number lever + generated SinkOnly + bench rewire + bench-private SinkParser nuke | `parse-that-regex/src/number/`, `codegen/src/lower/sink_only.rs`, `runtime/src/grammars/json/sink.rs`, `bbnf-bench/src/direct_struct.rs` | Track 1 calls generated runtime; Track 2 is structurally different; direct rows now pass for `citm_catalog`, `apache_builds`, `github_events`, and `instruments`; 13 direct rows remain red | SK-V5 packet §4 |
| 4 | Wave 3 | UTF-8 fusion + Class B `_x4` batched + utf8_block module | `parse-that-regex/src/lib.rs:331-339`, `parse-that-regex/src/unicode/`, `bbnf-simd/src/aarch64/utf8/`, `bbnf-simd/src/aarch64/unescape_uxxxx.rs` | source hooks admitted; UTF-8-fusion close refuted by REDRESS 50-55; parse-G and string-bound direct rows remain open | SK-V5 packet §5, superseded by SK-V6 |
| 5 | Wave 4 | Lock 14 remediation + working-tree nukes | `bbnf-simd/src/lib.rs`, `bbnf-simd/src/aarch64/*`, `bbnf-simd/src/x86_64/*`, `runtime/grammars/json/`, `simd-scan/`, `runtime/.../generated_eventcursor.rs`, `runtime/Cargo.toml` | Lock 14 audit clean; 7 grammar-neutral split items land | SK-V5 packet §6 |
| 6 | Wave 5 | Consumed primitive admission + checkasm hardening + runtime dispatch table | `bbnf-simd/src/{scalar,aarch64}/`, `bbnf-simd/tests/`, `bbnf-simd/src/dispatch.rs`, `runtime/grammars/json/scan.rs`, `xtask` | admitted primitives have scalar references, checkasm parity, and hot consumers; item 56 adds bulk position emit + structural/terminator classifier and clears Canada scan in the full matrix; orphan macro bodies stay blocked | SK-V5 packet §7 |
| 7 | Wave 6 | Strict workload matrix | `bbnf-bench/`, `RESULTS.md`, `restart/skinny/BENCH.md` | 17 corpora × 7 workloads × N sidecars with strictness disclosed; no parse-G, no N-direct | SK-V5 packet §8 |
| 8 | Wave 7 (optional) | x86 CollapsedStage successor | `bbnf-simd/src/x86_64/*.asm`, `runtime/grammars/json/json_collapsed.asm`, `codegen/src/grammars/json/tables.rs` | Gated on Zen 4 silicon + NASM author + checkasm-green Layer 1; otherwise `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` fallback | SK-V5 packet §9 |

**Implementation discipline**: Wave 1 profiling is mandatory before any new
kernel or substrate intervention. The M5 Max close condition has not fired:
parse-G rows and `N-direct` rows remain. Further implementation must remove
named rows or record falsified candidates in REDRESS. Wave 7 remains the x86
successor and cannot be used to close the M5 Max SK-V6 gate.

---

## §6a — Close condition (SK-V5)

Per `HANDOFF-SK-V5.md` lines 132-147 the SK-V5 close requires ALL of:

1. `skinny/RESULTS.md` has zero parse-G rows.
2. `skinny/RESULTS.md` has zero N-direct rows.
3. Strictness column disclosed honestly on every row (`Strictness | parse_utf8 | escape_complete | flaw_probe`).
4. Track 1 calls generated runtime (verified via `samply` symbol path; not `sink_only_digest`).
5. Track 2 is structurally different from Track 1 (different symbol path; not the same SinkParser).
6. `parse_value_at` no longer collapses to one symbol; PC-level attribution under the `parse-attribution` feature explains any remaining gap.
7. `cargo run -p xtask --release -- primitive-checkasm` passes including register-clobber detection.
8. Lock 1 + Lock 14 audit clean (manual grep + cohort verification; no grammar names in generic crates).
9. Sidecar comparator table records sonic-rs `Value` / typed direct, simdjson C++ DOM / On Demand, yyjson inlined DOM, asmjson SWAR strict/permissive, serde_json — all with API and output plane named.

SK-V5 does NOT promise beating asmjson on x86 AVX-512 hardware; that is
Wave 7, gated separately. SK-V5 does NOT introduce new directives, new
BIR variants, new locks, or new substrates. SK-V5 does NOT promise CSS L4
/ Sheets / BBNF-self closure; Wave 4 ensures generic crates STAY generic
so those grammars can land in subsequent tranches without further
architectural debt.

---

## §7 — Verification rituals

Before any wave dispatch:

```bash
git log --oneline -10
git status --short
```

Commit head should reflect the SK-V5 audit cohort + master synthesis docs. Per-target verification:

```bash
# SK-V5 authority docs in place (the receiver packet)
head -30 restart/skinny/tranches/sk-v5/SYNTHESIS.md
head -30 restart/skinny/tranches/sk-v5/SPEC.md
head -30 restart/skinny/tranches/sk-v5/NUKE-PLAN.md
head -30 restart/skinny/tranches/sk-v5/HANDOFF.md

# SK-V5 cohort artefacts present (15 reports)
ls restart/skinny/tranches/sk-v5/research/ | wc -l  # 15
ls restart/skinny/tranches/sk-v5/research/skv5-{A1,A2,A3,A4,A5,A6,B1,B2,B3,D1,D2,D3,D4,D5,D6}-*.md

# Wave 1 + Wave 2 historical cohort artefacts present
ls skinny/profile/{native-sidecars,wave2-asm,wave2-pmu,wave2-capacity,wave2-prototype}/PROFILE-REPORT.md
ls skinny/profile/native-sidecars/{yyjson,simdjson-cpp,asmjson}/

# SK-V6 purge gate: obsolete SK-V3/SK-V4 and SK-V1/SK-V2 hardening docs absent
rg -l "IMPLEMENTATION-PACKET-SK-V3\|HARDENING-.*-SK-V1\|LAZY-TAPE-DESIGN" restart/  # no output

# Locks 15-16 + Wave 1 extensions
grep -c "^15\.\|^16\." restart/locks/LOCKS.md  # ≥ 2 (Locks 15, 16)
grep -E "LD4-interleaved|BCAX|EOR3|svmatch_u8|kreg-facts|VPCLMULQDQ|vpmadd52|vpdpbusd|vpshufbitqmb" restart/locks/LOCKS.md | wc -l  # ≥ 5 Wave-1 additions to Lock 16

# 5 diagnostic codes
grep -E "BBNF-BACKEND-SHAPE-INCONSISTENT|BBNF-FORCE-INLINE-MISSED|BBNF-ICACHE-BUDGET-EXCEEDED|BBNF-UTF8-INVALID-AT-PARSE|BBNF-UNICODE-NONCHAR-CODEPOINT" restart/ARCHITECTURE.md | wc -l  # 5

# Tape-union clarification + LayoutFacts.backend_shape 5-shape
grep -A 5 "backend_shape" restart/ARCHITECTURE.md | head -25
grep -E "EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage" restart/ARCHITECTURE.md | wc -l  # ≥ 5
grep -A 3 "tape ≡ structural projection union\|structural projection IS the tape" restart/skinny/SUBSTRATE.md | head -10

# Corpus expansion + correctness gates
grep -c "twitterescaped\|unicode_mixed\|unicode_escapes\|marine_ik" restart/skinny/BENCH.md  # ≥ 4
grep "Gate [1-4]" restart/skinny/BENCH.md | head -8

# Lock 1 NOT amended (verbatim)
rg -n 'tape_mode|lazy-mode|dual-mode' restart/locks/LOCKS.md  # 0 matches
rg -n 'structural projection IS the tape' restart/skinny/SUBSTRATE.md  # ≥ 1

# Wave 0 preflight gates (run before any Wave 1/2 dispatch)
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity  # zero divergences
cargo run -p xtask --release -- bench-json --capacity-plan D --corpus update-center
cargo build --release -v 2>&1 | grep -E -- '-C lto=(fat|true)|-C codegen-units=1' | wc -l  # ≥ 1 per member

# SK-V5 corrected-diagnosis verification (the five empirical findings)
# (1) Class A kernel was wired and reverted — REDRESS lines 301-313
sed -n '301,313p' skinny/REDRESS.md
# (2) Eisel-Lemire production source vendorable
ls /Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/mod.rs
# (3) Bench-private dishonesty — both tracks call sink_only_digest
grep -n 'sink_only_digest' skinny/crates/bbnf-bench/src/direct_struct.rs
# (4) Codegen is decorative — `let _ = backend;`
grep -n 'let _ = backend' skinny/crates/codegen/src/lib.rs
# (5) BackendShape Rust state absent
grep -rn 'BackendShape\b' skinny/crates/ | grep -v '\.asm'  # zero Rust hits expected
grep -n 'shapes_for_json' skinny/crates/passes/src/lib.rs  # hardcoded selector
```

Per-corpus reproduction:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- gate-json
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o profile/reprofile-sk-v3/random.event_cursor.profile.json.gz \
  -- ./target/release/profile-lazy 50000 test_data/random.json
```

**Checkasm harness gate** (new, Wave 2 addition): every SIMD kernel in `crates/bbnf-simd/` carries a unit-parity test against the scalar reference and a corpus-parity test against the expanded skinny corpus. The harness mode `BBNF_SIMD_STRICT=1` runs adversarial xorshift seeds against every NEON kernel boundary; this is the gate that caught `escape_mask_64`. Per Lock 16 verification: every `core::arch::*` use-site and every `asm!` block traces to a citation in the Lock 16 allowlist.

---

## §8 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage; never cite "the prompt said" or "the user asked". Path:line citations on every concrete claim. Per-X tables for "all grammars" / "all backends" / "all topics" claims. Receiver / blocker / receiving-gate triple on every carry. **No quick solutions. No legacy code uncontested. No contrivances. No new directives. No new BIR variants. No deferrals.** Lock 14 is the binding generalisation discipline — full grammar generalisation; zero overfitting. Lock 15 + Lock 16 are the 2026-05-12 disciplines for build-profile fusion + SIMD/ASM admissibility, with the Wave 1 strict-additive Lock 16 amendments (5-pack AVX-512 + 3-pack NEON) carrying citation-anchored architectural names.

---

## §9 — Closing posture

Hereupon the SK-V3 and SK-V4 grand syntheses are historical, not sufficient for dispatch. The SK-V5 cohort plus implementation redress ledger is the receiver authority. The skinny JSON triad is green as historical evidence; the checked-in expanded parse plane has 13 G rows and four retained A / GO rows (`canada`, `mesh`, `marine_ik`, `numbers`), while the direct workload is `N-direct / NoGo` with four passing rows (`citm_catalog`, `apache_builds`, `github_events`, `instruments`) and 13 misses. Waves 0-5 have landed enough Rust state to close strictness disclosure, BackendShape/codegen authority, generated `SinkOnly`, trusted UTF-8 source hooks, Lock 14 cleanup, consumed primitive admission, Canada scan-floor redress, and bounded direct receiver/source-shape redress. The next close is retained event/tape consumption and direct field-layout materialization that remove the remaining G and `N-direct` rows.

Skinny v3 ALREADY wins on 4 of 17 corpora vs simdjson C++ / yyjson on M5 Max (citm, canada, mesh, unicode_mixed) — measured, not projected. The AVX-512 esoterica stack landed at Lock 16 as **strict additions on top of the asmjson architecture** for the >SOTA path on x86_64: 5-pack (k-mask arithmetic family, VPCLMULQDQ at 512-bit lane, AVX-IFMA `vpmadd52`, AVX-512 VNNI `vpdpbusd`, AVX-512 BITALG `vpshufbitqmb`+`vpopcntb`) + 3-pack M5 Max NEON (LD4-interleaved, BCAX/EOR3, NEON-port `svmatch_u8`). All citation-anchored. All conditional on hardware. None violates Lock 14.

The 16 locks govern. The 5-shape `BackendShape` generalises across JSON/CSS L4/BBNF-self/Sheets without per-grammar code in any generic crate; SK-V5 Wave 1 has landed the Rust state that materializes that generalisation (`BackendShape` enum + `LayoutFacts.backend_shape` field + `derive_backend_shape` function + `codegen/src/lower/` hierarchy). V1 planning should carry the triad pass as substrate evidence, the item-56 scan-floor pattern as the grammar-neutral classify+bulk-emit primitive lesson, item 57 as direct receiver/source-shape evidence, the retained G set plus `N-direct / NoGo` as the current implementation block, the per-grammar `BackendShape` matrix as the lowerer-template contract, and `restart/skinny/tranches/sk-v5/HANDOFF.md` as the binding close condition.

**Continue from the current implementation branch; do not restart at Wave 0.**
