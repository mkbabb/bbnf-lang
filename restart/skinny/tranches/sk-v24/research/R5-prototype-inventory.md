# R5 — Prototype inventory & PROVE-01..14 startability map

Recon cartography for SK-V24. Read-only sweep. Every path below verified to exist
(`ls`/`rg`/`find`) at recon time. Two prototype cohorts: **sk-v23/prototypes/PROTO-PB-1..6+3b**
(2026-07-10, the SK-V23 proving pass) and **sk-v21/prototypes/PROTO-{A1-3,B1,B3,P1-P7}**
(2026-07-05, the SK-V21 convergence pass).

## THE LOAD-BEARING FACT: where code artifacts live now

**Every prototype's runnable code is GONE.** All scratch crates were `/private/tmp/*` (ephemeral,
wiped) or discarded worktrees. Confirmed absent: `/private/tmp/pb1`, `/private/tmp/pb3-scratch`,
`/private/tmp/pb3b-scratch`, `/private/tmp/pb6`, `tomlproto`, `p4-kernelcost`, `p5-kernel` — `ls
/private/tmp/pb*` → no matches. The transient bench bins are also gone from the tree: `fd
a1_adjudication|json_canon_bench|direct_struct|p6_hm_spike|p2_fused_cold` → **zero hits**.

**So for every asset, the code artifact = the markdown report's inline diffs/kernels ONLY.**
Re-authoring from the report is mandatory; nothing is `cp`-restorable. The reports are dense and
carry the decisive diffs verbatim (PB-1 NEON kernels, PB-6 both Mula/vshrn kernels, PB-3 full
surface, PB-4/P6/B3 type cores, PB-5 V8 diff, B1 regex interpreter), so "start from code" below
means "re-author from the report's inline spec," not "resume a crate."

### What DID survive (live substrates the provers build on)
- **parse-that repo**: `/Users/mkbabb/Programming/parse-that` (SEPARATE repo; non-clobber law —
  PROVE-09/10/11 scratch-copy only). PB-1's `detect_accel` basis is LIVE at
  `parse-that/rust/regex/src/automata/accel.rs`; PB-6's basis LIVE at
  `parse-that/rust/parse_that/src/parsers/scan/number_simd.rs`. `rg Truffle|find_at_accel accel.rs
  dfa.rs` → **empty** (PB-1's truffle/accel-consumer never landed — orphaned as PB-1 stated).
- **Live NEON kernels** (PROVE-01/04/05 productionize INTO these): `skinny/crates/bbnf-simd/src/aarch64/`
  — `movemask.rs`, `classify_tbl4.rs`, `structural_classify_64.rs`, `byte_class_from_eq_set_64.rs`,
  `string_block.rs`, `utf8/validate_block.rs`. NB `skinny/crates/simd-scan/src/` is an EMPTY stub;
  root `crates/simd-scan/src/` has `neon.rs` + `avx2.rs avx512.rs wasm.rs` (PB-2 x86-purge NEVER
  landed — the files it deleted still exist).
- **Live value-plane consumers** (PB-3/3b mirror these): `skinny/crates/runtime/src/tape/mod.rs`,
  `tape/cursor.rs` (the "no shared vocabulary" disclaimer PB-3 answers/PB-3b vindicates),
  `grammars/json/{view,value,sink}.rs`, `grammars/sheets/view.rs`.
- **Live emitter (PROVE-13 un-fork target)**: `skinny/crates/codegen/src/general_descent/{emit,view,mod}.rs`
  (B1's WALL A/B loci), `json_templates/` (P7's verbatim JSON splice).
- **Live cold harnesses**: `skinny/crates/bbnf-bench/src/bin/{css_canon_bench.rs, profile_direct.rs,
  gate.rs}` (PMU `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` at profile_direct.rs:44-45). The JSON
  canon harnesses are gone; CSS one survives for PROVE-05.

### Worktrees (task's `/Users/mkbabb/.claude/worktrees` DOES NOT EXIST)
Real worktrees live under `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/` plus sibling dirs
(`bbnf-lang-skv6-candidate{7,8,9,10}-base`, `bbnf-lang-skv15-profile-*`, `bbnf-skv16-p1`). Of the
prototype-referenced worktrees, only **`wf_55cb3d1f-cb8-11`** (PB-2's x86-purge tree) survives —
base **stale at d2b32f6e3** (pre-un-fork). `wf_bdd2b86b-8a6-6` (B3) and `wf_be85d0ab-073-12` (P6):
GONE. None hold recoverable prover code; treat all as dead.

---

## PROVE-01..14 startability map

Legend — **CODE**: re-author from a report's inline diffs (high leverage, verdict already GO).
**PARTIAL**: a prototype proves a sub-mechanism; the prover's headline construct is new.
**SCRATCH**: no prototype; from first principles (a prototype may still scope it).

| PROVE | Family | Start | Primary asset(s) | Measured result carried |
|---|---|---|---|---|
| 01 shufti/truffle prefilter, GATE=16 | F1 | **CODE** | PROTO-PB-1 | GO; skip-heavy **18–67×**, dense recovered to **0.95–1.15×** via fused GATE=16, exact parity (0 mismatches) |
| 02 Mula/Lemire digits → Eisel–Lemire | F1 | **CODE** | PROTO-PB-6 (+A1 D2/D4 the canada loss) | GO; **53.8% cyc/B** canada (**69.6%** long runs), exact parity 3.8M runs, 2.28–3.11× fewer ins/B |
| 03 SIMD unescape + fused UTF-8 valid. | F1 | **SCRATCH** | — (reuse PB-6/PB-1 NEON idioms; `utf8/validate_block.rs` live) | none |
| 04 multi-block 2×64B pipelined scan, vshrn | F1 | **PARTIAL** | PROTO-A2 D1, PROTO-P5, PROTO-P4 | vshrn movemask door **+35%** (crossing lever binds, 2.6× classify lever); pipelining itself unproven |
| 05 tbl CSS token-class kernel | F1 | **SCRATCH** | — (reuse P4 `vqtbl4q` LUT + KernelCostTable; `css_canon_bench.rs` live) | none for CSS token class |
| 06 lazy path API (get/stop-early) | F4 | **CODE** | PROTO-PB-3 (Lane 2+3) | YES; `View`+`Path` byte-identical to `json/view.rs to_canonical`, ~283 LOC neutral surface |
| 07 direct-to-struct filler | F4 | **CODE** | PROTO-PB-3 (Lane 4) + serde bridge; A1/P2 caution | YES; `FromView` materializes Tweet struct; fused SINK is the competitive lane |
| 08 HM+CSP over `->` → typed sigs | F2 | **CODE** | PROTO-PB-4 + PROTO-P6 + PROTO-B3 | arrow==proj **5==5 / 13==13**; 189-LOC HM core, span_default=0; B3 lands **22 Nu8→U8**, 12-line fix |
| 09 parse-that arena de-nightly | F5 | **SCRATCH** | PROTO-A3 D2 scopes it | smallbox[nightly] is SOLE enabler (`parse_that/Cargo.toml:97`); bbnf-regex path smallbox-free (count=0) |
| 10 lazy/hybrid DFA + prefilter tiers | F5 | **CODE→**scratch DFA | PROTO-PB-1 (prefilter) + parse-that DFA | PB-1 prefilter tiers proven; 4096-bail/hybrid new |
| 11 parse-that TS V8 uplift | F5 | **CODE** | PROTO-PB-5 | same-map true (context [3] not [3,0]), mapState retired, floor-neutral (±1.4%) |
| 12 emit-spine residualization | F3 | **SCRATCH** | PROTO-PB-4 (§6 const-eval order), P7 (emitter map) | none (PB-4 names the emit-order steps) |
| 13 CSV through one generator | F1/F3 | **PARTIAL** | PROTO-B1 (TOML 7/7) + P7 (un-fork breaks) + A2/P4 (\|S\|≤2 flip) | B1: generic ws-policy + regex interpreter round-trip; P7: 6 generator-defects enumerated; CSV = the N*=3 flip grammar |
| 14 tape unification decoded in TS | F4 | **SCRATCH** | PROTO-PB-3 (Rust layout contract); `crates/bbnf-path-ts` live | none |

---

## Per-asset detail (verdict · numbers · where it lives)

### sk-v23 cohort (PROTO-PB-*, 2026-07-10)

- **PROTO-PB-1** — NEON shufti/truffle DFA prefilter. **GO (mandatory GATE).** skip-heavy 18–67×
  across 4 real grammar patterns; **dense regresses 1.1–1.9× ungated, fully recovered to 0.95–1.15×
  by the fused GATE=16 self-loop trigger**; truffle tier carries the >8-exit patterns (ws, ident)
  shufti abandoned; exhaustive per-offset parity 0 mismatches. Code: report inline
  (`accel.rs` Truffle enum + `search_accel.rs` NEON kernels + `dfa.rs find_at_accel_gated`); scratch
  `/private/tmp/pb1` GONE; basis LIVE `parse-that/rust/regex/src/automata/accel.rs` (`detect_accel`
  orphaned) → **PROVE-01, feeds PROVE-10**.
- **PROTO-PB-2** — x86-purge build probe. **PROVEN** (regen 310→0 x86 tokens, 891 deletions zero
  insertions = aarch64 path byte-identical; build+parity green). **NEVER LANDED**: `crates/simd-scan/src/{avx2,avx512,wasm}.rs`
  still present. Worktree `wf_55cb3d1f-cb8-11` survives (stale d2b32f6e3). Prerequisite hygiene for all
  F1 provers (x86 arms still inherited) → gates **PROVE-01/02/03/04/05**.
- **PROTO-PB-3** — greenfield value-plane API. **YES.** One neutral surface (`NodeClass`+2-method
  `Classify`) drives JSON + a flag-plane grammar in one binary; 4 lanes (Sink/View/Path/FromView)
  byte-identical to `json/view.rs to_canonical`. ~283 LOC. Scratch GONE → **PROVE-06/07, layout for
  PROVE-14**.
- **PROTO-PB-3b** — sheets-lane. **SHEETS = CARRY.** Sheets tape has ZERO Enter/Leave events;
  `NodeClass` collapses `rule_kind` navigation (Identifier & Formula both → Raw, byte-divergent);
  parity recoverable only by re-importing the 39-kind ordinal. 13/13 parity checks. **Constraint on
  PROVE-06/07**: the fabric serves JSON + CSS-decl value-trees only; sheets keeps native sweep. Do
  NOT adopt `NativeTag`.
- **PROTO-PB-4** — type-plane `TypeDesc` skeleton. **PASS all 4 criteria.** arrow-count==projection
  invariant **5==5 (json), 13==13 (easing)**; frozen projections a strict subset (4/5 json arrows
  gain a width the frozen scan drops); two HM surfaces (`@host` unify, rank-1 instantiate) named &
  dormant. rustc exit 0, zero deps → **PROVE-08** (with P6 + B3).
- **PROTO-PB-5** — V8 shape-normalization microbench. **MET.** Uniform 3-field context + `mapState`
  retirement: all contexts same-map (was `[3,0]`→`[3]`), 0 dictionary-mode, TurboFan opt-status 25,
  cold ratio **1.0038 median (floor-neutral, ±1.4%)**. Target parse-that TS (READ-ONLY). Scratch GONE
  → **PROVE-11**.
- **PROTO-PB-6** — SIMD mantissa accumulation. **GO decisively.** Mula/Lemire in-vector accumulate +
  vshrn digit-count vs SWAR: exact u64 parity 3.8M runs + full canada; **53.3–55.5% cyc/B win** full
  canada, **69.6%** long fractional runs, 2.28× fewer ins/B (load-invariant); nibble `tz>>2` trap
  handled. Mula is the dominant lever (−40/−61%), vshrn smaller (−4/−6%). Win leverage-gated (~8% on
  ≤2-digit). Feed u64 straight to Eisel–Lemire. Scratch GONE; basis LIVE `number_simd.rs` → **PROVE-02**.

### sk-v21 cohort (2026-07-05) — mostly methodology + adjudication; less code-startable

- **PROTO-A1** — JSON bench adjudication. Overturns P1 canada +9.3% → **TIE**; **settles fused LOSES
  sonic on canada (1.25× wall, 1.14× cyc)** — the loss PROVE-02 redresses; **fold-over-lazy-cursor
  LOSES 2.3–4.9×** (the fused SINK is the only competitive lane — a hard caution for PROVE-07).
  Harness `a1_adjudication.rs`/`direct_struct.rs` GONE. Method feeds PROVE-02/06/07 gates.
- **PROTO-A2** — kernel PMU. **vshrn movemask door +35%** (beats hand-asm by 20–28%; crossing lever
  is binding, 2.6× the classify-ALU lever) → asm door retired, intrinsic swap wins. Live nibble/multi
  crossover **N*=3** (flips only |S|≤2 = CSV). Feeds **PROVE-04** (vshrn idiom) + **PROVE-13** (CSV flip).
- **PROTO-A3** — TW-S stack pricing. **smallbox[nightly] is the SOLE nightly enabler**
  (`parse_that/Cargo.toml:97`); `bbnf-regex` subtree smallbox-free (count=0); channel-parity leg
  load-confounded (deferred). Priced dep-modernization table. Scopes **PROVE-09** de-nightly.
- **PROTO-B1** — TOML end-to-end (real un-forked emitter, base efc6b875f). **Both walls fixable with
  generic primitives; 7/7 `toml`-crate round-trip.** WALL A = one-bit `newline_significant` ws-class;
  WALL B = bounded regex interpreter (drop-in for `match_leaf`); WART C = delete two `"value"`
  name-gates + fold JSON into `general_descent`. Code: report inline; scratch GONE. → **PROVE-13**.
- **PROTO-B3** — width-preserving lowering fix. **DONE, 12-code-line helper, +45/−15 one file**
  (`crates/core/src/lower/expression/wrap.rs`): all **22 Nu8 tags land U8, 0 I64, 0 Span**. Worktree
  `wf_bdd2b86b-8a6-6` GONE; test was `crates/core/tests/p6_hm_spike.rs`. → **PROVE-08** (the lowering
  precondition).
- **PROTO-P1** — NEON presize promote/delete. **DELETE**: OneShotSimd is 1.6–2.4× cold regression;
  Sampled ties GrowOnly → realloc churn is negligible. Settled DELETE; not a build asset.
- **PROTO-P2** — fused RD parse+value cold. **SUPPORTS**: fused 0.62–0.64× sonic geomean (beats on
  twitter/citm/canada/gsoc), tape-then-walk 2.0×. Superseded on canada by A1 (mimalloc/load
  artifact). Establishes fused-SINK-default for PROVE-06/07.
- **PROTO-P3** — edition-2024/stable build. **MIXED**: skinny clean on stable (5 edits); root blocked
  by parse_that→smallbox[nightly]. Superseded/confirmed by A3 D2. Scopes PROVE-09.
- **PROTO-P4** — measured KernelCostTable. **SUPPORTS, 3 flips**: LUT beats multipass **5.76× @N=16,
  11.42× @N=32**; N*=2 low-end flip; `WIDE_LUT_MAX` is a collision not cost boundary; **vaddv 36%
  cheaper than vshrn** for classify. `vqtbl4q` LUT kernel + method → **PROVE-05/04/13**.
- **PROTO-P5** — hand-asm vs intrinsics (64B classify). **asm +9–12%** from an 8-op LLVM
  De-Morgan miss — but A2 shows the vshrn intrinsic door beats asm; **asm door NOT warranted**.
  Reusable = the **checkasm harness template** (parity + clobber/canary + liveness) for all F1 NEON
  provers.
- **PROTO-P6** — HM-core inference spike. **SUPPORTS**: 189-LOC bidirectional synth/check +
  union-find types both grammars span_default=0; 22 Nu8 land I64 (blocked by lowering → B3 fixes).
  Worktree `wf_be85d0ab-073-12` GONE. → **PROVE-08** core.
- **PROTO-P7** — TOML through the un-forked emitter. **NO general emitter at master**; 6 enumerated
  **generator-defects, all in the emitter back-half**, zero grammar-authoring breaks. The un-fork
  cost list (a–d) is the PROVE-13 task spec. Feeds **PROVE-13**.

---

## Reusable non-prototype assets
- **checkasm harness template** (P5/P4/A2): randomized bit-exact parity + callee-saved
  clobber/canary + liveness-iteration for NEON kernels — the mandated codegen-prover instrument for
  PROVE-01/02/03/04/05.
- **PMU method** (`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`, min-of-N, paired interleaved) — LIVE
  at `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:44-45`.
- **css_canon_bench.rs** (LIVE, skinny) — CSS cold harness for PROVE-05.
- **A1's parity discipline** (`fused == foldcursor == sonic` full byte-equality incl. fingerprint) —
  the value-fabric close-gate oracle for PROVE-06/07.
</content>
</invoke>
