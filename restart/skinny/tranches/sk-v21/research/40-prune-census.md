# research/40 — SK-V21 Full-Tree Prune Census

Date: 2026-07-06. Repo root `/Users/mkbabb/Programming/bbnf-lang` at HEAD `05302109d` (master).
Read-only pass (no cargo, no mutation). Disposition legend: **KEEP** (survives, possibly
transformed by a wave), **PRUNE** (deleted), **ARCHIVE** (moved out of the live tree, retained),
**DEFERRED** (owned by a deferral authority, not a TW wave), **UNOWNED** (no SPEC/LEDGER coordinate).
Sizes: `.rs` src-LOC (tracked, `-not -path */target/*`) unless noted. Coordinates cite frozen
`SPEC.md` / `DEFERRED-LEDGER.md`.

**Architecture reconciliation (load-bearing, and a flagged tension).** Two live trees:
- Root `crates/` — the 13-crate god-crate tree; `core` = 247,428 LOC matches SPEC §8 §0 exactly and
  research/38 buckets `crates/core` at 216,818 src-LOC. **This is the split/transform target** (TW1a
  onward operate here).
- Root `skinny/` — the modern totality workspace, **FROZEN as differential-oracle + checkasm/
  microbench harness** at tag `sk-v19-frozen` (SPEC §11 W0.0), architecture-read-only.

TENSION (finding, not resolved here): several SPEC coordinates point executable work AT skinny
(§5.1 `skinny/crates/passes/src/lib.rs:590` perf_cost fix at TW4.1; §9 TW6 TOML work at skinny
submodule `general_descent` @`efc6b875f`) while W0.0 declares skinny read-only. The cutover folds
skinny's modern generator into the surviving tree; which tree is the executable base for TW2+ is not
crisply stated. Census dispositions below follow the explicit per-surface coordinates.

---

## (1) Root `crates/*` — the 13-crate tree

| Crate | src-LOC | Disposition | Owner / gate | Coordinate |
|---|---:|---|---|---|
| **bootstrap** | 465 | **PRUNE** | TW0 W0.6 — husk, re-export→core, deleted | SPEC §8.4, §11.1 W0.6; LEDGER N18 |
| **ser** | 530 | **ARCHIVE** | TW0 W0.6 — Lock-12 archive decision executed | SPEC §8.4, §11.1 W0.6; LEDGER N11 |
| **gorgeous** | 1,441 | **ARCHIVE** | TW0 W0.6 — Lock-12 archive (ser+gorgeous pair) | SPEC §8.4, §11.1 W0.6 |
| **core** | 247,428 | **KEEP/TRANSFORM** | TW1a — split into 6 seams (frontend/grammar, codegen, runtime tape+value, kernels, passes, host); 81.6% regen-owned (176,887 LOC bulk `git mv`), 39,931 hand LOC partitioned | SPEC §8.1–8.2, §11 TW1a; research/38 |
| **ir** | 52,314 | **KEEP/TRANSFORM** | TW4.1 (tests-only `for_singleton_count` selector retired into KernelCostTable) + TW1c (presize `Sampled/Exact/OneShotSimd` delete lives here + simd-scan, zero `core` occ.) | SPEC §5.1, §2, §11 TW1c/TW4.1; research/38 §1 |
| **simd-scan** | 3,389 | **KEEP aarch64 / PRUNE x86+wasm** | TW0 W0.6 deletes x86/wasm bodies (952 LOC + 92 AVX2 hits); live `KernelShape::select` ladder (`alphabet.rs:103,118-124`) is TW4.1's cost-table consumer | SPEC §8.4, §5.1, §11.1 W0.6; LEDGER W5d |
| **egraph** | 2,762 | **KEEP** | TW4.2 (greedy tree-cost Extractor = permanent default) + TW0 W0.5 (Lock-4 fix: egraph→csp direct dep re-vendored, never forked) | SPEC §5, §7.2, §11.1 W0.5 |
| **egraph-derive** | 343 | **KEEP** (implicit) | supports egraph `Language` derive — **NOT explicitly named in SPEC/LEDGER** | (see UNOWNED) |
| **csp-solver** | 9,686 | **KEEP-VENDORED, bbnf stage RETIRED** | TW0 W0.5 (re-vendor vs csc411 upstream, diff-checkpoint first, 0.2.0 break pre-staged; `node_budget` guard verified N22) + TW4.2 (`decision_csp.rs` deleted — tautological 2nd stage) | SPEC §7.2, §5.2, §11.1 W0.5; LEDGER N22/N27 |
| **analysis** | 5,241 | **UNOWNED** | memory `analysis-consolidation` says eliminate→IR passes, but **no SPEC/LEDGER row** | (see UNOWNED) |
| **lsp** | 4,123 | **UNOWNED** | **zero SPEC/LEDGER mention** (grep-empty) | (see UNOWNED) |
| **bbnf-path** | 918 | **KEEP** | TW3 — N17 registry rides the one-path-alphabet plane | SPEC §4; LEDGER N17 |
| **bbnf-path-ts** | 1,280 | **DEFERRED** | full TS path runtime rides the WASM decision | LEDGER §5 (deferral-audit-5 §3) |

parse_that 0.4.0 is a real dep of **core** (`Cargo.toml:15,85`), **ir** (`:13`), **gorgeous**
(`:78`); Cargo.lock confirms `parse_that → smallbox` (lines 2239–2248). See (6).

## (2) Root `skinny/*` — post `sk-v19-frozen`, the oracle surface

Whole tree KEEP as differential-oracle + checkasm/microbench harness (W0.0), architecture-read-only.
`json_canon_bench` (in bbnf-bench) graduates to `crates/` (SPEC §12).

| Surface | LOC/size | Disposition | Owner | Coordinate |
|---|---:|---|---|---|
| crates/**bbnf-regex** | 322 | **PRUNE** | TW2 — the 322-line facade; ONE regex home | SPEC §7.3; LEDGER N7 |
| crates/**parse-that-regex** | 3,022 | **PRUNE** | TW2 — superseded by canonical `../parse-that/rust/regex` path patch (SPEC names the bbnf-regex facade + "ad-hoc FIRST scanner" explicitly; this vendored copy is the scanner home) | SPEC §7.3 |
| crates/**bbnf-bench** | 36,676 | **KEEP** | oracle/microbench harness; `json_canon_bench` graduates | SPEC §12 |
| crates/{bbnf, bbnf-simd, codegen, grammar, ir, passes, runtime, simd-scan, test-fixtures} | ~28,570 | **KEEP** (oracle) | passes carries `perf_cost:0` (§5.1); runtime/`general_descent` is the TW6 reference (§9) | SPEC §5.1, §9 |
| **REDRESS.md** | 413 KB | **ARCHIVE / UNOWNED** | stale SK-V12–V20 campaign log, unnamed in SPEC | (see UNOWNED) |
| **RESULTS.md** | 262 KB | **ARCHIVE / UNOWNED** | stale campaign results, unnamed | (see UNOWNED) |
| target/ | ~59 GB | (ignored) | build artifact; `.gitignore` | — |

## (3) `docs/` — pre-restart corpus (49 dirs, ~1,000+ files tracked)

**No wave owns any of docs/.** SPEC §8.1 supersedes `restart/ARCHITECTURE.md`+`MIGRATION.md` only;
docs/ is not referenced. Archive strategy is a genuine gap.

| Subdir | files | Disposition |
|---|---:|---|
| tranches/ (49 dirs) | 743 | **ARCHIVE / UNOWNED** — pre-restart AA→BD tranche corpus |
| benchmarks/ | 220 | **ARCHIVE / UNOWNED** — superseded by json_canon_bench/skinny bench |
| precepts/ | 39 | **KEEP** — git submodule (`git@github.com:mkbabb/precepts.git`) |
| audit, bbnf, cookbook, gorgeous, instructions, migration, optimizer, parse-that, performance, pprint | ~71 | **ARCHIVE / UNOWNED** |

## (4) `restart/*` — the ~600K-line doc layer

| Surface | size | Disposition | Owner |
|---|---:|---|---|
| **ARCHITECTURE.md** (24-crate design) | 181 KB | **SUPERSEDED** — marked at TW0 W0.1, real 6-seam target named | SPEC §8.1, §11.1 W0.1 |
| **MIGRATION.md** §3-14 | 101 KB | **SUPERSEDED** — marked W0.1 (KEEP/ABROGATE tables inherited by TW1a) | SPEC §8.1–8.2, W0.1 |
| **MASTER-PLAN.md** | 164 KB | §13.6 **reconciled** W0.8 (N16); remainder superseded-adjacent | SPEC §11.1 W0.8; LEDGER N16 |
| **skinny/tranches/sk-v21/** | frozen | **KEEP (NORMATIVE)** — the governing manifest | SPEC (self) |
| **locks/**LOCKS.md | — | **KEEP (NORMATIVE)** — five-lock falsifier audit + meta-lock | SPEC §8.3, W0.3 |
| corpora, inheritance, prompts, audit, research | — | **KEEP (NORMATIVE)** per task framing (campaign substrate) | — |
| README.md, HANDOFF.md | 102 KB | **UNOWNED** — superseded-adjacent, no explicit disposition | (see UNOWNED) |

## (5) Root junk

| Surface | Disposition | Owner |
|---|---|---|
| **playground/test-results/** (TRACKED) | **PRUNE** — "playground junk ignored" | SPEC §11.1 W0.10 (16 tail) |
| **.playwright-mcp/** (150 entries, gitignored) | **PRUNE** — screenshot residue, already ignored | W0.10 |
| **.DS_Store** (every dir) | **PRUNE** | — |
| rustc-ice / stray *.log / *.png at root | **NONE FOUND** at HEAD (only LICENSE) | — |
| **restart-archive-2026-05-04/** (1.6 MB) | **KEEP (ARCHIVE)** — prior archive snapshot | — |
| target/ (30 GB), node_modules (283 MB) | ignored build artifacts | `.gitignore` |
| .claude/worktrees | **NONE** — no worktrees dir present | — |

## (6) LEGACY-vs-SURVIVES — the parse_that 0.4.0 leaf

SPEC §3 scope decision: stable-build requirement covers "the skinny workspace and every root crate
the totality plan retains." **No whole root crate is retired solely as a legacy leaf**; the retired
surface is the **parse_that 0.4.0 dependency EDGE** (→ `smallbox[nightly]`, E0554, Cargo.lock:2248):
- **core** + **ir**: parse_that is the pre-restart parser runtime; the totality replaces it (bespoke
  engine + canonical `../parse-that/rust/regex`, smallbox-clean count=0 — PROTO-A3 D2). The crates
  SURVIVE (core→TW1a split, ir→TW4.1); the dep + `json_parse_that` competitor bench are abrogated.
- **gorgeous**: parse_that dev-dep is moot — gorgeous ARCHIVES at Lock-12 (W0.6) regardless.

The nightly blocker is "moot, not a blocker" (SPEC §3): no "de-nightly parse-that first" precondition;
§7.3 routes skinny onto the canonical regex whose graph carries no smallbox.

---

## UNOWNED — surfaces no TW wave owns

1. **crates/lsp** (4,123 LOC) — zero SPEC/LEDGER mention; grep-empty. Disposition undecided.
2. **crates/analysis** (5,241 LOC) — memory `analysis-consolidation` (eliminate→IR passes) has no
   SPEC/LEDGER row; consolidation may fold into TW1a passes seam or TW2 but is unnamed.
3. **crates/egraph-derive** (343 LOC) — supports egraph's Language derive; survives-by-implication,
   never explicitly dispositioned.
4. **docs/** (entire pre-restart corpus: tranches 743 files/49 dirs, benchmarks 220, audit 37, +8
   small subdirs) — no wave; no archive strategy in SPEC. precepts/ is a live submodule (KEEP).
5. **skinny/REDRESS.md** (413 KB) + **skinny/RESULTS.md** (262 KB) — stale SK-V12→V20 campaign logs,
   unnamed; dead weight inside the frozen oracle.
6. **restart/README.md + HANDOFF.md** (~102 KB) — superseded-adjacent, no explicit disposition beyond
   MASTER-PLAN §13.6 (N16).
7. **crates/bbnf-path-ts** (1,280 LOC) — DEFERRED (WASM authority), not TW-owned (documented, not a gap).
