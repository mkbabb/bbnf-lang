# AZ-IV Hardening Pass 3 — Diophantus (sibling-lib deep audit)

**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-diophantus`
**CARGO_TARGET_DIR**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-diophantus/target/diophantus`
**Mandate**: deeply analyze all legacy/deprecated code across this repo and sibling
libs (`parse-that`, `pprint`, `csc411`/`csp-solver`, in-tree workspace members), and
route every finding to W0.2 (absorb / new sub-unit / out-of-scope). Read-only audit;
no source/manifest edits.

## 1. Per-sibling delineation

Each row records: latest visible commit, public API surface size (`^pub (fn|struct|
enum|trait|mod|use|const|static)` count over the crate's `src/` tree), dead-surface
signal, lock drift, deprecation marker count (`rg -ic 'deprecat|TODO.*remove|legacy'`),
and route. External siblings first; in-tree workspace members second.

### 1a. External siblings

| Sibling | Latest commit | Pub-API surface | Dead-surface signal | Lock drift | Deprecation markers | Route |
|---|---|---|---|---|---|---|
| `/Users/mkbabb/Programming/parse-that` (Rust workspace: `bootstrap`, `parse_that`, `regex`, `src`) | `2b0596a chore(rust-lock): refresh parser workspace lock after tape removal`; B7.W1.A2-A4 divan migration; HIR predicate-module collapse; scanner dead-trim removal | 354 across `parse_that/src` + `regex/src` + `bootstrap/src` (regex HIR per `project_bespoke_regex` consumed by bbnf-lang via `parse_that::regex::*`) | `rust/bootstrap/src/lib.rs:8 use bbnf_derive::Parser;` is the only consumer of the deprecated `bbnf_derive` 0.2.x crates.io artefact, anchoring `rust/Cargo.lock:124` package + `:1376` transitive edge | `typescript/package-lock.json` mtime 1773165484 vs `typescript/package.json` mtime 1773643075 — **lock 477 591 sec (~5.5 days) older than `package.json`**. parse-that's `.cargo/config.toml` already deletes `bbnf_derive = path` because B2 retired `bbnf-lang/crates/derive` (verbatim comment: `# bbnf_derive: B2 deleted ../../bbnf-lang/crates/derive; stale path removed.`). The fix is incomplete — bootstrap still resolves `bbnf_derive` from crates.io 0.2 | source has 0 explicit `deprecat` markers (clean recent prose) | **EXPAND-W0.2 BOUND** to include `parse-that/rust/bootstrap/**`, `parse-that/rust/Cargo.lock`, `parse-that/typescript/package-lock.json` |
| `/Users/mkbabb/Programming/pprint` (Rust workspace: `pprint`, `pprint_derive`) | `2b4d2d4 style: cargo fmt`; B7.W2.A6-A8 divan migration, EmitSink → `Serializer<'a>` zero-unsafe migration, `text_inline_ws`, `light_checkpoint/restore`, `flat_width` u16 saturation | 29 across `rust/src/` + `rust/derive/lib.rs` (a small surface — published via `pprint = "0.3"` and `pprint_derive = "0.2.x"`) | `rust/examples/sizes.rs` is the sole example, 18 lines, prints type sizes only — no broken examples. `rust/tests/{builder,derive,digit_count,pretty}_tests.rs` total 828 LoC; recent commits show test additions (`test: add tests for light_checkpoint, text_inline_ws, flat_width saturation`) — test surface is healthy | no Cargo.lock entries flagged; no `bbnf_derive` linkage | clean (0 source `deprecat`/`legacy` markers) | **OUT-OF-SCOPE-W0.2** — sibling versions independently and is consumed by bbnf-lang only via crates.io `pprint = "0.3"`. Vestigial `target/package/{pprint-0.1.0..0.3.6}/` artefacts are local cargo-package build cache, not source residue |
| `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver` | `69d7330 docs(solver): document deferred extensions + add bench-compare harness`; criterion benches for `morph-core` align/primitives; proptest for `AssignmentBuilder` vs brute-force; `wasm-morph` scaffold + sub-crate; Régin's GAC for `AllDifferentExcept` | **532-line `lib.rs`**, 42 source files, criterion-bench harness, optional `py = ["dep:pyo3"]` feature, sibling crates `morph-core`, `wasm`, `wasm-morph` | strict-superset of `bbnf-lang/crates/csp-solver`: csc411 has `src/builder/`, `src/constraint/all_different_except.rs`, `src/domain/cost_finite.rs`, `src/solver/gac_alldiff_except.rs`, `src/py.rs` — **5 files exclusive to csc411**; **22 shared files diverge** content-wise per `diff -rq` (lib, adjacency, all `constraint/*`, `domain/{bitset,mod}`, all `puzzles/*`, all `solver/*`) | n/a (its own workspace); but `[patch.crates-io]` in parse-that AND in (declared by GESTALT.md:931) bbnf-lang point at csc411, so consumers transitively see its tree | clean source | **NEW-SUB-UNIT-W0.2.b** — `docs/GESTALT.md:928-933` already declares `csp-solver reconciliation: workspace copy is bench home; csc411 sibling is algorithm-evolution authoritative`. The split is policy, not bug — but the in-tree fork must either rebase to csc411's algorithm-evolution head, or pin csc411 to a commit hash to make the split a versioned contract. Currently neither: bidirectional drift |

### 1b. In-tree workspace members

`pub_count` is `^pub (fn|struct|enum|trait|mod|use|const|static)` line tally over each
crate's `src/` tree. Recent activity is the top line of `git log --oneline -10 --
crates/<n>/`. Deprecation tally is `rg -ic 'deprecat|TODO.*remove|legacy' crates/<n>/`
(includes both `src/` and `tests/`).

| Crate | Latest crate-touching commit | Pub-API | Dep-marker count | Notable surface | Route |
|---|---|---|---|---|---|
| `core` (bbnf) | `e73de57e bench(post-az-iii): refresh 17-entry matrix on canonical struct-only path` | 683 | 36 (highest in workspace) | `backend/rust/view/color.rs:290 LoC` is the Locke-narrowed shim; only one in-source `use .* color::` reference. `runtime/view.rs` is the live `RuntimeView<'p>` trait (~25 doc lines explaining surface) — re-exported only by `view/mod.rs`, no `color::` re-export | OUT-OF-SCOPE-W0.2 (Locke narrowed claim defers `color` shim cleanup until consumers migrate; that is a runtime-surface concern routed to W2, not topology) |
| `analysis` (bbnf-analysis) | `69c11112 fix(types/obligations): unify HeterogeneousAltJoin obligation surface` | 114 | 3 (all `#[allow(deprecated)]` on `ls-types 0.0.3` `deprecated:` field — `crates/analysis/src/features/document_symbols.rs:6-19` block calls this out: `reason = "ls-types 0.0.3 still requires the deprecated 'deprecated' field"`) | LSP feature surface, 17 src files | OUT-OF-SCOPE — `#[allow(deprecated)]` is required by external `ls-types` schema, not legacy bbnf code |
| `ir` (bbnf-ir) | `301acf47 test(csp/authority): cover shape, layout, dispatch consumers` | 576 | 31 | `passes/recognizers/node_facts.rs` retains `recognize_seq_legacy` / `recognize_alt_legacy` arms (5 hits); `passes/patterns/mod.rs` has 3 `legacy` mentions tied to `PatternAnnotations` — Locke narrowed claim says migrate Pratt before delete | ROUTE-TO-W1 (legacy recognizer arms are runtime-surface concerns, not topology repair) |
| `lsp` (bbnf-lsp) | `576b3701 style: cargo fmt --all post-AZ-II` | 33 | 0 | DAP bridge present and live: `crates/lsp/src/dap/{adapter,mapping,mod,protocol}.rs` + `tests/dap.rs`. Matches `docs/codegen-paths.md:DAP bridge: bbnf-lsp --dap speaks Debug Adapter Protocol` | OUT-OF-SCOPE (live, no drift) |
| `ser` (bbnf-ser) | `d1b8c4d6 fix(bench): eliminate root warning flood` | 13 | 0 | only 3 consumer sites: `crates/core/src/generate/serialize/*` (live) and 2 historical AZ-II cutover bench mirrors under `docs/benchmarks/archive/AZ-II/cutover/{stage-a,stage-b}-bbnf.rs` (not built) | NEW-SUB-UNIT-W0.2.c — verify the cutover stage files are flagged historical or excluded from `cargo metadata`; per Locke claim #5 docs sync is not a gate |
| `gorgeous` | `2755947e fix(types/obligations): replace silent BoxedEnum on compound Ref with named obligation` | 25 | 1 (`vm.rs` doc-comment `legacy alias`, feature-gated VM mode) | `6e7a57c5 fix(gorgeous/jit): delete retired JIT surface` already retired the JIT shim; remaining marker is feature-gate doc | OUT-OF-SCOPE (clean) |
| `bootstrap` (bbnf-bootstrap) | `a09173dc fix(bootstrap_parser): wrap mapped_factor mapping in anonymous compound` | 1 (sole `pub use ::bbnf::grammar::generated::BbnfBootstrap`) | 0 | dev-only bins: `cost_grid_sweep.rs`, `debug_parse.rs`, `dump_ir.rs`. Library is a thin re-export. **No Rust source consumes `use bbnf_bootstrap::*`** — every actual user (5 sites in `crates/core/{tests,benches,examples}`, `crates/core/src/grammar/generated/bbnf.rs`) consumes via `bbnf::grammar::generated::*`. Locke claim #8: "metadata must be updated if its role is now compatibility/diagnostic" | ABSORB-INTO-W0.2 (one-line metadata edit to `crates/bootstrap/Cargo.toml:6` `description = "..."` so the manifest matches the post-B2 role) |
| `egraph` | `26f95469 feat(egraph): Ruler-style enumerate + oracle + residue substrate` | 53 | 0 | `ruler/{enumerate,oracle,residue,mod}.rs` is live substrate; tests `ruler_{enumerate,oracle,residue}.rs` + example `examples/ruler_smoke.rs`. Locke optimization claim #5: ruler/RuleSet are live substrate but production codegen does not consume loaded rules end-to-end | OUT-OF-SCOPE-W0.2 (covered by W0.4 Map Preservation; broader Ruler activation is W2 territory) |
| `egraph-derive` | `f15a6f68 feat(egraph-derive): auto-detect recursive field types + multi-child arrays` | 1 | 0 | minimal proc-macro: 343 LoC, 1 file (`src/lib.rs`). 2 production consumers: `crates/ir/src/egraph/node.rs:18 use egraph_derive::Language` + `crates/egraph/tests/derive_language.rs`. Plus 2 doc references | OUT-OF-SCOPE (live, minimal, single-file proc-macro; not a shell) |
| `csp-solver` (in-tree) | `3100fc5b refactor(csp-solver): retire py.rs (zero Python consumers; orphan FFI surface)` | 110 | 0 | divergent fork from csc411 sibling (row 1a). Tranche AA.2 vendor lineage; recent `47d6fafb perf(csp-solver): soft-index + incremental bound in branch-and-bound` is bbnf-lang-only optimisation never returned to csc411 | covered by NEW-SUB-UNIT-W0.2.b above |
| `simd-scan` | `8bcf33da fix(simd-scan/structural-index): move structural index authority` | 33 | 0 | live consumers: 8 generated grammars + 2 own tests; no third-party. Locke claim #7: wired but gated and narrower than older comments imply | OUT-OF-SCOPE-W0.2 (gating breadth is a W2/W3 perf concern) |

## 2. `bbnf_derive`/`crates/derive` residue census

W0.2's hard-gate clause is `production rg "bbnf_derive|crates/derive" hits are zero or
explicitly archived`. Live hits inventoried below.

### 2a. In-tree archived doc-comments (acceptable per gate)

| File:line | Content | Class |
|---|---|---|
| `xtask/src/main.rs:4` | `//! build time, replacing the pre-B2 \`bbnf_derive\` proc-macro contract` | doc-comment, archived (refers to retirement) — acceptable |
| `xtask/src/regen.rs:3` | `//! Replaces the pre-B2 \`bbnf_derive\` proc-macro contract that ran the` | same |
| `xtask/src/regen.rs:14` | `//! Mirrors \`crates/derive/src/lib.rs\` lines 281-361 — the proc-macro` | acceptable, but the line-number references are **stale**: the file no longer exists, so consumers cannot verify the mirroring claim |
| `xtask/src/regen.rs:97` | `/// parsing (\`crates/derive/src/lib.rs:226-278\`).` | same — stale line link |
| `xtask/src/regen.rs:308` | `// call the proc-macro makes at \`crates/derive/src/lib.rs:324\`.` | same |
| `xtask/src/regen.rs:319` | `// \`crates/derive/src/lib.rs:335-353\`. The module name is` | same |

The 4 stale `crates/derive/src/lib.rs:NNN` references fail the verification spirit of
the gate ("explicitly archived" should mean retrievable from history with a commit
hash, not unverifiable line numbers in a deleted file). Suggested rephrase: drop the
line-number suffixes and cite commit `6142387f feat(b2): retire crates/derive proc-macro
crate; purge bbnf_derive deps (B2.W2)`.

### 2b. wasm/Cargo.lock — HARD HIT (active edge)

| File:line | Content | Class |
|---|---|---|
| `wasm/Cargo.lock:105-118` | `[[package]] name = "bbnf_derive" version = "0.2.11" dependencies = [bbnf, bbnf-ir, indexmap, parse_that, pprint, proc-macro2, quote, syn]` | **active 0.2.11 package block** |
| `wasm/Cargo.lock:259` | `"bbnf_derive",` (transitive dep of `parse-that-bootstrap = 0.1.10`) | **active dep edge** through parse-that's `regex-bootstrap` published version |

### 2c. parse-that — HARD HIT (active edge, both source and lock)

| File:line | Content | Class |
|---|---|---|
| `/Users/mkbabb/Programming/parse-that/rust/Cargo.lock:124-138` | `[[package]] name = "bbnf_derive" version = "0.2.9"` | **active 0.2.9 package block** |
| `/Users/mkbabb/Programming/parse-that/rust/Cargo.lock:1376` | `"bbnf_derive",` transitive | **active dep edge** |
| `/Users/mkbabb/Programming/parse-that/rust/bootstrap/src/lib.rs:8` | `use bbnf_derive::Parser;` | **runtime use of deprecated proc-macro** |
| `/Users/mkbabb/Programming/parse-that/rust/bootstrap/Cargo.toml:13` | `bbnf_derive = "0.2"` | **published-crate dependency** |

### 2d. Total live `bbnf_derive` edges

4 active edges: 1 wasm/Cargo.lock package, 1 wasm/Cargo.lock transitive, 1 parse-that
source `use`, 1 parse-that manifest dep. Plus 4 stale `crates/derive/src/lib.rs:NNN`
doc-link references in `xtask/src/regen.rs`.

## 3. NPM lock drift

| Lock | `package.json` mtime | lock mtime | Drift (sec) | Status |
|---|---|---|---|---|
| `bbnf-wt-aziv-w0-harden-diophantus/package-lock.json` | 1777645159 | 1777645159 | 0 | sync |
| `bbnf-wt-aziv-w0-harden-diophantus/extension/package-lock.json` | 1777645159 | 1777645159 | 0 | sync |
| `parse-that/typescript/package-lock.json` | pkg=1773643075 | lock=1773165484 | **-477 591 (~5.5 days stale)** | drift; matches Locke claim #4 |

Locke claim #4 ("NPM locks are stale in bbnf-lang and parse-that TypeScript"). bbnf-
lang side now appears synced at the recent `1777645159` mtime — likely already touched
in `c14832a3 chore: phase 1 cleanup`. parse-that side still drifts: `package.json`
edited 1773643075 (`docs: update benchmark numbers`), `package-lock.json` last touched
1773165484 (`chore: phase 1 cleanup — delete dead code, TS, prettier plugin`). The
later edit to package.json went un-locked.

## 4. csp-solver split source-of-truth

`diff -rq /Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-diophantus/crates/csp-solver/src
/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src` reports:

- **22 shared files diverge** content-wise: `lib.rs`, `adjacency.rs`, all
  `constraint/{all_different,dispatch,implication,lambda,mod,not_equal,soft,traits}.rs`,
  `domain/{bitset,mod}.rs`, `puzzles/futoshiki/mod.rs`,
  `puzzles/sudoku/{csp,generate,mod,transform}.rs`, all
  `solver/{ac3,backjump,backtrack,gac_alldiff,local_search,mod,monotonic,nogoods,optimize,propagate}.rs`.
- **csc411-only files (5)**: `src/builder/`, `src/constraint/all_different_except.rs`,
  `src/domain/cost_finite.rs`, `src/solver/gac_alldiff_except.rs`, `src/py.rs`.
- **bbnf-lang-only**: none (every bbnf-lang file has a csc411 counterpart, but with
  divergent content).

Bidirectional drift:
- bbnf-lang fork has retired `py.rs` and (per `47d6fafb`) added `soft-index +
  incremental bound in branch-and-bound`.
- csc411 has expanded the constraint vocabulary (`all_different_except`,
  `gac_alldiff_except`, `cost_finite`), added Régin's GAC for `AllDifferentExcept`,
  introduced `morph-core` and `wasm-morph` sibling crates, and switched bench harness
  to criterion.

`docs/GESTALT.md:928-933` declares **policy**: the bbnf-lang in-tree copy is `bench
home`; the csc411 sibling is `algorithm-evolution authoritative`. The policy is sound
but unenforced — divergent drift of 22 shared files indicates the in-tree fork has not
rebased onto csc411's recent algorithm work since the AA.2 vendor.

## 5. Deprecation marker totals

| Crate | Marker count | Material? |
|---|---|---|
| core | 36 | mostly comment-doc on `legacy bootstrap_parser shape`, `legacy iteration shape`, `color.rs` Locke-shim doc; not breakage signals |
| analysis | 3 | external-API workaround (`ls-types 0.0.3` `deprecated:` field) |
| ir | 31 | `recognize_*_legacy` arms + `PatternAnnotations` Pratt-tied legacy |
| lsp | 0 | clean |
| ser | 0 | clean |
| gorgeous | 1 | feature-gate doc |
| bootstrap | 0 | clean |
| egraph | 0 | clean |
| egraph-derive | 0 | clean |
| csp-solver | 0 | clean |
| simd-scan | 0 | clean |
| **total in-tree** | **71** | |

External siblings: parse-that 0 source markers (clean prose) but 4 active
`bbnf_derive` edges in source+manifest+lock; pprint 0; csc411-csp-solver 0.

## 6. W0.2 file-bound coverage verdict

W0.2's current `Files:` list reads:

> `wasm/**`, `.cargo/config.toml`, package locks, sibling parse-that/pprint docs as needed.

Coverage assessment per finding:

1. `wasm/Cargo.lock` `bbnf_derive` residue (§2b) — covered by `wasm/**` ✓
2. parse-that bootstrap source dep on `bbnf_derive` (§2c) — covered only as "sibling
   parse-that/pprint **docs** as needed". The repair is **source + manifest + lock**,
   not docs. **Bound is too narrow.**
3. parse-that npm lock drift (§3) — current bound says "package locks" without naming
   parse-that's `typescript/package-lock.json`. Ambiguous; **explicit listing safer.**
4. csp-solver canonical-source ambiguity (§4) — **not covered**: W0.2's mechanism says
   "pick one csp-solver source" but file bound includes neither `crates/csp-solver/**`
   nor `/Users/mkbabb/Programming/csc411/**`. **Bound is missing.** GESTALT already
   declares the policy; W0.2 must enforce it.
5. `xtask/src/regen.rs` stale `crates/derive/src/lib.rs:NNN` doc-links (§2a) — not in
   W0.2's bound. Could route to W0.3 (xtask is in W0.3's bound) but the content is
   doc-cleanup, not regen logic. **Bound gap.**
6. `crates/bootstrap/Cargo.toml` description (§1b row "bootstrap"; Locke claim #8) —
   W0.2's `Cargo.toml` bound only covers root, not crate manifests. **Bound gap.**

## 7. Routes

| Finding | Route |
|---|---|
| `wasm/Cargo.lock:105-118, :259` active `bbnf_derive` package + transitive | ABSORB-INTO-W0.2 (within current `wasm/**` bound) |
| parse-that `rust/bootstrap/Cargo.toml:13` + `lib.rs:8` deprecated dep | EXPAND-W0.2-BOUND to include `parse-that/rust/bootstrap/**` (source) and `parse-that/rust/Cargo.lock` |
| csp-solver split (22 divergent files; 5 csc411-only) | NEW-SUB-UNIT-W0.2.b (Topology canonicalisation) — enforce GESTALT policy by either re-vendoring csc411 head or pinning csc411 to a commit hash |
| `crates/bootstrap/Cargo.toml` description metadata | EXPAND-W0.2-BOUND to `crates/bootstrap/Cargo.toml` (single-line metadata edit, Locke claim #8) |
| `xtask/src/regen.rs` stale `crates/derive/src/lib.rs:NNN` doc-links (lines 14, 97, 308, 319) | OUT-OF-SCOPE-W0.2; ABSORB-INTO-W0.3 (xtask is W0.3's bound; doc cleanup is a free-rider) |
| parse-that `typescript/package-lock.json` 5.5-day drift | EXPAND-W0.2-BOUND to `parse-that/typescript/package-lock.json` |
| pprint packaged-artefact rename relic (`target/package/pprint-{0.1.0..0.3.6}/`) | OUT-OF-SCOPE — sibling versions independently |
| `bbnf-ser` historical AZ-II cutover bench files at `docs/benchmarks/archive/AZ-II/cutover/stage-{a,b}-bbnf.rs` | NEW-SUB-UNIT-W0.2.c (verify or convert to ignored historical) |
| `crates/ir/src/passes/recognizers/node_facts.rs` `recognize_*_legacy` + `PatternAnnotations` Pratt tie | OUT-OF-SCOPE-W0.2; ROUTE-TO-W1 runtime surface |
| `crates/core/src/backend/rust/view/color.rs` shim per Locke narrowed claim | OUT-OF-SCOPE-W0.2; ROUTE-TO-W2 runtime |
| Ruler end-to-end activation (consumes substrate but not loaded rules) | OUT-OF-SCOPE-W0.2; W0.4 Map Preservation handles `Map { fn_id }` only; broader Ruler ROUTE-TO-W2 |

## Exact Wave-Amendment Text

Apply to `docs/tranches/AZ-IV/waves/W0.md` §`AZ-IV.W0.2 Topology Repair`.

### A. Replace the `Files:` line

Currently:
```
- Files: `wasm/**`, `.cargo/config.toml`, package locks, sibling parse-that/pprint docs as needed.
```

Replace with:
```
- Files: `wasm/**`, `.cargo/config.toml`, root and `extension/` package locks,
  `crates/bootstrap/Cargo.toml` (metadata-only),
  `crates/csp-solver/**` and `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/**`
  (canonical-source declaration enforcement per `docs/GESTALT.md:928-933`),
  `/Users/mkbabb/Programming/parse-that/rust/bootstrap/**`,
  `/Users/mkbabb/Programming/parse-that/rust/Cargo.lock`,
  `/Users/mkbabb/Programming/parse-that/typescript/package-lock.json`,
  sibling parse-that/pprint docs as needed.
```

### B. Append a sub-gate clause

After the existing `- Sub-gate: ...` bullet, append:
```
- Sub-gate addendum (Diophantus 2026-05-01): production
  `rg "bbnf_derive|crates/derive"` over the active source tree (this repo's
  `crates/`, `xtask/`, `wasm/`, plus `parse-that/rust/`) returns at most
  archived-doc-comment hits with explicit retirement language (`pre-B2`,
  `retired`, `purged`); zero `use bbnf_derive::*`, zero `bbnf_derive = `
  manifest entries, zero `name = "bbnf_derive"` `Cargo.lock` blocks.
  csp-solver `diff -rq` between bbnf-lang `crates/csp-solver/src` and csc411
  `CSC411_HW2_ProgrammingQuestion/csp-solver/src` returns "Only in
  <canonical>" entries plus identical content for the shared file set, never
  "Files differ" rows.
```

### C. Append two new sub-units

After the existing `AZ-IV.W0.2 Topology Repair` paragraph, append:

```
### AZ-IV.W0.2.b csp-solver canonicalisation enforcement

- Mechanism: enforce the canonicalisation policy already declared in
  `docs/GESTALT.md:928-933` — bbnf-lang `crates/csp-solver` is bench
  authoritative; csc411 sibling is algorithm-evolution authoritative.
  Either rebase the in-tree copy onto a named csc411 commit hash and update
  `Cargo.toml` metadata to record that hash, or pin `[patch.crates-io]` in
  `.cargo/config.toml` to a specific csc411 commit so consumers can verify.
  Document the 5 csc411-only files (`builder/`, `constraint/all_different_except.rs`,
  `domain/cost_finite.rs`, `solver/gac_alldiff_except.rs`, `py.rs`) as
  declared-out-of-scope for the bbnf-lang copy.
- Files: `crates/csp-solver/**`,
  `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/**`
  (read-only reference), `.cargo/config.toml`, `docs/GESTALT.md` (clarify
  what "authoritative for benches" means concretely).
- Sub-gate: `diff -rq` between the two trees lists only the 5 declared csc411-
  only files plus an identical content set for the 22 shared files, never
  "Files differ"; `cargo metadata --locked` passes in both repos.

### AZ-IV.W0.2.c bbnf_derive eradication

- Mechanism: delete `bbnf_derive = "0.2"` from
  `parse-that/rust/bootstrap/Cargo.toml:13`, rewrite
  `parse-that/rust/bootstrap/src/lib.rs:8` to consume the post-B2 contract
  (path-included grammar via `cargo xtask regen`, mirroring bbnf-lang's
  `crates/bootstrap` thin re-export), regenerate
  `parse-that/rust/Cargo.lock` and `wasm/Cargo.lock` to drop the
  `bbnf_derive` package + transitive edge, and refresh
  `parse-that/typescript/package-lock.json` against
  `parse-that/typescript/package.json`. Also rephrase the 4 stale
  `crates/derive/src/lib.rs:NNN` doc-link references in `xtask/src/regen.rs`
  (lines 14, 97, 308, 319) so they cite commit `6142387f` instead of
  unverifiable line numbers in a deleted file.
- Files: `/Users/mkbabb/Programming/parse-that/rust/bootstrap/**`,
  `/Users/mkbabb/Programming/parse-that/rust/Cargo.lock`,
  `/Users/mkbabb/Programming/parse-that/typescript/package-lock.json`,
  `wasm/Cargo.lock`, `xtask/src/regen.rs` (4-line doc-comment fix only).
- Sub-gate: `rg "bbnf_derive"` over `parse-that/rust/`,
  `parse-that/typescript/`, `wasm/`, and bbnf-lang `crates/`,`xtask/` returns
  no live source/manifest/lock matches — only archived doc-comments with
  commit-hash citations. `cargo metadata --locked` passes in
  `parse-that/rust/` and `wasm/`.
```

## Per-sibling row count + top 5 lock-drift items (commit body)

- **External siblings audited**: 3 (`parse-that`, `pprint`, `csc411 csp-solver`).
- **In-tree workspace members audited**: 11 (core, analysis, ir, lsp, ser, gorgeous,
  bootstrap, egraph, egraph-derive, csp-solver, simd-scan).
- **Total deprecation markers in-tree**: 71 (core 36, ir 31, gorgeous 1, analysis 3
  external-API workaround; rest 0).
- **Top 5 lock-drift items**:
  1. `wasm/Cargo.lock:105-118` — active `bbnf_derive 0.2.11` package block.
  2. `wasm/Cargo.lock:259` — transitive `bbnf_derive` edge through
     `parse-that-bootstrap = 0.1.10`.
  3. `/Users/mkbabb/Programming/parse-that/rust/Cargo.lock:124-138` — sibling
     `bbnf_derive 0.2.9` package block.
  4. `/Users/mkbabb/Programming/parse-that/rust/Cargo.lock:1376` — sibling
     `bbnf_derive` transitive edge.
  5. `/Users/mkbabb/Programming/parse-that/typescript/package-lock.json` —
     5.5-day drift behind `package.json` (lock=1773165484, pkg=1773643075).
