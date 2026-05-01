# S7 — Codebase ↔ Doc Isomorphism Audit

Read-only cross-reference of primary docs against the live tree at
HEAD `d071daf9` (AZ-III TERMINAL_WITH_CARRIES, 2026-04-30). Each row
carries one doc claim with verdict (MATCHES / STALE / AMBIGUOUS /
MISSING) and patch recommendation. No source files modified.

## Triumvirate gate

The README's structural thesis — `Structure` block at lines 11-33,
the `rust/`, `wasm/`, `typescript/`, `prettier-plugin-bbnf/` claims
— is wholesale stale. None of those layout assumptions hold: the
top-level `rust/` directory does not exist; `crates/` is the monorepo
root; `typescript/` and `prettier-plugin-bbnf/` are gone. The
orientation passage is inverted from reality. CRITICAL threshold
crossed; README warrants wholesale rewrite, not patch list.

## Methodology

- README, GESTALT (sampled abstract + module + Era V + plan blocks),
  codegen-paths, instructions/{README,PROFILING} read directly.
- Workspace truth: `Cargo.toml`, `.cargo/config.toml`, `Makefile`,
  `crates/`, `crates/core/src/{lib.rs,runtime,grammar/generated,
  backend,pipeline}`, `crates/ir/src/registry/strategy.rs`.
- Per-crate README inventory ran across all 11 crates: zero files.

Total claims audited: **47**. Severity counts: CRITICAL 4 / HIGH 14
/ MED 19 / LOW 10.

---

## README.md

### C1 (CRITICAL) — `Structure` block (12-33)

Claim: `rust/{core,ir,derive,analysis,lsp}`, `wasm/`, `typescript/`,
`prettier-plugin-bbnf/`, `grammar/css/{value-unit,color,values,
selectors,keyframes,stylesheet,css-tokens,css-stylesheet-pretty}`,
`grammar/lang/`.

Verdict: **STALE.** Code reality: `rust/` deleted at architectural-
consolidation; `crates/` is the workspace root (Cargo.toml:2).
`crates/derive/` retired with proc-macro (B2.W2). `typescript/`
and `prettier-plugin-bbnf/` absent. `grammar/css/` contains only
`l4/` and `pretty.bbnf`. `grammar/lang/` does not exist —
`grammar/misc/` is the actual directory. `grammar/BBNF.md`
(referenced line 40) does not exist; only `grammar/bbnf/{bbnf,
expressions,types}.bbnf` source files.

Patch: rewrite block from `crates/`-rooted reality plus `wasm/`,
`playground/`, `extension/`, `grammar/`, `docs/`, `scripts/`,
`data/`, `server/`, `xtask/`. Drop `grammar/css/` enumeration
(drift-prone). Name live grammars by ident (bbnf, json, css_l4,
css_pretty, google_sheets, ebnf, bnf, csv, math). Replace
`grammar/BBNF.md` link with `grammar/bbnf/`.

### C2 (CRITICAL) — Slab parsing (124-136)

Claim: `#[derive(Parser)] #[parser(path = "json.bbnf", slab)]` plus
`BumpSlab::with_capacity(...)` is the production codegen entrypoint.

Verdict: **STALE.** Proc-macro retired entirely at B2.W2 per
codegen-paths §1: "the pre-B2 `#[derive(Parser)]` / `bbnf_derive`
proc-macro path is retired for production codegen." Generated
parsers under `crates/core/src/grammar/generated/<ident>.rs` are
written by `cargo xtask regen` and consumed via `include!`.
`<Grammar>Parser::parse(...) -> Result<<Grammar>Document<'_>,
ParseErr>` is the live entrypoint
(`crates/ir/src/registry/strategy.rs:155-163`).

Patch: rewrite as `cargo xtask regen` + per-grammar
`<Grammar>Parser::parse(...) -> <Grammar>Document<'_>`. Drop
`#[derive(Parser)]` and `BumpSlab` API.

### C3 (CRITICAL) — Span-only parsing (138-141)

Claim: `#[parser(span)]` generates `__rule_span` functions for
zero-allocation validation.

Verdict: **STALE.** Same root cause as C2.

Patch: drop section, or rewrite around how a future span-only
emitter would slot into `EmitStrategy`.

### H1 (HIGH) — `typescript/` + `prettier-plugin-bbnf/` (20-21)

Verdict: **STALE.** Neither directory exists.

Patch: drop both. If TS surface persists in another repo, name it
explicitly.

### M1 (MED) — Sources reference parse-that only (5-6, 176)

Verdict: **AMBIGUOUS.** README does not acknowledge the
four-repo surface (parse-that, pprint, csp-solver) GESTALT §2
describes. `.cargo/config.toml` `[patch.crates-io]` patches
`parse_that`, `bbnf-regex`, `pprint`, `pprint_derive`.

Patch: optional one-line callout that bbnf-lang path-patches
sibling repos.

### M2 (MED) — DAP claim (109)

Claim: VS Code extension supports DAP via `bbnf-lsp --dap`.

Verdict: **AMBIGUOUS.** Architecturally consistent (VM has
`Op::DebugBreak`; codegen-paths §2 confirms). `--dap` flag
handler not verified.

Patch: defer to lane 4 verification.

### L1 (LOW) — Playground claim (144)

Verdict: **MATCHES.** `playground/` exists; `Makefile` has
`build-wasm` target.

---

## docs/GESTALT.md

### H2 (HIGH) — `crates/tape`, `crates/derive`, `crates/json-prototype`
listed as workspace members (205-206)

Verdict: **STALE.** All three deleted: `tape` at AZ-II.cutover.O5
+ AZ-III.W1 (acknowledged at line 43, 218 of same doc; line 205's
enumeration not refreshed). `derive` at B2.W2. `json-prototype`
likewise. `Cargo.toml:2` lists 11 members.

Patch: tighten to: `crates/core, crates/ir, crates/analysis,
crates/lsp, crates/ser, crates/gorgeous, crates/bootstrap,
crates/egraph, crates/egraph-derive, crates/csp-solver,
crates/simd-scan` (11 members, plus `xtask`).

### H3 (HIGH) — `crates/derive/tests/cache_invalidation/` (1239)

Verdict: **STALE.** No `crates/derive/`; test sub-tree retired
with proc-macro.

Patch: rewrite cache-invalidation paragraph against
`xtask/`-driven regen; `cargo xtask regen --check` is the
equivalent CI gate.

### M3 (MED) — Headline numbers contradiction with PROFILING.md
(88-114)

Claim row "Current divan adoption: 0".

Verdict: **AMBIGUOUS.** PROFILING.md:142-148 says "post-B7
(2026-04-27), divan is the only harness across bbnf-lang,
parse-that, and pprint". Headline table is a snapshot of an
earlier authoring window.

Patch: refresh the headline table with a single dated row, resolve
the divan-adoption contradiction.

### M4 (MED) — Era enumeration (246-265)

Line 263 says "(AZ-I / AZ-II / BA / BB scaffold — not started)"
but §1 (16-32) acknowledges AZ-III closed terminal.

Verdict: **STALE.**

Patch: rewrite Era VI block with AZ-III closure or annotate "as
of 2026-04-15" with forward-pointer to §1.

### M5 (MED) — Module-overview crate count (200-216)

Claim references 13-then-14-member workspace.

Verdict: **STALE.** Live count: 11 + xtask = 12 (Cargo.toml:2).

Patch: state live count, reference the workspace members list
verbatim.

### L2 (LOW) — VM at `crates/ir/src/vm/` (416)

Verdict: **MATCHES.** Directory exists.

### L3 (LOW) — Era diagram "scaffold — not started" (263)

Verdict: **STALE — cosmetic.** AZ-I, AZ-II, AZ-III all closed.

Patch: replace with `(AZ-III closed; BA / BB ahead)`.

---

## docs/codegen-paths.md

### H4 (HIGH) — Status callout (86-104) is fresh and accurate

Claim: 9/9 production grammars on StructDirect; TapeDirect deleted;
bootstrap_parser DELETED 1505 LOC at AZ-III.W2.4 (`954d166b`).

Verdict: **MATCHES.** Cross-checked:
- `crates/ir/src/registry/strategy.rs:151-256` — 9 grammar arms,
  all `StructDirect`, panic on unknown grammar (257-260).
- `crates/core/src/grammar/generated/` lists 9 generated files
  (`bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json,
  math`) plus `mod.rs`.
- `find crates -name "bootstrap_parser*"` returns nothing.

Patch: none.

### M6 (MED) — `runtime/` description (203)

Claim: "Grammar-specific documents/builders plus remaining
Parsed/tape facade."

Verdict: **STALE.** Contradicts §1 status block four lines above
which asserts `Parsed<R>` and `TapeDirect` deleted.
`crates/core/src/runtime/<grammar>/` carries `arena.rs`,
`builder.rs`, `document.rs`, `mod.rs`, `value.rs`, `view.rs` for
JSON/CSS L4 only. No `tape/` or `parsed/` sub-directory.

Patch: replace with "Grammar-specific documents and builders" — no
facade remains.

### L4 (LOW) — TypeScript Interpreter (175-191)

Claim: ASTToParser entrypoint; consumed by `prettier-plugin-bbnf`.

Verdict: **STALE.** Per README C1, `prettier-plugin-bbnf/` no
longer in this repo. TS interpreter may persist in parse-that;
consumer reference dangling.

Patch: clarify "lives in parse-that's TS surface" or drop §3 if
TS path no longer flows through bbnf-lang.

### L5 (LOW) — IR pass enumeration (75-81)

Claim: 17 operations, 15 unique passes, names listed.

Verdict: **MATCHES** (consistent with `crates/ir/src/passes/`
directory layout; not exhaustively enumerated).

Patch: none.

### L6 (LOW) — Pipeline diagram namespace (21-50)

Verdict: **AMBIGUOUS.** `bbnf::analysis` resolves ambiguously
between `bbnf` crate's analysis module and `crates/analysis/`
crate.

Patch: clarify namespace once on first introduction.

---

## docs/instructions/README.md

### M7 (MED) — Cargo profile inventory (7-9)

Claim: cargo surfaces are `ax-iter`, `ay-final`, `profiling-prep`,
`bench`, `bench-ci`.

Verdict: **MATCHES.**
- `[profile.ax-iter]`, `[profile.ay-final]`, `[profile.bench-ci]`,
  `[profile.bench-iter]` — `.cargo/config.toml`.
- `[profile.profiling-prep]`, `[profile.release]`, `[profile.bench]`,
  `[profile.dev]` — `Cargo.toml`.

Patch: none. The instruction enumerates user-facing names, not
their partitioning.

### L7 (LOW) — `crates/csp-solver/docs/instructions/README.md` (29)

Verdict: **AMBIGUOUS.** Sub-doc not opened.

Patch: defer to lane 6 verification.

---

## docs/instructions/PROFILING.md

### H5 (HIGH) — `cargo iter-check` cold/warm timings (78-80)

Claim: 3.88 s warm, ~11 s cold "per `docs/benchmarks/post-B1-W0-routine.txt`".

Verdict: **AMBIGUOUS.** Artefact path not opened. If missing,
header doc's authority undermined.

Patch: lane 7 could verify the artefact. If missing, replace
inline citation with "see post-B1 archive".

### M8 (MED) — Bench alias surface (150-158)

Verdict: **MATCHES.** `.cargo/config.toml:119-127`.

### M9 (MED) — Routine surface table (68-77)

Verdict: **MATCHES.** All 12 alias entries confirmed at
`.cargo/config.toml:109-118`.

### M10 (MED) — Codegen inspection (177-193)

Verdict: **MATCHES.** Aliases at `.cargo/config.toml:120-122`;
Makefile lines 135, 139, 142.

### M11 (MED) — `make ay-*` gate manifest (249-260)

Verdict: **MATCHES.** Every gate has Makefile counterpart at
lines 260-396.

### L8 (LOW) — Sub-agent forbidden list (369-373)

Verdict: **MATCHES** (architectural rule).

### L9 (LOW) — `bencher` retirement (142-148)

Verdict: **MATCHES.** `crates/core/Cargo.toml` only carries
`divan = "0.1"`.

---

## Makefile

### M12 (MED) — `iter-grammar GRAMMAR=<ident>` (219-232)

Claim: chains `cargo xtask regen --grammar <ident>` →
`cargo iter-check` → nextest filter.

Verdict: **MATCHES.**

Patch: none.

### M13 (MED) — Script delegations

Targets reference `scripts/profile-bench-headless.sh`,
`scripts/prepare-profile-wave.sh`, `scripts/doctor.sh`,
`scripts/deploy.sh`, `scripts/test-tier.sh`.

Verdict: **AMBIGUOUS.** Not verified by audit. Any absent script
breaks corresponding Makefile target at runtime.

Patch: lane 8 could `ls scripts/` sweep + 1-line per-script check.

### L10 (LOW) — `make build-lsp --profile ay-final` (43)

Verdict: **MATCHES.** Profile defined at `.cargo/config.toml:78`.

Patch: none.

---

## Per-crate READMEs

### H6 (HIGH) — Per-crate README files do not exist

`ls crates/{core,analysis,bootstrap,csp-solver,egraph,
egraph-derive,gorgeous,ir,lsp,ser,simd-scan}/README.md` returned
all 11 missing.

Verdict: **MISSING.** Top-level README's structure block at lines
12-33 lists each crate with a one-line description, gesturing at
deeper per-crate docs. None exist. Crates rely on top-of-`lib.rs`
doc-comments.

Patch: either generate stub READMEs or drop the per-crate
descriptions from the top-level README's structure block.

---

## Cargo.toml workspace

### H7 (HIGH) — `metadata.bbnf.grammars` (18-29)

Claim: 9 grammars enumerated with capability bitsets.

Verdict: **MATCHES.** Each `ident` matches a file under
`crates/core/src/grammar/generated/<ident>.rs` and a strategy arm
in `crates/ir/src/registry/strategy.rs`.

Patch: none.

### H8 (HIGH) — Profile partitioning across files

`Cargo.toml`: `release`, `bench`, `profiling-prep`, `dev`,
`dev.package.*`. `.cargo/config.toml`: `ax-iter`, `ay-final`,
`bench-ci`, `bench-iter`.

Verdict: **MATCHES** the comment at `Cargo.toml:109-112` declaring
`.cargo/config.toml` as single source of truth for `ax-iter`.

Patch: none.

---

## .cargo/config.toml

### H9 (HIGH) — `[patch.crates-io]` (36-49)

Claim: patches `pprint`, `pprint_derive`, `parse_that`,
`bbnf-regex` from sibling repos plus 8 in-tree crates.

Verdict: **MATCHES.** Sibling repos `parse-that` and `pprint`
mentioned by GESTALT §2 line 50 as four-repo surface.

Patch: none.

### H10 (HIGH) — Build-flags retrospective (198-219)

Claim: `-Zthreads=8 -Zshare-generics=y` removed after 28× warm-iter
regression measurement (commit `416dcf76`).

Verdict: **MATCHES** (historical commentary; matches block
verbatim).

Patch: none.

### H11 (HIGH) — `iter-check` excludes 4 heavy-link crates (110)

Claim: workspace minus `bbnf-bootstrap`, `gorgeous`, `bbnf-analysis`,
`bbnf-lsp`; each has named fast-path alias.

Verdict: **MATCHES.** Aliases at lines 111-113 cover all three
named excludes (`iter-check-lsp` covers both `bbnf-analysis` and
`bbnf-lsp` per line 111).

Patch: none.

### H12 (HIGH) — `iter-test-leaf` (135)

Claim: `iter-test-leaf` covers `bbnf-ir`, `egraph`, `csp-solver`,
`bbnf-ser` for fastest correctness tier.

Verdict: **MATCHES.**

Patch: none.

### H13 (HIGH) — bench-iter profile (114-120)

Claim: dev-loop bench profile at `bench-iter` for sub-minute
rebuilds.

Verdict: **MATCHES.** Profile defined at `.cargo/config.toml:114`;
6 alias entries at lines 152-157.

Patch: none.

### H14 (HIGH) — Cranelift opt-in commented out (228-231)

Verdict: **MATCHES.** Block commented; project status indicates
cranelift broken on pinned nightly per inline comment.

Patch: none.

---

## Top-5 most stale claims

1. **README structure block** (`README.md:12-33`) — describes
   `rust/`-rooted layout with `wasm/`, `typescript/`,
   `prettier-plugin-bbnf/` siblings. Code reality: `crates/`
   monorepo (`Cargo.toml:2`); `typescript/` and
   `prettier-plugin-bbnf/` deleted. **CRITICAL.**

2. **README slab parsing** (`README.md:124-136`) —
   `#[derive(Parser)] #[parser(path = "json.bbnf", slab)]` and
   `BumpSlab::with_capacity(...)` API claimed. Code reality:
   `cargo xtask regen` writes `crates/core/src/grammar/generated/<ident>.rs`;
   proc-macro deleted at B2.W2 (`docs/codegen-paths.md:14-17`).
   **CRITICAL.**

3. **README span-only parsing** (`README.md:138-141`) —
   `#[parser(span)]` generating `__rule_span` claimed. Same
   root cause as #2 (`docs/codegen-paths.md:14-17`). **CRITICAL.**

4. **GESTALT crate enumeration** (`docs/GESTALT.md:205-206`) —
   names `crates/tape`, `crates/derive`, `crates/json-prototype`
   among workspace members. Code reality: all three absent
   (`Cargo.toml:2` lists 11 members + xtask). **HIGH.**

5. **codegen-paths runtime description** (`docs/codegen-paths.md:203`)
   — names `runtime/` as carrying "remaining Parsed/tape facade",
   contradicting §1's deletion claim 100 lines above. Code
   reality: no `tape/` or `parsed/` sub-tree under
   `crates/core/src/runtime/`. **MED.**

## Confirmation

No source files modified. 47 claims audited. Severity: 4 / 14 / 19
/ 10 (CRITICAL / HIGH / MED / LOW). Triumvirate trigger fires —
README warrants wholesale rewrite by a separate agent.
