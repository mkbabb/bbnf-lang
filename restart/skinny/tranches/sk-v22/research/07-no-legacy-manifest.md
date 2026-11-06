# SK-V22 Research 07 — The Cross-Repo No-Legacy Manifest

Date: 2026-07-09. Scope: the binding R4c charter correction — parse-that (Rust + TS) is IN scope
for a full SOTA + legacy/deprecated uplift, and the graduating skinny tree carries a not-yet-ledgered
legacy delta. This report sweeps BOTH repos and produces a per-item manifest: every row names the
item, the file:line evidence, a **gestalt transposition** (an architectural replacement — never a
patch, never a carried deprecation), the wave that owns it, and the gate that certifies it.

Discipline anchors: parse-that TS is a constellation backbone (value.js / latex-paper / keyframes);
its consumer suites and semver are the close gates. parse-that Rust genuinely rides nightly and
forked skinny's scanners. NO cargo/npm builds were run for this audit — findings are static +
cross-referenced against the SK-V21 ledgers so only NEW items are raised for skinny.

---

## Part 0 — Method and the "only-new" cross-check

The SK-V21 `DEFERRED-LEDGER.md` already dispositions the large skinny legacy items. I cross-checked
every candidate against it before raising it:

- **N12** (DEFERRED-LEDGER:72) — inline `#[cfg(test)]` in src/, "7 core + 21+ skinny", → TW0 W0.7.
  Raised here ONLY as a corrected census (the count is materially larger — see S-1).
- **N26** (DEFERRED-LEDGER:91) — the Rust edition/toolchain axis, five edits priced, → TW-S. The
  skinny `edition = "2021"` / `rust-version = "1.78"` lines are this item; confirmed still
  un-committed in the tree of record but NOT re-raised as new (see S-2, disposition = already-owned).
- **N7** (DEFERRED-LEDGER:67) + ADDENDUM:19 — smallbox[nightly] via the legacy `crates/` tree,
  MOOT at cutover. Not re-raised.

Everything below is either NEW (not in the ledgers) or a corrected magnitude of a ledgered row.

---

## Part 1 — parse-that Rust tree (`/Users/mkbabb/Programming/parse-that/rust`)

The headline correction: the SK-V21 pricing (PROTO-A3 D2) framed **smallbox's `nightly` Cargo
feature as "the SOLE nightly-feature enabler."** That is true for *Cargo features named nightly* and
FALSE for *raw `#![feature(...)]` lang gates*. parse_that's own `lib.rs` opens with three of them —
so de-nightlying parse-that Rust is a real engineering task, not a one-line Cargo edit, and it is
independent of the smallbox question.

| # | Item | Evidence (file:line) | Gestalt transposition | Wave | Gate |
|---|------|---------------------|------------------------|------|------|
| R-1 | parse_that intrinsically nightly via `#![feature(portable_simd)]`, `#![feature(fmt_helpers_for_derive)]`, `#![feature(stmt_expr_attributes)]` | `rust/parse_that/src/lib.rs:1-3` | Re-found the SIMD scanners on **stable `core::arch` intrinsics** behind a runtime-`is_*_feature_detected!` dispatch (skinny's `bbnf-simd` NEON pattern is the template); drop `fmt_helpers_for_derive` (hand-write the `Debug`/`Display` it accelerates) and `stmt_expr_attributes` (hoist the attribute to an item). de-nightly is then whole, not smallbox-gated. | TW-X.R (cross-repo uplift, Rust) | `cargo +stable build -p parse_that` EXIT 0; scanner bench parity vs the nightly baseline within the load-robust ratio band |
| R-2 | smallbox[nightly] `coerce` feature — the Cargo-feature nightly enabler | `rust/parse_that/Cargo.toml:97` (`smallbox = { …, features=["nightly"] }`); confirmed sole Cargo-feature enabler, PROTO-A3 D2 | Drop the `nightly` feature; replace the one `SmallBox<_, S32>` unsizing site with an explicit `enum` dispatch (arena/enum-over-boxing — matches the memory precept and the R-1 dispatch spine). Retires the last Cargo-feature nightly edge. | TW-X.R | `cargo tree -e features` shows zero `nightly` feature; box-elimination bench neutral-or-better |
| R-3 | **rust-toolchain.toml is a copy-paste of the bbnf-lang workspace pin** — it cites cranelift, `-Zshare-generics`, and the bbnf-lang `on_disk_cache.rs:663` ICE reproducer, none of which are parse-that's | `rust-toolchain.toml:2` ("bbnf-lang workspace pin"), body through :39; `channel = "nightly-2026-04-11"` | After R-1/R-2 land, **delete the pin** (parse-that becomes stable-clean) OR, if a pin is retained for a bench channel, rewrite the comment block to describe parse-that's own reason. No carried cross-repo mis-attribution. | TW-X.R | File absent (stable) or its prose references only parse-that facts; `grep -c 'bbnf-lang' rust-toolchain.toml == 0` |
| R-4 | **Dual combinator stacks** — `combinators/` (10 files, `methods/`) AND `span_parser/` (10 files, 1657 LOC) both `pub` + glob-re-exported; the CSS/JSON parsers lean on `SpanParser`, the base methods live in `combinators/` | `rust/parse_that/src/lib.rs:38` (`pub mod combinators`) + `:41` (`pub mod span_parser; pub use span_parser::*`); consumers `parsers/json.rs`, `parsers/css/{mod,selector,declaration}.rs`, `parsers/scan/mod.rs` | The SOTA combinator audit's core decision: **unify onto one span-first combinator surface**. TS already proved the tagged-union runtime-switch is a perf loss (future-research §7, KILLED) — so the transposition is a single monomorphized `Parser` whose methods return spans, allocation-minimized (exploit LLVM: enum dispatch over boxing, `#[inline]` on the hot methods). One stack, not two. | TW-X.R | One combinator module remains; `combinator_test.rs` + `css_parse_test.rs` green; allocation count per parse ≤ current on the CSS/JSON corpora |
| R-5 | **Orphaned bootstrap crate** — `regex-bootstrap` is a "thin re-export" whose sole purpose is preserving the `regex_bootstrap::*` bench name after the `bbnf_derive` proc-macro edge was retired; its `regex_parse` bench duplicates `rust/regex/benches/regex_parse.rs` | `rust/bootstrap/Cargo.toml:1-30` (self-described re-export); duplicate bench `rust/bootstrap/benches/regex_parse.rs` vs `rust/regex/benches/regex_parse.rs` | **Delete the crate**; move the one bench to `rust/regex/benches/` under a distinct name (or drop, since regex already benches parse). No vestigial re-export member in the workspace. | TW-X.R | `bootstrap/` gone from `rust/Cargo.toml` members; `cargo bench -p bbnf-regex` still exercises the parse path |
| R-6 | **Stale hand-timed CLI** — `parse_that_cli` is a 33-line `SystemTime::now()` micro-timer with hard-coded `../../data/...` relative paths; superseded by the divan bench tier | `rust/src/main.rs:1-33` | Delete the crate; the divan benches (`benches/parse_that/*`) and competitor suite are the measurement surface. Removes a warm-timing artifact that contradicts the cold-bench precept. | TW-X.R | `src/` gone from workspace members; benches remain the only timing entry point |
| R-7 | **Cross-repo `.cargo/config.toml` patch coupling** — parse-that's `bbnf-regex` resolves `egraph`, `egraph-derive` from `bbnf-lang/crates/` and `csp-solver` from `csc411/`; the config also patches `bbnf = crates/core` (the tree SK-V22 archives) | `rust/.cargo/config.toml:8` (`bbnf = …/crates/core`), `:15` (csp-solver → csc411), `:17-18` (egraph/egraph-derive → bbnf-lang/crates) | The archival of bbnf-lang `crates/` **breaks this patch table.** Transposition: the graduation re-homes `egraph`/`egraph-derive` to a durable general-infra crate home (memory precept: e-graphs live in own crate(s)), and both repos' configs re-point there. The dead `bbnf = crates/core` patch line is deleted (parse-that has no live `bbnf` dep). | TW-X.R (coordinated with the archival wave) | parse-that `bbnf-regex` builds after `crates/` archives; zero patch lines pointing into an archived path |

---

## Part 2 — parse-that TypeScript tree (`/Users/mkbabb/Programming/parse-that/typescript`)

The 1.0.0 `*Span` excision (commit `043c4d1`) is **complete and self-gated**: `grep` for
`altSpan|manySpan|regexSpan|stringSpan` over `typescript/src` returns **nothing**, and
`proof:no-span-surface` + `proof:no-dead-combinator` (the `thenMap`/`fuse` DELETE, `proof:all`)
enforce it in CI. The surviving `Span` type / `mergeSpans` / `spanToString` in `state.ts` are the
**diagnostic** offset-pair (furthest-error machinery), a legitimate live surface — NOT SpanParser
residue. So the TS SOURCE tree is clean; the TS legacy delta is entirely in **docs and the open
feature backlog**.

| # | Item | Evidence (file:line) | Gestalt transposition | Wave | Gate |
|---|------|---------------------|------------------------|------|------|
| T-1 | **Stale playground doc teaches a fully-excised API** — `span-combinators.md` imports `stringSpan, regexSpan, manySpan, sepBySpan, wrapSpan, optSpan, skipSpan, nextSpan` from `@mkbabb/parse-that`; none exist post-1.0.0 (only `mergeSpans`/`spanToString` survive) | `docs/playground/span-combinators.md:12-16`; excision confirmed by empty grep + future-research.md:92-107 | **Delete the doc** (the API is retired, KILLED per future-research §7 with a do-not-reattempt note). If a zero-copy-span narrative is retained, rewrite it against the diagnostic `Span` only. A playground page that imports non-existent exports is a live falsehood. | TW-X.T (cross-repo uplift, TS) | `proof:no-span-surface` extended to grep `docs/` for excised names; doc gone or import-valid |
| T-2 | PT-E1 (scoped diagnostics) + PT-E2 (inference leak) are open toward 1.1.0; the VALUEJS-PT-E ask letter (2026-07-05, git HEAD) is the live coordination record | `docs/tranches/A/VALUEJS-PT-E-2026-07-05.md`; package `version: 1.0.0` (`typescript/package.json:3`) | These are **feature work, not legacy** — flagged so synthesis does not mistake them for prunable surface. The uplift executes them to 1.1.0 with value.js's suite as the close gate (semver discipline). | TW-X.T | value.js + keyframes consumer suites green at 1.1.0; `proof:all` green |
| T-3 | Config/tooling currency check — `tsconfig` target ES2022 / `moduleResolution: bundler` / `verbatimModuleSyntax`; vite 7, vitest 3, typescript 5.8 | `typescript/tsconfig.json`; `typescript/package.json:devDependencies` | **No transposition needed — this is current 2026 SOTA** and is recorded as a NEGATIVE finding (audited, clean) so no agent re-opens it. The competitor devDeps (arcsecond/chevrotain/moo/nearley/ohm-js/parjs/parsimmon/peggy) are legitimate bench baselines, not dead deps. | TW-X.T (audit-close note) | n/a — recorded clean |

---

## Part 3 — The graduating skinny tree (`/Users/mkbabb/Programming/bbnf-lang/skinny`)

Only NEW items or corrected magnitudes of ledgered rows (Part 0 cross-check). Findings are lean:
skinny is the survivor tree, so its legacy surface is small and mostly already-owned.

| # | Item | Evidence (file:line) | Gestalt transposition | Wave | Gate |
|---|------|---------------------|------------------------|------|------|
| S-1 | **Inline `#[cfg(test)]` census is materially larger than ledgered** — N12 estimated "21+ skinny"; the actual count is **75 inline test blocks across 34 src files** (spanning `codegen`, `runtime`, `passes`, `ir`, `grammar`, `parse-that-regex`, `bbnf-regex`, and the whole `bbnf-bench` tree) | `grep -c '#\[cfg(test)\]' skinny/crates skinny/xtask` = 75 / 34 files; ledger row DEFERRED-LEDGER:72 | Honors the existing N12 disposition but **re-scopes W0.7 to the true magnitude**: all 75 blocks relocate to `tests/` per the no-inline-tests precept; the four `perf_cost` sites the ledger named are a subset. Not a new decision — a corrected budget for the owning wave. | TW0 W0.7 (re-scoped) | Zero `#[cfg(test)]` under any `src/`; suite green; the 34-file list is the checklist |
| S-2 | Tree-of-record still carries `edition = "2021"` / `rust-version = "1.78"` — the PROTO-A3 five-edit migration was worktree-only, never committed | `skinny/Cargo.toml:19`, `:22` | **Already owned by N26 → TW-S.1** (edition 2024 + pinned-stable-1.96.1). Recorded here as state-confirmation only: the migration must be re-applied and COMMITTED to the graduated tree, not left in a discarded worktree. No new disposition. | TW-S.1 (already-owned) | `skinny/Cargo.toml` shows edition 2024 / rust-version 1.88 committed; whole-repo stable build |
| S-3 | **Graduation-blocking dep coupling** — skinny's own workspace depends on `egraph` and `csp-solver` via `path = "../crates/egraph"` / `"../crates/csp-solver"`, i.e. INTO the bbnf-lang `crates/` tree that SK-V22 archives | `skinny/Cargo.toml:36-37` | This is the same coupling as R-7 from the other side: **`egraph`, `egraph-derive`, `csp-solver` must survive the archival** and re-home into the graduated tree (general-infra crates per the memory precept; csp-solver's canonical home is csc411). The archival wave cannot delete `crates/` until these three relocate. A hard ordering, not a patch. | TW2 / archival wave (ordering constraint) | Graduated skinny builds with no `path` dep reaching into an archived directory |
| S-4 | Commented-out code / stale decision-carrying TODOs — effectively ZERO: one `^// let/fn/…`-shaped line repo-wide; the TODO hits are `\uXXXX` identifiers and grammar-authored "typed leaves deferred to Span" notes that the ledgers already own | `grep` = 1 commented-code line across `crates/*/src` + `xtask/src`; TODO refs at `runtime/tests/sheets_conformance.rs:8`, `codegen/src/general_descent/mod.rs:24` | **Recorded clean** — no dead commented blocks, no orphan decision-TODOs. The two grammar TODOs are typed-leaf precision items already tracked as grammar work, not prunable legacy. Prevents a re-audit of skinny's comment surface. | n/a — audit-close note | n/a — recorded clean |

---

## Part 4 — Synthesis: the three that actually move the graduation

Most rows are hygiene. Three are load-bearing and belong in the SPEC's critical path:

1. **R-1 (parse_that is portable_simd-nightly, not smallbox-nightly).** The SK-V21 de-nightly
   pricing is INCOMPLETE. de-nightlying parse-that Rust means re-founding the SIMD scanners on
   stable `core::arch` — a genuine wave of work with a runtime-dispatch spine (the skinny
   `bbnf-simd` template), NOT the one-line Cargo edit PROTO-A3 D2 implied. This is the single
   most under-scoped item in the cross-repo uplift.

2. **R-7 + S-3 (the egraph/csp-solver survival coupling).** BOTH the survivor tree (skinny) and
   the backbone (parse-that/bbnf-regex) resolve `egraph`/`egraph-derive`/`csp-solver` via `path`
   into bbnf-lang `crates/` — the exact directory the great archival deletes. The archival wave is
   BLOCKED until these three general-infra crates re-home. This is a hard sequencing edge the
   archive-mechanics wave must encode, else the graduated tree and the backbone both fail to build.

3. **R-4 (the dual Rust combinator stack).** The charter's "combinators: minimize allocations,
   exploit the compilers" mandate has a concrete target: `combinators/` + `span_parser/` are two
   surfaces for one job. TS already ran and KILLED the runtime-switch experiment — that negative
   result (future-research §7) is the evidence that the Rust unification should be a single
   monomorphized span-first stack, not a tagged union.

## Open questions for synthesis to decide

- **The de-nightly SIMD path (R-1):** stable `core::arch` intrinsics (most control, most code) vs a
  `std::simd`-shim crate (`wide`) — which does the uplift adopt, and does it share the kernel with
  skinny's `bbnf-simd` rather than maintaining a second SIMD tree?
- **egraph/csp-solver home (R-7/S-3):** own repo, a `crates-infra/` in the graduated tree, or
  publish to registry — the archival wave needs the target BEFORE it can delete `crates/`. Note the
  csp-solver double-home (skinny→`bbnf-lang/crates/csp-solver`, parse-that→`csc411/…/csp-solver`)
  implies a copy that must be reconciled to one source.
- **Wave letters:** this manifest proposed `TW-X.R` / `TW-X.T` for the parse-that cross-repo uplift
  track; the SPEC must ratify the actual letters and slot them against TW0–TW6 + TW-S.
