# parse-that refresh — the fold-in/fold-out census (2026-07-05)

Scope: the sibling repository at `/Users/mkbabb/Programming/parse-that`, both its Rust and
TypeScript trees, re-audited against the state that bbnf totality must consume, duplicate,
or reject. This supersedes the prior refresh, which predates the 1.0.0 TypeScript cut and
the value.js coordination letter.

---

## 0. Headline

The refresh splits cleanly along the two trees, and they moved at very different tempos:

- **The Rust tree is frozen.** Its last commit touching `rust/` is `051a6d6`
  (`test(bbnf-regex/path-lexer)…`, 2026-05-02). Nothing under `rust/` has changed in the
  window since 2026-06-01. Every "1.0.0" event is TypeScript-only.
- **The TypeScript tree cut a breaking 1.0.0** on 2026-07-03 (`7eab78c`, tag `v1.0.0`):
  the CSS parser removed, the `*Span` combinator surface excised, the SpanParser
  tagged-union tier permanently killed, packrat/WDM left-recursion armed behind a
  `PACKRAT_ARMED` latch, and `chain()` falsy-seed threading fixed. It is now a pure
  primitive-combinator library.
- A **coordination letter** (`docs/tranches/A/VALUEJS-PT-E-2026-07-05.md`) arrived
  2026-07-05 from the value.js Tranche S orchestrator. It carries **no defect asks** —
  three 1.1.0+ items, one of which (Pratt-dormant) is a record rather than an ask. It is
  addressed to parse-that, not to bbnf, but it exposes one real cross-repo obligation on
  bbnf (§3).

The single most consequential structural fact for bbnf: **the canonical `bbnf-regex`
engine lives in parse-that, and bbnf totality already consumes it** — while the skinny
workspace ships a *second, incompatible* crate also named `bbnf-regex`. That is SK-V20 N7,
and it is a live collision, not a naming nit (§2.3).

---

## 1. Git log since 2026-06-01 — what changed and why (all TypeScript)

Reconstructed from `git log`. The arc is a deliberate walk from a CSS-carrying combinator
library to a pure primitive set, culminating in a breaking 1.0.0:

- `508aa6b` PT-WAVE-1 — thread furthest-offset / expected-set / diagnostics onto
  `ParserState` (the diagnostics substrate that PT-E1 later asks to finish, §3).
- `c9338e4` PT-WAVE-2 — isolate the dead packrat into an opt-in module; strip the
  per-parse reset tax.
- `393abb8` Tranche A charter — "pure primitives."
- `7cb7a51` A.W0 (0.9.1) — manifest hygiene (`typesVersions` purged, `sideEffects:false`).
- `c86a149` A.W1 (0.10.0) — **CSS-parser removal**: `src/parse/parsers/css/**` deleted
  (~3369 lines removed). parse-that becomes pure primitives; bbnf owns CSS.
- `193854d` A.W2 — the **packrat soundness fix**: `(id, offset)`-keyed left recursion.
- `3b559a9` A.W3 — subpath split + a SpanParser tagged-union dispatch prototype…
- `afea5c2` …whose perf hypothesis was **FALSIFIED** (retired from public surface, 0.11.0).
- `7901314` Tranche B (0.12.0) — packrat cross-input fix (a BLOCKER), combinator fusion,
  **SpanParser KILL**.
- `2c806fb` Tranche Q (0.13.0) — packrat re-entrancy + the >1 MB memo-key fix; `thenMap`/
  `fuse` delete; `subTable` retract.
- `934b2fa` S.H1 — **arm the packrat epoch behind `PACKRAT_ARMED`** (zero packrat Map
  allocation until the first `memoize()`/`mergeMemos()` construction).
- `043c4d1` S.H2 — the 1.0.0 legacy cut: **excise `*Span`** (`src/parse/span.ts`, 591 LOC,
  deleted) + fix `chain()` falsy seed (C-16).
- `7eab78c` S.H4 — cut 1.0.0; ledger closure; recorded non-goals.
- `ef10d5b` — the VALUEJS-PT-E ask letter.

Why it matters to bbnf: any SK-V20-era claim that parse-that ships CSS, a `*Span`
builder surface, or a SpanParser tier is now **false**. Report 17's prior premises about
those surfaces are stale.

---

## 2. The Rust tree — engine, editions, deps, what bbnf consumes vs duplicates

### 2.1 Workspace shape and editions (ahead of skinny)

`rust/Cargo.toml` is a 4-member workspace (`src`, `parse_that`, `bootstrap`, `regex`),
`resolver = "2"`. **Every member is `edition = "2024"`** — `parse_that` (v0.4.0,
`rust/parse_that/Cargo.toml:5`), `bbnf-regex` (v0.1.0, `rust/regex/Cargo.toml:5`),
`regex-bootstrap` (v0.1.1, edition 2024). parse-that's Rust tree is therefore **already
where the SK-V21 stack-abrogation wants bbnf to be** (the skinny holdout is edition 2021,
`skinny/Cargo.toml:18`). This is a fold-in opportunity, not a fold-in cost.

The toolchain pin is a hazard: `rust-toolchain.toml` at the parse-that root is a
same-date twin of bbnf-lang's pin — `channel = "nightly-2026-04-11"`, the same ~12-week
stale nightly the charter condemns. `rust/.cargo/config.toml` and the repo-root
`.cargo/config.toml` both state they mirror bbnf-lang's toolchain posture (rustflags
`-Zthreads=8 -Zshare-generics=y`, and the `ax-iter`/`ay-final`/`bench-ci` profile trio).
**Any bbnf abrogation to a fresh nightly must be mirrored into parse-that in the same
motion**, or the bidirectional path-patched build (§2.4) will split-brain on toolchain.

### 2.2 `bbnf-regex` (rust/regex) — the canonical bespoke engine

`rust/regex/`, package `bbnf-regex`, lib `bbnf_regex`, v0.1.0, edition 2024. Per its own
`rust/regex/CLAUDE.md:1-12`, it is "the pure regex engine used by parse-that and bbnf …
Used by `parse_that` (re-exports the engine), by `bbnf` (DFA codegen + regex
classification + structural facts), and by `bbnf-ir` (FIRST sets + the shared
`CharSet128` substrate)." A leaf crate with zero parser-combinator/grammar-IR deps.

Contents (`rust/regex/src/lib.rs:14-38` re-exports; module tree per CLAUDE):
- **HIR + hand-written parser** (`hir/mod.rs` 277 LOC; `hir/parser/{cursor,atom,repetition,
  group,class,escape,mod}.rs`) — recursive descent, split per Tranche T.
- **automata**: Thompson NFA (`nfa.rs` 449 LOC), NFA→DFA powerset + Hopcroft minimization +
  flat transition table (`dfa.rs` 438 LOC), single-byte accel (`accel.rs`).
- **classify** (`classify.rs` 1148 LOC) — `RegexClass`, `classify_regex[_from_hir]`.
- **sets** — `ByteSet` (256-bit), `CharSet128` (128-bit ASCII), byte-equivalence classes.
- **info** — unified analysis: nullable/width, literal-prefix/suffix, one-pass eligibility,
  DFA-size estimate.
- **first** — conservative FIRST-char extraction.
- **algebra** — ByteSet superset/union.
- **path_lexer** — bespoke path-string lexer (Span-carrying, for a `path!` proc-macro).
- **egraph** (`egraph/mod.rs` 200 LOC + `node/translate/cost/rules/*`) — an **HIR-tier
  equality-saturation e-graph** whose rewrite eligibility is scheduled by `csp-solver`,
  "the same way the grammar tier schedules its CSP passes" (`rust/regex/Cargo.toml:38-44`).

Critically, `bbnf-regex` depends on `egraph = "0.1"`, `egraph-derive = "0.1"`, and
`csp-solver = "0.1"` (`rust/regex/Cargo.toml:46-48`), plus `smallvec 1.13`, `proc-macro2 1`,
`rustc-hash 2`. Those e-graph/CSP crates are **the same csc411/bbnf-lang substrate** — the
architectural isomorphism bbnf keeps asking for already exists inside this crate.

### 2.3 The N7 collision — two crates named `bbnf-regex`, both v0.1.0, incompatible

There are two distinct crates named `bbnf-regex`, version `0.1.0`:

| | parse-that `rust/regex` | skinny `skinny/crates/bbnf-regex` |
|---|---|---|
| Size | ~2686+ LOC across HIR/NFA/DFA/classify/info/egraph | **322 LOC, single `src/lib.rs`** |
| Deps | egraph, egraph-derive, csp-solver, smallvec, proc-macro2, rustc-hash | **none** (`skinny/crates/bbnf-regex/Cargo.toml`) |
| API | `parse()`, `classify_regex()`, `Dfa::compile`, `RegexInfo`, `CharSet128`, FIRST sets | `analyze(&str) -> RegexFacts` with a 4-kind `RegexKind` (Whitespace / QuotedString / Numeric / Unknown) |
| Consumers | bbnf totality `crates/ir` + `crates/core` | skinny `crates/ir` + `crates/passes` |

The skinny variant is a coarse placeholder — skinny's own codegen admits it: "the runtime
has no general regex matcher, and the `bbnf-regex` HIR is a coarse [approximation]"
(`skinny/crates/codegen/src/general_descent/mod.rs:16`,
`.../regex_first.rs:3`). Its `analyze()` returns a nullable flag, a FIRST byte-set, byte
classes, and a `StringFacts { delimiter, escape }` — nothing resembling a DFA or a real
HIR.

Because the two live in **separate Cargo workspaces** (skinny is its own `[workspace]`,
`skinny/Cargo.toml:1`), they do not collide at build time *today*. But they collide the
instant skinny is folded into totality, and they collide *semantically* now: `bbnf-regex`
means two different things depending on which tree you read. On the skinny→totality fold,
**the skinny 322-LOC `bbnf-regex` must be deleted, not renamed** — totality already consumes
the real engine (§2.4).

There is a parallel duplication: skinny also ships `crates/parse-that-regex` (v0.1.0, 3022
LOC across `number/eisel_lemire/**`, `integration`, `unicode`;
`skinny/crates/parse-that-regex/Cargo.toml`, `.../src/number/mod.rs`). Its Eisel-Lemire
fast-float and quoted-string scanning **reimplement** `parse_that`'s own number fastpath and
`scanners.rs` SIMD lane (`rust/parse_that/src/scanners.rs` 370 LOC; parse-that's
`number_fastpath_test.rs`, `quoted_simd_test.rs`). Same substrate, two authors.

### 2.4 What bbnf currently consumes vs duplicates

**Totality (`crates/`) consumes the real thing — no duplication there.** The root
workspace (`Cargo.toml:2`, members `crates/core`, `crates/ir`, …) resolves parse-that via
`.cargo/config.toml:51-52`:
```
parse_that = { path = "../parse-that/rust/parse_that" }
bbnf-regex = { path = "../parse-that/rust/regex" }
```
and declares the deps in `crates/ir/Cargo.toml:13-14` (`parse_that = "0.4"`,
`bbnf-regex = { version = "0.1", features = ["serde"] }`) and `crates/core/Cargo.toml:15`
(`parse_that = "0.4"`, "built on parse_that").

**The duplication is entirely inside skinny**, which forks both crates (§2.3). So the
fold-out story is asymmetric: totality is already clean; skinny is the offender.

**The coupling is bidirectional and circular by path-patch.** parse-that's regex crate
depends on bbnf-lang/csc411's e-graph substrate: `rust/.cargo/config.toml` patches
`egraph`, `egraph-derive` → `../../bbnf-lang/crates/egraph[-derive]`, `csp-solver` →
`../../csc411/…/csp-solver`, and even `bbnf`/`bbnf-ir`/`bbnf-ser` → `../../bbnf-lang/crates/*`.
Meanwhile bbnf's `crates/ir` depends on parse-that's `bbnf-regex`. This mutual
path-patching works only because everything is edition-2024 on the same nightly — which is
exactly why the toolchain mirror (§2.1) is load-bearing.

### 2.5 The self-hosted regex parser (distinct from the engine)

Do not conflate `bbnf-regex` (the engine) with `parse_that::regex` (a module inside the
combinator crate). `rust/parse_that/src/regex/mod.rs:1-6` re-exports `bbnf_regex::*` and adds
a **self-hosted bootstrap path**: `generated.rs` (2293 LOC, emitted from `regex.bbnf`) +
`host.rs` (524 LOC fold to `Hir`), exposed as `parse_generated()`. `regex-bootstrap`
(v0.1.1) is now a thin re-export of `parse_that::regex::generated` since bbnf-lang's
`crates/derive` proc-macro was retired at B2 (`6142387f`;
`rust/bootstrap/Cargo.toml`). This is bbnf's **proof of self-hosting** — the grammar is
bbnf's, the generated parser proves the toolchain round-trips — and it should stay as
parse-that's bench/verification surface, with bbnf owning `regex.bbnf`.

---

## 3. The TypeScript tree and the value.js ask — obligations and opportunities

The TS package (`@mkbabb/parse-that@1.0.0`, `typescript/package.json:3`) is a pure
primitive set: `Parser<T>` + combinators (`parser.ts` 711 LOC), leaf parsers
(string/regex/eof/any/all/dispatch/whitespace, `leaf.ts` 399 LOC), opt-in packrat/WDM
behind `PACKRAT_ARMED` (`packrat.ts` 488 LOC), balanced split, and a diagnostics surface
(`utils.ts`, `debug.ts`). Subpath exports `. / core / diagnostics / packrat / utils` are
"the zone map" (`docs/future-research.md:206-208`). No CSS, no `*Span` builders.

The coordination letter (`VALUEJS-PT-E-2026-07-05.md`) is addressed to parse-that and
gates **no** parse-that wave. Its three items:

- **PT-E1 (HIGH)** — scoped per-parse diagnostics; cure the structurally-dead
  `ParseDiagnostic.expected` field. value.js authors correct parse-error messages that
  today cannot reach any consumer; when PT-E1 lands, value.js's messages flow through with
  no value.js change (`§the-asks`, `§the value.js-side half`).
- **PT-E2 (MED)** — combinator-inference tightening: the `Parser<any>` inference leak, so
  consumer barrels stop widening.
- **PT-E3 (record, not ask)** — Pratt stays dormant; the calc() 2-tier fold "does not clear
  a KISS/DRY bar"; value.js will **not** pull it forward.

Bearing on the bbnf Value-API plane:

- **No hard obligation, one opportunity (PT-E1).** Scoped per-parse diagnostics is exactly
  the error-surface bbnf's generated parsers want (bbnf already threads furthest-offset /
  expected-set state — the same substrate PT-WAVE-1 added on the TS side). If bbnf's
  Value-API grows a real error surface, this is the design to mirror, not an obligation the
  letter imposes.
- **PT-E2 is TS-generics-local.** The `Parser<any>` leak is a TypeScript inference concern;
  it does not bind bbnf's backend-agnostic (Rust) Value-API.
- **The real cross-repo obligation is latent in PT-E3 + the 1.0.0 non-goals, and it lands on
  bbnf, not parse-that.** parse-that's recorded decisions
  (`docs/future-research.md:214-220`) keep the packrat/WDM left-recursion tier
  **provisional, held open expressly for bbnf**: "the tier is kept pending the bbnf-lang
  LR-consumer question — bbnf-lang is the one grammar-DSL that would exercise it … If no LR
  consumer materializes, a future cut may retire the tier." PT-E3 confirms no Pratt tier is
  coming from parse-that either. **SK-V21 owes parse-that a GO/NO-GO on whether bbnf's
  grammar DSL will exercise LR/packrat left recursion.** That decision either justifies
  keeping the tier or frees parse-that to retire it — it is the one place bbnf's roadmap
  gates a neighbor.

---

## 4. The fold-in / fold-out plan

### Fold IN (bbnf totality consumes; do not reimplement)

1. **The canonical `bbnf-regex` engine** (parse-that `rust/regex`). Totality already
   consumes it (§2.4). On the skinny→totality fold, **retire skinny's 322-LOC `bbnf-regex`
   stub** and route skinny's `crates/ir`/`crates/passes` `analyze()` callers onto the real
   engine's `RegexInfo`/`classify_regex`/FIRST APIs. The engine's HIR e-graph already rides
   the shared egraph+csp-solver substrate — no new integration surface.
2. **Number / SIMD scanner substrate.** Consolidate onto `parse_that`'s number fastpath +
   `scanners.rs` SIMD lane; **retire skinny's `parse-that-regex/number/eisel_lemire`
   reimplementation** (§2.3). One Eisel-Lemire, one quoted-string SIMD scanner.
3. **Packrat/WDM learnings (design, not code).** If bbnf answers the LR question GO: the
   `(id, offset)`-keyed left-recursion soundness fix (`193854d`), the Float64-safe memo key
   (`id * 2^32 + offset`, whole-offset, fail-loud past the safe-integer ceiling — PT-Q2,
   `typescript/CLAUDE.md` conventions), and the `PACKRAT_ARMED` zero-alloc latch (`934b2fa`)
   are the correct designs to carry.
4. **The self-hosted regex parser as a bootstrap/verification artifact** (§2.5) — bbnf owns
   `regex.bbnf`; parse-that keeps the generated bench surface.
5. **Edition-2024 + toolchain alignment.** parse-that is already 2024; mirror any fresh
   nightly into parse-that's `rust-toolchain.toml` in the same commit as bbnf's abrogation.

### Fold OUT / stays out (do NOT pull into bbnf totality)

- **SpanParser tagged-union tier** — permanently KILLED; falsified ~10-14% *slower* on
  V8/TS (`docs/future-research.md:71-109`) and its only rationale (BBNF codegen) already
  moved to bbnf. Do not re-attempt runtime-switch dispatch.
- **The `*Span` combinator surface** — excised at 1.0.0, zero consumers across the
  constellation (`docs/future-research.md:100-110`). Only the `Span` *value* type + two
  helpers survive.
- **Pratt / precedence-climbing 2-tier fold** — recorded dormant; "does not clear KISS/DRY"
  (PT-E3).
- **Token streams, incremental parsing, Squirrel LR** — deliberate 1.0.0 non-goals
  (`docs/future-research.md:200-204`).
- **parse-that's CSS parser** — removed at A.W1; bbnf owns CSS via grammar. Do not
  resurrect a hand-rolled CSS surface in parse-that.
- **Zone-partitioning / splitting `parser.ts`** — recorded net-negative
  (`docs/future-research.md:206-208`).

### Decisions SK-V21 must make (load-bearing seams)

1. **The bbnf-lang LR-consumer GO/NO-GO** (§3). parse-that holds packrat/WDM open *for*
   bbnf. Answer it in the SPEC; do not leave the neighbor waiting.
2. **N7 crate-identity resolution.** Two `bbnf-regex@0.1.0` cannot coexist in a unified
   graph. Totality's consumption of the real engine is the winner; the skinny fork and the
   skinny `parse-that-regex` number-fork are the deletions.
3. **Toolchain mirror obligation** (§2.1). The stack abrogation is not bbnf-local: the
   cross-repo path-patch coupling means parse-that's pin moves in lockstep or the build
   split-brains.

---

## 5. Staleness corrections to prior report 17 / SK-V20 premises

- FALSE now: "parse-that ships CSS" (removed, A.W1), "parse-that ships a `*Span` builder
  surface" (excised, S.H2), "parse-that has a SpanParser tier" (killed, B.W0).
- The parse-that **Rust** tree did **not** participate in the 1.0.0 cut — it is frozen at
  `051a6d6` (2026-05-02). Treat any "1.0.0 Rust change" claim as spurious.
- parse-that Rust is **edition 2024 on nightly-2026-04-11** — the same stale pin bbnf
  carries; not a fresher stack.
