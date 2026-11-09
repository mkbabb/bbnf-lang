# 06 — The Coordination Census + Non-Clobber Protocol

Read-only census of `/Users/mkbabb/Programming/parse-that` (git HEAD, untracked
footprint, and sibling repos value.js / keyframes.js) to place one additive
coordination letter without collision. No file in parse-that, value.js, or
keyframes.js was modified, staged, or committed in the course of this audit.

## (a) — Where coordination letters actually live

`VALUEJS-PT-E-2026-07-05.md` (grep confirms — the only hit for that literal
string in the tree) lives at `docs/tranches/A/VALUEJS-PT-E-2026-07-05.md` —
**not** under a `docs/tranches/A/coordination/` subdirectory, even though
parse-that's own process spec defines that path as canonical
(`docs/precepts/instructions/TRANCHE-AND-WAVE-SPEC.md:240` — "coordination |
`docs/tranches/{LETTER}/coordination/*.md` | cross-repo race surfaces"). The
letter's own placement note explains the deviation (`docs/tranches/A/
VALUEJS-PT-E-2026-07-05.md:11-14`): *"parse-that carries no established inbox
convention (tranche A is your only tranche and is CLOSED+PUBLISHED …); this
file follows the constellation pattern (an incoming `VALUEJS-*` letter in the
recipient's latest tranche dir …). Re-home it if you open a coordination
inbox."* So the live precedent is "drop it in the latest `docs/tranches/
{LETTER}/` dir, flat" — the spec-correct `coordination/` subdir is invited but
not yet opened by any sender.

parse-that's own `docs/tranches/` currently contains exactly one directory:
`A/` (founding tranche, CLOSED + PUBLISHED at `@mkbabb/parse-that@0.11.0`,
2026-06-19). Everything since — Tranche B (0.12.0), Tranche Q (0.13.0), and
the informally-named "Tranche S" (S.H1/S.H2/S.H4, cut to 1.0.0 on
2026-07-03) — landed via direct source + `CHANGELOG.md` commits with **no**
`docs/tranches/{LETTER}/` directory of their own (confirmed: `git show
--stat` on the B, Q, S.H1, S.H4 commits shows zero `docs/tranches/` paths
touched). `A` is therefore both the founding AND the only formally-housed
tranche; "the latest tranche dir" the letter convention points at will stay
`A/` until parse-that opens a second one.

**ID schemes observed, three independent lineages, all distinct in prefix:**

| Lineage | Example | Sender-side source | Prefix shape |
|---|---|---|---|
| value.js → parse-that | `docs/tranches/A/VALUEJS-PT-E-2026-07-05.md` (source: value.js `docs/tranches/S/letters/PARSE-THAT-PT-E.md`) | value.js Tranche S orchestrator | `VALUEJS-PT-<letter>[-<date>]` |
| keyframes.js → parse-that | `typescript/CHANGELOG.md:115` cites `KF-TO-PARSETHAT-Q.md`; keyframes.js repo has `docs/tranches/Q/KF-TO-PARSETHAT-Q.md` and `docs/tranches/P/KF-TO-PARSETHAT-B.md` | keyframes.js orchestrator | `KF-TO-PARSETHAT-<letter>` |
| bbnf-lang → parse-that | none yet — this letter is the first | (proposed) | `BBNF-PT-<letter>-<date>` |

The `-E` suffix on the value.js letter is **not** a global shared counter —
value.js's own `docs/tranches/{R,S,T}/letters/` directories carry no `PT-A`
through `PT-D` files (grep across all three tranches turns up zero hits), so
`E` tracks value.js's internal audit-section numbering (`SYNTHESIS.md §7.2`),
not a cross-sender sequence. `BBNF-PT-F` therefore does not need to "come
after" value.js's `E` in any binding sense — it is safe purely because the
prefix (`BBNF-PT-`) is disjoint from both `VALUEJS-PT-` and
`KF-TO-PARSETHAT-`. Choosing `F` is cosmetic continuity for a reader scanning
`docs/tranches/A/` chronologically (E then F), not a collision requirement.

## (b) — Untracked footprint at parse-that HEAD

```
.gitmodules
docs/instructions/
docs/precepts/
rust/.cargo/
rust/parse_that/rustc-ice-2026-04-22T15_05_48-88533.txt
```

Read (not modified):

- **`.gitmodules` + `docs/precepts/`** — a submodule add-in-progress:
  `.gitmodules` declares `submodule "docs/precepts"` pointing at
  `git@github.com:mkbabb/precepts.git`, and `docs/precepts/.git` confirms the
  directory is a live git-submodule checkout, already containing
  `cross-repo-dev-resolution.md`, `instructions/TRANCHE-AND-WAVE-SPEC.md`,
  and the `REAUDIT-2026-04-30` style audits. keyframes.js already has this
  same submodule **committed** (`git config --get-regexp submodule` shows
  `submodule.docs/precepts.url` live there). parse-that is mid-migration onto
  the fleet-wide shared-precepts submodule; the `.gitmodules` commit simply
  has not landed yet locally. **Implication for our letter: do not propose
  edits under `docs/precepts/` — it is a shared submodule owned elsewhere,
  and do not touch `.gitmodules` — an in-flight, not-yet-committed fleet
  migration.**
- **`docs/instructions/`** — a single-file stub (`README.md`, 880 bytes,
  dated Apr 29), unrelated scaffolding, not live-session signal.
- **`rust/.cargo/config.toml`** — a `[patch.crates-io]` block that resolves
  parse-that's Rust crates against **sibling repo paths on disk**, including
  three bbnf-lang crates directly: `bbnf = { path = "../../bbnf-lang/crates/
  core" }`, `bbnf-ir = { path = "../../bbnf-lang/crates/ir" }`, `bbnf-ser =
  { path = "../../bbnf-lang/crates/ser" }`, plus `egraph`/`egraph-derive`
  (also `../../bbnf-lang/crates/…`) and `pprint`/`pprint_derive`
  (`../../pprint/rust`) and `csp-solver` (`../../csc411/…`). A code comment
  in the file already records a prior collision-and-fix: *"`bbnf_derive`
  retired in bbnf-lang B2 (commit `6142387f`); the `../../bbnf-lang/crates/
  derive` source no longer exists. Post-B2 the regex-bootstrap crate
  re-exports `parse_that::regex::generated` directly…"* — i.e. this exact
  file already broke once when bbnf-lang deleted a crate path, and was
  hand-patched to compensate. **Implication: any bbnf-lang wave that renames,
  moves, or deletes `crates/core`, `crates/ir`, `crates/ser`, `crates/
  egraph`, or `crates/egraph-derive` will silently break parse-that's local
  Rust build via this patch table — a live, undocumented coupling that our
  letter should name explicitly as an ask-and-offer item (see (e)).** This
  file is `.gitignore`d by design (its own header comment says so) — it will
  never appear in a parse-that PR diff, so bbnf-lang has no other way to
  learn about the coupling except by reading it here.
- **`rust/parse_that/rustc-ice-2026-04-22T15_05_48-88533.txt`** — a stale
  rustc internal-compiler-error crash dump from April 22 (LLVM codegen
  worker-thread panic, nightly `1.96.0-nightly (9602bda1d 2026-04-05)`,
  aarch64-darwin). Three months old relative to the 2026-07-09 charter date;
  a leftover crash artifact, not active-session signal. No action implied.

**Net read on active-session footprint**: parse-that's own working tree shows
no in-flight *parse-that* session (no dirty tracked files, no draft wave
docs); the untracked items are fleet infrastructure (precepts submodule) and
a genuine, presently-invisible-to-bbnf-lang cross-repo Cargo coupling — not
evidence of a concurrent parse-that agent session that our letter could
race.

## (c) — parse-that's native tranche lettering + ledger state

Formal `docs/tranches/{LETTER}/` directories: **only `A`** exists on disk
(founding tranche, CLOSED+PUBLISHED 0.11.0, 2026-06-19). Post-A work
continued under bare tranche-letter commit-message labels with no
`docs/tranches/` directory backing them:

| Label | Commit | Date | Landed as |
|---|---|---|---|
| Tranche B | `7901314` | 2026-06-22 | 0.12.0 — packrat cross-input fix + combinator fusion + SpanParser KILL |
| Tranche Q | `2c806fb` | 2026-06-23 | 0.13.0 — packrat re-entrancy + >1MB key fix; thenMap/fuse delete; `*Span` deprecate |
| "Tranche S" (S.H1/S.H2/S.H4) | `934b2fa`/`043c4d1`/`7eab78c` | 2026-07-03 | **1.0.0** — packrat epoch arming behind `PACKRAT_ARMED` + the `*Span`/`chain()` breaking cut |
| VALUEJS-PT-E coordination | `ef10d5b` | 2026-07-05 | doc-only; ask letter, no code |

`git log --oneline` also surfaces a much older, pre-tranche-system label set
(`H-1` through `H-7`, `J`, `K`, `M`, `Q` [informal, pre-A], `V`, `X.2/3`,
`Y.7/8/9`, `Z.2`, plus `PT-WAVE-1/2/3a`, `AZ-IV.*`, `AW-IV.*`) — these predate
`A.md`'s own claim that *"parse-that has no tranche system. A is its founding
tranche"* (`docs/tranches/A/A.md:22`), confirming they are informal,
pre-system commit-message tags, not entries in any letter/tranche registry
that a new coordination ID could collide with.

**Current HEAD is `1.0.0` (Tranche S, closed 2026-07-03), plus one
doc-only coordination-letter commit two days later.** No open tranche, no
`docs/tranches/B/` … `docs/tranches/S/` directory exists to collide with.
`BBNF-PT-F-2026-07-09` lands in the only tranche directory that exists (`A/`)
— the same placement value.js's letter used five days prior — and shares no
ID surface with `B`, `Q`, `S`, or any of the pre-system labels.

## (d) — value.js and keyframes.js: what they are mid-flight on, today

Both sibling repos are in an **acutely live** state on the charter's own
date (2026-07-09) — this materially raises the "review-not-execute" stakes:

- **value.js** (`git log -5`): HEAD is `a8d24b9`, *"docs(T · RATIFIED): the
  2026-07-09 owner rulings encoded — all 20 rows closed … dispatch gate
  OPENS"* — Tranche T just got its owner-ratification and its execution
  dispatch gate is opening as of today. `docs/tranches/` runs `A`
  through `T` (20 formal tranche directories), with `docs/tranches/T/
  letters/GLASSUI-T-ASKS.md` as its own most recent outbound coordination
  letter (to glass-ui, not parse-that). Its `PT`-lineage letter to
  parse-that (`PARSE-THAT-PT-E.md`) sits in the now-closed `S/letters/`,
  committed 2026-07-0x per `git log … PARSE-THAT-PT-E.md` → `332d080`.
  value.js has **no open ask toward parse-that beyond PT-E1/E2/E3**
  (diagnostics HIGH, inference MED, Pratt-dormant record) — nothing here
  needs bbnf-lang input.
- **keyframes.js** (`git log -5`): HEAD is `72d1873`, *"T close V: the
  library-gate hang cured …"* — closing out Tranche T's CI hardening, with
  the immediately-prior commit `b95973a` opening **Tranche U** off *"the
  owner's 2026-07-09 edict … the 32-lane audit fleet launched."*
  `docs/tranches/` runs `A` through `U`. Its parse-that-directed letters are
  the older `KF-TO-PARSETHAT-B.md` (Tranche P) and `KF-TO-PARSETHAT-Q.md`
  (Tranche Q) — both predate parse-that's own Tranche B/Q by name-coincidence
  only (independent counters); no open keyframes→parse-that ask found in the
  U audit-fleet lane list surfaced by the census (`lane-28-constellation-
  parsethat-glassui.md` exists but is a U-tranche internal audit lane, not
  an outbound letter).

**Reading**: both sibling repos are mid-dispatch on their own large,
just-ratified/just-opened tranches (T for value.js, U for keyframes.js) as
of today. Neither has an open ask blocking on bbnf-lang or parse-that. This
confirms the charter's instinct: our letter must be strictly additive,
review-at-leisure, and must not imply any gate the fleet has to clear before
its own T/U work proceeds.

## (e) — The protocol: `BBNF-PT-F-2026-07-09`

**Placement**: `docs/tranches/A/BBNF-PT-F-2026-07-09.md` in parse-that —
mirroring the `VALUEJS-PT-E-2026-07-05.md` precedent exactly (same
directory, same flat-file convention, same "re-home me if you open a
coordination inbox" invitation left for the recipient). bbnf-lang does not
write into parse-that's tree directly under the non-clobber law; this is the
**proposal's target path**, to be handed to a human or a parse-that-side
session to place, not written by this or any bbnf-lang session.

**ID**: `BBNF-PT-F-2026-07-09` — prefix `BBNF-PT-` is disjoint from
`VALUEJS-PT-` and `KF-TO-PARSETHAT-` (§a); letter `F` is cosmetic
alphabetic continuity after value.js's `E`, not a collision-driven
requirement, since no shared counter exists across senders.

**Content skeleton** (mirrors the value.js letter's proven shape — standing
context, an asks table with priorities, a record-only row, non-motion):

1. **Standing context** — bbnf-lang has zero committed edits in parse-that;
   the coupling is presently one-directional and invisible to parse-that:
   `rust/.cargo/config.toml` (gitignored, untracked, §b) patches three
   bbnf-lang crates (`crates/core`, `crates/ir`, `crates/ser`) plus
   `crates/egraph`/`crates/egraph-derive` directly into parse-that's local
   Rust build.
2. **The asks-and-offers table**, each row priority-tagged (HIGH/MED/record,
   matching value.js's own convention):
   - **Ask — crate-path stability notice.** Any bbnf-lang wave that renames
     or removes `crates/core`, `crates/ir`, `crates/ser`, `crates/egraph`,
     or `crates/egraph-derive` should land a one-line note in its close
     commit naming the old and new path, the way bbnf-lang's B2 crate
     retirement already forced a hand-patch of parse-that's `.cargo/
     config.toml` once (§b, documented in that file's own comment). This is
     an ask *of process*, not of code — no parse-that source changes.
   - **Offer — the PT-R fold-back items are proposals, not asks.** The
     x86/SIMD-touching PT-R.1/R.3/R.6 items (sk-v22 `SPEC.md` §PT-R —
     pure-primitives cut, regex-engine SOTA absorption, `Box::leak`
     diagnostic-label hazard closure) and the PT-T.1-5 TypeScript items
     (inference tightening = value.js's own PT-E2 ask; scoped diagnostics =
     PT-E1; V8 hidden-class normalization; legacy/state-tier sweep;
     megamorphism-lever carry-iff-measured) are **bbnf-lang-authored
     sketches parse-that may pull on its own cadence** — not a dispatch
     bbnf-lang expects parse-that to clear. PT-T.1/T.2 in particular already
     answer value.js's own live PT-E1/PT-E2 asks; naming that overlap saves
     parse-that from re-deriving the same fix independently.
   - **Record, not ask** — PT-R.4/R.5 (the fold-back resolution-graph
     relocation) are bbnf-lang-internal moves that touch the *shared*
     `bbnf-regex` grep-zero fork point; recorded here only so a future
     parse-that session sees the seam if it goes looking, per value.js's
     own PT-E3 "record, not ask" convention for the Pratt-dormant item.
3. **Review-not-execute framing, verbatim close line** (matching the
   value.js letter's stand-alone-document discipline): *"Nothing here gates
   any parse-that wave. This is a proposal-and-notice letter, folded on
   parse-that's own cadence, not a dispatch."*

**Non-clobber discipline for this letter's own authorship**: the letter
itself is authored and placed by a bbnf-lang-side session (or a human) that
writes ONLY inside bbnf-lang's own tree until a maintainer copies the final
text across the repo boundary — exactly as the census in this document
touched zero files in parse-that, value.js, or keyframes.js.
