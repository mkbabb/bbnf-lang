# 06 — Commit Archaeology

A retrospective taxonomy of 1,842 commits on master (including 945
unpushed) plus roughly 80 commits across 24 feature branches — 1,923
commit events across all refs. The archaeology partitions the project
history into six eras, reconstructs each era's architectural thesis
from tranche plan documents and FINAL documents, and traces which
approaches endured, which were reverted, and what the cost of each
reversal was.

Per-era deep dives live alongside this document in
`docs/tranches/meta-audit/archaeology/`:

- [era-II-foundations.md](./archaeology/era-II-foundations.md)
- [era-III-substrate.md](./archaeology/era-III-substrate.md)
- [era-IV-tape-first.md](./archaeology/era-IV-tape-first.md)
- [era-V-dta-psi-rut.md](./archaeology/era-V-dta-psi-rut.md)
- [era-VI-restart.md](./archaeology/era-VI-restart.md)

Era I is the 25-commit TextMate-grammar / LSP prelude from March 2023;
no tranche discipline; three-year hiatus before Era II opens. It is
covered inline in the Era II document.

## Headline numbers

| Measure | Value |
|---|---:|
| Total commits on master | 1,842 |
| Unpushed commits (`git log origin/master..HEAD`) | 945 |
| Total commits across all refs | 1,923 |
| Feature branches ahead of master | 24 |
| Feature-branch commits not on master | ~35 |
| First tranche-tagged commit (Tranche F, `a3fadf56`) | 2026-04-08 |
| First `Tranche X` plan doc (Tranche AA, `13411847`) | 2026-04-10 |
| First `PROGRESS.md` (Tranche AS, `536ac07c`) | 2026-04-13 |
| First `FINAL.md` (Tranche AU, `5281ec23`) | 2026-04-15 |
| First planning-only tranche (AZ) | 2026-04-20 |
| First pre-tranche-annex (B0) | 2026-04-20 |
| First meta-audit | 2026-04-22 |

## Era taxonomy summary

| Era | Dates | Duration | Commits | Tranches | Verdict |
|---|---|---:|---:|---|---|
| I | 2023-03-03 → 2023-03-06 | 4 days (then 3-year hiatus) | 25 | none | Prelude. LSP + TextMate grammar. |
| II | 2026-02-26 → 2026-03-15 | 18 days | ~264 | none formal | Monorepo scaffold + IR crate + grammar notation freeze. |
| III | 2026-03-16 → 2026-04-09 | 25 days | ~280 | F–W (18 letters, most single-commit) | Optimiser substrate — CSP, e-graph, regex HIR, NodeId keying, `IndexMap` determinism. |
| IV | 2026-04-10 → 2026-04-15 | 6 days | ~185 tranche-tagged | Y, Z, AA–AU (20 tranches) | Tape-first codegen. **AU is the baseline.** |
| V | 2026-04-15 → 2026-04-19 | 5 days | ~572 tranche-tagged | AV, AW-I–V, AX (7 tranche letters) | DTA/PSI/activation rut. 0/17 bench entries at AW-V close beat post-AU. Interpreter deleted at AX.W0b. |
| VI | 2026-04-20 → 2026-04-22 (in flight) | 3 days+ | ~130 | AY-I, AY-II, AZ, B0, B1, BA–BC scaffolds | Infra-truth restart. Column revert. B1 prelude blocks AY-II resume. |

## Part A — Tranche-by-tranche ledger

Verdicts: **Worked** = thesis held and substrate durable. **Partial**
= some phases landed, others deferred or superseded. **Reverted** =
thesis abandoned; substrate deleted. **Planning** = no runtime code.

### Era II — no tranches formal

Era II pre-dates tranche discipline. The bulk work lives under
conventional commit messages (`feat: ...`, `refactor: ...`).
Characterisation per `era-II-foundations.md`: monorepo scaffolding
(`cc499979`, 2026-02-26) through IR bootstrap (`1710d6f7`, 2026-03-15).

### Era III — optimiser substrate

| Tranche | Commits | First / Last | Plan? | FINAL? | Headline | Verdict |
|---|---:|---|:---:|:---:|---|---|
| F | 1 | `a3fadf56` 2026-04-08 | — | — | pre-solve delim_scan + key_dispatch per-grammar | Worked |
| G | dir only | — | — | — | bbnf-derive / egraph-derive split | Worked |
| H | ~4 | 2026-04-08 / 09 | — | — | e-graph rewrite rules (factor, merge_regex_alts, inline_acyclic) | Worked |
| I | 3 | 2026-04-09 | — | — | orphan entry-point purge + DAG invariants | Worked |
| J | 1 | `6becbf8b` 2026-04-09 | — | — | e-graph scheduler real-work measurement | Worked |
| K | 1 | `a5991bac` 2026-04-09 | — | — | CSP-scheduled e-graph execution | Worked |
| L | 1 | `f6119e0b` 2026-04-09 | — | — | NodeId everywhere; pointer-identity purged | Worked |
| M | 1 | `359eb068` 2026-04-09 | — | — | delete dead GrammarAnalysis lattice | Worked |
| N | 1 | `9932d348` 2026-04-09 | — | — | IndexMap deterministic codegen | Worked |
| O, P | dir only | — | — | — | no tagged commits | N/A |
| Q | 1 | `5e408f04` 2026-04-09 | — | — | bbnf-ir god-module splits (6 dirs) | Worked |
| R | 1 | `2d326760` 2026-04-09 | — | — | bbnf core god-module splits | Worked |
| S | 1 | `b2c26511` 2026-04-09 | — | — | bbnf analysis + ser splits | Worked |
| T | dir only | — | — | — | no tagged commits | N/A |
| U | 1 | `bab4405f` 2026-04-09 | — | — | docs + post-N bench baseline | Worked |
| V | 9 | 2026-04-09 | — | — | recognizer mining pipeline | Worked |
| W | 11 | 2026-04-09 | — | — | CSP + cost model + kernel family modules | Worked |

### Era IV — tape-first

| Tranche | Commits | First / Last | Plan? | FINAL? | Headline | Verdict |
|---|---:|---|:---:|:---:|---|---|
| X | 20 | 2026-04-09 → 2026-04-19 | — | — | span continuation + Lever 4 scaffolding | Partial (Lever 4 deleted at AX.W0b) |
| Y | 13 | 2026-04-10 | — | — | tape column splits (7-Vec substrate) | **Reverted** at AY-I.W1 |
| Z | 6 | 2026-04-10 | — | — | cursor + reader surface | Worked |
| AA | 15 | 2026-04-10 | ✓ | — | TypeDescInterner hash-cons (`c209c380`) | Worked |
| AB | 4 | 2026-04-10 | ✓ | — | tape as the only runtime substrate | Partial |
| AC | 10 | 2026-04-10 | ✓ | — | full tape transposition | Partial |
| AE | 17 | 2026-04-11 | ✓ | — | tape-first shape-agnostic walking (`85478284`) | Worked |
| AF | 26 | 2026-04-11 | ✓ (prototype) | — | three-tier emission design (`MustTape` / `MustFn` / `MayInline`) | **Reverted** at AQ.5 |
| AG | 6 | 2026-04-11 | — | — | cross-rule CSP tier variables | Subsumed by AQ.5 |
| AI | 9 | 2026-04-11 → 12 | — | — | scanner integration + payload consolidation | Worked |
| AJ | 4 | 2026-04-12 | — | — | post-AJ bench baseline | Worked |
| AK | 3 | 2026-04-12 | — | — | post-AK bench baseline | Worked |
| AL | 1 | 2026-04-12 | — | — | minor cleanup | Worked |
| AM | 7 | 2026-04-12 | — | — | AM.0 — 4 regressions resolved (`4d1afeb0`) | **Inflection** |
| AN | 5 | 2026-04-12 | ✓ | — | correctness + generalization + hyper-opt plan (`acaa1898`) | Partial |
| AO | 2 | 2026-04-12 | ✓ | — | structural dispatch v1 plan (`e64164e4`) | **Reverted** at AQ.5 |
| AP | 9 | 2026-04-13 | ✓ | — | structural dispatch v2 + Tier B emission (`480a4cb4`) | Partial |
| AQ | 13 | 2026-04-13 | ✓ | — | TypeDesc-driven projection + **structural dispatch deletion** (`2f7c1bd4`) | **Inflection** |
| AR | 16 | 2026-04-13 | ✓ | — | discriminator split + payload activation + CSS hardening | Worked |
| AS | 4 | 2026-04-13 | ✓ | `PROGRESS.md` intro (`536ac07c`) | CSS L4 parse activation + `TypeDesc::Span` admission | Worked |
| AT | 14 | 2026-04-13 | ✓ | — | multi-type payload projection, meta_idx pack | Partial |
| AU | 22 | 2026-04-13 → 15 | ✓ | `5281ec23` — **first FINAL.md** | Projection activation, regression redress, scanner truth | **Partial — baseline anchor** |

### Era V — DTA / PSI / activation rut

| Tranche | Commits | First / Last | Plan? | FINAL? | Headline | Verdict |
|---|---:|---|:---:|:---:|---|---|
| AV | 53 | `ca0875eb` → `be4b22b1` 2026-04-15 / 16 | ✓ | ✓ | The Flattening — DTA + PSI + columnar substrate; V0 closes AU Bug 1/2/2b; V6-V9 routed forward | Partial |
| AW-I | ~45 | 2026-04-16 | ✓ | `FINAL-I.md` | Activation — walker completion, `parse()` swap, MemoStore retirement | Partial |
| AW-II | 40 | 2026-04-16 | ✓ | `FINAL-III.md` combined | DTA self-host round-trip; consumer migration | Partial — viability question raised |
| AW-III | 93 | 2026-04-17 | ✓ | `FINAL-III.md` | Fused correctness + architectural transposition; 3 emitter passes + 5 consumers | **Missed gate** — 0/19 strict-better |
| AW-IV | 92 | 2026-04-17 | ✓ | `FINAL-IV.md` | Granular exceed + parity harnesses | **Missed gate** — 0/19 exceed post-AU |
| AW-V | 80 | 2026-04-17 → 19 | ✓ | `FINAL-V.md` | Final activation attempt; JSON shape-emitter demo at W3 close then lost by W6 | **Partial — thesis demonstrated and lost** |
| AX | 169 | 2026-04-16 → 20 | ✓ | `c590bcc2` | The RD Reckoning — 21 invariants, W0a gate repair, W0b interpreter deletion, W0c doc rewrite, W1r view + parity | **Partial — substrate + parity close; Block B → AY** |

### Era VI — restart

| Tranche | Commits | First / Last | Plan? | FINAL? | Headline | Verdict |
|---|---:|---|:---:|:---:|---|---|
| AY-I | 28 (pre-split) | 2026-04-19 → 20 | ✓ | ✓ | Pass I of AY — write-time substrate + direct-to-struct; parity not met | Partial — honest relinquish; **column revert at W1** |
| AY-II | 77 | 2026-04-21 → 22 | ✓ | `PATH-FORWARD.md` | Gestalt re-ordered remainder; W0' substrate refactors | **Paused for B1** |
| AZ | 14 | 2026-04-20 | ✓ | — | Planning — 6 research branches | **Planning only** |
| B0 | 17 | 2026-04-20 | ✓ | ✓ | Bounded prelude annex — profile tiers + `ay-*` Makefile | Worked |
| B1 | 7 | 2026-04-22 | ✓ | — | Dev-loop truth + proof-surface hardening | In flight |
| BA, BB, BC | 0 | — | ✓ (scaffold) | — | Scaffold only | Not started |

## Part B — Architectural inflection points

Commits that visibly pivoted approach. Each row cites the commit,
what it reversed, and the forward consequence.

| SHA | Date | Tranche | What it pivoted | Forward consequence |
|---|---|---|---|---|
| `85478284` | 2026-04-11 | AE.0/.1 | Tape-first shape-agnostic walking substrate | Every later tranche inherits tape-first lowering. |
| `4d1afeb0` | 2026-04-12 | AM.0 | 4 regressions blocking workspace resolved | The template for triage-before-forward opens every later tranche. |
| `2f7c1bd4` | 2026-04-13 | AQ.5 | **Deleted structural dispatch + EmissionTier.** Collapsed `MustTape`/`MustFn`/`MayInline` axis into `PayloadKind → TypeDesc`. | `no-orthogonal-codepaths` invariant. Two weeks of AF/AG substrate subsumed. |
| `ff757215` | 2026-04-13 | AU plan | Projection activation, scanner truth, debt elimination | AU is the baseline every later tranche cites. |
| `5281ec23` | 2026-04-15 | AU close | First `FINAL.md` (`docs/tranches/AU/FINAL.md`) | Convention adopted by every subsequent tranche. |
| `ca0875eb` | 2026-04-15 | AV plan | "The Flattening" — DTA + PSI + columnar substrate | Opens Era V. |
| `ec11f529` | 2026-04-16 | AW-II.W1.0 | `find_descendant_by_kind` promoted; DTA self-host round-trip begins | 40 commits of consumer migration follow. |
| `c1e86ab3` | 2026-04-17 | AW-V.W3 close | JSON shape-emitter thesis demonstrated exactly once | Demonstration lost by W6 — the peak-and-lose of Era V. |
| `4177a18c` | 2026-04-16 | AX plan | "The RD Reckoning" — 21 invariants, W0b interpreter deletion queued | Opens AX; frames Era V's close. |
| `bc550d2c` | 2026-04-20 | AX.W0b.A | Retire walker path + gate predicates, regen | First commit of the ~78K-LOC interpreter deletion. |
| `a206b962` | 2026-04-20 | AX.W0b.A | Delete `dta_walker/` + `emitter/dta.rs` | Interpreter gone. |
| `0adabb23` | 2026-04-20 | AX.W0b cleanup | Delete DTA-walker regression tests + carve dead profile fields | AX.W0b closes. |
| AY-I W1 (column revert) | 2026-04-20 | AY-I.W1 | Tape columns reverted from 7 structural Vecs to 1 `Vec<TapeRec>` + parallel `sib_skip` | Direct revert of Tranche Y; twitter recovers to 688 MB/s. |
| `bd563c1d` | 2026-04-21 | AY-II.W0'.a | Collapse `TapeBuilder` + `ValueBuilder` → `FusedBuilder` | Single parse-entry; retire standalone ValueBuilder. |
| `fef4416c` | 2026-04-22 | user-led | User-led normalization across tranches/ + instructions/ + B1 rename | Vocabulary convergence post-meta-audit. |
| `4869e715` | 2026-04-20 | B0.W0.1 | `.cargo/config.toml` `[alias]` block — `iter-*`, `expand-*`, `asm-parse` | Public fast-path commands begin. |

## Part C — Tranche process evolution

The tranche mechanism evolved through six discrete steps, each
visible in the commit record.

### Step 1 — Commit-message tag (Era III, from `a3fadf56`)

First tranches (F, G, H, I, J, K, L, M, N) carry `(Tranche X)` in
the commit subject but have no dedicated documentation. Tranches are
single-commit or short multi-commit units; the "tranche" concept is
a retrospective organisational tag, not a plan-and-execute protocol.

### Step 2 — Plan document (Era IV, from `13411847` AA.md)

2026-04-10 introduces `docs/tranches/AA.md` — the first named plan
document. Plans are prose at this point; no wave schedule, no hard
gates, no invariants. AA, AB, AC, AE, AF, AP each carry flat plan
docs.

### Step 3 — Prototype documents (Era IV, AF-prototype, AL-prototype)

Some tranches ship multiple prototype files (`AA-prototype.md`,
`AA-prototype-2.md`, `AL-prototype-1.md` through `-4.md`) — the
first use of iteratively-refined planning. Each prototype is a
distinct architectural proposal; the non-prototype file is the one
selected.

### Step 4 — Directory structure + PROGRESS (Era IV, `536ac07c` AS)

2026-04-13 `docs: tranche directory structure + crate ownership +
AS PROGRESS.md`. AS becomes the first tranche with a directory and
a `PROGRESS.md`. The `PROGRESS.md` is an append-only session log —
orchestrator writes "session N: did X, Y, Z; next session opens at
Z+1". This is the audit-trail mechanism.

### Step 5 — FINAL document + waves (Era IV/V boundary, `5281ec23` AU)

2026-04-15 AU publishes `docs/tranches/AU/FINAL.md` — the first
retrospective document. AU also ships wave numbering (W1 through
W7) as a first-class concept. Hard gates are declared in AU.md and
scored in FINAL.md. The `post-AU.json` bench file anchors every
subsequent baseline claim.

### Step 6 — Invariants + wire contracts (Era V, `4177a18c` AX)

2026-04-16 AX declares 21 numbered invariants at plan time. Each
invariant carries a forward-verification mechanism (wire contract,
nm symbol check, predicate symmetry, bench-checkpoint). The
"ledger-only wave = re-plan trigger" invariant 13 prohibits
substrate-without-consumer landings.

### Step 7 — Audit directories + triumvirate (Era V/VI, `AW/audit/`, `AX/audit/`, `AY-I/audit/`, `AY-II/audit/`)

Tranches now ship an `audit/` subdirectory with research artefacts
separate from the plan document. AW carries a full research/ dir.
The audit-triumvirate pattern (research commit → plan commit →
redress dispatch) lands in memory as `triumvirate-discipline` and
`triumvirate-auto-trigger`.

### Step 8 — Bounded prelude annex (Era VI, B0, B1)

B0 introduces the concept of a **prelude annex** — a non-runtime,
non-architectural tranche whose sole purpose is to make the next
runtime tranche trustable. B0 + B1 block AY-II resume until the dev
loop is truthful.

### Step 9 — Planning-only tranche (Era VI, AZ)

AZ ships 14 commits, all `docs(next-tranche):` — zero runtime. Six
parallel research branches, one per profile axis. AZ is the first
pure-planning tranche.

### Step 10 — Meta-audit (Era VI, 2026-04-22)

Four parallel agents, four axes, disjoint file bounds:
`01-session-friction.md`, `02-instruction-adherence.md`,
`03-tranche-drift.md`, `04-toolchain-pain.md`. User-led normalization
(`fef4416c`) writes the findings back into source documents. This
archaeology is the sixth meta-audit artefact.

## Part D — Approaches that worked — 7 techniques

### 1. Delim-scan pre-solved per-grammar (Tranche F, `a3fadf56`)

Compile-time CSP solves per-grammar delimiter classes; the scanner
inner loop gets constant byte sets, not runtime-reconstructed ones.
The invariant established: decisions that can be made at compile
time *must* be made at compile time and reach the emitted binary as
constants. Per `perf-breakthrough-accuracy` memory, delim-scan is
one of the three real breakthroughs of the project.

### 2. Bespoke regex HIR (Tranche W + `bbnf-regex` crate)

Hand-written parser replaces `regex-syntax`; explicit `Negated`
flag on character classes; HIR is a first-class IR that the e-graph
can rewrite like any other node. This subsumes `simplify_regex_
algebra` + `merge_regex_alts` at Tranche H-7 and becomes the input
to the DFA codegen in the bbnf crate. Per `perf-breakthrough-
accuracy` memory, bespoke-regex-HIR is the second real breakthrough.

### 3. IIFE elimination / inline scanner fusion (Tranche W.4 + AQ / AR)

Hot-path clone elimination at `7cb0015b` + derive-Copy on the parser
enum at `418efa95` + alloc_slice_copy. The scanner call sites stop
allocating per-byte, closure-wrapped visitors collapse to direct
`fn` calls. Per `perf-breakthrough-accuracy` memory, IIFE
elimination is the third real breakthrough.

### 4. NodeId-keyed analysis + IndexMap determinism (Tranches L + N)

`f6119e0b` purges pointer identity from passes; `9932d348` switches
the dependency graph to `IndexMap`. Together they make `generated.
rs` byte-stable. The invariant: *clean regen discipline*
(`clean-regen-discipline` memory) — generated files are always the
output of a fresh regen; never hand-patched. Every tranche regen
after N either produces cycle-1 = cycle-2 byte-identical output or
is rolled back.

### 5. Single decision surface via `PayloadKind → TypeDesc` (Tranche AQ.5, `2f7c1bd4`)

The deletion of `EmissionTier` and structural dispatch. Two parallel
axes of decision-making (*what tier does this rule emit in?* and
*what type does this rule project?*) collapse into one: the type
determines the tier. Per `no-orthogonal-codepaths` memory, "never
two decision surfaces for one semantic" is the lesson; AQ.5 is the
code. Roughly two weeks of AF/AG substrate absorbed cleanly.

### 6. Tape-first columnar substrate (Tranches AA + AC + AE)

The parse output is a flat `Vec<TapeRec>` with payload arenas and
`sib_skip` for parent/child navigation. No per-node allocations; no
`Value` tree; no per-rule `Parsed<T>` wrappers. The AU.1.1 tag-in-
`child_off` fix demonstrates the substrate's elasticity: canada.
json's 111K f64 payloads overflow `u16`; the fix folds the tag
into a `u32` range without API churn. The substrate has survived
four major Era-V reversals (column split, then re-unification).

### 7. Typed-materialisation invariant (Tranches AQ → AV.0)

"Every `->` in the grammar must reach the tape emitter; inference
composes types, never loses them; parity = full typed-AST
equivalence" — per `typed-materialization-invariant` memory. AQ
established the invariant; AT and AU audited its violations
(typed-parity-audit.md Bug 1, Bug 2, Bug 2b); AV.0 closed every
violation. The invariant is now load-bearing for every parity
harness in `tests/*_parity.rs`.

## Part E — Approaches that failed — 7 techniques

### 1. EmissionTier three-tier emission (Tranches AF + AG, deleted at AQ.5)

`MustTape` / `MustFn` / `MayInline` axis set orthogonal to payload
type. Two parallel decision surfaces for one semantic. Cost: ~32
commits across AF (26) + AG (6). Root cause: the tier was always
derivable from the payload's scalar-vs-aggregate distinction; making
it its own CSP variable produced inconsistent solutions when
inference yielded conflicting signals.

### 2. Full structural pre-scan (Tranche AO, `e64164e4`, deleted at AQ.5)

A document-wide structural pre-scan feeding dispatch decisions. Cost:
substrate landed in AO (2 commits), AP (9), AQ's first half (4).
Root cause: the pre-scan's data flow did not compose with `@host`
directive dispatch; keeping both alive required a second recognizer
pipeline. Deleted alongside EmissionTier at AQ.5.

### 3. DTA interpreter + PSI pipeline (Tranches AV → AW-V, deleted at AX.W0b)

A table-driven interpreter (~78,500 LOC reclaim at AX.W0b) that
was supposed to replace the `fn __<rule>` descent with a faster
structure-aware automaton. Cost: ~400 commits across AV / AW-I / AW-
II / AW-III / AW-IV / AW-V. Root cause (per AX.md proposition 4):
"novel levers compound only when they share a substrate AND a
demonstrable floor. V's substrate-first-consumer-later anti-pattern
must not recur." The DTA was substrate-first; five consecutive
tranches shipped it without a grammar running through it in
production. AW-V demonstrated it on JSON at W3 and lost the
demonstration by W6.

### 4. Full AW wave portfolio (AW-III / AW-IV hard gates, missed)

`AW-III` declared hard gate "strict-better-than post-AU on ≥ 15/19
entries." Result: 0/19 entries. `AW-IV` declared "every entry
exceeds post-AU; parity harnesses CI-gated." Result: 0/19. Cost: 92
+ 93 = 185 commits. Root cause: portfolio plans that ran parallel
substrate waves without per-wave runtime evidence. AX invariant 13
prohibits ledger-only waves from closing — authored specifically
against AW-III / AW-IV.

### 5. `StructRegistry` central IR registry (Tranche AS.2.3, deleted at AU.4.2 `ab8588a`)

A `HashMap<Name, StructLayout>` on `GrammarIR` meant to centralise
struct payload layouts. Cost: ~3 commits. Root cause: the `no-
backward-compat` invariant pushed toward per-backend type tables
(Rust emitter owns Rust layouts, TS owns TS layouts). A central IR
registry would be the fallback path; deleted as legacy-shape.
AV's `LargeAggregate` variant replaces the need.

### 6. Tape columns (Tranche Y, reverted at AY-I.W1)

The 7-column AoS split landed in Y with great ceremony and became
the measurement substrate for AU through AW-V. AY-I.W1 reverts to
a single `Vec<TapeRec>` + parallel `sib_skip`. Cost: every bench
regression between Tranche Y and AY-I.W1 carried the column-split
overhead. The revert is not a criticism of Y's decision at decision
time; it is a demonstration that "durable" in this codebase means
"durable until a later tranche proves otherwise."

### 7. AW-V `auto-derive the sonic-rs-class inner loop` thesis

The thesis itself: *one emitter auto-derives the inner loop for
every grammar*. The thesis was demonstrated on JSON at W3
(`c1e86ab3`) and lost by W6. AX absorbed the reality into a
view-layer + canonical-parity surface. Cost: 80 commits of AW-V
plus the AX W0c "AW-V plan-document rewrite in RD language" that
was required to close the tranche honestly. Root cause: the thesis
required every grammar's structural signature to fit into the
same dispatcher template; CSS and Sheets formula grammar do not.

## Part F — The "grammar-derived everything" thread

The idea enters at Era II (`29b17895` — codegen refactored to IR-
based architecture). Era III hardens it (`ce9d213b` NodeId keyed).
Era IV makes the grammar the *single source of truth* for payload
layouts (AQ.5 PayloadKind→TypeDesc). Era V attempts to extend the
thread to *runtime structure* (DTA + PSI + ShapeRef) and fails.
Era VI restates it clearly: per `backend-agnostic-types` memory,
"Grammar types are abstract (TypeDesc::Named); each backend resolves
to native types"; per `grammar-authoritative-status` memory,
"Grammar-authoritative migration: Phase 1+2 done, Phase 3 (host
fns) pending."

Current standing at 2026-04-22:

- **Payload types**: grammar-derived. The `->` annotation IS the
  decision; inference composes.
- **Scan policies**: grammar-derived at codegen (`STRUCTURAL_SCAN_
  POLICY` spliced into `emit_path_walk` at AY-II.W0'.c via
  `30aa83aa`).
- **Keyword classifiers**: grammar-derived (PHF tables per grammar).
- **Shape dispatch**: grammar-derived *architecturally* via the
  shape emitter; only JSON has a runtime path through it. CSS /
  Sheets / BBNF fall back.
- **View surface**: grammar-derived at AX.W1r.1 — IR-derived named-
  type resolver replaces static `BINDINGS`.
- **Host functions**: still a planned Phase 3; `@host` directive
  support partial.

The thread has never been fully compromised, but it has been
*consistently outrun* — every attempt to derive runtime structure
from the grammar (not just compile-time structure) has encountered
the grammar's own complexity. The AX.W1r pivot accepts this:
canonical-form parity against external comparators is the
generality claim, not runtime-automaton identity.

## Part G — The AU-baseline story

What AU-baseline was concretely on 2026-04-15:

**Correctness** (per `docs/tranches/AU/FINAL.md`):

- Seven waves landed across 2026-04-14 / 2026-04-15.
- `branch_pushes_children` true for Ref → `InlineBody` rules.
- All JSON `value` typed payload captures live (prior: dead stores).
- `grammar_roundtrip` 6/6, `payload_layouts` 13/13.
- CSS L4 parsed normalize + bootstrap + tailwind end-to-end.
- Workspace 967 pass / 33 fail / 30 ignored (`--no-fail-fast`).

**Performance** (per `docs/benchmarks/post-AU.json`):

- JSON canada 1,231 MB/s, citm 2,438 MB/s, twitter 1,967 MB/s,
  data_s 1,746 MB/s, data_xl 1,179 MB/s.
- CSS normalize 735 MB/s, bootstrap 454 MB/s, tailwind 496 MB/s.
- Sheets parse_simple 95 MB/s, parse_nested 128 MB/s, parse_stress
  121 MB/s.
- BBNF bbnf_self 394, json 283, ebnf 223, google_sheets 858,
  css_pretty 647, css_l4_grammar 496 MB/s.

**Debt** (AU FINAL §4, routed to AV):

- Bug 1 — alt-lit per-branch payload-write loss.
- Bug 2 — `-> Span` shorthand lowering.
- Bug 2b — `-> i64` / `-> f64` scanner-to-payload threading.
- Named-color factor-pass loss (35/148 branches dropped).
- Colour-function aggregate widening to ≥ 33 B.
- Empty-compound `has_payload` quirk.

**Performance gates AU missed** (carried forward to AV):

- canada target 1,800 MB/s — delta +46%.
- CSS bootstrap 600 — delta +32%.
- tailwind 1,200 — delta +142%.
- Sheets parse_simple 250 — delta +163%.

"Get back to AU-baseline" in 2026-04-22 terms means:

1. Every entry in the 17-entry bench matrix meets or exceeds the
   numbers above on the current substrate (post-AX.W0b +
   AY-I.W1 column revert + AY-II.W0' FusedBuilder collapse).
2. Bugs 1 / 2 / 2b remain closed (they do, per AV.0.x).
3. The parity harnesses against sonic-rs + lightningcss + simdjson
   + serde_json + cssparser remain green (they do, per AX.W1r).
4. No orthogonal decision surfaces (no `EmissionTier`; no
   `StructRegistry`; no DTA).
5. The view surface (`NodeView<'p>`, `TapeCursor<'p>`) routes every
   typed accessor.

AY-I.W1 recovered twitter to 688 MB/s — **35% of AU-baseline**.
AY-II.W0' has not yet published a fat-LTO bench matrix; B1 blocks
it. The gap from 688 to 1,967 is what Era VI owes.

## Part H — The push-vs-unpushed story

Master is 945 commits ahead of `origin/master` (`git log --oneline
origin/master..HEAD | wc -l`). Every commit from AU open (2026-04-
13, `ff757215`) through the meta-audit (2026-04-22, `48e6eaa9`) is
unpushed. The breakdown:

| Phase | Approx commits | Content |
|---|---:|---|
| AU (opening through close) | ~35 | Projection activation + scanner truth + FINAL |
| AV | ~53 | The Flattening + substrate + V0 bug close |
| AW-I / II / III / IV / V | ~250 | DTA activation arc |
| AX | ~169 | RD Reckoning + W0b interpreter deletion + W1r view |
| AY-I + AY-II | ~105 | Restart + W0' FusedBuilder collapse |
| AZ + B0 + B1 | ~38 | Planning + prelude annexes |
| Infill (regens, docs, bench captures, orchestrator housekeeping) | ~295 | |

The 24 feature branches carry ~35 commits not on master. They are
primarily per-agent worktree sources (`ax-w1r-*`, `ay-*`, `az-*`,
`aw5-w1-1`) whose contents were folded into master via cherry-pick
at wave close; the branches survive as provenance.

Two feature branches worth noting:

- `fix/at-7-css-attr-selector-ident` (ahead 1, behind 28) — one
  orphaned fix predating AT-7's resolution.
- `codex/bbnf-memo-bench` — a 2026-03-18 experimental bench branch,
  pre-Era-IV. Not integrated.

**The user-named "900+ unpushed" framing is an undercount.** The
actual count is 945 on master plus ~35 across feature branches,
for ~980 commits of unpushed engineering. The user's "nearly 2000
extant since the tranche push" matches the 1,923 total all-refs
count.

## Part I — Lessons learned

### 1. Refactor → optimize → semantic order (per `refactor-first-order`)

Every tranche that mixed semantic changes with optimisation work
spent cycles untangling the mixture. Tranche V's ordered breakdown
(refactor, then optimize, then grammar/semantic) is the template;
AW-III violated it by running three emitter passes and five consumer
activations in parallel — and missed its hard gate on every entry.

### 2. Every `->` must reach the emitter (per `typed-materialization-invariant`)

The invariant survives every era. AU introduced it; AT discovered
violations; AV closed every violation. Every subsequent compile
checks the parity. No shortcut that discards computed values
(`.map(|_|())`) survives past its discovery commit (see `no-value-
discard` memory).

### 3. Don't defer optimisations (per `no-deferrals`)

AV deferred V6–V9 to AW. AW-III deferred 10 of its 19 hard-gate
entries to AW-IV. AW-IV deferred to AW-V. AW-V deferred to AX. AX
deferred W2–W14 to AY. Every deferral surfaced as a dependency that
the later tranche could not absorb without re-planning. The AY-II
`PATH-FORWARD.md` acknowledges: "B1 owns infrastructure only. AY-II
resumes afterward." Deferrals are now bounded and documented.

### 4. Never two decision surfaces for one semantic (per `no-orthogonal-codepaths`)

AQ.5 is the paradigm. `EmissionTier` was one decision surface; the
payload type was another; the two were supposed to compose. They
did not. The deletion saved the project. AX invariant 1 ("one
codegen path — no hybrid, no fallback") is the hardened form.

### 5. Substrate-first-consumer-later is the anti-pattern (per AX proposition 4)

AV + AW-I + AW-II + AW-III + AW-IV + AW-V all shipped substrate
(constants, tables, shape dicts, emitter hooks) without a runtime
consumer. Each tranche claimed the next would consume; none did.
AX invariant 13 ("ledger-only wave = re-plan trigger") retroactively
names the pattern. Every substrate landing now requires a runtime
call site at the same commit.

### 6. Bench-checkpoint mid-wave (per AX invariant 10)

AX introduced mid-wave bench captures. `docs/benchmarks/post-AX-
W<N>-{mid,close}.json`. A regression ≥5% triggers re-plan. AW
ran ledger-only; AX ran measurement-gated. The bench discipline
is not an overhead; it is the discovery mechanism.

### 7. Build infra first (per `build-infra-first` memory)

B0 + B1 exist because Era V's dev loop was not trustable. The
bench runs took minutes; the expand outputs were stale; the
samply captures silently missed symbols. B0's `profiling-prep`
profile and B1's command-surface freeze are the corrective. Per
the memory: "Build/test infrastructure improvements land FIRST
in any tranche where dev iteration time is a bottleneck — never
deferred to later waves."

### 8. Triumvirate discipline (per `triumvirate-discipline` + `-auto-trigger`)

Research commit → plan commit → redress dispatch. Era V's failures
were frequently orchestrator-dispatched without a research anchor;
agents raced on overlapping files; JSONL-quiet intervals went
15+ minutes. Era VI's `triumvirate-auto-trigger` memory requires
the three-agent sequence unconditionally after the trigger. Audit
directories (`AW/audit/`, `AX/audit/`, `AY-I/audit/`, `AY-II/audit/`)
are the research-commit artefact.

## Part J — Decision quality ledger

Every tranche with a visible pivot or reversal.

| Tranche | Pivot SHA | What was reversed | Reason | Commits consumed before reversal |
|---|---|---|---|---:|
| H | `c7269f6b` | `simplify_regex_algebra` + early `merge_regex_alts` | E-graph subsumption | ~3 |
| H-DAG | `834dccf1` | `project_types` test-time fallback | DAG ordering makes it unnecessary | ~1 |
| AL→AM | `4d1afeb0` | Four workspace-blocking regressions | Triage-before-forward | ~2 |
| AO | subsumed at AQ.5 | Structural pre-scan as standalone | Non-composable with `@host` | ~15 (AO + AP + AQ.early) |
| AF+AG | `2f7c1bd4` (AQ.5) | `EmissionTier` three-tier axis | Payload type subsumes tier | ~32 |
| AS.2.3 | `ab8588a` (AU.4.2) | `StructRegistry` central IR registry | `no-backward-compat` + per-backend tables | ~3 |
| AU.1.1 | `83357e4` + later | `payload_idx: u16` overflow | canada 111K f64 payloads | ~1 |
| V0 | AV.0.1 `fb1f08a` | AU Bug 1 per-branch payload-write loss | Alt-lit loss | ~3 |
| V0 | AV.0.2 `d82c997` | AU Bug 2 `-> Span` shorthand | Bare Span admission | ~2 |
| V0 | AV.0.5/0.6 `e7add15` | AU empty-compound `has_payload` quirk | NONE variant | ~3 |
| AW-II | Plan revision | 3 architectural surfaces un-migrated | Consumer-only invariant | 40 |
| AW-III | AW-IV open | Hard gate 0/19 strict-better | Ledger-only waves | 93 |
| AW-IV | AW-V open | Hard gate 0/19 exceed | Substrate-without-consumer | 92 |
| AW-V | AX.W0c rewrite | "Auto-derive the sonic-rs-class inner loop" thesis | Grammar structural signatures incompatible | 80 |
| AX.W1.A+B | AX.W1r.0 `3429aaba` | Hand-coded `bbnf::json::Value` / `bbnf::css::StyleSheet` | 6,128 LOC legacy adapter; `backend-agnostic-types` invariant | ~6 |
| AX.W0b | `a206b962` | DTA interpreter + `dta_walker/` + `emitter/dta.rs` | Interpreter as architectural debt | ~572 (entire Era V arc before reckoning) |
| AY-I.W1 | Column revert | Tranche Y's 7-column AoS split | Cache locality favours 1-Vec | ~400 (Y through AW-V carried the split) |
| AY-II | `PATH-FORWARD.md` | "Parallel-infra path" posture | B1 must close first | ~10 |

Total reversals captured: 18. Commits consumed before a reversal
landed, summed: roughly 1,350 — which substantially overlaps because
AX.W0b subsumes all prior Era V work. Net "duplicate" work (commits
that reverted earlier commits and were not themselves rewritten):
estimate ~200–300 commits. The cost-of-indecision line item most
worth naming: *the 572-commit Era V arc recovered as the view-layer
+ parity-harness surface* — the substrate is not wasted, but the
direct runtime activation attempt is discarded.

## Closing note

The archaeology's single highest-leverage lesson: **measurement
gates substrate**. The Era V tranches that shipped the most
substrate are the ones that missed their hard gates by the widest
margins because they substituted ledger verification for runtime
evidence. AX invariant 13 names this pattern; Era VI's B0+B1
prelude embodies the remediation. Every future tranche should open
with the measurement surface already truthful — not with the
measurement surface as W-N's deliverable.

The second highest-leverage lesson, a corollary: **reversals are
the signal the system is healthy**. AQ.5's structural-dispatch
deletion, AU.4.2's `StructRegistry` deletion, AX.W0b's interpreter
deletion, and AY-I.W1's column revert are the project's four
most architecturally valuable commits. Each reversed a plan-
declared durable substrate once later evidence demonstrated the
substrate did not compose. A tranche discipline that cannot
reverse is a tranche discipline that accumulates unbounded
architectural debt.
