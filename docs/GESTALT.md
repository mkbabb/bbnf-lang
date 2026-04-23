# GESTALT — bbnf-lang, Universal Overview

A senior engineer joining the project reads this document first. Every
claim cites a commit or a file path; every number is measured. The
source corpus is the full audit record at master `48e6eaa9` plus eleven
worktree branches totalling roughly 15,000 lines of tranche
documentation, twelve of them new within the last calendar day.

## 1. Abstract

bbnf-lang is a grammar-derived compiler fleet. A BBNF grammar, typed by
`->` annotations on rules, is lowered through an IR-pass substrate
(`crates/ir`) into backend emitters that produce tape-first runtime
parsers for Rust, TypeScript, and — pending BA — WebAssembly. The IR
is optimised by a pluggable CSP solver (`crates/csp-solver`) and a
pluggable e-graph (`crates/egraph`), both grammar-agnostic; grammar
semantics flow in through `IrNode` alone. Generated parsers emit into
a flat `Vec<TapeRec>` of 16-byte records with opaque-payload scratch
tapes, modelled on simdjson's tape shape. A parse-that substrate
(`../parse-that`) carries the parser-combinator surface and a bespoke
HIR→NFA→DFA regex engine (`bbnf-regex`) that replaces the `regex`
crate at emission sites. A pprint substrate (`../pprint`) carries the
gorgeous auto-formatter.

The fleet state at the audit close: 1,859 master commits
(post-audit-merge), 945 unpushed, 24 feature branches, 18 tranches
of substrate work landed between 2026-04-08 and 2026-04-15,
followed by a five-day DTA/PSI rut (Era V, 2026-04-15 → 2026-04-19,
~572 tranche-tagged commits) in which zero of seventeen benchmark
entries recovered the 2026-04-15 AU-baseline. Era V closed at
AX.W0b (`a206b962`, 2026-04-20) with the deletion of ~78,000 LOC
of interpreter substrate. Era VI, the current era, is the
infra-truth restart: AY-I landed column revert and honest
relinquish; AY-II paused at W0' for the B1 prelude; B1 pins the
toolchain, migrates the bench harness to divan, rewires CI, and
re-anchors measurement on a clean substrate. JSON twitter sits at
688 MB/s — 35% of AU-baseline 1967 MB/s. The runway delivers:
**B1** (dev-loop truth, 4 waves, one week), **AY-II.W0'** close +
W1–W5 resume on the tape substrate (infra-truth closure, two weeks
after B1), **AZ-I** (grammar-derived direct-to-struct for JSON +
CSS L4 + Sheets with `StructRegistry` closure and JSON twitter
recovery to ≥ 1967 MB/s; tape retained for BBNF only; two to three
weeks), **AZ-II** (BBNF self-hosting direct-to-struct via
two-stage bootstrap cutover + `crates/tape/` deletion; one to two
weeks), **BA** (lazy typed pointer-path queries over the
grammar-derived struct tree, isomorphic Rust / TS / Python
`path!` macro; two weeks), **BB** (e-graph rule inference with
e-graph-first equivalence filtering and the VM interpreter at
`crates/ir/src/vm/` serving as the ground-truth oracle on residue;
grammar-colocated rule storage in `crates/ir/src/rewrites/` +
`grammar/<name>/rewrites/`; automatic ranker + tiered review; two
to three weeks). The sequence is mostly strict: B1 precedes
AY-II.W0'; AY-II precedes AZ-I; AZ-I precedes AZ-II; AZ-II
precedes BA. BB may open in parallel with AZ-II because rewrite
rules operate on `IrNode`, which stabilises at AZ-I close
regardless of the BBNF cutover state. The tranche letters BA, BB,
BC renamed during the audit — AZ-I and AZ-II are new, BA takes
the slot formerly labelled BB, BB takes the slot formerly labelled
BC, BC is retired.

### Headline numbers at gestalt time

| Measure | Value | Source |
|---|---:|---|
| Master commits | 1,842 | `git log master \| wc -l` |
| Unpushed commits | 945 | `git log origin/master..HEAD` |
| Total commits across all refs | 1,923 | per 06-archaeology |
| First tranche-tagged commit | `a3fadf56` (Tranche F, 2026-04-08) | archaeology §headline |
| First `FINAL.md` | `5281ec23` (AU, 2026-04-15) | archaeology §Step 5 |
| First planning-only tranche | AZ (2026-04-20) | archaeology §headline |
| First pre-tranche annex | B0 (2026-04-20) | archaeology §headline |
| First meta-audit | 01-session-friction.md (2026-04-22) | 05-validation |
| Era V commit count | ~572 | 06-archaeology §Era-V |
| AX.W0b LOC reclaim | ~78,000 | 06-archaeology Part B |
| AX.W1.A/B revert | −6,128 LOC | 06-archaeology Era V close |
| AW-V parse-bench miss | 0 of 17 exceed post-AU | AX plan `4177a18c` |
| JSON twitter at AU baseline | 1,967 MB/s | `AU/FINAL.md` |
| JSON twitter at AY-I.W1 post-fix | 688 MB/s | AY-I FINAL.md |
| 93-ICE cluster location | `on_disk_cache.rs:663:9` | `TOOLCHAIN-SOTA.md` |
| Ambient rustc when ICE cluster appeared | `1.96.0-nightly (9602bda1d 2026-04-05)` | `TOOLCHAIN-SOTA.md` |
| Pinned rustc under B1 | `nightly-2026-04-11` | `B1/B1.md` invariant 16 |
| Workspace `.cargo/config.toml` aliases | 7 current → collapsed in B1.W0 | `08-abrogation-catalog.md` |
| `scripts/` entries | 19 | `08-abrogation-catalog.md` Part 1 |
| Abrogation catalog total items | 63 | `08-abrogation-catalog.md` |
| Divan sites post-migration | 61 | `repo-modernization/INDEX.md` |
| Current divan adoption | 0 | `07-appurtenant-assay.md` |
| Appurtenant repos | 16 (4 sibling, 12 workspace) | `07-appurtenant-assay.md` |

The numbers are measured, not estimated. Every row cites its
authoritative document within the audit corpus.

## 2. What bbnf-lang is, from first principles

A BBNF grammar describes both a recogniser and the type of its
accepted value. Every rule carries an optional `-> T` annotation.
Type inference composes these — `feedback_typed-materialization-invariant`:
*every `->` in the grammar must reach the tape emitter; inference
composes types, never loses them; parity = full typed-AST equivalence*.
The grammar is the single source of truth. Hand-written
`bbnf::json::Value` or `bbnf::css::StyleSheet` containers do not
exist at HEAD; the earlier AX.W1.A/B experiments that produced them
landed and reverted at −6,128 LOC on 2026-04-20 under
`feedback_grammar-authoritative-status`.

The phrase "grammar-derived" does real work. It is not decoration.
A typical parser-generator treats the grammar as input to code
generation and the generated code as the authoritative artefact;
bbnf-lang treats the grammar as the authoritative artefact and the
generated code as its projection. The distinction matters at every
subsequent decision point. When the CSS L4 grammar declares
`length -> Length`, the emitter has no latitude to project `length`
as anything but a typed `Length`; if the lightningcss `Length` and
the bbnf-derived `Length` disagree in shape, the grammar is edited
to match, not the emitter. When a payload cannot be derived from
`->`, the grammar is extended (e.g., hybrid-grammar-host's Phase 3
adds host-function annotations for context-dependent semantics);
the emitter does not compensate for missing grammar information.
The discipline is what makes `feedback_no-backward-compat` viable:
all dev products migrate fully, because the grammar mediates change
and the emitter is a pure projection.

Four invariants bind the substrate.

**Typed materialisation.** `BA.md` states the consequence: every
`->` in every grammar reaches `push_leaf_with_*`, `begin_compound`,
or `end_compound`, and an IR audit pass enforces 100% coverage and
fails the build otherwise. The emitter never re-derives what the
grammar already declares. The `project_types` IR pass writes into
a `StructRegistry`; the emitter reads the registry; there is no
third party that opines on shape.

**No orthogonal codepaths.** `feedback_no-orthogonal-codepaths`:
arena allocation is a singular collection strategy; no conditional
Vec-vs-scratch branching; no combinator fallback alongside the
monolithic codegen; one regex system (HIR); KISS DRY. Tranche AQ.5
(`2f7c1bd4`, 2026-04-13) is the concretisation: the EmissionTier
lattice (`MustTape`/`MustFn`/`MayInline`) and structural dispatch
were deleted in one commit, collapsing two orthogonal decision
surfaces into a single `PayloadKind → TypeDesc` projection. Two
weeks of AF/AG substrate were subsumed.

**Direct-to-struct.** `feedback_direct-to-struct-approach`:
generalise regex-to-value conversion; no hard-coded pattern lists;
every `->` in the grammar projects directly to a typed record at
emission time, without an intermediate untyped phase. `project_types`
is the single projection pass; there is no parallel shape-derivation
pipeline.

**Grammar-authoritative.** The grammar owns leaf semantics through
`->`; host functions cover context-dependent and recursive
computations that the grammar cannot express. Hybrid-grammar-host
(`feedback_hybrid-grammar-host`) is the current migration posture
(Phase 1+2 done, Phase 3 host-fns pending per
`project_grammar_authoritative_status`). Backends see `TypeDesc::Named`
as abstract names; each backend resolves to native types via its own
registry (`feedback_backend-agnostic-types`). The CSP and e-graph do
not know which language they target; the emitter does.

The four invariants interlock. Typed materialisation requires direct-
to-struct, because anything else re-derives shape after inference has
already composed it. Direct-to-struct requires `no-orthogonal-codepaths`,
because a second projection surface would inevitably drift from the
first. `no-orthogonal-codepaths` requires grammar-authoritative,
because only a single source of truth can be canonical. Grammar-
authoritative requires typed materialisation, because without `->`
reaching the tape the grammar's authority ends at the parse boundary
and the runtime re-asserts its own types. The cycle is the core of
the architecture. AX's twenty-one invariants (declared at `4177a18c`,
2026-04-16) write these interlocking constraints out explicitly; Era
V's reverses were the visible symptom of violating one or more of
them at substrate level. Era VI's pauses are the mechanism for
verifying all four hold before substrate lands.

The ecosystem spans four repos. **bbnf-lang** owns the IR, the
workspace-internal crates (`crates/core`, `crates/ir`,
`crates/analysis`, `crates/lsp`, `crates/ser`, `crates/gorgeous`,
`crates/bootstrap`, `crates/egraph`, `crates/egraph-derive`,
`crates/csp-solver`, `crates/tape`, `crates/simd-scan`,
`crates/derive`, `crates/json-prototype`), the grammars, the
benchmark surface, and the CLI. **parse-that** owns the combinator
substrate (`parse_that`), the bespoke regex engine (`bbnf-regex`),
and `regex-bootstrap`; path-patched into the bbnf-lang workspace
through `.cargo/config.toml`. **pprint** owns the auto-formatter
runtime consumed by `crates/gorgeous`. **csp-solver** owns the
general CSP solver at `../../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver`;
the workspace member `crates/csp-solver` is currently a vendored
copy, reconciled to sibling-authoritative-but-workspace-bench in the
Wave 2 repo-modernization plans.

The choice to host general-infra crates outside bbnf-lang is
`feedback_general-infra-crates`: general-purpose constructs
(e-graphs, cost models) in their own crate(s), not stuffed into
domain crates. The e-graph is in `crates/egraph`; the CSP is
jointly in the sibling and `crates/csp-solver`; the regex engine
is in `../parse-that/rust/regex/`. Each of these has its own
optimisation architecture internally (`feedback_regex-crate-isomorphic`),
so the bespoke regex crate benefits from the same egraph-based
rewriting that bbnf-lang uses at grammar level. WASM bindings for
general-infra crates live as sub-crates inside the parent
(`feedback_wasm-subcrate-pattern`): a cargo workspace member, a
cdylib, a path-dep, isomorphic to the existing Python binding
location. The pattern means bbnf-buddy's procedural SVG mascot can
re-use the same CSP solver that bbnf-lang uses for cost modelling,
because the solver is not coupled to bbnf-lang's use.

## 3. The six eras — one diagram, one paragraph

Six eras, each with its own architectural thesis, each inheriting
the substrate the prior era produced. The commit-count-per-era
ratio sharpens as the project matures: Era II at ~14 commits per
day, Era III at ~11, Era IV at ~31 (tranche discipline begins
compounding output), Era V at ~114 (substrate explosion under
activation anxiety), Era VI at ~43 (pause-and-verify protocol
slows output deliberately). Era V's 114-per-day commit rate is a
symptom visible in retrospect; at the time, the rate read as
progress against a plan. The archaeology's value is exactly this —
the rate differential that flags plan-drift earlier in future eras.

```
 Era I  ── LSP/TextMate prelude ──────────────────── 2023-03-03 → 2023-03-06 (25c, 4d; 3-yr hiatus)
         │ [three-year gap]
 Era II ── Monorepo + IR scaffolding ──────────────── 2026-02-26 → 2026-03-15 (~264c, 18d; no tranches)
         │
 Era III ── Optimiser substrate ────────────────────── 2026-03-16 → 2026-04-09 (~280c, 25d; F–W)
         │ CSP / e-graph / regex HIR / NodeId / IndexMap determinism
         │
 Era IV ── Tape-first runtime ──────────────────────── 2026-04-10 → 2026-04-15 (~185c, 6d; Y, Z, AA–AU)
         │ 16-byte TapeRec / Lever 4 / EmissionTier / AU-baseline FINAL
         │                                                                 ► AU-baseline anchors everything
 Era V  ── DTA / PSI rut ───────────────────────────── 2026-04-15 → 2026-04-19 (~572c, 5d; AV, AW-I..V, AX)
         │ Dispatch-table automaton / columnar tape / shape emitter
         │ 0/17 bench entries strict-better at AW-V close
         │ AX.W0b deletion: ~78,000 LOC reclaimed
         │
 Era VI ── Infra-truth restart ─────────────────────── 2026-04-20 →        (~130c so far; AY-I, AY-II, B0, B1 + AZ-I / AZ-II / BA / BB planned)
           Column revert / AY-II path-forward / B1 dev-loop truth
           (AZ-I / AZ-II / BA / BB scaffold — not started)
```

**Era I** (March 2023) is the first commit of the original VSCode-
extension project, the TextMate grammar, an LSP prototype, a three-
year hiatus. Not architecturally continuous with the current fleet;
bridged only by `205310b` ("bump") on 2026-02-26.

**Era II** begins `cc499979` (2026-02-26) with the monorepo
restructure and runs through `1710d6f7` (2026-03-15) when the IR
crate stabilises. No tranche discipline; conventional commit
messages; the scaffolding that every later era inherits. Grammar
notation is frozen in this window; the workspace grows from a
single crate to the thirteen-member workspace Era III will extend.
The absence of tranche documents is not sloppy practice — tranche
discipline itself is an emergent artefact of Era IV; Era II's
commits are atomic, locally reviewable, and do not claim to
execute against a plan they have not written. The modern review-
before-act protocol that Wave 2 runs against eleven worktrees was
Era II's commit-then-PR habit generalised. Every Era III and IV
tranche inherits Era II's IR-pass substrate (`crates/ir`),
grammar-notation freeze, and monorepo layout without modification.

**Era III** (2026-03-16 → 2026-04-09, 18 tranche letters F through W)
builds the optimiser substrate. CSP solver, e-graph with scheduled
rewrites (Tranches H / J / K), NodeId-everywhere (`f6119e0b`,
Tranche L), IndexMap-deterministic codegen (Tranche N), bbnf-ir god-
module splits (Tranche Q), recognizer mining (Tranche V), kernel-
family modules (Tranche W). Every Era III tranche is "Worked" in the
archaeology ledger; none are reversed. The substrate Era III
produced carries every subsequent era. The CSP cost model defined
at Tranche W becomes the common variable surface for every later
decision point: regex engine selection, emission tier, wrap mode,
scanner family, and in BB, e-graph rule-application cost. The
e-graph's scheduler (Tranche J, `6becbf8b`, 2026-04-09) was
installed with real-work measurement at the outset, per
`feedback_csp-always-optimize` ("CSP optimization is always high
priority; foundational library, not gated by profile share"). Era
III's design discipline — `feedback_no-god-modules`,
`feedback_directory-module-structure`, `feedback_split-grammar-modules`
— produced a workspace shape Era IV never needed to refactor.

**Era IV** (2026-04-10 → 2026-04-15, tranches Y through AU) is the
tape-first runtime. Tape column splits (Y, reverted at AY-I.W1).
Cursor + reader surface (Z). TypeDescInterner hash-cons (AA,
`c209c380`). Tape-first shape-agnostic walking (AE, `85478284`).
Three-tier emission design (AF) — also reverted, at AQ.5. Structural
dispatch v1/v2 (AO, AP) — also reverted, at AQ.5. The AQ.5 commit
(`2f7c1bd4`) is the architectural inflection: `no-orthogonal-codepaths`
is enforced in code. Era IV closes at AU (`5281ec23`, the first
`FINAL.md`); the 17-entry AU-baseline becomes the measurement that
every later tranche cites. Era IV introduces the plan-execute-
progress-final cycle as a tranche's first-class shape: AU's
directory at `docs/tranches/AU/` establishes the convention that
AR onward adopts and that AV through AX maintain through the Era V
arc. AU's seven numbered waves (W1 through W7) introduce wave
discipline; AX's twenty-one numbered invariants formalise the
invariant-declaration-before-execute protocol; AY-II's
`PATH-FORWARD.md` introduces re-sequencing-against-audit as a
first-class tranche artefact. The evolution — commit tag → plan →
prototype → directory + PROGRESS → FINAL + waves → invariants →
audit + path-forward — visibly tightens the review surface each
tranche generation.

**Era V** is the DTA/PSI rut, treated in §4.

**Era VI** is where the project is now: AY-I landed the column
revert (Tranche Y's seven structural Vecs collapsed back to one
`Vec<TapeRec>` plus a parallel `sib_skip`), ending the column
experiment; AY-II wrote the rest-of-AY path forward and paused at
W0'; AZ opened as a planning-only tranche; B0 landed the bounded
prelude (profile tiers + `ay-*` Makefile); B1 is mid-flight. The
present is Era VI's infra-truth restart. AY-I's honest relinquish
— not "the thesis failed", but "the thesis's prerequisites were
unmet" — is Era VI's tonal signature. The commit record contains
three `FINAL.md` files admitting partial close (AY-I, AY-II, AV)
within the first eleven calendar days of Era VI. The feedback
memories that landed permanently in this window — `build-infra-first`,
`iter-profile-always`, `single-cargo-per-target`,
`test-output-to-file`, `bg-then-monitor`, `no-polling-loops`,
`status-tick-cadence`, `reconcile-task-census`,
`triumvirate-discipline`, `triumvirate-auto-trigger`,
`dispatch-hard-cap`, `abrogate-before-patch`,
`generated-size-budget` — all encode lessons from Era V's
execution-under-unreliable-infra experience. Era VI inverts the
posture: *verify the measurement surface first; run the tranche
second*.

## 4. Why Era V happened and what survives

Era V planned seven substrates: DTA (Dispatch Table Automaton,
grammar-derived table-driven parser replacing `fn __<rule>`), PSI
(Parallel Structural Index for document-level parallel parse),
columnar tape (seven typed Vecs replacing `Vec<TapeRec>`), ShapeRef
(compile-time shape dictionary at the cursor), PHF + SIMD keyword
classifiers, bloom + GADT runtime dedup, and the shape emitter
(auto-derived sonic-rs-class inner loop for every BBNF grammar).
The plan declared — `ca0875eb`, 2026-04-15 — that every `->` would
reach the tape, every typed AST would match its lightningcss or
sonic-rs counterpart node-for-node, no fallbacks, no legacy paths,
no workarounds. By `be4b22b1` four weeks later the seven substrates
had shipped. None reached break-even against the AU-baseline before
Era V ended.

**What failed.** Three orthogonal decision surfaces. The emitter
made `MustTape`/`MustFn`/`MayInline` decisions; structural dispatch
made a separate tier choice; PayloadKind made a third. These
interacted. Era V's invariants caught some of this — AQ.5 at the
boundary of Era IV collapsed two of the axes — but Era V planned
forward with assumptions that AQ.5 had deleted. The DTA walker was
also *substrate-first-consumer-later*: at every AW wave, the wave
shipped the compile-time emission of a table (the dispatch table, the
shape dictionary, the PHF table, the parallel structural index), and
routed the runtime consumer *forward* to the next wave. The consumer
never caught up. AW-V.W3 (`c1e86ab3`, 2026-04-17) *demonstrated* the
JSON shape-emitter thesis exactly once — workspace ran, twitter
emitted from the shape table — then W6 migrated consumers off the
demonstration and lost it. The `has_w4_classified` gate predicate at
`crates/core/src/backend/rust/emitter/grammar.rs:718` over-restricted
JSON's visitor-path; at AW-V close only JSON's `parse()` routed
through the shape emitter at runtime, while CSS/Sheets/BBNF still
delegated to `__dta_walker_inline::run` and sat at 3–7% of the
AU-baseline. AX's "RD Reckoning" (`4177a18c`, 2026-04-16) was the
honest recognition: the interpreter was to be deleted. At `bc550d2c`
and `a206b962` (2026-04-20, within AX.W0b.A) the DTA walker,
`dta_walker/`, `emitter/dta.rs`, and eight DTA-coupled test suites
were removed in one sitting. `b7aa41c0` carved seven dead
`GrammarProfile` slots and Lever 4 in the same cluster. `e839378c`
deleted the DTA-coupled test suites. `0d730c8f` retired the
`tape_parity_*` walker oracles per AX invariant 20.  `3429aaba`
(`W1r.0`) reverted the hand-coded `bbnf::json::Value` /
`bbnf::css::StyleSheet` containers from W1.A/W1.B at −6,128 LOC,
dropping the sonic-rs runtime dep to dev-only. `0adabb23` closed the
cleanup. Roughly 78,000 LOC reclaimed in one AX.W0b cluster. Era V's
plan had also assumed that a close-of-tranche ledger (tables written,
imports green, tests compile) was equivalent to runtime evidence that
the substrate worked; Era V's five closed FINALs each state the
ledger while recording 0/17 strict-better bench entries. Ledger-
verification substituted for runtime evidence. AX invariant 13 —
*a ledger-only wave is a re-plan trigger* — is the codification.
AW-IV's explicit hard gate was "every entry exceeds post-AU"; the
actual close was "0 entries exceed post-AU, 17/17 regressed". The
gap between declared gate and close reading is the precise shape of
the rut.

**What survives.** The VM interpreter at `crates/ir/src/vm/`
compiles at HEAD. The token-dispatch opcode machinery that the VM
needed for runtime interpretation is now BB's equivalence oracle
(see §5). The view layer and parity harnesses (`tests/*_parity.rs`
against sonic-rs, lightningcss, simdjson OnDemand, serde_json,
cssparser) are what AY-II cites as the parity reference. The
`bbnf-ir` crate itself — *one* analysis substrate, no parallel
`GrammarAnalysis` lattice (Tranche M deleted the lattice,
`359eb068`) — makes `project_analysis-consolidation` real: all
analysis lives in IR passes. The CSP + e-graph cost-model unification
(Tranches H / J / K / W) was unchanged by Era V and is what BB needs.
Debug infrastructure (`@debug`, source maps, `DebugBreak`, interpreter
hooks, compiled trace) landed across Era IV/V Phases 1-4 and is
durable. The `LargeAggregate` payload path (AV V2) and the
`PaddedView` SIMD cascade (AV V0) are consumer-facing improvements
kept at HEAD.

The specific AU bug-closure work that AV.V0 landed — AU Bug 1 typed
materialisation of alt-lit payloads, AU Bug 2 `-> Span` threading,
AU Bug 2b `-> i64` / `-> f64` scanner threading — is permanent. The
`GrammarProfile` const channel, of the 17 fields it originally
declared, retains 10 at HEAD after `b7aa41c0` carved 7 dead slots.
The shape emitter's JSON demonstration at `c1e86ab3` (AW-V.W3) is
not retrievable as code, but the demonstration is retrievable as
evidence: the thesis works for one grammar; the rut was in
generalising it to every grammar under Era V's decision-surface
constraints. The AZ-I / AZ-II plan does not resurrect the
shape-emitter thesis per se; AZ-I's direct-to-struct activation
is the same runtime outcome reached through a different substrate
path (StructRegistry + payload layouts + IR audit) that does not
require a second shape-derivation pass, and the AZ-I+AZ-II pair
goes further than the pre-audit BA framing by dissolving the
tape entirely (AZ-II.W3) rather than activating alongside it. The crate renames (`bbnf-tape` → `tape`,
`bbnf-simd-scan` → `simd-scan`, `bbnf-json-prototype` →
`json-prototype` — `b464a99c`, `1327491e`, `6ad76124`) reflect
Era V's honest recognition that these crates were not public-API;
the `bbnf-` prefix was removed so the name communicated the crate's
actual status. The canonical-form parity harnesses are the most
important testing artefact Era V produced: they define what
"bbnf-lang parity with lightningcss" means operationally — tree-
walk-equality node-for-node, not string-equality of serialised
output — and AZ-I.W3 cites them directly.

The twenty-one AX invariants are the procedural survival. They
declare: bench-checkpoint mid-wave (invariant 7); wire-contract
compile-gate (invariant 10); ledger-review at handoff (invariant 13);
frozen-contract rule for gate predicates (invariant 17); shape-
emitter as single source of truth (invariant 20). Era VI inherits
all twenty-one; the B1 sub-waves structure themselves around them;
both AZ-I and AZ-II cite invariants 7 and 13 directly in their
reversal criteria.

Era V's health signal is the three large reversals: **AX.W0b** (~572
commits' worth of DTA substrate deleted), **AY-I.W1** (column revert,
Tranche Y's 400-commit-equivalent substrate reverted to a single
`Vec<TapeRec>`), and **AQ.5** (32 commits of AF/AG structural-
dispatch substrate deleted at the inflection). The project reverses
decisively when evidence demands it. `feedback_no-workarounds`
(zero tolerance for workarounds, fallbacks, stubs, or legacy code
in any implementation) is what makes reversal a health signal: the
alternative — layering a workaround over a failing substrate — is
prohibited. Reversal is the mechanism of correctness maintenance.

## 5. The current plan — B1 → AY-II.W0' → AY-II W1–W5 → AZ-I → AZ-II → BA → BB

### B1 — dev-loop truth + proof-surface hardening

Bounded prelude annex. Four waves. W0 pins `rust-toolchain.toml`
to `nightly-2026-04-11` across bbnf-lang, `../parse-that`,
`../pprint` — eliminating the 93-ICE cluster at
`compiler/rustc_middle/src/query/on_disk_cache.rs:663:9` that the
meta-audit traced to an ambient `1.96.0-nightly (9602bda1d 2026-04-05)`
cache-staleness bug (not a bbnf bug). The ICE investigation in
`TOOLCHAIN-SOTA.md` found that every sampled ICE file (10 of 10
random) terminated in `#0 [analysis] running analysis passes on
crate bbnf_analysis`, with the immediate frames pointing at
`OnDiskCache::load_side_effect`, `DepGraphData::try_mark_previous_green`,
and `ensure_can_skip_execution` — a cache-decoder panic when the
rustc incremental cache encounters an `AttrId` it cannot decode.
The pin freezes rustc at a known-good date; the `cargo clean` plus
`rm -rf target/ax-iter/incremental` sequence that B1.W0 runs at the
start clears any corrupt cache entries from the ambient-nightly
period. Without the pin, sibling repos can drift to a rustc that
reintroduces the bug; B1.W2.c adds a CI job that diffs the pin
across bbnf-lang, parse-that, and pprint so that any drift is a
build failure. W0 also rewrites `.cargo/config.toml`,
`.config/nextest.toml`, and `Makefile`. W1 ships the exemplar divan
port (`crates/core/benches/compile_pipeline.rs`), the 18 remaining
bench ports, removes the `bencher = "0.1"` dev-dep outright, and
wires `iai-callgrind` into CI for instruction-count regression gating.
W2 rewires CI and absorbs the abrogation catalog — 19 scripts
verdicted KEEP / KEEP-MODERNIZE / REPLACE / ABROGATE / FOLD-INTO-TOOLING,
net −1,480 LOC and 37 Makefile targets consolidated. W3 finalises
`PROFILING.md`, writes `FINAL.md`, records the post-B1 17-entry
matrix. Hard gates: pinned substrate compiles the workspace clean;
every cargo alias resolves and exits 0; `iter-check-full` cold wall
recorded under the pin; ICE cluster cleared. Reversal criterion:
divan port introduces a measurement regression that is not a
benchmark-methodology artefact. Agent-slot count: 14 across four
waves. `bencher` deletion lands in the same commit as the final
ported bench; there is no dual-harness window — `feedback_no-backward-compat`.

### AY-II.W0' close + W1–W5 resume

AY-II.W0' consolidated `TapeBuilder` + `ValueBuilder` into
`FusedBuilder` (`bd563c1d`, 2026-04-21; follow-on `4edfac88` renaming
`finish` → `finish_fused`, `9c9906c8` retiring standalone
`ValueBuilder`, `0beda457` hoisting the single FusedBuilder parse-
entry, `f768f50d` landing O(1) `direct_child_count`) before the
infra audit revealed that the public command surface was diverged
from what the repo actually executed. `PATH-FORWARD.md` reordered
the remainder. `generated.rs` is still pre-regen with the bridge-era
parse entry, so AY-II.W0' is not formally closed; the W0'.a compose-
boundary aliases and shim surfaces are present by design until the
post-B1 regen replaces them (`feedback_no-backward-compat`'s
transient-boundary carve-out). B1 is the bounded prelude that must
close before W0' closes its ceremony and W1 dispatches. The paused
runtime work is typed-materialisation closure on the settled
parse-that substrate — no architectural surprises, but every gate
re-anchors against the post-B1 bench matrix. W0'.d1 through W0'.d7
already migrated `push_compound` / `mark_children` tests to the
FusedBuilder API, gated `gorgeous` derive sites, dropped `gorgeous`
as a mandatory dev-dep, narrowed the `build.rs` fingerprint, and
excluded heavy proc-macro crates from iter-check. W1 resumes on the
refreshed proof surface with all of this settled; agent-slot count
is specifically sized at W1 open against the measured post-B1
matrix.

### AZ-I — direct-to-struct for JSON + CSS L4 + Sheets

Opens on AY-II close. The first of two transformational tranches
of Era VI. AZ-I activates grammar-derived direct-to-struct for the
three data grammars and closes their `StructRegistry` coverage;
`crates/tape/` continues to exist and is used exclusively by BBNF
during AZ-I (the BBNF cutover + tape deletion is AZ-II's scope).
Every data grammar derives its native Rust struct shape from
`project_types` + `StructRegistry`; parse output lands directly
into those structs; four of the five tape roles (view-layer
intermediate, materialised-view shape, debug inspection surface,
parity-oracle substrate) migrate to the struct tree + `impl Debug`
+ `PartialEq`-based oracle comparisons for these three grammars.
`feedback_no-orthogonal-codepaths` binds: AZ-I exists because the
tape was a stepping stone, and the discipline forbids it
persisting alongside direct-to-struct on the grammars that have
active struct targets.

AZ-I opens with front-loaded classifier-unification research
(`docs/tranches/AZ-I/CLASSIFIER-UNIFICATION.md`). The research
studies how multiple payload kinds — `-> i64`, `-> String`,
`-> Color`, `-> Vec<Selector>` — compose in a single classifier
without collision, producing the unified dispatch shape before any
payload activates. The alternative — reacting to collision mid-AZ
with a sub-wave — was explicitly rejected as recipe-for-disaster
during the audit.

Wave structure at `AZ-I/AZ-I.md`: W0 research (classifier
unification + derive-cache lift to `$XDG_CACHE_HOME/bbnf-derive/`
with composite-key test suite) plus `StructRegistry` +
`project_types` closure audit; W1 hard-fail-and-block registry
closure for the three data grammars; W2 scalar payload
direct-to-struct for JSON + Sheets with twitter ≥ 1967 MB/s as
the hard gate — the AU-baseline recovery that AY-II deferred
lands here, proving direct-to-struct beats tape-plus-struct; W3
aggregate payload direct-to-struct for CSS L4 with lightningcss
node-for-node typed parity; W4 (FINAL) — 17-entry matrix parity
on the three data grammars, tape usage scoped to BBNF only,
AZ-I close handoff contract for AZ-II.

AZ-I's defensible floor: if classifier unification proves
intractable at W0 or `StructRegistry` closure leaves a data
grammar uncovered at W1, AZ-I closes with partial direct-to-struct
(JSON + Sheets) and tape retained for CSS + BBNF. The escape
clause is declared up front rather than discovered mid-tranche.

### AZ-II — BBNF self-hosting cutover + `crates/tape/` deletion

Opens on AZ-I close. The second transformational tranche. AZ-II
takes BBNF self-hosting onto the struct substrate via a two-stage
bootstrap cutover and deletes the `crates/tape/` crate entirely
at close. BBNF's grammar describes bbnf-lang itself; `project_types`
applied to `grammar/bbnf/bbnf.bbnf` produces a `BbnfAst` type that
mirrors `GrammarIR`, so BBNF has a native struct target like every
other grammar — there is no "undeclared struct target" fallback
role for the tape to fill. The fifth tape role (undeclared-grammar
fallback) dissolves here.

AZ-II's two-stage bootstrap resolves the circular dependency.
Stage A: the AZ-I pre-close tape-based compiler builds the AZ-II
struct-based BBNF parser candidate; `bbnf_derive` in the candidate
emits struct-writing parsers but the candidate itself was built
from a tape-writing parser. Stage B: the W1 candidate rebuilds
itself from its own source, producing a W2 compiler built from a
struct-writing parser *and* producing struct-writing parsers —
tape unwired in both directions. The close gate is byte-equal
reproducibility: the W2 compiler, run on every grammar in the
corpus, produces IR byte-equal to the pre-AZ-II compiler on the
same input.

Wave structure at `AZ-II/AZ-II.md`: W0 bootstrap research +
cutover design (`docs/tranches/AZ-II/BOOTSTRAP-CUTOVER.md`) +
classifier extension for BBNF-specific patterns applying AZ-I's
classifier unification; W1 Stage A (tape-based compiler builds
struct-based BBNF parser; byte-compare against pre-AZ-II output);
W2 Stage B (W1-candidate rebuilds itself; byte-compare against
Stage A output; hard gate: identical); W3 (FINAL) tape deletion —
`crates/tape/` removed, `crates/core/src/runtime/mod.rs` alias
shims retired, view codegen rewritten to target struct fields
directly, parity harnesses recoded to compare struct outputs —
plus `cargo build -p bbnf --no-default-features` succeeds
without requiring `crates/tape/`, the mechanical proof that the
crate is gone.

AZ-II's defensible floor: if W2 byte-equal reproducibility fails,
AZ-II closes with partial tape dissolution — `bbnf-tape-mini`
(a shrunken tape crate consumed only by BBNF) retained, with
tape-deletion-for-BBNF routed to a targeted follow-on tranche.
Drift sources (AST ordering, comment/trivia handling, numeric
formatting) are each addressed structurally in W0's cutover design
before W1 opens; unresolvable drift triggers the escape.

Both AZ-I and AZ-II share reversal criteria: wave-local 20% miss
reverts own substrate; parity-recovery precedence reverts any
substrate that regresses an already-passing AU-baseline entry;
no hedging forward. Ship no substrate without same-commit runtime
consumer and same-commit bench delta. Each tranche budgets for at
least one reversal per wave as the *expected* case, following
AQ.5's cleanest-reversal precedent (~32 commits) and explicitly
avoiding AW-IV's substrate-without-consumer anti-precedent (92
commits, 0/17 gate miss).

The split from a monolithic seven-wave AZ doubles end-to-end floor
probability (0.38 → 0.72 per `RISK-PERF-MATRIX.md §Cascade`) by
boundarising the single highest-risk piece (BBNF bootstrap
cutover) into its own tranche with its own reversal gates.

### BA — lazy typed pointer-path queries over the struct tree

Opens on AZ-II close. Tranche letter reassigned during the audit
(was BB in the pre-audit plan, promoted to BA when AZ absorbed
the old BA's scope). The substrate is the grammar-derived
struct tree — the tape dissolved at AZ-II.W3 — so every reference
to "tape backward pointer" or "sidecar column" in the pre-audit
plan translates to struct-tree navigation. The thesis is
otherwise unchanged: sonic-rs `pointer!` ergonomics, simdjson
OnDemand's forward-skip laziness, grammar-typed at compile time.
`path!("store", "books", 0, "title")` against a JSON grammar
resolves through IR type inference to a typed accessor against
the derived struct's field graph; invalid paths fail to compile
with a grammar-aware error. Strictly stronger than sonic-rs's
runtime path error.

Parent ascent, previously contemplated as a `TapeRec` sidecar
column under the pre-audit Q1 answer, becomes a struct-tree
question: embedded parent pointers in every struct node
(bloat-heavy but O(1) ascent), root-traversal from the AST
root (no bloat, O(depth) ascent on every path), or a hybrid
sidecar index parallel to the struct (zero bloat when feature
unused, one extra indirection on ascent). BA.W0 runs the
parent-pointer micro-bench across the three options; the sidecar
posture is the default per the audit's Q1 resolution translated
to struct-tree context. The `feedback_no-orthogonal-codepaths`
discipline applies: whichever option BA.W0 picks becomes the one
pattern every grammar's struct shape follows.

BA's handoff contract from AZ: `StructRegistry` populated for
JSON / CSS / Sheets / BBNF; IR audit pass at 100% `->` coverage;
17-entry AU-baseline at or above AZ-I.W2 close; `crates/tape/`
deleted. If AZ-I closed on a partial `StructRegistry` via the
defensible-floor escape clause, BA does not open until the
remainder lands — this is the hard-fail-and-block discipline
per Q2 and `feedback_typed-materialization-invariant`.

Operational posture: a `path!` macro plus a typed
`Path<Grammar, Target>` value; host bindings (Rust, TS, Python)
receive isomorphic signatures per `feedback_isomorphic-api`. Bench
surface: a lazy-path micro-bench suite extracting 3, 10, and
30 fields from citm, tailwind, sheets fixtures; every wave runs
both the micro-bench and the full 17-entry matrix at boundary,
with regression on either blocking wave close. Zero allocation
on traversal — intermediate state borrows from the struct —
verified by heaptrack or jemalloc-sampling at W1 close. BA.W2
ships the e-graph path-normalisation rewrites (duplicate prefix
elimination, redundant downcast elimination, path-fusion);
BB later extends this with inferred rules. Agent-slot count:
three waves plus FINAL, two to three agents each.

### BB — e-graph rewrite rule inference

Opens on AZ-I + AY-II close. BB does not wait for AZ-II — rewrite
rules operate on `IrNode`, which is substrate-independent (the
same IR shape whether emission targets tape or struct), and the
Tranche H ground-truth rule set is defined over IR patterns that
stabilise at AZ-I close. BB therefore runs in parallel with AZ-II
and its downstream BA. BA (pointer queries) is not a BB blocker. Tranche letter reassigned during
the audit (was BC in the pre-audit plan; promoted to BB when
BA absorbed the old BB's scope and BC was retired).

BB closes the loop on `feedback_pluggable-components` and
`feedback_csp-always-optimize` by letting the e-graph *discover*
grammar-level rewrite rules rather than only apply a fixed set.
Ruler-style (Nandi et al. 2021) CVC enumeration over `IrNode`
produces candidate terms up to a bounded size. The architecture
is **e-graph first, VM residue second** per the audit's Q3
resolution. The e-graph fast-path: if two candidates are already
in the same e-class under the current rewrite set, they are proven
equivalent; no VM call is needed. The VM acts as the non-circular
tie-break for the residue — candidates in distinct e-classes,
where the e-graph is silent and the question is whether they
are genuinely inequivalent or equivalent under a rule we have
not yet derived. Accepted VM-proved rules feed back into the
e-graph, extending the rewrite set; next enumeration pass,
their consequences are e-graph-free. The VM's workload is residue
throughput (< 10% of enumeration), not total throughput,
comfortably within the ~1800-LOC VM surface at `crates/ir/src/vm/`
that Era V's AX.W0b deliberately left at HEAD.

Rule storage is grammar-colocated and extensible per the audit's
Q4 resolution. Fleet-wide rules (associativity, commutativity,
absorption) live in a `crates/ir/src/rewrites/` module within the existing IR crate
rather than `crates/core` (`feedback_general-infra-crates`;
`feedback_no-core-dumping`). Grammar-specific rules colocate with
the grammar under `grammar/<name>/rewrites/*.ron` in a
standardised schema; the `bbnf_derive` macro scans the directory
at build time and compiles matches into the grammar's cost-config.
New grammars ship their own rules without touching core. The
schema declares name, LHS pattern, RHS pattern, cost delta,
provenance (hand-coded or BB-inferred), and discovery metadata.

Curation is automatic-ranked and tiered. The ranker lives at
`crates/ir/src/rewrites/rank.rs` and scores every candidate on
match frequency, cost delta, generality (grammars matched),
similarity to Tranche H hand-coded rules, novelty, and LHS
tree size. Tiered review: Class-1 trivial (algebraic identities
already in the ground-truth set) auto-accepts with machine-
generated justification and audit-log only; Class-2 structural
(candidates matching Tranche H pattern shape) fast-tracks;
Class-3 novel takes full human review. Target: > 90 % of
discovered candidates fall in Classes 1-2, so review time scales
on novel rules only. First-run review is bounded by the
Tranche H ground-truth set size — a finite, one-time effort.
Post-bootstrap enumerations produce delta candidates on IR
or cost-model changes, small and incremental.

The three durable VM surfaces BB uses are stated in `BB.md`.
First, the equivalence oracle on e-graph residue: two `IrNode`
candidates run through the VM against the fixture corpus, outputs
compared. Second, the cost-model reference: the VM's opcode count
per rule is a proxy for the cost the AOT path would pay if
inlined; BB uses this for cost-model calibration of CSP variables.
Third, the regression oracle: after a rule inference lands a
transformation, the VM runs pre-rule and post-rule forms on the
same fixtures to verify semantic identity. None require the full
DTA walker substrate; each uses only the token-dispatch opcode
machinery that AX.W0b deliberately preserved at HEAD.
`feedback_abrogate-before-patch` applies: BB does not re-open
the DTA walker; BB uses what remains.

The Tranche H factor / merge_regex_alts / inline_acyclic rewrites
that were hand-coded become the soundness check: BB's first
enumeration pass must rediscover them (possibly with different
surface presentation but semantically equivalent `IrNode`
transformations) before BB declares the mechanism sound. A
rediscovery failure at W0 is an immediate revert. Agent-slot
count: four waves plus FINAL.

## 6. The SOTA union — grammar-derived everything

The fleet is a compositional SOTA of parser literature pieces,
mediated by grammar-derived semantics. Each piece is taken for a
specific capability; each composes with bbnf's IR / CSP / e-graph
substrate through a grammar-side hook rather than a side-channel
runtime API.

**simdjson's tape.** 16-byte fixed records; compound open/close
record pairing; opaque strings on a scratch tape. bbnf adopts the
shape for every grammar, not only JSON — the tape is a first-class
runtime IR, populated by `push_leaf_with_*` / `begin_compound` /
`end_compound` calls that every `->`-annotated rule emits. BA.W1
adds the backward container pointer that simdjson itself does not
need (simdjson parses; it does not navigate), closing the one-way
asymmetry that Era IV left.

**sonic-rs's StructRegistry + `pointer!`.** Type-safe field access
through compile-time registration. bbnf adopts the registry as
`project_types` output — but populates the registry from grammar
`->` annotations, not from user `#[derive(StaticType)]` macros on
host-language structs. `pointer!` ergonomics become `path!` (BB)
with stronger compile-time validation.

**lightningcss's typed-value parity.** Parse `<length>` into a
typed `Length`, not a string; parse `<color>` into a typed `Color`;
every CSS L4 property rule returns the typed shape that
lightningcss produces from its hand-written Rust implementation.
bbnf derives the same shapes from the CSS L4 grammar — BA.W3 gates
parity node-for-node.

**Ruler's CVC rule enumeration.** Given a term algebra and an
equivalence oracle, enumerate terms up to a size bound, group by
equivalence, extract cross-class equivalences as rewrite rules.
bbnf uses `IrNode` as the algebra, the VM as the oracle, the
existing CSP cost model as the scheduler. BC's thesis.

**egg's e-graph substrate.** `crates/egraph` is the workspace
member; `crates/egraph-derive` derives the `Language` impl from
existing `IrNode` enum variants (`feedback_derive-language-macro`);
`crates/bbnf-regex` uses the same optimisation architecture
internally (`feedback_regex-crate-isomorphic`). The cost model is
CSP-modelled; the rewrite rules (factor, merge_regex_alts,
inline_acyclic) were hand-coded in Tranche H and will be e-graph-
inferred in BC.

**parse-that's combinator substrate.** The runtime parser surface.
A modern recursive-descent combinator layer with bespoke HIR
(`feedback_bespoke-regex-hir`) for the regex engine — explicit
negated flag, hand-written parser, no dependency on `regex-syntax`.
The `regex` crate does not appear in the emission path; `bbnf-regex`
replaces it through `[patch.crates-io]`.

**yyjson's dispatch-and-allocation frontier.** yyjson observes that
SIMD is not where the next 10% lives past a certain point; key
dispatch and in-place payload placement are. bbnf already ships AP.4
key dispatch (the Tranche AP structural-dispatch substrate that
survived AQ.5's rescope as a `PayloadKind → TypeDesc` projection)
and AP.5 NibbleLut. BA's direct-to-struct activation is the
in-place-payload piece; the grammar-derived StructRegistry tells the
emitter exactly which field receives each scalar payload, so the
emitter writes in-place without a two-stage "materialize-then-project"
pass. This is the yyjson reading applied at grammar abstraction
level — no per-grammar hand-written payload projection; the
projection is a single IR pass over every grammar.

The synthesis thesis: bbnf is a *compositional* SOTA of these
pieces, where composition is mediated by grammar-derived semantics.
simdjson's tape shape, sonic-rs's type registry, lightningcss's
typed values, Ruler's rule enumeration, egg's e-graph, parse-that's
combinators — each contributes a specific capability; each is
wired into bbnf through the grammar's `->` annotations, not through
a per-feature side channel. The IR is what makes the composition
coherent.

The grammar-derived mediation is what makes the composition
defensible at scale. A JSON-only speed-up that ships a JSON-specific
codepath is rejected at plan time (`feedback_preserve-rich-ast`:
never flatten typed grammar rules for speed; rich AST parity with
lightningcss is non-negotiable). A per-grammar parser hand-tuned to
beat a specific fixture is rejected at plan time. Every technique
bbnf adopts from the literature is applied at grammar abstraction
level, with the grammar's `->` annotations carrying the type
information that the technique expects. When BA activates scalar
payload directly to struct, it does so for *every* grammar with a
scalar `->` annotation, not for JSON's `value -> f64` alone. When
BB compiles pointer paths, the `path!` macro works for *any*
grammar, not for JSON's well-known structure alone. When BC infers
rewrite rules, it infers over `IrNode` — the grammar-agnostic IR —
producing rules that apply to any grammar by construction. This is
the composition principle: *the grammar is the only distinguishing
input, and everything downstream is uniform across grammars*.

## 7. The fleet — cross-repo shape

Sixteen rust artefacts across four sibling repos and twelve
workspace members. The appurtenant-assay counted them explicitly
on measured state at `48e6eaa9`: `../parse-that` with 18 benches and
a stray rustc-ICE file at `rust/parse_that/`, unpushed;
`../pprint` with 2 benches and a still-declared MSRV that Phase A
drops; `../gorgeous` with 2 benches destined for workspace
retirement; `../../csc411/.../csp-solver` with 6 solver benches
plus 2 morph-core benches destined for workspace-authoritative
promotion. Inside the workspace, `crates/core` carries 19 benches;
`crates/tape`, `crates/simd-scan`, and `crates/json-prototype` one
each; the remainder zero benches today. Divan adoption today: zero
across sixteen repos.

The repo matrix from the W2-F repo-modernization index reduces to
three phase groups.

**Phase A** (inside B1): pin `rust-toolchain.toml` in
`../parse-that`, `../pprint`, `../../csc411/.../csp-solver`;
create `.cargo/config.toml` under version control in parse-that
and csp-solver (both currently untracked per the assay); drop the
explicit MSRV declaration in pprint (Phase A prunes dead MSRV
statements); disable CI on the `../gorgeous` sibling pending
retirement.

**Phase B** (42-48 hours, post-B1): port 18 benches in parse-that
from `bencher` to `divan`; port 19 benches in `crates/core` (the
largest workstream); port 6 benches from csp-solver sibling into
the workspace `crates/csp-solver` (the reconciliation making
workspace authoritative for benches while leaving the sibling
solver authoritative for algorithm evolution); port 2 benches from
`../gorgeous` to `crates/gorgeous` as part of the sibling
retirement; add one new bench each to `crates/derive`,
`crates/ir`, `crates/egraph`, `crates/tape`, `crates/simd-scan`,
`crates/json-prototype`. Total divan sites post-migration: 61.
Current divan adoption across the 16 repos: 0.

**Phase C** (3-4 weeks, BA-parallel): deferred follow-ons —
derive-cache relocation to `$XDG_CACHE_HOME/bbnf-derive/`
(landed in BA.W0, not B1, because `crates/derive` is flagged
highest-risk); watt-based proc-macro precompilation; bench
architecture restructure.

**gorgeous-mirror disposition**: RETIRE. The sibling `../gorgeous`
duplicates `crates/gorgeous`; the 2 sibling benches migrate into
the workspace, CI disables on the sibling during Phase A, the
repo is archived post-Phase B.

**csp-solver reconciliation**: the workspace copy becomes
authoritative for benches; the `../../csc411/.../csp-solver`
sibling remains authoritative for algorithm evolution under
`project_csp-solver-crate`. `[patch.crates-io]` in bbnf-lang's
`.cargo/config.toml` points at the sibling; the workspace copy is
the bench home.

**Path-patches** in bbnf-lang's `.cargo/config.toml` reach
`../parse-that/parse_that`, `../parse-that/regex`,
`../../csc411/.../csp-solver`, plus the workspace-internal crates
each sibling re-exports. Proc-macro graph: `bbnf_derive` +
`egraph_derive` are workspace-internal; no sibling proc-macros
cross the boundary.

**Divan adoption after migration**: 61 sites. Current ambient
adoption across the 16 repos: 0 (every current bench uses
`bencher = "0.1"` or the older `criterion` in a handful of
sibling cases). The migration is one-shot; `bencher` is deleted
in the same commit as the final port (`feedback_no-backward-compat`).

## 8. The measurement discipline

*Measurement gates substrate.* AX invariant 13 codifies it: a
ledger-only wave — imports green, tests compile, substrate in place
but no runtime consumer exercising it — is a re-plan trigger, not a
close. Era V violated this five waves running; every post-Era V
tranche plan cites invariant 13 explicitly.

B1 re-anchors the measurement surface. `bencher = "0.1"` produces
`ns/iter` point estimates that do not model per-parse variance;
`divan` ships cold-per-parse samples with statistical distribution,
`--save-baseline` / `--baseline` JSON artefacts, and a `cargo
bench` UX. The `scripts/bench_regression.sh` script — a 89-line
Python file with a `.sh` extension that regex-scrapes `cargo
bench` text output against a JSON baseline — collapses to the
alias `cargo bench -- --save-baseline current && cargo bench --
--baseline main`. `iai-callgrind` adds instruction-count
regression gating to CI; wall-clock benches stay on bare metal,
instruction counts gate the contributor's PR. `feedback_no-warm-benches`
is enforced: every measurement is cold per-parse; warm/cached
benchmarks are disingenuous. `feedback_bench-sequential-regression`
is enforced: benchmarks run sequentially, never interleaved, and
check for regressions.

AY-II holds on the post-B1 refresh because every AY-II gate cites
a numeric floor; those floors re-measure under the pinned substrate
and the divan harness at B1 close. BA opens with an IR audit pass
plus a 17-entry bench re-anchor *before* substrate — BA.W0 lifts
the derive cache and re-measures the 17-entry matrix on the clean
BA-ready substrate; BA.W1 substrate only then lands. The sequencing
is deliberate: no substrate whose effect is measured against a
stale baseline.

The 17-entry AU-baseline matrix anchors every parity-recovery gate.
The seven highest-signal rows from `BA.md`'s parity-recovery table:

| Grammar / fixture | AU-baseline | BA floor | BA target |
|---|---:|---:|---:|
| JSON canada | 1,231 MB/s | 1,231 | 1,500 |
| JSON citm | 2,438 MB/s | 2,438 | 2,700 |
| JSON twitter | 1,967 MB/s | 1,967 | 2,200 |
| CSS normalize | 735 MB/s | 735 | 850 |
| CSS bootstrap | 454 MB/s | 600 | 700 |
| CSS tailwind | 496 MB/s | 500 | 600 |
| Sheets parse_simple | 95 MB/s | 95 | 110 |

Twitter currently sits at 688 MB/s (35% of AU-baseline), citm at a
lower share, tailwind at lower still. BA.W1's floor of 1,967 MB/s on
twitter is a recovery, not exceedance. BA.W1's target of 2,200 MB/s
is the first post-AU exceedance in project history if it lands.
Workspace gates per `BA.md`: pass count ≥ 967, fail count ≤ 33,
ignored count ≤ 30. Coverage gates: `grep -c 'push_leaf_with_'
crates/core/**/generated.rs ≥ count of scalar-payload ->` across
all grammars; `StructRegistry` non-empty for JSON pair/value, CSS
L4 declaration/dimension/colour, Sheets cell/formula, BBNF
rule/alt_branch. lightningcss typed-value parity: every `<length>`
rule in CSS L4 returns a typed `Length` equivalent to
`lightningcss::values::length::Length`, checked node-for-node on
the normalize fixture.

`feedback_actual-profiling`: run actual profilers (samply /
instruments / perf); do not guess from static analysis.
`feedback_samply-symbol-resolution`: samply needs `debug=true`
plus interactive `samply record` (not `--save-only`) for symbol
resolution. BA records samply profiles under
`docs/benchmarks/profiles/BA/<wave>/` before and after every
substrate change; no optimisation claim without a profile delta.

## 9. The instruction-layer discipline

The orchestrator and sub-agents share a rule set carved from real
incidents. These are not decorative; they are the protocol that
made the meta-audit archaeology possible and the twelve-branch
parallel waves tractable.

**Hard-cap on every dispatch.** `feedback_dispatch-hard-cap`:
every dispatch carries `HARD CAP: N min. At 0.9N commit, at N halt`.
Defaults 20 / 15 / 30 minutes for research / plan / redress.
Without the cap, sub-agents explore forever; with the cap, they
commit incrementally against a wall clock. This present doc's
dispatch carried a 50-minute cap; the commit schedule inside the
dispatch is explicitly *after main body lands, after appendix
lands*, so a timeout does not lose work.

**Status tick cadence.** `feedback_status-tick-cadence`: emit one-
line status tick every ~5 min of orchestrator-silent wait; never
make the user ask status twice. The discipline was carved from
sessions where agents fell silent under load; the tick is the
liveness signal.

**No bash-tail.** `feedback_no-polling-loops`: never poll sub-
agent or background progress via `ps aux`, `tail -f`, or sleep-
check loops; use the Monitor tool or `run_in_background`.
Validation V2 found Monitor adoption confined to exactly one
session of five — `4bec5721` — with 11 invocations; the other
four sessions had zero. The discipline is recent and uneven;
future work enforces it. `feedback_bg-then-monitor` is the
companion: any Bash invocation expected to take >60s must set
`run_in_background=true` and be followed by a Monitor call. The
meta-audit 01-session-friction document quantified the prior
pattern: 397 Bash calls in the largest session alone, many of
them polling loops that returned zero information over minutes of
wall clock.

**Triumvirate auto-trigger.** `feedback_triumvirate-auto-trigger`:
when a JSONL has been quiet for >15 min or a first-pass sub-agent
returns without a commit, the orchestrator dispatches a three-
agent triumvirate (research / plan / redress) without prompting
the user. `feedback_triumvirate-discipline`: research commits
attribution; plan commits plan; only then redress dispatches;
never merge roles. The Era V FINAL-III combined document is the
counter-example that motivated the rule.

**Worktree isolation.** Every dispatch in this fleet runs on its
own `worktree-agent-*` branch, disjoint file bounds declared in
the dispatch brief. `feedback_agent-orchestration`: never let
sub-agents race on shared files; commit before parallelizing; use
worktrees for overlap. The eleven branches this gestalt consumes
are eleven disjoint worktrees; none landed on master;
cross-branch synthesis happens only in the master commit that
merges them.

**Commit-before-parallelize.** Before every fan-out, commit the
shared state the fan-out depends on. The meta-audit commits
(`b8767b5a`, `823461d2`, `ea3313b6`, `0b59c7ba`) are four discrete
commits each preceding a parallel wave; the wave agents read
master at each commit's head.

**Empty-return redispatch.** `feedback_redispatch-empty-return`:
an empty sub-agent return is not scope-reveal; redispatch verbatim
with a prior-worktree pointer. The orchestrator does not
prematurely collapse scope on silence.

**Read-size preflight.** `feedback_read-size-preflight`: `wc -l`
before Read on any file > 2K lines; grep + offset for generated.rs,
transcripts, large audits. Era V's generated files ran to tens of
thousands of lines; naïve Read calls lost context.

**Generated-size budget.** `feedback_generated-size-budget`:
generated code has a per-tranche line-count budget; overflow
blocks a wave until the O(N) generator regression is traced.

**Archaic diction is voice.** `feedback_archaic-diction-is-voice`:
user's archaic diction (begets, therein, thereof) is deliberate
voice, not AI artefacts. Synthesis documents honour it.

**Iter-profile always.** `feedback_iter-profile-always`: every
iteration-loop `cargo check` / `cargo test` carries
`--profile ax-iter` explicitly; bare forms are heavy-surface. Meta-
audit 02 found 29 bare `cargo check -p bbnf --tests` invocations in
a single session with 26 GB peak RSS; the edict at
`docs/instructions/tranche/SPEC.md:577-594` now enumerates this
exact command under heavy-surface prohibition. `feedback_single-cargo-per-target`
is adjacent: at most one cargo invocation in flight per
`CARGO_TARGET_DIR` at any instant; lock contention silently
serialises and produces apparent hangs.

**Abrogate before patch.** `feedback_abrogate-before-patch`: for
intrinsic-failure subsystems, ask "can we delete?" before "can we
patch?". AX.W0b's 78,000-LOC reclaim was an abrogation decision,
not a patch decision. The 19-script abrogation catalog asks the
same question of each ad-hoc script: REPLACE (delete and substitute
a modern tool), ABROGATE (delete outright), FOLD-INTO-TOOLING
(merge into `.cargo/config.toml` alias). BC applies the pattern
forward: BC does not re-open the DTA walker to extract value; BC
uses only the VM surface that survives.

**Execute planned architecture.** `feedback_execute-planned-architecture`:
don't retreat from planned architectural changes under contact;
dispatch more agents, carry plan-declared unworkability, never
ship stub/shim. The rule is the mirror image of abrogate-before-patch:
once the plan declares an architectural change, execute it in full
or explicitly abrogate the plan; a mid-flight retreat that
half-ships the change is the worst outcome. AQ.5's structural-
dispatch deletion was a full execution of a planned reversal; the
AX.W1.A/B hand-coded `Value` containers were a stub that got
reverted under this rule. `feedback_no-workarounds-arch`:
architectural transpositions for elegance / simplicity / performance
are mandatory; no quick solutions.

**One codegen path.** `feedback_one-codegen-path`: one monolithic
codegen path; no combinator fallback; one regex system (HIR); KISS.
`feedback_no-combinators-monolithic`: zero combinator / SpanParser
usage in monolithic arena path; extend the classifier instead.
These two bind BA's substrate: the emitter has one codepath, not
two, and the StructRegistry is the single decision surface for
payload shape.

**No orthogonal codepaths.** `feedback_no-orthogonal-codepaths`:
arena allocation is a singular collection strategy; no conditional
Vec-vs-scratch branching. This is the enforcement mechanism for the
"one codegen path" principle at the allocator level. The Tranche Y
column-split was orthogonal to the base `Vec<TapeRec>`; AY-I.W1's
column revert is the enforcement of the principle against a
substrate that had drifted from it.

**Clean regen discipline.** `feedback_clean-regen-discipline`:
generated files are always the output of fresh regen; never hand-
patch. B0 cycle-1 = cycle-2 byte-identical bootstrap verified the
discipline empirically; any future divergence is a regression signal.

**Document alongside code.** `feedback_doc-alongside-code`: always
update SPEC.md and design docs alongside architectural changes. The
tranche documentation pattern itself — plan + progress + FINAL +
audit, all inside `docs/tranches/<X>/` — is the visible form of the
discipline.

**No metalanguage docs.** `feedback_no-metalanguage-docs`: docs must
never reference plans, commits, conversation history; standalone
prose only. This document honours the rule for its own subject
matter; the "commit X pivoted Y" references are operational
pointers, not meta-narration. Every tranche FINAL.md is a standalone
reading of what the tranche produced, not a transcription of the
plan's expectations.

**Inspect generated output.** `feedback_inspect-generated-output`:
always inspect expanded/compiled output when working on codegen;
use `cargo expand` + `cargo asm`. The `.cargo/config.toml` aliases
`expand-*` and `asm-parse` expose this as a first-class workflow
surface; B1.W0 keeps these aliases alive through the rewrite.

**Aesthetics critical.** `feedback_aesthetics-critical`: formatting
aesthetics are the purpose of gorgeous / pprint; never use
heuristic thresholds over actual configurable values. The gorgeous-
mirror retirement does not relax this; `crates/gorgeous` carries
the same discipline, just at workspace-authoritative location.

## 10. Decision record — the ten questions that gated the plan

Every architecture decision the audit surfaced is now expressed in
the tranche doc that owns its implementation. This section is the
canonical short-form record; the rationale, trade-offs, and
follow-up gates live in the named tranche doc for each decision.

**The tape is a stepping stone and dissolves entirely.** Shape C:
AY-II closes as scoped on the tape substrate; **AZ** (new) absorbs
direct-to-struct activation + full tape dissolution; **BA** (was
BB) operates on the grammar-derived struct tree; **BB** (was BC)
runs e-graph rule inference. BC letter retired. Shape A (tape
persists alongside struct) and Shape B (AY-II triples in scope)
were rejected. — `docs/tranches/AZ/AZ.md`.

**Parent-pointer on the struct tree is a sidecar.** A parallel
index alongside the struct, not embedded in every node — zero
bloat when unused, one extra indirection on ascent. Final
sidecar-versus-embedded-versus-hybrid decision is settled by
BA.W0 micro-bench on citm / tailwind / sheets fixtures. The
`AscentStrategy` trait is the reversal seam. — `docs/tranches/BA/BA.md`.

**StructRegistry closes hard or the tranche does not close.**
Per-grammar IR audit pass must return pass at AZ-I.W1; any fail
blocks AZ-I.W2 and every subsequent AZ-I / AZ-II wave. Partial registry is
not permitted to open BA. — `docs/tranches/AZ/AZ.md`.

**VM and e-graph cooperate; neither replaces the other.** E-graph
first as the fast-path equivalence check under the current
rewrite set; VM second as the non-circular ground-truth tie-break
on residue. Accepted VM-proved rules extend the e-graph so the
residue shrinks monotonically. VM workload is < 10 % of enumeration
— comfortable at the current ~1800-LOC surface. —
`docs/tranches/BB/BB.md`.

**Rules live outside core.** Grammar-specific rules colocate with
their grammar at `grammar/<name>/rewrites/*.ron`; fleet-wide rules
live in a `crates/ir/src/rewrites/` module within the existing
`bbnf-ir` crate — not a standalone crate, which was rejected
because rewrite rules are IR-specific and do not merit an
independent boundary. Automatic ranker scores candidates on match
frequency × cost delta × generality × similarity to ground truth
× novelty × tree size. Tiered review: Class-1 auto-accept with
audit log, Class-2 fast-track, Class-3 full human review. First-
run review is bounded by the Tranche H ground-truth set — a
one-time effort. — `docs/tranches/BB/BB.md`.

**Cross-worktree pin drift is CI-guarded.** A shared GitHub
Actions workflow reads `rust-toolchain.toml` from bbnf-lang,
parse-that, and pprint; any divergence fails every PR in all
three repos. Landing wave: B1.W2.c. — `docs/tranches/B1/B1.md`.

**Nextest parallelism respects rustc parallelism.**
`test-threads = 2` on the `ax-iter` nextest profile;
`test-threads = 8` on `close`. Wave-close sanity check:
`cargo iter-test` completes in < 30 s wall-clock on the 8-core
reference box. — `docs/tranches/B1/B1.md`.

**Derive-cache invalidation uses a composite key and a
chronic-pain test suite.** Key: `(grammar-sha256,
bbnf-derive-crate-version, rustc-version-sha)`. Test suite at
`crates/derive/tests/cache_invalidation/` exercises each factor
independently plus all combinations. Cache hit budget < 50 ms
with key derivation memoised per process. Size cap 2 GB with
atime LRU eviction. Every miss logs its reason — no silent
misses. One-time migration from `target/.bbnf-cache/` to
`$XDG_CACHE_HOME/bbnf-derive/`. Lives in AZ-I.W0. —
`docs/tranches/AZ/AZ.md`.

**The gorgeous sibling is retired.** Executed during the audit:
`/Users/mkbabb/Programming/gorgeous` moved to
`~/.Trash/gorgeous-retired-2026-04-23`. Its 2 benches migrate
inward to `crates/gorgeous/benches/` in a Phase-B follow-up.
Workspace `crates/gorgeous/` is the only canonical gorgeous;
every path patch in the fleet resolves to it.

**Classifier unification is front-loaded.** The classifier-
collision study runs in AZ-I.W0 before any payload activation;
the research artefact at `docs/tranches/AZ/CLASSIFIER-UNIFICATION.md`
either produces a unified classifier or declares unification
intractable, which is a re-plan trigger at AZ opening rather than
a mid-AZ sub-wave. The reactive-sub-wave shape was rejected as
recipe-for-disaster. — `docs/tranches/AZ/AZ.md`.

Every decision has an if-answered-X-then-plan-changes-to-Y
trigger expressed in the owning tranche doc. The pattern is
`feedback_execute-planned-architecture` applied to planning: the
plan carries its own unworkabilities, and every decision point
has a declared reversal path. Decisions that require measurement
to refine (parent-pointer form, tranche H rediscovery threshold,
VM oracle throughput) carry an explicit wave-level micro-bench
gate; decisions that are settled by direct rule (cache key,
pin-drift CI, test-thread count) are guarded by a standing test
or CI check.

The ten decisions span AZ's internal choices, BA's dependency on
AZ's choices, BB's oracle and storage architecture, and the
cross-tranche infrastructure guardrails. Each resolved decision
becomes a live invariant inside the owning tranche doc — the
tranche cannot close with its corresponding invariant unmet, and
a replacement gate may only decommission the invariant when the
new one is declared in the same commit. The decision record above
is a snapshot of the invariant surface at the time of writing;
every subsequent tranche FINAL.md records which invariants the
tranche satisfied and which it carried forward.

## 11. Appendix — branch index

Eleven worktree branches consumed in this gestalt, plus the prior
validation wave. Each row: worktree branch, commit SHA, deliverable
path, line count. Dive directly into any document from here.

| Wave | Branch | Commit | Deliverable | LOC |
|---|---|---|---|---:|
| Prior 1 | `worktree-agent-a1310f56` | `b5bba4d3` | `docs/instructions/*` streamlining | — |
| Prior 2 | `worktree-agent-a064cc28` | `26adf29f` | `docs/tranches/meta-audit/05-validation.md` | 410 |
| Prior 3 | `worktree-agent-a9c6ca4b` | `e0f556d6` | `docs/tranches/B1/TOOLCHAIN-SOTA.md` | 819 |
| Prior 4 | `worktree-agent-a4f869d1` | `f53d258e` | `docs/tranches/next-tranche-research/ARCHIVAL-SYNTHESIS.md` | 644 |
| W1-A | `worktree-agent-a2b99c29` | `2606b5ac` | `docs/tranches/meta-audit/06-commit-archaeology.md` + 5 era deep-dives | 1595 |
| W1-B | `worktree-agent-ac6a0c79` | `7b187b6d` | `docs/tranches/meta-audit/07-appurtenant-assay.md` | 1297 |
| W1-C | `worktree-agent-a3a8fdcf` | `a3c95801` | `docs/tranches/meta-audit/08-abrogation-catalog.md` | 811 |
| W1-D | `worktree-agent-a988e2ba` | `09330f92` | `docs/tranches/B1/TOOLCHAIN-MIGRATION.md` + 7 patches | 1623 |
| W2-D | `worktree-agent-a752c2f7` | `7f3394bc` | `docs/tranches/B1/{B1.md, waves/W0-W3.md, PROGRESS.md, AGENT_DISPATCH.md}` | 1549 |
| W2-F | `worktree-agent-aa76213d` | `2bfa6d6e` | `docs/tranches/next-tranche-research/repo-modernization/*` (19 files) | 2173 |
| W2-G (BA) | `worktree-agent-a8620933` | `e9dd4c3c` | `docs/tranches/BA/BA.md` (plan) | — |
| W2-G (BA-R) | `worktree-agent-a8620933` | `6746e749` | `docs/tranches/BA/RESEARCH.md` | — |
| W2-G (BB) | `worktree-agent-a8620933` | `990dbdbf` | `docs/tranches/BB/BB.md` | — |
| W2-G (BC) | `worktree-agent-a8620933` | `3cba2a43` | `docs/tranches/BC/BC.md` | — |
| W2-G total | — | — | BA/BB/BC refined plans | 1141 |

Key file pointers for readers diving deeper:

- Era narratives: `docs/tranches/meta-audit/archaeology/era-{II,III,IV,V,VI}-*.md`
- Invariant codification: `docs/instructions/tranche/SPEC.md` (especially §577–594 heavy-surface edicts, AX invariant 13, N-agent disjoint-row)
- AU-baseline: `docs/tranches/AU/FINAL.md` (`5281ec23`)
- Column revert: `docs/tranches/AY-I/FINAL.md` + `bd563c1d` for W0' consolidation
- Path forward: `docs/tranches/AY-II/PATH-FORWARD.md`
- VM oracle: `crates/ir/src/vm/` (source, compiles at HEAD)
- Parity harnesses: `tests/*_parity.rs` (sonic-rs, lightningcss, simdjson OnDemand, serde_json, cssparser)
- Feedback memory corpus: `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md`

### Cross-reference: six-era inflection commits

The three reversals the archaeology names as health signals, plus
the three forward inflection commits, all live on unpushed master.
Each is reproducible from the tree:

| Phase | Commit | Meaning |
|---|---|---|
| Era III arrival of CSP scheduling | `a5991bac` (Tranche K) | CSP-scheduled e-graph execution; cost-model spine for later decisions |
| Era IV tape-first substrate | `85478284` (AE.0/.1) | Tape-first shape-agnostic walking; every later tranche inherits |
| Era IV decision-surface collapse | `2f7c1bd4` (AQ.5) | Structural dispatch deleted; `no-orthogonal-codepaths` enforced |
| Era IV baseline | `5281ec23` (AU FINAL) | First `FINAL.md`; 17-entry baseline matrix; convention set |
| Era V peak-and-lose | `c1e86ab3` (AW-V.W3) | JSON shape-emitter thesis demonstrated once, lost by W6 |
| Era V reckoning | `4177a18c` (AX plan) | RD Reckoning; 21 invariants; interpreter deletion queued |
| Era V close (reversal 1) | `a206b962` (AX.W0b.A) | DTA walker deleted; ~78K LOC reclaimed |
| Era VI column revert (reversal 2) | AY-I.W1 | Seven structural Vecs → single `Vec<TapeRec>` + `sib_skip` |
| Era VI hand-coded revert (reversal 3) | `3429aaba` (W1r.0) | `bbnf::json::Value` / `css::StyleSheet` deleted; −6,128 LOC |
| Era VI infra pivot | B1 plan at `7f3394bc` | Dev-loop truth before runtime; AY-II paused |
| Era VI future (BA) | BA plan at `e9dd4c3c` | Twitter 1967 MB/s gate in W1; direct-to-struct activation |

The six arrows — CSP → tape-first → decision-collapse → baseline →
peak-and-lose → reckoning → reversal → column-revert → infra-pivot
→ activation — are the project's architectural spine. Every later
tranche cites at least one of them.

---

The thesis the reader walks away with: *bbnf-lang is grammar-
derived, one substrate, one measurement surface, and reversal is
the health signal.* The decision this document makes crisp: **B1
must close before AY-II.W0' closes; AY-II must close before BA
opens; BA must close with StructRegistry at 100% and twitter at
1967 MB/s before BB opens; BB must close before BC enumerates.**
The question a future maintainer reaches for this document to
answer: *why this sequence, and what would cause us to reverse it?*
Section 4 answers the first; Section 10 answers the second.

A closing note on what this document deliberately does not claim.
It does not claim the plan is correct; it claims the plan is the
one the fleet has converged on after eleven worktree branches of
independent audit. It does not claim BA's twitter gate will land;
it claims the gate is declared, the substrate path is measurement-
first, and the reversal criteria are codified. It does not claim
the VM oracle will scale; it flags the question under §10 and
specifies the BC.W0 micro-bench that will resolve it. The project
has twice in six months shipped substrate that failed to activate,
and twice responded with decisive reversal rather than patch.
Reversal is the mechanism of correctness; the plan budgets for it;
the reader should expect it. What the plan does not budget for is
continuing to execute on substrate that has not demonstrated its
runtime consumer — AX invariant 13 is the codification, and every
tranche in the runway cites it. The runway's end state is a
grammar that produces, for any language, a direct-to-struct tape-
first runtime parser that beats lightningcss, sonic-rs, and
simdjson OnDemand at their own games — parity first, exceedance
second, every `->` reaching the tape, one substrate, one
measurement surface, no orthogonal codepaths. That is the target;
the sequence above is the only one the fleet has found that
reaches it.
