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

The fleet state at `48e6eaa9`: 1,842 master commits, 945 unpushed,
24 feature branches, 18 tranches of substrate work landed between
2026-04-08 and 2026-04-15, followed by a five-day DTA/PSI rut
(Era V, 2026-04-15 → 2026-04-19, ~572 tranche-tagged commits) in
which zero of seventeen benchmark entries recovered the 2026-04-15
AU-baseline. Era V closed at AX.W0b (`a206b962`, 2026-04-20) with
the deletion of ~78,000 LOC of interpreter substrate. Era VI, the
current era, is the infra-truth restart: AY-I landed column revert
and honest relinquish; AY-II paused at W0' for the B1 prelude; B1
pins the toolchain, migrates the bench harness to divan, rewires
CI, and re-anchors measurement on a clean substrate. JSON twitter
sits at 688 MB/s — 35% of AU-baseline 1967 MB/s. The twelve-week
runway delivers: **B1** (dev-loop truth, 4 waves, one week),
**AY-II.W0'** close + W1–W5 resume (infra-blocked; two weeks after
B1), **BA** (grammar-derived tape activation; twitter 1967 MB/s
gate in W1; two to three weeks), **BB** (lazy typed pointer-path
queries over BA's tape; two weeks), **BC** (e-graph rule inference
with the VM interpreter surviving at HEAD as the equivalence
oracle; two to three weeks). The sequence is strict: B1 precedes
AY-II.W0'; AY-II precedes BA; BA precedes BB; BB precedes BC.

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

## 3. The six eras — one diagram, one paragraph

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
 Era VI ── Infra-truth restart ─────────────────────── 2026-04-20 →        (~130c so far; AY-I, AY-II, AZ, B0, B1, BA–BC)
           Column revert / AY-II path-forward / B1 dev-loop truth
           (BA / BB / BC scaffold — not started)
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

**Era III** (2026-03-16 → 2026-04-09, 18 tranche letters F through W)
builds the optimiser substrate. CSP solver, e-graph with scheduled
rewrites (Tranches H / J / K), NodeId-everywhere (`f6119e0b`,
Tranche L), IndexMap-deterministic codegen (Tranche N), bbnf-ir god-
module splits (Tranche Q), recognizer mining (Tranche V), kernel-
family modules (Tranche W). Every Era III tranche is "Worked" in the
archaeology ledger; none are reversed. The substrate Era III
produced carries every subsequent era.

**Era IV** (2026-04-10 → 2026-04-15, tranches Y through AU) is the
tape-first runtime. Tape column splits (Y, reverted at AY-I.W1).
Cursor + reader surface (Z). TypeDescInterner hash-cons (AA,
`c209c380`). Tape-first shape-agnostic walking (AE, `85478284`).
Three-tier emission design (AF) — also reverted, at AQ.5. Structural
dispatch v1/v2 (AO, AP) — also reverted, at AQ.5. The AQ.5 commit
(`2f7c1bd4`) is the architectural inflection: `no-orthogonal-codepaths`
is enforced in code. Era IV closes at AU (`5281ec23`, the first
`FINAL.md`); the 17-entry AU-baseline becomes the measurement that
every later tranche cites.

**Era V** is the DTA/PSI rut, treated in §4.

**Era VI** is where the project is now: AY-I landed the column
revert (Tranche Y's seven structural Vecs collapsed back to one
`Vec<TapeRec>` plus a parallel `sib_skip`), ending the column
experiment; AY-II wrote the rest-of-AY path forward and paused at
W0'; AZ opened as a planning-only tranche; B0 landed the bounded
prelude (profile tiers + `ay-*` Makefile); B1 is mid-flight. The
present is Era VI's infra-truth restart.

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
demonstration and lost it. AX's "RD Reckoning" (`4177a18c`,
2026-04-16) was the honest recognition: the interpreter was to be
deleted. At `bc550d2c` and `a206b962` (2026-04-20, within AX.W0b.A)
the DTA walker, `dta_walker/`, `emitter/dta.rs`, and eight DTA-
coupled test suites were removed in one sitting. `0adabb23` closed
the cleanup. Roughly 78,000 LOC reclaimed. Era V's plan had also
assumed that a close-of-tranche ledger (tables written, imports
green, tests compile) was equivalent to runtime evidence that the
substrate worked; Era V's five closed FINALs each state the ledger
while recording 0/17 strict-better bench entries. Ledger-verification
substituted for runtime evidence. AX invariant 13 — *a ledger-only
wave is a re-plan trigger* — is the codification.

**What survives.** The VM interpreter at `crates/ir/src/vm/`
compiles at HEAD. The token-dispatch opcode machinery that the VM
needed for runtime interpretation is now BC's equivalence oracle
(see §5). The view layer and parity harnesses (`tests/*_parity.rs`
against sonic-rs, lightningcss, simdjson OnDemand, serde_json,
cssparser) are what AY-II cites as the parity reference. The
`bbnf-ir` crate itself — *one* analysis substrate, no parallel
`GrammarAnalysis` lattice (Tranche M deleted the lattice,
`359eb068`) — makes `project_analysis-consolidation` real: all
analysis lives in IR passes. The CSP + e-graph cost-model unification
(Tranches H / J / K / W) was unchanged by Era V and is what BC needs.
Debug infrastructure (`@debug`, source maps, `DebugBreak`, interpreter
hooks, compiled trace) landed across Era IV/V Phases 1-4 and is
durable. The `LargeAggregate` payload path (AV V2) and the
`PaddedView` SIMD cascade (AV V0) are consumer-facing improvements
kept at HEAD.

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

## 5. The current plan — B1 → AY-II.W0' → AY-II W1–W5 → BA → BB → BC

### B1 — dev-loop truth + proof-surface hardening

Bounded prelude annex. Four waves. W0 pins `rust-toolchain.toml`
to `nightly-2026-04-11` across bbnf-lang, `../parse-that`,
`../pprint` — eliminating the 93-ICE cluster at
`compiler/rustc_middle/src/query/on_disk_cache.rs:663:9` that the
meta-audit traced to an ambient `1.96.0-nightly (9602bda1d 2026-04-05)`
cache-staleness bug (not a bbnf bug). W0 also rewrites `.cargo/config.toml`,
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
`FusedBuilder` (`bd563c1d`, 2026-04-21) before the infra audit
revealed that the public command surface was diverged from what the
repo actually executed. `PATH-FORWARD.md` reordered the remainder.
B1 is the bounded prelude that must close before W0' closes its
ceremony and W1 dispatches. The paused runtime work is typed-
materialisation closure on the settled parse-that substrate — no
architectural surprises, but every gate re-anchors against the
post-B1 bench matrix. Agent-slot count: resumed on the refreshed
proof surface; specifically sized at W1 open.

### BA — grammar-derived tape activation

Opens on AY-II close. Four waves. W0 lifts the derive cache from
the source tree into `$XDG_CACHE_HOME/bbnf-derive/` (the deferred
build-infra piece B1 chose not to absorb). W1 is the measurement
gate: JSON twitter at 1967 MB/s — recovery of the AU-baseline,
not exceedance — under a runtime consumer of the backward
container pointer that W1 adds to the tape record. The one-way
`child_off` asymmetry that has kept pointer-path work out of BB
closes in BA.W1. W2 activates `StructRegistry` population across
JSON, CSS L4, Sheets, BBNF grammars; the emitter emits the
registry at codegen, the runtime reads it, the IR audit pass
enforces 100% `->` coverage. W3 hardens CSS L4 to lightningcss
typed-value parity node-for-node — `<length>` rules return a typed
`Length` equivalent to lightningcss's, no approximation. W3 also
settles the backward-pointer form (in-record versus sidecar column)
for BB to consume at W0.

BA's reversal criteria: twitter does not recover 1967 MB/s after
W1 substrate lands; StructRegistry is partial at W2 close
(permitting a BA-carry wave but not BB open); a 17-entry matrix
regression against the post-B1 floor on any entry blocks wave close.
`feedback_execute-planned-architecture`: do not retreat from planned
substrate, but ship no substrate without a same-commit consumer
and same-commit bench delta. Agent-slot count: four waves, one to
three agents per wave, plus a dedicated profile-delta agent.
`feedback_no-orthogonal-codepaths` is enforced: one
`compute_payload_layouts` pass, one `StructRegistry`, one emitter
consumer per layout kind, one regex system.

### BB — lazy typed pointer-path queries

Opens on BA's settled substrate. The thesis is sonic-rs
`pointer!` ergonomics with compile-time type validation plus
simdjson OnDemand's forward-skip laziness, but grammar-typed:
`path!("store", "books", 0, "title")` against a JSON grammar
resolves through IR type inference to a `NodeView<'p, TitleRule>`
accessor; invalid paths fail to compile. Strictly stronger than
sonic-rs's runtime path error. Lazy skip uses BA's backward
(`parent_off`) and forward (`child_off`) container pointers.
Zero allocation on traversal — intermediate state borrows from
the tape. Path values compose with e-graph normalisation (duplicate
prefix elimination, redundant downcast elimination, path-fusion
with adjacent accessors); BC later extends this surface with
inferred rules. Dependencies on BA are strict: no StructRegistry
at BA close → no BB open. Agent-slot count: three waves, two to
three agents each.

### BC — e-graph rewrite rule inference

Opens on BB's settled path substrate. BC closes the loop on
`feedback_pluggable-components` and `feedback_csp-always-optimize`
by letting the e-graph *discover* grammar-level rewrite rules
rather than only apply a fixed set. Ruler-style (Nandi et al. 2021)
CVC enumeration over `IrNode` produces candidate terms up to a
bounded size. The bbnf IR interpreter at `crates/ir/src/vm/` — the
core of Era V's VM, salvaged at HEAD — is the equivalence oracle:
two `IrNode` candidates are equivalent if their tape output matches
across a corpus. CSP schedules candidate rule application against
the existing cost model (no new decision surface). Accepted rules
persist into `cost_config` for subsequent codegen consumption.
Every persisted rule round-trips to bbnf surface syntax with a
`Debug` form — a reviewer reads it, accepts or rejects, enumeration
is automated but curation is not. BC is the tranche where Era V's
long arc finally earns its keep: ~572 tranche-tagged commits do not
come back, but the VM's narrow surviving surface is what makes
rule-inference viable at all. `feedback_abrogate-before-patch`
applies: BC does not re-open the DTA walker; BC uses what remains.
Agent-slot count: four waves.

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

The synthesis thesis: bbnf is a *compositional* SOTA of these
pieces, where composition is mediated by grammar-derived semantics.
simdjson's tape shape, sonic-rs's type registry, lightningcss's
typed values, Ruler's rule enumeration, egg's e-graph, parse-that's
combinators — each contributes a specific capability; each is
wired into bbnf through the grammar's `->` annotations, not through
a per-feature side channel. The IR is what makes the composition
coherent.

## 7. The fleet — cross-repo shape

Sixteen rust artefacts across four sibling repos and twelve
workspace members. The repo matrix from the W2-F repo-modernization
index reduces to three phase groups.

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
JSON canada 1231 MB/s, JSON citm 2438 MB/s, JSON twitter 1967 MB/s
(currently 688 MB/s, 35% of AU), CSS normalize 735 MB/s, CSS
bootstrap 454 MB/s, CSS tailwind 496 MB/s, Sheets parse_simple
95 MB/s. BA's floor is parity recovery on all seven; BA's target
exceeds parity by 10–25% per entry. Workspace gates: pass count
≥ 967, fail count ≤ 33, ignored count ≤ 30. Coverage gates:
`grep -c 'push_leaf_with_' crates/core/**/generated.rs ≥
count of scalar-payload ->` across all grammars; StructRegistry
non-empty for JSON pair/value, CSS L4 declaration/dimension/colour,
Sheets cell/formula, BBNF rule/alt_branch.

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
future work enforces it.

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

## 10. Open questions that gate the plan

Four BA/BB/BC questions remain open at gestalt time, plus three
cross-tranche questions surfaced during Wave 2.

**Backward-pointer form** (BA.W3). In-record versus sidecar column.
In-record changes the 16-byte `TapeRec` layout; sidecar adds a
parallel `Vec<u32>` alongside `sib_skip`. The decision affects BB's
path resolver: in-record gives single-cache-line ascent; sidecar
gives better cache behaviour on parent-blind traversals but two
memory reads on ascent. If BA.W3 chooses sidecar, BB.W0 absorbs
the change; if BA.W3 chooses in-record, BB.W0 proceeds with one-
read ascent.

**StructRegistry partial-close** (BA.W2 → BB.W0 gate). BB does not
open on a partial `StructRegistry`. If BA.W2 closes with coverage
< 100% on any production grammar, a BA-carry wave lands the
remainder; BB.W0 waits. The gate is the IR audit pass's pass /
fail result.

**VM oracle performance at scale** (BC). Ruler-style enumeration
generates terms at polynomial growth in depth; the VM runs each
candidate against the corpus. If VM throughput against, say, the
twitter fixture is below 10 MB/s per candidate, BC's enumeration
bound has to shrink or the VM's inner loop has to be re-optimised.
A BC.W0 micro-bench establishes the floor.

**Rule curation cost** (BC). Every persisted rule is reviewed by a
human before it lands in `cost_config`. If the enumeration produces
> 50 rules per hour of VM time, review becomes the bottleneck, not
enumeration. BC's dispatch brief sizes the review surface.

**Cross-worktree pin drift** (B1.W2.c). Three siblings pin
`nightly-2026-04-11`; if ambient pins drift post-B1 (a sibling
bumps to a later date for an unrelated reason), the 93-ICE cluster
could return. B1.W2.c adds a CI job that diffs the pin across the
three siblings; any drift is a build failure.

**`--test-threads` × `-Zthreads` collision**. nextest's default
parallelism and rustc's `-Zthreads` rayon parallelism can
interact; a misconfiguration yields thread-starvation. B1.W0.c's
nextest profile set is chosen to avoid this; if a post-B1 profile
edit reintroduces the collision, a wave-close regression test
catches it.

**Derive-cache invalidation key**. `$XDG_CACHE_HOME/bbnf-derive/`
(BA.W0) needs an invalidation key that captures grammar changes,
IR pass versions, and rust toolchain version. If the key is too
narrow, stale cache entries; if too wide, no cache hit. BA.W0's
key derivation is gated by an explicit test suite that exercises
both extremes.

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
