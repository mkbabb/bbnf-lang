# Era IV — Tape-First + EmissionTier Arc (2026-04-10 → 2026-04-15)

Era IV is the *peak tranche* era. In six calendar days, twenty tranche
letters (Y, Z, AA, AB, AC, AE, AF, AG, AH, AI, AJ, AK, AL, AM, AN, AO,
AP, AQ, AR, AS, AT, AU) execute against a tape-first codegen substrate
that becomes the runtime invariant for every subsequent era. Era IV
closes at AU with the sharpest baseline the project has ever shipped —
post-AU.json: JSON canada 1231 MB/s, citm 2438 MB/s, twitter 1967 MB/s,
CSS normalize 735 MB/s, tailwind 496 MB/s, Sheets parse_simple 95 MB/s.

Commit density across Era IV tranches: AA 15, AB 4, AC 10, AE 17, AF
26, AG 6, AI 9, AJ 4, AK 3, AL 1, AM 7, AN 5, AO 2, AP 9, AQ 13, AR 16,
AS 4, AT 14, AU 22. Roughly 185 tranche-tagged commits plus infill.

## Architectural thesis

The parse output is a columnar tape (`TapeRec` + payload arenas). Every
`->` annotation in every grammar reaches the tape as a typed payload.
The emitter is *grammar-authoritative*: `TypeDesc` inference decides
what the runtime stores; the tape stores it; consumers project typed
accessors out of it. A single decision surface, one codegen path.

Four commits carry Era IV's thesis:

- `13411847` (2026-04-10) `docs(tranche): add Tranche AA plan + baseline
  cleanup` — first tranche *plan document*.
- `6ba80158` (2026-04-10) `docs(tranche): Tranche AB — Tape as the Only
  Runtime Substrate`.
- `85478284` (2026-04-11) `refactor(lower,host): tape-first shape-
  agnostic walking substrate (Tranche AE.0/AE.1 wip)`.
- `ff757215` (2026-04-13) `docs(AU): plan tranche — projection
  activation, scanner truth, debt elimination` — AU plan document.

## Tranche ledger

| Letter | Commits | Headline | Verdict |
|---|---:|---|---|
| Y | 13 | Tape column splits (first columnar substrate) | Worked — columns survive into AU then revert in AY-I.W1. |
| Z | 6 | Cursor + reader surface | Worked. |
| AA | 15 | `TypeDescInterner` hash-cons substrate (`c209c380`) | Worked. |
| AB | 4 | Tape as the only runtime substrate | Partial — thesis declared, execution spans AC–AE. |
| AC | 10 | Full tape transposition | Partial — some paths still carry `Value` wrappers. |
| AE | 17 | Tape-first shape-agnostic walking substrate (`85478284`) + tranche close | Worked — lowering becomes tape-first. |
| AF | 26 | Three-tier emission design (AF-prototype) — `MustTape`, `MustFn`, `MayInline` | Partially worked; tier surface grew and was pruned later. |
| AG | 6 | Cross-rule CSP tier variables + docs (`1c66b932`) | Worked. |
| AI | 9 | Scanner integration + payload consolidation | Worked. |
| AJ | 4 | Post-AJ bench baseline (`65e04690`) | Worked. |
| AK | 3 | Post-AK bench baseline (`c62ad389`) | Worked. |
| AL | 1 | Minor cleanup | Worked. |
| AM | 7 | 4 regressions blocking workspace resolved (`4d1afeb0`) — the inflection that recovered from an AL–AM regression window | Worked — inflection commit. |
| AN | 5 | Correctness, generalization, hyper-optimization plan (`acaa1898`) | Partially executed. |
| AO | 2 | Structural dispatch + scanner generalization + global CSP plan (`e64164e4`) | Scope migrated to AP/AQ; AO itself landed only 2 commits. |
| AP | 9 | Correctness-first, structural dispatch v2, Tier B emission plan (`480a4cb4`) | Partially worked. |
| AQ | 13 | `TypeDesc`-driven projection, self-hosting closure, **structural dispatch deletion** (`2f7c1bd4`) | Worked — pivotal. |
| AR | 16 | Discriminator split → payload activation → clone reduction → CSS hardening → tape capacity (`8204fa15`) | Worked. |
| AS | 4 | CSS L4 parse activation, `TypeDesc::Span` admission, tranche directory structure introduced (`536ac07c`) | Worked — `PROGRESS.md` convention lands here. |
| AT | 14 | Multi-type payload projection, SIMD guard, CSS spec gaps, structural tests (`05c89293` → `74ade4c6`) | Partially worked — AT flagged two systemic payload-discard bugs routed to AU. |
| AU | 22 | Projection activation, regression redress, scanner truth; first tranche to publish `FINAL.md` (`5281ec23`) | **Partially worked — the Era IV close baseline.** 10/24 hard gates met, 2 met-qualified, 5 partial, 5 missed. |

## The AQ.5 inflection — EmissionTier deletion

Commit `2f7c1bd4` (2026-04-13) `refactor(ir,codegen): delete structural
dispatch` is the single most important code-removal of Era IV. It
collapses an independent `EmissionTier` axis (`MustTape` / `MustFn` /
`MayInline`) and its attendant structural-dispatch pre-scan into the
pre-existing `PayloadKind → TypeDesc` path. The CSP-optimizer no longer
selects a tier separately from projecting a type; one decision surface
for one semantic. This is the "never two decision surfaces for one
semantic" lesson that feedback memory `no-orthogonal-codepaths`
enshrines.

The AQ.5 deletion directly refutes the AF–AG architectural thesis.
Roughly two weeks of AF/AG/AH/AI/AJ/AK work on tier substrate is
subsumed into the TypeDesc projection; nothing is "wasted" because the
cost-model hooks the tier work established become part of the unified
path.

## The AM.0 inflection — regression triage

Commit `4d1afeb0` (2026-04-12) `fix(core): resolve 4 regressions
blocking workspace (AM.0)` rescues the tree from an AL-era break in
which AF's tier substrate + AK's scanner work left four workspace
tests in a non-green state simultaneously. AM.0 is the paradigm for
every later *triage-before-forward* tranche opening.

## What landed durably

- **Columnar tape** (`crates/tape/`) — the `TapeRec` + payload arenas
  + `FusedBuilder` API survive through every later era.
- **`TypeDescInterner`** hash-cons — stable identity for types across
  the compiler.
- **`has_payload`, `has_scalar_payload_type`** gate predicates that
  become the vocabulary every later gate (AX.W0a's 21-assertion
  wire-contract matrix included) speaks.
- **Plan / PROGRESS / FINAL doc convention** — AS introduces the
  directory layout (`536ac07c`); AU publishes the first `FINAL.md`
  (`5281ec23`); every subsequent tranche inherits the convention.
- **Fingerprint-driven codegen capacity** — per-grammar `Vec::
  with_capacity` tuning via `GrammarProfile` substrate.
- **Post-AU baseline JSON** (`3b8b757`) — single canonical numeric
  anchor referenced by every Era V and Era VI plan doc. Era V's
  plans quote `docs/benchmarks/post-AU.json` as the floor to beat.

## What was reverted or superseded

- **`EmissionTier` enum** — deleted at AQ.5 (`2f7c1bd4`).
- **Full structural pre-scan** — deleted with AQ.5.
- **AO structural substrate without activation** — AO's plan
  (`e64164e4`) shipped substrate that AQ.5 later unified; AO's own
  2 commits are the high-water mark of the orthogonal approach.
- **`payload_idx: u16`** overflowed at canada.json's 111K f64 payloads;
  AU.1.1 (`83357e4`) folded the tag into `child_off` (u32 range) with
  sentinel `payload_idx: 1`.
- **`StructRegistry` scaffold** — AS.2.3 shipped it; AU.4.2
  (`ab8588a`) deleted it per the `no-backward-compat` invariant;
  AV's `LargeAggregate` path supplants it.
- **Scalar-bypass Bug-2 pinned assertions** — 5 Sheets parity tests
  flipped FAIL after W6.D, routed to AV for fix.

## Salvageable artefacts (all present at 2026-04-22)

- `crates/tape/src/` (tape crate; renamed from `bbnf-tape` at AX.W0b).
- `crates/core/src/backend/rust/emitter/` (the emitter surface that
  AW-V exhibited but AY is still trying to close).
- `crates/bbnf-tape-codegen/` (generator; marked for carve at AX.W0b).
- `docs/benchmarks/post-AU.json` — the measurement floor.
- `docs/tranches/AU/FINAL.md` — the narrative floor.

## The AU "finished state" — what was true on 2026-04-15

Per `docs/tranches/AU/FINAL.md`:

- Seven waves landed across 2026-04-14 / 2026-04-15.
- `branch_pushes_children` returned true for `Ref` nodes pointing to
  `CallStrategy::InlineBody` rules (AU.1.1, `83357e4`).
- All JSON `value` typed payload captures became *live writes* (prior
  to AU, they were dead stores).
- Payload correctness harness: `grammar_roundtrip` 6/6,
  `payload_layouts` 13/13.
- CSS L4 parsed normalize + bootstrap + tailwind end-to-end.
- Workspace: 967 pass / 33 fail / 30 ignored under `--no-fail-fast`.
- Of the 33 fails: 4 CSS `tape_parity` goldens W6.D's regen missed, 5
  Sheets parity assertions flipped by scalar-bypass landing, 23
  pre-existing Session-1 failures (closures, debug, analysis, graph,
  gorgeous, lsp, lower, recover).
- Systemic bugs documented in `typed-parity-audit.md`:
  - Bug 1 — alt-lit per-branch payload-write loss.
  - Bug 2 — `-> Span` shorthand lowering to `push_compound`.
  - Bug 2b — `-> i64` / `-> f64` scanner-to-payload threading missing.
- Missed perf gates: canada 1231 (target 1800), citm 2438 (target
  3000), CSS bootstrap 454 (target 600), tailwind 496 (target 1200),
  Sheets parse_simple 95 (target 250).

AU is "substrate complete, performance missed, two systemic bugs
documented, forwarded to AV." Every subsequent era references this
close state as the *comparison anchor*.

## Transition into Era V

AV's plan (`ca0875eb`, 2026-04-15) opens *The Flattening*. AV's thesis
is to ship DTA (dispatch table automaton) + PSI (parallel structural
index) + columnar substrate as one coherent architecture, and drive
sonic-rs + lightningcss parity. The substrate is designed to channel
AU's bug-close work into the tape writes that typed materialisation
requires. Era V runs AV through AX and ships ~400 commits, all of
which walk back to AU-baseline by Era VI's opening.

The Era IV → Era V boundary is the place where tranche discipline's
success (plan doc, waves, PROGRESS, FINAL) locks in, and simultaneously
where the substrate-first / consumer-later anti-pattern that defines
Era V's pain first takes root.
