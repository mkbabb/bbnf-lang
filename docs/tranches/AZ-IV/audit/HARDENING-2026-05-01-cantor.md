# AZ-IV Hardening - Cantor Lane (2026-05-01)

Plan-coherence and spec-adherence audit of `AZ-IV.md`, `GESTALT.md`,
`PROGRESS.md`, and `waves/W0..W3.md` against
`docs/precepts/instructions/{README,ORCHESTRATION,LESSONS-LEARNED}.md` and
`tranche/{SPEC,WAVE_SPEC,AGENT_DISPATCH_TEMPLATE}.md`. Read-only audit; no
source touched.

## Disposition Summary

The plan satisfies all thirteen WAVE_SPEC ordered sections in every wave,
keeps the six-agent ceiling, names the six-agent count per wave, uses the
canonical `{LETTER}.W<N> - <Title>` form for headers and the
`{LETTER}.W<N>.<x> <Title>` form for sub-units, invokes `commit-discipline`
in every Commit Plan, and references `close-honesty` in W3 and `PROGRESS`.
Carry ledger and BA/BB coverage are dense and lossless.

The plan has six narrow defects worth fixing before W0 dispatch, all in the
plan-text layer (no source change required):

1. **Within-wave file-bound overlap** in W2: `AZ-IV.W2.2` and `AZ-IV.W2.3`
   both list `crates/ir/src/passes/csp_strategy/**` as Files. WAVE_SPEC
   §4a forbids two units sharing a `modify` path. The Pratt emitter cited
   in `AZ-IV.W2.5` lives at `crates/core/src/backend/rust/emitter/shapes/pratt/`,
   so it falls inside the `shapes/**` glob owned by `AZ-IV.W2.3` — second
   within-wave overlap.
2. **Cross-wave shared `modify` paths**: `xtask/src/regen.rs`
   (W0 modify, W2 modify-carve), `crates/ir/src/egraph/**`
   (W0 modify-carve, W2 modify), `crates/core/src/backend/rust/emitter/shapes/**`
   (W1 modify-carve, W2 modify), `docs/GESTALT.md`,
   `docs/codegen-paths.md`, `docs/tranches/REMAINING-TRAJECTORY.md`
   (W0 modify, W3 modify-carve). Waves are sequential here, but
   WAVE_SPEC §4a still requires explicit acknowledgement; right now the
   Disjointness sections are silent on cross-wave repeats.
3. **Empty-return rule** is stated only in `W0.md` (line 30) and the
   parent `AZ-IV.md` §Orchestration Rules item 4. `W1`, `W2`, `W3`
   Triumvirate Dispatch sections do not restate it. Per
   `ORCHESTRATION.md` §Returns and `LESSONS-LEARNED.md` 2026-04-30 the
   rule is uniformly required.
4. **Three-diagnostic-loop trigger** is stated in `W0` and `W1` but
   missing from `W2.md` and `W3.md` (W3 mentions "profiling does not
   isolate a root cause", which is adjacent but not the canonical
   third-iteration halt). Auto-triggers (JSONL quiet >15min, first-pass
   no-commit) are nowhere — these are orchestrator-monitor concerns, but
   ORCHESTRATION.md §Triumvirate Auto-Triggers tells wave specs to
   reference them.
5. **HARD CAP language** is absent from every wave's Commit Plan and
   from `AZ-IV.md` despite `LESSONS-LEARNED.md` 2026-04-30 ("HARD CAPs
   On Every Dispatch") and `ORCHESTRATION.md` §Triumvirate. AZ-IV §6
   says "Read-only agents do not commit at hard cap" without ever
   defining the cap minutes; default `audit=25, research=20, plan=15,
   redress=30` are not cited.
6. **Generated-size budget** is missing. `W0` modifies
   `crates/core/src/grammar/generated/**` (regen output). Per
   `LESSONS-LEARNED.md` 2026-04-30 ("Generated Code Has A Size Budget")
   wave docs that change generators must include a
   `generated-size-budget` table in §Verification Artefacts. None of
   `W0..W3` does.

A handful of W-level hard gates are evidence-light but do not rise to
REJECT; see Findings table.

## Files Inspected

| File | Lines | Read |
|---|---:|:---:|
| `docs/precepts/instructions/README.md` | 115 | yes |
| `docs/precepts/instructions/ORCHESTRATION.md` | 207 | yes |
| `docs/precepts/instructions/tranche/SPEC.md` | 151 | yes |
| `docs/precepts/instructions/tranche/WAVE_SPEC.md` | 156 | yes |
| `docs/precepts/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` | 84 | yes |
| `docs/precepts/instructions/LESSONS-LEARNED.md` | 292 | yes |
| `docs/tranches/AZ-IV/AZ-IV.md` | 93 | yes |
| `docs/tranches/AZ-IV/GESTALT.md` | 83 | yes |
| `docs/tranches/AZ-IV/PROGRESS.md` | 49 | yes |
| `docs/tranches/AZ-IV/waves/W0.md` | 129 | yes |
| `docs/tranches/AZ-IV/waves/W1.md` | 136 | yes |
| `docs/tranches/AZ-IV/waves/W2.md` | 136 | yes |
| `docs/tranches/AZ-IV/waves/W3.md` | 122 | yes |
| `docs/tranches/AZ-IV/audit/HARDENING-SYNTHESIS-2026-05-01.md` | 80 | yes |
| `docs/tranches/AZ-IV/audit/LOSS-PREVENTION-SYNTHESIS-2026-05-01.md` | 120 | yes |

The precepts submodule was uninitialised in this worktree
(`docs/precepts/` empty). Read precepts from
`/Users/mkbabb/Programming/bbnf-lang/docs/precepts/`, which holds the
same SHA `fd9fab945c6ac7e0603b7521a8553b00109d3262` referenced by
`.gitmodules`.

## Findings

| # | File:line | Class | Finding |
|---:|---|---|---|
| 1 | `waves/W2.md:83,89` | REJECT | `AZ-IV.W2.2` and `AZ-IV.W2.3` Files both list `crates/ir/src/passes/csp_strategy/**`. Within-wave overlap violates WAVE_SPEC §4a. |
| 2 | `waves/W2.md:89,101` | REJECT | `AZ-IV.W2.5` Files name "Pratt emitter"; Pratt emitter lives under `crates/core/src/backend/rust/emitter/shapes/pratt/`, inside `AZ-IV.W2.3`'s `shapes/**` glob. Second within-wave overlap. |
| 3 | `waves/W0.md:44` vs `waves/W2.md:41` | NARROW | `xtask/src/regen.rs` modify (W0) + modify-carve (W2). Waves are sequential but Disjointness section is silent on the cross-wave touch. WAVE_SPEC §4a wants explicit handling. |
| 4 | `waves/W0.md:46` vs `waves/W2.md:38` | NARROW | `crates/ir/src/egraph/**` modify-carve (W0) + modify (W2). Same as above. |
| 5 | `waves/W1.md:49` vs `waves/W2.md:51` | NARROW | `crates/core/src/backend/rust/emitter/shapes/**` modify-carve (W1) + modify (W2). |
| 6 | `waves/W0.md:36-38` vs `waves/W3.md:46-48` | NARROW | `docs/GESTALT.md`, `docs/codegen-paths.md`, `docs/tranches/REMAINING-TRAJECTORY.md` modify (W0) + modify-carve (W3). Need explicit ownership note. |
| 7 | `waves/W1.md:21-29`, `waves/W2.md:23-30`, `waves/W3.md:22-30` | MISSING | Empty-return rule absent from Triumvirate Dispatch sections; only W0 carries it. |
| 8 | `waves/W2.md:23-30`, `waves/W3.md:22-30` | MISSING | Three-diagnostic-loop trigger absent. |
| 9 | All waves + `AZ-IV.md` | MISSING | HARD CAP minutes (`audit=25 research=20 plan=15 redress=30`) not stated; "0.9N commit, N halt" not stated. |
| 10 | `waves/W0.md:115-120` | MISSING | `generated-size-budget` table not declared though `crates/core/src/grammar/generated/**` is in scope. |
| 11 | `waves/W2.md:115` | NARROW | Hard gate item 7 ("DTA/dfa stale runtime claims are deleted; dead inline code is gone.") needs explicit deletion-proof grep path; sub-gate `AZ-IV.W2.4` provides it but gate item should mirror. |
| 12 | `waves/W1.md:107` | NARROW | Hard gate item 1 ("Regenerated tempdir outputs are used for parity.") names no test or evidence file; `bbnf_value_*` row already cited in item 3 — generalise. |
| 13 | All waves | ACCEPT | Header, State, Scope, Triumvirate Dispatch, File Bounds, Disjointness, Worktree Plan, Agent Units, Hard Gate, Format And Lint Cadence, Verification Artefacts, Commit Plan, Dependencies all present. |
| 14 | All waves | ACCEPT | Six-agent ceiling honoured (W0/W1/W2 = 5 parallel; W3 = 3 parallel). |
| 15 | All waves | ACCEPT | Sub-unit naming uses `AZ-IV.W<N>.<x> <Noun phrase>` form. |
| 16 | All waves | ACCEPT | `commit-discipline` skill named in every Commit Plan. |
| 17 | `waves/W3.md:6` + `PROGRESS.md:15` | ACCEPT | Close-honesty checklist referenced. |
| 18 | All waves | ACCEPT | Worktree Plan tables list per-unit absolute paths and unique `CARGO_TARGET_DIR`. |
| 19 | `waves/W0.md:30` + `AZ-IV.md:65` | ACCEPT | Empty-return rule stated at parent and W0; just needs to be repeated in W1/W2/W3. |

## Known Misses And Risks

- I did not enumerate every glob expansion across all four waves; the
  cross-wave overlaps cited above are the obvious ones. A full glob
  intersection between W2 (broadest) and W1 may surface more.
- The precepts submodule pointer in this worktree resolves to the same
  hash as the main checkout; if the main checkout has a stale precept
  draft, the audit would inherit that staleness.
- Hard-cap minutes are also not declared in the parent dispatch
  template (`AGENT_DISPATCH_TEMPLATE.md` line 9 names defaults), so
  AZ-IV may have considered them implicit. Treating that as MISSING is
  conservative; if orchestrator argues defaults are inherited, downgrade
  to ACCEPT.
- I did not verify that BA/BB carry rows in `AZ-IV.md` Carry Ledger
  resolve to a wave hard-gate item; that overlaps Babbage's lane.
- I did not run `git diff --check` because the worktree has no
  modifications until I write this audit doc. Run before commit.

## Exact Wave-Amendment Text

The orchestrator may copy these blocks verbatim into the named files.

### Amendment 1 - W2.md within-wave overlap split

In `docs/tranches/AZ-IV/waves/W2.md` §Agent Units, replace the Files
line of `AZ-IV.W2.2` (currently line 83) with:

```markdown
- Files: `crates/ir/src/passes/csp_strategy/strategy.rs`,
  `crates/ir/src/passes/csp_strategy/regex.rs`,
  `crates/core/src/backend/strategy/**`,
  `crates/core/src/backend/driver/**`,
  `crates/core/src/generate/regex/**`.
```

In the same file replace the Files line of `AZ-IV.W2.3`
(currently line 89) with:

```markdown
- Files: `crates/ir/src/passes/csp_strategy/shape.rs`,
  `crates/ir/src/passes/csp_strategy/structural_scan.rs`,
  `crates/core/src/backend/rust/emitter/shapes/dispatcher/**`,
  `crates/core/src/backend/rust/emitter/shapes/inline/**`,
  `crates/core/src/backend/rust/emitter/shapes/array/**`,
  `crates/simd-scan/**`.
```

In the same file replace the Files line of `AZ-IV.W2.5`
(currently line 101) with:

```markdown
- Files: `crates/core/src/backend/rust/emitter/shapes/pratt/**`,
  `crates/core/src/view/**`, `crates/core/src/runtime/css_l4/view.rs`,
  `crates/core/src/runtime/google_sheets/view.rs`.
```

(Names assume the actual concern split inside `csp_strategy/`. If the
real owners differ, the amending unit should rename to match HEAD;
the principle is one writer per leaf path.)

### Amendment 2 - W2.md File Bounds disjointness note

In `docs/tranches/AZ-IV/waves/W2.md` §Disjointness (currently line 60),
append:

```markdown

W2.2 owns CSP regex/strategy/dispatch leaves; W2.3 owns CSP shape/scan
leaves and `shapes/{dispatcher,inline,array}/**`; W2.5 owns
`shapes/pratt/**` and view rendering. The `csp_strategy/**` and
`shapes/**` globs are split per-leaf, never co-modified.

W2 also touches several files reserved by W0 or W1 in earlier waves
(`xtask/src/regen.rs` after W0, `crates/ir/src/egraph/**` after W0,
`crates/core/src/backend/rust/emitter/shapes/**` after W1). Sequencing
guarantees disjointness across the wave boundary; W2 may not open
until W1 closes and HEAD is clean for those paths.
```

### Amendment 3 - Triumvirate Dispatch uniformity

In `docs/tranches/AZ-IV/waves/W1.md`, `W2.md`, and `W3.md`, append to
each §Triumvirate Dispatch section (after the bulleted triggers):

```markdown

Empty/no-evidence returns get one verbatim redispatch with the same
worktree pointer. A second empty/no-evidence return triggers mandatory
triumvirate. Three diagnostic-loop iterations without isolating root
cause auto-trigger triumvirate. JSONL transcript quiet >15 minutes is
an orchestrator-side auto-trigger; record the condition and dispatch
time in `PROGRESS.md` alongside the wave's evidence ledger.
```

### Amendment 4 - HARD CAP block in Commit Plan

In every `docs/tranches/AZ-IV/waves/W<N>.md` §Commit Plan, prepend:

```markdown

Every dispatch carries `HARD CAP: N min. At 0.9N commit, at N halt.`
Defaults: research 20, plan 15, redress 30, audit 25. Read-only
audit/research agents do not commit at the cap; write-authorized
agents commit at 0.9N only when the staged slice is clean and owned.
```

### Amendment 5 - Generated-size budget for W0

In `docs/tranches/AZ-IV/waves/W0.md` §Verification Artefacts (currently
line 115), add a fifth bullet:

```markdown
- `docs/benchmarks/AZ-IV/W0-generated-size.txt` with the per-grammar
  table below.

| Generated artefact | Pre-W0 LOC | Post-W0 LOC ceiling | Source |
|---|---:|---:|---|
| `crates/core/src/grammar/generated/bbnf.rs` | TBD at dispatch | +/- 5% of pre-W0 | regen output |
| `crates/core/src/grammar/generated/json.rs` | TBD at dispatch | +/- 5% of pre-W0 | regen output |
| `crates/core/src/grammar/generated/css_l4.rs` | TBD at dispatch | +/- 5% of pre-W0 | regen output |
| `crates/core/src/grammar/generated/css_pretty.rs` | TBD at dispatch | +/- 5% of pre-W0 | regen output |
| `crates/core/src/grammar/generated/google_sheets.rs` | TBD at dispatch | +/- 5% of pre-W0 | regen output |
| `crates/core/src/grammar/generated/ebnf.rs` | TBD at dispatch | +/- 5% of pre-W0 | regen output |
| `crates/core/src/grammar/generated/bnf.rs` | TBD at dispatch | +/- 5% of pre-W0 | regen output |

The orchestrator records pre-W0 LOC at base commit. Overflow blocks
W0 close until the regression is traced, deliberately accepted with a
recorded ceiling raise, or rolled back.
```

### Amendment 6 - W2 / W1 hard-gate evidence paths

In `docs/tranches/AZ-IV/waves/W1.md` §Hard Gate, replace item 1
(currently line 107) with:

```markdown
1. Regenerated tempdir outputs are used for parity. Evidence:
   `docs/benchmarks/AZ-IV/W1-parity-matrix.txt` shows each parity
   suite ran against `cargo xtask regen --tempdir` output, not
   checked-in `crates/core/src/grammar/generated/**`.
```

In `docs/tranches/AZ-IV/waves/W2.md` §Hard Gate, replace item 7
(currently line 113) with:

```markdown
7. DTA/dfa stale runtime claims are deleted; dead inline code is
   gone. Evidence: `rg -n "emit_dfa_inline_body|DTA_TABLE|tape walker|dta walker"
   crates/ src/ docs/ --files-with-matches` returns only explicitly
   archived doc paths.
```

## Verification Commands Run

```text
git worktree list
wc -l docs/tranches/AZ-IV/{AZ-IV,GESTALT,PROGRESS}.md docs/tranches/AZ-IV/waves/*.md docs/tranches/AZ-IV/audit/*.md
wc -l /Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/{README,ORCHESTRATION,LESSONS-LEARNED}.md
wc -l /Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/tranche/{SPEC,WAVE_SPEC,AGENT_DISPATCH_TEMPLATE}.md
grep -nE "HARD CAP|HARDCAP|hard cap" docs/tranches/AZ-IV/waves/W*.md docs/tranches/AZ-IV/AZ-IV.md docs/tranches/AZ-IV/PROGRESS.md
grep -nE "generated[- ]size[- ]budget|generated.size.budget|line.count|LOC" docs/tranches/AZ-IV/waves/W*.md docs/tranches/AZ-IV/AZ-IV.md
grep -nE "close.honesty|close-honesty" docs/tranches/AZ-IV/waves/W3.md docs/tranches/AZ-IV/AZ-IV.md docs/tranches/AZ-IV/PROGRESS.md
grep -nE "commit-discipline|commit_discipline" docs/tranches/AZ-IV/waves/W*.md docs/tranches/AZ-IV/AZ-IV.md
grep -nE "empty[/ ]?(no.evidence )?return" docs/tranches/AZ-IV/waves/W*.md docs/tranches/AZ-IV/AZ-IV.md
grep -E "^### AZ-IV.W[0-9]" docs/tranches/AZ-IV/waves/W*.md
grep -E "^## " docs/tranches/AZ-IV/waves/W*.md
find crates/core/src/backend/rust/emitter -maxdepth 2 -type d
```
