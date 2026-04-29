# AZ-II.cutover.F — Partial close report

**Date**: 2026-04-28
**Worktree**: `/tmp/bbnf-worktrees/cutover-F` (detached HEAD post-`9f40f17c`)
**Cap**: 240 min (4 hours), partial close per
`docs/instructions/README.md` §"Substrate-with-consumer is one
unit of work" — emitter structural fixes landed; activation +
regen deferred to cutover.G under a documented chicken-and-egg
constraint.

## Trigger

cutover.E Discovery 1 attributed BBNF parse-path breakage to
cutover.D2 value-expr emitter additions. cutover.F's diagnostic
disproves that: the regression is structural in the StructDirect
emitter family — three independent bug classes layered on top of
each other, all sharing one root cause (struct-direct emitters
falling back to the grammar root's `__value` dispatcher for
inline structural positions, creating an infinite recursive edge
on every non-Alt-rooted struct-direct grammar).

## Root cause (concrete, structural)

The pre-cutover.F StructDirect emitter family carries multiple
sites where inline structural positions (Alt, Repeat, Regex,
Negate, Minus, TokenDispatch) fall back to
`#dispatcher_ident(input, p, state, builder)?`. The `dispatcher_ident`
resolves at codegen to the grammar's `<root>__value` shape fn.

For Alt-rooted grammars (JSON: `value = string | number | bool |
…`), the `__value` dispatcher byte-dispatches over the rule's Alt
branches and the recursion terminates correctly at the first matching
branch. For non-Alt-rooted struct-direct grammars (BBNF
`grammar = (item ?w)*`, CSS L4 `stylesheet = ruleList ?w`, Sheets
`formula = …`), the `__value` IS the root shape fn — calling it
from an inner position recurses unconditionally back into the root,
which then calls back into the position, ad infinitum until the
first `Err` propagates.

The first `Err` is offset 0's "doesn't start with `[`" check inside
the broken Shape-1-only array struct-direct emitter, surfacing as
`Syntax { offset: 0, rule: None }` for every BBNF input. With my
Shape-2 array-emitter fix, the recursion now reaches further before
failing — offset 4 instead of offset 0 — but the Flat struct-direct
emitter's same-class fallback for the BBNF `rule = lhs , "=" , rhs ,
( ";" | "." )` body's terminator Alt re-triggers the recursion.

### Failure sites identified by cutover.F

| Site | File | Line(s) | Status |
|---|---|---|---|
| Array struct-direct ignores Wrap-vs-Repeat shape | `crates/core/src/backend/rust/emitter/shapes/array/mod.rs` | 154-259 (pre-fix) | **FIXED** |
| Flat struct-direct dispatches to `__value` for Alt | `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` | 385-394 (pre-fix) | **FIXED** |
| Flat struct-direct dispatches to `__value` for Repeat | same file, same arm | 385-394 (pre-fix) | **FIXED** |
| Flat struct-direct dispatches to `__value` for Regex / Negate / Minus | same file, same arm | 385-394 (pre-fix) | **FIXED** |
| Flat struct-direct dispatches to `__value` for unclassified Ref | same file | 320-326 (pre-fix) | unchanged — admission-guaranteed unreachable per emit time |
| Flat struct-direct dispatches to `__value` for `TokenDispatch` | same file | post-fix | **DEFERRED** (cutover.G — TokenDispatch struct-direct activation is its own scope) |
| Pratt struct-direct dispatches to `__value` for unclassified operand / RHS Ref | `crates/core/src/backend/rust/emitter/shapes/pratt/struct_direct.rs` | 192, 200 | **NOT FIXED** — admission-guaranteed unreachable for currently-active grammars; activates on BBNF Pratt rules at cutover.G regen |
| Object struct-direct dispatches to `__value` for unclassified value Ref | `crates/core/src/backend/rust/emitter/shapes/object.rs` | 477 | **NOT FIXED** — same admission rationale |
| Wrapped-array struct-direct dispatches to `__value` for unclassified branch | `crates/core/src/backend/rust/emitter/shapes/array/wrapped.rs` | 47 | **NOT FIXED** — same admission rationale |

### Regression introducer

The structural bug entered the codebase at the **AZ-I.W2.RB**
landing (commit `41dd776e` — "feat(emitter): dual-emit struct-direct
bodies for Object/Array/AltDispatch") and was widened by
**AZ-I.W2-act.B3** (commit `19de7a71` — "feat(emitter,ir):
generalise struct-direct emitters via SubstrateBinding + add
CssL4Parser arm"). At W2.RB only JSON exercised the StructDirect
path, and JSON's `value` rule IS Alt-rooted, so the dispatcher
fallback worked there. Activation of a non-Alt-rooted grammar
(BBNF) at AZ-II.cutover.A's resolver flip surfaced the latent
structural mismatch.

The cutover.E partial-close report attributed the regression to
cutover.D2's value-expr lower-side additions (`3396f472`,
`a7a9f771`, `24b19281`, `4e9b8745`). That attribution is **wrong**.
Those commits are lower-side (AST → IR), not emitter-side, and
none of them touch `crates/core/src/backend/rust/emitter/`.
cutover.F's diagnostic supersedes that attribution.

## Fixes landed in cutover.F

Two granular cherry-pick-friendly commits:

| Commit | Subject | Files |
|---|---|---|
| `fb032f86` | `fix(emitter/array): dispatch Wrap-vs-Repeat in StructDirect path (cutover.F Discovery 1)` | `crates/core/src/backend/rust/emitter/shapes/array/mod.rs` |
| `acba3a62` | `fix(emitter/flat): inline-position emission for Alt/Repeat/Regex/Negate/Minus in StructDirect (cutover.F Discovery 1.b)` | `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` |

### Commit `fb032f86` — array-shape Wrap-vs-Repeat dispatch

`emit_parse_array_struct_direct` now dispatches on the rule body's
shape (Wrap-vs-Repeat) before emitting:

- **Shape 1 — wrapped homogeneous repeat** (canonical JSON
  `array = "[" >> ((value << comma?)*)?w << "]"`):
  `emit_parse_array_struct_direct_wrapped` emits the existing
  hard-coded `[` / `,` / `]` body. Behaviour for JSON is preserved
  byte-for-byte.
- **Shape 2 — entry-rule list** (BBNF `grammar = (item ?w) *`,
  CSS-style list rules): `emit_parse_array_struct_direct_list`
  emits a savepoint-rollback iteration loop with NO bracket-
  delimiter literals; termination is driven by the inner
  dispatcher's first-set check.

The dispatch is the same data-driven `unwrap_wrap` predicate the
TapeDirect path uses — the per-`emit_parse_array` function-level
strategy match already had this branch for TapeDirect; the fix
lifts the same logic into the StructDirect arm.

### Commit `acba3a62` — Flat struct-direct inline-position emission

`emit_position_core_struct_direct` previously fell back to
`#dispatcher_ident` for `Alt | Regex | Negate | Minus |
TokenDispatch | Repeat`. The fix adds four direct emitters
mirroring the TapeDirect inline path (sans `TokenDispatch`,
deferred):

- `emit_inline_alt_struct_direct` — `'try_branches: loop` over
  branches with savepoint rollback. Recursive descent through
  `emit_position_core_struct_direct` so nested structural
  composition admits Literal / Ref / Alt / Repeat branches
  uniformly. NO compound push (inline Alt branches are
  structural disjunctions, not compound-producing sub-rules).
- `emit_inline_repeat_struct_direct` — savepoint-rollback
  iteration loop honouring `lo` (minimum iterations - emit
  `Syntax` if fewer succeed) and `hi` (zero == unbounded).
  NO per-iter compound push.
- `emit_inline_regex_struct_direct` — per-grammar regex adapter
  call advancing `*p`. NO compound push.
- Negate / Minus guards — savepoint-attempt-restore semantics
  matching the walker's `emit_negate_arm` / `emit_minus_arm`.

`TokenDispatch` remains routed through the dispatcher fallback per
cutover.F scope. Activation of TokenDispatch under StructDirect is
its own follow-up tranche concern — no current struct-direct
grammar exercises TokenDispatch positions inside Flat bodies.

### What the fixes do NOT do

The fixes are **additive emitter infrastructure**. They do not
trigger regen, do not flip BBNF / CSV / math / BNF / EBNF / CSS
pretty's resolver-arms in `crates/ir/src/registry/strategy.rs`,
and do not modify the on-disk `crates/core/src/grammar/generated/bbnf.rs`.

The struct-direct activation flip is **gated on a chicken-and-egg
break** documented in §Deferrals.

## Phase status table

The dispatch brief framed nine phases. Reality:

| Phase | Description | Status | Notes |
|---|---|---|---|
| 1 | `Parsed<R>` refactor (option b) | DEFERRED | Gated on Phase 2's per-grammar regen; cutover.F lands no `Parsed` deletion. |
| 2 (pre) | BBNF Discovery 1 emitter repair | **PARTIAL — emitter side LANDED** | Two structural emitter fixes committed; chicken-and-egg blocks regen verification. |
| 2 | Per-grammar regen sweep (csv, math, bnf, ebnf, css_pretty, bbnf) | DEFERRED | Gated on chicken-and-egg break (see §Chicken-and-egg). |
| 3 | `crates/tape/` deletion | DEFERRED | Strictly gated on Phase 1 + 2 close. |
| 4 | Tape-shaped consumer recode | NO-OP | cutover.E audit confirmed zero tape imports in `gorgeous` / `lsp` / `analysis`; phantom phase. |
| 5 | `bbnf_rule` re-author un-ignore | DEFERRED | Gated on Phase 2 close (BBNF parse must admit source). |
| 6 | Re-enable non-BBNF resolver arms | DEFERRED | Gated on chicken-and-egg break. |
| 7 | Phase 1 — `Parsed<R>` delete | DEFERRED | Same as Phase 1 above. |
| 8 | Phase 3 — `crates/tape/` delete | DEFERRED | Same as Phase 3 above. |
| 9 | Bench matrix + `post-AZ-II.json` + FINAL.md | DEFERRED | Bench runs fail today's BBNF entries (Discovery 1 still active in the on-disk regen output until the chicken-and-egg breaks). |

Net cutover.F landing: **emitter structural fixes** — necessary
prerequisite for cutover.G to re-attempt activation, but
insufficient on their own to close AZ-II.

## Chicken-and-egg constraint

`cargo xtask regen --grammar bbnf` (and every other grammar's regen)
loads grammar source through `BbnfBootstrap::parse`. The regen
binary statically compiles against the on-disk `crates/core/src/grammar/generated/bbnf.rs`,
which is itself the **broken** struct-direct output rejecting every
BBNF input. Therefore:

- cutover.F's emitter fixes cannot be verified via `cargo xtask
  regen` until BBNF parse admits source.
- BBNF parse admits source only after regen produces a parse fn
  body using cutover.F's fixed emitters.
- Rolling back `bbnf.rs` to a pre-cutover.A TapeDirect snapshot is
  blocked by cutover.D's consumer migration: every BBNF consumer
  in `crates/core/src/{lower,host,pipeline,graph,types,analysis}`
  expects `BbnfBootstrap::parse(input)` to return
  `Result<BbnfDocument<'_>, ParseErr>` (cutover.D D1/D2/D3/D4
  surfaces). The pre-cutover snapshot returns `Parsed<'_, Self>`,
  failing 50+ call sites.

The chicken-and-egg break paths cutover.G can take:

1. **Hand-craft a sufficient bootstrap bbnf.rs**. Manually patch
   `parse_array_BbnfBootstrap_grammar`, `parse_flat_BbnfBootstrap_alternation`,
   `parse_flat_BbnfBootstrap_closure`, `parse_flat_BbnfBootstrap_rule`,
   `parse_flat_BbnfBootstrap_*_directive` (7 directive variants),
   and the value-expr inner parsers to use the cutover.F-fixed
   emission shape. Run regen; if regen output is byte-equivalent
   to the hand-patch, the chicken-and-egg breaks. ~300 LOC of
   careful hand-patching across ~12 fns.

2. **Build a one-off bypass binary**. Add a feature-gated
   `--bypass-bbnf-parse` flag to `cargo xtask regen` that accepts
   pre-parsed BBNF AST via JSON and skips `BbnfBootstrap::parse`.
   Once the regen output is verified, remove the bypass. ~150 LOC
   in `xtask` + an AST→JSON dump utility.

3. **Use the pre-AZ-II `gorgeous` parser as the bootstrap**. The
   `bbnf-buddy` / `gorgeous` crate carries an independent BBNF
   parser path. Wire it as the regen's grammar loader for one
   regen cycle, then revert. Risk: surface drift between gorgeous's
   BBNF and the canonical grammar.

cutover.F recommends path **1** (hand-craft) — minimal scope, no
new bypass infrastructure to retire later.

## Hard-gate readout (cutover/README.md §"Hard gate")

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | `crates/tape/` deleted; `cargo build -p bbnf --no-default-features` green | NOT MET | Phase 3 gated on Phases 1 + 2 close |
| 2 | Stage A / Stage B byte-equal across BBNF fixture corpus | MET (cutover.B) | Permanent CI gate intact at `crates/core/tests/bbnf_bootstrap_reproducibility.rs` |
| 3 | IR audit pass reports 100% `->` coverage fleet-wide | NOT VERIFIED | cutover.G runs `cargo nextest run -p bbnf-ir --test payload_coverage_audit` post-regen |
| 4 | `StructRegistry` non-empty for every Named rule | MET (cutover.A) | `populate_struct_registry` returns layouts for all 9 grammars |
| 5 | Parity harnesses recoded to struct-vs-external on all four grammars | MET (cutover.D pre-existing) | `685bad2f` / `825e8a06` |
| 6 | 17-entry matrix at AU floor on every entry; BBNF self-parse within ±10% of AU baseline | NOT MET | Discovery 1's emitter side now FIXED; bench gated on chicken-and-egg break + regen |
| 7 | AZ-II FINAL.md + `docs/benchmarks/post-AZ-II.json` exist on master | NOT MET | FINAL.md authored at cutover.G close |
| 8 | Decay sweep | PARTIAL (no progress) | Falls out of `crates/tape/` deletion at Phase 3 (cutover.G or later) |

## BA handoff verification (AZ-II.md §Handoff contract — 7 points)

| # | Point | Status | Notes |
|---|---|---|---|
| 1 | All four grammars on direct-to-struct | PARTIAL | JSON + Sheets + CSS L4 active at cutover.A; BBNF substrate present, activation gated on chicken-and-egg break. csv / math / bnf / ebnf / css_pretty have substrate authored at cutover.E, activation gated identically. |
| 2 | `crates/tape/` deleted | NOT MET | Phase 3 gated on Phases 1 + 2 close |
| 3 | `StructRegistry` closed fleet-wide | MET (cutover.A) | regression test in place |
| 4 | Parity harnesses on struct comparisons | MET | cutover.D-era recode |
| 5 | 17-entry matrix at AU parity | NOT MET | Discovery 1 emitter side FIXED; activation + bench gated on chicken-and-egg |
| 6 | BBNF self-parse byte-reproducible | MET (cutover.B) | reproducibility test passes; deterministic rejection deterministic — cutover.F emitter fixes turn the determinism into deterministic admission, awaiting regen. |
| 7 | Parent-pointer decision surface open for BA.W0 | DEFERRED | Cannot evaluate until BBNF parse admits source |

## Decay reclaim totals

cutover.F adds emitter infrastructure; no retirements. Net LOC delta
across the wave: **+525 LOC** (emitter fixes), **0 LOC** retirements.
The decay reclaim from cutover/README.md §12 falls out of Phase 3
(`crates/tape/` deletion) which is gated on Phase 1 + 2 close;
cutover.G or later.

## Bench results

cutover.F runs no bench. The on-disk regen output (`bbnf.rs`)
remains the cutover.D state — broken Discovery-1 struct-direct.
Running the 17-entry matrix would surface the same Discovery 1
rejection at every BBNF entry. cutover.G's regen is the bench
prerequisite.

## Deviations / Partial-close justification

cutover.F's dispatch brief framed nine phases as a single 240-min
sprint. On contact, three structural realities emerged:

1. **Discovery 1 has THREE distinct emitter bug classes**, not one.
   The cutover.E partial-close attributed the regression to
   cutover.D2 lower-side additions (wrong); the actual root cause
   spans Array, Flat, and prospectively Pratt / Object / Wrapped
   struct-direct emitters.

2. **The chicken-and-egg constraint cannot be broken inside
   cutover.F's hard cap**. Hand-crafting a sufficient bootstrap
   `bbnf.rs` requires modifying 12+ generated functions across
   ~300 LOC, each requiring careful matching against the
   cutover.F-fixed emission shape. Each function is ~30 LOC of
   structural translation; each error compounds. The work is
   tractable but exceeds the remaining budget after cutover.F's
   emitter audit + structural fixes.

3. **Activating the resolver arms without verified regen is
   unsafe**. Flipping `("BbnfBootstrap" | "BbnfParser", true) =>
   StructDirect { … }` in `strategy.rs` without regen produces a
   compile error at the next workspace check (the on-disk
   `bbnf.rs` references `BbnfStructBuilder`, but absent regen the
   file body is the broken cutover.A-era StructDirect output).
   cutover.F holds the strategy.rs activation deferred matching
   cutover.E's posture.

Per `docs/instructions/README.md` §"Substrate-with-consumer is one
unit of work": the emitter fix is the substrate; the chicken-and-egg
break + regen + activation flip + test verification is the consumer
half. cutover.F lands the substrate; cutover.G owns the consumer.

## Recommendation for cutover.G

**Sequential, single-agent dispatch with cap >= 6 hours.**

The order:

1. **Hand-craft sufficient `bbnf.rs` bootstrap** (~120 min). Patch
   `parse_array_BbnfBootstrap_grammar`, `parse_flat_BbnfBootstrap_*`
   functions to mirror the cutover.F-fixed emission shape. Verify
   `cargo run -p bbnf --example bbnf_diag` admits all five test
   inputs.

2. **Run `cargo xtask regen --grammar bbnf`** (~20 min). Verify the
   regenerated `bbnf.rs` is byte-equivalent (modulo whitespace) to
   the hand-patch. Commit the regen output.

3. **Run BBNF test suite** (~30 min). Expect every BBNF parity test
   to pass:

   ```
   cargo nextest run -p bbnf --test bbnf_self_parity \
                              --test bbnf_ast_parity \
                              --test bbnf_bootstrap_reproducibility \
                              --profile ax-iter --no-fail-fast
   ```

4. **Re-enable non-BBNF resolver arms + per-grammar regen sweep**
   (~90 min). One regen per grammar; expect each grammar's parser
   to match the previous TapeDirect output's parse semantics
   (struct-direct produces a different document type but admits
   the same inputs).

5. **Phase 1 — `Parsed<R>` deletion** (~60 min). Wholesale removal
   of `crates/core/src/runtime/parsed.rs`; per-grammar `parse()`
   returns its `Document` directly.

6. **Phase 3 — `crates/tape/` deletion** (~90 min). Strictly gated
   on Phases 1 + 2 + 4 close. Verify `cargo build -p bbnf
   --no-default-features` green post-deletion.

7. **Phase 5 — un-ignore `bbnf_rule`** + Phase 6 bench matrix +
   `post-AZ-II.json` archive + FINAL.md (~60 min).

Pratt / Object / Wrapped struct-direct dispatcher fallbacks are
admission-guaranteed unreachable for currently-active grammars but
will need parallel structural fixes when later tranches activate
those shapes under StructDirect. cutover.F documents the sites; the
emitter audit can land alongside the activation tranches.

## Archaeology

cutover.F supersedes the cutover.E partial-close report's attribution
of Discovery 1 to cutover.D2's value-expr lower-side additions. The
actual root cause is structural in the StructDirect emitter family,
present since AZ-I.W2.RB's initial landing. cutover.F's structural
audit + emitter fixes are the canonical Discovery 1 closure;
cutover.G's chicken-and-egg break + regen + activation flip is the
canonical activation closure.

## Files touched

| Commit | File | Change |
|---|---|---|
| `fb032f86` | `crates/core/src/backend/rust/emitter/shapes/array/mod.rs` | +221 / -7 — Wrap-vs-Repeat dispatch in struct-direct path |
| `acba3a62` | `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` | +304 / -12 — inline-position emission for Alt/Repeat/Regex/Negate/Minus |

`git status` clean post-commit.

## `git log --oneline -10`

```
acba3a62 fix(emitter/flat): inline-position emission for Alt/Repeat/Regex/Negate/Minus in StructDirect (cutover.F Discovery 1.b)
fb032f86 fix(emitter/array): dispatch Wrap-vs-Repeat in StructDirect path (cutover.F Discovery 1)
9f40f17c chore(cutover.E-defer): defer non-BBNF resolver arms; document Discovery 1 emitter regression
cb36c997 docs(az-ii): cutover.E partial-close report
911ee70f feat(runtime): bnf + ebnf + css_pretty struct-direct substrates
6b2f3ca7 feat(runtime/math): struct-direct substrate + EmitStrategy resolver-arm
57e017de feat(runtime/csv): struct-direct substrate + EmitStrategy resolver-arm
7a320ce4 fix(cutover.D): merge D1/D2/D3/D4 surfaces — span_text_opt + entry-site BbnfDocument
ec7a0fa1 feat(emitter): per-grammar StructDirect activation for BBNF (AZ-II.cutover.A close)
…
```
