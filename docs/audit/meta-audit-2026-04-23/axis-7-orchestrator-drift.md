# Axis 7 — Orchestrator Drift and Epistemic Quality

Current read-of-record for this audit: `5a260f94`.

Scope audited:

- `docs/GESTALT.md`
- `docs/RISK-PERF-MATRIX.md`
- `docs/tranches/AZ-I/AZ-I.md`
- `docs/tranches/AZ-II/AZ-II.md`
- `docs/tranches/BA/BA.md`
- `docs/tranches/BB/BB.md`
- `docs/instructions/CHANGELOG.md`
- current live repo state where the docs make present-tense claims

This is the uncomfortable axis. The question here is not whether the
audit was broadly useful. It was. The question is whether the audit's
own authoritative surfaces still carry softened commitments, stale
decision edges, or "resolved" claims that are stronger than the live
docs support.

## Verified

### 1. The standalone-`ir-rewrites` crate drift was corrected at the master-narrative level

The live repo no longer contains the original bad shape:

- repo grep returns zero hits for `crates/ir-rewrites`
- `docs/GESTALT.md:1180-1184` explicitly says rules live under
  `crates/ir/src/rewrites/` as a module within `bbnf-ir`
- the same section explicitly rejects the standalone-crate form

So the user-caught correction did land at the canonical narrative
surface. This part of the drift is real and fixed in the place that
matters most.

### 2. The cascade-math overstatement was corrected in direction

`docs/RISK-PERF-MATRIX.md:228-234` no longer claims the AZ split
improves raw multiplicative probability. It now says the opposite:

- splitting AZ lowers raw joint probability
- the benefit is reversal scope and cleaner checkpoints, not better
  raw odds

That is the right correction, and Axis 6 confirms the direction is
honest even though the arithmetic presentation still needs tightening.

### 3. The anti-drift feedback rules are now explicit, not implicit

The audit did not merely hand-wave "be disciplined." It codified the
anti-drift rules in first-read docs:

- `docs/GESTALT.md:1084-1107` records
  `feedback_execute-planned-architecture` and
  `feedback_no-orthogonal-codepaths`
- `docs/instructions/README.md` and
  `docs/instructions/tranche/SPEC.md` preserve the corresponding
  orchestration and scope-reveal discipline

That matters because the user's complaint was not just architectural.
It was epistemic: stop softening hard decisions into options. The audit
did react to that.

## Refined

### 1. The commissioning brief for this meta-audit is itself stale, and the audit should say so plainly

Several "fixed facts" in the brief are already behind live `HEAD`:

- the brief says `33` audit-epoch commits; live
  `git log 48e6eaa9..HEAD --oneline | wc -l` is `46`
- the brief says `1876` total commits; live
  `git rev-list --count master` is `1888`
- the brief says `GESTALT.md` is `1275` lines and
  `RISK-PERF-MATRIX.md` is `363`; live files are `1343` and `423`

This is not a flaw in the original audit. It is a caution for the
meta-audit: the brief must be treated as a hypothesis surface, not a
source of truth.

### 2. The instructions-streamlining pass preserved what it claimed to preserve, but only within its declared scope

`docs/instructions/CHANGELOG.md:1-82` is specific:

- the streamlining target was `README.md` and `PROFILING.md`
- the claim "No rule dropped" is about those two files

It is therefore too strong to read the streamlining pass as "the whole
instructions stack was normalized." The tranche subdocs still exist as
their own layer, and that appears to have been an intentional scope
boundary rather than silent loss.

### 3. "Defensible floor" is mostly consistent, but not fully normalized across the plan stack

The good version of a defensible floor is:

- under pressure, preserve the irreversible architectural gain and
  carry the residual gap explicitly

That is the posture in B1 and most of BA. But AZ-II and the risk matrix
still drift toward a softer reading where the floor becomes a partial
strategic completion state rather than a pressure-floor that still
blocks incompatible successor tranches. The distinction is the heart of
this axis.

## Flawed

### 1. The halt-after-AZ-I drift was not fully eliminated; it mutated into `bbnf-tape-mini`

The user-corrected issue was: AZ-II is required, not optional.
The live docs now say that sentence, but the operational logic still
undercuts it.

Evidence:

- `docs/GESTALT.md:617-623` says AZ-II is required, not optional
- but `docs/GESTALT.md:594-597` still defines an AZ-II floor of partial
  tape dissolution via `bbnf-tape-mini`
- `docs/RISK-PERF-MATRIX.md:148` repeats the same floor
- `docs/RISK-PERF-MATRIX.md:163-166` then says BA still opens if AZ-II
  invokes `bbnf-tape-mini`
- `docs/tranches/AZ-II/AZ-II.md:280-297` operationalizes the escape
- `docs/tranches/BA/BA.md:63-80` says the opposite: BA does **not**
  open on a partial substrate

This is the same drift pattern in a subtler form. The explicit
"permanent halt after AZ-I" framing is gone, but the plan still carries
an escape that preserves tape residue while claiming tape deletion is
non-optional.

### 2. The `ir-rewrites` drift is not repo-complete

The main narrative is fixed. The owning tranche doc is not:

- `docs/tranches/BB/BB.md:375` still says
  `crates/ir/src/rewrites/ | create (new crate)`
- `docs/tranches/BB/BB.md:392` still says
  `crates/ir/src/rewrites/ crate landed`

That is exactly the kind of residual doc drift that later orchestration
turns back into design drift. Axis 5 already found the same issue from
the GESTALT side; here the point is epistemic: the correction was
accepted, but not carried through the owning spec.

### 3. `feedback_execute-planned-architecture` is undermined by GESTALT's own closing thesis

The document that most strongly preaches "execute the declared
architecture" still closes on an obsolete architecture:

- `docs/GESTALT.md:1336-1341` says the target is
  "a direct-to-struct tape-first runtime parser" and
  "every `->` reaching the tape"
- `docs/tranches/AZ-II/AZ-II.md:69-79` says the tape is deleted
- `docs/tranches/BA/BA.md:57-74` says the struct tree is the only
  materialized substrate BA accepts

This is not just stale prose. It weakens the audit's most important
anti-drift rule by preserving the old end-state in the closing summary.

### 4. The decision record overclaims present-tense resolution in at least three places

Section 10 of `docs/GESTALT.md` treats several items as settled and
owned when the live tree does not fully support that:

- `docs/GESTALT.md:1191-1194` says cross-worktree pin drift is
  CI-guarded by a shared GitHub Actions workflow, but
  `docs/tranches/B1/B1.md:98,241-245` only requires mirrored
  `rust-toolchain.toml` files, not the shared workflow
- `docs/GESTALT.md:1213-1216` says the gorgeous sibling was retired to
  `~/.Trash/gorgeous-retired-2026-04-23`, but the live audit host has
  no such visible path and no `../gorgeous` checkout
- `docs/GESTALT.md:1222` and `:1210-1212` cite
  `AZ-I/CLASSIFIER-UNIFICATION.md` and `AZ-II/BOOTSTRAP-CUTOVER.md`,
  which do not exist in the working tree

These are not harmless future-facing aspirations because the section is
presented as a decision record, not a backlog.

## Open

### 1. The plan still needs one authoritative answer on `bbnf-tape-mini`

The live plan has not chosen between two incompatible positions:

- `bbnf-tape-mini` is a real allowed AZ-II floor that BA can tolerate
- or AZ-II must close with full tape deletion before BA can open

Both positions appear in current authoritative docs. One must go.

### 2. The audit would benefit from one explicit "historical snapshot vs live truth" convention

Several documents are valuable but time-bounded:

- archaeology numbers pinned to `48e6eaa9`
- GESTALT appendix branch tables
- appurtenant bench totals

The repo needs a consistent label for this. Without it, stale snapshot
facts keep masquerading as live operational truth.

### 3. A normalization pass should land before these documents are treated as execution canon

The direction is strong. The remaining drift is mostly not about
architecture choice; it is about plan-surface truth:

- AZ-II / BA / risk matrix agreement
- BB `ir-rewrites` wording
- GESTALT dead citations and stale closing thesis

Until that normalization lands, the audit is a good synthesis and a
partially unsafe execution surface.

## Axis 7 Verdict

The audit did catch and correct the most obvious forms of orchestrator
drift. It is materially better than the state the user intervened on.
But the correction is incomplete. The same drift pattern survives in
more respectable clothing:

- "optional continuation" became "`bbnf-tape-mini` floor"
- "new crate" survived in an owning tranche after being rejected in the
  capstone
- "execute planned architecture" coexists with a capstone closing
  paragraph that still describes the superseded architecture

So the honest verdict is: **drift was reduced, but not closed. The plan
still needs one deliberate redress pass on its own authoritative
surfaces before execution should treat it as canon.**
