# R2 — Instructions & Edicts Redress

## Summary

Dominant friction pattern: **the normative set treats walker-parity
as a structural invariant when walker is the scaffold being retired.
AX.W0a.2.d through .g burned ~20 agent-hours chasing walker-parity
deltas that a single edict ("walker-parity is a snapshot oracle; Value-
level external-parity is the correctness oracle") would have cut to
one sub-wave.**

Secondary: **sub-agent prompts re-establish ~800 words per dispatch
with no template.** Seven W0a dispatches = ~5,600 re-derived words.

Tertiary: **two operational mitigations surfaced under contact
(`CARGO_BUILD_JOBS=4`; worktree `target/` symlink) live only in
PROGRESS.md.** Next tranche's orchestrator rediscovers them.

Seven surgical edits; one briefing template + one oracle edict
deferred to restart.

## A1 — Prescriptive overhead

`README.md` (557L): ~45% invariants, ~35% process, ~12% evidence,
~8% ceremony. `SPEC.md` (403L): ~40% process, ~25% invariants,
~20% anti-pattern citations, ~15% ceremony. `WAVE_SPEC.md` (187L):
~80% format prescription.

Ceremony candidates for compression:
- `README.md:550-557` §Indefatigability duplicates `SPEC.md:318-341`.
- `SPEC.md:348-373` §Edicts re-enumerates README invariants.

Load-bearing cores: README §Code discipline (60-126), §Parallel
orchestration (127-261); SPEC §Activation-gate (185-205), §Scope-
reveal (236-300); WAVE_SPEC §Prohibitions (144-161).

## A2 — Conflicting / unenforceable edicts

### A2.1 "No workarounds" vs "Escape clause"

`README.md:60` ("NO workarounds") collides with `README.md:291-298`
escape clause; SPEC.md §Scope-reveal then licenses `Absorb` mode.
A README-only reader would call W0a.2.d→.g re-planning silent
deferral; a SPEC reader sees it as Absorb.

**Citation.** `post-AX-W0a2d-diag.md` §Suggested re-plan — three
follow-on agents dispatched under Absorb label with no README
sanction.

**Redress.** Delete escape clause from README; single-source in
SPEC.md with the Absorb/New-letter discriminator. **Edit 1.**

### A2.2 "One codegen path" vs. walker-in-transit

AX Invariant 1 ("One codegen path — no fallback") contradicts
walker-as-fallback across seven W0a sub-waves. SPEC.md offers no
carve for "in-transit fallback during elimination waves."

**Citation.** `post-AX-W0a2f-progress.md` §What reverted — widening
rolled back precisely to keep walker fallback green.

**Redress.** Add a named clause to SPEC.md §Scope-reveal:
"Transitional fallback during elimination waves." The invariant
binds at tranche close, not at every wave close. **Edit 2.**

### A2.3 "No deferrals" vs Absorb-mode boundary

SPEC §Absorb (262-270) is valid response; README:70 (`NO deferrals`)
makes Absorb look indistinguishable from deferral. README readers
don't see the mechanical discriminator in SPEC 262-286.

**Redress.** README:70-74 appended: "Absorb and new-letter responses
per SPEC.md §Scope-reveal are not deferrals." **Edit 3.**

## A3 — Missing guidance AX.W0a reveals

### A3.1 Serial-probe anti-pattern

W0a.2.d → .e → .f → .g each probed one blocker, found another,
reverted, re-probed. Cascading hidden-blocker discovery consumed
four sub-waves.

**Citation.** `post-AX-W0a2e-progress.md` (LLVM cycle) →
`post-AX-W0a2f-progress.md` (Keyword Ref gap) →
`post-AX-W0a2g-progress.md` (four more blockers: Flat Next/Skip,
Ref→HRegex Rule wrap, Repeat lo-guard, zero-length Seq elision).

**Redress.** SPEC §Scope-reveal gains: "Parallel-probe on >2
candidate blockers." When diagnosis enumerates multiple
architectural blockers, dispatch parallel probe-agents (one per
blocker) rather than serial-patch the first. **Edit 4.**

### A3.2 Aggregate-test-binary compile-RSS ceiling

`post-AX-W0a2d-diag.md` — single rustc peaked at 26 GB RSS on an
aggregate tape_parity binary linking 5 derive-Parser sites. Split
per-grammar at `61053374` dropped to ~3 GB per child under
`CARGO_BUILD_JOBS=4`. Neither the ceiling nor the split pattern
appears in instructions.

**Redress.** README gains §"Memory discipline for aggregate test
binaries": split ≥4-derive-site binaries; cap rustc parallelism.
**Edit 5.**

### A3.3 Worktree target symlink

`PROGRESS.md:146` — "worktree target/ now symlinks to main target/."
PROFILING.md sanctions one `CARGO_TARGET_DIR` but doesn't describe
the symlink escape for worktree-isolated sub-agents.

**Redress.** README §Worktree isolation gains: symlink
`<worktree>/target → <main>/target` before first build. Worktree
git isolation ≠ build isolation. **Edit 6.**

## A4 — Over-specified hard-gate framings

`W0a.md` gate 4 ("Bootstrap regen idempotent") forces a regen cycle
for a gate that could close pre-regen via source-grep + `cargo
expand`. ~15 min × ~20 regens during W0a.2 = 5 h regen-only wall
time. `W0b.md` gates 5 + 6 conflate idempotency with LOC delta;
both demand post-regen.

**Redress.** SPEC §Hard gates §Runtime-evidence clause gains a
distinction: **pre-regen** (source-grep / `cargo expand` against
current generated.rs) vs **post-regen** (bootstrap idempotency).
Gate authors phrase each gate explicitly. **Edit 7.**

## A5 — Sub-agent briefing template

W0a sub-agent prompts repeated ~800 words of: worktree path,
`CARGO_BUILD_JOBS=4`, read-first order, allow-list, forbidden
list, hard gate, return format, non-negotiables echo. Seven
dispatches = ~5,600 words.

**Redress.** New `docs/instructions/tranche/AGENT_BRIEF_TEMPLATE.md`
— boilerplate in template, per-wave prompts shrink to substitutions.
Target 50% reduction. **Deferred — size > 20-line surgical threshold.**

## A6 — The pivot-as-default

Seven sub-waves presumed walker-parity == correctness. But walker
is the retiring scaffold; shape emission is the target. The right
oracle is Value-level external-parity (W2 harness: sonic-rs,
lightningcss, serde_json). Once those pass, tape-shape delta
between walker and shape emission is artefact-of-scaffold, not
correctness blocker.

**Redress edict.** "Walker-parity is a snapshot oracle during
scaffold transition. Once `*_parity.rs` tests cover a grammar at
Value API level against external comparators, walker-parity is
advisory, not blocking. A wave completing on external-parity green
may ship intentional tape-shape divergence." Proposal **P2 —
deferred to restart.** This would have foreclosed W0a.2.d–.g.

## Edit manifest

| # | File | Lines | Before → After |
|---|------|-------|---|
| 1 | `README.md` | 291–298 | Escape-clause paragraph → one-line cross-ref to SPEC §Scope-reveal |
| 2 | `tranche/SPEC.md` | new, post-300 | New §"Transitional fallback during elimination waves" — 6 lines |
| 3 | `README.md` | 70–74 | Append: "Absorb/new-letter per SPEC §Scope-reveal are not deferrals." |
| 4 | `tranche/SPEC.md` | new, post-236 | New §"Parallel-probe on >2 blockers" — 4 lines |
| 5 | `README.md` | new, post-320 | New §"Memory discipline for aggregate test binaries" — 6 lines citing W0a |
| 6 | `README.md` | 149–164 | Append symlink-escape paragraph in §Worktree isolation |
| 7 | `tranche/SPEC.md` | new, post-220 | Pre-regen vs post-regen evidence distinction — 5 lines |

All edits < 20 lines; each cites specific W0a sub-wave friction.

## Proposals deferred to restart

- **P1 — `AGENT_BRIEF_TEMPLATE.md`**. ~60-line template file;
  halves per-dispatch prose. Orchestrator authors in restart.
- **P2 — Walker-parity oracle edict**. Either new `docs/instructions/
  tranche/ORACLE.md` or AX invariant 20. Architectural framing
  requiring orchestrator + user sign-off. Would have collapsed W0a.2
  from seven sub-waves to one.
- **P3 — Hard-gate floor-check tooling**. SPEC §Gate floor-check
  prescribes manual minimum-achievable computation; a sampler script
  at plan-time would mechanise it. Defer to W13/W14 tooling.

Every proposal traces to a specific sub-wave friction citation.
Edits are delivered in the companion commit.
