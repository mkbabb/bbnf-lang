# AM Retrospective

Forensic reading of AM: commits `4d1afeb0`..`69841997`, all landed
2026-04-12 08:01..11:44 EDT (~3 h 43 min, single day). Plan:
`docs/tranches/AM/AM.md`. No PROGRESS, FINAL, research/, or audit.

## 1. Scope reality vs plan

Plan declared 7 phases (AM.0–AM.6). Reality:

- **AM.0** landed (`4d1afeb0`): 4 blocker fixes.
- **AM.0+** appeared in-flight (`47d6fafb`): CSP soft-index + 269×
  compile speedup — undeclared at plan time; folded post-hoc.
- **AM.1** landed (`76085303`): −2306 LOC EmissionTier/BumpSlab.
- **AM.2** landed as substrate (`17f794e0`): payload buffer with
  zero codegen consumers. Activation deferred to AN Phase 0.
- **AM.3** landed (`cffcb6ba`) with two latent codegen bugs
  (`__has_children` never set; inner `__branch_idx` clobbered) —
  each repaired in AN.0.1/AN.0.2.
- **AM.4** landed in `parse-that` (`b079c0b`) as shelf-code: doc
  admits "~neutral impact"; no routing into emitter.
- **AM.5** landed in `parse-that` (`0bb7e22`) as infrastructure
  only; AM.md annotates as "structural bitmap scanner
  (infrastructure)". No codegen integration.
- **AM.6** (cost model grid sweep) — not attempted; silently
  absent from the commit history and eventually routed to AX
  (`43901076`).

## 2. Silent vs declared deferrals

All deferrals silent. AM.md declared none at plan time. Post-hoc
the plan was edited to annotate AM.4 as "neutral" and AM.5 as
"infrastructure"; AM.6 simply vanished. The "Remaining gap
analysis" section was appended after the fact to rationalise
AM.5's non-integration as architectural, not incomplete.

## 3. Orchestration friction

No wave structure; commits are solo, sequential. No cherry-picks
or worktrees in the log. Interleave with **AL.1** (`e6574a98`,
18:35) and **AN.0.1+** (`8012dacb`, 14:18) on the same day
indicates AM never "closed" before AN opened. Absent PROGRESS
tracking hides the overlap.

## 4. Agent-layer friction

Not observable — no multi-agent artefacts. Single-operator cadence.

## 5. Edict adherence

Measured against `docs/instructions/README.md` (which was
consolidated *after* AM, 2026-04-15):

- **Tranche directory structure**: violated. Only `AM.md` existed;
  no `PROGRESS.md`, `FINAL.md`, `research/`. Directory form was
  applied retroactively in `36945f60` (2026-04-15).
- **`post-AM.json` bench artefact**: missing. Bench numbers exist
  only as prose tables in `AM.md`.
- **Commit cadence**: acceptable (12 commits, milestone-granular).
- **Worktree isolation / cache clear / file-first**: no evidence
  either way; no artefacts preserved.

## 6. Chronic deferrals

Inbound to AM — none declared (AL had no FINAL either).
Outbound from AM:

- **AM.2 payload activation** → AN Phase 0 (three-tier payload
  projection, `PayloadKind`).
- **AM.3 correctness** → AN.0.1, AN.0.2 (regression redress).
- **AM.4 SIMD scanner routing** → shelf; later partly revived by
  AN.3.1 `@ws` routing.
- **AM.5 structural bitmap codegen integration** → AO, AP, AU
  (AP.1b `synchronized peek-only structural dispatch`,
  AU.2.7 `structural_alphabet` pass). Three tranches of partial
  activation; AQ.5 eventually **deleted** the structural dispatch
  infrastructure entirely.
- **AM.6 cost model grid sweep** → routed to AX (`43901076`,
  2026-04-16).

## 7. Mid-tranche restructuring

Restructuring was editorial, not procedural: the plan document
was rewritten during execution to add "What landed" and
"Remaining gap analysis" sections retroactively rationalising
scope shrinkage. No re-plan wave; no new agents dispatched. The
AM.0+ CSP work was folded in without declaration.

## 8. Lessons

1. **Substrate-only landings are debt.** AM.2 and AM.5 shipped
   without consumers; each bred a multi-tranche activation saga.
   Forbid "infrastructure" commits without a same-tranche wiring
   commit — consonant with the AW edict against placeholder arms.
2. **Missing PROGRESS is a structural failure mode.** When the plan
   is the only artefact, scope shrinkage becomes invisible via
   post-hoc plan edits. Require PROGRESS from tranche start;
   retro reconstruction from commits alone is expensive.
3. **Ship correctness before compounding optimisations.** AM.3
   booked +16% on canada while latent-breaking `__has_children`
   and `__branch_idx`; AN.0 paid the interest. Per-branch epilogue
   surgery required golden-parity before the bench celebration.
