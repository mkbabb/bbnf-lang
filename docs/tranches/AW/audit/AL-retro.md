# Tranche AL — Retrospective

## 1. Scope reality vs plan

No canonical `AL.md` plan was ever written. `docs/tranches/AL/` contains
only `research/prototype-1..4.md` — four competing audit/plan drafts
(tape purity, three-tier activation, emission unification, maximal
string-scanner pass) that were never reconciled into an executed plan.
No `PROGRESS.md`, no `FINAL.md`, no `post-AL.json`.

Actual landed work under the AL label: a single commit,
`e6574a98 fix(serialize): rewrite codegen for tape-first architecture
(AL.1)` — a targeted codegen rewrite to migrate `serialize_*` onto
`NodeView`/tape cursors after the enum-variant matching world was
removed. Net diff: +167 / −481; 22 serialize tests pass, 1 ignored.

The four prototypes each targeted ~5–7 phases (AL.0..AL.7): delete
EmissionTier, per-branch `mark_children`, allocation surgery, global
CSP, e-graph→CSP feedback, TaggedUnion, SIMD structural bitmap,
sonic-rs string parity. None of those landed under the AL label.

## 2. Silent vs declared deferrals

Effectively everything in the prototypes deferred silently. Because
no `AL.md` was written, the deferral was not **declared**; the plan
itself never coalesced. Items re-surfaced across downstream tranches:
EmissionTier deletion → AM.1; per-branch `push_leaf`/`push_compound` →
AM.3; tape payload → AM.2; SIMD structural bitmap → AM.5 and onward;
payload projection → AN Phase 0; structural dispatch delete → AQ.5;
payload layouts → AQ.6.

## 3. Orchestration friction

The singular AL.1 commit (18:35) landed **after** the AN plan commit
(`acaa1898` at 14:06) and after three AN.0 fix commits. The AN Phase 0
doc-fold (`17728fd7`, 18:48) then retroactively describes AL.1 as
"Phase 1.1" of AN. AL was preempted mid-stream: its label was reused
for a scoped correctness fix while planning had already shifted to AN.
No agent-claim falseness observed — the work is real — but the
tranche-identity discipline collapsed.

## 4. Agent-layer friction

The four `prototype-N.md` drafts are coherent individually, with
cross-references to file paths and line numbers. They disagree on
primary target (correctness vs sonic-rs gap vs CSP cleanup), signalling
no pre-dispatch convergence. No single agent was handed AL-authoritative
execution; no mid-stream re-brief exists in-tree.

## 5. Edict adherence

- Tranche-directory convention (`{LETTER}/{LETTER}.md`,
  `PROGRESS.md`, `FINAL.md`) was **not** satisfied — only `research/`
  materialised.
- Commit-at-milestone cadence was met for AL.1 itself (atomic, tested).
- No `post-AL.json` baseline was captured; AL.7.1 verification gate
  was implicit in every prototype but skipped.

## 6. Chronic deferrals carried

Into AL (from AK): three-tier activation, string-scanner gap to
sonic-rs, EmissionTier deadness, CSP globalisation. Out of AL: all of
the above, routed through AM/AN/AQ over the next 10 tranches.

## 7. Mid-tranche restructuring

Full restructure in flight. AN plan drafted, AL.1 folded into AN's
Phase 1.1 narrative within ~13 minutes of AL.1 committing. Research
prototypes were leveraged informationally but never re-dispatched
with a reconciled charter. No re-brief recorded.

## 8. Lessons

1. **Plan-before-execute is load-bearing.** When four prototypes
   compete and no reconciled `{LETTER}.md` lands, the tranche label
   becomes a stray sticker. Require `{LETTER}.md` as a hard gate
   before any `{LETTER}.N` commit.
2. **Label-integrity on mid-tranche pivots.** When scope pivots,
   open a new tranche letter immediately (per the
   `new-tranche-new-doc` edict) — do not reuse the old label for
   residual commits. Reuse corrupts the retrospective record.
3. **Research-prototype coalescence is a dedicated step.** 4
   prototypes → 1 plan needs an explicit merge agent, not implicit
   orchestrator judgment, so the loser scopes are declared-deferred
   rather than silently abandoned.
