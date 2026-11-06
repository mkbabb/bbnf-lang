# Session durability protocol

## One authority

`STATE.md` owns current status. Ledgers own evidence and dispositions. Wave files
own immutable acceptance contracts. No other document may say “next” or “active.”

## State dimensions

Every obligation row has independent fields:

```text
authority: proposed | admitted | superseded | retired
spec: draft | banked | sealed
code: unstarted | in_flight | landed
evidence: owed | captured | adjudicated | verified
durability: session_only | filesystem_only | git_durable | pushed
```

“Done” is not a legal substitute.

`OBLIGATION-LEDGER.tsv` carries these dimensions from formation onward. A vague
lane name is not an owner; each banked row needs a trigger, acceptance artifact,
and terminal disposition.

## Dispatch record

Before launch, record:

- stable task ID;
- exact input paths and hashes;
- clean HEAD and dirty digest;
- owner and actual callable model metadata;
- permitted mutations;
- expected durable artifact;
- acceptance class;
- exact resume command.

On return, reconcile disk and journals before trusting a summary. Never duplicate
a task because a chat response is absent.

## Walls

At a session wall:

1. stop launching;
2. inventory live tasks;
3. hash immutable artifacts;
4. record owners and dirty paths;
5. record the exact restart command;
6. mark uncommitted work `filesystem_only`.

Do not create a new scheduler, heartbeat, or run ledger for each wave. The active
task system plus `STATE.md` is sufficient.

## Review

Mechanical checks may be performed by one worker and replayed by root. Independent
hostile tri-fold review is reserved for the seams named in the charter.

Technical evidence remains evidence even if model-provenance ceremony is
incomplete. It simply cannot become an admitted judgment until provenance and
replay are sufficient.

## Git

No tranche may call filesystem-only work sealed. Commit authorization is separate.
When granted, use the repository's `commit-discipline` skill, preserve unrelated
staged/dirty work, and commit only a coherent slice with evidence.

## Archaeology

The lossless Claude extractor is:

`/Users/mkbabb/Programming/.p-totality/sci/atlas/docs/tranches/Q/audit/2026-07-24-perfection/instruments/anamnesis-extract.py`

The Codex root census and orchestrator are its sibling
`codex-anamnesis.mjs` and `anamnesis.mjs`.

The Python extractor classifies owner text separately from tool results,
sidechains, metadata, task notifications, and continuation summaries; records
line/bytes/SHA-256; includes zero-owner sessions; refuses nonempty output; and
verifies its counts. Future archaeology should reuse that receipt discipline.

Coverage must always distinguish independent owner sessions from execution
rollouts and subagents.

The formation packet is presently `filesystem_only`. Its receipt proves local
integrity, not git durability.
