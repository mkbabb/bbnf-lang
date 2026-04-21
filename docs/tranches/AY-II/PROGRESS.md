# AY-II — Progress Log

Dated execution log for tranche AY-II (pass II of AY; see
`../AY-I/FINAL.md` for pass-I close and `audit/AUDIT-{A,B,C,D}-*.md`
for the triumvirate that informs this pass).

- `Status`: planned
- `Current wave`: not started
- `Next wave`: W0

---

## Scaffold landing

AY-II opens at the commit that lands the split + this scaffold.
The four audit artefacts at `audit/AUDIT-{A,B,C,D}-*.md` were
cherry-picked from their worktrees during the pass-I → pass-II
transition and placed under this pass's `audit/` directory per
the multi-pass-tranche edict
(`docs/instructions/tranche/SPEC.md` §Multi-pass tranche split).

The plan (`AY-II.md`), wave specs (`waves/W0.md` + `waves/W1.md`),
and this PROGRESS were authored without an execution dispatch.
Any sub-agent dispatched into AY-II waves operates on the scaffold
as-is; mid-wave plan edits follow the SPEC §Scope-reveal protocol.

W7's preempted worktree from AY-I — and the four audit worktrees —
are discarded as part of this scaffold commit. The W7 draft fix
(`prev < new_idx` guard in `TapeBuilder::note_push`) is explicitly
abandoned; the architectural consolidation in W0 supersedes.
