# Omega-A Coherence Audit - Pass Omega V8 REDRESS-212 W5B-FRONTENDR

Date: 2026-05-26.
Scope: REDRESS-212 / W5B-FRONTENDR V1 spec coherence across Pass Omega,
SK-V14 SPEC Sections 2/8B, MASTER-PLAN W5B rows, locks, architecture directive
canon, and the W5B-FRONTEND V2 challenge archive.
Disposition: ACCEPT-WITH-REQUIRED-WAVE-GRAPH-AMENDMENTS.

## Verdict

REDRESS-212 is coherent as a Pass Omega V8 input. It is not an architecture or
lock change; it is a wave-graph and cap-accounting correction inside V7's
W5B-FRONTEND ownership. V7 correctly placed generic frontend/import/IR closure
before W5C-GEN and W5D-DELETE, but the current SPEC and MASTER-PLAN still give
W5B-FRONTEND one <=90 minute / <=1.0k C-1 part-A wave. The required closure now
needs formal W5B.0 through W5B.4 sub-waves before W5C-GEN can unblock.

No current evidence requires a new public directive, BIR variant,
BackendShape, substrate surface, lock, or lock amendment. `restart/ARCHITECTURE.md`
and `restart/locks/LOCKS.md` should remain read/no-op for V8 unless Omega-C or
CHALLENGE finds new contradictory evidence.

## Cited Evidence

- Pass Omega defines Omega-A as the cross-document V1 coherence audit and
  requires locks references to cite lock file lines:
  `restart/prompts/pass-contracts/PASS-OMEGA.md:24-35`.
- Pass Omega CRUD authority keeps ARCHITECTURE, MASTER-PLAN, LOCKS,
  HANDOFF/MIGRATION, skinny corpus, and audit cleanup as separate surfaces:
  `restart/prompts/pass-contracts/PASS-OMEGA.md:57-74`.
- Pass Omega requires CHALLENGE convergence before CRUD and G-Omega before
  locks merges: `restart/prompts/pass-contracts/PASS-OMEGA.md:86-104`.
- SK-V14 SPEC Section 2 grants W5B-FRONTEND one wave cap and one LOC envelope:
  `restart/skinny/tranches/sk-v14/SPEC.md:243-246`.
- The same SPEC cap section says LOC, generated-output audit, and time caps are
  conjunctive; over-cap plans must split before dispatch or return REVISE:
  `restart/skinny/tranches/sk-v14/SPEC.md:254-260`.
- SPEC Section 8B assigns the full frontend closure to W5B-FRONTEND: owner
  paths at `restart/skinny/tranches/sk-v14/SPEC.md:712-720`, entry gates at
  `restart/skinny/tranches/sk-v14/SPEC.md:722-729`, tasks at
  `restart/skinny/tranches/sk-v14/SPEC.md:731-738`, exit gates at
  `restart/skinny/tranches/sk-v14/SPEC.md:740-751`, same-wave consumer at
  `restart/skinny/tranches/sk-v14/SPEC.md:753-756`, and pre-blocked routes at
  `restart/skinny/tranches/sk-v14/SPEC.md:758-762`.
- MASTER-PLAN mirrors the V7 one-shot W5B-FRONTEND graph and budget:
  `restart/MASTER-PLAN.md:788-819`.
- REDRESS-212 rejects the current shape as documentation-only, with no W5B
  frontend/codegen/xtask source redress retained:
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md:7-18`.
- REDRESS-212 records the failed V2 challenge, including 2/7 acceptance, CH4
  cap failure, CH6 maintain-gate conflict, and the need for formal sub-waves or
  a narrowed non-closing slice:
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md:20-35`.
- The V2 consolidated challenge is REVISE with five orphan REVISE lenses:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:1-19`.
- The consolidated structural finding says W5B's four internal sub-slices
  require a SPEC-level wave-graph amendment or must remain a non-closing slice:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:21-35`.
- Required V2 folds include formal sub-waves/aggregate cap, Lock14-only first
  checkpoint, all-template guards, full public-retirement tests, maintain
  resolution, LOC accounting, and same-commit closure:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:37-58`.
- CH4 pinpoints the cap mismatch: four 30-minute W5B slices plus final
  verification do not fit the single W5B wave cap:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH4.md:9-13`.
- CH6 pinpoints the maintain mismatch: the plan substituted exact no-diff for
  SPEC's +/-1.0% full-table gate without SPEC authority:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH6.md:9-16`.
- The corrective packet formalizes W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE,
  W5B.2 LAYOUT-DISCARD, W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4
  REQUEST-CONSUMER, with W5C-GEN blocked until aggregate W5B close:
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:79-97`.
- The corrective packet names required V8 folds and explicitly says LOCKS and
  ARCHITECTURE should remain read/no-op absent unexpected public syntax,
  substrate, BackendShape, or Lock 14 evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:107-130`.
- Lock 10 already gates any new directive, BackendShape, or BIR variant through
  G-Omega: `restart/locks/LOCKS.md:269-280`.
- Lock 14 already requires grammar-neutral generic crates, generated runtime
  output from grammar source plus workspace metadata, and same-wave leak
  census for generic-crate touches: `restart/locks/LOCKS.md:349-387`.
- Architecture's V1 directive canon already folds `@ws` into `@layout(ws = ...)`
  and rejects standalone `@ws`: `restart/ARCHITECTURE.md:1616-1631`.
- Local invariant checks at this audit: `grep -cE '^[0-9]+\\. \\*\\*'
  restart/locks/LOCKS.md` returns `16`; `find crates/core/src/runtime
  -mindepth 2 -type f -name '*.rs' | wc -l | tr -d ' '` returns `67`.

## Proposed No-Ops

- CRUD-1 ARCHITECTURE: read/no-op. Existing V1 architecture already has the
  directive canon needed for `@ws` retirement and no evidence adds a public
  syntax, substrate, BIR, or BackendShape surface.
- CRUD-3 LOCKS: read/no-op. Record zero delta in `restart/audit/totality/astral/V8/locks-diff.md`;
  verify the 16-lock count and five-shape BackendShape canon. REDRESS-212 does
  not amend Lock 10, Lock 14, or the lock count.

## V8 Risks

- If SPEC Section 2/8B is not amended, W5B-FRONTEND can only admit by violating
  the dispatch cap or by hiding unfinished frontend closure as a deferral.
- If W5B.0 is not a Lock14-only gate, frontend source edits can land before the
  owner-path and parent-diff routing needed to police W5B scope.
- If W5B.0 through W5B.3 are treated as W5B close, W5C-GEN can unblock before
  the request consumer and proof-carry gate exists.
- If the maintain gate is not clarified, CH6 will reopen the exact no-diff vs.
  +/-1.0% full-table conflict.
- If the CH1 exactness fold is omitted, W5B can still paper-close with prose
  fail-closed cells, wildcard log greps, or unaccounted redress docs.
- If `@ws` is described as public syntax rather than compatibility lowering into
  canonical layout facts, V8 conflicts with the V1 directive canon.

## Required CRUD Surfaces

- CRUD-1 ARCHITECTURE: read/no-op; verify no directive, substrate, BIR, or
  BackendShape delta.
- CRUD-2 MASTER-PLAN: update Section 13.3 W5B rows to mirror W5B.0 through
  W5B.4, keep W5C-GEN blocked until aggregate W5B-FRONTEND close, and preserve
  W5D/W6/W7/new-admit blocking. Carry the CH1 exactness fold: owner file/type,
  exact positive/fail-closed tests per construct, exact W5B.0 Lock 14 tests,
  per-test/per-log nonzero assertions, and redress/REDRESS LOC accounting.
- CRUD-3 LOCKS: read/no-op; retain zero-delta locks diff and verify 16 locks.
- CRUD-4 HANDOFF + MIGRATION: record REDRESS-212 / Pass Omega V8 state and next
  dispatch W5B.0 LOCK14-GATE.
- CRUD-5 SKINNY CORPUS: update `restart/skinny/tranches/sk-v14/SPEC.md`
  Sections 2 and 8B; update `restart/skinny/tranches/sk-v14/SYNTHESIS.md`,
  `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`,
  `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`, and
  `restart/skinny/tranches/sk-v14/HANDOFF.md`; align
  `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` only where they still
  present W5B-FRONTEND as one-shot active authority or omit CH1's exact W5B
  test/nonzero/LOC gates. `restart/skinny/{BENCH,SUBSTRATE}.md`
  are read/no-op.
- CRUD-6 AUDIT + CLEANUP: keep the V2 challenge archive and REDRESS-212 as
  cited authority. No deletion is required by Omega-A.
