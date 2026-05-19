# SK-V9 Wave W3 CHALLENGE V4: Class-Lane-Only No-Redress

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 6;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research-v3.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-plan-v3.md`;
`skinny/REDRESS.md` Items 96 and 97.

Disposition: **REJECT for source redress**. The V3 plan correctly refuses to
dispatch the only remaining mechanically distinct route under the current W3
gate. The SK-V9 implementation track is therefore blocked at W3 until the
orchestrator amends, splits, resequences, or retires the current W3 gate.

## CH1 Correctness

Verdict: PASS on the plan's no-redress conclusion.

The class-lane-only candidate has a coherent local correctness proof: class
bytes can be written at the same parser branches that already validated each
retained offset, and `ValueRef` can remain layout-neutral while
`JsonNodeKind::at_cursor` reads `Tape::class_at`. That proof is not enough for
W3 correctness because current Section 6 binds the class lane to a same-wave
structural producer and deletion of structural rediscovery. Source redress
would be correct only for a narrower future class-lane proof wave.

## CH2 Generality / Lock 14

Verdict: REJECT as current-W3 source work.

The class-lane-only route would require Track 2, parity, event grammar witness,
and Lock 14 authorization changes. Those changes can be made generally, but
they would not satisfy the current W3 "same-wave producer" contract. Accepting
them as W3 would weaken Lock 14 by relabeling a parser-produced class stream as
the scanner-consumed union substrate.

## CH3 Regression / REDRESS

Verdict: REJECT as a redress candidate.

REDRESS 96 measured the full-position-vector producer and REDRESS 97 measured
the streaming-cursor producer. Both failed every W3 must-improve row and every
W10b maintain row. The class-lane-only route avoids those exact costs, but it
does so by removing the parse-only producer from the intervention. The likely
numeric result is neutral or negative on `track1_generated` because the parse
benchmark does not traverse retained views. That is not a credible route to
the current floors.

## CH4 Cost

Verdict: PASS on the decision not to implement.

A class-lane-only source patch is likely 250-330 hand/template/test LOC plus
generated output, before Lock 14 and handoff updates. Spending a redress cycle
on that patch would produce a predictable non-admission under the current gate.
The cheaper and more accurate action is to stop before source edits.

## CH5 Hidden Coupling

Verdict: REJECT as current-W3 dispatch.

The route hides three couplings that would need their own plan:

- `JsonEventGrammar::STRUCTURAL_CLASS_COUNT` currently models a seven-byte
  structural alphabet, not the nine retained JSON event classes required for a
  source-free `at_cursor`.
- Track 2 would need independent class writes, which changes the oracle proof
  posture.
- `gate-json` does not encode W3 numeric floors or profiler self-time, so
  redress would need explicit out-of-band gate enforcement.

These are acceptable to surface in a future SPEC amendment. They are not safe
to bury inside a current-W3 source attempt.

## CH6 Anti-Paper-Close

Verdict: REJECT.

A source-free `JsonNodeKind::at_cursor` would look like progress but would not
move the measured parse-only rows or remove the structural producer gap that
defines W3. Calling it W3 would be a paper-close: the same-wave consumer would
exist, but the same-wave producer and row gate would not.

## Disposition

The V3 plan is accepted only as a blocker statement and rejected as source
redress authority. No source files may be edited under W3 V3.

Escalation:

```text
BLOCKED: W3 current G-W3-UNION-SUBSTRATE has no remaining admissible
implementation route after REDRESS 96 and REDRESS 97. The orchestrator/user
must amend SPEC, split W3 into a class-lane proof plus parse-only producer,
resequence SK-V9 around another shortlist route, or retire the current W3 gate.
```
