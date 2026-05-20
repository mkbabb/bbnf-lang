# SK-V12 S-P2 CHALLENGE V1 — CH6 Anti-Paper-Close

Disposition: REVISE.

## Scope

Lens: anti-paper-close. I audited the six S-P2 research artifacts for
unsupported "researched/designed" claims, future-wave placeholders, uncited
comparator/ISA claims, broken citations, and candidate claims missing current
scalar-reference evidence. Read set: `restart/prompts/skinny/PASS-2-RESEARCH.md`
§3, all six `restart/skinny/tranches/sk-v12/research/p2/*.md` artifacts,
`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`,
`restart/skinny/tranches/sk-v12/HANDOFF.md`, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, and `restart/locks/LOCKS.md`.

## Findings

1. P2-A's candidate list is not yet CH6-grounded. PASS-2 says a primitive
   claim needs its scalar-reference sketch in §2 and that a candidate deferred
   to later detail is a paper-close
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:133`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:136`). P2-A lists seven
   candidate primitives in §2, but each entry carries shape, P1 antecedent,
   comparator antecedent, local delta, and admission boundary without a concrete
   scalar-reference status or scalar sketch
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:41`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:53`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:65`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:77`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:89`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:101`).
   Phrases such as "scalar reference first" and "strict scalar parity is
   mandatory" are admission rules, not scalar-reference evidence
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:39`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:63`).

2. P2-A contains non-orchestrator-citable local sources. Its source section
   cites `restart/prompts/ORCHESTRATOR.md:374` and `:474`, but the current
   `restart/prompts/ORCHESTRATOR.md` file ends before those lines
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:148`).
   It also cites SK-V12 replay and manifest files at tranche-root paths that do
   not exist; the live files are under `research/p1/`
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:157`,
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:158`).
   Finally, its historical SK-V11 P2 convergence source omits the `hardening/`
   directory that exists in the repository
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:162`).
   These are paper-close defects because the cited evidence cannot be followed
   by the orchestrator as written.

3. P2-D carries one future-evidence item in its candidate table. The table
   counts `retained_cursor_skip_projection` as a guarded candidate, but its own
   evidence says the arch surface is "None unless a later generated grammar
   proves a grammar-neutral cursor plan" and that it needs fresh retained-view
   hot-leaf evidence
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:77`).
   The prose then calls it "a future retained-view diagnostic"
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:83`).
   Under CH6, that item is not a current S-P2 candidate; it must be dropped
   from the candidate count/table or moved to risks as a non-candidate
   diagnostic.

4. The remaining artifacts do not present a CH6 paper-close requiring rejection.
   P2-B names scalar references, strict checkasm expectations, and same-wave
   consumer rules for each gate
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:29`,
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:35`).
   P2-C flags missing scalar oracles and smoke-level tests instead of treating
   them as admitted evidence
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:44`,
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:75`).
   P2-E supplies executable scalar sketches for the parse-that candidates
   (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:62`,
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:114`,
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:167`,
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:240`,
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:312`).
   P2-F states no new external comparator or ISA claims and confines ISA
   allowances to local Lock 16 rather than uncited external assertions
   (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:21`,
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:72`).

## Revise List

1. Revise P2-A §2 so every C1-C7 candidate either has a scalar-reference
   status plus scalar sketch/current executable reference, or is demoted out of
   the candidate list into support-only comparator evidence.

2. Repair P2-A's broken local citations: replace the out-of-range
   `ORCHESTRATOR.md:374` and `:474` references with live line references;
   replace tranche-root SK-V12 manifest/replay paths with
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
   and `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`; and
   replace the SK-V11 convergence path with
   `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.

3. Revise P2-D so `retained_cursor_skip_projection` is no longer counted as a
   current candidate. Keep it only as a risks/diagnostic note unless fresh
   retained-view hot-leaf evidence exists in the current S-P1 authority.

4. Re-run CH6 after those folds. No source, RESULTS, REDRESS, or sibling CH
   file change is required for this revision.
