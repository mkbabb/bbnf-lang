# SK-V8 W3 Hardening V1 CH6 - Anti-Paper-Close

Verdict: ACCEPT.

Confidence: 96%.

Target reviewed: `fc91c217`
(`docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`).

## Findings

1. W3 can proceed to REDRESS only as a rejection/routing disposition with no
   source patch. SPEC Section 6 requires a fresh W3 plan to name exact owner
   files, row gates, same-wave consumer, measured-path proof, Lock 1 fork,
   scalar/checkasm requirements, pre-block differences, and a fit estimate; if
   that estimate exceeds the W3 LOC budget or the 90-minute cap, W3 must split
   before dispatch or return REVISE. The reviewed plan does that: it identifies
   the scanner/tape event-model mismatch, names the broad source/generated/gate
   owner set that would be needed for a real Tier A implementation, and rejects
   implementation before authorizing source edits.

2. The rejection is evidence-backed, not a paper close. The W3 research records
   the concrete mismatch: scanner positions are structural punctuation plus real
   quotes, while retained tape offsets are parser events such as container
   opens/closes, opening quotes, number starts, and literal starts. Its example
   for `{"a":[1,true]}` shows retained tape offsets `[0, 1, 5, 6, 8, 12, 13]`
   versus scanner structural positions `[0, 1, 3, 4, 5, 7, 12, 13]`. That
   mismatch falsifies a bounded storage-only swap and supports routing the
   event-grammar/`ValueRef` redesign to SK-V9/Pass Omega.

3. Same-wave consumer discipline is preserved. SPEC Section 6 requires generated
   JSON retained Track 1 parsing, plus retained view/`ValueRef` proof as the W3
   consumer, and explicitly rejects telemetry-only rows. The plan refuses
   `tape_vs_tape`, `simd_structural_scan`, Track 2, comparator rows, and
   retained-view-only checks as production consumers. Because no valid generated
   retained-parser consumer can be named inside W3's budget, no source patch is
   authorized.

4. The missing folds are disposition folds, not implementation folds. Current
   `HANDOFF.md` still says W3 is active/next, while `skinny/REDRESS.md` ends at
   W2 item 91 and has no W3 entry. That is acceptable before redress, but W3
   must not be treated as closed or W4-unblocking until REDRESS records the W3
   rejection/routing and HANDOFF agrees.

5. No `skinny/RESULTS.md` content fold is required for this disposition. Since
   W3 rejects before implementation and before measurement, the correct RESULTS
   posture is zero diff. The redress text should state that no W3 source patch,
   generated output, benchmark row, or row-table admission landed.

## Verification

- `git rev-parse --short HEAD`: `fc91c217`.
- `git log --oneline --decorate --max-count=12`: confirmed HEAD is
  `docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`, after W3
  research and W2 hardening close commits.
- `git status --short --untracked-files=all` before writing this file showed
  only other hardening lane artifacts under
  `restart/skinny/tranches/sk-v8/research/wave-3-hardening/V1/`; I did not edit
  them.
- Reviewed SPEC Section 6, DISPATCH-PROMPT W3, HANDOFF W3/exit language, W3
  research, and W3 plan.
- `env CARGO_TARGET_DIR=/tmp/skv8-w3-ch6-target cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`
  from `skinny/`: PASS.
- `env CARGO_TARGET_DIR=/tmp/skv8-w3-ch6-target cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`
  from `skinny/`: PASS.
- `git diff --exit-code HEAD -- skinny/RESULTS.md`: PASS.
- `git diff --check`: PASS before this owned file was added.

## Required Folds

1. Add a `skinny/REDRESS.md` entry for SK-V8 W3 rejecting/routing Tier A for
   this wave. It must name the scanner/tape event-model mismatch, the failed W3
   fit gate, target rows `twitter/parse_only` and `apache_builds/parse_only`,
   guard rows `canada/parse_only`, `mesh/parse_only`, `numbers/parse_only`, and
   `marine_ik/parse_only`, and state that no source patch or rejected patch
   artifact exists because W3 stopped at pre-redress fit.

2. Update `restart/skinny/tranches/sk-v8/HANDOFF.md` so W3 is
   rejected/routed, W4 is unblocked only under that disposition, and the next
   move is W4 or the orchestrator-selected follow-up.

3. Preserve `skinny/RESULTS.md` unchanged. The redress/handoff fold should say
   RESULTS has zero W3 diff and no W3 row-table admission.

4. Route the split precursor to SK-V9/Pass Omega: define the retained
   class/event grammar including numbers/literals and string quote ownership,
   prove the retained `ValueRef` cursor contract over that grammar, and only
   then reconsider a measured structural-heavy parse row wave.

5. After CH1-CH6 complete, add the V1 consolidation before treating this
   challenge cycle as accepted. CH6 acceptance alone does not close W3.
