# SK-V12 S-P3 CHALLENGE V1 - CH6 Anti-Paper-Close

Disposition: ACCEPT

## Lens

CH6 asks whether every wave closes on measurement rather than a future-phase
promise; whether each wave carries a revert protocol; whether same-wave
consumers are named; and whether the SPEC forbids deferral, "wired" closes, and
orphan kernels (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:140`-`:145`).

## Findings

No blocking CH6 findings.

1. The global close condition is measurement-bound, not prose-bound. SK-V12
   closes only after W0 locks the telemetry/report/gate surface, W1 admits one
   generated non-JSON baseline or records a measured REDRESS block, W2 admits a
   same-row measured intervention at `ceil(W1_baseline_track1_mbps * 1.01)` or
   records a measured reject, and every emitted report/field is consumed by a
   same-wave gate (`restart/skinny/tranches/sk-v12/SPEC.md:42`-`:64`). The SPEC
   also states the close target as a generated non-JSON baseline plus one
   measured intervention, and permits only a measured generated-baseline BLOCKED
   route as the honest alternative (`restart/skinny/tranches/sk-v12/SPEC.md:66`-`:69`).

2. The anti-paper-close non-negotiables are explicit. The packet forbids
   behavior source changes without W0, checkasm-only/report-only/telemetry-only
   performance admission, and any wave close on "wired", "integrated", "future
   consumer", or other future-phase promise
   (`restart/skinny/tranches/sk-v12/SPEC.md:223`-`:244`). The dispatch prompt
   carries the same rule: every primitive or generated path must include its
   same-wave consumer and measurement gate, and FAIL/BLOCKED commits require
   REDRESS evidence plus a rejected patch when a patch was attempted
   (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:82`-`:93`).

3. W1 distinguishes a generated baseline from report/witness fixtures. The W1
   exit gate requires exactly one selected non-JSON row with generated Track 1,
   independent oracle/Track 2 evidence, positive Mbps, strict equality, compiled
   generated runtime, and same-wave gate consumption
   (`restart/skinny/tranches/sk-v12/SPEC.md:399`-`:408`). Its pre-blocked routes
   reject REDRESS 111 report fixture as baseline, REDRESS 112/113 future-phase
   promise, hand-only parser, stale `sheets_witness`, and source-only baseline
   claims without measured Mbps (`restart/skinny/tranches/sk-v12/SPEC.md:413`-`:416`).
   P3-D reinforces that REDRESS 111 is only a report/gate lane and cannot move a
   generated baseline row (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:52`-`:63`).

4. W2 is bound to the W1 row and cannot close on a primitive-only speedup. Its
   entry gate requires W1's admitted row and `W1_baseline_track1_mbps`; its exit
   gate requires selected-row Track 1 Mbps >=
   `ceil(W1_baseline_track1_mbps * 1.01)`, independent strict-equal Track 2, and
   a same-wave generated consumer in the sampled/profiled path or focused proof
   (`restart/skinny/tranches/sk-v12/SPEC.md:440`-`:450`,
   `restart/skinny/tranches/sk-v12/SPEC.md:462`-`:474`). P3-C states the same
   same-grammar/workload/output-plane requirement and rejects telemetry-only
   primitive speed (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:171`-`:190`).

5. W3 defaults to block unless its entry gate passes. The SPEC gives W3 no
   default behavior owner paths, requires W1/W2 disposition plus fresh material
   evidence beyond REDRESS 114-119, and says a failed entry gate records a routed
   W3 block with no source edit (`restart/skinny/tranches/sk-v12/SPEC.md:491`-`:512`).
   The exit gate likewise allows a no-behavior close only by recording why no
   current S-P2 candidate passes material reopen, with no source/RESULTS row
   movement (`restart/skinny/tranches/sk-v12/SPEC.md:517`-`:527`).

6. W4 close cannot launder stale docs or unconsumed telemetry. W4 must reconcile
   every wave disposition, ensure RESULTS/REDRESS/SYNTHESIS/HANDOFF/SPEC/dispatch
   agreement, preserve or measured-demote guard rows, and reject accepted source
   changes lacking profile, row threshold, scalar/parity proof, Lock 14 proof,
   same-wave consumer, and REDRESS id
   (`restart/skinny/tranches/sk-v12/SPEC.md:551`-`:569`). P3-C closes the same
   hole by failing close on prose-only Lock 14 claims, stale witness modules,
   hand-only non-JSON parsers, producer-only telemetry, stale run ids, oracle
   coupling, parse-only SOTA claims, W3 substrate reopen, or unconsumed fields
   (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:241`-`:257`).

7. Revert protocols are present per wave. W0 reverts gate/report/RESULTS changes
   and records malformed gate evidence (`restart/skinny/tranches/sk-v12/SPEC.md:358`-`:360`);
   W1 reverts codegen/runtime/bench/report/gate/RESULTS/generated files and saves
   `/tmp/skv12-waveW1-rejected.patch` (`restart/skinny/tranches/sk-v12/SPEC.md:418`-`:421`);
   W2 reverts intervention, generated output, tests, gate/report, RESULTS, and
   REDRESS while preserving W1 evidence (`restart/skinny/tranches/sk-v12/SPEC.md:481`-`:483`);
   W3 defaults to no source edit and reverts the behavior slice only if behavior
   dispatches and fails (`restart/skinny/tranches/sk-v12/SPEC.md:535`-`:537`);
   W4 reopens the producing wave or marks close blocked with exact missing
   evidence (`restart/skinny/tranches/sk-v12/SPEC.md:577`-`:578`).

## Fold Revisions

None required for CH6. The V1 packet already satisfies the anti-paper-close
criteria: measurable gates are named for W0-W4, same-wave consumers are named,
generated baseline evidence is separated from report/witness fixtures, W2 is
same-row and thresholded, W3 is blocked by default, and close rejects stale or
unconsumed evidence.
