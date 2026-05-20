# SK-V12 S-P3 PIN-V3 CH6 Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V3.
Lens: CH6 anti-paper-close.
Reviewed commit: `4c53119f`.
Date: 2026-05-20.

## Disposition

PASS.

Confidence: 97%.

CH6 finds no paper-close defect in the PIN-V3 packet. The packet does not allow
CSS close without generated CSS L4 Track 1 strictly greater than
`lightningcss_mbps + 1`, strict equality, an independent oracle/Track 2,
same-wave gate consumption, Lock 14/16 as applicable, and zero production
aarch64 orphans. FIXPOINT is also measured: it requires CSS redress, measured
uncloseability, a new union attempt, a new ASM-gen attempt, zero orphans, and
REDRESS evidence.

## Scope

Reviewed:

- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- P2 hardening and convergence docs as needed.

## Findings

1. CSS cannot close on generated-baseline existence, `>= 1 Mbps`, or stale
   baseline-relative formulas.
   - `SPEC.md:36-45` restricts close to ADMIT/FIXPOINT and requires a generated
     CSS L4 row, strict `track1_mbps > lightningcss_mbps + 1`, same corpus,
     same output plane, same host, strict equality, independent oracle/Track 2,
     and lightningcss equality.
   - `SPEC.md:85-87` explicitly rejects `>= 1 Mbps` and
     `ceil(baseline_mbps * 1.01)` as CSS close bars.
   - `DISPATCH-PROMPT.md:48-53` restates the same strict lightningcss `+1` bar
     and says the stale floors are not close bars.
   - `p3c-falsifiability-gates.md:241-256` makes equality at
     `lightningcss_mbps + 1` a FAIL and marks `ceil(baseline_mbps * 1.01)` stale.
   - `p3d-telemetry-schema.md:116-128` binds CSS admission to
     `track1_mbps > lightningcss_mbps + 1` and makes the admission floor
     `lightningcss_mbps + 1`.

2. Strict equality, independent oracle, lightningcss evidence, and same-wave
   gate consumption are load-bearing.
   - `SPEC.md:120-172` lists the CSS/non-JSON telemetry fields and rejects
     missing lightningcss evidence, missing independent oracle, stale run ids,
     producer-only telemetry, unsupported outcomes, generic policy leaks,
     parse-only admission, and orphan SIMD primitives.
   - `SPEC.md:433-489` splits W1b-1 scaffold from W1b-2 admission; W1b-2 must
     run three-way equality, same-host throughput, and consume generated-size,
     comparator, oracle, CSS, and JSON guard fields.
   - `p3b-wave-sequencing.md:144-159` requires generated Track 1 source,
     oracle path/Mbps, strict equality, provenance, lightningcss
     command/artifact/Mbps, and gate consumption before ADMIT eligibility.
   - `p3d-telemetry-schema.md:259-295` fails missing comparator, equality, or
     oracle evidence and states the exact ADMIT predicate.

3. Lock 14 and Lock 16 remain executable gates, not prose promises.
   - `SPEC.md:51-58` requires Lock 14 cleanup through `GrammarConfig` or
     equivalent generated metadata and requires Lock 16 scalar reference,
     checkasm/parity, same-host micro-proof, same-wave consumer, corpus parity
     where applicable, and `escape_mask_64` resolution.
   - `p3c-falsifiability-gates.md:171-190` requires generated metadata,
     negative generic-crate scans, JSON guard parity, and no CSS row admission in
     W1a.
   - `p3c-falsifiability-gates.md:278-290` requires the `escape_mask_64`
     falsifier, scalar/NEON parity, checkasm artifact, and forbids throughput
     credit from W2 alone.
   - `p3d-telemetry-schema.md:143-183` defines consumed Lock 14 and Lock 16
     fields instead of allowing unconsumed assertions.

4. Zero production aarch64 orphans are required at close.
   - `USER-PIN-W1-CSS-L4-SOTA.md:71-78` names the five carried orphans and sets
     zero orphan kernels as the SK-V12 close target.
   - `SPEC.md:58-64` and `SPEC.md:80-83` require zero orphans for ADMIT and
     FIXPOINT.
   - `p3c-falsifiability-gates.md:384-402` requires every carried orphan to be
     consumed, removed, or inventory-demoted, and says production orphans at
     close invalidate FIXPOINT.
   - `p3d-telemetry-schema.md:239-255` requires per-orphan status and makes
     `open` fail ADMIT and FIXPOINT close.

5. FIXPOINT cannot skip measured union and ASM-gen attempts.
   - `SPEC.md:66-83` requires measured CSS redress, measured uncloseability, a
     new REDRESS-recorded union attempt, a new REDRESS-recorded ASM-gen attempt,
     zero orphans, and routed remainder.
   - `SPEC.md:528-538` gives W3 FIXPOINT credit only for a measured or
     microbench-rejected material-differential union attempt recorded in
     REDRESS.
   - `SPEC.md:590-599` lets W4 pass or reject only with scalar/checkasm,
     microbench, same-wave consumer, equality, and complete REDRESS evidence.
   - `p3c-falsifiability-gates.md:424-438` restates the FIXPOINT checklist with
     measured CSS redress, W3 union evidence, W4 ASM-gen evidence, zero orphans,
     JSON guard state, REDRESS misses, and close-doc agreement.
   - `p3d-telemetry-schema.md:220-237` requires REDRESS citations, material
     differential, fresh profile/microbench, parity/equality, same-wave consumer,
     substrate cardinality, public API status, and attempt status.

6. Future-phase promises and fallback shortcuts do not close SK-V12.
   - `SPEC.md:234-235` invalidates "wired", "integrated", "future consumer", and
     other future-phase promise closes.
   - `SPEC.md:438-442` states W1b-1 scaffold failure does not satisfy the
     post-CSS-redress fallback condition; Sheets/BBNF fallback remains blocked
     until W1b-2 records measured CSS comparator/admission redress.
   - `p3b-wave-sequencing.md:104-110` keeps Sheets and BBNF-self out of W1b-1
     and W1b-2 and requires measured W1b-2 CSS redress before any later fallback
     wave.
   - `p3c-falsifiability-gates.md:258-264` allows Sheets/BBNF-self only after
     CSS L4 redress is recorded.
   - `p3c-falsifiability-gates.md:440-450` lists future-phase promise instead of
     measurement as close FAIL.

7. Current seed evidence does not mask a CSS admission.
   - `skinny/RESULTS.md:143` records overall `N-direct / NoGo`, not CSS L4
     admission.
   - `skinny/REDRESS.md:3539-3546` records the final SK-V11 JSON state and says
     the non-JSON generated-intervention axis remains blocked because SK-V11 did
     not stand up a generated non-JSON baseline.
   - `HARDENING-S-P2-CONVERGED.md:38-63` carries the same S-P3 facts: CSS first,
     strict lightningcss `+1` bar, parse-only diagnostic-only, conditional
     union/ASM categories, `escape_mask_64` prerequisite, and zero-orphan close.

## Required Fixes

None.

## CH6 Result

PASS.
