# SK-V12 S-P3 PIN-V2 CH6 Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V2.
Lens: CH6 anti-paper-close.
Reviewed commit: `7316d87b`.
Date: 2026-05-20.

## Disposition

PASS.

Confidence: 97%.

CH6 finds no paper-close defect in the PIN-V2 packet. The packet does not allow
CSS close without strict `track1_mbps > lightningcss_mbps + 1`, independent
oracle evidence, strict equality, and same-wave gate consumption. FIXPOINT is
also measured: it requires CSS redress, a new union attempt, a new ASM-gen
attempt, zero production aarch64 orphans, and REDRESS evidence.

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
- P2 hardening and the Lock 14 / Lock 16 audits where needed.

## Findings

1. CSS cannot close on baseline existence or a stale threshold.
   - `SPEC.md:36-45` restricts close to ADMIT/FIXPOINT and requires a generated
     CSS row, strict `track1_mbps > lightningcss_mbps + 1`, same corpus/output
     plane/host, strict equality, independent oracle/Track 2, and lightningcss
     equality.
   - `SPEC.md:85-87` explicitly rejects the old `>= 1 Mbps` and
     `ceil(baseline_mbps * 1.01)` close formulas.
   - `p3c-falsifiability-gates.md:241-256` makes equality at
     `lightningcss_mbps + 1` a FAIL and marks the old baseline formula stale.
   - `p3d-telemetry-schema.md:116-128` binds admission to
     `track1_mbps > lightningcss_mbps + 1` and marks baseline-relative admission
     obsolete.

2. Strict equality, independent oracle, and gate consumption are load-bearing.
   - `SPEC.md:120-172` lists the required telemetry and rejects missing
     lightningcss evidence, missing independent oracle, stale run ids,
     producer-only telemetry, generic policy leaks, parse-only admission, and
     orphan SIMD primitives.
   - `SPEC.md:433-489` splits CSS into W1b-1 oracle scaffold and W1b-2
     lightningcss admission gate; W1b-2 must consume comparator, oracle,
     generated-size, equality, and JSON guard fields.
   - `p3b-wave-sequencing.md:144-159` requires generated Track 1 source, oracle
     path/Mbps, strict equality, provenance, lightningcss command/artifact/Mbps,
     and gate consumption before ADMIT eligibility.
   - `p3d-telemetry-schema.md:259-295` fails missing comparator/equality/oracle
     evidence and spells the ADMIT predicates as exact telemetry checks.

3. Lock 14 and Lock 16 are executable, not prose promises.
   - `SPEC.md:51-58` requires Lock 14 cleanup through `GrammarConfig` or
     equivalent generated metadata and requires Lock 16 scalar reference,
     checkasm/parity, same-host micro-proof, same-wave consumer, corpus parity
     where applicable, and `escape_mask_64` resolution.
   - `SPEC.md:261-275` requires generic-crate scans, generated policy ownership,
     benchmark/equality exercise, and generated-size/O(N) tracking.
   - `p3c-falsifiability-gates.md:161-163` blocks new SIMD rows while W2 is open,
     and `p3c-falsifiability-gates.md:352-402` requires scalar/checkasm,
     microbench, same-wave consumer, feature fallback, and zero orphan state.
   - `p3d-telemetry-schema.md:143-183` defines consumed Lock 14 and Lock 16 fields
     rather than allowing unconsumed assertions.

4. Zero production aarch64 orphans are required at close.
   - `USER-PIN-W1-CSS-L4-SOTA.md:71-78` names the five carried orphans and makes
     zero orphan kernels the SK-V12 close target.
   - `SPEC.md:58-64` and `SPEC.md:80-83` require zero orphans for both ADMIT and
     FIXPOINT.
   - `p3d-telemetry-schema.md:239-255` requires per-orphan consumed/removed/
     inventory-demoted/open status and makes `open` fail ADMIT and FIXPOINT.
   - `skv12-aarch64-simd-coverage-audit.md:34-61` provides the five-orphan basis:
     `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
     `byte_context`, and `cache_hints`.

5. FIXPOINT is measured and cannot skip union or ASM-gen evidence.
   - `SPEC.md:66-83` requires measured CSS redress, measured uncloseability, a new
     REDRESS-recorded union attempt, a new REDRESS-recorded ASM-gen attempt, zero
     orphans, and routed remainder.
   - `SPEC.md:526-536` allows W3 FIXPOINT credit only for a measured or
     microbench-rejected material-differential union attempt recorded in REDRESS.
   - `SPEC.md:588-597` allows W4 reject only with scalar, checkasm, microbench,
     same-wave consumer, and complete REDRESS evidence.
   - `p3c-falsifiability-gates.md:424-438` restates the FIXPOINT close checklist
     with CSS redress, W3 union evidence, W4 ASM-gen evidence, zero orphans, JSON
     guard state, REDRESS misses, and close-doc agreement.
   - `p3d-telemetry-schema.md:220-237` requires historical REDRESS citations,
     material differential, fresh profile/microbench, parity/equality, consumer
     path, substrate cardinality, public API status, and attempt status.

6. Future-phase promises do not close the tranche.
   - `SPEC.md:234-235` makes future-consumer and future-phase promise close
     invalid.
   - `SPEC.md:629-635` gives W5 only three exits: PASS-ADMIT, PASS-FIXPOINT, or
     ROUTE; ROUTE does not stop the campaign.
   - `SPEC.md:671-674` requires wave dispositions plus ADMIT/FIXPOINT; otherwise
     W5 routes exact remainder into Pass Alpha for SK-V13.
   - `p3b-wave-sequencing.md:64-69` says W5 routes remainder rather than
     paper-closing when neither close clause is satisfied.
   - `p3c-falsifiability-gates.md:440-450` lists future-phase promise instead of
     measurement as close FAIL.

7. Current results do not mask a CSS admission.
   - `skinny/RESULTS.md:143-145` records overall `N-direct / NoGo` and the JSON
     Track 1/Track 2 basis; it is not a CSS L4 admission row.
   - `skinny/REDRESS.md:10-16` keeps `skinny/RESULTS.md` as the measured authority
     and records current overall `N-direct / NoGo`.
   - `HARDENING-S-P2-CONVERGED.md:38-63` carries the same S-P3 facts: CSS first,
     lightningcss `+1` strict bar, `parse_only` diagnostic-only, conditional
     union/ASM categories, `escape_mask_64` prerequisite, and zero-orphan close.

## Required Fixes

None.

## CH6 Result

PASS.
