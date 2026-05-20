# SK-V12 S-P3 CH6: Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V1.
Lens: CH6 anti-paper-close and falsifiability.
Date: 2026-05-20.

## Verdict

ACCEPT.

Confidence: 96%.

## Scope

Reviewed the PIN-V1 S-P3 packet:

- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md`

## Blocking Findings

None.

## File/Line Findings

1. Generated baseline existence cannot close.
   - `SPEC.md:35` states SK-V12 closes only by ADMIT or FIXPOINT.
   - `SPEC.md:39-49` requires a generated CSS row plus measured Track 1,
     independent oracle/Track 2, lightningcss equality, provenance, and gate
     consumption.
   - `SPEC.md:84-86` explicitly says old close formulas are not CSS close bars
     and that a measurable CSS baseline below lightningcss is evidence, not
     admission.
   - `SPEC.md:380-388` makes W1b's strict-equal below-lightningcss result
     `PASS-MEASURED-BASELINE`, not close.
   - `DISPATCH-PROMPT.md:149-156` carries the same distinction for
     implementation agents.

2. The stale `baseline * 1.01` path is rejected, not retained.
   - `SPEC.md:84-86`, `SPEC.md:216-217`, and `SPEC.md:553-554` block
     `ceil(baseline_mbps * 1.01)` and baseline-existence close.
   - `p3d-telemetry-schema.md:116-128` makes
     `track1_mbps > lightningcss_mbps + 1` the admission rule and marks the old
     formula obsolete.
   - `p3c-falsifiability-gates.md:412-418` lists baseline-relative close,
     missing lightningcss, missing oracle, and prose-only Lock 14 as close FAIL.

3. Lock 14 is executable, not prose-only.
   - `SPEC.md:50-52` requires the seven generic JSON leaks to resolve through
     `GrammarConfig` or equivalent before CSS L4 emission is legal.
   - `SPEC.md:312-344` assigns W1a to the legality gate and requires a generic
     scan, JSON parity/guards, and no directive/BIR/BackendShape/public
     substrate expansion.
   - `p3d-telemetry-schema.md:143-162` defines consumed Lock 14 fields,
     including scan artifacts and leak-resolution evidence.
   - `p3e-preblocked-ledger.md:125-130` blocks hand-only witnesses and prose
     Lock 14 claims as grammar-generalization proof.

4. SIMD/ASM cannot admit on checkasm alone.
   - `SPEC.md:53-56` requires scalar reference, checkasm/parity, same-host
     micro-proof, same-wave consumer, corpus parity where applicable, and the
     `escape_mask_64` fix before SIMD/ASM admission.
   - `SPEC.md:224-228` repeats scalar/reference, parity/checkasm, micro-proof,
     same-wave consumer, and escape-mask prerequisites as non-negotiables.
   - `SPEC.md:493-506` makes W4 add/refresh scalar reference and strict parity,
     wire a same-wave consumer, measure CSS/JSON guards, and reject if the
     measured candidate misses.
   - `p3d-telemetry-schema.md:164-183` requires Lock 16 telemetry fields beyond
     checkasm, including microbench, consumer, feature guard, escape-mask status,
     and ASM visibility where relevant.
   - `DISPATCH-PROMPT.md:191` and `DISPATCH-PROMPT.md:200-208` block orphan or
     checkasm-only performance admission.

5. CSS admission requires lightningcss plus independent oracle strict equality.
   - `SPEC.md:40-49` requires Track 1 strictly greater than
     `lightningcss_mbps + 1`, same corpus/output plane/host, strict equality,
     independent oracle/Track 2, lightningcss command/artifact, and consumed
     provenance.
   - `SPEC.md:363-388` requires the W1b plan and exit gate to name and consume
     the CSS corpus, fact stream, independent oracle/Track 2, lightningcss
     comparator, equality command, benchmark command, and gate command.
   - `p3d-telemetry-schema.md:97-142` defines the CSS L4 companion schema and
     fails missing comparator, equality, oracle independence, checksums, run id,
     or finite Mbps evidence.
   - `DISPATCH-PROMPT.md:162-174` requires the same fields and rejects missing
     lightningcss evidence, missing independent oracle, stale run ids, and
     producer-only telemetry.

6. FIXPOINT requires measured CSS redress, new union attempt, new ASM-gen
   attempt, and zero production orphans.
   - `SPEC.md:65-82` requires a measured CSS L4 redress attempt, measured
     uncloseability, new REDRESS-recorded union attempt, new REDRESS-recorded
     ASM-gen attempt, zero production orphans, and routed remainder.
   - `SPEC.md:454-459` permits W3 to count for FIXPOINT only when measured,
     materially differentiated, and recorded in REDRESS.
   - `SPEC.md:501-507` permits W4 measured reject only with scalar, checkasm,
     microbench, same-wave consumer, and REDRESS evidence complete.
   - `p3c-falsifiability-gates.md:396-410` restates the FIXPOINT checklist and
     includes CSS redress, W3 union, W4 ASM-gen, zero orphans, guards, and close
     docs.
   - `p3b-wave-sequencing.md:188-191` binds W5 FIXPOINT to measured CSS L4,
     measured new union, measured new ASM-gen, zero orphans, and REDRESS
     evidence.

7. W5 does not close on a future-tranche promise.
   - `SPEC.md:537-543` distinguishes PASS-ADMIT, PASS-FIXPOINT, and ROUTE; ROUTE
     opens SK-V13 only when ADMIT/FIXPOINT does not hold, and the campaign does
     not stop unless the user-pin close clause is met or a measured fixpoint is
     recorded.
   - `SPEC.md:579-581` repeats that convergence requires Section 0.1 ADMIT or
     FIXPOINT; otherwise W5 routes exact remainder into Pass Alpha for SK-V13.
   - `DISPATCH-PROMPT.md:226-230` uses the same shape: convergence requires wave
     dispositions plus SPEC Section 0 ADMIT or FIXPOINT; if neither holds, W5
     routes SK-V13 and campaign continues.
   - `p3b-wave-sequencing.md:61-66` explicitly says W5 synthesizes routed
     remainder rather than paper-closing when neither close clause is satisfied.

## Required Folds

None.

Optional cleanup: `p3b-wave-sequencing.md:166-168` says illegal retained-vector
routes "falsify W4"; the local context is W3. This is not an anti-paper-close
gap because the route is still fail-closed, but the next fold may correct the
wave label for clarity.
