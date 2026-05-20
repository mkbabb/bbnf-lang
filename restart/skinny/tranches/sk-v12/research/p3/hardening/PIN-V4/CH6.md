# SK-V12 S-P3 PIN-V4 CH6 Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V4.
Lens: CH6 anti-paper-close.
Reviewed commit: `471bf53e`.
Date: 2026-05-20.

## Disposition

PASS.

Confidence: 98%.

CH6 finds no paper-close regression in the PIN-V4 confirmation packet. The
packet preserves the PIN-V3 load-bearing corrections and does not allow CSS
close without generated CSS L4 Track 1 strictly greater than
`lightningcss_mbps + 1`, strict equality, independent oracle/Track 2 evidence,
same-wave gate consumption, Lock 14/16 gates where applicable, and zero
production aarch64 orphans. FIXPOINT also remains measured: CSS redress,
measured uncloseability, one new union-substrate attempt, one new ASM-gen
attempt, zero orphans, and REDRESS evidence are all required.

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
- S-P2 hardening convergence and PIN-V3 consolidated records.

## Findings

1. CSS close cannot occur on generated-baseline existence, `>= 1 Mbps`,
   baseline-relative lift, or equality at the threshold.
   - `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:29-37`
     rescinds `ceil(baseline_mbps * 1.01)` and sets the CSS admission floor to
     `lightningcss_mbps + 1`.
   - `restart/skinny/tranches/sk-v12/SPEC.md:36-45` limits close to ADMIT or
     FIXPOINT and requires generated CSS L4 Track 1, strict
     `track1_mbps > lightningcss_mbps + 1`, same corpus, same output plane,
     same host, strict equality, independent oracle/Track 2, and lightningcss
     fact-stream agreement.
   - `restart/skinny/tranches/sk-v12/SPEC.md:85-87` rejects `>= 1 Mbps` and
     `ceil(baseline_mbps * 1.01)` as CSS close bars.
   - `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:241-255`
     requires same-plane lightningcss evidence, strict three-way equality, and
     fails equality at `lightningcss_mbps + 1`.
   - `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:317-328`
     repeats that CSS ADMIT and intervention ADMIT both use the lightningcss
     strict `>` bar, not the old baseline formula.

2. Strict equality, independent oracle, same-run lightningcss, and gate
   consumption are load-bearing and fail closed when missing.
   - `restart/skinny/tranches/sk-v12/SPEC.md:120-172` lists required CSS/non-JSON
     fields and rejects missing lightningcss evidence, missing independent
     oracle, stale run id, producer-only telemetry, unsupported outcome, generic
     policy leak, parse-only admission, or orphan SIMD primitive.
   - `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:17-38`
     states that generated non-JSON placeholders and prose claims are
     insufficient and that every emitted field must be consumed by the same-wave
     gate.
   - `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:284-299`
     spells the ADMIT predicate as `grammar_id == css_l4`, strictness/equality
     pass, independent Track 2, `track1_mbps > lightningcss_mbps + 1`, Lock 14
     pass, valid JSON guard state, and no open production orphan at close.
   - `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:171-183` requires
     CSS/non-JSON gates to consume the full telemetry set and rejects
     producer-only fields, stale run ids, missing lightningcss evidence, missing
     independent oracle, unsupported outcomes, generic policy leaks,
     parse-only admission, and orphan SIMD primitives.

3. W1b-1 scaffold failure does not unlock fallback.
   - `restart/skinny/tranches/sk-v12/research/p3/hardening/PIN-V3/CONSOLIDATED.md:31-34`
     records the accepted PIN-V3 correction: W1b-1 scaffold failure records
     REDRESS and returns to plan, but fallback remains blocked until W1b-2
     records measured CSS lightningcss comparator/admission redress.
   - `restart/skinny/tranches/sk-v12/SPEC.md:438-442` preserves that exact rule:
     W1b-1 BLOCKED/FAIL does not satisfy the post-CSS-redress fallback condition.
   - `restart/skinny/tranches/sk-v12/SPEC.md:488-491` allows Sheets/BBNF fallback
     only after W1b-2 comparator/equality/oracle/throughput/gate redress is
     measured and a subsequent plan revision authorizes it.
   - `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:75-76`
     says W1b-1 cannot record CSS ADMIT and W1b-2 is the first point where
     measured CSS BLOCKED/REJECTED evidence can later route fallback.

4. Lock 14 and Lock 16 remain executable prerequisites, not prose promises.
   - `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97-106` carries
     Lock 14 grammar-neutrality, Lock 16 admission, the seven Lock-14 leaks, and
     the `escape_mask_64` falsifier prerequisite.
   - `restart/skinny/tranches/sk-v12/SPEC.md:51-58` requires Lock 14 cleanup
     through `GrammarConfig` or equivalent generated metadata before CSS L4
     emission and requires Lock 16 scalar reference, checkasm/parity,
     same-host micro-proof, same-wave consumer, corpus parity where applicable,
     and `escape_mask_64` resolution before SIMD/ASM admission.
   - `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:20-31`
     makes W1a the legality wave and W2 the correctness prerequisite, explicitly
     saying W2 is not a throughput admission wave.
   - `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:278-296`
     requires the xorshift falsifier, scalar/NEON agreement, checkasm artifact,
     and forbids waiving the bug with throughput evidence.

5. FIXPOINT requires measured union plus ASM-gen evidence and cannot close on
   future-phase claims.
   - `restart/skinny/tranches/sk-v12/SPEC.md:66-83` requires measured CSS
     redress, measured ADMIT uncloseability, a new REDRESS-recorded union
     attempt, a new REDRESS-recorded ASM-gen attempt, zero orphans, and routed
     remainder.
   - `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:339-346`
     gives W3 FIXPOINT credit only for implemented-and-measured or
     microbench-rejected material-differential union evidence recorded in
     REDRESS; a plan-time statement is not evidence.
   - `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:397-402`
     gives W4 FIXPOINT credit only for measured ASM-gen reject evidence with
     scalar/checkasm/microbench/equality evidence and says production orphans
     invalidate FIXPOINT.
   - `restart/skinny/tranches/sk-v12/SPEC.md:234-235` blocks close on "wired",
     "integrated", "future consumer", or other future-phase promises.
   - `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:440-450`
     lists future-phase promise, missing oracle, missing lightningcss comparator,
     unresolved Lock 14/Lock 16, production orphans, and skipped union/ASM-gen
     categories as close failures.

6. The zero-orphan close condition is explicit and gate-consumed.
   - `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-78` names the
     five carried aarch64 orphans and makes zero orphan kernels the SK-V12 close
     target.
   - `restart/skinny/tranches/sk-v12/SPEC.md:58-64` and
     `restart/skinny/tranches/sk-v12/SPEC.md:80-83` require zero production
     aarch64 orphans for ADMIT and FIXPOINT.
   - `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:239-255`
     requires per-orphan status and makes `open` fail ADMIT and FIXPOINT close.
   - `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:384-402`
     requires each orphan to be consumed, removed, or inventory-demoted with
     evidence and makes production orphans a FIXPOINT invalidator.

7. Current seed evidence does not hide a CSS admission or fallback close.
   - `skinny/RESULTS.md:143-146` records overall `N-direct / NoGo`, names JSON
     Track 1/Track 2, and says `gate-json` consumes the current manifest; it is
     not a CSS L4 admission row.
   - `skinny/REDRESS.md:3282-3309` admits only a non-JSON gate/report schema lane
     that did not create generated non-JSON baseline authority or move a parser
     row.
   - `skinny/REDRESS.md:3311-3338` records generated non-JSON baseline rejection:
     generated CSS L4 Track 1 was absent and no CSS runtime/RESULTS row moved.
   - `skinny/REDRESS.md:3531-3548` closes SK-V11 as measured fixpoint, not
     grammar-generalization admission, and says the non-JSON generated-intervention
     axis remained blocked.
   - `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:38-63`
     carries the S-P3 facts: CSS first, strict lightningcss `+1` bar, parse-only
     diagnostic-only, conditional union/ASM categories, `escape_mask_64`
     prerequisite, and zero-orphan close.

8. P2 and PIN-V3 confirmation records support, rather than weaken, PIN-V4.
   - `restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CONSOLIDATED.md:7-24`
     records S-P2 PIN-V3 as the first clean cycle and requires PIN-V4 confirmation
     before convergence.
   - `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:14-23`
     records PIN-V3 and PIN-V4 as consecutive clean S-P2 cycles.
   - `restart/skinny/tranches/sk-v12/research/p3/hardening/PIN-V3/CONSOLIDATED.md:29-47`
     records the accepted S-P3 PIN-V3 facts that PIN-V4 must preserve: fallback
     block, exact CSS row/output plane/runtime path, W2 prerequisite, strict CSS
     ADMIT, and measured FIXPOINT.

## Required Fixes

None.

## CH6 Result

PASS.
