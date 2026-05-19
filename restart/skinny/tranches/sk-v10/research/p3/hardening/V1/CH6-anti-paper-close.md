# SK-V10 S-P3 V1 CH6 - Anti-Paper-Close

Verdict: REVISE

Acceptance percentage: 88%

## Scope

Audit question: can any SK-V10 wave close on prose, stale evidence,
proof-only artifacts while claiming row movement, parse-only gains, future
consumer promises, or unmeasured microbench signals?

Sources audited:

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Concrete Findings

### F1 - W7/W8 proof-only microbench exits can still read as unmeasured proof closure

Severity: blocking REVISE.

The S-P3 CH6 lens requires every wave to close on measurement, not future-phase
promise, and explicitly calls out "wired" or "integrated" closure without a
bench-row threshold as paper-close
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:140-145`). P3-C states the
right micro-proof shape: scalar/checkasm/caller microbench artifacts must carry
run id, host flags, corpus slices, sample metadata, and a failure threshold
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:276-283`),
and unmeasurable gates may only close from named row metadata, gate-json, or
scalar/checkasm/caller microbench artifacts with run id and host flags
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:387-395`).

The final SPEC partially carries this forward by requiring W7 entry to name a
failure threshold
(`restart/skinny/tranches/sk-v10/SPEC.md:493-495`) and W8 entry to do the same
(`restart/skinny/tranches/sk-v10/SPEC.md:532-535`). But the W7 exit gate says
only "Microbench proves the selected primitive at the named cap and plane"
(`restart/skinny/tranches/sk-v10/SPEC.md:504-509`), and W8 says only
"Microbench proves the selected primitive against the named current caller"
(`restart/skinny/tranches/sk-v10/SPEC.md:545-550`). Those exit gates do not
explicitly require the artifact to clear the predeclared threshold, carry run id,
host/build flags, representative slices, and sample metadata, or record REDRESS
with observed value versus threshold on miss.

Why this matters: W7 and W8 are proof-only by design, but proof-only is still a
closure state. Without threshold-bearing exit language, a wave can close on a
narrative "microbench proves" claim or a local slope note while preserving a
future-consumer promise for W9. That is exactly the CH6 paper-close edge.

Required fix:

- In `SPEC.md`, change `G-W7-STRING-MICROPROOF` and
  `G-W8-ESCAPE-SEGMENT-MICROPROOF` so the exit gate requires the caller
  microbench artifact to clear the entry-gate threshold and record observed
  value, threshold, run id, host triple, build flags, feature gate,
  representative corpus slices, sample count, scalar oracle, and differential
  harness identity.
- In both revert protocols, require REDRESS to preserve observed value versus
  threshold, not just "microbench miss."
- Mirror the same wording in `DISPATCH-PROMPT.md` load-bearing facts or
  per-wave dispatch rules so implementation agents cannot treat threshold
  details as optional plan prose.

### F2 - Row-moving gates mostly resist prose, stale evidence, and plane transfer

Severity: non-blocking positive finding.

The final SPEC blocks row movement on stale or prose evidence in the close
condition and non-negotiables. It requires direct row movement to use W1 direct
contract evidence, strict same-run comparator evidence, generated Track 1,
independent Track 2/oracle, matching output plane, validation path, gate-json
consumption, and both tracks meeting `ceil(sonic_direct / 1.10)`
(`restart/skinny/tranches/sk-v10/SPEC.md:35-39`). Typed rows require
generated/serde_json/sonic-rs/independent checksum parity, same-run typed
comparator rows, and typed output-plane gates
(`restart/skinny/tranches/sk-v10/SPEC.md:40-42`). The non-negotiables also
reject strict admission on stale, permissive, lossy, absent, historical,
sidecar-only, PMU, cycles, masking-probe, structural-scan-only, or
Criterion-slope evidence
(`restart/skinny/tranches/sk-v10/SPEC.md:146-150`).

The wave gates then carry numeric floors for W2 direct movement
(`restart/skinny/tranches/sk-v10/SPEC.md:291-310`), W4 `instruments` typed
movement (`restart/skinny/tranches/sk-v10/SPEC.md:379-391`), W6 root typed row
movement (`restart/skinny/tranches/sk-v10/SPEC.md:462-474`), W9 kernel
production (`restart/skinny/tranches/sk-v10/SPEC.md:587-598`), and W10 direct
residual behavior (`restart/skinny/tranches/sk-v10/SPEC.md:626-636`). This is
not a paper-close problem after F1 is fixed.

Required fix: none beyond preserving this wording while fixing F1.

### F3 - Parse-only and W3 paper-close routes are correctly blocked

Severity: non-blocking positive finding.

The inherited results still show parse-only as `S / NO-GO` rows and direct/typed
as distinct output planes; for example `twitter` parse-only remains `S / NO-GO`
while its direct and typed rows are separate surfaces
(`skinny/RESULTS.md:5-7`). The final SPEC keeps parse-only outside the SOTA
target while rows remain `S / NO-GO`
(`restart/skinny/tranches/sk-v10/SPEC.md:32-34`) and makes W3 a firewall with no
source behavior or row movement
(`restart/skinny/tranches/sk-v10/SPEC.md:339-345`).

REDRESS supplies the empirical reason this must stay hard-blocked: REDRESS 96
and 97 were correctness/parity-green before measurement but missed every W3
must-improve row and every W10b maintain row
(`skinny/REDRESS.md:2823-2838`, `skinny/REDRESS.md:2881-2896`). REDRESS 98 then
retires `G-W3-UNION-SUBSTRATE`, and explicitly says the remaining class-lane-only
route would be a paper-close
(`skinny/REDRESS.md:2910-2927`).

Required fix: none.

### F4 - Future consumer promises are mostly contained

Severity: non-blocking positive finding, contingent on F1.

The dispatch prompt requires CHALLENGE to reject W7-W9 plans that combine
multiple primitive families, lack scalar oracle, lack checkasm/differential
parity, lack caller microbench, or lack production consumer
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:104-107`). W9 production
must consume only a W7/W8-proven primitive at an existing call site and must
measure affected rows plus W10b maintain floors
(`restart/skinny/tranches/sk-v10/SPEC.md:576-594`). The pre-blocked ledger also
requires every adjacent route to name the exact consumer plane and call site,
same-host benchmark or Criterion rows, failure threshold, and revert protocol
before implementation
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:242-259`).

Required fix: after F1, ensure W7/W8 cannot close on "current caller identified"
alone; the caller microbench threshold must be the proof closure condition.

## Required Fixes Before ACCEPT

1. Tighten W7 and W8 SPEC exit gates to require threshold-clearing measured
   microbench artifacts, not just "microbench proves" wording.
2. Add explicit observed-value-versus-threshold REDRESS requirements to W7 and
   W8 revert protocols.
3. Mirror the W7/W8 threshold-bearing closure rule into `DISPATCH-PROMPT.md`.

## Final CH6 Disposition

REVISE. The packet is close to acceptable: direct, typed, parse-only, stale
evidence, W3, and production-kernel row gates are materially measurable. The
remaining paper-close risk is concentrated in W7/W8 proof-only microbench exit
wording. Fixing that should move CH6 to ACCEPT.
