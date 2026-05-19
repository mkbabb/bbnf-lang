# SK-V10 S-P3 V3 CH6 - Anti-Paper-Close

Verdict: ACCEPT

Acceptance percentage: 96%

## Scope

V3 confirmation audit for the V2-accepted CH6 anti-paper-close contract. This
review checks that the V2 acceptance properties still hold after the hygiene
fold: proof-only waves cannot move rows, W7/W8 require threshold-clearing
measured caller microbench artifacts and REDRESS observed-value-vs-threshold on
miss, W9 is row-gated, and W3/parse-only close routes remain blocked.

Sources audited:

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH6-anti-paper-close.md`
- `skinny/RESULTS.md`

## Concrete Findings

### F1 - V2 CH6 acceptance is preserved as the confirmation baseline

Severity: accepting positive finding.

The V2 consolidated hardening record accepted all six lenses with CH6 at 96%,
mean lens score 95.8%, and zero critical defects
(`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:13-23`).
It also states that V3 is the required confirmation cycle after the V2 hygiene
fold (`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:37-43`).
The V2 CH6 lens specifically accepted the anti-paper-close contract because
W7/W8 proof-only closure depends on measured threshold-clearing caller
microbench artifacts, REDRESS records observed value versus threshold on miss,
proof-only waves cannot move rows, W9 remains row-gated, and W3/parse-only stays
firewalled
(`restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH6-anti-paper-close.md:148-154`).

Required fix: none.

### F2 - W7/W8 proof-only closure still requires threshold-clearing measured artifacts

Severity: accepting positive finding.

The global close condition keeps kernel/SIMD production behind scalar oracle,
differential/checkasm harness, target-host feature gate, representative corpus
slices, caller microbench, failure threshold, and same-wave production caller
before production (`restart/skinny/tranches/sk-v10/SPEC.md:43-46`). The W7
entry gate requires exactly one string primitive family plus caller, scalar
oracle, representative slices, feature gate, and failure threshold
(`restart/skinny/tranches/sk-v10/SPEC.md:544-546`). Its exit gate requires the
caller microbench artifact to clear the predeclared threshold and record
observed value, threshold, run id, host triple, build flags, feature gate,
representative corpus slices, sample count, scalar oracle identity, and
differential harness identity
(`restart/skinny/tranches/sk-v10/SPEC.md:557-566`).

W8 carries the same measured close shape: its entry gate requires one escape or
segment primitive family, caller, scalar oracle, representative slices, feature
gate, and failure threshold (`restart/skinny/tranches/sk-v10/SPEC.md:592-595`),
and its exit gate requires a threshold-clearing caller microbench artifact with
the same observed-value and run metadata
(`restart/skinny/tranches/sk-v10/SPEC.md:607-616`). The dispatch prompt repeats
the load-bearing rule and states that a miss records observed value versus
threshold in REDRESS
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:158-162`).

Required fix: none.

### F3 - Proof-only waves cannot move rows

Severity: accepting positive finding.

The SPEC manifest marks W7 and W8 as proof-only and W9 as the direct/typed
production follow-on (`restart/skinny/tranches/sk-v10/SPEC.md:171-173`). The
manifest rules make the split explicit: W7/W8 are deliberately proof-only micro
waves and a production caller lands only in W9
(`restart/skinny/tranches/sk-v10/SPEC.md:186-189`). W7 tasks forbid production
caller wiring and its exit gate forbids `RESULTS.md` row movement
(`restart/skinny/tranches/sk-v10/SPEC.md:548-566`); W8 does the same
(`restart/skinny/tranches/sk-v10/SPEC.md:597-616`). P3-C independently defines
`proof-only` as a close state that cannot edit `RESULTS.md` or claim SOTA
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:21-25`)
and repeats for W7/W8 that no production caller wiring and no `RESULTS.md`
movement occur
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:318-327`).

Required fix: none.

### F4 - REDRESS-on-miss records observed value versus threshold

Severity: accepting positive finding.

W7's revert protocol requires proof, harness, and microbench to revert as one
slice and records REDRESS with scalar parity failure or observed microbench
value versus threshold (`restart/skinny/tranches/sk-v10/SPEC.md:568-570`). W8's
revert protocol records scalar parity failure, policy leak, or observed
microbench value versus threshold
(`restart/skinny/tranches/sk-v10/SPEC.md:618-620`). P3-C's proof-only close gate
requires the proof artifact to record observed value, threshold, host triple,
CPU, target flags, feature gate, corpus slices, sample metadata, run id, scalar
oracle, and differential harness identity
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:318-327`).

Required fix: none.

### F5 - W9 is row-gated and cannot inherit a proof-only or W3 shortcut

Severity: accepting positive finding.

The SPEC constrains W9 to only a relevant accepted W7/W8 `C4`-`C7` primitive and
states that direct/typed movement is row-gated only if measured
(`restart/skinny/tranches/sk-v10/SPEC.md:627-630`). W9 entry requires the exact
accepted proof and exactly one current caller such as
`match_string_at_quote_trusted_utf8`, `validate_unicode_escape_run`,
`decode_unicode_escape`, or `unescape_string`
(`restart/skinny/tranches/sk-v10/SPEC.md:643-646`). Its tasks restrict the wave
to one proven primitive, one existing production caller, one consumer plane, and
one row-moving target set (`restart/skinny/tranches/sk-v10/SPEC.md:648-657`).
Its exit gate requires same-commit production caller consumption, direct or
typed row-floor evidence, Track 2/oracle independence for row movement, W10b
maintain floors, and parse-only rows staying `S / NO-GO`
(`restart/skinny/tranches/sk-v10/SPEC.md:659-668`). The dispatch prompt repeats
that W9 is the only kernel production wave and may consume only the relevant
accepted W7/W8 proof for the exact primitive and existing call site
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:59-64`).

Required fix: none.

### F6 - W3 and parse-only paper-close routes remain blocked

Severity: accepting positive finding.

The SPEC states at the top that parse-only remains diagnostic `S / NO-GO` and
that REDRESS 96, 97, and 98 retire the W3 union-substrate thesis with no renamed
reopen route (`restart/skinny/tranches/sk-v10/SPEC.md:10-13`). The close
condition keeps parse-only outside the SOTA close target and treats parse-only
throughput, PMU, cycles, masking probes, and structural scans as diagnostic
non-producers (`restart/skinny/tranches/sk-v10/SPEC.md:32-34`). The
non-negotiables ban W3 union/event substrate, retained class column, structural
cursor, `UnionTape`, class-lane-only route, W4-through-W3 cascade-lock, renamed
equivalents, and parse-only SOTA admission
(`restart/skinny/tranches/sk-v10/SPEC.md:123-129`).

W3 itself is a governance firewall: its tasks audit aliases and parse-only SOTA
claims, and its exit gate requires no live W3 dispatch route, gate rejection of
parse-only SOTA claims, no source behavior, and no row movement
(`restart/skinny/tranches/sk-v10/SPEC.md:360-391`). The dispatch prompt also
forbids reopening W3 through a renamed W3, structural cursor, `UnionTape`,
retained class column, sidecar producer, or W4 cascade-lock, and states that
parse-only is diagnostic while direct and typed product planes are row-moving
surfaces (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:12-16`). The
current result table is consistent: the opening `twitter` parse-only row remains
`S / NO-GO`, while direct and typed rows are separate planes
(`skinny/RESULTS.md:5-7`).

Required fix: none.

## Required Fixes

None required for CH6 acceptance.

## Final CH6 Disposition

ACCEPT. V3 confirms no regression from V2 acceptance. The current contract still
blocks paper close by requiring measured W7/W8 proof-only microbench artifacts
that clear predeclared thresholds, preserving observed-value-versus-threshold
REDRESS on miss, preventing proof-only row movement, keeping W9 as the only
row-gated production path, and preserving the W3/parse-only firewall.
