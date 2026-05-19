# SK-V10 S-P3 V2 CH6 - Anti-Paper-Close

Verdict: ACCEPT

Acceptance percentage: 96%

## Scope

Audit question: did the V2 challenge-fold close the V1 CH6 paper-close blockers
for W7/W8 proof-only microbench gates, REDRESS-on-miss metadata, proof-only row
movement, W9 row gating, and W3/parse-only firewall behavior?

Sources audited:

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`
- `skinny/RESULTS.md`

## Concrete Findings

### F1 - W7/W8 proof-only gates now require measured threshold-clearing artifacts

Severity: accepting positive finding.

V1 CH6 blocked because final W7/W8 exit wording could still close on
"microbench proves" prose. V2 fixes that in the binding SPEC. The global close
condition requires kernel/SIMD work to use scalar oracle, differential harness,
target-host feature gate, representative slices, caller microbench, failure
threshold, and same-wave production caller before production
(`restart/skinny/tranches/sk-v10/SPEC.md:43-46`). W7's entry gate requires the
plan to name exactly one primitive family, cap, output plane, caller, scalar
oracle, representative slices, feature gate, and failure threshold
(`restart/skinny/tranches/sk-v10/SPEC.md:544-546`), and its exit gate requires
the caller microbench artifact to clear the predeclared threshold while recording
observed value, threshold, run id, host triple, build flags, feature gate,
representative slices, sample count, scalar oracle identity, and differential
harness identity (`restart/skinny/tranches/sk-v10/SPEC.md:557-566`).

W8 mirrors the same fix. Its entry gate requires one escape/segment family,
caller, scalar oracle, representative slices, feature gate, and failure
threshold (`restart/skinny/tranches/sk-v10/SPEC.md:592-595`), and its exit gate
requires threshold clearance plus the same observed-value and metadata fields
(`restart/skinny/tranches/sk-v10/SPEC.md:607-616`). The dispatch contract also
folds this into load-bearing facts, requiring W7/W8 threshold-clearing caller
microbench artifacts and observed value versus threshold in REDRESS on miss
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:156-160`).

Required fix: none.

### F2 - REDRESS on W7/W8 miss now preserves observed value versus threshold

Severity: accepting positive finding.

V2 adds the missing failure metadata. W7's revert protocol now says to revert
proof, harness, and microbench as one slice and record REDRESS with scalar parity
failure or observed microbench value versus threshold
(`restart/skinny/tranches/sk-v10/SPEC.md:568-570`). W8 says the same for scalar
parity failure, policy leak, or observed microbench value versus threshold
(`restart/skinny/tranches/sk-v10/SPEC.md:618-620`). The underlying P3-C proof
gate also requires the proof artifact to record observed value, threshold, host,
target flags, feature gate, corpus slices, sample metadata, run id, scalar
oracle, and differential harness identity, with no production wiring or
`RESULTS.md` movement (`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:318-327`).

Required fix: none.

### F3 - Proof-only work cannot move rows

Severity: accepting positive finding.

The manifest explicitly marks W7 and W8 as proof-only and W9 as the only
production follow-on (`restart/skinny/tranches/sk-v10/SPEC.md:171-173`). The
manifest rules state that W7/W8 are deliberately proof-only micro waves and that
a production caller lands only in W9
(`restart/skinny/tranches/sk-v10/SPEC.md:183-189`). The W7 task list says not to
wire production caller behavior, and the W7 exit gate requires no `RESULTS.md`
row movement (`restart/skinny/tranches/sk-v10/SPEC.md:548-566`). W8 carries the
same no-production and no-row-movement rule
(`restart/skinny/tranches/sk-v10/SPEC.md:597-616`).

Required fix: none.

### F4 - W9 remains row-gated and does not inherit a W3 or proof-only shortcut

Severity: accepting positive finding.

W9 is constrained to a relevant accepted W7/W8 `C4`-`C7` primitive and is
row-gated only when measured (`restart/skinny/tranches/sk-v10/SPEC.md:627-630`).
Its entry gate requires the exact accepted proof and exactly one current
production caller such as `match_string_at_quote_trusted_utf8`,
`validate_unicode_escape_run`, `decode_unicode_escape`, or `unescape_string`
(`restart/skinny/tranches/sk-v10/SPEC.md:643-646`). Its exit gate requires
same-commit consumption by the named caller, direct floor or typed gate
clearance, Track 2/oracle independence for row movement, W10b maintain floors,
and parse-only rows remaining `S / NO-GO`
(`restart/skinny/tranches/sk-v10/SPEC.md:659-668`). The revert protocol reverts
primitive wiring, generated caller changes, gates, and `RESULTS.md` together on
parity failure, W10b maintain miss, missing caller, or row-floor miss
(`restart/skinny/tranches/sk-v10/SPEC.md:670-673`).

Required fix: none.

### F5 - W3 and parse-only paper-close routes remain blocked

Severity: accepting positive finding.

The V2 SPEC opens by preserving parse-only as diagnostic `S / NO-GO` and stating
that REDRESS 96-98 retire the W3 union-substrate thesis, including renamed
reopen attempts (`restart/skinny/tranches/sk-v10/SPEC.md:10-13`). The close
condition keeps parse-only outside the SOTA close target and treats throughput,
PMU, cycles, masking probes, and structural scans as diagnostic non-producers
(`restart/skinny/tranches/sk-v10/SPEC.md:32-34`). The non-negotiables ban W3
union/event substrate, retained class column, structural cursor, `UnionTape`,
class-lane-only route, W4-through-W3 cascade-lock, renamed equivalents, and
parse-only SOTA admission (`restart/skinny/tranches/sk-v10/SPEC.md:123-129`).

W3 itself is a governance firewall with no behavior source and no row movement:
its tasks audit W3 aliases and parse-only SOTA claims, and its exit gate requires
no live W3 dispatch route, `gate-json` rejection of parse-only SOTA claims, and
no source behavior or row movement (`restart/skinny/tranches/sk-v10/SPEC.md:360-391`).
The dispatch prompt repeats that no implementation agent may reopen W3 through a
renamed W3, structural cursor, `UnionTape`, retained class column, sidecar
producer, or W4 cascade-lock, and that parse-only is diagnostic while direct and
typed product planes are the row-moving surfaces
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:12-16`).

The current result surface is consistent with that contract: parse-only rows
remain `S / NO-GO` while direct and typed rows occupy separate output planes, as
shown by the opening `twitter` rows
(`skinny/RESULTS.md:5-7`).

Required fix: none.

## Required Fixes

None required for CH6 acceptance.

Non-blocking caution: preserve the V2 distinction between an intended/named
production consumer in W7/W8 proof work and actual production wiring in W9. The
binding sections are clear enough to accept, but future wave plans should quote
the SPEC's "do not wire production caller behavior" and "No `RESULTS.md` row
moves" clauses verbatim for W7/W8.

## Final CH6 Disposition

ACCEPT. The V2 folded contract fixes the V1 anti-paper-close blocker by making
W7/W8 proof-only closure depend on threshold-clearing measured caller microbench
artifacts with observed value versus threshold metadata, requiring REDRESS on
miss, prohibiting proof-only row movement, keeping W9 row-gated, and preserving
the W3/parse-only firewall.
