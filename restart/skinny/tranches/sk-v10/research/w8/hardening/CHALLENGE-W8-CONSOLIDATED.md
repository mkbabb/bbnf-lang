# SK-V10 W8 CHALLENGE - Consolidated

Disposition: ACCEPT.

## CH1 Correctness

ACCEPT. `C6-hex-escape-proof` is the only W8 route selected, and it is
independent of W7's rejected full-string proof. The current caller is
`unescape_string`, with the aarch64 x4 path reached through
`unescape_four_unicode_escapes`. The primitive remains fixed-width hex decode:
`unescape_uxxxx_x4_neon` consumes packed quartet bytes only.

Redress must prove caller behavior, not just primitive decoding. The
differential surface must cover valid BMP escapes, valid surrogate pairs,
invalid hex, lone surrogates, non-contiguous escape batches, and full fixture
string contents.

## CH2 Generality / Lock 14

ACCEPT. The generic SIMD primitive may classify/decode four fixed-width hex
quartets. JSON slash, `\u` introducer, surrogate-pair policy, invalid-offset
reporting, and materialized output stay in `parse-that-regex` or in the proof
artifact. No generated JSON parser, direct row, typed row, or generic codegen
behavior is edited in W8.

If redress edits only the proof artifact path, no Lock 14 allowance is needed.
If it edits `bbnf-simd` or `parse-that-regex`, the edit must be scalar
reference or harness-only; production behavior changes are W9 scope.

## CH3 Regression / REDRESS

ACCEPT. W8 is proof-only and moves no `RESULTS.md` row. On parity failure,
policy leak, or microbench failure, the proof/harness patch is saved to
`/tmp/skv10-waveW8-rejected.patch` and reverted. W9 remains responsible for
production wiring, W10b maintain floors, Track 2/oracle independence, and any
direct or typed row movement.

## CH4 Cost And Micro-Proof Adequacy

ACCEPT. The threshold is caller-level aggregate `>=1.08x` over a scalar-only
mirror on real `\u` string contents from `unicode_escapes`, `unicode_mixed`,
and `y_string_unicode`. A standalone `unescape_uxxxx_x4_neon` timing loop is
not sufficient.

The artifact must record observed aggregate speedup, threshold, run id, host
triple, build flags, feature gate, representative slices, sample count, scalar
oracle identity, differential harness identity, and current caller identity.

## CH5 Hidden Coupling

ACCEPT WITH WATCHPOINTS. `unescape_string` already contains the x4 aarch64 path.
The proof must therefore avoid behavior changes disguised as measurement. The
scalar-only mirror may live in the proof artifact, but redress must not alter
the current production caller or generated call sites.

The benchmark must include a fallback/non-contiguous case because
`unescape_four_unicode_escapes` intentionally refuses batches that are not four
adjacent `\uXXXX` escapes. A proof that only measures dense artificial batches
would not cover the current caller.

## CH6 Anti-Paper-Close

ACCEPT. W8 cannot close on "existing x4 is wired." It closes only if parity and
caller-level microbench both pass. If the benchmark misses the threshold, the
route is rejected even with green primitive correctness.

## Required Redress Discipline

Redress may proceed without a plan revision. It must:

1. Keep W8 proof-only and behavior-free.
2. Preserve JSON policy outside `bbnf-simd`.
3. Run scalar/reference and x4 parity evidence.
4. Produce the caller microbench artifact under
   `restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/`.
5. Record PASS only if aggregate observed speedup is `>=1.08x`.
6. Leave `skinny/RESULTS.md` unchanged.

## CHALLENGE Addendum - Fixture Eligibility Revision

Disposition: ACCEPT PLAN REVISION.

Redress found that `unicode_mixed` has no valid JSON `\uXXXX` string contents
for the C6 caller. This is not a reason to synthesize an artificial mixed row
or to treat escaped-backslash `\\u` data as a Unicode escape. The revised gate
must report `unicode_mixed` as zero eligible for C6 and run the aggregate
microbench threshold over the two eligible fixed-width Unicode escape slices:
`unicode_escapes` and `y_string_unicode`.

The addendum preserves CH4 because the benchmark remains caller-level and
threshold-bearing. It preserves CH2 because JSON escape policy still lives in
the caller/proof artifact. Redress must include an escaped-backslash `\\u`
differential case so the zero-eligible `unicode_mixed` finding is covered by
policy parity rather than ignored.
