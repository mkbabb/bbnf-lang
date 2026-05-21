# SK-V13 S-P1 V3 CH2: Generality / Lock 14

Pass: S-P1 Profile. Cycle: V3.
Reviewer: CH2 generality / Lock 14.
Owner file: `restart/skinny/tranches/sk-v13/research/p1/hardening/V3/CH2.md`.

## Disposition

ACCEPT.

V3 is generality-safe for S-P2 handoff. It does not prove that the measured hot
leaves generalize to CSS L4, Sheets, or BBNF-self, but it now labels the JSON
envelopes, JSON-only typed leaves, CSS nonparser overhead, and JSON-confirmed
primitive candidates clearly enough that S-P2 cannot honestly consume them as
grammar-neutral proof.

The CH2 bar is attribution safety, not gate admission: `PASS-1-PROFILE.md:129`-
`135` requires hot leaves to be named as primitives rather than mislabeled
JSON-role paths. V2 failed CH2 because generated JSON envelopes and JSON typed
paths were still too easy to read as primitive identity, and CSS had telemetry
without a parser primitive (`HARDENING-S-P1-V2-CONSOLIDATED.md:20`-`21`,
`:45`-`:56`). V3 supplies the missing status vocabulary and applies it with
explicit non-generalization boundaries.

## Evidence

### CH2-V3-001 - Status vocabulary blocks Lock 14 mis-attribution

The V3 ledger defines the needed CH2 vocabulary:

- `json-parse-envelope` and `json-direct-envelope` are explicitly "not a
  grammar-neutral primitive" (`support/evidence-ledger-v3.md:12`-`13`).
- `resolved-json-unicode-candidate` is "JSON-confirmed only" and
  `json-scan-primitive-candidate` says non-JSON confirmation is still absent
  (`support/evidence-ledger-v3.md:14`-`15`).
- `json-typed-only` cannot generalize to CSS/Sheets, and
  `css-profiled-nonparser-overhead` is timer/fact-sink overhead rather than a
  CSS parser leaf (`support/evidence-ledger-v3.md:16`-`17`).
- All rows are marked `profile_signal_not_gate_admission`
  (`support/evidence-ledger-v3.md:23`).

That is the decisive CH2 repair. The ledger does not rename JSON row roles into
neutral primitives; it quarantines them.

### CH2-V3-002 - JSON parse and direct envelopes are labeled safely

P1-A states that parse rows are classified as `json-parse-envelope`,
`function-only-sidecar`, or `resolved-json-unicode-candidate`, and that no
`dispatch_value` row is grammar-neutral primitive evidence
(`p1a-samply-mode-1.md:21`-`24`). The ledger carries that boundary: 15/17 parse
rows remain `json-parse-envelope`, `distinct_values` is `function-only-sidecar`,
and `y_string_unicode` is only a resolved JSON unicode candidate
(`support/evidence-ledger-v3.md:52`-`61`).

Direct rows are also safe. P1-B says generated `parse_*_direct` leaves are
`json-direct-envelope`, `unicode_escapes` / `unescape_string` is a
`resolved-json-unicode-candidate`, and `y_string_unicode` Track 2 is
`timer/noise` (`p1b-samply-mode-2.md:13`-`17`). The direct ledger applies those
statuses across all 17 rows: generated object/array direct wrappers are
`json-direct-envelope`, `unicode_escapes` is the sole direct
`resolved-json-unicode-candidate`, `instruments` is generic/noise, and
`y_string_unicode` carries the Track 2 timer warning
(`support/evidence-ledger-v3.md:32`-`50`).

S-P2 may inspect these rows for targeted follow-up, but CH2 does not allow it
to cite `dispatch_value`, `parse_object_value_at_direct`, or
`parse_array_element_at_direct` as grammar-neutral primitive proof.

### CH2-V3-003 - JSON-only typed leaves and missing typed surfaces are quarantined

V2 retained only seven generated typed rows, and P1-B says the ten unsupported
corpora were not invented in the V2 fold (`p1b-samply-mode-2.md:92`-`94`).
P1-F repeats that typed coverage is 7/17 and the remaining ten are missing
product surface, not profiled/admitted rows (`p1f-results-delta.md:48`-`55`,
`:120`-`:121`).

The V3 ledger labels the seven existing typed leaves as `json-typed-only` and
the ten absent rows as `missing-product-surface`
(`support/evidence-ledger-v3.md:63`-`76`). That satisfies CH2: generated
`generated_real_typed.rs` leaves may remain JSON product-plane evidence, but
they are not carried into S-P2 as CSS, Sheets, or BBNF-self primitives.

### CH2-V3-004 - CSS is admitted only as profiled nonparser overhead

P1-E reports CSS declaration-values rank-1/2/3 leaves as 17.6%
`mach_absolute_time`, 13.7% `LocalFactSink::finish`, and 7.5%
`FactSink::finish`, with the row dominated by timer/fact-sink leaves rather
than a parser primitive (`p1e-hot-leaf-attribution.md:79`-`83`). P1-F preserves
the same boundary: equality passes, the profile top leaf is timer/fact-sink
dominated, and the method-mismatched throughput is not a demotion or gate
admission (`p1f-results-delta.md:79`-`87`, `:98`-`:100`, `:122`-`:125`).

The V3 CSS ledger keeps this exact status as
`css-profiled-nonparser-overhead`; equality/throughput/profile exist, but the
parser hot leaf is unresolved (`support/evidence-ledger-v3.md:100`-`104`).
That is safe for S-P2 because CSS is no longer missing, but it is also not
misused to validate JSON envelopes or scanner candidates.

### CH2-V3-005 - JSON-confirmed primitive candidates are bounded

There are two useful candidate families, and both are constrained correctly.

First, direct unicode/string work has a named leaf:
`parse_that_regex::unescape_string` at `parse-that-regex/src/lib.rs:718`,
46.7% self-time in `unicode_escapes` Track 1 and 46.4% in Track 2
(`p1b-samply-mode-2.md:87`, `:119`-`:121`). The ledger marks it
`resolved-json-unicode-candidate` and sets `Non-JSON confirmed` to `no`
(`support/evidence-ledger-v3.md:47`).

Second, mode-III structural scan profiles expose `scan_tail`,
`scan_structurals`, and one `bulk_emit_positions_64_neon` family across 17/17
JSON structural probes (`p1c-samply-mode-3.md:69`-`87`). P1-C explicitly warns
that this is a scanner micro-result and does not reopen the prior union route
(`p1c-samply-mode-3.md:115`-`117`). The V3 ledger keeps the same boundary:
`structural_scan_scalar` and `structural_scan_simd` are
`json-scan-primitive-candidate`, JSON structural-set only / scanner
micro-result only, with REDRESS 96/97/98 not reopened
(`support/evidence-ledger-v3.md:85`-`91`).

These candidates are safe inputs for S-P2 questions, not S-P2 conclusions.
S-P2 must still obtain non-JSON confirmation before claiming grammar-neutral
generality.

## S-P2 Guard

CH2 acceptance authorizes S-P2 to consume V3 as a labeled evidence packet. It
does not authorize any of these claims:

1. JSON parse/direct envelopes are grammar-neutral primitives.
2. JSON generated typed leaves generalize to CSS/Sheets/BBNF-self.
3. CSS declaration-values currently names a CSS parser primitive.
4. JSON-confirmed unicode or scanner candidates are non-JSON confirmed.
5. Profile signals are gate admissions.

The required S-P2 posture is inquiry-only: use the V3 ledger to select
candidate follow-ups, preserve the `non_json_confirmed=no` boundary, and demand
CSS/Sheets/BBNF-self confirmation before turning any candidate into a general
primitive route.
