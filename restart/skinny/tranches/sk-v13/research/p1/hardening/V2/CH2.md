# SK-V13 S-P1 V2 CH2: Generality / Lock 14

Pass: S-P1 Profile. Cycle: V2.
Reviewer: CH2 generality / Lock 14.
Owner file: `restart/skinny/tranches/sk-v13/research/p1/hardening/V2/CH2.md`.

## Disposition

REVISE.

V2 is materially better than V1: direct-to-struct profiles are no longer panic
captures, mode-III probes have 85/85 profile coverage, and CSS declaration-values
now has a sampled profile. But CH2 cannot ACCEPT because the load-bearing hot
leaves are still mostly JSON generated wrappers/envelopes rather than
grammar-neutral primitives. The artifacts often disclose that limitation, so this
is not a REJECT paper-close defect. It remains a Lock 14 fold requirement before
S-P2 may generalize the profile to CSS L4, Sheets, or BBNF-self.

CH2 standard: `PASS-1-PROFILE.md:129`-`135` requires hot leaves to be attributed
to grammar-neutral primitives such as scanner, classifier, or tape. A JSON-role
hot leaf is acceptable only as an envelope/status marker, not as the primitive
that S-P2 can generalize.

## Evidence

### CH2-V2-001 - Parse-only still collapses to JSON `dispatch_value` envelopes

P1-A retains the V1 parse capture and says the fold did not recapture parse rows
(`p1a-samply-mode-1.md:13`-`19`). Its per-corpus table still attributes most
Track 1 parse-only rows to `runtime::generated_json::generated::dispatch_value`
or shortened `dispatch_value`, commonly at 95%-100% self-time
(`p1a-samply-mode-1.md:83`-`97`). P1-A correctly states the boundary:
`dispatch_value` identifies a hot generated function envelope, not always the
inner primitive (`p1a-samply-mode-1.md:101`), and later warns that it hides inner
string/number/structural attribution (`p1a-samply-mode-1.md:135`).

P1-E repeats the same parse-only attribution as `dispatch_value` for nearly every
corpus (`p1e-hot-leaf-attribution.md:47`-`65`) and uses a `dispatch-envelope`
classification vocabulary (`p1e-hot-leaf-attribution.md:43`-`45`). That is the
right honesty boundary, but it means parse-only is not yet a grammar-neutral
primitive ledger.

Fold action: keep every `dispatch_value` parse row as
`generality_status=unresolved-envelope` unless a deeper profile names a scanner,
classifier, tape, string, unicode, number, or sink primitive with symbol,
self-time, and file:line. S-P2 must not treat parse-only `dispatch_value`
self-time as primitive evidence.

### CH2-V2-002 - Direct coverage is fixed, but most direct leaves are JSON wrappers

P1-B fixed the V1 direct profiling failure: 34 direct profiles, 34 sidecars, and
0 bad return codes (`p1b-samply-mode-2.md:42`-`58`, `:96`-`:102`). However the
Track 1 rank-1 direct leaves are predominantly
`parse_object_value_at_direct::<JsonDigestSink>` or
`parse_array_element_at_direct::<JsonDigestSink>` in
`skinny/crates/runtime/src/grammars/json/generated.rs` (`p1b-samply-mode-2.md:66`-`84`).
P1-B explicitly says direct Track 1 still often resolves to generated direct
envelopes rather than primitive leaves (`p1b-samply-mode-2.md:110`-`112`).

One direct row does resolve to a real primitive candidate:
`unicode_escapes` has `parse_that_regex::unescape_string` at
`parse-that-regex/src/lib.rs:718` for both Track 1 and Track 2
(`p1b-samply-mode-2.md:81`, `:113`-`:115`; `p1e-hot-leaf-attribution.md:62`,
`:90`-`:91`). That can be carried forward as a `unicode/string-decode`
primitive candidate. The rest of the generated direct row names should remain
`json-direct-envelope`, not `resolved-neutral`.

Fold action: add a primitive-status crosswalk for all 17 direct rows. Mark
`unescape_string` as `resolved-neutral-candidate` pending non-JSON confirmation;
mark `parse_object_value_at_direct` and `parse_array_element_at_direct` as
`json-direct-envelope`; mark inlined generic/system leaves such as
`Option<&u8>::copied` and `mach_absolute_time` as `generic-noise` or
`timer/noise`, not parser primitives.

### CH2-V2-003 - Mode-III scanner evidence is useful, but still JSON-scoped

P1-C now has 85/85 mode-III profiles and explicitly unsupported routes for PEXT
and the duplicate dispatch-table probe (`p1c-samply-mode-3.md:41`-`54`,
`:98`-`:106`). Its structural rows consistently expose `scan_tail`,
`scan_structurals`, and one `bulk_emit_positions_64_neon` symbol
(`p1c-samply-mode-3.md:64`-`82`, `:84`-`:91`). This is the strongest CH2
primitive evidence in V2 because it names scanner work instead of only a
generated parse wrapper.

The boundary still matters: those scanner leaves are under the JSON scan surface
and P1-C itself frames the SIMD result as a scanner micro-result, not a union or
cross-grammar route (`p1c-samply-mode-3.md:108`-`122`). P1-D repeats that
structural SIMD is a profile fact, while prior union-substrate regressions remain
binding history (`p1d-pmu-cycles.md:127`-`129`).

Fold action: carry `scan_tail`, `scan_structurals`, and
`bulk_emit_positions_64_neon` as `json-scan-primitive-candidate`, not as fully
general grammar-neutral proof. S-P2 may use them as scanner evidence only if it
also states the JSON structural-set boundary and asks for CSS/non-JSON
confirmation.

### CH2-V2-004 - Typed leaves remain JSON generated product-plane evidence

V2 did not add typed product surfaces for the ten missing corpora
(`p1b-samply-mode-2.md:86`-`88`; `p1d-pmu-cycles.md:123`-`126`;
`p1f-results-delta.md:45`-`48`, `:101`-`:106`). P1-E attributes existing typed
rows to `DirectParser::skip_value`, `parse_option_scalar_string`,
`parse_type_plugin`, `parse_type_mesh`, and
`parse_type_marine_geometry_data` in `generated_real_typed.rs`
(`p1e-hot-leaf-attribution.md:49`-`65`). Those are useful JSON product-plane
observations, not grammar-neutral primitives.

Fold action: mark all `generated_real_typed.rs` leaves
`generality_status=json-typed-only`. Do not let S-P2 cite them as CSS, Sheets, or
BBNF-self evidence without a non-JSON typed/direct consumer showing the same
primitive.

### CH2-V2-005 - CSS is non-JSON telemetry, but does not yet name a CSS parser primitive

The addendum requires full CSS L4 parity and all JSON rows above strict SOTA
(`USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22`-`46`), and the SK-V13 handoff
requires fresh CSS and JSON profile truth (`HANDOFF.md:118`-`134`). V2 now has a
CSS declaration-values profile, but P1-E says its rank-1/2/3 leaves are
`mach_absolute_time`, `LocalFactSink::finish`, and `FactSink::finish`, with the
profile dominated by timer/fact-sink overhead rather than parser primitives
(`p1e-hot-leaf-attribution.md:67`-`72`, `:95`-`:96`). P1-F records the CSS row as
measured but method-mismatched against the SK-V12 Criterion close
(`p1f-results-delta.md:74`-`83`, `:93`-`:95`, `:117`-`:118`), and it records 23
remaining CSS parity rows as not yet measured by S-P1
(`p1f-results-delta.md:49`-`50`, `:105`-`:106`).

That is a clean non-JSON boundary: CSS should remain cross-plane telemetry, not a
JSON admission/classification row and not proof that JSON envelopes generalize.
But it also means CSS does not yet validate scanner/classifier/tape primitives
for CH2.

Fold action: preserve CSS as `non-json-css-telemetry`. Add a CSS primitive status
for the declaration-values row: `css-profiled-nonparser-overhead` until a longer
or narrower profile names a CSS parser primitive. Do not generalize JSON
`dispatch_value` or direct generated wrappers from this CSS sample.

### CH2-V2-006 - P1-F still presents row-path names beside hot leaves

P1-F correctly labels every classification as `profile_signal_not_gate_admission`
(`p1f-results-delta.md:30`-`32`, `:119`-`:120`) and separates profile coverage
from admission. But its direct progress table lists `parse_object_value_at_direct`
and `parse_array_element_at_direct` simply as rank-1 hot leaves with
`measured direct` profile signals (`p1f-results-delta.md:52`-`72`). Without a
generality-status column, that table can be consumed as if JSON row paths and
JSON generated wrapper names were primitive identity.

Fold action: add `primitive_status` to P1-F or the consolidated S-P1 ledger:
`resolved-neutral-candidate`, `json-parse-envelope`, `json-direct-envelope`,
`json-typed-only`, `json-scan-primitive-candidate`,
`css-profiled-nonparser-overhead`, `timer/noise`, or `missing-product-surface`.
Keep row-path outcomes separate from primitive attribution.

## Fold Gate

V2 may proceed only as REVISE for CH2. Required V3/consolidated fold actions:

1. Add a single primitive-attribution ledger across P1-A through P1-F with
   columns for `row`, `plane`, `symbol`, `self_time`, `file_line`,
   `envelope_symbol`, `primitive_class`, `primitive_status`, and
   `non_json_confirmed`.
2. Keep parse-only `dispatch_value` rows as unresolved JSON parse envelopes.
3. Keep direct `parse_object_value_at_direct` /
   `parse_array_element_at_direct` rows as JSON direct envelopes.
4. Carry `parse_that_regex::unescape_string` as the one clear V2 direct
   unicode/string primitive candidate, while noting that it is still only
   JSON-confirmed.
5. Carry mode-III `scan_tail`, `scan_structurals`, and
   `bulk_emit_positions_64_neon` as JSON scanner primitive candidates, with the
   JSON structural-set boundary explicit.
6. Quarantine all `generated_real_typed.rs` leaves as `json-typed-only`.
7. Keep CSS declaration-values as non-JSON telemetry and mark its current hot
   leaves as timer/fact-sink overhead until a CSS parser primitive is profiled.
8. Prevent S-P2 from using JSON envelopes, typed product paths, or CSS fact-sink
   overhead as grammar-neutral proof.

No ACCEPT: V2 still lacks a generality-safe primitive ledger and CSS/non-JSON
parser primitive confirmation.

No REJECT: the artifacts are mostly honest about envelopes, timer noise,
method-mismatch, and missing typed surfaces; the defects are foldable by labeling
and targeted recapture rather than by discarding the V2 profile packet.
