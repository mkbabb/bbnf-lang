# SK-V11 W5 R6 Preblocked Risk

Pass: W5 Phase 1 research.
Scope: preblocked ledger and CHALLENGE risks for W5 bounded string span and
special-byte scan.
Output: this file.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` §9.
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`.
- `skinny/REDRESS.md` entries 28, 33, 49, 54, 55, 59-62, 66-69, 72, 83, and
  106.
- Latest in-repo six-lens authority: S-P3 V4 hardening consolidation, accepted
  6/6 with zero open critical defects. No separate repository file named
  "six-axis audit" was found during scoped discovery.

## W5 Envelope

W5 may consider only C2 bounded special-byte scanning, D3
`borrowed_string_span`, P2-E `pt_bounded_plain_string_end`, and C7 support when
that support is consumed by C2 in the same wave. The allowed helper shape is a
grammar-parameterized span/end result such as `{content_start, content_end,
raw_end, needs_decode}` or first-interesting byte. It must return spans and
decode-needed status only.

The gate is `G-W5-STRING-SPAN-DIRECT`. Before redress, CHALLENGE must select
one generated direct or typed string/key consumer, a cap, and at most two
target rows. Selected rows must clear both Track 1 and independent Track
2/oracle direct floors:

| Row | Floor Mbps |
|---|---:|
| `twitter/direct_to_struct` | 13740 |
| `github_events/direct_to_struct` | 13403 |
| `update_center/direct_to_struct` | 10059 |
| `random/direct_to_struct` | 7878 |
| `distinct_values/direct_to_struct` | 2658 |
| `gsoc-2018/direct_to_struct` | 3737 |
| `y_string_unicode/direct_to_struct` | 3950 |

Unicode residual rows are not default W5 guards. If `unicode_escapes`,
`unicode_mixed`, or `y_string_unicode` is selected, it is floor-bearing. If it
is not selected, it remains a W6/W8 residual and may not be silently admitted
or demoted.

## Exact Forbidden Routes

W5 must not reopen these routes, including under renamed helper, wrapper,
"proof", or "field fact" wording:

| Block | Forbidden route in W5 |
|---|---|
| REDRESS 28 + 33 | Active 16-byte tiny-string NEON/TBL parser wiring, active tiny-string dispatch as parse/direct close, or any claim that primitive parity makes the old `match_tiny_plain_string` route production. Entry 28 measured about a 25% `twitter` regression when routed into Track 1/Track 2; entry 33 says the Class A kernel targeted the wrong boundary. |
| REDRESS 59 | Prescribing the SK-V5 UTF-8/16-byte body-scan fusion class as sufficient close without fresh same-row falsification gates, profile path, Mbps or c/B delta, and hot symbol boundary. |
| REDRESS 60 | Retained trusted-string boundary collapse by deleting the scalar tiny-string probe before the trusted full-string matcher. |
| REDRESS 61 | Always-wide retained trusted full-string scan, including a 64-byte scanner consumed by retained parse as the broad first block. |
| REDRESS 62 | Delayed-wide retained trusted string scan that keeps the tiny probe but enters a 64-byte scanner after a first clean local block. |
| REDRESS 72 | Global cap-16 string policy. The only admitted cap-16 shape is generated retained `OffsetTape`; W5 cannot apply cap 16 to generated direct `SinkOnly`, hand Track 2, or generic policy without new plane-specific evidence and gates. |
| REDRESS 83 | Generated-retained `StringBlock16` tiny wrapper or another retained wrapper over the current `string_block` primitive. Correctness and checkasm were green, but six named parse rows failed and regressed. |
| REDRESS 106 | Full-string primitive microproof replay, primitive-parity-only production, or production wiring justified by `string_special_block` checkasm alone. The REDRESS 106 caller proof produced aggregate `0.774x` versus required `1.08x`, with `unicode_mixed` and `unicode_basic` regressions. |
| REDRESS 49 | No-allocation decoded-string visitor layered over existing unescape, even though generated source hooks remain admitted as a substrate seam. |
| REDRESS 54 | Exact decoded-string stats sink: decoded length plus streamed hash/stats as a sink-local replacement for allocation. |
| REDRESS 55 | Quote-source fused streaming hash/materializer for the current digest workload. It still lost to allocate-then-contiguous-hash baseline. |
| REDRESS 66 | Direct source-hook receiver folding or field-layout materializer that only removes receiver/closure overhead around the existing escaped-string allocation path. |
| REDRESS 67 | Parser-owned decoded scratch threaded through generated direct parsing. |
| REDRESS 68 | Byte-output `unescape_json_string` materialization inside the current `Cow<str>` API or equivalent byte-writer rewrite without a new consumer contract. |
| REDRESS 69 | DirectBuild semantic string facts, decoded length/fingerprint facts, semantic string hash side channels, or output hash shortcuts for the current direct digest workload. |

Cross-cutting forbidden shapes: decoded scratch; retained string side tables;
retained semantic string facts; retained wide string facts; parser-owned
decoded materialization; output hash shortcuts; `StringBlock16` retained
wrappers; 64-byte retained scans; JSON policy in generic crates; and row
movement from parse-only, primitive-only, checkasm-only, PMU-only, or
telemetry-only evidence.

## Material Differentials W5 Could Clear

These are not permission to implement. They are the minimum differentials that
would let CHALLENGE decide a W5 plan is not merely replaying a rejected route:

| Rejected family | Minimum W5 material differential |
|---|---|
| REDRESS 28 + 33 tiny-string NEON/TBL | A different current hot boundary from fresh W5 profile evidence; scalar oracle exposed as the comparison point; strict parity/checkasm if native code is used; a generated direct/typed or non-JSON product consumer in the same wave; and selected row floors. No parse-only close and no active retained tiny-string parser wiring. |
| REDRESS 59-62 retained string scans | A non-retained or generated product-plane span helper with a fresh P1 owner, not retained parse widening. It must preserve the short-string early-out unless the selected product consumer proves a different boundary, and it must add no sidecar, retained fact, or wide retained scanner. |
| REDRESS 72 cap policy | Plane-specific evidence for the selected direct/typed consumer. Generated retained cap-16 evidence does not transfer to direct `SinkOnly`, hand Track 2, or non-JSON. The selected cap must be named before redress and enforced by row guards. |
| REDRESS 83 wrapper | A lower-overhead caller-local extractor or inline/native body with its own scalar oracle, strict alignment/tail/quote/escape/control/non-ASCII parity, caller microbench, same-wave generated direct/typed consumer, and row floors. A retained wrapper remains blocked. |
| REDRESS 106 full-string proof | A narrower caller than the rejected broad proof, new source delta, strict scalar/checkasm parity, caller microbench before production, and selected direct/typed/non-JSON row gates. Passing existing primitive checkasm is necessary but insufficient. |
| REDRESS 49/54/55 decoded materialization | A new product representation or generated typed/direct field materializer that beats the current allocate-then-contiguous-hash baseline through same-wave row gates. Another visitor, exact-stats pass, or streaming hash under the current digest contract is blocked. |
| REDRESS 66-69 direct materialization/scratch/facts | A consumer contract materially different from current direct digest hashing, such as a real typed string field access pattern or a grammar-neutral decoded-string primitive that beats `unescape_json_string` standalone and through a same-wave generated consumer. No parser-owned scratch, semantic hash facts, byte-output rewrite, or source-hook receiver shortcut. |

The strongest W5-safe differential is therefore: a borrowed string span/end
oracle consumed by exactly one generated direct or typed string/key path,
returning offsets plus `needs_decode`, with Track 1 and independent Track
2/oracle equality on the same output plane, selected row floors, non-JSON proof
if generic code changes, and no decoded materialization in the parser.

## Challenge Risks

1. Is the candidate a span/end helper, or does it smuggle decoded bytes,
   decoded stats, hashes, semantic facts, or scratch state across the parser
   boundary?
2. Which exact generated direct or typed string/key consumer consumes the span
   in the same wave, and what independent Track 2/oracle proves the same output
   plane?
3. Which one or two target rows are selected, and do both tracks clear the
   listed direct floors in the same run? If `y_string_unicode` is selected, is
   it treated as floor-bearing at 3950 Mbps?
4. What fresh profile evidence shows the selected boundary is current and not
   the REDRESS 28/33 or REDRESS 60-62 boundary under a new name?
5. If a SIMD/native body appears, where are the scalar oracle, strict parity
   sweep, feature/fallback, caller microbench, and no-regression gates recorded
   before production wiring?
6. Does the plan rely on primitive parity, checkasm, PMU, parse-only rows, or
   telemetry as product admission evidence? If yes, reject.
7. Does the cap transfer generated retained `OffsetTape` cap-16 evidence into
   direct `SinkOnly`, hand Track 2, or non-JSON? If yes, reject unless the plan
   supplies new plane-specific evidence and row gates.
8. Does the change touch `parse-that-regex`, `bbnf-simd`, codegen, or runtime
   outside generated JSON in a way that changes generic behavior? If yes, what
   CSS L4, Sheets, or BBNF-self string/literal proof is gate-consumed in the
   same wave?
9. Does the plan make W6 depend on a W5 behavior source that has not admitted?
   W6 may consume W5 span APIs only after W5 admits, or after CHALLENGE accepts
   a rejected-but-reusable scalar proof with no behavior source.
10. What is the revert slice? SPEC requires parse-that/simd/generated/bench/
    gate/RESULTS to revert as one slice on parity failure, row-floor miss,
    Unicode guard regression, or REDRESS 106 replay, with the rejected patch
    preserved for REDRESS.

## R6 Disposition

R6 should challenge any W5 proposal as a REDRESS replay unless it names:

- The exact forbidden routes it avoids.
- The material differential from each nearby rejected family.
- One same-wave product consumer and at most two selected rows.
- Scalar oracle and, when native code is present, strict parity plus caller
  microbench before production.
- Track 1 / independent Track 2-or-oracle equality on the same output plane.
- Lock 14 non-JSON proof when generic behavior changes.

Absent those bindings, the W5 plan is paper-close or rejected-route replay, not
an admissible bounded string span wave.
