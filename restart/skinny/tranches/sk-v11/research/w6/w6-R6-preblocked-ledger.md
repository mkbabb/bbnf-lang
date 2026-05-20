# SK-V11 W6 R6 Preblocked Ledger

Pass: W6 Phase 1 research.
Scope: REDRESS and preblocked ledger for W6 escaped segment and hex decode.
Output: this file only.
Source edits: none.
Date: 2026-05-20.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 10 and Section 13.
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md`.
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`.
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`.
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`.
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`.
- `skinny/REDRESS.md` entries 64, 66, 67, 68, 69, 82, 83, 107, 108, 113, and
  116.
- W5 research, CHALLENGE, CHALLENGE V2, and redress block artifacts under
  `restart/skinny/tranches/sk-v11/research/w5/`.

## W6 Envelope

W6 is the escaped segment and hex decode slice. Its permitted candidate surface
is C3 escape segment / hex decode, with C2/D3 support only where it is consumed
by the same W6 product path. `HEX_QUARTET_X4_PROOF` is support only unless W6
names a new source delta beyond the already-consuming `unescape_string` caller.

The SPEC Section 10 entry gate is now narrow because W5 ended at REDRESS 116:
W5 did not admit a bounded span API and did not produce a reusable scalar
proof. Therefore W6 cannot depend on a W5 behavior seam. W6 may dispatch only
if CHALLENGE accepts an independent segment plan that names a new source delta,
a direct/typed/non-JSON escaped-segment consumer, and measurement gates before
implementation.

The JSON direct rows W6 may select are:

| Row | Binding floor Mbps |
|---|---:|
| `unicode_escapes/direct_to_struct` | 3441 |
| `unicode_mixed/direct_to_struct` | 2588 |
| `y_string_unicode/direct_to_struct` | 3950 |

`unicode_basic/direct_to_struct` is the direct guard and must hold Track 1
2253 Mbps and Track 2 2182 Mbps. Any W6 source or gate/report change that
touches direct output also inherits the Section 0.5 direct and typed guard
floors.

## Hard-Blocked Routes

These routes are not admissible W6 plans, including under renamed helper,
wrapper, "proof", or "source delta" language.

| Block | W6 forbidden route |
|---|---|
| REDRESS 64 | Retained Unicode-escape run validator or dense four-unit retained `\uXXXX` validation fast path. The route improved dense `unicode_escapes` but regressed `y_string_unicode` and missed companion rows, proving contiguous fixed-width runs are too narrow. |
| REDRESS 66 | Direct source-hook receiver or field-layout materializer that only removes receiver/closure overhead around the existing escaped-string allocation path. |
| REDRESS 67 | Parser-owned decoded scratch threaded through generated direct parsing, including reusable `String` scratch passed from parser control into sink calls. |
| REDRESS 68 | Byte-output rewrite of `unescape_json_string` inside the current `Cow<str>` API or equivalent byte-writer materializer without a new consumer contract. |
| REDRESS 69 | DirectBuild semantic string facts, decoded length/fingerprint facts, semantic string hash side channels, or output-hash shortcuts for the current direct digest workload. |
| REDRESS 82 | Single-quartet Unicode materializer or one-quartet-at-a-time SIMD classifier consumed by current JSON escape decode/materialization. Correctness and checkasm passed, but direct Unicode rows stayed far below floor. |
| REDRESS 83 | Generated-retained `StringBlock16` wrapper, retained tiny-string probe widening, or using retained string-block evidence to justify W6 escape work. This is W5-adjacent but remains a W6 trap when an escape plan tries to route through retained string scanning. |
| REDRESS 107 | Treating the accepted x4 escape micro-proof as production admission. REDRESS 107 proves a caller microbench for the existing x4 path; it moves no row and wires no new production behavior. |
| REDRESS 108 | Reusing the already-wired `unescape_string` / `unescape_four_unicode_escapes` production path as W6 same-wave production. A cosmetic wrapper, constant, feature re-gate, or provenance relabel is a gate failure. |
| REDRESS 113 | Claiming non-JSON proof from W6 without the missing generated non-JSON baseline. W2 remains blocked; W6 may only use a non-JSON consumer if new authority supplies generated Track 1, independent same-plane oracle, strict equality, baseline Mbps, and gate consumption. |
| REDRESS 116 | Depending on W5 bounded-span source behavior, W5 span API, or W5 reusable scalar proof. W5 did not admit any of them. |

Cross-cutting hard blocks from SPEC Section 13 also bind W6: no parse-only row
admission, no sidecar or parallel substrate, no generic JSON policy leakage, no
new directive/BIR/BackendShape/public substrate, no decoded scratch or
semantic string side channel, no x86 work, and no behavior claim from PMU,
checkasm, primitive parity, or telemetry alone.

## Material Differential Required For An Admissible W6 Plan

An admissible W6 plan must state this package before CHALLENGE can accept
redress:

1. REDRESS citations: name REDRESS 64, 66-69, 82, 83, 107, 108, 113, and 116,
   then state why the plan is not any of those routes.
2. New source delta: identify a real escaped-segment or hex-run source change
   beyond current `unescape_string` production. Acceptable shapes include a
   scalar segment-stream oracle over raw spans, simple escapes, and decoded
   scalar values, or a generated product consumer that changes when decoded
   segments are consumed. A wrapper around `unescape_string` is not a source
   delta.
3. Same-wave consumer: wire exactly one generated direct, generated typed, or
   authorized non-JSON escaped-string/hex consumer in the same commit. For JSON,
   the consumer must be a product path, not retained parse and not parse-only.
4. Policy boundary: JSON surrogate policy stays in the generated JSON caller.
   CSS variable-width escapes, CSS hex-color policy, BBNF literal policy, and
   Sheets doubled-quote policy stay in their generated or host callers. Generic
   `parse-that-regex` and `bbnf-simd` code may expose neutral hex or segment
   primitives only.
5. Scalar oracle: define scalar segment parity over raw spans, simple escapes,
   valid Unicode escapes, invalid Unicode escapes, surrogate pairs, unpaired
   surrogates, boundary splits, controls, tails, and non-zero offsets.
6. SIMD/x4 proof, if used: add scalar x4 oracle and strict checkasm over valid,
   invalid, mixed-validity, alignment 0..63, surrogate, unpaired-surrogate,
   boundary, and tail cases before production wiring. Existing x4 smoke and
   REDRESS 107 are insufficient.
7. Micro-prove-first: run a same-host caller microbench for the exact selected
   consumer before Criterion. Primitive-only speed or REDRESS 107 aggregate
   proof does not admit W6.
8. Row gate: preselect `unicode_escapes`, `unicode_mixed`, `y_string_unicode`,
   or a CHALLENGE-approved subset, and require both generated Track 1 and
   independent Track 2/oracle to clear the listed floors in the same native run.
9. Guard block: preserve `unicode_basic/direct_to_struct` and all direct/typed
   guard rows required by SPEC Section 0.5.
10. Gate/report consumption: any row movement must be consumed by `gate-json`
    in the same wave with W6 provenance, direct digest output plane, strict
    comparator evidence, independent Track 2/oracle status, run id, host, flags,
    sample count, and REDRESS id.
11. Revert protocol: on parity failure, checkasm failure, missing source delta,
    Track 2 coupling, row-floor miss, guard miss, JSON policy leak, W5
    dependency, or REDRESS 108 replay, revert the full W6 source/generator/
    SIMD/bench/gate/RESULTS slice, preserve the rejected patch, and record
    measurements in REDRESS.

The clearest admissible differential is not "make x4 production." It is:
`pt_escaped_string_segments` or an equivalent neutral escaped-segment stream,
consumed by one product caller that was not already using the current
`unescape_string` path as its only work, with scalar segment parity, optional
strict x4 support, and direct row measurement.

## W5 Carry-Forward

W5 does not give W6 an admitted string span seam. REDRESS 116 says no behavior
source, generated runtime, SIMD kernel, benchmark body, gate/report schema, or
`RESULTS.md` row moved; it also says W5 admits no span API and no
rejected-but-reusable scalar proof. W6 therefore starts from the live pre-W5
source tree.

W5 research is still useful as negative boundary:

- W5 R1 and R6 block decoded bytes, decoded stats, hashes, semantic facts,
  parser-owned scratch, retained wrappers, and primitive-only proof.
- W5 R3 keeps current string-block SIMD out of production absent a new caller
  proof; W6 must apply the same discipline to escape SIMD/x4.
- W5 R5 and CH2 carry REDRESS 113: grammar-neutral candidate shape is not a
  generated non-JSON parser proof.
- W5 CH1 V2 and CH4 V2 show that CHALLENGE can block source redress when
  correctness fixtures or an independent Track 2 cost mechanism are missing.
  W6 must not proceed with a source patch if it cannot name the Track 2/oracle
  path that could plausibly clear the selected Unicode floors.

## Downstream W7 / W8 Impact

If W6 admits one or more Unicode direct rows:

- W7 must profile after W6 and may select only rows where `output_digest_hash`
  or the host sink remains a limiting leaf. W7 cannot use W6 admission as proof
  that digest/hash is now parser semantics, and it cannot introduce semantic
  string/hash side tables blocked by REDRESS 69.
- W8 removes admitted Unicode rows from the residual direct fixpoint set, keeps
  all unadmitted rows floor-bearing, and records W6 provenance for every moved
  row. `unicode_mixed` remains W0-clamped unless W6 provides behavior-wave
  provenance and both-track measurement above 2588 Mbps.

If W6 rejects with measurement:

- W7 still may dispatch, but only after a fresh post-W6 or carried-forward
  profile proves output digest/hash is limiting on a bounded selected residual
  subset. It must not claim the Unicode escape path was repaired.
- W8 must treat `unicode_escapes`, `unicode_mixed`, and `y_string_unicode` as
  residual direct rows unless each has a W6 REDRESS uncloseable proof naming
  attempted intervention, Track 1, Track 2/oracle, sonic direct comparator,
  floor, guard result, and exhausted route.
- The W8 fixpoint cannot paper-close with "x4 proof exists" or "unescape is
  already wired." REDRESS 107/108 plus any W6 rejection must be cited as proof
  that the existing path is exhausted under current authority.

If W6 cannot produce a measurable gate before redress:

- Record W6 as BLOCKED or REVISE before implementation rather than manufacturing
  a source probe. The unresolved gate is "new escaped-segment source delta plus
  same-wave product consumer beyond `unescape_string`."
- W7 and W8 may continue only with that unresolved Unicode route carried
  forward explicitly; W9 close must either show per-row uncloseable proofs or
  escalate the direct/non-JSON close condition.

## R6 Disposition

R6 should challenge any W6 proposal as REDRESS replay unless it states:

- the exact forbidden routes avoided;
- the material differential from `unescape_string` and x4 proof history;
- one same-wave product consumer and selected Unicode floors;
- scalar segment oracle and strict x4 checkasm if native code is used;
- independent Track 2/oracle equality on the same output plane;
- REDRESS 113 and REDRESS 116 carry-forward;
- the W7/W8 consequences for admit, reject, or blocked gate.

Absent those bindings, W6 is not an escaped-segment intervention. It is either
an already-wired `unescape_string` relabel, a proof-only x4 replay, or a
decoded-materialization route already falsified by REDRESS 66-69.
