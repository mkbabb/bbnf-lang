# SK-V8 W4 Hardening V1 CH3

Verdict: ACCEPT.

Confidence: 90%.

## Findings

1. The proposed hand Track 2 scalar-parent fold does not violate Track 2
   independence as scoped. `track1_digest` enters generated
   `runtime::generated_json::parse_direct`, while `track2_digest` enters
   `hand::sink_digest`; the hand parser owns its own cursor, value dispatch,
   object/array recursion, string matching, number materialization, literal
   handling, whitespace, and errors.
2. This is not REDRESS 84 under a new name if it remains hand-only and
   scalar-only. REDRESS 84 changed generated object/key control so key parsing
   returned the first value byte, then mirrored that generated boundary shape
   into Track 2 and array continuation. The W4 proposal leaves generated
   Track 1 unchanged and only avoids constructing temporary scalar child
   digests in the hand parser.
3. This is not REDRESS 72 if the 8-byte hand direct tiny-string probe remains
   fixed.
4. This is not REDRESS 66-69 if no string/materialization route changes.
   Scalar-parent folding must keep the existing `string() -> Cow<str>` path,
   existing `unescape_string`, exact decoded-value hashing, and no
   DirectBuild/string-fact surface.
5. The admissible implementation shape is narrow: fold only object/array
   scalar values directly into the parent digest. Root scalars and compound
   children still need the existing child digest path. Number folding must
   preserve `number_raw_known` classification, including signed/unsigned/f64
   width and `-0` handling.

## Required Folds

None for CH3 acceptance. Carry these abort checks into implementation review:
owner path limited to `skinny/crates/bbnf-bench/src/direct_struct.rs`; no
runtime/codegen/BIR/directive/substrate changes; no cap-16 rerun; no
source-hook/scratch/byte-output/string-fact changes; no value-byte or
generated helper-shape coupling; selected rows must meet W4 Track 1/Track 2
floors and full-table maintain or the candidate rejects with REDRESS.
