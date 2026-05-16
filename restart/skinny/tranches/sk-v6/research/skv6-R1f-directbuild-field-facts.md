# SK-V6 Wave 3 R1f: DirectBuild Field-Facts Route

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only inspection of skinny codegen/runtime/direct surfaces. No repository files edited.

## Required Inputs Read

- `skinny/REDRESS.md` entries 66-68.
- `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` sections 9-11.
- `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` Wave 3 summary.
- Direct/DirectBuild surfaces:
  - `skinny/crates/ir/src/lib.rs`
  - `skinny/crates/passes/src/lib.rs`
  - `skinny/crates/codegen/src/lower/sink_only.rs`
  - `skinny/crates/codegen/src/json_sink_direct.rs`
  - `skinny/crates/runtime/src/grammars/json/generated.rs`
  - `skinny/crates/runtime/src/grammars/json/sink.rs`
  - `skinny/crates/bbnf-bench/src/direct_struct.rs`

## Current Binding Facts

REDRESS 66 rejects direct source-hook field folding. It removed receiver/closure overhead but kept escaped-string materialization intact; focused medians moved only +0.99% on `unicode_escapes`, +0.11% on `unicode_mixed`, +1.75% on `y_string_unicode`, +1.54% on `distinct_values`, and -0.01% on `gsoc-2018`.

REDRESS 67 rejects parser-owned decoded scratch. It preserved strict correctness but regressed the escaped primary row badly: `unicode_escapes` 4999 -> 2798 Mbps (-44.03%), `unicode_mixed` 4541 -> 4318 Mbps (-4.91%), with partial `y_string_unicode` negative.

REDRESS 68 rejects byte-output `unescape_json_string` under the current `Cow<str>` API. `unicode_escapes` regressed 4970 -> 4771 Mbps (-4.00%). The direct-string allocation / receiver / byte-writing family is exhausted.

The synthesis now requires the next Wave 3 route to target DirectBuild field facts or a strict representation-level direct output contract, not local escaped-string writer churn.

## Code Surface Finding

The DirectBuild surface exists but is too lossy for the next direct intervention:

- `ir::BackendExpr::DirectBuild { shape, fields }` and `DirectBuildField { name, source }` exist in `skinny/crates/ir/src/lib.rs`.
- `passes::extract` currently hardcodes JSON rule-name facts in `direct_fields_for_rule`:
  - object -> `members` repeated `pair`
  - array -> `elements` repeated `value`
  - pair -> `key`, `value`
  - string -> `span`
  - number -> `span`
  - bool -> empty literal bytes
  - null -> no fields
- `codegen/src/lower/sink_only.rs` preserves cloned DirectBuild fields, but `json_sink_direct.rs` then validates hardcoded JSON shapes/rules and emits a fixed `JsonSink` event API.
- `runtime/src/grammars/json/generated.rs` emits direct events as source strings and scalar values (`key_source`, `string_source`, `object_string_source`, `array_string_source`, numeric/bool/null context hooks).
- `runtime/src/grammars/json/sink.rs` decodes escaped strings inside default source hooks before calling semantic sink methods.
- `bbnf-bench/src/direct_struct.rs` measures a digest-only "direct-to-struct" representation, not an actual typed representation. Equality is Track 1 digest == Track 2 digest and same shape as serde/sonic; full digest fingerprint equality to serde/sonic is not required.

The next admissible intervention is therefore not another string materializer. It is to make DirectBuild carry enough field facts to emit a typed/direct fact representation whose equality oracle does not require materializing every decoded string into a contiguous `&str` just to hash it.

## Recommended Intervention

Implement a DirectBuild field-facts representation for JSON direct output.

Owner files:

1. `skinny/crates/ir/src/lib.rs`
   - Enrich `DirectBuildField` / `DirectBuildSource` without adding a new BIR variant.
   - Add field source detail for scalar semantic facts: span kind, span flags (`needs_unescape`), literal-choice const value, child/repetition occurrence, and materializer kind.

2. `skinny/crates/passes/src/lib.rs`
   - Replace JSON rule-name switches in `direct_fields_for_rule` with supplied direct-build facts.
   - For skinny JSON, a small in-code or fixture-loaded fact table is acceptable as the first step; the key is that extraction consumes facts instead of deriving lossy fields from rule names.

3. `skinny/crates/codegen/src/lower/sink_only.rs`
   - Preserve the enriched field facts into `SinkOnlyProgram`.
   - Keep this BIR-only; do not inspect Grammar IR or JSON names here.

4. `skinny/crates/codegen/src/json_sink_direct.rs`
   - Use enriched field facts to emit representation-level direct output hooks/fact builders.
   - Stop treating `JsonString` as only `{ raw span, needs_unescape } -> source hook -> decoded &str`.
   - Emit a direct fact for strings such as `{ semantic_len, semantic_hash/fingerprint, escaped/plain classification }` where the materializer is specified by DirectBuild facts.

5. `skinny/crates/runtime/src/grammars/json/generated.rs`
   - Generated direct parser should call fact-level output for string/key fields when the requested direct representation is the benchmark fact representation.
   - It may still use existing `parse_string_direct` for validation/span discovery; the change is what DirectBuild says must be produced.

6. `skinny/crates/runtime/src/grammars/json/sink.rs`
   - Add a representation-level sink interface only if needed by generated output.
   - Do not add another default source hook shortcut that simply wraps `unescape_json_string`.

7. `skinny/crates/bbnf-bench/src/direct_struct.rs`
   - Replace or extend `JsonDigestSink` with a strict direct fact sink.
   - Track 1, Track 2, serde_json, and sonic-rs must compute the same semantic fact representation: counts, depth, number classes, key/string semantic byte length, and semantic content fingerprint.
   - For serde/sonic, compute the same fact from their already-decoded string views; for Track 1/Track 2, generated/hand parsers compute the fact from DirectBuild-declared materializers.

## Why This Preserves Strict Semantic Equality

The equality contract must be over semantic JSON values, not over raw source bytes:

- Plain strings and escaped strings that decode to the same Unicode scalar sequence must produce the same semantic length and fingerprint.
- Surrogate pairs, simple escapes, control rejection, invalid UTF-8 rejection, and number width classes remain validated by the existing parser/scanner/materializer rules.
- The direct fact representation is compared across four independent sources: generated Track 1, hand Track 2, serde_json, and sonic-rs.
- Retained parse output remains untouched: raw offset tape plus flags and lazy view decode. This avoids introducing a parallel retained projection.

This is strict if the fact representation is collision-resistant enough for the benchmark gate only insofar as the current digest gate already uses hashes. To avoid weakening the gate, keep all existing shape/count/depth/number fields and use the same `hash_bytes`/`mix` semantics after decoding. The intervention changes where the semantic fact is produced, not what fact is accepted.

## Non-Admissible Recurrences

Do not reopen:

- Direct source-hook receiver folding from REDRESS 66.
- Parser-owned decoded scratch from REDRESS 67.
- Byte-output `unescape_json_string` inside the current `Cow<str>` API from REDRESS 68.
- Sink-local decoded stats from REDRESS 54.
- Quote-source streaming hash from REDRESS 55.
- A new BIR variant, user-facing directive, JSON logic in generic crates, or a parallel retained source pass.

## Falsifiability Gate

Correctness:

- `cargo test -p runtime --profile ax-iter`
- `cargo test -p bbnf-bench --profile ax-iter`
- `cargo run -p xtask --release -- check-json`
- `cargo run -p xtask --release -- check-conformance`
- `assert_direct_struct_parity` must compare Track 1, Track 2, serde_json, and sonic-rs fact output for all direct rows.

Focused direct throughput rows from current `skinny/RESULTS.md`:

| row | current Track 1 Mbps | sonic-rs Mbps | required scout |
|---|---:|---:|---|
| `unicode_escapes` | 5143 | 14485 | >= +15% |
| `unicode_mixed` | 3881 | 10142 | >= +10% |
| `y_string_unicode` | 3674 | 8676 | >= +8% or attribution proof |
| `distinct_values` | 6072 | 13185 | >= +8% |
| `gsoc-2018` | 15013 | 24163 | >= +5% |

Guard direct rows:

- `twitter`, `apache_builds`, `github_events`, `update_center`, `random`, `unicode_basic`, `canada`, `numbers`, `citm_catalog`, `mesh`, `marine_ik`, `instruments`.
- No guard direct row may regress by more than 5%; no already passing direct row may lose PASS status.

Profile gate:

- On `unicode_escapes` and `unicode_mixed`, `parse_string_direct + unescape_json_string` combined self share must drop at least 15% relative, or the new DirectBuild fact materializer must appear as the named replacement cost and still deliver the throughput scout.
- On `distinct_values` and `gsoc-2018`, receiver/fold closure self share must drop at least 25% relative without reintroducing REDRESS 66 source hooks.

Close decision:

- If the scout passes, rerun the broader Wave 3 close thresholds from §9/REDRESS 66 lineage.
- If it fails, record a new REDRESS rejection and stop reopening direct string materialization under the current digest workload.

## Concise Recommendation

Dispatch a DirectBuild field-facts intervention, not a decoder intervention. The smallest credible next patch enriches `DirectBuildField`/`DirectBuildSource`, feeds JSON direct-build facts through `passes` and `sink_only`, and changes the direct benchmark output to a strict semantic fact representation that Track 1/Track 2/serde/sonic all compute. This is the next admissible Wave 3 route because REDRESS 66-68 falsified receiver, parser-owned allocation, and local byte-writer variants while leaving the representation contract itself unchanged.
