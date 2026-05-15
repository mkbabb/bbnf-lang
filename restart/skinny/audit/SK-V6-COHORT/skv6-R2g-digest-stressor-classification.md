# SK-V6 Wave 3 R2g: Direct Digest Stressor Classification

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only research. No repository files edited.

## Decision

Current `direct_to_struct` digest should be retained separately as a SOTA
stressor, not treated as the representative DirectBuild closure gate.

It is correctness-green and still valuable: generated Track 1, independent
hand Track 2, serde_json, and sonic-rs all exercise strict JSON semantics over
the same fixtures, and the digest catches shape/count/number/string semantic
mistakes. But it is not representative of most DirectBuild typed-struct
closures because it requires a global semantic digest over every key and every
string scalar. That workload forces all string bytes through decoded semantic
length/fingerprint work even when a real typed output would usually project
only selected fields, borrow plain spans, parse a few scalar fields, or skip
unknown fields.

The direct digest should remain in the benchmark matrix under an explicit
output-plane label such as `semantic_full_digest_stressor`. A separate
DirectBuild closure gate should be added for real typed projections with
field-specific access patterns.

## Required Reading Anchors

- `skinny/REDRESS.md` lines 54-55: Canada structural-only scan is now above the
  floor; remaining retained misses are runtime/materialization and event/tape
  consumption gaps, not scanner-floor failure.
- `skinny/REDRESS.md` lines 66-69: the prior direct summary shows only
  `marine_ik` and `numbers` clearly over sonic-rs, while Unicode rows remain
  severe misses.
- `skinny/RESULTS.md` direct rows: current output plane is
  `generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs
  typed serde`; current PASS rows are `citm_catalog`, `marine_ik`, and
  `numbers`; the rest are N-direct under 1.10x sonic time slack.
- REDRESS 54/55: sink-local exact decoded-string stats and quote-source fused
  streaming hash were rejected.
- REDRESS 66-69: source-hook receiver folding, parser-owned scratch,
  byte-output `unescape_json_string`, and DirectBuild semantic string fact
  hashing were all rejected on the generated Track 1 baseline.

## Why Sonic-rs Wins Here

Sonic-rs wins because this benchmark gives it a favorable direct typed-serde
plane while making BBNF pay a generic generated SinkOnly event plane plus a
synthetic full-digest consumer.

The BBNF Track 1 path is:

- generated `runtime::generated_json::parse_direct`;
- `ParsedString { raw, needs_unescape }`;
- `JsonSink::*_source`;
- default escaped decode through `unescape_json_string`;
- `JsonDigestSink` folding every semantic key/string byte into counts and a
  fingerprint.

The sonic-rs comparator is `sonic_rs::from_slice::<JsonDirectDigest>(bytes)`.
It drives serde directly into the digest representation. Its parser has
vectorized/branch-light string classification, borrowed or in-place plain
string paths, and direct visitor dispatch; it is not first producing a retained
offset tape or generic SinkOnly event API.

The profile evidence matches that shape:

- `unicode_escapes`: about 47% self in `unescape_json_string` and 44% in
  `parse_string_direct` in R4c; R3 reported the same split.
- `unicode_mixed`: about 49-51% `parse_string_direct`, 23% `unescape_json_string`,
  plus visible copy/memmove.
- `distinct_values`: no escape decode, but `parse_object_direct`,
  `parse_string_direct`, receiver/fold, and tiny-string matching dominate.
- `gsoc-2018`: large string scan plus `fold_string_scalar`, with some escaped
  decode.

So the miss is not a single bad helper. It is the full semantic digest output
contract: every string/key must become semantic bytes and be hashed, and the
BBNF path reaches that through a generic sink event representation.

## Output Plane Mismatch

The current row name says `direct_to_struct`, but the actual output plane is a
semantic full-document digest. That is stricter and less representative than a
typed struct projection.

The mismatch has three parts:

1. BBNF Track 1/Track 2 output is `SinkOnly digest`, not a concrete typed
   application struct with named fields and access locality.
2. Sonic-rs output is typed serde into the digest type, so its comparator plane
   is a mature direct visitor path rather than a generic DirectBuild event
   surface.
3. The digest intentionally touches all keys and all string scalar bytes. A
   real DirectBuild closure gate should distinguish selected-field projection,
   skipped unknown fields, borrowed plain strings, owned escaped strings, and
   typed numeric/literal fields.

This is why Candidate 10 is decisive. It made the DirectBuild semantic string
fact route architecturally cleaner, but still regressed `unicode_escapes` by
roughly 15%. That says the current digest representation is itself the stressor:
replacing decoded contiguous hashing with one-pass semantic fact hashing did
not close it.

## Spec Changes Needed

1. Split the benchmark taxonomy.

   Keep the current digest as `semantic_full_digest_stressor` or equivalent.
   Do not use it as the sole DirectBuild closure gate.

2. Define a representative DirectBuild closure gate.

   Add typed projection workloads where the target has named fields,
   field-specific materializers, skip/ignore behavior, and explicit ownership:
   `BorrowedStr`, `OwnedDecodedStr`, numeric primitives, literals, optional
   fields, repeated fields, and nested structs.

3. Clarify comparator output planes.

   Report BBNF generated DirectBuild projection, BBNF hand/reference projection,
   sonic-rs serde projection, and serde_json projection as separate planes.
   Digest stressor rows should say that they are all-fields semantic digest
   rows, not ordinary direct-to-struct rows.

4. Preserve strict equality without requiring every gate to hash every byte.

   Representative typed gates should compare actual typed field values and
   selected ownership/materialization behavior. The digest stressor can keep
   shape/count/depth/number/string semantic fingerprint equality as its own
   high-stress oracle.

5. Make DirectBuild field facts grammar-neutral.

   Keep the existing `DirectBuild { shape, fields }` BIR hook, but field facts
   need resolved rule/field/source/materializer ids rather than JSON rule-name
   switches. This is still needed for real typed projections, even though the
   digest-specific semantic fact hashing route failed.

## Next Route

Build one small real typed projection benchmark, then compare it against the
retained digest stressor rather than trying another digest close.

Recommended route:

- Add a generated DirectBuild typed projection for a field-selective JSON shape
  with mixed strings, numbers, bool/null, arrays, and nested objects.
- Include at least one corpus where most keys/strings are skipped, one where a
  few plain strings are borrowed, and one where escaped strings must be owned.
- Run generated Track 1, independent Track 2, sonic-rs serde, and serde_json on
  the same typed output.
- Keep the existing digest rows unchanged as stressor guard rows.

Suggested first benchmark shape:

- `github_events_typed_projection`: select stable fields such as event id/type,
  actor id/login, repo id/name, public flag, and a small nested payload subset;
  ignore the rest.
- Add a synthetic companion with escaped selected strings so owned decoding is
  still tested without forcing a digest over every key and string in the file.

Falsification:

- If BBNF still loses badly on selected-field typed projection, the DirectBuild
  implementation remains the closure blocker.
- If BBNF is competitive on typed projection but still loses the digest rows,
  the classification is confirmed: digest is a retained SOTA stressor, not the
  representative DirectBuild closure gate.

## Bottom Line

Stop trying to close SK-V6 by specializing the current digest path. REDRESS
54/55/66/67/68/69 have exhausted the local sink/materializer/fact-hash family
for this output contract. The next useful move is to split the spec: real
typed DirectBuild projection for closure, semantic full-document digest for
SOTA stress.
