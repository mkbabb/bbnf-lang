# SK-V11 W7 CH1 - Correctness Challenge

Date: 2026-05-20.
Scope: correctness, parity, and oracle review of
`research/w7/w7-plan-output-digest-entry-block.md`.
Disposition: ACCEPT BLOCK.

## Adjudication

The proposed BLOCK before source redress is correct.

W7 is legally only C8 output digest/hash oracle or per-product host sink work.
SPEC Section 11 requires CHALLENGE acceptance that output digest/hash is an
observed limiting hot leaf for a bounded selected product-row subset, with the
exact scalar fold/mix source, output plane, and independent oracle named before
redress. The W7 packet does not establish that entry condition. The available
profile evidence is behavior-equivalent S-P1 triage evidence, not a fresh
post-W6 profile, and it does not identify a bounded residual subset where
`output_digest_hash` can close both Track 1 and Track 2/oracle floors.

The plan is also correct to refuse the apparent escaped-string source-method
route. REDRESS 117 makes CH3 load-bearing: overriding
`JsonDigestSink::*_source` to fold decoded bytes reopens REDRESS 54 because it
uses the same sink seam, the same current `JsonDirectDigest`
length/fingerprint output contract, and the same allocation-removal claim, with
REDRESS 55/66/69 adjacency. Renaming that route as W7 host-sink work would not
make it a new correctness surface.

## Correctness / Parity Findings

The current direct output contract is a digest plane, not a typed plane.
`JsonDirectDigest` carries counters, `max_depth`, and `fingerprint`; Track 1 is
`track1_digest` through `runtime::generated_json::parse_direct(input, &mut
JsonDigestSink)`, and Track 2 is the independent hand parser through
`hand::sink_digest`. Existing parity requires exact Track 1 == Track 2 digest
equality, but only shape equality against serde and sonic. That is sufficient
for the current W0 direct correctness guard, but it is not sufficient oracle
coverage for a W7 hash/fingerprint intervention.

A correctness-safe W7 hash/fold change would need an independently written
reference fold or a new canonical digest oracle consumed by the gate. It would
also need strict raw/decoded boundary fixtures across key, root string, array
string, and object string sites, plus negative malformed-input agreement across
Track 1, Track 2, serde_json, and sonic-rs. The proposed BLOCK correctly
identifies that no such oracle packet exists.

The only source functions inside the remaining direct sink that are not
immediately the W6 source-method replay are `hash_bytes` and `mix` below the
decoded `&str` boundary. Those functions are used by Track 1, Track 2, and the
serde/sonic deserialization visitor in the same module. Changing them without a
separate oracle would couple the proof to the changed helper. Changing them
with a new digest contract would require report/gate consumption, exact
comparator semantics beyond `same_shape_as`, guard proof, and fresh hot-leaf
evidence. None is present in the W7 packet.

## Row Eligibility

No legal REVISE row is available on the present evidence.

- `apache_builds/direct_to_struct` has the strongest digest signal, but it is a
  direct guard row already admitted as `A / GO`, not a residual admission
  target.
- `distinct_values/direct_to_struct` has residual digest/hash signal, but the
  floor gap is larger than the visible digest/support bucket and Track 2 is not
  shown to be digest-limited enough to close.
- `random/direct_to_struct` is near on Track 1, but Track 2 still misses under
  the available digest signal and no independent W7 oracle is named.
- `update_center/direct_to_struct` and `github_events/direct_to_struct` have
  floor gaps too large for the visible digest bucket.
- The unicode residuals are string/escape limited; W6 already blocked the
  decoded source-method fold and W7 cannot recover it.
- The non-JSON route is unavailable because W1b did not admit a generated
  non-JSON Track 1 baseline or digest-output report authority, and REDRESS 113
  carries that block forward.

Therefore there is no exact legal row, source function, consumer, and oracle to
name for REVISE. The nearest lawful source functions would be
`hash_bytes`/`mix` in `skinny/crates/bbnf-bench/src/direct_struct.rs`, consumed
by generated Track 1 through `JsonDigestSink`, but they fail the W7 entry gate:
no fresh post-W6 limiting profile, no row-floor closure math on both tracks,
and no independent exact digest oracle.

## REDRESS 117 / 54 Avoidance

The accepted BLOCK avoids REDRESS 117 and REDRESS 54 by selecting no source
route. It does not override `key_source`, `string_source`,
`array_string_source`, or `object_string_source`; it does not compute decoded
length/hash from raw string segments inside the sink; it does not add hidden
semantic string facts; and it does not claim allocation-removal speedup as an
output-sink admission.

Any REVISE would have to be materially different from that family: a selected
residual row with fresh post-W6 `output_digest_hash` limiting evidence, a
source change below the decoded `&str` boundary or a different product output
representation, generated Track 1 product consumption, independent exact
Track 2/canonical oracle coverage for the changed hash contract, same-run
strict digest-plane comparators, and same-wave gate/report consumption. The W7
research packet does not contain those facts.

## Decision

CH1 accepts the proposed `BLOCK before source redress` from a
correctness/parity/oracle standpoint. W7 should proceed to a REDRESS block with
no source patch and no `skinny/RESULTS.md` movement unless a later governance
step first supplies a fresh profile and a genuinely independent exact digest
oracle for a bounded selected row.
