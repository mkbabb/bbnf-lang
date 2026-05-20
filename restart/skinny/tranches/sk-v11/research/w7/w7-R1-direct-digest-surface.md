# SK-V11 W7R1: Direct Digest Scalar Source And Host-Sink Surface

Date: 2026-05-20.
Scope: W7 Phase 1 research R1 - direct digest scalar source, current output
contract, host-sink-only surface, and REDRESS 54 / W6 pre-blocks.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. Phase 1 is read-only. The triumvirate contract requires each research agent to
   produce one artifact under `restart/skinny/tranches/sk-v{N}/research/` and
   make no source edits (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11`,
   `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:13`,
   `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:194`). W7 is SPEC
   Section 11, C8 only: output digest/hash oracle or per-product host sink
   (`restart/skinny/tranches/sk-v11/SPEC.md:648`,
   `restart/skinny/tranches/sk-v11/SPEC.md:650`). Its owner set is the bench
   direct/typed host surface plus report/gate/RESULTS/REDRESS, not parser
   semantics (`restart/skinny/tranches/sk-v11/SPEC.md:652` through
   `restart/skinny/tranches/sk-v11/SPEC.md:661`). The entry gate requires a
   fresh CHALLENGE-accepted claim that output digest/hash is the limiting hot
   leaf and that the plan names the exact scalar fold/mix source, output plane,
   and independent oracle (`restart/skinny/tranches/sk-v11/SPEC.md:663` through
   `restart/skinny/tranches/sk-v11/SPEC.md:665`).

   The S-P2/P3 C8 boundary says the same thing in candidate language: C8 is tied
   to the S-P1 `output_digest_hash` hot leaf but only as benchmark/oracle or
   per-product host-sink work, not parser vocabulary
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:39`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:55`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:67`).
   P2-B's `OUTPUT_DIGEST_HASH_ORACLE` row also says no parser row movement claim
   follows from digest-only speed and that promotion requires a same-wave
   report/gate or product host consumer using the digest update
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:275`).

2. The current direct output payload is `JsonDirectDigest`: ten counters plus
   `max_depth` and `fingerprint` (`skinny/crates/bbnf-bench/src/direct_struct.rs:16`
   through `skinny/crates/bbnf-bench/src/direct_struct.rs:28`). The scalar string
   contract is:

   - Root string: `JsonDirectDigest::string` sets `strings = 1`,
     `string_bytes = value.len()`, and `fingerprint = mix(0x53,
     hash_bytes(value.as_bytes()))` through `Self::scalar`
     (`skinny/crates/bbnf-bench/src/direct_struct.rs:48` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:63`).
   - Nested string: `fold_string_scalar` increments `strings`, adds decoded byte
     length, bumps `max_depth` to at least 2, and folds
     `mix(0x53, hash_bytes(value.as_bytes()))`
     (`skinny/crates/bbnf-bench/src/direct_struct.rs:123` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:127`).
   - Object key: `fold_key` adds key decoded byte length and folds
     `hash_bytes(key.as_bytes())`; member count is incremented by the sink before
     this helper is called (`skinny/crates/bbnf-bench/src/direct_struct.rs:184`
     through `skinny/crates/bbnf-bench/src/direct_struct.rs:186`,
     `skinny/crates/bbnf-bench/src/direct_struct.rs:300` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:307`).

3. The current scalar fold/mix/hash sources are exact and local:

   - `fold_number_scalar` folds `mix(tag, value)` with tags `0x4e49`,
     `0x4e55`, and `0x4e46` for i64/u64/f64; bool and null fold tags `0x42`
     and `0x30` (`skinny/crates/bbnf-bench/src/direct_struct.rs:67` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:84`,
     `skinny/crates/bbnf-bench/src/direct_struct.rs:140` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:164`).
   - `fold_child` merges all counters, sets parent depth to
     `max(child.max_depth + 1)`, and folds the child fingerprint
     (`skinny/crates/bbnf-bench/src/direct_struct.rs:168` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:180`).
   - Object and array frame fingerprints start at `0x7b` and `0x5b`, then fold
     final member/element count at close (`skinny/crates/bbnf-bench/src/direct_struct.rs:261`
     through `skinny/crates/bbnf-bench/src/direct_struct.rs:296`,
     `skinny/crates/bbnf-bench/src/direct_struct.rs:486` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:509`,
     `skinny/crates/bbnf-bench/src/direct_struct.rs:517` through
     `skinny/crates/bbnf-bench/src/direct_struct.rs:536`).
   - `hash_bytes` seeds `0xcbf29ce484222325 ^ len`, folds exact 8-byte
     little-endian chunks through `mix`, folds a little-endian tail word when
     present, then folds length again (`skinny/crates/bbnf-bench/src/direct_struct.rs:717`
     through `skinny/crates/bbnf-bench/src/direct_struct.rs:733`).
   - `mix(seed, value)` is `seed ^ (value + 0x9e3779b97f4a7c15 + (seed << 6) +
     (seed >> 2))` with wrapping adds (`skinny/crates/bbnf-bench/src/direct_struct.rs:737`
     through `skinny/crates/bbnf-bench/src/direct_struct.rs:742`).

4. Track 1 and Track 2 share the same direct digest output type but use separate
   parsers. Track 1 calls `runtime::generated_json::parse_direct(input, &mut
   JsonDigestSink)` (`skinny/crates/bbnf-bench/src/direct_struct.rs:401` through
   `skinny/crates/bbnf-bench/src/direct_struct.rs:405`). Track 2 calls the local
   hand parser (`skinny/crates/bbnf-bench/src/direct_struct.rs:408` through
   `skinny/crates/bbnf-bench/src/direct_struct.rs:410`). The generated direct
   parser routes string values through the four source methods
   `string_source`, `object_string_source`, `array_string_source`, and
   `key_source` (`skinny/crates/runtime/src/grammars/json/generated.rs:440`
   through `skinny/crates/runtime/src/grammars/json/generated.rs:443`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:480` through
   `skinny/crates/runtime/src/grammars/json/generated.rs:483`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:520` through
   `skinny/crates/runtime/src/grammars/json/generated.rs:523`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:563`). `JsonDigestSink`
   currently does not override those source methods; it implements only decoded
   semantic sink methods such as `key`, `string`, `array_string`, and
   `object_string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:259` through
   `skinny/crates/bbnf-bench/src/direct_struct.rs:399`). Therefore escaped
   Track 1 strings use the grammar-local `JsonSink` defaults, which allocate via
   `unescape_string(raw)?` before calling decoded sink methods
   (`skinny/crates/runtime/src/grammars/json/sink.rs:16` through
   `skinny/crates/runtime/src/grammars/json/sink.rs:35`,
   `skinny/crates/runtime/src/grammars/json/sink.rs:44` through
   `skinny/crates/runtime/src/grammars/json/sink.rs:52`,
   `skinny/crates/runtime/src/grammars/json/sink.rs:85` through
   `skinny/crates/runtime/src/grammars/json/sink.rs:93`).

5. Track 2's escaped-string path is independent parser control but not an
   independent digest algorithm. Its string helper fast-borrows tiny plain
   strings, otherwise calls `match_string_at_quote_trusted_utf8`, slices raw
   content, advances to `span.raw_end`, and calls `unescape_string(raw)` when
   `span.needs_decode()` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`
   through `skinny/crates/bbnf-bench/src/direct_struct.rs:560`). It then feeds the
   same digest helpers: root strings use `JsonDirectDigest::string`, object keys
   use `fold_key`, and array/object children fold through `fold_child`
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:465` through
   `skinny/crates/bbnf-bench/src/direct_struct.rs:499`,
   `skinny/crates/bbnf-bench/src/direct_struct.rs:514` through
   `skinny/crates/bbnf-bench/src/direct_struct.rs:536`).

6. The current correctness contract is stronger between Track 1 and Track 2 than
   against serde/sonic. `assert_direct_struct_parity` requires exact
   `JsonDirectDigest` equality for Track 1 and Track 2, then only
   `same_shape_as` for serde_json and sonic-rs (`skinny/crates/bbnf-bench/src/direct_struct.rs:420`
   through `skinny/crates/bbnf-bench/src/direct_struct.rs:425`). `same_shape_as`
   compares all counters and `max_depth` but does not compare `fingerprint`
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:190` through
   `skinny/crates/bbnf-bench/src/direct_struct.rs:202`). The report/gate layer
   then requires direct row movement to stay on the `digest` output plane with
   strict measured-row validation, complete escape validation,
   `independent_verified` Track 2 status, non-gate-only consumer, and same-run
   direct comparator evidence (`skinny/crates/bbnf-bench/src/report.rs:1085`
   through `skinny/crates/bbnf-bench/src/report.rs:1145`,
   `skinny/crates/bbnf-bench/src/report.rs:1676` through
   `skinny/crates/bbnf-bench/src/report.rs:1683`). Gate rendering likewise marks
   direct output as `digest` and describes the direct row as generated Track 1
   SinkOnly versus independent hand Track 2 SinkOnly
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:198` through
   `skinny/crates/bbnf-bench/src/bin/gate.rs:220`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:576` through
   `skinny/crates/bbnf-bench/src/bin/gate.rs:624`).

7. `generated_real_typed.rs` is not a direct digest sink. It is generated typed
   product code over host/API types. String fields use `Cow<'i, str>` via
   `parse_string`; plain strings are borrowed, escaped strings allocate through
   `unescape_string(raw)`, and skipped irrelevant strings only advance the cursor
   (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1198` through
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1204`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1649` through
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1670`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1796` through
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1807`). The typed
   checksum/hash support lives in the typed host layer, not in
   `JsonDirectDigest`: `hash_str` is byte-by-byte and uses the same `mix` shape
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:687` through
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:747`). A W7 direct-digest
   claim cannot use typed output evidence as proof; SPEC and gate rules reject
   direct digest as typed proof (`restart/skinny/tranches/sk-v11/SPEC.md:104`
   through `restart/skinny/tranches/sk-v11/SPEC.md:108`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:169` through
   `restart/skinny/tranches/sk-v11/HANDOFF.md:172`).

8. W6 closed the obvious host-sink source-method route before source dispatch.
   W6 CH3 found that overriding `JsonDigestSink::*_source` to fold decoded
   escaped bytes into the current length/fingerprint contract reopens REDRESS 54:
   same sink seam, same output-plane digest fields, same allocation-removal claim
   (`restart/skinny/tranches/sk-v11/research/w6/challenge/w6-CH3-redress-regression.md:17`
   through `restart/skinny/tranches/sk-v11/research/w6/challenge/w6-CH3-redress-regression.md:30`).
   REDRESS 117 records that conclusion as load-bearing and says W7 may proceed
   only through Section 11 while carrying REDRESS 54/55/66/69, 64, 82, 107, 108,
   113, 116, and 117 forward (`skinny/REDRESS.md:3434` through
   `skinny/REDRESS.md:3460`; `restart/skinny/tranches/sk-v11/HANDOFF.md:119`
   through `restart/skinny/tranches/sk-v11/HANDOFF.md:130`).

## §2 — Recommendations (named falsifiability gates)

1. For `G-W7-DIGEST-SINK`, do not select a `JsonDigestSink::*_source`
   decoded-byte fold into the current `JsonDirectDigest` fields. That route is
   not merely risky; it is the W6/REDRESS 117 replay of REDRESS 54.

2. A W7 plan can proceed only after a fresh post-W6 profile still names
   `output_digest_hash` as limiting on a bounded row subset, as required by SPEC
   Section 11 (`restart/skinny/tranches/sk-v11/SPEC.md:674` through
   `restart/skinny/tranches/sk-v11/SPEC.md:679`). If the hot leaves are still
   `full string`, `unescape`, `validate escape`, `dispatch`, or numeric parser
   work, W7 should record BLOCKED/REJECT rather than forcing a digest-sink patch.

3. The only JSON direct host-sink source delta that appears not immediately
   pre-blocked by REDRESS 54 is a hash-core-only change below the decoded
   `&str` boundary: replace or specialize `hash_bytes` / `mix` for already
   decoded strings, with either exact old fingerprint preservation or an explicit
   same-wave output-contract revision consumed by Track 1, Track 2, serde/sonic
   shape checks, report, and gate. This must not decode raw source, override
   `*_source`, add semantic string facts, add side tables, or claim typed
   admission from direct digest evidence.

4. `generated_real_typed.rs` should be treated as guard/host-product context for
   W7 R1, not a selected direct-digest target. A typed host-sink optimization
   would need its own typed output-plane gate and independent typed oracle; SPEC
   Section 11's exit text names selected direct rows or a selected non-JSON host
   sink, not a new JSON typed-row admission (`restart/skinny/tranches/sk-v11/SPEC.md:671`
   through `restart/skinny/tranches/sk-v11/SPEC.md:679`).

## §3 — Risks (REDRESS entries to pre-block)

1. REDRESS 54 blocks sink-local exact decoded stats/hash over the current direct
   digest workload. It kept the source hook seam, removed escaped-string
   allocation, computed decoded length/hash, passed correctness, and regressed
   escaped rows (`skinny/REDRESS.md:815` through `skinny/REDRESS.md:844`).

2. REDRESS 55 blocks the quote-source fused streaming materializer variant and
   concludes that another sink-local decoded hash path is non-canonical unless a
   future row overturns REDRESS 49/54/55 with a materially different product
   representation (`skinny/REDRESS.md:846` through `skinny/REDRESS.md:882`).

3. REDRESS 66/69 adjacency remains active through W6 CH3: a plan that changes
   escaped source handling but keeps the same current direct digest
   length/fingerprint consumer is still in the rejected family
   (`restart/skinny/tranches/sk-v11/research/w6/challenge/w6-CH3-redress-regression.md:50`
   through `restart/skinny/tranches/sk-v11/research/w6/challenge/w6-CH3-redress-regression.md:71`).

4. REDRESS 107/108 and W6 forbid treating existing `unescape_string` /
   `unescape_uxxxx_x4_neon` reachability as a W7 PASS. W7 is output sink only; x4
   remains proof/background unless a separate future wave supplies a new source
   delta, scalar oracle, strict parity, same-wave consumer, and row gate
   (`restart/skinny/tranches/sk-v11/research/w6/redress/w6-redress-entry-blocked.md:51`
   through `restart/skinny/tranches/sk-v11/research/w6/redress/w6-redress-entry-blocked.md:55`).

5. C8 cannot become parser vocabulary, generic hash semantics, hidden sidecar, or
   Track 1 == Track 2 shared parser. The P3 ledger says C8 is output oracle /
   product host sink only and pre-blocks digest/hash as parser semantics,
   semantic string/hash side tables, cache-hint-only proof, hidden Track 2
   coupling, and output-sink work without fresh limiting profile evidence
   (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:41`,
   `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:56`,
   `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:198`
   through `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:200`).

## §4 — Sources (every external citation)

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/w6/challenge/w6-CH3-redress-regression.md`
- `restart/skinny/tranches/sk-v11/research/w6/redress/w6-redress-entry-blocked.md`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
