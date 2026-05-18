# SK-V9 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V2 post-W0 rerun.
Date: 2026-05-18.
Scope: synthesize P1-A/P1-B/P1-C hot leaves into grammar-neutral attribution.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release/bench profiles with
debug symbols.
Profile tool: `samply 0.13.1` plus presymbolicated sidecars.
Corpus coverage: 17/17 parse-only, 17/17 direct, 4/4 measured real-typed,
17/17 masking/scan profiles.

## §1 - Method

Commands:

```bash
python3 <profile-summary extractor> \
  /tmp/skv9-p1-rerun/profiles \
  > /tmp/skv9-p1-rerun/profile-summary.json
```

The extractor resolves leaf frames through the samply `.syms.json`
`symbol_table` and inline `frames` arrays; it does not trust sparse
`funcTable.name` strings alone.

## §2 - Findings

| Surface | Attribution | Classification |
|---|---|---|
| Parse-only Track 1 | `runtime::generated_json::generated::dispatch_value` at `generated.rs:47` dominates every corpus at 95.6%-99.6% self-time. | fused generated parser dispatch; grammar-neutral but not split enough for primitive design |
| Direct object-heavy rows | `parse_object_value_at_direct::<JsonDigestSink>` at `generated.rs:468` dominates twitter, github_events, gsoc-2018, update_center, unicode_mixed, unicode_basic. | direct sink object projection |
| Direct array/number-heavy rows | `parse_array_element_at_direct::<JsonDigestSink>` at `generated.rs:508` dominates canada, mesh, marine_ik, instruments, numbers, distinct_values. | direct sink array projection plus number materialization |
| Unicode escape direct row | `parse_that_regex::unescape_string` at `parse-that-regex/src/lib.rs:718` is top self-time for `unicode_escapes/direct_to_struct`. | string/unicode materialization |
| Real typed rows | `DirectParser::skip_value`, generated typed parse functions, and scalar numeric vector parsing dominate the four W0-admitted typed rows. | typed direct projection; no new row admission |
| Eager decode probes | hot leaves move into `JsonNodeKind::at_cursor`, `string_body_range`, generated dispatch, iterators, and UTF-8 validation. | view materialization and traversal |
| Structural scan probes | `bbnf_bench::scan::structural_offsets_simd`, `bulk_emit_positions_64_neon`, and `bitmap_prefix_xor_64_neon` dominate. | transient structural scan diagnostic, not substrate |
| Serde alternate plan | serde_json `Value` deserialize, map/sequence access, `skip_to_escape`, `parse_escape`, and `parse_number`. | comparator diagnostic only |

## §3 - Delta vs SK-V8

P1-F owns row deltas. P1-E only maps fresh SK-V9-open profile symbols to
surface classes. No P1-E classification admits rows or changes verdicts.

## §4 - Anomalies + Masking Signals

- P1-A remains too fused: `dispatch_value` is a real symbol, but not a
  primitive-level split. Any S-P2 primitive that claims scan/string/number
  ancestry from P1-A alone is over-attributed.
- P1-B gives more useful direct-row split because generated direct object/array
  functions and `JsonDigestSink` closures survive inlining.
- P1-C shows view materialization costs under eager decode, but those are
  diagnostic non-producers under W0.
- PMU/cycles remain absent, so no hot-leaf percent can be converted into
  cycles-per-byte.

## §5 - Sources

- `/tmp/skv9-p1-rerun/profile-summary.json`
- `/tmp/skv9-p1-rerun/profile-summary-top5.md`
- `p1a-samply-mode-1.md`
- `p1b-samply-mode-2.md`
- `p1c-samply-mode-3.md`
- `p1d-pmu-cycles.md`
