# SK-V12 W4 CHALLENGE V2 - CH5 Hidden Coupling

Verdict: REVISE.

The five orphan dispositions are truthful against the live tree, but PLAN-V2's
required orphan artifact is not quite hard enough.

Required revision: make `orphan-disposition.md` require per-row:

- `orphan_status`
- explicit `consumer_path` or `no-production-consumer`
- `lock16_status`
- `redress_entry`
- source grep evidence
- test/checkasm evidence
- REDRESS adjacency
- material differential
- final disposition

The final section must still compute `orphan_count=0`.

Truths carried forward:

- `bitmap_prefix_xor_64` is production-reachable through JSON scan via
  `prefix_xor_64`, but the aarch64 body delegates to scalar.
- `bulk_emit_positions_64` is production-reachable through `compact_mask`, but
  the aarch64 body delegates to scalar.
- `bitmap_next_set_bit` has a dispatch/public surface but no non-test
  production caller found.
- `byte_context` and `cache_hints` are support/test-only.
- The selected `find_ascii_set_member64` candidate is separate from the
  five-row orphan set.
