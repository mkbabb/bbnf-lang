# SK-V12 S-P2 CHALLENGE V1 — CH1 Correctness

Disposition: REVISE.

Lens: CH1 CORRECTNESS.
Date: 2026-05-20.
Scope: verify that every S-P2 candidate traces to a named S-P1 hot leaf, that SOTA-comparator claims use the correct strictness plane and primary sources, and that ISA claims cite architecture references.

## Basis

The accepted S-P1 antecedent set is the ten-family list in `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:47`: `bounded_plain_string_scan`, `container_dispatch`, `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`, `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`, `typed_direct_projection`, and `serde_json_oracle_read_parse`. A candidate whose mechanism does not actually move one of those leaves is not S-P3-eligible.

## Findings

1. P2-A's strict comparator framing is correct. It excludes asmjson from the strict-anchor lane for published conformance caveats, keeps sonic-rs unchecked/lossy paths out of the strict lane, uses yyjson `YYJSON_READ_NOFLAG` as the strict lane, and treats simdjson's retained structural index as comparator architecture rather than an importable bbnf substrate (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:14`-`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:21`). The comparator source list is primary and commit-pinned for asmjson, sonic-rs, simdjson, and yyjson (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:166`-`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:207`). P2-A's seven candidates each name accepted S-P1 antecedents (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:33`, `:45`, `:57`, `:69`, `:81`, `:93`, `:105`).

2. P2-A has two stale local source paths. The capture-manifest and replay entries omit `research/p1/` and do not resolve at the stated locations (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:157`-`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:158`). The resolving paths are `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md` and `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`.

3. P2-B is CH1-sound. Its twelve process/admission gates trace to accepted hot leaves in the candidate table (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:38`-`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:49`), and its process claims are cited to primary checkasm, FFmpeg, and dav1d sources in §5. Entries that are support-only or oracle-only say so and do not claim row movement from primitive proof alone (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:33`, `:49`, `:51`).

4. P2-C has two speculative candidate entries that must be demoted or rejected before S-P3. `a64_ld4_interleaved_classifier64x4` depends on "a proven existing interleaved byte stream" (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:50`) while admitting no local LD4 scalar oracle and only a `vld1q_u8_x4` local precedent (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:51`). The named antecedents are broad classifier leaves (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:54`), not an S-P1 finding that any hot path already consumes interleaved four-channel input. `a64_sha3_ternary_bool_fold` likewise says it is valid only "when a measured hot leaf actually combines three masks" (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:98`), but the artifact names no such S-P1 source-line expression and admits no local body (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:99`). The ISA citations themselves are primary Arm references (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:139`-`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:147`); the defect is candidate eligibility, not architecture sourcing.

5. P2-D overstates guarded diagnostics as candidate primitives. Its own movement map says offset capacity/reserve is "Not yet" a P1 hot leaf, sparse flag lookup is "Not for current parse/direct hot leaves", and retained cursor-skip is "retained-view traversal only" with no current P1 hot leaf (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:48`-`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:51`). The §2 table still counts three guarded research candidates (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:69`-`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:77`). Under CH1, those entries are diagnostics until fresh P1 evidence names builder capacity, sparse flag lookup, or retained-view traversal as hot. `structural_class_lane_union` is correctly rejected, not merely deferred (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:78`).

6. P2-E is CH1-sound. It explicitly separates the five parse-that candidate leaves from the five non-candidate accepted leaves (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:1`-`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:46`), and each candidate names an accepted S-P1 antecedent before stating scalar reference and consumer requirements (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:85`, `:143`, `:212`, `:235`).

7. P2-F is CH1-sound as a grammar-neutral map. It introduces no new comparator or ISA claims (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:21`), distinguishes conditionally eligible parser/support families from oracle-only and accounting-only families (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:25`), and every listed family names accepted S-P1 antecedents (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:29`-`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:36`).

## Revise List

1. In P2-C, demote `a64_ld4_interleaved_classifier64x4` from candidate primitive to ISA inventory unless the artifact supplies a fresh S-P1 hot-leaf/source-line antecedent for an existing interleaved stream and a scalar deinterleave oracle.

2. In P2-C, demote `a64_sha3_ternary_bool_fold` from candidate primitive to ISA inventory unless the artifact supplies a fresh S-P1 hot-leaf/source-line antecedent for a real three-input boolean fold and an explicit scalar formula.

3. In P2-D, revise the candidate count/table so `offset_tape_capacity_policy`, `sparse_flag_lookup_policy`, and `retained_cursor_skip_projection` are diagnostic/ineligible under SK-V12 S-P1 unless fresh profile evidence names those exact movement loci. Keep `structural_class_lane_union` rejected.

4. In P2-A, correct the two stale S-P1 capture/replay paths to include `research/p1/`.

After these revisions, CH1 has no remaining objection to the comparator strictness plane, primary comparator sources, or Arm ISA source coverage.
