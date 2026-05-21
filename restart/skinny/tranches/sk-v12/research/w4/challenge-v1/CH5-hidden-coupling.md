# SK-V12 W4 CHALLENGE V1 - CH5 Hidden Coupling

Verdict: REVISE.

The five orphan dispositions are truthful against the live code, but the
selected candidate contract is not yet precise enough.

## Findings

1. PLAN-V1 selects `a64_ascii_set_run_skip` and then defines a delimiter
   finder that advances while bytes are not in `{`, `;`, or `}`. A2/A5 frame
   the same candidate as layout/trivia run-skip that advances while bytes are
   in the layout set. The generated runtime has separate sites for
   `scan_block` delimiter handling and `skip_ws_and_comments`. PLAN-V2 must
   name one consumer, scalar reference, checkasm, and microbench shape.

2. The orphan table is defensible:
   `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, and `bulk_emit_positions_64`
   are scalar delegates on aarch64; `bulk_emit_positions_64` is
   production-consumed through the scalar-delegate compact path; `byte_context`
   and `cache_hints` have support/test reachability only.

3. Consuming `byte_class_from_eq_set_64` does not reduce or increase the
   five-orphan count. It can satisfy the selected primitive's Lock 16 and
   same-wave-consumer requirement, but orphan accounting remains separate.

4. Shared-file coupling with W3/W5 must be explicit. W3 and W4 both own CSS
   runtime, bench, report/gate, `RESULTS.md`, and `REDRESS.md`; W5 owns close
   reconciliation. PLAN-V2 must require no concurrent W3 shared-file edits and
   bind W3 as not required on the current ADMIT path.
