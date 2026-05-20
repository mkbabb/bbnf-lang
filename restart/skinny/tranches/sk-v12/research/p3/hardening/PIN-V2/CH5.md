# SK-V12 S-P3 PIN-V2 CH5 Hidden Coupling

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V2.
Lens: CH5 hidden coupling.
Reviewed commit: `7316d87b`.
Disposition: ACCEPT.
Confidence: 92%.
CH5 result: PASS.

## Findings

### CH5-1: W2 / W1b-1 SIMD coupling is fail-closed

No defect found. PIN-V2 closes the PIN-V1 hidden-coupling gap where W1b-1 could
have been read as a CSS baseline wave that quietly called a SIMD helper before
`escape_mask_64` was fixed. The final SPEC sequences W2 before W1b-1 in the
manifest (`restart/skinny/tranches/sk-v12/SPEC.md:243`-`245`) and makes W1b-1
entry conditional on W2 unless the accepted plan proves the entire wave is
scalar-only and touches no `bbnf-simd`, aarch64 module, or ASM-backed helper
(`restart/skinny/tranches/sk-v12/SPEC.md:410`-`423`). DISPATCH carries the same
rule: W1b-1 may run before W2 only if scalar-only (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:80`-`83`).

This is load-bearing because the live JSON aarch64 scanner does call
`escape_mask_64` in the fast path and carries the state handoff through
`bs_carry` (`skinny/crates/runtime/src/grammars/json/scan.rs:236`-`263`).
The packet therefore correctly treats W2 as a correctness prerequisite, not a
throughput row mover (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:280`-`290`).

### CH5-2: W1b-2 / W3 / W4 shared-file race is acknowledged and bounded

No blocking defect found. The owner surfaces intentionally overlap: W1b-2 owns
the CSS bench/report/gate/result slice (`restart/skinny/tranches/sk-v12/SPEC.md:450`-`459`),
W3 owns runtime/codegen/generated CSS plus the same bench/report/gate files
(`restart/skinny/tranches/sk-v12/SPEC.md:499`-`509`), and W4 owns SIMD,
parse-that, generated CSS, codegen templates, bench/report/gate, orphan
disposition, RESULTS, and REDRESS (`restart/skinny/tranches/sk-v12/SPEC.md:546`-`559`).

The sequencing packet explicitly identifies the race and tells the orchestrator
to run W3 and W4 serially to avoid shared generated-runtime, bench, and gate-file
races (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:97`-`102`).
DISPATCH also says the order is firm and allows W3 to be routed as not-required
only on an already-admitted CSS path while W4 still disposes orphans
(`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:80`-`86`). The per-wave
protocol keeps redress to one implementation thread per wave and requires only
SPEC owner paths (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:124`-`133`).

Residual risk: an implementation orchestrator that ignores P3-B and dispatches
W3/W4 concurrently would race shared files. That is an execution violation, not
a packet-level CH5 failure.

### CH5-3: Substrate cardinality and public API escape hatches are closed

No defect found. The user pin unblocks the union category, but only as a new
material-differential implementation with scalar/parity evidence, same-wave
consumer, CHALLENGE, and REDRESS citations (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39`-`56`).
PIN-V2 does not turn that into public substrate/API permission: SPEC forbids new
directives, BIR variants, `BackendShape` variants, public substrate APIs,
parser-owned sidecars, decoded-byte sidecars, hidden host schemas, and orphan
generated/substrate paths (`restart/skinny/tranches/sk-v12/SPEC.md:220`-`227`).

W3 entry requires no sidecar substrate, no parser-owned cursor/list, no parallel
`UnionTape`, and no retained decoded-byte/class side vector
(`restart/skinny/tranches/sk-v12/SPEC.md:511`-`518`), and its task is explicitly
a single-substrate same-tape projection local to the CSS row
(`restart/skinny/tranches/sk-v12/SPEC.md:520`-`524`). P3-D makes the telemetry
side fail-closed too: union route `substrate_cardinality` must remain `one` and
`public_api_delta` must be no new public substrate API
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:226`-`237`).

The current source substrate surface is already one retained `Tape` with source,
offsets, sparse flags, payload arena, and id (`skinny/crates/runtime/src/tape/mod.rs:94`-`120`);
`ValueRef` remains tape plus cursor (`skinny/crates/runtime/src/tape/mod.rs:175`-`222`).
No PIN-V2 packet text requires adding a new public substrate surface.

### CH5-4: Zero-orphan accounting is explicit and gate-owned

No defect found. The aarch64 audit names exactly five carried orphans:
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, and `cache_hints` (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34`-`61`).
SPEC makes zero production orphans an ADMIT and FIXPOINT close condition
(`restart/skinny/tranches/sk-v12/SPEC.md:58`-`63`,
`restart/skinny/tranches/sk-v12/SPEC.md:80`-`83`), and W4 entry requires a
five-row accounting table before dispatch (`restart/skinny/tranches/sk-v12/SPEC.md:572`-`575`).
W4 tasks must record all five by consumption, removal, or inventory demotion
with evidence (`restart/skinny/tranches/sk-v12/SPEC.md:583`-`586`).

The source inventory confirms why this matters: the orphan-facing aarch64 modules
are publicly present (`skinny/crates/bbnf-simd/src/aarch64/mod.rs:1`-`14`), and
the bitmap wrappers still delegate to scalar bodies
(`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`-`4`,
`skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`-`4`,
`skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`-`4`), while
`byte_context` and `cache_hints` expose support-only helpers
(`skinny/crates/bbnf-simd/src/aarch64/byte_context.rs:1`-`10`,
`skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:1`-`32`). P3-D closes the
telemetry loop by requiring each orphan row to be `consumed`, `removed`, or
`inventory_demoted`, with `open` failing ADMIT/FIXPOINT close
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:239`-`255`).

### CH5-5: Same-wave consumer coupling is preserved

No defect found. The packet consistently rejects producer-only primitives,
generated paths, substrate changes, and telemetry. SPEC makes scalar reference,
parity/checkasm where applicable, same-host micro-proof, and same-wave consumer
mandatory for any primitive, SIMD/ASM kernel, parse-that helper, generated path,
substrate, or output-plane contract (`restart/skinny/tranches/sk-v12/SPEC.md:225`-`227`).
DISPATCH requires every primitive or generated parser path to include the
same-wave consumer and same-wave gate (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:130`-`133`).

W3 wires the CSS generated consumer in the same commit
(`restart/skinny/tranches/sk-v12/SPEC.md:520`-`524`). W4 wires the selected
primitive into a same-wave CSS generated consumer or JSON-guard consumer
(`restart/skinny/tranches/sk-v12/SPEC.md:577`-`582`). P3-D rejects
`harness_only` and `producer_only` SIMD/ASM consumers
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:166`-`183`)
and requires union/ASM `consumer_path` plus attempt status in the same route
record (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:220`-`237`).

## Required Fixes

None.

## Residual Risk

- W3/W4 shared owner paths remain high-risk during redress. The packet handles
  this through firm ordering and P3-B's serial-dispatch instruction; the
  implementation orchestrator must follow it.
- The existing SK-V12 non-JSON gate source is still the historical
  `sk-v12-nonjson-generated-v1` baseline/intervention schema and does not yet
  consume lightningcss. That is acceptable at S-P3 because W1b-2 explicitly owns
  the lightningcss comparator and admission-gate update
  (`restart/skinny/tranches/sk-v12/SPEC.md:470`-`489`).
