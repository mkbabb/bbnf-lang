# SK-V12 S-P3 PIN-V4 CH5 Hidden-Coupling Lens

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V4.
Lens: CH5 hidden coupling.
Date: 2026-05-20.
Packet under review: commit `471bf53e`.

## Disposition

ACCEPT.

Confidence: 93%.

CH5 finds no hidden-coupling regression in the PIN-V4 confirmation packet. The
commit is label-only against the PIN-V3 packet: `SPEC.md`, `DISPATCH-PROMPT.md`,
and the six P3 research files change `PIN-V3` to `PIN-V4` with no material
topology, gate, owner-path, or close-condition change. The prior clean PIN-V3
CH5 disposition therefore carries, and the source cross-check below did not find
a new public substrate, outcome, BackendShape, directive, fallback, or orphan
escape hatch introduced by this packet.

## Findings

### 1. W2 / W1b-1 coupling is explicit and fail-closed

No hidden coupling regression.

- `SPEC.md:243-245` sequences W2 as the `escape_mask_64` correctness
  prerequisite and W1b-1 as scalar-only unless W2 has passed.
- `SPEC.md:410-423` makes W1b-1 entry conditional on W1a plus W2 PASS unless the
  plan proves the entire wave is scalar-only and avoids `bbnf-simd`, aarch64
  modules, and ASM-backed helpers.
- `DISPATCH-PROMPT.md:80-83` repeats the same rule: W2 is before any new
  SIMD/ASM admission, while W1b-1 may precede W2 only under an accepted
  scalar-only plan.
- `p3c-falsifiability-gates.md:202-228` makes W1b-1 a scaffold/equality gate
  only and forbids CSS ADMIT there; `p3c-falsifiability-gates.md:271-290`
  makes W2 correctness-only with no throughput row credit.

The live caller confirms why this is load-bearing: JSON aarch64 scan uses
`escape_mask_64` in the hot scanner (`skinny/crates/runtime/src/grammars/json/scan.rs:200-239`),
while `escape_mask_64` itself carries state handoff in
`skinny/crates/bbnf-simd/src/lib.rs:175-205`. The packet does not let W1b-1
silently consume this path before W2.

### 2. Shared report/gate/runtime race risk is called out and serialized

No hidden coupling regression.

- `p3b-wave-sequencing.md:97-102` explicitly states that W3 and W4 are
  topologically independent only after W1b-2/W2, but should be run serially to
  avoid shared generated-runtime, bench, and gate-file races.
- `DISPATCH-PROMPT.md:88-93` requires research, plan, CHALLENGE, and redress as
  distinct dispatches and commits, with the redress agent editing only SPEC owner
  paths.
- `SPEC.md:452-461`, `SPEC.md:501-511`, and `SPEC.md:548-561` show the shared
  W1b-2/W3/W4 report/gate/bench/runtime paths are named, not implicit.
- `SPEC.md:618-621` makes W5 wait on W4 and on W3 when FIXPOINT requires it,
  preventing close from racing ahead of the shared-path dispositions.

### 3. Substrate cardinality remains one, with no public substrate escape

No hidden coupling regression.

- `SPEC.md:220-227` blocks new directives, BIR variants, BackendShape variants,
  public substrate APIs, parser-owned sidecars, decoded-byte sidecars, and any
  primitive/substrate without scalar/reference, parity, proof, and same-wave
  consumer.
- `SPEC.md:519-525` constrains W3 to no sidecar substrate, no parser-owned
  cursor/list, no parallel `UnionTape`, no retained decoded-byte/class side
  vector, and a single-substrate same-tape CSS-local projection.
- `p3d-telemetry-schema.md:220-237` requires union/ASM attempts to report
  `substrate_cardinality` as one and `public_api_delta` as no new public
  substrate API.
- Current source still has only the five established backend shapes in
  `skinny/crates/ir/src/lib.rs:401-408` and `skinny/crates/ir/src/cost.rs:127-135`.
  The Lock 14 baseline validator rejects BackendShape count drift and `UnionTape`
  in the IR surface at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:565-594`.
- Current grammar parsing accepts only the existing skinny directives `import`
  and `token`; other directives fail at
  `skinny/crates/grammar/src/lib.rs:80-98`.

The live tape substrate is still a single `Tape` containing source, offsets,
flag side tables, payload arena, and id (`skinny/crates/runtime/src/tape/mod.rs:94-120`),
constructed by `TapeBuilder` from one offsets vector plus flag/payload metadata
(`skinny/crates/runtime/src/tape/assembler.rs:42-123`). PIN-V4 adds no source
escape hatch around that substrate.

### 4. Zero-orphan accounting is carried as a hard close blocker

No hidden coupling regression.

- USER PIN D5 names the five carried orphans and requires zero orphan kernels by
  SK-V12 close (`USER-PIN-W1-CSS-L4-SOTA.md:71-78`).
- `SPEC.md:58-60`, `SPEC.md:80-81`, and `SPEC.md:585-588` carry the same five
  rows into ADMIT, FIXPOINT, and W4 accounting.
- `p3d-telemetry-schema.md:239-255` requires each orphan to be `consumed`,
  `removed`, or `inventory_demoted`; `open` fails ADMIT and FIXPOINT close.
- The SIMD coverage audit confirms the carried orphan count is five and names
  the exact wrappers/support modules (`skv12-aarch64-simd-coverage-audit.md:34-61`,
  `skv12-aarch64-simd-coverage-audit.md:191-199`).
- Live source matches the audit: three wrappers are public dispatch fields
  (`skinny/crates/bbnf-simd/src/dispatch.rs:49-55`) and aarch64 dispatch points
  (`skinny/crates/bbnf-simd/src/dispatch.rs:63-74`), but their aarch64 bodies
  delegate to scalar in
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`,
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`, and
  `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`.
  `byte_context` and `cache_hints` remain standalone aarch64 support modules at
  `skinny/crates/bbnf-simd/src/aarch64/byte_context.rs:1-10` and
  `skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:1-33`.

These are existing residuals, not a PIN-V4 regression, and W4/W5 cannot close
while any remains open.

### 5. Same-wave consumer coupling is explicit

No hidden coupling regression.

- `SPEC.md:169-172` rejects any emitted CSS/non-JSON field not consumed by
  `gate-json` or the named same-wave companion gate.
- `SPEC.md:225-227` requires every primitive, SIMD/ASM kernel, parse-that helper,
  generated path, substrate, or output-plane contract to have scalar/reference,
  parity/checkasm where applicable, same-host proof, and same-wave consumer.
- `p3d-telemetry-schema.md:164-183` requires `same_wave_consumer_class`,
  scalar-reference, parity, microbench, feature fallback, and
  `escape_mask_64` status for SIMD/ASM fields.
- The current report/gate code already rejects producer-only patterns in several
  places: SK-V12 non-JSON rows require companion-gate consumer classes at
  `skinny/crates/bbnf-bench/src/report.rs:2007-2020`, and W1a schema rows reject
  admission while requiring schema-only consumer evidence at
  `skinny/crates/bbnf-bench/src/report.rs:2070-2104`.

Residual source note: `skinny/crates/bbnf-bench/src/report.rs:1987-2000` still
contains an old intervention threshold branch based on `ceil(baseline * 1.01)`.
The PIN-V4 packet already identifies this as future CSS gate work, not an
admissible close path: `p3a-candidate-shortlist.md:93-100`, `SPEC.md:85-87`,
and `SPEC.md:217-218` all block the stale formula for CSS admission.

### 6. Fallback remains W1b-2-only

No hidden coupling regression.

- USER PIN D1 requires Sheets/BBNF-self fallback only after a CSS L4 redress
  attempt, not after preflight failure (`USER-PIN-W1-CSS-L4-SOTA.md:18-24`).
- `SPEC.md:438-442` states W1b-1 scaffold failure does not satisfy the
  post-CSS-redress fallback condition; fallback stays blocked until W1b-2 records
  measured CSS lightningcss comparator/admission redress.
- `SPEC.md:488-491` repeats that W1b-2 BLOCKED/FAIL records measured CSS redress
  and any Sheets/BBNF fallback requires later S-P3 or wave-plan revision.
- `p3b-wave-sequencing.md:104-110` explicitly refuses to hide a Sheets/BBNF
  fallback inside CSS redress and requires a new explicit fallback wave after
  W1b-2 measured CSS evidence.
- `p3c-falsifiability-gates.md:258-264` forbids hidden fallback inside W1b-2
  redress and allows Sheets/BBNF-self only after CSS L4 redress is recorded.

This resolves the prior PIN-V2 ambiguity captured in the PIN-V3 consolidated
record (`research/p3/hardening/PIN-V3/CONSOLIDATED.md:31-34`) and carries it
unchanged into PIN-V4.

## Required Fixes

None for S-P3 PIN-V4.

Residual implementation work remains intentionally routed to later waves:

- W1b-2 must replace or bypass the old non-JSON baseline-relative report branch
  for CSS admission and consume `lightningcss_mbps + 1` instead.
- W4/W5 must dispose the five aarch64 orphan rows by consumption, removal, or
  inventory demotion with evidence.
- W1a must land the `GrammarConfig` or equivalent generated metadata surface
  before any CSS L4 emission is legal.

These are wave obligations already captured by the packet, not confirmation
cycle blockers.

## CH5 Verdict

PASS.
