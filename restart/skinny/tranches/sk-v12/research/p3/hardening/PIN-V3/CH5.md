# SK-V12 S-P3 PIN-V3 CH5 Hidden Coupling

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V3.
Lens: CH5 hidden coupling.
Reviewed commit: `4c53119f`.
Disposition: ACCEPT.
Confidence: 93%.
CH5 result: PASS.

## Findings

### CH5-1: W2 and scalar-only W1b-1 coupling is fail-closed

No defect found. PIN-V3 keeps W2 before W1b-1 in the main manifest while still
allowing a scalar-only W1b-1 scaffold if the accepted plan proves it does not
touch `bbnf-simd`, aarch64 modules, or ASM-backed helpers
(`restart/skinny/tranches/sk-v12/SPEC.md:243`-`245`,
`restart/skinny/tranches/sk-v12/SPEC.md:410`-`423`). DISPATCH carries the same
rule: W1b-1 may run before W2 only when its accepted plan proves the CSS scaffold
is scalar-only (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:80`-`83`).
P3-C binds the same check at the W1b-1 gate
(`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:212`-`214`).

This is load-bearing because the current JSON scanner still exercises
`escape_mask_64` in the aarch64-capable fast path and carries `bs_carry` across
stripes (`skinny/crates/runtime/src/grammars/json/scan.rs:236`-`263`), while the
primitive's carry semantics live in `bbnf-simd`
(`skinny/crates/bbnf-simd/src/lib.rs:175`-`205`). The packet therefore correctly
treats W2 as a correctness prerequisite, not as row-movement credit
(`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:280`-`290`).

### CH5-2: Fallback is now W1b-2-only

No defect found. The user pin says Sheets and BBNF-self become fallbacks only
after a CSS L4 redress attempt fails, not after preflight failure
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:20`-`24`).
PIN-V3 folds the prior PIN-V2 ambiguity by stating that a W1b-1 scaffold failure
does not satisfy the fallback condition and that fallback remains blocked until
W1b-2 records measured CSS lightningcss comparator/admission redress
(`restart/skinny/tranches/sk-v12/SPEC.md:438`-`442`).

The surrounding packet agrees. W1b-2 is the wave that records comparator,
equality, oracle independence, generated-size, throughput, and gate-consumption
misses; only after that measured CSS redress may fallback be considered
(`restart/skinny/tranches/sk-v12/SPEC.md:482`-`491`). P3-B also states that
Sheets/BBNF-self are not W1b alternatives and require W1b-2 BLOCKED/REJECTED
evidence plus an explicit later fallback wave
(`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:104`-`110`).
P3-C repeats the no-hidden-fallback constraint inside W1b-2 redress
(`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:258`-`264`).

### CH5-3: W1b-2 / W3 / W4 shared-file race is acknowledged and bounded

No packet-level defect found. W1b-2 owns CSS bench/report/gate/result surfaces
(`restart/skinny/tranches/sk-v12/SPEC.md:452`-`461`), W3 owns runtime/codegen/CSS
generated output plus overlapping bench/report/gate surfaces
(`restart/skinny/tranches/sk-v12/SPEC.md:501`-`511`), and W4 owns SIMD,
parse-that, generated CSS, codegen template, bench/report/gate, orphan
disposition, RESULTS, and REDRESS surfaces
(`restart/skinny/tranches/sk-v12/SPEC.md:548`-`561`).

P3-B identifies the concurrency hazard and tells the orchestrator to run W3 and
W4 serially to avoid shared generated-runtime, bench, and gate-file races
(`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:97`-`102`).
DISPATCH keeps the order firm, allows W3 to be not-required only on an already
admitted CSS path, and still requires W4 orphan disposition
(`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:80`-`86`). The per-wave
protocol also constrains redress to one implementation thread and SPEC owner
paths only (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:124`-`133`).

Residual risk: an implementation orchestrator that ignores P3-B and dispatches
W3/W4 concurrently would race shared files. That is an execution violation, not
a PIN-V3 packet failure.

### CH5-4: Substrate cardinality and public API escape hatches remain closed

No defect found. The user pin reopens union and ASM-gen categories, but only
under scalar/reference, parity/checkasm, same-wave consumer, material
differential, and CHALLENGE discipline
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39`-`56`,
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:58`-`69`). The SPEC
does not turn that into public substrate permission: it forbids new directives,
BIR variants, `BackendShape` variants, public substrate APIs, parser-owned
sidecars, decoded-byte sidecars, hidden host schemas, and orphan generated or
substrate paths (`restart/skinny/tranches/sk-v12/SPEC.md:220`-`227`).

W3 entry requires no sidecar substrate, no parser-owned cursor/list, no
parallel `UnionTape`, and no retained decoded-byte/class side vector
(`restart/skinny/tranches/sk-v12/SPEC.md:515`-`520`). Its task is a
single-substrate same-tape projection local to the CSS row
(`restart/skinny/tranches/sk-v12/SPEC.md:522`-`526`). P3-D makes that telemetry
fail-closed: `substrate_cardinality` must remain `one`, and `public_api_delta`
must be no new public substrate API
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:220`-`237`).

The current source substrate is one retained `Tape` containing source, offsets,
sparse flags, payload arena, and tape id
(`skinny/crates/runtime/src/tape/mod.rs:94`-`120`). `ValueRef` remains tape plus
cursor (`skinny/crates/runtime/src/tape/mod.rs:175`-`222`). No PIN-V3 packet text
requires a second retained substrate or new public substrate API.

### CH5-5: Zero-orphan accounting is explicit and gate-owned

No defect found. The user pin names the five carried aarch64 orphans and requires
zero orphan kernels by close
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71`-`78`). The SIMD
audit confirms the same five-orphan inventory and explains why each needs
consumption, removal, or demotion evidence
(`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34`-`61`).

SPEC makes zero production orphans an ADMIT and FIXPOINT condition
(`restart/skinny/tranches/sk-v12/SPEC.md:58`-`63`,
`restart/skinny/tranches/sk-v12/SPEC.md:80`-`83`). W4 must enter with a five-row
orphan accounting table, and non-selected orphans may be demoted only with
evidence that no behavior source change is needed
(`restart/skinny/tranches/sk-v12/SPEC.md:574`-`577`). W4 then records all five
by consumption, removal, or inventory demotion
(`restart/skinny/tranches/sk-v12/SPEC.md:581`-`588`), and P3-D requires each
orphan row to carry status, consumer or demotion/removal artifact, Lock 16
status, and REDRESS id
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:239`-`255`).

The source inventory confirms that the orphan-facing modules are public within
the aarch64 module surface (`skinny/crates/bbnf-simd/src/aarch64/mod.rs:1`-`14`).
Three wrappers currently delegate to scalar bodies
(`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`-`4`,
`skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`-`4`,
`skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`-`4`), while
`byte_context` and `cache_hints` expose support-only helpers without a legal row
consumer (`skinny/crates/bbnf-simd/src/aarch64/byte_context.rs:1`-`10`,
`skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:1`-`33`).

### CH5-6: Same-wave consumer coupling is preserved

No defect found. SPEC rejects any primitive, SIMD/ASM kernel, parse-that helper,
generated path, substrate, or output-plane contract without scalar reference,
parity/checkasm where applicable, same-host micro-proof, and same-wave consumer
(`restart/skinny/tranches/sk-v12/SPEC.md:225`-`227`). DISPATCH requires every
primitive or generated parser path to include the same-wave consumer and
same-wave gate (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:130`-`133`).

W3 wires the CSS generated consumer in the same commit
(`restart/skinny/tranches/sk-v12/SPEC.md:522`-`526`). W4 wires the selected
primitive into a same-wave CSS generated consumer or JSON-guard consumer
(`restart/skinny/tranches/sk-v12/SPEC.md:579`-`584`). P3-D rejects
`harness_only` and `producer_only` SIMD/ASM consumers
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:164`-`183`)
and requires union/ASM route records to name `consumer_path`,
`substrate_cardinality`, and `public_api_delta` in the same attempt
(`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:220`-`237`).

## Required Fixes

None.

## Residual Risk

- W1b-2, W3, and W4 share generated runtime, bench, report, gate, RESULTS, and
  REDRESS surfaces. The packet handles this by firm ordering and P3-B's serial
  dispatch instruction; redress orchestration must follow it.
- Existing non-JSON gate/report source is still historical until W1b-2 upgrades
  it to the CSS L4 lightningcss field set. That is acceptable at S-P3 because
  W1b-2 explicitly owns the comparator and admission-gate implementation
  (`restart/skinny/tranches/sk-v12/SPEC.md:472`-`491`,
  `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:89`-`95`).

## Verdict

CH5 PASS. PIN-V3 closes the W1b-1 fallback ambiguity and preserves the hidden
coupling guards for W2/W1b-1 SIMD legality, W1b-2/W3/W4 shared-file sequencing,
single-substrate union attempts, zero-orphan accounting, public API escape
hatches, same-wave consumer requirements, and W1b-2-only fallback eligibility.
