# Implementation Packet SK-V6: Same-Plane SOTA Recovery

Date: 2026-05-15.

This packet supersedes ad hoc SK-V6 intervention notes after the twelve-agent
asmjson/DAV1D research pass. It does not supersede
`IMPLEMENTATION-AGENT-PROMPT-SK-V6.md`; that prompt remains the dispatch
contract. This packet materializes the next implementation sequence.

## 1. Non-Negotiables

- No new BBNF directives.
- No new BIR variants.
- No parallel substrate.
- No JSON code in generic crates.
- Every SIMD/ASM primitive has a scalar executable spec, checkasm parity, ABI
  checks, and a same-wave consumer.
- Every performance claim cites Mbps or c/B, corpus, output plane, strictness
  plane, hardware, feature mask, and profile path.
- asmjson comparisons are strict only when the row is strict. Permissive rows
  are flaw probes.
- `semantic_full_digest_stressor` is a guard workload. `real_typed_struct` is
  the representative direct-to-struct product workload once generated from
  host/API schema facts.

## 2. Reading Order

1. `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md`
2. `restart/skinny/tranches/sk-v6/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v6/research/skv6-B6-spec-edit-map.md`
4. `skinny/RESULTS.md`
5. `skinny/REDRESS.md`
6. Wave-specific cohort reports named below.
7. C-pass reports: `skv6-C1-retained-profile.md`,
   `skv6-C2-direct-profile.md`, `skv6-C3-sidecar-planes.md`,
   `skv6-C4-host-asm-profile.md`, `skv6-C5-parse-that-gaps.md`, and
   `skv6-C6-generality-costfacts.md`.

## 3. Wave 0: Comparator Schema V3 And Plane Repair

Owner paths:

- `restart/skinny/BENCH.md`
- `skinny/Cargo.toml`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Tasks:

- Remove or isolate sonic-rs `utf8_lossy` from strict S anchors. If a lossy row
  remains, label it `flaw_probe`, not `strict`.
- C3 confirms the current bench enables `utf8_lossy` in
  `skinny/crates/bbnf-bench/Cargo.toml`; all current sonic rows are therefore
  ineligible as strict anchors until rebuilt and rerun without that feature.
- Add schema v3 fields to every reported row: strictness plane, UTF-8 plane,
  escape plane, output plane, ownership plane, feature mask, API symbol,
  corpus hash, hardware, build flags, sidecar freshness, primitive status.
- Emit `BBNF-COMPARATOR-PLANE-DRIFT` when a candidate S anchor differs on any
  strictness/output/ownership field.
- Preserve unrelated staged work; this wave owns only the metadata/schema
  repair.

Exit gate:

- `cargo run -p xtask --release -- bench-json --advisory` produces schema v3
  rows.
- `cargo run -p xtask --release -- gate-json --advisory` refuses same-plane
  SOTA classification for lossy/permissive anchors.

## 4. Wave 1: DAV1D-Grade checkasm Hardening

Owner paths:

- `skinny/crates/bbnf-simd/`
- `skinny/xtask/`
- `restart/skinny/SUBSTRATE.md`
- `restart/ARCHITECTURE.md`

Tasks:

- Add forced feature-mask dispatch for scalar, NEON, AVX2, AVX-512 where
  available.
- Add register-clobber checked-call shims, stack-canary checks, recoverable
  signal/fault handling for unsafe test calls, rdtsc/cycle counters on x86, and
  host timer fallback on arm64.
- Normalize every primitive's scalar oracle output before target comparison.
- Reject dispatch-table admission unless the primitive has a same-wave runtime
  or generated consumer.

Exit gate:

- `cargo run -p xtask --release -- primitive-checkasm` passes on the active
  host.
- A deliberately corrupted target body is caught by the harness in a negative
  test.

## 5. Wave 2: Retained Parse Recovery, One Intervention Per Dispatch

Owner paths are selected by the winning candidate and must stay inside:

- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/parse-that-regex/src/`
- `skinny/crates/bbnf-simd/src/`
- generated JSON runtime outputs

Candidate shortlist from B3/B5/C1/C5:

1. Trusted quoted-span tail scan: optimize `match_string_at_quote` behavior for
   escape-heavy rows without introducing a sidecar scan.
2. Tiny/plain string threshold as a generated retained cost fact, not a global
   policy. REDRESS 72 admits generated retained cap16 and rejects global,
   direct, and Track 2 widening.
3. Object/key continuation shape under generated retained parsing, only after a
   profile proves it lifts object-heavy rows and does not reproduce REDRESS 73.
4. Per-`\uXXXX` table/TBL classifier inside the existing retained string path.
   It must operate per escape unit and must not replay the rejected
   four-contiguous-unit run validator from REDRESS 64.

Falsifiability gates:

- `unicode_escapes` retained must improve by at least 15 percent for an
  escape-tail candidate.
- `y_string_unicode` retained must improve by at least 8 percent for a
  per-`\uXXXX` candidate.
- `twitter`, `citm_catalog`, or `distinct_values` retained must improve by at
  least 10 percent for a tiny/plain candidate.
- No guard row may regress more than 5 percent under native Criterion.
- Failure records a REDRESS entry and the candidate is reverted.

## 6. Wave 3: Generated DirectBuild Field-Layout Materialization

Owner paths:

- `skinny/crates/ir/`
- `skinny/crates/passes/`
- `skinny/crates/codegen/src/lower/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/`

Tasks:

- Add grammar-neutral `DirectFieldFacts`: field id, source path, target type,
  cardinality, duplicate/unknown policy, null/default policy, representation,
  materializer, and diagnostic context.
- Feed `DirectBuild` from host/API output schema facts rather than benchmark
  private hand sinks.
- Generate `real_typed_struct` workloads for at least `mesh`,
  `unicode_escapes`, `numbers`, and `distinct_values` in addition to the two
  current representative rows.
- C2 nominates `mesh` as the first generated typed DirectBuild candidate:
  typed numeric vectors, explicit skip facts for non-output maps, and capacity
  hints for large arrays. The profile must show typed generated symbols rather
  than `JsonDigestSink` symbols.
- Keep `semantic_full_digest_stressor` as a visible guard workload, but do not
  use it as the product-plane SOTA close.

Exit gate:

- Generated Track 1 direct path appears in samply as
  `runtime::generated_json::parse_direct` for every row.
- At least four representative typed rows pass same-plane sonic-rs/serde sidecar
  slack, including one escape-heavy row and one numeric row.
- No bench-private Track 1 parser or checksum-only close is used.

## 7. Wave 4: Grammar-Neutral CostFacts And Lock 14 Cleanup

Owner paths:

- `skinny/crates/passes/`
- `skinny/crates/codegen/`
- `skinny/crates/bbnf-simd/`
- `skinny/crates/parse-that-regex/`
- `restart/ARCHITECTURE.md`
- `restart/skinny/COMPILER.md`

Tasks:

- Replace `shapes_for_json`, `nominate_json`, JSON rule-name matches, and JSON
  structural alphabet constructors in generic crates with grammar-derived facts.
- Replace JSON-named primitive APIs in `parse-that-regex` with grammar-neutral
  string, trivia, number, and Unicode primitive facts; C6 identifies this as a
  Wave 4 owner-path correction, not Wave 2 retained-performance work.
- Add `CostFacts` records for selected and rejected alternatives: threshold,
  tiny-string cap, quoted-span strategy, direct materializer, capacity policy,
  and primitive route.
- Emit `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` and
  `BBNF-COST-EVIDENCE-INCOMPLETE` from the lint/verify path.

Exit gate:

- `rg -n "json|Json|object|array|string|number" skinny/crates/{passes,codegen,bbnf-simd,parse-that-regex}/src`
  has no generic-crate grammar-name logic except generated test fixtures or
  explicitly annotated scalar-oracle samples.
- `cargo run -p xtask --release -- gen --check` remains green.

## 8. Wave 5: Primitive Bodies With Consumers

Owner paths:

- `skinny/crates/bbnf-simd/`
- `skinny/crates/runtime/`
- generated runtime consumer paths

Tasks:

- Land only primitives that Wave 2-4 consumers require. The candidate inventory
  is `quoted_span_match_trusted`, `string_special_mask_16`,
  `trusted_string_special_tail_scan`, `hex4x4_to_u16x4`,
  `surrogate_pair_join`, `decimal_span_classify_int_float`, and the remaining
  Layer 1 bitmap/stack/bulk emit bodies where consumed.
- Each target ISA implementation must match the scalar oracle and pass
  checkasm hardening from Wave 1.

Exit gate:

- The primitive consumer's named corpus rows move by the written threshold.
- `primitive-checkasm` passes with ABI hardening enabled.

## 9. Wave 6: Same-Plane Strict Matrix

Owner paths:

- `skinny/RESULTS.md`
- `skinny/crates/bbnf-bench/`
- `skinny/xtask/`
- `restart/skinny/tranches/sk-v6/HANDOFF.md`

Tasks:

- Run 17 corpora across retained parse, structural scan, semantic digest
  stressor, generated real typed structs, Track 2 retained, Track 2 direct, and
  sidecar comparators.
- Sidecars: sonic-rs strict typed/lazy rows, simdjson DOM and On Demand where
  available, yyjson inlined DOM, serde_json, and asmjson permissive/strict
  rows separated.
- Publish Mbps and c/B, not wall time alone.

Exit gate:

- Zero retained G rows and zero direct N rows, or every remaining row has a
  falsified REDRESS route with same-plane measurements.
- Same-plane Track 1 beats sonic-rs, simdjson, and yyjson by 1.10x slack where
  the output plane matches.
- asmjson is beaten only on strict same-plane hardware rows; permissive rows
  remain flaw probes.

## 10. Wave 7: x86 CollapsedStage Successor

Owner paths:

- `skinny/crates/bbnf-simd/ext/x86/`
- `skinny/crates/bbnf-simd/src/x86_64/`
- generated per-grammar `.data` and wrapper paths

Entry gate:

- Wave 6 closed.
- Zen 4 or equivalent AVX-512 silicon is available.
- A NASM author is declared.
- Layer 1 primitives consumed by the wrapper pass checkasm.

Tasks:

- Generate grammar strictness tables, byte-class tables, stack-policy tables,
  and output-event tables from existing facts.
- Author JSON `CollapsedStage` wrapper over Layer 1 macros.
- Compare strict-vs-strict against sonic-rs, simdjson, yyjson, and asmjson on
  the same hardware.

Exit gate:

- Strict `CollapsedStage` beats asmjson's same-plane row by at least 1.20x or
  records the failed route in REDRESS.

## 11. Close Condition

SK-V6 closes only when:

- comparator schema v3 is enforced;
- primitive-checkasm is DAV1D-grade;
- retained parse rows are closed or falsified;
- generated direct typed rows are closed or falsified;
- Lock 1 and Lock 14 audits are clean;
- same-plane SOTA rows beat sonic-rs, simdjson, yyjson, and strict asmjson
  where that hardware row exists;
- `restart/skinny/tranches/sk-v6/HANDOFF.md` records commits, row deltas, rejected
  routes, and remaining no-go evidence.
