# SK-V5 Handoff

Date: 2026-05-14.

Status: **SUPERSEDED FOR DISPATCH BY SK-V6. CURRENT GATE: N-direct / NoGo.**
SK-V5 remains the substrate-history authority, but SK-V6 is now the dispatch
authority for throughput recovery. The current measured authority is
`skinny/RESULTS.md`: retained parse has 13 G / NO-GO rows and four A / GO rows
(`canada`, `mesh`, `marine_ik`, `numbers`); direct-to-struct is
correctness-green with four 1.10x sonic-rs slack passes (`citm_catalog`,
`apache_builds`, `github_events`, `instruments`) and 13 remaining red rows.
Canada structural scan is no longer stale: the full matrix reports 69075 Mbps against the 40000
Mbps NEON floor. Strictness/output-plane columns are disclosed.

The implementation agent's reading order is:

1. `restart/skinny/tranches/sk-v5/SYNTHESIS.md` (the why)
2. `restart/skinny/tranches/sk-v5/SPEC.md` (the what + when)
3. `restart/skinny/tranches/sk-v5/NUKE-PLAN.md` (the removals)
4. This document (the entry/exit gates + dispatch posture)

Cohort authority: 15 audit reports under
`restart/skinny/tranches/sk-v5/research/` (9 deep cohort reports A1-A6 + B1-B3,
plus 6 novelty-challenge reports D1-D6; 5,559 LOC total).

## Where SK-V5 Stands

The architecture was declared in `restart/MASTER-PLAN.md` §13 (commit
`8fa51245`) and refined by the SK-V4/SK-V5 audit line. The obsolete SK-V4
receiver packet was purged by SK-V6 Wave 0; this handoff preserves only the
surviving SK-V5 findings. The Rust state behind that architecture now exists
partially:

- `BackendShape`, `LayoutFacts.backend_shape`, `derive_backend_shape`, and
  `codegen/src/lower/` exist. The generated direct parser is now emitted from
  a BIR-derived `SinkOnlyProgram`; the old static JSON `sink_direct` template
  is deleted. This closes the codegen-authority blocker, not the SOTA gate.
- Eisel-Lemire / integer materialization is wired through
  `parse-that-regex::number`; the current `numbers` direct row passes, while
  `canada`, `mesh`, and `marine_ik` remain near-miss direct NO-GO rows.
- Trusted UTF-8 string matching and Class B materialization support landed, but
  the SK-V5 Wave 3 UTF-8 fusion prescription did not close the rows. REDRESS
  50-55 block the measured sub-routes; SK-V6 requires fresh PC-level profiles
  on the generated Track 1 baseline before any new kernel prescription.
- Generated `SinkOnly` direct now preserves raw string source spans to the sink
  boundary through `JsonSink::*_source` hooks. The default hooks keep the old
  allocation behavior; a no-allocation decoded-string digest visitor was
  measured, regressed Unicode direct rows, and was rejected in
  `skinny/REDRESS.md` item 49. A later exact decoded-string stats sink kept
  the same hook seam and exact digest semantics but paid a two-pass decoded
  length/hash cost; it regressed generated Track 1 on `unicode_mixed`,
  `unicode_escapes`, and `y_string_unicode` and was rejected in item 54. A
  true quote-source one-pass streaming hasher was then measured and rejected in
  item 55; it avoided the prior two shapes but still lost to the default
  allocate-then-contiguous-hash baseline on escape-heavy rows.
- Retained projection aux side tables were measured and rejected in
  `skinny/REDRESS.md` item 50. Dense and sparse parse-time metadata improved
  view probes but regressed retained parse rows, so H.W1 remains typed event
  cursor consumption over the existing tape projection.
- A byte-class whitespace `EventCursor` wrapper was measured and rejected in
  `skinny/REDRESS.md` item 51. It was correctness-green after repair but
  regressed focused retained Track 1 rows to roughly twitter 7130 Mbps,
  citm_catalog 10291 Mbps, and canada 14110 Mbps. H.W1 must consume the live
  structural emit mask with O(1) pending state; renaming `skip_ws` is not an
  event-cursor close.
- A stricter parser-local structural-mask cursor was measured and rejected in
  `skinny/REDRESS.md` item 53. It consumed the live JSON emit mask and carried
  only O(1) pending state, but it regressed focused retained Track 1 to twitter
  6156 Mbps, citm_catalog 8344 Mbps, and canada 7139 Mbps. H.W1 must make the
  structural projection the single parser substrate; adding a second cursor to
  source-byte recursive descent is not the close.
- Wave 5 admits only consumed primitives:
  `BYTE_CLASS_FROM_EQ_SET_64`, `BYTE_CLASS_FROM_TABLE_64`,
  `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, and `EOB_PAD_CLAMP`.
  Orphan macro bodies remain blocked until same-wave consumers exist.
- A post-Wave 5 scan-floor slice admits two additional consumed scanner
  primitives: `BULK_EMIT_POSITIONS_64`, consumed by `compact_mask`, and the
  structural+terminator no-quote classifier, consumed by JSON scan. Focused
  `simd_scan` measured Canada at about 41833 Mbps, and the refreshed full
  `RESULTS.md` matrix records Canada structural scan at 69075 Mbps, clearing
  the 40000 Mbps NEON floor.

The current gate per `skinny/RESULTS.md`:

- 13 retained parse rows are G / NO-GO; four rows are A / GO.
- Canada structural scan is above the 40000 Mbps NEON floor.
- 13 of 17 `semantic_full_digest_stressor` direct rows are N-direct;
  `citm_catalog`, `apache_builds`, `github_events`, and `instruments` pass the
  1.10x sonic-rs time slack.
- `real_typed_struct` passes for `twitter` and `update_center` under the
  host/API output-schema plane.
- Strictness and output-plane columns are disclosed; bbnf rows remain
  `deferred / view-boundary / yes`.
- Track 1 calls generated runtime `parse_direct`; Track 2 is structurally
  different. Direct source is lowerer-authored from BIR. The remaining
  direct-to-struct failures are runtime/materialization gaps, not a codegen
  attribution gap.

## The Corrected Diagnosis

Two SK-V4 framings were wrong, and SK-V5 added one empirically refuted
prescription:

1. **Class A `match_tiny_plain_string` is not the parse-G fix.** D6
   verified: the kernel was previously wired and regressed twitter
   ~25%; it was reverted (REDRESS.md:301-313). The kernel is
   parity-green but targets the 8-byte scalar early-out layer
   (`bbnf-simd/src/lib.rs:195`), not the actual hot kernel boundary.
   B1's PC attribution confirms: `tiny_plain_string_scalar` is at most
   7.9% of `parse_value_at` self-time on random.

2. **The old UTF-8 hot-boundary diagnosis is not a sufficient
   prescription on the generated Track 1 baseline.** B1+B2+A3 named
   `validate_utf8_codepoint` at the SK-V5 audit baseline, but five
   subsequent attempts around projection side tables, byte-class cursors,
   parser-local masks, decoded-string stats, and quote-source streaming
   were measured and rejected in REDRESS 50-55. SK-V6 therefore forbids
   hypothesis transfer from the SK-V5 Wave 3 prescription and reopens the
   row only through fresh `parse-attribution` profiles.

The number lever is independent: vendor Eisel-Lemire from
`/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`
into skinny's `parse-that-regex/src/number/`. The integer materializer
is misplaced in `bbnf-bench/src/direct_struct.rs:501`; move to
`parse-that-regex/src/number/integer.rs`. This necessary work is landed and
closes `numbers`, but `canada` / `mesh` / `marine_ik` remain direct near
misses rather than closed rows.

## Wave Sequencing

Per `IMPLEMENTATION-PACKET-SK-V5.md`:

| Wave | Scope | Owner paths | Exit gate |
|---|---|---|---|
| 0 | Strictness columns + parse-attribution feature + nuke decisions | `skinny/RESULTS.md`, `runtime/Cargo.toml`, `runtime/src/grammars/json/generated.rs`, `NUKE-PLAN-SK-V5.md` | Strictness disclosed honestly; parse-attribution build green; nuke targets enumerated |
| 1 | BackendShape enum + LayoutFacts.backend_shape field + derive_backend_shape + codegen/src/lower/ hierarchy | `ir/src/`, `passes/src/`, `codegen/src/lib.rs`, `codegen/src/lower/` | Substrate plumbing complete; codegen no longer discards `&BackendIr`; regression-free transition |
| 2 | Number lever + generated SinkOnly + bench rewire + bench-private SinkParser nuke | `parse-that-regex/src/number/`, `codegen/src/lower/sink_only.rs`, `runtime/src/grammars/json/sink.rs`, `bbnf-bench/src/direct_struct.rs` | Track 1 calls generated runtime and Track 2 is structurally different; `numbers` direct passes after later redress, but other direct rows remain NO-GO |
| 3 | UTF-8 fusion + Class B `_x4` batched + utf8_block module + decoded source-hook assay | `parse-that-regex/src/lib.rs:331-339`, `parse-that-regex/src/unicode/`, `bbnf-simd/src/aarch64/utf8/`, `bbnf-simd/src/aarch64/unescape_uxxxx.rs`, `runtime/src/grammars/json/sink.rs` | duplicate UTF-8 validation is removed; generated source hooks are admitted; no-allocation visitor, exact decoded-stats sink, and quote-source streaming hasher routes are rejected; parse-G and string-bound direct rows remain open |
| 4 | Lock 14 remediation + working-tree nukes | `bbnf-simd/src/lib.rs`, `bbnf-simd/src/aarch64/*`, `bbnf-simd/src/x86_64/*`, `runtime/grammars/json/`, `simd-scan/`, `runtime/.../generated_eventcursor.rs`, `runtime/Cargo.toml` | Lock 14 audit clean; 7 grammar-neutral split items land |
| 5 | Consumed `bbnf.asm` primitive admission + checkasm hardening + admitted runtime dispatch | `bbnf-simd/src/{scalar,aarch64}/`, `bbnf-simd/src/dispatch.rs`, `bbnf-simd/tests/`, `runtime/grammars/json/scan.rs`, `xtask` | `BYTE_CLASS_FROM_EQ_SET_64`, `BYTE_CLASS_FROM_TABLE_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `EOB_PAD_CLAMP`, `BULK_EMIT_POSITIONS_64`, and the structural+terminator classifier have scalar refs, checkasm parity, and same-wave hot consumers; focused Canada scan clears the NEON floor; `BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and `FSM_DISPATCH_THREADED` are explicitly blocked as no-orphan future work |
| 6 | Strict workload matrix | `bbnf-bench/`, `RESULTS.md`, `restart/skinny/BENCH.md` | 17 corpora × 7 workloads × N sidecars with strictness disclosed; no parse-G, no N-direct |
| 7 (optional) | x86 CollapsedStage successor | `bbnf-simd/src/x86_64/*.asm`, `runtime/grammars/json/json_collapsed.asm`, `codegen/src/grammars/json/tables.rs` | Gated on Zen 4 silicon + NASM author + checkasm-green Layer 1 |

Wave 3 did not fire the SK-V5 close gate. Wave 4/5 are durability and
primitive-admission work on top of the still-open `N-direct / NoGo`
baseline. Wave 6 may continue from the consumed-primitive Wave 5 close,
but it must not claim all 9 primitive bodies are admitted. Wave 7 is the
x86 asmjson-beat successor.

## Entry Gates per Wave

Each wave begins by reading: GRAND-SYNTHESIS-SK-V5 §11-12 (the wave
sequencing rationale), IMPLEMENTATION-PACKET-SK-V5 §N (the specific
wave's owner paths + concrete file plan), the relevant cohort report
in `restart/skinny/tranches/sk-v5/research/` for the diagnostic findings.

Wave 0 entry gate: synthesis docs are landed (this commit).

Wave 1 entry gate: Wave 0 strictness columns recorded; parse-attribution
feature builds green; profile attribution run with the feature on shows
named hot leaves (not one fused symbol).

Wave 2 entry gate: BackendShape enum compiles; LayoutFacts.backend_shape
field populated by `derive_backend_shape`; codegen consumes `&BackendIr`
well enough to keep the transition regression-free. Post-redress update:
`SinkOnly` direct output is now rendered from the BIR-derived
`SinkOnlyProgram`, so subsequent Wave 2/3 work should treat codegen
attribution as closed unless new evidence contradicts it.

Wave 3 entry gate: Wave 2 closed; generated runtime SinkOnly active
on Track 1; `numbers` direct is green; Canada/mesh/marine_ik direct residuals
are recorded rather than treated as closure.

Wave 4 entry gate: Wave 3 landed source-hook and UTF-8/string work, but the
UTF-8 fusion close did not fire; parse-G rows remain open and are carried as
measured residuals. Post-redress update:
`JsonSink::*_source` hooks are present and generated direct consumes them, but
the first no-allocation decoded-string consumer is blocked by measured
regression, a later exact decoded-stats sink also regressed escaped-string
rows, and a quote-source one-pass streaming hasher still lost to the allocation
baseline. Future decoded delivery must materialize field-layout facts directly
rather than layer a generic visitor or sink-local hash helper over
`unescape_json_string`.

Wave 5 entry gate: Wave 4 audit clean; all nuked files removed; Lock 14
sweep returns no grammar leaks in generic crates.

Wave 6 entry gate: Wave 5 closed under the consumed-primitive admission
rule; `primitive-checkasm` passes for the admitted set; `skinny/REDRESS.md`
records the no-orphan block for `BULK_EMIT_COMPRESSED`,
`FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and `FSM_DISPATCH_THREADED`.
Post-redress update: the admitted set also includes
`BULK_EMIT_POSITIONS_64` and the structural+terminator classifier with a
focused Canada scan-floor pass. Wave 6 does not require all 9 bodies.

Wave 7 entry gate: optional; requires Zen 4 silicon access, declared NASM
author, and a real per-grammar `.asm` CollapsedStage consumer plan.
`FSM_DISPATCH_THREADED` body admission is gated on that consumer landing
in the same wave/change with scalar reference and checkasm parity.

## Exit Gate (SK-V5 close)

- `skinny/RESULTS.md` has zero parse-G rows.
- `skinny/RESULTS.md` has zero N-direct rows.
- Strictness column disclosed honestly on every row.
- Track 1 calls generated runtime (verified via `samply` symbol path).
- Track 2 is structurally different from Track 1 (different symbol
  path; not the same SinkParser).
- `parse_value_at` no longer collapses to one symbol; PC-level
  attribution explains any remaining gap.
- `cargo run -p xtask --release -- primitive-checkasm` passes for
  admitted primitives; Rust candidate calls use verified stack canaries.
  Raw callee-saved register sentinels are reserved for future FFI/ASM
  `call_new` shims.
- Lock 1 + Lock 14 audit clean (manual grep + cohort verification).
- Sidecar comparator table: sonic-rs `Value` / typed direct, simdjson
  C++ DOM / On Demand, yyjson inlined DOM, asmjson SWAR strict/permissive,
  serde_json — all recorded with API and output plane named.

## What This Wave Does Not Promise

SK-V5 does NOT promise beating asmjson on x86 AVX-512 hardware. asmjson's
10.93 GiB/s anchor is conditional on Zen 4 silicon access + per-grammar
NASM author + checkasm-green Layer 1. That work is Wave 7, optional, and
gated separately. The SK-V5 close is M5 Max against sonic-rs / simdjson
/ yyjson with strictness disclosed.

SK-V5 does NOT introduce new directives, new BIR variants, new locks, or
new substrates. The architecture from MASTER-PLAN.md §13 holds; SK-V5 fills in
the Rust state, and SK-V6 supersedes dispatch.

SK-V5 does NOT promise CSS L4 / Sheets / BBNF-self closure. The Lock 14
remediation (Wave 4) ensures generic crates STAY generic so those
grammars can land in subsequent tranches without further architectural
debt; the actual per-grammar work is a separate tranche.

## Dispatch Posture

The implementation agent dispatches Wave 0 first. Each wave's exit gate
must fire before the next wave begins. Failures in any wave land in
`skinny/REDRESS.md` as named rejected routes with measurements; the next
wave consumes the redress and amends its plan if needed.

The hard caps per wave are flexible (this is a multi-day campaign, not
a 30-min agent dispatch). Each wave's commit message must cite:
- The wave letter
- The owner paths touched
- The cohort report that diagnosed the work
- The exit-gate measurement that justifies closing the wave
- Any REDRESS entry created in the wave

## Triumvirate Discipline

The triumvirate (research → plan → redress) governs each wave. Research
agents return diagnoses; plan agents return implementation strategies;
redress agents (or the human author) execute. No agent merges roles. No
wave closes on "future phase will fix it."

The cohort reports A1-A6 + B1-B3 + D1-D6 ARE the research authority for
SK-V5. Subsequent waves may produce their own cohorts as needed, but the
foundational research is done.

## Final Posture

The 1,000-commit Era V "PSI/DTA failed for unclear reasons" frame is
fully retired. The failure was Rust-codegen automaton overhead that LLVM
cannot fold. Recursive descent in Rust compiles to an implicit automaton
via LLVM; the architecture preserves that property for four of five
`BackendShape` values. `CollapsedStage` is the only shape that requires
hand-written NASM, and it is gated separately per (grammar × ISA) by
`BBNF-COLLAPSEDSTAGE-NOT-VIABLE` with `OffsetTape` fallback.

The 16 architectural locks govern. The 9-macro grammar-neutral ASM
vocabulary at `bbnf-simd/ext/x86/bbnf.asm` is the load-bearing substrate
for all future per-grammar codegen output. The scalar Rust reference per
primitive is the executable specification. The checkasm differential
harness is the admission gate.

The five-shape taxonomy is correct. The Rust state behind it now exists for
BackendShape derivation, BIR-lowered SinkOnly, admitted consumed primitives,
and the focused Canada scan-floor repair. The close still requires full
matrix refresh, retained event/tape consumption, and direct field-layout
materialization.

**Continue from the current SK-V5 implementation state; do not restart at Wave 0.**
