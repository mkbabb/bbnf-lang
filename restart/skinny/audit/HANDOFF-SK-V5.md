# SK-V5 Handoff

Date: 2026-05-13.

Status: **SPEC MATERIALIZED. READY FOR IMPLEMENTATION DISPATCH.**

The implementation agent's reading order is:

1. `restart/skinny/audit/GRAND-SYNTHESIS-SK-V5.md` (the why)
2. `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md` (the what + when)
3. `restart/skinny/audit/NUKE-PLAN-SK-V5.md` (the removals)
4. This document (the entry/exit gates + dispatch posture)

Cohort authority: 15 audit reports under
`restart/skinny/audit/SK-V5-COHORT/` (9 deep cohort reports A1-A6 + B1-B3,
plus 6 novelty-challenge reports D1-D6; 5,559 LOC total).

## Where SK-V5 Stands

The architecture was declared in `restart/MASTER-PLAN.md` §13 (commit
`8fa51245`) and `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`
(commit `1519cf16`). The Rust state behind that architecture does not
exist:

- `BackendShape` enum: spec-declared at `ARCHITECTURE.md:1048-1072`,
  zero Rust hits in `skinny/crates/`.
- `derive_backend_shape`: spec-declared at `ARCHITECTURE.md:1075-1083`,
  no Rust symbol exists.
- `LayoutFacts.backend_shape`: absent from `passes/src/lib.rs:46-51`.
- `codegen/src/lower/`: SK-V4 Wave 1 owner path that does not exist on
  disk.
- `derive_backend_shape` selection: replaced today by `shapes_for_json`
  hardcoded at `passes/src/lib.rs:28-29` regardless of input grammar.
- Eisel-Lemire f64 materializer: zero implementation in skinny's
  `parse-that-regex`; full implementation exists at
  `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`
  (vendorable).
- NEON UTF-8 codepoint pipeline: zero implementation; the diagnosed hot
  kernel boundary at 25-40% of `parse_value_at` self-time on every
  parse-G row.
- 8 of 9 `bbnf.asm` Layer-1 macros: contract-only, no bodies.

The current gate per `skinny/RESULTS.md`:
- 4 parse-G rows (twitter 78%, random 51%, unicode_mixed 47%,
  unicode_basic 49% of sonic-rs).
- 11 of 17 direct rows N-direct (numbers 33%, canada 41%, mesh 52%,
  unicode_mixed 50%, etc.).
- Strictness plane undisclosed; some rows compare strict-bbnf to
  permissive-asmjson without flagging it.
- Track 1 == Track 2 == bench-private SinkParser (the bench measures
  itself, not the language).

## The Corrected Diagnosis

Two SK-V4 framings were wrong:

1. **Class A `match_tiny_plain_string` is not the parse-G fix.** D6
   verified: the kernel was previously wired and regressed twitter
   ~25%; it was reverted (REDRESS.md:301-313). The kernel is
   parity-green but targets the 8-byte scalar early-out layer
   (`bbnf-simd/src/lib.rs:195`), not the actual hot kernel boundary.
   B1's PC attribution confirms: `tiny_plain_string_scalar` is at most
   7.9% of `parse_value_at` self-time on random.

2. **The actual hot kernel boundary is `validate_utf8_codepoint`**
   reached via `skip_json_string_plain`'s NEON 16-byte block returning
   early on every byte ≥ 0x80. B1+B2+A3 corroborate. The fix is one
   kernel: fold UTF-8 validation INTO the NEON 16-byte string-body
   scan, eliminating both the early-exit and the scalar fall-through.
   All four parse-G rows share this pathology at four intensity
   levels.

The number lever is independent: vendor Eisel-Lemire from
`/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`
into skinny's `parse-that-regex/src/number/`. The integer materializer
is misplaced in `bbnf-bench/src/direct_struct.rs:501`; move to
`parse-that-regex/src/number/integer.rs`. This closes numbers / canada
/ mesh / marine_ik direct rows.

## Wave Sequencing

Per `IMPLEMENTATION-PACKET-SK-V5.md`:

| Wave | Scope | Owner paths | Exit gate |
|---|---|---|---|
| 0 | Strictness columns + parse-attribution feature + nuke decisions | `skinny/RESULTS.md`, `runtime/Cargo.toml`, `runtime/src/grammars/json/generated.rs`, `NUKE-PLAN-SK-V5.md` | Strictness disclosed honestly; parse-attribution build green; nuke targets enumerated |
| 1 | BackendShape enum + LayoutFacts.backend_shape field + derive_backend_shape + codegen/src/lower/ hierarchy | `ir/src/`, `passes/src/`, `codegen/src/lib.rs`, `codegen/src/lower/` | Substrate plumbing complete; codegen no longer discards `&BackendIr`; regression-free transition |
| 2 | Number lever + generated SinkOnly + bench rewire + bench-private SinkParser nuke | `parse-that-regex/src/number/`, `codegen/src/lower/sink_only.rs`, `runtime/src/grammars/json/sink.rs`, `bbnf-bench/src/direct_struct.rs` | numbers/canada/mesh/marine_ik direct rows cross 1.10× sonic-rs slack; Track 1 calls generated runtime; Track 2 is structurally different |
| 3 | UTF-8 fusion + Class B `_x4` batched + utf8_block module | `parse-that-regex/src/lib.rs:331-339`, `parse-that-regex/src/unicode/`, `bbnf-simd/src/aarch64/utf8/`, `bbnf-simd/src/aarch64/unescape_uxxxx.rs` | 4 parse-G rows close; string-bound direct rows lift |
| 4 | Lock 14 remediation + working-tree nukes | `bbnf-simd/src/lib.rs`, `bbnf-simd/src/aarch64/*`, `bbnf-simd/src/x86_64/*`, `runtime/grammars/json/`, `simd-scan/`, `runtime/.../generated_eventcursor.rs`, `runtime/Cargo.toml` | Lock 14 audit clean; 7 grammar-neutral split items land |
| 5 | Remaining 8 `bbnf.asm` primitive bodies + checkasm hardening + runtime dispatch table | `bbnf-simd/src/x86_64/<prim>.asm`, `bbnf-simd/src/aarch64/<prim>.rs`, `bbnf-simd/src/scalar/<prim>.rs`, `bbnf-simd/tests/`, `bbnf-simd/src/dispatch.rs` | Per-primitive scalar reference + checkasm parity green; all 8 primitives have generated/runtime consumer |
| 6 | Strict workload matrix | `bbnf-bench/`, `RESULTS.md`, `restart/skinny/BENCH.md` | 17 corpora × 7 workloads × N sidecars with strictness disclosed; no parse-G, no N-direct |
| 7 (optional) | x86 CollapsedStage successor | `bbnf-simd/src/x86_64/*.asm`, `runtime/grammars/json/json_collapsed.asm`, `codegen/src/grammars/json/tables.rs` | Gated on Zen 4 silicon + NASM author + checkasm-green Layer 1 |

After Wave 3, the M5 Max close condition is met (parse-G + N-direct
both gone). Wave 4-6 are durability work on top of a measured win.
Wave 7 is the x86 asmjson-beat successor.

## Entry Gates per Wave

Each wave begins by reading: GRAND-SYNTHESIS-SK-V5 §11-12 (the wave
sequencing rationale), IMPLEMENTATION-PACKET-SK-V5 §N (the specific
wave's owner paths + concrete file plan), the relevant cohort report
in `restart/skinny/audit/SK-V5-COHORT/` for the diagnostic findings.

Wave 0 entry gate: synthesis docs are landed (this commit).

Wave 1 entry gate: Wave 0 strictness columns recorded; parse-attribution
feature builds green; profile attribution run with the feature on shows
named hot leaves (not one fused symbol).

Wave 2 entry gate: BackendShape enum compiles; LayoutFacts.backend_shape
field populated by `derive_backend_shape`; codegen consumes `&BackendIr`
honestly; `cargo test --workspace` passes.

Wave 3 entry gate: Wave 2 closed; generated runtime SinkOnly active
on Track 1; numbers/canada/mesh/marine_ik direct rows above 1.10×
sonic-rs slack.

Wave 4 entry gate: Wave 3 closed; 4 parse-G rows above outcome-G
boundary.

Wave 5 entry gate: Wave 4 audit clean; all nuked files removed; Lock 14
sweep returns no grammar leaks in generic crates.

Wave 6 entry gate: Wave 5 closed; all 9 bbnf.asm primitives have bodies
+ scalar references + checkasm + consumer.

Wave 7 entry gate: optional; requires Zen 4 silicon access AND NASM
author declared AND Layer 1 vocabulary fully checkasm-green.

## Exit Gate (SK-V5 close)

- `skinny/RESULTS.md` has zero parse-G rows.
- `skinny/RESULTS.md` has zero N-direct rows.
- Strictness column disclosed honestly on every row.
- Track 1 calls generated runtime (verified via `samply` symbol path).
- Track 2 is structurally different from Track 1 (different symbol
  path; not the same SinkParser).
- `parse_value_at` no longer collapses to one symbol; PC-level
  attribution explains any remaining gap.
- `cargo run -p xtask --release -- primitive-checkasm` passes including
  register-clobber detection.
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
new substrates. The architecture from MASTER-PLAN.md §13 + SK-V4 packet
holds; SK-V5 fills in the Rust state.

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

The five-shape taxonomy is correct. The Rust state behind it must now
exist.

**Dispatch Wave 0.**
