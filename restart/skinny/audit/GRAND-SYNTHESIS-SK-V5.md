# SK-V5 Grand Synthesis

Date: 2026-05-13.

Cohort: 15 agent reports under `restart/skinny/audit/SK-V5-COHORT/` (9 deep
research/profile reports + 6 novelty-challenge reports). 5,559 LOC of
audit. Authority for the verdicts below.

Post-assay correction (2026-05-14): SK-V5 implementation is now partially
landed and the current gate remains `N-direct / NoGo`. The Rust scaffolding for
`BackendShape` / `derive_backend_shape` exists, and the generated direct parser
is now emitted from a BIR-derived `SinkOnlyProgram` rather than the former
static JSON direct template. The latest `skinny/RESULTS.md` records 13 retained
G rows, one Canada L row caused by the structural-scan floor (22136 Mbps vs
40000 Mbps), 3 retained A rows (`mesh`, `marine_ik`, `numbers`), and only one
direct PASS row (`numbers`). Treat the wave plan below as historical dispatch
intent amended by `HANDOFF-SK-V5.md` and `skinny/REDRESS.md` entries 46-48.

## 1. The Frame

SK-V5 is not new architecture. The architecture is already declared. SK-V5
is the execution of `restart/MASTER-PLAN.md` §13 H.W1 + H.W4 (commit
`8fa51245`, 2026-05-12) which specced the five-shape `BackendShape` +
`derive_backend_shape` + `LayoutFacts.backend_shape` + per-shape lowerer
at `crates/codegen/src/lower/rust.rs` + `BBNF-BACKEND-SHAPE-INCONSISTENT`
diagnostic — and were never coded. SK-V5 is also the execution of SK-V4
Wave 1 (generated `SinkOnly` from `BIR DirectBuild`), Wave 2 (exact direct
materializers), and Wave 3 (parse hot-hub attribution + EventCursor
lowering) — declared in `IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`
(commit `1519cf16`) without specification at the kernel boundary.

The novelty-challenge wave (D1-D6) verified this frame across six
finding clusters. Every cluster lands on the same pattern: the work was
declared at policy level in one of three places (SOTA-BEAT-DESIGN,
MASTER-PLAN H tranche, SK-V4 packet) and never reached the implementation
resolution that the SK-V5 cohort A/B agents named. SK-V5 is the
**implementation packet for prior-declared work**, not a fresh design
authority.

Three items in the cohort are genuinely-NEW resolution that no prior doc
named: the NEON UTF-8 codepoint pipeline at
`parse-that-regex/src/lib.rs:331-339`, the `unescape_uxxxx_x4_neon`
4-quartet batched form, and the `parse-attribution` feature flag for
no-inline diagnostic builds. Three items are NEW-SCOPE (vendor-and-wire,
not research): the Eisel-Lemire `compute_f64` algorithm copyable from
`/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`,
the integer materializer misplaced at `bbnf-bench/direct_struct.rs:501`,
and the dav1d-style checkasm hardening (register-clobber detection +
rdtsc + stack-canary XOR-fold) borrowable from the FFmpeg harness pattern.

## 2. The Corrected Diagnosis

The single most consequential finding is that **the SK-V4 Class A
diagnosis (`match_tiny_plain_string`) is wrong** — and was already known
to be wrong by the codebase. D6 confirmed by tracing REDRESS.md:301-313:
the Class A NEON kernel was authored, wired, and **regressed twitter
~25%**. It was reverted. The kernel is parity-green and grammar-generic
but targets the 8-byte scalar early-out at `bbnf-simd/src/lib.rs:195` —
which B1 invalidated empirically (`tiny_plain_string_scalar` is at most
7.9% on random; the four parse-G rows are not bound on it).

The actual dominant kernel boundary on every parse-G row is
`validate_utf8_codepoint` at 25-40% of `parse_value_at` self-time.
Reached via `skip_json_string_plain`'s NEON 16-byte block returning
early on any byte ≥ 0x80 (driven by `non_ascii_mask` in
`bbnf-simd/src/aarch64/string_block.rs:77` OR-ed into `interesting_mask`)
and falling through to a scalar per-byte validator at
`parse-that-regex/src/lib.rs:637-706` (70 LOC). B2 corroborated: the
16-byte NEON loop at `parse-that-regex/src/lib.rs:331` exits on every
`0x80..=0xff` byte, so Cyrillic / CJK / emoji corpora alternate
SIMD/scalar per codepoint.

The fix is one kernel — fold UTF-8 validation INTO the NEON 16-byte
body scan, eliminating both the early-exit and the scalar fall-through.
This is `scan_string_special_block` fused with a Lemire-style 64-byte
SIMD UTF-8 validator (NEON port via `vqtbl4q_u8` lookup tables for
leading-byte categorisation) plus a Hoehrmann DFA scalar reference. All
four parse-G rows (twitter 78%, random 51%, unicode_mixed 47%,
unicode_basic 49%) share this one pathology at four intensity levels.
String-handling totals: twitter 64%, random 65%, unicode_mixed 92%,
unicode_basic 79% of `parse_value_at` self-time.

c/B gaps vs simdjson C++ on M5 Max: twitter +0.13, random +1.67
(biggest), unicode_mixed −0.19 (bbnf already ahead — falsifies a hidden
assumption that all four rows are equally broken), unicode_basic +0.21.

## 3. The Number Lever

Three direct-NoGo rows (numbers 33% sonic-rs, canada 41%, mesh 52%) and
one fourth (marine_ik 73%) are number-bound. `serde_json::parse_number`
shows 19.91-50.27% leaf self-time on these rows. `bbnf-simd`'s AVX-IFMA
mantissa kernel at `mantissa.rs:37` is `unimplemented!("Wave 6:
vpmadd52luq …")`; no aarch64 fast-float path exists.

D1 verified the algorithm is **not** novel research and **not** a
re-opening. The prior `raw.parse::<f64>()` rejection on canada parity
(REDRESS.md:353-355) was a different algorithm (Rust stdlib's
`dec2flt::lemire` 1-ULP-disagrees with `fast_float2`). The Eisel-Lemire
algorithm itself is implemented, bit-parity-tested, and consumed in
production at `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`:
`mod.rs:147` exposes `compute_f64(i64, u64, bool) -> Option<f64>` with
full Clinger fast-path + Eisel-Lemire slow path + ambiguous-rounding
`None` sentinel. The main `crates/core` has consumed it since AY.W4.2
(commits b199afea, 7e1732d0, 4ca520d2). Skinny's `parse-that-regex` is
genuinely empty of f64 materialization: `lib.rs:149-220` is span-scanner
only.

The integer materializer is real and correct but **misplaced** at
`skinny/crates/bbnf-bench/src/direct_struct.rs:501-528`
(`parse_integer_digest` with proper `i64::MIN` handling). Move target:
`parse-that-regex/src/number/integer.rs`.

The AVX-IFMA mantissa stub names "Wave 6" but H.W6 in
`MASTER-PLAN.md:510` is CSS SOTA gates — documentation drift; the
kernel belongs to H.W5 (`MASTER-PLAN.md:509`). `BENCH.md:1298` Gate 4
names the wrap target `parse-that/float/eisel_lemire.rs` as a gate, not
a wave.

**Verdict**: vendor-and-wire tranche, not research.

## 4. The Strictness Honesty Gap

B3 surfaced the most consequential framing correction. The current
`skinny/RESULTS.md` does not disclose strictness plane. The N-direct
verdict against sonic-rs / simdjson / yyjson is read as a pure
throughput delta. It is partly a contract delta:

- asmjson SWAR (the only path measurable on M5 Max) is **permissive**:
  accepts `0x00..0x1F` as whitespace, passes unescaped controls inside
  string bodies (per asmjson's own docs). Any asmjson "beat" on
  `random`, `unicode_*`, `y_string_unicode` is not strict-vs-strict.
- **bbnf-skinny T1 parse is DEFERRED on UTF-8 validation** — strict
  at view-materialization time, permissive at parse time. The current
  RESULTS.md does not disclose this; the unicode_* and random rows
  compare bbnf-deferred against sonic-strict/simdjson-strict/yyjson-strict
  without flagging the contract delta.
- RapidJSON default flags do not validate UTF-8 either — permissive
  on adversarial input but benign on well-formed corpus rows.

The remediation is independent of throughput work: add
`Strictness | parse_utf8 | escape_complete | flaw_probe` columns to
RESULTS.md NOW so the existing rows can be honestly compared. This is
Wave 0 of SK-V5, not a Wave 5 deliverable.

## 5. The Bench-Private Dishonesty

B2 surfaced a load-bearing dishonesty in the current gate. The
"Track 1" being measured **is not the language**. Both `track1_digest`
and `track2_digest` in `bbnf-bench/src/direct_struct.rs:150-156` call
the same `sink_only_digest` — a hand-written recursive-descent
`SinkParser` over `&[u8]` + cursor that never touches `Tape` or
generated code. The bench is therefore not measuring codegen quality
on the direct workload; it is measuring a private parser twice.

Every primitive landed while this bench is on the gate is credited to
the bench, not the language. Once it's gone, the gate becomes honest
again. The remediation is two steps: land generated `SinkOnly`
emission from `BIR DirectBuild` (Wave 2 of SK-V5), then nuke the
bench-private `SinkParser` (post-wiring).

D5 verified that `BIR DirectBuild` existed but was skeletal. Post-assay
redress extends `DirectBuild` with field/source roster metadata, makes
`codegen/src/lower/sink_only.rs` lower BIR into a grammar-neutral
`SinkOnlyProgram`, and removes the static JSON direct template splice. Track 1
direct now calls generated runtime code whose `parse_direct` body is authored
from BIR; the remaining direct failures are materialization/runtime gaps, not
template-authority.

## 6. The Architecture Wiring Gap

D3 verified A5's diagnosis. Post-assay status: the Rust state behind the
five-shape taxonomy is now scaffolded (`BackendShape`,
`LayoutFacts.backend_shape`, `derive_backend_shape`, and `codegen/src/lower/`
exist), and the direct `SinkOnly` generated source is now authored by the BIR
lowerer. The remaining architecture wiring gap is cost-model and retained
materialization authority, not symbol absence:

- `lower::sink_only` now walks existing BIR variants into a `SinkOnlyProgram`
  and `emit_json_with_layout` appends lowerer-produced direct source.
- Per-rule shape selection must become a measured cost decision across retained
  and direct workloads, not a marker scan that treats `DirectBuild` shape
  strings as the full decision.
- The existing `ShapeFacts` at `ir/src/lib.rs:436-467` is a different
  thing: a typed-view catalogue for `view.rs` direct-builder emission
  (`JsonRoot { value: JsonValue<'i> }`), confusably co-named with the
  spec's `BackendShape` per-rule lowering-mode selector. Both should
  remain; they are orthogonal.

The codegen pipeline no longer remains decorative at the decisive BIR → direct
Rust text step. The BIR build itself (`extract::single_plan`) walks the grammar
honestly and produces real `BackendIr`; retained generated output and
SinkOnly direct output exist on disk, and the direct parser is now a product of
the per-shape lowerer. Retained parser generation still uses the historical
template surface, and cost-model selection remains measured work.

## 7. The bbnf-simd Lock 14 Status

The asm contract layer at `bbnf-simd/ext/x86/bbnf.asm` (9 macros) is
grammar-neutral by construction and holds Lock 14. Layer 0 `x86inc.asm`
(dav1d, BSD-2) is grammar-neutral by definition. Post-assay admission is
consumed-only: `BYTE_CLASS_FROM_EQ_SET_64`, `BYTE_CLASS_FROM_TABLE_64`,
`BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, and `EOB_PAD_CLAMP` are admitted
with scalar references, checkasm parity, and hot consumers. The remaining macro
bodies are blocked until same-wave consumers exist; admitting them now would be
orphan kernel work, not SOTA evidence. Any JSON-specific classifier tables must
stay outside grammar-neutral primitive bodies.
- `bbnf-simd/src/lib.rs` (716 LOC) is a JSON god-module:
  `JSON_STRUCTURAL`, `is_json_punctuation`, `scan_json_tail`,
  `JsonParseIndex`, `resolve_json_string_masks_64`, plus `mod neon`
  at lines 463-693 with 6× `vceqq_u8` JSON punctuation fan-in at
  `:642-647` and `:665-670`.

D4 verified the split-by-primitive intent at `restart/MIGRATION.md:259-269`;
only one macro was lifted in commit 9eef728c. The fossil at
`skinny/crates/simd-scan/src/lib.rs` (584 LOC, near-verbatim duplicate of
bbnf-simd `lib.rs:13-100`) is NOT in `skinny/Cargo.toml` members, NOT in
`workspace.dependencies`, zero crates depend on it (the `simd_scan` hit
in `bbnf-bench/Cargo.toml:34` is a criterion `[[bench]]` target name, not
a crate dep). The MIGRATION renamed but never deleted. Nukable.

## 8. The Tape Union Verdict

A4 verified Lock 1 (tape ≡ structural projection union) **VIOLATED** at
three load-bearing surfaces:

- Refuted parallel prepass `generated_eventcursor.rs` is wired behind
  `feature = "eventcursor"`. It is **committed**, not working-tree only.
- Bench-private `SinkParser` in `direct_struct.rs` bypasses the
  substrate.
- Cost-model `derive_backend_shape` decision substrate entirely absent
  (per §6 above).

The storage substrate itself (`Tape<'input>` + `TapeBuilder` + `ValueRef`)
holds cleanly with zero type-ambivalence and zero columnar / PayloadStream
residue. The skinny line has zero OpenFrame residue (`grep -rn
"OpenFrame" skinny/crates/` returns 0). The "10+ files" OpenFrame claim
from earlier waves is true only against the legacy pre-restart
`/crates/core/`, slated for ABROGATE-REPLACE per `restart/MIGRATION.md` §3.

## 9. The Comparator Reframe

A1 reframed the M5 Max close target. asmjson's 10.93 GiB/s headline is
**not** AVX-512 esoterica — its tight loop uses only stock AVX-512BW +
BMI (6× `vpcmpeqb`, 10× `kmovq`, 2× `vpcmpub`, 6× `korq`, 2× `vmovdqu8`,
18× `tzcnt`); zero exotic encodings. The 5× margin over yyjson comes
from a 9-state DPDA (carries `frames_buf[64]` + `open_buf[64]` as a
hardware-bounded explicit stack) with PC-as-state direct threading via
`r10`, tzcnt-driven seek, and msac-style EOB padding. asmjson's own
SWAR fallback already lands ~7 GiB/s on commodity hardware.

The M5 Max DOM-class leader is **yyjson at 3,687 MiB/s**, beating
simdjson DOM (1.142 c/B) and sonic-rs Value-DOM (~2.3 c/B) on this
host. Beating yyjson on M5 Max requires fusion-quality match (Lock 15
i-cache discipline) plus admitted NEON kernels inside the fused leaf,
NOT asmjson-class esoterica. asmjson on M5 Max is the SWAR fallback
only; comparing strict-bbnf to permissive-asmjson without a strictness
column is a category error.

The x86 successor (~12.8 GB/s twitter at 0.35 c/B via Zen 4 VBMI2)
remains gated on a per-grammar `.asm` author plus checkasm-green
Layer 1 primitives plus equivalent silicon. SK-V5 close does not
require Zen 4; it requires beating sonic-rs / simdjson / yyjson on
M5 Max NEON with strictness disclosed.

## 10. Validated / Invalidated Ledger Summary

From A6 (624 LOC, 108 ledgered items, 12 validated + 18 invalidated +
3 demoted + 14 still-open + 20 rejected routes):

Validated and load-bearing for SK-V5: lazy/event substrate as right
boundary, codegen overhead separable from substrate ceiling, Lock 15
i-cache discipline (yyjson evidence), Lock 16 primitive admission
discipline (escape_mask_64 bug evidence), SIMD vocabulary grammar-neutral
and checkasm-gated, sonic-rs/yyjson-style direct materialization,
simdjson On Demand iterator model, dav1d/FFmpeg process discipline,
two-layer ASM vocabulary, 5-shape BackendShape taxonomy as cost-model
output, expanded corpus over historical triad, direct-to-struct as
required workload.

Invalidated and pruned from SK-V5 scope: eager retained tape as
SOTA-beat substrate, 12-byte token width churn, pair-token fusion,
PSI/DTA Rust-codegen automaton, StructuralIndex sidecar prepass,
EventCursor as parallel prepass, function-pointer dispatch table,
capacity prescan one-shot/sampled, generic SWAR whitespace skipper,
separator elision, raw `f64` shortcut, **active 16-byte tiny-string
routing in the current parser shape** (the Class A wiring),
asmjson as M5 Max close condition, primitive-only work without
same-wave consumer, eager token stream as SOTA-beat substrate, cost
model as aspirational, dispatch table as the missing win, primitive
parity as sufficient.

Demoted: asmjson DPDA architecture (valid x86 successor; invalid as
ARM target), CollapsedStage shape (valid taxonomic value; invalid as
Rust codegen), StructuralIndex (valid as runtime intermediate concept;
invalid as parallel substrate).

## 11. The SK-V5 Diff vs SK-V4

SK-V4 declared the right architecture and the right non-negotiables.
SK-V5 corrects four items that SK-V4 either misdiagnosed or
under-resolved:

1. **Class A wiring is invalidated, not awaiting.** SK-V4 named
   `match_tiny_plain_string` as the Wave 1 NEON kernel. D6 verified
   the kernel was previously wired and regressed twitter ~25%; it
   was reverted. The kernel is parity-green but on the wrong layer.
   SK-V5 retires the Class A wiring entry and replaces it with the
   NEON UTF-8 codepoint pipeline + `unescape_uxxxx_x4_neon` batched
   form.

2. **Eisel-Lemire is vendor-and-wire, not authoring.** SK-V4 §4
   gestures at "exact f64 materialization matching serde/sonic" without
   naming the algorithm or its source. D1 verified the implementation
   exists at the upstream `parse-that` crate; skinny's parse-that-regex
   is genuinely empty. SK-V5 names the source path and the move-target
   path.

3. **The "Track 1 SinkOnly mandate" now has lowerer authority.**
   SK-V5 now has the codegen substrate path, Track 1 calls generated runtime
   code, and direct source is emitted from a BIR-derived `SinkOnlyProgram`.
   Generated direct source also preserves raw string spans to the sink boundary
   through `JsonSink::*_source` hooks. The remaining work is not a new
   directive or BIR variant; it is closing the measured runtime rows through a
   fused decoded-string sink primitive, exact materialization, event-stream
   consumption, and structural floor repair.

4. **Strictness disclosure is Wave 0, not Wave 5.** SK-V4 §7 declares
   "strictness plane named per row" as a Wave 5 requirement. B3 showed
   that the existing N-direct verdict cannot be honestly read against
   asmjson / RapidJSON without strictness disclosure now. SK-V5 lifts
   this to Wave 0.

## 12. Decision

SK-V5 is **ready for implementation dispatch** with the corrected
diagnosis and the verified novelty pattern. The path is:

- **Wave 0** (this packet + immediate): strictness columns in RESULTS.md;
  `parse-attribution` feature flag for `#[inline(never)]` on a small
  named set of kernel boundaries; nuke audit decisions for the working
  tree.
- **Wave 1** (substrate authoring): BackendShape enum,
  LayoutFacts.backend_shape field, derive_backend_shape function,
  codegen/src/lower/ hierarchy, BBNF-BACKEND-SHAPE-INCONSISTENT
  diagnostic. Zero per-shape lowering bodies; just the dispatch
  surface.
- **Wave 2** (number lever + sink emission together): vendor
  Eisel-Lemire from upstream parse-that into
  `parse-that-regex/src/number/`; move integer materializer from
  `bbnf-bench/direct_struct.rs:501` to
  `parse-that-regex/src/number/integer.rs`; land
  `codegen/src/lower/rust.rs` SinkOnly lowering for the 7 JSON rules
  emitted with `BackendShape::SinkOnly`; rewire `bbnf-bench` Track 1 to
  call generated runtime; nuke bench-private SinkParser. Post-assay
  correction: this work plus the lowerer redress closed codegen attribution
  and `numbers`; Canada / mesh / marine_ik remain direct residuals.
- **Wave 3** (UTF-8 fusion + Class B batch): NEON UTF-8 codepoint
  pipeline in `parse-that-regex/src/lib.rs:331-339` replacing the 0x80
  early-exit; `utf8_block.rs` module with Lemire 64-byte validator and
  Hoehrmann DFA reference; `unescape_uxxxx_x4_neon` 4-quartet batched
  body + NEON surrogate-pair join. Post-assay correction: this removed
  duplicate UTF-8 validation and lifted affected rows; generated string source
  hooks are admitted, while the attempted no-allocation decoded visitor route
  is rejected by measurement. Parse-G and direct string/Unicode gates remain
  open.
- **Wave 4** (Lock 14 remediation): split `bbnf-simd/src/lib.rs`
  god-module into per-primitive grammar-neutral modules; remove the 7
  hardcoded JSON punctuation char-lists from the 4 scalar-reference
  functions (move to codegen-emitted `.data` tables); delete
  `skinny/crates/simd-scan/` fossil crate; delete refuted
  `generated_eventcursor.rs` + `eventcursor` feature flag + cfg
  branches. Audit-clean against Lock 1 and Lock 14.
- **Wave 5** (consumed bbnf.asm primitive admission): admit only primitives
  with scalar references, checkasm parity, and same-wave runtime/generated
  consumers. `BYTE_CLASS_FROM_EQ_SET_64`, `BYTE_CLASS_FROM_TABLE_64`,
  `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, and `EOB_PAD_CLAMP` meet
  that bar. `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`,
  `BULK_EMIT_COMPRESSED`, and `FSM_DISPATCH_THREADED` remain blocked until a
  real consumer lands in the same change.
- **Wave 6** (strict workload matrix): finalize the 17-row parse +
  17-row direct matrix with strictness columns; sidecar comparator
  table with API and output plane named; cycles-per-byte where
  measurable.
- **Wave 7** (x86 CollapsedStage successor, optional): per-grammar
  NASM authoring for JSON's collapsed-stage kernel, only on equivalent
  Zen 4 silicon with declared author and checkasm-green Layer 1.

The close condition is unchanged from SK-V4 §0:
`skinny/RESULTS.md` has no parse G rows, direct-to-struct emits no
`N-direct`, sidecar rows for sonic-rs / simdjson C++ / yyjson / asmjson
are recorded with strictness and output plane named, Track 1 calls
generated runtime, parse_value_at no longer hides cost without
PC-level explanation.

The implementation packet for these waves is at
`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md` (companion
document). The nuke schedule is at
`restart/skinny/audit/NUKE-PLAN-SK-V5.md`. The handoff is at
`restart/skinny/audit/HANDOFF-SK-V5.md`.

## 13. Closing Posture

The 1,000-commit Era V "PSI/DTA failed for unclear reasons" frame is
fully retired. The failure was Rust-codegen-emitted automaton overhead
that LLVM cannot fold. Recursive descent in Rust compiles to an
implicit automaton via LLVM; the architecture preserves that property
for four of five `BackendShape` values. `CollapsedStage` is the only
shape that requires hand-written NASM, and it is gated separately per
(grammar × ISA) by `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` with OffsetTape
fallback.

The corpus of 16 architectural locks governs. The 9-macro grammar-neutral
ASM vocabulary at `bbnf-simd/ext/x86/bbnf.asm` is the load-bearing
substrate for all future per-grammar codegen output. The scalar Rust
reference per primitive is the executable specification. The checkasm
differential harness is the admission gate.

The path is execution. No new directives. No new BIR variants. No new
locks. No deferrals. The five-shape taxonomy is correct; the Rust state
behind it must now exist.
