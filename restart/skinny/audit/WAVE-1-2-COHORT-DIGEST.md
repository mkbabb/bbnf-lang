# Wave-1 + Wave-2 cohort digest — SOTA-BEAT research consolidation

Date: 2026-05-12  
Audit trail: research dispatched in two waves of six agents each against the
expanded G / NoGo gate recorded in `skinny/RESULTS.md`.  This digest closes the
cohort by recording the surviving load-bearing findings, the falsified routes,
the fix prescription, and the open questions that flow forward into the
SK-V3 implementation packet.  Authority lineage:
`restart/skinny/audit/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` →
`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` →
this digest.  Original wave research artefacts live under
`skinny/profile/{wave2-asm,wave2-pmu,wave2-capacity,wave2-prototype,native-sidecars}/`
and `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`; the wave-1 ASM/architecture
agents fed the design at `restart/skinny/audit/SOTA-BEAT-DESIGN.md`.

## (a) What is validated

**skinny v3 already wins on 4/17 measured corpora at the M5 Max anchor**
(Wave 2 Agent 1; `skinny/profile/native-sidecars/PROFILE-REPORT.md` §(a),(b)).
The lazy-offset tape + typed event consumption substrate carries citm
(3571 MiB/s, +43% vs yyjson 2498), canada (1675 MiB/s, +22% vs simdjson DOM
1370 and +8% vs yyjson 1550), mesh (1194 MiB/s, +6% vs simdjson DOM 1122),
and unicode_mixed (1719 MiB/s, +10% vs simdjson DOM 1568).  twitter at
2631 MiB/s leads simdjson C++ (2923 ahead) and trails yyjson (3687); on the
asmjson native M5 Max SWAR comparison twitter at 2631 reaches 80% of
asmjson's `string_array` 3315 and on `random` skinny's 1117 sits +67%
above asmjson's `mixed` 669.  The substrate is correct; the win column
forbids any redesign of Lock 1 (lazy-offset tape).

**The asmjson 10.93 GiB/s ceiling is x86_64 architecture, not AVX-512
esoterica** (Wave 1 Agent 1).  Disassembly of asmjson's tight loop walks
the published .S body: 6× `vpcmpeqb`, 10× `kmovq`, 2× `vpcmpub`, 6× `korq`,
2× `vmovdqu8`, 18× `tzcnt`.  Zero exotic encodings.  The 5× margin over
yyjson is sourced from a 9-state FSM with PC-as-state direct threading
via `r10`, `tzcnt`-driven seek, and msac-style EOB padding — NOT 512-bit
width.  asmjson's own SWAR fallback already lands 7 GiB/s on the same
host, which means the byte-by-byte intrinsics matter less than the
fused state-machine architecture.

**The checkasm differential parity harness is landed and catches NEON
correctness bugs on first run** (Wave 2 Agent 5;
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs` 1–452, 516 LOC after
strict-mode promotion; `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`).
On its first execution the harness caught a real `escape_mask_64` NEON
correctness bug: 112 / 448 alignment-sweep divergences and 2 / 32 uniform-
random divergences against the scalar reference, root-caused to
`new_carry` / `escaped` state-handoff confusion between
`escape_mask_64` and `scan_json_tail`.  Corpus-clean (17 / 17 expanded
corpora pass), so the production parser never trips the bug, but
synthetic and adversarial inputs do — confirming the dav1d/FFmpeg
discipline transfer (Wave 1 Agent 2): scalar reference per primitive,
xtask-style admission gate, target-feature kernel table.

**The 5-shape BackendShape taxonomy is the correct lowering matrix**
(Wave 1 Agent 6; folded into `restart/skinny/audit/SOTA-BEAT-DESIGN.md`
§4 and `restart/MASTER-PLAN.md` tranche H).  `BackendShape ∈
{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` exhausts
the lowering matrix without new BIR variants.  An 8-priority
`derive_backend_shape` algorithm pluggably selects per rule, and the
per-grammar matrix for JSON / CSS L4 / BBNF-self / Sheets is sketched
in `restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` §3.1.

**The dav1d/FFmpeg ASM architecture transfer is the discipline anchor**
(Wave 1 Agent 2; folded into
`restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` §2.1 and §2.2).
Vendored `x86inc.asm` (1978 LOC BSD-2), per-target-generic kernels +
per-grammar LUT data (NOT per-grammar codegen), `nasm-rs` + `cc-rs`
build, msac entropy decoder `cnt/buf/end` refill is strictly more
general than asmjson's byte-aligned bitmap.  The abstract primitive
lift framework — chunk-spanning byte-context via `vextq_u8`, digit-block
MAC via `udot`/`sdot`, k-mask arithmetic family — applies to any
grammar with a chunked-token primitive, not JSON specifically.

## (b) What is invalidated

**No new BIR variant is required.**  `Alt { Dispatch }` lowers to
five distinct access patterns based on `LayoutFacts.backend_shape`,
without a sixth variant.  The 20-variant BIR alphabet defined in
`restart/ARCHITECTURE.md` §7.2 is preserved verbatim
(`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §1
non-negotiable; `restart/skinny/audit/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`
§6).

**No new `@runtime` / `@simd` / `@backend` / `@shape` / `@asm` grammar
directive.**  The cost model auto-derives backend shape from grammar
facts; Lock 10 auto-detect stands.  Verification gate is `rg -n
"@(simd|runtime|backend|shape|asm)" grammars restart/skinny` returning
no new grammar surface
(`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §1).

**AVX-512 esoterica is NOT asmjson's secret.**  asmjson's measured
.S body uses only stock AVX-512BW + BMI instructions (Wave 1 Agent 1).
The architectural win is the fused 9-state FSM with PC-as-state direct
threading, not the instruction width.  AVX-512 esoterica beyond that
is a separable additive lift — see §(d) below.

**The sidecar structural-index typed-parser prepass is the wrong
framing.**  `restart/skinny/audit/SOTA-BEAT-DESIGN.md` §1 is now marked
HISTORICAL / SUPERSEDED: the retained tape projection IS the structural
index.  There is no second parallel substrate.  The mask stream is
transient; if retained, it is the tape projection (Wave 2 cross-cutting
finding, folded into
`restart/skinny/audit/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §3 and §5).

**The earlier "SWAR whitespace re-scan" pathology framing was wrong.**
Wave 2 Agent 2 ASM-per-corpus profile
(`skinny/profile/wave2-asm/PROFILE-REPORT.md` §(b.2);
re-baselined throughput at `skinny/profile/wave2-asm/`) partitions the
five failing corpora into TWO pathology classes and neither is
whitespace re-scan.  Class A is the scalar 8-byte
`match_tiny_plain_string` loop at
`skinny/crates/runtime/src/grammars/json/generated.rs:161–172`,
duplicated by the LTO-fat inliner at PCs `0x2734` (key path) and
`0x3158` (value path).  Class B is the scalar `\uXXXX` decoder in
`parse_that_regex`'s `unescape_json_string`, materialised into
`parse_value_at` as a `sub/csel/orr/lsl` cluster × 4 nibbles with
11-op dependency chain per nibble (Wave 2 Agent 3 PMU profile;
`skinny/profile/wave2-pmu/PMU-REPORT.md`).

**The allocator is NOT the bottleneck.**  Wave 2 Agent 6 capacity probes
(`skinny/profile/wave2-capacity/CAPACITY-REPORT.md`) measure
`RawVec::grow_one` at <0.05% of self-time on all measured corpora.
Plan D (grow-only, initial capacity 256, geometric doubling) wins on
random (+4.8% Mbps) and github_events (+10.2%) and reclaims 23–64%
capacity universally.  Plan A (sampled heuristic over-reserves 2.53×,
overfit to update-center's 4 KiB prefix), Plan B (full scalar pre-scan,
2.3× throughput hit), and Plan C (one-shot SIMD pre-scan, 120 µs/parse
cost and discards the position vector) are rejected as standalone in
`skinny/REDRESS.md`; Plan C re-examines once Wave 1 event-cursor lands
because the structural index would already be paid for.

**The Wave 2 Agent 4 eventcursor prototype REFUTED the 0.18–0.22 c/B
projection at this implementation level**
(`skinny/profile/wave2-prototype/PROTOTYPE-REPORT.md`).  Across all six
measured corpora the scoped-down prototype regresses 0.63×–0.89× vs the
V9.4 baseline (random 1280 vs 1641 Mbps; update-center 1559 vs 2481;
twitter 2194 vs 3152).  The full prototype (mask + LUT + ptr/end
sentinel + cold-path inline strategy) is in flight — the refutation
specifically falsifies the scoped-down (mask + LUT only) shape, not the
full design.  This is the most important "fail closed" datum from the
cohort: typed-event cursor work needs the complete bounds-elision +
cold-path discipline, not just the LUT.

## (c) Two-pathology-class fix prescription

A single NEON kernel pair closes 5 / 5 failing corpora
(`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §3).

**Class A fix — NEON `match_tiny_plain_string` 16-byte class check**
(`skinny/crates/runtime/src/grammars/json/generated.rs:161–172`).
Target corpora: github_events, update-center, random.  Replaces the
scalar 8-byte `ldrb / cmp #0x22 / b.eq / cmp #0x5c / b.eq / cmp #0x20
/ b.hs` cascade with three NEON class masks under `vorrq_u8` fusion,
compressed movemask via `vshrn_n_u16<4>`, and find-first via tzcnt-
equivalent on the low 64-bit lane.  A single source-level edit lands
both inlined call sites at PCs `0x2734` (key) and `0x3158` (value)
because LTO-fat duplication is the precondition, not a defect (Wave 2
Agent 3; `skinny/profile/wave2-asm/PROFILE-REPORT.md` Appendix C).
Aggregate self-time savings: 50.1% on github_events, 59.9% on
update-center, 51.5% on random.

**Class B fix — NEON TBL-driven `\uXXXX` hex decode**
(`unescape_json_string`, called from
`skinny/crates/runtime/src/grammars/json/parser.rs:78–113`).
Target corpora: unicode_escapes, y_string_unicode.  Replaces the
scalar `sub/csel/orr/lsl` cluster (× 4 nibbles, 11-op dependency
chain) with a `vqtbl1q_u8` 16-byte LUT mapping `'0'..'9' | 'a'..'f' |
'A'..'F'` → nibble value (and `0xFF` sentinel for invalid), then
`vshl` + `vorr` to pack into a `u16`.  3 ops/nibble vs 11 — expected
2–3× speedup, closing the smallest absolute gap on a NO-GO corpus
(unicode_escapes 587 MiB/s vs simdjson C++ 672;
`skinny/profile/native-sidecars/PROFILE-REPORT.md` §(b)).

Both kernels MUST clear the parity harness before any bench claim:
`BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test
checkasm_parity` returning zero divergences
(`skinny/crates/bbnf-simd/tests/checkasm_parity.rs`).

## (d) AVX-512 esoterica stack — strict additive lifts atop asmjson

The Wave 1 Agent 3 esoterica survey identified 5 strict additions, each
of which improves asmjson's published architecture without replacing
its FSM (`restart/skinny/audit/SOTA-BEAT-DESIGN.md` §3.3 and §5;
`restart/locks/14-LOCKS.md` Lock 16).

| Esoterica | Replaces | Win |
|---|---|---|
| GFNI `vgf2p8affineqb` classifier | 6× `vpcmpeqb` classify | 2× over PSHUFB; one fused 8-bit affine transform |
| k-mask arithmetic family (`kshiftl/kshiftr/kxor/kandn`) | 4× store+load per chunk for mask carry | mask state lives in opmask regs; eliminates the memory hop |
| `VPCLMULQDQ` at 512-bit width | scalar `_mm_clmulepi64_si128` prefix-XOR | 4× simdjson's 128-bit prefix-XOR string-bitmap |
| AVX-IFMA `vpmadd52luq` mantissa | f64 mantissa scaling that asmjson punts to Rust | digit-block MAC in 52-bit-lane integer |
| VNNI `vpdpbusd` digit-block MAC | scalar accumulate × 8 in `parse_number` | 4-byte MAC per cycle; one cycle for an 8-digit block |
| BITALG `vpopcntb` + `vpshufbitqmb` | scalar histogram for multi-class classify | one-instruction "where in the alphabet" answer |

Combined projected throughput on Zen 4: **~14 GiB/s = 1.28× asmjson
10.93 GiB/s**.  This stack is conditional on Wave 5 (collapsed-stage
backend) landing the 9-state FSM with PC-as-state direct threading
(`restart/skinny/audit/IMPLEMENTATION-PACKET-V2.md` §5).  The cost
model in `skinny/crates/passes/src/recognizers/backend_shape.rs`
selects `CollapsedStage` only when CPUID reports `avx512vbmi2` AND
the grammar hub has ≥ 4 byte-disjoint arms — so the esoterica stack
is auto-enabled, not directive-driven.

The M5 Max NEON parallel additions (Wave 1 Agent 4) are LD4-interleaved
4-channel classify, BCAX/EOR3 ternary fusion, NEON `svmatch_u8`
emulation, PMU instrumentation hooks, and saturating fast-path number
parse.  AMX is confirmed NOT viable for the hot path: handoff cost
via the store-unit kills the gain on parse-sized buffers.  SME (M4+)
is deferred until silicon access lands (see §(e)).

## (e) Outstanding open questions

**Wave 2 Agent 4 full-prototype event-cursor.**  The scoped-down
prototype refutation
(`skinny/profile/wave2-prototype/PROTOTYPE-REPORT.md`) leaves the
question open: does the *full* prototype (mask + LUT + `ptr`/`end`
sentinel bounds-elision + `#[inline(never)]` cold-path discipline)
recover the 0.18–0.22 c/B projection?  The packet
(`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §3)
requires this prototype to land before Wave 1 closes.  Provisional code
exists at
`skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs`
(360 lines) behind the `eventcursor` cargo feature.

**SME (Scalable Matrix Extension, M4+) prototype is deferred.**  AMX is
ruled out (Wave 1 Agent 4); SME has equivalent reach and a cleaner
load path, but Apple Silicon SME silicon is not yet available in the
dev fleet.  Deferred to a tranche-after-next probe — not on the
SK-V3 close gate.

**asmjson native Zen 4 measurement is anchored by published numbers
only.**  The native-sidecars cross-corpus table
(`skinny/profile/native-sidecars/PROFILE-REPORT.md` §(a),(b)) carries
asmjson's Zen 4 AVX-512 DOM at 11192 MiB/s as a published anchor;
the dev fleet has no Zen 4 AVX-512 host for a true measured row.
The `asmjson/NOTE.md` sidecar reports M5 Max native SWAR at
3315 / 2447 / 669 MiB/s on `string_array` / `string_object` / `mixed`
synthetic corpora — that is the only first-party measured asmjson
row on host hardware.  Wave 6 (x86_64 strict SOTA path,
`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §8)
gates on Zen 4 silicon access.

**Plan C capacity re-examination depends on event-cursor landing**
(`skinny/profile/wave2-capacity/CAPACITY-REPORT.md` §4 rejected-route
table; `skinny/REDRESS.md`).  Plan C ran a separate one-shot SIMD
pre-scan and discarded the position vector; once Wave 1's
`attach_structural_index` produces the structural-index as the
event-cursor input, plan C's pre-scan cost becomes free.  The
re-examination ticket is filed against the post-Wave-1 re-baseline.
