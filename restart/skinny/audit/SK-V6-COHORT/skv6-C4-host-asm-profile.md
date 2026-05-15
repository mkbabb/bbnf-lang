# SK-V6 C4 Host-Arch Instruction and ASM Opportunity Map

Date: 2026-05-15
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Discipline: read-only repo. No repository files edited. Report-only output.

## Scope and Inputs

Required SK-V6 inputs read:

- `restart/skinny/audit/SK-V6-COHORT/skv6-A2-dav1d-asm-process.md`
- `restart/skinny/audit/SK-V6-COHORT/skv6-A6-host-asm-instruction-map.md`
- `restart/skinny/audit/SK-V6-COHORT/skv6-B2-checkasm-hardening-plan.md`
- `restart/skinny/audit/SK-V6-COHORT/skv6-B5-primitive-gap-inventory.md`
- `restart/skinny/audit/SOTA-BEAT-DESIGN.md`

Additional local evidence used because this C4 task asks for current hot
retained/direct symbol mapping:

- `restart/skinny/audit/SK-V6-COHORT/skv6-B3-profile-retained-three-way.md`
- `restart/skinny/audit/SK-V6-COHORT/skv6-B4-profile-direct-three-way.md`
- `skinny/profile/wave2-asm/PROFILE-REPORT.md`
- `skinny/profile/wave2-pmu/PMU-REPORT.md`
- existing cargo-asm files in `skinny/profile/asm/*.s`
- existing Samply sidecars in `skinny/profile/wave2-asm/*.syms.json` and
  `skinny/profile/wave2-pmu/*.syms.json`
- existing built binaries `skinny/target/release/profile-lazy` and
  `skinny/target/release/profile_direct`

No builds or profiling runs were started. Lightweight inspection only:
`nm`, `otool -tV`, `rg`, `sed`, `jq`, `sysctl`.

## Host ISA

Host observed locally:

- `Apple M5 Max`
- `arm64` Darwin host
- present: NEON / AdvSIMD, DotProd, PMULL, CRC32, CSSC, SME/SME2
- unavailable through normal sysctl keys: `FEAT_SVE`, `FEAT_SVE2`

Conclusion follows A6 and the SK-V6 synthesis: the practical implementation
lane for this host is NEON/AdvSIMD plus PMULL, DotProd, CRC32/CSSC-aware scalar
bit ops. Treat SVE/SME as research-only for this parser unless a future
streaming-mode microbench proves transition/state costs are hidden. Do not use
ordinary SVE/SVE2 as an Apple-host close route here.

## Status Read

`SOTA-BEAT-DESIGN.md` is explicitly historical for broad sidecar dispatch:
the sidecar structural-index prepass route is rejected; retained tape
projection is the structural index. Still-live pieces are structural-index
lowering, generated SinkOnly, the `bbnf-simd` primitive vocabulary, and the
x86 `CollapsedStage`/asmjson research shape.

A2/B2 require DAV1D/FFmpeg-grade admission: scalar oracle, forced feature
tiers, raw checked-call shims for ASM, recoverable faults, guard regions, and a
same-wave consumer. AArch64 optional features must be gated by file/region and
dispatch tier, not leaked into generic code.

B5 says the primitive layer is not empty. The current gaps are not broad raw
UTF-8 validation or a missing structural classifier. Current gaps are retained
string matcher control/tail behavior, direct string/Unicode materializer shape,
direct numeric array materialization/emission shape, and generated structural
container cadence.

## Measured Hot Sites: Retained Parse

Freshest retained profile authority is B3 parse-attribution on `twitter`,
`gsoc-2018`, and `unicode_escapes`. These are sampled profiles; percentages
below are measured self-time in that attribution build.

| Corpus | Measured hot retained symbol | Self % | Current host instruction shape | ISA opportunity |
| --- | --- | ---: | --- | --- |
| `twitter` | `match_tiny_plain_string` | 42.26 | source loop is byte-at-a-time `ldrb`, `cmp #0x22`, `cmp #0x5c`, `cmp #0x20`, branch/backedge. Wave2 monolithic disasm saw the same scalar cascade at hot PCs. | Actual target: replace/retune generated retained tiny path with NEON 16B quote/backslash/control probe where row-specific evidence supports it. Candidate instructions: `ldr q`, `cmeq`, `cmhi`, movemask reduction, first-hit via `rbit; clz` or CSSC `ctz` if codegen proves it. |
| `twitter` | `match_string_at_quote` | 18.13 | trusted string matcher calls `skip_json_string_plain_trusted`; current disasm in direct/generated paths shows a NEON block scanner already exists in some direct paths (`cmeq.16b`, `cmhi.16b`, `shrn`, `zip1`, `tbl`, `rbit/clz`) plus scalar tails. | Optimize the boundary/tail shape, not raw UTF-8. A narrower retained-tail scanner is plausible; always-wide and tiny-probe deletion were already rejected in earlier redress. |
| `twitter` | `consume_container_next` | 9.82 | scalar delimiter/whitespace checks: `ldrb`, `cmp`, `ccmp`, SWAR whitespace chunks (`ldr`, `eor`, `add`, `bic`, `and`, `rbit/clz`). | Low/moderate. CSSC `ctz` can shave first-hit extraction if emitted; structural prepass is not the current close. |
| `twitter` | `parse_key_colon` | 5.48 | wrapper around quote consumption, tiny string, fallback string match, colon/whitespace scan. | Opportunity is to reduce helper fan-out and tail re-entry, not a standalone ASM body. |
| `twitter` | `ParserState::emit_plain_offset` | 4.51 | tape push path in existing asm is scalar vector growth check plus `str w`, len update; no NEON value. | Maybe scalar store/offset batching if measured across rows, but not a C4 ISA win. |
| `gsoc-2018` | `match_string_at_quote` | 59.54 | string body dominates; existing direct disasm shows NEON string special scanner pattern (`cmeq`, `cmhi`, `shrn`, `tbl`) where inlined, but retained attribution still says symbol-level string match owns the row. | High-priority measured retained site. Candidate is retained string-tail/control restructuring using existing NEON string-special primitives, not UTF-8 fusion. |
| `gsoc-2018` | `match_tiny_plain_string` | 23.87 | same scalar tiny loop. | Same as above, but row guard required because prior global cap changes regressed or failed other rows. |
| `unicode_escapes` | `match_string_at_quote` | 90.44 | measured as retained string/escape problem. Wave2 monolith saw scalar hex decode clusters (`ldrb`, `sub`, `cmp`, `csinv/csel`, `orr`, `lsl`) inside string/escape path. | High-priority measured site. Existing x4 AArch64 decode is present in `unescape_json_string`, but retained validation/matching still owns cost. Split validation/materialization attribution before adding more SIMD. |

Retained negative evidence:

- `simd_scan::scan_json_parse_index` is not a current B3 hot retained leaf.
- Raw UTF-8 validation is not a current retained close; trusted parse already
  accepts `&str` and uses trusted string matching.
- `canada`, `mesh`, `marine_ik`, and `numbers` retained are already GO rows in
  `skinny/RESULTS.md`; they must be guards for string work.

## Measured Hot Sites: Direct

Freshest direct authority is B4, profiled under Samply with generated Track1,
hand Track2, sonic-rs, and serde_json sidecars on `unicode_escapes`, `numbers`,
and `distinct_values`.

| Corpus/mode | Measured hot direct symbol | Self % | Current host instruction shape | ISA opportunity |
| --- | --- | ---: | --- | --- |
| `unicode_escapes` Track1 | `parse_that_regex::unescape_json_string` | 48.0 | `profile_direct` disasm shows allocation/copy, scalar escape dispatch, SWAR `find_next_escape_or_control`, scalar `\u` decode fallback, and an existing x4 NEON path. Scalar fallback has repeated `ldrb`, `sub`, `cmp`, `csinv`, `orr`, `lsl`. x4 path uses `ld1.s`, `add.16b`, `cmhi.16b`, `and.16b`, `orr.16b`, `uminv`, `tbl.16b`, `ushl`, `umov`, scalar pack/store. | Measured direct site. Best near-term route is materializer shape: run-level decode and UTF-8 emission that avoids per-char `String::push`, not just adding a new SIMD nibble op. Existing NEON x4 decode is real but residual cost is dispatch, segment copy, surrogate/UTF-8 output, and fallback. |
| `unicode_escapes` Track1 | `generated::parse_object_value_at_direct::<JsonDigestSink>` | 42.8 | disasm starts with scalar first-byte dispatch cascade (`ldrb`, `cmp #0x65/#0x73/#0x66/#0x6e/#0x22/#0x2d/#0x5b/#0x7b`), SWAR whitespace, inlined tiny string scalar cap, then inlined NEON string scanning for longer strings. | Mostly codegen/materializer problem. SIMD helps only where parser calls string materialization; direct close is generated DirectBuild/field-layout, not private digest folding. |
| `numbers` Track1 | `generated::parse_array_element_at_direct::<JsonDigestSink>` | 78.6 | scalar dispatch plus number scanner. Source has SWAR 8/4/2 digit scan (`ldr`, `add/sub`, `orr`, `tst`) and decimal materializer. | ISA work is secondary. DotProd `udot` candidate exists for 4-digit chunks, but B4 says Track1/Track2/sonic are already near parity around 11.3 Gbps in profiled pass. Prioritize generated numeric array/materialization shape before SIMD. |
| `numbers` Track1 | `materialize_f64` | 11.1 | Eisel-Lemire scalar: `clz`, `mul`, `umulh`, `adds/cinc`, `ucvtf`, `fmul`, table loads; fallback `from_utf8` + `FromStr`. | Not a NEON lane problem. Possible host scalar cleanup only if profiles isolate fallback/overflow. AVX-IFMA is x86 successor territory, not Apple C4 close. |
| `distinct_values` Track1 | `parse_array_element_at_direct::<JsonDigestSink>` | 52.9 | same generated direct dispatch/string paths as above. | Codegen loop/string fold problem; SIMD only helps if folded into same generated path and measured. |
| `distinct_values` Track1 | `parse_object_value_at_direct::<JsonDigestSink>` | 26.7 | scalar dispatch + string/number/direct sink folding. | Same as above. |
| `distinct_values` Track1 | `JsonDigestSink::array_string::{closure#0}` | 19.9 | disasm is scalar hash/fold over string bytes: 8-byte `ldr` loop, `add`, `eor`, tail `ldrb`/`orr`; one small `add.2d` vector counter update. | This is digest-stressor-specific. Do not charge this as a representative DirectBuild close. |

Direct negative evidence:

- Same-loop scalar-parent folding is already active for generated Track1 direct
  in these rows and does not close Unicode or distinct string rows by itself.
- Field-layout DirectBuild is not available for B4's three corpora, so no ISA
  claim should assume it.
- `numbers` is too close to sonic in the B4 profiled pass to justify a
  SIMD-first intervention.

## Actual Instruction Map From Existing Disassembly

Retained/direct code currently uses these arm64 shapes:

- First-byte dispatch: scalar `ldrb`, `cmp`, `b.eq`/`b.ne`/`b.le` cascades.
- Tiny strings: scalar `ldrb`, `cmp #0x22`, `cmp #0x5c`, `cmp #0x20`, cursor
  increment/backedge.
- SWAR whitespace/plain runs: `ldr` 64-bit chunk, `eor`, `add`, `sub`, `bic`,
  `and/tst`, `rbit`, `clz`.
- Existing string-special NEON in direct/generated paths: `ldr q`, `movi`,
  `cmeq.16b`, `cmhi.16b`, `shrn.8b`, `and.8b`, `orr.8b`, `zip1.8b`,
  `ushl`, `tbl.8b`, `fmov`, `rbit`, `clz`.
- Structural scan cargo-asm: `ldp q`, `and.16b`, `cmeq.16b`, `orr.16b`,
  `addv.8b`, `ext.16b`, `fmov`, scalar prefix-XOR (`eor` shifts), bit
  iteration (`rbit`, `clz`, `ands mask, mask-1`).
- Unicode unescape x4 NEON path: `ld1.s` lane loads, `add.16b`, `cmhi.16b`,
  `and.16b`, `orr.16b`, `uminv.16b`, `tbl.16b`, `ushl`, `umov`, scalar pack.
- Float materialization: scalar integer and FP (`clz`, `mul`, `umulh`,
  `ucvtf`, `fmul`, `adds/cinc`) with fallback to Rust float parse.

Not currently observed as emitted hot retained/direct Apple-close instructions:

- PMULL for prefix XOR.
- CSSC `ctz`; current first-hit extraction observed as `rbit; clz`.
- DotProd `udot` in current hot direct number path; source has a small
  `digit_mac` primitive but B4 does not prove it is on a hot row.
- SME/SME2 in parser hot loops.

## Speculative ISA Ideas, Kept Separate

These are candidates only; none is a measured win in C4.

### NEON / AdvSIMD

1. Retained tiny-string/tail scanner:
   - Candidate instructions: `ldr q`, `cmeq`, `cmhi`, `orr`, compact
     movemask, `rbit/clz` or CSSC `ctz`.
   - Must be row-local and generated-cost-fact controlled. Prior global
     widening/tiny deletion was rejected.

2. Retained trusted string-special tail:
   - Reuse `string_block::scan_string_special_block` style:
     `vld1q_u8`, `vceqq_u8`, `vcltq_u8`, `vcgeq_u8`, movemask.
   - Gate on B3 rows: `gsoc-2018`, `unicode_escapes`, `twitter`, with
     `canada`, `numbers`, `marine_ik`, `instruments` as guards.

3. Direct Unicode run materializer:
   - Existing x4 NEON decode is real. Next plausible shape is batching output
     and reducing `String::push`/segment-copy/fallback control, not adding a
     second equivalent `hex4` primitive.

### PMULL

1. Prefix XOR over quote masks:
   - Candidate instruction: `pmull`; shape is carryless multiply by all-ones.
   - Useful only if fresh profiles attribute retained/direct time to prefix
     quote-region formation. B3 does not currently show scan/prefix as the
     blocker.

### CSSC / Scalar Bit Ops

1. First-hit/next-set-bit:
   - Candidate instruction: CSSC `ctz`, replacing `rbit; clz`.
   - Current disassembly still shows `rbit; clz`; do not claim a CSSC win
     until `cargo asm`/`otool` on the exact binary shows `ctz`.

2. Bit iteration and bulk emit:
   - Current AArch64 wrappers for `bitmap_next_set_bit`, `bitmap_prefix_xor`,
     `bulk_emit_positions_64`, and `byte_class_from_table_64` call scalar.
   - Could be small wins where `emit_plain_offset`/compact emit stays visible,
     but B3/B4 do not make this the primary close.

### DotProd

1. Digit block MAC:
   - Candidate instruction: `udot`; source has `digit_mac::parse_4_digits`.
   - B4 `numbers` does not justify this as first close. It belongs behind a
     row where digit accumulation, not direct array loop/materialization, is
     proven hot.

### CRC32

1. Hash/fold sidecar:
   - `JsonDigestSink::array_string` is a digest-stressor hot leaf on
     `distinct_values`, but this is not representative DirectBuild output.
   - CRC32/hash acceleration may improve the stressor, but should not be used
     as SOTA recovery evidence unless the workload remains a hash workload.

## What Can Outclass asmjson Generally

Generalizable across grammars and not tied to asmjson's JSON-only finite
control:

- DAV1D/FFmpeg process discipline: forced feature masks, checkasm admission,
  checked ABI shims, recoverable faults, and table-oriented dispatch.
- Grammar-neutral byte classification:
  - Arm: NEON `cmeq`/`tbl`/range compare plus movemask.
  - x86: AVX2/AVX-512 equality/table classification.
- Quote/region prefix masks as a primitive:
  - Arm candidate PMULL.
  - x86 PCLMUL/VPCLMUL.
- Bit iteration/position emit:
  - Arm scalar/CSSC next-set-bit, maybe future vector batching.
  - x86 BMI1/BMI2/AVX-512 compressed emit.
- Direct materializer specialization from schema/layout facts:
  - This can beat asmjson generally because asmjson is not a typed
    DirectBuild generator and does not solve representative typed output.
- Retained/direct separation with same-plane strictness:
  - Current BBNF already beats Rust `simd-json` borrowed on the B3 rows; the
    remaining target is sonic/yyjson-class row-specific fusion, not asmjson
    permissive behavior.

## What Stays x86 CollapsedStage-Only

These are not general Apple-host C4 work items:

- `FSM_DISPATCH_THREADED`: PC-as-state `jmp [table + state*8]` is explicitly
  consumed only by per-grammar x86 `CollapsedStage` kernels. It is not a
  recursive-descent leaf primitive.
- Bounded frame push/pop macros modeled after asmjson's object/array stack:
  useful only when the recognizer proves a DPDA-like finite-control fragment
  and strict row semantics are available.
- AVX-512 k-mask residency, `korq`/`kortest`, `kmovq` discipline:
  x86 `CollapsedStage` implementation detail.
- AVX-512 VBMI2 `vpcompressb` offset/string emit:
  x86 successor primitive; no Apple NEON equivalent with the same one-op
  semantics.
- AVX-512 GFNI/BITALG/VBMI/VNNI/IFMA stack:
  possible way to outclass asmjson on Zen 4/Ice Lake+ class hardware, but only
  after strict same-plane x86 rows exist. Not part of current arm64 retained or
  direct close.
- "asmjson clone" claims:
  rejected. The lift is strict grammar-table `CollapsedStage`, not a new BBNF
  directive or JSON-special mode.

## Recommended C4 Ordering

1. Retained string-tail/matcher intervention first:
   - Target measured B3 symbols: `match_string_at_quote`,
     `match_tiny_plain_string`, `parse_key_colon`.
   - Use existing NEON string-special bodies and generated row-specific facts.
   - Measure `twitter`, `gsoc-2018`, `unicode_escapes`; guard `canada`,
     `numbers`, `marine_ik`, `instruments`.

2. Direct Unicode materializer shape second:
   - Target measured B4 symbol: `unescape_json_string`.
   - Use existing x4 NEON decode but reduce run-level control, segment copy,
     and per-char push overhead.
   - Do not call it a new SIMD primitive unless the vector semantics differ
     from current `unescape_uxxxx_x4_neon`.

3. Scalar bit-op cleanup third:
   - Inspect exact built code for CSSC `ctz` before claiming it.
   - Consider only if `emit_plain_offset`, `compact_mask`, or bit iteration
     remains sampled after string/materializer work.

4. Defer PMULL/DotProd/CRC32 unless profiles move there:
   - PMULL only if prefix quote masks show up.
   - DotProd only if digit accumulation is isolated.
   - CRC32/hash only for explicit hash workloads, not representative
     DirectBuild recovery.

5. Defer x86 `CollapsedStage`:
   - It is Wave-7/successor territory after arm64 same-plane rows close or are
     falsified and after strict asmjson-compatible comparison rows are defined.

## Bottom Line

The measured Apple-host hot sites are not raw structural scan, not broad UTF-8
validation, and not a generic missing SIMD crate. Retained misses are string
matcher/tail/escape validation sites. Direct misses are `unescape_json_string`
and generated direct loop/materializer shape. Existing disassembly already
shows a mix of scalar dispatch, SWAR, and NEON string/unescape primitives; the
next wins must be row-local integration and materializer shape, not speculative
ISA inventory.

The most credible arm64 ISA assists are NEON string-special/tail probes and
possibly CSSC first-hit extraction after proof. PMULL prefix and DotProd digit
MAC remain speculative until sampled profiles move there. The route that can
outclass asmjson generally is grammar-neutral primitive admission plus typed
DirectBuild and strict same-plane measurement. The route that stays
x86-only is asmjson-style `CollapsedStage` finite-control dispatch and its
AVX-512 k-mask/VBMI2/GFNI successor stack.
