# SK-V15 S-P2 Research V1 - CH4 COST

Pass: S-P2 Research. Cycle: V1.
Lens: CH4 COST.
Output: `restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH4.md`.

## Overall Verdict

**REVISE.**

The packet mostly satisfies the scalar-reference, checkasm/parity, same-wave-consumer, and no-orphan-kernel parts of CH4. P2-B defines the admission stages explicitly: scalar oracle, SIMD/ASM dispatch, strict checkasm, same-wave consumer, and manifest/lock closure (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:26`-`32`). It also states that source-present primitives must end as `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`, and that orphan intrinsic/ASM files do not close Lock 16 (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:41`).

The cost half is not yet acceptable. Orchestrator CH4 requires **LOC budget, risk class, wave alignment, and hard cap** to be stated and realistic (`restart/prompts/ORCHESTRATOR.md:81`-`87`). The S-P2 prompt adds the scalar/checkasm/same-wave minimum and says any candidate missing one of those three fails CH4 (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`124`). The P2 candidate tables carry many of the minimum gates, but they do not provide per-candidate LOC budgets, implementation risk classes, wave alignment, or hard caps. Survivors therefore need a cost block before S-P3 shortlisting.

## Line-Cited Findings

1. **CH4 authority is stricter than the current candidate tables.** The universal CH4 lens requires realistic LOC budget, risk class, wave alignment, hard cap, and same-wave consumer (`restart/prompts/ORCHESTRATOR.md:81`-`87`). The S-P2 specialization requires scalar-reference status, checkasm-parity expectation, and same-wave-consumer notes per candidate (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`124`).

2. **P2-B correctly imports the no-orphan discipline.** Its candidate admission gate requires scalar oracle, candidate path, strict checkasm, same-wave consumer, and manifest/lock closure (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:26`-`32`). The local checkasm report independently records admitted primitive consumers (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:229`-`239`) and keeps `BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and `FSM_DISPATCH_THREADED` blocked until real consumers exist (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:250`-`253`).

3. **The missing CH4 field is implementation cost.** P2-A's candidate table has shape, scalar-ref status, arch, antecedents, and lock status, but no checkasm, same-wave consumer, LOC budget, risk class, or hard cap columns (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:44`-`54`). P2-B/C/D/F tables add checkasm and consumer columns in several places, but still omit cost/hard-cap columns (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:43`-`58`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:24`-`35`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:34`-`41`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:24`-`57`).

4. **Standalone SIMD/ASM body fills are consistently rejected in prose.** P2-D says any SIMD mask/count/write primitive needs scalar reference, parity/checkasm, and same-wave consumer, and a local mask producer without them is an orphan kernel (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50`-`58`). P2-E similarly marks standalone SIMD body fills as non-candidates because every Layer-0 body needs scalar reference, strict checkasm, and same-wave consumer before product routing (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:240`-`248`).

5. **REDRESS-blocked instruction routes are handled correctly.** P2-C rejects CSSC CTZ bulk emission and PMULL prefix-XOR promotion because prior correctness/checkasm did not save the production rows (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:33`-`35`, `:70`-`:72`). P2-F carries those forward as rejected surfaces (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:81`-`90`, `:110`-`:114`).

## Candidate Dispositions

### P2-A Comparator-Derived Candidates

These rows are useful comparator context, but P2-A alone does not meet CH4 because it omits checkasm expectations, same-wave consumers, and cost budgets.

| Candidate | CH4 finding | Disposition |
|---|---|---|
| `byte_class_mask_64<const CLASS>` | Scalar status and local-temp lock status are present (`p2a-sota-teardown.md:48`), but checkasm/consumer/cost are absent in P2-A. | REVISE |
| `skip_byte_set_run` | Scalar loop status and no-retained-cursor risk are present (`p2a-sota-teardown.md:49`), but checkasm/consumer/cost are absent in P2-A. | REVISE |
| `scan_string_event_64` | Scalar status and same-call lock risk are present (`p2a-sota-teardown.md:50`), but checkasm and cost are absent in P2-A. | REVISE |
| `decode_escape_run` | Scalar reference is explicitly missing and mandatory (`p2a-sota-teardown.md:51`); REDRESS materializer risks are called out (`p2a-sota-teardown.md:74`). | REVISE |
| `raw_number_span_classify` | Scalar-start status exists, but checkasm expectation, same-wave consumer, and cost budget are not stated (`p2a-sota-teardown.md:52`). | REVISE |
| `container_skip_local_count_64` | Scalar requirement and retained-cursor block are present (`p2a-sota-teardown.md:53`), but checkasm and cost are absent. | REVISE |
| `tape_reserve_upper_bound` | Non-ISA scalar/control reference is present (`p2a-sota-teardown.md:54`), but equality gates and cost budget need to be explicit. | REVISE |

### P2-B Admission Process And bbnf-simd Inventory

| Candidate / process unit | CH4 finding | Disposition |
|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` | Existing scalar, checkasm floor, and generic structural scanner consumer are named (`p2b-dav1d-process.md:47`; `CHECKASM-REPORT.md:234`-`237`). New grammar alphabets still need per-row cost. | ACCEPT |
| `BYTE_CLASS_FROM_EQ_SET_64` | Scalar, AArch64 body, strict checkasm, and existing scanner consumer are named (`p2b-dav1d-process.md:48`). New string consumers must be same-wave. | ACCEPT |
| `BITMAP_PREFIX_XOR_64` | Scalar/checkasm and JSON string-region consumer are named; PMULL hot-body promotion is rejected (`p2b-dav1d-process.md:49`). | ACCEPT |
| `BITMAP_NEXT_SET_BIT` + `BULK_EMIT_POSITIONS_64` | Scalar/checkasm and `compact_mask` consumer are named; CSSC production route is rejected (`p2b-dav1d-process.md:50`). | ACCEPT |
| `EOB_PAD_CLAMP` | Scalar/checkasm and JSON scan tail consumer are named (`p2b-dav1d-process.md:51`; `CHECKASM-REPORT.md:239`). | ACCEPT |
| `escape_mask_64` correctness prerequisite | Scalar/checkasm evidence exists, but the checkasm report says W2 admits no throughput row or new SIMD primitive (`CHECKASM-REPORT.md:123`-`125`); P2-B names no throughput consumer (`p2b-dav1d-process.md:52`). | REVISE |
| UTF-8 block validation | Block checkasm exists, but P2-F says run-level parity is still required before Layer-1/product routing (`p2f-grammar-neutral.md:34`, `:55`). | REVISE |
| `unescape_uxxxx_x4` / unicode escape SIMD | P2-B states production use still needs dedicated checkasm for exact shape and caller (`p2b-dav1d-process.md:54`). | REVISE |
| Long-string special-byte scanner / string block | P2-B says scalar oracle and dedicated deterministic/random/boundary checkasm are still needed (`p2b-dav1d-process.md:55`). | REVISE |
| Direct parser cursor / whitespace skip primitive | P2-B says extraction needs its own checkasm/equality cell and same-wave caller (`p2b-dav1d-process.md:56`). | REVISE |
| Tape/allocation materialization pressure | Correctly rejected as SIMD/ASM primitive and routed to equality/corpus gates instead (`p2b-dav1d-process.md:57`). It still needs a concrete cost-model budget in P2-D/P3. | REVISE |
| Schema-shaped generated product builder / harness hash rows | No primitive oracle and not eligible as parser primitive (`p2b-dav1d-process.md:58`). | REJECT |

### P2-C Host-Arch Candidates

| Candidate | CH4 finding | Disposition |
|---|---|---|
| A64-CLASSIFY-TBL4-STRUCTURAL | Current JSON consumer exists, but new grammar use needs explicit scalar references and checkasm; no LOC/risk/hard cap is stated (`p2c-arch-esoterica.md:28`). | REVISE |
| A64-MOVEMASK-U8X16-COMPARE | Requires bit-order scalar reference and mandatory checkasm for new forms; no standalone mask producer allowed (`p2c-arch-esoterica.md:29`). Cost still missing. | REVISE |
| A64-WIDE-SHIFT-MOVEMASK-FOLD | Correctly framed as implementation detail under a real primitive; naked primitive has no semantic oracle (`p2c-arch-esoterica.md:30`, `:43`). | REVISE |
| A64-UDOT-DIGIT4-MAC | Scalar fallback exists, but exhaustive `0000..9999` and invalid-byte checkasm are still required (`p2c-arch-esoterica.md:31`). Cost still missing. | REVISE |
| A64-TBL-UNICODE-X4-BATCH | Existing single-quartet route is blocked; batch form needs mixed-validity checkasm and a same-wave consumer (`p2c-arch-esoterica.md:32`). | REVISE |
| A64-CSSC-CTZ-BULK-EMIT | P2-C says no admissible same-wave consumer exists in SK-V15 and REDRESS 89 blocks revival (`p2c-arch-esoterica.md:33`, `:71`). | REJECT |
| A64-PMULL-PREFIX-XOR | P2-C says no admissible same-wave consumer exists and REDRESS 88 blocks default hot-body promotion (`p2c-arch-esoterica.md:34`, `:70`). | REJECT |
| X86-DIAGNOSTIC-ONLY | SK-V15 admission is Apple M5 Max/aarch64; x86 is diagnostic only (`p2c-arch-esoterica.md:14`, `:35`, `:59`). | REJECT |

### P2-D Same-Substrate Tape Candidates

These are not SIMD/ASM kernel admissions by default, so checkasm is replaced by equality/materialisation parity unless an ISA body is introduced.

| Candidate | CH4 finding | Disposition |
|---|---|---|
| `offset_tape_capacity_policy_v2` | Scalar/equality oracle and required parity are named; no checkasm unless SIMD count oracle appears (`p2d-substrate-tape.md:38`). Needs LOC/risk/hard cap. | REVISE |
| `sparse_flag_same_tape_access` | Non-ISA parity and SIMD bit-packing condition are named (`p2d-substrate-tape.md:39`). Needs same-wave lazy consumer cost. | REVISE |
| `same_tape_fact_projection` | Token/fact/direct-product parity and no-checkasm-unless-SIMD rule are named; same-wave consumer must be generated fact/output work (`p2d-substrate-tape.md:40`). Needs cost and hard cap. | REVISE |
| `mask_to_tape_writer_local` | Scalar-vs-SIMD parity and row movement are required, and retained `StructuralIndex` is rejected (`p2d-substrate-tape.md:41`). Near REDRESS 96/97/98, so cost/risk must be explicit. | REVISE |

### P2-E parse-that Gap Candidates

P2-E is the strongest source for scalar/checkasm/consumer notes, but it still lacks per-candidate implementation budgets and hard caps.

| Candidate | CH4 finding | Disposition |
|---|---|---|
| PTG-WS-BYTESET-RUN / `skip_byte_set_run` | Scalar sketch, checkasm expectation, and same-wave JSON/non-JSON consumers are present (`p2e-parse-that-gaps.md:41`-`49`). Cost missing. | REVISE |
| PTG-STRUCT-DISPATCH-LOCAL / `classify_local_block_64` | Scalar reference, strict checkasm expectation, and same-wave consumer are present; standalone body is barred (`p2e-parse-that-gaps.md:74`-`82`). Cost missing. | REVISE |
| PTG-PLAIN-LITERAL-SPAN / `bounded_plain_literal_span` | Scalar sketch, SIMD checkasm cases, and same-wave consumers are present (`p2e-parse-that-gaps.md:107`-`115`). Cost missing. | REVISE |
| PTG-UTF8-RUN-VALIDATE / `validate_utf8_run` | Scalar run oracle, run-level parity expectation, and string/literal consumers are present (`p2e-parse-that-gaps.md:138`-`146`). Cost missing. | REVISE |
| PTG-DIGIT-RUN-ACCUMULATE / `digit_run_span_accumulate` | Scalar factoring, UDOT checkasm requirement, and numeric consumers are present (`p2e-parse-that-gaps.md:171`-`179`). Cost missing. | REVISE |
| PTG-ESCAPED-SEGMENTS / `escaped_literal_segments` | Scalar segment reference, current smoke insufficiency, and same-wave consumers are present (`p2e-parse-that-gaps.md:207`-`215`). Cost missing and REDRESS materializer risk is high. | REVISE |

### P2-F Cross-Artifact Verdict Table

P2-F correctly consolidates grammar-neutral dispositions, but it is not enough for CH4 because its table has no cost budget or hard-cap column (`p2f-grammar-neutral.md:24`-`57`). Its ACCEPT rows remain **REVISE under CH4** until cost blocks are attached; its explicit REJECT rows stand.

| P2-F surface | CH4 disposition |
|---|---|
| Byte-set/classifier ACCEPT rows (`BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, TBL4, movemask, `skip_byte_set_run`, `classify_local_block_64`, `bounded_plain_literal_span`) | REVISE unless already-existing bbnf-simd floor with current consumer; add LOC/risk/wave/hard-cap before S-P3. |
| Encoding/string validator ACCEPT rows (UTF-8 block/run, long-string special-byte scanner) | REVISE; run-level parity and same-wave consumer cost are still pending (`p2f-grammar-neutral.md:34`, `:36`, `:55`). |
| Digit ACCEPT rows (A64 UDOT, PTG digit run) | REVISE; exhaustive scalar/checkasm plus numeric consumer cost are pending (`p2f-grammar-neutral.md:43`, `:56`). |
| Same-tape ACCEPT rows (capacity, sparse flags, fact projection, local mask writer) | REVISE; equality/materialisation gates are named, but implementation cost and hard caps are absent (`p2f-grammar-neutral.md:48`-`51`). |
| PMULL hot body, CSSC CTZ bulk consumer, retained sidecars, schema/harness rows, x86 diagnostic routes | REJECT as P2-F states (`p2f-grammar-neutral.md:81`-`90`). |

## Required V2 Fold-In

Each non-REJECT candidate needs a small CH4 cost block before it can be shortlisted:

```text
CH4 cost:
- Scalar reference: existing / missing / new, with file or planned file.
- Parity gate: checkasm_<name> or equality/materialisation command.
- Same-wave consumer: exact production caller or deletion/rejection row.
- Cost budget: estimated LOC by crate/file, test LOC, and benchmark/equality rows touched.
- Risk class: low / medium / high, with REDRESS pre-blocks.
- Wave alignment: same-wave implementation, test, consumer, measurement, rollback.
- Hard cap: maximum LOC/time/complexity before delete or demote to diagnostic.
- Orphan risk: wired / scalar-delegate / deleted / REDRESS-blocked.
```

Until that block exists, S-P3 may consume the current packet as research inventory, but should not treat the non-REJECT rows as CH4-accepted implementation candidates.
