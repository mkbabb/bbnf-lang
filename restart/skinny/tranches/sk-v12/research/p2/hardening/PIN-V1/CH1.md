# SK-V12 S-P2 PIN-V1 CH1 Correctness

Verdict: REVISE

Score: 86%

## Blocking Findings

1. P2-C still places inventory-only ARM esoterica in the `## §2 - Candidate primitives` section even when the artifact itself says the rows lack a real pin S-P1 antecedent. CH1's S-P2 rule is explicit: every candidate primitive must trace to a named S-P1 hot leaf, and a candidate with no P1 antecedent is speculative (`restart/prompts/skinny/PASS-2-RESEARCH.md:95-100`). The live S-P1 antecedent set is only `bounded_plain_string_scan`, `container_dispatch`, `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`, `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`, `typed_direct_projection`, and `serde_json_oracle_read_parse` (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:60-64`). P2-C C2 says no S-P1 hot leaf proves a real interleaved stream (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:49-56`), C9 says the P1 antecedent is "none sufficient today" (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:112-119`), and C11 admits only an `output_digest_hash` adjacency while no parser primitive or consumer exists (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:130-137`). P2-F agrees these are inventory/drop rather than current candidates (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:43`, `:57-59`). S-P3 needs a candidate pool, not a mixed candidate/inventory list.

2. Two external source anchors used for comparator/process correctness do not resolve to the claimed source lines. P2-A cites yyjson README `#L271-L279` for ANSI C/no-explicit-SIMD/strict features (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:95`), but current upstream README has 267 lines and the cited feature text is at lines 10-15. P2-B cites dav1d `tests/checkasm/msac.c#L1127-L1227` for cloned state, `call_ref`, `call_new`, CDF/state comparison, and `bench_new` (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:98`), but current upstream `msac.c` has 311 lines; the relevant checks are around lines 115-136, 156-175, 184-195, 203-215, and 228-250. This violates the universal CH1 requirement that claims cite a resolving source (`restart/prompts/ORCHESTRATOR.md:81-84`) and S-P2 CH1's comparator/ISA citation rule (`restart/prompts/skinny/PASS-2-RESEARCH.md:95-100`).

3. P2-C's ISA sources are semantically the right family but too broad for CH1-grade citation. The artifact cites generic Arm ACLE pages for TBL/TBX, UDOT, shift/EXT, LD4, PMULL, CSSC, and SHA3 (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:210-214`). The claims themselves are plausible and I verified examples in the Arm docs, but the fold should carry section or line anchors for the concrete intrinsic/instruction assertions before CH1 accepts the citation surface.

## Nonblocking Notes

1. The strictness planes are otherwise correct. The user pin requires generated CSS L4 Track 1 to beat `lightningcss_mbps + 1` on the same corpus, output plane, host, and strict equality (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:29-35`), and the handoff requires one canonical CSS fact stream shared by generated Track 1, oracle/Track 2, and lightningcss (`restart/skinny/tranches/sk-v12/HANDOFF.md:54-58`). P2-A, P2-B, P2-E, and P2-F all preserve that boundary (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:14`, `:53`; `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:36`, `:90`; `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:12`, `:68`; `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:14`, `:18`).

2. CSS L4/lightningcss admission is not claimed prematurely. S-P1 records no generated CSS L4 Track 1 runtime, no same-plane lightningcss row, and no strict equality oracle (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:55-57`). P1-F confirms zero generated CSS L4 rows and no `lightningcss`/`css_l4` entry in `skinny/RESULTS.md` (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80-93`, `:173-210`). The P2 artifacts correctly treat JSON hot leaves as nomination evidence only.

3. x86 implementation work is kept out. The user pin carries x86 out of scope (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:90-99`). P2-C scopes itself to aarch64/Apple Silicon and says x86 is out (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:5`), while P2-A/P2-B/P2-E mention x86 only as comparator or out-of-scope future context (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:20`, `:31`; `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:44`; `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:33-39`).

4. The candidate lists are mostly concrete enough for S-P3 after the inventory cleanup. P2-A names seven primitive shapes with scalar-reference expectations (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:27-39`), P2-B names admission gates with checkasm and same-wave consumer classes (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:39-58`), P2-E names parse-that/API gaps with layers, consumers, and micro-proof needs (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:29-43`), and P2-F gives grammar-neutral verdicts (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:30-60`). P2-D is acceptable as a no-shortlist substrate lane because it explicitly says no P2-D primitive is shortlist-ready this cycle and gives conditional post-W1b apertures (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:100-112`).

## Exact Fold Edits If REVISE/REJECT

1. In `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`, split `## §2 - Candidate primitives` into candidate-eligible rows and "Inventory / not S-P3 candidates in PIN-V1." Move at least C2 `a64_ld4_interleaved_classifier64x4`, C9 `a64_sha3_ternary_mask_fold`, and C11 `a64_prfm_stnp_output_stream_hint` out of the candidate-eligible section, or delete them from §2 entirely. Keep them only as inventory/drop notes with `P1 antecedent: none for PIN-V1` and `S-P3 eligibility: no`.

2. In `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`, mirror that cleanup by marking `a64_ld4_interleaved_classifier64x4`, `a64_sha3_ternary_bool_fold`, `cache_hints`/PRFM/STNP, and any other "inventory/drop" row as outside the candidate pool consumed by S-P3. Do not let P2-F's broad "current P2-B/C/D/E candidate surface" wording imply these are selectable.

3. In `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`, replace source `[Y1]` with a resolving anchor for the actual yyjson feature lines, e.g. `https://github.com/ibireme/yyjson/blob/master/README.md#L10-L15`, or pin to a commit SHA with those lines.

4. In `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`, replace `[D1]` with resolving dav1d anchors, e.g. `https://github.com/videolan/dav1d/blob/master/tests/checkasm/msac.c#L115-L136`, `#L156-L175`, `#L184-L195`, `#L203-L215`, and `#L228-L250`, or pin to a commit SHA with those line ranges.

5. In `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`, add precise Arm reference anchors or section labels beside each ISA claim. At minimum anchor `vqtbl4q_u8`/`vqtbx4q_u8`, `vdotq_u32`/`__ARM_FEATURE_DOTPROD`, `vld4q_u8`, `vmull_p64`/`vmull_high_p64`, `__ARM_FEATURE_CSSC`, and `__ARM_FEATURE_SHA3`/EOR3/BCAX.

## Commands Used

- `rg --files restart | rg '(^|/)(PASS-2-RESEARCH\.md|ORCHESTRATOR\.md|USER-PIN|HANDOFF\.md|CH1\.md|PIN-V1|S-P1|p2|P2)'`
- `pwd`
- `git status --short`
- `rg -n "§3|CH1|3W|3Z|USER-PIN|S-P1|convergence|Convergence|PIN" restart/prompts/skinny/PASS-2-RESEARCH.md`
- `rg -n "§3W|§3Z|3W|3Z|PIN|S-P1|P2|CH1" restart/prompts/ORCHESTRATOR.md`
- `nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `nl -ba restart/skinny/tranches/sk-v12/HANDOFF.md`
- `nl -ba restart/HANDOFF.md`
- `awk 'NR>=1 && NR<=120 {printf "%6d\t%s\n", NR, $0}' restart/prompts/skinny/PASS-2-RESEARCH.md`
- `awk 'NR>=120 && NR<=260 {printf "%6d\t%s\n", NR, $0}' restart/prompts/skinny/PASS-2-RESEARCH.md`
- `awk 'NR>=70 && NR<=125 {printf "%6d\t%s\n", NR, $0}' restart/prompts/ORCHESTRATOR.md`
- `awk 'NR>=125 && NR<=180 {printf "%6d\t%s\n", NR, $0}' restart/prompts/ORCHESTRATOR.md`
- `find restart/skinny/tranches/sk-v12/research/p1 -maxdepth 3 -type f | sort`
- `rg -n "CONVERGED|converged|ACCEPT|Score|Verdict|hot leaf|hot-leaf|PIN-V" restart/skinny/tranches/sk-v12/research/p1 restart/skinny/tranches/sk-v12/HANDOFF.md`
- `awk 'NR>=1 && NR<=80 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `awk 'NR>=1 && NR<=240 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `awk 'NR>=1 && NR<=220 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `wc -l restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `awk '{printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `awk '{printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `awk 'NR>=1 && NR<=120 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `awk 'NR>=121 && NR<=260 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `awk 'NR>=1 && NR<=130 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `awk 'NR>=131 && NR<=270 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `awk '{printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `awk '{printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `rg -n "x86|lightningcss|CSS L4|admit|admission|candidate|P1 antecedent: none|none sufficient|not candidate|not selectable|inventory|drop" restart/skinny/tranches/sk-v12/research/p2/p2*.md`
- `rg -n "cache_hints|LD4|EOR3|BCAX|SHA3|BF16|CSSC|PMULL|VBMI|GFNI|AVX|x86" restart/skinny/tranches/sk-v12/research/p2/p2*.md`
- `rg -n "P1 antecedent|antecedents|S-P1|hot leaf|hot-leaf" restart/skinny/tranches/sk-v12/research/p2/p2*.md`
- `curl -L -s https://raw.githubusercontent.com/cloudwego/sonic-rs/main/README.md | awk 'NR>=55 && NR<=90 {printf "%6d\t%s\n", NR, $0} NR>=430 && NR<=460 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/cloudwego/sonic-rs/main/sonic-number/src/swar.rs | awk 'NR>=1 && NR<=50 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/simdjson/simdjson/master/src/generic/stage1/json_structural_indexer.h | awk 'NR>=1 && NR<=150 {printf "%6d\t%s\n", NR, $0} NR>=190 && NR<=250 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/simdjson/simdjson/master/src/generic/stage1/json_string_scanner.h | awk 'NR>=55 && NR<=90 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/ibireme/yyjson/master/README.md | wc -l`
- `curl -L -s https://raw.githubusercontent.com/ibireme/yyjson/master/README.md | awk 'NR>=1 && NR<=120 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/ibireme/yyjson/master/src/yyjson.c | awk 'NR>=3358 && NR<=3413 {printf "%6d\t%s\n", NR, $0} NR>=3880 && NR<=4045 {printf "%6d\t%s\n", NR, $0} NR>=4758 && NR<=4880 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/ibireme/yyjson/master/src/yyjson.h | awk 'NR>=736 && NR<=834 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/simdjson/simdjson/master/doc/basics.md | awk 'NR>=343 && NR<=349 {printf "%6d\t%s\n", NR, $0} NR>=2828 && NR<=2830 {printf "%6d\t%s\n", NR, $0} NR>=3173 && NR<=3178 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s https://raw.githubusercontent.com/simdjson/simdjson/master/doc/dom.md | awk 'NR>=29 && NR<=35 {printf "%6d\t%s\n", NR, $0}'`
- `curl -L -s -o /dev/null -w '%{http_code}\n' https://raw.githubusercontent.com/videolan/dav1d/master/tests/checkasm/msac.c`
- `curl -L -s https://raw.githubusercontent.com/videolan/dav1d/master/tests/checkasm/msac.c | wc -l`
- `curl -L -s https://raw.githubusercontent.com/videolan/dav1d/master/tests/checkasm/msac.c | rg -n "call_ref|call_new|memcmp|bench_new|cdf|state|msac"`
- `awk 'NR>=70 && NR<=100 {printf "%6d\t%s\n", NR, $0} NR>=170 && NR<=210 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `awk 'NR>=40 && NR<=70 {printf "%6d\t%s\n", NR, $0} NR>=88 && NR<=112 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md`
- `awk 'NR>=135 && NR<=185 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`
- `awk 'NR>=1 && NR<=70 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `find restart/skinny/tranches/sk-v12/research/p2/hardening -maxdepth 2 -type d | sort`
- `ls -la restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V1`
- `awk 'NR>=1 && NR<=80 {printf "%6d\t%s\n", NR, $0}' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V1/CH5.md`
