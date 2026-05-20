# SK-V12 S-P2 CHALLENGE V1 - CH2 GENERALITY

Disposition: ACCEPT.

Lens: CH2 GENERALITY. Lock 14 requires every proposed intervention to be
grammar-neutral and work for CSS L4, Sheets, and BBNF-self, or to be reframed
as a per-grammar template surface (`restart/prompts/skinny/PASS-2-RESEARCH.md:102`;
`restart/prompts/skinny/PASS-2-RESEARCH.md:107`;
`restart/prompts/ORCHESTRATOR.md:83`; `restart/locks/LOCKS.md:78`).

## Findings

1. P2-F supplies the required generality verdict surface. It defines eight
   primitive families, six conditionally eligible parser/support families, one
   oracle-only family, and one accounting-only family
   (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:23`;
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:36`).
   Its verdict table gives concrete Lock 14 status for F1-F8:
   byte-set/classifier/movemask, bounded string span, escape/hex decode, digit
   span, layout skip, FIRST/prefix dispatch, output digest/oracle, and
   tape/direct output accounting
   (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:40`;
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:49`).

2. P2-A candidates are covered by P2-F verdicts, not JSON-only:
   `class_mask64_transient` maps to F1; `bounded_special_byte_string_end` maps
   to F2; `escape_segment_hex_run_decode` maps to F3; `digit_run_accumulate`
   maps to F4; `separator_pair_probe_direct` and
   `generated_first_set_dispatch_template` map to F6; and
   `output_plane_event_sink_contract` maps to F7/F8
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`;
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:111`).
   P2-A's own generality table says these pass only when tables, branch arms,
   output products, and policy are generated or caller-owned
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:119`;
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:127`).

3. P2-B's twelve admission gates are covered by P2-F or explicitly non-parser.
   Byte-class, structural-terminator-as-class, string-block, hex-quartet,
   digit-span, bitmap, tail, and movemask gates all remain support primitives
   with grammar-owned callers; `OUTPUT_DIGEST_HASH_ORACLE_GATE` is explicitly
   grammar-neutral only as output verification, not parser semantics
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:36`;
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:49`;
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:55`;
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:68`).
   No P2-B gate is allowed to carry a JSON role enum, retained class column, or
   JSON surrogate policy into `bbnf-simd`
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:38`;
   `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:42`).

4. P2-C's eight AArch64 candidates are generality-clean under P2-F's F1-F6
   support families. TBL/TBX classifiers, LD4 classifiers, UDOT digit spans,
   wide string scans, hex quartet decode, ASCII set skipping, transient mask
   support, and SHA3 ternary folds are all framed as generated-byte-set,
   caller-policy, transient-mask, or boolean-helper primitives
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:40`;
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:102`;
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:106`;
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:113`).
   The two higher-risk shapes are correctly conditional: LD4 is not neutral if
   it creates a second scan stream, and mask emit/CTZ support may not persist
   positions or introduce a sidecar
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:107`;
   `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:112`).

5. P2-D is covered or rejected. `offset_tape_capacity_policy`,
   `sparse_flag_lookup_policy`, and `retained_cursor_skip_projection` map to
   P2-F F8 and remain guarded/diagnostic same-tape candidates;
   `structural_class_lane_union` is explicitly rejected as the falsified W3
   parallel-substrate route
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:69`;
   `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:78`).
   P2-D's generality section keeps flag meanings, cursor facts, and view rules
   in generated per-grammar metadata or the existing tape API, not generic
   grammar-name branches
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:88`;
   `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:112`).

6. P2-E's five parse-that candidates are covered by P2-F F2-F5/F1 and carry
   caller-owned policy. `pt_byte_set_run_skip` uses a neutral `ByteSet`;
   `pt_bounded_plain_string_end` parameterizes delimiter/escape/control/UTF-8;
   `pt_digit_run_span_accumulate` scans only ASCII digit runs;
   `pt_hex_quad_decode` returns a four-nibble code unit only; and
   `pt_escaped_string_segments` requires caller-supplied escape tables and
   scalar validity policy
   (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:50`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:58`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:95`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:153`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:222`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:284`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:360`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:370`).

7. The CSS L4 / Sheets / BBNF-self expression is concrete enough for CH2.
   P2-F cites CSS identifiers, escaped selector identifiers, and strings;
   Sheets numbers, doubled-quote strings, error literals, cell/range spans,
   operators, and primary/function dispatch; and BBNF-self identifiers,
   literal/regex spans, comments, directives, rules, and value-expression
   numbers (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:19`;
   `grammar/css/l4/tokens.bbnf:7`; `grammar/css/l4/tokens.bbnf:9`;
   `grammar/css/l4/values.bbnf:37`; `grammar/css/l4/values.bbnf:69`;
   `grammar/google-sheets/google-sheets.bbnf:6`;
   `grammar/google-sheets/google-sheets.bbnf:12`;
   `grammar/google-sheets/google-sheets.bbnf:34`;
   `grammar/google-sheets/google-sheets.bbnf:63`;
   `grammar/google-sheets/google-sheets.bbnf:125`;
   `grammar/bbnf/bbnf.bbnf:9`; `grammar/bbnf/bbnf.bbnf:17`;
   `grammar/bbnf/bbnf.bbnf:75`; `grammar/bbnf/expressions.bbnf:6`).

8. JSON-overfit is not admitted. The artifacts repeatedly preserve the SK-V12
   generated non-JSON baseline priority and treat JSON-only telemetry as
   diagnostic, not proof (`skinny/RESULTS.md:143`; `skinny/RESULTS.md:146`;
   `skinny/REDRESS.md:3313`; `skinny/REDRESS.md:3342`;
   `skinny/REDRESS.md:3531`; `skinny/REDRESS.md:3549`;
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:59`;
   `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:141`;
   `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:376`).

## Revise List

None for CH2. Carry the conditional clauses into S-P3: generated metadata owns
grammar byte sets and policy; SIMD masks stay transient; output digest remains
oracle-only unless a real row-owned product consumes it; and no JSON-only guard
or direct residual route can be shortlisted before the generated non-JSON
baseline authority exists.
