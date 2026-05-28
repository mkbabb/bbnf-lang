# SK-V16 P2-B: DAV1D/FFmpeg Checkasm Process

Pass: S-P2 Research. Cycle: V16.
Date: 2026-05-28.
Scope: scalar-oracle-first SIMD process mapped to `bbnf-simd`.
Output: this file.
P1 hot-leaf antecedents: scanner/string, scanner/whitespace, scanner/number, structural scan, tape/view.
Lock surface: both.

## Section 1 - Findings

The reusable process is not "write assembly." It is:

1. executable scalar oracle first;
2. differential harness over adversarial and randomized inputs;
3. ABI and fault discipline for any raw assembly boundary;
4. target-feature dispatch that never claims unsupported instructions;
5. same-wave consumer before admission.
6. negative controls where the harness supports them, so a deliberate bad
   candidate or clobber fails closed rather than silently passing.

External source anchors:

- VideoLAN checkasm exposes fixed-seed runs, function filters, benchmark modes,
  and structured benchmark formats at https://www-test.videolan.org/projects/checkasm/.
- The same page states the fork lineage from dav1d's internal checkasm and
  earlier FFmpeg/x264 checkasm copies.
- FFmpeg checkasm keeps reference-function selection in `checkasm_check_func`,
  failure accounting in `checkasm_fail_func`, and byte/word comparisons in
  `checkasm_check_*`. Source:
  https://sources.debian.org/src/ffmpeg/7%3A4.3.7-0%2Bdeb11u1/tests/checkasm/checkasm.c/.

Local state:

- `skinny/xtask/src/main.rs:14`-`25` lists the primitive checkasm tests.
- `skinny/xtask/src/main.rs:2225`-`2229` runs each primitive under
  `BBNF_SIMD_STRICT=1`.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16`-`24` documents strict
  parity mode.
- Existing primitive tests cover byte-class table/equality, bulk emit,
  structural terminator, prefix XOR, next-set-bit, escape mask, EOB clamp,
  parity, and UTF-8 block.

## Section 2 - Candidate Primitives

| Candidate | Shape | Scalar-ref status | Checkasm parity | Same-wave consumer |
|---|---|---|---|---|
| `byte_class_from_table_64` | 64-byte table classifier | existing scalar in `src/scalar/byte_class_from_table_64.rs` | existing `checkasm_byte_class_from_table_64` | JSON structural scan, future CSS/Sheets byte-set scan |
| `byte_class_from_eq_set_64` | first/member mask over <=8-byte set | existing scalar in `src/scalar/byte_class_from_eq_set_64.rs` | existing `checkasm_byte_class_from_eq_set_64` | `find_ascii_set_member64`; CSS delimiter search candidate |
| `escape_mask_64` | escaped-byte mask from backslash mask and carry | existing scalar in `skinny/crates/bbnf-simd/src/lib.rs:175`-`205` | existing `checkasm_escape_mask_64` | JSON string scan only if row-local profile still names it |
| `string_special_block_16` | terminator/escape/control/non-ASCII masks | scalar exists in `aarch64/string_block.rs:31`-`53` | `checkasm_parity.rs:626`-`630` covers scalar vs NEON; add a dedicated test only if S-P3 exposes it as a public primitive | JSON/CSS/BBNF string lexers |
| `hex_quad_decode_x4` | four hex quartets to codepoints, scalar-identical reject | scalar exists in `aarch64/unescape_uxxxx.rs:40`-`56` | `checkasm_utf8_block.rs:59`-`65` covers x4 parity; add a wider malformed/surrogate test if promoted | Unicode/escape consumer only with new generated semantic owner |
| `digit_block_accumulate` | digit classification and value accumulation | scalar exists through parse-that number scanner; DotProd 4-digit body exists | missing widened parity for 8/16-digit body | generated number parser, CSS dimensions, Sheets cell indices |
| `tape_cursor_step` | scalar cursor/kind/offset primitive | scalar-only; no SIMD claim | unit/golden view parity, not checkasm | generated value/view accessors |

## Section 3 - Grammar-Neutrality

The checkasm process admits only byte, mask, position, or tape operations. The
test file names may mention a current consumer, but the primitive API must not.
Generated per-grammar data supplies delimiter sets, class tables, numeric
policies, and tape-kind maps.

`string_special_block_16` is admissible only if parameterized by terminator,
escape, and control limit. A JSON-only quote/backslash function repeats Lock 14
leakage.

`hex_quad_decode_x4` is admitted at most as hex decode. JSON surrogate joining,
CSS escapes, and Sheets escapes are per-grammar semantic consumers above it.

S-P3 must reject any shortlist entry that does not specify all of:

- P1 row and hot leaf;
- scalar oracle path;
- strict `primitive-checkasm` command for SIMD/native paths;
- same-wave consumer path and row target;
- Lock 1 retention lifetime;
- Lock 14 policy owner;
- REDRESS non-reopen proof;
- cold same-workload measurement plan.

## Section 4 - Risks

- A primitive with tests but no same-wave consumer is an orphan and fails CH4.
- `BBNF_SIMD_STRICT=1` is mandatory for admission. Non-strict parity output is
  exploration only.
- No x86 macro body can satisfy SK-V16. Existing x86 files remain context and
  are not implementation scope.
- Any raw `asm!` or external assembly must add AAPCS64 ABI preservation and
  recoverable fault reporting before production admission. Current Rust closure
  tests are useful but not equivalent to a raw ABI shim.
- PMULL/CSSC/DotProd may be candidates, not proof. REDRESS 88/89 already
  reject production promotion from ISA availability alone.

## Section 5 - Sources

- VideoLAN checkasm project: https://www-test.videolan.org/projects/checkasm/
- FFmpeg checkasm source: https://sources.debian.org/src/ffmpeg/7%3A4.3.7-0%2Bdeb11u1/tests/checkasm/checkasm.c/
- `skinny/xtask/src/main.rs:14`-`25`, `:2225`-`:2229`
- `skinny/crates/bbnf-simd/tests/checkasm_*.rs`
- `restart/locks/LOCKS.md:478`-`:511`, `:607`
- `restart/skinny/tranches/sk-v16/research/p1/p1e-hot-leaf-attribution.md`
