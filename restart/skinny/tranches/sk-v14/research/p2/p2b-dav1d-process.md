# SK-V14 P2-B: DAV1D/FFmpeg ASM Process — Admission Gate for bbnf-simd

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-23.
Scope: hand-written-ASM SIMD discipline from dav1d / FFmpeg / VLC — scalar-oracle-first, checkasm differential harness, same-wave-consumer rule — mapped onto bbnf-simd's `checkasm_*` tests + scalar reference requirement; output is the primitive-admission process S-P3 gates against.
Output: this file.
P1 hot-leaf antecedents: every S-P1 hot leaf classified `scan` / `string` / `unicode` / `number` / `tape` / `dispatch` (P1-E §1.3 grammar-neutral vocabulary). The admission process is the gate the candidate primitives surfaced by P2-C/D/E (and the grammar-neutral verdicts from P2-F) pass through before S-P3 may shortlist them — therefore the antecedent surface is the full P1-E §2.1/§2.2/§2.4 hot-leaf census, not a subset.
Lock surface: Lock 16 (admissibility allowlist + scalar reference + checkasm parity + same-wave consumer is the Lock 16 closure shape) primarily; Lock 14 secondarily (the admission gate's grammar-policy-source check enforces zero-overfit on every admitted primitive); Lock 1 tangentially (the admission gate's `substrate_target` / `retention_lifetime` / `policy_owner` declaration is the Lock 1 substrate-union enforcement surface).

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### §1.1 — The dav1d/FFmpeg process, in three discipline tenets

The dav1d hand-written-ASM corpus (`/tmp/dav1d-research/dav1d/src/x86/*.asm` family + `/tmp/dav1d-research/dav1d/src/arm/64/*.S` family, vendored at `crates/bbnf-simd/ext/x86/x86inc.asm` per `LOCKS.md:303`) and the FFmpeg `checkasm` harness it inherits (`tests/checkasm/checkasm.{c,h}` in upstream FFmpeg) jointly impose three load-bearing tenets that bbnf-simd has imported in form but only partially in discipline:

1. **The scalar reference is the executable specification.** Every SIMD/ASM primitive in dav1d ships with a portable C reference (typically `src/<unit>.c` with the `_c` suffix; e.g. `src/itx.c` carries `dav1d_inv_txfm_add_dct_dct_4x4_c` as the oracle for every `dav1d_inv_txfm_add_dct_dct_4x4_*_{neon,avx2,sse4}` body). The reference is what `call_ref` invokes in the differential; the SIMD body is `call_new`. The dav1d ASM is not "an implementation"; it is "a checkasm-differential against the C reference". A divergence is a SIMD bug by construction. The bbnf-simd mirror of this discipline is the `scalar/` directory (`crates/bbnf-simd/src/scalar/`); every aarch64 / x86_64 body has a sibling scalar reference under the same name. The shape is correct at HEAD: nine scalar primitives (`bitmap_next_set_bit`, `bitmap_prefix_xor_64`, `bulk_emit_positions_64`, `byte_class_from_eq_set_64`, `byte_class_from_table_64`, `eob_pad_clamp`, `swar_8byte`, plus the two aarch64-only references included via `include!` in the test binary on non-aarch64 hosts at `tests/checkasm_parity.rs:42-55` — `match_tiny_plain_string` and `unescape_uxxxx`). Per dispatch context §0 Lock 16, this is the floor; every new primitive S-P3 admits must extend it.

2. **The checkasm differential harness is the admission gate.** dav1d's `tests/checkasm/checkasm.c` invokes each primitive twice against bit-identical inputs (two heap allocations, `memcpy`'d from the same source), compares both outputs *and* both source buffers post-call (catches kernels that scribble their inputs), sweeps misalignment 0..15, drops a stack canary, installs a `SIGSEGV`/`SIGBUS`/`SIGILL` longjmp trampoline, and reports timing via geometric-robust mean (drop samples where `t * count > sum * 4`). The bbnf-simd port lives at `crates/bbnf-simd/tests/checkasm_parity.rs:1-737`; the docstring at `:1-21` enumerates the six adopted mechanics, the `check_parity_at` impl at `:200-227` performs the dual-buffer compare, the `signal_guard::arm` at `:158-166` installs the SIGSEGV/SIGBUS/SIGILL handler, the `with_stack_canary_xor_fold` at `tests/checkasm_common.rs:50-72` installs the 1 KiB stack canary with xor-fold verification, the `robust_mean_ns` at `tests/checkasm_parity.rs:387-410` implements the geometric-robust-mean outlier filter verbatim from FFmpeg's `checkasm.h`, and the AArch64 callee-saved-register sentinel scheme at `tests/checkasm_common.rs:89-192` (the `x19..x28` save/restore-and-verify) extends the FFmpeg discipline with the dav1d-grade ABI-clobber check absent from upstream `checkasm.h` (per `crates/bbnf-simd/CHECKASM-REPORT.md:46` table). Per the CHECKASM-REPORT.md §d disclosure and `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:361`, the harness has earned its keep: it caught the `escape_mask_64` NEON state-handoff bug (xorshift seed `0xCAFEF00DBAADF00D`, chunk-boundary lookahead miscount) on first invocation — a class of bug scalar review missed. The SK-V12 W2 closure at `skinny/REDRESS.md:3603-3632` ratified the gate as the correctness prerequisite for every Lock 16 admission.

3. **No primitive ships without a same-wave consumer.** dav1d does not admit a `vextq_u8` body and a `udot` body in wave N to be wired into the inverse-transform pipeline in wave N+3 — every ASM file lands wave-coupled to the C code that calls it; the FFmpeg `libavcodec` discipline is identical (every `dsp.c` initialization that exposes a SIMD function pointer is committed in the same patch as the ASM body the pointer references). The bbnf-simd analogue lands at `LOCKS.md:282-307` (Lock 16 closing clause): "Every SIMD primitive carries a unit-parity test against the scalar reference and a corpus-parity test against the expanded skinny corpus … in `crates/bbnf-simd/tests/`" plus the v+1 amendment at `LOCKS.md:326-333`: "`escape_mask_64` is an admitted correctness prerequisite, not a production SIMD/ASM throughput primitive. Its checkasm-backed state covers the historical xorshift falsifier and scanner parity cases, but it admits a row only when a JSON/CSS string or escape consumer wires it in the same wave and moves or rejects the named row under strict comparator evidence." The same-wave-consumer rule was the closure shape for SK-V6 W2 Candidate-2 (`skinny/REDRESS.md:1385`: "reference plus `checkasm_string_block_64`, and a same-wave consumer inside …") and SK-V6 W2 Candidate-3 (`skinny/REDRESS.md:1445`: "scalar reference, AArch64 checkasm parity, and a same-wave consumer inside …") — every Lock 16 admission since SK-V5 W5 has carried the explicit same-wave-consumer cite or has been demoted to "inventory_demoted_with_evidence" per the close-state vocabulary at `LOCKS.md:335-342`.

### §1.2 — Empirical floor at HEAD (what the bbnf-simd checkasm corpus already proves)

Eleven `checkasm_*` test files at `crates/bbnf-simd/tests/`:

| Test file | Primitive under test | Scalar reference path | Strict-mode disposition |
|---|---|---|---|
| `checkasm_ascii_set_member_find_64.rs` | aarch64 set-membership find | `scalar/byte_class_from_eq_set_64.rs` (set-member fold) | strict-green per SOTA-BEAT-DESIGN §6.1 admission roll |
| `checkasm_bitmap_next_set_bit.rs` | CTZ next-bit consumer | `scalar/bitmap_next_set_bit.rs` | strict-green |
| `checkasm_bitmap_prefix_xor_64.rs` | bitmap prefix-XOR (sha-3 `vbcaxq_u8` path) | `scalar/bitmap_prefix_xor_64.rs:1-14` (6-stage shift+xor) | strict-green |
| `checkasm_bulk_emit_positions_64.rs` | NEON tape-position bulk emit | `scalar/bulk_emit_positions_64.rs` | strict-green |
| `checkasm_byte_class_from_eq_set_64.rs` | NEON byte-class fan (set membership against 16-byte alphabet, NEON port of SVE2 `svmatch_u8`) | `scalar/byte_class_from_eq_set_64.rs` | strict-green |
| `checkasm_byte_class_from_table_64.rs` | NEON byte-class via 4×16 table (vqtbl4q_u8 + movemask) | `scalar/byte_class_from_table_64.rs` | strict-green |
| `checkasm_eob_pad_clamp.rs` | end-of-buffer pad/clamp | `scalar/eob_pad_clamp.rs` | strict-green |
| `checkasm_escape_mask_64.rs` | escape-mask state-handoff (the SK-V12 W2 falsifier-prerequisite) | independent byte-walk scalar reference inline | strict-green per SK-V12 W2 closure (REDRESS.md:3603-3632) — admitted as correctness prerequisite only, not as throughput primitive |
| `checkasm_parity.rs` (§Test 1-4) | full structural classifier (NEON dispatch vs scalar dispatch) | `scan_scalar` (`lib.rs`) | strict-green at HEAD per CHECKASM-REPORT.md §c |
| `checkasm_structural_terminator_64.rs` | structural-terminator find | scalar terminator reference | strict-green |
| `checkasm_utf8_block.rs` | UTF-8 block validation | scalar UTF-8 reference | strict-green |

Plus three checkasm-adjacent tests (`aarch64_primitives.rs`, `classifier_parity.rs`, `corpus_parity.rs`) that pre-date the formal checkasm port. The eleven checkasm cells + the corpus-parity cell at `tests/checkasm_parity.rs:344-372` (which always asserts strictly, no env-var gate, per `:20` docstring) constitute the empirical floor S-P3 must extend, not start from scratch.

### §1.3 — The two checkasm modes (strict vs default) and what each means for admission

The harness operates in two modes:

- **Default mode** (`BBNF_SIMD_STRICT` unset): divergences on random/misaligned inputs are *recorded and logged* but do not fail the test. The docstring at `tests/checkasm_parity.rs:16-20` explains the rationale: "to land green in CI while the existing NEON↔scalar handoff bug … is being fixed". The CHECKASM-REPORT.md §c output records 112/448 alignment-sweep cases and 2/32 random trials as logged divergences in this mode.
- **Strict mode** (`BBNF_SIMD_STRICT=1`): same code path, divergences promoted to test failures. Per `LOCKS.md:320-324` v+1 amendment: "Admission checkasm commands run with `BBNF_SIMD_STRICT=1`. Non-strict parity is exploratory only and cannot admit a primitive, route, or row. Every scalar/checkasm/equality failure rejects the candidate for that wave."

Process consequence: S-P3 admission gates the new primitive *only* on strict-mode passing. The default-mode log is exploratory evidence only. Any S-P2 candidate that proposes admission "after the strict failures are downgraded" is a paper-close per CH6.

### §1.4 — Where bbnf-simd's discipline already diverges from dav1d (in bbnf-simd's favour)

Two extensions of FFmpeg `checkasm.h` that the bbnf-simd port adds, both binding for S-P3 admission:

- **AArch64 callee-saved register sentinel verification.** `tests/checkasm_common.rs:89-192` saves `x19..x28`, writes ten distinct sentinels into them, runs the candidate, reads `x19..x28` back, restores the saved values, and asserts the read values equal the sentinels. FFmpeg `checkasm.h` does not do this on aarch64 (it does the equivalent for x86_64 callee-saveds via the `checkasm_check_func` wrapper, but the arm64 port at upstream HEAD relies on the C compiler to flag ABI violations rather than runtime-verifying them). The bbnf-simd extension catches the class of bug where a hand-written `asm!` block forgets to restore an `x19..x28` register — a class scalar review cannot catch.
- **Stack canary xor-fold verification.** `tests/checkasm_common.rs:50-72` writes a 1 KiB xorshift-deterministic pattern into a stack-resident buffer, computes its xor-fold pre-call, runs the candidate, recomputes the xor-fold post-call, and asserts both folds + the canary buffer are byte-identical post-call. FFmpeg's `checkasm.h` has a 256-byte stack canary; bbnf-simd's is 4× larger and the xor-fold provides a numeric witness (not just byte-compare), which makes the failure-mode message diagnosable (`pre_fold=… post_fold=… first_bad_byte=…` per `:67-69`).

Both extensions are load-bearing for S-P3 admission: any new primitive that touches arm64 callee-saveds or the stack must pass them.

## §2 — Candidate "primitives" (admission process stages, each per the §2.1 frontmatter rule)

This agent's scope is the *process*, not new SIMD primitives. The five §2 entries below are therefore the **admission gate stages** S-P3 must run every candidate primitive (from P2-C/D/E) through; each entry carries the shape (what the stage produces), the scalar-reference status (the oracle the stage compares against), the arch (the target ISA the stage runs on), and the P1-antecedent (which P1-E hot leaves drive the stage's existence). The S-P3 shortlist is the set of P2-C/D/E candidates that pass all five stages with `dispatch_disposition = wired`.

### §2.A — Stage A: Scalar-Reference Authoring

**Shape.** A pure-Rust `<primitive>_scalar(input) -> output` function under `crates/bbnf-simd/src/scalar/<primitive>.rs`, no `core::arch::*` use, no `unsafe` (other than `unsafe { core::slice::from_raw_parts(...) }` for pointer-arity matching the SIMD signature). The function is the executable specification — every SIMD body for `<primitive>` is `checkasm`-differentialled against it.

**Scalar reference status.** Self — the stage *produces* the scalar reference. The exemplar is `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1-14`: six `mask ^= mask << k` shifts (k ∈ {1, 2, 4, 8, 16, 32}), optional carry-in inversion, `#[inline]` on the function. The shape applies to every primitive class in the P1-E vocabulary: `scan` (byte-by-byte structural-byte enumeration, as `scan_tail` at `runtime/src/grammars/json/scan.rs:107` already exemplifies), `string` (byte-by-byte plain-string-match-with-membership-table, as `match_tiny_plain_string_scalar` at `aarch64/match_tiny_plain_string.rs` already exemplifies), `unicode` (byte-by-byte hex-nibble decode, as `read_hex_unit_scalar` at `parse-that-regex/src/lib.rs:945` already exemplifies), `number` (byte-by-byte digit-run + accumulate, the reference shape Lock 16 row "x86_64 AVX-512 VNNI vpdpbusd" + "x86_64 AVX-IFMA vpmadd52" must beat), `tape` (positions.push byte-by-byte, the substrate-union shape).

**Arch.** Portable. The scalar reference must compile on every host the SIMD body targets *plus* every host the SIMD body does not target (i.e. it is the cross-arch portable fallback per Lock 16 closing clause).

**P1 antecedent.** Every P1-E hot leaf classified `scan` / `string` / `unicode` / `number` / `tape` (P1-E §1.3). For envelope-bound dispatch leaves (`dispatch_value`, `parse_object_value_at_direct`, `parse_array_element_at_direct` — 13/17 parse-only + 14/17 direct per P1-E §2.1/§2.2) the scalar reference is the *inner* primitive the envelope hides, surfaced via the `runtime/parse-attribution` cargo feature per dispatch context §1 CH2 F1.

**Stage A passes** when the scalar reference compiles, has no `panic!` paths under valid input, is `#[inline]`-marked (so the SIMD body can be `target_feature`-detect-dispatched without function-call overhead in the scalar fallback path), and the unit-test invocation `<primitive>_scalar(<corpus-derived input>)` is non-trivial (produces a non-zero output on at least one P1-E corpus fixture; the corpus-derived input may live inside the eventual `checkasm_<primitive>.rs` cell rather than a separate unit test).

### §2.B — Stage B: Differential Checkasm Cell Authoring

**Shape.** A `crates/bbnf-simd/tests/checkasm_<primitive>.rs` file with at minimum (a) a deterministic-input cell (handful of fixed `&[u8]` cases covering boundary behaviours: empty, all-zeros, all-ones, single-byte, alphabet-boundary, EOB-tail-residual), (b) a random-sweep cell (Xorshift64-driven, ≥4096 iterations per `checkasm_bitmap_prefix_xor_64.rs:26-37` exemplar), and (c) for any primitive whose signature takes a `&[u8]` of length > 16, an alignment-sweep cell modelled on `checkasm_parity.rs:233-289` (lengths in {1, 16, 32, 64, 128, 1024, 8192} × alignments 0..63). Each cell wraps the SIMD call in `guarded_call(|| <SIMD body>(...))` per `checkasm_common.rs:34-39` (the helper that installs the xor-fold stack canary). For aarch64 primitives that touch GPRs ≥ x19, the cell additionally wraps the call in `callee_saved_register_then(|| ...)` per `checkasm_common.rs:84-112`.

**Scalar reference status.** Required — the cell `assert_eq!`s the SIMD output against the §2.A scalar reference; this is the `call_ref` / `call_new` pair from FFmpeg `checkasm.h`. A cell that does not invoke the scalar reference is not a checkasm cell, it is a smoke test.

**Arch.** Compiles on every host (the SIMD body is conditional-compiled via `cfg(target_arch = ...)`, the scalar reference always compiles; the cell guards the SIMD call with the same `cfg` so non-target hosts skip but the cell still typechecks). Strict mode: `BBNF_SIMD_STRICT=1` must promote logged divergences to test failures per Lock 16 v+1 amendment at `LOCKS.md:320-324`.

**P1 antecedent.** Same set as §2.A. The cell's random-sweep iteration count is calibrated to P1-E primitive-class risk: `string` / `unicode` primitives need ≥4096 random trials because the SK-V12 `escape_mask_64` falsifier (xorshift seed `0xCAFEF00DBAADF00D` per CHECKASM-REPORT.md §d) was found by random-sweep, not by deterministic boundary cases; `scan` primitives need the full alignment-sweep because the structural-classifier divergence the harness logs at default-mode in CHECKASM-REPORT.md §c is alignment-driven.

**Stage B passes** when `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_<primitive> -- --nocapture` runs zero divergences on (a) the deterministic-input cell, (b) the random-sweep cell, (c) the alignment-sweep cell where applicable, *and* (d) the standing corpus-parity cell at `tests/checkasm_parity.rs:344-372` continues to pass strictly (no regression in the joint surface).

### §2.C — Stage C: Lock 16 Allowlist Citation + Same-Plane SOTA Cite

**Shape.** A line-item appended to the Lock 16 allowlist at `restart/locks/LOCKS.md:282-302` containing: the abstract primitive name (grammar-neutral, per the P1-E §1.3 vocabulary — `byte-window multiply-accumulate`, not `JSON-digit-block-decode`), the primary ISA reference (Arm A64 ISA section / Intel SDM section / AMD APM section / published architecture blog with named technique), and the prior-art lineage (the Lemire / Validark / Mula / Sneller / asmjson / sonic-rs / simdjson / yyjson / WikiChip citation that established the technique). Each existing Lock 16 row at `LOCKS.md:282-307` carries this shape exactly; the new line item must match.

**Scalar reference status.** Cite — the line item must point at the §2.A scalar reference path (`crates/bbnf-simd/src/scalar/<primitive>.rs`) explicitly. Per Lock 16 closing clause at `LOCKS.md:306-307`: "Every `core::arch::*` use-site and every `asm!` block in `crates/bbnf-simd/` traces to a citation in the Lock 16 allowlist or in the current skinny SOTA-BEAT synthesis." The reverse holds: the Lock 16 line item must trace to the scalar reference.

**Arch.** All. The line item names the target ISA(s) explicitly; primitives admissible on more than one arch carry one line item per arch (mirroring the existing "arm64 NEON byte classify" / "x86_64 AVX-512 GFNI" split shape).

**P1 antecedent.** Plus the **same-plane SOTA cite** per `LOCKS.md:130-137`: the line item names the comparator (asmjson / sonic-rs / simdjson / yyjson / lightning-css) whose existing same-plane number the primitive's admission is designed to beat, *and* the strictness plane the comparator is anchored against (sonic-rs strict-vs-strict per Lock 8 v+1 amendment at `LOCKS.md:120-137`, not sonic-rs `utf8_lossy` — that was the SK-V6 mistake the lock now blocks).

**Stage C passes** when the Lock 16 line item is committed (G-Omega edit per the v+1 governance boundary at `LOCKS.md:366-375`; not a T-P3 direct edit), the cited SOTA comparator number is current per the skinny `BENCH.md` rolling-SOTA-delta surface, and the strictness plane matches the comparator's strict-vs-strict plane (no permissive comparator per Lock 8).

### §2.D — Stage D: Same-Wave Consumer Wiring + Strict Row Movement

**Shape.** A commit in the *same* wave as Stage B that (a) wires the admitted primitive into a production caller in `crates/runtime/src/grammars/<g>/` or `crates/codegen/src/` (the latter when the primitive is emitted via codegen template per Lock 14 v+1 amendment at `LOCKS.md:222-238`), (b) re-runs the bench gate per `restart/skinny/BENCH.md` §3, (c) records the row movement (Mbps + c/B delta) in `skinny/RESULTS.md` against the strict-comparator anchor, and (d) tags the wave-close commit with the gate disposition per Lock 16 v+1 close vocabulary at `LOCKS.md:335-342`: `wired` / `deleted` / `scalar-delegate-non-ASM` / `architectural-block-with-REDRESS`. No fifth disposition. Specifically `inventory_demoted_with_evidence` is historical-only and does not close a new admission.

**Scalar reference status.** Held — the §2.A scalar reference remains the runtime fallback for hosts that lack the target feature; the dispatch surface at `crates/bbnf-simd/src/dispatch.rs` selects the SIMD body when `cfg(target_feature = "...")` holds, falls back to scalar otherwise. The same-wave consumer wiring cannot delete the scalar reference; it can only add the SIMD dispatch arm.

**Arch.** Production-runtime — the wiring lands in code that runs on the M5 Max benchmark host (aarch64) at minimum. x86_64 admission requires a Zen 4 / Sapphire Rapids cohort row per Lock 8 v+1 amendment ("AVX-512 literature is x86 architecture pressure and cannot close M5/aarch64 rows" per `LOCKS.md:346-349`) — which means a primitive admitted only on x86_64 carries `dispatch_disposition = wired` on the x86_64 dispatch arm but is `architectural-block-with-REDRESS` for the aarch64 plane until a NEON equivalent (per the NEON 3-pack at `LOCKS.md:289-291`: `vbcaxq_u8` / `vceqq_u8 + vorrq_u8` / LD4-interleaved 4-channel classifier) is authored.

**P1 antecedent.** The wave-close row in `skinny/RESULTS.md` must reference the P1-E hot-leaf antecedent it moves (e.g. "moves `unicode_escapes/direct_to_struct/main` row from N-direct/NO-GO to A/GO by replacing the inlined `unescape_string` byte-loop at `parse-that-regex/src/lib.rs:718` with the admitted SIMD primitive"). A primitive admitted without a hot-leaf antecedent movement is a speculative kernel per CH1 of the CHALLENGE.

**Stage D passes** when (a) `cargo test -p runtime` plus `cargo test -p bbnf-simd --release --test checkasm_<primitive>` plus the strict-comparator bench all pass in the same wave commit, (b) the `skinny/RESULTS.md` row delta is strict-positive (Mbps moved up, c/B moved down) against the sonic-rs / yyjson / simdjson / lightning-css anchor named in §2.C, and (c) the wave-close `skinny/REDRESS.md` entry names the primitive, the consumer wiring path:line, the row movement, and the disposition vocabulary token. The exemplar of this shape is the SK-V6 W2 Candidate-3 closure at `skinny/REDRESS.md:1439-1488`.

### §2.E — Stage E: Manifest-Row Cohesion + Substrate-Union Declaration

**Shape.** A row appended to the v+1 primitive manifest per `LOCKS.md:309-318` ("every `core::arch::*`, `target_feature`, and `asm!` use-site in `bbnf-simd`, parse-that facades, generated scanners, or collapsed-stage code maps to a manifest row containing stable primitive id, abstract primitive name, primary ISA/library citation, hardware gate, scalar reference, strict checkasm/parity command, corpus/equality parity, grammar policy source, substrate target, retention lifetime, policy owner, same-wave production consumer, expected row/feature gate, LOC/risk, rollback path, abrogate threshold, and final disposition"). Sixteen named columns.

**Scalar reference status.** Cited verbatim — the `scalar reference` column carries the absolute path to the §2.A file (`crates/bbnf-simd/src/scalar/<primitive>.rs`).

**Arch.** The `hardware gate` column names the `cfg(target_feature = "...")` predicate (`neon`, `sha3`, `avx512f`, `avx512vbmi2`, `gfni`, `vpclmulqdq`, etc. per the Lock 16 ISA inventory at `LOCKS.md:282-302`). Hosts that do not satisfy the predicate fall back to the §2.A scalar reference.

**P1 antecedent.** The `expected row/feature gate` column names the `skinny/RESULTS.md` row the primitive moves (matches the §2.D consumer-wiring antecedent). The `grammar policy source` column names the generated grammar config that supplies the primitive's per-grammar parameters (alphabet, quote byte, escape byte, control-byte cap, etc.) per Lock 14 v+1 amendment at `LOCKS.md:255-263`: "Quote, escape, control, delimiter, number, string, and no-string/no-number policy must come from generated grammar config or caller data, not hardcoded JSON/CSS constants." A primitive whose policy is hardcoded in `bbnf-simd` itself (i.e. carries a `b'"'` / `b'\\'` literal in the SIMD body) is JSON-overfit per Lock 14 and CH2 marks it REVISE (re-express to take policy via parameter) or REJECT (drop).

**Plus the Lock 1 substrate-union declaration** per `LOCKS.md:73-82`: the manifest row's `substrate_target` ∈ {`local_temp_only`, `existing_tape`, `direct_sink`, `admitted_fact_output`}, the `retention_lifetime` ∈ {`local_loop`, `generated_function`, `output_row`}, the `policy_owner` ∈ {`generated_grammar`, `caller_data`, `none`}. Per Lock 1 v+1 amendment at `LOCKS.md:79-82`: "Any retained class/mask stream, parser-owned cursor/list state, public substrate API, `UnionTape`, or second tape is rejected unless G-Omega explicitly amends Lock 1." A primitive that names `substrate_target = parallel_substrate` or any value outside the four enumerated ones is rejected by Stage E by construction.

**Stage E passes** when the sixteen-column manifest row is committed alongside the Stage D wave-close commit, every column is non-empty, the `final disposition` column matches the Stage D wave-close disposition token, the `rollback path` column names a verifiable git revert (commit-sha or branch), and the `abrogate threshold` column names a quantitative gate-back condition (Mbps regression %, c/B regression delta, or row-count of admitted-row regressions) that triggers automatic rollback per Lock 1 v+1 amendment at `LOCKS.md:84-90`: "REDRESS 96/97/98 are binding substrate-ceiling history. … not shortlist-safe without a fresh material differential, scalar/checkasm or equality proof, same-wave consumer, strict row gate, rollback path, and abrogate threshold."

### §2.0 — The five-stage gate, schematically

```
   [P2-C/D/E candidate primitive]
              │
              ▼
   Stage A (scalar reference)     ──fails──> rejected (no oracle)
              │ passes
              ▼
   Stage B (checkasm cell)        ──fails──> rejected (differential divergence; CH4 of CHALLENGE)
              │ passes (strict mode)
              ▼
   Stage C (Lock 16 cite + SOTA)  ──fails──> rejected (no architectural antecedent; CH1)
              │ passes
              ▼
   Stage D (same-wave consumer)   ──fails──> demoted (kernel without consumer; CH6 paper-close)
              │ passes (strict row movement)
              ▼
   Stage E (manifest + substrate) ──fails──> demoted (Lock 1 substrate violation; CH5)
              │ passes
              ▼
   [admitted: dispatch_disposition = wired]
```

S-P3 shortlists only candidates that pass all five stages. The shortlist is monotonic per CHALLENGE V{N}: a candidate that passes V1 may be re-tested at V{N+1} but is not re-admitted unless its Stage A–E artifacts re-pass.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

The five stages are themselves grammar-neutral by construction — they are admission *process*, not primitive *content* — but the §3 frontmatter requires a per-candidate verdict, and the grammar-neutrality of the *primitives the stages gate* is the load-bearing axis. The five verdicts:

- **Stage A (scalar reference).** Generic. The shape `<primitive>_scalar(input: &[u8], <per-grammar policy parameters>) -> output` is grammar-neutral by Lock 14 enforcement. Per `LOCKS.md:255-263` v+1 amendment, the policy parameters (alphabet, quote, escape, control cap, etc.) must come from caller; the scalar function must not carry hardcoded JSON / CSS / Sheets / BBNF-self literals. **Verdict: grammar-neutral by construction.**
- **Stage B (checkasm cell).** Generic, but the test fixtures the cell uses must include at least one non-JSON consumer or a measured deletion/rejection per Lock 14 v+1 amendment at `LOCKS.md:255-263` last clause ("A primitive claimed grammar-neutral must exercise at least one non-JSON consumer or record a measured deletion/rejection"). At HEAD the bbnf-simd checkasm cells exercise JSON-pool-biased inputs (per `tests/checkasm_parity.rs:94-100` `fill_jsonish`) — that satisfies the "exercise" clause for JSON only. New primitives need a fixture pool drawn from the target non-JSON grammar(s) (CSS L4 declaration-value bytes, Sheets formula bytes, BBNF-self grammar bytes) to meet the clause. **Verdict: grammar-neutral with mandatory non-JSON fixture extension at admission time.**
- **Stage C (Lock 16 cite).** Generic. Lock 16 abstract-primitive names are grammar-neutral by Lock 14 enforcement at `LOCKS.md:285-288` ("**abstract primitive: cross-chunk byte-context propagation** — applies to ANY grammar with chunk-spanning tokens, not just JSON"; "**abstract primitive: byte-window multiply-accumulate, lifted from dav1d's FIR filter** — applies to ANY grammar's digit-block decode, not just JSON"). The same-plane SOTA comparator cite is grammar-specific (sonic-rs is JSON-only, lightning-css is CSS-only) but that's a per-row anchor, not the primitive name. **Verdict: grammar-neutral by construction.**
- **Stage D (same-wave consumer).** Per-grammar. The consumer wires into a specific grammar's runtime (`crates/runtime/src/grammars/<g>/`) and moves a specific row in `skinny/RESULTS.md`. Different grammars per wave; the *gate* is grammar-neutral; the *evidence* is per-grammar. Per Lock 14 v+1 amendment at `LOCKS.md:240-253`: "With only one of Sheets or BBNF-self, the claim is scoped to the witnessed grammars and may not use fleet-wide grammar-neutral wording." A primitive admitted with only a JSON consumer carries `grammar_scope = json-only` on its manifest row; fleet-wide claims require at least one non-JSON same-wave consumer or a measured deletion/rejection from a non-JSON grammar in the same wave. **Verdict: grammar-neutral with per-grammar scope-tag on the admission.**
- **Stage E (manifest + substrate).** Generic, with a grammar-neutrality column. The manifest's `grammar policy source` column names "generated grammar config" or "caller data" or "none"; never names a specific grammar. A primitive whose manifest row carries `grammar policy source = bbnf-simd hardcoded JSON literal` is Lock-14 violating and Stage E rejects it. **Verdict: grammar-neutral by construction; the manifest is the audit surface that catches violations.**

The composite verdict for the admission process: **grammar-neutral by Lock 14 enforcement, with a per-grammar evidence-scope tag attached to each admitted primitive at Stage D and audited at Stage E**. CH2 of the S-P2 CHALLENGE marks a candidate REVISE if its proposed admission omits the Stage D non-JSON consumer (with the candidate then required to demonstrate one before re-submission) or REJECT if its scalar reference (Stage A) contains a hardcoded JSON literal that cannot be parameterised away.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

The dispatch context §3 CH3 binding enumerates the REDRESS pre-block surface. The admission process this artefact specifies is designed so no candidate that follows the five stages can implicitly reopen any of these routes, but the per-route risks are nonetheless enumerated below per CH3:

- **REDRESS 28+33 (Class A NEON tiny-string wiring).** `match_tiny_plain_string` was admitted under strict checkasm (`tests/checkasm_parity.rs:507-616`) but its parser-route wiring regressed `twitter` per `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:512`. The Stage D gate fails closed on the wave-close strict-row-movement clause: any tiny-string-class primitive re-admitted in S-P3 must move `twitter/parse_only/main` non-regressively, not merely pass strict checkasm. **Risk: Stage D's strict-row-movement clause is the gate; a candidate that proposes "admit the kernel; wire later" is rejected at Stage D by construction.**
- **REDRESS 50-55 (SK-V5 UTF-8 fusion).** Dispatch-table / function-pointer alternates were rejected under the Lock 14 + Lock 16 substrate-union surface per `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:263`. The Stage E substrate-target column blocks any candidate that names `parallel_substrate` or a function-pointer table; the admitted values are the four enumerated at `LOCKS.md:73-82`. **Risk: Stage E's substrate-target column blocks the re-opening by construction.**
- **REDRESS 60-72 (SK-V6 retained-parse + sidecar producers + digest cap-16).** Parser-local cursors, event sidecars, decoded-string stats sinks per `skinny/REDRESS.md:1314-2090` were rejected. Stage E's `retention_lifetime` column blocks any value outside `local_loop` / `generated_function` / `output_row`; a parser-owned cursor would require `retention_lifetime = parser_owned`, which is not in the allowlist and Stage E rejects it. **Risk: Stage E's retention-lifetime column blocks the re-opening by construction.**
- **REDRESS 80 (canada mantissa-widen).** The W7 zero-fallback mantissa-widen route was admitted under strict checkasm and same-wave consumer per `skinny/REDRESS.md:2215-2249`. Any S-P3 candidate that proposes a competing number-decode primitive must Stage D-move the `canada/parse_only/main` row non-regressively against the W7 mantissa-widen baseline; otherwise demoted. **Risk: Stage D's strict-row-movement clause covers it.**
- **REDRESS 82-84 (single-quartet unicode classifier, StringBlock16 tiny probe, object-pair compaction).** Per `skinny/REDRESS.md:2285-2396`. Same as REDRESS 60-72: Stage E's retention-lifetime + substrate-target columns block any re-opening that uses a retained classifier output or a parser-owned compaction buffer.
- **REDRESS 88 (PMULL prefix-XOR as hot body) and REDRESS 89 (CSSC CTZ next-bit bulk consumer).** Per dispatch context §4 "P2-C primary architecture". Both are aarch64-only routes the dispatch context flags as REDRESS-blocked. Stage C's same-plane SOTA cite + Stage D's strict-row-movement together gate them: any candidate that proposes PMULL prefix-XOR as the hot body must beat the existing prefix-XOR path on `skinny/RESULTS.md` rows in the same wave, and any candidate that proposes CSSC CTZ as the next-bit bulk consumer must move the bulk-emit row non-regressively. **Risk: the gates are present; the empirical evidence S-P2 P2-C surfaces is what drives the admission attempt, not this artefact.**
- **REDRESS 96/97/98 (production-union substrate routes).** Per `LOCKS.md:84-90` v+1 amendment: "Full class-column vectors, streaming structural cursors, class-lane-only replays, parser-owned sidecars, and `UnionTape`-style retained structures are not shortlist-safe without a fresh material differential, scalar/checkasm or equality proof, same-wave consumer, strict row gate, rollback path, and abrogate threshold." Stages A–E together enforce all six conditions (Stage A scalar/oracle; Stage B checkasm; Stage D consumer + row gate; Stage E rollback + abrogate). A candidate that proposes one of the REDRESS 96/97/98 substrate shapes must therefore explicitly carry the manifest row's `abrogate threshold` (i.e. the row-regression-count threshold at which an automated revert fires) — and that abrogate threshold is what makes the candidate shortlist-safe per the Lock 1 v+1 binding.
- **CH3 catchall: dispatch-table / function-pointer alternates, parser-local cursors, event sidecars, decoded-string stats sinks, generic source visitors, source-method digest folds, PEXT mask plan (REDRESS 126; aarch64 has no PEXT), production-union routes (REDRESS 96/97/98).** All blocked at Stage E substrate-target column by construction. The five-stage admission process is the structural answer to the CH3 enumeration: no candidate that passes all five stages can implicitly reopen a CH3 route, because the Lock 1 v+1 columns at Stage E catch every named pattern.

The CH3 disposition for the admission process itself: **no REDRESS entry is re-opened by the process; the process's purpose is to refuse re-opening at Stage E by enforcing the Lock 1 v+1 substrate-target / retention-lifetime / policy-owner enumeration**.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

### §5.1 — External sources

- **FFmpeg `tests/checkasm/checkasm.{c,h}`** — upstream FFmpeg testsuite, the canonical implementation of the SIMD differential harness pattern. The bbnf-simd port at `crates/bbnf-simd/tests/checkasm_parity.rs:1-21` docstring explicitly cites it; the table at `crates/bbnf-simd/CHECKASM-REPORT.md:43-51` maps each FFmpeg `checkasm.h` feature to its bbnf-simd Rust equivalent.
- **dav1d `tests/checkasm/`** — the dav1d testsuite extends FFmpeg `checkasm` with per-primitive cells for every `src/<arch>/*.{asm,S}` body. The pattern bbnf-simd's per-primitive `checkasm_*.rs` files (eleven cells at HEAD) mirror. Per `LOCKS.md:303-307`: "ffmpeg `x86inc.asm` macro corpus vendored verbatim at `crates/bbnf-simd/ext/x86/x86inc.asm`".
- **dav1d `src/x86/msac.asm:80-220`** — per `LOCKS.md:305`: "The msac entropy decoder's `cnt/buf/end` cross-chunk refill pattern (`/tmp/dav1d-research/dav1d/src/x86/msac.asm:80-220`) is the one genuinely transferable algorithmic insight beyond what simdjson/sonic-rs/yyjson already demonstrate." This is the structural antecedent the cross-chunk byte-context primitives (per `LOCKS.md:285`) trace to.
- **VLC `modules/codec/*` SIMD discipline** — same as FFmpeg (VLC inherits the FFmpeg admission discipline directly via `libavcodec` linkage); not a separate citation surface for this artefact.
- **Validark 2024, "Interleaved vectors on ARM"** — `validark.dev/posts/interleaved-vectors-on-arm/`, cited at `LOCKS.md:288` as the antecedent for the NEON LD4-interleaved 4-channel classifier; the simdjson PR #2333 referenced in the same row is the consumer-wiring exemplar.
- **Travis Downs, "kreg-facts" series** — `travisdowns.github.io/blog/2019/12/05/kreg-facts.html` + `2020/05/26/kreg2.html`, cited at `LOCKS.md:293` as the antecedent for the AVX-512 k-mask arithmetic family.
- **Lemire 2026, "The fastest way to match characters on ARM processors"** — cited at `LOCKS.md:290` for NEON set-membership / SVE2 `svmatch_u8` portability.
- **Arm Architecture Reference Manual ARMv8.2-A SHA3** — cited at `LOCKS.md:289` for `vbcaxq_u8` / `veor3q_u8` ternary bitwise.
- **WikiChip VPCLMULQDQ + BranchFree.org "Quote pairs with PCLMULQDQ" (2019)** — cited at `LOCKS.md:294`.
- **WikiChip AVX-512_IFMA + Lemire 2024 "Sapphire Rapids vs Zen 4 JSON"** — cited at `LOCKS.md:295`.
- **Lemire 2023, "Parsing integers quickly with AVX-512"** — cited at `LOCKS.md:296`.
- **WikiChip AVX-512_BITALG** — cited at `LOCKS.md:297`.
- **Wojciech Mula 2018-2024 + Intel GFNI Technology Guide 2018** — cited at `LOCKS.md:298`.
- **`ahash` crate** — cited at `LOCKS.md:301`; the hash-primitive lineage for path / key dictionaries.
- **sonic-rs / asmjson / simdjson / yyjson / lightning-css** — the SOTA comparator set per Lock 8 at `LOCKS.md:119`. Strict-vs-strict plane per v+1 amendment at `LOCKS.md:120-137`. P2-A's teardown of these comparators is the per-comparator citation surface this artefact does not duplicate.

### §5.2 — Prior bbnf tranche evidence (binding context for the admission process)

- `restart/locks/LOCKS.md:282-364` — Lock 16 full text + v+1 amendments (primitive manifest, strict checkasm command, close-state vocabulary, REDRESS-blocked-route notes).
- `restart/locks/LOCKS.md:73-90` — Lock 1 v+1 amendment (substrate-target / retention-lifetime / policy-owner enumeration; REDRESS 96/97/98 abrogate-threshold clause).
- `restart/locks/LOCKS.md:220-263` — Lock 14 v+1 amendments (generic-crate grammar-neutrality enforcement; primitive policy from generated grammar config or caller data).
- `restart/locks/LOCKS.md:366-375` — v+1 governance boundary (no implementation wave may use v+1 wording as permission to add a lock, retain a sidecar, or bypass the owning skinny SPEC gate).
- `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md:348-470` — the SK-V3 / SK-V6 prior synthesis of the Layer-0/Layer-1 vocabulary + the checkasm admission gate as the precondition to all subsequent phases.
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:1-120` — the prototype-report write-up of the bbnf-simd checkasm port, including the FFmpeg-feature-to-Rust mapping table, the default-mode + injection-mode + strict-mode runs, and the SK-V12 W2 escape-mask disposition.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-737` — the harness itself (4 tests at HEAD: alignment sweep, uniform-random sweep, corpus parity, robust-mean bench).
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs:1-192` — the shared canary + callee-saved-register sentinel helpers.
- `skinny/crates/bbnf-simd/tests/checkasm_{ascii_set_member_find_64,bitmap_next_set_bit,bitmap_prefix_xor_64,bulk_emit_positions_64,byte_class_from_eq_set_64,byte_class_from_table_64,eob_pad_clamp,escape_mask_64,structural_terminator_64,utf8_block}.rs` — the eleven per-primitive checkasm cells comprising the empirical floor.
- `skinny/crates/bbnf-simd/src/scalar/*.rs` — the seven cross-arch scalar references; the executable specifications the SIMD bodies differential against.
- `skinny/REDRESS.md:1241-1278` — SK-V5 W5 primitive admission redress (the first wave that codified the checkasm-gate + scalar-reference + same-wave-consumer triplet as the admission shape).
- `skinny/REDRESS.md:1380-1488` — SK-V6 W2 Candidate-2 + Candidate-3 closure exemplars (the canonical "scalar reference, AArch64 checkasm parity, and a same-wave consumer inside" shape).
- `skinny/REDRESS.md:3603-3632` — SK-V12 W2 `escape_mask_64` admitted-correctness-prerequisite closure (the v+1 amendment at `LOCKS.md:326-333` traces here).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:80-97` — P1-E §1.3 CH2 primitive classification table (the grammar-neutral primitive vocabulary the admission process gates against).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:230-263` — P1-E §4 anomalies + masking signals; specifically §4.1 CH2 Lock-14 mis-attribution census + §4.7 REDRESS guard reconciliation.
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md:1-63` — S-P2 dispatch context binding this agent (the artefact this output answers to).
- `restart/prompts/skinny/PASS-2-RESEARCH.md:1-257` — the S-P2 contract (§2 scope matrix row P2-B; §3 CH1–CH6 lens overlay; §7 hard caps; §8 bbnf-lang specifics — particularly §8.2 "the scalar-oracle-first discipline … P2-B produces the admission process; S-P3's falsifiability gates enforce it").
