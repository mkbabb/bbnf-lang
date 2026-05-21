# SK-V13 P2-B: FFmpeg / VideoLAN Checkasm Process

Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-21.
Scope: FFmpeg / VideoLAN checkasm process mapped onto bbnf-simd scalar-reference, checkasm, and same-wave-consumer discipline; dav1d lineage is bounded to cited VideoLAN context and is not source-file gate authority in V2.
Output: this file.
P1 hot-leaf antecedents: `parse_that_regex::unescape_string`, `read_hex_unit_scalar`, `match_tiny_plain_string_with_cap::<16>`, `scan_structurals`, `scan_tail`, `bulk_emit_positions_64_neon`, generated JSON direct envelopes, and the CSS declaration-values timer/fact-sink profile.
Lock surface: Lock 16 primary; Lock 14 and Lock 1 guard any grammar-neutral consumer or union/substrate-adjacent candidate.

## §1 — Findings

The transferable FFmpeg / VideoLAN checkasm process is not "write ASM, then
hope the row moves." It is a three-part admission loop: scalar oracle first,
differential checkasm over adversarial inputs, then production wiring into a
measured consumer. The VideoLAN checkasm page demonstrates the public shape:
aligned reference and optimized buffers, randomized inputs, `checkasm_call_ref`,
`checkasm_call_new`, output comparison, and `checkasm_bench_new` only after
correctness comparison. The same page records dav1d lineage at project level:
VideoLAN's standalone checkasm fork came from dav1d's internal checkasm copy
and from FFmpeg / x264 ancestry. V2 deliberately treats that as context only.
No dav1d-specific implementation or gate text may be copied into S-P3 unless a
future pass adds exact dav1d source-file anchors. FFmpeg's own `checkasm.c` is
the assembly testing and benchmarking tool, and FFmpeg `checkasm.h` exposes the
same call pattern through `declare_func`, `fail`, `checkasm_check`,
padded-buffer checking, and typed comparison helpers.

bbnf already has the local skeleton, but S-P3 must make it a wave gate rather than tribal memory. `bbnf-simd`'s `checkasm_parity.rs` says it is modelled on FFmpeg checkasm, runs scalar and candidate implementations on bit-identical inputs, compares outputs and source mutation, sweeps misalignment, protects stack state, traps bad candidate faults, and times kernels after parity (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1`-`20`). Its deterministic xorshift input generator is local and dependency-free (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:57`-`100`). Its strict mode is currently opt-in by `BBNF_SIMD_STRICT=1`, while corpus parity always asserts (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16`-`20`, `:112`-`115`). `checkasm_common.rs` adds the stack-canary fold and AArch64 callee-saved-register sentinel checks (`skinny/crates/bbnf-simd/tests/checkasm_common.rs:49`-`71`, `:83`-`111`). Those are the right primitives for a dav1d-style process.

The local gap is not test vocabulary; it is admission discipline. S-P1 explicitly labels all profile facts as `profile_signal_not_gate_admission` (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:8`-`23`). Direct JSON hot leaves are mostly generated envelopes rather than primitive leaves, with `unicode_escapes` as the clean direct primitive attribution to `parse_that_regex::unescape_string` (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:72`-`87`, `:107`-`120`). Mode III proves structural SIMD scan beats scalar scan on every JSON corpus, but P1-C correctly fences that as scanner micro-evidence, not a union reopening (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:71`-`87`, `:108`-`117`). CSS declaration-values has equality and throughput evidence, but its profile is timer/fact-sink dominated rather than a parser primitive (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:82`-`87`).

Lock 16 is binding: every SIMD primitive needs unit parity against scalar reference and corpus parity against the expanded skinny corpus in `crates/bbnf-simd/tests/` (`restart/locks/LOCKS.md:87`, `:112`). SK-V13 HANDOFF strengthens that for this tranche: no SIMD/ASM wiring into CSS, union, JSON `parse_only`, or shared generated code without grammar policy proof, scalar parity, checkasm/differential coverage, same-wave row measurement, no public substrate API, and no sidecar classifier state (`restart/skinny/tranches/sk-v13/HANDOFF.md:159`-`163`). The user addendum further removes support-only landings: every behavior wave must move a row or record architectural-block evidence (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:100`-`109`).

## §2 — Candidate primitives

### P2-B admission process S-P3 should gate

Every candidate below must pass the same eight-stage process before S-P3 can shortlist it as an implementation wave:

| Stage | Required evidence | Fail disposition |
|---|---|---|
| 1. P1 antecedent | Named S-P1 hot leaf and row family; no speculative kernel. | Drop candidate before S-P3. |
| 2. Primitive contract | Exact input bytes, state carries, output bits/positions/facts, and permitted memory access window. | REVISE; no checkasm can be written yet. |
| 3. Scalar reference | Safe scalar function in `bbnf-simd::scalar` or the consuming crate; executable specification, not prose. | REJECT for SIMD admission. |
| 4. Differential checkasm | Random, JSON-ish/CSS-ish, edge, alignment, tail, and corpus-derived vectors; source mutation, stack, callee-saved, and fault checks; `BBNF_SIMD_STRICT=1` in admission. | REJECT or route as proof-only. |
| 5. Microbench | Same-host primitive microbench with scalar and candidate timings, same feature mask, and noise handling. | REVISE if unmeasured; REJECT if slower without row-level compensating proof. |
| 6. Same-wave consumer | Production caller in the row family that P1 named; no orphan, no support-only landing. | REJECT under Lock 16 and addendum. |
| 7. Grammar policy | For generic `bbnf-simd` or shared generated code, the consuming grammar supplies quote, escape, control, delimiter, numeric, and no-string policy. | REVISE under Lock 14. |
| 8. Row gate | Strict comparator row movement in `RESULTS.md` / gate-json / REDRESS, with prior REDRESS material differential cited. | No ADMIT; record measured reject. |

### Candidate B1 — `unescape_uxxxx_x4` / decoded escape run

Shape: decode four `\uXXXX` quartets from 16 source bytes into four codepoints, with scalar surrogate policy retained above the primitive. The current scalar reference and NEON bodies already exist in `unescape_uxxxx.rs`: `unescape_uxxxx_scalar` is the parity anchor (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33`-`47`), `unescape_uxxxx_neon` is the single-quartet candidate (`:58`-`121`), and `unescape_uxxxx_x4_neon` is the four-quartet body (`:123`-`166`).

P1 antecedent: `unicode_escapes/direct_to_struct` rank-1 `parse_that_regex::unescape_string` at 46.7% Track 1 self-time and the parse-only `y_string_unicode` `read_hex_unit_scalar` leaf (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:87`, `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:104`).

Scalar-ref status: present for quartet decode; must add or cite a scalar `x4` wrapper that preserves the same invalid-nibble and surrogate-boundary semantics before any new body is admitted.

Checkasm parity expectation: expand `checkasm_parity` or a dedicated `checkasm_unescape_uxxxx_x4` to cover all valid/invalid nibble classes, high/low surrogate adjacency, tail lengths not divisible by four, misaligned starts, and corpus slices from `unicode_escapes`, `unicode_mixed`, `y_string_unicode`, and CSS escaped identifiers. Admission must run with `BBNF_SIMD_STRICT=1`.

Same-wave consumer: the only legal consumer is the existing `parse-that-regex::unescape_string` caller or a grammar-neutral decoded-string materializer that measures JSON unicode rows and a CSS escaped identifier / string row in the same wave. A standalone bbnf-simd primitive is an orphan.

REDRESS boundary: SK-V10 proof-only escape work failed to move direct rows and REDRESS 119 treats prior direct residuals as history. Reopen only with the material differential "x4 run consumed by the production materializer with same-row strict comparator," not "new wrapper around the existing primitive."

### Candidate B2 — ASCII set run-skip production split

Shape: find the first byte in or out of a small ASCII delimiter set over a 64-byte window; SK-V12 W4's `a64_ascii_set_run_skip` microbench reported 4.72x speedup but no production consumer. The scoping audit records scalar 18.51 ns/iter, candidate 3.92 ns/iter, parity pass, and production wiring deferred (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:36`-`62`).

P1 antecedent: CSS declaration-values has equality and throughput evidence but unresolved parser hot leaf; this candidate is justified by the SK-V12 W4 microbench plus CSS scanner/skip need, not by S-P1 CSS self-time. Treat that as a weaker antecedent that requires a narrow consumer profile before S-P3 wave scope.

Scalar-ref status: present in the W4 microbench as byte-walk reference per REDRESS-126 (`skinny/REDRESS.md:3774`-`3786`).

Checkasm parity expectation: retain `checkasm_ascii_set_member_find_64.rs`; add CSS delimiter/comment/whitespace corpora and verify all cursor positions, empty set, singleton set, `{}`, `;`, comment opener, and EOF-adjacent tails.

Same-wave consumer: CSS L4 generated scanner only. The consumer must be a named delimiter/whitespace/comment scan-block path, not a bench-only harness. It must maintain the admitted `css_l4/declaration_values/direct_to_struct/main` equality and speed bar (`skinny/RESULTS.md:94`) and, if used for new CSS rows, the full lightningcss parity criteria.

REDRESS boundary: REDRESS-126 explicitly says W4 was a measured route-production split and not production admission; it also keeps final orphan accounting separate from `a64_ascii_set_run_skip` (`skinny/REDRESS.md:3802`-`3812`, `:3864`-`3872`). S-P3 may shortlist production wiring, but not count SK-V12 W4 as row movement.

### Candidate B3 — structural scan consumer / union-adjacent scan

Shape: consume `scan_structurals` / `scan_tail` output as a transient mask/position primitive, not as a retained sidecar substrate. P1-C shows structural SIMD beats scalar scan across all 17 JSON rows, with large ratios on `mesh`, `canada`, and `numbers` (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:71`-`87`, `:115`-`117`).

P1 antecedent: mode-III structural-scan probes, especially `scan_structurals` and `bulk_emit_positions_64_neon` function-only rows (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:78`-`91`).

Scalar-ref status: existing JSON scanner scalar path can be the reference, but S-P3 must require the primitive contract to state whether the output is positions, classes, or a transient mask. "Class column" is not the scalar reference.

Checkasm parity expectation: dedicated runtime checkasm or bbnf-simd checkasm must compare scalar and SIMD positions, quote/escape carries, tail behavior, and mutation safety across corpus slices. P1-C sidecar function-only attribution is not enough.

Same-wave consumer: JSON direct/parse row movement or CSS structural row movement in the same commit. The consumer may move the existing tape/substrate projection but must not retain a new sidecar vector, parser-owned cursor, or parallel `UnionTape`.

REDRESS boundary: REDRESS 96/97/98 falsified prior union-substrate implementations. REDRESS 96 landed a full class-column substrate plus move-consumed `scan_structurals` and regressed; REDRESS 97 removed the allocation vector with a streaming cursor and still missed; REDRESS 98 retires the old gate and requires future union work to cite material differential (`skinny/REDRESS.md:2910`-`2949`). B3 is admissible only if the material differential is transient same-wave consumption or a truly new union shape, not a fourth class-column attempt.

### Candidate B4 — bitmap next-bit / bulk emit / prefix-XOR family

Shape: bitmask prefix-XOR, next-set-bit, and bulk position emission for quote/string regions and structural positions. Local scalar references exist for prefix-XOR, next-bit, and bulk emit (`skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`, `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs`, `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs`), with dedicated checkasm files present for each primitive.

P1 antecedent: `bulk_emit_positions_64_neon` appears in mode-III `distinct_values` structural SIMD attribution, but only as function-only sidecar evidence (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:86`, `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:88`-`91`).

Scalar-ref status: present for support primitives; production candidate must state exact carry/state semantics before PMULL/CSSC/EOR3 bodies are admissible.

Checkasm parity expectation: existing `checkasm_bitmap_prefix_xor_64`, `checkasm_bitmap_next_set_bit`, and `checkasm_bulk_emit_positions_64` must run in strict mode, with all-zero, all-one, alternating, single-bit, dense, sparse, carry-in, cursor-at-end, and corpus-derived masks.

Same-wave consumer: JSON scan or a new union/SIMD consumer must call the new body in production and show row movement. Keeping a scalar delegate under an aarch64 wrapper is not admission.

REDRESS boundary: REDRESS 88 rejected PMULL as the default hot prefix-XOR body after JSON row regressions, and REDRESS 89/90 kept CSSC/CTZ/bulk routes bounded by consumer proof (`skinny/REDRESS.md:2512`-`2540`, `:2547`-`2615`). D4 unblocks the category, not the exact failed implementation.

### Candidate B5 — byte-class / delimiter classifier

Shape: classify a 64-byte block against a grammar-supplied set of up to eight bytes and return a 64-bit mask. The scalar reference is explicit: input is 64 contiguous bytes plus set `S`, output bit `i` set iff `src[i] in S` (`skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1`-`37`).

P1 antecedent: JSON generated direct envelopes and CSS delimiter/scanner gaps need byte-class dispatch, but S-P1 did not isolate a non-JSON parser primitive. This candidate is therefore a process candidate: S-P3 may use it only if P2-F/P3 identifies a grammar-neutral consumer and P1/P2-C shows a row family.

Scalar-ref status: present.

Checkasm parity expectation: existing `checkasm_byte_class_from_eq_set_64` must be strict, grammar-set parameterized, and extended with CSS selector/declaration delimiters and JSON structural bytes. It must also prove the set-length cap and failure behavior for larger sets.

Same-wave consumer: CSS selector/declaration scanner, JSON structural scan, or Sheets delimiter scanner with strict row movement. A generic classifier without a row consumer fails addendum no-support-only.

REDRESS boundary: Lock 14 forbids JSON structural-byte policy in a generic crate. The consuming grammar must supply the byte set and policy; no hardcoded `{ } [ ] , : "` in shared admission.

## §3 — Grammar-neutrality

The grammar-neutral unit is not "JSON string scan" or "CSS delimiter scan." It is a byte-set, byte-window, carry, mask, or decoded-span primitive whose policy comes from grammar metadata. B5 is naturally grammar-neutral if the alphabet is caller-supplied. B2 can be grammar-neutral if it takes a caller-supplied ASCII set and has CSS as its first consumer. B1 is conditionally grammar-neutral: hex quartet decode applies to JSON `\uXXXX`, CSS escaped identifiers/strings, and future Sheets escaped cells, but the escape introducer, surrogate policy, and output materializer are grammar-owned. B3 and B4 are grammar-neutral only while they produce transient masks/positions; they violate Lock 1 if retained as a sidecar substrate.

S-P3 should require a `G-SIMD-GRAMMAR-POLICY`-style gate for every primitive that enters `bbnf-simd` or shared generated code. The gate must name:

- grammar domain and row;
- quote, escape, control, delimiter, and number policy, or an explicit no-string/no-number policy;
- scalar reference path;
- checkasm test path;
- production consumer path;
- row gate and comparator plane.

JSON-only wrappers such as `parse_object_value_at_direct` and `parse_array_element_at_direct` remain profile envelopes, not grammar-neutral primitives (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:32`-`50`). Typed JSON leaves are also JSON-only and cannot be generalized to CSS/Sheets (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:63`-`76`). CSS timer/fact-sink samples are nonparser overhead until a narrower CSS parser leaf is profiled (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:100`-`104`).

## §4 — Risks

- REDRESS 96/97/98: structural SIMD scan evidence must not silently revive the class-column substrate, streaming cursor, parser-owned structural projection, sidecar event vector, second source scan, or parallel `UnionTape`. New union work needs material differential and same-wave row movement (`skinny/REDRESS.md:2910`-`2949`; `restart/skinny/tranches/sk-v13/HANDOFF.md:152`-`163`).
- REDRESS 119/120: direct row residual history is lifted under the full-SOTA pin, but every reopen must cite the prior fixpoint and name a fresh material differential; profile signals are not admissions (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:60`-`75`; `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:127`-`130`).
- REDRESS 126: the five aarch64 "orphans" are demoted with evidence, not magically wired. New work on `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_context`, or `cache_hints` must either wire a consumer same-wave or delete/record architectural-block evidence (`restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:48`-`60`; `skinny/REDRESS.md:3864`-`3872`).
- REDRESS 28/33 and REDRESS 82-84: tiny-string and string-block primitives have parity-green history but failed row gates when wired broadly. Any string primitive must be scoped to the P1-named row family and cannot claim global string wins.
- REDRESS 88/89/90: PMULL and CSSC routes are category-unblocked by the user pin, but the prior default hot-body implementations remain measured failures. A new route must prove a different consumer, body, or row plane.
- Checkasm strictness risk: local `checkasm_parity` defaults to recording divergences unless `BBNF_SIMD_STRICT=1` is set. S-P3 must require strict mode for admission commands.
- V2 citation boundary: this artifact verifies FFmpeg and VideoLAN checkasm
  primary sources. It does not verify a specific dav1d source-file URL, so
  dav1d-specific process claims are downgraded to lineage context. S-P3 may
  cite the FFmpeg/VideoLAN checkasm process, but not dav1d file-level mechanics,
  unless a later research cycle adds exact `code.videolan.org/videolan/dav1d`
  source anchors.

## §5 — Sources

- `restart/prompts/skinny/PASS-2-RESEARCH.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`.
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`.
- `restart/skinny/tranches/sk-v13/HANDOFF.md`.
- `restart/locks/LOCKS.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md`.
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`.
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md`.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`.
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`.
- FFmpeg `tests/checkasm/checkasm.c` source: https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html
- FFmpeg `tests/checkasm/checkasm.h` source: https://ffmpeg.org/doxygen/trunk/checkasm_8h_source.html
- VideoLAN checkasm project page and dav1d lineage note, used as project-level
  lineage context only: https://www-test.videolan.org/projects/checkasm/
- VideoLAN dav1d project page: https://images.videolan.org/projects/dav1d.html
  (project context only; not source-file authority for S-P3 gate text)
