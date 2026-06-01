---
agent: 2F
pass: T-P2-research
cycle: V3
generated_at: 2026-06-01T19:10:00-04:00
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 24
techniques_grounded: 13
techniques_refuted: 6
sk_cycle: SK-V18
host_close_route: Apple-M5-Max-aarch64-PRIMARY (x86 SECONDARY/DELETED in skinny)
prior_cycle_dispositions_folded:
  accepted:
    - "PTG-2F-03 (SIMD byte scan): UPGRADED partial->grounded-and-WIRED — bbnf-simd carries 12 single-kernel checkasm_<primitive>.rs harnesses + 1 aggregate checkasm_parity.rs (+ the checkasm_common.rs helper module), 13 differential harnesses; byte_class_from_eq_set_64 is consumed by runtime_simd::count_top_level_commas on the live CSS path."
    - "PTG-2F-05 (float, no-fallback refutation): RE-CONFIRMED at materialize_f64 skinny/crates/parse-that-regex/src/number/mod.rs:271 `text.parse::<f64>()` fallback + skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:168 `None`-on-ambiguous (the `~0.01%` is a DOC-comment estimate at :8/:141, not a measured rate)."
  rejected:
    - "PTG-2F-V2 framing 'the live parse-that family is narrower than the SK-V14 dossier implied; no base parse-that crate; CSS outside the primitive family' — REFUTED this cycle: the UPSTREAM parse-that crate (/Users/mkbabb/Programming/parse-that/rust/parse_that) carries a FULL scan/ substrate (scan_balanced, structural_bitmap nibble-LUT classifier, quote_parity prefix-xor, number_simd) that V2 never audited; the prior 'gap' was a SCOPE error (V2 audited only the skinny-vendored parse-that-regex, not the upstream crate)."
  revised:
    - "PTG-2F-01 (regex/HIR): bbnf-regex DOES carry a real RegexHir/RegexKind + ByteSet256 + FirstSet facts surface (lib.rs:1-64), not merely heuristic classification — the HIR is shallow (4 RegexKind variants) but it is a structured fact API, not a stub."
    - "CH1-V3-SK18-02: SRC-PARSE-THAT-SCAN relocates find_first_of_nibble_lut/build_nibble_luts to their real home parse_that/src/scanners.rs:262/:235 (the scan/ files import them; structural_bitmap.rs:94 is a comment); the '14 checkasm-gated kernels' phrasing corrected to 12 single-kernel + 1 aggregate harness (+ checkasm_common helper) throughout."
    - "CH6-V3-01: SRC-HYPERSCAN attributes the abstract-primitive NAME SHUFTI/TRUFFLE to the Hyperscan codebase (src/nfa/shufti.c/truffle.c, VERIFIED present) + Langdale's branchfree writing, NOT the NSDI 2019 paper text (which does not name them); the NSDI paper stays as the project/lineage citation."
  first_cycle_additions: [PTG-2F-09, PTG-2F-10, PTG-2F-11, PTG-2F-12, PTG-2F-13]
locks_amendment_candidates: 3
t_p1_entry_state: CONVERGED-V5-near (1E LOCKS-AMENDMENTS V5 table live)
extends: prior V2 dossier (Source Registry + PTG-2F-01..08 carried verbatim where unchanged)
---

## Executive Summary

The prior V2 dossier declared the balanced-delimiter scan a parse-that GAP. **That
was a scope error, refuted here.** V2 audited only `skinny/crates/parse-that-regex`;
it never audited the *upstream* `parse-that` crate at
`/Users/mkbabb/Programming/parse-that/rust/parse_that`, which carries a complete
`scan/` substrate: `scan_balanced` (`scan/balanced.rs:26`, the exact
`css_balanced_component_scan` SHELL — nibble-LUT SIMD skip + nesting depth + quote
skip + terminator), `structural_bitmap.rs` (the Langdale-Lemire nibble-LUT NEON
classifier + `PaddedView` model — the G6 retarget kernel), `quote_parity.rs` (simdjson
§3.1.4 prefix-XOR via `vmull_p64`), and `number_simd.rs` (NEON 16-digit accumulate).

Independently, the *skinny* `bbnf-simd` crate already carries **13 differential
checkasm harnesses** (12 single-kernel `checkasm_<primitive>.rs` + 1 aggregate
`checkasm_parity.rs`, plus the `checkasm_common.rs` shared-helper module — NOT 14
kernels) covering the eq-set member scan (`byte_class_from_eq_set_64`,
`find_ascii_set_member64`), `bracket_depth_mask_64` (the R-F Candidate B
balanced-depth bitmap), `comment_body_mask_64`, and `bitmap_prefix_xor_64` (quote
parity) — and `byte_class_from_eq_set_64` is **LIVE-CONSUMED** by
`runtime_simd::count_top_level_commas` on the CSS path.

So the SK-V18 G6 WIRE is **not a build-from-scratch** — it is a RETARGET of the dead
`find_css_significant` (R7: flat skip, structurally wrong for the recursive shell)
onto the EXISTING eq-set kernel, with `scan_balanced` as the upstream reference
oracle and `bracket_depth_mask_64` as the documented bitmap upgrade path. The genuine
gaps are NARROW: (1) the recursive-shell vectorization (inert-run skip stopping at
`([{'"/`), (2) the >8-byte two-fan OR-reduce, and (3) the upstream-vs-vendor
provenance decision (the two substrates are SEPARATE — they must be reconciled, not
both kept). Float no-fallback remains REFUTED (`materialize_f64` falls back at
`number/mod.rs:271`).

## Source Registry

Carries the V2 registry verbatim (SRC-COX, SRC-RUST-REGEX-HIR, SRC-RUST-REGEX,
SRC-MEMCHR, SRC-SIMDJSON, SRC-SIMDUTF, SRC-HOEHRMANN, SRC-CLINGER, SRC-EISEL-LEMIRE,
SRC-FNF, SRC-RFC8259, SRC-CSS-SYNTAX, SRC-CSS-TYPED-OM, SRC-CSSPARSER,
SRC-LIGHTNINGCSS, SRC-PARSE-THAT-REGEX, SRC-BBNF-REGEX, SRC-BBNF-SIMD, SRC-CODEGEN,
SRC-SK-V15-FLOOR). The SK-V18 additions:

| ID | Primary source | grounded? | Use |
|---|---|---|---|
| SRC-LANGDALE-LEMIRE | Geoff Langdale and Daniel Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 28(6), 2019, [arXiv:1902.08318](https://arxiv.org/abs/1902.08318). VERIFIED — the nibble-LUT `vpshufb` two-stage structural classification (stage 1 = 64-byte block -> 64-bit structural bitset). | GROUNDED | The published basis for `parse-that/scan/structural_bitmap.rs:classify_16` (`f(c)=lo[c&0x0F] & hi[c>>4]`) AND skinny `bbnf-simd byte_class_from_eq_set_64`. The G6 NEON retarget rides this technique. |
| SRC-LANGDALE-VECCLASS | Geoff Langdale, "vectorized classification" technique; Daniel Lemire, ["Fast character classification with z3"](https://lemire.me/blog/2025/06/01/easy-vectorized-classification-with-z3/), 2025-06-01 (the rendered title is "Fast character classification with z3"; "easy-vectorized-classification-with-z3" is the URL slug, not the title); ["Paper: Parsing Gigabytes of JSON per Second"](https://branchfree.org/2019/02/25/paper-parsing-gigabytes-of-json-per-second/). VERIFIED — title text matches the source, and `f(c) = lut_lo[c & 0x0F] AND lut_hi[c >> 4]` nibble-pair classification. | GROUNDED | Names the exact 4-op (lo-lookup, hi-lookup, AND, non-zero) classifier in `classify_16` and `parse-that` `build_nibble_luts`. The named abstract primitive for the eq-set member scan. |
| SRC-HYPERSCAN | Wang, Hong, Chang, Park, Langdale, Hu, Zhu, "Hyperscan: A Fast Multi-pattern Regex Matcher for Modern CPUs", NSDI 2019 (the project/lineage citation: graph decomposition + SIMD-accelerated string/FA matching — author list VERIFIED EXACT; the paper does NOT name SHUFTI/TRUFFLE in its text). The abstract-primitive NAMES `SHUFTI`/`TRUFFLE` come from the Hyperscan CODEBASE algorithm names — the source files [`src/nfa/shufti.c`/`shufti.h`](https://github.com/intel/hyperscan/tree/master/src/nfa) and `truffle.c`/`truffle.h` (shuffle-based byte-class acceleration, `PSHUFB`/`vqtbl`) — and Geoff Langdale's branchfree.org shuffle-based-matching writing (e.g. the [SMH post](https://branchfree.org/2018/05/30/smh-the-swiss-army-chainsaw-of-shuffle-based-matching-sequences/)), NOT the NSDI 2019 paper text (which does not name them). | GROUNDED | The project lineage of shuffle-based byte classification (NSDI 2019); the abstract-primitive NAME `SHUFTI` for `byte_class_from_eq_set_64`'s small-set classifier is sourced to the Hyperscan codebase / Langdale's branchfree writing (the name is NOT in the NSDI paper text). |
| SRC-SIMDJSON-QUOTE | simdjson §3.1.4 "unescaped quote parity" (carry-less multiply prefix-XOR), cited in SRC-SIMDJSON; `_mm_clmulepi64_si128` / aarch64 `vmull_p64`. | GROUNDED | The basis for `parse-that/scan/quote_parity.rs:prefix_xor` AND skinny `bbnf-simd bitmap_prefix_xor_64`. The string-region masking primitive. |
| SRC-PARSE-THAT-SCAN | `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/scan/{mod.rs,balanced.rs:26,structural_bitmap.rs:36-154,quote_parity.rs:41-168,number_simd.rs:31-135,digits.rs,ident.rs,number_f64.rs}`. The two named nibble-LUT kernels `find_first_of_nibble_lut` and `build_nibble_luts` live in the PARENT module `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/scanners.rs:262` (`find_first_of_nibble_lut`) / `:235` (`build_nibble_luts`), NOT in the `scan/` directory — `balanced.rs:27` IMPORTS them; the `scan/structural_bitmap.rs:94` reference to `find_first_of_nibble_lut` is a COMMENT ("Replaces … `find_first_of_nibble_lut`"), not the definition. | LIVE | The upstream `parse-that` scan substrate V2 never audited — `scan_balanced` IS the `css_balanced_component_scan` shell reference; it rides the `scanners.rs` nibble-LUT classifier (`find_first_of_nibble_lut`) consumed via `balanced.rs:27`. |
| SRC-BBNF-SIMD-EQSET | `skinny/crates/bbnf-simd/src/lib.rs:209-226` (`find_ascii_set_member64`), `:283-293` (`byte_class_from_eq_set_64`); `aarch64/{byte_class_from_eq_set_64.rs,bracket_depth_mask_64.rs,comment_body_mask_64.rs,bitmap_prefix_xor_64.rs}`; `tests/checkasm_*.rs` (12 single-kernel `checkasm_<primitive>.rs` harnesses + 1 aggregate `checkasm_parity.rs` + the `checkasm_common.rs` shared-helper module — 13 differential harnesses, NOT 14 kernels). | LIVE | The skinny home of the eq-set/bracket/quote/comment kernels the G6 WIRE retargets; checkasm-gated already. |
| SRC-RUNTIME-SIMD | `skinny/crates/runtime/src/runtime_simd.rs:29` (`count_top_level_commas` LIVE-consumes `byte_class_from_eq_set_64`+`bracket_depth_mask_64`), `:112` (`find_comment_close` dead), `:169-214` (`find_css_significant` dead/test-only, R7 wrong-shape). | LIVE | Proof the eq-set kernel already moves a live CSS sub-leaf; proof the two `_significant`/`_comment_close` kernels are dead/misshapen. |
| SRC-CSS-HOT-LEAF | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:657-680` (`find_component_delim`), `:693-713` (`consume_balanced_at`). | LIVE | The 94.1% scalar hot leaf — the recursive shell the G6 NEON retargets, replicated byte-identically across 7 css_l4 generated.rs. |

## Technique Grounding and V3 Admission Manifest

Carries PTG-2F-01..08 from V2 (re-grounded where revised in frontmatter). The SK-V18
additions:

| spec-claim / T-P1-divergence id | published source | grounded/refuted/partial | bbnf-specific note |
|---|---|---|---|
| **PTG-2F-09** `css_balanced_component_scan` SHELL substrate exists in parse-that | SRC-PARSE-THAT-SCAN `scan/balanced.rs:26`; SRC-LANGDALE-LEMIRE; SRC-MEMCHR. | **GROUNDED** (refutes V2 "gap"). | `scan_balanced(bytes, &BalancedScanConfig{open,close,quotes,escape,terminators})` is the EXACT shell: nibble-LUT SIMD skip of inert bytes (`find_first_of_nibble_lut`), `depth` nesting on `open`/`close`, `memchr2`-accelerated quote skip, depth-0 terminator return. This IS the upstream reference oracle for the G2 `css_balanced_component_scan` primitive — NOT a from-scratch build. **GAP**: it lives in the SEPARATE upstream crate, not in skinny's `bbnf-simd`; the G2 primitive must be re-homed (vendor) or the upstream crate wired (path-dep) — a provenance decision, not an absence. The `<=8 unique structural bytes` cap (`balanced.rs:44`) matches the eq-set 8-byte contract; CSS's `{}:;` delims + `'"/([{ ` structural set = 9 bytes EXCEEDS the cap -> needs the two-fan OR-reduce (already salvageable from `find_css_significant:180-204`). **GENERALITY GATE (gates `grounded`):** the vendored `scan_balanced` shell's `grounded` status is conditional on the SAME byte-set-as-caller-data falsifier 2C applies to `css_balanced_component_scan` (the (a)-(b) test): (a) the vendored classifier (`find_first_of_nibble_lut`/`build_nibble_luts`) must take its alphabet as CALLER DATA (the `BalancedScanConfig{open,close,quotes,escape,terminators}` byte set), not a hard-coded CSS/structural table; (b) the emitted skip must VARY under a mutated byte set. A vendored classifier that HARDCODES the structural alphabet is grammar-SPECIFIC and INADMISSIBLE regardless of upstream provenance — provenance alone does not confer neutrality. Until (a)-(b) are confirmed against the vendored shell (in addition to the LAC-2F-V3-01 mask-unification close test), the substrate-presence is GROUNDED but the neutral admission of the vendored shell is gated. |
| **PTG-2F-10** eq-set member scan / SHUFTI classifier exists + LIVE-wired | SRC-BBNF-SIMD-EQSET `lib.rs:283`; SRC-RUNTIME-SIMD `:29`; SRC-LANGDALE-VECCLASS; SRC-HYPERSCAN. | **GROUNDED + WIRED** (UPGRADES V2 PTG-2F-03 partial). | `byte_class_from_eq_set_64(src:&[u8;64], set:&[u8<=8]) -> u64` is the named SHUFTI/vectorized-classification primitive (abstract name: SHUFTI eq-set membership — the `SHUFTI` name from the Hyperscan codebase `src/nfa/shufti.c` + Langdale's branchfree writing, NOT the NSDI paper text; the published technique lineage is the simdjson/Langdale-Lemire vectorized classification of SRC-LANGDALE-VECCLASS). It is checkasm-gated (`tests/checkasm_byte_class_from_eq_set_64.rs`), has a scalar reference (`scalar/byte_class_from_eq_set_64.rs`), an aarch64 NEON impl, AND a same-wave consumer ALREADY LIVE: `runtime_simd::count_top_level_commas` (`:29`) rides it on the CSS path. Its ONLY live production consumer is CSS — the JSON direct product path is scan-free (`json/scan.rs` ZERO samples, S-P1), so there is no live JSON consumer of this kernel; the structural neutrality is genuine (the byte set is caller data, the kernel names no grammar) but the empirical dual-consumer claim is NOT — record CSS-only per Lock 14's bbnf-simd clause. Lock 16's scalar-reference/checkasm/citation/abstract-name cells are SATISFIED today; the same-wave-consumer cell is satisfied by the CSS consumer ONLY (not a JSON one). The G6 WIRE retargets it onto the `find_component_delim` recursive shell. NOTE — the cited consumer file `runtime_simd.rs:6`-`7` carries an INACCURATE source comment ("the same kernel JSON's `scan_structurals` rides"): that is empirically FALSE. On the aarch64 close host JSON `scan_structurals` (`json/scan.rs:22`) returns `neon::scan` (`:25`) → `bbnf_simd::aarch64::classify_tbl4` (`:214`,`:219`,`:228`) — the TBL `byte_class_from_table_64` family, NOT the eq-set kernel; `scan_structurals_scalar` (`:29`) is the `#[allow(unreachable_code)]` non-aarch64 fallback. The JSON product path is additionally scan-free (`json/scan.rs` ZERO samples, S-P1). Either way the eq-set kernel is never on the JSON path, and JSON `scan_dispatch` rides `byte_class_from_table_64`. It is a same-wave G6 source-fix obligation; the structural neutrality (caller-supplied byte set) stands, only the source comment's JSON-rides claim is wrong. |
| **PTG-2F-11** balanced-depth bitmap (R-F Candidate B upgrade) exists | SRC-BBNF-SIMD-EQSET `aarch64/bracket_depth_mask_64.rs`; SRC-RUNTIME-SIMD `:47`; SRC-SIMDJSON. | **GROUNDED** (the documented S-P2 R-F Cand-B path). | `bracket_depth_mask_64(src, opens, closes, depth_in) -> (mask, depth_out)` is the standard running-balance bracket-matching scan as a 64-byte SIMD bitmap, with an i32 depth carry threaded across blocks — the EXACT "balanced-consume bitmap" R-F Candidate B names as "the documented upgrade path (its mask vocabulary IS the JSON scan vocabulary, the true JSON/CSS convergence point — defer until measured)". It EXISTS, checkasm-gated, and is LIVE-consumed by `count_top_level_commas` — but the live consumer rides it as a TRANSIENT per-block carry (the i32 depth threads within one call, never a retained depth side-array). **LEDGER FENCE (inline):** promotion of this bitmap to REPLACE the scalar recursion is gated by the REDRESS 96/97/98 streamed-cursor retirement (`G-W3-UNION-SUBSTRATE`, `skinny/REDRESS.md:2795`-`2940`,`:2928`-`2933` — a SIMD depth-bitmap threaded/materialised through the retained parse loop twice regressed every must-improve M5-Max row); the RETAINED form is the refuted shape. "EXISTS + live-consumed" is therefore NOT a promotion warrant — a downstream consumer must clear the retired prior, not promote off this row alone. The S-P2 "defer until measured" is honest — the scalar recursive shell + eq-set skip (R-F Cand-A) is the first WIRE; this bitmap is the second-order convergence point, fenced against REDRESS 96/97/98. |
| **PTG-2F-12** quote-parity / string-region masking exists | SRC-BBNF-SIMD-EQSET `aarch64/bitmap_prefix_xor_64.rs` + `lib.rs:170-206` (`prefix_xor_64`,`escape_mask_64`); SRC-PARSE-THAT-SCAN `scan/quote_parity.rs:41`; SRC-SIMDJSON-QUOTE. | **GROUNDED** (both crates). | The simdjson §3.1.4 prefix-XOR-via-carry-less-multiply is realized BOTH in skinny `bbnf-simd` (`bitmap_prefix_xor_64`, checkasm-gated, + scalar `escape_mask_64` backslash-parity at `lib.rs:175-206`) AND upstream `parse-that` (`prefix_xor` via `vmull_p64`/`_mm_clmulepi64`, `compute_in_string_bitmap`). CSS strings inside `find_component_delim` are currently scalar-skipped (`consume_string_at`); this primitive is the available SIMD path for the string-skip sub-leaf IF profiling promotes it. Not yet on the hot leaf (the scalar string skip is sub-dominant to the delim scan) — so it is source-present, admission-deferred-to-profile per S-P1 §5 (no orphan kernel). |
| **PTG-2F-13** `find_css_significant` is the WRONG-SHAPE dead kernel (R7) | SRC-RUNTIME-SIMD `:169-214`; SRC-CSS-HOT-LEAF `:657-713`; S-P1 SYNTHESIS-PROFILE `:105-112`. | **GROUNDED (as a refutation of "wire-as-is")**. | `find_css_significant(bytes, from, delimiters, fixed:&[u8;9]) -> usize` is a FLAT stop-at-significant-byte skip (two-fan OR-reduce over `set_a`/`set_b` to handle the <=13-byte set), with ZERO live callers (only `#[cfg(test)]`). The hot `find_component_delim`+`consume_balanced_at` machine RECURSES through nested `()[]{}` and skips strings/comments — a different function. So the dead kernel CANNOT be wired as-is (it would lose nesting/string/comment correctness); it must be RETARGETED: salvage its two-fan OR-reduce set-split (`:180-204`) into the recursive shell's inert-run skip, stopping AT `([{'"/` so the scalar shell still handles recursion. This is the precise R-F Candidate A move, and the S-P1 R7 caveat is exactly correct. **LEDGER (REDRESS):** the retarget is a ledger-grounded route, NOT a fresh unknown. REDRESS 144 (`skinny/REDRESS.md:4418`-`4438`, `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT) is the PRODUCTION PRECEDENT: the same `find_ascii_set_member64` kernel class WAS wired into production CSS `Scanner::scan_block` delimiter search and MOVED the CSS row (Track 1 444.2 Mbps vs 434.1, Criterion +109.87%, strict cssparser/lightningcss green). The deferred-to-H1 net-win must clear the CAUTIONARY priors: REDRESS 96/97/98 (`:2795`-`2940`, `G-W3-UNION-SUBSTRATE` retired — a SIMD structural cursor STREAMED/materialized through the retained JSON parse loop uniformly regressed every M5-Max row, `:2928`-`2933` finding) and REDRESS 126 (`:3766`-`3805`, the same `find_ascii_set_member64(…, b"{};")` microbench passed 4.7× but was closed as `ROUTE-PRODUCTION-SPLIT` WITHOUT production wiring — a microbench/checkasm PASS is explicitly NOT a production-row move). So G6's inert-run net-win re-opens a question the ledger answered NEGATIVELY for the JSON streamed-cursor case and POSITIVELY for the CSS delimiter case — it is not a new unknown. |

### Per-gap admission cost manifest (CH4 Lock-16 v+1 columns)

Row-local `loc_estimate` / `risk_class` / `rollback path` / `abrogate threshold` for each
SK-V18 gap (wave band: SPEC §8 G2 `≤450 LOC` `:439`, G5/G6 `≤450 LOC` `:443`):

| gap (PTG row) | wave_owner | loc_estimate | risk_class | rollback path | abrogate threshold |
|---|---|---|---|---|---|
| regex/HIR fact surface (PTG-2F-01, revised) | G1 (generator selection) | ≈ 0 (HIR exists, analysis-only) | LOW | N/A (facts already live) | if the shallow 4-variant HIR proves inadequate for generator selection, DELETE the selection dependency rather than grow a runtime DFA |
| `css_balanced_component_scan` SHELL re-home (PTG-2F-09) | G2 | ≤200 (vendor `scan_balanced` shell + dedupe) | MED | leave the upstream crate un-vendored; G2 keeps scalar shell | if the vendored shell forks a second mask convention, DELETE the vendor and wire the EXISTING `byte_class_from_eq_set_64`/`bracket_depth_mask_64` only |
| eq-set SHUFTI scan retarget (PTG-2F-10) | G5/G6 | ≤150 (retarget onto recursive shell + checkasm ext) | MED-HIGH | revert to scalar `find_component_delim` (no shipped output depends on the SIMD skip) | if the SIMD skip cannot net-beat the M5-Max scalar loop (REDRESS 96/98 risk), ABROGATE the wire and keep scalar (G6 outcome stays `C`) |
| balanced-depth bitmap (PTG-2F-11, R-F Cand-B) | G5/G6 (deferred) | ≤200 (bitmap consume path) | HIGH | not wired pre-measurement | DEFER permanently if the bitmap regresses vs scalar recursion on M5 Max (the REDRESS-98 streamed-cursor finding) |
| quote-parity string-skip (PTG-2F-12) | G5/G6 (profile-gated) | ≤100 | MED | scalar `consume_string_at` retained | DO NOT author unless the string-skip sub-leaf profiles hot post-G6 (no orphan kernel) |
| float no-fallback (refuted) | n/a | 0 | n/a | n/a (REFUTED — no admission) | n/a |
| provenance reconcile (upstream-vs-vendor) | G2 (decision) | ≈ 0 (decision, not code) | LOW | path-dep fallback if vendor proves heavier | if both substrates persist, ABROGATE the path-dep and single-substrate per LAC-2F-V3-01 |

## Architectural Assertions Defended

1. **The balanced-delimiter scan is a SOLVED substrate, not a parse-that gap.** Both the
   upstream `parse-that` crate (`scan_balanced` — full shell with nibble-LUT skip) AND
   skinny `bbnf-simd` (eq-set member scan + bracket-depth bitmap, checkasm-gated,
   live-consumed) carry the substrate. The V2 "narrower than implied / CSS outside the
   primitive family" framing was a scope artifact of auditing only `parse-that-regex`.
   Defends the SK-V18 SPEC G2 `css_balanced_component_scan` band (`<=450 LOC`, `:439`) as a
   RETARGET/RE-HOME, not a build-from-scratch — consistent with the §6 honest-finding
   admission (named, `.bbnf`-invoked, grammar-derived-args, scalar/checkasm reference).

2. **The G6 NEON WIRE satisfies Lock 16 because the underlying kernel ALREADY does.**
   `byte_class_from_eq_set_64` carries: a published citation (Langdale-Lemire nibble-LUT /
   Hyperscan SHUFTI), an abstract-primitive name (SHUFTI eq-set membership), a scalar
   reference (`scalar/byte_class_from_eq_set_64.rs`), a checkasm-parity test
   (`tests/checkasm_byte_class_from_eq_set_64.rs`), and a same-wave LIVE consumer
   (`count_top_level_commas`). The G6 admissibility burden is therefore NOT "admit a new
   hand-tuned intrinsic loop" (inadmissible) — it is "retarget an already-admissible kernel
   onto a new call site," which is the cleanest possible Lock-16 path. **The ledger grounds
   this retarget: REDRESS 144 (`G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT, `skinny/REDRESS.md:4418`-`4438`)
   wired the same kernel class into production CSS delimiter search and moved the Track 1 row
   (+109.87%) — so "the kernel already exists / retarget the admitted route" is ledger-precedented,
   not asserted.** The deferred-to-H1 net-win, however, is NOT free: it must clear REDRESS 96/97/98
   (`G-W3-UNION-SUBSTRATE` retired — the M5-Max scalar-cheaper-than-SIMD-cursor finding for the
   JSON streamed case) and REDRESS 126 (a microbench PASS is NOT a production-row move). The
   correctness/presence admission (G6 outcome `C` until H1) is admissible TODAY; the speedup
   defers to H1 and must beat the cautionary priors.

3. **The eq-set member scan is STRUCTURALLY grammar-NEUTRAL; the balanced SHELL is not.**
   The inner kernel takes caller-supplied byte sets (`set:&[u8]`), names no grammar — that
   STRUCTURAL fact (caller data, no grammar tag) is the genuine neutrality proof, not a
   dual-consumer census. Its ONLY live production consumer is the CSS
   `count_top_level_commas` path; the JSON `scan_dispatch` path rides a DIFFERENT primitive
   (`byte_class_from_table_64`, the 256-LUT TBL classifier), NOT the eq-set kernel, and the
   JSON direct product path is otherwise scan-free (S-P1). So the inner kernel is neutral by
   construction but CSS-only by live consumer — disclosed per Lock 14's bbnf-simd clause.
   The S-P2 §6 / 1E LAC-1E-V5-03 neutrality demotion
   (`balanced_component_scan` -> `css_balanced_component_scan`) applies ONLY to the
   recursive SHELL (CSS-exercised in this campaign), NOT to the eq-set sub-kernel. This is
   the correct seam: structurally-neutral kernel (CSS-only live consumer), honestly-scoped
   shell name.

4. **The float stack is correct-with-fallback, NOT no-fallback (re-confirmed).**
   `parse-that-regex::number::materialize_f64`
   (`skinny/crates/parse-that-regex/src/number/mod.rs:261-271`, terminal
   `text.parse::<f64>()` fallback) falls back on the ambiguous-rounding band:
   `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:168` returns
   `None` ("Ambiguous rounding — fallback needed"), and the `~0.01%` is a
   DOC-comment estimate (`eisel_lemire/mod.rs:8`,`:141`), NOT a measured rate.
   Defends "correct f64 with fallback"; SRC-FNF makes "no-fallback" a SEPARATE
   claim that the live code cannot make. (The bare `eisel_lemire/mod.rs` path is
   ambiguous across two crates — it is root-resolved here to the skinny
   `parse-that-regex` crate, which is where the refutation's `None`-on-ambiguous +
   `text.parse::<f64>()` fallback both live.)

5. **`bbnf-regex` carries a real structured HIR fact surface, not a stub.** `RegexFacts{
   nullable, first:FirstSet, byte_classes:Vec<ByteClass>, hir:RegexHir, string }` with a
   `ByteSet256` bit-set and a `RegexKind{Whitespace,QuotedString,Numeric,Unknown}` HIR
   (`lib.rs:1-64`). The HIR is SHALLOW (4 variants, classification-derived) — adequate for
   first-set/nullability/byte-class generator selection (Cox / regex-syntax HIR analysis
   model), inadequate as a runtime DFA matcher. Defends V2 PTG-2F-01 "facts upstream, no
   runtime regex import."

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| (V2) "The live parse-that family is narrower than the SK-V14 dossier implied; CSS L4 is outside the primitive family." | The UPSTREAM `parse-that` crate carries `scan/balanced.rs` (`scan_balanced`), `scan/structural_bitmap.rs` (nibble-LUT classifier), `scan/quote_parity.rs`, `scan/number_simd.rs`, AND a `parsers/css/` family — a full scan substrate V2 never opened. | The balanced-delimiter scan is NOT a gap. The real question is provenance (upstream-vs-vendor reconcile), not absence. Re-scope the "gap" frame. |
| "The G6 NEON kernel must be authored from scratch for the css_balanced scan." | `byte_class_from_eq_set_64` + `bracket_depth_mask_64` + `bitmap_prefix_xor_64` already exist, checkasm-gated, in `bbnf-simd`; `find_css_significant` already implements the two-fan OR-reduce. | G6 is a RETARGET (salvage set-split + recursive-shell skip), not authorship. <=450 LOC band is generous. |
| "`find_css_significant` can be wired as the CSS hot-leaf accelerator." | It is a FLAT skip with no nesting/string/comment handling; the hot `find_component_delim` RECURSES. Wiring as-is loses correctness (R7). | The kernel must be retargeted onto the recursive shell's inert-run skip; only the eq-set SIMD skip vectorizes, the scalar shell retains recursion/strings (the exact R-F Cand-A constraint). |
| "The float path is no-fallback fast." | `materialize_f64` terminal branch is `text.parse::<f64>()` (`skinny/crates/parse-that-regex/src/number/mod.rs:271`); `compute_float` returns `None` on the ambiguous band (`skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:168`, the doc-stated `~0.01%` at `:8`/`:141`, not a measured rate). | Per SRC-FNF, "no-fallback" is inadmissible against this code; "correct f64 with fallback" is the defensible claim. Carried from V2. |
| "CSS value parsing can reuse the upstream `parse-that/parsers/css/value.rs` parser." | That CSS value parser is a combinator-descent (no flat-scan pub fns; the `parsers/css` family is the lightningcss-shaped recursive descent the >SOTA scan was built to AVOID). | Reusing it REGRESSES >SOTA (the genuine §4 tension). The typed CSS provider must be grammar-DERIVED from the lowered scan IR, reusing only the byte KERNELS, not the upstream CSS parser. Carried/sharpened from V2 PTG-2F-06. |
| "A runtime DFA/regex matcher is needed for the generator." | `bbnf-regex` returns analysis FACTS (first-set/nullability/byte-class/shallow HIR); no live emitted path consumes a DFA matcher (`rg RegexProgram|find_at|SpanParser::Regex` confirms analysis-only). | No runtime regex import. The shallow HIR suffices for generator selection. Carried from V2 PTG-2F-02. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Provenance reconcile: is the G2/G6 substrate sourced from the UPSTREAM `parse-that` crate (`scan_balanced` + `scan/structural_bitmap`) as a path-dep, or VENDORED into skinny `bbnf-simd`? The two carry overlapping-but-distinct kernels (upstream `find_first_of_nibble_lut`/`build_nibble_luts` vs skinny `byte_class_from_eq_set_64`). | Before G2, run `rg -n "parse-that\b|parse_that::|scan_balanced|find_first_of_nibble_lut" skinny/Cargo.toml skinny/crates` (the upstream crate is NOT a skinny workspace member — confirmed `Cargo.toml:3-15`). Decide: vendor the `scan_balanced` shell into `bbnf-simd` (single substrate, aligns with [no-orthogonal-codepaths]) OR add the upstream crate as a path-dep. The vendor route is preferred — two SIMD scan substrates is itself an orthogonal-codepath risk. |
| Does the 9-byte CSS significant set (`{}:;` delims + `'"/([{ ` structural) force the two-fan OR-reduce permanently, or can the structural set be narrowed to <=8? | Enumerate the exact structural+delimiter byte set the `find_component_delim` recursive shell must stop at; if `{}` appear in BOTH the delimiter set and the opener set (idempotent membership, per `find_css_significant:182`), the unique count may fit 8. If not, the two-fan OR-reduce (`find_css_significant:199`) is the binding cost — measure its overhead vs a single 13-byte `byte_class_from_table_64` (256-LUT) classifier. |
| Does `scan_balanced`'s `<=8 unique structural bytes` debug_assert (`balanced.rs:44`) hold for the full CSS structural set, or does the upstream shell ALSO need the two-fan? | Read `balanced.rs:44` — the upstream shell caps at 8 via `build_nibble_luts`, so it would NOT directly accept the 9-byte CSS set either; the skinny two-fan is the necessary adaptation. Confirms vendor route must add the fan, not just copy. |
| Can `bracket_depth_mask_64` (R-F Cand-B) replace the SCALAR recursive shell entirely, eliminating `consume_balanced_at` recursion (14.6% leaf)? **LEDGER-FENCED:** a materialised SIMD depth-bitmap threaded across blocks through the retained parse loop is the EXACT streaming-cursor shape REDRESS 96/97/98 retired (`G-W3-UNION-SUBSTRATE`, `skinny/REDRESS.md:2795`-`2940`,`:2928`-`2933`) — it twice regressed every must-improve M5-Max row because consuming a streamed cursor adds memory traffic + indirection the cache-hot scalar loop does not pay. This promotion is NOT an open-ended measurement; it must clear that retirement finding, not merely "match parity AND beat it" in isolation. | Post-G6-Cand-A, measure whether the bitmap-depth scan (transient i32 carry, no call stack — note: the carry must stay TRANSIENT per-call, never a retained side array, or it becomes the retired streamed-cursor) over the real 71KB-495KB corpora matches `consume_balanced_at` parity AND net-beats it ON THE M5 MAX SPECIFICALLY (the REDRESS-98 host). This is the "defer until measured" S-P2 R-F Cand-B promotion gate — a MEASUREMENT against a ledger-retired prior, not a fresh correctness unknown. |
| Is the upstream `parse-that` `scan/quote_parity.rs` `compute_in_string_bitmap` a better CSS string-skip path than the scalar `consume_string_at` inside `find_component_delim`? | Only if the string-skip sub-leaf is profiled-hot AFTER G6 Cand-A lands (S-P1 §5 no-orphan-kernel rule). Sample `rich_summary` post-WIRE; if `consume_string_at` surfaces, wire `bitmap_prefix_xor_64` (already gated); else leave scalar. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | amendment | reason | verify action |
|---|---|---|---|
| LAC-2F-V3-01 | Bind a SINGLE-SIMD-SUBSTRATE lock with MASK-REPRESENTATION UNIFICATION: the SK-V18 close uses ONE scan substrate (skinny `bbnf-simd`), NOT both `bbnf-simd` AND the upstream `parse-that` `scan/` crate. The `scan_balanced` shell MUST be vendored into `bbnf-simd` (re-homed) AND consume the EXISTING `byte_class_from_eq_set_64`/`bracket_depth_mask_64` kernels + the project's one canonical SHRN movemask (`movemask.rs:5`) — it must NOT plant a second mask convention (the upstream uses an 8-byte nibble-LUT `structural` array + `PaddedView`; skinny uses the two-fan eq-set + `bracket_depth_mask_64`). A path-dep absence alone is NOT sufficient: a vendor that copies the upstream nibble-LUT classifier verbatim would pass a mere path-dep-absence test while planting a second parallel mask substrate INSIDE `bbnf-simd`. (Co-binds [no-orthogonal-codepaths] / LAC-2F-V1-01 owner split.) | Two SIMD scan substrates (upstream `find_first_of_nibble_lut`/`build_nibble_luts` + skinny `byte_class_from_eq_set_64`) is an orthogonal-codepath in gestation; the V2 dossier's blindness to the upstream crate is itself evidence the second substrate is easy to forget and re-fork; a second MASK convention inside one crate is the same hidden coupling at the representation level. | Before G2: (1) `rg -n "parse_that::\|parse-that =" skinny/Cargo.toml skinny/crates` == 0 (upstream not a dep); (2) MASK-UNIFICATION close test — `rg -n "build_nibble_luts\|find_first_of_nibble_lut" skinny/crates/bbnf-simd/src` == 0 (no upstream nibble-LUT classifier symbol lands in `bbnf-simd`); (3) gate that the G2 `css_balanced_component_scan` re-uses `byte_class_from_eq_set_64`/`bracket_depth_mask_64` + `movemask.rs:5` (the EXISTING kernels + canonical movemask), not a fresh nibble-LUT port. **STRUCTURAL MASK-CONVENTION CO-GATE (the name-grep above is NECESSARY-NOT-SUFFICIENT — a vendor that RENAMES the upstream classifier, e.g. `build_luts`/`first_of_lut`, passes both the symbol-name grep AND the `movemask.rs:5`-reuse gate by alias):** bind a structural co-gate symbol `bbnf_simd_single_mask_convention` — the `bbnf-simd` analog of `runtime_target_rows_collapsed` — asserting that EVERY 64-byte→64-bit pack in the crate routes through the ONE canonical `movemask::movemask_u8x16`, counting DISTINCT pack implementations (alias-immune: it counts `vshrn_n_u16`/`vaddv_u8` horizontal-pack call-sites that do not delegate to the canonical pack, NOT symbol names), so a renamed/aliased second nibble-LUT classifier is caught structurally. **Wave-owner / enforcement wave: G2 entry** (the guard becomes an enforced xtask/CI check at G2 entry, the way 2D names P3/G3 for the relocated-seam `runtime_target_rows_collapsed` co-gate); co-binds 2E LAC-2E-V6-03 (the singular `vshrn_n_u16::<4>` movemask-convention rule). |
| LAC-2F-V3-02 | RETARGET-not-AUTHOR is the admissible G6 move and must be lock-asserted: the G6 NEON acceleration RETARGETS an already-checkasm-gated kernel (`byte_class_from_eq_set_64`) onto the live recursive shell, salvaging the `find_css_significant` two-fan OR-reduce set-split — it does NOT author a new intrinsic loop. An author-from-scratch NEON loop on the CSS path is inadmissible (no scalar oracle precedence, re-litigates Lock 16). | The §6 honest-finding escape + Lock 16 both forbid an undocumented hand-tuned intrinsic loop; the existing kernels make the honest path a retarget. Binding this prevents a G6 author from re-deriving a bespoke NEON kernel when a gated one exists. | G6 exit gate: `rg -n "byte_class_from_eq_set_64|bracket_depth_mask_64" skinny/crates/runtime/src/runtime_simd.rs` non-empty in the live (non-test) retarget; checkasm differential reuses the EXISTING `checkasm_byte_class_from_eq_set_64.rs`, no NEW per-grammar kernel file under `generated.rs`. |
| LAC-2F-V3-03 | Re-scope the V2 "parse-that gap" frame in the LOCK surface: the balanced-delimiter scan is NOT a parse-that ABSENCE — it is present in BOTH the upstream `parse-that` `scan/` crate and skinny `bbnf-simd`. Any future audit citing a "balanced-scan gap" must FIRST audit `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/scan/` (the upstream substrate), not only `skinny/crates/parse-that-regex`. | The V2 dossier's "gap" was a scope error (audited the vendored crate, missed the upstream one); leaving the frame uncorrected risks a future wave re-authoring `scan_balanced` as if it were absent. | Audit-scope assertion: any 2F-class re-audit lists BOTH `parse-that/rust/parse_that/src/parsers/scan/` AND `skinny/crates/bbnf-simd/src/` before declaring a scan-primitive gap; `ls` both trees in the evidence header. |
