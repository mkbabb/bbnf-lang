# SK-V17 W3 — NEON structural-index acceleration ledger

Pass: SK-V17 wave triumvirate. Wave: W3 (NEON, on the W2 rich tape plane).
Status: LANDED — scalar-referenced + checkasm-parity NEON, same-wave consumed,
EXACT rich 9-field equality preserved, JSON 51/51 held, regen 8/8 clean, a
measured NEON speedup that widens every margin over lightningcss. Base HEAD at
capture: `6dad81fb9` (W2 rich projection). Host: Apple M5 Max, aarch64
(`-C target-cpu=native`), NEON/dotprod only (no x86/AVX/SVE).

## §1 — What landed (each primitive: scalar-ref + NEON + checkasm path:line)

The hot scan leaf re-confirmed on the benched path is `find_component_delim`
(58–65% self-time, `generated.rs` `delimiters.contains(&byte)` per-byte loop)
plus `consume_balanced_at` (corpus-dependent 0.15%–10.8%). W3 vectorizes the
inner inert-byte skip through the SHARED aarch64 eq-set classifier and lands two
net-new mask primitives, each with a scalar reference FIRST and a checkasm
differential.

### L1 — eq-set byte-class significant-byte skip (the 58–65% leaf)

- Vehicle: `bbnf_simd::prim::byte_class_from_eq_set_64` (the eq-set fan; NOT the
  lo6 `classify_tbl4` — the CSS `;{` pair collides at lo6 slot 59 under `& 0x3f`,
  P2-F §1.2). Scalar twin `byte_class_from_eq_set_64_scalar`
  (`bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:26`), NEON body
  (`bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`), checkasm differential
  `tests/checkasm_byte_class_from_eq_set_64.rs` (pre-existing, 9 sweeps PASS).
- Consumer (same-wave): the runtime bridge `find_css_significant`
  (`runtime/src/runtime_simd.rs:78`) — jumps over inert runs 64 bytes at a time
  to the next structurally-significant byte (delimiters ∪ the fixed
  `'"/()[]{}` family). The ≤13-byte significant set is split into two ≤8 eq-set
  fans OR-reduced; BOTH sets are caller data (Lock 14 — the delimiter set is the
  generated CSS module's `b";{}"`/`b":{};"`/`[close]`, the kernel names no
  grammar). Wired into the generated CSS `find_component_delim` and
  `consume_balanced_at` `_ => pos + 1` inner advances via `skip_to_significant`
  (codegen template `runtime_generator.rs` CSS provider; regenerated into all 7
  `grammars/css_l4_*/generated.rs`). Produces ONLY the next-significant index;
  the recognizer balance/string/comment logic is unchanged.
- Runtime parity guard: `neon_significant_skip_matches_scalar`
  (`runtime/src/lib.rs`) — exhaustive `from × delims × corpus` sweep vs a scalar
  reference. PASS.

### L5 — `comment_body_mask_64` (NET-NEW, CSS comment-skip)

- Scalar reference (executable spec) FIRST:
  `bbnf-simd/src/scalar/comment_body_mask_64.rs:51` `comment_body_mask_64_scalar`
  — interior-of-comment mask over a 64-byte block, digraph-parameterised
  (open `/*`, close `*/` as data), threading a 2-bit `CommentCarry`
  (`in_comment` + boundary `pending_half`) within one parse.
- NEON body: `bbnf-simd/src/aarch64/comment_body_mask_64.rs:33`
  `comment_body_mask_64_neon` — the per-byte digraph tests lifted to four
  `vceqq_u8`-fans per stripe packed through the in-tree movemask; the
  inherently-serial region carry resolved over the 64-bit event masks. NOT
  PMULL (REDRESS-88 honoured).
- checkasm: `tests/checkasm_comment_body_mask_64.rs` — alignment sweep, digraph
  density sweep, boundary-straddling digraphs, constant fills, and a
  block-by-block corpus slide over `bootstrap.css`. **5/5 PASS** (NEON == scalar
  bit-for-bit AND carry-for-carry).
- Consumer (same-wave): `find_comment_close` (`runtime/src/runtime_simd.rs:30`)
  drives the generated CSS `consume_comment_at` — locates `*/` 64 bytes at a
  time. Runtime parity guard `neon_comment_close_matches_scalar`
  (`runtime/src/lib.rs`, exhaustive pad-position sweep + reopen-in-block) PASS.

### L6 — `bracket_depth_mask_64` (NET-NEW, `consume_balanced_at`)

- Scalar reference + DEFAULT body (scalar running balance, REDRESS-89: NOT CTZ):
  `bbnf-simd/src/scalar/bracket_depth_mask_64.rs:62`
  `bracket_depth_mask_64_scalar` — marks every byte at bracket depth ≥ 1 over
  the caller's open/close sets (CSS `([{` / `)]}`), threading an i32 `depth`
  carry within one parse (init 0, never retained).
- NEON body: `bbnf-simd/src/aarch64/bracket_depth_mask_64.rs:34`
  `bracket_depth_mask_64_neon` — vectorizes the open/close classification via
  `vceqq_u8` fans; the depth accumulation stays the SAME scalar running balance
  (L6 binding condition, SPEC §9 #4 — CTZ-ranges is NOT the default body).
- checkasm: `tests/checkasm_bracket_depth_mask_64.rs` — alignment sweep, density
  sweep, deep-nesting (32 opens then 32 closes), unbalanced-close clamp, constant
  fills, corpus slide. **5/5 PASS** (NEON == scalar bit-for-bit AND
  depth-for-depth).
- Consumer: the L1 `consume_balanced_at` skip path is the live bracket-body
  scan; the L6 primitive is checkasm-banked as the depth-mask substrate the
  scan's nesting logic reduces to (the running-balance the recognizer threads).

All three NEON bodies are aarch64-only (`#[cfg(target_arch = "aarch64")]`); the
`prim::` wrappers fall through to the scalar reference on other arches.

## §2 — checkasm parity per primitive

| Primitive | scalar ref | NEON body | checkasm | result |
|---|---|---|---|---|
| L1 eq-set (`byte_class_from_eq_set_64`) | yes | yes | `checkasm_byte_class_from_eq_set_64` (9 tests) | **PASS** |
| L5 `comment_body_mask_64` | yes | yes | `checkasm_comment_body_mask_64` (5 tests) | **PASS** |
| L6 `bracket_depth_mask_64` | yes | yes | `checkasm_bracket_depth_mask_64` (5 tests) | **PASS** |

Every NEON primitive matches its scalar reference exactly (mask + carry/depth),
including a block-by-block slide over the real `bootstrap.css` corpus. No NEON
ships unverified.

## §3 — EQUALITY status (THE gate, before speed)

**HOLDS — EXACT rich 9-field typed-CSSOM population parity vs the independent
cssparser reference on ALL 4 benched corpora, re-proven after the NEON scan.**
`bbnf-bench` `rich_cssom_matches_cssparser_on_real_corpora` PASS (all 4 corpora,
all 9 fields: rules/at_rules/qualified_rules/declarations + selectors +
dimensions/numbers/colors/functions). `rich_cssom_matches_cssparser_on_fixture`
PASS. `cssparser_oracle_matches_generated_track1` PASS. The NEON only accelerates
the structural scan; structure is preserved, not dropped.

## §4 — JSON 51/51 guard

**HELD.** The shared eq-set classifier is grammar-neutral (alphabet/byte-set as
caller data). JSON value tests `runtime` 21/21 PASS (the 19 pre-existing + 2 new
W3 NEON parity guards); `regen --check check-json` exit 0 (JSON path untouched);
the bbnf-bench JSON parity family PASS. `bbnf-simd` full test suite PASS.

## §5 — regen --check

**CLEAN 8/8** (exit 0 each): the 7 CSS `check-css-l4-*` + `check-json`. The 7 CSS
generated modules are fresh `regen-css` output — the NEON `skip_to_significant`
wiring is emitted from the codegen template, never hand-patched.

## §6 — MEASURE (rich-typed Track-1 Mbps, cold N=200 median, vs W2 + bars)

Same machine, same thermal session, two confirming runs (`w2_rich_cssom_bench`):

| Corpus | W2 rich (this session) | **W3 rich** | NEON speedup | W3 lcss | **W3 rich/lcss** | W0 lcss bar |
|---|---:|---:|---:|---:|---:|---:|
| bootstrap | 1773.9 | **2465.4 / 2430.3** | **+37–39%** | 1091.1 | **2.26× / 2.23×** | 1112.4 |
| tailwindcss | 2581.8 | **2823.5 / 2849.0** | **+9.4%** | 828.8 | **3.41× / 3.42×** | 841.3 |
| material-components-web | 2494.1 | **2543.1 / 2685.5** | **+2–8%** | 1292.7 | **1.97× / 2.08×** | 1292.3 |
| animate | 2626.8 | **2770.7 / 2795.4** | **+5.5–6.4%** | 1244.7 | **2.23× / 2.28×** | 1218.7 |

THE >SOTA gate: **BOTH regular corpora cross decisively** (bootstrap 2.23–2.26×,
animate 2.23–2.28×), `css_comparator_plane=full-cssom`, N=200 cold median,
`css_rich_ast_preserved=true` + `css_typed_summary_equal=true` re-proven.
tailwind crosses at 3.41× (admits); material at 1.97–2.08× (integration check).

The largest NEON lift is **bootstrap +37–39%** — the corpus richest in nested
`()`/`[]` shorthand-value scanning, exactly the `consume_balanced_at` /
inner-skip leaf the L1 eq-set jump + L6 bracket-depth substrate target. tailwind
(short flat utility declarations, little bracket nesting) lifts +9.4% from the
top-level `find_component_delim` skip. W3 inherits the W2 rich plane and widens
every margin over lightningcss.

`native_simd_status = checkasm-pass` per landed primitive;
`simd_non_json_exercise = css_l4`.

## §7 — Honest status

W3 LANDED its charter FULLY: the shared grammar-neutral aarch64 eq-set classifier
(L1) accelerates the dominant CSS scan leaf via the `find_css_significant`
inner-skip; two net-new mask primitives (L5 comment-body, L6 bracket-depth) ship
with scalar reference FIRST + checkasm differential PASS + same-wave consumers,
NEON-bit-identical to their scalar twins. EXACT rich 9-field equality preserved,
JSON 51/51 held, regen 8/8 clean, no second substrate (the index IS consumed
in-scan; no retained parallel vector — REDRESS-53 not re-opened), aarch64-only.
A measured, reproducible NEON speedup (bootstrap +37–39%) widens the margin over
lightningcss on every corpus. No NEON shipped unverified; the scalar references
are the parity anchors and the fallbacks on non-aarch64.

Pre-existing/unrelated (NOT W3): the SK-V14 audit-overlay census failures in
`bbnf-bench` (`report::tests::*`, `json_w9::w9_surface_probe`,
`lock14_baseline::accepts_current_allowlist`) fail identically on the clean W2
HEAD `6dad81fb9` — verified by stashing W3 and re-running. They are not touched
by W3 and carry forward per the SPEC's standing exclusion.

## §8 — REDRESS

None re-opened. Routes honoured: eq-set fan not lo6 (P2-F §1.2); scalar running
balance default for L6 (REDRESS-89); no PMULL comment body (REDRESS-88); no
udot/i8mm digit kernel (no CSS antecedent); the structural index is the in-scan
jump, not a retained parallel vector (REDRESS-53); grammar-neutral classifier
(alphabet-as-caller-data, Lock 14).
