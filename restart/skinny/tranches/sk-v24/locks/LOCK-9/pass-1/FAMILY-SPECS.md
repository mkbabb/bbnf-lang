# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) · pass-1 FAMILY-SPECS

*Synthesis, pass 1. One cogent specification per family while routes remain incompatible;
then the incompatibility map. Elegance earns nothing on its own — a family stalled at a
missing primitive equal in difficulty to the original problem is marked BLOCKED-candidate.
Substrate claims below were spot-verified this session against the live trees (paths:lines
cited are real unless flagged SPEC-ONLY).*

Four families sit on **three distinct axes**, which is why some are opposites and some are
orthogonal:

- **Kernel-home axis** — *who owns the byte kernels.* 9A (one shared crate) ⊥ 9B (parse-that
  keeps its own). Direct opposites; this is the lock's central adjudication.
- **Scanner-provenance axis** — *are scanners authored or derived.* 9C (derived from the
  bootstrapped grammar's DFA; hand scanners cease to exist) vs {9A, 9B} (authored; either
  thinned to consumers or hand-rewritten).
- **Input-surface axis** — *whole-input vs windowed.* 9D (streaming Window witness) is
  orthogonal to all three, but contradicts the `PaddedView` eager invariant they silently share.

**Verified substrate (the shared floor).** `bbnf-simd` lives at `skinny/crates/bbnf-simd/src/`
(`aarch64/`, `scalar/`, `dispatch.rs`, `lib.rs`, `classifier.rs`); `movemask.rs:5` is
`vshrn_n_u16::<4>` (the law-compliant narrow). `dispatch.rs:59` is an `OnceLock<PrimitiveKernels>`
fn-pointer table; `:42` `select_classifier`, `:89` `select_backend`, `:101` `lo6_table_admissible`
(a compile-time-admissibility precedent). Carry kernels exist: `lib.rs:169` `prefix_xor_64(mask,
carry_in)`, `:174` `escape_mask_64(bs_mask, bs_carry_in) -> (u64,bool)`, `CommentCarry`. **The
`prim` facade is SPLIT** (verified): `byte_class_from_eq_set_64`, `structural_classify_64`,
`comment_body_mask_64` dispatch via direct `#[cfg(target_arch="aarch64")] return neon; scalar`
(inlineable); but `bitmap_prefix_xor_64`, `byte_class_from_table_64`, `bitmap_next_set_bit`,
`bulk_emit_positions_64`, `eob_pad_clamp` route through `crate::dispatch::primitive_kernels().<fn>`
— an **indirect call per invocation**. parse-that side: `state.rs:57` `INPUT_PAD_BYTES=64`,
`PaddedView` witness (`:111-114`, "load any stripe without a per-chunk guard"), `ws_bitmap`
(`:228`, the one extant lazy window cache); `number_simd.rs:54-55` still runs the forbidden
**double-`vaddv_u8`** in `count_leading_digits_neon`; `.cargo/config.toml:3 [patch.crates-io]`
already path-deps `bbnf`, `bbnf-regex`, `bbnf-ir`, `bbnf-ser`, `csp-solver`, `egraph`,
`egraph-derive` — **`bbnf-simd` is absent** (9A's whole physical seam is one added line);
`lib.rs:1 #![feature(portable_simd)]` is the nightly pin every route must delete.

**Binding laws (SPEC §0, non-negotiable, carried by every family's gates).** No stable-port
perf claim — de-nightly is STABILITY only, expected cyc/B parity, the no-regression gate may
FAIL (the shim failed window A 0.81–0.98×); "faster" appears in no gate. `vshrn` narrow, never
double-`vaddv` (the 2271-vs-509 `addv.8b` counterexample). No digit-scan SIMD (§0/02 NO_GO —
`number_simd` ports behaviour-preserving, never Mula/Lemire in-vector accumulation). No
block-scheduling pursuit (§0/04). x86 dead (purge `number_simd.rs:145-235` SSE4.2 +
`quote_parity.rs:42-49` pclmulqdq; `rg -E '_mm_|x86_64::|is_x86_feature' → 0`). Warm benches are
disinformation — cold, paired, N≥200, two windows, opposite arm order; PMU where the OS allows,
else divan wall-clock ratio (PMU cyc/B unavailable in macOS userspace). Consumer-less substrate
forbidden (Y.13).

---

## 9A — ONE-KERNEL-LIBRARY  ·  disposition: **ADVANCED (born-RED on the inline proof)**

**Architecture.** Collapse the two aarch64 kernel worlds — mature stable-clean checkasm-gated
`bbnf-simd`, and parse-that's eight `portable_simd` scanners — into **one crate, `bbnf-simd`**,
extended with a composable `vector/` lane-vocabulary tier, consumed by parse-that as a path-dep.
The isomorphism is *physical*: one source, drift impossible by construction. The win is not DRY
hygiene — physical sharing hands parse-that the `vshrn` movemask and the scalar-parity gate it
currently lacks, for free (its scanners today have integration tests but **no scalar-reference
differential**). Post-campaign home is the greenfield `crates/bbnf-simd` (GN-1 port-in).

**Mechanism.** Two consumer tiers. *Kernel tier* (strong iso): where a parse-that scan maps 1:1
to an existing whole-kernel, delete the hand copy and consume the kernel — `prefix_xor`→
`prim::bitmap_prefix_xor_64`, `escape_mask_64`→`escape_mask_64`, `classify_stripe_64`+
`odd_parity_backslashes`→`string_block::scan_string_special_block`, `structural_bitmap`→
`structural_classify_64`+`compact_mask`, decode `\uXXXX`→`unescape_uxxxx`. *Vector tier* (escape
hatch): where no kernel matches (`decode store_chunk`, `ws_comment`, `scanners`, `span_parser/scan`),
rewrite `use std::simd::prelude::*` → `use bbnf_simd::vector::*` — one-line swaps, shim ergonomics,
but the vocabulary is now shared and its `to_bitmask` **delegates to the existing `vshrn` movemask**
(this is the refuted 276-LOC `simd128.rs` promoted into the shared crate, minus its regressing
double-`vaddv`). Physical seam: **path-dep primary** (the memchr model — one kernel crate, many
consumers; add `bbnf-simd = { path = "../../bbnf-lang/crates/bbnf-simd" }` to `.cargo/config.toml`),
**vendor-under-canonical-diff-gate only if a standalone parse-that build is a hard requirement**
(the csp-solver precedent).

**Integration seams.** Delete/rewrite: `quote_parity.rs:41/:71/:89`, `quoted_simd.rs`,
`structural_bitmap.rs`, `decode.rs:296`, and the four vector-tier files. Provide/extend:
`aarch64/movemask.rs` (the reroute target), the `prim` module, `aarch64/{string_block,
unescape_uxxxx,structural_classify_64}.rs`; **ADD** `src/vector/{mod,neon,scalar}.rs`; **EXTEND**
`tests/checkasm_*.rs` with scalar refs for the parse-that-origin surface (in-string-bitmap
composition; prefix_xor/escape_mask already have bbnf-simd scalar bodies).

**Gates.** (1) Cross-crate inline: `otool -tv` the wired scanner proves the kernel **inlined** —
no `bl`/`blr` to the primitive in the block loop. (2) Paired no-regression ≥0.98× per corpus,
both windows, N≥200. (3) Movemask parity: rerouted `to_bitmask` shows `vshrn`, reduction-count
≤ 509 baseline. (4) checkasm green over the merged surface. (5) Path-dep-vs-vendor cost artifact.

**Exact open questions.** **(Q1, make-or-break, sharpened this session)** The kernels parse-that's
hot paths need — `bitmap_prefix_xor_64`, `byte_class_from_table_64`, `bulk_emit_positions_64` —
route through the `OnceLock` **fn-pointer table**, not the direct-`cfg` path. An indirect call per
block is a near-certain regression that fails gate 2. Does the crate need a refactor exposing
direct-`cfg` inline entry points for those kernels (feasible — three others already do it), and
does fat-LTO carry `#[inline]` across the path-dep boundary? **Unproven; this is the family's RED
edge.** (Q2) Padding-contract seam: parse-that's `PaddedView`/`INPUT_PAD_BYTES=64` vs bbnf-simd's
`prim::eob_pad_clamp` — risk of double-padding at the tail block. (Q3) Abstraction granularity:
kernel-tier is 64-byte-block, several parse-that scans are 16-byte-lane compositions → the physical
iso is lane-level (vector tier) for those, weaker than the charter ideal; the mapping census
quantifies the split. (Q4) `number_simd` is NO_GO territory — kept behaviour-preserving, x86 arm
excised, `digit_mac` NOT adopted.

**Why not BLOCKED.** No missing primitive of equal difficulty — Q1's fix is a known refactor
(add direct-`cfg` wrappers) plus a build-config proof, not a new invention. But the family cannot
be declared until Q1 is measured; born-RED.

---

## 9B — NATIVE-REBIRTH  ·  disposition: **ADVANCED (incumbent — PT-2 frozen codex chose this)**

**Architecture.** parse-that keeps its own kernels, each inner loop **rewritten directly in
`core::arch::aarch64`** (stable since 1.59 — no nightly), one scalar twin per kernel as the
correctness ground truth, a natively-ported checkasm harness proving each NEON body is both
*reached* (liveness) and *bit-parity*. **No physical code shared with bbnf-simd.** The isomorphism
to bbnf-lang's kernel home is *behavioral, by discipline* — same movemask idiom, same checkasm
protocol, same scalar-oracle law — never a vendored crate. Central bet: the "reasonable point" of
rust↔rust kernel iso (Constitution iii) is design-level, and duplicated-small-kernel drift is
cheaper to carry than a shared crate's lowest-common-denominator API + sync-gate tax. **That bet
is a null hypothesis to defeat by measurement** — if post-rebirth the kernels are near-identical,
9B loses to 9A on merit.

**Mechanism — the movemask taxonomy IS the family.** Three conventions, scanner picks by
*consumption pattern*: **(a) nibble-stride 4-bit/lane** (`vshrn_n_u16::<4>`, read via
`trailing_zeros()>>2`) for every position-scan — one `shrn.8b`+`fmov`, no reduction. **(b) dense-64
1-bit/byte** (simdjson `neon_movemask_bulk`: `vandq` bit-positions + three `vpaddq` pairwise-add
tree) for the ONE site that assembles a 64-bit mask across four blocks feeding a whole-word op
(`quote_parity::classify_stripe_64` → `prefix_xor` clmul). **(c) FORBIDDEN double-`vaddv` dense-16**
— *already live* at `number_simd.rs:54-55` (verified); its consumer wants only `trailing_zeros()`,
so it is strictly dominated by idiom (a). Engineered form: a thin `simd/movemask.rs` exposing
exactly two lawful producers (`movemask_nibble`, `movemask_bulk64`) + `first_match` sugar that
**elides the repack** bbnf-simd's movemask keeps. Module: `src/simd/{mod,movemask,neon,scalar}.rs`
— the flat shim productionized into a directory-module. NEON is baseline-mandatory on aarch64 →
purging x86 also deletes the entire `is_*_feature_detected!` machinery as dead weight.

**Integration seams.** Rebirth the eight scanners (census: `scanners.rs`, `structural_bitmap.rs`,
`quote_parity.rs`, `decode.rs`, `ws_comment.rs`, `span_parser/scan.rs`, `quoted_simd.rs`,
`state.rs`); purge the two x86 arms (`number_simd.rs:145-235`, `quote_parity.rs:42-49`), keep the
aarch64 `vmull_p64` + scalar oracles. Co-travelers (price of leaving nightly, inherited not owned):
`smallbox[nightly]`→stable `smallbox!` at `parse.rs:74`; delete `portable_simd`/`stmt_expr_attributes`
feature attrs once the last `std::simd` site is gone. Bench seam: divan `harness=false`, mimalloc,
fat-LTO/cgu=1/O3.

**Gates.** (1) Disasm liveness: `otool -tv` shows `shrn`/`vpaddq`/`tbl` present on the hot path
(parity ≠ liveness — a scalar-oracle agreement does NOT prove the NEON path ran). (2) Insn-count
no-regression: reduction ops ≤ baseline (2271-vs-509 must not reproduce). (3) checkasm-liveness
harness ported natively (alignment sweep 0..63, 10k random + JSON-alphabet vectors, stack-canary,
SIGSEGV/BUS/ILL guard, bug-injection, `cfg!(target_arch="aarch64")` liveness assert; both
`vmull_p64`-on and scalar-fallback arms). (4) Paired divan ≥0.98× every corpus both windows N≥200
(the gate the shim FAILED). (5) **The no-coupling cost, measured** — LOC duplicated vs bbnf-simd,
substrate-divergence table (`PaddedView`-64 vs unpadded blocks; `vqtbl1q_u8` single-table shufti vs
bbnf-simd's `vqtbl4q_u8` four-table classify), a shared differential conformance corpus *as data*
proving anti-drift without shared code. (6) `cargo +stable build`, `portable_simd` deleted.

**Exact open questions.** (Q1) The dense-64 quote-parity site is the sharpest self-regression risk
— naively applying nibble-stride `vshrn` there yields 256 bits and corrupts parity; it MUST use
idiom (b). (Q2) `vmull_p64` is `target_feature="aes"`-gated; the harness must exercise both arms so
the scalar fallback cannot rot. (Q3) The null-hypothesis measurement (gate 5): is post-rebirth LOC
divergence large enough to justify two homes, or does it concede to 9A?

**Why not BLOCKED.** Every intrinsic is stable `core::arch` since 1.59; the harness exists to port;
the taxonomy is settled art. Cleanest to execute; the incumbent. Its only failure mode is *losing to
9A on merit* (gate 5), not a missing primitive.

---

## 9C — SCANNER-AS-DFA  ·  disposition: **BLOCKED-candidate (cross-lock-coupled to LOCK-5; dominant-pass value is parity, not speed)**

**Architecture.** The scanners are not primitives — each is a finite-state acceptor a regex leaf in
the bootstrapped grammar already denotes (JSON string body = `"(?:[^"\\]|\\.)*"`; number span; ws+
comment = `RegexClass::WhitespaceWithBlockComment`; structural = byte-equivalence-class partition).
9C **dissolves the hand corpus**: acceptors authored once in `bootstrap/regex.bbnf`, compiled through
bbnf-regex's existing HIR→NFA→DFA→Hopcroft pipeline, lowered to a NEON kernel as a *compiled product*.
"Scanner" becomes a codegen tier, not a source file. The organizing law: the **derivation** (grammar
leaf → DFA → byte-class + transition tables → kernel template) is the architecture; the kernel is
downstream. Physical kernel-iso between repos is a *consequence* of both lowering to bbnf-simd's
classify primitive — reached through the DFA, not by extracting a library.

**Mechanism — the four-tier lowering ladder**, selected per site from the compiled DFA's own shape
by a plan predicate (the LOCK-2 scan-plan-selection e-graph island). **Tier 0 prefilter** (memchr
1/2/3, entry-set ≤3 bytes) — CONFIRMED (PROVE-10, 8.8–29.5× prefilterable, 0.99× controls).
**Tier 1 shufti/truffle** (single self-loop over a byte class; ws-skip, `[^"\\]+` interior) — a NEON
`tbl` nibble-LUT sourced from `Dfa::byte_classes`, CONFIRMED (skip-heavy ~7.5×/4.7×, dense-neutral
1.03–1.15×). **Tier 2 Sheng** (≤16 states, ≤16 classes — JSON string ≈3×3, number span ≈6–8×7):
flat transition table fits NEON registers, per-byte transition = `vqtbl1q_u8`/`vqtbl4q_u8` lookup —
**the genuine novum: a vectorized DFA scanner generated from a grammar leaf.** bbnf-simd's
`classify_tbl4.rs` already commits the `vqtbl4q_u8` 64-entry form. **Tier 3 simdjson-structural**
(quote-delimited-with-escape leaf → `RegexClass::QuotedString`): prefix-XOR quote parity, template
instantiated with the leaf's `quote_char`/`escape` as compile-time immediates (the CONFIRMED
emit-spine residualization; one template, JSON+CSS products). Fallthrough: scalar `find_at`
transition loop (always correct, never absent). bbnf-simd is the lowering backend for Tiers 1–3 —
so lowering fixes the movemask defect by construction.

**Integration seams.** DFA payload: `parse-that/rust/regex/src/automata/dfa.rs` (`byte_classes:52`,
`num_classes:54`, `flat_transitions:50`, `find_at:116` = the semantic oracle). Template selection:
`regex/src/classify.rs:55` `RegexClass` (+`canonical_pattern():132`) — the seam extended from "pick a
hand scanner" to "pick a lowering tier". Backend: `bbnf-simd/src/aarch64/classify_tbl4.rs`,
`movemask.rs`. Deletes the eight scanners (behavior re-derived). Codegen home: the recognizer-
derivation pass (post-GN-1 `crates/codegen/src/emit/regex/`). Grammar source: `bootstrap/regex.bbnf`
(constitution clause i).

**Gates.** PROVE-9C-A Sheng feasibility census (state×class per leaf, go/no-go for Tier 2 — cheap,
deterministic, no bench). PROVE-9C-B one Sheng scanner paired ≥0.98× dense + skip-win on interior,
`otool` shows `vqtbl`+vshrn never double-`vaddv`. PROVE-9C-C structural bitmap re-derived from a
compiled DFA's `byte_classes`, ≥0.98×, identical position stream, zero `portable_simd` tokens.
PROVE-9C-D deletion census — byte-identical scan streams over JSON+CSS, count of deleted files, named
residue for every Tier-miss (coverage is a measured percentage, not an assertion).

**Exact open questions & why BLOCKED-candidate.** (Q1, the family-killer) **The dense-classification
no-win**: structural bitmap classifies every byte — nothing to skip — so a DFA-lowered dense classify
is at best PARITY with hand simdjson. 9C's value on the *dominant* pass is therefore NOT speed; it is
deletion + grammar-specialization + stable-by-construction + free vshrn movemask. Real but modest. (Q2)
Sheng state/class explosion — a rich CSS `url()` or wide unicode class blows the ≤16×16/≤64 budget and
falls to scalar; the number DFA is the boundary case; **census-first is mandatory and its outcome is
unknown.** (Q3, the coupling) 9C's central pipeline — bootstrapped regex grammar as the DFA source of
truth, HIR→DFA→Sheng lowering — **is LOCK-5's deliverable, not LOCK-9's.** 9C cannot converge
independent of LOCK-5's verdict on the regex continuum, the bootstrap staging, and the DFA-as-source
law. This is the missing-primitive test: the novum (Tier-2 Sheng codegen) rides on existing DFA infra
+ existing tbl4 kernel, so it is *not* a primitive of equal difficulty — but its **enabling substrate
(the bootstrapped grammar → DFA → table pipeline as a stable, byte-fixpoint-gated source of truth) is
owned elsewhere and is itself unbuilt at the fidelity 9C needs.** Marked BLOCKED-candidate:
cross-lock-coupled + dominant-pass value gated to parity. Not retired — Tiers 0/1/3 are CONFIRMED art
and the deletion dividend is real; the family survives even if Tier 2 fails census, degraded to
"Tiers 0/1/3, dense stays hand-written" (a partial that must be named, not hidden).

---

## 9D — LAZY-SURFACE  ·  disposition: **BLOCKED-candidate (memory center gated on an unmeasured backtrack bound + an absent streaming consumer)**

**Architecture.** Streaming-first: scanners as incremental lazy planes over input windows; kernels
selected by a cost model at combinator-compile time; streaming parse becomes the design center.
Partiality-as-witness (the winnow `Partial<I>` model, NOT nom's forked streaming/complete APIs which
violate the one-path edict): a `Window` witness carrying (a) the ≤64-byte block view, (b) threaded
inter-block scanner carries, (c) a `final: bool` flag. **The eager whole-input parse is the degenerate
case** — one window = the whole padded buffer, carries zero, `final=true`. One code path; streaming is
the general case. This dissolves the kernel-sharing question physically: the streaming primitive set is
bbnf-simd's carry kernels, the `Window` witness is the shared calling convention.

**Mechanism.** The carries ARE the streaming vocabulary, and bbnf-simd already speaks it (verified):
`prefix_xor_64(mask, carry_in)`, `escape_mask_64(bs_mask, bs_carry_in) -> (u64,bool)`, `CommentCarry`,
`bracket_depth_mask_64(..., depth) -> (u64,i32)` — the *resumable* form of what parse-that's PaddedView
scanners compute *whole-buffer*. 9D refactors parse-that's scanners onto that carry-threaded window
form. The extant `ws_bitmap`/`ws_bitmap_start` (`state.rs:228`) is the ONE ad-hoc lazy window cache —
9D lifts that pattern to first class for every scanner. Compile-time `KernelPlan`: at `Parser::new`
(`parse.rs:74`) derive a plan from static grammar properties (alphabet admissibility from
`lo6_table_admissible`; prefilter tier from `Prefilter::from_dfa`; stride), store it in the scanner
node, the window loop reads it — folding PROVE-10's *runtime* prefilter branch and bbnf-simd's
`OnceLock` *runtime* dispatch into a *compile-time-selected* plan chosen once per node.

**Integration seams.** `state.rs:57/:82/:122/:285` (the whole-input allocation to generalize);
`state.rs:228` (the ws_bitmap seam to lift); `lazy.rs:20/:49` (**name-collision to disambiguate** —
that is *combinator-graph* laziness for recursion, a different axis from *input-window* laziness; they
meet at `Parser::new` but must not be conflated); bbnf-simd `dispatch.rs:101 lo6_table_admissible`,
`lib.rs:105 scan_dispatch` (already a block-window loop), the carry kernels; PROVE-10
`automata/prefilter.rs`. Best refactor candidate: `structural_bitmap.rs::classify_stripe` (already
stripe-at-a-time with an implicit whole-buffer carry).

**Gates.** P1 one scanner windowed, divan paired ≥0.98× six JSON corpora both windows (honest expected
outcome: throughput-neutral; >2% regression = the straddle guard is real and priced). P2 the streaming
consumer — a chunked-feed harness measuring **peak RSS** + time-to-first-structural vs eager; peak RSS
strictly < eager is the raison d'être (the Y.13 live consumer). P3 the backtrack bound — instrument
`furthest_offset − offset` rewind distance across JSON+CSS (with and without `diagnostics` recovery).
P4 compile-time KernelPlan, `cargo asm` proves no per-call dispatch in the hot loop.

**Exact open questions & why BLOCKED-candidate.** (Q1, the killer) **Backtracking defeats the memory
claim.** parse-that is a backtracking PEG — `alt`/`recover` can rewind past a window. If rewind distance
is unbounded (recovery to document start), the ring buffer must retain all consumed input = the
whole-input model with a lazy tail, **zero peak-RSS win**. The memory center survives ONLY if a bounded
backtrack window is *measured* for the target grammars — and that measurement does not exist. **This is
the missing-primitive test in its exact form: to make streaming's memory win real, 9D needs a bounded
backtrack guarantee that parse-that's PEG semantics may simply not provide — a property equal in
difficulty to redesigning the backtracking discipline itself.** (Q2) The straddle cost resurrects the
per-window tail guard `PaddedView` was built to delete → streaming is expected neutral-to-slower on
throughput; 9D must make ZERO throughput claim. (Q3, Y.13) A `Window` plane needs a real streaming
consumer in-wave — but parse-that's actual consumers (value.js, latex-paper, keyframes) parse
whole-document JSON/CSS; **no streaming consumer is named.** simdjson's own `document_stream` streams
ND-JSON at *whole-document* granularity, never a byte-partial document — the realistic unit is a
complete small document, which weakens the peak-RSS argument further. Marked BLOCKED-candidate: the
raison d'être (peak memory) is refuted-by-default unless P3 measures a bounded backtrack AND a real
streaming consumer materializes; absent both, 9D collapses to latency/time-to-first-token only. Either
outcome is a valid close *with an artifact* (P3 is cheap and deterministic).

---

## THE INCOMPATIBILITY MAP — which families cannot coexist, and precisely why

**9A ⊗ 9B — HARD-INCOMPATIBLE (same axis: kernel home). THE lock's central adjudication.**
9A's thesis is one shared crate (`bbnf-simd`) owning the kernels; parse-that scanners become thin
consumers via path-dep. 9B's thesis is parse-that keeps and hand-rewrites its own kernels with **no
physical code shared**. These are logical negations on one axis — a kernel is either shared-source or
own-source; it cannot be both. They CANNOT be composed; a gestalt must choose. The prior-art
adjudication (research-priorart §4): PT-2's frozen codex chose **9B explicitly** (parse-that is
READ-ONLY to the bbnf-lang campaign, non-clobber P23; the port's value is stability not coupling), and
9B is the null hypothesis to be *defeated by measured drift cost* — it stands until 9A's shared home is
shown to reduce measured maintenance below two-home drift. Both routes reach the *same* movemask idiom
(`vshrn`) and the *same* checkasm discipline; they differ only in whether one file or two files carry
them. The decider is Q1 of each: does 9A's cross-crate consumption inline no-regression (my fn-pointer
finding says: unproven), and does 9B's post-rebirth LOC-divergence justify two homes.

**9C ⊗ 9B — INCOMPATIBLE (scanner-provenance axis).** 9C deletes parse-that's hand kernels and emits
them as compiled DFA products; 9B's entire thesis is that parse-that *authors and hand-rewrites* its
kernels. A scanner cannot be simultaneously hand-authored-in-`core::arch` (9B) and non-existent-because-
generated (9C). Cannot coexist as organizing principle. (They could co-occur only in the degenerate
sense that a Tier-miss leaf in 9C falls back to a hand kernel — but that is 9C conceding coverage, not
9B being adopted.)

**9C vs 9A — PARTIALLY COMPATIBLE (agree on kernel home, disagree on scanner provenance) → a DAG
relationship, not a contradiction.** Both converge on `bbnf-simd` as the kernel backend: 9A by
extraction, 9C by lowering-to-it through the DFA. They AGREE on the kernel-home axis. They DISAGREE on
provenance: 9A keeps parse-that scanners as authored thin consumers (deleted only where 1:1); 9C makes
them non-existent generated products. This is sequenceable — 9A establishes `bbnf-simd` as the one
kernel home first, 9C later becomes the *generator that emits consumers of that home*. A gestalt can
hold both as ordered phases (kernel-home unification → scanner-derivation), NOT as a single-wave
contradiction. Note the double-home the gestalt must collapse (research-priorart §6): bbnf-simd's
`find_ascii_set_member64` and bbnf-regex's `Dfa::find` are two entry points to the same prefilter idea.

**9D vs {9A, 9B, 9C} — ORTHOGONAL AXIS (input surface) but PRECONDITIONED on generalizing `PaddedView`.**
9D changes *what feeds* the kernels (windowed vs whole-input), not *who owns* or *how derived* the
kernels are. At the mechanism level 9D composes with any of the three: its `Window` witness could thread
carries into 9A's shared kernels, 9B's native kernels, or 9C's compiled DFA products (9D explicitly
names bbnf-simd's carry kernels as its streaming primitive set — the same kernels 9A shares / 9B/9C
produce). **The one contradiction:** 9A/9B/9C all silently assume the eager `PaddedView`/`INPUT_PAD_BYTES
=64` whole-input witness (`state.rs:122`) that elides the per-block tail guard; 9D must *generalize or
dissolve* that witness, and its honest cost is exactly the tail guard PaddedView removed. So 9D is
compatible-in-principle with all three but requires re-founding the substrate floor they build on — a
composition cost, not a logical exclusion. Because 9D is BLOCKED-candidate on its own memory center
(backtrack bound + streaming consumer), the practical reading is: 9D is a *later, conditional* overlay
on whichever kernel-home/provenance verdict wins, admissible only if P3 measures a bounded backtrack.

**Summary matrix** (⊗ hard-incompatible · ◐ partial/sequenceable · ∥ orthogonal-composable):

|        | 9A            | 9B            | 9C            | 9D            |
|--------|---------------|---------------|---------------|---------------|
| **9A** | —             | ⊗ (kernel home)| ◐ (agree home, differ provenance; DAG) | ∥ (needs PaddedView generalized) |
| **9B** | ⊗             | —             | ⊗ (provenance)| ∥ (needs PaddedView generalized) |
| **9C** | ◐             | ⊗             | —             | ∥ (9C DFA = 9D resumable-scanner; strong affinity) |
| **9D** | ∥             | ∥             | ∥             | —             |

**Cross-cutting note (Ragel/re2c lineage):** 9C's Sheng DFA and 9D's resumable scanner are the *same
object viewed on two axes* — a streaming scanner IS a resumable DFA (research-D §3, Ragel `write exec`
saved-state model). If both survive their gates, 9C's compiled DFA is the natural body of 9D's windowed
carry-threaded scanner — the strongest positive affinity in the map, and a gestalt-Ω consolidation
candidate.

---

## Disposition roll-up for the REGISTRY

- **9A ONE-KERNEL-LIBRARY — ADVANCED, born-RED.** Convergence blocker: the cross-crate inline
  no-regression proof (Q1), sharpened by the verified `prim`-facade fn-pointer split.
- **9B NATIVE-REBIRTH — ADVANCED, incumbent.** No blocker; cleanest execution. Its close is the
  null-hypothesis measurement (does it concede to 9A). The `number_simd.rs:54-55` double-`vaddv`→
  nibble-stride swap is its cleanest standalone win.
- **9C SCANNER-AS-DFA — BLOCKED-candidate.** Cross-lock-coupled to LOCK-5 (the bootstrapped
  grammar→DFA source-of-truth pipeline is LOCK-5's deliverable); dominant dense pass is parity-only.
  Survives degraded (Tiers 0/1/3) even if Tier-2 Sheng fails census. Census-first is mandatory.
- **9D LAZY-SURFACE — BLOCKED-candidate.** Memory raison d'être refuted-by-default unless the P3
  backtrack-bound measurement returns bounded AND a real streaming consumer materializes (neither
  exists today). Latency/time-to-first-token survives regardless; a cheap deterministic close.
