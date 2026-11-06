# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) · pass-2 FAMILY-SPECS

*Synthesis, pass 2 (Fable-tier judgment). One cogent specification per family while the routes
remain incompatible, then the incompatibility map. Elegance earns nothing on its own: a family
stalled at a missing primitive equal in difficulty to the original problem is marked BLOCKED. This
pass folds research-{A,B,C,D,priorart} pass-2 against the pass-1 registry (PROTO-1 9A/9B kernel
fragment; PROTO-2 9C Sheng census; CRIT-1/CRIT-2). Substrate paths:lines were re-verified against
the live trees this session (`.cargo/config.toml` confirms `bbnf-regex = { path = "regex" }`
in-repo and **`bbnf-simd` absent** from the patch table; scanner files and the `micro_parse_that` /
`parse_that_combinator` divan benches confirmed present).*

The lock is not four features. It is **four dispositions of one aarch64 scanner/kernel plane on
three orthogonal axes**:

- **Kernel-home** — *who owns the byte kernels.* 9A (one shared crate `bbnf-simd`) ⊗ 9B
  (parse-that keeps its own). Direct opposites; **the lock's central adjudication.**
- **Scanner-provenance** — *are scanners authored or derived.* 9C (derived from the bootstrapped
  grammar's DFA; hand scanners cease to exist) vs {9A, 9B} (authored — thinned to consumers, or
  hand-rebirthed).
- **Input-surface** — *whole-input vs windowed.* 9D (streaming `Window` witness) — orthogonal to
  all three, but contradicts the eager `PaddedView` invariant they silently share.

**Binding laws carried by every family's gates (SPEC §0, non-negotiable).** No stable-port perf
claim — de-nightly is STABILITY only; the no-regression gate may FAIL (the `portable_simd`→NEON
shim failed window A 0.81–0.98×); "faster" appears in no gate. `vshrn` narrow, never double-`vaddv`
(the 2271-vs-509 `addv.8b` counterexample; **live at `number_simd.rs:54-55` today**). No digit-scan
SIMD (§0/02). No block-scheduling (§0/04). x86 dead (`number_simd.rs:145-235` SSE4.2,
`quote_parity.rs:42-49` pclmulqdq). Cold, paired, N≥200, two windows, opposite arm order; PMU cyc/B
is unavailable in macOS userspace → divan wall-clock ratio is the only certified metric.
Consumer-less substrate forbidden (Y.13). parse-that is READ-ONLY to the bbnf-lang campaign
(non-clobber P23) — every parse-that mutation is a **named** cross-lock supersession, never presumed.

**The one pass-2 convergence to record up front (the shared canonical-producer spec).** 9A and 9B
research **independently reached the same movemask answer**: there is no single canonical producer —
the producer is split by the *downstream consumer's contract*. **LEAN sites** (a single
`trailing_zeros`/count) take the lean nibble-stride `vshrn_n_u16::<4>` (≈4 insns, `pos = tz >> 2`,
zero repack). **DENSE sites** (a mask CTZ-iterated with bit-clear, shift-ORed across four 16-byte
chunks into a u64, or fed to bit arithmetic) take the simdjson **dense-64 `vpaddq` bit-weight tree**
(≈12 insns for all 64 bytes) — **not** 4× (`to_bitmask` + shift + OR), and **not** the 16-byte
full-repack of `bbnf-simd/aarch64/movemask.rs` (≈8 SIMD + ~24 scalar), and **never** the refuted
double-`vaddv`. This spec is common to both central families; the axis decider is orthogonal to it.
What remains **unmeasured** is whether the `vpaddq` tree actually beats 4×(`to_bitmask`+shift+OR) at
a real DENSE classify site, or washes — both families flag this as the owed instruction-count datum.

---

## 9A — ONE-KERNEL-LIBRARY · disposition: **ADVANCED · born-RED · 55%**

**Architecture.** Collapse parse-that's eight `portable_simd` scanners onto the extant,
stable-clean, checkasm-gated `skinny/crates/bbnf-simd` (1226 aarch64 LOC + a parallel scalar
reference tree + a dav1d-style checkasm suite). The isomorphism is *physical*: one source, drift
impossible by construction. bbnf-simd already carries a near-superset of parse-that's scan surface —
~13 of parse-that's ~14 kernel responsibilities have a bbnf-simd counterpart — and hands parse-that
the `vshrn` movemask and the scalar-parity oracle it currently lacks for free. Physical seam is
**one line** in the patch table (path-dep primary, the memchr model; vendor-under-canonical-diff
only if a standalone parse-that build is a hard requirement — the csp-solver precedent).

**Mechanism.** Two consumer tiers, quantified by the pass-2 mapping census of the 8 scanner files
(K = 1:1-to-kernel; V = vector-tier, SIMD fused to parse-that-local arena/enum state; NO_GO):
**K-tier dominant in 5 files** (`structural_bitmap` near-perfect 1:1 → `scan_dispatch` +
`byte_class_from_table_64` + `compact_mask` + `bulk_emit_positions_64`; `ws_comment`; `quoted_simd`;
`quote_parity` classify-half; `scanners` classify-half), **V-tier in 2** (`decode` store_chunk /
owned_decode; `span_parser/scan` enum-dispatch — the honest residue that stays hand-written even
under 9A), **NO_GO in 1** (`number_simd` — digit-scan SIMD is refutation-barred). The classify→mask
→CTZ→emit spine is fully K-tier and already homed; the arena-append surfaces are V-tier.
**Precondition:** the five `prim::` kernels parse-that's hot paths reach
(`bitmap_prefix_xor_64`, `byte_class_from_table_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`eob_pad_clamp`) route through an `OnceLock<PrimitiveKernels>` **fn-pointer facade**
(`dispatch.rs:50-61`; call at `lib.rs:256-258`) — an indirect branch that survives fat-LTO in-loop
and blocks `#[inline]`. Four sibling kernels already dispatch by direct
`#[cfg(target_arch="aarch64")]` (`lib.rs:285-352`) and ICF-fold to the inline body. **9A must
de-face the five to the direct-cfg form before any consumer measures** — the OnceLock-vs-direct-cfg
lever, orthogonal to shared-vs-own (9B inherits the identical constraint intra-crate).

**Canonical-producer finding (the pass-2 crux).** Under bbnf-simd's *current* architecture the
dense-16 repack IS load-bearing: `classify_tbl4::classify_block_from_table` packs four dense-16
masks into a contiguous dense-64 (`result.structural_mask |= u64::from(structural) << (lane*16)`)
and `compact_mask` / `bulk_emit_positions_64` / `bitmap_next_set_bit` CTZ-iterate assuming bit i ↔
byte i — a lean nibble-stride producer (4-bit stride × 64 lanes = 256 bits) cannot compose four
16-byte chunks into one u64. **But** the DENSE-64 `vpaddq` tree (idiom b) obviates *both* the per-16
repack *and* the shift-OR in one reduction — the same fix 9B names. Worse, bbnf-simd is internally
inconsistent: `byte_class_from_eq_set_64.rs:79-87` hand-rolls a **third, refuted** producer (the
double-`vaddv` idiom PROVE-09 killed) — the refuted idiom **living in the shared home** (R-A3). This
pass an asm census (`cargo rustc --release -p bbnf-simd --emit asm`) confirmed both the lean `shrn`
and the double-`vaddv` are live in one crate's codegen simultaneously.

**Integration seams.** Physical: `parse-that/rust/.cargo/config.toml [patch.crates-io]` — add
`bbnf-simd = { path = "../../bbnf-lang/skinny/crates/bbnf-simd" }` (P23 supersession required,
executed owner-side or in a scratch copy). De-facade: `bbnf-simd/src/dispatch.rs:50-61` → mirror the
direct-cfg form of `lib.rs:285-352`. Thin-consumer swaps: 8 files `use std::simd::prelude::*` →
`bbnf_simd::prim::*`; the 17 `to_bitmask` sites → the contract-split producer. Padded-input
reconcile: `state.rs:57,122` (`PaddedView`/`INPUT_PAD_BYTES=64`) ↔ `aarch64/eob_pad_clamp.rs`.
Producer loci to purge: `byte_class_from_eq_set_64.rs:79` (refuted double-`vaddv`).

**Gates it carries.** (1) **Live cross-crate inline** — `otool -tv` a **wired** hot scanner
(`structural_bitmap::classify_stripe`, not `prefix_xor`, not a micro-bench) reached through the
`[patch]` path-dep under fat-LTO: **zero indirect-branch-in-loop** with the de-faced entry, and the
OnceLock-faced entry demonstrably regressing to `blr` in-loop. (2) **Paired no-regression** ≥0.98×
per corpus, 6 JSON + CSS, both windows, N≥200 (PROVE-09 protocol) on the live consumer. (3)
**Movemask instruction-count arm (NEW, born-RED)** — gate must be able to fail on *instruction
weight*, not only reduction-op count: repack-dense-16×4+shift-OR vs dense-64 `vpaddq` tree vs lean
shrn-4bit, disasm'd at a real classify site. (4) checkasm parity over the merged surface. (5)
LOC / path-dep-vs-vendor cost artifact + the mapping-census split fraction.

**Exact open questions.** (Q1, make-or-break, **unmeasured**) Does fat-LTO carry `#[inline]` across
the `[patch]` path-dep on a **live** scanner (not ICF in an isolated 5-fn probe crate — the pass-1
positive was inferred)? This cannot be measured until the physical seam is materialized in a scratch
parse-that; that materialization is the family's mandatory first prototype. (Q2) The
canonical-producer instruction-count (contract-split answered in spec; the vpaddq-tree-vs-4×repack
weight owed). (Q3) `PaddedView`-64 vs `eob_pad_clamp` double-padding at the tail block; granularity
mismatch (64-byte-block kernels vs 16-byte-lane parse-that compositions → the physical iso is
lane-level for those). (Q4, cross-lock) the P23 read-only supersession is **named, not granted** —
9A's one added line mutates parse-that. (Q5, SPEC-ONLY) the `vector/{mod,neon,scalar}.rs` module —
the bulk of 9A's V-tier surface, "the refuted 276-LOC `simd128.rs` promoted minus double-`vaddv`" —
**does not exist**; promoting refuted code demands its own no-regression artifact.

**Why not BLOCKED.** Q1's fix is a known refactor (three sibling kernels already do direct-cfg) plus
a build-config proof — not a new invention of equal difficulty. The family cannot be *declared*
until the live-scanner inline+no-regress runs; born-RED, not blocked.

---

## 9B — NATIVE-REBIRTH · disposition: **ADVANCED · incumbent · 55%**

**Architecture.** parse-that keeps its own kernels, each inner loop rebirthed directly in stable
`core::arch::aarch64` NEON (stable since 1.59 — no nightly), one scalar twin per kernel as the
correctness ground truth, a natively-ported checkasm harness (~200 LOC over the existing divan
dev-dep) proving each NEON body is both *reached* (liveness) and *bit-parity*. **No physical code
shared with bbnf-simd.** The isomorphism to bbnf-lang's kernel home is *behavioral, by discipline* —
same movemask idiom, same checkasm protocol, same scalar-oracle law — never a vendored crate.
Central bet: the "reasonable point" of rust↔rust kernel iso (Constitution iii) is design-level, and
duplicated-small-kernel drift is cheaper to carry than a shared crate's LCD API + sync-gate tax.
**That bet is a null hypothesis to defeat by measurement** — if post-rebirth the kernels are
near-identical, 9B loses to 9A on merit.

**Mechanism — movemask-by-downstream-contract IS the family.** The pass-2 census partitions every
scan site by consumer (all sites verified live). **LEAN contract** (only consumer is one
`trailing_zeros`/count; a 4-bit-stride mask suffices): `number_simd.rs:54-55` (the refuted
double-`vaddv`, rebirth → `cmeq`→`vshrn_n_u16::<4>`→`vget_lane_u64`→`tz >> 2`),
`scanners.rs:208/286`, `span_parser/scan.rs:152`, `ws_comment.rs:150`. **DENSE contract** (mask
CTZ-iterated with bit-clear / shift-ORed into a u64 / fed to `vmull_p64` carry-less multiply):
`quote_parity.rs:81-82` (`classify_stripe_64`→`prefix_xor`), `structural_bitmap.rs:43`,
`decode.rs:82-83/189-190`, `quoted_simd.rs:45-46`, `ws_comment.rs:81/247`. Engineered form: a thin
`simd/{mod,movemask,neon,scalar}.rs` directory-module exposing exactly two lawful producers
(`movemask_nibble`, `movemask_bulk64`) + a `first_match` sugar that **elides the 16-byte repack
bbnf-simd keeps**. This is what a vocabulary-preserving shim structurally cannot do, and it is where
the only honest value beyond stability lives.

**Extant-tranche consciousness (named supersession).** 9B **re-cuts PT-2 W1**, does not contradict
it. PT-2 W1 as written is a *vocabulary-preserving* port (each site swaps one `use` line, keeps the
uniform `to_bitmask` dense semantics, then fixes the movemask impl) — which still owes a repack
wherever `to_bitmask` returns dense-u16 for a LEAN consumer. 9B's amendment: **do not preserve the
uniform vocabulary** — bind each site to its contract (§Mechanism). A strict superset of PT-2 W1's
movemask fix, reached by embracing the call-site churn W1 avoids. PT-2 W2 (smallbox de-nightly) and
W5 (guard rails) inherited unchanged.

**Integration seams.** Rebirth the eight scanners; purge the two live x86 arms
(`number_simd.rs:145-235`, `quote_parity.rs:42-49`) — a pure LOC reduction (~−90/−30) that also
deletes the `is_*_feature_detected!` machinery (NEON is baseline-mandatory). Keep aarch64
`vmull_p64` + the scalar oracles that already exist as `_scalar` tails (`number_simd.rs:239`,
`quote_parity.rs:58-68`, `structural_bitmap.rs:76`). Co-traveler (price of leaving nightly, inherited
not owned): `smallbox[nightly]`→stable `smallbox!` at `parse.rs:74`. `state.rs:57` `PaddedView`
untouched — every rebirth kernel keeps the padded-load-no-tail-guard invariant.

**Gates it carries.** (1) Disasm liveness — `otool -tv` shows `shrn`/`vpaddq`/`tbl` present on the
hot path (parity ≠ liveness). (2) Insn-count no-regress — reduction ops ≤ baseline (2271-vs-509 must
not reproduce). (3) Native checkasm-liveness — alignment sweep 0..63, random + JSON-alphabet
vectors, stack-canary, SIGSEGV/BUS/ILL guard, `INJECT_BUG` toggle flagging ≥90% of cases (must use
`black_box`/`read_volatile` inputs or the liveness check is vacuous), both `vmull_p64`-on and
scalar-fallback arms. (4) Paired divan ≥0.98× every corpus both windows N≥200 (the gate the shim
FAILED). (5) **Gate 5, the axis-decider** — the null-hypothesis LOC + substrate-divergence table
after rebirthing two idiomatically-opposite kernels (position scan idiom a; dense-64 stripe idiom b):
LOC duplicated vs bbnf-simd + substrate axes (`PaddedView`-64 vs `eob_pad_clamp`; `vqtbl1q_u8`
single-table-pair vs `vqtbl4q_u8` four-table) + a shared differential-conformance corpus **as data**.
(6) `cargo +stable build`, `portable_simd` deleted.

**Exact open questions.** (Q1, **the cheapest decisive win, un-landed**) `number_simd.rs:54-55`
double-`vaddv`→nibble-stride `vshrn` — VERIFIED live, a deterministic swap; a single `otool` +
insn-count banks 9B off SPEC-ONLY and removes a live refutation-law violation. Asserted pass-1,
never demonstrated. (Q2, self-regression) the dense-64 quote-parity site: naive nibble-stride `vshrn`
on `classify_stripe_64` yields 256 bits and corrupts parity → it MUST use idiom (b); the one place
9B can self-inflict the refuted defect is the one place unmeasured. (Q3, gate 5, **unmeasurable this
pass** because nothing was rebirthed) is post-rebirth LOC/substrate divergence large enough to
justify two homes, or near-identical (conceding to 9A)? Research argues the shareable *core* is ~4
instructions while the whole substrate diverges — an argument, not the rebirth measurement. (Q4)
is 9B's `first_match` movemask materially leaner than bbnf-simd's repack (cuts against 9A-gap-2 only
when disasm'd)? (Q5) `smallbox` de-nightly blast radius asserted bounded, not shown.

**Why not BLOCKED.** Every intrinsic is stable since 1.59; the harness exists to port; the taxonomy
is settled art. Its only failure mode is *losing to 9A on gate 5*, not a missing primitive.

---

## 9C — SCANNER-AS-DFA · disposition: **BLOCKED · 44%**

**Architecture.** The scanners are not primitives — each is a finite-state acceptor a regex leaf in
the bootstrapped grammar already denotes. 9C dissolves the hand corpus: acceptors authored once in
`bootstrap/regex.bbnf`, compiled through the existing HIR→NFA→DFA→Hopcroft→`byte_classes` pipeline,
lowered to a NEON kernel as a *compiled product*. "Scanner" becomes a codegen tier, not a source
file. The **derivation** (grammar leaf → DFA → byte-class + transition tables → kernel template) is
the architecture; the kernel is downstream.

**Mechanism — the pass-2 sharpening: a DFA is not one thing.** The eight scanners are DFAs of THREE
distinct shapes, each with a different optimal execution — a **tiered lowering keyed on the compiled
DFA's structure, not a single Sheng runner**:

- **Tier M (membership; 1–2 states, single accepting char-class).** The dominant every-byte pass.
  The DFA's whole computation is the byte→class map; no loop-carried state. Lowers to *exactly* the
  simdjson classify-and-movemask kernel — **because the hand LUTs already are hand-computed
  `byte_classes` tables** (`WS_SLASH_LO_LUT`/`HI_LUT`, `ws_comment.rs:40-62`). **Parity is by
  construction (Q1 answered).** The non-trivial content 9C must SOLVE is *which classifier producer*
  the partition admits — a **separability question**: (a) nibble-separable → simdjson lo×hi two-LUT;
  (b) lo6-separable → `vqtbl4q_u8` lo6-table; (c) lo6-colliding (the 11-byte structural set,
  `;`=0x3B ≡ `{`=0x3B) → `vceqq_u8` equality fan. A naive nibble-LUT is **wrong** for (c).
- **Tier S (small multistate; ≤16 states, real transitions).** The Sheng question. **Q3 answered:
  YES, the transition stalls per byte** — `state[i] = tbl(T, class[i], state[i-1])` is a loop-carried
  serial chain of one `vqtbl1q_u8` (~2–4 cyc latency, unhidable) → latency-bound at ~2–4 cyc/byte
  with no cross-byte parallelism, vs Tier M's ~0.1–0.3 cyc/byte (a `[a-z]+`×1000 counterexample:
  Sheng ~3000 cyc vs membership ~250 cyc, ~12×). For the SHORT runs these DFAs actually see
  (`ws_comment`: 84–100% of WS runs ≤4 bytes), the scalar `find_at` (`dfa.rs:116`, already the scalar
  Sheng) wins. **Sheng is a MEASURED NEGATIVE for the hot path** — retained only as a
  measured-against baseline. This retires the pass-1 "genuine novum" (vectorized Sheng DFA).
- **Tier X (explosion / transduction; NO_GO).** `\p{…}` unicode class (`regex.bbnf:32`) compiles to
  a byte-DFA whose `num_classes` can exceed 255 (breaking the `[u8;256]`/u8 class-id type in
  `equiv.rs`) and states exceed 4096 → `dfa.rs:262` returns `None`. This is the **correctness floor,
  not a scanner** — the census EXCLUDES Tier X. JSON `\uXXXX` decode is a transducer, host-emitted.

**The core gap (R6).** The `[u8;256] → SIMD-classifier` emit pass **does not exist**: `equiv.rs`
emits a scalar `[u8;256]` only; the separability-solved
`byte_classes → (lo_lut,hi_lut) ∨ lo6-table ∨ vceqq-fan` derivation is the family's new code, and its
correctness hinges on the separability solver (a bounded CSP over a 256-entry partition selecting
among three known forms — tractable, but unbuilt).

**Integration seams.** DFA payload: `parse-that/rust/regex/src/automata/dfa.rs`
(`byte_classes:52`, `num_classes:54`, `flat_transitions:50`, `find_at:116`). Class-map producer:
`regex/src/sets/equiv.rs:17 compute_byte_classes`. Template selection:
`regex/src/classify.rs:55 RegexClass` (extended from "pick a hand scanner" to "pick a lowering
tier"). Backend: `bbnf-simd/aarch64/classify_tbl4.rs`, `structural_classify_64.rs`, `movemask.rs`.
Grammar source: `bootstrap/regex.bbnf`. Codegen home: post-GN-1 `crates/codegen/src/emit/regex/`.

**Gates it carries.** (1) Membership parity (Q1) — compile `(?:\s|/)` and the 11-byte structural set
to `byte_classes` via `equiv.rs`, emit through the separability solver, `otool -tv` the generated
NEON vs hand `classify_ws_slash_16`/`structural_classify_64` at ≤1.0× reduction-instruction count +
paired ≥0.98×. (2) Sheng-stall (Q3) — a `vqtbl1q_u8` Sheng runner for
`WhitespaceWithBlockComment` benchmarked vs scalar `find_at`; expected Sheng ≥ scalar, both ≫ Tier-M
membership (confirms the negative). (3) Census + coverage % — every `RegexClass` leaf sourced from
`regex.bbnf`, `{num_states, num_classes, tier, state_limit-bail}`, **including `\p{L}+`/`\u{…}` as
explosion rows** → a measured percentage, not the pass-1 fit-chosen sample. (4) Separability solver
load-bearing proof — a naive nibble-only emit MUST misclassify the structural set.

**Exact open questions & why BLOCKED.** (Q1, the family-killer, confirmed) dense-classification is
**at best parity** with hand simdjson on the dominant pass — 9C's value is deletion +
grammar-specialization + stable-by-construction + free `vshrn`, real but modest. (Q2, cross-lock,
**the actual blocker**) the DFA source-of-truth is contested between **TWO** crates — parse-that's
in-repo `bbnf-regex = { path = "regex" }` (the crate parse-that actually links; full pipeline present
today) and bbnf-lang's `crates/bbnf-regex` (LOCK-5's home) — potentially two crates to reconcile;
the byte-fixpoint-gated grammar→DFA→table source-of-truth at scanner fidelity is **owned by LOCK-5
and unbuilt**. This is the missing-primitive test: substrate owned elsewhere and unbuilt. (Q3,
cross-lock) tier-selection is a LOCK-2 scan-plan e-graph island; the codegen home
`crates/codegen/src/emit/regex/` is a **named GN-codex supersession** (constitution ix), not an
implicit LOCK-9 land-grab. 9C is quad-homed (LOCK-5 + LOCK-2 + LOCK-9 backend + GN codegen) and
cannot be *selected* until LOCK-5 + LOCK-2 land. Survives degraded (Tier M + prefilter Tiers 0/1/3,
CONFIRMED art) even if the rest fails — a partial that must be named, not hidden.

---

## 9D — LAZY-SURFACE · disposition: **BLOCKED · 30%**

**Architecture.** Streaming-first: scanners as incremental lazy planes over input windows.
Partiality-as-witness (the winnow `Partial<I>` model, NOT nom's forked streaming/complete APIs which
violate the one-path edict): a `Window` witness carrying (a) the ≤64-byte block view, (b) threaded
inter-block scanner carries, (c) a `final: bool` flag. **The eager whole-input parse is the
degenerate one-window case** — one code path; streaming is the general case. Pass-2 separates two
capabilities the charter conflates: **M1 (windowed scan over a *resident* buffer)** — already exists
(`ws_bitmap`/`ws_bitmap_start` at `state.rs:228`; `NibbleBitmapIter` stripe cursor; compile-time
kernel selection via `classify_regex`), delivers no streaming/RSS value on its own; **M2 (streaming
*input*, `feed(chunk)`, never holding the whole document)** — the actual design center, and it **does
not exist in any form**.

**Mechanism.** The carries ARE the streaming vocabulary, and bbnf-simd already speaks it:
`prefix_xor_64(mask, carry_in)`, `escape_mask_64(bs_mask, bs_carry_in) -> (u64,bool)`,
`CommentCarry`, `bracket_depth_mask_64(..., depth) -> (u64,i32)` — the *resumable* form of what
parse-that's `PaddedView` scanners compute whole-buffer. 9D refactors the scanners onto that
carry-threaded window form. Compile-time `KernelPlan` at `Parser::new` (folds PROVE-10's runtime
prefilter + bbnf-simd's OnceLock runtime dispatch into a compile-time-selected plan) — **LOCK-2's
decision-driver territory** (the same island 9C's tier-selection needs).

**The two facts that govern viability (both adverse this pass).** (1) **Consumer — MEASURED
NEGATIVE.** An exhaustive grep over `parse_that/src`
(`fn feed|fn push_bytes|Decoder|BufRead|document_stream|from_reader|next_document|iter_documents`)
returns zero streaming entry points; the entire public surface is whole-buffer
(`Parser::parse(&self, src: &'a str)`), and `ParserState::new` *immediately materializes a full
padded second copy* (`allocate_padded_buf`, `state.rs:100`). The three shipped grammars build whole
**owned** trees (twitter.json = 5390 allocs, the RSS mass, built eagerly). `lazy()` (`lazy.rs:49`) is
a false friend (defers *parser construction* for recursion, not input). **Y.13 is tripped: a
streaming input surface has no in-wave consumer.** To be lawful 9D MUST co-deliver an
ndjson/concatenated-value document iterator (simdjson `document_stream` granularity — whole
documents, never a byte-partial document). (2) **Backtrack bound — structurally unbounded, and the
cheap decider STILL DID NOT RUN.** parse-that is a backtracking PEG; PEG gives NO bounded-backtrack
guarantee (Ford 2004: `("a"* "b")|("a"* "c")` rewinds O(input)); the shipped JSON/CSS grammars are
near-LL(1) and prefix-disambiguated, so the *measured* distribution is *expected* small — but
research-D delivered only the **instrumentation spec**, not the histogram. Rewind is structurally
unbounded, empirically small on shipped corpora; a window sized to a measured max is
corpus-conditional, not semantic.

**Integration seams.** `state.rs:57/82/122/285` (the whole-input allocation to generalize),
`state.rs:228` (the `ws_bitmap` seam to lift), the three restore sites for the P3 probe
(`combinators/methods/alternation.rs:23`, `parse.rs:117`, `combinators/macros.rs:91-117`),
`lazy.rs:20/49` (**name-collision to disambiguate** — combinator-graph laziness for recursion is a
different axis from input-window laziness), the bbnf-simd carry kernels, PROVE-10
`automata/prefilter.rs`. Best refactor candidate: `structural_bitmap.rs::classify_stripe` (already
stripe-at-a-time with an implicit whole-buffer carry).

**Gates it carries.** P3 (mandatory, cheap, deterministic) — thread-local histogram of
`furthest_offset − offset` at the three restore sites, over 6 JSON + 3 CSS corpora, `--features
diagnostics` on and off; report P50/P99/max; decision: P99 < 64 B (one stripe) ⇒ a single-stripe
window is corpus-sound; unbounded tail ⇒ M2 is unsound without full retention (file the measured
refutation). P2 — the minimal lawful streaming consumer (ndjson document iterator) + peak-RSS +
time-to-first-structural, paired, **no throughput claim** (the straddle guard is neutral-to-slower).
P1 — one scanner windowed, paired ≥0.98× (>2% regression = the straddle guard is real and priced).
P4 — a `Partial`/`Complete` input-type distinction ICF-folds the whole-buffer case back to today's
resident scan (`otool` no residual straddle branch), so the non-streaming 99% path pays zero.

**Exact open questions & why BLOCKED.** (Q1, the killer) unbounded rewind defeats the memory claim —
if rewind is unbounded, the ring buffer retains all input = whole-input with a lazy tail, **zero
peak-RSS win**. The memory center survives ONLY if P3 measures a bounded window AND the grammar class
is frozen (corpus-conditional). This is the missing-primitive test in its exact form: **a bounded
backtrack guarantee PEG semantics may simply not provide — a property equal in difficulty to
redesigning the backtracking discipline itself.** (Q2, Y.13) no streaming consumer named; the output
tree dominates RSS anyway (5390 allocs swamp the one padded-copy a windowed input removes), so RSS
value requires a streaming *output* consumer, not a streaming input. (Q3) 9D must make **zero
throughput claim**; latency/TTFS is the only survivable value, and even that is gated on the
consumer. Marked BLOCKED: refuted-by-default until P3 measures bounded backtrack AND a real consumer
materializes. Either outcome is a valid close with an artifact (P3 is cheap).

---

## THE INCOMPATIBILITY MAP — which families cannot coexist, and precisely why

**9A ⊗ 9B — HARD-INCOMPATIBLE (kernel-home axis). THE lock's central adjudication.** A kernel is
either shared-source (9A) or own-source (9B); it cannot be both. **Pass-2 sharpened the
convergence-in-mechanism:** both routes independently reach the identical movemask contract-split
(lean `vshrn` for LEAN sites, dense-64 `vpaddq` tree for DENSE sites, purge the 16-byte repack and
the double-`vaddv`) and the identical checkasm discipline — they differ only in whether one file or
two files carry them. But the **crowding-redirect rule (bank the redundant one) does NOT apply**,
because the redundancy is not *shown*: the decider was unrun on both sides —
(i) 9A's live cross-crate inline + no-regress on a real hot scanner (unmeasured; cannot be measured
without materializing the seam), and (ii) 9B's gate-5 rebirth LOC/substrate divergence (unmeasurable
this pass — nothing was rebirthed). Both measurements are **LOCK-6's inputs**: the kernel-home
decider is "the reasonable point of rust↔rust isomorphism" (Constitution iii) resolved by "null
hypothesis defeated by measured drift/maintenance cost" (LOCK-6D verbatim). **LOCK-9 produces the two
measurements; LOCK-6 renders the verdict. Neither family may be banked without evidence; 9B holds as
incumbent-default (PT-2 frozen codex), 9A must earn its place by producing the two datums.**

**9C ⊗ 9B — INCOMPATIBLE (scanner-provenance axis).** 9C deletes parse-that's hand kernels and emits
them as compiled DFA products; 9B's entire thesis is that parse-that *authors and hand-rebirths* its
kernels. A scanner cannot be simultaneously hand-authored-in-`core::arch` (9B) and
non-existent-because-generated (9C). Cannot coexist as organizing principle (they co-occur only in
the degenerate sense of a 9C Tier-miss leaf falling back to a hand kernel — 9C conceding coverage,
not 9B being adopted).

**9C ◐ 9A — PARTIALLY COMPATIBLE / SEQUENCEABLE (agree on kernel home, disagree on provenance) → a
DAG relationship, not a contradiction.** Both converge on `bbnf-simd` as the kernel backend: 9A by
extraction, 9C by lowering-to-it through the DFA. A gestalt can hold both as ordered phases
(kernel-home unification → scanner-derivation), NOT a single-wave contradiction. Note the
prefilter double-home the gestalt must collapse: `bbnf-simd::find_ascii_set_member64` ⟷
`bbnf-regex::Dfa::find` are two entry points to one prefilter idea (GN-2/PT-1 dispatch law: one
lowering per scan site, never both).

**9D ∥ {9A, 9B, 9C} — ORTHOGONAL AXIS (input surface), preconditioned on generalizing `PaddedView`.**
9D changes *what feeds* the kernels, not *who owns* or *how derived* them; its `Window` witness could
thread carries into 9A's shared kernels, 9B's native kernels, or 9C's compiled products. The one
contradiction: all three silently assume the eager `PaddedView`/`INPUT_PAD_BYTES=64` witness that
elides the per-block tail guard; 9D must generalize or dissolve it, and its honest cost is exactly
that tail guard. Because 9D is BLOCKED on its own memory center, it is a *later, conditional* overlay
on whichever kernel-home/provenance verdict wins.

**9C ⊗ 9D — strong positive affinity, RE-HOMED by pass-2.** Ragel/re2c lineage: a streaming scanner
IS a resumable DFA. Pass-1 named 9C's Sheng DFA as the natural body of 9D's windowed carry-threaded
scanner. **Pass-2 corrects the basis:** Sheng is a measured NEGATIVE, so 9C collapses to a
*stateless* Tier-M membership classify + Tier-3 structural — 9D therefore threads carries through the
**Tier-3/structural kernels** (`prefix_xor` / comment / bracket-depth carries bbnf-simd already
exports), not through a saved Sheng state. The "compiled resumable scanner" gestalt-Ω consolidation
candidate survives, but on the structural-carry basis. Neither 9C nor 9D is independently
converge-able as a separate LOCK-9 family — the gestalt-Ω carries both as inputs to ONE compiled
resumable scanner (a mild double-count the roll-up acknowledges).

**Summary matrix** (⊗ hard-incompatible · ◐ partial/sequenceable · ∥ orthogonal-composable):

|        | 9A            | 9B            | 9C            | 9D            |
|--------|---------------|---------------|---------------|---------------|
| **9A** | —             | ⊗ (kernel home)| ◐ (agree home, differ provenance; DAG) | ∥ (needs PaddedView generalized) |
| **9B** | ⊗             | —             | ⊗ (provenance)| ∥ (needs PaddedView generalized) |
| **9C** | ◐             | ⊗             | —             | ∥ (9C stateless classify + carries = 9D resumable scanner; strong, re-homed) |
| **9D** | ∥             | ∥             | ∥             | —             |

---

## CROSS-LOCK DAG ORDERING (blockers LOCK-9 CANNOT retire alone)

- **9A ⊗ 9B → LOCK-6.** LOCK-9 produces the two measurements (9A vector-tier inline+no-regress on a
  live scanner; 9B gate-5 divergence); LOCK-6 renders the home verdict. They co-converge in GESTALT;
  neither closes the axis alone.
- **Canonical movemask form → LOCK-9 ∩ LOCK-5.** The triple-home (bbnf-simd repack / PROVE-10 lean /
  parse-that `to_bitmask`) collapses to ONE lawful producer; memchr's lean form lives in
  bbnf-regex's prefilter (LOCK-5's crate). LOCK-9 runs the repack-vs-lean-vs-`vpaddq`-tree
  instruction-count; the gestalt collapses the home.
- **9C grammar→DFA source-of-truth → LOCK-5** (unbuilt substrate, two-regex-crate reconciliation);
  **9C tier-selection → LOCK-2** (scan-plan e-graph island); **9C codegen home → a named GN-codex
  supersession**. 9C cannot be *selected* until LOCK-5 + LOCK-2 land.
- **9D `KernelPlan` → LOCK-2** (compile-time decision driver, the same island 9C needs). Not a new
  LOCK-9 subsystem — a consumer of LOCK-2.
- **Un-homed consumer edge → LOCK-4 / LOCK-8.** The reborn scanner surface feeds the combinator core
  (LOCK-4) and, via LOCK-6, the TS backbone (LOCK-8); no 9-family names how it presents to LOCK-4's
  parser graph or whether LOCK-8's TS side mirrors 9C's compiled-DFA products. An un-homed edge the
  gestalt-P weave must close.

---

## Disposition roll-up for the REGISTRY

- **9A ONE-KERNEL-LIBRARY — ADVANCED, born-RED (55%).** Convergence blocker: the live cross-crate
  inline + no-regression proof on a real hot scanner (Q1) — inferred in pass 1 from ICF in an
  isolated probe crate, never measured live. The canonical-producer spec is answered (contract-split);
  its instruction-count is owed.
- **9B NATIVE-REBIRTH — ADVANCED, incumbent (55%).** No missing primitive; cleanest execution. Its
  close is the null-hypothesis gate-5 measurement (does it concede to 9A). The `number_simd.rs:54-55`
  double-`vaddv`→`vshrn` swap is its cheapest standalone win, un-landed.
- **9C SCANNER-AS-DFA — BLOCKED (44%).** Cross-lock-coupled to LOCK-5 (grammar→DFA source-of-truth,
  unbuilt, two-crate reconciliation) + LOCK-2 (tier island) + a GN-codex supersession; dominant dense
  pass is parity+deletion, not speed; Sheng (the pass-1 novum) is a measured negative. Survives
  degraded (Tier M + prefilter).
- **9D LAZY-SURFACE — BLOCKED (30%).** Memory raison d'être refuted-by-default: consumer measured
  absent (Y.13 tripped), backtrack bound structurally unbounded, P3 still unrun. Latency/TTFS is the
  only survivable value and is itself consumer-gated. A cheap, deterministic close is one prototype
  away.

---

## THE TWO SHARPEST UNCERTAINTIES → pass-3 prototype briefs

Both live on the lock's central axis (9A ⊗ 9B), because 9C and 9D are BLOCKED on cross-lock
substrate their internal resolution cannot unblock — the highest-advance uncertainties are the two
axis-deciders that neither pass-1 probe touched.

**BRIEF 1 — 9A live cross-crate inline + no-regression on a real hot scanner (pt-scratch).** The
axis-half nobody measured. In a scratch copy of `/Users/mkbabb/Programming/parse-that` (P23:
scratch-only, no owner-tree mutation): (1) in `bbnf-simd/src/dispatch.rs` de-face the five `OnceLock`
`prim::` kernels to `#[cfg(target_arch="aarch64")]` direct entries, mirroring `lib.rs:285-352`. (2)
Add `bbnf-simd = { path = "../../bbnf-lang/skinny/crates/bbnf-simd" }` to
`parse-that/rust/.cargo/config.toml [patch.crates-io]`. (3) Rewrite `parse_that/src/parsers/scan/
structural_bitmap.rs::classify_stripe` to consume `bbnf_simd::{scan_dispatch, byte_class_from_table_64,
compact_mask}`. (4) `cargo build --release` with `[profile.release] lto="fat"`, `codegen-units=1`.
(5) `otool -tv` the wired `classify_stripe` hot loop; (6) PROVE-09 paired cold divan (N≥200, two
windows opposite arm order) via `micro_parse_that` over `data/json/{apache-builds,canada,twitter,
citm_catalog}.json`. **PASS/FAIL:** zero `bl`/`blr` to the primitive inside the `classify_stripe`
block loop AND patched ratio ≥0.98× on every corpus. FAIL if the de-faced entry still emits an
in-loop indirect branch, or any corpus regresses <0.98×. This converts 9A from born-RED-inferred to
measured and delivers LOCK-6's 9A-side datum.

**BRIEF 2 — canonical movemask producer + 9B LEAN/DENSE rebirth (pt-scratch).** Settles the shared
spec both central families carry, banks 9B's cheapest win, and yields 9B gate-5's first datum. In a
scratch parse-that: (1) rebirth `number_simd.rs:54-55` (LEAN) double-`vaddv` →
`cmeq`→`vshrn_n_u16::<4>`→`vget_lane_u64`→`tz>>2`, deleting the x86 arm `:145-235`. (2) Rebirth
`quote_parity.rs::classify_stripe_64` (DENSE) → the simdjson dense-64 `vpaddq` bit-weight tree. (3)
`otool -tv` all producer forms — the two rebirths, `bbnf-simd/aarch64/movemask.rs` repack, and the
refuted `byte_class_from_eq_set_64.rs:79-87` double-`vaddv` — and count *total instructions to a
usable mask* per contract. (4) Paired cold divan (`micro_parse_that` number lane +
`parse_that_combinator`, N≥200, two windows) vs the pristine arm. (5) Count LOC duplicated vs the
bbnf-simd equivalents. **PASS/FAIL:** number scanner shows `shrn.8b …,#4` with **zero `vaddv`** and
insn-count ≤ baseline (the 2271-vs-509 defect must not reproduce); the DENSE `vpaddq` tree reported
as win / wash / loss vs 4×(`to_bitmask`+shift+OR) with its instruction count; both rebirths paired
≥0.98×; LOC-duplicated table emitted. A DENSE loss is a valid negative ("keep `to_bitmask` for
DENSE, rebirth only LEAN"). This produces the canonical-producer measurement (LOCK-9 ∩ LOCK-5 input)
and LOCK-6's 9B-side gate-5 seed in one probe.
