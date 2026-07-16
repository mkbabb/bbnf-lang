# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) · pass-3 FAMILY-SPECS

*Synthesis, pass 3 (Fable-tier judgment). One cogent specification per family while the routes
remain incompatible, then the incompatibility map. Elegance earns nothing on its own; a family
stalled at a missing primitive equal in difficulty to the original problem is marked
BLOCKED-candidate. Folds research-{A,B,C,D,priorart} pass-3 against the pass-2 registry (52/62/43/30;
PROTO-1 live cross-crate inline; PROTO-2 LEAN/DENSE rebirth) and the six reconciliations the
registry owed. Substrate paths:lines re-anchored against the live trees. CROSS-LOCK ledger
(XL-C, XL-H) conveyed as binding fact.*

The lock is **four dispositions of one aarch64 scanner/kernel plane on three orthogonal axes**:
**kernel-home** (9A shared crate ⊗ 9B own — the central adjudication), **scanner-provenance** (9C
DFA-derived vs {9A,9B} authored), **input-surface** (9D windowed vs whole-input — orthogonal, and
its streaming half contradicts the eager `PaddedView` invariant all three silently share).

## THE PASS-2 RECONCILIATION, ABSORBED (the six stale claims the FAMILY-SPECS carried and must now retract)

The lock's single most consequential result — **the 9A⊗9B axis is a PERFORMANCE WASH** — is now
folded into every spec below. Its six components, settled by the pass-2 probes and re-confirmed by
pass-3 research, are conveyed here as facts, not re-litigated:

1. **The axis is a performance wash.** Both routes converge on the identical movemask contract-split
   and checkasm discipline; the decider is no longer "which is faster" but a pure **LOCK-6
   maintenance-cost call** ("one crate or two for identical new code").
2. **The OnceLock facade is free at production `codegen-units=1`.** PROTO-1's CONTROL emitted
   byte-identical, `blr`-free code; LLVM devirtualized the facade entirely; the standalone object
   carried no `OnceLock`/`primitive_kernels` symbol. The pass-1 "0.92× barrier" was a
   probe-crate/multi-CGU artifact. **De-facing buys inline-PARITY, not a win.**
3. **The "free `vshrn` movemask" is refuted.** The two *lawful* producers (LEAN nibble-`vshrn`,
   DENSE `vpaddq` tree) exist in bbnf-simd **nowhere** (`vpaddq` in zero files); a **refuted**
   producer (double-`vaddv`) lives there in triplicate. Adopting 9A is "**author** the canonical
   producers into bbnf-simd and **purge** the refuted idiom," never "inherit good kernels for free."
4. **9B does NOT inherit the OnceLock constraint.** parse-that's scan files carry zero
   `OnceLock`/`primitive_kernels`; the facade is bbnf-simd-internal (`dispatch.rs:58-60`). **The
   de-face is a 9A-ONLY cost** — it counts *against* 9A on the axis.
5. **The regex-crate collision path is `skinny/crates/bbnf-regex`** (pass-1's `crates/bbnf-regex` was
   wrong and does not exist). It and parse-that's `rust/regex` fork are **both `bbnf-regex 0.1.0`** —
   a name+version collision resolving to two sources per build graph.
6. **Tier-M "parity by construction (Q1 answered)" downgrades to SPEC-ONLY** — the emit pass and the
   separability solver whose output that parity is asserted of **do not exist**.

**Binding laws carried by every family's gates (SPEC §0).** No stable-port perf claim — de-nightly
is STABILITY only; the no-regression gate may FAIL (the shim failed window A 0.81–0.98×); "faster"
appears in no gate. `vshrn` narrow, never double-`vaddv` (the 2271-vs-509 `addv.8b` counterexample,
live at `number_simd.rs:54-55`). No digit-scan SIMD (§0/02). No block-scheduling (§0/04). x86 dead.
Cold, paired, N≥200, two windows, opposite arm order; PMU cyc/B unavailable in macOS userspace →
divan wall-clock ratio is the only certified metric; static disasm insn-count is host-noise-immune.
Consumer-less substrate forbidden (Y.13). parse-that READ-ONLY to the bbnf campaign (P23) — every
mutation is a **named** cross-lock supersession, base-pinned (XL-H).

---

## 9A — ONE-KERNEL-LIBRARY · **ADVANCED · born-RED · 52%**

**Architecture.** Collapse parse-that's eight `portable_simd` scanners onto the stable-clean,
checkasm-gated `skinny/crates/bbnf-simd`; isomorphism is *physical* — one source, drift impossible by
construction. **The physical seam already exists**: `crates/parse-that-regex/Cargo.toml:10` already
consumes bbnf-simd under the skinny→parse-that vendor edge — a shared kernel need not be a new crate.
The value proposition, honestly restated after the wash: **maintenance/drift-avoidance ONLY, not a
perf win.** The drift it avoids is *already realized* — bbnf-simd today carries **three divergent
`movemask_u8x16` definitions, no two alike, two of them the refuted double-`vaddv`**
(`movemask.rs:19-24` scalar-tail repack; `byte_class_from_eq_set_64.rs:79-86`;
`comment_body_mask_64.rs:68-74`) — the drift-cost 9A pays down and 9B would compound across two homes.

**Mechanism — author the canonical producer once, purge the refuted idioms, then the residual is
small.** The load-bearing act is one authored contract-split producer pair, TYPE-DISTINCT so a
consumer cannot cross wires:
- **LEAN** `movemask_nibble_64(x0..x3) -> NibbleMask64`: per 16-byte lane
  `vshrn_n_u16::<4>(vreinterpretq_u16_u8(cmp))` → 8 bytes at 4-bit stride; four lanes → one 64-bit
  word; presence/first-match reads `tz >> 2`. 1 `vshrn` per 16 lanes, zero horizontal reduction.
- **DENSE** `movemask_dense_64(x0..x3) -> u64`: true 1-bit-per-byte for CTZ-*iterating* consumers,
  via `vandq` bit-position + a **`vpaddq_u8` log-reduction tree**, NEVER a per-half `vaddv`.

Purge the three divergent movemasks; author real NEON bodies for the four **scalar-stub "NEON"
primitives** (`byte_class_from_table_64.rs:2-4` delegates to scalar; likewise
`bitmap_next_set_bit`, `bitmap_prefix_xor_64`, `bulk_emit_positions_64`, `eob_pad_clamp`) — an
existing Y.13 hazard (advertised-but-dormant vector surface) the shared home must not perpetuate.
**Remove the `OnceLock` fn-ptr table** (`dispatch.rs:58-87`): since parse-that is aarch64-only
post-purge it is unnecessary; `#[cfg]`-static keeps the producers inline-clean into both consumers.
This is a concrete one-home simplification, not merely hygiene.

**Scanner-file census (F1, owed — delivered).** **4 kernel-tier** (`structural_bitmap`,
`ws_comment`, `quoted_simd`, `quote_parity`) · **3 vector-tier** (`scanners`, `decode` — `store_chunk`
stays domain-local, `span_parser/scan`) · **1 no-go** (`number_simd` — digit-scan SIMD, §0/02
refuted; purge its x86 arm, never enters the shared home). `state.rs` is substrate (PaddedView),
not a scanner.

**#[inline] across the seam (F1).** Cross-crate `#[inline]` across the rlib boundary is measured-live
(0 in-loop indirect). The only place a non-inlinable boundary *actually* appears is the `OnceLock`
fn-ptr table — a deliberate runtime-select indirect, uninlinable at any cgu/LTO. Guard: the LEAN/DENSE
producers are tiny and `#[inline]` → they always cross. Removing the table (aarch64-only) deletes the
one real seam-inline failure.

**Gates it carries (born-RED).** (1) **[BLOCKING] R-A3 purge** — the refuted double-`vaddv` in the
shared home is a §0/iv violation 9A imports unless purged; the no-regression proof is a *blocking*
born-RED gate, not advisory. (2) **Evolved-home producer insn-count** — LEAN `shrn` ≤ the 509-`addv`
portable baseline; DENSE a log2 `vpaddq` tree, zero double-`vaddv`, base-pinned. (3) **Live path-dep
inline + no-regress on the CORRECT surface** — `parse_that_css → scan_ws_block_comments →
classify_stripe` (JSON is a null: `parsers/json.rs` has zero `classify` refs), paired cold N≥200 two
windows ≥0.98×, reached through the **one-line `[patch]` path-dep** (not the vendor fallback PROTO-1
tested). (4) checkasm parity over the merged surface (bbnf-simd's 14-test suite is the ready home).
(5) Evolved-home divergence LOC delta.

**Exact open questions.** (Q1) The perf half remains zero-measured for a 2nd pass and was mis-surfaced
on CSS-only `classify_stripe` while the JSON corpus never ran it. (Q2) The one-line path-dep seam is
untested — the heavier vendor route was exercised. (Q3, cross-lock) P23 supersession named-not-granted;
the axis close is LOCK-6's. (Q4, SPEC-ONLY) the `vector/{mod,neon,scalar}.rs` module does not exist.

**Why not BLOCKED.** The physical inline promise is measured-GREEN live; no missing primitive of equal
difficulty — authoring the producers and removing the table are known refactors. The value is now
maintenance-only; 9A must first *evolve + clean* the shared crate before it inherits anything.

---

## 9B — NATIVE-REBIRTH · **ADVANCED · incumbent · 62%** (the only family off SPEC-ONLY)

**Architecture.** parse-that keeps its own kernels in a colocated `parse_that/src/simd/` directory
module — `mod.rs (~40)` public `u8x16`/`mask8x16` vocabulary + `#[cfg]` pivot; `neon.rs (~210)`
`core::arch::aarch64`; `scalar.rs (~130)` the `[u8;16]` parity oracle that IS the executable
specification. No physical code shared with bbnf-simd; the isomorphism is **behavioral, by
discipline** — same `vshrn` idiom, same checkasm protocol, same scalar-oracle law. Central bet: the
"reasonable point" of rust↔rust kernel iso (Constitution iii) is design-level, and duplicated-small-
kernel drift is cheaper to carry than a shared crate's LCD API + sync-gate tax. **That bet is a null
hypothesis to defeat by measurement.**

**Mechanism — movemask-by-downstream-contract IS the family (banked).** PROTO-2 banked both producers:
LEAN `number_simd.rs:54-55` double-`vaddv` → `vshrn_n_u16::<4>` (`shrn.8b`, **zero `vaddv`, 14 vs 19
insns**); DENSE `classify_stripe_64` → `vpaddq` bit-weight tree (**50 vs 64 insns, zero reduction
ops**, LLVM lowering the disjoint-bit tree to `uzp1/uzp2/orr`); both paired ≥0.98× (15/16). Iterating
consumers migrate `tz → tz >> 2`; presence-only lowers to `vmaxvq_u8`. This is bounded call-site churn
(~+80/−40 at the ~5 CTZ sites) — the honest cost the zero-churn shim traded away for a regressing tree.

**The x86 purge and the digit-scan guard.** Two live x86 arms excised: `number_simd.rs:145-235` (SSE4.2,
−~90) and `quote_parity.rs:42-49` (`pclmulqdq`, −~30); `decode.rs` x86 tokens are doc comments — scrub.
`number_simd` is **ported, never optimized** (§0/02 NO_GO): its `vaddv_u8` masked-half digit-sum
(`:54-55`) is a *legitimate horizontal-sum accumulate*, distinct from the movemask double-`addv`
anti-pattern — do not pursue Mula/Lemire in-vector accumulation (CPI 0.13→0.30 inversion).

**The stable cutover — the sole cross-repo residual is at the generator, not parse-that source.** Four
nightly enablers: `portable_simd` (→ `simd/`), `smallbox[nightly]` (one field + one construct site,
alloc-neutral: 6 construct / 5390 twitter unchanged), `stmt_expr_attributes` (dead — delete), and
**`fmt_helpers_for_derive`** — forced NOT by parse-that but by `@generated regex/generated.rs` emitting
the *expanded* `#[derive(Debug)]`. Fix at the **generator** (emit the attribute, not its expansion) +
regen; generated.rs net shrinks. On stable, all ~20 errors localize to generated.rs; the hand-ported
files carry zero.

**The differential oracle (transliterated from bbnf-simd checkasm, ~190 LOC, self-contained).**
`Xorshift64` + `guarded_call` (1 KiB stack canary, `black_box`-fenced), `callee_saved_register_then`
(x19–x28 sentinels via `asm!`), and the per-kernel differential test: density sweep
`[0,1,2,4,8,16,32,64,128,255,256]` × alignment sweep `for align in 0..64` × random fills, each
asserting `guarded_call(neon) == scalar_reference`. **INJECT_BUG**: a committed `#[should_panic]`
lane-flip fixture proving the sweep is not vacuous.

**Two-home basis (the maintenance argument, answered as data).** Even after authoring both producers
into a scratch shared home, the residual divergence is irreducible: **block width** (PT `u8x16`→u16
masks vs bbnf-simd fixed `[u8;64]`→u64), **pad/clamp** (`PaddedView` zero-copy vs `eob_pad_clamp`
copy), **shuffle model** (two-table nibble-LUT vs single `vqtbl4q_u8` lo6), **emit capacity** (u16 CTZ
vs `bulk_emit_positions_64` 64-base bulk store). Plus **toolchain decoupling**: PT goes to **stable**
(off the ICE-pinned nightly-2026-04-11, 93 recorded `on_disk_cache` ICEs) while bbnf-simd rides
skinny's nightly + `blake3`; a shared rlib forces a common MSRV floor + sync gate, coupling PT's stable
cutover to skinny's cadence. And sharing bbnf-simd *today* imports its scalar-stub dense classifier — a
consumer-less/perf-less share (Constitution viii).

**Exact open gaps.** (1) **[axis-decider, SEEDED not closed, WRONG baseline]** gate-5 divergence was
measured against **un-evolved** bbnf-simd (which lacks the `vpaddq` producer) → overstates 9B's case;
the corrected datum is **merged-surface divergence across ALL LEAN/DENSE sites vs the EVOLVED home**.
(2) **DENSE correctness is "parsed without panic," not a bit-parity oracle** — the checkasm harness is
a plan, not an artifact. (3) **The stable cutover is untested** — rebirthed *on* the nightly it claims
to shed. (4) The Q2 negative (naive nibble-stride corrupts the dense-64 site — silent `tz>>2` vs `tz`
misindex) used idiom (b) correctly but never *demonstrated* the negative as the load-bearing guard.
(5) The DENSE-win causal claim is not isolated — read the gate as **PASS only**, no speed claim.

**Why not BLOCKED.** Every intrinsic is stable since 1.59; the harness exists to port; the taxonomy is
settled art. The only failure mode is *losing to 9A on gate 5*, not a missing primitive.

---

## 9C — SCANNER-AS-DFA · **BLOCKED · 43%** (cross-lock, sequenceable — NOT BLOCKED-candidate)

**Architecture.** The hand scanner corpus dissolves: scanners become compiled DFA products of the
bootstrapped grammar. **The DFA is the scanner's PLANNER, never its RUNTIME** — it contributes exactly
two artifacts per scan site: the byte-equivalence **PARTITION** (`[u8;256] → byte_class_from_table_64`
or `_eq_set_64`) and the **ENTRY-BYTE SET** (→ PROVE-10 `Dfa::find` prefilter). Both are stateless
one-shot SIMD passes already resident in bbnf-simd; 9C is the emit pass that selects among them by
partition shape and deletes the hand nibble-LUT scanners they subsume.

**The substrate already exists, split across the two repos.** Producer half (bbnf-lang generator):
`crates/core/src/generate/regex/emit/dfa/mod.rs:128` already writes `static __BYTE_CLASSES:[u8;256]`
verbatim from the compiled DFA; `byte_class.rs:106 emit_byte_class_lut`; `regex_scan_adapter.rs:598`
gates it (`is_dispatchable && len>=4`, `MAX_DISPATCHED=32`). Consumer half (bbnf-simd kernels):
`byte_class_from_table_64` (its signature IS the DFA `byte_classes`), `byte_class_from_eq_set_64`
(≤8-member shufti), `classify_tbl4` (lo6-separable `vqtbl`). **The JOIN is the deliverable**: read
`dfa.byte_classes` + the non-dead entry set, SELECT the kernel by partition shape — entry ≤3 → memchr
prefilter (8.8–29×); ≤8 members → eq_set_64; lo6-separable → tbl4; general → byte_class_from_table_64.

**Census (F3, delivered — with over-ceiling denominators).** Rich-CSS `url()`
(`/[^)"'\s]+/`) collapses to **~2/255 classes, ~3/4096 states** — TIER-2 (eq_set membership over the
≤8-byte STOP set), the ideal case; a negated class is the paradigm scanner leaf. **Wide-unicode**
(`\p{L}`, `\u{...}` via `utf8.rs` byte-splitting) fans past 255 classes / 4096 states → `Dfa::compile`
returns `Err(state_limit)`: **0% coverage, the honest floor — falls back to scalar `char`-decode, not
a scanner.** Every leaf the scanners actually see (JSON structural, quote, ws, CSS STOP sets, `[0-9]`
membership) lands ≤~4 classes / ≤~5 states.

**Two confirmed-by-construction facts.** (a) **Naive nibble-only misclassifies the structural set**:
`{`=0x7B and `[`=0x5B share low-nibble 0xB, `}`=0x7D and `]`=0x5D share 0xD → a lo-only classifier
collapses brace↔bracket, corrupting depth. Immune: two-table shufti (`lo_r & hi_r`) or the full
256-table; INJECT_BUG gate must flip to lo-only and FAIL. (b) **The DFA cannot subsume the quote-parity
prefix-XOR** (carry-less `vmull_p64` in-string toggle) — an irreducible bit-parallel kernel 9C must
**carry**, never DFA-ify (dissolving it regresses the JSON hot path). Sheng (pass-1's novum) is
**retired to a measured negative** — the serial `vqtbl1q` chain loses to scalar `find_at` on the short
runs these DFAs see; the classify pass never walks state.

**Why BLOCKED (and why not BLOCKED-candidate).** Quad-homed on unbuilt cross-lock substrate: **LOCK-5**
owns the grammar→DFA source-of-truth (unbuilt at scanner fidelity) and must resolve the **`bbnf-regex
0.1.0` name+version collision** — authoritative source is the **parse-that-vendored `rust/regex`**
(XL-C one-regex-home; SPEC §1); GN-0 archives the legacy `skinny/crates/bbnf-regex` (a `[patch]`
`bbnf-regex→regex` makes the vendored copy win — collision dissolved by archival, no version bump).
**LOCK-2** owns tier-selection (scan-plan e-graph island). The codegen home
`crates/codegen/src/emit/regex/` is a **named GN-codex supersession** (Constitution ix), not a
land-grab. Q1 dense-classification is **at-best parity** with hand simdjson — value is deletion +
grammar-specialization + stable-by-construction + free `vshrn`, real but modest; Tier-M "parity by
construction" is **SPEC-ONLY** until the emit pass + separability solver exist. **The separability
solver is a bounded CSP over a 256-entry partition selecting among three known forms (nibble-pair ∨
lo6-table ∨ `vceqq`-fan) — tractable, NOT a primitive of equal difficulty to the original problem.**
So 9C is **cross-lock-BLOCKED and sequenceable behind LOCK-5+LOCK-2**, not BLOCKED-candidate; it cannot
converge as an independent LOCK-9 family and is carried into the gestalt as a phased consumer of the
kernel-home verdict (9C ◐ 9A).

---

## 9D — LAZY-SURFACE · **BLOCKED · 30%** (streaming center BLOCKED-candidate; RETIRE-or-run this pass)

**The family has two separable halves; only one survives.** research-D develops both to their
strongest form and finds the **streaming half SPEC-ONLY-negative** and the **cost-model half** the
durable, consumer-backed contribution.

**Streaming half — refuted at the substrate + a declared non-goal.** A grep census
(`feed|push_bytes|Decoder|BufRead|from_reader|document_stream|incremental`) returns **zero streaming
consumers**; `docs/future-research.md:201` lists "token streams, incremental parsing" as the
*deliberate non-goals of the 1.0.0 cut*. Every grammar builds a whole owned tree over a whole owned
input (`ParserState::new` → `allocate_padded_buf` single `copy_nonoverlapping`). Three structural
counterexamples a streaming redesign must defeat and cannot: **C1** the `PaddedView` over-read
invariant (`INPUT_PAD_BYTES=64`, branch-free tail stripe) — a bounded window has no valid tail padding
at a non-final chunk boundary, forcing either unsound over-read or the reintroduction of the per-stripe
bounds guard the padded design deletes; **C2** the zero-copy borrow tape (`StringPayload::Borrowed`) —
an evicted window invalidates every outstanding borrow, forcing every string to the `Owned` arena
copy; **C3** nested-checkpoint backtrack reaches arbitrarily far into resident bytes, so the retention
window degenerates to the whole in-flight subtree. **Per the constitution, whole-document-granularity
streaming does not reduce peak tree RSS** — the one streaming shape that *could* be bolted on buys
nothing on the metric streaming exists to improve.

**The missing primitive (the BLOCKED-candidate signature).** The memory center survives ONLY if
backtrack is bounded — but parse-that is a backtracking PEG, and PEG gives **no bounded-backtrack
guarantee** (Ford 2004). A semantic bound "may simply not be a property PEG provides — equal in
difficulty to redesigning the backtracking discipline itself." **The streaming/memory center is
BLOCKED-candidate.** research-D reasons the bound is *empirically* small (JSON is LL(1), first-byte
`dispatch_byte_multi` → wrong-branch rewind ≤1 byte; CSS `recover` is forward-resync with a
bounded rewind-to-frame-entry) — a *corpus-conditional* bound, not a semantic one. **P3 is the cheap,
deterministic decider that has now been skipped for three consecutive passes** while all four probes
went to the central axis: it either salvages a corpus-conditional ring buffer or files the measured
refutation.

**Cost-model half — the surviving design center, but a LOCK-2 CONSUMER, not an independent family.**
"Kernels selected by a cost model at combinator-compile time" survives intact. Substrate: **Seed A**
bbnf-simd `select_classifier(alphabet:&'static [u8;64])` — a compile-time-const choice; **Seed B**
`bbnf-regex::RegexInfo` (`first_chars`, `literal_prefix`, `negated_class`, `min/max_match_len`,
`dfa_size_estimate`) — the cost model's feature vector; **Seed C** `ws_comment.rs`'s runtime Tier1/2/3
branch (scalar reject / 16-byte classify / `#[cold]` 64-byte stripe) to *lift* to a compile-time
selection when a leaf's run-length distribution is known. The mechanism: each scan leaf carries its
grammar-derived `RegexInfo`; a pluggable **KernelCostTable** (fitted from paired PMU captures) scores
each legal kernel and bakes one specialized scanner. **This satisfies the no-consumerless-substrate law
by *being* the seam where LOCK-5's `RegexInfo` and LOCK-2's KernelCostTable cash out on the scan plane
— it is a consumer of LOCK-2's decision driver, the same island 9C's tier-selection needs, not a new
LOCK-9 subsystem.** The "lazy plane over windows" honestly reframed is the simdjson two-stage
structural-index over a *fully resident* buffer — needs no streaming.

**Disposition.** Marked **BLOCKED · 30%**, on its **last pass at a live %**: pass-3 runs P3 or 9D
converts to **RETIRED**, its residue (the cost-model kernel-selection as a LOCK-2 consumer; the
carry-threading into the gestalt-Ω compiled resumable scanner) surviving as inputs to other objects,
not as a live family. Zero throughput claim in any gate (the straddle guard is neutral-to-slower);
latency/TTFS is the only survivable streaming value and is itself consumer-gated.

---

## THE INCOMPATIBILITY MAP — which families cannot coexist, and precisely why

**9A ⊗ 9B — HARD-INCOMPATIBLE (kernel-home axis). THE central adjudication, now a PURE LOCK-6
MAINTENANCE CALL.** A kernel is either shared-source or own-source; it cannot be both. Both routes
reach the identical movemask contract-split (LEAN `vshrn` / DENSE `vpaddq` tree, purge the repack and
the double-`vaddv`) and the identical checkasm discipline — they differ only in whether one file or two
carry them. **The crowding-redirect rule (bank the redundant one) does NOT apply**: pass-2's two biases
point *opposite* ways (charge 9A the 9A-only de-face cost → tilts to 9B; re-measure gate-5 against the
*evolved* home → tilts to 9A), so banking either now picks the axis without the corrected evidence. The
verdict rests on **maintenance cost alone** (LOCK-6D: "null hypothesis defeated by measured
drift/maintenance cost"). LOCK-9 produces the two *corrected* measurements — 9A charged the de-face
cost + perf-half-on-CSS + evolved-home gate-5; 9B's merged-surface divergence vs the evolved home —
LOCK-6 renders the verdict. **Neither banked; 9B holds as incumbent-default (PT-2 frozen codex, the
only family off SPEC-ONLY); 9A's physical promise is proven live but its value is maintenance-only.**

**9C ⊗ 9B — INCOMPATIBLE (scanner-provenance axis).** 9C deletes parse-that's hand kernels and emits
them as compiled DFA products; 9B's thesis is that parse-that *authors and hand-rebirths* them. A
scanner cannot be simultaneously hand-authored-in-`core::arch` and non-existent-because-generated (they
co-occur only in the degenerate sense of a 9C Tier-miss leaf falling back to a hand kernel — 9C
conceding coverage, not 9B being adopted).

**9C ◐ 9A — PARTIALLY COMPATIBLE / SEQUENCEABLE (agree kernel home, differ provenance) → a DAG, not a
contradiction.** Both converge on bbnf-simd as the kernel backend: 9A by extraction, 9C by
lowering-to-it through the DFA. A gestalt holds both as ordered phases (kernel-home unification →
scanner-derivation). The prefilter double-home the gestalt must collapse:
`bbnf-simd::find_ascii_set_member64` ⟷ `bbnf-regex::Dfa::find` are two entry points to one prefilter
idea (GN-2/PT-1 dispatch law: one lowering per scan site, never both).

**9D ∥ {9A, 9B, 9C} — ORTHOGONAL AXIS (input surface).** 9D's *cost-model* half changes *what feeds*
the kernels and *which* kernel is selected, not *who owns* them — it threads into whichever kernel-home
verdict wins. Its *streaming* half is refuted and contradicts the eager `PaddedView` witness all three
share; it is a later, conditional overlay admissible only if P3 measures a bounded backtrack.

**9C ⊗ 9D — POSITIVE AFFINITY, re-homed onto structural carries.** Ragel/re2c lineage: a streaming
scanner IS a resumable DFA. Sheng being a measured negative, 9C collapses to a *stateless* Tier-M
membership classify + Tier-3 structural — 9D threads carries through the **Tier-3/structural kernels**
(`prefix_xor`/comment/bracket-depth carries bbnf-simd already exports), not a saved Sheng state.
Neither is independently converge-able; the **gestalt-Ω "compiled resumable scanner"** carries both as
inputs to ONE object. **If 9D RETIRES in pass-3, this graft is where its carry-threading residue
survives.**

**Summary matrix** (⊗ hard-incompatible · ◐ partial/sequenceable · ∥ orthogonal-composable):

|        | 9A            | 9B            | 9C            | 9D            |
|--------|---------------|---------------|---------------|---------------|
| **9A** | —             | ⊗ (kernel home)| ◐ (agree home, differ provenance; DAG) | ∥ (cost-model overlay; streaming needs PaddedView generalized) |
| **9B** | ⊗             | —             | ⊗ (provenance)| ∥ (same)      |
| **9C** | ◐             | ⊗             | —             | ⊗→gestalt-Ω (stateless classify + structural carries = resumable scanner) |
| **9D** | ∥             | ∥             | ⊗→gestalt-Ω   | —             |

---

## CROSS-LOCK DAG ORDERING (blockers LOCK-9 CANNOT retire alone)

- **9A ⊗ 9B → LOCK-6 (a maintenance-cost call, not a perf call).** LOCK-9 produces the corrected
  measurements (9A: perf-half-on-CSS + evolved-home gate-5 + the 9A-only de-face charge; 9B:
  merged-surface divergence vs the evolved home); LOCK-6 renders the home verdict. Co-converge at
  GESTALT; neither closes the axis alone.
- **Canonical movemask form → RESOLVED (measured); home-collapse → LOCK-9 ∩ LOCK-5.** The form is
  settled (contract-split, measured). Four producers must collapse to ONE lawful home (bbnf-simd
  repack + bbnf-simd refuted double-`vaddv` + PROVE-10 lean + parse-that `to_bitmask`); memchr's lean
  form lives in bbnf-regex's prefilter. The gestalt collapses the home.
- **Prefilter double-home → GN-2/PT-1 dispatch law.** One lowering per scan site, never both. 9C
  deepens this.
- **9C grammar→DFA source-of-truth → LOCK-5** (name+version collision resolved by GN-0 archival +
  `[patch]`; authoritative = vendored `rust/regex`); **9C tier-selection → LOCK-2** (scan-plan
  island); **9C codegen home → a named GN-codex supersession**. 9C is quad-homed; cannot be *selected*
  until LOCK-5 + LOCK-2 land.
- **9D cost-model `KernelPlan` → LOCK-2** (compile-time decision driver, the same island 9C needs) —
  a consumer, not a new subsystem.
- **Un-homed consumer edge → LOCK-4 / LOCK-8.** The reborn scanner surface feeds the combinator core
  (LOCK-4) and, via LOCK-6, the TS backbone (LOCK-8); no 9-family names how it presents to LOCK-4's
  parser graph or whether LOCK-8 mirrors 9C's compiled-DFA products. An un-homed edge the gestalt-P
  weave must close.

---

## Disposition roll-up for the REGISTRY

- **9A ONE-KERNEL-LIBRARY — ADVANCED, born-RED (52%).** Physical inline measured-GREEN live; value
  reconciled to maintenance-only (axis washes). Convergence blockers: perf half on the CORRECT CSS
  surface + one-line path-dep seam (both un-run); R-A3 purge must become a blocking gate; evolved-home
  divergence owed. No missing primitive of equal difficulty.
- **9B NATIVE-REBIRTH — ADVANCED, incumbent (62%).** Only family off SPEC-ONLY; LEAN+DENSE banked with
  insn counts. Close is the null-hypothesis gate-5 against the *evolved* home (concede or defeat 9A) +
  the bit-parity oracle + the stable-toolchain cutover (untested — rebirthed on nightly).
- **9C SCANNER-AS-DFA — BLOCKED (43%), sequenceable (NOT BLOCKED-candidate).** Cross-lock-coupled to
  LOCK-5 (grammar→DFA + name collision) + LOCK-2 (tier island) + a GN-codex supersession; dominant
  pass is parity + deletion, not speed; Sheng is a measured negative; the separability solver is a
  bounded CSP (tractable), unbuilt. Carried into the gestalt as a phased 9A consumer.
- **9D LAZY-SURFACE — BLOCKED (30%), streaming center BLOCKED-candidate.** Memory raison d'être
  refuted-by-default (consumer grep-absent, Y.13; bounded-backtrack a semantic primitive PEG may not
  provide); P3 unrun for a 3rd pass. **Pass-3 runs P3 or converts to RETIRED** (cost-model residue → a
  LOCK-2 consumer; carry-threading → the gestalt-Ω resumable scanner).

---

## THE TWO SHARPEST UNCERTAINTIES → pass-3 prototype briefs

The four prior probes all hit the central axis against the *wrong (un-evolved) baseline*; the two
BLOCKED families have never been probed. The two highest-advance uncertainties are therefore (1) the
**corrected** axis-decider — the evolved-home datum LOCK-6 actually needs, which no pass produced — and
(2) the cheapest, most-overdue family-disposition decider (9D-P3), which closes-or-retires a family
carried in limbo for three passes. Together they leave only 9C blocked on genuinely-cross-lock
substrate — the honest converged-core end state.

**BRIEF 1 — the evolved-home axis decider (bbnf-worktree).** Author the two canonical producers into a
scratch `skinny/crates/bbnf-simd` and re-measure the axis LOCK-6 needs. (1) In a scratch bbnf-simd,
add `movemask_nibble_64` (LEAN `vshrn_n_u16::<4>`) + `movemask_dense_64` (DENSE `vpaddq_u8` tree),
`#[inline]` `#[cfg(target_arch="aarch64")]`. (2) **Purge** the three refuted movemasks
(`movemask.rs:19-24`, `byte_class_from_eq_set_64.rs:79-86`, `comment_body_mask_64.rs:68-74`) and delete
the `OnceLock` table (`dispatch.rs:58-87`), re-pointing callers at direct `#[cfg]`-static. (3) Build
base-pinned to skinny@HEAD; `otool -tv` each producer. (4) Count merged-surface LOC across all
LEAN/DENSE sites (this evolved home) vs the two-home duplication; tabulate the residual substrate
divergence (padded-view/clamp, single- vs multi-table shuffle, capacity caps). **PASS/FAIL:** LEAN
emits `shrn.8b …,#4` with **zero `vaddv`** and ≤ the 509-`addv` portable baseline; DENSE is a log2
`vpaddq` tree with **zero reduction ops**; `rg 'vaddv|addv' bbnf-simd/src` over the purged crate
returns **zero** refuted double-`vaddv`; and the merged-surface-vs-duplicated LOC delta is emitted as a
number. FAIL if any producer shows a per-half `vaddv`, if the purge leaves a live double-`vaddv`, or if
the divergence table is not produced. Deliverable: the corrected LOCK-6 input (host-noise-immune static
disasm + LOC count), erasing the manufactured un-evolved divergence.

**BRIEF 2 — 9D-P3, the rewind histogram (pt-scratch).** The cheap deterministic decider skipped three
passes; it closes-or-retires 9D. In a scratch copy of `/Users/mkbabb/Programming/parse-that` (P23:
scratch-only): (1) add a `--features diagnostics`-gated thread-local counter at every
`state.offset = checkpoint` restore site (`leaf.rs:301-306` `one_of`, `combinators/macros.rs:91-297`,
`span_parser/assertions.rs:16-90`, `combinators/methods/recover.rs:37`, `span_parser/sep_by.rs:29-41`)
recording `checkpoint − offset_at_fail` and, separately, `furthest_offset − offset`. (2) Run over all
six JSON corpora (`data/json/{apache-builds,canada,citm_catalog,data,data_xl,twitter}.json`) + the CSS
corpora (bootstrap, normalize, tailwind-output), base-pinned; emit P50/P99/max per corpus, diagnostics
on vs off (to bound the probe's own cost). **PASS/FAIL:** if **P99 rewind distance < 64 bytes (one
stripe) on every corpus**, backtrack is bounded → a bounded ring buffer is sound and the 9D memory
center holds (survives as a conditional overlay). If **any corpus shows P99 in the kilobytes**,
backtrack is unbounded → file the measured refutation and **convert 9D to RETIRED** (memory center
defeated; cost-model residue folds into LOCK-2, carry-threading into gestalt-Ω). Either outcome is a
clean close with an artifact. Deliverable: 9D's disposition, settled after three deferrals.
