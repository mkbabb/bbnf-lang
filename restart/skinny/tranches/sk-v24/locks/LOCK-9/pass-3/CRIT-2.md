# LOCK-9 pass-3 · CRIT-2 — the cross-lock adversary (scanner & kernel plane)

*Adversarial critic 2 of 2, pass 3. Critic 1 owns the intra-family teardown; my mandated weight is the
CROSS-LOCK view — where LOCK-9 contradicts, double-homes, or leaves consumer-less work against the GN/PT
codex, the CROSS-LOCK ledger (XL-A..H, conveyed as binding fact), and the other locks. Every substrate
claim below was re-verified against the live trees THIS session — parse-that@`/Users/mkbabb/Programming/
parse-that`, bbnf-simd + skinny@`/Users/mkbabb/Programming/bbnf-lang/skinny`; citations flagged VERIFIED
are mine, not inherited. The failure-mode checklist is applied by name. HARD-CAP honored.*

---

## 0. Pipeline-order finding — THE SAME DEFECT AS PASS 2, RECURRING (governs §2, §4)

`FAMILY-SPECS.md` mtime **22:44:36**. Its own commissioned pass-3 prototypes landed AFTER it:
`PROTO-1.md` **23:23**, `PROTO-1-vaddv-purge.patch` **23:22**, `PROTO-2b.md` **23:29** (only `PROTO-2.md`
20:16 preceded it). **The synthesis has not seen PROTO-1 or PROTO-2b** — the two probes that discharge
its own headline open question. This is verbatim the structural defect CRIT-2 flagged in pass-2 (`pass-2/
CRIT-2.md §0`): the synthesis is frozen before the evidence commissioned to move it. It recurred.

The consequence is not cosmetic. **PROTO-1.md (23:23) RAN half (B) of the very pass-4 BRIEF 1 the
synthesis writes as "the keystone neither pass produced":** it authored the canonical DENSE-64 `vpaddq`
producer into a scratch bbnf-simd, **purged all four double-`vaddv` sites**, built green, showed the gate
`rg vaddv` **clean**, and **emitted the LOC/substrate divergence table** — the LOCK-6 maintenance-cost
datum. Therefore, as written:

- **9A Q1** ("the evolved-home residual divergence — the LOCK-6 datum neither pass produced") is **FALSE
  as of 23:23** — PROTO-1 produced it. Stale.
- **9A gate (4)** ("author the canonical producers into a scratch bbnf-simd + purge, THEN measure residual
  divergence") — the author+purge+divergence half **RAN**. Stale.
- **9B Q1** ("Gate 5 against the evolved home — SEEDED not closed") — the divergence table against the
  evolved home is **delivered** (and, read honestly, it weakens 9B, not 9A — §3.1). Stale.
- **pass-4 BRIEF 1** re-commissions completed work: its steps (1) author-into-scratch, (2)/(5) purge +
  divergence table are DONE. The **only** un-run piece is the CSS paired cold ratio (step 3–4), which
  PROTO-1 explicitly deferred (loadavg 5–9, contended). The correct pass-4 brief is JUST the CSS ratio +
  the seam rewire — a fraction of what is written.

The registry-writer must reconcile FAMILY-SPECS to PROTO-1/PROTO-2b, not the reverse — again.

---

## 1. Verification ledger (re-checked against the live trees, not taken on faith)

- **VERIFIED — bbnf-simd's four double-`vaddv` sites are live:** `bracket_depth_mask_64.rs:78-79`,
  `structural_classify_64.rs:151-152`, `comment_body_mask_64.rs:72-73`, `byte_class_from_eq_set_64.rs:83-84`.
  `vpaddq_u8` in **zero** files (`rg -c` empty). Reconciliation 3 is true against the tree; the canonical
  DENSE producer is genuinely absent and owed to any home. PROTO-1 purged all four **in scratch only**;
  the canonical tree still ships the refutation-law violation.
- **VERIFIED — no digit-MAC `vaddv` exists in bbnf-simd at all** (`rg vaddv` over all of
  `bbnf-simd/src` outside the four mask producers = empty). **This breaks 9A gate (1) as written:** the
  gate says `rg 'vaddv_u8|vaddvq_u8' skinny/crates/bbnf-simd/src/aarch64` "must return ONLY the digit-MAC
  site." There is no digit-MAC site in bbnf-simd — digit-scan SIMD is refutation-barred and 9A's own
  census marks `number_simd` as no-go, *per-repo*. The digit-MAC `vaddv` the binding-laws paragraph cites
  (`number_simd.rs:54-55`) lives in **parse-that**, a different home. So the gate greps home A but expects
  home B's residual. After a correct purge the grep returns **nothing** — which PROTO-1 confirms ("none").
  The gate's pass-condition is mis-specified (a residual that cannot be present); it is a gate that a
  correct purge would "fail." Fix the gate text to "returns nothing."
- **VERIFIED — parse-that `number_simd.rs:54-55` double-`vaddv` STILL LIVE in the owner tree** (`vaddv_u8
  (lo_masked)`, `vaddv_u8(hi_masked)`). Refutation-law iv is stated-and-violated-in-place for the **third
  consecutive pass**; 9B's abrogation is proposed across three passes, executed in none (P23 scratch only).
- **VERIFIED — `simd128.rs` does NOT EXIST anywhere in parse-that** (`find … -name simd128.rs` outside
  target = empty). **research-B's mechanism is anchored to a phantom file.** research-B §3 names
  "`src/simd128.rs:322` `to_bitmask` — the refuted producer; **the single point of correction** that fixes
  every vector-tier consumer at once"; §2 claims "the de-nightly shim `simd128.rs` (276 LOC) already
  exposes the exact `std::simd` vocabulary … each scanner ports by one line `use … crate::simd128::*`."
  None of that exists. `portable_simd` `to_bitmask` is used **inline across 8 scan files** (VERIFIED:
  `scanners.rs`, `span_parser/scan.rs`, `quote_parity.rs`, `quoted_simd.rs`, `structural_bitmap.rs`,
  `ws_comment.rs`, `decode.rs`, + `lib.rs`). There is **no single point of correction**; de-nightly must
  rewrite the idiom at every site. The pass-2 CRIT-2 itself mis-cited `simd128.rs:322` as VERIFIED — the
  phantom has been carried, uncorrected, across two critic passes. (The FAMILY-SPECS 9B mechanism silently
  dropped the shim and proposes a *new* `parse_that/src/simd/{lean,dense}.rs` dir-module — VERIFIED absent
  too, but that is a lawful proposal; the defect is that the **de-nightly blast-radius estimate** in
  research-B rests on the phantom, so 9B's stable-cutover cost is unsubstantiated, not "7 files one line").
- **VERIFIED — parse-that `.cargo/config.toml [patch.crates-io]` patches `bbnf-regex = { path = "regex" }`;
  `bbnf-simd` ABSENT.** 9A's "one added line" is real and the P23 mutation is real (still named-not-granted).
- **VERIFIED — skinny has NO in-tree DFA source-of-truth.** `skinny/crates/parse-that-regex/src` =
  `{number, unicode, integration, lib.rs}` — **no `automata/`**. `skinny/crates/bbnf-regex/src/lib.rs` =
  **322 LOC, no `automata/`** (the analysis stub). Reconciliation 5 is exact; 9C's substrate gap is real
  and LOCK-5/GN-1-owned.
- **VERIFIED — the F3 collision note is in-tree:** `structural_classify_64.rs:13-17` records `;`=0x3B ≡
  `{`=0x3B under lo6 and routes to the `vceqq` fan. `lo6_table_admissible` at `dispatch.rs:101`. The
  separability crux is genuinely half-built.

Substrate is honest where cited *and re-verified* — but 9B's mechanism cites a non-existent file (its
"single point of correction"), 9A gate (1) references a non-existent residual, and the synthesis is stale
against PROTO-1/2b (§0).

---

## 2. Where the pass-3 protos CONFIRM / EXTEND / CONTRADICT the synthesis

**CONFIRM+EXTEND (credit) — PROTO-1 delivered the purge, bit-identically.** The canonical DENSE-64
`vpaddq` producer is authored, **0/200k mismatches vs scalar AND vs the refuted form**, lowered to **7
`uzp/orr`, zero horizontal reduction** vs the refuted 8 `addv` (`PROTO-1-producers.disasm`); all four
sites purged; scratch crate builds green; gate clean. This retires pass-2 CRIT G9A-3 ("the purge has no
artifact") — the purge is now proven feasible and parity-preserving. **But it is scratch-only and the
synthesis never saw it** (§0), so the FAMILY-SPECS still lists it as owed.

**CONFIRM+EXTEND (credit) — PROTO-2 + PROTO-2b retire the 9C elegant-reduction trap for MEMBERSHIP.**
Pass-2 CRIT G9C-4 named "compile through `equiv.rs` (exists) then emit through the separability solver
(unbuilt)" as the read-and-then-the-hard-part pattern. PROTO-2/2b **build the hard part** against the
**real** bbnf-regex DFA: three leaves emit a bit-identical classifier (8129-block sweep, 0 mismatches),
the lo6-colliding `;`+`{` set is provably routed OFF `vqtbl4q` with no silent drop, INJECT_BUG bites 100%,
`\p{L}` = 41 classes/21 states (correcting the spec's `→ None` prediction), and PROTO-2b proves the
`byte_classes`-DIRECT partition == the `find_at` oracle partition. This is the cleanest evidence in the
lock. The elegant-reduction trap is **substantially retired for the membership tier** — a real 9C advance.

**CONTRADICT-1 — the 9C emit-feasibility proof covers MEMBERSHIP, not multi-kind STRUCTURAL classify.**
All three PROTO-2/2b leaves compile to **`num_classes = 2`** — a *binary* member/non-member split
(PROTO-2b run: every leaf "num_classes=2 … member classes [1]"). Even `json_structural` `[{}\[\]:,"\\]`
is one membership class; the DFA does **not** distinguish `{` from `:`. But the actual JSON/CSS structural
INDEX simdjson builds needs *per-kind* tokens (which structural byte, for the tape) — the `vceqq`-fan's
raison d'être. PROTO-2 cross-checked the fan **once** (probe 1, 0 mismatches) but on the same binary set;
PROTO-2b's direct path routes only to Tbl4/FullTable and **never exercises the fan**. So the census's
"**Tier structural (multi-kind, per-byte distinct)**" and research-C's `url([^)]*)` ~6-class row are
**emit-UNPROVEN** — the harder half of the lattice (multi-class output, not binary membership) remains
SPEC-ONLY. The synthesis's "all three leaves emit a bit-identical classifier" is true only for a
classifier that answers *is-member*, not *which-kind*. 9C's dissolution claim is narrower than stated.

**CONTRADICT-2 (stale) — 9A Q1 / 9B Q1 "neither pass produced the divergence datum" is false** (§0).
PROTO-1 produced it at 23:23.

**UNMEASURED for the THIRD consecutive pass — the CSS paired ratio.** PROTO-1 explicitly deferred the cold
paired `parse_that_css` ratio (loadavg 5–9). 9A gate (2) — the no-regression floor on the *correct* (CSS)
surface — has now never executed across **three** passes. The perf half is honestly SPEC-ONLY (PROTO-1
says so, constitution vii-clean), but it means 9A's only perf obligation is zero-measured after three passes.

---

## 3. Cross-lock adjudication (my mandated weight)

### 3.1 X-1 [carried, now DATA-FURNISHED] 9A⊗9B is LOCK-6's verdict — and the delivered datum tilts to 9A, not 9B
The portfolio homes the axis at LOCK-6 ("null hypothesis defeated by measured drift/maintenance cost",
LOCK-6D verbatim; CROSS-LOCK conveys it). The FAMILY-SPECS correctly refuses to render it internally —
good, no portfolio contradiction. **But its "9B holds as incumbent-default" reading is now under tension
from PROTO-1's own divergence table**, which the synthesis did not see. Read as data (PROTO-1.md:100-110):
the shared home (9A/bbnf-simd) already carries — that 9B must *author from scratch* — (i) the stable
`core::arch` dialect (9B is still on nightly, headline unverified), (ii) the `vqtbl4q` lo6 table
classifier (absent from parse-that scan/), (iii) the full checkasm scalar-parity oracle (parse-that
asserts only "parsed w/o panic"), and (iv) an x86-clean surface (9B carries dead SSE4.2 in 9 files to
purge either way). The genuinely domain-fit residual divergence (PaddedView-64 vs `eob_pad_clamp`;
per-16 vs 64-byte granularity) is small. So the *corrected* gate-5 input — the one CRIT-2 pass-2 demanded
(X-8) — **weakens the incumbent-default framing**: 9B's maintenance cost is "author the oracle + the table
+ leave nightly + purge x86," all of which 9A inherits. This is LOCK-6's to weigh, not mine to close, but
the synthesis's thumb on the scale ("9B holds as incumbent-default") is not supported by the datum its own
probe produced. Both remain un-banked; correct.

### 3.2 X-2 [carried, MEASURED] the movemask home-collapse is a LOCK-9 ∩ LOCK-5 gestalt act, now owed with data
Four producers (bbnf-simd `movemask.rs` scalar-repack [VERIFIED `:19-24` the `for pair in 0..8` spill];
bbnf-simd's four refuted double-`vaddv`; PROVE-10 LEAN; parse-that inline `to_bitmask`) collapse to ONE
lawful home. PROTO-1 measured the lawful DENSE producer (7 uzp/orr) and PROTO-2 the routing; **the collapse
itself is un-done** — the gestalt owes the home. memchr's LEAN form lives in the LOCK-5 prefilter. No new
contradiction; the measurement is now in hand, the home is not.

### 3.3 X-3 [carried, DEEPENED] the prefilter double-home AND the 9C↔home-axis coupling
`bbnf_simd::find_ascii_set_member64` ⟷ `Dfa::find` — GN-2/PT-1 dispatch law: one lowering per scan site.
9C deepens this (its emit routes DFA `byte_classes` into `bbnf_simd::byte_class_from_table_64`). **New
cross-lock finding the DAG glosses:** 9C's emit-feasibility was proven *against the bbnf-simd kernel
vocabulary* (PROTO-2/2b port `bbnf_simd::classify_tbl4`, `byte_class_from_table_64`, `dispatch::
lo6_table_admissible`). The FAMILY-SPECS DAG says "(kernel-home verdict 9A vs 9B) → (9C emits into that
home)" — home-agnostic in prose. But if **9B wins the home axis** (parse-that keeps its own kernels), 9C's
proven-feasible emit must be **re-proven against parse-that's native kernels**, which lack `vqtbl4q`
(VERIFIED: no `vqtbl` in parse-that scan/) and lack the `lo6_table_admissible` predicate. So 9C's
feasibility proof is **silently coupled to a 9A win**; under a 9B win it regresses to unproven. The DAG
must name this: 9C-feasibility ⊨ 9A-home, or 9C owes a second proof against 9B's vocabulary.

### 3.4 X-4 [carried] 9C stays triple-lock-coupled; the vendoring is un-executed (VERIFIED)
9C's substrate is LOCK-5/GN-1's un-executed automata-tier vendoring (VERIFIED: skinny `parse-that-regex`
has no `automata/`), plus LOCK-2's tier-selection island, plus a GN-codex codegen-home supersession
(`crates/codegen/src/emit/regex/`, correctly NAMED per constitution ix). PROTO-2/2b decouple *feasibility*
from vendoring (credit) — but *selectability* stays blocked on three locks. BLOCKED-candidate is the right
disposition; the uptick is justified but I temper it for the multi-kind gap (§2 CONTRADICT-1).

### 3.5 X-5 [carried] 9C and 9D-residue DOUBLE-CONSUME the same unbuilt LOCK-2 scan-plan island
9C's tier-selection ("LOCK-2 scan-plan island") and 9D's surviving residue (the cost-model `KernelPlan`
selection, re-homed to LOCK-2) and the prefilter dispatch all consume the **same** LOCK-2 decision-driver
island — which is **unbuilt** (pass-2 CRIT G9D-4, uncontested). This concentrates risk: if LOCK-2 does not
deliver that island, 9C tier-selection AND 9D-residue AND the prefilter one-lowering law all stall on one
missing consumer-substrate. The FAMILY-SPECS routes each to LOCK-2 separately but never names that they
are **one** island bearing three LOCK-9 consumers — a single point of cross-lock failure. Y.13 is
satisfied only if LOCK-2 builds it; until then all three are consumers of an unbuilt substrate.

### 3.6 X-6 [carried, UNADDRESSED] the un-homed scanner→combinator/TS edge
No 9-family names how the reborn scanner surface presents to LOCK-4's parser graph, nor whether LOCK-8's
TS backbone mirrors 9C's compiled-DFA products (XL-D: the fast TS face is the build-time emitter — does it
emit DFA classifiers too?). The FAMILY-SPECS CROSS-LOCK DAG lists it ("Un-homed consumer edge →
LOCK-4/LOCK-8") but closes nothing. This is the gestalt-P weave's, correctly deferred — but it is a live
open edge, not a resolved one, and LOCK-8's symmetric confirmation is still XL-G-pending (P2).

### 3.7 XL-C consistency — the synthesis absorbed the pass-2 map-correction (credit)
Reconciliation 5 correctly re-homes the collision to `skinny/crates/bbnf-regex` (the 322-LOC stub) and
`parse-that/rust/regex` (the real engine), superseding the pass-1/pass-2 phantom `bbnf-lang/crates/
bbnf-regex`. The rename-on-vendor mechanism (`parse-that-regex`) matches XL-C's "vendored parse-that-regex
inside skinny is the measurement substrate." No contradiction with the ledger; the map is now correct.

---

## 4. Per-family convergence + enumerated open gaps (critic-2 numbers, independent of critic-1)

### 9A — ONE-KERNEL-LIBRARY · **53%** · ADVANCED, born-RED (concur RED)
Under-counted by its own synthesis (§0): PROTO-1 delivered the purge (bit-identical, reduction-free,
green) + the divergence table. But the perf floor is zero-measured for a third pass, gate (1) is
mis-specified, and the vector/ substrate is SPEC-ONLY. Net a small rise from the purge artifact.
- **G9A-1 [gate (1) mis-specified].** "`rg vaddv … must return ONLY the digit-MAC site" — no digit-MAC
  site exists in bbnf-simd (VERIFIED); correct post-purge residual is *nothing* (PROTO-1 confirms). The
  gate conflates parse-that's `number_simd.rs:54-55` with a bbnf-simd residual. Fix to "returns nothing."
- **G9A-2 [perf floor unmeasured, 3rd pass].** Gate (2) CSS paired cold ratio never ran (PROTO-1 deferred,
  loadavg). 9A's sole perf obligation (a no-regression FLOOR, not a speedup) is SPEC-ONLY after three passes.
- **G9A-3 [the purge is scratch-only].** PROTO-1 purged a *copy*; the canonical bbnf-simd still ships four
  double-`vaddv` (VERIFIED). Adopting 9A requires the purge land in-tree first, still un-executed.
- **G9A-4 [SPEC-ONLY] `vector/{mod,neon,scalar}.rs` does not exist** (VERIFIED absent). The ~60-LOC shared
  u8x16/mask8x16 vocabulary the vector-tier files (`span_parser/scan`, `decode`) would consume is unbuilt;
  no no-regression artifact for it.
- **G9A-5 [cross-lock] P23 supersession named-not-granted** — the one-line `[patch]` mutates parse-that's
  READ-ONLY tree (VERIFIED bbnf-simd absent from the patch table).
- **G9A-6 [cross-lock] the axis close is LOCK-6's** (§3.1); the corrected datum tilts to 9A but LOCK-9
  cannot render it.

### 9B — NATIVE-REBIRTH · **60%** · ADVANCED, incumbent (docked from 62: phantom substrate + live violation)
The only family off SPEC-ONLY (real banked disasm wins). Docked for the phantom `simd128.rs` mechanism and
the refutation-violation persisting a third pass in-tree.
- **G9B-1 [mechanism anchored to a phantom file].** research-B's "single point of correction
  (`simd128.rs:322`)" and "port by one line via `crate::simd128`" cite a file that **does not exist**
  (VERIFIED). The FAMILY-SPECS silently substitutes a new `simd/{lean,dense}.rs` dir-module (lawful) but
  never flags the shim's absence, so the **de-nightly blast-radius is unsubstantiated** — the real cutover
  rewrites inline `portable_simd` `to_bitmask` across 8 files (VERIFIED), not "7 files, one line each."
- **G9B-2 [refutation-law violated in-place, 3rd pass].** `number_simd.rs:54-55` double-`vaddv` still live
  in the owner tree (VERIFIED). Abrogation proposed thrice, executed never (P23 scratch).
- **G9B-3 [gate 5 delivered-but-adverse].** PROTO-1's evolved-home divergence table is 9B's gate-5 datum
  too — and read honestly (§3.1) it lists what 9B must author that 9A inherits (oracle, `vqtbl4q`, stable
  dialect, x86 purge). The merged-surface table exists now; it does not obviously favor the incumbent.
- **G9B-4 [DENSE bit-parity oracle unbuilt].** The native checkasm harness is un-ported; DENSE correctness
  is "parsed without panic," not bit-parity. Q2 stands.
- **G9B-5 [stable cutover never run].** Headline (off nightly) unverified — rebirths authored on nightly;
  the `regex/generated.rs` `fmt_helpers_for_derive` regen-fix is outside the kernel plane and un-done.
- **G9B-6 [self-regression negative undemonstrated].** The naive-nibble-stride-corrupts-DENSE guard is used
  correctly but never shown as the load-bearing negative (PROTO-2/2b demonstrated the *lo6-collision* drop
  for 9C, a different negative; 9B's `<<k*16`-stitch corruption is unshown).

### 9C — SCANNER-AS-DFA · **46%** · BLOCKED-candidate (concur; modest uptick from 43, tempered)
The pass-3 mover: PROTO-2/2b are the cleanest artifacts in the lock and substantially retire the
elegant-reduction trap **for membership**. Tempered below the synthesis's 46-48 for the multi-kind gap.
- **G9C-1 [membership proven, multi-kind structural UNPROVEN].** §2 CONTRADICT-1: all probe leaves are
  `num_classes=2` (binary membership); the `vceqq`-fan multi-kind classify (the actual JSON/CSS structural
  index) is exercised once on a binary set and never on the direct path. The harder half of the emit
  lattice stays SPEC-ONLY.
- **G9C-2 [substrate un-vendored].** VERIFIED: skinny has no `automata/` (neither `parse-that-regex` nor
  the 322-LOC `bbnf-regex` stub). No in-tree DFA source; LOCK-5/GN-1 owns the un-executed vendoring.
- **G9C-3 [emit pass is a scratch example, not the codegen home].** PROTO-2/2b built `examples/emit_probe
  *.rs` — feasibility, not the production `crates/codegen/src/emit/regex/` pass. The named GN-codex
  supersession (constitution ix ✓) is declared, unbuilt.
- **G9C-4 [cross-lock] 9C-feasibility is silently coupled to a 9A home-win** (§3.3). Proven against
  bbnf-simd's vocabulary; under a 9B win it needs re-proof against kernels lacking `vqtbl4q`/`lo6_admissible`.
- **G9C-5 [cross-lock] triple-coupled** — LOCK-5 vendoring + LOCK-2 tier island (the shared unbuilt island,
  §3.5) + GN-codex codegen. BLOCKED-candidate stands; not equal-difficulty (engine green, predicate half-built).

### 9D — LAZY-SURFACE · **90% converged-as-RETIRED** (concur RETIRE)
The RETIRE is the honest, directive-sanctioned close: the memory center is over-determined-refuted
(consumer grep-absent, span-dangle wall, PaddedView anti-chunking, §0/51 dead-by-law). I concur.
- **G9D-1 [histogram retired-unmeasured].** P3 — the "cheap, deterministic, born-RED decider" the pass-1
  AND pass-2 registries both flagged mandatory — is unrun for a **third** consecutive pass; research-D
  delivered the spec, not the histogram. The RETIRE is sound *only because* the streaming center is
  refuted on independent grounds, so the histogram no longer gates anything — acceptable, but note the
  family retires without ever running its own declared decider.
- **G9D-2 [residue is an unbuilt-LOCK-2 consumer].** The surviving cost-model `KernelPlan` selection
  re-homes to the same unbuilt LOCK-2 island 9C and the prefilter law consume (§3.5) — the residue is not
  closed within LOCK-9; it is a deferred LOCK-2 consumer, conditional and un-substrated today.
- **G9D-3 [carry-threading folds to gestalt-Ω]** — the 9C⊗9D resumable-scanner object (re-based onto
  structural carries, Sheng-negative) is a gestalt artifact, not a LOCK-9 deliverable. Correctly deferred.

---

## 5. Constitution & evidence-misuse scan

- **(iv) refutation laws:** the double-`vaddv` the law forbids is **still live in BOTH candidate homes** —
  `number_simd.rs:54-55` (parse-that owner tree) and the four bbnf-simd mask producers (VERIFIED) —
  stated-and-violated-in-place across **three** passes. PROTO-1 purged bbnf-simd's four in scratch; no
  in-tree purge in either home.
- **(vii) concrete-artifacts-or-SPEC-ONLY:** clean on labeling — PROTO-1 self-flags the CSS ratio SPEC-ONLY;
  PROTO-2/2b make no perf claim; no SPEC-ONLY item anchors a "faster" claim (checked). But 9C's multi-kind
  structural tier is asserted from a binary-membership proof (§2) — a feasibility over-reach, not a
  mislabel; and 9A's perf floor is SPEC-ONLY for a third pass.
- **(viii) Y.13 consumer-less:** 9D's streaming center correctly retired as consumer-less (grep-verified in
  prior passes). **Watch:** 9A's `vector/` module (unbuilt, G9A-4) and the LOCK-2 scan-plan island that 9C
  tier-selection + 9D residue + prefilter dispatch all consume (§3.5) are **substrate-not-yet-built with
  consumers named** — lawful as designs only if the gestalt commits the substrate; a design that ships
  three consumers of an unbuilt island is Y.13-adjacent.
- **(ii) true isomorphism:** the movemask quad-home (§3.2) and the bbnf-regex name+version collision
  (resolved-in-mechanism, un-executed) are two live "one system" tensions, both LOCK-5/gestalt-owned.
- **(iii) rust↔rust iso:** 9C-feasibility⊨9A-home (§3.3) is an unnamed isomorphism coupling — if the homes
  diverge, 9C's proof does not transport.
- **(v) zero-runtime-cost types / (vi) parsimony:** n/a; clean (no god module; the proposed `simd/`,
  `vector/`, `emit/regex/` are proper directory-modules).
- **prototype-evidence misuse:** PROTO-1 and PROTO-2/2b are honest, self-flagging, base-pinned artifacts —
  no fabrication. The misuse is **upstream**: the synthesis is stale against PROTO-1/2b (§0), research-B
  cites a phantom file (§1), and the multi-kind claim over-reads binary-membership evidence (§2).

---

## 6. Roll-up (critic-2 numbers)

| family | disp (concur) | CRIT-2 % | Δ vs pass-2 CRIT-2 | the sentence that caps it |
|---|---|---:|:--:|---|
| **9A** | ADVANCED born-RED | **53%** | 55→53 | PROTO-1 (unseen by the synth) delivered the purge bit-identically + the LOCK-6 divergence table — a real advance the synthesis undercounts — but the CSS perf floor is zero-measured for a **third** pass, gate (1) references a non-existent digit-MAC residual, the vector/ substrate is SPEC-ONLY, and the in-tree purge + P23 grant are un-executed. |
| **9B** | ADVANCED incumbent | **60%** | 62→60 | Banked disasm wins are real, but the mechanism is anchored to a **phantom `simd128.rs`** (VERIFIED absent — its "single point of correction"), the double-`vaddv` is violated-in-place a third pass in the owner tree, the DENSE oracle + stable cutover are unrun, and PROTO-1's delivered gate-5 datum reads *against* the incumbent-default, not for it. |
| **9C** | BLOCKED-candidate | **46%** | 43→46 | PROTO-2/2b are the lock's cleanest artifacts and retire the elegant-reduction trap **for membership** — but the multi-kind `vceqq`-fan structural classify (the real JSON/CSS index) is proven only on binary sets, the automata tier is un-vendored (VERIFIED no skinny `automata/`), the emit is a scratch example not the codegen pass, and feasibility is silently coupled to a 9A home-win. |
| **9D** | RETIRED | **90%** | — | RETIRE is the honest directive-sanctioned close on an over-determined refutation; docked from "done" because P3 retires unmeasured a third time and the surviving residue is a consumer of the same unbuilt LOCK-2 island 9C + the prefilter law also need. |

**Blockers LOCK-9 CANNOT retire alone (→ gestalt/other locks):** 9A⊗9B verdict → LOCK-6 (§3.1, datum now
tilts to 9A); movemask home-collapse → LOCK-9 ∩ LOCK-5 gestalt (§3.2, measured, home owed); prefilter
double-home + 9C↔home-axis coupling → GN-2/PT-1 dispatch law + the DAG must name 9C-feasibility⊨9A-home
(§3.3); 9C substrate → LOCK-5/GN-1 un-executed vendoring (§3.4); the **single** unbuilt LOCK-2 scan-plan
island bearing THREE LOCK-9 consumers (9C tier-select, 9D residue, prefilter) → LOCK-2 (§3.5); un-homed
scanner→combinator/TS edge → LOCK-4/LOCK-8 + XL-G(P2) pending (§3.6).

**One-line verdict:** pass-3 produced the lock's two strongest artifacts — PROTO-1's bit-identical
evolved-home purge + divergence table and PROTO-2/2b's membership-emit feasibility against the real DFA —
yet **the synthesis froze at 22:44 before it saw either** (the same pipeline-order defect as pass 2), so it
re-commissions completed work as pass-4 BRIEF 1 and still calls the divergence datum "unproduced"; the
axis (9A⊗9B) stays LOCK-6's and the corrected datum tilts to **9A** (which inherits the oracle, table,
stable dialect, and x86-cleanliness 9B must author), 9B's own mechanism is anchored to a phantom
`simd128.rs`, 9C's clean feasibility proof covers only binary membership (not the multi-kind structural
index) and is silently coupled to a 9A home-win, and 9D retires honestly while its residue and 9C both wait
on one unbuilt LOCK-2 island — and the double-`vaddv` the refutation law forbids remains live in-place in
**both** candidate homes for the third consecutive pass.
