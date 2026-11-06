# LOCK-9 pass-1 — CRIT-1 (Adversarial Critic 1 of 2 · WITHIN-LOCK weight)

*Charter: internal soundness of each family spec, and whether the prototypes tested the
SHARPEST uncertainty — not a peripheral one. Substrate re-verified this session against the
live trees (`skinny/crates/bbnf-simd/`, `/Users/mkbabb/Programming/parse-that/rust/`).
Failure-mode checklist applied per family; constitution clauses (i–ix) checked; prototype-
evidence misuse audited. Convergence %s are earned-fraction estimates with exact enumerated
gaps — a family cannot exceed the fraction of its own gate set that ran and passed.*

---

## Substrate re-verification (what I could confirm independently)

CONFIRMED against live trees:
- `bbnf-simd/src/aarch64/movemask.rs:5` = `vshrn_n_u16::<4>` (law-compliant narrow). ✓
- `dispatch.rs` `PrimitiveKernels` is an `OnceLock` **fn-pointer struct** (`bitmap_prefix_xor_64:
  fn(u64,bool)->u64`, `byte_class_from_table_64`, `bulk_emit_positions_64`, …); the direct-cfg
  primitives (`select_backend`, `lo6_table_admissible`) coexist — the **split is real**. ✓
- parse-that `number_simd.rs:53-54`: double `vaddv_u8` (`lo_sum`,`hi_sum`) live. ✓
- `quote_parity.rs` `prefix_xor` carries the `#[cfg(…pclmulqdq)]` x86 arm. ✓
- `compute_in_string_bitmap`/`filter_quote_parity` are re-exports (`parsers/mod.rs`,
  `scan/mod.rs`) — PROTO-1's "dead relative to `json_parser`" call-graph claim is plausible. ✓
- `.cargo/config.toml`: `bbnf-regex`, `csp-solver` present; **`bbnf-simd` absent** — 9A's
  "one added line" seam claim verified. ✓
- **New finding the specs under-weight:** the patch table binds `bbnf-regex = { path = "regex" }`
  — parse-that's **own in-repo regex fork**, not bbnf-lang's `crates/bbnf-regex`. PROTO-2's
  census compiled through *that local fork*. This sharpens 9C-Q3 (below): the LOCK-5 coupling
  is not one regex crate to reconcile but potentially **two** (parse-that/rust/regex ⟂
  bbnf-lang/crates/bbnf-regex), and the census result speaks only for the local fork.

The substrate floor is honest. The defects are in **prototype-uncertainty targeting** and in
**gates that under-catch**, not in the paths:lines.

---

## LOCK-WIDE FINDING (weighs every family below)

**The lock's central adjudication — 9A ⊗ 9B, "who owns the byte kernels" — is NOT resolvable
from pass-1 evidence, and neither probe addressed its decider.** The incompatibility map names
the axis correctly and names the decider correctly (9A-Q1 cross-crate inline + 9B-gate-5 drift
measurement). But:
- PROTO-1 measured only a **fragment** of 9A-Q1 (the OnceLock devirtualization) **on a scalar
  kernel** (see 9A-G1), not the vector-tier inline-across-boundary question that is 9A's actual
  bet.
- **9B has zero artifact.** Gate 5 — the LOC-divergence / substrate-divergence measurement that
  is literally defined as the thing that decides the axis — was not run on either side.
- Pass-1 spent both probes on 9A(partial) + 9C(census), leaving the **incumbent (9B)** and the
  **cheapest-to-close family (9D-P3)** empty. Critic-1 judgment: the sharpest uncertainty in the
  entire lock is the 9A-vs-9B drift decider, and **no probe touched it.** This caps the lock's
  aggregate convergence and is the #1 pass-2 directive.

---

## 9A — ONE-KERNEL-LIBRARY · convergence **55%**

**What is earned.** Substrate verified; mechanism concrete; PROTO-1 **ran** and resolved a real
sub-question with a real gate FAILURE (Arm A's OnceLock `blr x8` survives fat-LTO, ~0.91–0.93×;
Arm B direct-cfg ICF-folds to baseline, 1.00×). Gate discipline is sound — the gate could and did
fail. The direct-cfg refactor prescription is actionable and low-invention (three primitives
already do it). Correctly NOT BLOCKED.

**Open gaps (enumerated):**

- **G1 — PROTO-1 tested the EASIEST kernel, not the sharpest (elegant-reduction trap).**
  `bitmap_prefix_xor_64` **delegates to a scalar 5-shift/xor bit-twiddle** — it has *no NEON body*.
  Arm B ICF-folded to baseline **precisely because it is scalar and stateless.** The family's
  actual mechanism is the **vector tier** (`use bbnf_simd::vector::*`, the shared `to_bitmask`
  movemask vocabulary) crossing the path-dep fat-LTO boundary as a **multi-instruction NEON body**.
  Whether *that* inlines with zero indirect calls AND no-regresses on a live consumer is
  **untested**. PROTO-1 proved devirtualization of a fn-pointer; it did not prove the family's bet.
  The "and-then-the-hard-part" (vector-tier inline + no-regress) is unconverged.

- **G2 — the headline free-win (the shared `vshrn` movemask) is untested AND contested by the
  family's own research.** The spec sells "`to_bitmask` delegates to the existing `vshrn`
  movemask … for free." PROTO-1 never exercised the movemask (it targeted prefix_xor).
  research-priorart §2a actively flags that `bbnf-simd/movemask.rs`'s reconstruction
  (`vand`/`vsri_n_u8::<3>`/`vzip1q_u8` + an 8-iter scalar pack loop) is **more elaborate than the
  lean `shrn#4→fmov→4-bit-stride` form** — i.e. 9A may hand parse-that an **over-built** movemask,
  the opposite of a free win. This is an internal-soundness contradiction between FAMILY-SPECS and
  its own research seed, unresolved.

- **G3 — gate 3 under-catches (a gate that cannot fully fail).** Gate 3 checks "reduction-count ≤
  509 baseline." The widen-back reconstruction in G2 uses **no `addv`**, so it PASSES gate 3 while
  still carrying the extra `vand`/`vsri`/`vzip`/8-iter-loop instruction weight. The gate measures
  reductions, not total instruction count, so it structurally cannot catch the over-engineering
  research-priorart named. Gate needs an instruction-count arm.

- **G4 — gate 2 (paired no-regression, 6 JSON corpora, live consumer) was NOT executed.** PROTO-1
  substituted a kernel micro-bench because the probed kernel is dead relative to `json_parser`
  (verified). Defensible honesty for *that* kernel, but the family's specified live-consumer gate
  remains formally unrun on any wired scanner.

- **G5 — the `vector/` module is SPEC-ONLY.** "ADD `src/vector/{mod,neon,scalar}.rs`" — it does
  not exist. The escape-hatch vector tier is **the bulk of 9A's actual surface** (decode
  store_chunk, ws_comment, scanners, span_parser/scan) and is entirely unbuilt and unproven. This
  is "the refuted 276-LOC simd128.rs promoted, minus double-vaddv" — promoting refuted code
  demands its *own* no-regression artifact, which does not exist (G1/G4).

- **G6 — Q3 granularity mismatch unquantified.** Several parse-that scans are 16-byte-lane
  compositions; bbnf-simd kernels are 64-byte-block. The spec concedes the physical iso is
  lane-level (weaker than the charter ideal) and defers to "the mapping census" — which was not
  produced. The consumer-tier-vs-vector-tier split fraction is unknown.

- **G7 — the path-dep-vs-vendor cost artifact (gate 5) not produced;** Q2 double-padding
  (`PaddedView`-64 vs `eob_pad_clamp`) named, not exercised.

**Verdict:** ADVANCED born-RED is the right disposition, but convergence is capped: PROTO-1 closed
the fn-pointer sub-question, not the family bet; the headline win is both untested and internally
contradicted.

---

## 9B — NATIVE-REBIRTH · convergence **55%**

**What is earned.** Substrate verified (the double-`vaddv` and x86 arms are live, as claimed).
Every intrinsic is stable `core::arch` since 1.59; the movemask taxonomy ((a) nibble-stride /
(b) dense-64 / (c) forbidden) is settled art with a named counterexample; the incumbent per PT-2.
Lowest-invention, cleanest to execute — correctly NOT BLOCKED.

**Open gaps (enumerated):**

- **G1 — ZERO prototype ran; the family is entirely SPEC-ONLY.** No artifact. Per constitution vii
  this is the SPEC-ONLY label, not a convergence. For the *incumbent* that is a discipline gap: 9B
  is the route a gestalt would default to, and it carries **no running evidence**.

- **G2 — the family's convergence-DECIDING gate (gate 5, null-hypothesis LOC/substrate divergence)
  is unrun.** The spec itself states 9B "loses to 9A on merit" iff post-rebirth kernels are
  near-identical, and that this is "a null hypothesis to defeat by measurement." That measurement
  **did not happen.** 9B cannot converge — and the 9A⊗9B axis cannot be adjudicated — until gate 5
  produces the divergence table. This is the same lock-wide finding, homed on 9B.

- **G3 — the cleanest standalone win was left unproven despite being trivially provable.**
  `number_simd.rs:53-54` double-`vaddv` → nibble-stride `vshrn` is a deterministic swap; a single
  `otool -tv` + insn-count would have produced a decisive pass-1 artifact for near-zero cost. It
  was asserted, not demonstrated. Pass-2 should bank it first — it is 9B's fastest path off
  SPEC-ONLY.

- **G4 — Q1 (dense-64 quote-parity self-regression) named, not exercised.** The taxonomy's claim
  that `classify_stripe_64` MUST use idiom (b) and that naive nibble-stride there corrupts parity
  (256 bits) is stated but not shown on the real site. The one place 9B can self-inflict the
  refuted defect is precisely the one place unmeasured.

- **G5 — `vmull_p64` `target_feature="aes"` dual-arm rot risk (Q2) named, harness unported.** The
  checkasm-liveness port (gate 3) is described in full but is prospective; "the harness exists to
  port" is a plan, not an artifact.

**Verdict:** ADVANCED-incumbent is defensible on risk, but convergence is capped at ~55% by total
absence of running evidence and — decisively — by gate 5 (the axis decider) being unrun. 9B's
"only failure mode is losing to 9A on merit" is exactly the measurement pass-1 skipped.

---

## 9C — SCANNER-AS-DFA · convergence **45%**

**What is earned.** PROTO-2 **ran**, deterministic, and cleanly closed **Q2 (register budget)**:
5/5 archetypal leaves fit the *strict* `vqtbl1q_u8` budget (≤16 states ∧ ≤16 classes); number
9×7=63 is the tight boundary and had to be measured. Real, reproducible, honest. Tiers 0/1/3 are
prior-confirmed art.

**Open gaps (enumerated):**

- **G1 — the family-KILLER (Q1, dense-classification no-win) is UNTESTED.** PROTO-2 is a census
  with "no bench." The family's own admission is that on the **dominant** pass (structural bitmap
  classifies every byte, nothing to skip) a DFA-lowered classify is **at best parity** with hand
  simdjson. The value proposition (deletion + stable-by-construction + free movemask) is real but
  **modest and unmeasured**. Q2 GO does not touch Q1.

- **G2 — Q3 (cross-lock coupling to LOCK-5) is UNTOUCHED by the prototype and is the actual
  BLOCKER.** PROTO-2 built its DFAs through **parse-that's existing hand-written regex fork**
  (`path = "regex"`, verified), NOT the bootstrapped-grammar-as-source-of-truth that 9C's
  architecture *requires*. So the census proves "a DFA minimizes small enough," and proves
  **nothing** about "the bootstrapped grammar → DFA → byte-fixpoint-gated source-of-truth pipeline
  exists at the fidelity 9C needs." That pipeline is LOCK-5's deliverable and is unbuilt. The
  missing-primitive test lands here: the enabling substrate is **owned elsewhere and unbuilt**.
  BLOCKED-candidate correctly held; PROTO-2 does **not** lift it.

- **G3 — PROTO-2's verdict overreaches its own gate (vacuous-convergence risk).** The report
  concludes "no degrade, no coverage-percentage caveat." But the spec's own framing keeps **dense
  classify hand-written** — that *is* a coverage caveat. The census adjudicates the transition-
  kernel question ONLY (which it says elsewhere), yet the headline sentence claims a
  whole-family clean bill. The clean result is narrow; the disposition is unchanged.

- **G4 — imported "CONFIRMED" labels (spec-cites-other-context circularity).** Tiers 0/1/3 are
  tagged CONFIRMED via PROVE-10 / emit-spine — confirmations earned in **bbnf-lang's** regex/emit
  context, not in **parse-that's scanner plane**. Using cross-repo/cross-context CONFIRMED to
  anchor 9C's in-plane tiers is mild circularity; an in-plane parity check is owed.

- **G5 — the "9C DFA = 9D resumable scanner" affinity is asserted (Ragel lineage), not
  prototyped;** correctly deferred to gestalt-Ω, noted here so it is not silently treated as
  converged.

**Verdict:** BLOCKED-candidate correct. Convergence ~45%: one sub-question (Q2) closed well; the
two family-deciding items (Q1 dense parity, Q3 coupling) untested, Q3 unbuildable within LOCK-9.

---

## 9D — LAZY-SURFACE · convergence **35%**

**What is earned.** The most honest self-diagnosis in the set: it names its own killers
(backtrack bound, absent streaming consumer, Y.13) rather than hiding them. The `Window`-witness /
degenerate-eager-case framing is elegant and single-path-compliant.

**Open gaps (enumerated):**

- **G1 — NO prototype ran, though the family calls its own decisive gate "cheap and
  deterministic."** P3 (instrument `furthest_offset − offset` rewind distance across JSON+CSS) is
  self-described as cheap and deterministic — and was **not executed** in pass-1. The entire memory
  raison d'être rests on an **unmeasured** property. This is the plainest prototype-allocation miss
  in the lock: the cheapest decisive probe was skipped.

- **G2 — the central claim is SPEC-ONLY and self-diagnosed as a missing primitive.** "parse-that's
  PEG semantics may simply not provide [a bounded backtrack] — a property equal in difficulty to
  redesigning the backtracking discipline itself." By the family's own words this is the
  missing-primitive-of-equal-difficulty test, failing by default until P3 measures otherwise.

- **G3 — live Y.13 violation (constitution viii) in the DESIGN.** The `Window` plane is
  consumer-less substrate: the spec admits "no streaming consumer is named," and parse-that's real
  consumers (value.js, latex-paper, keyframes) parse whole documents; simdjson's own
  `document_stream` is whole-document-granular. "Consumer-less substrate is forbidden — in designs
  as in code." The design as written trips the clause it cites.

- **G4 — `lazy.rs` name-collision disambiguation (combinator-graph laziness vs input-window
  laziness) is named, not resolved.** A real architectural debt left open.

- **G5 — the throughput honesty is correct but total: 9D must make ZERO throughput claim (the
  straddle-guard resurrects the tail guard `PaddedView` deleted), leaving latency/time-to-first-
  token as the only survivable value — and P2 (peak-RSS harness) that would prove even that was not
  run.**

**Verdict:** BLOCKED-candidate correct. Convergence ~35%: entirely SPEC-ONLY on its center, both
killer gates unrun despite one being cheap, raison d'être refuted-by-default.

---

## Constitution & failure-mode roll-up (critic-1 view)

- **(i) grammar totality:** 9A/9B kernels are lawfully exempt (primitives/combinators clause); no
  violation. 9C correctly rides the grammar but its enabling pipeline is unbuilt (9C-G2).
- **(iv) refutation laws:** well carried by every family's gate text; the double-`vaddv` / vshrn /
  no-digit-SIMD / x86-dead / warm-bench laws are explicit. No violation found.
- **(vii) concrete-artifacts-or-SPEC-ONLY:** 9A = partial artifact (wrong-kernel caveat, G1);
  9B = SPEC-ONLY (no artifact); 9C = census artifact (Q2 only); 9D = SPEC-ONLY, killer gates unrun.
  Two of four families have **no running evidence on their deciding question**, and the lock's
  central axis-decider (gate 5) is unrun on both sides.
- **(viii) consumer-less substrate:** 9D trips it in-design (9D-G3); 9A's `vector/` has consumers
  but is unbuilt (9A-G5).
- **Elegant-reduction trap:** 9A-G1 is the textbook instance — the load-bearing step (vector-tier
  inline + no-regress on a live consumer) reads "one-line swap, shim ergonomics" and is unconverged.
- **Gate-that-cannot-fully-fail:** 9A gate 3 (reduction-count only, misses instruction weight,
  9A-G3).
- **Vacuous convergence:** 9C PROTO-2's "no coverage-percentage caveat" headline vs its own dense-
  hand-written concession (9C-G3).
- **SPEC-ONLY anchoring a perf/merit claim:** 9B's entire merit case (gate 5) and 9D's memory case
  (P3) are SPEC-ONLY.

## Pass-2 directives (critic-1 priority order)

1. **Run the 9A⊗9B decider.** 9B gate 5 (LOC/substrate divergence table) + 9A's **vector-tier**
   cross-crate inline + no-regression on a **live** scanner (not prefix_xor, not a micro-bench).
   Until this runs the lock's central axis is unadjudicated.
2. **Re-probe 9A on a real NEON body** (`byte_class_from_table_64` / a `to_bitmask` movemask site),
   and settle 9A-G2/G3: is bbnf-simd's widen-back movemask the lean form parse-that should inherit,
   with an instruction-count gate arm added.
3. **Bank 9B-G3** (number_simd double-`vaddv` → `vshrn`, disasm + insn-count) — cheapest decisive
   pass-2 artifact.
4. **Run 9D-P3** (backtrack-bound instrumentation) — cheap, deterministic, decides the family.
5. **9C:** state explicitly that PROTO-2 closes Q2 ONLY; the disposition stays BLOCKED-candidate on
   Q1(dense parity) + Q3(LOCK-5 coupling, two-regex-crate reconciliation).

## Convergence summary

| family | disposition (held) | convergence | deciding gap |
|---|---|---:|---|
| 9A ONE-KERNEL-LIBRARY | ADVANCED born-RED | **55%** | vector-tier inline+no-regress untested; movemask win contested |
| 9B NATIVE-REBIRTH | ADVANCED incumbent | **55%** | zero artifact; gate-5 axis-decider unrun |
| 9C SCANNER-AS-DFA | BLOCKED-candidate | **45%** | Q1 dense parity + Q3 LOCK-5 coupling untested |
| 9D LAZY-SURFACE | BLOCKED-candidate | **35%** | P3 backtrack bound unrun; Y.13 tripped in-design |
