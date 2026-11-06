# LOCK-9 pass-1 · CRIT-2 — the cross-lock adversary (scanner & kernel plane)

*Adversarial critic 2 of 2, pass 1. Critic 1 owns the intra-family teardown; my weight is the
CROSS-LOCK view — where LOCK-9 contradicts or double-homes against the GN/PT codex and the
other eight locks. Every substrate claim below was re-verified against the live trees this
session (parse-that@`/Users/mkbabb/Programming/parse-that`, bbnf-simd@`skinny/crates/bbnf-simd`);
citations flagged VERIFIED are mine, not inherited. The failure-mode checklist is applied by
name to each family.*

---

## Verification ledger (what I re-checked, not took on faith)

- **VERIFIED** `number_simd.rs:54-55` — `let lo_sum = vaddv_u8(...)`, `let hi_sum = vaddv_u8(...)`.
  The forbidden double-`vaddv` is live exactly where FAMILY-SPECS says. (research-B's `:56-60`
  cite is the *consumer stitch*, the reductions are `:54-55` — FAMILY-SPECS is the accurate one.)
- **VERIFIED** `.cargo/config.toml [patch.crates-io]` = {bbnf-regex, bbnf, bbnf-ir, bbnf-ser,
  csp-solver, egraph, egraph-derive}; **`bbnf-simd` absent**. 9A's "one added line" is real.
- **VERIFIED** `bbnf-simd/src/lib.rs` prim split: `bitmap_prefix_xor_64` (`:261`),
  `byte_class_from_table_64` (`:256`), `bitmap_next_set_bit` (`:267`), `bulk_emit_positions_64`
  (`:272`), `eob_pad_clamp` (`:277`) all route `(dispatch::primitive_kernels().<fn>)(...)` —
  indirect. `structural_classify_64` (`:301`), the CSS masks (`:322`, `:344`) use direct
  `#[cfg(target_arch="aarch64")] return neon`. The facade IS split as claimed.
- **VERIFIED — and this one bites (see X-2):** `aarch64/movemask.rs:4` —
  `pub unsafe fn movemask_u8x16(value) -> u16`. It runs `vshrn_n_u16::<4>` **then repacks to a
  dense `u16`.** bbnf-simd's movemask is NOT the lean 4-bit-stride form PROVE-10's memchr lowering
  exhibits (`shrn.8b → fmov x8,d0 → 4-bit stride, no repack`). This is the "widen-back
  reconstruction" research-priorart §2a flagged — and it is load-bearing for both 9A and 9B.
- **VERIFIED** `quote_parity.rs`: `classify_stripe_64` (`:72`) does `.to_bitmask()` (`:81-82`);
  `prefix_xor` (`:41`), `pclmulqdq` arm (`:42`), `vmull_p64` (`:54`). As specified.

Substrate is honest. The problem is not the map — it is which gates *ran* and which the pass
quietly routed around.

---

## The two prototypes: what they actually decided vs. what they were commissioned to decide

**PROTO-1 (9A/9B) proved the negative half of Q1 and skipped 9A's entire raison d'être.**
It is a clean, real, disassembled result — the OnceLock fn-pointer facade survives fat-LTO as
`blr x8` in the loop body (VERIFIED-credible: the disasm is machine fact, robust to the loadavg-16
noise the author flags). That kills 9A-through-the-current-facade. Good. **But:**

1. **The make-or-break was never run at the commissioned altitude.** FAMILY-SPECS gate (1) reads
   "`otool -tv` **the wired scanner** proves the kernel inlined." PROTO-1 disassembled a *probe
   crate*, not a wired parse-that scanner, and proved arm_b works by **ICF-folding onto baseline
   in an isolated crate**. Identical-code-folding inside a 5-function probe binary does NOT
   demonstrate that `#[inline]` crosses the *real* parse_that↔bbnf-simd path-dep boundary under
   the actual scanner's register pressure. The positive half of Q1 — "direct-cfg + fat-LTO carries
   inline across the path-dep in the live integration" — is **inferred, not measured**. This is the
   elegant-reduction trap in textbook form: *"the OnceLock blocks inline (proven) and then the hard
   part — proving direct-cfg inlines in the real wired scanner — reads as done but is unconverged."*

2. **9A's stated central win — the free `vshrn` movemask — was explicitly NOT probed.** research-A
   §5 is unambiguous: 9A's structural payoff is "*9A contains the PT-2 W1 movemask fix as a
   corollary of sharing.*" PROTO-1 chose `bitmap_prefix_xor_64` **because** "it has no `to_bitmask`
   reduction" (its own criterion-(b) line: "N/A to this kernel… belongs to a classify-stripe
   probe, not the prefix-xor probe"). It probed the one kernel where the movemask question cannot
   arise, and deferred the movemask reroute — the *thing the family exists to deliver* — to a
   future probe. The prefix_xor kernel is also, by the proto's own §Method, **dead relative to the
   `json_parser` bench**. So PROTO-1 measured a micro-timing of a kernel that (a) never runs in the
   real bench and (b) does not exercise 9A's headline mechanism. **Unverified gestalt:** the axis
   verdict is being built on the one corner that dodges the load-bearing claim.

**PROTO-2 (9C Sheng census) returned GO for the archetypes and never censused the leaf its own
research named as the killer.** research-C's PROVE-9C-A commissioned "compile the number/string/
ws/identifier/hex leaves of the **five grammars (json, css, csv, plus the two authored)**." PROTO-2
compiled five **generic archetype regexes typed into a test file** — not sourced from
`bootstrap/regex.bbnf`, not from the CSS grammar. Two consequences:

3. **The grammar-totality path (constitution clause i) was bypassed.** 9C's entire thesis is
   *grammar leaf → bootstrapped DFA → kernel*. PROTO-2 fed hand-authored patterns straight into
   `Dfa::compile_with`, proving "these 5 regexes minimize small," NOT "the bootstrapped grammar's
   leaves minimize small." The source-of-truth pipeline that IS 9C was not exercised even at census
   altitude. Spec-cites-itself risk: the census validates the archetypes the census chose.

4. **The named counterexample was excluded from the sample.** research-C risk 2 and FAMILY-SPECS Q2
   both name "a **rich CSS `url()` or a wide unicode class**" as the state/class-explosion boundary.
   PROTO-2 censused none of these — and admits "byte-mode = unicode-mode… the escaped-dot did not
   expand the byte-class alphabet beyond ASCII **here**." I.e. no wide-unicode leaf was in the
   sample *at all*, so the one dimension that could blow the ≤16×16 budget was structurally absent.
   The verdict "no coverage-percentage caveat" is over-claimed: it is 100% of a sample that
   deliberately omits the failure case. **Gate that cannot fail:** a feasibility census whose sample
   excludes the infeasibility candidate.

Neither proto is dishonest — both flag their deviations. But an adversary must record that **the
pass produced zero measurement of (i) 9A's movemask reroute, (ii) 9A's real-integration inline,
(iii) any 9B kernel rebirth, (iv) 9C's grammar-sourced or wide-class leaves, (v) anything at all
for 9D.** The convergence numbers below are anchored to that census, not to the prose.

---

## Per-family convergence + enumerated open gaps

### 9A — ONE-KERNEL-LIBRARY · **58%** · ADVANCED, born-RED (I concur RED, dock the %)

The negative-half proof (OnceLock blocks inline) is genuine and moves the axis. Everything the
family needs to *stand* is open.

- **G9A-1 (the deferred make-or-break).** Gate (1) — direct-cfg kernel inlines in the *wired*
  parse-that scanner across the path-dep boundary — is inferred from probe-crate ICF, never
  disassembled at integration. Until an actual scanner is wired and `otool`'d, 9A's viability is
  a plausibility, not a result.
- **G9A-2 (the un-probed raison d'être).** The `vshrn` movemask reroute — 9A's headline win per
  research-A §5 — was explicitly skipped (criterion b "N/A to this kernel"). And **X-2 below shows
  the reroute may deliver a *dense-u16 repack*, not the lean form**, so the "free correct movemask"
  claim is not merely unmeasured, it is contested by the code I read.
- **G9A-3 (the dense-`to_bitmask` semantic contradiction, unresolved).** The vector-tier's selling
  point is one-line drop-in (`use std::simd::*` → `use bbnf_simd::vector::*`), which REQUIRES
  `to_bitmask()` to keep its *dense 1-bit-per-lane* semantics. `vshrn` produces *4-bit-per-lane*.
  So the vector tier either (a) changes semantics and breaks the one-line-swap ergonomics, or
  (b) keeps dense semantics and pays bbnf-simd's repack (VERIFIED: `movemask_u8x16 -> u16`) — whose
  cost vs the refuted shim's double-`vaddv` is **unmeasured**. FAMILY-SPECS asserts "minus its
  regressing double-`vaddv`" as if the repack were free; it is not established that vshrn+repack
  beats vand+2×vaddv. This is the *exact* semantic that forced the shim's defect, re-imported into
  the shared crate un-adjudicated.
- **G9A-4 (the paired-no-regression gate never executed).** Gate (2) — ≥0.98× per corpus, both
  windows, N≥200, at integration — did not run. PROTO-1 substituted a micro-timing of a
  bench-dead kernel and, at loadavg 13-16, got 0.91-0.93× for arm_a. The family's actual close
  condition is un-touched.
- **G9A-5 (cross-lock: the P23 read-only supersession, un-named).** 9A's "one added line" writes
  parse-that's `.cargo/config.toml`. Per research-priorart §4, parse-that is **READ-ONLY to the
  bbnf-lang campaign (non-clobber P23)**. Even a gitignored config edit is a mutation of a
  read-only repo. 9A requires a *named supersession* of P23 (or a gestalt adjudication); FAMILY-SPECS
  notes PT-2 chose 9B but never states that adopting 9A overturns the read-only law. Consumer-less
  substrate's inverse: a substrate change with no authority to make it.
- **G9A-6 (Q2 double-padding, untested).** `PaddedView`/`INPUT_PAD_BYTES=64` vs `prim::eob_pad_clamp`
  tail contract — flagged, never probed.

### 9B — NATIVE-REBIRTH · **64%** · ADVANCED, incumbent (highest, but zero artifact this pass)

Settled art, lowest-risk, cleanest execution — I concur it is the incumbent. But the pass produced
**no 9B measurement whatsoever**, and its close condition is symmetrically un-run.

- **G9B-1 (its cleanest standalone win is un-executed).** The `number_simd.rs:54-55` double-`vaddv`
  → nibble-stride swap (VERIFIED live) is named the family's "cleanest standalone win" and its
  first prototype target. It was not done. A pass that touches 9B and does not land the one clean
  win it advertises has not moved the family — it has re-stated it.
- **G9B-2 (the null-hypothesis gate (5) never measured — and it is the ONLY thing that decides
  9A⊗9B).** Gate (5) is the post-rebirth LOC-divergence + substrate-divergence measurement that
  "defeats or concedes to 9A." Nothing was rebirthed, so divergence is unmeasurable. The axis
  cannot close from *either* side this pass: 9A's positive half is un-probed (G9A-1/2) and 9B's
  divergence datum does not exist. **The central adjudication of the entire lock has zero
  measured input.**
- **G9B-3 (cross-lock: gate (5) IS a LOCK-6 verdict, mis-homed here).** 9B's founding bet —
  "the reasonable point of rust↔rust kernel iso (**Constitution clause iii**) is design-level" —
  is, verbatim, LOCK-6's question. The portfolio: "LOCK-6 decides where reasonable ends
  (measured, not asserted)." 9B's "null hypothesis defeated by measured drift cost" is LOCK-6D's
  framing word-for-word. LOCK-9 **cannot** settle 9A⊗9B internally; the drift-vs-coupling verdict
  is LOCK-6's to render and both consume it. FAMILY-SPECS treats the axis as LOCK-9-decidable via
  each family's Q1 — that is a double-home LOCK-9 does not own.
- **G9B-4 (the movemask-repack differentiator, un-disasm'd).** 9B's `first_match` "elides the
  repack bbnf-simd keeps" (research-B §2). VERIFIED: bbnf-simd's `movemask_u8x16` DOES repack to
  dense u16, so a leaner 9B movemask is *plausible* — but it is exactly the kind of "obvious win"
  §0/09 says must be paired-measured, and it was not. If 9B's movemask is materially leaner than
  the shared crate's, that is a point AGAINST 9A (G9A-2) — but it cuts both ways only when disasm'd.
- **G9B-5 (co-traveler scope creep, unverified alloc-neutrality).** `smallbox[nightly]` → stable
  `smallbox!` at `parse.rs:74` is claimed "alloc-neutral" with no measurement; leaving nightly
  drags in feature-attr deletions the family "inherits not owns." The blast radius of the
  de-nightly cutover is asserted bounded, not shown bounded.

### 9C — SCANNER-AS-DFA · **44%** · BLOCKED-candidate (I concur BLOCKED; the census over-claims GO)

The Sheng census is a real artifact and lifts 9C above 9D. But it is scoped to dodge its own
killer, and the family is not LOCK-9's to converge.

- **G9C-1 (the census excludes the explosion candidate).** Per finding 4 above: no rich-CSS-`url()`,
  no wide-unicode leaf in the sample. GO holds for ASCII archetypes only; the boundary case
  research-C named is un-censused. Coverage is "100% of a sample chosen to fit."
- **G9C-2 (grammar-totality path un-exercised).** Per finding 3: leaves were hand-typed, not sourced
  from `bootstrap/regex.bbnf`. The grammar → DFA source-of-truth pipeline — the constitution-i
  spine of 9C — was not run even at census altitude.
- **G9C-3 (dominant-pass value is parity, admitted).** Dense structural classify has nothing to
  skip; DFA-lowered dense = parity with hand simdjson at best. 9C's value on the hot pass is
  deletion + specialization + stable-by-construction, NOT speed. Honest, but it means 9C cannot
  justify itself on the perf axis the lock's gates are written in.
- **G9C-4 (cross-lock: quad-homed, and its enabling substrate is unbuilt).** By FAMILY-SPECS's own
  admission 9C "cannot converge independent of LOCK-5's verdict." Its pieces live in: LOCK-5
  (bootstrapped regex grammar → DFA source of truth), LOCK-2 (the scan-plan-selection e-graph
  island that picks the tier — named in LOCK-2/2A), LOCK-9 (the bbnf-simd `classify_tbl4` kernel
  backend), and GN codegen (`crates/codegen/src/emit/regex/`). This is not a LOCK-9 architectural
  center — it is a cross-lock integration wearing a LOCK-9 badge. The missing-primitive test lands
  exactly as FAMILY-SPECS states: the novum (Tier-2 codegen) is cheap, but its **enabling substrate
  — the byte-fixpoint-gated grammar→DFA→table source of truth — is owned by LOCK-5 and unbuilt at
  the fidelity 9C needs.** Substrate-less consumer.
- **G9C-5 (the class-map/Sheng composition is un-probed).** PROTO-2 measured only the *transition
  table size*. Whether the dense classify (produces the class byte) and the `vqtbl1q_u8` Sheng
  step (advances state) compose without a per-byte serial dependency stall — the thing that decides
  if a *vectorized* DFA actually vectorizes — was not touched. "State' = tbl(row[class], state)" is
  a data-dependent chain; PROVE-9C-B (the paired Sheng scanner) is the probe that would show it and
  it did not run.

### 9D — LAZY-SURFACE · **30%** · BLOCKED-candidate (nothing ran; even the cheap gate skipped)

- **G9D-1 (zero prototype — including the "cheap, deterministic" one).** P1-P4 all un-run. P3 (the
  backtrack-bound instrumentation) is the family's own decider and is described as "cheap and
  deterministic," yet it was not executed. A BLOCKED-candidate whose deciding measurement is cheap
  and was skipped has not been adjudicated — it has been deferred.
- **G9D-2 (memory raison d'être refuted-by-default).** Backtracking PEG rewind may be unbounded;
  if so, the ring buffer retains all input = whole-input model, zero peak-RSS win. Unmeasured =
  refuted per the family's own honest framing.
- **G9D-3 (Y.13 violation stands: no streaming consumer named).** value.js / latex-paper / keyframes
  all parse whole documents; simdjson's own `document_stream` is whole-document-granular. FAMILY-SPECS
  concedes "no streaming consumer is named." A `Window` plane with no in-wave consumer is forbidden
  substrate — the family is consumer-less by its own admission.
- **G9D-4 (cross-lock: KernelPlan double-consumes LOCK-2 machinery not built).** 9D's compile-time
  `KernelPlan` "folds PROVE-10's runtime prefilter + bbnf-simd's OnceLock dispatch into a
  compile-time-selected plan." That plan-selection-at-compile-time IS LOCK-2's decision-driver
  territory (the same island 9C needs). 9D leans on an unconverged LOCK-2 substrate — same
  substrate-less-consumer defect as 9C-G4, on a different axis.

---

## CROSS-LOCK adjudication (my mandated weight) — where LOCK-9 double-homes or contradicts

- **X-1 — The lock's central adjudication (9A⊗9B) is not LOCK-9's to decide; it is LOCK-6's.**
  Both families frame the kernel-home choice as "reasonable point of rust↔rust isomorphism"
  (Constitution iii) resolved by "null hypothesis defeated by measured drift/maintenance cost" —
  that is LOCK-6 clause iii + LOCK-6D verbatim. The portfolio homes the isomorphism verdict in
  LOCK-6. **Named requirement for the gestalt:** 9A⊗9B cannot close in LOCK-9 pass-N; it closes
  when LOCK-6 renders the reasonable-point verdict AND the two missing measurements (G9A-1/2,
  G9B-2) exist. Any LOCK-9 "convergence" that claims to settle the axis internally is contradicting
  the portfolio's own homing of LOCK-6.

- **X-2 — The movemask form is a triple-home contradiction the codex has not collapsed.** THREE
  entry points to "narrow a compare to a mask" now coexist: (1) bbnf-simd `movemask_u8x16` —
  VERIFIED returns dense `u16` via `vshrn`+repack; (2) PROVE-10's memchr lowering — lean
  `shrn.8b→fmov→4-bit-stride`, no repack; (3) parse-that's `to_bitmask` sites. research-priorart §2a
  and §7 flag this as an OPEN question ("is bbnf-simd's widen-back reconstruction measured-optimal
  or over-engineering?"). It is unresolved, and it is **load-bearing for BOTH 9A (inherits (1)'s
  repack — G9A-2/3) and 9B (claims to elide it — G9B-4).** The lock cannot converge either family
  while the canonical movemask form is undecided. This is a LOCK-9∩LOCK-5 double-home (memchr lives
  in bbnf-regex's prefilter, PROVE-10) that the gestalt must collapse to ONE lawful producer.

- **X-3 — The prefilter double-home (bbnf-simd ⟷ bbnf-regex) straddles LOCK-9 and LOCK-5.**
  research-priorart §6 + FAMILY-SPECS: `bbnf-simd::find_ascii_set_member64` and
  `bbnf-regex::Dfa::find` are two entry points to the same prefilter idea, governed by the
  "GN-2.W1⇄PT-1.W5 dispatch law: one lowering per scan site, never both." LOCK-9's 9C route
  *deepens* this coupling (it lowers scanners THROUGH the DFA). The gestalt-Ω must collapse this
  double-home; no single lock can, because the two entry points are homed in two different locks'
  crates. LOCK-9 must not silently pick one — it must defer to the GN-2/PT-1 dispatch law it
  inherits.

- **X-4 —9C is quad-homed and contradicts the GN/PT codex's tranche boundaries.** 9C's deliverable
  spans LOCK-5 (grammar/DFA), LOCK-2 (plan island), LOCK-9 (kernel), and GN codegen — and its
  codegen home `crates/codegen/src/emit/regex/` is GN-1-post territory. Per constitution ix
  (extant-tranche consciousness), placing scanner-derivation codegen there is a **re-cut of the GN
  codex that must be a named supersession**, not an implicit LOCK-9 land-grab. FAMILY-SPECS marks
  9C BLOCKED-candidate but does not name the GN-codex supersession its codegen home implies.

- **X-5 — The Ragel/re2c affinity (9C's DFA = 9D's resumable scanner) is a gestalt-Ω consolidation,
  correctly flagged — but it means neither 9C nor 9D is independently converge-able.** If a
  streaming scanner IS a resumable DFA (research-D §3), then 9C and 9D are two projections of one
  object and the gestalt should NOT converge them as separate LOCK-9 families — it should carry
  both as inputs to a single Ω-level "compiled resumable scanner" and collapse. FAMILY-SPECS notes
  the affinity but still books them as two families with two convergence tracks; that is a mild
  double-count the roll-up should acknowledge.

- **X-6 — No contradiction with LOCK-4/LOCK-8 detected, but a consumer seam is unstated.** The
  scanner plane feeds the combinator core (LOCK-4) and, via LOCK-6, the TS backbone (LOCK-8). None
  of the four 9-families names how the reborn scanner surface presents to LOCK-4's parser graph or
  whether LOCK-8's TS side mirrors any of it (9C's compiled-DFA products would need a TS story under
  LOCK-6). Not a contradiction — an *un-homed consumer edge* the gestalt-P weave must close, worth
  recording so it is not discovered late.

---

## Constitution & evidence-misuse scan (checklist, by clause)

- **(vii) Concrete artifacts or SPEC-ONLY:** 9D is effectively SPEC-ONLY this pass (no probe) yet is
  booked ADVANCED-adjacent as BLOCKED-*candidate* — the label is honest but the pass banked no
  artifact. 9B is settled-art but produced no measurement; its "cleanest win" (G9B-1) is un-landed.
  **No SPEC_ONLY item anchors a perf claim** — I checked; the families are disciplined about "no
  faster in any gate." Good.
- **(viii) Consumer-less substrate (Y.13):** 9D-G3 (no streaming consumer) and, more subtly, 9A's
  `vector/` tier promoting the *refuted* simd128.rs into the shared crate — its consumer is
  parse-that's compositional scanners, so it clears Y.13, but it re-homes a REFUTED artifact and
  the "minus the double-vaddv" claim is unverified (G9A-3). Watch that the refuted shim is not
  laundered into legitimacy by relocation.
- **(iv) Refutation laws:** honored in letter — vshrn-not-double-vaddv, no digit-scan SIMD, no
  block-scheduling, x86-dead, warm-benches-out are all carried. **But the double-`vaddv` the law
  forbids is STILL LIVE at `number_simd.rs:54-55` (VERIFIED) and the pass did not remove it**
  (G9B-1). The law is stated and violated-in-place simultaneously; the fix is named, un-executed.
- **(v) Zero-runtime-cost types:** n/a to the kernel plane; no violation.
- **(vi) Parsimony / no god modules:** 9B's `simd/{mod,movemask,neon,scalar}.rs` is clean. 9A's
  `vector/` tier is a genuine added surface; acceptable. No god module.
- **(ii) True isomorphism:** the movemask triple-home (X-2) is a direct tension with "one system";
  unresolved.
- **Prototype-evidence misuse:** PROTO-1 probed a **bench-dead kernel** and a **probe crate** in
  place of the commissioned wired-scanner gate, and skipped the movemask criterion that is the
  family's purpose (findings 1-2). PROTO-2 censused a sample that **excludes its own named
  explosion candidate** and bypassed the grammar-source path (findings 3-4). Neither fabricates,
  both self-flag — but the evidence does not underwrite the confidence the dispositions carry.

---

## Roll-up (critic-2 numbers; independent of critic-1)

| family | disp (concur) | CRIT-2 % | the single sentence that caps it |
|---|---|---|---|
| **9A** | ADVANCED born-RED | **58%** | Negative half of Q1 proven; the movemask reroute (its raison d'être), the real-integration inline, the paired gate, and the P23 supersession are all un-run — and the shared movemask VERIFIED-repacks to dense u16, contesting the "free correct movemask" claim. |
| **9B** | ADVANCED incumbent | **64%** | Settled art, lowest risk — but the pass landed zero 9B artifact, its cleanest win (`number_simd` double-vaddv, VERIFIED live) is un-executed, and its deciding gate (5) is un-measurable because nothing was rebirthed. |
| **9C** | BLOCKED-candidate | **44%** | Sheng GO holds only for an ASCII archetype sample that omits the rich-CSS/wide-unicode leaf its own research named as the killer, bypasses the grammar-source path, and the family is quad-homed on an unbuilt LOCK-5/LOCK-2 substrate. |
| **9D** | BLOCKED-candidate | **30%** | Nothing ran — including the cheap deterministic P3 that alone decides it; memory center refuted-by-default; no streaming consumer named (Y.13). |

**Convergence blockers the lock CANNOT retire inside LOCK-9 (deferred to gestalt / other locks):**
the 9A⊗9B verdict (→ LOCK-6, X-1), the canonical movemask form (→ LOCK-9∩LOCK-5, X-2), the
prefilter double-home (→ GN-2/PT-1 dispatch law, X-3), 9C's grammar→DFA source-of-truth (→ LOCK-5,
X-4), the 9C/9D resumable-scanner collapse (→ gestalt-Ω, X-5).

**One-line verdict:** the substrate map is honest and the two probes are real, but pass-1
systematically measured the corners that avoid each family's load-bearing claim — 9A's movemask,
9B's rebirth, 9C's wide-class leaf, 9D's backtrack bound — so no family is closer to convergence
than its *un-run* gate, and the lock's central axis is structurally a LOCK-6 verdict LOCK-9 cannot
render alone. Advance all four to pass-2 with the un-run gates promoted to born-RED must-run
status; do not let elegance stand in for the deferred measurement.
