# LOCK-9 pass-2 — CRIT-1 (Adversarial Critic 1 of 2 · WITHIN-LOCK weight)

*Charter: internal soundness of each family spec, and whether the pass-2 prototypes tested the
SHARPEST uncertainty — not a peripheral one. Substrate re-verified this session against the live
trees. Failure-mode checklist (vacuous convergence, spec-cites-itself, gates-that-cannot-fail, the
elegant-reduction trap, legacy aliases, masked fallbacks, unverified gestalt, consumer-less
substrate) applied per family; constitution clauses (i–ix) checked; prototype-evidence misuse
audited (SPEC_ONLY anchoring a perf claim). Convergence %s are earned-fraction estimates with exact
enumerated gaps — a family cannot exceed the fraction of its own deciding gate-set that ran and
passed. This pass folds PROTO-1 (9A live inline) and PROTO-2 (9B LEAN/DENSE rebirth) against the
pass-1 registry and CRIT-1/CRIT-2.*

---

## Substrate re-verification (independently confirmed this session)

- `parse-that/rust/parse_that/src/parsers/scan/number_simd.rs:54-55` — `vaddv_u8` ×2 (`lo_sum`,
  `hi_sum`) **live**. ✓ (9B's cheapest-win target and a live §0/`vaddv` refutation-law violation.)
- `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:83-84` — `vaddv_u8` ×2 **live in the shared
  home**. ✓ **R-A3 CONFIRMED: the refuted double-`vaddv` idiom lives inside bbnf-simd itself** — 9A
  inherits a refutation-law violation unless the purge (spec's "producer loci to purge") is
  mandatory-blocking, not advisory. (Spec cites `:79-87`; the `vaddv` lines are `:83-84`.)
- **classify_stripe is OFF the JSON hot path — CONFIRMED.** `classify_stripe` callers:
  `structural_bitmap.rs` internals + `ws_comment.rs:179`. `parsers/json.rs` contains **zero**
  `structural`/`classify`/`NibbleBitmap`/`find_next` references. `scan_ws_block_comments` (which
  reaches `classify_stripe`) is called only from `css/declaration.rs` and `css/media.rs`. So
  PROTO-1's material correction stands: **the scanner 9A's live-inline probe wired is a CSS-only
  scanner, and the JSON corpus the brief named never executes it.** ✓

The substrate floor is honest. As in pass-1, the defects are in **prototype-uncertainty targeting**
and **gates that under-catch / a gate now known un-satisfiable**, not in the paths:lines.

---

## LOCK-WIDE FINDING (weighs every family below) — the pass-2 probes narrowed the axis to a WASH

The pass-2 directive was correct: run the 9A⊗9B axis-decider. Both probes ran. But **their combined
result changes the character of the adjudication in a way the FAMILY-SPECS has not absorbed** (the
spec was authored 09:52, PROTO-1/2 landed 10:09-10:10):

1. **PROTO-1 falsified 9A's motivating lever.** 9A's Mechanism paragraph asserts the OnceLock
   fn-pointer facade is "an indirect branch that survives fat-LTO in-loop and blocks `#[inline]`."
   PROTO-1's CONTROL (facade retained) produced **byte-identical, `blr`-free** code at
   `codegen-units=1`: LLVM devirtualized the facade entirely. **At the production build config the
   de-face buys inlining-PARITY, not a win.** The spec is now internally inconsistent with its own
   probe on its central "Precondition."
2. **PROTO-2 showed the novel producers do not exist in bbnf-simd.** The LEAN nibble-`tz` and DENSE
   `vpaddq` tree "do not exist in bbnf-simd at all" (grep: `vpaddq` in **zero** bbnf-simd files) —
   so **whichever home wins, these producers must be authored once regardless.** PROTO-2 states this
   itself: "adopting 9A would not let parse-that inherit these producers for free."

**Consequence the gestalt must carry: the 9A vs 9B decision is not a performance decision — the two
routes wash.** 9A's inline advantage over OnceLock is vacuous at cgu=1; 9B's substrate-divergence is
driven by producers that must be written once anywhere. The axis therefore reduces to a pure LOCK-6
**maintenance-cost** call ("one crate or two for identical new code"), exactly LOCK-6D's null
hypothesis — but with **less performance daylight than either family's spec implies**. The
FAMILY-SPECS still frames "9A must earn its place by producing the two datums" as if the datums
would break the tie on merit; the datums that ran suggest the tie breaks on drift/maintenance cost
alone. **This is a refinement of the incompatibility map, not a contradiction — but it is the lock's
headline gestalt input and is currently unstated.**

A methodological caveat that weakens BOTH probes' generalization: PROTO-1 built the fat-LTO rlib
(bitcode, `otool` sees nothing) then took its disassembly from a **separate `-C lto=no
-C codegen-units=1` `--emit asm` codegen**. Both the "inline holds" and "control doesn't regress"
findings are therefore measured at `lto=no,cgu=1` and **generalized** to the shipping fat-LTO config,
not measured there. The direction is almost certainly right (cgu=1 devirt dominates), but neither arm
was measured under the config the seam would ship at.

---

## 9A — ONE-KERNEL-LIBRARY · convergence **52%** (was 55%; net-flat-to-down)

**What advanced.** PROTO-1 **ran** on a live wired cross-crate scanner and measured the physical
promise: the de-faced bbnf-simd kernel inlines across the `parse_that`↔`bbnf-simd` rlib boundary
with **zero in-loop indirect branch**. The pass-1 born-RED-inferred inline claim (ICF in an isolated
5-fn probe crate) is now measured on a real scanner. That is genuine — the "physical isomorphism,
one source, inlines across the boundary" thesis holds live.

**Why it did not net-advance.** The gap count did not shrink; one gap closed, two sharpened:

- **G1 — the necessity lever is FALSIFIED at the production config (gate-that-cannot-fail, inverted).**
  FAMILY-SPECS gate 1 requires "zero indirect-branch-in-loop with the de-faced entry AND the
  OnceLock-faced entry **demonstrably regressing to `blr` in-loop**." PROTO-1 proved the second
  conjunct **cannot hold at cgu=1** — the control did not regress. As written the gate is now known
  un-satisfiable in production, which inverts to: **9A's motivating problem (the OnceLock indirect-
  branch tax) is unprovable on real scanners at the shipping build config, and is plausibly a pass-1
  probe-crate artifact.** The de-face is harmless but not shown load-bearing. The gate must be
  re-homed to `cgu>1` or a non-inlinable (extern/dylib) boundary — but production IS cgu=1, so the
  necessity is moot precisely where it would ship.

- **G2 — the axis-decider PERF datum (gate 2, paired no-regression ≥0.98× on a live consumer) is
  STILL zero-measured, and the probe wired the wrong surface.** PROTO-1 rewired `classify_stripe`
  (changing its signature from nibble-LUT `u8x16` to `&[u8;256]` byte-class table — a different
  kernel shape than the incumbent), but `classify_stripe` is **CSS-only** (confirmed above). The
  named JSON corpus never runs it, so the paired ratio was correctly reported UNMEASURED. **9A has
  delivered HALF its LOCK-6 datum (inline GREEN) and NONE of the other half (no-regress). The perf
  half of the axis-decider — 9A's actual bet — remains untested after two passes.** The correct
  probe is a paired cold CSS bench (`parse_that_css → scan_ws_block_comments → classify_stripe`),
  N≥200, two windows — never named or run.

- **G3 — the seam actually tested was the FALLBACK, not the primary.** The spec's physical seam is
  "one line in the patch table (path-dep primary, the memchr model; vendor-under-canonical-diff only
  if a standalone build is a hard requirement)." PROTO-1 **vendored** bbnf-simd (standalone manifest,
  workspace flattened, dev-deps dropped) to honor P23. So the "one-line path-dep" seam — 9A's
  headline integration claim — remains **untested**; the heavier vendor route was exercised instead.

- **G4 — the `vector/{mod,neon,scalar}.rs` module (Q5) is still SPEC-ONLY.** "The refuted 276-LOC
  `simd128.rs` promoted minus double-`vaddv`" does not exist. It is the bulk of 9A's V-tier surface
  (decode store_chunk, ws_comment, scanners, span_parser/scan) and remains unbuilt; promoting refuted
  code demands its own no-regression artifact, which neither probe produced.

- **G5 — the refuted producer in the shared home (R-A3) makes the purge blocking, not advisory.**
  My grep confirms `byte_class_from_eq_set_64.rs:83-84` carries the double-`vaddv` PROVE-09 killed.
  Under §0 (iv) a shared kernel home that ships a refutation-law violation is non-compliant. 9A's
  gate set lists the purge as "producer loci to purge"; it must be a **born-RED blocking gate**, or
  9A ships the refuted idiom to parse-that.

- **G6 — Q2 (instruction-count) partially answered by PROTO-2, not by 9A's own probe;** Q3
  granularity (`PaddedView`-64 vs `eob_pad_clamp`; 64-byte-block kernels vs 16-byte-lane parse-that
  compositions) and the path-dep-vs-vendor cost artifact remain unexercised.

**Verdict.** ADVANCED born-RED is the right disposition; the physical-inline promise is now measured.
But convergence is capped and slightly reduced: the motivating lever is falsified at the production
config, the perf half of the axis-decider is still zero-measured on a mis-targeted (CSS-only)
surface, the primary seam was not the one tested, and the refuted-in-home producer needs a blocking
purge gate. **52%.**

---

## 9B — NATIVE-REBIRTH · convergence **62%** (was 55%; genuine advance)

**What advanced.** PROTO-2 is the strongest artifact in the lock. It moved 9B from SPEC-ONLY to
measured on three fronts, all with gates that could fail: (1) **LEAN win banked** — `number_simd`
double-`vaddv` → `vshrn_n_u16::<4>` emits `shrn.8b v0,v0,#0x4` with **zero `vaddv`**, 14 vs 19 insns
(−26%); the 2271-vs-509 defect does not reproduce; on a real JSON number path (my grep confirms
`number.rs:168,242`). (2) **DENSE win measured** — `vpaddq` bit-weight tree at 50 vs 64 insns
(−22%, lowers to `uzp`/`orr`, zero reduction ops). (3) **Paired ≥0.98× RAN** — 15/16 median+fastest
ratios PASS, the one sub-threshold a 13 ns `fastest` quantization whose median is 1.025. Pass-1
CRIT-1's G3 (bank the cheapest win) is CLOSED.

**Open gaps (enumerated):**

- **G1 — gate 5 (the axis-decider) is SEEDED, not closed, and its inductive base is weak.** PROTO-2
  measured 2 of ~14 kernels: ~2 LOC (LEAN) / 0 LOC (DENSE, novel) shared with bbnf-simd → "near-total
  divergence." But the divergence is near-total **because bbnf-simd lacks the `vpaddq` producer** —
  which (per the lock-wide finding) must be authored once regardless of home. So "near-total
  substrate divergence justifies two homes" **over-reads its own datum**: it conflates "these
  producers are new" with "these producers must be duplicated." The merged-surface count across all
  LEAN/DENSE sites — the number that actually decides the axis — is still owed. Gate 5's verdict
  remains open.

- **G2 — the DENSE rebirth's correctness rests on "parsed without panic," not a differential
  mask-equality oracle.** PROTO-2 reports "functional parity" (all 4 JSON parsed without panic) for
  `classify_stripe_64` → `vpaddq` tree. That is **not bit-parity of the mask**. Q2's self-regression
  risk (a naive producer on a DENSE site corrupts the 256-bit parity) was avoided by using idiom (b),
  but the vpaddq tree's output was never checked bit-for-bit against the scalar/`to_bitmask`
  baseline. A subtly-wrong mask that still parses these four corpora correctly would pass PROTO-2's
  check. Gate 3 (native checkasm-liveness with `INJECT_BUG` ≥90%, alignment sweep, `black_box`
  inputs) is the oracle that would catch it and it is **unbuilt** — "the harness exists to port" is
  still a plan, not an artifact.

- **G3 — 9B never left nightly; its headline claim is unverified.** PROTO-2 ran on
  `nightly-2026-04-11` (the parse-that pin). Gate 6 (`cargo +stable build`, `portable_simd` deleted)
  and Q5 (the `smallbox[nightly]` → stable `smallbox!` cutover, "blast radius asserted bounded, not
  shown") were **not exercised**. 9B's central promise — stable NEON, off nightly — remains
  untested; the rebirths were authored under the nightly toolchain they claim to shed.

- **G4 — a perf-narrative over-attribution (SPEC_ONLY-ish causal claim).** PROTO-2 reads the
  combinator medians (canada/citm/twitter 1.02-1.03×) as "consistent with the DENSE 50-vs-64 insn
  win." With the host saturated (loadavg 16.86 kickoff → 9.1-13.5 across windows, shared, not
  isolated) and no per-kernel isolation of `classify_stripe_64`'s contribution, that causal link is
  asserted, not demonstrated. The **gate verdict (≥0.98×) is sound**; the "the DENSE win causes a
  2-3% whole-parse speedup" narrative is not isolated and should be read only as PASS, not as a
  speed claim (§0: no stable-port perf claim, "faster" appears in no gate).

- **G5 — Q2/Q4 residue.** Q4 (is `first_match` materially leaner than bbnf-simd's repack) is largely
  answered by the disasm table (LEAN 14 vs bbnf-simd movemask 28 to u16; DENSE 50 vs a bbnf-simd-
  style 4×repack ≥64) — but the LEAN 28-vs-14 is cross-contract (u16 vs count), not apples-to-apples;
  the clean comparison is DENSE-to-DENSE, and there bbnf-simd has no equivalent to compare against.

**Verdict.** ADVANCED-incumbent, materially advanced. Two of five pass-1 gaps closed (cheapest win,
DENSE insn-count), gate 5 seeded. Remaining: the gate-5 merged-surface count (axis-decider), a
bit-parity oracle for DENSE (checkasm unbuilt), and the entire stable-toolchain cutover untested.
**62%.**

---

## 9C — SCANNER-AS-DFA · convergence **44%** (BLOCKED; held, no pass-2 probe)

**What is earned.** The pass-2 spec matured the analysis well: the M/S/X tiering is a genuine
sharpening (Sheng retired as a **measured** negative from pass-1's census — the pass-1 "novum"
correctly demoted), and the tier boundaries are honestly drawn (Tier X unicode-explosion excluded as
a correctness floor, not hidden). The cross-lock blocker is correctly held.

**Open gaps (enumerated):**

- **G1 — "Tier-M parity by construction (Q1 answered)" is a SPEC_ONLY claim dressed as answered
  (spec-cites-itself circularity).** The spec marks Q1 "answered / parity by construction" because
  "the hand LUTs already ARE hand-computed `byte_classes` tables." But the **emit pass
  (`[u8;256]` → SIMD classifier) does not exist** (spec's own "core gap, R6") and the **separability
  solver is unbuilt** — so the generated classifier whose parity is asserted **cannot yet be
  produced**. Parity is claimed between a hand table and a not-yet-emitted classifier: the thing
  whose parity is asserted isn't built. Gate 1 (compile `(?:\s|/)` + the structural set through
  `equiv.rs`, emit, `otool` vs the hand scanner at ≤1.0× reduction-insn) was **not run**. This is
  the exact prototype-evidence-misuse pattern: an unmeasured, currently-unfalsifiable claim carried
  as "answered."

- **G2 — the load-bearing separability example is factually mis-stated.** The spec's lo6-collision
  case reads "`;`=0x3B ≡ `{`=0x3B." `{` is **0x7B**, not 0x3B; the collision is that lo6(0x3B)=0x3B
  and lo6(0x7B)=0x3B. A slip in the one example that motivates the `vceqq`-fan third form — minor,
  but it is the datum that proves the separability solver is load-bearing, so it should be exact.

- **G3 — the actual BLOCKER (Q2, cross-lock) is untouched and unbuildable in LOCK-9.** The
  grammar→DFA→byte-fixpoint source-of-truth at scanner fidelity is owned by LOCK-5 and unbuilt, and
  is contested between **two** crates (parse-that's in-repo `bbnf-regex = { path = "regex" }` fork,
  which any 9C census actually compiles through, vs bbnf-lang's `crates/bbnf-regex`). The
  missing-primitive test lands here: substrate owned elsewhere and unbuilt. BLOCKED is correct.

- **G4 — the separability solver (bounded CSP over a 256-entry partition, three known forms) is the
  family's new code and is unbuilt;** its "tractable" label is an argument, not an artifact. Gate 4
  (a naive nibble-only emit MUST misclassify the structural set) is the load-bearing proof and was
  not run.

- **G5 — imported CONFIRMED labels (as pass-1).** "Survives degraded (Tier M + prefilter Tiers
  0/1/3, CONFIRMED art)" imports confirmations earned in bbnf-lang's regex/emit context into
  parse-that's scanner plane; an in-plane parity check is owed before the degraded-partial is banked.

**Verdict.** BLOCKED correct. No pass-2 probe (the two probe slots correctly went to the axis-
decider). Convergence roughly flat — the tiering analysis matured but no new in-plane evidence, and
one claimed-answered gate (Tier-M parity) is actually unbuilt+unmeasured. **44%.**

---

## 9D — LAZY-SURFACE · convergence **30%** (BLOCKED; held, no pass-2 probe)

**What is earned.** The M1 (windowed scan over a resident buffer, exists) / M2 (streaming input,
does not exist) separation is a real analytical advance, and the consumer-absence is now
**grep-measured** (`fn feed|push_bytes|Decoder|BufRead|document_stream|from_reader` → zero), not
asserted — a genuine hardening of the Y.13 finding over pass-1. The self-diagnosis remains the most
honest in the set.

**Open gaps (enumerated):**

- **G1 — P3 (the cheap, deterministic, self-described decisive gate) is UNRUN for the SECOND
  consecutive pass.** The backtrack-distance histogram (`furthest_offset − offset` at the three
  restore sites) is the measurement the family's entire memory raison d'être rests on. It is
  self-described "mandatory, cheap, deterministic" and was skipped in pass-1 AND pass-2. **Two passes
  of non-execution of the one gating measurement is itself evidence** — the family cannot be closed
  on the current trajectory; it should be closed with a measured refutation (file the histogram) or
  explicitly parked, not carried at a rising %. (research-D delivered only the instrumentation spec,
  again.)

- **G2 — the central claim is SPEC-ONLY and self-diagnosed as a missing primitive of equal
  difficulty.** "A bounded backtrack guarantee PEG semantics may simply not provide — a property
  equal in difficulty to redesigning the backtracking discipline itself." By the family's own words
  this is the missing-primitive test, failing by default until P3 measures otherwise. Correctly
  BLOCKED.

- **G3 — live Y.13 violation persists in the design (now grep-confirmed).** The `Window`/M2 plane is
  consumer-less substrate: no streaming consumer exists, and the shipped grammars build whole owned
  trees (twitter.json = 5390 allocs dominate RSS). A streaming *input* surface removes one padded
  copy while the output tree — the actual RSS mass — is untouched; so even a successful M2 delivers
  no peak-RSS win without a streaming *output* consumer that does not exist. "Consumer-less substrate
  is forbidden — in designs as in code."

- **G4 — the `lazy.rs` name-collision (combinator-graph laziness vs input-window laziness) is named,
  not resolved.** Architectural debt left open across two passes.

- **G5 — zero-throughput-claim honesty is correct but total;** latency/TTFS is the only survivable
  value and is itself consumer-gated, and P2 (peak-RSS + TTFS harness) that would prove even that
  was not run.

**Verdict.** BLOCKED correct. Convergence flat — the M1/M2 split and grep-measured consumer-absence
sharpen the BLOCKED verdict (they harden the refutation), but the decisive gate was skipped a second
time, so nothing on the deciding question advanced. **30%.**

---

## Did the two pass-3 briefs test the sharpest uncertainty? (critic-1 charter judgment)

**BRIEF 1 (9A live inline) — tested the right axis-half, wired the wrong surface, and its result
inverts a gate.** It correctly targeted 9A's untested bet (live cross-crate inline). But (a) it wired
a **CSS-only** scanner (`classify_stripe`) and then could not run the perf clause on the named JSON
corpus — the perf half of the axis-decider is still zero-measured; (b) it used the **vendor** seam,
not the "one-line path-dep" the spec calls primary; (c) it **falsified 9A's necessity lever** — a
real, load-bearing finding, but it means the probe validated the physical inline while demonstrating
9A buys parity, not a win, at production cgu=1. Net: it half-answered the sharpest 9A uncertainty and
opened a sharper one (is 9A's motivating problem an artifact?).

**BRIEF 2 (9B LEAN/DENSE rebirth) — cleanly hit the sharpest 9B uncertainties.** It banked the
cheapest win (LEAN), settled the owed DENSE instruction-count datum (WIN), and produced gate 5's
first seed. Its one miss against its own charter: it verified the DENSE rebirth only by no-panic, not
by the bit-parity oracle that the family's Q2 self-regression risk demands, and it stayed on nightly
(9B's headline is stable-toolchain). Strong probe; the residue is correctness-oracle and toolchain,
not targeting.

**The two probes together delivered the lock-wide finding above (the axis is a performance wash) —
which neither brief anticipated but which is the single most consequential pass-2 result, and is
currently un-absorbed by the FAMILY-SPECS.**

---

## Constitution & failure-mode roll-up (critic-1 view)

- **(i) grammar totality:** 9A/9B keep authored scanners; the spec relies on the "save primitives and
  combinators" exemption to reconcile this with clause (i) but never argues that hand scanners fall
  under it — an implicit assumption carried since pass-1, still unstated. 9C is the family that would
  satisfy (i) and is BLOCKED; the gestalt must name the constitution-prefers-9C tension explicitly.
- **(iv) refutation laws:** well carried in gate text; PROTO-2 correctly banks the double-`vaddv`
  removal (in scratch). **But R-A3 is CONFIRMED live** (`byte_class_from_eq_set_64.rs:83-84`): the
  refuted idiom lives in the shared home 9A adopts — the purge must be a blocking gate (9A-G5).
- **(vii) concrete-artifacts-or-SPEC-ONLY:** 9A now has an artifact but on a mis-targeted perf
  surface (perf half still SPEC-ONLY); 9B has a strong artifact; 9C's Tier-M "parity by construction"
  is SPEC_ONLY dressed as answered (9C-G1); 9D's P3 is SPEC-ONLY for a second pass.
- **(viii) consumer-less substrate:** 9D trips it in-design, now grep-confirmed (9D-G3); 9A's
  `vector/` module has consumers but is unbuilt (9A-G4).
- **Gate-that-cannot-fail (inverted form):** 9A gate 1's second conjunct is now known
  **un-satisfiable at production cgu=1** (9A-G1) — the sharpest single finding: 9A's necessity is
  unprovable where it would ship.
- **Elegant-reduction trap:** 9A's "known refactor plus a build-config proof" (Why-not-BLOCKED) —
  the build-config proof came back showing the refactor is unnecessary at cgu=1; the load-bearing
  "and-then" (perf no-regress on the correct live consumer) is still unconverged.
- **Unverified gestalt:** the FAMILY-SPECS' framing that the two datums break the 9A/9B tie on merit
  is contradicted by the probes (the axis washes on performance; the tie is a pure LOCK-6
  maintenance call). Absorb into the incompatibility map.
- **SPEC_ONLY anchoring a claim:** 9C Tier-M parity (9C-G1); 9B's DENSE-win causal narrative
  (9B-G4, gate verdict sound / causal claim not isolated); 9D memory case (9D-G1).

## Convergence summary

| family | disposition (held) | pass-1 → pass-2 | deciding gap |
|---|---|---:|---|
| 9A ONE-KERNEL-LIBRARY | ADVANCED born-RED | 55% → **52%** | necessity lever falsified at cgu=1; perf half of axis-decider still zero-measured on a CSS-only surface; primary (path-dep) seam untested |
| 9B NATIVE-REBIRTH | ADVANCED incumbent | 55% → **62%** | gate-5 merged-surface count owed; DENSE bit-parity oracle (checkasm) unbuilt; stable-toolchain cutover untested |
| 9C SCANNER-AS-DFA | BLOCKED | 45% → **44%** | Tier-M parity SPEC_ONLY (emit pass + separability solver unbuilt); Q3 LOCK-5 two-crate coupling unbuildable in LOCK-9 |
| 9D LAZY-SURFACE | BLOCKED | 35% → **30%** | P3 backtrack-bound histogram unrun for a 2nd pass; Y.13 tripped in-design (grep-confirmed) |

## Pass-3 directives (critic-1 priority order)

1. **Absorb the lock-wide finding into the FAMILY-SPECS/incompatibility map:** the 9A⊗9B axis washes
   on performance (PROTO-1 cgu=1 devirt + PROTO-2 producers-must-be-authored-once); it is a pure
   LOCK-6 maintenance-cost decision. Re-word "9A must earn its place by producing the two datums."
2. **Close 9A's perf half on the CORRECT surface:** paired cold `parse_that_css` (→
   `scan_ws_block_comments → classify_stripe`), N≥200, two windows — the JSON corpus never runs it.
   And test the **one-line path-dep** seam, not the vendor fallback.
3. **Make the R-A3 purge a blocking born-RED gate** (`byte_class_from_eq_set_64.rs:83-84` double-
   `vaddv` in the shared home) — 9A ships a refutation-law violation without it.
4. **Bank 9B's DENSE correctness with a bit-parity oracle** (not no-panic) and **run the stable-
   toolchain cutover** (gate 6 + smallbox) — 9B's headline is unverified on nightly.
5. **Run 9D-P3 or close 9D** with the measured refutation. A cheap gate skipped twice should not be
   carried a third pass at a live %.
6. **9C:** downgrade "Tier-M parity (Q1 answered)" to SPEC_ONLY until the emit pass + separability
   solver exist; fix the `{`=0x7B example.
