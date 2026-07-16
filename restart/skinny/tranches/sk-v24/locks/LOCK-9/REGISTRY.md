# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. The lock is not four features — it is four
dispositions of one aarch64 scanner/kernel plane on **three distinct axes**: **kernel-home** (9A one
shared crate ⊥ 9B parse-that keeps its own — the central adjudication), **scanner-provenance** (9C
derived-from-DFA vs authored {9A,9B}), **input-surface** (9D windowed vs whole-input — orthogonal,
contradicts the shared eager `PaddedView` invariant).*

**Live at pass 2.** Inputs folded: FAMILY-SPECS (pass-2 synthesis — **STALE against its own protos**,
authored 09:52 before PROTO-1/2 landed 10:09–10:10); research-{A,B,C,D,priorart} pass-2; PROTO-1 (9A
live cross-crate inline, RAN_MEASURED); PROTO-2 (9B LEAN/DENSE rebirth + canonical-producer disasm,
RAN_MEASURED); CRIT-1 (within-lock: 9A 52 / 9B 62 / 9C 44 / 9D 30); CRIT-2 (cross-lock: 9A 55 / 9B 62
/ 9C 43 / 9D 30). **The pass-2 headline the synthesis has not absorbed: the 9A⊗9B axis is a
PERFORMANCE WASH** — PROTO-1 falsified 9A's motivating lever (the OnceLock barrier is *free* at the
shipping `codegen-units=1`; de-face buys inline-parity, not a win) and PROTO-2 proved the canonical
producers both routes converge on **exist in bbnf-simd nowhere** (must be authored once regardless of
home). The axis therefore reduces to a **pure LOCK-6 maintenance-cost decision** ("one crate or two
for identical new code"), with *less* performance daylight than either family's spec implies. Two
pass-1 "EARNED · do-not-re-litigate" items were **overturned by pass-2's own probes** (see 9A §).
Substrate re-verified this session by the agglomerator: X-7 (bbnf-regex name+version collision) and
X-8a (9B has no OnceLock) both confirmed against the live trees.

---

## STATUS TABLE (after pass 2)

| Family | Status | Conv. | Δ p1→p2 | One-line |
|---|---|---:|:--:|---|
| **9A ONE-KERNEL-LIBRARY** | **ADVANCED · born-RED** | **52%** | 55→52 | Live cross-crate inline is now **measured-GREEN** (0 in-loop indirect across the real parse-that↔bbnf-simd rlib edge) — but **both headline merits FELL**: PROTO-1 falsified the OnceLock barrier (free at cgu=1), PROTO-2 refuted the "free `vshrn` movemask" (lawful producers absent from bbnf-simd; a *refuted* one lives there). The raison d'être in a wired consumer is still zero-measured (perf half untested for a 2nd pass, on a CSS-only surface), and the physical seam tested was the vendor fallback, not the one-line path-dep. |
| **9B NATIVE-REBIRTH** | **ADVANCED · incumbent** | **62%** | 55→62 | **The only family that moved.** PROTO-2 banked the cheapest win (LEAN `number_simd` double-`vaddv`→`shrn.8b`, 0 `vaddv`, 14≤19 insns) AND the DENSE win (`vpaddq` tree 50 vs 64, zero reduction ops), both paired ≥0.98× (15/16). Off SPEC-ONLY. Open: gate-5 seeded against the **un-evolved** shared crate (overstates the case), the checkasm bit-parity oracle unbuilt (correctness = "parsed without panic"), and the entire stable-toolchain cutover untested (rebirthed *on* the nightly it claims to shed). |
| **9C SCANNER-AS-DFA** | **BLOCKED** | **43%** | 44→43 | No pass-2 probe (both went to the axis, 2nd pass running). FAMILY-SPECS matured the M/S/X tiering — but it **de-escalates**: Sheng (pass-1's novum) is now a **measured negative** (Tier-S `vqtbl1q` serial stall loses to scalar `find_at` on the short runs these DFAs see), shrinking the family to Tier-M parity + deletion. The separability-solver core is unbuilt (elegant-reduction trap); the blocker is a **name+version collision** (`bbnf-regex 0.1.0` resolves to two sources) and pass-1's cited path was wrong. |
| **9D LAZY-SURFACE** | **BLOCKED** | **30%** | 30→30 | No pass-2 probe. **P3 — the cheap, deterministic, born-RED decider — unrun for the SECOND consecutive pass.** M1/M2 split is a real analytical advance and the consumer-absence is now **grep-measured** (zero `feed`/`document_stream`/`from_reader`), hardening the Y.13 refutation. SPEC-ONLY after two passes. Memory center refuted-by-default; latency/TTFS survives but is consumer-gated. **Last pass carried at a live %: pass-3 runs P3 or RETIRES.** |

Net: **no family converged; the central axis (9A⊗9B) is convergent-in-mechanism but its decider is
now shown to be a PURE LOCK-6 MAINTENANCE CALL (performance washes).** For the second consecutive
pass, both probes landed on the central axis — **four probes across two passes have all hit 9A/9B; the
two BLOCKED families have never been probed**, so 9C/9D carry zero new measured evidence and cannot
rise off their floors. The lock's single most consequential pass-2 result (the axis is a perf wash) is
still un-absorbed by the FAMILY-SPECS and must be reconciled into the incompatibility map before
gestalt.

---

## 9A — ONE-KERNEL-LIBRARY · ADVANCED · born-RED · 52%

**Floor rationale.** CRIT-1 52 / CRIT-2 55. Take **52** (CRIT-1's, the lower). CRIT-2's 55 sits on its
own pass-1 scale (58→55) and credits the measured-GREEN live inline; CRIT-1's 52 is the honest
composition change — **one gap closed, two sharpened**: the born-RED-inferred inline is now measured
live (real advance), but the *necessity lever is falsified at the production config*, the *perf half of
the axis-decider is still zero-measured* (and on a mis-targeted CSS-only surface), and the *primary
path-dep seam was never tested* (the heavier vendor route was). No reason CRIT-1 erred low: the two
headline merits both fell to pass-2's own probes; 9A is now honestly "physical-iso-only, and must first
*evolve + clean* the shared crate before it inherits anything." ADVANCED born-RED remains correct (the
physical-inline promise is measured; no missing primitive of equal difficulty).

**OVERTURNED this pass (pass-1 "EARNED · do-not-re-litigate" items falsified by pass-2 probes — DELETE
from the earned set).**
- **[OVERTURNED] EARNED-item-1 "the OnceLock facade is a hard devirtualization barrier fat-LTO cannot
  pierce → ~0.91–0.93×."** PROTO-1's CONTROL (facade retained, `defaced` off, `features=[]` verified)
  emitted **byte-identical, `blr`-free** code at `codegen-units=1`; the standalone `bbnf_simd` object
  carried **no `OnceLock`/`get_or_init`/`primitive_kernels` symbol at all** — LLVM devirtualized the
  facade entirely. The 0.92× barrier was a **pass-1 probe-crate / multi-CGU artifact**. Production IS
  cgu=1 → **the de-face buys inline-PARITY, not a win**; the necessity is moot precisely where it would
  ship. (Gate 1's second conjunct — "OnceLock arm demonstrably regressing to `blr` in-loop" — is now
  known **un-satisfiable at production cgu=1**; a gate-that-cannot-fail, inverted.)
- **[OVERTURNED] EARNED-item-2 "9B would inherit the identical OnceLock constraint intra-crate."**
  VERIFIED FALSE this session: parse-that's scan files contain **zero** `OnceLock`/`get_or_init`/
  `primitive_kernels`/`dispatch::` (the facade is bbnf-simd-internal, `dispatch.rs:58-60`); 9B keeps
  inline `portable_simd` kernels with no facade to de-face. **The de-face is a 9A-ONLY cost** — it
  should count *against* 9A on the axis, not be neutralized as "both inherit it" (CRIT-2 X-8a).
- **[REFUTED, was "contested"] The "free `vshrn` movemask" (pass-1 gap 2).** PROTO-2 + grep: the two
  *lawful* producers (LEAN nibble-`tz`, DENSE `vpaddq` tree) exist in **bbnf-simd nowhere** (`vpaddq`
  in ZERO files, agglomerator-confirmed), while a **refuted** producer (double-`vaddv`,
  `byte_class_from_eq_set_64.rs:79-87`) DOES live there. Adopting 9A is therefore **not** "inherit good
  kernels for free" — it is "**author** the canonical producers *into* bbnf-simd **and** purge the
  refuted double-`vaddv` there first." 9A's central merit sentence is overstated.

**What is STILL EARNED (do not re-litigate).** (1) **Live cross-crate inline is measured-GREEN.** The
de-faced bbnf-simd kernel inlines across the real `parse_that↔bbnf-simd` rlib boundary into a live
scanner loop (`scan_ws_bitmap_cold`, 450 asm lines) with **0 in-loop `bl`/`blr`** — the "one source,
inlines across the boundary" physical-iso thesis holds live. (2) The physical seam is one line:
`.cargo/config.toml [patch.crates-io]` path-deps bbnf/bbnf-regex/csp-solver; `bbnf-simd` is absent
(VERIFIED). (3) The refuted double-`vaddv` **lives in the shared home** (R-A3,
`byte_class_from_eq_set_64.rs:79-87`, VERIFIED) — a refutation-law violation 9A imports unless purged.

**Exact open gaps.**
1. **[raison d'être, un-measured for a 2nd pass, now CONTRADICTED] the movemask reroute as a wired 9A
   consumer.** PROTO-2 measured the producers for **9B's own-source rebirth**, not for a bbnf-simd
   consumer — and found them absent from bbnf-simd. 9A's headline win is not merely unmeasured, it is
   *contradicted*: nothing to inherit for free.
2. **[perf half of the axis-decider, zero-measured 2nd pass, WRONG SURFACE] gate 2 (paired ≥0.98× on a
   live consumer).** PROTO-1 rewired `classify_stripe`, which is **CSS-only** (VERIFIED:
   `parsers/json.rs` has zero `structural`/`classify` refs; the only live callers are
   `scan_ws_block_comments` from `css/declaration.rs`+`css/media.rs`). The commissioned JSON corpus
   never runs it → the ratio was correctly UNMEASURED. The correct probe is a paired cold **CSS** bench
   (`parse_that_css → scan_ws_block_comments → classify_stripe`), N≥200, two windows — never run.
3. **[R-A3 purge must be BLOCKING, not advisory]** a shared kernel home that ships the double-`vaddv`
   the refutation law forbids is non-compliant (§0 iv). 9A's gate set lists the purge as "producer loci
   to purge"; it must be a **born-RED blocking gate** or 9A ships the refuted idiom to parse-that.
4. **[wrong seam tested] the primary integration seam (one-line path-dep) is untested.** PROTO-1
   **vendored** bbnf-simd (standalone manifest, workspace flattened) to honor P23. The "one-line
   path-dep" headline seam — the memchr model — remains untested; the heavier vendor route was exercised.
5. **[gate-5 baseline is wrong-way biased AGAINST 9A]** PROTO-2's "near-total substrate divergence" was
   measured against **un-evolved** bbnf-simd (which lacks the canonical producers), manufacturing
   divergence 9A would *erase*. The honest 9A-side axis input needs gate-5 re-measured after the shared
   home adopts the producers (CRIT-2 X-8b).
6. **[SPEC-ONLY] the `vector/{mod,neon,scalar}.rs` module does not exist** (VERIFIED absent) — the bulk
   of 9A's V-tier surface, "the refuted 276-LOC `simd128.rs` promoted minus double-`vaddv`"; promoting
   refuted code demands its own no-regression artifact.
7. **[cross-lock] P23 supersession named-not-granted** — 9A's one added line mutates parse-that's
   read-only `.cargo/config.toml`. **[cross-lock] the axis close is LOCK-6's** (see DAG).

**Pass-2 disposition on the crux.** 9A's core physical promise (inline across the boundary) is proven
live; its *value proposition* is now purely maintenance/drift-avoidance, **not** a perf win — the
performance daylight both families' specs implied is gone. This is exactly the LOCK-6 maintenance-cost
call, now with the perf tie broken to "wash."

**Pass-3 directives.**
- *Prototype (close the perf half on the CORRECT surface):* paired cold **`parse_that_css`** (→
  `scan_ws_block_comments → classify_stripe`), N≥200, two windows — the JSON corpus is a null. AND test
  the **one-line path-dep** seam under fat-LTO cgu=1 (not the vendor fallback). This finally delivers
  9A's gate-2 datum after two empty passes.
- *Prototype (R-A3 blocking gate):* purge `byte_class_from_eq_set_64.rs:79-87`'s double-`vaddv` and
  make the no-regression proof a **blocking born-RED gate** — 9A ships a refutation-law violation
  without it.
- *Prototype (correct the axis baseline):* author the canonical producers into a scratch bbnf-simd,
  then re-measure gate-5 divergence against the **evolved** home — the honest 9A-side LOCK-6 input
  (erases the manufactured divergence).
- *Research must answer:* the mapping census (each of the 8 scanner files: K 1:1-to-kernel | V
  vector-tier | NO_GO) quantifying the kernel-tier/vector-tier split — still not produced. Does fat-LTO
  carry `#[inline]` across the seam under `cgu>1` or a non-inlinable boundary (where the OnceLock lever
  would actually appear)?
- *Withheld from researchers:* do **not** seed "the free `vshrn` movemask" (REFUTED — absent from
  bbnf-simd) or "the OnceLock barrier costs 0.92×" (OVERTURNED — free at cgu=1) or "9B inherits the
  same OnceLock constraint" (FALSE — 9B has no facade). Hand them only: live inline is measured-GREEN;
  the de-face is 9A-only hygiene not a perf lever; the canonical producers must be authored into
  bbnf-simd regardless; the perf axis washes. Do not seed P23 supersession as granted.

---

## 9B — NATIVE-REBIRTH · ADVANCED · incumbent · 62%

**Floor rationale.** CRIT-1 62 / CRIT-2 62. Take **62** — both agree, and 9B is the only family that
moved. PROTO-2 is the strongest artifact in the lock: two of five pass-1 gaps closed with gates that
could fail, gate 5 seeded. No reason to go lower than the concurrent floor; no reason to go higher —
the axis-deciding gate 5 is *seeded not closed*, the DENSE correctness rests on "parsed without panic"
not a bit-parity oracle, and the headline (stable, off-nightly) is untested (rebirthed on nightly).

**What is EARNED (banked artifacts, do not re-litigate).**
- **[BANKED] LEAN cheapest win.** `number_simd.rs:54-55` double-`vaddv` → `vshrn_n_u16::<4>` emits
  `shrn.8b v0,v0,#0x4` with **zero `vaddv`**, **14 vs 19 insns (−26%)**; the 2271-vs-509 `addv.8b`
  defect does NOT reproduce; on a real JSON number path. Pass-1's "cheapest decisive artifact" directive
  is CLOSED, and a live refutation-law violation is demonstrably fixable (rebirthed in scratch, P23-correct).
- **[BANKED] DENSE win (the owed instruction-count datum).** `classify_stripe_64` simdjson `vpaddq`
  bit-weight tree = **50 vs 64 insns (−22%), ZERO reduction ops** — LLVM lowers the disjoint-bit tree to
  `uzp1/uzp2/orr` (pairwise-add ≡ pairwise-OR under disjoint bits; cheaper port), obviating *both* the
  per-16 repack and the scalar shift-OR combine. **The DENSE site should be rebirthed too** (a valid
  positive where the family flagged a possible loss as acceptable).
- **[BANKED] paired ≥0.98×.** 15/16 median+fastest ratios PASS; the lone sub-threshold is a 13 ns
  `fastest` quantization whose median is 1.025.
- **Settled taxonomy:** (a) nibble-stride `vshrn_n_u16::<4>` for LEAN; (b) dense-64 `vpaddq` tree for
  DENSE; (c) FORBIDDEN double-`vaddv`. Stable `core::arch::aarch64` since 1.59 (no nightly); NEON
  baseline-mandatory. Correctly NOT BLOCKED.

**Exact open gaps.**
1. **[axis-decider, SEEDED not closed, WRONG baseline] gate 5 (LOC/substrate divergence).** PROTO-2
   measured 2 of ~14 kernels (~2 LOC LEAN / 0 LOC DENSE shared → "near-total divergence") — but the
   divergence is near-total **because bbnf-simd lacks the `vpaddq` producer**, which must be authored
   once regardless of home. So "near-total divergence justifies two homes" **over-reads its own datum**
   (conflates "these producers are new" with "they must be duplicated"). The **merged-surface count
   across all LEAN/DENSE sites after the shared home adopts the producers** — the number that actually
   decides the axis — is still owed. This is the corrected LOCK-6 input.
2. **[DENSE correctness is no-panic, not a bit-parity oracle] gate 3 (native checkasm-liveness).**
   PROTO-2 verified the `vpaddq` tree only by "all 4 JSON parsed without panic" — **not** bit-parity of
   the mask against the scalar/`to_bitmask` baseline. A subtly-wrong mask that still parses these four
   corpora would pass. The checkasm harness (`INJECT_BUG` ≥90%, alignment sweep, `black_box` inputs) is
   the oracle and remains **a plan, not an artifact**.
3. **[headline UNVERIFIED — never left nightly] gate 6 (stable-toolchain cutover).** PROTO-2 ran on
   `nightly-2026-04-11`. `cargo +stable build` / `portable_simd` deletion and the `smallbox[nightly]`→
   stable `smallbox!` cutover (blast radius asserted bounded, not shown) were not exercised. 9B's central
   promise — stable NEON, off nightly — is untested; the rebirths were authored under the toolchain they
   claim to shed.
4. **[Q2 negative un-demonstrated] the self-regression guard.** Naive nibble-stride `vshrn` on
   `classify_stripe_64` yields 256 bits and corrupts parity → MUST use idiom (b). PROTO-2 used idiom (b)
   (correct) but never demonstrated the *negative* (naive-nibble-corrupts) as the load-bearing guard.
5. **[perf narrative not isolated] the DENSE-win causal claim.** The combinator medians
   (canada/citm/twitter 1.02–1.03×) are read as "consistent with the DENSE 50-vs-64 win," but with the
   host saturated (loadavg 9–16, shared) and no per-kernel isolation, that is asserted. Read the gate as
   **PASS only**, not a speed claim (§0: "faster" in no gate).
6. **[cross-lock] the axis close is LOCK-6's** — 9B produced its half; it cannot self-declare.

**Pass-3 directives.**
- *Prototype (close gate 5 — the corrected axis-decider):* author the canonical producers into a
  scratch bbnf-simd (the evolved shared home), then produce the **merged-surface count across ALL
  LEAN/DENSE sites** + the substrate-divergence table (`PaddedView`-64 vs `eob_pad_clamp`; `vqtbl1q_u8`
  single-table vs `vqtbl4q_u8`) *as data*. This is the datum that defeats-or-concedes-to 9A; deliver it
  as LOCK-6's input, measured against the evolved home (not the un-evolved crate).
- *Prototype (bank DENSE correctness):* a bit-parity oracle for the `vpaddq` tree (differential vs the
  scalar/`to_bitmask` baseline), and **run the stable-toolchain cutover** (gate 6 + smallbox) — 9B's
  headline is unverified.
- *Research must answer:* post-evolution, is LOC/substrate divergence large enough to justify two homes,
  or do the kernels concede to 9A's near-identity? (Both outcomes valid; no home verdict.)
- *Withheld from researchers:* do **not** let "9B is the PT-2 incumbent, default to it" substitute for
  the gate-5 measurement, and do **not** carry the un-evolved-baseline divergence number as decisive (it
  overstates 9B's case). Zero "faster" claim in any gate. Hand them the banked LEAN/DENSE wins + the
  null-is-valid framing (9B concedes honestly if divergence measures small against the *evolved* home).

---

## 9C — SCANNER-AS-DFA · BLOCKED · 43%

**Floor rationale.** CRIT-1 44 / CRIT-2 43. Take **43** (CRIT-2's, the lower). No reason CRIT-2 erred:
its −1 correctly weights the 2nd-pass probe miss, the wrong-path citation (now corrected), and — most
tellingly — the **de-escalation**: Sheng (pass-1's "genuine novum," the vectorized DFA) is retired to a
**measured negative** this pass, shrinking the family to Tier-M parity + deletion. BLOCKED stands: the
family is cross-lock-coupled to LOCK-5 (unbuilt substrate) and LOCK-2 (tier island), its dominant-pass
value is parity-only, and its new code (the separability solver) is unbuilt.

**What is EARNED (do not re-litigate).** PROTO-2 (pass-1) closed **Q2 register budget** for the ASCII
archetype family (5/5 fit strict `vqtbl1q_u8`). The pass-2 M/S/X tiering is a genuine analytical
sharpening: **Tier M** (membership, dominant every-byte pass, lowers to simdjson classify-and-movemask);
**Tier S** (small multistate — the Sheng question, now answered NEGATIVE: `state[i]=tbl(T,class[i],
state[i-1])` is a loop-carried serial `vqtbl1q_u8` chain, ~2–4 cyc/byte unhidable, loses to scalar
`find_at` on the short runs these DFAs see — `ws_comment` 84–100% of WS runs ≤4 bytes); **Tier X**
(unicode explosion, `num_classes`>255 / states>4096, `dfa.rs:262` returns `None` — a correctness floor,
excluded as "not a scanner"). Tiers 0/1/3 prefilter art is prior-confirmed (PROVE-10 + emit-spine).

**Exact open gaps.**
1. **[family-killer, confirmed-modest] Q1 dense-classification is at-best parity** with hand simdjson —
   value is deletion + grammar-specialization + stable-by-construction + free `vshrn`. Honest, but not a
   perf win on the dominant pass. Sheng-as-negative *shrinks* the remaining value further.
2. **[Tier-M "parity by construction" is SPEC-ONLY dressed as answered]** the spec marks Q1 "answered"
   because "the hand LUTs already ARE hand-computed `byte_classes` tables" — but the emit pass
   (`[u8;256]`→SIMD classifier) **does not exist** (`equiv.rs` emits scalar only) and the separability
   solver is **unbuilt**, so the classifier whose parity is asserted **cannot yet be produced**. Parity
   is claimed between a hand table and a not-yet-emitted classifier. **Downgrade to SPEC-ONLY** until the
   emit pass + separability solver exist. (Load-bearing example is also mis-stated: `{`=**0x7B**, not
   0x3B — the collision is lo6(0x3B)=lo6(0x7B)=0x3B.)
3. **[elegant-reduction trap] the separability solver is the family's new code and is unbuilt.** A
   bounded CSP over a 256-entry partition selecting among three forms (nibble-sep ∨ lo6-table ∨
   `vceqq`-fan) — "tractable" is an argument, not an artifact. Gate 4 (naive nibble-only emit MUST
   misclassify the structural set) is the load-bearing proof and was not run.
4. **[born-RED census unrun, 2nd pass] the widened grammar-sourced census** (its own explosion
   candidate, `\p{L}+`/`url()`, sourced from `bootstrap/regex.bbnf`) was directed mandatory and again not
   run; Tier X is asserted from `dfa.rs:262` returning `None`, not censused; the coverage-% is unmeasured.
   The pass-1 "feasibility census excludes its infeasibility candidate" dodge is made *definitional* this
   pass (Tier X = "not a scanner, excluded") — honest ONLY if the census still reports Tier-X denominators
   (gate 3 says it should; it did not run, so the dodge is unrefuted in fact).
5. **[the actual BLOCKER — map-CORRECTED to a NAME+VERSION COLLISION] Q2 cross-lock to LOCK-5.**
   Agglomerator-confirmed this session: **`bbnf-lang/crates/bbnf-regex` DOES NOT EXIST** (pass-1's cited
   path was wrong); the real second crate is **`bbnf-lang/skinny/crates/bbnf-regex`**, and **both it and
   parse-that's `rust/regex` fork are `name = "bbnf-regex", version = "0.1.0"`** — one name+version
   resolving to **two different sources per build graph** (a direct Constitution-ii "one system" tension,
   not merely "two crates to reconcile"). The grammar→DFA→byte-fixpoint source-of-truth at scanner
   fidelity is owned by LOCK-5 and unbuilt. Missing-primitive test passes; BLOCKED stands.
6. **[cross-lock] quad-homed + GN-codex supersession un-named** (LOCK-5 grammar/DFA · LOCK-2 tier island
   · LOCK-9 backend · `crates/codegen/src/emit/regex/` GN-codegen). Constitution ix requires a *named*
   supersession, not an implicit land-grab. **[circularity]** imported CONFIRMED labels (Tiers 0/1/3 via
   PROVE-10/emit-spine) owe an in-plane parity check in parse-that's scanner plane.

**Pass-3 directives.**
- *State explicitly:* PROTO-2 (pass-1) closes **Q2 register-budget ONLY**; the disposition stays BLOCKED
  on Q1 (dense parity), the unbuilt separability solver, and Q3 (LOCK-5 name-collision). Do not treat
  "Sheng GO" or "register budget GO" as a whole-family bill; downgrade "Tier-M parity (Q1 answered)" to
  SPEC-ONLY.
- *Prototype (widen the census to its own killer — 2nd directive, unrun):* census the rich-CSS `url()`
  and a wide-unicode leaf **sourced from `bootstrap/regex.bbnf`**, reporting Tier-X rows as denominators
  so coverage is a measured percentage — not a fit-chosen sample.
- *Prototype (only if LOCK-5 substrate is ruled buildable):* build the separability solver + emit pass,
  lower one minimized leaf to a classifier, `otool` vs the hand scanner at ≤1.0× reduction-insn + paired
  ≥0.98×; and prove gate 4 (naive nibble-only MUST misclassify the structural set).
- *Research must answer:* does LOCK-5 build the byte-fixpoint-gated grammar→DFA pipeline at 9C's fidelity,
  and how is the `bbnf-regex 0.1.0` name+version collision resolved (which source is the DFA
  source-of-truth)? Absent a LOCK-5 GO, 9C cannot converge.
- *Withheld from researchers:* do **not** seed "register budget GO = 9C converges" or "Tier-M parity is
  answered." Hand them the narrow Q2 fact, Sheng-as-measured-negative, the four open items, and the
  LOCK-5 name-collision as a hard DAG predecessor.

---

## 9D — LAZY-SURFACE · BLOCKED · 30%

**Floor rationale.** CRIT-1 30 / CRIT-2 30. Take **30** — both concur, flat. Held BLOCKED (not RETIRED)
because P3 is cheap+deterministic and latency/TTFS survives regardless, so a valid close is one probe
away — **but this is the last pass 9D is carried at a live %**: P3 has now been skipped for two
consecutive passes while both probes went to the axis, and CRIT-1 is explicit that "two passes of
non-execution of the one gating measurement is itself evidence." **Pass-3 runs P3 or converts 9D to
RETIRED (latency-only residue).**

**What is EARNED (do not re-litigate).** The most honest self-diagnosis in the set, and two genuine
pass-2 hardenings: (1) the **M1/M2 split** — M1 (windowed scan over a *resident* buffer) already exists
(`ws_bitmap`/`ws_bitmap_start`, `state.rs:228`; `NibbleBitmapIter` stripe cursor) and delivers no
streaming/RSS value on its own; M2 (streaming *input*, `feed(chunk)`, never holding the whole document)
**does not exist in any form**. (2) The consumer-absence is now **grep-measured** (`fn feed|push_bytes|
Decoder|BufRead|document_stream|from_reader` → zero), not asserted. The partiality-as-witness
formulation (winnow `Partial<I>`, one path; eager whole-input = the degenerate one-window case) is
single-path-compliant, and bbnf-simd already speaks the carry vocabulary (`prefix_xor_64(mask,
carry_in)`, `escape_mask_64→(u64,bool)`, `CommentCarry`, `bracket_depth_mask_64`).

**Exact open gaps.**
1. **[the cheap decider, skipped for a 2nd pass] P3** (`furthest_offset − offset` rewind histogram at
   the three restore sites, JSON+CSS, `--features diagnostics` on/off, P50/P99/max). Self-described
   "mandatory, cheap, deterministic," skipped pass-1 AND pass-2. The entire memory raison d'être rests on
   this unmeasured property. research-D delivered only the instrumentation spec, again.
2. **[missing primitive, refuted-by-default] unbounded PEG rewind defeats the memory claim.** parse-that
   is a backtracking PEG (Ford 2004: no bounded-backtrack guarantee); if rewind is unbounded the ring
   buffer retains all input = whole-input with a lazy tail, **zero peak-RSS win** — "a property equal in
   difficulty to redesigning the backtracking discipline itself."
3. **[Y.13 tripped in-design, grep-confirmed] no streaming consumer.** The three shipped grammars build
   whole owned trees (twitter.json = 5390 allocs, the RSS mass); a streaming *input* surface removes one
   padded copy while the output tree — the actual RSS mass — is untouched, so even a successful M2
   delivers no peak-RSS win without a streaming *output* consumer that does not exist. Consumer-less
   substrate is forbidden.
4. **[architectural debt] `lazy.rs:20/49` name-collision** (combinator-graph laziness for recursion vs
   input-window laziness) named across two passes, not resolved.
5. **[zero-throughput-claim honesty is correct but total]** the straddle guard resurrects the per-window
   tail guard `PaddedView` deletes → neutral-to-slower; latency/TTFS is the only survivable value and is
   itself consumer-gated; P2 (peak-RSS + TTFS harness) unrun.
6. **[cross-lock] `KernelPlan` double-consumes an unbuilt LOCK-2** decision-driver island (same island 9C
   needs).

**Pass-3 directives.**
- *Prototype (the cheap decider — MANDATORY, or RETIRE):* run P3. Bounded (P99 < 64 B) → a bounded ring
  buffer is sound and the memory center holds; unbounded → **file the measured refutation** (memory
  center defeated, only latency survives). **If P3 is skipped a THIRD time, convert 9D to RETIRED**
  (latency-only residue folds into the 9C/9D gestalt-Ω as a carry-threading note, not a live family).
- *Prototype (Y.13 or refute):* name and ship a real chunked-feed streaming consumer measuring peak RSS +
  TTFS (P2), or file that no in-wave consumer exists and 9D collapses to latency-only.
- *Research must answer:* is there ANY in-wave streaming consumer for parse-that's actual grammars?
  (simdjson `document_stream` = whole-document granularity, weakening the peak-RSS argument.)
- *Withheld from researchers:* do **not** seed "streaming saves memory" (refuted-by-default) or any
  throughput claim. Hand them the cheap P3, the grep-confirmed Y.13 requirement, and the
  zero-throughput-claim law.

---

## CROWDING & REDIRECT (this pass)

**The central axis (9A⊗9B) is convergent-in-mechanism AND now shown to be a PERFORMANCE WASH — hold
both, bank neither.** Both routes reach the identical movemask contract-split and checkasm discipline;
pass-2 measured that the two *lawful* producers exist in bbnf-simd nowhere (must be authored once
regardless of home) and that the OnceLock inline advantage is free at production cgu=1. **So the axis is
no longer "which is faster" — it is a pure LOCK-6 maintenance-cost call ("one crate or two for identical
new code").** The crowding-redirect rule (bank the redundant one) still does **not** apply: the decider
is a LOCK-6 verdict LOCK-9 cannot render, and CRIT-2 X-8 shows pass-2's two biases point *opposite* ways
(charge 9A the 9A-only de-face cost → tilts to 9B; re-measure gate-5 against the *evolved* home → tilts
to 9A). Banking either now would pick the axis without the corrected evidence. **9B holds as
incumbent-default (PT-2 frozen codex) and is the only family off SPEC-ONLY; 9A's core physical promise is
proven live but its value is now maintenance-only; neither banked; the axis stays open pending LOCK-6.**

**The pass-1 crowding-redirect (open the lean nibble-stride no-repack movemask) is now RESOLVED by
measurement — bank the canonical-producer datum, retire the directive.** PROTO-2 measured all forms: LEAN
nibble-`vshrn` 14 insns (−26%, 0 `vaddv`), DENSE `vpaddq` tree 50 insns (−22%, 0 reduction ops, lowers to
`uzp`/`orr`), vs bbnf-simd's 28-insn repack and the refuted 11-insn double-`vaddv`. **The canonical
producer is settled (measured, not asserted): contract-split — LEAN nibble-`vshrn`, DENSE dense-64
`vpaddq` tree, NEVER the 16-byte repack, NEVER double-`vaddv`.** What remains is the *home collapse* (four
producers coexist — bbnf-simd repack + bbnf-simd refuted double-`vaddv` + PROVE-10 lean + parse-that
`to_bitmask`), a LOCK-9 ∩ LOCK-5 gestalt act; the *measurement* is done.

**The underexplored formulation to OPEN this pass (replaces the resolved movemask directive): "evolve the
shared home, THEN re-measure the axis."** Both PROTO-1's gate-5-against-un-evolved-bbnf-simd and PROTO-2's
divergence-against-un-evolved-bbnf-simd measured the *wrong baseline*. The honest axis-decider is: author
the two canonical producers into a scratch bbnf-simd + purge its refuted double-`vaddv`, then (i) measure
9B's residual LOC/substrate divergence against that *evolved* home and (ii) charge 9A the 9A-only de-face
cost — the two corrections CRIT-2 X-8 names. Neither pass touched the evolved-home baseline; it is the
pass-3 keystone and the datum LOCK-6 actually needs.

**9D's `KernelPlan` is not a new subsystem** — it is a consumer of LOCK-2's decision driver (same island
9C's tier-selection needs). Flag as a LOCK-2 DAG dependency; no LOCK-9-internal redirect.

## CROSS-POLLINATION (named grafts — both families developed enough; graft justified)

1. **9C ⊗ 9D — the compiled resumable scanner (PRIMARY, gestalt-Ω consolidation candidate; RE-BASED by
   pass-2).** research-D (Ragel/re2c): a streaming scanner IS a resumable DFA. **Pass-2 corrects the
   basis:** Sheng is now a measured NEGATIVE, so 9C collapses to a *stateless* Tier-M membership classify
   + Tier-3 structural — 9D therefore threads carries through the **Tier-3/structural kernels**
   (`prefix_xor`/comment/bracket-depth carries bbnf-simd already exports), **not** a saved Sheng state.
   The gestalt-Ω "compiled resumable scanner" survives on the structural-carry basis. Neither 9C nor 9D
   is independently converge-able as a separate LOCK-9 family — gestalt-Ω carries both as inputs to ONE
   object (a mild double-count the roll-up acknowledges). *If 9D RETIRES in pass-3, this graft is where its
   carry-threading residue survives.*
2. **9A/9B kernel home ⊗ 9D carry vocabulary (SECONDARY, orthogonal-composable).** 9D's `Window` witness
   threads carries into *whichever* kernel-home verdict wins; it changes *what feeds* the kernels, not
   *who owns* them (input-surface ⊥ kernel-home). A later, conditional overlay, admissible only if P3
   measures a bounded backtrack.

## CROSS-LOCK DAG ORDERING (for GESTALT — blockers LOCK-9 CANNOT retire alone)

- **9A⊗9B → LOCK-6 (now a maintenance-cost call, not a perf call).** The kernel-home decider is "the
  reasonable point of rust↔rust isomorphism" (Constitution iii) resolved by "null hypothesis defeated by
  measured drift/maintenance cost" (LOCK-6D verbatim). **Pass-2 established the perf axis washes**, so
  the verdict rests on maintenance cost alone. LOCK-9 produces the *corrected* measurements (9A: perf half
  on CSS + evolved-home gate-5 + the 9A-only de-face charge; 9B: merged-surface divergence against the
  evolved home); LOCK-6 renders the verdict. Co-converge in GESTALT; neither closes the axis alone.
- **Canonical movemask form → RESOLVED (measured); home-collapse → LOCK-9 ∩ LOCK-5.** The form is settled
  (contract-split, measured). Four producers must collapse to ONE lawful home (bbnf-simd repack +
  bbnf-simd refuted double-`vaddv` + PROVE-10 lean + parse-that `to_bitmask`); memchr's lean form lives in
  bbnf-regex's prefilter. The gestalt collapses the home.
- **Prefilter double-home → GN-2/PT-1 dispatch law.** `bbnf-simd::find_ascii_set_member64` ⟷
  `bbnf-regex::Dfa::find` — one lowering per scan site, never both. 9C deepens this.
- **9C grammar→DFA source-of-truth → LOCK-5 (MAP-CORRECTED).** The blocker is a **name+version
  collision**: `parse-that/rust/regex` and `bbnf-lang/skinny/crates/bbnf-regex` are **both `bbnf-regex
  0.1.0`**, resolving to different sources per build graph (Constitution-ii tension). Pass-1's cited path
  `bbnf-lang/crates/bbnf-regex` was **wrong and does not exist**. LOCK-5 owns the resolution + the unbuilt
  grammar→DFA pipeline. **9C tier-selection → LOCK-2** (scan-plan e-graph island); **9C codegen home
  `crates/codegen/src/emit/regex/` → a named GN-codex supersession** (constitution ix). 9C is quad-homed
  and cannot be *selected* until LOCK-5 + LOCK-2 land.
- **9C/9D resumable-scanner collapse → gestalt-Ω** (cross-pollination 1, re-based onto structural carries).
- **9D `KernelPlan` → LOCK-2** (compile-time decision driver, the same island 9C needs).
- **Un-homed consumer edge → LOCK-4/LOCK-8.** The reborn scanner surface feeds the combinator core
  (LOCK-4) and, via LOCK-6, the TS backbone (LOCK-8); no 9-family names how it presents to LOCK-4's parser
  graph or whether LOCK-8's TS side mirrors 9C's compiled-DFA products. An un-homed edge the gestalt-P
  weave must close.

## RECONCILIATION OWED (registry → FAMILY-SPECS, before gestalt)

The FAMILY-SPECS (09:52) is **stale against its own probes** (PROTO-1/2 landed 10:09–10:10) and both
critics flag it. Before gestalt the synthesis must absorb: (i) the axis is a **performance wash** — reword
"9A must earn its place by producing the two datums" (the datums that ran suggest the tie breaks on
maintenance cost, not merit); (ii) the OnceLock barrier is **free at cgu=1** (delete "an indirect branch
that survives fat-LTO in-loop"); (iii) the "free `vshrn` movemask" is **refuted** (lawful producers absent
from bbnf-simd; author-once regardless of home); (iv) "9B inherits the identical OnceLock constraint" is
**false** (9B has no facade; de-face is 9A-only); (v) the `bbnf-regex` path is **wrong** (skinny/crates,
name+version collision); (vi) Tier-M "parity by construction (Q1 answered)" downgrades to **SPEC-ONLY**.

---

## PASS HISTORY

### Pass 2 (2026-07-16) — agglomerated

- **Artifacts:** FAMILY-SPECS pass-2 (4 specs + M/S/X tiering + incompatibility map — STALE against the
  protos it postdates); research-{A,B,C,D,priorart} pass-2; PROTO-1 (9A live cross-crate inline,
  RAN_MEASURED); PROTO-2 (9B LEAN/DENSE rebirth + 6-form canonical-producer disasm, RAN_MEASURED); CRIT-1
  (52/62/44/30); CRIT-2 (55/62/43/30).
- **PROTO-1 disposition:** genuine, measured on a live wired scanner. **SETTLED:** the de-faced bbnf-simd
  kernel inlines across the real rlib boundary with 0 in-loop `bl`/`blr` (born-RED inline claim now
  measured-GREEN). **OVERTURNED (pass-1 do-not-re-litigate items):** the OnceLock CONTROL emitted
  byte-identical `blr`-free code at cgu=1 (the 0.92× barrier was a probe-crate/multi-CGU artifact; de-face
  buys inline-parity, not a win); production IS cgu=1 so the necessity is moot where it ships. **STILL
  UNMEASURED (2nd pass):** the perf half of the axis-decider — and the probe wired `classify_stripe`, which
  is **CSS-only**, so the commissioned JSON ratio would be a null. The **vendor** seam was tested, not the
  one-line path-dep.
- **PROTO-2 disposition:** the strongest artifact in the lock; host-noise-immune static disasm. **BANKED:**
  LEAN `number_simd` double-`vaddv`→`shrn.8b` (0 `vaddv`, 14 vs 19 insns); DENSE `vpaddq` tree (50 vs 64,
  0 reduction ops, lowers to `uzp`/`orr`); paired ≥0.98× (15/16). Pass-1's cheapest-win directive CLOSED.
  **NOT SETTLED:** gate-5 divergence measured against **un-evolved** bbnf-simd (overstates 9B's case — the
  producers must be authored once regardless of home); DENSE correctness is "parsed without panic," not a
  bit-parity oracle; 9B never left nightly (stable cutover untested).
- **The lock-wide finding (both probes together):** the 9A⊗9B axis is a **PERFORMANCE WASH** — the OnceLock
  inline advantage is free at cgu=1 AND the canonical producers must be authored once regardless of home.
  The axis reduces to a pure LOCK-6 maintenance-cost call; FAMILY-SPECS has not absorbed this.
- **Substrate corrections (agglomerator-confirmed this session):** **X-7** — `bbnf-lang/crates/bbnf-regex`
  does NOT exist; the real crate is `skinny/crates/bbnf-regex`, and it + parse-that's `rust/regex` fork are
  **both `bbnf-regex 0.1.0`** (name+version collision, not "two crates to reconcile"). **X-8a** — parse-that
  scan has **zero** OnceLock/primitive_kernels; the facade is bbnf-simd-internal → 9B has no facade, the
  de-face is a 9A-only cost. **`vpaddq` in bbnf-simd = ZERO files** (lawful DENSE producer absent).
- **Structural failure carried forward:** for the SECOND consecutive pass both probes went to the central
  axis (four probes across two passes, all on 9A/9B); the two BLOCKED families (9C widened census, 9D-P3 —
  both cheap, deterministic, born-RED) were **never probed**, so 9C/9D carry zero new measured evidence.
- **Dispositions set:** 9A ADVANCED born-RED **52** (down; two headline merits fell, inline measured-GREEN);
  9B ADVANCED incumbent **62** (up; two artifacts banked, off SPEC-ONLY); 9C BLOCKED **43** (down; Sheng
  retired to a measured negative, path corrected); 9D BLOCKED **30** (flat; P3 skipped a 2nd pass — LAST
  pass at a live %, pass-3 runs P3 or RETIRES). No family banked (axis is a LOCK-6 maintenance verdict).
- **Crowding/redirect:** the pass-1 lean-movemask directive is RESOLVED by measurement (canonical producer
  = contract-split, banked); OPEN instead "evolve the shared home, then re-measure the axis" (author the
  canonical producers into a scratch bbnf-simd + purge its refuted double-`vaddv`, then re-measure gate-5 vs
  the evolved home + charge 9A the 9A-only de-face cost) — the pass-3 keystone and LOCK-6's real input.
- **Cross-pollination:** 9C⊗9D compiled-resumable-scanner RE-BASED onto structural carries (Sheng negative);
  it is where 9D's residue survives if 9D retires.
- **Pass-3 keystone directives:** (1) **run the evolved-home axis re-measurement** — the corrected LOCK-6
  input neither pass produced; (2) close **9A's perf half on the CSS surface** (+ the one-line path-dep seam,
  not vendor); (3) make the **R-A3 purge a blocking gate**; (4) **bank 9B DENSE correctness with a bit-parity
  oracle + run the stable-toolchain cutover**; (5) **run 9D-P3 or RETIRE 9D**; (6) widen the **9C census to
  its killer** grammar-sourced + downgrade Tier-M parity to SPEC-ONLY; (7) **reconcile FAMILY-SPECS to the
  protos** (the six stale claims). Promote every un-run cheap decider to must-run; the axis probes have been
  run twice — pass-3 owes the BLOCKED families their commissioned measurements.

### Pass 1 (2026-07-16) — agglomerated

- **Artifacts:** FAMILY-SPECS (synthesis: 4 family specs + 3-axis incompatibility map + summary matrix);
  research-{A,B,C,D,priorart}; PROTO-1 9A/9B kernel-home (RAN_MEASURED, aarch64, fat-LTO disasm + probe
  crate + micro-timing); PROTO-2 9C Tier-2 Sheng census (RAN_MEASURED, deterministic, no bench); CRIT-1
  (within-lock: 9A 55 / 9B 55 / 9C 45 / 9D 35); CRIT-2 (cross-lock: 9A 58 / 9B 64 / 9C 44 / 9D 30).
- **PROTO-1 disposition:** genuine, disassembled, real gate FAILURE (arm_a OnceLock `blr x8` survives
  fat-LTO ~0.91–0.93×; arm_b direct-cfg ICF-folds to baseline 1.00×, `nm`-proven same address as
  `probe_baseline`). **But it tested the EASIEST, WRONG kernel:** `bitmap_prefix_xor_64` is a *scalar*
  5-shift/xor bit-twiddle with no NEON body — arm_b ICF-folded *because it is scalar and stateless*, not
  because `#[inline]` crosses the real wired-scanner boundary (proven only in a 5-fn probe crate, not the
  commissioned `otool` of a wired scanner). The family's raison d'être — the `vshrn` movemask reroute — was
  explicitly skipped (criterion (b) "N/A to this kernel"), and the kernel is bench-dead relative to
  `json_parser` (`compute_in_string_bitmap`/`filter_quote_parity` unreachable from the bench). **SETTLED
  (narrow):** OnceLock facade blocks inline (~0.92×); direct-cfg ICF-folds to baseline (1.00×); the lever is
  OnceLock-vs-direct-cfg, orthogonal to shared-vs-own. **NOT SETTLED (do not inherit):** the vector-tier
  inline on a live scanner; the "free `vshrn` movemask" (CONTESTED — bbnf-simd VERIFIED-repacks to dense u16).
  *[Pass-2 note: the SETTLED "~0.92×" and the "OnceLock-vs-direct-cfg lever" were OVERTURNED by PROTO-1
  pass-2 — free at cgu=1; and the CONTESTED movemask was REFUTED by PROTO-2 — lawful producers absent.]*
- **PROTO-2 disposition:** real, deterministic, cleanly closed Q2 — 5/5 archetype leaves fit strict
  `vqtbl1q_u8` (number 9×7=63 the tight boundary, had to be measured). **But scoped to dodge its own
  killer:** the sample excludes the named explosion candidate (rich-CSS `url()`/wide-unicode), and leaves
  were hand-typed generic archetypes fed straight to `Dfa::compile_with`, NOT sourced from
  `bootstrap/regex.bbnf`, and through parse-that's *local* regex fork (`path = "regex"`), not bbnf-lang's
  `bbnf-regex`. **SETTLED (narrow):** the transition-table register budget is GO for the ASCII archetype
  family. **NOT SETTLED:** dense parity (Q1); the grammar-sourced/wide-class census; the class-map∘Sheng
  data-dependent-stall composition. *[Pass-2 note: the class-map∘Sheng composition was answered NEGATIVE
  in FAMILY-SPECS pass-2 — Tier-S serial stall; Sheng is now a measured negative.]*
- **New substrate finding (CRIT-1 pass-1):** the 9C LOCK-5 coupling is potentially two regex crates to
  reconcile. *[Pass-2 correction: it is a name+version collision, `bbnf-regex 0.1.0` × two sources; the
  pass-1 path `bbnf-lang/crates/bbnf-regex` was wrong — real is `skinny/crates/bbnf-regex`.]*
- **Dispositions set:** 9A ADVANCED born-RED 55; 9B ADVANCED incumbent 55; 9C BLOCKED 44; 9D BLOCKED 30.
  No family banked — the central axis is convergent-in-mechanism but its decider is a deferred LOCK-6 verdict.
- **Crowding/redirect:** hold 9A/9B open (axis → LOCK-6); OPEN the underexplored lean-nibble-stride
  no-repack movemask as the contested canonical producer (directed pass-2 probe + instruction-count gate);
  name the 9C⊗9D compiled-resumable-scanner cross-pollination for gestalt-Ω. *[Pass-2: the lean-movemask
  probe RAN (PROTO-2) and RESOLVED the canonical producer by measurement.]*
- **Pass-2 keystone directives (as issued):** (1) run the 9A⊗9B decider on a live scanner; (2) re-probe 9A
  on a real NEON body + instruction-count gate; (3) BANK 9B's number_simd double-`vaddv`→`vshrn`; (4) run
  9D-P3; (5) widen the 9C census. *[Pass-2 outcome: (1) half-done — 9A inline measured, perf half missed on
  CSS-only surface; (2)/(3) done — PROTO-2 banked LEAN+DENSE with insn counts; (4)/(5) NOT RUN a 2nd time.]*
