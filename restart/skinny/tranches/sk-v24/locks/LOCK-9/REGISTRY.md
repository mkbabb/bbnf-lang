# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. Inputs folded: FAMILY-SPECS (synthesis +
incompatibility map), research-{A,B,C,D,priorart}, PROTO-1 (9A/9B kernel-home, +patches +probe crate
+disasm), PROTO-2 (9C Sheng census, +patch +run log), CRIT-1 (within-lock), CRIT-2 (cross-lock). The
lock is not four features — it is four dispositions of one aarch64 scanner/kernel plane on **three
distinct axes**: **kernel-home** (9A one shared crate ⊥ 9B parse-that keeps its own — the central
adjudication), **scanner-provenance** (9C derived-from-DFA vs authored {9A,9B}), **input-surface** (9D
windowed vs whole-input — orthogonal, contradicts the shared `PaddedView` eager invariant). The floor
is honest: substrate paths:lines were re-verified against the live trees by both critics; the defects
are in prototype-uncertainty targeting and gates that under-catch, not in the map.*

---

## STATUS TABLE (after pass 1)

| Family | Status | Conv. | One-line |
|---|---|---:|---|
| **9A ONE-KERNEL-LIBRARY** | **ADVANCED · born-RED** | **55%** | PROTO-1 proved the *negative* half of Q1 (OnceLock facade blocks inline, ~0.92×) and the direct-cfg fix (ICF-folds to baseline, 1.00×) — but on a **scalar** kernel dead to the bench; the family's raison d'être (the shared `vshrn` movemask reroute) was never probed and is CONTESTED (bbnf-simd repacks to dense u16). |
| **9B NATIVE-REBIRTH** | **ADVANCED · incumbent** | **55%** | Settled art, lowest-risk, PT-2 frozen-codex default — but **zero artifact ran**, its cleanest standalone win (`number_simd` double-`vaddv`→`vshrn`, VERIFIED live) is un-landed, and its axis-deciding gate 5 (LOC/substrate divergence) is unmeasurable because nothing was rebirthed. |
| **9C SCANNER-AS-DFA** | **BLOCKED** | **44%** | PROTO-2 cleanly closed **Q2 (register budget)** — 5/5 archetype leaves fit strict `vqtbl1q_u8` — but on an ASCII sample that **omits the rich-CSS/wide-unicode killer leaf**, via hand-typed patterns not `bootstrap/regex.bbnf`; the family-killer (Q1 dense parity) is untested and the enabling substrate (grammar→DFA source-of-truth) is **owned by LOCK-5 and unbuilt**. |
| **9D LAZY-SURFACE** | **BLOCKED** | **30%** | Nothing ran — including its own "cheap, deterministic" decider (P3 backtrack bound); memory raison d'être **refuted-by-default**; no streaming consumer named (Y.13 tripped in-design). Latency/time-to-first-token survives regardless. |

Net: **no family is converged; the lock's central axis (9A⊗9B) has ZERO measured input.** The single
sharpest uncertainty in the lock — the drift-vs-coupling decider — was touched by neither probe, and it
is structurally a **LOCK-6 verdict** LOCK-9 cannot render alone. Pass-1 spent both probes on 9A(a
fragment) + 9C(a census), leaving the incumbent (9B) and the cheapest-to-close family (9D-P3) empty.
Do not read the two green probe results as convergence on the load-bearing claims — both measured the
corners that *avoid* each family's central bet.

---

## 9A — ONE-KERNEL-LIBRARY · ADVANCED · born-RED · 55%

**Floor rationale.** CRIT-1 55 / CRIT-2 58. Take **55**: CRIT-2's +3 credits the genuine disassembled
result (the OnceLock `blr x8` survives fat-LTO; the direct-cfg arm ICF-folds to baseline), which is real
and moves the axis — but CRIT-1 correctly discounts because PROTO-1 tested the *easiest, wrong* kernel
(`bitmap_prefix_xor_64` is a **scalar** 5-shift/xor bit-twiddle with no NEON body — arm_b ICF-folded to
baseline *precisely because it is scalar and stateless*), and the family's actual mechanism (the vector
tier crossing the path-dep boundary as a multi-instruction NEON body + the `vshrn` movemask reroute) is
entirely unprobed. No reason below 55: the direct-cfg refactor prescription is actionable and
low-invention (three primitives already do it), and correctly NOT BLOCKED (no missing primitive of equal
difficulty).

**What is EARNED (do not re-litigate).** (1) The `OnceLock` fn-pointer facade
(`prim::bitmap_prefix_xor_64` → `dispatch::primitive_kernels().<fn>`) is a hard devirtualization barrier
fat-LTO cannot pierce: `blr x8` + `ldapr` acquire-load + `cbnz` init-branch + fn-ptr `ldr` survive in the
loop body every iteration → **~0.91–0.93× at the kernel micro level (fails ≥0.98×).** (2) A direct
`#[cfg(target_arch="aarch64")]` entry inlines to **zero** indirect branches and `nm`-proves identical to
parse-that's existing inline body (ICF, same address). **The lever is OnceLock-vs-direct-cfg, orthogonal
to shared-vs-own — 9B would inherit the identical constraint.** (3) The physical seam is one line:
`.cargo/config.toml [patch.crates-io]` already path-deps `bbnf`/`bbnf-regex`/`csp-solver`; `bbnf-simd` is
absent — verified by both critics.

**Exact open gaps.**
1. **[raison d'être, un-probed] The `vshrn` movemask reroute — 9A's headline win — was explicitly skipped.**
   PROTO-1 chose the one kernel where the movemask question *cannot arise* (criterion (b): "N/A to this
   kernel"). research-A §5 is unambiguous that "9A contains the PT-2 W1 movemask fix as a corollary of
   sharing" is the structural payoff; it is unmeasured.
2. **[contested] The shared movemask VERIFIED-repacks to a dense u16, contradicting "free correct movemask."**
   `bbnf-simd/aarch64/movemask.rs:4 movemask_u8x16 -> u16` runs `vshrn_n_u16::<4>` **then repacks**
   (`vand`/`vsri_n_u8::<3>`/`vzip1q_u8` + an 8-iter scalar pack loop) — *more elaborate* than PROVE-10's
   lean `shrn.8b→fmov→4-bit-stride, no repack` form (research-priorart §2a). So the vector-tier
   `to_bitmask` either (a) keeps dense 1-bit-per-lane semantics and pays the repack (whose cost vs the
   refuted shim's double-`vaddv` is **unmeasured** — FAMILY-SPECS asserts "minus its regressing
   double-`vaddv`" as if the repack were free), or (b) changes to 4-bit-per-lane and breaks the one-line-swap
   ergonomics. This is the exact semantic that forced the shim's defect, re-imported un-adjudicated.
3. **[gate-that-cannot-fully-fail] Gate 3 measures reduction-count only, not instruction weight.** The
   widen-back reconstruction uses no `addv`, so it PASSES gate 3 while carrying extra `vand`/`vsri`/`vzip`/
   8-iter-loop weight. Gate needs an instruction-count arm.
4. **[the make-or-break, inferred not measured] Gate (1) — direct-cfg inlines in the WIRED parse-that
   scanner across the path-dep boundary — was proven only by ICF inside a 5-function probe crate.** ICF in
   an isolated binary does not demonstrate `#[inline]` crosses the real `parse_that↔bbnf-simd` boundary
   under a live scanner's register pressure. The positive half of Q1 is inferred.
5. **[SPEC-ONLY] The `vector/{mod,neon,scalar}.rs` module does not exist** — it is the bulk of 9A's actual
   surface (decode store_chunk, ws_comment, scanners, span_parser/scan) and is the "refuted 276-LOC
   simd128.rs promoted, minus double-vaddv"; promoting refuted code demands its own no-regression artifact,
   which does not exist.
6. **[unrun] Gate 2 (paired ≥0.98×, 6 JSON corpora, both windows, live consumer) never executed** —
   PROTO-1 substituted a micro-bench of a bench-dead kernel at loadavg 13–16.
7. **[unquantified] Q3 granularity mismatch** (64-byte-block kernels vs 16-byte-lane parse-that compositions
   → the physical iso is lane-level for those); the kernel-tier-vs-vector-tier split fraction (the mapping
   census) was not produced. **Q2** double-padding (`PaddedView`-64 vs `eob_pad_clamp`) named, not exercised.
8. **[cross-lock, un-named] The P23 read-only supersession.** 9A's "one added line" mutates parse-that's
   `.cargo/config.toml`; parse-that is **READ-ONLY to the bbnf-lang campaign** (non-clobber P23). Adopting
   9A requires a *named* supersession of P23 or a gestalt adjudication; FAMILY-SPECS notes PT-2 chose 9B but
   never states 9A overturns the read-only law.

**Pass-2 directives.**
- *Prototype (the deferred raison d'être — now mandatory):* wire ONE **live** hot scanner
  (`structural_bitmap.rs`→`structural_classify_64`, or a real `to_bitmask` classify-stripe site — **not**
  prefix_xor, **not** a micro-bench) to consume `bbnf-simd` via the direct-cfg entry across the `[patch]`
  path-dep, fat-LTO; `otool -tv` the wired scanner for zero indirect-in-loop AND run the PROVE-09 paired
  cold N≥200 two-window protocol over 6 JSON corpora for ≥0.98×. This is gate (1)+(2) at the commissioned
  altitude, never inferred from a probe crate.
- *Prototype (settle the contested movemask, gaps 2/3):* disasm the rerouted `to_bitmask` — is bbnf-simd's
  dense-u16 repack the lean form parse-that should inherit, or over-engineering? Add the **instruction-count
  gate arm** so gate 3 can actually fail. This is the OPENED underexplored formulation (see CROWDING).
- *Research must answer:* the mapping census (each of the 8 scanner files: 1:1-to-kernel | vector-tier |
  NO_GO), quantifying the kernel-tier/vector-tier split; and does fat-LTO carry `#[inline]` across the
  path-dep on a live scanner (not ICF in isolation)?
- *Withheld from researchers:* do **not** seed "the free `vshrn` movemask" as a settled win — it is
  unmeasured AND contested by the family's own research (dense-u16 repack). Hand them only the settled facts
  (OnceLock blocks inline; direct-cfg ICF-folds to baseline; the lever is OnceLock-vs-direct-cfg) and the
  open contested-movemask question. Do not seed the P23 supersession as granted.

---

## 9B — NATIVE-REBIRTH · ADVANCED · incumbent · 55%

**Floor rationale.** CRIT-1 55 / CRIT-2 64. Take **55**, docking CRIT-2's 64 explicitly: CRIT-2 credits
settled-art + lowest-risk (real), but a family that produced **zero running evidence** and whose
convergence-DECIDING gate (5, the null-hypothesis LOC/substrate divergence) is *unmeasurable because
nothing was rebirthed* cannot sit above the incumbent floor. CRIT-2's 64 rewards the disposition's
low-risk posture, not measured convergence; per §0 (concrete-artifacts-or-SPEC-ONLY) the incumbent
carrying no artifact is a discipline gap, not a high mark. 55 matches 9A: the axis cannot be adjudicated
from *either* side this pass.

**What is EARNED (do not re-litigate).** The movemask taxonomy is settled art with a named counterexample:
**(a) nibble-stride** `vshrn_n_u16::<4>` (`trailing_zeros()>>2`, one `shrn.8b`+`fmov`) for every
position-scan; **(b) dense-64** simdjson `vpaddq` pairwise-add tree for the ONE site assembling a 64-bit
mask across four blocks feeding `prefix_xor` (`classify_stripe_64`); **(c) FORBIDDEN double-`vaddv`
dense-16** — VERIFIED live at `number_simd.rs:54-55`, strictly dominated by (a). Every intrinsic is stable
`core::arch::aarch64` since 1.59 → no nightly; NEON is baseline-mandatory → purging x86 deletes the whole
`is_*_feature_detected!` machinery as dead weight. Correctly NOT BLOCKED; cleanest to execute.

**Exact open gaps.**
1. **[the cheapest decisive win, un-landed] `number_simd.rs:54-55` double-`vaddv`→nibble-stride `vshrn`.**
   VERIFIED live by both critics; a deterministic swap; a single `otool -tv` + insn-count would have banked
   a decisive pass-1 artifact for near-zero cost. It was asserted, not demonstrated. **The refutation law
   is stated and violated-in-place simultaneously** (the double-`vaddv` the law forbids is still live and
   the pass did not remove it). This is 9B's fastest path off SPEC-ONLY.
2. **[axis-decider, unmeasurable this pass] Gate 5 (null-hypothesis LOC + substrate divergence) never ran.**
   The spec itself states 9B "loses to 9A on merit" iff post-rebirth kernels are near-identical — "a null
   hypothesis to defeat by measurement." Nothing was rebirthed, so divergence is unmeasurable. 9B cannot
   converge, and 9A⊗9B cannot be adjudicated, until this table exists.
3. **[named, not exercised] Q1 dense-64 quote-parity self-regression.** Naive nibble-stride `vshrn` on
   `classify_stripe_64` yields 256 bits and corrupts parity → it MUST use idiom (b). The one place 9B can
   self-inflict the refuted defect is precisely the one place unmeasured.
4. **[named, not disasm'd] The movemask-repack differentiator.** 9B's `first_match` "elides the repack
   bbnf-simd keeps"; VERIFIED bbnf-simd `movemask_u8x16` DOES repack to dense u16, so a leaner 9B movemask
   is *plausible* — but §0/09 says "obvious wins" must be paired-measured, and it was not. If 9B's movemask
   is materially leaner, that is a point AGAINST 9A (9A-gap-2) — it cuts both ways only when disasm'd.
5. **[prospective] The checkasm-liveness harness (gate 3) is a plan, not an artifact** — `vmull_p64`
   `target_feature="aes"` dual-arm rot risk (Q2) named, harness unported; parity ≠ liveness discipline
   described but not run.
6. **[unverified] Co-traveler scope creep.** `smallbox[nightly]`→stable `smallbox!` at `parse.rs:74`
   claimed alloc-neutral with no measurement; the de-nightly cutover blast radius is asserted bounded, not
   shown bounded.
7. **[cross-lock] Gate 5 IS a LOCK-6 verdict, mis-homed here.** 9B's founding bet — "the *reasonable point*
   of rust↔rust kernel iso (Constitution clause iii) is design-level" — is verbatim LOCK-6's question, and
   "null hypothesis defeated by measured drift cost" is LOCK-6D's framing word-for-word. LOCK-9 cannot
   settle 9A⊗9B internally; it produces the two measurements as LOCK-6's inputs.

**Pass-2 directives.**
- *Prototype (BANK FIRST — cheapest decisive artifact):* land gap 1 — `number_simd.rs:54-55` double-`vaddv`
  → nibble-stride `vshrn`, `otool -tv` + insn-count (reduction ops ≤ baseline). Gets 9B off SPEC-ONLY for
  near-zero cost and removes a live refutation-law violation.
- *Prototype (the axis half 9B owns):* native rebirth of the two idiomatically-opposite kernels —
  `structural_bitmap.rs`/`scanners.rs` position scan (idiom a) AND `quote_parity.rs::classify_stripe_64`
  dense-64 (idiom b, disasm-prove `vpaddq` not `vaddv` not shrn-then-repack) — then measure **gate 5**: LOC
  duplicated vs bbnf-simd + the substrate-divergence table (`PaddedView`-64 vs unpadded; `vqtbl1q_u8`
  single-table vs `vqtbl4q_u8` four-table) + a shared differential conformance corpus *as data*. This is
  the datum that defeats-or-concedes-to 9A; deliver it as LOCK-6's input, not as a LOCK-9-internal verdict.
- *Research must answer:* post-rebirth, is LOC/substrate divergence large enough to justify two homes, or do
  the kernels concede to 9A's near-identity? And is 9B's `first_match` movemask materially leaner than
  bbnf-simd's repack (gap 4)?
- *Withheld from researchers:* do **not** let "9B is the PT-2 incumbent, default to it" substitute for the
  measurement — the axis decider (gate 5) is unrun and the incumbent posture is a risk prior, not
  convergence. Hand them the settled taxonomy + the number_simd clean win to bank, and the null-is-valid
  framing (9B concedes to 9A honestly if divergence measures small). Zero "faster" claim in any gate.

---

## 9C — SCANNER-AS-DFA · BLOCKED · 44%

**Floor rationale.** CRIT-1 45 / CRIT-2 44. Take **44**: both agree PROTO-2 cleanly closed Q2 (register
budget) and both hold BLOCKED-candidate; CRIT-2's −1 correctly weights that the census *excludes its own
named explosion candidate* (rich-CSS `url()`/wide-unicode) and bypasses the grammar-source path, so the
GO is "100% of a sample chosen to fit." Held at BLOCKED (not ADVANCED): the family is cross-lock-coupled
to LOCK-5 whose deliverable it *requires* and which is unbuilt, and its dominant-pass value is
parity-only. Survives degraded (Tiers 0/1/3) even if Tier-2 fails a wider census.

**What is EARNED (do not re-litigate).** PROTO-2 ran, deterministic, and closed **Q2** for the archetype
family: number 9×7=63, string-body 4×4=16, ws+comment 5×5=25, identifier 2×3=6, hex 2×2=4 — **5/5 fit the
strict `vqtbl1q_u8` budget** (≤16 states AND ≤16 classes), tbl4 not even required. The number leaf at 63 is
the tight boundary (one below the tbl4 ceiling) and had to be measured, not estimated. Tiers 0/1/3
(memchr prefilter / shufti / simdjson-structural) are prior-confirmed art (PROVE-10 + emit-spine).

**Exact open gaps.**
1. **[the family-KILLER, untested] Q1 dense-classification no-win.** Structural bitmap classifies every
   byte — nothing to skip — so a DFA-lowered dense classify is **at best parity** with hand simdjson. 9C's
   value on the *dominant* pass is NOT speed; it is deletion + grammar-specialization +
   stable-by-construction + free movemask. Real but modest and unmeasured. Q2 GO does not touch Q1.
2. **[census scoped to dodge its own killer] The explosion candidate was excluded.** research-C risk 2 +
   FAMILY-SPECS Q2 both name "a rich CSS `url()` or wide unicode class" as the state/class-explosion
   boundary; PROTO-2 censused none — byte-mode==unicode-mode "here" *because no wide-unicode leaf was in the
   sample at all*. The "no coverage-percentage caveat" headline over-claims: it is 100% of a sample that
   structurally omits the infeasibility case (a feasibility census whose sample excludes the infeasibility
   candidate).
3. **[grammar-totality path un-exercised] Leaves were hand-typed generic archetypes, not sourced from
   `bootstrap/regex.bbnf`.** 9C's thesis is *grammar leaf → bootstrapped DFA → kernel* (constitution i);
   PROTO-2 fed patterns straight into `Dfa::compile_with`, proving "these 5 regexes minimize small," NOT
   "the bootstrapped grammar's leaves minimize small." The source-of-truth pipeline that IS 9C was not
   exercised even at census altitude.
4. **[the actual BLOCKER, untouched] Q3 cross-lock coupling to LOCK-5 — and it is a TWO-regex-crate
   reconciliation.** New finding (CRIT-1): `.cargo/config.toml` binds `bbnf-regex = { path = "regex" }` —
   parse-that's **own in-repo regex fork**, not bbnf-lang's `crates/bbnf-regex`. PROTO-2 compiled through
   *that local fork*, so the census speaks only for it. 9C's enabling substrate — the byte-fixpoint-gated
   grammar→DFA→table source-of-truth — is **owned by LOCK-5 and unbuilt at the fidelity 9C needs**, and the
   coupling is potentially two regex crates to reconcile (parse-that/rust/regex ⟂ bbnf-lang/crates/bbnf-regex),
   not one. This is the missing-primitive test: substrate owned elsewhere and unbuilt.
5. **[un-probed] The class-map/Sheng composition (PROVE-9C-B).** PROTO-2 measured only *table size*.
   Whether the dense classify (produces the class byte) and the `vqtbl1q_u8` Sheng step (advances state)
   compose without a **per-byte serial-dependency stall** — the thing that decides if a *vectorized* DFA
   actually vectorizes (`state' = tbl(row[class], state)` is a data-dependent chain) — was not touched.
6. **[circularity] Imported CONFIRMED labels.** Tiers 0/1/3 are tagged CONFIRMED via PROVE-10/emit-spine —
   earned in bbnf-lang's regex/emit context, not parse-that's scanner plane; an in-plane parity check is owed.
7. **[cross-lock, un-named supersession] 9C is quad-homed** — LOCK-5 (grammar/DFA), LOCK-2 (scan-plan
   e-graph island that picks the tier), LOCK-9 (bbnf-simd `classify_tbl4` backend), GN codegen
   (`crates/codegen/src/emit/regex/`, GN-1-post territory). Placing scanner-derivation codegen there is a
   **re-cut of the GN codex that must be a named supersession**, not an implicit LOCK-9 land-grab
   (constitution ix).

**Pass-2 directives.**
- *State explicitly:* PROTO-2 closes **Q2 (register budget) ONLY**; the disposition stays BLOCKED on Q1
  (dense parity) + Q3 (LOCK-5 coupling / two-regex-crate reconciliation). Do not treat "Sheng GO" as a
  whole-family clean bill.
- *Prototype (widen the census to its own killer):* census the **rich-CSS `url()` and a wide-unicode leaf**,
  sourced from `bootstrap/regex.bbnf` (the grammar path, constitution i) — not hand-typed archetypes — so
  the sample includes the infeasibility candidate and the coverage becomes a measured percentage.
- *Prototype (PROVE-9C-B, only if LOCK-5 substrate is ruled buildable):* lower one minimized leaf DFA to a
  `vqtbl1q_u8` transition kernel, measure the class-map∘Sheng composition for the data-dependent serial
  stall, paired ≥0.98× dense + skip-win on interior, `otool` shows `vqtbl`+vshrn never double-`vaddv`.
- *Research must answer:* which regex crate is the DFA source of truth (parse-that fork vs bbnf-lang
  `bbnf-regex`), and does LOCK-5 build the byte-fixpoint-gated grammar→DFA pipeline at 9C's needed fidelity?
  Absent a LOCK-5 GO, 9C cannot converge.
- *Withheld from researchers:* do **not** seed "Sheng GO = 9C converges." The GO is register-budget on an
  ASCII sample that omits the killer; dense parity, grammar-source, and wide-class census are all open.
  Hand them the narrow register-budget fact + the four open items + the LOCK-5/two-crate coupling as a hard
  DAG predecessor.

---

## 9D — LAZY-SURFACE · BLOCKED · 30%

**Floor rationale.** CRIT-1 35 / CRIT-2 30. Take **30**: CRIT-2 correctly weights that even the family's
own "cheap, deterministic" decider (P3 backtrack bound) was skipped, that the memory raison d'être is
refuted-by-default, and that the design trips the Y.13 consumer-less-substrate clause it cites. CRIT-1's
+5 credits the honest self-diagnosis (it names its own killers) and the elegant single-path `Window`-witness
framing — genuine, but "earns nothing on its own." Held BLOCKED (not RETIRED): P3 is cheap and
deterministic and latency/time-to-first-token survives regardless, so a valid close with an artifact is
one prototype away.

**What is EARNED (do not re-litigate).** The most honest self-diagnosis in the set. The
partiality-as-witness formulation (winnow `Partial<I>` model, NOT nom's forked streaming/complete APIs) is
single-path-compliant: the eager whole-input parse is the *degenerate one-window case* (one window = whole
padded buffer, carries zero, `final=true`). bbnf-simd already speaks the carry vocabulary
(`prefix_xor_64(mask,carry_in)`, `escape_mask_64→(u64,bool)`, `CommentCarry`, `bracket_depth_mask_64`),
and `ws_bitmap`/`ws_bitmap_start` (`state.rs:228`) is a real extant ad-hoc lazy window cache to lift.

**Exact open gaps.**
1. **[the cheap decider, skipped] P3 (instrument `furthest_offset − offset` rewind distance across
   JSON+CSS) did not run** — self-described "cheap and deterministic," the plainest prototype-allocation
   miss in the lock. The entire memory raison d'être rests on an unmeasured property.
2. **[missing primitive, refuted-by-default] Backtracking defeats the memory claim.** parse-that is a
   backtracking PEG — `alt`/`recover` can rewind past a window. If rewind is unbounded (recovery to document
   start), the ring buffer must retain all input = the whole-input model with a lazy tail, **zero peak-RSS
   win**. By the family's own words this needs "a bounded backtrack guarantee that parse-that's PEG
   semantics may simply not provide — a property equal in difficulty to redesigning the backtracking
   discipline itself." Failing by default until P3 measures otherwise.
3. **[Y.13 tripped in-design] No streaming consumer named.** value.js / latex-paper / keyframes all parse
   whole documents; simdjson's own `document_stream` is whole-document-granular (streams ND-JSON at
   document granularity, never a byte-partial document). "Consumer-less substrate is forbidden — in designs
   as in code." The design as written trips the clause it cites.
4. **[architectural debt, unresolved] `lazy.rs:20/:49` name-collision** — *combinator-graph* laziness
   (recursion) vs *input-window* laziness (9D); they meet at `Parser::new` but must not be conflated.
5. **[total throughput honesty] 9D must make ZERO throughput claim** — the straddle guard resurrects the
   per-window tail guard `PaddedView` was built to delete → expected neutral-to-slower. Latency/
   time-to-first-token is the only survivable value, and P2 (peak-RSS harness) that would prove even that
   did not run.
6. **[cross-lock] KernelPlan double-consumes an unbuilt LOCK-2.** 9D's compile-time `KernelPlan` folds
   PROVE-10's runtime prefilter + bbnf-simd's OnceLock dispatch into a compile-time-selected plan — that
   plan-selection-at-compile-time IS LOCK-2's decision-driver territory (the same island 9C needs).
   Substrate-less consumer on a different axis from 9C.

**Pass-2 directives.**
- *Prototype (the cheap decider — mandatory):* run P3 — instrument `furthest_offset − offset` rewind
  distance across JSON+CSS (with and without `diagnostics` recovery). Bounded (< a small window) → a bounded
  ring buffer is sound and the memory win holds; unbounded → **file the measured refutation** (memory center
  defeated, only incremental-feed/latency survives). Either outcome is a valid close with an artifact.
- *Prototype (Y.13 or refute):* name and ship a real chunked-feed streaming consumer measuring peak RSS +
  time-to-first-structural (P2), or file that no streaming consumer exists and 9D collapses to latency-only.
- *Research must answer:* is there ANY in-wave streaming consumer for parse-that's actual grammars? (simdjson
  `document_stream` says the realistic unit is a complete small document, weakening the peak-RSS argument.)
- *Withheld from researchers:* do **not** seed "streaming saves memory." It is refuted-by-default until P3
  measures a bounded backtrack AND a real consumer materializes. Hand them the cheap P3, the Y.13
  requirement, and the zero-throughput-claim law.

---

## CROWDING & REDIRECT (this pass)

**The central axis (9A⊗9B) is convergent at the MECHANISM level but the decider is unrun — hold both,
bank neither.** Both routes reach the *same* movemask idiom (`vshrn`) and the *same* checkasm discipline;
they differ only in whether one file or two files carry them (FAMILY-SPECS). That is convergence — but the
crowding-redirect rule (bank the redundant one) does **not** apply, because the redundancy is *not shown*:
the decider (drift cost vs coupling cost, gate 5) was unrun on both sides, and CRIT-2's X-1 establishes it
is a **LOCK-6 verdict** LOCK-9 cannot render. Banking either family now would be picking the axis without
evidence — the exact move both critics forbid ("any LOCK-9 convergence that claims to settle the axis
internally is contradicting the portfolio's homing of LOCK-6"). **9B holds as incumbent-default (PT-2
frozen codex); 9A must earn its place by producing the two missing measurements as LOCK-6's inputs. Neither
banked; the axis stays open pending LOCK-6.**

**The one genuine crowding to redirect: the movemask FORM is a triple-home convergence → open the
underexplored lean formulation.** THREE entry points to "narrow a compare to a mask" coexist: (1) bbnf-simd
`movemask_u8x16` — VERIFIED dense-u16 via `vshrn`+repack; (2) PROVE-10 memchr — lean
`shrn.8b→fmov→4-bit-stride`, no repack; (3) parse-that `to_bitmask` sites. This is load-bearing for BOTH 9A
(inherits (1)'s repack — 9A-gap-2/3) and 9B (claims to elide it — 9B-gap-4), and neither probe touched it.
**Redirect:** open the **lean nibble-stride, no-repack movemask as the candidate single canonical producer**
— the currently-contested, unmeasured formulation — as a directed pass-2 probe with an instruction-count
gate. The lock cannot converge either family while the canonical movemask form is undecided; the *collapse
to one lawful producer* is a LOCK-9∩LOCK-5 gestalt act (memchr lives in bbnf-regex's prefilter), but the
*measurement* (repack vs lean) is LOCK-9's to run.

**9D's KernelPlan is not a new subsystem to bank** — it is a consumer of LOCK-2's decision driver (same
island 9C's tier-selection needs). No redirect within LOCK-9; flag as a LOCK-2 DAG dependency.

## CROSS-POLLINATION (named grafts — both families developed enough; graft justified)

1. **9C ⊗ 9D — the compiled resumable scanner (PRIMARY, gestalt-Ω consolidation candidate).** research-D §3
   (Ragel/re2c lineage): a streaming scanner IS a resumable DFA (`write exec` saved-state model). 9C's Sheng
   DFA and 9D's carry-threaded windowed scanner are **the same object viewed on two axes** — the strongest
   positive affinity in the incompatibility map. Both are developed enough that the graft is named: 9C
   supplies the compiled DFA *body*, 9D supplies the resumable *calling convention* (the `Window` witness +
   carries, which bbnf-simd already exports). **If both survive their gates, 9C's compiled DFA is the natural
   body of 9D's windowed carry-threaded scanner.** Consequence: neither 9C nor 9D is independently
   converge-able as a separate LOCK-9 family — the gestalt-Ω should carry both as inputs to ONE "compiled
   resumable scanner" and collapse, not book them as two convergence tracks (a mild double-count the roll-up
   acknowledges).
2. **9A/9B kernel home ⊗ 9D carry vocabulary (SECONDARY, orthogonal-composable).** 9D's `Window` witness
   threads carries into *whichever* kernel-home verdict wins — 9A's shared kernels, 9B's native kernels, or
   9C's compiled DFA products (9D explicitly names bbnf-simd's carry kernels as its streaming primitive set).
   9D is a *later, conditional overlay* on the kernel-home/provenance verdict, admissible only if P3 measures
   a bounded backtrack. Justified: 9D changes *what feeds* the kernels, not *who owns* them — the axes are
   orthogonal (input-surface ⊥ kernel-home).

## CROSS-LOCK DAG ORDERING (for GESTALT — blockers LOCK-9 CANNOT retire alone)

- **9A⊗9B → LOCK-6.** The kernel-home decider is "the reasonable point of rust↔rust isomorphism"
  (Constitution iii) resolved by "null hypothesis defeated by measured drift/maintenance cost" — LOCK-6
  clause iii + LOCK-6D verbatim. LOCK-9 produces the two measurements (9A vector-tier inline+no-regress, 9B
  gate-5 divergence); **LOCK-6 renders the verdict. LOCK-9 ⊗ LOCK-6 co-converge in GESTALT; neither closes
  the axis alone.**
- **Canonical movemask form → LOCK-9 ∩ LOCK-5.** The triple-home (bbnf-simd repack / PROVE-10 lean /
  parse-that `to_bitmask`) must collapse to ONE lawful producer; memchr's lean form lives in bbnf-regex's
  prefilter (LOCK-5's crate). LOCK-9 runs the repack-vs-lean measurement; the gestalt collapses the home.
- **Prefilter double-home → GN-2/PT-1 dispatch law.** `bbnf-simd::find_ascii_set_member64` ⟷
  `bbnf-regex::Dfa::find` are two entry points to one prefilter idea ("one lowering per scan site, never
  both"). 9C *deepens* this (it lowers scanners through the DFA). LOCK-9 must defer to the GN-2/PT-1
  dispatch law it inherits, not silently pick one.
- **9C grammar→DFA source-of-truth → LOCK-5** (unbuilt substrate, two-regex-crate reconciliation); **9C
  tier-selection → LOCK-2** (scan-plan e-graph island); **9C codegen home
  `crates/codegen/src/emit/regex/` → a named GN-codex supersession** (constitution ix). 9C is quad-homed and
  cannot be *selected* until LOCK-5 + LOCK-2 land.
- **9C/9D resumable-scanner collapse → gestalt-Ω** (cross-pollination 1 above).
- **Un-homed consumer edge → LOCK-4/LOCK-8.** The reborn scanner surface feeds the combinator core (LOCK-4)
  and, via LOCK-6, the TS backbone (LOCK-8); no 9-family names how it presents to LOCK-4's parser graph or
  whether LOCK-8's TS side mirrors 9C's compiled-DFA products. Not a contradiction — an un-homed edge the
  gestalt-P weave must close, recorded so it is not discovered late.

---

## PASS HISTORY

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
- **PROTO-2 disposition:** real, deterministic, cleanly closed Q2 — 5/5 archetype leaves fit strict
  `vqtbl1q_u8` (number 9×7=63 the tight boundary, had to be measured). **But scoped to dodge its own
  killer:** the sample excludes the named explosion candidate (rich-CSS `url()`/wide-unicode — "byte-mode ==
  unicode-mode *here*" only because no wide-unicode leaf was present), and leaves were hand-typed generic
  archetypes fed straight to `Dfa::compile_with`, NOT sourced from `bootstrap/regex.bbnf` (the grammar-source
  path — constitution i — was bypassed), and through parse-that's *local* regex fork (`path = "regex"`), not
  bbnf-lang's `bbnf-regex`. **SETTLED (narrow):** the transition-table register budget is GO for the ASCII
  archetype family. **NOT SETTLED:** "no coverage-percentage caveat" (over-claims a fit-chosen sample); dense
  parity (Q1); the grammar-sourced/wide-class census; the class-map∘Sheng data-dependent-stall composition.
- **New substrate finding (CRIT-1):** the 9C LOCK-5 coupling is potentially **two** regex crates to
  reconcile (parse-that/rust/regex ⟂ bbnf-lang/crates/bbnf-regex), not one.
- **Dispositions set:** 9A ADVANCED born-RED 55; 9B ADVANCED incumbent 55; 9C BLOCKED 44; 9D BLOCKED 30.
  No family banked — the central axis is convergent-in-mechanism but its decider is a deferred LOCK-6
  verdict, so neither 9A nor 9B may be banked without evidence.
- **Crowding/redirect:** hold 9A/9B open (axis → LOCK-6); OPEN the underexplored lean-nibble-stride
  no-repack movemask as the contested canonical producer (directed pass-2 probe + instruction-count gate);
  name the 9C⊗9D compiled-resumable-scanner cross-pollination for gestalt-Ω (neither independently
  converge-able — a mild double-count).
- **Pass-2 keystone directives:** (1) **run the 9A⊗9B decider** — 9B gate 5 (LOC/substrate divergence) +
  9A vector-tier cross-crate inline + no-regress on a **live** scanner (not prefix_xor, not a micro-bench) —
  the sharpest uncertainty in the lock, touched by neither probe; (2) re-probe 9A on a **real NEON body** to
  settle the contested movemask + add the instruction-count gate arm; (3) **BANK 9B's number_simd
  double-`vaddv`→`vshrn`** (cheapest decisive artifact, off SPEC-ONLY, removes a live refutation-law
  violation); (4) **run 9D-P3** backtrack-bound (cheap, deterministic, decides the memory center); (5) widen
  the 9C census to its killer (rich-CSS/wide-unicode, grammar-sourced) and state Q2-only. Promote every
  un-run gate to born-RED must-run; do not let elegance stand in for the deferred measurement.
