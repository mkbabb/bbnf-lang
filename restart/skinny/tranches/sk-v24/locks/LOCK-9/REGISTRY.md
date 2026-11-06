# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. The lock is not four features — it is four
dispositions of one aarch64 scanner/kernel plane on **three distinct axes**: **kernel-home** (9A one
shared crate ⊥ 9B parse-that keeps its own — the central adjudication), **scanner-provenance** (9C
derived-from-DFA vs authored {9A,9B}), **input-surface** (9D windowed vs whole-input — orthogonal,
contradicts the shared eager `PaddedView` invariant).*

**Live at pass 3.** Inputs folded: FAMILY-SPECS pass-3 (4 specs + the six reconciliations + M/S/X
tiering + incompatibility map — **STALE against its own protos for the SECOND consecutive pass**,
authored 22:44 before PROTO-1 landed 23:23 and PROTO-2b 23:29); research-{A,B,C,D,priorart} pass-3;
PROTO-1 (9A **evolved-home construction** — canonical DENSE-64 `vpaddq` authored, all four double-`vaddv`
purged in scratch bit-identically, the LOC/substrate divergence table emitted, RAN_MEASURED); PROTO-2 +
PROTO-2b (9C emit-pass feasibility against the **real** bbnf-regex DFA — membership emit == oracle,
`byte_classes`-direct == oracle, gate-4 lo6-collision routed off `vqtbl4q` with no silent drop,
RAN_MEASURED); CRIT-1 (within-lock: 9A 50 / 9B 58 / 9C 46 / 9D RETIRE); CRIT-2 (cross-lock: 9A 53 / 9B 60
/ 9C 46 / 9D 90-as-RETIRED).

**The two pass-3 headlines the synthesis has not absorbed (same pipeline-order defect as pass 2):**
(1) **PROTO-1 produced the axis-deciding LOCK-6 divergence datum the FAMILY-SPECS still lists as "neither
pass produced"** — and, read as data, that datum **tilts to 9A, not 9B** (9B must author the checkasm
oracle + the `vqtbl4q` table + the stable dialect + the x86 purge, all of which 9A inherits; the
genuinely domain-fit residual — `PaddedView`-64 vs `eob_pad_clamp`, per-16 vs 64-byte — is small). The
synthesis's "9B incumbent-default" thumb is **not supported by the datum its own probe produced**. (2)
**PROTO-2/2b retired the 9C elegant-reduction trap FOR MEMBERSHIP** — but every probe leaf compiled to
`num_classes = 2` (a *binary* member/non-member split); the multi-kind `vceqq`-fan STRUCTURAL classify
(the actual per-kind JSON/CSS structural index) is **emit-UNPROVEN** — the harder half was skipped for the
tractable one.

**Substrate re-verified THIS session by the agglomerator (against the live trees):**
- parse-that `simd128.rs` **does NOT exist** (`find` empty) — research-B's mechanism is anchored to a
  **phantom file** ("`simd128.rs:322` — the single point of correction that fixes every vector-tier
  consumer at once"); the real de-nightly rewrites inline `to_bitmask` across **7 scan files** (verified
  hit-list), there is no single point.
- parse-that `number_simd.rs:54-55` double-`vaddv` (`vaddv_u8(lo_masked)`/`vaddv_u8(hi_masked)`) **STILL
  LIVE** in the owner tree — refutation-law iv stated-and-violated-in-place for the **third** pass.
- parse-that `rust/.cargo/config.toml` patches `bbnf-regex = { path = "regex" }`; **`bbnf-simd` ABSENT**.
  The file is **untracked** (`git check-ignore` → not ignored; `git ls-files` → not tracked) — **neither
  gitignored (research-A) NOR tree-of-record (FAMILY-SPECS)**; it is a local, un-versioned build-config.
  9A's "one line" P23 obligation is real but **lighter** than a source mutation (resolves CRIT-1 §9A-3).
- bbnf-simd `aarch64/`: the four double-`vaddv` sites live at *exactly* the named lines
  (`byte_class_from_eq_set_64.rs:83-84`, `structural_classify_64.rs:151-152`, `comment_body_mask_64.rs:72-73`,
  `bracket_depth_mask_64.rs:78-79`); `vpaddq_u8` in **zero** files; **no other `vaddv` anywhere in
  bbnf-simd/src** (no digit-MAC site) → **9A gate (1) is mis-specified** ("must return ONLY the digit-MAC
  site" — there is none; a correct purge returns *nothing*, which PROTO-1 confirms).
- skinny `crates/parse-that-regex/src` = `{integration, number, unicode, lib.rs}` — **no `automata/`**;
  `crates/bbnf-regex/src` = `lib.rs` (322-LOC stub). **No in-tree DFA source-of-truth** — 9C's substrate
  is LOCK-5/GN-1-owned and un-executed.

---

## STATUS TABLE (after pass 3)

| Family | Status | Conv. | Δ p2→p3 | One-line |
|---|---|---:|:--:|---|
| **9A ONE-KERNEL-LIBRARY** | **ADVANCED · born-RED** | **50%** | 52→50 | PROTO-1 delivered the evolved home **bit-identically** — canonical DENSE-64 `vpaddq` authored (0/200k vs scalar AND vs the refuted form, 7 `uzp/orr`, **0** horizontal reduction), all four double-`vaddv` purged in scratch (build green, gate clean), **the LOC/substrate divergence table emitted** — the LOCK-6 datum the STALE spec still lists as "neither pass produced," and one that reads *for* 9A. But the CSS perf floor is **zero-measured for a THIRD pass** (excused as "wash by law" via a near-vacuous gate), gate (1) references a non-existent digit-MAC residual, the in-tree purge is scratch-only, and the `vector/` module is SPEC-ONLY. Value is **maintenance/drift + refuted-idiom purge, NOT a speedup**. |
| **9B NATIVE-REBIRTH** | **ADVANCED · incumbent** | **58%** | 62→58 | Banked LEAN/DENSE disasm wins stand (the only family off SPEC-ONLY). Docked hard: research-B's de-nightly mechanism is anchored to a **phantom `simd128.rs`** (VERIFIED absent — its claimed "single point of correction"), so the stable-cutover blast-radius is **unsubstantiated**; the headline (off nightly) was **never left** (rebirthed *on* nightly, gate 6 unrun); the double-`vaddv` is **violated-in-place a THIRD pass** in the owner tree; the DENSE bit-parity oracle is unbuilt; and PROTO-1's delivered gate-5 datum reads **against** the incumbent-default (9B must author what 9A inherits). Its only failure mode remains "lose to 9A on the axis" — a LOCK-6 call. |
| **9C SCANNER-AS-DFA** | **BLOCKED · candidate** | **46%** | 43→46 | **The pass-3 mover.** PROTO-2/2b are the cleanest artifacts in the lock: the `[u8;256]→SIMD` emit pass is **measured-feasible against the real bbnf-regex DFA** (three leaves emit a bit-identical classifier over an 8129-block alignment sweep, `byte_classes`-direct == `find_at` oracle, gate-4 lo6-collision provably routed off `vqtbl4q` with no silent drop, INJECT_BUG 100%, `\p{L}`=41cls/21st correcting the `→None` prediction) — decoupling feasibility from the un-executed vendoring. **But the multi-kind `vceqq`-fan STRUCTURAL emit is UNPROVEN** (all leaves `num_classes=2`, binary membership); Tier-S "MEASURED NEGATIVE" is inferred not head-to-head; the census disagrees across researchers; and 9C stays triple-lock-coupled (LOCK-5 un-vendored + LOCK-2 island + GN-codex). |
| **9D LAZY-SURFACE** | **RETIRED** | **85%** | (was BLOCKED 30) | **Converted to RETIRED per the pass-2 terminus** ("pass-3 runs P3 or RETIRES"): research-D delivered the P3 instrumentation **spec a third time, not the histogram** — P3 unrun for a THIRD consecutive pass. The RETIRE is over-determined and directive-sanctioned: the memory center is refuted on **independent** grounds (streaming consumer grep-absent, span-dangle-vs-zero-copy wall, PaddedView anti-chunking, §0/51 fold-over-lazy dead-by-law), so the unrun histogram no longer gates anything. Residue re-homed: (a) the cost-model `KernelPlan` selection → LOCK-2; (b) carry-threading → the 9C⊗9D gestalt-Ω. |

Net: **no family converged; no family banked.** The central axis (9A⊗9B) is convergent-in-mechanism and a
**performance wash** — but pass-3 furnished the maintenance-cost datum LOCK-6 renders on, and it **tilts to
9A** (the incumbent-default thumb is retired). 9C rose off its floor for the first time on the lock's
cleanest concrete work, but its harder (multi-kind) half is untested and it stays cross-lock-blocked. 9D
retires. **The dominant structural failure recurs a THIRD time: the FAMILY-SPECS froze (22:44) before its
two most consequential probes finished (23:23, 23:29), so it lists prototype-closed items as open and
re-commissions completed work as pass-4 BRIEF 1** — the reconciliation the synthesis owes before gestalt.

---

## 9A — ONE-KERNEL-LIBRARY · ADVANCED · born-RED · 50%

**Floor rationale.** CRIT-1 50 / CRIT-2 53. Take **50** (CRIT-1's, the lower). No reason CRIT-1 erred: its
−2 from pass-2's 52 correctly composes a delivered-but-already-priced artifact against **new** defects. The
purge + divergence table PROTO-1 delivered were *commissioned and born-RED-inferred at pass 2* (the 52
already priced their feasibility), so delivering them **confirms** the disposition rather than raising it;
meanwhile the CSS perf floor is now zero-measured for a **third** pass (a hardening negative, not a neutral
one), gate (1) is mis-specified (verified: no digit-MAC `vaddv` exists in bbnf-simd), the in-tree purge is
scratch-only, and the family's whole value proposition has settled to "maintenance/drift-avoidance + the
refuted-idiom purge, **NOT a speedup**." CRIT-2's 53 credits the delivered artifact but sits on its own
higher pass-2 scale (55→53); the honest composition is the lower floor. ADVANCED born-RED stands — no
missing primitive of equal difficulty (live cross-crate inline was measured-GREEN pass-2; the producer is
now authored; the de-face is a known refactor).

**What is EARNED this pass (banked, do not re-litigate).**
- **[BANKED] The evolved home is constructed, bit-identically.** PROTO-1 authored the canonical DENSE-64
  `vpaddq` producer (~15 LOC), **0/200k mismatches vs scalar AND vs the refuted double-`vaddv`**, lowered
  by LLVM to **7 `uzp1/uzp2`+`orr.16b`, ZERO horizontal reduction** (vs the refuted 8 `addv.8b`); purged
  **all four** double-`vaddv` sites in a scratch bbnf-simd that **builds green** with the gate `rg vaddv`
  clean and `vpaddq` present ×4. This retires the pass-2 "the purge has no artifact" gap — the purge is
  proven feasible and parity-preserving (checkasm-safe by construction: bit-identical to the refuted form).
- **[BANKED] The LOC/substrate divergence table — the LOCK-6 maintenance-cost datum — is delivered.**
  Nightly↔stable dialect, `PaddedView`-64↔`eob_pad_clamp`, per-16↔64-byte granularity, `to_bitmask`↔hand
  `vshrn`, none↔`vqtbl4q` table, oracle-absent↔checkasm-present, x86-live↔x86-clean, LOC ~2956↔~1351. The
  datum "neither pass produced" **was produced** (23:23).
- **[STILL EARNED from pass 2]** live cross-crate `#[inline]` is measured-GREEN (0 in-loop indirect across
  the real rlib seam); the physical seam is one line in an untracked `.cargo/config.toml`; the refuted
  double-`vaddv` lives in the shared home (must be purged before adoption).

**Exact open gaps.**
1. **[gate (1) mis-specified — agglomerator-verified] the R-A3 purge gate.** "`rg 'vaddv_u8|vaddvq_u8'
   skinny/crates/bbnf-simd/src/aarch64` must return ONLY the digit-MAC site" — **there is no digit-MAC
   `vaddv` anywhere in bbnf-simd** (verified: the only `vaddv` in the whole crate are the four mask
   producers; the digit-MAC `vaddv` the binding-laws cite is `number_simd.rs:54-55`, in **parse-that**, a
   different home). A correct purge returns **nothing** (PROTO-1 confirms "none"). Fix the gate text to
   "returns nothing." The purge itself must still become a **blocking born-RED gate** — a shared home that
   ships the refuted idiom violates §0 iv.
2. **[perf floor zero-measured for a THIRD pass, wash-vs-testable tension unresolved] gate (2).** The
   paired cold **CSS** ratio (`parse_that_css → scan_ws_block_comments → classify_stripe`; the JSON corpus
   is a null for `classify_stripe`) has never executed across three passes. CRIT-1's tension is
   load-bearing: the spec excuses the missing measurement **as a wash by law** (OnceLock free at cgu=1 +
   author-once) yet **lists it as a born-RED gap** — a gate that can only pass if the axis truly washes, so
   it tests nothing the checkasm parity gate doesn't already catch. **Resolve:** either it is genuinely
   measurable (then run it on a quiet machine) or it is a wash (then delete the gate). The spec cannot want
   both.
3. **[the in-tree purge is un-executed] G9A-3.** PROTO-1 purged a *scratch copy*; the canonical bbnf-simd
   still ships four double-`vaddv` (verified). Adopting 9A requires the purge land in-tree first.
4. **[SPEC-ONLY] the `vector/{mod,neon,scalar}.rs` module does not exist** (verified absent). The ~60-LOC
   shared `u8x16`/`mask8x16` vocabulary the vector-tier files consume is unbuilt; promoting it needs its
   own no-regression artifact.
5. **[cross-lock, P23 — obligation is LIGHTER than stated] the one-line `[patch]` seam.** `.cargo/config.
   toml` is **untracked** (neither gitignored nor tree-of-record). The mutation is to a local build-config,
   not source — a real but light P23 obligation, still **named-not-granted**. (Note the **P23 asymmetry**
   CRIT-1 flags: 9A's one untracked line carries the supersession flag while 9B's *far larger* in-place
   mutation — rewrite 8 files, delete ~200 LOC, purge x86 in the owner tree — carries none; reconcile.)
6. **[cross-lock] the axis close is LOCK-6's** — the corrected datum tilts to 9A (§CROWDING), but LOCK-9
   cannot render it.

**Pass-3 disposition on the crux.** 9A's physical promise is proven and its evolved home is now
*constructed* (in scratch, bit-identically). Its value is honestly maintenance/drift-avoidance — the perf
axis washes — and the delivered divergence datum reads **for** 9A (9B must author the oracle, the table,
the stable dialect, and the x86 purge that 9A already carries). The only residual measurement is the CSS
perf FLOOR (a no-regression, not a speedup), which is either a wash-by-law (delete the gate) or must
finally be run after three empty passes.

**Pass-4 directives.**
- *First re-cut the STALE gaps* (both critics' headline): 9A Q1/gate-4 (author + purge + divergence table)
  **RAN** — do not carry the whole datum as open; the residual is only (a) the CSS perf floor and (b)
  LOCK-6's verdict.
- *Prototype (finally close the perf half OR delete the gate):* resolve the wash-vs-testable tension. If
  testable, run paired cold `parse_that_css` N≥200, two windows, on a quiet machine (loadavg <2) — three
  passes of loadavg-5-9 deferral is itself a signal the gate needs re-scoping.
- *Prototype (the in-tree purge + the one-line path-dep seam under fat-LTO cgu=1):* land the purge in a
  scratch bbnf-simd tree and rewire via the **path-dep** (not the vendor fallback PROTO-1's ancestor
  tested), `otool -tv` the wired loop for zero in-loop indirect.
- *Research must answer:* the scanner-file mapping census is **unreconciled** across researchers (see 9C
  §census) — settle the kernel-tier/vector-tier/no-go split for 9A's ~8 files. Fix gate (1)'s text.
- *Withheld from researchers:* do **not** re-seed "the OnceLock barrier costs 0.92×" (OVERTURNED, free at
  cgu=1), "the free `vshrn` movemask" (REFUTED, absent from bbnf-simd), or "9B inherits the same OnceLock
  constraint" (FALSE, 9B has no facade). Hand them: the evolved home is *built* (purge + divergence table
  delivered, bit-identical); the datum tilts to 9A; the perf axis washes; the CSS floor is the sole
  residual measurement; gate (1) fixed to "returns nothing"; the P23 seam mutates an untracked file.

---

## 9B — NATIVE-REBIRTH · ADVANCED · incumbent · 58%

**Floor rationale.** CRIT-1 58 / CRIT-2 60. Take **58** (CRIT-1's, the lower). No reason CRIT-1 erred: its
−4 from pass-2's 62 docks for the three sharpest 9B facts, all of which the agglomerator re-verified. The
banked LEAN/DENSE disasm wins are real (do-not-re-litigate) and keep 9B the only family off SPEC-ONLY, so
it does not fall further; but (i) research-B's de-nightly mechanism is **anchored to a phantom
`simd128.rs`** (verified absent — its claimed "single point of correction" and "port by one `use
crate::simd128::*` line, 7 files"), so the **stable-cutover blast-radius is unsubstantiated** (the real
cutover rewrites inline `to_bitmask` across 7 files); (ii) the **headline** — stable, off nightly — was
**never verified** (rebirthed *on* nightly-2026-04-11; gate 6 unrun, and its research cost-estimate rests
on the phantom, so it is doubly-unsubstantiated); (iii) the double-`vaddv` refutation law is
**violated-in-place a THIRD pass** in the owner tree (`number_simd.rs:54-55`, verified live). CRIT-2's 60
priced the phantom into a −2; CRIT-1's 58 priced the stable-cutover overclaim into a −4 — the two docks
**converge on the same gate** (gate 6, the stable cutover is unsubstantiated), not independent defects to
sum, so the floor sits at 58, not lower. The banked disasm anchors it there.

**What is EARNED (banked artifacts, do not re-litigate).**
- **[BANKED, pass 2] LEAN cheapest win.** `number_simd.rs:54-55` double-`vaddv` → `vshrn_n_u16::<4>`
  (`shrn.8b #4`, **0** `vaddv`, 14 vs 19 insns −26%); the 2271-vs-509 defect does not reproduce.
- **[BANKED, pass 2] DENSE win.** `classify_stripe_64` `vpaddq` tree = 50 vs 64 insns (−22%), **0**
  reduction ops (LLVM lowers to `uzp`/`orr`). Both paired ≥0.98× (15/16).
- **Settled taxonomy:** contract-split — LEAN nibble-`vshrn`, DENSE-64 `vpaddq` tree, FORBIDDEN
  double-`vaddv`; stable `core::arch::aarch64` since 1.59; NEON baseline-mandatory. Correctly NOT BLOCKED.

**Exact open gaps.**
1. **[mechanism anchored to a phantom file — agglomerator-verified] G9B-1.** research-B §3/§2 name
   `simd128.rs:322` as "the single point of correction that fixes every vector-tier consumer at once" and
   "each scanner ports by one `use crate::simd128::*` line (7 files, one line each)." **`simd128.rs` does
   not exist** (`find` empty). The FAMILY-SPECS silently substitutes a *new* `parse_that/src/simd/{lean,
   dense}.rs` dir-module (a lawful proposal, also absent) but never flags the shim's absence — so the
   de-nightly cost is unsubstantiated. The real cutover rewrites inline `to_bitmask` across **7 scan
   files** (verified hit-list), not "7 files, one line each."
2. **[headline UNVERIFIED — never left nightly, cost rests on the phantom] gate 6.** research-B §5 asserts
   "compiles on stable 1.96.1, zero errors in the 12 touched files" with **no build-log artifact** — an
   unlabeled feasibility claim. The rebirths were authored *on* `nightly-2026-04-11`. `cargo +stable
   build` / `portable_simd` deletion / the `smallbox` nightly→stable cut / the `regex/generated.rs`
   `fmt_helpers_for_derive` regen-fix were **not exercised**. 9B's central promise is untested and its
   blast-radius estimate is phantom-anchored (G9B-1).
3. **[refutation-law violated in-place, THIRD pass] G9B-2.** `number_simd.rs:54-55` double-`vaddv` still
   live in the owner tree (verified). Abrogation proposed across three passes, executed in none (P23
   scratch only). This is a §0 iv violation the incumbent tree *ships today*.
4. **[gate 5 delivered — and reads AGAINST the incumbent] G9B-3.** PROTO-1's evolved-home divergence
   table IS 9B's gate-5 datum too (same axis datum), and read honestly it enumerates what **9B must
   author that 9A inherits** — the checkasm scalar-parity oracle (parse-that has "parsed without panic"
   only), the `vqtbl4q` lo6 table classifier (no `vqtbl` in parse-that scan/), the stable `core::arch`
   dialect (9B still on nightly), and an x86-clean surface (9B carries dead SSE4.2 to purge either way).
   The genuinely domain-fit residual (`PaddedView`-64 vs `eob_pad_clamp`; per-16 vs 64-byte) is small. The
   "9B incumbent-default" framing is **not supported** by the datum 9B's own axis probe produced. (§CROWDING.)
5. **[DENSE bit-parity oracle unbuilt] gate 3.** DENSE correctness is "parsed without panic," not
   bit-parity against the scalar/`to_bitmask` baseline. The native checkasm harness (INJECT_BUG ≥90%,
   alignment sweep, `black_box`) is a plan, not an artifact.
6. **[self-regression negative undemonstrated] gate 4/Q2.** The naive-nibble-stride-corrupts-DENSE guard
   is *used* correctly but never *shown* as the load-bearing negative (PROTO-2/2b demonstrated the
   lo6-collision drop, a **different** negative for 9C; 9B's `<<k*16`-stitch corruption is unshown).
7. **[cross-lock] the axis close is LOCK-6's** — 9B produced its half; it cannot self-declare.

**Pass-4 directives.**
- *First re-cut the STALE gap:* gate 5 "SEEDED not closed" — the divergence table **is delivered** (and
  reads against 9B); the residual is not "produce the table" but the CSS perf ratio + LOCK-6's call.
- *Prototype (run the stable cutover — the headline):* `cargo +stable build -p parse_that` after the
  `regex/generated.rs` regen-fix + `smallbox` cut; count residual `#![feature]`; this is the single
  sharpest 9B gap and it is unmeasured after three passes on the nightly it claims to shed.
- *Prototype (bank DENSE correctness):* the bit-parity oracle for the `vpaddq` tree + demonstrate the
  naive-nibble-stitch negative on `classify_stripe`.
- *Research must answer:* correct research-B's phantom `simd128.rs` mechanism to the real 7-file inline
  `to_bitmask` surface; substantiate the stable-cutover blast-radius with a build log, not an assertion.
- *Withheld from researchers:* do **not** re-seed "single point of correction (`simd128.rs`)" (PHANTOM),
  do **not** carry the un-evolved-baseline divergence as decisive (the *evolved*-home table now exists and
  reads against 9B), do **not** let "9B is the PT-2 incumbent, default to it" substitute for the gate-5
  reading. Zero "faster" in any gate. Hand them the banked LEAN/DENSE wins + the null-is-valid framing (9B
  concedes honestly if divergence measures small against the evolved home — which it now measurably does).

---

## 9C — SCANNER-AS-DFA · BLOCKED · candidate · 46%

**Floor rationale.** CRIT-1 46 / CRIT-2 46. Both concur **46** — take it (up from pass-2's 43). The uptick
is the pass-3 emit-feasibility delivery (PROTO-2/2b, the lock's cleanest artifacts); the temper below any
higher read is the **multi-kind gap both critics independently name**: every probe leaf compiled to
`num_classes=2` (binary membership), so the `vceqq`-fan multi-kind STRUCTURAL classify — the actual
per-kind JSON/CSS structural *index* — is emit-UNPROVEN. BLOCKED (candidate) stands: 9C is cross-lock
coupled to LOCK-5 (un-executed automata-tier vendoring — verified: no skinny `automata/`), LOCK-2 (tier
island), and a GN-codex codegen-home supersession; its dominant-pass value is parity-only; and the harder
half of its own thesis is untested. Not equal-difficulty (the engine exists+green in parse-that,
`lo6_table_admissible` is a shipped half of the separability predicate) — hence BLOCKED-candidate, not
hard-BLOCKED.

**What is EARNED this pass (banked feasibility, do not re-litigate — for MEMBERSHIP).**
- **[BANKED] The `[u8;256]→SIMD` emit pass is measured-feasible against the REAL bbnf-regex DFA.**
  PROTO-2/2b, three leaves (ws `[ \t\n\r]+`, JSON structural `[{}\[\]:,"\\]`, lo6-colliding CSS `[;{}:]`):
  each emits a classifier **bit-identical to the DFA scan** over an 8129-block alignment sweep (0
  mismatches), the **`byte_classes`-direct** partition == the **`find_at` oracle** partition (two
  structurally-different DFA reads converge), INJECT_BUG bites **100%**. This substantially retires the
  pass-2 "elegant-reduction trap" — the emit is built, not argued — **for the membership tier.**
- **[BANKED] Gate 4 (the load-bearing separability proof) is genuine, not a gate-that-cannot-fail.** The
  `;`(0x3B)/`{`(0x7B) lo6 collision: a naive lo6 table **silently drops** `;` (`naive=0x…0400` vs
  `scalar=0x…0408`, differ=true); `lo6_table_admissible=false` routes the set **off** `vqtbl4q` to the
  full `[u8;256]` tier, which is bit-identical. Provably no silent drop.
- **[BANKED, census correction] `\p{L}` = 41 classes / 21 states** against the real engine — it does
  **NOT** return `None` (correcting the spec's `dfa.rs:262→None` prediction); Tier-X exclusion is by
  class-count over the SIMD membership-tier ceiling, not DFA explosion.
- **[EARNED, pass 1] Q2 register budget** GO for the ASCII archetype family (5/5 fit strict `vqtbl1q_u8`).

**Exact open gaps.**
1. **[the sharpest 9C uncertainty, UNTESTED — both critics' headline] G9C-1: membership proven,
   multi-kind STRUCTURAL unproven.** All three probe leaves are `num_classes=2` (binary member/non-member;
   even `json_structural` collapses to one membership class — the DFA does **not** distinguish `{` from
   `:`). But the actual JSON/CSS structural *index* simdjson builds needs **per-kind** tokens (which
   structural byte, for the tape) — the `vceqq`-fan's raison d'être. PROTO-2 cross-checked the fan **once**
   on the same binary set; PROTO-2b's direct path routes only to Tbl4/FullTable and **never exercises the
   fan**. The census's "Tier structural (multi-kind, per-byte distinct)" and research-C's `url()` ~6-class
   row are **emit-UNPROVEN**. 9C's dissolution claim is **narrower than stated** — proven for *is-member*,
   not *which-kind*.
2. **[MEASURED-label misuse] G9C-2: Tier-S "MEASURED NEGATIVE" is NOT measured this pass.** The serial
   `vqtbl1q` chain negative is anchored on the *directive's* assertion + `ws_comment` run-length stats
   (≤4 bytes 84–100%) — an **inference**, not a pass-3 head-to-head. research-C §7 item 4 still lists
   "Serial-vqtbl negative, reproduced" as a prototype-MUST-test (unrun). Downgrade "MEASURED NEGATIVE" →
   SPEC-ONLY/carried until the serial-vqtbl-vs-scalar run executes.
3. **[unreconciled census] G9C-3: the scanner-file trichotomy disagrees across researchers.** research-A:
   **4** kernel / 3 vector / **2** no-go. research-B: 4 kernel / 3 vector / **1** no-go. research-C: **2**
   1:1-kernel / 2 vector / **4** no-go. FAMILY-SPECS picks "~3 dissolve / ~4 carry-stateful" without
   reconciling research-C's own "2 dissolve" self-count. The count of files that *dissolve* is not settled
   — a within-lock census owed (shared with 9A's census gap).
4. **[the emit pass is a scratch example, not the codegen home] G9C-4.** PROTO-2/2b built `examples/
   emit_probe*.rs` in a scratch `parse-that/rust/regex` copy — feasibility, not the production
   `crates/codegen/src/emit/regex/` pass. The named GN-codex supersession (constitution ix ✓) is declared,
   unbuilt. The separability solver (bounded CSP over the 256-entry partition) is the family's new code and
   is unbuilt (`equiv.rs` emits scalar only) — `lo6_table_admissible` is a shipped half, so NOT
   equal-difficulty.
5. **[cross-lock, unnamed coupling — CRIT-2 §3.3] G9C-5: 9C-feasibility ⊨ 9A-home.** The emit was proven
   against **bbnf-simd's** kernel vocabulary (`classify_tbl4`, `byte_class_from_table_64`,
   `dispatch::lo6_table_admissible`). If **9B wins the home axis** (parse-that keeps its own kernels, which
   lack `vqtbl4q` and `lo6_table_admissible`), 9C's proven-feasible emit must be **re-proven against
   parse-that's native kernels**. The DAG must name this: **9C-feasibility ⊨ 9A-home**, or 9C owes a second
   proof against 9B's vocabulary. A silent rust↔rust isomorphism coupling (constitution iii).
6. **[cross-lock, triple-coupled] G9C-6.** LOCK-5 vendoring (verified: skinny has no `automata/`, neither
   `parse-that-regex` nor the 322-LOC `bbnf-regex` stub — no in-tree DFA source) + LOCK-2 tier island (the
   **shared unbuilt island**, §CROWDING X-5) + GN-codex codegen home. The `bbnf-regex 0.1.0` name+version
   collision is resolved-in-mechanism (rename-on-vendor → `parse-that-regex`; 322-LOC stub deleted at GN-0)
   but the vendoring is un-executed. BLOCKED-candidate; not equal-difficulty.

**Pass-4 directives.**
- *State explicitly:* PROTO-2/2b retire the elegant-reduction trap **for MEMBERSHIP only**; disposition
  stays BLOCKED on the multi-kind structural gap, the unbuilt separability solver + codegen pass, the
  un-executed LOCK-5 vendoring, and the LOCK-2 island. Do not read "membership emit GO" as a whole-family
  bill.
- *Prototype (the SHARPEST unrun uncertainty — the multi-kind structural emit):* compile a leaf whose DFA
  yields **>2 distinct member classes** needing per-kind output (a full JSON/CSS structural classify), emit
  through the `vceqq`-fan, assert **bit-identical per-kind classification** vs the hand
  `structural_classify_64`. This tests the half that can actually falsify 9C's dissolution claim — skipped
  this pass for the tractable membership one.
- *Prototype (only if LOCK-5 substrate is ruled buildable):* run the serial-vqtbl-vs-scalar head-to-head
  (Tier-S is inferred, not measured) and widen the census to a wide-unicode leaf sourced from
  `bootstrap/regex.bbnf` with Tier-X denominators (coverage as a measured %).
- *Research must answer:* reconcile the scanner-file trichotomy (3-way researcher disagreement); does the
  DAG carry **9C-feasibility ⊨ 9A-home**, or does 9C owe a second proof under a 9B win? Absent a LOCK-5 GO
  the vendoring stays un-executed and 9C cannot converge.
- *Withheld from researchers:* do **not** seed "membership emit GO = 9C converges" or "the multi-kind
  structural classify is proven" (it is UNPROVEN — binary-membership evidence only) or "Tier-S is measured"
  (inferred). Hand them the banked membership feasibility, the multi-kind gap as the sharpest open item, the
  3-way census disagreement, the 9C⊨9A-home coupling, and the LOCK-5 vendoring as a hard DAG predecessor.

---

## 9D — LAZY-SURFACE · RETIRED · 85%

**Floor rationale.** CRIT-1 "RETIRE, disposition soundness ~85%" / CRIT-2 "90% converged-as-RETIRED." Both
concur RETIRE; take the disposition-convergence **floor 85** (CRIT-1's). The pass-2 registry set the
terminus explicitly — "pass-3 runs P3 or converts 9D to RETIRED" — and P3 (the `furthest_offset − offset`
rewind histogram, the "cheap, deterministic, born-RED decider") was **skipped for a THIRD consecutive
pass** (research-D delivered the instrumentation *spec* a third time). Per directive, **9D converts to
RETIRED.** The RETIRE is sound *only because* the memory center is over-determined-refuted on **independent
grounds** — so the unrun histogram no longer gates anything (both critics agree). 9D exits on an unmeasured
backtrack prediction; note it **carried, never "measured."**

**Why the memory center is refuted (independent of the unrun histogram).** (1) **Consumer absent (Y.13,
grep-confirmed):** `feed|push_bytes|Decoder|BufRead|document_stream|from_reader` → zero real consumers;
every grammar builds a whole owned tree; `ParserState::new` materializes a full padded copy. (2)
**Span-dangle vs zero-copy wall:** `Span<'a>` + `Cow::Borrowed` borrow the whole `src` for the tree's
lifetime; a sliding window frees bytes the tree still points into → forces `Cow::Owned` copies →
reintroduces the 5390-alloc mass streaming was meant to avoid. A hard wall, not a knob. (3) **PaddedView is
anti-chunking:** the 64-NUL trailing guard breaks at every chunk boundary; simdjson's `document_stream` is
NDJSON (whole docs), never single-doc chunking. (4) **Dead-by-law:** §0/51 fold-over-lazy-cursor is
2.3–4.9× behind. (5) Peak RSS is the **tree**, not the input buffer — whole-document streaming (the only
available granularity) touches only the buffer, so there is no RSS lever even if M2 succeeded.

**The surviving residue (two, both re-homed — the RETIRE is not a deletion).**
- **(a) Cost-model kernel-selection at combinator-compile time** — 9D's only consumer-bearing surface
  (`leaf.rs:177` `take_until_any_span`'s `One/Two/Three/Lut` ladder, live at every `take_until`/scanner
  site; Y.13 satisfied). CRIT-1 correctly notes this is arguably a standalone-advanceable item, not merely
  a hand-off — but the disposition re-homes it to **LOCK-2's decision driver** (`KernelPlan`, the same
  island 9C's tier-selection needs, §CROWDING X-5), not a LOCK-9 subsystem.
- **(b) Carry-threading for the 9C⊗9D gestalt-Ω** "compiled resumable scanner" — since Sheng is a measured
  negative, 9D threads carries through the **Tier-3/structural kernels** (`prefix_xor`/comment/bracket-depth
  carries bbnf-simd already exports), not a saved Sheng state. This is where 9D's residue survives.

**Pass-4 directives.** None as a live family. Fold the two residues into the gestalt: (a) → LOCK-2's cost
model as one consumer of the shared scan-plan island; (b) → gestalt-Ω. If either residue is later found to
require a LOCK-9-internal artifact, re-open a *residue* item, not the family. Do **not** re-brief P3 — the
RETIRE rests on the independent refutation, not the histogram; running it would only upgrade a refutation
(a low gestalt-advance) and 9D is closed.

---

## CROWDING & REDIRECT (this pass)

**The central axis (9A⊗9B) stays convergent-in-mechanism AND a performance wash — hold both, bank
neither — BUT the pass-3 datum breaks the pass-2 symmetry toward 9A.** Pass-2 held that the two biases
point *opposite* ways (charge 9A the 9A-only de-face → tilts 9B; re-measure gate-5 against the evolved home
→ tilts 9A). Pass-3 **delivered the evolved-home gate-5** (PROTO-1's divergence table), and read as data it
points **one way**: 9B must author from scratch what 9A inherits — the checkasm scalar-parity oracle, the
`vqtbl4q` lo6 table, the stable `core::arch` dialect (9B is still on nightly, headline unverified), and an
x86-clean surface — while the genuinely domain-fit residual divergence (`PaddedView`-64 vs `eob_pad_clamp`;
per-16 vs 64-byte) is *small*. **The crowding-redirect rule (bank the redundant one) still does NOT
apply:** the home decider is a LOCK-6 verdict LOCK-9 cannot render, and banking either now would pick the
axis. But the agglomerator **retires the "9B incumbent-default" thumb** the FAMILY-SPECS carries — it is
not supported by the datum 9B's own axis probe produced. **The registry conveys to LOCK-6/gestalt: the
delivered maintenance-cost datum tilts to 9A; neither family is banked; the axis stays open pending LOCK-6.**

**The pass-2 keystone directive ("evolve the shared home, then re-measure the axis") is RESOLVED by
PROTO-1 — retire it.** The evolved home is constructed in scratch (canonical producers authored
bit-identically, four double-`vaddv` purged, build green) and the divergence table is emitted. What
remains on the axis is the CSS perf FLOOR (a wash-by-law the spec itself excuses — resolve the
gate-(2) wash-vs-testable tension) and LOCK-6's verdict.

**The underexplored formulation to OPEN this pass (replaces the resolved evolved-home directive): "prove
the 9C multi-kind STRUCTURAL emit, and name the 9C⊨9A-home coupling."** The lock's cleanest concrete work
(PROTO-2/2b) proved the membership emit and skipped the harder half — every probe leaf was
`num_classes=2` (binary membership), so the `vceqq`-fan multi-kind structural classify (the actual JSON/CSS
structural *index*, per-kind tokens for the tape) is **emit-UNPROVEN**. This is the one measurement that
can falsify 9C's dissolution claim, and it silently rides on a 9A home-win (the emit uses bbnf-simd's
`vqtbl4q`/`lo6_admissible` vocabulary parse-that lacks). Pass-4 opens it as the 9C keystone brief.

**9D's residue is not a redirect** — the cost-model `KernelPlan` selection is a consumer of LOCK-2's
decision-driver island (the same island 9C's tier-selection and the prefilter dispatch need — one island,
three consumers, §X-5). No LOCK-9-internal redirect.

## CROSS-POLLINATION (named grafts — both families developed enough; graft justified)

1. **9C ⊗ 9D — the compiled resumable scanner (PRIMARY, gestalt-Ω consolidation candidate; carried on the
   structural-carry basis).** research-D (Ragel/re2c): a streaming scanner IS a resumable DFA. Sheng is a
   measured negative, so 9C is a *stateless* Tier-M membership classify + Tier-3 structural — 9D threads
   carries through the **Tier-3/structural kernels** (`prefix_xor`/comment/bracket-depth carries bbnf-simd
   already exports), **not** a saved Sheng state. Both families are developed enough for the named graft:
   9C's emit is measured-feasible (membership); 9D's carry-threading residue points at a *named*, exported
   kernel vocabulary. **9D having RETIRED, this graft is where its carry-threading residue survives** — a
   gestalt-Ω input, not a live LOCK-9 family.
2. **9A/9B kernel home ⊗ 9C emit vocabulary (SECONDARY, but now a NAMED coupling, not merely composable).**
   9C's emit routes the DFA's `[u8;256]` into `bbnf_simd::byte_class_from_table_64` and its separability
   through `dispatch::lo6_table_admissible` — so **9C ◐ 9A is STRENGTHENED (9C consumes the 9A-shared
   home), but 9C-feasibility ⊨ 9A-home** (CRIT-2 X-3): under a 9B home-win, 9C's proof does not transport
   to parse-that's `vqtbl4q`-less kernels and must be re-proven. The gestalt DAG must carry this as a named
   isomorphism dependency, not a home-agnostic prose edge.

## CROSS-LOCK DAG ORDERING (for GESTALT — blockers LOCK-9 CANNOT retire alone)

- **9A⊗9B → LOCK-6 (a maintenance-cost call, perf washes; the datum now tilts to 9A).** LOCK-9 produced the
  corrected measurement (PROTO-1's evolved-home divergence table). LOCK-6 renders the verdict — "null
  hypothesis defeated by measured drift/maintenance cost" (LOCK-6D verbatim; XL conveys it). The remaining
  LOCK-9 residual is the CSS perf FLOOR (or the deletion of a wash-by-law gate) + the P23 asymmetry
  resolution. Co-converge in GESTALT-P; neither closes the axis alone.
- **Canonical movemask form → RESOLVED (measured); home-collapse → LOCK-9 ∩ LOCK-5.** The form is settled
  (contract-split, measured). Four producers (bbnf-simd repack + bbnf-simd's four refuted double-`vaddv` +
  PROVE-10 lean + parse-that inline `to_bitmask`) must collapse to ONE lawful home; memchr's lean form lives
  in bbnf-regex's prefilter. The gestalt collapses the home — the collapse itself is un-done.
- **9C grammar→DFA source-of-truth → LOCK-5 (leg DOWNGRADED, not cleared).** The `bbnf-regex 0.1.0`
  collision (real engine `parse-that/rust/regex` × skinny 322-LOC stub) has a concrete resolution mechanism
  (rename-on-vendor → `parse-that-regex`; stub archives at GN-0), but VERIFIED: the `automata/` DFA tier is
  **not yet vendored into skinny** — no in-tree DFA source exists. **9C tier-selection → LOCK-2;** **9C
  codegen home → a named GN-codex supersession** (constitution ix); **9C-feasibility ⊨ 9A-home** (§X-3). 9C
  is quad-homed and cannot be *selected* until LOCK-5 vendoring + LOCK-2 island + GN-codex land.
- **ONE unbuilt LOCK-2 scan-plan island bears THREE LOCK-9 consumers → single point of cross-lock failure
  (X-5).** 9C tier-selection **and** 9D's surviving cost-model `KernelPlan` residue **and** the prefilter
  one-lowering dispatch all consume the *same* unbuilt LOCK-2 decision-driver island. The FAMILY-SPECS
  routes each to LOCK-2 separately but never names them as ONE island with three consumers — if LOCK-2 does
  not deliver it, all three stall on one missing substrate. Y.13-adjacent until the gestalt commits it.
- **Prefilter double-home → GN-2/PT-1 dispatch law.** `bbnf-simd::find_ascii_set_member64` ⟷
  `bbnf-regex::Dfa::find` — one lowering per scan site, never both. 9C deepens this.
- **9C/9D resumable-scanner collapse → gestalt-Ω** (cross-pollination 1, on the structural-carry basis).
- **Un-homed scanner→combinator/TS edge → LOCK-4/LOCK-8.** No 9-family names how the reborn scanner surface
  presents to LOCK-4's parser graph, nor whether LOCK-8's build-time TS emitter (XL-D) mirrors 9C's
  compiled-DFA products. LOCK-8's symmetric confirmation is XL-G(P2)-pending. A live open edge the gestalt-P
  weave must close.

## RECONCILIATION OWED (registry → FAMILY-SPECS, before gestalt — the SAME defect as pass 2)

The FAMILY-SPECS (22:44) is **STALE against its own protos for the SECOND consecutive pass** (PROTO-1
23:23, PROTO-2b 23:29). Before gestalt the synthesis must absorb:
(i) **9A Q1 / gate (4) and 9B gate 5** — "the evolved-home residual divergence, the LOCK-6 datum neither
pass produced" is **FALSE**: PROTO-1 produced it (23:23), and it reads *for* 9A / *against* the 9B
incumbent-default.
(ii) **pass-4 BRIEF 1 re-commissions completed work** — its steps (author-into-scratch, purge, divergence
table) are **DONE**; the only un-run piece is the CSS paired ratio + the seam rewire. Re-cut BRIEF 1 to
just that (a fraction of what is written).
(iii) **9C "byte_classes-direct is the mechanical next step"** — PROTO-2b **RAN it** (`direct == oracle` on
all three leaves). The gap predates its own closure; the residual is the multi-kind structural emit, not
the direct-derivation.
(iv) **9A gate (1)** references a **non-existent digit-MAC residual** (verified: no `vaddv` in bbnf-simd
outside the four mask producers) — fix to "returns nothing."
(v) **research-B's mechanism is anchored to a phantom `simd128.rs`** (verified absent) — correct it to the
real 7-file inline `to_bitmask` surface; the stable-cutover blast-radius is unsubstantiated.
(vi) **the P23 asymmetry** — 9A's one untracked line carries the supersession flag while 9B's far larger
in-place mutation carries none; resolve symmetrically. `.cargo/config.toml` is untracked (not gitignored,
not tree-of-record) — a lighter P23 obligation than a source mutation.
(vii) **Tier-S "MEASURED NEGATIVE"** downgrades to SPEC-ONLY/carried (inferred from run-length + directive,
not a pass-3 head-to-head); **the scanner-file census** is unreconciled across three researchers.

---

## PASS HISTORY

### Pass 3 (2026-07-16) — agglomerated

- **Artifacts:** FAMILY-SPECS pass-3 (4 specs + the six reconciliations + M/S/X tiering + incompatibility
  map — STALE against its own protos, authored 22:44 before PROTO-1 23:23 / PROTO-2b 23:29);
  research-{A,B,C,D,priorart} pass-3; PROTO-1 (9A evolved-home construction, RAN_MEASURED); PROTO-2 +
  PROTO-2b (9C emit feasibility against the real DFA, RAN_MEASURED); CRIT-1 (50/58/46/RETIRE); CRIT-2
  (53/60/46/90-as-RETIRED).
- **PROTO-1 disposition:** the strongest 9A artifact — the evolved home *constructed* in scratch. **BANKED:**
  canonical DENSE-64 `vpaddq` authored (~15 LOC), 0/200k mismatches vs scalar AND vs the refuted form, 7
  `uzp/orr` / 0 horizontal reduction; all four double-`vaddv` purged, build green, gate clean; the
  LOC/substrate divergence table emitted — the LOCK-6 datum, which reads **for** 9A. **NOT run:** the CSS
  paired ratio (loadavg 5–9, deferred a THIRD pass); the in-tree purge (scratch-only); the path-dep seam
  (prior probe's CONFIRMED result, not re-run).
- **PROTO-2/2b disposition:** the lock's cleanest concrete work — the `[u8;256]→SIMD` emit pass is
  measured-feasible against the **real** bbnf-regex DFA (three leaves bit-identical over 8129 blocks,
  `byte_classes`-direct == `find_at` oracle, gate-4 lo6-collision routed off `vqtbl4q` with no silent drop,
  INJECT_BUG 100%, `\p{L}`=41cls/21st correcting the `→None` prediction). **CONTRADICT:** every leaf is
  `num_classes=2` (binary membership) — the multi-kind `vceqq`-fan STRUCTURAL emit is UNPROVEN (the harder
  half, skipped for the tractable one).
- **The lock-wide findings (agglomerator):** (1) the delivered evolved-home divergence datum **tilts to
  9A** — 9B must author the oracle + `vqtbl4q` table + stable dialect + x86 purge that 9A inherits; the "9B
  incumbent-default" thumb is retired. (2) The FAMILY-SPECS froze before its two most consequential probes
  finished (same pipeline-order defect as pass 2) — it lists prototype-closed items as open and
  re-commissions completed work as pass-4 BRIEF 1.
- **Substrate corrections (agglomerator-confirmed this session):** parse-that `simd128.rs` is a **PHANTOM**
  (research-B's "single point of correction" anchored to a non-existent file; the real de-nightly rewrites
  inline `to_bitmask` across 7 scan files); `number_simd.rs:54-55` double-`vaddv` **still live** (3rd-pass
  in-place refutation violation); `.cargo/config.toml` patches bbnf-regex (not bbnf-simd) and is
  **untracked** (neither gitignored nor tree-of-record); bbnf-simd's four double-`vaddv` at the named lines,
  `vpaddq` in zero files, **no digit-MAC `vaddv` anywhere** (9A gate (1) mis-specified); skinny has **no
  `automata/`** (no in-tree DFA source, 9C substrate LOCK-5-owned).
- **Dispositions set:** 9A ADVANCED born-RED **50** (down; evolved home delivered but already priced, CSS
  floor 3rd-pass-unmeasured, gate (1) mis-spec); 9B ADVANCED incumbent **58** (down; phantom mechanism +
  in-place violation + unverified headline; banked disasm anchors it); 9C BLOCKED-candidate **46** (up;
  membership emit measured-feasible against the real DFA, tempered by the untested multi-kind half); 9D
  **RETIRED 85** (P3 skipped a 3rd time; memory center over-determined-refuted; residues re-homed). No
  family banked (the axis is a LOCK-6 verdict; neither home is redundant to declare).
- **Crowding/redirect:** the pass-2 evolved-home directive is RESOLVED by PROTO-1 (retire it); the datum
  breaks the symmetry toward 9A. OPEN instead the **9C multi-kind STRUCTURAL emit** (the untested half that
  can falsify 9C) + name the **9C-feasibility ⊨ 9A-home** coupling.
- **Cross-pollination:** 9C⊗9D compiled-resumable-scanner carried on the structural-carry basis (Sheng
  negative) — where 9D's retired residue survives; 9C ◐ 9A strengthened but named as a home-win-coupled
  isomorphism dependency (X-3).
- **Pass-4 keystone directives:** (1) **re-cut the STALE FAMILY-SPECS** against PROTO-1/2b before any
  convergence claim (the divergence table is delivered; BRIEF 1 is mostly done); (2) **resolve 9A gate (2)
  wash-vs-testable** — run the CSS floor on a quiet machine or delete the gate; (3) **fix 9A gate (1)** to
  "returns nothing" + land the in-tree purge; (4) **run 9B's stable cutover** (the unverified headline) +
  build the DENSE bit-parity oracle + correct research-B's phantom mechanism; (5) **prove the 9C multi-kind
  structural emit** (the untested half); (6) **resolve the P23 asymmetry** + reconcile the scanner-file
  census + downgrade Tier-S to carried. The BLOCKED families finally got their commissioned measurement (9C
  membership); the un-run halves (9C multi-kind, 9A CSS floor) are pass-4's owed discriminators.

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
