# LOCK-9 pass-2 · CRIT-2 — the cross-lock adversary (scanner & kernel plane)

*Adversarial critic 2 of 2, pass 2. Critic 1 owns the intra-family teardown; my mandated weight is
the CROSS-LOCK view — where LOCK-9 contradicts or double-homes against the GN/PT codex and the other
locks. Every substrate claim below was re-verified against the live trees THIS session
(parse-that@`/Users/mkbabb/Programming/parse-that`, bbnf-simd@`bbnf-lang/skinny/crates/bbnf-simd`);
citations flagged VERIFIED are mine, not inherited. The failure-mode checklist is applied by name.*

---

## 0. Pipeline-order finding (governs everything below)

`FAMILY-SPECS.md` (mtime 09:52) was written **before** both pass-2 prototypes landed (`PROTO-1.md`
10:10, `PROTO-2.md` 10:09) and states so: it "folds research-{A..D,priorart} pass-2 against the
**pass-1** registry (PROTO-1 9A/9B kernel fragment; PROTO-2 9C Sheng census)." **The synthesis
therefore has not seen the pass-2 evidence.** I am the first document to fold the pass-2 protos
against the pass-2 synthesis. Two of the synthesis's load-bearing assertions are contradicted by the
very probes commissioned to test them (§2). The registry-writer must reconcile FAMILY-SPECS to
PROTO-1/2, not the reverse.

## 1. Verification ledger (re-checked, not taken on faith)

- **VERIFIED** `number_simd.rs:54-55` double-`vaddv` **still live in the owner tree** (`vaddv_u8(lo_masked)`,
  `vaddv_u8(hi_masked)`). PROTO-2 rebirthed it *in a scratch copy only* (P23-correct); the refutation-law
  violation remains in-place in parse-that. The law is stated and violated-in-place across two passes.
- **VERIFIED** parse-that `.cargo/config.toml [patch.crates-io]` = {parse_that, **bbnf-regex→`regex`**, pprint,
  pprint_derive, bbnf, bbnf-ir, bbnf-ser, csp-solver, egraph, egraph-derive}; **`bbnf-simd` absent.** 9A's
  "one added line" is real; the P23 mutation is real.
- **VERIFIED — corrects the map (X-7):** `bbnf-lang/crates/bbnf-regex` **does not exist.** The path
  FAMILY-SPECS 9C-Q2 and the pass-1 registry G9C-4 both cite ("bbnf-lang's `crates/bbnf-regex` (LOCK-5's
  home)") is wrong. The real second crate is `bbnf-lang/**skinny/crates**/bbnf-regex`. **Both manifests
  are `name = "bbnf-regex"`, `version = "0.1.0"`.** bbnf-lang's `ir`/`bbnf-path` crates depend on
  `bbnf-regex = { version = "0.1" }` (a registry edge, no path); parse-that patches `bbnf-regex → regex/`.
  So one crate *name+version* resolves to **two different sources per build graph** — a name collision, not
  merely "two crates to reconcile."
- **VERIFIED — kills a "do-not-re-litigate" claim (X-8):** parse-that's own scan files contain **zero**
  `OnceLock`/`get_or_init`/`primitive_kernels`/`dispatch::` — only a dead `is_x86_feature_detected!("sse4.2")`
  at `number_simd.rs:222`. The OnceLock fn-pointer facade is **bbnf-simd-internal** (`dispatch.rs:58-60`).
  FAMILY-SPECS 9A ("9B inherits the identical constraint intra-crate") and pass-1 registry EARNED-item-2
  ("9B would inherit the identical constraint") are **false**: 9B keeps inline `portable_simd` kernels; there
  is no OnceLock to de-face on the own-source path.
- **VERIFIED** bbnf-simd internal inconsistency (R-A3, real): `movemask.rs` is the 28-insn `vshrn`+repack
  (form #5); `byte_class_from_eq_set_64.rs:79-87` hand-rolls a **second, refuted** `movemask_u8x16` doing
  `vaddv_u8(low)`/`vaddv_u8(high)` — the double-`vaddv` PROVE-09 killed, **living in the shared home.**
- **VERIFIED** `grep vpaddq bbnf-simd/src` = **ZERO files** — confirms PROTO-2 and refutes 9A's "free movemask."

Substrate is honest where cited *and re-verified*; but two cited paths/claims are wrong (X-7, X-8) and the
synthesis is stale against its own protos (§2).

---

## 2. Where the pass-2 protos CONFIRM / EXTEND / CONTRADICT the pass-2 synthesis

**RESOLVED (synthesis's owed datum, now delivered — credit).** FAMILY-SPECS's headline shared-spec left one
item "unmeasured … the owed instruction-count datum": does the `vpaddq` tree beat 4×(`to_bitmask`+shift+OR).
**PROTO-2 measured it: 50 vs 64 insns (−22%, zero reduction ops), and the LEAN swap 14 vs 19 (−26%, zero
`vaddv`).** The static disasm is host-noise-immune; this is the pass's most solid result. The owed canonical-
producer datum (LOCK-9 ∩ LOCK-5 input) is now real. Gate (3)'s "instruction-count arm" — added in response
to pass-1 CRIT — actually ran. Credit both.

**CONTRADICTED-1 — 9A's "free movemask / near-superset" is refuted by PROTO-2.** FAMILY-SPECS 9A leads with
"bbnf-simd already carries a near-superset … and hands parse-that the `vshrn` movemask … **for free**."
PROTO-2 + my grep: the two *lawful* producers (LEAN nibble-`tz`, DENSE `vpaddq` tree) **exist in bbnf-simd
nowhere** (`vpaddq` in zero files), while a **refuted** producer *does* live there. Adopting 9A is therefore
**not** "inherit good kernels for free" — it is "author the canonical producers *into* bbnf-simd **and** purge
the refuted double-`vaddv` there first." The synthesis's central merit sentence overstates 9A.

**CONTRADICTED-2 — 9A's de-facing precondition is not load-bearing at the shipping config.** FAMILY-SPECS: "9A
**must** de-face the five to direct-cfg **before any consumer measures**," and pass-1 registry EARNED-item-1
("the OnceLock facade is a hard devirtualization barrier fat-LTO cannot pierce → ~0.91–0.93×") is stamped
*do-not-re-litigate*. **PROTO-1 falsifies it on a live scanner at `codegen-units=1`:** the de-faced arm AND the
OnceLock CONTROL both emit **0 `blr`**, byte-identical, and the standalone `bbnf_simd` object has **no
`OnceLock`/`get_or_init` symbol at all** — LLVM devirtualized the facade entirely. The pass-1 0.92× barrier was
a probe-crate/multi-CGU artifact; it "does not reproduce on this real scanner under cgu=1." So a pass-1
*do-not-re-litigate* fact is overturned by pass-2's own probe. Net for 9A: inline is now **measured-GREEN live**
(good), but the barrier the de-face exists to remove **is free at cgu=1** — the de-face buys inline-*parity*,
not a win.

**PROTO-1's perf clause: UNMEASURED for the second consecutive pass** — and PROTO-1 admits `classify_stripe`
is **off the JSON hot path** (only the CSS ws/comment scanner reaches it), so the commissioned JSON-corpus
ratio would be a null. 9A gate (2) (paired ≥0.98× on a live consumer) has **never executed across two passes.**

---

## 3. The repeated prototype-allocation miss (the pass's structural failure)

The pass-1 registry promoted two **cheap, deterministic, born-RED must-run** deciders: 9C's widened
grammar-sourced census (its own explosion killer) and **9D-P3** (the backtrack-bound histogram, "one prototype
away from a valid close"). **Pass-2 spent both probes on the central axis again (9A PROTO-1, 9B PROTO-2).** So
across two passes, **four probes have all landed on 9A/9B; the two BLOCKED families have never been probed.**
9D-P3 — self-described "cheap and deterministic," the plainest allocation miss in the lock — is now **unrun for
the second pass running**. 9C and 9D consequently carry **zero new measured evidence** and cannot rise off
their pass-1 floors; the FAMILY-SPECS "sharpening" of both is pure argument (§5, §6). This is not a map defect —
it is the pass declining, twice, to run the two measurements it declared mandatory.

---

## 4. Per-family convergence + enumerated open gaps

### 9A — ONE-KERNEL-LIBRARY · **55%** · ADVANCED, born-RED (I concur RED; flat, composition changed)
Pass-2 added one measured-GREEN (live cross-crate inline) but **falsified both headline merits** (free
movemask; OnceLock barrier). The family is now honestly "physical-iso-only, and must first evolve+clean the
shared crate." Net washes to flat.
- **G9A-1 [raison d'être STILL unmeasured, 2nd pass].** The movemask reroute *as a wired 9A consumer* was
  again not measured. PROTO-2 measured the producers for **9B's own-source rebirth**, not for a bbnf-simd
  consumer — and found the producers absent from bbnf-simd (CONTRADICTED-1). 9A's headline win is not merely
  unmeasured, it is **contradicted**: nothing to inherit for free.
- **G9A-2 [the de-face lever is not load-bearing at cgu=1].** PROTO-1/CONTRADICTED-2. The precondition survives
  only as correctness hygiene, not as a perf differentiator in the shipping config.
- **G9A-3 [refuted idiom in the shared home].** `byte_class_from_eq_set_64.rs:79-87` double-`vaddv` (VERIFIED)
  must be purged as a *precondition* of 9A, or 9A imports the refutation-law violation. FAMILY-SPECS names it
  (R-A3) but the purge has no artifact.
- **G9A-4 [gate 2 never ran, 2nd pass]** — paired ≥0.98× on a live consumer; the correct bench is CSS
  (`parse_that_css`), not the commissioned JSON corpus. Unrun.
- **G9A-5 [SPEC-ONLY] the `vector/{mod,neon,scalar}.rs` module does not exist** (VERIFIED absent) — the bulk of
  9A's V-tier surface, "the refuted 276-LOC simd128 promoted minus double-vaddv." Promoting refuted code demands
  its own no-regression artifact; none exists.
- **G9A-6 [cross-lock] P23 supersession still named-not-granted** — 9A mutates parse-that's read-only config.
- **G9A-7 [cross-lock, NEW] the gate-5 baseline is wrong-way biased against 9A** (see X-8): "near-total substrate
  divergence" was measured against **un-evolved** bbnf-simd, not against a shared home that has adopted the
  canonical producers. That baseline manufactures divergence 9A would erase.

### 9B — NATIVE-REBIRTH · **62%** · ADVANCED, incumbent (raised from 55: banked real artifacts)
The only family that moved. PROTO-2 banked the cheapest win (LEAN `shrn.8b`, zero `vaddv`, 14≤19) and the DENSE
`vpaddq` win (50 vs 64), both paired ≥0.98× (15/16). Off SPEC-ONLY, refutation-law violation demonstrably fixable.
- **G9B-1 [gate 5 SEEDED against the wrong baseline].** The "~2 LOC / 0 LOC overlap → near-total divergence"
  datum compares the rebirth to **today's** bbnf-simd, which lacks the canonical producers. The *decisive*
  gate-5 question is what STILL diverges after a shared home adopts them (the genuinely domain-fit residue:
  `PaddedView`-64 vs `eob_pad_clamp`; `vqtbl1q` vs `vqtbl4q` table arity). Measured against the un-evolved crate,
  the number **overstates 9B's case**; the merged-surface table still does not exist.
- **G9B-2 [host-saturated, off-ship profile].** The paired divan ran at loadavg 9–16 on a thin-LTO/16-CGU
  build-speed override, not fat-LTO cgu=1. The ratio-invariance argument is reasonable but the regime is not ship;
  1/16 ratios failed (0.973, a 13 ns bench). The disasm (14/19, 50/64) is the robust datum; the wall-clock is
  protocol-OK but noisy.
- **G9B-3 [gate 3 still a plan].** The native checkasm-liveness harness (`black_box`/`read_volatile` inputs,
  `INJECT_BUG` ≥90%, `vmull_p64`/aes dual-arm) is unported; parity ≠ liveness discipline described, not run.
- **G9B-4 [Q2 self-regression un-exercised]** — naive `vshrn` on `classify_stripe_64` yields 256 bits, corrupts
  parity → must use idiom (b). PROTO-2 rebirthed the DENSE site with the tree (correct), but the *negative*
  (naive-nibble-corrupts) was not demonstrated as the load-bearing guard.
- **G9B-5 [co-traveler]** `smallbox[nightly]`→stable blast radius asserted bounded, not shown.
- **G9B-6 [cross-lock] the axis close is still LOCK-6's** (X-1). 9B produced its half; it cannot self-declare.

### 9C — SCANNER-AS-DFA · **43%** · BLOCKED (docked 1: 2nd-pass probe miss + wrong-path citation)
No pass-2 probe. The FAMILY-SPECS "sharpening" (Tier M/S/X; Sheng retired to a measured negative) is **argument,
not measurement** — and it *de-escalates* (the pass-1 "genuine novum," vectorized Sheng, is now a negative,
shrinking the family's remaining value to Tier-M parity + deletion).
- **G9C-1 [family-killer, confirmed-modest]** dense classify is at-best parity with hand simdjson — value is
  deletion + grammar-specialization + stable-by-construction. Honest, but not a perf win on the dominant pass.
- **G9C-2 [born-RED census unrun, 2nd pass]** the widened grammar-sourced census (its own explosion candidate,
  `\p{L}+`/`url()`) was directed mandatory and again not run. Tier X is asserted from `dfa.rs:262` returns `None`,
  not censused; the coverage-% (gate 3) is still unmeasured.
- **G9C-3 [gate-that-cannot-fail, relabeled]** pass-1 flagged "feasibility census excludes its infeasibility
  candidate"; pass-2 responds by making the exclusion **definitional** (Tier X = "not a scanner, excluded"). This
  is honest ONLY if the census still reports Tier-X rows as denominators — gate (3) says it should, but it did
  not run, so the dodge is unrefuted in fact.
- **G9C-4 [elegant-reduction trap]** the core new code — `[u8;256] → SIMD-classifier` emit + the separability
  solver (nibble-sep ∨ lo6-table ∨ `vceqq`-fan) — **does not exist**; `equiv.rs` emits scalar only. "Compile
  through `equiv.rs` (exists) and then emit through the separability solver (unbuilt, correctness hinges on it)"
  is the read-and-then-the-hard-part pattern verbatim.
- **G9C-5 [cross-lock, map-corrected] two-crate reconciliation is a NAME COLLISION and the cited path is wrong**
  (X-7). The blocker is real and sharper: `parse-that/rust/regex` and `bbnf-lang/skinny/crates/bbnf-regex` are
  **both `bbnf-regex 0.1.0`**, resolving per build graph. Substrate owned by LOCK-5, unbuilt at 9C's fidelity —
  missing-primitive test passes; BLOCKED stands.
- **G9C-6 [cross-lock] quad-homed + GN-codex supersession un-named** (LOCK-5 grammar/DFA · LOCK-2 tier island ·
  LOCK-9 backend · `crates/codegen/src/emit/regex/` GN-codegen). Constitution ix requires a *named* supersession.

### 9D — LAZY-SURFACE · **30%** · BLOCKED (flat; nothing moved)
No pass-2 probe. Every gate P1–P4 remains SPEC-ONLY; the family is SPEC-ONLY after two passes (constitution vii
discipline gap).
- **G9D-1 [the cheap decider skipped, 2nd pass]** P3 unrun again. The memory raison d'être rests on an unmeasured
  property for two passes running.
- **G9D-2 [missing primitive, refuted-by-default]** unbounded PEG rewind ⇒ ring buffer retains all input ⇒ zero
  peak-RSS win. "A bounded backtrack guarantee PEG semantics may simply not provide — equal in difficulty to
  redesigning the backtracking discipline." The BLOCKED floor.
- **G9D-3 [Y.13 tripped in-design]** consumer measured-absent (exhaustive grep: zero `feed`/`document_stream`/
  `from_reader`); the output tree (5390 allocs) dominates RSS regardless. A streaming *input* surface with no
  in-wave consumer is forbidden substrate.
- **G9D-4 [cross-lock] `KernelPlan` double-consumes an unbuilt LOCK-2** decision-driver island (same island 9C
  needs) — substrate-less consumer on the input-surface axis.

---

## 5. Cross-lock adjudication (my mandated weight) — updated

- **X-1 [carried] 9A⊗9B is LOCK-6's verdict, not LOCK-9's.** "Reasonable point of rust↔rust iso" (Constitution
  iii) resolved by "null hypothesis defeated by measured drift cost" (LOCK-6D verbatim). Pass-2 produced 9B's
  seed and 9A's inline; **neither closes the axis alone.** Any LOCK-9 "convergence" claiming to settle it
  internally contradicts the portfolio's homing of LOCK-6.
- **X-2 [carried, now MEASURED] the movemask triple-home.** bbnf-simd repack (28-insn, form #5) / PROVE-10 lean /
  parse-that `to_bitmask`. PROTO-2 measured the lawful producers (LEAN 14, DENSE 50) and proved **neither lives
  in bbnf-simd**; the shared crate additionally carries a **fourth, refuted** producer (form #6). The collapse to
  ONE lawful producer is a LOCK-9 ∩ LOCK-5 gestalt act (memchr's lean form lives in bbnf-regex's prefilter);
  LOCK-9 has now run the measurement, the gestalt owes the home-collapse.
- **X-3 [carried, sharper] prefilter double-home** `bbnf-simd::find_ascii_set_member64` ⟷ `bbnf-regex::Dfa::find`
  — and `bbnf-regex` is itself now shown two-homed (X-7). GN-2/PT-1 dispatch law: one lowering per scan site.
- **X-7 [NEW, map-correcting] the two-crate blocker cites a non-existent path and understates the hazard.**
  `bbnf-lang/crates/bbnf-regex` is absent; the real crate is `skinny/crates/bbnf-regex`, and **both regex crates
  share the identical name+version `bbnf-regex 0.1.0`** while resolving to different sources per build graph. This
  is a direct tension with Constitution ii (true isomorphism — "one system") and is LOCK-5's to home. The registry
  must fix the citation and re-scope 9C's blocker from "reconcile two crates" to "resolve a name+version collision."
- **X-8 [NEW] two synthesis claims that "9B inherits the OnceLock constraint" and that gate-5's baseline is sound
  are both wrong-way biased on the axis.** (a) 9B has no OnceLock (VERIFIED) — the de-face lever is a *9A-only*
  cost, which should count *against* 9A on the axis, not be neutralized as "both inherit it." (b) PROTO-2's gate-5
  divergence was measured against un-evolved bbnf-simd, manufacturing divergence 9A would erase — biasing the
  axis *toward* 9B. The two biases point opposite ways; the honest axis input requires (a) charging 9A the de-face
  cost and (b) re-measuring gate-5 after the shared home adopts the canonical producers. Both are LOCK-6 inputs.
- **X-5 [carried] 9C/9D resumable-scanner collapse → gestalt-Ω.** Sheng-as-negative (pass-2) re-bases the collapse
  onto the structural/Tier-3 carries (not saved Sheng state); neither family is independently converge-able.
- **X-6 [carried] un-homed consumer edge → LOCK-4/LOCK-8.** No 9-family names how the reborn surface presents to
  the combinator graph or whether LOCK-8's TS mirrors 9C's compiled-DFA products.

---

## 6. Constitution & evidence-misuse scan

- **(iv) refutation laws:** the double-`vaddv` the law forbids is **still live** at `number_simd.rs:54-55`
  (owner tree) AND at `byte_class_from_eq_set_64.rs:79-87` (bbnf-simd) — stated-and-violated-in-place, across two
  passes, in *both* candidate homes.
- **(vii) concrete-artifacts-or-SPEC-ONLY:** 9B moved off SPEC-ONLY (credit). 9D is SPEC-ONLY after two passes; 9C
  produced no pass-2 artifact. **No SPEC_ONLY item anchors a "faster" claim** — checked; the families keep "faster"
  out of every gate. Clean.
- **(viii) Y.13 consumer-less:** 9D-G3 (streaming input, no consumer) stands, measured. 9A's absent `vector/`
  module (G9A-5) promotes refuted code with no no-regression artifact — a refuted substrate laundered by
  relocation; watch it.
- **(ii) true isomorphism:** X-2 (movemask triple/quad-home) and X-7 (bbnf-regex name+version collision) are two
  live tensions with "one system," both unresolved and both LOCK-5/gestalt-owned.
- **(v) zero-runtime-cost types / (vi) parsimony:** n/a and clean respectively (no god module; `simd/{mod,…}` and
  the proposed `vector/` are proper directory-modules).
- **prototype-evidence misuse:** PROTO-1 measured inline mechanics but its **negative-control premise was
  falsified**, and its perf clause is a second-pass null; PROTO-2 is the clean artifact but its gate-5 datum uses
  the wrong baseline (X-8b). FAMILY-SPECS is **stale against both** (§0). No fabrication; both protos self-flag.

---

## 7. Roll-up (critic-2 numbers; independent of critic-1)

| family | disp (concur) | CRIT-2 % | Δ vs pass-1 | the sentence that caps it |
|---|---|---:|:--:|---|
| **9A** | ADVANCED born-RED | **55%** | 58→55 | Live inline now measured-GREEN, but **both** headline merits fell: PROTO-2 proves the canonical producers are absent from bbnf-simd (no "free movemask") and PROTO-1 proves the OnceLock barrier is free at cgu=1 (de-face buys parity, not a win); the raison d'être in a wired consumer is still unmeasured and a refuted idiom must be purged from the shared home first. |
| **9B** | ADVANCED incumbent | **62%** | 64→62* | Banked the cheapest win and the DENSE `vpaddq` win with static disasm (14/19, 50/64) + paired ≥0.98× — off SPEC-ONLY — but gate-5 is seeded against the *un-evolved* shared crate (overstates its case), the checkasm harness is still a plan, the divan ran host-saturated off-ship, and the axis close is LOCK-6's. |
| **9C** | BLOCKED | **43%** | 44→43 | No pass-2 probe; Sheng (the pass-1 novum) retired to a measured negative shrinking the family to parity+deletion; the separability-solver core is unbuilt (elegant-reduction trap); the two-crate blocker is a name+version collision and its cited path (`crates/bbnf-regex`) is wrong. |
| **9D** | BLOCKED | **30%** | 30→30 | SPEC-ONLY after two passes; P3 — the cheap, deterministic, born-RED decider — unrun for the second consecutive pass; memory center refuted-by-default; consumer measured-absent (Y.13). |

\* CRIT-2 raised 9B from the pass-1 registry *floor* (55) but below my own pass-1 64: it banked artifacts (up
from floor) yet its axis-deciding gate-5 is seeded against the wrong baseline and unrun at merged-surface (down
from an un-caveated 64).

**Blockers LOCK-9 CANNOT retire alone (→ gestalt/other locks):** 9A⊗9B verdict → LOCK-6 (X-1, X-8); canonical
movemask home-collapse → LOCK-9 ∩ LOCK-5 (X-2, measurement now done, home owed); prefilter double-home →
GN-2/PT-1 dispatch law (X-3); `bbnf-regex` name+version collision → LOCK-5 (X-7); 9C grammar→DFA source-of-truth
→ LOCK-5 (unbuilt); 9C tier-selection + 9D KernelPlan → LOCK-2 (unbuilt island); 9C/9D collapse → gestalt-Ω (X-5);
un-homed scanner→combinator/TS edge → LOCK-4/LOCK-8 (X-6).

**One-line verdict:** pass-2 finally banked one real family (9B, via PROTO-2's disasm), but the synthesis is
stale against its own probes — PROTO-1 falsified a pass-1 "do-not-re-litigate" fact and PROTO-2 refuted 9A's
"free movemask," so 9A's two headline merits are gone while its raison d'être stays unmeasured; and for the
**second consecutive pass** both probes went to the central axis, leaving 9C's grammar-sourced census and 9D-P3
— the two cheap born-RED deciders — unrun. The lock's central axis (9A⊗9B) remains a LOCK-6 verdict, and pass-2's
evidence, once the wrong-baseline gate-5 (X-8b) and the un-charged de-face cost (X-8a) are corrected, tilts the
axis toward 9B without closing it.
