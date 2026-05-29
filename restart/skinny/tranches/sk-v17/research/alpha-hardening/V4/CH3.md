# CH3 REGRESSION — Pass Alpha SK-V17 Hardening (cycle V4)

Lens: CH3 (PASS-ALPHA §3 — does any proposed intervention re-open a REDRESS
pre-block? Cross-check the αE shortlist against entries 1-N. Has αC correctly
identified the pre-block list?). Host: aarch64 Apple M5 Max only. HEAD of record
`1c5bd7a25` (`git rev-parse --short HEAD` re-confirmed this cycle).

Reviewer focus (verbatim mandate): no candidate re-opens a REDRESS pre-block (AZ-IV
eager materialization, StructRegistry indirection, fact-stream, broadcast, FNV, x86);
αC pre-block list correct.

Subjects reviewed (V4): `research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md`
+ `SYNTHESIS.md` + `HANDOFF.md`. (No `alphaF.md` exists; the α-F contract draft is
folded into `SYNTHESIS.md` + `HANDOFF.md` per PASS-ALPHA §2 α-F row + §6 output
structure — correct, not a defect.)

## §0 — V3→V4 fold verification + convergence-stability check

V3 CH3 returned **ACCEPT 40 / REVISE 0 / REJECT 0 = 100%** with zero orphan REVISE
and both V2 REVISEs folded + all four V2 carry-forward obligations discharged in the
now-extant α-F deliverable. V4's mandate is therefore a stability + independent
re-verification pass: confirm nothing regressed, the candidate set is unchanged, and
every regression tripwire is still greppable on the benched skinny surface live.

**File stability:** the entire `sk-v17/` tranche is untracked (`git status --short`
= `?? restart/skinny/tranches/sk-v17/`; orchestrator commits after CHALLENGE). αE
carries a V4 cycle stamp (`alphaE.md:1,9`) but its V4 changelog
(`alphaE.md:12-43`) states verbatim: "The candidate content below is therefore
UNCHANGED from V3 (it converged); only this changelog and the cycle stamp advance
to V4." The three V3-fold edits it records all target **sibling** artefacts
(alphaA:138-148, alphaC:227-231, alphaD:154), NOT any α-E candidate / gate / owner
path / LOC budget / risk class / pre-block. The candidate set is exactly
**C0/C1/C2/C3/C4a/C4b** (`alphaE.md:163,216,298,364,412,445`) — identical to what V3
dispositioned at 11 ACCEPT. **No new candidate, no new pre-block, no pre-block
deletion.** The V4 advance is a pure cycle stamp; content converged at V3.

| V3 close item | V4 status | Live re-verification this cycle (NOT inherited) |
|---|---|---|
| αC §1 prospective-type-name fold (`CssTypedValue`/`CssColor`/`CssDimension` illustrative-only, grep-clean-absent) | **HOLDS** | Live grep `grep -rl` over `skinny/crates/`+`skinny/xtask/`: `CssTypedValue`=0, `CssColor`=0, `CssDimension`=0 files. The gate keys on the construct (per-leaf typed/`f64`/`Box` alloc), not the symbol. SYNTHESIS:178 names `CssTypedValue` as a *prospective reconstruction target*, NOT as an extant grep gate — §0.4 pre-block (`:191-194`) keys on the construct (`Box<CssColor>` per-color alloc), confirming αC's framing. CH3-clean. |
| 6-vs-24 broadcast reconciliation (αC §4/§7, αD §3, SYNTHESIS §0.2, HANDOFF) | **HOLDS** | `grep -c AUDIT-FALSIFIED skinny/RESULTS.md` = **24**; `grep -c '^| css_l4/'` = **24** (rows 112-135); `grep -c 'css_l4/.*/direct_to_struct/main'` = **25** (24 rows + the prose REDRESS-127 companion at `:154`, confirmed not a table row). The broadcast tuple `track1_mbps=2319.041;cssparser_mbps=2362.037;lightningcss_mbps=929.281` confirmed identical on rows 112 and 135, all `not_admitted` / `AUDIT-FALSIFIED`. Count = 24, range 112-135. CLOSED. |
| Carry-fwd #1-#4 (skinny-tree gates / fact-stream retirement clause / construct-framed §0.4 / 6-vs-24 reconciliation) | **HOLD** | SYNTHESIS §0.4 (`:185-264`) + HANDOFF Pre-Blocked Routes (`:148-185`) re-read live: construct-framed, all six CONTEXT pre-blocks verbatim, the `W5C_REQUEST_FACT_PROFILES` retire bullet, the "No second substrate" Lock-1 bullet, cross-call classifier-state retention named. No gate keys on `crates/core/`. |

## §1 — Independent live verification (every disposition grounded this cycle)

Re-greped the skinny benched tree at HEAD `1c5bd7a25`; every load-bearing CH3 claim
re-verified live, not inherited from V3:

- **Eight core-tree pre-block symbols grep-clean-absent in skinny** (the
  disambiguation rests on this): `StructLayout`, `OpenFrame`, `CssArena`,
  `begin_compound`, `TapeStructBuilder`, `CssTypedValue`, `CssColor`, `CssDimension`
  → **0 files each** across `skinny/crates/` + `skinny/xtask/`. **Confirmed.**
- **24-row broadcast:** `AUDIT-FALSIFIED` = 24; `^| css_l4/` table rows = 24
  (112-135); the 25th `css_l4/.*/direct_to_struct/main` match (`:154`) is the prose
  REDRESS-127 companion, not a row (verified by reading line 154). Identical
  `2319.041/2362.037/929.281` tuple on the bounding rows. **Confirmed; PERMANENT
  pre-block correct.**
- **Fact-stream routing (the Lock-1 parallel-substrate escape αC §3 closes):** 7
  `RuntimeEmitterKind::RequestFacts` registrations in `regen_css.rs` at exactly
  **45,63,81,99,117,135,153** (`grep -n` confirmed); `W5C_REQUEST_FACT_PROFILES` in
  `codegen/src/lib.rs` at **299,336,567,611** (decl :336, selected :299, iterated
  :567/:611). αC/αE/SYNTHESIS/HANDOFF all cite these exactly. **Confirmed.**
- **Orphan udot:** `parse_4_digits_dotprod` defined `aarch64/digit_mac.rs:27`; the
  ONLY call site is `digit_mac.rs:12` — inside the sibling `parse_4_digits` behind a
  `#[cfg(target_feature="dotprod")]` compile-time gate, **with NO production CSS/JSON
  parse-leaf caller.** The udot scan is banked-but-unwired into any benched parse
  path. C4a "wire the orphan into the CSS number leaf" (anti-orphan) framing is
  exact. **Confirmed.**
- **i8mm grep-clean-absent** from skinny (`grep -rn i8mm skinny/crates/` → **0**).
  C4b "NET-NEW kernel … GATED behind Wave-5 re-profile, no orphan kernel" framing
  correct. **Confirmed.**
- **Fixture overfit fingerprint:** `grep -c 'fn parse_'
  generated_real_typed.rs` = **148** (NOT the stale doc 187). αC §5 / αE / αD all
  corrected. **Confirmed.**
- **Lock anchors re-read live:** Lock 1 (`LOCKS.md:75` substrate-union + no parallel
  substrate + no `Vec<OpenFrame>::clone` 86.07% pathology + no-rename 2026-05-04
  amendment; fact-stream clause: string-only fact streams / second tape / cross-call
  classifier state all "remain rejected unless a later G-Omega explicitly amends"),
  Lock 2 (`:160` term `StructLayout` RETIRED → `Layout`/`LayoutFacts`), Lock 8 (`:595`
  repeated throughput tuples across conceptual row IDs non-admit + CSS close requires
  typed value + same-workload cssparser equality before lightningcss admit), Lock 16
  (`:607` SVE/SVE2 must NOT be filed as NEON; DotProd/I8MM require manifest + consumer
  proof), Lock 14 (`:387` "witnessed grammars … may not use fleet-wide grammar-neutral
  wording"). **All load-bearing, all accurate.**

## §2 — Verdict on the reviewer's two core questions

### Q1 — Does any candidate (C0, C1, C2, C3, C4a, C4b) re-open a pre-block? **NO.**

| Candidate | Pre-block re-open surface | Verdict (live-verified) |
|---|---|---|
| C0 de-fact-stream typed Track 1 (`alphaE.md:163-214`) | re-bench String as typed (PB#3); StructRegistry/eager route (PB#1/#2a); hand-curated catalogue (PB#5b) | NOT re-opened. C0:207-214 forbids `emit_fact_stream`/`fnv64`/schema headers as admission, StructRegistry/Arena<G>/Builder<G>, eager-by-default, per-grammar catalogue; names BOTH skinny fingerprints (148-fn `generated_real_typed.rs` + `W5C_REQUEST_FACT_PROFILES`) on the RETIRE/derive-from-grammar list. The typed summary IS the de-fact-stream. |
| C1 tape wiring + lazy cursor (`alphaE.md:216-296`) | second tape / Vec-clone / per-leaf `Box<CssColor>` (PB#1/#2a, Lock 1); relocated overfit into projection DATA (Lock 14) | NOT re-opened. C1:285-296 forbids parallel/second tape, Vec-clone, columnar SoA, per-leaf eager `Box::new`, relocated-overfit-into-projection-data; carries a derive-from-`.bbnf` pruning test. "No new cursor/builder type — the existing `Tape`/`ValueRef`/`TapeBuilder` is the single substrate" (`:224-225`). Rides the single landed substrate (Lock 1, `LOCKS.md:75`). |
| C2 NEON structural pre-scan (`alphaE.md:298-362`) | x86/AVX (PB#6); CSS-specific scanner vocabulary (Lock 14); cross-call classifier-state retention (Lock 1) | NOT re-opened. C2:356-362 forbids x86/AVX, cross-call classifier state, CSS-specific vocabulary; reuses checkasm-gated grammar-general kernels; NEON emits ONLY a `Vec<u32>` index (Lock 1 transient producer, `LOCKS.md:75`). scalar-ref + checkasm present (`:318-333`); the core-tree ~56%/~10% %% carry an explicit S-P1-re-confirm-on-benched-path obligation (actual-profiling, `:300-305`). |
| C3 commit-by-construction spine (`alphaE.md:364-410`) | speculative-rollback disguise; type-ambivalent dual representation (Lock 1) | NOT re-opened. C3:408-410 forbids type-ambivalent dual representation and speculative-rollback re-introduction as a fast path. REMOVES checkpoints, adds no mechanism (`:380-381`); builds on the banked SK-V16 O(1) checkpoint. |
| C4a wire orphan udot (`alphaE.md:412-443`) | x86/AVX (PB#6); fixture/per-corpus capacity consts (PB#5b); per-leaf feature detection | NOT re-opened. C4a:440-443 forbids x86/AVX, per-leaf `is_aarch64_feature_detected!` (dotprod is compile-time `target_feature`), fixture/capacity literals; the candidate's WHOLE purpose is to RETIRE the digit_mac orphan (anti-orphan). scalar-ref (`:423-425`) + checkasm (`:426-428`) present. |
| C4b NET-NEW i8mm kernel (`alphaE.md:445-492`) | x86/AVX-512/SVE filed as NEON (PB#6, Lock 16); orphan kernel; per-leaf detection | NOT re-opened. C4b:487-492 forbids x86/AVX-512, SVE (Apple no-SVE dead code, Lock 16 `LOCKS.md:607`), per-leaf detection (threads OnceLock ONCE); scalar-ref + checkasm REQUIRED for the new kernel (`:461-467`); **GATED behind a Wave-5 re-profile** (`:471-473`) so no orphan kernel lands if the digit leaf is not top-N tailwind self-time. |

Every candidate carries an explicit, correctly-scoped "REDRESS pre-blocks"
subsection; the SIMD candidates (C2/C4a/C4b) attach scalar-ref + checkasm +
same-wave-consumer. **No candidate's admission framing lands on the
OpenFrame / StructRegistry / Vec<Vec> / fact-stream-as-admission / broadcast /
FNV-arbiter / x86 carrier.** The αC §8 single load-bearing distinction
(`alphaC.md:361-371`: "typed/rich/retained is the goal; eager/allocating/
fragmented/serialized is the refuted carrier") is the correct regression
discriminant, and the V4 candidates respect it. The C4a/C4b split (unconditional
orphan-wiring vs GATED net-new kernel) eliminates the orphan-kernel admission risk.

### Q2 — Is αC's pre-block list correct + complete? **YES.**

αC enumerates exactly the six CONTEXT-named pre-blocks (AZ-IV eager §1,
StructRegistry indirection §2, fact-stream String §3, 24-row broadcast §4,
FNV/fixture §5, x86/AVX §6), splits #2 into **2a** (PERMANENT: the per-leaf
indirection) and **2b** (ADMIT-UNDER-FRAMING: the layout description itself, re-keyed
to `BackendRule`/`LayoutFacts` at `ir/cost.rs:119-121,259-271`), and the §7
consolidated ledger (`alphaC.md:346-355`) is faithful to the measured refutations
(118x `cb14970f`; 28-65x / 983x / 10583x WATCHDOG; ~34% emit_* self-time;
one-tuple-×-24 live-verified; 148 fixtures live-verified; x86 out-of-scope). The
two-bucket PERMANENT-vs-ADMIT-UNDER-DIFFERENT-FRAMING taxonomy is the correct
regression model — it prevents both the false-negative (re-admitting the refuted
carrier) and the false-positive (blocking the legitimate typed-rich intent). The
TREE-DISAMBIGUATION header (`alphaC.md:18-46`) + the fact-stream retirement clause
(`:195-199`) + the §1 prospective-type-name correction (`:75-83`) make every re-open
test greppable on the skinny benched surface. **No pre-block missing, no over-block.**

---

## §3 — Per-section dispositions

### alphaA (results extraction) — CH3-neutral
- **§0-§6 + Pre-blocked-routes note: ACCEPT.** No regression-hypothesis transfer.
  The broadcast count is stated as 24 (live-verified). The "Pre-blocked (do NOT
  re-open — see αC)" deferral + watermark-route guard are CH3-clean. The V3 fold at
  `:138-148` (cross-artefact reconciliation rewritten) is verified-landed; CH3-neutral.

### alphaB (competitor deltas) — CH3-neutral
- **All sections: ACCEPT.** No candidate proposed. Keeps lightningcss as the fair
  full-CSSOM bar / cssparser as the admission gate (PB#4 comparator-confusion out by
  construction). Per-corpus endpoints self-flagged INFERRED, carried as
  UNMEASURED-PENDING in SYNTHESIS §0.5 (anti-broadcast hygiene). CH3-clean.

### alphaC (REDRESS digest) — the load-bearing artefact for CH3
- **§0 TREE-DISAMBIGUATION header + two-bucket model: ACCEPT.** Every doc symbol
  translated to the skinny surface; "a gate keyed to `crates/core/...` is a CH1
  defect" stated up front. The single best regression-hygiene artefact in the set.
- **§1 AZ-IV eager (118x): ACCEPT.** Prospective-type-name fold holds
  (`CssTypedValue`/`CssColor`/`CssDimension` = 0 live); tripwire anchored to the
  construct + verified grep surface (`runtime/src/grammars/css_l4_*/` +
  `nonjson_css_l4.rs:596-624`). ADMIT-UNDER-FRAMING + payload-arena telemetry correct.
- **§2 StructRegistry / 2a+2b split: ACCEPT.** 2a PERMANENT (no framing recovers a
  per-leaf registry deref), 2b ADMIT-UNDER-FRAMING re-keyed to `BackendRule`/
  `LayoutFacts` (`ir/cost.rs:119-121,259-271`); Lock 2 RETIRED-`StructLayout` cited
  correctly (`LOCKS.md:160`).
- **§3 fact-stream String: ACCEPT.** PERMANENT-as-admission / ADMIT-as-diagnostic-only
  correct; the retirement clause (`:195-199`) makes "the 7 `RequestFacts` registrations
  (regen_css.rs:45..153) + `W5C_REQUEST_FACT_PROFILES` (lib.rs:336) STILL standing" a
  CH3/CH5 failure-if-NOT-done, closing the Lock 1 parallel-substrate escape. All
  citations live-verified exact.
- **§4 24-row broadcast: ACCEPT.** "24 falsified rows … lines 112-135, grep-verified";
  live count = 24, range 112-135, identical tuple. The basis note ("NOT six") is
  correct. PERMANENT-PRE-BLOCK verdict correct.
- **§5 FNV / fixture: ACCEPT.** 5a/5b split correct; 148-fixture count live-verified;
  the residual is "input.len() + `BackendRule`/`LayoutFacts`, grammar-general."
- **§6 x86 / AVX: ACCEPT.** PERMANENT-this-pass; Apple no-SVE dead-code argument
  correct (Lock 16 `LOCKS.md:607` verified); x86 reserved as successor phase
  (PASS-ALPHA §8), zero SK-V17 admission weight.
- **§7 consolidated ledger: ACCEPT.** Row 4 reads 24; row 2b re-keyed to skinny;
  Lock-2 `Layout`/`LayoutFacts` NB present; all six pre-blocks with skinny-keyed
  re-open tests. Complete.
- **§8 single load-bearing distinction: ACCEPT.** The correct one-line regression law;
  the "no `RequestFacts`/`W5C_REQUEST_FACT_PROFILES` route still admitting" addendum
  closes the Lock-1 escape.

### alphaD (validated/invalidated ledger)
- **§0 benched-surface disambiguation: ACCEPT.** doc→skinny translation grep-verified.
- **§1 V1-V6 validated wins: ACCEPT.** V6 re-pathed to skinny tape; V2-V5 attribute
  commit SHAs.
- **§2 invalidated ledger (I1-I7): ACCEPT.** The CH3 backbone. I5 (AZ-IV pre-block,
  no re-open) + I6 (timeline-misattribution: `cb14970f` 2026-05-02 predates restart
  `a5145a0bb` 2026-05-03) are the two regression traps, correctly disposed. I1/I2
  forbid hypothesis-transfer. I7 (N=1 invalid → N≥50 median) is the telemetry guard.
  Pre-block footer names `W5C_REQUEST_FACT_PROFILES`.
- **§3 still-open (O1-O5): ACCEPT.** Count = 24 (live-verified); the V3 fold at `:154`
  (css_l4.toml LOC relabelled INFORMATIONAL-not-gate, SK-V18 totality fold) verified;
  O5 retire-list names `W5C_REQUEST_FACT_PROFILES` + the 148-fn surface.
- **§4 demoted + §5 ledger text: ACCEPT.** "Micro-opt does not move the floor" is the
  correct anti-regression posture.

### alphaE (candidate shortlist) — the cross-check target for CH3
- **§0 ground-truth anchors + translation correction: ACCEPT.** Every anchor
  re-verified live (dispatch.rs, digit_mac.rs orphan call-site `:12` only,
  generated.rs fact-stream, nonjson_css_l4.rs:596, `W5C_REQUEST_FACT_PROFILES:336`,
  148 fixtures, i8mm-absent, sheets_witness stub). The αE:127-151 translation
  correction is the cleanest regression-hygiene artefact.
- **C0 de-fact-stream: ACCEPT.** Pre-block subsection (C0:207-214) forbids
  fact-stream-as-admission / StructRegistry / eager-by-default / per-grammar
  catalogue; names both skinny fingerprints on the retire list. No re-open.
- **C1 tape wiring: ACCEPT.** Pre-block subsection (C1:285-296) forbids second tape /
  Vec-clone / columnar SoA / per-leaf `Box::new`; carries the no-relocated-overfit
  pruning test + the JSON+CSS EXIT gate. The sheets_witness-struck repair (option b′)
  removes a non-dischargeable phantom generality target. No re-open.
- **C2 NEON pre-scan: ACCEPT.** Pre-block subsection (C2:356-362) forbids x86/AVX /
  cross-call carry / CSS-specific vocabulary; scalar-ref + checkasm present; NEON
  emits only `Vec<u32>` (Lock 1 transient producer); `lo6_table_admissible` is the
  honest scalar-fallback guard; the %% carry the S-P1-re-confirm obligation. No re-open.
- **C3 commit-by-construction: ACCEPT.** Pre-block subsection (C3:408-410) forbids
  type-ambivalent dual representation / speculative-rollback disguise. REMOVES
  mechanism. No re-open.
- **C4a wire orphan udot: ACCEPT.** LOW risk, admits unconditionally (scalar-ref +
  checkasm present); purpose is to RETIRE the orphan (anti-orphan); no per-leaf
  `is_aarch64_feature_detected!`. No re-open.
- **C4b NET-NEW i8mm kernel: ACCEPT.** The GATE (lands ONLY if a Wave-5 re-profile
  proves the digit leaf top-N tailwind self-time) is the correct anti-orphan-kernel
  discipline; scalar-ref + checkasm REQUIRED; no SVE-as-NEON; no x86; honest-residual
  exit (no paper-close). No re-open.
- **§2 dependency order, §3 cross-cutting discipline, §4 escalation: ACCEPT.** N≥50
  median + 8-field EXACT equality + grammar-neutral-witnessed-not-asserted (JSON+CSS
  only, Sheets deferred SK-V18) bind every gate; the borrowed-slice-vs-lazy directive
  elevated to a C1 ENTRY GATE (the documented W6 stall).

### SYNTHESIS.md (α-F deliverable)
- **§0.4 Pre-blocks (`:185-264`): ACCEPT.** Construct-framed. All six CONTEXT
  pre-blocks present verbatim + the `W5C_REQUEST_FACT_PROFILES` retire bullet (`:203-208`)
  + the "No second substrate" Lock-1 type-ambivalence bullet (`:236-239`,
  pre-blocking an introduced skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`) +
  the cross-call classifier-state retention pre-block (`:235`). The Generality clause
  (`:241-264`) correctly scopes the witness to JSON+CSS, defers Sheets/BBNF-self to
  SK-V18, and explicitly does NOT claim the Lock 14 CSS+Sheets minimum is met
  (`LOCKS.md:386-387` verified) — removing a phantom generality assertion that would
  be a relocated-overfit escape hatch. (Note re SYNTHESIS:178: `CssTypedValue` appears
  as a prospective *reconstruction target* in the lazy-view generator row, NOT as an
  extant-symbol gate; the §0.4 pre-blocks key on the construct — consistent with αC
  §1. CH3-clean.)
- **§0.1/§0.2/§0.3/§0.5/§0.6 + Section 1-3 telemetry schema: ACCEPT.** Every owner
  path is the benched skinny tree; the 24-count reconciliation (`§0.2`) names the
  grep-verified 24; the seam-flip site is the seven `RequestFactsProfile` literals
  (regen_css.rs:45..153, verified) + the two consumer loops (lib.rs:567,611,
  verified); the telemetry schema makes every regression tripwire a gate column
  (`tape_activated` not-grep-`crates/core/`, `w5c_profile_array_retired`,
  `projection_generality_exercise` with `sheets_witness` explicitly NOT a valid value,
  `simd_non_json_exercise=css_l4`, sample_count≥50, no one-tuple-across-rows).

### HANDOFF.md (α-F deliverable)
- **Pre-Blocked Routes (`:148-185`): ACCEPT.** Mirrors SYNTHESIS §0.4: all six
  pre-blocks verbatim, the W5C retire bullet (`:160-161`), the 24-row broadcast with
  lines 112-135 (`:162-164`), the "No second substrate" Lock-1 bullet (`:171-174`),
  and the hidden-coupling escape list with cross-call classifier-state retention
  named (`:179-185`). Binding on S-P0..S-P3. CH3-clean.
- **Current State + CH7 scope + Next Move: ACCEPT.** The 24-count stated correctly;
  CH7 overfit-prune framed as pass-added monotonic extension scanning
  `W5C_REQUEST_FACT_PROFILES` retirement + no-relocated-overfit + `.bbnf`-derivation;
  C4a unconditional / C4b gated-behind-re-profile preserved.

---

## §Disposition counts

- Total artefact sections dispositioned this cycle: **40**
  (alphaA 2 [§0-§6 block + Pre-blocked-routes note], alphaB 1, alphaC 9
  [§0,§1,§2,§3,§4,§5,§6,§7,§8], alphaD 5 [§0,§1,§2,§3,§4+§5], alphaE 11
  [§0,C0,C1,C2,C3,C4a,C4b,§2,§3,§4 + Q1/Q2 cross-check], SYNTHESIS 8 [§0.1,§0.2,§0.3,
  §0.4,§0.5,§0.6,Section1-3,benched-note], HANDOFF 4 [Pre-Blocked Routes, Current
  State, CH7 scope, Next Move]).
- **ACCEPT: 40**
- **REVISE: 0**
- **REJECT: 0**

**Both V2 REVISEs remain folded; all four V2 carry-forward obligations remain
discharged; the V3 100% ACCEPT holds at V4. Zero orphan REVISE. The candidate set is
unchanged (C0/C1/C2/C3/C4a/C4b); the V4 advance is a pure cycle stamp.**

## §CH3 bottom line

**No candidate (C0, C1, C2, C3, C4a, C4b) re-opens any REDRESS pre-block.** Each
carries a correctly-scoped, skinny-keyed pre-block subsection, routes through the
tape+lazy-view "different framing", and the SIMD candidates attach scalar-ref +
checkasm + same-wave-consumer. The C4a/C4b split and the fact-stream retirement
clause close the orphan-kernel and parallel-substrate escapes.

**αC's pre-block list is correct + complete** — six pre-blocks, the 2a/2b
PERMANENT-vs-ADMIT split sound, every measured refutation re-verified live this cycle
(eight core-tree symbols = 0; AUDIT-FALSIFIED = 24 at rows 112-135 with identical
tuple; 7 RequestFacts registrations + W5C at lib.rs:336/567/611; udot orphan called
only at digit_mac.rs:12 with no prod parse-leaf caller; i8mm = 0; 148 fixtures;
Lock 1/2/8/16 anchors accurate).

CH3 is at **40/40 = 100% ACCEPT** at V4. The artefacts converged at V3 and remain
stable; this V4 pass independently re-verified every regression tripwire live on the
benched skinny surface and confirms no regression. **No regression tripwire is
unverifiable on the benched surface; no candidate re-opens a pre-block; no orphan
REVISE carries forward.**
