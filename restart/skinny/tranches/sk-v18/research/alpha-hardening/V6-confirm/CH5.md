# CH5 — HIDDEN-COUPLING (lens, cycle V6-CONFIRM) — SK-V18 Pass-Alpha artefact review

Lens: CH5 Hidden Coupling (PASS-ALPHA §3 / ORCHESTRATOR §3W). Cycle: V6 CONFIRMING.
HEAD of record: `318d9c046` (SK-V17 closed `f6a38445b`; V3 audit `7dbe44c22`). This is a
CONFIRMING re-review AFTER the two orphan REVISE folds (CH2 §8.1 F16 field-enumeration +
CH5 F.6 P1 x86-deletion-list widening). Mandate: ACCEPT where correct+complete; verify the
two folds discharged; flag only genuine residual defects; do NOT re-litigate the converged
goalset. Discipline carried: aarch64 only; substrate-union Lock 1; preserve >SOTA.

Artefacts reviewed (live on disk): `SYNTHESIS.md` + `HANDOFF.md` (the αF binding contract —
no separate `alphaF.md`; SYNTHESIS+HANDOFF ARE the αF output per V5/CH5:12-14) +
`research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md` feeders. The V5 CH5 wave closed
ACCEPT 24 / REVISE 1 / REJECT 0, the single REVISE being F.6 (P1 deletion-list reach). This
cycle audits whether F.6 landed in the BINDING inventory-of-record it was filed against, and
re-verifies the four core CH5 axes survive at HEAD.

## §0 — Live ground-truth re-verification (re-grepped at HEAD `318d9c046` THIS cycle)

| Claim | Command / inspection | Result |
|---|---|---|
| P1 verify-grep firing surface (`-l`, the F.6 domain) | `grep -rilE --include='*.rs' --include='Cargo.toml' 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` | fires on `Cargo.toml`, `build.rs`, `src/lib.rs`, **`src/scalar/byte_class_from_eq_set_64.rs`**, **`tests/checkasm_parity.rs`** (+ the `src/x86_64/` tree). The two non-`x86_64/` escapees of the F.6 finding STILL fire. |
| `tests/checkasm_parity.rs` active x86 coupling | `grep -cE 'x86_64' …; grep -nE 'bbnf_simd::x86_64'` | **11 tokens**; **9 ACTIVE compile-coupled call sites** `:458,:464,:467,:477,:478,:484,:493,:497,:502` (`bbnf_simd::x86_64::avx2::classify::classify_block_scalar`, `…avx2::bmi2_emit::compact_mask_scalar`, `…avx2::prefix_xor::prefix_xor_scalar`, `…avx512_vbmi2::classify::classify_block_scalar`, `…avx512_gfni::classify_affine::classify_block_scalar`, `…avx512_bitalg::multiclass::classify_full_scalar`, `…avx512_vbmi2::mask_fuse::fuse_emit_scalar`, `…avx_ifma::mantissa::mul52_low_scalar`, `…avx512_vnni::digit_mac::parse_8_digits_scalar`). **Compile-coupled to `pub mod x86_64`** — deleting `src/x86_64/` WITHOUT decoupling these breaks the build. |
| `src/scalar/byte_class_from_eq_set_64.rs` x86 hits | `grep -inE 'avx\|x86' …` | `:10,:12,:15` comment-only ("vector body (NEON, AVX-512 BW)", "AVX-512 BW", "32-byte AVX2 path"). Grep fires; benign body. |
| `RuntimeTarget` struct (the F16 enumeration target) | `regen.rs:6-19` | **12 fields**: `grammar_name`/`profile`/`entry_rule`/`source_roots`/`output_dir`/`check_command`/`source_inputs`/`metadata_inputs`/`emitter`/`expected_files`/`frontend_requirements`/`output_labels`. `profile` (:8) present; `source_inputs` (:13)/`metadata_inputs` (:14) present. By-exclusion of the 2 path columns ⇒ 10 operative. |
| No second substrate (type level) | `grep -rln 'StructLayout\|TapeStructBuilder\|TapeCursor' …runtime/src …codegen/src` | **empty** — pre-block intact. |
| Forked emitter live (G3 subject) | `grep -rn 'enum RuntimeEmitterKind' …codegen/src` | `grammar_provider.rs:40` live. |
| `CssEventGrammar` absent / shared trait absent | `grep -rln 'CssEventGrammar\|SharedValueTrait\|trait Value\b\|trait Cursor\b' …runtime/src` | **empty** — DELETE-default + divergent-API surface unchanged. |

The four core CH5 axes (no second substrate, shared trait no-fork, phantom `<G>`
instantiate-or-delete on the `G` axis, relocated-seam policed structurally) remain disk-true
and gate-bound — unchanged from V5, re-confirmed. The load-bearing new evidence is rows 1-3:
the F.6 grep firing surface is UNCHANGED at HEAD.

---

## §1 — CH5 F.6 fold (P1 x86-deletion-list widening) — DISPOSITION: PARTIALLY DISCHARGED → REVISE

The V5 F.6 REVISE was filed with a precise, three-site target (V5/CH5:140-152, §7.2):
"add to the **binding** SYNTHESIS P1 deletion list (`:315`/`:326`) + the `x86_tree_deleted`
telemetry (`:563`/`:576`) + the **HANDOFF P1 receiver** (`:101-112`)" the two removal targets
(h) `tests/checkasm_parity.rs:454-482` and (i) `src/scalar/byte_class_from_eq_set_64.rs:10-15`,
"and make the binding verify grep `tests/`-inclusive." The finding's essence: **"the binding
inventory-of-record is narrower than its own grep, AND silent on the compile-coupling."**

### F.6a — The two FEEDERS carry the fold. ACCEPT (verification of feeder reach).

- **αC** (`alphaC-redress-digest.md:168-176,188-193,212-215`) is reach-complete and names
  `tests/checkasm_parity.rs` "COMPILE-COUPLED, NOT doc/test-only (V5 R-2/F.6)" with the 9
  active call sites, AND `src/scalar/byte_class_from_eq_set_64.rs` (`:10,:12,:15`) to re-word
  aarch64-neutral, AND the build-soundness rationale ("the `checkasm_parity.rs` decoupling
  (4a) is what keeps the `src/x86_64/` deletion from breaking compilation"). ✓
- **αE** (`alphaE-candidate-shortlist.md:94,101,104`) folds it into the P1 receiver as
  target **(3)** — "COMPILE-COUPLED removal/decoupling sites the verify grep ALSO fires on
  (V5 R-2/CH5 F.6 widening)", names the 9 active call sites + the `:672` `#[ignore]`
  `sk_v3_intrinsic_parity_x86_64` harness, marks `checkasm_parity.rs` DECOUPLE-not-delete,
  marks byte_class CLEAN-to-aarch64-neutral, and adds the build-soundness exit line
  ("`cargo build` AND `cargo test --no-run` clean — the `checkasm_parity.rs` decoupling (3)
  is what keeps the `src/x86_64/` deletion build-sound"). ✓

The feeder reach is correct and complete. But the V5 finding was NOT against the feeders —
V5/CH5:129-136,174-176 explicitly states "αC is reach-complete … the defect is **propagation**
… filed against the BINDING SYNTHESIS/HANDOFF rows, not αA/αC/αE." The feeders carrying the
fix is the PRE-EXISTING V5 state, not evidence of discharge.

### F.6b — The three BINDING sites the REVISE targeted are UNCHANGED. REVISE (orphan NOT closed).

Disk-true at HEAD `318d9c046`:

1. **SYNTHESIS P1 close-gate (`SYNTHESIS.md:326`)** — still enumerates **(a)–(g) only**;
   ends at "(g) the in-crate doc surfaces … scrubbed … OR the verify grep scoped to
   source+manifest." NO (h) `tests/checkasm_parity.rs`, NO (i)
   `src/scalar/byte_class_from_eq_set_64.rs`. The verify clause STILL reads verbatim "every
   active hit the grep flags is on the **(a)-(g)** removal list" — the EXACT
   RED-by-construction phrasing F.6 flagged. The grep (re-verified §0 row 1) fires on
   `tests/checkasm_parity.rs` (11 hits) + `src/scalar/byte_class_from_eq_set_64.rs` (3 hits),
   NEITHER on (a)–(g) ⇒ the gate is **RED-by-construction at HEAD**, and the
   build-coupling (deleting `src/x86_64/` (a)+(f) breaks `checkasm_parity.rs` compile) is
   **unstated** in the binding row.

2. **`x86_tree_deleted` telemetry (`SYNTHESIS.md:576`)** — the boolean's definition still
   stops at "the in-crate doc surfaces scrubbed-or-out-of-band"; it names `src/x86_64/`,
   `ext/x86/`, `build.rs`, `lib.rs:247`, `Cargo.toml:19`, `lib.rs:5`/`:285-288`, doc surfaces
   — but NOT `tests/checkasm_parity.rs` nor `src/scalar/byte_class_from_eq_set_64.rs`. The
   machine-gate the `gate-json` consumer REJECTs on therefore goes GREEN while the grep is
   RED on two unnamed files — the telemetry contradicts its own verify command.

3. **HANDOFF P1 receiver (`HANDOFF.md:101-112`)** — the receiver enumerates `src/x86_64/`,
   `ext/x86/`, `build.rs`, `lib.rs:247`, `Cargo.toml:19`, `lib.rs:5`/`:285-288`, doc surfaces,
   then "verify crate-wide … every active hit on the removal list." NO
   `tests/checkasm_parity.rs`, NO `byte_class_from_eq_set_64.rs`. (HANDOFF:17 has a passing
   meta-mention "P1 + `x86_tree_deleted` are EXTENDED so the deletion list is reach-matched"
   — but the actual P1 row at :101-112 was never extended; the prose promises a fold the
   receiver does not contain.)

**Conclusion:** F.6 is the EXACT defect it was at V5 — the binding inventory-of-record is
narrower than its own crate-wide verify grep, and silent on the compile-coupling between P1's
`src/x86_64/` deletion and the `tests/checkasm_parity.rs` test crate. The orphan REVISE is
**NOT discharged**. The fold landed in the feeders (where it already was at V5) and NOT in the
three binding sites the REVISE named. Per ORCHESTRATOR §3Z this orphan remains open.

**REVISE (not REJECT):** direction correct, fix mechanical and feeder-carried verbatim, zero
architectural re-open — but the binding rows must actually be edited. Concrete fix (unchanged
from V5/CH5 F.6, propagate αC:168-176 / αE:94 verbatim INTO the binding rows):
add **(h)** `tests/checkasm_parity.rs:454-482` (decouple-or-delete the 9 active
`bbnf_simd::x86_64::…::*_scalar(…)` call sites + the `:672` `#[ignore]`
`sk_v3_intrinsic_parity_x86_64` harness — closes the compile-coupling AND the grep RED) and
**(i)** `src/scalar/byte_class_from_eq_set_64.rs:10-15` (scrub doc x86 cross-refs
aarch64-neutral) to (1) `SYNTHESIS.md:326`, (2) `SYNTHESIS.md:576` `x86_tree_deleted`,
(3) `HANDOFF.md:101-112`; update the "(a)-(g)" reach-claim to "(a)-(i)"; state the
build-soundness invariant (`cargo test --no-run` clean) in the binding row. Then the
5-firing-file grep is matched by 5 named removal targets and the gate is
satisfiable-by-construction.

---

## §2 — CH2 §8.1 F16 fold (cross-context confirmation — sibling lens, NOT my disposition)

F16 is CH2's REVISE, not CH5's, but the dispatch asks me to verify it discharged. It IS
discharged in the binding contract:

- **SYNTHESIS G3 close-gate (`:333`, clause (iii))** now reads "(enumerate-by-exclusion over
  the live `regen.rs:6` … struct) **`profile`**/`entry_rule`/`source_roots`/`check_command`/
  **`source_inputs`**/**`metadata_inputs`**/`emitter`/`frontend_requirements`/`output_labels`
  (plus the `fact_schema`/`row_id`/`output_plane` per-profile content the `profile`
  discriminator selects) collapse to ONE distinct config-tuple per `grammar_name`", with
  "TODAY they carry 7 DISTINCT `profile` …, so this gate is correctly RED pre-P3" and "P3
  must PRESERVE profile-distinctness … not erase the `profile` discriminator." ✓
- **§0.4 pre-block (`:404-415`)** and the prose body (`:148-167`) carry the same by-exclusion
  enumeration naming `profile`/`source_inputs`/`metadata_inputs` explicitly. ✓
- The operative machine-check is now `count(distinct config-tuple-minus-(output_dir,
  expected_files)) == 1` — by-EXCLUSION, so any future `RuntimeTarget` field cannot silently
  fall outside the tuple (the V5 fix mechanism). ✓

The CH2 orphan REVISE-1 (`profile` omission) is folded into the binding rows. The P3
profile-distinctness obligation is preserved.

**One cosmetic nit (sub-REVISE, NOT a defect):** the F16 fold labels the struct "13-field"
(`:333`, `:410`); on disk `regen.rs:6-19` has **12 fields**. The enumerate-by-exclusion
mechanism is field-set-complete regardless of the printed count (it excludes the 2 path
columns and names the 10 operative fields, which matches disk exactly), so the miscount does
not weaken the gate — but the "13" is a stray. Flagging for accuracy, not as a blocking
finding (this is CH1/CH2 territory, and below the REVISE bar for CH5).

---

## §3 — Core CH5 axes + feeder re-confirmation (the V5 ACCEPTs that hold)

All re-verified disk-true at HEAD; no regression introduced by the (CH2) fold that DID land:

- **F.1 G4 gate** (phantom `<G>` on the `G` axis, shared-trait separable-from-`<G>`,
  ≥2 NON-test impls, `json_rich_navigation_preserved`, no-second-substrate). `SYNTHESIS.md:323`/
  `:555-557` intact; `CssEventGrammar`/`SharedValueTrait` absent on disk. **ACCEPT.**
- **F.2 G3 single-emitter** (arm-census FULL alphabet × codegen+xtask; type-census;
  STRUCTURAL `runtime_target_rows_collapsed`). `:333` intact + now F16-widened. **ACCEPT.**
- **F.3 §0.4 no-second-substrate pre-block** (`StructLayout`/`TapeStructBuilder`/`TapeCursor`
  REJECT; `UnionTape` forbidden-token guard). Disk-confirmed absent. **ACCEPT.**
- **F.4 Section-2 telemetry coupling columns** (`phantom_generic_resolved`,
  `shared_value_trait_instantiations>=2` NON-test, `emitter_fork_present==false`, etc., each
  gate-REJECTed at `:589-601`). **ACCEPT.**
- **F.5 plane honesty** (Track 2 ≠ Track 1; `css_typed_summary_equal` EXACT;
  `materialization_framing`; `corpus_in_timer`; JSON typed plane is guard-not-bar). **ACCEPT.**
- **αA** A.1 (phantom two-axis + no-second-substrate + fork/replica/blob + caveats). **ACCEPT ×3.**
- **αB** B.1–B.4 (plane symmetry; honest-`None`; Sheets no-competitor-bar; comparator-OUT vs
  impl-x86-scope boundary). **ACCEPT ×4.**
- **αC** C.1–C.4 (P1 reach-complete incl. `tests/`+`scalar/`; relocated-seam structural;
  §2.1–§2.6 pre-block families; Lock-1 thin-cursor). **ACCEPT ×4.**
- **αD** D.1–D.4 (I5 phantom-`G` test-only-does-not-count; S9 `G`-axis-only; §5 pre-block;
  count-fold no-coupling). **ACCEPT ×4.**
- **αE** E.1–E.4 (B1 single-emitter; B3 shared trait + phantom DELETE-default; B4
  no-orphan-kernel/Sheets-via-generator; F15 crate-wide + F16 projection; AND the F.6 (3)
  decoupling now folded in αE itself). **ACCEPT ×4.**

No new hidden-coupling surface introduced; no Track1≡Track2 dishonesty; no sidecar producer;
acceleration-wiring + orphan-kernel couplings remain gated at admission. The substrate is
singular (one `Tape`/`ValueRef`/`PayloadArena`), Lock 1 holds, >SOTA preserved per §8 ground
truth (JSON > sonic-rs strict; CSS > lightningcss 1.9–3.3× cold).

---

## §4 — Verdict

The CONFIRMING re-review finds the four core CH5 axes structurally honest AND disk-verified at
HEAD `318d9c046`, and confirms the **CH2 §8.1 F16 fold IS discharged** in the binding contract
(`profile`/`source_inputs`/`metadata_inputs` now enumerated, by-exclusion, with P3
profile-distinctness preserved — modulo a cosmetic "13-field"/12-field label slip).

But the **CH5 F.6 fold is NOT discharged**: the fix landed only in the feeders (αC/αE — where
it already was at V5) and NOT in the three binding sites the REVISE named — `SYNTHESIS.md:326`
(still "(a)-(g)"), `SYNTHESIS.md:576` `x86_tree_deleted`, `HANDOFF.md:101-112`. The defect
F.6 was filed against ("binding inventory narrower than its own crate-wide grep, silent on the
`tests/checkasm_parity.rs` compile-coupling") persists verbatim at HEAD: the verify grep fires
on `tests/checkasm_parity.rs` (11 hits, 9 active compile-coupled call sites) +
`src/scalar/byte_class_from_eq_set_64.rs` (3 hits), neither on the binding removal list ⇒
RED-by-construction gate + unstated build-coupling. This is the same orphan, NOT closed.

Per the CONFIRMING-cycle mandate, the disposition is ACCEPT on every core axis and feeder
(the artefact is correct+complete THERE), and a single REVISE on the binding F.6 propagation
that the dispatch asserted was folded but is not. This is a genuine residual defect, not a
re-litigation — the goalset, the §0.4 pre-blocks, the telemetry spine, and the αC/αE feeder
reach are all sound; the one open item is the mechanical propagation of αC:168-176 verbatim
into `SYNTHESIS.md:326`/`:576` + `HANDOFF.md:101-112`.

**§3Z impact:** one CH5 orphan REVISE remains open at HEAD. The second-consecutive ≥95%
orphan-free wave is NOT yet recorded for the CH5 lens until F.6 lands in the binding rows.

## §5 — Tally

CH5 reviewed six artefact-sections (αA, αB, αC, αD, αE, SYNTHESIS+HANDOFF) + the two fold
verifications. Core axes + feeders ACCEPT; CH2 F16 confirmed discharged (sibling-lens note,
not scored in CH5); CH5 F.6 NOT discharged in the binding rows ⇒ one REVISE.

- αA: ACCEPT ×3
- αB: ACCEPT ×4
- αC: ACCEPT ×4
- αD: ACCEPT ×4
- αE: ACCEPT ×4
- SYNTHESIS+HANDOFF: ACCEPT ×5 (F.1–F.5 core axes), REVISE ×1 (F.6 — binding P1 deletion-list
  reach STILL "(a)-(g)" at `SYNTHESIS.md:326`/`:576` + `HANDOFF.md:101-112`; `tests/checkasm_parity.rs`
  + `src/scalar/byte_class_from_eq_set_64.rs` fire the binding grep but are unnamed; fold landed
  in feeders only)

Total: ACCEPT 24, REVISE 1, REJECT 0.

TALLY accept=24 revise=1 reject=0
