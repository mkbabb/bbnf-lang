# SK-V18 Pass-Alpha CHALLENGE — V6-confirm · CH7 OVERFIT-PRUNE (CONFIRMING re-review)

CONFIRMING cycle over the αF binding contract (`SYNTHESIS.md` + `HANDOFF.md`) + the αA–αE
feeders, AFTER the two orphan REVISE folds (F16 field-enumeration; P1/F.6 x86 deletion-list
widening) were dispatched. Lens CH7 (OVERFIT-PRUNE): does the shortlist delete the overfit
without resurrecting it, and are the close-gates that police the prune satisfiable-by-
construction (no RED-by-construction paper-close hazard, the OVERFIT-PRUNE failure mode)?
Disk-verified live at HEAD `318d9c046`. ACCEPT where correct+complete; verify the two folds
discharged; flag only genuine residual defects. Cite `path:line`.

---

## §1 — Fold-discharge verification (the two orphan REVISEs)

### FOLD A — CH2 §8.1 / F16 (the `profile`/`source_inputs`/`metadata_inputs` enumeration) — **DISCHARGED, every binding surface**

The V5 REVISE-1 required the operative `runtime_target_rows_collapsed` machine-check to STOP
being a strict subset of its own prose — enumerate-by-EXCLUSION of the two path columns
(`output_dir`, `expected_files`) so `profile` (the 7-distinct per-profile discriminator) +
`source_inputs` + `metadata_inputs` cannot fall outside the tuple, AND preserve profile-
distinctness through the P3 collapse. Re-verified landed at every site CH2 named:

- **SYNTHESIS binding G3 close-gate row** (`SYNTHESIS.md:333`, the `(iii)` relocated-seam
  clause): now reads "enumerate-by-exclusion over the live `regen.rs:6` 13-field struct"
  with the operative set naming **`profile`**/`entry_rule`/`source_roots`/`check_command`/
  **`source_inputs`**/**`metadata_inputs`**/`emitter`/`frontend_requirements`/`output_labels`
  and the explicit "TODAY they carry 7 DISTINCT `profile` + per-profile `source_inputs`/
  `metadata_inputs`" RED-pre-P3 framing + "P3 must PRESERVE profile-distinctness … not erase
  the `profile` discriminator."
- **SYNTHESIS `runtime_target_rows_collapsed` telemetry column** (`SYNTHESIS.md:566`): the
  by-exclusion enumeration + `count(distinct config-tuple-minus-(output_dir,expected_files))
  == 1` + the `profile` discriminator are all present.
- **SYNTHESIS §8.1 narrative + `W5C_REQUEST_FACT_PROFILES` addendum** (`:133-167`, `:404-415`,
  `:608-609`): the prose-vs-operative equality fix is stated by exclusion with `profile`/
  `source_inputs`/`metadata_inputs` named.
- **HANDOFF** (`HANDOFF.md:21-25`, `:273-279`, `:330-332`): projection WIDENED from
  `(source_roots,entry_rule)` to "the full per-`grammar_name` config-tuple modulo the
  generated-artefact path columns," RED-pre-P3/GREEN-post-collapse framing intact.
- **αE** (`alphaE:237` cross-cutting note 5, the F16 fold-ledger): carries the full-tuple-
  modulo-path projection + profile-distinctness-preservation.

The fix is mechanism-correct (the Lock-14 spine + P3 collapse MECHANISM untouched; only the
projected column set widened), non-architectural, and — critically for OVERFIT-PRUNE — does
NOT erase the per-profile discriminator the 7 css_l4 replicas legitimately carry: P3 collapses
to ONE config **only if** the profiles are genuinely one grammar, preserving distinctness
otherwise. This is the correct anti-overfit posture: the prune (collapse 7 replicas) is gated
on a check that cannot be satisfied by erasing legitimate distinctness. **DISCHARGED.**

### FOLD B — CH5 F.6 (the P1 x86 deletion-list reach: `checkasm_parity.rs` + `byte_class_from_eq_set_64.rs`) — **DISCHARGED IN THE FEEDERS (αC, αE); NOT LANDED IN THE BINDING SYNTHESIS/HANDOFF — RESIDUAL DEFECT, see §2**

Disk ground truth re-verified live at HEAD `318d9c046`:
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`: `grep -cE 'x86_64'` = **11**; the 9
  ACTIVE compile-coupled call sites are real — `:458` `…avx2::classify::classify_block_scalar`,
  `:464` `…avx2::bmi2_emit::compact_mask_scalar`, `:467` `…avx2::prefix_xor::prefix_xor_scalar`,
  `:477` `…avx512_vbmi2::classify::…`, `:478` `…avx512_gfni::classify_affine::…`, `:484`
  `…avx512_bitalg::multiclass::…`, `:493` `…avx512_vbmi2::mask_fuse::…`, `:497`
  `…avx_ifma::mantissa::…`, `:502` `…avx512_vnni::digit_mac::…`, plus the `:673`
  `#[ignore] sk_v3_intrinsic_parity_x86_64` harness. These resolve into `src/x86_64/` and are
  compile-coupled to `pub mod x86_64;` — deleting the module WITHOUT decoupling these breaks
  `cargo test --no-run`. The F.6 finding is disk-accurate.
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`: `:10,:12,:15` carry
  "AVX-512 BW"/"AVX2" doc strings the crate-wide verify grep fires on. Confirmed on disk.

**Feeders — fold fully and correctly landed:**
- **αC** (`alphaC:168-179` deletion-target list; `:186-194` the (4a)/(4b) obligation; `:212-215`
  the build-soundness close gate): names `tests/checkasm_parity.rs` with all 9 line-cited active
  call sites + the `:672` `#[ignore]` harness, "DECOUPLE-OR-DELETE … BREAKS THE BUILD," AND
  `src/scalar/byte_class_from_eq_set_64.rs:10,12,15` "CLEAN to aarch64/scalar-neutral," AND
  "`cargo build` AND `cargo test --no-run` are clean — the `checkasm_parity.rs` decoupling (4a)
  is what keeps the `src/x86_64/` deletion from breaking compilation." Reach-complete.
- **αE** (`alphaE:94` P1 row item `(3)`; `:100-101` scalar-/checkasm-status; `:104` P1 exit gate):
  "**(3) COMPILE-COUPLED removal/decoupling sites the verify grep ALSO fires on (V5 R-2/CH5 F.6
  widening)**" naming both files with the 9 active sites + the build-soundness assertion. Reach-
  complete.

**The prompt's confirming criterion — "P1 x86 deletion list now includes `checkasm_parity.rs`
+ `byte_class_from_eq_set_64.rs`" — is SATISFIED in the feeders (αC + αE), the surfaces the
fold dispatch was scoped to ("widen … in alphaC + alphaE").** But the V5 CONSOLIDATED REVISE-2
fix text and the V5 CH5 F.6 finding BOTH named the *binding* SYNTHESIS P1 row + the
`x86_tree_deleted` telemetry + the HANDOFF P1 receiver as the propagation target ("αC's reach
was NOT carried into the BINDING SYNTHESIS P1 row, the `x86_tree_deleted` telemetry, or the
HANDOFF P1 receiver"). Those binding surfaces remain UN-widened — see §2.

---

## §2 — RESIDUAL DEFECT (CH7 OVERFIT-PRUNE) — **REVISE**

### CH7-R1 — the BINDING P1 x86 deletion list + `x86_tree_deleted` telemetry ship a RED-by-construction prune gate: the (h)/(i) compile-coupled sites land in the feeders but NOT in SYNTHESIS/HANDOFF, the inventory-of-record

This is squarely an OVERFIT-PRUNE defect: P1 is the lands-FIRST prune that deletes the
mis-arch x86 surface; its close-gate is the verify grep `grep -riE 'avx|gfni|sve|x86|nasm'
skinny/crates/bbnf-simd/` (crate-wide, INCLUDING `tests/`). A deletion list narrower than the
grep ships a **RED-by-construction gate** — the exact paper-close hazard ("invites a receiver to
silently narrow the grep back or hand-wave the hits dormant," `SYNTHESIS.md:114-116`) that this
whole fold-lineage exists to close. The defect re-incurs that hazard one reach-level deeper, in
the binding contract.

**The binding surfaces enumerate ONLY (a)-(g) — `tests/checkasm_parity.rs` and
`src/scalar/byte_class_from_eq_set_64.rs` are absent:**
- **SYNTHESIS binding P1 row** (`SYNTHESIS.md:326`): list runs (a) `src/x86_64/` … (g) in-crate
  doc surfaces; close claim "every active hit the grep flags is on the **(a)-(g)** removal list."
  No `tests/checkasm_parity.rs`, no `src/scalar/byte_class_from_eq_set_64.rs`. A receiver executing
  exactly this row deletes `src/x86_64/` + `pub mod x86_64;` and the grep stays RED on the 9
  `checkasm_parity.rs` call sites — AND `cargo test --no-run` fails to compile against the deleted
  `bbnf_simd::x86_64::…` paths. RED-by-construction + build-break, the precise mirror-defect.
- **SYNTHESIS `x86_tree_deleted` telemetry** (`SYNTHESIS.md:576`): enumerates `src/x86_64/` /
  `ext/x86/` / `build.rs` / `lib.rs:247` / `nasm-rs` / `lib.rs:5`+cfg-arms / doc surfaces — the
  (a)-(g) set only.
- **SYNTHESIS V4→V5 fold narrative** (`SYNTHESIS.md:102-124`): the P1-widening enumeration
  terminates at "(g) … scrub the in-crate doc surfaces." It never reaches (h)/(i).
- **HANDOFF P1 receiver** (`HANDOFF.md:101-114`): `src/x86_64/` … "scrub the in-crate doc surfaces
  OR scope the grep to source+manifest" — (a)-(g), terminating identically.
- **HANDOFF `x86_tree_deleted` telemetry** (`HANDOFF.md:336-338`): "`src/x86_64/` AND `ext/x86/`
  AND nasm `build.rs` AND the `nasm-rs` Cargo.toml dep AND `lib.rs` `pub mod x86_64;`/cfg-arms
  gone" — no `tests/`, no `scalar/`.

SYNTHESIS itself declares these the authoritative surface: "The crate-wide-AND-reach-extended
close-gate authored HERE (the P1 row + the `x86_tree_deleted` telemetry) **is the binding
inventory-of-record**; the αA/αE …" (`SYNTHESIS.md:129-130`). The PASS-IMPL receiver executes
the binding P1 row + telemetry, not the αC/αE research feeders. So the fold landing only in the
feeders leaves the operative gate RED-by-construction — the OVERFIT-PRUNE close-gate is NOT
satisfiable-by-construction on the inventory-of-record.

**Why this is the OVERFIT-PRUNE lens's concern (not merely CH5's):** the F.6 lineage is
explicitly the *mirror* of the V3 escape — V3 shipped a `src/`-scoped grep that read GREEN while
x86 survived; V4 widened the grep crate-wide but left the deletion list narrower (RED-by-
construction); V5 found the `tests/`+`scalar/` reach gap. Each iteration is the same overfit-
prune pathology: a prune gate whose deletion obligation does not match its detection reach. The
binding contract still carries that pathology. A green `x86_tree_deleted` is, on the binding
surfaces, either unachievable (the grep cannot reach floor) or achievable only by the very
hand-wave the gate forbids — the paper-close the prune exists to prevent.

**Fix (REVISE, mechanical, αC carries it verbatim — single edit on each of two binding files):**
propagate the αC/αE reach into the binding inventory-of-record. Add to the SYNTHESIS P1 row
(`:326`) + `x86_tree_deleted` telemetry (`:576`), and the HANDOFF P1 receiver (`:101-114`) +
telemetry (`:336`), two removal/decoupling targets — **(h)** `tests/checkasm_parity.rs:454-502`:
DECOUPLE-OR-DELETE the x86_64 reference block (9 active `bbnf_simd::x86_64::…::*_scalar(…)` call
sites + the `:673` `#[ignore]` harness) so `src/x86_64/` deletion stays build-sound; **(i)**
`src/scalar/byte_class_from_eq_set_64.rs:10-15`: scrub the AVX-512 BW/AVX2 doc strings aarch64-
neutral. Restate the close claim "every active hit on the **(a)-(i)** removal list" and fold
`cargo test --no-run` clean into the P1 build-soundness gate (αC `:212-215` is the verbatim
source). This is the SAME convergence-cheap, mechanism-correct, non-architectural propagation
the feeders already received — it does not re-litigate the prune obligation, it completes its
reach on the surface a receiver actually executes.

---

## §3 — CH7 overfit-prune health (the surfaces that ARE clean — ACCEPT)

The substantive OVERFIT-PRUNE posture survives intact; this is a propagation gap, not an
architecture defect. Confirmed clean:

1. **Shortlist is additive-by-deletion, no overfit resurrection** — A (PRUNE P1–P5) → B1–B4;
   no candidate added/removed across V1–V5; net LOC ≈ −12650…−12850; no re-opened REDRESS
   pre-block (AZ-IV eager, StructRegistry per-leaf, fact-stream-output, 24-broadcast,
   FNV-runtime, x86/AVX/SVE all stay pre-blocked). The prune deletes far more than it adds.
2. **P2/P3/P4/P5 prune gates clean** — P2 deletes the warm contrived `nonjson_css_l4.rs`
   `measure_mbps`/SHA-fixture path while KEEPING the honest `css_canon_bench`/`w2_rich_cssom_bench`
   + 9-field `assert_rich_strict_equality` oracle (`SYNTHESIS.md:376`, αE `:235`); P3 collapses
   the 7 replicas via the now-correct profile-preserving config-tuple check (FOLD A); P4 makes
   the Lock-14 gate meaningful (drops `diagnostic-x86`, extends `GENERIC_SCAN_ROOTS`); P5 purges
   the `parse_w11_1_number` metalang leak. None over-prunes a kept-honest artefact.
3. **The kept-honest inventory is explicit and not pruned** (αE `:235`): `css_canon_bench` +
   `w2_rich_cssom_bench`, the 9-field oracle, the 12 checkasm single-kernel differentials +
   `checkasm_common.rs` + `checkasm_parity.rs` hardening, the substrate (Lock 1). The checkasm
   `parity.rs` is correctly KEPT-and-DECOUPLED (only its x86_64 block removed, aarch64 parity
   assertions retained) — the prune does not throw out the aarch64 hardening with the x86 bathwater.
4. **The F13+F16 relocated-overfit-seam correction (αE `:237`)** is the load-bearing anti-overfit
   machine-check and is now reach-correct end-to-end (FOLD A): the structural collapse check cannot
   be satisfied by erasing legitimate profile-distinctness, and the arm-census grep is correctly
   scoped necessary-not-sufficient.
5. **checkasm count disk-true** (12 single-kernel + `checkasm_common.rs` + `checkasm_parity.rs`
   = 14; `ls checkasm_*.rs | wc -l` = 14) — the stale "18" cannot re-seed an un-satisfiable
   "18-present" gate; αE is the count-correct reference (F4/F14).

---

## §4 — Per-disposition tally

| # | Disposition | Verdict |
|---|---|---|
| 1 | FOLD A (F16 enumeration) discharged across SYNTHESIS+HANDOFF+αE — `profile`/`source_inputs`/`metadata_inputs` enumerated by exclusion, profile-distinctness preserved | ACCEPT |
| 2 | FOLD B (F.6 reach) discharged in the αC/αE FEEDERS — both files line-cited, decouple-or-delete + build-soundness gate | ACCEPT |
| 3 | Shortlist additive-by-deletion; no overfit resurrection; no re-opened pre-block | ACCEPT |
| 4 | P2/P3/P5 prune gates clean; kept-honest artefacts not over-pruned | ACCEPT |
| 5 | F13+F16 relocated-seam machine-check reach-correct (cannot erase distinctness) | ACCEPT |
| 6 | checkasm = 14 disk-true; no un-satisfiable "18-present" gate | ACCEPT |
| 7 | **CH7-R1: binding SYNTHESIS P1 row (`:326`) + `x86_tree_deleted` (`:576`) + HANDOFF P1 (`:101-114`) + telemetry (`:336`) enumerate ONLY (a)-(g) — `tests/checkasm_parity.rs` (9 active compile-coupled sites) + `src/scalar/byte_class_from_eq_set_64.rs` absent from the inventory-of-record → RED-by-construction prune gate, the OVERFIT-PRUNE mirror-defect carried one reach-level deeper into the binding contract** | **REVISE** |

**Verdict:** FOLD A (F16) is **fully discharged** on every binding surface — the §3Z-blocking
CH2 orphan is closed. FOLD B (F.6) is **discharged in the feeders** (αC + αE, the dispatch's
scoped surfaces) but the V5 CONSOLIDATED REVISE-2 fix + the CH5 F.6 finding explicitly required
the *binding* SYNTHESIS/HANDOFF inventory-of-record, which a PASS-IMPL receiver executes — and
that surface still ships the (a)-(g)-only, RED-by-construction P1 gate. The defect is a genuine
residual: single-edit, mechanism-correct, non-architectural, αC carries the verbatim fix; it is
not a re-open, not a stranded >SOTA, not a re-litigation of the converged goalset. One REVISE.

TALLY accept=6 revise=1 reject=0
