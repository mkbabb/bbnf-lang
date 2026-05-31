# CH1 — CORRECTNESS (cycle V6-confirm) — SK-V18 Pass-Alpha CHALLENGE

Lens: **CH1 Correctness** per `PASS-ALPHA.md §3` ("does every claim cite RESULTS.md
row, REDRESS entry, commit SHA, or measurement file? Are falsifiability gates
measurable? Are competitor deltas computed against the correct strictness plane?") +
ORCHESTRATOR §3W/§3Z. Subject: the SK-V18 Pass-Alpha artefacts
`research/alpha/{alphaA..E}.md` + the binding `sk-v18/SYNTHESIS.md` + `sk-v18/HANDOFF.md`
(the α-F deliverable per `PASS-ALPHA.md:27`/`:160-161`/§2 output mapping — SYNTHESIS.md +
HANDOFF.md live at the **tranche root**, not under `research/alpha/`; contract-compliant).

**This is a CONFIRMING cycle (V6-confirm).** Posture: ACCEPT where the artefact is
correct+complete; verify the two orphan REVISEs the V5 CONSOLIDATED carried (CH2 §8.1 / F16
field-enumeration; CH5 F.6 / P1 x86-deletion-list widening) are DISCHARGED; flag only
genuine residual defects. Not a re-litigation of the converged goalset. The V5 CH1 axis
itself was already 7 ACCEPT / 0 REVISE / 0 REJECT (100%); this pass re-verifies every
load-bearing path:line/SHA/count live at HEAD `318d9c046` AND audits whether the two folds
actually landed in the artefacts they were prescribed to touch.

## Disk re-verification — load-bearing CH1 facts re-confirmed at HEAD `318d9c046`

| Claim | Command | Result | Status |
|---|---|---|---|
| bracket HEAD | `git log --oneline -1` | `318d9c046 docs(sk-v18-handoff)…` | ✓ |
| `RuntimeTarget` struct fields | `awk '/struct RuntimeTarget/,/^}/' regen.rs` | **12 fields** (`grammar_name, profile, entry_rule, source_roots, output_dir, check_command, source_inputs, metadata_inputs, emitter, expected_files, frontend_requirements, output_labels`) — **no `fact_schema`/`row_id`/`output_plane` struct field** | ✓ |
| F16: `profile` distinct (per-profile discriminator) | `grep -oE 'profile: "css_l4[a-z_]*"' regen_css.rs \| sort -u \| wc -l` | **7 DISTINCT** | ✓ |
| F16: `fact_schema` distinct | `grep -oE 'css-l4-[a-z-]*-facts-v1' regen_css.rs \| sort -u \| wc -l` | **7 DISTINCT** | ✓ |
| F.6: `checkasm_parity.rs` x86 tokens | `grep -cE 'x86_64' tests/checkasm_parity.rs` | **11** (9 ACTIVE `bbnf_simd::x86_64::…::*_scalar(…)` call sites `:458,:464,:467,:477,:478,:484,:493,:497,:502`) | ✓ |
| F.6: `byte_class_from_eq_set_64.rs` x86 refs | `grep -nE 'AVX\|x86' src/scalar/byte_class_from_eq_set_64.rs` | `:10,:12,:15` "AVX-512 BW"/"AVX2" doc strings | ✓ |
| F.6: binding verify grep fires on both | `grep -rilE --include='*.rs' 'avx\|gfni\|sve\|x86\|nasm' tests/ src/scalar/byte_class_from_eq_set_64.rs` | **`checkasm_parity.rs` AND `byte_class_from_eq_set_64.rs`** both fire | ✓ |

The JSON/CSS >SOTA-plane facts, checkasm 14, x86 ext/3554-src/847-build/102 surface,
replica md5=1, sonic-strict skipper, and W5 ledger N=200 medians were exhaustively
re-verified in V5 CH1 (`V5/CH1.md` disk table) and are unchanged at the same HEAD — not
re-tabulated here. The two new live facts above are what the V6 folds turn on.

---

## §1 — CONFIRM the CH2 §8.1 / F16 field-enumeration fold — **DISCHARGED**

The V5 REVISE-1 fix required: make the operative machine-check equal its prose by
**enumerate-by-EXCLUSION** of the two path columns, **naming `profile` explicitly** (the
7-distinct per-profile discriminator) plus `source_inputs`/`metadata_inputs`, applied to
**αF SYNTHESIS.md** (and αE if it carries the enumeration).

**Verified landed in the binding contract AND both carrying feeders:**

- **SYNTHESIS.md** — the operative enumeration now reads, at EVERY restatement:
  - `:156` — `…/`**`profile`**`/entry_rule/source_roots/check_command/`**`source_inputs`**`/`**`metadata_inputs`**`/emitter/frontend_requirements/output_labels` with explicit prose "the prior enumeration `fact_schema`/`row_id`/`output_plane` … OMITTED `profile` itself … plus `source_inputs`/`metadata_inputs`".
  - `:333` (G3 verify (iii)), `:410-411` (W5C profile-array pre-block), `:566` (`runtime_target_rows_collapsed` telemetry), `:608-609` (gate-json reject column) — all carry the by-exclusion form naming `profile`/`source_inputs`/`metadata_inputs`.
  - Critically, every site correctly disambiguates `fact_schema`/`row_id`/`output_plane` as **per-profile content the `profile` discriminator selects, NOT struct fields** — which disk confirms (the 12-field struct has none of those three as a field). The enumeration is now COMPLETE: the 9 non-path operative fields + `grammar_name` (the key) + the 2 excluded path columns = the 12 live fields. No field can silently fall outside the tuple.
- **αE** `:19` (F16 ledger row), `:105` (P3 exit), `:156` (B2 distinct-grammar gate), `:207` (B4 litmus) — all enumerate `profile`/`source_inputs`/`metadata_inputs`/…/`output_labels` and state "the prior list OMITTED `profile` … plus `source_inputs`/`metadata_inputs`".
- **αC** `:280`, `:292`, `:332` — carry the corrected projection.
- **P3-preserves-profile-distinctness obligation present** (SYNTHESIS `:165-167`; αE `:19`,`:105`,`:156`): the collapse erases `profile` only where the 7 profiles are genuinely one grammar; distinct grammars differentiate by distinct `.bbnf` roots, never by erasing the discriminator. This is the exact P3 concern REVISE-1 named.
- **Gate is correctly RED today** (7 distinct `profile` + 7 distinct `fact_schema` on disk), GREEN only post-P3 — measurable against the actual close condition.

The F16 enumeration drift is closed; the operative machine-check now equals the prose. **DISCHARGED — ACCEPT.**

**Sub-disposition note (cosmetic, NOT a REVISE) — `13-field` vs 12-field count-label
drift.** The live `RuntimeTarget` struct has **12 fields** (`regen.rs:6-19`, disk-counted).
The feeders are correct (αC `:63`,`:274`,`:702` say "12 fields"); the binding SYNTHESIS
(`:142` "~13", `:153`/`:333`/`:410`/`:566`/`:608` "13-field") and αE (`:19`,`:105`,`:156`,
`:207` "13-field") carry a **13-field** label. This is a count-LABEL imprecision only — the
*enumeration itself* is complete and correct (9 non-path operative fields are named
explicitly; `grammar_name` is the partition key; `output_dir`/`expected_files` are the
2 excluded path columns; 9+1+2 = 12), and the machine-check `count(distinct
config-tuple-minus-(output_dir,expected_files)) == 1` is right regardless of the count
label. The gate is measurable + correct; the "13" is a stale arithmetic carry-over from the
V4 "~13 fields" approximation that the V5 by-exclusion fold superseded but did not re-count.
Weighed cosmetic per ORCHESTRATOR §3Z (does not seed a wrong gate, does not omit a field);
a one-token tightening (`13-field` → `12-field`) aligns the binding contract with its own
disk-correct enumeration and the αC feeder. Flagged for a deft V6 wording tightening;
dispose ACCEPT.

---

## §2 — CONFIRM the CH5 F.6 / P1 x86-deletion-list widening — **NOT FULLY DISCHARGED (residual defect)**

The V5 REVISE-2 fix required: add to the **binding P1 list + `x86_tree_deleted` telemetry**
two removal targets — **(h)** `tests/checkasm_parity.rs` (the 9 active compile-coupled x86
call sites re-homed/deleted — closes BOTH the compile-coupling AND the grep RED), **(i)**
`src/scalar/byte_class_from_eq_set_64.rs` (doc x86 cross-refs scrubbed aarch64-neutral). The
V5 CONSOLIDATED §4 fix text is explicit: "add to the **binding P1 list** + `x86_tree_deleted`
telemetry"; SYNTHESIS.md `:130` itself declares "the P1 row + the `x86_tree_deleted`
telemetry … is the binding inventory-of-record; the αA/αE [feeders] mirror it."

**Landed in the feeders — but NOT in the binding inventory-of-record:**

- **Feeders ACCEPT (fold present):**
  - αC `:168-215` — names `tests/checkasm_parity.rs` as "**COMPILE-COUPLED, NOT doc/test-only (V5 R-2/F.6)**", 11 tokens / 9 active call sites, "DECOUPLE-OR-DELETE", AND `src/scalar/byte_class_from_eq_set_64.rs:10,:12,:15` "re-word aarch64/scalar-neutral", AND the build-soundness rationale (`:215` "the `checkasm_parity.rs` decoupling … is what keeps the `src/x86_64/` deletion from breaking compilation").
  - αE `:94` (P1 row item **(3)**: "COMPILE-COUPLED removal/decoupling sites the verify grep ALSO fires on (V5 R-2/CH5 F.6 widening)" — names both, with the 9 call-site line numbers), `:101` ("`tests/checkasm_parity.rs` is DECOUPLED, not deleted"), `:104` (P1 exit "`cargo build` AND `cargo test --no-run` clean").
- **Binding contract STOPS at the V4-era (a)-(g) enumeration — the two F.6 sites are absent:**
  - **SYNTHESIS.md `:326`** (the binding P1 row) enumerates exactly **(a)-(g)** — `src/x86_64/`, `ext/x86/`, `build.rs`, `lib.rs:247`, `Cargo.toml:19` nasm, `lib.rs:5`+`:285-288`, in-crate docs — and terminates with the assertion *"every active hit the grep flags is on the **(a)-(g)** removal list."* **This assertion is FALSE on disk:** `grep -rilE --include='*.rs' 'avx|gfni|sve|x86|nasm' bbnf-simd/` fires on `tests/checkasm_parity.rs` AND `src/scalar/byte_class_from_eq_set_64.rs`, **neither of which is in (a)-(g)**.
  - **SYNTHESIS.md `:576`** (`x86_tree_deleted` telemetry, the binding machine-checkable column the `gate-json` consumer rejects on) — same (a)-(g)-bounded definition, "deletion list reach-matched to the verify grep"; does NOT name `checkasm_parity.rs` or `byte_class_from_eq_set_64.rs`.
  - **SYNTHESIS.md `:376`** (PRUNE capsule) and **`:474`** (close-condition table) — both stop at the V4 enumeration.
  - **HANDOFF.md `:17`** describes "P1 + `x86_tree_deleted` are EXTENDED so the deletion list is reach-matched" but the EXTENSION it narrates (`:14-16`) is the **V4** widening (`nasm-rs` dep, `lib.rs:5`, `:285-288`, doc surfaces) — it does NOT mention the V5 F.6 `tests/`+`scalar/` sites. **HANDOFF.md `:336`** (`x86_tree_deleted` definition) likewise stops at `src/x86_64/`+`ext/x86/`+nasm `build.rs`+Cargo.toml dep+`lib.rs` cfg-arms — `checkasm_parity.rs`/`byte_class_from_eq_set_64.rs` absent.

**Why this is a genuine residual defect, not cosmetic.** This is the *identical*
propagation-inversion the V5 CONSOLIDATED named for REVISE-2: the αC research feeder is
reach-complete, but the BINDING SYNTHESIS P1 row + the `x86_tree_deleted` telemetry are not.
The fold corrected the feeders and left the inventory-of-record narrower than its own verify
grep — the very condition (`a deletion list narrower than the grep ships a RED-by-construction
gate`) SYNTHESIS `:326` claims to have fixed. Two concrete consequences on disk:

1. **Build-soundness (the load-bearing one):** executing exactly the binding (a)-(g) deletes
   `pub mod x86_64;` while `tests/checkasm_parity.rs:458-502` still resolves 9 active
   `bbnf_simd::x86_64::avx2::…`/`avx512_…::*_scalar(…)` paths → `cargo test --no-run` FAILS
   to compile. The binding contract's P1 "`bbnf-simd` builds aarch64-only" verify omits the
   decoupling step that makes it true. The redress agent executing the BINDING P1 (not the
   feeder) ships a broken build.
2. **RED-by-construction gate:** `x86_tree_deleted == true` keys on the crate-wide grep,
   which stays RED on `checkasm_parity.rs`'s 11 hits + `byte_class_from_eq_set_64.rs`'s 3
   hits after a faithful (a)-(g) execution — the gate is un-satisfiable as the binding
   contract states it, the exact P4-class false gate this cycle is meant to eliminate.

**Disposition: REVISE (mechanism-correct, single-edit, αC + αE carry the verbatim fix).**
The fix is purely mechanical propagation — fold the αE P1-item-(3) text and αC `:168-215`
into SYNTHESIS.md `:326` (add **(h)** `tests/checkasm_parity.rs:458-502` decouple-or-delete +
**(i)** `src/scalar/byte_class_from_eq_set_64.rs:10-15` scrub) and into the `x86_tree_deleted`
definitions (SYNTHESIS `:576`, `:376`, `:474`; HANDOFF `:17`, `:336`), and re-point the P1
verify-grep terminal assertion from "(a)-(g)" to "(a)-(i)" / "5 firing files, 5 named removal
targets". No architecture, no re-open, no number reversal — the binding inventory-of-record
is brought up to the feeders' already-correct reach. Until that lands, the binding SYNTHESIS
P1 row + `x86_tree_deleted` telemetry + the HANDOFF P1 receiver remain incorrect against
disk (a false reach-match assertion + a RED-by-construction gate).

---

## §3 — αA / αB / αC / αD / αE re-confirmation (the carried ACCEPTs hold)

- **αA Results Extraction — ACCEPT.** JSON >sonic-strict 51-row table reproduces RESULTS to
  the decimal (re-verified V5); x86 crate-wide census carries both surfaces; checkasm 12+2;
  H1 lazy-vs-eager caveat present. No new defect.
- **αB Competitor Deltas — ACCEPT.** Strict-vs-strict plane (sonic strict skipper), CSS
  asymmetry disclosed, correctly self-excludes from the x86 REVISE. No new defect.
- **αC REDRESS Digest — ACCEPT (and F.6-reach-complete).** FOLD-1 x86 crate-wide; FOLD-3
  (F16) relocated-seam projection corrected to 12-field-correct enumeration (`:274` "12
  fields"); P1 `:168-215` carries the F.6 `checkasm_parity.rs`+`byte_class_from_eq_set_64.rs`
  decoupling verbatim with the build-soundness rationale. αC is AHEAD of the binding contract
  on the F.6 axis — the defect is propagation INTO SYNTHESIS/HANDOFF, not in αC.
- **αD Validated/Invalidated — ACCEPT.** checkasm 14; phantom-G two-axis precise; x86 I7/S1
  concorded. No new defect.
- **αE Candidate Shortlist — ACCEPT.** F15 (x86 crate-wide), F16 (12-field by-exclusion
  enumeration naming `profile`/`source_inputs`/`metadata_inputs`), and F.6 (P1 item (3)
  compile-coupled `checkasm_parity.rs`+`byte_class_from_eq_set_64.rs`) all present and
  disk-true. αE carries BOTH V5 folds correctly. The lone αE residual is the cosmetic
  `13-field` count-label (§1 sub-note), which does not seed a wrong gate.

## §SYNTHESIS (αF) — **REVISE** (F16 discharged; F.6 NOT propagated into the binding P1 row + `x86_tree_deleted` telemetry)

The F16 enumeration fold landed cleanly (`:156`,`:333`,`:410`,`:566`,`:608` — `profile`/
`source_inputs`/`metadata_inputs` named by exclusion, gate correctly RED). But the binding P1
row (`:326`) + `x86_tree_deleted` telemetry (`:576`) + PRUNE capsule (`:376`) + close-table
(`:474`) STOP at the V4 (a)-(g) enumeration; the F.6 compile-coupled sites
(`tests/checkasm_parity.rs`, `src/scalar/byte_class_from_eq_set_64.rs`) — which the binding
verify grep demonstrably fires on (disk-confirmed) — are absent. The P1 row's "every active
grep hit is on the (a)-(g) removal list" is false on disk; the `x86_tree_deleted` gate is
RED-by-construction as stated. Single-edit mechanical propagation from αC/αE. REVISE.

## §HANDOFF (αF) — **REVISE** (consistent with SYNTHESIS; same F.6 omission in the `x86_tree_deleted` receiver)

The F16 widening carries (`:21-25`). The x86 P1 receiver (`:12-19`) narrates only the V4
reach-extension; the `x86_tree_deleted` definition (`:336`) stops at the (a)-(g)-class
enumeration — `checkasm_parity.rs`/`byte_class_from_eq_set_64.rs` absent from the binding
HANDOFF inventory. Same single-edit propagation fix. REVISE.

---

## Disposition summary

| Artefact | Disposition | Basis |
|---|---|---|
| αA Results Extraction | **ACCEPT** | carried ACCEPT re-confirmed; no new defect |
| αB Competitor Deltas | **ACCEPT** | strict-vs-strict plane; self-excludes from x86 REVISE |
| αC REDRESS Digest | **ACCEPT** | F16 12-field enumeration + F.6 `checkasm_parity.rs`/`byte_class_from_eq_set_64.rs` decoupling both carried verbatim; AHEAD of the binding contract |
| αD Validated/Invalidated | **ACCEPT** | checkasm 14; phantom-G precise; x86 concorded |
| αE Candidate Shortlist | **ACCEPT** | F15+F16+F.6 all present, disk-true; lone residual is cosmetic `13-field` count-label (gate correct) |
| SYNTHESIS (αF) | **REVISE** | F16 discharged; **F.6 NOT propagated** — binding P1 row `:326` + `x86_tree_deleted` `:576` stop at (a)-(g), "every grep hit on (a)-(g)" FALSE on disk (`checkasm_parity.rs`+`byte_class_from_eq_set_64.rs` fire off-list → RED-by-construction + build-break) |
| HANDOFF (αF) | **REVISE** | same F.6 omission in `x86_tree_deleted` receiver `:336` + P1 narration `:12-19` |

**Discharge verdict on the two orphan REVISEs:**

- **CH2 §8.1 / F16 (field-enumeration) — DISCHARGED.** The operative enumeration now equals
  the prose by exclusion; `profile`/`source_inputs`/`metadata_inputs` named at every binding
  + feeder site; P3-preserves-profile-distinctness obligation present; gate correctly RED.
  (Residual: cosmetic `13-field` → `12-field` count-label, sub-REVISE.)
- **CH5 F.6 (P1 x86-deletion-list widening) — NOT FULLY DISCHARGED.** The fold reached the
  αC/αE feeders but did NOT propagate into the BINDING `SYNTHESIS.md` P1 row + `x86_tree_deleted`
  telemetry or the `HANDOFF.md` receiver — exactly the binding-inventory-of-record the V5
  fix text and SYNTHESIS `:130` name as authoritative. The binding P1 verify assertion is
  false on disk and the `x86_tree_deleted` gate is RED-by-construction; executing the binding
  (a)-(g) breaks the build (`checkasm_parity.rs` 9 active `bbnf_simd::x86_64::…` call sites).
  Single-edit, mechanism-correct, αC+αE carry the verbatim fix.

No architectural re-open, no stranded >SOTA, no wrong-plane comparator, no REJECT. The single
residual is the F.6 binding-propagation REVISE (2 sites: SYNTHESIS, HANDOFF) — the same
defect-class the orphan named, surviving one propagation level into the binding contract.

TALLY accept=5 revise=2 reject=0
