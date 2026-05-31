# SK-V18 Pass-Alpha CHALLENGE — V6-confirm · Lens CH6 ANTI-PAPER-CLOSE

CONFIRMING re-review of the SK-V18 Pass-Alpha artefacts at HEAD `318d9c046` AFTER the two
orphan REVISE folds the V5 CONSOLIDATED left open (CH2 §8.1 / F16 field-enumeration ·
CH5 F.6 / P1 x86-deletion-list widening). Artefacts re-read disk-live:
`SYNTHESIS.md`, `HANDOFF.md` (tranche root), `research/alpha/{alphaC,alphaE}.md`, the
V5 `CONSOLIDATED.md`, against the live `skinny/crates/bbnf-simd/` + `skinny/xtask/src/regen.rs`.

This is a CONFIRMING cycle: ACCEPT where the artefact is correct+complete; verify the two
folds discharged; flag only genuine residual defects. The lens posture is ANTI-PAPER-CLOSE —
a fold that lands in the research feeders but NOT in the BINDING inventory-of-record is a
paper-close hazard, because S-P3 and the `--skv18-generalization-report` gate consumer read
the binding SYNTHESIS P1 gate + telemetry columns + the HANDOFF invariants, NOT the feeders.

---

## §1 — Disk ground truth re-established (live at HEAD `318d9c046`)

**`RuntimeTarget` struct** (`skinny/xtask/src/regen.rs:6`): **12 fields** —
`grammar_name`, `profile`, `entry_rule`, `source_roots`, `output_dir`, `check_command`,
`source_inputs`, `metadata_inputs`, `emitter`, `expected_files`, `frontend_requirements`,
`output_labels`. (NB: the contract repeatedly labels this "13-field"; the disk truth is 12.
Pre-existing minor count nit present in feeders AND binding alike; non-load-bearing because
the by-exclusion statement is authoritative and the explicit list is its enumeration — but
it should be corrected to 12 wherever restated. Sub-REVISE, not blocking.)
Excluding the two path columns (`output_dir`, `expected_files`) leaves **10 non-path
fields**: `grammar_name`/`profile`/`entry_rule`/`source_roots`/`check_command`/`source_inputs`/
`metadata_inputs`/`emitter`/`frontend_requirements`/`output_labels`.

**x86 crate-wide verify grep** (the binding command,
`grep -rilE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`):
fires on **28 files**. Beyond the (a)-(g) covered set (`src/x86_64/*` 24 files, `Cargo.toml`,
`build.rs`, `src/lib.rs`), it ALSO fires on:
- `tests/checkasm_parity.rs` — **11 `x86_64` tokens**; 9 ACTIVE compile-coupled
  `bbnf_simd::x86_64::…::*_scalar(…)` call sites (`:458,:464,:467,:477,:478,:484,:493,:497,:502`)
  + the `:673` `#[ignore]` `sk_v3_intrinsic_parity_x86_64` harness + the `:454` comment. These
  resolve INTO `src/x86_64/` — deleting `src/x86_64/` WITHOUT decoupling this file breaks
  `cargo test --no-run`. Compile-coupled, exactly as REVISE-2 named.
- `src/scalar/byte_class_from_eq_set_64.rs` — `:10,:12,:15` x86 doc cross-refs
  ("AVX-512 BW", "AVX2"). The SCALAR sibling of the (a)-deleted `src/x86_64/byte_class_from_eq_set_64.rs`;
  NOT covered by (a)-(g).

Both disk facts confirm the V5 REVISE-2 refutation verbatim.

---

## §2 — Fold 1 (CH2 §8.1 / F16, REVISE-1): field-enumeration — VERIFICATION

V5 REVISE-1 required: the operative `runtime_target_rows_collapsed` enumeration must equal its
prose — enumerate-by-EXCLUSION of the two path columns; name `profile` explicitly at every
restatement; include `source_inputs`/`metadata_inputs`. Fix-scope stated by V5: "Apply to the
αF SYNTHESIS.md (and alphaE if it carries the enumeration)."

**DISCHARGED in the binding SYNTHESIS.md + both feeders — ACCEPT:**
- `SYNTHESIS.md:156` (V4→V5 fold narrative): operative set restated as
  `grammar_name`/**`profile`**/`entry_rule`/`source_roots`/`check_command`/**`source_inputs`**/
  **`metadata_inputs`**/`emitter`/`frontend_requirements`/`output_labels`, by-exclusion, with the
  explicit "OMITTED `profile` itself … plus `source_inputs`/`metadata_inputs`" correction.
- `SYNTHESIS.md:333` (G3 close-condition (iii)), `:411` (§0.4 pre-block), `:566` (telemetry
  column): all enumerate `profile`/`source_inputs`/`metadata_inputs`, all use
  `minus-(output_dir,expected_files)` (both path columns), all cite CH2 V4/V5 §8.1.
- `alphaE-candidate-shortlist.md:19` (F16 entry), `:105` (P3 exit), `:156`, `:207`: carry the
  by-exclusion operative set naming `profile`/`source_inputs`/`metadata_inputs`, tagged
  `[FOLD F13+F16 / V5 R-2/CH2 V4 §8.1; field set widened per F16/CH2 V5 §8.1]`, with the explicit
  "prior list OMITTED `profile`" annotation and the P3-preserve-profile-distinctness clause.
- `alphaC-redress-digest.md:64-65` enumerates the full 12-field struct; the §8.1 prose names
  the per-profile divergence on the columns the discriminator selects.

The mechanism (P3 structural collapse) is unchanged; the projection now matches the prose; the
P3-collapse-preserves-profile-distinctness obligation is carried. The F16 fix is mechanism-correct,
single-edit, non-architectural — folded precisely.

**RESIDUAL DEFECT — F16 NOT carried into HANDOFF.md (orphan-propagation, REVISE):**
`HANDOFF.md` is the binding handoff: its inviolable-invariant §5 (iv) AND its
`--skv18-generalization-report` telemetry schema are read by S-P3 and the gate consumer. Both
still carry the **V4 too-narrow form** the F16 REVISE condemned:
- `HANDOFF.md:276-280` (invariant 5, surface (iv)): `count(distinct config-tuple-minus-output_dir)
  == 1 … over fact_schema/row_id/output_plane/emitter/entry_rule/source_roots/check_command/
  frontend_requirements` — the EXACT defective enumeration: it OMITS `profile`, `source_inputs`,
  `metadata_inputs`; uses `minus-output_dir` (single path column, not `minus-(output_dir,
  expected_files)`); cites only "CH2 V4 §8.1" (pre-F16).
- `HANDOFF.md:331-333` (telemetry `runtime_target_rows_collapsed`): "the 5 per-profile columns
  `fact_schema`/`output_plane`/`emitter`/`row_id`/`entry_rule`, NOT a `(source_roots,entry_rule)`-only
  projection, CH2 V4 §8.1 / V3 §8.1" — again no `profile`/`source_inputs`/`metadata_inputs`.
- `grep -nE 'profile|source_inputs|metadata_inputs' HANDOFF.md` over the collapse enumeration:
  ZERO hits naming them as operative columns (the only `profile` hits are the retired
  `W5C_REQUEST_FACT_PROFILES` array and the "per-profile columns" content phrasing).

This is the SAME class of orphan the V4→V5 fold itself had to chase: a fold landed in
SYNTHESIS + the feeders but a co-binding document (here HANDOFF, which the gate consumer reads)
retained the pre-fold form. The `runtime_target_rows_collapsed` telemetry column the gate-json
consumer REJECTs on therefore ships with a projection that OMITS the `profile` discriminator —
an un-forked emitter dispatching on `target.profile` sails through the HANDOFF-specified check
exactly as REVISE-1 warned. **Single-edit fix:** restate HANDOFF.md:276-280 + :331-333 to the
by-exclusion operative set (`profile`/`source_inputs`/`metadata_inputs`/`emitter`/`entry_rule`/
`source_roots`/`check_command`/`frontend_requirements`/`output_labels`, `minus-(output_dir,
expected_files)`), citing CH2 V5 §8.1 / F16, mirroring SYNTHESIS.md:566 verbatim. Mechanism-correct,
non-architectural; does NOT re-litigate the goalset.

---

## §3 — Fold 2 (CH5 F.6, REVISE-2): P1 x86 deletion-list widening — VERIFICATION

V5 REVISE-2 required: widen the P1 deletion/decoupling list to include **(h)**
`tests/checkasm_parity.rs` (compile-coupled, decouple-or-delete the x86 block) and **(i)**
`src/scalar/byte_class_from_eq_set_64.rs` (scrub x86 doc-refs), so the list is reach-matched to
the crate-wide grep and the `src/x86_64/` deletion is BUILD-SOUND. The V5 fix text scoped this to
"the binding P1 list + `x86_tree_deleted` telemetry" AND noted "αC carries the verbatim fix."

**DISCHARGED in both feeders — ACCEPT:**
- `alphaE-candidate-shortlist.md:94` (the P1 backlog row) now carries item **(3)**:
  "`tests/checkasm_parity.rs` carries 11 `x86_64` tokens, 9 ACTIVE compile-coupled
  `bbnf_simd::x86_64::…::*_scalar(…)` call sites (`:458,…,:502`) … deleting `src/x86_64/` WITHOUT
  decoupling these BREAKS THE BUILD; DECOUPLE-OR-DELETE … and `src/scalar/byte_class_from_eq_set_64.rs`
  … CLEAN to aarch64/scalar-neutral", tagged `[V5 R-2/CH5 F.6 widening]`.
- `alphaE:101` (checkasm status): "`tests/checkasm_parity.rs` is DECOUPLED, not deleted — only its
  x86_64 reference block … is removed so the test crate compiles after `src/x86_64/` deletion; the
  aarch64 parity assertions are retained."
- `alphaE:104` (P1 exit): adds "**`cargo build` AND `cargo test --no-run` clean — the
  `checkasm_parity.rs` decoupling (3) is what keeps the `src/x86_64/` deletion build-sound**."
- `alphaC-redress-digest.md:168-176,188-193,212-215` carries the verbatim removal/decoupling
  detail + the build-soundness chain ("`checkasm_parity.rs` decoupling (4a) is what keeps the
  `src/x86_64/` deletion from breaking compilation").

The feeders are now genuinely reach-matched (every active grep hit named) and build-soundness is
explicit. The fix is mechanical, mechanism-correct, non-architectural — folded precisely. αC
carries it verbatim as the V5 fix promised.

**RESIDUAL DEFECT — CH5 F.6 NOT carried into the BINDING SYNTHESIS P1 gate + `x86_tree_deleted`
telemetry (REVISE):** The V5 REVISE-2 fix text explicitly directs the widening into "the binding
P1 list + `x86_tree_deleted` telemetry" — but the BINDING inventory-of-record retains the
(a)-(g) list and STILL ASSERTS, falsely, that it covers the grep's reach:
- `SYNTHESIS.md:326` (PRUNE P1 close-condition, the binding gate): enumerates (a)-(g), stops at
  (g), and closes with "every active hit the grep flags is on the **(a)-(g)** removal list." This
  is empirically FALSE at HEAD — `tests/checkasm_parity.rs` (9 active hits) and
  `src/scalar/byte_class_from_eq_set_64.rs` (3 hits) fire and are NOT on (a)-(g). The gate is
  RED-by-construction — the EXACT mirror-defect REVISE-2 (and CH6 V4 §1 before it) exists to
  close, re-incurred one reach level deeper. The compile-coupling (deleting `src/x86_64/` without
  decoupling `checkasm_parity.rs` breaks the build) is UNNAMED in the binding gate.
- `SYNTHESIS.md:576` (`x86_tree_deleted` telemetry column): enumerates the same (a)-(g)-bounded set
  and re-asserts "the deletion list is reach-matched to the verify grep so the gate is
  satisfiable-by-construction" — false for the same reason; `grep` over SYNTHESIS shows NO
  `tests/`/`checkasm_parity`/`scalar/byte_class` token in the P1 gate or the telemetry column.
- `HANDOFF.md:101-112` (P1 receiver) + `:336-338` (`x86_tree_deleted` telemetry): likewise
  (a)-(g)-bounded, no `tests/checkasm_parity.rs`/`src/scalar/byte_class_from_eq_set_64.rs`; the
  HANDOFF P1 row carries NO mention of either site.

A receiver executing the binding SYNTHESIS P1 gate exactly (a)-(g) deletes `src/x86_64/`, leaves
`tests/checkasm_parity.rs` referencing the deleted module → `cargo test --no-run` fails AND the
verify grep stays RED on 12 hits (9+3) → invites the receiver to silently narrow the grep or
hand-wave "tests dormant" — a paper-close hazard on the mandatory lands-FIRST PRUNE gate, which
is precisely what this lens exists to catch. **Single-edit fix:** add to `SYNTHESIS.md:326` +
`:576` and `HANDOFF.md:101-112` + `:336-338` two reach-matched targets — **(h)**
`tests/checkasm_parity.rs:454-502,672` x86_64 reference block decoupled-or-deleted (closes the
compile-coupling AND the grep RED); **(i)** `src/scalar/byte_class_from_eq_set_64.rs:10-15` x86
doc cross-refs scrubbed aarch64-neutral — mirroring αC/αE which already carry it verbatim.
Mechanism-correct, non-architectural; the direction (delete the whole x86 surface, verify
crate-wide) is right — only the binding obligation/grep reach mismatch is closed.

---

## §4 — Per-disposition tally (CH6 V6-confirm)

| # | Disposition | Verdict |
|---|---|---|
| 1 | F16 enumeration folded into SYNTHESIS.md (`:156`/`:333`/`:411`/`:566` name `profile`/`source_inputs`/`metadata_inputs`, by-exclusion, profile at every restatement) | **ACCEPT** |
| 2 | F16 folded into αE (`:19`/`:105`/`:156`/`:207`) + αC (`:64-65`, §8.1 prose) — full operative set, F16-tagged | **ACCEPT** |
| 3 | F16 P3-collapse-preserves-profile-distinctness obligation carried (SYNTHESIS `:165-167`, αE `:105`/`:156`/`:207`) | **ACCEPT** |
| 4 | CH5 F.6 widening folded into αE P1 row (`:94` item (3), `:101` decouple-not-delete, `:104` build-sound exit) | **ACCEPT** |
| 5 | CH5 F.6 widening folded into αC (`:168-176`/`:188-193`/`:212-215`, verbatim removal + build-soundness chain) | **ACCEPT** |
| 6 | Disk facts re-verified: checkasm_parity.rs 11 tokens / 9 active compile-coupled; scalar/byte_class x86 doc-refs; 28-file grep reach | **ACCEPT** |
| 7 | Substance-converged goalset (PRUNE→GENERALIZE→PROVE→HONESTY, net −12650…−12850, R10) un-touched, no re-litigation, no new paper-close surface in the goalset | **ACCEPT** |
| 8 | F16 enumeration NOT carried into HANDOFF.md inv.5 (iv) (`:276-280`) + telemetry (`:331-333`) — retains V4 `minus-output_dir` form OMITTING `profile`/`source_inputs`/`metadata_inputs`; the gate-consumer-read column ships the too-narrow projection | **REVISE** |
| 9 | CH5 F.6 widening NOT carried into binding SYNTHESIS P1 gate (`:326` (a)-(g) + false "every active hit on (a)-(g)" closure) + `x86_tree_deleted` telemetry (`:576`) + HANDOFF P1 (`:101-112`/`:336-338`); RED-by-construction binding gate, compile-coupling unnamed in the inventory-of-record | **REVISE** |

Both REVISEs are orphan-PROPAGATION residuals of the SAME two folds this cycle confirms: the
fold landed in SYNTHESIS+feeders (F16) / feeders only (F.6) but a co-binding document the gate
consumer reads (HANDOFF inv.5+telemetry; binding SYNTHESIS P1+telemetry) retained the pre-fold
form. Each carries a concrete single-edit fix that mirrors text already authored elsewhere in the
contract — mechanism-correct, non-architectural, not a finding reversal, not a goalset re-open.
Neither is a REJECT: the directions (by-exclusion projection; delete-the-whole-x86-surface
crate-wide) are correct; only the BINDING reach of each obligation is incomplete.

---

## §5 — CH6 verdict

The two orphan folds are mechanism-correct and PARTIALLY discharged: F16 is complete in
SYNTHESIS.md + αC + αE; CH5 F.6 is complete in αC + αE. The CONFIRMING posture ACCEPTs all seven
correct+complete dispositions. But the ANTI-PAPER-CLOSE lens flags two genuine residual defects:
**neither fold reached the BINDING inventory-of-record it is gated by** —
- F16's by-exclusion projection never entered HANDOFF.md's invariant-5 (iv) or the
  `runtime_target_rows_collapsed` telemetry column (still V4 `minus-output_dir`, OMITS `profile`);
- CH5 F.6's (h)/(i) sites never entered the binding SYNTHESIS P1 gate (`:326`) or the
  `x86_tree_deleted` telemetry (`:576`) or the HANDOFF P1 receiver — which STILL assert the
  (a)-(g) list is reach-matched, a claim the live 28-file grep falsifies, shipping a
  RED-by-construction PRUNE gate with an unnamed compile-coupling.

Because `--skv18-generalization-report` consumes the SYNTHESIS telemetry columns + the HANDOFF
schema (NOT the αC/αE feeders), these are not cosmetic feeder loose-ends — they are the precise
paper-close surface this lens guards: a gate the receiver can execute literally and still leave
RED, inviting silent grep-narrowing. Both are single-edit, text-already-authored-elsewhere folds.
**CH6 V6-confirm: 7 ACCEPT, 2 REVISE, 0 REJECT** — substance-converged, two binding-reach
residuals that the orchestrator must fold into HANDOFF.md (F16) and SYNTHESIS.md/HANDOFF.md P1
(F.6) before §3Z zero-orphan closes.

---

**TALLY accept=7 revise=2 reject=0**
