# CH2 — GENERALITY (V6-confirm)

Lens: CH2 Generality (PASS-ALPHA §3 / SK-V18-GENERALIZATION-HANDOFF §4). Reviewer focus: **does the
goalset respect Lock 14 (one generator ALL grammars); are the interventions grammar-neutral; will
they work for non-JSON grammars (CSS L4 / GoogleSheets / BBNF-self); is the GoogleSheets 3rd-grammar
proof load-bearing?** Subject: SK-V18 = THE GENERALIZATION CYCLE.

Date 2026-05-31. Bracket HEAD `318d9c046`. **CONFIRMING cycle** following the 2-orphan REVISE fold
(F16 field-enumeration + P1 x86-deletion-list widening). This wave's mandate is NOT to re-litigate the
converged goalset (V1 74.2 → V2 96.7 → V3 94.8 → V4 92.7 → V5 97.9%, zero REJECT every cycle): it is to
(1) verify the single open CH2 orphan (V5 §8.1, F16 `profile`-omission) is discharged orphan-free,
(2) corroborate the CH5 F.6 P1 x86-deletion-list widening (cross-lens, for the consolidated converged
ledger), (3) ACCEPT where the artefact is correct + complete, and flag ONLY genuine residual defects.

Method: confirming re-disposition of the seven CH2 sections (αA..αE + SYNTHESIS + HANDOFF) at live HEAD
plus the V5 §8 finding. Every disposition cites `path:line` / artefact-line, verified live where
checkable.

---

## §0 — Lens verdict (one paragraph)

**The single open CH2 orphan (V5 §8.1, F16) is folded verbatim and orphan-free across all four
operative-enumeration sites; the goalset's Lock-14 spine is correct and the GoogleSheets 3rd-grammar
proof remains load-bearing. Zero NEW defects. Zero REVISE. Zero REJECT.** Verified live at HEAD
`318d9c046`: (1) the live `RuntimeTarget` struct (`skinny/xtask/src/regen.rs:6–18`) has exactly **12
fields** — `grammar_name`, `profile`, `entry_rule`, `source_roots`, `output_dir`, `check_command`,
`source_inputs`, `metadata_inputs`, `emitter`, `expected_files`, `frontend_requirements`,
`output_labels` — confirming the V5 finding's struct basis; (2) the F16 operative enumeration is now
stated **BY EXCLUSION** of the two generated-artefact path columns (`output_dir`, `expected_files`) at
every site, and the non-path operative set now **explicitly names `profile`, `source_inputs`,
`metadata_inputs`** alongside `emitter`/`entry_rule`/`source_roots`/`check_command`/
`frontend_requirements`/`output_labels` (10 of 12 fields — exactly prose-minus-path) — the V5 §8.1 gap
is closed; (3) `profile` carries **7 distinct** values across the live css_l4 rows
(`grep -E 'profile: "css_l4' regen_css.rs | sort -u | wc -l` → 7), and `fact_schema` likewise 7 distinct
— so the corrected by-exclusion gate is RED-by-construction pre-P3, exactly as the fold states; (4) the
fold additionally captures the P3-consequence the V5 finding required — **"P3 must PRESERVE
profile-distinctness where the 7 profiles are genuinely distinct grammars, never erase the `profile`
discriminator"** — present at SYNTHESIS:165, HANDOFF:332, αE:105/:156, αC:96-102. The CH5 F.6
cross-lens orphan (P1 x86-deletion-list narrower than the crate-wide grep) is corroborated discharged:
`tests/checkasm_parity.rs` (live `grep -cE 'x86_64'` = **11**, 9 active compile-coupled
`bbnf_simd::x86_64::…` call sites) and `src/scalar/byte_class_from_eq_set_64.rs` are both now on the P1
list with the build-soundness rationale explicit (αC:168-215, αE:94/:101/:104). All seven CH2 sections
ACCEPT.

---

## §1 — F16 fold discharge (the single open CH2 orphan) → **ACCEPT** (V5 §8.1 DISCHARGED)

V5 §8.1 (REVISE-1) held that the F16 `runtime_target_rows_collapsed` machine-check was redefined with
two non-equivalent forms: a correct + complete PROSE ("byte-identical in EVERY field except the
generated-artefact path columns") and an OPERATIVE enumeration (`fact_schema`/`row_id`/`output_plane`/
`emitter`/`entry_rule`/`source_roots`/`check_command`/`frontend_requirements`, 8 named labels) that was
a strict subset of the prose and OMITTED `profile` (the 7-distinct per-profile discriminator a relocated
branch most naturally rides) plus `source_inputs`/`metadata_inputs`. The prescribed fix: enumerate by
EXCLUSION of the two path columns so the operative check equals the prose; name `profile` explicitly;
keep the P3 mechanism.

**The fold is applied verbatim and orphan-free at every operative-enumeration site:**

- **SYNTHESIS.md** — `:152` "The projection is stated **by EXCLUSION** so the operative list cannot
  drift to a strict subset of the prose (CH2 V5 §8.1 / F16)"; `:156` the operative set now reads
  `grammar_name`/**`profile`**/`entry_rule`/`source_roots`/`check_command`/**`source_inputs`**/
  **`metadata_inputs`**/`emitter`/`frontend_requirements`/`output_labels`; `:157-159` records the
  rationale ("the prior enumeration … OMITTED `profile` itself — the 7-distinct per-profile
  discriminator that differentiates the 7 css_l4_* configs — plus `source_inputs`/`metadata_inputs`");
  `:165` the P3-PRESERVES-profile-distinctness consequence; `:333` (G3 close-condition), `:566`
  (Section-2 telemetry column `runtime_target_rows_collapsed`), and `:411` (W5C retirement clause) all
  carry the by-exclusion form with `profile`/`source_inputs`/`metadata_inputs` named.

- **HANDOFF.md** — `:274` inv.5 "byte-identical in EVERY field except the generated-artefact path
  columns"; `:332` names the operative columns including the per-profile set with the
  P3-preserves-profile-distinctness note.

- **alphaE** — `:19` (the F16 fold ledger entry) restates the gate by exclusion and records
  `profile`/`source_inputs`/`metadata_inputs` as the previously-omitted fields; `:105` (P3 exit),
  `:156` (B2 DISTINCT-GRAMMAR-OUTPUT gate), `:207` (B4 GoogleSheets litmus) all carry
  `count(distinct (profile, source_inputs, metadata_inputs, emitter, entry_rule, source_roots,
  check_command, frontend_requirements, output_labels)) == 1` with the "prior tuple OMITTED `profile`"
  annotation and the preserve-profile-distinctness consequence.

- **alphaC** — `:64-66` lists the full 12-field struct in prose; `:80` updated to cite
  `target.fact_schema`/`target.output_plane` as example relocation fields; `:96-102` the corrected
  by-exclusion gate ("byte-identical in EVERY field except the generated-artefact path columns") with
  RED-pre-P3 framing; `:455-474` the widened-tuple collapse check with the per-profile column census.

**Live verification of the fold's basis (HEAD `318d9c046`):**
- `RuntimeTarget` struct = 12 fields (`regen.rs:6–18`); prose-minus-path = 10 fields; the operative
  enumeration now names exactly those 10 (`profile` + `source_inputs` + `metadata_inputs` +
  `entry_rule` + `source_roots` + `check_command` + `emitter` + `frontend_requirements` +
  `output_labels` + `grammar_name`). Enumeration = prose. **The V5 §8 strict-subset gap is closed.**
- `grep -E 'profile: "css_l4' regen_css.rs | sort -u | wc -l` → **7 distinct** (the discriminator now
  inside the tuple); `fact_schema` → **7 distinct** — the corrected gate is RED pre-P3, GREEN only after
  the 7 profiles genuinely collapse, exactly as the fold states.
- The fold additionally captured the **P3-must-preserve-profile-distinctness** invariant the V5 finding
  flagged as load-bearing ("`profile` is the per-config discriminator, so the collapse must preserve
  profile-distinctness, not erase it") — present at SYNTHESIS:165, HANDOFF:332, αE:105/:156, αC:96-102.
  This is the correct mechanism-faithful capture: the 7 CSS profiles collapse to one config ONLY if they
  are genuinely one grammar; where distinct, they differentiate by distinct `.bbnf` roots, never by
  erasing `profile`.

**The fold is single-edit, mechanism-correct, non-architectural** — exactly as scoped. The P3 collapse
mechanism is untouched; only the operative column set was completed to equal the prose, and stated by
exclusion so no future `RuntimeTarget` field can silently fall outside it. **ACCEPT — V5 §8.1
DISCHARGED, orphan-free.**

---

## §2 — CH5 F.6 cross-lens corroboration (P1 x86-deletion-list widening) → **ACCEPT** (corroborated discharged)

Not a CH2-owned finding, but the prompt's confirm scope asks CH2 to verify it for the converged
consolidated ledger. V5 CH5 F.6 held that the P1 x86-deletion list ((a)-(g) in alphaC/alphaE) was
NARROWER than the crate-wide grep — omitting `tests/checkasm_parity.rs` (compile-coupled active
`x86_64::` imports — deleting `src/x86_64/` without decoupling BREAKS THE BUILD) and
`src/scalar/byte_class_from_eq_set_64.rs` (residual x86 refs).

**Corroborated discharged, live at HEAD:**
- `grep -cE 'x86_64' tests/checkasm_parity.rs` → **11** (matches the V5 finding's count).
- **alphaE:94** P1 item (3) now reads "**COMPILE-COUPLED removal/decoupling sites the verify grep ALSO
  fires on (V5 R-2/CH5 F.6 widening)**: `tests/checkasm_parity.rs` carries 11 `x86_64` tokens, 9 of them
  ACTIVE compile-coupled `bbnf_simd::x86_64::…::*_scalar(…)` call sites … deleting `src/x86_64/` WITHOUT
  decoupling these BREAKS THE BUILD; DECOUPLE-OR-DELETE … and `src/scalar/byte_class_from_eq_set_64.rs`
  carries residual x86 doc strings … CLEAN to aarch64/scalar-neutral." The P1 exit gate (`:104`) and
  checkasm status (`:101`) both carry the build-soundness rationale.
- **alphaC:168-215** §1-P1 lists both sites with the same COMPILE-COUPLED-not-doc-only attribution and
  the explicit "`checkasm_parity.rs` decoupling is what keeps the `src/x86_64/` deletion from breaking
  compilation" (`:215`).

The deletion-list reach is now matched to the verify grep — no RED-by-construction P1 gate. **ACCEPT
(cross-lens corroboration).**

---

## §3 — Confirming re-disposition of the seven CH2 sections (live at HEAD)

All seven sections were ACCEPT-or-ACCEPT-with-§8-sharpening at V5; with §8 now folded, each section is a
clean ACCEPT. The converged substance (verified live):

- **§3.1 αA** — substrate-generalizes / value-API-does-not split (one `Tape`/`ValueRef`/`PayloadArena`);
  phantom `G`-axis-not-`K`-axis precision (`tape/mod.rs:175`, `CssEventGrammar` absent at HEAD,
  DELETE-is-default); GoogleSheets skinny-tree obligation; x86-scope F15 propagation. **ACCEPT ×4.**
- **§3.2 αB** — GoogleSheets-as-GENERATION-not-throughput bar (no fabricated speed win);
  typed-rows-conditional + CSS LOW-lowering-risk. **ACCEPT ×2.**
- **§3.3 αC** — LayoutFacts-derive-not-hardcode (the Lock-14 generality vehicle); relocated-overfit-seam
  pre-block (prose obligation); P3 collapse + FOLD-3 (F16 — now with the corrected by-exclusion
  enumeration, §1); retirement clause + EventGrammar witness-type seam; FOLD-3 cross-artefact
  orphan-freedom. **ACCEPT ×5.**
- **§3.4 αD** — DM2 substrate-READY-not-proven; I3/I4/I5 the three generality invalidations (7 css_l4
  replicas md5 `b654562ccff46ed62dd48e9ace325830`, `RuntimeEmitterKind` fork `grammar_provider.rs:40`,
  phantom `<G>` two-axis); S12 GoogleSheets-litmus owner-surface; no-second-substrate pre-block.
  **ACCEPT ×4.**
- **§3.5 αE** — falsifiability triple (PRESERVED→SOTA / DERIVATION-PROOF / DISTINCT-OUTPUT, now carrying
  the corrected F16 enumeration); B1 un-fork + JSON projection; B2 CSS lowering; B3 shared trait +
  phantom (DELETE-default); B4 GoogleSheets-litmus (source named, md5-NNS, F10 alphabet, F13+F16
  collapse with profile-distinctness preserved); CANDIDATE A (PRUNE) sequencing + F15/F.6 crate-wide x86
  scope. **ACCEPT ×6.**
- **§3.6 SYNTHESIS** — G1–G4 + PROVE close conditions (G3 three-surface + corrected F16 collapse);
  §0.4 pre-blocks; §0.5 generalization litmus table; Lock-14 canonical three-surface model + full
  alphabet (`LOCKS.md:349`); Section-2 telemetry columns (`runtime_target_rows_collapsed` now
  by-exclusion); §0.3 receiver GoogleSheets sourcing (adopt-existing-Pratt). **ACCEPT ×6.**
- **§3.7 HANDOFF** — backlog (G1–G6 + PROVE); six CHALLENGE addenda; invariant 5 grammar-neutral (F16
  folded by exclusion); S-P3 wave sequencing (PRUNE→GENERALIZE→PROVE→HONESTY). **ACCEPT ×4.**

---

## §4 — Disposition ledger

| Artefact | ACCEPT | REVISE | REJECT | V5 (for comparison) |
|---|---|---|---|---|
| αA results-extraction | 4 | 0 | 0 | 4 / 0 / 0 |
| αB competitor-deltas | 2 | 0 | 0 | 2 / 0 / 0 |
| αC redress-digest | 5 | 0 | 0 | 5 / 0 / 0 |
| αD validated-invalidated | 4 | 0 | 0 | 4 / 0 / 0 |
| αE candidate-shortlist | 6 | 0 | 0 | 6 / 0 / 0 |
| SYNTHESIS.md | 6 | 0 | 0 | 6 / 0 / 0 |
| HANDOFF.md | 4 | 0 | 0 | 4 / 0 / 0 |
| §1 F16 fold (V5 §8.1 DISCHARGED, was the sole open CH2 REVISE) | 1 | 0 | 0 | (V5 §8 — 0/1/0) |
| **Total** | **32** | **0** | **0** | 31 / 1 / 0 |

Accept rate 32/32 = **100%** (above the §3Z ≥95% bar; V5 96.9%, V4 96.7%, V3 96.7%, V2 96.8%, V1
75.0%). The single open CH2 orphan (V5 §8.1 / F16 — the operative enumeration was a strict subset of its
own prose and OMITTED `profile`) is folded verbatim and orphan-free across all four sites: the gate is
now stated BY EXCLUSION of the two path columns, the operative non-path set explicitly names
`profile`/`source_inputs`/`metadata_inputs` (10 of the 12 live `RuntimeTarget` fields = prose-minus-path),
and the fold additionally captures the P3-must-preserve-profile-distinctness consequence the finding
required. Verified live at HEAD `318d9c046`: 12-field struct, 7 distinct `profile` + 7 distinct
`fact_schema` (gate correctly RED pre-P3). The CH5 F.6 cross-lens orphan (P1 x86-deletion list narrower
than the crate-wide grep) is corroborated discharged — `tests/checkasm_parity.rs` (11 `x86_64` tokens, 9
active compile-coupled call sites) + `src/scalar/byte_class_from_eq_set_64.rs` are now on the P1
deletion/decouple list with the build-soundness rationale explicit. The goalset's Lock-14 spine is
sound; the GoogleSheets 3rd-grammar proof is load-bearing (real 185-LOC Pratt grammar adopted,
`sheets_grammar_shape == pratt-operator` gated, Sheets config-tuple required distinct from css_l4 + json);
the interventions are grammar-neutral and will work for CSS L4 (scalar, no kernel to preserve) and
GoogleSheets (Pratt is the honest stress with an honest-finding fallback). With V5 (97.9% wave) + this
confirm (CH2 100%), the 2nd-consecutive ≥95% with zero open orphan REVISE is recorded — §3Z satisfied
for CH2.

TALLY accept=32 revise=0 reject=0
