# S-P0 audit-overfit hardening V2 — CH6 ANTI-PAPER-CLOSE (post-fold confirm)

Lens (CH6 ANTI-PAPER-CLOSE): the audit-overfit pass does not declare victory over a surface it
has not closed. Three obligations: (1) the residual-overfit census is COMPLETE — no live overfit
surface silently waved past; (2) the 6 addenda are EXECUTABLE and correctly catch the V3 failure
modes — none decorative, none a prose assertion wearing a gate costume; (3) the PRUNE-sequencing
is SOUND — no GENERALIZE/PROVE wave marches over a RED predecessor, no escape hatch lets a
hand-written blob re-enter under a derived label. A paper-close here is the audit blessing a
surface it has not actually proven — the inverse of the SK-V13 build-first-audit-never pattern
this pass exists to forbid.

V2 is the POST-FOLD confirm: the two V1 CH6 REVISEs (R1-CH6, the R-A0-1 OR-escape "beats CSSOM"
qualifier; R2-CH6, the R-A0-2 deferred-P3 collapse-to-one disk answer) must be DISCHARGED into
the consolidated R-A0-* rows, not merely asserted in a0. Every dispositive witness below was
INDEPENDENTLY re-grepped at HEAD `83b66db42` this pass — the audit's and the prior CH6's claims
are not taken on their word.

## Independent disk re-verification (the anti-paper-close floor)

| Audit/prior-CH6 claim | Re-grep (this pass) | Verdict |
|---|---|---|
| `CSS_GENERATED_RS` verbatim `&str` const, body `:701`→`:1611` | `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`; closing `"#;` at `:1611`; 8 `_RS` couriers (`JSON_PARSE_ONLY_GENERATED_RS:195`,`JSON_PARSE_ONLY_PARSER_RS:550`,`JSON_MOD_RS:572`,`JSON_HOST_RS:594`,`CSS_MOD_RS:598`,`CSS_PARSER_RS:612`,`CSS_SINK_RS:665`,`CSS_GENERATED_RS:701`) | CONFIRMED |
| 7 css_l4 `generated.rs` byte-identical | all 7 share md5 `b654562ccff46ed62dd48e9ace325830` (re-md5'd this pass) | CONFIRMED |
| `RuntimeEmitterKind` grammar-family fork behind neutral enum | `grammar_provider.rs:40-42` enum `{CompiledLowering,RequestFacts}` + `:33` field + `:110` live dispatch | CONFIRMED |
| `ValueRef<G: EventGrammar = AnyGrammar>` phantom (G axis) | `tape/mod.rs:175` verbatim | CONFIRMED |
| warm path live; canonical kept | `nonjson_css_l4.rs:3091 fn measure_mbps`; `bin/css_canon_bench.rs` PRESENT | CONFIRMED |
| CSS NEON dead at admission | `find_css_significant` sole non-defn caller `lib.rs:574` (inside `#[cfg(test)]` from `:51`); only `count_top_level_commas` reaches the 7 generated css_l4 modules (cold rich-summary) | CONFIRMED |
| R16 BOTH nested structs | `RuntimeTarget` derives only `Clone, Copy, Debug` (`regen.rs:5`); `frontend_requirements` (#11) + `output_labels` (#12); `RuntimeFrontendRequirements:46` + `RuntimeOutputLabels:92` both derive `PartialEq, Eq` | CONFIRMED |
| **R-A0-1 discharge landed in SYNTHESIS** | `SYNTHESIS-AUDIT-OVERFIT.md:99` R-A0-1 row carries "an unqualified 'beats CSSOM'/'equal-work' close-report claim behind a re-label is a REJECT, per a0 §4"; a0 §4 source `a0:323-326` | CONFIRMED |
| **R-A0-2 discharge landed in SYNTHESIS** | `SYNTHESIS-AUDIT-OVERFIT.md:100` R-A0-2 row carries "DISK EVIDENCE is collapse-to-one … `generator_grammar_count == 3` = json+css+sheets, NOT json+7-css+sheets … the EXACT overfit the addendum forbids (a0 §5)"; a0 §5 source `a0:378-385` | CONFIRMED |

Not one claim is fabricated, stale, or rounded past a gate threshold. The audit re-grepped the
LIVE HEAD, not the contract-snapshot `318d9c046`. Both V1 CH6 REVISE discharges are REAL — they
landed in the consolidated R-A0-* rows, not just in a0.

## The three potential paper-close seams CH6 must test (all closed in the BINDING contract)

A paper-close at this altitude would be an audit that cites a gate the contract does not actually
enforce. I tested the three load-bearing gate names the R-A0-* and §6 dispositions rest on
against the BINDING `sk-v18/SYNTHESIS.md` (not the audit-overfit synthesis):

- **`generator_grammar_count == 3`** (the R-A0-2 collapse-to-one answer) is a REAL telemetry
  column, `SYNTHESIS.md:571` ("distinct grammars emitted by the ONE generator: json, css, sheets
  = 3", gated "yes for PROVE"), echoed `:593` + `HANDOFF.md:345`. NOT an invented column.
- **PROVE-Sheets "do NOT stub-prove"** anti-scaffold clause is REAL, `SYNTHESIS.md:337` + `:383`
  ("if Sheets cannot be emitted via the generator ONLY … do NOT stub-prove; do NOT hand-write a
  `_GENERATED_RS` Sheets block") + the gate row `:472`. The anti-scaffold backstop is binding.
- **`verbatim_blob_present == false`** (the honest-finding-escape (c) machine-check) is REAL,
  `SYNTHESIS.md:563`; the gate "REJECTS any row with `verbatim_blob_present == true`" (`:602`);
  the (a)-(c) escape gate is machine-bound at `:342` ("A primitive failing (a)-(c) is a relabeled
  hand-written blob — REJECT, REDRESS, do NOT close"). The largest paper-close surface is named
  and machine-gated, not blessed.

All three rest on real, contract-enforced gates. The audit's dispositions cite gates that exist.

## The convergence-honesty seam — DISCLOSED, not papered (the sharpest CH6 test)

The most tempting paper-close at S-P0 would be to conflate the audit-overfit pass's own
convergence with the upstream Alpha-hardening convergence. a0 §0 (`a0:76-82`) does NOT: it states
plainly that `CONSOLIDATED-CONVERGED.md` records `Converged=false` (V5 97.9% + confirm 88.9% is
NOT a 2-consecutive ≥95% pair — re-verified `CONSOLIDATED-CONVERGED.md:36-37,40-42,69`), names the
residuals as mechanical (binding-row propagation + a 12/13 struct-count slip), VERIFIES in §2 they
are SINCE folded at HEAD, and proceeds under the explicit dispatch instruction "INPUTS LOCKED,
G-Alpha closed." a2 §6 (`a2:460-483`) independently re-verifies the folds at HEAD. This is the
anti-paper-close discipline applied to the audit's own footing — the surface is treated as locked
by dispatch fiat AND the disclosure is on the record. Not a seam.

## Dispositions

- **(1) Audit completeness — no silent wave-past (ACCEPT).** The residual census
  (`SYNTHESIS-AUDIT-OVERFIT.md:81-101`, R1–R16 + R-A0-1/2/3) maps EVERY live overfit surface to a
  named PRUNE/GENERALIZE wave with a machine-checkable gate; zero orphan finding. The audit names
  its own deepest hole and bolts it: the honest-finding "named primitive" escape (a0 §6 / R-A0-3)
  is carried as a STANDING paper-close surface, machine-gated (a)-(c), rather than blessing the
  contract's prose-reviewed backstop. The two R-A0-* framing residuals are NOT paper-closed — the
  asymmetry REJECT clause and the collapse-to-one disk answer are carried UP into the consolidated
  rows (verified above), closing the V1 thin-seam where SYNTHESIS under-stated what a0 §4/§5 had
  reached.
- **(2a) The 6 addenda EXECUTABLE, not decorative (ACCEPT).** a1 §L1–L6 gives each addendum a
  concrete grep/diff/md5/samply runnable from `skinny/crates/`, a telemetry column the `gate-json`
  consumer REJECTs on, and a LIVE witness it FIRES on today — all six re-verified firing this pass.
  md5-distinctness is declared NECESSARY-NOT-SUFFICIENT (3-co-gate conjunction: md5 ∧
  `generator_grammar_branch_count==0` ∧ `generator_grammar_type_count==0` ∧
  `runtime_target_rows_collapsed==true`; the branch/type columns confirmed real at
  `SYNTHESIS.md:565`/`:567`). L3 catches the fork behind NEUTRAL enum names
  (`CompiledLowering`/`RequestFacts`) — the overfit-behind-an-abstraction a paper-close would hide.
- **(2b) The addenda correctly catch the V3 modes (ACCEPT).** Each lens is pinned to its V3 finding
  by path:line (L1→D1 `:30-31`, L2→D1 `:34`, L3→D1 `:32`, L4→D2 `:36`, L5→C3 `:53`+C2 `:50`,
  L6→C1 `:47`); the falsifiers are the precise inverse of each fake-generalization surface (L1's
  `.bbnf`-mutation test; L6's caller-census-excluding-tests). The R1-CH2 fold made the L1 (b)
  escape predicate a MACHINE per-primitive mutate-falsifier (the primitive's emitted output must
  VARY under a `.bbnf` mutation — "accepts a grammar-derived argument" is insufficient), closing
  the one-level-down prose-review seam that was the last prose-expressible predicate.
- **(2c) The relocated-overfit-seam closed STRUCTURALLY at full-row altitude (ACCEPT).** The R1-CH5
  fold widened `runtime_target_rows_collapsed` to inline BOTH nested structs (`frontend_requirements`
  #11 AND `output_labels` #12), forbidding the one-field-over shallow-compare false-green. Crucially
  the recipe is stated as a mechanism-AGNOSTIC INVARIANT (`a3:196-208`: `PartialEq` is "PREFERABLE
  … ONE sufficient mechanism," with serialize-then-hash / jq as alternatives) — `[pluggable-components]`
  compliant, NOT a hardcoded-mechanism close. The `+1`-line `PartialEq`-derive cost is disk-stated
  honestly (`RuntimeTarget` derives only `Clone, Copy, Debug`; both nests already `PartialEq, Eq`).
- **(3) PRUNE-sequencing SOUND (ACCEPT).** PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1 with
  exit-gate-blocks-successor binding (a2 §2). Two load-bearing couplings are bound and NOT
  paper-closed: P4-before-emitter-rebuild (the Lock-14 gate meaningful when the new emitter is
  authored, a2 §2 edge 1c) and P1↔`checkasm_parity.rs` build-soundness (the `src/x86_64/` deletion
  is build-BLOCKING without decoupling 9 active call sites in the SAME wave, a2 §3). The R1-CH3
  directional fold makes "G3-failure blocks PROVE" the explicit forward arrow (never a backward
  "G3 gates G1/G2") and annotates the dual entry-gate (G2 on BOTH G1 AND P3). The PROVE-Sheets
  do-not-stub-prove clause (`SYNTHESIS.md:337`/`:383`/`:472`) is the anti-scaffold backstop — no
  scaffold-only landing counts.

## Anti-paper-close affirmative (what the audit refuses to bless)

1. It does NOT bless the `@generated` banner — L1 checks the BODY, not the provenance header.
2. It does NOT accept md5-distinctness as proof of N grammars — the 3-co-gate conjunction.
3. It does NOT accept a checkasm-green kernel as an admitted acceleration — caller-census excludes
   tests; the G6 retire branch is gated on a samply non-top-N MEASUREMENT, not an assertion.
4. It does NOT let the honest-finding escape become a courier-relabel hatch — (a)-(c)
   machine-checked, the abrogate-before-patch outcome, not the default.
5. It does NOT stub-prove Sheets — the PROVE fallback surfaces an honest finding.
6. It does NOT conflate its own convergence with the upstream Alpha non-convergence — a0 §0
   discloses `Converged=false` and proceeds only under the dispatch's "INPUTS LOCKED" fiat.

## REVISE (0) / REJECT (0)

Both V1 CH6 REVISEs are DISCHARGED — the R-A0-1 "beats CSSOM" REJECT clause and the R-A0-2
collapse-to-one `generator_grammar_count == 3` disk answer are carried into the consolidated
R-A0-* rows (independently disk-verified at `SYNTHESIS-AUDIT-OVERFIT.md:99-100`), backed by real
contract telemetry columns. No surface is left blessed-but-unproven; no fabricated gate; no silent
wave-past. The audit is COMPLETE, the 6 addenda are EXECUTABLE + V3-catching, the PRUNE-sequencing
is SOUND. Zero orphan REVISE.

## Tally
ACCEPT 7 · REVISE 0 · REJECT 0 — **100%**. (1) completeness, (2a) executable, (2b) V3-catching,
(2c) relocated-seam structural at full-row altitude, (3) sequencing sound — all ACCEPT; both V1
CH6 REVISEs DISCHARGED into the R-A0-* rows; every load-bearing gate the dispositions cite is
real and contract-enforced; the convergence-honesty seam is disclosed, not papered.

TALLY accept=7 revise=0 reject=0
