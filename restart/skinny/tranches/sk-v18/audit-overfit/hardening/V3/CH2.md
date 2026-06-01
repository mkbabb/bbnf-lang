# S-P0 audit-overfit hardening V3 — CH2 Generality (independent re-grep at post-fold HEAD)

Reviewer: CHALLENGE lens CH2 GENERALITY (V3), per `PASS-0-OVERFIT-AUDIT.md` §3 + ORCHESTRATOR
§3W. Subject: the SK-V18 S-P0 audit-overfit artefacts `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md` +
`HARDENING-S-P0-CONSOLIDATED.md`, re-grepped LIVE at HEAD `83b66db4232374db6a5f9fa009882f41acc04342`
(`git rev-parse HEAD` matches). The a0–a3/SYNTHESIS bodies were last re-folded AFTER the prior
V3/CH siblings; this pass re-verifies CH2's three mandated questions against the CURRENT artefact
text and the CURRENT tree. CH2 owns: (1) residual-overfit-audit COMPLETENESS; (2) the 6 addenda
are EXECUTABLE + correctly catch the V3 failure modes WITHOUT single-surface overfit; (3) the
PRUNE-sequencing is GENERALITY-SOUND. Disposition ACCEPT/REVISE/REJECT with path:line.

The CH2 generality test is sharper than "does the lens grep something": a lens is general iff it
(a) FIRES on a real disk surface, (b) enumerates its DISGUISE SET (so a relabel/relocation cannot
slip), and (c) is NOT satisfiable by ERASURE of legitimate distinctness. All six addenda + R16 +
the PRUNE graph are re-checked against this standard, and every dispositive witness is re-grepped.

---

## §1 — Addendum DISGUISE-SET generality (no lens over-fits a single surface) — ACCEPT

Each addendum names a disguise set, not one literal. Re-verified LIVE this pass:

- **L1 verbatim-blob** catches the const-`&str` courier `runtime_generator.rs:701` (re-grepped:
  `const CSS_GENERATED_RS: &str = r#"` present) AND the relabel/fragment-concat disguise (a1
  check (a) `rg 'const \w*_RS\s*:\s*&str\s*=\s*r#"'` fires on the SYNTAX regardless of name —
  `a1:111`). Not keyed to `CSS_GENERATED_RS` alone; the 8 sibling couriers (`a1:104-106`) prove
  the class. Generality-positive.
- **L2 distinct-grammar-output** is a 3-co-gate CONJUNCTION (`a1:179-200`, `SYNTHESIS:59-63`):
  md5-distinct ∧ branch-count==0 ∧ type-count==0 ∧ row-collapse. Re-grepped the crux: the 7
  css_l4 rows carry 7 DISTINCT `profile` strings (`regen_css.rs:38,56,74,92,110,128,146`) under
  ONE neutral `grammar_name: "css_l4"` (`regen_css.rs:37,55,73`), and the arm-census regex
  `Json =>|CssL4 =>|GoogleSheets… =>|Sheets… =>` returns ZERO over `xtask/src/` — confirming the
  regex is SYNTACTICALLY INCAPABLE of firing on a neutral data-table. The md5-NOT-sufficient half
  is therefore load-bearing, not decorative. Generality-positive (the lens catches the relocated
  seam the regex cannot).
- **L3 single-emitter-path** catches the fork even behind NEUTRAL enum names — `RuntimeEmitterKind
  {CompiledLowering, RequestFacts}` (re-grepped `grammar_provider.rs:40-42` + `:110` dispatch),
  where the L2 arm-census is clean (`Json =>` absent) precisely because the names are neutral.
  L3 enumerates emitter-strategy discriminators (`a1:257`), not the literal enum. Generality-positive.
- **L4 phantom-generic** points at the `G` (EventGrammar) axis, EXPLICITLY excluding the real
  `K = AnyKind` axis (`a1:309`, re-grepped `tape/mod.rs:175` — `K = AnyKind, G: EventGrammar =
  AnyGrammar`); test-excludes `_proof_compiles`; companions `json_rich_navigation_preserved` so a
  ≥2 impl-count cannot LCD-flatten (`a1:348-354`). Three disguises enumerated (inert-witness,
  test-only-instantiation, LCD-flatten). Generality-positive.
- **L5 timed-plane + corpus-in-timer** enumerates THREE independent violations (warm /
  micro-fixture / more-work competitor — `a1:374-379`); re-grepped `measure_mbps:3091` live, 48
  grep hits; KEEPS the honest `css_canon_bench`. Not a single-flag check. Generality-positive.
- **L6 acceleration-wiring** distinguishes EXISTS-and-checkasm-green from REACHED-at-admission;
  re-grepped: `find_css_significant` has ZERO `generated.rs` callers and its only `runtime/src`
  caller is `lib.rs:574` (below the sole `#[cfg(test)]` at `lib.rs:51` = test-only), while
  `count_top_level_commas` DOES reach generated.rs (cold). Enumerates wire / retire-on-measurement
  / scalar-passthrough-label (`a1:506-515`). Generality-positive.

No addendum over-fits a single surface; each enumerates its disguise set; each FIRES on a
disk-verified live witness. **ACCEPT.**

## §2 — R1-CH2 (L1 (b) machine mutate-falsifier) holds and generalizes the escape — ACCEPT

The honest-finding "named primitive" escape (R-A0-3) is the contract's single largest paper-close
surface (`a0:76-81`, `SYNTHESIS:103`). The V1 CH2 REVISE generalized predicate (b) from the
prose "accepts grammar-derived DATA" to the per-primitive MACHINE mutate-falsifier: the
primitive's EMITTED OUTPUT must VARY under a `.bbnf` mutation of the invoking rule; a fixed body
keyed off a merely-decorative grammar-derived argument FAILS (b) (`a1:139-148`, `a0:122-129/432-441`).
This is the correct generality altitude — a decorative-argument disguise (argument grammar-derived,
body verbatim) is exactly the one-level-down relabel a prose predicate misses. All three escape
predicates (a)/(b)/(c) are now MACHINE checks (grep · mutate+regen-diff · telemetry column), so
the largest paper-close surface is no longer prose-reviewed-at-admission. **ACCEPT.**

## §3 — R1-CH5 / R16 (full-expanded-row recipe) holds — the deepest generality layer — ACCEPT

R16 pins `runtime_target_rows_collapsed` at the INVARIANT altitude — the FULLY-EXPANDED row minus
the two path columns, inlining EVERY nested-struct field — not at one named nested struct
(`a3:117-214`, `a2:382-400`, `a0:143-157`, `SYNTHESIS:211-221`). Re-verified the disk grounding
this pass: `RuntimeTarget` derives `Clone, Copy, Debug` ONLY (`regen.rs:5` — NOT `PartialEq`), and
its two nested-struct fields `frontend_requirements` (#11) + `output_labels` (#12) both wrap
structs that ALREADY derive `PartialEq, Eq` (`grammar_provider.rs:45`/`:91` re-grepped). So the
full-row `RuntimeTarget: PartialEq` mechanism is viable at a +1-line cost and covers BOTH nests
automatically — it cannot be coupled to a hand-rolled one-struct field list. The generality win
is precise: a recipe recursing into `output_labels` only would shallow-compare
`frontend_requirements` and false-green a FUTURE seam riding it (today uniform
`REQUEST_FACTS_REQUIREMENTS`, not yet a live vector — `a3:182-184` — but the gate's PURPOSE is to
forbid the future seam). Stated mechanism-agnostically (derive-PartialEq / serialize-hash / jq), so
no overfit to one implementation. This is the deepest layer of the necessary-not-sufficient
lineage (md5 → grep-alphabet → grep-cannot-fire → row-count → by-exclusion → recurse-both-nests).
**ACCEPT.**

## §4 — R1-CH3 (directional arrow) holds, generality-positive — ACCEPT

"G1/G3 co-derive; G3-failure blocks PROVE" is the general FORWARD revert claim; re-read a2 §4 +
§0 item 3 + §7/§8 and SYNTHESIS §5 — the dependency graph orders G3 AFTER G1/G2 and the binding
arrow is forward (G3-failure→PROVE), never a backward "G3 gates G1/G2" (`a2:298-408`,
`SYNTHESIS:202-207`). The dual entry-gate (G2 entry-gates on BOTH G1 AND P3 — a P3 failure ALSO
blocks G2 independent of G1) is annotated (`SYNTHESIS:206-207`, `a0:476-482`). Re-grepped a2 → zero
"un-fork gates G1/G2" assertions outside the explicit R1-CH3 fold note. The forward arrow is the
general form: un-fork is meaningless unless ≥1 grammar genuinely projects, so G1 projects first.
**ACCEPT.**

## §5 — PRUNE-sequencing generality: no gate satisfiable by ERASURE — ACCEPT

The crux generality hazard in a PRUNE graph is a gate that closes by ERASING legitimate
distinctness rather than by real collapse. Re-verified the anti-erasure posture holds at every
gate: P3 explicitly FORBIDS erasing legitimate `profile` distinctness — collapse-to-one ONLY when
genuinely one grammar, else differentiate by distinct `.bbnf` roots (`a2:376-380`,
`a0:387-403`, `SYNTHESIS:102`); R-A0-2 names "manufacturing 7 fake roots to satisfy a distinctness
gate" as the EXACT forbidden overfit, with the disk answer carried up (`generator_grammar_count
== 3` = json+css+sheets, NOT json+7-css+sheets — `a0:392-399`, `SYNTHESIS:102`). The dependency
graph (PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1) is generality-sound: P4-before-emitter-rebuild
so the Lock-14 gate is meaningful AS the un-forked emitter is built (`a2:235-239`); the P1 ↔
`checkasm_parity.rs` build-soundness coupling forces deletion-list = verify-grep reach (a narrower
list ships a RED-by-construction gate — `a2:254-294`); S-P1-profile-before-G5/G6 grounds the G6
retire branch in a samply MEASUREMENT not an assertion (`SYNTHESIS:208-210`). No PRUNE item carries
generalization risk (pure deletion + gate-tightening, zero >SOTA-bearing code removed —
`a2:70-77`). The relocated-overfit-seam — the one residual risk surviving INTO generalize — is
policed STRUCTURALLY (P3 collapse over the full-expanded row), not by the regex, binding P3 (PRUNE)
to G3 (GENERALIZE) (`a2:340-408`). **ACCEPT.**

## §6 — Residual-overfit-audit COMPLETENESS — ACCEPT

The residual census is complete at the generality altitude. The R1–R16 implementation residuals
are each LIVE-witnessed and mapped to a named PRUNE/GENERALIZE wave with a machine-checkable gate
(`SYNTHESIS:83-104`); the R-A0-* FRAMING residuals (the seams the contract's own escape clauses
leave open) are surfaced and bound — R-A0-1 ⊆ R14 (H1 lazy-vs-eager framing, with the explicit
"beats CSSOM"/"equal-work" REJECT clause — `a0:330-339`); R-A0-2 ⊆ R4/P3 (collapse decision bound
to S-P3 with the disk answer); R-A0-3 the verbatim-blob escape, machine-checked (a)-(c). The ONE
NEW finding (R16, MEDIUM) is the nested-`output_labels`/`frontend_requirements` gate-recipe
precision hazard — a genuine generality contribution the Alpha CHALLENGE did not fully surface,
disk-grounded this pass. ZERO new HIGH/CRITICAL residual; ZERO new hardcoding admitted by the
goalset; the affirmative-CLEAN inventory (substrate Lock 1, neutral NEON kernel, `css_canon_bench`,
14-file checkasm, regen plumbing — `SYNTHESIS:109-113`, `a0:494-515`) is explicit so PRUNE does not
throw aarch64 hardening out with the x86 bathwater. No generality gap. **ACCEPT.**

---

## Disposition summary

| § | Section | Disposition |
|---|---|---|
| §1 | Addendum disguise-set generality (no single-surface overfit) | ACCEPT |
| §2 | R1-CH2 L1 (b) machine mutate-falsifier (escape generalized) | ACCEPT |
| §3 | R1-CH5 / R16 full-expanded-row recipe (deepest generality layer) | ACCEPT |
| §4 | R1-CH3 directional arrow (forward revert generality) | ACCEPT |
| §5 | PRUNE-sequencing — no gate satisfiable by erasure | ACCEPT |
| §6 | Residual-overfit-audit completeness | ACCEPT |

Every dispositive witness re-grepped LIVE at HEAD `83b66db42` (CSS courier `:701`; emitter fork
`:40-42`/`:110`; phantom `G` `:175`; 7× md5 `b654562c…`; warm `measure_mbps:3091`; NEON
`find_css_significant` test-only `lib.rs:574` under `#[cfg(test)]:51` + zero generated.rs reach;
`RuntimeTarget` 12 fields, derives Clone/Copy/Debug only `regen.rs:5`, both nested structs
`PartialEq, Eq` `grammar_provider.rs:45`/`:91`; 7 distinct css_l4 `profile` under one neutral
`grammar_name`; arm-census regex zero hits on the data-table). No addendum over-fits a single
surface; each enumerates its disguise set; no gate is satisfiable by erasure; R16 holds at the
full-expanded-row altitude. No new generality defect; no orphan REVISE; zero REJECT.

## Tally
ACCEPT 6 · REVISE 0 · REJECT 0 — **100%**.

TALLY accept=6 revise=0 reject=0
