# S-P0 audit-overfit hardening V1 — CH6 ANTI-PAPER-CLOSE

Lens (CH6 ANTI-PAPER-CLOSE, V1): the audit-overfit pass does not declare victory over surfaces
it has not closed. Specifically — (1) the residual-overfit audit is COMPLETE (no live overfit
surface is silently waved past); (2) the 6 addenda are EXECUTABLE and correctly catch the V3
failure modes (no addendum is decorative, none is a prose assertion masquerading as a gate);
(3) the PRUNE-sequencing is SOUND (no GENERALIZE/PROVE wave is permitted to march over a RED
predecessor, no escape hatch lets a hand-written blob re-enter under a derived label). A
paper-close here is the audit blessing a surface it has not actually proven — the inverse of
the SK-V13 build-first-audit-never pattern this pass exists to forbid.

Every dispositive witness below was INDEPENDENTLY re-grepped at HEAD `83b66db42` this pass — the
audit's claims are not taken on the artefact's word.

## Independent disk re-verification (the anti-paper-close floor)

| Audit claim | Re-grep (this pass) | Verdict |
|---|---|---|
| `CSS_GENERATED_RS` verbatim `&str` const | `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`; 8 `_RS` couriers in codegen | CONFIRMED |
| 7 css_l4 `generated.rs` byte-identical | all 7 share md5 `b654562ccff46ed62dd48e9ace325830` | CONFIRMED |
| `RuntimeEmitterKind` grammar-family fork | `grammar_provider.rs:40-42` enum + `:33` field + `:110` live dispatch | CONFIRMED |
| `ValueRef<G: EventGrammar = AnyGrammar>` phantom (G axis) | `tape/mod.rs:175` | CONFIRMED |
| warm micro-fixture path live; canonical kept | `nonjson_css_l4.rs:3091 measure_mbps` (48 hits); `css_canon_bench.rs` PRESENT | CONFIRMED |
| CSS NEON dead at admission | `find_css_significant` sole caller `lib.rs:574` (in `#[cfg(test)]` from `:51`); `count_top_level_commas` cold rich-summary `generated.rs:157→:810` | CONFIRMED |
| R16 nested `output_labels` | `RuntimeTarget` = 12 fields (`regen.rs`), `output_labels: Option<RuntimeOutputLabels>` 12th; `RuntimeOutputLabels` distinct struct `grammar_provider.rs:92-95` (`fact_schema`/`row_id`/`output_plane` nested) | CONFIRMED |
| P1 build-soundness coupling | `src/x86_64/`=24 files, `ext/x86/`=4, `nasm-rs="0.3"` `Cargo.toml:19`; 9 ACTIVE `bbnf_simd::x86_64::…_scalar(` sites `checkasm_parity.rs:458,464,467,477,478,484,493,497,502` | CONFIRMED |
| P5 metalang leak | `parse_w11_1_number` ×7 in shipped `json/generated.rs` | CONFIRMED |
| honest-finding escape gate (the named largest paper-close surface) | `SYNTHESIS.md:342` carries (a)-(c) AND self-names "the single largest paper-close surface in the contract"; PROVE-Sheets fallback `:337`/`:383` "do NOT stub-prove" | CONFIRMED |

Not one claim is fabricated, stale, or rounded past a gate threshold. The audit re-grepped at the
LIVE HEAD rather than the contract-snapshot `318d9c046` (CH1 §HEAD-anchoring) — the honest move.

## Dispositions

- **(1) Audit completeness — no silent wave-past (ACCEPT).** The residual census (SYNTHESIS §2,
  R1–R16 + R-A0-1/2/3) maps EVERY live overfit surface to a named PRUNE or GENERALIZE wave with a
  machine-checkable gate; zero orphan finding (CH1 confirms each row cites witness + wave + gate).
  Crucially the audit does NOT paper-close the case the contract leaves open: a0 §6 / SYNTHESIS
  R-A0-3 explicitly carries the honest-finding "named primitive" escape as a STANDING paper-close
  surface (not a closed one), gated machine-checked (a)-(c) — `grep` the primitive name in the
  `.bbnf`, grammar-derived parameter, `verbatim_blob_present == false` — rather than blessing the
  contract's prose-reviewed-at-admission backstop. The audit names its own deepest hole and bolts
  it, which is the anti-paper-close discipline at its load-bearing point.

- **(2a) The 6 addenda are EXECUTABLE, not decorative (ACCEPT).** a1 §L1–L6 gives each addendum a
  concrete grep/diff/md5/samply runnable from `skinny/crates/`, a telemetry column the `gate-json`
  consumer REJECTs on, and a LIVE witness it FIRES on today (re-verified above: all six fire — none
  is a dead lens written for show). The decisive anti-paper-close move is that md5-distinctness is
  declared NECESSARY-NOT-SUFFICIENT (a1 §L2 / SYNTHESIS §2.1): the addendum is a 3-co-gate
  conjunction (md5 ∧ `generator_grammar_branch_count==0` ∧ `generator_grammar_type_count==0` ∧
  `runtime_target_rows_collapsed==true`) so a courier swap that produces md5-distinct output cannot
  paper-close the "N distinct grammars" claim. The single-emitter lens (L3) likewise catches the
  fork BEHIND neutral enum names (`CompiledLowering`/`RequestFacts`, not `Json`/`Css`) — exactly
  the overfit-behind-an-abstraction a paper-close would hide.

- **(2b) The addenda correctly catch the V3 failure modes (ACCEPT).** Each lens is pinned to its
  originating V3 finding by path:line (a1 registry summary table): L1→D1 (`CONSOLIDATED-AUDIT.md:30-31`),
  L2→D1 (`:34`), L3→D1 (`:32`), L4→D2 (`:36`), L5→C3 (`:53`)+C2 (`:50`), L6→C1 (`:47`). The
  falsifiers are the precise inverse of each fake-generalization surface: L1's `.bbnf`-mutation
  test (a const courier cannot pass — mutate the grammar, the const does not change); L6's
  caller-census-excluding-tests (a checkasm-green kernel with only `#[cfg(test)]` callers is NOT an
  admitted acceleration). These are the tests that would have caught each surface BEFORE SK-V17
  shipped it — the addenda are the falsification battery, not a restatement.

- **(2c) The relocated-overfit-seam is closed STRUCTURALLY, not by regex (ACCEPT).** The subtlest
  paper-close — a per-grammar branch moved out of a `match grammar` arm into a neutral-identifier
  `RuntimeTarget` data-table, which the arm-census regex is syntactically incapable of firing on —
  is caught by the structural `runtime_target_rows_collapsed` check (a2 §4a, a1 §L2). a3's R16
  sharpens this one layer deeper: the by-exclusion projection must RECURSE into the nested
  `output_labels` struct (disk-confirmed: `RuntimeOutputLabels` is a distinct struct carrying the
  7 distinct `fact_schema`/`row_id`/`output_plane` values), or a shallow `Option`-discriminant
  compare false-greens. The audit pins the gate INVARIANT mechanism-agnostically (full-row collapse;
  `PartialEq` over the expanded row is ONE sufficient realization) so S-P3 cannot author a
  shallow-compare false-green. This is the necessary-not-sufficient lineage carried to its deepest
  disguised layer — the anti-paper-close core.

- **(3) PRUNE-sequencing is SOUND (ACCEPT).** The entry-gate dependency chain
  PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1 (a2 §2, SYNTHESIS §5) is binding: a wave that
  fails its exit gate BLOCKS every downstream wave (G1 failure blocks G2/G3/G4/PROVE; G3 un-fork
  failure blocks PROVE, which emits Sheets THROUGH the un-forked generator — so a surviving fork
  makes "one generator emits three grammars" structurally false, not merely unproven). Two
  load-bearing couplings are correctly bound and not paper-closed: P4-before-emitter-rebuild (the
  Lock-14 gate must be MEANINGFUL when the new emitter is authored, or a grammar-named branch
  re-enters under a blind gate — a2 §2 edge 1c) and P1↔`checkasm_parity.rs` build-soundness (the
  `src/x86_64/` deletion is build-BLOCKING without decoupling the 9 active call sites in the SAME
  wave; an intermediate `src/x86_64`-deleted/`checkasm`-coupled commit is a broken-build state —
  a2 §3, disk-confirmed `:458…:502`). The PROVE-Sheets fallback is the explicit anti-stub clause:
  "if Sheets cannot be emitted via the generator ONLY, generalization is NOT real — surface
  honestly, do NOT stub-prove" (`SYNTHESIS.md:337`/`:383`) — no scaffold-only landing counts.

## REVISE (2)

- **R1-CH6 (framing completion — the R-A0-1 OR-escape qualifier).** The H1 honesty disposition
  reads "re-frame the CSS >SOTA as lazy-rich-vs-eager-full-CSSOM **OR** add a symmetric
  materialization-depth comparator" (SYNTHESIS R-A0-1, a0 §4, a3 §F-A3.3). a0 §4 BINDS the
  guard correctly (the re-label-only branch is acceptable ONLY if the close report discloses the
  materialization-depth asymmetry explicitly and the word "beats" carries the asymmetry
  qualifier), but the SYNTHESIS §2 R-A0-1 row states only "(the symmetric-comparator branch
  preferred; re-label-only must disclose the asymmetry)" — it does not carry a0 §4's
  explicit prohibition on UNQUALIFIED "beats CSSOM"/"equal-work" language in the close report.
  This is the one residual anti-paper-close gap at the framing layer: an OR whose cheaper branch
  (re-label, no symmetric work) could close the honesty gate while an unqualified ">beats CSSOM"
  claim stands. Non-blocking — a0 §4 binds it; the SYNTHESIS row under-states it. Fold: add to the
  SYNTHESIS R-A0-1 disposition "the re-label branch is acceptable ONLY with the asymmetry disclosed
  explicitly; an unqualified 'beats CSSOM'/'equal-work' close-report claim behind a re-label is a
  REJECT (a0 §4)."

- **R2-CH6 (framing completion — the deferred P3 decision binding).** R-A0-2 (a0 §5, SYNTHESIS
  R-A0-2) correctly identifies the P3 collapse-vs-differentiate DECISION as DEFERRED to B2 on a
  RED-by-design gate — a genuine paper-close RISK if a downstream implementer mis-reads
  "preserve-profile-distinctness" as "keep 7 CSS files" and manufactures 7 fake `.bbnf` roots. a0
  §5 binds the disk-grounded answer (collapse-to-one: the 7 profiles share one `stylesheet.bbnf`
  and byte-identical output, so they ARE one grammar; differentiation into 7 fake sub-grammars is
  the EXACT overfit the addendum forbids). But the SYNTHESIS §2 R-A0-2 row defers the binding to
  S-P3 ("S-P3 must bind which branch each of the 7 profiles takes") WITHOUT stating the
  disk-grounded answer a0 §5 already reaches. Leaving S-P3 to re-derive it is a thin paper-close
  seam: S-P3 could bind "differentiate" and satisfy a distinctness gate by manufacturing roots.
  Non-blocking — a0 §5 reaches the answer. Fold: carry a0 §5's disk-grounded conclusion into the
  SYNTHESIS R-A0-2 row ("disk evidence is collapse-to-one — one `stylesheet.bbnf`, byte-identical
  output; `generator_grammar_count == 3` = json+css+sheets, NOT json+7-css+sheets; manufacturing
  7 fake roots to satisfy a distinctness gate is the overfit the addendum forbids").

## Anti-paper-close affirmative (what the audit refuses to bless)

1. It does NOT bless the `@generated` banner — L1 checks the BODY, not the provenance header
   (a const `&str` under a `@generated` banner is hand-written, REJECT).
2. It does NOT accept md5-distinctness as proof of N grammars — the 3-co-gate conjunction.
3. It does NOT accept a checkasm-green kernel as an admitted acceleration — caller-census excludes
   tests; the G6 retire branch is gated on a samply non-top-N MEASUREMENT (an S-P1 dependency),
   not an assertion.
4. It does NOT let the honest-finding escape become a courier-relabel hatch — (a)-(c)
   machine-checked, the escape is the abrogate-before-patch outcome ("can the generator lower it?"
   before "can we splice a primitive?"), not the default.
5. It does NOT stub-prove Sheets — the PROVE fallback surfaces an honest finding rather than
   hand-writing a `_GENERATED_RS` Sheets block.

No CRITICAL/HIGH NEW residual; the one NEW finding (R16) is MEDIUM and pins a gate-recipe
precision hazard rather than blessing a surface. The audit is complete, the addenda are
executable and V3-catching, the sequencing is sound.

## Tally
ACCEPT 5 · REVISE 2 · REJECT 0 — **71.4%**. Both REVISEs are framing completions at the
SYNTHESIS-row layer (carry a0 §4's "beats" qualifier and a0 §5's collapse-to-one disk answer up
into the consolidated R-A0-* rows); single-edit, non-architectural, anti-paper-close-positive,
no surface left blessed-but-unproven.

TALLY accept=5 revise=2 reject=0
