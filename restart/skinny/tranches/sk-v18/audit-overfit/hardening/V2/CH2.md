# S-P0 audit-overfit hardening V2 — CH2 Generality (post-fold confirm)

Lens (CH2 GENERALITY): the 6 addenda + the PRUNE-list + the residual census GENERALIZE
correctly — no addendum is over-fit to TODAY'S literal surface; each gate catches the
RELOCATED / DISGUISED form, not only the witnessed one; the PRUNE-sequencing couplings are
general (build-soundness, exit-gate-blocks-successor, dual-entry-gate) rather than hand-tuned to
one wave. V2 is the POST-FOLD confirm of V1's single CH2 REVISE (R1-CH2 — the L1 honest-finding
escape's (b) parameterization predicate must be machine-grounded as a per-primitive
mutate-falsifier, not prose). Subject re-grepped INDEPENDENTLY this pass at HEAD `83b66db42`
(`git rev-parse HEAD` = `83b66db4232374db6a5f9fa009882f41acc04342`); every disposition
disk-true (§Verification).

## Verification (the generality spine is disk-true at `83b66db42`)

Re-grepped the structural facts the CH2 dispositions turn on, independently this cycle:
- `skinny/xtask/src/regen.rs:6-19` — `RuntimeTarget` is EXACTLY 12 fields; the two path columns
  are `output_dir:11` + `expected_files:16`; the TWO nested-struct fields are
  `frontend_requirements:17` (`RuntimeFrontendRequirements`) AND `output_labels:18`
  (`Option<RuntimeOutputLabels>`). `regen.rs:5` derives ONLY `Clone, Copy, Debug` — NOT
  `PartialEq`. ✓ (this is the crux of the R16/CH5 fold: the one-line derive add is the recipe's
  real, disk-true cost, and it is the only mechanism that covers BOTH nests by construction.)
- `grammar_provider.rs:45` `RuntimeFrontendRequirements` + `:91` `RuntimeOutputLabels` BOTH derive
  `Clone, Copy, Debug, PartialEq, Eq` — so the full-row `RuntimeTarget: PartialEq` is viable. ✓
- `grammar_provider.rs:92-95` — `RuntimeOutputLabels` carries `fact_schema:93`/`row_id:94`/
  `output_plane:95` as a DISTINCT nested struct. ✓ (the shallow-compare false-green hazard R16
  names is real — these are NOT top-level fields.)
- `grammar_provider.rs:40-42` — `RuntimeEmitterKind{CompiledLowering, RequestFacts}` with NEUTRAL
  variant names; `:33` selector field; `:110` live dispatch. ✓ (confirms L3's generality point:
  the L2 arm-census regex is syntactically incapable of firing on the neutral names.)
- `tape/mod.rs:175` — `ValueRef<'doc,'input, K = AnyKind, G: EventGrammar = AnyGrammar>`. ✓ (`G`
  is the phantom; `K` is the real axis — L4 axis-precision confirmed.)
- `runtime_generator.rs:701`→`:1611` — `const CSS_GENERATED_RS: &str = r#"…"#;` body span = 910
  LOC (open `:701`, close `"#;` at `:1611`). ✓ (no gate keys on the LOC; descriptive only.)
- 7 `css_l4_*/generated.rs` share one md5 `b654562ccff46ed62dd48e9ace325830` (verified
  `uniq -c` = 7). ✓
- `regen_css.rs` — `frontend_requirements:.*REQUEST_FACTS_REQUIREMENTS` count = 7 (uniform across
  all CSS rows). ✓ (confirms the CH5 framing: `frontend_requirements` is not yet a LIVE
  divergence vector, but a POLICED future one — the recipe pin must hold the future altitude.)

## V1 REVISE discharge (R1-CH2)

DISCHARGED on disk. `a1-six-addenda-lens-registry.md:121-128` now restates the L1 escape's (b)
predicate as a MACHINE per-primitive mutate-falsifier (independently re-read this pass): "(b)
the primitive's EMITTED OUTPUT VARIES correspondingly under a `.bbnf` mutation of the invoking
rule's shape — i.e. apply the same per-primitive mutate-falsifier … mutate the invoking rule,
regen, and the primitive's emitted body MUST change; a fixed body keyed off a merely-decorative
grammar-derived argument FAILS (b) exactly as a const courier fails the whole-path test, so
'accepts a grammar-derived argument' is NOT sufficient." The entry closes: "All three are
MACHINE predicates (grep · mutate + regen-diff · telemetry column) — none is
prose-reviewed-at-admission." This is EXACTLY the generalization R1-CH2 demanded: the (b)
predicate now catches the fixed-body-with-decorative-argument disguise that the old "accepts
grammar-derived DATA" prose could not falsify; (b) is now at the same machine altitude as (a)
(grep the `.bbnf`) and (c) (`verbatim_blob_present`). The fold is mirrored in `a0:112-118` (§1-L1)
and `a0:417-429` (§6 binding item 1). Single-sentence sharpening of a NEW-finding-adjacent escape
predicate, non-architectural — DISCHARGED, not carried.

## Dispositions (re-verified independently)

- **a1 §L2 distinct-grammar-output 3-co-gate CONJUNCTION (ACCEPT — the load-bearing generality
  move).** `a1:120-200` hardens distinct-output from md5 into {md5-distinct ∧
  `generator_grammar_branch_count==0` ∧ `generator_grammar_type_count==0` ∧
  `runtime_target_rows_collapsed`} — catching the literal replica (md5 census), the
  grammar-branching emitter body (arm census), the re-emitted grammar-named TYPE (type census),
  AND the relocated neutral-identifier data-table (structural row-collapse). The explicit
  "md5-distinctness is necessary-not-sufficient" statement (`a1:158-162`, `a0:120-124`) is the
  correct anti-overfit framing: an md5-only gate would be over-fit to today's literal-replica
  form; the conjunction is not. Unchanged by the fold; still general.

- **a1 §L3 single-emitter neutral-name generalization (ACCEPT).** `a1:213-249` correctly
  establishes the fork is caught BY THE DISPATCH, not the spelling — the `RuntimeEmitterKind`
  variants are neutral (`CompiledLowering`/`RequestFacts`), the L2 arm-census grep is CLEAN on
  them (disk-verified `grammar_provider.rs:40-42`), so L3 is a DISTINCT lens with its own
  enumerate-emitter-discriminator check AND reuses `runtime_target_rows_collapsed` for a fork
  relocated into the data-table. "The neutral name is NOT a defense" (`a1:255`) is the general
  invariant. Unchanged.

- **a1 §L4 / a2 §5 phantom-generic axis-precision (ACCEPT).** Points the phantom lens at the `G`
  (EventGrammar) axis NOT the `K` (Kind) axis (disk-confirmed `tape/mod.rs:175`), test-excludes
  `_proof_compiles`, and carries the SEPARATE `json_rich_navigation_preserved` condition so a
  naive "≥2 impls = resolved" count cannot LCD-flatten JSON's rich navigation. A naive "delete
  every generic" lens would over-prune the live `K`; a naive "≥2 impls" lens would under-catch
  the LCD-flatten. The instantiate-or-DELETE (DELETE-default) framing is the general
  abrogate-before-patch form. Unchanged.

- **R16 nested-struct recipe-pin, at FULL-EXPANDED-ROW altitude (ACCEPT — broader, more general
  after the CH5 fold).** The R1-CH5 fold raised the recipe from "recurse into `output_labels`" to
  "inline EVERY nested-struct field (`frontend_requirements` #11 AND `output_labels` #12)
  MINUS only the path columns" — disk-re-verified at a3 `:170-208` + SYNTHESIS `:209-219` + a2
  `:366-384` + a0 `:132-146`, all stated at the invariant altitude. This is a generality
  POSITIVE: the recipe pin now cannot be slipped by a future seam riding `frontend_requirements`
  (today uniform `REQUEST_FACTS_REQUIREMENTS` ×7, so the pin guards a FUTURE relocated seam — the
  correct generality altitude). The gate is stated mechanism-agnostically (the INVARIANT is the
  full-expanded-row collapse; `RuntimeTarget: PartialEq`, serialize-then-hash, or `jq` are all
  sufficient; only the shallow-compare-of-either-nest is forbidden) — the correct CH2 form that
  lets S-P3 pick the realization.

- **a2 §3 build-soundness coupling generalizes the deletion gate (ACCEPT).** `a2:238-279` binds
  P1's exit gate to `cargo build` + `cargo test --no-run` clean, not to `find … -type f = 0`,
  because the verify grep fires on 9 ACTIVE compile-coupled `checkasm_parity.rs` sites
  (`:458…:502`). The general invariant "a deletion list narrower than the verify grep ships a
  RED-by-construction gate" is a re-entry-resistant form, not a hand-list of today's files; the
  DECOUPLE-not-DELETE of the aarch64 parity assertions shows the prune does not over-generalize
  ("throw the aarch64 hardening out with the x86 bathwater"). Unchanged.

- **a2 §2/§4 sequencing as an entry-gate dependency GRAPH (ACCEPT).** The R1-CH3 directional fold
  ("G1/G3 co-derive; G3-failure blocks PROVE") makes the arrow general — a forward
  revert/precondition claim, not a backward "G3 gates G1/G2". Re-read a2 §4 title (`:282`) + §0
  item 3 (`:11-16`) + §7 (`:497`) + §8 (`:519-525`): all consistent on the forward arrow, and the
  dual-entry-gate (G2 entry-gates on BOTH G1 AND P3) is stated at SYNTHESIS `:204-205`. This is the
  general form: any wave failing its exit gate blocks every downstream wave that entry-gates on
  it — re-derivable, not hand-sequenced.

## Generality cross-checks (no over-fit, no under-catch)

- No addendum over-fits a single surface: L1 catches any `const \w*_RS: &str = r#"` REGARDLESS of
  name; L2 catches branch/type/table disguises; L3 the neutral-named fork; L4 the
  test-only/LCD-flatten disguises; L5 warm/micro-fixture/more-work independently; L6
  cfg-test-only/mislabel/orphan independently. Each lens enumerates its DISGUISE SET, not its
  single witness.
- No gate is satisfiable by erasure: P3's collapse FORBIDS erasing legitimate `profile`
  distinctness (a2 `:360-364`, a0 `:353-355`); the R-A0-2 fold pins collapse-to-one
  (`generator_grammar_count == 3` = json+css+sheets) and names "7 fake roots" as the forbidden
  overfit (a0 `:376-387`) — the general anti-erasure posture.
- The one residual-overfit risk surviving into GENERALIZE (the relocated-seam) is policed
  STRUCTURALLY (row-collapse over the full-expanded row), not by the regex that is "syntactically
  incapable" of it — the generality spine is correctly carried, and R16 sharpens it to recurse
  into BOTH nests.

## REVISE (0) / REJECT (0)

The single V1 CH2 REVISE (R1-CH2, the L1 (b) parameterization predicate) is DISCHARGED on disk
(a1 `:121-128`; mirrored a0 `:112-118`/`:417-429`) as a per-primitive mutate-falsifier MACHINE
check. No addendum over-fits and no gate under-catches after the folds — each fold RAISED
generality (the (b) machine predicate, the full-expanded-row recipe inlining both nests, the
directional revert arrow). Zero orphan REVISE: the prior cycle's R16 mechanism-agnosticism and
the R1-CH2 (b) predicate are both resolved on disk, not re-raised.

## Tally
ACCEPT 6 · REVISE 0 · REJECT 0 — **100%**. R1-CH2 DISCHARGED (the L1 (b) predicate is now a
per-primitive mutate-falsifier MACHINE check, disk-verified at a1 `:121-128`); the V1→V2 folds
raised generality across L1/R16/sequencing; zero orphan REVISE. Two-consecutive ≥95% met (V1
87.5% → V2 100% is the convergence transition; the single REVISE was non-architectural and is
discharged, so V2 closes the CH2 lens at 100%).

TALLY accept=6 revise=0 reject=0
