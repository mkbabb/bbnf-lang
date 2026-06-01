# S-P0 audit-overfit hardening V1 — CH2 Generality

Lens (CH2 GENERALITY): the 6 addenda + the PRUNE-list + the residual census generalize
correctly — no addendum is over-fit to TODAY'S literal surface; each gate catches the
RELOCATED / DISGUISED form, not only the witnessed one; the PRUNE-sequencing is sound and
its couplings are general (build-soundness, exit-gate-blocks-successor) rather than
hand-tuned to one wave. Reviewing `a0`/`a1`/`a2`/`a3` + `SYNTHESIS-AUDIT-OVERFIT.md` per
PASS-0-OVERFIT-AUDIT §3 + ORCHESTRATOR §3W. Every disposition disk-re-verified at the
live tree (see §Verification).

## Verification (the generality spine is disk-true)

Re-grepped the load-bearing structural facts the CH2 dispositions turn on:
- `skinny/xtask/src/regen.rs:6-18` — `RuntimeTarget` is EXACTLY 12 fields;
  `output_dir:11` + `expected_files:16` are the two path columns;
  `output_labels: Option<codegen::RuntimeOutputLabels>` is field #12 at `:18`. ✓
- `skinny/crates/codegen/src/grammar_provider.rs:92-95` — `RuntimeOutputLabels` is a
  DISTINCT nested struct carrying `fact_schema:93`/`row_id:94`/`output_plane:95`. ✓
  (This is the crux of R16: the 3 prose-named fields ARE nested, not top-level — the
  shallow-compare false-green hazard is real, not theoretical.)
- `grammar_provider.rs:40-42` — `RuntimeEmitterKind{CompiledLowering,RequestFacts}` with
  NEUTRAL variant names; `:110` dispatches on it. ✓ (confirms L3's generality point: the
  L2 arm-census regex is syntactically incapable of firing on the neutral names.)
- `tape/mod.rs:175` — `ValueRef<'doc,'input, K = AnyKind, G: EventGrammar = AnyGrammar>`. ✓
  (confirms L4 axis-precision: `G` is the phantom, `K` is the real axis.)

## Dispositions

- **a1 §L2 distinct-grammar-output as a co-gate CONJUNCTION (ACCEPT — the load-bearing
  generality move).** `a1-six-addenda-lens-registry.md:120-185` hardens distinct-output
  from md5 into {md5-distinct ∧ `generator_grammar_branch_count==0` ∧
  `generator_grammar_type_count==0` ∧ `runtime_target_rows_collapsed`}. This is the correct
  generalization: it catches the literal replica (md5 census, check (a)), the
  grammar-branching emitter body (arm census, check (b)), the re-emitted grammar-named TYPE
  (type census), AND the relocated neutral-identifier data-table (structural row-collapse,
  check (c)) — four disguises, one conjunction, with the explicit and correct statement
  (`a1:158-162`, `a0:99-101`) that md5-distinctness is "necessary-not-sufficient" and the
  arm-census regex is "syntactically incapable of firing on a neutral-identifier table." An
  md5-only gate would be over-fit to today's literal-replica form; the conjunction is not.

- **a1 §L3 single-emitter-path neutral-name generalization (ACCEPT).** `a1:188-249`
  correctly establishes the fork is caught BY THE DISPATCH, not the spelling: the
  `RuntimeEmitterKind` variants are neutral (`CompiledLowering`/`RequestFacts`), the L2
  arm-census grep is CLEAN on them (`a1:213-215`), so L3 is a DISTINCT lens with its own
  enumerate-emitter-kind-discriminator check (`a1:218-224`) AND reuses the
  `runtime_target_rows_collapsed` structural gate for a fork relocated into the data-table
  (`a1:226-229`, `a0:120-122`). "The neutral name is NOT a defense" (`a1:239`) is the
  general invariant. Disk-verified at `grammar_provider.rs:40-42`.

- **a1 §L4 / a2 §5 phantom-generic axis-precision (ACCEPT).** Pointing the phantom lens at
  the `G` (EventGrammar) axis NOT the `K` (Kind) axis (`a1:273-276`, `a2:357-363`,
  disk-confirmed `tape/mod.rs:175`), with test-exclusion of `_proof_compiles`
  (`a1:290-296`, the F6 exclusion mirror) AND the SEPARATE `json_rich_navigation_preserved`
  condition (`a1:316-321`, `a2:380-390`), generalizes correctly: it catches the decorative
  `<G>`, the test-only false-instantiation, AND the LCD-flatten regression that a naive
  "≥2 impls = resolved" count would admit — while NOT destroying the REAL `K` axis. A naive
  "delete every generic" lens would over-prune the live `K`; a naive "≥2 impls" lens would
  under-catch the LCD-flatten. The instantiate-or-DELETE (DELETE-default) framing is the
  general abrogate-before-patch form, not a one-surface patch.

- **a3 §3 / R16 nested-`output_labels` recipe finding, stated mechanism-agnostically
  (ACCEPT).** This is the NEW finding and the deepest generality layer. `a3:117-194`
  correctly identifies that `fact_schema`/`row_id`/`output_plane` are NESTED in
  `RuntimeOutputLabels` (disk-confirmed `grammar_provider.rs:92-95`), so an implementer
  authoring the machine-check from the PROSE's 3 named pseudo-fields could compare
  `output_labels` SHALLOWLY (by `Option` discriminant) and miss the 7 distinct nested values
  — a false-green of exactly the class the gate exists to prevent. Crucially, `a3:180-188`
  states the GATE as an INVARIANT — "the per-`grammar_name` config-tuple is over the
  FULLY-EXPANDED row (every nested field inlined) MINUS the path columns" — and explicitly
  enumerates THREE sufficient mechanisms (`derive(PartialEq)`, serialize-then-hash, `jq`),
  with the forbidden form being the shallow compare. This is the mechanism-agnostic
  generalization that lets S-P3 pick the realization; it is the correct CH2 form. (The prior
  CH2 cycle's R1-CH2 REVISE — "restate the invariant mechanism-independent of derive" — is
  ALREADY FOLDED into a3 `:180-188` as the "CH2 fold" annotation; it is RESOLVED on disk,
  not a fresh open REVISE.)

- **a2 §3 build-soundness coupling generalizes the deletion gate (ACCEPT).** `a2:219-260`
  binds P1's exit gate to `cargo build` + `cargo test --no-run` clean, not to
  `find … -type f = 0`, because the verify grep fires on 9 ACTIVE compile-coupled
  `checkasm_parity.rs` sites (`:458…:502`, disk-cited verbatim). This is the GENERAL
  reach-matched-deletion-list invariant ("a deletion list narrower than the verify grep
  ships a RED-by-construction gate") — a re-entry-resistant form, not a hand-list of today's
  files. The DECOUPLE-not-DELETE of the aarch64 parity assertions (retain `checkasm_common`
  + 12 single-kernel differentials) shows the prune does not over-generalize ("throw the
  aarch64 hardening out with the x86 bathwater," `a0:415`).

- **a2 §2 sequencing as an entry-gate dependency GRAPH, not a wave checklist (ACCEPT).**
  `a2:180-216` + SYNTHESIS §5 state PRUNE→GENERALIZE→PROVE as an explicit dependency graph
  with the general "exit-gate-blocks-successor" clause (`a2:206-211`) and the dual entry-gate
  (G2 gates on BOTH G1 AND P3, SYNTHESIS `:203-204`). This is the general form: any wave
  failing its exit gate blocks every downstream wave that entry-gates on it — re-derivable,
  not hand-sequenced. The P4-before-emitter-rebuild edge (`a2:200-204`) is the load-bearing
  generality edge: the Lock-14 gate must be MEANINGFUL when the new emitter is authored, or a
  grammar-named branch re-enters under a blind gate. This generalizes the gate's purpose
  beyond "today's leak surface" to "any future re-leak."

- **PRUNE-list + R-A0-* framing residuals carry general falsifiers (ACCEPT).** Each prune
  states a deletion + a falsifier that generalizes: P1's CRATE-WIDE grep (not `src/`-scoped —
  `a2:95-99`, catching the `ext/x86/` sibling + crate-root `build.rs`/`Cargo.toml`); P4's
  re-inject-`JsonSink`-token meaningfulness proof (not "gate green" — `a2:153-157`); P3's
  collapse-to-ONE bound to disk evidence (one `stylesheet.bbnf`, byte-identical output —
  `a0:307-327`) with the explicit anti-overfit guard that "differentiate into 7 fake
  sub-grammars to satisfy a distinctness gate would be the EXACT overfit the addendum forbids"
  (`a0:318-320`). The R-A0-1/2/3 framing residuals (`a0:241-368`) correctly generalize the
  paper-close seams the addenda police — the lazy-vs-eager `OR`-escape (R-A0-1), the deferred
  collapse-decision on a RED-by-design gate (R-A0-2), the prose-reviewed honest-finding escape
  bound (a)-(c) machine-checked (R-A0-3) — each is the general escape form, not a
  one-instance flag.

## REVISE (1)

- **R1-CH2 (a1 §L1 honest-finding escape — generalize the (b) parameterization predicate
  beyond a one-shot grep).** `a1-six-addenda-lens-registry.md:108-116` states the L1 REVISE
  escape as (a) the `.bbnf` invokes the primitive by name; (b) "it is parameterized by
  grammar-derived DATA, not a fixed body"; (c) `verbatim_blob_present == false`. The (a) and
  (c) predicates are machine-grounded (grep the `.bbnf`; the telemetry column). But (b)
  "parameterized by grammar-derived DATA" is stated only as prose — and a0 §6 (`a0:354`)
  correctly flags that the honest-finding escape is "the single largest paper-close surface"
  whose gate is "prose-reviewed-at-admission." The (b) predicate as written is the SAME
  prose-review hazard one level down: a primitive can accept a grammar-derived ARGUMENT yet
  still splice a fixed body keyed off it (the argument is decorative, the body is verbatim) —
  which (b) does not, as stated, falsify. For generality the audit should pin (b) as a
  MACHINE predicate independent of the specific primitive: the primitive's emitted body must
  VARY under a `.bbnf` mutation of the invoking rule's shape (the same mutate-the-`.bbnf`
  falsifier a0 §1 L1 already binds for the whole emit path — `a0:81-82` — applied to the
  primitive's output in isolation), so a fixed-body-with-decorative-argument fails (b) the
  same way a const courier fails the whole-path test. Fold: restate L1's (b) as "the
  primitive's EMITTED OUTPUT changes correspondingly under a `.bbnf` mutation of the invoking
  rule (the per-primitive mutate-falsifier), not merely 'accepts a grammar-derived
  argument'." This is a single-sentence sharpening of a NEW-finding-adjacent escape predicate,
  non-architectural; the (a)/(c) machine-checks and the §6 (a)-(c)-machine-checked binding are
  already correct — the gap is only that (b) is the one of the three still expressible as
  prose, and a0 §6's own "machine-checked not prose-reviewed" binding (`a0:351-357`) should
  reach (b) explicitly.

## Generality cross-checks (no over-fit, no under-catch)

- No addendum is over-fit to a single surface: L1 catches any `const \w*_RS: &str = r#"`
  REGARDLESS of name (`a1:104-106`), L2 catches branch/type/table disguises, L3 catches the
  neutral-named fork, L4 catches the test-only/LCD-flatten disguises, L5 catches
  warm/micro-fixture/more-work independently, L6 catches cfg-test-only/mislabel/orphan
  independently. Each lens enumerates its DISGUISE SET, not its single witness.
- No gate is satisfiable by erasure: P3's collapse gate explicitly FORBIDS erasing legitimate
  `profile` distinctness (`a0:294-296`, `a2:333-337`) — the general anti-erasure posture.
- The one residual-overfit risk surviving into GENERALIZE (the relocated-seam) is policed
  STRUCTURALLY (row-collapse), not by the regex that is "syntactically incapable" of it — the
  generality spine is correctly carried, and R16 sharpens it to recurse into the nest.

## Tally
ACCEPT 7 · REVISE 1 · REJECT 0 — **87.5%**. The single REVISE is a one-sentence
machine-grounding of the L1 escape's (b) parameterization predicate (the residual prose-review
seam a0 §6 already names but L1's (b) does not yet reach); it is non-architectural, the gate
INVARIANTS are already correct, and it is fully foldable by S-P3 before the first G1/G2 admit.
The prior cycle's R16 mechanism-agnosticism REVISE is RESOLVED on disk (a3 §3 "CH2 fold"),
not carried — so this is not an orphan-REVISE re-raise.

TALLY accept=7 revise=1 reject=0
