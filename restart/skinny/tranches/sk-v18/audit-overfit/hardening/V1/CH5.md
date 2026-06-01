# S-P0 audit-overfit hardening V1 — CH5 Hidden Coupling

Lens (CH5 HIDDEN-COUPLING, V1): does the residual-overfit audit surface every cross-surface
dependency the addenda gates rely on; can any gate read CLEAN only because it is silently
coupled to a leak-EXCLUDING scope; does any finding false-green another; is the
PRUNE-sequencing's revert/entry-gate graph a real dependency chain or an asserted order. Per
PASS-0-OVERFIT-AUDIT §3 + ORCHESTRATOR §3W. Subject: `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md`,
live HEAD `83b66db42`. Couplings re-grepped on disk this pass.

## Disposition ledger

### ACCEPT (5)

- **The Lock-14 green-by-exclusion coupling — A3/R9/P4 (ACCEPT — well surfaced).** This is the
  canonical hidden coupling and the audit names it exactly: the gate reads CLEAN
  (`accepts_current_allowlist` 2/0) only because `GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`)
  is coupled to a leak-EXCLUDING scan-root set — `runtime_generator.rs` routed to the weaker
  `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442`, the x86 tree tagged `"diagnostic-x86":2463`. a0 §3/a2 §1
  (P4) / a3 §2 / SYNTHESIS §3-A3 surface it AND bind de-coupling (move the leak files into the
  strict root) AND a meaningfulness falsifier (re-inject a `JsonSink` token → gate RED, then
  revert; `lock14_gate_scans_codegen == true`). A green gate over standing leaks named "worse
  than a red one" (a2 §1 P4). No hidden coupling left un-policed.

- **P4-before-rebuild sequencing coupling (ACCEPT).** SYNTHESIS §5 fact 2 / a2 §2 (1c) surface
  that the gate's TRUSTWORTHINESS is coupled to its landing ORDER, not just its content: P4 MUST
  land before G2/G3 so the un-forked emitter is scanned for neutrality AS it is authored. A gate
  that becomes meaningful only AFTER the rebuild it is meant to police is a sequencing coupling;
  the audit binds it as an entry-gate (1c), "not a preference" (a2 §2). Correct.

- **S-P1-profile cross-pass coupling for G6 (ACCEPT).** SYNTHESIS §5 fact 4 / a0 §1-L6 / a1 §L6
  check (d) surface that the G6 NEON RETIRE branch is coupled to an S-P1 samply non-top-N
  MEASUREMENT — a cross-pass dependency that, if missed, would let G6 close by assertion
  ("retire" grounded in a reading, not an admission-time claim). Bound as profile-FIRST
  (actual-profiling); the kernel-with-its-consumer-in-one-commit clause (no orphan kernel)
  closes the inverse coupling. Correct.

- **No-second-substrate coupling — Lock 1 / G4 (ACCEPT).** a2 §5 (4c) / SYNTHESIS CLEAN-list
  surface that the G3/G4 emit must ride the EXISTING `Tape`/`ValueRef`; an introduced
  `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed tape is a Lock-1
  type-ambivalence coupling (REJECT). The trait's existence held INDEPENDENT of the `<G>`
  phantom (deleting `<G>` and defining the trait are separable, a2 §5/4a) — this severs a coupling
  that would otherwise "manufacture the very phantom being deleted." Correct.

- **P3↔G3 PRUNE-binds-GENERALIZE coupling + the relocated-seam structural gate (ACCEPT).** a2
  §4/§4a / a0 §5 / SYNTHESIS §5 fact 3 surface the deepest non-obvious coupling: P3 (a PRUNE
  item) is NOT independent of G3 (a GENERALIZE item) — the `runtime_target_rows_collapsed`
  structural check is the ONLY mechanism that catches a per-grammar branch RELOCATED into a
  neutral-identifier `RuntimeTarget` data-table (the arm-census regex is syntactically incapable,
  CH2 V3 §8.1). The audit binds md5-distinctness as necessary-not-sufficient and the by-exclusion
  full-tuple collapse as the structural co-gate. This is the textbook hidden coupling (overfit
  hidden behind an abstraction-shaped data table) surfaced and gated. Correct.

### REVISE (1)

- **R1-CH5 — the R16 recipe-pin's named-nested-struct enumeration is itself a hidden coupling
  (REVISE, non-blocking).** `a3-arch-measurement-gate-residual.md:169-179` + SYNTHESIS §5 fact 5
  pin the `runtime_target_rows_collapsed` machine-check to "compare the full expanded row
  including nested `output_labels`, NOT the prose's 3 named pseudo-fields." The INVARIANT a3 §3
  states ("every nested field inlined ... MINUS the path columns") is correct and
  mechanism-agnostic. But the RECIPE prose names exactly ONE nested struct (`output_labels`),
  while the live `RuntimeTarget` has TWO nested-struct fields, BOTH reachable through a top-level
  field that the by-exclusion set includes:
  - `frontend_requirements: codegen::RuntimeFrontendRequirements` — field #11
    (`regen.rs:17`); the struct is distinct at `grammar_provider.rs:46`.
  - `output_labels: Option<codegen::RuntimeOutputLabels>` — field #12 (`regen.rs:18`); struct at
    `grammar_provider.rs:92`.

  a2 §4a's operative-set enumeration DOES list `frontend_requirements` (a2 line 321), so the
  by-exclusion INVARIANT covers it — but a3's recipe-pin prose (the surface S-P3 reads when
  authoring the consumer) recurses into `output_labels` ONLY. A recipe author following the a3
  prose literally would inline one nested struct and not the other — the EXACT shallow-compare
  false-green a3 §3 exists to prevent, displaced one field over. Today
  `frontend_requirements == REQUEST_FACTS_REQUIREMENTS` across all 7 css_l4 rows
  (`regen_css.rs:47…155`), so it is not a LIVE divergence vector — but the recipe pin's purpose
  is to forbid a FUTURE relocated seam, and a seam riding `frontend_requirements` would slip a
  one-nested-struct recipe. The recipe pin's named-field coupling is narrower than its own
  invariant.

  Non-blocking. Fold (single edit to a3 §3 / SYNTHESIS §5 fact 5): generalize the recipe pin
  from "recurse into `output_labels`" to "inline EVERY nested-struct field (`frontend_requirements`
  AND `output_labels`)" — i.e. state the recipe at the invariant's altitude (full expanded row),
  not at one named nested struct. The audit ALSO under-states the cleanest sufficient mechanism's
  coupling cost: `RuntimeTarget` derives only `#[derive(Clone, Copy, Debug)]` (`regen.rs:5`), NOT
  `PartialEq`, so the a3 §3 "`RuntimeTarget: PartialEq` full-row collapse is ONE sufficient
  mechanism" requires ADDING the derive — verified viable on disk (both nested structs and the
  `&'static [&'static str]` slice fields derive/support `PartialEq`: `RuntimeOutputLabels`
  +`RuntimeFrontendRequirements` both `#[derive(..., PartialEq, Eq)]`). The full-row derive is
  preferable precisely BECAUSE it covers both nested structs automatically and cannot be
  coupled to a hand-rolled field list; note the one-line derive addition as the pin's cost.

## Hidden-coupling sweep — couplings the audit did NOT miss (affirmative)

- The witness-emission scan-root coupling (`JsonEventGrammar`/`SheetsEventGrammar` live in
  `runtime/`, NOT the P4 codegen scan root — disk-confirmed: both witnesses under
  `crates/runtime/src/grammars/{json,sheets_witness}/event_grammar_witness.rs`) is surfaced as
  SYNTHESIS §2.1 obligation 2 + a1 §L3 + the `FORBIDDEN_GENERIC_TOKENS` `EventGrammar`/`*EventGrammar`
  extension (a2 §1 P4). The prior CH5 cycle's R1-CH5 flagged this; it is now folded into
  SYNTHESIS §2.1 — discharged. NOT re-raised.
- The build-soundness coupling P1↔`checkasm_parity.rs` (deleting `src/x86_64/` breaks the build via
  9 active compile-coupled call sites `:458…:502`) is a hidden coupling fully surfaced (a2 §3,
  a3 §2): same-commit decouple, exit gate `cargo test --no-run` clean — no intermediate
  broken-build state. NOT a residual.
- The JSON-richness↔`<G>`-resolution coupling (`json_rich_navigation_preserved == true` as a
  SEPARATE checked condition a ≥2-impl count does NOT imply) is surfaced as the preserve-rich-ast
  guard (a1 §L4, a2 §5/4b) — the coupling that a phantom "resolution" could LCD-flatten JSON is
  severed. NOT a residual.

## Tally

ACCEPT 5 · REVISE 1 · REJECT 0 — **83.3%**. Every cross-surface dependency the addenda gates
rely on is surfaced and de-coupled or entry-gated; zero gate reads CLEAN by a coupling left
un-policed. The single REVISE sharpens the R16 recipe-pin so its named-field enumeration matches
its own invariant altitude (both nested structs, not one) and names the one-line `PartialEq`-derive
cost of the cleanest sufficient mechanism. Single-edit, non-architectural, no REJECT.

TALLY accept=5 revise=1 reject=0
