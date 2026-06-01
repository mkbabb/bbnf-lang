# S-P0 audit-overfit hardening V2 — CH5 Hidden Coupling (post-fold confirm)

Lens (CH5 HIDDEN-COUPLING, V2): does the residual-overfit audit surface every cross-surface
dependency the addenda gates rely on; can any gate read CLEAN only because it is silently coupled
to a leak-EXCLUDING scope; does any finding false-green another; is the PRUNE-sequencing's
revert/entry-gate graph a real dependency chain. V2 is the POST-FOLD confirm: the single V1 CH5
REVISE (R1-CH5 — the R16 recipe-pin's named-nested-struct enumeration is itself a hidden coupling:
it named ONE nested struct while `RuntimeTarget` has TWO, and the cleanest mechanism's
`PartialEq`-derive cost was unstated) must be DISCHARGED. Subject: `a0`–`a3` +
`SYNTHESIS-AUDIT-OVERFIT.md`. Every coupling and the discharge re-grepped on disk this pass at live
HEAD `83b66db42`.

## Disk re-verification (this pass — every CH5 load-bearing claim grepped on disk)

- `RuntimeTarget` = 12 fields, derives ONLY `#[derive(Clone, Copy, Debug)]` — `regen.rs:5-19`
  (verified verbatim). `frontend_requirements: codegen::RuntimeFrontendRequirements` = field #11
  (`regen.rs:17`); `output_labels: Option<codegen::RuntimeOutputLabels>` = field #12 (`regen.rs:18`).
  TWO nested-struct fields, both in the by-exclusion set (only `output_dir`/`expected_files`
  excluded). CONFIRMED — `PartialEq` derive is genuinely ABSENT, so the full-row mechanism's
  one-line cost is real.
- Both nested structs derive `Clone, Copy, Debug, PartialEq, Eq` — `RuntimeFrontendRequirements`
  derive `grammar_provider.rs:45` (struct `:46`); `RuntimeOutputLabels` derive `grammar_provider.rs:91`
  (struct `:92`, carrying `fact_schema`/`row_id`/`output_plane` `:93-95`). CONFIRMED — the full-row
  `PartialEq` realization is viable (`&'static str`/`&'static [&'static str]` field types support it).
- `RuntimeEmitterKind = {CompiledLowering, RequestFacts}` grammar-family fork — `grammar_provider.rs:40-42`
  (L3/R3/G3). CONFIRMED.
- Phantom `G` axis — `tape/mod.rs:175 ValueRef<'doc,'input, K = AnyKind, G: EventGrammar = AnyGrammar>`;
  the only `G` instantiations are test-only `_proof_compiles::<JsonEventGrammar>/::<SheetsEventGrammar>`
  at `tape/event_grammar_tests.rs:18-21` (L4/R5/G4). CONFIRMED.
- Lock-14 green-by-exclusion roots — `lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS`, `:2420
  FORBIDDEN_GENERIC_TOKENS`, `:2442 SKV15_W2_EXTRA_COVERAGE_ROOTS`, `:2463 ("…/x86_64","diagnostic-x86")`;
  the loop at `:2511` chains the WEAK set, so `runtime_generator.rs` rides the leak-excluding root
  (R9/P4). CONFIRMED.
- Witness-emission scan-root coupling — `JsonEventGrammar`/`SheetsEventGrammar` witnesses live under
  `crates/runtime/src/grammars/{json,sheets_witness}/event_grammar_witness.rs`, NOT the codegen P4
  scan root. CONFIRMED — the `FORBIDDEN_GENERIC_TOKENS` `EventGrammar`/`*EventGrammar` extension is
  genuinely load-bearing, not decorative.

## V1 REVISE discharge (R1-CH5)

DISCHARGED in BOTH parts, confirmed at every surface the V1 lens named:

- **The recipe pin now enumerates BOTH nested-struct fields, at the invariant's altitude.**
  `a3-arch-measurement-gate-residual.md:169-182` (§3 item 3) states verbatim: "The machine-check
  MUST recurse into EVERY nested-struct field, not just `output_labels`. `RuntimeTarget` carries TWO
  nested-struct fields … `frontend_requirements: RuntimeFrontendRequirements` (field #11, `regen.rs:17`;
  struct at `grammar_provider.rs:46`) AND `output_labels: Option<RuntimeOutputLabels>` (field #12,
  `regen.rs:18`; struct at `grammar_provider.rs:92`). A recipe author who recurses into `output_labels`
  ONLY … would inline one nested struct and not the other — the SAME shallow-compare false-green
  displaced one field over." The Gate-INVARIANT block (`a3:196-208`) is stated at the FULL-EXPANDED-ROW
  altitude ("every nested field inlined — BOTH `frontend_requirements`'s fields AND `output_labels`'s
  …"). The fold is propagated CONSISTENTLY: `a2-prune-sequencing.md:366-384` (§4a, "the config-tuple
  must be the FULLY-EXPANDED row … both nested structs, not one"), `a0-goalset-residual-overfit.md:132-146`
  (§1-L2) + `a0:266-278` (§2.4), and `SYNTHESIS-AUDIT-OVERFIT.md:209-219` (§5 fact 5, "BOTH
  `frontend_requirements` (field #11) AND `output_labels` (field #12)"). The named-field coupling is
  now matched to its own invariant altitude — no surface recurses into one nested struct only.

- **The `PartialEq`-derive cost is stated AND disk-true.** Disk-verified this pass: `RuntimeTarget`
  derives only `#[derive(Clone, Copy, Debug)]` (`regen.rs:5`) — NOT `PartialEq` — so the full-row
  `PartialEq` mechanism requires ADDING the derive (one line). Both nested structs already derive
  `PartialEq, Eq` (`grammar_provider.rs:45`/`:91`) and the field types support it, so the addition is
  viable. The cost note appears at `a3:183-189`, `a2:377-383`, `a0:140-143`, and `SYNTHESIS §5 fact 5`
  (`:214-219`: "Cost: `RuntimeTarget` today derives only `Clone, Copy, Debug` (`regen.rs:5`), so this
  requires ADDING the `PartialEq` derive (one line)"), each stating the full-row derive is PREFERABLE
  precisely because it covers both nested structs automatically and cannot be coupled to a hand-rolled
  field list. DISCHARGED.

## Disposition ledger (re-verified)

- **Lock-14 green-by-exclusion coupling — A3/R9/P4 (ACCEPT).** The canonical hidden coupling: the gate
  reads CLEAN only because `GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`) is coupled to a
  leak-EXCLUDING root set — `runtime_generator.rs` routed to the weak `SKV15_W2_EXTRA_COVERAGE_ROOTS`
  (`:2442`, chained at `:2511`); the x86 tree tagged `"diagnostic-x86"` (`:2463`). Surfaced (a0 §3 /
  a2 §1 P4 / a3 §2 / SYNTHESIS §3-A3 + §4 P4) + de-coupled (move leak files into strict root, extend
  `FORBIDDEN_GENERIC_TOKENS`, drop the x86 exclusion) + a meaningfulness falsifier (re-inject `JsonSink`
  → gate RED, then revert; `lock14_gate_scans_codegen == true`). A green gate over standing leaks named
  "worse than a red one" (a2 §1 P4). No hidden coupling left un-policed.

- **P4-before-rebuild sequencing coupling (ACCEPT).** SYNTHESIS §5 fact 2 / a2 §2 (1c) / a0 §7 surface
  that the gate's TRUSTWORTHINESS is coupled to its landing ORDER, not its content: P4 MUST land before
  G2/G3 so the un-forked emitter is scanned for neutrality AS authored. Bound as an entry-gate ("not a
  preference," a2 §2). Correct.

- **S-P1-profile cross-pass coupling for G6 (ACCEPT).** SYNTHESIS §5 fact 4 / a0 §1-L6 / a1 §L6 surface
  that the G6 NEON RETIRE branch is coupled to an S-P1 samply non-top-N MEASUREMENT — a cross-pass
  dependency that, if missed, would let G6 close by assertion. Bound profile-FIRST (actual-profiling);
  the kernel-with-its-consumer-in-one-commit clause (no orphan kernel) closes the inverse coupling.
  Correct.

- **No-second-substrate coupling — Lock 1 / G4 (ACCEPT).** a2 §5 (4c) / SYNTHESIS CLEAN-list / a0 §8
  surface that G3/G4 emit over the EXISTING `Tape`/`ValueRef`; an introduced
  `StructLayout`/`TapeStructBuilder`/`TapeCursor` is a Lock-1 type-ambivalence coupling (REJECT). The
  shared trait's existence is held INDEPENDENT of the `<G>` phantom (deleting `<G>` and defining the
  trait are separable, a2 §5/4a) — severing a coupling that would otherwise "manufacture the very
  phantom being deleted." Correct.

- **P3↔G3 PRUNE-binds-GENERALIZE coupling + relocated-seam structural gate (ACCEPT, now broader).**
  a2 §4/§4a / a0 §5 / SYNTHESIS §5 fact 3 surface the deepest non-obvious coupling: P3 (a PRUNE item)
  is NOT independent of G3 (a GENERALIZE item) — the `runtime_target_rows_collapsed` structural check
  is the ONLY mechanism that catches a per-grammar branch RELOCATED into a neutral-identifier
  `RuntimeTarget` data-table (the arm-census regex is syntactically incapable). The R1-CH5 fold WIDENED
  this structural recipe to inline BOTH nested structs — the relocated-seam coupling is now policed at
  the full-row altitude, closing the one-field-over displacement the V1 lens identified (a seam riding
  `frontend_requirements` can no longer slip a one-nested-struct recipe). Correct, and now broader.

## Hidden-coupling sweep — couplings the audit did NOT miss (affirmative)

- **Witness-emission scan-root coupling.** `JsonEventGrammar`/`SheetsEventGrammar` live in `runtime/`
  (disk-confirmed: `crates/runtime/src/grammars/{json,sheets_witness}/event_grammar_witness.rs`), NOT
  the P4 codegen scan root. IF the un-forked generator emits a grammar-named `EventGrammar` literal,
  it must be caught at its emit site — surfaced as SYNTHESIS §2.1 obligation 2 + a1 §L3 + the
  `FORBIDDEN_GENERIC_TOKENS` `EventGrammar`/`*EventGrammar` extension (a2 §1 P4). Discharged in the
  artefact; NOT a residual.
- **Build-soundness coupling P1↔`checkasm_parity.rs`.** Deleting `src/x86_64/` breaks the build via 9
  active compile-coupled call sites (`:458…:502`, enumerated verbatim a2 §3). Same-commit
  decouple-or-delete, exit gate `cargo test --no-run` clean — no intermediate broken-build state.
  Surfaced a2 §3 / a3 §2 / a0 §7 P1. NOT a residual.
- **JSON-richness↔`<G>`-resolution coupling.** `json_rich_navigation_preserved == true` is a SEPARATE
  checked condition a ≥2 impl-count does NOT imply (a2 §5/4b, a0 §1-L4, SYNTHESIS §1 hardening). The
  coupling that a phantom "resolution" could LCD-flatten JSON's navigation is severed (preserve-rich-ast).
  NOT a residual.

## REVISE (0) / REJECT (0)

The single V1 CH5 REVISE (R1-CH5) is DISCHARGED in BOTH parts and confirmed at every surface the V1
lens named (a3 §3, a2 §4a, a0 §1-L2 + §2.4, SYNTHESIS §5 fact 5): the recipe pin now enumerates BOTH
`frontend_requirements` AND `output_labels` at the full-expanded-row invariant altitude, and the
one-line `RuntimeTarget: PartialEq` derive cost is disk-verified (`regen.rs:5` carries only
`Clone, Copy, Debug`) and stated as the pin's preferred realization. Every cross-surface dependency
the addenda gates rely on is surfaced and de-coupled or entry-gated; zero gate reads CLEAN by a
coupling left un-policed; zero finding false-greens another; the PRUNE-sequencing revert/entry-gate
graph is a real dependency chain (P4-before-rebuild, G1→G2/G3/G4/PROVE, G3→PROVE, dual G2-entry on
G1∧P3). Zero orphan REVISE.

## Tally

ACCEPT 6 · REVISE 0 · REJECT 0 — **100%**. R1-CH5 DISCHARGED (recipe pin enumerates BOTH nested
structs at the invariant altitude; the one-line `RuntimeTarget: PartialEq` derive cost disk-verified
and stated); the relocated-seam structural gate now policed at the full-row altitude; every hidden
coupling surfaced and de-coupled or entry-gated.

TALLY accept=6 revise=0 reject=0
