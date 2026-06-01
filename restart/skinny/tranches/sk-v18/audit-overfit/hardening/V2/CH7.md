# S-P0 audit-overfit hardening V2 — CH7 Overfit-Prune (the binding new lens, independent re-verification)

Lens (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`): the audit-overfit synthesis
itself must show that every SK-V18 surface is grammar-derived OR PRUNED; Lock-14 generic-crate
compliance preserved; every admit lands via a real parser/codegen/SIMD source change measured
strict-vs-strict same-plane with a per-iteration equality oracle; every generated output
round-trips (delete + regen ⇒ byte-equivalent); no scaffold-only landing counts as an admit;
and the 6 V3 addenda fire as REJECT triggers. CH7 REJECT cannot be carried as "acknowledged but
not blocking" (`:86-87`). ORCHESTRATOR §3W/§3Z bar: ≥95% across CH1–CH7 for two consecutive
cycles (CH7 the new lens), zero orphan REVISE, V≤5.

Subject: `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md`. Live HEAD `83b66db4232374db6a5f9fa009882f41acc04342`
== `git rev-parse HEAD` (confirmed this pass). V2 is the POST-FOLD confirm: V1 CH7 raised ZERO
REVISE; V2 re-verifies that (a) the 7 V1 fold edits to a0–a3/SYNTHESIS introduced no new CH7
defect, (b) the 6 addenda still FIRE on a live un-remediated surface, and (c) no REJECT-class V3
finding was softened to REVISE by the folds.

CH7's load-bearing question is whether the 6 addenda are EXECUTABLE and CORRECTLY catch the V3
failure modes on a REAL live surface — a decorative addendum that fires on nothing is itself a
paper-close. So every witness below was INDEPENDENTLY re-grepped this pass (not inherited from
a0–a3).

## Independent witness re-verification (every dispositive claim re-grepped at HEAD `83b66db42`)

- **L1 verbatim-blob** — `crates/codegen/src/runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`
  confirmed; `rg 'const \w*_RS *: *&str *= *r#"'` returns ALL 8 couriers at
  `:195/:550/:572/:594/:598/:612/:665/:701`; the CSS body closes at `:1611` (`"#;`) → span
  701→1611 = 910 LOC, corroborating the R1-CH1 fold (descriptive, no gate keys on LOC). FIRES. ✓
- **L2 distinct-grammar-output** — `md5 css_l4_*/generated.rs | sort | uniq -c` = `7 b654562c…`
  (all 7 byte-identical, ONE parser ×7). The 3-co-gate conjunction (md5 ∧ branch-count==0 ∧
  row-collapse) is the correct necessary-not-sufficient hardening. FIRES. ✓
- **L3 single-emitter-path** — `grammar_provider.rs:40-42 enum RuntimeEmitterKind { CompiledLowering, RequestFacts }`
  + `:33 pub emitter:` + `:110` live dispatch confirmed. Neutral variant names defeat L2's
  arm-census — exactly why L3 is a DISTINCT lens. FIRES. ✓
- **L4 phantom-generic** — `tape/mod.rs:175 ValueRef<…, K = AnyKind, G: EventGrammar = AnyGrammar>`
  confirmed; the test-excluded non-test G-instantiation census returns EMPTY (phantom confirmed
  on the `G` axis, NOT the already-real `K` axis). FIRES. ✓
- **L5 timed-plane + corpus-in-timer** — `nonjson_css_l4.rs:3091 fn measure_mbps` warm
  (grep-count 48) confirmed; `css_canon_bench.rs` (the honest cold harness KEPT) PRESENT. FIRES. ✓
- **L6 acceleration-wiring** — `find_css_significant` has ZERO callers in `grammars/*/generated.rs`;
  the sole non-`runtime_simd` caller is `lib.rs:574`, confirmed inside a `#[test]` at `:561`; only
  `count_top_level_commas` reaches a generated module (`css_l4_at_rules_and_media/generated.rs:157`,
  the cold rich-summary). NEON unwired at admission. FIRES. ✓

**PRUNE-sequencing witnesses also independently re-verified:** P1 `src/x86_64/`=24 files,
`ext/x86/`=4 files, `nasm-rs = "0.3"` `Cargo.toml:19`, and the build-soundness coupling — 9 ACTIVE
`bbnf_simd::x86_64::` call sites in `checkasm_parity.rs` at exactly `:458,464,467,477,478,484,493,497,502`
(matching a2 §3 verbatim; an `rm -rf src/x86_64/` without same-wave decoupling IS a broken-build
state). P4 `GENERIC_SCAN_ROOTS:2409` + `diagnostic-x86:2463` exclusion confirmed (R9 green-by-
exclusion disk-true). P5 `parse_w11_1_number` ×7 in `json/generated.rs` confirmed. **R16 (the NEW
finding) independently confirmed:** `RuntimeTarget` (`regen.rs:5-18`) derives ONLY
`#[derive(Clone, Copy, Debug)]` — NOT `PartialEq` (so the one-line derive cost is real); it carries
TWO nested-struct fields, `frontend_requirements` (#11, `regen.rs:17`) AND `output_labels` (#12,
`regen.rs:18`); both nested structs derive `PartialEq, Eq` (`RuntimeFrontendRequirements`
`grammar_provider.rs:45-46`, `RuntimeOutputLabels` `:91-92`). 7 distinct `fact_schema` strings
across the css_l4 rows (`regen_css.rs:49…157`) — the gate is correctly RED pre-P3. And
`frontend_requirements == REQUEST_FACTS_REQUIREMENTS` ×7 (`regen_css.rs:47…155`) — confirming a3's
exact claim: NOT a live divergence vector but a POLICED FUTURE seam. The R16 pin (inline BOTH
nested structs; `RuntimeTarget: PartialEq` is the preferable mechanism) is disk-grounded.

## Section dispositions

### Residual-overfit audit COMPLETE (ACCEPT)

- **A1 — Every surface grammar-derived OR pruned; nothing blessed as hand-written-under-`@generated` (ACCEPT).**
  The residual census R1–R16 (`SYNTHESIS §2`) maps every overfit surface to a PRUNE or GENERALIZE
  disposition: CSS const courier (R1/`runtime_generator.rs:701`) → G2 retire; JSON fixed-literal
  render (R2) → G1 project; fork (R3/`grammar_provider.rs:40-42`) → G3 un-fork; 7 replicas (R4) →
  P3 collapse. The audit checks the BODY, not the `@generated` banner (the L1 verbatim-blob lens
  is precisely the BODY check). No hand-written surface admitted under a provenance header. The
  R1-CH2 fold makes the honest-finding escape (b) predicate a machine mutate-falsifier — closing
  the decorative-argument hatch that §6 polices.

### The 6 addenda EXECUTABLE + correctly catch the V3 failure modes (ACCEPT — the load-bearing output)

- **A2 — The 6 addenda are EXECUTABLE and V3-catching (ACCEPT).** Independently verified above:
  every L1–L6 fires on a REAL live witness at `83b66db42`, and each catches a DISTINCT V3 mode the
  others miss — L3's neutral enum survives L2's arm-census; L4 targets `G` not `K`; L5's
  corpus-in-timer is orthogonal to L6's hot-path reach. The a1 registry pins for each (1) the V3
  finding by path:line, (2) the grep/diff/md5/samply check runnable from `skinny/crates/`, (3)
  DISTINCT REJECT-vs-REVISE criteria. The necessary-not-sufficient hardenings — L2 3-co-gate
  (md5 ∧ branch-count ∧ row-collapse), L4 rich-nav guard (`json_rich_navigation_preserved`),
  L6 retire-on-measurement (S-P1 samply, not assertion) — close the disguised-relocation seams a
  naive md5/grep/checkasm gate would false-green. The R1-CH5 fold deepened the relocated-seam
  structural gate to the full-expanded-row altitude (BOTH nested structs, disk-confirmed). These
  ARE the CH7 enforcement, not a rider.

### Lock-14 trustworthy-before-rebuild (ACCEPT)

- **A3 — Lock-14 compliance preserved + made TRUSTWORTHY before the rebuild (ACCEPT).** R9
  green-by-exclusion is disk-confirmed (`runtime_generator.rs` is NOT in the strict
  `GENERIC_SCAN_ROOTS:2409`; `diagnostic-x86:2463` exclusion live). P4 binds BEFORE G2/G3
  (`SYNTHESIS §5` fact 2; a2 §1c) with the de-exclusion + `CSS_`/`_RS`/`EventGrammar` token
  extension + `diagnostic-x86` drop + the `JsonSink`→RED re-inject falsifier. The gate becomes
  meaningful when the un-forked emitter is authored — the correct sequencing, not a green-over-leaks
  pass. Unchanged by the V1 folds.

### Round-trip / strict-same-plane / per-iter-oracle (ACCEPT)

- **A4 — derivation-proof + measurement discipline carried (ACCEPT).** `regen --check` clean + the
  byte-for-byte `json_templates/` parity oracle bound as the G1/G2 derivation proof, with the
  `.bbnf`-mutation test so a const-courier swap cannot pass. The R1-CH4 fold correctly states the
  ±5% line-count as a SOFT tripwire and the oracle diff-match as the BINDING cost-control
  (a faithful projection may legitimately reorder/dedupe past ±5%). Measurement plane = cold
  `css_canon_bench` (real corpus 71KB–495KB, N≥200, no broadcast) + the 9-field EXACT cssparser
  oracle + JSON 51/51 strict cold. No gate-relabel admit.

### No scaffold-only landing (ACCEPT)

- **A5 — No scaffold-only landing counts (ACCEPT).** PROVE-Sheets emits THROUGH the un-forked
  generator ONLY; if Sheets cannot be generator-emitted, generalization is NOT real and must
  surface as a genuine §6 finding, never stub-proved (`SYNTHESIS §6`, §7 tee-up). The
  honest-finding escape — the single largest paper-close surface, per the contract itself — is
  machine-gated (a)-(c): grammar-INVOKED by name + grammar-DERIVED data + `verbatim_blob_present
  == false` (`SYNTHESIS:245`; R-A0-3 row `:101`), with the R1-CH2 fold making (b) a machine
  mutate-falsifier (the primitive's emitted output must VARY under a `.bbnf` mutation), not the
  prose-reviewable "accepts a grammar-derived argument." This closes the one-level-down prose-review
  seam. A primitive failing (a)-(c) is a relabeled blob → REJECT.

### PRUNE-sequencing sound (ACCEPT)

- **A6 — PRUNE-sequencing sound, anchored, build-safe (ACCEPT).** PRUNE→GENERALIZE→PROVE→HONESTY
  with the load-bearing edges (P3-before-G2 / P4-before-emitter-rebuild / exit-gate-blocks-
  successor) is disk-anchored. The R1-CH3 directional fold makes "G1/G3 co-derive; G3-failure
  blocks PROVE" the explicit FORWARD revert arrow (never a backward "G3 gates G1/G2"), and the
  dual-entry-gate (G2 entry-gates on BOTH G1 AND P3, so a P3 failure ALSO blocks G2) is annotated.
  The P1↔`checkasm_parity.rs` build-soundness coupling is REAL (9 active sites verified, exact line
  numbers). PRUNE carries zero generalization risk (pure deletion + gate-tightening, no
  >SOTA-bearing code removed; net ≈ −10800 LOC).

### Fold integrity (ACCEPT)

- **A7 — The 7 V1 fold edits introduced no new CH7 defect (ACCEPT).** Each fold (R1-CH1 910-LOC
  pin · R1-CH2 machine (b)-predicate · R1-CH3 directional arrow · R1-CH4 oracle-diff cost-control ·
  R1-CH5 full-row R16 · R1-CH6 "beats CSSOM" REJECT clause · R2-CH6 collapse-to-one) is
  prose-precision / machine-grounding over an already-disk-true surface. None admits new
  hardcoding; none softens a REJECT-class V3 finding into a REVISE (the L1–L6 REJECT-vs-REVISE
  criteria stay distinct per abrogate-before-patch — 29 REJECT references in a1, each a distinct
  trigger); none blesses an unproven surface. The R1-CH6 fold HARDENS L5 (an unqualified "beats"
  behind a re-label is now a REJECT, not a soft preference) — a tightening, not a softening.

## One accuracy nit (NON-BLOCKING, descriptive — carried, not a REVISE)

a3 §3's narrative header paraphrase (a3 lines 124-129) numbers `RuntimeTarget` fields in a
shifted scheme while its own fenced code-block and its F-A3.5 disposition row cite the operative
nested fields at `regen.rs:17-18` correctly (disk-confirmed: `frontend_requirements:17`,
`output_labels:18`). No gate keys on the field-number labels (the gate is field-NAMED and
mechanism-agnostic — full-expanded-row collapse), so this is a documentation cosmetic, not a gate
defect. It does not move any disposition and is sub-REVISE; I record it so the synthesis layer can
absorb it if a later cycle re-touches a3, but it does NOT raise a CH7 REVISE (a sub-cosmetic
numbering paraphrase over a field-named, disk-true invariant is not a CH7-class defect).

## REVISE (0) / REJECT (0)

V1 CH7 carried zero REVISE; the fold edits to a0–a3/SYNTHESIS introduced none. Every addendum
FIRES on a live un-remediated surface at HEAD `83b66db42`; none is decorative; no REJECT-class V3
finding (D1–D4, C1–C3) is softened into a REVISE — each maps to a named SK-V18 wave with a
machine-checkable gate. The honest-finding-escape (a)-(c) gate is machine-checked, not
prose-reviewed (the SK-V13 failure mode is closed at the escape hatch). No CRITICAL S-P0 finding ⇒
the PASS-0 forward-halt does not trigger; the prune list is the goalset's own already-Alpha-survived
PRUNE cluster, so S-P0's posture (confirm cleanliness + harden the addenda + pin R16) is correct.

## Tally

ACCEPT 7 (A1 audit-complete · A2 addenda-executable-and-V3-catching · A3 Lock-14-trustworthy ·
A4 round-trip/strict-plane/oracle · A5 no-scaffold-only · A6 PRUNE-sequencing-sound ·
A7 fold-integrity) · REVISE 0 · REJECT 0 — **100%**. The 6 addenda are executable, independently
witness-re-verified live, each catching a DISTINCT V3 failure mode; the residual census R1–R16 is
complete; the PRUNE-sequencing is sound and build-safe (9-site checkasm coupling verified); the
R16 nested-struct precision pin is disk-grounded (both nested structs derive `PartialEq, Eq`,
`RuntimeTarget` does not). The one accuracy nit is sub-REVISE/non-blocking. CH7 is the spine of the
audit, not a rider.

TALLY accept=7 revise=0 reject=0
