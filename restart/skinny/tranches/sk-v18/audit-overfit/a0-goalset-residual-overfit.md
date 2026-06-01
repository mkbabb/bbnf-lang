# A0 (S-P0, cycle V2) — SK-V18 Goalset RESIDUAL Overfit Audit + the 6 CHALLENGE Addenda Formalized

Date: 2026-05-31. Pass: S-P0 Overfit Audit (skinny). Agent: A0. Cycle: V3 (V1→V2 hardened by CH1–CH7; V2 confirm 7×100%, zero REVISE/REJECT; V3 = post-confirm label-precision fold).
Subject HEAD at audit: `83b66db42` (`docs(sk-v18-alpha): … generalization goalset bracketed (G-Alpha ready)`).
Contract bracket cited in the docs: `318d9c046`; SK-V17 close `f6a38445b`; V3 audit seed `7dbe44c22`.
Inputs (LOCKED, G-Alpha closed): `sk-v18/SYNTHESIS.md`, `sk-v18/HANDOFF.md`,
`research/alpha/{alphaA..F}.md`, `research/alpha-hardening/CONSOLIDATED-CONVERGED.md`,
seed `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` + `AGENT-{1..6}-*.md`.

**V3 CONFIRM (V2 CH1–CH7 all 7×100% ACCEPT, zero REVISE, zero REJECT — disk-re-verified at HEAD
`83b66db42`):** the V2 hardening pass re-grepped every dispositive witness independently and
DISCHARGED all seven V1 REVISEs on disk (CH1 R1 910-LOC; CH2 R1 (b)-mutate-falsifier; CH3 R1
dual-entry-gate/directional-arrow; CH4 R1 ±5%-soft/oracle-binding; CH5 R1 both-nested-struct
recipe + `PartialEq`-derive cost; CH6 R1/R2 "beats"-REJECT + collapse-to-one carry-up). a0 carries
ZERO orphan REVISE into V3. The only sub-REVISE items the V2 pass recorded — CH1's loose "alphaE §A"
section label (the quote is verbatim-exact at `alphaE-candidate-shortlist.md:109`, in the Risk
bullet) and CH7's a3 field-number paraphrase (a3-local, not a0) — are non-blocking cosmetics; the
a0-local one is folded this V3 cycle (§5 now cites the exact Risk bullet at `:109`). No gate, no
disposition, no ground-truth witness changes.

**V2 FOLD (CH1–CH7 V1 dispositions resolved, all disk-re-verified at HEAD `83b66db42`):**
- **CH1 R1** (a1 LOC-range): the CSS courier body span is disk-measured `runtime_generator.rs:701`→`:1611`
  = **exactly 910 LOC** (the V3-seed "646–910" is a pre-measurement estimate, superseded). Pinned in
  the L1 witness below — no gate keys on the LOC (the binding gate is `verbatim_blob_present==false`
  + the `.bbnf`-mutation test).
- **CH2 R1** (L1 escape (b) predicate): the honest-finding-primitive (b) predicate is restated as the
  MACHINE per-primitive mutate-falsifier (the primitive's EMITTED OUTPUT must vary under a `.bbnf`
  mutation of the invoking rule), not "accepts a grammar-derived argument" — folded into §1-L1 + §6.
- **CH5 R1** (R16 recipe-pin altitude): the `runtime_target_rows_collapsed` recipe inlines EVERY
  nested-struct field — `frontend_requirements` (#11) AND `output_labels` (#12), NOT only
  `output_labels` — at the invariant's full-expanded-row altitude; the cleanest sufficient mechanism
  is adding `#[derive(PartialEq)]` to `RuntimeTarget` (disk: it derives only `Clone, Copy, Debug`;
  both nested structs already derive `PartialEq, Eq`). Folded into §1-L2 + §2.4.
- **CH6 R1** (R-A0-1 "beats" qualifier): a0 §4 already binds it — sharpened to an explicit REJECT
  clause (unqualified "beats CSSOM"/"equal-work" close-report language behind a re-label is REJECT).
- **CH6 R2** (R-A0-2 collapse-to-one): a0 §5 already reaches the disk-grounded answer — sharpened so
  the binding is unambiguous (`generator_grammar_count == 3` = json+css+sheets, NOT json+7-css+sheets).
- **CH3 R1 / CH4 R1**: folds target SYNTHESIS-AUDIT-OVERFIT + the alphaE feeder (revert-graph prose
  disambiguation; G1 ±5% LOC stated SOFT, oracle diff-match binding) — carried to the synthesis layer,
  noted in §7 + §9 here so no orphan-REVISE survives. **CH7: 0 REVISE (100%).**

S-P0 mandate (this artefact): (1) audit the SK-V18 GENERALIZATION goalset + the alphaE
shortlist for RESIDUAL overfit/contrivance/hardcoding that survived the Alpha CHALLENGE,
BEFORE profiling; (2) formalize the 6 new CHALLENGE addenda as binding S-P0 lenses;
(3) produce the audit-overfit synthesis with the PRUNE-before-GENERALIZE-before-PROVE
sequencing + the PRUNE-list. The 6 addenda are the load-bearing output.

---

## §0 — Verdict

**The SK-V18 goalset is, at the architecture/spine level, the HONEST inverse of the SK-V13
build-first-audit-never pattern: a generalization cycle whose net effect is DELETION
(net LOC ≈ −12,650…−12,850), gated by close-conditions and telemetry columns the
`gate-json` consumer REJECTs on.** Every dispositive ground-truth claim verifies on disk
at HEAD `83b66db42` (§2). The 6 addenda from the V3 audit are bound THREE ways each
(close gate + §0.4 pre-block + a machine-checkable telemetry column) — the strongest
anti-overfit posture any skinny tranche has carried.

**But the goalset's INTEGRITY is load-bearing on the gates being SATISFIABLE and the prune
landing FIRST, and three residual seams survive the Alpha CHALLENGE that S-P0 must carry
forward as binding lenses, not as REJECTs:**

- **R-A0-1 (RESIDUAL CONTRIVANCE, MEDIUM):** the >SOTA-PRESERVATION framing still smuggles
  the lazy-vs-eager asymmetry as the DEFAULT comparator, with H1 offering "re-frame OR add
  a symmetric comparator" — the OR lets the cheaper path (re-label, no symmetric work)
  close the honesty gate. This is the C2 contrivance surviving as a framing escape, not a
  measurement fix. (§4 — corpus-in-the-timer + timed-plane-symmetry addenda.)
- **R-A0-2 (PROFILE-DISTINCTNESS ERASURE HAZARD, MEDIUM):** P3 "collapse the 7 CSS replicas"
  has a genuine, contract-acknowledged tension — collapse-to-one vs preserve-7-distinct — and
  the `runtime_target_rows_collapsed` gate is RED-by-design pre-P3 and only goes GREEN by a
  collapse that MUST NOT erase legitimate `profile` distinctness. The gate is correct, but it
  is one of two outcomes (collapse / differentiate) and the contract DEFERS the decision to
  B2 — a deferred decision on a RED gate is a paper-close risk if S-P3 does not bind which
  branch each of the 7 profiles takes. (§5 — distinct-grammar-output addendum.)
- **R-A0-3 (HONEST-FINDING ESCAPE = the single largest paper-close surface, MEDIUM):** the
  "named validated grammar-parameterized primitive" escape (HANDOFF §6 / SYNTHESIS PASS-IMPL
  V4 row) is the contract's own admission that a grammar-derived parser MAY fail to preserve
  >SOTA and the hand-shaping survives as a "primitive." It is gated (a)-(c), but the gate is
  prose-reviewed-at-admission, not machine-checked, and is exactly where a verbatim blob can
  re-enter wearing a "primitive" label. (§6 — verbatim-blob addendum.)

**None of the three is a REJECT.** They are the residual seams the 6 addenda exist to police,
sharpened to S-P0 lenses. The goalset spine, the prune list, and all 6 addenda are
structurally sound and disk-true. The PRUNE list (§7) lands FIRST, unchanged.

**Sequencing note (process):** the alpha-hardening `CONSOLIDATED-CONVERGED.md` records
`Converged=false` at the §3Z bar (V5 97.9% + confirm 88.9% is NOT a 2-consecutive ≥95% pair).
The residuals it names are mechanical (binding-row propagation + a 12/13 struct-count slip);
I verify in §2 they have SINCE been folded into the binding contract (the (h)/(i) x86 reach
and the by-exclusion `profile` enumeration are both present at `SYNTHESIS.md:326/:333/:566/:576`).
S-P0 inherits a substance-converged, formally-short goalset; this audit treats it as the
LOCKED G-Alpha surface per the dispatch ("INPUTS LOCKED, G-Alpha closed").

---

## §1 — The 6 CHALLENGE addenda FORMALIZED as binding S-P0 lenses

These are the load-bearing output. Each addendum is restated as (i) the lens, (ii) the live
witness it fires on TODAY, (iii) the binding machine-check (the telemetry column the
`gate-json` consumer REJECTs on), (iv) the S-P0 RESIDUAL it must police downstream. They bind
S-P0 (this pass) AND every downstream pass CHALLENGE (S-P2/S-P3 + every wave plan + redress).

### L1 — verbatim-blob
- **Lens:** a `@generated` file that is a verbatim `&str` string-literal in codegen = hand-written,
  NOT derived → REJECT the "grammar-driven" claim.
- **Live witness (disk):** `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"…"#`
  — the raw-string body runs `:701`→`:1611` = **exactly 910 LOC** (disk-measured; CH1 R1 fold —
  the V3-seed "646–910" range was a pre-measurement estimate, superseded; no gate keys on the LOC,
  the figure is descriptive). The un-remediated SK-16 finding now wears a provenance-honest
  `@generated` header (`:685` comment); `json_sink_direct.rs:4 render(program)` then emits fixed
  `push_str` literal bodies (`:96 render_entry`, `:124 render_value_dispatch`, …) — the grammar
  only `validate()`-gates at `:18-31`, it does NOT shape the body.
- **Machine-check:** `verbatim_blob_present == false` (gate REJECTs `true`). The operational
  falsifier (alphaE §0.2): **mutate the `.bbnf` → the regenerated `generated.rs` changes
  correspondingly** — a const courier cannot pass this.
- **S-P0 RESIDUAL (R-A0-3):** the verbatim-blob re-entry is NOT only a fresh `const _RS`;
  it is the **honest-finding "named primitive" escape** (HANDOFF §6). A relabeled blob spliced
  by the emitter — not invoked by name from the `.bbnf`, not parameterized by grammar-derived
  data — is a verbatim blob wearing a "primitive" label. The escape's (a)-(c) gate IS the
  verbatim-blob lens applied to the escape; S-P0 binds it as machine-checked (`verbatim_blob_present`
  on the primitive surface), NOT prose-reviewed. **CH2 R1 fold — the (b) parameterization predicate
  is itself a per-primitive MACHINE mutate-falsifier, not "accepts a grammar-derived argument":**
  the primitive's EMITTED OUTPUT must change correspondingly under a `.bbnf` mutation of the
  invoking rule's shape (the whole-path `.bbnf`-mutation test applied to the primitive's output in
  isolation). A primitive that accepts a grammar-derived argument yet splices a FIXED body keyed
  off it (argument decorative, body verbatim) fails (b) the same way a const courier fails the
  whole-path test — REJECT. This closes the one-level-down prose-review seam (the only one of the
  three predicates previously expressible as prose). See §6.

### L2 — distinct-grammar-output
- **Lens:** N claimed grammars must have N NON-identical `generated.rs` (diff/md5 census).
  md5-distinctness is NECESSARY-NOT-SUFFICIENT — also require `generator_grammar_branch_count == 0`
  (no grammar-named `match` arms) AND `generator_grammar_type_count == 0` AND the structural
  `runtime_target_rows_collapsed` check.
- **Live witness (disk):** all 7 `css_l4_*/generated.rs` share md5
  `b654562ccff46ed62dd48e9ace325830` (verified across all 7) — ONE CSS parser replicated 7×,
  materially overstating "7 grammars admitted."
- **Machine-check:** `generated_md5_distinct == true` AND `generator_grammar_branch_count == 0`
  AND `generator_grammar_type_count == 0` AND `runtime_target_rows_collapsed == true`. The last
  is the STRUCTURAL relocated-seam check the arm-census regex is syntactically incapable of
  firing on (a neutral-identifier data-table carries no `Json =>` arm syntax, CH2 V3 §8.1).
  **CH5 R1 fold — the `runtime_target_rows_collapsed` recipe is stated at the INVARIANT's altitude
  (the FULLY-EXPANDED row, every nested-struct field inlined, MINUS only the path columns
  `output_dir`/`expected_files`), NOT at one named nested struct.** The live `RuntimeTarget`
  (`regen.rs:6-18`) has TWO nested-struct fields, both in the by-exclusion comparison set:
  `frontend_requirements` (#11, `RuntimeFrontendRequirements` `grammar_provider.rs:46`) AND
  `output_labels` (#12, `RuntimeOutputLabels` `grammar_provider.rs:92`). A recipe that recurses into
  `output_labels` ONLY would shallow-compare `frontend_requirements` and false-green a future seam
  riding it — the EXACT shallow-compare hazard displaced one field over. The cleanest sufficient
  mechanism is adding `#[derive(PartialEq)]` to `RuntimeTarget` (disk: it derives only
  `Clone, Copy, Debug` at `regen.rs:5`; both nested structs already derive `PartialEq, Eq` at
  `grammar_provider.rs:39/45/91`, so the one-line derive is viable and covers BOTH nested structs
  automatically — it cannot be coupled to a hand-rolled field list). Note the one-line derive as the
  pin's cost. Today `frontend_requirements == REQUEST_FACTS_REQUIREMENTS` across all 7 css_l4 rows
  (`regen_css.rs:47…155`), so it is not a LIVE divergence vector — but the pin forbids a FUTURE
  relocated seam, and that is precisely the altitude it must hold.
- **S-P0 RESIDUAL (R-A0-2):** distinctness has TWO escape routes the contract acknowledges —
  (1) a grammar-branching emitter body that produces md5-distinct output (caught by the arm/type
  census); (2) a relocated branch in a NEUTRAL-identifier `RuntimeTarget` data-table riding
  `profile`/`frontend_requirements`/`output_labels` (the nested `fact_schema`/`output_plane`)/`emitter`
  (caught ONLY by the per-`grammar_name` full-expanded-row collapse over all non-path columns). The
  by-exclusion projection (§2.4) is sound; S-P0's residual is that P3's collapse-vs-differentiate
  decision is DEFERRED to B2, on a gate that is RED-by-design until resolved.

### L3 — single-emitter-path
- **Lens:** one grammar-agnostic emitter; flag grammar-family forks behind an abstract enum.
- **Live witness (disk):** `grammar_provider.rs:40-42 RuntimeEmitterKind = {CompiledLowering, RequestFacts}`,
  dispatched `runtime_generator.rs:17,25`; consumed `grammar_provider.rs:110` and in the live
  `regen_css.rs` rows (`emitter: codegen::RuntimeEmitterKind::RequestFacts`, 7×). A grammar-family
  fork (JSON vs CSS) behind an abstract enum — the textbook overfit-behind-an-abstraction.
- **Machine-check:** `emitter_fork_present == false` (gate REJECTs `true`); `grep -c
  'RuntimeEmitterKind\|CompiledLowering\|RequestFacts'` in codegen → 0 post-G3.
- **S-P0 RESIDUAL:** `RuntimeEmitterKind` is referenced from `xtask/src/regen_css.rs` (the
  metadata surface), not only codegen. G3 must retire it from BOTH roots; the `emitter` column
  becoming neutral (a strategy enum with no grammar-family semantics) is itself a relocation
  hazard policed by `runtime_target_rows_collapsed` (the `emitter` field is in the operative
  non-path set, so a per-grammar emitter divergence is caught).

### L4 — phantom-generic
- **Lens:** a generic type param never instantiated with a real type OUTSIDE `#[cfg(test)]` is
  decorative → instantiate-or-delete. DELETE is the default; preserve JSON rich navigation so a
  ≥2 impl-count cannot LCD-flatten.
- **Live witness (disk):** `tape/mod.rs:175 ValueRef<'doc,'input, K = AnyKind, G: EventGrammar = AnyGrammar>`
  — the `G` axis defaults to `AnyGrammar` and is instantiated with a real grammar ONLY in
  `tape/event_grammar_tests.rs` (`_proof_compiles::<JsonEventGrammar>`/`::<SheetsEventGrammar>`,
  test-only). The `K=Kind` axis IS real (correctly NOT the phantom target). `CssEventGrammar`
  does NOT exist at HEAD.
- **Machine-check:** `phantom_generic_resolved ∈ {instantiated, deleted}` (gate REJECTs `phantom`).
  Test-excluded grep: `grep -rn 'ValueRef<.*EventGrammar>' --include='*.rs' … | grep -v
  'tests\.rs\|#\[cfg(test)\]'` → ≥1 production for INSTANTIATE; `grep -c 'G: EventGrammar'
  tape/mod.rs` → 0 for DELETE. AND `json_rich_navigation_preserved == true`.
- **S-P0 RESIDUAL:** the contract correctly makes DELETE the abrogate-before-patch DEFAULT and
  warns "do NOT couple the trait's shape to animating `<G>` (that would manufacture the very
  phantom we are deleting)" (`SYNTHESIS.md:334`). The residual: the G4 shared-trait existence
  and the `<G>` deletion are SEPARABLE; S-P0 binds that the ≥2 trait-impl gate be PRODUCTION-only
  (test-excluded, mirroring the F6/F9 exclusion) so a `#[cfg(test)] impl SharedValueTrait for
  CssTestNode` cannot false-green the count — and that the rich-AST preservation be a SEPARATE
  checked condition, not implied by the ≥2 count.

### L5 — timed-plane-symmetry + corpus-in-the-timer
- **Lens:** the >SOTA comparator must do EQUAL work on the REAL corpus, COLD — no micro-fixtures,
  no more-work-competitor; the canonical `css_canon_bench` is the honest one.
- **Live witness (disk):** the contrivance — `nonjson_css_l4.rs lightningcss_facts:528`
  (+ 3 siblings) / warm `measure_mbps`: warm iters, 85–357-byte SHA256-pinned micro-fixtures,
  timed competitor does MORE work (parse + SHA256 + a second cssparser re-parse). It did NOT
  produce the headline numbers (those came from `css_canon_bench`) but is a LIVE contrivance.
- **Machine-check:** `corpus_in_timer == true` (gate REJECTs `false`); P2 DELETES the warm path;
  `materialization_framing ∈ {lazy-rich-vs-eager-cssom, symmetric-comparator}`.
- **S-P0 RESIDUAL (R-A0-1):** the canonical `css_canon_bench` is cold + real-corpus + N≥200 +
  no-broadcast (A5 live-reproduced) — the corpus-in-the-timer half is HONEST. The residual is
  the timed-PLANE-symmetry half: Track 1 counts 9 aggregate fields LAZILY while lightningcss
  builds an OWNED full CSSOM — not equal work. H1's "re-frame OR add a symmetric comparator"
  lets the cheaper branch (re-label only) close the gate. S-P0 binds that the framing escape
  (`materialization_framing == lazy-rich-vs-eager-cssom`) is the HONEST disclosure, NOT a
  symmetric-work claim — and that the word "beats" in any close report be qualified by the
  materialization-depth asymmetry. See §4.

### L6 — acceleration-wiring
- **Lens:** a NEON/ASM acceleration claim must show the kernel reached AT ADMISSION (hot path),
  not only under `#[cfg(test)]`.
- **Live witness (disk):** `runtime_simd.rs` carries `count_top_level_commas:29`,
  `find_comment_close:112`, `find_css_significant:169`. ONLY `count_top_level_commas` reaches a
  generated module (`css_l4_*/generated.rs:157`/`:809-810`, the COLD rich-summary path);
  `find_css_significant`/`find_comment_close` have ZERO callers outside the `#[cfg(test)] mod
  tests` parity-guard block (verified: no hits in `grammars/`). The hot CSS scan is SCALAR.
  5 kernels are `_neon`-suffixed scalar passthroughs; the UDOT `digit_mac` is an orphan.
- **Machine-check:** `acceleration_at_admission ∈ {admission, scalar-passthrough-labeled, retired}`
  (gate REJECTs `cfg-test-only`). The G6 retire branch is gated on a samply non-top-N MEASUREMENT,
  not an assertion.
- **S-P0 RESIDUAL:** the contract correctly demands the retire branch carry a samply attribution
  row (a measurement, `SYNTHESIS.md:336`/`:382`). S-P0 binds that this is profile-FIRST: S-P1
  must re-confirm the JSON+CSS hot leaves on the benched path BEFORE any G5/G6 kernel lands, so
  "retire" is grounded in a non-top-N reading, not an admission-time assertion. No "neon" label
  survives on a scalar body.

---

## §2 — Ground-truth verification (every dispositive claim re-grepped at HEAD `83b66db42`)

| Claim (contract) | Disk verdict | Cite |
|---|---|---|
| `CSS_GENERATED_RS` verbatim `&str` const, `.bbnf` never consumed by CSS emit | CONFIRMED | `runtime_generator.rs:91,685,701` |
| `json_sink_direct::render` emits fixed string-literal bodies | CONFIRMED | `json_sink_direct.rs:4,68,96,124,251,326,367` |
| `RuntimeEmitterKind = {CompiledLowering, RequestFacts}` fork | CONFIRMED | `grammar_provider.rs:40-42,110`; `regen_css.rs:45,…` (7×) |
| 7 `css_l4_*/generated.rs` byte-identical, md5 `b654562c…` | CONFIRMED (all 7 share md5) | `runtime/src/grammars/css_l4_*/generated.rs` |
| `ValueRef<G: EventGrammar = AnyGrammar>` phantom (G axis); K axis real | CONFIRMED | `tape/mod.rs:175` |
| x86 surface 1: `src/x86_64/` = 24 files | CONFIRMED (24) | `bbnf-simd/src/x86_64/` |
| x86 surface 2: `ext/x86/` = 4 files + nasm `build.rs` + `nasm-rs="0.3"` dep | CONFIRMED (4 files; `build.rs` 3784B; `Cargo.toml:19`) | `bbnf-simd/ext/x86/`, `build.rs`, `Cargo.toml:15,19` |
| `GENERIC_SCAN_ROOTS` + `diagnostic-x86` exclusion + weak `SKV15_W2_EXTRA_COVERAGE_ROOTS` | CONFIRMED | `lock14_baseline.rs:2409,2420,2442,2463` |
| CSS NEON `find_css_significant`/`find_comment_close` cfg(test)-only; only `count_top_level_commas` in generated (cold) | CONFIRMED (0 callers in `grammars/`; cold rich-summary `:807-810`) | `runtime/src/lib.rs:500-502,574,598,608`; generated `:157,:810` |
| JSON bespoke `neon::scan` not neutral | CONFIRMED | `json/scan.rs:25,201` |
| Sheets = 25-LOC stub (24+1) | CONFIRMED | `sheets_witness/{event_grammar_witness.rs(24),mod.rs(1)}` |
| `grammar/google-sheets/google-sheets.bbnf` real Pratt grammar, 185 LOC, NOT in skinny tree | CONFIRMED (185 LOC, lives under `grammar/`) | `grammar/google-sheets/google-sheets.bbnf` |
| Metalang leak `parse_w11_1_number` ×7 in shipped JSON `generated.rs` | CONFIRMED (count = 7) | `runtime/src/grammars/json/generated.rs` |
| `RuntimeTarget` = **12 fields** (the "13-field" labels are a slip) | CONFIRMED (12) | `xtask/src/regen.rs:6-18` |
| `fact_schema`/`row_id`/`output_plane` are NOT `RuntimeTarget` fields; they live in `RuntimeOutputLabels` wrapped by `output_labels` | CONFIRMED — they are `RuntimeOutputLabels` fields (`grammar_provider.rs:92-95`), reached via `output_labels: Option<RuntimeOutputLabels>` | `regen_css.rs:47-52`; `grammar_provider.rs:92-95` |
| binding P1 row + `x86_tree_deleted` carry (h) `checkasm_parity.rs` + (i) `byte_class_from_eq_set_64.rs` (the V6-confirm CH7-R1 residual) | **DISCHARGED on disk** — (a)-(i) present, build-soundness `cargo test --no-run` folded | `SYNTHESIS.md:326,576`; `HANDOFF.md:110-121,336` |

**Every dispositive ground-truth claim verifies.** The single accuracy slip the alpha-hardening
confirm flagged — `RuntimeTarget` "13-field" vs disk 12-field — is real but NON-blocking: the
by-exclusion mechanism names the operative fields regardless of the printed count, and the count
appears corrected to 12 at the operative G3 close-gate (`SYNTHESIS.md:333`) while a stale "13"
survives elsewhere in the fold narrative. S-P0 carries it as a documentation-accuracy nit, not a
gate defect (the gate is field-named, not count-driven).

### §2.4 — The by-exclusion P3 projection is SOUND (the F16 fold verified)

The most-litigated gate in the contract (`runtime_target_rows_collapsed`, V1 md5 → V2 grep-alphabet
→ V3 grep-cannot-fire → V4 row-count-too-narrow → V5 `profile`-omission) lands correctly:
- The live `RuntimeTarget` (`regen.rs:6`) has 12 fields; `fact_schema`/`row_id`/`output_plane`
  are NOT among them — they are `RuntimeOutputLabels` fields reached via the `output_labels`
  field (the 12th). The contract's framing ("per-profile content the `profile` discriminator
  selects, not struct fields") is PRECISE.
- The by-exclusion operative set EXCLUDES only `output_dir` + `expected_files` and INCLUDES the
  TWO nested-struct fields `frontend_requirements` (#11) AND `output_labels` (#12) — so per-profile
  `RuntimeOutputLabels` divergence (the 7 distinct `fact_schema`/`output_plane` strings) IS caught
  structurally via the wrapping `output_labels` field, AND a future seam riding `frontend_requirements`
  is equally caught. A relocated branch riding any per-profile nested field cannot escape **provided
  the recipe inlines BOTH nested structs** (CH5 R1): the binding INVARIANT is the full-expanded-row
  collapse, and the cleanest realization is `#[derive(PartialEq)]` on `RuntimeTarget` (one line;
  disk: `RuntimeTarget` derives only `Clone, Copy, Debug` `regen.rs:5`; both nested structs already
  `PartialEq, Eq`), which covers both nests automatically and cannot be coupled to a hand-rolled
  one-struct field list. A recipe that recurses into `output_labels` only is a shallow-compare
  false-green displaced one field over — FORBIDDEN.
- The gate is correctly RED today (7 distinct `profile` + 7 distinct `output_labels`;
  `frontend_requirements` is uniform `REQUEST_FACTS_REQUIREMENTS` today, not a live vector but a
  policed future one) and GREEN only post-collapse. The mechanism + projection are now reach-complete
  at the invariant's altitude (both nested structs, not one).

This is the gate's strongest form. S-P0's residual on it is R-A0-2 (the collapse-vs-differentiate
DECISION is deferred), NOT the projection.

---

## §3 — RESIDUAL overfit/contrivance/hardcoding that survived the Alpha CHALLENGE

S-P0 found NO new per-grammar special-case introduced by the goalset, NO GENERALIZE wave that
secretly preserves a hand-written blob in its CLOSE CONDITION (each G-wave's verify clause is a
neutral grep + a >SOTA-preservation floor), and NO P3 profile-distinctness erasure in the gate
itself (the gate forbids erasing legitimate distinctness). The three residuals below are
contrivance/paper-close SEAMS the addenda must police at execution, not goalset defects.

---

## §4 — R-A0-1: the >SOTA-PRESERVATION framing still smuggles lazy-vs-eager (the C2 contrivance as a framing escape)

**Finding (MEDIUM, addendum L5).** The CSS >SOTA headline (bootstrap 2.210× / animate 2.355× /
tailwind 3.348× / material 1.996×) is MEASUREMENT-VALID — `css_canon_bench` is cold, real-corpus
(71KB–495KB), N≥200, distinct per-corpus medians, no broadcast, genuine 9-field oracle
(A5 live-reproduced 2.15/2.91/1.91/1.98×). The corpus-in-the-timer half is honest.

But the timed-PLANE-symmetry half is NOT equal-work: Track 1 *counts* 9 aggregate fields LAZILY
(zero payload writes, value-head classification) while lightningcss *builds an owned typed
CSSOM*. The rich rider costs ~25-33% over the 4-field path (real per-node work, so "materially
less severe than a brace-counter"), but the honest framing is **"lazy rich-summary beats eager
full-CSSOM,"** NOT "equal-work CSSOM beats CSSOM."

**The residual the CHALLENGE did not close:** H1 reads "re-frame the CSS >SOTA as
lazy-rich-summary vs eager-full-CSSOM **OR** add a symmetric materialization-depth comparator"
(`SYNTHESIS.md:338`, HONESTY H1). The OR is the seam. The cheaper branch — re-label the
existing measurement, add NO symmetric comparator — closes the gate while the asymmetry stands.
That is a FRAMING fix, not a measurement fix. The lazy-vs-eager contrivance is then preserved
behind a `materialization_framing == lazy-rich-vs-eager-cssom` label that the gate accepts.

**This is the lazy-vs-eager contrivance the dispatch named as the thing a ">SOTA-preservation
framing might smuggle.** It is smuggled — not by a fabricated number (the numbers are real), but
by a comparator that does asymmetric work, disclosed-but-not-corrected.

**S-P0 BINDING (carry into S-P2/S-P3 + every H1 redress):**
1. `materialization_framing == lazy-rich-vs-eager-cssom` is the HONEST DISCLOSURE branch — it is
   ACCEPTABLE only if the close report states the materialization-depth asymmetry EXPLICITLY and
   does NOT use unqualified "beats CSSOM" / "equal-work" language. The word "beats" must carry the
   asymmetry qualifier. **CH6 R1 fold — this is a REJECT clause, not a preference:** an unqualified
   "beats CSSOM" / "equal-work" close-report claim standing behind a re-label (the cheaper OR branch
   with no symmetric work) is a CH7 REJECT, NOT a passing disclosure. The OR's re-label branch closes
   the honesty gate ONLY with the asymmetry disclosed explicitly; the bare re-label that leaves an
   unqualified ">beats" claim is the paper-close the gate exists to forbid. This binding must be
   carried up into the SYNTHESIS R-A0-1 row (which under-states it as "preferred / must disclose").
2. The symmetric-comparator branch is STRONGER and PREFERRED where dischargeable (a comparator
   that materializes the same 9 fields lightningcss does, or a lightningcss-mode that emits only
   the 9 aggregates) — but it is NOT mandatory, because the lazy-vs-eager disclosure is itself
   honest IF stated. The OR survives; the dishonesty (an unqualified ">beats CSSOM" claim behind
   a re-label) does not.
3. `corpus_in_timer == true` is mandatory regardless (P2 deletes the warm micro-fixture path).
   This half is non-negotiable.

**Not a REJECT:** the contract DOES disclose the asymmetry in C2/H1 and the SYNTHESIS ground-truth
("BUT Track 1 counts 9 aggregate fields lazily while lightningcss builds an owned full CSSOM —
honest framing is 'lazy rich-summary beats eager full-CSSOM'", `SYNTHESIS.md:284-291`). The
residual is that the CLOSE gate accepts the disclosure label without binding the report language.
S-P0 binds the language.

---

## §5 — R-A0-2: P3 collapse-vs-differentiate is a DEFERRED decision on a RED-by-design gate

**Finding (MEDIUM, addendum L2).** P3 ("collapse the 7 byte-identical CSS replicas") is the
distinct-grammar-output addendum's centerpiece. The contract correctly offers TWO outcomes
(SYNTHESIS PRUNE P3, alphaE §P3): **collapse to ONE CSS grammar** (one `generated.rs`), OR
**N non-identical generated files each derived from a distinct `.bbnf`** (differentiate via
`color.bbnf`/`media.bbnf`/`selectors.bbnf`). The `runtime_target_rows_collapsed` gate is RED
today (7 distinct `profile` + `output_labels`) and goes GREEN only when the 7 profiles
GENUINELY collapse to one CSS config — AND the gate explicitly FORBIDS erasing legitimate
`profile` distinctness (P3 "must PRESERVE profile-distinctness where the profiles are distinct
grammars," `SYNTHESIS.md:333`).

This is the correct anti-erasure posture — the gate cannot be satisfied by erasing real
distinctness. **But alphaE defers the decision** (the "Risk: LOW" bullet, `alphaE-candidate-shortlist.md:109`):
"P3 must decide collapse-vs-differentiate (defer the *which* to B2, but the *replica deletion*
lands here)" (verbatim-exact at `:109`; the quote lives in the Risk bullet, not a "§A"
section — CH1 V2 label-precision fold). A deferred decision on a RED gate is a paper-close RISK: if the 7 css_l4
profiles are genuinely ONE grammar (one `stylesheet.bbnf`, one `entry_rule`), collapse-to-one is
correct and `runtime_target_rows_collapsed` goes GREEN honestly. If they are 7 genuinely-distinct
sub-grammars, they must point at 7 distinct `.bbnf` roots so their `generated.rs` truly diverge
(md5-distinct) AND each names the distinct config it derives from — and the gate goes GREEN by
DIFFERENTIATION, not collapse.

**The disk truth (verified §1):** all 7 share `stylesheet.bbnf` (`source_roots: CSS_L4_ROOTS`,
`entry_rule: "stylesheet"`) and produce a byte-IDENTICAL `generated.rs`. They are ONE grammar
replicated 7× with 7 distinct `profile`/`output_labels` METADATA tags. The honest collapse is
collapse-TO-ONE (they ARE one grammar at the parser level); the 7 `profile` tags are bench-row
labels, not distinct grammars. **The distinct-grammar-output litmus is therefore satisfied by
collapse-to-one + Sheets as the genuine third grammar** — NOT by manufacturing 7 fake CSS
sub-grammars.

**S-P0 BINDING (carry into S-P3 wave plan):**
1. S-P3 MUST bind the P3 decision EXPLICITLY before the wave dispatches: the disk evidence is
   collapse-to-one (one `stylesheet.bbnf`, byte-identical output). Differentiation into 7 fake
   sub-grammars to satisfy a distinctness gate would be the EXACT overfit the addendum forbids
   (manufacturing N grammars from one). The honest path is ONE CSS `generated.rs` + Sheets as the
   3rd grammar (`generator_grammar_count == 3` = json + css + sheets, NOT json + 7-css + sheets).
   **CH6 R2 fold — this disk-grounded answer must be carried UP into the SYNTHESIS R-A0-2 row, not
   left for S-P3 to re-derive:** the SYNTHESIS row currently defers "S-P3 must bind which branch each
   of the 7 profiles takes" without stating the answer, which is a thin paper-close seam (S-P3 could
   bind "differentiate" and manufacture roots). The consolidated row must carry "disk evidence is
   collapse-to-one — one `stylesheet.bbnf`, byte-identical output; `generator_grammar_count == 3` =
   json+css+sheets; manufacturing 7 fake roots to satisfy a distinctness gate is the overfit the
   addendum forbids."
2. The 7 `profile` discriminators are bench-row metadata, not grammars — collapsing them to one
   CSS config is the correct outcome and does NOT erase a real grammar. The gate's
   "preserve-profile-distinctness" clause protects against erasing a TRUE sub-grammar; here there
   is none to preserve, so collapse is honest.
3. `generated_md5_distinct == true` is then satisfied by {json, css, sheets} being 3 distinct
   files — NOT by 7 distinct CSS files.

**Not a REJECT:** the gate is correct and cannot be cheated by erasure. The residual is that the
DECISION is deferred and a downstream implementer could mis-read "preserve-profile-distinctness"
as "keep 7 CSS files" and manufacture 7 fake roots. S-P3 binds collapse-to-one as the disk-grounded
answer.

---

## §6 — R-A0-3: the honest-finding "named primitive" escape is the largest paper-close surface

**Finding (MEDIUM, addendum L1).** The contract's own PASS-IMPL V4 close row names this:
"**the honest-finding escape is itself the single largest paper-close surface in the contract**"
(`SYNTHESIS.md:342`). The escape: if a grammar-derived parser CANNOT preserve >SOTA without
hand-shaping, the hand-shaping survives as a "named validated grammar-parameterized primitive"
(HANDOFF §6), gated (a) the `.bbnf` INVOKES it by name; (b) it is parameterized by grammar-derived
DATA (alphabet/delimiter from the rule shape); (c) it carries `verbatim_blob_present == false`.

This is where a verbatim blob re-enters wearing a "primitive" label. A "primitive" failing
(a)-(c) is a relabeled hand-written blob — but the gate is **prose-reviewed-at-admission**
(`SYNTHESIS.md:342` "reviewed at admission"; alphaE cross-cutting §2 "the §0.4 prose obligation …
reviewed at admission" is the human backstop). Prose review at admission is exactly the SK-V13
failure mode (the build-first-audit-never pattern) re-applied at the escape hatch.

**S-P0 BINDING (carry into S-P0 + every G1/G2/B4 redress CHALLENGE):**
1. The (a)-(c) gate must be MACHINE-CHECKED, not prose-reviewed: (a) `grep` the primitive's name
   in the `.bbnf` (it must be a callable the grammar references); (b) **the primitive's EMITTED
   OUTPUT must VARY under a `.bbnf` mutation of the invoking rule's shape** — the per-primitive
   mutate-falsifier (the whole-path `.bbnf`-mutation test of §1-L1 applied to the primitive's output
   in isolation). CH2 R1 fold: (b) is NOT merely "accepts a grammar-derived argument" — a primitive
   that accepts a grammar-derived ARGUMENT yet splices a FIXED body keyed off it (argument decorative,
   body verbatim) does NOT vary under the mutation and fails (b) the same way a const courier fails
   the whole-path test. This makes (b) a MACHINE predicate (the one of the three previously
   expressible as prose), closing the one-level-down prose-review seam. (c) `verbatim_blob_present
   == false` on the primitive surface (the same telemetry as any derived surface). A primitive that
   splices a const the emitter holds — not invoked from the `.bbnf` — is a verbatim blob (L1),
   REJECT-REDRESS.
2. The honest-finding escape is a LEGITIMATE outcome (a genuine "the generator cannot lower Pratt"
   finding is real, not a paper-close) — but the result must be a PLUGGABLE, `.bbnf`-invoked,
   parameterized primitive with a checkasm/scalar reference (the [pluggable-components] +
   [abrogate-before-patch] discipline), NOT a silent `_RS` block. S-P0 binds that the escape is
   the abrogate-before-patch outcome (ask "can the generator lower it?" before "can we splice a
   primitive?"), not the default.

**Not a REJECT:** the escape is correctly gated and correctly flagged as the largest paper-close
surface by the contract ITSELF. The residual is that its gate is prose-reviewed; S-P0 binds it
machine-checked.

---

## §7 — The PRUNE list (lands FIRST, unchanged; PRUNE-before-GENERALIZE-before-PROVE)

The standing order is **PRUNE → GENERALIZE → PROVE → HONESTY**, with PRUNE landing FIRST
(it reduces surface for the GENERALIZE waves and makes the Lock-14 gate trustworthy BEFORE the
emitter rebuild). The prune list is disk-verified (§2) and unchanged by this audit:

| Prune | Action | Disk witness | Gate column |
|---|---|---|---|
| **P1** | DELETE the WHOLE x86 surface crate-wide, deletion list (a)-(i) reach-matched to the verify grep — `src/x86_64/` (24 files) + `ext/x86/` (4 files vendored ASM) + nasm `build.rs` + `nasm-rs="0.3"` `Cargo.toml:19` dep + `lib.rs:5`/`:247`/`:285-288` + doc surfaces + (h) `checkasm_parity.rs` 9 compile-coupled sites DECOUPLE-OR-DELETE + (i) `byte_class_from_eq_set_64.rs:10-15` doc-string scrub; build-sound (`cargo test --no-run` clean) | `bbnf-simd/{src/x86_64,ext/x86,build.rs,Cargo.toml:19}`; `checkasm_parity.rs`; `scalar/byte_class_from_eq_set_64.rs` | `x86_tree_deleted == true` |
| **P2** | DELETE the OLD contrived warm CSS bench (`nonjson_css_l4.rs lightningcss_facts:528` + 3 siblings + warm `measure_mbps` SHA-fixture path); KEEP `css_canon_bench` + the 9-field `assert_rich_strict_equality` oracle | `nonjson_css_l4.rs:528,3091` | `corpus_in_timer == true` |
| **P3** | COLLAPSE the 7 byte-identical CSS replicas → ONE CSS grammar (disk: one `stylesheet.bbnf`, 7 profiles are bench-row metadata; §5 binds collapse-to-one, NOT 7 fake roots) | 7× md5 `b654562c…`; `regen_css.rs:35-` TARGETS | `runtime_target_rows_collapsed == true`, `generated_md5_distinct == true` |
| **P4** | MAKE the Lock-14 gate meaningful — move `runtime_generator.rs`/`json_sink_direct.rs`/`json_templates/`/`grammar_provider.rs` from weak `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442` INTO strict `GENERIC_SCAN_ROOTS:2409`; drop `diagnostic-x86:2463` (x86 gone, P1); extend `FORBIDDEN_GENERIC_TOKENS` with `CSS_`/`_RS` patterns. MUST land BEFORE G2/G3 emitter rebuild | `lock14_baseline.rs:2409,2420,2442,2463` | `lock14_gate_scans_codegen == true` |
| **P5** | PURGE `parse_w11_1_number` (×7) from shipped JSON `generated.rs` — fix at the generator/template source so `regen --check` stays clean | `json/generated.rs` (7 hits) | `metalang_leak_present == false` |

**Sequencing constraint (binding):** P4 lands BEFORE B1/G2/G3 so the un-forked emitter is
scanned for neutrality AS it is built. The entry-gate dependency chain
**PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1** is binding: a wave that fails its exit gate
BLOCKS every downstream wave (G1 failure blocks G2/G3/G4/PROVE; G3 un-fork failure blocks PROVE,
which emits Sheets THROUGH the un-forked generator). No GENERALIZE wave dispatches over a
REDRESSed predecessor or a RED prune gate.

**CH3 R1 fold (revert-graph prose disambiguation — carry into SYNTHESIS §5 fact 3 + a2 §4):**
G2 entry-gates on BOTH G1 AND P3 (the dual entry-gate) — **a P3 failure ALSO blocks G2 independent
of G1**, so "G1 failure blocks G2" is necessary-not-sufficient prose; the binding edge is the dual
gate. G3 is ordered AFTER G1/G2 (PRUNE → G1 → G2 → G3); the binding directional revert claim is
"**G3-failure blocks PROVE**" (G1/G3 co-derive — the un-fork is the discharge of REDRESS-W2-1, not a
re-open), NOT "G3 gates G1/G2" (that inverts the arrow). The dependency graph itself never inverts;
these are prose restatements that must read at the graph's direction.

**CH4 R1 fold (G1 LOC budget — carry into the alphaE feeder + every G1 budget citation):** the G1
`json/generated.rs` ±5% line-count tripwire (today 1235 LOC, ±5% = ±62 LOC) is a SOFT tripwire ONLY,
NOT a hard reject — a faithful projection that legitimately reorders/dedupes/renames
(`parse_w11_1_number`→`parse_number`, P5) could exceed ±5% while remaining a true projection. The
BINDING cost-control is the byte-for-byte `json_templates/` oracle diff-match BEFORE deletion (the
G1 row binds it as primary). State the ±5% as soft and the oracle diff-match as binding wherever the
G1 budget is cited.

---

## §8 — What is CLEAN (S-P0 affirmative findings, not papered)

1. **Substrate is the genuine foundation (Lock 1 holds).** One `Tape`/`ValueRef`/`PayloadArena`
   (`tape/mod.rs:94,175,38`); both grammars ride it; CSS at-rule tag reuses the sparse flag pair —
   no second tape. The generator is built ON it; the substrate is NOT touched. This is the genuine,
   generalizable bedrock — NOT overfit.
2. **The 6 addenda are bound THREE ways each** (close gate + §0.4 pre-block + a telemetry column
   the `gate-json` consumer REJECTs on). This is the strongest anti-overfit posture in any skinny
   tranche — the inverse of SK-V13's exclusion-gate.
3. **No GENERALIZE close-condition preserves a hand-written blob.** Every G-wave verify clause is a
   neutral grep (`generator_grammar_branch_count==0`, `generator_grammar_type_count==0`,
   `verbatim_blob_present==false`) + a >SOTA-preservation floor. The hand-written parsers become
   parity ORACLES (G1: byte-for-byte diff-equal), then are deleted — they are NOT the product.
4. **The by-exclusion P3 projection is reach-complete** (§2.4) — the most-litigated gate lands in
   its strongest form (md5 → grep-alphabet → grep-cannot-fire → row-count → by-exclusion-modulo-path).
5. **The (h)/(i) x86 reach is discharged in the binding contract** (§2) — the V6-confirm CH7-R1
   propagation residual is folded; the P1 gate is satisfiable-by-construction (deletion list
   (a)-(i) reach-matched to the crate-wide verify grep + build-soundness).
6. **The kept-honest inventory is explicit** (alphaE cross-cutting §3): `css_canon_bench` +
   `w2_rich_cssom_bench`, the 9-field oracle, the 12 checkasm single-kernel differentials +
   `checkasm_common.rs` + `checkasm_parity.rs` (KEPT-and-DECOUPLED, aarch64 parity assertions
   retained), the substrate. The prune does not throw the aarch64 hardening out with the x86 bathwater.

---

## §9 — Sub-agent scope handoff (A1-A3 + SYNTHESIS-AUDIT-OVERFIT, per PASS-0 §2)

S-P0 fans out; A0 (this artefact) owns the goalset/shortlist residual-overfit axis + the 6-addenda
formalization. The remaining axes (per `PASS-0-OVERFIT-AUDIT.md` axis table, mapped to the
generalization surface):

- **A1 (measurement integrity):** verify the CSS >SOTA framing residual (R-A0-1) at the harness
  level — `css_canon_bench` corpus-in-timer + the lazy-vs-eager plane; confirm JSON 51/51 cold
  strict same-plane; the +1.4% apache_builds thinnest tripwire is load-bearing for G1.
- **A2 (admit-mechanism / generator-vs-hand-curated):** the verbatim-blob + single-emitter-path
  axes — `CSS_GENERATED_RS`, `json_sink_direct::render`, `RuntimeEmitterKind`; the round-trip
  `.bbnf`-mutation falsifier for G1/G2.
- **A3 (Lock-14 generic-crate + phantom + acceleration-wiring):** P4 gate meaningfulness, the
  `ValueRef<G>` phantom (L4), the CSS NEON cfg(test)-only deadness (L6), the JSON bespoke scanner
  (G5).
- **SYNTHESIS-AUDIT-OVERFIT.md:** consolidate A0-A3 into the single prune list (§7 is the seed) +
  the 3 residual seams (R-A0-1/2/3) as binding CH7-lensed carries into every downstream wave.

**V2 fold dispositions the SYNTHESIS-AUDIT-OVERFIT consolidation must absorb (CH-traceable):**
- **CH6 R1** → the SYNTHESIS R-A0-1 row must carry the explicit REJECT clause for unqualified
  "beats CSSOM"/"equal-work" close language behind a re-label (§4 binding item 1).
- **CH6 R2** → the SYNTHESIS R-A0-2 row must carry the disk-grounded collapse-to-one answer
  (`generator_grammar_count == 3` = json+css+sheets) rather than deferring it to S-P3 (§5 binding item 1).
- **CH5 R1** → the SYNTHESIS §5 fact-5 R16 recipe-pin must state the full-expanded-row altitude
  (both nested structs `frontend_requirements` + `output_labels`; `#[derive(PartialEq)]` on
  `RuntimeTarget` as the one-line cleanest mechanism) (§1-L2 + §2.4).
- **CH3 R1** → SYNTHESIS §5 fact-3 must annotate the dual entry-gate (P3 failure blocks G2
  independent of G1) and the directional revert claims (§7 sequencing fold).
- **CH4 R1** → wherever the G1 LOC budget is cited (alphaE feeder + SYNTHESIS §2.1) the ±5% is SOFT,
  the oracle byte-for-byte diff-match is the BINDING cost-control (§7 sequencing fold).
- **CH2 R1** → the §6 honest-finding-escape (b) predicate is the per-primitive mutate-falsifier
  (§1-L1 + §6 binding item 1), not "accepts a grammar-derived argument."
- **CH1 R1** → the CSS courier body span is disk-measured 910 LOC (`:701`→`:1611`); the V3-seed
  646–910 estimate is superseded (a1 annotation; §1-L1 witness here). **CH7: 0 REVISE.**

CH7 (Overfit-Prune) is the binding lens: every wave's CLOSE must show grammar-derived (not
verbatim-blob), Lock-14 compliant, real-source-change admit, round-trip-clean generated output,
NO scaffold-only landing. CH7 REJECT triggers immediate revise/revert — it cannot be carried
"acknowledged but not blocking."

---

## §10 — TALLY

Residual findings: **3 (R-A0-1 framing-escape MEDIUM · R-A0-2 P3-decision-deferred MEDIUM ·
R-A0-3 honest-finding-escape-prose-reviewed MEDIUM). 0 CRITICAL. 0 HIGH. 0 REJECT.**
Ground-truth claims verified: **16/16 dispositive claims disk-true at HEAD `83b66db42`.**
6 addenda formalized as binding S-P0 lenses L1-L6 (§1). PRUNE list (§7) disk-verified, unchanged,
lands FIRST. The goalset spine + the 6 addenda + the prune list are structurally sound and
disk-true; the 3 residuals are contrivance/paper-close SEAMS the addenda exist to police,
sharpened to machine-checked S-P0 bindings — NONE blocks G-Alpha; all bind downstream.

**V2 fold (CH1–CH7 V1 dispositions resolved; zero orphan REVISE):** all 6 V1 REVISEs folded into
this a0 (CH1 R1 910-LOC pin §1-L1; CH2 R1 per-primitive (b) mutate-falsifier §1-L1+§6; CH5 R1
both-nested-struct recipe altitude + `PartialEq`-derive cost §1-L2+§2.4; CH6 R1 "beats"-REJECT clause
§4; CH6 R2 collapse-to-one carry-up §5; CH3 R1 dual-entry-gate + directional-revert §7; CH4 R1 G1
±5%-soft / oracle-binding §7) — the CH3/CH4/CH6 folds also seed the SYNTHESIS-AUDIT-OVERFIT
consolidation (§9 traceability list). Every fold disk-re-verified at HEAD `83b66db42`
(`RuntimeTarget` = 12 fields, derives `Clone, Copy, Debug` only; `RuntimeFrontendRequirements` +
`RuntimeOutputLabels` both `PartialEq, Eq`; CSS courier span `:701`→`:1611` = 910 LOC;
`frontend_requirements` uniform `REQUEST_FACTS_REQUIREMENTS` ×7). CH7 V1 = 100%, 0 REVISE.

**V3 confirm closure (V2 CH1–CH7 = 7×100% ACCEPT, zero REVISE, zero REJECT):** the V2 hardening
pass independently re-grepped every dispositive witness at HEAD `83b66db42` and DISCHARGED all seven
V1 REVISEs on disk — CH1 ACCEPT 10 (R1 discharged), CH2 ACCEPT 6 (R1 discharged), CH3 ACCEPT 6
(R1 both-halves discharged), CH4 ACCEPT 5 (R1 discharged), CH5 ACCEPT 6 (R1 both-parts discharged),
CH6 ACCEPT 7 (R1+R2 discharged into the consolidated R-A0-* rows), CH7 ACCEPT 7 (zero REVISE; the
7 fold edits introduced no new defect, none softened a REJECT-class V3 finding). a0 inherits ZERO
orphan REVISE/REJECT. Two-consecutive ≥95% met per lens (CH2 87.5%→100%, CH4 83.3%→100%, others
100%→100%). The two sub-REVISE cosmetics the V2 pass recorded are non-blocking: CH1's "alphaE §A"
label (quote verbatim-exact at `alphaE-candidate-shortlist.md:109`, Risk bullet) is folded this V3
cycle (§5); CH7's a3 field-number paraphrase is a3-local (not a0). No gate, disposition, or
ground-truth witness moves. The 3 residuals (R-A0-1/2/3) stand as the binding downstream lenses;
the PRUNE list (§7) lands FIRST, unchanged.
