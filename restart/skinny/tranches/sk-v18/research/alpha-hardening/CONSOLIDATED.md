# SK-V18 Pass-Alpha CHALLENGE — alpha-hardening CONSOLIDATED (§3Z verdict)

Pass Alpha SK-V17→SK-V18 — the GENERALIZATION cycle (the inflection backtrack).
Seven-lens CHALLENGE (CH1 Correctness · CH2 Generality · CH3 Regression · CH4 Cost ·
CH5 Hidden-Coupling · CH6 Next-Tranche-Impact · CH7 Overfit-Prune) over the αF contract
(`SYNTHESIS.md` + `HANDOFF.md`) + the αA–αE feeders. Per `PASS-ALPHA.md` §2 (alpha-A..F),
§3 (CHALLENGE), §4 (goalset), §3Z + `ORCHESTRATOR.md` §3W/§3Z. Bracket HEAD `318d9c046`;
SK-V17 closed `f6a38445b`; V3 audit `7dbe44c22`. Subject is **NOT a new-feature cycle**:
JSON + CSS are both >SOTA with a working value API (SK-V17, `f6a38445b`); per the binding
principle SK-V18 BACKTRACKS the hand-written/forked parsers into ONE grammar-driven
generator emitting all grammars from `.bbnf`, over the unified tape/`ValueRef` substrate,
shared value API, PROVEN on a 3rd grammar (Sheets), PRESERVING >SOTA.

## §1 — Per-cycle convergence trajectory (the V≤5 ceiling iteration)

| Cycle | ACCEPT rate | Posture |
|---|---|---|
| V1 | **74.2%** | First clean pass; 24 tightening REVISEs / 0 REJECT; folded to V2 |
| V2 | **96.7%** | First ≥95%; goalset Lock-14/Sheets/ValueRef/revert folds landed |
| V3 | **94.8%** | Sub-95% straggler wave (F13 relocated-seam attribution; checkasm "18"; FOLD-1 second-x86-surface authored in αC/SYNTHESIS/HANDOFF) |
| V4 | **92.7%** | Sub-95%; 5 REVISE clusters — FOLD-1 orphan into αA/αE feeders, P1 deletion-list reach (RED-by-construction), `runtime_target_rows_collapsed` projection tuple, ledger-anchor drift |
| **V5** | **97.9%** | Ceiling iteration (V≤5). 95 ACCEPT / 2 REVISE / 0 REJECT across 97 dispositions |

The two V5 REVISEs are the §3Z-blocking residual (see §4). The §3Z requirement is
**≥95% × 2 consecutive AND zero orphan REVISE AND V≤5**; V5 is at the V≤5 ceiling, and is
the first ≥95% cycle since V2 (V3/V4 dipped sub-95%), so a second consecutive ≥95% has
NOT yet been recorded by an orphan-free wave — and two orphan REVISEs remain open.

## §2 — V5 per-lens tally (re-verified live at HEAD `318d9c046`)

| Lens | ACCEPT | REVISE | REJECT | V4 → V5 | Verdict |
|---|---|---|---|---|---|
| CH1 Correctness | 7 | 0 | 0 | 85.7% → **100%** | V4 αE x86-scope orphan RESOLVED (F15 crate-wide disk-true 3554/847/102); 2 cosmetic prose nits sub-REVISE |
| CH2 Generality | 31 | 1 | 0 | 96.7% → **96.9%** | V4 §8.1 F16 folded orphan-free; NEW §8.1: F16 OPERATIVE enumeration (8 fields) ⊊ its own PROSE ("all but path columns"), OMITS `profile` (7-distinct per-profile discriminator) + `source_inputs`/`metadata_inputs` |
| CH3 Regression | 7 | 0 | 0 | 71.4% → **100%** | both V4 CH3 REVISEs (αE false-green `src/`-scoped gate; stale αA census) discharged on disk; no re-open; no stranded >SOTA |
| CH4 Cost | 6 | 0 | 0 | 100% → **100%** | no orphan V4 REVISE; F15 cost-FAVOURABLE (−847→≈−4500); F16 cost-FREE (zero-LOC projection); net ≈ −12650…−12850 |
| CH5 Hidden-Coupling | 24 | 1 | 0 | 100% → **96.0%** | 4 core axes structurally honest + disk-verified; NEW F.6: binding P1 deletion list NARROWER than its crate-wide grep — `tests/checkasm_parity.rs` (11 active `x86_64::` imports, compile-coupled) + `src/scalar/byte_class_from_eq_set_64.rs` off the (a)-(g) list |
| CH6 Next-Tranche-Impact | 13 | 0 | 0 | 84.6% → **100%** | both V4 REVISEs (BLOCKING §1 P1 reach; §13 ledger drift) folded as binding gate text; no new paper-close surface |
| CH7 Overfit-Prune | 7 | 0 | 0 | 87.5% → **100%** | all 5 V4 CONSOLIDATED clusters folded orphan-free; six addenda fire honestly; the four-pass straggler ABSENT |
| **Total** | **95** | **2** | **0** | — | **97.9%** |

Wave aggregate **95/97 = 97.9%** (above the §3Z ≥95% bar). **Zero REJECTs.** Every REVISE
is a convergence-cheap tightening with a concrete in-place fix, not a finding reversal and
not a re-opened pre-block. CH4 and CH6 swung from sub-threshold to 100% on the fold;
CH1/CH3/CH7 closed their orphans; the only open defects are the two NEW one-level-deeper
sharpenings (CH2 §8.1 and CH5 F.6), both in the same necessary-not-sufficient lineage.

## §3 — §3Z VERDICT

**Converged = false.** V5 clears the ≥95% accept-rate bar (97.9%) at the V≤5 ceiling and
records zero REJECT, but FAILS the §3Z conjunction on two counts:

1. **Two orphan REVISEs remain open** (CH2 §8.1, CH5 F.6). §3Z requires **zero orphan
   REVISE**; these are NEW V5 findings with no V6 fold landed yet.
2. **Two-consecutive ≥95% not yet recorded by an orphan-free wave.** V3 (94.8%) and V4
   (92.7%) both dipped sub-95%; V5 (97.9%) is the first ≥95% since V2 but carries 2
   orphans, so the consecutive-pair condition is unmet.

**V5 is the V≤5 ceiling.** Per PASS-ALPHA §8 / ORCHESTRATOR §3Z this is the escalation
boundary: a sixth confirming cycle to close the two residual REVISEs would exceed V5.
Both residuals are mechanical enumeration/reach folds (NOT architecture, NOT a re-open,
NOT a stranded >SOTA) — each carries a concrete, disk-grounded, single-edit fix that the
orchestrator may fold and confirm, OR escalate to the user per the V>V5 path. The contract
spine, goalset, and all six addenda are structurally sound and disk-true; the two open
defects sharpen enforcement reach on an already-correct mechanism. **The verdict the
orchestrator carries to G-Alpha: substance-converged at 97.9% with two named,
single-edit-fixable residual REVISEs, no REJECT, no orphan architectural defect.**

## §4 — The two residual REVISEs (the orphans blocking §3Z)

### REVISE-1 — CH2 §8.1: the F16 `runtime_target_rows_collapsed` machine-check is two non-equivalent forms; the OPERATIVE enumeration omits `profile`

The V4 §8.1 fold (F16) correctly widened the relocated-overfit-seam structural check from
the V3-too-narrow `(source_roots, entry_rule)` projection — but states the gate in TWO
non-equivalent forms at the same site (`SYNTHESIS.md:553`, `:397`, `:322(iii)`;
`HANDOFF.md:24`, `:273`; αE `:156`, `:197`, `:207`, `:236`; αC `:254`, `:269`, `:432`,
`:619`):

- **PROSE form (correct + complete):** "all `RuntimeTarget` rows sharing one `grammar_name`
  byte-identical in EVERY field except the generated-artefact path columns
  (`output_dir`/`expected_files`)".
- **OPERATIVE form (the machine-check):** `count(distinct config-tuple-minus-output_dir) == 1`
  over the **enumerated** set `fact_schema`/`row_id`/`output_plane`/`emitter`/`entry_rule`/
  `source_roots`/`check_command`/`frontend_requirements`.

Empirical refutation live at HEAD (`skinny/xtask/src/regen.rs:6` + `regen_css.rs`): the
`RuntimeTarget` struct has **12 fields**; prose-minus-path-columns covers 10; the
enumeration names 8 and OMITS three — **`profile`** (`grep -E 'profile: "css_l4' regen_css.rs
| sort -u | wc -l` → **7 DISTINCT**, the single most explicit per-profile router), plus
`source_inputs` and `metadata_inputs`. An un-forked emitter dispatching
`match target.profile { "css_l4_visual_functions" => … }` — the most natural relocated seam
— sails through the enumerated `count(distinct config-tuple) == 1` because `profile` is not
in the named tuple. Same md5 → grep-alphabet → grep-cannot-fire → row-count-projects-2-columns
necessary-not-sufficient lineage, carried one level deeper INTO the F16 fix itself.

**Fix (REVISE, not REJECT — enumeration-completeness, mechanism correct):** make the
operative machine-check equal the prose by **enumerate-by-EXCLUSION** of the two path
columns (`output_dir`, `expected_files`) — implement as a struct-level `#[derive(Hash)]`
minus the two path fields, so any future `RuntimeTarget` field cannot silently fall outside
the tuple; name `profile` explicitly wherever the enumeration is restated; keep the P3
collapse mechanism + the §0.4 prose obligation. RED today (7 distinct `profile` AND 7
distinct `fact_schema`); GREEN only post-P3-collapse — correct against the actual close
condition.

### REVISE-2 — CH5 F.6: the binding P1 x86 deletion list is narrower than its own crate-wide verify grep (`tests/` + `scalar/` escape)

The V4→V5 fold widened the binding P1 deletion list to (a) `src/x86_64/`, (b) `ext/x86/`,
(c) `build.rs`, (d) `lib.rs:247` ref, (e) `Cargo.toml:19` nasm dep, (f) `lib.rs:5 pub mod
x86_64;` + `:285-288` cfg arms, (g) in-crate doc surfaces — and the binding verify grep is
`grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' bbnf-simd/`
(`SYNTHESIS.md:315`/`:563`, `HANDOFF.md:110`), asserted "reach-matched … satisfiable-by-
construction." Re-grepped live, that grep fires on **5 `.rs`/`.toml` files**, TWO of which
are NOT on the (a)-(g) list:

1. **`tests/checkasm_parity.rs`** — `grep -cE 'x86_64'` = **11**; `:454-482` actively `use`
   the module target (f) deletes (`bbnf_simd::x86_64::avx2::classify::classify_block_scalar`,
   `…avx2::bmi2_emit::compact_mask_scalar`, `…avx512_vbmi2::classify::…`,
   `…avx512_gfni::classify_affine::…`). It is **compile-coupled** to `pub mod x86_64`:
   executing exactly (a)-(g) deletes the module and `checkasm_parity.rs` fails to compile —
   an unnamed coupling the contract does not surface; AND the grep stays RED on its 11 hits
   → **RED-by-construction**, the exact mirror-defect CH6 V4 §1 claimed to close, re-incurred
   one reach level deeper (the V4 fold audited `src/`, `ext/`, `build.rs`, `Cargo.toml`,
   `lib.rs` but never `tests/`).
2. **`src/scalar/byte_class_from_eq_set_64.rs`** — `:10,12,15` comment-only `AVX-512 BW`/
   `AVX2` cross-references; benign body, but `--include='*.rs'` fires → RED unless scrubbed.

αC (the research feeder, §4 C.1 / `:168-179` / `:196`) is **reach-complete** — it EXPLICITLY
names dropping the residual x86 strings in `src/scalar/byte_class_from_eq_set_64.rs` +
`tests/checkasm_parity.rs` and scopes the grep over `src/`, `ext/`, `build.rs`, `Cargo.toml`,
`tests/`. The defect is **propagation**: αC's `tests/`+`scalar/` reach was NOT carried into
the BINDING SYNTHESIS P1 row (`:315`), the `x86_tree_deleted` telemetry (`:563`), or the
HANDOFF P1 receiver (`:101-112`).

**Fix (REVISE, not REJECT — mechanical fold; αC carries the verbatim fix):** add to the
binding P1 list + `x86_tree_deleted` telemetry two removal targets — **(h)**
`tests/checkasm_parity.rs:454-482` re-homed/deleted (closes the compile-coupling AND the
grep RED); **(i)** `src/scalar/byte_class_from_eq_set_64.rs:10-15` doc x86 cross-refs
scrubbed aarch64-neutral. With (h)+(i) the list is genuinely reach-matched (5 firing files,
5 named removal targets) and satisfiable-by-construction.

## §5 — LOCKED SK-V18 generalization goalset (the inflection backtrack)

The CHALLENGE-survived, R10-binding goalset — one grammar-driven generator, NOT a feature set:

1. **ONE grammar-driven generator emitting all grammars from `.bbnf`.** Retire the
   `RuntimeEmitterKind` JSON-vs-CSS fork (`grammar_provider.rs:40`); one grammar-agnostic
   emitter over the `SinkOnlyProgram`/`BackendShape` 5-shape lowering. JSON projects from
   grammar (the hand-written template becomes a byte-for-byte parity oracle, NOT the
   product); CSS lowers from grammar (retire the `CSS_GENERATED_RS` const-`&str` blob,
   `runtime_generator.rs:701`). Lock-14: zero grammar-named branches in generic crates;
   the gate (P4) actually scans `codegen/src` AND `xtask/src` (arm census + type census +
   the structural `runtime_target_rows_collapsed` collapse).
2. **Shared value API.** One `Value`/`Document`/`Cursor` trait both JSON and CSS instantiate
   (≥2 NON-test production impls); lazy over the EXISTING `Tape`/`ValueRef` (no second
   substrate, Lock 1); rich-AST preserved (`json_rich_navigation_preserved`, no LCD-flatten).
   The phantom `<G>` (`tape/mod.rs:175 G: EventGrammar = AnyGrammar`, the PHANTOM axis, NOT
   the real `K=Kind`) is INSTANTIATE-or-DELETE — DELETE is the abrogate-before-patch DEFAULT
   (`CssEventGrammar` does not exist at HEAD).
3. **Sheets proof (the honest generalization litmus).** The real 185-LOC Pratt
   `grammar/google-sheets/google-sheets.bbnf` (`error_literal`/`cell_ref`/precedence-tower
   shapes NO JSON/CSS rule exercises) ADOPTED into the benched tree and emitted via the
   generator ONLY — md5(Sheets) ≠ JSON ≠ CSS, instantiates the G4 trait, ZERO hand-authored
   runtime Rust, `sheets_grammar_shape == pratt-operator` (a flat-stream/tree REJECTed as
   third-JSON hollowing). The 25-LOC `sheets_witness/` stub retired.
4. **Preserved >SOTA from the grammar-DERIVED parsers.** JSON > sonic-rs strict (the
   `IgnoredAny`+`.end()` strict skipper; range +1.4%…+164.7%, apache_builds thinnest at
   +1.4% the load-bearing tripwire, unicode_escapes widest); CSS > lightningcss (N=200
   `css_canon_bench` per-row floors, H1-framed lazy-rich-summary vs eager-full-CSSOM). A
   derived parser that loses the speed or the equality is not done; the honest-finding
   escape is GATED ((a) `.bbnf`-invoked by name, (b) grammar-derived DATA, (c)
   `verbatim_blob_present == false`) — not a paper-close hatch.
5. **x86 DELETED (aarch64-only).** NO x86 surface anywhere in `bbnf-simd` — crate-wide,
   reach-matched: `src/x86_64/` (847) AND `ext/x86/` (3554) AND `build.rs` (102) AND the
   `nasm-rs` Cargo.toml dep AND `lib.rs:5`/`:247`/`:285-288` AND in-crate doc surfaces.
   `x86_tree_deleted` is the R10 binding pin (subject to REVISE-2's `tests/`+`scalar/` reach
   completion).

**R10 success criterion:** one generator emits JSON + CSS + Sheets from `.bbnf`; shared
value-API trait both instantiate; phantom `<G>` instantiated-or-deleted; >SOTA preserved
honestly; x86 gone; Lock-14 gate meaningful; regen --check clean; PASS-IMPL V4 accepts every
axis or records intrinsic-block proof.

## §6 — Candidate-wave shortlist (5 candidates, survived CHALLENGE, additive-by-deletion)

PRUNE → GENERALIZE → PROVE → HONESTY. No candidate added or removed across V1–V5; the
shortlist is additive-by-deletion (removes overfit, never resurrects rejected architecture);
no re-opened REDRESS pre-block.

| # | Candidate (V3 items) | Risk | LOC Δ | Entry-gate | Falsifier |
|---|---|---|---|---|---|
| **A** | PRUNE overfit/x86/contrivance (P1–P5) | LOW | ≈ −10800 (P1 ≈ −4500 BOTH x86 surfaces) | first | x86=0 crate-wide reach-matched; 7 replicas collapsed (per-`grammar_name` config-tuple collapse over all non-path columns); Lock-14 gate meaningful; metalang purged |
| **B1** | un-fork emitter + project JSON (G3+G1) | MED | ≈ −800 | A | single-emitter-path + neutral body (arm/type census); grammar-derivation-proof (`.bbnf` mutation test — a const courier cannot pass); JSON >sonic preserved (apache +1.4% tripwire) |
| **B2** | derive CSS from lowering (G2) | LOW | ≈ −1500 | B1, P3 | verbatim-blob retired; CSS >lightningcss preserved (N=200 per-row floors, H1-framed); distinct+neutral+config-tuple collapse |
| **B3** | shared value trait + kill phantom `<G>` (G4+H1) | MED | ≈ ±0 | B1, B2 | phantom-generic resolved (DELETE default, test-excluded grep); shared trait both-impl + rich-ast reachable THROUGH it; zero-cost (no vtable in hot path) |
| **B4** | PROVE Sheets + scanner/NEON honesty (PROVE+G5+G6) | MED-HIGH | ≈ +250 (capped) | B1, B2, B3 | 3 md5-distinct generated.rs; Sheets 0-hand-LOC from google-sheets.bbnf; accel-wired-at-admission (NOT `#[cfg(test)]`); JSON preserved |

**Net LOC ≈ −12650…−12850** — a generalization cycle that deletes far more than it adds
(the inflection backtrack collapses two forked hand-written parsers + replicas + BOTH x86
surfaces into one generator).

### What survived CHALLENGE (the PRUNE+GENERALIZE+PROVE+HONESTY shortlist)

- **PRUNE (A / P1–P5):** delete the WHOLE x86 surface crate-wide (P1, F15 — reach-matched
  modulo REVISE-2's `tests/`+`scalar/`); delete the old warm contrived CSS bench
  (`nonjson_css_l4.rs` `measure_mbps`, 187-byte SHA fixtures, P2) while KEEPING the honest
  `css_canon_bench`/`w2_rich_cssom_bench` + 9-field `assert_rich_strict_equality` oracle;
  collapse the 7 byte-identical css_l4 `generated.rs` replicas AND the 7 xtask
  `RuntimeTarget` rows to one config (P3); make the Lock-14 gate meaningful — land P4 BEFORE
  the B1/G2/G3 emitter rebuild; purge the `parse_w11_1_number` metalang leak (P5, ×7 live).
- **GENERALIZE (B1–B3 / G1–G4):** un-fork + project JSON (the SK-V17 REDRESS-W2-1
  single-emitter SUBJECT admitted to discharge, NOT a re-open); derive CSS from lowering
  (LOW risk — scalar hot path, no fragile kernel to preserve); shared value trait +
  instantiate-or-delete the phantom `<G>` on the RIGHT (`G`) axis.
- **PROVE (B4 / PROVE+G5):** Sheets through the generator ONLY (the load-bearing
  non-hollow litmus — a real Pratt grammar, gated `sheets_grammar_shape == pratt-operator`,
  config-tuple distinct from css_l4 + json); migrate JSON's bespoke scanner (`json/scan.rs`,
  the speed holdout) onto the neutral NEON kernel.
- **HONESTY (B4 / G6 / H1):** wire-or-retire the CSS NEON honestly at admission (retire
  gated on a samply non-top-N MEASUREMENT, not an assertion); ASM backlog aarch64-only
  (PMULL/UDOT/TBX/CSSC + 5 scalar-passthrough kernels, each WITH its hot-path consumer or
  retired); reframe CSS >SOTA lazy-rich-summary vs eager-full-CSSOM (`materialization_framing`,
  `corpus_in_timer`); `regen --check` clean.

## §7 — G-Alpha presentation summary

**Subject:** SK-V18 = the GENERALIZATION cycle (the inflection backtrack). NOT a new-feature
cycle — JSON + CSS are both >SOTA with a working value API (SK-V17, `f6a38445b`); SK-V18
backtracks the hand-written/forked parsers into ONE grammar-driven generator over the
unified tape/`ValueRef` substrate, shared value API, PROVEN on Sheets, PRESERVING >SOTA.

- **Waves (PRUNE → GENERALIZE → PROVE → HONESTY, dependency-ordered):** PRUNE P1–P5
  (P4 lands BEFORE the emitter rebuild) → GENERALIZE G1 (JSON projection) → G2 (CSS
  lowering) → G3 (un-fork emitter) → G4 (shared trait + phantom) → G5 (JSON scanner onto
  neutral NEON) → G6 (CSS NEON wire-or-retire + ASM backlog) → PROVE (Sheets via generator
  only) → HONESTY (H1 framing + regen --check). Each primitive lands WITH its hot-path
  consumer in the same commit (no orphan kernels).
- **LOC:** net ≈ **−12650…−12850** (A ≈ −10600…−10800 with P1 ≈ −4500 deleting BOTH x86
  surfaces; B1 −800; B2 −1500; B3 ±0; B4 +250 capped). Deletes far more than it adds.
- **Hard caps:** standing `[dispatch-hard-cap]` defaults — research/plan/redress **20/15/30
  min**, "at 0.9N commit, at N halt"; the Sheets/NEON cluster (B4) is MED-HIGH and may carry
  a documented larger cap. Revert dependency graph: PRUNE → G1 → G2 → G3 → G4 → G5/G6 →
  PROVE → H1, a failed exit gate BLOCKS every downstream wave (G1 failure blocks
  G2/G3/G4/PROVE; G3 un-fork failure blocks PROVE). Revert protocol + caps + per-wave
  triumvirate sanctioned-deferred to S-P3 (PASS-ALPHA §4.4) with these two binding carries.
- **Pre-blocked routes (no re-open):** AZ-IV eager value-tree, StructRegistry per-leaf
  indirection, fact-stream-as-output, 24-row broadcast, FNV-in-runtime, x86/AVX/SVE/nasm
  (now crate-wide). Plus the six addenda re-entries: verbatim-blob, distinct-grammar-output,
  single-emitter-path fork, phantom-generic, timed-plane-symmetry+corpus-in-timer,
  acceleration-wiring — each bound THREE ways (close gate + §0.4 pre-block + machine-checkable
  telemetry column the `gate-json` consumer REJECTs on).
- **Predicted close:** A (LOW) + B2 (LOW) close with high confidence (no kernel to preserve).
  B1/B3 (MED) carry the zero-cost-projection / zero-cost-trait risk (B1's apache_builds +1.4%
  the thinnest tripwire). B4 (MED-HIGH) is the litmus + the only real >SOTA-regression surface
  (G5 JSON scanner). If B4's Sheets litmus fails, SK-V18 does NOT paper-close — it surfaces
  "generator is still JSON+CSS-overfit," iterates B1/B2, and B4 re-enters. Generalization
  real at close: one generator emits JSON + CSS + Sheets from `.bbnf`; shared trait both
  instantiate; phantom `<G>` resolved; JSON >sonic-rs AND CSS >lightningcss PRESERVED (cold,
  real-corpus, honestly framed) from grammar-DERIVED parsers; x86 gone; Lock-14 gate
  meaningful; regen --check clean. PASS-IMPL V4 accepts every axis or records intrinsic-block
  proof.

## §8 — Ground truth (carried, re-verified live)

JSON > sonic-rs strict VALID (twitter +69.9%, canada +28.8% / simdjson DOM +45.4%, apache
+1.4% thinnest, unicode_escapes +164.7% widest). CSS canonical > lightningcss 1.9–3.3×
cold real-corpus (N=200 medians bootstrap 2.210× / animate 2.355× / tailwind 3.348× /
material 1.996×; lazy-vs-eager caveat per V3 C2 / H1). Substrate-union Lock 1 holds (the
genuine foundation — one `Tape`/`ValueRef`/`PayloadArena`). PRE-BLOCK REDRESS not re-opened:
AZ-IV eager, StructRegistry per-leaf, fact-stream-output, 24-broadcast, FNV-runtime,
x86/AVX/SVE. NEW CHALLENGE addenda all fire honestly against live surfaces:
verbatim-blob (`runtime_generator.rs:701`), distinct-grammar-output (7× md5
`b654562ccff46ed62dd48e9ace325830`), single-emitter-path (`grammar_provider.rs:40`),
phantom-generic (`tape/mod.rs:175`, sole `G` witness proof-feature-gated), timed-plane +
corpus-in-timer, acceleration-wiring (`runtime/src/lib.rs:51 #[cfg(test)]`, 2-of-3 CSS NEON
dead). checkasm = 14 (12 single-kernel + `checkasm_common.rs` + `checkasm_parity.rs`), NOT 18.

---

**TALLY (V5 wave): accept=95 revise=2 reject=0 — 97.9% · Converged=false (2 orphan REVISE
at the V≤5 ceiling: CH2 §8.1 `profile`-omission, CH5 F.6 `tests/`+`scalar/` reach;
both single-edit, mechanism-correct, αC carries the CH5 fix verbatim).**
