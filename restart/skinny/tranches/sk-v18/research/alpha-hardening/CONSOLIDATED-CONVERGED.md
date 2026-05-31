# SK-V18 Pass-Alpha — alpha-hardening CONSOLIDATED-CONVERGED (§3Z verdict, post-fold + confirm)

Pass Alpha SK-V17→SK-V18 — the GENERALIZATION cycle (the inflection backtrack). NOT a
new-feature cycle: JSON + CSS are both >SOTA with a working value API at SK-V17 (`f6a38445b`).
Per the binding principle, SK-V18 BACKTRACKS the hand-written/forked parsers into ONE
grammar-driven generator emitting all grammars from `.bbnf`, over the unified `Tape`/`ValueRef`
substrate (substrate-union Lock 1), a shared value API, PROVEN on a 3rd grammar (Sheets),
PRESERVING >SOTA. Seven-lens CHALLENGE (CH1 Correctness · CH2 Generality · CH3 Regression ·
CH4 Cost · CH5 Hidden-Coupling · CH6 Next-Tranche-Impact · CH7 Overfit-Prune) over the αF
contract (`SYNTHESIS.md` + `HANDOFF.md`) + the αA–αE feeders. Per `PASS-ALPHA.md` §2 (alpha-A..F),
§3 (CHALLENGE), §4 (goalset), §3Z + `ORCHESTRATOR.md` §3W/§3Z. Bracket HEAD `318d9c046`;
SK-V17 closed `f6a38445b`; V3 audit seed `7dbe44c22`.

This CONSOLIDATED-CONVERGED supersedes `CONSOLIDATED.md` (the V5 §3Z verdict): it records the
fold of the two V5 orphan REVISEs (F16 field-enumeration · F.6 x86-deletion-list widening) and
the CONFIRMING CHALLENGE that followed.

## §0 — The first Pass-Alpha attempt: TOTAL INFRA VOID (no-data, discounted)

The FIRST Pass-Alpha dispatch was a **total-infrastructure failure**: every fanned-out agent
returned **0 tokens** — a void, no artefacts, no dispositions, no signal. It is NOT a cycle, NOT
a sub-95% reading, NOT evidence of any kind; it is discounted entirely and excluded from the
convergence trajectory. The substantive Pass-Alpha is the V1→V5 ceiling iteration plus the
post-fold CONFIRM below. The void is recorded here only so the cycle history is honest about
why the first run produced nothing.

## §1 — Cycle history (the full trajectory, infra-void → V5 → confirm)

| Cycle | ACCEPT rate | Posture |
|---|---|---|
| **infra-void** | — | First Pass-Alpha attempt; ALL agents 0 tokens; total infra failure; no data; DISCOUNTED |
| V1 | **74.2%** | First clean pass; 24 tightening REVISEs / 0 REJECT; folded to V2 |
| V2 | **96.7%** | First ≥95%; goalset Lock-14 / Sheets / `ValueRef` / revert folds landed |
| V3 | **94.8%** | Sub-95% straggler wave (F13 relocated-seam attribution; checkasm "18"; FOLD-1 second-x86-surface authored in αC/SYNTHESIS/HANDOFF) |
| V4 | **92.7%** | Sub-95%; 5 REVISE clusters — FOLD-1 orphan into αA/αE feeders; P1 deletion-list reach (RED-by-construction); `runtime_target_rows_collapsed` projection tuple; ledger-anchor drift |
| V5 | **97.9%** | Ceiling iteration (V≤5). 95 ACCEPT / 2 REVISE / 0 REJECT across 97 dispositions; the 2 REVISEs are the §3Z-blocking orphans (F16 `profile`-omission · F.6 `tests/`+`scalar/` reach) |
| **confirm** | **88.9%** | Post-2-fold CONFIRMING CHALLENGE; 80 ACCEPT / 10 REVISE / 0 REJECT across 90 dispositions, 7/7 lenses; the 2 NAMED orphans (F16 + x86-list) DISCHARGED; 10 NEW one-level-deeper sharpenings surfaced |

Zero REJECT every cycle (void excepted, which produced nothing). The trajectory is
substance-converged — V1 74.2 → V2 96.7 → V3 94.8 → V4 92.7 → V5 97.9 — but the §3Z conjunction
(**≥95% × 2 consecutive AND zero orphan REVISE AND V≤5**) is unmet: V5 97.9% + the confirm 88.9%
is NOT a 2-consecutive ≥95% pair.

## §2 — The confirm wave: per-lens tally (re-verified live at HEAD `318d9c046`)

| Lens | ACCEPT | REVISE | REJECT | Disposition summary |
|---|---|---|---|---|
| CH1 Correctness | 5 | 2 | 0 | F16 fold DISCHARGED; 2 one-deeper accuracy nits (incl. "13-field"/12-field struct-count slip restated across binding surfaces) |
| CH2 Generality | 32 | 0 | 0 | F16 fold DISCHARGED orphan-free at all 4 enumeration sites (by-exclusion, `profile`/`source_inputs`/`metadata_inputs` named, P3-preserves-profile-distinctness captured); Lock-14 spine sound; Sheets proof load-bearing; clean ACCEPT ×32 |
| CH3 Regression | 5 | 2 | 0 | No pre-block re-opened; F16 discharged; F.6 fold reach-complete in feeders but the binding contract row (`SYNTHESIS.md:326`/`:576`) not yet carrying (h)+(i) — propagation-residual REVISE |
| CH4 Cost | 1 | 2 | 0 | F16 cost-free; LOC envelope unchanged (P1 ≈ −4500); F.6 binding-row propagation + struct-count residuals |
| CH5 Hidden-Coupling | 24 | 1 | 0 | 4 core axes structurally honest + disk-true; F16 discharged in binding rows; F.6 landed in feeders (αC/αE) but not the 3 binding sites it was filed against — propagation-residual REVISE |
| CH6 Next-Tranche-Impact | 7 | 2 | 0 | Wave sequencing + pre-blocks sound; F.6 binding-row reach + struct-count residuals; no paper-close surface |
| CH7 Overfit-Prune | 6 | 1 | 0 | FOLD A (F16) DISCHARGED every binding surface; FOLD B (x86-list) discharged in feeders, residual binding-row propagation flagged |
| **Total** | **80** | **10** | **0** | **88.9% (80/90), 7/7 lenses, zero REJECT** |

The confirm is **88.9%** — below the §3Z ≥95% bar. **Zero REJECTs.** The 10 REVISEs are NOT
finding reversals and NOT re-opened architecture: they are one-level-deeper sharpenings in the
same necessary-not-sufficient lineage the prior cycles tracked. The two NAMED orphans the fold
targeted are discharged (CH2/CH7 confirm F16 across every binding surface; the x86-list fold is
reach-complete in the αC/αE feeders). The new REVISE mass is the **propagation residual** — the
F.6 x86-list reach landed in the feeders but several lenses (CH3/CH5/CH6) find it not yet carried
verbatim into the binding `SYNTHESIS.md:326`/`:576` + `HANDOFF.md:101-112` rows — plus the
"13-field"/12-field struct-count slip restated across binding surfaces (CH1/CH4/CH5/CH6 each
flag it as a sub-REVISE accuracy nit, not a blocking defect).

## §3Z — VERDICT

**Converged = false.** The fold + confirm DISCHARGE the two V5 orphan REVISEs, but the §3Z
conjunction is still unmet on the consecutive-pair count.

1. **The two V5 orphan REVISEs are DISCHARGED.**
   - **REVISE-1 (CH2 §8.1 / F16 `profile`-omission)** — DISCHARGED, orphan-free, at every
     operative-enumeration site (`SYNTHESIS.md:152/:156/:165/:333/:411/:566`,
     `HANDOFF.md:274/:332`, αE `:19/:105/:156/:207`, αC `:96-102/:455-474`). The gate is now
     stated **by EXCLUSION** of the two generated-artefact path columns (`output_dir`,
     `expected_files`); the operative non-path set explicitly names `profile`, `source_inputs`,
     `metadata_inputs` (10 of the 12 live `RuntimeTarget` fields = prose-minus-path); and the fold
     additionally captures the **P3-must-preserve-profile-distinctness** consequence the finding
     required. Verified live: `regen.rs:6-18` = 12 fields; `profile` 7 distinct, `fact_schema` 7
     distinct → the corrected gate is correctly RED pre-P3, GREEN only after a genuine collapse.
     CH2 100% (32/0/0), CH7 confirms DISCHARGED every binding surface.
   - **REVISE-2 (CH5 F.6 / P1 x86-deletion-list reach)** — DISCHARGED. The two
     crate-wide-grep-firing escapees the V5 finding named — `tests/checkasm_parity.rs` (11
     `x86_64` tokens, 9 ACTIVE compile-coupled `bbnf_simd::x86_64::…::*_scalar(…)` call sites at
     `:458,:464,:467,:477,:478,:484,:493,:497,:502`, plus the `:672` `#[ignore]` x86 harness) and
     `src/scalar/byte_class_from_eq_set_64.rs:10-15` (doc x86 cross-refs) — are on the P1
     deletion/decouple list in the αC (`:168-215`) and αE (`:94/:101/:104`) feeders with the
     **build-soundness rationale explicit** (decoupling `checkasm_parity.rs` is what keeps the
     `src/x86_64/` deletion from breaking `cargo test --no-run`). P1 is now reach-matched and
     build-sound. (A propagation-residual — carrying the verbatim feeder text into the binding
     `SYNTHESIS.md:326`/`:576` + `HANDOFF.md:101-112` rows — is the redress carry for the
     plan/redress phase; it does not re-open the goalset and is a single-edit mechanical fold.)

2. **Two-consecutive ≥95% NOT yet recorded.** V5 (97.9%) is the only ≥95% reading since V2; the
   CONFIRMING wave landed at **88.9%**, so the consecutive-pair condition is unmet. The confirm
   surfaced 10 new one-deeper REVISEs (propagation-residual + struct-count nit), each single-edit,
   mechanism-correct, non-architectural.

**The verdict the orchestrator carries to G-Alpha:** substance-converged across V1→V5 (74.2 →
96.7 → 94.8 → 92.7 → 97.9, zero REJECT all cycles); the two V5 orphan REVISEs (F16 + x86-list)
DISCHARGED; but formally short of §3Z — the post-fold CONFIRM landed 88.9% (10 new
single-edit sharpenings, zero REJECT, zero re-opened architecture), so a 2nd consecutive ≥95% is
NOT yet on the board. No REJECT, no orphan architectural defect, no stranded >SOTA, no re-opened
pre-block. The residuals are mechanical (binding-row propagation of the already-correct feeder
text + a struct-count label slip), foldable by the orchestrator and re-confirmable, OR escalated
to the user per the V>V5 path. The contract spine, goalset, and all six addenda are structurally
sound and disk-true.

## §4 — LOCKED SK-V18 generalization goalset (the inflection backtrack)

The CHALLENGE-survived, R10-binding goalset — ONE grammar-driven generator, NOT a feature set.
Carried verbatim from V5; the fold did not re-litigate it.

1. **ONE grammar-driven generator emitting all grammars from `.bbnf`.** Retire the
   `RuntimeEmitterKind` JSON-vs-CSS fork (`grammar_provider.rs:40`); one grammar-agnostic emitter
   over the `SinkOnlyProgram`/`BackendShape` 5-shape lowering. JSON projects from grammar (the
   hand-written template becomes a byte-for-byte parity oracle, NOT the product); CSS lowers from
   grammar (retire the `CSS_GENERATED_RS` const-`&str` blob, `runtime_generator.rs:701`). Lock-14:
   zero grammar-named branches in generic crates; the gate (P4) actually scans `codegen/src` AND
   `xtask/src` (arm census + type census + the structural `runtime_target_rows_collapsed` collapse,
   now F16-corrected by-exclusion).
2. **Shared value API.** One `Value`/`Document`/`Cursor` trait both JSON and CSS instantiate (≥2
   NON-test production impls); lazy over the EXISTING `Tape`/`ValueRef` (no second substrate,
   Lock 1); rich-AST preserved (`json_rich_navigation_preserved`, no LCD-flatten). The phantom
   `<G>` (`tape/mod.rs:175 G: EventGrammar = AnyGrammar`, the PHANTOM axis, NOT the real `K=Kind`)
   is INSTANTIATE-or-DELETE — DELETE is the abrogate-before-patch DEFAULT (`CssEventGrammar` does
   not exist at HEAD).
3. **Sheets proof (the honest generalization litmus).** The real 185-LOC Pratt
   `grammar/google-sheets/google-sheets.bbnf` (`error_literal`/`cell_ref`/precedence-tower shapes
   NO JSON/CSS rule exercises) ADOPTED into the benched tree and emitted via the generator ONLY —
   md5(Sheets) ≠ JSON ≠ CSS, instantiates the G4 trait, ZERO hand-authored runtime Rust,
   `sheets_grammar_shape == pratt-operator` (a flat-stream/tree REJECTed as third-JSON hollowing).
   The 25-LOC `sheets_witness/` stub retired.
4. **Preserved >SOTA from the grammar-DERIVED parsers.** JSON > sonic-rs strict (the
   `IgnoredAny`+`.end()` strict skipper; range +1.4%…+164.7%, apache_builds thinnest at +1.4% the
   load-bearing tripwire, unicode_escapes widest). CSS > lightningcss (N=200 `css_canon_bench`
   per-row floors, H1-framed lazy-rich-summary vs eager-full-CSSOM). A derived parser that loses
   the speed or the equality is not done; the honest-finding escape is GATED ((a) `.bbnf`-invoked
   by name, (b) grammar-derived DATA, (c) `verbatim_blob_present == false`) — not a paper-close
   hatch.
5. **x86 DELETED (aarch64-only), build-sound.** NO x86 surface anywhere in `bbnf-simd` —
   crate-wide, reach-matched and build-sound: `src/x86_64/` (847) AND `ext/x86/` (3554) AND
   `build.rs` (102) AND the `nasm-rs` Cargo.toml dep AND `lib.rs:5`/`:247`/`:285-288` AND in-crate
   doc surfaces — AND the now-folded compile-coupled sites: **(h)** `tests/checkasm_parity.rs`
   (9 active `bbnf_simd::x86_64::…::*_scalar(…)` call sites + the `#[ignore]` x86 harness,
   decouple-or-delete so deleting `src/x86_64/` does NOT break the build) and **(i)**
   `src/scalar/byte_class_from_eq_set_64.rs:10-15` (doc x86 cross-refs scrubbed aarch64-neutral).
   `x86_tree_deleted` is the R10 binding pin; with (h)+(i) the verify grep is reach-matched
   (5 firing files, 5 named removal targets) and satisfiable-by-construction.

**R10 success criterion:** one generator emits JSON + CSS + Sheets from `.bbnf`; shared value-API
trait both instantiate; phantom `<G>` instantiated-or-deleted; >SOTA preserved honestly; x86 gone
build-sound; Lock-14 gate meaningful; regen --check clean; PASS-IMPL V4 accepts every axis or
records intrinsic-block proof.

## §5 — Candidate-wave shortlist (survived CHALLENGE, additive-by-deletion)

PRUNE → GENERALIZE → PROVE → HONESTY. No candidate added or removed across V1→V5 or the confirm;
the shortlist is additive-by-deletion (removes overfit, never resurrects rejected architecture);
no re-opened REDRESS pre-block.

| Cluster | Members | Risk | LOC Δ | Entry-gate | Falsifier |
|---|---|---|---|---|---|
| **PRUNE** | P1–P5 | LOW | ≈ −10800 (P1 ≈ −4500 BOTH x86 surfaces) | first | x86=0 crate-wide reach-matched + build-sound (incl. (h)+(i)); 7 replicas collapsed (per-`grammar_name` config-tuple collapse over all non-path columns, profile-distinctness preserved); Lock-14 gate meaningful; metalang purged |
| **GENERALIZE G1** | un-fork emitter + project JSON (G3+G1) | MED | ≈ −800 | PRUNE | single-emitter-path + neutral body (arm/type census); grammar-derivation-proof (`.bbnf` mutation test — a const courier cannot pass); JSON >sonic preserved (apache +1.4% tripwire) |
| **GENERALIZE G2** | derive CSS from lowering | LOW | ≈ −1500 | G1, P3 | verbatim-blob retired; CSS >lightningcss preserved (N=200 per-row floors, H1-framed); distinct + neutral + config-tuple collapse |
| **GENERALIZE G3** | shared value trait + kill phantom `<G>` (G4+H1) | MED | ≈ ±0 | G1, G2 | phantom-generic resolved (DELETE default, test-excluded grep); shared trait both-impl + rich-ast reachable THROUGH it; zero-cost (no vtable in hot path) |
| **PROVE / HONESTY** | Sheets + scanner/NEON honesty (PROVE+G5+G6+H1) | MED-HIGH | ≈ +250 (capped) | G1, G2, G3 | 3 md5-distinct generated.rs; Sheets 0-hand-LOC from google-sheets.bbnf; accel-wired-at-admission (NOT `#[cfg(test)]`); JSON preserved; `regen --check` clean |

**Net LOC ≈ −12650…−12850** — a generalization cycle that deletes far more than it adds (the
inflection backtrack collapses two forked hand-written parsers + replicas + BOTH x86 surfaces
into one generator).

### PRUNE P1–P5 (additive-by-deletion)
- **P1** — delete the WHOLE x86 surface crate-wide (F15: `ext/x86/` 3554 + `src/x86_64/` 847 +
  `build.rs` 102) AND the compile-coupled (h) `tests/checkasm_parity.rs` + (i)
  `src/scalar/byte_class_from_eq_set_64.rs` so the deletion is build-sound and the verify grep is
  reach-matched.
- **P2** — delete the old warm contrived CSS bench (`nonjson_css_l4.rs` `measure_mbps`, 187-byte
  SHA fixtures) while KEEPING the honest `css_canon_bench`/`w2_rich_cssom_bench` + 9-field
  `assert_rich_strict_equality` oracle.
- **P3** — collapse the 7 byte-identical css_l4 `generated.rs` replicas AND the 7 xtask
  `RuntimeTarget` rows to one config, PRESERVING profile-distinctness (collapse only where the 7
  are genuinely one grammar; never erase the `profile` discriminator).
- **P4** — make the Lock-14 gate meaningful; land it BEFORE the B1/G2/G3 emitter rebuild.
- **P5** — purge the `parse_w11_1_number` metalang leak (×7 live).

### GENERALIZE G1–G6
Un-fork + project JSON (the SK-V17 REDRESS-W2-1 single-emitter SUBJECT admitted to discharge, NOT
a re-open); derive CSS from lowering (LOW risk — scalar hot path, no fragile kernel to preserve);
shared value trait + instantiate-or-delete the phantom `<G>` on the RIGHT (`G`) axis; migrate
JSON's bespoke scanner (`json/scan.rs`, the speed holdout) onto the neutral NEON kernel (G5);
wire-or-retire the CSS NEON honestly at admission + ASM backlog aarch64-only (G6).

### PROVE-Sheets
Sheets through the generator ONLY — the load-bearing non-hollow litmus: a real 185-LOC Pratt
grammar, gated `sheets_grammar_shape == pratt-operator`, config-tuple distinct from css_l4 + json.
If the Sheets litmus fails, SK-V18 does NOT paper-close — it surfaces "generator is still
JSON+CSS-overfit," iterates B1/B2, and B4 re-enters.

### HONESTY H1
Wire-or-retire the CSS NEON at admission (retire gated on a samply non-top-N MEASUREMENT, not an
assertion); ASM backlog aarch64-only (PMULL/UDOT/TBX/CSSC + 5 scalar-passthrough kernels, each
WITH its hot-path consumer or retired); reframe CSS >SOTA lazy-rich-summary vs eager-full-CSSOM
(`materialization_framing`, `corpus_in_timer`); `regen --check` clean.

## §6 — G-Alpha presentation summary

**Subject:** SK-V18 = the GENERALIZATION cycle (the inflection backtrack). NOT a new-feature
cycle — JSON + CSS are both >SOTA with a working value API (SK-V17, `f6a38445b`); SK-V18
backtracks the hand-written/forked parsers into ONE grammar-driven generator over the unified
tape/`ValueRef` substrate, shared value API, PROVEN on Sheets, PRESERVING >SOTA.

- **Waves (PRUNE → GENERALIZE → PROVE → HONESTY, dependency-ordered):** PRUNE P1–P5 (P4 lands
  BEFORE the emitter rebuild) → GENERALIZE G1 (JSON projection) → G2 (CSS lowering) → G3 (un-fork
  emitter) → G4 (shared trait + phantom) → G5 (JSON scanner onto neutral NEON) → G6 (CSS NEON
  wire-or-retire + ASM backlog) → PROVE (Sheets via generator only) → HONESTY (H1 framing +
  regen --check). Each primitive lands WITH its hot-path consumer in the same commit (no orphan
  kernels). The revert dependency graph PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1 means a
  failed exit gate BLOCKS every downstream wave (G1 failure blocks G2/G3/G4/PROVE; G3 un-fork
  failure blocks PROVE).
- **LOC:** net ≈ **−12650…−12850** (PRUNE ≈ −10600…−10800 with P1 ≈ −4500 deleting BOTH x86
  surfaces; G1 −800; G2 −1500; G3 ±0; PROVE/HONESTY +250 capped). Deletes far more than it adds.
- **Hard caps:** standing `[dispatch-hard-cap]` defaults — research/plan/redress **20/15/30 min**,
  "at 0.9N commit, at N halt"; the Sheets/NEON cluster (PROVE/HONESTY) is MED-HIGH and may carry a
  documented larger cap. Revert protocol + caps + per-wave triumvirate sanctioned-deferred to S-P3
  (PASS-ALPHA §4.4) with these binding carries.
- **Pre-blocked routes (no re-open):** AZ-IV eager value-tree, StructRegistry per-leaf
  indirection, fact-stream-as-output, 24-row broadcast, FNV-in-runtime, x86/AVX/SVE/nasm (now
  crate-wide + compile-coupled (h)+(i)). Plus the six addenda re-entries: verbatim-blob,
  distinct-grammar-output, single-emitter-path fork, phantom-generic, timed-plane-symmetry +
  corpus-in-timer, acceleration-wiring — each bound THREE ways (close gate + §0.4 pre-block +
  machine-checkable telemetry column the `gate-json` consumer REJECTs on).
- **Predicted close:** PRUNE (LOW) + G2 (LOW) close with high confidence (no kernel to preserve).
  G1/G3 (MED) carry the zero-cost-projection / zero-cost-trait risk (G1's apache_builds +1.4% the
  thinnest tripwire). PROVE/HONESTY (MED-HIGH) is the litmus + the only real >SOTA-regression
  surface (G5 JSON scanner). Generalization real at close: one generator emits JSON + CSS + Sheets
  from `.bbnf`; shared trait both instantiate; phantom `<G>` resolved; JSON >sonic-rs AND CSS
  >lightningcss PRESERVED (cold, real-corpus, honestly framed) from grammar-DERIVED parsers; x86
  gone build-sound; Lock-14 gate meaningful; regen --check clean. PASS-IMPL V4 accepts every axis
  or records intrinsic-block proof.

## §7 — Ground truth (carried, re-verified live at HEAD `318d9c046`)

JSON > sonic-rs strict VALID (twitter +69.9%, canada +28.8% / simdjson DOM +45.4%, apache +1.4%
thinnest, unicode_escapes +164.7% widest). CSS canonical > lightningcss 1.9–3.3× cold real-corpus
(N=200 medians bootstrap 2.210× / animate 2.355× / tailwind 3.348× / material 1.996×; lazy-vs-eager
caveat per V3 C2 / H1). Substrate-union Lock 1 holds (the genuine foundation — one
`Tape`/`ValueRef`/`PayloadArena`). PRE-BLOCK REDRESS not re-opened: AZ-IV eager, StructRegistry
per-leaf, fact-stream-output, 24-broadcast, FNV-runtime, x86/AVX/SVE. NEW CHALLENGE addenda all
fire honestly against live surfaces: verbatim-blob (`runtime_generator.rs:701`),
distinct-grammar-output (7× md5 `b654562ccff46ed62dd48e9ace325830`), single-emitter-path
(`grammar_provider.rs:40`), phantom-generic (`tape/mod.rs:175`, sole `G` witness
proof-feature-gated), timed-plane + corpus-in-timer, acceleration-wiring
(`runtime/src/lib.rs:51 #[cfg(test)]`, 2-of-3 CSS NEON dead). `RuntimeTarget` = **12 fields**
(`regen.rs:6-18`; the binding contract's "13-field" labels are a sub-REVISE accuracy slip, not a
gate defect — the by-exclusion mechanism names the 10 operative fields regardless of the printed
count). checkasm = 14 (12 single-kernel + `checkasm_common.rs` + `checkasm_parity.rs`), NOT 18.

---

**TALLY (confirm wave): accept=80 revise=10 reject=0 — 88.9% (80/90, 7/7 lenses) ·
Converged=false.** The two V5 orphan REVISEs (CH2 §8.1 F16 `profile`-omission · CH5 F.6
`tests/`+`scalar/` x86 reach) are DISCHARGED; but V5 97.9% + confirm 88.9% is NOT a 2-consecutive
≥95% pair, so §3Z is unmet. Substance-converged (V1 74.2 → V2 96.7 → V3 94.8 → V4 92.7 → V5 97.9,
zero REJECT all cycles; first attempt was a discounted total infra-void). The 10 confirm REVISEs
are single-edit, mechanism-correct, non-architectural (binding-row propagation of the
already-correct feeder x86-list fold + a 12/13 struct-count label slip). No REJECT, no orphan
architectural defect, no stranded >SOTA, no re-opened pre-block. **Next: G-Alpha user gate.**
