# S-P0 audit-overfit hardening V2 — CH1 CORRECTNESS (post-fold confirm)

Lens (CH1 CORRECTNESS, per `PASS-0-OVERFIT-AUDIT.md` §Procedure + ORCHESTRATOR §3W): every
path:line / SHA / count / md5 in `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md` resolves on disk at the
LIVE audit HEAD; no fabricated witness, no stale count, no mis-attributed gate column. V2 is the
POST-FOLD confirm: (1) the single V1 CH1 REVISE (R1-CH1, the a1 CSS-courier LOC-range annotation)
must be DISCHARGED on disk; (2) every ground-truth witness must STILL resolve after the V1 fold
edits across all seven lenses. This is an INDEPENDENT re-grep — every claim below was re-verified
this pass via absolute-path `grep`/`md5`/`find`/`sed`, not inherited from V1 or the prior artefact.

HEAD re-anchored: `git rev-parse HEAD` = `83b66db4232374db6a5f9fa009882f41acc04342` — matches the
audit's live HEAD; the bracket-snapshot `318d9c046` is correctly distinguished (one commit prior,
the SYNTHESIS/HANDOFF contract snapshot) and every dispositive surface re-grepped at the LIVE HEAD,
not assumed from the snapshot. The audit's posture (re-grep live, do not trust the snapshot) is the
correct CH1 stance and is itself ACCEPT.

## V1 REVISE discharge (R1-CH1) — the post-fold obligation

DISCHARGED on disk. The V1 CH1 REVISE flagged that a1 §L1 carried the V3-seed "~646–910-LOC" range
in the same lens entry as the disk-measured 910, with the 646 lower bound neither re-derived nor
reconciled. Re-verified this pass:

- The courier span is disk-exact: `const CSS_GENERATED_RS: &str = r#"` at `runtime_generator.rs:701`
  (re-grepped); the closing `"#;` at `:1611` (re-read line 1611 verbatim). Span = `1611 − 701` =
  **910 lines exactly**.
- The fold is present at `a1-six-addenda-lens-registry.md:66-71`: "the raw-string body runs
  `runtime_generator.rs:701`→`:1611` = **910 LOC** … The V3-seed '646–910' range is the seed's
  PRE-MEASUREMENT estimate; it is SUPERSEDED by the disk-measured 910-line body span. The figure is
  DESCRIPTIVE only — no gate keys on the LOC; the binding gate is `verbatim_blob_present == false` +
  the `.bbnf`-mutation test." The 646 lower bound is now explicitly annotated as a superseded
  pre-measurement estimate (`a1:63` retains the seed quote, `:68-69` reconciles it), so a downstream
  reader cannot treat the wider range as a verified bound.
- The fold propagated consistently: a0 §1-L1 (`a0:97-99`) and SYNTHESIS-AUDIT-OVERFIT §2.1
  (`:128-130` "≈910 LOC (cohort-carried, not gate-keyed)") carry the same disk-measured figure and
  the same non-gate framing. a2 §1/P3 (`a2:147-148`) cites "6 × 910" using the disk-measured 910.

Non-gate, single-edit, prose-precision — the binding gate keys on `verbatim_blob_present` + the
mutation test, not the LOC. **DISCHARGED.**

## Dispositions (every witness re-verified LIVE at HEAD `83b66db42`)

### §2 ground-truth table (a0 §2) — ACCEPT (re-verified, all rows disk-true)
Independently re-grepped every dispositive row:
- `CSS_GENERATED_RS: &str = r#"` const courier at `runtime_generator.rs:701`, closing `:1611` ✓.
- `RuntimeEmitterKind { CompiledLowering, RequestFacts }` at `grammar_provider.rs:40-42`, field
  `emitter` at `:33`, dispatched at `:110` ✓.
- 7× `css_l4_*/generated.rs` share md5 `b654562ccff46ed62dd48e9ace325830` (`md5 -q … | sort | uniq
  -c` = `7  b654562c…`; 7 dirs counted) ✓.
- `ValueRef<'doc,'input, K = AnyKind, G: EventGrammar = AnyGrammar>` at `tape/mod.rs:175` ✓.
- x86 surface 1: `src/x86_64/` = **24** files (`find -type f | wc -l`) ✓; surface 2: `ext/x86/` =
  **4** files (`LICENSE-VENDOR`, `bbnf.asm`, `x86inc.asm`, `x86util.asm`) ✓; `nasm-rs = "0.3"` at
  `Cargo.toml:19` ✓.
- Metalang leak `parse_w11_1_number` ×**7** in the shipped `json/generated.rs` (`grep -c` = 7) ✓.
- `RuntimeTarget` = **12 fields** (`regen.rs:6-18`: grammar_name…output_labels) ✓; derives only
  `Clone, Copy, Debug` at `regen.rs:5` (NOT `PartialEq`) ✓.
- `fact_schema`/`row_id`/`output_plane` are NOT top-level `RuntimeTarget` fields — they are the
  fields of nested `RuntimeOutputLabels` (struct at `grammar_provider.rs:92`, fields `:93-95`),
  reached via `output_labels: Option<RuntimeOutputLabels>` (field #12) ✓. Both nested structs
  `RuntimeFrontendRequirements` (`:46`) and `RuntimeOutputLabels` (`:92`) derive `PartialEq, Eq`
  (`:45`/`:91`) ✓ — so the one-line `RuntimeTarget: PartialEq` mechanism the fold proposes is viable.
None of these were disturbed by the fold edits. The §2.4 by-exclusion projection (exclude only
`output_dir`+`expected_files`; include BOTH nested structs) is disk-sound.

### The 6 addenda each FIRE on a real surface (a1 §L1–L6) — ACCEPT ×6
Re-verified each fires, none decorative, each catches its V3 mode:
- **L1 verbatim-blob** fires on `CSS_GENERATED_RS:701` (catches V3 D1). The L1 LOC annotation is
  descriptive prose, NOT the firing surface — the firing surface is the const-`&str` body itself,
  re-grepped this pass.
- **L2 distinct-grammar-output** fires on 7× md5 `b654562c…` (catches V3 D1); the
  branch-count/type-count/`runtime_target_rows_collapsed` co-gate correctly closes the
  relocated-seam hole the md5 cannot.
- **L3 single-emitter-path** fires on `RuntimeEmitterKind:40-42`+`:110` (catches V3 D1 neutral-named
  fork) — distinct from L2 because the arm-census does NOT fire on the neutral enum names.
- **L4 phantom-generic** fires on `ValueRef<…G: EventGrammar = AnyGrammar>:175` (catches V3 D2); the
  `G` axis is phantom (test-only animation), the `K=AnyKind` axis correctly excluded as the real one.
- **L5 timed-plane-symmetry + corpus-in-timer** fires on `measure_mbps:3091` / `lightningcss_facts`
  warm micro-fixture path (catches V3 C3+C2); `css_canon_bench` PRESENT (honest harness KEPT).
- **L6 acceleration-wiring** fires on `runtime_simd.rs:29/:112/:169`; `find_css_significant` caller
  ONLY at `lib.rs:574`, which is AFTER the sole `#[cfg(test)]` at `:51` → test-only; ZERO hits in
  `grammars/` (the hot path) — confirming the cfg(test)-only deadness (catches V3 C1).

### R1-CH2 fold (a1 §L1 (b) per-primitive mutate-falsifier) — ACCEPT (disk-true)
a1 §L1 REVISE-criterion (`a1:120-130`) now states the (b) predicate as a MACHINE per-primitive
mutate-falsifier — "the primitive's EMITTED OUTPUT VARIES correspondingly under a `.bbnf` mutation
of the invoking rule's shape … a fixed body keyed off a merely-decorative grammar-derived argument
FAILS (b)" — not "accepts a grammar-derived argument." All three predicates (grep · mutate+regen-diff
· telemetry) are machine; none prose-reviewed-at-admission. Correctly grounded; closes the
one-level-down prose-review seam. The escape's (a)-(c) gate exists in the binding contract at
`SYNTHESIS.md:342` (re-read verbatim this pass — the "single largest paper-close surface" admission
and the REJECT-on-(a)-(c)-failure clause are present).

### R1-CH5 fold (R16 recipe-pin, BOTH nested structs) — ACCEPT, correctness-positive (re-verified broader)
The fold broadens the `runtime_target_rows_collapsed` recipe to inline EVERY nested-struct field —
`frontend_requirements` (#11) AND `output_labels` (#12), not only `output_labels`. Re-verified the
disk basis: `frontend_requirements: codegen::RuntimeFrontendRequirements` (`regen.rs:17`, struct
`grammar_provider.rs:46`, `PartialEq, Eq`); `output_labels` (`regen.rs:18`, struct `:92`,
`PartialEq, Eq`); `RuntimeTarget` derives only `Clone, Copy, Debug` (`regen.rs:5`) — so the
one-line `PartialEq`-derive cost the fold states is accurate, and the mechanism covers both nests
automatically. SHARPENS the gate recipe; does not contradict. a3 §3 (F-A3.5), a2 §4a, and SYNTHESIS
§5 fact-5 all carry the broadened altitude consistently.

### R1-CH6 / R2-CH6 SYNTHESIS rows — ACCEPT (mirror the a0 source content)
- R-A0-1 row (`SYNTHESIS-AUDIT-OVERFIT.md:99`) carries the explicit REJECT clause ("an unqualified
  'beats CSSOM'/'equal-work' close-report claim behind a re-label is a REJECT, per a0 §4"). Mirrors
  a0 §4 binding item 1 (`a0:323-328`). The underlying H1 OR survives on disk at `SYNTHESIS.md:338`
  ("re-frame … OR a symmetric materialization-depth comparator"); the C2 disclosure
  ("lazy rich-summary beats eager full-CSSOM") at `SYNTHESIS.md:284-291` — both re-read verbatim.
- R-A0-2 row (`:100`) carries the disk-grounded collapse-to-one answer (`generator_grammar_count ==
  3` = json+css+sheets; "manufacturing 7 fake `.bbnf` roots … is the EXACT overfit the addendum
  forbids"). Mirrors a0 §5 (`a0:381-387`). The alphaE defer it sharpens is exact: `alphaE:109`
  carries "P3 must decide collapse-vs-differentiate (defer the *which* to B2, but the *replica
  deletion* lands here)" verbatim (re-grepped — exact line + exact quote).

### PRUNE-sequencing (a2) — ACCEPT (the load-bearing CH1 build-coupling re-verified exact)
- P1 ↔ `checkasm_parity.rs`: `grep -cE 'bbnf_simd::x86_64::.*_scalar\('` = **9**, at exactly
  `:458,:464,:467,:477,:478,:484,:493,:497,:502` (re-grepped — every cited line carries the named
  scalar call). The build-blocking decouple-same-wave constraint is real and disk-exact.
- P4 line anchors disk-true: `GENERIC_SCAN_ROOTS:2409`, `FORBIDDEN_GENERIC_TOKENS:2420`,
  `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442`, `("crates/bbnf-simd/src/x86_64","diagnostic-x86"):2463`.
- R1-CH3 directional fold ("G1/G3 co-derive; G3-failure blocks PROVE", never a backward "gates
  G1/G2") + the dual entry-gate (G2 entry-gates on BOTH G1 AND P3) are stated consistently across
  a2 §0/§4/§7/§8 and SYNTHESIS §5 fact-3; the dependency chain
  PRUNE→G1→G2→G3→G4→G5/G6→PROVE→H1 is internally consistent and disk-anchored.

### SYNTHESIS-AUDIT-OVERFIT telemetry-column attribution — ACCEPT
Re-grepped every cited column at its named SYNTHESIS.md line: `verbatim_blob_present:563`,
`emitter_fork_present:564`, `generator_grammar_branch_count:565`, `runtime_target_rows_collapsed:566`,
`generator_grammar_type_count:567`, `phantom_generic_resolved:568`, `json_rich_navigation_preserved:570`,
`generated_md5_distinct:572`, `acceleration_at_admission:575`, `x86_tree_deleted:576`,
`materialization_framing:579`, `corpus_in_timer:580` — ALL resolve to the named column. The
"enumerate-by-exclusion over the live `regen.rs:6` 12-field struct" framing is present at the
operative gate (`:566`, `:410`) — confirming the 12-vs-13 slip is folded out at the binding gate
(the gate is field-named, not count-driven).

### Audit completeness (V3 D1–D4 / C1–C3 → addenda + residuals) — ACCEPT
Re-verified the V3 seed: the 6 addenda proposed at `CONSOLIDATED-AUDIT.md:89-94` map verbatim to
L1–L6; D1→L1/L2/L3 (R1–R4), D2→L4 (R5/R6), D3→P1 (R8), D4→P4 (R9), C1→L6 (R7/R10/R11), C2→L5
(R14/R-A0-1), C3→L5/P2 (R13). No V3 dispositive finding dropped. The one NEW finding (R16) is
genuinely beyond the V3 set and is disk-true (nested-struct recipe hazard, `regen.rs:17-18` +
`grammar_provider.rs:92-95`).

## REVISE (0) / REJECT (0)

The single V1 CH1 REVISE (R1-CH1) is DISCHARGED on disk; every fold is prose-precision over an
already-disk-true witness; no new correctness defect introduced by any of the seven folds. Every
dispositive path:line / SHA / count / md5 / gate-column resolves at HEAD `83b66db42`. Citation nit
noted but non-defective: a0 §5 labels the alphaE quote "alphaE §A" while the exact line is 109 (in
the P-cluster Risk line) — the path:line `alphaE:109` and the quote are both verbatim-exact, so this
is a loose section label, not a fabricated or stale witness; it does not rise to REVISE.

## Verdict

The residual-overfit audit is COMPLETE (V3 D1–D4 / C1–C3 → L1–L6 + R1–R16 + R-A0-1/2/3, no drop).
The 6 addenda are EXECUTABLE (every check is a grep/diff/md5/samply over the live tree) and each
CORRECTLY catches its V3 failure mode, firing on a real disk surface verified this pass — none
decorative. The PRUNE-sequencing is SOUND and disk-grounded (P1↔checkasm 9-site build-coupling
re-verified exact; P4 anchors exact; the chain internally consistent). The single V1 CH1 REVISE is
discharged; the fold edits preserved every witness.

## Tally
ACCEPT 10 · REVISE 0 · REJECT 0 — **100%**. Sections: (1) R1-CH1 discharge; (2) §2 ground-truth
table; (3) 6 addenda fire+catch; (4) R1-CH2 (b) mutate-falsifier; (5) R1-CH5 R16 both-nested-struct
recipe; (6) R1-CH6/R2-CH6 SYNTHESIS rows; (7) a2 PRUNE-sequencing + P1↔checkasm build-coupling;
(8) SYNTHESIS telemetry-column attribution; (9) audit completeness V3→addenda; (10) HEAD-anchoring
honesty. All ACCEPT; zero orphan REVISE; every dispositive witness disk-true at HEAD `83b66db42`.

TALLY accept=10 revise=0 reject=0
