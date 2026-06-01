# S-P0 audit-overfit hardening V1 — CH1 Correctness

Lens (CH1 CORRECTNESS, per `PASS-0-OVERFIT-AUDIT.md` §Procedure + ORCHESTRATOR §3W): every
path:line / SHA / count / md5 in `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md` resolves on disk at
the LIVE audit HEAD; no fabricated witness, no stale count, no mis-attributed gate column. The
6 addenda must each FIRE on a real surface (none decorative), correctly catch their V3 failure
mode, and the PRUNE-sequencing must be disk-grounded. This is an INDEPENDENT re-grep — every
claim below was re-verified this pass, not inherited.

## Method

HEAD re-anchored: `git rev-parse HEAD` = `83b66db4232374db6a5f9fa009882f41acc04342` — matches
the audit's live HEAD exactly; the contract-snapshot `318d9c046` is correctly distinguished as
the BRACKET HEAD (one commit prior) and every dispositive surface re-grepped at the LIVE HEAD,
not assumed from the snapshot. The audit's honest move (re-grep live, do not trust the snapshot)
is the correct CH1 posture and is itself ACCEPT.

## Dispositions

### Goalset residual audit completeness (a0) — ACCEPT
- **§0 verdict + R-A0-1/2/3 (ACCEPT).** The three residual seams (framing-escape, P3-deferred-
  decision, honest-finding-primitive-escape) are each anchored to a live SYNTHESIS/HANDOFF line
  and a live disk witness; none is a REJECT-mislabelled-as-MEDIUM (I verified each fires on a
  real surface, §below). The residual census is COMPLETE against the V3-found set: I cross-checked
  the V3 D1–D4 + C1–C3 findings map 1:1 to L1–L6 + P1–P5, with no V3 dispositive finding dropped.
- **§2 ground-truth table (ACCEPT, 16/16 re-verified).** I independently re-grepped every row:
  `CSS_GENERATED_RS:701` const `&str` ✓; `json_sink_direct::render` fixed-literal bodies ✓
  (render takes `&SinkOnlyProgram` and `validate()`-gates at `:18-31`, but `render_entry`/
  `render_value_dispatch` take only `out: &mut String` — NO program arg, so the grammar gates
  but does NOT shape the body: a0/a1's claim is EXACT); `RuntimeEmitterKind:40-42`+`:110` ✓;
  7× md5 `b654562ccff46ed62dd48e9ace325830` (`uniq -c` = `7  b654562c…`, 7 dirs) ✓;
  `ValueRef<…G: EventGrammar = AnyGrammar>:175` ✓; `src/x86_64/` = 24 files ✓; `ext/x86/` = 4
  files (LICENSE-VENDOR/bbnf.asm/x86inc.asm/x86util.asm) ✓; `nasm-rs = "0.3"` at `Cargo.toml:19`
  ✓; `parse_w11_1_number` ×7 in shipped JSON `generated.rs` ✓; `RuntimeTarget` = **12 fields**
  (`regen.rs:6-19`) ✓; `fact_schema`/`row_id`/`output_plane` are `RuntimeOutputLabels` fields
  (`grammar_provider.rs:93-95`) nested via `output_labels` ✓.
- **§2.4 by-exclusion projection (ACCEPT).** The operative-set reasoning (exclude only
  `output_dir`+`expected_files`, INCLUDE `output_labels` so the 7 distinct nested labels are
  caught) is disk-true and sound.

### The 6 addenda — each FIRES on a real surface, each catches its V3 mode (a1) — ACCEPT ×6
- **L1 verbatim-blob (ACCEPT).** Fires: `CSS_GENERATED_RS:701` + 8 sibling couriers at EXACTLY
  the cited lines (`JSON_PARSE_ONLY_GENERATED_RS:195`, `JSON_PARSE_ONLY_PARSER_RS:550`,
  `JSON_MOD_RS:572`, `JSON_HOST_RS:594`, `CSS_MOD_RS:598`, `CSS_PARSER_RS:612`, `CSS_SINK_RS:665`,
  `CSS_GENERATED_RS:701` — all 8 re-grepped, all exact). Catches V3 D1 (hand-written scanner under
  a `@generated` banner). The `.bbnf`-mutation falsifier is a correct operational test a const
  courier provably cannot pass.
- **L2 distinct-grammar-output (ACCEPT).** Fires: 7× identical md5. Catches V3 D1. The
  necessary-not-sufficient co-gate (`generator_grammar_branch_count==0` + type-count + structural
  `runtime_target_rows_collapsed`) correctly closes the relocated-seam hole the regex cannot.
- **L3 single-emitter-path (ACCEPT).** Fires: `RuntimeEmitterKind{CompiledLowering,RequestFacts}`
  at `:40-42`, dispatched `:110`. Catches V3 D1's neutral-named fork — the lens correctly notes
  the arm-census (L2) does NOT fire on the neutral enum names, which is exactly why L3 is distinct.
- **L4 phantom-generic (ACCEPT).** Fires: `ValueRef<…G: EventGrammar = AnyGrammar>:175`; the `G`
  axis is correctly identified as phantom (test-only `_proof_compiles`) and the `K=AnyKind` axis
  correctly excluded as the real one. Catches V3 D2. The preserve-rich-ast guard
  (`json_rich_navigation_preserved` SEPARATE from the ≥2 impl-count) is a correct hardening.
- **L5 timed-plane-symmetry + corpus-in-timer (ACCEPT).** Fires: `measure_mbps:3091`,
  `lightningcss_facts:528`, count `measure_mbps|lightningcss_facts` = 48; `css_canon_bench.rs`
  PRESENT (the honest harness KEPT). Catches V3 C3 (warm/micro-fixture/more-work) + C2 (lazy-vs-
  eager framing). All re-verified.
- **L6 acceleration-wiring (ACCEPT).** Fires: kernels at `runtime_simd.rs:29/:112/:169`;
  `find_css_significant` caller at `lib.rs:574`, which is AFTER the sole `#[cfg(test)]` at `:51`
  — so the caller is test-only, confirming the cfg(test)-only deadness. Catches V3 C1.

### PRUNE-sequencing soundness (a2) — ACCEPT
- **P1–P5 disk anchoring (ACCEPT).** Every PRUNE witness re-grepped: P1 (24+4 files, nasm:19),
  P2 (48 grep hits), P3 (7× md5), P4 (`GENERIC_SCAN_ROOTS:2409`/`FORBIDDEN_GENERIC_TOKENS:2420`/
  `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442`/`diagnostic-x86:2463` — all 4 at EXACT lines in
  `bbnf-bench/src/lock14_baseline.rs`), P5 (×7). All disk-true.
- **P1 ↔ `checkasm_parity.rs` build-soundness coupling (ACCEPT — the most load-bearing CH1 check).**
  a2 cites 9 ACTIVE `bbnf_simd::x86_64::…::*_scalar(…)` call sites at `:458,:464,:467,:477,:478,
  :484,:493,:497,:502`. I re-grepped: `grep -cE 'bbnf_simd::x86_64::.*_scalar\('` = **9**, and
  EACH of the 9 cited line numbers carries the named scalar call verbatim. The build-blocking
  coupling (delete `src/x86_64/` without decoupling ⇒ test crate fails to compile) is real; the
  same-wave decouple constraint is correct.
- **G3-gates-G1/G2, G4 DELETE-default-on-G-axis, exit-gate-blocks-successor (ACCEPT).** Each
  coupling cites a live witness that resolves; the dependency chain
  PRUNE→G1→G2→G3→G4→G5/G6→PROVE→H1 is internally consistent (G2 dual-gates on G1 AND P3).

### NEW finding F-A3.5 / R16 nested `output_labels` (a3) — ACCEPT (correctness-positive)
Independently verified the NEW finding is CORRECT and disk-true: `fact_schema`/`row_id`/
`output_plane` are NOT top-level `RuntimeTarget` fields — they are `RuntimeOutputLabels` fields
(`grammar_provider.rs:93-95`), nested inside `output_labels: Some(codegen::RuntimeOutputLabels
{…})` in each css_l4 row (`regen_css.rs:48-52`, re-grepped: each of the 7 rows carries a distinct
`fact_schema`/`row_id`/`output_plane`). The reconciliation with a0/a2 ("not TOP-LEVEL fields" is
right; "nested in the by-exclusion-included `output_labels`" is the sharpening) is correct, and
the recipe pin (recurse into the nest; forbid a shallow `Option`-discriminant compare) is a real
correctness hardening of the gate recipe, not a cosmetic note. This SHARPENS the audit; it does
not contradict it.

### SYNTHESIS-AUDIT-OVERFIT gate-column attribution (ACCEPT)
Re-grepped every telemetry-column line the SYNTHESIS + a0–a3 cite: `verbatim_blob_present:563`,
`emitter_fork_present:564`, `generator_grammar_branch_count:565`, `runtime_target_rows_collapsed:566`,
`generator_grammar_type_count:567`, `phantom_generic_resolved:568`, `json_rich_navigation_preserved:570`,
`generated_md5_distinct:572`, `acceleration_at_admission:575`, `x86_tree_deleted:576`,
`materialization_framing:579`, `corpus_in_timer:580` — ALL resolve to the named column at the
named SYNTHESIS.md line. The "12-field struct / enumerate-by-exclusion" correction is present at
the operative gate (`SYNTHESIS.md:410`), confirming a0's claim that the 12-vs-13 slip is folded
out at the binding gate (the gate is field-named, not count-driven).

## REVISE (1)

- **R1-CH1 — the CSS-courier LOC figure carries an inconsistent secondary range.** SYNTHESIS §2.1
  and the prior fold present the courier as "≈910 LOC (cohort-carried, not gate-keyed)," which is
  defensible — and I independently MEASURED it: the raw-string body runs `runtime_generator.rs:701`
  → closes at `:1611`, so the span is exactly 910 lines, VINDICATING the 910 figure as more than a
  cohort estimate. However a1 (`a1-six-addenda-lens-registry.md:64`, quoting the V3 seed) carries a
  WIDER "~646–910-LOC" range in the same lens entry, and that lower bound (646) is neither re-derived
  nor reconciled against the disk-measured 910. Non-blocking: no gate keys on the LOC (the binding
  gate is `verbatim_blob_present==false` + the `.bbnf`-mutation test), so the figure is descriptive,
  not dispositive. Fold (single-edit): annotate the a1 entry as "≈910 LOC body span (disk-measured
  `:701`→`:1611`); the V3-seed '646–910' range is the seed's pre-measurement estimate, superseded"
  so a downstream reader does not treat the wider range as a verified bound. This is a non-gate
  prose-precision annotation, single-edit, non-architectural — REVISE, not REJECT.

## Verdict

The residual-overfit audit is COMPLETE (V3 D1–D4/C1–C3 → L1–L6/P1–P5, 1:1, no drop). The 6 addenda
are EXECUTABLE (every check is a grep/diff/md5/samply over the live tree) and each CORRECTLY catches
its V3 failure mode, firing on a real disk surface verified this pass — none decorative. The
PRUNE-sequencing is SOUND and disk-grounded (P1↔checkasm 9-site build-coupling re-verified exact;
the chain is internally consistent). Every dispositive path:line/SHA/count/md5/gate-column resolves
on disk at HEAD `83b66db42`. The one REVISE is a non-gate LOC-range precision annotation.

## Tally
ACCEPT 9 · REVISE 1 · REJECT 0 — **90.0%**. (Sections: a0 goalset audit · 6 addenda fire+catch ·
a2 PRUNE-anchoring · a2 P1↔checkasm build-coupling · a2 sequencing-couplings · a3 R16 NEW finding ·
SYNTHESIS gate-column attribution · HEAD-anchoring honesty · 6/6-addenda-non-decorative = 9 ACCEPT;
the a1 LOC-range = 1 REVISE.) The single REVISE is non-gate, single-edit, non-architectural; zero
REJECT — every dispositive witness is disk-true.

TALLY accept=9 revise=1 reject=0
