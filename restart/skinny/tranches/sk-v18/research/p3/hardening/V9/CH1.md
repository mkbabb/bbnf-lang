# SK-V18 S-P3 CHALLENGE — V9 / CH1 — GATE-FALSIFIABILITY lens

Lens: GATE-FALSIFIABILITY. Does every wave exit-gate (and the close / telemetry claims they
discharge) name a CONCRETE falsifier — a grep / test / bench / count that turns RED — rather than a
prose assertion? Flag any unfalsifiable gate, broken sequence, or addenda violation. Cycle V9;
prior V1–V8 all reject=0 (V6/CH1 = 14A/2R/0; V7/CH1 = 14A/2R/0 — both V7 REVISEs folded; V8/CH1 =
14A/0R/0, the FIRST clean cycle). Mandate: confirm whether the V8 clean holds for the
2-consecutive-clean fixed point — drive out any RESIDUAL precision REVISE, catch any genuine
REJECT, and be PROPORTIONATE (a wording nit on a 1660-line doc is REVISE only if it would mislead an
implementer; do not invent churn).

Reviewed: `restart/skinny/tranches/sk-v18/SPEC.md` (1660 lines, full) against
`research/p2/SYNTHESIS-RESEARCH.md §3` (per-wave entry/exit sequencing) and
`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1/§2/§5` (the 6 addenda + R16 + sequencing). Independent
re-grep this pass — not relying on the prior verdicts' conclusions.

## V8 carry-forward — the FIRST clean cycle, re-confirmed (nothing reopened)

V8/CH1 was the first clean CH1. It had no REVISE to fold, so there is no carry-forward edit to
re-verify; the obligation this pass is to INDEPENDENTLY re-run the sweep and confirm no surviving
instance of the V6/V7 REVISE class (a `!= X` REJECT-predicate conjunct restated without a named RED
while its siblings carry one; a column the doc flags as the unfalsifiable hazard yet restated "by
construction") and no NEW unfalsifiable gate / broken sequence / addenda violation. Both V7 REVISE
folds (the §1/§7 "not by construction" alignment at lines 408/1196; the §9
`sheets_value_instantiates_g4_trait` inline `rg 'impl (Cursor|DocumentView) … for' grammars/sheets/`
falsifier at lines 1480/1508) re-confirmed present on disk this pass.

## Disk-truth spot-check (gate pre-state RED witnesses are real, not phantom)

- P1 `find …/src/x86_64 …/ext/x86 -type f` = **28** (matches SPEC "today 28").
- P3 `ls css_l4_*/generated.rs` = **7**; `md5 -q | sort | uniq -c` = **7× b654562c…** (the
  pre-collapse RED witness is exact).
- `regen.rs:5` = `#[derive(Clone, Copy, Debug)]`, NO `PartialEq` (the R16 +1-line claim is real).
- G6 census glob `grammars/*/generated.rs` = **8** files today (7 css_l4 + json) — a glob that
  survives the P3 collapse to the singular CSS scan (the census target is real).

Every gate the SPEC names fires against a real on-disk witness or a stated RED operation. No gate is
keyed on a nonexistent grep target — none found.

---

## Enumeration (each exit-gate / telemetry / close claim under the lens)

### P1 (§3.1) — DELETE x86 surface — ACCEPT
`find …/src/x86_64 …/ext/x86 -type f == 0` (RED if >0, today 28); `grep -riE 'avx|gfni|sve|x86|nasm'`
collapses to the neutral-comment floor (non-neutral hit RED); BUILD-SOUNDNESS `cargo build` + `cargo
test --no-run` clean (the 9 `*_scalar` call sites resolve into `src/x86_64/`; deletion without the
same-commit decouple BREAKS the build — a genuine RED). The "deletion list reach-matched to the
verify grep" note is itself falsifier-protection. `x86_tree_deleted == true` consumed.

### P2 (§3.2) — DELETE warm micro-fixture bench — ACCEPT
`grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs == 0` (today 48 → RED if nonzero);
`css_canon_bench` present + green; the extracted 9-field oracle still asserts. Three mechanical
RED-able checks. `corpus_in_timer` consumed.

### P3 (§3.3) — collapse 7 css_l4 replicas + RuntimeTarget row-collapse — ACCEPT
The most carefully-falsified P-gate (it carries R16). The SPEC splits md5 into a pre-collapse RED
witness (7× `b654562c…`) and a post-collapse GREEN witness, and pre-empts the lens's own concern: the
naive post-collapse self-glob over `css_l4_*` "would be a SINGLE file with no possible pair — an
unfalsifiable check," replaced with the CROSS-GRAMMAR `md5 …/grammars/{json,css_l4}/generated.rs |
uniq -c → no byte-identical pair." The structural co-gate `runtime_target_rows_collapsed == true` via
full-row `PartialEq` is mechanically RED-able (a relocated per-grammar seam makes two would-be-equal
rows compare UNEQUAL). The (i) full-row seam check vs (ii)
config-tuple-minus-(output_dir,expected_files) collapse-count split is precise — the two artefact-path
columns are excluded from (ii) ONLY, never from (i). Sound.

### P4 (§3.4) — fix Lock-14 green-by-exclusion gate — ACCEPT
The re-inject falsifier: inject a `GENERATED_RS`-bearing or `EventGrammar` token into
`runtime_generator.rs` → `accepts_current_allowlist` RED → revert. The SPEC corrects the upstream
"re-inject a `JsonSink` token" phrasing (a bare `JsonSink` is NOT in the extended set and would NOT
fire — a SILENT no-fire), and scopes `_RS` to the `GENERATED_RS` suffix so the six surviving
MOD/HOST/PARSER/SINK scaffold consts do not false-RED under `source.contains` substring semantics —
itself a falsifiability hardening. `lock14_gate_scans_codegen`/`forbidden_generic_tokens_extended`
consumed. Sound.

### P5 (§3.5) — purge metalang leak — ACCEPT
`grep -c parse_w11_1_number == 0` (today 7); no `w[0-9]+`/`sk_v`/corpus tag; `regen --check` clean —
the latter the binding falsifier that a SOURCE fix (not a hand-patch) landed (a hand-patch diverging
from fresh generator output fails `regen --check` → RED). `metalang_leak_present == false` consumed.

### G1 (§4) — JSON projection — ACCEPT
Five conjuncts all RED-able: (1) byte-equivalence diff-control via `EmittedSource::check_dir` exact
`actual != *source`, ±5% line delta demoted to SOFT tripwire that "does NOT gate"; (2)
`.bbnf`-mutation falsifier (drop `bool` → `b't'/b'f'` arms vanish, revert); (3) hot-leaf grep for `fn
parse_object_value_at_direct` with identical inline cfg + `sink.*` call sites; (4)
`verbatim_blob_present == false` by grep for `r#"…"#` bodies == 0 + `JSON_PARSE_ONLY_GENERATED_RS`
deleted; (5) P5 re-assertion on the REGENERATED file. The §6 (a)-(d) primitive gate is
machine-checked: (b) digit-class WIDEN mutation, (d) the arithmetic `g1_leaf_primitive_loc <=
g1_leaf_primitive_profiled_leaf_extent` god-kernel REJECT. Every column carries a stated `!= X`
REJECT.

### G2 (§5) — CSS lowering — ACCEPT
Five conjuncts each RED-able: (1) `CSS_GENERATED_RS` grep == 0; (2) per-primitive arg-mutation
falsifier (mutate `stylesheet.bbnf` → emitted delimiter byte-array changes); (3) the 9-field cssparser
oracle EXACT parity "EXACTLY across the 4 benched corpora" (REDs on any per-field mismatch); (4) the
EXPLICIT >SOTA-regression gate replacing the un-re-lockable absolute floor (correctly named "the
unfalsifiable hazard") with the SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no regression vs
`track1_rich_over_lcss_ratio_pre_g2` captured AT G2 ENTRY in one quiet run — two figures in one plane
is a falsifiable comparison; (5) the FORCED `css_balanced_component_scan` demotion (gate REJECTs a
neutral name with zero structurally-compatible non-CSS caller). The (d) god-kernel check is the
arithmetic `g2_balanced_scan_primitive_loc <= g2_balanced_scan_profiled_leaf_extent`. Gate-before-speed
explicit. The two bare-looking conjuncts (`g2_css_rich_projection_not_flattened`, `g2_css_replica_singular`)
are each co-falsified by a NAMED sibling the prose points to — the former by the 9-field oracle parity
(a flattened CSS projection cannot reproduce `CssRule::selector_count`/`CssDeclaration::typed_value` and
diverges from the oracle) + the G2.4-named G4 substitution falsifier; the latter by `runtime_target_rows_collapsed`
(the R16 count==1 per `grammar_name`) + `generated_md5_distinct`. Falsifiable in mechanism with the
co-falsifier named (the V7/V8-disposed pattern). Sound.

### G3 (§6) — un-fork emitter, FIVE-conjunct gate — ACCEPT
Conjuncts 1-3 grep counts (`RuntimeEmitterKind|CompiledLowering|RequestFacts == 0`; `match
grammar`/`"json"`/`"css"` literal arms == 0; grammar-named emit-type == 0). Conjunct 4 the STRUCTURAL
`runtime_target_rows_collapsed` full-row `PartialEq` (the only check that catches the relocated seam
the arm-grep is "syntactically incapable" of seeing). Conjunct 5 `emit_shape_source == lowered_program`
is grep-the-`render(program)`-body-for-`target.*`-reads == 0, with the binding rationale stated:
"Without this fourth conjunct, the §5-risk-1 relocated seam … passes all of conjuncts 1-4 under a green
gate" — a lens-grade argument naming WHY the cheaper conjuncts are insufficient. Conjuncts 6/7/8 are
consumed columns (byte-equivalent `regen --check` clean + "diff of regenerated vs shipped == empty for
every grammar"; JSON hot-leaf grep; CSS same-run ratio), "CONSUMED columns the gate REJECTs on, NOT
prose-only gates." Strong.

### G4 (§7) — shared trait + phantom, THREE-conjunct gate — ACCEPT
The model gate. (1) `phantom_generic_resolved == deleted` by test-excluded grep
`EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData == 0`; (2)
`json_rich_navigation_preserved == true` by the G4.2-conjunct-2 BYTE-EQUAL DIFF (`value.rs` navigation
surface diffs EMPTY vs pre-G4 ∧ JSON 51/51) — NOT "by construction"; (3) `shared_trait_non_collapsible
== true` by the substitution falsifier (swap JSON's nav impl for CSS's → crate FAILS to compile; a
degenerate-equal CSS impl COMPILES → false → REJECT) — a compile-failure differential, maximally
concrete. The bare `shared_trait_impl_count >= 2` is explicitly demoted to "necessary-NOT-sufficient."
Cross-cutting conjunct 4 (no second substrate, Lock 1) is `grep for a new *Tape/*Cursor substrate type
== 0; no per-leaf Box::new`. With V7-R1 folded, §1 standing-law and §7 intro prose align with this gate
(no "by construction" license). Sound.

### G5/G6 (§8) — neutral scan retarget — ACCEPT
`acceleration_at_admission == admission` requires BOTH conjuncts, each RED-able: (i) caller census `rg
runtime_simd::find_… grammars/*/generated.rs` NON-EMPTY (NOT `#[cfg(test)]`); (ii)
`simd_admission_profile_sampled == true` with the V6-folded `self_time_samples == 0` RED predicate (the
entry ABSENT or at exactly zero attribution → `dead` → REJECT — gates on PRESENCE-with-attribution, not
a magnitude). The "Falsifiers (each gate RED-able)" block is explicitly enumerated: revert the call-site
swap → census empty → `dead` → RED; mutate the two-fan OR-reduce by one byte → parity FAIL → RED; emit
7-ways → singular FALSE → RED; emit a speedup off the checkasm plane → addendum-5 REJECT. The
bare-looking `significant_set_is_caller_data == true` is co-falsified by `css_scan_call_site_singular`
(the generator-emits-the-CALL census the `simd_admission_caller` grep verifies) — falsifiable in
mechanism. `g6_speedup_median_mbps` null pre-H1 "does NOT gate G5/G6" (deferred to the H1 timer),
correctly avoiding a gate on an un-re-locked figure. The most explicitly-falsified gate in the doc.
Sound.

### PROVE (§9) — Sheets via generator only — ACCEPT
With V7-R2 folded, all nine conjuncts carry an inline grep/count/compile RED. `sheets_grammar_shape ==
pratt-operator` is "NON-HOLLOW PROVEN BY A CONCRETE STRUCTURAL FALSIFIER, NOT asserted by construction"
— the `>= 7` chained level-fn count + the cyclic `paren_expr→expression` back-edge (an integer test);
`generated_md5_distinct` (`md5 -q` over the trio); `sheets_verbatim_blob_present` (`grep -c
'const.*_RS.*r#' == 0`); the arm-census + type-census as literal `rg` patterns (`GoogleSheets`
un-abbreviated); `generator_grammar_count == 3`; R16 full-row; the NOW-FOLDED
`sheets_value_instantiates_g4_trait` (the `rg 'impl (Cursor|DocumentView) … for' grammars/sheets/`
NON-EMPTY + compiles falsifier, isomorphic to G4.2-conjunct-3); `import_closure_relaxation_is_data`
co-falsified by the named `generator_grammar_branch_count == 0` arm-census (a `GoogleSheets =>
import_closure` arm REDs it; the falsifiers block names "(b) G3 routes Sheets via a `GoogleSheets =>`
arm" as a RED). `json_css_preservation_held` carries byte-equivalence + `dirty_generated_state ==
clean` + the 51/51 ±1.0% band (concrete). The line-1474 "emitted by construction" is benign — it
grounds the structural consequence of the G4 axis-deletion AND backstops it with `rg
'EventGrammar|\*EventGrammar' … == 0` + the P4 emit-site catch (a falsifier-backed claim, not an
unfalsifiable close). The binding fallback `sheets_emission_path == shim → outcome N` is correctly NOT
a gate-REJECT but the negative-control verdict surfaced honestly (never paper-closed). Sound.

### H1 (§10) — honesty close — ACCEPT
`materialization_framing` is a CLOSED enum `{lazy-rich-vs-eager-cssom|undisclosed}` — the §0.4 note
that an open `|...` "accepts any string and is unfalsifiable" is honored; `undisclosed` turns the gate
RED. The load falsifier is concrete: an absolute Mbps claim with harness-stamped `host_loadavg >= 1.0`
(or no stamp) → RED; the H1 harness MUST stamp `host_loadavg`. `regen --check` exit ≠ 0 → RED.
Speedup-plane mismatch (figure off the checkasm plane) → addendum-5 REJECT. The outcome split `A`
(quiet) vs `S` (directional) vs REJECT is precise. The V6-folded ≥1-regular-corpus floor matches G2
(`css_sota_ratio_held` on ≥1 regular corpus, animate OR bootstrap; a tailwind miss re-confirmed as a
residual, NOT re-litigated as an H1 block). Sound.

### §0.4 telemetry schema (the gate consumer) — ACCEPT
Producer-AND-consumer bound ("a column emitted but never consumed FAILS the wave"). Each of the 13
binding columns has a typed domain + a per-wave MUST-be predicate; every supporting column maps to a
named consuming wave slice. The `acceleration_at_admission` enum is held to the SAME two-value domain
(`admission|dead`) §0.3 and §8/G6 decide on — "a third state would make the gate non-deterministic
between the schema and the G6 falsifier." The closed `materialization_framing` enum is justified as
un-spoofable. Sound.

### §0.3 outcome enum + §0.5 goalset — ACCEPT
`A`/`S`/`C`/`N`/`L` each tied to a measurable condition (checkasm PASS with no corpus-in-timer figure =
`C` never `A`; figure under `host_loadavg >= 1.0` = `S` not `A`; shim = `N`). §0.5's per-corpus table
binds the same-run `> 1.0×` ∧ no-pre-G2-regression gate with "≥1 regular corpus (animate OR bootstrap)
crossing mandatory." The gate REJECTs "any corpus-average substituting for per-corpus ratios" and "any
single-tuple broadcast." Sound.

### Lattice / sequencing (§2.1, §3 close, GROUND fold) — ACCEPT (no broken sequence)
Cross-checked the SPEC lattice against SYNTHESIS-RESEARCH §3 and SYNTHESIS-AUDIT §5, and verified
internal consistency across all three SPEC restatement sites (wave-manifest table lines 438-444, §2.1
lattice diagram lines 539-547, per-wave entry-gate sections). Consistent: G1 = P-cluster closed (P4
live, P5 closed); G2 dual-gates on G1 ∧ P3 ∧ P4-live; G3 = G1 ∧ G2 ∧ P4-live ∧ P3-row-collapse; G4 =
G1 ∧ G2 ∧ G3; G5/G6 = P1 ∧ P3 ∧ G3 (PARALLEL to G4/PROVE, the seq/C7 correction — NOT under G4); PROVE
= G3 ∧ G4 (G4 a DIRECT conjunct, the seq/C6 correction — "PROVE NEVER admits before G4 closes"); H1 =
G5/G6 ∧ PROVE both closed (the two parallel branches join at H1). The audit-§5 diagram nested G5/G6
UNDER G4 and PROVE under G5/G6; the SPEC explicitly carries the seq/C6+C7 corrections that supersede it
and names them as the GROUND `seq.md` revise-fold (lines 1646-1652). The s6/C4 FORCED demotion of
`balanced_component_scan` → `css_balanced_component_scan` is consistently applied: the 4 bare
occurrences (lines 9/377/382/1649) are all demotion-discussion context, never a live admitted primitive
name; the 15 `css_balanced_component_scan` references are the operative ones. The 6 addenda each map to
a wave with a RED-able falsifier (verbatim-blob→G1/G2 grep; distinct-output→P3+G3 4-co-gate;
single-emitter→G3 5-conjunct; phantom→G4 substitution falsifier; corpus-in-timer→P2/G2/H1;
accel-wiring→G6 census+profile). R16 pinned to full-row `PartialEq` in P3/G3 with hand-rolled-prose-field
comparison FORBIDDEN. No broken sequence, no addenda violation.

---

## Residual-precision sweep (the V9 mandate: confirm the fixed point holds)

I independently re-ran the V6/V7 REVISE-class hunt — every `!= X` / `∉` / `<` REJECT predicate in
every per-wave consumer block (lines 783, 890, 1044/1046, 1168, 1276, 1393, 1468, 1514/1519, 1594) —
and the "by construction" / "wired" / "advisory" / "integrated" close-verb hunt. Findings:

- The two V7 REVISE folds are present on disk (lines 408/1196 "not by construction"; lines 1480/1508
  the `sheets_value_instantiates_g4_trait` inline falsifier). No regression.
- The four bare-looking conjuncts I re-examined this pass — `g2_css_rich_projection_not_flattened`,
  `g2_css_replica_singular`, `significant_set_is_caller_data`, `import_closure_relaxation_is_data` (and
  `json_css_preservation_held`) — are EACH co-falsified by a NAMED sibling conjunct the prose explicitly
  points to (the 9-field oracle parity + G2.4 gating note; the R16 row-collapse count + md5-distinct;
  the `simd_admission_caller` census; the `generator_grammar_branch_count == 0` arm-census). This is the
  EXACT pattern V7 ACCEPTED for `import_closure_relaxation_is_data` and V8 ACCEPTED for
  `g2_css_rich_projection_not_flattened`. Raising any would re-litigate a settled disposition and invent
  churn the proportionality clause forbids. No REVISE.
- All "by construction" occurrences are negated/forbidden (the close-cond #4 REJECT, the G4.2 / PROVE
  "not by construction") or falsifier-backed (line 1474, grounded by `rg … == 0` + the P4 catch). All
  "wired"/"advisory"/"integrated" occurrences are in the FORBIDDEN-close list (line 416-417) or the
  demoted SOFT tripwire `line_delta_vs_oracle` (explicitly "never REJECT"). No gate closes on a prose
  verb.

The doc remains in the state V8 reached: every `!= X` REJECT predicate in every per-wave consumer block
resolves to a stated grep, count, mutate-and-observe, compile-failure differential, exit code, or
same-run bench comparison — or is co-falsified by a NAMED sibling conjunct the prose points to. No
surviving instance of the V6/V7 REVISE class; no new unfalsifiable gate, broken sequence, or addenda
violation.

---

## Verdict summary

The SPEC is CLEAN under the GATE-FALSIFIABILITY lens. Every wave exit-gate names at least one concrete
falsifier — a grep count, a mutate-and-observe, a compile-failure differential, an integer count, a
`regen --check` exit code, or a same-run bench comparison — and the doc repeatedly anticipates the
lens's own concern (the §3.3 unfalsifiable self-glob it patches; the §0.4 closed-enum justification;
the G3 conjunct-5 "passes conjuncts 1-4 under a green gate" rationale; the G4 substitution falsifier
replacing "by construction"; the V6-folded G6 `self_time_samples == 0` predicate + H1 ≥1-regular floor;
the V7-folded §1/§7 "not by construction" alignment + §9 trait-instantiation falsifier). This is the
SECOND consecutive clean CH1 cycle (V8 14A/0R/0, V9 14A/0R/0) — the 2-consecutive-clean fixed point is
reached for this lens. No unfalsifiable gate, no broken sequence, no addenda violation found; no
genuine REJECT.

ACCEPT: 14 (P1, P2, P3, P4, P5, G1, G2, G3, G4, G5/G6, PROVE, H1, §0.4 schema, §0.3+§0.5+lattice)
REVISE: 0
REJECT: 0

TALLY accept=14 revise=0 reject=0
