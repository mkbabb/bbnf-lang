# SK-V18 S-P3 CHALLENGE — V7 / CH1 — GATE-FALSIFIABILITY lens

Lens: GATE-FALSIFIABILITY. Does every wave exit-gate (and the close/telemetry claims they
discharge) name a CONCRETE falsifier — a grep / test / bench / count that turns RED — rather than a
prose assertion? Flag any unfalsifiable gate, broken sequence, or addenda violation. Cycle V7;
prior V1–V6 all reject=0 (V6/CH1 = accept=14 revise=2 reject=0). Mandate: drive out the RESIDUAL
precision REVISEs to a 2-consecutive-clean fixed point; proportionate (a wording nit is REVISE only
if it would mislead an implementer).

Reviewed: `restart/skinny/tranches/sk-v18/SPEC.md` (1657 lines, full) against
`research/p2/SYNTHESIS-RESEARCH.md §3` (per-wave entry/exit sequencing) and
`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1/§2/§5` (the 6 addenda + R16 + sequencing).

## V6 carry-forward — both prior REVISEs CONFIRMED FOLDED (independent re-grep)

- V6-R1 (§8/G6 `simd_admission_profile_sampled` lacked a thresholded RED predicate): FOLDED.
  Line 1350 now reads "the RED predicate is `self_time_samples == 0` (the entry ABSENT from the
  sample, or present at exactly zero attribution) → `acceleration_at_admission == dead` → REJECT —
  the conjunct gates on PRESENCE-with-attribution, not a speedup magnitude." The existence-assertion
  read is closed; isomorphic to the census conjunct.
- V6-R2 (§10/H1 `css_sota_ratio_held` restated without the "≥1 regular corpus" floor, over-reading
  as all-four-corpora): FOLDED. Lines 1563-1567 and the telemetry line 1587 now read "on ≥1 regular
  corpus (animate OR bootstrap) … the SAME binding floor G2 closed on (§0.5), a tailwind miss …
  re-confirmed as a residual here NOT re-litigated as an H1 block." H1 re-confirmation now matches
  the G2 gate it re-confirms.

## Disk-truth spot-check (gate pre-state RED witnesses are real, not phantom)

- P3 `css_l4_*/generated.rs` = **7 files** today (the pre-collapse RED witness; matches "today 7").
- G6 census glob `grammars/*/generated.rs` = **8 files** today (7 css_l4 + json), a glob that
  survives the P3 collapse to the singular CSS scan (matches the SPEC's census target).
- §8/G6 `self_time_samples == 0` RED predicate present at line 1350; §10/H1 ≥1-regular floor present
  at lines 1563/1587. Both V6-fold sites land on real lines.

Every gate the SPEC names fires against a real on-disk witness or a stated RED operation. No gate is
keyed on a nonexistent grep target (the first unfalsifiable-gate failure mode) — none found.

---

## Enumeration (each exit-gate / telemetry / close claim under the lens)

### P1 (§3.1) — DELETE x86 surface — ACCEPT
`find …/src/x86_64 …/ext/x86 -type f == 0` (count → RED if >0); `grep -riE 'avx|gfni|sve|x86|nasm'`
collapses to the neutral floor (non-neutral hit RED); build-soundness `cargo build` + `cargo test
--no-run` clean (the 9 `*_scalar` call sites resolve into `src/x86_64/`; deletion without the
same-commit decouple BREAKS the build — a genuine RED). The "deletion list reach-matched to the
verify grep" note is itself falsifier-protection. `x86_tree_deleted` consumed.

### P2 (§3.2) — DELETE warm micro-fixture bench — ACCEPT
`grep -c 'measure_mbps\|lightningcss_facts' == 0` (today 48 → RED if nonzero); `css_canon_bench`
present + green; the extracted 9-field oracle still asserts. `corpus_in_timer` consumed. Three
mechanical RED-able checks.

### P3 (§3.3) — collapse 7 css_l4 replicas + RuntimeTarget row-collapse — ACCEPT
The most carefully-falsified P-gate (it carries R16). The SPEC splits md5 into a pre-collapse RED
witness (7× `b654562c…`) and a post-collapse GREEN witness, AND pre-empts the lens's own concern:
the naive post-collapse self-glob `css_l4_*` "would be a SINGLE file with no possible pair — an
unfalsifiable check," replaced with the CROSS-GRAMMAR `md5 …/grammars/{json,css_l4}/generated.rs |
uniq -c → no byte-identical pair." The structural co-gate `runtime_target_rows_collapsed == true`
via full-row `PartialEq` is mechanically RED-able. The (i) full-row seam check vs (ii)
config-tuple-minus-(output_dir,expected_files) collapse-count split is precise (and the SPEC
correctly excludes the two artefact-path columns from (ii) ONLY, not from (i)). Sound.

### P4 (§3.4) — fix Lock-14 green-by-exclusion gate — ACCEPT
The re-inject falsifier: inject a `GENERATED_RS`-bearing or `EventGrammar` token into
`runtime_generator.rs` → `accepts_current_allowlist` RED → revert. The SPEC corrects the upstream
"re-inject a `JsonSink` token" phrasing (a bare `JsonSink` is NOT in the extended set and would NOT
fire — a SILENT no-fire), and scopes `_RS` to the `GENERATED_RS` suffix so the six surviving
MOD/HOST/PARSER/SINK scaffold consts do not false-RED under `source.contains` substring semantics.
That scoping is itself a falsifiability hardening (a bare `CSS_`/`_RS` would collide).
`lock14_gate_scans_codegen`/`forbidden_generic_tokens_extended` consumed. Sound.

### P5 (§3.5) — purge metalang leak — ACCEPT
`grep -c parse_w11_1_number == 0` (today 7); no `w[0-9]+`/`sk_v`/corpus tag; `regen --check` clean —
the latter the binding falsifier that a SOURCE fix (not a hand-patch) landed (a hand-patch diverging
from fresh generator output fails `regen --check` → RED). `metalang_leak_present == false` consumed.

### G1 (§4) — JSON projection — ACCEPT
Five conjuncts all RED-able: (1) byte-equivalence diff-control via `EmittedSource::check_dir` exact
`actual != *source`, ±5% line delta demoted to SOFT tripwire that "does NOT gate"; (2)
`.bbnf`-mutation falsifier (drop `bool` → `b't'/b'f'` arms vanish, revert); (3) hot-leaf grep for
`fn parse_object_value_at_direct` with identical inline cfg + `sink.*` call sites; (4)
`verbatim_blob_present == false` by grep for `r#"…"#` bodies == 0 + `JSON_PARSE_ONLY_GENERATED_RS`
deleted; (5) P5 re-assertion on the REGENERATED file. The §6 (a)-(d) primitive gate is
machine-checked: (b) digit-class WIDEN mutation, (d) the arithmetic `g1_leaf_primitive_loc <=
g1_leaf_primitive_profiled_leaf_extent` god-kernel REJECT. Every column has a stated `!= X` REJECT.

### G2 (§5) — CSS lowering — ACCEPT
Five conjuncts each RED-able: (1) `CSS_GENERATED_RS` grep == 0; (2) per-primitive arg-mutation
falsifier (mutate `stylesheet.bbnf` → emitted delimiter byte-array changes); (3) the 9-field
cssparser oracle EXACT parity "EXACTLY across the 4 benched corpora" (a per-corpus per-field
equality assertion that REDs on any mismatch — falsifiable); (4) the EXPLICIT >SOTA-regression gate
replacing the un-re-lockable absolute floor (correctly named "the unfalsifiable hazard") with the
SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no regression vs `track1_rich_over_lcss_ratio_pre_g2`
captured AT G2 ENTRY in one quiet run — two figures in one plane is a falsifiable comparison; (5)
the FORCED `css_balanced_component_scan` demotion (gate REJECTs a neutral name with zero
structurally-compatible non-CSS caller). The (d) god-kernel check is the arithmetic
`g2_balanced_scan_primitive_loc <= g2_balanced_scan_profiled_leaf_extent`. Gate-before-speed
explicit. Sound. (Note: `g2_css_rich_projection_not_flattened` is emitted at G2 but consumed at G4 —
the G2.4 prose "tracked here, gated at G4" names the consuming wave, so it is not a producer-only
violation; ACCEPT.)

### G3 (§6) — un-fork emitter, FIVE-conjunct gate — ACCEPT
Conjuncts 1-3 grep counts (`RuntimeEmitterKind|CompiledLowering|RequestFacts == 0`; `match
grammar`/`"json"`/`"css"` literal arms == 0; grammar-named emit-type == 0). Conjunct 4 the
STRUCTURAL `runtime_target_rows_collapsed` full-row `PartialEq` (the only check that catches the
relocated seam the arm-grep is "syntactically incapable" of seeing). Conjunct 5 `emit_shape_source
== lowered_program` is grep-the-`render(program)`-body-for-`target.*`-reads == 0, with the binding
rationale stated: "Without this fourth conjunct, the §5-risk-1 relocated seam … passes all of
conjuncts 1-4 under a green gate." That is a lens-grade argument — the gate names WHY the cheaper
conjuncts are insufficient and supplies the structural falsifier that is not. Conjuncts 6/7/8 are
consumed columns (byte-equivalent `regen --check`, JSON hot-leaf grep, CSS same-run ratio),
"CONSUMED columns the gate REJECTs on, NOT prose-only gates." Strong.

### G4 (§7) — shared trait + phantom, THREE-conjunct gate — ACCEPT (binding gate); see REVISE-1 (prose)
The binding gate is concrete: (1) `phantom_generic_resolved == deleted` by test-excluded grep
`EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData == 0`; (2)
`json_rich_navigation_preserved == true` by the G4.2-conjunct-2 BYTE-EQUAL DIFF (`value.rs`
navigation surface diffs EMPTY vs pre-G4 ∧ JSON 51/51) — NOT "by construction"; (3)
`shared_trait_non_collapsible == true` by the substitution falsifier (swap JSON's nav impl for CSS's
→ crate FAILS to compile; a degenerate-equal CSS impl COMPILES → false → REJECT) — a compile-failure
differential, maximally concrete. The bare `shared_trait_impl_count >= 2` is explicitly demoted to
"necessary-NOT-sufficient." The G4.2 EXIT GATE is the model gate. The REVISE-1 below concerns the
STANDING-LAW prose restatement of the same column, not this gate.

### G5/G6 (§8) — neutral scan retarget — ACCEPT
`acceleration_at_admission == admission` requires BOTH conjuncts, each RED-able: (i) caller census
`rg runtime_simd::find_… grammars/*/generated.rs` NON-EMPTY (NOT `#[cfg(test)]`); (ii)
`simd_admission_profile_sampled == true` with the V6-folded `self_time_samples == 0` RED predicate.
The "Falsifiers (each gate RED-able)" block is explicitly enumerated: revert the call-site swap →
census empty → `dead` → RED; mutate the two-fan OR-reduce by one byte → parity FAIL → RED; emit
7-ways → singular FALSE → RED; emit a speedup off the checkasm plane → addendum-5 REJECT. The most
explicitly-falsified gate in the doc. `g6_speedup_median_mbps` null pre-H1 "does NOT gate G5/G6"
(deferred to the H1 timer), correctly avoiding a gate on an un-re-locked figure. Sound.

### PROVE (§9) — Sheets via generator only — REVISE (one bare conjunct; see REVISE-2)
The negative-control gate is the one most at risk of a "non-hollow by construction" assertion, and
the SPEC pre-empts the headline: `sheets_grammar_shape == pratt-operator` is "NON-HOLLOW PROVEN BY A
CONCRETE STRUCTURAL FALSIFIER, NOT asserted by construction" — the `>= 7` chained level-fn count + the
cyclic back-edge (an integer test). The arm-census greps are spelled as literal `rg` patterns. The
binding fallback `sheets_emission_path == shim → outcome N` is correctly NOT a gate-REJECT but the
negative-control verdict surfaced honestly. SEVEN of the nine conjuncts carry an inline grep/count
(`md5 -q`; `grep -c 'const.*_RS.*r#'`; the `>= 7` count; the `rg -nE 'match...'` arm-census; the `rg
'JsonParser|...'` type-census; the grammar-count; R16 full-row). BUT two conjuncts —
`sheets_value_instantiates_g4_trait == true` and `import_closure_relaxation_is_data == true` (line
1476), both listed as `!= true` REJECT predicates in the consumer (lines 1514-1515) — are stated
BARE. `import_closure_relaxation_is_data` is co-falsified by the named `generator_grammar_branch_count
== 0` arm-census (a `match grammar { GoogleSheets => import_closure }` arm REDs it; the falsifiers
block names "(b) G3 routes Sheets via a `GoogleSheets =>` arm" as a RED) — so it is falsifiable in
mechanism, ACCEPT. `sheets_value_instantiates_g4_trait` names NO RED operation, unlike its 7 sibling
conjuncts — this is the sibling-asymmetry pattern V6 flagged for `simd_admission_profile_sampled`.
See REVISE-2.

### H1 (§10) — honesty close — ACCEPT
`materialization_framing` is a CLOSED enum `{lazy-rich-vs-eager-cssom|undisclosed}` — the §0.4 note
that an open `|...` "accepts any string and is unfalsifiable" is honored; `undisclosed` turns the
gate RED. The load falsifier is concrete: an absolute Mbps claim with harness-stamped `host_loadavg
>= 1.0` (or no stamp) → RED; the H1 harness MUST stamp `host_loadavg`. `regen --check` exit ≠ 0 →
RED. Speedup-plane mismatch (figure off the checkasm plane) → addendum-5 REJECT. The outcome split
`A` (quiet) vs `S` (directional) vs REJECT is precise. The V6-folded ≥1-regular-corpus floor now
matches G2. Sound.

### §0.4 telemetry schema (the gate consumer) — ACCEPT
Producer-AND-consumer bound ("a column emitted but never consumed FAILS the wave"). Each of the 13
binding columns has a typed domain + a per-wave MUST-be predicate; every supporting column maps to a
named consuming wave slice. The `acceleration_at_admission` enum is held to the SAME two-value domain
(`admission|dead`) §0.3 and §8/G6 decide on — "a third state would make the gate non-deterministic
between the schema and the G6 falsifier" (a falsifiability-coherence argument across three doc
locations). The closed `materialization_framing` enum is justified as un-spoofable. Sound.

### §0.3 outcome enum + §0.5 goalset — ACCEPT
`A`/`S`/`C`/`N`/`L` each tied to a measurable condition (checkasm PASS with no corpus-in-timer figure
= `C` never `A`; figure under `host_loadavg >= 1.0` = `S` not `A`; shim = `N`). §0.5's per-corpus
table binds the same-run `> 1.0×` ∧ no-pre-G2-regression gate with "≥1 regular corpus (animate OR
bootstrap) crossing mandatory" — a concrete crossing requirement. The gate REJECTs "any corpus-average
substituting for per-corpus ratios" and "any single-tuple broadcast." Sound.

### Lattice / sequencing (§2.1, §3 close, GROUND fold) — ACCEPT (no broken sequence)
Cross-checked the SPEC lattice against SYNTHESIS-RESEARCH §3 and SYNTHESIS-AUDIT §5. Consistent: G2
dual-gates on G1 ∧ P3 ∧ P4-live; G3 = G1 ∧ G2 ∧ P4-live ∧ P3-row-collapse; G4 = G1 ∧ G2 ∧ G3; G5/G6
= P1 ∧ P3 ∧ G3 (PARALLEL to G4, the seq/C7 correction — NOT under G4); PROVE = G3 ∧ G4 (G4 a DIRECT
conjunct, the seq/C6 correction — "PROVE NEVER admits before G4 closes"); H1 = G5/G6 ∧ PROVE. The
audit-§5 diagram nested G5/G6 UNDER G4 and PROVE under G5/G6; the SPEC explicitly carries the
seq/C6+C7 corrections that supersede it and names them as the GROUND `seq.md` revise-fold. The s6/C4
FORCED demotion of `balanced_component_scan` → `css_balanced_component_scan` is folded into Section 1,
§5, and the route ledger consistently. The 6 addenda each map to a wave with a RED-able falsifier
(verbatim-blob→G1/G2 grep; distinct-output→P3+G3 4-co-gate; single-emitter→G3 5-conjunct;
phantom→G4 substitution falsifier; corpus-in-timer→P2/G2/H1; accel-wiring→G6 census+profile). R16
pinned to full-row `PartialEq` in P3/G3 with hand-rolled-prose-field comparison FORBIDDEN. No broken
sequence, no addenda violation.

---

## Residual-precision sweep (the V7 mandate: drive out the last misleading-to-an-implementer nits)

I hunted specifically for gates where an implementer could read the falsifier as satisfiable WITHOUT
the intended RED, after V6 cleared its two. Two passed my proportionality bar; the rest are sound.

**REVISE-1 — §1 standing-law (line 406) and §7 candidate-intro (line 1192): the column the doc's OWN
close-condition #4 names as the unfalsifiable-gate hazard is restated as "preserved by construction."**
Close-condition #4 (lines 90-93) binds `json_rich_navigation_preserved` and states verbatim: an
"asserted by construction" close "is the unfalsifiable-gate hazard and is REJECT," requiring the
G4.2-conjunct-2 byte-equal diff instead. Yet the §1 preserve-rich-ast standing law (line 406) and the
§7 G4 candidate intro (line 1192) both say JSON's rich-tree navigation "is preserved **by
construction** (`json_rich_navigation_preserved == true`)." The binding G4.2 gate IS concrete and
overrides, so the gate cannot in fact close on assertion — but §1 is the STANDING LAW restated on
every wave CHALLENGE, and an implementer reading line 406 in isolation could read it as license to
skip the byte-equal diff, in direct verbal contradiction with the doc's own #4 REJECT. This is the
one place the SPEC uses the exact phrase it elsewhere flags as the hazard, against the exact column it
flags it for.
EXACT edit — §1 line 406: change "navigation is preserved by construction
(`json_rich_navigation_preserved==true`)." to "navigation is preserved — PROVEN at G4 by the
byte-equal diff of JSON's `value.rs` navigation surface vs its pre-G4 form (not 'by construction',
per close-condition #4), `json_rich_navigation_preserved==true`." Apply the same one-clause fix at
§7 line 1192 ("preserved — PROVEN by the G4.2-conjunct-2 byte-equal diff, not 'by construction'").
Aligns the standing-law and candidate-intro prose with the #4 REJECT and the concrete G4.2 gate.

**REVISE-2 — §9/PROVE exit-gate: `sheets_value_instantiates_g4_trait == true` is a `!= true`
REJECT-predicate conjunct with NO named RED operation, unlike its seven sibling PROVE conjuncts.**
The PROVE exit gate (lines 1454-1479) names an inline grep/count for every conjunct EXCEPT
`sheets_value_instantiates_g4_trait` (line 1476) and `import_closure_relaxation_is_data` (the latter
co-falsified by the named arm-census, ACCEPT). The trait-instantiation conjunct is listed in the
consumer as `sheets_value_instantiates_g4_trait != true` (line 1514) and its telemetry descriptor
(line 1504) merely restates its meaning ("the Sheets value type instantiates the R-D
Cursor/DocumentView seam") — no RED operation. This is exactly the V6 `simd_admission_profile_sampled`
sibling-asymmetry, and PROVE's own `sheets_grammar_shape` line already establishes that
trait/shape-instantiation claims must be "PROVEN BY A CONCRETE STRUCTURAL FALSIFIER, NOT asserted by
construction." G4's conjunct-3 supplies the model RED (a compile-or-grep on the `impl … for
<value-type>` block). An implementer could green this conjunct on assertion.
EXACT edit — §9 exit-gate (line 1476), the `sheets_value_instantiates_g4_trait == true` clause,
append: " (PROVEN by a concrete falsifier, NOT asserted: `rg 'impl\s+(Cursor|DocumentView)\b.*\bfor\b'
grammars/sheets/` is NON-EMPTY AND the crate compiles with the Sheets value type bound to the G4
seam; an absent impl block or a compile failure → `sheets_value_instantiates_g4_trait == false` →
REJECT, isomorphic to the G4.2-conjunct-3 substitution falsifier)." Mirror the inline-falsifier
note at the telemetry descriptor (line 1504). This makes the third trait-impl close on a stated RED,
matching its seven siblings and G4's own model.

Both are precision restatements of gates whose binding MECHANISM is already falsifiable (the G4.2
byte-equal diff exists; the trait-impl is a compile-or-grep in principle); the wording at one
restatement site each could mislead. Neither is a REJECT: no gate is unfalsifiable in mechanism, no
sequence is broken, no addendum is violated.

---

## Verdict summary

Every wave exit-gate names at least one concrete falsifier — a grep count, a mutate-and-observe, a
compile-failure differential, an integer count, a `regen --check` exit code, or a same-run bench
comparison — and the doc repeatedly anticipates the lens's own concern (the §3.3 unfalsifiable
self-glob it patches; the §0.4 closed-enum justification; the G3 conjunct-5 "passes conjuncts 1-4
under a green gate" rationale; the G4 substitution falsifier replacing "by construction"; the V6-folded
G6 `self_time_samples == 0` predicate and H1 ≥1-regular floor). The two residual REVISEs are the SAME
class V6 found (a column flagged as the unfalsifiable hazard yet restated "by construction"; a
REJECT-predicate conjunct with no named RED while its siblings have one) — both materially clarifying
restatement nits, neither touching the falsifiability mechanism. No unfalsifiable gate, no broken
sequence, no addenda violation found.

ACCEPT: 14 (P1, P2, P3, P4, P5, G1, G2, G3, G4, G5/G6, H1, §0.4 schema, §0.3+§0.5+lattice; PROVE
gate mechanism sound modulo REVISE-2)
REVISE: 2 (§1+§7 "preserved by construction" vs close-cond #4 REJECT; §9/PROVE
`sheets_value_instantiates_g4_trait` bare conjunct lacks a named RED)
REJECT: 0

TALLY accept=14 revise=2 reject=0
