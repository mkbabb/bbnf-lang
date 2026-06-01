# SK-V18 S-P3 CHALLENGE — V6 / CH1 — GATE-FALSIFIABILITY lens

Lens: GATE-FALSIFIABILITY. Does every wave exit-gate (and the close/telemetry claims they
discharge) name a CONCRETE falsifier — a grep / test / bench / count that turns RED — rather than a
prose assertion? Flag any unfalsifiable gate, broken sequence, or addenda violation.

Reviewed: `restart/skinny/tranches/sk-v18/SPEC.md` (1643 lines, full) against
`research/p2/SYNTHESIS-RESEARCH.md §3` (the per-wave entry/exit sequencing) and
`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1/§2/§5` (the 6 addenda + R16 + sequencing).

Disk-truth spot-check (the gates' pre-state RED witnesses are real, not phantom):
- P1 `find …/x86_64 …/ext/x86 -type f` = **28** (SPEC "today 28"; matches).
- P3 `md5 …/css_l4_*/generated.rs | uniq -c` = **7× b654562c…** (matches the SPEC witness exactly).
- P5 `grep -c parse_w11_1_number json/generated.rs` = **7** (matches "today 7").
- `regen.rs:5` = `#[derive(Clone, Copy, Debug)]`, NO `PartialEq` (matches the R16 +1-line claim).
- H1 lazy-rich citation `css_l4_declaration_values/generated.rs:297-304` is a real docstring proving
  the `lazy-rich-vs-eager-cssom` framing (re-derives from `(source,offset)` spans, "writing nothing
  to the payload arena").
- G6 census glob `grammars/*/generated.rs` = 8 files today, a glob that survives the P3 collapse.
- `RuntimeEmitterKind` lives in `runtime_generator.rs`/`grammar_provider.rs`/`lib.rs` (G3's deletion
  target; grep-falsifiable).

Every gate the SPEC names fires against a real on-disk witness. There is no gate keyed on a
nonexistent grep target, which is the first unfalsifiable-gate failure mode — none found.

---

## Enumeration (each exit-gate / telemetry / close claim under the lens)

### P1 (§3.1) — DELETE x86 surface
**ACCEPT.** Exit falsifier is concrete and triple-anchored: `find …/src/x86_64 …/ext/x86 -type f
== 0` (count grep → RED if >0); `grep -riE 'avx|gfni|sve|x86|nasm' bbnf-simd/` collapses to the
neutral floor (a non-neutral hit RED); AND the build-soundness falsifier `cargo build` + `cargo
test --no-run` clean (the 9 `*_scalar` call sites resolve into `src/x86_64/`; deletion without the
same-commit decouple BREAKS the build — a genuine RED, not prose). `x86_tree_deleted == true` is a
consumed telemetry bool. The "deletion list reach-matched to the verify grep" note is itself a
falsifier-protection (a list narrower than the grep ships RED-by-construction).

### P2 (§3.2) — DELETE warm micro-fixture bench
**ACCEPT.** `grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs == 0` (today 48 → RED if
nonzero); `css_canon_bench` present + green; the extracted 9-field oracle still asserts.
`corpus_in_timer == true` consumed. All three are mechanical RED-able checks.

### P3 (§3.3) — collapse 7 css_l4 replicas + RuntimeTarget row-collapse
**ACCEPT.** This is the most carefully-falsified P-gate, and correctly so (it carries R16). The
SPEC explicitly splits the md5 check into a pre-collapse RED witness (7× `b654562c…`) and a
post-collapse GREEN witness, AND flags that the naive post-collapse self-glob `css_l4_*` would be
"a SINGLE file with no possible pair — an unfalsifiable check," replacing it with the
CROSS-GRAMMAR `md5 …/grammars/{json,css_l4}/generated.rs | uniq -c → no byte-identical pair`. That
is exactly the lens's own concern, pre-empted: the SPEC names and patches an unfalsifiable check.
The structural co-gate `runtime_target_rows_collapsed == true` via the full-row `PartialEq` derive
is mechanically RED-able (a relocated per-grammar seam makes two would-be-equal rows compare
UNEQUAL). The §3.3 separation of (i) the full-row seam check vs (ii) the
config-tuple-minus-(output_dir,expected_files) collapse-count is precise. Sound.

### P4 (§3.4) — fix Lock-14 green-by-exclusion gate
**ACCEPT.** The binding falsifier is the **re-inject** test: inject a `_RS`-bearing or `CSS_` token
into `runtime_generator.rs` → `accepts_current_allowlist` turns RED → revert. This is the
canonical "gate must turn RED" proof — the SPEC even corrects the S-P2 §3/audit-§4 phrasing that
said "re-inject a `JsonSink` token," noting a bare `JsonSink` is NOT in the extended set and would
NOT fire (so the falsifier must use a token actually in `{CSS_,_RS,EventGrammar,*EventGrammar}`).
That correction is itself a falsifiability hardening — the audit/S-P2's `JsonSink` falsifier would
have been a SILENT no-fire. `lock14_gate_scans_codegen`/`forbidden_generic_tokens_extended`
consumed. Sound and an improvement over the upstream docs.

### P5 (§3.5) — purge metalang leak
**ACCEPT.** `grep -c parse_w11_1_number == 0` (today 7); no `w[0-9]+`/`sk_v`/corpus tag; AND `regen
--check` clean — the latter being the binding falsifier that a SOURCE fix (not a hand-patch)
landed (a hand-patch diverging from fresh generator output fails `regen --check` → RED).
Concrete. `metalang_leak_present == false` consumed.

### G1 (§4) — JSON projection exit gate
**ACCEPT.** All five conjuncts are RED-able: (1) byte-equivalence diff-control via
`EmittedSource::check_dir` exact `actual != *source` (a test that fails), with the ±5% line delta
correctly demoted to a SOFT tripwire that "does NOT gate"; (2) the `.bbnf`-mutation falsifier (drop
`bool` → `b't'/b'f'` arms vanish, revert) — a real mutate-and-observe; (3) hot-leaf preservation
by grep for `fn parse_object_value_at_direct` with identical inline cfg + `sink.*` call sites; (4)
`verbatim_blob_present == false` by grep for `r#"…"#` bodies == 0 + `JSON_PARSE_ONLY_GENERATED_RS`
deleted; (5) P5 re-assertion on the REGENERATED file. The §6 (a)-(d) primitive gate is
machine-checked, not prose: (b) is the digit-class WIDEN mutation, (d) is the numeric comparison
`g1_leaf_primitive_loc <= g1_leaf_primitive_profiled_leaf_extent` — a god-kernel REJECT that is an
arithmetic test, not a judgement. Every G1 telemetry column has a stated `!= X` REJECT predicate.
Strong.

### G2 (§5) — CSS lowering exit gate
**ACCEPT.** Five conjuncts, each RED-able: (1) `CSS_GENERATED_RS` grep == 0; (2) the per-primitive
arg-mutation falsifier (mutate `stylesheet.bbnf` → emitted delimiter byte-array changes,
`g2_balanced_scan_arg_mutation_fires`); (3) the 9-field cssparser oracle EXACT parity
(gate-before-speed); (4) the EXPLICIT >SOTA-regression gate — and here the SPEC does the key
falsifiability work: it replaces the un-re-lockable absolute-ratio floor (correctly named "the
unfalsifiable hazard") with the SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no same-run regression
vs `track1_rich_over_lcss_ratio_pre_g2` captured AT G2 ENTRY in one quiet run. Two figures in one
plane is a falsifiable comparison where an absolute load-depressed antecedent is not; (5) the
neutrality FORCED-demotion to `css_balanced_component_scan` with the gate REJECTing a neutral name
with zero structurally-compatible non-CSS caller. The (d) god-kernel check is again the arithmetic
`g2_balanced_scan_primitive_loc <= g2_balanced_scan_profiled_leaf_extent`. The
gate-before-speed ordering ("the speed falsifier is admissible ONLY after the parity falsifier
passes") is explicit. Sound.

### G3 (§6) — un-fork emitter, FIVE-conjunct gate
**ACCEPT.** This is the gate where an unfalsifiable "asserted un-fork" would be most tempting, and
the SPEC closes it. Conjuncts 1-3 are grep counts (`RuntimeEmitterKind|CompiledLowering|RequestFacts`
== 0; `match grammar`/`"json"`/`"css"` literal arms == 0; grammar-named emit-type == 0). Conjunct 4
is the STRUCTURAL `runtime_target_rows_collapsed` full-row `PartialEq` (the only check that catches
the relocated seam — the arm-grep is "syntactically incapable" of seeing it). Conjunct 5
`emit_shape_source == lowered_program` is grep-the-`render(program)`-body-for-`target.*`-reads ==
0, and the SPEC states the binding rationale: "Without this fourth conjunct, the §5-risk-1 relocated
seam … passes all of conjuncts 1-4 under a green gate." That is precisely a lens-grade falsifiability
argument — the gate names WHY the cheaper conjuncts are insufficient and supplies the structural
falsifier that is not. Conjuncts 6/7/8 are consumed columns (byte-equivalent `regen --check`, JSON
hot-leaf grep, CSS same-run ratio), explicitly "CONSUMED columns the gate REJECTs on, NOT prose-only
gates." Strong.

### G4 (§7) — shared trait + phantom, THREE-conjunct gate
**ACCEPT.** The lens's headline risk here is close-condition #4's own warning: an "asserted by
construction" close is the unfalsifiable-gate hazard. The SPEC resolves it concretely. (1)
`phantom_generic_resolved == deleted` by test-excluded grep `EventGrammar|AnyGrammar|G:
EventGrammar|_grammar: PhantomData == 0`. (2) `json_rich_navigation_preserved == true` by the
G4.2-conjunct-2 BYTE-EQUAL DIFF (`value.rs` navigation surface diffs EMPTY vs pre-G4 ∧ JSON 51/51) —
NOT "by construction." (3) `shared_trait_non_collapsible == true` by the G4.2-conjunct-3 SUBSTITUTION
falsifier: swap JSON's nav impl for CSS's → the crate FAILS to compile; a degenerate-equal CSS impl
COMPILES → false → REJECT. A compile-failure differential is a maximally concrete RED. The SPEC
explicitly demotes the bare `shared_trait_impl_count >= 2` to "necessary-NOT-sufficient." This is
the model the rest of the doc should be (and largely is) measured against. Sound.

### G5/G6 (§8) — neutral scan retarget
**ACCEPT.** `acceleration_at_admission == admission` requires BOTH conjuncts, each RED-able: (i)
the caller census `rg runtime_simd::find_… grammars/*/generated.rs` NON-EMPTY (NOT `#[cfg(test)]`);
(ii) `simd_admission_profile_sampled == true` — a samply re-sample attributing non-zero self-time,
the addendum-6 runtime-reachability conjunct that catches "a census hit in dead/unreachable code."
The "Falsifiers (each gate RED-able)" block is explicitly enumerated: revert the call-site swap →
census empty → `dead` → RED; mutate the two-fan OR-reduce by one byte → parity FAIL → RED; emit
7-ways → singular-site FALSE → RED; emit a speedup off the checkasm plane → addendum-5 REJECT. This
is the most explicitly-falsified gate in the doc — every column has a named RED operation.
`g6_speedup_median_mbps` is null pre-H1 and "does NOT gate G5/G6" (deferred to the H1 timer), which
correctly avoids gating on an un-re-locked figure. Sound.

### PROVE (§9) — Sheets via generator only
**ACCEPT.** The negative-control gate is the one most at risk of a "non-hollow by construction"
assertion, and the SPEC pre-empts it: `sheets_grammar_shape == pratt-operator` is "NON-HOLLOW PROVEN
BY A CONCRETE STRUCTURAL FALSIFIER, NOT asserted by construction" — machine-checked by COUNTING the
emitted per-level descent fns (`>= 7` chained non-terminal levels each calling the next) AND the
cyclic `paren_expr→expression` back-edge; a flat-stream or tree emission has FEWER than 7 chained
level fns and FAILS the count. A `>= 7` count is an integer test. The arm-census greps are spelled
as literal `rg` patterns (`match … Json =>|CssL4 =>|(GoogleSheets|Sheets)…`). The binding fallback
`sheets_emission_path == shim → outcome N` is correctly NOT a gate-REJECT but the negative-control
verdict surfaced honestly. Sound.

### H1 (§10) — honesty close
**ACCEPT.** `materialization_framing` is a CLOSED enum `{lazy-rich-vs-eager-cssom|undisclosed}` —
the §0.4 note that an open `|...` "accepts any string and is unfalsifiable" is honored, so the gate
can REJECT any other value; `undisclosed` turns the gate RED. The load falsifier is concrete: an
absolute Mbps claim with harness-stamped `host_loadavg >= 1.0` (or no stamp) → RED; the H1 harness
MUST stamp `host_loadavg`. `regen --check` exit ≠ 0 → RED. The speedup-plane mismatch (figure off
the checkasm plane) → addendum-5 REJECT. The outcome split `A` (quiet) vs `S` (directional) vs
REJECT is precise. Sound.

### §0.4 telemetry schema (the gate consumer)
**ACCEPT.** The schema is producer-AND-consumer bound: "a column emitted but never consumed FAILS
the wave." Each of the 13 binding columns has a typed domain + a per-wave MUST-be predicate, and
every supporting column is mapped to a named consuming wave slice. The `acceleration_at_admission`
enum is held to the SAME two-value domain (`admission|dead`) the §0.3 outcome enum and §8/G6 gate
decide on — "a third state would make the gate non-deterministic between the schema and the G6
falsifier." That is a falsifiability-coherence argument across three doc locations. The closed
`materialization_framing` enum is explicitly justified as un-spoofable. Sound.

### §0.3 outcome enum + §0.5 goalset
**ACCEPT.** The `A`/`S`/`C`/`N`/`L` distinctions are each tied to a measurable condition (a
checkasm PASS with no corpus-in-timer figure is `C`, never `A`; a figure under `host_loadavg >=
1.0` is `S`, not `A`; a shim is `N`). §0.5's per-corpus table binds the same-run `> 1.0×` ∧
no-pre-G2-regression gate with "≥1 regular corpus (animate OR bootstrap) crossing mandatory" — a
concrete crossing requirement, not "beats on average." The gate REJECTs "any corpus-average
substituting for per-corpus ratios" and "any single-tuple broadcast." Sound.

### Lattice / sequencing (§2.1, §3 close, GROUND fold)
**ACCEPT (no broken sequence).** Cross-checked the SPEC lattice against SYNTHESIS-RESEARCH §3 and
SYNTHESIS-AUDIT §5. The folds are consistent: G2 dual-gates on G1 ∧ P3 ∧ P4-live; G3 = G1 ∧ G2 ∧
P4-live ∧ P3-row-collapse; G4 = G1 ∧ G2 ∧ G3; G5/G6 = P1 ∧ P3 ∧ G3 (PARALLEL to G4, the seq/C7
correction — NOT under G4); PROVE = G3 ∧ G4 (G4 a DIRECT conjunct, the seq/C6 correction — "PROVE
NEVER admits before G4 closes"); H1 = G5/G6 ∧ PROVE. The audit-§5 diagram nested G5/G6 UNDER G4 and
PROVE under G5/G6; the SPEC explicitly carries the seq/C6+C7 corrections that supersede it and names
them as the GROUND `seq.md` revise-fold. The s6/C4 FORCED demotion of `balanced_component_scan` →
`css_balanced_component_scan` (the dischargers are parse-with-emit descents structurally
incompatible with the CSS byte-SKIP shell) is folded into Section 1, §5, and the route ledger
consistently. No addenda violation: the 6 addenda each map to a wave with a RED-able falsifier
(verbatim-blob→G1/G2 grep; distinct-output→P3+G3 4-co-gate; single-emitter→G3 5-conjunct;
phantom→G4 substitution falsifier; corpus-in-timer→P2/G2/H1; accel-wiring→G6 census+profile). R16
is pinned to the full-row `PartialEq` mechanism in P3/G3 with the hand-rolled-prose-field comparison
explicitly FORBIDDEN as a shallow-compare false-green. No broken sequence, no addenda violation.

---

## Residual-precision sweep (the V6 mandate: drive out the last misleading-to-an-implementer nits)

I hunted specifically for gates where an implementer could read the falsifier as satisfiable
WITHOUT the intended RED — the lens's narrow remaining surface after 5 clean cycles. Two passed
my proportionality bar (would materially mislead an implementer); the rest are sound as written.

**1. REVISE — §8 / G6 exit-gate: `simd_admission_profile_sampled` has no named numeric/threshold
falsifier, unlike its sibling conjuncts.** Section 8's exit gate and telemetry define the
profile-reachability conjunct as "the `runtime_simd` entry appears in the css_canon_bench samply
sample with non-zero self-time." Every OTHER G6 conjunct names a mechanical RED operation (census
grep NON-EMPTY; one-byte OR-reduce mutation → parity FAIL; 7-way emit → singular FALSE). But
"appears … with non-zero self-time" is read by an implementer as "any sample > 0.0%," which a
single stray attribution sample satisfies — i.e. it can GREEN on noise without proving the call
site is actually HOT. The §0.4 schema and §8 both say "non-zero self-time" but neither states the
RED predicate as a comparison the gate evaluates. This is the one G6 conjunct whose falsifier is
phrased as an existence assertion rather than a thresholded test.
EXACT edit — §8 exit-gate, the `simd_admission_profile_sampled == true` clause (line ~1343),
append after "non-zero self-time)": "; the RED predicate is `self_time_samples == 0` (the entry
ABSENT from the sample, or present at exactly zero attribution) → `acceleration_at_admission ==
dead` → REJECT — the conjunct gates on PRESENCE-with-attribution, not a speedup magnitude (the
magnitude is the deferred H1 figure)." This makes the falsifier a stated `== 0` RED operation
isomorphic to the census conjunct, closing the "greens on a noise sample" read.

**2. REVISE — §10 / H1 exit-gate (line ~1553): `css_sota_ratio_held` is restated WITHOUT the "≥1
regular corpus mandatory" crossing requirement that §0.5 / §5 G2 make binding.** §0.5 and the G2
telemetry (`g2_sota_ratio_held`, line ~1026) both state the binding floor as "PASS REQUIRES >= 1
REGULAR corpus (animate OR bootstrap) crossing > 1.0× with no regression … tailwindcss below 1.0× is
an honest residual recorded, NOT tranche-blocking." But the H1 exit-gate (§10 conjunct 5 / the
`css_sota_ratio_held` telemetry line) restates it as "same-run `track1_rich/lightningcss > 1.0×` per
corpus with no same-run regression" — read literally, "per corpus" demands ALL FOUR corpora cross,
which contradicts the G2/§0.5 "≥1 regular, tailwind residual permitted" floor. An implementer
closing H1 could either over-reject (a tailwind miss that G2 admitted as a residual blocks H1) or be
forced to silently re-interpret. The H1 close must restate the SAME falsifier G2 closed on, since H1
"only RE-CONFIRMS the already-closed G2 close-ratio DIRECTIONALLY."
EXACT edit — §10 exit-gate, the `css_sota_ratio_held` clause (line ~1553) and the matching telemetry
line (~1573): change "CSS >SOTA same-run ratio `> 1.0×` with no same-run regression vs the pre-G2
baseline" to "CSS >SOTA same-run ratio `> 1.0×` on ≥1 regular corpus (animate OR bootstrap) with no
same-run regression vs the pre-G2 baseline — the SAME binding floor G2 closed on (§0.5); a tailwind
miss recorded as an honest residual at G2 is re-confirmed as a residual here, NOT re-litigated as an
H1 block." This aligns the H1 re-confirmation with the G2 gate it re-confirms, removing the
all-four-corpora over-read.

Both are precision restatements of an ALREADY-falsifiable gate (the underlying mechanism is sound;
the wording could mislead the implementer at one of the two restatement sites). Neither is a REJECT:
no gate is unfalsifiable in mechanism, no sequence is broken, no addendum is violated.

---

## Verdict summary

Every wave exit-gate names at least one concrete falsifier — a grep count, a mutate-and-observe, a
compile-failure differential, an integer count, a `regen --check` exit code, or a same-run bench
comparison — and the doc repeatedly anticipates the lens's own concern (the §3.3 unfalsifiable
self-glob it patches; the §0.4 closed-enum justification; the G3 conjunct-5 "passes conjuncts 1-4
under a green gate" rationale; the G4 substitution falsifier replacing "by construction"; the P4
`JsonSink` no-fire correction). The two REVISEs are residual restatement-site precision nits, both
materially clarifying for an implementer, neither touching the falsifiability mechanism. No
unfalsifiable gate, no broken sequence, no addenda violation found.

ACCEPT: 14 (P1, P2, P3, P4, P5, G1, G2, G3, G4, G5/G6, PROVE, H1, §0.4 schema, §0.3+§0.5+lattice)
REVISE: 2 (G6 `simd_admission_profile_sampled` threshold; H1 `css_sota_ratio_held` ≥1-regular floor)
REJECT: 0

TALLY accept=14 revise=2 reject=0
