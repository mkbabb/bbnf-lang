# SK-V18 S-P2 CHALLENGE — CH2 ADDENDA-COMPLIANCE (cycle V2)

Lens: does any RECOMMENDED candidate (R-A..R-F, SYNTHESIS §1) violate a binding addendum?
Read: `SYNTHESIS-RESEARCH.md` (primary) + `rA-emitter-unify.md`. One targeted grep:
`select_lowering`/`BackendShape`/`CostFacts.chosen` grounding (`lower/mod.rs:18-26`,
`ir/cost.rs:7`, `passes/decision_csp.rs`). Addenda referenced by number per the prompt's
REJECT-trigger registry (1 courier, 2 N-distinct+collapsed-rows, 3 single emitter,
4 phantom-`<G>`/JSON-rich, 5 timed-symmetry+corpus-in-timer, 6 NEON-at-admission).

---

## R-A "dispatch on BackendShape" — genuinely grammar-DERIVED or a relocated-seam?

### V1 [ACCEPT] — Candidate A's discriminator IS grammar-derived, not a relocated seam (addendum 3 + addendum 2)
SYNTHESIS §1 R-A row + rA §2.A claim the `BackendShape` axis is "grammar-DERIVED by
construction (the shape comes from the cost model over the rule shapes, not from a config
field)". GROUNDED on disk: `lower/mod.rs:18-26` — `select_lowering(cost: &CostFacts)` matches
`cost.chosen` over a 5-variant `BackendShape` enum carrying ZERO grammar tokens; `cost.chosen`
is itself the output of the CSP shape-selection in `passes/decision_csp.rs:68-127` over
candidate shapes ranked by rule cost, NOT a per-grammar literal. This is exactly the
already-Lock-14-clean neutral discriminator the addenda permit. Addendum 3 (single emitter,
no grammar-family fork) is satisfiable on this axis; addendum 2's `generator_grammar_branch_count
== 0` is structurally reachable because the dispatch token is a cost-model enum. ACCEPT.

### V2 [ACCEPT] — B (`ProjectionSpec`) correctly REJECTED as primary on relocated-seam grounds (addendum 2/3)
SYNTHESIS §1 footnote + rA §2.B/§4 reject Candidate B because "a `ProjectionSpec` IS the
relocated-seam shape unless every field is lowering-derived". This is the correct addendum-2
reading: md5-distinctness is necessary-not-sufficient; a neutral data table whose
`output_shape` is `profile`-selected passes the arm-census grep yet is a REJECT. The synthesis
does NOT recommend B; it absorbs only B's roster-as-derived-data idea into A's per-shape
renderers (each renderer declares its roster from the lowered program, retiring
`COMPILED_RUNTIME_FILES`/`REQUEST_FACTS_RUNTIME_FILES`). No violation. ACCEPT.

### V3 [REVISE] — A's exit-gate must FORBID reading output-shape from `target.*`, and the binding rule must reach the gate predicate
rA §4 states the binding rule "the un-forked emitter must NOT select its output-shape from
`target.profile`/`target.emitter`/`target.output_labels`; it must read it from the lowered
program." This is the load-bearing mitigation for the §5-risk-1 relocated seam — but the G3
exit-gate as written in SYNTHESIS §3 (`emitter_fork_present == false` ∧
`generator_grammar_branch_count == 0` ∧ `runtime_target_rows_collapsed == true`) does NOT
encode it: all three pass for a relocated seam riding the neutral per-profile columns
(`profile`/`source_inputs`/`fact_schema`/`output_plane`), exactly the case rA §4 warns "passes
md5-distinctness and the arm census yet is a REJECT". The binding rule lives only in prose.
EDIT (`SYNTHESIS-RESEARCH.md` §3, G3 Exit line ~171-174): add a fourth conjunct —
`emit_shape_source == lowered_program` — falsified by grepping the un-forked `render(program)`
body for any read of `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`
(must be 0); the dispatch argument must trace to `program.policy_summary.backend_shape`
(`sink_only.rs:48`), not a `RuntimeTarget` field. Without this conjunct the §5-risk-1 seam is
admitted under a green gate. REVISE (not REJECT: A is still the lowest-exposure candidate; the
gate is incomplete, not the candidate wrong).

---

## R-B / R-C named primitives — (a)-(b)-(c) gated or a courier relabel?

### V4 [ACCEPT] — `balanced_component_scan` (R-B) is per-primitive (a)-(b)-(c) gated, not a courier (addendum 1)
SYNTHESIS §1 R-B row + §4 first bullet name the primitive, require it be grammar-INVOKED,
take grammar-DERIVED byte-set ARGS, and carry "a per-primitive mutate-falsifier (mutate the
invoking `.bbnf` rule → emitted ARG byte sets change)" plus a scalar/checkasm reference. §4's
closing paragraph binds ALL such primitives to the machine-checked triple: (a)
grammar-INVOKED-by-name, (b) emitted-output-VARIES-under-invoking-rule-mutation, (c)
`verbatim_blob_present == false`, with "failing any of the three is a relabeled hand-written
blob = REJECT." The G2 exit-gate (§3, ~169-170) wires (a)/(b) concretely ("mutate the invoking
`.bbnf` rule → emitted ARG byte sets change") and (c) (`CSS_GENERATED_RS` grep == 0). This is a
genuine gated primitive, not the verbatim `normalize(CSS_GENERATED_RS)` courier of `:91` it
retires. Addendum 1 satisfied. ACCEPT.

### V5 [ACCEPT] — JSON leaf scanners (R-C) stay byte-stable as (a)-(c)-gated primitives, no LCD-unify (addendum 1)
SYNTHESIS §1 R-C row + §4 second bullet keep the 91.5% hot inner kernels (`b'-'|b'0'..=b'9'`
fast-path, `match_tiny_plain_string_direct`) "byte-stable as named primitives invoked by the
`.bbnf` `string`/`number` rules, each carrying its own (a)-(c) machine falsifier — the
structural SKELETON is walk-derived, only the proven-hot leaf kernels are gated primitives."
§5-risk-3 explicitly forbids LCD-unifying the value/object/array dispatch triple. The
skeleton-from-grammar + leaf-as-gated-primitive split is the correct addendum-1 posture (body
grammar-DERIVED, kernel gated, not a courier swap). ACCEPT.

### V6 [REVISE] — R-C/R-B "byte-equivalence" gate over a HAND body silently re-admits the very courier addendum 1 forbids unless the equivalence is to a REGENERATED artifact
SYNTHESIS §3 G1 Exit demands "byte-equivalence of regenerated `generated.rs` against the
`json_templates/` oracle + shipped file BEFORE oracle deletion". The hazard (rA §0, §5-risk-3):
the current JSON body is itself a "fixed-literal courier wrapped in render functions" — a
byte-equivalence gate is trivially satisfiable by routing the SAME literal through the new
walk (the courier survives, relabeled). The synthesis already names the escape
(`verbatim_blob_present == false` ∧ the `.bbnf`-mutation falsifier "drop `bool`, the
`b't'`/`b'f'` arms vanish") but the (b)-falsifier is asserted for the SKELETON only; the
gated-LEAF-kernels (digit fast-path, tiny-string) are exempted from (b) by §4 ("only the
proven-hot leaf kernels"). EDIT (`SYNTHESIS-RESEARCH.md` §4, R-C bullet ~218): state that each
leaf kernel's (b) falsifier is the BYTE-SET / numeric-class mutation (e.g. widen `number`'s
digit class in `.bbnf` → the `b'0'..=b'9'` literal in the emitted kernel widens), so a kernel
that does NOT vary under its own rule's class mutation is a courier even though the skeleton
varies. Without this the leaf kernels are (a)+(c) gated but NOT (b) gated — a partial-gate gap
that admits a relabeled fixed kernel. REVISE.

---

## R-D trait — LCD-flatten risk? (addendum 4)

### V7 [ACCEPT] — Candidate A (thin `Cursor` micro-trait) is the anti-LCD candidate, by construction (addendum 4)
SYNTHESIS §1 R-D row + §5-risk-4 + rA-aligned §2.4 recommend the `Cursor` micro-trait that
"shares the laziness/cursor contract, NEVER navigation, so JSON's rich tree (`get`/`pairs`/typed
`JsonValue`/recursive visitor) is preserved by construction (`json_rich_navigation_preserved ==
true`)". This directly discharges addendum 4's second clause (no LCD-flatten of JSON rich nav):
the trait is deliberately too narrow to force a common value shape. §5-risk-4 even pre-empts the
"too thin" critique with the correct rebuttal ("ANY trait wide enough to satisfy the critic is
wide enough to LCD-flatten JSON"). The candidates that WOULD violate (B tree-shaped `Value`
stack, C `DocumentView`+stream-only) are correctly REJECTED in §1 footnote. ACCEPT.

### V8 [ACCEPT] — phantom `<G>` resolved by DELETE, instantiate-or-delete honored (addendum 4)
SYNTHESIS §1 R-D + preamble (lines 26-30) ground the DELETE default: the `_proof_compiles`
G-instantiation census excluding `_tests.rs` returns EMPTY; the `EventGrammar` impls in
`grammars/json/` + `grammars/sheets_witness/` are "consumed ONLY by the `_tests.rs`
`_proof_compiles` proof — defined, never animated." Addendum 4's first clause forbids
"uninstantiated phantom `<G>` outside `#[cfg(test)]` (instantiate-or-delete)". The census proves
zero non-test production animator, so DELETE is the grounded resolution, not a phantom left
standing. G4 Exit (`phantom_generic_resolved == deleted`) encodes it. ACCEPT.

---

## R-F — wired at admission? (addendum 6)

### V9 [ACCEPT] — R-F retarget reaches the hot path AT admission, gated by a generated-caller census (addendum 6)
SYNTHESIS §1 R-F row + §3 G5/G6 Exit require `acceleration_at_admission == admission` "proven
by the generated-`generated.rs` caller census (`rg runtime_simd::find_… …/grammars/*/generated.rs`
non-empty), NOT a `#[cfg(test)]` caller". Addendum 6 (NEON/ASM primitive must reach the hot path
at admission) is satisfied: the kernel already exists checkasm-gated in `bbnf-simd`, the wave
lands the generated call-site swap WITH its consumer (no orphan kernel), and the census proves a
PRODUCTION (non-test) caller in the shipped `generated.rs`. The seam is co-located with R-B's
named primitive (the G6 retarget call site IS the `balanced_component_scan` invocation),
directly hitting the measured 79.5%/94.1% CSS hot leaf. ACCEPT.

### V10 [REVISE] — addendum 5 (timed-plane-symmetry + corpus-in-timer) is asserted only at H1; R-F's own speedup measurement (§5-risk-6) can be claimed pre-H1 outside the symmetric timer
SYNTHESIS §3 binds `corpus_in_timer == true` at H1 Exit and §5-risk-6 makes the realized R-F
speedup "a MEASUREMENT to confirm post-wire". But the G5/G6 Exit (§3, ~178-183) asserts the
checkasm differential + `neon_significant_skip_matches_scalar` guard over "the REAL
71KB-495KB corpora" WITHOUT requiring that the speedup figure those gates produce be taken
under the addendum-5 timed-plane-symmetry harness (corpus-in-timer, same plane both sides). The
checkasm differential is a CORRECTNESS gate (scalar==vector), not a timing gate — so a G6
speedup number could be reported from a non-symmetric/warm measurement before H1 re-imposes the
discipline. EDIT (`SYNTHESIS-RESEARCH.md` §3, G5/G6 Exit ~178-183): add that any Mbps/speedup
figure emitted by G6 MUST come from the corpus-in-timer symmetric harness (the P2-survivor
cold/real-corpus path), and that absolute figures inherit §5-risk-7's QUIET-recapture caveat;
G6 may report only the checkasm-differential PASS/FAIL pre-H1, deferring any speedup CLAIM to
the H1 symmetric timer. Without this, addendum 5 is enforced one wave too late and a warm/
asymmetric R-F speedup can leak into the record. REVISE.

---

## Cross-cutting

### V11 [ACCEPT] — R-E Sheets is the genuine grammar-DERIVED litmus, not a relabeled courier (addendum 1 + addendum 3)
SYNTHESIS §1 R-E + §2.5 + §5-risk-5 make Sheets emit THROUGH the un-forked G3 generator with a
precedence tower "structurally unlike both JSON and CSS, so it CANNOT be a relabeled courier";
the `Nu8`-tagged-alt family is correctly DEMOTED from the litmus (CSS uses `-> Nu8u8` 295× vs
Sheets' 21× — a SHARED construct the generator must already handle). PROVE Exit forbids a Sheets
`const.*_RS.*r#` blob and requires md5-distinct Sheets `generated.rs` + a distinct
`grammar_name="google_sheets"` row. The binding fallback ("if Sheets cannot emit via the
generator ONLY, generalization is NOT real — do NOT hand-write a `_GENERATED_RS` Sheets block")
is the correct addendum-1/3 posture. ACCEPT.

### V12 [ACCEPT] — R16 `RuntimeTarget: PartialEq` full-row derive is the right structural co-gate for the relocated seam (addendum 2)
SYNTHESIS §2.7 + §5-risk-1 ground the row-collapse on a one-line `RuntimeTarget: PartialEq`
derive that recurses into BOTH nested structs (`frontend_requirements` field #11,
`output_labels` field #12, `regen.rs:17-18`) and "cannot be coupled to a hand-rolled field
list." This is the ONLY check that catches the relocated seam the arm-census grep is
syntactically blind to (addendum 2's `runtime_target_rows_collapsed`). The ordinal-conflation
hazard (`RuntimeTarget` #11/#12 vs `RuntimeProfileContract` #3/#4) is correctly flagged. ACCEPT.

---

## Summary of REVISE items (none rise to REJECT)
- V3: G3 Exit needs a fourth conjunct `emit_shape_source == lowered_program` (grep `target.*`
  reads in `render(program)` == 0); the binding rule currently lives only in prose and the
  three existing conjuncts all pass a per-profile-column relocated seam.
- V6: R-C/R-B leaf-kernel (b) falsifier must be the byte-set/numeric-class mutation, else the
  gated leaves are (a)+(c) but not (b) — admitting a relabeled fixed kernel under the
  byte-equivalence gate.
- V10: addendum-5 timed-plane-symmetry + corpus-in-timer must bind at G6 for any speedup CLAIM,
  not only at H1; G6 may report checkasm PASS/FAIL pre-H1 but must defer Mbps to the symmetric
  timer.

No RECOMMENDED candidate (R-A..R-F) VIOLATES an addendum outright — each recommended pick is the
lowest-exposure option in its class and the rejected alternatives (R-A B/C, R-B C, R-C, R-D B/C,
R-E-3, R-F B/C) are the ones that would violate. The three REVISEs are gate-completeness gaps,
not candidate-selection errors: the candidates are addenda-compliant but two exit-gates (G3, G6)
and one §4 falsifier (R-C leaf) under-specify the predicate that PROVES compliance, leaving a
relocated-seam (V3), a partial-gate courier-leaf (V6), and an asymmetric-timer speedup (V10)
admissible under currently-green gates.

TALLY accept=9 revise=3 reject=0
