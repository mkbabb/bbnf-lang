# SK-V18 S-P3 CHALLENGE — CH3 ADDENDA-LAW (cycle V9)

Lens: does every wave preserve the 6 addenda + the §6 (a)-(d) escape gate + the >SOTA gate +
dav1d/aarch64-only + preserve-rich-ast? Does any wave admit a courier / fork / phantom /
orphan-kernel? Is any addendum-gate unfalsifiable, broken-sequenced, or addenda-violating?
Reviewer: CH3 adversarial (V9, the residual-precision drive toward 2-consecutive-clean). V8 was
revise=0/reject=0 (a clean cycle confirming the V7 C8/C9 folds propagated); a SECOND consecutive
clean reaches the fixed point. This cycle re-grounds the V7/V8 folds INDEPENDENTLY on disk AND
fans an adversarial sweep across the seams prior cycles did NOT examine — the parallel-window
(PROVE ∥ G6) baseline collisions, the closed-enum discipline across ALL enum columns (not just
`sheets_grammar_shape`), and the cross-grammar md5 set threading. Read: `SPEC.md` (Sections 0–11,
full) against `research/p2/SYNTHESIS-RESEARCH.md` §3 + the addenda in
`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1/§5/§6, and the V6/V7/V8 CH3 dispositions.

## Grounding pass (the V7/V8 folds + the addenda witnesses are SPEC-true, re-grepped this cycle)

- **V7 C8 (GENERATED_RS suffix-scope) — DISCHARGED, re-confirmed.** `grep -nE '\{CSS_,\s*_RS|CSS_,_RS'
  SPEC.md` → ZERO hits. All six forbidden-set restatements (§0.1#8 L137, §2 rerun L474, §3.4 L711/L726,
  §3.6 L778, G1.1 L828) carry `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}`; the G2.1
  L941 subset `{GENERATED_RS, CSS_GENERATED_RS}` is correctly scoped (G2 only retires the CSS courier;
  the EventGrammar arm is a G3/Sheets concern, not a G2 one — not an inconsistency). All three `JsonSink`
  mentions (L139, L474, L723) state it is NOT in the scoped set and would not fire. No self-contradiction.
- **V7 C9 (sheets_grammar_shape closed enum) — DISCHARGED, re-confirmed.** §0.4 L253 declares the union
  closed enum `{pratt-operator|flat-stream|tree|courier|hollow}` matching the PROVE structural falsifier
  (L1461-1468) and telemetry (L1503). The REJECT predicate keys on `!= pratt-operator`; the failing
  values its own falsifier names (`flat-stream`/`tree`) are now in-domain.
- The eight `runtime_generator.rs const … _RS` consts, the `source.contains` substring semantics, the
  `RuntimeEmitterKind`/`<G>=EventGrammar`/`RuntimeTarget`-derive/`import_closure` witnesses carry
  forward from V6/V7/V8 unchanged on disk; re-confirmed at the SPEC-cited lines.

## Claims

### C1 [ACCEPT] — all 6 addenda fire as REJECT triggers on every wave they govern (re-confirmed)
Section 1 binds the 6 addenda as standing law; each maps to a §0.4 telemetry column the `gate-json`
consumer REJECTs on AND a per-wave exit conjunct: 1 → `verbatim_blob_present==false` (G1.2.4 / G2.2.1 /
PROVE `sheets_verbatim_blob_present`); 2 → the 4-co-gate {md5-distinct ∧ branch==0 ∧ type==0 ∧
rows_collapsed} (P3-structural / G3-arm / PROVE-re-assert-all-4); 3 → `emitter_fork_present==false ∧
emit_shape_source==lowered_program` (G3.2.1/.5); 4 → `phantom_generic_resolved==deleted ∧
json_rich_navigation_preserved==true` (G4.2.1/.2); 5 → `corpus_in_timer==true ∧
materialization_framing==lazy-rich-vs-eager-cssom` (G2/G6/H1); 6 → `acceleration_at_admission==admission`
w/ BOTH conjuncts (G6). No addendum decorative; each fires on a disk-true live witness. ACCEPT.

### C2 [ACCEPT] — addendum-2 conjunction unbroken P3→G3→PROVE; relocated seam caught ONLY by the full-row co-gate; cross-grammar md5 set correctly threaded
The full {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed} is split P3-structural (md5-distinct +
rows_collapsed via `RuntimeTarget: PartialEq` full-row over BOTH nested structs) → G3 lands conjuncts
2/3/4 → PROVE re-runs ALL FOUR with a Sheets-SPECIFIC arm-census (`(GoogleSheets|Sheets)\w*\s*=>`,
un-abbreviated) + type-census (`GoogleSheetsParser`). The md5 SET is correctly threaded across waves
this cycle's independent grep confirms: P3 binds CROSS-GRAMMAR `{json,css_l4}` (the post-collapse
single-CSS self-glob being unfalsifiable is correctly disambiguated §3.3), PROVE extends to
`{json,sheets,css_l4}` (L1458-1459), and `generator_grammar_count==3` (json+css+sheets, NEVER
json+7-css+sheets — the P3-collapse overfit pre-blocked, R-A0-2) is consistent at all 5 cite sites. The
relocated seam is caught ONLY by the full-row derive; the hand-rolled prose-field compare is REJECT; the
row-collapse COUNT excludes `output_dir`/`expected_files` from the count-check ONLY, not the seam-check
(i). No window asserts the conjunction complete while a conjunct is unmet. ACCEPT.

### C3 [ACCEPT] — emit_shape_source is the binding defence against the relocated fork; field-sets identical at all 3 sites
The §5-risk-1 relocated seam is closed by the 5th G3 conjunct `emit_shape_source==lowered_program` (the
`render(program)` grep for ANY read of
`target.profile|target.emitter|target.output_labels|target.profile_contract|contract.emitter` == 0). The
field-set is IDENTICAL between the Section-1#3 standing seam-scan (L342), the §2.1 standing relocated-seam
scan (L499-501), and the G3-exit conjunct-5 grep (L1117-1118), so the standing scan is provably as strong
as G3's own. Candidate B (`ProjectionSpec`) is ABSORBED into A's per-`BackendShape` renderers, not
retained as a `target.profile`-selected value; the §11 G3 ledger pre-blocks the `target.profile`-selected
route. No fork (visible or relocated) admitted. ACCEPT.

### C4 [ACCEPT] — phantom <G> resolved by DELETE with a test-excluding falsifier; K-axis preserved; enum domain consistent
G4.2.1 greps `EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData` over `runtime/src/`
test-EXCLUDED (the `_proof_compiles::<JsonEventGrammar>` is test-only, must NOT false-green; named, and
§11 pre-blocks the route). The `K=Kind` axis is PRESERVED (preserve-rich-ast). DELETE-default grounded in
the EMPTY non-test instantiation census (re-confirmed on disk). The `phantom_generic_resolved` enum
`{deleted|instantiated|present}` is CLOSED with a well-defined REJECT predicate (`!= deleted`): unlike the
V7-C9 `sheets_grammar_shape` defect, here every wave-falsifier value is IN-domain (the two failing states
`instantiated`/`present` are named in the schema enum), so no schema-vs-falsifier vocabulary mismatch
exists. The witness-emission coupling is sound and ordered: Sheets emits at PROVE which entry-gates on G4
closed, G4 DELETES the axis, so post-G4 no `EventGrammar` literal can be emitted (structural impossibility,
not assertion), P4 `FORBIDDEN ⊇ {EventGrammar,*EventGrammar}` defence-in-depth. ACCEPT.

### C5 [ACCEPT] — no orphan kernel at the G6 admission gate itself; the two-value enum is consistent and defended; dav1d/aarch64-only intact
Addendum-6's no-orphan law is bound at §1 (dav1d: scalar reference FIRST + checkasm differential a
CORRECTNESS gate ONLY, speedup CLAIMS deferred to H1), the G2↔G6 one-seam coupling
(`css_balanced_component_scan` IS the G6 retarget call site), and G6 task-3 ("land consumer + entry in ONE
commit"). `acceleration_at_admission==admission` requires BOTH the generated-`generated.rs` caller census
(NOT `#[cfg(test)]`) AND `simd_admission_profile_sampled==true` (runtime-reachability via the
`css_canon_bench` samply sample) — a census hit in dead/unreachable code == `dead`. The enum is the SAME
two-value `{admission|dead}` at §0.4 (L252) and §8 (L1381), §0.4 explicitly defending the closure against
a third state. dav1d/aarch64-only bound at addendum-6, P1 (x86 DELETED crate-wide, build-soundness decouple
same-commit), G6 task-4 (aarch64 NEON/dotprod ONLY), §11. The orphan-JSON-classifier route is pre-blocked
(G5 authors NOTHING). The G6-internal admission gate admits no orphan. ACCEPT. (The CROSS-WAVE orphan
hazard — PROVE's parallel regen silently stripping G6's wired call site — is C8, REVISE; it does not break
the G6-internal gate, which is sound.)

### C6 [ACCEPT] — Sheets is a genuine negative-control; the shim verdict is N not a paper-close; sheets_emission_path closed
PROVE's `sheets_grammar_shape==pratt-operator` is proven by a CONCRETE structural falsifier (≥7 chained
per-level descent fns `comparison→…→primary` + the cyclic `paren_expr→expression` back-edge,
machine-counted) — NOT "by construction"; a `flat-stream`/`tree` emission has <7 chained level fns and
FAILS. `sheets_value_instantiates_g4_trait` is proven by a concrete falsifier (`rg 'impl (Cursor|
DocumentView) … for' grammars/sheets/` NON-EMPTY AND the crate compiles bound), isomorphic to the
G4.2-conjunct-3 substitution falsifier — NOT asserted. The `sheets_emission_path {generator-only|shim}`
domain is closed; `shim==N` (the §0.3 negative-control verdict), surfaced HONESTLY, NEVER `S`, NEVER
paper-closed, NEVER a hand-written `_GENERATED_RS` block. The `import_closure` relaxation is DATA
(present-iff-grammar-has-imports from facts), NOT a `match grammar` arm. No Sheets courier/fork
admissible. ACCEPT.

### C7 [ACCEPT] — preserve-rich-ast bound on all three axes; the >SOTA gate is load-robust, captured-at-G2-entry, falsifiable; recognition-only does not discharge
preserve-rich-ast bound on the TRAIT axis (G4.2.2 `json_rich_navigation_preserved` byte-equal diff vs
pre-G4 + G4.2.3 `shared_trait_non_collapsible` substitution falsifier — a degenerate-equal CSS impl
COMPILES under substitution ⇒ REJECT, the bare ≥2 count necessary-NOT-sufficient), the BENCH axis
(`track1_rich` LAZY-rich, re-derived from spans, nothing eager to the arena, `lazy-rich-vs-eager-cssom`),
and the DISPATCH-TRIPLE axis (G1 `g1_dispatch_triple_not_lcd_collapsed`). The CSS >SOTA gate is the
SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no regression vs the pre-G2 baseline CAPTURED AT G2 ENTRY in
ONE quiet run (pre-G2 tree-checkout + post-G2 build timed together so host depression cancels); the
falsifier FIRES at G2 exit, G3/H1 re-confirm DIRECTIONALLY against the G2-RECORDED figure, never
re-measuring the vanished pre-G2 code (close-cond #6) — consistent across all 6 sites. Keying on an
un-re-locked absolute is the unfalsifiable hazard the same-run comparison REPLACES. Oracle parity (9-field
cssparser / 51-row JSON) gates BEFORE any speed admission; recognition-only `track1_full_parse` (ZERO
typed-field materialization vs `track1_rich`'s 9 fields, NOT preserve-rich-ast) does NOT by itself
discharge the typed close (§0.2 / §0.3 / L182 / L229-231). ACCEPT.

### C8 [REVISE] — PROVE's `json_css_preservation_held` pins CSS preservation to byte-equality "vs the G3-closed shipped file," but G6 (PARALLEL to PROVE, off the same G3) legitimately WIRES `runtime_simd::find_…` INTO that exact file; under the G6-before-PROVE landing order the check either false-REDs PROVE for G6's work OR silently strips G6's call site and false-GREENs — an addendum-6 orphan-kernel admitted at H1, never surfaced before
PROVE telemetry `json_css_preservation_held` (L1511) defines CSS preservation as "CSS track1_rich/lcss
preserved by byte-equivalence of `grammars/css_l4/generated.rs` vs **the G3-closed shipped file**
(`dirty_generated_state == clean`)." The intent is correct and proportionate: PROVE re-touches the shared
`render(program)` body (adding Sheets), so the falsifier for "PROVE must not perturb the JSON/CSS hot
leaves" is a byte-diff. BUT the baseline is pinned UNCONDITIONALLY to the G3-closed file, and that file is
NOT stable across the parallel window:

- **G6 (§8) MUTATES `grammars/css_l4/generated.rs`.** Its admission gate REQUIRES the
  `runtime_simd::find_…` call site to live in `grammars/*/generated.rs` (L1346, L1382: "the
  `runtime_simd::find_…` call site in `grammars/*/generated.rs`; empty == FAIL"; G6 task-3 "swap the
  generated inner-skip call site"). So a CLOSED G6 leaves `css_l4/generated.rs` byte-DIFFERENT from the
  G3-closed file BY DESIGN — that diff IS `acceleration_at_admission==admission`.
- **G6 and PROVE are PARALLEL** — both hang off G3, both gate H1, with NO ordering between them (L544-546
  diagram; L1407 "G5/G6 does NOT block PROVE … PARALLEL"; L1427 "PARALLEL to G5/G6"). Two landing orders
  are admissible.
- **Order PROVE-before-G6:** the working file is still the G3-closed file; byte-equality-vs-G3-closed is
  the correct, well-defined falsifier. SOUND.
- **Order G6-before-PROVE:** the working `css_l4/generated.rs` now carries G6's wire. PROVE re-running the
  shared emitter and re-emitting CSS then byte-diffs against the G3-closed file and shows G6's NEON
  call-site as a "difference" PROVE did not make → `json_css_preservation_held` false-REDs PROVE for G6's
  legitimate work; OR — the more dangerous branch — PROVE's regen of `css_l4/generated.rs` from the shared
  `render(program)` body (which has no knowledge of G6's post-hoc call-site swap) OVERWRITES the file
  WITHOUT G6's wire, byte-equality-vs-G3-closed then PASSES (false-GREEN), and G6's
  `acceleration_at_admission` silently demotes to `dead`. This is exactly the addendum-6 orphan-kernel the
  campaign forbids (a wired kernel reverted to dead, the SK-V5 failure), admitted under a GREEN PROVE gate
  and surfaced only at the H1 `regen_check_clean`/`acceleration_at_admission` re-confirmation (one wave too
  late, no longer attributable to PROVE).

This is NOT an addenda violation (the addendum intent — CSS unperturbed — is sound) and NOT a broken DAG
(the entry gates are correct). It is a gate-precision BASELINE collision in the parallel window: the
preservation baseline must track G6's legitimate mutation rather than the frozen G3-closed file. Every
OTHER byte-equality baseline in the SPEC pins to an UPSTREAM closed wave (G3 conjunct-6 "vs G1/G2-closed"
both upstream; G4.2.2 "vs pre-G4" pre-PROVE; the >SOTA "vs pre-G2" captured-at-G2-entry) — PROVE's
`json_css_preservation_held` is the SOLE byte-equality check whose baseline collides with a PARALLEL
sibling that mutates the compared file. All three prior S-P3 cycles missed it: V8 C7 (L128-130) endorsed
"BYTE-EQUALITY of `grammars/css_l4/generated.rs` vs the G3-closed file" as "consistent and falsifiable"
without examining the G6-parallel collision. NAMES the section + the EXACT one-line edit:

- §9 PROVE telemetry, L1511 (`json_css_preservation_held`): replace "CSS track1_rich/lcss preserved by
  byte-equivalence of `grammars/css_l4/generated.rs` vs the G3-closed shipped file (`dirty_generated_state
  == clean`)" with "CSS track1_rich/lcss preserved by byte-equivalence of the CSS grammar-derived BODY in
  `grammars/css_l4/generated.rs` vs its baseline (the G3-closed file, OR — if G6 has already landed — the
  G6-wired file, so PROVE's shared-`render(program)` re-emit MUST preserve any `runtime_simd::find_…` call
  site G6 wired; stripping a landed G6 call site is an addendum-6 orphan-kernel REJECT, not a clean diff),
  `dirty_generated_state == clean`". (Equivalently, add to the §11 PROVE ledger row: "a PROVE regen that
  reverts a landed G6 `runtime_simd` call site (false-green byte-equality vs the stale G3-closed
  baseline)".) REVISE (parallel-window baseline precision on an addendum-6-bearing preservation column;
  not unfalsifiable, not a DAG break, not candidate-selection — but materially misleading to the PROVE
  implementer in the G6-before-PROVE order).

## Net

Every wave preserves the 6 addenda, the §6 (a)-(d) escape, the >SOTA gate, dav1d/aarch64-only, and
preserve-rich-ast; the visible-fork / phantom / courier routes all sit in the §11 ledger behind turning-RED
falsifiers, and the addenda witnesses + the V7/V8 C8/C9 folds ground SPEC-true at every cited line
(independently re-grepped this cycle: zero bare `{CSS_,_RS}`, the JsonSink-would-not-fire correction, the
closed `sheets_grammar_shape` union, the threaded cross-grammar md5 set, the closed
`phantom_generic_resolved`/`acceleration_at_admission`/`sheets_emission_path` enums). The
addendum-2 binding sequence (P3 structural-half → G3 arm-half → PROVE re-assert-all-4 with a Sheets-specific
arm/type census) is unbroken; the `emit_shape_source` relocated-fork defence, the test-excluding phantom
falsifier, the two-value admission enum, the concrete `pratt-operator` falsifier, and the same-run >SOTA
falsifier captured-at-G2-entry are all consistent and falsifiable. ONE residual REVISE this cycle, narrow
and proportionate, that V6/V7/V8 did not examine: C8 — PROVE's `json_css_preservation_held` byte-equality
baseline ("vs the G3-closed shipped file") collides with G6's PARALLEL, legitimate mutation of that exact
file (`runtime_simd::find_…` wired into `grammars/css_l4/generated.rs`), so under the G6-before-PROVE
landing order the gate either false-REDs PROVE for G6's work or silently strips G6's wire and false-GREENs
— an addendum-6 orphan-kernel admitted under a green PROVE gate, surfaced only at H1. The fix is a one-line
baseline pin (G3-closed OR the G6-wired file; stripping a landed G6 call site is an orphan-kernel REJECT).
No REJECT: the addenda framework is sound, complete, and falsifiable; no other parallel-window baseline
collides; no churn warranted beyond the single C8 edit. This cycle is revise=1 (following V8's clean
revise=0), so the 2-consecutive-clean fixed point is NOT yet reached — the C8 fold + one more clean cycle
closes it.

TALLY accept=7 revise=1 reject=0
