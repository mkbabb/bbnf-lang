# SK-V18 S-P3 CHALLENGE — CH3 ADDENDA-LAW (cycle V8)

Lens: does every wave preserve the 6 addenda + the §6 (a)-(d) escape gate + the >SOTA gate +
dav1d/aarch64-only + preserve-rich-ast? Does any wave admit a courier / fork / phantom /
orphan-kernel? Is any addendum-gate unfalsifiable, broken-sequenced, or addenda-violating?
Reviewer: CH3 adversarial (V8, the residual-precision drive toward 2-consecutive-clean — V7 was
revise=2 (C8 GENERATED_RS propagation + C9 sheets_grammar_shape enum); this cycle re-grounds the
addenda witnesses INDEPENDENTLY on disk and re-adjudicates the V7 folds + hunts for any NEW residual).
Read: `SPEC.md` (Sections 0–11, full) against `research/p2/SYNTHESIS-RESEARCH.md` §3 + the addenda in
`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1/§5/§6, and the V6+V7 CH3 dispositions.

## Grounding pass (the addenda witnesses are disk-true at the SPEC paths, re-grepped this cycle)

- `runtime_generator.rs` carries EIGHT `const … : &str = r#"…"#` consts (disk-confirmed via
  `grep -nE 'const [A-Z_]+_RS' skinny/crates/codegen/src/runtime_generator.rs`):
  `JSON_PARSE_ONLY_GENERATED_RS:195`, `JSON_PARSE_ONLY_PARSER_RS:550`, `JSON_MOD_RS:572`,
  `JSON_HOST_RS:594`, `CSS_MOD_RS:598`, `CSS_PARSER_RS:612`, `CSS_SINK_RS:665`, `CSS_GENERATED_RS:701`.
  Exactly TWO are the grammar-body couriers the campaign retires (`JSON_PARSE_ONLY_GENERATED_RS` at G1,
  `CSS_GENERATED_RS` at G2); the OTHER SIX (`*_PARSER_RS`/`*_MOD_RS`/`*_HOST_RS`/`*_SINK_RS`) are scaffold
  consts that survive P4. The `GENERATED_RS` suffix-scope catches exactly the two couriers under the
  substring semantics WITHOUT false-REDing the six survivors — the V6/V7 C8 fold is disk-validated.
- `lock14_baseline.rs:2694 for (class, token) in FORBIDDEN_GENERIC_TOKENS { if source.contains(token)`
  — the forbidden-token match is a plain SUBSTRING test (disk-confirmed; SPEC cites `:2695`, off-by-one
  on the `if` line but the mechanism is exact and immaterial). The P4 anchors ground EXACT:
  `GENERIC_SCAN_ROOTS:2409`, `FORBIDDEN_GENERIC_TOKENS:2420`, `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442`,
  `("…/x86_64","diagnostic-x86"):2463`, `accepts_current_allowlist:2729` — all SPEC §3.4 cites correct.
- `RuntimeEmitterKind`/`<G>=EventGrammar`/`RuntimeTarget`-derive-`Clone,Copy,Debug`-only/`import_closure`
  witnesses re-confirmed at the SPEC-cited lines (carried from V6/V7; unchanged on disk).

## Claims

### C1 [ACCEPT] — all 6 addenda fire as REJECT triggers on every wave they govern (re-confirmed)
Section 1 binds the 6 addenda as standing law; each maps to a §0.4 telemetry column the `gate-json`
consumer REJECTs on AND a per-wave exit-gate conjunct: addendum 1 → `verbatim_blob_present==false`
(G1.2.4 / G2.2.1 / PROVE `sheets_verbatim_blob_present`); addendum 2 → the 4-co-gate {md5-distinct ∧
branch==0 ∧ type==0 ∧ rows_collapsed} (P3-structural / G3-arm / PROVE-re-assert-all-4); addendum 3 →
`emitter_fork_present==false ∧ emit_shape_source==lowered_program` (G3.2.1/.5); addendum 4 →
`phantom_generic_resolved==deleted ∧ json_rich_navigation_preserved==true` (G4.2.1/.2); addendum 5 →
`corpus_in_timer==true ∧ materialization_framing==lazy-rich-vs-eager-cssom` (G2/G6/H1); addendum 6 →
`acceleration_at_admission==admission` with BOTH conjuncts (G6). No addendum is decorative; each fires
on a disk-true live witness. ACCEPT.

### C2 [ACCEPT] — addendum-2 conjunction unbroken across P3→G3→PROVE; relocated seam caught ONLY by the full-row co-gate
Section 1#2 states the full {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed}. §3.3 scopes P3 to the
structural half (md5-distinct + rows_collapsed via `RuntimeTarget: PartialEq` full-row over BOTH nested
structs), DEFERS branch/type to G3's exit. The md5 two-phase is correctly disambiguated (§3.3:
pre-collapse self-glob RED → post-collapse CROSS-GRAMMAR distinctness over `{json,css_l4}`, sheets
joining at PROVE) — no unfalsifiable single-file self-glob. G3.2 lands conjuncts 2/3/4; PROVE re-runs
ALL FOUR for the third grammar with a Sheets-SPECIFIC arm-census (`(GoogleSheets|Sheets)\w*\s*=>`,
un-abbreviated) and type-census (`GoogleSheetsParser`), so a Sheets-introduced branch/type cannot
slip on G3's inherited green. No window asserts the conjunction complete while a conjunct is unmet. The
relocated seam (a per-grammar branch moved into a neutral data-table field, incl. a nested struct) is
caught ONLY by the full-row derive; the hand-rolled prose-field compare is correctly named REJECT
(shallow-compare false-green of EITHER nested struct), and the row-collapse COUNT excludes
`output_dir`/`expected_files` from the count-check ONLY, NOT from the seam-check (i) — the two R16 uses
are explicitly disambiguated §3.3. ACCEPT.

### C3 [ACCEPT] — emit_shape_source is the binding defence against the relocated fork (no phantom-fork admitted)
The §5-risk-1 relocated seam is closed by the 5th G3 conjunct `emit_shape_source==lowered_program` —
the `render(program)` body grep for ANY read of
`target.profile|target.emitter|target.output_labels|target.profile_contract|contract.emitter` == 0. The
field-set is IDENTICAL between the Section-1#3 standing seam-scan (line 342) and the G3-exit conjunct-5
grep (lines 1117-1118) and the §2.1 standing relocated-seam scan (lines 499-501), so the standing scan
is provably as strong as G3's own. The SPEC names the hazard explicitly ("Without this fourth conjunct,
the §5-risk-1 relocated seam riding the neutral per-profile columns passes all of conjuncts 1-4 under a
green gate"). Candidate B (`ProjectionSpec`) is ABSORBED into A's per-`BackendShape` renderers (each
declares its own roster), not retained as a `target.profile`-selected value — the §11 G3 ledger
pre-blocks the `target.profile`-selected `ProjectionSpec` route. No fork (visible or relocated)
admitted. ACCEPT.

### C4 [ACCEPT] — the phantom <G> is resolved by DELETE with a test-excluding falsifier; K-axis preserved
G4.2.1 greps `EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData` over `runtime/src/`
test-EXCLUDED (the standing `_proof_compiles::<JsonEventGrammar>` is test-only and must NOT false-green
— explicitly named, and the §11 G4 ledger pre-blocks the `_proof_compiles` false-green route). The
`K=Kind` axis is PRESERVED (`_kind: PhantomData<fn() -> K>` survives — preserve-rich-ast, named at
G4a/G4.2.1/§11). The DELETE-default is grounded in the EMPTY non-test instantiation census (S-P2 §0
re-confirmed on disk: the `…_witness.rs` impls are DEFINITIONS consumed ONLY by `_tests.rs`). The
witness-emission coupling is sound and ordered: Sheets emits at PROVE, which entry-gates on G4 closed,
G4 DELETES the axis, so post-G4 no `EventGrammar` literal can be emitted (a structural impossibility,
not an assertion), with the P4 `FORBIDDEN ⊇ {EventGrammar,*EventGrammar}` standing as defence-in-depth.
No orphan phantom. ACCEPT.

### C5 [ACCEPT] — no orphan kernel; dav1d discipline intact; the two-value admission enum is consistent
Addendum-6's no-orphan law is bound at §1 (dav1d discipline: scalar reference FIRST + checkasm
differential parity, the differential a CORRECTNESS gate ONLY, speedup CLAIMS deferred to H1), the
G2↔G6 one-seam coupling (`css_balanced_component_scan` IS the G6 retarget call site — no per-grammar
re-emit), and G6 task-3 ("land consumer + entry in ONE commit"). `acceleration_at_admission==admission`
requires BOTH the generated-`generated.rs` caller census (NOT `#[cfg(test)]`) AND
`simd_admission_profile_sampled==true` (runtime-reachability: the `runtime_simd` entry appears in the
`css_canon_bench` samply sample with non-zero self-time) — a census hit in dead/unreachable code with no
profile attribution == `dead`. The enum is the SAME two-value `{admission|dead}` at §0.4 (line 252) and
§8 (line 1381), with §0.4 explicitly defending the two-value closure against a third state. G5
retires/neutralizes the zero-sampled `json/scan.rs` and authors NOTHING for JSON; the orphan-JSON-
classifier route is pre-blocked (§11). dav1d/aarch64-only bound at addendum-6, P1 (x86 DELETED
crate-wide, build-soundness coupling to `checkasm_parity.rs` decouple same-commit), and §11. The
G6 plane-mismatch (`g6_speedup_median_mbps` off the checkasm plane) is an addendum-5 REJECT. No orphan
kernel admitted. ACCEPT.

### C6 [ACCEPT] — Sheets is a genuine negative-control; no courier admitted; the shim verdict is N not a paper-close
PROVE's `sheets_grammar_shape==pratt-operator` is proven by a CONCRETE structural falsifier (≥7 chained
per-level descent fns `comparison→…→primary` + the cyclic `paren_expr→expression` back-edge,
machine-counted) — NOT "by construction"; a `flat-stream`/`tree` emission has <7 chained level fns and
FAILS the count. The V7 C9 fold is disk-validated: §0.4 line 253 now declares the CLOSED enum
`{pratt-operator|flat-stream|tree|courier|hollow}` matching the PROVE falsifier's named failing values,
so the gate-json closed-enum validator has no domain ambiguity. `sheets_value_instantiates_g4_trait` is
proven by a concrete falsifier (`rg 'impl (Cursor|DocumentView) … for' grammars/sheets/` NON-EMPTY AND
the crate compiles bound), isomorphic to the G4.2-conjunct-3 substitution falsifier — NOT asserted. The
binding fallback is the §0.3 `N` verdict, surfaced HONESTLY, never `S`, never paper-closed, never a
hand-written `_GENERATED_RS` block. The `import_closure` relaxation is DATA
(present-iff-grammar-has-imports from facts), NOT a `match grammar` arm. `generator_grammar_count==3`
(json+css+sheets, NOT json+7-css+sheets — the P3-collapse overfit pre-blocked). No Sheets courier/fork
admissible. ACCEPT.

### C7 [ACCEPT] — preserve-rich-ast bound on all three axes; the >SOTA gate is load-robust and falsifiable
preserve-rich-ast bound on: the TRAIT axis (G4.2.2 `json_rich_navigation_preserved` byte-equal diff of
JSON's `value.rs` nav surface vs pre-G4 + G4.2.3 `shared_trait_non_collapsible` substitution falsifier —
a degenerate-equal CSS impl COMPILES under substitution ⇒ REJECT, the bare ≥2 count being
necessary-NOT-sufficient); the BENCH axis (`track1_rich` LAZY-rich, re-derived from spans, nothing eager
to the arena, the honest `lazy-rich-vs-eager-cssom` framing); the DISPATCH-TRIPLE axis (G1
`g1_dispatch_triple_not_lcd_collapsed` — value/object/array stay 3 sink-prefix variants). The CSS >SOTA
gate is the SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no regression vs the pre-G2 baseline captured AT
G2 ENTRY in ONE quiet run (pre-G2 tree-checkout + post-G2 build timed together so host depression cancels);
the falsifier FIRES at G2 exit, G3/H1 re-confirm DIRECTIONALLY against the G2-RECORDED figure (never
re-measuring the vanished pre-G2 code, per close-cond #6) — described consistently across all 6 sites.
Keying on an un-re-locked absolute is explicitly the unfalsifiable hazard the same-run comparison
REPLACES (addendum-5 timed-plane-symmetry). Oracle parity (9-field cssparser / 51-row JSON) gates
BEFORE any speed admission; recognition-only `track1_full_parse` does NOT discharge the typed close.
PROVE preserves the CSS leaf at the one emit-path-touching wave by BYTE-EQUALITY of
`grammars/css_l4/generated.rs` vs the G3-closed file (`dirty_generated_state==clean`), NOT a re-litigated
ratio bench — falsifiable, no paper-close. ACCEPT.

### C8 [ACCEPT] — the V7 C8 GENERATED_RS fold propagated fully; zero residual bare {CSS_,_RS} forms; no JsonSink self-contradiction
The V7 C8 REVISE flagged four downstream restatements of the forbidden-set still carrying the bare
`{CSS_,_RS}` form + one §0.1#8 self-contradiction (line 139 asserted `JsonSink` fires, contradicting
§3.4). This cycle re-greps the WHOLE SPEC: `grep -nE '\{CSS_,\s*_RS|CSS_,_RS,EventGrammar'` returns ZERO
hits; all three `JsonSink` mentions (lines 139, 474, 723) now correctly state it is "NOT in the scoped
set and would not fire"; the `SHEETS_GENERATED_RS` re-inject is consistent at §0.1#8, the §2 rerun table,
§3.4, §3.6, and §5 G2.1. The §0.4-anchored `GENERATED_RS,CSS_GENERATED_RS` form appears at 6 sites
uniformly. The addendum-1 enforcement vehicle (the P4 Lock-14 gate, the load-bearing predecessor of every
emitter wave) now reads identically from the close-condition and from §3.4 — an implementer reading the
CLOSE CONDITION implements the same suffix-scoped set that does NOT false-RED on the six surviving
scaffold consts. The V7 hazard is DISCHARGED. ACCEPT.

### C9 [ACCEPT] — the V7 C9 sheets_grammar_shape closed-enum fold propagated; schema and falsifier vocabularies now agree
The V7 C9 REVISE flagged the §0.4 schema declaring `{pratt-operator|courier|hollow}` while the PROVE
falsifier/telemetry named the failing shapes `{flat-stream|tree}` (disjoint failing-value vocabularies on
a PROVE close-gate). This cycle confirms §0.4 line 253 now declares the union closed enum
`{pratt-operator|flat-stream|tree|courier|hollow}` with the inline gloss "any other shape (flat-stream/tree
= <7 chained level fns per the §9 falsifier; courier/hollow = relabeled) REJECTs", matching the PROVE
structural falsifier (lines 1462-1468) and telemetry (line 1503). The gate-json closed-enum validator can
now adjudicate `flat-stream`/`tree` as in-domain-but-failing rather than mis-typed; the REJECT predicate
keys on `!= pratt-operator`, so the negative-control teeth are intact. The V7 ambiguity is DISCHARGED.
ACCEPT.

## Net

Every wave preserves the 6 addenda, the §6 (a)-(d) escape, the >SOTA gate, dav1d/aarch64-only, and
preserve-rich-ast; no wave admits a courier / fork / phantom / orphan-kernel by design — every such route
sits in the §11 ledger behind a turning-RED falsifier, and the addenda witnesses ground disk-true at every
cited path:line (re-grepped this cycle). The addenda-binding sequence (P3 structural-half → G3 arm-half →
PROVE re-assert-all-4 with a Sheets-specific arm/type census) is unbroken; the `emit_shape_source`
relocated-fork defence, the test-excluding phantom falsifier, the two-value `acceleration_at_admission`
enum, the concrete `pratt-operator` structural falsifier, the same-run >SOTA falsifier captured-at-G2-entry,
and the byte-equality CSS-leaf preservation at PROVE are all consistent and falsifiable. The TWO V7
residual REVISEs are both fully folded with no regressions and no new sites: C8 (GENERATED_RS suffix-scope
across all 6 forbidden-set restatements + the JsonSink-would-not-fire correction) and C9 (the
sheets_grammar_shape closed-enum union) are DISCHARGED and re-adjudicated ACCEPT this cycle. An
independent fresh sweep — producer/consumer column integrity (per-wave self-contained, §0.4 cross-cutting
only), enum-domain consistency across schema-vs-wave-telemetry, every "by construction" phrasing NEGATED
or a true structural-impossibility proof, the import-closure-as-data relaxation, the N-not-paper-close
shim verdict — surfaced ZERO new residual. This is a clean cycle (revise=0, reject=0) following V7's
revise=2; one further clean cycle reaches the 2-consecutive-clean fixed point. No REJECT: the addenda
framework is sound, complete, and falsifiable; the sequence is intact; no churn is warranted on a
1660-line doc whose every gate under this lens is disk-grounded and turning-RED-falsifiable.

TALLY accept=9 revise=0 reject=0
