# SK-V18 S-P3 CHALLENGE — CH3 ADDENDA-LAW (cycle V7)

Lens: does every wave preserve the 6 addenda + the §6 (a)-(d) escape gate + the >SOTA gate +
dav1d/aarch64-only + preserve-rich-ast? Does any wave admit a courier / fork / phantom /
orphan-kernel? Is any addendum-gate unfalsifiable, broken-sequenced, or addenda-violating?
Reviewer: CH3 adversarial (V7, the residual-precision drive toward 2-consecutive-clean).
Read: `SPEC.md` (Sections 0–11, full) against `research/p2/SYNTHESIS-RESEARCH.md` §3 + the addenda
in `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1/§5/§6, and the V6 CH3 disposition (C8 fold).
Every load-bearing witness re-grounded on disk this pass.

## Grounding pass (the addenda witnesses are disk-true at the SPEC paths)

- `runtime_generator.rs` carries EIGHT `const … : &str = r#"…"#` consts (disk-confirmed,
  `skinny/crates/codegen/src/runtime_generator.rs`): `JSON_PARSE_ONLY_GENERATED_RS:195`,
  `JSON_PARSE_ONLY_PARSER_RS:550`, `JSON_MOD_RS:572`, `JSON_HOST_RS:594`, `CSS_MOD_RS:598`,
  `CSS_PARSER_RS:612`, `CSS_SINK_RS:665`, `CSS_GENERATED_RS:701`. Of these, exactly TWO are the
  grammar-body couriers the campaign retires (`JSON_PARSE_ONLY_GENERATED_RS` at G1,
  `CSS_GENERATED_RS` at G2); the OTHER SIX are mod/host/parser/sink scaffold consts that survive P4.
- `lock14_baseline.rs:2695 if source.contains(token)` — the forbidden-token match is a plain
  SUBSTRING test (disk-confirmed). This is the V6 C8 binding semantics: a bare `_RS`/`CSS_` token
  substring-matches the six surviving scaffold consts.
- The V6 CH3 C8 fold landed correctly at the AUTHORITATIVE site §3.4 task-2 (line 708:
  `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}`) and the §3.4 exit-gate falsifier
  (lines 719-724: re-inject `SHEETS_GENERATED_RS`; `JsonSink` "NOT in the set and would not fire").
- `RuntimeEmitterKind`/`<G>=EventGrammar`/`RuntimeTarget` derive / `import_closure` witnesses all
  re-confirmed at the SPEC-cited lines (carried from V6; unchanged).

## Claims

### C1 [ACCEPT] — all 6 addenda fire as REJECT triggers on every wave they govern
Section 1 binds the 6 addenda as standing law; each maps to a §0.4 telemetry column the `gate-json`
consumer REJECTs on AND a per-wave exit-gate conjunct: addendum 1 → `verbatim_blob_present==false`
(G1.2.4 / G2.2.1 / PROVE); addendum 2 → the 4-co-gate {md5-distinct ∧ branch==0 ∧ type==0 ∧
rows_collapsed} (split P3-structural / G3-arm / PROVE-re-assert-all-4); addendum 3 →
`emitter_fork_present==false ∧ emit_shape_source==lowered_program` (G3.2.1/.5); addendum 4 →
`phantom_generic_resolved==deleted ∧ json_rich_navigation_preserved==true` (G4.2.1/.2); addendum 5 →
`corpus_in_timer==true ∧ materialization_framing==lazy-rich-vs-eager-cssom` (G2/G6/H1); addendum 6 →
`acceleration_at_admission==admission` w/ BOTH conjuncts (G6). No addendum is decorative. ACCEPT.

### C2 [ACCEPT] — addendum-2 conjunction is bound completely across P3→G3→PROVE; relocated-seam caught only by the full-row co-gate
Section 1#2 states the full {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed}. §3.3 scopes P3 to the
structural half (md5-distinct + rows_collapsed), DEFERS branch/type to G3's exit; G3.2 lands
conjuncts 2/3/4; PROVE re-asserts ALL FOUR for the third grammar. No window where the conjunction is
asserted complete while a conjunct is unmet. The relocated seam (a per-grammar branch moved into a
neutral data table) is caught ONLY by the `RuntimeTarget: PartialEq` full-row co-gate (§3.3(i),
G3.2.4, §2.1) — the FORBIDDEN hand-rolled prose-compare correctly named REJECT; the md5 two-phase
(pre-collapse self-glob RED, post-collapse cross-grammar distinctness) is correctly disambiguated
§3.3. ACCEPT.

### C3 [ACCEPT] — emit_shape_source is the binding defence against the relocated fork (no phantom-fork admitted)
The §5-risk-1 relocated seam is closed by the 5th G3 conjunct `emit_shape_source==lowered_program` —
the `render(program)` body grep for ANY read of
`target.profile|target.emitter|target.output_labels|target.profile_contract|contract.emitter` == 0.
The field-set is IDENTICAL between the Section-1#3 standing seam-scan (line 342) and the G3-exit
conjunct-5 grep (lines 1111-1116), so the standing scan is as strong as G3's own. The SPEC names the
hazard explicitly ("Without this fourth conjunct, the §5-risk-1 relocated seam ... passes all of
conjuncts 1-4 under a green gate"). No fork (visible or relocated) admitted. ACCEPT.

### C4 [ACCEPT] — the phantom <G> is resolved by DELETE with a test-excluding falsifier (no phantom false-greens)
G4.2.1 greps `EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData` over `runtime/src/`
test-EXCLUDED (the standing `_proof_compiles::<JsonEventGrammar>` is test-only and must NOT
false-green — explicitly named). The `K=Kind` axis is PRESERVED (preserve-rich-ast). The
witness-emission coupling is sound: Sheets emits at PROVE, which entry-gates on G4 closed, and G4
DELETES the axis, so post-G4 no `EventGrammar` literal can be emitted by construction; the P4
`FORBIDDEN ⊇ {EventGrammar,*EventGrammar}` stands as defence-in-depth. DELETE-default grounded in the
EMPTY non-test instantiation census. No orphan phantom. ACCEPT.

### C5 [ACCEPT] — no orphan kernel: every primitive lands WITH its hot-path consumer; G5 authors nothing
Addendum-6's no-orphan law is bound at §1 (dav1d discipline), the G2↔G6 one-seam coupling
(`css_balanced_component_scan` IS the G6 retarget call site), and G6 task-3 ("land consumer + entry
in ONE commit"). `acceleration_at_admission==admission` requires BOTH the generated-`generated.rs`
caller census AND `simd_admission_profile_sampled==true` (runtime-reachability) — a census hit in
dead code with no profile attribution == `dead`. The enum is correctly the SAME two-value
`{admission|dead}` domain at §0.4 (line 251) and §8 (line 1377), with §0.4 explicitly defending the
two-value closure against a third state. G5 retires/neutralizes the zero-sampled `json/scan.rs` and
authors NOTHING for JSON; the orphan-JSON-classifier route is pre-blocked (§11). dav1d/aarch64-only
bound at addendum-6, P1, §11. No orphan kernel admitted. ACCEPT.

### C6 [ACCEPT] — Sheets is a genuine negative-control: no courier admitted, the shim verdict is N not a paper-close
PROVE's `sheets_grammar_shape==pratt-operator` is proven by a CONCRETE structural falsifier (≥7
chained per-level descent fns + the cyclic `paren_expr→expression` back-edge, machine-counted) — NOT
"by construction". The binding fallback is the §0.3 `N` verdict, surfaced HONESTLY, NEVER `S`, NEVER
paper-closed, NEVER a hand-written `_GENERATED_RS` block. The `import_closure` relaxation is DATA
(present-iff-grammar-has-imports from facts), NOT a `match grammar` arm. No Sheets courier/fork is
admissible. (The enum-vocabulary precision seam between the §0.4 schema domain and the falsifier's
named failing values is C9, REVISE — it does not break the negative-control teeth, which key on
`!= pratt-operator`.) ACCEPT.

### C7 [ACCEPT] — preserve-rich-ast bound on all three axes; the >SOTA gate is load-robust and falsifiable
preserve-rich-ast bound on: the trait axis (G4.2.2 `json_rich_navigation_preserved` byte-equal diff +
G4.2.3 `shared_trait_non_collapsible` substitution falsifier — a degenerate-equal CSS impl COMPILES
under substitution ⇒ REJECT); the bench axis (`track1_rich` LAZY-rich, re-derived from spans); the
dispatch-triple axis (G1 do-NOT-LCD-collapse value/object/array). The CSS >SOTA gate is the SAME-RUN
`track1_rich/lightningcss > 1.0×` ∧ no regression vs the G2-entry-captured pre-G2 baseline (one quiet
plane so host depression cancels) — keying on an un-re-locked absolute is explicitly the unfalsifiable
hazard the same-run comparison REPLACES; oracle parity gates BEFORE speed; recognition-only
`track1_full_parse` does NOT discharge the typed close. ACCEPT.

### C8 [REVISE] — the V6 C8 `GENERATED_RS` fold landed at §3.4 ONLY; four downstream restatements of the forbidden-set still carry the bare `{CSS_,_RS}` form V6 proved un-closeable, and §0.1#8 carries a flat self-contradiction of §3.4 on the `JsonSink` falsifier
The V6 CH3 C8 REVISE corrected §3.4 task-2 (line 708) and the §3.4 exit-gate falsifier (lines
719-724) to the suffix-scoped `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` — the
ONLY set that catches the two grammar-body couriers under `source.contains` WITHOUT false-REDing the
six surviving scaffold consts (`*_MOD_RS`/`*_HOST_RS`/`*_PARSER_RS`/`*_SINK_RS`, disk-confirmed
present + untouched by any wave before P4 closes). That fold did NOT propagate to the four other
sites that state the same forbidden-set, which the V6 verdict itself flagged "should carry the same
`GENERATED_RS` scoping for consistency":

- **§0.1#8, line 137** — `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}` (bare).
- **§0.1#8, line 139** — "the re-inject-a-`JsonSink`-token falsifier turns the gate RED" — this
  asserts `JsonSink` DOES fire. §3.4 line 720 states the EXACT OPPOSITE: `JsonSink` is "NOT in the
  set and would not fire." A flat self-contradiction on the binding falsifier mechanism, in the
  close-condition (the SPEC's most authoritative statement of what SK-V18 must satisfy).
- **§2 rerun-ceiling table, line 471** — `FORBIDDEN ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}` (bare;
  the `JsonSink` exclusion in this cell is correct, but the token-set is not).
- **§3.6 telemetry column, line 775** — `forbidden_generic_tokens_extended (P4; true —
  FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar})` (bare).
- **§5 G2.1 entry gate, line 938** — `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_, _RS}` (bare).

Consequence under the addenda-law lens: addendum-1's enforcement vehicle is the P4 Lock-14 gate (the
load-bearing predecessor of every emitter wave). An implementer reading the CLOSE CONDITION (§0.1#8)
— not §3.4 — implements the bare `{CSS_,_RS}` set + the `JsonSink` re-inject, hits a P4 gate that
RED-fails on six scaffold consts NO wave retires before P4, and is then directly contradicted by
§3.4. This is the IDENTICAL un-closeable-gate hazard V6 C8 named, surviving in four sites because the
fold was partial. Not an addendum violation (addendum-1 correctly wants verbatim `_RS` blobs gone)
and not a broken DAG — a gate-precision mislead on addendum-1's predecessor. NAMES the section + the
EXACT one-line edits:
- §0.1#8 line 137: replace `{CSS_,_RS,EventGrammar,*EventGrammar}` with
  `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}`.
- §0.1#8 line 139: replace "the re-inject-a-`JsonSink`-token falsifier turns the gate RED" with
  "the re-inject-a-`SHEETS_GENERATED_RS`-token falsifier turns the gate RED (a bare `JsonSink` is NOT
  in the scoped set and would not fire — §3.4)".
- §2 line 471: replace `FORBIDDEN ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}` with
  `FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` and "a `_RS`-bearing or
  `CSS_` token" with "a `GENERATED_RS`-bearing courier token e.g. `SHEETS_GENERATED_RS`".
- §3.6 line 775: replace `{CSS_,_RS,EventGrammar,*EventGrammar}` with
  `{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}`.
- §5 G2.1 line 938: replace `{CSS_, _RS}` with `{GENERATED_RS, CSS_GENERATED_RS}`.
The line-137/139 close-condition edit is the load-bearing one (§3.4 is already correct). REVISE
(gate-precision on addendum-1's predecessor + one flat self-contradiction; not candidate-selection,
not sequencing-DAG, not an addenda violation).

### C9 [REVISE] — `sheets_grammar_shape` §0.4 schema declares closed enum `{pratt-operator|courier|hollow}`, but the PROVE falsifier and telemetry name the failing shapes `{flat-stream|tree}`; the closed-enum REJECT vocabularies are disjoint
§0.4 (line 252) declares `sheets_grammar_shape (enum {pratt-operator|courier|hollow})` — a CLOSED
domain, exactly the disciplined closure the SPEC applies to `materialization_framing` (line 255,
"the enum is CLOSED to these two values so the gate can REJECT any other"). But the PROVE structural
falsifier (lines 1462-1463) and the PROVE telemetry column (line 1499, "pratt-operator — NOT
flat-stream/tree") name the FAILING shapes as `flat-stream` (the R-E-3 flattened-tower predicate) and
`tree` (single recursive value match) — values NOT in the schema's closed `{courier|hollow}` failing
set. The two failing-value vocabularies are disjoint. §0.4's own validator contract REJECTs a run if
any column is "mis-typed"; a PROVE wave emitting the value its OWN falsifier names (`flat-stream`)
emits an out-of-schema-domain string, so the column trips schema-validation for the RIGHT reason (it
genuinely is not `pratt-operator`) under the WRONG mechanism (out-of-domain rather than the named
structural falsifier). The PROVE REJECT predicate keys on `!= pratt-operator` (line 1511), so the
gate stays falsifiable — this is NOT a REJECT — but the gate-json closed-enum validator author cannot
tell whether to admit `flat-stream`/`tree` as valid-but-failing values or treat them as mis-typed,
which is a material ambiguity on a PROVE close-gate. NAMES the section + the EXACT one-line edit:
§0.4 line 252 — replace `(enum {pratt-operator|courier|hollow}; addendum 2 / R-E — MUST be
pratt-operator at PROVE; a courier/hollow shape = relabeled, REJECT)` with `(enum
{pratt-operator|flat-stream|tree|courier|hollow}; addendum 2 / R-E — MUST be pratt-operator at PROVE;
any other shape (flat-stream/tree = <7 chained level fns per the §9 falsifier; courier/hollow =
relabeled) REJECTs)`. REVISE (closed-enum precision on a PROVE close-gate; not unfalsifiable — the
gate keys on `!= pratt-operator`).

## Net

Every wave preserves the 6 addenda, the §6 (a)-(d) escape, the >SOTA gate, dav1d/aarch64-only, and
preserve-rich-ast; no wave admits a courier/fork/phantom/orphan-kernel by design — every such route
sits in the §11 ledger behind a turning-RED falsifier, and the addenda witnesses ground disk-true.
The addenda-binding sequence (P3 structural-half → G3 arm-half → PROVE re-assert-all-4) is unbroken;
the `acceleration_at_admission` two-value enum is consistent and defended. TWO residual precision
REVISEs, both narrow: C8 — the V6 `GENERATED_RS` fold landed at §3.4 ONLY, leaving four downstream
restatements (§0.1#8, the rerun table, §3.6, §5 G2.1) carrying the bare `{CSS_,_RS}` form V6 proved
un-closeable on the six surviving scaffold consts, with §0.1#8 line 139 a flat self-contradiction of
§3.4's `JsonSink`-would-not-fire statement, on addendum-1's load-bearing P4 predecessor; C9 — the
`sheets_grammar_shape` §0.4 closed-enum names different failing values (`courier`/`hollow`) than its
own PROVE falsifier (`flat-stream`/`tree`), a closed-enum vocabulary mismatch on a PROVE close-gate.
No REJECT: the addenda framework is sound, complete, and falsifiable; the sequence is intact; both
fixes are single token/enum-scoping edits.

TALLY accept=7 revise=2 reject=0
