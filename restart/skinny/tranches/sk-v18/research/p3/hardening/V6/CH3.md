# SK-V18 S-P3 CHALLENGE — CH3 ADDENDA-LAW (cycle V6)

Lens: does every wave preserve the 6 addenda + the §6 (a)-(d) escape gate + the >SOTA gate +
dav1d/aarch64-only + preserve-rich-ast? Does any wave admit a courier / fork / phantom /
orphan-kernel? Is any addendum-gate unfalsifiable, broken-sequenced, or addenda-violating?
Reviewer: CH3 adversarial (V6, the residual-precision drive toward 2-consecutive-clean).
Read: `SPEC.md` (Sections 0–11, full) against `research/p2/SYNTHESIS-RESEARCH.md` §3 + the addenda
in `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1/§5/§6. Every load-bearing witness re-grounded on
disk this pass (`skinny/crates/codegen/src/runtime_generator.rs`, `grammar_provider.rs`,
`xtask/.../regen.rs`, `runtime/src/tape/`, `lock14_baseline.rs`).

## Grounding pass (the addenda witnesses are disk-true)

- `RuntimeTarget` derives `Clone, Copy, Debug` ONLY (`regen.rs:5`); fields counted on disk:
  `emitter` = #9 (`:15`), `frontend_requirements` = #11 (`:17`), `output_labels` = #12 (`:18`).
  SPEC §0.1#3 / G3.3 ordinals + lines EXACT. Both nested structs derive `PartialEq, Eq`
  (`grammar_provider.rs:45`/`:91`) — the R16 +1-line `PartialEq` recipe is grounded and the
  recurse is free. ACCEPT.
- `RuntimeEmitterKind{CompiledLowering, RequestFacts}` at `grammar_provider.rs:40-42`; the
  `profile_contract.emitter` read at `:110`. SPEC §0.1#2 / G3 owner-paths EXACT; the standing
  seam-scan including `contract.emitter` is grounded. ACCEPT.
- `ValueRef<…G: EventGrammar = AnyGrammar>` (`tape/mod.rs:175`), `_grammar: PhantomData` (`:179`);
  `event_grammar.rs` + `event_grammar_tests.rs` + the two `event_grammar_witness.rs` (json,
  sheets_witness). SPEC §0.1#5 / G4 owner-paths EXACT. ACCEPT.
- `import_closure: true` (`grammar_provider.rs:77`), the reject at `:263`. SPEC §9 task-3 EXACT.
- `CSS_GENERATED_RS` courier in `runtime_generator.rs`. SPEC §0.1#1 / G2 EXACT.

## Claims

### C1 [ACCEPT] — all 6 addenda fire as REJECT triggers on every wave they govern
Section 1 binds the 6 addenda as standing law; each maps to a §0.4 telemetry column the
`gate-json` consumer REJECTs on, AND a per-wave exit-gate conjunct: addendum 1 →
`verbatim_blob_present==false` (G1.2.4 / G2.2.1 / PROVE `sheets_verbatim_blob_present`); addendum
2 → the 4-co-gate {md5-distinct ∧ branch==0 ∧ type==0 ∧ rows_collapsed} (split P3-lands-structural /
G3-lands-arm / PROVE-re-asserts-all-4, correctly); addendum 3 → `emitter_fork_present==false ∧
emit_shape_source==lowered_program` (G3.2.1/.5); addendum 4 → `phantom_generic_resolved==deleted ∧
json_rich_navigation_preserved==true` (G4.2.1/.2); addendum 5 → `corpus_in_timer==true ∧
materialization_framing==lazy-rich-vs-eager-cssom` (G2/G6/H1); addendum 6 →
`acceleration_at_admission==admission` w/ BOTH conjuncts (G6). No addendum is decorative; each
fires on a real live surface. ACCEPT.

### C2 [ACCEPT] — addendum-2 conjunction is bound completely (not partially) across P3→G3→PROVE
Section 1#2 states the full {md5 ∧ branch==0 ∧ type==0 ∧ rows_collapsed}. §3.3 correctly scopes P3
to the structural half (md5-distinct + rows_collapsed) and DEFERS branch/type to G3's exit ("G3's
exit, not P3's; P3 lands the structural-collapse half so the G3 un-fork can satisfy the whole").
G3.2 lands conjuncts 2/3/4; PROVE re-asserts ALL FOUR for the third grammar
(`generator_grammar_count==3`, branch==0, type==0, rows_collapsed). No window where the conjunction
is asserted complete while a conjunct is unmet. The relocated-seam (a per-grammar branch moved into
a neutral data table) is caught ONLY by the full-row `PartialEq` co-gate, never a regex — bound at
P3/§2.1/G3.2.4 with the FORBIDDEN hand-rolled prose-compare correctly named REJECT. ACCEPT.

### C3 [ACCEPT] — emit_shape_source is the binding defence against the relocated-fork (no phantom-fork admitted)
The §5-risk-1 relocated seam (un-fork the visible `RuntimeEmitterKind` but leave a per-grammar
branch in a neutral `RuntimeTarget`/`ProjectionSpec`/per-profile column) is closed by the 5th G3
conjunct `emit_shape_source==lowered_program` — the `render(program)` body grep for ANY read of
`target.profile|target.emitter|target.output_labels|target.profile_contract|contract.emitter` == 0.
The field-set is IDENTICAL between the Section-1#3 standing seam-scan and the G3-exit conjunct-5
grep (verified line 342 vs 1108-1110), so the standing scan is as strong as G3's own. The SPEC
explicitly states "Without this fourth conjunct, the §5-risk-1 relocated seam ... passes all of
conjuncts 1-4 under a green gate" — the hazard is named and gated. No fork (visible or relocated)
is admitted. ACCEPT.

### C4 [ACCEPT] — the phantom <G> is resolved by DELETE with a test-excluding falsifier (no phantom false-greens)
G4.2.1 greps `EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData` over `runtime/src/`
test-EXCLUDED (the standing `_proof_compiles::<JsonEventGrammar>` is test-only and must NOT
false-green — explicitly named). The `K=Kind` axis is PRESERVED (preserve-rich-ast). The
witness-emission coupling is sound: Sheets emits at PROVE, which entry-gates on G4 closed, and G4
DELETES the axis, so post-G4 no `EventGrammar` literal can be emitted by construction; the P4
`FORBIDDEN ⊇ {EventGrammar,*EventGrammar}` stands as defence-in-depth (lines 516-517, 1460-1464).
The `instantiated` enum value being unreachable-as-a-pass at G4 is harmless (it is a non-passing
state the gate REJECTs, like `present`); the DELETE branch of addendum-4's "instantiate-or-delete"
is grounded in the EMPTY non-test instantiation census. No orphan phantom. ACCEPT.

### C5 [ACCEPT] — no orphan kernel: every primitive lands WITH its hot-path consumer; G5 authors nothing
Addendum 6's no-orphan law is bound at §1 (dav1d discipline: "same-wave hot-path consumer per
primitive, no orphan kernel ships, the SK-V5 failure"), at the G2↔G6 one-seam coupling
(`css_balanced_component_scan` IS the G6 retarget call site), and at G6 task-3 ("land consumer +
entry in ONE commit — addendum 6 no-orphan law"). `acceleration_at_admission==admission` requires
BOTH a generated-`generated.rs` caller census AND `simd_admission_profile_sampled==true`
(runtime-reachability) — a census hit in dead code with no profile attribution == `dead`. G5
retires/neutralizes the zero-sampled `json/scan.rs` and authors NOTHING for JSON (S-P1 has no JSON
G5 hot leaf); the orphan-JSON-classifier route is pre-blocked (§11). dav1d/aarch64-only is bound at
addendum-6, the dav1d non-negotiable, P1, and §11. No orphan kernel admitted. ACCEPT.

### C6 [ACCEPT] — Sheets is a genuine negative-control: no courier admitted, the shim verdict is N not a paper-close
PROVE's `sheets_grammar_shape==pratt-operator` is proven by a CONCRETE structural falsifier (≥7
chained per-level descent fns + the cyclic `paren_expr→expression` back-edge, machine-counted) —
NOT "by construction"; a flat-stream/tree emission has <7 chained fns and FAILS. The binding
fallback is the §0.3 `N` verdict ("Sheets cannot emit via the generator ONLY → generalization NOT
real"), surfaced HONESTLY, NEVER `S`, NEVER paper-closed, NEVER a hand-written `_GENERATED_RS`
block. The `import_closure` relaxation is DATA (present-iff-grammar-has-imports from facts), NOT a
`match grammar` arm. No Sheets courier/fork is admissible. ACCEPT.

### C7 [ACCEPT] — preserve-rich-ast is bound on all three axes; the >SOTA gate is load-robust and falsifiable
preserve-rich-ast bound on: the trait axis (G4.2.2 `json_rich_navigation_preserved` byte-equal diff
+ G4.2.3 `shared_trait_non_collapsible` substitution falsifier — a degenerate-equal CSS impl
COMPILES under substitution ⇒ REJECT); the bench axis (`track1_rich` LAZY-rich, re-derived from
spans, nothing eager to arena); the dispatch-triple axis (G1 do-NOT-LCD-collapse the value/object/
array variants). The CSS >SOTA gate is the SAME-RUN `track1_rich/lightningcss > 1.0×` ∧ no
regression vs the G2-entry-captured pre-G2 baseline (measured in ONE quiet plane so host depression
cancels) — keying on an un-re-locked absolute is explicitly the unfalsifiable hazard the same-run
comparison REPLACES; oracle parity gates BEFORE speed. recognition-only `track1_full_parse` does
NOT discharge the typed close. ACCEPT.

### C8 [REVISE] — P4's `{CSS_, _RS}` forbidden tokens over-match 6 surviving scaffold consts under `source.contains`, so the P4 Lock-14 gate (addendum-1's load-bearing predecessor) cannot close GREEN as specified
P4 (§3.4 task-2) extends `FORBIDDEN_GENERIC_TOKENS` with `{CSS_, _RS, EventGrammar, *EventGrammar}`
AND moves `runtime_generator.rs` into strict `GENERIC_SCAN_ROOTS`. The match semantics on disk is a
plain substring test — `lock14_baseline.rs:2695 if source.contains(token)`. Re-grepping
`runtime_generator.rs` this pass shows EIGHT `const … : &str = r#"…"#` `_RS` couriers, of which the
SPEC names only TWO as retired (G1 folds `JSON_PARSE_ONLY_GENERATED_RS:195`; G2 retires
`CSS_GENERATED_RS:701`). The SIX UNNAMED survivors are all verbatim `&str = r#"…"#` blobs in the
P4-scanned file:
`JSON_PARSE_ONLY_PARSER_RS:550`, `JSON_MOD_RS:572`, `JSON_HOST_RS:594`, `CSS_MOD_RS:598`,
`CSS_PARSER_RS:612`, `CSS_SINK_RS:665` (mod/host/parser/sink scaffolding — disk-confirmed:
`JSON_MOD_RS` is a `pub mod …; pub use …;` scaffold, `CSS_PARSER_RS`/`CSS_SINK_RS` are spliced Rust).
Under `source.contains`, the `_RS` token matches ALL SIX identifier names, and `CSS_` independently
matches `CSS_MOD_RS`/`CSS_PARSER_RS`/`CSS_SINK_RS`. Consequence under the addenda-law lens: P4's exit
gate requires `accepts_current_allowlist` GREEN "AFTER the re-inject/revert proof, not
green-by-exclusion" (§3.4 line 720), but with the six scaffold consts present and untouched by P4,
the gate is RED at P4 close on couriers NO wave retires before P4 — P4 retires none (root-move +
token-extend only), G1 retires only the GENERATED courier, G2 only `CSS_GENERATED_RS`. The SPEC
nowhere accounts for these six (`grep MOD_RS|HOST_RS|PARSER_RS|SINK_RS SPEC.md` == 0). This is not
an addendum violation (addendum 1 correctly WANTS verbatim `_RS` blobs gone, and `verbatim_blob_present
== false` campaign-wide is the right intent) and not a broken sequence in the lattice — it is an
unfalsifiable/unachievable P4 exit-gate as authored, on the load-bearing predecessor of every
emitter wave. An implementer reaching P4 hits a RED gate with no SPEC guidance on whether to retire
the six (addendum-1 coverage) or scope the token (gate precision); either reading materially
mis-directs. NAMES the section + the EXACT one-line edit:
SPEC §3.4 task-2 (line 708) — replace `with `{CSS_, _RS, EventGrammar, *EventGrammar}`` with
`with `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` (the `_RS` token is scoped to
the grammar-body-courier suffix `GENERATED_RS` — catching BOTH `CSS_GENERATED_RS` and
`JSON_PARSE_ONLY_GENERATED_RS`, the two grammar-body couriers G1/G2 retire — so the six MOD/HOST/
PARSER/SINK scaffold consts do not false-RED P4; the bare `CSS_`/`_RS` substrings collide with the
surviving scaffold under `source.contains`, lock14_baseline.rs:2695)`. The matching re-inject
example (line 715) and the P4/P5 rerun-ceiling row (line 471) and §3.6 column (line 769) should
carry the same `GENERATED_RS` scoping for consistency, but the line-708 token-list is the
load-bearing edit. REVISE (gate-precision on addendum-1's predecessor, not candidate-selection, not
sequencing-DAG, not an addenda violation).

## Net

Every wave preserves the 6 addenda, the §6 (a)-(d) escape, the >SOTA gate, dav1d/aarch64-only, and
preserve-rich-ast; no wave admits a courier/fork/phantom/orphan-kernel by design — every such route
sits in the §11 ledger behind a turning-RED falsifier, and the addenda witnesses ground disk-true.
The addenda-binding sequence (P3 structural-half → G3 arm-half → PROVE re-assert-all-4) is unbroken.
ONE residual precision REVISE (C8): P4's `{CSS_, _RS}` forbidden-token set over-matches six surviving
scaffold `_RS`/`CSS_` consts in the now-strict-scanned `runtime_generator.rs` under the plain
`source.contains` semantics, leaving the P4 Lock-14 exit-gate — addendum-1's load-bearing emitter
predecessor — unable to close GREEN as authored; the one-line fix scopes the token to the
`GENERATED_RS` courier-body suffix. No REJECT: the addenda framework is sound, complete, and
falsifiable; the sequence is intact; the fix is a single token-scoping edit.

TALLY accept=7 revise=1 reject=0
