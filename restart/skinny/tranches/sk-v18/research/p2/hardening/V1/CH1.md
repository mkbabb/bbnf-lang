# SK-V18 S-P2 CHALLENGE — Lens CH1 GROUNDEDNESS (cycle V1)

**Lens question:** is every recommended candidate and load-bearing claim in the S-P2 research
grounded in ACTUAL cited `path:line` code, not abstract hand-waving? Flag any claim not traceable
to a real file.

**Targets read:** `SYNTHESIS-RESEARCH.md` (primary), `rA-emitter-unify.md`, `rC-json-projection.md`.
**Verification method:** Read/Grep against the live tree at the cited locations. Line numbers
judged on substance, ±a few lines tolerated. Repo root `/Users/mkbabb/Programming/bbnf-lang`.

## Verdict summary up front

The S-P2 research is, on the groundedness axis, **exceptionally well-anchored**. Every spotlight
claim resolves to real code at the cited (or near-cited) line. Several LOC/md5 claims are
byte-exact (`json_typed_direct.rs`=1245, shipped `json/generated.rs`=1235, `json_templates/`=391,
the 7 css_l4 replicas all md5 `b654562c…`). I found exactly ONE genuinely misplaced citation (a
line number pointing at the wrong file inside SYNTHESIS-RESEARCH's compressed grounding paragraph),
plus one census-phrasing claim that is technically correct but needs a one-clause sharpening to be
unimpeachable. No claim cites fabricated/non-existent code.

---

## Enumerated claims and judgments

### Spotlight set (the four the lens named)

1. **`grammar_provider.rs:40-42` — `enum RuntimeEmitterKind { CompiledLowering, RequestFacts }`.**
   VERIFIED exact: enum at lines 40-42, `#[derive(Clone, Copy, Debug, PartialEq, Eq)]` at line 39
   (rA cites `:39` for the derive). **ACCEPT.**

2. **`runtime_generator.rs ~:91 (CSS_GENERATED_RS courier)` and `~:37 (json_sink_direct::render)`.**
   VERIFIED exact: line 91 = `("generated.rs".to_string(), normalize(CSS_GENERATED_RS))`; the
   `CSS_GENERATED_RS` const itself is defined at `:701` and the raw string runs to EOF `:1611`
   (rA cites `:701`→`:1611`, 910-LOC — exact). Line 37 = `json_sink_direct::render(sink_only)`
   inside `emit_compiled` (`:29`). **ACCEPT.**

3. **`lower/mod.rs:18-25` — `select_lowering(cost)` over `BackendShape` (5-shape neutral axis
   `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`).** VERIFIED exact: `pub fn
   select_lowering(cost: &CostFacts)` at line 18, match arms 19-25, all five shapes present, zero
   grammar names. The "Lock-14-clean grammar-NEUTRAL discriminator" framing is faithful. **ACCEPT.**

4. **`tape/mod.rs:175` — the phantom `<G>` (`ValueRef<…K=AnyKind, G: EventGrammar = AnyGrammar>`
   + `_grammar: PhantomData`).** VERIFIED exact: `pub struct ValueRef<'doc, 'input: 'doc, K =
   AnyKind, G: EventGrammar = AnyGrammar>` at line 175; `_grammar: PhantomData<fn() -> G>` at line
   179. SYNTHESIS also cites `:227-228 (latent DocumentView/type Root)` — VERIFIED at 227-229
   (`pub trait DocumentView<'a> { type Root: 'a; … }`). **ACCEPT.**

### SYNTHESIS-RESEARCH §intro grounding paragraph (the compressed citation block, lines 11-27)

5. **`grammar_provider.rs:46-57` — `RuntimeFrontendRequirements`, the 10-bool struct.** VERIFIED
   exact: struct at 46-57, all 10 bools (`import_closure … comma`) present. **ACCEPT.**

6. **`:32-37` — `RuntimeProfileContract` carries `emitter` + `frontend_requirements` (field #3) +
   `output_labels` (field #4).** VERIFIED: struct at 31-37; `emitter`=field#1 (`:33`),
   `expected_files`=#2 (`:34`), `frontend_requirements`=#3 (`:35`), `output_labels`=#4 (`:36`).
   Ordinals exact. **ACCEPT.**

7. **`runtime_generator.rs:16` — the live fork dispatch (`match request.profile_contract.emitter`).**
   VERIFIED exact: line 16. **ACCEPT.**

8. **`runtime_generator.rs:110` — "JSON-only `first_unsupported` gate".** **REJECT (misplaced
   file).** `first_unsupported` does NOT appear anywhere in `runtime_generator.rs` (grep returns
   zero). `runtime_generator.rs:110` is a comment line (`// SK-V17 W1: the fact-stream policy-plane
   constants …`) inside `render_request_facts_config`. The actual `first_unsupported()` fail-closed
   gate lives at **`grammar_provider.rs:110-111`** (`if request.profile_contract.emitter !=
   RuntimeEmitterKind::RequestFacts { if let Some(unsupported) = facts.first_unsupported() …`).
   FIX (SYNTHESIS-RESEARCH §intro, line 17): change `runtime_generator.rs:16`+`:110` (the live fork
   dispatch + JSON-only `first_unsupported` gate)` to `runtime_generator.rs:16` (fork dispatch) +
   `grammar_provider.rs:110-111` (JSON-only `first_unsupported` gate)`. NB: rA-emitter-unify.md
   §0 cites this CORRECTLY (`grammar_provider.rs:110`); only the SYNTHESIS roll-up mis-attributed
   the line to the wrong file. Substance (a real exemption gate exists, neutral-named, JSON pays /
   CSS exempt) is sound; only the file:line is wrong.

9. **`runtime_generator.rs:37 (json_sink_direct::render)`.** Duplicate of #2; VERIFIED. **ACCEPT.**

10. **`lower/sink_only.rs:68-96` — the rich `SinkOnlyExpr` IR the `render()` discards.** VERIFIED:
    `pub enum SinkOnlyExpr` at 69-96 (Entry/Seq/Alt{mode,branches}/RepeatLoop{body,min}/
    OptionalBranch/ByteLiteral/RegexProgram{span_kind,pattern}/CallRule{callee}/SpanMark/TapeEmit/
    DirectBuild/ValueProject/Return). All cited node names exist; cite is `:68-96`, enum body
    `69-96` (the `:68` covers the `#[derive]` at `:68`). The "`render()` discards" framing matches
    json_sink_direct's `render_*(out: &mut String)` signatures (see #16). **ACCEPT.**

11. **`xtask/regen.rs:5-18 — RuntimeTarget derives only `Clone,Copy,Debug`; carries `emitter`,
    `frontend_requirements` (field #11, `:17`), `output_labels` (field #12, `:18`)`; the R16 +1-line
    `PartialEq` target.** VERIFIED exact: `#[derive(Clone, Copy, Debug)]` at `:5` (no PartialEq);
    struct fields counted from `grammar_name`=#1 (`:7`) → `emitter`=#9 (`:15`), `expected_files`=#10
    (`:16`), `frontend_requirements`=#11 (`:17`), `output_labels`=#12 (`:18`). File resolves to
    `skinny/xtask/src/regen.rs`. Ordinals exact. **ACCEPT.**

12. **The R16 numbering hazard (`grammar_provider.rs:35-36` field #3/#4 vs `regen.rs:17-18` field
    #11/#12; `PartialEq,Eq` derives the recipe leans on at `grammar_provider.rs:45`/`:91`).**
    VERIFIED: `RuntimeFrontendRequirements` derive incl. `PartialEq, Eq` at `:45`; `RuntimeOutputLabels`
    derive incl. `PartialEq, Eq` at `:91`. Both nested struct TYPES already derive PartialEq, so the
    "+1-line `RuntimeTarget: PartialEq` recurses automatically" recipe is mechanically sound. The
    two-ordinal-system warning is itself grounded. **ACCEPT.**

13. **The `_proof_compiles` G-instantiation census excluding `_tests.rs` returns EMPTY → `<G>` has
    zero non-test production animator.** VERIFIED-WITH-CAVEAT → **REVISE (sharpen one clause).**
    Grep: `_proof_compiles` appears ONLY in `skinny/crates/runtime/src/tape/event_grammar_tests.rs`
    (definition `:23`; instantiations `:18/:20/:21/:35/:43/:44` over JsonEventGrammar/
    SheetsEventGrammar/AnyGrammar). So "`_proof_compiles` census excluding `_tests.rs` = EMPTY" is
    literally TRUE, and the conclusion "zero non-test production ANIMATOR of `ValueRef<…,G>`" is
    correct (nothing in production parameterizes `ValueRef` over a concrete `G`). HOWEVER the
    surrounding prose risks over-reading: production-tree witness DEFINITIONS *do* exist —
    `grammars/json/event_grammar_witness.rs` (`struct JsonEventGrammar; impl EventGrammar …`) and
    `grammars/sheets_witness/event_grammar_witness.rs` — plus the trait/`AnyGrammar` at
    `tape/event_grammar.rs:4/:17`. They are unanimated witnesses (only consumed by the `_tests.rs`
    proof), not animators. FIX (SYNTHESIS-RESEARCH line 26): append a clause — "(the `…_witness.rs`
    `EventGrammar` impls in json/ + sheets_witness/ exist but are consumed ONLY by the `_tests.rs`
    proof — defined, never animated)". This makes the DELETE-default claim unimpeachable rather
    than vulnerable to a "but the witness types are production code" rebuttal. The load-bearing
    conclusion (R-D DELETE is grounded) STANDS.

### rA-emitter-unify.md spot-checks

14. **`runtime_generator.rs:17-24` JSON arm → `crate::emit_from_source` → … → `emit_compiled`
    (`:29`) → `json_sink_direct::render`.** VERIFIED: lines 17-24 are the `CompiledLowering` arm
    calling `crate::emit_from_source` (`:23`); `emit_compiled` at `:29`; `json_sink_direct::render`
    at `:37`. **ACCEPT.**

15. **CSS arm: `runtime_generator.rs:25` → `emit_request_facts` (`:76`) → `("generated.rs",
    normalize(CSS_GENERATED_RS))` (`:91`).** VERIFIED exact: `:25` RequestFacts arm,
    `emit_request_facts` fn at `:76`, the const courier line at `:91`. **ACCEPT.**

16. **`json_sink_direct.rs` render functions take only `out: &mut String` and push fixed bodies;
    only `render_header` + `render_number_emitter` touch program data.** VERIFIED: `render` `:4`,
    `render_header(program, out)` `:68`, `render_value_dispatch(out)` `:124`,
    `render_container_rules(out)` `:251`, `render_string_rule(out)` `:326`,
    `render_number_rules(out)` `:367`, `render_number_emitter(out, name, prefix)` `:457`,
    `render_utility_rules(out)` `:497`. Confirms the "fixed-literal courier wrapped in render
    functions; only header + number-emitter parameterize" claim. `validate()` at `:18`. **ACCEPT.**

17. **Roster asymmetry: `COMPILED_RUNTIME_FILES` (8 files) `main.rs:175`; `REQUEST_FACTS_RUNTIME_FILES`
    (5 files) `regen_css.rs:25`.** VERIFIED: `const COMPILED_RUNTIME_FILES` at `xtask/src/main.rs:175`;
    `const REQUEST_FACTS_RUNTIME_FILES` at `xtask/src/regen_css.rs:25`. (A second authoritative copy
    of both consts lives at `codegen/src/grammar_profile.rs:3/:15` — the one `validate_generated_roster`
    actually reads; rA's cite to the xtask copy is real, not wrong, but a stricter cite would name
    `grammar_profile.rs:3/:15` as the consumed pair. Not a defect — both exist.) **ACCEPT.**

18. **The 7 byte-identical CSS `RuntimeTarget` rows (`regen_css.rs:35-162`), md5 `b654562c…`.**
    VERIFIED exact: 7 `RuntimeTarget { grammar_name: "css_l4" … }` rows at lines 36/54/72/90/108/126/144,
    all `expected_files: REQUEST_FACTS_RUNTIME_FILES`. All 7 shipped `css_l4*/generated.rs` files
    md5 to **`b654562ccff46ed62dd48e9ace325830`** — exact match to the cited `b654562c…`. The P3
    "7 byte-identical replicas" claim is on-disk reproducible. **ACCEPT.**

### rC-json-projection.md spot-checks

19. **Shipped `json/generated.rs` = 1235 LOC; the three-source concatenation; hot leaf
    `parse_object_value_at_direct` shipped at `:823`; `parse_only_value_iterative` at `:433`.**
    VERIFIED byte-exact: `wc -l` = 1235; `fn parse_object_value_at_direct<'i, S: JsonSink>` at
    shipped line 823; `fn parse_only_value_iterative` at shipped line 433. `json_templates/generated.rs`
    = 391 LOC (rC says 391 — exact). **ACCEPT.**

20. **`JSON_PARSE_ONLY_GENERATED_RS` const courier at `runtime_generator.rs:195`.** VERIFIED exact:
    `const JSON_PARSE_ONLY_GENERATED_RS: &str = r#"` at line 195; spliced in `emit_compiled` at
    `:35`. **ACCEPT.**

21. **`json_sink_direct.rs:169` = `parse_object_value_at_direct` (the codegen-side body);
    `render_number_emitter:457` with `{prefix}` for `sink.`/`sink.object_`/`sink.array_`.** VERIFIED:
    `fn parse_object_value_at_direct<'i, S: JsonSink>` at codegen `:169`; `render_number_emitter(out:
    &mut String, name: &str, prefix: &str)` at `:457`. **ACCEPT.**

22. **House-pattern proofs: `lower/tape_plan.rs` `render_expr` recurses `BackendExpr`;
    `json_typed_direct.rs:render` (1245 LOC) iterates schema with `:91 for field in fields`.**
    VERIFIED: `fn render_expr(expr: &BackendExpr, …)` at `tape_plan.rs:79` (and `render_rule` `:58`);
    `json_typed_direct.rs` `pub fn render(program: &TypedDirectProgram)` at `:10`, `for field in
    fields` at `:91` (and `:116/:192/:322`); file = 1245 LOC exact. **ACCEPT.**

23. **`SinkOnlyExpr` node set reconstructible (rC §1 list) + `policy_summary.backend_shape` on
    `SinkOnlyProgram` (`sink_only.rs:48`).** VERIFIED: rC's node enumeration matches the real enum
    (#10); `RuntimePolicySummary.backend_shape` at `sink_only.rs:48`, reachable as
    `program.policy_summary.backend_shape`. rA's identical claim (`sink_only.rs:48`) also holds.
    **ACCEPT.**

---

## Groundedness assessment

Of 23 enumerated groundedness-relevant claims, 21 ACCEPT, 1 REVISE, 1 REJECT. The single REJECT
(#8) is a file-attribution slip inside the SYNTHESIS roll-up paragraph — the underlying fact is
real and the source digest (rA) cites it correctly; only the consolidated line points
`first_unsupported` at `runtime_generator.rs:110` when it lives at `grammar_provider.rs:110-111`.
The single REVISE (#13) is a precise-but-incomplete census phrasing that should name the
production witness definitions it implicitly excludes, so the DELETE-default cannot be challenged
as ignoring production `EventGrammar` impls. No recommended CANDIDATE (R-A "A", R-B "B⊃A", R-C
"C1", R-D "A", R-E-2, R-F "A") rests on a fabricated citation; each candidate's load-bearing anchor
(the neutral `BackendShape` axis, the `CSS_GENERATED_RS` const courier, the `SinkOnlyExpr` IR vs
fixed render bodies, the phantom `<G>`/`DocumentView`, the `find_*`/replica surfaces) verifies on
disk. The research's "every load-bearing claim re-grounded at the cited `path:line`" header is
substantially TRUE — with the two corrections above applied.

TALLY accept=21 revise=1 reject=1
