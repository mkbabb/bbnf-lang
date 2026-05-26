# SK-V14 W5A-E: Sheets/BBNF-Self Witness Contract

Date: 2026-05-26.
Scope: W5A research agent E - how Sheets and BBNF-self can satisfy fail-closed or generated-role witness requirements through the same source-consuming parser/contract.
Output: this file.

## §1 - Findings (concrete, file:line cited)

1. W5A's authority requires the Sheets and BBNF-self proof to use the same parser/contract as CSS/JSON, not an adjacent witness path. SPEC Section 8 makes W5A own `skinny/crates/grammar/src/lib.rs`, `skinny/crates/codegen/src/lib.rs`, a successor `grammar_provider.rs`, `regen.rs`, and `regen_css.rs` for one grammar-neutral source-consuming runtime generator contract (`restart/skinny/tranches/sk-v14/SPEC.md:637-645`). The W5A entry gate explicitly requires a plan that names the source + workspace metadata contract and the Sheets/BBNF-self fail-closed or generated-role witnesses (`restart/skinny/tranches/sk-v14/SPEC.md:654-658`). The task/exit gate repeats that Sheets and BBNF-self must either fail closed with named unsupported constructs or emit generated-role witnesses through the same parser/contract (`restart/skinny/tranches/sk-v14/SPEC.md:660-678`). G-Omega V5 CH2 says the fold was accepted only after it required "Sheets and BBNF-self fail-closed or generated-role witnesses through the same parser/contract" (`restart/audit/totality/astral/V5/hardening/CH2.md:17-25`).

2. The dispatch and triumvirate contracts make this research read-only and load-bearing for the W5A plan. DISPATCH requires each research agent to read current source, latest results/redress, SPEC wave section, synthesis, dispatch context, and S-P2 carry-forward packets, then produce one artifact (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:49-54`). Before W5A, dispatch must verify Pass Omega V5 close and the amended sequencing, and provider/template deletion is forbidden until W5A admits all-seven CSS coverage plus JSON/Sheets/BBNF-self proof (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:135-145`). The CHALLENGE lens asks whether the plan generalizes to non-JSON grammars and avoids paper-close (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:169-177`). The triumvirate contract says research is read-only and one artifact, with no source edits or files outside research (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11-13`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190-196`).

3. REDRESS-209 is the live failure shape W5A must avoid. It records that original W5 was rejected because it required a grammar-agnostic generator consuming grammar source and workspace metadata plus CSS provider/template deletion in one slice, while HEAD still routes `regen-css` through `codegen::emit_runtime_profile(target.profile)` and hashes source/metadata only as freshness evidence (`skinny/REDRESS.md:5171-5183`). The supersession note says REDRESS-209 remains historical, and the current route is W5A source-consuming generator capability followed by W5B deletion after W5A admits (`skinny/REDRESS.md:5189-5193`).

4. Current skinny code already has one source-consuming JSON path, but CSS runtime emission still uses profile-only dispatch. `codegen::emit_from_source(grammar_name, source)` parses source, runs passes, and lowers through `emit_with_layout` (`skinny/crates/codegen/src/lib.rs:98-107`). `codegen::emit_runtime_profile(grammar_name)` selects a profile by name and renders provider-backed files (`skinny/crates/codegen/src/lib.rs:117-120`). `render_runtime_profile` then matches concrete `RuntimeProvider` variants for seven CSS profiles and JSON (`skinny/crates/codegen/src/lib.rs:162-229`), while `grammar_profile.rs` defines provider enum variants and registers the eight profiles (`skinny/crates/codegen/src/grammar_profile.rs:16-26`, `skinny/crates/codegen/src/grammar_profile.rs:100-110`). `regen.rs` validates inputs but calls `emit_runtime_profile` for both write and check paths (`skinny/xtask/src/regen.rs:14-33`); its `validate_inputs` hashes source and metadata but never passes the bytes to codegen (`skinny/xtask/src/regen.rs:61-74`). Therefore, a W5A witness that calls only `emit_runtime_profile` would repeat REDRESS-209.

5. The current skinny parser can be the common contract, but today it is too small and its failures are not named enough for W5A. `grammar::parse_grammar` is the sole public parse entry for arbitrary grammar names (`skinny/crates/grammar/src/lib.rs:29-35`). It only accepts `@import` and `@token` directives, rejecting other directives as `BBNF-DIRECTIVE-NOT-IN-SKINNY` (`skinny/crates/grammar/src/lib.rs:80-99`), and its atom parser supports double-quoted literals, slash regexes, parenthesized expressions, and refs only (`skinny/crates/grammar/src/lib.rs:196-230`). Tests prove non-skinny directives reject (`skinny/crates/grammar/src/lib.rs:408-417`). That is a viable fail-closed base, but not yet a W5A-quality witness because an input such as `-> f64` currently surfaces as a generic token error rather than a named unsupported construct such as `BBNF-UNSUPPORTED-PROJECTION`.

6. Sheets is a strong fail-closed target if the contract reports named unsupported constructs. Its grammar needs typed projections on numeric, string/span, bool, tag, cell, and identifier rules (`grammar/google-sheets/google-sheets.bbnf:6-18`, `grammar/google-sheets/google-sheets.bbnf:34-42`, `grammar/google-sheets/google-sheets.bbnf:52-63`, `grammar/google-sheets/google-sheets.bbnf:86-90`). It also uses BBNF separators and whitespace sugar that the skinny parser does not handle, including `?w`, comma-separated sequences, `<<`, and `@pretty` directives (`grammar/google-sheets/google-sheets.bbnf:103-121`, `grammar/google-sheets/google-sheets.bbnf:141-155`, `grammar/google-sheets/google-sheets.bbnf:171-185`). If W5A chooses fail-closed proof, the first acceptable Sheets result should be an explicit unsupported code for projection or sequence separator, with file/offset/source hash recorded by the same `RuntimeEmissionRequest` used by CSS.

7. BBNF-self has even more unsupported surface, so it is better as a named fail-closed witness than as a full generated-role witness inside W5A's cap. The self grammar imports expression/type grammars (`grammar/bbnf/bbnf.bbnf:4-5`), uses typed projections and source-span leaves (`grammar/bbnf/bbnf.bbnf:9-18`), embeds `@{...}` host/value expressions and `?w` whitespace sugar (`grammar/bbnf/bbnf.bbnf:29-42`), uses binary grammar operators (`<<`, `>>`, `-`) and closure syntax (`grammar/bbnf/bbnf.bbnf:44-52`), and declares multiple directives beyond the skinny parser's accepted set (`grammar/bbnf/bbnf.bbnf:56-85`). The imported expression grammar adds `@import`, projection, typed literals, function calls, precedence rules, and `type_annotation` (`grammar/bbnf/expressions.bbnf:3-37`); `types.bbnf` defines primitive and identifier type names (`grammar/bbnf/types.bbnf:1-7`). A W5A BBNF-self witness should therefore fail closed on a named unsupported import resolution, projection, host capture, or directive construct rather than attempt full BBNF-self generation.

8. The full core generator proves the role inventory W5A should imitate, but it is not itself the W5A witness because it is outside skinny and already generated. Sheets generated code embeds the source grammar, structural alphabet, digraphs, quote classes, PHF dispatch, and Pratt metadata (`crates/core/src/grammar/generated/google_sheets.rs:33-47`, `crates/core/src/grammar/generated/google_sheets.rs:75-90`). It has concrete parser entry points for formula parsing (`crates/core/src/grammar/generated/google_sheets.rs:9442-9573`). The Sheets runtime builder documents typed role routing for `number`, `string`, `boolean`, error tags, sheet prefixes, cell refs, identifiers, and operators (`crates/core/src/runtime/google_sheets/builder.rs:23-37`) and exposes specialized leaf methods for cell refs, identifiers, sheet prefixes, and errors (`crates/core/src/runtime/google_sheets/builder.rs:316-354`). This is the shape of a generated-role witness: grammar source facts produce role facts, not handwritten assertions.

9. BBNF generated code likewise shows the target role shape. It embeds `grammar/bbnf/bbnf.bbnf`, structural alphabet, digraphs, and quote classes (`crates/core/src/grammar/generated/bbnf.rs:33-66`), then exposes `parse_flat_BbnfBootstrap_rule`, `parse_wrap_BbnfBootstrap_grammar_item`, `parse_array_BbnfBootstrap_grammar`, and `parse_BbnfBootstrap_grammar` (`crates/core/src/grammar/generated/bbnf.rs:13440-13470`, `crates/core/src/grammar/generated/bbnf.rs:14136-14440`). The BBNF runtime value type documents that shape derivation comes from `grammar/bbnf/bbnf.bbnf` plus imports and collapses every compound rule into `BbnfValue::Compound` while preserving rule-level discriminators (`crates/core/src/runtime/bbnf/value.rs:10-29`, `crates/core/src/runtime/bbnf/value.rs:49-65`). W5A can emit a narrow witness over this role vocabulary without generating the full runtime.

10. Existing skinny `sheets_witness` is not sufficient for W5A by itself. It is compiled only under test/proof (`skinny/crates/runtime/src/lib.rs:27-33`), and its role set is handwritten (`skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:1-23`). The proof test only checks the marker compiles and class bounds hold (`skinny/crates/runtime/src/tape/event_grammar_tests.rs:40-49`). Existing SK-V13 witness JSON files are status artifacts (`restart/skinny/tranches/sk-v13/research/w7/sheets-witness.json:1-8`, `restart/skinny/tranches/sk-v13/research/w7/bbnf-self-witness.json:1-8`); W5A needs a same-contract output from the source-consuming request, not reuse of those proof-only artifacts.

11. Workspace metadata is uneven between full core and skinny. The root workspace strategy table already names Google Sheets and BBNF parser idents plus builder/document paths (`Cargo.toml:45-50`). The skinny workspace metadata names only JSON as a grammar source/output target (`skinny/Cargo.toml:54-71`). `regen_css.rs` compensates by hard-coding all CSS source inputs and both root + skinny Cargo metadata as freshness inputs (`skinny/xtask/src/regen_css.rs:5-23`), then defines seven CSS runtime targets (`skinny/xtask/src/regen_css.rs:25-75`). W5A should not add grammar-specific branches for Sheets/BBNF; it should let the same request type carry a source bundle and metadata digest for any grammar.

### Commands Executed At HEAD

```
pwd && git status --short && git rev-parse HEAD
```

Result: cwd was `/Users/mkbabb/Programming/bbnf-lang`; HEAD was `66d15dfb504b3ef31b2f11f7c5529c8ce6505cd6`. Dirty files existed before this work under older research paths plus `restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md`; none are part of this artifact.

```
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '637,698p'
nl -ba restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md | sed -n '49,65p;94,117p;135,145p;147,156p;158,177p;184,203p'
nl -ba restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md | sed -n '11,38p;177,199p'
```

Result: W5A owner paths/tasks/exit gate require same-contract Sheets/BBNF proof; dispatch requires one research artifact and V5 pre-dispatch checks; triumvirate role separation forbids source edits in research.

```
printf 'providers='; find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l
printf 'css_templates='; find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l
printf 'skinny_grammar_sources='; find skinny/grammars -type f -name '*.bbnf' | wc -l
printf 'root_sheets_bbnf_sources='; find grammar/google-sheets grammar/bbnf -type f -name '*.bbnf' | wc -l
```

Result: `providers=8`, `css_templates=7`, `skinny_grammar_sources=1`, `root_sheets_bbnf_sources=4`.

```
rg -n -- '->|<<|\?w|@pretty|@import|@host|@\{' grammar/google-sheets/google-sheets.bbnf grammar/bbnf/bbnf.bbnf grammar/bbnf/types.bbnf grammar/bbnf/expressions.bbnf | head -80
```

Result: hits include Sheets `->` projections and `?w`/`<<`/`@pretty` surfaces, plus BBNF `@import`, `->`, `@{...}`, `?w`, `<<`, `@host`, and `@pretty` surfaces. These are the unsupported constructs W5A must name or parse into role facts.

```
rg -n "parse_grammar\(|emit_from_source\(|emit_runtime_profile\(|validate_inputs\(|sheets_witness|event_grammar_witnesses_compile|unsupported|BBNF-DIRECTIVE-NOT-IN-SKINNY" skinny/crates skinny/xtask -g '*.rs'
```

Result: source-consuming calls are JSON-only today (`emit_from_source`); CSS regen/check calls are profile-only (`emit_runtime_profile`); `sheets_witness` is test/proof-only.

```
rg -n "pub struct (GoogleSheetsParser|BbnfBootstrap)|GRAMMAR_STRUCTURAL_ALPHABET|GRAMMAR_STRUCTURAL_DIGRAPHS|parse_.*(GoogleSheetsParser|BbnfBootstrap).*formula|parse_.*BbnfBootstrap_grammar|pub fn parse\(" crates/core/src/grammar/generated/google_sheets.rs crates/core/src/grammar/generated/bbnf.rs | head -120
```

Result: core generated Sheets/BBNF already carry structural alphabets/digraphs and concrete parse entry points, which are useful target shapes for generated-role witnesses but not sufficient as skinny W5A proof.

## §2 - Recommendations (named falsifiability gates)

1. Add one W5A contract type, not separate Sheets/BBNF hooks. Suggested shape:

```rust
pub struct RuntimeEmissionRequest<'a> {
    pub grammar_id: &'a str,
    pub target_id: &'a str,
    pub sources: &'a [GrammarSource<'a>],
    pub workspace_metadata: &'a [MetadataInput<'a>],
    pub output_roster: &'a [&'a str],
}

pub enum RuntimeEmissionProof {
    Emitted(EmittedSource),
    FailClosed(UnsupportedConstruct),
    RoleWitness(GeneratedRoleWitness),
}
```

The implementation should call the same `grammar::parse_grammar` successor for JSON, CSS, Sheets, and BBNF-self. `regen-css` then passes CSS source + metadata into this request; Sheets/BBNF tests pass their source bundles into the same request in witness mode.

2. Make fail-closed errors named, source-located, and contract-owned. Minimum named codes:

- `BBNF-UNSUPPORTED-PROJECTION` for `-> f64`, `-> input : Span`, `-> 0u8`, and `-> Span`.
- `BBNF-UNSUPPORTED-SEQ-SEPARATOR` for comma-separated BBNF sequences if W5A does not parse commas yet.
- `BBNF-UNSUPPORTED-WHITESPACE-MODIFIER` for `?w`.
- `BBNF-UNSUPPORTED-IMPORT-RESOLUTION` when an `@import` is syntactically accepted but source bundle resolution is not implemented.
- `BBNF-UNSUPPORTED-DIRECTIVE` with the directive name for `@pretty`, `@recover`, `@ws`, `@debug`, and `@host`.
- `BBNF-UNSUPPORTED-HOST-CAPTURE` for `@{...}`.

Gate: `cargo test -p grammar source_contract_reports_named_unsupported_constructs --lib` with fixtures from Sheets and BBNF-self. Expected result: exact code, grammar id, relative path, byte offset, and source hash are asserted.

3. Prefer fail-closed witnesses for W5A; defer full generated-role witnesses unless the parser work is already needed for CSS. Sheets and BBNF-self contain far more than the current skinny parser's JSON surface, and W5A's primary deliverable is CSS source-consuming `regen-css`. A fail-closed witness is valid under SPEC if it is named and exits through the same parser/contract. Generated-role witnesses are only worth selecting if the W5A parser extension already supports projections/import bundles as part of the CSS path.

4. If the plan selects generated-role witnesses, keep them minimal and generated from parsed source. Required witness fields:

- `grammar_id`, `source_paths`, `source_hash`, `metadata_hash`.
- `structural_singletons`, `structural_digraphs`, and quote classes where known.
- `rules_seen` with `rule_name`, `source_span`, and `role`: `span_leaf`, `numeric_leaf`, `bool_leaf`, `tag_leaf`, `compound`, `operator_tag`, or `unsupported`.
- `unsupported_constructs` must be empty for `RoleWitness`, non-empty for `FailClosed`.
- `generated_by` must name the W5A contract function, not `sheets_witness`.

Gate: `cargo test -p codegen source_contract_generates_sheets_bbnf_role_witnesses --lib`. Expected result: no handwritten fixture lookup; the test mutates source text and observes source hash/offset/role changes.

5. Add a codegen-level W5A witness test rather than a runtime/generated file. Suggested exact gate names for the plan:

- `cargo test -p codegen w5a_runtime_contract_uses_source_and_metadata --lib`
- `cargo test -p codegen w5a_sheets_bbnf_witnesses_use_runtime_contract --lib`
- `cargo test -p grammar w5a_named_unsupported_constructs --lib`

These tests avoid writing generated output while proving the contract. `cargo xtask regen-css` and the seven `check-css-l4-*` companions remain the same-wave production consumers for CSS.

6. Do not satisfy W5A with the existing `sheets_witness` module or SK-V13 witness JSON. Add a guard test or grep in the plan:

```
rg -n "sheets_witness|sk-v13/research/w7/(sheets|bbnf-self)-witness" skinny/crates/codegen skinny/xtask
```

Expected result: zero matches in the W5A contract path. Test/proof-only runtime modules may remain untouched.

## §3 - Risks (REDRESS entries to pre-block)

1. **Paper fail-closed risk:** Current parser errors are too generic for W5A. A raw `unexpected token '-'` on `->` does not satisfy "named unsupported constructs"; it would reopen REDRESS-209 as a renamed static-provider proof.

2. **Sidecar witness risk:** Reusing `sheets_witness` or SK-V13 witness JSON would not prove source-consuming behavior. The witness must be produced by the same request that `regen-css` consumes.

3. **Import false-negative risk:** BBNF-self uses three files. If `@import` is skipped without loading the bundle, the parser may fail later as unresolved refs. That is only acceptable if the contract reports `BBNF-UNSUPPORTED-IMPORT-RESOLUTION` at the import site, not a downstream unresolved symbol.

4. **Full-parser scope risk:** Full Sheets + BBNF-self generation inside W5A likely exceeds the <=1.0k C-1 part-A cap. Use fail-closed unless parser extensions are already necessary for the CSS source path.

5. **Core-generated leak risk:** Full core generated BBNF currently has an eager empty path typed as `markers::Json` in the BBNF parse body (`crates/core/src/grammar/generated/bbnf.rs:21446-21456`). Do not cite core generated output as W5A proof without a skinny-produced witness and a JSON-policy leak check.

6. **Metadata branch risk:** Root metadata knows Sheets/BBNF while skinny metadata only knows JSON. Adding `grammar_id == sheets` or `grammar_id == bbnf` branches in codegen would violate Lock 14; source bundle + metadata rows must feed one generic contract.

## §4 - Sources (every external citation)

No external sources used. All citations are local repository paths re-read at HEAD `66d15dfb504b3ef31b2f11f7c5529c8ce6505cd6`.
