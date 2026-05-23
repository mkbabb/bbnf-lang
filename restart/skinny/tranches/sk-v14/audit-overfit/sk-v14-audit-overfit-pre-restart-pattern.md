# SK-V14 S-P0 Axis A6 — Pre-Restart Pattern Recurrence

## §0 — Disposition summary

Pass criterion (from `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §Scope` row A6):

> Zero CRITICAL Pattern H violations; every other pattern reads CLEAN.

- Findings: **CRITICAL = 3, HIGH = 2, MED = 1, LOW = 1** (7 total).
- Verdict: **FAIL** — Pattern H is recurrent at category scale (60 hand-written per-grammar runtime files in `crates/core/src/runtime/`, plus 48 hand-written per-grammar runtime files in `skinny/crates/runtime/src/grammars/`), and the SK-V13 "hand-written templates loaded via `include_str!()` + fake `@generated` prefix" fake-codegen pattern is intact across all 8 CSS L4 + JSON provider modules in `skinny/crates/codegen/src/`.
- Confirms SK-V13 audit pack: **yes** — every Pattern H finding from the SK-V13 audit pack survives unchanged at SK-V14 starting state (no SK-V14 implementation has landed; only docs in `restart/skinny/tranches/sk-v14/`).
- New findings (not previously enumerated as such in SK-V13's per-axis files): **2** — (a) a `Path` → `LegacyPath` rename shim aliased at the `use` site in 4 grammar `parse_with.rs` files (a backwards-compat shim by every plain reading of the term, latent across the tranches that introduced `TypedSegment`), (b) the `arena_template.rs` / `builder_template.rs` doc-comments explicitly enumerate which grammars *opt out* of template instantiation (JSON, CSS L4, BBNF) — i.e. the substrate itself documents that Pattern H is intentional for the four hot grammars, which is a category-level rather than per-file finding.

V13-disposition disambiguation for finding (a): the V13 `SYNTHESIS-AUDIT-OVERFIT.md` line 74 "Honest patterns left clean" disposition ("No backwards-compat shims that aren't legitimate refactors (Agent F Pattern G)") did not enumerate the `LegacyPath` alias surface. V14 records the `LegacyPath` alias as a NEW finding rather than a reversal of the V13 Pattern G CLEAN reading: the alias was introduced at `0e8dbc104 feat(runtime/parse-with-{json,css-l4,sheets,bbnf}): land W3.2 entry points (AZ-IV.W3.2)` (2026-05-02), before V13 close, as part of an in-flight typed-path refactor whose cursor-threaded `parse_<rule>` consumer was scheduled to land in W3.3. V13's Pattern G scan therefore most plausibly either did not survey the four `parse_with.rs` `use`-site aliases or read them as a legitimate in-flight refactor under the "that aren't legitimate refactors" carveout. V14 surveys them explicitly post-W3.2-landing, classifies the alias as a shim that should be collapsed inside C-1 PRUNE-4 (typed-path collapse) per §4, and records the disposition as scope-extension over V13's Pattern G slate rather than reversal of any specific V13 finding.

## §1 — Methodology

Executable verification commands run (per §3 dispatch context's "Executable verification mandate"):

```
$ find crates/core/src/runtime -name '*.rs' \( -path '*/json/*' -o -path '*/css_l4/*' \
    -o -path '*/google_sheets/*' -o -path '*/bbnf/*' -o -path '*/csv/*' \
    -o -path '*/ebnf/*' -o -path '*/bnf/*' -o -path '*/math/*' \) | wc -l
60

$ find crates/core/src/runtime -name '*.rs' | wc -l
75

$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d
crates/core/src/runtime/bbnf
crates/core/src/runtime/bnf
crates/core/src/runtime/css_l4
crates/core/src/runtime/css_pretty
crates/core/src/runtime/csv
crates/core/src/runtime/ebnf
crates/core/src/runtime/google_sheets
crates/core/src/runtime/json
crates/core/src/runtime/math
```

The dispatch context cited "expect 64". The literal find expression as quoted in the brief evaluates to **60** because it omits `*/css_pretty/*`. Including `css_pretty` raises the count to **67**. The SK-V13 audit-pack body cited 64, which sits between; the gap reconciles to the inclusion-or-not of `css_pretty/*` (7 files) and the orthogonal substrate files `arena_template.rs`, `builder_template.rs`, `builder.rs`, `error.rs`, `handle.rs`, `mod.rs`, `path.rs`, `view.rs` (8 files, all genuinely shared). Either way the Pattern H surface is intact: 9 per-grammar directories under `crates/core/src/runtime/`, all hand-written.

Per-grammar file census under `crates/core/src/runtime/`:

| Grammar dir | `.rs` files |
| --- | --- |
| `bbnf/` | 8 |
| `bnf/` | 7 |
| `css_l4/` | 7 |
| `css_pretty/` | 7 |
| `csv/` | 7 |
| `ebnf/` | 7 |
| `google_sheets/` | 10 |
| `json/` | 7 |
| `math/` | 7 |
| **Total** | **67** |

Skinny mirror under `skinny/crates/runtime/src/grammars/`:

```
$ find skinny/crates/runtime/src/grammars -name '*.rs' | wc -l
48

$ find skinny/crates/runtime/src/grammars -mindepth 1 -maxdepth 1 -type d
skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media
skinny/crates/runtime/src/grammars/css_l4_declaration_values
skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended
skinny/crates/runtime/src/grammars/css_l4_nested_layout
skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors
skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules
skinny/crates/runtime/src/grammars/css_l4_visual_functions
skinny/crates/runtime/src/grammars/json
skinny/crates/runtime/src/grammars/sheets_witness
```

Sampling — 3 files inspected for hand-written vs grammar-derived markers per the brief:

1. `crates/core/src/runtime/json/arena.rs:1-26` — module header opens "AZ-I.W2.A — JSON parse arena", names `JsonValue`, `JsonPair`, `JsonArrayId`, `JsonObjectId`, `JsonArena`, `JsonStructBuilder`; documents the slab-of-Vec choice as a deliberate design call ("simplest substrate that meets the `feedback_no-orthogonal-codepaths` invariant"); 186 LOC; no `// @generated` header. **Hand-written.**
2. `crates/core/src/runtime/css_l4/builder.rs:1-30` — module header opens "AZ-I.W2-act.B3 — `CssStructBuilder` — the concrete `StructBuilder` impl that the generated CSS L4 parse function targets"; enumerates fourteen `OpenFrame` variants by grammar rule name (`stylesheet / rule / style rule / media rule / keyframe block / declaration / selector list / value list`); 1,014 LOC; no `// @generated` header. **Hand-written, per-grammar, deeply rule-aware** (the prose itself says "the deposit logic projects through grammar-specific typed values").
3. `crates/core/src/runtime/google_sheets/document/canonical.rs:1-30` — module header opens "Canonical-form serializer for [`super::SheetsDocument`]"; documents per-compound emission rules (`commas inside arg-lists`, `:` between range endpoints, parentheses around paren-expr, braces around array-literal, `;` between array rows); 411 LOC; no `// @generated` header; explicitly contrasts with a pre-W2-act tape-based emitter ("`GoogleSheetsParser::serialize_compact(node)` against the cursor-backed `tape::TapeCursor`; that emitter retired alongside the tape substrate when the struct-direct flip activated"). **Hand-written, per-grammar canonicaliser.**

Other-pattern scans:

```
# Hand-coded fake @generated headers in skinny codegen / runtime
$ grep -rln -- '@generated by skinny bbnf-codegen' skinny/crates/
skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs
skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs
skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs
skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs
skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs
skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs
skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs
skinny/crates/runtime/src/grammars/json/generated.rs
[…plus the codegen-side template + provider files that emit them]

# Codegen provider modules that fake the @generated header
$ head -50 skinny/crates/codegen/src/css_l4_visual_functions_provider.rs
…
pub(crate) fn emit_runtime_files() -> BTreeMap<String, String> {
    BTreeMap::from([
        ("config.rs".into(),
         render(include_str!("css_l4_visual_functions_templates/config.rs"))),
        ("generated.rs".into(),
         render(include_str!("css_l4_visual_functions_templates/generated.rs"))),
        ("mod.rs".into(),
         render(include_str!("css_l4_visual_functions_templates/mod.rs"))),
        ("parser.rs".into(),
         render(include_str!("css_l4_visual_functions_templates/parser.rs"))),
        ("sink.rs".into(),
         render(include_str!("css_l4_visual_functions_templates/sink.rs"))),
    ])
}

fn render(source: &str) -> String {
    let mut out = String::from("// @generated by skinny bbnf-codegen; do not edit by hand.\n");
    out.push_str(source.trim_matches('\n'));
    out.push('\n');
…
```

So every CSS L4 "provider" loads a hand-curated template via `include_str!`, prepends a fake `@generated` header at `render()`, and emits the result — there is no grammar source on the path. Across `skinny/crates/codegen/src/*_provider.rs` the per-grammar provider count is **8** (7 CSS L4 + 1 JSON), all using the same `include_str!` + fake-`@generated` shape. The `*_templates/` sister directories carrying the hand-written source bodies total **8** as well. The `.bbnf` grammars at `/grammar/css/l4/` are not opened by any of these providers; no xtask path leads from `.bbnf` → `templates/` → emitted files.

```
# Renamed-from-pre-restart / backwards-compat / shim markers
$ grep -rn 'pre[-_]restart\|legacy\|deprecated\|backward[-_ ]compat\|compat shim' \
    crates/core/src/runtime/ skinny/crates/runtime/src/ skinny/crates/codegen/src/
crates/core/src/runtime/google_sheets/parse_with.rs:29:  use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};
crates/core/src/runtime/css_l4/parse_with.rs:29:        use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};
crates/core/src/runtime/bbnf/parse_with.rs:28:        use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};
crates/core/src/runtime/json/parse_with.rs:54:        use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

# Combinator/monolithic fallback mixes
$ grep -rn 'fn parse_combinator\|combinator_fallback\|parse_with_fallback' \
    crates/core/src/runtime/ skinny/crates/runtime/src/
(no matches)

# Backend-specific code in nominally-generic crates
$ grep -rn -E '\b(Json|Css|Sheets|JsonValue|CssValue|SheetsValue|CssL4|CssPretty)\b' \
    skinny/crates/bbnf-regex/src/ skinny/crates/bbnf-simd/src/ \
    skinny/crates/simd-scan/src/ skinny/crates/parse-that-regex/src/ \
    skinny/crates/ir/src/
(no token-level matches; only an asm comment in bbnf-simd referencing
 the asmjson SOTA paper, which is bibliographic not code-coupling.)
```

The grammar-name surface in the nominally-generic crates (`bbnf-regex`, `bbnf-simd`, `simd-scan`, `parse-that-regex`, `ir`) is clean. Lock 14 violations are concentrated in `passes` + `codegen` + `runtime`, which is A3's territory; this axis records only the residual.

## §2 — Per-finding ledger

| Severity | Finding | Citation | Status |
| --- | --- | --- | --- |
| **CRITICAL** | Pattern H — 67 hand-written per-grammar runtime files across 9 directories under `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/`. Every directory ships its own hand-written `arena.rs` + `builder.rs` + `document.rs` + `value.rs` + `view.rs` ± `parse_with.rs` ± `kind.rs` ± `serialize.rs`, all without `// @generated`, all naming the grammar in the type surface (`JsonStructBuilder`, `CssStructBuilder`, `SheetsStructBuilder`, `BbnfStructBuilder`, …). No xtask + `.bbnf` source path emits these files. | `crates/core/src/runtime/json/arena.rs:1-26`; `crates/core/src/runtime/css_l4/builder.rs:1-30`; `crates/core/src/runtime/google_sheets/document/canonical.rs:1-30` (samples); category-wide. | CONFIRMS V13 |
| **CRITICAL** | Pattern H mirror — 48 hand-written per-grammar runtime files across 9 directories under `skinny/crates/runtime/src/grammars/{css_l4_at_rules_and_media, css_l4_declaration_values, css_l4_declaration_values_extended, css_l4_nested_layout, css_l4_stylesheet_selectors, css_l4_vendor_and_custom_atrules, css_l4_visual_functions, json, sheets_witness}/`. Each ships `config.rs` + `generated.rs` + `mod.rs` + `parser.rs` + `sink.rs` (CSS L4) or a superset (JSON adds `host.rs`, `scan.rs`, `value.rs`, `visitor.rs`, `view.rs`, `event_grammar_witness.rs`). | `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs:1` (`// @generated by skinny bbnf-codegen; do not edit by hand.`); `skinny/crates/runtime/src/grammars/json/generated.rs:1` (same). | CONFIRMS V13 |
| **CRITICAL** | Fake-codegen — every `skinny/crates/codegen/src/{json, css_l4_*}_provider.rs` (8 providers) uses `include_str!("…_templates/<file>")` to slurp a hand-written body and prepends `// @generated by skinny bbnf-codegen; do not edit by hand.\n` at `render()`. No grammar source (`.bbnf`) is read on this path; the `/grammar/css/l4/` directory is not opened by any of these emitters. | `skinny/crates/codegen/src/css_l4_visual_functions_provider.rs:18-50`; `skinny/crates/codegen/src/css_l4_visual_functions_templates/{config,generated,mod,parser,sink}.rs` (sister hand-written bodies); identical shape across the other 7 providers. | CONFIRMS V13 |
| **HIGH** | Backwards-compat shim — every grammar `parse_with.rs` aliases the older `Path` / `PathSegment` types from `crates/core/src/runtime/path.rs` to `LegacyPath` / `LegacySegment` at the `use` site, then lowers the newer `TypedSegment` enum onto `LegacySegment` inside `lower(...)` before invoking the document accessor. The rename is a bridge between two path representations co-existing in source; one is necessarily transitional. V13-disposition: V13 SYNTHESIS line 74 ("No backwards-compat shims that aren't legitimate refactors (Agent F Pattern G)") did not enumerate this alias surface; the alias was introduced at `0e8dbc104` (W3.2, 2026-05-02), pre-V13 close, as part of an in-flight typed-path refactor whose W3.3 cursor-threaded consumer was the planned collapse point. V14 records this as a NEW finding rather than a reversal — V13's Pattern G scan most plausibly read the W3.2 alias as a legitimate in-flight refactor under its own carveout, or did not survey the `use`-site aliases at all; V14 surveys them post-W3.2-landing and routes the collapse into C-1 PRUNE-4 (typed-path collapse) per §4. | `crates/core/src/runtime/json/parse_with.rs:54`; `crates/core/src/runtime/css_l4/parse_with.rs:29`; `crates/core/src/runtime/bbnf/parse_with.rs:28`; `crates/core/src/runtime/google_sheets/parse_with.rs:29` (4 files; identical pattern); introduction commit `0e8dbc104` (W3.2). | NEW (scope-extension over V13 Pattern G; not a reversal) |
| **HIGH** | The substrate templates explicitly enumerate hot-grammar opt-outs — `crates/core/src/runtime/builder_template.rs:13-31` documents that JSON, CSS L4, and BBNF "Distinct shape → distinct module (no template instantiation)" and ship per-grammar bodies. `arena_template.rs:1-31` documents the same shape: 5 grammars instantiate the template, the rest are hand-written. This makes the Pattern H recurrence design-of-record, not accident — and the audit cannot read CLEAN until either the hot-grammar bodies become genuine codegen output of a grammar-derived template, or the substrate doc is rewritten with a deletion plan. | `crates/core/src/runtime/builder_template.rs:13-31`; `crates/core/src/runtime/arena_template.rs:1-31`. | NEW (re-frames a V13 observation) |
| **MED** | Pre-restart code lineage — `crates/core/src/runtime/google_sheets/document/canonical.rs:13-17` documents that "Pre-W2-act this surface lived as `GoogleSheetsParser::serialize_compact(node)` against the cursor-backed `tape::TapeCursor`; that emitter retired alongside the tape substrate when the struct-direct flip activated. The struct-tree walker is the substrate-with-consumer authentic equivalent." This is a re-implementation of a pre-restart surface inside the current per-grammar runtime — the original was tape-based, the current is struct-tree-based, but the API surface is preserved. Lock-14-adjacent, but the file is in `runtime/google_sheets/` so it is properly scoped; recorded MED because the comment shows a deliberate behavioural-equivalence carry. | `crates/core/src/runtime/google_sheets/document/canonical.rs:13-17`. | NEW |
| **LOW** | One bibliographic grammar-name reference in a nominally-generic crate — `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.asm:13,16` cites the "asmjson (Lemire et al.)" SOTA paper in asm comments. Not code coupling; not a Lock 14 violation under any reasonable reading; flagged only so the synthesis can confirm A3 already disposed of it. | `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.asm:13`; `:16`. | NEW (LOW) |

Combinator/monolithic mixes: **none observed** in either `crates/core/src/runtime/` or `skinny/crates/runtime/src/`. The only `parse_dispatch` symbol is `crates/ir/tests/vm/interpreter.rs:433`, which is a test name, not a runtime fallback. CLEAN on that axis.

Backend-specific code in nominally-generic crates (`bbnf-regex`, `bbnf-simd`, `simd-scan`, `parse-that-regex`, `ir`): **none observed** beyond the asm bibliographic citation in the LOW finding above. CLEAN.

Renamed pre-restart scanners: **none observed**. CLEAN.

## §3 — Pass criterion verdict

Quoting `PASS-0-OVERFIT-AUDIT.md §Scope` row A6 verbatim:

> Zero CRITICAL Pattern H violations; every other pattern reads CLEAN.

**FAIL.** Three CRITICAL Pattern H findings hold at SK-V14 starting state — (1) 67 hand-written per-grammar runtime files under `crates/core/src/runtime/`, (2) 48 hand-written per-grammar runtime files under `skinny/crates/runtime/src/grammars/`, (3) 8 fake-codegen providers in `skinny/crates/codegen/src/` that `include_str!` hand-written templates and apply a fake `@generated` prefix. Two HIGH findings (backwards-compat `LegacyPath` shim across 4 files; substrate docs that enshrine the Pattern H opt-out for the hot grammars). One MED (pre-restart-API behaviour carry inside `google_sheets/document/canonical.rs`). One LOW (asm bibliographic citation). Combinator/monolithic mixes, backend-specific code in generic crates, and renamed pre-restart scanners are CLEAN.

The "every other pattern reads CLEAN" half of the criterion is also not met because of the HIGH backwards-compat shim. Net: **FAIL on both clauses.**

## §4 — Recommended prune actions

Cross-references to SK-V14 SYNTHESIS §3 C-1..C-5 + the PRUNE-1..5 + R4..R8 surface laid out in §2 of the dispatch context:

| Finding | Prune target |
| --- | --- |
| Pattern H — `crates/core/src/runtime/` 67 files | C-1 = PRUNE-4 (per-grammar runtime collapse onto generic substrate; substrate-template route for cohort grammars; genuine codegen for the hot grammars). The `builder_template.rs` + `arena_template.rs` substrates exist and cover the 5-grammar cohort already; PRUNE-4 must (a) instantiate the cohort grammars onto the template (eliminate the per-grammar hand bodies for BNF/CSV/EBNF/CssPretty/Math), (b) author a real `xtask regen-runtime` that emits the JSON / CSS L4 / BBNF / Sheets bodies from `.bbnf` + a registered template, (c) delete the substrate-doc passages that enshrine the opt-out. |
| Pattern H — `skinny/crates/runtime/src/grammars/` 48 files | C-1 = PRUNE-4 (skinny mirror); the skinny providers in `skinny/crates/codegen/src/` are the codegen path that must be made real. |
| Fake-codegen — 8 providers `include_str!` hand-written templates | C-1 = PRUNE-2 + R4. PRUNE-2 deletes the fake-codegen `*_templates/` bodies + the providers + the runtime mirrors; R4 wires `.bbnf` at `/grammar/css/l4/` (15 grammars, present per HANDOFF §3) through a real `cargo xtask regen-css` that consumes them and emits the runtime files. PRUNE-2 cannot land before the 24 CSS L4 fake-admit rows revert via C-5 (PRUNE-1 + PRUNE-2). |
| Backwards-compat `LegacyPath` shim (4 files) | NEW — fold into PRUNE-4 or open a small "C-6 typed-path collapse" sub-prune: pick one of `Path` / `TypedSegment` (V14 SYNTHESIS leans typed) and rewrite the 4 `parse_with.rs` files + the consumers; delete the `LegacyPath` alias surface. The rename itself is a quarter-day-of-work shim and should not survive the tranche. |
| Substrate-doc opt-out enshrinement (`builder_template.rs:13-31`, `arena_template.rs:1-31`) | NEW — fold into PRUNE-4: the substrate doc must, after PRUNE-4 lands, either describe a uniform template that covers all 9 grammars (with per-grammar profile metadata explaining the divergences) or be rewritten to declare the cohort-vs-hot split as a *single* dispatch with the hot grammars consuming a richer template, not as "five short modules + four bespoke bodies". |
| Pre-restart-API behaviour carry (`google_sheets/document/canonical.rs:13-17`) | LOW priority; fold into PRUNE-4's wave-close note. The behavioural-equivalence comment is genuine engineering provenance and can stay if the surface itself becomes generated; the comment then becomes a regen-time docstring. |
| Asm bibliographic citation | None — keep. SOTA-paper citations in asm comments are not Lock-14 or Pattern-H coupling under any reasonable reading. |

No escalation flagged beyond the campaign-wide PRUNE-1..5 + C-1..C-5 already enumerated in the SK-V14 SYNTHESIS. The two NEW findings (LegacyPath shim, substrate-doc enshrinement) extend PRUNE-4's scope by roughly half a day of work each and should be carried into the SK-V14 wave manifest's S-P3 input.
