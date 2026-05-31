# AUDIT-3 — Grammar Generality (Per-Grammar Runtime/Codegen Proliferation)

**Pass:** PASS-IMPL-OVERFIT-AUDIT V3 (closing SK-V17)
**HEAD:** `f6a38445b` (SK-V17 CLOSED)
**Axis:** Is the model ONE generator emitting N grammars, or N hand-written per-grammar surfaces?
**Verdict:** **MIXED — leaning PRUNE-REQUIRED at the inflection gate.** Two divergent generator paths (JSON via grammar-derived lowering; CSS via static const-string recognizer). The 7 CSS "sub-grammars" are a **cosmetic split**: their runtime output is byte-identical. Hand-craft is admissible for the >SOTA proof, but the per-grammar directory proliferation is a contrivance that overstates grammar coverage.

---

## 1. Per-grammar runtime census

`find skinny/crates/runtime/src/grammars -name '*.rs' | wc -l` → **48 files**, across **9 directories**:

| Grammar dir | files | total LOC | path |
|---|---|---|---|
| `json/` | 11 | 2531 | `skinny/crates/runtime/src/grammars/json/` |
| `css_l4_at_rules_and_media/` | 5 | 998 | (config, generated, mod, parser, sink) |
| `css_l4_declaration_values/` | 5 | 998 | identical shape |
| `css_l4_declaration_values_extended/` | 5 | 998 | identical shape |
| `css_l4_nested_layout/` | 5 | 998 | identical shape |
| `css_l4_stylesheet_selectors/` | 5 | 998 | identical shape |
| `css_l4_vendor_and_custom_atrules/` | 5 | 998 | identical shape |
| `css_l4_visual_functions/` | 5 | 998 | identical shape |
| `sheets_witness/` | 2 | 25 | stub (`mod.rs` = one `pub mod` line) |

Effective distinct grammars: **2 real (JSON, CSS), 1 stub (Sheets).** The 7 CSS directories are 7 instantiations of ONE recognizer.

### 1a. The 7 CSS sub-grammars are byte-identical runtime [HIGH — contrivance flag]

`diff css_l4_at_rules_and_media/generated.rs <each other CSS>/generated.rs` → **0 diff lines** for all 6 comparisons. `parser.rs` and `sink.rs` likewise **0 diff lines**. The ONLY per-grammar variation is in `config.rs` — **4 diff lines**, and they are diagnostic-identity strings only:

```
css_l4_at_rules_and_media/config.rs:2  ROW_ID = "css_l4/at_rules_and_media/direct_to_struct/main"
css_l4_at_rules_and_media/config.rs:3  REQUEST_PROFILE = "css_l4_at_rules_and_media"
                          (vs)
css_l4_stylesheet_selectors/config.rs:2 ROW_ID = "css_l4/stylesheet_and_selectors/direct_to_struct/main"
css_l4_stylesheet_selectors/config.rs:3 REQUEST_PROFILE = "css_l4_stylesheet_selectors"
```

**Root cause** (`skinny/xtask/src/regen_css.rs:5-22, 41-43`): all 7 `RuntimeTarget`s share `source_roots: CSS_L4_ROOTS = ["grammar/css/l4/stylesheet.bbnf"]`, the identical `CSS_L4_SOURCES` list, and `entry_rule: "stylesheet"` (`grep "entry_rule:" → 7× "stylesheet"`). They cannot diverge because they consume the identical grammar root with the identical entry. The 7 directories × 5 files = 35 files encode ~910 unique lines once, duplicated 7×.

**Severity HIGH:** this is not contrivance in the bench-short-circuit sense, but it materially overstates "7 CSS sub-grammars admitted" — there is one CSS parser, replicated. A reader of the wave ledger would infer 7 distinct grammar surfaces; the tree holds one.

---

## 2. Generator topology — FORKED (two paths) [HIGH]

`RuntimeEmitterKind` (`skinny/crates/codegen/src/grammar_provider.rs:40-42`) is still the V1-flagged 2-variant fork:

```
enum RuntimeEmitterKind { CompiledLowering, RequestFacts }
```

Dispatch at `skinny/crates/codegen/src/runtime_generator.rs:16-26`:

- **JSON → `CompiledLowering`** → `crate::emit_from_source` → `passes::compile(&grammar)` → `lower::lower_to_rust` (`skinny/crates/codegen/src/lib.rs:107-178`). This IS grammar-derived: it lowers the parsed `.bbnf` to a `SinkOnlyProgram` and the sink renderer iterates `program.rules` / `shape.fields` (`json_sink_direct.rs:4, 47-55`).
- **CSS → `RequestFacts`** → `emit_request_facts` (`runtime_generator.rs:77-104`). The grammar facts (`facts.frontend.*`) feed ONLY `render_request_facts_config` — counts and hashes for diagnostic identity (`runtime_generator.rs:128-132`). `generated.rs`, `parser.rs`, `sink.rs`, `mod.rs` are emitted verbatim as `normalize(CSS_<X>_RS)` **static const string literals** (`runtime_generator.rs:90-94`).

### 2a. CSS is a grammar-blind hand-written recognizer baked into codegen [MEDIUM — admissible-as-proof but flag]

`CSS_GENERATED_RS` (`runtime_generator.rs:701`, ~210 lines) plus the `CssFullParser` (`runtime_generator.rs:1125-1372+`) is a hand-written CSS recognizer keyed on raw byte literals: `b'{'`, `b'}'`, `b';'`, `b'@'`, `b':'` (`runtime_generator.rs:1169, 1194, 1199, 1234, 1238, 1346, 1372`). It performs balanced-delimiter scanning (`consume_balanced_at`, `scan_value_end`) and recovers node kind from the source byte (`CssNodeKind::at_cursor`, `runtime_generator.rs:724-742`). **Nothing in this body is parameterized by the 7 sub-grammars' rules.** It is one CSS parser, grammar-independent.

Per the pass discipline (§5: "Hand-crafted parsers are acceptable during >SOTA proof"), the existence of a hand-written CSS recognizer is **admissible**. The contrivance flag is not the hand-craft — it is (a) the 7× directory replication masquerading as 7 grammars (§1a), and (b) that this hand-written recognizer lives **inside codegen as a const string**, not as a grammar-driven emission. The `RequestFacts` emitter does not generate CSS from CSS — it copies a fixed CSS parser and stamps a config header.

### 2b. JSON's "generated" code is also part-template, part-derived [LOW]

JSON's `generated.rs` is `include_str!("json_templates/generated.rs")` + `JSON_PARSE_ONLY_GENERATED_RS` const + the sink rendered from the lowered program (`runtime_generator.rs:33-66`). The dispatch shell is parametric over `program.rules`, but leaf renderers (`render_container_rules`, `render_string_rule`, `render_number_rules`, `json_sink_direct.rs:251, 326, 11-14`) emit hardcoded `parse_object_direct` / `JsonSink`-named bodies as string literals (`json_sink_direct.rs:251-260`). So even the "grammar-derived" path is a hybrid: grammar-shaped dispatch, hand-written JSON leaves. This is more honest than CSS (the dispatch genuinely reflects the grammar) but still not a pure ".bbnf → parser" generator.

---

## 3. @generated headers + regen discipline [PASS]

- All per-grammar runtime files carry true generator-owned headers: `// @generated by skinny bbnf-codegen; do not edit by hand.` (`css_l4_*/generated.rs:1`, `css_l4_*/mod.rs:1`, `json/generated.rs:1`). Header originates from `crate::GENERATED_HEADER` (`runtime_generator.rs:121`) — not a hand-faked literal.
- Regen round-trips clean: working tree is **clean** vs HEAD (`git diff HEAD --stat` on the CSS generated files → empty; the initial git-status snapshot listing them modified was stale). The 7 CSS targets and JSON target are driven by `xtask/src/regen.rs::write_targets` (`regen.rs:21-37`) and `check_target` (`regen.rs:38-42`) through the single `codegen::emit_runtime_from_request` entry. No hand-patches detected.
- `validate_unique_targets` (`regen.rs:90`) and `validate_generated_roster` (`runtime_generator.rs:69, 96`) enforce roster integrity.

**Verdict: regen discipline is honest.** The headers are real and the round-trip is clean. The problem is upstream (what the generator emits), not the regen plumbing.

---

## 4. Other-grammar coverage [MEDIUM]

- **Sheets:** `sheets_witness/mod.rs` is a 1-line stub (`pub mod event_grammar_witness;`), 25 LOC total. NOT grammar-derived, NOT a runtime surface — a witness placeholder. The W2 ledger's "Sheets/BBNF-self by-construction under SK-V18" is borne out: no real Sheets runtime exists yet.
- **BBNF-self, math, csv:** absent from `crates/runtime/src/grammars/`. No runtime surfaces.
- The tape/`ValueRef` model is **JSON+CSS-only**. CSS's `CssNode`/`ValueRef` lazy view (`runtime_generator.rs:744-790`) is explicitly described as "isomorphic to JSON's `value_from_ref`" — i.e. hand-ported per grammar, not auto-derived. There is no general "emit tape view from grammar" mechanism; each grammar's tape view is hand-written.

`crates/runtime/src/lib.rs` registers **8** `generated_*` modules (`grep -c "pub mod generated_"` → 8: json + 7 css). The benched surface is exactly the 2-real / 7-replica / 1-stub population.

---

## 5. INFLECTION-POINT assessment

**Is this axis at the grammar-driven generalization inflection point? NO — PRUNE FIRST.**

The two >SOTA grammars (JSON, CSS) are proven, but the path to "ONE generator emitting all grammars from `.bbnf`" is **not yet structurally present**. The current topology is two forked emitters where:

- `CompiledLowering` (JSON) is the *closer* of the two to general: it lowers `.bbnf` → `SinkOnlyProgram` → rendered dispatch. Its leaf renderers are still hand-written JSON, but the spine is grammar-shaped.
- `RequestFacts` (CSS) is **not a generator at all** — it is a const-string courier that copies a fixed hand-written CSS parser and stamps a per-profile config header. It cannot emit a different grammar; pointing it at a non-CSS `.bbnf` would still emit the CSS parser.

### Concrete structure of the SK-V18 generalization target

To reach ONE grammar-driven generator, the structural moves are:

1. **Collapse the 7 CSS replicas to 1.** Since all 7 share root/entry/sources and emit byte-identical runtime, the 7 `RuntimeTarget`s (`regen_css.rs`) should be ONE target, or the 7 profiles should consume **distinct** `.bbnf` roots (the `grammar/css/l4/*.bbnf` modules exist — `color.bbnf`, `media.bbnf`, `selectors.bbnf`, etc.) with distinct entry rules so the output genuinely diverges. Today they all point at `stylesheet.bbnf` (`regen_css.rs:23`). Either collapse (honest: 1 CSS grammar) or differentiate (honest: 7 grammars) — the current middle state (7 dirs, 1 output) is the contrivance.

2. **Unfork the emitters.** `RuntimeEmitterKind` should become ONE path. CSS must route through `CompiledLowering`-style grammar lowering so its parser is *emitted from* `stylesheet.bbnf`, not couriered as `CSS_GENERATED_RS`. This requires the lowering pipeline to express CSS's balanced-delimiter recognizer as a grammar-derived emission (the `BackendRule` branch-tag flag the CSS header already references suggests the IR vocabulary exists; it is not yet driving emission).

3. **Generalize the tape-view emission.** Both JSON `value_from_ref` and CSS `CssNode::at_cursor` are hand-ported "lazy view over the tape." A general grammar→tape-view emitter (driven by the grammar's node-kind set) would eliminate the per-grammar hand-port. This is the structural prerequisite for adding Sheets/BBNF-self without writing a third hand `ValueRef` view.

4. **De-stub Sheets** as the first proof that the generalized path (2+3) emits a *new* grammar with zero hand-written runtime — the litmus test for the inflection.

Until (1) and (2) land, "grammar-driven generalization" is not demonstrable: the campaign has 2 hand-shaped parsers behind a 2-way fork, with one of them (CSS) replicated 7× to inflate the grammar count.

---

## 6. Prune / course-correct recommendations

| # | Action | Path:line | Severity |
|---|---|---|---|
| P1 | Collapse the 7 byte-identical CSS targets to 1 (or differentiate their `source_roots`/`entry_rule` so output truly diverges). The 7-dir / 1-output split overstates grammar coverage. | `skinny/xtask/src/regen_css.rs:34-...` (TARGETS), `regen_css.rs:23` CSS_L4_ROOTS | HIGH |
| P2 | Replace the `RequestFacts` const-string courier with grammar-derived CSS emission via the `CompiledLowering` lowering pipeline; retire `CSS_GENERATED_RS`/`CSS_PARSER_RS`/`CSS_SINK_RS`/`CSS_MOD_RS` const literals. | `runtime_generator.rs:598, 612, 665, 701`; `grammar_provider.rs:40-42` | HIGH |
| P3 | Generalize JSON+CSS hand-written leaf renderers / tape views into a grammar-parametric emitter (single source for `value_from_ref` ≅ `CssNode::at_cursor`). | `json_sink_direct.rs:251, 326`; `runtime_generator.rs:724-790` | MEDIUM |
| P4 | De-stub `sheets_witness` as the generalization litmus, or remove it from the tree until SK-V18 so it does not read as coverage. | `crates/runtime/src/grammars/sheets_witness/mod.rs:1` | LOW |

**Keep (do NOT prune):** the `@generated` headers, the `validate_unique_targets`/`validate_generated_roster` roster gates, and the regen `write_targets`/`check_target` round-trip — these are honest and load-bearing (§3).

---

## 7. Forward-lens note (for SK-V18 S-P0)

The next cycle's spec audit should add a CH-procedural addendum: **"distinct-grammar-output gate"** — for any tranche claiming N grammars admitted, S-P0 must verify the N runtime `generated.rs` are NOT byte-identical (a `diff` census). A grammar count is only real if the emitted parsers differ. SK-V17 admitted "7 CSS sub-grammars" whose runtime is one parser replicated 7×; an automated diff-census in the gate would have caught the replication. Pair this with a **"single-emitter-path"** lens: flag any `RuntimeEmitterKind` proliferation where one variant is a const-string courier rather than a grammar lowering — the courier variant cannot generalize and is a standing contrivance until folded into the lowering path.
