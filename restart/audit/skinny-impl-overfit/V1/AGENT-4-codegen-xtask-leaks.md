# AUDIT-4: Codegen / Xtask Grammar-Name Leaks

Date: 2026-05-26. HEAD: `8e7378025`. Hard cap: 30 min.

Scope: Lock 14 grammar-neutral-infrastructure compliance in
`skinny/crates/codegen/`, `skinny/xtask/`, `skinny/crates/passes/`,
`xtask/`. Pattern H runtime files (`crates/core/src/runtime/<grammar>/`)
are out of scope (AUDIT-3 owns); they are referenced only for cross-axis
counts.

The user's stated discipline ("hardcoded grammars are acceptable for now to
prove >SOTA speed; generalize at the exact inflection point") is read as
permissive at the runtime layer and restrictive at the generic
infrastructure layer (codegen, xtask, cost-model, passes, ir). Lock 14
exists precisely so the "generalize later" promise is recoverable.

## Findings

### F-1 HIGH LOCK-14-LEAK — `xtask/src/main.rs` `Cmd` enum carries 9 grammar-named variants

`xtask/src/main.rs:31-89` defines:

```
RegenCss, RegenMath, RegenCsv, RegenBbnf, RegenBnf, RegenEbnf,
RegenCssPretty, RegenGoogleSheets, RegenJson
```

with 9 grammar-named `Cmd::Regen<Grammar> => ...` match arms at
`xtask/src/main.rs:100-108`, each dispatching to a hardcoded
`regen_simple_runtime::run("<grammar>")` literal. The generic `Cmd::Regen
{ grammar: Option<String>, ... }` at `xtask/src/main.rs:34-61` already
demonstrates the grammar-agnostic shape — the 9 specialized variants are
strictly per-grammar surface duplication. This is the exact "hand-coded
provider enum" topology Lock 14 v+1 (`restart/locks/LOCKS.md`
L14-HC-09) was authored to forbid. R4 W2 first-instance discipline
("regen-css is the first instance of a regen-{grammar} family") has been
shipped 9 times without ever being extracted into a template.

### F-2 HIGH LOCK-14-LEAK — `xtask/src/regen_simple_runtime.rs` grammar-named enum + dispatch

`xtask/src/regen_simple_runtime.rs:32-37`:

```
enum RuntimeStyle { Simple, TypedFormula, TypedBbnf, TypedJson }
```

with 4-arm match at `:81-84`:

```
RuntimeStyle::TypedFormula => emit_typed_formula_runtime(...),
RuntimeStyle::TypedBbnf => emit_typed_bbnf_runtime(...),
RuntimeStyle::TypedJson => emit_typed_json_runtime(...),
```

Three grammar-named emitter functions (`emit_typed_formula_runtime`,
`emit_typed_bbnf_runtime`, `emit_typed_json_runtime`) live inside the
4185-line generic emitter (`regen_simple_runtime.rs`). The enum is the
classic Lock 14 v+1 violation: the union of grammars is encoded into a
generic-crate type rather than driven by a workspace-metadata manifest.

### F-3 HIGH LOCK-14-LEAK — `xtask/src/regen.rs` BBNF-name short-circuit

`xtask/src/regen.rs:88-100`:

```
fn marker_ident(&self) -> syn::Ident {
    match self.ident.as_str() {
        "bbnf" => format_ident!("BbnfBootstrap"),
        other => { ... format_ident!("{}Parser", pascal_case(other)) }
    }
}
```

A grammar-named branch (`"bbnf" => ...`) hardcoded in the generic xtask
binary. The comment ("for the BBNF self-host this is BbnfBootstrap... the
manifest carries it implicitly") candidly admits the leak; the manifest
field is the correct receiver. This is a 1-line fix but a load-bearing
illustration that the manifest plumbing already exists and was not used.

### F-4 HIGH LOCK-14-LEAK — `skinny/crates/codegen/src/runtime_generator.rs` embeds JSON + CSS template constants

`skinny/crates/codegen/src/runtime_generator.rs:253/608/630/652`:
four `const JSON_PARSE_ONLY_GENERATED_RS / JSON_PARSE_ONLY_PARSER_RS /
JSON_MOD_RS / JSON_HOST_RS: &str` literal blobs.
Plus `CSS_GENERATED_RS / CSS_MOD_RS / CSS_PARSER_RS / CSS_SINK_RS` at
the same file (see the `normalize(CSS_*_RS)` references). The "generic"
runtime generator is the union of one JSON template + one CSS template
inlined as Rust string constants. The two are then dispatched on
`profile.mode()` (`runtime_generator.rs:19-29`): `PassCompiled` ⇒ JSON
path, `FrontendFacts` ⇒ CSS path. The mode enum *is* the grammar
branch, renamed.

The `emit_compiled` function (`runtime_generator.rs:32-79`) is JSON-only:
hardcodes `render_json_config`, `json_sink_direct::render`, and pulls
all 8 JSON file names as string literals (`"config.rs"`, `"host.rs"`,
`"value.rs"`, `"view.rs"`, `"visitor.rs"`). The "generic" entrypoint is
two grammars with shared signatures.

### F-5 HIGH LOCK-14-LEAK — `skinny/crates/codegen/src/runtime_generator.rs:114-153` per-profile CSS config table

`runtime_generator.rs:114-153`: `fn css_profile_config(profile_id: &str)`
is a 7-arm string-match selecting `(fact_schema, row_id, output_plane)`
per CSS profile id. The seven CSS L4 profile names are hardcoded:
`css_l4_declaration_values`, `..._extended`, `_stylesheet_selectors`,
`_visual_functions`, `_at_rules_and_media`, `_vendor_and_custom_atrules`,
`_nested_layout`. The plane-name suffix table (`fact_stream` /
`fact_stream_extended` / ...) is hand-coded into generic codegen. Per
Lock 14 v+1 the table should live in `[workspace.metadata.bbnf]` and be
read by the manifest loader.

### F-6 HIGH LOCK-14-LEAK — `skinny/crates/codegen/src/grammar_provider.rs:210-249` `validate_non_json_frontend_materiality`

`grammar_provider.rs:210` defines a function literally named
`validate_non_json_frontend_materiality`. It's invoked at
`grammar_provider.rs:70-72` under `if profile.mode() ==
RuntimeGenerationMode::FrontendFacts`. The name encodes the grammar
partition (JSON vs not-JSON) into the generic API; the body enumerates 8
required frontend fact buckets, all of which are properly grammar-neutral
predicates. The leak is *the function name + branch* not the predicate;
trivial rename.

### F-7 MEDIUM LOCK-14-LEAK — `skinny/crates/passes/src/lib.rs` JSON byte / literal recognizer leaks (pass-layer class e)

Per ARCHITECTURE.md §7.4 leak class (e), the pass layer carries:

- Byte whitelist at `passes/src/lib.rs:338`:
  `matches!(byte, b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"')` and
  `present.insert(b'"')` at `:345` — JSON structural alphabet hardcoded
  into the SIMD recognizer derivation.
- Role mining at `passes/src/lib.rs:1370/1373/1381`: matches `b"true"`,
  `b"false"`, `b"null"` literals to identify the bool/null rules; matches
  `b"{" b"}"` for container, `b"[" b"]"` for sequence
  (`:1388/1389/:1396/:1397`). The role mine is a JSON-shape detector
  embedded in generic codegen pass.
- Labels at `passes/src/lib.rs:1111/1131/1154`: hard-coded `"object"`,
  `"array"`, `"pair"` label strings written into MaterializationDescriptor.

These survive at HEAD verbatim per ARCH §7.4 / 3E-D10 (`LAC-2C-02`).
SK-V14 did not touch them. They are the substrate for "shape detection
disguised as grammar neutrality": works on any grammar whose
literal/byte alphabet is the JSON 7, fails silently on any other.

### F-8 MEDIUM LOCK-14-LEAK — generic-crate scan deliberately excludes the per-grammar codegen files

`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370-2379`:

```
const GENERIC_SCAN_ROOTS: &[&str] = &[
    "crates/bbnf-regex/src",
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/lower",
    "crates/codegen/src/grammar_profile.rs",
    "crates/passes/src",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/tape",
    "crates/ir/src",
];
```

Conspicuously absent: `crates/codegen/src/runtime_generator.rs`,
`crates/codegen/src/grammar_provider.rs`,
`crates/codegen/src/json_sink_direct.rs`,
`crates/codegen/src/json_typed_direct.rs`, and
`crates/codegen/src/json_templates/`. The "generic-crate neutrality"
validator therefore cannot see F-4, F-5, or F-6. The 16-element
`FORBIDDEN_GENERIC_TOKENS` list (`lock14_baseline.rs:2381-2398`)
includes `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, etc., but
those tokens are referenced 30 times across the excluded files, so the
classifier passes vacuously.

This is the most consequential structural finding: the Lock 14 gate has
been narrowed to the surface it already passes, while the actual
per-grammar codegen surface is exempted from gating.

### F-9 MEDIUM ACCEPTED — `regen_css.rs` is correctly grammar-agnostic in shape

`skinny/xtask/src/regen.rs` (185 lines) is a proper generic template:
`RuntimeTarget` struct (grammar_name + profile + entry_rule +
source_roots + output_dir + check_command + source_inputs +
metadata_inputs as `&'static str` fields), `write_targets` /
`check_target` / `runtime_request` / `validate_unique_targets` /
`workspace_metadata` are all loop-over-slice over `&[RuntimeTarget]`.
Zero grammar-named branches in `regen.rs`.

`skinny/xtask/src/regen_css.rs` (170 lines) is the CSS-named caller:
declares the 7 `RuntimeTarget` constants + 7 `pub(crate) fn
check_<profile>(...)` wrappers. The first-instance discipline (W2 exit
gate) was *honored* on the skinny side: `regen.rs` is the generic
template; `regen_css.rs` is the per-grammar call site. This is the
correct topology.

The 7 grammar-named `check_*` wrappers exist solely to bridge the CLI
match arm in `skinny/xtask/src/main.rs:23-33` (7 hardcoded match arms
on `"check-css-l4-..."` string commands). Both are eliminable by
replacing the hardcoded CLI commands with a `check --profile <id>`
generic form. Estimated effort: 50 LOC.

### F-10 MEDIUM ACCEPTED — `RuntimeProvider` enum has been ABROGATED in SK-V14

ARCHITECTURE.md §7.4 still cites "`RuntimeProvider` enum + roster" as
the (a) leak class. That enum is **gone** at HEAD. `grep -rn
RuntimeProvider skinny/crates/codegen/src/ skinny/xtask/src/` returns
zero matches. The replacement at
`skinny/crates/codegen/src/grammar_profile.rs:5-26` is a `struct
GrammarProfile { id: &'static str, generated_runtime_files: &'static
[&'static str], mode: RuntimeGenerationMode }` + 8 `static
<GRAMMAR>_PROFILE: GrammarProfile` constants + a slice loop in
`select_runtime_profile_for_name`. The 8-arm enum match has become a
slice loop with id-string compare; functional improvement, but the 8
hardcoded `static` profile entries are still per-grammar code in
generic crate.

W5C-GEN landed `b194754` ("replace provider dispatch with request
frontend generator") and W5D-DELETE landed `b39681a` ("delete provider
template residue"). The `RuntimeProvider` axis is GREEN. ARCHITECTURE.md
is stale on this row.

### F-11 LOW DOC-STALE — ARCH §7.4 leak-surface table is stale

The §7.4 table cites "8 variants + 8 roster entries + 7 arms" for
`RuntimeProvider` (gone), and the (b) row "8 modules under
`skinny/crates/codegen/`" (also gone; deleted in W5D). The numbers (c),
(d), (e) are still live but the (a) and (b) rows must be updated to
"abrogated" and a new (a') row for the residual 8 static
`GrammarProfile` constants is warranted.

## Leak Surface Census (5 classes per ARCHITECTURE §7.4)

| Leak class | Count at HEAD | Trajectory vs pre-SK-V14 | Example sites |
|---|---|---|---|
| (a) `RuntimeProvider` enum + roster | 0 (was 8 variants + 8 roster + 7 arms) | ABROGATED in W5C-GEN + W5D-DELETE | n/a |
| (a') `GrammarProfile` static constants (residual) | 8 static blocks | NEW substitute surface, lower-leak but still per-grammar | `skinny/crates/codegen/src/grammar_profile.rs:100-110` (slice) + `:100-200` (8 static blocks) |
| (b) per-grammar provider modules | 0 (was 8) | DELETED in W5D | n/a |
| (b') per-grammar template embedded constants | 2 grammar families: JSON (4 const blobs) + CSS (4 const blobs) | NEW substitute surface | `skinny/crates/codegen/src/runtime_generator.rs:253-808` (8 const &str blobs) |
| (c) Pattern H runtime grammar-named symbols | 67 hand-written files across 9 grammar dirs | Unchanged (AUDIT-3 owns) | `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets,...}/` |
| (c') parser-name leaks (30 sites baseline) | 30 (live re-run) | Unchanged | Same as (c) |
| (d) runtime-root reexport census | ~127 grammar-named idents across 47 lines in host root | Unchanged | `crates/core/src/runtime/mod.rs:25-71` (16 `pub use` blocks, ~130 named identifiers) |
| (d') skinny runtime reexports | 8 `pub mod generated_<grammar>` + 8 `pub use as <grammar>` | Unchanged from SK-V13 | `skinny/crates/runtime/src/lib.rs:3-44` |
| (e) pass-layer JSON-byte / literal recognizer leaks | 3 sites (byte whitelist + role mining + labels) + 3 literal predicates | Unchanged | `skinny/crates/passes/src/lib.rs:338, 345, 1111, 1131, 1154, 1370, 1373, 1381` |
| (f) NEW — root xtask `Cmd` enum grammar-named variants | 9 variants + 9 match arms | NEW (or untracked) | `xtask/src/main.rs:62-89, 100-108` |
| (g) NEW — root xtask `RuntimeStyle` enum + grammar-named emitters | 4 variants + 3 grammar-named emitters | NEW (or untracked) | `xtask/src/regen_simple_runtime.rs:32-37, 81-84` |
| (h) NEW — `validate_non_json_frontend_materiality` function name | 1 grammar-partition function | NEW in SK-V14 W5C-GEN | `skinny/crates/codegen/src/grammar_provider.rs:71, 210` |
| (i) NEW — `css_profile_config` 7-arm match | 7 hardcoded CSS profile ids | NEW in SK-V14 W5C-GEN | `skinny/crates/codegen/src/runtime_generator.rs:114-153` |

Net trajectory: SK-V14 W5C-GEN + W5D-DELETE retired the
`RuntimeProvider` axis (good) but spawned three new
leak-axes — `RuntimeStyle` enum in root xtask, embedded JSON+CSS
template constants in `runtime_generator.rs`, and the
`validate_non_json_*` function — that recapitulate the same pattern.
Lock 14 grep gate did not catch them because the new files are excluded
from `GENERIC_SCAN_ROOTS`.

## Grammar-named identifiers in xtask binary

| File | Count of `(Json|CssL4|CssPretty|Math|Csv|GoogleSheets|Ebnf|Bbnf|Bnf)` references |
|---|---|
| `skinny/xtask/src/main.rs` | per-grep ~50 (CLI commands + JSON_TARGET + JSON_SOURCES + 7 CSS check arms) |
| `skinny/xtask/src/regen.rs` | 0 (generic — correct topology) |
| `skinny/xtask/src/regen_css.rs` | 37 (legitimate per-grammar caller) |
| `xtask/src/main.rs` | 16 (the 9 `Cmd::Regen<Grammar>` variants + 9 dispatch arms; see F-1) |
| `xtask/src/regen.rs` | 5 (incl. F-3 BBNF branch) |
| `xtask/src/regen_simple_runtime.rs` | 64 (incl. F-2 `RuntimeStyle` enum) |
| `xtask/src/regen_css.rs` | 2 |

Total grammar-named identifiers in xtask binaries: ~174 across both
skinny + root xtask. The W2 exit-gate claim "zero grammar-named branches
in xtask itself; the `regen_css.rs` module name is the only css-named
identifier in the xtask binary" is **invalidated** at HEAD: the
`xtask/src/main.rs` `Cmd` enum has 9 grammar-named branches, the
`xtask/src/regen_simple_runtime.rs` `RuntimeStyle` enum has 3 typed
grammar-name variants + 3 grammar-named emitter functions, and the
skinny `main.rs` USAGE string carries 10 grammar-named CLI commands.

## Grammar-named identifiers in codegen lib

| File | Json/CssL4/etc references (production only) | Note |
|---|---|---|
| `skinny/crates/codegen/src/lib.rs` | ~10 (test rig only; production code is grammar-neutral) | OK in production |
| `skinny/crates/codegen/src/grammar_profile.rs` | 8 (static profile constants) | Per-grammar data, generic shape |
| `skinny/crates/codegen/src/grammar_provider.rs` | 1 function name (`validate_non_json_*`; F-6) | LEAK |
| `skinny/crates/codegen/src/runtime_generator.rs` | 8 const blobs (JSON_* + CSS_*; F-4) + 7-arm match (F-5) + `render_json_config` + `render_css_config` | LEAK |
| `skinny/crates/codegen/src/json_sink_direct.rs` | 14 `JsonSink` references | Excluded from Lock 14 grep |
| `skinny/crates/codegen/src/json_typed_direct.rs` | 11 `Json*` references | Excluded from Lock 14 grep |
| `skinny/crates/codegen/src/json_templates/` | 6 files of JSON-named types | Excluded from Lock 14 grep |

The `json_sink_direct.rs` / `json_typed_direct.rs` / `json_templates/`
are the (b') template surface — they are the "checked-in JSON
templates" the codegen splices via include_str!. They count as
per-grammar templates (Lock 14-tolerated per the W5D retention rule:
"json_templates remains until a later production-dependency proof
retires it"; `skv14-W5D-close.md`). But they have no CSS sibling
templates after W5D, so the "template per grammar" promise is half-
shipped: CSS has 4 template blobs embedded inline in `runtime_generator.rs`,
JSON has 8 files in `json_templates/`. Two grammars, two unrelated
template substrates.

## `regen-{grammar}` family status

**Skinny side (correct topology):**
- `skinny/xtask/src/regen.rs` (185 lines) — grammar-agnostic
  `RuntimeTarget` template + `write_targets/check_target/runtime_request`
  loop-over-slice. Zero grammar-named branches.
- `skinny/xtask/src/regen_css.rs` (170 lines) — CSS L4 caller with 7
  `RuntimeTarget` constants + 7 `check_*` wrappers. Wrappers exist
  *only* to bridge to hardcoded CLI strings in `main.rs:23-33`.

The first-instance discipline ("R4: regen-css is the first instance of a
regen-{grammar} family") was honored on the *generic template* axis
(`regen.rs` is a true template) but **not** on the CLI dispatch axis
(`main.rs` still names every check command explicitly: `check-css-l4-
at-rules-and-media`, `check-css-l4-declaration-values`, ...).

**Root side (broken topology):**
- `xtask/src/regen_css.rs` (1551 lines) — CSS-specific monolith.
- `xtask/src/regen_simple_runtime.rs` (4185 lines) — claims to be
  generic (`run("math")` / `run("csv")` / etc.) but encodes 4-way
  branch on grammar style in `RuntimeStyle` enum + 3 grammar-named
  emitters. The "simple runtime" framing is a euphemism for "the 8
  non-CSS runtimes share a template, except for 3 of them which need
  custom code."
- `xtask/src/regen.rs` (879 lines) — generic BBNF-bootstrap regen with
  one hardcoded `"bbnf" => BbnfBootstrap` branch (F-3).

The 5798 LOC of root xtask is the "generalize later" debt manifest. SK-V14
W6.0-W6.8 generated per-grammar root runtimes, but each rests on a
grammar-named emitter; the union of (Cmd variants + RuntimeStyle
variants + per-grammar emitters + hardcoded BBNF branch) is the
"hardcoded grammar set" the user explicitly warned against carrying
past the inflection point.

## W5 PRUNE-3 trait-dispatch landing

W5 PRUNE-3 was re-scoped mid-tranche to PRUNE-3A (source-consuming
generator contract) + PRUNE-3B (provider/template deletion), per
`restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:68-110`.

What landed:
- W5C-GEN (`b1947548`, "replace provider dispatch with request
  frontend generator") — replaced 8-arm `match RuntimeProvider`
  dispatch with `match profile.mode()` 2-arm dispatch on the
  `RuntimeGenerationMode { PassCompiled, FrontendFacts }` enum.
- W5D-DELETE (`b39681a6`, "delete provider template residue") —
  deleted the 7 CSS provider modules + 7 CSS template directories from
  `skinny/crates/codegen/src/`.

What did NOT land:
- **Trait dispatch**: the original PRUNE-3 spec ("trait dispatch +
  grammar-agnostic generator template per
  `restart/skinny/tranches/sk-v14/SPEC.md:626-684`",
  3F-migration-handoff:107). The replacement is a `mode` enum with 2
  variants — `PassCompiled` (JSON-shaped) and `FrontendFacts`
  (CSS-shaped). The 2-arm match `at runtime_generator.rs:19-29` is a
  binary grammar-family branch wearing a mode-enum hat.
- **Grammar-agnostic generator template**: W5C-GEN landed *one*
  generator function per family (JSON: `emit_compiled` at
  `runtime_generator.rs:32-79`; CSS: `emit_frontend_facts` at
  `runtime_generator.rs:81-105`). The 8 const blobs embedded as
  `&'static str` (F-4) are the templates; they are per-family hardcoded,
  not driven by per-grammar projection files.
- **The 30 parser-name leak baseline → 0 monotone-decrease**: not
  attempted. Pattern H is owned by W6.0-W6.8 (root runtime generation),
  not W5 (codegen plane).

Net W5 PRUNE-3 verdict: **partial landing**. The `RuntimeProvider`
enum is gone (good), the 8 provider modules are deleted (good), but the
trait-dispatch substitution was traded for a mode-enum + family-split
emitter pair. The new substrate has 1/4 the enum-variant leak surface
but introduced four new leak axes (F-2, F-4, F-5, F-6). Net leak
arithmetic favors W5; net grammar-neutrality story does not.

## Verdict

**LOCK-14-LEAKS-PRESENT** (mixed direction).

SK-V14 closed the (a) `RuntimeProvider` enum + (b) per-grammar provider
modules axes — both genuine improvements. Simultaneously, W5C-GEN +
W5D-DELETE spawned (b') embedded template constants, (h) `non_json`
branch function names, and (i) hardcoded CSS profile config tables.
W6.0-W6.8 spawned 9 new `Cmd::Regen<Grammar>` enum variants and the
`RuntimeStyle { TypedFormula, TypedBbnf, TypedJson }` enum in the root
xtask. The Lock 14 grep gate (`GENERIC_SCAN_ROOTS` at
`lock14_baseline.rs:2370`) was *narrowed* to a 7-path subset that
already passes, so the leaks land green.

The user's framing — "hardcoded grammars are acceptable for now to
prove >SOTA speed; we generalize at the exact inflection point" —
makes Pattern H (per-grammar runtime hand-writes, 67 files at HEAD,
AUDIT-3 axis) acceptable provisional debt. But it does **not** sanction
grammar-named enums and branch arms inside the *generic* infrastructure
(xtask binaries, codegen lib, passes lib), because the inflection-point
generalization story only works if the generic layer was kept generic.
The current state defers the inflection point further with every new
grammar added.

## Prune Recommendations

P-1 [HIGH, ~30 LOC]: collapse `xtask/src/main.rs:62-108` 9 grammar-named
`Cmd::Regen<Grammar>` variants into the existing generic
`Cmd::Regen { grammar: Option<String>, ... }`. The dispatch table at
`:100-108` becomes one line. Workspace metadata already carries the
grammar list (`Cargo.toml [workspace.metadata.bbnf.grammars]` per
`regen.rs:136-148`). Pure mechanical refactor.

P-2 [HIGH, ~200 LOC]: replace `xtask/src/regen_simple_runtime.rs:32-37`
`RuntimeStyle { Simple, TypedFormula, TypedBbnf, TypedJson }` with a
TOML-driven emitter table. The four emitter functions
(`emit_simple_runtime`, `emit_typed_formula_runtime`,
`emit_typed_bbnf_runtime`, `emit_typed_json_runtime`) become four
declarative projections in `xtask/runtime-projections/<grammar>.toml`,
read by a single emitter. This is the inflection-point generalization
the project has been deferring for 4 grammars.

P-3 [MEDIUM, 1 LOC]: rename `xtask/src/regen.rs:90` `"bbnf" =>
format_ident!("BbnfBootstrap")` to read the marker ident from
`entry.metadata.bbnf.marker` (manifest field already proposed at
ARCH 7.4; trivially added).

P-4 [HIGH, ~150 LOC]: extract the 4 inline CSS template constants in
`skinny/crates/codegen/src/runtime_generator.rs:660-808` into a
`skinny/crates/codegen/src/css_templates/` directory mirroring
`json_templates/`. Then collapse `emit_compiled` (JSON) +
`emit_frontend_facts` (CSS) into one generic `emit_from_request`
parameterized on profile.template_dir, removing the
`RuntimeGenerationMode` enum and the F-4/F-5/F-6 leak triad in one cut.

P-5 [HIGH, ~80 LOC]: rename `validate_non_json_frontend_materiality` at
`grammar_provider.rs:210` to `validate_frontend_facts_materiality` and
move the `if profile.mode() == FrontendFacts { ... }` branch to a
trait method on `GrammarProfile`.

P-6 [MEDIUM, repair-only]: widen `lock14_baseline.rs:2370-2379`
`GENERIC_SCAN_ROOTS` to include `crates/codegen/src/runtime_generator.rs`,
`crates/codegen/src/grammar_provider.rs`. This will surface F-4, F-5,
F-6 to the Lock 14 grep gate; P-1..P-5 land first so the gate passes.

P-7 [MEDIUM, doc-only]: update `restart/ARCHITECTURE.md:1283-1300`
§7.4 leak-surface table to reflect SK-V14 reality (RuntimeProvider
ABROGATED; new (b') (h) (i) substitute leaks live).

## Inflection-point assessment

The user's "inflection point" rule asks: *when is the right time to
backtrack the hardcoded grammars into a generic substrate?* For the
codegen / xtask axis, two answers:

1. **Skinny codegen** has crossed the inflection: 2 grammars (JSON +
   CSS), 8 profile ids, a 2-variant mode enum hiding the family branch,
   and 4 leak axes added during the SK-V14 attempt to *remove* the
   leaks. Every new grammar onboarded now will add a third mode-enum
   variant or split the emitter again. P-4 is the next-tranche move.

2. **Root xtask** has crossed it twice: 9 grammars, 9 `Cmd` variants, 4
   `RuntimeStyle` variants, 3 grammar-named emitter functions, 1
   hardcoded BBNF branch, 5798 LOC. Each new grammar (the user's
   stated next move is `xml`, `yaml`, `toml`) doubles the integer
   counts. P-1 + P-2 + P-3 should land as a single PRUNE-6 wave
   before any new grammar admit.

The runtime layer (Pattern H, 67 files) is owned by AUDIT-3; the user's
hardcoded-runtime indulgence applies there. The generic-infrastructure
layer (codegen, xtask, passes, ir) is exactly where Lock 14 v+1 says
"no grammar-named identifiers", and the leak count there is rising, not
falling.

## Forward-lens note for the next S-P0

The next overfit audit should:

1. Add the four NEW leak axes (F-1, F-2, F-3, F-4) to the §7.4
   enumeration so future audits inherit them.
2. Track `GENERIC_SCAN_ROOTS` coverage as a first-class Lock 14
   sub-metric: leaks outside the scan are functionally unlocked.
3. Bind the W5 PRUNE-3 partial-landing to a follow-up PRUNE-6 (or
   PRUNE-3C) wave that lands trait dispatch as originally specified
   (3F-migration-handoff:107), not the mode-enum substitution.
4. Treat every new `xtask/src/regen_<grammar>.rs` file as a finding,
   not a feature: the SPEC R4 first-instance rule was designed to
   prevent this proliferation, and at HEAD there are 3 such files
   (`regen_css.rs`, `regen_simple_runtime.rs` is a multi-grammar
   substitute) in the root xtask alone.
5. Investigate why `xtask/src/regen_simple_runtime.rs` weighs 4185
   LOC. A grammar-agnostic emitter parameterized on TOML projections
   should be <1000 LOC; the surplus is likely 3× per-grammar emitter
   bodies inlined under one filename.
