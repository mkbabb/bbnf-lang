# AY-III — Agent Dispatch Templates

Per-wave dispatch boilerplate composed against
`docs/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md`. Thirteen
templates total: 4 in W0 (admission totality, competitor lane,
samply + nm, wire-contract); 5 in W1 (CSS audit, CSS materialiser
regen, CSS parity-test tighten, Sheets parity, CSS+Sheets samply +
AZ-I baseline); 4 in W2 (self-host identity, BBNF totality, BBNF
samply + bench, close ceremony).

Each template carries an explicit allow-list, forbidden-list,
hard-gate items, and verification methodology. Substitute
`{WORKTREE_PATH}` per dispatch. The orchestrator pre-creates
worktrees; sub-agents never share write access to the same file
within a wave.

## W0 templates (JSON closure)

### Template W0.a — JSON admission totality audit + missing-projection wiring

```
You are sub-agent W0.a for tranche AY-III. AY-III is the gestalt
continuation pass on the post-B5 substrate; W0 owns JSON closure.
Your job: audit JSON admission surface against the post-B5
generated source, author `crates/core/tests/json_admission_totality.rs`
asserting 1:1:1 admission ↔ materialiser ↔ consumer wire contract,
and verify totality holds on the post-B5 substrate.

## Worktree (ABSOLUTE ROOT — all work here)

`{WORKTREE_PATH}` (orchestrator-supplied; typical
`/Users/mkbabb/Programming/bbnf-wt-ay-iii-w0-a`).

Never leave that directory. Never touch
`/Users/mkbabb/Programming/bbnf-lang` — that is the orchestrator's
main checkout. `target/` symlinks to main; `data/` is seeded.

## Memory discipline (non-negotiable)

Before every cargo invocation:

    export CARGO_BUILD_JOBS=4

Prefer `cargo {test,check} --profile ax-iter` during iteration.
Never run two cargo invocations concurrently inside the same
target directory.

## Read first (required, in order)

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md` — §Hard gates,
   §Runtime-evidence, §Activation-gate, §Scope-reveal.
3. `docs/tranches/AY-III/AY-III.md` — invariants + wave
   summary.
4. `docs/tranches/AY-III/waves/W0.md` — your wave spec
   (W0.a sub-phase).
5. `grammar/json/json.bbnf`.
6. `target/expand/ay-json.rs` (post-B5 expand).

## Scope — W0.a only

1. Read `grammar/json/json.bbnf`; enumerate every `-> Type`
   admission and every rule whose payload-layout admits
   direct-to-struct.
2. Cross-reference against post-B5 `ay-json.rs` expand;
   confirm per-admission `materialize_projection_*_JsonParser`
   fn emission, call site in `project_value_JsonParser`, and
   `JsonParserValue` variant coverage.
3. Author `crates/core/tests/json_admission_totality.rs`.
4. Assert admission count == materialiser count == consumer
   count via emitted const length, never hard-coded numbers.

Do NOT touch items outside W0.a. Sibling agents own
neighbouring scope (W0.b benches; W0.c samply; W0.d
parity-test).

## File bounds

Allow-list:
- `crates/core/tests/json_admission_totality.rs` — create.

Forbidden:
- Any emitter source (`crates/core/src/backend/**`).
- Any runtime source (`crates/core/src/runtime/**`).
- `crates/{ir,tape}/**`.
- `grammar/**/*.bbnf` (per AY-III invariant 8 — no new
  features).
- Any other test file.
- Any bench file.
- `crates/core/src/grammar/generated/json.rs` (orchestrator
  regen).

## Hard gate

1. `cargo test -p bbnf --test json_admission_totality
   --profile ax-iter` exit 0.
2. `grep -c 'materialize_projection_.*_JsonParser'
   target/expand/ay-json.rs` equals
   `<JsonParser>::PROJECTION_DIRECT_TO_STRUCT.len()`.
3. Workspace nextest 1477+/1477+ green at sub-phase close.

## Commit discipline

- Commit at every natural milestone, not at wave end.
- NEVER commit `crates/core/src/grammar/generated/*.rs`
  unless you own the regen window (you do NOT).

Commit message template:

    test(core): add json_admission_totality wire contract (AY-III.W0.a)

    Asserts admission count == materialiser count == consumer
    count for JsonParser. Grammar-derived; no hard-coded
    expected numbers. Verifies the post-B5 substrate's
    direct-to-struct path holds totality on JSON.

    Evidence: pass log + cargo expand slice path.

## Return format

≤ 300 words. Dense technical reporting only. Include:

1. Commit SHA(s) with one-line descriptions.
2. Admission inventory: per-rule (name, type, materialiser,
   consumer).
3. Hard-gate status table — exit status + artefact path per
   gate item.
4. Any deviation from this spec with rationale.
5. `git status --short` (must be empty or contain only
   `target/` symlink).

No narrative filler. No "I then ran" prose.

## Non-negotiables

- No stubs, no fallbacks, no feature flags, no `#[ignore]` /
  `#[allow(dead_code)]` introduced.
- Runtime evidence per AY-III invariant 4; grep alone is
  insufficient.
- If scope-reveal surfaces, halt and report per SPEC
  §Scope-reveal — do not silently ship a partial fix.

## HARD CAP: 30 min (redress role)

At 0.9× cap (27 min) commit deliverable; at cap halt.

Begin.
```

### Template W0.b — Competitor bench extension + value-lane close capture

```
You are sub-agent W0.b for tranche AY-III. W0 owns JSON closure.
Your job: extend `crates/core/benches/json/competitors.rs` with
a `bench_bbnf` group across all 5 fixtures; capture fat-LTO
value-lane numbers; publish per-fixture and geomean ratios
versus sonic-rs / simd-json / 7 other competitors.

## Worktree

`{WORKTREE_PATH}`. Never leave that directory.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md` (§Bench contract,
   §Three-tier command surface).
3. `docs/instructions/PROFILING.md`.
4. `docs/tranches/AY-III/AY-III.md`.
5. `docs/tranches/AY-III/waves/W0.md` (W0.b sub-phase).
6. `crates/core/benches/json/{competitors,value,monolithic}.rs`.

## Scope — W0.b only

1. Add `bench_bbnf` group mirroring the 9 existing competitor
   groups; per-fixture entries (`bbnf_data`, `bbnf_twitter`,
   `bbnf_citm`, `bbnf_canada`, `bbnf_data_xl`).
2. Capture fat-LTO numbers via `cargo bench --profile bench`.
3. Post-process value-lane output into
   `docs/benchmarks/post-AY-III-W0-eager.json` with per-fixture
   `bbnf_value_<fx> / sonic_value_<fx>` ratios + 5-fixture
   geomean.
4. Post-process competitors output into
   `docs/benchmarks/post-AY-III-W0-bbnf-value-ratios.txt`
   tabulating bbnf vs each competitor per fixture.

## File bounds

Allow-list:
- `crates/core/benches/json/competitors.rs` (modify).
- `crates/core/benches/json/value.rs` (modify only if
  `black_box` discipline needs touch-up).
- `crates/core/benches/json/monolithic.rs` (modify only if
  `black_box` discipline needs touch-up).
- `docs/benchmarks/post-AY-III-W0-eager.json` (create).
- `docs/benchmarks/post-AY-III-W0-competitors-json.txt`
  (create).
- `docs/benchmarks/post-AY-III-W0-monolithic.txt` (create).
- `docs/benchmarks/post-AY-III-W0-bbnf-value-ratios.txt`
  (create).

Forbidden:
- Any emitter / runtime / IR / tape source.
- Any test file.
- Any non-JSON bench.
- `Makefile`, `.cargo/config.toml`, `scripts/**`.

## Hard gate

1. `cargo bench --profile bench -p bbnf --bench competitors
   bench_bbnf` runs clean with per-fixture numbers.
2. `post-AY-III-W0-eager.json` valid JSON with
   `bbnf_value_<fx>`, `sonic_value_<fx>`, `ratio` keys for all
   5 fixtures + `geomean_bbnf_over_sonic`.
3. `post-AY-III-W0-competitors-json.txt` carries 10 lanes
   (bbnf + 9 external) × 5 fixtures.

## Return format

≤ 500 words. Include:

1. Commit SHA(s).
2. Per-fixture ratio table (twitter / canada / citm / data /
   data_xl bbnf_value vs sonic_value + geomean).
3. Hard-gate status table.
4. Any deviation with rationale.
5. `git status --short`.

## HARD CAP: 30 min (redress role)

Begin.
```

### Template W0.c — Samply per-fixture attribution + nm verification

```
You are sub-agent W0.c for tranche AY-III. W0 owns JSON closure.
Your job: capture samply profiles for all 5 JSON fixtures under
`profiling-prep`; resolve symbols via nm; verify retired-surface
absence and post-B5 surface presence; post-process top-5
self-time per fixture into a summary artefact.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/PROFILING.md` — full read; §samply
   symbol resolution.
3. `docs/instructions/tranche/SPEC.md` (§Three-tier command
   surface).
4. `docs/tranches/AY-III/AY-III.md`.
5. `docs/tranches/AY-III/waves/W0.md` (W0.c sub-phase).
6. `docs/tranches/B5/FINAL.md` §API surface changes (post-B5
   surface symbols).

## Scope — W0.c only

1. `make ay-prepare-profile-wave WAVE=III-W0` produces the
   `profiling-prep` JSON bench binary.
2. Drive interactive samply per fixture (NOT `--save-only`
   per `feedback_samply_symbols`); five profiles under
   `.profiles/samply/AY-III-W0/json/<fixture>/profile.json.gz`.
3. Resolve symbols via nm; capture
   `post-AY-III-W0-nm-json.txt`.
4. Assert retired-surface absence (post-B5 surface map):
   `note_push`, `value_frame_at`, `value_payload_for`,
   `value_children`, `value_payload_narrow`,
   `value_payload_wide`, `value_frame_index_at`,
   `value_open_at`, `navigate_tape`, `push_compound`
   (emitter-call-graph), `ValueBuilder::*`, `FusedBuilder<R>`,
   `FusedOutput<R>`, `ValueFramesOutput<R>`,
   `parse_with_visitor_JsonParser`.
5. Assert post-B5 surface presence: `Tape::push_leaf_*`,
   `Tape::begin_compound`, `Tape::end_compound`,
   `Tape::end_compound_post_order`, `Tape::position`,
   `Tape::rollback_to`.
6. Post-process top-5 self-time per fixture into
   `post-AY-III-W0-samply-summary-json.txt`; explicit
   `parse_with_visitor` attribution check (must be 0).

## File bounds

Allow-list:
- `.profiles/samply/AY-III-W0/json/{data_s,twitter,citm,canada,data_xl}/`
  (populate).
- `docs/benchmarks/post-AY-III-W0-nm-json.txt` (create).
- `docs/benchmarks/post-AY-III-W0-samply-summary-json.txt`
  (create).

Forbidden:
- Any source code.
- Any test or bench source (W0.b owns benches; W0.a owns
  totality test; W0.d owns parity tests).

## Hard gate

1. Five samply profiles exist with non-zero sample count.
2. Twitter top-5 names emitted `parse_*_JsonParser_*` + at
   least one `Tape<R>` symbol.
3. nm: every retired-surface symbol absent per the W0.md
   §AY-III.W0.c list; every post-B5 surface symbol present.
4. No samply profile attributes > 0.5% to any retired-surface
   symbol.

## Return format

≤ 500 words. Include:

1. Per-fixture top-5 self-time table.
2. nm absence/presence audit table.
3. Hard-gate status table.
4. Any deviation with rationale.
5. `git status --short`.

## HARD CAP: 30 min (redress role)

Begin.
```

### Template W0.d — Parity-test tightening + fused-pipeline wire contract

```
You are sub-agent W0.d for tranche AY-III. W0 owns JSON closure.
Your job: tighten three JSON parity tests on the post-B5
substrate; un-ignore `beat_sonic_twitter_eager` under release;
author `json_fused_pipeline_parsecount.rs` asserting parse-root
invocation count equals `to_value()` invocation count.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md`.
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/waves/W0.md` (W0.d sub-phase).
5. `crates/core/tests/{sonic_rs_parity,json_canonical_parity,
   value_api_apples_to_apples,json_value_parity,json_parity,
   json_decode,json_slab}.rs`.
6. `docs/tranches/B5/FINAL.md` (post-B5 substrate surface).

## Scope — W0.d only

1. `sonic_rs_parity.rs` — bbnf side uses `Parsed::to_value()`
   directly via `<JsonParserValue as Into<RefValue>>` (or
   equivalent emitted projection); remove tape-text re-parse.
2. `json_canonical_parity.rs` — add 3-way oracle check
   (sonic-rs + simd-json + serde_json); reject divergence.
   Keep `cfg_attr(debug_assertions, ignore)` on
   `canonical_parity_data_xl`.
3. `value_api_apples_to_apples.rs` — un-ignore
   `beat_sonic_twitter_eager` under release at ≤ 1.50 sanity
   floor; comment cites `post-AY-III-W0-eager.json` as the
   hard-gate artefact.
4. Author `json_fused_pipeline_parsecount.rs` —
   `#[cfg(test)]` atomic counter or post-B5 `Tape<R>`
   construction count; assert `parse_value_JsonParser_value`
   called exactly once per `Parsed::to_value()`.

## File bounds

Allow-list:
- `crates/core/tests/sonic_rs_parity.rs` (modify).
- `crates/core/tests/json_canonical_parity.rs` (modify).
- `crates/core/tests/value_api_apples_to_apples.rs` (modify).
- `crates/core/tests/json_value_parity.rs` (modify if
  needed).
- `crates/core/tests/json_parity.rs` (modify if needed).
- `crates/core/tests/json_decode.rs` (modify if needed).
- `crates/core/tests/json_slab.rs` (modify if needed).
- `crates/core/tests/json_fused_pipeline_parsecount.rs`
  (create).

Forbidden:
- Any source code.
- Any non-JSON test.
- Any bench.

## Hard gate

1. `cargo test -p bbnf --test sonic_rs_parity --release`
   green.
2. `cargo test -p bbnf --test json_canonical_parity
   --release` green (data_xl passes under release).
3. `cargo test -p bbnf --test value_api_apples_to_apples
   --release` green including `beat_sonic_twitter_eager` at
   ≤ 1.50.
4. `cargo test -p bbnf --test json_fused_pipeline_parsecount
   --release` green on twitter + canada + citm.
5. Zero new `#[ignore]` lands.

## Return format

≤ 500 words.

## HARD CAP: 30 min

Begin.
```

## W1 templates (CSS L4 + Sheets + AZ-I baseline)

### Template W1.a — CSS L4 lightningcss audit + grammar annotation

```
You are sub-agent W1.a for tranche AY-III. W1 owns CSS L4
typed parity + Sheets + AZ-I.W0 baseline absorption. Your job:
walk lightningcss 1.0.0-alpha.71 source under the cargo registry;
produce `docs/tranches/AY-III/waves/W1-lightningcss-audit.csv`
mapping every AST surface to a grammar rule with EXISTS / PARTIAL
/ MISSING / OUT-OF-SCOPE state; land grammar `-> TypeName`
annotations for EXISTS rows; create
`grammar/css/l4/at-rules.bbnf` for MISSING rows under the
admission-already-shapes-rule discipline (per AY-III invariant 8,
no new features beyond annotations on already-admitted rules).

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md` (§Activation-gate
   rule, §Scope-reveal).
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/waves/W1.md` (W1.a sub-phase).
5. `grammar/css/l4/{stylesheet,properties,color,selectors,
   media,value-unit,func-body,values,gradients,transforms,
   filters,easing,keywords}.bbnf`.
6. `~/.cargo/registry/src/index.crates.io-*/lightningcss-1.0.0-alpha.71/src/{rules,values,properties,selector,media_query}*.rs`.

## Scope — W1.a only

1. Produce `W1-lightningcss-audit.csv` with columns
   `lightningcss_type, lightningcss_path, grammar_rule,
   grammar_file_line, state`.
2. EXISTS rows: add `-> TypeName` annotation to existing
   grammar rule.
3. MISSING rows where the rule already exists under
   `genericAtRule`: extend the existing rule's annotation;
   create `at-rules.bbnf` only when an admission is needed.
4. PARTIAL rows: extend the rule body to add the typed
   alternative.
5. OUT-OF-SCOPE rows: `grep -c` evidence in the audit CSV
   (zero matches in `data/css/{normalize,bootstrap,tailwind}.css`);
   `genericAtRule` fallback admits token-for-token.

Bounded grammar carve-out per AY-III invariant 8: only land
annotations / extensions where an admission already shapes the
rule. No new features.

## File bounds

Allow-list:
- `grammar/css/l4/stylesheet.bbnf` (modify).
- `grammar/css/l4/properties.bbnf` (modify).
- `grammar/css/l4/color.bbnf` (modify).
- `grammar/css/l4/selectors.bbnf` (modify).
- `grammar/css/l4/media.bbnf` (modify).
- `grammar/css/l4/value-unit.bbnf` (modify).
- `grammar/css/l4/func-body.bbnf` (modify).
- `grammar/css/l4/values.bbnf` (modify).
- `grammar/css/l4/gradients.bbnf` (modify).
- `grammar/css/l4/transforms.bbnf` (modify).
- `grammar/css/l4/filters.bbnf` (modify).
- `grammar/css/l4/easing.bbnf` (modify).
- `grammar/css/l4/keywords.bbnf` (modify).
- `grammar/css/l4/at-rules.bbnf` (create).
- `docs/tranches/AY-III/waves/W1-lightningcss-audit.csv`
  (create).

Forbidden:
- Any non-CSS grammar file.
- Any source code (W1.b owns IR + emitter; W1.c owns parity
  tests).
- Any bench.
- `crates/core/src/grammar/generated/css_l4.rs` (orchestrator
  regen).

## Hard gate

1. `W1-lightningcss-audit.csv` committed; every row carries
   state + (OUT-OF-SCOPE) `grep` evidence.
2. `cargo iter-check` exit 0 post-annotation.
3. `grep -c 'pub struct CssL4Parser\w*Projection'
   target/expand/ay-css-l4.rs` ≥ 80 (pre-W1: 48 per AUDIT-B).
4. `cargo test -p bbnf --test css_l4_parity --release` green
   (annotations do not perturb admission).
5. No `.bbnf` edit touches a rule outside `grammar/css/l4/**`.

## Return format

≤ 500 words.

## HARD CAP: 30 min

Begin.
```

### Template W1.b — CSS L4 IR layout + materialiser regen

```
You are sub-agent W1.b for tranche AY-III. W1 owns CSS typed
parity. Your job: extend `crates/ir/src/passes/payload/layout.rs`
to admit the W1.a annotations (recursive Alt → variant-tag u8 +
arena-handle pattern; repeat-of-complex → arena slab handle;
per-group `PropertyProjection` u8 dispatch); retire
`__named_type_shim_color` via `view/named_types.rs` carve;
emit `materialize_projection_*_CssL4Parser` per admission via
`shapes/value_materialize.rs`.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md`.
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/waves/W1.md` (W1.b sub-phase).
5. `crates/ir/src/passes/payload/layout.rs`.
6. `crates/core/src/backend/rust/view/named_types.rs`.
7. `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`.

## Scope — W1.b only

1. Layout-pass extensions (grammar-structure-driven):
   - Selector / SelectorList / RelativeSelectorList:
     repeat-of-complex → `CssL4ParserSelectorProjection {
     components: (arena_offset, arena_len) }`.
   - MediaCondition / ContainerCondition / SupportsCondition:
     variant-tag u8 + arena-handle.
   - Length::Calc / mathExpr nesting: arena-handle pattern.
   - Property<'i> per-group: 27 typed-decl groups emit
     `CssL4Parser<Group>PropertyProjection { variant_idx: u8,
     value: arena_handle }`.
2. Retire `__named_type_shim_color` at `view/named_types.rs`;
   route emitter to consume the W0' / B5-folded peel-derived
   shape directly.
3. Author `crates/core/tests/css_admission_totality.rs` — CSS
   1:1:1 wire-contract; admission count == materialiser count
   == consumer count.

## File bounds

Allow-list:
- `crates/ir/src/passes/payload/layout.rs` (modify).
- `crates/core/src/backend/rust/view/named_types.rs`
  (modify-carve).
- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
  (modify).
- `crates/core/tests/css_admission_totality.rs` (create).

Forbidden:
- Any grammar file (W1.a owns).
- Any non-CSS test (W1.c W1.d own).
- Any bench (W1.e owns).
- `crates/core/src/grammar/generated/css_l4.rs` (orchestrator
  regen).
- `crates/tape/**` (post-B5 closed substrate).

## Hard gate

1. `cargo iter-check` exit 0.
2. `cargo test -p bbnf --test css_admission_totality
   --profile ax-iter` green.
3. `cargo test -p bbnf --test named_type_preservation
   --profile ax-iter` green.
4. `grep -c '__named_type_shim' target/expand/ay-css-l4.rs`
   → 0.
5. `grep -c 'pub struct CssL4Parser\w*Projection'
   target/expand/ay-css-l4.rs` ≥ 80;
   `grep -c 'fn materialize_projection_\w*_CssL4Parser'`
   equals projection count.
6. `grep 'static.*NamedTypeBinding\|const.*NamedTypeBinding\|&\[NamedTypeBinding\b'
   crates/core/src/backend/rust/view/named_types.rs` empty.

## Return format

≤ 500 words.

## HARD CAP: 30 min

Begin.
```

### Template W1.c — CSS L4 parity-test tightening

```
You are sub-agent W1.c for tranche AY-III. W1 owns CSS typed
parity. Your job: extend `lightningcss_parity.rs` with
`color_channel_parity_all_families`, `selector_parity`,
`media_query_parity`, `property_parity`; tighten
`typed_accessor_surface.rs` floors to exact post-W1 counts;
extend `css_l4_canonical_parity.rs` corpus under
`data/css/lightningcss-corpus/` with ≥ 15 hand-authored
fixtures.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md`.
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/waves/W1.md` (W1.c sub-phase).
5. `crates/core/tests/{lightningcss_parity,css_l4_canonical_parity,
   typed_accessor_surface,css_l4_parity,css_l4_named_color_parity}.rs`.

## Scope — W1.c only

1. `lightningcss_parity.rs`:
   - Extend `color_channel_parity_rgb_family` to
     `color_channel_parity_all_families` (RGB + LAB/LCH/
     OKLab/OKLCH + Predefined + Float + CurrentColor).
   - New `selector_parity` (10 inline fixtures).
   - New `media_query_parity` (5 fixtures).
   - New `property_parity` (20-decl inline CSS).
2. `typed_accessor_surface.rs`: tighten `r.aggregate >= 12`,
   `r.leaf_scalar >= 60`, `r.alt >= 25` floors (or post-W1
   exact).
3. `css_l4_canonical_parity.rs`: ≥ 15 fixtures under
   `data/css/lightningcss-corpus/` with one
   `canonical_parity_<feature>` test each.
4. `css_l4_parity.rs` + `css_l4_named_color_parity.rs`
   assertions track 148 named colors.
5. Zero `#[ignore]` admitted.

## File bounds

Allow-list:
- `crates/core/tests/lightningcss_parity.rs` (modify).
- `crates/core/tests/css_l4_canonical_parity.rs` (modify).
- `crates/core/tests/typed_accessor_surface.rs` (modify).
- `crates/core/tests/css_l4_parity.rs` (modify).
- `crates/core/tests/css_l4_named_color_parity.rs` (modify).
- `crates/core/tests/common/css_normalize.rs` (modify).
- `data/css/lightningcss-corpus/` (create).

Forbidden:
- Any grammar / source / bench (W1.a / W1.b / W1.e own).
- Any non-CSS test (W1.d owns Sheets).

## Hard gate

1. `cargo test -p bbnf --test lightningcss_parity --release`
   green; new tests green; zero `#[ignore]`.
2. `cargo test -p bbnf --test css_l4_canonical_parity
   --release` green.
3. `cargo test -p bbnf --test typed_accessor_surface
   --profile ax-iter` green; counts match post-W1 floors.
4. `cargo test -p bbnf --test css_l4_parity --release` +
   `--test css_l4_named_color_parity --release` green.
5. `grep -rn '#\[ignore\]' crates/core/tests/{lightningcss_parity,
   css_l4_canonical_parity,typed_accessor_surface,
   css_l4_parity,css_l4_named_color_parity}.rs` empty.

## Return format

≤ 500 words.

## HARD CAP: 30 min

Begin.
```

### Template W1.d — Sheets parity + fat-LTO panic verification

```
You are sub-agent W1.d for tranche AY-III. W1 owns CSS + Sheets +
AZ-I baseline. Your job: tighten `sheets_parity.rs`,
`sheets_expr_parity.rs`, `sheets_self_parity.rs` for fused-
pipeline truth on the post-B5 substrate; add ≥ 9
`SheetsGValue` field-for-field assertions in
`sheets_expr_parity`; author `sheets_parse_nested_no_panic`
sentry; verify Sheets projection totality holds 1:1:1.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md`.
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/waves/W1.md` (W1.d sub-phase).
5. `crates/core/tests/{sheets_parity,sheets_expr_parity,sheets_self_parity}.rs`.
6. `grammar/google-sheets/google-sheets.bbnf`.
7. `data/sheets/nested.txt`.

## Scope — W1.d only

1. `sheets_parity.rs` — drop obsolete pinned-to-fail framing
   on tests whose post-B5 semantics fires positively; tighten
   `≥ N occurrences` to exact count where structural invariant
   admits.
2. `sheets_expr_parity.rs` — ≥ 9 field-for-field
   `SheetsGValue` assertions (FnCall, BinOp, Literal::Number,
   Literal::Str, Literal::Bool, Literal::Error, Cell, Ident,
   Paren).
3. `sheets_self_parity.rs` — verify 50+ serialise-roundtrip +
   prettify-idempotency cases green; full corpus sweeps.
4. `sheets_parse_nested_no_panic` — direct-path test parsing
   every line of `data/sheets/nested.txt`; deeply-nested
   IF(AND(...), VLOOKUP(...), IFERROR(...)) is the
   AUDIT-D §5 tripping shape; green state is the transitive
   fix evidence.
5. `sheets_admission_totality` — Sheets axis 10:10:10
   verification (or post-W1.d totality count).

## File bounds

Allow-list:
- `crates/core/tests/sheets_parity.rs` (modify).
- `crates/core/tests/sheets_expr_parity.rs` (modify).
- `crates/core/tests/sheets_self_parity.rs` (modify).
- `crates/core/tests/sheets_parse_nested_no_panic.rs`
  (create).
- `crates/core/tests/sheets_admission_totality.rs` (create).

Forbidden:
- Any grammar / source / bench.
- Any CSS or JSON or BBNF test.
- `crates/core/src/grammar/generated/google_sheets.rs`
  (orchestrator regen).

## Hard gate

1. `cargo test -p bbnf --test sheets_parity --profile
   ax-iter` green; zero new `#[ignore]`.
2. `cargo test -p bbnf --test sheets_expr_parity --profile
   ax-iter` green with ≥ 9 new field-for-field assertions.
3. `cargo test -p bbnf --test sheets_self_parity --profile
   ax-iter` green on the full corpus.
4. `cargo test -p bbnf --test sheets_parse_nested_no_panic
   --profile bench` green.
5. `cargo test -p bbnf --test sheets_admission_totality
   --profile ax-iter` green.
6. `cargo bench --profile bench -p bbnf --bench
   google_sheets_monolithic parse_nested` exit 0.

## Return format

≤ 500 words.

## HARD CAP: 30 min

Begin.
```

### Template W1.e — CSS + Sheets samply + bench + AZ-I baseline

```
You are sub-agent W1.e for tranche AY-III. W1 owns CSS + Sheets +
AZ-I baseline. Your job: extend `crates/core/benches/css/competitors.rs`
with bbnf entries; capture fat-LTO numbers vs lightningcss +
cssparser; samply per CSS fixture (tailwind, bootstrap, normalize)
plus per Sheets fixture (simple, nested, stress); publish the
AZ-I.W0 baseline-bench artefact at
`docs/benchmarks/post-AZ-I-W0-baseline.json` (the merge
absorption of AZ-I.W0 baseline-bench numbers).

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/PROFILING.md`.
3. `docs/instructions/tranche/SPEC.md`.
4. `docs/tranches/AY-III/AY-III.md`.
5. `docs/tranches/AY-III/waves/W1.md` (W1.e sub-phase).
6. `docs/tranches/AZ-I/AZ-I.md` §W0 (the merged
   baseline-bench items).
7. `crates/core/benches/{css,google_sheets}/**`.

## Scope — W1.e only

1. Extend `css/competitors.rs` with `bench_bbnf_css` group
   (normalize / bootstrap / tailwind).
2. Capture `cargo bench --profile bench -p bbnf --bench
   competitors` + `--bench css_l4` + `--bench
   google_sheets_monolithic`; publish to
   `post-AY-III-W1-competitors-css.txt`,
   `post-AY-III-W1-monolithic.txt`.
3. Samply per CSS fixture (interactive `samply record`, NOT
   `--save-only`); captures under
   `.profiles/samply/AY-III-W1/css_{fixture}/`.
4. Samply per Sheets fixture under
   `.profiles/samply/AY-III-W1/sheets_{fixture}/`.
5. Top-5 self-time per profile to
   `post-AY-III-W1-samply-summary.txt`.
6. `nm` audit for retired-surface absence (post-B5 surface
   map per AY-III.W0.c) on each bench binary; capture at
   `post-AY-III-W1-nm.txt`.
7. Author `docs/benchmarks/post-AZ-I-W0-baseline.json`
   capturing the 5 fixture-baseline numbers AZ-I.W0 needs as
   its baseline (5-bench fat-LTO entries: json_monolithic,
   css_l4, google_sheets_monolithic, bbnf_monolithic,
   compile_pipeline). MERGE absorption of AZ-I.W0 baseline-
   bench items.

## File bounds

Allow-list:
- `crates/core/benches/css/competitors.rs` (modify).
- `crates/core/benches/css/l4.rs` (modify if needed).
- `crates/core/benches/google_sheets/monolithic.rs` (modify
  if needed).
- `docs/benchmarks/post-AY-III-W1-competitors-css.txt`
  (create).
- `docs/benchmarks/post-AY-III-W1-monolithic.txt` (create).
- `docs/benchmarks/post-AY-III-W1-samply-summary.txt`
  (create).
- `docs/benchmarks/post-AY-III-W1-nm.txt` (create).
- `docs/benchmarks/post-AZ-I-W0-baseline.json` (create —
  MERGE absorption).
- `.profiles/samply/AY-III-W1/css_{tailwind,bootstrap,normalize}/`
  (populate).
- `.profiles/samply/AY-III-W1/sheets_{simple,nested,stress}/`
  (populate).

Forbidden:
- Any grammar / source / test.
- Any non-CSS / non-Sheets bench.
- `Makefile`, `.cargo/config.toml`, `scripts/**`.

## Hard gate

1. `cargo bench --profile bench -p bbnf --bench
   competitors bench_bbnf_css` runs clean.
2. `post-AY-III-W1-competitors-css.txt` carries 3-parser
   ratios (bbnf + lightningcss + cssparser) × 3 fixtures.
3. Samply profiles exist for 6 captures (3 CSS + 3 Sheets).
4. nm: retired-surface symbols absent on every bench binary.
5. `post-AZ-I-W0-baseline.json` is valid JSON with 5
   fixture-baseline entries.
6. `make ay-bench-close WAVE=W1-close` clean across 5 bench
   binaries.

## Return format

≤ 700 words (deep — 3 captures plus AZ-I baseline absorption).

## HARD CAP: 30 min

Begin.
```

## W2 templates (BBNF self-host + close ceremony)

### Template W2.a — BBNF self-hosting identity

```
You are sub-agent W2.a for tranche AY-III. W2 owns BBNF self-host
+ close ceremony. Your job: run `cargo xtask regen --check`;
diagnose drift if non-zero; fix in place at `xtask/src/regen.rs`
if drift is in xtask post-processing; publish empty-diff artefact
as the positive evidence.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md` (§Self-host circular-dependency
   escape — the AX.W0a one-shot is for the parser-cannot-parse
   case, NOT routine post-processing drift).
2. `docs/instructions/tranche/SPEC.md` (§Diagnostic-loop
   relinquish).
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/waves/W2.md` (W2.a sub-phase).
5. `xtask/src/regen.rs`.
6. `docs/tranches/B2/FINAL.md` (the post-B2 native xtask
   regen contract).

## Scope — W2.a only

1. Run `cargo xtask regen --check`; non-zero exit triggers
   §Diagnostic-loop relinquish.
2. If drift, run `cargo xtask regen` (full sweep);
   `git diff` against in-tree per-grammar source;
   `docs/benchmarks/post-AY-III-W2-regen-diff.txt` carries
   the diff (empty after re-run is the positive artefact).
3. If drift is in xtask post-processing (`prettyplease`,
   derive idempotency, blank-line normalisation), fix in
   place at `xtask/src/regen.rs`. Routine post-processing
   drift is a regen-emitter bug, not an escape recipe.

## File bounds

Allow-list:
- `xtask/src/regen.rs` (modify only if drift found).
- `docs/benchmarks/post-AY-III-W2-regen-diff.txt` (create —
  empty content = positive).

Forbidden:
- Any grammar (per AY-III invariant 8).
- Any other source.
- `crates/core/src/grammar/generated/**` (orchestrator regen
  at close).

## Hard gate

1. `cargo xtask regen --check` exit 0 (positive artefact).
2. `wc -l crates/core/src/grammar/generated/bbnf.rs` within
   ±1% of B5-close line count.
3. `cargo check -p bbnf --lib --profile ax-iter` exit 0
   against post-regen per-grammar source.
4. Workspace nextest 1477+/1477+ green at sub-phase close.

## Return format

≤ 300 words.

## HARD CAP: 30 min

Begin.
```

### Template W2.b — BBNF grammar-meta projection totality

```
You are sub-agent W2.b for tranche AY-III. W2 owns BBNF self-host
+ close ceremony. Your job: audit `target/expand/ay-bbnf-self-parity.rs`
for projection totality on the BBNF axis; add `-> Type`
annotations to declared rules whose grammar-derived field shape
admits a projection (annotations only — no rule-body edits, per
invariant 8); author `bbnf_admission_totality.rs` asserting BBNF
1:1:1 wire contract; tighten `bbnf_parity.rs` and
`bbnf_ast_parity.rs`; verify `bbnf_self_parity.rs` covers every
`@pretty`-bearing grammar.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md`.
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/waves/W2.md` (W2.b sub-phase).
5. `grammar/bbnf/{bbnf,expressions,types}.bbnf`.
6. `crates/core/tests/{bbnf_parity,bbnf_ast_parity,bbnf_self_parity}.rs`.

## Scope — W2.b only

1. Audit `target/expand/ay-bbnf-self-parity.rs`: confirm
   `pub struct BbnfEmit\w+Projection` count ==
   `materialize_projection_\w+_BbnfEmit` count ==
   `PROJECTION_DIRECT_TO_STRUCT` count on the BBNF axis (floor
   10:10:10 at master HEAD).
2. Add `-> Type` annotations only — no rule-body edits.
3. Author `crates/core/tests/bbnf_admission_totality.rs`.
4. `bbnf_parity.rs` — one `#[test]` per BBNF `-> Span` leaf
   (`identifier`, `literal`, `regex`, `comment`,
   `big_comment`).
5. `bbnf_ast_parity.rs` — extend RefGrammar oracle with
   per-admission field-layout parity.
6. `bbnf_self_parity.rs` — every `@pretty`-bearing grammar
   wired (3 in `grammar/bbnf/bbnf.bbnf` + 6 in
   `grammar/css/pretty.bbnf`).
7. Author `crates/core/tests/grammar_roundtrip.rs` — every
   `@pretty`-bearing grammar re-emits byte-identically across
   the round trip.

## File bounds

Allow-list:
- `grammar/bbnf/bbnf.bbnf` (modify — annotations only).
- `grammar/bbnf/expressions.bbnf` (modify — annotations
  only).
- `grammar/bbnf/types.bbnf` (modify — annotations only).
- `crates/core/tests/bbnf_parity.rs` (modify).
- `crates/core/tests/bbnf_ast_parity.rs` (modify).
- `crates/core/tests/bbnf_self_parity.rs` (modify).
- `crates/core/tests/bbnf_admission_totality.rs` (create).
- `crates/core/tests/grammar_roundtrip.rs` (create).

Forbidden:
- Any non-BBNF grammar (W1 owns CSS / Sheets).
- Any source code.
- Any non-BBNF test.
- `crates/core/src/grammar/generated/bbnf.rs` (orchestrator
  regen).

## Hard gate

1. `cargo test -p bbnf --test bbnf_admission_totality
   --profile ax-iter` green with BBNF axis ≥ 10 admissions
   1:1:1.
2. `cargo test -p bbnf --test bbnf_parity --release` green
   with ≥ 2 structural-reach tests per Span-admitted leaf.
3. `cargo test -p bbnf --test bbnf_ast_parity --release`
   green with field-layout parity.
4. `cargo test -p bbnf --test bbnf_self_parity --release`
   green; zero `#[ignore]`.
5. `cargo test -p bbnf --test grammar_roundtrip --release`
   green.
6. `cargo test -p bbnf --test named_type_preservation
   --profile ax-iter` green.
7. `cargo test -p gorgeous --tests --profile ax-iter` green
   (cross-crate proof on post-regen per-grammar source).

## Return format

≤ 700 words (deep — 3 grammars + 3 test extensions + 2 new
tests).

## HARD CAP: 30 min

Begin.
```

### Template W2.c — BBNF samply + bench + close-ceremony evidence

```
You are sub-agent W2.c for tranche AY-III. W2 owns BBNF self-host
+ close ceremony. Your job: capture samply on `bbnf_self`; nm
audit on the bench binary; full fat-LTO 5-bench close-matrix run;
publish competitor benches at the tranche-close artefact paths.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/PROFILING.md`.
3. `docs/instructions/tranche/SPEC.md`.
4. `docs/tranches/AY-III/AY-III.md`.
5. `docs/tranches/AY-III/waves/W2.md` (W2.c sub-phase).
6. `docs/tranches/B5/FINAL.md` (post-B5 surface).

## Scope — W2.c only

1. `make ay-prepare-profile-wave WAVE=AY-III-W2` produces
   the `profiling-prep` `bbnf_monolithic` binary.
2. `samply record` interactive (NOT `--save-only`):
   `.profiles/samply/AY-III-W2/bbnf_self/profile.json.gz`.
3. `nm` on bench binary; capture
   `post-AY-III-W2-nm.txt`. Same retired-surface absence
   audit as W0.c (post-B5 surface map).
4. `make ay-bench-close WAVE=W2-close` across all 5 bench
   binaries; aggregate to `docs/benchmarks/post-AY-III.json`.
5. Run competitor benches:
   `cargo bench --profile bench -p bbnf --bench competitors
   > docs/benchmarks/post-AY-III-competitors-json.txt`.
6. Run CSS competitors:
   `cargo bench --profile bench -p bbnf --bench
   css_competitors >
   docs/benchmarks/post-AY-III-competitors-css.txt`.
7. Top-8 self-time on `bbnf_self` to
   `post-AY-III-W2-samply-summary.txt`.

## File bounds

Allow-list:
- `.profiles/samply/AY-III-W2/bbnf_self/profile.json.gz`
  (create).
- `docs/benchmarks/post-AY-III-W2-nm.txt` (create).
- `docs/benchmarks/post-AY-III-W2-samply-summary.txt`
  (create).
- `docs/benchmarks/post-AY-III.json` (create — tranche-close
  matrix).
- `docs/benchmarks/post-AY-III-competitors-json.txt` (create).
- `docs/benchmarks/post-AY-III-competitors-css.txt` (create).

Forbidden:
- Any source.
- Any test.
- Any bench source modification.

## Hard gate

1. Profile exists with ≥ 1000 samples on `bbnf_self`.
2. nm: retired-surface symbols absent (post-B5 surface map);
   `Tape<R>::*` symbols present.
3. Samply top-8 includes a `Tape<R>` symbol ≥ 1 % AND
   excludes every retired-surface symbol from any `to_value`
   lineage.
4. `make ay-bench-close WAVE=W2-close` clean across 5 bench
   binaries; `bbnf_self` ≥ 98 MB/s; `compile_bbnf` median
   ≤ 2.806 ms × 1.05.
5. `post-AY-III.json` valid JSON covering the 5-bench matrix.
6. Competitor outputs published.

## Return format

≤ 700 words.

## HARD CAP: 30 min

Begin.
```

### Template W2.final — Close ceremony (serial closer)

```
You are sub-agent W2.final for tranche AY-III. W2.final is the
serial close ceremony after W2.a/b/c land. Your job: author
`docs/tranches/AY-III/FINAL.md` against the close artefacts;
update successor tranche docs (AZ-I, AZ-II, BA, BB) to reference
the AY-III close commit; reconcile cross-tranche debt; update
`docs/RISK-PERF-MATRIX.md` and `docs/tranches/REMAINING-TRAJECTORY.md`.

## Worktree

`{WORKTREE_PATH}`.

## Read first

1. `docs/instructions/README.md`.
2. `docs/instructions/tranche/SPEC.md` (§Closing ceremony,
   §Document set).
3. `docs/tranches/AY-III/AY-III.md`.
4. `docs/tranches/AY-III/PROGRESS.md`.
5. `docs/tranches/AY-III/waves/{W0,W1,W2}.md`.
6. `docs/tranches/AY-II-I/AY-II-I.md` (predecessor pass-II
   plan; cross-pass references via absolute path).
7. `docs/tranches/B5/FINAL.md`, `docs/tranches/B6/FINAL.md`
   (predecessor B-series substrate truth).
8. All W0 + W1 + W2 close artefacts under
   `docs/benchmarks/post-AY-III-*`.

## Scope — W2.final only

1. Author `docs/tranches/AY-III/FINAL.md`. Required sections:
   - Architectural narrative (≤ 30 lines, imperative;
     post-B5 substrate, three waves, audit consolidation).
   - Wave-by-wave recap with commit hashes.
   - Performance table (B5 baseline vs AY-III close per
     fixture; declared / floor / actual).
   - Test results.
   - API surface changes (none — invariant 10 holds; surface
     map at `docs/tranches/B5/FINAL.md` is unchanged).
   - Cross-tranche debt: inherited (closed in AY-III) +
     forwarded (with named destination).
   - Defensible-floor result (declared vs landed vs floor;
     escape-clause status).
   - Verdict.
2. Update `docs/tranches/AZ-I/AZ-I.md` precondition to
   reference AY-III close commit.
3. Update `docs/tranches/AZ-II/AZ-II.md` similarly.
4. Update `docs/tranches/BA/BA.md`, `BB/BB.md` similarly.
5. Update `docs/RISK-PERF-MATRIX.md` (`AY-II.W0'` row →
   `AY-III` row at landed status).
6. Update `docs/tranches/REMAINING-TRAJECTORY.md` similarly.
7. Append AY-III close section to PROGRESS.md.

## File bounds

Allow-list:
- `docs/tranches/AY-III/FINAL.md` (create).
- `docs/tranches/AY-III/PROGRESS.md` (modify — close entry).
- `docs/tranches/AZ-I/AZ-I.md` (modify — precondition
  update).
- `docs/tranches/AZ-II/AZ-II.md` (modify — precondition
  update).
- `docs/tranches/BA/BA.md` (modify — precondition update).
- `docs/tranches/BB/BB.md` (modify — precondition update).
- `docs/RISK-PERF-MATRIX.md` (modify — landed-status update).
- `docs/tranches/REMAINING-TRAJECTORY.md` (modify — landed-
  status update).

Forbidden:
- Any source code.
- Any test.
- Any bench.
- Any grammar.

## Hard gate

1. `docs/tranches/AY-III/FINAL.md` exists with all required
   sections.
2. Successor tranche docs updated (AZ-I, AZ-II, BA, BB) to
   reference AY-III close commit.
3. PROGRESS.md carries close entry with tranche HEAD commit
   SHA.
4. `docs/RISK-PERF-MATRIX.md` AY-III row reflects landed
   status.
5. `docs/tranches/REMAINING-TRAJECTORY.md` AY-III row
   reflects landed status.
6. Workspace nextest 1477+/1477+ green at tranche close.
7. `cargo test --workspace --no-fail-fast` returns 0 failures.

## Return format

≤ 700 words. Include:

1. FINAL.md commit SHA + sections present.
2. Predecessor / successor doc-update commit SHAs.
3. Hard-gate status table.
4. Final invariant-verification table (10 invariants ×
   artefact path × PASS / FAIL).
5. `git status --short`.

## HARD CAP: 30 min (redress role; ceremony only — no
implementation)

Begin.
```

## Inherited dispatch invariants (do NOT re-state per prompt)

Per `docs/instructions/README.md` and the inherited scope of
`docs/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md`, every
sub-agent reads on first step:

- Tranche structure + directory layout.
- Crate ownership (full write access; external `path = ../*`
  crates included).
- Commit-at-milestone cadence.
- Worktree isolation rules.
- Bench contract (cold per-parse; mimalloc global allocator).
- Wire-contract pipelines require end-to-end tests.

## Cherry-pick consolidation discipline

W1 dispatches 5 agents on disjoint file bounds. The
orchestrator pre-declares one consolidation commit per
`docs/instructions/tranche/SPEC.md` §N-agent shared-file
consolidation: when the W1 cherry-pick sequence hits a
3-way merge conflict on `docs/benchmarks/post-AY-III-W1-*.json`
or any shared module-declaration line, the orchestrator splices
directly with attribution to each agent's worktree. The first
two agents' commits cherry-pick cleanly; the remaining three
land via direct orchestrator surgery on master.

W0 and W2 dispatch fewer agents on tighter file bounds; no
shared-file consolidation declared. Cherry-pick conflict
resolution per SPEC §Cherry-pick conflict resolution rules
applies.

## Triumvirate fallback

Per SPEC §Diagnostic-loop relinquish, any sub-agent in a
multi-cycle diagnostic loop (3+ iterations without commit, or
30+ minutes wall without forward motion) halts, reports, and
relinquishes. The orchestrator dispatches a research + plan +
redress triumvirate landing under
`docs/tranches/AY-III/audit/`. JSONL quiet > 15 min OR
first-pass no-commit triggers the triumvirate without user
prompt per `feedback_triumvirate_auto_trigger`.
