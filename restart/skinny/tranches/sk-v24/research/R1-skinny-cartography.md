# R1 — SKINNY CARTOGRAPHY (SK-V24 recon)

VERIFIED pointers for the PROVE fleet. Every path checked via `ls`/`rg`; code cited `path:line`.
All paths absolute. Read-only recon; no build invoked.

## 0. Path correction (BINDING — read first)

The brief says "workspace root under `/restart/skinny`". **FALSE.** `restart/skinny/` is
DOCS-ONLY (tranches/*.md + research support harnesses). The only `Cargo.toml`s under
`restart/skinny` are three throwaway harnesses (`sk-v10/.../escape-segment-proof`,
`sk-v13/.../mode3`, `sk-v13/.../css_profiler`). **The real skinny Rust workspace is at the repo
root:** `/Users/mkbabb/Programming/bbnf-lang/skinny/`. Every pointer below is under that root
unless prefixed otherwise. Grammar SOURCES live one level up in the bbnf-lang repo `grammar/`
dir (repo_root, NOT skinny root) — see §6.

## 1. Workspace root & members

- Root manifest: `/Users/mkbabb/Programming/bbnf-lang/skinny/Cargo.toml`
  - 12 members (`Cargo.toml:3-16`); resolver 2; edition 2021; rust-version 1.78.
  - Cross-repo path-deps: `egraph = ../crates/egraph`, `csp-solver = ../crates/csc411`-vendored
    (`Cargo.toml:36-37`) — these live in the OUTER repo `crates/`, not skinny.
  - `[workspace.metadata.bbnf]` (`Cargo.toml:54-70`): `generated_root = crates/runtime/src/grammars`,
    `fixture_root = crates/test-fixtures/corpus`, `profile = balanced`, `host_registry = skinny-none`.
    Only ONE grammar declared in metadata: `json` (source `grammars/json.bbnf`, tape_mode `lazy`,
    loc_budget 4000).
  - Profiles (`Cargo.toml:72-101`): `release` = opt3/fat-LTO/cgu1/panic=abort/debug=true;
    `bench` inherits release; `ax-iter` = thin-LTO/cgu16 (the iteration profile per memory law).

### Crates, roles, LOC (hand-written `src/` vs generated)

| Crate | Path (`crates/…/src`) | Role | Hand LOC | Gen LOC |
|-------|----------------------|------|---------:|--------:|
| `bbnf` | `bbnf/` | thin facade re-export (`lib.rs` only) | 111 | – |
| `grammar` | `grammar/` | `.bbnf` front-end: `parse_grammar` → `GrammarIr` | 2142 | – |
| `ir` | `ir/` | `GrammarIr`, `BackendIr`, cost model | 1020 | – |
| `passes` | `passes/` | compile pipeline: infer→shape→CSP→egraph | 2750 | – |
| `codegen` | `codegen/` | grammar→Rust generator (THE generator) | 10201 | – |
| `runtime` | `runtime/` | tape, scan shells, generated grammars host | 1369 | 5153 |
| `bbnf-simd` | `bbnf-simd/` | NEON/scalar kernels + dispatch seam | 2324 | – |
| `bbnf-regex` | `bbnf-regex/` | thin regex facade (322 LOC, mostly re-export) | 322 | – |
| `parse-that-regex` | `parse-that-regex/` | string/number span kernels, Eisel–Lemire | 3022 | – |
| `bbnf-bench` | `bbnf-bench/` | benches + probes + gate | 35933 | – |
| `test-fixtures` | `test-fixtures/` | corpus loader | 404 | – |
| `xtask` | `xtask/` (repo `skinny/xtask`) | regen + gate + checkasm CLI | 5401 | – |

## 2. Generator entry & emit pipeline (codegen)

- **Entry**: `codegen/src/lib.rs:101` `emit_from_source(grammar_name, source) -> EmittedSource`.
  Pipeline: `grammar::parse_grammar` (`lib.rs:102`) → `passes::compile` (`lib.rs:103`, returns
  `PipelineOutput { backend_ir, layout_facts, diagnostics }`) → `emit_with_layout` (`lib.rs:194`)
  → `lower::lower_to_rust` (`lib.rs:176`) → `runtime_generator::emit_sink_only` (`lib.rs:210`).
- **Typed entry**: `codegen/src/lib.rs:130` `emit_typed_from_source(...)`; adds
  `lower::schema_direct::lower_program` + `json_typed_direct::render` (`lib.rs:238-243`) — the
  direct-to-struct emitter (PROVE-07 anchor).
- `emit(backend)` at `lib.rs:114` is **fail-closed** (W7): requires pass-produced shape/cost/CSP.
- **Emit sub-modules** (all `codegen/src/`):
  - `lower/` — IR→Rust lowering. `lower/mod.rs:*` `lower_to_rust`; `LowerCtx` carries
    `backend_shape`/`cost_facts`/`per_grammar_policy`/`same_substrate_union` (`lib.rs:177-184`).
    Shapes: `lower/eager_tape.rs`, `offset_tape.rs`, `event_tape.rs`, `sink_only.rs` (278),
    `collapsed_stage.rs`, `css_scan.rs` (712 — CSS STOP-set derivation), `schema_direct.rs`,
    `tape_plan.rs` (179), `rust.rs` (170).
  - `json_sink_direct/` — the JSON un-fork emitter: `dispatch.rs` (318, "the 91.5% hot leaf"),
    `first_set.rs` (144), `recognize.rs` (191), `skeleton.rs` (330), `mod.rs` (230).
  - `css_scan_direct/` — `drivers.rs` (320), `projection.rs` (588), `mod.rs` (247).
  - `general_descent/` — `emit.rs` (452), `view.rs` (431), `regex_first.rs` (230), `mod.rs` (93).
  - `json_templates/` — `generated.rs`/`view.rs`/`value.rs`/`parser.rs`/`visitor.rs` (value API).
  - `json_typed_direct.rs` (1245), `direct_schema.rs` (362), `runtime_generator.rs` (522),
    `grammar_provider.rs` (297), `grammar_profile.rs` (57).

## 3. Scan / classify layer + NEON/SIMD kernels (bbnf-simd)

**Only aarch64** — x86 abrogated (STANDS, HANDOFF §5). Structure: `crates/bbnf-simd/src/`
`{lib.rs, dispatch.rs, classifier.rs, aarch64/, scalar/}`.

- `lib.rs:105` `scan_dispatch(input, &StructuralAlphabet)` — the 64-byte block loop; calls
  `prim::byte_class_from_table_64` (`lib.rs:113`) then `compact_mask` (`lib.rs:228`).
- `lib.rs:250` **`pub mod prim`** — the per-kernel dispatch facade. Each fn: `#[cfg(aarch64)]`
  NEON body + scalar reference fallthrough (checkasm-parity gated). Members:
  `byte_class_from_table_64` (:256), `bitmap_prefix_xor_64` (:261), `bitmap_next_set_bit` (:266),
  `bulk_emit_positions_64` (:271), `eob_pad_clamp` (:276), `byte_class_from_eq_set_64` (:283),
  **`structural_classify_64` (:301)** — the grammar-neutral vceqq-fan stage-1 kernel,
  `comment_body_mask_64` (:316), `bracket_depth_mask_64` (:336).
- `lib.rs:174` `escape_mask_64` (odd/even backslash-run carry), `lib.rs:169` `prefix_xor_64`,
  `lib.rs:208` `find_ascii_set_member64`.

### aarch64 kernels (`bbnf-simd/src/aarch64/`, `wc -l`)

- `structural_classify_64.rs` (155) — **vceqq fan** producing significant+per-kind masks;
  packs via `movemask_u8x16` (`:147`, the multiply-bit-weight variant). Header (`:1-31`)
  documents WHY fan not LUT (lo6 collisions) and the simdjson lo×hi two-LUT upgrade path.
- `movemask.rs:5` — **`vshrn_n_u16::<4>`** mask-narrow idiom (the empirical-spine +35% win).
- `classify_tbl4.rs` (104) — `vqtbl4q` lo6-table classifier (`build_lo6_table`/`classify_block_from_table`).
- `byte_class_from_eq_set_64.rs` (87), `bracket_depth_mask_64.rs` (107, uses `vshrn`+`quad_load`),
  `comment_body_mask_64.rs` (145), `match_tiny_plain_string.rs` (136), `unescape_uxxxx.rs` (214),
  `string_block.rs` (72), `digit_mac.rs` (71), `utf8/validate_block.rs` (158), `quad_load.rs`,
  `cache_hints.rs`, `byte_context.rs`.
- Scalar references (parity anchors): `scalar/structural_classify_64.rs` (108),
  `scalar/bracket_depth_mask_64.rs` (94), `scalar/comment_body_mask_64.rs` (120),
  `scalar/swar_8byte.rs` (18).

**No shufti/truffle/prefilter kernel exists in skinny.** The ONLY repo mention of
"prefilter" is a test name `parse-that-regex/src/lib.rs:1322`. PROVE-01 builds this NET-NEW —
the natural home is a new `prim::` fn (§8) atop the existing eq-set/tbl toolkit.

## 4. Tape writer & node representation (runtime)

- `runtime/src/tape/mod.rs` — the tape. `Tape<'input>` struct at `:92`: `source`, `offsets:Vec<u32>`,
  `flag_cursors:Vec<u32>`, `flag_values:Vec<u8>`, `payloads:PayloadArena`, `id:TapeId`. Built via
  `from_offsets` (`:102`). `OffsetFlags(u8)` at `:16`. `PayloadArena` at `:36` (`write_bytes` `:63`,
  bench-gated write/alloc counters). `ValueRef<'doc,'input,K>` at `:173` (zero-cost typed cursor,
  `offset()` `:214`). `DocumentView` trait `:223`.
- `tape/assembler.rs` (124) — `TapeBuilder`/`CapacityPlan`. `tape/cursor.rs` (36) `Cursor`.
  `tape/offsets.rs` (6) `OffsetTapeStats`.
- Lazy-projection invariant enforced in tests: `runtime/src/lib.rs:42-44` asserts
  `payloads().len()==0` and zero writes/allocs after JSON parse (AoS/offset-only tape).
- `runtime/src/runtime_scan.rs` (317) — `css_balanced_component_scan` shell (CSS-scoped, emits
  nothing; header `:1-35`). Consumes `bbnf_simd::prim::{structural_classify_64,KindPartition}`
  (`:37`), builds `KindPartition` from grammar-derived STOP/SKIP sets (`:64-80`).
- `runtime/src/runtime_simd.rs` (97) — grammar-neutral bridge to bbnf-simd primitives.

## 5. Generated grammar outputs & regeneration

- Generated root: `crates/runtime/src/grammars/`. THREE live grammars (NOT toml/csv):
  - `json/` — `generated.rs`, `parser.rs`, `scan.rs`, `sink.rs`, `value.rs`, `view.rs`,
    `visitor.rs`, `host.rs`, `config.rs`, `mod.rs`. Header: `generated.rs:1`
    `// @generated by skinny bbnf-codegen; do not edit by hand.` Runtime scan NEUTRALIZED on
    live path (`scan.rs:1-14`): `parse_direct` uses `GrowOnly` const-256 capacity; NEON
    `scan_structurals` (`scan.rs:36`) reachable only under `BBNF_TAPE_CAPACITY=A|B|C`.
  - `css_l4_declaration_values/` — the SINGLE canonical CSS config (7 fabricated replicas
    collapsed; `runtime/src/lib.rs:16-21`).
  - `sheets/` — `generated.rs`, `parser.rs`, `view.rs`, `config.rs`, `mod.rs`.
  - Wired at `runtime/src/lib.rs:13-30` (`generated_json`, `generated_css_l4_declaration_values`,
    `generated_sheets`; re-exported under `mod grammars`).
- Generated LOC = 5153 (runtime table §1).

### Regeneration (run from `skinny/` workspace root)

`cargo xtask <cmd>`; dispatch in `xtask/src/main.rs:39-52`:
- `regen-json` / `check-json` (`main.rs:40-41`); target `JSON_TARGET` `main.rs:177`,
  output `crates/runtime/src/grammars/json`, source `skinny/grammars/json.bbnf` (`main.rs:164`).
- `regen-css` / `check-css-l4-declaration-values` (`main.rs:44-45`) → `xtask/src/regen_css.rs`;
  roots `grammar/css/l4/stylesheet.bbnf` (`regen_css.rs:24`), 15 module sources (`regen_css.rs:6-20`).
- `regen-sheets` / `check-sheets` (`main.rs:42-43`); source `grammar/google-sheets/google-sheets.bbnf`
  (`main.rs:191`).
- `regen-real-typed` / `check-real-typed` (`main.rs:46-47`); `lint-loc` (:49, LOC budget gate);
  `primitive-checkasm` (:52, the dav1d parity gate); `gate-json`/`bench-json` (:50-51).
- Mechanics: `xtask/src/regen.rs:19` `write_targets` → `codegen::emit_runtime_from_request` →
  `emitted.write_to_dir`; `check_target` (`regen.rs:36`) fails if stale. Source paths resolved
  against repo_root (bbnf-lang), so `.bbnf` sources are the OUTER `grammar/` tree.

## 6. Grammar sources (repo `grammar/`, NOT skinny)

- JSON: `/Users/mkbabb/Programming/bbnf-lang/skinny/grammars/json.bbnf` (skinny-local, 492B) AND
  `/Users/mkbabb/Programming/bbnf-lang/grammar/json/json.bbnf`.
- CSS L4: `/Users/mkbabb/Programming/bbnf-lang/grammar/css/l4/*.bbnf` (16 modules; root `stylesheet.bbnf`).
- Sheets: `/Users/mkbabb/Programming/bbnf-lang/grammar/google-sheets/google-sheets.bbnf`.
- **CSV (PROVE-13 generality target)**: `/Users/mkbabb/Programming/bbnf-lang/grammar/misc/csv.bbnf`
  EXISTS — no generated output yet, no regen target. **No TOML `.bbnf` exists anywhere.**
- Others in `grammar/misc/`: `math.bbnf`, `emoji.bbnf`, `g4.bbnf`, `regex.bbnf`, `json-commented.bbnf`.

## 7. Number/string span kernels (PROVE-02/03 targets) — parse-that-regex

- `parse-that-regex/src/number/mod.rs:106` `scan_digit_run` — ALREADY SWAR-blocked: 8/4/2-digit
  paths (`:113`,`:125`,`:137`) via `parse_eight_digits`/`is_eight_ascii_digits`. PROVE-02
  (Mula/Lemire SIMD digit-run) upgrades THIS. `match_number_span_from_first` `:38`, `NumberSpan` `:5`.
- `parse-that-regex/src/number/eisel_lemire/mod.rs:147` `compute_f64(exp,mantissa,neg)`;
  `algorithm.rs` (93), `table.rs` (660). Consumed by JSON number path.
- Strings (PROVE-03): `parse-that-regex/src/lib.rs:168` `match_string_at_quote_trusted_utf8`,
  `:243` `..._after_plain_prefix_trusted_utf8`, `:441` `decode_unicode_escape`, `:783`
  `classify_string_content`, `:867` `unescape_string`. NEON unescape: `bbnf-simd/aarch64/unescape_uxxxx.rs`.
- `lib.rs:119` `skip_ascii_whitespace`. Re-exports at `lib.rs:8`.
- `integration/simd_scan_hook.rs` (19) — the current parse-that↔bbnf-simd seam (thin).

## 8. WHERE A NEW SIMD KERNEL INTEGRATES (the seam — PROVE-01/04/05)

Three coordinated layers. To productionize shufti/truffle prefilter (PROVE-01) or multi-block
scan (PROVE-04):

1. **Kernel body + dispatch facade** (`bbnf-simd`):
   - Add scalar reference `scalar/<kernel>.rs` (executable spec / checkasm parity anchor) +
     aarch64 body `aarch64/<kernel>.rs` (reuse `uint8x16` stripe + `vceqq`/`vqtbl4q` + `movemask`
     toolkit — verbatim reuse pattern documented at `aarch64/structural_classify_64.rs:23-28`).
   - Expose via new `prim::<kernel>` fn in `bbnf-simd/src/lib.rs:250` (mirror the
     `#[cfg(aarch64)] return neon; scalar` shape of `structural_classify_64` at `:301-309`).
   - For a runtime-selected kernel: extend `PrimitiveKernels` fn-ptr table
     (`dispatch.rs:50-56`) + `select_primitive_kernels` (`dispatch.rs:63`, OnceLock at `:58`).
     For a classifier-backend choice: extend `enum SelectedBackend` (`dispatch.rs:11-16`, today
     `Scalar`|`NeonTbl4`) + `select_backend` admissibility (`dispatch.rs:89`, gate fn
     `lo6_table_admissible` `:101`). This enum IS the cost-driven dispatch switch.

2. **IR recognizer node** (`ir/src/lib.rs`):
   - `enum Recognizer::SimdScan { mode:SimdMode, alphabet:StructuralAlphabet, site:SimdSite }`
     at `:416-422`. `SimdMode` (`:425`, today only `Exact`) and `SimdSite` (`:430`, today only
     `PreEntry`) are the extension enums — add a `Prefilter`/`MultiBlock` mode or a per-kind
     schedule here. `Recognizer`s produced by `passes::derive_recognizers`
     (`passes/src/lib.rs:338`). Emitted-source comment shows the current node:
     `runtime/src/grammars/json/generated.rs:10` `// recognizer: SimdScan Exact PreEntry alphabet=...`.

3. **Cost model / cost flags** (the selection driver):
   - `ir/src/cost.rs:5` `CostFacts` (projection `:24`); `:57` `SubstrateTarget`; `:173`
     `ActiveCostFacts` (egraph/CSP selection provenance — `selected_shape`, cost-freshness,
     rewrite counts); `:203` `DecisionCspFacts`; `:264` `PriorityStep` (8-step, `ALL` at `:276`).
   - `enum BackendShape` at `ir/src/lib.rs:361`: `EagerTape|OffsetTape|EventTape|SinkOnly|
     CollapsedStage` (5; `all_backend_shapes()` `cost.rs:334`). A SIMD-scan cost lever plugs into
     the shape/cost selection in `passes::derive_backend_shape` (`passes/src/lib.rs:398`) via
     `BackendShapePlan.cost_facts` (`:390-392`); the N*=3 kernel-crossover (empirical spine) is
     the cost threshold this would encode.
   - Grammar-side kernel dispatch (CSS token-class, PROVE-05) surfaces in
     `codegen/src/json_sink_direct/dispatch.rs:derive_arms` (first-byte keys, `dispatch.rs:1-11`)
     and `codegen/src/lower/css_scan.rs` (STOP-set derivation, 712 LOC).

## 9. Corpora & benchmarks (PROVE measurement inputs)

- JSON: `skinny/data/json/{twitter,canada,citm_catalog,data_xl,data_supermaxx}.json` — the
  twitter/canada/citm empirical-spine trio (HANDOFF §4).
- CSS L4: `skinny/corpora/css-l4-sk-v14/{bootstrap-5.3.3,tailwindcss-0.2.0,animate-4.1.1,
  material-components-web-14.0.0}.min.css`; also `skinny/data/css/{bootstrap,tailwind,normalize}.css`.
- Sheets: `skinny/data/sheets/{simple,nested,stress}.txt`. Extra JSON: `skinny/test_data/*.json`.
- Benches (`crates/bbnf-bench/`): `benches/json_parity.rs`, `benches/simd_scan.rs` (declared
  `bbnf-bench/Cargo.toml:35-40`). Bins `src/bin/`: `css_track1_profile`, `gate`, `profile_direct`,
  `w1_tape_typed_bench`, `w2_rich_cssom_bench`, `xctrace_probe`, `css_canon_bench`.
- Gate/probe surfaces (`bbnf-bench/src/`): `gate.rs`, `scan.rs`, `probes.rs`, `parity.rs`,
  `sonic_skipper.rs`, `direct_struct.rs`, `real_typed_struct.rs`, `materialization.rs`,
  `lock14_baseline.rs`, `json_w9.rs`, `css_l4_corpus.rs`.
- xtask probe bins: `xtask/src/bin/{capacity_probe,parity_oracle,profile_lazy}.rs`
  (feature-gated `runtime-tools`).

## 10. Gotchas for implementers

- Path trap: work happens under repo-root `skinny/`, but `.bbnf` sources resolve against the
  OUTER bbnf-lang repo `grammar/` (regen source paths are repo-root-relative). `regen.rs` computes
  `repo_root` above skinny.
- x86 is ABROGATED: NO AVX2/SSE. All new kernels are aarch64 NEON + scalar reference only.
  4-layer enforcement STANDS (HANDOFF §5).
- Every NEON kernel needs a scalar reference (the executable spec) — `primitive-checkasm`
  parity is the dav1d admission gate (`xtask ... primitive-checkasm`).
- Generated files are OUTPUT-ONLY (`// @generated ... do not edit`); mutate the `.bbnf` + regen,
  never hand-patch (memory: clean-regen-discipline).
- No shufti/truffle/prefilter exists — PROVE-01 is net-new atop the eq-set/tbl toolkit.
- No TOML grammar; CSV grammar exists (`grammar/misc/csv.bbnf`) but has no generated output —
  PROVE-13 must add a regen target + generated dir.
- `emit(backend)` (`codegen/src/lib.rs:114`) is fail-closed; use `emit_from_source` /
  `emit_typed_from_source`.
- csp-solver/egraph are OUTER-repo path-deps (`../crates/`), not skinny members.
