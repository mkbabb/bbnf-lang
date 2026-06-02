# W-PRUNE P3 — COLLAPSE Ledger (SK-V18 generalization, G-Omega closed)

Wave: P3 (PRUNE: collapse the 7 byte-identical css_l4 replicas + RuntimeTarget
row-collapse). SPEC authority: `restart/skinny/tranches/sk-v18/SPEC.md` §3.3.
Source/config edits only — NO cargo, NO regen run (the orchestrator runs the
single controlled regen+build+test verification).

## Disk-evidence confirmation (re-grepped this wave)

The 7 `css_l4_*/generated.rs` were byte-identical — all md5 `b654562ccff46ed62dd48e9ace325830`:

```
b654562c…  css_l4_at_rules_and_media/generated.rs
b654562c…  css_l4_declaration_values/generated.rs          <- canonical (kept)
b654562c…  css_l4_declaration_values_extended/generated.rs
b654562c…  css_l4_nested_layout/generated.rs
b654562c…  css_l4_stylesheet_selectors/generated.rs
b654562c…  css_l4_vendor_and_custom_atrules/generated.rs
b654562c…  css_l4_visual_functions/generated.rs
```

All 7 `RuntimeTarget` rows in `regen_css.rs` shared: `grammar_name:"css_l4"`,
`entry_rule:"stylesheet"`, `source_roots:CSS_L4_ROOTS` (the ONE
`grammar/css/l4/stylesheet.bbnf` root), `source_inputs:CSS_L4_SOURCES` (the same
15 `.bbnf` files). They differed ONLY in `profile`, `output_dir`, `check_command`,
and the three `output_labels` strings. => ONE grammar over ONE root wearing 7
fabricated profile labels. The collapse-to-one default is FORCED (SPEC §3.3 /
R-A0-2): no real `.bbnf` root differentiates them; minting fake roots to fake
distinctness is the exact addendum-2 overfit.

Canonical chosen: `css_l4_declaration_values` — the bench-consumed `track1_rich`
path (`css_canon_bench.rs:39 use runtime::generated_css_l4_declaration_values as
css_decl`; `nonjson_css_l4.rs:105`; SPEC §0.2 cites
`css_l4_declaration_values/generated.rs:297-304`). All non-W8 bench binaries
(`w1_tape_typed_bench`, `w2_rich_cssom_bench`, `css_cold_*`, `css_track1_profile`,
`w4_css_reprofile`) already used ONLY this module — unaffected by the collapse.

Cross-grammar distinctness already holds: json `generated.rs` md5
`5410050ea23eec6f41c68f87578bd9d2` != css_l4 `b654562c…`. Post-regen the
binding `generated_md5_distinct` witness is the CROSS-grammar pair
`md5 {json,css_l4}/generated.rs` (no byte-identical pair) — the self-glob over
`css_l4_*` is gone (single file, unfalsifiable), exactly as SPEC §3.3 specifies.

## The collapse (5 changes)

### 1. RuntimeTarget 7->1 row collapse — `skinny/xtask/src/regen_css.rs`
- `TARGETS` reduced from 7 `RuntimeTarget` rows to ONE (`css_l4_declaration_values`).
- Deleted the 6 redundant `check_*` fns (`check_at_rules_and_media`,
  `check_declaration_values_extended`, `check_nested_layout`,
  `check_stylesheet_selectors`, `check_vendor_and_custom_atrules`,
  `check_visual_functions`); kept `check_declaration_values`.
- Test `css_l4_roster_has_seven_distinct_companions` (asserted 7) replaced with
  `css_l4_roster_is_one_collapsed_config` (asserts `TARGETS.len()==1`). The
  `…_names_all_fifteen_sources` test kept (still valid).

### 2. RuntimeTarget PartialEq derive (the R16 co-gate) — `skinny/xtask/src/regen.rs:5`
- `#[derive(Clone, Copy, Debug)]` -> `#[derive(Clone, Copy, Debug, PartialEq, Eq)]`.
- `Eq` is valid: every field is `Eq` (`&'static str`, `&'static [&'static str]`,
  and the three nested types — `RuntimeEmitterKind` (grammar_provider.rs:39),
  `RuntimeFrontendRequirements` (:45), `RuntimeOutputLabels` (:91) all derive
  `PartialEq, Eq`; `Option<RuntimeOutputLabels>` inherits `Eq`).
- This is the FULL-ROW derive recursing into BOTH nested structs
  `frontend_requirements` (#11) AND `output_labels` (#12) automatically — the R16
  recipe-pin that makes `runtime_target_rows_collapsed` structurally checkable.
  A per-grammar branch relocated into ANY field (incl. a nested struct) makes two
  would-be-equal rows compare UNEQUAL, turning the gate RED where arm-grep is blind.

### 3. xtask command dispatch — `skinny/xtask/src/main.rs`
- USAGE string and the `match command` block reduced from the 7
  `check-css-l4-*` subcommands to the one surviving `check-css-l4-declaration-values`
  (6 removed dispatch arms + their USAGE tokens).

### 4. Runtime module decls — deleted 6 replica dirs + decls
- Deleted dirs (full `config.rs`/`generated.rs`/`mod.rs`/`parser.rs`/`sink.rs`):
  `runtime/src/grammars/{css_l4_at_rules_and_media, css_l4_declaration_values_extended,
  css_l4_nested_layout, css_l4_stylesheet_selectors, css_l4_vendor_and_custom_atrules,
  css_l4_visual_functions}/`. Kept `css_l4_declaration_values/` (canonical; regen
  will overwrite it from the collapsed config).
- `runtime/src/lib.rs`: removed the 6 `#[path=…] pub mod generated_css_l4_*;`
  declarations and the 6 `pub use … as css_l4_*` re-exports; kept the canonical
  `generated_css_l4_declaration_values` + its `css_l4_declaration_values` re-export.
- The 6 `#[test] css_l4_*_routes_into_tape` fns re-pointed at the canonical module
  (`crate::grammars::css_l4_declaration_values::parse`) — preserving the feature
  coverage (selectors / extended decls / visual fns / at-rules+media / vendor+custom
  / nested layout), which the parsers were byte-identical for. Renamed to
  `…_route_into_tape` to drop the stale per-module implication. The canonical
  `css_l4_declaration_values_routes_into_tape` + `css_l4_rich_projection_is_lazy_and_typed`
  tests unchanged.

### 5. Consumer re-point — `skinny/crates/bbnf-bench/src/css_l4_w8.rs`
The ONLY compile-breaking consumer (live `use runtime::{generated_css_l4_*}` of the
6 deleted modules). Collapsed:
- Import reduced to `use runtime::generated_css_l4_declaration_values`.
- `TRACK1_PROFILES` collapsed 7->1 (the canonical `declaration_values` profile).
  The W8 diagnostic's `generated_full_parse_marker` matches `parse_full` output by
  the profile's `row_id`; since all parsers are now the canonical one (output
  `row_id = css_l4/declaration_values/…`), a 7-entry table would mismatch 6 of 7
  and break `track1_full_parse_runs == track1_profile_runs`. Collapsing the table
  to 1 keeps the diagnostic self-consistent — all counts derive from
  `TRACK1_PROFILES.len()`; `W8_SELECTED_CSS_ROWS=24` is an independent constant.
- Deleted the 6 now-dead per-companion `parse_*` fns; kept `parse_declaration_values`.

## Consumers NOT re-pointed (reported, not guessed — over P3's owner-path scope)

SPEC §3.3 owner paths are `regen_css.rs` + `regen.rs` + the runtime grammar dirs.
The following reference the 6 deleted profile/module NAMES only as STRING LITERALS
(they COMPILE cleanly — no live Rust-path import — but are semantically stale gate
content). They encode the "7 distinct companions" SOTA-gate semantics the SPEC
routes to the G-waves (G2 CSS lowering re-derives the scan + its companion gates),
NOT to P3. Re-shaping their gate semantics by guess would invent gate meaning and
violate `[no-workarounds]`, so they are surfaced for the G2/gate owner:

- `crates/bbnf-bench/src/report.rs` — per-companion SOTA validators
  (`.contains("generated_css_l4_<name>::parser::parse")` at ~7836/7996/8156/8316/
  8479/8643; `_extended` validator strings 11416/11491; `"cargo xtask check-css-l4-*"`
  command lists 1496-1502, 9534-9540). Stale: the per-companion rows + xtask
  subcommands they validate no longer exist.
- `crates/bbnf-bench/src/bin/gate.rs` — per-companion retained-artifact validators +
  lane names (`track1_generated_css_l4_<name>` at 1234/1289/1344/1399/1454/1509;
  `validate_css_l4_<name>_retained_artifacts` fns). Stale: lanes/artifacts pinned
  to the 6 deleted companions.
- `crates/bbnf-bench/src/lock14_baseline.rs` — per-companion provider/template path
  lists (`crates/codegen/src/css_l4_<name>_provider.rs`, `…_templates/…`) at
  835-936, 995-1008, 1154-1168, 3165. These reference codegen provider/template
  paths that are a SEPARATE surface (NOT the runtime replica dirs P3 deletes) and
  are out of P3's scope.
- `crates/codegen/src/lib.rs:304` — `CSS_PROFILE_IDS` 7-entry list inside
  `#[cfg(test)] mod tests` (test-only; W5A codegen request-shape unit tests). Compiles;
  tests assert request shape, not runtime module existence.
- `xtask/tests/skv15_w0.rs:166,186` — `generated_css_l4_stylesheet_selectors` inside a
  markdown-row test fixture (string data). Compiles; stale fixture content.

These are the SOTA-companion-gate surface (`css_l4_w8.rs` was its only live-import
member, now collapsed). The full de-overfit of the per-companion gate/report/lane
validators belongs to G2 (which re-derives the CSS scan and owns the companion
gates), per SPEC §2 wave manifest. P3 lands the structural collapse + the R16
co-gate; the gate-semantic cleanup of the per-companion validators is flagged here
for the G2 owner rather than guessed.

## Falsifier (orchestrator verifies post-regen)
- `md5 …/grammars/{json,css_l4}/generated.rs | sort | uniq -c` => NO byte-identical
  pair (the css_l4_* replica set is gone; the singular css_l4 output distinct from
  json). `generated_md5_distinct == true`.
- `RuntimeTarget` derives `PartialEq` (regen.rs:5) — `runtime_target_rows_collapsed`
  structurally checkable via the full-row derive over BOTH nested structs.
- Row-collapse count: `count(distinct config-tuple minus (output_dir,
  expected_files)) per grammar_name == 1 for css_l4` (TARGETS.len()==1).
- `cargo xtask regen --check` exit 0 after regen (orchestrator).

Outcome: P (prune-clean: deletion + R16 co-gate add; zero generalization risk; no
>SOTA-bearing code deleted — the canonical `track1_rich` path is preserved).
