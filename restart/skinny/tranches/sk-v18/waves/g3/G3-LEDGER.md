# SK-V18 G3 — LEDGER: un-fork the emitter (RuntimeEmitterKind DELETED, artifact-type dispatch)

Date: 2026-06-02. Wave G3 (§6, R-A A). Entry gate G1 ∧ G2 closed ∧ P4 live ∧ P3 row-collapse — all GREEN.
Implementation per research.md option (c) + AMENDMENT.md (conjunct-5 widened to lowered-artifact).

---

## THE UN-FORK (what changed)

The grammar-family fork `match request.profile_contract.emitter { CompiledLowering => …, RequestFacts => … }`
(`runtime_generator.rs:16`) is GONE. `emit_from_request` now reads:

```rust
let artifact = lower_for_runtime(request, facts)?;   // shape-DERIVED selection
render(&artifact, request, facts)                    // ONE grammar-agnostic dispatch
```

- `LoweredArtifact { SinkOnly(SinkOnlyProgram) | Scan(CssScanProgram) }` — the neutral lowered-artifact
  sum type (shape-named variants, NOT grammar-named).
- `lower_for_runtime` picks the variant on `facts.first_unsupported().is_none()` — the SAME construct-KIND
  predicate that already gated lowering. Lowerable grammar (JSON) → `lower_sink_only_from_source` →
  `SinkOnly`; non-lowerable grammar (CSS, with `@token`/`@pretty`/`->`) → `css_scan::derive_program` →
  `Scan`. The variant is COMPUTED, never read from a config field.
- `render` matches the variant → `emit_sink_only` (was `emit_compiled`, body unchanged) / `emit_scan`
  (was `emit_request_facts`, body unchanged).

**What `RuntimeEmitterKind` became:** DELETED outright. The enum + `CompiledLowering`/`RequestFacts`
variants are gone; the `emitter` field is removed from `RuntimeProfileContract` (codegen) and
`RuntimeTarget` (xtask) + both row literals (`main.rs` JSON_TARGET, `regen_css.rs` CSS TARGETS). The
routing decision MOVED from a stored config tag to the computed `first_unsupported` lowering predicate.
The `lib.rs` re-export + the now-unreachable `UnsupportedRuntimeConstruct` error variant (only produced
by the deleted lowering-skip gate) were removed. The two per-arm rosters
(`COMPILED_RUNTIME_FILES`/`REQUEST_FACTS_RUNTIME_FILES`) became the each-renderer-owns-its-roster
absorption (candidate B); `REQUEST_FACTS_RUNTIME_FILES` un-gated from `#[cfg(test)]`.

Files touched (path change only, no emit-LOGIC change): `crates/codegen/src/runtime_generator.rs`,
`crates/codegen/src/grammar_provider.rs`, `crates/codegen/src/grammar_profile.rs`,
`crates/codegen/src/lib.rs`, `crates/codegen/src/lower/css_scan.rs` (prose comment only),
`xtask/src/regen.rs`, `xtask/src/main.rs`, `xtask/src/regen_css.rs`. Generated output regenerated, BYTE-IDENTICAL.

---

## §6 EXIT-GATE TELEMETRY

| Telemetry | Value | Evidence |
|---|---|---|
| `emitter_fork_present` | **false** | `git grep -E 'RuntimeEmitterKind\|CompiledLowering\|RequestFacts' crates/codegen/src xtask/src` == **0** matches |
| `generator_grammar_branch_count` | **0** | arm-census over `emit_from_request`+`lower_for_runtime`+`render` bodies: no `match grammar`/`grammar_name ==`/`profile ==`/`"json"`/`"css"`/`"google_sheets"` |
| `generator_grammar_type_count` | **0** | no `*Emitter`-per-grammar type; only `LoweredArtifact{SinkOnly,Scan}` (shape-named) + pre-existing `SinkOnlyProgram`/`CssScanProgram` |
| `emit_shape_source` firewall | **lowered_program** (amended) | `lower_for_runtime`+`render` code bodies: **ZERO** `target.*`/`contract.*` reads (only doc-comment prose mentions the word "firewall"). Variant is COMPUTED (`first_unsupported`), not CONFIGURED. SinkOnly arm reads `program.policy_summary.backend_shape`; Scan arm reads the program's `alphabet`/`at_rule_branches` |
| `runtime_target_rows_collapsed` | **true** | `RuntimeTarget` still derives `#[derive(Clone, Copy, Debug, PartialEq, Eq)]` (`regen.rs:5`) after the `emitter` field removal (count 12→11); nested structs `RuntimeFrontendRequirements`/`RuntimeOutputLabels` still derive `PartialEq, Eq`. No hand-rolled prose comparison |
| `g3_output_byte_identical[json]` | **true** | all 8 `json/*.rs` byte-identical to `git show HEAD:`; `git status` of dir CLEAN; `check-json` exit 0 |
| `g3_output_byte_identical[css]` | **true** | all 5 `css_l4_declaration_values/*.rs` byte-identical to `git show HEAD:`; `git status` of dir CLEAN; `check-css-l4-declaration-values` exit 0 |
| `g3_all_tests_green` | **true (G3-attributable)** | build exit 0; codegen lib 34/0, runtime lib 22/0, bbnf-bench rich_cssom 2/0, lock14_baseline 101/1 — the 1 failure (`accepts_current_allowlist`) is a PRE-EXISTING G2-commit parent-diff defect (PROVEN identical on clean pre-G3 HEAD), out of G3 scope; every G3-attributable test is GREEN |

---

## VERIFICATION RESULTS

- **Build:** `cargo build --profile ax-iter -p codegen -p runtime -p xtask -p bbnf-bench` — exit 0
  (only pre-existing patch-not-used / panic-on-bench warnings, no errors).
- **Regen-clean:** `regen-json` + `regen-css` ran; `check-json` exit 0, `check-css-l4-declaration-values`
  exit 0.
- **Byte-equivalence:** all 13 generated files (8 json + 5 css) byte-identical to the pre-G3 committed
  state (`git show HEAD:`). The un-fork is a PURE PATH CHANGE.
- **Codegen tests:** lib-unittests 34/34 passed, 0 failed (incl. `w5a_json_request_matches_emit_from_source`,
  `css_l4_frontend_profiles_are_request_generated`, `css_l4_generated_runtimes_reproducible_from_request`).
- **Runtime tests:** lib-unittests 22/22 passed, 0 failed (incl. all `css_l4_*_route_into_tape`,
  `css_l4_rich_projection_is_lazy_and_typed`, JSON projection/scan).
- **bbnf-bench rich_cssom:** 2/2 passed, 0 failed (`rich_cssom_matches_cssparser_on_fixture`,
  `rich_cssom_matches_cssparser_on_real_corpora` — the 9-field parity oracle GREEN).
- **bbnf-bench lock14_baseline:** 101 passed, 1 failed — the SINGLE failure is
  `accepts_current_allowlist`, which is a **PRE-EXISTING G2-commit defect, NOT a G3 regression**.
  PROVEN: on a fully-clean pre-G3 HEAD (all 8 G3 files reverted, working tree pristine) the test
  STILL fails 0/1 with the IDENTICAL error. The failure is the `validate_parent_frozen_diff`
  (`git diff HEAD^ HEAD`) check, which reads ONLY committed history (HEAD = the G2 commit
  `ed538afd7`) and is INVARIANT to the working tree. Root cause: the G2 commit modified
  `crates/bbnf-bench/src/lock14_baseline.rs` (to ADD `SK_V18_G1_G2_EMITTER_OWNER_PATHS`) but omitted
  `crates/bbnf-bench/src/lock14_baseline.rs` from that very allowlist, so the parent-diff rejects that
  path. The other 101 lock14 sub-tests (incl. the W6 root-rejection battery, the courier-leak scan,
  the backend-shape surface) PASS. The mandate's "102/0" assumption does not hold against the actual
  committed G2 HEAD; this is out of G3 scope (a G2 freeze-allowlist omission) and the G3 un-fork
  provably neither caused nor can fix it without committing (forbidden).

---

## §6 FINDING (the amendment, recorded in AMENDMENT.md)

G3 is a PATH change with byte-equivalent OUTPUT. The only §6 deviation is the conjunct-5 amendment
(already ratified in `waves/g3/AMENDMENT.md`): `emit_shape_source` widens from "shape from
`program.policy_summary.backend_shape`" to "shape from the LOWERED ARTIFACT (the `LoweredArtifact`
variant for the Scan path ∨ `policy_summary.backend_shape` for the SinkOnly path), NEVER a
`target.*`/`contract.*` field". This is forced by the ratified G2 LOWERING GAP: CSS provably never
lowers (`first_unsupported` rejects `@token`/`@pretty`/`->`), so it carries a `CssScanProgram` with NO
`policy_summary` — there is no single field name the CSS arm could read. The binding firewall (no
`target.*`/`contract.*` read) is PRESERVED verbatim and is strictly STRONGER here (the grep scans both
`render` AND `lower_for_runtime` bodies). Synthesizing a `BackendShape` for CSS to satisfy the literal
wording was REJECTED as a fabricated-shape relocated seam (the L1 reject).

No new finding beyond the recorded amendment. The un-fork is a genuine single neutral path: the variant
is the OUTPUT of the shape-derived lowering pipeline, not a relocated grammar tag in a data table.

NOTE: the 6 pre-existing report/json_w9 lib-unittest failures are KNOWN out-of-scope and were not touched.
NOT committed (per mandate).
