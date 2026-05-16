# SK-V7 Wave 9 R3 - Codegen Lowering Research

Date: 2026-05-16
Scope: research only; no Rust source edits.

## Findings

1. The W9 owner path explicitly puts `codegen/src/lower/mod.rs` on the hook for a `ShapeLowering` trait that consumes `CostFacts`, with W9 exit requiring populated JSON CostFacts and `gate-json --with-cost-facts` output (`restart/skinny/tranches/sk-v7/SPEC.md:343`, `restart/skinny/tranches/sk-v7/SPEC.md:345`, `restart/skinny/tranches/sk-v7/SPEC.md:348`, `restart/skinny/tranches/sk-v7/SPEC.md:358`, `restart/skinny/tranches/sk-v7/SPEC.md:360`). The current module does not yet define that trait: it only exposes the lowerer modules and re-exports `lower_to_rust`/`LowerCtx` (`skinny/crates/codegen/src/lower/mod.rs:1`, `skinny/crates/codegen/src/lower/mod.rs:9`).

2. The active lowering context is still shape-only. `LowerCtx` carries `backend_shape` plus diagnostics, `lower_to_rust` enumerates backend rules, and `shape_for` indexes `RuleId(index)` with an `EagerTape` fallback (`skinny/crates/codegen/src/lower/rust.rs:20`, `skinny/crates/codegen/src/lower/rust.rs:26`, `skinny/crates/codegen/src/lower/rust.rs:31`, `skinny/crates/codegen/src/lower/rust.rs:41`). That index coupling matters because `BackendIr` stores `rules: Vec<BackendRule>` and `BackendRule` itself has no `RuleId` field (`skinny/crates/ir/src/lib.rs:386`, `skinny/crates/ir/src/lib.rs:391`, `skinny/crates/ir/src/lib.rs:404`).

3. The producer still writes `layout_facts.backend_shape` directly from `derive_backend_shape_with_diagnostics` (`skinny/crates/passes/src/lib.rs:42`, `skinny/crates/passes/src/lib.rs:48`). `LayoutFacts` has no CostFacts field yet (`skinny/crates/passes/src/lib.rs:77`, `skinny/crates/passes/src/lib.rs:83`), while B2 requires `cost_facts` to become the source of truth and `backend_shape` to remain only a projection guarded by a parity assertion (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:140`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:155`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:159`).

4. The current shape decision tree is a match ladder over gates, with silent `OffsetTape` fallback and diagnostics only for collapsed-stage waiver absence or missing backend rule (`skinny/crates/passes/src/lib.rs:380`, `skinny/crates/passes/src/lib.rs:389`, `skinny/crates/passes/src/lib.rs:392`, `skinny/crates/passes/src/lib.rs:395`, `skinny/crates/passes/src/lib.rs:396`, `skinny/crates/passes/src/lib.rs:407`, `skinny/crates/passes/src/lib.rs:408`, `skinny/crates/passes/src/lib.rs:410`). B2 calls out this exact producer-drift risk and recommends encoding the priority table as data so each gate and rejection record stay coupled (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:506`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:508`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:515`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:516`).

5. `SinkOnly` is the only lowerer with a real BIR walk today. `sink_only::lower_program` traverses every backend rule, collects DirectBuild shapes, literals, span kinds, and dispatch-alt counts, and returns `None` only when there are no DirectBuild shapes (`skinny/crates/codegen/src/lower/sink_only.rs:97`, `skinny/crates/codegen/src/lower/sink_only.rs:101`, `skinny/crates/codegen/src/lower/sink_only.rs:113`, `skinny/crates/codegen/src/lower/sink_only.rs:117`). The four other shape files still only format diagnostic strings (`skinny/crates/codegen/src/lower/eager_tape.rs:3`, `skinny/crates/codegen/src/lower/offset_tape.rs:3`, `skinny/crates/codegen/src/lower/event_tape.rs:3`, `skinny/crates/codegen/src/lower/collapsed_stage.rs:3`).

6. Generated parser output is not produced from `RuleLoweringPlan.body`. `emit_with_layout` calls `lower_to_rust`, then appends `sink_direct::render(sink_only)` to the existing `generated_rs()` template (`skinny/crates/codegen/src/lib.rs:98`, `skinny/crates/codegen/src/lib.rs:104`, `skinny/crates/codegen/src/lib.rs:113`, `skinny/crates/codegen/src/lib.rs:119`, `skinny/crates/codegen/src/lib.rs:215`). This makes the first safe W9 codegen move a thread-through and parity assertion, not a behavioral renderer rewrite.

7. The requested `lower/sink_direct.rs` and `lower/typed_direct.rs` paths do not exist in the current tree. The lower module contains `schema_direct`, while the renderers live at crate root as `sink_direct` and `typed_direct` (`skinny/crates/codegen/src/lower/mod.rs:6`, `skinny/crates/codegen/src/lower/mod.rs:7`, `skinny/crates/codegen/src/lib.rs:1`, `skinny/crates/codegen/src/lib.rs:3`, `skinny/crates/codegen/src/lib.rs:4`). `schema_direct` only validates a `SinkOnlyProgram` plus `DirectSchemaSet` and copies the direct-shape roster (`skinny/crates/codegen/src/lower/schema_direct.rs:11`, `skinny/crates/codegen/src/lower/schema_direct.rs:15`, `skinny/crates/codegen/src/lower/schema_direct.rs:16`, `skinny/crates/codegen/src/lower/schema_direct.rs:22`).

8. The byte-identical constraint is load-bearing. W8 REDRESS says codegen shell neutralization intentionally kept emitted JSON parser output byte-identical and verified no diffs in `generated.rs`, `generated_real_typed.rs`, or `RESULTS.md` (`skinny/REDRESS.md:2441`, `skinny/REDRESS.md:2452`, `skinny/REDRESS.md:2453`, `skinny/REDRESS.md:2455`). Current `sink_direct` uses the prelude-provided direct tiny-string helper, and the template fixes retained at cap 16 and direct at cap 8 (`skinny/crates/codegen/src/sink_direct.rs:315`, `skinny/crates/codegen/src/sink_direct.rs:325`, `skinny/crates/codegen/src/json_templates/generated.rs:161`, `skinny/crates/codegen/src/json_templates/generated.rs:166`, `skinny/crates/codegen/src/json_templates/generated.rs:167`).

9. REDRESS 72 is the concrete CostFacts capacity hazard: cap 16 was admitted only for generated retained `OffsetTape`, while generated direct `SinkOnly`, hand retained Track 2, and hand direct Track 2 stayed at cap 8 after direct guard regressions (`skinny/REDRESS.md:1996`, `skinny/REDRESS.md:2001`, `skinny/REDRESS.md:2045`, `skinny/REDRESS.md:2049`, `skinny/REDRESS.md:2051`, `skinny/REDRESS.md:2052`). Any CostFacts plumbing that normalizes this into a grammar-wide cap would recreate the route B2 says the substrate is meant to prevent (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:34`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:35`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:36`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:37`).

10. The W9 rule-count gate needs care: SPEC says seven JSON rules must have CostFacts (`restart/skinny/tranches/sk-v7/SPEC.md:358`, `restart/skinny/tranches/sk-v7/SPEC.md:359`), while B2 says "currently 7" but lists nine names (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:470`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:472`). The falsifiable gate should derive the current roster from compiler facts rather than hard-code either prose count.

## Recommendations

1. Add `CostFacts` to `LowerCtx` as a read-only side table, but keep `backend_shape` during the W9 transition:

   ```rust
   pub struct LowerCtx<'a> {
       pub backend_shape: &'a HashMap<RuleId, BackendShape>,
       pub cost_facts: &'a HashMap<RuleId, CostFacts>,
       pub diagnostics: &'a [PassDiagnostic],
   }
   ```

   Lookup should be by the same `RuleId(index)` used today, then assert `backend_shape[rule_id] == cost.chosen` before selecting a lowerer. For legacy `emit(BackendIr)`, either synthesize projection-only CostFacts from `default_backend_shape` or keep a narrow fallback that is not used by `emit_from_source`. Falsifiability gate: a debug/unit test with deliberately mismatched `backend_shape` and `cost_facts.chosen` must fail or emit the W9 inconsistency diagnostic; the normal JSON path must produce byte-identical files.

2. Implement `ShapeLowering` as a trait adapter over the existing free-function lowerers before changing renderer behavior. `select_lowering(&CostFacts)` should switch on `cost.chosen`, and each shape lowerer should receive `(&mut LowerCtx, &BackendRule, &CostFacts)` as B2 specifies (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:305`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:312`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:324`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:329`). Falsifiability gate: `RuleLoweringPlan { rule, shape, body }` must be byte-for-byte identical to the current output when CostFacts are the projection of existing `backend_shape`.

3. Do not filter `sink_only::lower_program` by `cost.chosen` in the first W9 codegen patch. The current renderer requires a whole-program `SinkOnlyProgram`, and `emit_with_layout` fails if that program is absent (`skinny/crates/codegen/src/lib.rs:113`, `skinny/crates/codegen/src/lib.rs:114`, `skinny/crates/codegen/src/lib.rs:119`). Falsifiability gate: `direct_parser_is_authored_from_sink_only_lowering` remains green and `generated.rs` retains the same sink-only header and `parse_direct` body (`skinny/crates/codegen/src/lib.rs:309`, `skinny/crates/codegen/src/lib.rs:329`, `skinny/crates/codegen/src/lib.rs:331`, `skinny/crates/codegen/src/lib.rs:332`).

4. Keep CostFacts thread-through separate from capacity-policy consumption. B2 says lowerers eventually consume `CapacityPolicy` and rejected alternatives so thresholds are not reinvented (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:461`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:462`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:463`), but the first codegen lowering change should not alter `match_tiny_plain_string` caps, `DirectTypeRef::capacity_hint`, or typed-direct helper output (`skinny/crates/codegen/src/direct_schema.rs:60`, `skinny/crates/codegen/src/direct_schema.rs:64`, `skinny/crates/codegen/src/direct_schema.rs:69`, `skinny/crates/codegen/src/typed_direct.rs:311`, `skinny/crates/codegen/src/typed_direct.rs:317`, `skinny/crates/codegen/src/typed_direct.rs:344`). Falsifiability gate: `git diff --exit-code -- skinny/crates/runtime/src/grammars/json/generated.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/RESULTS.md` after regeneration.

5. Make the W9 generated-output gate use live commands. Older REDRESS says `cargo run -p xtask --release -- gen --check` is not a live command in this workspace and `check-json` is the on-disk parity gate (`skinny/REDRESS.md:1568`, `skinny/REDRESS.md:1571`, `skinny/REDRESS.md:1573`, `skinny/REDRESS.md:1574`); W8 also used `check-json`, `check-real-typed`, `check-conformance`, workspace tests, and root `cargo xtask regen --check` (`skinny/REDRESS.md:2446`, `skinny/REDRESS.md:2447`, `skinny/REDRESS.md:2448`, `skinny/REDRESS.md:2450`, `skinny/REDRESS.md:2451`). Falsifiability gate: at minimum run `cargo test -p codegen -p passes -p ir`, `cargo run -p xtask --release -- check-json`, `cargo run -p xtask --release -- check-real-typed`, and root `cargo xtask regen --check`.

6. Drive `gate-json --with-cost-facts` from serialized CostFacts, not from generated Rust text. B2 already requires serde-exported CostFacts and CI assertions over that table (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:433`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:434`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:435`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:466`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:481`). Falsifiability gate: every current shape-roster rule has CostFacts, every chosen shape has one rejection per non-chosen shape, REDRESS 72 appears as `EvidenceSource::RedressBackfill`, and no silent `DefaultOffsetTape` record passes without `BBNF-COSTFACTS-MISSING-EVIDENCE` (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:477`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:479`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:483`).

## Risks/pre-blocked routes

1. Producer drift is the top implementation risk. If the priority walker and CostFacts record are maintained separately, future edits can change shape selection without updating rejected-alternative evidence (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:508`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:509`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:511`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:513`).

2. Do not reopen pre-blocked retained/direct-materialization routes. HANDOFF section 3 blocks REDRESS 60-72 retained parse and direct-materialization routes, including parser-owned decoded scratch, byte-output unescape, and DirectBuild semantic string facts (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`, `restart/skinny/tranches/sk-v7/HANDOFF.md:75`, `restart/skinny/tranches/sk-v7/HANDOFF.md:78`, `restart/skinny/tranches/sk-v7/HANDOFF.md:79`, `restart/skinny/tranches/sk-v7/HANDOFF.md:80`).

3. Do not revive old parallel or sidecar substrates. HANDOFF section 3 separately blocks EventCursor parallel prepass and other earlier rejected routes such as capacity prescan, separator elision, raw f64 shortcut, and generic SWAR whitespace skipper (`restart/skinny/tranches/sk-v7/HANDOFF.md:84`, `restart/skinny/tranches/sk-v7/HANDOFF.md:88`, `restart/skinny/tranches/sk-v7/HANDOFF.md:89`, `restart/skinny/tranches/sk-v7/HANDOFF.md:90`, `restart/skinny/tranches/sk-v7/HANDOFF.md:91`, `restart/skinny/tranches/sk-v7/HANDOFF.md:93`).

4. Do not use CostFacts as permission to perturb generated output during the thread-through. W8 closed by proving generated JSON outputs stayed unchanged and explicitly routed W9 to the CostFacts substrate before further route-fact decisions (`skinny/REDRESS.md:2452`, `skinny/REDRESS.md:2455`, `skinny/REDRESS.md:2460`, `skinny/REDRESS.md:2463`, `skinny/REDRESS.md:2464`).

5. Do not move root `sink_direct.rs` or `typed_direct.rs` under `lower/` as part of the CostFacts thread-through unless a separate byte-identical shell-neutralization patch owns that move. The current crate structure distinguishes `lower::schema_direct` from root renderers (`skinny/crates/codegen/src/lower/mod.rs:6`, `skinny/crates/codegen/src/lib.rs:1`, `skinny/crates/codegen/src/lib.rs:3`, `skinny/crates/codegen/src/lib.rs:4`), and moving renderers risks output churn unrelated to CostFacts.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md` section 11.
- `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md`.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` section 3.
- `skinny/REDRESS.md`.
- `skinny/crates/codegen/src/lower/mod.rs`.
- `skinny/crates/codegen/src/lower/rust.rs`.
- `skinny/crates/codegen/src/lower/sink_only.rs`.
- `skinny/crates/codegen/src/lower/schema_direct.rs`.
- `skinny/crates/codegen/src/sink_direct.rs`.
- `skinny/crates/codegen/src/typed_direct.rs`.
- `skinny/crates/codegen/src/direct_schema.rs`.
- `skinny/crates/codegen/src/lib.rs`.
- `skinny/crates/codegen/src/lower/eager_tape.rs`.
- `skinny/crates/codegen/src/lower/offset_tape.rs`.
- `skinny/crates/codegen/src/lower/event_tape.rs`.
- `skinny/crates/codegen/src/lower/collapsed_stage.rs`.
- `skinny/crates/passes/src/lib.rs`.
- `skinny/crates/passes/src/diagnostics.rs`.
- `skinny/crates/ir/src/lib.rs`.
