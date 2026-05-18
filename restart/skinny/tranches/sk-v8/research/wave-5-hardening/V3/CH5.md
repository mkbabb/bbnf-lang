# SK-V8 W5 Hardening V3 CH5 - Hidden Coupling

Target: `b71a8aed2e4bc4ada47a517e93d52cc842551059` (`docs(sk-v8-wave5-hardening): fold V2 redress anchors and cleanup posture`).

Verdict: ACCEPT

Confidence %: 95

## Findings

1. The V3 target adds no new directive, BIR, or substrate surface. The parent
   diff is limited to W5 research/plan text and V2 hardening artifacts; there is
   no source, `HANDOFF.md`, `SPEC.md`, `RESULTS.md`, runtime, generated-output,
   IR, passes, SIMD, parser, or xtask change in `b71a8aed`.
2. No `UnionTape` or `BackendShape` drift is hidden by the V2 documentation
   fold. The current IR still has only `EagerTape`, `OffsetTape`, `EventTape`,
   `SinkOnly`, and `CollapsedStage`, and Lock 14 still enforces the five-variant
   surface plus `UnionTape` absence.
3. No sidecar/substrate relabeling is introduced. The only accepted new class is
   `per_grammar_provider` for `crates/codegen/src/json_provider.rs`; the W5 plan
   continues to describe it as provider-boundary cleanup, not sidecar,
   substrate, directive, or alternate parser production work.
4. No Track 1 / Track 2 coupling is introduced. The target does not alter
   generated JSON runtime output, generated typed output, direct guard source,
   `skinny/RESULTS.md`, or Track 2 authority. The W5 research only carries the
   existing current-results anchors and repeats the no-generated-output/no-row
   movement constraint.
5. No generic JSON policy leaks through the provider boundary. At HEAD,
   `codegen/src/lib.rs` performs a private handoff to `json_provider`; the JSON
   grammar-profile guard and JSON template/runtime include paths reside in
   `json_provider.rs`. Generic codegen scans excluding the provider/templates
   return no `grammar_name == "json"`, `backend.grammar_name`, JSON template
   include, or runtime JSON include matches.
6. V3 CH5 does not dispatch W6. The plan now explicitly says V3 must challenge
   the folded documentation and unchanged source packet, then requires one more
   unchanged qualifying ACCEPT cycle before W5 may close.

## Verification/Evidence

- `git rev-parse HEAD` resolved to target `b71a8aed2e4bc4ada47a517e93d52cc842551059`.
- `git diff --name-status b71a8aed^ b71a8aed` listed only
  `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md`,
  `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md`, and new V2
  hardening markdown files.
- `git diff --name-only b71a8aed^ b71a8aed -- skinny restart/skinny/tranches/sk-v8/HANDOFF.md restart/skinny/tranches/sk-v8/SPEC.md restart/skinny/tranches/sk-v8/SYNTHESIS.md`
  returned no paths.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:30-36` confines the
  implementation fold to provider-boundary cleanup; `:75-85` keeps allowed JSON
  surfaces and generated/RESULTS drift constraints narrow; `:132-137` requires
  V3 plus a further unchanged qualifying cycle before close.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:174-183`
  says the V2 fold keeps code unchanged, records the 148 insertion count, names
  `codegen/src/lib.rs` as delegation only, and preserves exact REDRESS anchors.
- `skinny/crates/codegen/src/lib.rs:108` and `:118-135` delegate runtime
  profile and generated files to `json_provider`; `skinny/crates/codegen/src/json_provider.rs:4-12`
  owns the JSON profile guard and `:48-61` owns JSON template/runtime includes.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:188-193` classifies
  `json_provider.rs` as `per_grammar_provider`; `:477-485` keeps W5 parent-diff
  authorization limited to the named W5 owner paths; `:530-558` enforces the
  five-variant `BackendShape` surface and rejects `UnionTape`.
- `rg` forbidden-symbol scan for old/renamed JSON policy, `UnionTape`,
  `BackendShape::Union`, and `BackendShape::Json` returned no matches.
- Generic provider-policy scan excluding `json_provider.rs` and
  `json_templates/**` returned no matches.
- Provider-residency scan returned only `skinny/xtask/src/main.rs:124`,
  `:132`, `:183`, and `skinny/crates/codegen/src/json_provider.rs:57`, `:61`.
- `rg -n 'sidecar|Sidecar|substrate|Substrate'` over codegen, IR, passes, and
  `lock14_baseline.rs` returned no matches.

## Required Folds

None for CH5.

Carry-forward constraint: do not dispatch W6 from this CH5 ACCEPT.
