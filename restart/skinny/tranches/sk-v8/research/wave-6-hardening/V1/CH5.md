# CH5 - Lock 14/Lock 15 And Grammar-Neutral Close Review

Verdict: ACCEPT
Confidence: 96%

## Evidence

- W6 does not silently weaken Lock 14. The W6 research admits no source,
  generated-output, `RESULTS.md`, or `REDRESS.md` change
  (`skv8-W6-close-reconciliation-research.md:5-7`), the W6 plan excludes parser,
  scanner, runtime, codegen, IR, SIMD, generated output, benchmark rows, new
  directives, substrate surfaces, and Lock 1/Lock 14 amendments
  (`skv8-W6-plan.md:39-45`), and the close artifact makes source/generated/result
  drift a falsifier (`skv8-W6-close-and-alpha-feedback.md:97-106`). A read-only
  diff over source, generated, `RESULTS.md`, `REDRESS.md`, SPEC, and HANDOFF
  surfaces returned no paths.
- W5 proof is preserved, not generalized. W6 repeats that W5 admitted only the
  named Lock 14 provider-boundary cleanup with no generated output, row-table,
  performance, or `RESULTS.md` movement (`skv8-W6-plan.md:19-20`, `:61-67`;
  `skv8-W6-close-and-alpha-feedback.md:20`, `:32`). HANDOFF records the concrete
  W5 fold: `codegen/src/lib.rs` delegated JSON provider material to
  `json_provider.rs`, added `per_grammar_provider`, and authorized only the W5
  owner-path parent diff (`HANDOFF.md:231-244`). W5 V5 closed 6/6 ACCEPT with
  minimum 95%, a 148 source/test insertion cleanup, passing Lock 14 baseline,
  clean generic JSON scans, and no generated/result drift
  (`HARDENING-W5-V5-CONSOLIDATED.md:20-57`).
- The executable Lock 14 boundary remains narrow. `lock14_baseline.rs` classifies
  `json_provider.rs` as `per_grammar_provider` (`:188-193`), keeps
  `codegen/src/lib.rs` as `generic_surface` (`:249-253`), authorizes only
  `crates/codegen/src/lib.rs` and `crates/codegen/src/json_provider.rs` for W5
  parent diffs (`:411-414`, `:477-485`), forbids `UnionTape` and enforces the
  five-variant `BackendShape` surface (`:550-558`), and admits provider/template
  classes without adding a generic JSON-policy class (`:562-575`). My read-only
  forbidden-policy and generic-codegen JSON branch scans returned no matches;
  provider residency returned only xtask generated-output tooling and
  `json_provider.rs` includes.
- Grammar-specific cleanup is not treated as generic permission. SPEC requires
  generic codegen to consume grammar-derived facts rather than hard-coded JSON
  policy under neutral names (`SPEC.md:261-286`) and says W5 may fix only named
  Lock 14 drift within cap (`SPEC.md:673-699`). W6 routes broad lock amendments
  and canonical cleanup to Pass Omega (`skv8-W6-close-reconciliation-research.md:68-71`;
  `skv8-W6-close-and-alpha-feedback.md:90-93`), and HANDOFF says Omega cannot
  weaken Lock 14 or authorize generic JSON policy leaks (`HANDOFF.md:304-309`).
- Lock 15 is not reopened or weakened. SPEC/HANDOFF still require Lock 14 and
  Lock 15 gates at close (`SPEC.md:44-60`; `HANDOFF.md:279-287`), while W6 is
  limited to zero source LOC and document reconciliation with no performance
  rerun (`SPEC.md:223-257`; `skv8-W6-plan.md:39-45`, `:92-94`). The current
  release profile still carries `lto = "fat"`, `codegen-units = 1`,
  `panic = "abort"`, and `debug = true` (`skinny/Cargo.toml:73-80`), and W6 does
  not touch Cargo, runtime, codegen, or hot-code surfaces.

## Required Fold

None.
