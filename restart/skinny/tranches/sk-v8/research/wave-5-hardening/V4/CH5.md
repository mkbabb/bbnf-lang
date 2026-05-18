# SK-V8 W5 Hardening V4 CH5 - Hidden Coupling

Target: `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22` (`docs(sk-v8-wave5-plan): route V4 after V3 revise`).

Verdict: ACCEPT

Confidence %: 95

## Findings

1. The V4 target introduces no directive, BIR, or substrate surface. The target
   diff is limited to the W5 plan and W5 research markdown; there are no
   source, generated-output, runtime, parser, IR, pass, SIMD, xtask,
   `HANDOFF.md`, `SPEC.md`, or `SYNTHESIS.md` changes.
2. No `UnionTape` or `BackendShape` drift is hidden by the routing fold. The
   current IR surface still has only `EagerTape`, `OffsetTape`, `EventTape`,
   `SinkOnly`, and `CollapsedStage`; Lock 14 still checks for exactly that
   five-variant surface and rejects `UnionTape` residue.
3. No sidecar/substrate relabeling is introduced. The target does not modify
   benchmark/report/substrate surfaces, SIMD comments, generated output, or
   source code. The W5 plan continues to frame CH5 as a hidden-coupling review,
   not as permission to relabel sidecar or substrate evidence.
4. No Track 1 / Track 2 coupling is introduced. The plan keeps generated output
   and `skinny/RESULTS.md` out of scope, repeats zero-drift requirements for
   generated JSON, generated typed output, direct guard source, and generic
   crate surfaces, and records Track 2 only as existing `RESULTS.md` authority.
5. No generic JSON policy leaks through the provider boundary. Generic codegen
   scans excluding `json_provider.rs` and `json_templates/**` returned no JSON
   grammar-name branch, JSON template include, runtime JSON include, or
   `backend.grammar_name` matches; provider residency remains confined to
   `json_provider.rs` and xtask generated-output tooling.
6. The target does not dispatch W6. The plan routes V4 after the V3 CH1 REVISE
   and still requires one more unchanged qualifying ACCEPT cycle before W5 may
   close.

## Verification/Evidence

- `git rev-parse HEAD` resolved to the target
  `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`.
- `git show --name-status d3398a68` listed only
  `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md`
  and `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md`.
- `git diff --name-only d3398a68^ d3398a68 -- skinny
  restart/skinny/tranches/sk-v8/HANDOFF.md
  restart/skinny/tranches/sk-v8/SPEC.md
  restart/skinny/tranches/sk-v8/SYNTHESIS.md` returned no paths.
- `git diff --exit-code d3398a68^ d3398a68 --` over `skinny/RESULTS.md`,
  generated JSON output, generated typed output, direct guard source, IR,
  codegen, passes, parse-that-regex, SIMD, runtime, skinny bbnf, and xtask
  paths returned clean.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:30-36` keeps
  generated output and `skinny/RESULTS.md` out of scope while describing only
  the named provider-boundary cleanup; `:75-85` confines allowed JSON surfaces
  and zero-drift requirements; `:128-139` names CH5 hidden coupling and requires
  another qualifying cycle before close.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:188-210`
  records the V3 fold as source-unchanged, routes V4, and keeps the challenge
  focused on provider-boundary cleanup, REDRESS anchors, generic JSON policy,
  generated drift, and paper-close risk.
- `skinny/crates/ir/src/lib.rs:402-407` contains the five current
  `BackendShape` variants only. `skinny/crates/bbnf-bench/src/lock14_baseline.rs:530-558`
  enforces the five-variant surface and rejects `UnionTape`; `:188-193` keeps
  `json_provider.rs` classified as `per_grammar_provider`; `:477-485`
  authorizes W5 parent diffs only under the named W5 owner paths.
- The forbidden-symbol scan for old/renamed JSON policy,
  `StructuralAlphabet::json`, `UnionTape`, `BackendShape::Union`, and
  `BackendShape::Json` returned no matches outside allowed generated
  JSON/template surfaces.
- The generic provider-policy scan over `skinny/crates/codegen/src`, excluding
  `json_provider.rs` and `json_templates/**`, returned no matches.
- The provider-residency scan returned only xtask generated-output tooling at
  `skinny/xtask/src/main.rs:124`, `:132`, `:183` and provider includes at
  `skinny/crates/codegen/src/json_provider.rs:57`, `:61`.
- The focused directive/BIR/substrate/sidecar scan found no target-diff change.
  Current live code matches are pre-existing Lock 14 rejection checks and
  sink-only renderer diagnostics, not new W5 coupling.

## Required Folds

None for CH5.

Do not dispatch W6 from this ACCEPT. W5 still needs the next unchanged
qualifying challenge cycle before close.
