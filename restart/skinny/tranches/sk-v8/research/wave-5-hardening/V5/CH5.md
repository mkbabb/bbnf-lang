# SK-V8 W5 Hardening V5 CH5 - Hidden Coupling

Target: `42d5f034eee2a1931e46d13e7e20c62e49ca8c7a`
(`docs(sk-v8-wave5-hardening): record V4 qualifying accept cycle`).

Verdict: ACCEPT

Confidence %: 95%

## Findings

1. The V5 target introduces no directive, BIR, or substrate surface. Its parent is
   `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`, and the target diff adds only
   V4 hardening markdown under
   `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V4/`.
2. No `UnionTape` or `BackendShape` drift is hidden in the unchanged packet.
   The live IR still exposes only `EagerTape`, `OffsetTape`, `EventTape`,
   `SinkOnly`, and `CollapsedStage`; Lock 14 still checks exactly that
   five-variant surface and rejects `UnionTape` residue.
3. No sidecar/substrate relabeling is introduced. The target does not modify
   benchmark/report/substrate code, `skinny/RESULTS.md`, generated output,
   SIMD comments, `HANDOFF.md`, `SPEC.md`, or `SYNTHESIS.md`. Existing W0
   sidecar checks still reject `sidecar-same-run` without a structured
   manifest.
4. No Track 1 / Track 2 coupling is introduced. The W5 plan continues to keep
   generated output and `skinny/RESULTS.md` out of scope, keeps CH5 focused on
   hidden coupling, and does not alter Track 1/Track 2 code paths or report
   authority.
5. No generic JSON policy crosses the provider boundary. The generic codegen
   scan excluding `json_provider.rs` and `json_templates/**` found no JSON
   grammar-name branch, template include, runtime JSON include, or
   `backend.grammar_name` match. Provider residency remains limited to xtask
   generated-output tooling plus `json_provider.rs`.
6. The V4 consolidated result is correctly carried as one qualifying ACCEPT
   cycle, not as W6 dispatch. This CH5 re-challenge finds no hidden coupling
   blocker, but CH5 alone is not W6 dispatch authority.

## Verification/Evidence

- `git rev-parse HEAD` returned
  `42d5f034eee2a1931e46d13e7e20c62e49ca8c7a`.
- `git rev-parse 42d5f034^` returned
  `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`.
- `git diff --name-status --no-renames 42d5f034^ 42d5f034` listed only seven
  added V4 hardening files: `CH1.md` through `CH6.md` and
  `HARDENING-W5-V4-CONSOLIDATED.md`.
- `git diff --name-only 6e159f5c 42d5f034 -- skinny/crates skinny/RESULTS.md
  restart/skinny/tranches/sk-v8/HANDOFF.md
  restart/skinny/tranches/sk-v8/SPEC.md
  restart/skinny/tranches/sk-v8/SYNTHESIS.md` returned no paths, so no live
  source/result/spec/handoff surface changed after the V1 provider-boundary
  source fold.
- `git diff --exit-code 42d5f034^ 42d5f034 -- skinny/RESULTS.md
  skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src
  skinny/crates/codegen/src skinny/crates/passes/src
  skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src
  skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src` returned
  clean.
- `skinny/crates/ir/src/lib.rs:401-408` contains only the five current
  `BackendShape` variants. `skinny/crates/bbnf-bench/src/lock14_baseline.rs:530-558`
  enforces the five-variant count and rejects `UnionTape`; `:188-193` keeps
  `json_provider.rs` classified as `per_grammar_provider`; `:477-485`
  authorizes only W5 owner paths for the W5 parent diff.
- The forbidden renamed-policy scan for old JSON helper names,
  `StructuralAlphabet::json`, `UnionTape`, `BackendShape::Union`, and
  `BackendShape::Json` returned no matches outside allowed generated
  JSON/template surfaces.
- The generic provider-policy scan over `skinny/crates/codegen/src`, excluding
  `json_provider.rs` and `json_templates/**`, returned no matches.
- The provider-residency scan returned only generated-output tooling at
  `skinny/xtask/src/main.rs:124`, `:132`, `:183` and provider includes at
  `skinny/crates/codegen/src/json_provider.rs:57`, `:61`.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:30-36` confines the
  W5 cleanup to the named provider-boundary fold; `:66-85` defines the generic
  JSON/provider scans and zero-drift requirements; `:128-139` names CH5 hidden
  coupling and requires the unchanged re-challenge before W5 close.
- `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V4/HARDENING-W5-V4-CONSOLIDATED.md:20-21`
  records V4 as the first qualifying W5 acceptance cycle, and `:50-53` says V4
  does not close W5 or dispatch W6.
- I did not run cargo commands for this CH5 pass because the user constrained
  writes to this markdown file only. This review used read-only git, source, and
  `rg` verification; the target is doc-only over the prior V4 evidence packet.

## Required Folds

None for CH5.

Do not dispatch W6 from this CH5 ACCEPT.
