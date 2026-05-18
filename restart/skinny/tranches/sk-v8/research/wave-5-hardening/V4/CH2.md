# SK-V8 W5 Hardening V4 CH2 - Generality

Target: `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`.

Verdict: ACCEPT.

Confidence %: 96.

## Findings

1. Target `d3398a68` does not reopen the CH2 provider-boundary issue. The
   target changes only W5 plan/research markdown, routing V4 after the V3
   REVISE; source, generated output, and `skinny/RESULTS.md` are unchanged.
2. The live provider boundary remains correctly classified. JSON-specific
   runtime-profile gating and JSON template/runtime includes live in
   `skinny/crates/codegen/src/json_provider.rs`, while
   `skinny/crates/codegen/src/lib.rs` delegates provider material and remains a
   generic surface. The Lock 14 baseline classifies `json_provider.rs` as
   `per_grammar_provider`, admits that class, and limits the W5 parent-diff
   allowance to `crates/codegen/src/lib.rs` plus
   `crates/codegen/src/json_provider.rs`.
3. No generic JSON policy or renamed JSON-policy surface was found in audited
   generic roots. The focused scan returned no matches for the W5 blocked
   helper/policy names, `StructuralAlphabet::json`, `UnionTape`,
   `union_tape`, `BackendShape::Union`, or `BackendShape::Json`, after
   excluding generated JSON output and JSON templates.
4. Grammar-branch/provider scans remain clean enough for CH2. The generic
   codegen scan excluding `json_provider.rs` and `json_templates/**` returned
   no matches for `grammar_name == "json"`, `backend.grammar_name`, JSON
   template includes, or `runtime/src/grammars/json` includes. The residency
   scan returned only xtask generated-output tooling plus
   `json_provider.rs` runtime includes.
5. REDRESS 36-38/85/86 closure is now anchored at the active W5 assertions:
   REDRESS 36-38 (`skinny/REDRESS.md:460-515`) are treated as historical
   violation records, reconciled by REDRESS 85
   (`skinny/REDRESS.md:2399-2427`) and REDRESS 86
   (`skinny/REDRESS.md:2431-2464`). The target does not claim an in-place
   REDRESS rewrite or reopen a pre-blocked route.
6. The non-JSON proof remains a no-drift/provider-isolation proof, not a broad
   multi-grammar provider redesign. The packet records root
   `cargo xtask regen --check` clean over all 9 grammars, including CSS L4,
   Google Sheets, and BBNF-self; this target does not change the source or
   generated files behind that evidence.

## Verification/Evidence

- `git rev-parse HEAD` resolved to
  `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`.
- `git diff --name-status d3398a68^ d3398a68` returned only:
  `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md`
  and `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md`.
- `git diff --check d3398a68^ d3398a68` returned clean.
- `git diff --name-status b71a8aed d3398a68 -- skinny/crates/codegen/src/lib.rs
  skinny/crates/codegen/src/json_provider.rs
  skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/RESULTS.md
  skinny/crates/runtime/src/grammars/json
  skinny/crates/codegen/src/json_templates` returned no paths.
- `skinny/crates/codegen/src/json_provider.rs:4-13` owns the only live
  `backend.grammar_name == "json"` runtime-profile guard in codegen, and
  `:48-73` owns JSON template/runtime includes.
- `skinny/crates/codegen/src/lib.rs:102-136` delegates generated runtime
  material to `json_provider`; it does not carry the JSON guard or includes
  directly.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:188-193`,
  `:411-414`, `:477-485`, and `:562-575` establish the
  `per_grammar_provider` classification and W5-only parent-diff allowance.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:66-85` carries the
  grammar-branch/provider-residency, allowed-surface, REDRESS, zero-drift,
  non-JSON proof, and no-performance-claim gates.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:132-139` routes V4
  after the V3 REVISE and still requires a later unchanged qualifying
  re-challenge before W5 close.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:26-35`
  anchors REDRESS 36-38/85/86 and frames the generic `codegen/src/lib.rs`
  edit as provider delegation plus unchanged-output coverage.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:188-210`
  records the V3 fold, the V4 challenge route, source owner paths, no
  generated/`RESULTS.md` edit, and the hardening focus.
- Read-only forbidden-policy scan returned no matches in the audited generic
  roots.
- Read-only generic grammar-branch scan returned no matches outside
  `json_provider.rs` and `json_templates/**`.
- Read-only provider-residency scan returned only:
  `skinny/xtask/src/main.rs:124`, `:132`, `:183`, and
  `skinny/crates/codegen/src/json_provider.rs:57`, `:61`.
- I did not run cargo tests or regen locally because this assignment restricts
  writes to this CH2 markdown path; those commands may create build artifacts.
  CH2 relies on the packet's recorded post-fold command evidence plus the
  read-only scans/diffs above.

## Required Folds

None for CH2.

This ACCEPT is not W6 dispatch authority. W5 still needs the challenge process
and unchanged qualifying re-challenge required by the W5 plan.
