# SK-V8 W5 Hardening V2 CH3 - Disposition Integrity

Target: `6e159f5c70aa5b4560d874a0e446587beb8f857e`

Verdict: ACCEPT

Confidence %: 94

## Findings

1. REDRESS 36-38 / 85 / 86 consistency holds for CH3. REDRESS 36-38 remain the
   append-only historical Lock 14 violation records at `skinny/REDRESS.md:460-515`.
   W5 does not claim those old entries were edited in place. It relies on the
   later admitted neutralization records: REDRESS 85 at
   `skinny/REDRESS.md:2399-2427` and REDRESS 86 at
   `skinny/REDRESS.md:2431-2464`.
2. The V2 fold does not reopen the blocked route family. The only source delta is
   the provider-boundary cleanup: `codegen/src/lib.rs` delegates JSON profile and
   template/runtime material to `codegen/src/json_provider.rs`, and
   `lock14_baseline.rs` classifies that file as `per_grammar_provider`.
3. The RESULTS no-change claim is intact. The target commit does not modify
   `skinny/RESULTS.md`; current authority still reports 38 main rows at
   `skinny/RESULTS.md:5-42`, overall `N-direct / NoGo` at
   `skinny/RESULTS.md:138`, and Track 2 independence at
   `skinny/RESULTS.md:139-140`.
4. The generated-output no-drift claim is intact at the disposition layer. The
   target commit does not modify generated JSON output, generated typed output,
   JSON templates, direct guard source, IR, passes, parse-that-regex, SIMD,
   runtime, skinny bbnf, or xtask owner paths. The research packet records the
   post-fold regen and zero-drift checks at
   `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:150-162`.
5. W5 is not being converted into a performance claim. The V2 plan explicitly
   keeps generated output and `skinny/RESULTS.md` out of scope, requires zero
   diff for those paths, forbids a row-table refresh, and repeats that W5 must
   not claim throughput movement
   (`restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:30-36`,
   `:79-83`, `:143`).
6. Strict-vs-strict comparator integrity is preserved because W5 makes no row
   admission or comparator refresh. The current RESULTS authority keeps native
   Rust comparators as same-run and C++ sidecars as historical or absent, never
   strict anchors in W0 (`skinny/RESULTS.md:141`). W5 does not reinterpret those
   rows.
7. No stale row or authority citation is used by the V2 packet. The stale
   `skinny/RESULTS.md:217-218` anchor appears only inside V1 hardening as the
   defect being folded. The V2 research packet uses current anchors:
   `skinny/RESULTS.md:46-85` for W0 manifest rows and
   `skinny/RESULTS.md:138-141` for report / Track 2 authority.

## Verification/Evidence

- `git rev-parse HEAD` returned the target
  `6e159f5c70aa5b4560d874a0e446587beb8f857e`.
- `git diff --name-status 6e159f5c^ 6e159f5c` shows only W5 research/hardening
  docs plus `skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
  `skinny/crates/codegen/src/lib.rs`, and
  `skinny/crates/codegen/src/json_provider.rs`.
- `git diff --name-only 6e159f5c^ 6e159f5c -- skinny/RESULTS.md
  skinny/crates/runtime/src/grammars/json
  skinny/crates/codegen/src/json_templates
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src
  skinny/crates/passes/src skinny/crates/parse-that-regex/src
  skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src
  skinny/xtask/src` returned no paths.
- The generic codegen residency scan excluding `json_provider.rs` and
  `json_templates/**` returned no matches for `grammar_name == "json"`,
  `backend.grammar_name`, JSON template includes, or runtime JSON includes.
- The provider-residency scan returned only xtask generated-output tooling and
  `json_provider.rs` runtime includes, matching the V2 allowlist story.
- I did not rerun cargo tests or regen checks for this CH3 because the user
  constrained writes to this file only; cargo verification would write build
  artifacts. I relied on read-only diffs/scans plus the target packet's recorded
  post-fold command evidence.

## Required Folds

None for CH3 acceptance.

Carry-forward constraints: do not mark W5 closed from this single CH3 result,
do not dispatch W6, and preserve the narrow wording: W5 proves no reopened
REDRESS 36-38/85/86 route surface, no RESULTS/generated-output drift, no
performance claim, and no stale authority citation in the V2 packet.
