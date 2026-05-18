# CH3 W0 Hardening Review - Lock 14 Grammar-Neutral Freeze

## Decision

REJECT.

## Acceptance Probability

22%.

The W0 commit does not appear to edit runtime, generated, grammar, codegen, IR,
passes, or SIMD source paths directly, but the new Lock 14 enforcement is not a
freeze. It is a path-existence allowlist with major blind spots.

## Blocking Findings

1. `lock14_baseline::validate` does not enforce frozen content or commit/tree
   cleanliness for any read-only path.

   Evidence: `skinny/crates/bbnf-bench/src/bin/gate.rs:27` calls
   `lock14_baseline::validate(&workspace_root())`, but
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:293-325` only checks
   duplicate allowlist entries, class names, mutability labels, two path-name
   substrings, and whether each listed path exists. There is no hash, no
   `git diff`, no comparison to `6d8cb701^`, and no current-tree dirty check
   for read-only surfaces. This fails the W0 exit/pre-block requirement that no
   parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or
   generated parser output change lands (`restart/skinny/tranches/sk-v8/SPEC.md:371-379`).

2. The allowlist omits frozen generic surfaces that can introduce exactly the
   Lock 14 regressions W0 is supposed to guard.

   Evidence: the allowlist body is limited to the entries in
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:12-277`. It does not list
   the runtime public/substrate API in `skinny/crates/runtime/src/lib.rs:1-4`,
   `skinny/crates/runtime/src/tape/mod.rs:7-8`,
   `skinny/crates/runtime/src/tape/mod.rs:87-90`, and
   `skinny/crates/runtime/src/tape/mod.rs:171-225`; the BIR and `BackendShape`
   surfaces in `skinny/crates/ir/src/lib.rs:392-408` and
   `skinny/crates/ir/src/lib.rs:416-435`; the generic codegen/lowering surfaces
   in `skinny/crates/codegen/src/lib.rs:68-83`,
   `skinny/crates/codegen/src/lib.rs:169-177`, and
   `skinny/crates/codegen/src/lower/mod.rs:17-24`; or the scanner/SIMD
   substrate surfaces in `skinny/crates/bbnf-simd/src/lib.rs:13-17` and
   `skinny/crates/bbnf-simd/src/lib.rs:71-78`. Those are the paths where new
   directive/BIR/substrate/API/`BackendShape`/scanner behavior would actually
   enter, and SPEC forbids those surfaces (`restart/skinny/tranches/sk-v8/SPEC.md:232-248`,
   `restart/skinny/tranches/sk-v8/SPEC.md:777-781`).

3. The validator cannot detect newly added forbidden files or renamed forbidden
   surfaces.

   Evidence: `skinny/crates/bbnf-bench/src/lock14_baseline.rs:302-325`
   iterates only the static `ALLOWLIST`; it never scans the filesystem or Git
   index for unlisted additions under frozen roots. The only forbidden-surface
   check is a case-sensitive path substring test for `"UnionTape"` or
   `"directive"` on already-listed paths
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:319-321`). A new
   `crates/runtime/src/union_tape.rs`, a new `BackendShape` variant in
   `ir/src/lib.rs`, a new directive enum/token in grammar/passes, or a new
   public tape API would not be rejected by this gate unless somebody also
   manually added that path to the allowlist. That is the opposite of a freeze.

## Nonblocking Findings

- I found no direct commit-level generated/runtime/codegen/grammar/IR/passes/SIMD
  edit in `6d8cb701`: `git diff --name-status 6d8cb701^ 6d8cb701 --` the
  runtime, codegen, IR, passes, bbnf-simd, grammars, test-fixtures, and
  test_data roots returned no paths.
- The new `S` outcome is a bench-gate enum/status surface, not a BIR or runtime
  substrate surface (`skinny/crates/bbnf-bench/src/gate.rs:4-20`). Its W0 use
  demotes parse rows to substrate-guard non-admission
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:317-321`) and report validation
  rejects parse rows that are not `K` or `S`
  (`skinny/crates/bbnf-bench/src/report.rs:360-365`).
- `skinny/crates/codegen/src/lib.rs:169-177` still contains an existing generic
  codegen JSON-profile guard. This review does not mark it as newly introduced
  by W0, but a real Lock 14 freeze must cover that file because it is a generic
  crate with JSON policy risk.

## Evidence Inspected

- Commit metadata and changed-file set for `6d8cb701`.
- Current tree status before writing this review file.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` allowlist and validator.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` Lock 14 invocation and W0 report
  path.
- `skinny/crates/bbnf-bench/src/gate.rs` outcome/strict-admission additions.
- `skinny/crates/bbnf-bench/src/report.rs` W0 telemetry validation.
- Frozen-surface candidates under `skinny/crates/runtime`, `skinny/crates/codegen`,
  `skinny/crates/ir`, `skinny/crates/passes`, `skinny/crates/bbnf-simd`,
  `skinny/grammars`, `skinny/crates/test-fixtures`, and `skinny/test_data`.
- SK-V8 SPEC Section 1 non-negotiables, Section 3 W0 exit/pre-blocks, and
  Section 10 global pre-blocks.

## Exact Remediation If Rejected

1. Replace the existence-only allowlist with an actual freeze gate. The gate
   must compare frozen paths against the W0 baseline by content hash or by
   `git diff --quiet <baseline> -- <frozen-paths>`, and must reject dirty or
   changed frozen paths before report generation.
2. Split the path model into explicit W0-mutable roots and frozen roots. W0
   mutable roots may include only the bench/report/gate/schema/test/doc owner
   paths. Frozen roots must include at least grammar inputs, JSON fixtures,
   generated JSON runtime output, generated typed output, runtime/tape, runtime
   crate exports, codegen generic/lower/template surfaces, IR/BIR/BackendShape,
   passes, SIMD/asm scanner primitives, and host/API schema facts.
3. Add an unknown-addition scan for frozen roots. Any new file under runtime,
   codegen, IR, passes, bbnf-simd, grammars, test fixtures, or generated output
   must reject unless it is explicitly in the W0 mutable set and named by the
   wave.
4. Add content-aware forbidden-surface checks, not path-substring checks only:
   no new directive tokens/enums, no new BIR variants, exactly the existing five
   `BackendShape` variants, no `UnionTape`/`union_tape`, no new public substrate
   API, no parser-owned cursor/fact slots, and no generic JSON policy drift.
5. Add negative tests that prove the freeze rejects: a changed generated JSON
   file, a changed runtime tape file, a changed codegen lowerer file, a new
   `union_tape.rs`, and an added `BackendShape` variant.
