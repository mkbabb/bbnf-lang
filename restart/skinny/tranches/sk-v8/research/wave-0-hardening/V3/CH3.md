# CH3 W0 V3 Hardening Challenge - Lock 14 Freeze And Regression Surface

Reviewed commit: `61d5d30407d96ed176cc59e410f7884e30ed30ba`
(`fix(sk-v8-wave0): fold hardening V2 gate blockers`).

## Verdict

ACCEPT.

Confidence: 96%.

Scope: adversarial CH3 review of W0 after the V2 fold, focused on Lock 14
frozen roots and grammar-neutrality, directive/parser admission, BIR/substrate
/ asm / build surfaces, generated/runtime/tape/typed/direct surfaces, and
Git-backed freeze evidence. This ACCEPT is scoped to CH3 only. It does not
close W0 by itself; ORCHESTRATOR convergence still requires two consecutive
cycles at >=95% ACCEPT with no open critical defects or unresolved REVISE
(`restart/prompts/ORCHESTRATOR.md:118`, `restart/prompts/ORCHESTRATOR.md:120`,
`restart/prompts/ORCHESTRATOR.md:123`).

## Findings

1. The V2 CH3 frozen-root blocker is folded.

   `FROZEN_ROOTS` now covers the original parser/runtime/codegen/product-plane
   roots plus the V2 omissions: grammar/directive admission, the bbnf facade,
   SIMD build script, SIMD asm/ext includes, and parse-that-regex
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:383`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:386`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:388`). The gate calls this
   validator before report generation (`skinny/crates/bbnf-bench/src/bin/gate.rs:41`).

2. Directive and parser admission are now covered by the freeze.

   The live directive surface is still the grammar parser's top-level `@` branch
   and `parse_directive` admission list (`skinny/crates/grammar/src/lib.rs:62`,
   `skinny/crates/grammar/src/lib.rs:64`,
   `skinny/crates/grammar/src/lib.rs:80`,
   `skinny/crates/grammar/src/lib.rs:91`). Only `import` and `token` are
   admitted (`skinny/crates/grammar/src/lib.rs:92`). The freeze now includes
   `crates/grammar/src`, `crates/bbnf/src`, and `crates/parse-that-regex/src`,
   so a directive/parser admission edit cannot land in W0 without tripping
   Lock 14 (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:383`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:384`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:388`).

3. BIR, substrate, asm, and build surfaces are frozen and separately checked.

   SK-V8 forbids new directives, BIR variants, `BackendShape` variants,
   `UnionTape`, new substrate surfaces, parser-owned cursor/facts, sidecar
   substrates, and stale sidecar strict admission
   (`restart/skinny/tranches/sk-v8/SPEC.md:234`,
   `restart/skinny/tranches/sk-v8/SPEC.md:236`,
   `restart/skinny/tranches/sk-v8/SPEC.md:237`,
   `restart/skinny/tranches/sk-v8/SPEC.md:241`,
   `restart/skinny/tranches/sk-v8/SPEC.md:242`,
   `restart/skinny/tranches/sk-v8/SPEC.md:245`). The implementation freezes
   runtime, IR, passes, codegen, SIMD source, SIMD build script, SIMD ext, and
   parse-that-regex (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:379`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:382`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:385`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:387`). It also checks the
   live `BackendShape` surface for exactly the five existing variants and rejects
   `UnionTape` / `union_tape` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:465`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:485`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:488`).

   The asm/build root is not theoretical: `build.rs` declares ext/x86 rebuild
   triggers and gathers `.asm` / `.S` sources under `src`
   (`skinny/crates/bbnf-simd/build.rs:27`,
   `skinny/crates/bbnf-simd/build.rs:30`,
   `skinny/crates/bbnf-simd/build.rs:43`,
   `skinny/crates/bbnf-simd/build.rs:44`,
   `skinny/crates/bbnf-simd/build.rs:52`,
   `skinny/crates/bbnf-simd/build.rs:97`). Those roots are now in
   `FROZEN_ROOTS`.

4. Generated/runtime/tape/typed/direct surfaces did not move.

   W0 closes only if no parser, scanner, SIMD, asm, codegen behavior,
   product-plane behavior, or generated parser output changes land
   (`restart/skinny/tranches/sk-v8/SPEC.md:371`,
   `restart/skinny/tranches/sk-v8/SPEC.md:372`). The expanded freeze includes
   `crates/runtime/src`, `crates/codegen/src`, direct/typed/generated bench
   product files, `track2`, parity, scan, and materialization
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:379`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:382`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:389`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:396`). The reviewed commit's
   actual file set is limited to `skinny/crates/bbnf-bench/src/bin/gate.rs`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, and
   `skinny/crates/bbnf-bench/src/report.rs`, so it stays inside the W0
   telemetry/gate owner path.

5. Git-freeze evidence is live, not prose-only.

   `validate_git_freeze` checks porcelain status, worktree diff, and parent diff
   across `FROZEN_ROOTS` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:402`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:403`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:404`). Tests cover dirty
   frozen-root strings, directive/asm coverage, and `BackendShape` drift
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:562`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:571`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:589`).

6. Grammar-neutrality holds for this W0 slice.

   SPEC Section 2.1 bans public JSON-named APIs, JSON grammar branches in
   generic code, generic JSON structural policy, and template/provider boundary
   leaks (`restart/skinny/tranches/sk-v8/SPEC.md:302`,
   `restart/skinny/tranches/sk-v8/SPEC.md:307`,
   `restart/skinny/tranches/sk-v8/SPEC.md:308`,
   `restart/skinny/tranches/sk-v8/SPEC.md:313`,
   `restart/skinny/tranches/sk-v8/SPEC.md:317`). The reviewed commit edits only
   bench gate/report/freeze code, while generic/parser/runtime/codegen/SIMD roots
   are covered by the parent-diff freeze. I found no W0 generic-crate JSON policy
   leak.

## Evidence Run

- `git diff --name-only 61d5d304^ 61d5d304 -- <expanded frozen roots>`:
  no output.
- `git diff --exit-code -- <expanded frozen roots>`: PASS.
- `git diff --exit-code 61d5d304^ -- <expanded frozen roots>`: PASS.
- `cargo test -p bbnf-bench`: PASS, 49 library tests plus 6 gate-bin tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS against the committed W0 evidence root.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --update-results --include-volatile-probes`: expected REJECT before write.
- `cargo xtask check-json && cargo xtask check-real-typed && cargo xtask check-conformance`: PASS; conformance accepted 21 valid fixtures and rejected 7 invalid fixtures.

I also ran `cargo xtask gate-json --advisory --check-results` against my default
local `target/criterion`; it failed on `json/twitter/parse_only/main` moving
63.03% from `SK-V8-open`. That is not a CH3 blocker: it demonstrates the
baseline-delta gate rejects a stale or different local Criterion capture instead
of silently promoting it.

## Mandatory Fold Items

None for CH3.
