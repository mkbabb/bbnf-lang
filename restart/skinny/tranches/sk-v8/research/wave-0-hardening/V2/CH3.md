# CH3 W0 V2 Hardening Challenge - Lock 14 Freeze

## Verdict

REJECT.

Confidence: 86%.

Reviewed current HEAD `cb0fdba0` after `fix(sk-v8-wave0): fold hardening V1 gate blockers`. This review used the CH3 regression lens under ORCHESTRATOR Section 3W and the convergence rule under Section 3Z: each challenge writes one hardening file, hardening folds before advance, and convergence needs two consecutive cycles at >=95% ACCEPT with no critical defects (`restart/prompts/ORCHESTRATOR.md:74`, `restart/prompts/ORCHESTRATOR.md:78`, `restart/prompts/ORCHESTRATOR.md:114`, `restart/prompts/ORCHESTRATOR.md:116`, `restart/prompts/ORCHESTRATOR.md:120`).

V2 fixes most of the V1 CH3 failure. The gate now calls `lock14_baseline::validate` before report generation (`skinny/crates/bbnf-bench/src/bin/gate.rs:35`), the validator checks frozen-root status/diff against Git rather than only path existence (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:394`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:397`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:398`), and the live IR surface is checked for exactly the five existing `BackendShape` variants plus `UnionTape`/`union_tape` absence (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:457`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:460`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:480`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:483`). That is content enforcement for the roots it covers.

## Blocking Findings

1. The frozen-root set still omits the BBNF directive admission surface.

   W0 inherits the non-negotiable "No new BBNF directives" rule (`restart/skinny/tranches/sk-v8/SPEC.md:234`) and the global block on new directives (`restart/skinny/tranches/sk-v8/SPEC.md:777`). The current parser directive gate lives in `skinny/crates/grammar/src/lib.rs`: top-level `@` routes to `parse_directive` (`skinny/crates/grammar/src/lib.rs:62`, `skinny/crates/grammar/src/lib.rs:64`, `skinny/crates/grammar/src/lib.rs:65`), and the allowed directive names are currently only `import` and `token` (`skinny/crates/grammar/src/lib.rs:91`, `skinny/crates/grammar/src/lib.rs:92`). But the frozen roots are `grammars`, fixture roots, runtime/IR/passes/codegen/SIMD source roots, selected bench product-plane files, and `xtask/src/real_typed_schema.rs` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:379`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:383`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:391`). They do not include `crates/grammar/src` or any grammar-parser facade. A W0 fold could admit a new directive in the grammar parser without tripping the Lock 14 freeze gate.

2. The frozen-root set still omits SIMD assembler/build surfaces.

   SPEC W0 closes only if no scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated parser output changes land (`restart/skinny/tranches/sk-v8/SPEC.md:371`, `restart/skinny/tranches/sk-v8/SPEC.md:372`). V1 also required the freeze to include SIMD/asm scanner surfaces (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V1/HARDENING-W0-V1-CONSOLIDATED.md:52`, `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V1/HARDENING-W0-V1-CONSOLIDATED.md:55`). V2 freezes `crates/bbnf-simd/src` only (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:383`). The actual assembler wiring and include roots live outside that root: `build.rs` declares the x86 asm build surface and rebuild triggers (`skinny/crates/bbnf-simd/build.rs:1`, `skinny/crates/bbnf-simd/build.rs:27`, `skinny/crates/bbnf-simd/build.rs:28`, `skinny/crates/bbnf-simd/build.rs:30`), gathers `.asm`/`.S` sources under `src` (`skinny/crates/bbnf-simd/build.rs:43`, `skinny/crates/bbnf-simd/build.rs:44`, `skinny/crates/bbnf-simd/build.rs:79`), and includes `ext/x86` for assembly compilation (`skinny/crates/bbnf-simd/build.rs:52`, `skinny/crates/bbnf-simd/build.rs:57`). Changes to `crates/bbnf-simd/build.rs` or `crates/bbnf-simd/ext/x86/*` can alter asm scanner behavior but are not frozen.

3. The negative test coverage does not prove the live freeze for the omitted classes.

   The new tests cover allowlist class/path checks, dirty-status string rejection, and a local `BackendShape` literal count (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:510`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:546`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:557`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:563`). They do not create a changed `crates/grammar/src/lib.rs`, changed `crates/bbnf-simd/build.rs`, changed `ext/x86` include, or a new asm include/source and prove `lock14_baseline::validate` rejects the live tree. The existing focused test run passes, but it only proves the current covered roots and helper strings, not the missing W0 forbidden surfaces.

## Missed Tests And Evidence

- Ran: `cargo test -p bbnf-bench lock14_baseline::tests`. Result: pass, 6 tests.
- Ran: `git status --porcelain` and `git diff --quiet HEAD^` over the current V2 frozen-root set. Result: no dirty or parent-diff paths.
- Ran: `git diff --quiet HEAD^` over the omitted directive/asm candidate roots `skinny/crates/grammar/src`, `skinny/crates/bbnf/src`, `skinny/crates/bbnf-simd/build.rs`, `skinny/crates/bbnf-simd/ext`, and `skinny/crates/parse-that-regex/src`. Result: no current diff. This lowers immediate behavior-drift risk, but it is not gate evidence because those paths are not consumed by `lock14_baseline::validate`.
- Not rerun: full W0 evidence suite. The commit body records the full W0 suite, and this CH3 pass only needed focused Lock 14/frozen-root challenge evidence.

## Mandatory Fold Items

1. Extend `FROZEN_ROOTS` to cover directive admission and asm wiring: at minimum `crates/grammar/src`, `crates/bbnf/src` if it exposes directive/parser entry behavior, `crates/bbnf-simd/build.rs`, and `crates/bbnf-simd/ext`. If `parse-that-regex` is treated as scanner behavior for Lock 14, include `crates/parse-that-regex/src` or explicitly classify why it is outside W0.
2. Add live negative tests or an injectable temp-repo harness proving `lock14_baseline::validate` rejects a changed grammar directive parser, an added directive name, a changed SIMD build script, a changed asm include, and a newly added asm source/include under the frozen asm surface.
3. Keep the existing V2 improvements intact: Git status/diff enforcement, parent diff, exact five-shape `BackendShape` guard, `UnionTape`/`union_tape` rejection, and early `gate-json` invocation before report write.
4. After the fold, rerun the focused Lock 14 test plus the W0 gate evidence and include an explicit no-diff proof over the expanded frozen-root set.
