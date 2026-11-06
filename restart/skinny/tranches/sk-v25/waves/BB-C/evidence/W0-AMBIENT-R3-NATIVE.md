MODEL: gpt-5.6-sol; REASONING: ultra; WORKFLOW: v2

# BB-C.W0 ambient optimizer authority — R3 native ARM receipt

- Executed: `2026-07-22`
- Host: Apple `arm64`; `uname -m = arm64`
- Rust: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`
- Cargo: `cargo 1.96.0-nightly (eb94155a9 2026-04-09)`
- Target: `aarch64-apple-darwin`
- Build jobs: `1`
- Retry count: `0`
- x86 route: **none**
- Terminal status: **R3 NATIVE RED**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**

The three commands below were the sole frozen R3 native campaign. Each ran
once. `OUTPUT_SHA256` is the SHA-256 of the complete zsh command-substitution
transcript after its trailing newlines were stripped; the transcript printed
immediately afterward with one terminal newline. No source repair or rerun was
performed after the failure.

## Gate 1 — optimizer unit witnesses

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests -- --nocapture
```

- Exit: `0`
- Output SHA-256: `190d46b383907aadb506a11c3af5cf06485afbea6a8f5ee1989d2e0b921de629`
- Result: `9 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out`

This executed the immutable prune/restore, exhaustive constrained optimum,
hard-constraint inversion, semantic-site join, duplicate-write rejection,
pre/post-incumbent cancellation, budget/unsat, malformed assignment, and
factorized objective witnesses.

## Gate 2 — public cross-rule witnesses

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf-ir --test vm cross_rule_csp -- --nocapture
```

- Exit: `0`
- Output SHA-256: `d0a266ecf5afb2fdcb12c6b770ada0971d695489c489764fafbd00c2d9812d01`
- Result: `10 passed; 0 failed; 0 ignored; 0 measured; 62 filtered out`
- Warning: unused imports `PrettyHints` and `RuleDirectives` in
  `crates/ir/tests/vm/cross_rule_csp.rs:19`

The warning is cleanup debt in the frozen R3 subject. It was not edited after
the run.

## Gate 3 — canonical core pipeline witness

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf --lib pipeline::compile::pipeline::tests -- --nocapture
```

- Exit: `101`
- Output SHA-256: `b208f4519b0e75bdd338ca8cf617090913aad7b048534aebe08e147aa8da5feb`
- Result: `1 passed; 1 failed; 0 ignored; 0 measured; 17 filtered out`
- Structured-API witness: **passed**
- Direct typed-arm/VM witness: **failed before optimizer execution**

Exact panic:

```text
thread 'pipeline::compile::pipeline::tests::direct_pipeline_preserves_both_typed_arms_and_current_vm_contract' panicked at crates/core/src/lower/expression/wrap.rs:242:13:
mapped_factor: source span "(\"x\" -> 0u8)" contains a `->`/`=>` arrow but structural detection found no value_expr head among the compound's 1 children — typed-materialization invariant violated
```

The executable reached native test execution; the prior `bbnf-path` linker
SIGKILL did not recur. The panic occurs while lowering the witness grammar,
before the CSP route under review. This proves that the author record's static
direct-witness shape does not establish an executable pipeline oracle. Whether
the witness grammar is invalid for the canonical mapping syntax or the lowerer
rejects a valid parenthesized mapping is not adjudicated here.

The core build also emitted 183 warnings, predominantly pre-existing generated
`__iter_count < 0u32` comparisons plus a small number of unused/dead items.
Those warnings do not change the exit cause and receive no cleanup by this
receipt.

## Post-run preservation

Every one of the fourteen R3 subject hashes remained byte-identical to
`W0-AMBIENT-R3-AUTHOR.md`. `Cargo.lock` remained
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
Cargo wrote only ordinary target artifacts. No package, lock, source, test,
generated runtime, parse-that, Value, Glass/SCI/Atlas, or playground byte was
mutated by the evidence campaign.

## Disposition

R3 has strong executable evidence for the repaired optimizer and public
cross-rule transaction, but its required core pipeline witness is RED. Two
fresh hostile critics and a fresh adjudicator must classify the panic, the
warning debt, the report design, the source implementation, and the missing
core proof before any repair. There is no permission to rerun, advance
BB-C.W1, or claim BB-C.W0 item 3.

**NATIVE RED. CREDIT 0. HOSTILE CLEAN 0/2. BB-C.W1 MAY NOT BEGIN.**
