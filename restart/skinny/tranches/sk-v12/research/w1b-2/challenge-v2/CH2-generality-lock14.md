# SK-V12 W1b-2 CH2 V2 - Generality / Lock 14

Verdict: ACCEPT.

No CH2 blocker remains.

Accepted facts:

- `skinny/Cargo.lock` is now authorized as generated dependency evidence.
- W1b retained artifacts are now owner-listed.
- lightningcss stays scoped to `skinny/crates/bbnf-bench/Cargo.toml`.
- Commands run from `skinny/` and use `bbnf-bench --bin gate` directly.
- No new directive, BIR variant, `BackendShape`, or public substrate API is
  authorized.

Residual guardrail:

- Redress must touch nested `skinny/Cargo.lock`, not the repository root
  `Cargo.lock`.
