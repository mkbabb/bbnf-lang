# SK-V12 W0-A6: Command Feasibility

Date: 2026-05-20.
Scope: SK-V12 W0 read-only audit of executable validation commands and
authority roots.
Output: this file.

## Section 1 - Findings

W0 validation can run without behavior source changes. The JSON gate and
conformance surface already exists through `xtask` and `bbnf-bench`.

The default `skinny/target/criterion` cache is not valid W0 authority:
`gate-json --advisory --check-results` rejects it as unsupported W0 capture
metadata. The retained SK-V12-open authority is
`/tmp/skv11-open-criterion-3ce75df`. With that root,
`gate-json --advisory --check-results` passed during audit.

`gate-json --with-cost-facts --check-results`, `check-json`,
`check-real-typed`, and `check-conformance` passed during audit.

The non-JSON gate exists only through `bbnf-bench --bin gate` today. `xtask
gate-json` does not pass through the companion non-JSON report flag.

## Section 2 - Recommendations

Run W0 validation serialized with the retained Criterion authority:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv12-w0-target RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json
CARGO_TARGET_DIR=/tmp/skv12-w0-target RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-real-typed
CARGO_TARGET_DIR=/tmp/skv12-w0-target RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-conformance
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

After W0 lands the SK-V12 companion lane, add unit tests for the SK-V12
companion schema and validate a passing companion report through `xtask
gate-json --skv12-non-json-report`.

## Section 3 - Risks

Cargo commands should be serialized to avoid package and build locks. The JSON
gate must use `--advisory` for opening validation because the overall result
remains `N-direct / NO-GO`; that is the expected carry-in state, not a W0
failure.

## Section 4 - Sources

- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/Cargo.toml`
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 3
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
