# Type-kernel v3 evidence manifest

Evidence date: **2026-07-18**. Scope: the isolated crate in this directory.
Production adoption is not claimed.

## Environment

```text
host architecture: arm64
OS: macOS 26.4.1 (25E253)
rustc: 1.96.0-nightly (02c7f9bec 2026-04-10)
Rust host: aarch64-apple-darwin
LLVM: 22.1.2
cargo: 1.96.0-nightly (eb94155a9 2026-04-09)
Node: v26.0.0
TypeScript: 5.9.3
```

`bbnf-ir` is the real local package at `crates/ir`, version `0.1.6`; the
prototype does not copy its enums. `Cargo.toml` uses a local path dependency.
The crate is an isolated workspace and intentionally retains no checked-in
`Cargo.lock`.

## Exact gates and results

| Gate | Result | Evidence |
| --- | --- | --- |
| 1. Store/binder | **GREEN in carrier** | Nested outer use, nested vacuity, prebuilt composition, panic recovery, and alpha-stable recursion are direct external tests. |
| 2. Rows/zonk | **GREEN in carrier** | Field-recursive row occurs, type occurs, lacks conflict, flexible open/open joins, sibling zonk, repeated-zonk identity, closed/open behavior, algebraic identity/commutativity/associativity/idempotence, canonical field ordering, and 1,024 distinct row-heavy constraint permutations pass. Public free-row IDs are reallocated canonically after a hostile algebra test caught internal residual leakage. |
| 3. Schemes | **GREEN in carrier** | Lacks survives generalize/instantiate; instantiations are fresh; rigid environment variables are shared and usable; level cutoff, syntactic-value restriction, expansive restriction, reintroduced-label rejection, and nested-recursion round trip pass. |
| 4. Budget/proof | **GREEN in carrier** | Zero-budget ingestion is typed; a 20,000-deep program on a 512-KiB thread returns `Exhausted { limit: 256 }`; contradiction, type-occurs, row-occurs, and lacks errors have non-empty attributable slices; an unrelated tautology is absent. |
| 5. Real IR/host boundary | **GREEN for kill-carrier scope** | All 14 live `IrNode` variants are constructed and solved; every `FnDescriptor` arm and all 18 live `TypeDesc` shapes are bridged through the real dependency. Literal/regex/epsilon, homogeneous alt, negate, reference, token dispatch, and map-return fixtures assert adjudicated outcomes. Exhaustive matches have no wildcard, so adding an enum variant fails compilation. |
| 6. Apple arm64 physical layout | **GREEN for explicit carriers on measured host** | Native `size_of`, `align_of`, and `offset_of` agree with modeled `repr(C)` Span/header/padded carriers. Explicit tagged option is 16/8; null-niche pointer option is 8/8. Rust library containers remain rejected as physical lowering inputs. |
| 7. Rust/TypeScript projection | **GREEN in carrier** | Hostile keywords, punctuation, Unicode policy, empty and leading-digit names, normalized collisions, quoted TS labels, nested values, and singleton tuples compile as real values under strict real toolchains. Two clean generations are byte-identical and carry source/semantic digests, generator version, toolchain identity, and reproduction command. |
| 8. Atomic production cut | **RED / deliberately untouched** | `project_types`, ignored CSP errors, masking fallbacks, `GrammarIR` sidecars, and production consumers were outside this carrier's ownership. No shim or feature flag was added. |
| 9. Production consumers/performance/audits | **RED** | BBNF/JSON/CSS/CSV/math/Sheets corpus consumers, shared sealed Rust/TS/VM handoff, runtime execution, cold compile/allocation/byte benchmarks, production transposition, and two independent clean audits have not occurred. |

## Test inventory

- 32 external integration tests; zero source-unit tests; zero ignored tests.
- 1,024 distinct lexicographic permutations of a seven-constraint row-heavy
  system, plus six ordinary equality permutations and focused forward/reverse
  row-order witnesses.
- One 20,000-constructor typed-exhaustion witness on an explicitly 512-KiB
  thread stack.
- 14/14 live `IrNode` variant roots, 6/6 live `FnDescriptor` arms, and 18/18
  `TypeDesc` shapes constructed through `bbnf-ir`.
- Generated Rust and TypeScript values include normalized collisions and a
  singleton tuple. The external test itself invokes both compilers.

## Native layout observations

On the recorded `aarch64-apple-darwin` host:

```text
String                     size 24, align 8  (measured Rust implementation)
Vec<u8>                    size 24, align 8  (measured Rust implementation)
Option<String>             size 24, align 8  (measured Rust implementation)
Option<NonNull<u8>>        size  8, align 8  (measured null niche)
kernel repr(C) Span        size  8, align 4  (modeled, native-probe matched)
kernel three-word header   size 24, align 8  (modeled, native-probe matched)
kernel tagged Option<u64>  size 16, align 8  (modeled explicit tag)
```

The first four are labeled `Measured` and never authorize `String`/`Vec`/Rust
`Option` lowering. Kernel plans are labeled `Modeled`; external native probes
compare the explicit representations on a permitted host.

## Commands

Executed from `/Users/mkbabb/Programming/bbnf-lang`:

```sh
cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --check
cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
```

Expected final result: formatting exit 0; clippy exit 0 with warnings denied;
both test runs report `32 passed; 0 failed; 0 ignored`. Cargo prints only the
repository workspace's pre-existing unused-patch notices before these results.

The generated-value test executes the effective compiler commands below in a
fresh process-specific temporary directory and deletes that directory after
success:

```sh
rustc --edition=2024 -D warnings --crate-type=lib hostile.rs -o libhostile.rlib
npx --yes tsc --strict --noEmit --lib es2022,dom hostile.ts
```

## Limitations and all remaining RED work

1. No production file was edited and no production state carrier consumes this
   solution. Gate 8 is wholly RED.
2. No manifest grammar corpus or runtime backend consumes the carrier. The real
   enum bridge proves vocabulary and local outcomes, not production adoption.
3. No VM/WASM/Rust/TS shared `LoweringPlan`, runtime payload execution, or
   production disconnect witness exists. These are gate-9 work.
4. No performance/allocation/bytes baseline or representation freeze exists.
5. No independent hostile critic, adjudicator, or second clean audit has reviewed
   this v3 artifact.
6. Rust library container measurements are toolchain observations only. Layout
   authority is intentionally limited to kernel-owned explicit carriers.
7. A deep successful reification benchmark is not claimed. The required
   20,000-depth gate proves typed exhaustion before recursive materialization.
8. The generated source digest covers the generated definition body; the
   provenance comment containing that digest is excluded to avoid a recursive
   self-hash.

