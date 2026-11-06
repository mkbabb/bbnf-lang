# BB-C.W0 ambient optimizer authority — R1 author record

`MODEL: gpt-5.6-sol; REASONING: ultra; WORKFLOW: v2`

- Recorded: `2026-07-22T16:57:56Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Scope: BB-C.W0 item 3 only. This is not a G3 replay.
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit pending
  executable GREEN evidence, two hostile reviews, and fresh adjudication**.

## Bound ownership

The owned production surface was the one live whole-program grammar e-graph edge
and comments that described that edge as authoritative:

- `crates/core/src/pipeline/compile/pipeline.rs`
- `crates/ir/src/egraph/mod.rs`
- `crates/ir/src/passes/transform/mod.rs`
- `crates/ir/src/passes/materialization/classify.rs`
- new external witness `crates/core/tests/direct_pipeline_semantics.rs`

No Cargo manifest/configuration, lockfile, generated/runtime file, G3-owned file,
Parse-that, Value, playground, or frontend byte was owned or edited.

## Reconciled pre-state

| File | SHA-256 before R1 |
|---|---|
| `crates/core/src/pipeline/compile/pipeline.rs` | `231087210679f23a1c5c677915bc7e9fcbd14b4fc8c8f9e1b2b842e512edc520` |
| `crates/ir/src/egraph/mod.rs` | `8ef520c1f0c808eaef8d2ac29d3722d3f0695eb21598188b64d2fd98e218326b` |
| `crates/ir/src/passes/transform/mod.rs` | `a2cccdbef3678d02d991e8c3c1c15757194953088f945084e0ca02dbdf8bb47d` |
| `crates/ir/src/passes/materialization/classify.rs` | `8bd062dc0703c706a057cd8b8efb2cd450dbff3a7cbd0c883e790a1865794b63` |
| `crates/core/tests/direct_pipeline_semantics.rs` | absent |
| `crates/core/Cargo.toml` (protected concurrent dirt) | `ca7720900e9a573f3ce7057c126ddafd6fe595396791f0e067060f2411a12a85` |
| `Cargo.lock` (protected concurrent dirt) | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` |

## Implementation

The production compiler formerly executed this unconditional edge after direct
normalization:

`build_and_saturate → GrammarCostModel extraction → write_back_optimized → pool.write_back`

R1 deletes that edge, the write-back-only debug assertion, and stale claims that
the e-graph is a permanent production optimizer. It adds no option, feature flag,
fallback, alias, shim, or dual route. Direct normalized `GrammarIR` is now the
incumbent passed to subsequent deterministic analysis.

The e-graph implementation remains intact as an explicitly non-authoritative
research/differential-test substrate. No bounded island is admitted here.

Existing tracked files changed by `+20/-76` lines (net `-56`); the external
94-line executable witness is the only new code file.

## Causal executable witness

`direct_pipeline_semantics.rs` uses the smallest current differentiator found:

```bbnf
branch = ("x" -> 0u8), "z" | ("y" -> 1u8), "z" ;
start = "!", branch ;
```

The direct production result must remain `Alt(Seq(_, z), Seq(_, z))` with both
typed `Map` nodes. The same test then invokes the removed edge explicitly on a
clone as a test-only differential; common-suffix extraction changes the clone to
`Seq(Alt(_, _), z)`. This makes the authority assertion causal rather than a
source-text-only check.

The direct arm also specifies exact VM behavior:

- `!xz`: accepted, offset `3`, no diagnostics, exact nested tagged values and
  spans `(0,3)`, `(0,1)`, `(1,3)`, `(1,2)`, `(2,3)`;
- `!x`: rejected, restored offset `0`, no value, one `branch` diagnostic at
  offset `2`, exact expected byte `[z]`.

## Production authority census

Command:

```text
rg -n 'bbnf_ir::egraph::(build_and_saturate|write_back_optimized|GrammarCostModel)|egraph_build_saturate_writeback|pool\.write_back' crates/core/src
```

Result: no rows, `rg` exit `1`. There is therefore no remaining production-core
caller of the grammar saturation, extraction, write-back, pool-writeback, or old
timer route.

A wider definition census finds only the preserved research substrate:

```text
crates/ir/src/egraph/mod.rs:34:pub use write_back::{extract_ir_node, write_back_optimized};
crates/ir/src/egraph/mod.rs:45:/// `write_back_optimized`.
crates/ir/src/egraph/mod.rs:46:pub fn build_and_saturate(
crates/ir/src/egraph/write_back.rs:32:/// `build_and_saturate` step; the cost model guides best-node
crates/ir/src/egraph/write_back.rs:34:pub fn write_back_optimized(
crates/ir/src/egraph/write_back.rs:123:/// [`write_back_optimized`] for full IR reconstruction.
crates/ir/src/egraph/write_back.rs:155:    // own top-level `write_back_optimized` iteration. This preserves
```

The remaining production files mentioning e-graph types are an exact, separate
inventory: backend inline cost-weight projections; cost configuration; direct
fact/materialization lattices; regex-HIR saturation cache; rewrite metadata; and
`GrammarIR` fact fields. None calls grammar `build_and_saturate` or write-back.

```text
crates/core/src/backend/rust/analysis/inline/budgets.rs
crates/core/src/backend/rust/analysis/inline/constraints.rs
crates/core/src/backend/rust/analysis/inline/mod.rs
crates/core/src/backend/rust/analysis/inline/visit.rs
crates/ir/src/cost_config.rs
crates/ir/src/passes/facts/mod.rs
crates/ir/src/passes/materialization/classify.rs
crates/ir/src/passes/materialization/mod.rs
crates/ir/src/passes/mod.rs
crates/ir/src/passes/profile.rs
crates/ir/src/passes/regex_info.rs
crates/ir/src/rewrites/base.rs
crates/ir/src/rewrites/mod.rs
crates/ir/src/types/grammar.rs
```

## Post-state and preservation

| File | SHA-256 after R1 |
|---|---|
| `crates/core/src/pipeline/compile/pipeline.rs` | `8ee0c2ff7d61b2d9be7791a7aa267b3861f303778f84febcb133be132c2ac020` |
| `crates/ir/src/egraph/mod.rs` | `04b675f97b050661a3c6551d4caf87da29fec8b3472e41f02c9ee842933e80ab` |
| `crates/ir/src/passes/transform/mod.rs` | `320772ce08b618596513a531fe052982ff8f27f6955d804a25f03982522e5343` |
| `crates/ir/src/passes/materialization/classify.rs` | `7a4d8fb861582e3744cf93da6880b05be648ddfbd930c56e7950ef7752fd51a2` |
| `crates/core/tests/direct_pipeline_semantics.rs` | `d438007ff2e54ef071c49b1b6e46477a78dafe38e73e16c5ee06ed00c05eef83` |
| `crates/core/Cargo.toml` | `ca7720900e9a573f3ce7057c126ddafd6fe595396791f0e067060f2411a12a85` (byte-identical) |
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` (byte-identical) |

G3-owned preservation at the terminal checkpoint:

| File | SHA-256 |
|---|---|
| `crates/ir/src/egraph/rules/universal.rs` | `9835616cc8900eb5e0e5fde769e34d149bfcdf8e2493f0aef74b958426768b5b` |
| `crates/ir/src/egraph/rules/mod.rs` | `da0a7b1b9120e6d3110582254824fc6cb6509b181aadd13332c39ab80cc897c4` |
| `crates/ir/tests/egraph/egraph_universal.rs` | `e4e9beab261c1a10a52d537f2d773e8f69c051e763918f3d35814d1252576ede` |
| `crates/core/tests/egraph_semantic.rs` | `ef08223fdce747c821a0d11db54057314aaefb8a57bc41c8f14a9ba12b5aeaf2` |

`git diff --check` was clean for the owned slice.

## Sole post-release native replay: RED before test execution

Command (warm target only; no clean/full workspace build):

```text
/usr/bin/arch -arm64 cargo test --target aarch64-apple-darwin -p bbnf --test direct_pipeline_semantics -- --nocapture
```

Machine facts: `/usr/bin/arch = arm64`, `uname -m = arm64`,
`sysctl.proc_translated = 0`. The linker invocation also contained only
`-arch arm64`.

Result: exit `101`. `bbnf-ir` and `bbnf` compiled, but the dependency
`bbnf-path` dylib link was killed by signal 9 before the test binary executed:

```text
error: linking with `cc` failed: signal: 9 (SIGKILL)
error: could not compile `bbnf-path` (lib) due to 1 previous error
```

Per the bounded replay order, there was no retry. This is safe RED execution
evidence, not an ownership blocker and not a semantic failure of the witness.
The slice remains implemented but unverified; hostile review must not promote it
without a later separately authorized executable GREEN run.
