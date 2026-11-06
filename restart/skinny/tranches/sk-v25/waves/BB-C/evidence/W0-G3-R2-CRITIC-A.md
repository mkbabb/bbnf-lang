# BB-C.W0 G3 deletion R2 — hostile critic A

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Frozen subject hashes matched:

- `crates/ir/src/egraph/rules/universal.rs`: `9835616cc8900eb5e0e5fde769e34d149bfcdf8e2493f0aef74b958426768b5b`
- `crates/ir/src/egraph/rules/mod.rs`: `da0a7b1b9120e6d3110582254824fc6cb6509b181aadd13332c39ab80cc897c4`
- `crates/ir/tests/egraph/egraph_universal.rs`: `e4e9beab261c1a10a52d537f2d773e8f69c051e763918f3d35814d1252576ede`
- `crates/core/tests/egraph_semantic.rs`: `bce230664724c15129ce582b7108b4e11c30cfc36a89f07488f7cc1b8046b56f`
- author record: `7fb3bec9c4d57e62bf7560369540a1b6cbb63d04a0375a26770c4abd2251a5ff`

Verdict: **REJECT R2 evidence; ACCEPT the narrow production deletion.**

## Findings

1. **The diagnostic counterexample is unrelated to G3.** The rejection case
   uses a grammar with no epsilon alternation, so the removed rule cannot affect
   it. The pre-deletion crater aborts on the earlier empty-input assertion before
   reaching the diagnostic block. Use a nullable production in a required
   surrounding context and an independent historical diagnostic RED.
2. **The crater does not seal its dependency closure.** Its archived
   `.cargo/config.toml` resolves `parse_that`, `bbnf-regex`, `pprint`, and
   `pprint_derive` through live sibling paths. `Cargo.lock` does not content-pin
   those path dependencies, and no sibling byte ledger was recorded.
3. **Twin-replay equality is uncheckable.** The normalized digest exists only in
   prose. No raw run-A/run-B artifact, retained normalized payload, or executable
   normalization contract permits independent recomputation.

## Verified facts

- Current Apple ARM replay passed 9/9 IR tests and 1/1 semantic test; the lock
  remained `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
- The historical overlay reproduced the allowlist, graph-union, and empty-input
  failures with exit 101; its lock stayed `a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`.
- G3 symbols, type machinery, export, registration, and named route are absent.
- The default allowlist and graph-level nullable-alt witness are valid.
- The production change is parsimonious and correctly scoped to BB-C.W0 item 1;
  it earns no larger wave or movement credit.

Hostile clean count remains `0/2`. No files were edited.
