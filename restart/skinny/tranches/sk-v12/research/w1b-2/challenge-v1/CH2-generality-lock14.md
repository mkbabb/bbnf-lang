# SK-V12 W1b-2 CH2 - Generality / Lock 14

Verdict: REVISE.

Blockers:

- Dependency ownership is incomplete. The plan adds
  `lightningcss = { version = "=1.0.0-alpha.71", default-features = false }`
  to `skinny/crates/bbnf-bench/Cargo.toml` and requires a Cargo.lock package
  checksum, but the owner list did not authorize `skinny/Cargo.lock`.
- Persistent fact artifacts are under
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/`, but the owner list
  named only the fixture and W1b-2 report JSON. Either authorize the artifact
  directory or make those files ephemeral.

Non-blocking assessment:

- The direct `bbnf-bench` dependency is the right boundary; adding lightningcss
  at workspace scope would be broader.
- No JSON semantic leak, new directive, BIR variant, BackendShape variant, or
  public substrate API is present in the plan.

Required revision:

- Add `skinny/Cargo.lock` as generated dependency evidence.
- Add the W1b artifact directory for generated comparator evidence, or state
  that artifacts are generated but not committed.
