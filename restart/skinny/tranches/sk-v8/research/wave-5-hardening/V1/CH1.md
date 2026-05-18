# SK-V8 W5 Hardening V1 CH1 - Correctness

Verdict: REVISE.

Confidence: 88%.

## Findings

1. The W5 no-source audit is measurable, but the verification block is not
   exact enough to close CH1 as written. The W5 plan lists commands without
   working directories. A repo-root run of
   `cargo test -p bbnf-bench lock14_baseline -- --nocapture` fails because the
   root workspace does not contain `bbnf-bench`; the same command passes from
   `skinny/`. Conversely, `cargo xtask regen --check` fails from `skinny/` with
   `unknown xtask command 'regen'`, while repo-root `cargo xtask regen --check`
   passes with `regen --check: clean (9 of 9 grammars matched)`.
2. W5 claim traceability is too broad for ORCHESTRATOR CH1. CH1 requires every
   claim to cite file:line, commit SHA, RESULTS row, REDRESS entry, or command
   evidence that resolves. The W5 plan and research need exact cwd-qualified
   command evidence and file anchors for their close claims.
3. One inherited P3-C anchor is stale against current `skinny/RESULTS.md`.
   P3-C cites `skinny/RESULTS.md:217-218`, but current `skinny/RESULTS.md` has
   141 lines. Current Track 2 authority resolves at `skinny/RESULTS.md:138-141`,
   and W0 manifest rows resolve at `skinny/RESULTS.md:46-85`. W5 must not rely
   on the stale `217-218` anchor.
4. The zero-drift check is sufficient only if W5 names both pieces of evidence.
   The explicit W5 diff command passed with exit 0 and no output for
   `skinny/RESULTS.md`, generated JSON output, generated typed output,
   `direct_struct.rs`, and the listed generic surfaces. The Lock 14 test also
   passed 10/10 from `skinny/` and is the broader frozen-root guard.
5. REDRESS reconciliation is correct but needs exact citations in the W5 close.
   REDRESS 36-38 identify the historical Lock 14 violations at
   `skinny/REDRESS.md:460-515`; REDRESS 85 admits W7 Phase A+B neutralization at
   `skinny/REDRESS.md:2399-2427`; REDRESS 86 admits W8 Phase C+D neutralization
   and zero generated/RESULTS drift at `skinny/REDRESS.md:2431-2464`.

## Command Evidence

- Repo-root `git status --short`: exit 0, no output.
- Repo-root `cargo test -p bbnf-bench lock14_baseline -- --nocapture`: exit
  101, package not found.
- `cd skinny && cargo test -p bbnf-bench lock14_baseline -- --nocapture`: exit
  0, 10 passed.
- `cd skinny && cargo xtask check-json`: exit 0.
- `cd skinny && cargo xtask check-real-typed`: exit 0.
- `cd skinny && cargo xtask check-conformance`: exit 0, 21 valid fixtures
  accepted and 7 invalid fixtures rejected.
- `cd skinny && cargo test -p parse-that-regex -p passes -p codegen -p ir`:
  exit 0.
- `cd skinny && cargo xtask regen --check`: exit 1, unknown command.
- Repo-root `cargo xtask regen --check`: exit 0, 9 of 9 grammars matched.
- W5 forbidden-name `rg` command from repo root: exit 1, no matches.
- W5 path-scoped `git diff --exit-code HEAD -- ...`: exit 0, no output.

## Required Folds

- Rewrite the W5 verification block with explicit cwd/manifest context:
  `cd skinny` for `bbnf-bench`, skinny `xtask check-*`, and skinny package
  tests; repo root for `cargo xtask regen --check`, repo-path `git diff`, and
  repo-path `rg`.
- Replace broad W5 claim citations with exact anchors: SPEC W5 gate
  `restart/skinny/tranches/sk-v8/SPEC.md:652-702`, Lock 14 source
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375-538`, REDRESS spans
  above, current RESULTS spans `skinny/RESULTS.md:46-85` and
  `skinny/RESULTS.md:138-141`, plus command-output evidence.
- Do not claim W5 has closed from V1.
