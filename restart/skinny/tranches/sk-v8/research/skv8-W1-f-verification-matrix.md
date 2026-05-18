# SK-V8 W1 Research F: Verification Matrix And No-Behavior-Drift Proof

Date: 2026-05-18.
Scope: W1 verification matrix for CostFacts gate binding, generated-output and
parser no-drift proof, full-table maintain, Lock 14/non-JSON proof, and
commit-sliced rollback.
Output: this file.

## Findings

W1 is dispatchable only because W0 closed: V12 says "W0 is closed. W1 may
dispatch under the SK-V8 SPEC Section 4 entry gate"
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:19`),
and HANDOFF repeats that W1 entry requires W0 admission, every current main row
to carry `SK-V8-open` telemetry, no generated/parser behavior drift, and
`gate-json --with-cost-facts` to become the same-wave CostFacts consumer
(`restart/skinny/tranches/sk-v8/HANDOFF.md:174-181`).

The current code is still pre-W1 on CostFacts. `gate-json --with-cost-facts`
only accepts `--advisory`, reads `grammars/json.bbnf`, calls
`codegen::cost_facts_from_source("json", ...)`, and prints a JSON snapshot
(`skinny/xtask/src/main.rs:274-304`). W0 validation also requires the sentinel
`none:pre-W1` CostFacts tuple and rejects anything else
(`skinny/crates/bbnf-bench/src/report.rs:1007-1018`). V12 routes that residual
to W1: W1 owns replacing `none:pre-W1` with real gate-consumed CostFacts before
any behavior wave can cite route quality
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:65-72`).

Therefore W1 cannot close by showing that CostFacts are printed. It must prove
they are consumed by the gate and fail closed when any required evidence is
missing. SPEC Section 4 requires rule ids, chosen shape, rejected alternatives,
evidence source, wave id, and REDRESS reference in the gate report, strict
comparator fields bound into refusal, `gate-json --with-cost-facts` rejecting
missing evidence, grammar-neutral CostFacts/report fields, unchanged generated
JSON output and parser behavior, and full-table maintain within +/-1.0 percent
of `SK-V8-open` (`restart/skinny/tranches/sk-v8/SPEC.md:396-416`).

## Verification Matrix

Run commands from the repository root unless the command explicitly starts with
`cd skinny`. Use the admitted W0 Criterion target dir as `CARGO_TARGET_DIR`.
The examples below use `/tmp/skv8-w1-target`; if W1 is reusing the V12 W0 target,
replace that value with the captured W0 target and record the run id.

| Gate | Command | Expected pass | Expected fail |
|---|---|---|---|
| Preflight scope | `git status --short` and `git diff --name-only -- skinny restart/skinny/tranches/sk-v8/research` | Dirty state is understood before redress; unrelated user files are not staged or rewritten. | Unknown source/generated paths in the W1 slice block close until routed. |
| Focused W1 CostFacts negative tests | `(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w1-target cargo test -p bbnf-bench w1_costfacts -- --nocapture)` | W1 redress adds tests with this prefix. They must accept complete materialized JSON rule evidence and reject missing `costfacts_rule_id`, `costfacts_chosen_shape`, empty/missing rejected alternatives, missing evidence source, missing REDRESS reference, missing wave id, and leftover `none:pre-W1` on post-W1 materialized JSON rules. | Any missing-field mutation passes, or evidence is emitted but not consumed by the validator. This is `producer_only_telemetry` / `w1_costfacts_missing` per P3-D (`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:148-163`). |
| CLI CostFacts consumer smoke | `(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w1-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --with-cost-facts --advisory > /tmp/skv8-w1-costfacts.json)` | Exit 0. Output/report uses a W1 schema, has no materialized JSON rule with `none:pre-W1`, includes chosen shape plus rejected alternatives, and is produced by the same validation path covered by `w1_costfacts` tests. | The command only prints a snapshot, tolerates missing evidence, or leaves materialized rules on `none:pre-W1`. Current pre-W1 behavior is output-only (`skinny/xtask/src/main.rs:284-304`) and is not sufficient. |
| Strict comparator refusal stays fail-closed | `(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w1-target cargo test -p bbnf-bench strict -- --nocapture)` | Existing strict tests still reject unsupported outcomes, non-GO outcomes, deferred/view-boundary validation, plane mismatch, stale sidecars, and sidecar-same-run without a structured manifest (`skinny/crates/bbnf-bench/src/gate.rs:451-518`). | Any strict-admission refusal weakens while wiring CostFacts/comparator fields. |
| Full bbnf-bench unit surface | `(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w1-target RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench -- --nocapture)` | Existing parity, Track 2, direct, real-typed, metadata, report, Lock 14, and W1 tests pass. Current parser/Track 2/direct/typed tests cover generated parse parity and product outputs (`skinny/crates/bbnf-bench/src/parity.rs:23-80`, `skinny/crates/bbnf-bench/src/track2/json.rs:341-363`, `skinny/crates/bbnf-bench/src/direct_struct.rs:748-768`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:438-464`). | Any parser/product behavior test fails, or new W1 negative tests are absent. |
| Generated JSON output freshness | `(cd skinny && cargo xtask check-json)` | Exit 0. The command regenerates JSON runtime output in memory and checks checked-in `crates/runtime/src/grammars/json` without writing (`skinny/xtask/src/main.rs:120-132`). | Stale generated JSON runtime output. W1 rejects unless split into a challenged behavior/generated-output wave. |
| Generated typed output freshness | `(cd skinny && cargo xtask check-real-typed)` | Exit 0. Generated real-typed DirectBuild output matches checked-in `crates/bbnf-bench/src` (`skinny/xtask/src/main.rs:135-151`). | Stale generated typed output or W1-induced drift. |
| Parser behavior conformance | `(cd skinny && cargo xtask check-conformance)` | Exit 0. Valid fixtures parse, invalid fixtures reject, and float bits match serde for float forms (`skinny/xtask/src/main.rs:34-82`). | Parser behavior drift. W1 has no authority to reinterpret this as CostFacts work. |
| Generated/parser diff freeze | `git diff --exit-code -- skinny/grammars/json.bbnf skinny/crates/runtime/src/grammars/json skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/codegen/src/json_templates` | No diff. These are the generated JSON output, generated typed output, grammar input, and per-grammar template/provider surfaces named by the Lock 14 allowlist (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:129-217`). | Any diff is generated-output drift and a W1 blocker unless a separate challenged behavior consumer was accepted. |
| Product/helper diff freeze | `git diff --exit-code -- skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/track2 skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/materialization.rs` | No product-plane or Track 2 helper drift inside the otherwise allowed `bbnf-bench` owner tree. | Any diff means W1 changed behavior/helper surfaces and must split or reject. |
| Full-table maintain gate refresh | `(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w1-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results)` | Exit 0 against the captured Criterion root. Report validation sees exactly the 38 `SK-V8-open` rows, unchanged outcomes/verdicts, and Track 1/Track 2 movement within +/-1.0 percent (`skinny/crates/bbnf-bench/src/report.rs:494-526`, `skinny/crates/bbnf-bench/src/report.rs:937-952`). | Missing row, duplicate row, changed outcome/verdict, missing Mbps, or movement beyond +/-1.0 percent. SPEC allows only one W1 gate refresh (`restart/skinny/tranches/sk-v8/SPEC.md:247-259`). |
| Lock 14 baseline gate | The same `cargo xtask gate-json --advisory --check-results` command | `gate-json` calls `lock14_baseline::validate` before report validation (`skinny/crates/bbnf-bench/src/bin/gate.rs:37-44`). Frozen roots are clean against status, worktree diff, and parent diff (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399-405`). | Dirty frozen roots, forbidden `UnionTape`, `BackendShape` drift, duplicate/unknown allowlist class, or forbidden surface name (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:473-506`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:514-593`). |
| Lock 14 new-diff scan | `git diff --unified=0 -- skinny/crates/ir/src skinny/crates/passes/src skinny/crates/codegen/src skinny/crates/runtime/src skinny/crates/bbnf-simd/src skinny/crates/parse-that-regex/src | rg -n '^\\+.*(StructuralAlphabet::json|StrictJson|skip_json|match_json|unescape_json|UnionTape|union_tape|BackendShape)'` | No new generic JSON policy, public JSON helper, `UnionTape`, or `BackendShape` addition outside explicitly allowed per-grammar/generated surfaces. | Any hit must be justified by an allowed surface or W1 rejects under SPEC Section 2.1. |
| Non-JSON proof when generic CostFacts/codegen/IR/passes changed | `cargo xtask regen --grammar css_l4 --check`; `cargo xtask regen --grammar google_sheets --check`; `cargo xtask regen --grammar bbnf --check` | All three exit 0, proving CSS L4, Sheets, and BBNF-self compile/lower/regenerate byte-identical without JSON structural roles. Root xtask `regen --check` regenerates to a tempdir and diffs checked-in `.rs` and `.registry.json` outputs (`xtask/src/main.rs:25-35`, `xtask/src/regen.rs:570-648`); the manifest contains `bbnf`, `css_l4`, and `google_sheets` rows (`Cargo.toml:20-24`). | Any drift or compile/lower failure blocks W1 if the W1 slice touched generic CostFacts/codegen/IR/passes. Skinny-only `check-json` is not enough for SPEC Section 2.1 non-JSON proof (`restart/skinny/tranches/sk-v8/SPEC.md:261-286`). |
| Whitespace/staging hygiene | `git diff --check`; `git diff --name-only --cached` before commit | No whitespace errors; staged files match the intended W1 slice only. | Whitespace failure or unrelated/user files staged. |
| Rollback is commit-sliced | After a W1 redress commit exists: `git diff --name-status HEAD^..HEAD -- skinny restart/skinny/tranches/sk-v8/research`; `git diff --quiet HEAD^..HEAD -- skinny/crates/runtime/src/grammars/json skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/track2 skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/materialization.rs` | The commit contains only CostFacts/report/gate/test/RESULTS or REDRESS paths named by W1; generated/parser/product frozen surfaces are absent. A failed W1 can be reverted by reverting that commit/slice. | Mixed behavior/generated changes in the W1 commit. This violates the W1 revert protocol (`restart/skinny/tranches/sk-v8/SPEC.md:425-427`) and HANDOFF rollback accounting (`restart/skinny/tranches/sk-v8/HANDOFF.md:142-156`). |

Optional rollback simulation, only in a throwaway worktree after a W1 redress
commit exists:

```sh
tmp="$(mktemp -d /tmp/skv8-w1-rollback.XXXXXX)"
git worktree add --detach "$tmp" HEAD
(
  cd "$tmp/skinny" &&
  git revert --no-commit HEAD &&
  CARGO_TARGET_DIR=/tmp/skv8-w1-rollback-target cargo test -p bbnf-bench w0_ -- --nocapture
)
git worktree remove --force "$tmp"
```

Expected signal: the single W1 slice reverts without touching unrelated work,
and the W0 gate/report tests still compile. If the revert needs hand-picked
generated/parser/product reversions, W1 was not commit-sliced.

## Minimum Tests W1 Must Add

The W1 redress should add tests with the `w1_costfacts` prefix so the matrix can
run one focused command. The minimum test set is:

- `w1_costfacts_accepts_complete_materialized_rule_evidence`.
- `w1_costfacts_rejects_missing_rule_id`.
- `w1_costfacts_rejects_missing_chosen_shape`.
- `w1_costfacts_rejects_missing_rejected_alternatives`.
- `w1_costfacts_rejects_missing_evidence_source`.
- `w1_costfacts_rejects_missing_redress_reference`.
- `w1_costfacts_rejects_missing_wave_id`.
- `w1_costfacts_rejects_pre_w1_sentinel_after_w1`.
- `w1_costfacts_rejects_producer_only_rendering`.
- `w1_costfacts_keeps_strict_comparator_refusal_bound`.

The tests should exercise the same validator that `gate-json --with-cost-facts`
calls. If the tests validate a separate helper that the CLI does not call, they
do not prove the W1 same-wave consumer.

## Risks And Pre-Blocks

- Current `gate-json --with-cost-facts` is output-only and uses schema
  `sk-v7-costfacts-v1`; W1 must make it a gate-consumed validation path before
  claiming exit.
- `none:pre-W1` is currently required by W0. W1 must transition the gate without
  weakening W0 baseline checks or allowing mixed pre/post-W1 rows.
- Full-table maintain depends on the admitted W0 Criterion artifacts. If the
  target dir is missing, W1 is blocked until the W0 artifacts are restored or a
  rerun is explicitly routed as REDRESS cost evidence.
- Skinny has only `skinny/grammars/json.bbnf`; CSS L4, Sheets, and BBNF-self
  proof must use the repository-root regen checks if W1 touches generic
  CostFacts/codegen/IR/passes. A skinny-only JSON check is not a non-JSON proof.
- Generated output drift is pre-blocked for default W1. Any generated/parser
  behavior change must split into a separate challenged behavior consumer before
  redress.
- CostFacts are evidence, not performance admission. P3-E blocks CostFacts as a
  hot path, parser change, or automatic route reopen (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:86-96`).

## Sources

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:7-23`,
  `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:41-63`,
  `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:65-88`.
- `restart/skinny/tranches/sk-v8/SPEC.md:40-146`,
  `restart/skinny/tranches/sk-v8/SPEC.md:214-286`,
  `restart/skinny/tranches/sk-v8/SPEC.md:374-429`,
  `restart/skinny/tranches/sk-v8/SPEC.md:756-807`.
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:97-123`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:171-202`.
- `restart/skinny/tranches/sk-v8/HANDOFF.md:130-181`.
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md:207-224`.
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:47-72`.
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:66-92`.
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:104-163`.
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:24-49`,
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:103-141`.
- `skinny/xtask/src/main.rs:7-25`, `skinny/xtask/src/main.rs:34-151`,
  `skinny/xtask/src/main.rs:240-305`.
- `skinny/crates/bbnf-bench/src/report.rs:43-68`,
  `skinny/crates/bbnf-bench/src/report.rs:280-372`,
  `skinny/crates/bbnf-bench/src/report.rs:494-526`,
  `skinny/crates/bbnf-bench/src/report.rs:937-952`,
  `skinny/crates/bbnf-bench/src/report.rs:1007-1045`,
  `skinny/crates/bbnf-bench/src/report.rs:1960-2070`.
- `skinny/crates/bbnf-bench/src/gate.rs:135-176`,
  `skinny/crates/bbnf-bench/src/gate.rs:451-518`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:37-44`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319-335`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1672-1785`.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:129-217`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399-459`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:473-593`.
- `xtask/src/main.rs:23-54`, `xtask/src/regen.rs:177-221`,
  `xtask/src/regen.rs:570-648`, `Cargo.toml:20-24`.
