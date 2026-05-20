# SK-V12 W0 PIN Plan - Results Manifest Revalidation

Date: 2026-05-20.
Wave: W0 - Pin Telemetry And Gate Revalidation.
Phase: Plan.

## Selection

Select a narrow generated-report exactness revalidation for
`G-W0-PIN-TELEMETRY`.

W0 will not edit `skinny/RESULTS.md`. Redress preflight proved that
`RESULTS.md` is exact generated output: adding surrounding prose makes
`gate-json --check-results` fail stale. The retained `SK-V9-open` manifest
heading, rows, and `sk-v9-open:criterion-fnv64-*` run ids therefore remain
the generated JSON seed authority. SK-V12 pin revalidation context is recorded
in the W0 redress artifact, not inside the generated report.

## Owner Paths

Editable:

- `restart/skinny/tranches/sk-v12/research/w0-pin/REDRESS.md`

Read/verify:

- `restart/skinny/tranches/sk-v12/research/w0-pin/`
- `restart/skinny/tranches/sk-v12/research/p1/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/xtask/src/main.rs`

No parser, scanner, SIMD/ASM, codegen behavior, generated runtime output,
benchmark body, report schema, gate semantic, generated report, or row-data
edit is selected.

## Entry Gate

- S-P3 has converged under the user pin.
- W0 research is committed.
- Worktree slice is clean or unrelated dirty state is isolated.
- `/tmp/skv12-pin-p1`, `skv12-p1-pin-replay.tsv`, PMU/samply/xctrace status,
  and W0 lock `f788eb97` remain accepted.

## Exit Gate

`G-W0-PIN-TELEMETRY` passes when:

- pin profile artifacts exist and are cited;
- the JSON seed state is reconciled;
- retained SK-V9-open manifest rows remain explicitly frozen seed evidence;
- no behavior/source/gate/report-schema drift occurs;
- `gate-json --check-results` and `gate-json --with-cost-facts --check-results`
  pass.

REVISE if the pin profile or W0 companion gate artifact is missing.
FAIL/BLOCKED if W0 lock drift changes result semantics; route back to S-P3.

## Same-Wave Consumer

The same-wave consumer is the existing `gate-json --check-results` result
snapshot validator plus `gate-json --with-cost-facts --check-results`.

The consumer intentionally requires the retained `SK-V9-open`/`sk-v9-open`
manifest fields and compares the generated report exactly. W0 redress
therefore leaves `RESULTS.md` unchanged and records SK-V12 pin context in the
wave redress artifact.

## CHALLENGE

Skip CHALLENGE. The plan changes no gate semantics, accepted wave labels,
run-id grammar, report schema validation, or `gate-json` snapshot semantics.
Per `DISPATCH-PROMPT.md`, W0 CHALLENGE is mandatory only if the plan changes
gate semantics.

## JSON Guard Treatment

Preserve every JSON row id, outcome, verdict, strictness, validation state,
Mbps value, Track 1/Track 2 fact, sample count, sample cost, comparator
evidence, hot-leaf signal, guard status, manifest heading, wave id, and run id.

No fresh JSON or CSS admission is claimed. CSS L4 generation, oracle,
lightningcss comparison, and admission remain W1b-1/W1b-2 work.

## Revert Protocol

If redress fails, revert the W0 redress artifact. No behavior or generated
report patch exists. Edit `skinny/REDRESS.md` only if a measured W0 failure or
blocker must be recorded.

## Verification

Run:

```bash
git status --short
git diff --stat f788eb97..HEAD -- skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/report.rs skinny/xtask/src/main.rs skinny/RESULTS.md
test -f /tmp/skv12-pin-p1/pmu/done.txt
test -f /tmp/skv12-pin-p1/samply/done.txt
test -f /tmp/skv12-pin-p1/xctrace/done.txt
awk -F '\t' 'NR>1 && $7!="PASS"{bad++} END{exit bad?1:0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
awk -F '\t' 'NR>1 && $7!="PASS"{bad++} END{exit bad?1:0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
awk -F '\t' 'NR>1 && $7!="PASS"{bad++} END{exit bad?1:0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```
