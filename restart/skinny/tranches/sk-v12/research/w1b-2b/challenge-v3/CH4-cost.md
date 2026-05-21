# SK-V12 W1b-2b CHALLENGE V3 CH4 - Cost / Budget

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 Lightningcss SOTA Report + Admission Gate.
Lens: CH4 cost / budget.
Disposition: ACCEPT.

## Scope Read

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` §4.
- `restart/skinny/tranches/sk-v12/SPEC.md` §2 and §7.2.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`.
- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- W1b retained fact/equality artifacts under
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/`.

## Findings

No blocking cost findings remain.

PLAN-V3 addresses the V2 budget blockers by raising the W1b-2b source budget
from the stale `<=220` estimate to `<=330 report/gate/test LOC`, routing
`skinny/RESULTS.md` reconciliation to W5, and splitting the evidence into two
commands so one `CRITERION_HOME` no longer has to serve both CSS and JSON
authorities. That is the right budget shape for this wave: W1b-2b is a
companion-gate/report admit candidate, not a renderer or stale-results rewrite.

The `<=330` budget is tight but credible if the redress stays inside PLAN-V3's
bounded implementation style:

- `report.rs`: roughly 145-180 LOC for the dedicated
  `sk-v12-css-l4-sota-v1` structs, one-row validation, exact CSS identity
  checks, threshold/margin derivation, and focused tests. Existing
  `deny_unknown_fields`, `positive_finite`, `is_skv12_run_id`, and SK-V12
  non-JSON validator patterns reduce the cost.
- `gate.rs`: roughly 115-145 LOC for the new companion branch, parser flag
  update, three Criterion lane reads from `new/`, benchmark byte/sample
  validation, Mbps recomputation, report comparison, retained artifact
  freshness checks, and CLI tests. Existing `criterion_root()`,
  companion-report parsing, `serde_json::Value`, and the no-write JSON path
  are reusable.
- Report JSON plus REDRESS/SPEC text are outside the source LOC pressure and
  are already named owner artifacts. `sha2` is already a `bbnf-bench`
  dependency, so fact-stream SHA-256 verification does not require a
  `Cargo.toml`/`Cargo.lock` owner expansion.

The no-RESULTS rule is cost-correct. Current W1b-2a measurements likely make
`PASS-ADMIT-CANDIDATE` the expected outcome, but moving a CSS row into
`skinny/RESULTS.md` while preserving the existing JSON stale-results renderer
would reopen the unbounded branch CH4 rejected in V2. PLAN-V3's route to W5 is
therefore not a workaround; it preserves W1b-2b's falsifiability while keeping
the wave's owner surface finite.

The two-command evidence protocol is credible. The CSS command validates only
`target/criterion/nonjson_css_l4/.../new/` and the CSS report. The JSON guard
then runs separately with `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion`
and no CSS report flag. This avoids a new criterion-root flag and avoids
mixing CSS-only and JSON guard roots in one process.

The 30-minute cap discipline is credible only if redress treats missing/stale
Criterion lanes as rerun-or-fail, not as an invitation to broaden harness work.
PLAN-V3 states that at 0.9x cap the agent commits or records blocking state,
and at cap it halts and saves the rejected patch. That satisfies SPEC §2's cap
discipline.

## Non-Blocking Guardrails For Redress

- Do not change `skinny/RESULTS.md` or the JSON report renderer in W1b-2b.
- Do not add a CSS Criterion root flag unless the two-command protocol proves
  impossible; adding one is likely still small, but it would consume the
  contingency budget.
- Keep artifact freshness checks mechanical: read the five retained W1b files,
  compare fact bytes/SHA-256, require the exact row id, `input_fnv64`,
  `input_bytes`, `stream_fnv64`, and equality `status=pass`. Avoid building a
  general artifact registry.
- Keep comparator isolation as a focused source audit recorded in REDRESS,
  unless another CHALLENGE lens requires it to become executable code.

## Verdict

ACCEPT. PLAN-V3 gives W1b-2b a bounded source budget, finite owner paths,
split-root evidence protocol, no renderer branch, and enforceable cap
discipline. The redress agent should proceed under the `<=330` budget and fail
closed if the implementation starts expanding into RESULTS rendering, benchmark
harness work, or generalized artifact infrastructure.
