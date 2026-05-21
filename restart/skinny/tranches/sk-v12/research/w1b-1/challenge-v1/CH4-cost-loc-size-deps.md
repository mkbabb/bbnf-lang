# SK-V12 W1b-1 CH4: Cost / LOC / Generated Size / Deps

Date: 2026-05-20.
Scope: W1b-1 CHALLENGE lens CH4 only.
Output: this file.

## Verdict

REVISE.

The plan is directionally legal, but it does not fit the pinned W1b-1 redress
cap as written. SPEC caps W1b-1 at <=360 hand LOC and <=30 min redress
(`SPEC.md:239-253`). The plan asks one redress thread to add a new codegen
profile/provider/templates surface, generated runtime, runtime export, CSS-local
sink, independent `cssparser` oracle, Criterion bench, companion report schema
expansion, report validation tests, fixture/report/artifacts, REDRESS, CSS
bench measurement, companion gate, and full JSON guard rerun
(`PLAN.md:14-25`, `PLAN.md:77-91`, `PLAN.md:93-167`, `PLAN.md:169-188`).

That is too much for a 30-minute redress commit under the pinned discipline. CH4
should not let the implementation phase discover that overrun after source
edits. Redress may proceed only after the plan is narrowed to a cap-fit slice or
the SPEC is explicitly amended with a larger cap.

## Evidence

- The pinned campaign makes the cap binding: W1b-1 is `<=360 hand; generated
  output named separately`, `high`, `<=30 min`, and the general cap says "At
  0.9x cap the agent commits or records the blocking state. At the cap it
  halts" (`SPEC.md:239-257`).
- Section 6 authorizes many files, but authorization is not cost proof:
  grammar inputs, workspace deps, codegen, runtime, bench oracle, report/gate,
  fixture, artifacts, `RESULTS.md`, and `REDRESS.md` are all in scope
  (`SPEC.md:394-417`).
- The plan's hand LOC target totals 405 when read literally, not 360: 95
  codegen + 55 runtime + 145 oracle/bench + 65 report/gate + 40 docs/report
  (`PLAN.md:136-146`). Even if fixture/report/REDRESS are treated as docs
  outside the "hand source" cap, the implementation/code hand slice is exactly
  360 with no slack for tests, glue, imports, or error paths.
- The existing report contract is not a small append. `SkV12NonJsonRow` already
  has 33 serialized fields (`report.rs:169-203`), while the plan adds 13 more
  consumed fields (`PLAN.md:93-112`). The validator currently hardcodes
  `direct_to_struct -> direct_sink` (`report.rs:1897-1905`), so W1b-1 also
  needs output-plane-specific validation for
  `css_l4_declaration_value_fact_stream`, plus positive/negative tests. The
  planned 65 LOC for report/gate validation is therefore optimistic.
- The codegen profile surface is currently JSON-only. `runtime_profiles()`
  returns exactly one profile, `json_provider::runtime_profile()`
  (`grammar_profile.rs:76-78`). Adding CSS without a generic grammar-name branch
  is feasible, but the provider/profile/reproducibility tests are more than a
  token edit.
- The measurement suite is too large for the cap. The plan requires four cargo
  tests, a Criterion bench, companion gate, full JSON bench/gate rerun, cost
  facts gate, and JSON floor awk check (`PLAN.md:153-188`). With new
  dependencies and first-build compile cost, this is not a 30-minute redress
  commitment.
- The generated-size guard is correctly named but not yet operationally
  bounded. The target `<=300 generated LOC` and `<=14000 module bytes`, with an
  O(N) explanation against the 405 LOC / 18114 byte grammar-input baseline, is
  acceptable as a gate (`PLAN.md:148-151`). Redress must compute these numbers
  from committed generated files and reject producer-only claims.
- The `cssparser` dependency is correctly scoped if, and only if, it is added to
  `[workspace.dependencies]` in `skinny/Cargo.toml` and consumed only by
  `bbnf-bench` (`Cargo.toml:23-47`, `bbnf-bench/Cargo.toml:9-27`,
  `PLAN.md:77-85`). It must not appear in `runtime`, `codegen`, Track 1, or the
  generated module.
- `xtask` already passes `--skv12-non-json-report` through to the bench gate
  (`xtask/src/main.rs:262-280`), so the plan is right to avoid selecting
  `skinny/xtask/src/main.rs` for W1b-1 (`PLAN.md:47-48`).

## Required Redress Preconditions

Before redress, the orchestrator must choose one of these two legal shapes:

1. Split or narrow W1b-1 so the first redress commit fits <=30 min and <=360
   hand LOC. The cap-fit slice should include the fixture, CSS-owned provider
   roster, generated runtime reproducibility test, runtime export, minimal
   Track 1 fact emission, and a unit-level oracle/equality test. Report/gate
   schema expansion plus full Criterion/JSON guard can then be a separately
   authorized follow-on wave, unless SPEC redefines W1b-1 as multi-redress.
2. Amend SPEC Section 6 before redress with an explicit larger cap and
   rationale. Without that amendment, a plan that knowingly exceeds 30 minutes
   violates the pinned campaign discipline.

If the current one-commit W1b-1 shape is retained, redress must pre-register a
hard stop checklist:

- source edit stop at 27 minutes if the generated runtime and oracle equality
  tests are not already green;
- no `cssparser` outside `bbnf-bench`;
- generated LOC and module-byte counters implemented before the report JSON is
  emitted;
- report validation rejects missing or bad `generated_loc`,
  `generated_module_bytes`, `grammar_size_guard`, Lock 14/16, scalar reference,
  and parity fields before any `GO` report can be accepted;
- Criterion and full JSON guard timing are measured against the cap, not treated
  as optional after-work;
- on overrun, save `/tmp/skv12-waveW1b-1-rejected.patch` and record
  BLOCKED/FAIL rather than silently rolling the slice into W1b-2.

## Blockers

1. **Cap blocker:** the plan's implementation plus measurement suite does not
   fit W1b-1's <=30 min cap. This is a CH4 blocker until the plan is split,
   narrowed, or the SPEC cap is amended.
2. **LOC blocker:** the plan's own allocation has no slack and is likely above
   the 360 hand LOC cap once tests and validation are counted. Redress must
   define what counts as hand source versus docs/report artifacts before edit.
3. **Report-cost blocker:** extending `SkV12NonJsonRow` by 13 consumed fields,
   changing the CSS output-plane validator, and adding rejection tests is
   underbudgeted at 65 LOC.
4. **Measurement-cost blocker:** first-build compile, `cargo bench`, full JSON
   guard, cost-facts gate, and floor verification are mandatory in the plan but
   not budgeted against the 30-minute redress wall clock.
5. **Dependency precondition:** `cssparser` is acceptable only as a workspace
   dependency consumed by `bbnf-bench`; any runtime/codegen/generated use is a
   CH4 + CH5 reject.
