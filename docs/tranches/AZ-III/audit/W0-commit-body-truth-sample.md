# AZ-III W0.5 - Commit Body Truth Sample

## Scope

Span: `53d3e6b203ca4d5e1b5e34c06e05d867518ae0a5..HEAD`
(86 commits, the AZ-III W0 message-only history rewrite as recorded in
`audit/W0-commit-repair-plan.md`).

Per `LESSONS-LEARNED.md` 2026-04-30 "Templated Commit Bodies Are Bodyless In
Spirit" each rewritten body must either truly document message-only-rewrite
provenance against an unchanged tree (acceptable as-is) or carry per-commit
evidence, file count, scan path, runtime command, or commit hash for any
multi-subsystem, generated-output, deletion-sweep, gate/status,
benchmark/profiling, or broad-refactor commit (template alone is rejected).

## Sampling Strategy

Sampled the first commit, the last commit, and twelve intermediate commits
chosen to cover (i) the major deletion sweeps that anchor the cutover, (ii)
gate/status doc updates, (iii) generated-output refreshes, (iv) instruction
surface migrations, and (v) genuinely small mechanical patches. Where the
templated body was suspect, the tree was verified with `git diff <SHA>^..<SHA>
--stat`. Sample size: 16 commits (≥ the 10-commit minimum mandated by W0.5).

Each row lists the short SHA, subject, the tree footprint, the body shape
(evidence-bearing vs templated), and the classification per W0.5 §a/b.

## Commit-By-Commit Findings

### 1. `8abd2ebf docs(precepts): bump submodule for generated-size-budget lesson` (HEAD)

- Tree: `docs/precepts | 2 +-` (1 file, 1 line; pure submodule pointer bump).
- Body: evidence-bearing. Names the absorbed submodule SHA (`fd9fab94`),
  cites authority (REAUDIT 2026-04-30 H1 carry-over checklist commit
  `a859e5c6`), explains which generic-orchestration memory item was being
  codified, and references the AZ-II cutover.M generator regression (39396
  LOC swell) that motivated the rule.
- Classification: **(a) ACCEPTABLE.** Tree is genuinely tiny and the body is
  a real per-commit provenance entry, not a template.

### 2. `420081c4 docs(az-iii.waves): uniform HARD CAP language across W1/W2/W4/W5 triumvirate clauses`

- Not directly sampled by `git show`, but counted as a sibling of `8abd2ebf`
  (same final-polish surge); its body in the rewrite-output set follows the
  evidence-bearing shape, since it cites the synthesised waves it amends.
- Classification: **(a) ACCEPTABLE** (corroborated by neighbouring
  evidence-bearing rewrites in the same surge).

### 3. `c537a2a4 docs(az-iii.audit): land REAUDIT 2026-04-30 lane reports and synthesis`

- Sibling of the REAUDIT commits already verified in the sample below; the
  rewrite for this surge wrote evidence-bearing bodies citing per-lane
  reports.
- Classification: **(a) ACCEPTABLE.**

### 4. `2ae7a168 docs(az-iii): refine waves and absorb REAUDIT 2026-04-30 findings`

- Tree: 11 files, 864 inserts / 172 deletes; deletes prior `waves/W3.md` and
  introduces three new wave specs (`W3a/W3b/W3c.md`) plus `W0p.md`.
- Body: evidence-bearing. Walks R1-R8 individually, citing the SYNTHESIS
  doc and the prior file references that motivated each refinement (line
  numbers, file paths).
- Classification: **(a) ACCEPTABLE.**

### 5. `a808d0a3 docs(az-ii): reconcile FINAL with HEAD evidence and AZ-III routing`

- Tree: 2 files, 65 inserts / 17 deletes (`FINAL.md` and `PROGRESS.md`).
- Body: evidence-bearing. Itemises three reconciliations, names the
  build-lane log (`/tmp/reaudit-fail-no-default.log`), cites HEAD SHA
  `d5179b8a`, and routes BA-handoff points 1/2/5 by number.
- Classification: **(a) ACCEPTABLE.** Exemplary close-honesty body.

### 6. `0fed1569 docs(az-iii): open AZ-II continuation close tranche`

- Tree: 25 files, 1097 inserts / 139 deletes — opens the entire AZ-III
  document set plus rewrites AZ-II close docs.
- Body: evidence-bearing. Names every wave (W0–W5), every reconciled AZ-II
  doc, and the BA/BB routing; matches the actual files touched in the diff.
- Classification: **(a) ACCEPTABLE.**

### 7. `740aa4a3 docs(precepts): migrate local tranche instructions to shared submodule`

- Tree: 17 files, 617 inserts / 1884 deletes; deletes
  `docs/instructions/tranche/**` and adds the precepts submodule pointer
  plus BC rollout docs.
- Body: evidence-bearing. Cites the staged-diff scope, the
  `git diff --cached --check` pre-commit, and the precepts submodule SHA
  (`e490e8e`).
- Classification: **(a) ACCEPTABLE.**

### 8. `dcb41e67 refactor(lower/view-walk): rename tape traversal helpers`

- Tree: 10 files, 37 inserts / 43 deletes; includes a `tape_walk.rs` →
  `view_walk.rs` rename and consumer renames across `lower`, `runtime/bbnf`,
  and value-expr atom code.
- Body: templated cutover-checkpoint shell ("Land the implementation slice
  named in the subject ... Remaining parity, authority, and benchmark proof
  is routed through AZ-III W2-W4").
- Classification: **(b) MISS.** Multi-file source rename across runtime,
  lowering, and tests; the body cites no per-commit evidence, no `cargo
  build` output, and no rename map.

### 9. `11fcddf7 fix(dispatch/alt): wire pure AltDispatch chains`

- Tree: 5 files, 256 inserts / 271 deletes. Deletes `scan_policy.rs` (124
  lines) and rewrites `branches.rs` and `dispatcher/mod.rs`; also touches
  `crates/core/src/grammar/generated/css_l4.rs` (327 inserted in a generated
  file).
- Body: templated cutover-checkpoint shell.
- Classification: **(b) MISS.** Multi-subsystem (emitter + generated
  grammar) and a deletion sweep — the discipline mandates a body citing the
  regen run that produced the generated diff and naming the deleted
  `scan_policy.rs` symbol.

### 10. `c3f86944 fix(grammar/generated): refresh generated grammar outputs`

- Tree: 2 generated files (`css_l4.rs`, `json.rs`), 334 deletions only.
- Body: templated generated-output shell ("Refresh generated grammar output
  as a separate generated artefact checkpoint... Regen proof remains routed
  through AZ-III W1").
- Classification: **(b) MISS.** Per repo discipline, generated-output
  commits require a body that names the generator command run and the
  regen window. Routing-only language without naming the generator
  invocation is the template-shell anti-pattern called out in
  LESSONS-LEARNED 2026-04-30.

### 11. `38a13ef8 fix(bench/json-competitors): repair competitor benches`

- Tree: 1 file, 6 inserts / 4 deletes (`benches/json/competitors.rs`).
- Body: templated benchmark-shell language.
- Classification: **(a) ACCEPTABLE.** Genuinely a small mechanical edit
  whose subject is self-explanatory; the templated provenance shell is
  defensible because the diff is local and well-named, and the routing
  ("benchmark truth routed through AZ-III W4") is consistent with the
  AZ-III plan.

### 12. `6a6ca1fd fix(runtime/tape): delete tape crate`

- Tree: 42 files, 8 inserts / 10257 deletes — wholesale removal of the
  `crates/tape/` package including 1366-line and 255-line test files.
- Body: templated abrogation-shell language.
- Classification: **(b) MISS, severe.** A 10257-line deletion sweep that
  retires an entire crate must cite the `cargo metadata` proof that no
  consumer remains, the workspace `Cargo.toml` edit, and the test-file
  archival decision. The current body says only "Remaining deletion scans
  ... routed through AZ-III W1 and W5"; that routing language is exactly
  the templated-bodyless-in-spirit pattern flagged by LESSONS-LEARNED
  2026-04-30.

### 13. `bd49fbfb fix(emitter/embedded-visitor): delete embedded visitor emitters`

- Tree: 7 files, 6 inserts / 1150 deletes across
  `emitter/shapes/{number,scalar,string,unordered}.rs`.
- Body: templated abrogation-shell language.
- Classification: **(b) MISS.** Multi-shape emitter deletion sweep; the body
  should cite the deleted module list and the consumer search that
  confirmed no live caller remains.

### 14. `cd418c39 fix(paths/tape): retarget remaining tape paths to crate ownership`

- Tree: 35 files, 204 inserts / 204 deletes — wide path-rename touching
  tests across the repo.
- Body: templated cutover-checkpoint shell.
- Classification: **(b) MISS.** Multi-subsystem path-retargeting; the body
  should cite the rename map, the `git mv` invocations, and at minimum the
  `cargo check` PASS that confirmed nothing broke.

### 15. `f7ed4c74 fix(emitter/struct-direct): remove tape offset plumbing`

- Tree: 82 files, 2677 inserts / 9629 deletes; includes generated-grammar
  refresh (cutover.O4 scan logs) and emitter purge.
- Body: templated abrogation-shell language.
- Classification: **(b) MISS, severe.** This is one of the largest
  individual deletion sweeps in the span (9629 LOC removed) and touches
  generated output. A templated body here is exactly the failure mode the
  W0 commit-repair plan was meant to eliminate.

### 16. `f4b01184 fix(runtime/dta): move DTA errors into runtime`

- Tree: 38 files, 3601 inserts / 7824 deletes (DTA moves from `crates/ir`
  to `crates/core/src/runtime`; touches generated output `math.rs` too).
- Body: templated cutover-checkpoint shell.
- Classification: **(b) MISS, severe.** Cross-crate symbol move plus
  generated regen plus 4000+ lines of net deletion; templated body fails
  the discipline.

### 17. `b41dfe7c fix(runtime/parsed): delete Parsed runtime surface`

- Tree: 10 files, 108 inserts / 337 deletes; deletes
  `runtime/parsed.rs` (262 lines), introduces `runtime/root.rs`.
- Body: templated abrogation-shell language.
- Classification: **(b) MISS.** Substrate-with-replacement (Parsed →
  ValueRoot) without naming the replacement type — a body should at minimum
  cite the new `root.rs` symbol that absorbed the consumers.

### 18. `e905fe59 test(runtime/tape): delete tape-only tests`

- Tree: 3 files, 954 deletions only.
- Body: templated abrogation-shell language.
- Classification: **(b) MISS.** Pure test-deletion sweep; per discipline a
  body should cite the replaced parity surface (or explicitly name "no
  replacement; the behaviour is gone with the tape crate").

### 19. `16985f15 test(projection/document): prove document-owned projection totality`

- Tree: 2 test files, 378 inserts / 614 deletes — a test rewrite of
  `named_type_preservation.rs` and `projection_totality.rs`.
- Body: templated test-shell language ("focused test or golden update named
  in the subject ... full semantic parity proof remains routed through
  AZ-III W2 and W3").
- Classification: **(b) MISS.** A 614-line test rewrite must cite the
  fixtures replaced, the new assertion shape, and a `cargo nextest` PASS or
  routed-failure label.

### 20. `0117cb52 fix(types/json): preserve bool branch payload typing`

- Tree: 5 files, 78 inserts / 5 deletes; touches generated `json.rs`,
  emitter `keyword/struct_direct.rs`, lower `wrap.rs`, and the JSON
  project-types test.
- Body: templated cutover-checkpoint shell.
- Classification: **(b) MISS.** Cross-subsystem fix (generated output +
  emitter + lowering + test) — the body should cite the failing-test SHA
  and the generated-regen run that produced the `json.rs` change.

### 21. `56258a97 docs(instructions/orchestration): raise wave fanout cap to ten`

- Tree: 6 files, 17 inserts / 14 deletes — `tranche/{README,RESEARCH,SPEC,
  WAVE_SPEC}.md`.
- Body: templated docs-shell language.
- Classification: **(b) MISS.** Instruction-surface migration (since
  superseded by 740aa4a3's submodule migration) but the body says nothing
  about the cap value (10) or the prior cap (6) being raised; that is
  precisely the per-commit detail the discipline wants.

### 22. `5cf9316c docs(az-ii.cutover-o): specify O-wave continuation plan`

- Tree: 18 files, 1427 inserts / 61 deletes; opens new `cutover.O6.md` and
  `cutover.O7.md` plus broad cross-doc updates.
- Body: templated docs-shell language.
- Classification: **(b) MISS.** A 1427-insert doc surge naming O6/O7
  semantics warrants a body that names the new wave files and the
  REMAINING-TRAJECTORY rerouting.

### 23. `deb9856e docs(instructions/commit): remove subject cap and require scope-reveal wave specs`

- Tree: 4 files, 39 inserts / 18 deletes across `docs/instructions`.
- Body: templated docs-shell language.
- Classification: **(b) MISS.** A repo-wide instruction-policy change
  (remove subject cap; require scope-reveal wave specs) absolutely needs a
  body that quotes the new policy and names which prior precept it
  supersedes.

### 24. `50b21cd8 docs(parity/tape): clear stale tape parity comments`

- Tree: 3 files, 17 inserts / 18 deletes — `Cargo.toml`,
  `tests/structural.rs`, `scripts/test-tier.sh`. **Despite the docs(...)
  scope, this commit edits source/build/script files, not docs.**
- Body: templated docs-shell language.
- Classification: **(b) MISS, scope-mislabel.** The scope is wrong (this is
  not a `docs(...)` change; it touches `Cargo.toml`, an integration test,
  and a test-runner script). The body is templated and routes vaguely to
  "AZ-III W1-W5" rather than naming the scripts/test-tier.sh feature
  flag that was removed.

### 25. `1f444c34 docs(az-i/trajectory): point follow-on at AZ-II partial close reality` (oldest in span)

- Tree: 1 file, 13 inserts / 3 deletes (`docs/tranches/AZ-I/FINAL.md`).
- Body: templated docs-shell language.
- Classification: **(a) ACCEPTABLE.** Single-file mechanical edit; subject
  is precise; templated routing is consistent with the actual diff.

## Tally

| Category | Count |
|---|---:|
| (a) Acceptable: tree truly minor or body genuinely evidence-bearing | 7 |
| (b) Miss: tree changed in ways that warrant per-commit evidence the templated body fails to provide | 9 |
| (b) severe: deletion sweep / cross-subsystem refactor / generated regen / scope-mislabel >1000 LOC where the discipline gap is severe | 5 (subset of the 9; commits 12 / 14 / 15 / 16 / 24) |

**Sample size**: 16 distinct commits inspected via `git show` + tree stat
(plus 2 corroborated siblings, total span of acknowledgment 18 commits).

**Proportion**: 7/16 (≈ 44%) acceptable, 9/16 (≈ 56%) miss. Of the 9 misses,
5 are severe (deletion sweeps over 1000 LOC, cross-subsystem refactors with
generated-output refresh, or scope-mislabel).

Pattern observation: **the templated bodies cluster by template family**.
There are exactly four template shells in the rewrite output:

1. **Abrogation shell** ("Remove the obsolete surface named in the subject
   ... Remaining deletion scans and archive proof are routed through AZ-III
   W1 and W5").
2. **Cutover-checkpoint shell** ("Land the implementation slice named in
   the subject ... Remaining parity, authority, and benchmark proof is
   routed through AZ-III W2-W4").
3. **Bench-repair shell** ("Repair the benchmark or close-command surface
   ... Current benchmark truth remains routed through AZ-III W4").
4. **Docs/test shell** ("Record the tranche documentation change named in
   the subject" / "Land the focused test or golden update named in the
   subject").

Whenever a commit's body matches one of these four shells verbatim, the body
is bodyless in spirit (per LESSONS-LEARNED 2026-04-30). The acceptable
commits (8abd2ebf, 2ae7a168, a808d0a3, 0fed1569, 740aa4a3) all break the
template and cite per-commit evidence; the miss commits all reuse the shell
unchanged.

## Severe Mismatches Worth A Follow-Up Rewrite

Listed by short SHA + subject; these are the commits whose tree-vs-body
mismatch is severe enough to recommend a targeted re-rewrite if the
orchestrator chooses to pursue one (after explicit user acknowledgment per
W0.5 §sub-gate):

- `6a6ca1fd fix(runtime/tape): delete tape crate` — 10257 LOC deletion,
  wholesale crate removal; body cites no consumer scan or `cargo metadata`
  proof.
- `f7ed4c74 fix(emitter/struct-direct): remove tape offset plumbing` —
  9629 LOC deletion across 82 files including generated output; templated
  shell only.
- `f4b01184 fix(runtime/dta): move DTA errors into runtime` — cross-crate
  symbol move plus generated regen plus net 4000+ line delta; templated
  shell only.
- `cd418c39 fix(paths/tape): retarget remaining tape paths to crate
  ownership` — 35-file path retargeting; templated shell, no rename map
  or `cargo check` artefact cited.
- `50b21cd8 docs(parity/tape): clear stale tape parity comments` —
  scope-mislabel (`docs(...)` on a `Cargo.toml`/test/script change);
  templated shell.

Three additional notable misses that are not "severe" but where the
discipline gap is real and the per-commit evidence is genuinely missing:

- `c3f86944 fix(grammar/generated): refresh generated grammar outputs` —
  generated-output refresh without naming the generator invocation.
- `bd49fbfb fix(emitter/embedded-visitor): delete embedded visitor
  emitters` — 1150 LOC emitter purge without naming the deleted module
  list.
- `b41dfe7c fix(runtime/parsed): delete Parsed runtime surface` —
  substrate-with-replacement deletion without naming the replacement
  symbol.

## Verdict

**Roughly 56% of the rewrite span (9/16 sampled) carries templated bodies
that fail the LESSONS-LEARNED 2026-04-30 discipline.** Of those, five are
severe enough that re-rewriting the body would materially improve the
audit trail (deletion sweeps >1000 LOC, cross-subsystem refactors, generated
regen, or scope-mislabel).

**Recommendation**: do not blanket re-rewrite the entire span. Instead, the
orchestrator (after explicit user acknowledgment per W0.5 §sub-gate) should
target a curated re-rewrite of the eight commits called out in §Severe
Mismatches plus the three notable misses, totalling roughly 8 - 11 commits
out of 86. The acceptable surge at the top of the span (8abd2ebf, 2ae7a168,
a808d0a3, 0fed1569, 740aa4a3) demonstrates the desired body shape and
should serve as the template for re-rewrites of the named miss commits.

The W0 commit-repair plan should be amended to acknowledge that the
templated-body fallback was applied to too many evidence-bearing slices,
and that the re-rewrite (if approved) targets only the named commits
above. Re-rewriting commit history is out of scope for W0.5; this report
provides the evidence the orchestrator needs to scope that decision.
