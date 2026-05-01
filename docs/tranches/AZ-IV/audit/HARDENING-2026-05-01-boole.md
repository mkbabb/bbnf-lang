# AZ-IV Hardening 2026-05-01 - Boole Lane (Dev-Speed + Chronic-Deferral Genealogy)

**Pass**: hardening 3 (dual-scope read-only)
**Lane angle**: dev iteration speed + 5-tranche chronic-deferral genealogy
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-boole`
**HEAD at audit**: master `8abd2ebf`
**Mandate**: name dev-speed bottlenecks and chronic-deferral genealogy; propose architectural fixes and AZ-IV hard-gate language that prevents close-by-routing.

This audit reads existing measurement evidence only. No `cargo`,
`make`, or bench command was invoked.

## Part (a) - Dev Iteration Speed

User directive: *"We must expedite development dramatically insofar as
testing, benching, building."* The user's complaint is not "compile
once a day is slow"; it is *"the iteration loop costs too much per
loop."* Architectural fixes; no bandaids.

### a.1 Observed wall-time matrix (existing evidence)

| # | Surface | Wall (cold) | Wall (warm) | Source |
|---|---|---:|---:|---|
| 1 | `cargo bench-iter-json --no-run` (6 harness build) | 105 s | 0.468 s | `docs/benchmarks/AZ-III/W0p-bench-iter-walls.txt` |
| 2 | `cargo iter-check` (workspace narrow) | 0.41 s (semi-cold) | 0.13 s | `docs/tranches/meta-audit/04-toolchain-pain.md` row 1a/1b |
| 3 | `cargo iter-check` after touch `runtime/parsed.rs` | 4.12 s | -- | `docs/tranches/meta-audit/04-toolchain-pain.md` row 1c |
| 4 | `cargo iter-test-leaf` cold | 41.67 s | 1.10 s | `docs/tranches/meta-audit/04-toolchain-pain.md` row 2a/2b |
| 5 | `cargo check -p bbnf-bootstrap --lib` cold | >130 s (killed @ 23 min cap) | -- | `docs/tranches/meta-audit/04-toolchain-pain.md` row 3b |
| 6 | `cargo check -p gorgeous --lib` cold (parallel) | 42 s wall (0 % CPU; lock-blocked) | -- | `docs/tranches/meta-audit/04-toolchain-pain.md` row 3a |
| 7 | `cargo iter-check-full` (workspace) cold | >12 min floor | -- | `docs/tranches/meta-audit/04-toolchain-pain.md` Pain 2 |
| 8 | `cargo xtask regen --check --staged` | 0.098 s binary / 1.5 s hook wrapper | -- | `docs/benchmarks/AZ-III/W0p-regen-staged-wall.txt` |
| 9 | `cargo xtask regen --check` strict (full fleet) | RED on 7/9 grammars | -- | `docs/benchmarks/AZ-III/W4-structural-audits.txt` |
| 10 | `cargo nextest run --workspace --cargo-profile ax-iter --no-fail-fast` | 292.97 s (190.8 s test wall after compile) | -- | `docs/benchmarks/AZ-III/W4-workspace.txt` |
| 11 | `make doctor` | 0.01 s real | -- | `docs/benchmarks/AZ-III/W0p-doctor.txt` |
| 12 | `cargo bench` json fat-LTO (per harness) | ~10 min cold | -- | `docs/benchmarks/AZ-III/W0p-bench-iter-walls.txt` Verdict |
| 13 | `cargo bench` css.tailwind | WATCHDOG_HALT (>120 s CPU) | -- | `docs/benchmarks/post-AZ-III.json` |
| 14 | `cargo bench` json.data_xl | WATCHDOG_HALT (2.417 s observed vs 1 s limit) | -- | `docs/benchmarks/post-AZ-III.json` |
| 15 | `cargo bench` compile_pipeline.compile_css_l4 | WATCHDOG_HALT (263 ms vs 200 ms limit) | -- | `docs/benchmarks/post-AZ-III.json` |
| 16 | `bash scripts/bootstrap-bbnf.sh` cycle-N (every run) | >130 s (forced cold; cache nuked) | -- | `docs/tranches/meta-audit/04-toolchain-pain.md` Pain 1+6 |

Cold-path floor for the routine close ceremony is therefore
**workspace nextest + bench-iter sweep + strict regen**, which sums to
~7 minutes for nextest (290 s) + ~5 min for bench-iter (5-harness JSON
+ CSS + sheets + bbnf + compile = >5 x 105 s = >525 s) + the bootstrap
re-derivation cost any time the cache is nuked. Empirically the close
gate exceeds 15 minutes of cold compute.

### a.2 Top 3 bottlenecks (architectural, not bandaid)

#### Bottleneck 1: bbnf-bootstrap single-derive wall (>130 s, single-rustc, recurring)

- **Surface**: every `make ay-expand-bbnf`, `scripts/bootstrap-bbnf.sh`,
  every `iter-check-full`, every workspace `cargo check`, CI on every
  push.
- **Root cause**: 133-LOC `BbnfBootstrap` grammar in
  `crates/bootstrap/src/lib.rs` expands ~30 k LOC of `TokenStream` in
  one rustc; `scripts/bootstrap-bbnf.sh:28` deletes
  `target/.bbnf-cache/` before every run, defeating the content-keyed
  cache at `crates/derive/src/lib.rs:300-358`. The proc-macro is
  serialised: codegen-units cannot help.
- **Why it has not been fixed**: B1 was meant to. B1 plan exists; the
  W0.d sub-item that lands the cache-honesty fix is not on master per
  `docs/tranches/meta-audit/04-toolchain-pain.md` Pain 1.
- **Architectural fix (not bandaid)**:
  1. Stop nuking `target/.bbnf-cache/` in `scripts/bootstrap-bbnf.sh`.
     Trust the content-keyed cache; force-refresh via `--clean` flag if
     a developer wants determinism.
  2. Land a `make ay-prime` that primes the cache from a single
     successful expand, documented in PROFILING.md.
  3. Move `bbnf-bootstrap` out of the workspace `iter-check-full`
     surface entirely. Its purpose is diagnostic/compatibility per
     hardening-synthesis 2026-05-01 §Accepted Source Claims item 8.
     If the canonical generated path is the runtime, bbnf-bootstrap
     does not belong on the routine surface.
- **Expected gain**: cold workspace check drops from >12 min to <30 s
  for the bbnf-bootstrap component (a 24x improvement on that lane).

#### Bottleneck 2: bench harnesses cannot iterate without fat-LTO (~10 min/harness cold)

- **Surface**: every benchmark validation, every perf claim refresh,
  every WATCHDOG investigation, every "did this regress?" check.
- **Root cause**: AZ-III.W0p added `[profile.bench-iter]` with
  `lto = "off"`, `codegen-units = 16`, `incremental = true`,
  `debug = "line-tables-only"`, dropping cold compile from 600 s to
  105 s (6x). But three rows still WATCHDOG_HALT:
  `json.data_xl`, `css.tailwind`, `compile_css_l4`. Two cannot be
  measured without bandaid harness modify-carves
  (`json_monolithic` + `compile_pipeline` per AZ-III.W4). The fat-LTO
  bench profile remains the only publishable numeric, and its compile
  cost is the mechanical reason post-AZ-III rows are
  cross-profile-stale vs post-AU/post-AZ.
- **Why it has not been fixed**: WATCHDOG_HALT regressions were
  routed forward at every tranche from AZ-I (post-AY 70-80 % regression
  vs AU baseline; `docs/tranches/AZ-II/FINAL.md` line 122) without
  isolating root cause. AZ-IV W3 hard gate now requires no watchdog
  rows but does not budget the profiling work that resolves them.
- **Architectural fix (not bandaid)**:
  1. Treat each WATCHDOG_HALT row as a **named perf bug** with an
     owner and a profile artefact, not a tolerated outcome. Profile
     before patching.
  2. Add a `[profile.bench-iter-pgo]` that lands per-grammar PGO
     profile data so iter-mode benches reproduce within 2x of fat-LTO
     numerics; close evidence cites both profiles.
  3. Bench cold-wall ceiling becomes a numeric in AZ-IV.W3 hard gate,
     not the implicit "watchdog row is invalid".
- **Expected gain**: bench iteration cycles drop from ~10 min/harness
  to <2 min/harness (the iter-pgo number, while close evidence pays
  fat-LTO once); WATCHDOG_HALT items become named bugs that close
  rather than chronic carries.

#### Bottleneck 3: workspace test+compile cycle is 5+ minutes per round

- **Surface**: every close ceremony, every successor-letter open
  preflight, every "did I break something" check; CI on every push.
- **Root cause**: 1527 tests across 137 binaries (per
  `docs/benchmarks/AZ-III/W4-workspace.txt`) all compile through one
  cargo invocation per shard. CI ships a 3-shard partition per
  `W0p-ci-partition.txt`, but local dev has no equivalent until the
  developer manually invokes shard count. 92.1 % pass rate at AZ-III
  close masks 118 known-failing + 2 timeout tests, all routed forward
  to BA/BB/AZ-IV.
- **Why it has not been fixed**: tier discipline (`scripts/test-tier.sh`)
  exists but routine workflow uses `cargo iter-test-leaf` (41 s cold,
  1 s warm) which excludes the heavy crates. Heavy tier hits the
  workspace 290 s wall every time. Test failures are routed but never
  closed (see Part b items 3, 5, 7 below).
- **Architectural fix (not bandaid)**:
  1. Local mirror of the CI 3-shard partition with a
     `cargo iter-test-shard N` alias; per-shard wall is ~100 s and
     maps onto a single agent's iteration loop.
  2. The 118 failing tests need bisection: each one lives in a known
     carry register but the tests pollute the workspace pass-rate
     signal. Either fix or `#[ignore]` with a tracked owner -
     never both `failing` and `tracked`.
  3. The 2 lsp timeouts at 90 s (sibling-repo state) need the lsp
     binding fixed or the tests gated behind a feature - not
     "carried-forward to next tranche".
- **Expected gain**: routine local cycle drops from ~5 min to ~100 s
  per shard; pass-rate signal becomes meaningful again.

### a.3 What does 10x dev speed look like?

Concrete: today's 5-min routine close ceremony becomes 30 s; today's
10-min bench iteration becomes 1 min; today's 130-s bbnf-bootstrap
cold drops to <5 s warm cache hit. Components:

| Component | Today | Target | Mechanism | Risk |
|---|---:|---:|---|---|
| bbnf-bootstrap derive cycle | 130 s cold every time (cache nuked) | 1 s warm cache hit | cache-honesty fix + bbnf-bootstrap off iter-check-full | low; cache is content-keyed; failure = cold rebuild |
| bench iter compile (per harness) | 105 s cold | 30 s cold (with PGO data shared) | bench-iter-pgo profile + grammar-shard parallel build | medium; PGO data freshness |
| Workspace test cycle | 5 min cold | 100 s per-shard | local 3-shard alias + nuked failing-test backlog | low; structural |
| Strict regen | RED on 7/9 grammars | GREEN live | absorbed in W0 (chronic carry; see Part b item 1) | high; this is the chronic blocker |
| Close ceremony total | ~15 min | ~3 min | union of above | low; each component is structural |

The 10x figure is the union, not the per-component multiplier.

### a.4 Proposed AZ-IV W0/W3 amendments (text in §Exact Wave-Amendment Text)

W0 needs a "dev-iteration baseline gate" hard-gate row capturing
cold-wall measurement for the routine surface; W3 needs an explicit
WATCHDOG_HALT root-cause-or-fix discipline (no row may close on a
"profile artefact saved" without a named consumer of that profile).

## Part (b) - 5-Tranche Chronic Deferral Genealogy

User directive: *"We must not keep deferring and over-optimizing
superfluous nonsense."* Chronic deferral is the failure mode the
tranche-drift meta-audit named (`docs/tranches/meta-audit/03-tranche-drift.md`).
Tally per item across **B5 + AZ-I + AZ-II + AZ-III + AZ-IV(planned)**.

### b.1 Per-item genealogy (15 items)

#### Item 1: Strict regen drift (`cargo xtask regen --check` RED on 7/9)

- **First appearance**: B2 introduced `cargo xtask regen --check` as
  canonical CI/pre-commit gate. B2 closed clean.
- **B5**: not the focus; B5 was substrate cleanup. Regen exit-0 noted
  in §Defensible floor (`B5/FINAL.md:352`).
- **AZ-I**: deferred; W2-act recovery flagged it as "later".
- **AZ-II**: deferred to AZ-III.W1 - O5 Reclose (per `AZ-II/FINAL.md`
  cutover.O.5 row "blocked in cutover.O.5").
- **AZ-III**: NAMED-BLOCKER -> MET-AT-CARRY (per AZ-III FINAL Hard Gate
  1). AZ-III closed strict regen as PASSING via xtask
  content-equality skip; strict mode still RED on 4 substrate
  divergences (W2.4.u keyword span; W3a.4 entry-rule classifier;
  HRegex payload; PHF table generation). Routed to BB.W0; BB never
  opened.
- **AZ-IV planned**: W0 - Truth And Canonical Regen (Carry Ledger
  row "Strict regen drift").
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: regen drift is the visible symptom of
  the canonical generation path having known unfilled gaps - keyword
  Span synthesis, entry-rule classifier (Scalar vs Array), HRegex
  payload typing, PHF table generation. Each is a generator
  capability, not a target-tree manual fix. Carry pattern: every
  tranche says "the consumer matters more this wave" and the strict
  gate gets routed forward.
- **AZ-IV hard-gate language**: routing this to a successor tranche
  is forbidden. AZ-IV.W0 hard-gate item 1 is unequivocal: strict
  green for 9/9 manifest grammars, archived under
  `docs/benchmarks/AZ-IV/W0-regen.txt`. **NON-ROUTABLE**.

#### Item 2: Egraph cost extractor strips `Map { fn_id }` wrapper

- **First appearance**: AZ-III.W3c.1 (alt_dispatch named_color emitter
  substrate landed; runtime activation blocked because cost extractor
  takes the cheaper inner node).
- **B5/AZ-I/AZ-II**: not yet relevant (no egraph extraction over Map
  wrappers).
- **AZ-III**: NAMED-CARRY routed to BB.W0 cost-model preflight.
- **AZ-IV planned**: W0 (Carry Ledger row "Egraph `Map` stripping").
- **Tranches deferred**: 2 (AZ-III, AZ-IV-planned). Recent;
  not yet chronic but on track.
- **Architectural root cause**: cost model is heuristic-coded
  (cheaper node wins) without semantic guard. The fix is a
  preservation rule in extraction: "if Map wrapper is reachable from
  any rewrite, the wrapped form is the cost minimum." Pluggable
  cost-model rule per `feedback_pluggable_components`.
- **AZ-IV hard-gate language**: AZ-IV §Hard Gates row 5 already
  forbids Map stripping. Strengthen by **NON-ROUTABLE** language.

#### Item 3: Sheets parity gap (11 named tests + remainder cluster)

- **First appearance**: AZ-I (CSS L4 + Sheets struct-direct routing
  flipped at W2-act; Sheets parity harness recoded at AZ-I.B2).
- **AZ-II**: deferred. Cutover.M flipped Sheets resolver-arm; parity
  not at 100 %.
- **AZ-III.W2.3**: 100 -> 122/133 (+22; 11 NAMED-CARRIED to W3c).
  W3c.2 routed to BA.W0 path-API substrate.
- **AZ-IV planned**: W1 - Runtime Surface And Semantic Parity (BA
  carry row "BA typed path/query product"). Hardening synthesis 2026-05-01
  raised concern: "prior docs say 122/133, but a hardening live rerun
  saw 115/133" - **regression detected, not just deferral**.
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: Sheets parity needs path-API surface
  (typed accessors), which itself is BA scope. AZ-IV folds BA in,
  but Sheets parity has been "the path API will fix it" for 4
  tranches without any path-API ever shipping.
- **AZ-IV hard-gate language**: AZ-IV.W1 hard gate must require
  Sheets parity proven from regenerated tempdir output (not stale
  artefact), with the regression to 115/133 identified and reverted
  before close. **NON-ROUTABLE**.

#### Item 4: Tailwind regex_scan perf timeout (>120 s CPU watchdog)

- **First appearance**: AZ-I (post-AY data-grammar perf regression
  cluster).
- **AZ-II**: deferred under "fat-LTO bench compile takes >10 min per
  harness" (`AZ-II/FINAL.md` cutover.O.6 row).
- **AZ-III.W2.2**: NAMED-CARRY to BB.W2 (CSS-wide alphabet
  enumeration); also W4 row `css.tailwind` is WATCHDOG_HALT in
  `post-AZ-III.json`.
- **AZ-IV planned**: W2 - Optimization Substrate Activation (Carry
  Ledger row "Tailwind regex timeout").
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: per AZ-III FINAL line 261, no
  profile artefact has been produced isolating which regex is
  the timeout class. The carry has been routed for 4 tranches
  without anyone profiling it.
- **AZ-IV hard-gate language**: AZ-IV.W2 close requires
  `docs/benchmarks/AZ-IV/profiles/tailwind-profile.json.gz` plus the
  named regex/scan operation that is the hot loop. AZ-IV.W3 hard
  gate 8 already requires no watchdog row; tighten to require the
  measured row carry the profile-pinpointed mechanism. **NON-ROUTABLE**.

#### Item 5: TS backend executable parity

- **First appearance**: AZ-I (W2-act.close noted TS backend as
  "string-checked, not executable").
- **AZ-II**: deferred. Cutover.O does not address TS at all.
- **AZ-III.W3c.2**: 1 `ts_backend_emits_discriminated_union` test
  failing; routed to BA.W2 host-binding isomorphism.
- **AZ-IV planned**: W1 (Carry Ledger row "TS discriminated union" +
  "BA host-binding isomorphism"). Hardening synthesis 2026-05-01
  §Accepted Grammar Claims item 7: "TS is red and under-gated:
  structural string tests do not prove executable parser parity."
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: TS backend was promoted to
  "executable" in plan, never on master. Test framework currently
  greps emitted strings rather than running the parser through Node.
  Each tranche routed forward because no Node executor was in scope.
- **AZ-IV hard-gate language**: AZ-IV §Invariants item 5 already
  requires "TS is executable rather than string-checked." Tighten
  W1 hard gate to require `node` execution proof and reject
  string-grep-only proof. **NON-ROUTABLE**.

#### Item 6: Watchdog bench rows under cross-profile (3 rows)

- **First appearance**: AZ-III.W4 introduced `[profile.bench-iter]`
  for fast iteration; the publication-grade fat-LTO comparison was
  routed forward.
- **B5/AZ-I/AZ-II**: prior tranches paid fat-LTO cost; the
  cross-profile gap is AZ-III's specific consequence of the
  bench-iter profile addition.
- **AZ-III**: NAMED-CARRY (`json.data_xl`, `css.tailwind`,
  `compile_css_l4`). Routed to BB.close.
- **AZ-IV planned**: W3 - Measurement And Close (Carry Ledger row
  "Watchdog bench rows" + "Post-AU and sonic-rs performance floor").
- **Tranches deferred**: 2 (AZ-III, AZ-IV-planned).
  Recent. Tightly coupled to Item 4 (Tailwind) and Item 7 (sonic-rs
  perf).
- **Architectural root cause**: bench-iter is iter-fast but
  numerically unrepresentative; fat-LTO is publishable but compile-
  expensive. The user wants both. Solution is per Bottleneck 2:
  PGO-driven iter profile that approaches fat-LTO numerics in <2x
  the compile cost.
- **AZ-IV hard-gate language**: AZ-IV.W3 hard gate 6 already
  requires row-by-row AU floor + post-AZ same-profile deltas + AZ-III
  bench-iter deltas with status `MEASURED`. Tighten: "no row may be
  watchdog-routed" plus "no row may close on a profile artefact
  without a routed fix or named blocker accepted by triumvirate."
  **NON-ROUTABLE**.

#### Item 7: JSON value/path projection vs sonic-rs perf

- **First appearance**: AZ-I (post-AY perf regression vs AU baseline
  -55.6 % canada / -39.5 % citm / -28.7 % twitter from
  `AZ-II/FINAL.md` line 116-118).
- **AZ-II**: deferred under cutover.O.6 (fat-LTO bench compile
  cost).
- **AZ-III.W2.1**: parity 5/5 GREEN via cast_f64 oracle. Throughput
  refresh in `post-AZ-III.json` measured under bench-iter (no apples-
  to-apples vs AU/AZ-I).
- **AZ-IV planned**: W3 (Carry Ledger row "Post-AU and sonic-rs
  performance floor"). Hard Gate 12: same-harness sonic-rs path/value
  rows.
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: parse-only perf has been the
  measurement; struct projection (which sonic-rs excels at) has
  never been benchmarked against bbnf at parity. Until the parse
  path was settled (AZ-III.W2.4 `bootstrap_parser.rs` deletion), no
  meaningful number could land. Now the substrate exists; the
  benchmark matrix did not get extended.
- **AZ-IV hard-gate language**: AZ-IV §Invariants item 6 already
  requires "Direct struct projection must perform." Hard Gate 12
  already requires `bbnf_value_*` parity-or-better against
  `sonic_value_*`. **NON-ROUTABLE**.

#### Item 8: CSS named_color runtime activation

- **First appearance**: AZ-I (CSS L4 W2 substrate stage).
- **AZ-II**: deferred at cutover.M (Sheets/CSS parity remained
  partial).
- **AZ-III.W2.2**: substrate landed; `named_color` payload emission
  blocked because alt_dispatch egraph extraction strips Map wrapper
  (Item 2). NAMED-CARRY to W3c, then to BB.W0.
- **AZ-IV planned**: W1 (Carry Ledger row "Sheets parity gap"
  alongside named-color cluster). Hard Gate 4: "CSS/Sheets parity
  cannot close on early-return payload gaps, hand normalizer
  equivalence, rule-name projection, or synthetic default payloads."
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: blocks on Item 2 (Map stripping).
  Fix Item 2 first, then named_color trivially activates.
- **AZ-IV hard-gate language**: AZ-IV.W1 close requires named_color
  payload parity vs lightningcss; AZ-IV.W2 requires Map preservation
  test. Bind together: W1 cannot MET without W0 closing item 2.
  **NON-ROUTABLE**.

#### Item 9: PatternAnnotations migration

- **First appearance**: pre-B5 (Pratt detection consumer; legacy).
- **B5**: not addressed.
- **AZ-I/AZ-II/AZ-III**: not addressed in any FINAL.md.
- **AZ-IV planned**: W2 - Optimization Substrate Activation
  (substrate denominator ledger). Hardening synthesis 2026-05-01
  §Narrowed Claims: "PatternAnnotations can delete immediately ->
  migrate Pratt and any remaining consumers first."
- **Tranches deferred**: 5+ (B5, AZ-I, AZ-II, AZ-III, AZ-IV-planned).
  **CHRONIC**.
- **Architectural root cause**: migration target unclear (Pratt
  consumer expects PatternAnnotations API); no owner. Each tranche
  said "out-of-scope" without naming a successor.
- **AZ-IV hard-gate language**: AZ-IV.W2 substrate denominator
  ledger must enumerate PatternAnnotations as a tracked legacy with
  named consumer migration plan + timeline; W2 close requires
  migration complete or PatternAnnotations deleted. **NON-ROUTABLE**.

#### Item 10: Bootstrap residue / derive residue

- **First appearance**: B2 (proc-macro IR-pipeline contract retired;
  `crates/derive/` deleted outright). But `bbnf_derive` references
  persisted in WASM lock and parse-that.
- **B5/AZ-I/AZ-II**: not addressed.
- **AZ-III**: AZ-III.W2.4 deleted `bootstrap_parser.rs` 1505 LOC -
  the keystone. But sibling-lib derive residue (parse-that bootstrap
  still depends on deprecated `bbnf_derive` per hardening synthesis
  2026-05-01) remained.
- **AZ-IV planned**: W0 (Carry Ledger row "WASM/derive residue").
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: derive deletion was scoped to bbnf
  workspace; sibling repos (parse-that, wasm) carried independent
  copies. No cross-repo gate.
- **AZ-IV hard-gate language**: AZ-IV.W0 hard gate 2 already
  requires `cargo metadata --locked` clean at root + wasm/. Tighten
  to include parse-that. Hard Gate 10 already names "derive/bootstrap
  residue, duplicated host shims, stale package locks." **NON-ROUTABLE**.

#### Item 11: DTA / dfa naming/cleanup

- **First appearance**: B5 (god-module split scope; DTA wording
  identified as legacy).
- **AZ-I/AZ-II**: not addressed.
- **AZ-III**: partial - `crates/runtime/dta.rs` deleted at W1; DTA
  wording in IR comments scrubbed at W1. But DTA as fact-extraction
  surface remains (per hardening synthesis 2026-05-01: "DTA is not
  dead. It remains a regex/operator/precedence fact source, but
  walker/tape/runtime-table wording is stale").
- **AZ-IV planned**: W0/W2 (Loss-prevention synthesis "DTA/dfa
  cleanup"). Hard Gate 10 names "DTA walker/tape wording" for
  deletion.
- **Tranches deferred**: 4 (B5, AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: DTA = "deterministic tape automaton";
  the substrate became fact-extraction-only without rename. Documents
  refer to walkers and tapes that no longer exist.
- **AZ-IV hard-gate language**: AZ-IV.W2 substrate denominator must
  enumerate every DTA reference with current consumer; non-consumed
  references are deleted; consumed references rename. **NON-ROUTABLE**.

#### Item 12: Backend/rust/view/color hack

- **First appearance**: pre-B5 (compatibility shim for `Color`
  enum during CSS L4 substrate cutover).
- **B5/AZ-I/AZ-II/AZ-III**: not addressed.
- **AZ-IV planned**: W2 (Loss-prevention synthesis names "old color
  compatibility" for deletion). Hard Gate 10 names "old color
  compatibility."
- **Tranches deferred**: 5+ (B5, AZ-I, AZ-II, AZ-III, AZ-IV-planned).
  **CHRONIC**.
- **Architectural root cause**: dual `Color` types (legacy
  `backend/rust/view/color` vs current `runtime::css_l4::CssColor`)
  with shim re-export; no migration owner. Per hardening synthesis:
  "old `Color` is compatibility/test-used; runtime `CssColor` is
  current."
- **AZ-IV hard-gate language**: AZ-IV.W2 must delete
  `backend/rust/view/color` shim or rename it with named consumer.
  **NON-ROUTABLE**.

#### Item 13: Substrate denominator (CSP/regex/SIMD/Pratt/view consumption)

- **First appearance**: AZ-I (substrate emitters landed; consumer
  wiring deferred to W2-act).
- **AZ-II**: cutover.M activated some; CSP shape/layout/dispatch
  still under-consumed.
- **AZ-III.W3b**: 4 sub-units MET (shape/layout/dispatch installers
  + csp-solver alignment); BUT hardening synthesis 2026-05-01 §Accepted
  Optimization Claims: "CSP is consumed, but not authoritative
  everywhere," "Generic shape dictionary runtime consumption is not
  substantiated," "SIMD/structural scan is wired but gated and
  narrower than older comments imply."
- **AZ-IV planned**: W2 (Carry Ledger row "Full substrate denominator").
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: substrate-with-consumer is an
  invariant in every recent tranche FINAL but never exhaustively
  enumerated and proven. Tranches close on "substrate landed" without
  the consumer ledger.
- **AZ-IV hard-gate language**: AZ-IV §Hard Gates item 9 already
  requires "Every active shape, miner, Pratt, view, structural scan,
  regex HIR, CSP, and egraph fact is in the denominator ledger and
  has generated/runtime consumer evidence or deletion proof." Tighten:
  ledger must be exhaustive and machine-checkable; W2 cannot close
  without 100 % rows resolved. **NON-ROUTABLE**.

#### Item 14: Rewrite/ruler production wiring

- **First appearance**: BB tranche scaffolded (`docs/tranches/BB/`).
- **AZ-I/AZ-II/AZ-III**: not in scope; BB always "future tranche."
- **AZ-IV planned**: W2 (BB absorption per `AZ-IV.md` §Cross-Tranche
  Debt + Carry Ledger row "BB rewrite/ruler program" + "Rewrite/ruler
  substrate unconsumed"). Hardening synthesis 2026-05-01: "Ruler and
  RuleSet are live substrate, but production egraph/codegen still
  does not consume loaded rules end to end."
- **Tranches deferred**: 4+ (AZ-I, AZ-II, AZ-III all routed BB
  forward; AZ-IV-planned absorbs). **CHRONIC**.
- **Architectural root cause**: rewrite/ruler substrate landed
  speculatively in `crates/ir/src/rewrites/` and `crates/egraph/src/ruler/`
  without a named end-to-end consumer. Fixing Item 13 (denominator)
  forces this to surface.
- **AZ-IV hard-gate language**: AZ-IV §Hard Gates item 7 already
  requires "Every non-empty loaded rewrite/ruler ruleset traverses
  the full production chain: RON load, egraph search/apply,
  VM-residue oracle where egraph is silent, rank/tier, extraction,
  writeback, generated Rust diff, oracle proof, and benchmark/parity
  non-regression." **NON-ROUTABLE**.

#### Item 15: WASM lock drift / sibling-lib derive residue

- **First appearance**: B2 (`crates/derive/` deletion; sibling locks
  not synced).
- **AZ-I/AZ-II/AZ-III**: not addressed.
- **AZ-IV planned**: W0 (Carry Ledger row "WASM/derive residue"). Per
  hardening synthesis 2026-05-01 §Accepted Sibling Claims: "WASM still
  carries `bbnf_derive`/deleted derive residue in lock/config surfaces."
- **Tranches deferred**: 4 (AZ-I, AZ-II, AZ-III, AZ-IV-planned). **CHRONIC**.
- **Architectural root cause**: cross-repo coordination has no gate.
  Each tranche says "wasm/parse-that are sibling repos, out of scope."
- **AZ-IV hard-gate language**: AZ-IV.W0 hard gate 2 already
  requires `cargo metadata --locked` at root + wasm/. Extend to
  include parse-that and a `cargo deny` rule rejecting `bbnf_derive`
  references in any sibling lock. **NON-ROUTABLE**.

### b.2 Tally - Deferral Count >= 3 tranches

| Item | Tranches deferred | Architectural root cause |
|---|---:|---|
| 1 - Strict regen drift | 4 | generator capability gaps (keyword Span, classifier, HRegex, PHF); never paid down |
| 3 - Sheets parity gap | 4 | "path API will fix it" carried for 4 tranches; path API never shipped; regression to 115/133 detected |
| 4 - Tailwind regex_scan timeout | 4 | no profile artefact ever produced; routed without isolating root cause |
| 5 - TS backend executable parity | 4 | structural string tests substituted for Node execution; no executor in scope |
| 7 - JSON value/path projection vs sonic-rs | 4 | parse-only perf measured; struct projection never benchmarked at parity |
| 8 - CSS named_color runtime activation | 4 | blocks on item 2 (Map stripping); item 2 was always "next tranche" |
| 9 - PatternAnnotations migration | 5+ | Pratt consumer expects API; no migration owner across 5 tranches |
| 10 - Bootstrap/derive residue | 4 | bbnf workspace cleaned; sibling repos carried independent copies; no cross-repo gate |
| 11 - DTA/dfa naming/cleanup | 5 | DTA = legacy "deterministic tape automaton"; substrate became fact-extraction without rename |
| 12 - Backend/rust/view/color hack | 5+ | dual `Color` types with shim re-export; no migration owner |
| 13 - Substrate denominator (CSP/regex/SIMD/Pratt/view) | 4 | substrate-with-consumer invariant unproven exhaustively across 4 tranches |
| 14 - Rewrite/ruler production wiring | 4+ | substrate landed in `crates/ir/src/rewrites/` without named end-to-end consumer |
| 15 - WASM/sibling derive residue | 4 | no cross-repo gate; "out of scope" per tranche |

**13 of 15 items deferred >= 3 tranches**. Only items 2 (Egraph Map
stripping; recent) and 6 (Watchdog rows; recent) are not yet chronic.

### b.3 Architectural root-cause naming

Three meta-causes drive chronic deferral across 13 items:

1. **"Substrate-with-consumer" routinely declared MET on substrate
   alone.** Items 2, 8, 13, 14 all share this pattern: substrate
   landed, consumer carried forward. The `feedback_substrate_with_consumer`
   precept exists but does not block close.
2. **Sibling-repo / cross-repo work has no gate.** Items 10, 15.
   Each tranche says "out of scope, sibling repo." No tranche owns
   them.
3. **Stale-but-reachable code lacks a deletion deadline.** Items
   9, 11, 12. Each tranche says "deletable later." No deadline.

### b.4 Recommended AZ-IV hard-gate language: NON-ROUTABLE

The Carry Ledger format is **necessary but insufficient**. A "carry
row" can be honored by routing to a successor letter; that pattern
created the 13-item chronic backlog. AZ-IV needs **non-routable**
language that explicitly forbids close-by-routing.

Proposed text in §Exact Wave-Amendment Text below.

## Exact Wave-Amendment Text

### Amendment 1: AZ-IV.md - new "Non-Routable Carries" section

Insert after `## Carry Ledger` table, before `## Wave Table`:

```markdown
## Non-Routable Carries

The 13 items below have been deferred >= 3 tranches across B5, AZ-I,
AZ-II, AZ-III, and AZ-IV-planned per
`docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-boole.md` §b.2.
They are designated **non-routable in AZ-IV**: AZ-IV cannot close
by routing them to a successor letter. They land inside AZ-IV with
evidence, or AZ-IV does not close.

| # | Item | Owner wave | Closure proof |
|---|---|---|---|
| 1 | Strict regen drift (7/9 grammars red) | W0 | `cargo xtask regen --check` green live for 9/9; archive `W0-regen.txt` |
| 3 | Sheets parity gap | W1 | full Sheets parity GREEN from regenerated tempdir output; named regression to 115/133 reverted |
| 4 | Tailwind regex_scan perf timeout | W2 | `profiles/tailwind-profile.json.gz` plus named hot regex/scan op + non-watchdog measured row |
| 5 | TS backend executable parity | W1 | TS backend emits + typechecks + Node-executes representative grammars |
| 7 | JSON value/path vs sonic-rs perf | W3 | `bbnf_value_*` parity-or-better against `sonic_value_*` same-harness, fat-LTO |
| 8 | CSS named_color runtime activation | W1 (binds W0) | named_color payload parity vs lightningcss; W1 close blocked until W0 item 2 closes |
| 9 | PatternAnnotations migration | W2 | every consumer migrated or PatternAnnotations deleted |
| 10 | Bootstrap/derive residue (sibling) | W0 | `cargo metadata --locked` at root + wasm/ + parse-that; `cargo deny` rule rejects `bbnf_derive` |
| 11 | DTA/dfa naming and cleanup | W2 | every DTA reference enumerated with current consumer; non-consumed deleted, consumed renamed |
| 12 | Backend/rust/view/color hack | W2 | `backend/rust/view/color` shim deleted or named consumer migrated |
| 13 | Substrate denominator (CSP/regex/SIMD/Pratt/view) | W2 | exhaustive ledger machine-checkable; 100 % rows resolved (consumed or deleted) |
| 14 | Rewrite/ruler production wiring | W2 | every non-empty ruleset proves load/apply/extract/writeback/generated diff/oracle/bench |
| 15 | WASM/sibling derive residue | W0 | locks clean at root + wasm/ + parse-that; sibling sync gate live |

A non-routable item that cannot land inside AZ-IV does not get a
new successor letter; it triggers a triumvirate scope-reveal review
of the AZ-IV thesis itself.
```

### Amendment 2: AZ-IV.W0.md - new hard-gate row 7 (dev-iteration baseline)

In `docs/tranches/AZ-IV/waves/W0.md` §Hard Gate, append after current
item 6:

```markdown
7. Dev-iteration baseline gate: `docs/benchmarks/AZ-IV/W0-dev-baseline.txt`
   records cold and warm walls for `cargo iter-check`,
   `cargo iter-test-leaf`, `cargo iter-check-full`,
   `cargo bench-iter-json --no-run`, `cargo nextest run --workspace
   --cargo-profile ax-iter`, and `cargo xtask regen --check`. Each row
   carries (a) the wall, (b) the AZ-III baseline from
   `docs/benchmarks/AZ-III/W0p-bench-iter-walls.txt` and
   `docs/tranches/meta-audit/04-toolchain-pain.md`, (c) delta vs
   baseline, (d) status MET / REGRESSED / IMPROVED. Regressions block
   W0 close until root-caused.
```

In §Scope, add:

```markdown
8. Stop `scripts/bootstrap-bbnf.sh` from nuking `target/.bbnf-cache/`;
   the content-keyed cache at `crates/derive/src/lib.rs:300-358` is
   the truth. Cycle-2 wall must be <= 10 % of cycle-1 wall.
```

In §Verification Artefacts, add:

```markdown
- `docs/benchmarks/AZ-IV/W0-dev-baseline.txt`
- `docs/benchmarks/AZ-IV/W0-bootstrap-cache-honesty.txt`
```

### Amendment 3: AZ-IV.W3.md - tighten watchdog/profile discipline

In `docs/tranches/AZ-IV/waves/W3.md` §Hard Gate, replace current
item 9 with:

```markdown
9. Profiles for `data_xl`, `tailwind`, `compile_css_l4`, and every
   target miss include `profile.json.gz`, `profile.json.syms.json`,
   `syms-proof.txt`, and a hotspot summary that names the exact
   function or regex that consumed the watchdog budget. The summary
   carries either (a) a routed fix landing in W3 commits, or (b) a
   triumvirate-accepted blocker with profile evidence proving the
   limit is non-actionable. "Profile saved" without a named consumer
   is not valid close evidence.
```

In §Triumvirate Dispatch, append:

```markdown
- a non-routable carry from AZ-IV.md cannot close in W3 with the
  evidence available; this is a thesis-invalidating scope reveal
  and triggers triumvirate review of AZ-IV thesis, not a successor
  routing.
```

### Amendment 4: AZ-IV.md - tighten Cross-Tranche Debt section

Replace current `## Cross-Tranche Debt` text with:

```markdown
## Cross-Tranche Debt

AZ-IV absorbs BA and BB functionally while rejecting their stale or
contradictory mechanisms. BA's typed path/query requirements land
through the existing runtime/document/type-inference surface. BB's
rewrite/ruler requirements land through the existing
`crates/ir/src/rewrites`, `crates/egraph/src/ruler`, `xtask`, and
egraph pipeline.

AZ-IV is also lossless with respect to the 13 non-routable carries
(see §Non-Routable Carries). Routing a non-routable item to a
successor letter is forbidden. If a non-routable item cannot land
inside AZ-IV without changing the AZ-IV thesis, the response is a
triumvirate review of the thesis - not a new tranche letter.

BC.W5/W6 debug/minimise tooling is not opened unless W3 proves a
close blocker that needs it; if so, it enters a named scope-reveal
ledger before implementation. If any BA/BB item cannot land inside
AZ-IV without changing the thesis, `FINAL.md` must name the exact
successor destination and cite the artefact that proves why it
cannot be absorbed.
```

## Boole Lane Conclusion

Two non-negotiables for AZ-IV:

1. **Dev-speed baseline gate in W0** prevents the iteration-cost
   debt from compounding across waves; every wave starts on the same
   measurable floor.
2. **Non-routable carry list in AZ-IV.md** prevents the 13 chronic
   items from routing to a successor letter for the 5th time. Either
   they close in AZ-IV or AZ-IV does not close.

The user's two directives ("expedite development dramatically" +
"do not keep deferring") map onto these two amendments. Without the
amendments, AZ-IV is the 5th tranche in a row to defer the same 13
items.

---

Authored 2026-05-01 by agent BOOLE for AZ-IV hardening pass 3.
Read-only audit; no source/plan changes outside this file.
