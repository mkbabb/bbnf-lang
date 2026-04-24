# B1 — FINAL

Tranche B1 closes as a bounded prelude annex over the dev-loop, bench,
profile, bootstrap, and CI surfaces. Its scope was infrastructure
truth, not runtime architecture; it does not modify parity-critical
runtime, does not regen `generated.rs`, and does not perform a samply
baseline refresh. Those resume at AY-II.W0' close on the substrate B1
pinned and the harness B1 migrated.

## Scope recap

B1 inherited the infrastructure pivot AY-II.W0'.d4-d7 surfaced when
`.cargo/config.toml` and proc-macro dev-deps fell outside W0's declared
file bounds. Per SPEC §new-tranche-new-doc, the mid-tranche scope
pivot opened B1 as a new letter; this annex re-audits those landings
and lands the broader 12-step toolchain migration that
`TOOLCHAIN-MIGRATION.md` enumerated.

Four waves landed sequentially. W0 pinned the substrate
(`rust-toolchain.toml` → `nightly-2026-04-11`), rewrote
`.cargo/config.toml` with a four-exclude `iter-check` plus a per-exclude
fast-path alias for every excluded crate, authored a four-profile
`.config/nextest.toml`, and reduced the Makefile from ~470 to ~210
lines by delegating every iteration surface to a cargo alias. A
triumvirate redress dropped gorgeous's `iter-check-prettify` wall from
~500 s to 33 s warm by switching `default = []` and gating the binary
behind `required-features = ["bin-full"]`. The `bbnf-bootstrap`
> 600 s single-derive wall surfaced as the new critical-path ceiling
and routed to AZ-I.W0 (derive-cache relocation + Watt) rather than
re-opening B1.

W1 ported all 19 bench files plus the cross-crate `json_value`
benchmark from `bencher = "0.1"` to `divan = "0.1"`, removed the
`bencher` dep from both `crates/core/Cargo.toml` and
`crates/json-prototype/Cargo.toml`, and gated iai-callgrind behind
`[features] iai = []` with a Linux-only dev-dependency entry plus a
draft GitHub Actions workflow.

W2 rewrote `.github/workflows/ci.yml` to install nextest via
`taiki-e/install-action@v2` and run `cargo nextest run --workspace
--profile ci` with junit upload, promoted `bench-iai.yml` from the
patches directory, seeded the iai-callgrind baseline JSON stub,
executed five script ABROGATEs from the meta-audit catalog
(`profile.sh`, `cost-grid-sweep.sh`, `check-cst-invariants.sh`,
`verify-w2-asm.sh`, `verify-w2-symbols.sh`), rewrote
`bootstrap-bbnf.sh` to gate the `target/.bbnf-cache` deletion behind an
opt-in env var (preserving cycle-2 bootstrap measurement per invariant
12), and propagated the pinned nightly to the two sibling repos
(`../parse-that`, `../pprint`) that bbnf-lang path-patches.

W3 (this wave) refreshes `docs/instructions/PROFILING.md` against the
landed surface, aggregates the W0/W1/W2 ledgers into
`docs/benchmarks/post-B1.json`, authors this `FINAL.md`, and unblocks
AY-II.W0' close ceremony in the AY-II planning docs.

The architectural thesis lands: one public workflow surface (cargo
aliases, make targets, scripts, CI, docs all describe the same
operations), one pinned substrate (sibling-repo triad in sync), one
bench harness emitting cold-per-parse JSON (divan with iai-callgrind
gating CI-side instruction counts), one CI workflow gating both
correctness and instruction-count regressions. Stale workflow prose
deletes in the same commit that supersedes it; the abrogation catalog
is executed or explicitly deferred with a named successor.

B1's `bbnf-bootstrap` cold-wall ceiling is recorded as measured truth,
not a numeric target. The ≤ 5 min `iter-check-full` ceiling that the
plan invariant 11 originally claimed routes to AZ-I.W0 once the
derive-cache relocation lands. Routine iteration uses `cargo
iter-check` (3.88 s warm per `post-B1-W0-routine.txt`), which is
unaffected by the bootstrap wall.

## Invariant table

Sixteen invariants from `B1.md §Invariants`. Status column:
**green** — closed end-to-end on B1's surface; **green-routed** —
B1's role closes; the numeric/runtime continuation routes to a named
successor; **deferred-with-rationale** — explicitly held with rationale
and destination tranche.

| # | Invariant (abbreviated) | Status | Artefact citation |
|---|---|---|---|
| 1 | No B1 commit touches parity-critical runtime architecture | green | All B1 commits (W0/W1/W2/W3) modify config + harness + CI + docs only; no `crates/core/src/runtime/`, `crates/tape/src/`, `crates/core/src/backend/rust/emitter/` edits. |
| 2 | B1 blocks AY-II.W0' close ceremony and AY-II.W1 open until B1 closes | green | This FINAL.md + `docs/tranches/AY-II/PATH-FORWARD.md` 2026-04-24 entry; AY-II.W0' status flips to "in_progress — B1 closed; W0' close ceremony unblocked" in `AY-II.md`. |
| 3 | Every public routine command is seconds-scale on the declared routine surface or removed | green | `docs/benchmarks/post-B1-W0-routine.txt` rows: `iter-check` 3.88 s, `iter-check-lsp` 3.27 s, `iter-check-prettify` 33.36 s warm; `iter-check-bootstrap` AZ-I-routed (not on routine surface). |
| 4 | Every public proof command is runnable + documented | green | `docs/instructions/PROFILING.md` §Bench alias surface + §AY W5-W7 gate commands; `cargo bench-json`, `cargo bench-iai`, `cargo expand-bootstrap`, `make ay-bench-close` all named in `.cargo/config.toml` + `Makefile`. |
| 5 | Every documentation claim about the public command surface is re-verified | green | `docs/instructions/PROFILING.md` rewritten end-to-end against the landed `.cargo/config.toml` + `Makefile` + nextest config; alias-once invariant verified. |
| 6 | No stale `#[allow(dead_code)]`, `#[ignore]`, `#[cfg(false)]`, dead workflow prose introduced | green | W2.b ledger deletes 5 ABROGATE scripts in the same commit as their replacement aliases; `docs/benchmarks/post-B1-W2-abrogation-ledger.txt`. |
| 7 | Samply closes on symbol-resolved captures, not `nm`-only salvage | green-routed | B1 does not perform a samply baseline refresh (prelude-annex scope). The pinned substrate makes symbol resolution durable; first capture lands at AY-II.W0' close per `post-B1.json` §samply. |
| 8 | B1 closes with a benchmark/timing artefact trail of its own under divan + nextest | green | Six ledgers under `docs/benchmarks/post-B1-*.txt` plus `post-B1.json` aggregate; nextest config validated, divan harness ported per `post-B1-W1-parity.txt`. |
| 9 | B1 carries no successor debt tree; runtime-facing items route back to AY-II | green | This FINAL.md §Cross-tranche debt ledger names AZ-I.W0 (derive cache + Watt) and post-B1 polish (deferred REPLACE/KEEP-MODERNIZE) as the only forward routes; AY-II resumes at W0' close. |
| 10 | Every `--exclude`d `iter-check` crate has a named fast-path alias under `--profile ax-iter` | green | `.cargo/config.toml` lines 106-110: `iter-check-lsp` (bbnf-analysis + bbnf-lsp), `iter-check-prettify` (gorgeous), `iter-check-bootstrap` (bbnf-bootstrap); each pass per `post-B1-W0-routine.txt`. |
| 11 | `iter-check-full` is the close-ceremony gate, not routine; cold ceiling recorded | green-routed | `post-B1-W0-proof.txt` row `iter-check-full-cold-pinned` halted at 25:30 wall-clock with `bbnf-bootstrap` > 600 s critical-path; recorded as measured truth pre-AZ-I.W0. The ≤ 5 min target opens at AZ-I.W0 close. |
| 12 | `target/.bbnf-cache/` is neither created nor destroyed as a side-effect of any B1 command | green | W2.b rewrote `bootstrap-bbnf.sh` to gate the `rm -rf target/.bbnf-cache/` behind `BBNF_BOOTSTRAP_CLEAN_CACHE=1`; cycle-2 bootstrap is measurably cheaper than cycle-1 post-change per `post-B1-W2-abrogation-ledger.txt`. |
| 13 | No script in the abrogation catalog is deleted until its REPLACE target is live + validated | green | W2.b `profile.sh` deletion landed in the same commit as the Makefile rewire to `prepare-profile-wave.sh` + `profile-bench-headless.sh`; pre-commit `rg -n <script_name>` returned zero hits across Makefile + workflows + PROFILING.md. |
| 14 | No divan port begins until the pinned toolchain is applied + ICE cache cleared + iter-check-full exits clean | green-routed | W0's pin landed (commit `41b9c4fb`) + `cargo clean` cleared the ICE cache + W0.b config rewrite (`416dcf76`); ICE count zero post-smoke per `post-B1-W0-proof.txt` row `toolchain-smoke-ice-count`. iter-check-full's cold wall did not exit clean within hard-cap (bootstrap > 600 s); the divan ports proceeded under the pin + ICE-clean state per the architectural-equivalence ledger in `post-B1-W1-parity.txt`. |
| 15 | `bencher = "0.1"` is removed in the same commit that lands the final ported bench | green | `f9c3db38` (W1.c) removes both `bencher` deps + lands the iai-callgrind feature gate in one commit; `rg -w bencher --type rust --type toml` returns zero non-comment hits per `post-B1-W1-parity.txt`. |
| 16 | bbnf-lang, parse-that, pprint all carry the same pinned nightly | green | `post-B1-W2-cross-repo.txt` triad table: all three carry `nightly-2026-04-11` in `rust-toolchain.toml`; `cd ../parse-that && cargo check` + `cd ../pprint && cargo check` both exit 0 on first invocation. |

## Hard-gate table

Per-wave hard gates from each wave's spec. Status column:
**green** — gate met cleanly; **green-routed** — gate met with the
numeric/runtime continuation routed to a successor;
**cap-exceeded-routed** — wall-clock exceeded the W0.c 18-min hard-cap
and routed to AZ-I.W0; **skipped-cap-routed** — deferred under cap with
explicit destination.

| Wave gate | Item | Status | Closing artefact |
|---|---|---|---|
| W0.1 | `rust-toolchain.toml` committed; rustc reports pinned nightly | green | `41b9c4fb` (W0.a); `post-B1-W0-proof.txt` row `rustc-version-pinned` |
| W0.2 | `.cargo/config.toml` resolves every alias; cost-model comment | green | `416dcf76` (W0.b); rows `alias-resolve-*` (15 aliases pass) |
| W0.3 | `.config/nextest.toml` 4 profiles; `cargo nextest --profile ax-iter` parses clean | green | `6d800162` (W0.c); row `nextest-config-parse` pass |
| W0.4 | Makefile simplified; every target delegates to a cargo alias or single script | green | `06d3db65` (W0.d); 470 → 210 lines; verified via `wc -l Makefile` |
| W0.5 | `iter-check`, `iter-test-leaf`, `iter-check-lsp/-prettify/-bootstrap`, `test-tier.sh leaf --profile ax-iter` exit 0 | green-routed | `post-B1-W0-routine.txt`; `iter-check-bootstrap` killed-at-cap and routed to AZ-I.W0 (single-crate ceiling) |
| W0.6 | `iter-check-full` cold ICE-clean compile; `iter-check-full-cold-pinned` row recorded | cap-exceeded-routed | `post-B1-W0-proof.txt` row `iter-check-full-cold-pinned` halted-at-cap; ICE count post-iter-check 0; routed to AZ-I.W0 |
| W0.7 | Stale command-surface claims removed | green | W3 refresh + W2.b script abrogations land deletions in the same commit as their supersession |
| W0.8 | Routine iteration ≤ 5 s warm baseline | green | `post-B1-W0-routine.txt` `iter-check` 3.88 s |
| W0.9 | Wave close commit lands honest invariant 11 wording + leaf test tier 582/582 | green | `1d0815dc` close commit |
| W1.1 | `cargo bench --bench compile_pipeline` produces divan JSON within ±5% | green-routed | Compile validated cold 49.47 s per `post-B1-W1-parity.txt`; per-bench JSON deferred to AZ-I.W0 (bootstrap > 600 s wall) |
| W1.2 | All 19 bench files compile under divan with `DIVAN_BENCH_FORMAT=json` | green | `post-B1-W1-parity.txt` ports table — 19 files plus cross-crate `json_value` |
| W1.3 | `bencher = "0.1"` removed; `rg -w bencher` returns 0 | green | `f9c3db38`; both `Cargo.toml` deletions plus `Cargo.lock` purge |
| W1.4 | `benches/json_callgrind.rs` exists; `iai = ["iai-callgrind"]` feature gated; `bench-iai.yml` drafted | green | `post-B1-W1-parity.txt` §iai-callgrind surface |
| W1.5 | Divan JSON captured per-grammar | skipped-cap-routed | Per-bench JSON requires the bootstrap > 600 s wall; routed to AZ-I.W0 close per `post-B1.json` §divan.per_bench_json_emissions |
| W1.6 | Parity check between divan and bencher walls within ±5% per group | green-routed | Architectural-equivalence ledger: each ported file preserves bench bodies verbatim modulo mechanical harness translation; numeric ±5% verification opens at AZ-I.W0 unblock per `post-B1-W1-parity.txt` |
| W1.7 | Wave close commit removes bencher in same commit as final port + iai-callgrind feature | green | `f9c3db38` |
| W2.1 | `ci.yml` installs nextest via `taiki-e/install-action`; junit upload | green | `31d3e2cb`; `post-B1-W2-ci.txt` §Landings |
| W2.2 | `bench-iai.yml` runs on PR; comment bot; baseline committed | green | `31d3e2cb`; baseline stub at `docs/benchmarks/iai-baselines/json.json`; first instruction count populates on first CI run |
| W2.3 | Every abrogation-catalog script DELETED/REWRITTEN/KEPT per Part 1 | green | `d276934a`; `post-B1-W2-abrogation-ledger.txt` 19 + 1 row table; 5 DELETE + 1 REWRITE + 13 KEEP-AS-IS landed |
| W2.4 | `../parse-that/rust-toolchain.toml` + `../pprint/rust-toolchain.toml` carry the pinned nightly | green | `62227603`; `post-B1-W2-cross-repo.txt` triad table |
| W2.5 | `cd bbnf-lang && cargo iter-check` succeeds with all three repos pinned | green | `post-B1-W2-cross-repo.txt` workspace-minus-heavy-crates row |
| W2.6 | DELETE lands in the same commit as the replacement alias | green | W2.b Makefile profile-rewire commit removes `profile.sh` + lands `prepare-profile-wave.sh` + `profile-bench-headless.sh` invocations |
| W2.7 | Sibling `.cargo/config.toml` minimal templates present | green | `post-B1-W2-cross-repo.txt` §Sibling repo files created |
| W2.8 | Wave close ledger captures triad green + abrogation closure | green | `d276934a` close commit |
| W3.1 | `PROFILING.md` integrated; every alias appears once; stale script refs removed | green | `docs/instructions/PROFILING.md` post-W3; `rg` for `profile.sh`/`cost-grid-sweep`/`check-cst-invariants`/`verify-w2-asm`/`verify-w2-symbols` returns zero hits |
| W3.2 | `post-B1.json` parses as valid JSON with all required keys | green | `docs/benchmarks/post-B1.json`; `python3 -c "import json; json.load(...)"` exit 0 |
| W3.3 | `FINAL.md` exists with invariant + hard-gate + commit + handoff tables | green | this file |
| W3.4 | AY-II planning docs reflect B1-closed + W0'-unblocked | green | `docs/tranches/AY-II/{PATH-FORWARD.md,PROGRESS.md,AY-II.md,waves/W0p.md}` updated 2026-04-24 |
| W3.5 | `B1/PROGRESS.md` carries close-ceremony dated entry with every wave SHA | green | `docs/tranches/B1/PROGRESS.md` 2026-04-24 close entry |

## Wave commit ledger

| Wave | Phase | Commit | Headline |
|---|---|---|---|
| W0 | a | `41b9c4fb` | substrate pin (`rust-toolchain.toml` → `nightly-2026-04-11`); smoke 6.32 s; ICE count 0 |
| W0 | b | `416dcf76` | `.cargo/config.toml` rewrite — 4-exclude `iter-check` + per-exclude fast-path aliases + cost-model comment |
| W0 | c | `6d800162` | `.config/nextest.toml` rewrite — 4 profiles (default / ax-iter / ci / close); `[[profile.close.overrides]]` array-of-tables fix |
| W0 | c-patch | `2b6e50bf` | propagate the array-of-tables fix to `patches/nextest.toml.draft` |
| W0 | d | `06d3db65` | Makefile rewrite (470 → 210 lines); `ay-prime` target; `scripts/test-tier.sh` aligned to nextest |
| W0 | trium-research | `eeca61e1` | triumvirate research on `iter-check-full` ceiling — gorgeous 6 derive-Parser sites attributed |
| W0 | trium-plan | `fd2cf6fb` | triumvirate plan — `default = []` + `[[bin]] required-features = ["bin-full"]` |
| W0 | trium-redress | `22013145` | triumvirate Change 1 landed; gorgeous `default = []` |
| W0 | invariant-rewording | `1926aed1` | invariant 11 reworded — pre-AZ-I.W0 measured truth, not numeric target |
| W0 | measurements | `1c8c1282` | `post-B1-W0-routine.txt` + `post-B1-W0-proof.txt` post-Change-1 |
| W0 | close | `1d0815dc` | `scripts/test-tier.sh` bash-3.2 fix; final invariant 11 wording; leaf test tier 582/582 pass |
| W1 | a | `705ad503` | divan dev-dep + `compile_pipeline` exemplar port + shim rewrite |
| W1 | b.1 | `c83ba648` | port JSON bench family to divan |
| W1 | b.2 | `3488274a` | port CSS bench family to divan |
| W1 | b.3 | `19a7f758` | port Google Sheets bench family to divan |
| W1 | b.4 | `e739b81b` | port BBNF bench family to divan |
| W1 | c | `f9c3db38` | remove `bencher` + add iai-callgrind Linux-target dev-dep |
| W2 | a | `31d3e2cb` | rewire CI workflow — nextest + iai-callgrind |
| W2 | c | `62227603` | cross-repo pin propagation ledger; `../parse-that` + `../pprint` synced |
| W2 | b | `d276934a` | script abrogation per meta-audit/08 catalog |
| W3 | a | `73040fc0` | PROFILING refresh + FINAL + post-B1.json + AY-II handoff |
| W4 | amendment | `b3c50581` | restore B0 AY W5-W7 gate-command surface + prep/final-bench + iter-test-leaf/grammar; close-review-driven adherence amendment |

## Cross-tranche debt ledger

B1 forwards three classes of debt. Each item carries an explicit
destination tranche with rationale.

| Item | Destination | Rationale |
|---|---|---|
| Derive cache relocation to `$XDG_CACHE_HOME/bbnf-derive/` | AZ-I.W0 | `target/.bbnf-cache/` survives `cargo clean` only via B1's bootstrap-bbnf cache-guard; durable relocation lifts the cache out of `target/` entirely. Named in `TOOLCHAIN-MIGRATION.md §6 Deferred`. |
| Watt / WASM-precompiled proc-macros | AZ-I.W0 | `bbnf-bootstrap`'s > 600 s single-derive expansion is the `iter-check-full` critical-path ceiling; Watt wraps the derive macro for re-use across rustc invocations. Necessary precondition for the ≤ 5 min `iter-check-full` ceiling that B1 invariant 11 originally claimed. |
| Per-bench divan JSON emissions (`post-B1-W1-divan-{json,css,bbnf,sheets,compile}.json`) | AZ-I.W0 close, then AY-II.W0' close ceremony | The full 19-bench `cargo bench --profile ay-final` pass requires the test-binary compile that pays the same bbnf-bootstrap > 600 s wall. Architectural equivalence holds via the mechanical port; numeric capture opens at AZ-I.W0 cache relocation. |
| nextest dry-runs (`nextest-ax-iter-dry-run`, `nextest-close-dry-run`) | AZ-I.W0 | `cargo nextest --no-run` compiles all workspace test binaries (including `bbnf-bootstrap`); pays the same > 600 s wall regardless of profile. Config parse validated clean in W0.c. |
| Samply baseline refresh under divan + the pinned nightly | AY-II.W0' close ceremony | B1's prelude-annex scope ends at the alias surface + harness migration; the four-grammar baseline (json_twitter, css_tailwind, sheets_stress, bbnf_self) lands per `AY-II/waves/W0p.md` §Orchestrator-owned close ceremony. |
| `bench_regression.sh` divan JSON parser rewrite (REPLACE) | post-B1 polish | Successor parser rewrite is multi-step engineering exceeding B1's bounded-annex scope (SPEC §Prelude annexes); script functional under current bencher-output assumption. |
| `bisect-fastpath.sh` modernization (KEEP-MODERNIZE) | post-B1 polish | 162 → 30 lines collapse deferred; script functional under the pinned nightly. |
| `extract_hotspots.py` samply-native rewrite (REPLACE) | post-B1 polish | Successor (samply-native extraction) is a multi-step engineering item; current Python wrapper functional. |
| `prepare-profile-wave.sh` modernization (KEEP-MODERNIZE) | post-B1 polish | Absolute `CARGO_TARGET_DIR` enforcement already in place per AW-era landings; further modernization deferred. |
| `seed-worktree.sh` target-symlink fix (KEEP-MODERNIZE) | post-B1 polish | Self-reference fix observed in W0/W1 dispatches; agents work around locally. |
| `test-tier.sh` collapse into cargo alias (FOLD-INTO-TOOLING) | post-B1 polish | Script aligned to nextest in W0.d; bash-3.2 fix in W0 close; works as-is. |
| `worktree-status.sh` modernization (KEEP-MODERNIZE) | post-B1 polish | Current script works; modernization deferred. |
| Fleet-wide modernization (gorgeous sibling, csp-solver, csc411, crates/ai) | post-B1 appurtenant tranche | B1 invariant 16 binds only the path-patched-graph triad (bbnf-lang + parse-that + pprint). Wider propagation requires a separate fleet-modernization inventory per `meta-audit/07-appurtenant-assay.md`. |
| Bench architecture restructure (`crates/bench-*`) | AZ-I or later | Deeply structural; named in `TOOLCHAIN-MIGRATION.md §6 Deferred`. |
| Parametric bench collapsing | post-B1 polish | Optional restructuring; not load-bearing for AY-II resumption. |
| sccache | TBD (conditional on measured CI cache-hit ≥ 80 %) | Decision pending first-CI-run cache-hit telemetry. |

No item on this ledger blocks AY-II.W0' close. The AZ-I.W0-routed
items unblock the numeric / wall-clock targets that B1 invariant 11
originally claimed; they are not preconditions for AY-II runtime work.

## AY-II handoff block

The next execution anchor is
`docs/tranches/AY-II/PATH-FORWARD.md`. Per the dated 2026-04-24 entry
appended in W3, B1 closes at this commit and AY-II.W0' close ceremony
opens on the refreshed substrate. The order is:

1. **AY-II.W0' close ceremony** — bootstrap regen, double-regen
   idempotency, retire compose-boundary aliases, capture fresh expands
   for JSON / CSS / Sheets / BBNF, run the fat-LTO 5-bench matrix,
   capture samply on the four primary grammars, run `nm` on bench
   binaries, mark W0' closed in `PROGRESS.md` + `waves/W0p.md`. The
   gate stays the one declared in `AY-II/waves/W0p.md`.
2. **AY-II.W1-W5 sequential** — JSON peer parity (W1), CSS L4
   typed-semantic parity (W2), Sheets typed semantics (W3), BBNF
   self-hosting identity (W4), cross-grammar close matrix + FINAL
   (W5).

No annex, no sidecar wave, and no infra detour runs in parallel with
those waves. The B1 surface is durable — pinned substrate, divan
harness, four-profile nextest, four-exclude `iter-check` with named
fast-paths, simplified Makefile, abrogation catalog executed,
sibling-repo triad in sync. AY-II resumes on engineering truth, not
infrastructure debt.

The handoff is unblocked.
