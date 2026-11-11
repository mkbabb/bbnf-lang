# SK-V27 LW2 — the work order (topology, toolchain, ABI freezes, the feasibility seam)

**Status:** ORDERED 2026-11-06. Executable as one workflow: 16 seats, 6 phases in series, at most 4 seats live at once.
**Authority:** PLAN.md §5 LW2, under RULINGS.md (`0c0a8b2a0`): X-1..X-5 and X-7 apply in this wave; X-6, X-8, X-9 and X-10 bind where cited. LEDGER.md addenda through the LG-1 row (bbnf-lang `sk-v27-lw1-gate` `b133466de`) and the TOPO rows on `sk-v27-topo` `2cff53f01` are the entry facts.
**This file is not edited.** A change is an addendum row in LEDGER.md that names the section it amends.

## 0. Entry facts (re-read at Phase 0, never assumed)

| Fact | Value at ordering | Consequence for LW2 |
|---|---|---|
| LW1 TOPO lane | TOPO-0..4 GREEN, TOPO-5 merge made (`077d9338d`, draft PR #9), all on `sk-v27-topo` `2cff53f01`, **not yet in the gate branch** | Phase 0 integrates it by ordinary merge. |
| wf_d837f96c | CLOSED by record (value.js `4ad2ca653`, `ownership-W0.json`) | The §1 ownership holds are released. No `contest/ci` route is needed. value.js paths stay under value.js's own app-work holds. |
| Stale pre-commit hook | Removed by LW1.COMP-4 (copy in `~/.dev-logs/bbnf-lockstep/custody/hooks/`); R-d discharged | No `--no-verify` and no `core.hooksPath` override in any LW2 seat. |
| bbnf-lang CI (PR #8) | clippy 0, hermetic, iai and extension GREEN; nextest 48 failures = 47 gitignored-`data/` corpus tests + `substrate_audit` (32 zero-caller `pub` items) | Cured here by LW2.TOPO-6 and LW1.COMP-1b. |
| parse-that #3, pprint #2 CI | RED at dependency resolution: the git edges into bbnf-lang clone its private `docs/precepts` submodule | Cured here by topology (LW2.TOPO-1), so R-a stops blocking. parse-that also carries `test_nested_media` (debug stack overflow) → LW2.PERF-3. |
| Repo cycle | parse-that `rust/regex` (`bbnf-regex`) takes egraph, egraph-derive and csp-solver from bbnf-lang; pprint's optional `ser` feature takes `bbnf-ser` from bbnf-lang | LW2.TOPO-1 removes both reverse edges (X-5). |
| Toolchain | bbnf-lang `rust-toolchain.toml` = `nightly-2026-04-11` plus cranelift; committed `rustc-wrapper = "sccache"` with no fallback; parse-that commits `-Zthreads`/`-Zshare-generics`; `smallbox` `features=["nightly"]` reaches bbnf-lang through parse_that (LW1.PERF-2: rc 101 on both repos) | LW2.TOPO-2. Stable pin is `rust 1.99.1` from `bench/W0-BENCH-PINS.tsv` v2. |
| Records | Every LW1 record is NOISY (load 14–27). User-interactive QoS cannot be reached from the seat's ancestry (LW1.CON-1 host fact) | Floors can be frozen only from Phase 3's window. See §5 for the QoS reading. |

**Owner rulings at entry:**
- **R-c (corpus artifact form: release asset or LFS)** is needed by Phase 0. If it is not in RULINGS.md when Phase 0 starts, LW2.TOPO-6 does its form-independent half only (manifest, resolver, verifier, tamper test), the transport row carries as RULING, bbnf-lang master CI stays RED, and the Phase 5 master merges hold.
- **R-a** no longer blocks once TOPO-1 lands, because parse-that and pprint then take no edge into bbnf-lang. The order's default: the topology is the only cure, and `docs/precepts` stays as it is. The adjudicator asks R-a again only if TOPO-1's falsifier fails.
- **R-b** (the `->`, `@ws` and `@token` dialect) is needed by LW3, not by LW2. LG-b stays a baseline in this wave.

## 1. Branches, worktrees and seat law

- **Integration branch `sk-v27-lw2` in each repo**, cut in Phase 0:
  - bbnf-lang: off `origin/sk-v27-lw1-gate`, with ordinary merges of `origin/sk-v27-topo`, `origin/sk-v27` (RULINGS.md and this order) and `origin/lockstep/merge-skv25` (the history merge, so the LW2 PR carries `af15f63e0` into master; PR #9 then closes as included). The only expected conflicts are `.gitignore`, resolved as the union TOPO-5 recorded, and LEDGER.md, resolved by union.
  - parse-that: off `origin/sk-v27` (`0f6c598`).
  - pprint: off `origin/sk-v27-pprint` (`cf83853`).
- **Seat branches:** `lw2/<seat>` off the integration tip at seat start. Worktrees go in `~/Programming/{bbnf-lang,parse-that,pprint}-wt/lw2-<seat>`. A seat merges its branch into `sk-v27-lw2` with `--no-ff` at seat end. A rejected push is answered by fetch plus another ordinary merge.
- **Conflict rules:**
  - LEDGER.md is append-only, so a conflict is resolved as the union.
  - A `Cargo.lock` conflict is resolved by taking either side and running `cargo update --workspace`, which moves no dependency version. The seat records the before and after `cargo tree --locked` diff, and that diff must be empty outside the workspace's own packages.
- **Git law:** commit by pathspec; never stash, reset, rebase or force; primary checkouts are never switched. A cross-repo crate move is a single commit in the receiving repo with a `Moved-from: mkbabb/<repo>@<sha> <path>` trailer, plus a deletion commit in the source repo that cites the receiving SHA.
- **Push order for git+rev pins:** pprint, then parse-that, then bbnf-lang. A downstream pin names only a SHA that `git ls-remote` already shows.
- **Unit law:**
  - Every unit reads its falsifier RED ×2 for the stated reason, then GREEN ×2, at SHAs that are in `heads-LW2.json` or are pushed by the seat itself.
  - Every unit appends one ledger addendum row.
  - Evidence goes in `restart/skinny/tranches/sk-v27/evidence/LW2.<LANE>/`, and logs go in `~/.dev-logs/bbnf-lockstep/lw2/<seat>/`.
  - No lint threshold, allow-list, timeout or test assertion is widened to make a row GREEN. The only exception is a sanction entry under LW1.COMP-1b's rule.
- **Machine law:**
  - Every process goes through `~/.claude/guard/guarded`, including one-shot probes, debuggers, stack measurements and language-server drivers (LW1's LG-f breach). Quick commands use `--light`.
  - Cargo builds and tests are heavy runs, and a seat runs one heavy run at a time.
  - No command name contains `probe`, because the guard classes such a command as a browser run.
  - Fuzzing runs locally only as smoke, in shards of at most 80 min. Corpora go in `~/.dev-logs/bbnf-lockstep/fuzz/<target>`. Admission needs 10⁵ inputs per target.
  - Nothing durable goes in `/tmp`.
- **Model law:** every seat is spawned with an explicit model, Opus 5.5 (`opus`). The recorded model is checked against the declaration in the seat's receipt.
- **Generality law:** parse-that carries no BBNF vocabulary. Every crate, feature, env var and doc that moves into parse-that is checked with `git grep -niE 'bbnf' -- <moved paths>` = 0.

## 2. Phases and seats

| Phase | Seats (live at once) | Opens when |
|---|---|---|
| **0 — integrate, custody, census** | S1 CUSTODY, S2 CENSUS | dispatch |
| **1 — repo DAG** | S3 DAG (alone: it rewrites every manifest in three repos) | S1 and S2 merged into `sk-v27-lw2` |
| **2a — toolchain and first lanes** | S4 TOOLCHAIN, S5 COMP, S8 PP, S9 CON-SPEC | S3 merged and pushed in all three repos |
| **2b — kernels, regex, consumers** | S6 PERF, S7 RX, S10 USE (S5 COMP continues) | S4 merged |
| **3 — the bench window** | S11 BENCH (alone; the orchestrator dispatches nothing else, per X-7) | every Phase 2 seat merged; `bench/PREREG.md` hash committed |
| **4 — seams** | S12 CRITIC-A, S13 CRITIC-B, then S14 ADJUDICATOR | S11 records committed |
| **ruling checkpoint** | none: the workflow returns its ruling asks and is resumed after the owner or the owner's delegate appends them to RULINGS.md | S14's asks are filed |
| **5 — freeze and gate** | S15 FREEZE, then S16 GATE | resumed with the rulings present |

At most 4 seats are live at any time, and the machine-wide guard queues heavy runs beyond its own slots. A seat whose unit is blocked by an absent ruling finishes the ruling-independent part, files the row as RULING, and ends. It never waits in a loop.

## 3. Units by lane: work, falsifier and gate

### TOPO

| Unit | Seat | Work | Falsifier / gate |
|---|---|---|---|
| **LW2.TOPO-0** integrate | S1 | Cut the three `sk-v27-lw2` branches (§1) and take `heads-LW2.json` with the LW1.TOPO-1 checker. | Every SHA cited in this order and in LEDGER rows from the gate onward resolves against `heads-LW2.json` (0 unresolved). `merge-base --is-ancestor` of `sk-v27-topo`, `sk-v27-lw1-gate`, `sk-v27` and `lockstep/merge-skv25` into bbnf-lang `sk-v27-lw2` is true ×4. |
| **LW2.TOPO-6** corpus custody | S1 | One versioned corpus artifact holds every gitignored `data/` corpus that a test, bench or example reads in bbnf-lang and parse-that. It carries a sha256 manifest. `cargo xtask corpus fetch` and `cargo xtask corpus verify` provide it locally and in CI. Every reader resolves the corpus through one resolver function, with no literal `data/` path in test code. The transport is whatever R-c rules. Exactly one `twitter.json` survives: the file whose sha256 equals upstream simdjson `jsonexamples/twitter.json` at the CON-2-pinned commit. The other file is deleted from the tracked trees, and the skinny/root NOT-COMPARABLE row closes. The CSS part stays as it is until LW3.USE-1 replaces it with X-8's released value.js corpus. | RED ×2: a clean checkout fails `corpus verify` (absent), and a one-byte tamper mutant is refused. GREEN ×2: bbnf-lang CI nextest failures go from 48 to ≤ 1 (only `substrate_audit` may remain until S2's fates land); parse-that `css_parse_test` reads `data/css` through the resolver; the 7 W3 corpus symlinks are gone, leaving the W3 guard RED only for the R-b grammar copies (LW3.COMP-1). Also `git grep -nE '"(\.\./)*data/' -- '*/tests/*' '*/benches/*' '*/examples/*'` = 0 outside the resolver, and the two workspaces hold exactly 1 distinct `twitter.json` hash. |
| **LW2.TOPO-1** repo DAG (X-5, X-2) | S3 | **(a)** egraph, egraph-derive and csp-solver move into parse-that `rust/` as standalone general workspace members. bbnf-lang deletes `crates/{egraph,egraph-derive,csp-solver}` and takes them as git+rev edges on parse-that. **(b)** `bbnf-regex` is renamed `parse-that-regex` (lib `parse_that_regex`), with its description generalized. bbnf-lang names its thin adapter crate `bbnf-regex-facts` (an empty facade is not created: the adapter is born in LW4.RX-2, when `RegexInfo` converges). **(c)** pprint's `ser` feature edge into bbnf-lang goes away: `rust/src/serializer.rs`, the bridge from bbnf-ser to pprint, moves into bbnf-lang `crates/ser` behind a `pprint` feature, because a bridge lives with the higher crate in the DAG. PP-07's `pprint-ser` stays at LW4. **(d)** `[patch.crates-io]` leaves every committed manifest and config. Edges are git+rev until LW6. Each edge also carries the `version` it will take on crates.io, so the LW6 cascade edits nothing but sources. | New `scripts/check-repo-dag.sh` in each repo reads `cargo metadata --locked`, maps every package to its source repo, and asserts that the edges are a subset of {parse-that → pprint, bbnf-lang → parse-that, bbnf-lang → pprint}. It reads RED ×2 at the integration base (two reverse edges) and GREEN ×2 after. `git grep -n '^\[patch' -- '*.toml'` = 0 in each repo. `check-hermetic-build.sh` is GREEN in each repo. **Gate:** parse-that and pprint CI pass dependency resolution credential-free on the LW2 PRs, which cures LG-a independent of R-a. The generality grep is 0 over the moved crates. The baseline count of `RegexInfo` definitions outside `parse-that-regex` is recorded (→ 0 at LW4.RX-2). |
| **LW2.TOPO-2** de-nightly | S4 | The committed `.cargo/config.toml` in each repo keeps only portable profiles and aliases, plus an explicit `+nightly` alias family for cranelift and `-Z` work. `rustc-wrapper` moves to CI env (`RUSTC_WRAPPER`, with the sccache action) and to the developer's `~/.cargo/config.toml`; the config comment states how. bbnf-lang's `rust-toolchain.toml` pins stable `1.99.1`, and nightly becomes opt-in. parse_that's `smallbox` nightly feature becomes an opt-in `nightly` feature that is off by default; the cure is in parse-that's manifest, not a downstream override. parse-that's `-Zthreads`/`-Zshare-generics` go. Bench profiles are unchanged, and the build plane of every bench arm is recorded (competition U1b). | The LW1.PERF-2 command is re-run from clean clones: `cargo +stable build --release` and `cargo +stable test --workspace --locked`. It reads rc 101 ×2 at base and rc 0 ×2 in all three repos. `git grep -nE 'rustc-wrapper\|-Z' -- .cargo/config.toml` matches only inside the `+nightly` alias family. |
| **LW2.TOPO-3** CI matrices | S4 | bbnf-lang adds an `ubuntu-latest` arm and parse-that adds `macos-14`. Each repo gets a `cargo +stable` job and an aggregate `ci-ok` job that needs every arm. An Intel SDE job runs AVX-512 (X-4). Clippy `-D warnings` runs on both arms. The arms exist for S6's tier reading (PERF-2). | Each repo's PR CI shows the arm set (macos-14, ubuntu-latest, stable, SDE), with `ci-ok` GREEN on the LW2 PRs. If SDE cannot execute the AVX-512 kernel, S6 deletes the kernel (X-4); no job is skipped to pass. |
| **LW2.TOPO-4** publish dry-run | S16 | `cargo publish` in DAG order into a throwaway local alternative registry at `~/.dev-logs/bbnf-lockstep/registry/`, configured on the command line and never committed: egraph, egraph-derive, csp-solver → pprint, pprint_derive → parse_that, parse-that-regex, parse-that-simd → the bbnf-lang crates. S3 reads crates.io name availability for every new name at TOPO-1, so the API-freeze seam sees it before the dry-run. | Every crate packages and publishes to the local registry, and a fresh crate that depends on `bbnf` from that registry builds with `cargo +stable build` rc 0 ×2. A crates.io name that is taken (for example `egraph` or `csp-solver`) becomes a RULING row for the API-freeze seam. It is not renamed silently. |
| **LW1.TOPO-5** master merges (carried) | S16 | After CI is GREEN on each LW2 PR, merge in DAG order: pprint, then parse-that, then bbnf-lang (which carries `077d9338d`). Use ordinary merges and close PRs #8, #9, parse-that #3 and pprint #2 as included. | `git merge-base --is-ancestor af15f63e0 origin/master` reads rc 1 ×2 before the bbnf-lang merge and rc 0 ×2 after. Each master's CI is GREEN on the merge commit. |

### COMP

| Unit | Seat | Work | Falsifier / gate |
|---|---|---|---|
| **LW1.COMP-1b** substrate fates (carried) | S2 | Each of the 32 zero-caller `pub` items gets a fate in `CRATE-CENSUS.tsv`. The rules follow X-1 (the root pipeline is a quarry that can only delete) and X-5. **core 23 / ir 3:** DELETE, unless the census names an LW2–LW4 unit that consumes the item. Then the fate is ROUTE: one `SANCTIONED_SUBSTRATES` entry whose reason cites that unit id, removed when the unit lands. **csp-solver 3:** these leave the audit's scope with TOPO-1. In parse-that they are general library API, each with a doctest caller. **simd-scan 3:** these leave with PERF-1. `compact_stripe_pext` is DELETED (PERF marked it dead). | `substrate_audit` RED ×2 at 32, then GREEN ×2 at `sk-v27-lw2` once S3 and S6 have merged; the GATE reads it. The sanction list grows by no more than the number of ROUTE rows, and every new reason cites a unit id. |
| **LW2.COMP-2** CSP Unsat cure | S5 | `let _ = csp.propagate()` → a typed result at all 6 sites. Unsat becomes a diagnostic that names the conflicting constraint pair. `InlineDomain::join` becomes a lattice join in which a Decided/Decided conflict gives ⊤. | Pinned form `git grep -nE '^\s*let _ = csp\.propagate\(\);' -- crates` goes from 6 to 0. The LW1.COMP-3 contradictory-grammar born-RED test goes from RED (code emitted) to GREEN (emission refused with the Unsat diagnostic). The `LatticeDomain` law test (commutative, associative, idempotent, monotone; ≥ 10⁴ property cases) is GREEN on `InlineDomain`. `cargo xtask regen --check` rc 0 (in-tree grammars unchanged). |
| **LW2.COMP-3** passes home | S5 | The inline plan and its CSP consumption move into `crates/ir` passes. The move preserves behaviour. | `cargo tree -i csp-solver` inside bbnf-lang shows `bbnf-ir` as the only direct dependent. `regen --check` rc 0 ×2. The nextest set is unchanged apart from the COMP-2 flips. |
| **LW2.COMP-4** CB-5.5 slice | S5 | A minimal plan-driven JSON emitter slice (Program → Facts → Plan → emit) for `grammar/json/json.bbnf`, end to end, as the trials' harness. | RED ×2: the slice is absent. GREEN ×2: on JSONTestSuite at its CON-2 pin, the slice's accept/reject equals root `backend/rust` on 100 % of `y_` and `n_` files, its tree equals it on every `y_` file, and `i_` results are recorded. |
| **LW2.COMP-1** root trials, pre-registered | S5 → S11 | Two trials on the slice: e-graph (P5) on against off, and CSP against greedy. Each trial has compile-artifact gates (emit time, generated bytes, `regen` bytes) and CB-B0 microbench cells. S5 writes the cells, the deadlines (72 h per trial, re-queued once, per X-7) and the verdict rule into `bench/PREREG.md` before Phase 3. The rule: a win needs a Holm-adjusted CI that clears the 3 % minimum effect; anything else is UNDECIDED and goes to the simpler organ (e-graph off, greedy). S11 runs the cells and applies the rule mechanically. | The PREREG hash is committed before S11 starts, and S11 refuses any cell that is not registered (a mutant run is refused ×2). Each trial ends WIN, LOSS or UNDECIDED with its record ids. The loser is tagged `trial/lw2-<organ>-loser`. Porting the winner is LW3.COMP-1. |

### PERF

| Unit | Seat | Work | Falsifier / gate |
|---|---|---|---|
| **LW2.PERF-1** form `parse-that-simd` (X-3, X-4) | S6 | Merge bbnf-lang `crates/simd-scan`, `skinny/crates/simd-scan` and `skinny/crates/bbnf-simd` into parse-that `rust/simd` (crate `parse-that-simd`). A kernel duplicated across the three sources survives once, by census row. The crate has:<br>• one `OnceLock` tier table over NEON, AVX2, AVX-512 and wasm32-simd128, with one scalar oracle per kernel;<br>• the override `PARSE_THAT_SCAN_TIER` and a feature `force-scalar` (no BBNF vocabulary);<br>• wasm32 ported onto `stripe.rs`, and runtime CLMUL detection in place of the `cfg`;<br>• a differential fuzz target per kernel, the guard-page test (a trailing `PROT_NONE` page), and Miri on the scalar and dispatch code.<br>`parse_that::scan` is its public face. The three source crates are deleted from bbnf-lang. | `git grep -l 'core::arch'` across both repos lists only files under parse-that `rust/simd/`. The generality grep over `rust/simd` is 0. The LW1 born-RED R1/R2/R2x/R3 stay GREEN ×2. The guard-page test passes ×2. Miri rc 0. Each fuzz target reaches 10⁵ inputs with 0 oracle divergences. |
| **LW2.PERF-2** tier selected on every arm | S6 | A `dispatch_tier_selected` test asserts the expected tier for the arm it runs on. | CI logs show `neon` on macos-14, `avx2` on ubuntu-latest (real silicon: this closes LW1.PERF-1's carried row), `avx512` under SDE, and `simd128` on the wasm32 arm. A `force-scalar` mutant fails the assertion on every arm ×2. If SDE cannot execute the AVX-512 kernel, the kernel is deleted (X-4). |
| **LW2.PERF-3** `test_nested_media` stack | S6 | Find the root cause of the debug stack growth of about 1.36 MiB per at-rule nesting level. The open suspect is the first call into each level's fresh `lazy()` tree. Raising `RUST_MIN_STACK`, using a thread builder or skipping the test in debug is not a cure. | RED ×2: debug `test_nested_media` SIGABRT. GREEN ×2: it passes at the default test-thread stack. A depth sweep (1, 2, 4, 8, 16) shows per-level stack growth that does not depend on depth; the number is recorded. |

### RX

| Unit | Seat | Work | Falsifier / gate |
|---|---|---|---|
| **LW2.RX-1** OR-7 spike | S7 | Route A (regex-automata at its CON-2 pin, used as the determinizer) against Route B (in-house). The leaves are the 115 distinct value.js leaves plus the sk-v24 56. Measure fidelity against the LW1.RX-1 V8/JSC oracle, compile time and state counts. Emitted-code speed is queued as Phase 3 cells. The 38 lookaround leaves are reported as their own tier, never pooled (C1-9 ruling). The spikes live on `spike/or7-route-{a,b}`. | `rx/OR7-SPIKE.md` has one row per leaf per route (no gaps), each with an oracle verdict. Disagreements are classified ENGINE or SPEC-AMBIGUITY. S14 turns this into the OR-7 ruling ask. |
| **LW2.RX-3** `Hir::to_source` | S7 | The round-trip oracle (R6): parse → Hir → `to_source` → re-parse. | Over every leaf and the smoke corpus, the re-parse gives an equal Hir, and V8 and JSC agree between source and `to_source` on every smoke input. The `parse_generated` `Span::as_str 40..41` panic is filed born-RED ×2 and cured at root if it is in this crate. |
| **LW2.RX-4** fusion off for Rust | S7 | `FuseAltRegexBranches` leaves the Rust pass list until R3 (LW3.RX-1) re-admits it with its `L_cut` certificate. It is removed from the list, not disabled by a flag. | The LW1 born-RED `Alt(/a/,/ab/)` on `"ab"` goes GREEN for Rust (it matches 1 byte, as the oracle does). The pass is absent from the Rust pipeline's printed pass list. TS output is byte-identical (`regen --check`). |
| **LW2.RX-2** Hir ABI freeze | S15 | After OR-7 is ruled: freeze `parse-that-regex`'s Hir types, plus the state type if Route B is ruled. | An `abi_fingerprint` test asserts a recorded hash over the variants, fields and layouts. A mutant that adds a variant fails ×2. The hash is cited in the API-freeze seam record. |

### PP

| Unit | Seat | Work | Falsifier / gate |
|---|---|---|---|
| **LW2.PP-1** op.rs ABI v2 | S8 | `Op` ≤ 24 B, const-asserted. `Text(&'a str)` is kept. A `GroupHdr` side table with u32 fields. | The LW1 u16 `flat_width` born-RED (a group over 65,535) goes GREEN ×2. A 32-byte `Op` mutant fails to compile. The layout record goes in `pprint/rust/ABI-v2.md`, for S15's freeze. |
| **LW2.PP-2** builder `finish()` | S8 | The monotone-stack `trailing` (L3′). Unbalanced input returns `Err`. `Mediumline` is deleted in pprint and at every bbnf-lang user (gorgeous and the emitters, on their own `lw2/pp` branch in bbnf-lang). | The L3′ counterexamples (a fill ancestor; a `HAS_HARD` sibling) go GREEN ×2. An unbalanced-input test returns `Err`. The LW1.PP-1 grep for `Mediumline` reads 0 across pprint, gorgeous, the emitters and `typescript/`. |
| **LW2.PP-3** oracle complete | S8 | The Wadler 2003 §§3–5 plus Leijen `align`/`fill` oracle is finished, with L1–L8 property tests against it. Each test cites the equation it checks. | ≥ 10⁵ cases per law. A mutant renderer (an off-by-one in `fits`) is caught by at least one law ×2. |

### CON

| Unit | Seat | Work | Falsifier / gate |
|---|---|---|---|
| **LW2.CON-1** plane specs and parity classes | S9 | `bench/planes/P1..P7.md`: UTF-8 validation, escapes, number fidelity, duplicate keys and error offsets. `bench/contest/ROWS.tsv` gives every row an output-parity class and a work checklist, or marks it "no comparator". Units are fixed at the source: skinny `profile_direct` reports MB/s (10⁶ B/s) rather than megabits. The legacy predicate `track1_beats_lightningcss` is retired. S9 also writes the PREREG families and the A/A, slowdown and plane-mismatch cells for Phase 3. | Falsifiers (c) and (d) of competition U4: a skip-unescape mutant is refused as plane-inequivalent ×2, and a build-plane mismatch is refused ×2. Every ROWS row has a class. `git grep -n 'track1_beats_lightningcss\|mbps'` = 0 in the bench sources. |
| **LW2.CON-2** feasibility seam: measure | S11 | Measured under §5: A/A self-vs-self noise per family; a baseline per goal (G-C1, G-C2, G-R1, G-R2, G-J1, G-J3, G-G1..G3, G-P1, G-P2, G-E1, and hygiene P-5); a roofline per goal (memory bandwidth, plus PERF's per-kernel NEON peak); COMP-1's trial cells; RX-1's emitted-code cells; USE's G-S cells and LG-d timings. Floors are PROPOSED as `max(10 %, 3 × A/A noise)` (competition §3) in `bench/FLOORS-PROPOSED.tsv`. | Falsifiers (a) and (b) of U4: an A/A run reports 0 wins beyond the floor, and an injected 15 % slowdown mutant is detected. Every record carries its load, thermal state and QoS. A goal whose records are NOISY gets no proposed floor: it is marked NOISY-UNFROZEN and re-queued once inside its 72 h budget (X-7). |
| **LW2.CON-2s** U6s seal | S15 | After the floors ruling: one hashed decision-plan commit, `bench/DECISION-PLAN.lw2.json`. It fixes every gate value, certification cell, family, primary competitor and per-unit wall-clock budget. Its sha256 goes into the RULINGS addendum. | `shasum -a 256` of the file equals the RULINGS citation. Any later change needs an addendum with owner sign-off (competition U6s). |

### USE

| Unit | Seat | Work | Falsifier / gate |
|---|---|---|---|
| **LW2.USE-1** G-S1/G-S2 pre-registration | S10 | Add to `bench/PREREG.md`: ABBA order, seed and corpus hash for the 14 G-S1 cells; G-S2 tiers TOKENIZE, COLOR→RGBA and FULL-VALUE. | The PREREG hash is committed before S11, and S11 refuses unregistered cells (shared with COMP-1's falsifier). |
| **LW2.USE-2** equal-work proof | S10 | For base `4038dce5`: both arms do the same work over the 17,316 WPT inputs (output checksum walk inside the timed region). | Checksums are equal on 100 % of inputs ×2. A skip-work mutant is refused ×2. |
| **LW2.USE-3** LG-d same-artifact conformance | S10 | Clear row 0's NOT-SAME-ARTIFACT caveat. Re-judge V-C on the published 4.2.1 tarball whose bytes row 0 carries, or on 4.3.0 if value.js has published it by then. Read-only: the tarball comes from npm, and the value.js tree is not edited. | Row 0's conformance and bytes cite one tarball sha256. The re-read timing is a Phase 3 cell. |
| **LW1.USE-5** `files` gap (carried) | S10 | The hold has passed from wf_d837f96c to value.js's own app-work hold. Re-read the born-RED test and file the ask on a COHESION pointer line. Do not edit value.js `package.json`. | `use5-files-gap.mjs` is still RED ×2 until a value.js release carries the six `.bbnf` files. The row names the edit and its owner. |

### Seams, freeze and gate

| Unit | Seat | Work | Gate |
|---|---|---|---|
| **LW2.SEAM-A** architecture and API freeze (PLAN §6: LW1 close and LW2 close) | S12, S14 | S12 critiques: the crate census and DAG (TOPO-1, COMP-1b), the published names (S3's availability read), the Hir ABI candidate (RX-2 inputs), op.rs ABI v2 and the toolchain plane. | S14 rules each objection ACCEPTED, accepted in part, or REJECTED, with reasons, in `LW2-SEAMS.md`. |
| **LW2.SEAM-B** feasibility (CON-2) | S13, S14 | S13 critiques: the floors' derivation, A/A noise, the rooflines, the NOISY handling, the COMP-1 verdicts, and the QoS reading (§5). | The same as SEAM-A. S14 files the ruling asks: OR-7, FLOORS (one row per goal), the QoS reading, R-c if still owed, and every taken crate name. |
| **LW2.GATE** | S16 | TOPO-4 and the TOPO-5 master merges. Read LG-2 ×2 (§4). Append the ledger row, the value.js COHESION §0hz pointer (in a temp worktree off local `tranche-u`: merge origin, push, remove the worktree) and the parse-that `docs/tranches/SK-V27.md` pointer. Then a process census. | §4. The gate seat writes no product code. It merges only after CI is GREEN, and it never takes a PR out of draft with a RED row. |

## 4. LG-2: the lockstep gate for LW2

These are PLAN §4 rows LG-a..LG-g, plus:

| Row | Measure |
|---|---|
| LG-a | pprint → parse-that → bbnf-lang build and test from clean clones at pinned git revs, credential-free, on CI, with no `[patch]`. `cargo +stable` rc 0 in all three. |
| LG-b | The baseline is re-read at `grammar/MANIFEST`'s hash, which replaces the `MANIFEST.sha256` stand-in. The 0-divergence bar starts at LW3 (after R-b). |
| LG-c | Consumer gates are re-read as at LW1. No consumer pin moves in LW2. |
| LG-d | Row 0 has the same artifact across bytes and conformance (USE-3). Timings are re-read in the Phase 3 window. |
| LG-e | Every cell is re-read. Only Phase 3 records can propose a floor. Nothing is CERTIFIED in LW2. |
| LG-f | Every process went through `guarded`, including probes, debuggers and drivers. The bench seat used `--exclusive bench --cpu-cores 0`. A process census at close shows nothing left running. |
| LG-g | The branches are pushed, the ledger is updated, and the COHESION and SK-V27 pointers are in place. |
| **LG-2.1** | `check-repo-dag.sh` GREEN in all three repos, and parse-that and pprint take no edge into bbnf-lang. |
| **LG-2.2** | bbnf-lang, parse-that and pprint master CI GREEN on their merge commits (nextest 0 failures, `substrate_audit` included), and `is-ancestor af15f63e0 origin/master` true. |
| **LG-2.3** | ABI freezes recorded: op.rs v2 (const assert plus `ABI-v2.md`) and the Hir fingerprint (RX-2), or the OR-7 RULING row. |
| **LG-2.4** | Floors frozen: `DECISION-PLAN.lw2.json`'s hash is cited in RULINGS, or each unfrozen goal is a named NOISY-UNFROZEN or RULING row. |
| **LG-2.5** | The custody receipt still verifies (`shasum -c` rc 0) and no born-RED test from LW1 has regressed. |

The wave closes only when every row is GREEN ×2. A RED row is cured inside the wave or carried as a named ledger row with its blocker or ruling. No row is waived silently.

## 5. The bench window (X-7) and the QoS reading

- **Scheduling:** S11 runs with no other seat of this workflow live, and the orchestrator dispatches no heavy seat while S11 holds `guarded --exclusive bench`.
- **Each record** waits at most 60 min for a 1-minute load below 8, then runs labelled NOISY.
- **Blocks:** paired and interleaved, after A/A calibration. A block with load ≥ 8 or a non-nominal thermal state (`pmset -g therm`) is discarded (competition §3).
- **Budget:** each unit has 72 h and is re-queued once.
- **Records:** raw samples go in `~/.dev-logs/bbnf-lockstep/bench/lw2/`, and the records in `docs/benchmarks/records/LW2-CON-2/`, keyed by tree hash plus commit sha.
- **QoS reading (proposed by this order; ruled at SEAM-B):** under X-7, the NOISY label is decided by load and thermal state. User-interactive QoS cannot be reached from this host's seat ancestry (LW1.CON-1). Every record states its QoS class as a disclosed host condition. A pair whose two arms ran under different QoS accounting is discarded. If the owner rules instead that QoS must be interactive, no floor is frozen in LW2, and the runner-context fix becomes the owner's machine-law act.

## 6. Order of execution, in one view

```
P0  S1 CUSTODY [TOPO-0, TOPO-6]        ║ S2 CENSUS [COMP-1b]
P1  S3 DAG [TOPO-1 a–d]                (alone; push pprint → parse-that → bbnf-lang)
P2a S4 TOOLCHAIN [TOPO-2, TOPO-3]      ║ S5 COMP [COMP-2, COMP-3, COMP-4, COMP-1 prereg]
    S8 PP [PP-1..3]                    ║ S9 CON-SPEC [CON-1, PREREG families]
P2b S6 PERF [PERF-1..3]                ║ S7 RX [RX-1, RX-3, RX-4]
    S10 USE [USE-1..3, USE-5]          ║ (S5 continues)
P3  S11 BENCH [CON-2 measure, COMP-1 verdicts, queued cells]   (alone, X-7)
P4  S12 CRITIC-A ║ S13 CRITIC-B  →  S14 ADJUDICATOR [SEAM-A, SEAM-B, ruling asks]
--  ruling checkpoint: workflow returns; resumed when RULINGS.md carries OR-7, FLOORS, QoS (+ R-c, names)
P5  S15 FREEZE [RX-2, CON-2s, ABI records]  →  S16 GATE [TOPO-4, TOPO-5, LG-2 ×2, custody]
```

The ruling asks of record: **R-c** (needed at Phase 0), **OR-7** (from RX-1), **FLOORS** per goal (from CON-2), the **QoS reading** (§5), and any **taken crates.io name** (TOPO-4). R-a is moot if TOPO-1's falsifier is GREEN. R-b is owed for LW3.
