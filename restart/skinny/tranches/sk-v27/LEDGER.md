# SK-V27 "LOCKSTEP": the unit ledger

This ledger is append-only. A unit's status changes only through a dated addendum row in §3; no row is edited in place. Status values: PLANNED, OPEN, RED (born-RED, as expected), GREEN, CLOSED, CARRIED, UNREAD, RULING.

## 1. LW1 units (starting now), with falsifiers

| Unit | Lane | Falsifier / gate | Output | Status |
|---|---|---|---|---|
| LW1.TOPO-0 | TOPO | `designs/{compiler,rust-perf,regex,pprint,competition,consumers,programme}.md` exist and are complete; the truncation marks in §4 are cleared | full adjudicated texts | PLANNED |
| LW1.TOPO-1 | TOPO | every SHA in later records resolves against `heads-W0.json` | `~/.dev-logs/bbnf-lockstep/heads-W0.json`, `CUSTODY-RECEIPT.tsv` | PLANNED |
| LW1.TOPO-2 | TOPO | wf_d837f96c's close is cited by commit; the ownership file lists every held glob | `ownership-W0.json` | PLANNED |
| LW1.TOPO-3 | TOPO | `git ls-remote` shows `sk-lineage/local-master` = `af15f63e0`, `codex/sk-v26-bbnf` = `b5fd0ca98`, `codex/sk-v26-parse-that` = `e31fbfe` and `sk-lineage/pprint-local` = `2b4d2d4`; `shasum -c` over the custody manifest is rc 0 | custody blobs and manifest | PLANNED |
| LW1.TOPO-4 | TOPO | the `grammar/MANIFEST` hash is cited by LG-b; the divergence is classified 71/38 by commit | `grammar/MANIFEST`, `DIVERGENCE.tsv` | PLANNED |
| LW1.TOPO-5 | TOPO | `git merge-base --is-ancestor af15f63e0 origin/master`: false → true; CI GREEN before the master merge | `lockstep/merge-skv25` | PLANNED |
| LW1.COMP-1 | COMP | one census row per crate across root, skinny and sk-v26, each with a fate and an owning unit | `CRATE-CENSUS.tsv` | PLANNED |
| LW1.COMP-2 | COMP | the sk-v26 W2-spine and ABI-3 receipt tests pass ×2 at the merged HEAD, or are filed RED | `RECERT.md` | PLANNED |
| LW1.COMP-3 | COMP | contradictory grammar emits code today (RED); `LatticeDomain` law test RED on `InlineDomain`; `git grep -c 'let _ = csp.propagate'` = 6 | born-RED tests | PLANNED |
| LW1.COMP-4 | COMP | clean clone of each repo builds with no sibling checkout and no `../` path | CB-1a | PLANNED |
| LW1.PERF-1 | PERF | the tier-reachability test is RED on a default x86 build (the `cfg(target_feature)` gates) | born-RED test | PLANNED |
| LW1.PERF-2 | PERF | `cargo +stable build --release -p bbnf -p parse_that -p pprint -p simd-scan`: rc recorded (RED expected) | probe record | PLANNED |
| LW1.PERF-3 | PERF | RP-13 1.56× has a plane, API and HEAD, or a retraction row | `RP-13.md` | PLANNED |
| LW1.RX-1 | RX | V8 and JSC agree with themselves ×2 on the smoke corpus; a disagreement is SPEC-AMBIGUITY | oracle harness | PLANNED |
| LW1.RX-2 | RX | the four cases read RED ×2 for the stated reason (`/\d+(?!\.\d)/y` → V8 `"1"`; Alt fusion; SupersetAbsorb; Kelvin) | born-RED tests | PLANNED |
| LW1.RX-3 | RX | a lookaround count over 121 + 56 leaves; if > 0, a lookaround charter becomes a G-R1 gate | `LOOKAROUND-CENSUS.tsv` | PLANNED |
| LW1.RX-4 | RX | C1-9 and C2-6 restated and ruled | adjudication addendum | UNREAD |
| LW1.PP-1 | PP | `Mediumline` hits = 0, or an escalation to the owner | grep record | PLANNED |
| LW1.PP-2 | PP | the u16 overflow and L3′ counterexample tests read RED ×2 | born-RED tests | PLANNED |
| LW1.PP-3 | PP | each oracle function cites its Wadler or Leijen equation | `oracle/` | PLANNED |
| LW1.CON-1 | CON | a schema-valid manifest from a dry run; each record is keyed by tree hash excluding `docs/benchmarks/records/` | CB-B0 harness | PLANNED |
| LW1.CON-2 | CON | every pin has a checksum and a publish date ≥ 14 days before resolution | `W0-BENCH-PINS.tsv` v2 | PLANNED |
| LW1.CON-3 | CON | the `guarded --exclusive bench` behaviour is recorded; fallback is a lock file at `~/.dev-logs/bbnf-lockstep/bench.lock` | record | PLANNED |
| LW1.CON-4 | CON | baselines on both matrices, labelled NOISY unless the load was < 8 | records | PLANNED |
| LW1.CON-5 | CON | `bench/PREREG.md` skeleton committed and hashed | PREREG | PLANNED |
| LW1.USE-1 | USE | every parse site in the six consumers is listed with its file and line | `CONSUMER-CENSUS.tsv` | PLANNED |
| LW1.USE-2 | USE | LG-d row 0 for value, keyframes and fourier | benefit table | PLANNED |
| LW1.USE-3 | USE | 66 rows, each with a lane and a unit | V-C intake | PLANNED |
| LW1.USE-4 | USE | the pin per app and lockfile is recorded | pin census | PLANNED |
| LW1.USE-5 | USE | a filed row naming the gated `package.json` edit | row | PLANNED |

## 2. Design crosswalk (every adjudicated item → unit)

| Design item | Unit(s) |
|---|---|
| compiler C1-1 (CB-1a/1b) | LW1.COMP-4; LW6.TOPO-1 |
| compiler C1-2 / C2-6 (equivalence contract, generality, held-out) | LW2.CON-1; LW3.COMP-5 |
| compiler C1-3 / C2-8 (cold process, statistics) | LW1.CON-1; LW5.CON-1 |
| compiler C1-4 (deadlines, UNDECIDED) | LW2.COMP-1; X-7 |
| compiler C1-5 / C2-4 (CB-5.5, CB-B0) | LW2.COMP-4; LW1.CON-1 |
| compiler C1-6 (CB-10 parity gate) | LW4.COMP-1; LW7.COMP-1 |
| compiler C1-7 / I8 (independent oracles) | LW3.COMP-3 |
| compiler C1-8 / I9 (`obs()`, failure contract, depth) | LW3.COMP-4 |
| compiler C2-1 (CB-10b; CB-11 VM) | LW4.COMP-2; LW4.COMP-3 |
| compiler C2-2 (CB-0.5; gorgeous; kernels; bbnf-ser) | LW1.COMP-1; LW4.PP-1; LW2.PERF-1; LW4.COMP-1 |
| compiler C2-3 (one regex engine) | X-2; LW4.RX-2 |
| compiler C2-5 (root trials first) | LW2.COMP-1 |
| compiler C2-7 (competitor parsers upstream; one profile) | LW1.CON-2 |
| compiler R1..R4 | X-1, X-4, LW3.COMP-2, LW1.TOPO-3/5 |
| rust-perf R0-a / R0-b / R0-d | X-6; LW1.TOPO-3; LW1.COMP-2 |
| rust-perf R0-c | X-4 |
| rust-perf R0-e / R0-f / R0-g | X-1 / X-3, X-5 / X-7 |
| rust-perf O-3 (target, stable) | LW2.TOPO-2/3 |
| rust-perf canada (RP-13 / U10) | LW1.PERF-3; LW5.PERF-1 |
| rust-perf G-C1a/b, G-P1, tracks, matrices | LW3.CON-1; LW5.CON-1 |
| rust-perf regex oracle dialect, lookaround corollary | LW1.RX-1, LW1.RX-3 |
| regex C1-1 (`end_unambiguous`) | LW1.RX-2; LW3.RX-1 |
| regex C1-2 / C2-2 (u/v semantics, encodings) | LW1.RX-1; LW3.RX-1 |
| regex C1-3 (V8 plus JSC authority) | LW1.RX-1 |
| regex C1-4 / C2-3 (OR-7 spike) | LW2.RX-1 |
| regex C1-5 (convergence) | LW4.RX-2 |
| regex C1-6 (DAG) | X-2, X-5 |
| regex C1-7 (x86 cfg gates) | LW1.PERF-1; X-4 |
| regex C1-8 (Teddy) | LW4.RX-3 |
| regex C2-1 (`L_cut` certificate; absorb preconditions) | LW1.RX-2; LW2.RX-4; LW4.RX-2 |
| regex C2-4 (`to_source` split) | LW2.RX-3 |
| regex C2-5 (R1.6 ABI freeze) | LW2.RX-2 |
| regex C1-9, C2-6 | LW1.RX-4 (UNREAD) |
| regex ADJ-1 / ADJ-2 / ADJ-3 | LW5.RX-1 / LW3.RX-2 / LW1.TOPO-5 |
| pprint C1-O1 (Wadler semantics) | LW1.PP-3; LW2.PP-3 |
| pprint C1-O2 / C2-O1 (L3′) | LW1.PP-2; LW2.PP-2 |
| pprint C1-O3 / C2-O3 (trivia U4b) | LW3.PP-2 |
| pprint C1-O4 (G7/G8 out of pprint) | LW4.PP-4; X-10 |
| pprint C1-O5 / C2-O8d (G6a/b) | LW5.PP-1 |
| pprint C1-O6 / C2-O8a-c (statistics) | LW1.CON-1; X-7 |
| pprint C1-O7 / C2-O8e (G4 budget, G5 later) | LW5.PP-1 |
| pprint C1-O8 / C2-O6 (alpha route) | LW3.PP-3; LW6.TOPO-1 |
| pprint C1-O9 / C2-O7 (opt on/off identity) | LW4.PP-2 |
| pprint C2-O2 (`Mediumline`, tabs) | LW1.PP-1; LW2.PP-2 |
| pprint C2-O5 (`Text(&'a str)`) | LW2.PP-1 |
| pprint `pprint-ser` (PP-07) | LW4.COMP-1; LW6.TOPO-1 |
| competition C1-1 (U1a leaf repo) | X-5 (recorded alternative) |
| competition C1-2 / C2-1 (U1b toolchain fairness) | LW2.TOPO-2; LW1.CON-2 |
| competition C1-3 (simd-scan exists) | X-3 |
| competition C1-4 (custody) | LW1.TOPO-3 |
| competition C1-5 (comparators; U6s seal) | LW2.CON-1/2 |
| competition C1-6 (pins) | LW1.CON-2 |
| competition C1-7 (Apple Silicon conditions) | LW1.CON-1 |
| competition C2-2 (heads receipt; held globs) | LW1.TOPO-1/2; PLAN §1 |
| competition C2-3 (certification set; 72 h budget) | LW5.CON-1; X-7 |
| competition C2-4 (falsifiable goals) | LW2.CON-1 |
| competition C2-5 (CSS and regex first; own indexer) | LW3.CON-1; LW3.PERF-1 |
| competition C2-6 (SIMD discipline) | LW2.PERF-1; LW4.PERF-1 |
| competition C2-7 (U7b) | LW6.TOPO-2 |
| competition C2-8 (U15e error quality; WASM cut; incremental out of scope) | LW5.CON-1 (G-E1); recorded scope rows |
| consumers O-1 (P-IR-TS) | LW3.USE-3 |
| consumers O-2 / #2 (Unsat ×6; lattice laws) | LW1.COMP-3; LW2.COMP-2 |
| consumers O-3 (G-P1 narrowed; P-PP-TS) | LW4.USE-1; LW4.PP-3 |
| consumers O-4 / P-1 (released corpus; `files`) | X-8; LW3.USE-1; LW1.USE-5 |
| consumers O-5 / #1 (worktree merge) | LW1.TOPO-5 |
| consumers O-6 / #6 (G-S1 prereg) | LW2.USE-1/2 |
| consumers O-7 (tiers) | LW2.USE-1 |
| consumers O-8 (x86 correctness only) | X-4 |
| consumers O-9 (G-J1 end to end) | LW4.CON-1 |
| consumers #3 (regex lift) | X-2 |
| consumers #4 / #5 (pins; observe 4.3.0) | LW1.USE-4; LW6.USE-1..3 |
| consumers #7 (truncated) | UNREAD (§4) |
| programme R-1..R-8, C1-1..C1-9, C2-* | X-4; §6; LW2.TOPO-*; X-2; LW2.CON-2; LW1.TOPO-*; X-9 |

## 3. Carried tail (from sk-v25 §9, parse-that T/U and the megatranche)

| Row | Home |
|---|---|
| BA-G1a, BB-G1..4, BC-G4, BD-G5, BD-G7 (R-6: retired as plans, gates carried) | re-homed by LW1.COMP-1 census rows |
| T-TAIL-9 rows | re-homed per unit at LW1 close |
| parse-that U.W1 → LW2 (TOPO/PERF); U.W2 → LW4.PERF; U.W3-5 → LW3/LW4.RX; U.W7 retired | parse-that `docs/tranches/SK-V27.md` |
| value.js X·P records (`docs/tranches/X/parse-that/`, COHESION §0ht–§0hz); `.rd` waiting for a quiet window; `.o104` UNREACHABLE | LW1.USE-3; LW5.USE-1 |
| 66 V-C css-color misses (§0hy) | LW1.USE-3 |
| ESC-XPW10-1 rulings 2 and 3 (6 `W10v-CUSTOM-COMPONENTS` rows; the V-C value judge) | value.js X·P; observed by USE |
| WASM (L4w) cut from gated scope; incremental parsing out of scope | recorded scope |

## 4. Open records

- **Dispatch truncation.** The seven design texts reached this plan cut off at compiler §E (G1 onward), rust-perf §1, regex §A (DAG rules), pprint B.2 L9, competition §1 (L4t), consumers §2 (G-J1) and programme R-8. Rulings past those cuts are not in this plan. LW1.TOPO-0 files the full texts, and any further item gets a crosswalk addendum row.
- **UNREAD objections:** regex C1-9 and C2-6; consumers critic 2 #7 (the part past its cut).

## 5. Addenda

| Date | Row |
|---|---|
| 2026-11-05 | Plan formed on branch `sk-v27` off bbnf-lang `origin/master` `c9b93adb4`. Pointers: value.js COHESION §0hx.12; parse-that `docs/tranches/SK-V27.md` (branch `sk-v27` off `47e928acc`). |
| 2026-11-05 | LW1.PERF-1 → **GREEN** (branch `sk-v27-rust-perf`; record `perf/LW1-PERF-1.md`). The born-RED tests were RED x2 for their stated reasons: R1, x86 tiers not compiled on a default build; R2/R2x, the NEON wide/multi epilogues and the AVX2 kernel diverge from `scalar::scan`; R3, a one-byte digraph overread, SIGBUS at a `PROT_NONE` page. The cure: a `dispatch` `OnceLock` tier table, `#[target_feature]` kernels, and shared `stripe` semantics. GREEN x2 native, on default x86_64 (Rosetta) and for AVX2 under Miri. Carried: AVX-512 never executed (Miri lacks compress; it needs an SDE or AVX-512 CI arm, X-4); real-silicon AVX2 → LW2.TOPO-3; wasm32 port and the CLMUL `cfg` → LW2.PERF-1. |
| 2026-11-05 | LW1.PERF-2 → **RED recorded** (`perf/LW1-PERF-2.md`). bbnf-lang rc 101 (`smallbox` `features=["nightly"]` via parse_that); parse-that rc 101 (`-Zthreads`/`-Zshare-generics` in `.cargo/config.toml`). `pprint`, `simd-scan`, `bbnf-regex` and `bbnf-ser` build on stable. The cure is owned by LW2.TOPO-3 and LW2.PERF-1. |
| 2026-11-05 | LW1.PERF-3 → **TRACED, re-scoped** (`perf/RP-13.md`). The 1.56x is the LOCK-4 4B probe: `ce51c36fc` plus the uncommitted numeric-leaf patch, emitted `JsonParser` / parse-that `json_parser()`, fastest-sample. That head is not on `origin/master`, and the patch landed nowhere. Retracted as a standing property. NOISY re-measure at `b4e635d3e`: canada 1.61x / 1.75x (`perf/BENCH-LW1.md`). → LW5.PERF-1. |
| 2026-11-05 | **LW1.CON-1 → GREEN** (branch `sk-v27-competition`). `bench/cb-b0/cbb0.py` + `manifest.schema.json`: one fresh process per rep (posix_spawn), load and `pmset -g therm` before and after, per-rep kernel QoS accounting (proc_pid_rusage on the unreaped child), records keyed by tree hash minus `docs/benchmarks/records/` plus commit sha. RED ×2 at base (no harness) and on a records-including mutant; GREEN ×2 (10 tests). Host fact kept as an expectedFailure: user-interactive QoS is unreachable from the seat's ancestry (spawn-attribute raises EINVAL, exec resets thread QoS, a self-raise is accounted as legacy; a utility clamp is accounted as utility, so the accounting is truthful). Every record is NOISY on QoS until the runner context grants interactive QoS; routed to X-7. |
| 2026-11-05 | **LW1.CON-2 → GREEN.** `bench/W0-BENCH-PINS.tsv` v2: 40 pins (18 crates, 11 npm, 10 upstream commits, rust stable 1.99.1) resolved 2026-11-05, newest release ≥ 14 d old, each with checksum or tag commit, publish date, and departure from the root/skinny locks and from v1 (`bench/pins/W0-BENCH-PINS.v1-sk-v26.tsv`, verbatim from `47d51a190`). `pins.py check`: RED ×2 on v1 (4 violations), GREEN ×2 on v2 (0). |
| 2026-11-05 | **LW1.CON-3 → GREEN (recorded).** `guarded --exclusive bench` exists (flock `~/.claude/guard/slots/ex-bench`); it serialises bench seats and does not exclude non-bench seats (expectedFailure, routed to X-7). No lock-file fallback is needed. RED ×2 on a mutant whose lock probe is constant False; GREEN ×2. |
| 2026-11-05 | **LW1.CON-4 → GREEN, all NOISY.** 20 schema-valid records in `docs/benchmarks/records/LW1-CON-4/`: skinny `profile_direct` (twitter, citm_catalog, canada × parse_only track1, sonic, serde) and root `json_check`, on PORTABLE and NATIVE, 10 reps each. 1-minute load was 14–27 for the whole seat and QoS accounting was legacy. RED ×2: root `json_check` measured nothing in a clean worktree because `data/` is gitignored, so the root corpus is untracked. Cure: the example takes a corpus dir and is run on the tracked `skinny/test_data`. Found along the way: the root `data/json/twitter.json` (`a08b769f…`) and `skinny/test_data/twitter.json` (`30721e49…`) are different files, so twitter cells across the two workspaces are NOT-COMPARABLE until LW2.CON-1 names one input. The root build is not hermetic either: the `../pprint` and `../parse-that` patches were pointed by `--config` at pprint `2b4d2d4` and parse-that `sk-v27` `b4f63f4`. That gap belongs to LW1.COMP-4. |
| 2026-11-05 | **LW1.CON-5 → GREEN.** `bench/PREREG.md` skeleton (7 families, Holm, α = 0.01, minimum effect 3 %, ≥ 200 pairs to certify, NOISY never certifies), sha256 `a26628789f33b25a201d95b8e7f5434a1920f31387897a9d7a8ffdce3f623e42`. RED ×2 (no file), GREEN ×2. |
| 2026-11-05 | **LW1.CON-4 addendum: records re-taken at a clean tree.** The first take (`179623b1f`) was keyed over untracked `bench/**/__pycache__` bytecode (dirty = true). It was withdrawn after a born-RED test (RED ×2, GREEN ×2 with `bench/.gitignore`). All 20 records were re-taken at commit `46c605608`, tree `a77ddb43…`, dirty = false. They are still NOISY: load 15–21, QoS legacy. Across the two takes, PORTABLE versus NATIVE flips sign on 7 of 9 skinny cells (for example canada track1: 12247/15136 in the first take, 17402/12293 in the second). Under this load the matrix difference cannot be decided, which is why LW2.CON-2 measures self-vs-self noise in a quiet window before any floor is frozen. |
| 2026-11-05 | **LW1.USE-1 GREEN** (`consumers/use1-census.mjs`; `ee63331df`). `CONSUMER-CENSUS.tsv`: 5,411 sites in six consumers at their `origin/master` heads (`consumers/HEADS.tsv`), read through git, never a working tree. Calls in src scope: value.js 20 parse / 7 serialize + 8 generated-module edges + 6 grammars; keyframes.js 1 parse (`parse-facade.ts:184` `parseStylesheet`, its single funnel) / 2 serialize; glass-ui 2 / 1; daraksha 1; fourier 1 (`colors.ts:93`); sci-report 0 (easing/math only). Falsifier: 12 oracle sites from independent `git grep`, 2 of them comment lines that must not count. RED ×2 (10 absent) → GREEN ×2 (12/12, byte-identical regeneration). |
| 2026-11-05 | **LW1.USE-2 GREEN, timing NOISY** (`consumers/use2-benefit.mjs`, `BENEFIT-ROW0.tsv`, `records/use2-row0.jsonl`). LG-d row 0 from the published tarballs (sha256-checked): value 4.2.1 ./css+./color closure 283,846 B / 60,312 gz9; keyframes (value 4.2.0, 6 subpaths) 382,008 / 61,982; fourier (4.2.0 ./color) 131,580 / 28,968. Parse warm median (IQR), 15 fresh-process reps each: value `parseCssValue` × 17,316 WPT inputs 51.07 ms (45.61–55.41); value `parseCssColor` × 5,778 css-color inputs 15.39 (13.35–17.02); keyframes `parseStylesheet` × 5 kf sheets 3.29 (3.11–3.53); fourier `parseCssColor` × 5,778 14.23 (13.10–16.22). Load1 22.9–25.7 throughout: **NOISY**, no quiet window found; LW2 re-reads it under `guarded --exclusive bench`. Conformance: value's V-C at W10 close (17,316 / 9,651 pass / 7,599 ruled / 66 MISS) on the unreleased 4.3.0 candidate; 4.2.1 was not re-judged; kf and fourier have no WPT denominator. Hand-parse sites (src REGEX rows): value 62, keyframes 20, fourier 40; lines deleted 0. RED ×2 (missing) → GREEN ×2. |
| 2026-11-05 | **LW1.USE-3 GREEN** (`VC-INTAKE.tsv`). 66/66 misses from value.js `5a3ba9707:bench/records/W10close/gates/vc-1.txt.gz` (sha256 `65f5b11f…`; local tranche-u, push held) classed by rule. New units, observed by USE and cured by value.js X·P: **LW3.USE-4a** contrast-color() 17; **LW3.USE-4b** `none` lost in relative-colour serialization 22, plus powerless hue 2; **LW3.USE-4c** math over channel keywords 12; **LW3.USE-4e** RULING candidates: computed-time `var()` 12 and an em length in color-mix 1. Correction: §0hy's "24 trig relative colours" conflates two counts. The record has 24 *serialization* misses and 12 trig/math refusals. RED ×2 (0/66) → GREEN ×2. |
| 2026-11-05 | **LW1.USE-4 GREEN** (`PIN-CENSUS.tsv`, 24 rows). Pins: value 4.2.0 exact in keyframes.js, daraksha and fourier web; fourier keyframes.js 6.3.0, daraksha 6.2.1; glass-ui ^4.0.0 (lock 4.0.0); sci-report 4.0.0 exact. Oracle 4/4; RED ×2 → GREEN ×2. **One-copy gate RED (read, not cured):** fourier `web/package-lock.json` resolves `@mkbabb/parse-that` 0.7.2 (under latex-paper) and 0.8.2. This goes to fourier's owner. Note: value.js's own dev lock resolves `@mkbabb/value.js` 4.0.0 through its keyframes.js/glass-ui devDeps. |
| 2026-11-05 | **LW1.USE-5 FILED, born-RED ×2** (`use5-files-gap.mjs`). value.js 4.2.1 and 4.2.0 tarballs carry 0/6 `src/css/grammar/*.bbnf`, because `files` = `["dist","!dist/gh-pages","!dist/gh-pages/**"]`. **Gated edit:** value.js `package.json` `files` gains `"src/css/grammar/*.bbnf"`. That path is held by wf_d837f96c until LW1.TOPO-2 confirms the close, and then by value.js's own app-work hold. value.js makes the edit and releases it. The check turns GREEN when a released tarball carries all six byte-identical to its tag; LW3.USE-1 (X-8) waits on that. |
| 2026-11-05 | **LW1 LOCKSTEP GATE — verdict: NOT CLOSED (adversarial reading, seat claude-opus-5-5).** Lane results integrated on `sk-v27-lw1-gate` (ordinary merges of compiler `70e9f4317`, rust-perf `7ee85ae0d`, regex `3af771630`, competition `214b43cd1`, consumers `9afc2a1a0` over `sk-v27`; this ledger union-resolved, no row edited). Heads read: parse-that `sk-v27` `085acdd`, pprint `sk-v27-pprint` `cf83853`, value.js `2fefc1f2b`, keyframes.js `origin/master` `fdb5987f`, fourier `origin/master` `c6afb468c`. GREEN: LG-c, LG-f. RED or unread: LG-a, LG-b (baseline only), LG-d (caveat), LG-e (nothing certifiable), LG-g (TOPO lane absent). Nothing merged to master: no CI is GREEN. Evidence `evidence/LW1.GATE/`; logs `~/.dev-logs/bbnf-lockstep/gate/`. |
| 2026-11-05 | **Gate · TOPO lane: no wave-1 results exist.** TOPO-0, 1, 2, 4 and 5 stay OPEN and carry as must-close-first rows for LW1 (no `heads-W0.json`, `ownership-W0.json`, `CUSTODY-RECEIPT.tsv`, `grammar/MANIFEST`, `DIVERGENCE.tsv` or `lockstep/merge-skv25`). **TOPO-3 ref custody done by the gate** (new branches only, nothing forced): bbnf-lang `sk-lineage/local-master` = `af15f63e0`, `codex/sk-v26-bbnf` = `b5fd0ca98`; parse-that `codex/sk-v26-parse-that` = `e31fbfedc`; pprint `sk-lineage/pprint-local` = `2b4d2d486` (the remote is `mkbabb/pprint`; `mkbabb/pretty` redirects). TOPO-3's dirty-tree capture is NOT done and is a custody risk: the parse-that master checkout holds 14 modified files (+262/−469, incl. `scanners.rs` `ByteClass8` NEON/wasm kernels and deletion of `tests/regex/parser_test.rs`); the bbnf-lang master checkout is dirty too. Local bbnf-lang master `af15f63e0` is 71/38 against `origin/master` and cannot fast-forward; it is left untouched for TOPO-5. |
| 2026-11-05 | **Gate · LG-a cross-repo build: RED.** (1) bbnf-lang: COMP-4's RED is cured by the pprint-local push. Clean clone of `sk-v27-compiler` with a fresh `CARGO_HOME` git cache, `cargo check --workspace --locked`: rc 0 ×2 (pprint fetched over the network). **COMP-4 → GREEN locally.** (2) pprint: clean clone `cargo metadata --locked` RED ×2 at `b133b73` (`no matching package named bbnf-ser`) → cure `cf83853` (bbnf-ser as a git+rev edge on bbnf-lang `c9b93adb4`; lockfile +1 source line) → clean-clone `cargo test --locked --all-features` rc 0 ×2 (86 passed, 11 ignored). (3) parse-that: RED ×2 at `b4f63f4` (`no matching package named egraph-derive`) → cure `3a63bbd` (egraph, egraph-derive, csp-solver on bbnf-lang `c9b93adb4`; pprint, pprint_derive on `2b4d2d48`, the rev bbnf-lang pins; lockfile changes by source lines, and the stale `[[patch.unused]]` rows drop out) plus `085acdd` (the `regex` test target imported `parse_that::regex::parser`, gone since the `hir::parser` move: E0432; the test now imports from `parse_that::regex`). The suite is still **RED ×2: 458 passed, 12 failed.** json_test ×11: `JsonValue::String` keeps its delimiting quotes (`""` parses to `"\"\""`, and keys arrive as `"\"type\""`). css_parse_test `test_nested_media` overflows its stack in debug (SIGABRT). One doctest: the prose fence in `scan/quote_parity.rs:13` is compiled. css_parse_test also needs the gitignored `data/css`. parse-that master CI has been RED on every push since 2026-09-23 (`egraph-derive`), so none of this was visible. |
| 2026-11-05 | **Gate · masking found in CB-1a.** The local clean-clone GREENs used this host's SSH credentials. bbnf-lang carries a private SSH submodule (`docs/precepts` → `git@github.com:mkbabb/precepts.git`, PRIVATE), and cargo updates the submodules of every git dependency. So every git+rev edge into bbnf-lang fails on a public runner: parse-that PR #3 and pprint PR #2 CI RED with `failed to update submodule docs/precepts`. Edges into parse-that and pprint resolve on CI (bbnf-lang PR #8 got past `cargo xtask regen --check`). The root cure is the planned topology rather than a credential: X-5 moves egraph, egraph-derive and csp-solver into parse-that (LW2.TOPO-1), and PP-07's `pprint-ser` removes pprint's bbnf-ser edge. Until then the cycle edges are not hermetic on CI. **OWNER RULING R-a:** keep the X-5/PP-07 cure as the only cure, or also take `docs/precepts` out of bbnf-lang (or make it public). |
| 2026-11-05 | **Gate · bbnf-lang CI (PR #8, draft, `sk-v27-lw1-gate`).** Cure: the rust job and `bench-iai.yml` install sccache (`mozilla-actions/sccache-action@v0.0.9`, `SCCACHE_GHA_ENABLED`). `.cargo/config.toml` mandates sccache as `rustc-wrapper` with no fallback, so every master run since #5 died at `sccache rustc -vV`. With that fixed, the next REDs, masked until now, are read: **clippy `-D warnings`: 554 findings workspace-wide** (177 `comparison is useless due to type limits` from emitted code `__iter_count < 0u32`, which is an emitter root and needs a regen; 2 deny-level `approx_constant` in `tests/structural.rs` and `tests/css_l4_substrate.rs`; the rest `collapsible_if` and kin, starting at `egraph-derive/src/lib.rs:282`). **nextest: 1,038 run, 1,008 passed, 29 failed, 1 timed out.** 28 of the failures read gitignored `data/` corpora, and `substrate_audit_zero_caller_substrates` finds 32 zero-caller `pub` substrates (23 core, 3 ir, 3 csp-solver, 3 simd-scan: `compaction.rs:53`, `compaction.rs:82`, `parity.rs:210`). The list is in `evidence/LW1.GATE/CI-PR8-FAILED.txt`. Routed: clippy debt and the emitter comparison → LW2.TOPO-3 with LW2.COMP; substrate audit → LW1.COMP-1 census fates (PERF already marked `compact_stripe_pext` dead). |
| 2026-11-05 | **Gate · cross-lane root cause: corpus custody.** Gitignored `data/` is behind CI's 28 corpus failures, COMP-2's 7 W3 symlinks, PERF's `data/` symlink, CON-4's root RED, and parse-that's `css_parse_test`. **New unit LW2.TOPO-6 (CORPUS):** one versioned corpus artifact (a release asset with a sha256 manifest), fetched by an `xtask` step locally and in CI; the CSS part is X-8's released value.js corpus. **OWNER RULING R-c:** the form of the artifact (release asset or LFS). |
| 2026-11-05 | **Gate · LG-b TS↔Rust parity: baseline recorded, 20 unruled divergences.** No instance-parity harness exists yet, and `obs()` plus the failure contract arrive in LW3.COMP-4. So the baseline is grammar-language accept/reject over 36 files: bbnf-lang's 30 `grammar/**/*.bbnf` and value.js's 6 canonical grammars at `02603478f`. TOPO-4's `grammar/MANIFEST` is absent, so `MANIFEST.sha256` stands in (sha256 `ff174d99…`). Arms: published `@mkbabb/bbnf-lang` 0.3.0 `BBNFToASTWithImports` against Rust `BbnfBootstrap::parse` at the gate HEAD, each byte-identical ×2. **Result:** 15 both accept, 1 both reject (`misc/emoji.bbnf`), **19 Rust accepts and TS rejects**, **1 Rust rejects and TS accepts** (`misc/json-commented.bbnf`, Rust `Syntax { offset: 264 }`). Of the 19, 18 use `->` type projections (minimal witness `a = "x" -> 0u8 ;`: TS REJECT, `a = "x" ;`: ACCEPT) and 1 (`css/pretty.bbnf`) uses `@ws`/`@token`. value.js's 6 grammars: 6/6 agree. The two front ends accept different languages. **OWNER RULING R-b:** is `->` (and `@ws`/`@token`) BBNF, so TS gains it, or a Rust-backend extension that the parity corpus excludes? It must be ruled before LW3's 0-divergence bar. `evidence/LW1.GATE/LGB-BASELINE.tsv`. |
| 2026-11-05 | **Gate · LG-c consumer gates: GREEN ×2.** value.js `2fefc1f2b`, clean detached worktree, the W9pin/W10close recipe (`evidence/LW1.GATE/lgc-vjs-gates.sh`) under `guarded`, ×2: `gen-grammar --check` 0; E-6 grammar.js 84,412 / **15,181** gz and color.js 29,620 / **6,054** gz (within 15,200 / 6,100); self-contained 0/0; test/css 32 files / 513; MIRROR-DEFECTS 0 (parseCssColor and parseCssScalar, 27,021 cells each); V-C **66** unruled misses, not risen, byte-identical to `W10close` bar the pid (it exits 1 by its own criterion, and LG-c's criterion is "does not rise"). keyframes.js `ci` on master `fdb5987f`: success (check:lib, check, test:lib, test:demo, lint, proof:publish, browser roster). fourier `CI` on master `c6afb468c`: success (api with Mongo, e2e, web, compose boot). No consumer pin moved in LW1. |
| 2026-11-05 | **Gate · LG-d benefit table:** row 0 stands, and the LW1 delta is 0 by construction. **Caveat:** value row 0's conformance column cites the V-C record of the unreleased 4.3.0 candidate, while its bytes and ms are of 4.2.1. That column is marked NOT-SAME-ARTIFACT until LW2.USE re-judges 4.2.1 or measures the 4.3.0 tarball. |
| 2026-11-05 | **Gate · LG-e competition re-read: nothing CERTIFIED, nothing promoted.** All 20 CON-4 records carry `status: NOISY` and the PERF bench is NOISY (2 process reps). Corrections: (1) skinny `profile_direct` `mbps` is **megabits/s** (`profile_direct.rs:117`: bytes × 8 / 10⁶). It sits beside root `json_check` MB/s, and the two use different twitter.json files, so skinny and root columns are NOT-COMPARABLE. (2) BENCH-LW1's "no regression on any corpus" is itself a claim inside noise; read it as "none observed (NOISY)". (3) **PERF-3's re-measure (generated ÷ parse-that canada 1.61× / 1.75×) is NOT-COMPARABLE:** at that HEAD parse-that's `json_parser` returns quoted strings and keys (json_test ×11 above), so the arms do not produce equal output. LW2.CON-1's output-parity classes must exist before any such ratio. (4) The legacy predicate `track1_beats_lightningcss` (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:116`: a +1 Mbit/s margin with no dispersion, disposition only) breaks PREREG's 3 % / Holm rule. Its fate goes to the COMP-1 census (retire). |
| 2026-11-05 | **Gate · falsifier corrections (masking).** (1) COMP-3's falsifier `git grep -c 'let _ = csp.propagate'` = 6 reads **12** tree-wide at the gate HEAD: PLAN, LEDGER, STATUS.tsv, an AZ-IV audit doc and `csp_contract.rs:21`'s own comment all match. The pinned form is `git grep -nE '^\s*let _ = csp\.propagate\(\);' -- crates` = 6 (`plan.rs:167`, `csp_scheduler.rs:235`, `eligibility.rs:67`, `first_sets.rs:71`, `follow.rs:70`, `span.rs:61`), and LW2.COMP-2's 6 → 0 is measured by that form. (2) COMP-4's GREEN was credential-assisted (see the CB-1a row). (3) Re-read: pprint born-RED 9/9 RED under `--ignored` at `cf83853` (one gate reading, each for its stated test). (4) The shared `.git/hooks/pre-commit` is a local, untracked sk-v26 hook that runs an unguarded `cargo test` (`w3_replay_public_contract`). Every LW1 seat bypassed it with `--no-verify` or `core.hooksPath=/dev/null`. **OWNER ACT R-d:** retire it (it breaks the machine law). The gate did not edit git config. |
| 2026-11-05 | **Gate · LG-f machine law: GREEN.** Every gate run went through `guarded` (cargo checks, tests, clippy, npm ci, the value.js gates). The gate ran no benchmark. Process census at close: no cargo, rustc, bench or oracle processes from the lockstep; one foreign `sleep 600` loop shell (pid 54724, another session). Note: guard classes any command containing `probe` as a browser run, so the gate's first TS read queued in the browser pool (renamed `lgb_ts_read.mjs`; same class as RX's `run.mjs`). |
| 2026-11-05 | **Gate · wave-2 readiness.** **LW1 must-close-first (carried):** TOPO-0, 1, 2, 3 (dirty-tree capture + `shasum -c` manifest), 4 (`grammar/MANIFEST`, which LG-b then cites) and 5 (history merge); then COMP-2's re-run at the merged HEAD. **Ready for LW2 now (born-RED in place, no ruling or merge needed):** LW2.COMP-2 (CSP Unsat, pinned falsifier above); LW2.PP-1, LW2.PP-2; LW2.RX-3 (also file `parse_generated`'s `Span::as_str 40..41` panic as a born-RED), LW2.RX-4; LW2.PERF-1 (wasm32 onto `stripe.rs`; runtime CLMUL); LW2.CON-1 (output-parity classes, one twitter.json, units); LW2.USE-1, LW2.USE-2. **Blocked:** LW2.TOPO-1 (X-5 move; also cures R-a), LW2.TOPO-3 (stable CI; needs the clippy debt, LW2.TOPO-6 and the substrate audit), LW2.COMP-1 trials (CB-B0 records still NOISY: no interactive QoS, X-7), any master merge (TOPO-5, and no GREEN CI on any of PR #8 / parse-that #3 / pprint #2). **Owner rulings owed:** R-a (precepts submodule), R-b (BBNF dialect `->` / `@ws` / `@token`), R-c (corpus artifact form), R-d (retire the local hook); plus X-1..X-5 and X-7 for LW2 itself. |
| 2026-11-06 | **LW1.COMP-4 → GREEN (CB-1a, credential-free, CI too).** `scripts/check-hermetic-build.sh` reads cargo's own resolution: no path package outside the repository, every git package pinned to a rev or an upstream release tag, then `cargo check --workspace --all-targets --locked`. A new CI job `hermetic` runs it on a fresh checkout with no siblings and `persist-credentials: false`. Falsifier `evidence/LW1.COMP/cb1a-clean-clone.sh` (blobless https clone, fresh `CARGO_HOME`, no git config, no ssh agent, ssh disabled): RED ×2 at `c9b93adb4` (rc 101, `../parse-that/rust/regex` missing) → GREEN ×2 at `97b4a2db4` (rc 0) — this replaces the gate's credential-assisted GREEN. CI: `hermetic` GREEN on PR #8 (run `42169585394`) and on the following push. The first credential-free read flagged gorgeous's biome edges (tag `cli/v1.9.4`, locked `fa93a147`); the rule was stated precisely as "rev or release tag" (`97b4a2db4`). **Stale hook removed** (ruling): the shared `.git/hooks/pre-commit` (sha256 `23ed3ccf…`, byte-identical to `b5fd0ca98:scripts/hooks/pre-commit`) is copied to `~/.dev-logs/bbnf-lockstep/custody/hooks/` and deleted; `hook-check.sh` RED ×2 → GREEN ×2; no `--no-verify` or `core.hooksPath` override since. Git config untouched. Out of scope and still RED: git edges *into* bbnf-lang (parse-that #3, pprint #2) need R-a or X-5/PP-07. |
| 2026-11-06 | **LW1.COMP-2 → DONE at the merged HEAD.** The spine is not in TOPO-5's `077d9338d` (`b5fd0ca98` is 24 commits on `af15f63e0`; its merge is LW3.COMP-1), so the receipts were re-proved on an ordinary trial merge of `b5fd0ca98` over `077d9338d` (tree `08e8844be`, no conflicts, only `typescript/`, `release-npm.yml` and 2 `.gitignore` lines differ from the spine; not pushed). **ABI-3 92/92 ×2** at parse-that `e31fbfedc`, the doctest included, matching the receipt's 92. **W2 spine 60/60 ×2.** **W3 replay/public guard RED ×2 for two reasons:** the 7 gitignored corpus symlinks (→ LW2.TOPO-6), and, new with the merge, 5 byte-identical grammar copies origin `0999cb8c3` checked in under `typescript/test/fixtures/grammar/lang/` because the TS front end cannot read the Rust dialect. Recreating the symlinks leaves the guard RED ×2 for the copies alone. → LW3.COMP-1 after owner ruling R-b. Also for LW3.COMP-1: the spine's `../parse-that-skv26` path deps become git+rev edges on `e31fbfedc`. `evidence/LW1.COMP/RECERT.md`. |
