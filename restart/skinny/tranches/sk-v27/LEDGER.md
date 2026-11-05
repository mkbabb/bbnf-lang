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
| 2026-11-05 | **LW1.CON-1 → GREEN** (branch `sk-v27-competition`). `bench/cb-b0/cbb0.py` + `manifest.schema.json`: one fresh process per rep (posix_spawn), load and `pmset -g therm` before and after, per-rep kernel QoS accounting (proc_pid_rusage on the unreaped child), records keyed by tree hash minus `docs/benchmarks/records/` plus commit sha. RED ×2 at base (no harness) and on a records-including mutant; GREEN ×2 (10 tests). Host fact kept as an expectedFailure: user-interactive QoS is unreachable from the seat's ancestry (spawn-attribute raises EINVAL, exec resets thread QoS, a self-raise is accounted as legacy; a utility clamp is accounted as utility, so the accounting is truthful). Every record is NOISY on QoS until the runner context grants interactive QoS; routed to X-7. |
| 2026-11-05 | **LW1.CON-2 → GREEN.** `bench/W0-BENCH-PINS.tsv` v2: 40 pins (18 crates, 11 npm, 10 upstream commits, rust stable 1.99.1) resolved 2026-11-05, newest release ≥ 14 d old, each with checksum or tag commit, publish date, and departure from the root/skinny locks and from v1 (`bench/pins/W0-BENCH-PINS.v1-sk-v26.tsv`, verbatim from `47d51a190`). `pins.py check`: RED ×2 on v1 (4 violations), GREEN ×2 on v2 (0). |
| 2026-11-05 | **LW1.CON-3 → GREEN (recorded).** `guarded --exclusive bench` exists (flock `~/.claude/guard/slots/ex-bench`); it serialises bench seats and does not exclude non-bench seats (expectedFailure, routed to X-7). No lock-file fallback is needed. RED ×2 on a mutant whose lock probe is constant False; GREEN ×2. |
| 2026-11-05 | **LW1.CON-4 → GREEN, all NOISY.** 20 schema-valid records in `docs/benchmarks/records/LW1-CON-4/`: skinny `profile_direct` (twitter, citm_catalog, canada × parse_only track1, sonic, serde) and root `json_check`, on PORTABLE and NATIVE, 10 reps each. 1-minute load was 14–27 for the whole seat and QoS accounting was legacy. RED ×2: root `json_check` measured nothing in a clean worktree because `data/` is gitignored, so the root corpus is untracked. Cure: the example takes a corpus dir and is run on the tracked `skinny/test_data`. Found along the way: the root `data/json/twitter.json` (`a08b769f…`) and `skinny/test_data/twitter.json` (`30721e49…`) are different files, so twitter cells across the two workspaces are NOT-COMPARABLE until LW2.CON-1 names one input. The root build is not hermetic either: the `../pprint` and `../parse-that` patches were pointed by `--config` at pprint `2b4d2d4` and parse-that `sk-v27` `b4f63f4`. That gap belongs to LW1.COMP-4. |
| 2026-11-05 | **LW1.CON-5 → GREEN.** `bench/PREREG.md` skeleton (7 families, Holm, α = 0.01, minimum effect 3 %, ≥ 200 pairs to certify, NOISY never certifies), sha256 `a26628789f33b25a201d95b8e7f5434a1920f31387897a9d7a8ffdce3f623e42`. RED ×2 (no file), GREEN ×2. |
| 2026-11-05 | **LW1.CON-4 addendum: records re-taken at a clean tree.** The first take (`179623b1f`) was keyed over untracked `bench/**/__pycache__` bytecode (dirty = true). It was withdrawn after a born-RED test (RED ×2, GREEN ×2 with `bench/.gitignore`). All 20 records were re-taken at commit `46c605608`, tree `a77ddb43…`, dirty = false. They are still NOISY: load 15–21, QoS legacy. Across the two takes, PORTABLE versus NATIVE flips sign on 7 of 9 skinny cells (for example canada track1: 12247/15136 in the first take, 17402/12293 in the second). Under this load the matrix difference cannot be decided, which is why LW2.CON-2 measures self-vs-self noise in a quiet window before any floor is frozen. |
