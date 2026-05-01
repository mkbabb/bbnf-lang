# AUDIT-A — B1 retrospective: invariant honesty + miss attribution

Read-only retrospective on tranche B1, cut against `B1.md` invariants,
the per-wave hard gates, the landed surface, and the bench ledgers.
Audit run 2026-04-25 on `audit-alpha`
(`/Users/mkbabb/Programming/bbnf-wt-audit-alpha`).

## 1. Headline verdict

B1 closed an honest infrastructure pivot — pin, alias surface, nextest
4-profile config, divan migration mechanics, CI rewire, abrogation
catalog — with cherry-pick discipline and zero master conflicts. The
triumvirate caught gorgeous's `default = [...]` activator and cut the
prettify wall from ~500 s to 33 s warm. **But B1 also (a) introduced a
28× warm regression on its own routine surface (`iter-check` 0.14 s →
3.88 s) without ever measuring against the AY-II.W0'.d7 baseline;
(b) downgraded invariant 11 from "≤ 5 min" to "measured truth"
mid-tranche after the bootstrap wall surfaced, then declared
"green-routed" on a numeric target never met; (c) shipped W3 close with
an undispatchable `make ay-bench-close` that needed a W4 amendment to
restore; (d) mis-named `bench-json` so the alias does not resolve.**
The architectural lever (gorgeous flip) is real and load-bearing for
AZ-I; the numeric close was face-saving.

## 2. Invariant-by-invariant audit

| # | Invariant (abbreviated) | FINAL | Honest reassessment |
|---|---|---|---|
| 1 | No parity-critical runtime touch | green | **green** — every commit `41b9c4fb..d019bcc6` is config / harness / CI / docs only. |
| 2 | Blocks AY-II.W0' close | green | **green**. |
| 3 | Routine commands seconds-scale | green | **AMBER** — `iter-check` 3.88 s warm vs d7 0.14 s warm is a 28× regression on the very surface invariant 3 governs. `AY-II/PROGRESS.md:345` records 0.14 s; `post-B1-W0-routine.txt:3` records 3.88 s without flagging the delta. |
| 4 | Public proof commands runnable + documented | green | **AMBER** — `bench-json` targets `--bench json_value`; core has `json_monolithic_value` (`Cargo.toml:165`); `json_value` lives in `crates/json-prototype/`. Alias does not resolve. |
| 5 | Doc claims re-verified | green | **AMBER** (same as 4). |
| 6 | No stale `dead_code` / `cfg(false)` | green | **green**. |
| 7 | Samply symbol-resolved | green-routed | **defensible** — B1 never ran samply; routing to W0' is honest. |
| 8 | Divan + nextest artefact trail | green | green-mechanics, green-routed for per-bench JSON which never landed. |
| 9 | No successor debt tree | green | **AMBER** — ledger lists 7 "post-B1 polish" items; "polish" is not a tranche letter. SPEC's named-successor test is met only formally. |
| 10 | Per-exclude fast-path aliases | green | **green** (`.cargo/config.toml:106-110`). |
| 11 | iter-check-full ceiling | green-routed | **face-saving rewrite** — invariant 11 originally read "≤ 5 min cold". `1926aed1` reworded it to "measured truth, NOT a numeric target" *after* the >660 s halted-at-cap measurement. Honest label: deferred-with-rationale; "green-routed" overstates closure. |
| 12 | `.bbnf-cache` untouched | green | **green** (`d276934a`). |
| 13 | DELETE only after REPLACE live | green | **green**. |
| 14 | No divan port until iter-check-full clean | green-routed | **defensible-but-thin** — iter-check-full did NOT exit clean. W1 ports landed under "ICE-clean substitutes for clean-exit". The "architectural-equivalence ledger" framing converts a missed gate into a satisfied one; the plain reading is invariant 14 was violated as written and rewritten as a mechanical-equivalence test. |
| 15 | bencher removed in same commit as final port | green | **green** (`f9c3db38`). |
| 16 | Sibling-repo pin triad | green | **green** (`62227603`). |

Net: **11 of 16 close cleanly**, **3 are face-saving / softened** (3, 11, 14), **2 are AMBER on the surface** (4, 9).

## 3. Wall-clock regression diagnosis (iter-check warm 0.14 s → 3.88 s)

The `iter-check` alias body is byte-identical between `700501f5`
(AY-II.W0'.d7) and `416dcf76` (B1.W0.b) — both exclude the same four
crates. The 28× regression cannot come from the alias.

It comes from **B1.W0.b adding `[build] rustflags = ["-Zthreads=8",
"-Zshare-generics=y"]` plus the same flags on
`[target.aarch64-apple-darwin]`** to a config that previously had
*zero* active rustflags (`git show 700501f5:.cargo/config.toml` shows
the lld block fully commented out and no `[build]` section).

`-Zthreads=8` spawns the parallel rustc front-end and
`-Zshare-generics=y` forces cross-crate generic-sharing — both help
**cold** workspace compilation where they amortize. They pay
fixed-cost overhead per rustc invocation that **dominates a warm
incremental check** on a small crate set. The pin is not the cause:
toolchain commit was identical between d7's retest and B1's routine
measurement once the override took effect. The cause is the rustflags
addition.

`B1.W0.b` smoke measured `cargo build -p bbnf --profile ax-iter` cold
at 44.91 s and recorded `w0b-smoke pass`. There is **no warm
measurement of `iter-check` between d7 and the post-triumvirate routine
ledger.** The regression was not measured because the W0.b agent never
re-ran the d7-baseline command that invariant 3 implicitly requires.

Mitigation: drop `-Zshare-generics=y` from `[build]` (its benefit is
strictly cold + cross-crate-generic; warm `iter-check` is neither), or
scope both flags to `[profile.ax-iter]` so they only fire on
intentional compile invocations. This was not in any B1 risk register.

## 4. The miss: bbnf-bootstrap structural wall

The triumvirate research correctly identified gorgeous's 6 derive-Parser
sites + `default = [...]` as the prettify wall (research §Q5, §Q8
Lever 1). The same research probed bbnf-bootstrap at P3 and recorded
"≥ 300 s observed at this pin, killed at 5:00, single rustc inside
derive expansion." It routed the wall to AZ-I.W0 (Watt /
cache-relocation) rather than absorbing it.

**The miss is not the routing; it is the plan-time classification.**
Three facts were available before dispatch:

1. The same single-rustc-single-derive pattern that produced gorgeous
   500 s produces bootstrap > 600 s (one derive site vs six).
2. AY-II's IR-pipeline accumulation (`project_types` totality,
   structural-scan emission per `487b17b7`, materializer projection,
   egraph G1-G4 per `e189ebaf`/`a5d581ab`) all execute inside
   `bbnf_derive::bbnf_derive` at expand-time. Pre-AY-II this wall was
   ~10-15 min; post-AY-II it is bound by per-grammar IR work.
3. `meta-audit/04-toolchain-pain.md:20` already cited > 130 s pre-AY-II.

A plan-time check would have asked: "If gorgeous is 6×130 s and
bootstrap is 1×130 s, why does this run show 600+ s?" The answer is
IR-pipeline accumulation. B1 flagged the wall but did not record the
**per-derive-site multiplier delta** between AY-II d4 and B1.W0.d. The
8× regression hypothesis (130 s → > 1000 s per single derive) is
consistent with P3 but is not stated in any B1 doc. AZ-I.W0 inherits
"fix it" without "diagnose what AY-II added to the per-derive cost."

The plan-time check that should have caught it: **before authoring
invariant 11's ≤ 5 min target, the planner should have re-probed
`cargo check -p bbnf-bootstrap --lib` cold under the pin and budgeted
from that number.** Instead the planner used the meta-audit/04 130 s
figure — a pre-AY-II observation against an older IR pipeline.

## 5. Plan-doc-code drift survey

| Drift | Status |
|---|---|
| `bench-json` alias → `--bench json_value` (does not exist in core); core has `json_monolithic_value`; `json_value` lives in `crates/json-prototype/` | unfixed; alias does not resolve |
| `iter-test-leaf` missing at W0 close (`post-B1-W0-routine.txt:7` flagged it) | W4 fixed |
| `make ay-bench-close` undispatchable at W3 close (W0.d deleted `ay-*` block; PROFILING.md still cited it) | W4 restored 160 lines |
| `prep-bench` / `final-bench` / `expand-{json,css,bbnf,sheets}` / `asm-parse` aliases missing at W0 close | W4 fixed |
| `iter-check-full-cold-pinned` row both claims "ceiling holds" and notes "halted-at-cap exceeds the 5-min ceiling" | self-contradictory (`proof.txt:38`) |
| Cross-tranche debt names "post-B1 polish" as destination for 7 items; SPEC §B0 prelude requires named successor tranche | letter-not-spirit |

No orphan references to the 5 deleted scripts survive in tracked
`.cargo` / `Makefile` / `.github/workflows/` / `docs/instructions/`
files. Residual mentions are in `docs/tranches/AW/PROGRESS.md` and
abrogation ledgers — historical breadcrumbs. Every divan-ported
`[[bench]]` entry in `crates/core/Cargo.toml` points at a file
importing `divan` (`json_callgrind.rs` lacks it because it is gated
behind `cfg(feature = "iai")`).

## 6. Process pattern audit

**Costly:** agent dispatch failures (`ScheduleWakeup`/`Monitor` for
exit events); target symlink self-reference on every worktree
(`seed-worktree.sh` modernization deferred — bit every W0/W1
dispatch); parallel-agent target-lock serialization
(`post-B1-W1-parity.txt` shows W1.b.2 CSS port took 8m31s because three
parallel W1.b agents hit the same target — `single-cargo-per-target`
feedback exists, was not enforced); triumvirate cost vs payoff (three
commits for a 2-line Cargo.toml diff — but it DID catch the gorgeous
flip); orchestrator-doing-it-directly when agents stalled (routine
measurements were orchestrator-captured; the routine ledger does not
cite agent worktrees).

**Worked:** "deferred-to-AZ-I.W0" ledger entries are precisely scoped —
AZ-I inherits a clean problem. Scope compression at dispatch time:
W0.d's invariant rewording (`1926aed1`) is a contractual softening but
operationally correct (land the lever, flag the gap, don't block on a
ceiling that needs AZ-I work). Cherry-pick discipline holds — every
wave commit composes onto master with disjoint file bounds and no
merge conflicts. Ledgers stay append-only with row-name discipline.

## 7. Honest scores + closing stance

| Axis | Score | Rationale |
|---|---|---|
| Plan adherence | **6/10** | 11/16 invariants close cleanly; 11 rewritten mid-tranche; 14 violated as written; Makefile rewrite required W4 amendment; `bench-json` unresolved. |
| Architectural value | **8/10** | Pin, divan port mechanically complete, gorgeous flip, cross-repo triad, abrogation catalog, CI rewire. Workflow surface is genuinely simpler than pre-B1. |
| Defensible floor | **6/10** | Item 1 ("Truthful routine command docs") undermined by the 28× warm regression and the unresolved `bench-json`. |
| Cost vs payoff | **5/10** | ~11 hours of agent + orchestrator time for an annex that does not unblock its own declared target (≤ 5 min iter-check-full) and introduces a 28× warm regression on its routine surface. AY-II.W0' resumes on a substrate *better* than pre-B1 in alias surface and pin discipline, *worse* than pre-B1 in warm dev-loop wall. |

**Closing.** B1 is a defensible architectural close on a softened
numeric contract. Bootstrap routing to AZ-I is a real handoff. The
warm regression is real and was not measured. The triumvirate paid
once (gorgeous flip) and not twice (stopped at gorgeous, missed
bootstrap as a B1-flaggable per-derive-multiplier *measurement* even
though it correctly routed bootstrap as an AZ-I *problem*).

For AY-II.W0' to resume on truthful ground, two 1-line cleanups should
land first: (1) drop or scope `-Zshare-generics=y` from `[build]` and
re-measure `iter-check` warm against d7's 0.14 s; (2) fix `bench-json`
(`json_value` → `json_monolithic_value`). Neither requires AZ-I work.
