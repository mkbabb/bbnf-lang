# AZ-III REAUDIT 2026-04-30 — Lane 5: Plan / Waves / Tranche Drift

Read-only audit of AZ-II FINAL/PROGRESS/PROGRESS-SNAPSHOT, the AZ-III
plan, the six AZ-III wave specs (W0..W5), the SIX-AGENT-SYNTHESIS, the
W0 dispatch packets, and the prior four tranche FINALs (AU/AV/AW/AX
plus AY-I and AZ-I context). HEAD `d5179b8a` (master). Cap 25 min.

**Verdict on the user's claim "last several tranches have not landed
quite properly":** **CONFIRMED**. AS, AT, AV, AW, AY-I, AY-II-I,
AY-III, AZ-I, AZ-II all closed with recorded misses, deferral
ledgers, partials, or supersession-into-the-next-letter. AZ-II is the
first close that admits the pattern in plain language ("continuation
handoff") — but the same drift was disguised across at least eight
prior tranches as "carry-forward", "deferred", "absorbed", "routed to
the successor", "PARTIAL CLOSE", or "honest scope-reveal". The
chronic-deferral ledger in §2 names sixteen items that crossed two or
more tranche boundaries; five crossed five or more.

**Verdict on the AZ-III plan:** **mostly well-formed, but with
identifiable drift, three duplicated commitments, one missing axis
(commit-discipline retro on already-landed AZ-II commits), one
under-bound wave (W4), and one wave (W3) whose scope is
larger-than-the-rest-of-the-tranche-combined and should be split.**
Concrete reorder/merge/split proposals are in §7; wave-spec patches
are in §8.

---

## 1. Tranche close-honesty audit (last four tranches before AZ-II)

The last four tranches before AZ-II are AY-I, AY-II-I, AY-III, AZ-I.
Walking AY-III back further reveals AX, AW (with five sub-passes
I/II/III/IV/V), AV, AU as additional context.

### AU — closed PARTIAL with five MISSED gates and one DEFERRED

Source: `docs/tranches/AU/FINAL.md:1-30`, gate table
`docs/tranches/AU/FINAL.md:245-288`.

> "Ten hard gates fully met; two met with a documented qualifier;
> five partial; five missed; one deferred; one not applicable."
> (`AU/FINAL.md:7-9`)

Concrete unmet gates:
- Gate 11 — bbnf/sonic ratio ≥ 0.60 twitter / ≥ 0.80 canada: MISSED,
  routed to AV (`AU/FINAL.md:267`).
- Gate 12 — every `->` reaches tape: MISSED, "Bug 1 + Bug 2
  systemic" (`AU/FINAL.md:268`).
- Gate 21 — JSON canada ≥ 1800 MB/s on decoded path: MISSED
  (`AU/FINAL.md:287`).
- Gate 22 — CSS bootstrap ≥ 600 MB/s: MISSED (`AU/FINAL.md:288`).

**Was the deferred work absorbed by AV?** Partially. AV closed Bug 1
+ Bug 2 typed-materialization at AV.0 (`AV/FINAL.md:21-49`), but
plan items V6–V9 were "deferred to AW per orchestrator scope
decision" (`AV/FINAL.md:7-12`). Throughput parity (canada ≥ 1800,
twitter ratio) **was NOT closed in AV** — AV deferred it forward
again.

### AV — closed substrate-only, V6-V9 punted to AW

Source: `docs/tranches/AV/FINAL.md:1-20`.

> "V6 (document-level parallel parse), V7 (SIMD keyword dispatch +
> PHF + selector classifier), V8 (runtime bloom+GADT dedup), V9
> (walker + reader migration closure) routed forward to tranche AW
> per orchestrator scope decision after V5 lands."
> (`AV/FINAL.md:5-10`)

Two AV gates explicitly partial:
- Hard gate 12 — 6× SIMD-packed reorder: PARTIAL, "3.3× scalar
  reordering vs. the plan's 6× SIMD-packed target; full
  vectorisation routes to AW" (`AV/FINAL.md:114-115`).
- Hard gate 15 — fn-per-rule deletion: NOT MET, "deferred to AW"
  (`AV/FINAL.md:138`).

Plus thirteen `serialize_roundtrip`/structural-roundtrip tests
explicitly `#[ignore]`d with "AW V6+ forward-tickets"
(`AV/FINAL.md:208`).

**Was AV's deferral absorbed by AW?** **NO.** AW exploded into five
sub-passes (AW-I, AW-II, AW-III, AW-IV, AW-V) over five days, none
of which beat post-AU. The bench gate that AV punted to AW finished
AW-V at "0/17 parse entries exceed post-AU" (`AW/FINAL.md:417-431`).

### AW (I/II/III/IV/V — five sub-tranches) — 0/17 close with carry-forward triplet

Source: `docs/tranches/AW/FINAL.md` (five close blocks).

AW-I is not present as `FINAL.md` but referenced; AW-II closed
`1050 passed / 50 failed / 67 ignored`; AW-III scoped to fix the
"DTA viability" question that AW-II surfaced; AW-IV closed `1412/0/36`
with `0/17 parse entries exceed post-AU` (`AW/FINAL.md:373-379`); AW-V
closed `1597/0/36` with **same** `0/17 exceed post-AU` outcome
(`AW/FINAL.md:430`).

Four of the AW closes used the same "AX retains the cold-path replay
subsystem" handoff language (`AW/FINAL.md:407, 456`) — **the same
deferral text is reproduced verbatim across four passes**, indicating
the deferred items never landed.

### AX — closed scope-cut at W1, 13 declared waves never executed

Source: `docs/tranches/AX/FINAL.md:1-30`.

> "Block B (W2-W14: parity CI gating, lever portfolio, e-graph
> rewriting, document-parallel) did not execute under AX's letter —
> the W1 absorb re-plan reframed AX as substrate-and-API closure,
> and the optimisation arc routes wholesale into AY (the BEAT-sonic
> tranche, opened from this close)." (`AX/FINAL.md:14-21`)

AX redefined its own scope mid-tranche. **Thirteen waves declared,
seven sub-waves of W0a executed, all of W2–W14 routed forward.**

### AY-I — closed with seven of eight gates MISS, recorded misses + relinquish

Source: `docs/tranches/AY-I/FINAL.md:135-150`.

> "twitter ≤ 1.15 × sonic | W8 | 3.995× (MISS) | AY-II/W1
> canada ≤ 1.20 | W8 | ≈ 3.07× (from value bench, MISS) | AY-II/W1
> citm ≤ 1.20 | W8 | ≈ 3.8× (MISS) | AY-II/W1
> 5-fixture geomean ≤ 1.20 | W8 | ≈ 3.7× (MISS) | AY-II/W1
> CSS / Sheets / BBNF preserve functional guarantees | W8 | CSS +
>   Sheets PANIC at fat-LTO HEAD | AY-II/W0 fixes transitively
> Structural scan as first-class same-path | W7 | surface
>   available, no consumer | AY-II/W0 retires
> twitter regressed: 746 → 616 → 548 MB/s (-27% W4→W6)"
> (`AY-I/FINAL.md:81-84, 144-149`)

Plus W7 "stalled on a rollback-invariant violation in
`TapeBuilder::note_push`" (`AY-I/FINAL.md:91-99`) and W8 "superseded
by AY-II".

### AY-II-I — never executed, all five planned waves DEFERRED in pending-debt ledger

Source: `docs/tranches/AY-II-I/AY-II-I.md:1-31`.

> "STATUS: SUPERSEDED-BY-AY-III + DEFERRED — 2026-04-27. Per the
> post-B7 audit cycle, AY-II-I.W1-W5 was never executed."
> (`AY-II-I/AY-II-I.md:3-5`)

Twelve numbered items in the pending-debt ledger, all routed to
later tranches (`AY-II-I/AY-II-I.md:14-31`).

### AY-III — DEFERRED, never opened

Source: `docs/tranches/AY-III/AY-III.md:1-12`.

> "STATUS: DEFERRED — 2026-04-27. Per the fifth /plan synthesis
> cycle, AY-III's tape-substrate verification is on a deprecating
> substrate." (`AY-III/AY-III.md:3-7`)

Durable AY-III gates were "absorbed" into AZ-I.W4 and AZ-II.W2
(`AY-III/AY-III.md:7-11`). **Whether they actually closed there is
auditable in §3 below — and the answer is "partly".**

### AZ-I — closed WITH RECORDED MISSES on per-bench performance

Source: `docs/tranches/AZ-I/FINAL.md:158-161, 184-225`.

| Gate | Status |
|---|---|
| 17-entry close-matrix bench at AU floor | MISSED (recorded; routed to BB.close) |
| samply fleet under `docs/benchmarks/profiles/AZ-I/W2-act/` | WAIVED |

> "Per AZ-I.md §Reversal rule 1 (wave-local 20% rule) the misses
> would normally trigger substrate reversal. Reversal would
> re-introduce dual codegen paths
> (`feedback_no-orthogonal-codepaths` violation). AZ-I closes WITH
> RECORDED MISSES on perf — mirrors AY-I FINAL precedent."
> (`AZ-I/FINAL.md:217-222`)

Per-bench JSON regression: canada -55.6% / citm -39.5% / twitter
-28.7% / BBNF self-parse -77.9% (`AZ-I/FINAL.md:189-205`). The
"close" is a list of misses justified as "BB.close handles it later".

### AZ-II — closed continuation handoff (the first honest close)

Source: `docs/tranches/AZ-II/FINAL.md:1-12`.

> "CLOSED AS CONTINUATION HANDOFF — cutover.A through cutover.M
> Phase 3 LANDED at master; cutover.N dispatched + halted at
> organizational usage limit; cutover.O.0 tooling preflight, O1
> builder transactions, O2 EBNF direct projection, O3a failure-
> baseline routing, O3 generated view purge, and O4 `Parsed<R>` /
> `TapeDirect` deletion LANDED. O5 did not close green, O6 did not
> run, and O7 did not run." (`AZ-II/FINAL.md:3-9`)

AZ-II is the only close in the sample that explicitly names itself
as a continuation, not a terminal close. The PROGRESS-SNAPSHOT puts
it more starkly:

> "cutover.O accumulated dirty main state, bodyless large commits,
> and mid-flight instruction migration."
> (`AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md` — paraphrased; exact
> source `AZ-III/waves/W0.md:127-129`)

### Roll-up

| Tranche | Close type | Carry-forward count | Successor absorbed all? |
|---|---|---:|---|
| AU | PARTIAL | 5 MISS + 1 DEFER + 5 PARTIAL | NO (AV punted again) |
| AV | substrate-only | 4 declared waves DEFERRED | NO (AW exploded x5) |
| AW-I | (no FINAL) | scope reveal absorbed at W2 | n/a |
| AW-II | recorded-miss | 50 fail + 67 ignored + 5 carry | NO (AW-III opened) |
| AW-III | replan | DTA viability question | NO (AW-IV) |
| AW-IV | recorded-miss | 0/17 + carry-triplet | NO (AW-V) |
| AW-V | recorded-miss | 0/17 + same triplet | NO (AX absorbed) |
| AX | scope-cut | 13 waves dropped | NO (AY) |
| AY-I | recorded-miss | 7/8 gates MISS, W7 stalled | NO (AY-II opened) |
| AY-II-I | DEFERRED | 12 items never executed | NO (AY-III opened) |
| AY-III | DEFERRED | never opened | NO (B-series + AZ) |
| AZ-I | recorded-miss | 1 MISS + 1 WAIVE | NO (AZ-II handles) |
| AZ-II | continuation handoff | O5 + O6 + O7 + audit substrate | **AZ-III is owning it now** |

**Pattern:** every tranche back to AU defers its hardest gate forward
into the next letter, and the next letter does not close it either.
AZ-II is the first close where the orchestrator stops pretending
otherwise. AZ-III's job is to halt the pattern, which means it must
NOT contain "if time" or "stretch" language for any item, and the
items in §2 below must each have a wave owner before AZ-III opens
its first source dispatch.

---

## 2. Chronic-deferral ledger (items that crossed 2+ tranche boundaries)

For each item: what it is, why it was deferred, current status, and
proposed AZ-III owner. Row order is by tranche-count crossed
(highest first).

| # | Item | First open | Tranches crossed | Status now | AZ-III owner |
|---|---|---|---|---|---|
| 1 | 17-entry bench matrix at AU floor / sonic-rs parity / lightningcss parity | AU.11 (`AU/FINAL.md:267`) | AU → AV → AW (5 passes) → AX → AY-I → AZ-I → AZ-II → **AZ-III** | NOT MEASURED post-AZ-II; AZ-I missed by 28-78% on JSON; AZ-II punted to "BB.close"; FINAL.md `AZ-II/FINAL.md:96-105` shows stale cutover.E placeholder | W4 (named) |
| 2 | `crates/tape/` deletion + `cargo build -p bbnf --no-default-features` green | AU/AV → "AW retains cold-path replay" (`AW/FINAL.md:407, 456`) → AX W0b deletion of interpreter ~78K LOC but tape kept → AY-I → AZ-I (tape retained for BBNF only) → AZ-II.O5 (BLOCKED `AZ-II/FINAL.md:71-78, 176`) → **AZ-III.W1** | crates/tape DELETED at `6a6ca1fd` but no-default-build/regen-check evidence stale | W1 (named) |
| 3 | BBNF self-host canonical generated parse (no `bootstrap_parser.rs`) | cutover.G (`AZ-II/FINAL.md:55`) → cutover.H/I/K/L/M kept the bootstrap_parser as bridge → AZ-II.O7 did not run → **AZ-III.W2** | bootstrap_parser still in production at `crates/core/src/grammar/mod.rs::parse()` | W2.4 (named) |
| 4 | `Parsed<R>` / `TapeDirect` removal | AU.4.1 11-tranche deferral closed for `ParsedGrammar` only (`AU/FINAL.md:138`) → AY-II planned → AY-III deferred → AZ-I (RuntimeView trait introduced) → AZ-II.O4 LANDED `c51f9742..8040bd69` (`AZ-II/FINAL.md:64`) | LANDED in production source. Generated still has imports. | none — done |
| 5 | json-prototype retirement | AS.4 `crates/json-prototype` deferred → AT → AU → AV → AW (used as proto-target for visitor lane) → AY-I.W2-act demoted to bench adjunct (`AZ-I/FINAL.md:97`) → AZ-II.W1.3 metadata audit | LANDED at AZ-I W2-act; W1.3 still asks for evidence | W1.3 (named, but redundant) |
| 6 | Stale-bench placeholder values in `post-*.json` files | AY-I → AZ-I `post-AZ-I.json` recorded misses (`AZ-I/FINAL.md:189-205`) → AZ-II `post-AZ-II.json` retains cutover.E placeholder (`AZ-II/FINAL.md:94-105`) → AZ-III.W4 | active drift; W4 must publish post-AZ-III.json | W4 (named) |
| 7 | Sheets parity + Sheets parse_simple SIGABRT | AU → AV → AW-V (Sheets 6-7 MB/s — walker fallback `AW/FINAL.md:428`) → AY-I (`AZ-I/FINAL.md:230-241` SIGABRT-blocked entries) → AZ-I (Sheets retained SIGABRT) → AZ-II.O3a S1 cohort routed → AZ-III.W2 | AZ-III.W2.3 owns | W2.3 (named) |
| 8 | CSS bootstrap SIGABRT under fat-LTO | AY-I.B3 documented; bench harness still fails → AZ-I.FINAL `AZ-I/FINAL.md:230-241` → AZ-II → AZ-III | AZ-III.W2 / W4 | W2.2 + W4.2 (split named) |
| 9 | EBNF activation | cutover.E deferred (`AZ-II/FINAL.md:53`) → cutover.M deferred (`AZ-II/FINAL.md:61`) → cutover.O2 LANDED (`AZ-II/FINAL.md:62`) | LANDED | none — done |
| 10 | StructDirect speculative parsing rollback | cutover.K → O1 LANDED (`AZ-II/FINAL.md:163-167`) | LANDED | none — done |
| 11 | Generated tape views purge | cutover.D scope reveal → O3 LANDED (`AZ-II/FINAL.md:171-173`) | LANDED | none — done |
| 12 | Direct-to-struct admission across all 9 grammars | AS.5 → AT (KvPair) → AU.1 (BBNF) → AV.0 (typed-materialization) → AW (visitor lane) → AY-II planned → AZ-I (3 grammars) → AZ-II.M (8 grammars) → AZ-II.O2 (9 grammars) | LANDED | none — done |
| 13 | CSP `shape_dict` no-op installation | AV.6.3 (`BBNF_SHAPE_DICT` emitted but no consumer) → AW.W4 lever — never activated → AY-I W6 retires `navigate_tape` but NOT `shape_dict` → AZ-II audit names it as **still no-op** (`AZ-III/AZ-III.md:65-66`) → AZ-III.W3.3 | unconsumed | W3.3 (named) |
| 14 | Silent `BoxedEnum` cyclic / heterogeneous fallback | AU.2.6 (colour aggregates → AV) → AV.0 closed primitive Bug 1; cyclic types still BoxedEnum → AW → AY-II → AZ-II audit names it (`AZ-III/AZ-III.md:65-66`) → AZ-III.W3.2 | active fallback | W3.2 (named) |
| 15 | Durable egraph/node/projection facts authority | AT.1 (general type resolver) → AU (typed payloads partial) → AV (GrammarProfile codegen) → AW (DTA/PSI substrate retired) → AY (W6 admission audit) → AZ-II audit names "facts under-consumed" → AZ-III.W3.1 | substrate exists, consumer drift | W3.1 (named) |
| 16 | Commit discipline retroactive repair on AZ-II terse commits | AZ-II.cutover.M onward (recent 75 commits since `53d3e6b2`) → SIX-AGENT-SYNTHESIS finding 7 (`AZ-III/audit/SIX-AGENT-SYNTHESIS-2026-04-30.md:21-23`) → AZ-III.W0.2 | message-only rewrite landed once at `codex/az-history-before-reword-20260430-114057`; precept tightened; **no scheme to verify subsequent commits land with bodies** | **MISSING explicit owner** (proposed §6) |
| 17 | Sibling-repo audit close (parse-that, pprint, gorgeous) | AY-I/AY-II → AZ-I → AZ-II audit only mentions parse-that test red status (`AZ-III/PROGRESS.md:42-43`) | parse-that + pprint clippy red, parse-that test red on `parse_that 0.3.3` published version | **MISSING explicit owner** (proposed §6) |

**Five items crossed five+ tranche boundaries** (rows 1, 2, 3, 7, 12).
Three items remain unowned in AZ-III's current plan (rows 16, 17, and
parts of 8). The remainder are owned but in waves whose scope is too
large to actually close them (rows 13–15 all live in W3, see §4).

---

## 3. AZ-II honesty check — does FINAL match what landed?

**Mostly honest, with three concrete mismatches between FINAL.md and
PROGRESS-SNAPSHOT.**

### Mismatch 1: hard-gate 1 status

`AZ-II/FINAL.md:71` says O5 hard gate 1:
> "CONTINUED IN AZ-III.W1 - O5 Reclose | `crates/tape` is absent.
> Later audit found the old no-default blocker stale-repaired,
> but the O5 close packet remains stale and regen drift remains
> active."

`AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md:70`:
> "1 | `crates/tape/` deleted; `cargo build -p bbnf --no-default-features` green | DEFERRED — ~10k cross-crate refs; gated on generated view purge plus Phase 4 (Parsed<R> / TapeDirect deletion)"

**These describe two different states.** Snapshot says "DEFERRED"
with ~10k refs. FINAL says crate "is absent". The rewrite at
`6a6ca1fd` `fix(runtime/tape): delete tape crate` happened **after**
both docs, and FINAL.md was retroactively updated to reflect crate
absence — but the snapshot's "10k cross-crate refs" claim was never
reconciled. Either the 10k refs were demolished in the deletion
sweep (true, per the long string of `fix(emitter/*-tape):` commits
between `15bd381a..6effcb0b`), or one of these two source-of-truth
docs is wrong. **The PROGRESS-SNAPSHOT is the older statement and
should be marked superseded.** AZ-III.W0.2 should add a footer to
the snapshot pointing to FINAL.md row.

### Mismatch 2: BA handoff point 1 status

`AZ-II/FINAL.md:84` says BA handoff point 1:
> "MET for the named four; terminal surface still partial"

`AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md:82-84`:
> "1 | All four grammars on direct-to-struct (JSON + CSS L4 +
> Sheets + BBNF) | MET — plus CSV / Math / BNF / CSS Pretty also
> on StructDirect (cutover.M); EBNF on StructDirect after cutover.O2"

**These say opposite things.** SNAPSHOT says MET unqualified plus
extras. FINAL says MET for four but "terminal surface still
partial" because "the codegen-emitted `BbnfBootstrap::parse`
self-host is still bridged by cutover.G's hand-written parser."
FINAL is more honest. SNAPSHOT should be tightened to PARTIAL.

### Mismatch 3: hard-gate 6 (17-entry matrix)

`AZ-II/FINAL.md:76`:
> "PARTIAL | Bench archive captured at `docs/benchmarks/post-AZ-II.json`
> (cutover.E-era placeholder plus later notes). Full refresh belongs
> to cutover.O after builder transactions, EBNF projection, and tape
> deletion."

`AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md:75`:
> "PARTIAL — cutover.H Phase 6 partial captured JSON/Sheets/BBNF
> (within ±2% of AZ-I close); CSS bootstrap + Sheets parse_simple
> SIGABRT pre-existing; full bench refresh deferred to cutover.N"

These are roughly consistent in status ("PARTIAL") but wildly
different in detail. Snapshot says "within ±2% of AZ-I close" —
that is **a measured comparison that does not exist** in
post-AZ-II.json (which is the cutover.E placeholder per FINAL).
The "within ±2%" claim is at minimum unsourced; at maximum
fabricated. AZ-III.W0.2 should grep for this number and either cite
it or strike it.

### Should-be-PARTIAL gates currently marked MET

- **Hard gate 4 — StructRegistry non-empty for every Named rule**
  (`AZ-II/FINAL.md:74`): MET. Verifiable. OK.
- **Hard gate 5 — Parity harnesses recoded** (`AZ-II/FINAL.md:75`):
  MET. OK.
- **Hard gate 7 — AZ-II interim manifest exists**
  (`AZ-II/FINAL.md:77`): "MET (interim form)". This is honest.
- **BA handoff 3 — `StructRegistry` closed fleet-wide**
  (`AZ-II/FINAL.md:86`): MET — accurate.
- **BA handoff 6 — BBNF self-parse byte-reproducible**
  (`AZ-II/FINAL.md:89`): MET. **This claim is partially compromised
  by the fact that "BBNF self-parse still routes through
  `bootstrap_parser`, not generated self-host parse"**
  (`AZ-II/FINAL.md:84`). Byte-reproducibility holds for regen
  output, but the *self-parse* the gate names is bridged. Either
  rename gate 6 or downgrade to PARTIAL.

### Recently-rewritten commit messages misrepresent landed state

Commit `dcb41e67 refactor(lower/view-walk): rename tape traversal helpers`
body says (`git log -1 dcb41e67`):
> "Evidence: message-only AZ-III W0 history repair; the tree for
> this commit is unchanged by the reword. Remaining parity,
> authority, and benchmark proof is routed through AZ-III W2-W4."

This is correct discipline (it admits the body is post-hoc). But
the body itself does not say what the original commit *did* — it
says what general class of work it belongs to. **That is a
template.** Across the AZ-II-implementation rewrite span, the
bodies are formulaic and non-specific. The repair plan
(`AZ-III/audit/W0-commit-repair-plan.md:36-46`) says future commits
must do better; the **rewritten** commits do not.

This is not a charge against the W0 work, but it is a missing
audit row: AZ-III's commit-discipline gate (W0) was satisfied for
commit-message *form*, not for commit-message *truth*. **Proposal
§6.A:** AZ-III opens a `W0.5 Commit Body Truth Pass` that picks
the 8–12 highest-impact AZ-II implementation commits and rewrites
each body with the actual file-level evidence (LOC delta, test
delta, gate impact). This is not history rewrite for its own sake;
it is a bounded follow-on for the AZ-II commit drift the
SIX-AGENT-SYNTHESIS named.

---

## 4. AZ-III plan vs evidence — wave-by-wave

### W0 — Quarantine and Dispatch Repair

Source: `docs/tranches/AZ-III/waves/W0.md`.

**Aligned:** Yes. Every scope item maps to a real artefact already
landed (state ledger at `AZ-III/audit/W0-state-ledger.txt`; commit
repair plan at `W0-commit-repair-plan.md`; dispatch packets at
`W0-dispatch-packets.md`). Hard gate 1–5 each cite an artefact path.

**Gaps identified:**
- Item 6 in scope ("Audit recent bodyless commits and decide
  whether history rewrite, follow-up provenance commits, or archival
  commit notes are the chosen repair path") was satisfied by the
  message-only rewrite at backup branch
  `codex/az-history-before-reword-20260430-114057`. But the rewrite
  produced **template bodies, not evidence-bearing bodies** (see
  §3 above). The wave does not have a sub-gate that verifies body
  *content*. **Proposed addition: W0.5 sub-unit "Commit Body Truth
  Sample"** — pick 8 commits randomly across the rewritten range,
  verify each body cites a concrete file path or commit-statistic
  (LOC, test count, function name). If <50% pass, re-author the
  bodies.
- W0 file-bounds row "`docs/precepts` | modify pointer only in
  parent; source edits happen in submodule root" reflects a
  submodule structure that is partially-migrated per
  `docs/precepts-plan.md` (untracked, mentioned at
  `AZ-III/audit/W0-state-ledger.txt:32`). W0.3 is supposed to close
  this, but its sub-gate language ("no half-migrated instruction
  path remains active") is too soft. **Proposed strengthening: add
  hard gate 6 to W0** — `rg "docs/instructions/tranche|458c2d1|six-agent"`
  over live source returns zero hits, archived to
  `W0-precepts-migration-scan.txt`.
- W0 does not own **sibling-repo audit close** (parse-that, pprint
  clippy/test red status documented at
  `AZ-III/PROGRESS.md:38-43`). **Proposed addition: W0.6 Sibling
  Repo Triage Packet** (this is also missing from §6).

### W1 — O5 Reclose

**Aligned:** Yes. Tight scope, four narrow agent units, four
verification artefacts cited. Hard gate is reproducible
(`cargo xtask regen --check`, `cargo build -p bbnf --no-default-features`,
`cargo metadata`, deletion grep). This is the best-formed wave.

**Gap:** Hard gate 4 says
> `rg -n "crates/tape|json-prototype|tape::|Tape|Gorgeous JIT|A1"`
> over live source and active docs is archived; every hit is either
> deleted, an allowed historical archive, or leaves W1 - O5 Reclose
> blocked.

`Tape` is too broad — every Rust trait or type called `Tape*` will
match (e.g. `TapeKind` lingers in some legacy comments;
`gorgeous::JIT` may not exist anymore). The grep needs to be
regex-tightened to avoid false hits and to require **all hits be
non-source** (docs/archive-only or zero). **Proposed wording in §8.**

**Duplicated work:** none with later waves.

### W2 — Semantic Parity and Bootstrap Canonicalization

**Aligned:** Yes. Four agent units (JSON / CSS / Sheets / BBNF
parity); hard gate ties to focused test commands; verification
artefacts named.

**Gaps:**
- W2.4 BBNF Bootstrap Canonical Path scope says "replace canonical
  `bootstrap_parser` routing with generated self-host parse if
  green; otherwise produce blocker proof". This is fine, but the
  **"otherwise"** clause permits closure-with-blocker. Across the
  audit, "close-with-blocker" is the AY/AZ-I/AZ-II pattern that
  this REAUDIT exists to halt. **Proposed: tighten to "if not
  green, AZ-III remains blocked. No same-tranche redress wave is
  permitted to close W2 with `bootstrap_parser` retained."**
- W2 hard gate 5 grep `rg -n "return Ok\\(|#[ignore]|bootstrap_parser"`
  over parity tests — the `#[ignore]` regex is wrong (the brackets
  are inside a character class; `\\[` should be used). **Trivial
  fix in §8.**
- **Duplicated commitment:** W2.4 BBNF self-host vs W3.4 Projection
  Consumption both touch BBNF projection. W2.4 is parity-test driven;
  W3.4 is emitter-fallback driven. They share files
  (`crates/core/src/grammar/generated/bbnf.rs`). **W2.4 should
  declare BBNF parity test as the close gate and explicitly defer
  emitter-fallback removal to W3.4. §8 patch.**

### W3 — Fact, Type, CSP, and Projection Authority

**Aligned:** Partially. Scope is coherent but **vastly larger than
any other AZ-III wave**:

- W3.1 Durable Fact Authority — egraph, recognizer, node, projection
  facts (all of `crates/ir/src/egraph/`,
  `crates/ir/src/passes/recognizers/`, the projection IR layers)
- W3.2 Type Obligation Solver — replace BoxedEnum fallback (touches
  `crates/ir/src/passes/types/`)
- W3.3 CSP Strategy Globalization — `shape_dict` install + consumer
  (`crates/ir/src/passes/csp_strategy/`,
  `crates/ir/src/constraints/shape_dict.rs`)
- W3.4 Projection Consumption — wire authority into StructDirect
  (`crates/core/src/backend/rust/emitter/**`)

Each of these is a sub-tranche. Together they touch IR + emitter +
generated output. The wave file-bounds row
(`docs/tranches/AZ-III/waves/W3.md:24-39`) lists eight directories
with `modify` or `modify-carve` access. **This is a tranche, not a
wave.**

**Over-reach:** W3.1 + W3.4 both name "egraph/node/projection facts"
as their substrate. They are different abstraction layers but they
share consumer churn. If one fails to land, the other becomes a
substrate-only wave (which §1 names as the AY-era anti-pattern).

**Mitigation: W3 must split.** Proposal §7.1.

**Cross-wave conflict with W2:** W3.4 says "wire authority into
StructDirect projection/emission, including unsupported variants
that currently route through dispatcher fallback." Files
`crates/core/src/backend/rust/emitter/**` are the SAME files W2's
parity work edits ("modify-carve for proven parity root causes").
**W2 and W3.4 will race on `shapes/` and `alt_dispatch/`.** §5
elaborates.

### W4 — Benchmark, Profile, and Workspace Truth

**Aligned:** Yes for measurement. Mostly aligned for hard gate.

**Gaps:**
- W4.1 Workspace and Structural Audits scope is coarse: "run
  nextest/build/regen/static scans". The W0-state-ledger already
  noted that `cargo iter-test` fails fast on `bootstrap_full_parse`
  (`AZ-III/audit/W0-state-ledger.txt:46-50`). W4 must NOT silently
  inherit a workspace-test failure; it must require workspace-green
  before measurement. **Proposed: add hard gate that workspace
  nextest is non-zero-fail.** §8.
- W4.2 / W4.3 split is sound. W4.4 Profile Truth has no hard-gate
  threshold; "profile files are archived and referenced by W5" is
  the entire gate. This is too soft for a measurement wave. **No
  threshold means no measurement floor;** the wave can close on a
  single profile that nobody reads. Suggested addition: each named
  regression that exceeds Y% over post-AU baseline must produce a
  profile that names the top-3 self-time symbols. §8.
- The `Makefile` / `.cargo/config.toml` / `scripts/*` file bounds
  show W4 will edit dev infrastructure (`prebuild-benches.sh`,
  `prepare-profile-wave.sh`, `profile-bench-headless.sh`). **This
  is the dev-loop iteration speedup item that the user's
  `feedback_build_infra_first` precept says should land FIRST in
  any tranche where dev iteration time is a bottleneck.** Putting
  it at W4 (after W1/W2/W3 source work) violates the precept. The
  Lane 6 audit (per the brief) will detail; this lane flags it.
  **Proposal: split W4 dev-loop infra into a W0.7 sub-unit and
  surface it before W1.** §7.4.

### W5 — Terminal Close and Handoff

**Aligned:** Yes. Four agent units; close conversion; archive plan.

**Gap:** scope item 4 ("Archive or delete stale active docs only
after inbound links are rewritten") names `meta-audit/` and
`next-tranche-research/` for deletion. These directories were AZ-I
audit substrate. **The deletion is not bounded:** the wave does not
say what becomes of the audit research that was load-bearing for
the AZ thesis. Proposed: explicit triage table — each `meta-audit/*.md`
and `next-tranche-research/*.md` file is either (a) archived to
`docs/archive/`, (b) deleted with the inbound-link rewrite proven,
or (c) retained because it has a live consumer. Closing without
that triage repeats the "absorbed" pattern of AY-II / AY-III.
§8 patch.

### Summary

| Wave | Aligned with audit findings? | Over-reach? | Gaps named in §8? |
|---|---|---|---|
| W0 | Yes | none | yes (W0.5 / W0.6 / W0.7) |
| W1 | Yes | none | yes (regex tightening) |
| W2 | Yes | none | yes (W2.4 close strictness) |
| W3 | Partial | **yes — split** | yes (split + bounds) |
| W4 | Partial | none, but soft gates | yes (gates + dev-loop split) |
| W5 | Yes | none | yes (archive triage) |

---

## 5. Wave-bound conflicts (file-bound and stretch-language audit)

### Overlapping file bounds between waves

**W2 vs W3.4 — `crates/core/src/backend/rust/emitter/**`**

W2 claims (`waves/W2.md:31`):
> "`crates/core/src/backend/**` | modify-carve for proven parity
> root causes"

W3 claims (`waves/W3.md:32`):
> "`crates/core/src/backend/rust/emitter/**` | modify-carve"

These are nested. Per `feedback_agent_orchestration` ("Never let
sub-agents race on shared files; commit before parallelizing"), W2
and W3 cannot both be in flight on `emitter/**` at the same time.
The W2 prerequisite says "Opens after W1 - O5 Reclose"; the W3
prerequisite says "Opens after W1 - O5 Reclose, and may run in
parallel with W2 - Semantic Parity and Bootstrap Canonicalization
after W0 - Quarantine and Dispatch Repair only when file bounds are
disjoint." The "only when file bounds are disjoint" caveat is
**stated, but the file-bound declarations are NOT disjoint**.

**Resolution:** Either (a) W2 declares its emitter access as
`modify-carve for shapes/{flat,wrap,keyword}/struct_direct.rs ONLY`
and W3.4 takes everything else under `emitter/` (preferred); or (b)
W3.4 sequences AFTER W2. §8 patches.

**W2 vs W3.2 — `crates/core/src/grammar/**`**

W2 claims `crates/core/src/grammar/**` (modify-carve), `bootstrap_parser.rs`
(modify/delete if replaced), and `grammar/mod.rs` (modify-carve).
W3 does not name `crates/core/src/grammar/**` in its file bounds
**but** W3.2 Type Obligation Solver inevitably edits `crates/ir/src/passes/types/`
which itself reads from grammar IR. Cross-crate boundary; should be
fine. No actual conflict.

**W3 vs W4 — `crates/core/src/grammar/generated/*.rs`**

W3 claims (`waves/W3.md:34`):
> "`crates/core/src/grammar/generated/*.rs` | modify after regen only"

W4 doesn't claim generated files but its "structural audits" (W4.1
sub-gate) inspect them. Read-only is fine. No conflict.

### Wave with no consumer-paired substrate

**W3.1 Durable Fact Authority** says the substrate is "egraph,
recognizer, node, and projection facts." The sub-gate is "one
production emitter/layout consumer fails without the fact." That is
a consumer requirement. OK.

**W3.3 CSP Strategy Globalization** says `shape_dict::install` is
"no longer a no-op." The sub-gate is "emitted decisions cite CSP
facts". That is a consumer requirement. OK.

**W3.4 Projection Consumption** wires authority into emitters.
Self-evident consumer.

**W3.2 Type Obligation Solver** says obligations replace silent
fallback. Sub-gate is "tests cover EBNF/CSS-like heterogeneous
alternations and recursive rules without silent fallback." That is
a test requirement, not a production-consumer requirement.
**Proposed: add explicit production consumer name** — e.g., the
`payload/layout.rs` planner consumes the type-obligation result.
§8.

### "If time" / "stretch" / soft language scan

`grep -n "stretch\|if time\|nice to have\|consider\|optional\|opportunity" docs/tranches/AZ-III/waves/W*.md`:

- W0.md: zero hits.
- W1.md: zero hits.
- W2.md: zero hits.
- W3.md: zero hits.
- W4.md: zero hits.
- W5.md: zero hits.

**Good.** The waves do not use stretch language.

But there is a soft "or" pattern: **W2's gate 5** says "any active
masking or fallback **keeps W2 blocked**", and **W3's gate 5** says
"no compatibility shim closes the wave." **W4's gate 4** says
"`rg -n "NOT_MEASURED|placeholder|post-AZ-II|TBD"` over
`post-AZ-III.json` and W4 artefacts is archived with no active
hits." All three are firm.

Soft hits (require attention but not stretch):
- W2 scope item 4: "If that cannot close inside W2 ..., W2 closes
  blocked and opens a same-tranche redress wave; it does not
  retain a silent bootstrap fallback." (`waves/W2.md:14-18`)
  This permits a same-tranche redress wave. Per AZ-III invariant 1
  ("Continuation, not deferral"), this is fine because it routes
  to a named wave, not to AZ-IV. But the wave file bounds for the
  redress wave are not pre-declared. Proposal §8 — add a
  pre-declared `W2-Z` redress shell so the orchestrator does not
  re-author bounds at the failure moment.

### Roll-up

| Conflict / soft-spot | Severity | Proposal |
|---|---|---|
| W2 vs W3.4 file bounds overlap on `emitter/**` | HIGH | §7.1 split + §8 bounds patch |
| W3.2 missing production consumer name | MED | §8 sub-gate addition |
| W2.4 close-with-blocker too permissive | MED | §8 wording tighten |
| W4.4 no profile floor threshold | MED | §8 sub-gate addition |
| W4 dev-loop scripts ordered after source work | HIGH (precept violation) | §7.4 reorder |
| W2 same-tranche redress wave has no pre-declared bounds | LOW | §8 W2-Z shell |
| W5 archive triage unbounded | LOW | §8 triage table |

---

## 6. Missing AZ-III items

### A. Commit Body Truth Pass on already-landed AZ-II implementation commits

**Status:** PARTIAL. The W0 commit-repair-plan.md landed
message-only rewrites with template bodies. SIX-AGENT-SYNTHESIS
finding 7 (`AZ-III/audit/SIX-AGENT-SYNTHESIS-2026-04-30.md:21-23`)
named "concrete scopes and evidence-bearing bodies" as the
discipline; the rewrite achieved the *scopes* but the *bodies* are
templates (see §3 above).

**Proposed AZ-III owner:** new W0.5 sub-unit (§8 patch).

### B. Sibling-repo audit close (parse-that, pprint, gorgeous)

**Status:** OPEN. AZ-III.PROGRESS notes "Root, parse-that, and
pprint format checks are green. Root compile passes. Root tests,
root clippy, parse-that tests/clippy, and pprint clippy are red"
(`AZ-III/PROGRESS.md:38-43`). The W0 state ledger names the issues
in detail (`AZ-III/audit/W0-state-ledger.txt:38-43`).

**No AZ-III wave names this scope.** The waves all carve "Do NOT
touch: BA/BB source, path APIs, rewrite inference, optimization
work" — none lists sibling-repo cleanup. **Proposal §8: W0.6 +
explicit cap.**

### C. Dead-code / legacy purge as a named wave item

**Status:** SCATTERED. The W1 "deletion and metadata audit" closes
tape/json-prototype residue. But broader legacy purge — gorgeous
JIT (deleted at `6e7a57c5`), visitor surfaces (deleted across many
commits), and DTA artefacts (`f4b01184`) — happened **as part of
the AZ-II implementation slice**, not as a named AZ-III wave. The
chronicled commits `dc1999ed..6e7a57c5` (visitor + JIT deletion)
are not in any AZ-III wave; they're part of "the dirty source
slice" the W0 state ledger names.

**The remaining legacy that *is* AZ-III's:** dead-code from
generated/, dead helpers in `crates/core/src/runtime/dta/`, post-tape
cleanup of `crates/core/src/runtime/`. Proposal §8: W1.5 sub-unit
"Dead Code Purge" with an explicit grep over the post-deletion
codebase.

### D. Build/test/bench iteration speedup (Lane 6 will detail)

**Status:** PARTIAL. W0's hard gate uses `cargo iter-check` and
`cargo iter-test` which are existing aliases. W4's file bounds
include `Makefile`, `.cargo/config.toml`, `scripts/*.sh`. But these
are *modify-carve* under W4 (the bench wave), not under W0 (the
quarantine wave that opens FIRST).

**Per `feedback_build_infra_first` precept:** "Build/test
infrastructure improvements land FIRST in any tranche where dev
iteration time is a bottleneck — never deferred to later waves."

The W0-state-ledger noted `cargo iter-test` fails fast and only ran
202/1509 tests. That is a dev-loop bottleneck. **Proposal §7.4:
split bench-script work out of W4, add it as W0.7 (or new W0.b
dispatch) so all subsequent waves consume the improvement.**

### E. SIX-AGENT-SYNTHESIS items not visibly placed in a wave

The SIX-AGENT-SYNTHESIS lists seven findings
(`AZ-III/audit/SIX-AGENT-SYNTHESIS-2026-04-30.md:8-23`). Mapping:

| Finding | Wave |
|---|---|
| 1 — AZ-II open, not terminal | W1+W5 |
| 2 — AZ-III continuation, not deferral | invariant |
| 3 — O5/O6/O7 must close before BA/BB | W1+W2+W4+W5 |
| 4 — Grammar-general authority is the legitimate substrate axis | W3 |
| 5 — Bootstrap canonicalization, parity, O5 evidence, bench truth as close blockers (not optional) | W1+W2+W4 |
| 6 — Current dirty main blocks dispatch | W0 |
| 7 — Commit discipline drift | W0 (insufficient — see A above) |

Six of seven map cleanly. Finding 7 is partial (see A).

### F. CSP optimization is foundational (per `feedback_csp_always_optimize` precept)

The user's precept says "CSP optimization is always high priority;
foundational library, not gated by profile share." AZ-III.W3.3
treats CSP as one of four sub-units inside W3. **It is named, but
not foundational-priority.** This is a soft drift. Proposal §8: in
the W3 split (§7.1), CSP gets its own wave with parallel-eligibility
to W2. This also resolves the over-reach in W3.

---

## 7. Reorder / merge / split proposals (concrete, no "consider")

### §7.1 Split W3 into three waves: W3a, W3b, W3c

**Current W3:** Fact + Type + CSP + Projection Authority — four
sub-units, eight directories, single wave. Over-reach per §4.

**Proposal:**
- **W3a — Type Obligation Solver** (current W3.2). Files:
  `crates/ir/src/passes/types/**`, `crates/ir/tests/**`,
  `crates/core/tests/types_*.rs`. Hard gate: BoxedEnum fallback
  removed; cyclic and heterogeneous alternation tests cover real
  EBNF/CSS shapes.
- **W3b — CSP Strategy Globalization** (current W3.3). Files:
  `crates/ir/src/passes/csp_strategy/**`,
  `crates/ir/src/constraints/shape_dict.rs`,
  `crates/ir/src/passes/recognizers/**` (carve), tests. Hard gate:
  `shape_dict::install` is no-op-free; emitter dispatch cites CSP
  facts. **Eligible for parallel with W3a** if file-bounds
  disjoint, which they are.
- **W3c — Fact Authority + Projection Consumption** (current W3.1
  + W3.4). Files: `crates/ir/src/egraph/**`,
  `crates/core/src/backend/rust/emitter/**`,
  `crates/core/src/grammar/generated/*.rs` (after regen). Hard
  gate: each fact has a production consumer; StructDirect emitter
  fallbacks deleted; EBNF/CSS/Sheets/BBNF projection tests fail
  without authority.

**Sequencing:** W3a + W3b parallel after W1; W3c after both.

### §7.2 Resolve W2 + W3c emitter race

**Decision:** W2 declares emitter access as carve `shapes/{flat,
wrap,keyword,alt_dispatch}/struct_direct.rs ONLY` (parity-driven
fixes). W3c (current W3.4) takes everything else under
`backend/rust/emitter/**` (authority-driven fallback removal).

**Sequencing:** W2 before W3c. §8 file-bounds patch encodes this.

### §7.3 Move dev-loop infra from W4 to a new W0.b dispatch

**Decision:** Pull `Makefile`, `.cargo/config.toml`, and
`scripts/{prebuild-benches,prepare-profile-wave,profile-bench-headless}.sh`
out of W4's file bounds. Open a parallel `W0.b — Dev Loop Truth`
sub-dispatch under W0 that lands before W1 dispatches.

**Rationale:** the W0 state ledger names cargo iter-test fail-fast
as the active blocker; W1+ all consume that command. Per
`feedback_build_infra_first`, this must precede source work.

### §7.4 Add W0.5 Commit Body Truth and W0.6 Sibling Repo Triage

**Decision:** W0 grows two sub-units already named in W0's "scope"
list but not split into agent-units. Concrete additions in §8.

### §7.5 No merges proposed

The opposite problem (waves with too-narrow scope) is not present.
W1 is narrow but appropriately so (single hard-gate cluster).

---

## 8. Wave-spec patches (exact text)

### W0.md — add W0.5, W0.6, W0.7 agent units

After `### AZ-III.W0.4 Dispatch Packet Authoring` (line 65–69), insert:

```markdown
### AZ-III.W0.5 Commit Body Truth Sample

- Mechanism: pick eight commits at random across the AZ-II rewrite
  range `53d3e6b203ca4d5e1b5e34c06e05d867518ae0a5..HEAD`. For each,
  verify the body cites a concrete file path, a measurable LOC or
  test delta, or a named hard-gate row. If fewer than five of the
  eight pass, re-author the failing bodies with file-level evidence
  (no template language, no "remaining proof routed forward" alone).
- Files: git history; commit messages only. No tree edits.
- Sub-gate: `audit/W0-commit-body-truth-sample.txt` archives the
  eight commits, the verification per commit, and a final pass/fail
  count >= 5/8.

### AZ-III.W0.6 Sibling Repo Triage Packet

- Mechanism: classify parse-that and pprint clippy/test red surfaces
  into (a) AZ-III source dependency, (b) sibling-repo independent
  cleanup, (c) blocker on published `parse_that 0.3.3` registry
  pin. Author dispatch packets only for class (a). Class (b) is
  archived as `docs/tranches/AZ-III/audit/W0-sibling-repo-deferred.md`
  with named owner. Class (c) requires a sibling-repo release before
  AZ-III may close.
- Files: `docs/tranches/AZ-III/audit/W0-sibling-repo-*.md` only.
  Read-only on sibling repo source.
- Sub-gate: every red surface in
  `audit/W0-state-ledger.txt:38-50` is classified.

### AZ-III.W0.7 Dev Loop Truth

- Mechanism: triage `Makefile`, `.cargo/config.toml`, and
  `scripts/{prebuild-benches,prepare-profile-wave,profile-bench-headless}.sh`
  for any blocker that breaks `cargo iter-check`, `cargo iter-test`,
  or `cargo xtask regen --check` after the AZ-II implementation
  slice is reconciled. Land repairs that unblock subsequent waves.
  Do NOT add new bench harness work; that is W4's scope.
- Files: `Makefile`, `.cargo/config.toml`, `scripts/*.sh` (modify-carve).
  No source-code edits.
- Sub-gate: `cargo iter-check`, `cargo iter-test`, and
  `cargo xtask regen --check` complete without infrastructure errors
  on a clean tree. Test failures from source defects route to W1+;
  infrastructure failures close here.
```

In `## File Bounds` (lines 24-36) replace the existing table to add:

```markdown
| `Makefile` | modify-carve (W0.7 only) |
| `.cargo/config.toml` | modify-carve (W0.7 only) |
| `scripts/{prebuild-benches,prepare-profile-wave,profile-bench-headless}.sh` | modify-carve (W0.7 only) |
```

In `## Hard Gate` (lines 81-94) add:

```markdown
6. `rg "docs/instructions/tranche|458c2d1|six-agent|6 agents"`
   over live source AND `docs/precepts/` returns zero active hits;
   archived to `W0-precepts-migration-scan.txt`.
7. `audit/W0-commit-body-truth-sample.txt` archives a sampled
   verification of the AZ-II rewrite range with >= 5/8 pass.
8. `audit/W0-sibling-repo-classification.md` classifies every
   sibling-repo red surface with named owner.
9. `cargo iter-check && cargo iter-test --no-run` complete without
   infrastructure errors on the close tree.
```

### W1.md — tighten metadata grep regex

Replace line 81-84:

```markdown
4. `rg -n "crates/tape|json-prototype|tape::Tape|::Tape|TapeBuilder|TapeCursor|TapeRec|TapeOffset|TapeKind|TapeDirect|gorgeous_jit|GorgeousJit"`
   over live source returns zero hits, and the same scan over
   active docs is archived; every doc hit is either an explicit
   archived-history reference or leaves W1 - O5 Reclose blocked.
```

(Adds explicit symbol list; removes overbroad bare `Tape`.)

Also add hard gate 5:

```markdown
5. `rg -n "TapeKind|payload_idx|sib_skip|tape_walk"` over live source
   returns zero hits in production paths; any hit in `docs/archive/`
   or `crates/core/benches/json-prototype/` is acceptable.
```

(Catches dead-code residue named in §6.C.)

### W2.md — tighten W2.4 close-with-blocker; fix grep regex

Replace `## Scope` item 4 (line 14-19):

```markdown
4. Prove BBNF generated self-hosting is the canonical parse path.
   `crates/core/src/grammar/mod.rs::parse()` must route through
   the generated `BbnfBootstrap::parse`, not through
   `bootstrap_parser.rs`. If that proof cannot land inside W2 -
   Semantic Parity and Bootstrap Canonicalization, W2 closes
   blocked. AZ-III remains blocked. No same-tranche redress wave
   may keep `bootstrap_parser.rs` in production routing.
```

Replace hard gate 5 (line 97-99):

```markdown
5. `rg -n 'return Ok\\(|#\\[ignore\\]|bootstrap_parser'` over
   parity tests, grammar routing, and `crates/core/src/grammar/`
   is archived. Any active masking, ignore, or `bootstrap_parser`
   call in production routing keeps W2 blocked.
```

Adjust W2 file bounds (line 22-44) — replace
`crates/core/src/backend/**` row with:

```markdown
| `crates/core/src/backend/rust/emitter/shapes/{flat,wrap,keyword,alt_dispatch}/struct_direct.rs` | modify-carve for proven parity root causes |
| other `crates/core/src/backend/**` | read-only (W3c owns) |
```

### W3.md — split into W3a / W3b / W3c

Per §7.1, this requires three new wave files. Concretely:

1. Rename current `W3.md` to `W3c.md` (Fact + Projection).
2. Create `W3a.md` (Type Obligations) — file bounds:
   `crates/ir/src/passes/types/**`, `crates/ir/tests/**`,
   `crates/core/tests/types_*.rs`,
   `crates/core/tests/projection_obligation_*.rs`.
3. Create `W3b.md` (CSP Globalization) — file bounds:
   `crates/ir/src/passes/csp_strategy/**`,
   `crates/ir/src/constraints/shape_dict.rs`,
   `crates/ir/src/passes/recognizers/**` (modify-carve),
   `crates/ir/tests/**`.
4. `W3c.md` keeps egraph + projection consumption + emitter
   fallback removal.

Update `AZ-III.md:74-80` Wave Table to list W3a / W3b / W3c
(replacing the single W3 row).

Update `AZ-III/PROGRESS.md:23-32` Wave Status table similarly.

In each new W3a / W3b / W3c spec:
- Hard gate cites named production consumer (W3.2 missing piece).
- W3a: consumer is `payload/layout.rs` planner.
- W3b: consumer is `EmitStrategy::for_grammar` resolver via
  emitted CSP fact.
- W3c: consumer is the StructDirect per-shape emitter fallback
  paths.

### W4.md — add profile threshold; remove dev-loop file bounds

Replace `### AZ-III.W4.4 Profile Truth` (line 60-63):

```markdown
### AZ-III.W4.4 Profile Truth

- Mechanism: capture profiles for any 17-entry matrix row that
  regresses more than 10% versus the AU baseline or 5% versus
  AZ-I. Each profile names the top-3 self-time symbols and an
  attribution narrative.
- Files: profile artifacts.
- Sub-gate: profile files are archived under
  `docs/benchmarks/profiles/AZ-III/`; each named regression has a
  profile referenced from W5 - Terminal Close and Handoff.
```

Remove from `## File Bounds` (lines 22-32) the rows:
- `Makefile`
- `.cargo/config.toml`
- `scripts/prebuild-benches.sh`
- `scripts/prepare-profile-wave.sh`
- `scripts/profile-bench-headless.sh`

(They moved to W0.7 per §7.3.)

Add to `## Hard Gate` (lines 76-87) after gate 5:

```markdown
6. Workspace nextest at the W4 measurement floor: pass/fail counts
   archived; any new failures versus the W0.7 baseline keep W4
   blocked.
```

### W5.md — explicit archive triage table

After `### AZ-III.W5.3 Archive/Delete Plan` (line 55-58), append:

```markdown
Each candidate file in `docs/tranches/meta-audit/` and
`docs/tranches/next-tranche-research/` must be classified in the
AZ-III FINAL.md archive triage table:

| File | Disposition | Rationale | Inbound link rewrite |
|---|---|---|---|
| (every `*.md` under those two dirs) | `archive` / `delete` / `retain` | one sentence | proven by `rg "<filename>"` returning zero or rewritten only |

Closing without that table repeats the AY-II / AY-III "absorbed"
pattern.
```

### AZ-III.md — Wave Table refresh

Replace lines 73-80 with:

```markdown
| Wave | Agents | Closes on | Status |
|---|---:|---|---|
| [W0 - Quarantine and Dispatch Repair](waves/W0.md) | up to 10 parallel | clean state, commit/orchestration repair, dev-loop infra, sibling-repo triage, AZ-II handoff docs, dispatch packets | in_progress |
| [W1 - O5 Reclose](waves/W1.md) | up to 10 parallel | O5 close packet green: regen, no-default build, metadata, deletion scans | planned |
| [W2 - Semantic Parity and Bootstrap Canonicalization](waves/W2.md) | up to 10 parallel | semantic parity and generated BBNF canonical path proof | planned |
| [W3a - Type Obligation Solver](waves/W3a.md) | up to 5 parallel | no silent BoxedEnum fallback for cycles or heterogeneous joins; payload/layout consumer | planned |
| [W3b - CSP Strategy Globalization](waves/W3b.md) | up to 5 parallel | shape_dict installed and consumed by emitter strategy | planned |
| [W3c - Fact Authority and Projection Consumption](waves/W3c.md) | up to 10 parallel | egraph/projection facts authoritative; StructDirect fallback removed | planned |
| [W4 - Benchmark, Profile, and Workspace Truth](waves/W4.md) | up to 10 parallel | workspace, structural, profile, and 17-entry benchmark truth | planned |
| [W5 - Terminal Close and Handoff](waves/W5.md) | up to 10 parallel | terminal AZ close docs, BA/BB handoff, archive triage | planned |
```

---

## 9. AZ-III thesis check — does the two-duty thesis hold?

`AZ-III/AZ-III.md:28-33` says AZ-III has two duties:

1. finish AZ-II terminal close work that was not honestly green;
2. land grammar-general authority substrate audits show is required.

**Both duties are real.** The chronic-deferral ledger §2 confirms
duty 1 is unavoidable. Duty 2 is named by SIX-AGENT-SYNTHESIS
finding 4 and aligns with the chronic items 13–15.

**Is there a third axis?** Three candidates:

### Candidate axis 3a — Build/test/bench iteration speedup

§6.D and `feedback_build_infra_first` argue this is foundational.
But it is a means to enable duties 1 and 2, not a separate goal.
**Verdict: NOT a third axis.** Fold into W0.7 (already proposed).

### Candidate axis 3b — Commit discipline retroactive enforcement

§3 and finding 7 of SIX-AGENT-SYNTHESIS name commit discipline
drift. The W0 message-only rewrite is a partial fix; W0.5
(proposed §8) closes it. **Verdict: NOT a third axis.** It is
process scaffolding for both duties.

### Candidate axis 3c — Stop-the-bleed on close discipline (the AY/AZ pattern)

§1's tranche-by-tranche audit shows nine consecutive tranches that
"close with recorded misses" — AU through AZ-I, plus AW's five
sub-passes. AZ-II is the first honest close. AZ-III's INVARIANT 1
("Continuation, not deferral") is the rule that breaks the
pattern, but **the rule is invariant, not a wave.**

**Verdict on third axis:** The chronic close-discipline pattern is
real, but AZ-III addresses it through invariants, not through a
fourth content-bearing axis. The two-duty thesis stands. The
**process** axis (commit discipline + dev-loop infra +
close-honesty invariants) is correctly placed in W0 + W5 + the
invariant declarations.

**One caveat:** the invariant 1 binding is enforced only by orchestrator
discipline. There is no automated check that catches "AZ-III closes
with recorded miss." The §8 patches add archive-triage and
profile-threshold gates, but no `git`-level guard against close
drift. AZ-III's W5 hard gate 1 (`docs/tranches/AZ-III/FINAL.md` is
terminal or explicitly blocked, with `rg` scan for "interim |
placeholder | TBD | later | defer") is the closest thing. **It is
sufficient if respected.**

---

## Top-7 plan refinement proposals (ranked by impact)

Returned to caller, ranked by close-impact:

1. **Split W3 into W3a / W3b / W3c** (§7.1, §8). W3 as currently
   written is a tranche, not a wave; the single-wave packaging
   guarantees substrate-only close per the AY-era pattern. This is
   the highest-impact refinement.

2. **Move dev-loop scripts (`Makefile` / `.cargo/config.toml` /
   `scripts/*.sh`) out of W4 and into a new W0.7** (§7.3, §8).
   Per `feedback_build_infra_first`, infrastructure must precede
   source work; W4's bench wave is too late. Unblocks W1's
   `cargo iter-test` workspace fail-fast surface.

3. **Resolve W2 vs W3c emitter file-bounds race** (§5, §7.2, §8
   file-bounds patches). W2 takes shape-specific struct_direct.rs
   files; W3c takes the rest. Without this, parallel dispatch is
   unsafe.

4. **Add W0.5 Commit Body Truth Sample** (§3, §6.A, §8). The
   message-only rewrite produced template bodies, not
   evidence-bearing bodies. The SIX-AGENT-SYNTHESIS commitment is
   not satisfied; the verification of commit-body content has no
   wave owner.

5. **Add W0.6 Sibling Repo Triage Packet** (§6.B, §8). parse-that
   and pprint red surfaces are named in PROGRESS.md but not owned
   by any wave. Without explicit triage, AZ-III closes against an
   unbounded sibling-repo surface.

6. **Tighten W2.4 BBNF bootstrap canonical close** (§4 W2 gap, §8
   patch). Currently permits "produce blocker proof" as a close;
   per AZ-III invariant 1 ("Continuation, not deferral") and the
   AZ-II close-honesty pattern, this needs explicit "no
   `bootstrap_parser` in production routing" gate.

7. **Reconcile AZ-II FINAL vs PROGRESS-SNAPSHOT mismatches** (§3,
   §8 W0 hard-gate 6 + 7 + 8). Three concrete mismatches between
   FINAL.md and the snapshot exist; one ("within ±2% of AZ-I close")
   is unsourced and possibly fabricated. AZ-III.W0.2 should
   resolve these before W1 dispatches against an inconsistent
   reference.

---

## Citations index

**AZ-II FINAL.md** (`docs/tranches/AZ-II/FINAL.md`): lines 3-9, 53,
55, 61-65, 71-78, 84, 86-90, 94-105, 161-167, 171-173, 176, 200-202.

**AZ-II PROGRESS-SNAPSHOT-2026-04-29.md**: lines 3-12, 30-54, 70,
75-77, 82-84, 100-110.

**AZ-III AZ-III.md**: lines 28-33, 36-54, 56-69, 71-80, 91-110,
112-117.

**AZ-III PROGRESS.md**: lines 13-22, 27-32, 38-43.

**AZ-III audit/SIX-AGENT-SYNTHESIS-2026-04-30.md**: lines 8-23.

**AZ-III audit/W0-commit-repair-plan.md**: lines 28-46.

**AZ-III audit/W0-state-ledger.txt**: lines 23-50.

**AZ-III waves**: W0.md:24-36, 83-94; W1.md:30-32, 80-85;
W2.md:30-44, 95-99; W3.md:24-39, 83-93; W4.md:22-32, 76-87;
W5.md:39-65.

**AU FINAL.md**: lines 7-9, 245-288.
**AV FINAL.md**: lines 5-12, 105-115, 138, 208, 220-241, 287.
**AW FINAL.md**: lines 95, 174, 200, 207, 276, 313-326, 368-411,
417-456.
**AX FINAL.md**: lines 14-21, 76-83, 339-369.
**AY-I FINAL.md**: lines 81-84, 91-99, 144-149.
**AY-II-I AY-II-I.md**: lines 3-31.
**AY-III AY-III.md**: lines 3-12.
**AZ-I FINAL.md**: lines 158-161, 184-225, 230-241.

**Git evidence**:
- AZ-II opening commit `ecd12792 docs(tranches): physical split AZ → AZ-I + AZ-II`.
- AZ-II close-handoff commit `0fed1569 docs(az-iii): open AZ-II continuation close tranche`.
- 75 commits in `53d3e6b203ca4d5e1b5e34c06e05d867518ae0a5..HEAD` (the AZ-II rewrite + AZ-III opening range).
- Backup branch: `codex/az-history-before-reword-20260430-114057`.
- Recent terse-commit example: `dcb41e67`, `fb46a734`, `6a6ca1fd`,
  `8aa4c5df` — bodies are template language, not evidence-bearing.
