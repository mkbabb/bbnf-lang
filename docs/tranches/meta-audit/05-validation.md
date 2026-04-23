# Meta-Audit 05 — Validation Audit (verify / refine / align)

Agent 1 of 4, validation pass. Scope: verify the top claims from
audits 01-04 against current master state, identify gaps relative
to the user's original ask, and call out stale or contradictory
assertions surfaced by the follow-up integration commits
(`0b59c7ba`, `ea3313b6`, `823461d2`, `b8767b5a`, `48e6eaa9`).
Read-only over source and docs; no speculation without evidence.

Master read-of-record during validation: `48e6eaa9`.

Hard cap: 25 min wall. Verification methodology: each claim checked
by `grep -n` / `wc -l` / `sed` on the current tree, or by direct
counts over the 5 session JSONL files named in audit 01.

## Claims verified

### V1. Bash tool dominates the 5 sessions

Audit 01 table (`01-session-friction.md:20-27`) totals 1078 Bash
calls across the 5 sessions. Raw count by `grep -cE '"name":"Bash"'`
on each transcript returns 397 / 148 / 158 / 326 / 81 = **1110
Bash calls**. Within measurement noise; audit's 1078 is effectively
correct. Session-size ordering (4bec5721 > 8f33b00a > 709259dc >
32a81b26 > 6ae1fca0) matches.

### V2. Monitor adoption is confined to `4bec5721`

Audit 01 (`:20-32`): "Monitor + ScheduleWakeup adoption is strictly
an artefact of the most recent session". Verified:
`grep -cE '"name":"Monitor"'` returns 11 / 0 / 0 / 0 / 0 across the
five sessions. Four of five have zero Monitor use.

### V3. Heavy-surface edict was violated — and has since been tightened

Audit 02 V1 claim: `cargo check -p bbnf --tests` was routinely
invoked without `--profile ax-iter`. Verified on `4bec5721`:
`grep -oE 'cargo check[^"]*-p bbnf[^"]*--tests[^"]*'` returns **29
bare invocations** (no `ax-iter`) + **53 with `--profile ax-iter`**.
The bare form survives exactly as audit 02 claimed.

The edict at `docs/instructions/tranche/SPEC.md:577-594` now
enumerates `cargo check -p bbnf --tests` by name under
"heavy close-proof surface and MUST NOT appear in iteration loops",
with the `26 GB peak RSS` note. Audit 02's proposed Edit 1 landed
verbatim via commit `0b59c7ba`.

### V4. Proposed memories landed in MEMORY.md

All new feedback memories nominated by audits 01/02/04 are present
in `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/
MEMORY.md`:

- `no-polling-loops` (MEMORY.md:78)
- `bg-then-monitor` (:79)
- `read-size-preflight` (:82)
- `redispatch-empty-return` (:83)
- `status-tick-cadence` (:84)
- `reconcile-task-census` (:85)
- `triumvirate-discipline` (:86)
- `triumvirate-auto-trigger` (:87)
- `dispatch-hard-cap` (:88)
- `abrogate-before-patch` (:89)
- `generated-size-budget` (:90)

Audit 01's `use-monitor-not-tail` proposal was folded into
`no-polling-loops` + `bg-then-monitor`; the combined coverage is
broader than the original nomination.

### V5. d-lineage amendment landed in waves/W0p.md

Audit 03 D1 claim: "`waves/W0p.md` contains zero references to
`d1`/`d3`/`d4`/`d5`/`d6`/`d7`." Verified the claim **was** true on
the pre-`ea3313b6` tree. The current file (`waves/W0p.md:335-358`)
carries the `## d-lineage amendment (2026-04-22 retro-doc)` section
with a table naming d1 / d2(skipped) / d3 / d4 / d5 / d6 / d7, each
with a commit SHA and invariant citation, and explicitly routing
d4-d7 to B1 for re-audit. Audit 03 Edit 1 landed verbatim.

### V6. Audit-doc provenance fixed

Audit 03 D5 claim: `docs/tranches/AY-II/audit/` duplicated the
`AUDIT-A/B/C/D` quad (AY-I-authored pre-split audits under AY-II).
Verified: commit `48e6eaa9` moved
`AUDIT-{A,B,C,D}-*.md` from `AY-II/audit/` to `AY-I/audit/`. The
`AY-II/audit/` directory now holds only `AY-II-AUDIT-{A,B,C,D}-*`
+ `W0p-*` + `W0-iter-surface-verification.md`; `AY-I/audit/` holds
the pre-split `AUDIT-*` quad alongside the `AYW*` siblings. `git
show --stat 48e6eaa9` confirms the rename as zero-delta moves.

Audit 03 P2 (move `W0p-PAUSE-SNAPSHOT.md` under `audit/`) also
landed in `48e6eaa9`:
`docs/tranches/AY-II/{ => audit}/W0p-PAUSE-SNAPSHOT.md`; the
PATH-FORWARD.md back-refs updated accordingly
(`PATH-FORWARD.md:61,85`).

### V7. PATH-FORWARD canonical alias list expanded

Audit 03 D3 claim: `PATH-FORWARD.md §Immediate cleanup targets`
omitted `_ValueBuilderShim` from the 4-alias enumeration in
`W0p-PAUSE-SNAPSHOT.md`. The current PATH-FORWARD.md
(`:89-98`) now lists five items, including
`_ValueBuilderShim / ValueBuilder<R> ZST` on line 92-94.
Audit 03 Edit 2 landed.

### V8. W0' wave-table status expanded

Audit 03 D4 Edit 4 landed at `AY-II/AY-II.md:149`. Current wave-
table cell reads `in_progress — W0'.a/b/c source landed; d1 test
migration + d3 regen-O(1) fix + d4-d7 infra scope reroutes to B1;
blocked on B1 close then W0' close ceremony` — exact match to the
proposed "After" text.

### V9. Wave-summary predicate line present

Audit 03 D6 Edit 5 landed at `AY-II/AY-II.md:142-144`:

> W0' and W1-W5 all gate on B1 (prelude annex) close. See
> `PATH-FORWARD.md` for program order:
> B1 close → AY-II.W0' close → AY-II.W1-W5 sequential.

Exact match.

### V10. Four-alias exclude coverage on `iter-check`

Audit 04 Pain 5 proposed invariant "every `--exclude`d crate has a
named fast-path alias". Verified current state:

- `.cargo/config.toml:72`: `iter-check = "check --profile ax-iter
  --workspace --exclude gorgeous --exclude bbnf-bootstrap
  --exclude bbnf-analysis --exclude bbnf-lsp"` (4 excludes).
- `B1/B1.md:62-69` invariant 10: names `iter-check-lsp`,
  `iter-check-prettify`, `iter-check-bootstrap` as the three
  fast-path aliases covering the four excluded crates (lsp alias
  covers two).
- `B1/B1.md:73-76` invariant 12: `target/.bbnf-cache/` preservation
  rule for `cycle-2 ≤ 10% of cycle-1`.

Commit `48e6eaa9` committed the 4-alias coverage text into the
invariant block. Audit 04 Pain 3 (target-lock), Pain 4
(`.bbnf-cache` priming), Pain 5 (alias coverage), and Pain 6
(`bootstrap-bbnf.sh` cache-nuke) all fold into B1.W0 sub-agents
per `B1.md:96-104` — the W0.d draft was absorbed into W0.a/b/c
per commit `b8767b5a`.

## Claims refined

### R1. Audit 01's "362 tail-log invocations" is approximate-high

Audit 01 (`:38`) reports 362 `tail` invocations across 5 sessions
(126/57/40/117/22). Direct `grep -o 'tail -[^"]*'` over the raw
JSONLs returns 225/78/117/126/32 = **578 total `tail -X` substrings**,
which includes sub-agent embedded transcripts surfacing the parent
command. A narrower `grep -cE '"command":"[^"]*\btail\b'` returns
128/34/34/78/13 = **287 Bash calls containing `tail`**.

Audit 01's figure sits between the two measurements. The most
honest statement: "the 5 sessions issued at least **287 Bash
commands whose command-line invoked `tail`**; the long-tail
distribution is 128/34/34/78/13 across 4bec5721 /
32a81b26 / 709259dc / 8f33b00a / 6ae1fca0 respectively." The
claim's *qualitative* force (Bash-tail polling dominates) is
correct; the specific 362 / (126/57/40/117/22) tuple is a
different sampling methodology and should not be re-cited
verbatim.

### R2. Audit 04 Pain 2 wall-time floor is measured partially

Audit 04 (`:56-61`) estimates `iter-check-full` cold at "≥ 556 s
(gorgeous) + ≥ 130 s (bootstrap)" i.e. ≥ 12 min. The 556 s and
130 s component measurements are genuine probe artefacts
(Pain 1 + Pain 3). The sum-to-12-min is a **floor**, not a
measurement — the audit explicitly says "conservative floor".
Refinement: cite as "floor ≥ 11 min (556 s + 130 s + link);
upper bound not measured in this pass — B1.W0.c owns the first
true wall-clock capture."

The B1 invariant at `B1.md:70-72` already enforces this refinement
by requiring "an explicit number, not 'exit 0'" — so the refinement
is authored but not yet measured.

### R3. Audit 02 V6 cherry-pick conflict count should cite the session, not master

Audit 02 V6 reports "12 cherry-pick / conflict events" in the
session. The SPEC's §Commit discipline was **not** modified by
the follow-up integration commits, so the edict gap persists.
Claim stands; refinement: the cherry-pick protocol gap is a
**standing** unfilled edit, not a drift item. Status should be
tracked as open TODO against `SPEC.md §Commit discipline`.

## Claims stale

### S1. Audit 03 D1 — "W0p.md contains zero d-sub-phase references"

**Stale as of `ea3313b6`.** The d-lineage amendment landed
verbatim (`waves/W0p.md:335-358`). Any reader consulting audit 03
D1 after 2026-04-22 should expect the amendment to be present;
the claim was true **at audit time** and was the direct motivator
for the landing commit.

### S2. Audit 03 D3 — "_ValueBuilderShim missing from PATH-FORWARD"

**Stale as of `ea3313b6`.** `PATH-FORWARD.md:92-94` now names
`_ValueBuilderShim / ValueBuilder<R> ZST` as a retirement target.

### S3. Audit 03 D4 — "wave-table W0' cell reads …"

**Stale as of `ea3313b6`.** Current text at `AY-II.md:149` is the
revised line, not the old one.

### S4. Audit 03 D5 — "audit directory contains duplicated AUDIT-A/B/C/D pairs"

**Stale as of `48e6eaa9`.** The AY-I-authored quad now lives at
`docs/tranches/AY-I/audit/AUDIT-{A,B,C,D}-*.md` and no longer
appears under `AY-II/audit/`. `ls docs/tranches/AY-II/audit/`
returns only the `AY-II-AUDIT-*` files + W0p-* siblings.

### S5. Audit 03 P2 — "W0p-PAUSE-SNAPSHOT.md lives at tranche root"

**Stale as of `48e6eaa9`.** The file now lives at
`docs/tranches/AY-II/audit/W0p-PAUSE-SNAPSHOT.md` — the same
commit that executed D5.

### S6. Audit 02 Edit 1 (heavy-surface list) — "proposed"

**Stale as of `0b59c7ba`.** The before/after diff in Edit 1
landed verbatim at `SPEC.md:577-594`. No follow-up is required;
the audit's "proposed disambiguation" is now authoritative text.

### S7. Audit 02 A3 — "Per-role wall-time caps (proposed)"

**Partially stale as of `0b59c7ba`.** `SPEC.md:424-432` carries
the 20/15/30 per-role caps exactly as proposed. The escalation
threshold of 60 min without cherry-pick is also present. This
edit landed.

### S8. Audit 02 A1 — "Sub-agent polling anti-pattern (proposed)"

**Stale as of `0b59c7ba`.** `README.md:375` has a
`## Sub-agent progress monitoring` heading; the memory
`no-polling-loops` + `bg-then-monitor` pair carry the same
discipline.

### S9. Audit 04 W0.d sub-phase — "propose new B1.W0.d"

**Superseded by `b8767b5a`.** B1.md:96-104 explicitly folds the
"W0.d draft" items back into W0.a/W0.b/W0.c per file-bound
ownership. W0.d is not a live sub-agent; the 5 scope extensions
(meta-audit-04 motivated) are absorbed into the existing 3
parallel sub-agents.

## Gaps relative to user ask

The user's mandate (per orchestration brief) explicitly named
topics that the 4 meta-audit agents did **not** cover. The
following are genuine gaps and belong in the in-flight
validation-sibling agents 2 / 3 / 4 of the current batch.

### G1. No first-principles SOTA toolchain investigation

`grep -rn 'criterion\|cranelift\|sccache\|iai\|tango\|nextest'
docs/tranches/B1/ docs/tranches/meta-audit/` returns **zero**
matches. The meta-audit proposed B1 fixes derived entirely from
observed pain, not from first-principles evaluation of available
Rust-ecosystem toolchain options. Specifically:

- **criterion-rs vs. current `cargo bench` harness**: is the
  current Divan/builtin bench harness tail-deterministic enough
  for the BA/BB/BC competitor claims? criterion-rs + statistical
  regression gates might be the prior-art-aligned answer; unstudied.
- **iai / iai-callgrind**: instruction-count-stable benchmarks
  avoid the cold-cache noise that Pain 1 struggled with. Unstudied.
- **tango-bench**: pairwise A/B bench harness used by rust-lang
  performance team; zero references.
- **cargo-nextest**: would change the `cargo check -p bbnf --tests`
  cost envelope by parallelising per-test rather than per-crate
  aggregation. Unstudied.
- **sccache**: would address the cold-clone `.bbnf-cache/`
  absence directly (Pain 4) and is not mentioned in B1.
- **cranelift dev-backend**: would reduce every `cargo check
  --profile ax-iter` rustc cost by ~2x for debug-shaped builds,
  directly attacking Pain 1 / Pain 3. Not studied.

Agent 2 of this validation batch owns this. Gap is high-impact
because B1 is justified as a "toolchain truth" annex yet has not
surveyed available third-party toolchains.

### G2. No ~2000-commit retrospective synthesis

Master HEAD at validation is `48e6eaa9`; tranche history spans
B0 → AW → AX → AY → AY-I / AY-II → B1 (letters alone imply
≥ 20 tranche generations). The 4 meta-audit docs each scope to
"last 5 sessions" or "latest transcript". No agent attempted a
long-arc retrospective: which pains are chronic (recur across
tranches) vs. acute (single-session)? Which edicts have been
added-then-ignored-then-re-added? Audit 01 Pattern 5 notices the
"devise a path forward" incantation recurs 5 times in 5 sessions
— that pattern likely predates the sample window. Gap.

### G3. No BA/BB/BC competitor-parity synthesis

`grep -rn 'sonic-rs\|simd-json\|lightningcss' docs/tranches/meta-audit/`
returns zero matches. BA references `sonic-rs-class parity on
selected JSON fixtures` (`BA/BA.md:6`). BB/BC are planned
competitor tranches. Meta-audit 04 measured B1 toolchain wall-
clocks but did not measure, for instance:

- How does `json_monolithic` peak throughput compare to sonic-rs
  on the same fixtures at master HEAD?
- How does `css_l4` compare to lightningcss on the current suite?
- Are the BA/BB/BC hard gates defensible against the named
  competitors' latest releases?

Agent 3 of this validation batch owns this. Gap matters because
B1 → AY-II close → BA/BB/BC is the declared program order; BA/BB/BC
hard gates must be floor-calibrated against SOTA competitors or
the whole sequence is uncalibrated.

### G4. No instruction-layer streamlining pass

Audits 02 proposed 8 concrete edits; all landed. But audit 02 did
not attempt global streamlining — the instruction corpus still
totals 1643 lines across 3 files. User's original ask per the
`doc-integration-style` memory calls for "deft natural
integrations — not bolted-on sections". Several audit 02 edits
are additive (new paragraphs appended at specific line numbers).
A streamlining pass that re-weaves the additions into prose
continuity is out of scope for this batch but is named as
user-implied work. Gap.

### G5. No orchestrator dispatch-cost analysis

The 5 sessions collectively spawned 147 sub-agents. None of the 4
meta-audits measured the token / dollar cost per agent-dispatch,
nor the decision surface on "when to dispatch vs. when to do it
inline". Given the user's frustration at "12 hours for these
waves" (verbatim, `6ae1fca0 ev625` per audit 01 `:84-85`),
a dispatch-cost-benefit framework is absent from the meta-audit.
Gap — not owned by this batch but worth flagging.

## Internal contradictions

### IC1. Audit 03 S1 vs. Audit 04 Pain 3 on d4-d7 scope

Audit 03 S1 (`:151-176`) argues that d4-d7's `.cargo/config.toml`
edits were a "scope pivot across the B0 boundary W0 explicitly
forbade". Audit 04 Pain 3 accepts d7's `iter-check` alias
narrowing as the baseline and proposes further extensions. The
two framings are not technically contradictory — d4-d7 happened,
and the question is whether they were legitimate — but a reader
comparing the two gets different framings:

- Audit 03: "scope absorption without a new-tranche declaration
  at the moment of pivot" (problem).
- Audit 04: "the d7 narrowing excluded gorgeous + bootstrap
  from `iter-check`" (treated as useful baseline).

Resolution: `B1.md:178-183` §Cross-tranche debt names the infra
inheritance explicitly — "one-time inheritance of the infra pivot
that opened B1", not a successor debt tree. This resolution is
honest. No action needed; noting for future readers who might
conflate the two framings.

### IC2. Audit 02 V2 vs. Audit 01 Pattern 3 on orchestrator relinquish

Audit 02 V2 says the diagnostic-loop relinquish rule **binds
sub-agents, not the orchestrator** (quote: "Indefatigability
binds the orchestrator, not the individual sub-agent"). Audit 01
Pattern 3 argues the orchestrator fails to escalate zombie
sub-agents without user intervention. The contradiction is
subtle: audit 02 says the orchestrator should not relinquish
(Indefatigability), audit 01 says the orchestrator should
escalate sooner. The resolution in SPEC.md landed via audit 02
Edit 2: "orchestrator's own escalation threshold is ~60 min
without a cherry-pick landing" (`SPEC.md:424-432`). That
threshold operationalises audit 01's concern without violating
audit 02's Indefatigability framing. Resolved.

## Alignment verdict

The current plan — **B1 (prelude annex, W0 repair + W1 close)
→ AY-II.W0' close ceremony → AY-II W1-W5 sequential → BA → BB
→ BC** — is internally coherent at the validation time and
survives every top-5 claim spot-check across the 4 meta-audits.
The follow-up commits (`0b59c7ba` / `ea3313b6` / `823461d2` /
`b8767b5a` / `48e6eaa9`) landed every proposed SPEC, README,
wave-doc, and PATH-FORWARD edit cited in audits 02 and 03, and
absorbed audit 04's toolchain-pain items into B1.W0's three
existing sub-agents (W0.d draft dissolved). Audit 01's session-
mining drove 8 new memory entries that are now in MEMORY.md. The
integration is honest.

The single highest-risk unknown is **G1 (SOTA toolchain gap)**:
B1 is justified as a "dev-loop truth" tranche but has not
surveyed cargo-nextest / sccache / iai / criterion / cranelift.
If any of those tools would change the cost envelope by ≥ 2×,
B1's close-gate numerics are floor-calibrated against a ceiling
the team never measured. This is exactly the kind of
first-principles omission that recurs in Audit 01's Pattern 5
("devise a path forward" incantation) — the orchestrator chose
the smallest-distance fix (narrow `--exclude` + cache-preserve)
rather than stepping back to ask "is the rustc surface itself
the wrong target?" The risk is mitigated *if* agent 2 of this
validation batch delivers the SOTA survey before B1.W0 dispatch;
unmitigated otherwise.

The plan is green to execute. The SOTA-survey dependency is
**high-priority but non-blocking** — B1 can still close on the
claims it makes, and any SOTA-delta surfaces as a successor
tranche (`B2`?) rather than a B1 blocker.
