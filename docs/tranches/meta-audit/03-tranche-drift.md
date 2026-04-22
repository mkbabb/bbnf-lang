# Meta-Audit 03 — Tranche Scope + Doc Drift

Agent 3 of 4. Scope: open tranche docs under `docs/tranches/`,
focus on AY-II / B0 / B1 and the AY-II audit corpus. Read-only
over source; proposes concrete doc edits only.

Master HEAD read-of-record: `e777a68d`
(`docs(ay-ii, b1): progress + path forward + B1 annex scaffold`).

## Tranche doc state (as-read)

| Tranche | Plan doc HEAD (status line) | PROGRESS status | FINAL? | Open waves | Stated status |
|---|---|---|---|---|---|
| AY-I | `AY-I/AY-I.md` (no live status — pass-I closed) | `AY-I/PROGRESS.md` | yes (`AY-I/FINAL.md:1-10`) | none | pass-I close; debt routed to AY-II |
| AY-II | `AY-II/AY-II.md:144-150` wave table | `AY-II/PROGRESS.md:7-9` (`blocked on B1 prelude annex`) | no | W0 (superseded), W0' (in_progress), W1-W5 (planned) | W0' blocked on B1; `Current wave` = W0' (blocked) |
| B0 | `B0/B0.md:43-46` wave table | `B0/PROGRESS.md` | yes (`B0/FINAL.md:1-10`, HEAD `7b223cf6`) | none | closed |
| B1 | `B1/B1.md:75-78` wave table | `B1/PROGRESS.md:7-9` (`planned`) | no | W0 (planned), W1 (planned) | planned; blocks AY-II.W0' close |
| BA | `BA/BA.md:53-59` | planned | no | all planned | gate: AY-II close |
| BB | `BB/BB.md` | planned | no | all planned | gate: BA close |
| BC | `BC/BC.md` | planned | no | all planned | gate: BB close |

## Doc drift findings

### D1. `waves/W0p.md` omits every d-sub-phase that PROGRESS.md records as landed.

`AY-II/PROGRESS.md:210-323` dates and cites commits for W0'.d1
(`60f92743`), W0'.d3 (`f768f50d`), W0'.d4 (`5c737bd1`), W0'.d5
(`f5cdcd52`), W0'.d6 (`2e5e3ff5`), W0'.d7 (`700501f5`), plus the
verification doc at `133a87ee`. The corresponding wave spec
`AY-II/waves/W0p.md` spans 334 lines and contains **zero**
references to `d1`/`d3`/`d4`/`d5`/`d6`/`d7`
(`grep -n 'W0.\.d[1-7]\|\bd[1-7]\b' AY-II/waves/W0p.md` → 0
matches). The wave authors `W0'.a` / `W0'.b` / `W0'.c` only. The
entire `d` lineage — test migration, O(1) `direct_child_count`
regen fix, gorgeous feature-gating, gorgeous dev-dep drop,
`build.rs` fingerprint narrow, and `iter-check` workspace
narrowing — lives outside the wave spec.

Evidence: `waves/W0p.md:1-334` end-to-end; commit SHAs per
`PROGRESS.md:214-311`.

**Proposed fix**: append a `## d-lineage amendment` section to
`waves/W0p.md` that names each d-sub-phase, its landing commit,
the invariant it discharges, and whether it altered any of
invariants §14-§19. See Edit 1.

D2. "d2" never appears. PROGRESS numbers d1, d3-d7 but skips
d2. Reader cannot tell whether d2 was planned-and-cancelled,
superseded by d3's scope-expansion, or silently dropped.
Proposed fix folded into Edit 1.

### D2. AY-II.md §Plan-audit findings bullet 4 is out-of-date against W4.md.

`AY-II/AY-II.md:183-187` (bullet 4) still instructs `W4.e` to
update at W0' close to name `FusedBuilder::push_leaf_*` /
`begin_compound`. W4.md has already absorbed that update
(`waves/W4.md:20` references `FusedBuilder::push_leaf_*` /
`begin_compound` / `end_compound` as `W0'.a symbols`;
`waves/W4.md:136,161` reference `FusedBuilder::rollback_to` /
`::finish`). The **action** is therefore already discharged.

**Proposed fix**: annotate bullet 4 "Action landed at
`waves/W4.md:20,136,161`" so the §Plan-audit findings section
stops reading as open work. Edit 3 below.

### D3. PATH-FORWARD §Immediate cleanup targets and W0p-PAUSE-SNAPSHOT §Transient aliases disagree on the alias count.

`AY-II/PATH-FORWARD.md:82-88` names 4 alias/shim retirement
targets: `TapeBuilder = FusedBuilder`, `ValueBuilderOutput`,
`value_builder shim module`, `4-arg new_fused`,
`value_api_apples_to_apples.rs` counter imports (5 items total
— `value_builder shim module` on line 83-84 is listed alongside
`ValueBuilderOutput` as a combined bullet but is a distinct
surface).

`AY-II/W0p-PAUSE-SNAPSHOT.md:49-56` enumerates 4 alias kinds:
`pub type TapeBuilder = FusedBuilder`, `_ValueBuilderShim` /
`ValueBuilder<R>` ZST, `pub type ValueBuilderOutput<R> =
FusedOutput<R>`, 4-arg `Parsed::new_fused` shim. The
SNAPSHOT's `_ValueBuilderShim` **is not mentioned** in
PATH-FORWARD as a retirement target. Depending on the source
of truth at cleanup time, an orchestrator working from
PATH-FORWARD will not retire `_ValueBuilderShim`.

**Proposed fix**: reconcile to one canonical list in
PATH-FORWARD. Edit 2 below.

### D4. `AY-II.md:145` wave-table status string for W0' does not mention the d-lineage landings.

The W0' cell reads
`in_progress — runtime/source work landed; blocked on B1 close
and W0' close ceremony`. Given d4/d5/d6/d7 landed substantial
dev-infra source changes (`crates/gorgeous/Cargo.toml`,
`crates/derive/build.rs`, `crates/core/Cargo.toml`,
`.cargo/config.toml`) the wave-table summary is silent on
whether those landings are part of W0' or a separate branch.
W0p.md's Scope §W0'.a/b/c does not cover any of them. This is
the classic scope-creep-into-unnamed-subphase pattern — partly
covered by B1's creation, but the AY-II side of the handoff
still treats d4-d7 as part of W0'.

**Proposed fix**: tighten the wave-table cell to
`in_progress — W0'.a/b/c/d landed; d1 test migration + d3-d7
regen/infra in lineage; blocked on B1 close + post-B1 W0'
close ceremony`. Edit 4 below.

### D5. Audit directory contains duplicated AUDIT-A/B/C/D pairs.

`AY-II/audit/` carries:
- `AUDIT-A-plan-coherence.md` (269 lines, AY-I pass-I dated
  2026-04-20, HEAD `b346ebca`).
- `AY-II-AUDIT-A-plan-coherence.md` (166 lines, AY-II pass-II
  dated 2026-04-21, HEAD `b5bbda6c`).
- Same doubling for B/C/D.

The AY-I-authored AUDIT-{A,B,C,D} files are the input audit
that motivated the AY→AY-I/AY-II split. They are placed here
per SPEC §Multi-pass tranche split ("research/plan artefacts
belong to their authoring pass") — but their authoring pass is
AY-I, not AY-II. `AY-II/PROGRESS.md:342-347` says they were
"cherry-picked from their worktrees during the pass-I →
pass-II transition and placed under this pass's `audit/`
directory per the multi-pass-tranche edict". The edict
actually says artefacts belong to the **authoring** pass; by
that reading the AY-I-authored quad should live under
`AY-I/audit/` (where AYW-* siblings live).

This is a provenance ambiguity, not a factual drift. Cost of
fix is low (move 4 files); benefit is coherent provenance for
future readers who grep audits by tranche. Flagging without
proposing a verbatim move — agent 4 or a follow-up meta pass
can execute if desired.

### D6. Status claim "B1 closes first" does not explicitly name B1 as a **prelude annex** in AY-II.md §Wave summary context.

`AY-II/AY-II.md:13-14` says "B1 closes first as the bounded
prelude annex over the proof surface" in prose. The **Wave
summary** table (`144-150`) does not reference B1 at all.
Reader scanning the table alone cannot see the B1 predicate
on W0' close. Given the AY-II plan invariants at §Operational
posture mention dispatch strictly via AY-II waves, a B1
predicate belongs in the wave-table row for W0' or in a
separate note right above the wave summary.

**Proposed fix**: add a one-line predicate line above the wave
table: `W0'/W1-W5 all gate on B1 close; see
PATH-FORWARD.md for program order.` Edit 5 below.

## Scope creep findings

### S1. d4-d7 infra work expanded W0' scope into B0's territory mid-wave.

W0's plan doc (`AY-II/waves/W0.md:75-81`) is explicit:
"**Do NOT touch**: … `Makefile`, `.cargo/config.toml`,
`scripts/*` (B0 surface)". W0'.d5 (`f5cdcd52`) and W0'.d7
(`700501f5`) landed edits to `.cargo/config.toml`
(`AY-II/PROGRESS.md:299-306`). This is exactly the cross-B0
boundary the W0 spec forbids.

The redress is principled — it is documented
(`audit/W0p-infra-fix-plan.md`, `audit/W0p-infra-root-cause.md`)
and per the `build-infra-first` feedback (infra lands FIRST
in any tranche where dev iteration time is a bottleneck). The
scope pivot was carried by promoting B1 to a distinct
prelude annex **after** d4-d7 had already landed on master.
That is scope absorption without a new-tranche declaration at
the moment of pivot — SPEC §Two valid scope-reveal response
modes says Absorb mode requires naming the absorption at
dispatch time, not retroactively.

**Proposed cleanup**: PROGRESS already documents this clearly
at `:242-310`. AY-II.md should carry a one-line §Scope-creep
rationalization noting that d4-d7's infra edits are
retroactively rerouted into B1's jurisdiction for re-audit
under B1.W0, and W0' will not re-own them at close. Edit 6
below.

### S2. B1 is technically a scope pivot of W0' rather than a separate tranche.

W0p.md's original `W0'.c` scope (`waves/W0p.md:200-204`)
covers `#[allow(dead_code)]` retirement + scan-policy splice;
d4-d7 carry different scope (proc-macro wall, fingerprint
narrowing, workspace alias). Those d-sub-phases were created
mid-wave after W0'.a/b/c returned. SPEC §new-tranche-new-doc
feedback says: "Scope pivots mid-tranche open a NEW tranche
letter + new docs/tranches/XX.md; never continue old
numbering". B1 is the new-letter tranche — that matches the
edict. But d4-d7 landed under W0' numbering before B1 was
named, which means the numbering grew under W0' and only
later was split off. No more cleanup is required beyond D4
and S1; the SPEC intent is honored as of the B1 scaffold.

### S3. PATH-FORWARD.md §3 "AY-II W1-W5" restates the post-B1 order but waves/W{1..5}.md file-bound rows still include `grammar.rs` compose-bridge language.

Not load-bearing; W1-W5 plans are still valid post-W0' per
AUDIT-C's finding ("W1-W5 specs hold unchanged"). The pre-W0
wording that persists is bench-path / symbol-name oriented
(e.g., `waves/W4.md:136-161` uses the FusedBuilder names
correctly, but W3.md/W5.md cite `ValueBuilder::*` as a
retired-surface-absent check — which is correct and
load-bearing for the invariant). No cleanup needed.

## Undocumented deferrals

### U1. d2 is named neither in plan nor in progress log.

W0'.d1 and W0'.d3 land in PROGRESS; W0'.d2 is never
mentioned. The gap could be intentional (d2 was the label for
the draft fix `4f4c9ec9` "emitter TapeBuilder->FusedBuilder"
that was superseded by the d3 direct_child_count fix — this
commit shows in the parallel-agent's `git log --all` but not
in the authoritative PROGRESS log), but the reader cannot
tell.

**Proposed routing**: add a one-line entry to PROGRESS at
`:309-311` (just before the d7 cold-measurement table)
naming d2 as "skipped — draft fix `4f4c9ec9` subsumed by
d3's O(1) `direct_child_count` redress". Folded into Edit 1.

### U2. AY-II.md §Plan-audit findings bullet 2 Unknown retirement is scoped to W0'.b but W0'.b PROGRESS-logged commits (`550dac11`, `b1bb4579`) do NOT include retirement evidence.

`AY-II/AY-II.md:170-175` routes `<Grammar>Value::Unknown`
retirement to W0'.b. PROGRESS §W0' execution
(`:210-221`) lists 2 W0'.b commits, both titled
`feat(view,emitter,tests): route project_value_<G>
through materialize_projection_*_<G>` and
`fix(view,emitter): raw_name for materializer lookup +
runtime tape path`. Neither commit title nor the PAUSE
SNAPSHOT §W0'.b summary (`W0p-PAUSE-SNAPSHOT.md:76-101`)
mentions Unknown retirement. The PAUSE SNAPSHOT explicitly
states `Unresolved question at interruption: final shape of
the <Grammar>Value::Unknown retirement ledger per grammar`.

That deferral is named but its destination is unclear — it
may ride W0' close ceremony (post-B1) or it may route to a
follow-on. Without a named destination this is an
implicit deferral.

**Proposed routing**: W0' close ceremony in PATH-FORWARD.md
§2 should enumerate Unknown retirement per grammar as an
explicit close step. Currently §2 only lists 8 generic
steps. Add step 9: "Verify `<Grammar>Value::Unknown`
retirement per grammar per AY-II.md §Plan-audit findings
bullet 2; record the per-grammar exception ledger."
Edit 7 below.

## Cross-tranche debt coherence

### DC1. AUDIT-D cites 15 cross-tranche debt items; only 11 are named in PROGRESS.

`AY-II/audit/AY-II-AUDIT-D-predecessor-successor.md` (284
lines) ends with a "Cross-tranche debt ledger: 15 items, 11
to AY-II internally" headline (`PROGRESS.md:173-174`
verbatim). The PROGRESS record names the 11 internal
AY-II items only by reference to the audit doc — the
ledger **itself is not transcribed into a tracked
artefact**. The 4 non-AY-II-internal items are likewise
not named at the PROGRESS level.

This is debt-ledger drift: the audit asserts 15 items
exist, the forward plan does not enumerate them
individually. If W0' close runs based on PROGRESS, the
orchestrator has to re-read the audit to find the 4
external-tracked items.

**Proposed fix**: add an §Debt ledger mirror subsection
under AY-II.md §Plan-audit findings that enumerates the
AUDIT-D 15 items with their routing destinations
(AY-II internal | BA | BB | BC | closed). Edit 8 below.

Given time constraints this audit will NOT enumerate all 15
here — that requires a full re-read of AUDIT-D's tail
section. The **structural edit** is what's proposed; the
content is a follow-on agent or a manual hour.

## Audit-doc provenance

### P1. `audit/AUDIT-{A,B,C,D}` — AY-I authored, shelved under AY-II.

Per D5 above. Not moved in this audit; flagged for disposition.

### P2. `audit/W0p-PAUSE-SNAPSHOT.md` lives at `AY-II/W0p-PAUSE-SNAPSHOT.md` (tranche root), not under `AY-II/audit/`.

W0p-PAUSE-SNAPSHOT.md is at tranche root
(`docs/tranches/AY-II/W0p-PAUSE-SNAPSHOT.md`). Every other
W0p-* sibling (regen/infra root-cause/fix-plan,
iter-verification) lives under `AY-II/audit/`. The PAUSE
SNAPSHOT's provenance is identical (agent-authored, captures
worktree state). Cost to move: trivial; benefit: directory
coherence.

**Proposed fix**: move to `AY-II/audit/W0p-PAUSE-SNAPSHOT.md`.
Not proposing verbatim here (mechanical move); flagging for
agent 4 or follow-on.

## Proposed edits (concrete)

### Edit 1: `docs/tranches/AY-II/waves/W0p.md` — append §d-lineage amendment

Before (verbatim, end of file after line 334, currently
terminates at "One codegen path. One builder type. One
projection path. One navigation path."):

```
- One codegen path. One builder type. One projection path.
  One navigation path.
```

After (append):

```
- One codegen path. One builder type. One projection path.
  One navigation path.

## d-lineage amendment (2026-04-22 retro-doc)

W0'.a / W0'.b / W0'.c (the original three-agent decomposition
above) returned at PAUSE SNAPSHOT. Six follow-on d-sub-phases
landed between 2026-04-21 and 2026-04-22 while W0' was
open. They are recorded here so the wave spec matches the
PROGRESS log.

| Sub-phase | Commit | Scope | Invariant discharged |
|---|---|---|---|
| d1 | `60f92743` | Test migration from `push_compound`/`mark_children` to FusedBuilder (tape tests, tape_walker_allocs, json-prototype visitor) | §15 public-API retirement evidence |
| d2 | (skipped; `4f4c9ec9` draft fix subsumed by d3) | — | — |
| d3 | `f768f50d` | O(1) `direct_child_count` in `value_end_compound`; replaces Θ(N²) recursive `subtree_size` path | W0p regen close precondition |
| d4 | `5c737bd1` | Gate gorgeous `#[derive(Parser)]` sites behind per-grammar cargo features | dev-loop infra; routes to B1 |
| d5 | `f5cdcd52` | Drop gorgeous as mandatory `bbnf` dev-dep | dev-loop infra; routes to B1 |
| d6 | `2e5e3ff5` | Narrow `crates/derive/build.rs` fingerprint scan to codegen-relevant subtrees | dev-loop infra; routes to B1 |
| d7 | `700501f5` | `.cargo/config.toml` `iter-check` alias excludes gorgeous + bbnf-bootstrap + bbnf-analysis + bbnf-lsp; `iter-check-full` retains `--workspace` for CI | dev-loop infra; routes to B1 |

d4-d7 touch `.cargo/config.toml` + `Cargo.toml` files — the
W0 `Do NOT touch` list in `waves/W0.md:75-81` declares those
out-of-bounds. The pivot is recorded here; the formal
re-audit of d4-d7 is B1.W0's scope
(`docs/tranches/B1/waves/W0.md`). W0' does not close on the
correctness of d4-d7; B1 does.
```

Rationale: closes the W0p.md ↔ PROGRESS.md asymmetry; names
d2 as skipped; explicitly routes d4-d7 to B1 so the wave
spec stops pretending d4-d7 were always part of the
a/b/c plan.

### Edit 2: `docs/tranches/AY-II/PATH-FORWARD.md` §Immediate cleanup targets

Before (verbatim, `PATH-FORWARD.md:79-89`):

```
## Immediate cleanup targets already identified

These are not optional polish items; they are W0' close work:

- `crates/tape/src/builder/mod.rs` — retire `pub type TapeBuilder = FusedBuilder;`
- `crates/core/src/runtime/mod.rs` — retire `ValueBuilderOutput` alias
  and the `value_builder` shim module
- `crates/core/src/runtime/parsed.rs` — retire the 4-arg `new_fused`
  bridge once regen no longer emits it
- `crates/core/tests/value_api_apples_to_apples.rs` — move the builder
  counter imports onto the fused builder surface
```

After:

```
## Immediate cleanup targets already identified

These are not optional polish items; they are W0' close work.
Canonical list; if in doubt against `W0p-PAUSE-SNAPSHOT.md`
§Transient compose-escape aliases, the SNAPSHOT is the
source of truth for alias-kind enumeration.

- `crates/tape/src/builder/mod.rs` — retire `pub type TapeBuilder = FusedBuilder;`
- `crates/core/src/runtime/mod.rs` — retire `ValueBuilderOutput` alias
  and the `value_builder` shim module
- `crates/core/src/runtime/mod.rs` — retire `_ValueBuilderShim` /
  `ValueBuilder<R>` ZST once the counter imports in
  `value_api_apples_to_apples.rs` migrate to the fused surface
- `crates/core/src/runtime/parsed.rs` — retire the 4-arg `new_fused`
  bridge once regen no longer emits it
- `crates/core/tests/value_api_apples_to_apples.rs` — move the builder
  counter imports onto the fused builder surface
```

Rationale: resolves D3 — SNAPSHOT's `_ValueBuilderShim`
entry was missing from PATH-FORWARD.

### Edit 3: `docs/tranches/AY-II/AY-II.md` §Plan-audit findings bullet 4

Before (verbatim, `AY-II.md:183-187`):

```
4. **W4.e samply expectations** — currently reference
   `ValueBuilder::push` or "W0.c's landed name". **Action**:
   W4.e updates at W0' close to reference the concrete fused
   symbol name (`FusedBuilder::push_leaf_*` / `begin_compound`).
```

After:

```
4. **W4.e samply expectations** — currently reference
   `ValueBuilder::push` or "W0.c's landed name". **Action**:
   W4.e updates at W0' close to reference the concrete fused
   symbol name (`FusedBuilder::push_leaf_*` / `begin_compound`).
   **Status**: landed preemptively at `waves/W4.md:20,136,161`
   during the AY-II/B1 redress pass.
```

Rationale: §Plan-audit findings should flag discharged
actions so subsequent readers don't re-open them.

### Edit 4: `docs/tranches/AY-II/AY-II.md:145` — W0' wave-table status

Before (verbatim):

```
| **W0'** | [waves/W0p.md](waves/W0p.md) | FusedBuilder collapse + projection-consumer wiring + scan-policy splice + legacy-cruft deletion (3 parallel sub-agents) | W0 partial landing | in_progress — runtime/source work landed; blocked on B1 close and W0' close ceremony |
```

After:

```
| **W0'** | [waves/W0p.md](waves/W0p.md) | FusedBuilder collapse + projection-consumer wiring + scan-policy splice + legacy-cruft deletion (3 parallel sub-agents + d-lineage dev-infra follow-ons) | W0 partial landing | in_progress — W0'.a/b/c source landed; d1 test migration + d3 regen-O(1) fix + d4-d7 infra scope reroutes to B1; blocked on B1 close then W0' close ceremony |
```

Rationale: D4 — wave-table mirrors PROGRESS truth, and the
d-lineage routing to B1 is visible to a reader scanning the
table alone.

### Edit 5: `docs/tranches/AY-II/AY-II.md` — insert predicate line above §Wave summary

Before (verbatim, `AY-II.md:140-142`):

```
## Wave summary

| Wave | Spec | Headline | Opens after | Status |
```

After:

```
## Wave summary

W0' and W1-W5 all gate on B1 (prelude annex) close. See
`PATH-FORWARD.md` for program order:
B1 close → AY-II.W0' close → AY-II.W1-W5 sequential.

| Wave | Spec | Headline | Opens after | Status |
```

Rationale: D6 — a reader scanning the wave table sees the B1
predicate without having to cross-reference PATH-FORWARD.

### Edit 6: `docs/tranches/AY-II/AY-II.md` §Plan-audit findings — new subsection

Before (verbatim, `AY-II.md:207-218`, just before
`### Downstream-tranche audit`):

```
### Scoped deferrals correctly routed (no action needed)

6. **W2's `scale_interop_tailwind` calc-evaluator gap** →
   destination BA (calc-evaluator workstream). W2 plan explicit
   + BA plan inherits. Clean.
7. **W2 OUT-OF-SCOPE rows** (`CounterStyleRule`, `ScopeRule`,
   etc.) — zero matches in declared fixtures; admission via
   `genericAtRule` fallback; typed parity in BA scope. Clean.

### Downstream-tranche audit
```

After:

```
### Scoped deferrals correctly routed (no action needed)

6. **W2's `scale_interop_tailwind` calc-evaluator gap** →
   destination BA (calc-evaluator workstream). W2 plan explicit
   + BA plan inherits. Clean.
7. **W2 OUT-OF-SCOPE rows** (`CounterStyleRule`, `ScopeRule`,
   etc.) — zero matches in declared fixtures; admission via
   `genericAtRule` fallback; typed parity in BA scope. Clean.

### d-lineage scope-creep rationalisation

W0'.d4-d7 landed edits to `.cargo/config.toml` + proc-macro
dev-deps (`PROGRESS.md:287-310`). W0 §File bounds declared
`.cargo/config.toml` and `scripts/*` out-of-bounds
(`waves/W0.md:75-81`). The pivot was documented in
`audit/W0p-infra-root-cause.md` +
`audit/W0p-infra-fix-plan.md` +
`audit/W0-iter-surface-verification.md`, and the tranche
response was to promote B1 from scaffold to authoritative
prelude annex. Per SPEC §new-tranche-new-doc, a mid-tranche
scope pivot opens a new letter — B1 is that letter. W0'
does not re-audit d4-d7 at close; B1.W0 owns the re-audit
(`docs/tranches/B1/waves/W0.md`).

### Downstream-tranche audit
```

Rationale: S1 + S2 — names the scope creep explicitly and
makes the B1 pivot's coverage of d4-d7 legible.

### Edit 7: `docs/tranches/AY-II/PATH-FORWARD.md` §2 — add Unknown retirement step

Before (verbatim, `PATH-FORWARD.md:49-60`):

```
### 2. AY-II.W0' close ceremony

After B1 closes, finish W0' in one uninterrupted sequence:

1. Run bootstrap regen.
2. Run double-regen idempotency.
3. Retire the W0'.a compose-boundary aliases and shim surfaces.
4. Capture fresh expands for JSON, CSS, Sheets, and BBNF.
5. Run the fat-LTO 5-bench matrix.
6. Capture samply on the four primary grammars.
7. Run `nm` on the bench binaries.
8. Update `PROGRESS.md`, `AY-II.md`, and `waves/W0p.md` to mark W0'
   closed.
```

After:

```
### 2. AY-II.W0' close ceremony

After B1 closes, finish W0' in one uninterrupted sequence:

1. Run bootstrap regen.
2. Run double-regen idempotency.
3. Retire the W0'.a compose-boundary aliases and shim surfaces.
4. Capture fresh expands for JSON, CSS, Sheets, and BBNF.
5. Run the fat-LTO 5-bench matrix.
6. Capture samply on the four primary grammars.
7. Run `nm` on the bench binaries.
8. Verify `<Grammar>Value::Unknown` retirement per grammar per
   `AY-II.md` §Plan-audit findings bullet 2; record the
   per-grammar exception ledger (`W0p-PAUSE-SNAPSHOT.md:87-89`
   marked this unresolved at pause).
9. Update `PROGRESS.md`, `AY-II.md`, and `waves/W0p.md` to mark
   W0' closed.
```

Rationale: U2 — Unknown retirement is a named W0'.b
deliverable whose outcome was unresolved at PAUSE; give it
an explicit close-ceremony step.

### Edit 8: `docs/tranches/AY-II/AY-II.md` §Plan-audit findings — new Debt ledger mirror subsection

Before (verbatim, `AY-II.md:197-206`, just before
`### Scoped deferrals correctly routed`):

```
5. **W3 `TODO AU.6.7` aggregate / variant-tagged forms** — W3's
   grammar has `cell_ref`, `range_end`, and similar rules whose
   declared canonical target is an aggregate tuple or tagged
   union that the current `type_annotation` grammar cannot
   express. The deferral ledger in W3.a is specified but the
   destination tranche is not explicitly named in the wave text.
   **Action**: W3 plan update names BA as the destination (BA
   owns `type_annotation` grammar extensions per BA.md §Scope).

### Scoped deferrals correctly routed (no action needed)
```

After:

```
5. **W3 `TODO AU.6.7` aggregate / variant-tagged forms** — W3's
   grammar has `cell_ref`, `range_end`, and similar rules whose
   declared canonical target is an aggregate tuple or tagged
   union that the current `type_annotation` grammar cannot
   express. The deferral ledger in W3.a is specified but the
   destination tranche is not explicitly named in the wave text.
   **Action**: W3 plan update names BA as the destination (BA
   owns `type_annotation` grammar extensions per BA.md §Scope).

### AUDIT-D 15-item debt ledger mirror

`audit/AY-II-AUDIT-D-predecessor-successor.md` enumerates 15
cross-tranche debt items (11 AY-II internal, 4 external).
Source is authoritative; this is the enumeration mirror for
status-scanning purposes. Transcribe on next editorial pass;
currently the count is tracked but individual items are not
surfaced outside the audit doc. See U2 / DC1 in
`docs/tranches/meta-audit/03-tranche-drift.md`.

### Scoped deferrals correctly routed (no action needed)
```

Rationale: DC1 — at minimum, name the debt-ledger as
load-bearing and point the reader to the source doc.
Full enumeration is a follow-on (time-constrained meta-audit
did not re-read the 284-line audit tail to transcribe 15
items verbatim).

### Edit 9: `docs/tranches/B1/B1.md` — add Cross-tranche debt clarification (not scope extension)

Boundary clarification only; does not extend B1 scope. B1's
thesis ("no runtime architecture") holds as-written.

Before (verbatim, `B1/B1.md:146-152`):

```
## Cross-tranche debt

- Runtime parity, fused-substrate closure, projection/materializer
  totality, and all semantic-wave work remain in AY-II.
- If B1 surfaces runtime regressions while proving bench/bootstrap/
  profile surfaces, those regressions route to AY-II immediately with
  artefact citations.
```

After:

```
## Cross-tranche debt

- Runtime parity, fused-substrate closure, projection/materializer
  totality, and all semantic-wave work remain in AY-II.
- If B1 surfaces runtime regressions while proving bench/bootstrap/
  profile surfaces, those regressions route to AY-II immediately with
  artefact citations.
- B1 inherits the **infra** scope that W0'.d4-d7 landed on
  master pre-B1 (`AY-II/PROGRESS.md:287-310` + `audit/W0p-infra-*`).
  B1.W0 re-audits those landings on current repo state. This is
  not a prelude-annex successor debt tree (SPEC §B0 prelude
  invariant: prelude annexes carry no successor debt tree); it
  is a one-time inheritance of the infra pivot that opened B1.
- B1 does not inherit B0's `bbnf-analysis` nightly-rustc ICE
  signal — that is a signal to be measured by B1's routine
  surface, not a runtime fix to carry. Any actual workaround
  beyond `.cargo/config.toml` aliasing routes to AY-II or
  later.
```

Rationale: Boundary clarification answers focus-point #10
("does B1's thesis hold, or does it leak runtime concerns?").
The `bbnf-analysis` ICE mitigation in d7's `iter-check`
alias is **infra**, not runtime — flipping a workspace
`--exclude` flag in `.cargo/config.toml`. B1 thesis holds.

## B0 FINAL reconciliation

B0 FINAL.md §W0 close evidence
(`B0/FINAL.md:36`) claims:

> `cargo iter-check` warm **0.16 s** vs baseline
> `cargo check --workspace` warm **7.16 s** ≈ 45×.

`AY-II/audit/W0-iter-surface-verification.md:1-117` proves:

- Post-d7 warm: 0.14 s (`PROGRESS.md:318`).
- The 0.16 s measurement in B0 FINAL was taken against a
  `target/` that had already been primed by a prior
  `--workspace` compile that went through gorgeous's proc-macro
  wall (`W0-iter-surface-verification.md:73-78`).
- A cold worktree does not reproduce B0's warm number without
  the d7 fix.

B0's 45× headline was **valid under B0's measurement
conditions** (warm-over-warm, comparing B0's `iter-check`
alias vs `cargo check --workspace`). It was **invalid as a
general claim** — a fresh clone cannot reproduce it without
d7's alias narrowing, because `--workspace` still activates
gorgeous's full derive cascade.

AY-II.PROGRESS.md:313-322 already records the "d7 restored
45× warm" framing post-infra-fix. A B0 FINAL errata is
warranted:

**Proposed B0 FINAL errata** (not added in this audit — B0 is
closed and I'm flagging for editorial disposition):

Add to `B0/FINAL.md` §W0 close evidence, under the 45×
bullet:

```
> **Errata (post-B1 pre-announce, 2026-04-22)**: The 0.16 s
> warm `iter-check` measurement holds in the target state B0
> left behind, but does NOT reproduce on a fresh
> worktree or after a clean `target/`. The proximal cause is
> `iter-check`'s `--workspace` alias forcing gorgeous's six
> `#[derive(Parser)]` sites through proc-macro expansion, which
> costs >10 min cold.
> AY-II.W0'.d7 (`700501f5`,
> `docs/tranches/AY-II/PROGRESS.md:299-322`) restored
> reproducible warm=0.14 s by narrowing the alias to exclude
> gorgeous + bbnf-bootstrap + bbnf-analysis + bbnf-lsp. B1
> re-audits the full routine-surface claim against
> post-d7 repo state.
```

Decision: B0 FINAL errata is a **light-touch** correction
(not a re-open). Recommend applying it; agent 4 or user
executes if agreed.

## Summary table

| # | Finding | Severity | Proposed destination | Est. LOC change |
|---|---|---|---|---|
| D1 | `waves/W0p.md` omits d1/d3-d7 lineage | **high** — wave spec ↔ PROGRESS divergence | Edit 1 (append d-lineage amendment) | +40 lines |
| D1b | d2 never named | medium (reader cannot reconstruct) | Edit 1 (row in d-lineage table) | 0 (in Edit 1) |
| D2 | AY-II.md bullet 4 already discharged but not annotated | low | Edit 3 | +1 line |
| D3 | PATH-FORWARD alias list missing `_ValueBuilderShim` | medium | Edit 2 | +3 lines |
| D4 | W0' wave-table cell doesn't surface d-lineage | medium | Edit 4 | +1 line (replace) |
| D5 | AUDIT-{A,B,C,D} (AY-I pass-I) placed under AY-II/audit | low; provenance ambiguity | follow-on: `git mv` to AY-I/audit/ | 0 (file moves only) |
| D6 | Wave summary lacks B1-predicate line | medium | Edit 5 | +4 lines |
| S1 | d4-d7 crossed B0 boundary mid-wave | high (process); mitigated by B1 | Edit 6 | +12 lines |
| S2 | B1-as-scope-pivot honored retroactively | low; SPEC intent upheld | no edit | 0 |
| S3 | W1-W5 pre-W0' wording | no-op per AUDIT-C | no edit | 0 |
| U1 | d2 silent skip | low | Edit 1 | 0 (in Edit 1) |
| U2 | `<Grammar>Value::Unknown` retirement undestined | medium | Edit 7 | +3 lines |
| DC1 | AUDIT-D 15-item debt ledger not mirrored | **high** — debt is tracked only by audit reference | Edit 8 | +10 lines (stub) |
| P1 | AUDIT-{A,B,C,D} provenance (AY-I→AY-II dir) | low; covered by D5 | follow-on | 0 |
| P2 | `W0p-PAUSE-SNAPSHOT.md` at tranche root, not audit/ | low | follow-on | 0 |
| B0-FINAL | 45× claim reproducibility caveat | medium | B0 FINAL errata (flagged, not applied here) | +6 lines |
| B1 | boundary clarification for d4-d7 inheritance | low (optional clarity win) | Edit 9 | +10 lines |

Total proposed edit surface: **~90 lines** added across 4
files (`AY-II/AY-II.md`, `AY-II/PATH-FORWARD.md`,
`AY-II/waves/W0p.md`, `B1/B1.md`). Three follow-on
disposition items (AUDIT file moves, PAUSE SNAPSHOT move,
B0 FINAL errata) deferred to agent 4 or manual pass.

---

Agent 3 of 4 — meta-audit 03 complete.
Authored 2026-04-22. Hard 25-min cap honored.
Does not touch: `docs/instructions/` (agent 2 scope), session
transcripts (agent 1 scope), or `B1/B1.md` scope beyond the
boundary-clarification Edit 9.
