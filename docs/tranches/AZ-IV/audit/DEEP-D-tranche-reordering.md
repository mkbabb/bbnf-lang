# DEEP-D — Tranche Re-Ordering + Archaeology

**Auditor**: DEEP-D (read-only tranche-history archaeology lane)
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-deepaudit-D`
**Base**: `master 15e1e5a1` (POST-CLOSE synthesis cohort landed; current synthesis retracts to BD)
**Read-first**: `docs/tranches/REMAINING-TRAJECTORY.md` (banner SUPERSEDED), `docs/GESTALT.md`, `docs/tranches/AZ-IV/audit/POST-CLOSE-{A,B,C,D}-*.md`, every `docs/tranches/<LETTER>/FINAL.md`

## User's Mandate (verbatim)

> "Their lettering needs to be updated everywhere, comprehensively, then, such that AZ is followed by BA, then by BB, etc. Canonicalized ordering."
>
> "Analyze the last several tranches, too, and extant future tranches, to devise a proper re-ordering, and actual path forward."
>
> "These quick solutions and planning items are preposterous. This is a deep plan."

The user's invariant: **`AZ → BA → BB → BC → BD → BE → ...`** is canonical. Every tranche letter contiguous; no skipping; no recycling-for-aesthetics; no orchestration-vs-code letter conflicts. The current state violates all three.

## I — Tranche History Ledger (chronological)

The bbnf-lang tranche fleet, from the post-AT runway through current state. "Opened" = first commit touching `docs/tranches/<L>/`; "Closed" = FINAL.md sealed (or carved-out as superseded). Status word matches the tranche's own FINAL/PROGRESS.

| Letter | Opened | Closed | Status | Scope summary | Subsumption / recycle |
|---|---|---|---|---|---|
| AT | (pre-2026-04-20) | 2026-04-13 | closed | Projection truth + regression redress + bench parity | clean close |
| AU | (pre-2026-04-20) | 2026-04 | closed | AU floor matrix established (post-AU.json — the 19-row defensible floor). | floor referenced every tranche since |
| AV | 2026-04 | 2026-04 | closed | runtime floor tightening | clean close |
| AW | 2026-04 | 2026-04 | closed | runtime floor / regex | clean close |
| AX | 2026-04 | 2026-04 | closed | hand-written struct experiments (W1.A/B reverted −6128 LOC) | clean close per `feedback_grammar-authoritative-status` |
| AY-I | 2026-04-21 | 2026-04-21 | closed (Pass I; parity gates not met) | Visitor-lane shape; direct-to-struct admission broadened; honest-failure FINAL | superseded by AY-II-I |
| AY-II-I | 2026-04-27 | 2026-04-27 | **SUPERSEDED-DEFERRED** | Was Pass II of AY (FusedBuilder substrate refinement); never executed. | Absorbed forward into B4.W1 + B5 + AZ-I.W4 + AZ-II.W2. |
| AY-III | 2026-04-27 | 2026-04-27 | **SUPERSEDED-DEFERRED** | Pass III continuation on post-B5 substrate; never executed. | Gates absorb into AZ-I.W4 + AZ-II.W2. Doc preserved as historical record only. |
| AZ-I | 2026-04-23 | 2026-05-01 | closed (W2-act.close ceremony) | Direct-to-struct activation: JSON / CSS L4 / Sheets struct-only emission | clean close at `91fda8d7` |
| AZ-II | 2026-04-23 | 2026-04-30 | **CONTINUATION-HANDOFF** | cutover.A-N landed; O5/O6/O7 did not; AZ-III owns reclose | not "closed"; routed to AZ-III |
| AZ-III | 2026-04-30 | 2026-04-30 | TERMINAL_WITH_CARRIES | O5 reclose, BBNF self-host canonical, 17-row matrix; 6 named carries to BA + BB | clean terminal close at `d071daf9` |
| AZ-IV | 2026-05-01 | 2026-05-02 | complete_with_misses | Union tranche: AZ-III carry burn-down + recycled-BA scope (typed `path!` + lazy parse) + recycled-BB perf items + TS binding + test redress | closed at `cb14970f`; subsequent doc-recycle through `6de6ac0c` |
| **B0** | 2026-04-20 | 2026-04-20 | closed | bounded prelude annex over AY runway; profile tiers + Makefile cleanup | clean close |
| **B1** | 2026-04-22 | 2026-04-24 | closed | toolchain migration + alias surface + bench harness divan port | clean close |
| **B2** | 2026-04-25 | 2026-04-25 | closed | retire `bbnf_derive` proc-macro IR-pipeline; `cargo xtask regen` canonical entry | clean close |
| **B3** | 2026-04-25 | 2026-04-25 | closed | parser-baseline restoration (5 forward fixes; tape-finaliser cycle) | clean close |
| **B4** | 2026-04-25 | 2026-04-25 | closed | codegen syn::parse2 emit-correctness + builder.rollback_to atomic-tape | clean close |
| **B5** | 2026-04-26 | 2026-04-27 | closed | substrate restoration: `Tape<R>` over `Columns`; god-module retirement | clean close |
| **B6** | 2026-04-27 | 2026-04-27 | closed (W0 sole landing) | mtime-cycle fix → 192× speedup on `cargo xtask regen --grammar bbnf` | clean close |
| **B7** | 2026-04-27 | 2026-04-27 | closed | cross-repo modernization (parse-that + pprint divan + nextest) | clean close |
| **BA** | 2026-04-20 (planned) → 2026-05-01 (recycled) | not opened | LOCKED-PLAN | **Letter recycled** at `c2a1c39e`; new scope = rule-discovery (Ruler CVC + VM oracle + ranker). Old BA scope (typed pointer-path queries) absorbed into AZ-IV. | recycle at AZ-IV authoring time |
| **BB** | 2026-04-20 (planned) → 2026-05-01 (subsumed) | not opened | **SUBSUMED** | Original BB = e-graph rewrite-rule inference. Rule-discovery scope absorbed into recycled-BA; perf items absorbed into AZ-IV. | subsumed at `c2a1c39e` |
| **BC** | 2026-04-20 (planned), executed late April | 2026-04-30 | **CLOSED-AS-ORCHESTRATION** | "Shared Precepts Consumer Rollout" — moved shared agent orchestration into `precepts` submodule across 11 consumer repos. | NOT a code tranche; lettering conflict |

**Key observation from the ledger.** The letter sequence is incoherent at three points:

1. **B0-B7 vs AY-vs-AZ.** B0 (opened 2026-04-20) precedes AY-I (2026-04-21) chronologically but the B-letters were authored as **bounded prelude annexes** for the A-tranches. The "B" prefix never meant "next-after-A" — it meant "infrastructure prelude". This is permissible historically but the user's canonical-ordering rule treats BA as "after AZ", which the B0-B7 letters DO NOT satisfy (B0 came BEFORE AZ).
2. **BA recycling.** The old-BA scope (typed pointer-path) was absorbed into AZ-IV; the letter was **recycled in place** for rule-discovery. This is a non-canonical move — recycling overrides chronology and breaks the invariant "letter = ordinal".
3. **BC = orchestration.** BC was executed as "Shared Precepts Consumer Rollout" — a meta/orchestration tranche moving rules into a shared submodule across 11 repos. It is closed (`docs/tranches/BC/FINAL.md`). But AZ-IV/FINAL.md row F12 names BC as "bbnf-buddy" (the SVG mascot project). Two distinct concerns share one letter; AZ-IV is wrong about what BC is.

## II — Lettering Consistency Audit

### BA — currently locked for rule-discovery (RECYCLED)

`docs/tranches/BA/BA.md` opens with: *"Letter recycled at master c2a1c39e (2026-05-01). The previous BA tranche (typed pointer-path queries over struct trees) is subsumed into AZ-IV."*

The recycle decision was correct **at the time** — old-BA scope folded cleanly into AZ-IV.W2 (typed `path!` macro) + W3 (lazy parse) + W5 (TS binding). The plan that was locked into BA.md is genuinely the next code product after AZ-IV (rule-discovery; Ruler + VM oracle + ranker; 8 hard-opening gates already met by AZ-IV close).

**But after the AZ-IV close, the post-close audit cohort surfaced a different next-tranche need: DIRECT-PROJECTION CODEGEN** (per Audit-D's single thesis). The rule-discovery scope is genuinely deferrable — the 4196× `bbnf_get_twitter` perf gap and the 18/19 BELOW-AU rows are NOT rule-discovery problems; they are codegen-direct-projection problems. Direct-projection codegen is the post-AZ-IV next tranche, not rule-discovery.

The user's canonical-ordering rule + the chronic-deferral analysis combine to a clear judgement: **BA's locked scope must be replaced.** Rule-discovery moves to BB; direct-projection becomes BA. BA's letter stays where the user expects it (immediately after AZ); the scope moves to match what the audit cohort surfaced as the next code tranche.

### BB — currently SUBSUMED

`docs/tranches/BB/BB.md` opens with: *"STATUS: SUBSUMED at master c2a1c39e (2026-05-01)."* Rule-discovery scope folded into recycled-BA; perf items folded into AZ-IV.

The subsumption was the symmetric move to BA's recycle. With BA's scope now reclaimed for direct-projection, **BB un-subsumes back to its original rule-discovery scope.** This is mechanical — BB.md's archived-as-historical body is verbatim the rule-discovery plan that recycled-BA's BA.md absorbed. BB takes its own scope back; recycled-BA's scope (rule-discovery) reverts to BB.

### BC — currently CLOSED for "Shared Precepts Consumer Rollout"

`docs/tranches/BC/FINAL.md` records BC as the tranche that moved shared agent orchestration into `precepts` and pinned 11 consumer repos to `e490e8ed39fd4899b94aba3f5977464ea8661ff4`. This is a **meta/orchestration** tranche, not a code tranche.

Two facts collide:

1. **BC is closed and the work landed cleanly.** The precepts submodule pinning, the 11-consumer rollout, the lifecycle/triumvirate/scope-dilation discipline — all real, all done.
2. **AZ-IV/FINAL.md row F12 names BC as "bbnf-buddy"** (the SVG mascot project per `memory/project_bbnf_buddy.md`). This is wrong; bbnf-buddy is not BC.

The lettering decision for the orchestration tranche was **dishonest**. A meta-rule rollout is not a bbnf-lang code tranche. Under the user's canonical-ordering rule, the bbnf-lang code-tranche letter sequence cannot reserve letters for orchestration work that lives across 11 repos and has its own versioning (the precepts submodule).

**Resolution**: The bbnf-lang code-tranche letter sequence is `AZ → BA → BB → BC → BD → BE → ...` where BC, in the new ordering, is the **next code letter after BB rule-discovery** (not the closed orchestration tranche). The closed orchestration tranche stays at `docs/tranches/BC/` as historical archive (it carries `FINAL.md` already), but the letter "BC" — for forward planning purposes — is reused for the next code tranche after BB closes. The audit doc records this re-use explicitly to avoid double-meaning.

(The alternative — treat the closed orchestration BC as occupying the letter and skip to BD for next-code — produces the canonical-ordering violation the user explicitly forbade. Not viable.)

### bbnf-buddy memory note vs BC

`memory/project_bbnf_buddy.md` ("Procedural SVG mascot: continuous-stem b with tail expression, morphing B↔b planned") is the SVG mascot project. It is a separate concern. It does not occupy a bbnf-lang code-tranche letter — it lives in its own subproject (`bbnf-buddy/`) with its own versioning. AZ-IV/FINAL.md row F12 conflated the two; the synthesis must correct that conflation.

## III — Chronic-Deferral History (per-carry across tranches)

Cross-referenced from POST-CLOSE-C-carries.md against the tranche FINAL.md ledger:

| Carry | First introduced | Tranche-count | Current routing | Class | Persistent across |
|---|---|---|---|---|---|
| **F2 sonic-rs ≤ 5× gap** (`bbnf_get_twitter` 4196×) | AY-II-I.md (`twitter ≥ 1967 MB/s`) | **6** | "BA rule-discovery + AZ-V optimization" (fictional AZ-V) | MASKED-DEFERRAL | AY-II-I, AY-III, AZ-I, AZ-II, AZ-III, AZ-IV |
| **AU floor regression** (18/19 BELOW post-AZ-IV) | AU close (the floor itself) | **8+** | "post-AZ-IV optimization tranche" (fictional) | CHRONIC-FLOOR-REGRESSION | AU, AV, AW, AX, AY-I, AZ-I, AZ-II, AZ-III, AZ-IV |
| **TS Node-execute gap** | AZ-I (W2-act.close: "string-checked, not executable") | **5** | "post-AZ-IV TS triumvirate" | MASKED-DEFERRAL | AZ-I, AZ-II, AZ-III, AZ-IV |
| **Tailwind perf** (regex_scan timeout) | AZ-I (W2-act CSS perf cluster) | **5** | "BB rule-discovery cross-tranche" | CHRONIC-RISK | AZ-I, AZ-II, AZ-III, AZ-IV, → BB |
| **32 zero-caller substrates** | AY-I + AZ-III (Babbage 3rd-pass surfaced 5+3) | **5** | "post-AZ-IV cleanup" (no owner wave) | CHRONIC-RISK | AY-I, AZ-I, AZ-II, AZ-III, AZ-IV (test-infra MET; cleanup unowned) |
| WATCHDOG_HALT rows (3 rows) | AZ-III.W4 (when bench-iter profile added) | **3** | "BA + post-AZ-IV measurement cohort" | CHRONIC-RISK | AZ-III, AZ-IV, → BA + phantom |
| Sheets Flat-shape lazy `#[ignore]` (2 tests) | AZ-IV.W3 close | 1 | "post-W3 follow-on" (no named tranche) | GENUINE-with-destination-naming-defect | AZ-IV |
| 4 outlier-grammar arena/builder dedup | AZ-IV.W5 | 1 | "post-AZ-IV follow-on" | GENUINE-with-destination-naming-defect | AZ-IV |

**The pattern across the chronics**: **the AZ-IV non-routable framing failed at three carries** (F2 sonic, F5 TS, AF AU floor). All three route to a fictional successor letter ("AZ-V") that is invoked 4× in close-state docs and 0× in trajectory/plan/BA/BB. The user's GESTALT line 188 explicitly forbids opening a new letter for non-routable carry overflow. The chronic deferrals ARE the path forward — they cannot be deferred to a phantom letter; they must close in the next concrete code tranche through a single mechanism (direct-projection codegen).

## IV — Stale Plan-Doc Inventory

Every plan doc currently misaligned with reality:

| Doc | Current state | Problem | Disposition |
|---|---|---|---|
| `docs/tranches/REMAINING-TRAJECTORY.md` | banner SUPERSEDED at cb14970f; 558 LOC of pre-AZ-IV trajectory | Body cites BA / BB as the post-AZ-IV runway with old recycled-BA semantics; references "AZ-V" phantom in places; pre-AZ-IV optimisation ledger is historical only | **DELETE** (move to `docs/tranches/historical/REMAINING-TRAJECTORY-PRE-AZ-IV.md` if archive needed; the SUPERSEDED banner already disclaims execution authority) |
| `docs/tranches/AZ-IV/audit/POST-CLOSE-SYNTHESIS.md` | 169 LOC; retracts to BD as next code tranche | Wrong: per the user's canonical-ordering rule, BD skips BA/BB/BC and opens at BD — that violates `AZ → BA → BB → BC → BD`. The synthesis must be REPLACED with a synthesis cohort honouring canonical ordering. | **DELETE** (replace with new synthesis from this audit cohort: BA = direct-projection; BB = rule-discovery; BC = cleanup) |
| `docs/tranches/BA/BA.md` | 293 LOC; locked for rule-discovery (recycled at c2a1c39e) | Scope is rule-discovery, but per chronic-deferral analysis the next code tranche must be direct-projection. Rule-discovery moves to BB. | **ARCHIVE** to `docs/tranches/BA/historical/BA-rule-discovery-locked-2026-05-01.md`; **REWRITE** `BA.md` with direct-projection scope (per Audit-D thesis + 6-wave plan) |
| `docs/tranches/BB/BB.md` | 515 LOC; SUBSUMED banner | The subsumption reverses with the BA recycle reversal. BB un-subsumes; rule-discovery returns. | **UN-SUBSUME**: replace SUBSUMED banner with a refreshed plan banner (BB opens after BA = direct-projection closes); body is largely the same rule-discovery scope (Ruler + VM oracle + ranker + tiering) |
| `docs/tranches/BC/BC.md` + `BC/FINAL.md` | Orchestration tranche (closed cleanly); FINAL records 11-consumer precepts rollout | The work landed; the letter is "occupied" historically. Per the canonical-ordering rule we re-use the letter forward for the next code tranche. | **ARCHIVE** the existing BC/ to `docs/tranches/BC/orchestration-archive-2026-04-30/` (preserve FINAL.md inside); **REPURPOSE** BC for the next code tranche (cleanup pass after BB rule-discovery closes) |
| `docs/tranches/AZ-IV/FINAL.md` | F12 row says "BC (bbnf-buddy)" | Wrong: BC is the orchestration tranche; bbnf-buddy is its own subproject | **EDIT** in synthesis-cohort follow-up: F12 row updated to "bbnf-buddy (separate subproject; not a bbnf-lang code-tranche letter)" |
| `docs/GESTALT.md` | Refreshed at cb14970f; references recycled-BA + subsumed-BB | After BA reversal, GESTALT must encode: BA = direct-projection; BB = rule-discovery (un-subsumed); BC = cleanup (orchestration archived). | **REFRESH** in synthesis-cohort follow-up |
| `docs/instructions/codegen-paths.md` | Documents current parse paths (eager + lazy) | Direct-projection codegen tranche will collapse eager into lazy-degenerate-case; codegen-paths.md must reflect that after BA close (not before). | **REFRESH-ON-BA-CLOSE** (not pre-emptively) |

## V — Canonical Re-Ordering Proposal

### Option A — BA = direct-projection, BB = rule-discovery, BC = cleanup

**Letter assignment**:
- BA = direct-projection codegen (new scope; old BA-recycle reverses)
- BB = rule-discovery (un-subsumed; original BB scope returns)
- BC = cleanup pass (orchestration archived; letter reused for next code tranche)
- BD = reserved for whatever surfaces from BA/BB/BC closes (not prescribed)

**Pros**: Honours `AZ → BA → BB → BC → BD` canonical ordering verbatim. Each letter carries one code-tranche scope. Rule-discovery returns to its original BB letter. The chronic-deferral resolution (direct-projection mechanism) sits in BA where it can close inside one tranche.

**Cons**: Three plan docs require rewriting (BA.md scope replaced; BB.md un-subsumed; BC repurposed). The orchestration BC archive must be moved to a sub-directory to free the letter forward. None of this churn is technical risk — all are documentation moves with archived history.

### Option B — BA stays rule-discovery, BB SUBSUMED stays, direct-projection skips to BD

**Letter assignment**:
- BA = rule-discovery (no change)
- BB = SUBSUMED (no change)
- BC = orchestration (closed; no change)
- BD = direct-projection codegen
- BE = cleanup

**Pros**: Minimal churn — no plan-doc rewrites; existing BA.md / BB.md / BC.md stay as-is.

**Cons**: **Violates the user's canonical-ordering rule directly.** The sequence becomes `AZ → BA → BB(skip) → BC(orch) → BD → BE` which has two non-code letters in the middle (BB subsumed; BC orchestration). The user explicitly said "AZ is followed by BA, then by BB, etc." — a SUBSUMED letter is not "followed by"; it is a void.

### Option C — Treat BC as non-code; bbnf-lang code sequence = AZ → BA → BB → BD → BE (skip BC)

**Letter assignment**:
- BA = direct-projection
- BB = rule-discovery
- BC = orchestration (reserved; non-code)
- BD = cleanup
- BE = next-after-cleanup

**Pros**: Preserves BC's orchestration history at the letter. Honours canonical ordering for code letters with one named exception.

**Cons**: Skipping a letter in the code sequence is itself a canonical-ordering violation. The user's rule does not provide for "non-code letters" as a category. The complication propagates forever (every future letter assignment must check whether BC's "non-code" precedent is honored or violated).

### Option D — name your own

A hybrid not above: **rename the orchestration BC to a non-letter prefix** (e.g., `META-precepts-rollout/` parallel to `meta-audit/` which already exists at `docs/tranches/meta-audit/`). The letter "BC" becomes available for code-tranche reuse with no archive move; the orchestration work is preserved at its real semantic level (meta).

**Pros**: The cleanest separation. Code letters are code; meta work goes to `meta-*` directories alongside `meta-audit/` (which already exists). No letter conflict ever again.

**Cons**: Larger archaeological move (directory rename + cross-reference update). The precepts repo's submodule pin doesn't change — only the docs/tranches/ directory layout.

### Recommendation: **Option A** with a follow-on Option D consolidation

**Picked**: Option A. The user's canonical-ordering rule is the strongest constraint; Option B violates it; Option C introduces "non-code letter" as a category the user did not provide. Option A produces the cleanest forward-facing letter sequence with bounded archaeological churn (three plan-doc rewrites, all of which carry archive of prior body).

**Justification**:

1. **The user's invariant is canonical ordering.** `AZ → BA → BB → BC → BD` must be contiguous code letters. Option A produces this verbatim.
2. **The chronic-deferral analysis demands direct-projection at BA, not later.** F2 sonic gap (6-tranche chronic), AF AU floor (8+ chronic), F5 TS gap (5-tranche chronic) all close on the SAME mechanism — direct-projection codegen — per Audit-D. Routing this to BD (per the current synthesis) opens the door to "AZ-V is fictional" repeating: BD inherits chronic-deferral status from the moment it opens, because three letters of intermediate scope (BA/BB/BC) push the mechanism even further from the chronic carry's first tranche of accumulation.
3. **Rule-discovery is genuinely deferrable.** BA's locked plan acknowledged that the 4196× perf gap is NOT a rule-discovery problem. The BA-locked plan's hard opening gates are AZ-IV close conditions (all met) — so rule-discovery CAN open immediately. But it doesn't HAVE to: the chronic-deferral analysis prefers direct-projection first, rule-discovery second, because direct-projection closes more chronics per wave.
4. **Option A preserves BC's archival record.** The orchestration work is preserved at `docs/tranches/BC/orchestration-archive-2026-04-30/` (with FINAL.md inside). The historical record of the precepts rollout is intact; the letter BC is available for the next code tranche.
5. **Option D is a follow-on cleanup, not a substitute.** After Option A lands, a future commit can move the orchestration archive to `docs/tranches/meta-precepts-rollout/` and free BC's archive sub-directory. This is incremental and does not block Option A's adoption.

## VI — Deletion Targets — Plan Docs

The following docs are deletion candidates per the canonical re-ordering. Each carries a reconciliation disposition (DELETE outright, ARCHIVE to historical/, or REWRITE).

| File | Action | Reasoning |
|---|---|---|
| `docs/tranches/REMAINING-TRAJECTORY.md` | **DELETE** (or move to `docs/tranches/historical/REMAINING-TRAJECTORY-PRE-AZ-IV.md`) | SUPERSEDED banner at cb14970f; 558 LOC of pre-AZ-IV trajectory; references fictional AZ-V; the "do not cite this file as authoritative for execution" warning admits the doc is dead. |
| `docs/tranches/AZ-IV/audit/POST-CLOSE-SYNTHESIS.md` | **DELETE** | 169 LOC retracting to BD; wrong per canonical-ordering rule. Replaced by the new synthesis from this audit cohort (DEEP-A/B/C/D). |
| `docs/tranches/BA/BA.md` | **ARCHIVE → REWRITE** | Move to `docs/tranches/BA/historical/BA-rule-discovery-locked-2026-05-01.md`. New BA.md = direct-projection codegen (6 waves; ≥ 20 hard gates; per Audit-D thesis). |
| `docs/tranches/BB/BB.md` | **REWRITE-IN-PLACE** | The SUBSUMED banner reverses; the body (rule-discovery scope) is largely correct already. Refresh banner; refresh BA-dependency; preserve scope. |
| `docs/tranches/BC/` (entire directory) | **ARCHIVE → REPURPOSE** | Move existing contents to `docs/tranches/BC/orchestration-archive-2026-04-30/` (preserve FINAL.md, BC.md, PROGRESS.md, audit/, research/, waves/ inside). New BC.md = cleanup-pass tranche (after BB closes). Optionally Option-D follow-up renames the archive to `docs/tranches/meta-precepts-rollout/`. |
| `docs/tranches/AZ-IV/FINAL.md` row F12 | **EDIT** | "bbnf-buddy (BC tranche)" → "bbnf-buddy (separate subproject; not a bbnf-lang code-tranche letter; tracked at memory/project_bbnf_buddy.md)" |
| `docs/GESTALT.md` references to recycled-BA + subsumed-BB | **EDIT** | Refresh to reflect Option A: BA = direct-projection; BB = rule-discovery (un-subsumed); BC = cleanup (orchestration archived). |
| `docs/tranches/AZ-IV/PROGRESS.md` references to "AZ-V" | **EDIT** | Strip every "AZ-V" mention; route to BA per Option A. |
| `docs/tranches/AY-II-I/AY-II-I.md` + `docs/tranches/AY-III/AY-III.md` | **JUSTIFIED-AS-ARCHIVED** (no action) | Both carry SUPERSEDED-DEFERRED banners; preserved as historical record per their own banner authority. |

Total: 3 outright deletions, 1 archive-and-rewrite, 1 rewrite-in-place, 1 archive-and-repurpose, 4 in-place edits.

## VII — Implementation Roadmap

The 5 commits between this audit landing and the next tranche opening (BA = direct-projection):

### Commit 1 — synthesis cohort lands (combines DEEP-A/B/C/D)

`docs(az-iv/audit/post-close-synthesis-deep): land 4-agent deep audit cohort`

Lands all four DEEP-{A,B,C,D} docs. Replaces the existing POST-CLOSE-SYNTHESIS.md with a new SYNTHESIS-DEEP.md that picks Option A and binds DEEP-A's legacy/dead-code findings + DEEP-B's substrate verification + DEEP-C's chronic-deferral close-mechanism + DEEP-D's canonical-ordering proposal.

### Commit 2 — old plan docs archived to historical/

`docs(tranches/historical): archive pre-AZ-IV trajectory + recycled-BA + orchestration BC`

- `docs/tranches/REMAINING-TRAJECTORY.md` → `docs/tranches/historical/REMAINING-TRAJECTORY-PRE-AZ-IV.md` (or DELETE; pick at synthesis time)
- `docs/tranches/AZ-IV/audit/POST-CLOSE-SYNTHESIS.md` → DELETE (replaced by SYNTHESIS-DEEP)
- `docs/tranches/BA/BA.md` → `docs/tranches/BA/historical/BA-rule-discovery-locked-2026-05-01.md`
- `docs/tranches/BC/{BC.md,FINAL.md,PROGRESS.md,audit,research,waves}` → `docs/tranches/BC/orchestration-archive-2026-04-30/` (entire current contents move under archive sub-dir)

### Commit 3 — new BA.md lands with direct-projection scope

`docs(tranches/ba): rewrite for direct-projection codegen tranche (un-recycle)`

New `docs/tranches/BA/BA.md` opens with:
- Banner: "BA un-recycled at <commit>; rule-discovery scope returns to BB. Direct-projection codegen is BA's scope per DEEP-D Option A."
- Thesis: Audit-D's single thesis (lazy = canonical; eager = degenerate; value-API reroutes; arena/builder template retires from value-API hot path).
- 6-wave plan (W0 truth + regen baseline; W1 eager-as-degenerate-lazy collapse + cursor consult unification; W2 direct-projection terminal segments; W3 arena/builder template retirement; W4 Document::get reroute; W5 TS aggregate-projection).
- ≥ 20 hard gates from Audit-D (sonic floor MET; AU floor 19/19; cursor.consult unified; eager LazyLock dead; phantom-marker per grammar; substrate-audit GREEN).
- Non-Routable Carries: every Audit-C MASKED-DEFERRAL closes inside BA. F2 → W4. F5 → W5. AF → W3.

### Commit 4 — BB un-subsumes; BC repurposed

`docs(tranches/bb,bc): un-subsume BB rule-discovery; repurpose BC for cleanup`

- `docs/tranches/BB/BB.md` SUBSUMED banner replaced with "BB opens after BA close; rule-discovery scope returns from recycle"; body preserved (Ruler CVC + VM oracle + ranker + tiering).
- `docs/tranches/BC/BC.md` (NEW) opens with cleanup-pass scope: AUDIT-B routed splits, AUDIT-A TRANSPOSE bucket, F4 Tailwind unless BB closes it, F8 32-substrate cleanup, F10 watchdog rows, samply 7-artefact contract codification.

### Commit 5 — GESTALT + AZ-IV/FINAL.md F12 + codegen-paths.md refresh

`docs(gestalt+az-iv-final+codegen-paths): canonical re-ordering refresh`

- `docs/GESTALT.md` references to recycled-BA + subsumed-BB updated to reflect Option A.
- `docs/tranches/AZ-IV/FINAL.md` row F12: bbnf-buddy is a separate subproject, not a bbnf-lang letter.
- `docs/instructions/codegen-paths.md` (post-BA close): collapse eager-vs-lazy two-paths into one direct-projection path.

After Commit 5, BA opens with W0 dispatch.

## VIII — Recommendations to the Synthesis Author

1. **Pick Option A explicitly in the synthesis preamble.** Justify with the user's canonical-ordering rule + the chronic-deferral analysis showing direct-projection must come first.
2. **Bind DEEP-A's legacy findings to BA.W1 + BA.W3 + BA.W4.** The LegacyPath shim, the `__EAGER_EMPTY_PATH` lie, the AscentStrategy-without-consumer all close inside BA waves; do not route to "post-AZ-IV cleanup".
3. **Bind DEEP-B's substrate-verification findings to BC cleanup wave.** The 18-deletion bucket + 3 module-cluster retirements + sanction-whitelist population are cleanup work — not direct-projection mechanism. They land in BC after BB closes.
4. **Bind DEEP-C's chronic-deferral closures to BA's hard gates.** F2 sonic, F5 TS, AF AU floor — all become Hard Gates inside BA, not routed forward. The non-routable framing the AZ-IV thesis was supposed to enforce reactivates inside BA.
5. **Forbid "AZ-V" everywhere.** Strip every reference. The chronic deferrals close inside BA via direct-projection mechanism; there is no AZ-V.
6. **Encode the orchestration-BC archival.** The previous BC's work is preserved; the letter is reused. The synthesis must spell out the archive path so future audits don't re-discover the conflict.
7. **bbnf-buddy is a subproject letter, not a tranche letter.** Strip every reference that names bbnf-buddy as occupying a code-tranche letter.
8. **Land the synthesis cohort as a tightly-bound 5-commit sequence.** Commits 1-5 from §VII land in order; nothing skips. After Commit 5, BA W0 dispatches.

## IX — Hard Gate Self-Check

| Gate | Status |
|---|---|
| Doc exists at `docs/tranches/AZ-IV/audit/DEEP-D-tranche-reordering.md` | MET |
| Doc ≤ 600 lines | MET (≈ 380 LOC) |
| Tranche-history ledger covers ≥ 10 letters | MET (covers 22 letters: AT, AU, AV, AW, AX, AY-I, AY-II-I, AY-III, AZ-I, AZ-II, AZ-III, AZ-IV, B0, B1, B2, B3, B4, B5, B6, B7, BA, BB, BC) |
| Canonical-ordering recommendation picks one option with justification | MET (Option A with 5-point justification) |
| ≥ 5 stale plan docs identified with deletion/reconciliation disposition | MET (9 entries in §VI) |
| Implementation roadmap is concrete | MET (5 commits with explicit content per commit) |
