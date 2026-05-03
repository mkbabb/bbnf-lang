# Pass C Agent 6 — Cross-cut Analysis + Commit Chain

Date: 2026-05-03. Lens: cross-cuts spanning Pass-C scope, with the **commit chain analysis** as the primary deliverable.

---

## §A — Pass-C cross-cuts

### §A.1 — Docs ↔ tranches

Tranche docs reference each other heavily (per `feedback_no-metalanguage-docs` exception for tranche-internal-prose). Cross-references include:

- `docs/tranches/AY-I/PATH-FORWARD.md` cites the column-revert at AY-I.W1.
- `docs/tranches/AX/FINAL.md` cites AW-V's W3 close as "demonstrated thesis lost by W6".
- `docs/tranches/B0/`, `docs/tranches/B1/` reference each other as paired prelude annexes.
- `docs/tranches/BA/`, `BB/`, `BC/`, `BD/` (current restart drafts) reference each other extensively.
- `docs/tranches/archive/pre-restart-{BA,BB,BC}/` retains its own internal cross-references (now archaeological).

Disposition: **honoured-by-design**. Tranche metalanguage is the discipline.

### §A.2 — Audit ↔ tranches

Audit corpora cite tranche files extensively:

- `audit/CENSUS-2026-05-03.md` cites `crates/core/src/runtime/css_l4/builder.rs:713`, `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs`, etc.
- `audit/MODULES-2026-05-03.md` cites tranche-letter-by-tranche progression.
- `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md` cites the BA/BB/BC plan-set verbatim.
- `audit/PHASE-4-SYNTHESIS-2026-05-03.md` references the Phase-4 specification depth.
- `docs/tranches/meta-audit/06-commit-archaeology.md` IS the cross-reference index — it cites every tranche letter in tabular form.

Disposition: **honoured-by-design**. Audit-tranche coupling is the audit's job.

### §A.3 — Archive ↔ active

Per Lock 12, no active code references archive contents:

| Active surface | Reference to archive |
|---|---|
| `crates/core/src/` | None expected; Pass-A scope to verify |
| `crates/analysis/src/`, `crates/lsp/src/` | None expected |
| `Cargo.toml` | STILL references `crates/ser`, `crates/gorgeous` (Lock 12 unexecuted) |
| `Makefile` | No archive references found in line scan |
| `scripts/` | No archive references found |
| `docs/` | Cross-references `docs/tranches/archive/pre-restart-{BA,BB,BC}/` from new BA/BB/BC drafts (these are archive-of-prior-plan, not abrogated-code) |

Verdict: **violated** for `Cargo.toml` (the workspace `members` list); honoured-by-design for the BA/BB/BC plan-set archive cross-references.

### §A.4 — Sibling ↔ workspace

Sibling repos (parse-that, csc411, bbnf-buddy, gorgeous-external, pprint-external) couple to workspace as follows:

| Sibling | Coupling kind | Path |
|---|---|---|
| parse-that-rs | path-dep (workspace member, presumed external repo source) | `crates/parse-that/` (per CENSUS not in `members` list — check) |
| csc411 csp-solver | path-dep mirror | `crates/csp-solver/` |
| bbnf-buddy | brand only | none |
| gorgeous-external | active mirror at `crates/gorgeous/` (archive candidate) | crates path |
| pprint-external | docs reference at `docs/pprint/` only | docs path |
| precepts (submodule) | submodule | `docs/precepts/` |

Verdict: **honoured-mostly** — only the gorgeous coupling is fault, resolved by Lock 12 archive.

---

## §B — Commit chain analysis (the major deliverable)

### §B.1 — Total + breakdown

```
git log --oneline | wc -l          → 2621
git log origin/master..HEAD | wc -l → 1724  (unpushed)
```

Per the archaeology snapshot at 2026-04-22 the chain was 1,842 commits on master + 945 unpushed. Since then, ~700+ commits have landed (Phase-4 directive + Lock 14 codification + Phase-4 spec-depth re-draft for BA/BB/BC/BD + Phase-4 hardening audit for the plan-set + `pre-restart-*` archive ceremony + greenfield restart suite drafting). The current state is 2,621 commits.

### §B.2 — Per-era commit table

The archaeology + post-archaeology drift:

| Era | Letters | Commits | Era summary | Substrate at HEAD |
|---|---|---:|---|---|
| I (TextMate prelude) | none | ~25 | LSP + TextMate grammar; 3-year hiatus | dead — replaced |
| II (monorepo scaffold) | none formal | ~264 | monorepo structure + IR crate bootstrap | partial — `crates/ir/` survives |
| III (optimiser substrate) | F-W | ~280 | CSP, e-graph, regex HIR, NodeId, IndexMap determinism | LIVE — every Era III pivot survives |
| IV (tape-first) | X, Y, Z, AA-AU | ~185 | tape-first codegen; AU baseline | DEAD — tape severed (Lock 1); columns reverted (AY-I.W1); AU silent (Lock 8) |
| V (DTA/PSI rut) | AV, AW-I/II/III/IV/V, AX | ~572 | The 1,000-commit DTA arc; substrate-first/consumer-later | DEAD — DTA interpreter deleted at AX.W0b (~78K LOC reclaim); shape emitter substrate retained as view layer |
| VI (restart) | AY-I/II/III, AZ-I/II/III/IV, B0-B7, BA-BD | ~1,095 (incl. ~700 post-archaeology) | restart waves + prelude annexes + Phase-4 + greenfield restart | LIVE — current state |

### §B.3 — Load-bearing test

For each era, the question: *which commits' artefacts are still in the working tree at HEAD?*

| Era | Working-tree presence | Load-bearing | Archaeology |
|---|---|---|---|
| I | None — original code rewritten | 0 | 25 commits |
| II | Partial — `crates/ir/` carries Era II bootstrap, but rewritten | <50 | ~210 commits |
| III | Most — Era III techniques (delim-scan, regex HIR, NodeId, IndexMap) are LOAD-BEARING per archaeology Part D + the user's `perf-breakthrough-accuracy` memory citing AQ.5, etc. | ~250 | ~30 |
| IV | DEAD — tape substrate removed; AU bench corpus archaeological. Tape-first AE/AC/AA = ~50 commits archaeological. | 0 | ~185 |
| V | DEAD — DTA interpreter + walker fully deleted; shape emitter substrate KEPT as view layer (~50 commits' worth of substrate retained); the rest (572 - 50) ~520 commits archaeological. | ~50 | ~520 |
| VI | LIVE — current state. | ~1,095 | 0 (in flight) |

Estimated load-bearing: ~1,395 of 2,621 commits (53%). Estimated archaeology: ~1,225 of 2,621 (47%).

### §B.4 — Provenance preservation analysis

If commits are squashed, what is lost?

**Author intent.** Era III's pivots (AQ.5 EmissionTier deletion, AY-I.W1 column revert) are explicitly cited in memory items. Their commit subjects + bodies carry the *why*. Squashing collapses 32 AF/AG commits into 1 squashed commit; the per-commit "why-was-this-tier-resolution-attempted" archaeology vanishes.

**Bug-fix archaeology.** AU Bug 1, Bug 2, Bug 2b were closed at AV.0.x with named commits per the AV FINAL. Squashing AV-as-1-commit collapses the per-bug attribution.

**Performance-improvement attribution.** The `perf-breakthrough-accuracy` memory cites: delim-scan (Tranche F), bespoke regex HIR (Tranche W + bbnf-regex crate), IIFE elimination (Tranche W.4 + AQ + AR). Squash-by-era erases the per-tranche attribution.

**Reversal record.** Per archaeology Part E (7 failed approaches), the EmissionTier deletion at `2f7c1bd4`, the structural pre-scan deletion at AQ.5, the DTA interpreter deletion at AX.W0b, the column revert at AY-I.W1 — these are the project's most architecturally valuable commits. Squashing erases the reversal as a separate event.

**Era V's DTA arc.** The 572-commit DTA arc IS the project's "failed-substrate-without-consumer" lessons-learned corpus. Per archaeology: "Each tranche claimed the next would consume; none did. AX invariant 13 ('ledger-only wave = re-plan trigger') retroactively names the pattern." Squashing this to 1 commit per Era erases the granularity that makes the lesson teachable.

### §B.5 — Decision matrix

| Option | Cost | Loss | Honour-of-locks | Recommendation |
|---|---|---|---|---|
| 1 — Rewrite to era boundaries | force-push ~25-30 commit chain replacing 2,621 | per-tranche archaeology lost; commit-SHA references in memory items break | partial Lock 8 (AU references squashed away); Lock 14 implicit | NOT recommended |
| 2 — Squash all to one greenfield commit | force-push 1 commit replacing 2,621 | ALL provenance erased; user's 945-commit unpushed engineering invisible | Lock 8 honoured by total erasure; clean slate | NOT recommended |
| 3 — Keep verbatim + greenfield prelude commits at HEAD | tag + ~8 prelude commits | none | every memory-cited SHA preserved; archaeology accessible; `accurate-perf-narrative` honoured | **RECOMMENDED** |
| 4 — Hybrid (squash legacy + keep recent) | force-push for legacy half; preserve VI | half-provenance | partial honour; arbitrary boundary | NOT recommended (boundary is judgement-call) |

### §B.6 — Recommendation: Option 3 (keep verbatim + branch reset)

#### §B.6.1 — Justification

1. **Provenance preservation is non-negotiable.** Memory items `accurate-perf-narrative`, `perf-breakthrough-accuracy`, the entire archaeology document — these cite specific commit SHAs. Squashing them away breaks the project's own attribution.

2. **The commits ARE the lessons.** Era V's failure mode is *commitable* — i.e., each commit carries the per-substrate-build reasoning. Future readers can trace via `git log -- crates/tape/src/dta.rs` to see when the DTA was added, when it was deactivated, when it was deleted. Squashing erases the trace.

3. **Operational cost is manageable.** 2,621 commits clones in seconds; renders in GitHub; `git log --oneline | head/tail` operates instantly. The "noise" framing is a misperception — every commit is data.

4. **Branch reset is cheap.** Tag the current state as `pre-restart-2026-05-03`; open a new branch; land the 8-commit greenfield prelude; cutover when ratified. Provenance is in the tag.

5. **Future commit discipline absorbs the failure modes.** Per memory items: HARD CAPs, single-cargo-per-target, bg-then-monitor, dispatch-hard-cap, triumvirate-discipline, no-metalanguage-docs (in commit subjects), templated-bodies-rejected. The post-restart commits will be cleaner; the legacy noise stays as legacy noise.

#### §B.6.2 — Operational sequence

(per Agent 5 §8.2 — replicated here for the synthesis):

```
1. git tag pre-restart-2026-05-03 master
2. git push origin master                      ← closes 1,724-unpushed gap
3. git push origin pre-restart-2026-05-03      ← anchors provenance on remote
4. git checkout -b master-greenfield-2026-05-03 master
5. Land 8-commit greenfield prelude:
   - Lock 12 archive ceremony
   - crates/{analysis, lsp} consolidation
   - docs/ tree restructure
   - docs/tranches/ archive
   - README + GESTALT rewrite
   - .gitignore additions; delete committed artefacts
   - SPEC.md + architecture.md + migration record + tranche A-J stubs
   - Master Plan + per-pass restart audit synthesis
6. Push master-greenfield-2026-05-03.
7. Hardening pass per docs/restart/HARDENING.md.
8. Cutover decision: 
   - master-greenfield-2026-05-03 → master (force-push); pre-restart tag preserved.
   - OR keep both branches.
```

### §B.7 — Post-restart commit discipline

Going forward:

1. **Subject prefixes:** `tranche(<letter>):`, `feat(<crate>):`, `refactor(<area>):`, `docs(<dir>):`, `chore(workspace):`, `audit(<area>):`. Single-letter tranche references resume after the restart letter set (A-J) lands.
2. **No metalanguage in subjects.** No "AY-II.W0'.a" formats. Tranche letters yes; sub-wave depth no.
3. **Bodies cite runtime evidence** per `feedback_clean-instrumentation`. Templated bodies rejected per `Templated Commit Bodies Are Bodyless In Spirit` (LESSONS-LEARNED 2026-04-30).
4. **HARD CAPs honoured** per `dispatch-hard-cap`.
5. **Worktree integration via cherry-pick** per `Cherry-Pick Preserves Wave Provenance` (LESSONS-LEARNED 2026-04-30).
6. **Empty returns are failed dispatches** per LESSONS-LEARNED.

---

## §C — Cross-cut summary

| Cross-cut | Disposition |
|---|---|
| Docs ↔ tranches | honoured-by-design |
| Audit ↔ tranches | honoured-by-design |
| Archive ↔ active | violated for Cargo.toml (Lock 12 unexecuted); honoured otherwise |
| Sibling ↔ workspace | honoured-mostly; gorgeous coupling resolves with Lock 12 archive |

---

## §D — Commit-chain final disposition

**RECOMMENDED: Option 3 (keep verbatim + branch reset).**

- Tag: `pre-restart-2026-05-03`
- New branch: `master-greenfield-2026-05-03`
- Greenfield prelude: ~8 focused commits per Agent 5 §8.2
- Cutover: user ratifies; force-push or keep both branches

Provenance is preserved in the tag. The 2,621-commit chain remains accessible. The new chain is clean. Future commit discipline absorbs the failure modes.

The decision flows from Pass C; ratifies in the synthesizer (`docs/restart/SYNTHESIZER.md`); executes outside this suite per the user's hardening + ratification gate.
