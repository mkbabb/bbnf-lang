# SK-V9 S-P3 Hardening — CH4 COST — V4

Lens: CH4 COST (cost-discipline / wave-manifest / LOC-cap integrity).
Pass: S-P3 Synthesis-Plan. Cycle: V4.
Date: 2026-05-18.
V4 surface: the `docs(sk-v9-p3-v4)` commit `cef745d2` — a two-hunk,
single-file diff to `skv9-p3-A-candidate-shortlist.md` (8 insertions,
4 deletions), moving `gsoc-2018` from the W3 must-improve exit-gate
list to a no-regression-only clause.
Convergence rule: ≥95% × 2 consecutive cycles (`ORCHESTRATOR.md` §3Z).

V3 CH4 cleared 100%. The CH4 question for V4: does relaxing
gsoc-2018's W3 gate threshold uncost any wave, perturb any wave
manifest, or touch any LOC / hard-cap surface?

---

## §1 — V4-diff cost audit

The cost-discipline lens watches three surfaces: (a) the wave manifest
(W0-W5 / W4a-d shape, hard caps, redress sub-caps), (b) per-wave LOC
envelopes and the per-tranche line-count budget, and (c) the
falsifiability gates *as cost guards* — a gate that demands a row
*rise* prices a wave's effort; a gate that merely demands a row *not
fall* prices nothing extra.

The V4 diff is two hunks, both in P3-A, both inside the C3
falsifiability-gate prose. Neither hunk touches:

- the W3 hard cap (`≤90 min wall`, 75-min redress sub-cap, ≤110-min
  CHALLENGE-gated extension — P3-A:300-307 region / F-spec §6
  `:643-646` / P3-C §2 W3 `:138`);
- the C3 LOC envelope (`~265 hand + ~120 regen` + the P2-D §5 chain
  `~120-220 SIMD + ~30-60 VEXT + ~60-120 cursor + ~50-90 checkasm` —
  P3-A:295-299);
- the wave manifest (W0-W5 / W4a / W4b-1/2/3 / W4c / W4d — F-spec §2,
  P3-C §1.4, all siblings);
- the per-tranche generated-size budget.

The audit confirms the cost question reduces to one point: does
moving gsoc-2018 from must-improve to no-regression *uncost* W3 (i.e.
silently shrink the work W3 must do, masking effort that was real)?
It does not — the gsoc-2018 closure work was never costed into W3 in
the first place.

---

## §2 — V4 dispositions

Verified against: `git show cef745d2`; P3-A §2.2 C3 (gate prose +
LOC envelope + risk + caps) + §4 table; F-spec §2 W3 (`:130-138`),
§6 (`:628-737`); P3-C §1.4, §2 W3 (`:130-138` region).

| # | Claim under review | Artefact | Verdict | Evidence |
|--:|---|---|---|---|
| 1 | Moving gsoc-2018 to no-regression-only does NOT uncost W3 | P3-A §2.2 C3, F-spec §6 | ACCEPT | W3's costed work is the union event-model + the P2-D §5 structural-bitmap chain (P3-A:295-299, F-spec §6 `:636-643`). gsoc-2018's *closure* was never a W3 deliverable — F-spec §6 `:723-724`: gsoc-2018 is "a P1-named uncloseable row … full closure routes to W4." The V4 edit removes a gate threshold that costed *zero W3 effort* (no W3 slice targets gsoc-2018; the union substrate is grammar-neutral and lifts whatever rows it lifts). Removing an aspirational ceiling that was never a deliverable uncosts nothing — the W3 effort estimate is unchanged because gsoc-2018 closure was never inside it. |
| 2 | The W3 hard cap (≤90 min / 75-min redress sub-cap / ≤110-min extension) is unchanged | P3-A §2.2 C3, F-spec §6, P3-C §2 W3 | ACCEPT | The V4 commit touches only P3-A's §2.2 gate-prose enumeration and §4 table. The W3 cap clauses — P3-A:300-307 "Risk class" region, F-spec §6 `:643-646`, P3-C §2 W3 row `:138` "CHALLENGE-gated redress extension to ≤110 min" — sit outside both diff hunks. The cap surface is byte-identical to V3. |
| 3 | The C3 LOC envelope is unchanged | P3-A §2.2 C3 | ACCEPT | The C3 "Preliminary LOC envelope — ~265 hand + ~120 regen net … Plus the P2-D §5 structural-chain body (~120-220 bbnf-simd + ~30-60 VEXT + ~60-120 cursor + ~50-90 checkasm)" clause (P3-A:295-299) is below hunk 1's window — untouched. The W3 LOC estimate is byte-identical to V3. A gate-threshold edit carries no LOC implication: the union substrate's code is the same whether gsoc-2018 is gated or not. |
| 4 | No wave manifest surface is touched | F-spec §2, P3-C §1.4, all siblings | ACCEPT | The V4 commit (`git show --stat`) is `1 file changed` — `skv9-p3-A-candidate-shortlist.md` only. F-spec §2's W0-W5 / W4a / W4b-1/2/3 / W4c / W4d manifest, P3-C §1.4's candidate→wave table, and every sibling's manifest are untouched. The V4 edit does not add, remove, re-letter, or re-cap a single wave. |
| 5 | The per-tranche generated-size / LOC budget is not touched | all | ACCEPT | The V4 diff is documentation prose (a gate enumeration + a table cell); it generates no code, regenerates no `generated.rs`, and changes no per-wave LOC budget. The per-tranche line-count budget surface is entirely outside the diff. |
| 6 | The four surviving W3 must-improve rows still price W3's exit gate correctly | P3-A §2.2 C3, F-spec §2 W3 | ACCEPT | After V4, W3's must-improve exit gate is `twitter ≥ 17685`, `apache_builds ≥ 14124`, `distinct_values ≥ 15731`, `update_center ≥ 14370` — four structural-dense rows the union substrate genuinely lifts (their gaps are *within* the structural budget, unlike gsoc-2018's). These four are the honest cost-priced exit gate; they match F-spec §2 W3 (`:134`) and §6 (`:684-689`). The V4 edit leaves the *real* W3 cost gate intact and removes only the phantom one. |
| 7 | The W10b six-row maintain block — the W3 cost guard against WIN-block regression — is unchanged | P3-A §2.2 C3 | ACCEPT | The W10b six-row block (`canada`/`citm_catalog`/`instruments`/`marine_ik`/`mesh`/`numbers`, P3-A:287-291) is W3's no-regression cost guard — it prices the constraint that W3 must not buy structural-row speed by regressing the WIN block. It sits below hunk 1's window — byte-identical to V3. The cost guard is intact. |
| 8 | gsoc-2018's no-regression status uncovers no hidden W4 cost either | F-spec §2a, P3-C §"W4b-2", P3-A C4/C5 | ACCEPT | The V4 edit explicitly routes gsoc-2018's residual to W4 ("full closure routes to W4"). W4's cost for gsoc-2018 was *already* priced as no-regression-only — F-spec §2a W4b-2 (`:169`) `gsoc-2018 ≥ 21963` (no-regression basis), P3-C §"W4b-2" `:353-360` ("gsoc-2018 is out of scope for the codec"). The V4 edit does not push a new must-improve obligation onto W4; W4's gsoc-2018 cost is unchanged. No wave is uncosted at W3 and silently recosted at W4 — gsoc-2018 is no-regression at *both*. |
| 9 | The C3 risk class is unchanged — no cost-risk re-pricing | P3-A §2.2 C3 | ACCEPT | The C3 "Risk class — MEDIUM … HIGH if the P2-D §5 chain folds in whole" clause (P3-A:300-307) is untouched. Relaxing gsoc-2018's gate does not lower W3's risk class (risk is driven by the substrate-replacement mechanism, not by the row count) — and the V4 edit correctly leaves the risk clause alone. No cost-risk surface moved. |
| 10 | The C3 same-wave-consumer and dependency cost constraints are unchanged | P3-A §2.2 C3 | ACCEPT | The C3 "Same-wave consumer" (P3-A:270-275) and "Dependency — Depends on C2 … C3 is the same-wave dependency for C4/C5/C6/C7" (P3-A:308-311) clauses are outside both diff hunks. The cascade-cost structure (C2→C3→C4-C7 same-wave) — which prices the W3→W4 sequencing — is byte-identical to V3. |
| 11 | The V4 diff adds no cost-bearing claim | P3-A §2.2 C3, §4 | ACCEPT | The 8 inserted lines are: a 5-line no-regression clause for gsoc-2018 in §2.2, plus the rewritten §4 C3 table cell. Neither introduces a new wave, slice, LOC figure, hard cap, or budget. The net documentation delta is +4 lines (8 ins − 4 del), entirely prose — no cost surface created. |

---

## §3 — Aggregate verdict

**11 dispositions: 11 ACCEPT, 0 REVISE, 0 REJECT.**

ACCEPT rate = 11 / 11 = **100%.**

This **clears** the §3Z 95% threshold. The V4 gsoc-2018 W3-gate move
carries no cost-discipline regression:

- It does **not** uncost W3. gsoc-2018's *closure* was never a W3
  deliverable — F-spec §6 names it a P1-uncloseable row whose full
  closure routes to W4. The union substrate is grammar-neutral; it
  lifts whatever rows fall within the structural budget regardless of
  which rows the gate names. Removing an aspirational threshold that
  priced zero W3 effort uncosts nothing — the W3 LOC envelope
  (`~265 hand + ~120 regen` + the P2-D §5 chain) and hard caps
  (`≤90 min`, 75-min redress sub-cap, ≤110-min extension) are
  byte-identical to V3.
- It touches **no wave manifest** — `git show --stat` confirms one
  file changed; the W0-W5 / W4a-d shape, every per-wave cap, and
  every sibling manifest are untouched.
- It touches **no LOC / cap / per-tranche-budget surface** — the diff
  is documentation prose (a gate enumeration + one table cell),
  generates no code, and changes no budget.
- The *real* W3 cost gate — the four structural-dense must-improve
  rows (twitter/apache_builds/distinct_values/update_center, all with
  gaps inside the structural budget) and the W10b six-row
  no-regression cost guard — is fully intact.
- gsoc-2018's residual is correctly routed to W4, where it was
  *already* priced as no-regression-only (F-spec §2a, P3-C §W4b-2).
  No wave is uncosted at W3 and silently recosted at W4.

**CH4 V4 = 100%.** Paired with V3 CH4 at 100%, CH4 has two
consecutive ≥95% cycles — CH4 is converged on the V4 surface.
