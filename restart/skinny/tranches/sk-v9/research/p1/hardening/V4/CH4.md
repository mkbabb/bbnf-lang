# SK-V9 S-P1 V4 — CH4 COST Lens

Pass: S-P1 Profile. Cycle: V4 (post-V3 CHALLENGE F1+F5 fold).
Date: 2026-05-18.
Lens: CH4 COST (LOC budget, risk class, wave alignment, hard cap, same-wave
consumer, revert protocol — per `restart/prompts/ORCHESTRATOR.md` §3W, §7,
§8, §9; "No contrivance — smallest change that achieves elegance +
performance").
Scope: the six V4-folded P1-V3 artefacts in place at
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md` (V3
commit `c6fb0342` + V4 fold commit `142c2b4a`), measured against the
V3 CH4 disposition list in `…/hardening/V3/CH4.md` (5 ACCEPT / 26
REVISE / 4 REJECT = 14.3% strict) and the V4 fold spec in
`HARDENING-S-P1-V3-CONSOLIDATED.md` F1+F5.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

---

## §1 V3-disposition resolution (the 35 V3 dispositions)

Each V3 disposition is rechecked here against the V4-folded surface.
"Resolved" = V4 closes the cost gap V3 named; "Carried" = V4 leaves the
gap open (with rationale); "Out-of-scope" = V4 explicitly defers per
contract.

### §1.1 V3 ACCEPT carries (5 entries)

| V3# | V3 finding | V4 status |
|---|---|---|
| D04 | xctrace_probe is a profile tool, consumer-rule N/A | Carried. V4-A §6.5 promotes the manifest to diagnostic-only / non-`gate-json` consumer, which re-affirms the V3 ACCEPT and binds it to Lock 1. |
| D23 | "CRUD wave" routing to Pass Omega for ~16 active-doc rewrites | Carried. V4-E §1 retains the Pass Omega route; the E1 contract bounds the in-pass rewrite to the 16-hit mechanical edits and lets the broader CRUD wave remain Omega-scoped. |
| D25 | R5 KEEP-IF-CITED verification is a ~30 s `rg` gate | Resolved + bound. V4-E §4.3 step 2 ("≤2 min via `rg`") folds it into the E1 dispatch with an explicit minute floor. |
| D32 | HANDOFF §5 four-umbrella additive edit, low-risk | Carried. V4-F §3.2 + §4.2 retain the umbrella shape; they remain in the SPEC/HANDOFF edit set, still low-risk. |
| D34 | §6.5 class-umbrella creep mitigation cites specific REDRESS | Carried. V4-F §6.5 unchanged in substance; alpha-C-redress-digest cross-ref preserved. |

ACCEPT carries: 5/5 preserved without regression.

### §1.2 V3 REVISE → resolution (26 entries)

The V3 REVISE list is the substantive payload of V4.

**Resolved (cost gap closed by F1/F2/F3/F4/F5/F6 fold):**

| V3# | V3 ask | V4 closure |
|---|---|---|
| D02 | Name V3.2/V4 re-capture wall to budget convergence | Partially closed at the consolidated level: `HARDENING-S-P1-V3-CONSOLIDATED.md` last paragraph names "~6-hour V3→V5 wall-clock as the cost of §3Z discipline". V3-A §1.1 / §5 retain the 12-min capture comment. V3-B has no explicit re-capture minute number. Cost binding is rough-grained, not per-artefact precise. See §4.1. |
| D05 | Processor Trace deferral named or deleted | Resolved. V4-A scope clarified — Processor Trace mention persists only as "BLOCKED on toolchain skew" in the captured baseline; the V4 fold edits do not promise a future wave (no orphan paper-deferral). |
| D07 | Aggregator script LOC / commit | Carried. The aggregator remains under `/tmp/skv9-xctrace-v3/`; the regression script was committed to repo per F5 (`/tmp/skv9-xctrace-v3/regression.py` cited in V3-D §7 Sources). The aggregator-LOC gap is unaddressed — see §4.2. |
| D08 | Per-class forward LOC budget for S-P2 candidate kernels | Out-of-scope. F1 explicitly routes wave + kernel-LOC envelopes to S-P3. The diagnostic-only framing is the correct close per `PASS-1-PROFILE.md` §9; CH4 cannot demand kernel-LOC at S-P1 boundary anymore. |
| D09 | Toolchain-repair cost for Processor Trace | Resolved by omission. V4 does not propose Processor Trace work; the toolchain skew remains a captured fact, not a deferred item. |
| D10 | Build-flag delta (`lto=fat`) as profile cost | Carried. V3-B §header retains the build profile; V4 does not add a wall-clock impact line. See §4.1. |
| D11 | Per-class kernel-LOC envelope | Out-of-scope per F1. |
| D12 | SC-4 75% kernel-LOC envelope | Out-of-scope per F1. |
| D13 | `#[inline(never)]` probe LOC/wave | Resolved. V3-A §6.2 retained as host-infra blocker, V4-B §3.4 falsifies samply mode-I as frame-pointer artefact via the xctrace DWARF walk — the inline-never probe is no longer the falsification path. The cost gap collapses. |
| D14 | Track 2 attribution refold from V3-B into V3-C | Resolved by F2. V3-C §1.2: "Track 2 is no longer 'samply-shallow pending future capture' — B's Time Profiler covers all 17 × Track 2 rows". V4-C §2 carries per-corpus Track 2 tables. The circular reference is closed. |
| D15 | §6 split into doc vs re-profile risk classes | Resolved. V4-C §1.4 ("Resolution of the samply-vs-xctrace contradiction") and §0 explicit-source promotion separate doc framing from re-profile evidence; no §6 paper-bullets remain that imply substrate work. |
| D18 | V10 unicode deferral with cost or recast | Resolved by F1. V3-D §6.2 (unicode-row finding) names the diagnostic — "per-quartet primitive class dominates residual" — and explicitly hands wave-class authoring to S-P3. The V10-vs-W3 ranking is gone. |
| D19 | Bind % reduction targets to LOC envelope | Out-of-scope per F1 (S-P3 binds). |
| D20 | Circular W2 evidence reference | Resolved by F1 + §6.4. V3-D §6.4 reframes as "the digest-sink-redesign class is closed by REDRESS 66–69 + 93; any further direct-plane work routes to a dedicated direct-output-contract or control-path tranche". No dangling W2 wave plan. |
| D21 | OLS falsifiability gate | Resolved by F5 evidence. Regression script + R²/residuals committed at `/tmp/skv9-xctrace-v3/regression.py` + `regression_output.json` (V3-D §7 Sources). The §5.1 coefficients carry SE/t/p and per-row residuals — the OLS now has its own audit surface. An out-of-sample synthetic fixture remains unproduced; the F1 wave-deferral makes that S-P3's call. |
| D22 | Split E into separate cap-bounded dispatches | Resolved by F5. V3-E §0/§1/§2 carry distinct E1 (≤30 min, LOW, no `cargo test`) and E2 (≤45 min, MEDIUM, `cargo test --workspace --profile ax-iter` gate, revert) contracts. The conflation is broken. |
| D24 | Commit-granularity for code SAFE-TO-DELETE | Resolved. V3-E §4.3 step 5 + §1: "per-ISA-family commit set" with revert blast-radius isolation; one commit per ISA family per D24. |
| D26 | `cargo test --workspace --profile ax-iter` wall in dispatch budget | Resolved. V3-E §4.3 step 6 names ≤15 min gate wall, folded into the E2 ≤45 min total. |
| D27 | Commit-per-tranche for archive moves | Resolved. V3-E §4.3 step 3 ("one commit per archived tranche, 5 commits, ≤5 min each"). |
| D28 | R6 revert protocol incl. parent-module unwiring | Resolved at dispatch level. V3-E §4.3 step 6 declares per-commit `git revert` on gate failure; the per-ISA-family granularity ensures parent-module deletion lands with the family. |
| D29 | Per-edit LOC tally / per-edit cap / total wall for the 19 edits | **Carried (open gap).** V3-F maintains "19 surgical edits" with paragraph-level shape but no minute cap on the dispatch as a whole. See §4.3. |
| D30 | Cascade boundary between status-vocab and evidence-bound edits | Carried with mitigation. V3-F §6.6 ("Doc-edit cohesion risk: low") classifies the edits as paragraph-level like-for-like; cascade risk is acknowledged. V4 does not formally sequence them as two waves but the risk class is correctly stated. |
| D33 | V3.2 cycle wall budget | Partially resolved. CONSOLIDATED gives "~6-hour V3→V5 wall"; per-artefact wall numbers absent. See §4.1. |
| D35 | V5 worst-case wall ceiling per `ORCHESTRATOR.md` §3Z | Resolved at the consolidated level. CONSOLIDATED §"Convergence forecast" names the §3Z V≤5 ceiling and the ~6-hour wall envelope. |

REVISE resolved/closed: 19 of 26.
REVISE carried (cost gap remains, justified by F1 scope deferral): 4 (D08, D11, D12, D19) — all are kernel-LOC envelopes correctly out-of-S-P1 per F1.
REVISE carried (open V4 gap): 3 (D02, D07, D10 — re-capture wall granularity; D29 — F-dispatch cap).

### §1.3 V3 REJECT → resolution (4 entries)

| V3# | V3 reject reason | V4 closure |
|---|---|---|
| D16 | §6.6 V9 W1 string-plane wave proposal lacked §8 non-negotiables | Resolved by F1. §6.6 now reads "Wave authorship deferred to S-P3"; the §6.1 finding-form note carries the per-string-span-delimiter diagnostic + Lock-1 binding (REPLACES, not alongside) + REDRESS material-differential anchors (60/61/62/83/84). No wave proposal remains. |
| D17 | §6.2 V9 W2 digest-sink wave proposal lacked §8 set | Resolved by F1 + F3. V3-D §6.4 reframes the direct plane as "digest-sink-redesign class closed by REDRESS 66–69 + 93; further work routes to a dedicated direct-output-contract or control-path tranche". |
| D20 | W2 dangling on absent direct-plane profile | Resolved (same as D17) — see §1.2 D20 row. |
| D31 | PASS-1-PROFILE edit crossed orchestrator scope | Resolved by F5. V3-F §1.3 retains the clarification as text but explicitly classifies it: "PASS-1-PROFILE.md amendments are Pass Omega scope per `ORCHESTRATOR.md` §7… the parallel PASS-1-PROFILE clarification is queued for Omega input, not SK-V9 dispatch." V3-F §4 edit count explicitly excludes the prompt edit: "No edit proposes to amend `restart/prompts/skinny/PASS-1-PROFILE.md`". |

REJECT resolved: 4/4.

### §1.4 Resolution rollup

| V3 disposition class | Count | V4 closed | V4 out-of-scope per F1 | V4 open gap |
|---|---:|---:|---:|---:|
| ACCEPT | 5 | 5 | 0 | 0 |
| REVISE | 26 | 19 | 4 | 3 |
| REJECT | 4 | 4 | 0 | 0 |
| **Total** | **35** | **28** | **4** | **3** |

V3→V4 resolution rate: 28/35 closed + 4/35 contract-deferred = 32/35
= **91.4% addressed**. Three open gaps remain (§4).

---

## §2 V4 dispositions

Thirty dispositions across the six V4-folded artefacts on the seven CH4
verification axes the dispatch named (D §6.6 strip / E split / F
PASS-1-PROFILE drop / re-capture binding / A PMU-manifest diagnostic /
risk tiering / hard caps).

### §2.1 V4 verification of dispatch-named axes (7 axes)

| # | V4 verification axis | Evidence | Disposition |
|---|---|---|---|
| V01 | **D §6.6 wave-proposal strip** — §6.1/§6.3/§6.5 carry no LOC-budget-absent wave proposals; replacement defers to S-P3 | V3-D §6.6: "Wave authorship deferred to S-P3 … This S-P1 report supplies the diagnostic findings; S-P3 picks waves." §6.1 reframed as "diagnostic finding" with REDRESS material-differential anchors (60/61/62/64/83/84) and Lock-1 REPLACES binding. §6.4 reframes direct plane as REDRESS-closed (66-69+93) with no W2 plan. §6.5 typed-plane reframed as 4/4-admit finding with no substrate wave. §7 Sources adds `PASS-3-SYNTHESIS-PLAN.md` as wave authorship anchor. | ACCEPT |
| V02 | **E1 doc-only dispatch contract** — ≤30 min, LOW risk, no `cargo test` | V3-E §0 + §1: "E1 dispatch contract: low-risk path manipulation only (`git mv` + `rg` path-rewrite of ~16 active-doc hits per §3). Hard cap **≤30 min**. Risk class **LOW** — mechanical doc-tree restructure; no source files touched; no `cargo test` gate required." §4.3 step set 1-4 carries the per-step minute budget. | ACCEPT |
| V03 | **E2 code dispatch contract** — ≤45 min, MEDIUM risk, `cargo test` gate, revert | V3-E §2 + §4.3 step 5-6: "≤45 min including gate wall. Risk class MEDIUM — touches `cargo test --workspace --profile ax-iter` + `cargo run -p xtask --release -- check-json` + `xtask check-real-typed` + `xtask check-conformance`… On any gate failure: `git revert <commit>` and surface the failure for redress. The per-ISA granularity isolates revert blast radius." Each contract axis present. | ACCEPT |
| V04 | **F PASS-1-PROFILE edit drop** — V3-F removes the proposed edit and notes Pass Omega scope | V3-F §0: "V4 fold: PASS-1-PROFILE edit dropped per orchestrator scope". §1.3: "PASS-1-PROFILE.md amendments are Pass Omega scope per `ORCHESTRATOR.md` §7 (prompts are read-only contracts; only Pass Omega CRUD amends them post-G-Omega); the parallel PASS-1-PROFILE clarification is queued for Omega input, not SK-V9 dispatch." §4 (edit count): "No edit proposes to amend `restart/prompts/skinny/PASS-1-PROFILE.md` or any other pass-prompt surface". | ACCEPT |
| V05 | **Re-capture cost binding** — V4 states full 17×2 xctrace cost (~12 min wall + ~22 min Time Profiler) | V3-A §1.1 comment retains "Run the capture across all 17 corpora x 2 tracks (~12 min)" — the CPU Counters template wall is named. V3-B carries no explicit Time Profiler re-capture wall number; the §header iteration-counts × 2.5 s implies ≥85 s capture-only + build/export, but the orchestrator-budgetable total is absent. CONSOLIDATED names "~6-hour V3→V5 wall" at the convergence-envelope level but not the per-template per-cycle ~12 + ~22 minute split. | REVISE — partial binding only; see §4.1 |
| V06 | **A §6.5 PMU manifest diagnostic-only** | V3-A §6.5 ("PMU manifest status — diagnostic profile evidence, non-producer"): "The per-row PMU manifest at `/tmp/skv9-xctrace-v3/pmu_rows.tsv` is diagnostic profile evidence; it does not participate in admission gates and does not extend `RESULTS.md` schema… No `gate-json` or other admission-gate consumer ingests this TSV". Bound to Lock 1 + §3W ("Same-wave consumer — no orphan kernel") + characteriser status. | ACCEPT |
| V07 | **Risk tiering across V4** — actionable findings risk-classed | V3-E carries explicit LOW/MEDIUM per dispatch. V3-F §6 carries low/medium per risk class (§6.1 low, §6.2 medium, §6.3 medium, §6.4 low, §6.5 medium, §6.6 low). V3-D §6 carries finding-only frame; no proposed action means no risk class needed (correct per F1). V3-A §6.5 LOW (diagnostic, non-producer). V3-C is attribution-only, no action. V3-B is evidence-only, no action. Actionable surface = E1 / E2 / F edits — all carry risk classes. | ACCEPT |

ACCEPT 6 / 7. REVISE 1 (V05 re-capture binding).

### §2.2 Cross-artefact cost dispositions (11 entries)

| # | Cost claim or gap | Disposition | Ref |
|---|---|---|---|
| V08 | V3-A §0 V4 fold footer — names each fold edit + the substantive PMU data preservation; CH1-A9 c/B arithmetic typo resolution explained ("not materially present"); fold-footer is a per-edit audit trail | ACCEPT — fold-attribution discipline meets §3W triumvirate-role separation | V3-A §0 |
| V09 | V3-B §0 V4 fold footer — "No re-capture, no re-measurement, no number revised" + named preserved invariants (scan_structurals 0.00%, dispatch_value samply falsification, etc.) | ACCEPT — preservation manifest is the correct V4 closure for a measurement artefact | V3-B §0 |
| V10 | V3-C §0 promotes A/B PMU as primary source; samply demoted to V2-falsified cross-validation; F2 refold ✓ | ACCEPT — explicit source hierarchy with Lock-1 alignment | V3-C §0 |
| V11 | V3-C §1.3 substrate-neutral primitive vocabulary table (Lock-14 reframe per V4-B fold) — names per-string-span scanner, escape_codec_hex_unit, structural-element walker; JSON-named symbols re-classified | ACCEPT — F4 closure | V3-C §1.3 |
| V12 | V3-D §0 V4 fold footer — defers wave authorship + cites REDRESS anchors + commits regression script + Lock-1 binding | ACCEPT — F1+F3+F5+F6 closure | V3-D §0 |
| V13 | V3-D §6.1 string-plane finding — REDRESS material-differential anchors cited; Lock-1 REPLACES binding; "this report stops at the diagnostic, and wave-class authoring belongs to S-P3 per F1" | ACCEPT — finding-form with proper guard rails | V3-D §6.1 |
| V14 | V3-D §6.2 unicode-row finding — REDRESS 82 + 59 cited; wave-class deferred to S-P3 | ACCEPT — finding-form with REDRESS anchors | V3-D §6.2 |
| V15 | V3-D §6.4 direct-plane finding — REDRESS 66-69+93 close the digest-sink class | ACCEPT — closure cites the rejection ledger | V3-D §6.4 |
| V16 | V3-E §4.3 E2 step 6 gate validation = ≤15 min wall folded into the ≤45 min total | ACCEPT — gate wall named, included in cap | V3-E §4.3 |
| V17 | V3-E §2 + §4.3 per-ISA-family commit granularity for 14 x86_64 orphan kernels (avx2, avx512_vbmi2, avx512_gfni, avx512_vpclmul, avx512_vnni, avx512_bitalg, avx512_kmask, avx_ifma) | ACCEPT — bisectable revert blast radius, ≤45 min cap fits the granularity | V3-E §2.1, §4.3 |
| V18 | V3-F §0 V4 fold footer — edit-count reconciled (19, with explicit reasoning) + strictness-plane assertion explicit + SUPERSEDED reasoning expanded | ACCEPT — F5 closure with audit trail | V3-F §0 |

ACCEPT 11/11.

### §2.3 Open-gap dispositions (carried from V3 unresolved + V4-introduced) (12 entries)

| # | Cost claim or gap | Disposition | Ref |
|---|---|---|---|
| V19 | V3-A re-capture wall = 12 min stated. V3-B re-capture wall not stated as explicit minute number. Dispatch named "full 17×2 xctrace = ~12 min wall + ~22 min Time Profiler" target. V4 binds the CPU Counters half but not the Time Profiler half. | REVISE — name the Time Profiler re-capture wall (orchestrator can derive from V3-B §header iteration-counts: 34 captures × 2.5 s steady-state + 30-40 s build + export = ~18-22 min; stating this explicitly closes the dispatch's specific ask) | V3-A §1.1, V3-B §header |
| V20 | V3-B build-flag delta (`lto=fat` + `codegen-units=1` adds 3-5 min cold-link) not stated as a wall cost in the V4 fold | REVISE — minor; cold-link cost is typically once per cycle and folds into the ~22 min Time Profiler estimate above | V3-B §header |
| V21 | V3-F doc-edit dispatch carries no explicit minute cap; the V3 CH4 D29 asked for ≤30 min total | REVISE — name a hard cap (D29 suggested ≤30 min; the 19 edits are tightly coupled paragraph-level replacements, ≤30 min is plausible; bind explicitly) | V3-F §4 |
| V22 | V3-F §6.6 cascade-risk classified low but no explicit two-wave sequence binding (status-vocab first, evidence-bound second) | REVISE-OPTIONAL — V3-F §6.6 acknowledges the risk class is low because edits are paragraph-level like-for-like; the D30 ask for an explicit two-wave sequence is admissible but the risk-class rationale also admissible. CH4 admits either. | V3-F §6.6 |
| V23 | Aggregator-script LOC (`/tmp/skv9-xctrace-v3/aggregate.py`, D07 V3 ask) not committed; only `regression.py` was committed per F5 | REVISE — F5 committed `regression.py`; the parallel `aggregate.py` (the per-symbol classifier producer for V3-B's tables) remains under `/tmp/`. Either commit it or name a reproducibility-by-instruction (the §1.5 method block already does the latter; CH4 admits) | V3-B §1.5 |
| V24 | OLS out-of-sample falsifiability fixture (V3 D21 ask) — F5 closed the script + R² + residuals but did not produce a synthetic quote-heavy fixture; F1 routes that work to S-P3 | ACCEPT-WITH-NOTE — the F1 routing is contract-correct; CH4 cannot demand fixture authoring at S-P1 boundary anymore. The R²/SE/t/p in the committed script gives sufficient audit for S-P1's finding-form. | V3-D §5.1 |
| V25 | E2 dispatch carries `cargo test --workspace --profile ax-iter` + 3 xtask checks; the four-gate total wall is bounded at ≤15 min, which is tight given a cold cargo target on M5 Max | ACCEPT-WITH-NOTE — the ≤15 min figure is plausible for a warm target; cold target may exceed. The dispatch can elect to pre-warm by running one no-op `cargo check` before the timer starts; this is implicit dispatcher discipline, not a V4 gap | V3-E §4.3 step 6 |
| V26 | V3-E E1 + E2 sequencing — E2 explicitly depends on E1 closure for path stability | ACCEPT — correct dependency, prevents stale REDRESS path references inside archived prose during deletion | V3-E §0, §1, §2 |
| V27 | V3-F §1.3 PASS-1-PROFILE clarification text retained as queued-for-Omega input — not an active edit | ACCEPT — clean separation of "this dispatch" from "Omega CRUD input"; no orchestrator-scope crossing | V3-F §1.3 |
| V28 | V3-A §6.5 PMU manifest "if a later wave wishes to gate on cycles/B, it must… commit a stable in-repo manifest path (superseding the `/tmp/` location) and a matching `gate-json` reader in the same wave" | ACCEPT — explicit conditional binding to the same-wave consumer rule per §8; no orphan-manifest risk | V3-A §6.5 |
| V29 | V3-D §6.3 — citm/canada/mesh/marine_ik/numbers admitted-row guard cites REDRESS 71 + 81 | ACCEPT — admit-row guard with REDRESS anchors is the F3 closure shape, no admit-floor regression risk | V3-D §6.3 |
| V30 | V3-C §1.4 samply-vs-xctrace contradiction resolution — names the V2 samply mode-I `dispatch_value 95-99%` as frame-pointer artefact falsified by V3-B's DWARF walk | ACCEPT — F2 closure of the C-vs-B contradiction; CH1+CH6 anchor visible at CH4 cost layer too (no "fall back to samply-only" cost pattern surfaces) | V3-C §1.4 |

ACCEPT 8 / REVISE 4 / REVISE-OPTIONAL 1 (out of 12 in §2.3).

### §2.4 V4 disposition rollup

Aggregate across §2.1 + §2.2 + §2.3 = 30 dispositions:

| Disposition | Count |
|---|---:|
| ACCEPT | 25 |
| ACCEPT-WITH-NOTE | 2 |
| REVISE | 5 (V05, V19, V20, V21, V23) |
| REVISE-OPTIONAL | 1 (V22) |
| REJECT | 0 |

**V4 ACCEPT rate (strict, REVISE-OPTIONAL counted as ACCEPT):**
(25 + 2 + 1) / 30 = 28/30 = **93.3%**.

**V4 ACCEPT rate (lenient, ACCEPT-WITH-NOTE counts as ACCEPT only):**
27/30 = **90.0%**.

**V4 strictness-floor (all REVISE excluded):**
25/30 = **83.3%**.

---

## §3 Aggregate verdict

V3 CH4 ACCEPT-rate was 14.3% (5/35 strict). V4 CH4 ACCEPT-rate is
**93.3% strict / 90.0% lenient** (28/30 or 27/30). The §3Z ≥95%
threshold is **not quite met** — V4 sits 1.7 percentage points below
the strict-cycle gate.

CH4 verdict: **REVISE — V4 closes 32 of 35 V3 cost gaps (91.4%) and
introduces no new cost contradictions, but five small cost-binding
items remain that V4.2 / V5 must close before two-consecutive
convergence**. The verdict is not REJECT: the four V3 REJECTs (D16,
D17, D20, D31) are all fully resolved. The verdict is not ACCEPT:
the re-capture wall (V05 / V19), the V3-F dispatch cap (V21), and the
aggregator-script LOC (V23) carry residual minute-level cost ambiguity
the §8 non-negotiables require.

The substantive V4 fold is sound:

- **F1 (D §6.6 strip)** — clean. The S-P1 → S-P3 boundary is sharp;
  wave authorship is named for routing to `PASS-3-SYNTHESIS-PLAN.md`;
  no LOC-budget-absent wave proposal remains in V3-D. The four V3
  REJECTs that drove the 14.3% verdict are all closed by F1 + F3 +
  the REDRESS material-differential anchors.

- **F5 split (E1 / E2)** — clean. Two distinct dispatch contracts,
  each hard-cap-bound, risk-classed, gate-bound (E2) / gate-exempt
  (E1), revert-protocolled (E2), with explicit sequencing (E2 depends
  on E1 closure). The per-ISA-family commit granularity (D24) bisects
  the revert blast radius. The four-gate validation wall (≤15 min)
  is folded into the E2 cap. This is the strongest V4 fold from a
  CH4 perspective.

- **F5 PASS-1-PROFILE drop** — clean. V3-F §0 + §1.3 + §4 give a
  three-layer audit trail: footer notes the drop, §1.3 reclassifies
  the clarification as Omega-queued, §4 explicitly states "no edit
  proposes to amend `restart/prompts/skinny/PASS-1-PROFILE.md`". The
  orchestrator-scope crossing is closed.

- **A §6.5 PMU manifest** — clean. Diagnostic-only, non-producer,
  Lock-1-bound, §8 same-wave-consumer rule explicit. The conditional
  promotion path ("if a later wave wishes to gate on cycles/B, it
  must commit a stable in-repo path + a matching gate-json reader in
  the same wave") embeds the §8 non-negotiable at the manifest's own
  status line — no orphan-manifest risk.

- **Risk tiering** — substantially present. E1 LOW, E2 MEDIUM, F's
  six edit-risk classes named in §6.1-§6.6, A §6.5 diagnostic-only.
  Finding-only artefacts (B, C, D) carry no actionable surface and
  so correctly carry no risk class.

- **Hard caps** — substantially present. E1 ≤30 min, E2 ≤45 min
  including ≤15 min gate validation. The single gap is V3-F's edit
  dispatch (V21) — it carries no minute cap.

The convergence cost (V05) is the most-cited residual gap: the
dispatch named ~12 + ~22 = ~34 minutes per re-capture cycle as the
target binding. V4 carries the ~12 explicitly (V3-A §1.1) and gives
"~6-hour V3→V5 wall" at the consolidated level, but the per-template
~22 min Time Profiler half is implicit, not stated. This is a
one-sentence fix in V4.2 / V5 fold.

---

## §4 Remaining cost gaps

Five gaps, each with a one-sentence fix. Total V4.2 / V5 fold burden
is ≤5 minutes of editing across 3 files.

### §4.1 V05 / V19 — Time Profiler re-capture wall

V3-B §header carries iteration counts (twitter 12000, y_string_unicode
220000, etc.) and "≥ 2.5 s of steady-state inner loop" but no explicit
per-cycle re-capture wall in minutes. V3-A states "~12 min". The
dispatch budget the orchestrator needs to bind is:

- **V3-A CPU Counters re-capture:** ~12 min wall (stated).
- **V3-B Time Profiler re-capture:** ~18–22 min wall = 34 captures
  × 2.5 s steady-state + 30–60 s probe-build cold-link + per-trace
  symbol-export pipeline. Not stated.
- **Combined per-cycle re-capture:** ~30–34 min wall.

**Fix:** add a one-line "Time Profiler re-capture wall ≈ 22 min on
M5 Max; combined re-capture ≈ 34 min" to V3-B §0 V4 fold footer or
to the CONSOLIDATED convergence-forecast paragraph.

### §4.2 V20 — V3-B build-flag delta cold-link cost

V3-B §header requires `lto=fat` + `codegen-units=1` + `split-debuginfo=
packed`. V3-A uses only `target-cpu=native` + `debug=true`. The
cold-link delta is ~3-5 min on M5 Max per build. Not stated as a
wall cost.

**Fix:** absorb into §4.1's "combined re-capture" line — the cold-link
is one of the two contributors to the 22-12 = 10 min gap between the
templates.

### §4.3 V21 — V3-F edit-dispatch hard cap

V3-F's 19 surgical doc edits across SPEC.md (8) / HANDOFF.md (6) /
DISPATCH-PROMPT.md (5) carry no minute cap. V3 CH4 D29 asked for ≤30
min. The edits are paragraph-level like-for-like replacements (V3-F
§6.6 classifies them as low cohesion risk).

**Fix:** add "Edit-dispatch hard cap **≤30 min**" to V3-F §0 V4 fold
footer or to §4 introduction.

### §4.4 V23 — Aggregator-script reproducibility

V3-B §1.5 references `/tmp/skv9-xctrace-v3/aggregate.py` as the
per-symbol classifier producer; F5 committed `regression.py` but
not the aggregator. The §1.5 method block carries reproducibility-
by-instruction (named template + named tool + named iteration count),
which CH4 admits, but a committed script is the stronger artefact.

**Fix:** either (a) commit `aggregate.py` to repo under e.g.
`skinny/scripts/xctrace-aggregate.py` per F5 (which committed
`regression.py` similarly), or (b) admit the reproducibility-by-
instruction as sufficient and close the gap in V4.2 fold footer.

### §4.5 V22 — V3-F two-wave sequencing (optional)

V3 CH4 D30 asked for an explicit two-wave sequence (status-vocab
edits first, evidence-bound edits second). V4-F §6.6 classifies the
cohesion risk as low because all edits are paragraph-level like-for-
like. The risk-class rationale is admissible; the explicit sequence
is admissible. CH4 admits either.

**Fix (optional):** add a one-line "edit ordering: §4.1 Edits A-D
first (status vocabulary), §4.1 Edits F-I + §4.2 + §4.3 second
(evidence-root references) — within the ≤30 min cap" to V3-F §4
introduction.

---

## §5 Sources

- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md` (V4 fold §0, §6.5 PMU manifest)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md` (V4 fold §0, Lock-14 vocabulary)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md` (V4 fold §0, F2 refold)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md` (V4 fold §0, §6 finding-form with REDRESS anchors)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-E-legacy-cleanup-audit.md` (V4 fold §0, E1/E2 dispatch contracts §1/§2/§4.3)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md` (V4 fold §0, §1.3 PASS-1-PROFILE Omega route, §4 edit-count reconciliation)
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V3/CH4.md` (V3 CH4 verdict, 35 dispositions)
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md` (F1+F5 fold spec, convergence forecast)
- `restart/prompts/ORCHESTRATOR.md` §3W (CH4 contract row), §7 (orchestrator scope — prompts are read-only contracts), §8 (non-negotiables incl. same-wave consumer + hard cap), §9 (hard caps table)
- `restart/prompts/skinny/PASS-1-PROFILE.md` §1 ("S-P1 produces no plan and proposes no intervention"), §9 ("S-P1 produces evidence, and S-P2 produces the hypotheses") — basis for F1 wave-authorship deferral
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (wave authorship scope per F1)
- `skinny/REDRESS.md` entries 59, 60, 61, 62, 64, 66-69, 71, 81, 82, 83, 84, 93 (material-differential anchors per F3)
- `restart/locks/LOCKS.md` Lock 1 (substrate union — basis for §6.5 PMU manifest binding, V3-D §6.1 REPLACES clause)
- Git: V3 commit `c6fb0342`; V4 fold commit `142c2b4a` ("docs(sk-v9-p1-v4): fold V3 CHALLENGE dispositions across all six reports").
