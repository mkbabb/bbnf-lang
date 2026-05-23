# HARDENING ALPHA V3 — CONSOLIDATED (Pass Alpha SK-V13 → SK-V14)

Aggregator: SK-V14 CHALLENGE V3 over the V3 micro-redispatched artefacts
(commit `5e2ae78b4` landed the two V2 fresh-finding folds atomically:
F-V3-α-E-1 against `alpha-E-candidate-shortlist.md:362-387` and
F-V3-α-F-1 against `HANDOFF.md:192-197`; SYNTHESIS, α-A, α-B, α-C,
α-D, DISPATCH-CONTEXT STAND from the V2 baseline `958406257` per the V3
commit body). Seven lenses dispatched (CH1 CORRECTNESS, CH2 GENERALITY,
CH3 REGRESSION, CH4 COST, CH5 HIDDEN COUPLING, CH6 ANTI-PAPER-CLOSE,
CH7 OVERFIT-PRUNE) per the V1 lens binding extended by
`V2/CHALLENGE-V2-ADDENDUM.md §1`'s fold-verification + fresh-finding
overlay, carried into V3 as the confirming pass per `ORCHESTRATOR.md
§3Z` two-consecutive-cycle rule.

This consolidated authors the V3 verdict and the V4 fold dispositions
per `ORCHESTRATOR.md §3Z step 4`. Cycle convergence floor remains
≥ 95 % ACCEPT for **two consecutive** cycles; V1 was 86.86 % (FAIL),
V2 was 99.27 % (PASS first link of a new chain — clean at close with
its two fresh REVISEs both routed to V3 micro-fold and landed via
`5e2ae78b4`), V3 is **99.27 %** (PASS second link by aggregate floor —
but carries 2 orphan REVISEs surfaced by CH1's executable verification
of the V3-folded surface). V3 therefore does **NOT** close §3Z on its
own terms: the two-consecutive-cycle rule reads `(V_n ≥ 95 % AND
V_{n+1} ≥ 95 % AND V_{n+1} has zero orphan REVISEs)`; V3 clears the
percentage floor but leaves two orphan REVISEs that route to a V4
micro-fold. V4 + V5 close the chain at the V ≤ 5 ceiling per §3Z.

## §0 — V3 cycle verdict

### §0.1 — Per-lens dispositions (V3)

| Lens | ACCEPT | Total | Rate | REJECT | REVISE |
|---|---|---|---|---|---|
| CH1 CORRECTNESS | 51 | 53 | 96.23 % | 0 | 2 |
| CH2 GENERALITY | 33 | 33 | 100.00 % | 0 | 0 |
| CH3 REGRESSION | 31 | 31 | 100.00 % | 0 | 0 |
| CH4 COST | 34 | 34 | 100.00 % | 0 | 0 |
| CH5 HIDDEN COUPLING | 46 | 46 | 100.00 % | 0 | 0 |
| CH6 ANTI-PAPER-CLOSE | 42 | 42 | 100.00 % | 0 | 0 |
| CH7 OVERFIT-PRUNE | 36 | 36 | 100.00 % | 0 | 0 |
| **Aggregate** | **273** | **275** | **99.27 %** | **0** | **2** |

The 275-row denominator is preserved verbatim from V2 (V1's 274 → V2's
275 re-count of the α-C §2 P-7 cell under both CH5 and CH1 holds
through V3). Aggregate rate is byte-equivalent to V2 at 99.27 %; the
two REVISEs migrate from V2 (CH2 NF-1 + CH3 F-V3-1) to V3 (CH1 REV-1 +
CH1 REV-2) because the V3 micro-fold's surface itself introduced two
fresh editorial defects that CH1's executable verification caught and
CH2 + CH3's documentary verification did not.

### §0.2 — Convergence test

Per `ORCHESTRATOR.md §3Z`, convergence requires ≥ 95 % ACCEPT for **two
consecutive cycles**. V2 was 99.27 % (PASS — closed clean at V2-cycle
end with both fresh REVISEs routed to the V3 micro-fold and landed via
`5e2ae78b4`). V3 is **99.27 %** (PASS — clears the percentage floor by
4.27 pp). Both cycles clear the per-cycle floor; however, V3 surfaces
two NEW REVISEs (CH1 REV-1, CH1 REV-2) inside the V3-folded surface.

The two-consecutive-cycle rule's spirit binds convergence to a clean
close at the trailing cycle. A literal-percentage reading reads V2
(99.27 %) + V3 (99.27 %) = chain CLOSED; an orphan-REVISE-aware
reading reads V3 with 2 unresolved REVISEs ⇒ chain NOT CLOSED until
the REVISEs fold and a confirming pass holds with zero orphans.

This aggregator adopts the orphan-REVISE-aware reading per the V2 →
V3 precedent (V2 closed only because its two REVISEs routed to the V3
micro-fold and landed; V3 is symmetric — its two REVISEs route to a
V4 micro-fold). The V3 cycle verdict is therefore
**CONVERGED-EXPECTING-V4-MICRO-FOLD-PLUS-V5-CONFIRM**: V3 clears the
per-cycle floor with margin; the two-cycle close chain requires V4 to
land the orphan REVISEs and V5 to confirm the V4 surface holds with
zero new orphans. V5 is the last cycle permitted under §3Z's V ≤ 5
ceiling before BLOCKED escalation.

No CH lens issues a BINDING REJECT in V3; the V1 CH7 BINDING REJECT
on C-3's round-trip gate (FOLD-LANDED in V2 via E-1) holds in V3 with
CH7's documentary STRENGTHENING note that F-V3-α-E-1 "strengthens
CH7-1 + CH7-2 + CH7-4" — though CH1's executable verification then
caught the literal command as non-executable, surfacing REV-1 (see
§1's lens-depth divergence note). No V3 finding triggers immediate
plan-revise or redress-revert under any lens.

**Verdict: CONVERGED-EXPECTING-V4-MICRO-FOLD-PLUS-V5-CONFIRM.** V3
clears the single-cycle ≥ 95 % floor; the V4 micro-fold (≈ 13 min
wall-clock; two folds; LOW risk) closes the orphan REVISEs; V5
confirming pass closes the §3Z chain at the V ≤ 5 ceiling.

### §0.3 — REJECT list (verbatim, 0 total across all lenses)

**No V3 REJECTs.** Both V2 REVISEs (CH2 NF-1, CH3 F-V3-1) FOLD-LANDED
verbatim per V2 CONSOLIDATED §2 prescription (per V3 CH2 §2 + V3 CH3
§2 + V3 CH7 §3.1 confirmation against commit `5e2ae78b4`). The V1 +
V2 BINDING REJECTs (CH5 E-3 owner-paths Lock-1 triad; CH7 E-1
three-part round-trip + bypass-header detector) hold byte-equivalent
at V3 HEAD per the six-anchor preservation table in V3 CH5 §1 and the
fold-landing tracker in V3 CH7 §1.

### §0.4 — REVISE list (verbatim, 2 NEW V3 findings; 0 carry-over from V2)

Both V2 REVISEs (CH2 NF-1, CH3 F-V3-1) closed via the V3 micro-fold
commit `5e2ae78b4` — see §1 below. The two NEW V3 REVISEs are
surfaced fresh by CH1's executable verification of the V3-folded
surface; they live entirely inside the prose the V3 micro-fold
authored (so the V2 lenses could not have caught them, since the
surface did not yet exist at V2 close).

#### REV-1 — CH1 V3 fresh-finding (REVISE)

Verbatim from `V3/CH1.md §2 REV-1`:

> **REV-1 — α-E §5 falsifiability gate shell command not mechanically
> executable.**
>
> **Location:** `alpha-E-candidate-shortlist.md:362-375`.
> **Defect class:** falsifiability gate is presented as mechanically
> executable, but the literal shell command does not parse the actual
> `cargo metadata` schema.
>
> **Evidence (executed in `/Users/mkbabb/Programming/bbnf-lang`):**
>
> ```
> $ cargo metadata --format-version 1 --no-deps | jq -r '.workspace_metadata.bbnf.grammars | keys[]'
> jq: error (at <stdin>:1): null (null) has no keys
> $ cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
> bbnf
> json
> css_l4
> css_pretty
> google_sheets
> ebnf
> bnf
> csv
> math
> ```
>
> The `cargo metadata --format-version 1` JSON exposes workspace-level
> metadata under the top-level key **`.metadata`** (not
> `.workspace_metadata`); the grammar array entries are objects keyed
> by `ident`, so the canonical jq form is `.metadata.bbnf.grammars[].ident`,
> NOT `.workspace_metadata.bbnf.grammars | keys[]`. The latter pipes
> `null` into `keys[]` and fails; the `for g in $(…)` loop iterates
> over zero items; the `rm -rf … && cargo xtask … && git diff` body
> never executes; the gate silently passes regardless of whether the
> underlying round-trip would actually hold. A wave that emits a
> hand-patched `crates/core/src/runtime/<g>/` file would not be caught
> by the gate as written.
>
> Secondary defect on the same fold: the parenthetical narrative at
> line 365 enumerates 8 grammars `{json, css_l4, google_sheets, bbnf,
> csv, ebnf, bnf, math}`; `Cargo.toml:18-29` carries 9 (adds
> `css_pretty`). The V3 fold inherited the V2-era count without
> re-counting against `Cargo.toml` at fold-authoring time.
>
> **Risk class:** LOW (single-paragraph edit; no architectural change;
> gate substance preserved post-fix).

REV-1 is a CH1 (CORRECTNESS) lens finding; the lens binding is "every
claim cites file:line, commit SHA, RESULTS row, or REDRESS entry that
resolves" and "falsifiability gates are measurable". A gate whose
shell command fails closed under the actual `cargo metadata` schema
is not measurable; the V3 lens caught what V2's NF-1 prescription
wrote in good faith but did not validate against the runtime.

#### REV-2 — CH1 V3 fresh-finding (REVISE)

Verbatim from `V3/CH1.md §2 REV-2`:

> **REV-2 — HANDOFF §7 cite to `SYNTHESIS.md §1.3` is a stale anchor.**
>
> **Location:** `HANDOFF.md:195-196`.
> **Defect class:** citation mis-anchor; the cited § does not carry the
> named reconciliation.
>
> **Evidence:** SYNTHESIS §1.3 (lines 211-218, header at line 211) is
> the *post-PRUNE rolling delta* — four lines reading
> `JSON parse_only: 0 / 17`, `JSON direct: 0 / 17`, `JSON typed:
> 0 / 17`, `CSS L4: 0 / 24`. It does not enumerate the 4+7 vs 6+11
> reconciliation. The 4+7 vs 6+11 reconciliation is authored at
> SYNTHESIS §0.2 (lines 73-84) under the header "Numeric-divergence
> reconciliation (per CH6 §2.2 REJ-2)" and is re-stated at SYNTHESIS
> §1.2's reconciliation block (lines 200-209) under the header
> "Reconciliation (per CH6 §2.2 REJ-2 + §0.2 above)". HANDOFF §7's V3
> fold should cite §0.2 OR §1.2's reconciliation block — not §1.3.
>
> **Risk class:** LOW (single-clause edit; the broadening from 41 to 47
> is correct; only the citation anchor is wrong).

REV-2 is a CH1 (CORRECTNESS) lens finding; the §1.3 anchor was the
V2 NF-1 → F-V3-1 prescription's recommended text (per V2 CONSOLIDATED
§2.2 lines 322-329); the V3 lens caught that the prescription itself
inherited a stale cite from the V2 NF-1 author. The broadening of 41
→ 47 is correct on the row-count axis; only the citation anchor is
wrong.

### §0.5 — Cross-cycle convergence chain

| Cycle | Aggregate | Floor met? | Orphan REVISEs at close | Chain status |
|---|---|---|---|---|
| V1 | 86.86 % | NO | 29 (all routed to V2 fold) | chain broken |
| V2 | 99.27 % | YES | 2 (CH2 NF-1, CH3 F-V3-1; routed to V3 micro-fold; landed `5e2ae78b4`) | link 1 of 2 (chain restart) |
| V3 | **99.27 %** | YES | 2 (CH1 REV-1, CH1 REV-2; route to V4 micro-fold) | link 2 of 2 by percentage; chain NOT closed by orphan-REVISE rule |
| V4 (forecast) | ≥ 99 % (point 100 %) | YES expected | 0 expected | link 2 of 2 (chain re-anchors) |
| V5 (forecast) | ≥ 95 % (point 100 %) | YES expected | 0 expected | link 3 of 3 → **LOCKED** at V ≤ 5 ceiling |

The chain re-reads at V3: the percentage floor holds (4.27 pp margin)
but orphan REVISEs do not. The V4 micro-fold (≈ 13 min wall-clock) +
V5 confirming pass close §3Z at the V ≤ 5 ceiling; both cycles are
expected to converge at 100 % under verbatim micro-fold landing per
the V2 → V3 precedent (which produced exactly the pattern this
aggregator now observes — V2's two REVISEs landed cleanly via the
V3 micro-fold, and V3 is forecast to repeat the pattern).

## §1 — V2 → V3 fold landing matrix + lens-depth divergence

Per V3 dispatch context §1, each V2 REVISE (2 total: NF-1, F-V3-1)
verified against V3 evidence post-micro-fold commit `5e2ae78b4` and
marked **FOLD-LANDED** / **FOLD-PARTIAL** / **FOLD-MISSING**. Both
folds landed; one carries a lens-depth divergence between
documentary (CH7) and executable (CH1) verification that is itself a
methodological note for future cycles.

### §1.1 — V2 REVISEs (2 total — both FOLD-LANDED)

| V2 Lens | V2 finding (short) | V3 fold ID | V3 site | Status |
|---|---|---|---|---|
| CH2 | NF-1 — C-3 round-trip gate hardcodes 8-grammar literal | F-V3-α-E-1 | `alpha-E-candidate-shortlist.md:362-387` (V2 lines 362-365 expanded to derived-enumeration form per V2 CONSOLIDATED §2.1 prescription; cross-cite to `LOCKS.md:220` added beyond the V2 recommendation) | **FOLD-LANDED** (CH2 V3 §2 + CH7 V3 §3.1) |
| CH3 | F-V3-1 — HANDOFF §7 carry-over count desync 41 vs 47 | F-V3-α-F-1 | `HANDOFF.md:192-197` (broadens 41 → 47 rows per V2 CONSOLIDATED §2.2 prescription character-for-character) | **FOLD-LANDED** (CH3 V3 §2) |

**Fold-landing roll-up: 2 V2 findings = 2 FOLD-LANDED.** Zero
FOLD-PARTIAL; zero FOLD-MISSING. Both folds landed verbatim per the
V2 CONSOLIDATED §2 prescription; F-V3-α-E-1 strengthens the
prescription with an additional `LOCKS.md:220` cross-cite that the
V2 author did not require.

### §1.2 — Lens-depth divergence (methodological note for V4+)

The two V3 lenses that audited F-V3-α-E-1 reached opposing surface
verdicts via different verification depths:

- **CH7 V3 §3.1 (documentary verification).** Read the post-fold
  prose; verified that the gate text relocated grammar-derivation
  authority from a hardcoded literal to a `cargo metadata + jq`
  query; concluded "CH7-1 STRENGTHENED + CH7-2 STRENGTHENED + CH7-4
  preserved verbatim." Did NOT execute the literal shell command.
- **CH1 V3 §2 REV-1 (executable verification).** Read the post-fold
  prose; executed the literal shell command in the live workspace;
  observed `jq: error (at <stdin>:1): null (null) has no keys`;
  concluded the gate is non-executable as written.

Both verdicts are internally consistent under their lens scope: CH7's
charter is the audit-overlay integrity of the gate's intent, and the
intent is honoured (derived enumeration > hardcoded literal); CH1's
charter is "falsifiability gates are measurable", and a gate whose
literal command fails closed under the actual `cargo metadata` schema
is not measurable.

The divergence is a useful methodological note for future hardening
cycles: documentary lens depth (read-the-prose-and-judge-intent) is
necessary but not sufficient for falsifiability-gate verification;
executable lens depth (run-the-literal-command-against-the-workspace)
catches a class of defects that documentary depth cannot. The V4
micro-fold prescription below resolves both lens views — the fold
preserves the metadata-derivation intent CH7 commended AND makes the
literal shell command match the actual `cargo metadata` schema that
CH1's execution revealed.

For V5 + downstream cycles, lens dispatch should consider an explicit
"executable verification" requirement on any falsifiability gate that
ships a literal shell command; this would surface CH1-class defects
at the same cycle the documentary verification surfaces CH7-class
verdicts.

### §1.3 — Per-artefact V3 convergence digest

| Artefact | V2 disposition | V3 disposition | Change |
|---|---|---|---|
| SYNTHESIS.md | ACCEPT (all V2 folds landed) | ACCEPT (all V2 carries holds; no V3 fold) | unchanged (407 lines) |
| HANDOFF.md | ACCEPT (CH3 NF surfaces §7 line-193 carry-over count desync) | ACCEPT-WITH-REVISE (F-V3-α-F-1 landed at :192-197; CH1 REV-2 surfaces §1.3 cite anchor mis-binding) | 242 → 245 lines |
| α-A | ACCEPT (all V2 folds landed) | ACCEPT (no V3 fold) | unchanged (420 lines) |
| α-B | STAND | STAND | unchanged |
| α-C | ACCEPT (V2 fold landed) | ACCEPT (no V3 fold) | unchanged (460 lines) |
| α-D | STAND | STAND | unchanged |
| α-E | ACCEPT (all V2 folds landed; CH2 NF surfaces §5 hardcoded grammar-list at gate site) | ACCEPT-WITH-REVISE (F-V3-α-E-1 landed at :362-387; CH1 REV-1 surfaces shell-command non-executability + 8-vs-9 grammar parenthetical count drift) | 800 → 815 lines |
| DISPATCH-CONTEXT | STAND | STAND | unchanged |

## §2 — V4 fold dispositions (binding V4 micro-redispatch)

Per `[agent-orchestration]` + V3 dispatch §3, the V4 cycle is a
**second micro-fold** under the §3Z two-consecutive-cycle rule. Two
V3 fresh-finding REVISEs (REV-1, REV-2) route to a **micro-α-
redispatch** before V4 fires. Both are single-paragraph / single-clause
edits with LOW risk; total V4 redispatch surface ≈ 13 minutes
wall-clock + lens-agent cap.

### §2.1 — F-V4-α-E-1 (CH1 REV-1 — α-E §5 shell command + grammar-count repair)

**Owner:** α-E (single redress wave on α-E §5; SYNTHESIS untouched).
**Hard cap:** ≈ 10 min (`[dispatch-hard-cap]` narrow-fold cap;
single-paragraph shell-command + parenthetical correction; LOW risk).
**Risk:** LOW (no architectural change; gate substance preserved;
shell-command repair only).

**Fold prescription.** Fix the broken `cargo metadata | jq` command in
α-E:362-387's C-3 round-trip gate. The agent MUST: (i) read
`Cargo.toml` lines 18-29 to see the actual workspace.metadata.bbnf
structure; (ii) run `cargo metadata --format-version 1 | jq` against
the actual workspace to determine the correct key path; (iii) write a
shell command that executes correctly; (iv) update the parenthetical
to enumerate all 9 grammars (including `css_pretty`) OR remove the
parenthetical entirely and rely on the derived enumeration as the
single source.

Recommended text (per V3 CH1 §3 F-V4-α-E-1 recommendation, validated
against the executed `cargo metadata` output enumerated under
V3/CH1.md §2 REV-1 evidence block):

> **Round-trip (core tree, all rostered grammars).** For each grammar
> name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in
> the top-level `Cargo.toml` (currently `{bbnf, json, css_l4,
> css_pretty, google_sheets, ebnf, bnf, csv, math}` — the list is
> metadata-derived, not source-of-truth at the gate site; the canonical
> shell form is `for g in $(cargo metadata --format-version 1 | jq -r
> '.metadata.bbnf.grammars[].ident'); do rm -rf
> "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git
> diff -- "crates/core/src/runtime/${g}/" || exit 1; done`): the loop
> produces empty `git diff` output for every iterated grammar.

The downstream parity-with-C-1 paragraph at lines 375-387 stands
verbatim — only the jq path + the grammar-list parenthetical change.

**Forward-discipline rationale.** A mechanically-executable
falsifiability gate is the CH1 binding; a gate whose shell command
fails closed under the actual `cargo metadata` schema is no gate. The
correction preserves the metadata-derivation discipline F-V3-α-E-1
introduced; only the jq path is repaired and the grammar-count drift
closed. The strengthening to "all rostered grammars" + the
`LOCKS.md:220` cross-cite F-V3-α-E-1 introduced both stand verbatim.

### §2.2 — F-V4-α-F-1 (CH1 REV-2 — HANDOFF §7 citation anchor repair)

**Owner:** α-F (single redress wave on HANDOFF; SYNTHESIS untouched).
**Hard cap:** ≈ 3 min (`[dispatch-hard-cap]` narrow-fold cap;
single-clause edit; citation anchor only; LOW risk).
**Risk:** LOW (broadening from 41 → 47 stands; only the cite changes).

**Fold prescription.** Replace the `SYNTHESIS.md §1.3` citation at
HANDOFF:195-196 with `SYNTHESIS.md §0.2` (the lines 73-84
dispatch-vs-ledger reconciliation block) OR `SYNTHESIS.md §1.2
reconciliation block` (lines 200-209). Pick whichever matches the V2
F-1 fold's verbatim landing text — the V2 F-1 fold landed both anchors
per V2 CONSOLIDATED §1.1 row CH6 REJ-2 (`SYNTHESIS.md:73-84 + :194-195
+ :200-209`); either anchor is valid; the §0.2 anchor is preferred
because it is the canonical first authoring of the reconciliation
paragraph.

Recommended text:

> - inherits any of the audit-falsified admit rows (25 CSS + 5
> parse_only + 6 direct + 11 typed = **47 rows** under the broader
> `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower
> bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
> §0.2` reconciliation) as carry-over without fresh material
> differential under rebound comparator;

(Alternatively `SYNTHESIS.md §1.2 reconciliation block` — both surfaces
carry the same binding per `SYNTHESIS.md:200-209`.)

**Forward-discipline rationale.** HANDOFF §7 is the canonical
pre-admit gate S-P3 consults; a citation that resolves to the wrong §
sows downstream mistrust. The CH1 lens binds "every claim cites
file:line, commit SHA, RESULTS row, or REDRESS entry that resolves."
The correction restores citation integrity without touching the §7
carry-over guard structure or the broadening F-V3-α-F-1 introduced.

### §2.3 — V4 micro-α-redispatch sequencing

Both folds touch different artefacts and may parallelise:

1. **F-V4-α-E-1 dispatch** (α-E single redress wave, ≈ 10 min cap;
   shell-command + parenthetical correction at
   `alpha-E-candidate-shortlist.md:362-387`).
2. **F-V4-α-F-1 dispatch** (α-F single redress wave, ≈ 3 min cap;
   single-clause edit at `HANDOFF.md:195-196`).
3. **CHALLENGE V4 lens dispatch** (7 lenses × 30 min cap;
   parallelisable; per-lens fold-verification overlay focuses on the
   two REV folds plus full per-§ ACCEPT roll-up + executable
   verification of the F-V4-α-E-1 shell command as a methodological
   reinforcement per §1.2).
4. **CHALLENGE V4 aggregator** authors HARDENING-ALPHA-V4-CONSOLIDATED
   and commits the V4 hardening files atomically.

Per `[agent-orchestration]`, the two α-redispatches touch non-
overlapping files (α-E ↔ α-F-via-HANDOFF) and may parallel-dispatch
without staging-race risk. The V2 → V3 atomic-commit mechanism (one
commit for both V3 micro-folds, per-fold attribution in commit body)
remains the binding posture; V4 micro-folds should likewise atomic-
commit per the V2 attribution discipline.

## §3 — V4 + V5 convergence forecast

### §3.1 — Lens-level forecast post-V4-fold

If F-V4-α-E-1 + F-V4-α-F-1 land verbatim, V4 lens-level dispositions:

| Lens | V3 ACCEPT-rate | V4 forecast |
|---|---|---|
| CH1 | 96.23 % | **100.00 %** (REV-1 + REV-2 close; two cells flip ACCEPT) |
| CH2 | 100.00 % | 100.00 % (unchanged; F-V4-α-E-1 preserves metadata-derivation intent) |
| CH3 | 100.00 % | 100.00 % (unchanged) |
| CH4 | 100.00 % | 100.00 % (unchanged) |
| CH5 | 100.00 % | 100.00 % (unchanged) |
| CH6 | 100.00 % | 100.00 % (unchanged) |
| CH7 | 100.00 % | 100.00 % (unchanged; documentary verification of the strengthened gate holds) |
| **Aggregate** | **99.27 %** | **100.00 % (275/275)** point forecast |

Realistic floor under "0–3 new findings per lens" historical
assumption (per V1 → V2 → V3 actual record of 7+29 V1 / 2 V2 / 2 V3
findings — a converging series): V4 floor ≥ 99 %; V4 point forecast
100 % under verbatim micro-fold landing.

### §3.2 — V5 confirming pass forecast

V5 fires as the confirming pass over the V4 artefacts. Under the V2 →
V3 precedent (V2 micro-fold landed clean; V3 found 2 fresh defects
inside the V2 micro-fold's surface), the V5 cycle may find 0–2 new
fresh defects in the V4 micro-fold's surface. The V4 prescriptions
above are tightly scoped (one shell-command repair + one citation
anchor swap); the surface for V5 to surface fresh defects is minimal.

V5 forecast: aggregate ≥ 99 % (point 100 %); zero orphan REVISEs
expected; chain closes at V5 per §3Z.

### §3.3 — Two-consecutive-cycle convergence chain (post-V4 + V5)

| Cycle | Aggregate | Floor met? | Orphan REVISEs at close | Chain status |
|---|---|---|---|---|
| V1 | 86.86 % | NO | 29 (routed) | chain broken |
| V2 | 99.27 % | YES | 2 (routed; landed `5e2ae78b4`) | link 1 of 2 (chain restart) |
| V3 | 99.27 % | YES | 2 (route to V4 micro-fold) | link 2 of 2 by percentage; chain NOT closed by orphan-REVISE rule |
| V4 | ≥ 99 % (forecast 100 %) | YES expected | 0 expected | link 1 of 2 (re-anchor) |
| V5 | ≥ 95 % (forecast 100 %) | YES expected | 0 expected | link 2 of 2 → **LOCKED** |

The §3Z two-consecutive-cycle rule is expected to be satisfied by the
V4 + V5 pair at the V ≤ 5 ceiling. At V5 convergence, the SK-V14
contract locks immediately; G-Alpha auto-signs per the V2 addendum §4
forecast; the orchestrator proceeds directly to S-P0 per the SK-V14
ORCHESTRATOR-PROMPT pin.

V5 is the **last cycle permitted** under `ORCHESTRATOR.md §3Z`'s
V ≤ 5 ceiling. If V5 does not close (i.e., V5 surfaces fresh orphan
REVISEs that cannot be folded into V5's own micro-redispatch surface
before close), the bracket BLOCKED-escalates per §3Z. The V4 fold
prescriptions are tightly scoped specifically to avoid this outcome —
both are surgical edits to V3-folded surfaces, and the V2 → V3
precedent shows micro-folds of this scope land clean.

### §3.4 — Structural concerns surviving V3 (none)

No CH lens issues an escalation flag in V3. The CH7 V1 BINDING REJECT
remains closed via E-1 + V3 documentary STRENGTHENING; the CH5 V1
BINDING REJECT remains closed via E-3 (per the six-anchor
preservation table in V3 CH5 §1). All other V1 / V2 REJECTs +
REVISEs close FOLD-LANDED per §1.

The V3 cycle introduced TWO new defects under fold pressure (CH1
REV-1, CH1 REV-2) — both at the editorial-discipline tier, neither
architectural, neither convergence-blocking. The V3 lens-depth
divergence noted in §1.2 (CH7 documentary verification + CH1
executable verification reaching opposing verdicts on the same gate)
is a methodological note that strengthens future cycles rather than
a finding against this cycle. The fold work itself preserved CH6
anti-paper-close character throughout per V3 CH6 §3 — both V3 folds
add gates / widen guards rather than removing them, name strict-
subset relationships explicitly, and bind prior narratives to
measurable artefacts.

### §3.5 — Cycle envelope and contract lock

V3 cycle wall-clock: ≈ 10 min α-redispatches (two V2 → V3 single-edit
folds atomic-committed at `5e2ae78b4`) + ≈ 30 min × 7 = 210 min CH
lens-agent work (parallelisable to ≈ 30–45 min) + aggregator. V4
envelope is ≈ 13 min α-redispatches (one 10-min α-E shell-command
repair + one 3-min α-F citation swap) + lens-cycle work + aggregator.
V5 envelope is ≈ 0 min α-redispatches (confirming pass; no folds) +
lens-cycle work + aggregator. V4 + V5 should close inside two
orchestrator sessions or one session if dispatched in tight sequence.

Post-V5 lock: SK-V14 contract locks; G-Alpha auto-signs; S-P0 fires;
PRUNE-1 → PRUNE-2 → PRUNE-3 → PRUNE-4 wave program initiates per the
C-5 → C-1 → C-3 → C-4 → C-2 sequencing in α-E §9.

## §4 — Final aggregator verdict

V3 aggregate ACCEPT-rate **99.27 %** across 275 per-§ dispositions;
**0** REJECT (all V1 + V2 REJECTs FOLD-LANDED including the V1 CH7
BINDING REJECT (with V3 documentary STRENGTHENING) and the V1 CH5
BINDING REJECT (per V3 CH5 §1 six-anchor preservation)); **2** NEW
REVISE (CH1 REV-1 α-E §5 shell-command non-executability + 8-vs-9
grammar parenthetical drift; CH1 REV-2 HANDOFF §7 `SYNTHESIS.md §1.3`
stale citation anchor); 2 V2 REVISEs (CH2 NF-1, CH3 F-V3-1) all
FOLD-LANDED via commit `5e2ae78b4`.

**Cycle verdict: CONVERGED-EXPECTING-V4-MICRO-FOLD-PLUS-V5-CONFIRM.**
V3 clears the single-cycle ≥ 95 % floor with 4.27 pp margin; V3
matches V2's aggregate floor verbatim but does not close the §3Z
two-consecutive-cycle rule on the orphan-REVISE-aware reading. V4
micro-fold (≈ 13 min wall-clock) closes the orphan REVISEs; V5
confirming pass closes the §3Z chain at the V ≤ 5 ceiling.

V4 dispatch is a two-fold micro-α-redispatch (F-V4-α-E-1 ≈ 10 min
shell-command repair; F-V4-α-F-1 ≈ 3 min citation swap;
parallelisable; LOW risk) followed by the seven-lens V4 challenge
pass. V4 point forecast 100 % (275/275); realistic floor ≥ 99 %. No
architectural-block surfaces. No source-side fold implicated. V4
envelope is docs-only.

V5 dispatch is the seven-lens confirming pass with no α-redispatch
expected. V5 point forecast 100 % (275/275); realistic floor ≥ 95 %.
At V5 convergence, the SK-V14 contract locks immediately.

The V1 → V2 → V3 cycle chain surfaced 7 + 29 V1 + 2 V2 + 2 V3
findings = 40 total; 38 of 40 FOLD-LANDED at V3 close (95 %
fold-completion rate); 2 of 40 route to V4 (5 %). The convergent-
cycle behaviour is consistent with the V1 CONSOLIDATED §3.1 "0–3 new
findings per lens" historical assumption and tightens at each cycle
(7+29 → 2 → 2 → forecast 0). The lens-depth methodological note
captured in §1.2 (documentary vs executable verification) is the V3
cycle's substantive contribution beyond the per-row dispositions; it
inflects the V4 lens dispatch toward explicit executable verification
of any shipped literal shell command.

The SK-V14 alpha-bracket sits two cycles from lock at the V ≤ 5
ceiling; V4 fires next.
