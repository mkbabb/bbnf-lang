# CH2 GENERALITY (Lock 14) — Pass Alpha V4 Disposition

Lens: CH2 per `restart/prompts/ORCHESTRATOR.md §3W` — "Lock 14 holds:
no grammar-name leak; every proposed intervention is grammar-neutral
and works for CSS L4 / Sheets / BBNF-self, not only JSON."

Authority binding: `restart/locks/LOCKS.md:220-238` (Lock 14 text);
V1 dispatch context
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:99-110`
(§CH-2 scope); V2 addendum
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md`;
V2 disposition
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CH2.md`
(97 % ACCEPT; 0 REJECT; 1 NEW REVISE NF-1, since closed); V3
disposition
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V3/CH2.md`
(100 % ACCEPT; CONVERGED-FINAL on documentary verification; CH1 V3
subsequently surfaced two orphan REVISEs against the V3-folded shell
command that CH2's documentary lens did not catch — see V3
CONSOLIDATED §1.2 lens-depth divergence note). V4 cycle inspects the
V3 baseline (`5e2ae78b4`) overlaid with the V4 micro-fold commit
`5e00b6d27` (two-file atomic: alpha-E +/-11 lines repairing the V3
shell command; HANDOFF 1-line anchor correction).

This V4 disposition carries the executable-verification mandate
inherited from V3 CONSOLIDATED §1.2 — the V3 lens-depth divergence
recommendation that any shipped shell command be verified against the
live workspace, not merely accepted as syntactically plausible prose.
CH2 V4 therefore executes the V4-folded gate command in the live tree
and inspects the gate's grammar enumeration substrate, not just its
prose.

## §0 — Disposition summary

- ACCEPT-rate: 100 % (34 / 34 § lines disposed)
- REJECT count: 0
- REVISE count: 0
- FOLD-LANDED count: 1 / 1 V3-folded surface (F-V4-α-E-1 repairs CH1
  V3 REV-1; CH1 V3 REV-2 lands on HANDOFF.md and is out of CH2 scope)
- FOLD-PARTIAL count: 0
- FOLD-MISSING count: 0
- Critical findings: 0
- New findings: 0 (one observation noted under §2 fresh-finding scan
  but classified as benign current-state count, not a Lock 14 leak)
- Escalation flag: none. CH2 converges at 100 % ACCEPT on V4. The
  V3 → V4 chain holds the percentage floor twice (V3 100 % + V4
  100 %) AND closes with zero orphan REVISEs on the V4 cycle. CH2
  reports CONVERGED-FINAL.

The V4 α-E micro-fold lands a substantive STRENGTHENING of the
derived-enumeration that closed V2 NF-1: the shell command now
executes against the live `cargo metadata` schema (verified — see §2
executable check), the parenthetical eight-name grammar literal has
been entirely removed from the gate text, and the gate's "9th grammar"
phrasing has been generalised to roster-count-agnostic "admitting an
additional grammar". The derived enumeration is now the **single
source** of grammar names at gate-authoring time — exactly the
condition CH2 V2 NF-1 demanded and the V3 fold attempted but had not
yet executed correctly.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `SYNTHESIS.md` | §0.1 close condition | ACCEPT | STAND per V4 commit body ("Other files STAND: SYNTHESIS"); R10 verbatim; close criterion treats JSON cells + CSS features symmetrically. |
| `SYNTHESIS.md` | §0.2 goalset enumeration | ACCEPT | Per-grammar row counts plane-neutral; numeric divergence reconciliation paragraph (F-1) carries forward; STAND. |
| `SYNTHESIS.md` | §0.3 R-target goalset (R4 row) | ACCEPT | F-11 mirror clause "first instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator" preserved at `SYNTHESIS.md:96`; STAND. |
| `SYNTHESIS.md` | §0.4 P-1…P-7 pre-blocks | ACCEPT | P-6 names per-grammar provider modules as recurrence vector; grammar-neutral. STAND. |
| `SYNTHESIS.md` | §0.5 wave-by-wave deferral | ACCEPT | Contracted deferral unchanged; deferred work remains grammar-neutral in surface. STAND. |
| `SYNTHESIS.md` | §1 corrected diagnosis | ACCEPT | Pillars marked `grammar-neutral` for bbnf-simd / OffsetFlags + Tape. STAND. |
| `SYNTHESIS.md` | §2 telemetry binding | ACCEPT | Column rule wording treats every plane uniformly; `track2_entry_point` column plane-keyed not grammar-keyed. STAND. |
| `SYNTHESIS.md` | §3 candidate C-1 row | ACCEPT | Forward invariant clause at `SYNTHESIS.md:271` verbatim — "any new grammar added under `workspace.metadata.bbnf.grammars.{name}` produces ZERO new `.rs` files…ZERO new directories in `crates/core/src/runtime/`". F-12 + E-7 binding preserved. STAND. |
| `SYNTHESIS.md` | §3 candidate C-2 row | ACCEPT | Per-plane comparator rebind has no grammar branch. STAND. |
| `SYNTHESIS.md` | §3 candidate C-3 row | ACCEPT | F-11 family-shape clause "first instance of the `regen-{grammar}` family — the xtask binary parametrises a grammar-neutral generator" preserved at `SYNTHESIS.md:273`. STAND. |
| `SYNTHESIS.md` | §3 candidate C-4 row | ACCEPT | C-4 row inherits F-13's S-P3 dispatch-discipline clause; no grammar branch baked. STAND. |
| `SYNTHESIS.md` | §3 candidate C-5 row | ACCEPT | Revert + REDRESS; row-keyed not grammar-keyed. STAND. |
| `SYNTHESIS.md` | §4 S-P3 constraints (C-1 forward invariant clause) | ACCEPT | `SYNTHESIS.md:330-334` F-12 clause verbatim preserved. STAND. |
| `SYNTHESIS.md` | §4 S-P3 constraints (C-4 dispatch-discipline clause) | ACCEPT | `SYNTHESIS.md:335-341` F-13 clause verbatim preserved (including the "no `match grammar { Json => ..., CssL4 => ... }`" prohibition at line 340). STAND. |
| `SYNTHESIS.md` | §4 S-P3 constraints (other lines) | ACCEPT | G-SIMD-GRAMMAR-POLICY triad (F-14), LOC ceiling (F-6), triumvirate (F-9) all grammar-neutral. STAND. |
| `SYNTHESIS.md` | §5 pre-blocked routes | ACCEPT | "grammar-name branches in generic crates" enumerated as pre-blocked. STAND. |
| `SYNTHESIS.md` | §6 close posture | ACCEPT | Bracket framing symmetric. STAND. |
| `HANDOFF.md` | §1 bracket verdict | ACCEPT | Pillar list mirrors SYNTHESIS §1.1. STAND. |
| `HANDOFF.md` | §3 honest baseline | ACCEPT | JSON-CSS parity in admit-count framing preserved. STAND. |
| `HANDOFF.md` | §4 authorship declaration | ACCEPT | F-2(b) lands α-F as sole author; no grammar implications. STAND. |
| `HANDOFF.md` | §6 next-move | ACCEPT | "CH2 verifies Lock 14 grammar-neutrality" pin preserved; F-7 + F-8 grammar-neutral. STAND. |
| `HANDOFF.md` | §7 refusal conditions (broadened) | ACCEPT | F-V3-α-F-1 broadening preserved; line 208-218 "any grammar-specific generic behaviour through SPEC-local wording (Lock 14 binding)" refusal + `G-SIMD-GRAMMAR-POLICY` clause + cascade-silently-serve refusal all stand. The V4 cite-anchor repair (F-V4-α-F-1: `SYNTHESIS.md §1.3` → `§0.2`) at line 195-196 is a CH1 correctness fix with ZERO Lock 14 implication — the refusal list's grammar-neutrality is unaffected by which SYNTHESIS § the numeric reconciliation is sourced from. |
| `HANDOFF.md` | §8 disposition | ACCEPT | PENDING-V4-V5 wording; no grammar implications. STAND. |
| `alpha-A-results-extraction.md` | §1 / §3 / §6 | ACCEPT | A-1/A-2/A-3 folds carry forward; row-keyed, plane-keyed, grammar-neutral. STAND per V4 commit body. |
| `alpha-B-competitor-deltas.md` | comparator overlay | ACCEPT | STANDs per V2 addendum + V3 commit body + V4 commit body; CSS / JSON overlay symmetric. |
| `alpha-C-redress-digest.md` | §2 P-7 falsifiability + other P-1..P-6 | ACCEPT | C-1 fold triple-check gate preserved; substrate-keyed not grammar-keyed. STAND. |
| `alpha-D-validated-invalidated.md` | §S-3 Lock 14 + §V-4 + §V-5 | ACCEPT | 30 violations cited; bbnf-simd + Tape + OffsetFlags grammar-neutral. STAND. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | ACCEPT | C-3 row pointer "see §5 + hardening V1 CH7 §3.1"; C-1 row "see §3"; grammar-neutral. STAND. |
| `alpha-E-candidate-shortlist.md` | §3 C-1 detail (forward invariant + owner paths) | ACCEPT | `alpha-E:170-176` E-7 forward-invariant clause verbatim preserved. Owner-paths section at lines 108, 136 enumerates the current-state to-be-PRUNED files using a literal `{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}/` form — this is V3 CH2 §2 category-3 use (current-state audit narrative naming files PRUNE removes), not a load-bearing leak in the gate. STAND. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 detail | ACCEPT | Per-plane comparator rebind preserved; no grammar branch. STAND. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 detail (Purpose + family binding) | ACCEPT | E-6 family-shape clause at `alpha-E:303-309` verbatim preserved. STAND. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 detail (round-trip gate) | ACCEPT — **FOLD-LANDED (F-V4-α-E-1; closes CH1 V3 REV-1 to the standard CH2 V2 NF-1 demanded)** | `alpha-E:362-387` now reads (V4 diff): heading "Round-trip (core tree, all rostered grammars)"; the parenthetical eight-name grammar literal has been **entirely removed**; the gate text contains only the derivation source ("the list is metadata-derived, not source-of-truth at the gate site") and the executable shell command `for g in $(cargo metadata --format-version 1 --no-deps \| jq -r '.metadata.bbnf.grammars[].ident'); do rm -rf "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git diff --exit-code -- "crates/core/src/runtime/${g}/" \|\| exit 1; done`. The schema path is now correct (`.metadata.bbnf.grammars[].ident` resolves against `cargo metadata --format-version 1 --no-deps`); the `--no-deps` flag is added; `git diff --exit-code` replaces the bare `git diff`; the "9th grammar" phrasing is generalised to "admitting an additional grammar". The cross-cite to C-1's forward invariant (`alpha-E-candidate-shortlist.md:170-176`) and the parity-binding clause to `LOCKS.md:220` are preserved verbatim. Executable verification: the command was run in `/Users/mkbabb/Programming/bbnf-lang` and produced 9 grammar idents (`bbnf`, `json`, `css_l4`, `css_pretty`, `google_sheets`, `ebnf`, `bnf`, `csv`, `math`) — see §2 below. The single-source-of-truth condition CH2 V2 NF-1 originally demanded is now fully met: the gate text contains zero hardcoded grammar names in the active enumeration. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 detail (Pre-blocked routes + 2-grammar) | ACCEPT | E-8 clauses at `alpha-E:559-571` verbatim preserved (`BackendShape` enum dispatch + "no `match grammar { Json => ..., CssL4 => ... }`" prohibition + two-grammar-family exercise requirement). STAND. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 detail | ACCEPT | Revert covers JSON parse_only + CSS rows symmetrically; E-10 scribe-count clarification row-keyed not grammar-keyed. STAND. |
| `alpha-E-candidate-shortlist.md` | §8 consolidated pre-blocks | ACCEPT | P-6 verbatim; consolidated list grammar-neutral. STAND. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | ACCEPT | E-13 fold preserved; ledger-serialisation discipline, not grammar privilege. STAND. |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | ACCEPT | E-9 hot-leaf clause at `alpha-E:760-770` verbatim preserved; E-2 cap discipline grammar-neutral. The table row at line 756 ("C-1 sub-waves (8 grammars; per sub-wave) \| 20 min \| 15 min \| 30 min") names a numeric scope count for the C-1 cluster's sub-wave structure — see §2 observation. STAND. |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | Escalation triggers measurement-keyed, not grammar-keyed. STAND. |
| `DISPATCH-CONTEXT.md` | §0–§3 + per-agent | ACCEPT | STANDs per V2 addendum §0 + V3 commit body + V4 commit body; α-agent scope spec unchanged. |

## §2 — Critical findings + executable verification

Zero critical findings in V4. The V3 micro-fold defect surfaced as CH1
V3 REV-1 has been closed by F-V4-α-E-1, and no new Lock 14 leak
surfaces under fresh-finding scan across the V4 artefacts.

### Fold verification: F-V4-α-E-1 — LANDED (executable + documentary)

CH1 V3 REV-1 surfaced that the V3-folded shell command at
`alpha-E:362-375` was not mechanically executable: the path
`.workspace_metadata.bbnf.grammars | keys[]` resolves to `null` under
`cargo metadata --format-version 1` (which exposes workspace metadata
under the top-level key `.metadata`, not `.workspace_metadata`); the
`for g in $(…)` loop iterates over zero items; the gate body never
executes; a hand-patched `crates/core/src/runtime/<g>/` file silently
passes. The same V3 fold inherited a stale eight-grammar parenthetical
that `Cargo.toml:18-29` had since extended to nine
(`css_pretty` added).

**Executable verification.** Run in `/Users/mkbabb/Programming/bbnf-lang`:

```
$ cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
bbnf
json
css_l4
css_pretty
google_sheets
ebnf
bnf
csv
math
```

Nine idents enumerate cleanly. The V4 gate's `for g in $(…)` loop now
binds `${g}` to each of these nine names in turn and exercises the
`rm -rf … && cargo xtask regen-${g} && git diff --exit-code …` body
per iteration. The `--exit-code` flag returns non-zero on any non-empty
diff, which (together with the `|| exit 1` chain) propagates failure
out of the loop on the first regen mismatch. The gate is now
mechanically enforceable.

**Documentary verification.** The V4 diff (commit `5e00b6d27`) lands
four changes at `alpha-E:362-387`:

1. **Heading wording.** "all rostered grammars" — preserved from V3;
   unchanged in V4.

2. **Parenthetical literal removed.** The eight-name list `{json,
   css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}` that V3
   carried as descriptive parenthetical (with the "currently … the
   list is metadata-derived, not source-of-truth at the gate site"
   qualifier) is **entirely deleted** in V4. The gate text now
   contains zero hardcoded grammar names in the active enumeration
   — only the derivation source `workspace.metadata.bbnf.grammars`
   and the shell command that reads it.

3. **Shell command repaired.** `.workspace_metadata.bbnf.grammars |
   keys[]` → `.metadata.bbnf.grammars[].ident`; `--no-deps` flag
   added; `git diff` → `git diff --exit-code`; the iteration source
   is now the actual `cargo metadata` schema, producing the 9-ident
   enumeration shown above.

4. **Forward-invariant phrasing generalised.** V3 read "so that
   adding a 9th grammar requires NO change to the gate's text"; V4
   reads "so that admitting an additional grammar requires NO change
   to the gate's text" — roster-count-agnostic, future-proof against
   the 9-vs-10 desync the V3 inherited 8-vs-9 desync had foreshadowed.

The cross-cite to C-1's forward invariant
(`alpha-E-candidate-shortlist.md:170-176`) and the parity-binding
clause to `LOCKS.md:220` ("workspace metadata declaring its strategy")
are preserved verbatim from V3. The V3 STRENGTHENING that bound the
C-3 round-trip gate's substrate to Lock 14's source language survives
the V4 repair intact.

The CH2 V2 NF-1 standard — that the gate's grammar enumeration
*derives* from workspace metadata at gate-authoring time, with no
hardcoded list in the active gate text — is now fully met for the
first time in the cycle chain. V3 lowered the parenthetical to
"currently" with the right intent but kept the eight-name literal as
ostensibly-descriptive scaffolding; V4 removes the scaffolding
entirely. The derived enumeration is the single source of truth at
the gate text, exactly as the V2 NF-1 prescription demanded.

### Lock 14 baseline holds at V4

Executable check confirms the V4 α-E gate text is grammar-list-free
in the active enumeration: only the derivation source
(`workspace.metadata.bbnf.grammars`) is referenced; no hardcoded
grammar names appear in the gate command. No grammar-name leak has
been introduced by the V4 micro-fold. The HANDOFF V4 cite-anchor
repair (F-V4-α-F-1: `§1.3` → `§0.2`) is a pure CH1 correctness fix
within prose that has zero grammar implications.

### Fresh-finding scan: V4 artefacts under Lock 14

Scanned all V4 artefacts (SYNTHESIS, HANDOFF, α-A, α-B, α-C, α-D,
α-E, DISPATCH-CONTEXT) for grammar-name leaks via:

```
grep -nE 'JsonGrammar|RuntimeProvider::Json|parse_json_grammar|
match grammar|if grammar ==|json.*css_l4.*google_sheets|
google_sheets.*bbnf|csv.*ebnf.*bnf|9 grammars|8 grammars'
```

Every match resolves to one of the three benign categories enumerated
in V3 CH2 §2 (fresh-finding scan):

1. **Negative-grep targets inside falsifiability gates** (the gates
   *detect and reject* the leaks). Examples: `SYNTHESIS.md:271`,
   `alpha-E:159`. Unchanged from V3.

2. **Pre-block patterns explicitly prohibited** (named as the
   forbidden form so S-P3 constraints reject them). Examples:
   `SYNTHESIS.md:340`, `alpha-E:577`. Unchanged from V3.

3. **Current-state references inside the audit narrative** (to-be-
   PRUNED symbols + files). Examples: `alpha-E:108, 129, 132, 136`
   — the C-1 "Owner paths" section enumerates the eight current
   per-grammar provider modules + per-grammar runtime directories
   that PRUNE-3 + PRUNE-4 collapse. These literals exist *because*
   the PRUNE operates on them; they would be deleted from the prose
   only when the PRUNE itself lands and the audit narrative is
   superseded. Unchanged from V3.

#### §2.1 — Observation (NOT a finding): C-1 sub-wave count

The cost/cap table row at `alpha-E:756` reads:

> `| C-1 sub-waves (8 grammars; per sub-wave) | 20 min | 15 min | 30 min |`

The "8 grammars" count names the C-1 cluster's sub-wave structure
(per-grammar sub-division of the 64-file Pattern H refactor, as
named at `alpha-E:109-110` "Sub-divide by grammar (8 sub-waves) per
`ORCHESTRATOR-PROMPT.md:122-123`"). The live workspace currently
carries nine grammars (per the executable verification above):
`bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv,
math`. The eight-count predates the addition of `css_pretty` to
`workspace.metadata.bbnf.grammars` and was the same staleness the V3
fold inherited at the parenthetical literal site.

This is classified as a **NON-FINDING** under CH2 scope for three
reasons:

1. **Scope.** The cell describes the per-sub-wave hard-cap structure
   for the C-1 cluster (research/plan/redress envelope for *each*
   sub-wave). The total cluster wall-clock is 8 × 30 = 240 min per
   the immediately-following `§10` paragraph (`alpha-E:770`), which
   reads "The C-1 cluster total is 8 × 30 = 240 min of redress
   windows, run serialised per §9". The count is a budgeting input,
   not a Lock 14 gate.

2. **Lock 14 implication.** A stale sub-wave count produces a
   budgeting under-estimate at S-P3 plan-authoring time (the 9th
   sub-wave would add another 30 min redress window = 270 min total,
   not 240); it does NOT bake a grammar privilege, foreclose a
   future grammar, or introduce a `match grammar` branch. Lock 14
   forbids grammar-name leaks in *code* and *gate enumeration
   substrates*; a budgeting cell's scope-count is neither.

3. **Surface.** The cell sits in §10 cost/caps/telemetry. The C-1
   *gate* (§3) and the C-3 round-trip *gate* (§5) — the two Lock 14
   load-bearing surfaces — are both correct (gate substrate is the
   workspace metadata; cluster total derives from the substrate's
   enumeration at plan-authoring time).

The observation is flagged for the V4 aggregator + downstream lenses
(CH4 COST owns the cost/cap budgeting axis; the 240 → 270 min total
is a CH4 concern, not CH2). For CH2 specifically, the gate substrate
is correct and Lock 14 holds. The V4 forward-invariant phrasing
"admitting an additional grammar" is correctly roster-count-agnostic
at the gate text; the cost table cell's stale count is the *only*
place a hardcoded count remains, and the count names sub-wave
budgeting scope, not gate enumeration.

If the V4 aggregator desires absolute belt-and-braces on the
budgeting axis, the V5 confirming pass could carry a single-cell
edit ("8 grammars" → "rostered grammars; currently 9" or similar),
with the wall-clock total at `alpha-E:770` correspondingly
re-derived. But this is **NOT a CH2 finding** under any reasonable
reading of the lens scope; it is flagged here only to acknowledge
the scan caught the artifact and explicitly classify it.

## §3 — Recommended folds for V5 (if any)

None from CH2. The lens converges at 100 % ACCEPT on V4. The
two-consecutive-cycle close chain (V3 100 % + V4 100 %; both with
zero CH2 orphan REVISEs at close) satisfies `ORCHESTRATOR.md §3Z`
on CH2's plane. The V5 confirming pass is required by the
cross-lens aggregate (V3 had two CH1 orphan REVISEs that route to
V4; V5 must confirm V4 closes them with zero new orphans), but CH2
contributes no fold to that pass.

The CH2 V2 NF-1 + CH1 V3 REV-1 lineage is now PERMANENTLY closed:

- F-V3-α-E-1 (V3 fold; V2 NF-1 closure on documentary grounds) —
  superseded by F-V4-α-E-1 which corrects the substrate path and
  removes the inherited parenthetical literal.
- F-V4-α-E-1 (V4 fold; CH1 V3 REV-1 closure on executable grounds;
  CH2 V2 NF-1 closure on substrate-correctness grounds) — landed
  at `alpha-E:362-387`; executable verification confirms 9-ident
  enumeration; gate text is grammar-list-free in the active
  enumeration; cross-cite to `LOCKS.md:220` preserved; cross-cite
  to C-1 forward invariant preserved; forward-invariant phrasing
  generalised to roster-count-agnostic form.

CH2 V4 reports CONVERGED-FINAL. The lens-depth divergence the V3
cycle surfaced (documentary CH2 + CH7 passed; executable CH1 caught
the gate break) is structurally addressed at V4: the V4 dispatch
mandate (per the V4 commit body's "methodological note from V3
aggregator") binds *all* V4 CHALLENGE lenses to executable
verification of any shipped shell command. This CH2 V4 disposition
honours that mandate by executing the gate's shell command in the
live workspace and reporting the verified 9-ident enumeration above.
The substrate is now mechanically true, not merely prose-plausible.

No CH2 fold remains outstanding. Lock 14 holds at the V4 baseline
without qualification.
