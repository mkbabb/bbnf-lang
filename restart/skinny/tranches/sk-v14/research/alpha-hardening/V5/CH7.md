# CH7 Overfit-Prune — Pass Alpha V5 Disposition

Lens binding unchanged: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`.
Five CH7 criteria remain the disposition spine (CH7-1 grammar-derived
only; CH7-2 Lock 14 generic-crate compliance; CH7-3 real source change
+ strict-vs-strict comparator + per-iter equality; CH7-4 round-trip on
generated output; CH7-5 no scaffold admit). V5 is the **§3Z confirming
pass at the V ≤ 5 ceiling** under the strict reading the V4 aggregator
adopted (V4 CONSOLIDATED §0.5). V5 overlay per the V5 dispatch context:
(1) re-execute the E-1 BINDING C-3 round-trip gate's shell command
against the live workspace and confirm the 9-grammar enumeration
including `css_pretty`; (2) verify the V5 micro-fold (commit
`87ee874f0`) extends the roster-count-agnostic pattern from the gate
text (V4 surface) to the cost/cap budgeting cell (V5 surface)
consistently; (3) fresh-finding scan across all five CH7 criteria.

## §0 — Disposition summary

- Artefacts re-reviewed: 1 V5-touched (α-E via F-V5-α-E-1) + 4
  STAND-from-V4 (SYNTHESIS, HANDOFF, α-A, α-C) + 3 STAND-from-V1
  (α-B, α-D, DISPATCH-CONTEXT). Same surface as V2 + V3 + V4.
- Per-section dispositions issued (V5 overlay): **36** (same
  denominator as V2 + V3 + V4).
- ACCEPT: **36**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT-rate: 36 / 36 = **100.0 %**.
- Critical findings: 0.
- Escalation flag: **NO.** The V4 baseline holds verbatim with one
  prescribed improvement on the C-1 cost/cap budgeting surface:
  F-V5-α-E-1 converts the only remaining forward-rooted hardcoded
  grammar-count literal (`8 grammars` at `alpha-E:756`) and its
  derived wall-clock total (`8 × 30 = 240 min` at `alpha-E:770`) to
  roster-count-agnostic phrasings (`per rostered grammar` and
  `N × 30 min where N is the live rostered-grammar enumeration via
  cargo metadata | jq`). Executable verification §1.1 below confirms
  the corrected shell command continues to enumerate 9 grammars
  including `css_pretty`. The CH7 lens converges at **100 %** for the
  fourth consecutive cycle, closing the §3Z two-consecutive-cycle
  chain at the V ≤ 5 ceiling.

## §1 — Executable verification (V5 confirming-pass mandate)

### §1.1 — C-3 round-trip gate's metadata-derived loop re-executed at V5

Per the V5 dispatch context mandate (re-verify the E-1 BINDING C-3
round-trip gate's shell command remains executable post-V5 micro-fold),
the V4-corrected `cargo metadata | jq` form at
`alpha-E-candidate-shortlist.md:366-367` was re-executed against the
live workspace at HEAD = `87ee874f0`.

**Command (verbatim from `alpha-E-candidate-shortlist.md:366-367`):**

```
cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
```

**Output (captured in `/Users/mkbabb/Programming/bbnf-lang`):**

```
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

The command enumerates **9 grammars** verbatim: `bbnf`, `json`,
`css_l4`, `css_pretty`, `google_sheets`, `ebnf`, `bnf`, `csv`, `math`.
Output is byte-identical to the V4 attestation (CH7 V4 §1.1). The
ninth grammar `css_pretty` continues to be enumerated at the gate's
runtime. The V5 micro-fold does NOT touch the gate text at `:362-387`
(verified via `git diff bb715a202..87ee874f0 -- alpha-E-candidate-shortlist.md`:
the V4→V5 cumulative diff shows only the V4-and-prior gate edits at
`:361-378` plus the V5 cost/cap edits at `:756` + `:770-774`; no
co-edit clobbers the gate). The E-1 BINDING gate remains executable
and mechanically enforces per-iteration round-trip discipline on every
rostered grammar.

The V4 strengthening discipline (corrected jq path; `--no-deps`;
`git diff --exit-code` for strict failure propagation) is preserved
verbatim at `:362-378`. CH7-4 (round-trip on generated output) remains
EXECUTABLY-VERIFIED with the silent-pass failure mode eliminated.

### §1.2 — V5 micro-fold extends roster-count-agnostic pattern to budgeting cell

V4 CONSOLIDATED §2.2 prescribed the V5 belt-and-braces fold: edit
`alpha-E:756` from "C-1 sub-waves (8 grammars; per sub-wave)" to
roster-count-agnostic phrasing AND re-derive `:770` wall-clock total
from `8 × 30 = 240` to roster-count-agnostic phrasing. The V5
micro-fold landed at commit `87ee874f0`; the cumulative diff at
`alpha-E-candidate-shortlist.md:753-774` reads (post-V5):

```
| C-1 sub-waves (per rostered grammar; per sub-wave) | 20 min | 15 min | 30 min |
...
The C-1 cluster total is N × 30 min of redress windows where N is the
live rostered-grammar enumeration (`cargo metadata | jq` over the
grammar roster at HEAD), run serialised per §9; the C-4 cluster total
is M × 45 min where M is the number of shapes the wiring exercises
(≥ 2 per E-8's two-grammar-family requirement).
```

Three corrections land in one micro-fold:

1. **Cost-cap cell roster-count-agnostic.** `:756` reads "per
   rostered grammar; per sub-wave" — no grammar-count literal. Budget
   discipline is parametrised over the roster at plan-authoring time,
   matching the gate substrate the V4 cycle institutionalised.
2. **Wall-clock total roster-count-agnostic.** `:770-772` reads
   "N × 30 min … where N is the live rostered-grammar enumeration
   (`cargo metadata | jq` over the grammar roster at HEAD)" — the
   total cites the same metadata-enumeration command the gate at
   `:362-378` uses, binding the cost-axis to the same substrate Lock
   14 names (`LOCKS.md:220` "workspace metadata declaring its
   strategy"). The cost/cap axis now derives from the same authoritative
   source as the gate axis.
3. **Variable rename N → M avoids variable collision.** `:773-774`
   uses `M × 45 min where M is the number of shapes the wiring
   exercises` — the C-4 shape count is distinct from the C-1 grammar
   count and the variable shadowing is resolved. The two formulae
   coexist unambiguously in the paragraph.

The V5 fold extends the V4 cycle's "strengthen beyond literal
prescription" pattern: V4 added `--no-deps` and `--exit-code` beyond
the V3 prescription at the gate site; V5 binds the cost/cap formula
to the same `cargo metadata | jq` substrate beyond the V4 CONSOLIDATED
literal prescription (which permitted either roster-count-agnostic
phrasing OR "currently 9" with re-derived `9 × 30 = 270`; the V5
authoring chose the more durable substrate-bound phrasing). Post-V5
the cost/cap surface is roster-count-agnostic AND substrate-bound to
the same `cargo metadata | jq` clause as the gate.

### §1.3 — Residual grammar-count audit across V5 artefact set

A campaign-wide grep over the V5 artefact set for any remaining
hardcoded grammar-count literal in **forward-rooted** content:

```
grep -rn "8 grammars\|9 grammars\|8th\|9th\|ninth\|eighth" \
  restart/skinny/tranches/sk-v14/research/alpha/ \
  restart/skinny/tranches/sk-v14/SYNTHESIS.md \
  restart/skinny/tranches/sk-v14/HANDOFF.md
```

Returns **zero hits** post-V5. All "9 grammars" / "9th" / "ninth"
language has been excised at the V4 cycle (gate text) and at the V5
cycle (cost/cap budgeting); no forward-rooted text in any V5 artefact
carries a hardcoded grammar count.

Separately, residual `8 per-grammar provider modules` / `8 sub-waves`
references appear at `SYNTHESIS.md:95,271`, `alpha-D.md:488,497,500`,
`alpha-C.md:314,317,446`, `alpha-E.md:105,109`, and
`DISPATCH-CONTEXT.md:165`. These describe the **SK-V13 baseline
historical fact** (the audit-pack-fixed scope of 8 per-grammar provider
modules existing under `codegen/` and 8 per-grammar runtime directories
existing under `crates/core/src/runtime/{grammar}/` at the SK-V13
baseline — see audit pack `crates/core/src/runtime/` enumeration). The
8-count is the PRE-PRUNE-3 measurement that the PRUNE-3 collapse is
DEFINED AGAINST; it is a historical baseline measurement, not a forward
roster count or a gate substrate. The V4 CH2 lens correctly did not
flag these references (CH2 V4 §2.1 explicitly scoped the observation
to the cost/cap budgeting cell, which IS forward-rooted as a future
wall-clock budget). The PRUNE-3 target (collapse the 8 historical
modules → 1 grammar-agnostic template) does not change if a 9th
grammar admits AFTER PRUNE-3; the post-PRUNE roster is consumed by the
template (and by C-1's forward invariant per `alpha-E:170-176`), not
by any literal under PRUNE-3's scope. The 8 references describe a
COMPLETED-AT-SK-V13 measurement; they are CH7-neutral.

CH7-1 (grammar-derived only) holds across the entire V5 surface: every
forward-rooted enumeration substrate-binds to `workspace.metadata.bbnf.grammars`;
every historical reference cites a fixed pre-PRUNE count whose
truth-value is bound to the SK-V13 audit pack, not to the live roster.

## §2 — V4 baseline + V5 micro-fold verification table

Each binding CH7 disposition tracked from its V1 origin through V2
fold-landing through V3/V4 verification through V5 verification.

| CH7 disposition (V1 origin) | V2 fold | V3/V4 status | V5 verification | Status |
|---|---|---|---|---|
| **BINDING REJECT — C-3 round-trip gate CH7-1-blind to Pattern H** (V1/CH7.md §2.1 + §3.1) | E-1 (BINDING) | V3 STRENGTHENED via F-V3-α-E-1 (metadata-derived loop); V4 EXECUTABLE-VERIFIED via F-V4-α-E-1 (jq path corrected; `--no-deps`; `--exit-code`) | **STRENGTHENED + EXECUTABLE-VERIFIED + COST-AXIS-BOUND.** V5 fold F-V5-α-E-1 extends substrate-binding from the gate surface to the cost/cap surface; the same `cargo metadata | jq` clause now drives BOTH the gate's runtime enumeration AND the cost cluster total's parametric formula. §1.1 re-executes the gate command and quotes the 9-grammar output at V5 HEAD. The gate text at `:362-387` is untouched; V4 corrections preserved verbatim. | **FOLD-LANDED + EXECUTABLE-VERIFIED + COST-AXIS-BOUND** |
| REVISE — §3 row C-3 + C-4 compress CH7 surface | F-17 | HOLDS through V4 | **HOLDS.** SYNTHESIS untouched by V5 commit `87ee874f0`; row text unchanged at `SYNTHESIS.md:273-274`. | **FOLD-LANDED** |
| REVISE — α-E §2 shortlist table C-3/C-4 cite | E-14 | HOLDS through V4 | **HOLDS.** α-E §2 lines 85-86 unchanged by V5 cycle (F-V5-α-E-1 touches §10 lines 756 + 770-774 only; §2 untouched). | **FOLD-LANDED** |
| REVISE — §10 cap clarity (per-sub-wave vs per-cluster) | E-2 (CH4 R3 authoritative per V2 CONSOLIDATED §0.5) | HOLDS through V4 | **HOLDS + STRENGTHENED.** Cap table at `:754-760` carries roster-count-agnostic phrasing post-V5; reconciliation paragraph at `:762-774` carries substrate-bound formulae (N × 30 for C-1 derived from live roster; M × 45 for C-4 per CSP-selectable shape; variable rename avoids collision). The CH4 R3 per-sub-wave-vs-per-cluster distinction is preserved and now substrate-bound. | **FOLD-LANDED + ROSTER-AGNOSTIC** |
| REVISE — V2-DISP-α-E-C3-table mirror | E-14 (α-E §2 mirror) + F-17 (SYNTHESIS §3 mirror) | HOLDS through V4 | **HOLDS.** Both mirrors intact post-V5. | **FOLD-LANDED** |
| REVISE — V2-DISP-SYNTHESIS-§3-C3-C4 | F-17 | HOLDS through V4 | **HOLDS.** | **FOLD-LANDED** |
| (informational, V2-deferred) α-A cite spot-check expansion | (CH1 V2 surface) | DEFERRED through V4 | **DEFERRED** (CH1 V5 carries citation surface; no V5 redispatch of α-A required). | **DEFERRED** (acceptable) |

**Fold tally (V5):** 1 BINDING REJECT FOLD-LANDED + EXECUTABLE-VERIFIED
+ COST-AXIS-BOUND; 5 REVISEs FOLD-LANDED (with §10 cap discipline
STRENGTHENED to roster-agnostic post-V5); 1 informational REVISE
deferred per V2 CONSOLIDATED §0.5. Zero FOLD-PARTIAL; zero
FOLD-MISSING; zero REGRESSED.

## §3 — Per-artefact V5 disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| SYNTHESIS.md | §0.1 | ACCEPT | unchanged from V4; CH7-3 close-condition binding holds. |
| SYNTHESIS.md | §0.2 | ACCEPT | unchanged from V4; reconciliation paragraph at `:200-209` lifts CH6 REJ-2 + CH7's audit-overlay integrity. |
| SYNTHESIS.md | §0.3 | ACCEPT | unchanged from V4; R4 row reads "first instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator"; CH7-4 round-trip + CH7-2 grammar-neutrality preserved. |
| SYNTHESIS.md | §0.4 | ACCEPT | unchanged from V4; P-1 W10.3 nested_layout round-trip-rule trigger + ≥ 50× SOTA-comparator threshold; CH7-1 + CH7-4 + CH3 cross-binding. |
| SYNTHESIS.md | §0.5 | ACCEPT | unchanged; contracted S-P3 deferral. |
| SYNTHESIS.md | §1.1 | ACCEPT | unchanged. |
| SYNTHESIS.md | §1.2 | ACCEPT | unchanged from V4; 4+7 → 6+11 reconciliation block at lines 200-209 holds. |
| SYNTHESIS.md | §1.3 | ACCEPT | unchanged; rolling delta restated; audit-zero baseline holds. |
| SYNTHESIS.md | §2 | ACCEPT | unchanged from V4; telemetry schema includes `track2_entry_point`; CH7's CH5 cross-binding mechanically enforced. |
| SYNTHESIS.md | §3 | ACCEPT | unchanged from V4; candidate table at `:273` lifts dual-tree round-trip + bypass-header detector + §5 pointer per F-17. C-4 row at `:274` names `json/numbers/direct_to_struct/main` + per-shape Lock-1 triad. CH7-1, CH7-2, CH7-4, CH7-5 intact. V5 commit does not touch SYNTHESIS. The "8 sub-waves" descriptor at `:271` and "8 per-grammar provider modules" at `:271` describe SK-V13 baseline historical scope (audit-pack-fixed), not forward roster — CH7-neutral per §1.3 audit. |
| SYNTHESIS.md | §4 | ACCEPT | unchanged from V4; S-P3 constraints carry per-wave LOC ceiling (F-6), C-1 forward invariant (F-12), C-4 two-grammar-family exercise + no-grammar-branch dispatch (F-13), G-SIMD-GRAMMAR-POLICY triad (F-14), triumvirate discipline (F-9). |
| SYNTHESIS.md | §5 | ACCEPT | unchanged. |
| SYNTHESIS.md | §6 | ACCEPT | unchanged. |
| HANDOFF.md | §1 | ACCEPT | unchanged. |
| HANDOFF.md | §2 | ACCEPT | unchanged. |
| HANDOFF.md | §3 | ACCEPT | unchanged from V4; numeric reconciliation holds per F-1; CH7-3 measurement honesty inherits. |
| HANDOFF.md | §4 | ACCEPT | unchanged from V4; α-F sole-author posture intact per F-2. |
| HANDOFF.md | §5 | ACCEPT | unchanged; CH7 lens binding cited at step 4. |
| HANDOFF.md | §6 | ACCEPT | unchanged from V4; next-move chain echoes hard caps (F-7) + restores G-Omega (F-8); cap paragraph cites "30-min lens-agent cap; research 20 min / plan 15 min / redress 30 min (45 min only for the addendum-amended decision-engine fold + C-4 per CONSOLIDATED §0.5 cap discipline)". |
| HANDOFF.md | §7 | ACCEPT | unchanged from V4; refusal list intact at CH7-relevant bullets (W10.3 round-trip-rule trigger per F-10; UnionTape verbatim refusal per F-16; P-1..P-7 fold inheritance). The V4 F-V4-α-F-1 reconciliation cite correction holds. CH7-1 + CH7-4 + CH7-5 lens posture unchanged. |
| HANDOFF.md | §8 | ACCEPT | unchanged. |
| α-A §1 parse_only | ACCEPT | per-row audit-overlay citations unchanged from V4; CH7-3 carry. |
| α-A §2 direct | ACCEPT | reconciliation table at `alpha-A-results-extraction.md:125-130` per A-1 (V2) holds; CH7-3 measurement integrity intact. |
| α-A §3 typed | ACCEPT | +4 extension rows per A-2 (V2) carry the `[ext†]` annotation; CH7-3 audit-overlay integrity holds. |
| α-A §4 CSS L4 | ACCEPT | unchanged; CH7-1 + CH7-4 audit cite intact. |
| α-A §5 c/B telemetry | ACCEPT | per A-3 (V2) c/B telemetry LOC budget assigned via C-2 envelope; CH7-3 schema-debt closure. |
| α-A §6 | ACCEPT | unchanged. |
| α-B (entire) | ACCEPT | STAND from V1 + V2 + V3 + V4; zero changes in V5 cycle. |
| α-C §1 | ACCEPT | unchanged. |
| α-C §2 | ACCEPT | per C-1 (V2) P-7 triple-check gate at `alpha-C-redress-digest.md:348-385` holds; CH7-3 cross-binding with CH5 intact. The "8 per-grammar provider modules" reference at `:314,317,446` describes SK-V13 baseline historical scope — CH7-neutral per §1.3 audit. |
| α-D (entire) | ACCEPT | STAND from V1 + V2 + V3 + V4; zero changes. The "8 per-grammar" recurrence-vector / reopen-path references at `:488,497,500` describe SK-V13 baseline historical scope — CH7-neutral per §1.3 audit. |
| α-E §1 | ACCEPT | unchanged. |
| α-E §2 shortlist table | ACCEPT | per E-14 (V2): C-3 + C-4 rows carry explicit gates with §5 + §6 pointers; CH7-4 + CH7-5 binding holds; no V5 edit. |
| α-E §3 C-1 | ACCEPT | per E-7 (V2) C-1 forward invariant at `alpha-E-candidate-shortlist.md:170-176` holds; per E-11 LOC lower bound 2.8k holds; per E-13 §9 strict serialisation holds. CH7-1 + CH7-2 reinforced. F-V4-α-E-1 preserves the C-1 parity citation at `:375`; F-V5-α-E-1 does not touch §3. The "Collapse 8 per-grammar provider modules" reference at `:105,109` describes SK-V13 baseline historical scope — CH7-neutral. |
| α-E §4 C-2 | ACCEPT | per E-12 (V2) LOC envelope +80 for Skipper fallback holds; CH7-3 plane-correct comparators unchanged. |
| α-E §5 C-3 | ACCEPT | **per E-1 BINDING (V2) + F-V3-α-E-1 (V3) STRENGTHENING + F-V4-α-E-1 (V4) EXECUTABLE-VERIFICATION + V5 PRESERVATION:** three-part round-trip + bypass-header detector verbatim at `alpha-E-candidate-shortlist.md:355-398`. V5 commit `87ee874f0` does NOT touch the gate text; the V4 jq path correction (`.metadata.bbnf.grammars[].ident`), `--no-deps`, `--exit-code`, and roster-count-agnostic phrasing (`admitting an additional grammar`) all preserved byte-equivalent. §1.1 above re-executes the gate command at V5 HEAD and quotes the 9-grammar output. CH7-1 (grammar-derived), CH7-2 (Lock 14), CH7-4 (round-trip on generated output) all STRICTLY PRESERVED. |
| α-E §6 C-4 | ACCEPT | per E-3 (V2) per-shape Lock-1 triad at `:474-491`; per E-4 module-path discipline at `:514-524`; per E-5 pre-wave hot-leaf citation at `:531-538`; per E-8 no grammar-branched dispatch + two-grammar exercise. CH7-5 wired with multi-layer falsification surface. No V5 edit. |
| α-E §7 C-5 | ACCEPT | per E-10 (V2) scribe contract "29 row-keyed REDRESS entries" verbatim; CH7-1 audit-trail restoration unchanged. |
| α-E §8 | ACCEPT | unchanged. |
| α-E §9 | ACCEPT | per E-13 (V2) §9 vs §6 dependency-matrix resolved at `:730-741`; CH7-1 audit-trail discipline preserved. |
| α-E §10 | ACCEPT | **per E-2 + V2 CONSOLIDATED §0.5 + F-V5-α-E-1 (V5) ROSTER-AGNOSTIC FOLD:** caps at `:754-760` read C-1/C-2/C-3/C-5 = 30 min, C-4 = 45 min (per CSP-selectable shape). V5 fold converts the C-1 sub-wave cell from "8 grammars; per sub-wave" to "per rostered grammar; per sub-wave" and re-derives `:770-774` cluster total to "N × 30 min … where N is the live rostered-grammar enumeration (`cargo metadata | jq` over the grammar roster at HEAD)"; the C-4 cluster total uses M (per CSP-selectable shape; rename avoids variable collision). The cost/cap surface is now both ROSTER-COUNT-AGNOSTIC and SUBSTRATE-BOUND to the same `cargo metadata | jq` clause that drives the C-3 round-trip gate. CH7 cap discipline matches CH4 R3 + extends to substrate-bound parametric budgeting. CH7-1 (grammar-derived) is STRICTLY STRENGTHENED on the cost axis. |
| α-E §11 | ACCEPT | unchanged. |
| DISPATCH-CONTEXT.md | (full) | ACCEPT | STAND from V1 + V2 + V3 + V4; zero changes in V5 cycle. The "8 per-grammar provider modules" reference at `:165` describes SK-V13 baseline historical scope — CH7-neutral per §1.3 audit. |

Total: **36 ACCEPT / 0 REVISE / 0 REJECT.**

## §4 — Critical findings

### §4.1 — F-V5-α-E-1 binds cost/cap surface to gate's substrate; total roster-agnostic posture achieved

The V5 micro-fold F-V5-α-E-1 (commit `87ee874f0`) extends the V4
cycle's roster-count-agnostic discipline from the gate text (CH7-1 +
CH7-4 surface) to the cost/cap budgeting surface (CH4 R3 + CH7-1
surface). The fold's diff at `alpha-E:753-774`:

- **`:756` cell text.** "C-1 sub-waves (8 grammars; per sub-wave)" →
  "C-1 sub-waves (per rostered grammar; per sub-wave)". No literal
  count; parametric over the roster.
- **`:770-774` cluster-total derivation.** "The C-1 cluster total is
  8 × 30 = 240 min" → "The C-1 cluster total is N × 30 min … where N
  is the live rostered-grammar enumeration (`cargo metadata | jq` over
  the grammar roster at HEAD)". The wall-clock budget is now bound to
  the SAME `cargo metadata | jq` substrate the C-3 round-trip gate
  uses for its per-iteration enumeration. The cost-axis and the
  gate-axis derive from the SAME authoritative source.
- **Variable rename N → M for C-4.** "the C-4 cluster total is N × 45
  min where N is the number of shapes" → "the C-4 cluster total is
  M × 45 min where M is the number of shapes". The C-4 shape count is
  distinct from the C-1 grammar count and the rename resolves the
  variable shadowing.

The V5 fold extends the V4 cycle's "strengthen beyond literal
prescription" pattern. V4 CONSOLIDATED §2.2 permitted two alternatives:
(a) `(rostered grammars; currently 9; per sub-wave)` with re-derived
`9 × 30 = 270` total; or (b) `(per sub-wave; one per rostered grammar)`
roster-count-agnostic phrasing. The V5 author chose neither verbatim
— instead authoring (b)-equivalent phrasing for the cell AND extending
the cost-total formula to substrate-bind directly to the `cargo
metadata | jq` clause (a strengthening beyond either literal V4
prescription option). The result is the most durable possible
phrasing: no count to re-derive on roster admission; the formula
itself names the live enumeration substrate.

Post-V5, **the only remaining hardcoded grammar-count literal in any
forward-rooted V5 artefact is none** (verified via §1.3 grep). The
SK-V13 baseline historical references (8 per-grammar provider modules;
8 sub-waves at the PRUNE-3 site) are CH7-neutral per §1.3 audit —
they measure a fixed pre-PRUNE artefact count whose truth-value is
bound to the SK-V13 audit pack, not to the live forward roster.

The CH7-1 (grammar-derived) binding is STRICTLY STRENGTHENED on the
cost axis; the cost/cap surface now joins the gate substrate as a
substrate-bound parametric quantity, not a literal.

### §4.2 — E-1 gate's V4 corrections preserved verbatim through V5

The V5 micro-fold's scope is strictly the cost/cap budgeting cells at
`alpha-E:756 + :770-774`; the V4 cycle's gate corrections at
`:362-378` (jq path `.metadata.bbnf.grammars[].ident`; `--no-deps`;
`git diff --exit-code`; roster-count-agnostic "admitting an additional
grammar") are untouched. §1.1 re-executes the gate command at V5 HEAD
and quotes the byte-identical 9-grammar output the V4 verification
established. The gate text at `:362-387` is BYTE-IDENTICAL to V4
HEAD; no V5 co-edit clobbers the V4 strengthening. The E-1 BINDING
REJECT remediation chain (V1 REJECT → V2 FOLD-LANDED → V3 STRENGTHENED
→ V4 EXECUTABLE-VERIFIED → V5 PRESERVED + COST-AXIS-BOUND) holds
verbatim.

### §4.3 — P-1..P-7 ↔ CH7-N mapping holds through V5

The V2 §2.2 bijective mapping (P-1↔CH7-1; P-2/P-3/P-4↔CH7-3;
P-5↔CH7-5; P-6↔CH7-2; P-7 cross-bind to CH5) persists unchanged in V5
at `SYNTHESIS.md:104-148`. The W10.3 round-trip-rule trigger added to
P-1 in V2 (per F-10) at `SYNTHESIS.md:113-120` carries through;
HANDOFF §7 carries the matching refusal bullet. V5 micro-fold does
not touch SYNTHESIS or HANDOFF; the pattern pre-block ↔ CH7-N mapping
holds verbatim.

### §4.4 — SK-V13 baseline "8 per-grammar" historical references are CH7-neutral

The §1.3 grep surfaces multiple residual `8 per-grammar provider
modules` / `8 sub-waves` references at `SYNTHESIS.md:95,271`,
`alpha-D.md:488,497,500`, `alpha-C.md:314,317,446`, `alpha-E.md:105,109`,
and `DISPATCH-CONTEXT.md:165`. These describe the audit-pack-fixed
SK-V13 baseline scope: 8 per-grammar provider modules existing under
`codegen/` AND 8 per-grammar runtime directories existing under
`crates/core/src/runtime/{grammar}/` at the SK-V13 baseline. The
PRUNE-3 / PRUNE-4 refactor is defined AGAINST this measurement (collapse
8 → 1 grammar-agnostic template; refactor 64 hand-written files → emitted
output across 8 sub-waves bound to the 8 historical grammars).

These references are CH7-neutral because:

1. **The historical fact is fixed.** The 8-count at SK-V13 baseline
   is an audit-pack measurement; admitting a 9th grammar AFTER PRUNE-3
   does not change the SK-V13 baseline count of 8 historical modules.
2. **The PRUNE target is the historical count.** The refactor target
   (collapse 8 hand-written modules → 1 template) is defined against
   the SK-V13 audit-pack scope; the template's downstream consumers
   are roster-agnostic per C-1's forward invariant at `alpha-E:170-176`.
3. **No forward-rooted enumeration is hardcoded.** Every forward-
   rooted enumeration (the C-3 gate at `:362-378`; the C-1 forward
   invariant at `:170-176`; the V5 cost/cap derivation at `:770-774`)
   substrate-binds to `workspace.metadata.bbnf.grammars`.
4. **V4 CH2 explicitly scoped the observation.** CH2 V4 §2.1 limited
   the observation to the cost/cap cell at `alpha-E:756 + :770`;
   neither CH2 nor any other lens flagged the historical baseline
   references at any cycle. The V4 aggregator (V4 CONSOLIDATED §0.2)
   adjudicated the cost/cap cell as the only forward-rooted residual.

CH7-1 (grammar-derived) holds verbatim across the entire V5 surface;
the historical references measure a fixed audit-pack fact and do not
introduce roster-count overfit.

## §5 — Fresh-finding scan (V5-cycle defect surface)

Per the V5 dispatch context, scan all V5 artefacts for any new CH7-N
criterion violation introduced by the V5 micro-fold cycle. Scan
dimensions:

- **New fake `@generated` instances introduced by V5.** None. The V5
  cycle edits one file (α-E) at one paragraph (`:753-774`). No
  `@generated` reference touched; the bypass-header detector clauses at
  `:389,395,397` are untouched.
- **New scaffold-as-load-bearing claims.** None. C-4 (the only
  candidate touching W8 / W9 SCAFFOLD-ONLY surface) is untouched by
  the V5 cycle; all C-4 falsifiers added in V2 (E-3 Lock-1 triad, E-4
  module-path discipline, E-5 pre-wave citation, E-8
  two-grammar-family exercise) remain intact. The C-4 cluster total
  formula receives only a variable rename (N → M) for collision
  avoidance; the substantive C-4 binding is untouched.
- **New gate-relabel risk.** None. C-2's per-iter equality oracle
  remains the comparator integrity gate; no V5 fold touches the
  comparator surface.
- **New Lock 14 generic-crate leaks.** None. F-V5-α-E-1 STRENGTHENS
  the Lock 14 posture on the cost axis: the cost/cap derivation now
  substrate-binds to the same `workspace.metadata.bbnf.grammars`
  clause Lock 14 names (`LOCKS.md:220`), extending the V4 cycle's gate
  substrate-binding to the cost axis. The V5 cycle reduces, not
  increases, the Lock 14 leak surface.
- **New round-trip scope gaps.** None. The C-3 round-trip + bypass-
  header detector gate at `:355-398` is byte-identical to V4 HEAD
  (verified via `git diff bb715a202..87ee874f0 -- alpha-E-candidate-shortlist.md`:
  V5 diff scope is `:753-774` only; the gate at `:355-398` is
  untouched). CH7-4 is STRICTLY PRESERVED with the V4 strengthening
  intact.
- **Cross-lens conflict.** None. F-V5-α-E-1 (CH2/CH4/CH7 scope —
  cost/cap cell roster-agnostic; gate substrate extended to cost axis)
  touches non-overlapping CH7 criteria (CH7-1 strengthened on cost
  axis; CH7-2 strengthened on cost axis; CH7-4 untouched at the gate
  text). The CH4 R3 cap discipline (per CONSOLIDATED §0.5) is
  preserved and now substrate-bound; the addendum cap distinctions
  (45-min for C-4 per CSP-selectable shape; 30-min for non-decision-
  engine waves) are unchanged. No cross-lens conflict surfaces.

Zero new findings across all six scan dimensions.

## §6 — Recommended folds for V6 (if any)

**None — and no V6 is expected to fire.** V5 has verified the V4
100 % CH7 baseline holds intact, and the V5 micro-fold F-V5-α-E-1
STRENGTHENS the cost-axis posture by extending the V4 gate-substrate
binding to the cost/cap derivation; the fresh-finding scan returns
zero new findings.

Per `ORCHESTRATOR.md §3Z`, the multi-consecutive-cycle convergence
rule is now satisfied for the CH7 lens at the V ≤ 5 ceiling: V2 =
100 % (link 1), V3 = 100 % (link 2), V4 = 100 % (link 3), V5 = 100 %
(link 4). The CH7 lens-local convergence chain holds through V5 across
four consecutive cycles; the §3Z chain CLOSES at the V ≤ 5 ceiling
with zero CH7 fold pressure into any putative V6. Per the V4
CONSOLIDATED §0.5 strict-reading verdict (V5 confirming pass at V ≤ 5
ceiling), the SK-V14 Pass Alpha bracket LOCKS at the V5 cycle close.

## §7 — Bracket-level CH7 verdict

CH7 V5 converges at **100 %** for the lens, extending the consecutive-
cycle convergence chain to four cycles (V2 + V3 + V4 + V5 all at
100 %) and CLOSING the §3Z two-consecutive-cycle chain at the V ≤ 5
ceiling under the V4 aggregator's strict reading. The V1 BINDING
REJECT remediation landed verbatim in V2, held in V3 with substantive
strengthening at the gate's grammar-enumeration site, at V4 strictly
strengthened to executable-verified status, and at V5 holds verbatim
with the cost-axis surface now substrate-bound to the same `cargo
metadata | jq` clause. The 5 V1 REVISEs landed in V2, hold in V5 with
no regression, and §10 cap discipline post-V5 is strengthened to
roster-count-agnostic + substrate-bound. The V2/V3/V4/V5 fresh-finding
scans each returned zero CH7 findings.

The CH7 surface is fully closed across all five criteria, with the
cost/cap surface joining the gate substrate as a roster-count-agnostic
substrate-bound quantity post-V5:

- **CH7-1** (grammar-derived only): C-1 forward invariant + C-3
  bypass-header detector + C-3 executably-derived gate enumeration (V4
  jq-path correction; V5 preserved) + C-5 deletion ledger + **C-1
  cost/cap derivation substrate-bound (V5)**.
- **CH7-2** (Lock 14 generic compliance): C-1 trait-dispatch +
  grammar-agnostic generator + C-4 no-grammar-branched dispatch +
  C-3 gate-text grammar-derivation via live `cargo metadata` (V4) +
  **C-1 cost-axis substrate-binding to `workspace.metadata.bbnf.grammars`
  (V5)**.
- **CH7-3** (real source + strict comparator + per-iter equality):
  C-2 three plane-correct strict comparators + per-iter equality
  column; audit-overlay column at SYNTHESIS §2. Unchanged through V5.
- **CH7-4** (round-trip on generated output): C-3 three-part
  round-trip + bypass-header detector covering both runtime trees +
  all rostered grammars under workspace metadata enumeration +
  `git diff --exit-code` strict failure propagation (V4) + V5
  byte-identical preservation; the V3 silent-pass failure mode remains
  eliminated.
- **CH7-5** (no scaffold admit): C-4 hot-leaf attribution change +
  per-shape Lock-1 triad + module-path discipline + two-grammar
  exercise. Unchanged through V5 (V5 fold's N → M rename in the C-4
  cluster total formula is variable-collision avoidance only; the
  substantive C-4 binding is untouched).

The lens cleared a fourth consecutive cycle and closes the §3Z chain
at the V ≤ 5 ceiling. The aggregator should mark the CH7 lens at the
V5 verdict as "CONVERGED — four-cycle chain with executable-verification
of the binding gate AND cost-axis substrate-binding"; CH7 carries no
fold into any putative V6 and no further CH7 work is required for the
SK-V14 Pass Alpha bracket contract. The SK-V14 Pass Alpha bracket
LOCKS at V5 from the CH7 axis; G-Alpha may auto-sign per the SK-V14
ORCHESTRATOR-PROMPT pin; orchestrator proceeds directly to S-P0.

**E-1 landing status: FOLD-LANDED + V3-STRENGTHENED + V4-EXECUTABLE-VERIFIED + V5-PRESERVED-COST-AXIS-BOUND.**
**F-V5-α-E-1 landing status: VERIFIED-EXTENDS-ROSTER-AGNOSTIC-PATTERN-TO-COST-AXIS + STRENGTHENS-CH7-1-COST-AXIS + STRENGTHENS-CH7-2-COST-AXIS.**
**§3Z chain status: CLOSED at V ≤ 5 ceiling (V4 link 1 + V5 link 2; both clean of orphans).**
