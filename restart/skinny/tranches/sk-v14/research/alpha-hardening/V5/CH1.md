# CH1 CORRECTNESS — Pass Alpha V5 Disposition

Lens: every claim cites file:line, commit SHA, RESULTS row, or REDRESS
entry that resolves. Falsifiability gates are measurable. Comparator
deltas match the strictness plane. Audit-overlay verdicts cite the
correct validation §reference. R-target acceptance criteria are
empirically verifiable.

V5 confirming-pass overlay per `V2/CHALLENGE-V2-ADDENDUM.md §1` (cycle
methodology unchanged): (1) verify the V5 micro-fold landed the V4
CH2 non-finding remediation — F-V5-α-E-1 (cost/cap table roster-count
delamination at α-E:756 + :770); (2) verify the V4 100 % baseline
holds (no regression of any prior CH1 anchor including A-1, A-2, A-3,
E-14, F-1, F-2, F-V4-α-E-1, F-V4-α-F-1); (3) fresh-finding scan over
the V5 delta surface and the carried surfaces.

## §0 — Disposition summary

- Sections audited: 53 (7 SYNTHESIS, 8 HANDOFF, 7 α-A, 8 α-B, 5 α-C,
  6 α-D, 12 α-E). Population preserved verbatim from V1 / V2 / V3 / V4.
- V4 baseline holds: **YES**. Every V4 ACCEPT-V3-CARRIED re-verifies
  at the named `path:line` anchors; HANDOFF §7 citation still
  resolves at `SYNTHESIS.md:73-84`; F-V4-α-E-1 jq path
  (`.metadata.bbnf.grammars[].ident` with `--no-deps`) re-runs
  against live workspace and enumerates the same 9 grammars; the
  `--exit-code` strengthening on `git diff` carries; the
  count-bake-excision in α-E:362-387 still holds (the line range now
  reads "metadata-derived, not source-of-truth at the gate site"
  without any 8-name parenthetical).
- V5 micro-fold disposition:
  - **F-V5-α-E-1 → FOLD-LANDED** (verbatim per V4 CONSOLIDATED §2.3
    prescription; `α-E:756 + :770`; commit `87ee874f0`).
- **EXECUTABLE RE-VERIFICATION DISCHARGED:** the F-V4-α-E-1 jq
  command was re-run against the live workspace from
  `/Users/mkbabb/Programming/bbnf-lang` under V5 and still enumerates
  **9 grammars** byte-equivalent to `Cargo.toml:18-29` — `bbnf`,
  `json`, `css_l4`, `css_pretty`, `google_sheets`, `ebnf`, `bnf`,
  `csv`, `math`. The V5 fold's "N × 30 min where N = live
  rostered-grammar enumeration" phrasing is therefore live-anchored,
  not documentary.
- Fresh V5 findings (REJECT or REVISE the V4 lens missed): **0** in
  CH1 scope (one CH2-scope observation logged in §2 as advisory).
- V5 ACCEPT-rate (per-§): 53 / 53 = **100.00 %** (≥95 % converges
  per `ORCHESTRATOR.md §3Z`).
- Critical findings: 0 REJECT.
- Escalation flag: **NONE**.

CH1 V5 converges at 100 %. Combined with CH1 V4 at 100 %, CH1 has
satisfied the two-consecutive-cycle convergence ceiling per
`ORCHESTRATOR.md §3Z` (100 % / 100 % at the V≤5 ceiling). The V5
belt-and-braces fold is a clean roster-count-agnostic phrasing
change at a single surface; no citation regression, no anchor drift,
no introduced ambiguity. The V5 redress writes the same
`workspace.metadata.bbnf.grammars`-derived idiom into the cap-discipline
prose that the V4 round-trip-gate prose at α-E:362-387 already uses,
restoring intra-document idiom parity — the cost-table and the
round-trip-gate now both enumerate from the same metadata clause.

## §1 — Per-artefact disposition table (V5)

### SYNTHESIS.md (407 lines, unchanged V2 → V3 → V4 → V5)

| § | Disposition | Reason |
|---|---|---|
| §0.1 close condition R10 verbatim | ACCEPT-V4-CARRIED | No V5 fold touches. |
| §0.2 goalset enumeration (75-row population) | ACCEPT-V4-CARRIED | F-1 reconciliation block at lines 73-84 holds verbatim under the V5 carry; HANDOFF §7 anchor still resolves here. |
| §0.3 R-target table (R1–R10 acceptance) | ACCEPT-V4-CARRIED | Empirically-verifiable gates preserved. |
| §0.4 pre-blocks P-1..P-7 | ACCEPT-V4-CARRIED | Validation-pack §refs intact. |
| §0.5 wave-by-wave gate deferral | ACCEPT-V4-CARRIED | Contracted per PASS-ALPHA §4.4. |
| §1 corrected diagnosis | ACCEPT-V4-CARRIED | Per-pillar citation table + §1.2 :200-209 reconciliation block intact. |
| §2 telemetry binding | ACCEPT-V4-CARRIED | Extended-schema columns hold. |
| §3 candidate shortlist | ACCEPT-V4-CARRIED | C-3 + C-4 gate rows still cite both runtime trees + bypass-header detector + pre-wave row + Lock-1 triad. |
| §4 S-P3 constraints | ACCEPT-V4-CARRIED | 11 constraints bind to specific gates. |
| §5 pre-blocked / unblocked routes | ACCEPT-V4-CARRIED | Citations resolve. |
| §6 close posture | ACCEPT-V4-CARRIED | Standalone prose. |

### HANDOFF.md (245 lines, unchanged V4 → V5)

| § | Disposition | Reason |
|---|---|---|
| §1 bracket verdict | ACCEPT-V4-CARRIED | 0/43 verdict preserved. |
| §2 authority list | ACCEPT-V4-CARRIED | 14-item ordered read intact. |
| §3 honest baseline summary | ACCEPT-V4-CARRIED | F-1 carry preserved; numeric ledger holds. |
| §4 pre-S-P0 readiness | ACCEPT-V4-CARRIED | Git history resolves. |
| §5 pass sequence (10 steps) | ACCEPT-V4-CARRIED | Each step cites binding doc. |
| §6 next-move chain | ACCEPT-V4-CARRIED | Sign-off gates aligned. |
| §7 refusal conditions | ACCEPT-V4-CARRIED | F-V4-α-F-1 anchor at `:195-196` (`SYNTHESIS.md §0.2` + `(lines 73-84)`) still resolves verbatim. |
| §8 V1 disposition | ACCEPT-V4-CARRIED | Honest pending state. |

### α-A — Results extraction (420 lines, unchanged V2 → V5)

| § | Disposition | Reason |
|---|---|---|
| §0 preamble + conventions | ACCEPT-V4-CARRIED | Line refs match wc -l. |
| §1 parse_only table (17 rows) | ACCEPT-V4-CARRIED | Per-row audit overlay intact. |
| §2 direct_to_struct table | ACCEPT-V4-CARRIED | A-1 reconciliation table at lines 125-134 holds; PRUNE-1 6-row count binding intact. |
| §3 real_typed_struct table | ACCEPT-V4-CARRIED | A-2 [ext†] marks + wave-id legend table at lines 184-203 hold. |
| §4 CSS L4 table (24 rows) | ACCEPT-V4-CARRIED | Per-row audit citations intact. |
| §5 c/B + telemetry | ACCEPT-V4-CARRIED | A-3 LOC budget decomposition at lines 296-319 holds. |
| §6 audit verdict summary | ACCEPT-V4-CARRIED | 0/75 audit-zero bind. |
| §7 forward pointers | ACCEPT-V4-CARRIED | Downstream consumers correctly named. |

### α-B — Competitor deltas (328 lines, STANDS unchanged from V1)

All sections ACCEPT-V1-CARRIED via V2/V3/V4/V5 carry. No fold touches α-B.

### α-C — REDRESS digest (460 lines, unchanged V2 → V5)

All sections ACCEPT-V4-CARRIED. No V5 fold touches α-C.

### α-D — Validated / invalidated / demoted / still-open (545 lines, STANDS)

All sections ACCEPT-V1-CARRIED via carry. No fold touches α-D.

### α-E — Candidate shortlist (816 lines; F-V5-α-E-1 micro-fold landed; net +1-line edit, 6 insertions / 5 deletions)

| § | Disposition | Reason |
|---|---|---|
| §0 authority + binding posture | ACCEPT-V4-CARRIED | Citations resolve. |
| §1 why prune-first | ACCEPT-V4-CARRIED | Reasoning sound. |
| §2 shortlist table | ACCEPT-V4-CARRIED | C-3 row + C-4 row gates carry. |
| §3 C-1 Lock-14 refactor | ACCEPT-V4-CARRIED | Owner paths intact; line 170-176 forward-invariant clause unchanged. See §2 advisory below for a CH2-scope observation. |
| §4 C-2 comparator rebind | ACCEPT-V4-CARRIED | Skipper fallback preserved. |
| §5 C-3 regen-css + corpora | ACCEPT-V4-CARRIED | F-V4-α-E-1 anchor at `:362-387` re-verifies: jq path `.metadata.bbnf.grammars[].ident`, `--no-deps`, `--exit-code`, generalised "additional grammar" phrasing all hold; live-workspace re-run enumerates 9 grammars. |
| §6 C-4 W8+W9 wiring | ACCEPT-V4-CARRIED | E-14 / REVISE-3 pre-wave row binding at lines 531-538 holds. |
| §7 C-5 clean revert | ACCEPT-V4-CARRIED | 29 REDRESS entry count intact. |
| §8 consolidated pre-blocks | ACCEPT-V4-CARRIED | α-C P-1..P-7 carried verbatim. |
| §9 concurrency + serialisation | ACCEPT-V4-CARRIED | Wave-Zero matrix preserved. |
| §10 cost + caps + telemetry | **ACCEPT (V5 fold landed)** | F-V5-α-E-1 micro-fold landed verbatim at `α-E:756 + :770-774`: (a) line 756 row label changed from `C-1 sub-waves (8 grammars; per sub-wave)` to `C-1 sub-waves (per rostered grammar; per sub-wave)` — count-bake excised; (b) lines 770-774 wall-clock prose changed from `8 × 30 = 240 min of redress windows` to `N × 30 min of redress windows where N is the live rostered-grammar enumeration (\`cargo metadata \| jq\` over the grammar roster at HEAD)`; (c) the C-4 cluster total uses `M × 45 min where M = exercised shapes` (N→M rename avoids variable collision with the C-1 N). The phrasing is now isomorphic to the F-V4-α-E-1 round-trip-gate idiom at `α-E:362-387` — both surfaces enumerate from the same `workspace.metadata.bbnf.grammars` clause. EXECUTABLE-RE-VERIFIED: the jq enumeration still resolves to 9 grammars against the live workspace; the V5 phrasing inherits its truth from that live enumeration rather than from a documentary integer. |
| §11 convergence + escalation | ACCEPT-V4-CARRIED | Four escalation paths intact. |

### DISPATCH-CONTEXT.md (206 lines, STANDS unchanged from V1)

Out of V2 / V3 / V4 / V5 fold scope. All V1 dispositions carry.

## §2 — Critical findings (V5)

No REJECT-class findings. No REVISE-class findings within the CH1
lens scope. The V5 fold landed cleanly under executable re-verification.

### FOLD-LANDED-AND-EXECUTABLY-VERIFIED — F-V5-α-E-1 (V4 CH2 non-finding closure)

**V4 prescription source:** V4 CONSOLIDATED §2.3 (commit `1bc9380b8`)
flagged the residual `8 grammars` parenthetical at `α-E:756` and the
`8 × 30 = 240 min` cluster total at `α-E:770` as the last
count-baked surface after F-V4-α-E-1 excised the parallel pattern at
`α-E:362-387`.

**Fold landed at:** `restart/skinny/tranches/sk-v14/research/alpha/
alpha-E-candidate-shortlist.md:756 + :770-774` (commit `87ee874f0`).

**Diff vs V4 (commit `87ee874f0`):**

```
-| C-1 sub-waves (8 grammars; per sub-wave) | 20 min | 15 min | 30 min |
+| C-1 sub-waves (per rostered grammar; per sub-wave) | 20 min | 15 min | 30 min |
```

```
-cluster total is 8 × 30 = 240 min of redress windows, run serialised
-per §9; the C-4 cluster total is N × 45 min where N is the number of
-shapes the wiring exercises (≥ 2 per E-8's two-grammar-family
-requirement).
+cluster total is N × 30 min of redress windows where N is the live
+rostered-grammar enumeration (`cargo metadata | jq` over the grammar
+roster at HEAD), run serialised per §9; the C-4 cluster total is
+M × 45 min where M is the number of shapes the wiring exercises
+(≥ 2 per E-8's two-grammar-family requirement).
```

**Executable re-verification (run in `/Users/mkbabb/Programming/bbnf-lang`):**

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

$ cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident' | wc -l
9
```

The live roster has drifted from 8 (the V4 cost-table integer) to 9
(`css_pretty` now under `[workspace.metadata.bbnf]` at
`Cargo.toml:23`); F-V5-α-E-1's roster-count-agnostic phrasing means
the cost-table prose continues to bind correctly without further
edit. The C-1 cluster total resolves to `9 × 30 = 270 min` under
live enumeration; future grammar admissions (10th, 11th, …) will
require zero further edits to this surface — the count is derived,
not declared. The N→M rename for C-4 is necessary because both
clusters now use a derived-count variable and the prior text used N
for the C-4 shape count; without rename the V5 prose would be
ambiguous about whether `N` refers to grammars or shapes.

### §2.1 — Advisory: CH2-scope count-bake remaining at α-E §3 / SYNTHESIS §3

**Out-of-CH1-scope; advisory for CH2.** Three sites in the artefacts
still hardcode `8` against the (now-9-grammar) live roster:

- `α-E:105` "Collapse the 8 per-grammar provider modules in `codegen/`"
- `α-E:108-110` "Refactor the 64 hand-written per-grammar files in
  `crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv,
  ebnf, bnf, math}/`" + "(8 sub-waves)"
- `α-E:116` + `α-E:118` "8 `mod` declarations" + "8 provider files"
- `SYNTHESIS.md:271` C-1 row "Collapse 8 per-grammar provider modules"
  + "Refactor 64 hand-written per-grammar files" + "(8 sub-waves)"

These citations RESOLVE against present-state truth: there are indeed
8 provider modules at `skinny/crates/codegen/src/*_provider.rs` (and
no `css_pretty_provider.rs`), and the 8-name `{json, …, math}` list
correctly enumerates what currently has per-grammar runtime
directories OTHER than `css_pretty`. The CH1 lens (file:line
resolution + measurability) is satisfied — these are not unresolved
citations, mis-cited audit overlays, or unmeasurable gates.

However, the V5 belt-and-braces phrasing pattern (count-derived from
`workspace.metadata.bbnf.grammars`) has now been applied at two
α-E surfaces (the C-3 round-trip gate at `:362-387` under V4; the
C-10 cost table at `:756 + :770` under V5) but NOT at the C-1 scope
description at `:105-110, :116-118`. This is a CH2 / Lock-14
generality concern (count-drift recurrence vector) rather than a CH1
correctness defect, and is flagged here only for cross-lens awareness
— CH2 may wish to recommend a V6 fold extending the
roster-count-agnostic phrasing to the C-1 scope surface if CH2
treats this as a residual Lock-14 leak. CH1 does not REJECT or
REVISE on this surface.

## §3 — Recommended folds for V6 (if any)

None within the CH1 lens. CH1 V5 converges at 100.00 %. CH1 V4 was
at 100.00 %. Both cycles at the V≤5 ceiling, satisfying the two-
consecutive-cycle convergence rule per `ORCHESTRATOR.md §3Z`. No
fresh REJECT or REVISE surface remains within the CH1 lens scope.

The §2.1 advisory is for CH2 consideration; if CH2 elects to recommend
a V6 fold, CH1 would join the disposition under aggregator
arbitration. Independent of that, the CH1 V5 verdict stands at 100 %
and the §3Z chain (V4 + V5 both ≥ 95 %) closes.

The aggregator will integrate this disposition with the other six V5
lens outputs into the V5 consolidated verdict. The V4 CONSOLIDATED
forecast (V5 closes the bracket at the V≤5 ceiling under the §3Z
two-consecutive-cycle rule, with SK-V14 contract durable and
G-Alpha auto-signing) is on-track from the CH1 side.

---

The V5 micro-redispatch was the minimal belt-and-braces edit required
to close the V4 CH2 non-finding at `α-E:756`. The fold preserves
intra-document idiom parity with the V4 F-V4-α-E-1 surface at
`α-E:362-387` — both now enumerate from
`workspace.metadata.bbnf.grammars`, eliminating the count-drift
recurrence vector at both authoritative cost surfaces. The N→M
variable rename in the C-4 prose was forward-discipline hardening
beyond the literal fold prescription, preventing variable shadowing
under the now-derived C-1 count. CH1 V5 returns CONVERGED at 100 %
with one out-of-scope advisory logged for CH2.
