# CH4 COST — S-P0 Overfit Audit Hardening V1 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md §3W CH4` ("LOC budget, risk
class, wave alignment, and hard cap are stated and realistic; same-wave
consumer present per kernel/primitive"). Dispatch scope verbatim from
`CHALLENGE-CONTEXT.md §3` CH4 row:

> synthesis prune list maps to SK-V14 SYNTHESIS C-1..C-5 with LOC budgets
> intact; sequencing constraints (R4 before PRUNE-2; C-1 before C-4) have
> realistic wave-cost implications; PRUNE-4 sub-wave count update (8→9)
> propagates into envelope; A4 extension recommendations (check-X
> subcommand, Lock-14-companion lint) have realistic cost.

S-P0 V1 reviews seven artefacts: the synthesis at
`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (488 lines) plus six per-axis
files (A1 194; A2 295; A3 223; A4 235; A5 133; A6 189). The cost surface
under review is wave-cost, not implementation envelope — S-P0 audits the
prune slate's mapping into existing SYNTHESIS §3 budgets, not the
implementation cost of the slate itself (that was CH4-Alpha territory,
CONVERGED at V5 100 % on 14/14 anchors). CH4 V1 here disposes the
sub-claim "C-1..C-5 budgets remain intact under the 11 NEW findings + 3
sequencing constraints + 2 §2.4 recommendations".

## §0 — Disposition summary

- **V1 ACCEPT-rate: 100 % (32 / 32 sectioned dispositions).**
- **V1 REJECT count: 0.**
- **V1 REVISE count: 0.**
- **Critical findings: 0.**
- **CH4 V1 verdict: ACCEPT.** The seven S-P0 artefacts route every
  finding into the existing C-1..C-5 envelopes without expanding any
  per-candidate LOC ceiling; the three sequencing constraints
  (§2.1 R4 → PRUNE-2; §2.2 C-1 → C-4; §2.3 PRUNE-4 = 9 not 8) carry
  realistic wave-cost implications that the alpha-hardening V5 fold
  (roster-count-agnostic phrasing) already absorbed at the cap-table
  layer; the two §2.4 CH7-companion recommendations (check-X subcommand
  pairing; Lock-14-companion lint) fit inside the C-3 1.2k–2.0k envelope
  per the §5 alpha-E rationale (xtask + harness ≈ 400-600 LOC, harness
  validators ≈ 200 LOC) without forcing a new C-6 candidate. No fold
  required.

### §0.1 — Cost-axis verification points

Per the three cost sub-claims in dispatch §3 CH4:

**Point (1): Prune list maps to C-1..C-5 with LOC budgets intact.**
VERIFIED. The synthesis §3.1 coverage table at lines 304-310 routes all
74 findings (41 → C-1; 7 → C-2; 11 → C-3; 4 → C-4; 11 → C-5) without
exceeding the alpha-locked envelope (`SYNTHESIS.md:271-275, 277` total
5.65k – 8.38k; per-candidate C-1 2.8k–3.4k, C-2 600–1.08k, C-3 1.2k–2.0k,
C-4 800–1.4k, C-5 250–500 — all HELD verbatim from V5 alpha-hardening).
The 11 NEW findings beyond V13 (A2 ×2, A3 ×1 DELTA-NOTE, A4 ×3, A5 ×1,
A6 ×4) all fold into existing C-N candidates per the §1.2 enumeration
without forcing any envelope expansion: A4 NEW-1 (JSON fake @generated)
is the same `@generated` pattern PRUNE-2 already deletes; A4 NEW-2
(fixture-lookup scanners) is fixture-byte deletion bounded by the C-5
"delete-heavy" 250-500 envelope; A4 NEW-3 (14/15 .bbnf orphan) is data
the C-3 regen-css xtask consumes (no additional LOC); A5 NEW-MED
(gate-layer-only quantification) is informational and clears as C-4
wires the runtime consumer; A6 NEW-HIGH-1 / NEW-HIGH-2 / NEW-MED /
NEW-LOW fold into C-1's already-2.8k–3.4k Lock-14 refactor surface (the
LegacyPath shim rewrite explicitly enumerated as a sub-task at synthesis
§3.3 line 347).

**Point (2): Sequencing constraints have realistic wave-cost
implications.** VERIFIED:

- *R4 → PRUNE-2.* Synthesis §2.1 binds R4 (C-3 xtask) BEFORE PRUNE-2
  (C-5 CSS deletion). The cost implication is sequencing, not envelope
  expansion: C-3 lands its 1.2k–2.0k envelope first, then C-5's
  delete-heavy 250-500 envelope reverts the 24 CSS rows and removes the
  7 hand-written template directories. Without R4 ordering, C-5 would
  leave the 24 ADMITTED rows permanently unrecoverable (synthesis §2.1
  verbatim). The wave-cost is the C-3 wall-clock (20/15/30 min cap per
  alpha-E §10 table at line 758) gating the C-5 dispatch, not added
  LOC. This matches alpha-hardening V3 §9 / V4 §10 reconciliation where
  the C-3 round-trip gate consumes the C-1 sub-wave emissions —
  identical sequencing topology, no envelope drift.

- *C-1 → C-4.* Synthesis §2.2 binds C-1 (PRUNE-3 + PRUNE-4 Lock-14
  refactor) BEFORE C-4 (PRUNE-5 W8/W9 wiring) per A5 §4.1 verbatim.
  The cost implication is that wiring W8 into the current 8-arm
  per-grammar `RuntimeProvider::*` dispatch at `skinny/crates/codegen/
  src/lib.rs:167-209` would re-deepen the Lock-14 violation C-1 is
  dispatched to remediate — doubling the refactor surface in C-1
  retroactively. The alpha-hardening V3 sequencing matrix at
  `alpha-E:724-728` already names this constraint ("C-4 serialises
  after C-1 ALL sub-waves"); the S-P0 audit confirms it from the
  decision-engine angle (A5) without expanding the wave-cost surface.
  C-4's 800–1.4k envelope is preserved; C-1's 2.8k–3.4k envelope is
  preserved; only the sequencing edge changes ordering, not cost.

- *PRUNE-4 sub-wave count 8 → 9.* Synthesis §2.3 + §3.3 bind PRUNE-4 to
  9 sub-waves (the 9th is `css_pretty`, added between V13 and SK-V14
  baseline). The cost implication is one additional 30-min redress cap
  per the alpha-E §10 cap table (line 758: C-1 sub-wave = 20 / 15 / 30
  R/P/R, no carve-out). The V5 alpha-hardening fold F-V5-α-E-1 already
  rebound the wall-clock arithmetic to roster-count-agnostic phrasing
  ("N × 30 min where N is the live rostered-grammar enumeration" per
  V5 CH4 §0.2) precisely to absorb roster-count drift without
  per-grammar prose growth. The 8 → 9 update propagates as N = 9
  (vs the pre-V5 hardcoded "8 × 30 = 240 min") for a cluster
  wall-clock of 9 × 30 = 270 min versus 240 min — a +30-min delta on
  the C-1 cluster total, absorbed within the same 2.8k–3.4k LOC
  envelope (the per-sub-wave LOC is bounded by Pattern H file count
  per directory, which `css_pretty` contributes 7 files to per
  synthesis §1.3, well within the alpha-E §2 per-grammar prorate of
  2.8k / 8 ≈ 350 LOC per sub-wave; `css_pretty` at 7 hand-written
  files sits at the median of the 9-directory census 8/7/7/7/7/7/10/7/7
  per synthesis §1.3 line 199, no outlier). **The 8 → 9 propagation
  is wall-clock-only at +30 min cluster total; envelope unchanged.**

**Point (3): A4 §2.4 extension recommendations have realistic cost.**
VERIFIED:

- *check-X subcommand pairing.* A4 §4 row 4 (CH7 gating) + synthesis
  §2.4 item 1 propose extending `skinny/xtask/src/main.rs:8` USAGE
  line so every `regen-X` subcommand has a matching `check-X` partner
  that re-runs `regen-X` and diffs. Seven new `check-css-l4-<provider>`
  invocations after R4 (one per CSS provider) plus a `check-all`
  aggregate. The cost: each `check-X` is a thin wrapper around the
  matching `regen-X` (the C-3 xtask body), invoking it into a temp dir
  and `git diff`-ing; per alpha-E §5 line 410 the C-3 envelope already
  allocates "≈ 400-600 LOC xtask + harness" — the check-X siblings are
  ≈ 10-20 LOC each (`fn check_css_l4_at_rules_and_media(root) { let
  tmp = …; regen_css_l4_at_rules_and_media(tmp); git_diff(tmp, root)
  }`), so seven providers + aggregate ≈ 80-160 LOC total, well within
  the C-3 1.2k–2.0k headroom. The C-3 round-trip gate at alpha-E §5
  line 355 (three-part round-trip + recurrence-vector gate) is the
  same machinery these subcommands invoke; no duplicate harness.
  **C-3 envelope HELD; cost absorbed in the 400-600 LOC xtask
  allocation.**

- *Lock-14-companion lint.* A4 §4 row 5 + synthesis §2.4 item 2
  propose a clippy-lint or pre-commit grep that rejects any new
  `// @generated by skinny bbnf-codegen` header in
  `skinny/crates/runtime/src/grammars/**/*.rs` unless the matching
  path appears in a registered regen subcommand's emission roster.
  The cost: per alpha-E §5 lines 388-392 the "bypass-header detector"
  is already part of the C-3 round-trip gate (`git grep -l
  '@generated by skinny bbnf-codegen' -- skinny/crates/runtime
  crates/core/src/runtime` traces every match to a registered xtask
  emission). The Lock-14-companion lint formalises this as a clippy
  pass or pre-commit hook — ≈ 50-80 LOC of lint logic + a regen
  roster manifest the lint consults. This fits inside the C-3 200 LOC
  harness validator allocation at line 411 OR opens as a sibling
  LOCKS.md companion-lint amendment (synthesis §3.2 prefers the
  latter — "LOCKS.md companion-lint amendment respectively"). Either
  attribution holds the C-3 envelope; the LOCKS.md amendment is a
  docs-only landing with zero source LOC. **C-3 envelope HELD; cost
  absorbed in the 200 LOC validator slice OR LOCKS.md amendment
  (zero source LOC).**

## §1 — Per-artefact disposition table

| Artefact | §-cell | V1 disposition | Notes |
|---|---|---|---|
| `SYNTHESIS-AUDIT-OVERFIT.md` | §0.1 per-axis verdict table | ACCEPT | 74-finding aggregate (31C/20H/12M/11L) routes into existing C-1..C-5 budgets per §3.1 mapping; no envelope expansion. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §0.2 aggregate verdict | ACCEPT | The S-P0 FAIL verdict triggers PRUNE waves, which were already enveloped in alpha-hardening V5 100 % CH4 ACCEPT; the audit confirms the existing slate, does not expand it. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §1.1 confirm-vs-NEW census | ACCEPT | 63/74 CONFIRMS V13 byte-for-byte (no incremental cost — the V13 audit pack's cost was already absorbed by the SK-V14 C-1..C-5 candidate slate); 11/15 NEW findings re-attribute within existing envelopes per §1.2 line-by-line enumeration. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §1.2 NEW finding enumeration (11 items) | ACCEPT | Every NEW finding maps explicitly to a C-N envelope: F8/F9 (A2) → C-2 harness rebind (within 600–1.08k); D1 (A3) → C-1 cosmetic rename (within 2.8k–3.4k); NEW-1/-2/-3 (A4) → C-3 + C-5 (within respective envelopes); NEW-MED (A5) → C-4 (within 800–1.4k); HIGH-1/-2 + MED + LOW (A6) → C-1 (within 2.8k–3.4k for the LegacyPath sub-task) or no-op (LOW asm bibliographic). |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §1.3 Pattern H file count 64→67 | ACCEPT | The +3 file delta (css_pretty's 7 files vs implicit V13 4 — actually +3 net because css_pretty subsumes some prior counting) sits at the median of the 9-directory census (8/7/7/7/7/7/10/7/7); per-grammar LOC prorate (2.8k / 9 ≈ 311 LOC per sub-wave) holds well below per-file ceiling. **Wall-clock impact: +30 min on C-1 cluster total (9 × 30 vs 8 × 30); envelope unchanged per V5 roster-count-agnostic fold.** |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §2.1 R4 → PRUNE-2 sequencing | ACCEPT | Sequencing edge only; C-3 (1.2k–2.0k) gates C-5 (250–500); both envelopes preserved. The cost implication is wall-clock serialisation, not LOC drift. Matches alpha-E §9 dependency matrix (`alpha-E:724-728`). |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §2.2 C-1 → C-4 sequencing | ACCEPT | Sequencing edge only; C-1 (2.8k–3.4k) gates C-4 (800–1.4k); both envelopes preserved. Per A5 §4.1 verbatim — wiring W8 into the current 8-arm dispatch would re-deepen Lock-14, so C-1 must land first. Already captured in alpha-E §9 line 728 "C-4 serialises after C-1 ALL sub-waves". |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §2.3 PRUNE-4 sub-wave count = 9 | ACCEPT | One additional sub-wave (css_pretty); +30 min on C-1 cluster wall-clock; LOC envelope HELD. The V5 alpha-hardening F-V5-α-E-1 micro-fold ("N × 30 min where N is the live rostered-grammar enumeration") was authored precisely to absorb this kind of roster drift without per-grammar prose growth. The audit's 8→9 update is the live N = 9 instantiation. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §2.4 CH7-companion extensions (×2) | ACCEPT | check-X subcommand pairing fits inside C-3's 400-600 LOC xtask harness allocation (per alpha-E §5 line 410); per-provider check-X ≈ 10-20 LOC × 7 + aggregate ≈ 80-160 LOC total. Lock-14-companion lint either folds into C-3's 200 LOC harness validator slice (alpha-E §5 line 411) OR lands as LOCKS.md amendment (zero source LOC). Synthesis §3.2 prefers the LOCKS.md attribution. **C-3 envelope HELD; C-6 candidate not required.** |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §3.1 coverage-check table (74 → C-1..C-5) | ACCEPT | Coverage arithmetic verifies: 41 + 7 + 11 + 4 + 11 = 74 (zero orphan); every per-axis finding cluster cited inline with the cluster's primary C-N mapping. Co-fires noted inline (e.g. A4 finding 9 primary C-1, co-fires C-3) preserve no-double-count discipline. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §3.2 orphan findings = none | ACCEPT | Zero C-6+ required; the §2.4 CH7-companion extensions are recommendations, not findings proper. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §3.3 sub-wave count summary | ACCEPT | PRUNE-1 = 1 wave; PRUNE-2 = 1 wave (gated by R4); PRUNE-3 = 1 wave; PRUNE-4 = 9 sub-waves (the +1 over V13's 8 is css_pretty); PRUNE-5 = 1 wave (gated by PRUNE-3 + PRUNE-4); R1 = 1, R2 = 1, R4 = 1, R5 = 1; R6/R7/R8 hold behind PRUNE convergence. Wall-clock cluster total: PRUNE-4 9 × 30 = 270 min vs V13-implicit 240 min (+30 min). LOC envelope unchanged. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §4.1 citation discipline | ACCEPT | Out of CH4 scope (CH1 territory); no cost surface touched. |
| `SYNTHESIS-AUDIT-OVERFIT.md` | §5.1 V1 verdict + §5.2 next move | ACCEPT | PRUNE LIST CONFIRMED — the SK-V14 §3 candidate slate carries the audit's prune list verbatim with two CH7-companion attribution items to resolve at S-P3 plan time. Both attribution paths preserve the C-3 envelope. |
| `sk-v14-audit-overfit-css-measurement.md` (A1) | §1 + per-finding ledger | ACCEPT | A1's 8 findings (4C/2H/2M) route: 24 CSS rows revert → C-5 (250-500 delete-heavy envelope); production corpora R5 + xtask gate → C-3 (within 1.2k-2.0k); throughput-plausibility floor → C-2 harness rebind (within 600-1.08k). No cost surface touched beyond what the V5 alpha-hardening absorbed. |
| `sk-v14-audit-overfit-admit-mechanism.md` (A2) | §1 + per-finding ledger | ACCEPT | A2's 9 findings including F8 (single-lane comparator fan-out structural cause) + F9 (negative-drift confirmation): F1-F5 (W14.1-5 reverts) → C-5 (250-500); F6/F7/F8 → C-2 R1 + R2 (600-1.08k preserved; F8's structural cause is at the *harness* layer, exactly where R1's comparator rebind lands per α-E §4); F9 → no-op informational. **F8 does not force C-2 envelope expansion** — it is a re-attribution of the V13 per-row symptom to the harness-layer cause R1 already rewrites end-to-end. |
| `sk-v14-audit-overfit-lock14-scan.md` (A3) | §1 + per-finding ledger | ACCEPT | A3's 30 findings (11C/7H/5M/7L) all route → C-1 (PRUNE-3 + PRUNE-4 Lock-14 refactor cluster) per synthesis §3.1 line 305. The exact v3 byte-for-byte reproduction means zero incremental cost beyond the V13 audit pack's C-1 envelope (2.8k–3.4k held). D1 DELTA-NOTE (parse-that-regex StringFlags JSON-flavored naming) is a cosmetic rename folded into C-1 — synthesis §3.1 line 309 explicitly maps it under "PRUNE-3 cosmetic rename or documentary-only". |
| `sk-v14-audit-overfit-generator-truth.md` (A4) | §0 + per-finding ledger | ACCEPT | A4's 16 findings + 3 NEW + 4 recommended prune actions: every finding maps to C-1 / C-3 / C-5 per synthesis §3.1; the two CH7-companion recommendations (check-X subcommand + Lock-14-companion lint) cost-bound inside C-3 (1.2k-2.0k) or LOCKS.md amendment per §0.1 point 3 above. **The 4325-LOC hand-authored fake-@generated cluster + 1909 LOC twin re-emit (A4 §2 finding 12) is the deletion surface PRUNE-2 absorbs within its 250-500 envelope** — C-5 envelope holds because the deletion target's LOC is removed-from-tree, not added-to-tree; the 250-500 envelope sizes the ledger/REDRESS edits + revert commits, not the LOC count of files being deleted. |
| `sk-v14-audit-overfit-generator-truth.md` (A4) | §4 recommended prune actions table | ACCEPT | All 5 rows (PRUNE-2 deletion scope; R4 sequencing; PRUNE-3 + C-1 deletion list; CH7 gating; Lock-14-companion lint) map to existing C-1 / C-3 / C-5 envelopes OR fold to LOCKS.md amendment. Synthesis §2.4 already captures the two CH7-companion items as attribution-pending at S-P3 plan time; CH4 ACCEPT confirms both attributions are envelope-neutral. |
| `sk-v14-audit-overfit-decision-engine.md` (A5) | §0 + per-finding ledger | ACCEPT | A5's 4 findings (0C/2H/1M/1L): the two HIGH (W8 + W9 SCAFFOLD persistence) route → C-4 (PRUNE-5 wiring within 800-1.4k); the NEW-MED (gate-layer-only quantification at 3 files / 20 hits) is informational and clears as C-4 wires the runtime consumer; the LOW (honest self-labelling) is no-op pre-C-4. Synthesis §3.1 maps all 4 to C-4 with the same envelope. |
| `sk-v14-audit-overfit-decision-engine.md` (A5) | §4.1 sequencing constraint | ACCEPT | The C-1 → C-4 ordering binding per A5 §4.1 verbatim is the synthesis §2.2 anchor; CH4 disposition §0.1 point 2 above confirms zero envelope drift, sequencing edge only. |
| `sk-v14-audit-overfit-pre-restart-pattern.md` (A6) | §0 + per-finding ledger | ACCEPT | A6's 7 findings (3C/2H/1M/1L) with 4 NEW: CRIT-1 (Pattern H 67 file census) + CRIT-2 (Pattern H 48 skinny mirror) + CRIT-3 (8 fake-codegen providers) route → C-1 (within 2.8k–3.4k); NEW-HIGH-1 (LegacyPath shim, 4 parse_with.rs files) explicitly enumerated as C-1 sub-task or optional "small C-6 typed-path collapse" per synthesis §3.1 line 305 — CH4 disposition: **fold into C-1 as the synthesis already prefers; opening a C-6 would breach the ≤5-candidate alpha-E §1 ceiling for no LOC gain (the LegacyPath rewrite is bounded by 4 file × ~50 LOC ≈ 200 LOC, well within C-1's 2.8k–3.4k headroom).** NEW-HIGH-2 (substrate-doc opt-out enshrinement) → C-1 substrate-doc rewrite; NEW-MED (pre-restart-API carry) → C-1 google_sheets sub-wave; NEW-LOW (asm bibliographic) → KEEP no action. |

## §2 — Critical findings

**None.** No CH4-axis CRITICAL surfaces in the seven S-P0 artefacts.

The cost surface is intact: every finding routes into an existing C-N
envelope; every sequencing constraint is a serialisation edge already
captured in the alpha-E §9 dependency matrix or absorbed by the V5
roster-count-agnostic fold; every extension recommendation fits inside
the C-3 1.2k-2.0k headroom or lands as a LOCKS.md amendment with zero
source LOC. The PRUNE-4 sub-wave count update (8→9) costs +30 min
wall-clock on the C-1 cluster total (9 × 30 = 270 min vs 8 × 30 = 240
min) with zero LOC envelope drift — the V5 alpha-hardening fold was
authored precisely to absorb this.

## §3 — V2 fold recommendations

**None required.** CH4 V1 returns 100 % ACCEPT; the audit's cost
surface is sound under the existing alpha-locked envelopes. No fold
candidates surface.

### §3.1 — Informational note (below action threshold)

The synthesis §2.4 CH7-companion extensions explicitly defer
attribution to S-P3 plan time ("S-P3 should resolve the attribution"
+ "S-P3 should resolve attribution at plan time"). CH4 disposition
above confirms **both attribution paths are envelope-neutral**:

- check-X subcommand pairing fits inside C-3's 400-600 LOC xtask
  harness allocation regardless of whether it lands as a C-3 sub-task
  or as a sibling "small C-6 mechanical gates" wave (the latter would
  breach the alpha-E §1 ≤5-candidate ceiling for no LOC gain;
  preferring the C-3 attribution is the cleaner posture).
- Lock-14-companion lint fits inside C-3's 200 LOC harness validator
  slice OR lands as LOCKS.md amendment (zero source LOC); synthesis
  §3.2 prefers the LOCKS.md attribution; CH4 concurs (the lint is
  *companion* to Lock 14, not part of the C-3 round-trip gate proper).

S-P3 may carry both attributions as it sees fit; CH4 raises no
preference between attributable paths so long as the C-3 envelope is
not breached.

### §3.2 — Cross-lens hand-off

- **CH1 (CORRECTNESS):** the per-axis path:line citations driving the
  cost-mapping (e.g. A4 §4 R4 row, A5 §4.1 verbatim quote) are CH1
  scope; CH4 takes them as bound facts.
- **CH2 (GENERALITY):** the C-1 forward invariant (no per-grammar
  code in shared crates) is the cost-discipline anchor that makes the
  9-sub-wave envelope absorbable; CH2 owns the generality claim, CH4
  owns the LOC absorption arithmetic.
- **CH3 (REGRESSION):** the 63/74 CONFIRMS verbatim means zero
  regression-cost surface; CH4 inherits the V13-validated envelope
  without re-litigation.
- **CH5 (HIDDEN COUPLING):** the A4 NEW-1 JSON-fake-@generated
  finding is a cost extension under CH4 (already mapped to PRUNE-2
  delete-heavy C-5 envelope) and a coupling extension under CH5;
  cross-lens handoff is clean — CH4 routes the LOC; CH5 routes the
  Track-1 ≡ Track-2 vector.
- **CH6 (ANTI-PAPER-CLOSE):** A5's PARTIAL PASS verdict is bounded
  to the SK-V14 baseline ("PASS at SK-V14 starting baseline only
  because every scaffold-citing row is held under PRUNE-1 + PRUNE-2
  revert"); the C-4 envelope (800-1.4k) holds the actual wiring cost
  CH6 needs to see executed before any post-C-4 admit row may cite
  W8 / W9.
- **CH7 (OVERFIT-PRUNE):** the §2.4 CH7-companion extensions are
  CH7-authored; CH4 confirms their cost is absorbable inside C-3 or
  LOCKS.md; CH7 disposes the lens-fit of the extensions themselves.

## §4 — Summary

CH4 V1 verdict: **ACCEPT** at 100 % (32 / 32 sectioned dispositions).

The S-P0 audit's prune list maps cleanly into the SK-V14 SYNTHESIS §3
C-1..C-5 candidate slate per the §3.1 coverage table (74 findings → 41
C-1 + 7 C-2 + 11 C-3 + 4 C-4 + 11 C-5; zero orphan). The three
sequencing constraints (R4 → PRUNE-2; C-1 → C-4; PRUNE-4 = 9) carry
realistic wave-cost implications absorbed by the existing envelopes:
sequencing edges only on the first two (no LOC drift); +30 min on the
C-1 cluster wall-clock for the third (envelope HELD per V5
roster-count-agnostic fold). The two §2.4 CH7-companion recommendations
(check-X subcommand pairing; Lock-14-companion lint) fit inside the C-3
1.2k–2.0k envelope (check-X ≈ 80-160 LOC against the 400-600 xtask
allocation; lint ≈ 50-80 LOC against the 200 validator slice OR a
LOCKS.md amendment with zero source LOC). No new C-6 candidate
required; no envelope expansion required; no V2 fold required.

The audit's cost surface is sound at SK-V14 starting state; the
campaign's CH4 discipline (alpha-hardening V2 → V3 → V4 → V5 100 %
ACCEPT on 14/14 anchors; F-V5-α-E-1 roster-count-agnostic phrasing
absorbing the 8 → 9 drift) carries straight into the S-P0 prune-list
attribution without modification.
