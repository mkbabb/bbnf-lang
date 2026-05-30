---
lens: CH7 OVERFIT-PRUNE
pass: T-P3-synthesis
cycle: V1
reviewer: CH7 (V1)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
subject: restart/audit/totality/sk-v17/p3/{3a,3b,3c,3d,3e,3f}.md + 3c-locks-v+1-diff.md
focus: 16-lock count preserved (no silent renumber; ADD/RETIRE G-Omega-gated); no contrivance; the fold is genuinely general; no fabricated speed claim; lightningcss the fair bar
dispositions: { ACCEPT: 11, REVISE: 1, REJECT: 0 }
verdict: ACCEPT-WITH-ONE-REVISE (91.7% ACCEPT)
---

# CH7 OVERFIT-PRUNE — T-P3 SK-V17 Synthesis (cycle V1)

## Lens scope

CH7 is the overfit-prune lens. It does not re-audit citation-resolution
(CH1) or coupling (CH5); it asks five questions and only those: (1) is the
16-lock count genuinely preserved, with no silent renumber and ADD/RETIRE
G-Omega-gated; (2) is any delta a contrivance dressed up as principle; (3)
is the tape/`ValueRef<G>`/NEON fold *genuinely* general or JSON+CSS overfit
wearing a generality costume; (4) is any speed claim fabricated; (5) is
lightningcss (and the JSON SOTA cohort) held as the fair, un-gamed bar. I
verified the load-bearing claims against ground truth at HEAD `2a76916ac`,
not against the prose's self-citations.

## Verification ledger (ground-truth re-execution at HEAD)

| claim under test | source | re-executed result | verdict |
|---|---|---|---|
| 16 numbered locks, no renumber | `restart/locks/LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453` | All 16 list-items 1.–16. present and verbatim (`Tape…`→`SIMD/ASM allowlist`); v+1 diff inserts an addendum at `:608-609`, adds/retires/renumbers ZERO | TRUE |
| 5 BackendShape variants verbatim | `restart/locks/LOCKS.md:107`-`108` | `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` present; addendum restates the five, adds no 6th | TRUE |
| v+1 diff insertion point | `3c-locks-v+1-diff.md:49` `@@ -606,6 +606,52 @@` | SK-V15 addendum ends `:608`; `## v+1 Governance Boundary` at `:610`; insertion at `:608-609` is correct | TRUE |
| P1 leak baseline "7 hits, all strategy.rs" | `3e:139` | `rg 'JsonParser\|CssL4Parser' crates/ir/src` = 7 hits, all `registry/strategy.rs:132,137,149,197-198,292,315` — string-ident registry + doc-comments, NOT runtime `match grammar{}` arms | TRUE |
| `StructLayout` = 960 sites in crates/ | `3c:94`, `3a:62`, `3b:146` | `rg -c StructLayout crates/` summed = 960 | TRUE |
| `backend_shape`/`LayoutFacts` = 0 in crates/ | `3c:94` (path-(b) non-zero realisation) | `rg -c 'backend_shape\|LayoutFacts' crates/` = 0 | TRUE |
| 28-65×/983×/10583× regression | SPEC `:793`-`795` | SPEC text matches exactly (28-65 bbnf/sheets, 983 css bootstrap, 10583 WATCHDOG tailwind) | TRUE (cited, not invented) |
| JSON >SOTA carrier | `skinny/RESULTS.md:5-55` | twitter parse_only: Track 1 8349.290 Mbps > sonic-rs strict 4913.095 Mbps (+69.9%), per-iter equality PASS | TRUE (real measured row) |

Every load-bearing number CH7 could falsify resolved to ground truth. No
fabrication surfaced.

## Section dispositions

### §3A ARCHITECTURE synthesis — ACCEPT
Eight deltas; each fold (`ARCH-3A-S17-D01..D08`) is conservative against the
V1 surface (`3a:33`-`38`: ARCH §7.3 already frames the five shapes as the
tape's projections; the fold makes placement explicit, does not invent a
substrate). D04 holds the 5-shape canon verbatim and discharges the
"no silent 6th shape" mandate in the **negative on two independent grounds**
(`3a:60`): the LAC-1E-14 categorical precedent + the `admits_collapsed_stage`
x86-binding that mechanically refuses on aarch64. The D03 classifier
"impl-exceeds-spec, 0-LOC narrative fold" claim is true — the classifier is
wired across 8-of-9 generated grammars (I confirmed the alphabet-as-data
form). No fabricated speed claim; regression numbers cited to SPEC. No
contrivance.

### §3B MASTER-PLAN reconciliation — ACCEPT
No refuted wave revived (CH3-adjacent but in CH7 remit for overfit): the
AZ-IV 118× / per-leaf-indirection 28-65×/983×/10583× / fact-stream-String /
x86 rejections are carried as **fences** (`3b:105`-`106`,`:127`,`:146`),
each pre-blocked as a fold-target shape, NOT re-derived nor re-opened.
W4 fence is "0 LOC + 40-80 doc LOC" honestly priced. No fabricated throughput.

### §3C LOCKS crystallisation — REVISE (one defect) — see below

### §3D skinny-fold — ACCEPT
Monotonic direction held (`3d:32`-`50`): SKINNY wins → V1-authoritative;
SKINNY rejections → locks-strengthening; totality never dictates back. The
ONE load-bearing WIN is correctly the SoA `Tape`+`ValueRef<G>` (`3d:42`,`:73`),
grounded in `RESULTS.md:5-55` (verified). The >SOTA framing defers to the
empirical row, does not assert a fresh number. Honest.

### §3E grammar-generalisation — ACCEPT (the strongest anti-overfit section)
This is where overfit would hide, and it does not. The per-grammar matrix
(`3e:98`-`111`) labels JSON **by-exercise proven**, CSS **by-exercise
SK-V17 first-mover**, and Sheets/BBNF-self **by-construction (SK-V18 proof)** —
never claimed proven. The future-grammar onboarding test (`3e:137`-`149`)
uses a **monotonic-decrease-to-zero rule against a live HEAD baseline** (P1 =
7 hits, re-executed and confirmed), NOT a fabricated clean gate — the precise
discipline that defeats paper-generality. The fail-closed condition scopes
the claim "to the witnessed grammars and may not use fleet-wide wording"
(`3e:148`-`149`). The classifier generality is genuinely config-breadth
(alphabet-as-data across 8 grammars), a separate axis from the value-fold,
and is never conflated with fleet-wide value-plane proof. This is general,
not overfit. Lock 14 grammar-neutrality preserved; no JSON-narrowing.

### §3F MIGRATION/HANDOFF — ACCEPT
Next-cycle directive names concrete, measurable entry conditions
(`3f:62`,`:68`-`69`,`:83`): Pass Omega CRUD-4 → G-Omega → SK-V18 W0, with
named blockers and gates; the engineered-defer aperture is closed (`3f:69`).
CSS-on-tape >SOTA is the SK-V18 *proof obligation*, not an asserted-as-met
claim — correct anti-paper-close.

## The one REVISE

**CH7-S17-R1 — REVISE** (`restart/audit/totality/sk-v17/p3/3c-locks-crystallisation.md:55`)

The Executive Summary states: "**9 ACCEPT, 5 MODIFY, 0 REJECT, 0 DEFER**".
This contradicts the authoritative disposition tally (`3c:143`-`147`):
9 ACCEPT + 3 ACCEPT(ORQ-crystallised) + 2 MODIFY + 0 REJECT + 0 DEFER = 14
candidates. The "5 MODIFY" figure is wrong — there are exactly **2 MODIFY**
(`LAC-2F-FOLD-05`, `LAC-1E-SKV17-04`). The frontmatter sums correctly
(answered: 14) and the `3c-locks-v+1-diff.md:39` correctly reads
"9 ACCEPT, 3 ORQ-ACCEPT, 2 MODIFY"; the defect is isolated to the prose
summary line. The "5" appears to be a stale transcription (conflated with
the 5 fold-LACs / 5 clauses).

Why CH7 (not merely CH1): the disposition arithmetic is the integrity
guarantee that "no candidate is silently dropped" (the CH1+CH6 REJECT class
this lens shares responsibility for via the 16-lock-count-preserved axis). A
G3 reader scans the exec summary; a tally that does not reconcile with its
own matrix erodes the "no silent synthesis" guarantee even when the matrix
itself is correct.

**Concrete fix**: at `3c:55` replace "**9 ACCEPT, 5 MODIFY, 0 REJECT, 0 DEFER**"
with "**9 ACCEPT, 3 ACCEPT (ORQ-crystallised), 2 MODIFY, 0 REJECT, 0 DEFER**"
(14 total) — matching the tally at `3c:143`-`147` and the diff at
`3c-locks-v+1-diff.md:39`.

## Overfit-prune findings: NONE beyond R1

- **16-lock count**: preserved, verified verbatim; ADD/RETIRE correctly
  G-Omega-gated; the addendum is the gate object, not an in-place edit. No
  silent renumber. ACCEPT.
- **Contrivance scan**: every delta answers a T-P1 divergence or T-P2 LAC
  with verified path:line; D04's two-ground refutation and D05's fence are
  principled, not bolted-on. No contrivance.
- **Genuine generality**: §3E proves it with by-construction scoping + a
  live-baseline monotonic onboarding test. The fold is general (alphabet-as-
  data, grammar-parametric `ValueRef<G>`), not JSON+CSS overfit. No
  fleet-wide over-claim survives.
- **No fabricated speed claim**: all perf numbers (118×, 28-65×/983×/10583×,
  8349 Mbps) cited to SPEC or RESULTS.md and re-verified. No invented "Nx
  faster". CSS bar is an open SK-V18 obligation, honestly so.
- **lightningcss fair bar**: the bar is held via >SOTA + RESULTS.md same-plane
  rows for JSON; CSS-vs-lightningcss is correctly *not yet asserted met*. Fair.

## Open question (tagged)

| lens | question | receiver | gate |
|---|---|---|---|
| CH7/CH4 | After R1's tally fix, should the exec summaries of 3A/3D/3E carry a one-line "scope-honesty banner" (proven-vs-by-construction) so a G3 skim cannot mistake the by-construction grammars as proven? | 3C + 3A/3D/3E authors (V2) | CH6 anti-paper-close re-scan of the V2 exec summaries |

## Verdict

**ACCEPT-WITH-ONE-REVISE.** 11 ACCEPT, 1 REVISE, 0 REJECT (91.7% ACCEPT).
The 16-lock count and 5-shape canon are preserved and verified verbatim; the
fold is genuinely general (not overfit) with honest by-construction scoping
and a live-baseline onboarding test; no speed claim is fabricated; the SOTA
bar is held fair. The single REVISE is an arithmetic inconsistency in the 3C
exec-summary tally that contradicts the artefact's own (correct) matrix —
a load-bearing integrity surface, cheap to fix, no candidate actually dropped.
