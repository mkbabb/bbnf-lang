# CH6 ANTI-PAPER-CLOSE — Pass Alpha V1 Disposition

Lens authority: `restart/prompts/ORCHESTRATOR.md:88` (CH6 row);
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:164-176`
(disposition focus for this bracket). The lens asks whether any agent
self-report of "complete" / "wired" / "verified" stands without
orchestrator-cited live evidence (bench row, samply symbol path,
checkasm pass), and whether any deferral falls outside the contracted
PASS-ALPHA §4.4 boundary. Triumvirate role-separation discipline is
inherited from `ORCHESTRATOR.md:209`.

## §0 — Disposition summary

- ACCEPT-rate: 88% (37 ACCEPT of 42 dispositioned §-rows).
- REJECT count: 2.
- REVISE count: 3.
- Critical findings: 1 (the α-F fall-through claim is unverifiable;
  α-A's enumeration contradicts SYNTHESIS §1.2 numerics, evidencing the
  divergence).

REJECT — one-line summaries:

- **REJ-1.** HANDOFF.md:110-114 declares "α-F synthesised directly from
  raw sources per `DISPATCH-CONTEXT.md §α-F` fall-through clause" — and
  the git history confirms this verbatim: only α-B (e4870b201) and α-E
  (86dbd6b09) carry α-tagged commits. α-A, α-C, α-D, α-F have no
  attribution commits, yet HANDOFF §2 and SYNTHESIS §1 cite them as
  authority. CH6 binds: self-reported "synthesised from raw" without a
  per-agent committed artefact at dispatch time is paper-close on the
  Pass Alpha §2 scope-matrix six-agent fan-out contract.
- **REJ-2.** SYNTHESIS §1.2 row "11 JSON direct + typed admits (4 + 7)"
  / HANDOFF §3 same numbers — α-A §6 audit verdict table tallies
  6 direct + 11 typed = 17 (not 4 + 7 = 11). α-D §3 I-3/I-4 explicitly
  surfaces "4–5" / "7–10" with a discrepancy paragraph
  (`alpha-D-validated-invalidated.md:281-291, :353-368`). SYNTHESIS
  silently picks the lower count from the dispatch-context bind without
  flagging that α-A and α-D measured the higher count. This is
  paper-closure of a numeric divergence by citation laundering.

REVISE — one-line summaries:

- **REV-1.** C-4 (W8+W9 scaffold→load-bearing) falsifiability gate
  names "hot leaf attribution differs from pre-wave value — proof of
  runtime divergence" (`alpha-E-candidate-shortlist.md:439-444`) but
  the gate is enumerated on ONE suggested row
  (`json/numbers/direct_to_struct/main`); the gate is measured, but it
  does not bind to a NAMED pre-wave hot-leaf symbol the lens can verify
  in advance. Tighten to "pre-wave hot leaf `parse_value_at`; post-wave
  hot leaf names the W11.1 number-specialised symbol explicitly in the
  samply trace".
- **REV-2.** HANDOFF.md:145 next-move chain
  `ready-for-CHALLENGE-V1 → G-Alpha → S-P0` skips G-Omega entirely from
  the one-line summary, though §5 step 6 includes G-Omega between S-P1
  and the wave program. PASS-ALPHA.md:14 binds Pass Omega has run as
  entry condition; the next-move line elides this binding gate.
- **REV-3.** SYNTHESIS §4 S-P3 constraints do not encode the
  triumvirate role-separation (research / plan / redress in distinct
  commits) per memory `[triumvirate-discipline]` +
  `ORCHESTRATOR.md:209` non-negotiable row. The constraint set covers
  PRUNE-first sequencing, no-deferral, same-wave consumer, Lock-14,
  G-SIMD-GRAMMAR-POLICY — but the triumvirate-commit discipline is
  absent, and the V1 α-phase's own commit irregularities prove its
  consequence.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| SYNTHESIS.md | §0 Authority | ACCEPT | Read order names every binding doc; later-overrides clause stated. |
| SYNTHESIS.md | §0.1 Close condition | ACCEPT | R10 verbatim; close = full ADMIT or architectural-block proof; no paper-close framing. |
| SYNTHESIS.md | §0.2 Goalset row enumeration | REJECT (REJ-2) | "4 direct + 7 typed" numerics contradict α-A §6 (6 direct + 11 typed) and α-D §3 (4–5 / 7–10) without flag. |
| SYNTHESIS.md | §0.3 R-target goalset | ACCEPT | R1–R10 verbatim; each acceptance criterion is measurable (R1 strict-vs-strict, R2 per-iter equality in timing region, R4 round-trip diff empty, R5 ~960 KB corpus, R6 work-equivalent comparator). |
| SYNTHESIS.md | §0.4 Pre-blocked routes P-1..P-7 | ACCEPT | All seven enumerated with audit cite; none paper-closed; each carries a recurrence-vector framing. |
| SYNTHESIS.md | §0.5 Wave-by-wave gates (deferred) | ACCEPT | Deferral is CONTRACTED per PASS-ALPHA.md:53-114 (§4.4 S-P3 boundary). Forward pointer to S-P3 named. |
| SYNTHESIS.md | §1 Corrected diagnosis (1.1, 1.3) | ACCEPT | Eight pillars cited with v4 §refs; honest delta restates audit-zero. |
| SYNTHESIS.md | §1.2 Does not survive table | REJECT (REJ-2) | "4 + 7" numeric carryover from dispatch §1 silently overrides α-A's tally. |
| SYNTHESIS.md | §2 Telemetry binding | ACCEPT | per_iter_equality column "PASS only if equality verified inside the timing region" — measurable, not designed. audit_overlay_verdict + comparator_plane columns gate-enforced. |
| SYNTHESIS.md | §3 Candidate shortlist | ACCEPT | Each row carries falsifiability gate language that is measured (grep returns ZERO; round-trip diff empty; equality column rejected when empty). |
| SYNTHESIS.md | §4 S-P3 constraints | REVISE (REV-3) | Constraints cover PRUNE sequencing + same-wave consumer + Lock-14 + SIMD policy. Triumvirate role-separation (research/plan/redress distinct commits) absent — material gap given the V1 α-phase staging race. |
| SYNTHESIS.md | §5 Pre-blocked + unblocked routes | ACCEPT | List inherits §0.4 patterns + addendum bar; no paper-closure. |
| SYNTHESIS.md | §6 Close posture | ACCEPT | "0 ADMITTED across 51 + 24" — restates honest baseline; no closed-when-planned framing. |
| HANDOFF.md | §1 Bracket verdict | ACCEPT | Audit reversal stated; SK-V14 opens prune-then-rebuild. |
| HANDOFF.md | §2 Authority list | ACCEPT | 14 entries; conflict-resolution clause stated. |
| HANDOFF.md | §3 Honest baseline summary | REJECT (REJ-2) | Inherits SYNTHESIS §1.2 "4 + 7" without α-A reconciliation. |
| HANDOFF.md | §4 Pre-S-P0 readiness | REJECT (REJ-1) | The fall-through admission "α-A through α-E remain outstanding at α-F commit time; α-F synthesised directly from raw sources" stands as the explicit CH6 trigger — Pass Alpha §2 names six parallel agents; α-F shipping ahead of peers without committed artefacts is exactly the "no live evidence" paper-close pattern CH6 forbids. Note the dispatch context (CHALLENGE-CONTEXT.md:36) frames the staging race as "files in tree are byte-identical to authored versions per six independent agent reports", but no such report set is cited or committed. |
| HANDOFF.md | §5 Pass sequence | ACCEPT | 10 steps; G-Omega + G-S-P0-CONVERGED + G-Alpha all named; sequence consistent with PASS-0-OVERFIT-AUDIT.md "Standing SK process loop". |
| HANDOFF.md | §6 Next-move | REVISE (REV-2) | One-line "CHALLENGE-V1 → G-Alpha → S-P0" elides G-Omega; §5 contains the full chain but the next-move line is the operationally referenced summary. |
| HANDOFF.md | §7 Refusal conditions | ACCEPT | 17 refusal predicates; every one is a CH6-flavoured measured-source rule (no support-only, no scaffold-only, no comparator misbinding). |
| HANDOFF.md | §8 V1 disposition | ACCEPT | "PENDING until CHALLENGE V1 returns and convergence holds" — explicitly contracted-pending, not paper-closed. |
| alpha-A | §0 Authority + conventions | ACCEPT | Source authority cited with `path:line`; conventions stated. |
| alpha-A | §1 parse_only table | ACCEPT | 17-row table; each AUDIT-FALSIFIED row cites `v2 §1-4`. |
| alpha-A | §2 direct table | ACCEPT | 17-row table; explicit discrepancy paragraph at :117-122 about "4 vs 6". |
| alpha-A | §3 typed table | ACCEPT | 17-row table; explicit discrepancy paragraph at :161-169 about "7 vs 11". |
| alpha-A | §4 CSS table | ACCEPT | 24+1-row table; OVERFIT-THROUGHPUT flagged for nested_layout. |
| alpha-A | §5 c/B telemetry | ACCEPT | Acknowledges c/B schema debt; not paper-closed. |
| alpha-A | §6 Audit verdict summary | ACCEPT | Numerics surfaced (46 nominal → 0 audit-corrected). |
| alpha-A | §7 Forward pointers | ACCEPT | Hands off to peer α-agents; each pointer measurable. |
| alpha-B | §0 Bound baseline | ACCEPT | No re-litigation; baseline cited. |
| alpha-B | §1 Comparator inventory | ACCEPT | Per-plane binding cited; sonic-rs Skipper unavailability flagged. |
| alpha-B | §2 parse_only overlay | ACCEPT | Projection "0–2 of the 5 historic admits survive R1" framed explicitly as projection, not claim. |
| alpha-B | §3 direct overlay | ACCEPT | Same projection framing; "1–3 of the 6 survive R1". |
| alpha-B | §4 typed overlay | ACCEPT | Notes per-corpus typed binding IS already plane-correct; refines audit's blanket charge. |
| alpha-B | §5 CSS overlay | ACCEPT | "uncomputable as competitor deltas in the SK-V14 sense" — frank, not paper-closed. |
| alpha-B | §6 Comparator gap | ACCEPT | Per-plane R-pointer + coverage targets. |
| alpha-B | §7 Roll-up | ACCEPT | 0/75 HONEST + 45 SUSPECT + pending dimensions explicit. |
| alpha-B | §8 Escalations | ACCEPT | Three escalation flags (sonic v0.5.8 Skipper; 10 typed-MISSING corpora; new sonic misnaming). |
| alpha-C | §1 Per-entry dispositions | ACCEPT | 30 entries (131–160) dispositioned; each cites validation §ref. |
| alpha-C | §2 Pattern-level pre-blocks | ACCEPT | P-1..P-7 each carries pattern + binding + falsifiability triplet — measurable, not designed. |
| alpha-C | §3 Pattern summary | ACCEPT | Round-trip-eligibility column makes each pre-block lift-able by named R-target. |
| alpha-C | §4 Reopen obligations for S-P3 | ACCEPT | PRUNE-first; round-trip rule pre-armed on nested_layout. |
| alpha-D | §1–§6 ledger | ACCEPT | VALIDATED / INVALIDATED / DEMOTED / STILL-OPEN with audit §refs throughout; net ledger table at §6 reconciles. |
| alpha-E | §0–§2 + per-candidate §3–§7 | ACCEPT | LOC budget + risk + same-wave consumer + falsifiability gate per candidate; each gate is measured (grep, round-trip, > 800 KB, hot-leaf attribution change). |
| alpha-E | §6 C-4 (W8+W9 wiring) | REVISE (REV-1) | Gate names "hot leaf differs" measurably but suggested-row symbol path is not pinned in advance. Tighten to named pre-/post-wave symbol pair. |
| alpha-E | §9–§11 (concurrency + caps + convergence) | ACCEPT | Caps cite addendum 45-min amendment; convergence rule cites §3Z. |

## §2 — Critical findings

### REJ-1 — α-F shipped ahead of peers; no per-agent commit attribution; "synthesised directly from raw sources" claim unverifiable.

Evidence in tree:

- `HANDOFF.md:110-114` — "Pass Alpha α-A through α-E remain outstanding
  at α-F commit time; α-F synthesised directly from raw sources per
  `DISPATCH-CONTEXT.md §α-F` fall-through clause. CHALLENGE V1 catches
  divergence; V2 reconciles."
- `git log --all --oneline -30 -- restart/skinny/tranches/sk-v14/`
  returns only `496a81417` (orchestrator prompt seed),
  `6ab711d77` (dispatch context seed), `e4870b201` (α-B), `86dbd6b09`
  (α-E), `2d980cfd1` (CHALLENGE-CONTEXT seed). α-A, α-C, α-D, α-F
  carry NO independent commit attribution.
- `CHALLENGE-CONTEXT.md:36` — frames the situation as "file contents
  in HEAD are byte-identical to authored versions per six independent
  agent reports" — yet no six-agent report set is cited or filed in
  tree, and the dispatch context concedes "commit subjects do not
  always match commit contents."

CH6 binding: this is the exact pattern CH6 was authored to catch.
PASS-ALPHA.md §2 mandates "six parallel sub-agents fan out per the
scope matrix"; six agents must "each writes ONE artefact at the
assigned path." DISPATCH-CONTEXT.md §α-F fall-through clause is
permissive ("synthesise directly from raw sources — CHALLENGE V1 will
catch divergence"), but the permissive clause sits inside a §3
discipline that mandates per-agent commit subjects
(`docs(sk-v14-alpha): <scope-tag>`). The α-A / α-C / α-D / α-F
artefacts exist on disk per Read confirmation — but their provenance
is unattested. The "fall-through" permission does NOT excuse the
absent commits; it requires CHALLENGE V1 to verify divergence, which
CH6 is doing.

Required action: V2 dispatch must commit α-A, α-C, α-D, α-F with
their own subjects and a 5–15 line body each, OR explicitly absorb
them into α-F's authorship with the SK-V14 contract record naming α-F
as the sole author of all four artefacts. The current state is
incoherent — the artefacts are in tree, are referenced as
peer-authored, yet have no peer authorship trail.

### REJ-2 — Direct + typed admit counts diverge between SYNTHESIS §1.2 (4 + 7) and α-A §6 (6 + 11) / α-D §3 (4–5 / 7–10); SYNTHESIS does not flag.

Evidence:

- `SYNTHESIS.md:177` — "4 JSON direct admits ... 7 JSON typed admits".
- `HANDOFF.md:80-83` — "11 JSON direct + typed admits (4 + 7)".
- `alpha-A-results-extraction.md:275-280` — table 6 shows nominal
  ADMITTED parse_only=5, direct=6, typed=11, CSS=24; total=46.
- `alpha-A-results-extraction.md:117-122` — explicit reconciliation
  paragraph noting "DISPATCH §1 enumerates '4 JSON direct admits' — the
  6 actually marked ADMITTED in `ROLLING-SOTA-DELTA.md:13-93` are
  citm_catalog, apache_builds, marine_ik, instruments, numbers,
  unicode_basic."
- `alpha-A-results-extraction.md:161-169` — explicit reconciliation
  paragraph noting "DISPATCH §1 enumerates '7 JSON typed admits' — the
  11 enumerated rows exceed that count."
- `alpha-D-validated-invalidated.md:281-291` — same discrepancy
  surfaced in I-3 prose "honest count across `ROLLING-SOTA-DELTA.md
  :13-64` is 6 rows ... dispatch §1 4-direct bind is taken as
  authoritative".
- `alpha-D-validated-invalidated.md:353-368` — "rolling-sota-delta
  additionally lists random +757, instruments +5 254, numbers +1 031
  as typed ADMITTED; these inherit the same comparator misbinding";
  dispatch §1 "7 JSON typed" taken authoritatively for bracket count.

The α-A and α-D agents both surface the numerical disagreement and
both default to the dispatch-context "4 + 7" bind for tally purposes.
SYNTHESIS §1.2 simply inherits "4 + 7" without acknowledging the
disagreement. This is not paper-closure of the audit — the audit
itself is unchanged — but it is paper-closure of an empirical
divergence between the dispatch context's enumeration and the
ROLLING-SOTA-DELTA.md ledger that peer α-agents independently
measured. CH6 binding: "no agent self-report of 'verified' stands
without orchestrator-cited live evidence" — SYNTHESIS's "4 + 7"
inherits dispatch authority without folding the α-A / α-D
counter-measurement.

Required action: SYNTHESIS §0.2 and §1.2 + HANDOFF §3 must add a
discrepancy note ("dispatch §1 cites 4 + 7; α-A / α-D measure 6 + 11
under the broader ROLLING-SOTA-DELTA ledger; both populations
reclassify AUDIT-FALSIFIED under v6 §1 rows 3-4; reconciliation
captured in α-A:117-122 + :161-169") so the SK-V14 PRUNE-1 ledger
revert covers the wider population, not just the narrower 11-row
count the synthesis cites.

### REV-1 — C-4 falsifiability gate is measured but loosely pinned.

Evidence:

- `alpha-E-candidate-shortlist.md:439-444` — "Pick one pre-wave row
  (suggested: `json/numbers/direct_to_struct/main` for the
  numeric-array dispatch divergence). Pre-wave: hot leaf is
  `parse_value_at` per Lock 15 evidence. Post-wave: if W8 chooses a
  number-specialised shape, hot leaf in samply trace is the
  number-specialised symbol. If hot leaf is unchanged, the wave is
  REJECT."

The gate measures hot-leaf attribution change — that is the right CH6
class of measurement (samply symbol path is named). The looseness is
"the number-specialised symbol" not "symbol X named in advance".
Tighten C-4 acceptance to name the specific post-wave symbol the
samply trace must show, so the gate cannot be retroactively
re-targeted.

### REV-2 — HANDOFF next-move chain elides G-Omega.

Evidence:

- `HANDOFF.md:145` — "**Next-move:** `ready-for-CHALLENGE-V1 → G-Alpha → S-P0`."
- `HANDOFF.md:130-140` — §5 step 6 includes G-Omega between S-P1 and
  the wave program; step 9 holds Wave 0 behind both G-S-P0-CONVERGED
  and G-Omega.
- `PASS-ALPHA.md:14` — entry condition "Pass Omega has run for the
  current totality cycle (so V1 spec is current; skinny stays subset
  of totality)."
- `PASS-0-OVERFIT-AUDIT.md:135-146` — "Standing SK process loop"
  places S-P0 first, then Pass Omega T-P1/T-P2/T-P3 concurrent with
  S-P1.

The §6 next-move summary is the operationally consulted line; eliding
G-Omega from it risks a downstream agent reading the one-liner and
dispatching past G-Omega. Restate as
`ready-for-CHALLENGE-V1 → G-Alpha → S-P0 → S-P1/S-P2/S-P3 ∥ Pass Omega → G-Omega → Wave 0 (PRUNE-1)`.

### REV-3 — SYNTHESIS §4 S-P3 constraints omit triumvirate role-separation.

Evidence:

- `SYNTHESIS.md:252-287` — twelve constraint clauses; none names
  research / plan / redress as distinct commits.
- `ORCHESTRATOR.md:209` — "Triumvirate role separation —
  research/plan/redress in distinct commits | CH6" enforcement row.
- The V1 α-phase staging race (REJ-1 above) is precisely the
  consequence of weak commit-discipline; binding the triumvirate
  pattern into S-P3 constraints prevents the same race propagating
  into SK-V14 waves.

Required action: append a thirteenth constraint to SYNTHESIS §4:
"every wave fans out as research → plan → redress in distinct
commits per `[triumvirate-discipline]` + ORCHESTRATOR §8; a wave
that lands a single research-plan-redress mega-commit fails the gate
at S-P3."

## §3 — Recommended folds for V2

For α-F redispatch (the SYNTHESIS + HANDOFF author):

1. Fold REJ-1 — either (a) commit α-A / α-C / α-D / α-F with their own
   `docs(sk-v14-alpha): <tag>` subjects and 5–15 line bodies per
   DISPATCH-CONTEXT.md §3 discipline, OR (b) restate HANDOFF §4 to
   declare α-F as the sole author of all four artefacts and remove
   the "α-A through α-E ... outstanding" framing. Pick (a).
2. Fold REJ-2 — add a discrepancy paragraph to SYNTHESIS §0.2 and §1.2
   citing α-A:117-122 + :161-169 + α-D:281-291 + :353-368; let the
   PRUNE-1 revert ledger cover the wider 6+11 population.
3. Fold REV-2 — restate the HANDOFF §6 next-move line with the
   G-Omega and S-P1/S-P2/S-P3 gates present.
4. Fold REV-3 — append the triumvirate-discipline constraint to
   SYNTHESIS §4.

For α-E redispatch (the candidate shortlist):

5. Fold REV-1 — tighten C-4's pre-/post-wave hot-leaf symbol naming
   to a specific symbol pair (e.g. `parse_value_at` →
   `parse_number_array_specialised` or whatever W11.1 emits) so the
   samply gate is unambiguous.

For aggregator:

6. The contracted §4.4 deferral (SYNTHESIS §0.5) is honest and
   PASS-ALPHA-compliant. The "PENDING until CHALLENGE V1 returns and
   convergence holds" framing in HANDOFF §8 is also contracted, not
   paper-close. Neither needs revision.

CH6 overall verdict: the artefact set is largely substantive — the
honest baseline is rigorously bound, every gate is measured rather
than designed, the §4.4 deferral is contracted, no candidate claims
"complete" / "wired" / "verified" without an attached falsifiability
gate. The two REJECTs trace to the V1 α-phase staging race
(REJ-1: missing peer commits; REJ-2: dispatch-vs-ledger numeric
divergence that SYNTHESIS papered over) — both fix in V2 with author
attribution + a reconciliation paragraph. No deferral to a future
phase is detected outside the contracted §4.4 boundary.
