# SK-V9 S-P2 V1 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V1.
Inputs: `restart/skinny/tranches/sk-v9/research/p2/hardening/V1/CH{1..6}.md`.

## Verdict — V2 fold required (4 of 6 lenses below the bar)

| Lens | ACCEPT-rate | Verdict |
|---|---:|---|
| CH1 CORRECTNESS | 96.7% (59/61, 0 REJECT, 2 REVISE) | ACCEPT |
| CH2 GENERALITY | 80.6% (29/36 ACCEPT, 7 REVISE, 0 REJECT) | REVISE |
| CH3 REGRESSION | 67.4% (29/43 ACCEPT, 11 REVISE, 3 REJECT) | REVISE |
| CH4 COST | 22.7% (10/44, 33 REVISE, 1 REJECT) | REVISE |
| CH5 HIDDEN COUPLING | ACCEPT (33 HONOURED, 7 HONOURED-WITH-CONDITION, 0 VIOLATION) | ACCEPT |
| CH6 ANTI-PAPER-CLOSE | 68% (38 dispositions, 2 critical REJECT) | REVISE |

## Load-bearing defects (must fold before V2 CHALLENGE)

### F1 — P2-D §2.1 wiring claim REJECT (CH6-D-1)

P2-D §2.1 claims `unescape_uxxxx_x4_neon` is "neither wired into the
parse-that-regex hot path". This is wrong: the kernel IS wired at
`parse-that-regex/src/lib.rs:402` inside `unescape_four_unicode_escapes`
(lines 384-459). P2-E correctly identifies the consumer. P2-D must fix.

The downstream implication: P2-D's "consumer wiring as the REDRESS 82
material differential" argument needs reframing — the differential is
not "unwired now, wire it" but rather "x4-only batching is the existing
shape; broaden to all-quartet, change consumer cardinality, etc."

### F2 — P2-E §6.1/§6.2 PMU projection REJECT (CH6-E-3)

P2-E §6.1 c/B baseline column (0.354 / 0.628 / 0.787 / 0.193) does not
reconcile to `/tmp/skv9-xctrace-v3/pmu_rows.tsv`. The TSV has
y_string_unicode/t1 `cycles_per_byte=5.710` and `ns_per_byte=1.466` —
neither matches 0.787. The §6.2 falsifiability projections rest on
fabricated or mis-sourced PMU data. P2-E must:

- Rederive §6.1 from the actual TSV.
- Recompute §6.2 projections.
- The §6.4 verdict (unicode_escapes admits at 100.5%, y_string_unicode
  near-fails at 94.5%, unicode_mixed fails at 68.7%) is suspect until
  the redo.

### F3 — P2-F synthesis overreach (CH3-F-2/F-3/F-6 + CH4 P2-F §7.4)

P2-F §7.4 sequencing table + cumulative impact projection IS S-P3
P3-B/P3-C scope (the S-P1 V4 CH4 failure mode recurring). P2-F §7.2
silently expands codec consumer to "DirectBuild field-fact emit site"
which is REDRESS 66-69 territory. P2-F §7.3 admission 1 is REDRESS 33's
exact rejected shape. P2-F must:

- Reframe §7.4 as a dependency graph (I ← P2-A ← P2-B; III ← P2-A) not
  a sequenced wave plan.
- Strip §7.2 DirectBuild emit-site expansion.
- Strip §7.3 admission 1 / admission 2 (REDRESS reopens).
- §3 "Room to widen the lead" — walk back the synthesis-grade claims;
  defer to S-P3.

### F4 — Per-slice cost discipline (CH4 cross-report)

All reports except P2-B lack per-slice minute caps + revert protocols.
Fold:

- P2-A: 8 intervention slices need per-slice minute cap + revert.
- P2-C: per-slice LOC break-out (currently aggregate-only).
- P2-D: per-opportunity LOC + risk class with explicit "final cost-set
  authored by S-P3" deferral.
- P2-E: per-slice minute caps; E.4 TOML no-consumer disposition.
- P2-F: §7.4 reframe + §7.3 owner resolution (no-owner gap between P2-D
  and P2-A on REDRESS 28+33).

### F5 — Lock-14 surgical reframings (CH2 7 edits)

Mostly paragraph-level. The 7 edits per CH2 §4:
- P2-A §2.5 JSON-role function naming + `json_templates/` codegen-dir
  carve-out language.
- P2-B explicit `AnyGrammar` empty-grammar declaration.
- P2-C cross-grammar transposition prose + Track-2-oracle
  JSON-specificity acknowledgment.
- P2-D §4 string-block-widening explicit Lock-14 framing paragraph.
- P2-E scaffold-vs-production-consumer naming.

### F6 — CH3 REVISEs (11 items + REJECTs)

Tighten falsifiers, add no-regression gates on the W10b six-row block,
walk back P2-F's synthesis overreach (covered by F3), bind P2-D §3
codec wiring to P2-A's landing (no-orphan).

## V2 fold path

Six V2-fold agents, one per P2 report, addressing every disposition
against their report:
- V2-A: F4 cost discipline (P2-A slice caps) + F5 Lock-14 surgical.
- V2-B: F4 (P2-B slice caps) + F5 AnyGrammar declaration.
- V2-C: F4 (P2-C LOC break-out) + F5 cross-grammar prose.
- V2-D: F1 unescape_uxxxx_x4_neon wiring fix + F4 per-opportunity LOC +
  F5 Lock-14 + F6 string-block widening framing.
- V2-E: F2 PMU rederivation (load-bearing) + F4 slice caps + F5 scaffold
  naming + E.4 TOML disposition.
- V2-F: F3 synthesis overreach walk-back (load-bearing) + F4 §7.4
  reframe + F6 owner resolution.

Hardest folds: V2-E (PMU rederivation) + V2-F (synthesis reframe). The
other 4 are surgical.

## Convergence forecast

V2 fold lifts all 4 below-bar lenses if F1-F6 land cleanly. CH1+CH5 are
already converged on V1. V2 CHALLENGE expected to clear all 6.

If V2 CHALLENGE clears ≥95% on all 6: V2 = first qualifying cycle. Need
V3 (re-CHALLENGE) for second consecutive. S-P2 converges at V3.

The S-P1 path was V3→V4→V5→V6 (~6 hours wall-clock). S-P2 is starting
at V1; expect V1→V2→V3 (~3-4 hours wall-clock) given the convergent
substantive findings + clean V2 fold targets.
