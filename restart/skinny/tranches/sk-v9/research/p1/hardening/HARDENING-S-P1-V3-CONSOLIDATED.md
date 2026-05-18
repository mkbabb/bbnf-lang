# SK-V9 S-P1 V3 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V3 (post-PMU-unblock).
Lens set: CH1–CH6 per `restart/prompts/ORCHESTRATOR.md` §3W.
Inputs: `restart/skinny/tranches/sk-v9/research/p1/hardening/V3/CH{1..6}.md`.

## Verdict — V3 does not converge

Per `ORCHESTRATOR.md` §3Z, S-P1 advances only when CHALLENGE returns
≥95% ACCEPT for two consecutive cycles. V3 cleared 95% on **one** lens
(CH5), so V3 alone cannot count as a qualifying cycle. V4 fold is
required.

| Lens | ACCEPT-rate (strict) | Dispositions | Verdict |
|---|---:|---:|---|
| CH1 CORRECTNESS | 67.3% (94.2% lenient) | 35 A / 14 REVISE / 3 REJ | REVISE |
| CH2 GENERALITY | 33% (2 of 6 reports) | 47 total — A & F ACCEPT, B/C/D/E REVISE | REVISE |
| CH3 REGRESSION | 91.7% (97.2% if WATCH counts) | 33 A / 2 WATCH / 2 REVISE / 0 REJ | CONDITIONAL ACCEPT |
| CH4 COST | 14.3% | 5 A / 26 REVISE / 4 REJ | REVISE |
| CH5 HIDDEN COUPLING | 95.6% | 43 A / 2 REVISE / 0 REJ | ACCEPT |
| CH6 ANTI-PAPER-CLOSE | 88.6% | 31 A / 4 REVISE | REVISE |

Aggregate across ~250 dispositions: ~78% strict / ~92% lenient. Below
the §3Z bar on four of six lenses.

## What V3 closed honestly

- PMU is real: 34 trace bundles + 34-row `pmu_rows.tsv` on disk; xctrace
  is contract-admitted (V2 was infrastructure-bound, not
  contract-bound); the V2 P1-D BLOCKED axis is closed.
- `scan_structurals` 0.00% self-time on every (corpus, track) row —
  SC-1's "discarded" claim is the strongest form of true.
- `match_tiny_plain_string + match_string_at_quote` is 47–67% on dense-
  key losses (sharper than SC-4's 75% claim).
- `y_string_unicode` bottleneck is unicode-escape codec — a class SC-4
  missed.
- V2 samply mode-I `dispatch_value` 95–99% is a frame-pointer-coalescing
  artefact — falsified at symbol level on every row.
- The SC-4 step function (`q_frac ≤ 0.135` wins, `≥ 0.726` loses)
  reconfirmed on SK-V9-open.
- 524 docs + ~700 LOC orphan kernels triaged for SAFE-TO-DELETE with
  REDRESS citations.

## V4 fold requirements (load-bearing)

Each fold below targets the precise CHn finding(s) that drove the lens
below 95%.

### F1 — Strip S-P1-overreach wave proposals (CH4 root cause)

P1-V3-D §6.6 "three V9/V10 waves, ranked" pre-empts S-P3. S-P1 is
profile + excavation; wave authorship is S-P3 scope per
`PASS-3-SYNTHESIS-PLAN.md`. Fold:

- Remove §6.6 from P1-V3-D verbatim.
- Replace with a single paragraph naming the diagnostic findings
  (per-quote ~21× baseline cost; 10% cut → 7/11 losers cleared; unicode
  needs separate validation kernel) and stating that wave-class
  selection + cost set are S-P3 scope.
- Mirror the strip in P1-V3-C §recommendations and P1-V3-E
  recommendations.

### F2 — Refold P1-V3-C with A/B PMU truth (CH1 + CH6 root cause)

P1-V3-C ran before P1-V3-A/B landed and never refolded. The result:
A and C still cite samply mode-I `dispatch_value 95–99%` (the
frame-pointer artefact B falsified). Fold:

- Re-execute P1-V3-C with the on-disk A `/tmp/skv9-xctrace-v3/p1a/`
  PMU + B `/tmp/skv9-xctrace-v3/p1b-tp/exports/` Time Profiler symbols
  as primary inputs; samply as cross-validation only.
- Resolve the `match_tiny_plain_string` zero-appearances contradiction:
  B shows rank-1 at 46.2% / 61.9% on twitter / distinct_values track1.
- Replace "to refine after xctrace lands" placeholders with refolded
  attribution.

### F3 — Cite REDRESS material differentials (CH3 root cause)

P1-V3-D §6.1 "masked bitmap pass + deferred escape-complete" soft-
reopens REDRESS 60/61/62/83/84. §6.3 V10 unicode soft-reopens
REDRESS 59/82. Fold:

- Cite each REDRESS entry per soft-reopen.
- State the material differential vs the rejected shape, or demote to
  deferred hypothesis.
- Reframe "redesign" language in §6.5 (W2 digest-sink) to "profile the
  digest path" per REDRESS 66–69 + 93.

### F4 — Lock-14 reframing of JSON-role symbols (CH2 root cause)

P1-V3-B/C/D/E carry JSON-named symbol identities in classifier outputs
(`match_tiny_plain_string`, `read_hex_unit_scalar`, `\uXXXX`) without
generalising to the primitive class. Fold:

- Rename per-symbol attribution buckets to substrate-neutral primitive
  classes (per-string-span scanner, escape-codec hex-unit,
  structural-element walker). Use P1-V3-B's classifier vocabulary as
  the canonical naming.
- Reframe `\uXXXX` as the cross-grammar `escape_codec_hex_unit`
  primitive parameterised by `{hex_digit_count, surrogate_join_policy,
  terminator_policy}` (admits CSS L4 `\HHHHHH`).
- Split P1-V3-E's SAFE-TO-DELETE into "corpus-scoped consumer status"
  + "primitive-class status" columns so deletions don't masquerade as
  class-retiring.

### F5 — CH1 specific defects + CH6 paper-close fixes

Surgical edits, not re-authors:

- P1-V3-A §y_string_unicode 4.4% residual — remove (samply artefact;
  B falsifies).
- P1-V3-A distinct_values c/B arithmetic — typo 2.88 → 3.85.
- P1-V3-A §1.3 / §5 TP path citation —
  `/tmp/skv9-xctrace-v3/p1a-time-profile/` →
  `/tmp/skv9-xctrace-v3/p1b-tp/`.
- Corpus-name shear — `update-center` (A's TSV) vs `update_center`
  (B's exports). Reconcile to one canonical spelling across both.
- P1-V3-D regression provenance — commit the script that produced
  `ns_per_byte = 8.64·(q/B) + 1.47·(n/B) + 0.410` + R² + residuals.
- P1-V3-F edit-count rollup — 20 not 19.
- P1-V3-F strictness-plane assertion — make explicit.
- P1-V3-F drops the proposed edit to `prompts/skinny/PASS-1-PROFILE.md`
  (orchestrator-scope violation per `ORCHESTRATOR.md` §7).
- P1-V3-E split into E1 (doc, ≤30 min, low-risk) + E2 (code, ≤45 min
  + `cargo test --workspace` gate, medium-risk).
- P1-V3-E simd-scan/ "empty directory" claim — directory does not
  exist; rephrase or remove.

### F6 — Lock-1 binding sentences (CH5 residual)

Two REVISE items kept CH5 at 95.6%; folding them lifts to 100%:

- P1-V3-D §6.1 explicit Lock-1 binding: the proposed string-plane
  bitmap REPLACES `match_tiny_plain_string_with_cap` /
  `match_string_at_quote_trusted_utf8` on the production hot path —
  not alongside; substrate cardinality stays at one.
- P1-V3-D §6.1 "deferred escape-complete check" — name as inline within
  the same SIMD pass OR strictly `#[cfg(test)]` diagnostic-only.

## Required next moves

1. Apply F1–F6 surgical/refold edits to V3 reports (in place; git
   preserves V3 history).
2. Commit V4 fold as `docs(sk-v9-p1-v4): fold V3 CHALLENGE dispositions`.
3. Re-dispatch CHALLENGE V4 (six CH1–CH6 agents).
4. If V4 CHALLENGE ≥95% on all six lenses: V4 = one qualifying cycle.
   Run a V5 (or V4.2) re-CHALLENGE without substantive change for the
   second consecutive qualifying cycle. Convergence at V5.
5. If any lens stays below 95%: another fold to V5; CHALLENGE V5; etc.
   §3Z hard ceiling V ≤ 5.

## Convergence forecast

V3 → V4 fold targets every cited defect mechanically; the load-bearing
risks are (a) F2's C refold producing fresh contradictions with A/B
(unlikely — A/B are PMU-grounded), and (b) F3's REDRESS-citation work
requiring fresh measurement evidence we do not have (mitigated by
demoting to deferred hypothesis instead of admitting).

V5 second-consecutive cycle is mechanical given F1–F6 land cleanly. The
~6-hour V3→V5 wall-clock is the cost of §3Z discipline; absent it, S-P1
ships paper-close.
