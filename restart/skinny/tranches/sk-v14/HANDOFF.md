# Handoff SK-V14

Date: 2026-05-22.

Status: Pass Alpha α-F contract handoff. Do not dispatch SK-V14 Wave 0
from this file alone. `SPEC.md` and `DISPATCH-PROMPT.md` are intentionally
absent and authored downstream by skinny pass S-P3 after the required
G-Omega gate and the S-P0 Overfit Audit Pass for the new tranche.

## 1. Bracket Verdict

SK-V13 closed under audit reversal. The six-agent S-P0 audit pack
(`restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
+ `validation/v{1..6}-*.md`) falsified the SK-V13 admit story: 0 / 43
admitted rows survive strict-vs-strict audit. The architectural
skeleton (W5 / W6 / W7 / `bbnf-simd` / OffsetFlags / Tape /
`generated_json::parse_direct` / `generated_real_typed::parse_*` / 15
unwired CSS `.bbnf` grammars) holds. The admit machinery does not.

SK-V14 opens **prune-then-rebuild**. The SK-V14 contract is at
`restart/skinny/tranches/sk-v14/SYNTHESIS.md`. The R-target goalset
(R1–R10) is verbatim from
`restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`, with §0.4
pattern-level pre-blocks (P-1 … P-7) drawn from the validation pack.

## 2. Authority List

Read in this order:

1. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
2. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`
3. `restart/skinny/tranches/sk-v14/SYNTHESIS.md`
4. `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
5. `restart/skinny/tranches/sk-v13/audit-overfit/validation/v{1..6}-*.md`
6. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`
7. `restart/prompts/pass-contracts/PASS-ALPHA.md`
8. `restart/prompts/ORCHESTRATOR.md`
9. `restart/locks/LOCKS.md` (V1.1; Lock 14 + CH7 lens binding)
10. `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
11. `restart/skinny/tranches/sk-v13/HANDOFF.md`
12. `skinny/RESULTS.md`
13. `skinny/REDRESS.md`
14. `restart/skinny/ROLLING-SOTA-DELTA.md` (requires honest re-baseline
    post-PRUNE-1 + PRUNE-2)

The binding addendum + the SK-V14 fresh-session orchestrator prompt
control conflicts. PRUNE waves precede every new-admit attempt; comparator
rebind precedes every re-baseline; G-Omega precedes Wave 0; the campaign
continues indefinitely until full ADMIT or per-row architectural-block
proof covers everything.

## 3. Honest Baseline Summary

### Survives (eight pillars carry forward)

- W5 bbnf-regex extraction — LOAD-BEARING
  (`audit-overfit/validation/v4-decision-engine-trace.md §1`).
- W6 e-graph Language + cost — LOAD-BEARING, extraction-only
  (`v4 §1`).
- W7 CSP solver, 5 constraints, fail-closed — LOAD-BEARING
  (`v4 §1 + §2`; `skinny/crates/passes/lib.rs:476–478`).
- `bbnf-simd` (52 files) — grammar-neutral (`v3 §4`).
- OffsetFlags + Tape — grammar-neutral (`v3 §2`).
- `generated_json::parse_direct` — real codegen from grammar
  (`v2 §3.1`).
- `generated_real_typed::parse_*` — real codegen from grammar
  (`v2 §4.1`).
- 15 CSS `.bbnf` grammars at `/grammar/css/l4/` — present, unwired
  (R4 makes load-bearing).

### Does not survive (four falsifications)

- **25 CSS L4 admitted rows** incl. SK-V12 W1b 2.54× headline —
  hand-written templates with fake `@generated` header; no `regen-css`
  xtask (`v1 §1 + §2 + §5`).
- **5 JSON `parse_only` admits (W14.1–.5)** — gate-relabel only; source
  diffs touch `gate.rs` / `report.rs` / `lock14_baseline.rs` and not the
  parser; comparator misnamed (`v2 §1 + §2`).
- **11 JSON direct + typed admits (4 + 7)** — REAL parsers, comparator
  misbinding: `sonic_rs::from_slice::<Value>` eager DOM instead of
  strict per-corpus struct deser (`v2 §3.2 + §4.2`; `v6 §3`).
- **W8 per-grammar policy + W9 same-substrate union** — COSMETIC; zero
  runtime consumption (`v4 §4 + §5 + §6`).

Plus **30 Lock 14 violations** (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW;
codex undercount by 43 %). 8 hand-written per-grammar provider modules
under `skinny/crates/codegen/` are the recurrence vector (`v3 §1`;
Lock 14 at `restart/locks/LOCKS.md:220–238`).

### Honest rolling delta (SK-V14 starting baseline)

```
JSON parse_only: 0 / 17  (all OPEN)
JSON direct:     0 / 17  (4 comparator-rebind candidates; 13 fresh)
JSON typed:      0 / 17  (7 comparator-rebind candidates; 10 fresh)
CSS L4:          0 / 24  (all OPEN; templates deleted + xtask built)
```

Campaign at zero on numbers; non-zero on architecture. The
`restart/skinny/ROLLING-SOTA-DELTA.md` table currently at commit
`653cdf795+w15.1-redress` requires re-baseline through PRUNE-1 + PRUNE-2.

## 4. Pre-S-P0 Readiness

- Working tree clean (SK-V14 doc seeds + audit JSONL edits only).
- `restart/skinny/tranches/sk-v14/` seeded:
  - `ORCHESTRATOR-PROMPT.md` (commit `496a81417`).
  - `research/alpha/DISPATCH-CONTEXT.md` (commit `6ab711d77`).
  - `SYNTHESIS.md` + `HANDOFF.md` (this commit).
- Pass Alpha α-A through α-E remain outstanding at α-F commit time;
  α-F synthesised directly from raw sources per
  `DISPATCH-CONTEXT.md §α-F` fall-through clause. CHALLENGE V1 catches
  divergence; V2 reconciles.
- Audit pack is at the latest commit `b24232776` (cross-tranche stability
  + pattern emergence); validation pack is six files end-to-end.
- USER PIN ADDENDUM 2026-05-21 + USER PIN W1 CSS L4 SOTA 2026-05-20 are
  in force; REDRESS-119 + REDRESS-120 are HISTORY only per addendum.

## 5. Pass Sequence

1. Pass Alpha α-A through α-F commit + CHALLENGE V1 dispatch (CH1–CH6
   adversarial review per `PASS-ALPHA.md §3` + ORCHESTRATOR.md §3W).
2. CHALLENGE V{N} folds into V{N+1} until ≥95 % ACCEPT for two
   consecutive cycles per `ORCHESTRATOR.md §3Z`; V ≤ 5 ceiling.
3. G-Alpha user sign-off per `ORCHESTRATOR.md §6` (SK-V14 contract
   pinned for downstream).
4. S-P0 Overfit Audit Pass dispatch (6 fresh agents A1–A6 per
   `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`). CH7 Overfit-Prune
   lens binding. Gate G-S-P0-CONVERGED before S-P1 dispatches.
5. Pass Omega T-P1 / T-P2 / T-P3 concurrent with skinny S-P1 (fresh
   PMU + corpora capture under rebound comparators) + S-P2 (cohort
   research, CH1–CH7 lensed).
6. G-Omega user gate (locks-diff + spec deltas mandatory).
7. Totality CRUD execute.
8. S-P3 Synthesis-Plan authors `sk-v14/SPEC.md` +
   `DISPATCH-PROMPT.md` from G1–G7 + R1–R10 + this §0 goalset.
9. SK-V14 wave program — PRUNE waves (C-5 → C-1 → C-2 → C-3 → C-4) FIRST;
   new-admit waves (R6 + R7 + R8) only after PRUNE converges and
   `ROLLING-SOTA-DELTA.md` is restated to the §3 honest baseline.
10. Pass Alpha close + bracket → SK-V15 if R1–R10 not fully admitted /
    architecturally blocked.

## 6. Next-Move

**Next-move:** `ready-for-CHALLENGE-V1 → G-Alpha → S-P0`.

1. Dispatch Pass Alpha CHALLENGE V1 wave (CH1–CH6) over the SYNTHESIS +
   HANDOFF + α-A through α-E artefacts. CH5 reviews Track 1 ≡ Track 2
   plane integrity; CH3 reviews the pre-block list against REDRESS;
   CH2 verifies Lock 14 grammar-neutrality of every proposed candidate.
2. Aggregator authors `restart/skinny/tranches/sk-v14/research/alpha-
   hardening/V1/HARDENING-ALPHA-V1-CONSOLIDATED.md`.
3. Fold dispositions into V2 if not converged; iterate to V ≤ 5.
4. At convergence (≥95 % ACCEPT × 2 cycles), present SK-V14 contract for
   G-Alpha sign-off.
5. After G-Alpha, dispatch S-P0 Overfit Audit Pass for SK-V14 (6 fresh
   agents in parallel; CH7 lens binding). The S-P0 pass is now a
   permanent first phase of every skinny tranche per
   `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`.
6. Hold Wave 0 (PRUNE-1 dispatch) behind both G-S-P0-CONVERGED and
   G-Omega.

## 7. Refusal Conditions

Return REVISE for any downstream plan that:

- dispatches any implementation wave before G-Omega + G-S-P0-CONVERGED;
- skips PRUNE-1 / PRUNE-2 baseline revert in favour of new admit attempt;
- counts a row as admitted under the misbound `sonic_rs::from_slice::<Value>`
  comparator (P-2);
- treats `parse_only` as diagnostic-only;
- inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only
  + 4 direct + 7 typed = 41 rows) as carry-over without fresh material
  differential under rebound comparator;
- counts lossy / permissive comparators (sonic-rs lossy / RapidJSON /
  asmjson SWAR) as SOTA anchor;
- leaves any CSS feature `PARTIAL` at close;
- adds support-only primitives, union substrates, resolver
  infrastructure, or codegen paths without a same-wave measured
  consumer;
- inherits weaker scoping labels (`optional`, `fallback`, `diagnostic`,
  `support-only`, `scaffold-only`, `future-tranche`) for pinned R1–R10
  work instead of converting them to admitted row targets,
  architectural-block proofs, or user re-pin issues;
- authorises a new directive, BIR variant, `BackendShape`, public
  substrate API, or grammar-specific generic behaviour through
  SPEC-local wording (Lock 14 binding);
- wires `bbnf-simd` into CSS, union, JSON `parse_only`, or shared
  generated code without `G-SIMD-GRAMMAR-POLICY` proving the consuming
  grammar's quote/escape/control policy or a no-string policy, plus
  scalar parity, checkasm/differential coverage, same-wave row
  measurement, no public substrate API, no sidecar classifier state;
- lets the hardcoded P1–P8 cascade silently serve JSON / CSS / Sheets /
  BBNF-self rows after the resolver fold rather than failing closed
  with visible rejection / non-admission;
- allows source / gate edits without telemetry and rolling delta
  updates;
- closes a tranche with implementation-limited misses instead of full
  ADMIT, architectural-block proof, or immediate bracket to SK-V15;
- introduces any of patterns P-1 through P-7
  (`SYNTHESIS.md §0.4`) — fake `@generated` header on hand-written
  output; mislabelled eager-DOM comparator; tiny-fixture Mbps inflation;
  gate-relabel as admit; scaffold-only as load-bearing; per-grammar
  provider modules in generic codegen; Track 1 ≡ Track 2 plane collapse.

## 8. Pass Alpha Bracket V1 Disposition

V1 disposition is **PENDING** until CHALLENGE V1 returns and convergence
holds per §3Z. The SK-V14 contract is a draft until G-Alpha closes.

After G-Alpha, the contract is binding through SK-V14 close. Subsequent
brackets to SK-V15+ inherit R1–R10 verbatim unless a row family achieves
admit or proves architectural-level intrinsic-block.
