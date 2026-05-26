# Handoff SK-V14

Date: 2026-05-22.

Status: Pass Alpha α-F contract handoff, superseded for active dispatch by
the S-P3 SPEC/DISPATCH packet and amended by Pass Omega V3 W2R, Pass
Omega V4 W4R, Pass Omega V5 W5R, Pass Omega V6 W5BR, and Pass Omega V7 W5B-GENR on 2026-05-26. REDRESS-183 rejected the original W2 dual-tree
round-trip; W2 admitted as skinny-side `regen-css` only at commit
`45568e669`, while `crates/core/src/runtime/css_l4/` remains W6.0 work after
W5D-DELETE. W3 admitted the production CSS L4 corpus loader at `b0a864f0b`.
REDRESS-184 rejected the original W4 provider-deletion gate; W4R closed the
ledger-only CSS L4 PRUNE with REDRESS-185..208. REDRESS-209 now rejects W5's
current provider-collapse gate because the source-consuming generic generator
does not yet exist; Pass Omega V5 W5R closed the correction by splitting W5
into W5A source-consuming generator capability and W5B provider/template
deletion. REDRESS-210 rejected W5B deletion because W5A admitted only the
request boundary while live provider-backed generation remained; Pass Omega V6
W5BR split the remaining receiver into W5B-GEN provider-free generator body
and W5C-DELETE provider/template deletion. REDRESS-211 rejected W5B-GEN under
that shape; Pass Omega V7 W5B-GENR now splits the receiver into W5B-FRONTEND
frontend/import/IR closure, W5C-GEN provider-free generator body, and
W5D-DELETE provider/template deletion plus Lock 14 baseline close.

## 1. Bracket Verdict

SK-V13 closed under audit reversal. The six-agent S-P0 audit pack
(`restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
+ `validation/v{1..6}-*.md`) falsified the SK-V13 admit story: 0 / 43
admitted rows survive strict-vs-strict audit. The architectural
skeleton (W5A / W5B-FRONTEND / W5C-GEN / W5D-DELETE / W6 / W7 / `bbnf-simd` / OffsetFlags / Tape /
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
- **JSON direct + typed admits (dispatch §1 cites 4 + 7; α-A / α-D
  measure 6 + 11 under the broader `ROLLING-SOTA-DELTA.md:13-93`
  ledger)** — REAL parsers, comparator misbinding:
  `sonic_rs::from_slice::<Value>` eager DOM instead of strict per-corpus
  struct deser (`v2 §3.2 + §4.2`; `v6 §3`). Both populations
  reclassify AUDIT-FALSIFIED under v6 §1 rows 3-4; PRUNE-1's ledger
  revert binds the wider 6+11 population (direct +2: marine_ik,
  instruments; typed +4: random, instruments, numbers, unicode_basic
  via W13.1/.2/.3/.4 plus update_center W15.1 adjusted), not the
  narrower 4+7 the dispatch summarises. Reconciliation captured at
  α-A:117-122 + α-A:161-169 + α-D:281-291 + α-D:353-368.
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
CSS L4:          0 / 24  (all OPEN; templates pending PRUNE-2; amended skinny-side xtask pending W2 rerun)
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
- Pass Alpha V1 α-A / α-C / α-D / α-F artefacts in tree were authored
  by α-F under the `DISPATCH-CONTEXT.md §α-F` fall-through clause;
  α-B and α-E carry their own peer commits (`e4870b201` and
  `86dbd6b09` respectively). The V2 redispatch wave generates fresh
  per-agent commits for α-A, α-C, α-E via the orchestrator's atomic
  commit on V2 cycle close; the V1 staging-race ambiguity is resolved
  by naming α-F as the sole V1 author of α-A / α-C / α-D and the
  SYNTHESIS + HANDOFF pair, per CH6 §2.2 REJ-1 disposition (b)
  (CONSOLIDATED §2.1 F-2). CHALLENGE V1 caught the divergence; V2
  reconciles.
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

**Next-move:** Dispatch W5B-FRONTEND PRUNE-3B under the amended SPEC §8B. W5A
admitted the source-consuming request boundary at `286233fa2`; W5B-FRONTEND
now closes generic BBNF grammar-source frontend/import/IR lowering, with CSS L4
compatibility constructs such as `@ws`, `@pretty`, `?w`, `>>`, `<<`, span
capture, typed host projections, and imports lowering into canonical IR rather
than new public syntax. W5C-GEN inherits provider-free generator body
replacement only after W5B-FRONTEND admits, W5D-DELETE inherits deletion only
after W5C-GEN admits, and W6.0 `crates/core/src/runtime/css_l4/`, W7, and
W8/W9/W10 remain blocked until the amended PRUNE-3/4/5 chain closes.

Hard caps echoed per `[dispatch-hard-cap]`: 30-min lens-agent cap;
research 20 min / plan 15 min / redress 30 min (45 min only for the
addendum-amended decision-engine fold + C-4 per CONSOLIDATED §0.5 cap
discipline).

1. Dispatch W5B-FRONTEND research; Pass Omega V7 G-Omega is closed and
   CRUD/SPEC patches have landed.
2. Before W5B-FRONTEND source redress, add explicit
   `lock14_baseline.rs` owner-path and parent-diff subject routing for
   `sk-v14-waveW5B-FRONTEND` / `sk-v14-waveW5B-FRONTEND-redress`.
3. Preserve W2/W3/W4/W5A/W5B-FRONTEND/W5C-GEN/W5D-DELETE root-runtime exclusion:
   `crates/core/src/runtime/css_l4/` remains W6.0 and must not be claimed
   before W6.0.
4. Delete CSS provider/template directories only in W5D-DELETE, after W5C-GEN
   admits the provider-free generator body and the amended W5D-DELETE
   verification gates authorise the deletion.

## 7. Refusal Conditions

Return REVISE for any downstream plan that:

- dispatches any implementation wave before G-Omega + G-S-P0-CONVERGED, or
  dispatches W3+ before Pass Omega V3 W2R CRUD and amended W2 admission, or
  dispatches W5 before Pass Omega V4 W4R CRUD and amended W4 ledger close, or
  dispatches W5B-FRONTEND before Pass Omega V7 W5B-GENR CRUD and amended SPEC
  close, or deletes providers/templates before W5D-DELETE;
- skips PRUNE-1 / PRUNE-2 baseline revert in favour of new admit attempt;
- counts a row as admitted under the misbound `sonic_rs::from_slice::<Value>`
  comparator (P-2);
- treats `parse_only` as diagnostic-only;
- inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only
  + 6 direct + 11 typed = **47 rows** under the broader
  `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower
  bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
  §0.2` reconciliation block (lines 73-84)) as carry-over without fresh material
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
  provider modules in generic codegen; Track 1 ≡ Track 2 plane collapse;
- reopens W10.3 nested_layout a second time within a tranche without
  user re-pin + intrinsic-block evidence; equivalently, admits any
  future CSS feature whose claimed Mbps exceeds the same-plane SOTA
  comparator by ≥ 50× without the same round-trip-rule trigger
  (per `SYNTHESIS.md §0.4 P-1` + α-C §4);
- proposes `UnionTape`, a second tape, a public substrate API, a
  retained class/mask stream, or parser-owned cursor/list state; only
  G-Omega may amend Lock 1's substrate-union closure
  (`LOCKS.md:73-82`).

## 8. Pass Alpha Bracket V1 Disposition

V1 disposition is **PENDING** until CHALLENGE V1 returns and convergence
holds per §3Z. The SK-V14 contract is a draft until G-Alpha closes.

After G-Alpha, the contract is binding through SK-V14 close. Subsequent
brackets to SK-V15+ inherit R1–R10 verbatim unless a row family achieves
admit or proves architectural-level intrinsic-block.
