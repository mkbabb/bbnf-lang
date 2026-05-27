# Handoff SK-V14

Date: 2026-05-22.

Status: Pass Alpha alpha-F contract handoff, superseded for active dispatch by
the S-P3 SPEC/DISPATCH packet and amended by Pass Omega V3 W2R, Pass
Omega V4 W4R, Pass Omega V5 W5R, Pass Omega V6 W5BR, Pass Omega V7 W5B-GENR,
and Pass Omega V8 W5B-FRONTENDR on 2026-05-26. REDRESS-183 rejected the original W2 dual-tree
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
that shape; Pass Omega V7 W5B-GENR split the receiver into W5B-FRONTEND
frontend/import/IR closure, W5C-GEN provider-free generator body, and
W5D-DELETE provider/template deletion plus Lock 14 baseline close. REDRESS-212
rejected the V7 one-shot W5B-FRONTEND cap shape; Pass Omega V8
W5B-FRONTENDR now splits W5B-FRONTEND into W5B.0 LOCK14-GATE, W5B.1
IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4
REQUEST-CONSUMER. W5B.0..W5B.4, W5C-GEN, and W5D-DELETE have now admitted.
W6.0 admitted only its Lock 14 route gate at `e3c8c8706`; REDRESS-213 rejected
the first CSS L4 root-runtime collapse because the root workspace had no
runtime projection emitter for `crates/core/src/runtime/css_l4/`. W6.0R then
landed the runtime projection correction through `63ec1f0b5`, `7e6e12e9c`,
and `d5599f4ef`. W6.1..W6.8 admitted the remaining root runtime projections
through `b4c47666f`. W7 admitted policy/union runtime wiring at `672b927d5`.
W8 is now rejected by REDRESS-215: the production CSS corpus and
lightningcss/cssparser full-parse probes are available, but all seven post-W7
generated CSS Track 1 profiles still emit fact streams rather than a CSS
full-parse equality plane. W9 then closed mixed under REDRESS-216: 11 / 17
JSON typed rows admit from cold `profile_direct` evidence, all 17 direct rows
were blocked on digest-plane evidence, and 6 / 17 typed rows remained missing
typed product surfaces. W10 then closed mixed under REDRESS-217: 6 / 17 JSON
parse_only rows admit from a distinct no-tape `generated_json::parse_only`
path and cold `profile_direct` evidence; 11 / 17 parse_only rows remained open.
W10R then admitted `canada/parse_only` through the parse-only prefix
continuation source improvement under REDRESS-218, raising parse_only to
7 / 17 admitted and 10 / 17 open. W10S then admitted
`unicode_mixed/parse_only` through the string-end prefix-scan implementation
under REDRESS-219, so parse_only now stands at 8 / 17 admitted and 9 / 17
open. W10T then admitted `instruments/parse_only` from the cold open-row sweep
under REDRESS-220, so parse_only now stands at 9 / 17 admitted and 8 / 17
open. W10V then admitted `citm_catalog/parse_only` from the current-HEAD cold
resweep under REDRESS-222, so parse_only now stands at 10 / 17 admitted and
7 / 17 open. W10W then admitted `apache_builds/parse_only` through the
generated parse-only iterative stack under REDRESS-223, so parse_only now
stands at 11 / 17 admitted and 6 / 17 open. W10X and W10Y/W10Z then rejected
additional parse_only residual routes under REDRESS-224 and REDRESS-225 with
no row movement. W9Y then rejected a generated
`y_string_unicode/real_typed_struct` root under REDRESS-226, so typed remained
11 / 17 admitted and 6 / 17 missing. W9AA then admitted
`distinct_values/real_typed_struct` through generated dynamic string-entry
capture under REDRESS-227, raising typed to 12 / 17 admitted and 5 / 17
missing. W9AB then admitted `canada/real_typed_struct` through generated
numeric lexeme capture under REDRESS-228, raising typed to 13 / 17 admitted
and 4 / 17 missing. W9AC then rejected the generated
`gsoc-2018/real_typed_struct` numeric-key route under REDRESS-229, so typed
remains 13 / 17 admitted and 4 / 17 missing. W10AA then rejected a generated
parse_only fused string/object-loop source route under REDRESS-230, so
parse_only remains 11 / 17 admitted and 6 / 17 open. W11A then admitted
thirteen `direct_to_struct` rows through strict product evidence under
REDRESS-231, so direct now stands at 13 / 17 admitted and 4 / 17 open. W11B
then rejected transient unicode strict products for `unicode_mixed` and
`unicode_escapes` under REDRESS-232; the source patch was reverted, no row
moved, and product-surface-only unicode routes are pre-blocked without a fresh
material differential. W11C then rejected transient `gsoc-2018` strict
products under REDRESS-233 across numeric-key, ordered, identity, full, and
required-full variants; the source patch was reverted, no row moved, and
product-surface-only `gsoc-2018` routes are likewise pre-blocked without a
fresh material differential. W11D then rejected a generated parse_only
value-context delimiter-threading route under REDRESS-234; the source patch
was reverted, no row moved, and context-threaded delimiter consumption is
pre-blocked without a fresh material differential. W11E then rejected a
shared 64-byte JSON whitespace skip route under REDRESS-235; the source patch
was reverted, no row moved, and that full set-member whitespace shape is
pre-blocked without a fresh material differential. W11F then rejected a
generated object-member string/object fast-arm route under REDRESS-236; the
source patch was reverted, no row moved, and that no-carry fast-arm shape is
pre-blocked without a fresh material differential. W11G then rejected a
generated key-string plus colon fusion under REDRESS-237; the source patch was
reverted, no row moved, and no-carry key-colon fusion is pre-blocked without a
fresh material differential. W11H then rejected object-member value-byte carry
under REDRESS-238; the source patch was reverted, no row moved, and key-colon
value-byte carry is pre-blocked without a fresh material differential. W11I
then rejected array comma-to-next-value byte carry under REDRESS-239; the
source patch was reverted, no row moved, and array value-byte carry is
pre-blocked without a fresh material differential. W11J then rejected object
comma-to-next-key specialization under REDRESS-240; the source patch was
reverted, no row moved, and object key-start specialization is pre-blocked
without a fresh material differential.
Continue actual implementation against the remaining residual queue, not
another Omega or Alpha pass unless a future source attempt exposes a real
spec-level amendment.

## 1. Bracket Verdict

SK-V13 closed under audit reversal. The six-agent S-P0 audit pack
(`restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
+ `validation/v{1..6}-*.md`) falsified the SK-V13 admit story: 0 / 43
admitted rows survive strict-vs-strict audit. The architectural
skeleton (W5A / W5B.0..W5B.4 / W5C-GEN / W5D-DELETE / W6 / W7 / `bbnf-simd` / OffsetFlags / Tape /
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
JSON parse_only: 11 / 17 (6 OPEN after W10W residual implementation)
JSON direct:     0 / 17  (17 OPEN after W9; digest-plane evidence is not strict per-corpus product evidence)
JSON typed:      13 / 17 (4 MISSING product surfaces after W9AB)
CSS L4:          0 / 24  (all OPEN after W8; Track 1 still emits fact streams, not full-parse equality)
```

Opening baseline was zero admitted after audit prune. W11 close-state is
mixed: 19 JSON cells are admitted, CSS L4 remains 0 / 24, and all residual
rows are routed to implementation work because no architectural-block proof
closes them.

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

**Next-move:** implement against the W11 residual queue captured in
`restart/skinny/tranches/sk-v14/research/skv14-W11-close.md`. The live row
state is:

- JSON parse_only: 11 / 17 ADMITTED, 6 OPEN under REDRESS-223; REDRESS-230
  rejects the fused string/object-loop residual route without moving any row,
  and REDRESS-234 rejects value-context delimiter threading without moving any
  row. REDRESS-235 rejects a shared 64-byte JSON whitespace skip without
  moving any row. REDRESS-236 rejects object-member string/object fast arms
  without moving any row. REDRESS-237 rejects key-string plus colon fusion
  without moving any row. REDRESS-238 rejects object-member value-byte carry
  without moving any row. REDRESS-239 rejects array value-byte carry without
  moving any row. REDRESS-240 rejects object key-start specialization without
  moving any row.
- JSON direct_to_struct: 13 / 17 ADMITTED, 4 OPEN under REDRESS-231; remaining
  rows lack generated strict product surfaces. REDRESS-232 rejects
  product-surface-only unicode strict products for `unicode_mixed` and
  `unicode_escapes`, and REDRESS-233 rejects product-surface-only
  `gsoc-2018` strict products.
- JSON real_typed_struct: 13 / 17 ADMITTED, 4 MISSING product surfaces under
  REDRESS-216/227/228; REDRESS-226 rejects the generated `y_string_unicode`
  root and REDRESS-229 rejects the generated `gsoc-2018` numeric-key root
  without moving either row. REDRESS-232 rejects and reverts transient
  `unicode_mixed` and `unicode_escapes` products; REDRESS-233 rejects and
  reverts transient `gsoc-2018` products.
- CSS L4: 0 / 24 ADMITTED, 24 OPEN under REDRESS-215.

Per the latest user instruction, do not spend another cycle on Omega/Alpha
governance before implementation. The next implementation packet should select
one residual family and produce source, evidence, RESULTS/DELTA updates, and
REDRESS reconciliation. If that implementation discovers a true spec-level
amendment, route that amendment through the required gate; otherwise keep
moving in implementation mode.

Hard caps echoed per `[dispatch-hard-cap]`: 30-min lens-agent cap;
research 20 min / plan 15 min / redress 30 min (45 min only for the
addendum-amended decision-engine fold + C-4 per CONSOLIDATED §0.5 cap
discipline).

1. W6.0R and W6.1..W6.8 have admitted; do not reopen the PRUNE-4 root runtime
   projection chain unless a later executable gate fails.
2. W7 has admitted the Lock-1 policy/union consumer wiring; W8/W9/W10 may cite
   those generated runtime policy facts but may not cite W7 as row-admit
   evidence.
3. W8 has rejected 0 / 24 CSS L4 rows under REDRESS-215. Any future CSS L4
   admit must provide generated Track 1 CSS full-parse output on the same plane
   as lightningcss/cssparser, not fact-stream output.
4. W9 is closed mixed: 11 typed admits, 17 direct blocks, 6 typed missing
   product blocks. W9Y rejects the generated `y_string_unicode` typed root
   under REDRESS-226, so `y_string_unicode` remains missing. W9AA admits
   `distinct_values/real_typed_struct` under REDRESS-227, so typed now stands
   at 12 admitted and 5 missing. W9AB admits `canada/real_typed_struct` under
   REDRESS-228, so typed now stands at 13 admitted and 4 missing. W9AC rejects
   `gsoc-2018/real_typed_struct` under REDRESS-229, so `gsoc-2018` remains
   missing.
5. W10 is closed mixed: 6 parse_only admits and 11 parse_only open rows under
   REDRESS-217. W10R then admits `canada/parse_only`, W10S admits
   `unicode_mixed/parse_only`, W10T admits `instruments/parse_only`, W10V
   admits `citm_catalog/parse_only`, and W10W admits
   `apache_builds/parse_only`, leaving 6 parse_only open rows under
   REDRESS-223. W10X, W10Y/W10Z, and W10AA reject additional residual routes
   under REDRESS-224, REDRESS-225, and REDRESS-230 without changing the count.
6. W11A admits thirteen strict-product JSON direct rows under REDRESS-231.
   The remaining direct rows are `gsoc-2018`, `unicode_mixed`,
   `unicode_escapes`, and `y_string_unicode`.
7. W11B rejects transient `unicode_mixed` and `unicode_escapes` strict product
   routes under REDRESS-232. The products were correct but missed strict sonic
   by at least 2014.202 Mbps, so no source patch lands and no row moves.
8. W11C rejects transient `gsoc-2018` strict product routes under REDRESS-233.
   Numeric-key, ordered, identity, full, and required-full products were
   correct but all missed strict sonic, so no source patch lands and no row
   moves.

## 7. Refusal Conditions

Return REVISE for any downstream plan that:

- dispatches any implementation wave before G-Omega + G-S-P0-CONVERGED, or
  dispatches W3+ before Pass Omega V3 W2R CRUD and amended W2 admission, or
  dispatches W5 before Pass Omega V4 W4R CRUD and amended W4 ledger close, or
  dispatches W5B.0 before Pass Omega V8 W5B-FRONTENDR CRUD and amended SPEC
  close, or dispatches W5B.1..W5B.4 before W5B.0 admits, or dispatches W5C-GEN
  before all W5B.0..W5B.4 sub-waves admit, or deletes providers/templates
  before W5D-DELETE;
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
  ADMIT, architectural-block proof, or a W11 residual implementation route;
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

## 8. W11 Close Disposition

SK-V14 closes as a mixed implementation tranche. It preserves 17 admitted JSON
cells and routes the remaining JSON and CSS rows to implementation residuals.
No residual row has an architectural-level intrinsic-block proof. Subsequent
work inherits R1-R10 until a row family achieves admission or proves an
architectural block, but the immediate continuation is source implementation
against the W11 residual queue. REDRESS-232 makes unicode product-surface-only
retries non-viable without a fresh material differential; REDRESS-233 applies
the same pre-block to `gsoc-2018` product-surface-only retries. REDRESS-234
pre-blocks parse_only context-threaded delimiter consumption without a fresh
material differential. REDRESS-235 pre-blocks full 64-byte JSON whitespace
set-member skipping without a fresh material differential. REDRESS-236
pre-blocks parse_only object-member string/object fast arms without a fresh
material differential. REDRESS-237 pre-blocks parse_only key-colon fusion
without a fresh material differential. REDRESS-238 pre-blocks parse_only
object-member value-byte carry without a fresh material differential.
REDRESS-239 pre-blocks parse_only array value-byte carry without a fresh
material differential. REDRESS-240 pre-blocks parse_only object key-start
specialization without a fresh material differential.
