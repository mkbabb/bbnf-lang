# T-P2 V1 CH3 Regression / REDRESS Review

Pass: T-P2 Totality Research.
Cycle: V1.
Lens: CH3 regression / REDRESS.
Agent: CH3.
Date: 2026-05-21.
Ownership: `restart/audit/totality/p2/hardening/V1/CH3.md` only.

## Verdict

REVISE.

The V1 dossiers mostly honor the REDRESS ledger: REDRESS 88/89 PMULL/CSSC,
REDRESS 96/97/98 retained union, REDRESS 122 escape-mask prerequisite,
REDRESS 126 ASCII run-skip production split, and REDRESS 127 SK-V12 close are
not treated as admitted behavior routes. However, V1 is not CH3-clean because
two route families are still described too broadly for T-P3 to scope safely:

1. REDRESS-119 direct residual reopening is stated as generally eligible under
   "new material differentials from decision engine, union, and ASM routes"
   without a row-by-row differential ledger.
2. The PMULL+CSSC / Union-C shorthand risks composing three measured failures
   (REDRESS 88, 89, and 96/97/98) into a new label before the dossier pins the
   consumer-level differential that makes it unlike those failures.

This is a V1 research-fold problem, not a REJECT. The dossiers already contain
the needed fences in scattered form; V2 must make those fences mechanical and
row/consumer-specific.

## Evidence

### REDRESS Ledger Facts CH3 Treats As Binding

- REDRESS 88 rejected PMULL as a default hot `bitmap_prefix_xor_64` body after
  correctness/checkasm/disasm passed but JSON row regressions appeared
  (`skinny/REDRESS.md:2510`, `skinny/REDRESS.md:2527`,
  `skinny/REDRESS.md:2535`).
- REDRESS 89 rejected the narrowed CSSC CTZ / bulk consumer after checkasm and
  explicit `ctz` disasm proof because parse-only guard rows regressed
  (`skinny/REDRESS.md:2544`, `skinny/REDRESS.md:2565`,
  `skinny/REDRESS.md:2573`).
- REDRESS 96 and 97 rejected two correctness-green retained union variants:
  full class-column + move-consumed vector and allocation-free streaming cursor.
  Both missed every must-improve and maintain row (`skinny/REDRESS.md:2797`,
  `skinny/REDRESS.md:2823`, `skinny/REDRESS.md:2852`,
  `skinny/REDRESS.md:2881`).
- REDRESS 98 retires the SK-V9 union-substrate thesis for that shape family:
  retained class/cursor substrate adds parse-loop traffic and cursor
  indirection on the M5 Max (`skinny/REDRESS.md:2910`,
  `skinny/REDRESS.md:2928`, `skinny/REDRESS.md:2934`).
- REDRESS 119 closed the 13 direct residual rows as measured fixpoint, not GO,
  with no source intervention or row movement (`skinny/REDRESS.md:3497`,
  `skinny/REDRESS.md:3506`).
- REDRESS 122 is a correctness prerequisite only; it made no production
  scanner, SIMD body, gate, RESULTS, or row admission change
  (`skinny/REDRESS.md:3605`, `skinny/REDRESS.md:3629`).
- REDRESS 123/124/125 progressively scaffolded, compared, and then admitted a
  CSS candidate only after strict fact-stream equality plus lightningcss+1
  gate consumption; 123 and 124 were not SOTA admissions
  (`skinny/REDRESS.md:3636`, `skinny/REDRESS.md:3683`,
  `skinny/REDRESS.md:3720`).
- REDRESS 126 is explicitly `ROUTE-PRODUCTION-SPLIT`, not production SIMD/ASM
  admission; it did not wire CSS, did not claim strict CSS fact-stream
  equality, and did not claim a same-wave production consumer
  (`skinny/REDRESS.md:3768`, `skinny/REDRESS.md:3800`).
- REDRESS 127 admits SK-V12 by the CSS row, but separately records union as
  unblocked for future materially differentiated attempts and W4 ASM-gen as a
  routed production split, not retroactive production work
  (`skinny/REDRESS.md:3824`, `skinny/REDRESS.md:3860`,
  `skinny/REDRESS.md:3864`).

### What V1 Gets Right

- 2A correctly says simdjson stage-1 evidence allows transient mask production,
  not retained class-column replay (`restart/audit/totality/p2/2A-sota-landscape.md:42`).
- 2A correctly flags SIMD parity or microbench-only landings as non-admitting,
  citing SK-V12 W2 and W4 prerequisite-only evidence
  (`restart/audit/totality/p2/2A-sota-landscape.md:75`).
- 2B correctly classifies escape mask as mandatory correctness gate, not row
  movement (`restart/audit/totality/p2/2B-primitive-vocabulary.md:111`).
- 2B correctly states structural masks are transient facts and do not justify
  replaying class-column, streaming-cursor, parser-owned sidecar, or parallel
  `UnionTape` routes (`restart/audit/totality/p2/2B-primitive-vocabulary.md:173`).
- 2B correctly says REDRESS-126 orphan demotion is not a perfected zero-orphan
  close under the addendum (`restart/audit/totality/p2/2B-primitive-vocabulary.md:204`).
- 2C correctly treats primitive parity alone as non-admission and requires a
  same-wave CSS/JSON/Sheets/BBNF-self consumer
  (`restart/audit/totality/p2/2C-grammar-neutrality.md:62`).
- 2D correctly refutes the idea that D3 lets T-P3 ignore REDRESS 96/97/98
  (`restart/audit/totality/p2/2D-cost-model.md:73`).
- 2E correctly fences REDRESS 88/89 as route-specific failures, not category
  bans, and requires material differential plus row-moving consumer
  (`restart/audit/totality/p2/2E-host-arch-esoterica.md:101`).
- 2F correctly forbids retaining a structural-index substrate inside
  parse-that imports and allows transient scanner consumers only
  (`restart/audit/totality/p2/2F-parse-that-gaps.md:70`).
- 2F correctly treats REDRESS 122/126 as proof-only/prerequisite outcomes that
  cannot close parse-that gaps without production callers
  (`restart/audit/totality/p2/2F-parse-that-gaps.md:115`).

## Blockers / Fold Requirements

### CH3-B1: REDRESS-119 Reopen Must Become Row-Specific

2A says REDRESS-119 is not a permanent architecture block and that direct rows
remain eligible under "new material differentials from decision engine, union,
and ASM routes" (`restart/audit/totality/p2/2A-sota-landscape.md:76`). That is
directionally compatible with the SK-V13 addendum, but it is too coarse for
CH3: REDRESS-119 is a per-row fixpoint table, and a broad category name does
not yet distinguish a new attempt from the W3-W7 attempted/blocked routes.

V2 fold requirement:

- Add a REDRESS-119 reopen matrix to 2A or a shared V2 appendix covering all 13
  residual direct rows.
- For each row, list prior REDRESS proof, proposed material differential,
  expected consumer, strict comparator/oracle, and "blocked if same as old
  route" condition.
- Do not let "decision engine", "union", or "ASM" stand alone as a
  differential. They are only route families until a row-local consumer and
  changed dataflow are named.

### CH3-B2: Union-C / PMULL+CSSC Must Be Expanded Before Shortlist Use

2B and 2E correctly say PMULL/CSSC are category-unblocked but historically
rejected. The risk is that both dossiers still use "SIMD-first union C" /
"Union-C's PMULL+CSSC path" as shorthand
(`restart/audit/totality/p2/2B-primitive-vocabulary.md:120`,
`restart/audit/totality/p2/2B-primitive-vocabulary.md:184`,
`restart/audit/totality/p2/2E-host-arch-esoterica.md:57`,
`restart/audit/totality/p2/2E-host-arch-esoterica.md:137`). Without expansion,
that label can paper over three prior failures: PMULL default prefix-XOR
(REDRESS 88), CTZ/bulk consumer (REDRESS 89), and retained union cursor/vector
(REDRESS 96/97/98).

V2 fold requirement:

- Define the minimum material differential for any PMULL+CSSC union candidate:
  which old scalar delegate is not replaced globally, which retained substrate
  shape is not replayed, which consumer owns the masks/positions, and which row
  pays less work than REDRESS 96/97.
- Require an isolated microbench artifact before S-P3 scoping and a same-wave
  row consumer before admission.
- Add an explicit CH3 pre-block: "Union-C" is not S-P3-eligible if it is merely
  PMULL prefix-XOR plus CSSC next-bit plus retained class/cursor replay.

### CH3-B3: REDRESS-121..127 Need A Prerequisite/Admission Taxonomy

V1 mostly uses the SK-V12 entries correctly, but the status is distributed
across dossiers. T-P3 needs a single taxonomy so prerequisite-only outcomes do
not become accidental admissions.

V2 fold requirement:

- Add a compact table for REDRESS 121-127:
  - 121 GrammarConfig legality gate: prerequisite / partial Lock 14 repair.
  - 122 escape-mask correctness: prerequisite only.
  - 123 CSS scaffold: generated baseline / oracle, not SOTA.
  - 124 lightningcss comparator: comparator only.
  - 125 CSS SOTA candidate: admitted candidate, promoted only by 127.
  - 126 ASCII run-skip: microbench route-production split, not production SIMD.
  - 127 SK-V12 close: CSS row admit; union remains future material-differential
    route; ASM production split remains routed.
- Each T-P2 V2 dossier that cites a SK-V12 route should refer to this taxonomy.

## Non-Blocking Observations

- 2D's `T2D-TAPE-MATERIALIZATION` line says simdjson grounds
  `OffsetTape`/retained structural plans while also naming REDRESS 96/97 as
  failed bbnf integration (`restart/audit/totality/p2/2D-cost-model.md:47`).
  This is acceptable if V2 tightens the wording to "general staged
  materialization class" and not "retained union class-column/cursor replay."
- 2F's string/unicode and digest sections correctly avoid treating prior
  decoded-string/direct-digest attempts as viable by citation alone
  (`restart/audit/totality/p2/2F-parse-that-gaps.md:72`,
  `restart/audit/totality/p2/2F-parse-that-gaps.md:76`,
  `restart/audit/totality/p2/2F-parse-that-gaps.md:98`).
- No V1 dossier appears to claim REDRESS 122 or 126 admitted row movement.
  The risk is future S-P3 drift, so B3 is a fold requirement rather than a
  rejection.

## Disposition

T-P2 V1 is not safe to advance as-is. Fold CH3-B1 through CH3-B3 into V2, then
rerun CH3. No source edit is required for this lens. The target V2 posture is:
prior REDRESS routes remain historical evidence; category unblocking is
permitted only when the dossier names the row-local material differential,
scalar/checkasm prerequisite, same-wave consumer, and measured gate that make
the new route different from the rejected implementation.
