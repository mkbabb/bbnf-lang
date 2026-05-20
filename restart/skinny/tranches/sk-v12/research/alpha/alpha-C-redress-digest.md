# SK-V12 Alpha-C REDRESS Digest

Pass: Pass Alpha re-bracket SK-V11 -> SK-V12.
Lane: alpha-C REDRESS digest.
Date: 2026-05-20.
Scope: SK-V12 re-bracket under `USER-PIN-W1-CSS-L4-SOTA.md`.

This digest supersedes the pre-pin Alpha-C V1 digest. The earlier digest
correctly preserved SK-V11 close evidence, but it treated the union-substrate
and ASM-gen families as category-level hard pre-blocks. The user pin changes
that part of the contract.

## Authority

`PASS-ALPHA.md` assigns alpha-C to walk REDRESS, classify admitted, rejected,
and partial routes, identify routes that should pre-block the next tranche, and
identify routes that may admit under materially different framing. CHALLENGE
then checks the pre-block list against the next tranche candidate set.

The re-bracket authority is now:

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `skinny/REDRESS.md` through REDRESS 120
- the six 2026-05-20 audits:
  `skv12-W1-A7-sheets-execution-scout.md`,
  `skv12-aarch64-simd-coverage-audit.md`,
  `skv12-profile-truth-audit.md`,
  `skv12-value-api-audit.md`,
  `skv12-decision-engine-audit.md`, and
  `skv12-totality-fold-scout.md`

The user pin is load-bearing:

- D1 makes CSS L4 authoritative. Sheets and BBNF-self are fallback-only after a
  CSS L4 redress attempt fails, not after plan-time preflight.
- D2 raises the close floor to `lightningcss_mbps + 1` on the same CSS corpus,
  same output plane, strict equality, and same-host oracle/Track 2.
- D3 unblocks the Rust union-substrate architectural category. REDRESS 96,
  97, and 98 remain historical specific failures requiring material
  differential plus CHALLENGE.
- D4 unblocks the ASM-gen category. REDRESS 88, 89, and 90 remain historical
  specific evidence.
- D5 requires zero orphan aarch64 primitives at close.
- D6 makes parse time / `>SOTA` top priority, JSON guards second, and
  `parse_only` diagnostic-only.

## Pin Corrections To REDRESS Pre-Blocks

### Union substrate

REDRESS 96 rejected the full class-column substrate plus move-consumed
structural-index implementation after correctness and parity were green. It
missed every W3 must-improve row and every W10b maintain row.

REDRESS 97 rejected the materially different allocation-free streaming cursor
variant. It removed the full structural-position vector critique from REDRESS
96, but still missed every W3 must-improve row and every W10b maintain row.

REDRESS 98 retired `G-W3-UNION-SUBSTRATE` for SK-V9 and recorded the empirical
finding that the two attempted JSON parse-loop union implementations were
slower than the existing scalar path on the M5 Max.

Under user pin D3, those entries no longer block the union-substrate category.
They block only replay of the measured implementations:

- V1 class-column plus move-consumed structural-position vector is historical
  rejected evidence.
- V2 streaming cursor plus retained class lane is historical rejected evidence.
- V3 class-lane-only remains a historical CHALLENGE rejection.

Any new union attempt must cite REDRESS 96/97/98, name the material
differential, pass CHALLENGE, provide scalar/reference parity where applicable,
wire a same-wave consumer, and measure against the pinned CSS L4 or JSON guard
hot leaf. A renamed class-column, streaming cursor, sidecar substrate,
`UnionTape`, aux projection column, retained cursor list, or parser-owned fact
slot without that differential remains blocked.

REDRESS 102 still blocks parse-only SOTA admission and parse-only row movement.
It does not by itself block a new union-substrate attempt under D3 if the
attempt targets the pinned CSS L4 row or a JSON guard hot leaf and keeps
`parse_only` diagnostic-only.

### ASM-gen

REDRESS 88 rejected PMULL as the default hot `bitmap_prefix_xor_64` body after
parse benchmarks regressed escape-heavy and narrow rows despite correctness and
visible asm proof.

REDRESS 89 rejected the narrowed CSSC CTZ / bulk-position consumer. Correctness
and asm proof passed, but six Track 1/2 rows dropped beyond the maintain
invariant.

REDRESS 90 admitted only the B6 checkasm canary hardening stage. It did not
admit PMULL prefix-XOR or the CSSC CTZ bulk consumer.

Under user pin D4, ASM-gen is unblocked at the category level. The historical
implementations remain specific rejects:

- PMULL as the default prefix-XOR production body remains rejected.
- CSSC CTZ as the bulk position-emission consumer remains rejected.
- Canary hardening remains admitted test-harness infrastructure, not row
  movement.

New PMULL, CSSC, EOR3, UDOT, TBL/TBX, or other ARMv9.2-A candidates may
dispatch only with micro-prove-first evidence, scalar reference, checkasm or
strict parity, same-wave consumer, and a row gate tied to CSS L4 or a JSON
guard hot leaf. Orphan kernels cannot close SK-V12.

### Non-JSON baseline and CSS mandate

REDRESS 111 admitted a companion non-JSON report lane. It did not create a
generated non-JSON Track 1 parser, independent oracle, benchmark row, or
`RESULTS.md` admission. Report-only close remains blocked.

REDRESS 112 rejected the SK-V11 generated non-JSON baseline attempt because
the skinny codegen/runtime path was still JSON-profiled and no generated CSS L4
runtime existed. REDRESS 113 blocked a later CSS intervention from creating
the first measurable baseline row as a future-phase side effect.

Under user pin D1/D2, REDRESS 112/113 are superseded only as blockers to an
explicit CSS L4 redress attempt. They do not authorize a paper close:

- W1 must attempt CSS L4 first.
- Sheets/BBNF-self may be selected only after a CSS L4 redress attempt fails.
- A CSS L4 row admits only with generated Track 1, strict equality, same-host
  independent oracle/Track 2, lightningcss comparator on the same output
  plane, gate-consumed provenance, Lock 14 clean, and Lock 16 clean if SIMD is
  touched.
- Future-phase promises remain blocked. A wave may not claim "CSS baseline
  later" as a close.

## REDRESS Range Classification

| REDRESS | Classification under re-bracket | SK-V12 carry-forward |
|---:|---|---|
| 88 | REJECTED historical ASM implementation | PMULL prefix-XOR default body remains rejected; ASM-gen category is open under D4 with material differential. |
| 89 | REJECTED historical ASM consumer | CSSC CTZ bulk consumer remains rejected; CSSC category is open under D4 with material differential. |
| 90 | ADMITTED test hardening only | Canary hardening is reusable checkasm infrastructure, not parser row movement. |
| 91 | ADMITTED typed product source slice | Typed-product proof carries forward; it is not CSS L4 authority. |
| 92 | REJECTED/routed structural-projection fit | Historical precursor to REDRESS 96/97; does not block D3 category reopen. |
| 93 | REJECTED scalar-parent Track 2 fold | Do not replay scalar-parent digest folding for direct rows without new evidence. |
| 94 | ADMITTED Apache/CITM typed row-table slice | Typed rows carry as guards; not a non-JSON close. |
| 95 | ADMITTED EventGrammar / ValueRef proof | Useful infrastructure; does not make union attempts free of CHALLENGE. |
| 96 | REJECTED union V1 | Specific class-column plus consumed structural vector remains blocked; category open under D3. |
| 97 | REJECTED union V2 | Specific streaming cursor plus class lane remains blocked; category open under D3. |
| 98 | RETIRED SK-V9 gate | Historical falsification of SK-V9 gate; no longer a category pre-block after D3. |
| 99 | CLOSED telemetry freeze | Explicit `CRITERION_HOME` discipline carries forward. |
| 100 | ADMITTED direct output contract | Direct row movement rules remain useful guard discipline. |
| 101 | ADMITTED direct row-table reclamation | JSON direct admitted rows carry as guards; CSS L4 target is separate. |
| 102 | ADMITTED parse-only firewall | Parse-only remains diagnostic-only; no parse-only SOTA admission. |
| 103 | REJECTED instruments typed admission | Do not use Track 1-only typed evidence when Track 2 misses. |
| 104 | ADMITTED root typed proof | Infrastructure only unless measured row gates consume it. |
| 105 | ADMITTED github_events typed row | Typed guard row carries forward. |
| 106 | REJECTED full string micro-proof | Existing full-string caller proof failed aggregate threshold; new string route needs narrower consumer evidence. |
| 107 | ADMITTED escape micro-proof | Proof-only unless a later wave adds a real source delta and production row gate. |
| 108 | REJECTED existing escape production reuse | Existing `unescape_string` consumption cannot be relabeled as same-wave integration. |
| 109 | ADMITTED instruments direct residual row | JSON direct guard row carries forward; not CSS authority. |
| 110 | CLOSED SK-V10 | Historical close, no behavior source. |
| 111 | ADMITTED report/gate lane only | Report-only baseline remains blocked. |
| 112 | REJECTED generated non-JSON baseline | Superseded only for explicit CSS mandate; still proves JSON-profiled codegen/runtime blocker must be fixed. |
| 113 | BLOCKED CSS intervention side effect | Superseded only for explicit CSS mandate; future-phase closes remain blocked. |
| 114 | REJECTED numeric direct route | JSON direct residual evidence remains guard-only. |
| 115 | REJECTED container-tail direct route | JSON direct residual evidence remains guard-only. |
| 116 | BLOCKED bounded string span | Must close malformed parity and Track 2 cost objections before replay. |
| 117 | BLOCKED escaped-segment digest fold | REDRESS 54/55/66/69 adjacency remains blocked for decoded-source sink seams. |
| 118 | BLOCKED output digest/hash host-sink | No legal source/consumer/oracle route found; needs fresh profile and differential. |
| 119 | FIXPOINT direct residual ledger | JSON direct residual rows are guard-only unless fresh profile and micro-proof reopen a row. |
| 120 | CLOSE SK-V11 fixpoint | SK-V11 closed without generated non-JSON admission; SK-V12 must pursue CSS L4. |

## Audit Fold-Ins

### `skv12-W1-A7-sheets-execution-scout.md`

The Sheets execution scout remains useful only as fallback engineering
inventory. The pin demotes Sheets to post-CSS-redress fallback. Its
`sheets_direct.rs` style path also needs re-validation because CH2 later
identified grammar-named generic codegen as a Lock 14 problem. Do not use the
Sheets scout to justify skipping CSS at plan time.

### `skv12-aarch64-simd-coverage-audit.md`

The audit found five orphan/support aarch64 primitives:
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, and `cache_hints`. Before the pin, PMULL and CSSC were treated
as blocked by REDRESS 88/89. After D4, the categories are open, but the
specific default bodies remain historical rejections. SK-V12 close requires
zero orphan primitives or a measured disposition for each.

The strongest admissible ASM-gen candidates are still those with scalar
reference, checkasm, and a consumer: TBL byte-class with a grammar-neutral
layout/byte-set consumer, UDOT digit span with strict x4 coverage, or a narrow
string-special consumer. Each must be micro-proven on the target host before
S-P3 wave scoping.

### `skv12-profile-truth-audit.md`

SK-V12-open PMU authority is `/tmp/skv12-p1` with `-C target-cpu=native`.
Samply is artifact-only; xctrace Time Profiler and CPU counters are authority.
The audit refutes four prior narrative embellishments, so SK-V12 cannot rely
on old prose about output-digest dominance, unicode kernel attribution, CPI
invariance, or universal ALU saturation. CSS L4 has not yet been captured; W1
must create and profile the CSS target rather than borrowing JSON PMU claims.

### `skv12-value-api-audit.md`

CSS L4 emission is illegal until the seven Lock 14 leaks in the JSON template
surface are resolved or isolated through a grammar-owned generated surface.
The minimum legal route is a `GrammarConfig`-style grammar metadata/config
surface before CSS emission, with syntax policy owned by generated per-grammar
runtime output rather than a generic JSON-shaped template. The user pin carries
this explicitly.

### `skv12-decision-engine-audit.md`

The skinny cost model is a passive ledger; CSP and e-graph optimization are
not live in skinny. Candidate selection remains hardcoded in recognizers and
shape priority logic. SK-V12 must not claim a solver-backed or optimizer-backed
CSS route unless that machinery actually lands and is measured.

### `skv12-totality-fold-scout.md`

The scout records two close-relevant gates: the `escape_mask_64` NEON
correctness bug and totality fold deltas for Locks 1, 14, and 16. The pin
makes the `escape_mask_64` fix mandatory before any new SIMD admission. The
scout's old "union rejected" fold is now historical context, not a category
block, because D3 explicitly reopens the category.

## Hard Pre-Blocks Under The Pin

1. Do not dispatch Sheets or BBNF-self before a CSS L4 redress attempt fails.
2. Do not close on REDRESS 111 report-only infrastructure.
3. Do not close on REDRESS 112/113 future-phase promises. They are superseded
   only to permit the explicit CSS L4 mandate to attempt the work.
4. Do not replay REDRESS 96 class-column vector, REDRESS 97 streaming cursor,
   or REDRESS 98 class-lane-only without material differential and CHALLENGE.
5. Do not replay REDRESS 88 PMULL default prefix-XOR or REDRESS 89 CSSC bulk
   consumer without material differential, micro-proof, scalar reference,
   checkasm/parity, and same-wave consumer.
6. Do not admit parse-only rows as SOTA. `parse_only` remains diagnostic-only.
7. Do not touch x86 implementation work.
8. Do not admit a SIMD route before the `escape_mask_64` NEON correctness bug
   is verified and resolved.
9. Do not emit CSS L4 through a generic JSON-policy template. Resolve the
   Lock 14 leaks through a grammar-owned `GrammarConfig`/generated runtime
   surface first.
10. Do not leave orphan aarch64 primitives at close without a measured remove,
    admit, or reject disposition.

## Routes Eligible Under Materially Different Framing

| Route family | Eligible only if... | Required differential |
|---|---|---|
| CSS L4 generated baseline | It is W1's first redress target and measures against lightningcss on the same output plane. | Generated Track 1 CSS L4 parser, independent oracle/Track 2, strict equality, `lightningcss_mbps + 1`, gate-consumed provenance, Lock 14 clean. |
| Union substrate | The target is CSS L4 or a JSON guard hot leaf, not parse-only SOTA. | Not REDRESS 96 V1, REDRESS 97 V2, or REDRESS 98 V3. Must name material differential, pass CHALLENGE, and measure same-wave consumer. |
| ASM-gen | The primitive is consumed by CSS L4 or a JSON guard hot leaf. | Not REDRESS 88 PMULL-default replay or REDRESS 89 CTZ-bulk replay. Must micro-prove, checkasm, and wire consumer in same wave. |
| Sheets baseline | CSS L4 redress has failed and REDRESS records the failure. | Fallback only; must still satisfy generated Track 1, oracle, strict equality, gate provenance, and Lock 14. |
| JSON direct residual | Fresh profile plus micro-proof shows a new route beyond REDRESS 114-119. | Guard-only under D6 unless an admitted JSON guard regression requires in-tranche recovery. |
| Escape/string kernels | They avoid proof-only reuse of REDRESS 107 and the existing-consumer rejection in REDRESS 108. | New source delta, strict scalar/checkasm parity, same-wave production consumer, and row measurement. |

## Alpha-C Handoff

Alpha-E and Alpha-F should rebase the SK-V12 contract around CSS L4, not
Sheets. The correct candidate stack is:

1. CSS L4 generated baseline with lightningcss strict comparator and gate
   provenance.
2. GrammarConfig / generated per-grammar runtime isolation to clear the seven
   Lock 14 leaks before CSS emission.
3. `escape_mask_64` correctness resolution before any new SIMD admission.
4. One or more micro-proven union and ASM-gen routes, now category-open under
   D3/D4, but with REDRESS-specific replays blocked unless a material
   differential passes CHALLENGE.
5. JSON guard preservation or measured demotion; JSON direct residual work is
   secondary to the pinned CSS L4 parse-time / `>SOTA` target.

The SK-V11 measured fixpoint remains real evidence. It no longer authorizes a
Sheets-first SK-V12, nor does it forbid the user-pinned union and ASM-gen
categories. It requires stricter accounting: specific rejected implementations
stay rejected, categories reopen only through measurable differentials, and
CSS L4 is the authoritative first row.
