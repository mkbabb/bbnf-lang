# SK-V11 Pass Alpha V1 - Alpha-D Validated / Invalidated Ledger

Pass: Pass Alpha V1.
Agent: alpha-D.
Date: 2026-05-19.
Scope: SK-V10 -> SK-V11 validated / invalidated / demoted / still-open ledger.
Output: this file only.

## Contract Boundary

Pass Alpha consumes a completed skinny cycle and produces the next skinny
contract surfaces; Alpha-D specifically updates the validated / invalidated /
demoted / still-open ledger and must cite commit SHAs plus RESULTS rows
(`restart/prompts/pass-contracts/PASS-ALPHA.md:1-5`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:20-29`). CHALLENGE will check that
claims cite resolving file lines or REDRESS entries, that generality is not
JSON-only, that no proposal reopens REDRESS routes, that same-wave consumers
exist for kernels, that hidden sidecars/substrates are absent, and that no
"wired" claim stands without live evidence (`restart/prompts/ORCHESTRATOR.md:74-88`).

The measured close authority is SK-V10 Close: W10 rendered
`skinny/RESULTS.md` over `/tmp/skv10-w10-full-criterion` with run id
`sk-v9-open:criterion-fnv64-6f007527061ee26d`, and Close passed
`gate-json --with-cost-facts --check-results`
(`restart/skinny/tranches/sk-v10/research/close/close-redress.md:33-64`).

## Commit Anchors

| Evidence | Commit | Classification |
|---|---|---|
| SK-V9 W1 Apache/CITM typed rows | `54c00ec7` | validated typed product evidence |
| SK-V9 W3 full class-column rejection | `2ab8d707` | invalidated union substrate implementation |
| SK-V9 W3 streaming cursor rejection | `020b8e4c` | invalidated union substrate implementation |
| SK-V9 W3 retirement / Alpha resequence | `8b9c8aef` | retired gate and pre-block |
| SK-V10 W1 direct contract | `fc354701` | validated gate contract |
| SK-V10 W2 direct row reclamation | `a25ab5ce` | validated direct row movement |
| SK-V10 W3 parse firewall | `4eb259d8` | validated refusal route |
| SK-V10 W6 github_events typed row | `5379b0e6` | validated typed row movement |
| SK-V10 W7 full string micro-proof rejection | `c29e4813` | invalidated caller-level route |
| SK-V10 W8 hex escape micro-proof | `4893446e` | validated proof-only route |
| SK-V10 W9 existing escape production rejection | `aebcc99f` | invalidated production-integration claim |
| SK-V10 W10 instruments direct row | `c16cc915` | validated direct row movement |
| SK-V10 Close | `36cadd99` | final close authority |

## Validated Ledger

### V1 - Typed product plane is banked and must be held

SK-V10 closes with seven `real_typed_struct A / GO` rows
(`restart/skinny/tranches/sk-v10/research/close/close-redress.md:39-47`).
The final measured rows are:

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic typed Mbps | Evidence |
|---|---:|---:|---:|---|
| `twitter` | 18241 | 16492 | 15636 | `skinny/RESULTS.md:7` |
| `citm_catalog` | 36135 | 19245 | 22066 | `skinny/RESULTS.md:10` |
| `apache_builds` | 8534 | 7079 | 8321 | `skinny/RESULTS.md:15` |
| `github_events` | 13137 | 12855 | 12926 | `skinny/RESULTS.md:18` |
| `update_center` | 12069 | 10603 | 12727 | `skinny/RESULTS.md:21` |
| `mesh` | 9690 | 8072 | 9253 | `skinny/RESULTS.md:24` |
| `marine_ik` | 12186 | 9985 | 9322 | `skinny/RESULTS.md:31` |

Carry-forward rule: SK-V11 must preserve all seven typed rows. The typed surface
is the validated product-plane SOTA surface, but the ledger remains strict about
what is banked: `update_center` is GO under the gate while still below sonic
typed on raw Track 1 Mbps (`skinny/RESULTS.md:21`), and future typed admissions
must carry full Track 1, independent Track 2/oracle, serde_json typed, sonic-rs
typed, checksum parity, and same-run measured evidence (`skinny/REDRESS.md:3108-3122`).

### V2 - Direct digest plane is validated as the primary JSON frontier

SK-V10 closes with six `direct_to_struct A / GO` rows and eleven
`N-direct / NO-GO` rows (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:39-47`).
The six banked direct rows are:

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | Provenance |
|---|---:|---:|---:|---|
| `citm_catalog` | 21595 | 20592 | 20036 | banked guard, `skinny/RESULTS.md:9` |
| `apache_builds` | 11469 | 10368 | 11190 | W2, `skinny/RESULTS.md:14`, `skinny/REDRESS.md:3005-3019` |
| `marine_ik` | 9066 | 9025 | 8235 | banked guard, `skinny/RESULTS.md:30` |
| `instruments` | 12040 | 11166 | 12674 | W10, `skinny/RESULTS.md:33`, `skinny/REDRESS.md:3224-3255` |
| `numbers` | 12619 | 12296 | 13038 | W2, `skinny/RESULTS.md:35`, `skinny/REDRESS.md:3005-3019` |
| `unicode_basic` | 9030 | 8360 | 8940 | banked guard, `skinny/RESULTS.md:41` |

The direct movement contract itself is validated. W1 made direct row movement
executable only for digest-plane `A / GO` transitions with strict measured-row
evidence, independent Track 2, REDRESS provenance, non-gate-only consumer, and
same-run native direct comparator sources (`skinny/REDRESS.md:2978-3001`).
W2 then moved exactly `apache_builds/direct_to_struct` and
`numbers/direct_to_struct` (`skinny/REDRESS.md:3003-3038`). W10 moved exactly
`instruments/direct_to_struct` and did not change parser runtime, generated
direct caller, SIMD primitive, generic crate, typed row, or W3-adjacent
substrate path (`skinny/REDRESS.md:3224-3255`).

Carry-forward rule: SK-V11 should target the remaining direct rows first, while
holding these six rows and the seven typed rows. The SK-V10 handoff names
`direct_to_struct` as the primary JSON frontier and parse-only as retired from
the close target (`restart/skinny/tranches/sk-v10/HANDOFF.md:5-21`,
`restart/skinny/tranches/sk-v10/HANDOFF.md:106-131`).

### V3 - W8 validates only a narrow hex escape micro-proof

W8 proves that the existing `unescape_string` -> `unescape_four_unicode_escapes`
path and `unescape_uxxxx_x4_neon` primitive can beat a scalar-only mirror on
eligible fixed-width Unicode escape slices. The proof is same-host, aarch64,
`-C target-cpu=native`, scalar-oracle backed, and cleared the `>=1.08x`
threshold at aggregate `1.268x` (`skinny/REDRESS.md:3172-3196`).

Carry-forward rule: W8 is a valid proof artifact, not a row admission. It may
seed SK-V11 only through a fresh SPEC/CHALLENGE route with a real source delta,
same-wave consumer, and measured direct/typed row gate.

### V4 - Micro-prove-first is validated as a gate discipline

SK-V10 made profile-first insufficient by construction: no substrate or kernel
intervention reaches S-P3 wave-scoping without a same-host micro-benchmark,
scalar reference, harness, host flags, representative target slices, expected
consumer, and failure threshold (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:227-244`).
W7 and W9 are the falsifying controls: W7 had primitive parity but failed
caller throughput, while W9 had a proof for an already-wired caller and
therefore could not claim same-commit production integration
(`restart/skinny/tranches/sk-v10/SYNTHESIS.md:192-211`,
`restart/skinny/tranches/sk-v10/SYNTHESIS.md:239-244`).

Carry-forward rule: every SK-V11 ASM/SIMD candidate must first pass the
micro-proof gate, then land with a same-wave consumer and row gate. The
accepted proof must not be counted twice if the consumer already exists.

## Invalidated Ledger

### I1 - W3 union event-model / substrate-ceiling thesis is retired

SK-V9 S-P3 originally made W3 the union event-model wave and made W4 kernels
depend on W3 (`restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md:44-57`,
`restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md:68-82`).
The implementation evidence invalidated that thesis:

- REDRESS 96 implemented the class-column plus move-consumed structural-index
  path, passed correctness/parity checks, then missed every W3 must-improve row
  and every W10b maintain row (`skinny/REDRESS.md:2795-2848`).
- REDRESS 97 removed the full vector and implemented an allocation-free
  streaming cursor, then again missed every W3 must-improve row and every W10b
  maintain row (`skinny/REDRESS.md:2850-2906`).
- REDRESS 98 retires `G-W3-UNION-SUBSTRATE`, states that both faithful
  implementations regressed uniformly, rejects the class-lane-only route as
  paper-close, and abrogates W4's cascade-lock dependency on W3
  (`skinny/REDRESS.md:2908-2950`).

Carry-forward rule: SK-V11 must pre-block any renamed W3 route: union/event
substrate, retained class column, `UnionTape`, parser-owned structural
projection, streaming cursor, class-lane-only repair, or W4-through-W3
cascade-lock. The only lawful future structural route would require a new
Alpha/S-P3 contract, a same-host micro-proof, a live non-parse output target,
and explicit REDRESS-98 differential.

### I2 - parse_only SOTA is conceded for SK-V11

Close records 17 `parse_only S / NO-GO` rows, no parse row outside S/NO-GO,
and overall `N-direct / NoGo` (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:39-47`,
`skinny/REDRESS.md:3268-3277`). SK-V10 W3 also closed a parse-only firewall:
no live route through W3 union/event substrate or parser-owned structural
projection, and the validator rejects parse-only SOTA movement
(`skinny/REDRESS.md:3040-3058`).

Carry-forward rule: parse-only remains diagnostic evidence only. It must not
be used as a SOTA admission, an SK-V11 close target, or a route to repair the
direct/typed planes by substrate relabeling.

### I3 - W7 full string primitive route is invalidated at caller level

W7 selected `C5-full-string-proof`, tested the existing
`match_string_at_quote_trusted_utf8` caller against a scalar-only mirror, and
passed scalar/reference and strict checkasm parity. The caller microbench
falsified the route: aggregate `0.774x` versus required `1.08x`, with
`unicode_mixed` at `0.471x`, `unicode_escapes` at `1.315x`, and
`unicode_basic` at `0.604x` (`skinny/REDRESS.md:3150-3170`).

Carry-forward rule: the route is invalid as a broad full-string primitive. It
cannot be consumed by W9-like production work and cannot be generalized from
the one winning slice.

### I4 - W9 production consumption of W8 is invalidated

W9 rejected production consumption because the exact `unescape_string` caller
already consumed `unescape_uxxxx_x4_neon`; there was no legitimate
same-commit source delta, and no source/report/RESULTS row changed
(`skinny/REDRESS.md:3198-3222`). The targeted direct rows also failed floors:
`unicode_escapes` measured 5207 / 5234 Mbps versus floor 12527, and
`y_string_unicode` measured 5096 / 3723 Mbps versus floor 8027
(`skinny/REDRESS.md:3212-3218`).

Carry-forward rule: existing wired SIMD is not a new SK-V11 admission. A
future escape route must name an unimplemented or materially changed consumer,
then prove and measure row movement.

### I5 - instruments typed product row is not banked

W4 rejected `instruments/real_typed_struct`: Track 1 measured 20678 Mbps, but
independent Track 2 measured 12127 Mbps below the 14491 Mbps floor, so no row
moved (`skinny/REDRESS.md:3060-3081`). W10 later admitted
`instruments/direct_to_struct`, not the typed product row
(`skinny/REDRESS.md:3224-3255`, `skinny/RESULTS.md:33`).

Carry-forward rule: SK-V11 must not count `instruments` as a typed product
win. It is direct `A / GO`; typed remains unadmitted unless a new full-fixture
typed route clears both tracks.

## Demoted Ledger

| Item | Demotion | Evidence | SK-V11 handling |
|---|---|---|---|
| `parse_only` | from SOTA target to diagnostic substrate guard | 17 `S / NO-GO`, parse-only firewall, no SOTA movement (`skinny/REDRESS.md:3040-3058`, `skinny/REDRESS.md:3268-3277`) | never a close target; use only for profiling context |
| W3 union/event substrate | from central architecture to pre-block | REDRESS 96/97/98 retirement (`skinny/REDRESS.md:2908-2950`) | refuse renamed substrate-ceiling routes |
| W8 hex escape | from production candidate to proof-only primitive | proof moved no RESULTS row and no production behavior (`skinny/REDRESS.md:3194-3196`) | require new real consumer/source delta and row gate |
| W7 full string | from broad SIMD route to rejected caller shape | aggregate 0.774x vs 1.08x (`skinny/REDRESS.md:3159-3168`) | do not consume; only a materially narrower proof can re-enter |
| Direct digest gate | from broad analogy to row-specific measured admissions | W1 contract and W2/W10 row-specific predicates (`skinny/REDRESS.md:2978-3001`, `skinny/REDRESS.md:3224-3255`) | one row, one floor, both tracks, same-run direct comparators |
| JSON-only generality | from assumed generator proof to open risk | totality receives CSS L4 / Sheets / BBNF-self risk (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:66-73`) | SK-V11 must include non-JSON exercised evidence |

## Still-Open Ledger For SK-V11

### O1 - Eleven direct residual rows remain the live JSON close surface

The close state is 6 direct `A / GO` and 11 direct `N-direct / NO-GO`
(`restart/skinny/tranches/sk-v10/research/close/close-redress.md:39-47`).
The still-open rows and current strict direct comparator evidence are:

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | 1.10x floor | Evidence |
|---|---:|---:|---:|---:|---|
| `twitter` | 11905 | 10968 | 15244 | 13859 | `skinny/RESULTS.md:6` |
| `canada` | 10590 | 10286 | 12157 | 11052 | `skinny/RESULTS.md:12` |
| `github_events` | 12439 | 11430 | 16206 | 14733 | `skinny/RESULTS.md:17` |
| `update_center` | 8425 | 7620 | 11186 | 10170 | `skinny/RESULTS.md:20` |
| `mesh` | 8562 | 8596 | 9422 | 8566 | `skinny/RESULTS.md:23` |
| `random` | 7887 | 7132 | 8948 | 8135 | `skinny/RESULTS.md:26` |
| `gsoc-2018` | 15056 | 14534 | 23437 | 21307 | `skinny/RESULTS.md:28` |
| `unicode_mixed` | 4700 | 4556 | 10480 | 9528 | `skinny/RESULTS.md:37` |
| `unicode_escapes` | 5069 | 5222 | 14147 | 12861 | `skinny/RESULTS.md:39` |
| `distinct_values` | 6303 | 5654 | 11978 | 10890 | `skinny/RESULTS.md:43` |
| `y_string_unicode` | 5067 | 3746 | 9211 | 8374 | `skinny/RESULTS.md:45` |

SK-V11 candidate implication: direct residuals need fresh S-P1 profile on the
direct workload itself. The close packet says remaining direct rows have no
accepted SK-V10 candidate left in that wave envelope, not that they are proven
intrinsically impossible (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:45-47`).

### O2 - Non-JSON grammar generalization is open and load-bearing

Close routes CSS L4 / Sheets / BBNF-self grammar generalization to the totality
track and states that JSON-only SK-V10 evidence validates JSON typed/direct
frontiers, not the full generator thesis
(`restart/skinny/tranches/sk-v10/research/close/close-redress.md:66-73`).
The SK-V10 synthesis repeats that JSON-only wins are not enough to prove the
generator thesis and names non-JSON T-P1 as the unexercised thesis risk
(`restart/skinny/tranches/sk-v10/SYNTHESIS.md:113-117`,
`restart/skinny/tranches/sk-v10/SYNTHESIS.md:246-254`). The handoff refusal
conditions also block generic/runtime/codegen edits outside JSON without a
named CSS L4 / Sheets / BBNF-self proof (`restart/skinny/tranches/sk-v10/HANDOFF.md:141-153`).

SK-V11 candidate implication: at least one wave must exercise a non-JSON
grammar with benchmarked direct or typed evidence, not a Lock 14 prose claim.

### O3 - ASM/SIMD remains open only under strict aarch64 proof discipline

W8 provides the only accepted SK-V10 SIMD proof, and it is explicitly
`aarch64-apple-darwin` with `-C target-cpu=native`, scalar oracle, strict parity
checks, and a caller-level threshold (`skinny/REDRESS.md:3172-3196`). W7 proves
that scalar/checkasm parity alone is insufficient (`skinny/REDRESS.md:3150-3170`).
W9 proves that an already-wired caller is insufficient for same-wave production
admission (`skinny/REDRESS.md:3198-3222`).

SK-V11 candidate implication: any NEON TBL, CSSC, PMULL, UDOT, SHA3 EOR3, or
other aarch64 kernel proposal must carry scalar reference, strict parity,
same-host microbench, exact consumer, same-wave source delta, and measured row
gate. No x86 work is implied by the SK-V10 evidence.

### O4 - Pass Omega lock amendment remains open

SK-V10 close routes REDRESS 98 to Pass Omega as a substrate-ceiling lock
amendment route (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:66-73`).
The synthesis says future profile-derived structural/substrate rewrites should
be pre-blocked unless micro-prove-first survives and the output plane is a live
close target (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:246-251`).

SK-V11 candidate implication: Pass Alpha may carry this as a pre-block now, but
Pass Omega still needs to crystallize it into the durable totality lock surface.

## Alpha-D Recommendation To Alpha-F / S-P1

1. Preserve the seven typed and six direct A/GO rows as maintain gates.
2. Make the eleven direct residual rows the JSON behavior frontier.
3. Keep parse-only out of the SOTA close target.
4. Pre-block W3 union/event substrate and every renamed substrate-ceiling route.
5. Keep W8 as proof-only unless a new consumer/source delta plus row gate exists.
6. Require micro-prove-first for all aarch64 SIMD/ASM before S-P3 wave scoping.
7. Require an exercised non-JSON grammar wave in SK-V11; JSON-only generality is
   still open.

## Alpha-D Disposition

ACCEPT for Pass Alpha V1 input. The SK-V11 path forward is not another
parse-plane substrate campaign. It is direct residual closure plus one
benchmarked non-JSON grammar exercise, with SIMD admitted only through
micro-proof, same-wave consumer, and measured row movement.
