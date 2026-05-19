# SK-V11 Alpha-C REDRESS Digest

Pass: Pass Alpha V1.
Agent: alpha-C.
Date: 2026-05-19.
Scope: REDRESS digest for SK-V10 -> SK-V11.

## Authority

PASS-ALPHA assigns alpha-C to walk the prior skinny cycle REDRESS entries,
classify admitted/rejected/partial routes, identify pre-blocks, and identify
routes that may admit under different framing; each alpha agent writes exactly
one artifact in `restart/skinny/tranches/sk-v{N+1}/research/alpha/`
(`restart/prompts/pass-contracts/PASS-ALPHA.md:20-29`). The orchestrator's
CH3/CH5/CH6 lenses make REDRESS pre-blocking, substrate integrity, and
evidence-backed closure mandatory review surfaces
(`restart/prompts/ORCHESTRATOR.md:81-88`).

The SK-V10 close authority is the W10 full native Criterion render:
`parse_only` remains 17 `S / NO-GO`, `direct_to_struct` is 6 `A / GO` and
11 `N-direct / NO-GO`, `real_typed_struct` is 7 `A / GO`, and overall remains
`N-direct / NoGo` (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:33-47`;
`skinny/REDRESS.md:3268-3274`). SK-V10 HANDOFF records the same current state
and marks SK-V10 converged through REDRESS 110
(`restart/skinny/tranches/sk-v10/HANDOFF.md:112-118`;
`restart/skinny/tranches/sk-v10/HANDOFF.md:155-163`).

## REDRESS 96-98: W3 Falsification

The SK-V9 S-P3 plan made W3 the structural fix: delete scalar
`consume_structural`, wire the discarded SIMD structural index into the parser,
and then land W4 aarch64 kernels against that union substrate
(`restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md:77-82`).
REDRESS 96, 97, and 98 are therefore not local cleanup entries; they are the
measured falsification of the central substrate route.

| Entry | Route | Disposition | Evidence | SK-V11 carry-forward |
|---:|---|---|---|---|
| 96 | Full class-column substrate plus move-consumed structural-index vector | REJECT | Correctness checks were green before measurement, but every W3 must-improve row and every W10b maintain row failed; the rejected patch is saved at `/tmp/skv9-waveW3-rejected.patch` (`skinny/REDRESS.md:2797-2848`). | Hard pre-block for any union/event/class-column/structural-index route. |
| 97 | Allocation-free streaming cursor over the aarch64 scanner plus class lane | REJECT | The route was materially different from REDRESS 96, correctness checks were green, and every W3/W10b row still failed; the rejected patch is saved at `/tmp/skv9-waveW3-v2-rejected.patch` (`skinny/REDRESS.md:2852-2906`). | Blocks the "same idea, less allocation" retry. |
| 98 | Retirement of `G-W3-UNION-SUBSTRATE` | RETIRE | REDRESS says 96 and 97 are not implementation misses; both faithful implementations regressed uniformly, and the class-lane-only V3 route was rejected by CHALLENGE as paper-close (`skinny/REDRESS.md:2910-2927`). | Carry as a hard pre-block; W3 cannot be forced, amended, split, renamed, or used as W4's consumer in SK-V11. |

The load-bearing finding is that the current scalar delimiter path is cheaper on
the M5 Max than materializing or streaming a SIMD structural cursor through
retained parsing (`skinny/REDRESS.md:2928-2933`). REDRESS 98 retires the gate,
abrogates the W4-through-W3 cascade-lock, and allows only existing
offset-tape/string/unescape call-site work with its own scalar reference,
checkasm/parity, same-wave consumer, and maintain gates
(`skinny/REDRESS.md:2934-2950`).

## REDRESS 99-110: SK-V10 Wave Dispositions

| Entry | Wave | Disposition | Row effect | Digest |
|---:|---|---|---|---|
| 99 | W0 telemetry freeze | ADMIT/CLOSED | None | Gate-only telemetry freeze; opening authority remains the W1-rendered SK-V9 snapshot and W0 moves no rows (`skinny/REDRESS.md:2954-2976`). |
| 100 | W1 direct output contract | ADMIT/CLOSED | None | Contract-only direct movement predicate; no parser/runtime behavior changed, and future direct movement requires strict digest plane, Track 2 independence, same-run comparator, provenance, and gate consumption (`skinny/REDRESS.md:2980-3001`). |
| 101 | W2 direct row-table reclamation | ADMIT | `apache_builds/direct_to_struct`, `numbers/direct_to_struct` | Both rows moved from `N-direct / NO-GO` to `A / GO`; all direct and typed guards held, and the W2 gate is consumed by `gate-json` (`skinny/REDRESS.md:3005-3038`). |
| 102 | W3 parse-only firewall | ADMIT/CLOSED | None | Firewall-only: no live dispatch route through W3 union/event substrate, class column, cursor, `UnionTape`, or W4 cascade-lock; parse-only SOTA movement is rejected while rows are `S / NO-GO` (`skinny/REDRESS.md:3042-3058`). |
| 103 | W4 `instruments` typed product admission | REJECT | None | Track 1 passed but independent Track 2 missed the W4 floor, so no typed row moved and the patch was saved at `/tmp/skv10-waveW4-rejected.patch` (`skinny/REDRESS.md:3062-3081`). |
| 104 | W5 root-type typed proof | ADMIT/CLOSED | None | `DirectRootSchema` gained root type proof coverage for array and map-entry roots, but no `RESULTS.md` row moved (`skinny/REDRESS.md:3085-3104`). |
| 105 | W6 root typed row admission | ADMIT | `github_events/real_typed_struct` | W6 consumed the W5 root model and admitted exactly one typed product row with Track 1 12827, independent Track 2 12645, sonic typed 12695, floor 11541, and gate provenance (`skinny/REDRESS.md:3108-3122`). Existing typed maintain rows held in the same report (`skinny/REDRESS.md:3127-3146`). |
| 106 | W7 full string primitive micro-proof | REJECT | None | Scalar and checkasm parity passed, but the caller microbench failed: aggregate 0.774x versus required 1.08x; no production caller or row change remained (`skinny/REDRESS.md:3152-3170`). |
| 107 | W8 hex escape micro-proof | ADMIT/CLOSED proof-only | None | C6 proved `unescape_uxxxx_x4_neon` through the current `unescape_string` caller at aggregate 1.268x, but W8 wired no new production behavior and moved no row (`skinny/REDRESS.md:3174-3196`). |
| 108 | W9 existing escape production | REJECT | None | The exact accepted W8 caller already consumed `unescape_uxxxx_x4_neon`; no cosmetic source delta was attempted, targeted direct floors failed, and future production reuse requires a new SPEC/CHALLENGE route with a real source delta (`skinny/REDRESS.md:3200-3222`). |
| 109 | W10 instruments direct residual admission | ADMIT | `instruments/direct_to_struct` | W10 moved exactly one direct digest row with no parser/runtime/generated/SIMD/generic/W3-adjacent source change; Track 1 12040 and Track 2 11166 both cleared the 11086 floor, guards held, and eleven direct residual rows remain `N-direct / NO-GO` (`skinny/REDRESS.md:3226-3255`). |
| 110 | Close accounting | CLOSED | None | Documentation-only close; all W0-W10 dispositions are complete, final authority is the W10 full native render, and routed remainder is Pass Omega for REDRESS 98 plus totality for CSS L4 / Sheets / BBNF-self generalization (`skinny/REDRESS.md:3259-3280`). |

SK-V10 SPEC carries the same disposition table and states SK-V10 is converged:
W0 99, W1 100, W2 101, W3 102, W4 103, W5 104, W6 105, W7 106, W8 107,
W9 108, W10 109, and Close 110
(`restart/skinny/tranches/sk-v10/SPEC.md:180-204`).

## SK-V11 Pre-Block List

1. W3/union/event substrate is pre-blocked: retained class column, `UnionTape`,
   structural index, streaming structural cursor, parser-owned structural
   projection, class-lane-only fallback, W4 cascade-lock through W3, and renamed
   equivalents are not SK-V11 routes (`restart/skinny/tranches/sk-v10/SPEC.md:145-147`;
   `restart/skinny/tranches/sk-v10/SPEC.md:788-807`).
2. Parse-only SOTA close remains pre-blocked. Parse-only rows are diagnostic
   `S / NO-GO`; they cannot satisfy SK-V11 SOTA while in that state
   (`restart/skinny/tranches/sk-v10/SPEC.md:148`;
   `restart/skinny/tranches/sk-v10/HANDOFF.md:145-148`).
3. Sidecar or parallel substrate producers remain pre-blocked: no sidecar
   parser data, retained cursor state, row output producer, second source pass,
   parser-owned fact slot, public substrate API, or parallel retained tape
   (`restart/skinny/tranches/sk-v10/SPEC.md:161-162`;
   `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:47-60`).
4. Generic JSON policy leaks remain pre-blocked. Generic crates, codegen, or
   runtime outside generated JSON need grammar-neutral design plus named CSS L4,
   Sheets, or BBNF-self evidence; JSON quote, slash, `\u`, surrogate, number,
   whitespace, output, and row policy belong in generated per-grammar templates
   (`restart/skinny/tranches/sk-v10/SPEC.md:163-165`;
   `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:54-55`).
5. Output-plane transfers remain pre-blocked: direct digest rows cannot become
   typed product proof, Apache/CITM typed admission cannot generalize by analogy,
   and Canada typed cannot close through digest, length, coordinate count, schema
   shape, partial fixture, or numeric primitive proof
   (`restart/skinny/tranches/sk-v10/SPEC.md:149-154`;
   `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:56-57`).
6. Direct residual work may not reopen REDRESS 73 helper transfer, REDRESS 93
   scalar-parent folding, REDRESS 50-55 sidecars, REDRESS 66-69 scratch or
   materialization/fact routes, generic JSON policy leaks, W3, sidecars,
   scratch, or parse-only routes (`restart/skinny/tranches/sk-v10/SPEC.md:745-753`;
   `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:102-117`).
7. W7 cannot feed production: its full-string proof failed the caller
   microbench. W8 remains proof-only for the existing C6 caller. W9 measured and
   rejected the exact production route because the caller was already wired and
   direct row floors failed; it cannot be replayed without new SPEC/CHALLENGE
   authority and a real source delta (`restart/skinny/tranches/sk-v10/SPEC.md:211-221`;
   `skinny/REDRESS.md:3152-3222`).
8. PMULL/VPCLMUL prefix-XOR defaults and CSSC/CTZ bulk emission defaults remain
   blocked; future variants must be narrow, caller-proven, non-default,
   same-host, and tied to a named production consumer plus maintain floors
   (`restart/skinny/tranches/sk-v10/SPEC.md:801-803`;
   `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:58-60`).
9. Eager scratch or decoded direct materialization replay and capacity pre-scan
   as product evidence remain blocked (`restart/skinny/tranches/sk-v10/SPEC.md:803-804`;
   `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:59-60`).
10. Process pre-blocks remain binding: no behavior source before S-P3 names owner
    paths, gates, floors, same-wave consumer, cap, and revert protocol; no
    kernel without scalar reference, parity/checkasm, feature gate, microbench,
    and same-wave hot-path caller; no orphan primitive/proof; no producer-only
    telemetry; no new outcome variant without same-wave report/gate/fixture
    updates (`restart/skinny/tranches/sk-v10/SPEC.md:155-178`).

## Routes Eligible Only Under New Framing

These routes may feed SK-V11 only if the SK-V11 Alpha/S-P1/S-P2/S-P3 chain
supplies the missing framing, fresh measurement, and material differential.

| Route family | Eligible framing | Required differential |
|---|---|---|
| Remaining direct residual rows | The W1 direct output/control contract is live, the final surface has 11 `N-direct / NO-GO` direct rows, and direct is the current JSON frontier (`restart/skinny/tranches/sk-v10/HANDOFF.md:112-124`; `skinny/REDRESS.md:3254-3255`). | Fresh direct profile, exact generated direct caller or gate-only row contract, same-run strict direct comparator, independent Track 2/oracle, both tracks meeting the Section 0.2 floor, and guard preservation (`restart/skinny/tranches/sk-v10/SPEC.md:48-52`; `restart/skinny/tranches/sk-v10/SPEC.md:86-117`). |
| Typed product generalization | Typed remains the banked product surface with 7 `A / GO`; W5 proved root typing and W6 admitted `github_events`, while W4 proved that `instruments` cannot be admitted when Track 2 misses (`restart/skinny/tranches/sk-v10/HANDOFF.md:116-127`; `skinny/REDRESS.md:3062-3146`). | Full-fixture generated typed output, independent Track 2/oracle, serde_json typed, sonic typed, checksum parity, same-run comparator rows, typed floors, and no Apache/CITM-by-analogy or direct-digest relabel (`restart/skinny/tranches/sk-v10/SPEC.md:149-154`; `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:149-187`). |
| Existing-substrate unicode/string/escape kernels | Current string/unescape call sites may be targeted; W3 cannot be the caller, consumer, or entry gate. W7 failed, W8 proved only C6, and W9 rejected the exact existing production route (`restart/skinny/tranches/sk-v10/HANDOFF.md:127-130`; `skinny/REDRESS.md:3152-3222`). | Micro-prove-first with scalar oracle, parity/checkasm, host feature gate, representative slices, per-call-site microbench, failure threshold, exact same-wave production consumer, cap/plane declaration, and W10b maintain floors (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:189-230`). |
| Non-JSON grammar proof | SK-V10 routed CSS L4 / Sheets / BBNF-self generalization risk to the totality track, and generic/runtime edits require named non-JSON proof (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:66-73`; `skinny/REDRESS.md:3278-3280`). | SK-V11 must exercise at least one non-JSON grammar with benchmarked direct or typed evidence if a generic, codegen, SIMD, or runtime-outside-JSON intervention is proposed (`restart/skinny/tranches/sk-v10/SPEC.md:236-253`). |
| W3-adjacent structural ideas | Not eligible as W3 in SK-V11. The only possible future framing is a non-W3, non-parse-plane, live direct/typed output-plane route with same-host micro-proof and proof that it is not REDRESS 96/97 by another name (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:49-52`; `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:126-142`). | Any missing material-differential item makes the route not S-P3 eligible (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:267-284`). |

## Routes That Must Not Be Reopened In SK-V11

- Do not reopen W3, the union/event substrate, retained class column,
  structural cursor, `UnionTape`, class-lane-only route, W4-through-W3
  cascade-lock, or renamed equivalent (`restart/skinny/tranches/sk-v10/SPEC.md:145-147`;
  `restart/skinny/tranches/sk-v10/HANDOFF.md:143-146`).
- Do not make parse-only a SOTA target while rows remain `S / NO-GO`
  (`restart/skinny/tranches/sk-v10/SPEC.md:45-47`;
  `restart/skinny/tranches/sk-v10/HANDOFF.md:147-148`).
- Do not weaken `gate-json` strictness, run-id, provenance, Track 2,
  validation, W3 reopen, or parse-only SOTA checks
  (`restart/skinny/tranches/sk-v10/SPEC.md:772-780`;
  `restart/skinny/tranches/sk-v10/HANDOFF.md:152-153`).
- Do not reuse W8/W9 as a production win: W8 is proof-only, and W9 rejected the
  exact existing-call-site production route with no row movement
  (`skinny/REDRESS.md:3194-3222`).
- Do not reclassify SK-V10 close as open implementation work. REDRESS 110 says
  overall `N-direct / NoGo` is the measured close state, not an open SK-V10 wave
  (`skinny/REDRESS.md:3273-3274`;
  `restart/skinny/tranches/sk-v10/research/close/close-redress.md:45-47`).

## Alpha-C Handoff To Alpha-E/Alpha-F

SK-V11 should inherit the direct-frontier target and typed-plane hold from the
SK-V10 close state, while treating REDRESS 96/97/98 as hard negative authority.
The Alpha/S-P3 contract may shortlist direct residual work, typed product
generalization, existing-substrate micro-proven kernels, and non-JSON grammar
proofs only if each candidate names its REDRESS adjacency, material
differential, exact output plane, same-host measurement, independent oracle,
same-wave consumer, and revert path. The checklist is already binding: a route
adjacent to the ledger is not S-P3 eligible if any material-differential item is
missing (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:267-284`).
