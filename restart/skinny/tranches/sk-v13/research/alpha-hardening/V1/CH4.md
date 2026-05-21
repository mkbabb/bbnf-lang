# CH4 - Cost / Wave Realism Review for SK-V13 Pass Alpha V1

Role: Alpha CH4 COST/WAVE REALISM for SK-V13 Pass Alpha V1.
Disposition: **REVISE**.

## Scope Read

- `restart/prompts/ORCHESTRATOR.md` §3W defines CH4 as the lens for LOC budget, risk class, wave alignment, hard-cap realism, and same-wave consumers; the same table also binds CH4 to scalar/checkasm-before-wiring and no-orphan same-wave consumer discipline (`restart/prompts/ORCHESTRATOR.md:83-88`, `restart/prompts/ORCHESTRATOR.md:197-212`).
- `restart/prompts/pass-contracts/PASS-ALPHA.md` requires Alpha-E candidates to carry file paths, scalar/checkasm status, same-wave consumer plan, falsifiability gate, LOC budget, and risk classification, while Alpha CH4 asks whether LOC budget, risk, wave alignment, and same-wave consumer are present per intervention (`restart/prompts/pass-contracts/PASS-ALPHA.md:23-29`, `restart/prompts/pass-contracts/PASS-ALPHA.md:33-49`).
- The user addendum raises the bar beyond ordinary Alpha: 24 non-OUT_OF_SCOPE CSS features with 23 still to land, 51 JSON rows, parse-only admission, no support-only landings, aggressive concurrency only for non-overlapping domains, and a redress hard-cap amendment from 30 to 45 minutes only for W5-W9 and W12 union-SIMD (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-46`, `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:96-134`).

## Findings

### F1 - CSS LOC Envelope Is Internally Inconsistent

Disposition: **REVISE**.

Alpha-E states E1 covers 23 remaining CSS L4 parity features and assigns an aggregate `3.0k-5.0k` source/test LOC envelope across W3/W4/W10 (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:49-55`). The same E1 section then states `350-950 LOC per CSS feature family` while still claiming `3.0k-5.0k` aggregate (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:113-119`). Under the addendum, W10 is one wave per non-admitted CSS feature and N is about 22 after W3 stylesheet+selectors fan-out (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:136-145`).

The arithmetic does not hold. At 350-950 LOC per remaining feature family, 23 CSS families imply roughly 8.0k-21.9k LOC before generated LOC accounting, not 3.0k-5.0k. The scoping packet's six composite CSS waves sum to a plausible ~2.9k-4.2k only because they group many features into broad waves, but that grouping conflicts with the addendum's per-feature W10 manifest and per-row movement rule (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:136-225`).

Required revision: split E1 into measurable CSS wave families with per-feature or explicitly bundled row lists, and recompute LOC for W3/W4 foundation plus W10.N fanout. Generated LOC must be accounted separately, as Alpha-E promises, but source/test/oracle/report LOC still needs a realistic aggregate.

### F2 - Hard Caps Are Not Assigned Per Candidate/Wave

Disposition: **REVISE**.

The contract requires each downstream wave to carry hard caps for research, plan, and redress phases (`restart/prompts/pass-contracts/PASS-ALPHA.md:112-122`). The addendum only amends redress from 30 to 45 minutes for the W5-W9 decision-engine fold and W12 union-SIMD wave, leaving research and plan caps unchanged at 20/15 minutes (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129-134`). Alpha-E provides LOC and risk classes, but does not assign minute caps to E1-E5 or to their W10/W11/W14 fanouts (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:47-55`).

The absence matters because E3 is a 2.3k-3.6k very-high-risk fold across a new regex crate, egraph, active cost, CSP, and cascade deletion (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:251-258`), and E5 is 800-1600 LOC of SIMD/checkasm/consumer work (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:411-419`). Without explicit per-wave caps, Alpha-F cannot prove that S-P3 can decompose these into the addendum's 20/15/45-minute envelope.

Required revision: add a cost table that assigns research/plan/redress caps per E-family fanout, names which waves receive the 45-minute redress amendment, and marks all other waves as ordinary cap unless SPEC later proves otherwise.

### F3 - Dependency Order Is Mostly Correct, But Concurrency Is Overstated

Disposition: **REVISE**.

The handoff allows concurrency only after G-Omega and S-P3 convergence, only when file domains do not overlap, and requires RESULTS/REDRESS appends to serialize (`restart/skinny/tranches/sk-v13/HANDOFF.md:76-106`). Alpha-E also records real dependency edges: E1 depends on E2; E2 precedes most of E1 and feeds E4; E3 depends on E2 and may select E4/E5; E4 depends on E2, with C2 depending on E3 and C3 on E5; E5 depends on E2 and may be selected by E3 (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:121-124`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:183-186`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:260-264`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:333-337`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:421-425`).

Those dependencies sharply reduce safe parallelism. CSS expansion and E2 both touch codegen/runtime grammar surfaces; E3 and E4 both touch codegen lowering/runtime shape surfaces; E5 and JSON row reopening both touch `bbnf-simd` and JSON runtime consumers. The addendum permits aggressive worktree fanout only for independent waves with non-overlapping domains (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:112-127`). The current Alpha text names the rule, but it does not provide a dependency DAG or a conflict matrix proving which W10/W11/W12/W14 redress waves can actually run at the same time.

Required revision: S-P3 must serialize E2 before most CSS/value/union work, serialize W5-W9 internal resolver stages unless a concrete substage has isolated owner paths, and only parallelize W10.N or W11.N waves after each wave's owner paths and shared gate/report writes are disjoint.

### F4 - Support-Only Prohibitions Are Stated And Mostly Enforceable

Disposition: **ACCEPT with carry-forward gate**.

The addendum bans support-only landings and requires every primitive to wire same-commit to a consumer that moves a row (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:96-102`). Alpha-E repeats that behavior waves must move at least one row or prove intrinsic-block, and rejects support-only/checkasm-only/future-consumer landings (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:30-45`). Each candidate family includes a same-wave consumer plan: CSS rows emit and measure the exact feature, E2 config fields need CSS/generated consumers, E3 rewrites/CSP constraints must be consumed by generated code, E4 union variants require CSS/JSON row consumers, and E5 SIMD kernels must execute in measured rows (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:95-111`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:160-175`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:232-249`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:306-323`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:390-409`).

The only caveat is E3's allowance for W5 to be infrastructure-only if S-P3 classifies it as non-behavior and binds it to W6/W7 in the same redress tranche (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:232-238`). That can be legal only if S-P3 treats the bound tranche as one behavior unit with row movement before close. It cannot become an orphan `bbnf-regex` crate or paper resolver stage.

### F5 - Candidate Families Are Decomposable Into Measurable Waves, With Two Required Tightenings

Disposition: **ACCEPT with revisions from F1-F3**.

The five-family framing is a reasonable Alpha compression of the addendum's larger wave manifest. E1 decomposes naturally into W3/W4 foundation and W10.N per CSS feature; E3 decomposes into W5-W9 resolver stages and W11/W14 row fanout; E4 decomposes into C1/C2/C3 union variants; E5 decomposes into W4b production split, PMULL/CTZ, string-special, and UDOT attempts (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:79-85`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:214-223`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:293-329`, `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:370-417`).

Two tightenings are mandatory before ACCEPT:

1. E1 must align its bundled CSS feature waves with the addendum's one-row movement rule and per-feature W10 manifest.
2. E3 must define which stages are non-behavior prerequisites versus behavior waves, because support-only resolver infrastructure is not admissible unless bound to same-tranche row movement.

## Final CH4 Disposition

**REVISE, not REJECT.** The Alpha V1 candidate set is directionally decomposable and mostly carries same-wave consumer discipline, but the V1 cost surface is not yet realistic enough for G-Alpha closure. Fix the CSS LOC arithmetic, add explicit hard caps per fanout, and replace broad concurrency language with a dependency/conflict matrix that respects E2/E3/E4/E5 ordering and serialized RESULTS/REDRESS writes.
