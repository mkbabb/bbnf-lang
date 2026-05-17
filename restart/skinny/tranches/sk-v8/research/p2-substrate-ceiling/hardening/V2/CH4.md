# S-P2 V2 CH4 - Cost

Role: CH4 (Cost) adversarial review of the folded S-P2 substrate-ceiling
cohort and packet docs.

Verdict: REVISE

Score: 76/100

## Blocking Findings

1. **The folded W3 story still prices a narrow class-column migration while
   promising string-boundary deletion.** `SPEC.md` says the lead W3 hypothesis
   fixes parser rediscovery of "every structural byte and string boundary" and
   rewrites the parser as a class-column cursor-walker
   (`restart/skinny/tranches/sk-v8/SPEC.md:392-403`). SC-2 makes the same
   promise more concretely: `parse_value_at` no longer calls
   `match_tiny_plain_string` / `match_string_at_quote` to find string ends, and
   the same-wave consumer must stop re-walking strings
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:296-305`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:327-342`).
   SC-4's recommendation likewise relies on quote, backslash, and
   parallel-prefix parity masks so strings consume bounds from the union cursor
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:281-289`).
   But SC-3's priced Tier A explicitly excludes quote/backslash/parity masks,
   CostFacts/template parity, non-JSON grammars, and broader lowerer bodies
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:359-367`),
   while routing quote/backslash/parity and the larger string-index union to
   Tier B / multi-wave work
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:421-437`).
   SC-4 independently says full string-index closure is multi-wave unless a
   later W3 plan fits the 650-LOC template-parity cap and verification budget
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:365-370`).
   Tier A can be a W3 candidate only if the packet stops selling it as the
   string-boundary close.

2. **Tier A touches generic substrate/codegen surfaces but excludes the
   non-JSON proof that the packet requires.** The global Lock 14 gate requires
   generic CostFacts, codegen, runtime, SIMD, or parser-template edits to prove
   CSS L4, Sheets, and BBNF-self do not require JSON structural roles to
   compile, lower, cost, or run
   (`restart/skinny/tranches/sk-v8/SPEC.md:227-244`). Tier A changes
   `bbnf-simd`, `runtime/src/tape/`, generated JSON parser output, and codegen
   table emission
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:369-404`),
   yet declares non-JSON grammars out of scope
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:364-366`).
   SC-6 supplies the conceptual repair by making `StructuralAlphabet` generated
   per-grammar data with fixed neutral roles and a Lock 14 verification command
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:337-379`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:486-506`),
   but SC-3's Tier A rerun budget only names scalar/checkasm parity, JSON
   runtime tests, and one full SK-V8 gate refresh
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:405-419`).
   The non-JSON proof is therefore still unpriced, which makes the union
   overbroad under the no-new-directive/no-new-BIR/no-new-substrate posture.

3. **The same-wave consumer package is not concrete enough for a bounded S-P3
   W3 plan or a 90-minute challenge.** SC-3's Tier A consumer list covers JSON
   retained `OffsetTape` parsing, existing JSON `EventTape` sparse fact
   patching, `ValueRef::offset()`, and generated `consume_structural` sites
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:359-367`).
   SC-6's Lock 1 risk says the union lands only when cursor, `ValueRef`,
   `path!`, and any retained-view/direct consumers touched by the representation
   change are migrated in the same wave
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:610-620`).
   SPEC keeps W3 owner paths unpreauthorized and requires the later W3 plan to
   name exact files and prove same parser/tape or SinkOnly consumption
   (`restart/skinny/tranches/sk-v8/SPEC.md:416-421`). That is acceptable as a
   governance gate, but not enough as a folded S-P2 cost artifact: S-P3 still
   has to discover whether `path!`, retained view, direct/SinkOnly, generated
   Track 1, and independent Track 2 are in or out. The test plan is similarly
   abstract: SC-3 names a scalar oracle and checkasm placeholder
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:377-380`)
   but only budgets a generic "JSON generated runtime test pass"
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:415-419`),
   while SPEC requires named parser/primitive tests, generated-output audit, and
   full-table maintain checking
   (`restart/skinny/tranches/sk-v8/SPEC.md:212-220`). The 90-minute challenge
   phase (`restart/skinny/tranches/sk-v8/SPEC.md:186-193`) cannot reliably prove
   this from placeholders.

4. **SC-5's gate/schema work remains costed in SC-5 but not folded into the
   packet's W0/W1 manifest.** SC-5 recommends demoting `parse_only`, adding a
   substrate-guard outcome such as `S`, and adding `tape_vs_tape` as W1/W0-plan
   telemetry, not as a W3 production consumer
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:166-190`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:210-230`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:247-252`).
   It prices the comparator-harness/report work at about 120-180 LOC plus
   focused tests and one gate refresh
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:295-307`).
   The packet SPEC still freezes the outcome enum at `{A,C,G,K,L,N-direct}`
   (`restart/skinny/tranches/sk-v8/SPEC.md:57-73`) and W1 already has a
   300-LOC CostFacts gate-binding cap plus one gate refresh
   (`restart/skinny/tranches/sk-v8/SPEC.md:179`,
   `restart/skinny/tranches/sk-v8/SPEC.md:205`,
   `restart/skinny/tranches/sk-v8/SPEC.md:217`). If SC-5's option is adopted,
   it needs an explicit W0/W1 route and budget; if it is not adopted, it must be
   marked residual. It cannot remain an unpriced folded recommendation.

## Non-Blocking Notes

- The V1 CH4 fold materially improved SC-3: Tier A/Tier B now exist, Tier A is
  reduced to about +150 source LOC, and the modified `compact_mask` path names a
  scalar oracle and checkasm placeholder
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:369-419`).
- The packet now correctly says the S-P2 cohort nominates a lead W3 hypothesis
  rather than selecting W3, and it keeps W3 blocked on W0/W1 closure, owner
  paths, same-wave consumer, revert protocol, thresholds, and challenge
  acceptance (`restart/skinny/tranches/sk-v8/SPEC.md:405-414`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:67-75`).
- SC-6's Pass Omega fork is visible and directionally correct: W3 must either
  wait for SC-6-L1-R1 ratification or prove Lock 1 as written with a routed
  Omega residual (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:647-657`).

## Required Fold Actions

1. Split the W3 language across `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`,
   SC-1, SC-2, SC-3, and SC-4 into two named scopes: **Tier A structural-class
   cursor migration** and **Tier B string-boundary / quote-backslash-parity /
   CostFacts-template union**. Tier A must not claim it stops re-walking strings
   unless it includes the Tier B string-index work and fits the 650-LOC cap plus
   verification budget.
2. Add an S-P3-ready W3 cost table for Tier A: exact owner files, source LOC,
   generated-output audit cost, strict same-run comparator rows/planes,
   same-wave consumers, named tests, exact scalar/checkasm commands, full-gate
   rerun budget, and revert slice. Include `path!`, retained-view,
   direct/SinkOnly, generated Track 1, and Track 2 as explicit "touched" or
   "proven untouched" rows.
3. Price the Lock 14/no-new-substrate proof inside Tier A if generic crates are
   edited: CSS L4, Sheets, and BBNF-self no-op/diff tests or unchanged-output
   audits; grep/API scans; and proof that `StructuralAlphabet`/`EscapeKind`
   remain generated data without a new directive, BIR variant, public generic
   grammar API, or second substrate.
4. Route SC-5's `S` outcome and `tape_vs_tape` work explicitly to W0/W1 with
   LOC, owner files, tests, and gate refresh cost, or mark it as a residual not
   folded into SK-V8. Do not let `tape_vs_tape` count as W3's production
   same-wave consumer.
5. Keep the no-dispatch posture intact: no implementation wave is authorized by
   this S-P2 hardening cycle, and S-P3 cannot consume the cohort as converged
   until these cost/scope splits are folded and re-challenged.
