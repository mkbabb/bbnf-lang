# SK-V13 S-P2 V2 CH4: Cost / Micro-Proof

Verdict: ACCEPT.

## Evidence

- The CH4 contract remains binary: every candidate needs scalar-reference
  status, a checkasm/parity expectation, and a same-wave-consumer note; missing
  any one fails CH4 (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`:124`).
  V2 keeps the P1 provenance quarantine intact: all profile rows are
  `profile_signal_not_gate_admission`, CSS is still profiled as timer/fact-sink
  overhead rather than a parser hot leaf, and future SIMD routes still require
  scalar reference, checkasm/parity, feature-mask disclosure, same-wave
  consumer, and zero-orphan disposition
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:8`-`:23`,
  `:100`-`:104`, `:115`-`:128`).

- V2 preserves and sharpens the V1 CH4 acceptance. The V1 consolidation had
  already accepted cost/micro-proof, but required V2 to stamp speculative SIMD
  inventory as non-shortlistable, make CSS ASCII run-skip conditional on fresh
  narrow CSS proof or same-wave scan-block measurement, and demote D1 lazy tape
  capacity unless paired with a named hot leaf and row-moving consumer
  (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:21`-`:24`,
  `:31`-`:40`). V2 does those folds: P2-C marks EOR3 and byte-context
  standalone routes `NOT-S-P3-ELIGIBLE`; P2-D marks D1 not standalone
  S-P3-eligible; P2-F marks cache hints, standalone prefix/next/bulk, standalone
  byte-context, EOR3, and similar refinements inventory-only or not eligible
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:31`-`:43`;
  `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:180`-`:185`;
  `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:85`-`:92`).

- Candidate costs and primitive gates are explicit enough for S-P3. P2-A's
  candidate table includes scalar-reference status, arch/checkasm expectation,
  P1 antecedent, and same-wave consumer/reject boundary for C1-C8, and it
  explicitly rejects support-only inventory unless attached to a row-moving
  primitive with scalar reference, checkasm/parity, and same-wave row consumer
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:72`-`:87`).
  P2-B upgrades that into an eight-stage S-P3 gate: P1 antecedent, primitive
  contract, executable scalar reference, strict differential checkasm,
  same-host microbench, same-wave consumer, grammar policy, and strict row gate
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:37`-`:48`).

- Micro-prove-first is explicit, not implied. P2-B requires same-host
  primitive timing before row admission and rejects slower candidates without
  row-level compensating proof
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:43`-`:48`).
  Individual V2 routes carry the same rule: C7/B2 may not count the SK-V12 W4
  microbench as row movement and needs a production CSS consumer; D3 cannot be
  shortlisted unless S-P3 gives it a micro-prove-first gate and same-wave row
  consumer
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:89`-`:94`;
  `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:66`-`:76`;
  `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:267`-`:275`).

- Narrow CSS profiling requirements are explicit enough. P2-A says C7 can enter
  S-P3 only if the plan requires a fresh narrow CSS parser profile or same-wave
  CSS scan-block measurement with strict lightningcss equality
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:89`-`:94`).
  P2-B narrows B2 to a named CSS generated scanner consumer and requires
  preserving the admitted declaration-values row plus full lightningcss criteria
  for new rows
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:68`-`:76`).
  P2-C names the exact generated `scan_block` delimiter loop and says the W4
  route is prior micro-proof plus caller contract, not a P1 parser-hot-leaf
  proof (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:12`,
  `:37`). P2-E and P2-F repeat the same CSS boundary for `ByteSetRunSkip64`
  (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:90`-`:99`;
  `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:78`,
  `:114`).

- Resolver-time and generated-size constraints are explicit enough for S-P3
  when P2-F is read with the decision-engine scoping packet. P2-F maps the
  e-graph, active cost function, CSP resolver, cascade deletion, and union routes
  into scalar/proof or regression-oracle requirements and requires output-piped
  regex facts -> e-graph candidates -> cost extraction -> CSP assignment ->
  codegen, rejecting fused solver coupling
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:95`-`:102`,
  `:195`-`:200`). The decision-engine packet gives LOC envelopes for e-graph
  (850-1250 LOC), active cost (500-800 LOC), CSP (610-970 LOC), and final
  cascade replacement (180-240 LOC); it includes code-size in cost and CSP
  objectives, a 200 ms CSP timeout against the >1 s risk, e-graph state/time
  bounds, stale-cost >30% abrogate criteria, and a file-level +1626 LOC summary
  (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:66`-`:73`,
  `:104`-`:111`, `:126`-`:133`, `:164`-`:166`, `:747`-`:754`, `:815`-`:828`).

- Generated-size risk is visible enough for planning rather than papered over.
  P2-F's active cost entry explicitly consumes measurements and code size, and
  its CSP entry carries cost/capacity constraints
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:97`-`:98`).
  The decision-engine packet places code size directly in the cost function,
  uses a DFA-to-NFA rewrite when code size exceeds 4 KB, includes code size in
  CSP objectives, and caps CSP problem codegen complexity at 1000 LOC before
  abrogating that route
  (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:83`, `:108`,
  `:131`, `:150`-`:152`, `:753`, `:866`).

## Blockers

None for CH4.

## Fold Requirements For S-P3

- Preserve the P2-B eight-stage gate as an S-P3 shortlist precondition for any
  SIMD/ASM or shared generated-code candidate.
- Carry C7/B2 CSS ASCII set run-skip only as conditional route-production:
  require fresh narrow CSS parser profiling or same-wave CSS scan-block
  measurement with strict lightningcss equality before admission.
- Do not promote D1 lazy capacity, standalone resolver scaffolding, cache hints,
  EOR3, standalone prefix/next/bulk bitmap helpers, or standalone byte-context
  without fresh P1 evidence, scalar/proof oracle, and same-wave row consumer.
- Keep resolver plans time/size bounded: 200 ms CSP timeout, >1 s solve-time
  abrogate, stale-cost >30% abrogate, e-graph memory/state bounds, code-size
  participation in cost, and visible generated-size/LOC accounting.

## Disposition

CH4 accepts V2 for S-P3 consumption. The accepted scope is conditional
shortlisting, not implementation admission: S-P3 may plan from these candidates
only where it preserves scalar/proof-first gates, same-wave consumers, narrow CSS
profiling, and resolver time/size constraints.
