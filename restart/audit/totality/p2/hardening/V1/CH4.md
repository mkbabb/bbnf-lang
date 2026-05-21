# T-P2 V1 CH4 Cost / Admission Discipline

Pass: T-P2 Research.
Cycle: V1.
Lens: CH4 COST.
Date: 2026-05-21.

## Verdict

REVISE.

The V1 dossiers correctly identify the admission discipline: scalar reference,
checkasm/parity, hardware gate, same-wave consumer, row movement, and no orphan
primitive closure. They do not yet make that discipline executable enough for
T-P3. Cost is still mostly prose and amendment-candidate language, not a
per-technique adoption ledger with concrete LOC/risk, checkasm command, named
consumer path, row gate, and abrogate threshold. This is a V1-quality result,
not a passable convergence cycle.

## Evidence

- `2A-sota-landscape.md` grounds the process correctly: FFmpeg/dav1d checkasm
  transfer as scalar oracle plus parity plus benchmark, and it explicitly says
  parity or microbench alone is not admission. Its Lock 16 amendment candidate
  asks for a primitive manifest carrying primary source, hardware gate, scalar
  reference, checkasm/parity, corpus parity, same-wave consumer, and row
  movement.
- `2B-primitive-vocabulary.md` is the strongest admission-discipline dossier.
  It marks `BBNF_SIMD_STRICT=1` as mandatory for admission, distinguishes
  admitted/candidate/inventory primitives, and names source-present orphan
  risks such as `byte_context`, `cache_hints`, scalar-delegate bitmap bodies,
  and UDOT digit MAC. It still leaves several candidates at "wire with row
  consumer" granularity rather than a complete adoption cost table.
- `2C-grammar-neutrality.md` correctly refuses primitive parity alone and
  requires generated grammar policy plus same-wave CSS/JSON/Sheets/BBNF-self
  consumers. Its amendment candidates include risk labels, but those labels are
  coarse and do not quantify generated LOC, changed crates, expected row gates,
  or rollback surface.
- `2D-cost-model.md` correctly refutes the P1-P8 cascade as an optimizer and
  says `CostFacts` must become an active objective/frontier model. It also
  names one useful abrogate rule: CH4 rejects if more than 30% of candidate
  expressions use stale/static fallback. It does not yet bind saturation node
  caps, CSP timeout limits, per-grammar extraction budgets, or a required
  emitted report schema tightly enough for T-P3.
- `2E-host-arch-esoterica.md` is properly conservative on aarch64 features:
  TBL is production-real, ASCII run-skip is micro-proven but not admitted,
  PMULL/CSSC are route-specific reopens, UDOT/EOR3/LD4/cache hints remain
  inventory until a same-wave consumer exists, and SVE2 MATCH is out of SK-V13
  NEON scope. It needs explicit adoption cost per primitive family before those
  entries become wave candidates.
- `2F-parse-that-gaps.md` identifies the largest hidden adoption cost:
  extracting/importing real regex/HIR/NFA/DFA machinery as `bbnf-regex`, while
  keeping SIMD bodies in `bbnf-simd` and generated grammar consumers thin. It
  does not yet price the import in LOC, crate boundaries, license/version pin,
  parity harnesses, or first row consumer.

## Blockers / Fold Requirements

1. Add a per-technique admission ledger across 2A-2F.

   Every primitive, substrate, resolver, regex, or scanner candidate must have
   a row with: `candidate_id`, owner dossier, source paths or external source,
   scalar reference, checkasm/parity command, `BBNF_SIMD_STRICT` status where
   applicable, corpus/equality oracle, hardware gate, same-wave consumer path,
   expected row/feature gate, LOC budget, risk class, rollback path, and
   admissibility state (`eligible`, `conditional`, `inventory`, `delete`,
   `refuted`). Dossier prose alone is not gate-consumable.

2. Make orphan-kernel risk mechanical.

   V1 says support-only inventory must be wired, deleted, or blocked, but it
   does not produce a single source-present orphan table. Fold in the five
   source-present SK-V12 demotions plus UDOT/string/context/cache-hint/x86
   background entries and assign each an allowed T-P3 disposition. "Inventory"
   cannot be an implicit close state under the user pin.

3. Attach LOC and risk realism to the decision-engine fold.

   The egraph/CSP/cost route spans `bbnf-regex`, `crates/egraph`, skinny IR,
   passes, cost facts, codegen, generated output, and JSON/CSS equality tests.
   V1 names the architecture but not the implementation envelope. V2 must give
   approximate LOC ranges, touched crates, generated-size risk, equality test
   count, and rollback path for each fold segment.

4. Define abrogate criteria as gates, not observations.

   At minimum, V2 must bind: egraph saturation node/iteration caps, CSP timeout
   ceiling, stale-cost fallback percentage, generated LOC growth threshold,
   row-regression threshold, and parity-harness failure disposition. The V1
   30% stale/static fallback rule is useful but isolated.

5. Name consumers before marking primitives S-P3-eligible.

   ASCII run-skip, UDOT digit MAC, PMULL/CSSC union, EOR3/BCAX, LD4, unicode
   codec, parse-that span scanners, and digest SIMD mix must each name the
   exact generated/runtime consumer family they will wire into, or remain
   conditional/inventory. "A row-moving consumer" is not specific enough for
   CH4 acceptance.

6. Separate proof prerequisites from admission outcomes.

   V1 sometimes says "grounded" for techniques that have only source
   availability or micro-proof. V2 should normalize vocabulary: source-backed,
   scalar-backed, checkasm-backed, micro-proven, production-wired, row-admitted,
   measured-rejected, and architectural-block are distinct states.

## Disposition

V1 is a useful research cycle and should fold forward, not be rejected. The
accepted carry-forward is the process discipline itself: no primitive admission
without scalar reference, strict parity/checkasm, same-wave consumer, and row
movement. The revise work is to convert that discipline from repeated prose
into an executable cost/admission matrix that T-P3 can directly turn into locks
diffs and wave gates.
