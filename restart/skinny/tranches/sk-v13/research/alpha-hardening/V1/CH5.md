# CH5 Hidden Coupling - SK-V13 Alpha V1

Disposition: ACCEPT-WITH-REVISE.

The Alpha V1 contract mostly passes CH5. It forbids the hidden coupling classes
this lens checks: parallel substrate, retained sidecars, public substrate API
churn, parser-owned substrate, Track 1 / Track 2 collapse, and orphan kernels.
The required revise is narrow: S-P3 must restate the Alpha-C / Alpha-E negative
clauses verbatim around the exploratory union and SIMD candidates, because two
scoping examples are phrased in a way that could be misread as runtime routing or
a vector sidecar unless the binding constraints are carried into SPEC gates.

## Findings

### F1 - Hidden sidecars are not permitted

Verdict: ACCEPT.

Lock 1 says the structural projection is the tape if retained, and names
parallel substrates and consumer-later tape implementations as faults
(`restart/locks/LOCKS.md:52`). The skinny substrate spec restates the one-buffer
posture: `Tape` owns offsets, payload arena, and flags; there is no parallel
structural-index `Vec`; if structural offsets are retained, they are the tape
(`restart/skinny/SUBSTRATE.md:225-229`). It further pre-blocks parse-time aux
columns after REDRESS 50 and says retained projection facts must not be another
parse-time retained column (`restart/skinny/SUBSTRATE.md:231-242`).

Alpha-C carries that into SK-V13: REDRESS 50 pre-blocks sidecar projection
tables, REDRESS 51/53 pre-block parser-owned cursor/list routes and sidecar
structural cursors, and legal union routes must avoid sidecar vectors
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:68-69`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:126-132`).
Alpha-E's union gate only passes if no sidecar class column/vector/list/cursor is
retained (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:315-323`).

### F2 - Public substrate API is not permitted by default

Verdict: ACCEPT.

The scoping value/API audit records that W1a deliberately shipped no public
`GrammarConfig` trait and used `pub(crate)` generated metadata instead
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:11-36`).
Its practical SK-V13 approach is to emit per-grammar `pub(crate)` config
functions and avoid expanding public API
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:234-239`).
The same file's binding clauses explicitly forbid a new directive, new BIR
variant, new `BackendShape`, or public tape/substrate API change such as
`pub trait GrammarConfig`
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:270-281`).

Alpha-E makes that binding at candidate level: E2 must remove JSON policy leaks
without adding a public `GrammarConfig` trait or new substrate API
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:132-136`),
and its falsifiability gate rejects any public trait, directive, BIR variant,
BackendShape, or public `UnionTape`-style substrate change unless S-P3 records an
explicit user-approved SPEC override
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:167-175`).

### F3 - Track 1 / Track 2 hidden coupling is blocked

Verdict: ACCEPT.

ORCHESTRATOR CH5 is defined to catch Track 1 == Track 2 dishonesty and substrate
union violations (`restart/prompts/ORCHESTRATOR.md:74-88`). PASS-ALPHA repeats
that Alpha CH5 must verify no Track 1 / Track 2 dishonesty and that typed product
plane gates remain structurally honest
(`restart/prompts/pass-contracts/PASS-ALPHA.md:33-49`).

SK-V13 SYNTHESIS requires `Track 1 Mbps` and `Track 2 Mbps` for every row and
rejects mixed output planes, report-only Mbps, producer-only telemetry, missing
equality artifacts, and rows lacking provenance
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:152-184`). The contract also says
union admission requires parity, measured row movement, same-wave consumer
wiring, and no hidden Track 1 / Track 2 plane collapse
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:73-83`). Alpha-A preserves the
current CSS row as generated Track 1 plus cssparser oracle / Track 2, with a
shared output plane and exact Mbps values
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:33-37`).

### F4 - Orphan kernels and support-only primitives are blocked

Verdict: ACCEPT.

Lock 16 requires every SIMD primitive to carry scalar parity and corpus parity
(`restart/locks/LOCKS.md:87-112`). The addendum raises the bar: every behavior
wave must move a row or record architectural block, and every primitive must
wire same-commit to a consumer that moves a row
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:96-102`).
SYNTHESIS G4 requires zero aarch64 production orphans; a primitive is closed only
if wired to a same-wave production consumer and measured, deleted, or demoted
with evidence (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:84-93`).

Alpha-D invalidates support-only SIMD/ASM admission and requires every new
primitive to wire to a row-moving same-wave consumer or be removed/demoted with
evidence before close
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md:193-198`).
Alpha-E's E5 gate repeats the zero-orphan rule and rejects a parity pass with no
row movement unless the primitive is removed or demoted
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:346-351`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:401-409`).

### F5 - Parser-owned substrate is not permitted

Verdict: ACCEPT.

The substrate spec excludes a renamed whitespace skipper or second parser-local
scanner, requiring any typed event cursor to consume the scanner/tape event
stream as the single parse substrate with no retained structural index,
`Vec<JsonEvent>`, whitespace bitmap sidecar, or aux projection column
(`restart/skinny/SUBSTRATE.md:244-256`). Alpha-C applies the same rule to SK-V13
union routes by forbidding parser-owned structural cursors and requiring a stable
single shared tape surface
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:68-69`,
`restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:126-132`).

The scoping union legality section is aligned where it says legal union variants
must share `Tape`, avoid parser-owned unions, and keep all state in the shared
tape (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:283-293`).

### F6 - Union reopening is constrained, not a blanket substrate pass

Verdict: ACCEPT.

The user pin reopens rows and categories only with fresh material differential;
REDRESS-119/120 are history, and reopens must cite prior fixpoint REDRESS and
name the material differential
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:58-75`). Alpha-C
states the decisive interpretation: category-level union and ASM-gen blocks are
lifted, but historical implementations remain pre-blocked unless a fresh route
names a material differential, passes CHALLENGE, and wires a same-wave consumer
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:11-28`).

For union specifically, Alpha-C requires SK-V13 routes to cite REDRESS 96/97/98,
state the material differential, avoid sidecars/parser-owned cursors/public API
churn, keep the single shared tape stable, pass parity/checkasm where
applicable, and produce same-wave consumer measurement
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md:120-137`).
Alpha-E's E4 repeats that the union category may be exercised only as a legal
same-tape implementation and cannot introduce a sidecar substrate
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:273-278`).

### F7 - Scoping candidate prose needs a SPEC guard

Verdict: REVISE.

Two exploratory scoping clauses are acceptable as research only, but unsafe if
copied into SPEC without Alpha-C/E constraints:

- C1 says generated parser calls `config::should_use_event_tape(rule_id)` and
  configures `TapeBuilder` accordingly
  (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:318-347`).
  The same file later says runtime union decision must be compile-time per rule,
  driven by codegen rather than runtime dispatch
  (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:294-302`).
  S-P3 must resolve this by specifying monomorphic generated code, not a dynamic
  parser-owned selector.
- C3 says a vector lane index is computed once per 64-byte SIMD block and reused
  across rule invocations
  (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:370-384`).
  Under Lock 1 this can only be a transient producer or direct write into the
  shared tape, not a retained sidecar. Alpha-E's gate already supplies the needed
  rule: no sidecar class column/vector/list/cursor is retained
  (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:315-323`).

Required S-P3 edit: every union/SIMD wave must include an explicit CH5 gate:
single shared tape only; no retained sidecar; no parser-owned cursor/list; no
public substrate API; no dynamic runtime substrate dispatch; Track 1 and Track 2
remain independently produced and reported; every primitive or union route lands
with a same-wave measured consumer.

## Final Disposition

Alpha V1 is CH5-acceptable if S-P3 preserves the binding Alpha-C/Alpha-E
constraints and does not promote exploratory scoping language into executable
SPEC text without the guard above. The contract does not permit hidden sidecars,
public substrate API churn, Track 1 / Track 2 coupling, orphan kernels, or
parser-owned substrate. Union category reopening is constrained by material
differential, same-wave consumer measurement, parity/checkasm where applicable,
and the single-tape substrate contract.
