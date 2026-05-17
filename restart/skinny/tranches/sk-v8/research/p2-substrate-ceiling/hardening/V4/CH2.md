# S-P2 V4 CH2 - Generality / Lock 14 Review

Role: CH2 (Generality)

Verdict: ACCEPT

Score: 96/100

## Blocking Findings With Refs

None.

The V4 fold satisfies the CH2 lens: Lock 14 remains the governing test, and the
candidate is framed as grammar-neutral byte-set/class/fact machinery rather than
JSON policy in generic crates (`restart/prompts/ORCHESTRATOR.md:81-88`;
`restart/prompts/skinny/PASS-2-RESEARCH.md:102-107`). The packet now makes that
boundary executable in the per-wave gate: generic code may store/search generated
structural-class ordinals or opaque fact ids, while event-role, recovery, layout,
record-boundary, indentation, and reused-punctuation meaning is interpreted only
inside generated grammar modules keyed by parser state plus class/byte
(`restart/skinny/tranches/sk-v8/SPEC.md:247-269`).

## Notes

1. The generated-byte-set / opaque-ordinal boundary is folded consistently.
   SC-6's proposed Lock 1 refinement says the retained projection uses a
   generated per-grammar byte-set table plus opaque structural-class ordinals,
   and generic substrate code must not expose a public generic grammar API,
   branch on grammar names, or name JSON/role semantics
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:263-287`).
   SC-6 then defines `StructuralAlphabet` as generated per-grammar data and
   bars generic code from mapping ordinals to `Open`, `Close`, JSON member
   delimiters, CSS declaration terminators, Sheets range operators, or any
   other event role
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:350-395`).
   SC-3 states the same runtime boundary: `StructuralClassTable` and fact-id
   tables are emitted per grammar, while `compact_mask` and `runtime/src/tape/`
   consume ordinals they do not interpret
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:324-355`).

2. The opaque fact lane does not become a generic recovery/layout/JSON policy
   escape hatch. SC-3 explicitly narrows `facts` to opaque generated ids, forbids
   matching on fact ids or naming JSON/JSONL/CSS/indentation policy in
   `runtime/src/tape/`, and excludes density tables, quote caches, skip caches,
   parser-owned slots, and independent lifetimes from Tier A
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:192-204`).
   The JSONL and indentation examples are correctly marked as Lock 14 examples,
   not Tier A implementation scope, and they forbid generic newline/record
   handling or a generic indent stack
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:206-240`).

3. JSON string-density evidence is telemetry only. SC-1 keeps JSON quote-count
   share in per-grammar `RecognizerFacts`/`CostFacts`, not in a generic density
   selector (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:126-135`),
   and says the specific quote-count predictor does not generalise
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:379-385`).
   SC-4 demotes the string-density knee to diagnostic telemetry unless a later
   plan supplies command, row set, formula, numeric target, maintain budget, and
   pass/fail rule
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:190-193`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:322-327`).
   The packet carries that demotion: JSON quote-count share is not a generic
   selector policy, and SC-4's string-density evidence is diagnostic telemetry,
   not an admission gate (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:122-128`;
   `restart/skinny/tranches/sk-v8/SPEC.md:436-445`).

4. The non-JSON examples are substantive enough for CH2. SC-3 gives different
   generated structural alphabets for CSS L4, Google Sheets, and BBNF-self over
   the same substrate/compaction machinery
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:366-399`).
   SC-6 tests reused punctuation and escape differences: CSS `:` is interpreted
   by generated CSS state, Sheets doubled-quote handling stays in the generated
   Sheets module or routes away from the retained representation, and arbitrary
   user grammars with no byte-disjoint skeleton route to `EagerTape`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:418-506`).
   SC-4 separately keeps CSS strings/url/comments and Sheets string literals as
   generated facts with per-plane gates, not JSON quote-density policy
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:329-358`).

5. `tape_vs_tape` is not a hidden generic substrate or admission policy. The
   packet routes it as residual telemetry, outside default W0/W1 scope, and says
   it is neither current SOTA admission evidence nor W3's production same-wave
   consumer (`restart/skinny/tranches/sk-v8/SPEC.md:125-131`). SC-5 preserves the
   same boundary and prices any future row as explicitly owned telemetry/gate work
   with owner files, focused tests, and one rerun budget
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326-346`).

6. The no-new-directive / no-new-BIR / no-new-substrate boundary is intact. SPEC
   keeps those as non-negotiables (`restart/skinny/tranches/sk-v8/SPEC.md:180-185`).
   SC-3 makes the union one retained `Tape` with one producer and no post-build
   `StructuralIndex` API, clone, cache, parser-owned cursor, or attachment hook
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:286-295`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:407-431`).
   SC-6 explicitly rejects `UnionTape`, a sixth `BackendShape`, a BIR variant, a
   BBNF directive, a public substrate type, a public generic grammar API, a
   grammar-name branch, and the old offset append constructor
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:301-320`;
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657-666`).

7. Minor non-blocking polish for a future non-code fold: SC-6 includes
   `pad/clamp policy` inside the `StructuralAlphabet` data bundle
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:359-364`).
   This is not a Lock 14 blocker because it is Lock 16 tail discipline rather
   than JSON semantics, but the cleaner long-term wording would keep tail policy
   under primitive metadata and reserve `StructuralAlphabet` for generated byte
   sets plus opaque class ordinals.

## Required Folds If REVISE

N/A. Verdict is ACCEPT.
