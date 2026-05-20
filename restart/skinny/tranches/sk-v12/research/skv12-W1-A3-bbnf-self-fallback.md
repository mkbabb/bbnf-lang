# SK-V12 W1 A3 - BBNF-Self Fallback

Scope: read-only preflight of the third ordered W1 target, BBNF-self grammar.

## Conclusion

BBNF-self is a legitimate final fallback only after concrete CSS and Sheets
preflight failures. It should not be selected ahead of Sheets. If reached, the
least invasive row would be:

- row: `bbnf_self/grammar/direct_to_struct/main`
- grammar: `bbnf_self`
- workload: `direct_to_struct`
- output plane: `direct_sink`

## Evidence

The root repository already carries BBNF grammar sources and a full-core
bootstrap parser, but the skinny W1 owner surface does not expose a generated
BBNF-self runtime module. The W1 Track 1 path must be generated under skinny
codegen/runtime ownership rather than imported from the full core parser.

Useful source fixtures for a later fallback route include:

- `grammar/bbnf/bbnf.bbnf`
- `grammar/bbnf/expressions.bbnf`
- `grammar/bbnf/types.bbnf`

The same shared blocker applies: current skinny direct and typed emitters are
JSON-profiled and JSON-shaped. BBNF-self would need a selected non-JSON
provider branch, runtime module, same-plane oracle, and companion gate report.

## Pre-Blocks

The following routes are not admissible:

- Treating the full core bootstrap parser as the skinny generated Track 1.
- Hand-only direct digest code under a generated name.
- A source-only BBNF grammar compile claim without measured Mbps.
- New directive, BIR, or backend-shape additions.

## Recommendation

Keep BBNF-self as a third-route fallback. For the current W1 plan, it is
research support rather than the preferred selected target.
