# SK-V13 W5 CHALLENGE - Regex Extraction + Decision Gate

Cycle: W5 CHALLENGE. Disposition: ACCEPT WITH CONSTRAINTS.

The W5 plan is admissible under SPEC Section 8. The proposed
`skinny/crates/bbnf-regex/` crate and the corresponding `skinny/Cargo.toml`
workspace edit are accepted as the smallest scoped way to keep regex analysis
out of the runtime scanner crate while giving IR/passes a grammar-neutral fact
source.

## CH1 Correctness

PASS with constraints.

The redress must preserve current JSON nullability and first-byte behavior for
the live JSON regexes, including the `\d` number spelling in
`skinny/grammars/json.bbnf`. Unit tests must cover whitespace, quoted string,
numeric, byte range/class, nullable quantified atom, and unknown regex facts.

Unknown first sets must fail closed for dispatch disjointness. Treating an
unknown branch as non-overlapping is a REJECT.

## CH2 Generality / Lock 14

PASS with constraints.

The public API may expose grammar-neutral fact names such as quoted string,
numeric, whitespace, byte class, nullable, and first set. It must not expose a
JSON-only policy branch under a neutral name. Any generic-crate edit must pass
the Lock 14 scan, and W5 may not claim CSS/JSON generality unless the report
names a generated selection path that consumes the facts or records the
measured architectural block.

## CH3 Regression / REDRESS

PASS with constraints.

REDRESS 84, 87, 114, 115, 119, 120, and 121 remain binding. W5 cannot reopen
string, unicode, digest, source-hook, or old cascade routes. JSON and admitted
CSS guard rows must maintain under the advisory gate run. If a guard regresses
and is not recovered in-wave, W5 records a measured reject.

## CH4 Cost

PASS.

The plan fits the W5 redress extension if the analysis crate stays small and
does not import the runtime scanner. If source/test LOC exceeds the SPEC budget
or a regex dependency adds disproportionate compile/runtime cost, redress must
abrogate to the minimal in-repo analyzer before committing.

## CH5 Hidden Coupling

PASS with constraints.

The redress may touch `parse-that-regex` only if a compile boundary forces it;
the preferred path is no runtime scanner edit. `ir` and `passes` are the
same-wave consumers. `codegen` may be touched only for generated-selection
evidence or tests; no parser output shape or public substrate API change is
authorized by W5.

## CH6 Anti-Paper-Close

PASS with constraints.

`G-W5-DECISION-REGEX` must reject `support_only`, `gate_only`, empty generated
selection, stale fact hashes, and silent cascade fallback. If no generated JSON
or CSS selection path can consume the extracted facts in this wave, W5 may
still close only as a measured architectural block:
`JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH`.

That block must be in `skinny/REDRESS.md`, must cite the material differential
from REDRESS 119/120, and must include a gate-consumed report with live fact
artifact hash and named `ir`/`passes` consumer paths.

## Accepted Redress Contract

- Create `bbnf-regex` as an analysis-only crate.
- Consume its facts in `ir` nullability and `passes` regex decision helpers.
- Remove exact JSON regex pattern decisions from generic decision logic for
  nullable, first bytes, and span classification.
- Add `sk-v13-decision-regex-v1` report validation and xtask pass-through.
- Run the W5 verification commands from the plan.
- Commit source, tests, report, and REDRESS evidence together on PASS or save
  `/tmp/skv13-waveW5-rejected.patch` on FAIL.
