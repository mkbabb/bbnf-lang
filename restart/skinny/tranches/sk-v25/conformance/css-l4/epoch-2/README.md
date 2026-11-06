# CSS L4 W0 successor input — epoch 2

This directory is an executable, born-RED external-oracle input. It is **not CSS
support**, a candidate, a BBNF schema/type authority, or permission to advance a
tranche. Candidate schema/type consumption is mechanically forbidden throughout
the four stages.

The pipeline is intentionally direct:

1. `src/scope.py` binds the epoch-1 bank, canonical CSSWG sources, normative
   dependencies, dated-publication receipts, and the exact WPT commit/tree/full
   path-to-blob manifest plus source-declared routes.
2. `src/extract.py` performs byte-oriented comment/quote/raw-text-aware extraction,
   records exact spans and hashes, and classifies normativity and source dialects.
3. `src/grammar.py` parses CSS value definitions, indentation-based railroads, and
   legacy grammars; it structures property/descriptor fields and emits profile,
   conflict, and reference-closure decisions.
4. `src/matrix.py` maps each production/entity/expression/algorithm/IDL family to
   explicit BBNF-expectation, fixture, WPT, recovery, diagnostic, serialization,
   and owner planes. Missing external evidence stays visibly `UNMAPPED`, `UNRUN`,
   or `UNPROVEN`.

`fixtures/counterexamples.json` is an independently authored expected-value set;
the producers never read it. The five source-isomorphic test modules exercise
their matching stages plus two complete deterministic regenerations with network
creation disabled.

Verification requires the CSSWG checkout at
`/private/tmp/bbnf-csswg.X76O73/csswg-drafts` and the sparse WPT verification
checkout at `vendor/wpt`, at the exact revisions recorded in `src/common.py`.
Checkout-internal WPT Git/cache bytes are ignored and are not part of this subject;
`data/scope.json` contains the complete 160,788-file path/blob manifest.

```sh
python3 -B src/regen.py --check --allow-blocked
python3 -B -m unittest discover -s tests -p '*.py'
```

Omit `--allow-blocked` to enforce the fail-closed W0 gate. The current input is
expected to exit blocked because authoritative publication, parsing-dependency,
WPT-route, grammar-closure, and external observation evidence is incomplete. See
`AUTHOR.md` for the exact receipt. No network access is used during regeneration
or verification.
