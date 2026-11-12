# W2 hostile review B

Task: `/root/skv26_w2_compact_review_b`.

Terminal disposition: `ACCEPT`.

No P0, P1, or P2 finding remains.

## Frozen identities

- evidence commit `60ee1a51d694b5cf3fd76779336ef165e593129f`,
  tree `02688383ebb21aeac1904377bf1598eb412fa218`;
- BBNF carrier commit `28a91db51177495efb61cd1c664a85b37fdd9529`,
  tree `df6f1b063acfc49b3e0382fbd4a46892be7e5328`;
- parse-that commit `282ccb6f6da15ef1e5b355b5588a23cc6a76ae7f`,
  tree `4e813a353251f86906baee44a9c4d28e743fe27b`.

The evidence commit's parent is the exact BBNF carrier. Its diff contains only
the six named W2 artifacts; the stale earlier review was absent.

## Resolved findings

The three earlier P2 findings are resolved:

1. Minimum-consumption arithmetic distinguishes finite, empty, and overflow
   states. Independent examples reject sequence, record, and masked-descendant
   overflow with typed `MinimumConsumptionOverflow` at the direct arithmetic
   origin.
2. Analysis consumes dependency-first SCC order. Acyclic chains through 1,026
   rules produce `iterations == rules` and `work_units == 7 * rules`; exact
   work caps pass, one-under caps reject, and the recursive fixture rejects at
   one pass and converges at two.
3. Public views are nameable. Crate-level `deny(unnameable_types)` and the
   independent `-D unnameable-types` gate pass.

Zero analysis work rejects a valid-regex grammar before analysis regex
initialization. Replay exposes `bbnf.analysis-facts.v3`,
`bbnf.compiled-module.v5`, and serialized `work_units`.

## Gates and probes

BBNF passes 57 workspace tests:

- 13 compiler unit laws;
- 3 carrier-lookup laws;
- 5 dependency laws;
- 4 emitter laws;
- 9 frontend laws;
- 1 trybuild harness covering 5 compile-fail cases;
- 2 record laws;
- 6 replay/edit laws;
- 14 type/analysis laws.

BBNF also passes strict Clippy, formatting, warning-denying rustdoc, wasm32,
and public-nameability checks.

The paired parse-that tree passes 32 regex/library laws, 48 kernel laws, 1
allocation law, 7 runtime-bridge laws, strict Clippy, formatting,
warning-denying rustdoc, and wasm32.

External controls pass 8 generated-scalar, 5 recursive, 4 record, and 3
scalar-profile tests. All four controls also pass strict Clippy.

## Package reproducibility

Each archive was produced twice and compared byte-for-byte:

- `parse_that-0.4.1.crate`: 15,270 bytes, SHA-256
  `76f1c1f107ea2125cb787e99c1f69c41a4acfb1588c55771f8218899c5846f94`;
- `bbnf-regex-0.1.0.crate`: 82,832 bytes, SHA-256
  `2e5e85ffb8f198665414b5d5744baa47cb3d1b625438684ab4ec1e077294a50b`.

## Artifact audit

All six committed artifact hashes match:

- source schema:
  `7a4c5fc6bdac4d2eff23c82707838d694ff4cbc901f6b3453052851e46945d7e`;
- event schema:
  `710c956b87a24f870a061452ed12d48662ac62308483a8d9ac20d28ee09dea7a`;
- type laws:
  `e92c34b29320f0ccf853a36ea8372f998382bd8ed711ed1b6f40595c108906cb`;
- event conformance:
  `4e5720600eebb8869c4120e991e75549426f0e840b1cb5c8db5b46530cb6df10`;
- scalar profile:
  `ca74a0c6079a654ac6877f310a4a77d5190bec4801faae178d0a6dcfd70f2cd5`;
- replay:
  `2d0c51679c7f23b9008c7acc8d3d5b1a8ce78876b9924270548bf95069e05218`.

All 59 JSON path/digest references, 33 replay-table digests, and 3 grammar
digests resolve exactly.

The complete-packet schema passes Draft 2020-12 meta-validation. An
independently regenerated recursive-record packet validates and exactly
reproduces 39,934 bytes, 12 node facts, 4 rule facts, 4 iterations, 78 work
units, packet SHA-256
`2521dc79b97d094f1ebd5ab086acccab737895a516ab2f5d5e247d20e68b4d56`,
and facts SHA-256
`1c833e8325d2ed036c0a19af5cfcb81426696ce625df6135353b55277fff6eca`.

The scalar-profile receipt reproduces the decisive intervals and mechanical
`REJECT` disposition from the frozen raw measurements.

## Residual limitations

Acceptance is limited to the sealed first-order W2 vertical. It does not
establish HM/generalized inference, incremental compilation, standalone
inversion of individual stage packets, universal grammar coverage, totality
cutover, tape/DOM/path/typed outputs, Rust/ASM completion, or SOTA performance.

The scalar classifier remains a historical standalone negative performance
witness and is prohibited from production promotion.

Final BBNF packaging remains externally registry-blocked until
`bbnf-regex 0.1.0` is published. The sibling crate itself packages
reproducibly.
