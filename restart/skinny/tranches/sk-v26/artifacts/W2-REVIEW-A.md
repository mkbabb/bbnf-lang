# W2 hostile review A

Task: `/root/skv26_record_design_a`.

Terminal disposition: `ACCEPT`.

No P0, P1, or P2 defect remains in the reviewed W2 code, paired kernel, or
sealed six-file evidence set.

## Exact subjects

- BBNF carrier commit
  `28a91db51177495efb61cd1c664a85b37fdd9529`, tree
  `df6f1b063acfc49b3e0382fbd4a46892be7e5328`;
- parse-that commit `282ccb6f6da15ef1e5b355b5588a23cc6a76ae7f`,
  tree `4e813a353251f86906baee44a9c4d28e743fe27b`;
- evidence commit `60ee1a51d694b5cf3fd76779336ef165e593129f`,
  tree `02688383ebb21aeac1904377bf1598eb412fa218`.

The review used clean, detached, tree-exact clones.

## Repaired invariants

Minimum consumption distinguishes `Empty`, `Finite(u64)`, and `Overflow`;
surviving inexact public facts reject with typed
`MinimumConsumptionOverflow` rather than masquerading as empty languages.

Analysis executes dependency-first SCC work, evaluates acyclic components
once, bounds recursive fixed points separately, and charges deterministic
initialization, traversal, scheduling, and evaluation through
`analysis_work`. Exact-cap and one-under behavior are typed and deterministic.

Every intended public view is nameable. Persistent rows retain private
four-byte ordinals, public identities remain full-revision capabilities,
public lookups reject foreign revisions, and replay serialization remains
isolated behind private compact DTOs.

## Independent hostile probes

The exact carrier passed:

- a 4,002-rule acyclic reference chain with `iterations == rule_count` and
  exactly seven work units per rule;
- a 512-rule consuming SCC, converging in two component passes plus one
  entry-rule pass;
- exact recursive-SCC work-cap acceptance and one-unit-under
  `ResourceLimit`;
- recursive one-pass rejection and exact two-pass acceptance;
- zero-work rejection for a valid regex before analysis initialization;
- sequence overflow, record overflow, and an overflowing non-entry descendant
  hidden behind a smaller entry alternative;
- deterministic direct overflow origin;
- explicit external naming of the exported borrowed rows, iterators, views,
  and `LoweredRegex`;
- `analysis-facts.v3` and `compiled-module.v5` serialization;
- `analysis_work` replay-option tampering rejection.

A malformed regex may reject during frontend validation before the
analysis-stage budget. That is expected stage ordering, not an
`analysis_work` escape.

## Gates

BBNF passed formatting; all 57 locked workspace/all-target/all-feature tests;
five identity-opacity compile-fail cases; strict Clippy; warning-denying
rustdoc; wasm32; `-D unnameable-types`; the 8/5/4 generated, recursive, and
record controls; and strict Clippy for all three controls.

The unchanged paired parse-that carrier passed 48 kernel laws, the serialized
allocation law, all regex suites and benchmark-harness compilation, strict
Clippy, warning-denying rustdoc, wasm32 for parse-that and bbnf-regex, seven
runtime-bridge laws with strict Clippy, and reproducible package verification.

Package receipts reproduced exactly:

- `parse_that-0.4.1.crate`: 15,270 bytes, SHA-256
  `76f1c1f107ea2125cb787e99c1f69c41a4acfb1588c55771f8218899c5846f94`;
- `bbnf-regex-0.1.0.crate`: 82,832 bytes, SHA-256
  `2e5e85ffb8f198665414b5d5744baa47cb3d1b625438684ab4ec1e077294a50b`.

## Evidence verification

The six committed evidence hashes matched exactly:

- source-plan schema:
  `7a4c5fc6bdac4d2eff23c82707838d694ff4cbc901f6b3453052851e46945d7e`;
- event schema:
  `710c956b87a24f870a061452ed12d48662ac62308483a8d9ac20d28ee09dea7a`;
- type laws:
  `e92c34b29320f0ccf853a36ea8372f998382bd8ed711ed1b6f40595c108906cb`;
- event conformance:
  `4e5720600eebb8869c4120e991e75549426f0e840b1cb5c8db5b46530cb6df10`;
- scalar profile:
  `ca74a0c6079a654ac6877f310a4a77d5190bec4801faae178d0a6dcfd70f2cd5`;
- replay receipt:
  `2d0c51679c7f23b9008c7acc8d3d5b1a8ce78876b9924270548bf95069e05218`.

All enumerated JSON path/digest bindings matched the exact carrier files, and
Draft 2020-12 meta-schema validation passed.

The live recursive-record packet reproduced 39,934 bytes, packet SHA-256
`2521dc79b97d094f1ebd5ab086acccab737895a516ab2f5d5e247d20e68b4d56`,
facts SHA-256
`1c833e8325d2ed036c0a19af5cfcb81426696ce625df6135353b55277fff6eca`,
12 node facts, 4 rule facts, 4 iterations, and 78 work units. It validates
against the committed complete-packet schema.

The scalar prototype remains correctly classified `REJECT`, standalone,
nonproduction, and without cutover or benchmark authority. The stale mutable
`W2-REVIEW-B.md` was excluded from the evidence tree and is not part of this
acceptance.

## Residual limitations

This acceptance is confined to the sealed W2 first-order semantic/event
vertical. It does not establish totality cutover readiness, SOTA performance,
universal grammar coverage, Rust/ASM completion, tape/DOM/path/typed outputs,
host-transform execution, incremental compilation, or standalone trusted
inversion of individual stage packets.

BBNF packaging remains `BLOCKED_EXTERNAL_REGISTRY` because
`bbnf-regex 0.1.0` is unpublished. The exact sibling package verifies
successfully, so this is not a W2 code or admission defect.
