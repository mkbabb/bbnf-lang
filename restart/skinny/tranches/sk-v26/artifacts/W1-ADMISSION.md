# W1 admission

Disposition: `ADMITTED_GIT_DURABLE`.

Selected commits:

- parse-that W0 payload
  `627c9b46a773a278ed8c60728c9d335be6747905`,
  tree `b0144769472a972bcd4de8f3e67ffcfe264cef99`;
- parse-that clean-break kernel
  `9f6772a70298ae36b86a1815fe8a86484b355d2c`,
  tree `fbd3f2ea87cf81c2d3b76b826524fe56f1709245`;
- parse-that hostile-boundary repair
  `9c9714dfdfa6d33b70f5738c311a9abccb4a1bdf`,
  tree `76141a76dee446249233056e94ea9b043968bdd4`;
- BBNF W1 evidence payload
  `ad7aec5c6b3731fd6d57008d051fb0799377ae22`,
  tree `a6b75de6b07995a741d455d81acc246f7067e061`.

W1 replaced the legacy Rust parse-that implementation with one checked,
borrowed, invocation-owned recognition kernel. Its production source is seven
files and 854 lines, has zero normal dependencies and no unsafe block, and owns
only cursor offset plus one append-only event-sink length as transaction state.
The runtime does not own a grammar graph, regex syntax, HIR, an automaton
builder, memoization, recovery, mutable diagnostics, or domain parser.

The compile/runtime regex boundary is likewise singular:
`bbnf-regex::RegexTablePlan` emits checked immutable tables and
`parse_that::RegexPlan` executes them. The accepted class domain is exactly
1–256 on both sides. Text compilation has one byte-oriented policy, public HIR
is validated before recursive lowering, and default nesting/NFA/DFA budgets
fail with typed errors.

Acceptance evidence:

- 37 kernel laws and four allocation assertions;
- 16 table-plan laws and 7 standalone real-bridge laws;
- 70 locked all-target/all-feature workspace laws;
- strict workspace and bridge Clippy and warning-denying rustdoc;
- zero observed allocations for the selected NoSink success, mismatch, retry,
  progress-fault, empty-provenance, and erased-event cases;
- clean `parse_that` package: 20 files, 12,419 compressed bytes, zero normal
  dependencies, SHA-256
  `4c30d451e42fef441fab29afed4394e142a71b293dc9c4fe301a3b93e8c92293`;
- clean `bbnf-regex` package: 46 files, 82,831 compressed bytes, one default
  normal dependency, SHA-256
  `c9ca32cf4022dbdd238387f9600bc7502d8de3a0f8c6f725a4f289d4e3499d50`;
- 43 BBNF grammar files, 126 distinct regex cases, and 230 occurrences sealed;
- corrected BBNF crater: 148 paths and 3,930 sites, all classified, all still
  byte-identical to W0;
- hostile API reviews A and B: `ACCEPT`;
- fresh final adjudicator: `ACCEPT`;
- 20 fresh clean-compile samples and fixed recognition samples recorded under
  the presealed decision plan with a `NON_PROMOTING_CORRECTNESS_TRANSPOSITION`
  disposition.

The BBNF workspace remains deliberately RED at the first legacy consumer:
`crates/gorgeous/Cargo.toml:78` requests the removed `parse_that/diagnostics`
feature. Restoring that feature or a compatibility shim is forbidden. W2 owns
the consuming clean break.

The W1 direct-control fixture is isomorphic generated-shaped control, not
genuine generator output. W2 therefore starts with a non-negotiable gate:
BBNF must emit literal, regex, sequence, choice, repeat, and EOF direct control
and execute it through both discarding and retaining sinks. Failure reopens W1.
