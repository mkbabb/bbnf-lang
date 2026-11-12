# W3 generated-Rust v6 canonical-shape addendum

Status: `BORN_RED_REQUIRED`.

Authority extended:

- `W3-DESIGN-ADJUDICATION.md`;
- the W2 direct-static emitter in `crates/core/src/compiler/emit.rs`;
- the hostile W3 born-RED reviews through semantic subject
  `7b8be85531b1cb48268068b1d301871d45ee0b5980790f9be814200649c63101`.

This addendum does not select, compile, materialize, or execute a held-out
candidate. It closes the generated-Rust v6 structure before the W3 production
implementation begins.

## Why this addendum exists

The first generalized W3 AST verifier tried to admit arbitrary mixtures of
free functions, inherent and trait methods, aliases, stored closures, function
pointers, and alternative helper decompositions. Repeated hostile review
proved that approach both overfit and underproved the result:

- terminal-name collisions manufactured work and nesting guards;
- suffix resolution created ambiguous or false call edges;
- dead branches manufactured apparently live Pratt SCCs;
- co-located but unused trivia plans appeared authoritative;
- the verifier grew toward an incomplete Rust resolver rather than verifying
  BBNF's deliberately direct generated product.

Generated Rust is compiler-owned output, not user-authored extension code.
W3 therefore freezes one small canonical control topology and rejects other
parser-control decompositions. This is a clean-break simplification, not a
compatibility restriction.

## Canonical control topology

Generated Rust v6 has exactly these parser-control identities:

- one top-level free `rule_<ordinal>` function for every lowered rule;
- one top-level free `node_<ordinal>` function for every lowering-plan node;
- one top-level free `pratt_<table-ordinal>` recursive function for every
  Pratt table;
- one top-level free `charge_work`, `with_depth`, `emit_event`, and
  `parse_with_sink` helper;
- the public free roots `check`, `check_with_limits`, `parse_events`, and
  `parse_events_with_limits`.

Descriptor getters may remain inherent methods, but they never participate in
the parser-control graph.

Generated parser control contains no:

- trait or dynamic dispatch;
- parser-control inherent methods;
- function pointers or callable fields;
- stored, returned, or locally aliased closures;
- local aliases for parser-control functions;
- additional recursive parser SCC;
- plugin, callback, backend, format, recovery, or dependency dispatcher.

Inline closures are admitted only as immediate arguments in the canonical
callback positions of parse-that transactional primitives, `with_depth`, and
`emit_event`. They are analyzed as part of their owning canonical function.

Every internal parser call therefore resolves by one exact, unique
single-segment free-function identity. Zero-target and multiple-target
internal calls are invalid generated v6.

## Rule and node binding

`rule_N` is the sole execution owner of lowered rule ordinal `N`. Its canonical
body:

1. charges work and exits on every non-match;
2. enters one nesting frame;
3. opens one `parse_that::sequence` transaction;
4. emits `EnterRule` with `identity(Some(N), None, None, None)`;
5. invokes exactly the rule's recorded `node_<root>`;
6. emits `LeaveRule` with the same rule identity and the recognized span;
7. returns the transaction outcome.

`node_N` is the sole execution owner of lowering `PlanNodeId N`. It charges
work once and implements exactly that node's `PlanOp`. Event-producing nodes
must co-locate the exact `identity` arguments for `N`; a separate identifier
occurrence is not evidence.

The canonical public root has one live value path:

```text
check[_with_limits] / parse_events[_with_limits]
  -> parse_with_sink
  -> Run::parse_module
  -> parse_that::sequence
  -> rule_<instance-entry>
  -> eof
  -> EndInput
```

There is no alternate parser, dead decoy, or handwritten dispatch path.

## Canonical resource helpers

`charge_work` reads the one Work counter, compares it with the configured
limit, returns the exact Work resource fault at the current run cursor when
exhausted, otherwise increments that same counter exactly once and returns
`Step::Matched(())`.

`with_depth` reads the one Nesting counter, compares it with the configured
limit, returns the exact Nesting resource fault when exhausted, increments the
same counter, invokes its one immediate parser closure inside the frame,
restores the prior depth for every returned `Step`, and returns that outcome.

Only the exact sole `Step::Matched` arm continues after `charge_work`; every
Mismatch and Fault arm exits. OR patterns and guarded continuation arms are
invalid.

Transaction authority is a resolved direct parse-that transactional primitive.
Only the immediate parser closure argument inherits that transaction. Eager
arguments do not.

## Canonical Difference

Every `PlanOp::Difference { include, exclude }` at node `N` is implemented by
`node_N` with exactly one live direct call to the owned
`parse_that::difference`.

The call receives the current run plus immediate include and exclude parser
closures that invoke the exact recorded child node functions. Its returned
`Step` is the returned dataflow of `node_N`. Comments, dead calls, manual
exclusion probes, or duplicated Difference primitives are invalid.

## Canonical trivia and Trim

Lowering owns zero or one compiled trivia DFA, distinct from ordinary grammar
regex plans. When present, generated v6 emits one canonically named
`TRIVIA_PLAN`; its byte classes, dimensions, start state, transitions, and
accepting table equal `LoweringPlan::trivia()` field for field.

Every `PlanOp::Trim { child }` at node `N` has this canonical value path:

1. direct `parse_that::regex` with the current run and a terminal constructed
   from `TRIVIA_PLAN`;
2. on exact `Step::Matched(span)`, emit leading `Trivia` only when that span is
   nonempty, using node `N`, `span.start()`, `span.end()`, and side value `0`;
3. invoke the exact child node;
4. repeat the same canonical regex recognition;
5. on exact `Step::Matched(span)`, emit trailing `Trivia` only when nonempty,
   using node `N`, that same span, and side value `1`.

Mismatch and Fault propagate transactionally. No alternate trivia recognizer,
duplicated plan, inline plan, or trivia event outside a Trim node exists.
Null trivia emits no trivia plan, recognition call, or Trivia event.

A Trim node's complete leading-trivia, child, and trailing-trivia path is one
transaction. If leading trivia consumes and emits but the child mismatches or
faults, recognition returns to the Trim entry cursor and retains none of those
events. Retaining and NoSink modes return the same outcome and logical count.

A mixed fixture containing ordinary grammar regex nodes and Trim proves that
ordinary `REGEX_<ordinal>_PLAN` values and `TRIVIA_PLAN` cannot be conflated.

## Canonical Pratt

Each lowering Pratt table ordinal `T` owns exactly one `pratt_T`. The
corresponding `node_N` for `PlanOp::Pratt { atom, table: T }` invokes
`pratt_T(run, limits, 0)` and no other Pratt control. `node_N` is the sole
authorized ingress adapter for that table whenever the site is reached,
including re-entry through a recursive atom reference.

Every invocation of `pratt_T`:

1. charges Work with the canonical helper;
2. enters the canonical Nesting frame;
3. opens an immediate `parse_that::sequence` transaction;
4. performs prefix selection in manifest order using the exact recorded
   matcher `rule_N` functions;
5. recurses to `pratt_T` for the prefix operand at the declared right binding
   power, or invokes the exact atom rule;
6. probes postfix and infix rows in manifest order;
7. recurses to `pratt_T` for an infix RHS at the declared right binding power;
8. emits `PrattReduce` only after the selected operator commits, with the
   exact Pratt node, exported operator ordinal, and complete reduced span.

The exact exported `PRATT_TABLES` and `PRATT_OPERATORS` initializer rows are
the descriptor authority. Every lowering row occurs exactly once with its
table, site rule, atom rule, range, matcher rule, fixity, and powers. Pratt
control references those same rows; fake descriptor constants do not count.

Nonzero/minimum-binding-power self-calls, operator-loop recursion and probing,
and `PrattReduce` construction occur only inside the owning canonical
`pratt_T`. There is no alternative Pratt algorithm or recursive helper.

Every lowering Pratt table is in bijection with exactly one canonical
`pratt_T` and its exact `PlanOp::Pratt` site node, even when that site is
outside the selected entry's dependency closure. A table-owned callable must
be reachable from its exact site adapter. Selected public-root reachability is
required exactly when the dependency solution says the site rule is reachable
from the selected entry; unused but otherwise valid tables are not rejected.

Prefix, postfix, and infix each have:

- manifest-order overlap controls;
- swapped-row mutation controls;
- a partially consuming failed matcher before a successful fallback;
- speculative cursor/event rollback checks;
- retaining/NoSink parity;
- independently discovered monotone work deltas.

## Generated manifest

The canonical generated-v6 manifest contains exactly:

```text
schema
event_schema
revision
source_blake3
source_bytes
nodes
rules
regexes
transforms
fields
trivia_dfas
pratt_tables
pratt_operators
```

Every count is bound to its owning carrier or exported descriptor collection.
The manifest is serialized canonically; key closure and value composition are
one byte-level law.

## Required verifier falsifiers

The born-RED verifier must reject mutations that introduce or substitute:

- an extra or colliding resource helper;
- a parser-control method, trait call, function pointer, local callable alias,
  or stored/returned closure;
- an orphan or synthetic Pratt SCC not in bijection with a lowering table;
- a table-owned Pratt SCC unreachable from its exact `PlanOp::Pratt` site;
- a selected-entry-reachable Pratt site absent from the public-root path;
- an unguarded prefix or RHS recursive call;
- a second public-root parser path;
- a wrong rule/node/table/operator ordinal or descriptor row;
- a manual or dead Difference call;
- a duplicated, inline, or byte-divergent trivia plan;
- a Trivia span, node, or side not derived from the matching canonical call;
- an alternate trivia recognizer;
- a child mismatch after leading trivia that fails to restore the Trim entry
  cursor and both retaining/NoSink event state;
- a prefix, postfix, or infix probe that violates manifest order or rollback.

These mutation fixtures test the verifier itself. Passing public grammars
alone is not sufficient evidence.

This addendum freezes the evidence topology of generated Rust v6, not scalar
local names, harmless attributes, loop unrolling, descriptor lookup
specialization, or inlining that preserves the canonical callable and
transaction boundaries. A topology change requires a generated schema and law
revision rather than an invisible emitter substitution.

## Disposition

The generalized arbitrary-Rust verifier is rejected and must be pruned from
the accepted born-RED subject. Generated-v6 canonical structure is required
before implementation, hostile review, or freeze.
