# W3 pre-freeze design B

Task: `/root/skv26_w3_prefreeze_design_b`.

Disposition: `PROPOSED_FOR_ADJUDICATION`.

The reviewer independently inspected the W3 charter, all five pool sources,
the selected compiler stages and emitter, replay schemas, and parse-that
transactions without edits and without calculating or naming the held-out
selection.

## Accepted spine

Design B retained Design A's necessary structure:

- corrective public-witness authority before product freeze;
- clean-break explicit grammar instances and entry selection;
- parse-that transactional difference at version `0.5.0` and ABI 3;
- data-driven Pratt rather than a precedence-tower surrogate;
- one generic external harness;
- hard pre-selection freeze and exact post-selection zero-diff proof.

## Simplifications

It rejected unwitnessed or duplicative surface:

- no per-instance normalization: W2 seals raw input-byte spans and no decoded
  numeric/string value plane;
- no automatic recovery manifest: no scanner/synchronization/progress/event
  law or parse-that primitive justifies one;
- no plugin, feature, dependency, native callback, format, backend, action, or
  arbitrary metadata field;
- one `Projection { keep }`, not separate left/right operations;
- two-sided `Trim`, not trailing-only trivia;
- one `PrattReduce` event because successful matcher nodes already expose
  operator-token events;
- no product plugin registry merely to represent absence;
- no compatibility `compile_module`/`ModuleInput` path.

## Strict instance and revision

The proposed instance contains only:

```text
schema
logical_path
entry
trivia_regex | null
pratt_tables[]
```

It rejects unknown and duplicate fields, invalid paths, absent entries,
unbounded manifest/table/operator sizes, invalid trivia, unresolved rules,
duplicate sites/operators, and malformed fixity/binding-power rows.

`Revision` uses a domain-separated, canonical typed length-prefixed encoding
of the instance plus source bytes. JSON key order and whitespace do not change
identity; source, path, entry, trivia, or Pratt changes do.

## Semantic proposals

The frontend adds action-free comments, brace repeat, bracket option,
projection, difference, `?w`, and finite inert `@pretty`. Imports, host arrows,
actions, callbacks, legacy whitespace/recovery directives, and native code
remain rejected.

`difference(run, include, exclude)` probes exclusion first. Every exclusion
outcome is provenance-validated and rolled back; exclusion match returns an
entry-position mismatch, exclusion mismatch runs inclusion from the same
entry, exclusion fault propagates without inclusion, and inclusion
mismatch/fault restores the whole operation. No speculative event survives
and monotone work remains charged.

Difference type is the included type. FIRST remains conservatively the
included FIRST set. Nullable is `include && !exclude`; a nullable exclusion
denotes empty language. Minimum consumption inherits the include lower bound
when exclusion is non-nullable. Both operands remain dependency, fact,
overflow, and work participants.

Pratt uses manifest-defined site, atom, matcher rules, fixity, and binding
powers. Operators receive contiguous revision-bound IDs. The plan must reject
nullable atoms, non-consuming matchers, unresolved rows, invalid fixity/power
shapes, duplicate rows, unsupported nesting, and unbounded recursion. Every
prefix/infix descent charges nesting and work; a consumed infix without a
right operand rejects the whole expression. `PrattReduce` records the complete
reduced span and operator identity.

## Recovery and witness authority

Design B interprets the charter's recovery/error-token disjunction narrowly.
An action-free grammar branch wrapped in the existing inert `@error`
transform may prove committed error-token continuation without compiler
format dispatch. Other instances prove disabled recovery by atomic rejection
and an empty committed sink. The evidence must not claim an automatic runtime
recovery facility.

The corrective addendum seals exact public grammar, instance, case bytes,
hashes, stable feature/case IDs, and all five candidate manifests/cases before
freeze. Candidate suites remain unexecuted until selection; pre-sealing
prevents post-selection tailoring.

## Harness, freeze, and falsifiers

One data-driven contract package lexically enumerates strict instances,
invokes only `compile_instance`, generates into fresh directories, compiles
against exact parse-that, normalizes generated event types, and checks ordered
events in retaining/discarding modes. It contains no format switch.

A temporary local registry containing exact parse-that, bbnf-regex, and BBNF
archives supplies the clean package-consumer gate.

The freeze verifier compares git objects, modes, additions, deletions,
renames, symlinks, manifests, locks, schemas, compiler/emitter/runtime roots,
harness, and verifier code across both repositories. Mutation self-tests must
break every forbidden class.

No-overfit controls include full rule/path renaming, misleading label swaps,
independent rule-order permutations, delimiter/token twins, exhaustive generic
root scans, and rejection of raw grammar reparsing, `include_str!`,
per-instance dependencies/features, and grammar-specific runtime directories.

This document is design evidence only. It authorizes no product mutation and
does not advance or evaluate held-out selection.
