# AX.W0a.2.h — 7-grammar predicate matrix (final HEAD)

Per AX invariant 9 ("Gate predicates frozen after introducing wave"),
this matrix captures the predicate outputs after the AX.W0a.2.h
admission widening landed at commit `29bfd055`. Downstream waves that
widen or narrow any predicate must amend `crates/core/tests/gate_
predicate_wire_contract.rs` in the same commit.

## Matrix

| Grammar | `has_w4_classified` | `has_full_shape_coverage` | `has_shape_dispatcher_entrypoint` |
|---|---|---|---|
| JSON | false | true | **true** |
| CSS L4 | true | true | **true** |
| Sheets | true | true | **true** |
| BBNF | true | true | **true** |
| EBNF | false | true | **true** |
| BNF | false | true | **true** |
| BbnfBootstrap | true | true | **true** |

## Rationale

- **`has_w4_classified`** — true when a grammar carries `Pratt` or
  `Unordered` shape tags. JSON / EBNF / BNF are W3-pure (no operator
  towers or unordered sets). The remaining four carry at least one
  Pratt site (operator chains, CSS calc / min / max, BBNF value-
  expression rungs) that gates the visitor-path dispatcher.

- **`has_full_shape_coverage`** — true for every grammar per the
  classified-entry + Alt-of-Refs-entry criteria. Unchanged in
  AX.W0a.2.h.

- **`has_shape_dispatcher_entrypoint`** — WIDENED to true for every
  grammar whose entry-reachable Ref closure is closed over classified
  targets. AX.W0a.2.h retires
  `body_has_dispatcher_fallback_position` as an admission blocker
  (commit `29bfd055`). The shape-authoritative pivot: shape
  emission's tape is the reference; walker-parity retires as the
  correctness oracle; semantic correctness verifies via end-to-end
  `*_parity.rs` decode tests.

## Wire-contract test

```
cargo test -p bbnf --test gate_predicate_wire_contract
test result: ok. 7 passed; 0 failed; 0 ignored; 0 measured
```

Every row passes under the post-`29bfd055` predicates.
