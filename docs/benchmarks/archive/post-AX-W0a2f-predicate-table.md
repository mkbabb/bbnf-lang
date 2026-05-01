# AX.W0a.2.f — 7-grammar predicate table (final state)

Verified via `cargo test -p bbnf --test gate_predicate_wire_contract`
(exit 0; 7/7 passing).

| Grammar | `has_w4_classified` | `has_full_shape_coverage` | `has_shape_dispatcher_entrypoint` |
|---|---|---|---|
| JSON | false | true | **true** |
| CSS L4 | true | true | **true** |
| Sheets | true | true | **true** |
| BBNF | true | true | **true** |
| EBNF | false | true | **true** |
| BNF | false | true | **true** |
| BbnfBootstrap | true | true | **true** |

Hard gate #3 ("`has_shape_dispatcher_entrypoint == true` for all
7 grammars") closed — every grammar's `parse()` routes through
the shape dispatcher as its top-level entrypoint. Hard gate #4
("zero walker-reach from `parse()`") closed for every grammar;
each `parse()` body calls `parse_<grammar>_<root>` directly. See
`post-AX-W0a2f-expand-bbnf.txt` for the representative BBNF slice.

The predicate narrowing retires the `body_has_dispatcher_fallback_position`
false-positive gate that was rejecting every non-Alt-rooted
grammar. The prior rationale — "inline Alt/Regex/Negate/Minus/
TokenDispatch positions would emit `#dispatcher_ident` fallback"
— has been made obsolete by the W0a.2.e + W0a.2.f inline-position
wiring in Flat / ArgList / Wrap / Array. Combined with the
compound-shape-fn `#[inline]` downgrade (D1), LLVM's inliner
collapses the cross-shape recursive edges without the pathological
`#[inline(always)]` unrolling that SIGBUSed the W0a.2.e probe.

Downstream wiring gaps (Keyword Ref-branch handling, BNF walker-
parity record count divergence) are documented in
`post-AX-W0a2f-progress.md` §Remaining-blockers; those blockers
surface only after admission widens and require their own sub-
waves to close.
