SERVED MODEL: claude-opus-5-5

# BBNF value semantics (`@mkbabb/bbnf-lang` 0.2)

What a compiled BBNF rule answers for an input: success or failure, the value, and the offset where
the match ends. **One semantics binds every face**: runtime `compile()` (`BBNFToParser`,
`BBNFToParserFromFile`), the ahead-of-time emitted module, and the parse-that façade. The
conformance corpus in `test/conformance/` is this document's executable form: `semantics.json`
states one case per law by hand, `grammars.json` freezes bbnf-lang's own grammars on fixed inputs
(`freeze-grammars.ts`), and `test/conformance.test.ts` runs both. A face that answers any case
differently is non-conformant.

Values are JavaScript values. `undefined` is a real value (JSON writes it `{"$undefined": true}`).

## The laws

| Form | Succeeds when | Value | Ends at |
|---|---|---|---|
| `"lit"` | the input continues with `lit` | `"lit"` | after it |
| `/re/flags` | `re` matches (sticky) at the offset, **including at end of input** | the matched text; an empty match is `undefined` | after the match |
| `ε` | always | `undefined` | where it started |
| `a , b , …` | every element succeeds, in order | **positional**: one array, slot *i* = element *i*'s value, `undefined` slots kept (length = element count) | after the last |
| `a ?` | always | `a`'s value, or `undefined` when `a` fails | after `a`, or where it started |
| `a *` | always | the array of `a`'s values; the loop **stops at the first iteration that fails or consumes nothing** (that iteration contributes no slot) | after the last counted iteration |
| `a +` | `a *` counted at least one iteration | as `a *` | as `a *` |
| `a - b` | `b` fails here **(tried first)** and then `a` succeeds | `a`'s value | after `a` |
| `a >> b` | `a` then `b` | **`b`'s** value (the right) | after `b` |
| `a << b` | `a` then `b` | **`a`'s** value (the left) | after `b` |
| `a | b | …` | the **first** alternative, in order, that succeeds (ordered choice) | its value | after it |
| `a ?w` | `a` succeeds after skipping ASCII whitespace (9–13, 32); whitespace after it is skipped too | `a`'s value | after the trailing whitespace |
| `( a )` | `a` | `a`'s value | as `a` |

A failed parse carries no value. Nothing in any face writes to the console.

## Routing never changes an answer

A face may route an ordered choice by the code unit at the cursor (or end of input) using the one
analysis in `src/analysis/` (`regex.ts`, `first.ts`): for each ASCII unit, for every non-ASCII unit,
and for end of input, only the alternatives that can start there are tried, **in their original
order**. The analysis is sound (a superset of the truth): a regex's first units read its source
and its flags (`/i` admits both cases; `/iu` also folds K/U+212A and s/U+017F), `\u`/`\x` escapes
name their unit, lookarounds and anchors are zero-width, every non-ASCII unit has one bit, and
`nullable`/`eofOk` are computed to a fixpoint. An alternative that can match empty is kept on every
route in its place, so `/a?/ | "("` on `(` answers the empty match, exactly as the plain choice
does. A rule the grammar does not define (the host supplies it) is unknown: any unit, empty, end of
input.

## Changed from 0.1.4

- **Concatenation is positional** (0.1.4 dropped `undefined` elements, so `"a" , "b"?` on `a`
  answered `["a"]`; it now answers `["a", undefined]`). This matches parse-that 2.x `all()` and
  gives consumers fixed-arity tuples to destructure (value.js COHESION §0ck decision 2).
- **Regex at end of input** follows parse-that 2.x: an empty-matching regex succeeds there.
- **Routing is sound.** 0.1.4 read regex sources without their flags (F-b-3), refused every
  non-ASCII unit at a routed choice (F-b-4), and tried a nullable alternative after the routed
  group instead of in its place.
