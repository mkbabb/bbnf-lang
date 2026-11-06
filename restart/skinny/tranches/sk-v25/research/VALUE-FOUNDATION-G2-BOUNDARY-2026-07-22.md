# Value foundation G2 ↔ BBNF CSS boundary receipt

Date: 2026-07-22 EDT  
Authority: bounded read-only architecture assay; no Value/BBNF production,
package, feature acceptance, or mutation authority.

## Custody

| Operand | SHA-256 |
| --- | --- |
| Value `foundation-g2/BRIEF.md` | `2c611bb84c27600f2e1e49d48b2e9285e295d54c7411cacce9fbcafa2ce0112f` |
| Value `foundation-g2/fixtures.json` | `138820f96520045ab4c866e4e3172cda609cd4355b7c8db2ec254b4b403f5be6` |
| terminal shared `MODULE-DAG.md` | `291e51451d84f2fd41adb8b9a31b3c33cf1503b6f47019a06e16702c98daca8f` |
| live BBNF `grammar/css/l4/tokens.bbnf` | `23e535851ce43d7c5e6b243e3b766a4616c5181bf20cc6017405e4406f954054` |
| live BBNF `grammar/css/l4/value-unit.bbnf` | `cb57f79050a6304d79e3a7d8c7b690c902f243880aab4c896a63899290dd727b` |
| SK-v25 value/tape P0 | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| SK-v25 BB-D.W0 | `e8ab6875b02fe1fb9090be8866bfb844f7e97e189ae92962391445cacf4d0d1c` |

Normative pin independently confirmed: CSS Syntax Module Level 3, Editor's
Draft 10 June 2026, especially
[preprocessing](https://drafts.csswg.org/css-syntax/#input-preprocessing),
[comments](https://drafts.csswg.org/css-syntax/#consume-comments),
[strings](https://drafts.csswg.org/css-syntax/#consume-string-token),
[escapes](https://drafts.csswg.org/css-syntax/#consume-escaped-code-point), and
[ident sequences](https://drafts.csswg.org/css-syntax/#consume-name).

## Verdict

**QUALIFIED ACK.** The four direct productions fit the terminal shared DAG and
do not require a lexer, token-object, atom, or CST runtime. One cardinality
correction and one ownership condition bind any BBNF transpose; neither rejects
the Value-local prototype brief.

### 1. Production ownership

`SYNTAX-ESCAPE`, `SYNTAX-IDENT`, `SYNTAX-STRING`, and consuming-only
`SYNTAX-SPACING` belong to the sole `tokens` family. If the physical BBNF module
is split, the Goldilocks shape is:

```text
grammar/css/l4/tokens.bbnf
grammar/css/l4/tokens/escape.bbnf
grammar/css/l4/tokens/ident.bbnf
grammar/css/l4/tokens/string.bbnf
grammar/css/l4/tokens/spacing.bbnf
```

The child filenames deliberately strip the `tokens-` prefix. They are grammar
productions returning primitive semantic leaves/recovery products. They do not
constitute a separately executed lexical layer.

The current BBNF tree has not yet reached that shape. Its active `tokens.bbnf`
contains simplified regex-only `ident` and `string` productions; comment-aware
whitespace remains duplicated in `stylesheet.bbnf`, and `func-body.bbnf` and
`properties.bbnf` redeclare string/identifier spellings. Those are existing
BBNF debts to supersede, not reasons to move G2 out of `tokens`.

Input preprocessing is one shared semantic authority for these productions.
Its CR/FF/CRLF and NUL/surrogate substitutions must preserve raw caller offsets;
four independent hidden preprocessors would duplicate semantics and risk span
drift.

### 2. Recovery products

The feature-local result information is semantically sufficient:

- escape: EOF returns U+FFFD plus one EOF recovery;
- ident: the only listed recovery is propagated from its terminal EOF escape;
- string: EOF returns a recovered string; unescaped newline returns the bad-
  string disposition without consuming that newline;
- comment: EOF is successful comment recovery with one parse error;
- spacing: discarded comment semantics may be lifted into the enclosing spacing
  recovery product in source order.

These are non-duplicative only if one layer owns each observation. A comment
consumed through `cssSpacing` must not additionally publish the same recovery
through an independent global channel; spacing translates the discarded child
recovery once. BBNF classifies these as successful `RecoveryProduct` values,
distinct from transactional parser failure/`Diagnostic`.

The exact shared type cardinality is narrower than the brief's
`readonly "unexpected-eof-comment"[]`: an unterminated comment consumes through
EOF, so a single spacing parse can observe at most one such recovery. BBNF should
infer `None | UnexpectedEofComment` (or an isomorphic optional recovery), not an
unbounded list. Value may retain its local array-shaped API, but it must prove
length `0..=1`; that container is not BBNF type authority.

### 3. BBNF result boundary

BBNF requires no different semantic owner. It does require the result and
recovery algebra to be inferred from the `.bbnf` productions, mappings, and
decorators themselves. The handwritten TypeScript declarations in the G2 brief
are an observation contract, never compiler input or a hidden schema. The public
BBNF value fabric may name the field `recovery` rather than `error`, because a
recovered success is not parser failure, provided the observable variants remain
isomorphic and no information is dropped or duplicated.

### 4. Numeric/dimension boundary

Confirmed: generic dimension spelling and the closed CSS Values unit
classification remain outside G2. The shared DAG assigns number, integer,
percentage, unit, and dimension productions to `value-unit`; CSS Syntax's
[numeric-token algorithm](https://drafts.csswg.org/css-syntax/#consume-numeric-token)
composes a number with an ident sequence or `%`, while Values supplies the
closed semantic unit families. G2 may export `cssIdentifier` for reuse but may
not absorb dimension or unit classification.

## Shared-boundary close

There is no architecture blocker. Adoption credit remains zero until candidate
bytes independently prove the brief's preprocessing, exact non-ASCII ranges,
transaction/furthest behavior, no-throw linearity, and the `0..=1` spacing
recovery invariant. This receipt changes no active package or source authority.
