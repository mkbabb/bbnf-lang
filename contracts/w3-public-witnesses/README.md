# W3 public and candidate witness packet

This directory is pre-freeze data authority. It contains no executable parser,
format switch, selected-instance directory, callback, plugin registry, or
backend identity.

Directories are ordinal:

- `public/0001` through `public/0003` are mapped to stable witness IDs only by
  `artifacts/W0-PUBLIC-WITNESS-ADDENDUM.tsv`;
- `candidates/0001` through `candidates/0005` are mapped to the W0 pool only by
  `artifacts/W3-CANDIDATE-PRESEAL.tsv`.

Every `instance.json` has exactly the five fields admitted by
`bbnf.grammar-instance.v1`. Candidate sources are not copied: their manifests
refer to the exact W0 grammar paths and the preseal table binds those source
bytes.

Case input files use one generic transport:

```json
{
  "schema": "bbnf.w3.input-bytes.v1",
  "encoding": "hex",
  "bytes": "exact lowercase hexadecimal parser-input bytes",
  "decoded_sha256": "sha256 of the decoded parser-input bytes"
}
```

The envelope makes absence of a terminal newline explicit. A harness must
reject unknown or duplicate keys, non-lowercase or odd-length hex, and a
decoded digest mismatch before invoking the parser. It feeds only the decoded
bytes to the generated entry point.

Expected files use `bbnf.w3.symbolic-expectation.v1`. Their oracle is
independent and hand-derived from the sealed grammar, instance, and decoded
input, never captured from implementation output. `observations` are an exact
normalized committed sequence:

- committed nonempty literal or regex events become `TOKEN` rows in input
  order; zero-width regex events are omitted. `TOKEN.symbol` is the closed,
  mechanically derived set `literal | regex`: `EventKind::Literal` maps to
  `literal`, `EventKind::Regex` maps to `regex`, the exact matched bytes remain
  in `lexeme`, and every other symbol value is rejected;
- nonempty Trim events become `TRIVIA` rows;
- committed `@error` transforms become `ERROR_TOKEN` rows;
- Pratt reductions remain `PRATT_REDUCE` rows at their reduction point;
- the successful full-input marker becomes `END_INPUT`;
- structural enter/leave, choice, and repeat-count events are deliberately
  outside this normalized alphabet.

For a mismatch, the whole entry transaction must restore the entry cursor and
publish no observation. Runtime recovery is always `Disabled`.
`GRAMMAR_ERROR_TOKEN_CONTINUE` means only that the ordinary action-free
`@error(child)` branch committed and parsing continued; the compiler never
dispatches on that transform name.

Candidate cases are sealed but MUST NOT be compiled, generated, or executed
before the frozen receipt is git-durable and the W0 selection rule is then
evaluated. Nothing in this packet calculates or names that selection.
