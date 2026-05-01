## CSS L4

Entry rule `stylesheet` body:

OW(
  Repeat[0..=4294967295](
    OW(
      Ref(ruleItem)
    )
  )
)

Note: sub-variant type collision between `error_literal::error_literal_0` and `compare_op::compare_op_1` (U8). Codegen will resolve by rule-scoped lookup.
Note: sub-variant type collision between `error_literal::error_literal_1` and `compare_op::compare_op_0` (Tuple([Span, U8])). Codegen will resolve by rule-scoped lookup.
## Sheets

Entry rule `formula` body:

Seq[
  Regex("=?")
  Ref(comparison_expr)
]

Note: sub-variant type collision between `value_atom::value_atom_3` and `term::term_2` (Tuple([Span, BoxedEnum, Span])). Codegen will resolve by rule-scoped lookup.
## BBNF

Entry rule `grammar` body:

Repeat[0..=4294967295](
  OW(
    Alt[
      Ref(comment)
      Ref(big_comment)
      Ref(directive)
      Ref(rule)
    ]
  )
)

## EBNF

Entry rule `grammar` body:

Repeat[0..=4294967295](
  Seq[
    Regex("[ \\t\\n\\r\\f]*")
    Ref(rule)
    Regex("[ \\t\\n\\r\\f]*")
  ]
)

## BNF

Entry rule `grammar` body:

Repeat[0..=4294967295](
  OW(
    Ref(rule)
  )
)

Note: sub-variant type collision between `term::term_2` and `value_atom::value_atom_3` (Tuple([Span, BoxedEnum, Span])). Codegen will resolve by rule-scoped lookup.
## BbnfBootstrap

Entry rule `grammar` body:

Repeat[0..=4294967295](
  OW(
    Ref(grammar_item)
  )
)

test describe_entries ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.21s

