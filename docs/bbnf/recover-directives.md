---
title: Error Recovery
order: 5
section: BBNF
---

# Recovery Directives

`@recover` directives associate error-recovery synchronisation expressions with production rules. When the parser fails inside the named rule, it skips ahead to the next match of the sync expression and resumes parsing.

## Syntax

```bbnf
@recover ruleName syncExpr ;
```

The sync expression can be any valid BBNF expression — a literal, regex, alternation, etc.

## How It Works

1. Parser attempts to match the named rule
2. If parsing fails, the recovery mechanism activates
3. Input is scanned forward until the sync expression matches
4. The failed content is wrapped in a `Recovered` node
5. Parsing continues from after the sync match

This allows collecting **multiple errors** in a single parse pass, rather than stopping at the first failure.

## Example

```bbnf
declaration = propertyName ?w , ":" ?w , valueSpan , optSemicolon ;
@recover declaration /[^;{}]+;/ ;

qualifiedRule = selectorSpan ?w , ruleBlock ;
@recover qualifiedRule /[^}]+}/ ;
```

In this CSS grammar:

- If a declaration fails to parse, the parser skips to the next `;` and resumes
- If a qualified rule fails, the parser skips to the next `}` and resumes

## Multi-Error Diagnostics

With recovery in place, the parser can report all errors in the input:

```
Error 1: unexpected token at line 3, col 5
Error 2: missing semicolon at line 7, col 12
Error 3: invalid selector at line 15, col 1
```

Each error includes position information derived from the input offset where the failure occurred.

## Best Practices

- Use regex sync expressions that match common statement/block terminators
- For statement-level recovery, sync on `;` or `}`
- For block-level recovery, sync on `}`
- Recovery is greedy — it will skip as little input as possible to find the sync match
