# BBNF Specification

BBNF (Better Backus-Naur Form) is a grammar notation for defining parsers. It extends
EBNF with operators for whitespace handling, value projection (skip/next), set
difference, and mapping functions. BBNF grammars serve as the shared contract between
the TypeScript and Rust implementations of `parse-that`.

A BBNF file consists of **import directives** and **production rules** in any order,
interleaved with comments. Each rule defines a named nonterminal in terms of an
expression built from terminals, nonterminal references, and operators.

## Import Directives

Import directives allow a grammar to reference rules defined in other `.bbnf` files.
They may appear at any position in the file.

### Whole-file import

Imports all rules from the specified file into the local scope:

```
@import "path/to/base.bbnf" ;
```

### Selective import

Imports only the named rules from the specified file:

```
@import { number, integer, whitespace } from "path/to/common.bbnf" ;
```

### Semantics

- **Path resolution**: Paths are relative to the importing file's directory. If no
  extension is given, `.bbnf` is appended.
- **Non-transitive**: If A imports B and B imports C, A does **not** see C's rules.
  A must import C explicitly.
- **Transitive dependency unfurling**: Selective imports include any rules the
  named imports reference. `@import { percentage } from "base.bbnf"` also
  brings `number` and `percentageUnit` if `percentage` depends on them.
- **Local shadows imports**: A locally defined rule takes precedence over an imported
  rule of the same name (with a warning).
- **Circular imports are allowed**: A module's symbol table is registered before
  its imports are resolved, so mutual import cycles don't deadlock or error.
- **Name conflicts are errors**: Two imports defining the same rule name is an error.

## Recovery Directives

A `@recover` directive associates an error-recovery synchronisation expression with a
production rule. When the parser fails inside the named rule, it skips ahead to the
next match of the sync expression and resumes:

```
@recover rule syncExpr ;
```

The sync expression may be any valid BBNF expression (literal, regex, alternation, etc.).

## Pretty Directives

A `@pretty` directive attaches formatting hints to a production rule for the prettify
code generator. Each hint keyword controls how the rule's `to_doc()` implementation
emits `Doc` nodes:

```
@pretty rule hint1 hint2 ;
```

Recognised hints: `group`, `indent`, `dedent`, `block`, `blankline`, `nobreak`,
`softbreak`, `hardbreak`, `compact`, `fast`, `off`.

A grammar-wide meta-directive controls heuristic inference for un-annotated rules:

```
@pretty * auto ;     // auto (default), minimal, or off
```

When no `@pretty` directive is present and heuristics are enabled, the code generator
auto-infers hints from rule shape (e.g. toplevel entry points, brace-delimited blocks,
large compound types).

## Token Directives

A `@token` directive marks a production rule as a lexical token:

```
@token ruleName ;
```

Token rules are forced span-eligible and use fusion-style inlining—the rule body is
inlined at every call site, but the enum variant is preserved. This differs from
`@inline`, which eliminates the variant entirely. `@token` is compatible with `@pretty`
directives that need to reference the variant for formatting.

## Production Rules

A production rule binds a name (the left-hand side) to an expression (the right-hand
side), terminated by `;` or `.`:

```
name = expression ;
```

The left-hand side is an **identifier**: one or more characters matching
`[_a-zA-Z][_a-zA-Z0-9-]*`. Hyphens are allowed in identifiers (e.g. `color-value`).

The first rule in a grammar is treated as the start symbol by convention.

### Per-Expression Mapping `->` (Rust only)

The `->` operator maps the result of any factor expression through a Rust function,
closure, or constant. It is a postfix operator at the same precedence level as `?w`:

```
number = /[0-9]+/ -> |s: Span| -> f64 { s.as_str().parse().unwrap() } ;
```

Three forms are supported:

| Form | Syntax | Example |
|------|--------|---------|
| Closure | `-> \|params\| -> RetType { body }` | `-> \|s: Span\| -> f64 { s.as_str().parse().unwrap() }` |
| Function path | `-> path::to::func` | `-> crate::parse_color` |
| Constant | `-> value` | `-> 0u8` |

Because `->` attaches to individual factors, different alternation branches can map
to different values or types:

```
lengthUnit = "px" -> 0u8 | "em" -> 1u8 | "rem" -> 2u8 | "vw" -> 3u8 ;
```

The `=>` operator is a legacy alias that applies a mapping to the entire rule
right-hand side (equivalent to wrapping the RHS in parentheses and applying `->`):

```
// These are equivalent:
number = /[0-9]+/ => |s: &str| -> i64 { s.parse().unwrap() } ;
number = ( /[0-9]+/ ) -> |s: &str| -> i64 { s.parse().unwrap() } ;
```

This feature is not available in the TypeScript implementation.

## Terminal Expressions

### String Literals

String literals match an exact sequence of characters. They are delimited by double
quotes, single quotes, or backticks:

```
"hello"
'world'
`template`
```

Escape sequences within string literals use the backslash character. Any character
preceded by `\` is treated literally:

| Sequence | Meaning |
|----------|---------|
| `\"` | Literal `"` |
| `\\` | Literal `\` |
| `\n` | Literal `n` (not a newline -- no C-style escapes) |

In the TypeScript implementation, the backslash-prefixed character is unescaped during
parsing (i.e. `\\` in the grammar source becomes a single `\` in the matched string).

### Regular Expressions

Regular expressions match a pattern against the input. They are delimited by forward
slashes:

```
/[0-9]+/
/[_a-zA-Z][_a-zA-Z0-9]*/
```

Escape sequences within regex delimiters follow the same backslash rule:
`\/` produces a literal `/`, `\\` produces a literal `\`. The content between the
delimiters is passed directly to the host language's regex engine (JavaScript `RegExp`
or Rust `regex::Regex`).

The TypeScript implementation additionally supports regex flags after the closing
delimiter (e.g. `/pattern/i`), matching the JavaScript `RegExp` flag set `[gimuy]`.

### Epsilon

The keyword `epsilon` (or the Unicode symbol `ε`) matches the empty string without
consuming any input:

```
empty = epsilon ;
maybe = "x" | ε ;
```

## Nonterminal References

A bare identifier in an expression refers to another production rule by name:

```
value = object | array | string | number | bool | null ;
```

Nonterminal references are resolved lazily, so rules may appear in any order and
mutually recursive grammars are supported.

## Operators

Operators are listed below from **lowest** to **highest** precedence. Higher-precedence
operators bind more tightly.

### 1. Alternation `|` (lowest precedence)

Ordered choice. Tries each alternative left to right and returns the first successful
match:

```
bool = "true" | "false" ;
```

When all alternatives are string literals, the code generators may emit an optimized
dispatch table instead of sequential trial-and-error.

### 2. Concatenation `,`

Sequence. Matches each operand in order and collects the results into a tuple (Rust)
or array (TypeScript):

```
pair = key , ":" , value ;
```

The comma is **optional** when the operands are unambiguous, but including it is
recommended for clarity. Both implementations parse `binary_factor` items separated
by optional commas at this precedence level.

### 3. Skip `<<` and Next `>>`

Value-projection operators. Both match two sub-expressions in sequence but discard
one side of the result:

- `A << B` -- match `A` then `B`, **keep the value of `A`**, discard `B`.
- `A >> B` -- match `A` then `B`, **keep the value of `B`**, discard `A`.

These are the workhorses for discarding delimiters and whitespace:

```
array = "[" >> elements << "]" ;
field = "," >> value ;
```

Skip and next are **left-associative** binary operators at equal precedence, so they
can be chained:

```
// parses "(", then inner, then ")" -- keeps inner
wrapped = "(" >> inner << ")" ;
```

### 4. Minus `-`

Set difference. Matches `A` only if `B` does **not** match at the same position:

```
non_digit = /\w/ - /\d/ ;
```

In the generated parser this compiles to `A.not(B)`: attempt `A`, but fail if `B`
would also succeed. Same precedence level as skip and next.

### 5. Quantifiers `*`, `+`, `?` (postfix, high precedence)

Repetition and optionality, applied as a **postfix** suffix to a term:

| Operator | Name | Meaning |
|----------|------|---------|
| `*` | Many | Zero or more repetitions |
| `+` | Many1 | One or more repetitions |
| `?` | Optional | Zero or one occurrence |

```
digits = /[0-9]/ + ;
items  = item * ;
sign   = ("+" | "-") ? ;
```

### 6. Optional Whitespace `?w` and Mapping `->` (postfix, high precedence)

The `?w` operator wraps the preceding term so that optional whitespace is consumed
(and discarded) **before and after** it:

```
comma = "," ?w ;
rule  = lhs , "=" ?w , rhs ;
```

`expr ?w` is equivalent to `ws* >> expr << ws*` where `ws` matches `\s`. This is
distinct from `?` (which means "zero or one") -- the trailing `w` makes it a
whitespace-trimming operator.

The `->` operator maps the preceding factor through a Rust closure, function path,
or constant value (see [Per-Expression Mapping](#per-expression-mapping---rust-only)).
It has the same precedence as `?w`.

### 7. Grouping Constructs (highest precedence)

Parentheses and brackets override precedence and introduce special semantics:

| Syntax | Name | Meaning |
|--------|------|---------|
| `( expr )` | Group | Parenthesized sub-expression (no semantic change) |
| `[ expr ]` | Optional group | Equivalent to `( expr ) ?` |
| `{ expr }` | Repetition group | Equivalent to `( expr ) *` |
| `@{ expr }` | Span capture | Parse `expr` for validation, return raw `Span` |

```
// These two are equivalent:
array_a = "[" , [ items ] , "]" ;
array_b = "[" , ( items ) ? , "]" ;

// These two are equivalent:
list_a = item , { "," , item } ;
list_b = item , ( "," , item ) * ;
```

### Span Capture `@{ expr }` (Rust only)

The `@{...}` operator parses `expr` for structural validation but discards the
typed result and returns a raw `Span` covering the matched input. This is useful
when the grammar must validate syntax (e.g., balanced parentheses, correct
argument structure) but the caller wants to defer semantic processing:

```
// Validate color function syntax, return raw span for deferred conversion
colorFunction = @{ colorType , "(" >> args << ")" } ;

// Preserve raw URL text for source maps
urlFunction = @{ "url" , "(" >> content << ")" } ;
```

Type inference always produces `TypeDesc::Span` for `@{...}` expressions.
The inner expression is parsed normally — errors, recovery, and whitespace
handling all apply — but the generated code constructs a `Span` from the
start and end offsets instead of the typed result.

This feature is not available in the TypeScript implementation.

## Precedence Summary

From lowest to highest:

| Level | Operator(s) | Associativity | Description |
|-------|-------------|---------------|-------------|
| 1 | `\|` | left | Alternation (ordered choice) |
| 2 | `,` | left | Concatenation (sequence) |
| 3 | `<<` `>>` `-` | left | Skip, next, minus |
| 4 | `*` `+` `?` `?w` `->` | postfix | Quantifiers, whitespace, mapping |
| 5 | `(` `)` `[` `]` `{` `}` `@{` `}` | -- | Grouping constructs |

## Comments

BBNF supports two comment styles, matching C/JavaScript conventions:

```
// Line comment: extends to the end of the line

/* Block comment:
   may span multiple lines */
```

Comments may appear before or after production rules and before or after individual
factors within an expression. Both implementations preserve comments in the AST for
round-tripping and documentation generation.

## Debug Expressions (Rust only)

When the `debug` flag is set in the Rust code generator's `ParserAttributes`, each
generated nonterminal parser is wrapped in a `.debug(name)` call. This is a codegen-
level feature controlled by an attribute on the derive macro, not a syntactic construct
in the grammar source itself. The resulting `DebugExpression` AST node pairs the
inner expression with the nonterminal's name for runtime tracing.

## Full Grammar

BBNF is self-describing. The following grammar, written in BBNF, defines the BBNF
notation itself:

```bbnf
identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;

literal = "\"" , /(\\.|[^"\\])*/  , "\""
        | "'"  , /(\\.|[^'\\])*/  , "'"
        | "`"  , /(\\.|[^`\\])*/  , "`" ;

big_comment = ( "/*" , /[^\*]*/ , "*/" ) ?w ;
comment = ( "//" , /.*/ ) ?w ;

regex = "/" , /(\\.|[^\/])+/ , "/" ;

lhs = identifier ;

term = "ε"
     | identifier
     | literal
     | regex
     | "(" , rhs ?w , ")"
     | "[" , rhs ?w , "]"
     | "{" , rhs ?w , "}" ;

mapper = "|" , /[^;]*/ | /[^\s|,;]+/ ;

factor = big_comment ? , (
      term ?w , "?w"
    | term ?w , "?"
    | term ?w , "*"
    | term ?w , "+"
    | term
) , ( "->" ?w , mapper ) ? , big_comment ? ;

binary_operators = "<<" | ">>" | "-" ;

binary_factor = factor , ( binary_operators ?w , factor ) * ;

concatenation = ( binary_factor ?w , "," ? ) + ;
alternation = ( concatenation ?w , "|" ? ) + ;

rhs = alternation ;

rule = lhs , "=" ?w , rhs ?w , ( ";" | "." ) ;

grammar = ( comment ? , rule ?w , comment ? ) * ;
```

## Examples

### JSON

```bbnf
null = "null" ;
bool = "true" | "false" ;

number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;

comma = "," ?w ;
colon = ":" ?w ;

string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;
array = "[" >> (( value << comma ? ) *)?w << "]" ;

pair = string, colon >> value ;
object = "{" >> (( pair << comma ? ) *)?w << "}" ;

value = object | array | string | number | bool | null ;
```

### CSV

```bbnf
// CSV grammar (simplified RFC 4180)

DQUOTE = "\"" ;
escaped = DQUOTE >> /[^"]*/ << DQUOTE ;
textdata = /[^,"\r\n]+/ ;

field = escaped | textdata ;

record = field, ( "," >> field ) * ;

csv = record, ( /\r?\n/ >> record ) * ;
```

### Arithmetic Expressions

```bbnf
expr = term, { ("+" | "-"), term } ;
term = factor, { ("*" | "/"), factor } ;

wrapped = "(", expr, ")" ;

factor = number | wrapped ;

number = /(\d+)?(\.\d+)?([eE][-+]?\d+)?/ ;
```

## Implementation Notes

- **TypeScript**: Grammars are parsed at runtime by `BBNFGrammar` (in `grammar.ts`)
  and compiled to parser combinator trees by `ASTToParser` (in `generate.ts`). Mapping
  functions are not supported; post-parse transforms are applied programmatically.

- **Rust**: Grammars are parsed at compile time via a proc-macro derive
  (`#[derive(BBNF)]`). The derive macro reads `.bbnf` files, builds an AST, and emits
  Rust source code that constructs the equivalent parser combinator tree. Inline
  mapping functions (`=> |x| ...`) are supported and compiled directly into the
  generated code. The codegen also supports optional left-recursion removal, dispatch
  table optimization, and debug wrapping via derive attributes.
