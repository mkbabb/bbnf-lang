# Google Sheets Formula Formatter -- Test Cases

Test cases for validating a BBNF grammar for Google Sheets formulas in the playground.
Each case shows the **input** (compact formula) and **expected** formatted output
(pretty-printed with line breaks and indentation at `maxWidth=80, indent=2`).

---

## Category 1: Simple Formulas

These should remain on a single line -- they are short enough to fit within the
print width.

### 1.1 Binary arithmetic

**Input:**
```
=A1+B1
```

**Expected:**
```
=A1 + B1
```

### 1.2 Simple function call

**Input:**
```
=SUM(A1:A10)
```

**Expected:**
```
=SUM(A1:A10)
```

### 1.3 IF with short arguments

**Input:**
```
=IF(A1>0, "positive", "negative")
```

**Expected:**
```
=IF(A1 > 0, "positive", "negative")
```

### 1.4 String concatenation

**Input:**
```
="hello" & " " & "world"
```

**Expected:**
```
="hello" & " " & "world"
```

### 1.5 Simple negation

**Input:**
```
=-A1
```

**Expected:**
```
=-A1
```

### 1.6 Percentage

**Input:**
```
=A1*100%
```

**Expected:**
```
=A1 * 100%
```

### 1.7 Boolean literal

**Input:**
```
=AND(TRUE, FALSE)
```

**Expected:**
```
=AND(TRUE, FALSE)
```

---

## Category 2: Nested Functions

These test multi-argument function calls with cross-sheet references, enough
arguments to trigger wrapping.

### 2.1 VLOOKUP

**Input:**
```
=VLOOKUP(A1, Sheet2!A:C, 3, FALSE)
```

**Expected:**
```
=VLOOKUP(A1, Sheet2!A:C, 3, FALSE)
```

### 2.2 INDEX-MATCH (nested)

**Input:**
```
=INDEX(A1:D100, MATCH(A1, B:B, 0), MATCH("Header", 1:1, 0))
```

**Expected (wraps because >80 chars):**
```
=INDEX(
  A1:D100,
  MATCH(A1, B:B, 0),
  MATCH("Header", 1:1, 0)
)
```

### 2.3 SUMPRODUCT with array ops

**Input:**
```
=SUMPRODUCT((A1:A10>0)*(B1:B10))
```

**Expected:**
```
=SUMPRODUCT((A1:A10 > 0) * (B1:B10))
```

### 2.4 IFERROR wrapping VLOOKUP

**Input:**
```
=IFERROR(VLOOKUP(A1, data!A:D, 4, FALSE), "Not found")
```

**Expected:**
```
=IFERROR(
  VLOOKUP(A1, data!A:D, 4, FALSE),
  "Not found"
)
```

### 2.5 CONCATENATE with many args

**Input:**
```
=CONCATENATE(A1, " - ", B1, " (", C1, ")")
```

**Expected:**
```
=CONCATENATE(A1, " - ", B1, " (", C1, ")")
```

### 2.6 Nested SUBSTITUTE

**Input:**
```
=SUBSTITUTE(SUBSTITUTE(SUBSTITUTE(A1, "a", "b"), "c", "d"), "e", "f")
```

**Expected:**
```
=SUBSTITUTE(
  SUBSTITUTE(
    SUBSTITUTE(A1, "a", "b"),
    "c",
    "d"
  ),
  "e",
  "f"
)
```

### 2.7 COUNTIFS with multiple criteria

**Input:**
```
=COUNTIFS(A:A, ">0", B:B, "<100", C:C, "<>""")
```

**Expected:**
```
=COUNTIFS(
  A:A, ">0",
  B:B, "<100",
  C:C, "<>\"\""
)
```

---

## Category 3: LET and LAMBDA

These are the hardest cases. LET binds name-value pairs then a body expression;
LAMBDA declares parameters then a body. The formatter must understand the
semantic grouping of bindings.

### 3.1 Simple LET

**Input:**
```
=LET(x, 10, y, 20, x+y)
```

**Expected:**
```
=LET(
  x, 10,
  y, 20,
  x + y
)
```

### 3.2 LET with nested functions in body

**Input:**
```
=LET(data, A1:Z100, filtered, FILTER(data, INDEX(data,,1)>0), count, ROWS(filtered), IF(count>0, MAKEARRAY(count, 3, LAMBDA(r, c, INDEX(filtered, r, c))), "No data"))
```

**Expected:**
```
=LET(
  data, A1:Z100,
  filtered, FILTER(data, INDEX(data, , 1) > 0),
  count, ROWS(filtered),
  IF(
    count > 0,
    MAKEARRAY(
      count,
      3,
      LAMBDA(r, c, INDEX(filtered, r, c))
    ),
    "No data"
  )
)
```

### 3.3 LAMBDA definition and immediate call

**Input:**
```
=LAMBDA(x, y, x^2 + y^2)(3, 4)
```

**Expected:**
```
=LAMBDA(x, y, x ^ 2 + y ^ 2)(3, 4)
```

### 3.4 MAP with LAMBDA

**Input:**
```
=MAP(A1:A10, LAMBDA(cell, IF(cell>0, cell*2, 0)))
```

**Expected:**
```
=MAP(
  A1:A10,
  LAMBDA(cell, IF(cell > 0, cell * 2, 0))
)
```

### 3.5 REDUCE with LAMBDA

**Input:**
```
=REDUCE(0, A1:A10, LAMBDA(acc, val, acc+val))
```

**Expected:**
```
=REDUCE(
  0,
  A1:A10,
  LAMBDA(acc, val, acc + val)
)
```

### 3.6 SCAN with LAMBDA

**Input:**
```
=SCAN(0, A1:A10, LAMBDA(acc, val, acc+val))
```

**Expected:**
```
=SCAN(
  0,
  A1:A10,
  LAMBDA(acc, val, acc + val)
)
```

### 3.7 Nested LAMBDA (higher-order)

**Input:**
```
=LAMBDA(fn, MAP(A1:A10, fn))(LAMBDA(x, x*2))
```

**Expected:**
```
=LAMBDA(fn, MAP(A1:A10, fn))(
  LAMBDA(x, x * 2)
)
```

### 3.8 LET with LAMBDA binding

**Input:**
```
=LET(double, LAMBDA(x, x*2), triple, LAMBDA(x, x*3), MAP(A1:A10, LAMBDA(cell, double(cell) + triple(cell))))
```

**Expected:**
```
=LET(
  double, LAMBDA(x, x * 2),
  triple, LAMBDA(x, x * 3),
  MAP(
    A1:A10,
    LAMBDA(cell, double(cell) + triple(cell))
  )
)
```

---

## Category 4: Array Formulas

### 4.1 Array literal (row separator `;`, column separator `,`)

**Input:**
```
={1,2,3;4,5,6}
```

**Expected:**
```
={1, 2, 3; 4, 5, 6}
```

### 4.2 MAKEARRAY with LAMBDA

**Input:**
```
=MAKEARRAY(5, 3, LAMBDA(r, c, r*c))
```

**Expected:**
```
=MAKEARRAY(5, 3, LAMBDA(r, c, r * c))
```

### 4.3 BYROW with LAMBDA

**Input:**
```
=BYROW(A1:C10, LAMBDA(row, SUM(row)))
```

**Expected:**
```
=BYROW(A1:C10, LAMBDA(row, SUM(row)))
```

### 4.4 BYCOL with LAMBDA

**Input:**
```
=BYCOL(A1:C10, LAMBDA(col, AVERAGE(col)))
```

**Expected:**
```
=BYCOL(A1:C10, LAMBDA(col, AVERAGE(col)))
```

### 4.5 Large array literal (should wrap)

**Input:**
```
={1,2,3,4,5;6,7,8,9,10;11,12,13,14,15;16,17,18,19,20}
```

**Expected:**
```
={
  1, 2, 3, 4, 5;
  6, 7, 8, 9, 10;
  11, 12, 13, 14, 15;
  16, 17, 18, 19, 20
}
```

### 4.6 CHOOSECOLS / CHOOSEROWS

**Input:**
```
=CHOOSECOLS(A1:Z100, 1, 5, 10, 15, 20, 25)
```

**Expected:**
```
=CHOOSECOLS(A1:Z100, 1, 5, 10, 15, 20, 25)
```

---

## Category 5: Complex Real-World Formulas

### 5.1 LET + FILTER + SORT + LAMBDA pipeline

**Input:**
```
=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10,ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))
```

**Expected:**
```
=LET(
  raw, A2:E1000,
  filtered, FILTER(
    raw,
    (INDEX(raw, , 3) > 100) * (INDEX(raw, , 5) = "Active")
  ),
  sorted, SORT(filtered, 3, FALSE),
  IF(
    ROWS(sorted) > 0,
    MAP(
      SEQUENCE(MIN(10, ROWS(sorted))),
      LAMBDA(
        i,
        INDEX(sorted, i, 1)
          & " - "
          & TEXT(INDEX(sorted, i, 3), "$#,##0")
      )
    ),
    "No results"
  )
)
```

### 5.2 Deeply nested IFs

**Input:**
```
=IF(A1>=90, "A", IF(A1>=80, "B", IF(A1>=70, "C", IF(A1>=60, "D", "F"))))
```

**Expected:**
```
=IF(
  A1 >= 90,
  "A",
  IF(
    A1 >= 80,
    "B",
    IF(
      A1 >= 70,
      "C",
      IF(A1 >= 60, "D", "F")
    )
  )
)
```

### 5.3 SWITCH

**Input:**
```
=SWITCH(WEEKDAY(A1), 1, "Sunday", 2, "Monday", 3, "Tuesday", 4, "Wednesday", 5, "Thursday", 6, "Friday", 7, "Saturday", "Unknown")
```

**Expected:**
```
=SWITCH(
  WEEKDAY(A1),
  1, "Sunday",
  2, "Monday",
  3, "Tuesday",
  4, "Wednesday",
  5, "Thursday",
  6, "Friday",
  7, "Saturday",
  "Unknown"
)
```

### 5.4 IFS (multi-condition)

**Input:**
```
=IFS(A1>=90, "A", A1>=80, "B", A1>=70, "C", A1>=60, "D", TRUE, "F")
```

**Expected:**
```
=IFS(
  A1 >= 90, "A",
  A1 >= 80, "B",
  A1 >= 70, "C",
  A1 >= 60, "D",
  TRUE, "F"
)
```

### 5.5 QUERY (Google Sheets specific)

**Input:**
```
=QUERY(Sheet1!A1:F1000, "SELECT A, B, SUM(F) WHERE C = 'Active' AND D > 100 GROUP BY A, B ORDER BY SUM(F) DESC LABEL SUM(F) 'Total'", 1)
```

**Expected:**
```
=QUERY(
  Sheet1!A1:F1000,
  "SELECT A, B, SUM(F) WHERE C = 'Active' AND D > 100 GROUP BY A, B ORDER BY SUM(F) DESC LABEL SUM(F) 'Total'",
  1
)
```

### 5.6 ARRAYFORMULA wrapping complex expression

**Input:**
```
=ARRAYFORMULA(IF(A2:A<>"", IF(B2:B>C2:C, B2:B-C2:C, 0) * IF(D2:D="Yes", 1.1, 1), ""))
```

**Expected:**
```
=ARRAYFORMULA(
  IF(
    A2:A <> "",
    IF(B2:B > C2:C, B2:B - C2:C, 0)
      * IF(D2:D = "Yes", 1.1, 1),
    ""
  )
)
```

### 5.7 XLOOKUP (modern lookup)

**Input:**
```
=XLOOKUP(E1, A2:A100, B2:D100, "Not found", 0, 1)
```

**Expected:**
```
=XLOOKUP(E1, A2:A100, B2:D100, "Not found", 0, 1)
```

### 5.8 Complex LET with REDUCE building a string

**Input:**
```
=LET(names, FILTER(A2:A100, B2:B100="Active"), count, ROWS(names), result, REDUCE("", SEQUENCE(count), LAMBDA(acc, i, acc & IF(acc<>"", ", ", "") & INDEX(names, i, 1))), IF(count>0, count & " active: " & result, "None active"))
```

**Expected:**
```
=LET(
  names, FILTER(A2:A100, B2:B100 = "Active"),
  count, ROWS(names),
  result, REDUCE(
    "",
    SEQUENCE(count),
    LAMBDA(
      acc,
      i,
      acc & IF(acc <> "", ", ", "") & INDEX(names, i, 1)
    )
  ),
  IF(
    count > 0,
    count & " active: " & result,
    "None active"
  )
)
```

### 5.9 Nested LAMBDA with MAKEARRAY for matrix ops

**Input:**
```
=LET(m1, A1:C3, m2, E1:G3, rows, ROWS(m1), cols, COLUMNS(m2), inner, COLUMNS(m1), MAKEARRAY(rows, cols, LAMBDA(r, c, REDUCE(0, SEQUENCE(inner), LAMBDA(acc, k, acc + INDEX(m1, r, k) * INDEX(m2, k, c))))))
```

**Expected:**
```
=LET(
  m1, A1:C3,
  m2, E1:G3,
  rows, ROWS(m1),
  cols, COLUMNS(m2),
  inner, COLUMNS(m1),
  MAKEARRAY(
    rows,
    cols,
    LAMBDA(
      r,
      c,
      REDUCE(
        0,
        SEQUENCE(inner),
        LAMBDA(
          acc,
          k,
          acc + INDEX(m1, r, k) * INDEX(m2, k, c)
        )
      )
    )
  )
)
```

### 5.10 Mixed operators and comparisons

**Input:**
```
=IF(AND(A1>0, B1<100, OR(C1="X", C1="Y")), A1*B1/100+10, -1)
```

**Expected:**
```
=IF(
  AND(A1 > 0, B1 < 100, OR(C1 = "X", C1 = "Y")),
  A1 * B1 / 100 + 10,
  -1
)
```

---

## Edge Cases

### E.1 Empty arguments

**Input:**
```
=INDEX(A1:D10,,3)
```

**Expected:**
```
=INDEX(A1:D10, , 3)
```

### E.2 Nested parentheses (no function)

**Input:**
```
=((A1+B1)*C1)/D1
```

**Expected:**
```
=((A1 + B1) * C1) / D1
```

### E.3 Unary minus in expression

**Input:**
```
=SUM(-A1, -B1, -C1)
```

**Expected:**
```
=SUM(-A1, -B1, -C1)
```

### E.4 Cross-sheet with spaces in name

**Input:**
```
='Sheet With Spaces'!A1+B1
```

**Expected:**
```
='Sheet With Spaces'!A1 + B1
```

### E.5 R1C1-style reference (less common)

**Input:**
```
=R1C1+R2C2
```

**Expected:**
```
=R1C1 + R2C2
```

### E.6 Named range

**Input:**
```
=SUM(SalesData)
```

**Expected:**
```
=SUM(SalesData)
```

### E.7 Formula with only a string

**Input:**
```
="Hello, World!"
```

**Expected:**
```
="Hello, World!"
```

### E.8 Comparison chain

**Input:**
```
=IF(AND(A1>=B1, B1>=C1, C1>=D1), "Descending", "Not")
```

**Expected:**
```
=IF(
  AND(A1 >= B1, B1 >= C1, C1 >= D1),
  "Descending",
  "Not"
)
```

---

## Notes for Grammar Design

1. **Operator spacing**: Binary operators (`+`, `-`, `*`, `/`, `^`, `&`, `=`,
   `<>`, `>=`, `<=`, `>`, `<`) should have spaces around them in the formatted
   output. Unary `-` should not have a space after it.

2. **Function call wrapping**: When a function call fits on one line, keep it
   flat. When it exceeds `maxWidth`, break after `(` and before `)`, indenting
   arguments by one level. Each argument gets its own line.

3. **Comma spacing**: Always one space after `,` in flat mode. In broken mode,
   commas are at end-of-line with newline after.

4. **LET binding pairs**: LET arguments come in pairs (`name, value`) except
   the final body expression. The formatter should keep each pair on one line
   when possible: `name, value,`.

5. **LAMBDA parameters**: Similar to LET -- parameter names are simple
   identifiers, the last argument is the body expression.

6. **Array literals**: `{...}` with `,` as column separator and `;` as row
   separator. Rows should break to separate lines when the array is too wide.

7. **Empty arguments**: Google Sheets allows empty arguments (e.g.,
   `INDEX(range,,col)`). The grammar must accept consecutive commas with
   optional whitespace between them.

8. **Sheet references**: `Sheet1!A1`, `'Sheet Name'!A1:B10`. The `!` is part
   of the reference, not an operator.

9. **Immediate LAMBDA invocation**: `LAMBDA(...)(args)` -- the second set of
   parens is a function call on the result.
