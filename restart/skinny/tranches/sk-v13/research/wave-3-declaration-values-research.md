# SK-V13 W3 Research - CSS Declaration-Value Expansion

Wave: W3. Phase: Research. Date: 2026-05-21.

## Scope

W3 is dispatchable because W1 and W2 are admitted. SPEC Section 6 targets a
CSS declaration-value expansion row and names `var()`, `calc()`, URL,
color-function, string, escaped-ident, and bounded recursion coverage.

Research was read-only. Six sidecar scopes were dispatched across owner-surface,
oracle/comparator, gate/report, Lock 14, fixture/performance, and feature
accounting. The first two completed before this synthesis; the remaining scopes
are non-blocking because W3 can use the existing W1/W2 gate pattern.

## Findings

1. The admitted SK-V12 declaration-values row is a fixed generated runtime
   profile, not general CSS grammar lowering. Codegen registers
   `css_l4_declaration_values` through
   `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs`,
   `grammar_profile.rs`, and the `RuntimeProvider::CssL4DeclarationValues`
   branch in `skinny/crates/codegen/src/lib.rs`.

2. The current generated declaration-values scanner is shallow. It emits
   declaration, hash, number, percentage, dimension, ident, function, delimiter,
   and close-paren facts, but it does not explicitly model quoted strings,
   URL tokens, escaped identifiers, or bounded nested function recursion. Those
   gaps are the exact SPEC W3 target.

3. The independent cssparser oracle already understands the W3 token families:
   nested `Function` blocks for `var()`, `calc()`, `clamp()`, and color
   functions; `QuotedString`; `UnquotedUrl`; and escaped identifiers after
   tokenizer normalization. That makes cssparser a viable Track 2 oracle for
   token facts while lightningcss remains the strict same-plane SOTA anchor.

4. Escaped identifiers and strings are risky if the fact plane claims exact
   source spans derived from normalized token values. W3 should either avoid
   per-token source-span claims for escaped values or keep any escaped feature
   as lexeme-normalized facts with byte-identical Track 1/cssparser/lightningcss
   artifacts.

5. W3 should not mutate the already-admitted
   `css_l4/declaration_values/direct_to_struct/main` row. A separate generated
   profile and row, `css_l4/declaration_values_extended/direct_to_struct/main`,
   preserves the SK-V12 admit as a maintain guard and prevents the existing W1b
   gate from coupling to a new fixture.

6. The rolling CSS feature rows W3 can honestly move are:
   `declarations`, `css_variables`, `calc_expressions`, `var_url_functions`,
   and `color_functions`. String and escaped-ident coverage are required
   evidence inside the grouped W3 row, but they do not have standalone rows in
   the current 24-feature rolling matrix.

7. W3 needs its own companion report and Criterion group, analogous to W2:
   Track 1 generated extended declaration-values lane, cssparser/golden oracle
   lane, and lightningcss strict lane. Gate-json must reread those lanes instead
   of accepting report-only Mbps.

8. Lock 14 must authorize the new CSS-specific generated profile paths without
   weakening generic forbidden-token scans. No generic JSON string, number, or
   source mapping policy should move in W3.

## Selected Research Conclusion

W3 should land one generated row:

```text
css_l4/declaration_values_extended/direct_to_struct/main
```

The fixture should cover custom properties, nested `var()`/`calc()`/`clamp()`,
quoted and unquoted URL forms, color functions, quoted string content, and an
escaped custom-property identifier. The fact plane remains a strict token/fact
stream with bounded recursion and same-run lightningcss comparator evidence.

The redress owner set should mirror W2 but use a new
`css_l4_declaration_values_extended` provider/runtime/report/gate path, keeping
the existing SK-V12 declaration-values row as a maintain guard.
