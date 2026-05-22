# SK-V13 W10.2 Research - CSS Vendor And Custom At-Rules

Wave: W10.2. Phase: Research. Date: 2026-05-22.

## Scope

W10.2 is the next `W10.N` CSS parity expansion subwave after W10.1 admitted
`at_rules_keyframes` and `media_queries` under REDRESS-133. SPEC Section 14
authorizes one subwave per remaining non-OUT_OF_SCOPE CSS feature not already
admitted by W2-W4/W10.1.

Research was read-only. Six sidecar scopes covered fixture/oracle shape,
codegen/runtime owner paths, report/gate schema, lightningcss typed parity,
current CSS vendor/custom grammar surfaces, and dispatch-contract risks.

## Findings

1. The still-open rolling rows with the smallest shared taxonomy are:

   - `css_l4/vendor_prefixes/direct_to_struct/main`
   - `css_l4/custom_at_rules/direct_to_struct/main`

   These map to one grouped generated row:

   ```text
   css_l4/vendor_and_custom_atrules/direct_to_struct/main
   ```

2. The row must not claim nested rules, logical properties, grid, or flexbox.
   Those require recursive rule/block or typed layout value facts and remain
   later W10 subwaves. W10.2 only proves vendor-prefixed declarations,
   vendor-prefixed keyframes, and custom at-rule taxonomy.

3. The smallest strict fixture that exercises the selected row is:

   ```css
   @custom-media --narrow (max-width:30em);
   @-webkit-keyframes fade{from{opacity:0}to{opacity:1}}
   a{-webkit-user-select:none;-moz-user-select:none;user-select:none}
   ```

   Include the final newline. Its identity is:

   ```text
   bytes=162
   fnv64=b7905e059e2fe40e
   sha256=367122942a2c937654b35a1065edc33ae85694a4bcd02b50d6ed50ea1631995f
   ```

4. Fact coverage must include stylesheet root, one `@custom-media` rule,
   the custom media name `--narrow`, its media feature `max-width:30em`, one
   vendor-prefixed `@-webkit-keyframes` rule, keyframe selectors `from` and
   `to`, and declarations for `-webkit-user-select`, `-moz-user-select`, and
   unprefixed `user-select`.

5. Strict lightningcss parity is typed AST parity, not parse success. The
   sidecar must assert `CssRule::CustomMedia`, vendor-prefixed
   `CssRule::Keyframes`, and style declarations that preserve the three
   user-select spellings. Unknown-at-rule coverage is intentionally excluded
   from this first vendor/custom row because `CssRule::Unknown` would need a
   broader syntax matrix to prove every accepted/rejected variant.

6. Existing W10.1 at-rules/media code proves the block-level fact-stream and
   Criterion reread pattern. W10.2 can reuse that local pattern with a new
   `css_l4_vendor_and_custom_atrules` generated profile, report schema,
   companion Criterion group, retained artifacts, lock14 owner paths, and
   xtask passthrough flag. No generic emitter, substrate, JSON, SIMD, x86,
   directive, BIR, or `BackendShape` route is needed.

## Selected Research Conclusion

W10.2 should land one grouped generated CSS row:

```text
css_l4/vendor_and_custom_atrules/direct_to_struct/main
```

It admits `vendor_prefixes` and `custom_at_rules` only if strict equality,
feature coverage, independent oracle, retained lightningcss facts, Criterion
lane reread, and `Track 1 > lightningcss + 1` all pass. Nested rules, logical
properties, grid, flexbox, and typed property groups remain open W10 subwaves.
