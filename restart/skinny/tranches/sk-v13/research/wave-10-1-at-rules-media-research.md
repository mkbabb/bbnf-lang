# SK-V13 W10.1 Research - CSS At-Rules And Media

Wave: W10.1. Phase: Research. Date: 2026-05-22.

## Scope

W10.1 is the first `W10.N` CSS parity expansion subwave after W4 closed
`visual_functions` under REDRESS-132. SPEC Section 14 authorizes one subwave
per remaining non-OUT_OF_SCOPE CSS feature not admitted by W2-W4.

Research was read-only. Six sidecar scopes covered fixture/oracle shape,
codegen/runtime owner paths, report/gate schema, lightningcss typed parity,
existing CSS at-rule grammar/runtime surfaces, and dispatch-contract risks.

## Findings

1. The correct dispatch label is `SK-V13 W10.1`, not W4. W4 is closed and
   admitted only `gradients`, `transforms`, `filters`, and `easing_functions`.

2. The stable grouped row name from the P3 gates is:

   ```text
   css_l4/at_rules_and_media/direct_to_struct/main
   ```

   The row maps only to the two still-open rolling feature rows:

   - `css_l4/at_rules_keyframes/direct_to_struct/main`
   - `css_l4/media_queries/direct_to_struct/main`

   `custom_at_rules`, `vendor_prefixes`, and `nested_rules` remain separate
   W10 subwaves because this fixture does not prove custom at-rule taxonomy,
   vendor-prefix handling, or nested-rule semantics.

3. The smallest strict fixture that exercises the selected row is:

   ```css
   @media screen and (min-width:1px){a{color:red}}
   @keyframes k{from,50%,to{opacity:1}}
   ```

   Include the final newline. Its identity is:

   ```text
   bytes=85
   fnv64=83cb4eb20e5253c7
   sha256=234dde82e1ead1e66be251a5d219892b666f16e853fcd5c03e67aca22fb07958
   ```

4. Fact coverage must include stylesheet root, one media at-rule, one media
   query with `screen` and `(min-width:1px)`, the qualified rule inside the
   media body, one keyframes at-rule, keyframes name `k`, a keyframe selector
   list `from,50%,to`, and the keyframe declaration body boundary. The nested
   declaration facts are evidence that the at-rule bodies were consumed, not
   admissions for declarations or selectors.

5. Strict lightningcss parity is not parse-success-only. The oracle must reject
   `CssRule::Unknown`, reject hidden `MediaCondition::Unknown`, and detect
   dropped keyframe blocks. Pretty-printed CSS is not a valid equality oracle
   because lightningcss normalizes media syntax, keyframe selectors, and
   declaration/value spelling.

6. Existing full CSS runtime surfaces already include `mediaRule`,
   `keyframesRule`, `genericAtRule`, and `mediaQueryList`, but they are not
   enough for a skinny generated row: `MediaRule.query` and keyframe selectors
   are currently raw strings in the runtime builder path, and generic literal
   token callbacks are not available. W10.1 therefore uses a CSS-specific
   generated fact stream, mirroring W2-W4, rather than generic emitter or
   substrate changes.

7. The owner paths mirror W2-W4 with a new `css_l4_at_rules_and_media`
   generated runtime profile, companion Criterion group, retained artifacts,
   report validator, lock14 owner paths, and xtask passthrough flag. No new
   directive, BIR variant, `BackendShape`, public substrate API, generic CSS
   policy branch, SIMD route, or x86 route is needed.

## Selected Research Conclusion

W10.1 should land one grouped generated CSS row:

```text
css_l4/at_rules_and_media/direct_to_struct/main
```

It admits `at_rules_keyframes` and `media_queries` only if strict equality,
feature coverage, independent oracle, retained lightningcss facts, Criterion
lane reread, and `Track 1 > lightningcss + 1` all pass. Custom at-rules,
vendor prefixes, nesting, logical properties, grid, flexbox, and typed
property groups remain open W10 subwaves.
