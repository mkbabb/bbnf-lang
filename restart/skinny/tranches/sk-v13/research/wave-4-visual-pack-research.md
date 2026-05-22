# SK-V13 W4 Research - CSS Visual Functions Pack

Wave: W4. Phase: Research. Date: 2026-05-22.

## Scope

W4 is dispatchable because W2 and W3 are admitted. SPEC Section 7 authorizes
one or more CSS visual, at-rule, nesting, or taxonomy packs, with a split
required if LOC or redress risk would overflow the cap.

Research was read-only. Six sidecar scopes were dispatched across visual
functions, at-rules/keyframes, nesting/vendor/custom taxonomy, telemetry/gate
plumbing, codegen/runtime owner paths, and oracle fixture strategy. The local
critical-path read found that the visual-functions pack is the only W4 pack
that can reuse the W3 declaration-value token/fact machinery without first
landing stylesheet block-structure changes.

## Findings

1. The open rolling CSS matrix rows after W3 are:
   `at_rules_keyframes`, `nested_rules`, `gradients`, `transforms`, `filters`,
   `easing_functions`, `media_queries`, `vendor_prefixes`, `custom_at_rules`,
   `logical_properties`, `grid`, `flexbox`, and `typed_property_groups`.

2. The visual rows `gradients`, `transforms`, `filters`, and
   `easing_functions` are declaration-value families. They can be exercised by
   a single strict CSS fixture containing `linear-gradient`, transform
   functions, filter functions, and timing functions. This aligns with the W3
   extended declaration-value row rather than requiring a new stylesheet AST.

3. At-rules, media queries, keyframes, nested rules, vendor taxonomy, custom
   at-rules, logical properties, grid, flexbox, and typed property groups need
   block-level or property-taxonomy evidence beyond the W3 value scanner. They
   are still admissible, but they should be W10 subwaves unless a sidecar
   proves a smaller same-plane row.

4. The W2/W3 telemetry shape is reusable: one measured grouped CSS row, a
   covered-feature mapping, retained Track 1/oracle/lightningcss artifacts, and
   a gate-json companion flag that rereads Criterion lanes instead of trusting
   report-only Mbps.

5. The safest W4 measured row is:

   ```text
   css_l4/visual_functions/direct_to_struct/main
   ```

   The row should cover exactly these feature rows:

   - `gradients`
   - `transforms`
   - `filters`
   - `easing_functions`

6. The fact plane should stay token/fact based, not semantic-normalization
   based. The independent oracle can be a golden fact stream plus cssparser or
   lightningcss strict parse checks, mirroring W2/W3 where parser AST
   normalization could otherwise drift from source-token facts.

7. Lock 14 risk is limited if W4 keeps visual semantics in
   `css_l4_visual_functions` generated/runtime paths. Generic edits should be
   limited to provider registration, runtime export, report/gate plumbing,
   lock14 owner inventory, and xtask passthrough.

## Selected Research Conclusion

W4 should land the visual-functions pack as one grouped generated CSS row:

```text
css_l4/visual_functions/direct_to_struct/main
```

The row should admit `gradients`, `transforms`, `filters`, and
`easing_functions` if strict equality and lightningcss + 1 pass. At-rule,
nesting, vendor/custom, logical, grid, flexbox, and typed-property rows remain
routed to W10 subwaves, not because they are impossible, but because they need a
different block/property evidence shape than the W4 visual value pack.
