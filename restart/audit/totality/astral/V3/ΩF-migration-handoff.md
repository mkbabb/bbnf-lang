# Omega-F Migration + Handoff - Pass Omega V3 W2R

Pass: Pass Omega V3.
Date: 2026-05-25.
Scope: proposed HANDOFF and MIGRATION wording for REDRESS-183 / W2R.
Status: proposed CRUD-4 routing only; no V1 surface is edited here.

## Verdict

ACCEPT-WITH-CRUD-4.

`restart/HANDOFF.md` and `restart/MIGRATION.md` require mandatory updates after
G-Omega. The current handoff still describes Pass Omega V2 CRUD as in progress
and does not record the W2 rejection / W2R block. MIGRATION already routes
Pattern H to W6, but it should explicitly state that CSS L4 root runtime is
W6.0, not W2.

REDRESS-183 is the live disposition. W2 is rejected under `G-SK-V14-W2-R4`
because the current W2 destructive round-trip deletes both the skinny CSS L4
runtime profiles and `crates/core/src/runtime/css_l4/`, but no current
generator restores the root Pattern H runtime tree. This is a real wave-graph
cycle, not a paper deferral.

## Proposed Handoff Diff

```diff
diff --git a/restart/HANDOFF.md b/restart/HANDOFF.md
--- a/restart/HANDOFF.md
+++ b/restart/HANDOFF.md
@@
-Status: **Pass Omega V2 CRUD IN PROGRESS.**
+Status: **Pass Omega V3 W2R CRUD pending; SK-V14 implementation dispatch
+BLOCKED until W2 re-admits.** REDRESS-183 is the live edge: W2 rejected
+`G-W2-FULL-ROUNDTRIP` under `G-SK-V14-W2-R4` because the current W2
+dual-tree destructive gate requires `cargo xtask regen-css` to restore both
+`skinny/crates/runtime/src/grammars/css_l4_*` and
+`crates/core/src/runtime/css_l4/`, while the root generator restores only
+`crates/core/src/grammar/generated/css_l4.{rs,registry.json}`.
@@
+Pass Omega V3 W2R proposed correction: W2 becomes skinny-side `regen-css`
+only; W6.0 becomes CSS L4 root-runtime collapse; W6 remains nine sub-waves
+under the <=90 min/sub-wave and <=810 min aggregate caps. Until G-Omega
+authorizes the amendment and W2 re-admits under the amended gate, do not
+dispatch W3+ or any W8/W9/W10 new-admit wave.
```

## V3 Migration Receiver Table

```markdown
## 0.2 Pass Omega V3 W2R Migration Receiver

Pass Omega V3 W2R consumes REDRESS-183 and the W2R corrective packet. It changes
wave ownership and exit-gate wording only. It does not amend LOCKS or
ARCHITECTURE unless Omega-C later contradicts this no-op disposition.

| Receiver | V3 W2R migration rule |
|---|---|
| REDRESS-183 / W2 rejection | `G-W2-FULL-ROUNDTRIP` under `G-SK-V14-W2-R4` is REJECTED. Current W2 required `regen-css` to restore both skinny CSS L4 runtime profiles and `crates/core/src/runtime/css_l4/`; no current generator restores the core runtime Pattern H tree. Dispatch from the current state is blocked. |
| W2 amended receiver | After G-Omega V3 + CRUD, rerun W2 under a skinny-only `regen-css` gate: emit the existing CSS L4 runtime profile directories under `skinny/crates/runtime/src/grammars/css_l4_*`; run `check-css-l4-*` companions; run the skinny-only destructive round-trip; preserve the bypass-header detector across skinny and root runtime trees. W2 may not move CSS SOTA rows, touch `crates/core/src/runtime/css_l4/`, or claim Pattern H closure. |
| Core-runtime CSS L4 receiver | `crates/core/src/runtime/css_l4/` remains Pattern H runtime-root work. It moves to W6.0 after W5's generic generator template exists. W6.0 emits or collapses the CSS L4 root-runtime tree from grammar source + workspace metadata, then passes the destructive root-runtime round-trip for that tree. |
| Remaining W6 receiver | W6 remains nine sub-waves total, renumbered as W6.0-W6.8. W6.1-W6.8 cover the eight remaining Pattern H grammar runtime roots: `bbnf`, `bnf`, `css_pretty`, `csv`, `ebnf`, `google_sheets`, `json`, and `math`. |
| Dispatch block | W3+ remains blocked until the amended W2 admits. Stale notes saying W3/W5/W6/W7/W9/W10 may proceed independently after W2 rejection are non-controlling; hard entry gates and REDRESS-183 control. |
| Proposal boundary | V3 W2R CRUD artifacts are authorization logs. They do not authorize source, generated, gate, `RESULTS.md`, or new `REDRESS.md` edits except through the owning SK-V14 wave dispatch after CRUD. |
```

## Proposed Migration Diff

```diff
diff --git a/restart/MIGRATION.md b/restart/MIGRATION.md
--- a/restart/MIGRATION.md
+++ b/restart/MIGRATION.md
@@
-| Per-grammar runtime roots + Pattern H = 67 (3F-MIG-003) | **Pattern H = 67 hand-written per-grammar runtime files** across 9 dirs under `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/` ... receiver is **SK-V14 W6 PRUNE-4 with 9 sub-waves NOT 8** per S-P0 §2.3. |
+| Per-grammar runtime roots + Pattern H = 67 (3F-MIG-003) | **Pattern H = 67 hand-written per-grammar runtime files** across 9 dirs under `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/` ... receiver is **SK-V14 W6 PRUNE-4 with 9 sub-waves NOT 8** per S-P0 §2.3. Pass Omega V3 W2R assigns `crates/core/src/runtime/css_l4/` to W6.0 after W5; W2 owns only skinny-side `regen-css` output. |
```

## Next Dispatch Directive

Directive ID: `3F-DISPATCH-W2R-001`.

After G-Omega authorizes W2R and CRUD applies the document patches:

1. Re-dispatch SK-V14 W2 under the amended skinny-only gate.
2. W2 proof must include the `regen-css` command, CSS L4 profile emissions
   under skinny runtime, the seven exact companion commands, skinny-only
   destructive round-trip, bypass-header ownership detector, no CSS row
   movement, and no root runtime Pattern H claim:

   ```sh
   cargo xtask regen-css
   cargo xtask check-css-l4-at-rules-and-media
   cargo xtask check-css-l4-declaration-values
   cargo xtask check-css-l4-declaration-values-extended
   cargo xtask check-css-l4-nested-layout
   cargo xtask check-css-l4-stylesheet-selectors
   cargo xtask check-css-l4-vendor-and-custom-atrules
   cargo xtask check-css-l4-visual-functions
   rm -rf skinny/crates/runtime/src/grammars/css_l4_* &&
     cargo xtask regen-css &&
     git diff --exit-code -- skinny/crates/runtime/src/grammars
   ```
3. If W2 admits, proceed to W3, then W4, W5, W6.0..W6.8, W7.
4. If W2 rejects again, W3+ remains blocked and REDRESS records the new
   disposition.
5. Only after PRUNE-1..PRUNE-5 close may W8/W9/W10 new-admit waves proceed.
6. W11 closes SK-V14 per R10 or brackets SK-V15 through Pass Alpha.

Until then: no W2 rerun and no W3+ dispatch.
