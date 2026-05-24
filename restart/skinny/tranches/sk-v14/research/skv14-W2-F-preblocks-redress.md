# SK-V14 W2-F: Pre-Blocks And REDRESS

Date: 2026-05-24.
Scope: Identify W2 dependencies, pre-blocked routes, and REDRESS ledger constraints.
Output: this file.

## §1 — Findings (concrete, file:line cited)

- W2 is `regen-css xtask (R4)` and is conditional on W1 close in the wave manifest at `restart/skinny/tranches/sk-v14/SPEC.md:235-240`; W1 is now closed, so W2 entry is open.
- W2's entry gate requires W1 admitted, 15 CSS `.bbnf` files present, and a plan that names the `regen-css` USAGE entry, generator input contract, and dual-tree output destinations at `restart/skinny/tranches/sk-v14/SPEC.md:476-480`.
- W2's tasks are command, generator, and companion-check tasks only: add `regen-css`, consume the 15 CSS files plus metadata, parameterize the generator family, and add `check-css-l4-<provider>` companions at `restart/skinny/tranches/sk-v14/SPEC.md:482-487`.
- W2's exit gate requires destructive regen round-trip, `regen-css` command visibility, companion existence, bypass-header ownership, Lock 14 baseline gate, and full-table JSON maintain at `restart/skinny/tranches/sk-v14/SPEC.md:489-496`.
- W2 rejection blocks W4 and W8 because PRUNE-2 needs the CSS regeneration recovery path and W8 needs the grammar-derived pipeline at `restart/skinny/tranches/sk-v14/SPEC.md:512-515`. The totality handoff also says W2 must precede W4 and W3/W4 sequence after W2 at `restart/audit/totality/p3/3F-migration-handoff.md:238-246`.
- W1 already added REDRESS Items 161-182 for JSON PRUNE-1 at `skinny/REDRESS.md:5043-5088`. W2 should not create duplicate JSON prune rows.

## §2 — Recommendations (named falsifiability gates)

- `G-W2-NON-ADMIT`: W2 plan and redress must state that no CSS SOTA admission, row movement, or throughput claim lands in W2; it is gate evidence only.
- `G-W2-REDRESS-ONLY-ON-REJECT`: create a new REDRESS item only if W2 rejects, naming the failing round-trip case or missing grammar-derived emission path as required by `restart/skinny/tranches/sk-v14/SPEC.md:508-510`.
- `G-W2-DEPENDENCY-PROOF`: cite W1 close and the 15-file CSS source inventory before implementation dispatch.
- `G-W2-NO-OLD-CSS-AUTHORITY`: prior CSS REDRESS items may be cited only as history/pre-block evidence, not as admission authority for W2.

## §3 — Risks (REDRESS entries to pre-block)

- REDRESS 113 records an earlier W2 block where a wave tried to create first measurable non-JSON movement without a baseline at `skinny/REDRESS.md:3340-3355`; SK-V14 W2 must avoid defining admit thresholds or moving rows.
- REDRESS 122 is a useful pattern for support/correctness prerequisites closing without row movement, and explicitly admits no CSS L4 row, lightningcss movement, JSON guard row, or SIMD throughput at `skinny/REDRESS.md:3605-3632`.
- REDRESS 123 and later historical CSS scaffold/admit rows are audit-zeroed under SK-V14; `skinny/REDRESS.md:3634-3646` shows the earlier CSS declaration-values scaffold, but W2 may not reuse it as current admission authority.

## §4 — Sources (every external citation)

No external citations. Local repository sources only.
