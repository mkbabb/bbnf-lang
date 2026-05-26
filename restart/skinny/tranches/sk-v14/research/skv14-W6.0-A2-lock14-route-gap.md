# SK-V14 W6.0 A2: Lock 14 Route Gap

Date: 2026-05-26.
Scope: audit whether W6.0 root-runtime edits are route-authorized before source redress.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. The active frozen-root roster stops at W5D. `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1101-1140` defines W5B, W5C, and W5D owner paths, and `lock14_baseline.rs:1142-1203` builds the current owner set from those rosters.
2. The frozen roots omit the root CSS runtime. `FROZEN_ROOTS` at `lock14_baseline.rs:696-727` covers skinny roots such as `crates/runtime/src` and `crates/codegen/src`, but not `../crates/core/src/runtime/css_l4` as seen from the skinny workspace.
3. Parent-diff authorization has no W6.0 branch. `validate_authorized_parent_diff` currently admits SK-V14 W0/W1/W2/W4/W5A/W5B/W5C/W5D subjects at `lock14_baseline.rs:1624-1693`; no W6.0 subject can authorize a root runtime parent diff.
4. `gate-json` invokes this Lock 14 baseline before report processing, so a route gap can either hide root-runtime edits from the gate or fail after a W6.0 commit depending on how the path is staged.

## §2 — Recommendations (named falsifiability gates)

- Add a narrow `SK_V14_W6_0_OWNER_PATHS` roster before root runtime edits. It must include `../crates/core/src/runtime/css_l4/`, `../xtask/src/main.rs`, `../xtask/src/lib.rs`, `../xtask/src/regen.rs`, and the Lock 14 gate file only if the W6.0 plan owns root xtask generation.
- Extend `FROZEN_ROOTS` with `../crates/core/src/runtime/css_l4` and the exact root xtask paths W6.0 owns.
- Add tests proving W6.0 admits only CSS L4 root-runtime paths under W6.0 subjects, rejects W5D/W6.1/generic W6 subjects for the same paths, and rejects sibling runtime paths such as `../crates/core/src/runtime/json/...`.

## §3 — Risks (REDRESS entries to pre-block)

- Adding `../crates/core/src/runtime/` as a broad owner path would authorize the entire W6 aggregate in one sub-wave and erase sub-wave accountability.
- Letting W5D subjects authorize root runtime deletions would blur the W5D provider/template close with W6.0 implementation and violate the wave boundary.
- Leaving the root runtime outside frozen roots would make the Lock 14 baseline blind to W6.0.

## §4 — Sources (every external citation)

- Local repository only; no external sources.
