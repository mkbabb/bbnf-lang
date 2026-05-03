# Tranche BC — Cleanup Pass + Discipline Codification

> **Letter status — repurposed at master `40092b28` (post-AZ-IV close).**
> The previous BC tranche ("Shared Precepts Consumer Rollout" — orchestration meta-tranche; closed cleanly with the precepts submodule pinned at `e490e8ed`) is **archived unmodified** at `docs/tranches/BC/orchestration-archive-2026-04-30/`. The close artefact, audit, research, and waves directories are all preserved. Per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`, the canonical post-AZ-IV letter sequence is **AZ → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) → BD+**. The BC letter is repurposed as the cleanup tranche; the orchestration content is preserved unchanged in the archive directory.
>
> BC opens after BB close.

## Thesis

BC absorbs the residual carries that survived BA (direct-projection) + BB (rule-discovery) without invalidating either's thesis: the Audit-A TRANSPOSE bucket (12 items), the AUDIT-B routed splits (`runtime/css_l4/builder.rs` 1014 LOC, `passes/types/mod.rs` 786 LOC, `csp_strategy/mod.rs` further splits), the worktree fixture symlink contract (W6.2 known miss), the samply 7-artefact contract canonicalization (no more environmental gating), and the post-BA-and-BB substrate-audit residual.

## Wave Table (skeleton; full body produced at BC open)

| Wave | Scope |
|---|---|
| BC.W0 | Truth + cleanup-substrate inventory; refresh substrate-audit denominator after BA + BB; identify residual TRANSPOSE items |
| BC.W1 | AUDIT-B routed splits land (css_l4/builder.rs, passes/types/mod.rs, csp_strategy/mod.rs splits) |
| BC.W2 | Worktree fixture symlink contract codified (W6.2 carry) — `data/{json,css,bbnf,sheets}` materialise on worktree open via `xtask worktree-init` or equivalent |
| BC.W3 | Samply 7-artefact contract canonicalization — every close-state row carries the contract; environmental gating retires |
| BC.W4 | Audit-A TRANSPOSE residue absorption (any items not closed in BA or BB) |
| BC.W5 | Final cross-repo discipline (csp-solver canonical-source split refresh; bbnf-regex sub-crate-of-parse-that resolution) |
| BC.W6 | Measurement + close + FINAL.md |

## Non-Routable Carries

Whatever survived BA close + BB close as routed carry, with named close criterion per row.

## TS / WASM Position

The TS/WASM re-engineering tranche (BD candidate) opens after BC close. BC does not touch TS or WASM.
