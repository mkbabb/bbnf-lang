# Tranche AU — Progress Log

Operational protocol: see `/INSTRUCTIONS.md` at repo root.

## Pre-AU (AT audit findings)

### Audit methodology

6 parallel worktree-isolated agents performed deep analysis:

1. **Codegen regression audit** — expanded AQ vs AT JSON parsers,
   identified dead payload captures, quantified overhead sources
2. **Projection activation audit** — all 4 grammars push counts,
   payload firing, KvPair status, .map(|_| ()) catalog
3. **Prior tranche gates** — full test suite, bootstrap idempotency
4. **JSON hot path profile** — instruction counts, push method costs,
   `branch_pushes_children` misclassification root cause
5. **CSS+Sheets+BBNF profiles** — scanner activation, fused scanner
   absence, per-grammar bench numbers
6. **Deferred items + arch debt** — 11-tranche ParsedGrammar deferral,
   dead StructRegistry, schema stubs

### Root cause of JSON regression

`branch_pushes_children()` in `alt.rs:67-69` has `_ => true` catch-all
that misclassifies nested leaf Alt nodes (inlined `bool` rule) as
compound. Tape surgery forces `mark_children + push_compound` on ALL
branches, making ALL payload captures dead stores. The f64/bool/u8
payloads are computed but never stored — a correctness bug.

### Architectural verdicts from audit

- **kind_meta packing: KEEP** — strictly better than meta Vec
- **__payload_tag match: KEEP** — clean architecture, negligible cost
- **payload Vec pre-alloc: REVERT** — 2.1MB wasted per JSON parse
- **branch_pushes_children: FIX** — single root cause of regression
- **ParsedGrammar: ELIMINATE** — 11 tranches, BLOCKING
- **StructRegistry: IMPLEMENT OR DELETE** — dead scaffold

## Phase 1 — Fix projection activation

Status: IN PROGRESS

### AU.1.1 branch_pushes_children investigation

Commit 908067f added TransparentElide materialization check + nested
Alt/Skip/Next/Minus recursion + reverted payload pre-alloc to lazy.

**Root cause found**: `branch_pushes_children` didn't check
`CallStrategy::InlineBody`. The driver inlines leaf rules at Ref
sites via the CSP-driven inline analysis, but the branch classifier
only checked `is_transparent` and `MaterializationClass::TransparentElide`.

Fix (commit 83357e4): add `DriverState` parameter, check
`call_strategy(rid) == InlineBody`, recurse into inlined body.

Also fixed: payload_idx u16 overflow — canada.json has 111K f64
payloads, exceeding u16 max (65535). Stored byte offset in
`child_off` for payload-bearing leaves (full u32 range).

**Post-fix bench results** (f64 values now ACTUALLY STORED):

| Dataset | AQ (no values) | AU (with f64) | Delta |
|---------|---------------|---------------|-------|
| canada | 1796 | **1294** | -28% (111K f64 writes) |
| citm | 2698 | **2627** | -3% |
| twitter | 2086 | **2142** | +3% |
| data | 1939 | **1890** | -3% |

The canada delta is the inherent cost of materializing 111K f64
values (888KB of payload writes). AQ discarded these values.
This is now an apples-to-apples comparison — we store what sonic-rs
stores.

**Bonus finding**: `inline_acyclic` and `fuse_single_use` are
effectively no-ops at the IR level because `scc_id` is always
`Some(...)` during the normalizer loop (set in lowering before
`compute_scc` runs). All inlining happens at the driver level.

## Phase 2 — CSS scanner activation

Status: IN PROGRESS

### AU.2.1 WS scanner
`scan_ws_block_comments` IS the fused scanner — 319 call sites are
correct. The "zero fused CSS scanners" claim was incorrect. The
scanner handles both `\s` and `/* */` in one pass. No separate
`css_ws_comment_fast` exists or is needed.

### AU.2.2 Ident config
7 of 8 ident scans use `DEFAULT_IDENT_CONFIG`. 1 uses `CSS_IDENT_CONFIG`.
The CSS ident regex `[a-zA-Z_\x80-\xff][\w\x80-\xff-]*` does NOT have
leading dash in the first char class — it starts with `[a-zA-Z_\x80-\xff]`.
So `DEFAULT_IDENT_CONFIG` is actually correct for most CSS idents.
Only `selectorIdent` (which allows leading dash via `(?:-?...)`) needs
CSS config. Current routing is correct.

## Phase 3 — String decode + honest JSON bench

Status: NOT STARTED

## Phase 4 — Accumulated debt elimination

Status: NOT STARTED

## Phase 5 — Profile-driven optimization + bench parity

Status: NOT STARTED
