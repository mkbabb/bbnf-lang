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

**Critical finding**: JSON leaf rules (null, bool, number, string)
have `MaterializationClass::MustTape` (not TransparentElide), yet NO
parse function is emitted. Only `_prettify` variants exist. The rules
ARE inlined at call sites in `__value`, but NOT by the `is_transparent`
metadata — by some other mechanism (possibly the driver's inline
analysis or the `@pretty` codegen path).

Agent investigating the exact elision mechanism.

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
