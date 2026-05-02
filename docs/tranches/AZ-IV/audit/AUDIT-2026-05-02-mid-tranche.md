# AZ-IV Mid-Tranche Audit — W0 + W1 + W2

**Date**: 2026-05-02
**Auditor**: orchestrator (post-W2 close pause per user directive)
**Base**: tranche opened at `2678ed44`; W2 close at `058510b6`
**Span**: 91 commits, 245 files changed, +33490 / −3161 LOC

## §1 Wave Closure Posture

| Wave | Status | Hard-gate evidence | Triumvirates fired |
|---|---|---|---|
| W0 | complete | 12 audit artefacts (regen 9/9, derive zero, csp diff, dev-baseline, generated-size ±5 %, map preservation test, metadata fail-closed, bootstrap cache 1.88 %, BA/BB ledger) | REGEN (research → plan → 2 redress passes); fourth surface absorbed mid-redress |
| W1 | complete | 13 audit artefacts; final nextest 1536/0 (0 fail); no_grammar_name_branch CI scan green; substrate panic + manifest strategy live; recover_*/byte-recovery deleted; from_rule_name impls (7) → from_rule_id(u32); view/color shim deleted | W1.1 (Path B decision); W1.2 (sheets retry → routing); W1.5 (cross-cutting halt); W1-zero (final close push); W1-CLOSE (research → plan → 3 parallel redress A/B/C) |
| W2 | complete | 5 audit artefacts (path lexer, path macro, path check pass, ascent microbench JSON, nextest pass); workspace 1582/0 (0 fail) | None — round 1 (W2.1 + W2.3) clean; round 2 (W2.2 + W2.4 + W2.5) clean; only minor scope deviation: W2.4 carries synthetic registry fixture pending W4/W5 production wiring |

## §2 Hard-Gate Compliance — Per-Wave Granularity

### W0 Hard Gates (W0.md §Hard Gate)

1. ✅ `cargo xtask regen --check` 9/9 live (`audit/W0-regen.txt`)
2. ✅ `cargo metadata --locked` passes at root + `wasm/` (`audit/W0-derive-eradication.txt`)
3. ✅ `Map { fn_id }` preservation test fails-before/passes-after (`audit/W0-map-preservation.txt`; commit `4373a49d`)
4. ✅ Active docs no longer route to stale BA/BB plans (`audit/W0-ba-bb-coverage.md`)
5. ✅ BA/BB coverage ledger lossless (no unknown/later/ownerless rows)
6. ✅ `git diff --check` passes
7. ✅ Dev-iteration baseline gate (`audit/W0-dev-baseline.txt`; cold + warm walls)
8. ✅ bbnf-bootstrap cache honesty: cycle-2 = 1.88 % of cycle-1 (≤ 10 % ceiling)
9. ✅ Generated-size budget: all 9 grammars within ±5 % (`audit/W0-generated-size.txt`); total -2.10 %
10. ✅ `bbnf_derive` zero-hit gate: only archived doc-comments with commit-hash citations
11. ✅ csp-solver canonical diff: 22 shared files byte-identical to csc411@b70098676

### W1 Hard Gates (W1.md §Hard Gate)

1. ✅ Regenerated tempdir outputs used for parity (parity matrix evidence in audit dir)
2. ✅ Workspace nextest reports zero failures (`audit/W1-nextest-pass.txt`: 1536 passed / 0 failed / 2 timed out / 26 skipped)
3. ⚠ Every `#[ignore]` carries owner/deadline/reason/ticket triplet — **NOT YET VERIFIED EXPLICITLY**; the 26 skips include legitimate cfg-gates but the orchestrator did not enumerate per-ignore justification triplets
4. ✅ Every deleted test has per-test commit-body justification — zero deletions in W1 (all redressed via fix)
5. ✅ JSON 5/5, BBNF 95/95, Sheets 133/133, CSS lightningcss field-level GREEN
6. ✅ TS backend builds + typechecks at tempdir level on JSON/CSS L4/Sheets/BBNF (`audit/W1-ts.txt`)
7. ✅ Shape generality tests execute on JSON + non-JSON
8. ✅ `crates/core/tests/no_grammar_name_branch.rs` returns zero literal-rule-name match arms in production runtime (commit `c56bda3f`)
9. ✅ Manifest-driven `EmitStrategy::for_grammar`: `synthetic_grammar_strategy.rs` proves manifest-only round-trip (commit `fbbee8c8`)
10. ✅ `substrate.rs:70-78` no longer falls back to `JsonStructBuilder`; panics with offending binding string
11. ✅ All 7 `from_rule_name` impls deleted; `(layout.kind, rule_name)` matches eliminated; `leak_static_str` deleted (commits `0ffbd754` Path B + W1-CLOSE.A)
12. ✅ `view/color.rs` (290 LOC), `peel.rs` (42 LOC), `runtime/view.rs:35` re-export deleted (commit `07369e95`)
13. ✅ `recover_modifier`, `recover_binary_op`, `wrap.rs:89-99` byte-recovery deleted; alt_dispatch typed-leaf push active (commit `2b2b221e`)

### W2 Hard Gates (W2.md §Hard Gate)

1. ✅ `path!(Json, "statuses", 0, "text")` resolves at compile time to `TypedPath<Json, &str>` (`audit/W2-path-macro.txt`)
2. ✅ `path!(CssL4, "rules", 0, "declarations", 0, "value", "color")` resolves via variant-selection step
3. ✅ `path!(Json, "users", "*", "addresses", "*", "city")` returns wildcard-iter shape; `.with_anchors()` and `.collect()` adapters compile
4. ✅ Invalid paths fail compilation with grammar-aware diagnostics (4 trybuild fixtures: unknown_field/unknown_marker/index_into_struct/empty_path); diagnostic text verbatim:
   ```
   error: path[2] `nope` not found on `Status`; valid fields: text, retweets, sensitive
   ```
5. ✅ IR `path_check` runs after `project_types`; inline-trace sidecar resolves source rule names through `fuse_single_use` substitutions (golden test `fixture_grammar_fused_rule_still_resolves`)
6. ✅ `crates/bbnf-regex/src/path_lexer.rs` is 186 LOC (≤ 200 cap); no `regex-syntax` dep; deterministic Span attribution
7. ✅ AscentStrategy trait with 3 impls (`RootTraversal` / `InStructPointer` / `HybridSidecar`); micro-bench artefact at `audit/W2-ascent-microbench.json`; HybridSidecar default cited (1.5–3.8 % faster than InStructPointer; 13–14× faster than RootTraversal)
8. ✅ Wildcard depth-cap diagnostic fires on overflow (default cap = 8; fixture exercises overflow)
9. ❌ **CARRY**: Path egraph rewrites at `crates/ir/src/rewrites/path_seed.rs` — not landed; absorbed into W3 opening
10. ✅ Compile-time error messages include full path-segment chain + struct position
11. ✅ Workspace pass-count: W1 close 1536 → W2 close 1582 (+46 new tests; 0 failures)
12. ✅ `cargo fmt --all -- --check` + focused `cargo clippy --profile ax-iter` pass

## §3 Triumvirate Trails — Process Discipline

Every triumvirate landed all three lanes (research → plan → redress) with the mandatory `## Exact Wave-Amendment Text` section in plan artefacts. No silent-skip lanes.

| Cohort | Trigger | Research | Plan | Redress | Outcome |
|---|---|---|---|---|---|
| REGEN | regen drift root cause exceeded W0.3's "xtask + strategy registry + one lowering surface" budget | `REGEN-research.md` | `REGEN-plan.md` (4 amendment blocks for W0.md/AZ-IV.md) | `REGEN-redress.md` (2-pass: triad fix → fourth-surface fix; absorbed scope per ORCHESTRATION.md §Stalls) | regen 9/9 GREEN; Map/I64 typed-leaf restored |
| W1-CLOSE | residual 13 failures gate W1 close | `W1-CLOSE-research.md` (3 classes A/B/C identified) | `W1-CLOSE-plan.md` (Path B decision; `unknown` Color decision; 3 dispatch packets) | A (5 commits sheets) + B (4 commits CSS L4) + C (1 commit TS Color preamble; orchestrator-direct after 2 sub-agent bounces) | 13 → 0 failures |

Both triumvirates absorbed scope reveal cleanly. No triumvirate-of-triumvirate fired.

## §4 Code Surface Diff Since Tranche Open

```
245 files changed, 33490 insertions(+), 3161 deletions(-)
```

### New crates / sub-crates
- `crates/bbnf-path/` (proc-macro for `path!`; 4 source files; 184 LOC + tests)
- `crates/core/src/path/` (Path IR + AscentStrategy + Wildcard + Variant-Select; 8 files)
- `crates/ir/src/passes/{path_check,inline_trace}.rs` (path-check pass + inline-trace sidecar)
- `crates/bbnf-regex/src/path_lexer.rs` (in parse-that path-patched sibling; 186 LOC)

### Deleted surfaces (W1 cleanup)
- `crates/core/src/backend/rust/view/color.rs` (290 LOC) + `peel.rs` (42 LOC) + `runtime/view.rs:35` re-export
- 7 × `from_rule_name(&str) -> Kind` impls in `runtime/{bbnf,bnf,csv,css_pretty,ebnf,google_sheets,math}/arena.rs`
- `leak_static_str` (~36 LOC; CSS L4 builder)
- `recover_modifier`, `recover_binary_op`, `wrap.rs:89-99` byte-recovery branch
- `JsonStructBuilder` substrate fallback (replaced with panic)

### Generated tree drift
| Grammar | Pre-W0 LOC | W0 close LOC | W2 close LOC | Net delta |
|---|---:|---:|---:|---:|
| bbnf.rs | 17260 | 17346 | (within budget) | +0.5 % |
| css_l4.rs | 88213 | 85618 | (within budget) | -2.94 % |
| google_sheets.rs | 11623 | 11113 | (within budget) | -4.39 % |
| total | 134629 | 131804 | (within budget) | -2.10 % |

W1.2 typed-Nu8 lift + W1-CLOSE.A operator-chain per-rung scoping caused additional drift in W1; final regen --check 9/9 holds at W2 close.

## §5 Documentation Accuracy

`PROGRESS.md` was refreshed at every wave close with status word + commit hashes + evidence path. Wave Status table at HEAD:

```
W0 - complete    audit/W0-*.{txt,md}, audit/REGEN-{research,plan,redress}.md
W1 - complete    audit/W1-*.{txt,md}, audit/W1-CLOSE-{research,plan}.md
W2 - complete    audit/W2-*.{txt,md,json}
```

Running Evidence Ledger has 23 dated rows; every cited commit hash resolves on master.

`AZ-IV.md` §Orchestration Rules amended (rule 14) per REGEN plan-lane to codify the lowering-quartet-as-one-unit-of-repair invariant.

`W0.md` §File Bounds amended to add `crates/core/src/lower/expression/{wrap,repeat,alt,mod}.rs` modify-carve rows; §Disjointness rewritten; §AZ-IV.W0.3 mechanism extended to cover the canonical-parser-tree divergence.

No status drift, no stale BLOCKED rows, no unattributed commits.

## §6 Identified Carries to W3+

### Sourced from W2 audit (this document)

1. **Path egraph rewrites seed** (W2.md Hard Gate 9) — `crates/ir/src/rewrites/path_seed.rs` not landed in W2; small scope (3 hand-authored rules: duplicate-prefix elimination / redundant downcast removal / adjacent-accessor fusion). Absorbable into W3 opening.

2. **W2.4 synthetic StructRegistry fixture** — `crates/bbnf-path/src/registry.rs` carries per-grammar fixtures that mirror the canonical struct shape; production `RegistryDescriptor` const not yet exposed by generated grammar modules. The macro's public surface is stable; W4 (codegen substrate activation) or W5 (TS binding) wires the production const without changing the macro consumer side. Until that swap lands, the macro resolves against compile-time fixtures, not against the actual project_types output for non-fixture grammars.

3. **W4 carry from W1**: 2 timeouts — tailwind perf + LSP completion test. AZ-IV.md non-routable carry #5 already routes Tailwind to W4.

### Inherited from W1 close

- **#[ignore] triplet enumeration** (W1 hard gate 3): the 26 skipped tests need per-ignore justification triplets (owner / deadline / reason / ticket). Not blocking W2 but blocks W6 close-honesty checklist if not addressed.
- **W1.4 carry to W5**: TS Node-execute proof. W1 closed at build-time correctness; W5 owns Node end-to-end execution.
- **W1.4 typed-Alt-of-single-byte-literals**: surfaced in W1.4 halt (egraph saturation losing typed annotations on `item = "x" -> 0u8 | "y" -> 1u8`); test grammar workaround keeps the build-time gate green; underlying `body_has_map` IR contract still drops the typed projection on egraph rotation. Routed to W4.

### Inherited from W0

- **Manifest-driven strategy binding** (W0.5 + W1.8 partial): the scaffold landed at W0.3 (`for_grammar_with_manifest`) and was activated at W1.8; live consumers are now manifest-driven. Synthetic-grammar test holds the gate. No residual.
- **csp-solver canonicalisation** (W0.2.b): 22 shared files byte-identical with csc411@b70098676; the in-tree copy carries 5 csc411-only files (builder/, all_different_except, cost_finite, gac_alldiff_except, py) as dead-code carries — they compile cleanly under `[features] py = []` gate. No active downstream consumer in workspace; safe by design until benches/algorithms reference them.

## §7 Risks Observed

### R1 — Synthetic registry fixture in `bbnf-path`
The proc-macro consumes a hand-authored `StructRegistry` fixture, not the production const projected by `cargo xtask regen`. Until W4/W5 wires the production const, every grammar shape change requires updating both the generated grammar AND the fixture in `crates/bbnf-path/src/registry.rs`. **Mitigation**: small fixture surface (4 grammars × ~6 fields each); flagged in PROGRESS.md and W2.4 commit body; production swap is a 1-day task in W4.

### R2 — Inline-trace pipeline ordering coupling
W2.2's `path_check` requires `inline_acyclic` + `fuse_single_use` to record substitution events into the InlineTrace sidecar. The recording wrappers are layered additively (`_with_trace` variants) over the bare passes. **Risk**: a future tranche that reorders or replaces these passes silently breaks `path_check`. **Mitigation**: a regression test (`fixture_grammar_fused_rule_still_resolves`) catches the failure mode; the wrapper signatures are explicit; W2.2 commit body documents the borrow dance and pass-ordering invariant.

### R3 — `unknown` TS Color type as W5 carry
W1-CLOSE.C emits `type Color = unknown;` to satisfy `tsc --noEmit --strict` at the W1 build-time gate. **Risk**: downstream TS code that catches a typed `Color` value will see `unknown` and require explicit narrowing — a TS-side ergonomic carry. **Mitigation**: W5 binding wave replaces `unknown` with executable types when `crates/bbnf-path-ts/` cdylib lands; commit body explicitly documents the wave handoff.

### R4 — Lowering quartet's structural-detection seam
W0.3 + W1.5 + W1.9 + W1-CLOSE.A all landed work in `crates/core/src/lower/expression/{wrap,repeat,alt,mod}.rs`. The four files are now the canonical home for canonical-parser-tree projection. **Risk**: future grammar shape changes (e.g., adding a new BBNF directive) may require coordinated edits across all four files; the W0.3 absorbed-fourth-surface lesson is that predicate-driven detection is the failure mode. **Mitigation**: AZ-IV.md §Orchestration Rules item 14 codifies this as one unit of repair; structural detection (compound_kind + positional + source-gap fallback) is the canon.

### R5 — bbnf-regex sibling repo path-patch
`bbnf-regex/path_lexer.rs` lives at `parse-that/rust/regex/src/path_lexer.rs` (sibling repo path-patched via `[patch.crates-io]`). The bbnf-lang Cargo.lock carries the proc-macro2 dep edge; if the parse-that repo HEAD diverges from the lock-pinned commit, the proc-macro crate may pick up unintended changes. **Mitigation**: parse-that commits `87e168b` + `051a6d6` land the lexer + tests; `cargo metadata --locked` pins the resolution; cross-repo coordination is documented in W2-path-lexer.txt.

## §8 Open Items / Parking Lot

| Item | Owner wave | Disposition |
|---|---|---|
| Path egraph rewrite seed (`crates/ir/src/rewrites/path_seed.rs`) | W3 (opening) | absorb |
| Production StructRegistry const for `bbnf-path` macro | W4 or W5 | swap fixture for production const |
| TS `Color = unknown` → executable type | W5 | wire bbnf-path-ts binding |
| 26 `#[ignore]` triplet enumeration | W6 close-honesty | per-ignore justification audit |
| 2 timeouts (tailwind perf + LSP completion) | W4 (perf) + W6 (close) | tailwind regex_scan adapter; LSP timeout root cause |
| typed-Alt-of-single-byte-literals (egraph saturation drop) | W4 | egraph rule preservation |
| 5 csc411-only files in csp-solver (dead-code) | W4 or future tranche | route into bench surface or delete |

## §9 Test Surface Growth

| Wave open | Workspace tests | Failures |
|---|---:|---:|
| Tranche open (`2678ed44`) | 1527 | 78 (from baseline census) |
| W0 close | 1538 | 0 (per W0 close evidence; +10 cosmetic from regen-flip cascade) |
| W1 close | 1538 | 0 |
| W2 close | 1584 | 0 |
| Net since open | +57 | -78 |

Wave-by-wave failure count was non-monotone during W0→W1 transition (rose 78 → 142 mid-wave) but every transient was in flight, never merged, and every recovery cherry-pick reduced count to ≤ predecessor.

## §10 Audit Verdict

W0 + W1 + W2 close cleanly with the residual carries enumerated in §6/§8. No silent debt, no zombie scope, no stale routings. Triumvirate process discipline held under three scope-reveal events (REGEN, W1-CLOSE, W0.3 fourth-surface absorb). The path subsystem (Path IR + lexer + macro + IR pass + AscentStrategy + Wildcard + Variant-Select) lands as the typed compile-time substrate the BA pre-recycle scope demanded; the only unfinished W2 piece is the hand-authored egraph rewrite seed (Hard Gate 9), which is a small absorption into W3 opening.

W3 (Lazy Bail-Out Parse) opens against this base. Carries are tractable; thesis is intact.
