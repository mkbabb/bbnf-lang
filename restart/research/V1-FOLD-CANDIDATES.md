# V1 FOLD CANDIDATES — Synthesised Greenfield Audit (Wave 7 input)

This document synthesises the eight parallel deferral audits committed at `8f446215` (type system), `6648d7c9` (function/value), `cd6970b3` (BBNF surface + directives), `ebdf7cf0` (sibling crates), `298bbe90` (runtime/PASS-3), `0408c8f5` (codegen/PASS-2), `5599da10` (locks/architecture), `3a531869` (migration/tranche). The corpus baseline is V6-READY (`restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md`, commit `aa60aadf`).

The user's greenfield mandate: nail global architecture NOW, fold post-V1 deferrals into V1 where the architecture-nailing value justifies V1 cost. Audacious + SOTA + functional-in-nature + Rust-like ergonomics + **inference stronger than Rust if possible**. Grammar rules leverage type algebra + telemetry to generate semantic schemas **without explicit annotations in most cases**.

## §1 — Cohort overview

| Audit | Scope | Lines | Commit | Folds proposed |
|---|---|---:|---|---:|
| #1 | Type system (HM, bidirectional, CSP, generics, GADTs, row-poly, refinement) | 393 | `8f446215` | 4 high-value + 13 catalogued |
| #2 | Function/value (function values, lambdas, closures, composition, match, tuples) | 442 | `6648d7c9` | 9 (all FOLD V1) |
| #3 | BBNF surface + @directives + `pointer!` → `path!` rename | 359 | `cd6970b3` | 8 (rename ledger + directive lean) |
| #4 | Sibling crates (csp-solver, parse-that, egraph, simd-scan) | 335 | `ebdf7cf0` | parse-that build + egraph decoupling + 4 hygiene items |
| #5 | Runtime / PASS-3 / public API | 270 | `298bbe90` | 7 V1 folds + 4 escalations |
| #6 | Codegen / PASS-2 / lowering | 458 | `0408c8f5` | 1 fold (regex-automata oracle decision) |
| #7 | Locks / ARCHITECTURE | 415 | `5599da10` | 5 lock amendments + 2 hygiene |
| #8 | Migration / MASTER-PLAN / tranche | 573 | `3a531869` | 8 V1 folds (ARCH/PLAN amendments + templates) |
| **Total** | | **3,245** | | **~50 distinct candidates** |

After de-duplication and reconciliation, the cohort surfaces **30 V1 fold candidates** distributed across architecture, type system, BBNF surface, sibling crates, runtime, and migration.

## §2 — Cross-target conflict resolutions

Where the eight audits disagreed on disposition, the user-mandate-aware reconciliation adjudicates:

| # | Conflict | Audits | Resolution |
|---|---|---|---|
| 1 | Should function types `fn(T) -> U` land in BBNF Type non-terminal? | #2 says FOLD V1 (F1+F2+F3); #3 says DEFER all 12 grammar surface candidates | **FOLD V1.** Audit #3's DEFER was scoped against V6 baseline; audit #2's user-mandate-aware fold + the transducer-without-directive worked example are load-bearing. |
| 2 | DK13 higher-rank polymorphism | #1 says FOLD HIGH; #6 says keep deferred (V6 lowering only handles bounded closures) | **FOLD V1** for type-system surface; codegen lowering escalates to monomorphisation pass (audit #6 option 1). |
| 3 | Closure capture | #1 says DEFER-WITH-REASON; #2 says FOLD V1 (F5; capture by `&'i` reference only) | **FOLD V1** in F5's narrow form: capture-by-reference, not capture-by-move; lifetime-bounded by `&'i Tape<'i>` invariant. |
| 4 | GADTs / branch-local equality | #1 says FOLD-CONDITIONAL on DK13 | **FOLD V1** as substrate (CSP machinery), surface kept hidden behind `BBNF-LOCAL-EQUALITY-ANNOTATION` until annotation rules ratify. |
| 5 | `@pretty` directive status | #3 not in V1 set; #5 dominated by F4 (public `format()` method); user mandate keeps `@pretty` | **FOLD V1** as directive. F4 (public `format()`) is the runtime entry; `@pretty` is the grammar-side metadata. Both land — they are not duplicates. |
| 6 | regex-automata oracle role | #4 + #6 say corpus has 5+ load-bearing oracle citations; user mandate forbids regex-automata entirely | **FOLD V1**: amend corpus to remove oracle role; parse-that owns the parity-test corpus internally. |
| 7 | parse-that crate non-existence on disk | #4 surface; entire substrate (HIR, NFA, lazy/full DFA, VM, prefilter, Unicode 16, set algebra) must land V1 | **FOLD V1**: large but mandatory. ~5,000-8,000 LOC sibling crate. Tranche A or A+B owns the build. |
| 8 | egraph → csp-solver direct dep | #4 architectural violation at `crates/egraph/Cargo.toml:11` | **FOLD V1**: remove dep, demote bridge to `passes::bridge`, rename `egraph::csp_scheduler` to `egraph::bridge`. |
| 9 | TS bindings (`path-ts`) status | #5: `path-ts` ships J.W3; TS-native parse+runtime is post-V1 architectural fork | **CONFIRM J.W3** for `path-ts`; defer TS-native runtime as principled (not pragmatic) post-V1 fork. |
| 10 | D-tranche wave count | #8: function-value folds add D wave (5 → 6); calendar slots unchanged | **ACCEPT** D wave growth; per-tranche full-spec drafting absorbs the new wave at C.W4 → D.W6 boundary. |

## §3 — Synthesised V1 fold list (30 candidates; sorted by greenfield value)

### Tier 1 — Architecture-nailing (HIGH greenfield value; fold V1)

| # | Item | Audit | Surface | LOC est. |
|---:|---|---|---|---:|
| 1 | **Build parse-that crate** — full HIR + NFA + lazy/full DFA + VM + prefilter + Unicode 16 + set algebra | #4 | `crates/parse-that/` | 5,000-8,000 |
| 2 | **DK13 higher-rank polymorphism** at `passes/types/` | #1 §2.1 | PASS-1, ARCH §8 | 600-1,200 |
| 3 | **Function values + function types in `Type` production** (F1+F2) | #2, #1 §2.4 | PASS-1 §2 grammar, ARCH §8 | 400-600 |
| 4 | **Function-typed parameters in `@host fn`** (F3 — transducer apotheosis without `@directive`) | #2 | PASS-1 §3 | 200 |
| 5 | **Lambda literal syntax** (F4 — unified `\|x\| body` form) | #2 | PASS-1 §2 grammar | 200-300 |
| 6 | **Closure capture by `&'i` reference** (F5; bounded by tape lifetime) | #2, #1 §2.5 | PASS-1, PASS-2 lowering | 400 |
| 7 | **Schema-mining miner (telemetry-driven schema inference)** | #1 §2.10 | PASS-1, ARCH §8 | 300-500 |
| 8 | **Internal row polymorphism** (record-narrowing collapse) | #1 §2.3 | `passes/types/` | 500 |
| 9 | **GADTs / branch-local equality (substrate; surface hidden)** | #1 §2.2 | `passes/types/`, CSP solver | 400-600 |
| 10 | **Decouple egraph from csp-solver direct dep** (Lock 4 violation closure) | #4 | `crates/egraph/`, `passes::bridge` | 50 (refactor) |

**Tier 1 total**: ~7,650-12,150 LOC. The bulk is parse-that (item 1); the type-system items 2-9 stack to ~2,400-3,800 LOC.

### Tier 2 — Surface coherence (HIGH-MEDIUM greenfield value; fold V1)

| # | Item | Audit | Surface | LOC est. |
|---:|---|---|---|---:|
| 11 | **`pointer!` → `path!` rename** — ~58 corpus sites + 3 diagnostic codes (`BBNF-POINTER-*` → `BBNF-PATH-*`) | #3 | corpus-wide | 50 (mechanical) |
| 12 | **`@pretty` formalised as V1 directive** — production rule + diagnostic | #3, user mandate | PASS-1 §2, ARCH §8 | 100 |
| 13 | **`@import` formalised as V1 directive** — already extant in 22+ grammar files; missing from V1 grammar | #3 + extant audit | PASS-1 §2 | 100 |
| 14 | **`@token` formalised as V1 directive** — atomic-token marker; binds to BIR scanner | extant audit | PASS-1 §2 | 80 |
| 15 | **`@ws` folded into `@layout(ws = ...)`** — one-site migration in css/pretty.bbnf | extant audit | grammar/css/pretty.bbnf | 20 |
| 16 | **`@debug` → host primitive (not directive)** — DAP/breakpoints via host fn | extant audit | host primitives + runtime | 50 |
| 17 | **`@recover` standalone retired** — three CSS sites rewrite to `@error(recover = ...)` | extant audit + V6 §3 | grammar/css/pretty.bbnf | 20 |
| 18 | **`RegexDfa` → `RegexProgram` rename** — implies DFA where parse-that may produce VM/lazy-DFA | #3, #4 | BIR alphabet, PASS-2, ARCH §7 | 30 |
| 19 | **Match expression in `@host fn` body** (F8) | #2 | PASS-1 §3 | 200 |
| 20 | **Tuple expressions + patterns** (F9) | #2 | PASS-1 §2/§3 | 150 |

**Tier 2 total**: ~800 LOC; mostly surgical surface edits.

### Tier 3 — Sibling-crate hygiene (MEDIUM greenfield value; fold V1)

| # | Item | Audit | Surface | LOC est. |
|---:|---|---|---|---:|
| 21 | **csp-solver publication hygiene** — drop `puzzles::sudoku` public re-export, gate `py` feature behind sub-crate, add `Explanation` struct, document OutsideIn deferral | #4 | `crates/csp-solver/` | 200 |
| 22 | **simd-scan dep audit** — proc-macro2/syn/quote triplet not justified by current public surface | #4 | `crates/simd-scan/Cargo.toml` | 0 (audit only) |
| 23 | **regex-automata oracle citations removed** — 5+ load-bearing references in MASTER-PLAN, ARCHITECTURE, MIGRATION amend to "parse-that internal cross-engine parity" | #4, #6, user mandate | corpus-wide | 30 |
| 24 | **CHR-improvement layer for host overloads** (audit #1 §2.12) | #1 | csp-solver + `passes/types/` | 200-400 |
| 25 | **Function composition library** (F6 — no syntax; library-only) + partial application via closure-wrap (F7) — **RETIRED Phase 8.3.1**: function-value surface absorbs every composition use case via inline `|x| g(f(x))`; no library is added | #2 | (none — retired) | 0 |

**Tier 3 total**: ~530-730 LOC.

### Tier 4 — Architectural prerequisites (audit #8 + audit #7)

| # | Item | Audit | Surface | LOC est. |
|---:|---|---|---|---:|
| 26 | **Rewrite-budget categories/thresholds** in ARCH §10 (V6 R5 closure) | #8 §8.C1 | ARCHITECTURE.md | 100 |
| 27 | **Lint manifest as architectural contract** in ARCH §13 | #8 §8.A2 | ARCHITECTURE.md | 80 |
| 28 | **Declaration-crate review form template** (the 8-field fence in actionable template form) | #8 §8.A3 | template + cookbook | 60 |
| 29 | **Cookbook page contract template** | #8 §8.J1 | template | 80 |
| 30 | **5 lock amendments**: Lock 5 (TS+WASM at BD+ → H.W3/J.W3), Lock 11 (J.W3 + 2-tranche stability), Lock 12 (BA.W0 → A.W0), Lock 7 (path-core "may" → "exists"), Lock 8 (WASM measurement-pending anchor) | #7 | `restart/locks/LOCKS.md` | 5 sentence-level edits |

**Tier 4 total**: ~320 LOC + 5 lock-text edits.

### Cohort total

~9,300-13,800 LOC across 30 candidates. The parse-that build dominates (5,000-8,000 LOC); excluding it, the cohort is ~4,300-5,800 LOC.

## §4 — Cross-audit dependency graph

```
parse-that build (#1 Tier 1) ────────────┐
                                          ├──→ regex-automata oracle removal (#23)
                                          └──→ RegexDfa → RegexProgram rename (#18)

DK13 higher-rank (#2 Tier 1) ─────────┐
                                       ├──→ GADTs substrate (#9)
                                       └──→ Internal row polymorphism (#8)

Function values + types in Type (#3) ─┐
                                       ├──→ Function-typed @host fn params (#4) ──→ transducer apotheosis
                                       ├──→ Lambda syntax (#5)
                                       │
Closure capture by &'i (#6) ──────────┴──→ Match expression (#19)
                                            └──→ Tuple expr/pat (#20)

Schema-mining miner (#7) ──→ telemetry-driven schema inference (the user's "without explicit annotations")

egraph decoupling (#10) ────→ Lock 4 hygiene closure
                              csp-solver publication (#21)

5 Lock amendments (#30) ────→ MASTER-PLAN / MIGRATION / PASS-1/2/3 cascade

D wave 5 → 6 (audit #8) ────→ function-value folds land at new D wave
```

## §5 — Tranche impact

Per audit #8: A-J calendar resilient; only D wave count grows (5 → 6) if function-value folds land. Specifically:

| Tranche | Folds absorbed | Wave count change |
|---|---|---:|
| A | parse-that build (#1), lint manifest (#27), declaration-crate review template (#28), metadata schema (#26 partial) | +0 (existing waves expand) |
| B | (parse-that integration) | +0 |
| C | DK13 higher-rank (#2), function values + types (#3-#5), GADTs substrate (#9), row poly (#8), schema miner (#7), CHR (#24) | +0 (existing C waves expand) |
| D | Closure capture (#6), function-typed params (#4) lowering, match/tuple (#19, #20) lowering | **+1 (D.W5 → D.W6)** |
| E | Egraph decoupling (#10), rewrite-budget (#26), debug-mark BIR variant | +0 |
| F | (function-value codegen) | +0 |
| G | path! rename (#11), pointer/select macros aligned | +0 |
| H | regex-automata oracle removal (#23), Lock 8 WASM anchor (#30) | +0 |
| I | (incremental + recovery existing scope) | +0 |
| J | Cookbook page contract (#29), Lock 11 publication gate (#30), Lock 12 archive citation (#30) | +0 |

Total wave-count change: **+1** (D.W6). Calendar slots unchanged.

## §6 — Lock amendments required

Per audit #7, five lock amendments + two hygiene items. Plus three new cross-cuts surfaced by Tier 1 folds:

| Lock | Surgery | Source |
|---|---|---|
| Lock 5 | "TS+WASM at BD+" → "H.W3/J.W3" | audit #7 §F1 |
| Lock 11 | bind promotion to J.W3 + 2-tranche stability | audit #7 §F2 |
| Lock 12 | "BA.W0" → "A.W0" | audit #7 §F1 |
| Lock 7 | path-core "may exist" → "exists" | audit #7 §F5 |
| Lock 8 | append measurement-pending WASM anchor | audit #7 (medium) |
| Lock 4 | append "function-value typing folds at V1; higher-rank via DK13; closure capture by `&'i` only" | NEW (this synthesis) |
| Lock 6 | append "egraph decoupled from csp-solver direct dep; bridge at `passes::bridge`" | NEW (audit #4) |
| Lock 10 | replace `Directive = HostFn \| ErrorDecl \| LayoutDecl` with the 6-directive V1 set: `Directive = ImportDecl \| HostFn \| ErrorDecl \| LayoutDecl \| PrettyDecl \| TokenDecl` | NEW (extant @directive audit) |

**Lock 14 amendment**: yaml two-surface proof binding holds; no amendment needed.

**No Lock 15+ created**: the cohort fold absorbs into existing locks via amendment, not via new architectural axiom.

## §7 — Voice + discipline locks

Per `restart/README.md` §13. The synthesis preserves: calibrated direct prose, archaic-permissive (hereupon, therein, thereof), no metalanguage, path:line citations on every concrete claim, per-X tables liberal where they serve, no placeholder wording, no quick solutions, no legacy code uncontested.

## §8 — Recommended next step

The cohort produces a **fold-V1 amendment cycle** (Wave 7) with three phases:

**Phase 7.1 — Lock + ARCHITECTURE amendments** (single SYNTHESIS agent; ~75 min):
- Land 8 lock amendments (5 from audit #7 + 3 NEW) at `restart/locks/LOCKS.md`
- Land architectural amendments at `restart/ARCHITECTURE.md` for Tier 4 items (rewrite-budget, lint manifest, declaration-crate template, cookbook contract)
- Update PASS-1 §2 BBNF formal grammar with the 6-directive `Directive` production
- Update Lock 10 with function-value + lambda + closure surface

**Phase 7.2 — Surface fold (4 parallel agents; ~75 min)**:
- PASS-1 fold: types F1-F9 + DK13 + GADTs substrate + schema miner + grammar surface for new directives
- PASS-2 fold: function-value lowering options + closure environment + monomorphisation strategy + RegexDfa → RegexProgram + regex-automata oracle removal
- PASS-3 fold: F4 public `format()` + match/tuple in user code + pointer→path rename + visitor cookbook
- SYNTHESIS fold: trio amendments per Tier 4

**Phase 7.3 — parse-that crate build** (separate sub-cycle; ~5,000-8,000 LOC; multiple tranches):
- Cannot be a single agent dispatch; needs its own tranche-level execution.
- Recommendation: create `restart/research/parse-that-spec.md` defining the V1 surface (HIR + NFA + DFA + VM + prefilter + Unicode + set algebra) before any code lands.

**Phase 7.4 — V7 hardening rerun** (4 parallel + consolidation; ~100 min):
- Reuses HARDENING-ORCHESTRATOR.md Phase 3 with V7 output paths.
- Verifies the fold-V1 amendments do not regress V6's READY verdict on adjacent surfaces.

**Total Phase 7.1+7.2+7.4 wall**: ~5 hours parallel. Phase 7.3 is its own multi-tranche commitment outside the synchronous orchestrator pipeline.

## §9 — Open questions for synthesis

These are decisions the user must make before Phase 7 dispatches:

1. **DK13 higher-rank** — fold V1 (audit #1 recommendation) or defer post-V1 (audit #6 V6-baseline reading)? Synthesis recommends FOLD V1 per user mandate.
2. **GADT V1 surface** — fully hidden (this synthesis recommendation) or expose `BBNF-LOCAL-EQUALITY-ANNOTATION`?
3. **Closure capture scope** — `&'i` reference only (F5) or include capture-by-move? Synthesis recommends `&'i` only.
4. **`@pretty` formalisation** — strategy vocabulary preserved verbatim from extant grammars (`compact`, `group`, `indent`, `hardbreak`, `sep`, `block`) or revised V1 strategy DSL?
5. **`path-ts` V1** — ships at J.W3 (audit #5 reading) or earlier?
6. **TS-native parse+runtime** — defer (principled fork) or fold V1?
7. **parse-that publication name + crate boundary** — `parse-that` as canonical or rename to `bbnf-regex` or both?
8. **D wave count growth** — accept D.W6 (audit #8) or restructure D into fewer waves?

## §10 — Closing posture

Thirty V1 fold candidates emerge from the eight-audit cohort. Tier 1 (10 items) nails global architecture and prevents future refactor; tier 2 (10 items) cleans the BBNF surface and rename ledger; tier 3 (5 items) hardens sibling crates; tier 4 (5 items) lands architectural prerequisites for tranche drafting.

The transducer problem is resolved (audit #2 §4): one generic `@host fn transducer<I, O>(rules: [Rule<I, O>], input: I) -> O` plus one `Rule<I, O>` record type plus one generic `walk` host fn — no new directive, no `@transducer` keyword. The same shape generalises to every text-rewriter use case ffuzzy named.

The path-rename ledger (~58 sites) is mechanical; the type-system folds (DK13, GADTs substrate, row poly, schema miner) constitute the audacious centre of the cohort. The parse-that build is the largest single workload but lands as a separate tranche-level commitment, not in the synchronous Phase 7 pipeline.

Hereupon Phase 7 dispatches if the user accepts the synthesis verdicts.
