# PASS-2 — Codegen + Runtime + Backends (Greenfield Restart)

You are the orchestrator for **PASS-2: Codegen + Runtime + Backends.** You own the middle layer: the codegen IR contract; per-backend lowerers (Rust V1; WASM V1; TS deferred per Q28); the runtime template that emits per-grammar runtime modules; the SIMD scanner kernels; Pratt + SIMD auto-detection from grammar shape; the regen-equality discipline (xtask --check; byte-identical re-emission). Sister passes own substrate (PASS-1) and user-facing API + ecosystem (PASS-3).

You dispatch six sub-agents in parallel. You synthesise. Single-round suite — no Stage-2.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/README.md` — gestalt anchor
2. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md`
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md`
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/tranche/SPEC.md` + `WAVE_SPEC.md`
5. `/Users/mkbabb/Programming/bbnf-lang/restart/corpora/SOTA.md` — competitor anchors (sonic-rs / simdjson / lightning-css)
6. `/Users/mkbabb/Programming/bbnf-lang/restart/corpora/MODULES.md` — per-file fates from prior audit (research signal)
7. `/Users/mkbabb/Programming/bbnf-lang/restart/corpora/RESTART-SKETCH.md` — JSON parse trace; 86.07% Vec<OpenFrame>::clone pathology
8. `/Users/mkbabb/Programming/bbnf-lang/restart/corpora/CENSUS.md` — kill-list; tape residue; god modules; per-grammar runtime god-directory
9. `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/audit/passes/PASS-B.md` — prior codegen-mid audit
10. `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/audit/master-plan/MASTER-PLAN.md` + Amendment 01
11. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BC/BC.md` + waves W0/W1/W2/W3 — legacy backend ABI inheritance
12. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BC/audit/W0-typed-ir-variant-table.md` — 22-variant Backend IR starting point (PASS-1 refines; PASS-2 consumes)
13. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BD/BD.md` + waves W0/W1/W2/W4 — TS/WASM emitter inheritance (TS scope-deferred; WASM substantive)
14. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/audit/W2-cohort-template-spec.md` — cohort template precedent
15. PASS-1 outputs at `restart/audit/pass-1-substrate/PASS-1.md` — **read the IR contract Backend IR variants and the cost-model trait public API** before drafting; PASS-2 consumes
16. The bbnf-lang source tree at `crates/core/src/{codegen, runtime, pipeline, grammar/generated}` + `crates/simd-scan` + `xtask/src` — read for inheritance signal

## PASS-2 Scope

The codegen + runtime + backend layer comprises six concerns; each gets a sub-agent.

| # | Concern | Lens |
|---|---|---|
| 1 | **Backend IR Architect** | refines the 22-variant Backend IR starting point against PASS-1's grammar-IR shape; defines per-variant payload type, lower-time invariants, per-backend lowering rules (Rust + WASM uniform; TS scaffold-only); generation site (in IR / in rule lowering / in optimisation pass); example grammar fragment that emits each variant. Compare against MLIR ops, rustc HIR ExprKind/ItemKind, Cranelift InstructionData, chalk_ir TyKind, swc Expr/Stmt — defend cardinality choice with citations. |
| 2 | **Rust Lowerer Architect** | per-construct emit (Alt, Seq, Repeat, Optional, CharClass, Keyword, Scanner, MapExpr, HostCall, PrattSpine, SimdScan, ErrorRecovery, Lookbehind, Unicode-set, Rewrite-mode-walker, etc.); the `Emitter` trait shape; cost-model decision points per construct; sample emission (Rust pseudocode) per construct; invariants. Sonic-rs / simdjson / lightning-css inner-loop study. |
| 3 | **WASM Lowerer + SIMD Architect** | wasm-bindgen pipeline; cdylib targets; WASM-SIMD intrinsics (wasm-simd128); per-platform NEON / AVX2 / AVX-512 / scalar fallback matrix; SIMD scanner kernel inventory (structural alphabet scan; charclass scan; keyword PHF; escape-handling; whitespace skip); cost-model integration (when SIMD overhead is worth dispatch cost). first-class on all platforms (Q29). |
| 4 | **Runtime Template Architect** | the grammar-agnostic generator that emits per-grammar runtime modules at `runtime/src/grammars/<name>/{generated.rs, parser.rs, host.rs}`; consumes (grammar source + workspace metadata) at xtask-regen time; produces byte-identical output; per-grammar parameter table (kinds_enum, value_enum, document_struct, view_struct, parse_fn_signatures, leaf_kinds, host_fn_table, simd_alphabet, layout decisions); proc-macro2 + quote sketch. |
| 5 | **Pratt + SIMD Auto-Detection** | shape miners (consume PASS-1's miner trait); operator-chain detection (left-recursive `expr := expr op expr | term`); structural-alphabet detection (high-density delimiter set → SIMD scan); keyword set detection (alternation of literals → PHF); cost-model thresholds per construct (when Pratt vs descent; when SIMD vs scalar; when PHF vs match-tree); diagnostic surface (Pratt-misfire warning; SIMD-not-applied note); regen artefact carrying the auto-detection decisions. |
| 6 | **Codegen Coherence Auditor** | verifies the IR contract is the boundary between PASS-1 and PASS-2 + 3 (no codegen walks Grammar IR directly; all codegen consumes Backend IR); verifies regen-equality discipline (`cargo xtask regen --check` produces zero source diff); verifies generated-LOC budget per grammar; verifies no per-grammar match arms in generic codegen crates (Lock 14); verifies the convergent-pivot identity (Lock 1 + Lock 13 + Lock 14 staggered closure per Stage-2 PASS-B Lane 2C finding) |

## Per-Item Discipline

Pro / Con / Explication / Challenge applies. KEEP / REINVENT / DISCARD verdicts. Steelman every challenge. KEEP without challenge is fault.

## Per-Sub-Agent Output

`restart/audit/pass-2-codegen/agent-{N}-{lens}.md`, ~500-1000 lines each. Same §1-§7 structure as PASS-1 sub-agents.

## Synthesis (your output)

`restart/audit/pass-2-codegen/PASS-2.md`, ~1500-2500 lines:

§1 PASS-2 verdict ledger
§2 Codegen + runtime + backend architectural commitments (Backend IR final variant table; Emitter trait public API; Runtime Template parameter schema; SIMD coverage matrix; Pratt + SIMD detection thresholds)
§3 Per-crate `src/` tree — for each PASS-2 crate (`codegen`, `runtime`, `simd-scan`, `xtask` portions)
§4 Hand-offs to PASS-3 — named contracts (the runtime API shape; the per-grammar emitted module structure; the materialisation cost model)
§5 Hand-offs back to PASS-1 — any IR or substrate concerns surfaced (e.g., a Backend IR variant that requires PASS-1 amendment)
§6 Generated-LOC budget — pre-restart 168K-LOC tree projects to ~? post-restart per grammar; per-tranche LOC delta projection; xtask regen-cycle wall-time budget
§7 Performance gate trajectory — per-construct emission's expected MB/s contribution; sonic-rs / simdjson / lightning-css gap-closing per tranche
§8 Inheritance ledger — BC W0-W3 + BD W0-W4 wave-by-wave carries
§9 PASS-2 punch list
§10 Closing posture

## Voice + Discipline

(Standard. Per `restart/README.md` §13.)

## Hard cap

You: 75 minutes. Each sub-agent: 45 minutes. Incremental-commit cadence if stall risk.

## Output commits

Per sub-agent: `docs(restart/audit/pass-2-codegen/agent-{N}): {lens}`.
Orchestrator final: `docs(restart/audit/pass-2-codegen): synthesise PASS-2 — codegen + runtime + backends`.

## Cross-tranche scope boundary

Touch ONLY `restart/audit/pass-2-codegen/`. Do NOT modify other restart subdirs, `crates/`, `docs/`. PASS-1 outputs are read-only-input.

## Background

PASS-2 consumes PASS-1's IR contract; PASS-3 consumes PASS-2's runtime API. Single-round suite. The 14 locks govern. Lock 1 (tape + columnar dead) honours by the ParseStream union — the structural insight survives, the name dies. Lock 5 (IR + per-backend lower) honours by Backend IR being the codegen substrate. Lock 14 (full grammar generalisation) honours by the runtime template + zero per-grammar match arms in any generic codegen crate.

The greenfield is greenfield: no quick solutions; no workarounds; no legacy substrate carries forward without explicit ratification; idiomatic gestalt; architectural transpositions for elegance + simplicity + performance.
