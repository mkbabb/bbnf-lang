# PASS-1 — Substrate (Greenfield Restart)

You are the orchestrator for **PASS-1: Substrate.** You own the bottom layer: source acquisition, grammar parsing, the IR, the type system, the optimisation passes (CSP + e-graph + shape mining + cost model), the VM, the host-fn dispatch + primitives, the error vocabulary, the BBNF extensions (rewrite-mode + lookbehind + Unicode char-class algebra + generics + `@host fn` + `@error` + `@layout` + multi-function chaining). Sister passes own codegen + runtime (PASS-2) and user-facing API + ecosystem (PASS-3).

You dispatch six sub-agents in parallel. You synthesise their outputs into a single PASS-1 deliverable. The 5-prompt suite is **single-round** — no Stage-2 double-back, no meta-review. The hardening prompt at `restart/prompts/HARDENING.md` is the gate after synthesis.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/README.md` — the gestalt anchor; settled positions Q1-Q35
2. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md` — the 14 locks (settled)
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md`
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/tranche/SPEC.md` + `WAVE_SPEC.md` + `RESEARCH.md`
5. `/Users/mkbabb/Programming/bbnf-lang/docs/ffuzzy.md` — the three primitives + multi-function chaining gap
6. `/Users/mkbabb/Programming/bbnf-lang/restart/corpora/CENSUS.md`, `MODULES.md`, `RESTART-SKETCH.md`, `SOTA.md`
7. `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/audit/passes/PASS-A.md` — prior parse-front audit (research signal, not authority)
8. `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/audit/master-plan/MASTER-PLAN.md` + `AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md`
9. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/BA.md` + waves W0/W1/W2/W3/W4 — legacy parse-front + IR-fracture inheritance
10. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BC/audit/W0-typed-ir-variant-table.md` — 22-variant Backend IR starting point
11. The bbnf-lang source tree at `crates/{ir, core/src/source, core/src/parse, core/src/lower, core/src/host, ir/src/passes, ir/src/registry, csp-solver, egraph, simd-scan}` — read for inheritance signal

## PASS-1 Scope

The substrate layer comprises six concerns; each gets a sub-agent.

| # | Concern | Lens |
|---|---|---|
| 1 | **IR Architect** | designs Grammar IR + Backend IR; defines variants; defines the contract; specifies side-tables (typed annotations, cost annotations, shape hints); rowan-inspired CST representation; Backend IR ~22 variants (refines the BC.W0 starting point) |
| 2 | **Type System Designer** | Pierce-Turner + Hindley-Milner + bidirectional inference; CSP-backed unification; full subtyping with subsumption; type annotation surface (hybrid: pure inference default, first-class explicit annotations); generic rules; multi-function chaining type semantics; closure semantics for `@host fn` directive; cite primary research (Dunfield-Krishnaswami; Pierce-Turner; Hindley-Milner; chalk; egg's Language derive) |
| 3 | **CSP & E-graph Architect** | designs CSP variables, propagation (AC-3, GAC, conflict-driven backtracking); e-graph rewrites (algebraic, charclass merging, keyword sets, operator-chain detection, repeat-loop hoisting, tail-call elimination, non-progressing-Alt removal); CSP↔egraph union/bridge for cross-substrate decisions; saturation discipline; cost-model integration |
| 4 | **Cost Model Architect** | `Cost` trait (parser + regex implement); local/global/per-path costs; per-construct + per-rule + per-path; trait-based shared analytical surface; integration with regex cost (state count, DFA size, backtrack risk); fixed-point convergence guarantees |
| 5 | **Grammar Extension Designer** | the three ffuzzy primitives (rewrite-mode `mode = "rewrite"` parser attribute; bounded-width lookbehind operator `\|<`; Unicode char-class algebra `[:L:]` + `A & B` + `A - B` + `A \| B` + `^A` + named productions); `@host fn` directive with closure semantics; `@error(skip \| recover \| halt)` directive; `@layout(struct \| enum \| tuple \| slice)` hint; generic rules `Object<V>`; multi-function chaining (Rust-style `.method()` or piped `\|>`); BBNF grammar formal specification |
| 6 | **Substrate Coherence Auditor** | verifies all five interlock; identifies orthogonal codepath risks; verifies "everything grammar-derived" anthem; identifies fault patterns from prior restart attempts the new design avoids; verifies the future-grammar onboarding test (yaml.bbnf passes via TWO surfaces only) |

## Per-Item Discipline — Pro / Con / Explication / Challenge

Every claim, gate, decision, surgery, verdict, and proposal in PASS-1 outputs carries:

- **Explication** — what the item *means*; the underlying intent; what it commits the project to
- **Pros** — why it earns its place; benefits relative to alternatives; locks/precepts honoured
- **Cons** — costs it imposes; failure modes; locks/precepts strained
- **Challenge** — the adversarial counter-position; the steelman alternative; the redline this item must survive

Verdicts: **KEEP** (pros outweigh cons; challenge defeated) / **REINVENT** (pros real but current shape carries surplus con; redesign named) / **DISCARD** (cons outweigh pros; challenge wins; replacement named).

## Per-Sub-Agent Output

Each sub-agent writes to `restart/audit/pass-1-substrate/agent-{N}-{lens}.md`. ~500-1000 lines per sub-agent. Each output includes:

§1 Scope + framing
§2 Per-item table (the dominant shape; Pro/Con/Explication/Challenge columns; verdict column)
§3 Architectural commitments ratified (the KEEPs and REINVENTs that survive challenge)
§4 New facilities proposed (items not extant today; located at named paths under `restart/`)
§5 Cross-cuts to PASS-2 / PASS-3 (concerns spanning sister passes; named hand-offs)
§6 Risk + mitigation table
§7 Inheritance ledger — which BA / BB / BC / BD waves' substance survives, which dissolve, which re-anchor

## Synthesis (your output)

After all six sub-agents commit, you produce `restart/audit/pass-1-substrate/PASS-1.md`, ~1500-2500 lines, structured:

§1 PASS-1 verdict ledger — every concern surfaced; per-item KEEP/REINVENT/DISCARD; per-sub-agent attribution
§2 Substrate architectural commitments — the consolidated set (IR shape, type system algorithm, CSP + e-graph composition, cost-model trait, BBNF grammar specification, host-fn primitive library, error vocabulary, VM scope, multi-function chaining semantics)
§3 Per-crate `src/` tree — for each PASS-1 crate (`error`, `pipeline`, `source`, `grammar`, `ir`, `passes`, `vm`, `host`, `cost-model`, `egraph`, `csp-solver`, `parse-that`), the proposed `src/` module layout (4-10 children per dir; sibling-API uniformity per Lock 13)
§4 Hand-offs to PASS-2 — named contracts (Grammar IR variants; Backend IR variants; Backend IR consumer interface; cost-model trait public API; e-graph rewrite plug-in registry interface)
§5 Hand-offs to PASS-3 — named contracts (host-fn dispatch from grammar; error type vocabulary the user surfaces consume; debug VM hooks for incremental parsing)
§6 BBNF grammar formal specification — the canonical EBNF or formal definition of the new BBNF (with rewrite-mode + lookbehind + Unicode + generics + `@host fn` + `@error` + `@layout` + multi-function chaining + closure semantics)
§7 Inheritance ledger — wave-by-wave cite of legacy BA/BB/BC/BD waves whose substance carries forward
§8 PASS-1 punch list — surgical edits / tranche commitments for the master plan
§9 Voice + discipline locks
§10 Closing posture

## Voice + Discipline

(Per `restart/README.md` §13. Calibrated; archaic-permissive; no metalanguage; path:line citations on every concrete claim; tables liberal; per-X tables for every "all-X" claim; no "TBD" / "user adjudicates" / "future without receiver"; no quick solutions; no workarounds; idiomatic gestalt.)

## Hard cap

You: 75 minutes. Each sub-agent: 45 minutes (longer than Pass-A/B/C precedent because per-item Pro/Con/Explication/Challenge discipline is heavier per-row).

If sub-agents stall, dispatch with the incremental-commit cadence pattern (skeleton phase → §1-§3 phase → §4-§6 phase → §7-§10 phase) per the prior MASTER-PLAN hardening continuation precedent (commit `a950da03bd88f9347`). The watchdog tracks idle time between tool calls; commits between phases reset.

## Output commits

Per sub-agent: `docs(restart/audit/pass-1-substrate/agent-{N}): {lens}`.
Orchestrator final: `docs(restart/audit/pass-1-substrate): synthesise PASS-1 — substrate`.

## Cross-tranche scope boundary

You touch ONLY `restart/audit/pass-1-substrate/`. Do NOT modify `crates/`, `docs/tranches/`, `docs/precepts/`, `restart/prompts/` (suite definition; read-only), `restart/locks/`, `restart/corpora/`, `restart-archive-2026-05-04/`. Do NOT execute git operations beyond per-sub-agent commits.

## Background

PASS-1 is the bottom layer; PASS-2 (codegen + runtime) and PASS-3 (user-facing API + ecosystem) consume PASS-1's contracts. Sister passes run in parallel against `restart/audit/pass-{2,3}-{codegen,runtime}/`. The synthesizer consolidates all three into the master plan; hardening verifies; tranche drafting executes.

The 14 locks are settled. The precepts are settled. The 35-answer interrogation is settled. The greenfield mandate is settled. The ffuzzy three primitives are settled. PASS-1 specifies; PASS-1 does not relitigate.

The user-stated discipline: **no quick solutions, no workarounds, no legacy code uncontested, no contrivance, no overfitting.** Architectural transpositions for elegance, simplicity, performance are mandatory. The CSP + e-graph + shape mining + cost model is the apotheosis. Grammar-derived everything. Familiar user-facing API. Deep optimisation internals.
