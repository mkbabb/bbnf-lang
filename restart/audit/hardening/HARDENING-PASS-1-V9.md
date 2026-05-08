# HARDENING-PASS-1-V9 — PASS-1 substrate audit

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` |
| Cycle | V9, post-V8.1 freshness audit |
| Scope | PASS-1 only; no implementation amendment |
| Lines audited | 360 (`wc -l restart/audit/pass-1-substrate/PASS-1.md`) |
| Prior verdict | V8.1 PASS-1 READY (`restart/audit/hardening/HARDENING-PASS-1-V8.1.md:176-186`) |
| V9 verdict | **AMENDMENT-REQUIRED-NARROW** |

The V8.1 READY verdict holds on the main substrate shape: tape/direct union, two IRs, side tables, DK13 + GADT V1 surface, CHR-improvement V1, six-directive PASS-1 grammar, `Call { kind: Map | Host }`, BIR 19-reference, alphabetic diagnostics, and Lock 14 onboarding proof. V9 surfaces four narrow faults: one active Lock 5 / Lock 8 backend-scope contradiction, one line-citation hygiene fault, one host-language delegation overclaim, and one cross-doc drift item against ARCH §8.1.

## §2 Lens table

| Lens | Verdict | Finding(s) | Recommendation |
|---|---|---|---|
| Lane 1 — Lock-adherence | REINVENT | PASS-1 keeps WASM V1 obligations despite Lock 5/8 deferring WASM post-V1. | Narrow edit PASS-1 §2 / §5 / §6 per P1. |
| Lane 2 — Sequencing | N/A | PASS-1 is not a multi-wave tranche plan. | No action. |
| Lane 3 — Cohesion | REINVENT | PASS-1 cites retired prompt paths that are absent from the current prompt suite. | Replace stale citations per P2. |
| Lane 4 — SOTA anchoring | KEEP | PASS-1 carries no throughput close gate; the only backend-scope fault is handled under Lane 1 / Lens K. | No SOTA-number edit in PASS-1. |
| Lane 5 — Grammar-authoritative / Lock 14 | KEEP | No grammar match arms found; yaml onboarding proof is two author inputs only. | No action. |
| Lane 6 — Generated-code budget | KEEP | Budget schema exists and names LOC, delta, regen wall, and evidence columns. | No action. |
| Lane 7 — Friction forecast | REINVENT | Capture-by-move diagnostic is useful, but PASS-1 assigns it to parse/rustc in a way the syntax and host semantics do not support. | Reword per P3. |
| Lane 8 — Carry / deferral | KEEP-WITH-NARROW-FIX | Tranche-D carries mostly name receiver + blocker + gate; WASM V1 rows are the exception because they turn a V2 carry into V1 scope. | P1 closes. |
| Lane 9 — Greenfield discipline | REINVENT | WASM V1 columns add apparatus beyond the V1 Rust line. | P1 closes. |
| Lens F — LLM bias | KEEP | Prose is direct enough; no blocking hedging pattern. | No action. |
| Lens G — Overfitting | KEEP | Grammar-name mentions are audit anchors / fixture names / per-X rows, not generic-crate logic. | No action. |
| Lens H — Hallucination + provenance | REINVENT | Retired prompt citations are wrong-line/wrong-file provenance. | P2 closes. |
| Lens I — Contrivance | REINVENT | WASM V1 obligation table double-tracks the real Backend trait deferral. | P1 closes. |
| Lens J — Host-language delegation | REINVENT | Closure-capture wording overstates rustc rejection and misplaces the bbnf diagnostic stage. | P3 closes. |
| Lens K — Meta-grammar discipline | REINVENT | PASS-1 should specify backend-neutral substrate plus Rust V1 realization; WASM/TS bodies are V2 Backend impls. | P1 closes. |

## §3 Findings

### V9-P1 — WASM V1 scope survived the V8.1 fold

| Site | Evidence | Fault |
|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md:61` | "Rust V1 and WASM V1 are in scope" while also saying future `WasmBackend` lands V2. | The sentence carries both the retired and corrected model. |
| `restart/audit/pass-1-substrate/PASS-1.md:63-71` | The obligations table has a `WASM V1 lowering obligation` column. | V1 has no active WasmBackend. |
| `restart/audit/pass-1-substrate/PASS-1.md:207` | `Rust/WASM parity` says Rust and WASM V1 share BIR semantics and points to H/J parity gates. | It turns a V2 carry into a V1 hand-off. |
| `restart/audit/pass-1-substrate/PASS-1.md:290` | "all backends" applies to Rust V1, WASM V1, TS scaffold. | It contradicts the active-backend matrix. |

The governing sources are explicit: Lock 5 says "TS and WASM backends defer post-V1; V1 ships the Rust impl only" (`restart/locks/14-LOCKS.md:42`), Lock 8 says WASM SOTA defers post-V1 (`restart/locks/14-LOCKS.md:48`), and ARCH §7.5 says V1 ships `RustBackend: Backend` only while V2 adds `WasmBackend` and `TsBackend` (`restart/ARCHITECTURE.md:1090-1097`). V8.1 missed this by affirming the Rust+WASM table as READY (`restart/audit/hardening/HARDENING-PASS-1-V8.1.md:81`).

Verdict: **REINVENT**. Keep backend-neutral BIR obligations, but express realization as Rust V1 only plus V2 Wasm/Ts rows.

### V9-P2 — Retired prompt citations remain active provenance

PASS-1 cites `restart/prompts/PASS-1-SUBSTRATE.md` at `restart/audit/pass-1-substrate/PASS-1.md:16` and `restart/audit/pass-1-substrate/PASS-1.md:328`. HANDOFF says that prompt was retired at Phase 8.0 with `PASS-2-CODEGEN.md`, `PASS-3-RUNTIME.md`, and `SYNTHESIS.md` (`restart/HANDOFF.md:71-79`); the live prompt suite is the five-file orchestrator set (`restart/HANDOFF.md:71-77`, `restart/prompts/ORCHESTRATOR.md:7-17`). The file is absent from `restart/prompts/`.

Verdict: **REINVENT**. Retired dispatch prompts may be archaeology, but current PASS-1 claims need live path:line provenance or agent-report provenance.

### V9-P3 — Closure capture diagnostic is assigned to the wrong layer

PASS-1 says rustc rejects move-captures from generated `&'i Tape<'i>` references and frames `BBNF-CLOSURE-CAPTURE-BY-MOVE` as a parse-time diagnostic (`restart/audit/pass-1-substrate/PASS-1.md:87`). §6 repeats that capture-by-move is a parse error (`restart/audit/pass-1-substrate/PASS-1.md:263`). Lock 4 only requires that closures capture by `&'i` and capture-by-move is forbidden in V1 (`restart/locks/14-LOCKS.md:40`); Lens J specifically asks the audit to use Rust's borrow checker where it truly applies and avoid redundant lifetime machinery (`restart/prompts/HARDENING.md:164-175`).

The BBNF V1 syntax has no `move` capture marker in `LambdaExpr` (`restart/audit/pass-1-substrate/PASS-1.md:241-255`), so parse-time rejection is not the right contract unless a future `move` keyword is added. The useful V1 contract is: closures lower as borrowed environments; `passes::layout` / closure environment validation rejects any non-borrow capture mode before emission; rustc remains the final generated-source correctness gate.

Verdict: **REINVENT**. Keep the diagnostic, move it out of parser semantics, and remove the rustc-overclaim.

### V9-X1 — ARCH §8.1 contradicts PASS-1's six-directive grammar

PASS-1's grammar production is lock-correct: `Directive = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl ;` (`restart/audit/pass-1-substrate/PASS-1.md:214-223`), matching Lock 10 (`restart/locks/14-LOCKS.md:52`). ARCH §13.1 also enforces six directives (`restart/ARCHITECTURE.md:1666-1669`). But ARCH §8.1 currently writes `Directive ::= ImportDecl | HostFn | RuleDecl | LayoutDecl | ErrorDecl | PrettyDecl | TokenDecl` (`restart/ARCHITECTURE.md:1167-1175`) and then calls it the "six-directive" canon (`restart/ARCHITECTURE.md:1221-1236`).

Verdict: **CROSS-DOC DRIFT, PASS-1 KEEP**. PASS-1 should not change for this item; the MASTER-PLAN / ARCH hardener should route an ARCH-only edit: `Grammar ::= { Directive | RuleDecl }` and `Directive ::= ImportDecl | HostFn | LayoutDecl | ErrorDecl | PrettyDecl | TokenDecl`.

## §4 Ratified surfaces

| Surface | Evidence | V9 result |
|---|---|---|
| Lock 14 onboarding | yaml proof permits only `grammars/yaml.bbnf` and metadata, forbids `crates/yaml/` and registry edits (`restart/audit/pass-1-substrate/PASS-1.md:275-283`). | KEEP |
| Grammar-name grep | The broad grammar-name matches in PASS-1 are fixture paths, per-X table cells, archaeology, or diagnostic examples; the generic match-arm grep returned zero. | KEEP |
| BIR ownership | PASS-1 says ARCH §7.2 owns the authoritative BIR set and PASS-2 may refine payloads, not redefine variants (`restart/audit/pass-1-substrate/PASS-1.md:41-57`). | KEEP |
| Diagnostic numeric retirement | PASS-1 catalogue is alphabetic-only (`restart/audit/pass-1-substrate/PASS-1.md:119-133`). | KEEP |
| Carry triples | Rank-N body, schema telemetry, or-patterns, match guards, row-poly surface, and structural-decreasing generic detection route to tranche D / D.W3 / D.W6 with blockers stated in the surrounding prose (`restart/audit/pass-1-substrate/PASS-1.md:73`, `restart/audit/pass-1-substrate/PASS-1.md:89-93`, `restart/audit/pass-1-substrate/PASS-1.md:355-357`). | KEEP, except WASM rows fixed by P1 |

## §5 Punch list

| # | Target | Surgery | Source verdict | Owner | Scope |
|---:|---|---|---|---|---|
| P1 | `restart/audit/pass-1-substrate/PASS-1.md:61-71`, `:207`, `:290` | Replace WASM V1 language with "Rust V1 active; V2 `WasmBackend: Backend` and `TsBackend: Backend` consume the same BIR without alphabet changes." Remove the WASM V1 obligation column or retitle it V2-reference / deferred-backend obligation. Change `Rust/WASM parity` to V2 carry. Change "all backends" applies-to cell to `RustBackend V1; WasmBackend/TsBackend V2`. | REINVENT | PASS-1 amendment | Lock 5, Lock 8, Lane 1, Lens I/K |
| P2 | `restart/audit/pass-1-substrate/PASS-1.md:16`, `:328` | Remove live citations to `restart/prompts/PASS-1-SUBSTRATE.md`. For rewrite-mode provenance, cite live README rejection plus agent-5 archaeology (`restart/audit/pass-1-substrate/agent-5-grammar-extension-designer.md:39-42`) or omit the prompt clause. For citation discipline, cite `restart/prompts/HARDENING.md:230-246`. | REINVENT | PASS-1 amendment | Lane 3, Lens H |
| P3 | `restart/audit/pass-1-substrate/PASS-1.md:87`, `:131`, `:263` | Rephrase `BBNF-CLOSURE-CAPTURE-BY-MOVE` as a `passes::layout` / closure-environment validation diagnostic emitted before Rust source emission. Remove "parse-time" / "parse error" unless a `move` token is added to the grammar. Replace "rustc rejects move-captures from such references" with "rustc remains the final borrow/lifetime correctness gate for generated borrowed environments." | REINVENT | PASS-1 amendment | Lane 7, Lens J |
| X1 | `restart/ARCHITECTURE.md:1167-1175` | ARCH-only: move `RuleDecl` out of `Directive` so §8.1 matches Lock 10 and PASS-1. | CROSS-DOC | MASTER-PLAN / ARCH hardener | Cross-target drift |

## §6 Final decision

**Decision: AMENDMENT-REQUIRED-NARROW.**

PASS-1 does not need re-draft. Its core substrate survives V9: grammar-derived onboarding, two-IR boundary, BIR ownership, type-system fold, diagnostic simplification, and Lock 14 all hold. The blockers are surgical: retire the surviving WASM V1 rows, replace retired prompt citations, and move closure-capture diagnostics to the semantic validation layer. ARCH §8.1 carries a separate six-directive drift item for the MASTER-PLAN / ARCH hardener.

Hereupon PASS-1 should receive a narrow amendment before it is treated as a clean Wave 9 substrate baseline.
