# HARDENING-MASTER-PLAN-V9.1 - MASTER-PLAN trio verification

Cycle V9.1 verifies the amended MASTER-PLAN trio and state documents after the
V9 hardening consolidation. Scope is read-only against the target corpus; this
report is the sole output.

## 1. Target Identification

| Field | Value |
|---|---|
| Target | MASTER-PLAN trio, V9.1 verification |
| Workspace | `/Users/mkbabb/Programming/bbnf-lang` |
| Baseline commit observed | `af3d1a73` |
| Verification time | 2026-05-07T18:16:26-04:00 |
| Primary target files | `restart/ARCHITECTURE.md` (1,725 lines), `restart/MIGRATION.md` (817), `restart/MASTER-PLAN.md` (886) |
| State files | `restart/HANDOFF.md` (185), `restart/prompts/ORCHESTRATOR.md` (145) |
| Hardening authorities | `restart/prompts/audit-specs/HARDENING-LENS-SET.md`, `restart/audit/hardening/HARDENING-MASTER-PLAN-V9.md`, `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.md` |
| Support file inspected | `restart/README.md` (458) |

The worktree already contained external V9 edits and untracked V9 hardening
reports before this file was created. Those changes were treated as other-agent
work and were not edited.

## 2. Targeted Scan Log

| Scan | Result |
|---|---|
| ``rg -n 'Rust \+ WASM in scope|WASM.*V1|V1.*WASM|wasm = true|WASM lower-and-bench|WASM/TS parity|V1 carries no WASM|V1 ships `RustBackend` only|path-ts.*defer|path-ts.*V2' restart/{README.md,ARCHITECTURE.md,MIGRATION.md,MASTER-PLAN.md,HANDOFF.md,prompts/ORCHESTRATOR.md}`` | Trio and state docs route `path-ts`, TS, WASM, parity, and ABI to V2; support README still has one stale section 9 line saying "Rust + WASM in scope" at `restart/README.md:328`. |
| `rg -n '23 variants|23-variant' restart/{README.md,ARCHITECTURE.md,MIGRATION.md,MASTER-PLAN.md,HANDOFF.md,prompts/ORCHESTRATOR.md}` | Zero matches in the live verification surface. |
| `rg -n '20 variants|20-variant|19 semantic|Return|BIR' ...` | Current BIR authority is 20 rows / 19 semantic variants plus `Return` at `restart/ARCHITECTURE.md:905`, `restart/ARCHITECTURE.md:934-936`, `restart/MASTER-PLAN.md:35-36`, `restart/MASTER-PLAN.md:391`, `restart/MIGRATION.md:386`, `restart/README.md:111`. |
| `rg -n 'Grammar ::=|Directive ::=|RuleDecl|@import|@host fn|@error|@layout|@pretty|@token' ...` | `RuleDecl` is outside `Directive` at `restart/ARCHITECTURE.md:1166`, `restart/ARCHITECTURE.md:1172`, and explained at `restart/ARCHITECTURE.md:1220`; six-directive canon is repeated in `restart/HANDOFF.md:60`. |
| `rg -n 'HostCall|Map|Call \{ kind: Map \| Host \}|kind: Map|kind: Host|Grammar IR' ...` | Grammar IR has `Call (kind: Map | Host)` at `restart/ARCHITECTURE.md:866` and payload row at `restart/ARCHITECTURE.md:885`; remaining `HostCall` hits are grammar syntax productions, not Grammar IR variants. |
| `rg -n 'BBNF-[A-Z]+[0-9]|BBNF-RECOVERY001|BBNF-RECOVERY\*|numeric diagnostic|diagnostic vocabulary|mnemonic' ...` | Numeric diagnostics survive only as deletion archaeology at `restart/ARCHITECTURE.md:1040-1044` and `restart/MASTER-PLAN.md:874`; yaml syntax friction uses `BBNF-RECOVERY*` at `restart/MASTER-PLAN.md:823`. |
| `rg -n 'OpenFrame|parallel-substrate|parallel substrate|clone absence|clone-absent|Vec<OpenFrame>' ...` | ARCH owner row is neutral at `restart/ARCHITECTURE.md:1499`; OpenFrame references are old-stack absence or archaeology, e.g. `restart/MASTER-PLAN.md:306`, `restart/MIGRATION.md:343-345`. |
| `rg -n 'V9\.1|Phase 8|Phase-8|Phase 8\.3|Phase 8\.4|Wave 9|current' restart/HANDOFF.md restart/prompts/ORCHESTRATOR.md ...` | HANDOFF explicitly says V9.1 verification is pending before Wave 9 (`restart/HANDOFF.md:4`, `restart/HANDOFF.md:47-49`, `restart/HANDOFF.md:161-169`, `restart/HANDOFF.md:181-185`). ORCHESTRATOR names V9 as the active pre-Wave-9 gate and V9.1 as the paired output (`restart/prompts/ORCHESTRATOR.md:52`, `restart/prompts/ORCHESTRATOR.md:68`). |
| `rg -n 'Cross-host metadata carrier|cross-host metadata|739-745|sidecar|metadata carrier' ...` | MASTER-PLAN citation now points to `ARCH section 5:739-745` at `restart/MASTER-PLAN.md:875`; target ARCH note is at `restart/ARCHITECTURE.md:739-745`. |
| `rg -nP 'match\s+\w+\s*\{[^}]*((Json|CssL4|Bbnf\w*|GoogleSheets\w*)\s*=>)' restart/{ARCHITECTURE.md,MIGRATION.md,MASTER-PLAN.md,README.md}` | Zero matches. Grammar-name hits are per-X tables, fixture rows, macro examples, or audit anchors, not generic-crate dispatch logic. |

## 3. Lens Sweep A-K

| Lens | Verdict | Verification |
|---|---|---|
| A - Inter-document narrative coherence | AMENDMENT-REQUIRED-NARROW | Trio, HANDOFF, and ORCHESTRATOR now agree on V9 closure shape; README support line `restart/README.md:328` still says "Rust + WASM in scope" and conflicts with the V1 RustBackend/V2 WASM rule. |
| B - Vocabulary drift | AMENDMENT-REQUIRED-NARROW | `path-ts`, `TsBackend`, and `WasmBackend` vocabulary is coherent in ARCH/MIGRATION/MASTER-PLAN; README section 9 keeps stale "WASM in scope" wording. |
| C - Worked-example scarcity | READY | yaml onboarding, `path!`/`select!`, metadata sidecar, diagnostic, and SOTA rows have concrete examples and gates (`restart/ARCHITECTURE.md:1572-1599`, `restart/MASTER-PLAN.md:816-823`). |
| D - Coverage gaps | READY | The V9 punch list was scanned directly; no `match ... Json =>` / `CssL4 =>` / `Bbnf =>` hardcoded dispatch arm remains. |
| E - Architectural axiom cumulative consistency | AMENDMENT-REQUIRED-NARROW | The target trio obeys V1 Rust-only; README support prose at `restart/README.md:328` still strains Lock 5/8/11 wording. |
| F - LLM bias | AMENDMENT-REQUIRED-NARROW | Numeric pseudo-precision has been downgraded to deletion archaeology; the remaining support fault is stale confident scope prose in README section 9. |
| G - Overfitting | READY | `path-ts` and backend rows are V2 receivers, not V1 apparatus; generic grammar scan found no per-grammar match-arm dispatch. |
| H - Hallucination/provenance | READY | The wrong carrier citation is fixed to `ARCH section 5:739-745`; BIR count citations point to live ARCH section 7.2. |
| I - Contrivance / over-engineering | READY | `RuleDecl` is not a directive, Grammar IR callable shape is collapsed, and V1 active backend cardinality is one. |
| J - Host-language leverage | READY | ARCH section 3.1 carries the Rust-line parse API note and lets V2 WASM/TS expose host-idiomatic allocation/GC forms without BIR changes. |
| K - Meta-grammar discipline | AMENDMENT-REQUIRED-NARROW | The trio keeps bbnf as a V1 Rust parser-generator with V2 backend receivers; README section 9 still blurs V1 WASM scope. |

## 4. V9 Punch-List Closure

| V9 item | Status | Evidence |
|---|---|---|
| 1. V1 RustBackend only; `path-ts` / TS / WASM V2 deferred | PARTIAL | Closed in trio and state docs: `restart/ARCHITECTURE.md:733`, `restart/ARCHITECTURE.md:1093-1095`, `restart/ARCHITECTURE.md:1441-1442`, `restart/ARCHITECTURE.md:1617-1622`; `restart/MASTER-PLAN.md:488-489`, `restart/MASTER-PLAN.md:567`, `restart/MASTER-PLAN.md:794-803`; `restart/MIGRATION.md:71`, `restart/MIGRATION.md:126`, `restart/MIGRATION.md:661`, `restart/MIGRATION.md:681-682`, `restart/MIGRATION.md:801-803`; `restart/HANDOFF.md:17`, `restart/HANDOFF.md:22`, `restart/HANDOFF.md:59`. README support mostly agrees at `restart/README.md:46`, `restart/README.md:52`, `restart/README.md:111`, `restart/README.md:391-393`, but `restart/README.md:328` remains stale. |
| 2. BIR count is 20 rows / 19 semantic plus `Return` | VERIFIED | `restart/ARCHITECTURE.md:905`, `restart/ARCHITECTURE.md:934-936`, `restart/MASTER-PLAN.md:35-36`, `restart/MASTER-PLAN.md:51`, `restart/MASTER-PLAN.md:184`, `restart/MASTER-PLAN.md:391`, `restart/MIGRATION.md:386`, `restart/README.md:111`; `rg '23 variants|23-variant'` returned zero. |
| 3. ARCH grammar sketch keeps `RuleDecl` outside `Directive` | VERIFIED | `restart/ARCHITECTURE.md:1166` has `Grammar ::= (Directive | RuleDecl)*`; `restart/ARCHITECTURE.md:1172` defines `RuleDecl`; `restart/ARCHITECTURE.md:1220` says `RuleDecl` is a grammar member, not a directive. |
| 4. ARCH Grammar IR collapses `Map` + `HostCall` to `Call { kind: Map | Host }` | VERIFIED | `restart/ARCHITECTURE.md:866` and `restart/ARCHITECTURE.md:885` carry the collapsed `Call` shape; `restart/audit/pass-1-substrate/PASS-1.md:24` matches. |
| 5. Numeric diagnostic residue is deletion archaeology only; yaml row uses mnemonic family | VERIFIED | Deletion archaeology is explicit at `restart/ARCHITECTURE.md:1040-1044` and `restart/MASTER-PLAN.md:874`; yaml syntax row uses `BBNF-RECOVERY*` at `restart/MASTER-PLAN.md:823`. |
| 6. OpenFrame perf owner row uses neutral parallel-substrate wording | VERIFIED | Owner row is `Parallel-substrate clone absence` at `restart/ARCHITECTURE.md:1499`; OpenFrame hits are old-stack absence checks or archaeology. |
| 7. ARCH section 3.1 has cross-host parse API note | VERIFIED | `restart/ARCHITECTURE.md:218-223` identifies `parse` / `parse_in` / `parse_owned` as V1 Rust-line surfaces and routes V2 WASM/TS host entrypoint spelling to backend APIs. |
| 8. HANDOFF and ORCHESTRATOR point to V9.1 verification, not stale Phase 8 dispatch | VERIFIED | HANDOFF is explicit at `restart/HANDOFF.md:4`, `restart/HANDOFF.md:47-49`, `restart/HANDOFF.md:161-169`, `restart/HANDOFF.md:181-185`; ORCHESTRATOR names V9 as active before Wave 9 at `restart/prompts/ORCHESTRATOR.md:52` and names V9.1 output at `restart/prompts/ORCHESTRATOR.md:68`. |
| 9. MASTER-PLAN cross-host carrier citation points to current ARCH section 5 lines | VERIFIED | MASTER-PLAN now cites `ARCH section 5:739-745` at `restart/MASTER-PLAN.md:875`; the target note is present at `restart/ARCHITECTURE.md:739-745`. |

## 5. Remaining Residues

| ID | Site | Residue | Required narrow amendment | Lenses |
|---|---|---|---|---|
| V9.1-R1 | `restart/README.md:328` | Support README still says "Backend agnostic in design; Rust + WASM in scope; TS scope-deferred", contradicting the amended V1 RustBackend-only rule. | Rewrite to: "Backend agnostic in design; V1 measures the Rust line, while WASM and TS defer to V2 `WasmBackend: Backend` / `TsBackend: Backend` without BIR retrofit." | A, B, E, F, K |
| V9.1-R2 | `restart/README.md:170`, `restart/README.md:268` | Generic-rule paragraphs say WASM handles generic lowering via type erasure + dispatch inside V1 "Land V1" prose. This can be read as future backend-design context, but it is not explicitly marked V2 like the rest of the corpus. | Qualify the parenthetical as V2 WASM lowering behavior, leaving generic rules themselves V1 on the Rust line. | A, B, J, K |

No remaining trio-internal blocker was found. The residues are support-document
scope wording, not architecture-contract contradictions in ARCH/MIGRATION/
MASTER-PLAN.

## 6. Final Decision

**Decision: AMENDMENT-REQUIRED-NARROW.**

The amended MASTER-PLAN trio closes the V9 punch list: backend scope, BIR
cardinality, directive grammar, Grammar IR callable shape, diagnostic naming,
OpenFrame wording, parse API host split, state-doc gate, and carrier citation
all verify against current line evidence. The only remaining residues are README
support prose that still blurs WASM as V1 scope. This does not require re-draft;
it requires a narrow support-doc wording pass before the corpus should be called
fully READY.

Hereupon: amend the two README support rows, then rerun the V9.1 support scan or
fold that confirmation into the consolidated V9.1 report.
