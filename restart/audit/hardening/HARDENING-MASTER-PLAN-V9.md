# HARDENING-MASTER-PLAN-V9 — MASTER-PLAN trio audit

Cycle V9 audits the MASTER-PLAN trio after V8.1. Scope is read-only against
`restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, and
`restart/MASTER-PLAN.md`; this report is the sole output path.

## §1 Target identification

| Target | Path | Lines audited | Baseline commit | Verdict |
|---|---|---:|---|---|
| Architecture | `restart/ARCHITECTURE.md` | 1,727 | `af3d1a73` | AMENDMENT-REQUIRED-NARROW |
| Migration | `restart/MIGRATION.md` | 816 | `af3d1a73` | AMENDMENT-REQUIRED-NARROW |
| Master plan | `restart/MASTER-PLAN.md` | 886 | `af3d1a73` | AMENDMENT-REQUIRED-NARROW |

The V8.1 baseline records five known non-blocking residues and asks V9 to
rediscover them if still present (`restart/audit/hardening/HARDENING-CONSOLIDATED-V8.1.md:73-81`).
All five still reproduce, and V9 surfaces additional trio conflicts.

## §2 Lens table

| Lens | Verdict | Finding pressure |
|---|---|---|
| Lane 1 — Lock-adherence | REINVENT | Lock 5/7/8 V2 deferral conflicts with ARCH V1 `path-ts`/WASM rows; Lock 10 six-directive canon is contradicted by the grammar sketch. |
| Lane 2 — Sequencing discipline | REINVENT | J.W0 still gates WASM V1 parity after H and §24 route WASM to V2. |
| Lane 3 — Cohesion | REINVENT | BIR counts, Grammar IR variant shape, and HANDOFF state disagree across live authorities. |
| Lane 4 — SOTA anchoring | KEEP | Exact SOTA rows carry competitor, dataset, platform, and owner; the WASM residue is sequencing/carry, not a benchmark-number fault. |
| Lane 5 — Grammar-authoritative discipline | REINVENT | Future-grammar proof is strong, but V1 `path-ts`/TS rows widen the two-surface Rust-line story and contradict the carry ledger. |
| Lane 6 — Generated-code + LOC budget | KEEP | ARCH §12.2 and MASTER-PLAN §20 carry per-grammar budgets and gates. |
| Lane 7 — Friction forecast | REINVENT | MASTER-PLAN §24 retains a numeric recovery diagnostic after numeric alias retirement. |
| Lane 8 — Carry & deferral audit | REINVENT | HANDOFF current state, WASM parity, `path-ts`, and cross-host citation rows have stale receivers or wrong status. |
| Lane 9 — Greenfield discipline | REINVENT | The residual V1 TS/WASM apparatus is inherited plan shape rather than load-bearing V1 work. |
| Lens F — LLM bias | REINVENT | Stale pseudo-precision survives as exact but wrong variant counts and phase-status prose. |
| Lens G — Overfitting | REINVENT | `path-ts` and WASM V1 rows look carried from the prior multi-backend plan despite V8.1's Rust-line V1 decision. |
| Lens H — Hallucination/provenance | REINVENT | At least one cross-host metadata citation points at the wrong line range. |
| Lens I — Contrivance | REINVENT | `path-ts` in the V1 workspace and `RuleDecl` inside `Directive` add apparatus with no V1 use. |
| Lens J — Host-language leverage | REINVENT | Rust-line parse API lacks the promised cross-host divergence note; host carriers are otherwise routed correctly. |
| Lens K — Meta-grammar discipline | REINVENT | The V1 meta-grammar boundary is clear in some rows, but TS/WASM and directive grammar drift exceed it. |

Final lane count: 2 KEEP, 13 REINVENT, 0 DISCARD. No re-draft threshold is met;
the required work is a surgical coherence amendment.

## §3 Findings

| ID | Site | Fault | Surgery | Lenses |
|---|---|---|---|---|
| V9-1 | `restart/ARCHITECTURE.md:57`, `restart/ARCHITECTURE.md:63`, `restart/ARCHITECTURE.md:294`, `restart/ARCHITECTURE.md:520-524`, `restart/ARCHITECTURE.md:643`, `restart/ARCHITECTURE.md:733`, `restart/MASTER-PLAN.md:263`, `restart/MASTER-PLAN.md:567`, `restart/MASTER-PLAN.md:794-803`, `restart/MIGRATION.md:71`, `restart/MIGRATION.md:126`, `restart/MIGRATION.md:202`, `restart/MIGRATION.md:680-681` | V1 TS/WASM deferral is not coherent. ARCH still makes `path-ts` a public/member crate and says `wasm = true` selects a WASM V1 lowerer; MASTER-PLAN says `path-ts` must not appear as a V1 workspace member and routes TS/WASM to V2; J.W0 nevertheless requires WASM V1 parity. | Remove `path-ts` from V1 ARCH workspace/API/module/Cargo skeleton rows or mark every `path-ts` row as V2-only. Change ARCH `codegen`/metadata rows from WASM V1 to V2 `WasmBackend`. Change MASTER-PLAN J.W0 to Rust+VM parity only. Move MIGRATION `path-ts` rows to V2/deferred disposition rather than G-owned V1 work. | 1, 2, 5, 8, 9, G, I, K |
| V9-2 | `restart/ARCHITECTURE.md:913-940`, `restart/MASTER-PLAN.md:35-36`, `restart/MASTER-PLAN.md:51`, `restart/MASTER-PLAN.md:184`, `restart/MASTER-PLAN.md:391`, `restart/MIGRATION.md:386` | BIR alphabet count drifts. ARCH and MASTER-PLAN summary now say 20 variants / 19 semantic plus `Return`, while MASTER-PLAN §1, Tranche E inheritance, and MIGRATION §6 still cite 23 variants. | Replace every live trio "23 variants" row with "20-variant shape (19 semantic variants plus `Return`) per ARCH §7.2"; keep PASS-2 23 only as archaeology if explicitly labelled pre-fold. | 3, F, H, I |
| V9-3 | `restart/ARCHITECTURE.md:1168-1169`, `restart/ARCHITECTURE.md:1174-1180`, `restart/ARCHITECTURE.md:1221-1236`, `restart/HANDOFF.md:58-60` | The grammar sketch puts `RuleDecl` inside `Directive` while the settled canon says six directives: `@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`. Rule declarations are grammar members, not directives. | Change the sketch to `Grammar ::= (Directive | RuleDecl)*` and `Directive ::= ImportDecl | HostFn | LayoutDecl | ErrorDecl | PrettyDecl | TokenDecl`; keep the six-directive paragraph intact. | 1, 3, 9, I, K |
| V9-4 | `restart/ARCHITECTURE.md:866-887`, `restart/audit/pass-1-substrate/PASS-1.md:24`, `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.1.md:24` | ARCH §7.1 still carries separate Grammar IR `Map` and `HostCall` variants after PASS-1 V8.1 merged them into `Call { kind: Map | Host }`. The trio names ARCH §7.1 as authoritative, so the stale split leaks into Wave 9. | Collapse ARCH §7.1 variant and payload rows to `Call`; update BIR coverage examples that mention map/host origin to cite `kind`. | 3, 5, I |
| V9-5 | `restart/ARCHITECTURE.md:1041-1049`, `restart/ARCHITECTURE.md:1076`, `restart/MASTER-PLAN.md:823`, `restart/MASTER-PLAN.md:874` | Numeric diagnostic vocabulary retirement is incomplete in the trio. MASTER-PLAN says cookbook references already use human-readable forms, but the yaml LSP friction row still emits `BBNF-RECOVERY001`. | Replace `BBNF-RECOVERY001` with a mnemonic recovery code once I.W0 authors it, or with `BBNF-RECOVERY*` plus a note that I.W0 specializes the concrete code. | 7, F, H |
| V9-6 | `restart/ARCHITECTURE.md:1501`, `restart/MIGRATION.md:343-345`, `restart/MASTER-PLAN.md:303-310` | The V8.1 OpenFrame residue persists. ARCH §11 still names the perf owner row "OpenFrame clone absence" instead of the post-fold neutral wording; Migration and MASTER-PLAN already speak in clone-stack absence terms. | Rename the ARCH gate to `parallel-substrate-clone-absent` or "Parallel-substrate clone absence"; keep OpenFrame in explanatory archaeology only. | 1, 3, F |
| V9-7 | `restart/ARCHITECTURE.md:191-216`, `restart/ARCHITECTURE.md:1095-1097`, `restart/MASTER-PLAN.md:884` | ARCH §3.1 lacks the V8-P11 cross-host divergence note. The parse API is Rust-line V1 (`parse`, `parse_in`, `parse_owned`), while §7.5 defers WASM/TS backends; MASTER-PLAN says the ARCH-internal amendment should live in the synthesis commit, but the note is absent. | Add one paragraph after ARCH §3.1: V1 parse APIs are Rust-line surfaces; V2 `WasmBackend`/`TsBackend` may expose host-idiomatic allocation/GC forms without changing BIR. | J, K, 8 |
| V9-8 | `restart/HANDOFF.md:47-49`, `restart/HANDOFF.md:156-160`, `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.1.md:112-121` | HANDOFF current verdict is stale. It still says V8 SIMPLIFY-AVAILABLE, Phase 8.3.1 is current, and Phase 8.4/8.5 are pending; V8.1 says Phase 8 is closed and V9 dispatches at user direction. | Update HANDOFF §3 and phase table to `HARDENING-CONSOLIDATED-V8.1.md` / READY-WITH-NARROW-RESIDUE / V9 hardening active. | 3, 8, F |
| V9-9 | `restart/MASTER-PLAN.md:875`, `restart/ARCHITECTURE.md:739-745` | MASTER-PLAN §27 cites the cross-host carrier amendment as `ARCH §5:604-665`, but the actual sidecar note lives at ARCH lines 739-745. | Correct the ledger citation to `ARCH §5:739-745` or a section-only reference if line churn is expected. | H |

## §4 Punch list

| # | Target | Edit class | Owner |
|---:|---|---|---|
| 1 | ARCH + MASTER-PLAN + MIGRATION | Normalize TS/WASM deferral: V1 RustBackend only; `path-ts`, WASM ABI, WASM parity, and WASM lowerer rows become V2-only. | synthesis amendment |
| 2 | MASTER-PLAN + MIGRATION | Replace live 23-variant BIR mentions with the post-fold 20-variant / 19 semantic + `Return` wording. | synthesis amendment |
| 3 | ARCH | Fix the grammar sketch so `RuleDecl` is not counted as a directive. | synthesis amendment |
| 4 | ARCH | Collapse Grammar IR `Map` + `HostCall` to `Call { kind: Map | Host }` to match PASS-1 V8.1. | synthesis amendment |
| 5 | MASTER-PLAN | Retire `BBNF-RECOVERY001` from the friction row. | synthesis amendment |
| 6 | ARCH | Rename the OpenFrame perf gate row to neutral parallel-substrate clone wording. | synthesis amendment |
| 7 | ARCH | Add the parse API cross-host divergence note under §3.1. | synthesis amendment |
| 8 | HANDOFF | Update current verdict and Phase 8 table to V8.1/V9 reality. | handoff owner |
| 9 | MASTER-PLAN | Correct the §27 cross-host carrier citation. | synthesis amendment |

## §5 Final decision

**Decision: amendment-required-narrow.**

The trio is not a re-draft. Its core architecture still holds: tape/direct
union, two IRs, BIR-only lowerers, grammar-derived onboarding, human-readable
diagnostics, V1 Rust line, and V2 TS/WASM backends. The V9 faults are coherence
faults introduced by incomplete fold propagation: V1 TS/WASM residues, BIR count
drift, directive grammar drift, Grammar IR `Call` drift, diagnostic alias drift,
OpenFrame wording, parse cross-host note absence, HANDOFF status drift, and one
wrong-line citation.

Hereupon the next step is a narrow amendment pass over the cited rows, followed
by a V9.1 verification against the trio. Wave 9 per-tranche drafting should wait
for the punch list to close, because several rows directly govern tranche A/E/G/H/J
dispatch shape.
