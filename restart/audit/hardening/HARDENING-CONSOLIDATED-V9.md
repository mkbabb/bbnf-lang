# HARDENING-CONSOLIDATED-V9 — Codex V9 Hardening

V9 re-runs the full hardening lens set A-K after the V8.1 halt point. Four
independent target reports all returned AMENDMENT-REQUIRED-NARROW, not re-draft.
This consolidation records the cross-target synthesis, the narrow amendment set
applied to the live corpus, and the remaining V9.1 verification gate.

## §1 Target identifications

| Target | Audited surface | V9 report | Report evidence | Verdict |
|---|---|---|---|---|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` | `restart/audit/hardening/HARDENING-PASS-1-V9.md` | Target + verdict at report lines 7-14; punch list at lines 81-88. | AMENDMENT-REQUIRED-NARROW |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` | `restart/audit/hardening/HARDENING-PASS-2-V9.md` | Target + freshness fault at report lines 11-20; final decision at lines 155-168. | AMENDMENT-REQUIRED-NARROW |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` | `restart/audit/hardening/HARDENING-PASS-3-V9.md` | Target + four narrow findings at report lines 7-14; final decision at lines 55-59. | AMENDMENT-REQUIRED-NARROW |
| MASTER-PLAN trio | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | `restart/audit/hardening/HARDENING-MASTER-PLAN-V9.md` | Trio target table at report lines 9-17; final decision at lines 70-84. | AMENDMENT-REQUIRED-NARROW |

| Cohort | READY | AMENDMENT-REQUIRED-NARROW | RE-DRAFT | Cohort verdict |
|---|---:|---:|---:|---|
| Four-target V9 cohort | 0 of 4 | 4 of 4 | 0 of 4 | **AMENDMENT-REQUIRED-NARROW; amendments applied; V9.1 verification pending** |

## §2 Cohort synthesis

All four reports agree on the main architecture: tape/direct union, two-IR
boundary, BIR-only lowerers, grammar-derived onboarding, V1 Rust line, V2
TS/WASM backends, mnemonic diagnostics, and Lock 14 two-author-surface
onboarding. The V9 pressure is propagation drift after the Phase 8 fold.

The repeated residues collapse into seven coherence classes:

| Class | Found by | Live amendment disposition |
|---|---|---|
| V1 Rust-only vs V2 TS/WASM scope | PASS-1, PASS-2, MASTER-PLAN | PASS-1 / PASS-2 active WASM V1 rows now route to V2 `WasmBackend`; README, ARCH, MASTER-PLAN, and MIGRATION mark `path-ts`, TS production, WASM ABI, WASM parity, and WASM lower-and-bench as V2-only. |
| BIR count and spelling drift | PASS-2, MASTER-PLAN | Live BIR wording converges on 20 rows: 19 semantic variants plus `Return`; PASS-2's table is a payload-refiner map over ARCH §7.2, not a competing final alphabet. |
| Retired prompt and out-of-bounds citation drift | PASS-1, PASS-2, PASS-3 | PASS syntheses now cite live authorities or deletion archaeology, not retired dispatch prompts; stale `restart/README.md:473` and bad PASS-1 line ranges were removed. |
| Diagnostic alias split | PASS-3, MASTER-PLAN | PASS-3 user-facing ledger now uses ARCH §7.4 mnemonic codes; MASTER-PLAN uses `BBNF-RECOVERY*` for the I.W0-specialized recovery family. |
| Meta-grammar directive and Grammar IR drift | PASS-1, MASTER-PLAN | ARCH §8.1 now keeps `RuleDecl` outside `Directive`; ARCH §7.1 collapses `Map` + `HostCall` into `Call { kind: Map | Host }`. |
| Host-language and V1/V2 phrasing | PASS-1, PASS-3, MASTER-PLAN | Closure-capture diagnostics now route through semantic validation before emission and rustc as the final gate; PASS-3 closure broadening is a Lock 1 amendment surface, not generic V2 wording. |
| Handoff/orchestrator state drift | MASTER-PLAN | HANDOFF now names V9 as the current amendment-applied state and V9.1 as the next gate; ORCHESTRATOR names V9 as the active pre-Wave-9 hardening cycle. |

## §3 Cross-target conflict resolution

| Conflict | Resolution |
|---|---|
| PASS-1 / PASS-2 retained active WASM V1 obligations while locks and ARCH defer TS/WASM. | The live corpus now says V1 ships `RustBackend: Backend`; `WasmBackend` and `TsBackend` are V2 backend impls consuming the same BIR without grammar or alphabet change. |
| PASS-2 presented a BIR table that looked like a separate final alphabet. | PASS-2 now labels the table as payload refinement mapped to ARCH §7.2 and uses ARCH variant names, including `Return`. |
| ARCH directive grammar counted `RuleDecl` as a directive while PASS-1 and Lock 10 say six directives. | ARCH now models `Grammar ::= (Directive | RuleDecl)*`; `Directive` contains only the six directive forms. |
| ARCH Grammar IR split `Map` and `HostCall` after PASS-1 had merged them. | ARCH now carries one `Call { kind: Map | Host }` node with syntactic-origin discriminator. |
| PASS-3 produced numeric diagnostic aliases after ARCH retired them. | PASS-3 producer strings now use mnemonic identifiers; old numeric forms survive only as deletion archaeology in explicit ledgers. |
| HANDOFF still pointed at Phase 8 work as the current next move. | HANDOFF now points to V9.1 verification; Phase 8 is closed by V8.1. |

No re-draft threshold is met. The defects were narrow propagation faults across
already-settled contracts, not contradictions in the contracts themselves.

## §4 Punch list disposition

| # | Item | Source report | Disposition |
|---:|---|---|---|
| V9-A | Retire active WASM V1 / TS V1 obligations. | PASS-1 lines 38-49; PASS-2 lines 45-66; MASTER-PLAN lines 46, 60. | APPLIED across PASS-1, PASS-2, README, ARCH, MASTER-PLAN, MIGRATION, HANDOFF. |
| V9-B | Recast BIR as 20-row ARCH §7.2 alphabet and PASS-2 payload-refiner map. | PASS-2 lines 68-90; MASTER-PLAN lines 47, 61. | APPLIED across PASS-1, PASS-2, README, ARCH, MASTER-PLAN, MIGRATION. |
| V9-C | Remove retired prompt citations and wrong-line provenance. | PASS-1 lines 51-55; PASS-2 lines 92-104, 119-132; PASS-3 lines 42-43; MASTER-PLAN line 54. | APPLIED in PASS-1, PASS-2, PASS-3, HANDOFF, MASTER-PLAN. |
| V9-D | Move closure-capture diagnostic to semantic validation / rustc-final-gate wording. | PASS-1 lines 57-63; PASS-3 lines 40-41. | APPLIED in PASS-1 and PASS-3. |
| V9-E | Collapse PASS-3 diagnostic namespace to mnemonic codes. | PASS-3 lines 38-43; MASTER-PLAN line 50. | APPLIED in PASS-3 and MASTER-PLAN. |
| V9-F | Fix ARCH directive grammar and Grammar IR `Call` drift. | PASS-1 lines 65-69; MASTER-PLAN lines 48-49, 62-63. | APPLIED in ARCH; reflected in PASS-1. |
| V9-G | Rename OpenFrame clone perf gate and add parse cross-host note. | MASTER-PLAN lines 51-52, 65-66. | APPLIED in ARCH. |
| V9-H | Refresh HANDOFF / ORCHESTRATOR state. | MASTER-PLAN lines 53, 67. | APPLIED in HANDOFF and ORCHESTRATOR. |

## §5 Current verdict

**AMENDMENT-REQUIRED-NARROW, amendment set applied.**

The V9 cohort did not authorize Wave 9 directly. The correct next gate is a
V9.1 verification rerun against the amended corpus. If V9.1 returns READY, Wave
9 per-tranche full-spec drafting can dispatch. If V9.1 finds only fresh narrow
residue, the residue must be consolidated before Wave 9.

The remaining known carry is verification-class, not a fresh architecture
blocker:

| Carry | Receiver | Gate |
|---|---|---|
| Verify the V9 amendment set against all four hardener reports. | V9.1 hardening cohort. | `HARDENING-CONSOLIDATED-V9.1.md` returns READY or documents a smaller residue. |
| `BBNF-PATTERN-NONEXHAUSTIVE` remains a tranche-D friction-class diagnostic specialization. | Tranche D / diagnostic cookbook work. | Tranche D diagnostic catalogue and cookbook close. |

## §6 Closing posture

V9 was valuable because it re-found the V8.1 residues and exposed incomplete
fold propagation around the same architectural seams: backend scope, BIR
cardinality, diagnostic naming, directive grammar, Grammar IR callable shape,
and stale current-state prose. The corpus now speaks the V1 Rust / V2 backend
boundary consistently enough to verify.

Hereupon: dispatch V9.1 verification before Wave 9.
