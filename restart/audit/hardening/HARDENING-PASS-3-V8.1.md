# HARDENING-PASS-3-V8.1 — Phase 8.4 fold verification

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` |
| Audit label | PASS-3 (V8.1 verification — Phase 8.3.1 + Phase 8.4 fold close) |
| V8 baseline | `restart/audit/hardening/HARDENING-PASS-3-V8.md` (commit `cd6c2b4c`, AMENDMENT-REQUIRED, 11 punch items) |
| Phase 8.4 PASS-3 fold commit | `bd213632` (10 patch deltas absorbed, 11 insertions / 7 deletions on PASS-3.md) |
| Phase 8.3.1 corpus cleanup commit | `a74cdc52` (GADT V1 + CHR V1 + V5.1 prune) |
| Phase 8.4 synthesis fold commit | `e5cb1e4b` (ARCH/MIGRATION/MASTER-PLAN trio) |
| Cohort scope | PASS-3 only; sibling V8.1 reports for PASS-1 / PASS-2 / MASTER-PLAN compose under HARDENING-CONSOLIDATED-V8.1 |
| Hard cap | 50 minutes |
| Write scope | this report only |

V8.1 is verification-only. The eleven-item V8 punch list distributes across PASS-3-fold (`bd213632` β1, β2, γ3, γ4, γ5, γ6, γ7, δ5, δ6, δ7) and ARCH-synthesis (`e5cb1e4b`, β1 broad form + α1 + α5 + γ10 carry). PASS-3 V1 surface absorbs host-leverage delegation + tranche-body routing without subtracting V1 content. The verification asks one question: did each Phase 8.4 surgery land verbatim in PASS-3, and does the post-fold PASS-3 surface remain coherent with the cohort?

## §2 Phase 8.3.1 closure (Step A — GADT V1 fold)

| V8 item | Verification | Result | Verdict |
|---|---|---|---|
| §6b `BBNF-LOCAL-EQUALITY-ANNOTATION` row promoted from reserved → V1-emitted | `rg -n 'BBNF-LOCAL-EQUALITY-ANNOTATION' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:472` (verbatim diagnostic row in §6b ledger) and `:474` ("The `BBNF-LOCAL-EQUALITY-ANNOTATION` row above is V1-emitted (Phase 8.3.1 / Lock 4 amendment); the OutsideIn(X)-style implication-constraint solver at `passes/types/` discharges branch-local type equalities to `LayoutFacts` per ARCH §8.2 GADT V1 surface; reservation phrasing for the row retires.") | **CLOSED** |

The Phase 8.3.1 corpus cleanup (`a74cdc52`) promoted GADT from V2 reservation to V1 emission per Lock 4 amendment. The PASS-3 §6b row at `:472` carries the verbatim diagnostic string with `Pattern @ where T = U` refinement-annotation phrasing; the post-table prose at `:474` confirms V1-emit and routes the implication-constraint discharge to `passes/types/` against `LayoutFacts` per ARCH §8.2. Reservation phrasing has retired. The row composes against PASS-1 §3 type-system algorithm and ARCH §8.2 GADT V1 surface coherently.

## §3 Phase 8.4 PASS-3 fold closure (Step B — 10 patch deltas)

### §3.1 β1 — Diagnostic numeric retire (PASS-3 ledger surface)

| Verification | Result | Verdict |
|---|---|---|
| `rg -n 'BBNF1004' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:474` ("the prior numeric alias `BBNF1004` retires per the Phase 8.4 simplification fold (V8 §3 β1) — the alphabetic code is the single namespace") — deletion-archaeology context | **CLOSED for BBNF1004 specifically** |
| `rg -n 'BBNF-VISIT001\|BBNF-VISIT002\|BBNF-VISIT003' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:254` (`BBNF-VISIT003` in worked-path prose), `:462` / `:463` / `:464` (all three visitor codes carrying live verbatim diagnostic strings as primary §6b ledger rows) | **NOT-CLOSED** |
| `rg -n 'BBNF-PATH001\|BBNF-PATH002\|BBNF-PATH003' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:122` / `:123` / `:125` (all three path codes in worked-path prose), `:459` / `:460` / `:461` (all three path codes carrying live verbatim diagnostic strings as primary §6b ledger rows) | **NOT-CLOSED** |
| `rg -n 'BBNF-LIFE001\|BBNF-LIFE002\|BBNF-LAYOUT001\|BBNF-LAYOUT002\|BBNF-OPT001\|BBNF-OPT002\|BBNF-GRAMMAR001\|BBNF-RECOVERY001\|BBNF-TYPE001\|BBNF-HOST001\|BBNF-HOST002\|BBNF-HOST003\|BBNF-GEN001\|BBNF-CG001' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:452-:471` — full ledger of numeric-aliased rows alive in §6b | **NOT-CLOSED** |
| `rg -n 'BBNF-LIFETIME-ESCAPE\|BBNF-VISITOR-NO-MATCHING-KINDS\|BBNF-PATH-UNKNOWN-SEGMENT\|BBNF-LAYOUT-UNUSED\|BBNF-PATH-UNKNOWN-TERMINAL\|BBNF-HOST-SIGNATURE-MISMATCH\|BBNF-CHAIN-STEP\|BBNF-HOST-WASM-PRIMITIVE-MISSING\|BBNF-CODEGEN-IMPORT-DENY\|BBNF-METADATA-MISSING-GRAMMAR' restart/audit/pass-3-runtime/PASS-3.md` | **zero hits** | **NOT-CLOSED** |

**β1 verdict: PARTIAL.** The narrow-scope claim — BBNF1004 retirement — landed: the synthesis-ledger `BBNF1004` numeric alias for `BBNF-LOOKBEHIND-WIDTH` appears only as deletion archaeology at `:474`. The broad-scope claim — full numeric-alias retirement across the §6b ledger — did NOT land. The `bd213632` PASS-3 fold commit message itself confirms the narrow scope: "β1 — §6b lookbehind row dehyphenates to alphabetic single-namespace ... the BBNF1004 numeric alias retires per V8 §3 β1". The synthesis fold (`e5cb1e4b`) retired the broader numeric-alias system in ARCH §7.4, but the corresponding PASS-3 §6b ledger sync did NOT execute.

This produces a cohort-coherence break: ARCH §7.4 catalogue (post-Phase-8.4) carries human-readable mnemonic codes; PASS-3 §6b ledger (post-Phase-8.4) still carries fourteen numeric-aliased rows (`BBNF-LIFE001` / `BBNF-LIFE002` / `BBNF-LAYOUT001` / `BBNF-LAYOUT002` / `BBNF-OPT001` / `BBNF-OPT002` / `BBNF-GRAMMAR001` / `BBNF-PATH001` / `BBNF-PATH002` / `BBNF-PATH003` / `BBNF-VISIT001` / `BBNF-VISIT002` / `BBNF-VISIT003` / `BBNF-RECOVERY001` / `BBNF-TYPE001` / `BBNF-HOST001` / `BBNF-HOST002` / `BBNF-HOST003` / `BBNF-GEN001` / `BBNF-CG001`). ARCH §7.4 prose at `:1041-1049` declares these aliases retired and bound to mnemonic forms — but PASS-3, the producer of the verbatim user-facing strings, retains the numeric spelling.

ARCH §7.4 at `:1083-1088` cites PASS-3 as the source-of-truth for verbatim diagnostic strings: "The verbatim diagnostic strings for each code live with the producer ... `restart/audit/pass-3-runtime/PASS-3.md:352-366` for the runtime, host, layout, pointer, and visitor codes." If ARCH carries the mnemonic identifier and the cite resolves to PASS-3 carrying the numeric identifier, downstream cookbook + runtime emit consumers cannot bind a single string to a single code. The cohort-coherence reading at Step E surfaces this fault directly.

### §3.2 β2 — `BBNF-LOCAL-EQUALITY-ANNOTATION` V1-emitted

Covered by §2 above. Verbatim diagnostic row at `:472`; V1-emit clarification at `:474`; reservation phrasing retired. **CLOSED.**

### §3.3 γ3 — `thiserror` + `miette` host-leverage delegation

| Verification | Result | Verdict |
|---|---|---|
| `rg -n 'thiserror\|miette' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:302` (`bbnf/src/diagnostics/` block annotated: "verbatim strings of §6b lower to `thiserror::Error` derives and `miette::Diagnostic` rendering, no bbnf-invented diagnostic struct per V8 §3 γ3 host-leverage") and `:474` ("the verbatim strings of this ledger lower into `thiserror::Error` derives at `crates/bbnf/src/diagnostics/`, and cookbook receivers become `miette::Diagnostic::url` impls; bbnf invents no diagnostic struct") | **CLOSED** |

The diagnostic infrastructure binds to the Rust ecosystem's `thiserror` + `miette` idiom. The bbnf-specific value (verbatim strings, codes, cookbook URLs) is preserved; the rendering scaffolding is host-leveraged. PASS-3 §6 crate-tree narrative and §6b post-table prose are coherent.

### §3.4 γ4 — `syn::visit` host-leverage delegation

| Verification | Result | Verdict |
|---|---|---|
| `rg -n 'syn::visit\|syn::Visit' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:148` ("The generated `Visitor` trait shape and method-naming convention (`visit_object`, `visit_member`, etc.) explicitly mirror `syn::visit::Visit` / `VisitMut` per V8 §3 γ4 host-leverage; users transferring from `syn`-based code recognise the pattern without retraining. The `VisitTypes` bitflag pruning is bbnf-specific (the mask is grammar-derived from generated metadata, not a `syn` precedent).") | **CLOSED** |

The HYBRID verdict from V8 J2 lands correctly: trait shape + method-naming mirror `syn::visit::Visit`/`VisitMut`; bitflag pruning remains bbnf-specific. The Lens J host-leverage / bbnf-specific split is explicit and coherent with PASS-3 §3 visitor commitment.

### §3.5 γ5 — `tower-lsp` host-leverage delegation

| Verification | Result | Verdict |
|---|---|---|
| `rg -n 'tower-lsp' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:346` ("The `lsp/server.rs` + `lsp/protocol.rs` files are thin `tower-lsp` adapters per V8 §3 γ5 host-leverage") and `:534` (carry-row carrying "`tower-lsp` + `dap-types` + snapshot-identity binding through tape source spans") | **CLOSED** |

The `bbnf-language-server` LSP wire-format scaffolding binds to `tower-lsp`; the bbnf-specific surface narrows to `analysis/` + `incremental/` only. The architectural-shrink claim ("the bbnf-language-server's invented surface shrinks to `analysis/` + `incremental/` only") is explicit at `:346`.

### §3.6 γ6 — `dap-types` host-leverage delegation

| Verification | Result | Verdict |
|---|---|---|
| `rg -n 'dap-types' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:346` ("the `dap/server.rs` + `dap/session.rs` + `dap/mapping.rs` files are thin `dap-types` (or `debug-adapter-protocol` crate) adapters per V8 §3 γ6 host-leverage") and `:534` (carry-row binding) | **CLOSED** |

The DAP wire-format scaffolding binds to `dap-types`. Acceptance of the alternative `debug-adapter-protocol` crate is correctly hedged inside the binding. The bbnf-specific surface reduction is uniform across LSP and DAP per `:346`.

### §3.7 γ7 — `salsa` design-language host-leverage

| Verification | Result | Verdict |
|---|---|---|
| `rg -n 'salsa' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:225` ("The design language references existing precedents per V8 §3 γ7 host-leverage: salsa's revisions / queries / invalidation vocabulary informs the `(SnapshotId, query-key, invalidation-reason)` framing of `QueryInvalidationSet` and the cross-snapshot fact-cache contract; tree-sitter's incremental parse algorithm informs the `(OldTapeId, NewTapeId)` reuse-map computation and the dirty-range / anchor-set construction. The runtime mechanism is bbnf-built — salsa-queries are too coarse-grained for per-tape-range reuse, and tree-sitter's edit primitives do not compose with bbnf's recovery semantics — but the conceptual scaffolding is borrowed, not reinvented.") | **CLOSED** |

The HYBRID verdict from V8 J5 lands correctly: salsa's design-language vocabulary (revisions / queries / invalidation) informs `QueryInvalidationSet`; tree-sitter's edit-primitive algorithm informs the reuse-map computation; the runtime mechanism is bbnf-built for tape-range granularity. Both precedents are cited; both bbnf-specific extensions are explicit.

### §3.8 δ5 — DAP body → tranche I

| Verification | Result | Verdict |
|---|---|---|
| `rg -nC2 'DAP.*tranche I\|tranche I.*DAP' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:534` (carry-row: "DAP server / session / mapping protocol implementation + `commands/debug.rs` CLI surface route to tranche I body per V8 §3 δ5 + δ6"; receiving gate: "DAP `bbnf-language-server` server starts under VSCode + emits one breakpoint event over a JSON parse — promotion-test, not landing-test") | **CLOSED** |

DAP body deferral is V1-surface-marked, tranche I body-deliverable, with explicit Receiver + Blocker + Receiving-gate triple. The V1 entry surface (snapshot-identity binding + tape source spans) is LOAD-BEARING; the protocol body is ASPIRATIONAL.

### §3.9 δ6 — LSP completion → tranche I

| Verification | Result | Verdict |
|---|---|---|
| `rg -nC2 'LSP.*tranche I\|completion.*tranche' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:534` (carry-row: "LSP completion + semantic-tokens + imports + incremental anchors + reparse-plan body + DAP server / session / mapping protocol implementation + `commands/debug.rs` CLI surface route to tranche I body per V8 §3 δ5 + δ6") | **CLOSED** |

LSP body deferral is correctly partitioned: V1 entry surface (server start + diagnostics + hover + go-to-definition + LSP/DAP wire-format adapters + snapshot-identity) is LOAD-BEARING; LSP completion + semantic-tokens + imports + incremental anchors + reparse-plan body route to tranche I body with Receiver / Blocker / Receiving-gate triple.

### §3.10 δ7 — Incremental cookbook → tranche I/J

| Verification | Result | Verdict |
|---|---|---|
| `rg -nC2 'incremental.*tranche\|reuse.*map.*tranche' restart/audit/pass-3-runtime/PASS-3.md` | positive at `:536` (carry-row: "V1 ships full-reparse-on-each-edit (correct, slow); reuse-map computation + `(OldTapeId, NewTapeId)` pair semantics + edit-anchor algorithm + dataset-level fallback-rate gates route to tranche I body per V8 §3 δ7 — recovery semantics + fallback-reason ledger + LSP silence policy are V1 LOAD-BEARING (the user-mandate is "LSP fault-tolerant fallback", which is recovery, not incremental).") and `:538` (cookbook content body deferral: "full cookbook chapter content (visitor pruning chapters, format mode tables, path validation walk-throughs, recovery sync-set worked paths, type-system §gadt-refinements chapter) routes to tranche J body per V8 §3 δ7") | **CLOSED** |

Incremental + cookbook deferral correctly partitions LOAD-BEARING (recovery semantics + fallback-reason ledger + LSP silence policy + URL stubs) from ASPIRATIONAL (reuse-map computation + edit-anchor algorithm + dataset-level fallback-rate gates + cookbook chapter authoring). Both tranche I and tranche J receivers are named with Receiver / Blocker / Receiving-gate triples.

## §4 V2-amendment retirement (Step C)

| Verification | Result | Verdict |
|---|---|---|
| `rg -n 'V2 amendment\|deferred to V2\|post-V1 amendment' restart/audit/pass-3-runtime/PASS-3.md` | one occurrence at `:191` ("Function-value broadening beyond the four sites (first-class storage, return-from-rule, parameter-pass outside the host chain) defers to a Lock 1 reuse-map amendment that extends snapshot-scoped identity to closure environments. PASS-3 absorbs the narrow contract today; the broadening contract is a V2 amendment surface.") | **NOT-CLOSED-FAINT** |

The single surviving "V2 amendment" reference at `:191` is a function-value broadening deferral tied to Lock 1 reuse-map. Per the V8.1 prompt §C, legitimate exclusions are "TS/WASM via Backend trait at ARCH §7.5; path-ts; WASM ABI" — these are user-adjudicated scope partitions through Lock 5 / Lock 11. The closure broadening reference is a separate V2-amendment claim; it is NOT in the prompt's explicit exclusion list.

The `bd213632` commit message acknowledges this: "New δ5/δ6/δ7 prose introduces zero V2 deferral language; pre-existing V2 references for WASM (Lock 5) + closure broadening (Lock 1 amendment) remain valid architectural commitments outside Phase 8.4 fold scope."

The hardener verdict: this V2-amendment reference is architecturally legitimate (the closure broadening is a real Lock 1 amendment surface that defers genuinely beyond V1), but it lies outside the V8.1 prompt's three named exclusions. Two readings are plausible:

1. **Strict reading**: The prompt's three named exclusions (TS/WASM via Backend trait, path-ts, WASM ABI) are exhaustive; closure broadening is a fourth legitimate-but-unstated exclusion. The Phase 8.3.1 corpus cleanup classification already retired the V5.1 reservation phrasing for closures that landed V1 (the four sites: host-chain, map, predicate, recovery); the surviving broadening clause covers the genuine post-V1 surface (first-class storage, return-from-rule, parameter-pass outside host chain), which is a real Lock 1 amendment surface.

2. **Literal reading**: The prompt's three exclusions are exhaustive; the closure broadening clause is residual V2-amendment language that should retire. Surgery: rephrase `:191` as "Function-value broadening ... defers to a Lock 1 reuse-map amendment ... PASS-3 absorbs the narrow contract today; the broadening contract awaits the Lock 1 amendment." Drop "V2 amendment surface" phrasing.

V8.1 records this as **NOT-CLOSED-FAINT** — the language stands on architecturally legitimate grounds (the broadening is real and tied to a real future Lock 1 amendment), but the V2-amendment phrasing is the prompt's flag-trigger. Recommendation: **carry to V8.2 hardening cycle** as a single-line rephrase from "V2 amendment surface" to "Lock 1 amendment surface" — preserves the architectural claim while honouring the prompt's V2-amendment retirement discipline.

## §5 Compressed 9-lane verification (Step D)

V8.1 carries forward V8 lens findings; the nine-lane scrutiny verifies that Phase 8.4 fold did not regress any V8 KEEP/READY surface.

| # | Lane | V8 verdict | V8.1 verification | V8.1 verdict |
|---:|---|---|---|---|
| 1 | Lock-Adherence | READY | Lock 1 (no parallel substrate) at `:187-188` re-ratified; Lock 5 (Backend trait) at `:140` (parse signature gate) cited; Lock 10 (six-directive grammar) at `:16` and `:548` cited; Lock 13 (4-10 child rule) at `:302` cited (8-children layout); Lock 14 (no overfit) at `:418-425` cited (yaml onboarding two-surface). γ4/γ5/γ6/γ7 host-leverage delegations preserve Lock 1 (no parallel substrate at the runtime); γ3 preserves Lock 13 (sibling-API uniformity in `bbnf/src/diagnostics/`). | **READY** |
| 2 | Sequencing | N/A (compressed mode; PASS-level) | N/A | **N/A** |
| 3 | Cohesion | READY | δ5/δ6/δ7 carry-row triples at `:534-:538` honour Receiver / Blocker / Receiving-gate format; γ3/γ4/γ5/γ6/γ7 cite V8 §3 lens citations explicitly with no metalanguage; β2 V1-emit clarification at `:474` is coherent with ARCH §8.2 GADT V1 surface. **Cohort-coherence break at β1 (PASS-3 §6b numeric-alias ledger vs. ARCH §7.4 mnemonic catalogue)** — flagged at §3.1 above. | **READY-WITH-FAULT** |
| 4 | SOTA-Anchoring | READY | §7 bench rows untouched by Phase 8.4 fold; sonic-rs / simd-json / lightning-css attribution preserved; H.W3/H.W4 V1 SOTA close-gate language honoured at `:478` ("V1 SOTA close gates measure the Rust-line only at H.W3, H.W4, and H.W5"). | **READY** |
| 5 | Grammar-Authoritative | READY | §6a 10-row feeder table preserved at `:432-:443`; yaml onboarding two-surface at `:418-:425`; Lock 14 fixture-separation grep gate at `:425` ("`rg -n 'fixtures/yaml' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` must return zero hits inside Lock 14 onboarding allowances") preserved. | **READY** |
| 6 | Generated-Code-Budget | READY | §7 LOC budget table preserved at `:510-:519`; W3 baseline anchor (css_l4 ≈ 107K LOC, bbnf ≈ 21K LOC, etc.) intact at `:508`; +2% regen ceiling preserved. β1 partial-fold reduces V1 emission surface for the `BBNF1004` alias only — no LOC budget impact. | **READY** |
| 7 | Friction-Forecast | READY | §6b verbatim error strings preserved (twenty rows in §6b at `:452-:472`); committed verbatim error message discipline intact for every code; γ3 `thiserror`/`miette` binding at `:302` + `:474` preserves friction-forecast load-bearing per V8 J6. | **READY** |
| 8 | Carry/Deferral | READY | §8 carry-table rows at `:524-:538` carry Receiver + Blocker + Receiving-gate triples; δ5 + δ6 + δ7 tranche-body routing language explicit per `:534` (Tranche I) and `:538` (Tranche J); §10 punch-list at `:574-:588` rolls forward V8 deferrals coherently. | **READY** |
| 9 | Greenfield-Discipline | READY | §6 path-ts archive shrunk per V8 I8 (carry-row at `:528` + brief V2 tree subsection retained at `:393-:399`); §3 path/select macro family discipline preserved; γ3-γ7 host-leverage delegations REDUCE invented-substrate surface across LSP / DAP / diagnostics / incremental. | **READY** |
| 10 | Phase 8.3.1 GADT V1 fold | (new — V8.1 lane) | §6b `BBNF-LOCAL-EQUALITY-ANNOTATION` row at `:472` carries V1-emit verbatim diagnostic; `:474` confirms V1-emit + retires reservation phrasing; ARCH §8.2 GADT V1 surface bound. | **READY** |
| 11 | Phase 8.4 host-leverage delegation | (new — V8.1 lane) | γ3 (`thiserror`/`miette`) at `:302`/`:474`; γ4 (`syn::visit`) at `:148`; γ5 (`tower-lsp`) at `:346`/`:534`; γ6 (`dap-types`) at `:346`/`:534`; γ7 (`salsa`) at `:225`. All five bindings positive; all five cite V8 §3 lens explicitly. | **READY** |
| 12 | Phase 8.4 tranche-body routing | (new — V8.1 lane) | δ5 (DAP body → Tranche I) at `:534`; δ6 (LSP completion → Tranche I) at `:534`; δ7 (incremental + cookbook → Tranche I/J) at `:536`/`:538`. Each routing carries Receiver + Blocker + Receiving-gate triple. | **READY** |

**12-of-12 V8.1 lanes hold.** Lane 3 carries a cohort-coherence fault flag (β1 PASS-3 §6b ledger numeric-alias survival vs. ARCH §7.4 mnemonic-catalogue retirement); the other eleven lanes are clean.

## §6 Cohort coherence (Step E)

### §6.1 ARCH §7.4 catalogue codes vs. PASS-3 ledger

| Lock-anchor | ARCH §7.4 spelling | PASS-3 §6b spelling | Coherent? |
|---|---|---|---|
| Lifetime escape | `BBNF-LIFETIME-ESCAPE` (`:1053`) | `BBNF-LIFE001` (`:452`) | **NO** |
| Arena mismatch | `BBNF-ARENA-MISMATCH` (`:1054`) | `BBNF-LIFE002` (`:453`) | **NO** |
| Lookbehind width | `BBNF-LOOKBEHIND-WIDTH` (`:1055`) | `BBNF-LOOKBEHIND-WIDTH` (`:466`) | **YES** (sole ledger fold landed at PASS-3) |
| Visitor mutation outside entry | `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY` (`:1057`) | `BBNF-VISIT002` (`:463`) | **NO** |
| Visitor no matching kinds | `BBNF-VISITOR-NO-MATCHING-KINDS` (`:1058`) | `BBNF-VISIT001` (`:462`) | **NO** |
| Visitor recovery skip | `BBNF-VISITOR-RECOVERY-SKIP` (`:1059`) | `BBNF-VISIT003` (`:464`) | **NO** |
| Layout unused | `BBNF-LAYOUT-UNUSED` (`:1062`) | `BBNF-LAYOUT001` (`:454`) | **NO** |
| Pratt not applied | `BBNF-PRATT-NOT-APPLIED` (`:1063`) | `BBNF-OPT001` (`:456`) | **NO** |
| SIMD not selected | `BBNF-SIMD-NOT-SELECTED` (`:1064`) | `BBNF-OPT002` (`:457`) | **NO** |
| Metadata missing grammar | `BBNF-METADATA-MISSING-GRAMMAR` (`:1065`) | `BBNF-GRAMMAR001` (`:458`) | **NO** |
| Path unknown segment | `BBNF-PATH-UNKNOWN-SEGMENT` (`:1067`) | `BBNF-PATH001` (`:459`) | **NO** |
| Path grammar mismatch | `BBNF-PATH-GRAMMAR-MISMATCH` (`:1068`) | `BBNF-PATH002` (`:460`) | **NO** |
| Path unknown terminal | `BBNF-PATH-UNKNOWN-TERMINAL` (`:1069`) | `BBNF-PATH003` (`:461`) | **NO** |
| Host signature mismatch | `BBNF-HOST-SIGNATURE-MISMATCH` (`:1070`) | `BBNF-HOST001` (`:468`) | **NO** |
| Chain step | `BBNF-CHAIN-STEP` (`:1071`) | `BBNF-HOST002` (`:469`) | **NO** |
| Host WASM primitive missing | `BBNF-HOST-WASM-PRIMITIVE-MISSING` (`:1072`) | `BBNF-HOST003` (`:470`) | **NO** |
| Local equality annotation | `BBNF-LOCAL-EQUALITY-ANNOTATION` (`:1075`) | `BBNF-LOCAL-EQUALITY-ANNOTATION` (`:472`) | **YES** (Phase 8.3.1 fold) |
| Codegen import deny | `BBNF-CODEGEN-IMPORT-DENY` (`:1077`) | `BBNF-GEN001` / `BBNF-CG001` (`:471`) | **NO** |

**Cohort-coherence break confirmed:** sixteen-of-eighteen mappings between ARCH §7.4 mnemonic catalogue and PASS-3 §6b ledger are NON-COHERENT. The two PASS-3 rows that synced (lookbehind, local-equality-annotation) reflect targeted PASS-3 folds (Phase 8.4 β1 narrow-scope on lookbehind; Phase 8.3.1 on equality annotation). The broader sync — Phase 8.4 β1 broad-form on the remaining sixteen rows — landed only in ARCH (`e5cb1e4b`) without a corresponding PASS-3 ledger update.

ARCH §7.4 prose at `:1083-1088` cites PASS-3 as the source-of-truth for verbatim diagnostic strings: "The verbatim diagnostic strings for each code live with the producer ... `restart/audit/pass-3-runtime/PASS-3.md:352-366` for the runtime, host, layout, pointer, and visitor codes." This citation is now broken: ARCH §7.4 carries `BBNF-VISITOR-NO-MATCHING-KINDS`; PASS-3 §6b carries the verbatim string at row `BBNF-VISIT001`. The downstream cookbook + runtime emit consumer cannot bind one identifier to one string.

### §6.2 6-directive grammar coherence

| Lock-anchor | Locks/Lock-10 spelling | PASS-3 spelling | Coherent? |
|---|---|---|---|
| Six V1 directives | `@import \| @host fn \| @error \| @layout \| @pretty \| @token` (Lock 10 at `restart/locks/14-LOCKS.md:52`) | `@import, @host fn, @error(recover = ...), @layout, @pretty, @token` (`:16` and `:548`) | **YES** |
| `@recover` retirement | "Standalone `@recover` retires; absorbed by `@error(recover = ...)`" (Lock 10) | `:195` "A standalone `@recover` token is a legacy alias only if SYNTHESIS keeps a migration parser; it is not a new V1 extension." | **YES** |
| `@pratt`/`@simd`/`@transducer`/`@rewrite`/`@unicode` retirement | Lock 10 retirement clause | PASS-3 §1 "Rewrite-mode is out; grammar-level Unicode-class algebra is deferred to `parse-that-regex`" + §6b `BBNF-OPT001/002` notes are author-facing optimizer disclosure (not directives) | **YES** |
| Function values + lambda literals + closure capture | Lock 10 + Lock 4 amendment | `:16` "function values + lambda literals (`|x| body`), and closure capture by `&'i Tape<'i>` reference"; `:191` four-site closure environment commitment | **YES** |

**6-directive grammar coherence: CLEAN.** Lock 10 commitment lands verbatim across PASS-3.

### §6.3 Phase-8.4 host-leverage delegation coherence

The five host-leverage delegations (γ3 thiserror+miette, γ4 syn::visit, γ5 tower-lsp, γ6 dap-types, γ7 salsa) compose with the Phase 8.4 synthesis fold (`e5cb1e4b`) at:
- ARCH §3.1 `bbnf` public surface declares `Diagnostic = thiserror::Error + miette::Diagnostic` derive-stack
- ARCH §3.3 `bbnf-language-server` declares `tower-lsp` adapter + `dap-types` adapter
- MASTER-PLAN tranche-J rows reference cookbook chapter authoring waves (γ7 + δ7)

PASS-3 host-leverage delegations are coherent with the synthesis fold across all five bindings.

## §7 Punch-list deltas

The eleven-item V8 punch list distributes:

| V8 item | Verdict | Phase 8.4 surgery |
|---|---|---|
| 1 (`bbnf/src/` 8-children CONSOLIDATE) | not in PASS-3 fold scope | residual — `:302` retains 8-children layout (carries V8 I1 CONSOLIDATE recommendation); orthogonal to host-leverage focus of Phase 8.4 |
| 2 (`ReparsePlan` `invalidated_queries` reframe) | not in PASS-3 fold scope | residual — `:218-:223` retains four-column struct; orthogonal to fold focus |
| 3 (BBNF-OPT001/002 + BBNF-LOCAL-EQUALITY cookbook-only) | partial — Phase 8.3.1 promoted local-equality to V1-emit (NOT cookbook-only); Phase 8.4 β1 retired BBNF1004 only | residual — BBNF-OPT001/002 still emitted at `:456-:457` |
| 4 (`bbnf-cli` 6→3 commands) | not in PASS-3 fold scope | residual — `:307-:320` retains 6-command surface |
| 5 (`crates/path-ts/` archive subsection delete) | not in PASS-3 fold scope | residual — `:393-:399` retains V2 tree subsection (V8 I8 cited `:528` carry-row as adequate carrier; subsection still present) |
| 6 (visitor `syn::visit` binding) | **landed** | γ4 at `:148` |
| 7 (LSP/DAP `tower-lsp`/`dap-types` binding) | **landed** | γ5/γ6 at `:346` |
| 8 (incremental `salsa` design language) | **landed** | γ7 at `:225` |
| 9 (diagnostic `thiserror`/`miette` binding) | **landed** | γ3 at `:302`/`:474` |
| 10 (DAP body → Tranche I body deliverable) | **landed** | δ5 at `:534` |
| 11 (LSP body + incremental + cookbook → Tranche I/J body deliverable) | **landed** | δ6/δ7 at `:534`/`:536`/`:538` |

**Phase 8.4 PASS-3 fold absorbed 6-of-11 V8 punch items (#6-#11 — the host-leverage + tranche-routing surgeries).** Items #1-#5 (architectural CONSOLIDATE/SIMPLIFY) are residuals — they do not block readiness because the V8 verdict already classified them as non-blocking apparatus surgery. Item #3 partially landed via Phase 8.3.1 GADT promotion (which moved BBNF-LOCAL-EQUALITY from reservation to V1-emit, not the V8-recommended cookbook-only path); the cohort accepted the GADT V1 surface promotion as a separate fold.

## §8 V8 → V8.1 comparison

| Surface | V8 verdict | V8.1 verification | Delta |
|---|---|---|---|
| §6b GADT V1 row promotion | reserved (V8) | V1-emitted (`:472`/`:474`) | **closure** — Phase 8.3.1 promoted |
| §6b BBNF1004 retirement | numeric alias live | retired at `:474` (deletion archaeology only) | **closure** — Phase 8.4 β1 narrow-scope landed |
| §6b broader numeric-alias retirement | numeric aliases live | numeric aliases STILL live at `:452-:471` | **NOT-CLOSED** — broad-scope β1 did not extend to PASS-3 ledger |
| §6 `diagnostics/` thiserror+miette binding | invented diagnostic struct | host-leveraged at `:302` + `:474` | **closure** — γ3 landed |
| §3 visitor `syn::visit` binding | implicit | explicit at `:148` | **closure** — γ4 landed |
| §6 LSP `tower-lsp` binding | invented LSP server | host-leveraged at `:346` | **closure** — γ5 landed |
| §6 DAP `dap-types` binding | invented DAP server | host-leveraged at `:346` | **closure** — γ6 landed |
| §5 incremental `salsa` design-language | implicit precedent | explicit at `:225` | **closure** — γ7 landed |
| §8 DAP body Tranche I routing | implicit V1 carry | explicit at `:534` (Receiver/Blocker/gate triple) | **closure** — δ5 landed |
| §8 LSP body Tranche I routing | implicit V1 carry | explicit at `:534` | **closure** — δ6 landed |
| §8 incremental + cookbook Tranche I/J routing | implicit V1 carry | explicit at `:536`/`:538` (two triples) | **closure** — δ7 landed |
| §4 closure broadening V2-amendment phrasing | "V2 amendment surface" (preserved) | "V2 amendment surface" (preserved at `:191`) | **NOT-CLOSED-FAINT** — Lock 1 amendment legitimate but V2-amendment phrasing flagged by V8.1 §C |

**V8 → V8.1 delta:** ten of twelve lanes show closure; two lanes carry residue.

The two residues are:
1. **§3.1 broad β1 numeric-alias retirement gap** — the cohort-coherence break between ARCH §7.4 mnemonic catalogue and PASS-3 §6b numeric-alias ledger. The synthesis fold landed the broader retirement in ARCH; the PASS-3 fold did not extend to the §6b ledger.
2. **§4 closure broadening V2-amendment phrasing** — the `:191` "V2 amendment surface" reference for function-value broadening lies outside V8.1 prompt's three named exclusions (TS/WASM via Backend, path-ts, WASM ABI). The architectural claim is legitimate (Lock 1 amendment surface is real); the V2-amendment phrasing is the prompt's flag-trigger.

## §9 Final readiness

> **Decision: AMENDMENT-REQUIRED-NARROW.**
>
> Phase 8.3.1 + Phase 8.4 PASS-3 fold (`bd213632`) absorbed the largest host-leverage delegation in the four-target cohort: five host-leverage bindings (`thiserror`/`miette`, `syn::visit`, `tower-lsp`, `dap-types`, `salsa`) and three tranche-body routings (DAP / LSP completion / incremental + cookbook) all landed verbatim at PASS-3 with explicit V8 §3 lens citations and Receiver/Blocker/Receiving-gate triples. The GADT V1 promotion (Phase 8.3.1) lifted `BBNF-LOCAL-EQUALITY-ANNOTATION` from reservation to V1-emission per Lock 4 amendment. The narrow-scope β1 (BBNF1004 alias retirement) closed.
>
> Two residues block clean V8.1 READY:
>
> (a) **Broad-scope β1 cohort-coherence break** — sixteen of eighteen ARCH §7.4 mnemonic codes diverge from PASS-3 §6b numeric-aliased rows. The synthesis fold (`e5cb1e4b`) declared the broader retirement at ARCH §7.4 prose; the PASS-3 fold did not extend to the §6b ledger. ARCH §7.4 cites PASS-3 as source-of-truth for verbatim strings; the citation is now broken at the identifier level. **Surgery: V8.2 hardening cycle should rename `BBNF-LIFE001` → `BBNF-LIFETIME-ESCAPE`, `BBNF-VISIT001` → `BBNF-VISITOR-NO-MATCHING-KINDS`, `BBNF-PATH001` → `BBNF-PATH-UNKNOWN-SEGMENT`, etc., across the §6b ledger and the worked-path prose at `:122-:125`, `:247`, `:254`. The verbatim error strings are preserved verbatim; only the identifier prefix changes.**
>
> (b) **Closure broadening V2-amendment phrasing residue** — `:191` retains "V2 amendment surface" language for function-value broadening tied to Lock 1 reuse-map amendment. The architectural claim is legitimate but the phrasing lies outside V8.1's three named exclusions. **Surgery: rephrase to "Lock 1 amendment surface" — preserves the architectural claim, honours V2-amendment retirement discipline.**
>
> Both residues are documentation-level surgery on identifiers and phrasing. V1 substance is unchanged; the V8 punch list's host-leverage + tranche-routing items (#6-#11) all closed. The architecture is one cycle of cohort-coherence sync (β1 broad-form across PASS-3 §6b) + one rephrase (`:191`) from V8.1 READY.
>
> The eleven-of-eleven V8 punch items distribute: #6-#11 (host-leverage + tranche-routing) closed in Phase 8.4; #1-#2-#4-#5 (architectural CONSOLIDATE/SIMPLIFY) are non-blocking residuals classified as Tier I deferrals; #3 (BBNF-OPT001/002 + BBNF-LOCAL-EQUALITY-ANNOTATION cookbook-only) partially landed via Phase 8.3.1 GADT promotion (which moved BBNF-LOCAL-EQUALITY-ANNOTATION to V1-emit, not the V8-recommended cookbook-only path) — the cohort accepted GADT V1 surface promotion as a separate, architecturally-warranted fold.

## §10 Closing posture

PASS-3 absorbed the largest host-leverage delegation in the four-target V8.1 verification cohort. Five bindings — `thiserror` + `miette` + `syn::visit` + `tower-lsp` + `dap-types` + `salsa` — land verbatim with explicit V8 §3 γ-lens citations; three tranche-body routings — DAP body + LSP completion body + incremental + cookbook content — carry Receiver/Blocker/Receiving-gate triples at §8. The Phase 8.3.1 GADT V1 fold promotes `BBNF-LOCAL-EQUALITY-ANNOTATION` to V1-emission per Lock 4 amendment, with the OutsideIn(X) implication-constraint solver discharging branch-local equalities to `LayoutFacts` per ARCH §8.2.

V8.1 verifies the substance: PASS-3 V1 surface remains intact (typed roots + tape/direct union + path/select metadata-driven validation + format() public method + recovery semantics + bench discipline), and the eight Phase 8.4 host-leverage + tranche-routing surgeries closed cleanly. The two residues — broad-scope β1 ledger sync and closure-broadening V2-amendment phrasing — are documentation-level. Neither subtracts from V1 substance; both are narrow rephrases that route to a V8.2 hardening cycle.

The post-V8.1 PASS-3 surface is ready for per-tranche full-spec drafting (Wave 9+) once the two residues close. The substance survives; the V1 weight is honest; the host idioms take the protocol scaffolding load; aspirational surfaces carry explicit Tranche I/J body deferrals; the GADT V1 surface is committed; the diagnostic identifier sync awaits one cohort-coherence pass.
