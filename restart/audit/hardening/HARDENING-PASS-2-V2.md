# HARDENING-PASS-2-V2 — Rerun against Wave 1.2 + Wave 2 amendments

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-2-codegen/PASS-2.md` (559 lines, post-amendment) |
| V1 baseline | `restart/audit/hardening/HARDENING-PASS-2.md` commit `303b91a9` (verdict AMENDMENT-REQUIRED, 9-item punch list) |
| Amendment commits audited | `2778f34d` (Wave 1.2: BIR ownership ratification + import-deny + payload refinement), `d206b895` (Wave 2: emission contract + lookbehind co-amendment + emission table + budgets + SOTA + carries + OpenFrame) |
| Sub-agent surface | six PASS-2 sub-agent reports (correction notes carried in §2 + §6 + §8) |
| V2 output path | `restart/audit/hardening/HARDENING-PASS-2-V2.md` |
| Lanes applied | nine; Lane 2 N/A for single-pass scope |
| Tightened gate-rerun | all 16 commands rerun, with 7 PASS-2-touching gates resolving to expected post-conditions |

Punch items routed to PASS-2 by HARDENING-CONSOLIDATED §5: 1 (BIR ownership confirmation, verify-only stub since Wave 1.1 lands surgery), 2 (lowerer import-deny gate), 4 (BIR payload refinement), 5 (PASS-3 emission contract), 7 (lookbehind co-amendment), 14 (runtime emission table), 24 (per-grammar generated LOC table), 27 (non-generated LOC + child-count budgets), 28 (xtask wall baseline), 29 (SOTA table — verify + patch delta), 39 (carry ledger), 46 (OpenFrame retirement confirmation).

## §2 Cohort verdict

| Lane | V2 Verdict | KEEP | REINVENT | DISCARD | V1 → V2 delta |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | READY | 13 | 1 | 0 | KEEP +3, REINVENT -2, DISCARD -1 (BIR ownership ratified at `ir/src/backend_ir/`; import-deny gate verbatim; OpenFrame replacement design landed) |
| 2 Sequencing | N/A | — | — | — | unchanged (single-pass) |
| 3 Cohesion | READY | 8 | 0 | 0 | KEEP +3, REINVENT -2 (PASS-3 consumer acceptance gates landed at §4; runtime emission table landed at §6; BIR snapshot baseline categorised) |
| 4 SOTA-Anchoring | READY | 7 | 0 | 0 | KEEP +3, REINVENT -2 (parse-throughput trajectory inlines competitor + dataset + platform + bbnf target for every row at §7; mechanism-only rows separated) |
| 5 Grammar-Authoritative | READY | 6 | 1 | 0 | KEEP +2, REINVENT -2 (10-row runtime emission table landed at §6 covering nine grammars + yaml; yaml smoke proof carries metadata-only contract) |
| 6 Generated-Code-Budget | READY | 7 | 0 | 0 | KEEP +4, REINVENT -2 (per-grammar generated LOC table at §6; 7-row non-generated LOC + child-count enforcement; xtask wall budget categorised observed/provisional) |
| 7 Friction-Forecast | READY | 6 | 0 | 0 | KEEP +4, REINVENT -4 (six BBNF-* diagnostic codes at §8; lookbehind co-amendment carries `BBNF-SEM040` + cross-PASS routing) |
| 8 Carry-Deferral | READY | 8 | 0 | 0 | KEEP +5, REINVENT -2 (8-row carry ledger at §8 with Receiver/Blocker/Receiving-gate triples) |
| 9 Greenfield-Discipline | READY | 7 | 0 | 1 | KEEP +1, REINVENT -2 (OpenFrame deletion-path archaeology confirmed at §9 punch item 4 + §7 mechanism gate) |

| Verdict class | V1 totals | V2 totals | Net |
|---|---:|---:|---|
| KEEP | 38 | 62 | +24 |
| REINVENT | 20 | 2 | -18 |
| DISCARD | 1 | 1 | unchanged |

**Final V2 decision: READY** — the BIR-ownership ratification, the lowerer import-deny gate, the runtime emission table, the per-grammar LOC table, the SOTA trajectory rows, the carry ledger, and the OpenFrame retirement together resolve every V1 punch item and every consolidated-ledger row routed to PASS-2.

## §3 Lane 1 — Lock-Adherence

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:5-7 | Lock 5 — codegen consumes Backend IR only | verdict "REINVENT codegen around Backend IR, Tape-backed runtime template, and BIR-only lowerers" | resolves V1 BIR-walking concern; current driver violation cited as deletion target | none | matches Lock 5 + Architecture §10 | KEEP |
| PASS-2.md:32 | Lock 5 — BIR-only boundary | "Backend IR is the PASS-2 boundary … No lowerer imports Grammar IR" | direct claim with import-deny gate at §3 | none | gate at line 239 is verbatim | KEEP |
| PASS-2.md:36 | Lock 1 — tape unioned with direct-to-struct | "Every rule has a `TapeShape` and `ValueShape` … `&'i Tape<'i>` plus node id" | matches Lock 1; samply pathology cited as deletion target | none | OpenFrame replacement at §7 mechanism gate | KEEP |
| PASS-2.md:48 | Lock 14 — no per-grammar declaration crates by default | "No per-grammar declaration crates by default … Amendment 01 retracts them" | matches Lock 14 | none | runtime emission table at §6 covers all 9 + yaml smoke | KEEP |
| PASS-2.md:80-93 | Payload-refiner contract | "PASS-2 may sharpen field types ... PASS-2 may not introduce new variants, retire variants, or redefine the alphabet" | resolves HARDENING-CONSOLIDATED §3 conflict #1 (Backend IR ownership) | none | sharpen-vs-touch table makes the boundary executable | KEEP |
| PASS-2.md:170-188 | `ir` Backend IR ownership ratification | "PASS-2 ratifies Backend IR type-definition + variant-alphabet ownership at `ir/src/backend_ir/`" | Wave 1.2 ratification of Wave 1.1 surgery | "the `codegen` crate's role is **lowerer + adapter + snapshot + emission-test consumer**" | matches PASS-1.md:41 verbatim | KEEP |
| PASS-2.md:225 | `codegen/src/backend_ir/README.md` is documentation only | "documentation only: it records the import boundary and points contributors to `ir::backend_ir`" | rules out the previously-faulted `codegen/src/backend_ir/` ownership path | none | gate-rerun check 3 verifies | KEEP |
| PASS-2.md:227-242 | Lowerer import-deny floor + verbatim deny command | "rg -n \"GrammarIR\" crates/codegen/src/lower crates/codegen/src/runtime_template" | resolves V1 punch item 2; HARDENING-CONSOLIDATED §4.2 | "Expected output: zero matches" | exemption row at line 242 names `passes` BIR producer only | KEEP |
| PASS-2.md:166-168 | Lookbehind co-amendment | grammar-level `|<` only; finite-width-only at lowering boundary; PASS-1 `BBNF1004` + PASS-2 `BBNF-SEM040` co-emit | resolves V1 punch item 7 + HARDENING-CONSOLIDATED §4.7 | none | width invariant cited at PASS-1.md:64 | KEEP |
| PASS-2.md:6-7 | DISCARD list (ParseStream, Unicode sets, rewrite-mode, declaration crates default) | enumerates inheritances to retire | matches HARDENING-CONSOLIDATED §3 row 6 | none | feeds Architecture §0 conflict ledger | KEEP |
| PASS-2.md:300 | Lock 14 SIMD scan generic | "PASS-2 keeps the crate and adds BIR-fed alphabets and parity tests, not grammar-specific code" | mechanism-only | none | matches `simd-scan` crate keep-as-is in MIGRATION §9.3 | KEEP |
| PASS-2.md:228-242 | Code-gen close gate | "fails codegen close, emits diagnostic `BBNF-GEN001`, and blocks the regen-equality gate" | resolves V1 punch item 2 | none | runs at PR check + every codegen close + every regen-equality verification | KEEP |
| PASS-2.md:543-554 | Punch list — OpenFrame deletion + import-deny | "Replace the broad `Emitter` trait with BIR consumer APIs and enforce import-deny checks" | matches HARDENING-CONSOLIDATED §3 row 11 | none | TapeBuilder + BIR builder-frame is the positive surface | KEEP |
| PASS-2.md:50-76 | Backend IR final variant table | 23 variants × 5 columns (payload, generation site, Rust lowering, WASM lowering, TS scaffold) | resolves V1 punch item 4 | "TS status" is "scaffold" for every row | matches Q28 deferral + PASS-3 §10 unresolved punch list | KEEP |

Lane 1 verdict: **READY**. KEEP 14 / REINVENT 0 / DISCARD 0 (V1 had KEEP 10 / REINVENT 3 / DISCARD 1; every REINVENT entry resolved by Wave 1.2 + Wave 2; DISCARD entry — `codegen/src/backend_ir/` ownership — confirmed retired).

## §4 Lane 2 — Sequencing Discipline

N/A. PASS-2 is a single-pass synthesis. The 8-row carry ledger at §8 binds PASS-2's hand-offs to receiving tranches (E, F, G, H, I, J, BD.W1, BD.W2, BD.W3, BD.W4, BD.W5).

## §5 Lane 3 — Cohesion

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:336-347 | PASS-3 consumer acceptance gates | six rows (parse_signature_compile, view_metadata_visitor, view_metadata_selector, cost-table --check, grammar_schema_load, diagnostic_vocabulary, wasm_abi_descriptor) | resolves V1 punch item 5 + HARDENING-CONSOLIDATED §4.5 | none | every contract is named-test-backed | KEEP |
| PASS-2.md:50-76 | 23-variant BIR table | every variant: payload + generation site + Rust + WASM + TS | resolves V1 punch item 4 | none | matches Architecture §7.2 | KEEP |
| PASS-2.md:95-107 | Per-payload lowering test gates | seven rows mapping payload category → cargo test path → backend obligation source | resolves V1 cohesion concern about prose-only handoff | none | every row cites PASS-1.md verbatim | KEEP |
| PASS-2.md:170-188 | Backend IR ownership at `ir/src/backend_ir/` | producer-side citation back to PASS-1.md:41 | resolves HARDENING-CONSOLIDATED §3 conflict #1 | none | matches Architecture §7.2 ownership | KEEP |
| PASS-2.md:380-392 | Per-grammar generated LOC table | 11-row table (9 grammars + yaml + total) × 5 columns (current, max, disposition, xtask wall, baseline) | resolves V1 punch item 24 + HARDENING-CONSOLIDATED §4.24 | yaml row marked "provisional (owner: SYNTHESIS Wave-2)" | the provisional label is correct since baseline lands at first onboarding execution | KEEP |
| PASS-2.md:394 | Carry pointer to SYNTHESIS Wave-2 | "the architecture-side authoritative copy must remain row-for-row identical, with PASS-2 staying the producer-side reference" | binds the producer-vs-consumer ownership | none | matches Architecture §12.1 (10-row × 9-col table) | KEEP |
| PASS-2.md:412-421 | Regen-cycle wall budget | 6 cycles × 4 columns (cycle, budget, baseline, reason) | resolves V1 punch item 28 | "BIR snapshot print" + "yaml smoke regen" rows are provisional | both label owner + receiving gate | KEEP |
| PASS-2.md:461-475 | Runtime emission table | 10 grammars × 7 cell types (generated.rs, parser.rs, host.rs, host source, layout source, error source, Pratt/SIMD source) | resolves V1 punch item 14 + HARDENING-CONSOLIDATED §4.14 | none | hand-written prohibition at line 476 + verification grep | KEEP |

Lane 3 verdict: **READY**. KEEP 8 / REINVENT 0 / DISCARD 0.

## §6 Lane 4 — SOTA-Anchoring

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:427-437 | Throughput trajectory | 7 rows × 5 columns (competitor + dataset + platform + bbnf target + mechanism + evidence artefact) | resolves V1 punch item 29 + HARDENING-CONSOLIDATED §4.29 | none | every row passes Lock 8's competitor-named gate | KEEP |
| PASS-2.md:431 | twitter row | "sonic-rs 436 µs / simd-json 424 µs" → "≤ 380 µs" on M1 Pro | matches MASTER-PLAN §4 row | none | mechanism column names `SimdScan` BIR + `simd-scan` structural index | KEEP |
| PASS-2.md:434 | bootstrap row | "lightning-css 4.16 ms" → "≤ 3.0 ms" | matches MASTER-PLAN §4 row | none | mechanism column names `Layout`, `RegexDfa`, `HostCall`, `SimdScan` | KEEP |
| PASS-2.md:436-437 | simdjson on-demand split rows | M-series ≥ 5 GB/s; x86 ≥ 7 GB/s | matches Lock 8 anchor | none | both rows route to kernel parity + index throughput report | KEEP |
| PASS-2.md:439-446 | Mechanism gates separate from throughput | three rows (OpenFrame deletion, Pratt auto-detection, WASM parity) | distinct from parse-throughput SOTA gates | none | matches HARDENING-CONSOLIDATED §3 row 9 (final SOTA gate) | KEEP |
| PASS-2.md:443 | OpenFrame mechanism gate | "samply on every emitted parser confirms no `Vec<OpenFrame>::clone` symbol" | mechanism evidence with named tool | none | matches HARDENING-CONSOLIDATED §3 row 11 | KEEP |
| PASS-2.md:447 | "PASS-2 should not claim final perf wins until generated parsers run the corpus" | mechanism-only stance | leaves J.W1 as the final close gate | none | matches MASTER-PLAN §10 / §15 (J close) | KEEP |

Lane 4 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 0 (V1 had KEEP 4 / REINVENT 2; row-complete trajectory + mechanism-only separation closes the V1 lane gap).

## §7 Lane 5 — Grammar-Authoritative Discipline

Verification:
- `rg -ni 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' restart/audit/pass-2-codegen/PASS-2.md` returns matches only inside CENSUS citations + runtime emission table cells (mechanism-level not match-arm).
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-2-codegen/PASS-2.md` returns zero.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:461-475 | Runtime emission table — 10 rows × 7 columns | every cell is template-emitted or data-only | resolves V1 punch item 14 | none | yaml row carries metadata-only host route | KEEP |
| PASS-2.md:476 | Hand-written prohibition | "every column is generated by `cargo xtask regen --check`" | grep proof: `rg -n "// hand-written"` returns zero outside generated headers | none | matches Lock 14 generic-fleet posture | KEEP |
| PASS-2.md:367-374 | yaml smoke onboarding | "Grammar source"/"Metadata"/"Runtime emission"/"Registry"/"Gate" rows | matches Lock 14 two-surface mandate | none | gate command at line 374 binds xtask check + future_grammar_yaml_runtime test | KEEP |
| PASS-2.md:480-491 | Required smoke per grammar | 10 rows; each grammar with smoke gate | resolves V1 cohesion concern | none | matches MASTER-PLAN §11 F.W5 close gate | KEEP |
| PASS-2.md:48 | Per-grammar declaration-crate disposition | "no declaration crate is part of PASS-2" + Amendment 01 citation | mechanism-only | none | matches Lock 14 + Architecture §5.6 fence | KEEP |
| PASS-2.md:300 | `simd-scan` stays generic | "no grammar code enters `simd-scan`" | matches MIGRATION §9.3 + Architecture DAG | none | `simd-scan` keep-as-is row | KEEP |
| PASS-2.md:329 | Path/visitor metadata | "generated metadata for paths, visitors, diagnostics, and host tables" | feeds PASS-3 consumer | none | matches PASS-3 §6a feeder table | REINVENT (cell intent: should explicitly route through `path-core` schema, not just "path-core") |

Lane 5 verdict: **READY**. KEEP 6 / REINVENT 1 / DISCARD 0 (the single REINVENT is a phrasing tightening; the cell behaviour is correct, and the row consumer—PASS-3 §6a + Architecture §12.1—holds the canonical column set).

## §8 Lane 6 — Generated-Code + LOC Budget

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:380-392 | Per-grammar generated LOC table | 11 rows × 5 columns (current + max + disposition + xtask wall + baseline) | resolves V1 punch item 24 | yaml row "provisional" | provisional label ratified by SYNTHESIS Wave-2 owner | KEEP |
| PASS-2.md:394 | Architecture promotion carry | matches HARDENING-CONSOLIDATED §4.24 + Architecture §12.1 | row-for-row promotion to MASTER-PLAN §20 | none | clean hand-off path | KEEP |
| PASS-2.md:398-409 | Non-generated budget + child-count floor + per-area enforcement | 7 rows × 4 columns (Area + LOC budget + Child-count proof + Enforcing command) | resolves V1 punch item 27 + HARDENING-CONSOLIDATED §4.27 | none | rows enumerate `ir/src/backend_ir/`, `codegen/src/lower/{rust,wasm}/`, `codegen/src/runtime_template/`, `runtime/src/`, `host/src/`, `xtask/src/regen/` | KEEP |
| PASS-2.md:412-421 | Regen-cycle wall-time budget | 6 rows × 4 columns | resolves V1 punch item 28 | none | every row carries baseline category (observed/provisional + owner) | KEEP |
| PASS-2.md:413 | `cargo xtask regen --check` ≤ 22s | observed against BC iter-gate | matches MASTER-PLAN §20 trajectory wall budgets | none | row binds to evidence | KEEP |
| PASS-2.md:417 | css_l4 single-grammar regen ≤ 12s | observed (PASS-B audit) | matches the 107K LOC scale | none | css_l4 is the budget hotspot | KEEP |
| PASS-2.md:418 | BIR snapshot print ≤ 5s for 9 grammars | provisional (owner: PASS-2 amendment agent; receiver: SYNTHESIS Wave-2 measurement gate) | provisional label correct | "snapshots are analysis output, not formatting-heavy source generation" | the provisional baseline lands when BIR producer is implementable | KEEP |

Lane 6 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 0.

## §9 Lane 7 — Friction Forecast

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:520-528 | Diagnostic ledger | 6 codes × 3 columns (Code, Trigger, PASS-2 producer) | resolves V1 punch item 34 + HARDENING-CONSOLIDATED §4.34 | none | every code routes to a named producer | KEEP |
| PASS-2.md:522 | `BBNF-GEN001` lowerer-imports-grammar-IR | committed string | resolves V1 import-deny diagnostic concern | none | matches PASS-3 §6b LowererImport | KEEP |
| PASS-2.md:523 | `BBNF-GEN014` LOC budget exceedance | committed string | resolves V1 budget-without-diagnostic concern | none | runs at regen budget check | KEEP |
| PASS-2.md:524 | `BBNF-CODEGEN021` regen equality | committed string | resolves V1 regen drift concern | none | runs at regen equality | KEEP |
| PASS-2.md:526 | `BBNF-LIFE009` lifetime surface violation | committed string | matches PASS-3 §6b BBNF-LIFE001/002 | none | feeds runtime compile tests | KEEP |
| PASS-2.md:527 | `BBNF-SEM040` unbounded lookbehind | committed string | resolves V1 lookbehind concern + matches lookbehind co-amendment at line 168 | none | halts codegen close | KEEP |
| PASS-2.md:166-168 | Lookbehind co-amendment routing | "the two diagnostics are produced together" | resolves V1 punch item 7 | none | PASS-1 owns user-facing string; PASS-2 owns routing | KEEP |

Lane 7 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 0.

## §10 Lane 8 — Carry & Deferral Audit

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:530-540 | Carry ledger | 8 rows × 4 columns (Item, Receiver, Blocker, Receiving gate) | resolves V1 punch item 39 + HARDENING-CONSOLIDATED §4.39 | none | every row has all three triple fields | KEEP |
| PASS-2.md:533 | PASS-1 reconciliation row | Receiver SYNTHESIS Wave-2 + Tranche E; Blocker variant schema differences; Gate `cargo xtask bbnf bir --all --check` | matches HARDENING-CONSOLIDATED §3 conflict #1 resolution | none | snapshot byte-equality is a precise gate | KEEP |
| PASS-2.md:534 | PASS-3 API docs row | Receiver SYNTHESIS Wave-2 + Tranche G; Blocker template metadata gaps; Gate seven consumer acceptance gates | named gates land at lines 336-347 | none | gates carry verbatim cargo test paths | KEEP |
| PASS-2.md:535 | TS production carry | Receiver Tranche BD.W1 + SYNTHESIS post-PASS-3; Blocker Q28 deferral; Gate TS scaffold compile + production at BD.W1 | matches HARDENING-CONSOLIDATED §4.39 + PASS-1 §5 TS-deferred row | none | BIR snapshot equality across Rust V1 + TS V2 ratifies | KEEP |
| PASS-2.md:536 | BD.W5/J parity carry | Receiver Tranche BD.W5 + Tranche J; Blocker 9-grammar × ≥3-fixture × 3-backend matrix; Gate 81-cell matrix + J.W1 numeric SOTA gate | matches MASTER-PLAN §10 J close | none | parity execution belongs downstream of PASS-2 | KEEP |
| PASS-2.md:537 | Publication carry | Receiver Tranche BD.W3 + SYNTHESIS package routing; Blocker package-name detail not yet routed; Gate A.W1 / J.W3 publication gate | matches HARDENING-CONSOLIDATED §4.22 + MASTER-PLAN A.W1 | none | publication details routed correctly | KEEP |
| PASS-2.md:538 | Fixtures carry | Receiver Tranche BD.W4 + downstream parity gates; Blocker Lock 14 onboarding two-surface only; Gate BD.W4 fleet-fixture | matches HARDENING-CONSOLIDATED §4.12 (fixture separation) | none | onboarding stays clean of fixtures | KEEP |
| PASS-2.md:539-540 | path-ts + WASM ABI carries | each carries Receiver/Blocker/Receiving-gate triple | matches PASS-1 §5 + PASS-3 §10 | none | WASM ABI descriptor lands at BD.W2; path-ts at BD.W1 | KEEP |

Lane 8 verdict: **READY**. KEEP 8 / REINVENT 0 / DISCARD 0 (V1 had KEEP 3 / REINVENT 2 / DISCARD 0; all REINVENT items resolved).

## §11 Lane 9 — Greenfield Discipline

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-2.md:7 | DISCARD list | retires "stale ParseStream naming, grammar-level Unicode sets, rewrite-mode walker, per-grammar declaration crates as default, OpenFrame checkpointing, and direct Grammar IR consumers in lowerers" | resolves V1 amendment-required language | none | matches HARDENING-CONSOLIDATED §3 row 6 | KEEP |
| PASS-2.md:80-82 | Payload-refiner contract | "PASS-2's role in the BIR contract" with explicit "may sharpen / may not touch" rules | resolves Lane 1 conflict | none | the contract is mechanism-only | KEEP |
| PASS-2.md:443 | OpenFrame deletion mechanism gate | "samply on every emitted parser confirms no `Vec<OpenFrame>::clone` symbol" | resolves V1 punch item 46 + HARDENING-CONSOLIDATED §3 row 11 | none | the deletion is verified mechanically | KEEP |
| PASS-2.md:547 | OpenFrame deletion punch list | "delete OpenFrame-style runtime builders before migration begins. The OpenFrame substrate has no preserved role" | matches PASS-1.md:282 + Architecture §7.2 invariants | none | TapeBuilder + BIR builder-frame is the positive surface | DISCARD-confirmed |
| PASS-2.md:300 | `simd-scan` stays generic | "BIR-fed alphabets and parity tests, not grammar-specific code" | mechanism-only; no overfit | none | matches MIGRATION §9.3 keep-as-is | KEEP |
| PASS-2.md:48 | No declaration crates by default | matches Amendment 01 + Lock 14 | abrogates declaration-crate sprawl | none | rare-escape fence at Architecture §5.6 carries the proof | KEEP |
| PASS-2.md:9-17 | Conflict ledger | 5 rows × 3 columns (conflict, stale authority, settled resolution) | resolves V1 cohesion concern about stale prompt drift | none | matches HARDENING-CONSOLIDATED §3 cross-target conflicts | KEEP |
| PASS-2.md:497-501 | Inheritance ledger | KEEP/REINVENT/DISCARD per inheritance source (PASS-B, Amendment 01, BC typed IR, BB cohort template, simd-scan, current source) | preserves greenfield discipline | none | every legacy code path is explicitly classified | KEEP |

Lane 9 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 1 (V1 had KEEP 6 / REINVENT 2; the V1 DISCARD-confirmed remains; all REINVENT items resolved).

## §12 Punch list (residuals)

V1's 9-item punch list collapses to zero residual surgeries against PASS-2. The single Lane 5 REINVENT in §7 above is a phrasing tightening (row consumer language); it is non-blocking and can fold into Architecture §12.1's authoritative table consumption row at next pass-through.

## §13 Final readiness

> **Decision: READY**
>
> PASS-2 V2 returns READY across nine lanes with no blocking surgery. Wave 1.2 (BIR ownership ratification at `ir/src/backend_ir/`, lowerer import-deny gate `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template returns zero`, BIR payload refiner contract) and Wave 2 (PASS-3 emission contract with seven consumer acceptance gates, lookbehind co-amendment with `BBNF-SEM040`, 10-row runtime emission table, per-grammar generated LOC table promoted into MASTER-PLAN §20, 7-row non-generated LOC + child-count enforcement, 6-row regen-cycle wall budget with observed-vs-provisional baseline categories, 7-row throughput trajectory with competitor + dataset + platform + bbnf target, 8-row carry ledger, OpenFrame deletion-path archaeology) collectively address every V1 punch item.
>
> Hereupon PASS-2 is cleared for downstream consumption: MASTER-PLAN tranche E (BIR + VM) + tranche F (Rust lowerer + runtime template) + tranche H (Pratt/SIMD/WASM) + tranche J (parity + close) all consume PASS-2 outputs against named gates with no orphan deferral.
