# HARDENING-PASS-2-V9.1

V9.1 verification for `target=PASS-2`, cycle `V9.1`, against the amended
PASS-2 corpus after V9. Scope: `restart/audit/pass-2-codegen/PASS-2.md`, with
targeted reads of `restart/audit/pass-1-substrate/PASS-1.md` where PASS-2
cites it, and `restart/ARCHITECTURE.md` where PASS-2 cites ARCH 7.2, 7.5, and
8.4. This report does not amend the target documents.

## 1. Target identification

| Item | Value |
|---|---|
| Target | `restart/audit/pass-2-codegen/PASS-2.md` |
| Lines audited | 633 |
| Prior hardening | `restart/audit/hardening/HARDENING-PASS-2-V9.md` |
| Consolidated V9 state | `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.md` |
| Verification output | `restart/audit/hardening/HARDENING-PASS-2-V9.1.md` |
| Lens set | V9+ A-K, focused on V9 punch-list closure |

## 2. Targeted rg scans

| Scan | Result | Verification use |
|---|---|---|
| `rg -n -i -F 'WASM V1' restart/audit/pass-2-codegen/PASS-2.md` | zero matches | No live `WASM V1` phrase remains in PASS-2. |
| `rg -n -i -F 'V1 WASM' restart/audit/pass-2-codegen/PASS-2.md` | one match at PASS-2:140, in "post-V1 WASM SOTA gate" | The remaining match is a post-V1 receiver phrase, not an active V1 obligation. |
| `rg -n -i 'WasmBackend|TsBackend|RustBackend|TS production|path-ts|WASM host primitive|V2 WASM|wasm_abi_descriptor' ...` | PASS-2:40, 131, 139-141, 384, 493, 587-592 align with PASS-1:61 and ARCH:1094-1095 | Receiver language is coherent: V1 `RustBackend`; V2 `WasmBackend` / `TsBackend`. |
| `rg -n 'Backend IR final|final variant table|22-row|22-variant|20-row|payload-refiner table mapped|ARCH .7\.2 variant|19 semantic variants|20 variants' PASS-2.md` | No "final variant table"; PASS-2:50-76 names a payload-refiner table with rows 1-20 | The competing 22-row final alphabet is gone. Historical 22-collapse mentions remain as archaeology/fold explanation. |
| `rg -n 'PASS-1-SUBSTRATE|PASS-2-CODEGEN|PASS-3-RUNTIME|SYNTHESIS\.md|restart/prompts/(PASS-1|PASS-2|PASS-3|SYNTHESIS)' PASS-2.md` | zero matches | Retired dispatch prompts are not live PASS-2 authority. |
| `rg -n 'pointer!' restart/audit/pass-2-codegen/PASS-2.md` | zero matches | `pointer!` is gone from live PASS-2 user/consumer gates. |
| `rg -n '\bErrorRecovery\b|`Layout`|\bHostCall\b' PASS-2.md` | PASS-2:367, 383, 507, 619 | `ErrorRecovery` and one perf-row `Layout` remain live residues; PASS-2:619 is explicit archaeology and acceptable. |
| `rg -n 'ARCHITECTURE\.md:921|ARCHITECTURE\.md:952|ARCHITECTURE\.md:1187-1207|line 417|line 232' PASS-2.md` | PASS-2:34, 78, 197-199, 386 | V9 wrong-line citations are partly reopened by current line drift. |

## 3. V9 punch-list verification

| V9 item | Current evidence | Verdict |
|---|---|---|
| Retire active WASM V1 obligations | PASS-2 says "WASM and TS are V2 backend impls" and "V1 emits Rust through `RustBackend` only" at PASS-2:40. The obligation table has `RustBackend` active and `WasmBackend` / `TsBackend` carried post-V1 at PASS-2:139-141. PASS-1 states the same at PASS-1:61; ARCH states the same at ARCH:1094-1095. | CLOSED |
| Make Backend/TS/WASM receiver language coherent | PASS-2 routes `path-ts`, TS production, WASM ABI descriptor, and cross-backend parity to V2 receivers at PASS-2:74, 384, 587-592. This matches ARCH 7.5 at ARCH:1090-1153. | CLOSED |
| Remove competing 22-row final BIR table | PASS-2 now labels the table "payload-refiner table mapped to ARCH 7.2 Backend IR" at PASS-2:50 and enumerates rows 1-20 at PASS-2:52-76. PASS-2 also states it is "payload refiner, not BIR re-owner" at PASS-2:82-95. | CLOSED |
| Align BIR names with ARCH 7.2 | The main table uses `LayoutScope`, `ErrorRecover`, and `CallHost` at PASS-2:64-66; ARCH owns the same names at ARCH:923-925 and payload rows ARCH:954-956. | PARTIAL |
| Remove retired prompt authority | Retired prompt scan returns zero matches in PASS-2. The live `restart/prompts/ORCHESTRATOR.md:54-69` citation at PASS-2:5 points to the hardening-cycle naming canon and is not one of the retired dispatch prompts. | CLOSED |
| Close V9 wrong-line citations | The old `ARCHITECTURE.md:1200-1207` forbidden-behavior citation is gone, and PASS-2 now uses ARCH:1366-1373 for forbidden closure behavior at PASS-2:198 and PASS-2:200. However the `RegexProgram` and ARCH 8.4 line anchors now point at wrong current lines. | PARTIAL |
| Remove `pointer!` from PASS-2 gates | PASS-2 uses typed `path!` at PASS-2:74, 131, 139, and 382. `rg 'pointer!' PASS-2.md` returns zero. | CLOSED |

## 4. Remaining residues

| Residue | Evidence | Why it matters | Required narrow amendment |
|---|---|---|---|
| `ErrorRecovery` remains as a live BIR spelling in PASS-3 handoff rows. | PASS-2:367 and PASS-2:383 say BIR `ErrorRecovery`; ARCH owns `ErrorRecover` at ARCH:925 and ARCH:956. | This is exactly the V9 old-spelling class: a live user/consumer gate names a non-authoritative BIR variant. | Replace both live `ErrorRecovery` mentions with `ErrorRecover`. |
| `Layout` remains as a live perf/contribution BIR row. | PASS-2:507 lists construct ``Layout`` in the per-construct contribution plan. ARCH owns `LayoutScope` at ARCH:924 and ARCH:955. | The per-construct plan is BIR-shaped (`Alt`, `SimdScan`, `RegexProgram`, `PrattSpine`, `CallHost`), so `Layout` reads as the old BIR spelling, not merely Lock 2 vocabulary. | Replace PASS-2:507 construct with `LayoutScope`; if the intent is the Lock 2 side-table, say `LayoutFacts -> LayoutScope` explicitly. |
| `RegexProgram` line anchors are stale. | PASS-2:34 and PASS-2:78 cite `restart/ARCHITECTURE.md:921`; current ARCH:921 is `PrattSpine`. PASS-2:78 cites payload row `restart/ARCHITECTURE.md:952`; current ARCH:952 is also `PrattSpine`. Current `RegexProgram` anchors are ARCH:919 and ARCH:950. | V9 item P2-V9-5 was line-citation hygiene. The citation now misdirects readers to a different BIR variant. | Update PASS-2:34 and PASS-2:78 to ARCH:919 and ARCH:950, or cite ARCH 7.2 section-only if line drift is expected. |
| ARCH 8.4 closure inventory citation is stale. | PASS-2:197 cites `restart/ARCHITECTURE.md:1187-1207` for the four bounded closure forms; current ARCH 8.4 begins at ARCH:1351, with the inventory at ARCH:1357-1362. | The cited range is now core grammar sketch material, not closure semantics. | Replace the citation with ARCH:1357-1362. |
| Internal PASS-2 line references drifted. | PASS-2:198 and PASS-2:199 say the generic monomorphisation budget gate is at line 417; current gate is PASS-2:439. PASS-2:386 says the codegen close gate fires at line 232; current import-deny/codegen gate text is PASS-2:270-279. | These are lower-priority than wrong external citations, but they still degrade audit reproducibility. | Replace absolute self-line references with section names or current anchors. |

Accepted archaeology: PASS-2:619 says "PASS-2's historical `Layout` collapse" inside the Phase 8.4 fold classification. That is explicit archaeology, not a live BIR spelling.

## 5. Lens A-K verification table

| Lens | Verdict | Reason |
|---|---|---|
| A / lock adherence | AMENDMENT-REQUIRED-NARROW | Lock 5 and Lock 8 are honored for V1 Rust / V2 backends; Lock 2/5 terminology still has live `ErrorRecovery` and `Layout` residue. |
| B / sequencing | READY | Deferrals name receivers, blockers, and gates at PASS-2:581-592. |
| C / cohesion | AMENDMENT-REQUIRED-NARROW | Architecture is cohesive; stale line anchors break local provenance for ARCH 7.2 and 8.4. |
| D / SOTA anchoring | READY | Throughput rows name competitor, dataset, platform, target, mechanism, and evidence at PASS-2:472-484. |
| E / grammar-authoritative | READY | Future yaml onboarding uses exactly source plus metadata at PASS-2:407-417; no `pointer!` and no default declaration crate path. |
| F / LLM-bias | READY | No fresh hedging or pseudo-precision fault blocks V9.1; remaining problems are mechanical. |
| G / overfitting | READY | PASS-2 retains per-grammar matrixing only as generated/template output, not generic-crate grammar branching. |
| H / provenance | AMENDMENT-REQUIRED-NARROW | Retired prompts are gone, but wrong-line citations remain at PASS-2:34, 78, 197-199, and 386. |
| I / contrivance | READY | The two-method formal `Backend` trait plus internal 8-method lowerer decomposition is coherent at PASS-2:131-143 and ARCH:1097-1125. |
| J / host-language leverage | READY | Closure lifetime and monomorphisation are delegated to rustc at PASS-2:198-200. |
| K / meta-grammar discipline | READY | V1 is RustBackend; V2 backend impls consume the same BIR without grammar-side change at PASS-2:40, 139-141, and ARCH:1147-1153. |

## 6. Final decision

**Decision: AMENDMENT-REQUIRED-NARROW.**

The V9 PASS-2 amendment set closed the major architectural faults: WASM/TS are
V2 backend receivers, PASS-2 no longer re-owns a competing BIR alphabet, retired
dispatch prompt citations are gone, and `pointer!` no longer appears in PASS-2.
The remaining defects are narrow but live: two old BIR spellings survive in
handoff/perf rows, and current ARCH/PASS-2 line drift reopens citation hygiene.

Hereupon: run a narrow PASS-2 amendment for the spelling and citation rows above,
then rerun V9.1 verification. No re-draft threshold is met.
