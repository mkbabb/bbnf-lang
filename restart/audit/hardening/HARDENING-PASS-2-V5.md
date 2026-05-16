# HARDENING-PASS-2-V5

## §1 Target Identification

Target: `restart/audit/pass-2-codegen/PASS-2.md`.

Output: `restart/audit/hardening/HARDENING-PASS-2-V5.md`.

Worker role: Phase 0 V5 metahardening agent for `target=PASS-2`.

Audit mode: read-only verification against PASS-2 and its restart bindings.

No target patching was performed.

No other hardening report was edited.

Target revision shape:

| Field | Value |
|---|---|
| Primary target | `restart/audit/pass-2-codegen/PASS-2.md` |
| Target range audited | `PASS-2.md:1-573` |
| Carry baseline | PASS-2 V4 returned READY |
| V5 disposition | Re-open only narrow amendment faults |
| Required lenses | A through H |
| Lane discipline | Compressed 9-lane hardening; Lane 2 is N/A for report-only V5 |
| Patch discipline | Verify claims before rows; do not patch target surfaces |
| Commit scope | Stage and commit only this file |

Mandatory authority read before writing:

| Authority | Status |
|---|---|
| `restart/README.md` | Read |
| `restart/locks/LOCKS.md` | Read |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md` | Read |
| `restart/audit/hardening/HARDENING-CONSOLIDATED.md` | Read |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V2.md` | Read |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V3.md` | Read |
| `restart/research/INDEX.md` | Read |
| `restart/prompts/sub-orchestrators/HARDENING.md` | Read |
| `restart/prompts/audit-specs/HARDENING-LENS-SET.md` | Read |
| `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` | Read |
| `docs/precepts/instructions/STYLE.md` | Read |
| `docs/precepts/instructions/LESSONS-LEARNED.md` | Read |
| `docs/precepts/instructions/CONSUMING.md` | Read |
| `restart/ARCHITECTURE.md` | Read |
| `restart/MIGRATION.md` | Read |
| `restart/MASTER-PLAN.md` | Read |
| `restart/audit/pass-1-substrate/PASS-1.md` | Read |
| `restart/audit/pass-2-codegen/PASS-2.md` | Read |
| `restart/audit/pass-3-runtime/PASS-3.md` | Read |
| PASS-2 V1 through V4 history reports | Read |

Method:

1. Treat V4 READY as a carry baseline, not as immunity.
2. Re-check exact claims before adding a row.
3. Separate documentary drift from implementation risk.
4. Keep PASS-2 as the target while binding to ARCHITECTURE, MASTER, and MIGRATION.
5. Escalate only faults that would mislead the next amendment or synthesis worker.

## §2 Carry-Aware Lens Table A-E

| Lens | Row | Verified site | Finding | Disposition |
|---|---:|---|---|---|
| A narrative coherence | A1 | `PASS-2.md:196-253`; `ARCHITECTURE.md:870-903`; `MASTER-PLAN.md:343-372` | Backend IR ownership is coherent: `ir/src/backend_ir/` owns the executable boundary and codegen lowerers do not own Grammar IR. | KEEP |
| A narrative coherence | A2 | `PASS-2.md:32-36`; `PASS-2.md:347-358`; `PASS-3.md:416-429` | PASS-2 and PASS-3 agree that generated runtime metadata must feed path, visitor, diagnostics, and consumer gates. | KEEP |
| A narrative coherence | A3 | `ARCHITECTURE.md:1065`; `PASS-1.md:196-217`; `README.md:125-129`; `PASS-2.md:174` | ARCHITECTURE's grammar sketch makes lookbehind prefix-shaped (`"|<" Suffix`) while PASS-1/README/PASS-2 settle grammar-level infix lookbehind. | AMEND |
| A narrative coherence | A4 | `ARCHITECTURE.md:1077-1081`; `PASS-1.md:183`; `PASS-1.md:211`; `README.md:145-157` | ARCHITECTURE sketches declaration-only `@host fn` and dot chains, conflicting with block-bodied host functions and `->` chaining. | AMEND |
| A narrative coherence | A5 | `ARCHITECTURE.md:1248`; `MASTER-PLAN.md:123`; `MASTER-PLAN.md:631`; `MIGRATION.md:515-516`; `PASS-2.md:388-404` | Cross-doc references still cite stale PASS-2 generated LOC lines `293-310`; the current authority is §6 at `388-404`. | AMEND |
| B vocabulary drift | B1 | `PASS-2.md:69`; `ARCHITECTURE.md:971-990`; `MASTER-PLAN.md:295`; `MIGRATION.md:237` | Layout vocabulary is now stable: `passes::layout` emits `LayoutFacts`, and codegen consumes through `LayoutSink`/BIR. | KEEP |
| B vocabulary drift | B2 | `PASS-2.md:32`; `PASS-2.md:196`; `ARCHITECTURE.md:870`; `MASTER-PLAN.md:31-34` | `BackendIR`/`BIR` naming is consistent enough: prose uses `Backend IR`, implementation paths use `backend_ir`, and BIR names the executable shape. | KEEP |
| B vocabulary drift | B3 | `PASS-2.md:36`; `PASS-2.md:130-151`; `ARCHITECTURE.md:1158-1206`; `PASS-3.md:208` | `Tape`, `TapeBuilder`, and `runtime/src/tape/` are consistently the substrate surface, with direct-to-struct as a materialization peer. | KEEP |
| B vocabulary drift | B4 | `PASS-2.md:540-541`; `14-LOCKS.md:52`; `MASTER-PLAN.md:441-448`; `PASS-3.md:356-357` | PASS-2 diagnostic strings reintroduce `@pratt` and `@simd` as author hints; Lock 10 forbids those syntax directives. | AMEND |
| B vocabulary drift | B5 | `ARCHITECTURE.md:271-272`; `README.md:276-283`; `PASS-3.md:359-361`; `MASTER-PLAN.md:764` | `pointer!` and `select!` survive, but examples disagree on grammar-qualified shape and no shared query walkthrough exists. | AMEND |
| B vocabulary drift | B6 | `ARCHITECTURE.md:1008`; `ARCHITECTURE.md:1016`; `PASS-1.md:98`; `PASS-3.md:365` | `LookbehindWidth`, `BBNF-LOOKBEHIND-WIDTH`, `BBNF1004`, and `BBNF-LIFE003` coexist; aliases are recorded but the user-facing hierarchy needs one cookbook mapping. | AMEND-NARROW |
| C worked examples | C1 | `PASS-2.md:377-386`; `ARCHITECTURE.md:1270-1331`; `MASTER-PLAN.md:110`; `MIGRATION.md:747-759` | YAML onboarding has gates and the two-surface invariant, but no single end-to-end trace from `yaml.bbnf` through metadata, BIR, runtime emission, and budget. | AMEND |
| C worked examples | C2 | `README.md:276-283`; `ARCHITECTURE.md:271-272`; `PASS-3.md:393`; `MASTER-PLAN.md:764` | Query onboarding lacks one worked JSON/CSS example that exercises both `pointer!` and `select!` through generated metadata. | AMEND |
| C worked examples | C3 | `PASS-3.md:162-190`; `MASTER-PLAN.md:473-502`; `ARCHITECTURE.md:262-264`; `PASS-2.md:147-149` | Incremental parsing has thresholds and runtime hooks, but PASS-2 lacks an example showing how `incremental_marker` is emitted and consumed. | AMEND |
| C worked examples | C4 | `PASS-2.md:72`; `PASS-2.md:144`; `PASS-2.md:393`; `ARCHITECTURE.md:1027`; `PASS-3.md:160` | `@error(recover)` is present as policy and diagnostics, but no worked lowering shows recover policy under generated error/runtime metadata. | AMEND |
| C worked examples | C5 | `MASTER-PLAN.md:208-537`; `PASS-2.md:543-554`; `MIGRATION.md:690-770` | No single A-to-F-to-J grammar trajectory follows one grammar through bootstrap, codegen, runtime, parity, and publication gates. | AMEND |
| D coverage gaps | D1 | `README.md:168-174`; `PASS-2.md:72`; `PASS-2.md:78`; `ARCHITECTURE.md:1103-1120`; `MASTER-PLAN.md:327-331` | Generic rule typing under `@error(recover)` is not explicitly tested as a cross-product; this is a real authoring edge. | AMEND |
| D coverage gaps | D2 | `PASS-2.md:40`; `PASS-2.md:106`; `PASS-2.md:356`; `ARCHITECTURE.md:1026`; `MASTER-PLAN.md:459` | WASM V1 host primitives are named but not demonstrated through one scalar/SIMD parity example with host primitive lowering. | AMEND |
| D coverage gaps | D3 | `PASS-2.md:79`; `PASS-2.md:108`; `PASS-3.md:181-190`; `MASTER-PLAN.md:486-501` | Debug markers and LSP fallback policy are split across PASS-2/PASS-3; no binding example connects `DebugMarker` to fallback reporting. | AMEND |
| D coverage gaps | D4 | `MASTER-PLAN.md:524`; `README.md:393`; `14-LOCKS.md:54`; `PASS-2.md:547-554` | Lock 11 incubation stability is a MASTER/J concern; PASS-2 carries no false ownership but should name it as non-owner for generated dependency surfaces. | AMEND-NARROW |
| E cumulative lock tensions | E1 | `PASS-2.md:5-7`; `PASS-2.md:455`; `PASS-1.md:57`; `14-LOCKS.md:34` | Lock 1 is satisfied: OpenFrame is deletion archaeology, replaced by TapeBuilder checkpoints and BIR builder frames. | KEEP |
| E cumulative lock tensions | E2 | `PASS-2.md:240-253`; `14-LOCKS.md:42`; `MASTER-PLAN.md:756` | Lock 5 is satisfied: codegen lowerers are BIR-only and the `GrammarIR` token is legal only as deny-gate text. | KEEP |
| E cumulative lock tensions | E3 | `PASS-2.md:388-422`; `ARCHITECTURE.md:1354-1375`; `MASTER-PLAN.md:688-692` | Lock 13 is mostly satisfied: generated files have budgets, handwritten files and children have lint gates. | KEEP |
| E cumulative lock tensions | E4 | `PASS-2.md:377-386`; `PASS-3.md:320-344`; `MASTER-PLAN.md:770`; `14-LOCKS.md:60` | Lock 14 is satisfied for yaml onboarding surfaces but still needs a worked proof for unfamiliar grammar authors. | AMEND |
| E cumulative lock tensions | E5 | `PASS-2.md:540-541`; `14-LOCKS.md:52`; `MASTER-PLAN.md:768` | Lock 10 is violated by PASS-2 diagnostic wording even though MASTER explains auto-detection and disable-only metadata. | AMEND |

Carry conclusion for A-E:

PASS-2 V4 closed many structural faults.

The V5 re-open is not a broad rejection.

The live risks are concentrated in diagnostic wording, cross-document provenance, grammar-surface drift, and missing worked examples.

The most severe single target-surface issue is `PASS-2.md:540-541`.

The most severe cross-document issue is `ARCHITECTURE.md:1049-1081`.

## §3 LLM-Pathology Table F-H

| Lens | Row | Verified site | Pathology subclass | Finding | Disposition |
|---|---:|---|---|---|---|
| F LLM bias | F1 | `HARDENING-PASS-2-V4.md:112`; `PASS-2.md:540-541`; `14-LOCKS.md:52` | Normalization bias | V4 accepted `@pratt`/`@simd` strings as hint surfaces, but Lock 10 says those directives do not exist. | AMEND |
| F LLM bias | F2 | `PASS-2.md:81`; `PASS-2.md:50-79`; `ARCHITECTURE.md:870-903` | Cardinality confidence | The 23-variant BIR table is plausible and bounded, but its "closest local proxy" defence is weak provenance for an executable IR boundary. | AMEND-NARROW |
| F LLM bias | F3 | `PASS-2.md:437-459`; `ARCHITECTURE.md:1254-1259`; `MASTER-PLAN.md:143-149` | Benchmark certainty | Performance rows avoid final-win claims and require metadata; this is a good anti-bias pattern. | KEEP |
| F LLM bias | F4 | `PASS-2.md:543-554`; `MASTER-PLAN.md:737-757` | Handoff optimism | Carry ledgers exist, but some items rely on future gates without a worked grammar trajectory. | AMEND |
| G overfitting | G1 | `ARCHITECTURE.md:271-272`; `README.md:276-283`; `PASS-3.md:359-361` | Old macro-shape overfit | Prior `path!`/string-pointer examples still influence `pointer!` shape; the grammar-qualified macro form is not consistently taught. | AMEND |
| G overfitting | G2 | `PASS-3.md:162-190`; `README.md:344-348`; `PASS-2.md:147-149` | Generic incremental-parser overfit | Incremental thresholds look borrowed from established editor architectures; PASS-2 lacks the emitted marker example that proves bbnf-specific fit. | AMEND |
| G overfitting | G3 | `PASS-2.md:40`; `MASTER-PLAN.md:459`; `ARCHITECTURE.md:1220` | WASM parity overfit | WASM V1 is scoped, but host primitive parity is not shown with an actual grammar-author host chain. | AMEND |
| G overfitting | G4 | `PASS-2.md:153-170`; `MASTER-PLAN.md:441-461`; `PASS-3.md:356-357` | Recognizer overfit | Pratt/SIMD auto-detection is well scoped; the only overfit is the accidental directive wording in PASS-2 diagnostics. | AMEND |
| H hallucination/provenance | H1 | `ARCHITECTURE.md:1248`; `MASTER-PLAN.md:123`; `MASTER-PLAN.md:631`; `MIGRATION.md:515-516`; `PASS-2.md:388-404` | Wrong-line citation | Multiple docs cite `PASS-2.md:293-310` for generated LOC; that range is no longer the authority. | AMEND |
| H hallucination/provenance | H2 | `MASTER-PLAN.md:635-636`; `ARCHITECTURE.md:1320-1331` | Wrong-line citation | MASTER says ARCH §12.1 generated LOC lives at lines `1273-1281`; current per-grammar rows live at `1320-1331`. | AMEND |
| H hallucination/provenance | H3 | `MIGRATION.md:515`; `PASS-2.md:130-151`; `PASS-2.md:473-488` | Wrong-line citation | MIGRATION cites `PASS-2.md:98-116` for runtime template schema; current schema is `130-151`, with runtime emission at `473-488`. | AMEND |
| H hallucination/provenance | H4 | `PASS-2.md:437`; `restart/corpora/SOTA.md:50-89`; `restart/corpora/SOTA.md:130-136` | Primary-source compression | PASS-2 relies on local SOTA summaries, not direct competitor docs, but it correctly routes benchmark metadata and avoids unsupported publication claims. | KEEP-WITH-ROUTE |
| H hallucination/provenance | H5 | `PASS-2.md:174`; `PASS-1.md:215`; `ARCHITECTURE.md:1008-1016` | Alias drift | Lookbehind diagnostic aliases are real, but user-facing code mapping risks drift unless cookbook and PASS-3 ledger declare the canonical order. | AMEND-NARROW |

Pathology conclusion:

The V5 fault is not that PASS-2 is incoherent.

The fault is that a few strong-sounding strings would train later workers on non-existent author syntax.

The strongest hallucination class is stale line provenance.

The strongest overfit class is importing familiar directive patterns into a syntax that deliberately rejects them.

## §4 Compressed 9-Lane Verification

| Lane | Row | Site | Verification | Verdict |
|---|---:|---|---|---|
| 1 Lock alignment | 1.1 | `PASS-2.md:5-7`; `14-LOCKS.md:34` | OpenFrame and ParseStream are discarded, not preserved. | PASS |
| 1 Lock alignment | 1.2 | `PASS-2.md:196-253`; `14-LOCKS.md:42` | Lowerer import-deny gate keeps codegen on Backend IR. | PASS |
| 1 Lock alignment | 1.3 | `PASS-2.md:540-541`; `14-LOCKS.md:52` | Diagnostic strings mention forbidden `@pratt` and `@simd` directives. | FAIL-AMEND |
| 1 Lock alignment | 1.4 | `PASS-2.md:377-386`; `14-LOCKS.md:60` | YAML two-surface invariant is explicit. | PASS |
| 2 Patch applicability | 2.1 | Output-only V5 | Lane 2 is N/A because this worker must not patch target surfaces. | N/A |
| 3 Narrative cohesion | 3.1 | `PASS-2.md:32-36`; `PASS-3.md:416-429` | Codegen/runtime consumer contracts line up. | PASS |
| 3 Narrative cohesion | 3.2 | `ARCHITECTURE.md:1049-1081`; `PASS-1.md:183`; `PASS-1.md:196-217` | Core grammar sketch conflicts with settled host/lookbehind/chaining surfaces. | FAIL-AMEND |
| 3 Narrative cohesion | 3.3 | `MASTER-PLAN.md:123`; `MIGRATION.md:515-516`; `PASS-2.md:388-404` | Stale citations point future workers at wrong PASS-2 lines. | FAIL-AMEND |
| 4 SOTA/perf | 4.1 | `PASS-2.md:437-459`; `ARCHITECTURE.md:1254-1259` | Competitor gates carry platform and metadata requirements. | PASS |
| 4 SOTA/perf | 4.2 | `PASS-2.md:388-404`; `PASS-2.md:428-433` | Generated LOC and regen wall budgets are explicit. | PASS |
| 4 SOTA/perf | 4.3 | `PASS-2.md:81`; `restart/corpora/SOTA.md` | BIR cardinality proof is local-proxy based; acceptable only with research carry. | WARN |
| 5 Grammar authoring | 5.1 | `PASS-2.md:377-386`; `MASTER-PLAN.md:770` | New grammar onboarding has gates but lacks a narrative trace. | FAIL-AMEND |
| 5 Grammar authoring | 5.2 | `ARCHITECTURE.md:271-272`; `README.md:276-283` | Query macro examples disagree and omit a complete pointer/select path. | FAIL-AMEND |
| 5 Grammar authoring | 5.3 | `PASS-2.md:530-541`; `PASS-3.md:356-357` | PASS-3 has good misfire strings; PASS-2 has forbidden-hint strings. | FAIL-AMEND |
| 6 Generated surfaces | 6.1 | `PASS-2.md:130-151`; `PASS-2.md:473-488` | Template parameters and emitted runtime columns cover path, visitor, host, layout, error, SIMD. | PASS |
| 6 Generated surfaces | 6.2 | `PASS-2.md:410-422`; `ARCHITECTURE.md:1363-1375` | Non-generated LOC and child-count gates are carried. | PASS |
| 7 Diagnostics/cookbook | 7.1 | `PASS-2.md:530-541`; `PASS-3.md:352-366` | Ledger includes verbatim strings, but two strings teach invalid directives. | FAIL-AMEND |
| 7 Diagnostics/cookbook | 7.2 | `MASTER-PLAN.md:762-770` | Cookbook friction rows exist but are not backed by worked examples. | FAIL-AMEND |
| 8 Carry management | 8.1 | `PASS-2.md:543-554`; `MASTER-PLAN.md:737-757` | Receiver/blocker/receiving-gate form is present. | PASS |
| 8 Carry management | 8.2 | `PASS-2.md:431-433`; `HARDENING-PASS-2-V4.md:176-178` | BIR snapshot and yaml smoke baselines remain provisional but routed. | PASS-WITH-CARRY |
| 9 Greenfield/LLM risk | 9.1 | `PASS-2.md:5-7`; `README.md:450-452` | Replacement posture is direct and does not hedge old surfaces back in. | PASS |
| 9 Greenfield/LLM risk | 9.2 | `PASS-2.md:540-541`; `MASTER-PLAN.md:441-448` | LLM-friendly `@pratt`/`@simd` syntax is a high-risk training artifact. | FAIL-AMEND |

Lane summary:

Lane 2 is explicitly N/A.

The failing rows are narrow and document-only.

No row requires re-drafting PASS-2 wholesale.

The target amendment should be surgical.

## §5 16-Command Gate-Rerun

All commands below were run as read-only verification scans.

| # | Command pattern | Evidence | Result | Follow-up |
|---:|---|---|---|---|
| 1 | `rg ... "ParseStream|rewrite-mode|Unicode class algebra"` | Matches are deletion, stale-prompt, or routing contexts, including `PASS-2.md:5-7`. | PASS | None |
| 2 | `rg ... "bbnf-path|bbnf-test-fixtures|path!"` | Matches are legacy citations/deletion contexts; PASS-2 does not introduce `path!`. | PASS-WITH-LEGACY | Pointer/select example route |
| 3 | `rg ... "codegen/src/backend_ir"` | Only PASS-2 doc-only references at `PASS-2.md:196`, `233`, `248`. | PASS | None |
| 4 | `rg ... "fixtures/yaml"` | Fixture mentions are parity-phase or fixture-separation text, not onboarding inputs. | PASS | Add yaml trace |
| 5 | `rg ... "@recover"` | `ARCHITECTURE.md:1100` folds standalone `@recover` into `@error(recover = ...)`. | PASS | Add recover lowering example |
| 6 | `rg ... "OpenFrame"` | PASS-2 occurrences are deletion-pathology language at `PASS-2.md:7`, `36`, `455`, `561`. | PASS | None |
| 7 | `rg ... "GrammarIR"` | PASS-2 uses `GrammarIR` for violation context and deny-gate examples only. | PASS | None |
| 8 | `rg ... "__EAGER_EMPTY_PATH|CursorDecision::Skip"` | `PASS-2.md:176`, ARCH, MIGRATION, and MASTER bind empty-path/skip obligations. | PASS | None |
| 9 | `rg ... "twitter|canada|citm|bootstrap|animate|On-Demand"` | PASS-2/ARCH/MASTER/PASS-3 share SOTA rows with metadata. | PASS | None |
| 10 | `rg ... "Receiver|Blocker|Receiving gate"` | PASS-2 has the ledger at `PASS-2.md:543-554`. | PASS | Add trajectory pointer |
| 11 | `rg ... "yaml.bbnf|workspace.metadata.bbnf.grammars.yaml"` | PASS-2/ARCH/MASTER/MIGRATION bind yaml to two surfaces. | PASS | Add worked trace |
| 12 | `rg ... "generated_loc|Generated LOC|regen_wall|xtask|generated-loc-budget"` | Current authority is `PASS-2.md:388-404`; stale refs remain in ARCH/MASTER/MIGRATION. | FAIL-AMEND | P2V5-5 through P2V5-10 |
| 13 | `rg ... "BBNF-LIFE|BBNF-LAYOUT|BBNF-OPT|..."` | `PASS-2.md:540-541` mentions forbidden directives; `ARCHITECTURE.md:1049-1081` sketches conflicting grammar. | FAIL-AMEND | P2V5-1 through P2V5-4 |
| 14 | `rg ... "child count|child-count|500 LOC|exception rationale"` | PASS-2, ARCH, and MASTER carry child-count/LOC gates. | PASS | None |
| 15 | `rg ... "declaration-crate review|declaration_crate|..."` | ARCH, MASTER, and PASS-3 carry the fenced declaration-crate escape valve. | PASS | None |
| 16 | `rg ... "CPU model|compiler flags|input hash|..."` | ARCH and MASTER require CPU, flags, input hash, competitor version, warmup, and sample policy. | PASS | None |

Gate-rerun summary:

14 commands passed or passed with non-blocking legacy classification.

2 commands produced amendment-required evidence.

The amendment triggers are documentary, not implementation patches.

## §6 Cross-Document Binding Ledger To ARCH/MASTER/MIGRATION

| # | Topic | PASS-2 | ARCHITECTURE | MASTER | MIGRATION | Disposition |
|---:|---|---|---|---|---|---|
| 1 | Backend IR ownership | `PASS-2.md:196-253` | `ARCHITECTURE.md:870-903` | `MASTER-PLAN.md:343-372` | `MIGRATION.md:715` | Bound and coherent |
| 2 | Runtime template schema | `PASS-2.md:130-151` | `ARCHITECTURE.md:1210-1225` | `MASTER-PLAN.md:374-405` | `MIGRATION.md:515` | Bound; MIGRATION line stale |
| 3 | Generated LOC budget | `PASS-2.md:388-404` | `ARCHITECTURE.md:1247-1263`, `1320-1331` | `MASTER-PLAN.md:117`, `631-636` | `MIGRATION.md:515-516`, `740-742` | Budget coherent; citations stale |
| 4 | Layout lowering | `PASS-2.md:69`, `143` | `ARCHITECTURE.md:971-990` | `MASTER-PLAN.md:295`, `767` | Pipeline migration gates | Bound and coherent |
| 5 | Pratt/SIMD auto-detection | `PASS-2.md:153-170`, `540-541` | `ARCHITECTURE.md:1017-1018` | `MASTER-PLAN.md:441-448`, `768` | Generated output validation | Mechanism bound; diagnostics violate no-directive rule |
| 6 | Lookbehind | `PASS-2.md:77`, `170-176`, `539` | `ARCHITECTURE.md:1065`, `1016` | `MASTER-PLAN.md:327-337` | Validation/lowering gates | Diagnostic bound; grammar sketch conflicts |
| 7 | Block-bodied host and chaining | `PASS-2.md:72`, `141`, `484` | `ARCHITECTURE.md:1077-1081`, `1122-1134` | `MASTER-PLAN.md:327-331`, `459` | `MIGRATION.md:777` | Host route bound; grammar sketch stale |
| 8 | YAML onboarding | `PASS-2.md:377-386` | `ARCHITECTURE.md:1275-1281`, `1331` | `MASTER-PLAN.md:110`, `770` | `MIGRATION.md:750-757` | Two-surface invariant bound; trace absent |
| 9 | Pointer/select APIs | `PASS-2.md:347-358`, `473-488` | `ARCHITECTURE.md:271-272`, `1021-1023` | `MASTER-PLAN.md:764` | `MIGRATION.md:777` | API routed; example inconsistent |
| 10 | Error recovery and incremental parse | `PASS-2.md:78`, `144`, `147-149` | `ARCHITECTURE.md:1027`, `1100` | `MASTER-PLAN.md:473-502` | Runtime eager/skip gates | Bound at policy level; examples absent |
| 11 | Receiver/blocker/gate discipline | `PASS-2.md:543-554` | Stable authority/conflict ledger | `MASTER-PLAN.md:737-757` | `MIGRATION.md:766-783` | Bound and coherent |
| 12 | Benchmark metadata | `PASS-2.md:437-459` | `ARCHITECTURE.md:1254-1259` | `MASTER-PLAN.md:125-150`, `727` | Parity/generation gates | Bound and coherent |

## §7 Deduped Punch List

| Item | Path:line | Surgery | Acceptance gate | Lens origin |
|---|---|---|---|---|
| P2V5-1 | `restart/audit/pass-2-codegen/PASS-2.md:540` | Replace `BBNF-OPT001` so it never says `@pratt`; say Pratt is auto-detected and explain rejected reason/restructure path. | `rg -n "@pratt|@simd"` returns only Lock 10 prohibition or legacy-deletion contexts. | B4, E5, F1, G4, Lane 1.3 |
| P2V5-2 | `restart/audit/pass-2-codegen/PASS-2.md:541` | Replace `BBNF-OPT002` so it never says `@simd hint may force`; say SIMD is auto-selected or disabled by metadata policy. | PASS-2 agrees with `PASS-3.md:356-357` and Lock 10. | B4, E5, F1, G4 |
| P2V5-3 | `restart/ARCHITECTURE.md:1065` | Change core grammar sketch to PASS-1/README infix lookbehind or defer explicitly to PASS-1. | ARCH/PASS-1/PASS-2 show one compatible `|<` syntax. | A3, B6, H5 |
| P2V5-4 | `restart/ARCHITECTURE.md:1077` | Align `HostCall`, `Chain`, `MapExpr`, and `HostDecl` with block-bodied `@host fn` and `->` chaining. | Sketch matches `PASS-1.md:183`, `PASS-1.md:211`, `README.md:145-166`. | A4, B4, D2 |
| P2V5-5 | `restart/ARCHITECTURE.md:1248` | Update stale PASS-2 generated LOC citation to `PASS-2.md:388-404`. | `rg -n "PASS-2.md:293-310"` across ARCH/MASTER/MIGRATION/PASS-3 returns zero. | A5, H1 |
| P2V5-6 | `restart/MASTER-PLAN.md:123` | Update stale PASS-2 generated LOC citation to `PASS-2.md:388-404`. | Same as P2V5-5. | A5, H1 |
| P2V5-7 | `restart/MASTER-PLAN.md:631` | Update generated LOC authority citation to `PASS-2.md:388-404`. | Same as P2V5-5. | A5, H1 |
| P2V5-8 | `restart/MASTER-PLAN.md:635` | Update ARCH per-grammar generated LOC provenance to `ARCHITECTURE.md:1320-1331` or section-only citation. | `rg -n "1273-1281"` across restart authority docs returns zero. | H2 |
| P2V5-9 | `restart/MIGRATION.md:515` | Update runtime template schema citation to `PASS-2.md:130-151`. | `rg -n "PASS-2.md:98-116"` across ARCH/MASTER/MIGRATION returns zero. | H3 |
| P2V5-10 | `restart/MIGRATION.md:516` | Update generated LOC citation to `PASS-2.md:388-404`. | Same as P2V5-5. | A5, H1 |
| P2V5-11 | `restart/audit/pass-2-codegen/PASS-2.md:377` | Add or route yaml onboarding trace through metadata, BIR, runtime, path/visitor/host metadata, LOC budget, and post-onboarding parity. | Trace preserves `PASS-2.md:386` and never treats `fixtures/yaml/*` as onboarding input. | C1, E4 |
| P2V5-12 | `restart/MASTER-PLAN.md:764` | Add or route one query example using settled `pointer!` and `select!` macro forms with grammar qualification. | Example aligns with `README.md:276-283` and `PASS-3.md:359-361`. | B5, C2, G1 |
| P2V5-13 | `restart/audit/pass-2-codegen/PASS-2.md:147` | Bind `incremental_marker` to a minimal incremental parse example or explicit PASS-3 metadata route. | Route names PASS-3 reuse/fallback thresholds and what PASS-2 emits. | C3, D3, G2 |
| P2V5-14 | `restart/audit/pass-2-codegen/PASS-2.md:144` | Add `@error(recover)` lowering note for `error_policy`, recovery metadata, generic-rule typing, and PASS-3 consumption. | Cross-references README/ARCH/PASS-3 without reviving standalone `@recover`. | C4, D1 |
| P2V5-15 | `restart/audit/pass-2-codegen/PASS-2.md:106` | Add WASM host-primitives smoke row or carry route with scalar/SIMD parity and no declaration crate. | Binds `MASTER-PLAN.md:459`, `ARCHITECTURE.md:1026`, and `ARCHITECTURE.md:715-752`. | D2, G3 |
| P2V5-16 | `restart/audit/pass-2-codegen/PASS-2.md:543` | Add A-to-F-to-J grammar trajectory pointer while preserving receiver/blocker/receiving-gate columns. | Route names MASTER tranches A, F, G, H, I, J. | C5, F4 |

Deduping note:

P2V5-1 and P2V5-2 are separate because each diagnostic string can be patched independently.

P2V5-5 through P2V5-10 are grouped provenance repairs, but each path has its own line and acceptance gate.

P2V5-11 through P2V5-16 are example/coverage amendments and do not require PASS-2 re-draft.

## §8 V1-To-V4 History Note

V1:

`restart/audit/hardening/HARDENING-PASS-2.md` found nine PASS-2 hardening issues.

The center of gravity was BIR ownership, generated LOC, child-count, carry rows, diagnostics, and future-grammar proof.

V2:

`restart/audit/hardening/HARDENING-PASS-2-V2.md` returned READY after amendments.

That ready state was useful but not final because subsequent carry-aware passes reopened subtler seams.

V3:

`restart/audit/hardening/HARDENING-PASS-2-V3.md` reopened eight mandatory and one optional issue.

V3 specifically requested `BBNF-OPT001` and `BBNF-OPT002` diagnostic strings.

The V3 prescription seeded the current V5 problem because it asked for actionable strings without rechecking Lock 10's no-directive rule.

V4:

`restart/audit/hardening/HARDENING-PASS-2-V4.md` verified the V3 punch list and returned READY.

V4 treated the `@pratt` and `@simd` strings as hint surfaces rather than forbidden directive surfaces.

V5:

V5 keeps the V4 closure where it is correct.

V5 reopens only the drift visible after comparing PASS-2 to locks, ARCHITECTURE, MASTER, MIGRATION, PASS-1, PASS-3, and README.

The V5 verdict is therefore amendment-required, not re-draft.

## §9 LLM-Pathology Summary

LLM bias class 1:

Directive completion bias.

The model-friendly help text "`promote ... with @pratt`" and "`@simd hint may force`" looks useful but invents author controls that Lock 10 rejects.

The fix is not to soften the diagnostic.

The fix is to make the diagnostic more precise: auto-detected, cost-rejected, explainable, and disable-only where metadata allows.

LLM bias class 2:

Cross-document citation confidence.

Several stale line references survived because the surrounding concepts were still true.

This is dangerous for amendment workers because they may patch the wrong section or conclude a gate is absent.

The fix is exact provenance repair or section-only citations where line churn is likely.

Overfitting class 1:

Existing parser ecosystem examples pull the docs toward old path macro, dot-call, and directive-hint conventions.

The restart language deliberately rejects several of those familiar shapes.

Worked examples must teach the actual settled grammar instead of leaning on familiar syntax.

Overfitting class 2:

Incremental parsing and LSP fallback policy are plausible by analogy.

They still need one bbnf-specific emitted metadata example so the lowerer/runtime handoff is concrete.

Hallucination/provenance class 1:

The generated LOC authority moved to `PASS-2.md:388-404`.

ARCHITECTURE, MASTER, and MIGRATION still point to older PASS-2 ranges.

Hallucination/provenance class 2:

ARCHITECTURE's core grammar sketch is authority-shaped prose and therefore dangerous when stale.

Because it sits near the settled BBNF extension list, later workers may copy it as grammar truth.

The grammar sketch must either match PASS-1 or explicitly defer to PASS-1.

Positive anti-pathology findings:

PASS-2's SOTA/performance rows avoid claiming final benchmark wins before implementation.

PASS-2's carry ledger uses receivers, blockers, and receiving gates.

PASS-2's BIR ownership and import-deny gate are clear enough to resist direct Grammar IR lowerers.

## §10 Verdict

Verdict: AMENDMENT-REQUIRED.

Scope:

Narrow documentary amendment.

No PASS-2 re-draft is required.

No implementation patch is implied by this V5 report.

Blocking conditions:

1. `PASS-2.md:540-541` teaches forbidden `@pratt`/`@simd` directive syntax.
2. `ARCHITECTURE.md:1049-1081` conflicts with settled host, chain, map, and lookbehind grammar surfaces.
3. `ARCHITECTURE.md:1248`, `MASTER-PLAN.md:123`, `MASTER-PLAN.md:631`, and `MIGRATION.md:515-516` carry stale PASS-2 citations.
4. `MASTER-PLAN.md:635-636` carries stale ARCHITECTURE line provenance.
5. Worked examples remain insufficient for yaml onboarding, pointer/select queries, incremental parse, `@error(recover)`, and one A-to-F-to-J grammar path.

Ready conditions after amendment:

1. No `@pratt` or `@simd` appears as valid user syntax outside Lock 10 prohibition/deletion contexts.
2. ARCHITECTURE grammar sketch is reconciled with PASS-1 and README.
3. Generated LOC and runtime-template citations point to current target ranges.
4. The example gaps are either filled in PASS-2/MASTER/ARCH or routed to a named cookbook gate with acceptance tests.
5. The carry ledger remains intact after any added example text.

Verdict rationale:

PASS-2's architecture is viable.

The amendment requirement exists because the remaining faults are high-leverage documentation traps.

They would mislead implementers before they would break tests.

That is exactly the class V5 is meant to catch.

## §11 Closing Posture

Closing posture:

Stop expansion here.

Patch none of the target surfaces in this worker.

Stage and commit only `restart/audit/hardening/HARDENING-PASS-2-V5.md`.

Recommended amendment wall-time:

90 to 140 minutes.

Estimate basis:

20 minutes for PASS-2 diagnostic string surgery and verification.

25 minutes for ARCHITECTURE grammar sketch reconciliation.

20 minutes for stale citation repair across ARCHITECTURE, MASTER, and MIGRATION.

35 to 75 minutes for compact worked-example routing, depending on whether examples land inline or as cookbook pointers.

Risk if skipped:

Future workers may implement forbidden recognizer directives.

Future workers may copy a stale host/lookbehind grammar sketch.

Future workers may audit generated LOC against the wrong PASS-2 lines.

Final posture:

AMENDMENT-REQUIRED with bounded surgery.

No re-draft.

No code change.

No carry reset.
