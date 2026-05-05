# HARDENING-CONSOLIDATED-V5

## §1 Target identifications

Phase 0 V5 metahardening audited the V4-READY restart corpus with carry-aware
lenses A-E and LLM-pathology lenses F-H. The cohort did not re-run the old V1
through V4 punch-list frame. It asked whether the post-V4 documents still bind
as one corpus when formal fragments, examples, citations, and user-facing
diagnostic strings are treated as contracts.

| Target | Target output audited | Hardening report | Hardening commit | Verdict | Report size | Primary V5 finding |
|---|---|---|---|---|---:|---|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` | `restart/audit/hardening/HARDENING-PASS-1-V5.md` | `5dc4e011` | AMENDMENT-REQUIRED | 471 lines | PASS-1 remains internally strong, but Architecture §8.1 contradicts PASS-1 §6 on `@host fn`, lookbehind, and chain/map syntax. |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` | `restart/audit/hardening/HARDENING-PASS-2-V5.md` | `67f42db6` | AMENDMENT-REQUIRED | 419 lines | PASS-2 diagnostic strings reintroduce `@pratt` and `@simd` as user controls that Lock 10 rejects. |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` | `restart/audit/hardening/HARDENING-PASS-3-V5.md` | `6f647033` | AMENDMENT-REQUIRED | 432 lines | PASS-3 has correct runtime shape but carries stale citations, weak debug/DAP wording, and sparse worked examples. |
| MASTER-PLAN trio | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | `restart/audit/hardening/HARDENING-MASTER-PLAN-V5.md` | `8cae7c5e` | AMENDMENT-REQUIRED | 419 lines | The trio is structurally coherent, but has hard source faults in Architecture grammar, yaml table shape, and lock-line citations. |

| Cohort | Reports | READY | AMENDMENT-REQUIRED | RE-DRAFT | Final verdict |
|---|---:|---:|---:|---:|---|
| V5 metahardening | 4 | 0 | 4 | 0 | **AMENDMENT-REQUIRED** |

The V5 verdict is not a rollback of V4's structural closure. V4 correctly
closed broad conflicts around Backend IR ownership, `pointer!`, path crates,
layout vocabulary, generated budgets, OpenFrame deletion, and yaml's two
onboarding surfaces. V5 found the smaller class that V4 did not center:
formal grammar fragments that lagged behind prose, diagnostic help strings
that taught retired syntax, line citations that no longer point to the named
authority, and high-value user flows represented only as tables.

### Report integrity check

| Check | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cohort result |
|---|---|---|---|---|---|
| Required reading declared | Yes. | Yes. | Yes. | Yes. | PASS. |
| A-E carry-aware lens table | Present with >=15 rows. | Present with >=15 rows. | Present with 30 rows. | Present with >=15 rows. | PASS. |
| F-H LLM-pathology lens table | Present with >=9 rows. | Present with >=9 rows. | Present with 15 rows. | Present with >=9 rows. | PASS. |
| Compressed lane verification | Present; Lane 2 N/A. | Present; Lane 2 N/A. | Present; Lane 2 N/A. | Present; Lane 2 full. | PASS. |
| 16-command gate-rerun | Present. | Present. | Present. | Present. | PASS. |
| Cross-document ledger | Present. | Present. | Present. | Present. | PASS. |
| Deduped punch list | Present. | Present. | Present. | Present. | PASS. |
| V1->V4 history note | Present. | Present. | Present. | Present. | PASS. |
| LLM-pathology summary | Present. | Present. | Present. | Present. | PASS. |
| Verdict | AMENDMENT-REQUIRED. | AMENDMENT-REQUIRED. | AMENDMENT-REQUIRED. | AMENDMENT-REQUIRED. | AMENDMENT-REQUIRED. |
| Commit discipline | Worker reports staged only own report. | Worker reports worktree clean after commit. | Worker reports staged only own report. | Worker reports own file only. | PASS. |

### V1->V5 readiness trajectory

| Cycle | Cohort verdict | Main issue class | What changed |
|---|---|---|---|
| V1 | AMENDMENT-REQUIRED | Broad conflicts: BIR ownership, path naming, layout, yaml, SOTA, OpenFrame, fixtures. | Produced the first consolidated amendment queue. |
| V2 | READY | Residual phrasing only. | Verified broad amendment closure with high KEEP rate. |
| V3 | AMENDMENT-REQUIRED | Independent parallel audit reopened cross-document precision and baseline anchoring. | Routed a 24-item narrow amendment set. |
| V4 | READY | V3 punch list closed. | Confirmed the corpus could advance to tranche drafting under the old audit frame. |
| V5 | AMENDMENT-REQUIRED | Formal-fragment drift, diagnostic-string drift, stale citations, example scarcity. | Requires Phase 0.5 before research. |

The trajectory matters because V5's defects are not the same class as V1. V1
found architecture still unsettled. V5 finds an otherwise settled architecture
with enough high-authority stale fragments to misdirect implementation.

## §2 Cohort verdict by lane

| Lane | PASS-1 V5 | PASS-2 V5 | PASS-3 V5 | MASTER-PLAN V5 | Cumulative |
|---|---|---|---|---|---|
| 1 Lock-Adherence | AMEND: Architecture grammar conflicts with PASS-1 and Lock 10 diagnostic drift is visible. | AMEND: `@pratt`/`@simd` strings violate Lock 10 despite BIR ownership holding. | AMEND: wrong Lock 8 citation and stale lowerer crate prefix. | AMEND: Architecture §8.1 and yaml table/citation faults. | AMENDMENT-REQUIRED; locks survive, contract fragments drift. |
| 2 Sequencing | N/A. | N/A. | N/A. | AMEND: tranche order holds, but worked A->F->J trajectory is missing. | AMENDMENT-REQUIRED for trio only. |
| 3 Cohesion | AMEND: Architecture §8.1 must bind to PASS-1 §6. | AMEND: stale generated LOC and runtime-template references weaken evidence. | AMEND: README stale prose keeps retired concepts alive. | AMEND: trio binds broadly but formal examples lag. | AMENDMENT-REQUIRED. |
| 4 SOTA-Anchoring | KEEP with citation polish. | KEEP with provenance polish. | AMEND: SOTA rule cites Lock 14 instead of Lock 8. | AMEND: `json/canada` competitor set differs between Architecture and Master. | AMENDMENT-REQUIRED, not SOTA rework. |
| 5 Grammar-Authoritative | AMEND: yaml proof is tabular, not worked. | AMEND: yaml BIR/runtime trace missing. | AMEND: yaml and query examples sparse. | AMEND: yaml authority row malformed. | AMENDMENT-REQUIRED; two-surface rule remains settled. |
| 6 Generated-Code Budget | AMEND: stale Architecture baseline line citation. | AMEND: stale PASS-2 generated LOC citations. | KEEP with receiving gate. | AMEND: yaml row and PASS-2 line refs. | AMENDMENT-REQUIRED through provenance/table shape. |
| 7 Friction Forecast | AMEND: query, incremental, recovery examples absent. | AMEND: diagnostics teach retired recognizer syntax. | AMEND: pointer/select and incremental recovery examples absent. | AMEND: cookbook rows need compact worked examples. | AMENDMENT-REQUIRED. |
| 8 Carry & Deferral | AMEND: PASS-1 reconciliation row omits Architecture §8.1. | AMEND: example routes and Lock 11 non-owner need explicit receiver gates. | AMEND: WASM host example must route to H.W3 without invented numbers. | AMEND: WASM ABI matrix and trajectory route needed. | AMENDMENT-REQUIRED. |
| 9 Greenfield Discipline | KEEP: PASS-1 remains substrate source. | AMEND: directive completion bias would mislead implementers. | AMEND: stale README wording reopens retired concepts by prose. | AMEND: formal snippets must match settled architecture. | AMENDMENT-REQUIRED; no redraft trigger. |

V5 confirms the greenfield architecture. Tape/direct, Backend IR ownership,
layout vocabulary, generic crate boundaries, path crate names, and two-surface
yaml onboarding remain intact. The amendment is documentary and gate-surface
surgery. It does not redesign tranches, locks, IRs, runtime substrate, or
migration topology.

### Gate-rerun cohort summary

The four reports reran the tightened 16-command gate set in target-specific
form. The command set remains mostly healthy, but four gates now carry V5
attention:

| Gate family | Cohort result | Source reports | Consolidated disposition |
|---|---|---|---|
| Retired surface scan (`ParseStream`, rewrite-mode, Unicode class algebra) | Mostly PASS, but README has positive stale lines. | PASS-1 §3/§7; PASS-3 §2/§7. | Patch README stale-positive prose or explicitly mark it archaeology. |
| Recognizer directive scan (`@pratt`, `@simd`) | FAIL-NARROW in PASS-2 diagnostics. | PASS-1 §2/§7; PASS-2 §2/§7; PASS-3 §2/§7. | Rewrite `BBNF-OPT001` and `BBNF-OPT002`; no user-forced directives. |
| `fixtures/yaml` / yaml authority shape | PARTIAL: two-surface rule holds, but Architecture yaml row is malformed. | PASS-3 §2/§7; MASTER-PLAN §5/§7. | Restore yaml row cell count and host route; add worked yaml flow. |
| Citation/provenance scans | Several wrong-line citations. | PASS-1 §3/§7; PASS-2 §3/§7; PASS-3 §3/§7; MASTER-PLAN §3/§7. | Repair specific lock/PASS/Architecture line references or cite sections. |

### Lens-level synthesis

| Lens | Dominant V5 result | Blocking? | Consolidated action |
|---|---|---:|---|
| A narrative coherence | Trio/PASS surfaces mostly bind, but Architecture §8.1 contradicts PASS-1 grammar and stale citations break provenance. | Yes. | Bundle 1 and bundle 3. |
| B vocabulary drift | Layout, BIR, tape, path, and OpenFrame vocabulary are stable; recognizer directives and lookbehind aliases drift. | Yes. | Bundle 2 and bundle 9. |
| C worked-example scarcity | All four targets report missing walkthroughs for yaml, pointer/select, incremental recovery, `@error(recover)`, and one A->F->J grammar path. | Yes. | Bundles 4, 5, 6, and 8. |
| D coverage gaps | Ergonomic onboarding, debug/DAP identity, generic recovery typing, and WASM host primitive route need concrete gates. | Yes. | Bundles 6, 7, and 9. |
| E cumulative lock consistency | Locks hold cumulatively; the violations are in contract expression, not architecture. | Yes, narrow. | Bundle 1 and bundle 2; no lock revision. |
| F LLM bias | Plausible compiler-looking snippets and directive help text survived because they sound useful. | Yes. | Replace snippets/diagnostics with settled forms. |
| G overfitting | Tables prove invariants but do not prove unfamiliar author workflows; JSON/CSS hot fixtures dominate examples. | Yes. | Add yaml/non-hot trajectory and query walkthrough. |
| H hallucination/provenance | No confabulated lock change; wrong-line citations and alias chains are the issue. | Yes. | Repair citations and alias ledgers. |

### Cohort KEEP / AMEND / RE-DRAFT posture by architecture area

| Area | V5 status | Notes |
|---|---|---|
| Runtime substrate | KEEP | Tape/direct and `TapeBuilder` checkpoints hold. |
| Direct-to-struct union | KEEP | No report asks to split substrate. |
| Backend IR boundary | KEEP | `ir::backend_ir` ownership holds. |
| Codegen lowerer input | KEEP | Grammar IR import-deny remains sound. |
| BBNF formal grammar sketch | AMEND | Architecture §8.1 is stale. |
| Host function surface | AMEND | Formal grammar says declarations while prose says definitions. |
| Pratt/SIMD author surface | AMEND | PASS-2 diagnostics mention forbidden directive controls. |
| Path/query user surface | AMEND | Names hold; examples and canonical grammar-prefix syntax need binding. |
| YAML onboarding | AMEND | Two-surface rule holds; table row and walkthrough need repair. |
| Incremental recovery/LSP | AMEND | Thresholds exist; worked edit path missing. |
| WASM host primitive surface | AMEND | H.W3 route exists; ABI matrix needed. |
| SOTA gates | AMEND-NARROW | Numeric gates hold; canada comparator and citation parity need correction. |
| Generated LOC budgets | AMEND-NARROW | Budget shape holds; stale line references need correction. |
| Lock 11 incubation | AMEND-NARROW | Policy holds; citation/ownership row polish. |
| Migration topology | KEEP | No report contests tranche or file-disposition shape. |

## §3 Cross-target conflicts and agreements

| Conflict | Sources | Per-target verdicts | Resolution recommendation |
|---|---|---|---|
| Architecture §8.1 formal grammar vs PASS-1 §6 | PASS-1 V5 cites `PASS-1.md:176-217` vs `ARCHITECTURE.md:1049-1081`; PASS-2 V5 cites `ARCHITECTURE.md:1065`, `1077-1081`; MASTER V5 cites `ARCHITECTURE.md:1079`, `1081`. | PASS-1 AMEND; PASS-2 AMEND; MASTER AMEND. PASS-3 indirectly depends through diagnostics. | Rewrite Architecture grammar sketch to match PASS-1 or make it non-normative with explicit PASS-1 authority. Block-bodied `@host fn`, infix lookbehind, and `->` chains are the surviving form. |
| Recognizer diagnostics revive retired syntax | PASS-2 target lines `PASS-2.md:540-541`; PASS-1 V5 F3/E3; PASS-2 V5 B4/E5; PASS-3 V5 item 5. | PASS-2 AMEND; PASS-1/PASS-3 route cross-pass; MASTER keeps Lock 10. | Replace user-facing `@pratt` and `@simd` remediation with automatic detection, cost evidence, grammar restructuring, and metadata disable-only language. |
| README stale-positive close prose | PASS-1 V5 F4; PASS-3 V5 item 1 cites `README.md:391` and `README.md:473`. | PASS-3 AMEND; PASS-1 AMEND; PASS-2/MASTER not primary. | Patch README only if amendment scope allows the prior Wave-4.1 README exception pattern; otherwise route as explicit archaeology before V5.1. |
| Generated LOC and template provenance | PASS-2 V5 items P2V5-5 through P2V5-10; PASS-1 V5 items 4-5. | PASS-1 AMEND; PASS-2 AMEND; MASTER AMEND. | Update stale `PASS-2.md:293-310`, `PASS-2.md:98-116`, and `ARCHITECTURE.md:1273-1281` citations to current ranges or section citations. |
| PASS-3 diagnostic provenance | PASS-3 V5 items 2-4; target lines `PASS-3.md:365`, `369`, `375`. | PASS-3 AMEND. | Cite Lock 8 for SOTA rows, replace stale `bbnf_ir::` prefixes, and bind `BBNF1004` / `BBNF-LOOKBEHIND-WIDTH` / `LookbehindWidth`. |
| YAML authority and onboarding | PASS-1 V5 C1/D1; PASS-2 V5 C1/E4; PASS-3 V5 item 6/10; MASTER V5 P5/P10. | All four AMEND. | Fix Architecture yaml table row and add one two-surface onboarding walkthrough from `yaml.bbnf` + metadata to generated runtime/path/visitor outputs, with fixtures deferred to parity. |
| Worked examples | PASS-1 V5 items 7-11; PASS-2 V5 items 11-16; PASS-3 V5 items 6-8/11; MASTER V5 P9-P14. | All four AMEND. | Add compact examples or routed cookbook gates for yaml onboarding, pointer/select query, incremental recovery, `@error(recover)`, WASM host primitive, and one A->F->J grammar trajectory. |
| Debug/DAP identity | PASS-1 V5 F2/D3; PASS-3 V5 item 9. | PASS-1 AMEND by route; PASS-3 AMEND. | Change advisory "should reuse this identity" to mandatory tape snapshot identity with an acceptance gate. |

### Closed agreements reaffirmed by V5

| Agreement | Evidence | V5 disposition |
|---|---|---|
| Tape remains the substrate unioned with direct-to-struct. | PASS-1 V5 E1, PASS-3 V5 A2/B3, MASTER V5 E1. | KEEP. |
| OpenFrame is deletion archaeology only. | PASS-1 V5 B3/E1, PASS-3 V5 B6. | KEEP. |
| Backend IR lives in `ir::backend_ir`; codegen consumes BIR only. | PASS-2 V5 A1/B2/E2, MASTER V5 B2. | KEEP. |
| `LayoutFacts` / `passes::layout` is the public layout vocabulary; `TypeFacts` is internal. | PASS-1 V5 B1/E6, PASS-2 V5 B1, MASTER V5 B1. | KEEP. |
| Public path surface is `pointer!`, `select!`, `path`, `path-core`, `path-ts`. | PASS-1 V5 E5, PASS-3 V5 A3/B4, MASTER V5 B3. | KEEP, with example syntax clarification. |
| Lock 11 incubation does not force premature sister-crate publication. | PASS-1 V5 D7, PASS-2 V5 D4, MASTER V5 D6/E4. | KEEP with citation polish. |

## §4 Punch list consolidation

The four reports carry many local rows, but they collapse to nine amendment
bundles. That keeps Phase 0.5 inside the orchestrator's <=10 narrow-item rule.
Items are ordered by readiness impact.

| # | Bundle | Target paths | Consolidated surgery | Acceptance gate | Route |
|---:|---|---|---|---|---|
| 1 | Formal BBNF grammar reconciliation | `restart/ARCHITECTURE.md:1049-1081`; `restart/MASTER-PLAN.md:329`; `restart/MASTER-PLAN.md:748` | Replace declaration-only `HostDecl`, rule-level `=>`, prefix-shaped lookbehind, and rule-level method-chain ambiguity with PASS-1-bound productions, or mark Architecture §8.1 non-normative and bind to PASS-1 §6. Update Master D.W2 and PASS-1 reconciliation row to say block-bodied `@host fn` definitions and Architecture §8.1 equivalence. | `rg -n 'HostDecl.*;|=> TypeExpr|MapExpr|Lookbehind    ::= "\\|<" Suffix' restart/ARCHITECTURE.md` returns zero except rejection/non-normative context; Master mentions PASS-1 §6 / Architecture §8.1 reconciliation. | SYNTHESIS amendment. |
| 2 | Recognizer diagnostic wording | `restart/audit/pass-2-codegen/PASS-2.md:540-541`; mirrored by PASS-3 diagnostic policy. | Rewrite `BBNF-OPT001` and `BBNF-OPT002` to explain automatic Pratt/SIMD detection, cost-model rejection, and grammar/metadata remedies without exposing `@pratt` or user-forced `@simd`. | `rg -n '@pratt|@simd' restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md` returns only Lock 10 prohibition or deletion context. | PASS-2 amendment. |
| 3 | Stale-positive README and stale citations | `restart/README.md:391`, `restart/README.md:473`; `ARCHITECTURE.md:1000`, `1248`; `MASTER-PLAN.md:123`, `524`, `631`, `635-636`; `MIGRATION.md:515-516`; `PASS-2.md:174`; `PASS-3.md:375`. | Replace positive `ParseStream` / rewrite-mode / Unicode-bundle README prose with settled vocabulary or archaeology. Repair wrong Lock 14/Lock 11/Lock 8 citations and stale PASS-2/Architecture line ranges. | Scans for `ParseStream` and `rewrite-mode + lookbehind + Unicode sets` in README have no settled-state hits; stale line refs `PASS-2.md:293-310`, `PASS-2.md:98-116`, `ARCHITECTURE.md:1273-1281`, `14-LOCKS.md:60` for Lock 11, and `14-LOCKS.md:69-72` for Lock 14 are gone or section-scoped. | SYNTHESIS amendment plus PASS-local citation fixes. |
| 4 | YAML table and onboarding walkthrough | `restart/ARCHITECTURE.md:1275-1331`; `restart/MASTER-PLAN.md:770`; PASS-1/PASS-2/PASS-3 yaml rows. | Insert the missing yaml host-route cell, then add a compact two-surface yaml walkthrough: `grammars/yaml.bbnf`, `[workspace.metadata.bbnf.grammars.yaml]`, generated runtime/path/visitor/host outputs, LOC budget, zero Rust registry edits, parity fixture deferred. | `awk -F'|' 'NR>=1320 && NR<=1331 { print NR, NF-1 }' restart/ARCHITECTURE.md` reports a consistent cell count; onboarding example forbids `fixtures/yaml` as source authority. | SYNTHESIS amendment, PASS-3 receiver note if needed. |
| 5 | Pointer/select worked query | `restart/ARCHITECTURE.md:270-272`; `restart/MASTER-PLAN.md:764`; `restart/audit/pass-3-runtime/PASS-3.md:80-103`. | Add or route one example with canonical grammar-qualified `pointer!` and one `select!` structural query, including one success and one `BBNF-POINTER` diagnostic failure. | Example syntax agrees with README and PASS-3 diagnostics; `BBNF-POINTER001/002/003` remain referenced. | PASS-3 amendment, with SYNTHESIS route if example lives in Master. |
| 6 | Incremental + recovery + debug identity | `restart/audit/pass-3-runtime/PASS-3.md:156`, `160-190`; `restart/MASTER-PLAN.md:490-501`; `restart/ARCHITECTURE.md:1027`. | Add one malformed-edit walkthrough covering `@error(recover = ...)`, changed span, recovery node, fallback accounting, LSP quiet policy, and mandatory debug/DAP tape snapshot identity. | `rg -n 'should reuse this identity' restart/audit/pass-3-runtime/PASS-3.md` returns zero; example names dirty range, fallback reason, recovery diagnostic, and LSP policy. | PASS-3 amendment. |
| 7 | WASM host primitive route | `restart/audit/pass-2-codegen/PASS-2.md:106`; `restart/audit/pass-3-runtime/PASS-3.md:368`; `restart/MASTER-PLAN.md:459`, `753`. | Add a WASM host primitive ABI matrix gate with exported function names, host-call shape, marshalling rule, primitive coverage, scalar/SIMD parity, and no invented `{N}`/`{M}` measurements. | H.W3 fails if exported names or host-call shape rows are missing; PASS-3 routes numbers to H owner. | SYNTHESIS amendment plus PASS-2/PASS-3 receiver notes. |
| 8 | One grammar A->F->J trajectory | `restart/MASTER-PLAN.md:208-537`; `restart/audit/pass-2-codegen/PASS-2.md:543`; `restart/audit/pass-3-runtime/PASS-3.md:414-429`. | Add one trajectory for a single grammar, preferably yaml or json, through A skeleton/metadata, F generated runtime, G path/visitor surface, H optional perf, I recovery/LSP, and J parity/publication/docs close. | `rg -n 'A->F->J|A to F to J|grammar trajectory|yaml trajectory|json trajectory' restart/MASTER-PLAN.md restart/ARCHITECTURE.md` finds the path and receiving gates. | SYNTHESIS amendment. |
| 9 | Rare escape fence and diagnostic alias polish | `restart/audit/pass-1-substrate/PASS-1.md:81-91`; `restart/audit/pass-3-runtime/PASS-3.md:365`, `369`; `restart/MASTER-PLAN.md:677`. | Bridge PASS-1's rare declaration-crate fence to Architecture's eight-field form; bind `BBNF1004` / `BBNF-LOOKBEHIND-WIDTH` / `LookbehindWidth`; replace stale `bbnf_ir::` diagnostic prefix with current `ir::` or Architecture vocabulary; optionally expand Lock 11 owner row. | PASS-1 names `deletion path` and `reviewer` or cites the eight-field Architecture form; PASS-3 has no `bbnf_ir::` hits and names lookbehind aliases together. | PASS-1/PASS-3 amendment. |

### Amendment routing matrix

| Route | Primary write surface | Bundles | Notes |
|---|---|---|---|
| SYNTHESIS narrow | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md`, with README stale-line exception if accepted | 1, 3, 4, 7, 8 | This route owns high-authority formal snippets, trio examples, yaml table, cross-doc citations, WASM ABI gate, and grammar trajectory. |
| PASS-2/PASS-1 narrow | `restart/audit/pass-2-codegen/PASS-2.md`; `restart/audit/pass-1-substrate/PASS-1.md` | 2, 3, 4, 7, 9 | This route owns recognizer diagnostic strings, PASS-2 line refs, incremental marker/recovery lowering notes, WASM receiver row, and PASS-1 rare-fence bridge. |
| PASS-3 narrow | `restart/audit/pass-3-runtime/PASS-3.md` | 3, 5, 6, 7, 9 | This route owns Lock 8 citation, lowerer diagnostic prefix, lookbehind alias row, examples, mandatory debug/DAP identity, and H.W3 WASM route. |

The route split preserves non-overlapping write surfaces. README is listed only
as an explicit stale-line exception because V5 found authority prose outside
the trio/PASS surfaces; the amendment prompt must either allow that exception
or leave the row explicitly routed as unresolved before V5.1.

### Local report row mapping

The table below records how report-local punch rows collapse into the nine
consolidated bundles. It is the routing guard for Phase 0.5; amendment agents
should not rediscover their own bundle map from scratch.

| Consolidated bundle | PASS-1 local rows | PASS-2 local rows | PASS-3 local rows | MASTER local rows |
|---|---|---|---|---|
| 1 Formal BBNF grammar reconciliation | Items 1-2; A1/A2/B4/B5/B6/F1/G1. | P2V5-3, P2V5-4; A3/A4. | Indirect through diagnostic policy and `@error` examples. | P1, P2, P3, P4; A2/A3/B5/F3/F4. |
| 2 Recognizer diagnostic wording | Item 3; B7/E3/F3. | P2V5-1, P2V5-2; B4/E5/F1/G4. | Item 5; E2/F3/H2. | Reinforced by Lock 10 keep rows. |
| 3 README and stale citations | Items 4, 5, 12; F4/H1/H2. | P2V5-5 through P2V5-10. | Items 1, 2, 3; A1/A4/B2/H1/H3. | P6, P7, P8 plus citation findings H1-H3. |
| 4 YAML table and onboarding | Item 7; C1/D1/G2. | P2V5-11; C1/E4. | Items 6 and 10; C1/D1/G5/A6/E3. | P5 and P10; A4/C1/D1/G1. |
| 5 Pointer/select worked query | Item 8; C2. | P2V5-12; B5/C2/G1. | Item 7; C2/D1/G1. | P9; B4/C2/H4. |
| 6 Incremental/recovery/debug | Items 9-10; C3/C4/D2/D3/F2/G3. | P2V5-13, P2V5-14; C3/C4/D1/D3/G2. | Items 8-9; C3/C4/D2/F1/G2/G3. | P11, P12; C3/C4/D2/D4. |
| 7 WASM host primitive route | D5. | P2V5-15; D2/G3. | Item 12; D5/H5. | P13; D5. |
| 8 One grammar trajectory | Item 11; C5. | P2V5-16; C5/F4. | Item 11; C5/E4. | P14; C5/G4. |
| 9 Rare fence and alias polish | Item 6; H3/D1. | H5 alias drift; D4 Lock 11 non-owner. | Items 3-4; B2/B5/E5/H4. | P15 optional plus H4 public macro canonicalization. |

### Bundle acceptance order

| Order | Bundle | Why first/last |
|---:|---|---|
| 1 | Formal BBNF grammar reconciliation | Other examples must not copy stale syntax. |
| 2 | Recognizer diagnostic wording | Diagnostics are executable language surface for grammar authors. |
| 3 | Stale citations/README | Later agents route by these anchors. |
| 4 | YAML table/onboarding | Lock 14 proof depends on table shape before examples. |
| 5 | PASS-3 diagnostic alias/prefix polish | Keeps error vocabulary coherent before examples cite it. |
| 6 | Pointer/select example | Depends on canonical macro syntax and diagnostics. |
| 7 | Incremental/recovery/debug example | Depends on corrected `@error` and diagnostics. |
| 8 | WASM host primitive route | Can stay routed to H.W3 with no invented numbers. |
| 9 | One grammar trajectory | Consolidates the repaired surfaces into a single path. |

### Route-specific stop conditions

| Route | Stop condition |
|---|---|
| SYNTHESIS narrow | Stop if fixing Architecture §8.1 implies changing PASS-1's grammar contract or a settled lock. Route to user as RE-DRAFT-class conflict instead of inventing a compromise. |
| PASS-2/PASS-1 narrow | Stop if removing `@pratt`/`@simd` requires adding a new metadata force directive. Lock 10 permits auto-detection and disabling policy, not author-forced directives. |
| PASS-3 narrow | Stop if examples require new runtime API not already present in PASS-3/Architecture. Examples must demonstrate existing commitments, not add API. |

## §5 Final readiness verdict

**AMENDMENT-REQUIRED**.

No target returned READY. No target returned RE-DRAFT. The cumulative issue
set is narrow enough for a single Phase 0.5 amendment cycle because the local
rows dedupe to nine bundles and all bundles are documentary or gate-surface
repairs.

### Why not READY

The V5 cohort cannot advance to research deep-dives while high-authority docs
teach stale syntax or point at wrong evidence:

- `restart/ARCHITECTURE.md:1049-1081` currently looks normative and conflicts
  with PASS-1 §6.
- `restart/audit/pass-2-codegen/PASS-2.md:540-541` currently teaches
  non-existent `@pratt` and user-forced `@simd` surfaces.
- `restart/ARCHITECTURE.md:1331` currently drops the yaml host-route cell.
- README and several line citations still preserve stale authority cues.
- The corpus still relies on tables where one compact worked example is the
  right proof for grammar onboarding, queries, incremental recovery, and
  tranche progression.

### Why not RE-DRAFT

V5 did not surface a structural lock contradiction:

- Tape/direct remains intact.
- Backend IR ownership remains intact.
- Layout vocabulary remains intact.
- Lock 14's two-surface rule remains intact.
- Lock 10 remains intact in the trio; only one PASS-2 diagnostic text violates
  it.
- The tranche plan remains coherent.
- Migration routing remains coherent.
- SOTA gates remain numeric and competitor-bound; only provenance parity needs
  correction.

The V5 result is therefore **AMENDMENT-REQUIRED with <=10 narrow bundles**.
Per `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` §2, Phase 0.5 should run
one narrow amendment cycle, then a V5.1 verification cycle before Phase 1
research dispatch.

### Re-draft threshold check

| Threshold | Present after V5? | Evidence |
|---|---|---|
| Tape/direct union replaced by another substrate | No. | PASS-1, PASS-3, and MASTER all keep tape/direct. |
| Backend IR remains owned by codegen | No. | PASS-2 and MASTER keep `ir::backend_ir`; codegen README is documentation-only. |
| YAML onboarding requires third source surface | No. | Two-surface rule holds; table/example proof needs repair. |
| SOTA close permits unresolved misses | No. | Numeric gates remain; provenance parity needs correction. |
| Tranche sequence consumes later-wave artifact | No structural fault. | MASTER V5 asks for one trajectory, not tranche reorder. |
| Generated budgets absent from F/H/J | No. | Budgets exist; stale line references need repair. |
| Carry ledgers lack receiver/blocker/gate broadly | No. | V5 asks for examples and a few receiver gates, not ledger rebuild. |
| Public API revives `path!` or prefixed path crates | No. | `pointer!`, `select!`, `path`, `path-core`, `path-ts` remain settled. |
| Standalone `@recover`, grammar rewrite-mode, or grammar Unicode algebra returns | No. | README stale prose is the risk; grammar surfaces reject these. |
| OpenFrame preservation returns | No. | All reports keep OpenFrame as deletion archaeology. |

### Phase 0.5 success criteria

| Criterion | Required evidence before V5.1 |
|---|---|
| Architecture grammar reconciled | Architecture §8.1 matches PASS-1 §6 or is explicitly non-normative with PASS-1 as authority. |
| Recognizer directives removed | `@pratt` and `@simd` appear only in prohibition/deletion contexts. |
| README stale prose resolved | README no longer presents `ParseStream`, rewrite-mode, or Unicode set algebra as settled. |
| Citation drift repaired | Stale line references named in bundle 3 are gone or section-scoped. |
| YAML row fixed | Architecture per-grammar table has consistent cell count and yaml host route. |
| YAML walkthrough present | Example has exactly two input surfaces and generated-only output changes. |
| Pointer/select example present | Example uses canonical macro grammar-prefix syntax and one diagnostic failure. |
| Incremental/recovery example present | Example ties malformed edit to recovery node, fallback accounting, and LSP policy. |
| Debug/DAP identity hardened | PASS-3 or Master uses mandatory wording and a receiving gate. |
| WASM host route explicit | H.W3 has host primitive ABI matrix gate without invented measurements. |
| One grammar trajectory present | One named grammar crosses A/F/G/H/I/J or a subset with explicit receiving gates. |
| No new architecture introduced | Diff remains documentary/gate-level; no lock or tranche redesign. |

## §6 Voice and discipline locks

The four V5 reports follow the restart voice sufficiently for hardening output:
direct faults, path:line citations, no broad redesign, and no implementation
patching. The reports also reveal a process lesson for the corpus:

| Discipline | V5 observation | Required posture |
|---|---|---|
| Path:line citations | Wrong-line citations survived because concepts remained true. | Treat line citations as evidence, not decoration; repair or cite sections where line churn is likely. |
| No hedged contracts | PASS-3's "should reuse this identity" weakens debug/DAP identity. | Runtime identity obligations use "must" or name a receiving gate. |
| No retired syntax as help text | `@pratt` and `@simd` came back through diagnostics, not through grammar prose. | Diagnostic strings are language surface; audit them as grammar contracts. |
| Per-X tables | The yaml table exists but one row malformed. | Table shape is a gate; parse it when it is the authority. |
| Worked examples | Tables closed V4 but did not teach unfamiliar author flows. | Add compact examples where a worker or user must operate the surface. |
| Archaic-permissive voice | "Hereupon" and similar terms are acceptable. | Ornament cannot substitute for commitment or evidence. |
| Scope control | V5 findings are bounded. | Amendment must not redesign tape, BIR, layout, locks, tranche sequence, or migration. |

## §7 Closing posture

Hereupon Phase 0 returns **AMENDMENT-REQUIRED**.

The next step is Phase 0.5 narrow amendment:

1. Dispatch three amendment agents on the routing matrix in §4.
2. Require verify-then-patch pre-fill per `restart/prompts/AMENDMENT-DISPATCH.md` §1.
3. Preserve disjoint write ownership.
4. Stage only intended paths.
5. Commit each amendment with a body that states why, what landed, evidence,
   and routed remainder.
6. Rerun a V5.1 verification cycle against amended surfaces.

Estimated amendment wall time: 2.5 to 4.5 hours, depending on whether examples
land inline or as precise cookbook receivers. Estimated V5.1 verification wall
time: 45 to 75 minutes with three parallel verification agents. PASS-1 does
not carry READY through V5 because its report returned AMENDMENT-REQUIRED; it
does, however, remain the canonical source for grammar-surface repair.

Research Phase 1 stays blocked until V5.1 returns READY or the cohort is
escalated.

### Phase 0.5 dispatch contract

The amendment dispatches should be self-contained and should cite this
consolidation as the routing authority. Each worker reads the V5 reports, then
pre-fills every assigned bundle with current target text before editing. A
bundle whose target text is already fixed by another worker becomes
verify-only; a bundle whose requested surgery contradicts a settled lock
becomes an escalation row, not an improvisation.

#### SYNTHESIS narrow packet

| Field | Contract |
|---|---|
| Role | Phase 0.5 SYNTHESIS amendment worker. |
| Write scope | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md`; README stale-line exception only if explicitly accepted in the dispatch. |
| Read set | V5 consolidation; four V5 reports; README; Locks; PASS-1 §6; PASS-2 diagnostics; PASS-3 feeder/diagnostic rows; Amendment Dispatch §1. |
| Bundles | 1, 3, 4, 7, 8. |
| Non-goals | No lock edits; no prompt edits; no migration topology redesign; no tranche sequence redesign. |
| Commit subject | `docs(restart/synthesis): wave-5.1 narrow amendment — formal grammar, provenance, and examples`. |
| Required body | Why V5 reopened; what surfaces changed; evidence commands; routed remainder. |

SYNTHESIS pre-fill rows:

| Bundle | Pre-fill check | Edit guard |
|---|---|---|
| 1 | Quote current `ARCHITECTURE.md:1049-1081` and PASS-1 canonical production lines. | Do not invent syntax absent from PASS-1 or README. |
| 1 | Quote current `MASTER-PLAN.md:329` and `MASTER-PLAN.md:748`. | Replace declaration wording with block-bodied definition wording. |
| 3 | Grep stale citations before editing. | Prefer section citations if line ranges would churn. |
| 3 | If README is in scope, classify every stale-positive hit first. | Do not rewrite the README broadly. |
| 4 | Run the yaml row cell-count command before editing. | Keep two-surface onboarding; fixtures remain parity-phase. |
| 4 | Compare PASS-3 feeder row to Architecture §12.1 yaml row. | Preserve generated-only output language. |
| 7 | Quote existing H.W3 / WASM ABI carry rows. | Add gate shape, not benchmark numbers. |
| 8 | Pick one grammar and stick to it through the whole trajectory. | Do not create a second special grammar path. |

#### PASS-2/PASS-1 narrow packet

| Field | Contract |
|---|---|
| Role | Phase 0.5 PASS-2/PASS-1 amendment worker. |
| Write scope | `restart/audit/pass-2-codegen/PASS-2.md`; `restart/audit/pass-1-substrate/PASS-1.md`. |
| Read set | V5 consolidation; PASS-1 V5; PASS-2 V5; PASS-3 V5 rows that cite PASS-2 diagnostics; Locks; Architecture §8.1 and §12.1. |
| Bundles | 2, 3, 4, 7, 9. |
| Non-goals | No PASS-3 edits; no trio edits; no new recognizer directive; no BIR alphabet change. |
| Commit subject | `docs(restart/pass-2): wave-5.1 narrow amendment — recognizer diagnostics and provenance`. |
| Required body | Why diagnostics were unsafe; what changed; grep evidence; routed examples. |

PASS-2/PASS-1 pre-fill rows:

| Bundle | Pre-fill check | Edit guard |
|---|---|---|
| 2 | Quote current `BBNF-OPT001` and `BBNF-OPT002` strings. | Remove `@pratt` and `@simd` from valid user remediation. |
| 2 | Check MASTER Lock 10 language. | Do not introduce a new force directive by another name. |
| 3 | Locate `BBNF-SEM040` references and current diagnostic table row. | Repair only stale line refs. |
| 4 | Quote PASS-2 yaml smoke/runtime rows. | Add trace or receiver without changing source surfaces. |
| 7 | Quote PASS-2 WASM lowerer scope. | Add ABI/primitive route without inventing latency numbers. |
| 9 | Quote PASS-1 rare escape fence and Architecture eight-field fence. | Add bridge or missing fields; do not change rare-escape policy. |

#### PASS-3 narrow packet

| Field | Contract |
|---|---|
| Role | Phase 0.5 PASS-3 amendment worker. |
| Write scope | `restart/audit/pass-3-runtime/PASS-3.md`. |
| Read set | V5 consolidation; PASS-3 V5; PASS-1 diagnostic rows; PASS-2 updated diagnostics if already committed; MASTER carry/friction rows. |
| Bundles | 3, 5, 6, 7, 9. |
| Non-goals | No runtime API redesign; no path macro rename; no SOTA threshold changes; no README/trio edits. |
| Commit subject | `docs(restart/pass-3): wave-5.1 narrow amendment — runtime examples and diagnostic provenance`. |
| Required body | Why PASS-3 reopened; what examples/provenance landed; evidence commands; routed remainder. |

PASS-3 pre-fill rows:

| Bundle | Pre-fill check | Edit guard |
|---|---|---|
| 3 | Quote wrong Lock 8 citation and `bbnf_ir::` diagnostic string. | Fix references without changing lowerer boundary. |
| 5 | Quote current pointer/select section and diagnostics. | Use settled `pointer!`/`select!` names only. |
| 6 | Quote current recovery/incremental section and debug/DAP sentence. | Add example, not new API. |
| 7 | Quote `WasmHost` diagnostic. | Route to H.W3; no invented `{N}`/`{M}` values. |
| 9 | Quote lookbehind alias row. | Bind numeric code, alias, and kind together. |

### Phase 0.5 close evidence table

| Evidence | Owner route | Required before commit |
|---|---|---|
| `git diff --check` | All routes | Clean. |
| `rg -n '@pratt|@simd' ...` classified | PASS-2/PASS-1 | No valid directive remediation remains. |
| Architecture grammar grep classified | SYNTHESIS | No normative stale grammar forms remain. |
| README stale hit classification | SYNTHESIS if README touched | No settled-state stale hits remain. |
| YAML row cell count | SYNTHESIS | Consistent row shape. |
| PASS-3 `bbnf_ir::` grep | PASS-3 | Zero hits. |
| PASS-3 `should reuse this identity` grep | PASS-3 | Zero hits. |
| Stale citation grep | SYNTHESIS and PASS-2/PASS-1 | Named stale line refs removed or section-scoped. |
| Worked example grep | SYNTHESIS and PASS-3 | Examples or precise cookbook receivers present. |
| Commit body | All routes | Names why, what, evidence, and routed remainder. |

### V5.1 verification plan

V5.1 should not be a full new V5 metahardening. It should verify the amended
surfaces against the nine bundles above and keep the existing V5 reports as
the punch source. Three verification agents are enough if Phase 0.5 preserves
the route split.

| V5.1 verifier | Read targets | Must verify | Expected output |
|---|---|---|---|
| SYNTHESIS verifier | `ARCHITECTURE.md`, `MIGRATION.md`, `MASTER-PLAN.md`, README exception if touched, V5 consolidation | Bundles 1, 3, 4, 7, 8 and 16-command gate deltas. | `HARDENING-MASTER-PLAN-V5.1.md` or equivalent V5.1 verification note. |
| PASS-2/PASS-1 verifier | `PASS-2.md`, `PASS-1.md`, V5 consolidation | Bundles 2, 3, 4, 7, 9. | `HARDENING-PASS-2-V5.1.md` plus PASS-1 carry note if needed. |
| PASS-3 verifier | `PASS-3.md`, V5 consolidation | Bundles 3, 5, 6, 7, 9. | `HARDENING-PASS-3-V5.1.md`. |

Minimum V5.1 command list:

| Command class | Command or check |
|---|---|
| Retired recognizer directives | `rg -n '@pratt|@simd' restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md` classified as prohibition-only. |
| Architecture stale grammar | `rg -n 'HostDecl.*;|=> TypeExpr|MapExpr' restart/ARCHITECTURE.md` returns zero or non-normative/rejection-only context. |
| Lookbehind shape | `rg -n 'Lookbehind.*\\|<' restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md` shows one compatible grammar-level form. |
| README stale prose | `rg -n 'ParseStream|rewrite-mode|Unicode class algebra' restart/README.md` has no settled-state hits. |
| PASS-2 stale refs | `rg -n 'PASS-2.md:293-310|PASS-2.md:98-116' restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-*` returns zero unless explicitly marked stale history. |
| Architecture stale row refs | `rg -n 'ARCHITECTURE.md:1273-1281' restart/MASTER-PLAN.md restart/audit/pass-*` returns zero. |
| Lock citation repair | Manual check that Lock 8, Lock 11, and Lock 14 citations point to the named lock lines or section names. |
| YAML row cell count | `awk -F'|' 'NR>=1320 && NR<=1331 { print NR, NF-1 }' restart/ARCHITECTURE.md` reports consistent cells. |
| YAML two-surface proof | Example contains `grammars/yaml.bbnf` and `[workspace.metadata.bbnf.grammars.yaml]`, not a Rust registry edit. |
| Pointer/select example | `rg -n 'pointer!.*select!|select!.*pointer!|BBNF-POINTER' restart/MASTER-PLAN.md restart/audit/pass-3-runtime/PASS-3.md restart/ARCHITECTURE.md` finds success and failure paths. |
| Incremental/recovery example | `rg -n 'dirty range|fallback reason|@error\\(recover|DocumentSnapshot|ReparsePlan' restart/MASTER-PLAN.md restart/audit/pass-3-runtime/PASS-3.md` finds the worked path. |
| Debug/DAP identity | `rg -n 'should reuse this identity' restart/audit/pass-3-runtime/PASS-3.md` returns zero. |
| Lowerer diagnostic prefix | `rg -n 'bbnf_ir::' restart/audit/pass-3-runtime/PASS-3.md` returns zero. |
| Lookbehind alias binding | `rg -n 'BBNF1004|BBNF-LOOKBEHIND-WIDTH|LookbehindWidth' restart/audit/pass-3-runtime/PASS-3.md restart/ARCHITECTURE.md` shows the alias chain. |
| WASM host ABI gate | `rg -n 'WASM host primitive|host primitive ABI|exported function names|host-call shape' restart/MASTER-PLAN.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md` finds the H.W3 route. |
| Grammar trajectory | `rg -n 'A->F->J|A to F to J|grammar trajectory|yaml trajectory|json trajectory' restart/MASTER-PLAN.md restart/ARCHITECTURE.md` finds one named grammar path. |

V5.1 returns READY only if every bundle is closed or explicitly routed with a
receiver, blocker, and receiving gate. V5.1 returns AMENDMENT-REQUIRED if any
bundle remains open but is still narrow. V5.1 returns RE-DRAFT only if an
amendment proves one of the re-draft thresholds in §5 true.

## §8 LLM-pathology summary across the cohort

The dominant pathology is **formal-fragment drift**. Prose was amended, but
grammar snippets and diagnostic strings retained shapes that sound plausible:
bodyless host declarations, `=>` maps, rule-level method chains, and recognizer
directives. Those fragments are high-risk because implementers copy them.

The second pathology is **closure bias after READY**. V4 correctly closed the
large V3 list and then treated related surfaces as mature. V5 shows that a
READY corpus can still carry stale authority cues in secondary locations:
README closing prose, line citations, diagnostic strings, and example syntax.

The third pathology is **matrix satisfaction**. The corpus has tables for
yaml onboarding, per-grammar authority, carry ledgers, budgets, and cookbook
receivers. Those tables are necessary. They are insufficient where the risk is
operator ergonomics: adding a grammar, writing a pointer, debugging an
incremental edit, or following one grammar through tranches.

The fourth pathology is **citation confidence**. Wrong-line citations point to
real files and nearby concepts, which makes them easy to miss. V5 treats that
as a provenance gap because later agents will route surgery by those anchors.

No V5 report found evidence of SOTA citation fabrication that forces lock
revision. Several reports noted source compression through local SOTA corpora
rather than direct primary sources; that is exactly what Phase 1 research is
designed to ground after V5.1 clears the corpus.

The cohort posture is therefore adversarial but bounded: correct the stale
fragments, add compact examples, repair provenance, and rerun V5.1. Do not
invent new architecture to solve an LLM-authorship cleanup problem.
