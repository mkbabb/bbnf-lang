# HARDENING-MASTER-PLAN-V5

## §1 Target Identification

Phase 0 V5 metahardening target: MASTER-PLAN trio.

Target surfaces audited:

- `restart/ARCHITECTURE.md`
- `restart/MIGRATION.md`
- `restart/MASTER-PLAN.md`

Output path:

- `restart/audit/hardening/HARDENING-MASTER-PLAN-V5.md`

Audit posture:

- V5 worker scope: audit only.
- Target surfaces were read, not patched.
- Prior hardening reports V1 through V4 were treated as carry history, not as proof.
- `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` §1-§2 supplied the Phase 0 V5 contract.
- `restart/prompts/HARDENING.md` supplied the compressed 9-lane audit discipline.
- `restart/prompts/AMENDMENT-DISPATCH.md` §1 supplied verify-then-patch hygiene; this report verifies claims and routes amendments without applying them.
- `docs/precepts/instructions/STYLE.md`, `LESSONS-LEARNED.md`, and `CONSUMING.md` supplied voice and process discipline.

Mandatory read set completed:

- `restart/README.md`
- `restart/locks/14-LOCKS.md`
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md`
- `restart/audit/hardening/HARDENING-CONSOLIDATED.md`
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V2.md`
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V3.md`
- `restart/research/INDEX.md`
- `restart/prompts/HARDENING-ORCHESTRATOR.md`
- `restart/prompts/HARDENING.md`
- `restart/prompts/AMENDMENT-DISPATCH.md`
- `docs/precepts/instructions/STYLE.md`
- `docs/precepts/instructions/LESSONS-LEARNED.md`
- `docs/precepts/instructions/CONSUMING.md`
- `restart/ARCHITECTURE.md`
- `restart/MIGRATION.md`
- `restart/MASTER-PLAN.md`
- `restart/audit/pass-1-substrate/PASS-1.md`
- `restart/audit/pass-2-codegen/PASS-2.md`
- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/audit/hardening/HARDENING-MASTER-PLAN.md`
- `restart/audit/hardening/HARDENING-MASTER-PLAN-V2.md`
- `restart/audit/hardening/HARDENING-MASTER-PLAN-V3.md`
- `restart/audit/hardening/HARDENING-MASTER-PLAN-V4.md`

Verdict:

- **AMENDMENT-REQUIRED**

Reason for non-READY verdict:

- The trio is structurally coherent after V4, but V5 found concrete source faults that are not merely polish.
- `ARCHITECTURE.md` still contains a declaration-only `HostDecl` grammar production while PASS-1 and README define block-bodied `@host fn`.
- `ARCHITECTURE.md` still contains rule-level chain syntax drift through `MapExpr ::= Expr "=>" TypeExpr`.
- `ARCHITECTURE.md` has a malformed YAML per-grammar table row with one fewer cell than the header and feeder table.
- Two lock references cite the wrong line numbers.
- Worked examples remain too sparse for the public surfaces the plan now commits to shipping.

Redraft threshold:

- Not crossed.
- The amendment set is narrow.
- The tranche topology, lock ownership, migration routing, and close gates still hold.

## §2 Carry-Aware Lens Table A-E

| ID | Lens | Verified anchors | Finding | Carry disposition |
|---|---|---|---|---|
| A1 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:19-30`; `restart/MIGRATION.md:47-56`; `restart/MASTER-PLAN.md:737-757` | Conflict ledger, migration disposition, and carry ledger now share the same center of gravity: tape/direct, BIR, layout, path, yaml, SOTA, and archive closure. | KEEP |
| A2 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:1049-1081`; `restart/audit/pass-1-substrate/PASS-1.md:183`; `restart/audit/pass-1-substrate/PASS-1.md:211` | Architecture prose says block-bodied `@host fn`, but the formal sketch still accepts only a semicolon declaration. | AMEND |
| A3 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:1079`; `restart/audit/pass-1-substrate/PASS-1.md:205-217`; `restart/README.md:159-166` | Architecture maps rule projection through `=> TypeExpr`, while the canonical rule-level chain is `Expr -> f1 -> f2`. | AMEND |
| A4 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:1320-1331`; `restart/audit/pass-3-runtime/PASS-3.md:331-342`; `restart/MASTER-PLAN.md:641-652` | The architecture per-grammar table is mostly bound to PASS-3 and MASTER, but the YAML row drops the host-route cell. | AMEND |
| A5 | Inter-document narrative coherence | `restart/MASTER-PLAN.md:737-757`; `restart/MIGRATION.md:772-783`; `restart/MASTER-PLAN.md:735` | MASTER now owns carry rows and MIGRATION points to them rather than duplicating them. | KEEP |
| B1 | Vocabulary drift | `restart/ARCHITECTURE.md:435-442`; `restart/ARCHITECTURE.md:792`; `restart/ARCHITECTURE.md:983-990` | `LayoutFacts`, `LayoutSink`, and `passes::layout` are materially settled. `TypeFacts` is internal to layout lowering, not exported as a competing pass. | KEEP |
| B2 | Vocabulary drift | `restart/ARCHITECTURE.md:870-903`; `restart/audit/pass-2-codegen/PASS-2.md:196`; `restart/MASTER-PLAN.md:380-395` | Backend IR ownership is stable as `ir::backend_ir`; `codegen/src/backend_ir/README.md` is documentation-only in PASS-2. | KEEP |
| B3 | Vocabulary drift | `restart/ARCHITECTURE.md:55-57`; `restart/MASTER-PLAN.md:227`; gate G02 | Retired `bbnf-path` appears only as migration prose; public crate names are `path`, `path-core`, and `path-ts`. | KEEP |
| B4 | Vocabulary drift | `restart/ARCHITECTURE.md:270-272`; `restart/README.md:278-283`; `restart/audit/pass-3-runtime/PASS-3.md:359-360` | `pointer!` syntax is not fully harmonized across examples: Architecture uses string JSON-pointer form, README uses typed segment list, PASS-3 diagnostic recommends `pointer!(Json => "/...")`. | AMEND |
| B5 | Vocabulary drift | `restart/ARCHITECTURE.md:1081`; `restart/MASTER-PLAN.md:329`; `restart/MASTER-PLAN.md:722`; `restart/audit/pass-1-substrate/PASS-1.md:211` | The word "declarations" around `@host fn` still invites the retired bodyless form unless formal grammar and wave text say block-bodied definitions. | AMEND |
| C1 | Worked-example scarcity | `restart/ARCHITECTURE.md:1275-1306`; `restart/MASTER-PLAN.md:110`; `restart/MASTER-PLAN.md:770` | The yaml onboarding gate exists, but no single example walks grammar source, workspace metadata, generated runtime, path schema, host route, and bench manifest. | AMEND |
| C2 | Worked-example scarcity | `restart/ARCHITECTURE.md:270-272`; `restart/MASTER-PLAN.md:764`; `restart/audit/pass-3-runtime/PASS-3.md:359-361` | Query ergonomics are asserted, but no example shows one document queried through `pointer!` and `select!` with diagnostic failure cases. | AMEND |
| C3 | Worked-example scarcity | `restart/MASTER-PLAN.md:490-501`; `restart/audit/pass-3-runtime/PASS-3.md:181-190`; `restart/MASTER-PLAN.md:726` | Fault-tolerant incremental parsing has real PASS-3 thresholds, but MASTER has no worked edit path from batch parse to LSP fallback. | AMEND |
| C4 | Worked-example scarcity | `restart/ARCHITECTURE.md:1027`; `restart/ARCHITECTURE.md:1100`; `restart/MASTER-PLAN.md:393` | `@error(recover = ...)` is carried, but no example shows a recoverable grammar branch, emitted diagnostic, and generated recovery node. | AMEND |
| C5 | Worked-example scarcity | `restart/MASTER-PLAN.md:225-246`; `restart/MASTER-PLAN.md:380-407`; `restart/MASTER-PLAN.md:520-526` | The plan lacks one A -> F -> J grammar trajectory that traces a grammar from skeleton through BIR/codegen to publication readiness. | AMEND |
| D1 | Coverage gap | `restart/MASTER-PLAN.md:762-770`; `restart/ARCHITECTURE.md:1275-1306`; `restart/audit/pass-3-runtime/PASS-3.md:320-344` | Unfamiliar grammar onboarding is close but still cookbook-dependent; the target user needs an acceptance example, not only gate prose. | AMEND |
| D2 | Coverage gap | `restart/audit/pass-3-runtime/PASS-3.md:181-190`; `restart/MASTER-PLAN.md:490-501`; `restart/MIGRATION.md:417` | Incremental parsing fallback policy exists in PASS-3; MASTER should bind to the numeric reuse and full-reparse thresholds. | AMEND |
| D3 | Coverage gap | `restart/ARCHITECTURE.md:242`; `restart/ARCHITECTURE.md:898-930`; `restart/MASTER-PLAN.md:490-501` | Debug/runtime hook claims remain under-specified: the BIR and runtime sections name hooks, but no trace event schema or acceptance fixture is named. | AMEND |
| D4 | Coverage gap | `restart/ARCHITECTURE.md:1105-1120`; `restart/ARCHITECTURE.md:1027`; `restart/MASTER-PLAN.md:393` | Generic-rule typing and recovery facts are each specified, but their intersection under `@error(recover = ...)` is not gated. | AMEND |
| D5 | Coverage gap | `restart/MASTER-PLAN.md:459`; `restart/MASTER-PLAN.md:753`; `restart/ARCHITECTURE.md:868` | WASM host primitives have a performance gate and ABI carry row, but no primitive matrix tying host-call shape to exported WASM binding names. | AMEND |
| D6 | Coverage gap | `restart/MASTER-PLAN.md:524`; `restart/locks/14-LOCKS.md:54`; `restart/MASTER-PLAN.md:677` | Lock 11 incubation stability is correctly expanded in J.W3, though the lock ownership table remains terse. | KEEP-WITH-POLISH |
| E1 | Cumulative lock tension | `restart/locks/14-LOCKS.md:34`; `restart/ARCHITECTURE.md:1165-1206`; `restart/MASTER-PLAN.md:251-272` | Lock 1 tape/direct pressure is honored across architecture, runtime waves, and migration gates. | KEEP |
| E2 | Cumulative lock tension | `restart/locks/14-LOCKS.md:42`; `restart/ARCHITECTURE.md:780-903`; `restart/MASTER-PLAN.md:380-407` | Lock 5 BIR sequencing is mostly coherent; the chain grammar drift is the remaining Lock 4/Lock 5 boundary fault. | AMEND |
| E3 | Cumulative lock tension | `restart/locks/14-LOCKS.md:52`; `restart/ARCHITECTURE.md:817-819`; `restart/MASTER-PLAN.md:428` | Lock 10 Pratt/SIMD auto-detection is preserved in trio surfaces. | KEEP |
| E4 | Cumulative lock tension | `restart/locks/14-LOCKS.md:54`; `restart/MASTER-PLAN.md:524`; `restart/MASTER-PLAN.md:677` | Lock 11 path-dep incubation no longer forces sister crate publication; J.W3 has a stability gate. The line citation is wrong. | AMEND |
| E5 | Cumulative lock tension | `restart/locks/14-LOCKS.md:60`; `restart/ARCHITECTURE.md:1275-1306`; `restart/MASTER-PLAN.md:770` | Lock 14 onboarding is preserved as two surfaces, but table and example gaps weaken enforcement for the first unfamiliar grammar. | AMEND |

Lens A-E summary:

- V4 correctly closed the broad cross-document contradictions.
- V5 finds narrower faults where formal grammar, examples, and citations lag behind the settled prose.
- The hard faults are surgical and document-local.
- No tranche redraft is recommended.

## §3 LLM-Pathology Table F-H

| ID | Lens | Verified anchors | Pathology check | Finding |
|---|---|---|---|---|
| F1 | LLM bias | `restart/MASTER-PLAN.md:641-652`; `restart/ARCHITECTURE.md:1320-1331` | Pseudo-precision around LOC ceilings | Mostly controlled: ceilings derive from Architecture baselines, but the malformed YAML row makes the table look more precise than it is. |
| F2 | LLM bias | `restart/MASTER-PLAN.md:459`; `restart/MIGRATION.md:769` | Placeholders masquerading as commitments | `{N}` and `{M}` are acceptable because H.W3 owns measurement and blocker state. No amendment beyond the WASM primitive matrix. |
| F3 | LLM bias | `restart/ARCHITECTURE.md:1081`; `restart/audit/pass-1-substrate/PASS-1.md:211` | Confident prose hiding formal contradiction | Present. The prose says block-bodied host functions while the grammar accepts bodyless declarations. |
| F4 | LLM bias | `restart/ARCHITECTURE.md:1079`; `restart/README.md:159-166` | Old syntax retained because it sounds compiler-like | Present. `=> TypeExpr` looks plausible but contradicts the canonical chain surface. |
| F5 | LLM bias | `restart/MASTER-PLAN.md:764-770`; `restart/ARCHITECTURE.md:270-272` | User-facing assertions without examples | Present. Cookbook rows do not substitute for a worked query or yaml path through the system. |
| G1 | Overfitting | `restart/ARCHITECTURE.md:1275-1306`; `restart/MASTER-PLAN.md:770`; `restart/audit/pass-3-runtime/PASS-3.md:320-344` | YAML as a named future grammar could become a special case | Controlled by two-surface rule, but needs the corrected table row and an unfamiliar-grammar walkthrough. |
| G2 | Overfitting | `restart/ARCHITECTURE.md:1320-1331`; `restart/MASTER-PLAN.md:641-652` | Seed-grammar budget rows could freeze today’s grammar set | Mostly controlled by yaml probe and generated budget gates. The YAML row fault weakens the proof. |
| G3 | Overfitting | `restart/MASTER-PLAN.md:719`; `restart/MASTER-PLAN.md:744`; `restart/locks/14-LOCKS.md:60` | Generic crates could regain grammar-named branches | Controlled by lint rows and future grammar gate. |
| G4 | Overfitting | `restart/ARCHITECTURE.md:1252-1259`; `restart/MASTER-PLAN.md:129-136` | SOTA rows could bias only JSON/CSS hot fixtures | Partly controlled by explicit fixtures, but a worked non-hot grammar trajectory would reduce overfit pressure. |
| H1 | Hallucination/provenance | `restart/ARCHITECTURE.md:1000`; `restart/locks/14-LOCKS.md:69-72`; `restart/locks/14-LOCKS.md:60` | Wrong citation line | Present. Lock 14 itself is line 60; lines 69-72 introduce lanes, not metadata/onboarding clauses. |
| H2 | Hallucination/provenance | `restart/MASTER-PLAN.md:524`; `restart/locks/14-LOCKS.md:54`; `restart/locks/14-LOCKS.md:60` | Wrong citation line | Present. MASTER says Lock 11 at line 60, but line 60 is Lock 14; Lock 11 is line 54. |
| H3 | Hallucination/provenance | `restart/ARCHITECTURE.md:1256`; `restart/MASTER-PLAN.md:133`; `restart/corpora/SOTA.md:56` | Cross-document benchmark provenance mismatch | Architecture lists only sonic-rs for `json/canada`; MASTER carries both sonic-rs and simd-json with SOTA citation. |
| H4 | Hallucination/provenance | `restart/ARCHITECTURE.md:270-272`; `restart/README.md:278-283`; `restart/audit/pass-3-runtime/PASS-3.md:360` | Multiple public examples without an explicit canonical form | Present. The trio needs one canonical macro grammar-prefix rule or a compatibility note. |
| H5 | Hallucination/provenance | `restart/MASTER-PLAN.md:737-757`; `restart/MIGRATION.md:772-783` | Remainder routed without owner | Not present. Remainder is routed to MASTER §23-§24 and MIGRATION points there. |

Pathology summary:

- The main V5 pathology is not invention of new architecture.
- It is retention of plausible stale fragments after V4 amendments.
- Formal grammar fragments, line citations, and example syntax require the same rigor as tranche tables.

## §4 Compressed 9-Lane Verification

| Row | Lane | Surface | Verification | Status |
|---|---|---|---|---|
| 1 | Lane 1 - Lock coverage | Lock 1 | Tape/direct substrate appears in ARCH runtime shape and MASTER B/F/H gates. | honored |
| 2 | Lane 1 - Lock coverage | Lock 5 | BIR is owned by `ir::backend_ir`; codegen is lowerer/consumer. | honored |
| 3 | Lane 1 - Lock coverage | Lock 14 | Future grammar rule is present in ARCH, MASTER, PASS-3. | honored-with-amendment |
| 4 | Lane 2 - Sequencing, Lock 1 | B before F/H | MASTER B establishes checkpoints before F generated runtime and H optimizations. | honored |
| 5 | Lane 2 - Sequencing, Lock 2 | D/F layout | `passes::layout` and `LayoutFacts` land before BIR layout replay gates. | honored |
| 6 | Lane 2 - Sequencing, Lock 3 | source normalization | Unicode grammar algebra is not introduced; regex layer owns it. | honored |
| 7 | Lane 2 - Sequencing, Lock 4 | BBNF surface | Lookbehind, host functions, chaining, generics, error, layout all land in V1; grammar sketch needs repair. | violated-with-recommendation |
| 8 | Lane 2 - Sequencing, Lock 5 | IR before codegen | PASS-1/ARCH define BIR before PASS-2/codegen lowerers. | honored |
| 9 | Lane 2 - Sequencing, Lock 6 | egraph/CSP | ARCH stages type/egraph/cost facts before BIR and codegen. | honored |
| 10 | Lane 2 - Sequencing, Lock 7 | path DSL | Path crate split lands in A and consumer smokes land in G/I/J. | honored-with-example-gap |
| 11 | Lane 2 - Sequencing, Lock 8 | SOTA | H/J benchmark metadata gates exist; Architecture canada row needs comparator parity. | honored-with-amendment |
| 12 | Lane 2 - Sequencing, Lock 9 | LSP | I owns language-server parity after runtime/path surfaces exist. | honored |
| 13 | Lane 2 - Sequencing, Lock 10 | recognizer auto-detect | Pratt/SIMD remain shape-mined, not directives. | honored |
| 14 | Lane 2 - Sequencing, Lock 11 | path-dep incubation | J.W3 stability split is correct; line citation wrong. | honored-with-amendment |
| 15 | Lane 2 - Sequencing, Lock 12 | archive before BA | A/J closure rows keep archive out of active workspace. | honored |
| 16 | Lane 2 - Sequencing, Lock 13 | tree shape | A skeleton and lint gates enforce 4-10 children and 500 LOC caps. | honored |
| 17 | Lane 2 - Sequencing, Lock 14 | future grammar | A/G/F/J yaml gates exist; Architecture table row is malformed. | violated-with-recommendation |
| 18 | Lane 3 - Carry chain | V1 carry | V1 broad contradictions are mostly closed by V4; V5 does not reopen closed items. | honored |
| 19 | Lane 3 - Carry chain | V3 carry | Lock 2, Lock 11, bench, diagnostics, and yaml routing were carried into trio. | honored |
| 20 | Lane 4 - Evidence | Gate corpus | 16 grep gates rerun locally; all returned matches for review. | complete |
| 21 | Lane 4 - Evidence | Formal grammar | PASS-1 line proof contradicts ARCH grammar sketch. | failed |
| 22 | Lane 5 - Migration | Migration routing | MIGRATION routes unresolved implementation receivers to MASTER §23-§24. | honored |
| 23 | Lane 5 - Migration | Archive | Archive placement and workspace exclusion are gated. | honored |
| 24 | Lane 6 - Public surface | `pointer!`/`select!` | Public path DSL has diagnostics and cookbook row, but no harmonized example syntax. | amendment |
| 25 | Lane 6 - Public surface | `@error(recover)` | Recovery vocabulary is carried, but no worked example ties grammar, diagnostic, and runtime node. | amendment |
| 26 | Lane 7 - Overfit | yaml | Two-surface yaml onboarding prevents special-case code, subject to table correction. | amendment |
| 27 | Lane 7 - Overfit | generated LOC | Budget table handles seed grammars and provisional yaml. | honored |
| 28 | Lane 8 - LLM pathology | citations | Two wrong line references found and verified against locks file. | failed |
| 29 | Lane 8 - LLM pathology | prose/formal mismatch | Host function and chain syntax drift are stale plausible fragments. | failed |
| 30 | Lane 9 - Verdict | Trio readiness | Narrow amendments required; no redraft. | AMENDMENT-REQUIRED |

Lane 2 full conclusion:

- All fourteen locks have sequencing coverage in MASTER-PLAN.
- Locks 4, 8, 11, and 14 require narrow text amendments.
- The sequence itself does not need replacement.

## §5 16-Command Gate Rerun

| Gate | Command | Observed matches | V5 read |
|---|---|---:|---|
| G01 | `rg -n "ParseStream\|rewrite-mode\|Unicode class algebra" restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-*/*.md` | 100 | PASS. Residue is explanatory and rejection-oriented, not revived architecture. |
| G02 | `rg -n "bbnf-path\|bbnf-test-fixtures\|path!" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-3-runtime` | 35 | PASS. Old names appear as migration provenance; `path!` is not a public surface. |
| G03 | `rg -n "codegen/src/backend_ir" restart/ARCHITECTURE.md restart/audit/pass-2-codegen` | 4 | PASS-WITH-NOTE. PASS-2 confines this to a documentation-only README. |
| G04 | `rg -n "fixtures/yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` | 9 | PARTIAL. PASS-3 confines fixture rows to parity prose, but ARCH YAML table row is malformed. |
| G05 | `rg -n "@recover" restart/ARCHITECTURE.md restart/audit/pass-3-runtime` | 8 | PASS. Standalone `@recover` is migration alias/rejection prose; `@error(recover = ...)` is canonical. |
| G06 | `rg -n "OpenFrame" restart/audit/pass-1-substrate restart/audit/pass-2-codegen restart/MASTER-PLAN.md` | 30 | PASS. OpenFrame appears as retired failure mode and proof target. |
| G07 | `rg -n "GrammarIR" restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md` | 3 | PASS. PASS-2 names `GrammarIR` only to deny direct codegen ownership. |
| G08 | `rg -n "__EAGER_EMPTY_PATH\|CursorDecision::Skip" restart/MASTER-PLAN.md restart/MIGRATION.md` | 2 | PASS. Runtime skip fixtures are routed. |
| G09 | `rg -n "twitter\|canada\|citm\|bootstrap\|animate\|On-Demand" restart/MASTER-PLAN.md restart/audit/pass-3-runtime/PASS-3.md` | 21 | PASS-WITH-NOTE. Benchmark fixtures are named; Architecture canada comparator parity remains a provenance polish fault. |
| G10 | `rg -n "receiver\|blocker\|receiving gate" restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-*` | 20 | PASS. Receivers and blockers are routed to MASTER carry rows. |
| G11 | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` | 16 | PASS. Two-surface yaml onboarding is present. |
| G12 | `rg -n "generated_loc\|regen_wall\|xtask" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` | 75 | PASS. Generated LOC and xtask gates exist; `regen_wall` token remains non-blocking V4 polish. |
| G13 | `rg -n "BBNF-LIFE\|BBNF-LAYOUT\|BBNF-OPT\|BBNF-GRAMMAR\|BBNF-POINTER\|lookbehind\|HostSignature" restart/ARCHITECTURE.md restart/audit/pass-*` | 80 | PASS. Diagnostic vocabulary is consolidated; citation line to Lock 14 is wrong. |
| G14 | `rg -n "child count\|500 LOC\|exception rationale" restart/ARCHITECTURE.md restart/MASTER-PLAN.md` | 10 | PASS. Lock 13 tree/LOC gates are represented. |
| G15 | `rg -n "declaration-crate review\|why metadata\|deletion path\|reviewer" restart/ARCHITECTURE.md restart/MIGRATION.md` | 7 | PASS. Declaration-crate escape form has owner, reviewer, and deletion path. |
| G16 | `rg -n "CPU model\|compiler flags\|input hash\|competitor version\|warmup\|sample" restart/MASTER-PLAN.md restart/MIGRATION.md` | 4 | PASS-WITH-NOTE. MASTER has benchmark metadata rows; competitor version and input hashes should remain required in H/J reports. |

Gate summary:

- PASS: G01, G02, G03, G05, G06, G07, G08, G10, G11, G12, G14, G15.
- PASS-WITH-NOTE: G09, G13, G16.
- PARTIAL: G04.
- Hard amendment evidence also came from targeted formal grammar and citation checks.

Targeted verification notes:

- `rg -n "HostDecl|HostFn|MapTail|MapExpr|ChainExpr|@host" restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md restart/MASTER-PLAN.md`
- Result: PASS-1 line 183 defines `HostFn` with trailing `Block`.
- Result: PASS-1 line 211 rejects declaration-only `@host fn`.
- Result: PASS-1 lines 205-217 define canonical rule-level chain syntax.
- Result: ARCHITECTURE line 1081 still defines `HostDecl` with trailing semicolon.
- Result: ARCHITECTURE line 1079 still defines `MapExpr ::= Expr "=>" TypeExpr`.
- Result: MASTER line 329 still uses `@host fn declarations`.
- V5 read: P1, P2, P3, and P4 are verified.

- `awk -F'|' 'NR>=1320 && NR<=1331 { print NR, NF-1, $0 }' restart/ARCHITECTURE.md`
- Result: ARCHITECTURE rows 1320-1330 each report 11 pipe-delimited cells.
- Result: ARCHITECTURE row 1331 reports 10 pipe-delimited cells.
- Result: PASS-3 row 342 supplies the missing YAML host-route content.
- V5 read: P5 is verified.

- `nl -ba restart/locks/14-LOCKS.md | sed -n '52,62p;69,72p'`
- Result: Lock 11 is line 54.
- Result: Lock 14 is line 60.
- Result: lines 69-72 introduce lane scaffolding, not metadata/onboarding clauses.
- V5 read: P6 and P7 are verified.

- `nl -ba restart/README.md | sed -n '145,166p;276,283p'`
- Result: README defines block-bodied `@host fn` examples.
- Result: README defines multi-function chaining as `-> f1 -> f2 -> f3`.
- Result: README path DSL examples use `pointer!(Json, ["a", "b", 0])` and `select!(Css, "...")`.
- V5 read: README agrees with PASS-1 on host body and chain syntax, but public path examples need harmonization with PASS-3 diagnostic syntax.

- `nl -ba restart/audit/pass-3-runtime/PASS-3.md | sed -n '318,344p;352,361p'`
- Result: PASS-3 line 322 requires generated value API, `pointer!`, `select!`, visitor, host route, and bench manifest from yaml source plus metadata.
- Result: PASS-3 line 342 includes the YAML host route that ARCHITECTURE row 1331 omits.
- Result: PASS-3 lines 359-360 define pointer diagnostics and explicit grammar-prefix help.
- V5 read: public examples and YAML table correction are load-bearing.

Local edit hygiene:

- Only this report path was written.
- Target surfaces were not patched.
- No lock, prompt, research, pass, crate, corpus, inheritance, archive, README, or other hardening report was edited.

## §6 Cross-Document Binding Ledger

| Claim | ARCHITECTURE binding | MIGRATION binding | MASTER binding | External binding | V5 result |
|---|---|---|---|---|---|
| Tape/direct is the one runtime substrate. | `restart/ARCHITECTURE.md:1165-1206` | `restart/MIGRATION.md:90-105` | `restart/MASTER-PLAN.md:251-272` | `restart/locks/14-LOCKS.md:34` | KEEP |
| OpenFrame is retired. | `restart/ARCHITECTURE.md:19-30` | `restart/MIGRATION.md:90-105` | `restart/MASTER-PLAN.md:26`; `restart/MASTER-PLAN.md:113` | PASS-3 runtime carry | KEEP |
| Backend IR belongs in `ir::backend_ir`. | `restart/ARCHITECTURE.md:780-903` | `restart/MIGRATION.md:232-244` | `restart/MASTER-PLAN.md:380-407` | `restart/audit/pass-2-codegen/PASS-2.md:196` | KEEP |
| Codegen may not walk Grammar IR directly. | `restart/ARCHITECTURE.md:780-903` | `restart/MIGRATION.md:232-244` | `restart/MASTER-PLAN.md:721` | `restart/audit/pass-2-codegen/PASS-2.md:5` | KEEP |
| Layout lowers through `LayoutFacts`. | `restart/ARCHITECTURE.md:971-990` | `restart/MIGRATION.md:237` | `restart/MASTER-PLAN.md:740` | PASS-2 layout rows | KEEP |
| `TypeFacts` is internal to layout. | `restart/ARCHITECTURE.md:990` | Not duplicated | D/F gates consume layout | PASS-1/PASS-2 split | KEEP |
| `@host fn` is block-bodied. | `restart/ARCHITECTURE.md:734`; conflict at `restart/ARCHITECTURE.md:1081` | Declaration-crate escape refers to host failures | `restart/MASTER-PLAN.md:329`; `restart/MASTER-PLAN.md:722` | `restart/audit/pass-1-substrate/PASS-1.md:183`; `restart/audit/pass-1-substrate/PASS-1.md:211` | AMEND |
| Rule-level chain syntax is `-> f1 -> f2`. | Conflict at `restart/ARCHITECTURE.md:1079` | Not duplicated | `restart/MASTER-PLAN.md:37`; `restart/MASTER-PLAN.md:393` | `restart/README.md:159-166`; `restart/audit/pass-1-substrate/PASS-1.md:205-217` | AMEND |
| Method-chain syntax belongs inside `@host fn` bodies. | Conflict risk at `restart/ARCHITECTURE.md:1077-1078` | Not duplicated | D/F host waves consume | `restart/audit/pass-1-substrate/PASS-1.md:217` | AMEND |
| Standalone `@recover` is retired. | `restart/ARCHITECTURE.md:1100` | Not duplicated | D/F/I recovery gates | `restart/audit/pass-3-runtime/PASS-3.md:160` | KEEP |
| Lookbehind width is bounded and diagnostic-bearing. | `restart/ARCHITECTURE.md:1005-1008` | Not duplicated | D grammar gates | `restart/audit/pass-1-substrate/PASS-1.md:215` | KEEP |
| `BBNF-LOOKBEHIND-WIDTH` and `BBNF1004` are vocabulary anchors. | `restart/ARCHITECTURE.md:1005-1008` | Not duplicated | Diagnostic table consumers in §24 | PASS-1 diagnostic prose | KEEP |
| `pointer!` and `select!` are public path surfaces. | `restart/ARCHITECTURE.md:270-272`; `restart/ARCHITECTURE.md:55-57` | Crate split migration | `restart/MASTER-PLAN.md:764` | `restart/README.md:278-283`; PASS-3 diagnostics | AMEND-EXAMPLE |
| Retired `path!` stays absent. | Gate G02 | Crate migration row | A.W1 package-name gate | Lock 7 | KEEP |
| `path-core` underlies Rust and TS path surfaces. | `restart/ARCHITECTURE.md:56-57` | `restart/MIGRATION.md:777` | `restart/MASTER-PLAN.md:752` | Lock 7 | KEEP |
| YAML onboarding uses only grammar source and workspace metadata. | `restart/ARCHITECTURE.md:1275-1306` | Future grammar migration gate | `restart/MASTER-PLAN.md:110`; `restart/MASTER-PLAN.md:770` | `restart/audit/pass-3-runtime/PASS-3.md:320-344` | KEEP |
| YAML parity fixtures are not onboarding surface. | `restart/ARCHITECTURE.md:1331` | Not duplicated | `restart/MASTER-PLAN.md:770` | PASS-3 fixture separation | AMEND-TABLE |
| YAML host route derives from metadata and host primitives. | Missing cell at `restart/ARCHITECTURE.md:1331` | Not duplicated | F/G carry | `restart/audit/pass-3-runtime/PASS-3.md:342` | AMEND |
| Generated LOC budget is per grammar. | `restart/ARCHITECTURE.md:1320-1331` | Migration status rows | `restart/MASTER-PLAN.md:641-652` | PASS-2 budget discipline | KEEP-WITH-YAML-CORRECTION |
| Future grammar admission is proof against overfitting. | `restart/ARCHITECTURE.md:1275-1306` | `restart/MIGRATION.md:749-759` | `restart/MASTER-PLAN.md:428`; `restart/MASTER-PLAN.md:744` | Lock 14 | KEEP-WITH-EXAMPLE |
| Public/dev crate split is settled. | `restart/ARCHITECTURE.md:38-63` | `restart/MIGRATION.md:282-297` | `restart/MASTER-PLAN.md:227`; `restart/MASTER-PLAN.md:524` | README package names | KEEP |
| Lock 11 path-dep incubation has stability gate. | `restart/ARCHITECTURE.md:58-62` | Migration package rows | `restart/MASTER-PLAN.md:524`; terse at `restart/MASTER-PLAN.md:677` | `restart/locks/14-LOCKS.md:54` | AMEND-CITATION |
| Lock 14 metadata/onboarding citation points to lock text. | `restart/ARCHITECTURE.md:1000` | Not duplicated | §24 consumes diagnostics | `restart/locks/14-LOCKS.md:60`, not 69-72 | AMEND-CITATION |
| Benchmark SOTA rows have competitor anchors. | `restart/ARCHITECTURE.md:1252-1259` | Benchmark metadata gate | `restart/MASTER-PLAN.md:129-136` | `restart/corpora/SOTA.md:54-56` | AMEND-PROVENANCE |
| Benchmark metadata captures machine/build/input. | Not fully in ARCH | `restart/MIGRATION.md:344` | `restart/MASTER-PLAN.md:143-149`; `restart/MASTER-PLAN.md:727` | SOTA corpus | KEEP |
| WASM V1 uses competitor-anchored gate. | ARCH backend rows | Migration receivers | `restart/MASTER-PLAN.md:459`; `restart/MASTER-PLAN.md:753` | Lock 8 | AMEND-HOST-MATRIX |
| Incremental parser fallback has thresholds. | Runtime architecture references hooks | `restart/MIGRATION.md:417` | `restart/MASTER-PLAN.md:490-501` | `restart/audit/pass-3-runtime/PASS-3.md:181-190` | AMEND-MASTER-BINDING |
| LSP does not diverge from batch parser. | Runtime/path diagnostics | Migration LSP row | `restart/MASTER-PLAN.md:726` | PASS-3 fallback table | KEEP-WITH-THRESHOLD-BIND |
| Declaration-crate escape valve has owner/reviewer/deletion path. | `restart/ARCHITECTURE.md:723-760` | `restart/MIGRATION.md:777` | `restart/MASTER-PLAN.md:739`; `restart/MASTER-PLAN.md:722` | Lock 14 | KEEP |
| Archive closure is active before production work. | Architecture conflict ledger | `restart/MIGRATION.md:785-795` | `restart/MASTER-PLAN.md:745`; `restart/MASTER-PLAN.md:728` | Lock 12 | KEEP |

Ledger conclusion:

- The binding ledger is strong enough for amendment, not redraft.
- The highest-risk live items are exactly where a formal sketch or table can mislead implementers.
- Worked examples should be treated as load-bearing because MASTER §24 now routes user confusion to cookbook pages and diagnostics.

## §7 Deduped Punch List

| ID | Path:line | Surgery | Acceptance gate | Lens origin |
|---|---|---|---|---|
| P1 | `restart/ARCHITECTURE.md:1081` | Replace declaration-only `HostDecl ... ";"` with PASS-1 block-bodied `HostFn ... HostAttrs? Block`; ensure no top-level bodyless host production remains. | `rg -n 'HostDecl.*;|HostFn.*;' restart/ARCHITECTURE.md` returns zero except rejection prose; PASS-1 line 183 stays the canonical form. | A2, B5, E2, F3 |
| P2 | `restart/ARCHITECTURE.md:1079` | Replace `MapExpr ::= Expr "=>" TypeExpr` with the canonical `MapTail ::= "->" ChainExpr` / `ChainExpr ::= Ident { "->" Ident }`, or cite PASS-1 if the sketch is intentionally abbreviated. | `rg -n '=> TypeExpr|MapExpr' restart/ARCHITECTURE.md` returns zero or only a retired-syntax rejection row. | A3, F4 |
| P3 | `restart/ARCHITECTURE.md:1077-1078` | Clarify that method-chain syntax is legal only inside `@host fn` bodies, not as a rule-level `HostCall` chain surface. | Architecture grammar sketch and PASS-1 line 217 agree on rule-level vs host-body chain syntax. | A3, B5 |
| P4 | `restart/MASTER-PLAN.md:329` | Change `@host fn declarations` to block-bodied `@host fn` definitions and host primitive registry. | `rg -n '@host fn declarations' restart/MASTER-PLAN.md restart/ARCHITECTURE.md` returns zero. | B5, E2 |
| P5 | `restart/ARCHITECTURE.md:1331` | Insert the missing YAML `Host route` cell from PASS-3 feeder row before the generated LOC cell. | `awk -F'|' 'NR>=1320 && NR<=1331 { print NR, NF-1 }' restart/ARCHITECTURE.md` reports 11 pipe-delimited cells for every row. | A4, D1, E5, G1 |
| P6 | `restart/ARCHITECTURE.md:1000` | Replace `restart/locks/14-LOCKS.md:69-72` with the actual Lock 14 citation at line 60 or cite a separate source that truly owns metadata/onboarding diagnostics. | Manual check: referenced line names Lock 14, not lane scaffolding. | H1 |
| P7 | `restart/MASTER-PLAN.md:524` | Replace Lock 11 citation from `restart/locks/14-LOCKS.md:60` to `restart/locks/14-LOCKS.md:54`. | Manual check: J.W3 cites Lock 11 line 54; line 60 remains Lock 14 only. | E4, H2 |
| P8 | `restart/ARCHITECTURE.md:1256` | Add the `json/canada` simd-json comparator present in MASTER, or state why Architecture’s SOTA table intentionally records only one comparator. | ARCH and MASTER canada rows carry the same competitor set and SOTA source citation. | A3, H3, G4 |
| P9 | `restart/MASTER-PLAN.md:764`; `restart/ARCHITECTURE.md:270-272` | Add or route a compact worked example showing one JSON query through `pointer!` and one structural query through `select!`, including canonical macro grammar-prefix syntax and one diagnostic failure. | Example syntax agrees with README and PASS-3 diagnostics; `BBNF-POINTER001/002/003` remain referenced. | B4, C2, H4 |
| P10 | `restart/ARCHITECTURE.md:1275-1306`; `restart/MASTER-PLAN.md:770` | Add or route an end-to-end yaml onboarding example: `yaml.bbnf`, workspace metadata, generated runtime files, path schema, host route, zero Rust edits, parity fixture deferred. | Future grammar test explicitly checks zero Rust edits and zero `fixtures/yaml/` during onboarding. | C1, D1, G1 |
| P11 | `restart/MASTER-PLAN.md:490-501` | Bind I.W incremental parsing to PASS-3 numeric thresholds for token-span reuse and full-reparse fallback. | MASTER cites the JSON/CSS reuse percentages and fallback limits from PASS-3 lines 185-186 or routes them to a named I.W gate. | C3, D2 |
| P12 | `restart/MASTER-PLAN.md:393`; `restart/ARCHITECTURE.md:1027` | Add or route a worked `@error(recover = ...)` example covering grammar branch, generated diagnostic, and recovery node. | Cookbook/error example names `@error(recover = ...)` and forbids standalone `@recover`. | C4, D4 |
| P13 | `restart/MASTER-PLAN.md:459`; `restart/MASTER-PLAN.md:753` | Add a WASM host primitive ABI matrix gate: exported function names, host-call shape, marshalling rule, and primitive coverage. | H.W3 report must fail if WASM host primitives lack exported names or a host-call shape row. | D5 |
| P14 | `restart/MASTER-PLAN.md:225-246`; `restart/MASTER-PLAN.md:380-407`; `restart/MASTER-PLAN.md:520-526` | Add one grammar trajectory row or appendix that traces a single grammar from A skeleton through F generated runtime to J publication readiness. | The trajectory references the same grammar across A, F, and J and records gates that close each handoff. | C5, G4 |
| P15 | `restart/MASTER-PLAN.md:677` | Optional polish: expand Lock 11 owner row to mention the stable-surface vs incubation-cleared split already present in J.W3. | Lock ownership table and J.W3 carry the same package-publication policy. | D6, E4 |

Punch-list priority:

- P1 through P7 are amendment blockers.
- P8 through P14 are amendment-level clarity gaps because they affect public or implementer-facing gates.
- P15 is polish and can ride with the same amendment if touched.

## §8 V1->V4 History Note

V1 result:

- `HARDENING-MASTER-PLAN.md` and consolidated V1 found broad contradictions.
- Major V1 themes were BIR ownership, path crate naming, `pointer!`, layout, cursor skip, BBNF surface drift, yaml onboarding, generated LOC, SOTA, OpenFrame, and fixture role.
- V1 was amendment-required.

V2 result:

- `HARDENING-MASTER-PLAN-V2.md` and consolidated V2 moved to READY after a gate-oriented closure pass.
- V2 was effective at surface reconciliation.
- V2 did not exhaust cross-document line-level binding.

V3 result:

- `HARDENING-MASTER-PLAN-V3.md` and consolidated V3 reopened amendment-required status.
- V3 found nine important misses: Lock 2 path canon, Lock 11 publication split, `bbnf-bench` public/dev status, H.W3 WASM anchor, per-grammar LOC source-of-truth, diagnostic vocabulary, carry-ledger asymmetry, H.W2 metadata phrasing, and json/canada SOTA delta.

V4 result:

- `HARDENING-MASTER-PLAN-V4.md` and consolidated V4 returned the trio to READY.
- V4 verified that the wave-4.1 synthesis amendment had landed the V3 misses.
- V4 left only residual polish: per-crate child-count audit, `regen_wall` token form, and yaml provisional LOC owner.

V5 delta:

- V5 does not contest V4’s closure of the large structural carries.
- V5 applies a harder Phase 0 lens to formal fragments, examples, lock-line citations, and public-surface proof paths.
- V5 found stale fragments that V4’s broader carry audit did not catch.
- The carry lesson is specific: once a surface is declared settled, formal grammar sketches and example syntax must be audited as contracts, not as illustrative prose.

## §9 LLM-Pathology Summary

LLM bias subclasses observed:

- Plausible stale syntax survived because it looked compiler-like.
- Formal grammar snippets lagged behind narrative amendments.
- Cookbook rows were allowed to stand in for worked examples.
- Line citations were treated as decorative anchors until checked.

Overfitting subclasses observed:

- YAML is protected by two-surface onboarding, but the malformed row weakens the proof.
- JSON/CSS benchmark fixtures dominate SOTA proof; a non-hot grammar trajectory would help.
- Path/query examples need a generic schema story rather than grammar-specific intuition.

Hallucination/provenance subclasses observed:

- Two citations point to real files but wrong lines.
- `json/canada` competitor provenance is not mirrored between ARCHITECTURE and MASTER.
- The public macro examples do not name one canonical grammar-prefix rule.

Pathology boundary:

- No new architecture should be invented in response.
- The amendment should correct stale fragments, add compact examples, and tighten citations.
- Research fold work can then proceed from a cleaner trio.

## §10 Verdict

Verdict:

- **AMENDMENT-REQUIRED**

Not READY because:

- P1, P2, P5, P6, and P7 are hard source faults.
- P3 and P4 prevent the same `@host fn` drift from re-entering wave text.
- P9 through P14 are public-surface proof gaps that matter for MASTER-PLAN as an execution document.

Not RE-DRAFT because:

- Tranche topology remains coherent.
- Migration routing remains coherent.
- Lock ownership remains coherent.
- Gate structure remains coherent.
- The amendment can be applied without changing the plan’s architecture.

Acceptance standard for re-verdict:

- P1 through P14 resolved or explicitly routed with receiver, blocker, and gate.
- The 16-command gate rerun remains no worse than this V5 result.
- Formal grammar sketch agrees with PASS-1.
- YAML table cell count matches the header.
- Lock citations resolve to the named locks.
- Public query, yaml onboarding, recovery, incremental, and WASM host examples/gates have concrete anchors.

## §11 Closing Posture

Recommended posture:

- Apply a narrow trio amendment.
- Do not redraft tranche sequence.
- Do not reopen closed V1-V4 items unless one of the new fixes touches them.
- Keep PASS files as ground truth where they already own the detail.

Estimated amendment wall time:

- 2.5 to 4.0 hours for a careful amendment pass.
- 45 minutes for formal grammar and citation corrections.
- 45 minutes for YAML table and SOTA provenance correction.
- 60 to 120 minutes for compact worked examples and acceptance-gate wiring.

Stop condition for the amendment:

- The trio should become READY after the narrow fixes above.
- Any attempt to redesign tape, BIR, layout, or tranche order is outside the V5 finding set.
