# REVIEW-C — Lock 14 + Greenfield-Discipline Depth Audit

## §1 Audit target identification

| Audit target | Path | Lines | Commit |
|---|---|---:|---|
| HARDENING-CONSOLIDATED.md | `restart/audit/hardening/HARDENING-CONSOLIDATED.md` | 619 | `1cf6dac0` |
| ARCHITECTURE.md | `restart/ARCHITECTURE.md` | 1259 | `015317db` |
| MIGRATION.md | `restart/MIGRATION.md` | 740 | `015317db` |
| MASTER-PLAN.md | `restart/MASTER-PLAN.md` | 727 | `015317db` |
| PASS-1 synthesis | `restart/audit/pass-1-substrate/PASS-1.md` | 235 | `015317db` |
| PASS-2 synthesis | `restart/audit/pass-2-codegen/PASS-2.md` | 467 | `015317db` |
| PASS-3 synthesis | `restart/audit/pass-3-runtime/PASS-3.md` | 382 | `015317db` |
| 14-LOCKS.md | `restart/locks/14-LOCKS.md` | 249 | `015317db` |
| README.md | `restart/README.md` | 475 | `015317db` |

| Audit lens | Reviewer C — Lock 14 + greenfield-discipline depth |
|---|---|
| Six lanes | yaml two-surface proof; per-X grammar tables; declaration-crate fence; OpenFrame retiral; convergent pivot sequencing; greenfield discipline |
| Hardening punch items in scope | #11 (yaml proof), #12 (fixture separation), #13 (per-X table), #15 (declaration-crate review form), #41 (C/E/H sequencing), #46 (OpenFrame deletion) |
| Spirit asks | "everything is grammar-derived"; "no quick solutions, no workarounds, no legacy code uncontested" |
| Letter asks | exactly two onboarding surfaces; fence form complete; OpenFrame retiral verified; consumer-coupled sequencing |

The cohort verdict from HARDENING-CONSOLIDATED is AMENDMENT-REQUIRED (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:14`). Reviewer C verifies whether the four-target hardening's surfacing of these six concerns was sufficient or whether the corpus carries residual breaches that escaped the consolidated punch list.

---

## §2 Lane 1 — Yaml onboarding two-surface proof

The hardening's punch item #11 admits exactly two changes (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:230-233`): grammar source plus workspace metadata; zero Rust edits, zero generic-crate diff, zero per-grammar match arms, zero declaration crate, zero fixture allowance.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| Onboarding allowed-change list is exactly two surfaces | `restart/ARCHITECTURE.md:1170-1175` lists `grammars/yaml.bbnf` and `Cargo.toml [workspace.metadata.bbnf.grammars.yaml]` | Two lines under "Allowed changes"; no third surface | honoured |
| Forbidden-change list excludes fixtures | `restart/ARCHITECTURE.md:1177-1186` enumerates Rust source, Cargo manifests, parser registries, path registries, host shims, per-grammar declaration crates | The `crates/*/src/**/*.rs` rule excludes hand-written runtime; `runtime/src/grammars/yaml` is the only generated allowance | honoured |
| Verification commands cite concrete grep + cargo invocations | `restart/ARCHITECTURE.md:1188-1196` and `restart/MIGRATION.md:691-697` | `cargo xtask bbnf check yaml`, `cargo xtask bbnf build yaml`, `cargo test`, `git diff -- crates ':!crates/runtime/src/grammars/yaml'`, `rg "yaml|Yaml" crates/*/src` | honoured |
| Test is gated at a specific tranche/wave | `restart/MASTER-PLAN.md:412` (G.W4), `restart/MASTER-PLAN.md:420` (`cargo test -p test-fixtures future_grammar_yaml`), `restart/MASTER-PLAN.md:688` (carry row owner A/F/G/J) | Tranche G owns the proof; A seeds metadata, F seeds runtime template | honoured |
| Master plan close gate row references yaml | `restart/MASTER-PLAN.md:110` (Future grammar add — A/G/F) | Hard-gates table names yaml as Lock 14 close proof | honoured |
| MIGRATION §19.6 future-grammar gate | `restart/MIGRATION.md:691-701` | Inlines the same five-line script with the two-surface diff invariant | honoured |
| PASS-1 onboarding proof admits two surfaces | `restart/audit/pass-1-substrate/PASS-1.md:181-187` | Three rows (source, metadata, generate); the third is "xtask-emitted runtime/path/visitor metadata" — generated output, not an onboarding surface | honoured |
| PASS-2 onboarding smoke admits two surfaces | `restart/audit/pass-2-codegen/PASS-2.md:312-318` | Five rows with explicit "Runtime emission ... is generated from BIR/runtime template only" — runtime is derivative, not onboarding | honoured |
| Architecture §11 generated LOC budget keeps yaml separate from seed-grammar baseline | `restart/ARCHITECTURE.md:1158-1161` | "New yaml grammar | Reported separately until admitted as a seed grammar." — the +2% ceiling does not apply to yaml during onboarding | honoured |
| Fixture-allowance fault from MASTER-PLAN earlier hardening | hardening §3 conflict #7 (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:148`) demanded the removal of `fixtures/yaml/*` | `rg "fixtures/yaml" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` returns zero hits; only the two surfaces remain | honoured |

**Lane 1 verdict: HONOURED**. The post-amendment two-surface proof is consistent across ARCHITECTURE.md §12, MIGRATION.md §19.6, MASTER-PLAN.md §12 G.W4, MASTER-PLAN.md §24 carry row, PASS-1 §6 future-grammar table, and PASS-2 §5 future-grammar smoke. No fixture leak. The verification commands are concrete (rg, find, cargo, git diff). The test is gated at G.W4 with seed support from A.W2 (metadata schema) and F.W5 (runtime template).

The sole minor-grade ambiguity: `restart/MASTER-PLAN.md:412` reads "yaml enters through grammar source plus metadata; generated runtime is derivative." The phrasing "derivative" is correct in spirit (runtime emission flows from BIR + template) but does not name the third "surface" admitted (generated runtime under `runtime/src/grammars/yaml/`). MIGRATION.md §19.6 makes the rule explicit: `git diff -- crates ':!crates/runtime/src/grammars/yaml'` excludes generated yaml runtime from the diff. Architecture §12 names this explicitly at line 1198-1199. The corpus collectively honours the two-input / one-derivative-output contract.

---

## §3 Lane 2 — Per-X grammar proof completeness

The hardening's punch item #13 demands per-X tables for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, and yaml across columns: typed root, ValueRef, runtime files, visitor, path schema, fixture manifest, host route, generated LOC, declaration-crate status (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:241-246`).

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| ARCHITECTURE.md owns a per-X grammar table at architecture-authoritative scope | walk `restart/ARCHITECTURE.md` looking for table that enumerates the 9+1 grammars across the demanded columns | No table at architecture authority enumerates the 10 grammars × 9 columns. Architecture §11 has a generated-LOC budget row family but no per-grammar enumeration; §5.6 declaration-crate review form has no per-grammar declaration-crate exception table | violated-with-recommendation |
| MASTER-PLAN.md owns or cites a per-X grammar table | walk `restart/MASTER-PLAN.md` for "all grammars" / "every grammar" / "nine seed" enumerations | `restart/MASTER-PLAN.md:212` "Metadata validation accepts current nine grammars."; `restart/MASTER-PLAN.md:380` "Current nine grammar regeneration."; `restart/MASTER-PLAN.md:608` "Current nine seed grammars regenerate." — claims enumerated as "nine seed", but no per-X table grounds them | violated-with-recommendation |
| PASS-2 owns the per-X generated LOC table | `restart/audit/pass-2-codegen/PASS-2.md:324-335` | Table covers nine grammars × two columns (current, max) — only one of the nine columns demanded by punch item #13 | partial |
| PASS-2 owns the per-X runtime emission table | `restart/audit/pass-2-codegen/PASS-2.md:391-403` | Table covers ten grammars (nine + yaml) × two columns (runtime module, required smoke) — covers `runtime files` column but not the other seven | partial |
| PASS-1 per-X broad-claim table | `restart/audit/pass-1-substrate/PASS-1.md:188-194` | Three-row table at high-level claim granularity ("normal grammars need no declaration crate"; "all backends consume Backend IR"; "all grammar variation is data or generated code") — proof anchors but no column matrix | partial |
| HARDENING-CONSOLIDATED.md punch row #13 escalation | `restart/audit/hardening/HARDENING-CONSOLIDATED.md:241-246` | Surgery: "Add a table for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, and yaml. Gate: table columns include typed root, ValueRef, runtime files, visitor, path schema, fixture manifest, host route, generated LOC, and declaration-crate status." — the punch demands a 10×9 table | acknowledged but unfulfilled in current corpus |
| "all-X" claims in Master/Architecture without per-X backing | `restart/MASTER-PLAN.md:212`, `:380`, `:608`; `restart/ARCHITECTURE.md:1156-1161` ("Nine seed grammars total"); `restart/ARCHITECTURE.md:543` ("No directory may become a dumping ground"); `restart/MIGRATION.md:220`, `:259`, `:261` — multiple "all X grammars" / "nine grammars" references | These claims rely on PASS-2's two partial tables (LOC + runtime emission) plus PASS-1's high-level row table. No single architecture-owned table joins typed root, ValueRef, visitor, path schema, fixture manifest, host route, generated LOC, and declaration-crate status across the ten grammars | violated-with-recommendation |
| Per-X declaration-crate status table | `restart/audit/pass-1-substrate/PASS-1.md:71` "Exception table is empty for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, and math" | Names eight grammars; omits math? No — eight named, plus the row — actually nine (bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math); yaml is not named because yaml is the test grammar entering through onboarding. The empty exception table is asserted but not in tabular form. ARCHITECTURE.md §5.6 inlines the form (`restart/ARCHITECTURE.md:711-719`) but does not show a per-grammar exception ledger | partial |

**Lane 2 verdict: HARDENING SURFACING INSUFFICIENT**. Punch item #13 is acknowledged in the consolidated report (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:241-246`) and routed to the SYNTHESIS amendment (`:488` row). The current synthesis trio (ARCHITECTURE/MIGRATION/MASTER-PLAN) does not yet land the demanded 10×9 table. The available material is fragmented:

- PASS-2 generated LOC table — 9 grammars × 2 columns
- PASS-2 runtime emission table — 10 grammars × 2 columns
- PASS-1 broad-claim table — 3 rows × 3 columns
- PASS-1 declaration-crate exception table — prose ("Exception table is empty for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, and math")
- ARCHITECTURE §11 LOC budget — row scope, no per-grammar enumeration

The consolidated `Architecture-owned` table demanded by punch #13 surgery does not yet exist in `restart/ARCHITECTURE.md` or `restart/MASTER-PLAN.md`. The hardening's surfacing is correct (the punch is in the queue) but the current corpus has not landed the surgery; this is a known amendment debt (the cohort verdict is AMENDMENT-REQUIRED), not a deeper review-C breach. Reviewer C ratifies the punch and recommends no additional surfacing.

**Surgical recommendation**: Architecture §12 (Future Grammar Onboarding Test) adds an immediately-following §12.1 "Per-grammar onboarding ledger" with rows: bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, yaml. Columns per punch #13: `typed root`, `ValueRef`, `runtime files`, `visitor`, `path schema`, `fixture manifest`, `host route`, `generated LOC`, `declaration-crate status`. Cite the table from PASS-2 (LOC) and from `[workspace.metadata.bbnf.grammars.<name>]` blocks (host route, fixture manifest).

---

## §4 Lane 3 — Declaration-crate fence

The hardening's punch item #15 (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:253-257`) demands a review form with reason, owner, why metadata + @host fn fail, declaration location, no generic import, deletion path, reviewer, receiving gate. The exception table must be empty for the 9 extant grammars.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| Onboarding default routes through metadata + @host fn | `restart/README.md:11-25` ("two onboarding surfaces"); `restart/ARCHITECTURE.md:1015-1027`; `restart/MASTER-PLAN.md:18-21`; `restart/MIGRATION.md:14-16` | Composition is the canonical surface; declaration-crate is "rare last resort" | honoured |
| The 9 extant grammars carry no declaration crate | `restart/audit/pass-1-substrate/PASS-1.md:71` "Exception table is empty for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, and math" | Names all nine; PASS-2 §1 conflict ledger ratifies the same (`restart/audit/pass-2-codegen/PASS-2.md:16`) | honoured |
| Declaration-crate fence form lives in ARCHITECTURE.md | `restart/ARCHITECTURE.md:711-719` | Five fields: Grammar, Failed surface, Scope, Deletion path, Review gate | violated-with-recommendation |
| The form covers all eight punch-item fields (reason, owner, why metadata + @host fn fail, declaration location, no generic import, deletion path, reviewer, receiving gate) | inspect `restart/ARCHITECTURE.md:711-719` for each field | "Failed surface" covers "why metadata + @host fn fail"; "Scope" overlaps "declaration location" partially; "Deletion path" matches; "Review gate" covers "reviewer + receiving gate" approximately. Missing: explicit `owner` field; explicit `no generic import` rule (mentioned in row description but not as form field); the "Reason" field is implicitly merged with "Failed surface" rather than given its own row | violated-with-recommendation |
| Metadata schema enforces the form | `restart/ARCHITECTURE.md:675-676` (`allow_declaration_crate = false`, `declaration_crate_reason = ""`); `restart/ARCHITECTURE.md:703` ("`allow_declaration_crate = true` requires an explicit reason and review gate"); `restart/MASTER-PLAN.md:683` ("Metadata validator rejects `allow_declaration_crate = true` without full review fields") | Two metadata fields exist; the validator rejects bare `true` flag, but the schema does not reify all eight punch fields as TOML keys | partial |
| Lock 14's option (c) (declaration crate) is the rare last resort | `restart/locks/14-LOCKS.md:60` ("optionally a per-grammar declaration crate ... carrying host-fn implementations"); README §1 "the optional declaration crate (Lock 14's escape valve) is not used for any of the 9 extant grammars" (`restart/README.md:13`); MIGRATION.md §1 ("Per-grammar declaration crates | Not default. A rare escape valve must be explicit and fenced.") | The carry row at MASTER-PLAN.md:683 names the metadata gate. The escape-valve discipline reads as last-resort across all sources | honoured |
| Default escape-valve discipline is verified by greenfield text | `restart/MASTER-PLAN.md:325` ("No declaration crate is introduced unless the rare escape-valve gate is used.") and `restart/audit/pass-1-substrate/PASS-1.md:62-72` (escape-valve fence with five fields including "Approval owner", "Failure proof", "Location", "Import rule", "Extant grammars", "Verification") | PASS-1 fence has six fields including "Approval owner" and "Import rule" — broader than ARCHITECTURE §5.6's five fields. The corpus has TWO different fence shapes. Architecture's version is the executable authority but is missing fields that PASS-1 names | violated-with-recommendation |

**Lane 3 verdict: HARDENING SURFACING INSUFFICIENT**. Punch #15 is in the queue but the consolidated report's surgery (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:253-257`) lists eight required fields and the executable authority `restart/ARCHITECTURE.md:711-719` carries five. PASS-1's earlier escape-valve fence (`restart/audit/pass-1-substrate/PASS-1.md:62-72`) carries six. The two forms are not aligned.

The escape-valve discipline (Lock 14 option (c) is genuinely a last resort) is honoured at the level of intent: README, ARCHITECTURE.md §0 conflict ledger, MIGRATION.md §1 disposition table, MASTER-PLAN.md §1 ledger, and PASS-1/PASS-2/PASS-3 syntheses all declare default routing via metadata + `@host fn` and treat the declaration crate as fenced exception. The 9 extant grammars carry no declaration crate (asserted in PASS-1 §2; ratified in PASS-2 §1).

**Surgical recommendation A**: ARCHITECTURE §5.6 declaration-crate review form expands from five fields to the eight named in `restart/audit/hardening/HARDENING-CONSOLIDATED.md:255`: (1) `Reason` — the architectural pressure forcing the escape; (2) `Owner` — the review-form author and the maintaining team; (3) `Failed surface` — why metadata + `@host fn` cannot express the boundary; (4) `Declaration location` — exact crate path under `crates/<grammar>/`; (5) `No-generic-import rule` — the lint that prevents generic crates from importing the declaration crate; (6) `Deletion path` — the condition that lets the declaration crate retire; (7) `Reviewer` — the architecture-owner approving the exception; (8) `Receiving gate` — the close-gate row in the receiving tranche.

**Surgical recommendation B**: PASS-1 §2 escape-valve fence is reconciled to the same eight fields (via a SYNTHESIS amendment per `restart/audit/hardening/HARDENING-CONSOLIDATED.md:484-489` route table). The two-shape divergence retires.

**Surgical recommendation C**: ARCHITECTURE.md §5 metadata schema reifies all eight fields as TOML keys (currently only `allow_declaration_crate` and `declaration_crate_reason`). Add `[workspace.metadata.bbnf.grammars.<name>.declaration_crate]` sub-block with `reason`, `owner`, `failed_surface`, `location`, `no_generic_import_lint`, `deletion_path`, `reviewer`, `receiving_gate`.

---

## §5 Lane 4 — OpenFrame retiral completeness

The hardening's punch item #46 demands no OpenFrame name or substrate survives in generic runtime/codegen plan text (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:451-455`). The greenfield uses generated Backend IR builder frames and TapeBuilder checkpoints. Reviewer C performs `rg -n 'OpenFrame'` across plan files and classifies each match.

| Match path:line | Text | Classification |
|---|---|---|
| `restart/locks/14-LOCKS.md:34` | "the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology"; "OpenFrame ladders" rejected; "type ambivalence (tape and OpenFrame and direct-to-struct competing for the same role)" | pathology citation — Lock 1 records what failed; honoured |
| `restart/README.md:287` | "OpenFrame leakage — not the name" | pathology citation — README §8 explains the failure; honoured |
| `restart/README.md:314` | "the convergent pivot at Tranche F retires OpenFrame across all 9 grammars in a single architectural movement"; "no Vec<OpenFrame> ladder" | retiral plan + pathology citation; honoured |
| `restart/README.md:383` | "the prior failed implementation — orthogonal codepaths, OpenFrame parallel substrate, type ambivalence, the Vec<OpenFrame>::clone 86.07% pathology" | pathology citation — Lock 1 reframe; honoured |
| `restart/README.md:465` | "OpenFrame had not actually retired across all 9 grammars" | pathology citation — provenance §14; honoured |
| `restart/ARCHITECTURE.md:23` | "Lock 1 rejects parallel substrates and OpenFrame ladders" | pathology citation; honoured |
| `restart/ARCHITECTURE.md:52` | "Replaces hand-written per-grammar runtime dirs and OpenFrame-heavy flow" | retiral citation — runtime crate role description; honoured |
| `restart/ARCHITECTURE.md:871` | "Must not clone OpenFrame stacks" — `SpeculativeAlt` BIR variant lowering rule | runtime invariant gate — generic runtime/codegen plan text; **review needed** |
| `restart/ARCHITECTURE.md:926` | "OpenFrame clone stacks are absent. | Generated code review plus perf gate." — Backend IR invariant | invariant gate — generic plan text; **review needed** |
| `restart/ARCHITECTURE.md:1075` | "Rollback is bounded and does not clone OpenFrame stacks." — runtime tape invariant | invariant gate — generic runtime plan text; **review needed** |
| `restart/ARCHITECTURE.md:1138` | "OpenFrame clone absence | `runtime`, `codegen`, `bbnf-bench`." — Lock 1 perf gate ownership | gate-ownership citation; honoured |
| `restart/MASTER-PLAN.md:26` | "and OpenFrame ladders" — Lock 1 inheritance | pathology citation; honoured |
| `restart/MASTER-PLAN.md:113` | "Tape/direct substrate | Runtime tests, no OpenFrame clone stack, no ParseStream runtime concept." | gate row; honoured |
| `restart/MASTER-PLAN.md:235` | "Tape/direct substrate and no OpenFrame ladders" — Tranche B inheritance | pathology citation; honoured |
| `restart/MASTER-PLAN.md:237` | "Remove OpenFrame clone pressure" — Tranche B inheritance | retiral citation; honoured |
| `restart/MASTER-PLAN.md:244` | "Speculative branch test without OpenFrame clone." — B.W1 consumer gate | gate row; honoured |
| `restart/MASTER-PLAN.md:254` | `rg "OpenFrame|Vec<OpenFrame>|ParseStream" crates/runtime/src crates/codegen/src` — Tranche B hard close grep | grep gate — not a preservation; honoured |
| `restart/MASTER-PLAN.md:574` | "Tape/direct runtime exists and old OpenFrame/ParseStream runtime concepts are blocked." — Tranche B close migration state | retiral citation; honoured |
| `restart/MASTER-PLAN.md:628` | "Lock 1 ... | B/F/H | Runtime tests, no OpenFrame clone stack." — Lock 1 ownership row | gate row; honoured |
| `restart/MIGRATION.md:284` | "OpenFrame/checkpoint-heavy fallback logic | ABROGATE-REPLACE | Tape builder with bounded checkpoints." — `crates/core` runtime disposition | retiral disposition; honoured |
| `restart/MIGRATION.md:287` | "The restart sketch measured `Vec<OpenFrame>::clone` at 86.07 percent inclusive samples in the current path" | pathology citation; honoured |
| `restart/MIGRATION.md:289` | "The new runtime must prove OpenFrame clone stacks are gone." | retiral mandate; honoured |
| `restart/MIGRATION.md:669` | `rg "OpenFrame|Vec<OpenFrame>|ParseStream" crates/runtime/src crates/codegen/src` — §19.4 runtime gate | grep gate; honoured |
| `restart/MIGRATION.md:676` | "no old OpenFrame clone stack or ParseStream runtime concept." — §19.4 expected result | gate result; honoured |
| `restart/audit/pass-1-substrate/PASS-1.md:46` | "no OpenFrame clone stack; rollback is bounded" — Backend IR Dispatch/speculation invariant | invariant gate — generic plan text; **review needed** |
| `restart/audit/pass-2-codegen/PASS-2.md:7` | "Discard ... OpenFrame checkpointing" — verdict ledger | retiral citation; honoured |
| `restart/audit/pass-2-codegen/PASS-2.md:36` | "avoids the prior OpenFrame checkpoint clone that dominated samples" — Tape/direct commitment | retiral citation; honoured |
| `restart/audit/pass-2-codegen/PASS-2.md:366` | "Remove OpenFrame checkpoint cost | ... TapeBuilder length checkpoints. | samply confirms no `Vec<OpenFrame>::clone`." — perf trajectory | retiral plan; honoured |
| `restart/audit/pass-2-codegen/PASS-2.md:455` | "Build `runtime/src/tape/` and TapeBuilder checkpoints; delete OpenFrame-style runtime builders during migration." — punch list | retiral mandate; honoured |

**Reviewer C interpretation of "review needed" rows**: the four flagged rows (`restart/ARCHITECTURE.md:871`, `:926`, `:1075`; `restart/audit/pass-1-substrate/PASS-1.md:46`) all phrase the OpenFrame name as a *negative* invariant — "must not clone OpenFrame stacks" / "OpenFrame clone stacks are absent" / "no OpenFrame clone stack" — not as preservation. Each is paired with the active design (TapeBuilder length checkpoints; bounded rollback). Per the hardening's classification rule (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:565`): *"`rg -n "OpenFrame" restart/audit/pass-1-substrate restart/audit/pass-2-codegen restart/MASTER-PLAN.md` classifies every match as deletion pathology, never preservation."* — these four rows pass the classification: they cite the failure mode by name to gate the new design's correctness. No row says "use OpenFrame as backend stack" or otherwise preserves the substrate.

**Lane 4 verdict: HONOURED**. Every OpenFrame match in plan text classifies as pathology citation, retiral plan, gate row, grep gate, retiral disposition, or invariant gate. None preserves OpenFrame as substrate.

The hardening's surfacing was sufficient: punch #46 is in the queue, the four-target hardening flagged the residue, and the post-amendment corpus aligns. The single observation: Lock 1 in `restart/locks/14-LOCKS.md:34` still names "Tape + columnar dead" as the lock title even though the 2026-05-04 reframe explicitly retires the wholesale tape rejection. This is honoured as historical-record archaeology because the lock body inlines the reframe.

---

## §6 Lane 5 — Convergent pivot sequencing

The hardening's punch item #41 (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:419-423`) reads: "C.W3/C.W5 produce recognizer/extraction facts before real BIR + Pratt/SIMD consumers exist. Surgery: Give C.W3/C.W5 same-wave BIR snapshot consumers or move recognizer/extraction proof into E/H where real BIR and Pratt/SIMD consumers exist."

Punch #40 (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:413-417`): "Move ShapeFacts before B.W3, split B.W3 into shell plus C-owned materialization, or change C.W2's consumer away from B direct builder."

| Wave | What it produces | Who consumes (same wave / next wave / later) | Era V failure check | Verdict |
|---|---|---|---|---|
| A.W0 | Branch/tag preflight, archive `ser`+`gorgeous`, remove from workspace | A.W1 (workspace skeleton consumes archive-clean state) | same-wave / next-wave consumer | honoured |
| A.W1 | 24 crate skeletons | A.W2 (metadata schema) and B.W0 (runtime tape) | next-wave consumer | honoured |
| A.W2 | Root metadata schema + validator | A.W3 (grammar can parse seed `.bbnf`); F.W5 (regen consumer); G.W4 (yaml gate) | next-wave consumer; later-wave routing complete | honoured |
| A.W3 | source/error/grammar minimal APIs | C.W0 (Grammar IR builder) | next-wave consumer | honoured |
| A.W4 | Generalization + tree-shape lint gates | A through J (all subsequent tranches) | continuous consumer; lock 14 grep gate | honoured |
| B.W0 | `runtime/src/tape` tokens, spans, append builder | B.W1 (checkpoints), B.W2 (DocumentView), B.W3 (direct builder) | same-wave / next-wave consumer | honoured |
| B.W1 | Bounded checkpoints + rollback | B.W2 (DocumentView), F.W1 (tape/direct emit) | next-wave / later consumer | honoured |
| B.W2 | DocumentView, OwnedDocument, NodeView, TokenView | B.W3 (direct builder), G.W2 (ValueRef), I.W2 (LSP) | next-wave / later consumer | honoured |
| B.W3 | Direct builder shell + tape identity hooks | B.W4 (seed grammar shell); F.W1 (tape/direct emit consumer); **C.W2 ShapeFacts** | The hardening punch #40 flagged this: B.W3 builds direct views before C.W2 produces ShapeFacts, while C.W2 says the direct builder consumes those facts. Post-amendment fix: `restart/MASTER-PLAN.md:280` reads "Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps." — punch #40 surgery is partially landed: the C.W2 consumer is now in C with a fixture, recording B integration gaps for later F integration. B.W3 ships as shell only, and C.W2 carries the materialization proof | honoured (post-amendment) |
| B.W4 | Seed generated grammar shell | F.W1 (tape/direct emit); H (recognizer consumers) | later-wave consumer | honoured |
| C.W0 | Grammar IR enum, IDs, spans, validation | C.W1 (TypeFacts producer reads Grammar IR); D.W0 (lookbehind parser); E.W1 (BIR builder consumer) | same-wave / next-wave consumer | honoured |
| C.W1 | TypeFacts and HM/bidirectional core | C.W2 (ShapeFacts depends on TypeFacts); D.W1 (generics typecheck); E.W1 (BIR builder) | same-wave / next-wave consumer | honoured |
| C.W2 | ShapeFacts and value-shape mining | "Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps." (`restart/MASTER-PLAN.md:280`) | Post-amendment: the consumer is C-owned (the C fixture); the cross-wave B integration is recorded as an explicit gap to be closed in F. Era V failure averted | honoured (post-amendment) |
| C.W3 | RecognizerFacts and Pratt/SIMD candidate mining | "Facts feed E-owned BIR snapshots, not placeholder hints." (`restart/MASTER-PLAN.md:281`) | Post-amendment: punch #41 surgery — the C.W3 consumer is named (E-owned BIR snapshots, not placeholder hints). Real consumer in E.W1 (BIR builder) — next-wave consumer; H.W0 (PrattSpine) and H.W1 (SimdScan) are later real consumers. The "not placeholder hints" clause explicitly rejects substrate-without-consumer | honoured (post-amendment) |
| C.W4 | CSP/egraph bridge tables | C.W5 (cost extraction); E.W1 (BIR builder reads bridge facts) | same-wave / next-wave consumer | honoured |
| C.W5 | CostFacts + extraction skeleton | "Backend IR builder receives selected alternatives." (`restart/MASTER-PLAN.md:283`) | Post-amendment: the consumer is named (Backend IR builder = E.W1, next-wave). Era V failure averted | honoured (post-amendment) |
| D.W0-D.W4 | Lookbehind, generics, host fn, chains, error/layout | E.W0 (BIR variant integration), F.W2 (extension seed grammar codegen) | next-wave consumer | honoured |
| E.W0 | Backend IR enum, IDs, validation | E.W1 (Grammar IR + side tables to BIR); F.W0 (Rust lowerer) | same-wave / next-wave consumer | honoured |
| E.W1 | Grammar IR + side tables to BIR builder | E.W2 (VM consumes BIR); F.W0 (Rust lowerer); H.W0 (PrattSpine BIR) | same-wave / next-wave consumer | honoured |
| E.W2-E.W4 | VM interpreter, replay, lowerer trait | F.W0 (Rust lowerer trait consumer); F.W3 (regen-equality); H (recognizers) | next-wave consumer | honoured |
| F.W0-F.W5 | Rust lowerer, tape/direct emit, host calls, runtime template, nine-grammar regen | G.W0 (path-core consumes runtime), G.W4 (yaml proof), H.W0+ (recognizer real activations), I.W2 (LSP consumer) | next-wave / later consumer | honoured |
| G.W0-G.W4 | Path-core, pointer!/select!, ValueRef, visitor mutation, future grammar test | H.W3 (WASM grammar parse), I.W2 (LSP), J.W0 (parity matrix) | next-wave / later consumer | honoured |
| H.W0-H.W5 | Pratt recognizer, SIMD recognizer, AVX2/NEON dispatch, WASM V1, JSON + CSS SOTA gates | J.W1 (final SOTA), J.W2 (docs cite numbers) | next-wave consumer | honoured |
| I.W0-I.W4 | RecoveryFacts, incremental, LSP diagnostics, debug/replay, CLI/LSP parity | J.W0 (parity matrix), J.W2 (docs) | next-wave consumer | honoured |
| J.W0-J.W5 | Parity matrix, final SOTA, docs, package readiness, archive audit, close report | close gate | n/a | honoured |

**Lane 5 verdict: HONOURED (post-amendment)**. Punches #40 and #41 from the consolidated hardening are landed in the current MASTER-PLAN.md text:

- B/C: B.W3 ships shell-only; C.W2 is the materialization proof owner with explicit B integration gap recording (`restart/MASTER-PLAN.md:280`).
- C/E: C.W3 facts feed E.W1 BIR snapshots (next-wave consumer) and downstream H.W0/H.W1 real recognizer activation; "not placeholder hints" (`restart/MASTER-PLAN.md:281`) explicitly forbids substrate-without-consumer.
- C/H: C.W5 CostFacts + extraction skeleton consumed by E.W1 Backend IR builder (next-wave); H.W0/H.W1 activate the recognizers as real consumers.

The Era V failure mode (substrate-first / consumer-later — seven dead substrate crates between AV and AX) does not recur. Every C-tranche side-table producer has a same-wave fixture or next-wave Backend IR consumer named; every recognizer (Pratt, SIMD) has a real activation in H, and the BIR variant consumer is named upstream in E.

The single residual observation: B.W4 ("Seed generated grammar shell") leaks beyond Tranche B's hard close (`restart/MASTER-PLAN.md:251-256`) into F.W1's tape/direct emit. This is acceptable because B.W4's consumer is "One grammar parses through tape/direct shell" — proven within Tranche B — and the deeper generated runtime template lands at F. The seed is not "substrate-without-consumer" because B.W4 itself parses through it.

---

## §7 Lane 6 — Greenfield discipline + no-quick-solutions

The user's stated discipline: no quick solutions, no workarounds, no legacy code uncontested, no contrivance, no overengineering. Reviewer C walks ARCHITECTURE.md and MASTER-PLAN.md for contrivance signals.

| Signal pattern | Path:line | Text | Classification | Verdict |
|---|---|---|---|---|
| "may" as architectural permission | `restart/ARCHITECTURE.md:246` | "The CLI may expose grammar names as user input, but it must not encode those names in Rust switches." | bounded permission with hard rule following — not contrivance | honoured |
| "may" as architectural permission | `restart/ARCHITECTURE.md:291` | "A crate may expose additional test helpers under `cfg(test)` or crate-local integration features, but those helpers are not part of the public contract." | bounded permission — test-only escape, not production contrivance | honoured |
| "may" as architectural permission | `restart/ARCHITECTURE.md:543` | "No directory may become a dumping ground." — Lock 13 negative permission | hard rule | honoured |
| "may" — metadata schema rules | `restart/ARCHITECTURE.md:701-702` | "Metadata may name files, profiles, and feature flags." / "Metadata may not name Rust parser types, generated modules, or builder structs." | bounded positive + negative rule pair | honoured |
| "may" — backend IR variant SIMD note | `restart/ARCHITECTURE.md:870`, `:872`, `:874` | "SIMD may feed discriminator." / "SIMD may accelerate body prefix." / "SIMD may widen compare." | shape-mining permission — auto-detected per Lock 10, not a directive | honoured |
| "may" — regex Unicode | `restart/ARCHITECTURE.md:993` | "Unicode classes may exist inside that regex syntax, but BBNF itself does not expose a set algebra surface." | architectural rule (Unicode delegated below BBNF per README) | honoured |
| "may" — yaml seed admission | `restart/ARCHITECTURE.md:1160` | "Reported separately until admitted as a seed grammar." — yaml LOC budget | future-state condition; not a workaround. The admission criterion is implicit | partial — names "until admitted" without naming the admission gate |
| "may" — H SIMD/WASM target additions | `restart/MASTER-PLAN.md:609` | "H | SIMD/WASM generation may add target-specific output. | Attribute budget by target." | bounded permission with budget gate following | honoured |
| "may" — declaration-crate is rare last resort | `restart/MASTER-PLAN.md:325`, `:683`; `restart/ARCHITECTURE.md:1023-1027` | "may exist only after Architecture records the exception" | rare-escape language, fenced; not contrivance | honoured |
| "consider" / "perhaps" / "TBD" | `rg "consider \|perhaps\|TBD\|tbd\|to be determined" restart/ARCHITECTURE.md restart/MASTER-PLAN.md` | zero hits | no soft hedging | honoured |
| "future tranche" / "future without receiver" | `rg "future tranche\|future work\|in a future" restart/MASTER-PLAN.md restart/ARCHITECTURE.md` | "future grammar test" appears multiple times (yaml onboarding) — these are named carry rows with receiver tranches G/F; "future grammar process" appears in §22 docs row | every "future" is a named receiver with gate; not unowned | honoured |
| "temporary" / "for now" / "until X" / "thin wrapper" | `rg "temporary\|for now\|until \|thin wrapper" restart/MASTER-PLAN.md restart/ARCHITECTURE.md` | "until admitted as a seed grammar" (yaml LOC) — only hit | one occurrence, narrow scope; not a code workaround | honoured |
| "inherit from BA W4" / legacy preservation | `rg "BA\|BB\|BC\|BD" restart/MASTER-PLAN.md restart/MIGRATION.md` returns inheritance ledger rows | inheritance is mined research signal per `restart/README.md:467`; the master plan §0 ("not a continuation of the old tranche plan ... legacy plans remain inheritance, not governing truth"); MIGRATION §16 ("legacy is mined") | inheritance is contested (named as research signal); not unconfested legacy | honoured |
| Per-grammar match arms in proposed generic crates | `rg -E 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|Bbnf\w*\s*=>\|GoogleSheets\w*\s*=>' restart/ARCHITECTURE.md restart/MASTER-PLAN.md` | zero hits (only in `restart/locks/14-LOCKS.md:60` as the lock's verification command) | no generic-crate per-grammar match arms in plan text | honoured |
| Generic-crate grammar-named modules in proposed trees | walk `restart/ARCHITECTURE.md §4` (per-crate src/ trees) for grammar-named modules | `runtime/src/grammars/<name>/` — but this is the template-emitted output dir, not a generic-crate module. Other generic crate trees (`ir`, `passes`, `vm`, `host`, `cost-model`, `path-core`, `egraph`, `csp-solver`, `parse-that`, `simd-scan`) carry no grammar-named children | honoured |
| Workaround language ("just for", "quick fix", "temporary shim") | `rg "quick fix\|just for now\|temporary shim\|stop-gap" restart/MASTER-PLAN.md restart/ARCHITECTURE.md restart/MIGRATION.md` | zero hits | no workaround diction | honoured |
| Final SOTA escape clause | `restart/MASTER-PLAN.md:506` | "J.W1 | Final SOTA gate and benchmark report. | JSON/CSS/SIMD targets met; misses require amendment before close." | per-amendment of punch #30 (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:349-353`): "Delete the routing escape; replace with 'If a target is missed, J.W1 fails and opens a named architecture amendment before close.'" — present text reads exactly that; the escape is closed | honoured (post-amendment) |
| Greenfield mandate alignment | README §1 ("no quick solutions, no workarounds, no legacy code uncontested, no contrivance, no overfitting"); `restart/README.md:5` | The mandate is the anchor; the master plan §6 ("Tranche A — Workspace Genesis") begins with archive ceremony to clear legacy; MIGRATION §16 mines legacy as inheritance only | mandate clearly stated and operationalized via archive ceremony + inheritance discipline | honoured |
| "thin wrapper" / "adapter" / "shim" usage | `rg "wrapper\|adapter\|shim" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | "wasm-compatible regex call" / "ABI-safe wrapper" (`restart/ARCHITECTURE.md:879`) — narrow boundary wrappers; "host shim" / "css_types.rs and host shims" (`restart/ARCHITECTURE.md:328`, `restart/MIGRATION.md:171`, `:303`) used as the deletion target, not as proposed architecture | shim language describes what is being deleted, not what is being built | honoured |

**Lane 6 verdict: HONOURED (with one minor partial)**.

The greenfield discipline holds across the corpus:
- All `may` occurrences are bounded permissions with paired hard rules or auto-detection rationale.
- Zero "consider" / "perhaps" / "TBD" hedging.
- Every "future" carry has a receiver tranche and gate.
- Zero per-grammar match arms in proposed generic crates.
- Zero generic-crate grammar-named modules.
- The final SOTA escape clause is closed (post-amendment).
- Inheritance is named as research signal, not governing legacy.
- Workaround / shim language only describes what is retiring.

**Sole partial**: `restart/ARCHITECTURE.md:1160` reads "Reported separately until admitted as a seed grammar." The yaml LOC budget condition exists ("until admitted") but the admission gate is not named. Per Reviewer C's discipline ("no future without receiver"): this should name the receiving condition explicitly. The most surgical fix: "Reported separately until yaml's metadata block lands in `[workspace.metadata.bbnf.grammars.yaml]` post-onboarding parity gate at G.W4." That binds the implicit future to a named receiver.

---

## §8 Reviewer-C verdict

**LOCK 14 + GREENFIELD DISCIPLINE HONOURED POST-AMENDMENT — with two narrow surfacing additions required**

The four-target hardening's surfacing of Lock 14 and the greenfield mandate is, in the main, sufficient. The cohort-level verdict AMENDMENT-REQUIRED at `restart/audit/hardening/HARDENING-CONSOLIDATED.md:14` correctly catches the substantive surgeries needed. Reviewer C ratifies the consolidated punch list and finds only two depth-audit additions that the existing surfacing did not fully resolve:

### Reviewer-C additions to the amendment queue

**Addition 1 — Per-X grammar table location authority** (Lane 2). Punch item #13 (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:241-246`) names the demanded 10×9 table but routes it to the SYNTHESIS amendment cohort. The current corpus has fragments (PASS-2 LOC table, PASS-2 runtime emission table, PASS-1 broad-claim table, ARCHITECTURE §11 LOC budget rows). None is the demanded 10×9 architecture-owned table. Reviewer C's surgical recommendation:

- Insert a new §12.1 in `restart/ARCHITECTURE.md` (immediately following §12 Future Grammar Onboarding Test) titled "Per-grammar onboarding ledger".
- Rows: bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, yaml.
- Columns per punch #13: typed root, ValueRef, runtime files, visitor, path schema, fixture manifest, host route, generated LOC, declaration-crate status.
- Cite source data (PASS-2 §6 LOC; metadata blocks for host route and fixture manifest; PASS-1 §2 escape-valve fence for declaration-crate status).

This is amendment-class surgery, not re-draft. Punch #13 already names this work; Reviewer C verifies that the punch must remain open until the architecture-owned table lands.

**Addition 2 — Declaration-crate review form alignment** (Lane 3). Punch item #15 (`restart/audit/hardening/HARDENING-CONSOLIDATED.md:253-257`) demands eight fields. ARCHITECTURE §5.6 (`restart/ARCHITECTURE.md:711-719`) carries five. PASS-1 §2 (`restart/audit/pass-1-substrate/PASS-1.md:62-72`) carries six. Reviewer C's surgical recommendation:

- ARCHITECTURE §5.6 expands to eight rows: Reason; Owner; Failed surface; Declaration location; No-generic-import lint; Deletion path; Reviewer; Receiving gate.
- PASS-1 §2 escape-valve fence reconciles to the same eight rows (via SYNTHESIS amendment per route table at `restart/audit/hardening/HARDENING-CONSOLIDATED.md:484-489`).
- ARCHITECTURE §5 metadata schema reifies all eight fields as TOML keys under `[workspace.metadata.bbnf.grammars.<name>.declaration_crate]`.

This is amendment-class surgery, not re-draft. Punch #15 already names the eight fields; Reviewer C verifies that the punch must remain open until the form alignment lands across both authority documents and the metadata schema.

### Reviewer-C ratifications

- **Lane 1 (yaml two-surface proof)**: HONOURED. `restart/ARCHITECTURE.md:1170-1199`, `restart/MIGRATION.md:691-701`, `restart/MASTER-PLAN.md:412 + :420 + :688`, `restart/audit/pass-1-substrate/PASS-1.md:181-187`, `restart/audit/pass-2-codegen/PASS-2.md:312-318` agree on two onboarding surfaces, one derivative-output dir, named verification commands, and a G.W4 receiving gate. No fixture leak. Hardening's surfacing was sufficient.

- **Lane 4 (OpenFrame retiral)**: HONOURED. Every OpenFrame match across plan files classifies as pathology citation, retiral plan, gate row, or invariant gate. Zero preservation language. The four flagged "negative invariant" rows (`restart/ARCHITECTURE.md:871`, `:926`, `:1075`; `restart/audit/pass-1-substrate/PASS-1.md:46`) all phrase OpenFrame as the failure mode being prevented, paired with the active design (TapeBuilder length checkpoints; bounded rollback). Hardening's surfacing was sufficient.

- **Lane 5 (convergent pivot sequencing)**: HONOURED (post-amendment). Punches #40 and #41 are landed in MASTER-PLAN.md text. C.W2 ShapeFacts fixture in C with explicit B integration gap recording. C.W3 RecognizerFacts feed E-owned BIR snapshots ("not placeholder hints"). C.W5 CostFacts feed E.W1 Backend IR builder. Era V failure averted. Hardening's surfacing was sufficient.

- **Lane 6 (greenfield discipline + no-quick-solutions)**: HONOURED. All `may` occurrences bounded; zero hedging diction; every future carry named with receiver and gate; zero generic-crate per-grammar match arms; zero generic-crate grammar-named modules; final SOTA escape closed; inheritance contested as research signal. One minor partial at `restart/ARCHITECTURE.md:1160` ("until admitted as a seed grammar") missing the admission gate — surgical fix: bind to G.W4 post-onboarding parity gate.

### Cross-target consistency

The greenfield mandate's three core invariants are alive across all authority documents:

| Invariant | README | Architecture | Master plan | Migration | Locks | Status |
|---|---|---|---|---|---|---|
| Two-surface onboarding | `:11-25` | §12 `:1163-1199` | §11 G.W4 `:412 + :420` | §19.6 `:691-701` | Lock 14 `:60` | aligned |
| OpenFrame retiral | `:287, :314, :383, :465` | §0 `:23`, §7.2 `:871`, `:926`, §9.1 `:1075`, §11 `:1138` | §1 `:26`, §7 B inheritance `:235-244`, hard close `:254`, §18 `:574`, §21 `:628` | §5.3 `:284-289`, §19.4 `:669-676` | Lock 1 `:34` | aligned |
| Declaration-crate fence | §1 `:11-25` | §0 `:27`, §5.6 `:711-719`, §8.3 `:1015-1027`, §12 `:1183-1186` | §1 ledger `:58`, §9 close `:325`, §24 carry `:683` | §1 disposition `:14-16` | Lock 14 `:60` | aligned but form is undersized |

### Re-draft threshold check

Per `restart/audit/hardening/HARDENING-CONSOLIDATED.md:579-591`, RE-DRAFT escalates if any of these conditions appears after amendment:

| Condition | Reviewer-C check | Status |
|---|---|---|
| Tape/direct union replaced by direct-only / ParseStream / OpenFrame / columnar / parallel | `rg` confirms tape unioned with direct-to-struct; OpenFrame in pathology citation only | not present |
| Backend IR owned by `codegen` or lowerer walks Grammar IR | post-amendment §7 places BIR in `ir`; codegen consumes via BIR snapshots; punch #2 import-deny gate routed | not present |
| yaml proof requires third surface | two surfaces; runtime is derivative | not present |
| SOTA close without numeric gates or named blocking amendment | `restart/MASTER-PLAN.md:506` requires amendment before close on miss | not present |
| B/C or C/E/H sequencing consumes later-wave artefact | post-amendment §8.5 places consumers same-wave or next-wave | not present |
| Generated-code budgets absent from F/H/J | `restart/MASTER-PLAN.md:599-622` carries trajectory; PASS-2 §6 baseline | not present |
| Carry ledgers without receiver/blocker/gate | §24 carry ledger has all three columns | not present |
| Public API exposes prefixed internal path crates or `path!` macro | post-amendment §3.4 uses `pointer!`/`select!`; crates `path`/`path-core`/`path-ts` unprefixed | not present |
| Standalone `@recover` / grammar-level rewrite-mode / grammar-level Unicode | rewrite-mode out; Unicode below BBNF; `@recover` folded into `@error(recover)` | not present |
| OpenFrame as proposed implementation detail | every OpenFrame match is pathology / retiral / gate; zero preservation | not present |

Zero re-draft conditions present. Reviewer-C verdict stands at AMENDMENT-REQUIRED with two narrow additions.

### Final posture

The hardening's four-target consolidated punch list catches the substantive surgeries; the AMENDMENT-REQUIRED verdict is correct. The greenfield mandate ("no quick solutions, no workarounds, no legacy code uncontested, no contrivance, no overfitting") survives in spirit and in letter across the README, ARCHITECTURE, MIGRATION, MASTER-PLAN, the three PASS syntheses, the 14 locks, and HARDENING-CONSOLIDATED. Lock 14's three-surface contract (a) grammar source, (b) workspace metadata, (c) optional fenced declaration crate — with (c) genuinely reserved as last resort — holds across all authority documents.

The two depth-audit additions (per-X table at architecture authority; declaration-crate review form expansion to eight fields with TOML reification) are amendment-class surgeries, not re-drafts. They join the consolidated queue without forcing a separate hardening pass.

Hereupon Reviewer C halts.

---

*Reviewer C — Lock 14 + greenfield-discipline depth audit*
*Output committed against `restart/audit/hardening/REVIEW-C-LOCK-14-GREENFIELD.md`*
*Cross-tranche scope boundary preserved: this audit touches only the named output file.*
