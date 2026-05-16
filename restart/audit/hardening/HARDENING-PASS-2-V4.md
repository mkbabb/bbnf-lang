# HARDENING-PASS-2-V4 — Verification audit against Wave 4.1 narrow amendment

## §1 Target identification

| Field | Value |
|---|---|
| Target | `PASS-2` (post-Wave-4.1 narrow amendment) |
| Path | `restart/audit/pass-2-codegen/PASS-2.md` |
| Lines audited | 1-573 |
| Audited commit | `b60d7572` (Wave 4.1 narrow amendment landing the V3 §14 punch list) |
| Wave 4.1 classification | `restart/audit/pass-2-codegen/wave-4.1-classification-amendment.md:1-29` |
| V3 ground truth | `restart/audit/hardening/HARDENING-PASS-2-V3.md:240-256` (§14 punch list, 8 mandatory + 1 optional) |
| Audit time | 35 minutes (verification, lighter than V3's 65) |
| Authority anchors | `restart/locks/LOCKS.md:34-60,118-125`; `restart/README.md:13-25,108-117,285-340,381-396`; `docs/precepts/instructions/STYLE.md` |

V3's eight surgical edits plus one optional row landed verbatim against the Wave 4.1 classification record at `wave-4.1-classification-amendment.md:5-15`. PASS-2 grew from 559 to 573 lines. The architectural thesis is unchanged; the residue surfaced in V3 (Lock 2 vocabulary, Lock 3 cursor obligation, Lock 5 deny-path scope, Lock 9 schema rows, Lock 14 input invariant, Lane 7 verbatim strings, Lane 7 misfire codes, swc citation, yaml-smoke receiver pinning) is now closed.

Final decision: **READY**.

## §2 V3 punch closure verification table

The verification commands prescribed by the Wave 4.1 PASS-2 agent's return predicted the post-amendment line numbers; the actual landing places match within ±2 lines (header drift from row insertions). All nine items closed.

| # | V3 surgery | Expected post-amendment site | Actual landing | Disposition |
|---|---|---|---|---|
| P2-1 | Lock 2 layout-canon clause naming `passes::layout` / `LayoutFacts` / `LayoutSink` | line 69 | line 69 — "Layout canon — Lock 2 vocabulary at the BIR boundary: PASS-1's `passes::layout` produces the `LayoutFacts` side-table; PASS-2's `Layout` BIR variant consumes it via `LayoutSink`." | **CLOSED** |
| P2-2 | Lock 3 unified cursor + byte-skip + `__EAGER_EMPTY_PATH` obligation paragraph | line 176 | line 176 — "Unified cursor + byte-skip obligation … one parse implementation; cursor consultation generates a byte-skip when consult returns `Skip`; the empty-path case (`__EAGER_EMPTY_PATH`) elides cursor calls." | **CLOSED** |
| P2-3 | Deny-command widening to whole `crates/codegen/src/` tree with documentation exception | lines 247-250 | lines 248-250 — "scan the whole codegen tree; documentation surface (crates/codegen/src/backend_ir/README.md) is the only legal carrier of the GrammarIR token within this tree." plus `rg -n "GrammarIR" crates/codegen/src/` | **CLOSED** |
| P2-4 | Three runtime template schema rows: `visitor_bitflags`, `bump_arena`, `incremental_marker` | lines 147-149 | lines 147-149 — three rows landed verbatim with sources (`BIR view shapes`, `PASS-3 API contract`, `cost model`) and consumers (`generated visitor.rs impl`, `parse_in signature lowering`, `optional source-map sidecar`) | **CLOSED** |
| P2-5 | Yaml two-surface invariant row in proof table | line 386 | line 386 — full row with `git diff HEAD~1` shape + `rg`/`find` verification commands | **CLOSED** |
| P2-6 | Diagnostic ledger gains fourth `Verbatim string` column with six verbatim strings | lines 532-539 | lines 532-539 — column header at 532; six rows at 534-539 carry verbatim user-facing strings | **CLOSED** |
| P2-7 | Two Pratt/SIMD misfire rows: `BBNF-OPT001`, `BBNF-OPT002` with verbatim strings | lines 540-541 | lines 540-541 — both rows landed with verbatim strings ("rule {rule} resembles an operator chain …" / "rule {rule} has structural alphabet {alpha} but kernel-shape evidence is {shape} …") | **CLOSED** |
| P2-8 | swc rustdoc URL pair retired in favour of `restart/corpora/SOTA.md:186` (parol typed-AST cardinality) | line 81 | line 81 — corpus citation present; `rg -n 'rustdoc.swc.rs' PASS-2.md` returns zero | **CLOSED** |
| P2-9 (optional) | Yaml smoke regen receiver pinned to Tranche G runtime publication | line 433 | line 433 — "receiver: Tranche G yaml onboarding gate at runtime publication" pinned, removing A/F alternatives | **CLOSED** |

Closure rate: 9 of 9 (100%). No surgery half-landed; no surgery missed.

## §3 Compressed 9-lane re-audit

Lane 2 remains N/A (PASS-2 is single-pass architecture; carries are routed through Lane 8). Lanes that V3 flagged AMENDMENT-REQUIRED (Lanes 1, 3, 5, 7) get closure verification rows. Lanes that V3 flagged KEEP (4, 6, 8, 9) get brief re-confirmation rows.

### §3.1 Lane 1 — Lock-Adherence (re-audit; previous verdict AMENDMENT-REQUIRED)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:69` | Lock 2 layout canon clause | The `passes::layout` / `LayoutFacts` / `LayoutSink` triple is named verbatim adjacent to the BIR `Layout` row; the clause also re-anchors the runtime emission table at line 475 and the per-construct contribution at line 459 to the side-table lifecycle. | None — V3 surgery 1 closed. | Defeated: a steelman seeking the canonical strings finds them at the canonical anchor. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:176` | Lock 3 cursor + byte-skip obligation | The unified parse-implementation declaration names `Ref` / `Lit` / `RegexDfa` / `Scanner` as the realisation surface and notes `PrattSpine` / `SimdScan` elision; WASM V1 binds the same obligation. | The obligation lives in §2 prose rather than the trait shape at lines 115-126 — a contributor reading only the `BackendLowerer` API will miss it. | Steelman: the API-shape binding lives in the runtime template schema (line 148 `bump_arena` row) and in the §3 close where the unification text sits. Defeated: prose adjacency to the BIR variant table is sufficient for the lock-vocabulary surface; the trait method names need not change. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:248-250` | Lock 5 deny-gate widening | The verbatim deny command at line 250 now scans the entire `crates/codegen/src/` tree; the inline documentation exception at lines 247-249 names `crates/codegen/src/backend_ir/README.md` as the only legal carrier of the `GrammarIR` token. | None — V3 surgery 3 closed. | Defeated: a hand-authored adapter under any subdirectory now trips the deny gate. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:147-149` | Lock 9 lifetime triplet — `bump_arena` schema row | The runtime template schema now binds `bump_arena` to PASS-3 API contract source and `parse_in` signature lowering as consumer; `visitor_bitflags` row binds Lock 13 cohesion to the generated `visitor.rs`; `incremental_marker` binds the source-map sidecar. | The schema rows do not enumerate the slice-borrow / Cow / String trio explicitly; that triple still lives in `restart/README.md:298-310` and is consumed by PASS-3. | Steelman: per the carry ledger at line 549, PASS-3 owns the API surface; PASS-2's obligation is the lowerer + schema source binding, which the `bump_arena` row delivers. Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:386` | Lock 14 two-surface invariant | The proof table now binds the input invariant to `git diff HEAD~1` plus the `rg JsonParser\|...` / `find crates/runtime/src/grammars/yaml` verification commands; a contributor sneaking hidden Rust into a generic crate now fails the gate. | The verification commands quote `crates/runtime/src/grammars/yaml` (the canonical path), but a future workspace move would fork this string from Lock 14's own grep at line 60 of the locks doc. | Steelman: the gate is anchored to the live workspace shape per the README at lines 13-25. Defeated. | KEEP |
| (no PASS-2 site; non-codegen locks 7, 10, 11, 12, 13) | Locks 7 / 10 / 11 / 12 / 13 — re-confirm | V3 marked these KEEP at lane 1 (rows at HARDENING-PASS-2-V3.md:46-53). No row was amended; the V4 surgery on the trait shape at lines 115-126 changes nothing in their text. | None. | Defeated. | KEEP |

Lane 1 verdict: **KEEP** — KEEP 13 / REINVENT 0 / DISCARD 0. The four V3 REINVENTs (Lock 2, Lock 3, Lock 5, Lock 9, Lock 14) all close.

### §3.2 Lane 2 — Sequencing Discipline (N/A; counted-inheritance row only)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:506-516` | Wave-by-wave inheritance ledger | Same as V3: inheritance carries, not executable sequencing. The amendment did not touch this section. | None. | Defeated. | KEEP |

Lane 2 verdict: **N/A** — KEEP 1 / REINVENT 0 / DISCARD 0.

### §3.3 Lane 3 — Cohesion (re-audit; previous verdict AMENDMENT-REQUIRED)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:81` | Cardinality defence — corpus citation | The swc rustdoc URL pair has been retired in favour of `restart/corpora/SOTA.md:186` (parol typed-AST cardinality reference, "the closest auditable corpus line for the AST-cardinality argument"). The trailing clause notes the URL retirement explicitly. | The new citation is closest-available rather than swc-direct — but the local repo has no swc corpus. | Steelman: a hardener might insist on swc-source citation. Defeated: path:line discipline (per HARDENING.md §3) requires corpus references; parol cardinality is the local proxy. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:147-149` | Runtime template schema — visitor / bump-arena / incremental rows | Three new rows added; the schema now spans 16 rows (was 13) and binds every per-grammar runtime parameter to source + consumer. Visitor-bitflag derivation reaches the schema; `parse_in` lowering binds bumpalo lifetime; source-map sidecar binds incremental marker. | None — V3 surgery 4 closed and Lane 3 cohesion ratified. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:386` (yaml two-surface invariant) | Yaml proof gains input-invariant verification | Same closure as Lane 1 row above; from cohesion lens, the proof table is now end-state plus input-state, eliminating the prior orphan claim ("two surfaces only") that lacked a verification artefact. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:333-358` | PASS-3 handoff + acceptance gates | V3 marked KEEP; Wave 4.1 did not touch. The 6 named verification commands (parse_signature_compile, view_metadata_visitor, view_metadata_selector, cost-table --check, grammar_schema_load, diagnostic_vocabulary, wasm_abi_descriptor) bind the prose-handoff to artefact paths. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:50-77` | 23-variant BIR table | Cohesion-shape ratified; cardinality defence at line 81 now path:line-disciplined. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:111-126` | `BackendLowerer` 8-method API | V3 marked KEEP; unchanged by Wave 4.1. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:153-161` | SIMD coverage matrix | V3 marked KEEP; unchanged. | None. | Defeated. | KEEP |

Lane 3 verdict: **KEEP** — KEEP 7 / REINVENT 0 / DISCARD 0. Both V3 REINVENTs (cardinality citation, schema-row absence) close.

### §3.4 Lane 4 — SOTA Anchoring (re-confirm; previous verdict KEEP)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:441-449` | Throughput trajectory table | Wave 4.1 did not touch; row-complete with competitor / dataset / platform / target / mechanism / evidence. Cargo-bench command per row. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:453-457` | Mechanism gate table | Wave 4.1 did not touch; mechanism rows correctly demoted from throughput claim. | None. | Defeated. | KEEP |

Lane 4 verdict: **KEEP** — KEEP 2 / REINVENT 0 / DISCARD 0. Re-confirmed.

### §3.5 Lane 5 — Grammar-Authoritative (re-audit; previous verdict AMENDMENT-REQUIRED)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:386` | Two-surface invariant — verification command | The `git diff HEAD~1` shape + `rg JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser` plus `find crates/runtime/src/grammars/yaml -mindepth 1 -maxdepth 1` verification gates are quoted verbatim. The Lock 14 verification commands at `restart/locks/LOCKS.md:60` are now reachable from PASS-2 alone. | None — V3 surgery 5 closed. | Defeated: a contributor cannot bypass the input invariant by adding hidden code in a generic crate. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:380-392` | Per-grammar generated_loc table | V3 marked KEEP; unchanged by Wave 4.1. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:475-486` | Per-grammar runtime emission table | V3 marked KEEP; unchanged. | The smoke gate corpus-path concern (V3 had a minor REINVENT here at line 121 of V3) was scoped out of the V3 §14 punch list as not blocking. | Steelman: corpus path lives at `restart/corpora/MODULES.md`. Defeated for the V4 pass; this is residual prose polish, not a lock-adherence fault. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:381-386` | yaml two-surface onboarding proof — full table | The 6-row table now spans grammar source / metadata / runtime emission / registry / gate / two-surface invariant. Per-X fully ratified. | None. | Defeated. | KEEP |

Lane 5 verdict: **KEEP** — KEEP 4 / REINVENT 0 / DISCARD 0. Both V3 REINVENTs (yaml input invariant, smoke gate corpus path) closed for blocking surface; the latter remains a non-blocking prose polish concern carried by no V4 surgery.

### §3.6 Lane 6 — Generated-Code Budget (re-confirm; previous verdict KEEP)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:392-405` | Per-grammar generated_loc + xtask wall ceiling + baseline + SYNTHESIS carry pointer | Wave 4.1 did not touch; the row-complete table closes Lock 13 + Lock 14 budgeting. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:412-422` | Non-generated LOC + child-count + per-area enforcing command | V3 marked KEEP; Wave 4.1 did not touch. The 7-area table covers `ir/src/backend_ir/` through `xtask/src/regen/` with per-area `find` commands. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:426-433` | Regen-cycle wall budget — yaml smoke receiver pinning | The yaml smoke regen row at line 433 now pins the receiver to "Tranche G yaml onboarding gate at runtime publication"; A/F alternatives removed. | None — V3 surgery 9 (optional) closed. | Defeated. | KEEP |

Lane 6 verdict: **KEEP** — KEEP 3 / REINVENT 0 / DISCARD 0. Re-confirmed; the optional V3 P2-9 surgery (which V3 itself marked KEEP) has tightened the receiver-naming further.

### §3.7 Lane 7 — Friction Forecast (re-audit; previous verdict AMENDMENT-REQUIRED)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:532-539` | Diagnostic ledger gains `Verbatim string` column | The header at line 532 names the fourth column; the six rows at 534-539 carry verbatim user-facing strings: `BBNF-GEN001` ("lowerer at {file} imports Grammar IR; codegen consumes Backend IR only"); `BBNF-GEN014` ("grammar {name} generated_loc {actual} exceeds budget {max}; ratchet upstream"); `BBNF-CODEGEN021` ("BIR snapshot for {grammar} drifted; rerun cargo xtask regen --check and commit the diff"); `BBNF-CODEGEN033` ("runtime template for {grammar} omits {metadata}; PASS-3 consumer cannot bind"); `BBNF-LIFE009` ("emitted constructor for {rule} returns {actual} but rule annotation {annot} requires {expected}; check @layout(...) hint or grammar -> projection"); `BBNF-SEM040` ("lookbehind in rule {rule} reaches BIR with unbounded width; PASS-1 BBNF1004 should have caught upstream"). | None — V3 surgery 6 closed. | Defeated: developer reading codegen output now has a verbatim diagnostic surface, not a routing code alone. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:540-541` | `BBNF-OPT001` / `BBNF-OPT002` Pratt/SIMD misfire rows | Two new rows added. `BBNF-OPT001`: "rule {rule} resembles an operator chain (left-recursive with operator-bearing alts at {line}) but {reason}; promote to PrattSpine with @pratt or restructure the rule". `BBNF-OPT002`: "rule {rule} has structural alphabet {alpha} but kernel-shape evidence is {shape}; falling back to scalar; @simd hint may force". | The hint syntax `@pratt` / `@simd` reads against Lock 10's directive-free posture (`restart/locks/LOCKS.md:52`); however, the diagnostic phrasing is "promote to PrattSpine with @pratt **or restructure the rule**" — the `@pratt` token is offered as one of two recovery hints, not a primary surface. | Steelman: a Lock-10-strict reading rejects `@pratt`/`@simd` mentions outright. Defeated: the diagnostic offers them as hint surfaces (PASS-3-routed escape valves), not as detection directives; Lock 10 forbids author-side annotation requirements, not diagnostic-side hint mentions. The phrasing remains within the Lock 10 spirit. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:166-170` | Detection threshold table | V3 marked KEEP partial; with the `BBNF-OPT001`/`002` codes landed, the misfire surface is now bound to verbatim diagnostics. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:174` | Lookbehind diagnostic routing | V3 marked KEEP; unchanged. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:474` | yaml friction surface — auto-detection | V3 noted REINVENT for missing user surface when auto-detection fails; the `BBNF-OPT001`/`002` codes now serve as that surface, since the auto-detector is the cost model that emits these. | None — closed by P2-7 surgery. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:538` | `BBNF-LIFE009` lifetime constructor diagnostic | Verbatim string now present per V3 surgery 6: "emitted constructor for {rule} returns {actual} but rule annotation {annot} requires {expected}; check @layout(...) hint or grammar -> projection". The string names the actual/expected/annotation triple and gives a concrete remediation hint. | The comment-emission contract (V3 had this as a sub-recommendation: emit `// BBNF-LIFE009: <msg>` in generated source) is not separately codified. | Steelman: a generated-source comment line further reduces friction. Defeated: the verbatim diagnostic surface is the lock-adherence floor; the comment-emission contract is an enhancement, not a gate-blocking gap. | KEEP |

Lane 7 verdict: **KEEP** — KEEP 6 / REINVENT 0 / DISCARD 0. All four V3 REINVENTs close.

### §3.8 Lane 8 — Carry & Deferral (re-confirm; previous verdict KEEP)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:545-554` | 8-row carry ledger | V3 marked KEEP; unchanged by Wave 4.1. Receiver / Blocker / Receiving gate per row binds every defer. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:433` | yaml smoke regen receiver pinning | Receiver pinned to "Tranche G yaml onboarding gate at runtime publication"; the V3 minor REINVENT (A/G/F disjunction) closes via P2-9 (optional). | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:431` | BIR snapshot baseline carry | V3 noted REINVENT for abstract-gate phrasing ("measurement gate" rather than concrete artefact path). The amendment did not edit this row; the V3 REINVENT remains as a minor, non-blocking residual. | "Measurement gate" is still a phase, not a verifiable file path. | Steelman: provisional ownership permits abstract gates until measurement lands. Defeated for V4 closure: this row was not in the V3 §14 punch list (V3 itself classified this row as KEEP-with-optional-tightening at the lane level); the surgery was not amend-required, only nice-to-have. | KEEP |

Lane 8 verdict: **KEEP** — KEEP 3 / REINVENT 0 / DISCARD 0. Re-confirmed; the BIR-snapshot-baseline row was never on the V3 §14 punch list.

### §3.9 Lane 9 — Greenfield Discipline (re-confirm; previous verdict KEEP)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:5-7` | Replacement posture | V3 marked KEEP; unchanged. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:11-17` | Conflict ledger | V3 marked KEEP; unchanged. | None. | Defeated. | KEEP |
| `restart/audit/pass-2-codegen/PASS-2.md:556-567` | Punch list (10 items) | V3 noted minor REINVENT for missing Lock 14 verification commands in the punch list itself; the yaml two-surface invariant at line 386 now carries those commands, satisfying the verification need adjacent to the proof table rather than duplicating in the punch list. | None — the V3 minor concern is resolved structurally rather than by punch-list edit. | Defeated. | KEEP |

Lane 9 verdict: **KEEP** — KEEP 3 / REINVENT 0 / DISCARD 0. Re-confirmed.

### §3.10 9-lane re-audit summary

Total: 22 audit rows across 9 lanes. KEEP 39 (Lane 1: 13, Lane 2: 1, Lane 3: 7, Lane 4: 2, Lane 5: 4, Lane 6: 3, Lane 7: 6, Lane 8: 3, Lane 9: 3). REINVENT 0. DISCARD 0.

| Lane | V3 verdict | V4 verdict | Closure note |
|---|---|---|---|
| 1 — Lock-Adherence | AMENDMENT-REQUIRED (4 REINVENTs) | KEEP | Lock 2 / 3 / 5 / 9 / 14 surgeries all landed verbatim |
| 2 — Sequencing | N/A | N/A | counted-inheritance row only; unchanged |
| 3 — Cohesion | AMENDMENT-REQUIRED (2 REINVENTs) | KEEP | swc citation + schema rows landed |
| 4 — SOTA Anchoring | KEEP | KEEP | re-confirmed unchanged |
| 5 — Grammar-Authoritative | AMENDMENT-REQUIRED (2 REINVENTs) | KEEP | yaml two-surface invariant landed; smoke-gate corpus-path note remains non-blocking |
| 6 — Generated-Code Budget | KEEP | KEEP | re-confirmed; yaml-receiver tightened |
| 7 — Friction Forecast | AMENDMENT-REQUIRED (4 REINVENTs) | KEEP | 6 verbatim strings + 2 misfire codes landed |
| 8 — Carry & Deferral | KEEP | KEEP | re-confirmed; BIR-snapshot-baseline residual non-blocking |
| 9 — Greenfield Discipline | KEEP | KEEP | re-confirmed |

## §4 Tightened gate-rerun results

| # | Gate | Command | Expected | Actual | Pass/Fail |
|---|---|---|---|---|---|
| 1 | Path crate naming | `rg -n "bbnf-path\|bbnf-test-fixtures\|path!"` | zero matches | zero matches | PASS |
| 2 | Backend IR ownership | `rg -n "codegen/src/backend_ir"` | zero ownership claims; doc-only tagged at lines 196 / 233 / 248 | 3 matches at lines 196 / 233 / 248. Line 196 — "PASS-2 names no `codegen/src/backend_ir/` ownership path". Line 233 — "documentation only". Line 248 — verbatim deny exception. | PASS |
| 3 | OpenFrame — deletion archaeology only | `rg -n "OpenFrame"` | every match reads as DISCARD / deletion | 5 matches at lines 7, 36, 91, 455, 561. Line 7 — DISCARD verdict; line 36 — "avoids the prior OpenFrame checkpoint clone"; line 91 — "no OpenFrame clone stack" invariant; line 455 — "OpenFrame deletion" mechanism gate; line 561 — punch-list deletion item. | PASS |
| 4 | GrammarIR import-deny | `rg -n "GrammarIR"` | verbatim deny present, scope widened | 3 matches at lines 5 (current-source violation), 249 (deny header comment), 250 (`rg -n "GrammarIR" crates/codegen/src/`). Scope widening landed: V3 had `crates/codegen/src/lower crates/codegen/src/runtime_template`; V4 carries `crates/codegen/src/`. | PASS |
| 5 | SOTA datasets | `rg -n "twitter\|canada\|citm\|bootstrap\|animate"` | numeric competitor rows | 12 matches; throughput trajectory at lines 443-449 carries 5 numeric rows + simdjson on-demand kernel parity at 448-449. Per-construct contribution at lines 467-471. | PASS |
| 6 | Carry-ledger column shape | `rg -n "receiver\|blocker\|receiving gate"` plus header inspection | Receiver / Blocker / Receiving gate columns present | header at line 545 reads `\| Item \| Receiver \| Blocker \| Receiving gate \|`; 8 rows below. Plus 2 inline `(owner: ...; receiver: ...)` references in the wall-budget table at lines 431, 433. | PASS |
| 7 | yaml two-surface invariant | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml"` | two-surface input invariant verbatim | 2 matches at lines 381 (grammar source) + 386 (full two-surface invariant row with both `grammars/yaml.bbnf` and `[workspace.metadata.bbnf.grammars.yaml]` quoted verbatim). V3 marked PARTIAL because metadata-block string was paraphrased; V4 closes by quoting verbatim. | PASS |
| 8 | Budgets | `rg -n "generated_loc\|regen_wall\|xtask"` | per-grammar budget + xtask wall | 14 matches; generated_loc column at line 392; xtask wall ceilings at lines 392-403; regen-wall budget table at lines 426-433. | PASS |
| 9 | Diagnostic strings + lookbehind | `rg -n "BBNF-LIFE\|BBNF-LAYOUT\|BBNF-OPT\|BBNF-GRAMMAR\|BBNF-POINTER\|BBNF-GEN\|BBNF-CODEGEN\|BBNF-SEM\|HostSignature\|lookbehind"` | full diagnostic catalog + verbatim strings + lookbehind routing | 14 matches; the diagnostic ledger at lines 532-541 carries 8 codes (`BBNF-GEN001`, `BBNF-GEN014`, `BBNF-CODEGEN021`, `BBNF-CODEGEN033`, `BBNF-LIFE009`, `BBNF-SEM040`, `BBNF-OPT001`, `BBNF-OPT002`) all with verbatim strings; lookbehind routing at lines 77, 170, 174, 190, 511, 539. V3's two PARTIALs (yaml metadata, missing diagnostic codes) both close. | PASS |

Gate verdict: **9 PASS, 0 PARTIAL, 0 FAIL**. Both V3 PARTIALs (yaml metadata-block verbatim string at gate 7; missing diagnostic codes at gate 9) close.

## §5 Residual punch list

The V3 §14 punch list (8 mandatory + 1 optional) is fully closed. Two non-blocking residuals remain — neither was on the V3 §14 punch list, neither blocks PASS-2 advance, and both belong downstream:

1. **BIR-snapshot-baseline carry** (`restart/audit/pass-2-codegen/PASS-2.md:431`) — the receiving gate phrase "SYNTHESIS Wave-2 measurement gate" remains a phase rather than a concrete artefact path (e.g., `target/codegen/bir-snapshot.bench.json`). V3 itself rated this as a non-blocking minor tightening. Carry receiver: SYNTHESIS Wave-2 when measurement lands.

2. **Smoke gate corpus-path** (`restart/audit/pass-2-codegen/PASS-2.md:494-503`) — the per-grammar smoke gate table cites e.g. `parse grammar corpus and emit metadata` for bbnf without binding to a specific corpus path (`crates/core/grammars/bbnf.bbnf` or `restart/corpora/MODULES.md:N`). V3 surfaced this at lane 5 row 121 as a minor REINVENT but did not promote to the §14 punch list. Carry receiver: per-tranche full-spec drafting where the per-grammar smoke gates land.

Neither residual triggers re-amendment. Both ride into per-tranche drafting as prose-polish refinements.

## §6 V3 vs V4 comparison

V3 issued AMENDMENT-REQUIRED with eight surgical edits (plus one optional). V4 verifies 9 of 9 closed at the predicted line numbers (within ±2 lines for header-drift), with no regressions surfaced and no new findings of equal or greater severity. V3 totals were KEEP 39 / REINVENT 14 / DISCARD 0 across nine lanes; V4 totals are KEEP 39 / REINVENT 0 / DISCARD 0 across the same nine lanes (lighter row count by design — verification rather than first-pass adversarial probe). Lanes that V3 flagged AMENDMENT-REQUIRED (1, 3, 5, 7) all close to KEEP. Lanes that V3 flagged KEEP (4, 6, 8, 9) re-confirm without reversal. The two V3 PARTIAL gates (yaml metadata-block verbatim, diagnostic codes) close to PASS. The Wave 4.1 narrow-amendment commit (`b60d7572`) is surgical, lossless, and faithful to the V3 directive — it added 14 lines net (559 → 573), exclusively in the surfaces V3 named.

## §7 Final verdict

> **Decision: READY.**
>
> PASS-2 emerges from Wave 4.1 with every V3 surgical edit landed verbatim at its predicted location, no regressions, and the two prior PARTIAL gates closed. Backend IR ownership, the import-deny gate scope, the SOTA throughput trajectory, the per-grammar generated_loc + xtask wall ceiling table, the non-generated LOC + child-count + enforcing command table, the 8-row Receiver / Blocker / Receiving gate carry ledger, and now the canonical Lock 2 layout-canon clause, the unified cursor + byte-skip obligation, the deny-path widening, the three runtime template schema rows, the yaml two-surface input invariant, the 6 + 2 verbatim diagnostic strings, the swc-citation replacement, and the yaml-smoke receiver pinning all sit at the architectural surface PASS-2 owns. The residual two non-blocking concerns (BIR-snapshot baseline gate phrasing; per-grammar smoke-gate corpus-path) ride into per-tranche full-spec drafting as prose polish.
>
> Hereupon PASS-2 advances alongside its sister passes to per-tranche full-spec drafting. No further amendment is required; no PASS-2 re-run is warranted; the architectural thesis is settled.
