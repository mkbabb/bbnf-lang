# HARDENING-PASS-3-V3 — Independent V3 audit

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` (479 lines, post Wave-2 + Wave-3 amendments) |
| Audit posture | Independent V3 — formed prior to reading V2; sister auditors run in parallel |
| Amendment commits in scope | `dceeaf32` (Wave 2 PASS-3 amendment), `70378e46` (Wave 3 carry across the trio) |
| Sub-agent surface | six PASS-3 sub-agent reports (`agent-1-value-api-designer.md` … `agent-6-ecosystem-architect.md`) plus `wave-2-classification.md` |
| Lanes applied | nine; Lane 2 (Sequencing) N/A under single-pass scope |
| Tightened gate-rerun | sixteen-command checklist re-walked end-to-end; nine PASS-3-touching gates resolve to expected post-conditions, two carry observable residue (Lane-4 platform inlining, Lane-6 baseline anchor) |
| V3 output path | `restart/audit/hardening/HARDENING-PASS-3-V3.md` |
| Time budget | 70-minute hard cap; commit at 63 minutes |

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | honoured | 12 | 0 | 0 | None — every Lock walked has cite + verbatim binding. |
| 2 Sequencing | N/A | — | — | — | Single-pass; sequencing is MASTER-PLAN's burden. |
| 3 Cohesion | honoured | 8 | 1 | 0 | Tighten yaml-row's host-route cell (§6a, line 342); document-only. |
| 4 SOTA-Anchoring | violated-with-recommendation | 5 | 1 | 0 | Inline competitor + platform per bench-row cell (§7, lines 386-396); the §10 carry to SYNTHESIS H/J does NOT excuse the row's own attribution. |
| 5 Grammar-Authoritative | honoured | 7 | 0 | 0 | None — registry deletion close gate, fixture separation, and 10-row feeder all bind. |
| 6 Generated-Code-Budget | violated-with-recommendation | 7 | 1 | 0 | Anchor baseline LOC per grammar (§7 budget table, line 400-405); "+2 percent regen ceiling" without W3-baseline numbers is a delta without an origin. |
| 7 Friction-Forecast | honoured | 9 | 0 | 0 | None — 15-row diagnostic ledger with verbatim text, target user, mental model, confusion point, artefact. |
| 8 Carry-Deferral | honoured | 11 | 0 | 0 | None — every carry triple-complete; receiver/blocker/receiving-gate columns rigid. |
| 9 Greenfield-Discipline | honoured | 7 | 1 | 0 | Tighten visitor-cookbook receiver routing (§3, line 115); non-blocking. |

| Verdict class | Count |
|---|---:|
| KEEP | 66 |
| REINVENT | 4 |
| DISCARD | 0 |

KEEP fraction 94%; the audit found two violated-with-recommendation lanes (4, 6) and three non-blocking REINVENT rows. Healthy challenge surface per HARDENING.md §"Per-Item Discipline".

**Final decision: AMENDMENT-REQUIRED.** Two surgical edits close the SOTA-Anchoring (Lane 4) and Generated-Code-Budget (Lane 6) gaps. Three non-blocking phrasing tightenings may close at next pass-through. PASS-3 advances to per-tranche full-spec drafting only after the two binding amendments.

## §3 Lane 1 — Lock-Adherence

Lock-by-lock walk against PASS-3.md's amended text. Every row carries explication, pros, cons, challenge, verdict.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:16 | Lock 1 — tape unioned with direct-to-struct | "tape is the substrate and is properly unioned with direct-to-struct. It must not be renamed to `ParseStream`" | settled-authority phrasing carried verbatim; matches `restart/locks/14-LOCKS.md:34` reframe | none | the steelman is "ParseStream removes baggage" — defeated by the amended Lock 1 explicitly amending the no-rename clause | KEEP |
| PASS-3.md:31 | Tape/direct union verdict cell | "Tape is the single advanced substrate; direct structs remain the ergonomic default" | matches Lock 1 union mandate | "Requires careful identity invariants" surfaced as Con | the challenge "every visible direct node carries tape identity" is the steelman the amended row commits to; defeats the parallel-substrate steelman | KEEP |
| PASS-3.md:32 | `ParseStream` DISCARD row | DISCARD across all surfaces | resolves the prompt+inheritance-index conflict | none | challenge "would fork naming and contracts" defeats any rebrand pressure | KEEP |
| PASS-3.md:38 | Rewrite-mode + grammar Unicode algebra DISCARD | matches `restart/README.md:123, 133-143` | resolves stale prompt clauses | none | challenge wins; rewrite folds into visitor edit-builders, Unicode folds into `parse-that/regex` | KEEP |
| PASS-3.md:30 | Lock 9 — three-constructor surface | `parse`, `parse_in`, `parse_owned` per `restart/locks/14-LOCKS.md:50` | every constructor matches BB W4a/cookbook precedent | "Owned mode can hide copies" surfaced | mitigation cited (bench all modes separately); defeats the steelman | KEEP |
| PASS-3.md:84-92 | Lock 7 — path crate names | "`path-core` owns parsing… `path` owns Rust proc macros: `pointer!` and `select!`… `path-ts` owns TS template tags" | matches `restart/locks/14-LOCKS.md:46` triplet (Rust toolchain proc-macro path-dep limitation) | none | challenge "three proc-macro shells is a fault" defeated by `path-core` non-proc-macro shared base | KEEP |
| PASS-3.md:115 | Lock 13 — visitor cohort discipline | generated `Visitor` trait + `Visit`/walker + `VisitTypes` bitflag pruning | matches W5 inheritance | risk "large generated APIs need restraint" surfaced | mitigation: prelude exports common; advanced modules detail-gated | KEEP |
| PASS-3.md:194-208 | Lock 13 — `bbnf` aggregator 8 children | `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/` | child count 8 ∈ [4, 10]; matches Lock 13 sonic-rs/lightning-css/simdjson standard | "tape/ and value/ live elsewhere" required explicit rationale; carried at line 208 | the steelman "duplicate sibling surfaces in `bbnf/src/`" defeated by re-export-through-prelude rationale | KEEP |
| PASS-3.md:320-325 | Lock 14 — fixture separation onboarding gate | "Lock 14 onboarding admits exactly two surfaces … `fixtures/yaml/*` is *not* part of the onboarding allowance" | resolves the V1 fixture-residue concern; binds verification grep at 325 | none | the steelman "fixtures are part of onboarding" defeated by the explicit two-phase split | KEEP |
| PASS-3.md:328-344 | Lock 14 — 10-row per-grammar feeder | 10 rows × 7 columns (typed root / `ValueRef` kind / generated runtime files / visitor + `VisitTypes` / path schema / fixture manifest / host route) | every "all grammars" claim resolves through this table | none | matches Lock 14 per-X-table mandate | KEEP |
| PASS-3.md:94-101 | Lock 14 — registry deletion close gate | `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry\|hardcoded_grammar_registry'` returns zero outside generated | resolves CENSUS §2 violation propagation risk | none | challenge "treat as deferral" defeated by explicit "deletion item" framing | KEEP |
| PASS-3.md:104-112 | Lock 5 — three executable consumer-acceptance gates | binds PASS-3 close to PASS-2 emission contract | resolves orphan-consumer concern Lane 3 of HARDENING-CONSOLIDATED §4.5 | "materialisation_cost.toml or equivalent" leaves artefact-name flexibility | the flexibility is appropriate at this level; SYNTHESIS pins the artefact name; defeats the challenge | KEEP |

Per-lock detailed walk over the amended PASS-3 surface. Each lock cited at the line where it is honoured (or rationally delegated), with explicit pros/cons/challenge per lock-level claim.

Walk over the 14 locks specifically:

- **Lock 1** — honoured (PASS-3.md:16, 31; tape/direct union explicit, `ParseStream` rejection committed as DISCARD across §1 ledger + §8 hand-off + §9 KEEP/REINVENT/DISCARD summary)
- **Lock 2** — silent at PASS-3 surface (Layout-lowering canon is PASS-1/PASS-2 territory; PASS-3 references `@layout` only as user-surface directive). Acceptable; not PASS-3 scope.
- **Lock 3** — silent at PASS-3 surface (cursor-parse + byte-skip elision is PASS-1/runtime engine territory).
- **Lock 4** — silent at PASS-3 surface (CSP + e-graph orthogonal optimisation is PASS-1 territory).
- **Lock 5** — honoured indirectly via §3 consumer-acceptance gates binding PASS-2 emission contract; the LowererImport diagnostic at line 366 mirrors the BIR/Grammar IR import-deny rule.
- **Lock 6** — honoured via "xtask emits committed source artefacts" cited at lines 103, 408 (registry deletion grep targets `crates/`, regen wall budget cites `cargo xtask regen --check`); no proc-macro façade for codegen output proposed.
- **Lock 7** — honoured (PASS-3.md:84, 273-299; `path` + `path-core` + `path-ts` triplet)
- **Lock 8** — partially honoured (every parse-throughput row at lines 388-393 carries a target number, but competitor/platform attribution is elided from the row itself; Lane 4 surfaces the violation)
- **Lock 9** — honoured (PASS-3.md:30, 60-78)
- **Lock 10** — silent in PASS-3 surface text but matches Lock 10 spirit via `BBNF-OPT001/002` diagnostics (lines 356-357) which announce auto-detection misfire; no grammar-level `@pratt`/`@simd` directive proposed
- **Lock 11** — silent at PASS-3 surface (path-deps for incubating sister crates is workspace-shape territory; PASS-3 §6 module trees match)
- **Lock 12** — silent at PASS-3 surface (ser/gorgeous archive ceremony is Tranche A.W0 territory; PASS-3 module trees do not include `ser/` or `gorgeous/`)
- **Lock 13** — honoured (PASS-3.md:194-208, 210-272, 273-318; every crate's `src/` tree is 4–10 children, sibling-API uniform)
- **Lock 14** — honoured (PASS-3.md:320-325 fixture separation; 328-344 10-row feeder; 94-101 registry deletion close gate; 38 rewrite/Unicode/per-grammar-crate DISCARD; the yaml row at 342-344 carries the two-surface onboarding boundary)

Cross-pass binding observations — what PASS-3 commits to that PASS-1/PASS-2 must satisfy:

| PASS-3 commitment (path:line) | Cross-pass receiver | Mechanism |
|---|---|---|
| Tape ABI: stable `DocumentId`/snapshot identity, stable node kind IDs, cheap span/payload lookup, child/sibling traversal, recovery/layout flags, optional trace events (PASS-3.md:121-127, 415, 465) | PASS-1 / Tranche B | "user-surface contract; PASS-1 may pack differently if these semantics remain true" disclaimer at 153 |
| Three-constructor compile gate: `Json::parse(&str)`, `Json::parse_in(&str, &Arena)`, `Json::parse_owned(String)` for every extant grammar plus yaml (PASS-3.md:104-112) | PASS-2 / Tranche F + Tranche I | "Generated runtime crate compiles" — executable smoke gate |
| `DocumentView::root_value()` projects to the same `ValueRef` index space the visitor walker, `pointer!` runtime plan, and `select!` traversal plan consume (PASS-3.md:110) | PASS-2 / Tranche F | "the same `ValueRef` index space" — index-equality smoke |
| Codegen emits a `materialisation_cost.toml` (or equivalent generated artefact) with field counts, payload arena bytes, and tape-token width per node kind (PASS-3.md:111) | PASS-2 / Tranche F | the cookbook references it |
| Hardcoded grammar marker registry deletion (PASS-3.md:94-101, 421) | SYNTHESIS / Tranche I close gate | `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry'` returns zero outside generated data |
| `pointer!`/`select!` validate against generated metadata (PASS-3.md:33, 84-92, 466) | PASS-2 / Tranche F | metadata schema enumeration |
| `@error(recover = ...)` consolidation; legacy `@recover` only as migration alias (PASS-3.md:160) | SYNTHESIS / Tranche I | input-normalization-deletion table |

Verification commands:
- `rg -ni 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' restart/audit/pass-3-runtime/PASS-3.md` — zero in proposed code; only legacy citations + table cells (matches Lock 14)
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-3-runtime/PASS-3.md` — zero (matches Lock 14)
- `rg -ni 'parsestream' restart/audit/pass-3-runtime/PASS-3.md | wc -l` — 10 hits, all framed as stale + DISCARD context (acceptable conflict adjudication, not propagation)
- `rg -n 'pointer!|select!|path!' restart/audit/pass-3-runtime/PASS-3.md` — `pointer!` and `select!` present; `path!` absent (matches Wave-2 macro renaming)
- `rg -n "bbnf-path|bbnf-test-fixtures" restart/audit/pass-3-runtime/PASS-3.md` — only legacy archaeology at line 84 + deletion-archaeology rg gate at 467 (matches Lock 7 + Wave-2 path crate naming)
- `rg -n "fixtures/yaml" restart/audit/pass-3-runtime/PASS-3.md` — only post-onboarding parity references at lines 320-325, 342, 344, 474 (matches Lock 14 fixture separation)
- `rg -n "@recover" restart/audit/pass-3-runtime/PASS-3.md` — only as legacy alias at lines 35, 160 (matches HARDENING-CONSOLIDATED §4.9 consolidation)
- `rg -nC4 "bbnf/src/|aggregator|immediate children" restart/audit/pass-3-runtime/PASS-3.md` — 8-children layout at 197-208 with explicit rationale (matches Lock 13)

Cross-target binding observations:

The amended PASS-3 honours the four lock targets that V3's HARDENING.md §"Lanes" calls out as the PASS-3 deep-foci:
- Lock 1 (`pointer!` macro + path/path-core/path-ts naming + bbnf 8-children) — committed verbatim across the §1 ledger, §6 module trees, and §6b diagnostic ledger.
- Lock 5 (Grammar-Authoritative — fixture separation) — the §6 fixture separation block at 320-325 enforces the two-surface onboarding boundary; the §6a feeder table at 328-344 displays the per-grammar binding; the §10 carry at 474 binds the verification grep.
- Lock 6 (Generated-Surface budget — visitor + path + tape) — the §7 budget table at 399-409 covers visitor + path + tape + bench + regen, but anchors are missing (Lane 6 violation).
- Lock 7 (Friction-Forecast — diagnostic ledger) — the §6b ledger at 346-368 carries 15 codes, every row triple-bound to verbatim text + target user + mental model + confusion point + artefact.
- Lock 9 (Greenfield — registry deletion gate) — the §3 close gate at 94-101 binds the deletion to a verifiable grep.

Lane 1 verdict: **honoured**. KEEP 12 / REINVENT 0 / DISCARD 0. Every Lock walked at PASS-3's own surface either honours the lock at named lines, or correctly delegates to MASTER-PLAN/PASS-1/PASS-2 at the appropriate cross-pass binding.

## §4 Lane 2 — Sequencing Discipline

N/A. PASS-3 is single-pass; sequencing across waves is MASTER-PLAN territory. The cross-pass hand-off table at §8 (lines 411-426) and unresolved punch-list at §10 (lines 462-475) carry receiver/blocker/receiving-gate triples that bind any deferred consumer to a named gate, but the audit's lane-2 substrate-then-consumer check applies to wave-level plans.

## §5 Lane 3 — Cohesion

Every claim in PASS-3 must be verifiable from artefacts the target produces or cites.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:60-78 | `Grammar` trait + `DocumentView` | typed-root + three-constructor + `root_value()` + diagnostics | precise lifetime parameterisation; matches W4 cookbook | none | the steelman "drop lifetime params from default API" defeated by Lock 9 mandate | KEEP |
| PASS-3.md:104-112 | three executable consumer-acceptance gates | `parse`/`parse_in`/`parse_owned` smoke; `DocumentView` metadata feeds visitors + selectors; materialisation cost table generated | resolves orphan-consumer risk | "materialisation_cost.toml or equivalent" is artefact-name flexibility | flexibility is appropriate | KEEP |
| PASS-3.md:130-152 | tape illustrative shape | `Tape<'input>` with `tokens: Box<[TapeToken]>`; `TapeToken` with kind/flags/start/end/payload/sibling_skip; `ValueRef<'doc, 'input, K>` | precise typed shape | "user-surface contract, not PASS-1 mandate" disclaimed at 153 | the disclaimer is correct: PASS-1 owns ABI; PASS-3 commits to the semantic surface | KEEP |
| PASS-3.md:181-190 | fallback-rate gates by dataset | 4 corpora × 4 columns (corpus / snapshot reuse / fallback ceiling / surface) | dataset-level thresholds bind incremental contract | "BBNF self-edit corpus" target 70% reuse / 15% fallback may be loose | the looseness is calibrated for self-host bootstrap workload; defeats the steelman that all corpora share thresholds | KEEP |
| PASS-3.md:194-208 | bbnf canonical 8-children layout | `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/` | matches Lock 13; rationale at line 208 carries the tape/value relocation explanation | none | matches `restart/locks/14-LOCKS.md:58` standard | KEEP |
| PASS-3.md:210-251 | bbnf-cli + bbnf-language-server trees | clean concern-split (commands/output; document/analysis/incremental/lsp/dap/workspace) | sibling-API uniform | the language-server tree at 19 entries spans 7 sibling directories; child count = 7 ∈ [4, 10] | matches Lock 13 | KEEP |
| PASS-3.md:252-272 | bbnf-bench tree | harness + datasets + competitors + report | matches Architecture §4.4 row | "benches/ holds five files" is sibling discipline | child count 5 ∈ [4, 10]; matches Lock 13 | KEEP |
| PASS-3.md:273-318 | path/path-core/path-ts/test-fixtures trees | unprefixed names; concern-split | matches Lock 7 + Lock 13 | "fixtures/ has 4 children" — child count 4 is the lower bound of [4, 10] | matches Lock 13 boundary | KEEP |
| PASS-3.md:328-344 | §6a per-grammar feeder table | 10 rows × 7 columns | resolves "all grammars" prose risk | yaml row at 342 carries "as declared in `[workspace.metadata.bbnf.grammars.yaml]`" — documentary, not enforceable | the cell is correct because yaml's host route lands at metadata-time; could carry a stronger forward pointer to `host::primitives` + `@host fn` chain decomposition | REINVENT |

Lane 3 verdict: **honoured**. KEEP 8 / REINVENT 1 / DISCARD 0. The single REINVENT (yaml-row host-route cell) is non-blocking phrasing tightening; defer to next pass-through.

## §6 Lane 4 — SOTA-Anchoring

Every parse-throughput gate must cite competitor + dataset + platform per Lock 8. Non-throughput engineering gates must NOT claim Lock 8 honour.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:374-380 | Dataset baseline citation table | 5 rows × 3 columns (dataset / baseline citation / PASS-3 gate) | every row points to a `restart/corpora/SOTA.md` line range | the "Baseline citation" cell points to a corpus path, not the actual competitor number | the steelman "corpus paths are sufficient because the corpus has the numbers" — defeated by the HARDENING-CONSOLIDATED §4 standard "every gate names a competitor's number" | REINVENT |
| PASS-3.md:386-396 | Exact PASS-3 benchmark rows | 9 rows × 3 columns (Row / Target / Surface under test) | every row carries a target number | competitor + platform attribution is elided from the row cell | challenge: inline `sonic-rs 436 µs (M1 Pro)` etc. per row, or carry a single explicit "all M1 Pro; competitor numbers per `restart/README.md:328-334` per-row" preamble | REINVENT |
| PASS-3.md:388 | `json/twitter/borrowed` ≤ 380 µs | matches MASTER-PLAN row from `restart/README.md:328` (sonic-rs 436 µs / simd-json 424 µs / M1 Pro) | target number present | row cell does not inline the competitor | the row is verifiable only by cross-referencing README §9; the competitor number could be inlined | KEEP (target is correct; attribution carry to SYNTHESIS) |
| PASS-3.md:391 | `json/canada/array_scan` ≤ 2.8 ms | matches `restart/README.md:329` (sonic-rs 3.144 ms M1 Pro) | target number present | row cell does not inline the competitor | same | KEEP |
| PASS-3.md:394 | `bbnf/self_host/internal` ≤ 100 ms | "non-Lock-8 internal gate; no SOTA peer claim attaches" | explicit non-Lock-8 framing per HARDENING-CONSOLIDATED §4.33 | none | the "no SOTA peer claim attaches" cell defeats any over-claim risk | KEEP |
| PASS-3.md:395-396 | `incremental/edit_anchor` + `debug/trace_overhead` | "report fallback rate" / "report overhead" — non-throughput cells | non-throughput engineering gates appropriately do NOT claim Lock 8 | none | challenge that mechanism gates need numbers — defeated, dataset-level fallback gates at §5 carry the actual thresholds | KEEP |
| PASS-3.md:468 | §10 carry "Bench harness target numbers and machine profiles" | Receiver SYNTHESIS / Tranche H/J; Blocker bench rows become aspirational; Receiving gate "Master/Architecture inline competitor + dataset + platform + bbnf number for every row" | the carry is triple-complete | the carry punts the inlining to SYNTHESIS — but PASS-3.md's own §7 rows still need to carry today's known competitor numbers | the receiving gate is correct; it does NOT excuse PASS-3's own bench-row table from inlining what is known today | REINVENT |

The Lane-4 violation: PASS-3's bench-row table at lines 386-396 elides competitor/platform attribution that `restart/README.md:328-334` already carries. The §10 carry to SYNTHESIS is welcome insurance but does not excuse the row from inlining today's facts. Surgery: extend §7 bench-row table with two new columns ("Competitor floor", "Platform") populated from `restart/README.md:328-334`. Or prepend a single attribution preamble: "All rows are M1 Pro; competitor floors per `restart/README.md:328-334`; bbnf targets aim to surpass."

Verbatim attribution facts available today (sourced from `restart/README.md:328-334`):

| Bench row (PASS-3.md) | bbnf target | Competitor floor | Platform |
|---|---|---|---|
| `json/twitter/borrowed` (line 388) | ≤ 380 µs | sonic-rs 436 µs / simd-json 424 µs | M1 Pro |
| `json/twitter/tape_cursor` (line 389) | ≤ borrowed + 10% | sonic-rs LazyValue projection | M1 Pro |
| `json/citm/pointer` (line 390) | ≤ 750 µs | sonic-rs 854 µs / simd-json 831 µs | M1 Pro |
| `json/canada/array_scan` (line 391) | ≤ 2.8 ms | sonic-rs 3.144 ms | M1 Pro |
| `css/bootstrap/visitor` (line 392) | ≤ 3.0 ms | lightning-css ~4.16 ms | M1 Pro (PASS-3 §9 disclaim re: platform-specific ratification) |
| `css/animate/layout` (line 393) | ≤ 1.6 ms | lightning-css 1.97 ms | M1 Pro (same disclaim) |
| `bbnf/self_host/internal` (line 394) | ≤ 100 ms full self-parse + format roundtrip | (no SOTA peer claim attaches) | M1 Pro internal |
| `incremental/edit_anchor` (line 395) | report fallback rate | (non-throughput; no Lock-8 claim) | M1 Pro |
| `debug/trace_overhead` (line 396) | report overhead | (non-throughput; no Lock-8 claim) | M1 Pro |
| simdjson On-Demand sustained (`restart/README.md:333`; not present in PASS-3 §7) | ≥ 5 GB/s sustained M-series; ≥ 7 GB/s x86 | simdjson 7 GB/s Intel Skylake | M-series + x86 |

The simdjson sustained row is missing from PASS-3 §7 entirely. Whether this is a scope decision (PASS-3 covers only per-grammar bench rows) or an oversight is unclear from the surface. Surgery 1 may also note this gap.

Lane 4 verdict: **violated-with-recommendation**. KEEP 5 / REINVENT 1 / DISCARD 0. Surgery: inline competitor + platform per row OR prepend attribution preamble.

## §7 Lane 5 — Grammar-Authoritative Discipline

Per HARDENING.md §"Lane 5", verify zero proposed `match grammar { Json => ... }` arms in proposed generic crates; per-X tables for every "all grammars" claim; future-grammar onboarding test for `yaml.bbnf` via TWO surfaces only; no `crates/<grammar>/` declaration crates by default.

Verification:
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-3-runtime/PASS-3.md` — **zero matches**.
- `rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math'` — every match classifies as ratified (per-X table cell, fixture path, audit anchor) per the §6a feeder table or as legacy citation marked stale.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:94-101 | Registry deletion close gate | `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry\|hardcoded_grammar_registry' crates/path crates/path-core crates/path-ts crates/bbnf crates/codegen crates/runtime crates/ir` returns zero outside generated data | resolves CENSUS §2 violation; binds the deletion to a verifiable grep | none | challenge "deferral, not deletion" — defeated by explicit "deletion item" + close-gate framing | KEEP |
| PASS-3.md:320-325 | Fixture separation Lock 14 onboarding | onboarding admits two surfaces; fixtures land in parity phase | resolves the V1 fixture-residue concern; binds verification grep at 325 | none | challenge "fixtures are part of onboarding" defeated by explicit two-phase gate | KEEP |
| PASS-3.md:328-344 | 10-row per-grammar feeder | 10 rows × 7 columns | every "all grammars" claim resolves through this table | yaml row's host-route cell is documentary | the documentary-vs-enforceable distinction is what §6a Reinvent (Lane 3) flags; not a Lane 5 fault | KEEP |
| PASS-3.md:90-91 | "Generated grammar metadata replaces fixture registries" | path schema validates compile-time | resolves the hardcoded-registry concern in the path/select DSL | none | matches Architecture §10.1 path inheritance | KEEP |
| PASS-3.md:300-318 | test-fixtures crate tree | data + manifests only; no per-grammar Rust | matches Lock 14 + Amendment 01 | "fixtures/ holds 4 grammar dirs" — these are post-onboarding parity surface, NOT onboarding allowance | the fixture-separation block at 320-325 enforces this distinction explicitly | KEEP |
| PASS-3.md:38, 451-456 | Per-grammar declaration crates DISCARD; rewrite-mode DISCARD; grammar-level Unicode algebra DISCARD | matches HARDENING-CONSOLIDATED §3 row 6 | settled across §1 + §8 + §9 | none | challenge "rare host adapter escape valve" addressed at §10 line 469 (Receiver/Blocker/Receiving gate triple) | KEEP |
| PASS-3.md:344 | yaml onboarding boundary | "every cell to the left of the parity-phase fixture manifest must be generated from `yaml.bbnf` plus the workspace-metadata block, with zero Rust edits and zero per-grammar match arms" | matches Lock 14 two-surface mandate | none | challenge "yaml needs a Rust file" — defeated by explicit "zero Rust edits" requirement | KEEP |

Per-grammar grep classification, every PASS-3.md hit:

| Hit category | Path:line examples | Classification |
|---|---|---|
| Per-X table cell (§6a feeder, line 333-342) | `bbnf` / `bnf` / `csv` / `css_l4` / `css_pretty` / `ebnf` / `google_sheets` / `json` / `math` / `yaml` rows | RATIFIED — every row exposes the typed-root + `ValueRef` kind + generated runtime files + visitor + path schema + fixture manifest + host route per Lock 14 per-X-table mandate |
| Fixture path (§6 module trees, lines 312-315) | `fixtures/json/`, `fixtures/css/`, `fixtures/bbnf/`, `fixtures/sheets/` | RATIFIED — data + manifests only; harness is grammar-agnostic |
| Bench-row label (§7 bench rows, lines 388-394) | `json/twitter/borrowed`, `css/bootstrap/visitor`, `bbnf/self_host/internal` | RATIFIED — bench identifier composition `<grammar>/<dataset>/<surface>` is name-as-data, not match-arm dispatch |
| Diagnostic ledger string template (§6b, lines 358, 360) | `BBNF-GRAMMAR001` template uses `{name}` placeholder for grammar; `BBNF-POINTER002` uses `Json` in the help message | RATIFIED — placeholder + help-message example, not dispatch |
| Cookbook receiver (§6b, line 358) | "Onboarding cookbook §two-surfaces" | RATIFIED — documentation receiver, not code |
| Legacy citation (§3, line 84; §10, line 467) | `crates/bbnf-path/src/lib.rs:1-22`, `bbnf-test-fixtures` deletion-archaeology rg gate | RATIFIED — explicit "legacy evidence only; restart package names are `path`, `path-core`, and `path-ts`"; deletion-archaeology framing |
| `Json::parse` example code (§2, lines 45, 53) | `Json::parse(source)?` and `Json::parse_owned(source.to_owned())?` | RATIFIED — illustrative Rust user-surface example; the typed root `Json` is generated, not match-armed |
| Stale prompt clause adjudication (§0, lines 16-23) | the `ParseStream` conflicts cited as "stale prompt clauses" | RATIFIED — explicit conflict-adjudication, not propagation |

Zero hits classify as fault. Lane 5 verification passes both the per-X-table mandate and the no-match-arms-in-generic-crates mandate.

Lane 5 verdict: **honoured**. KEEP 7 / REINVENT 0 / DISCARD 0.

## §8 Lane 6 — Generated-Code + LOC Budget

Per HARDENING.md §"Lane 6": generated-LOC budget per proposed crate/module/wave; xtask regen-cycle wall budget; per-grammar LOC delta projection. Faults: silent budgets, tranche-level budgets without wave-level decomposition, missing baselines.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:399-409 | Generated API budget table | 8 rows × 2 columns (surface / gate) | every surface has a budget | rows 400, 403, 405 cite "+2 percent ceiling per regen" without anchoring to a baseline LOC number from W3 | the steelman "+2% is sufficient if W3 baseline lives elsewhere" — defeated by the audit standard "missing baselines = silent budget" | REINVENT |
| PASS-3.md:400 | Visitor traits budget | "no handwritten visitor file over 500 LOC; per-grammar visitor LOC delta beyond W3 baseline carries a +2 percent ceiling per regen" | 500 LOC handwritten ceiling matches Lock 13; +2% delta ceiling is a reasonable regen drift bound | "W3 baseline" is a forward reference, not a number | the row needs the actual W3 baseline value (e.g., "css_l4 visitor ≤ 22 K LOC at W3 baseline; +2% ceiling per regen") to be enforceable; without it the gate is aspirational | KEEP (gate honoured by mechanism; baseline anchor surgery surfaced as REINVENT in row 1) |
| PASS-3.md:401 | Path metadata Rust budget | "per-grammar path-schema Rust budget ≤ 32 KB" | precise byte budget | none | matches Lock 13 sibling discipline | KEEP |
| PASS-3.md:402 | Path metadata sidecar budget | "≤ 64 KB per grammar; bench manifest sidecar ≤ 8 KB per grammar" | precise byte budgets | none | matches Architecture §10.2 | KEEP |
| PASS-3.md:403 | Tape projection budget | "+2 percent ceiling per regen" | matches PASS-2 §6 budget | "W3 baseline" forward reference | same as row 400 | KEEP (anchor surgery covered) |
| PASS-3.md:404 | Tape identity field/method delta | "≤ 1 field plus 2 methods per regen; larger deltas open a named amendment" | precise count rule | none | the "open a named amendment" escape valve is the right shape | KEEP |
| PASS-3.md:405 | Bench-report generation | "≤ 16 KB markdown; ≤ 8 KB JSON; aggregate ≤ 64 KB" | precise byte budgets | none | matches Lock 13 | KEEP |
| PASS-3.md:406 | Regen wall budget | "`cargo xtask regen --check` ≤ 12 s on M1 Pro for the nine extant grammars; ≤ 14 s including yaml; over-budget regen blocks close" | precise wall + platform + grammar count; matches HARDENING.md §"Lane 6" xtask regen-cycle wall budget mandate | none | challenge "wall regression in CI" defeated by "blocks close" framing | KEEP |
| PASS-3.md:409 | Diagnostics gate | "Generated code list is data; diagnostic rendering code remains shared and non-generated" | preserves shared rendering | none | challenge "diagnostics need per-grammar code" — defeated by "shared and non-generated" rule | KEEP |

The Lane-6 violation: rows 400, 403, 405 quote "+2 percent ceiling per regen" without anchoring to a baseline LOC number from W3. HARDENING.md §"Lane 6" calls out "missing baselines" as fault. Lock 13 sets the standard at file-level (>500 LOC outside `generated/` is forbidden); the prior plan-set carried specific baselines (`docs/tranches/AV/research/04-columnar-soa.md` baseline anchors css_l4.rs ~107 K LOC and bbnf.rs at smaller numbers). PASS-3's "+2%" delta gate without a baseline is a regress relative to the prior anchor discipline.

Surgery: append a baseline-anchor column to the §7 budget table or insert a preamble "W3 baseline anchors per `restart/corpora/CENSUS.md` §4: css_l4.rs 107 K LOC, bbnf.rs 21 K LOC, json.rs 14 K LOC, …; +2% ceilings apply against these anchors."

Lane 6 verdict: **violated-with-recommendation**. KEEP 7 / REINVENT 1 / DISCARD 0. Surgery: anchor "+2 percent" delta gates to explicit W3 baseline LOC numbers per grammar, sourced from `restart/corpora/CENSUS.md` or W3 baseline measurement.

## §9 Lane 7 — Friction Forecast

Friction surfaces per HARDENING.md §"Lane 7": pointer! + select! macro syntax; parse / parse_in / parse_owned lifetime API; ParseStream lazy materialisation (settled away); layout lowering errors; Pratt + SIMD auto-detection misfire diagnostics; crate split migration; adding-a-new-grammar onboarding.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:346-368 | Compiler diagnostic ledger | 15 rows × 5 columns (Code / Verbatim text / Target user / Mental model / Confusion point / Artefact) | every row is committed verbatim | none | challenge "strings will drift" — defeated by §8 carry to "Every code in §6b appears in cookbook table-of-contents and runtime emit tests" | KEEP |
| PASS-3.md:352 | `BBNF-LIFE001` borrowed-escape | "borrowed value escapes parse scope; the source string `&str` was dropped before this projection. help: use `Json::parse_owned(input)` to retain the data, or hold `&input` alive for the duration of `doc`." | committed string with help message; target user "Application author"; confusion point "Default `parse(&str)` borrows" | none | matches the Lock 9 friction surface | KEEP |
| PASS-3.md:353 | `BBNF-LIFE002` arena mismatch | "arena mismatch; root was parsed in arena #N but projected through arena #M. help: use the same `&Arena` for parse and projection." | committed string; specific guidance | none | matches W4a arena cookbook | KEEP |
| PASS-3.md:354-355 | `BBNF-LAYOUT001/002` | warning + error pair on `@layout` | committed strings | none | matches Lane-7 mandate "layout lowering errors" | KEEP |
| PASS-3.md:356-357 | `BBNF-OPT001/002` | Pratt + SIMD informational notes | committed strings; "did not apply" tone is informational | "informational" tone may seem soft | the diagnostics are auto-detection notices, not user errors; informational tone is correct per Lock 10 | KEEP |
| PASS-3.md:358 | `BBNF-GRAMMAR001` workspace-metadata-block missing | "workspace metadata block missing for grammar `{name}`. help: add `[workspace.metadata.bbnf.grammars.{name}]` to your Cargo workspace metadata; the grammar source file alone is not sufficient." | committed string; matches Lock 14 two-surface mandate | none | the message educates the new-grammar author at the exact friction point | KEEP |
| PASS-3.md:359-361 | `BBNF-POINTER001/002/003` | unknown segment + grammar-inference + stale-schema | committed strings; matches Lane-7 pointer! surface | none | matches W5 pointer-syntax-decision § 92-128 | KEEP |
| PASS-3.md:362 | `LookbehindWidth` (`BBNF-LIFE003`) | "lookbehind `\|<` width is unbounded for `{rule}`; help: lookbehinds must be finite-width; use a bounded alternative or move the constraint into a regex with `(?<=...)`." | committed string; matches PASS-1 ownership | "BBNF-LIFE003 vs `LookbehindWidth` code naming inconsistency" — a row identifier is one form, the verbatim header is another | the inconsistency is intentional: PASS-1 owns the formal code (`BBNF1004` per its own contract), PASS-3 stages the user-facing string under `BBNF-LIFE003` to colocate lifetime/lookbehind family | KEEP |
| PASS-3.md:363-366 | `HostSignature` + `ChainStep` + `WasmHost` + `LowererImport` | committed strings; lowerer-import code mirrored from PASS-2 ownership | matches PASS-1 + PASS-2 ownership | none | cross-PASS string ownership is clear | KEEP |

Mental-model coverage census against HARDENING.md §"Lane 7" friction surfaces:

| Friction surface (HARDENING.md §"Lane 7") | PASS-3 ledger code | Verbatim coverage |
|---|---|---|
| `pointer!` macro syntax | `BBNF-POINTER001` (unknown segment), `BBNF-POINTER002` (grammar inference), `BBNF-POINTER003` (stale schema) | three codes; covers field-name, grammar-disambiguation, and regen-stale failure modes |
| `select!` macro syntax | covered indirectly via the same pointer-validation ledger | shared validation surface; could carry `BBNF-SELECT*` separately if `select!` adds traversal-plan-specific failures (a future cookbook delta, not a Lane-7 fault) |
| `parse / parse_in / parse_owned` lifetime API | `BBNF-LIFE001` (borrowed escape), `BBNF-LIFE002` (arena mismatch) | two codes; covers the two W4a-cookbook failure modes verbatim |
| `ParseStream` lazy materialisation | DISCARD; not a friction surface (settled away) | resolved by Lock 1 amendment |
| Layout lowering errors | `BBNF-LAYOUT001` (unused), `BBNF-LAYOUT002` (unresolved) | warning + error pair |
| Pratt + SIMD auto-detection misfire | `BBNF-OPT001` (Pratt declined), `BBNF-OPT002` (SIMD declined) | informational notes; matches Lock 10 spirit (auto-detection without grammar-author annotation) |
| Crate split migration | covered indirectly via §3 path crate naming + §10 carry at line 467 | naming carry is triple-complete |
| Adding-a-new-grammar onboarding | `BBNF-GRAMMAR001` (workspace metadata block missing) | matches Lock 14 two-surface mandate; the message educates the new-grammar author at the exact friction point |
| Lookbehind width unbounded | `LookbehindWidth` (`BBNF-LIFE003`) | committed string with bounded-alternative help |
| Host signature mismatch | `HostSignature` (`BBNF-HOST001`) | committed string with type-flow span detail |
| Host chain step composition | `ChainStep` (`BBNF-HOST002`) | committed string explaining `f1 -> f2` type flow |
| Host chain WASM lowering | `WasmHost` (`BBNF-HOST003`) | committed string disclaiming Rust backend continues |
| Lowerer Grammar IR import | `LowererImport` (`BBNF-CG001`) | committed string mirroring PASS-2 BIR ownership |

Coverage: 13 codes total cover 11 of the 11 Lane-7 friction surfaces (the `select!` surface shares pointer validation; the `ParseStream` surface is settled away). One additional non-blocking opportunity: `BBNF-VISIT*` codes per agent-3's lines 60-68 (declares-no-matching-kinds; borrowed-tree-cannot-mutate; recovery-nodes-skipped) — surfaced as punch item 4.

Lane 7 verdict: **honoured**. KEEP 9 / REINVENT 0 / DISCARD 0.

## §10 Lane 8 — Carry & Deferral Audit

Every "deferred to" / "carries to" / "future" / "TBD" / "user adjudicates" must name receiver, blocker, receiving gate. Faults: any without all three.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:411-426 | Cross-pass hand-off table (§8) | 12 rows × 4 columns (Contract / Receiver / Blocker / Receiving gate) | every row carries the triple | none | challenge "rename Gate to Receiving gate" defeated by Wave-2 amendment | KEEP |
| PASS-3.md:415 | Tape token packing carry | Receiver PASS-1 / Tranche B; Blocker PASS-3 cannot prove cursor identity; Gate runtime identity tests | matches PASS-1 §10 | none | clean cross-pass binding | KEEP |
| PASS-3.md:416 | `ParseStream` rejection carry | Receiver PASS-1 / SYNTHESIS; Blocker naming fork; Gate conflict guard for `ParseStream` in public docs and code | resolves Lock 1 ambiguity | none | matches HARDENING-CONSOLIDATED §3 conflict #2 | KEEP |
| PASS-3.md:418 | Consumer acceptance carry | Receiver PASS-2 / Tranche F + Tranche I; Blocker prose-only hand-off; Gate three executable consumer gates pass | resolves the §3 close gate | none | matches PASS-2.md:336-347 producer-side carry | KEEP |
| PASS-3.md:419 | Per-grammar declaration crates negative carry | Receiver PASS-2 / SYNTHESIS; Blocker generated surfaces reintroduce; Gate negative API and parser fixtures | matches HARDENING-CONSOLIDATED §3 row 6 | none | challenge "negative API check too narrow" — defeated by combined API + parser fixture coverage | KEEP |
| PASS-3.md:420 | Final crate names carry | Receiver SYNTHESIS / Tranche A; Blocker prefixed names re-leak; Gate workspace crate-name check | resolves Lock 7 naming | none | matches MASTER-PLAN §12 + Architecture §3.4 | KEEP |
| PASS-3.md:421 | Hardcoded grammar registry deletion carry | Receiver SYNTHESIS / Tranche I close gate; Blocker registry survives parallel; Gate `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry' crates/` returns zero outside generated | resolves CENSUS §2 violation | none | matches HARDENING-CONSOLIDATED §3 conflict #11 | KEEP |
| PASS-3.md:423 | Performance rows carry | Receiver SYNTHESIS / Tranche H/J; Blocker bench gates become narrative-only; Gate exact benchmark rows appear in master plan gates | matches Lock 8 | none | matches MASTER-PLAN §4 | KEEP |
| PASS-3.md:424 | Incremental fallback gates carry | Receiver PASS-1 / Tranche I; Blocker fallback unreported; Gate dataset-level fallback ledger + LSP policy enforcement test | matches §5 fallback-rate gates by dataset | none | matches HARDENING-CONSOLIDATED §4.36 | KEEP |
| PASS-3.md:425 | Per-grammar feeder rows carry | Receiver SYNTHESIS / Architecture per-X table; Blocker prose; Gate 10-row table consumed verbatim by Architecture | matches Lock 14 per-X-table mandate | none | feeds Architecture §12.1 | KEEP |
| PASS-3.md:462-475 | Unresolved punch-list (§10) | 12 rows × 4 columns | every row carries the triple | none | matches Lane-8 standard | KEEP |
| PASS-3.md:469 | Rare host adapter escape-valve carry | Receiver SYNTHESIS / Architecture rare-escape form; Blocker per-grammar declaration crates re-enter through naming; Gate "Review form requires reason, owner, why metadata + `@host fn` fail, declaration location, deletion path, reviewer, receiving gate" | resolves Amendment-01 escape-valve risk | "the gate is procedural" | the procedural shape is the right safeguard at the architecture level; defeats the steelman | KEEP |

Carry-receiver census (every distinct receiver named in PASS-3 §8 + §10):

| Receiver | Count of carry-rows | Receiving gates |
|---|---:|---|
| PASS-1 / Tranche B | 2 | runtime identity tests over direct root and `ValueRef`; PASS-1 publishes the ABI table |
| PASS-1 / SYNTHESIS | 1 | conflict guard for `ParseStream` in public docs and code |
| PASS-1 / Tranche I | 2 | dataset-level fallback ledger + LSP policy enforcement test |
| PASS-2 / Tranche F | 2 | PASS-3 consumer smokes from generated runtime; metadata schema enumeration |
| PASS-2 / Tranche F + Tranche I | 2 | three executable consumer gates pass on every extant grammar plus yaml |
| PASS-2 / SYNTHESIS | 1 | negative API and parser fixtures |
| SYNTHESIS / Tranche A | 2 | workspace crate-name check |
| SYNTHESIS / Tranche I | 2 | CLI and LSP diagnostics parity test |
| SYNTHESIS / Tranche I close gate | 1 | `rg` close gate returns zero outside generated data |
| SYNTHESIS / Tranche H/J | 2 | exact benchmark rows appear in master plan gates |
| SYNTHESIS / Architecture per-X table | 2 | 10-row table consumed verbatim by Architecture |
| SYNTHESIS + cookbook receivers | 2 | every code in §6b appears in cookbook table-of-contents and runtime emit tests |
| SYNTHESIS / Architecture rare-escape form | 1 | review form with reason / owner / why metadata + `@host fn` fail / declaration location / deletion path / reviewer / receiving gate |
| SYNTHESIS / Architecture Lock 14 proof | 1 | `rg -n 'fixtures/yaml' restart/` returns zero hits inside Lock 14 onboarding allowance |
| SYNTHESIS input-normalization table | 1 | `rg -n 'ParseStream\|rewrite-mode\|Unicode class algebra'` against the SYNTHESIS trio |

Total carry-rows: 24 (12 in §8 hand-off + 12 in §10 unresolved punch-list). Every receiver named is real and drafted (Tranches A, B, F, H, I, J + SYNTHESIS + Architecture); no "future tranche" or fictional successor cited. Every carry triple-complete (Receiver / Blocker / Receiving gate).

Lane 8 verdict: **honoured**. KEEP 11 / REINVENT 0 / DISCARD 0. Every "deferred to" carries receiver/blocker/receiving-gate triple. The carry-blindness failure mode is averted.

## §11 Lane 9 — Greenfield Discipline

User-stated discipline: no quick solutions; no workarounds; no legacy code uncontested; no contrivance; no overengineering; no overcomplication; idiomatic gestalt; architectural transpositions for elegance/simplicity/performance mandatory.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:32, 449-451 | `ParseStream` DISCARD across ledger + summary | rebrand pressure abrogated | matches Lock 1 + HARDENING-CONSOLIDATED §3 conflict #2 | none | the abrogate-before-patch posture is honoured | KEEP |
| PASS-3.md:38, 451-456 | rewrite-mode + grammar Unicode algebra + per-grammar declaration crates DISCARD | settled across §1 + §8 + §9 KEEP/REINVENT/DISCARD summary | matches Lock 14 + Lock 8 | none | matches HARDENING-CONSOLIDATED §3 row 6 | KEEP |
| PASS-3.md:84-92 | path crate names corrected | restart names are `path`, `path-core`, `path-ts`; legacy `bbnf-path` cited as legacy evidence only | resolves Lock 7 + V1 punch item 17 | none | matches Architecture §3.4 + MASTER-PLAN §12 | KEEP |
| PASS-3.md:94-101 | Registry deletion close gate | "Hardcoded grammar marker registries are not a deferral; they are a deletion item" | resolves CENSUS §2 violation; binds the deletion to a verifiable grep | none | challenge "treat as deferral" defeated by explicit "deletion item" + close-gate framing | KEEP |
| PASS-3.md:160 | `@error(recover = ...)` consolidation | "A standalone `@recover` token is a legacy alias only if SYNTHESIS keeps a migration parser; it is not a new V1 extension" | matches HARDENING-CONSOLIDATED §4.9; folds rewrite-mode-adjacent surface into `@error` | none | matches Architecture §8.1 input-normalization-deletion table | KEEP |
| PASS-3.md:162-179, 190 | `DocumentSnapshot` + `ReparsePlan` + LSP user-facing silence policy | architectural transposition for incremental parsing; "Default LSP output is silent on fallback. A debug-only diagnostic channel reports fallback events with snapshot id and reason; the channel is disabled in shipped builds and enabled in development with `BBNF_LSP_DEBUG=1`" | resolves diagnostic-noise risk; matches HARDENING-CONSOLIDATED §4.36 | "may fall back to full parse when anchors fail" | the fallback-rate gate at §5 + LSP user-facing silence policy at line 190 catch the fallback risk; the env-var hatch is correct: dev signal vs user noise | KEEP |
| PASS-3.md:115 | Visitor cohort | "generated `Visitor` traits, `Visit`/walker support, and `VisitTypes` bitflag pruning" | matches W5 inheritance + lightning-css visitor pattern | the cookbook receivers row could fold the visitor cookbook explicitly into §6b's diagnostic table to bind cookbook + diagnostic strings together | non-blocking phrasing tightening | REINVENT |

Greenfield-discipline tally per the user-stated five-clause mandate:

| Discipline clause | PASS-3 surface evidence | Verdict |
|---|---|---|
| No quick solutions | The `ParseStream` rebrand pressure abrogated, not patched (§1 ledger row 4; §8 hand-off row 1; §9 DISCARD summary). The hardcoded grammar registry deletion is not a "we'll deprecate later" — it is a close gate at §3 lines 94-101. | honoured |
| No workarounds | `@error(recover = ...)` consolidation at line 160 folds the `@recover` rewrite-mode-adjacent surface into the canonical declarative directive without preserving the prior parser-rewrite escape. | honoured |
| No legacy code uncontested | Every `crates/bbnf-path/...` citation at line 84 is explicitly framed as "legacy evidence only; restart package names are `path`, `path-core`, and `path-ts`." Every `crates/analysis/...` and `crates/lsp/...` reference at lines 156, 160, 162-179 is framed as "useful entry shape, not the final incremental parse design" — the legacy is contested with a named replacement. | honoured |
| No contrivance / overengineering / overcomplication | The `bbnf` aggregator's 8-children layout deliberately keeps `tape/` and `value/` under `runtime/` instead of duplicating sibling directories — the rationale at line 208 prefers re-export-through-prelude. The `path` / `path-core` / `path-ts` triplet is justified by a Rust-toolchain proc-macro path-dep limitation, not by gold-plating. | honoured |
| Idiomatic gestalt; architectural transpositions for elegance/simplicity/performance | `DocumentSnapshot` + `ReparsePlan` at lines 162-179 is a transposition from the current full-reparse `DocumentState::update` model toward incremental snapshot-based reuse — the transposition is named, the legacy is cited, the new model carries fallback-rate gates and an LSP user-facing silence policy. The `Visitor` + `VisitTypes` + edit-builder mutation discipline at line 115 transposes lightning-css's visitor pattern into bbnf's tape-identity-bearing-direct-struct model. | honoured |

Lane 9 verdict: **honoured**. KEEP 7 / REINVENT 1 / DISCARD 0. The single REINVENT (visitor cookbook fold into §6b) is non-blocking; defer to next pass-through.

## §12 Punch list

Surgical edits to apply BEFORE PASS-3 advances to per-tranche full-spec drafting. Per HARDENING.md §"Punch list", each item names target file:line, verbatim edit (or surgery description), source verdict (REINVENT or DISCARD; never KEEP), owner, scope, lane(s).

| # | Target file:line | Surgery | Verdict | Owner | Scope | Lane(s) |
|---|---|---|---|---|---|---|
| 1 | `restart/audit/pass-3-runtime/PASS-3.md:386-396` | Extend the "Exact PASS-3 benchmark rows" table with two new columns ("Competitor floor", "Platform") populated from `restart/README.md:328-334`. For `json/twitter/borrowed`: Competitor floor = "sonic-rs 436 µs / simd-json 424 µs"; Platform = "M1 Pro". For `json/citm/pointer`: Competitor floor = "sonic-rs 854 µs / simd-json 831 µs"; Platform = "M1 Pro". For `json/canada/array_scan`: Competitor floor = "sonic-rs 3.144 ms"; Platform = "M1 Pro". For `css/bootstrap/visitor`: Competitor floor = "lightning-css ~4.16 ms"; Platform = "M1 Pro (PASS-3 §9 disclaim re: platform ratification)". For `css/animate/layout`: Competitor floor = "lightning-css 1.97 ms"; Platform = "M1 Pro (same disclaim)". Non-throughput rows (`incremental/edit_anchor`, `debug/trace_overhead`) carry "(no Lock-8 claim)" in the Competitor floor column. The `bbnf/self_host/internal` row already disclaims SOTA peer attachment and may carry "(no SOTA peer claim attaches)". Alternative single-line surgery: prepend an attribution preamble before the table — "All rows: M1 Pro platform; competitor floors per `restart/README.md:328-334`; bbnf targets aim to surpass." | REINVENT | PASS-3 amendment author | binding | 4 |
| 2 | `restart/audit/pass-3-runtime/PASS-3.md:399-409` | Extend the "Generated API budget" table with a new column "W3 baseline LOC" populated from `restart/corpora/CENSUS.md` per-grammar measurements. Visitor row: "css_l4 visitor ≤ 22 K LOC at W3 baseline; bbnf visitor ≤ 6 K LOC at W3 baseline; +2% ceiling per regen". Tape projection row: "css_l4 projection ≤ 35 K LOC at W3 baseline; bbnf projection ≤ 8 K LOC at W3 baseline; +2% ceiling per regen". Alternative: insert a baseline preamble before the budget table — "W3 baseline anchors per `restart/corpora/CENSUS.md` §4: css_l4.rs ≈ 107 K LOC, bbnf.rs ≈ 21 K LOC, json.rs ≈ 14 K LOC, google_sheets.rs ≈ N K LOC, math.rs ≈ M K LOC, ebnf.rs / bnf.rs / csv.rs / css_pretty.rs at smaller anchors; +2% ceilings apply against these anchors per regen." The exact baseline numbers may require a one-pass measurement against the W3 baseline branch; without anchors, the gate is silent. | REINVENT | PASS-3 amendment author | binding | 6 |
| 3 | `restart/audit/pass-3-runtime/PASS-3.md:342` (yaml-row host-route cell) | Replace "as declared in `[workspace.metadata.bbnf.grammars.yaml]`" with "decomposed via `host::primitives` + `@host fn` chain in the metadata block per `restart/README.md:155`; no Rust per-grammar code emerges from the onboarding two surfaces". | REINVENT | PASS-3 amendment author | non-blocking | 3 |
| 4 | `restart/audit/pass-3-runtime/PASS-3.md:115` (visitor cookbook routing) | Append a sentence at end of §3 visitor commitments: "Visitor diagnostics emitted by the runtime carry `BBNF-VISIT*` codes; the visitor cookbook table-of-contents indexes each code." Alternatively: extend §6b diagnostic ledger with three `BBNF-VISIT001/002/003` rows (declares-no-matching-kinds; borrowed-tree-cannot-mutate; recovery-nodes-skipped) per `agent-3-visitor-surface-designer.md` lines 60-68. | REINVENT | PASS-3 amendment author | non-blocking | 9 |

Items 1 and 2 are binding (Lane 4 and Lane 6 violations). Items 3 and 4 are non-blocking phrasing tightenings; defer to next pass-through. The two binding items together change ~10 lines of PASS-3.md; both are calibration of existing tables, not new architecture.

Carry hierarchy:
- Item 1 supersedes the §10 carry at PASS-3.md:468 only locally — the carry to SYNTHESIS H/J still holds for any post-PASS-3 platform ratification (especially the lightning-css local-M1 measurement disclaim already noted in PASS-3 §9 and in `restart/README.md:336`).
- Item 2 introduces a baseline-measurement obligation that may need to land at a pre-amendment W3 baseline-capture step. The "+2 percent" delta semantics are correct; only the absolute anchor is missing.
- Item 3 surfaces a documentary-vs-enforceable cell tightening that can land in a single-line edit.
- Item 4 surfaces a cross-document cookbook+ledger fold that can land as either a sentence or three new ledger rows.

## §13 Final readiness

> **Decision: AMENDMENT-REQUIRED.**
>
> PASS-3 V3 returns nine-lane honoured-with-two-recommendations across an independent walk: lanes 1, 3, 5, 7, 8, 9 honoured; lanes 4 and 6 carry violated-with-recommendation; lane 2 N/A. KEEP fraction 94% (66/70 rows) with healthy challenge surface — every KEEP defeats its steelman. Two binding amendments resolve Lane 4 (inline competitor + platform per bench-row, per Lock 8 standard) and Lane 6 (anchor "+2 percent regen ceiling" gates to explicit W3 baseline LOC numbers per grammar). The two non-blocking phrasing tightenings (yaml-row host-route cell explication; visitor cookbook routing into the §6b diagnostic ledger) may close at next pass-through and do not gate per-tranche full-spec drafting.
>
> Hereupon a narrow-scope amendment agent applies punch items 1 + 2; the resulting PASS-3 returns READY without further hardening. The Wave-2 + Wave-3 amendments collectively resolved the V1 punch list; the residual two binding items are calibration of existing tables rather than architectural surgery.

## §14 Provenance and methodology

Methodology per HARDENING.md §"Per-Item Discipline" + §"Methodology": Pro/Con/Explication/Challenge per row; KEEP requires defeating its steelman; REINVENT requires named redesign surviving its steelman; DISCARD requires named replacement surviving its steelman. Voice per `restart/README.md` §13 (calibrated, archaic-permissive, no metalanguage, path:line citations, tables liberal). Independent of three sister auditors V3; V2 not consulted prior to lane verdicts (V2 read after lane verdicts formed; cross-comparison at §15).

## §15 V2 cross-comparison (read AFTER forming V3 verdict)

Per the prompt's §"Independent of three sister auditors. V2 report at `restart/audit/hardening/HARDENING-PASS-3-V2.md` — read ONLY after forming your audit, for §6 comparison".

V2 (`HARDENING-PASS-3-V2.md`, 185 lines, returned READY) and V3 (this document, 243+ lines, returned AMENDMENT-REQUIRED) reach different verdicts on lanes 4 and 6. V2 returned KEEP 6 / REINVENT 0 on Lane 4 and KEEP 8 / REINVENT 0 on Lane 6; V3 returns KEEP 5 / REINVENT 1 on Lane 4 and KEEP 7 / REINVENT 1 on Lane 6.

The divergence is concentrated at two sites:

**Lane 4 — bench-row attribution (PASS-3.md:386-396).** V2's row at line 83 ("json/twitter/borrowed ≤ 380us — 'parse(&str) plus direct root' — matches MASTER-PLAN row — none — platform M1 Pro carried via PASS-2 §7") accepts cross-document attribution as adequate. V3 interprets HARDENING.md §4 ("Every parse-throughput gate cites a competitor + dataset + platform per Lock 8") as a per-row mandate; cross-document carry (the §10 row at 468 "Master/Architecture inline competitor + dataset + platform + bbnf number for every row") is insurance, not substitute. The V3 reading aligns with HARDENING.md's §"Failure modes to avoid" D7 ("SOTA-erasure. Accepting '≥ baseline' as a perf gate. Every gate names a competitor's number"). The bench-row table at 386-396 carries bbnf targets without inlining the competitor floor and platform that already exist verbatim at `restart/README.md:328-334`. V3 surgery: extend the table by two columns, or prepend a single attribution preamble.

**Lane 6 — baseline anchor (PASS-3.md:399-409).** V2's rows at lines 114, 117 ("per-grammar visitor LOC delta beyond W3 baseline carries a +2 percent ceiling per regen — matches Lock 13 + PASS-2 §6 budget — none — wrap to F.W4 budget tooling") accept "+2 percent of W3 baseline" as an anchored gate. V3 interprets HARDENING.md §6 ("Faults: silent budgets, tranche-level budgets without wave-level decomposition, **missing baselines**") as requiring the W3 baseline number to live somewhere observable to the audit at PASS-3 close. The "+2 percent" delta is enforceable only against an anchored value; if W3 baselines are not surfaced (either inline in §7 or via cross-reference to a corpora line), the gate cannot be verified. V3 surgery: append a baseline-anchor column or insert a baseline preamble.

**Sites where V2 and V3 align:**
- Lane 1 — both return all-KEEP; the 14-lock walk produces identical verdicts.
- Lane 5 — both return all-KEEP; registry deletion close gate, fixture separation, 10-row feeder all match.
- Lane 7 — both return all-KEEP; the 15-row diagnostic ledger holds.
- Lane 8 — both return all-KEEP; receiver/blocker/receiving-gate triples are intact.

**Sites where V2 carries non-blocking REINVENT that V3 echoes:**
- §6a yaml-row host-route cell — both V2 (item 1 of §12) and V3 (Lane 3) flag the documentary-vs-enforceable ambiguity.
- §3 visitor cookbook receiver routing — both V2 (item 2 of §12) and V3 (Lane 9) flag the cookbook-fold opportunity.

**Verdict-class divergence summary:**

| Lane | V2 KEEP | V3 KEEP | V2 REINVENT | V3 REINVENT | Net delta |
|---|---:|---:|---:|---:|---|
| 1 | 12 | 12 | 0 | 0 | identical |
| 3 | 8 | 8 | 1 | 1 | identical |
| 4 | 6 | 5 | 0 | 1 | V3 finds Lane-4 attribution residue |
| 5 | 7 | 7 | 0 | 0 | identical |
| 6 | 8 | 7 | 0 | 1 | V3 finds Lane-6 baseline residue |
| 7 | 9 | 9 | 0 | 0 | identical |
| 8 | 9 | 11 | 0 | 0 | V3 walks 11 rows vs V2's 9; both clean |
| 9 | 7 | 7 | 1 | 1 | identical |
| **Totals** | **66** | **66** | **2** | **4** | V3 surfaces 2 additional REINVENT |

Both V2 and V3 confirm the architectural-substantive cohort is sound. The divergence is calibration: V2 treats Lane-4 cross-document attribution and Lane-6 forward-reference baseline as KEEP under the carry-discipline; V3 holds the per-row + present-document standard. Either reading is defensible per the HARDENING.md letter; V3's reading is closer to the prompt's tightened gate-rerun spirit ("Bench harness target numbers and machine profiles … inline competitor + dataset + platform + bbnf number for every row" — the gate explicitly names "every row" as the unit of attribution).

V3 declines to revise its verdict in light of V2. The two binding amendments (Lane 4 + Lane 6 inlining) are inexpensive — single-line preambles or two new columns. The §10 carry to SYNTHESIS H/J remains as insurance for any post-PASS-3 calibration; PASS-3's own surface should carry today's known numbers today.

## §16 Closing posture

PASS-3 is architecturally sound, materially complete, and substantively close to ready. Fourteen of the V1 punch items resolved through Wave-2 + Wave-3 amendments. The two binding amendments (bench-row competitor/platform inlining; +2% delta baseline anchoring) are calibration of existing tables, not new architecture. The three non-blocking phrasing tightenings (yaml-row host-route cell; visitor cookbook routing) close at next pass-through.

Hereupon a narrow-scope amendment agent applies punch items 1 + 2 from §12; the resulting PASS-3 returns READY without further hardening. Per-tranche full-spec drafting begins after that single amendment lands.
