# HARDENING-PASS-3-V4 — Independent V4 closure audit

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` (482 lines, post Wave-4.1 narrow amendment) |
| Audit posture | Independent V4 closure-focused; verify the four V3 punch items closed; 9-lane re-audit |
| Amendment commit in scope | `11806d5d` (Wave 4.1 narrow amendment landing P3-1..P3-4 from `HARDENING-PASS-3-V3.md` §12) |
| Antecedent V3 verdict | AMENDMENT-REQUIRED (KEEP 66 / REINVENT 4 / DISCARD 0); two binding (P3-1, P3-2) + two non-blocking (P3-3, P3-4) |
| V4 invocation lanes | nine; Lane 2 (Sequencing) N/A under single-pass scope |
| Tightened gate-rerun | nine-command checklist re-walked; every gate resolves to expected post-amendment shape |
| V4 output path | `restart/audit/hardening/HARDENING-PASS-3-V4.md` |
| Time budget | 45-minute hard cap; commit at 40 min |

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | honoured | 12 | 0 | 0 | None — 14-lock walk identical to V3; bench-row attribution now inline at PASS-3.md:391-396 closes the prior Lock 8 row-level residue. |
| 2 Sequencing | N/A | — | — | — | Single-pass; sequencing remains MASTER-PLAN's burden. |
| 3 Cohesion | honoured | 9 | 0 | 0 | V3's REINVENT (yaml-row host-route cell at line 342) closed by P3-3 surgery; cell now binds to `host::primitives` + `@host fn` chain. |
| 4 SOTA-Anchoring | honoured | 7 | 0 | 0 | V3's REINVENT (bench-row competitor + platform attribution at lines 386-396) closed by P3-1 surgery; every throughput row now inlines competitor floor + platform. |
| 5 Grammar-Authoritative | honoured | 7 | 0 | 0 | None — registry deletion close gate, fixture separation, and 10-row feeder all hold; yaml-row host-route cell tightening reinforces no-Rust-per-grammar invariant. |
| 6 Generated-Code-Budget | honoured | 8 | 0 | 0 | V3's REINVENT (silent +2% baseline at lines 399-409) closed by P3-2 surgery; W3 baseline LOC anchors now inline (css_l4 ≈ 107 K, bbnf ≈ 21 K, …) with PASS-2.md §6 cross-reference. |
| 7 Friction-Forecast | honoured | 12 | 0 | 0 | Diagnostic ledger grows from 13 to 16 codes (BBNF-VISIT001/002/003 added by P3-4); 11 of 11 friction surfaces covered with verbatim text. |
| 8 Carry-Deferral | honoured | 11 | 0 | 0 | None — every receiver/blocker/receiving-gate triple intact; the §10 carry to SYNTHESIS H/J remains as platform-ratification insurance. |
| 9 Greenfield-Discipline | honoured | 8 | 0 | 0 | V3's REINVENT (visitor cookbook routing at line 115) closed by P3-4 surgery; new sentence binds runtime `BBNF-VISIT*` emission to the cookbook table-of-contents. |

| Verdict class | V3 count | V4 count | Net |
|---|---:|---:|---|
| KEEP | 66 | 74 | +8 (visitor diagnostic rows + closures) |
| REINVENT | 4 | 0 | −4 (all V3 punch items closed) |
| DISCARD | 0 | 0 | 0 |

KEEP fraction 100% (74/74). The healthy challenge surface from V3 is preserved through the per-row Pro/Con/Explication/Challenge discipline; every KEEP defeats its steelman.

**Final decision: READY.** Four surgical edits at commit `11806d5d` close the V3 punch list verbatim. PASS-3 advances to per-tranche full-spec drafting without further hardening.

## §3 V3 punch closure verification

The Wave 4.1 amendment landed four edits against the V3 §12 punch list. Each item is verified at the expected post-amendment line + content.

| # | V3 §12 item | Expected post-amendment evidence | V4 verification | Status |
|---|---|---|---|---|
| P3-1 | Bench-row table extended with Competitor floor + Platform columns (Lane 4 binding) | PASS-3.md:387-399 | Row count 9; lines 391-396 carry `sonic-rs 436 µs / simd-json 424 µs` (twitter), `sonic-rs 854 µs / simd-json 831 µs` (citm), `sonic-rs 3.144 ms` (canada), `lightning-css ~4.16 ms` (bootstrap), `lightning-css 1.97 ms` (animate); non-throughput rows carry `(no Lock-8 claim)`; all rows carry `M1 Pro` platform; bootstrap + animate carry the `restart/README.md:336` ratification disclaim verbatim | CLOSED |
| P3-2 | Generated API budget table extended with W3 baseline LOC column (Lane 6 binding) | PASS-3.md:401-413 | Preamble at line 401 inlines per-grammar W3 baseline LOC: css_l4 ≈ 107,138; bbnf ≈ 21,503; google_sheets ≈ 14,088; css_pretty ≈ 9,021; ebnf ≈ 7,646; json ≈ 3,500; bnf ≈ 3,290; csv ≈ 1,693; math ≈ 871; total 168,750; yaml provisional ≤ 4,000. Visitor row at 405 binds css_l4 ≤ 22 K (≈ 20 percent of 107 K) + bbnf ≤ 6 K (≈ 28 percent of 21 K); tape projection row at 408 binds css_l4 ≤ 35 K (≈ 33 percent of 107 K) + bbnf ≤ 8 K (≈ 38 percent of 21 K); +2% delta gates against named anchors | CLOSED |
| P3-3 | yaml-row host-route cell tightened (Lane 3 non-blocking) | PASS-3.md:342 | Cell now reads `decomposed via host::primitives + @host fn chain in the metadata block per restart/README.md:155; no Rust per-grammar code emerges from the onboarding two surfaces`; the documentary "as declared in `[workspace.metadata.bbnf.grammars.yaml]`" was replaced with the enforceable forward pointer | CLOSED |
| P3-4 | Visitor cookbook routing into §6b ledger (Lane 9 non-blocking) | PASS-3.md:115 + §6b ledger | §3 line 115 trailing sentence: `Visitor diagnostics emitted by the runtime carry BBNF-VISIT* codes (rows in §6b); the visitor cookbook table-of-contents indexes each code so authors land on the relevant chapter from the diagnostic alone`; §6b ledger at lines 362-364 carries three new rows: BBNF-VISIT001 (declares-no-matching-kinds, warning, Visitor cookbook §pruning), BBNF-VISIT002 (borrowed-tree-cannot-mutate, error, Visitor cookbook §mutation), BBNF-VISIT003 (recovery-nodes-skipped, warning, Visitor cookbook §recovery) per `agent-3-visitor-surface-designer.md:60-68` | CLOSED |

All four V3 punch items closed. The amendment touched 55 lines of PASS-3.md (per `git show --stat 11806d5d`), 35 lines of MASTER-PLAN.md, and 2 lines of README.md — within scope expectations for narrow calibration of existing tables and cells.

## §4 Lane 1 — Lock-Adherence (re-confirm)

V3 returned all-KEEP across the 14-lock walk; the Wave 4.1 amendment left the structural lock surface untouched, only filling in attribution + anchor cells. The closure of the Lock 8 row-level residue (P3-1) means Lock 8 advances from "partially honoured" (V3 §3 narrative) to fully honoured at the present-document level.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:16 | Lock 1 — tape unioned with direct-to-struct | "tape is the substrate and is properly unioned with direct-to-struct. It must not be renamed to `ParseStream`" | settled-authority phrasing carried verbatim; matches `restart/locks/LOCKS.md:34` reframe | none | the steelman "ParseStream removes baggage" — defeated by the amended Lock 1 explicitly amending the no-rename clause | KEEP |
| PASS-3.md:31 | Tape/direct union verdict cell | "Tape is the single advanced substrate; direct structs remain the ergonomic default" | matches Lock 1 union mandate | "Requires careful identity invariants" surfaced as Con | the challenge "every visible direct node carries tape identity" is the steelman the amended row commits to; defeats the parallel-substrate steelman | KEEP |
| PASS-3.md:32 | `ParseStream` DISCARD row | DISCARD across all surfaces | resolves the prompt+inheritance-index conflict | none | challenge "would fork naming and contracts" defeats any rebrand pressure | KEEP |
| PASS-3.md:38 | Rewrite-mode + grammar Unicode algebra DISCARD | matches `restart/README.md:123, 133-143` | resolves stale prompt clauses | none | challenge wins; rewrite folds into visitor edit-builders, Unicode folds into `parse-that/regex` | KEEP |
| PASS-3.md:30 | Lock 9 — three-constructor surface | `parse`, `parse_in`, `parse_owned` per `restart/locks/LOCKS.md:50` | every constructor matches BB W4a/cookbook precedent | "Owned mode can hide copies" surfaced | mitigation cited (bench all modes separately); defeats the steelman | KEEP |
| PASS-3.md:84-92 | Lock 7 — path crate names | `path-core` + `path` + `path-ts` triplet matches `restart/locks/LOCKS.md:46` (Rust-toolchain proc-macro path-dep limitation) | none | challenge "three proc-macro shells is a fault" defeated by `path-core` non-proc-macro shared base | KEEP |
| PASS-3.md:115 | Lock 13 — visitor cohort discipline | generated `Visitor` trait + `Visit`/walker + `VisitTypes` bitflag pruning + cookbook routing for `BBNF-VISIT*` codes | matches W5 inheritance + cookbook discoverability | risk "large generated APIs need restraint" surfaced | mitigation: prelude exports common; advanced modules detail-gated; cookbook indexes each code | KEEP |
| PASS-3.md:194-208 | Lock 13 — `bbnf` aggregator 8 children | `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/` | child count 8 ∈ [4, 10]; matches Lock 13 sonic-rs/lightning-css/simdjson standard | "tape/ and value/ live elsewhere" required explicit rationale; carried at line 208 | the steelman "duplicate sibling surfaces in `bbnf/src/`" defeated by re-export-through-prelude rationale | KEEP |
| PASS-3.md:320-325 | Lock 14 — fixture separation onboarding gate | "Lock 14 onboarding admits exactly two surfaces … `fixtures/yaml/*` is *not* part of the onboarding allowance" | resolves the V1 fixture-residue concern; binds verification grep at 325 | none | the steelman "fixtures are part of onboarding" defeated by the explicit two-phase split | KEEP |
| PASS-3.md:328-344 | Lock 14 — 10-row per-grammar feeder | 10 rows × 7 columns; yaml row's host-route cell now binds `host::primitives` + `@host fn` chain (P3-3) | every "all grammars" claim resolves through this table; documentary cell now enforceable | none | matches Lock 14 per-X-table mandate | KEEP |
| PASS-3.md:391-396 | Lock 8 — bench-row competitor + platform attribution | every throughput row inlines competitor floor + M1 Pro platform; non-throughput rows declare "(no Lock-8 claim)"; bootstrap + animate carry README:336 disclaim | every parse-throughput gate now cites competitor + dataset + platform per Lock 8 | none | the steelman "cross-document carry suffices" defeated by HARDENING.md §"Failure modes" D7 ("Every gate names a competitor's number") at the row level | KEEP |
| PASS-3.md:94-101 | Lock 14 — registry deletion close gate | `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry\|hardcoded_grammar_registry'` returns zero outside generated | resolves CENSUS §2 violation propagation risk | none | challenge "treat as deferral" defeated by explicit "deletion item" framing | KEEP |

Walk over the 14 locks at the line where each is honoured (or rationally delegated):

- **Lock 1** — honoured (PASS-3.md:16, 31; tape/direct union explicit; `ParseStream` DISCARD across §1 ledger + §8 hand-off + §9 summary)
- **Lock 2** — silent at PASS-3 surface (Layout-lowering canon is PASS-1/PASS-2 territory; PASS-3 references `@layout` only as user-surface directive). Acceptable; not PASS-3 scope.
- **Lock 3** — silent at PASS-3 surface (cursor-parse + byte-skip elision is PASS-1/runtime engine territory).
- **Lock 4** — silent at PASS-3 surface (CSP + e-graph orthogonal optimisation is PASS-1 territory).
- **Lock 5** — honoured indirectly via §3 consumer-acceptance gates binding PASS-2 emission contract; the LowererImport diagnostic at line 366 mirrors the BIR/Grammar IR import-deny rule.
- **Lock 6** — honoured via "xtask emits committed source artefacts" cited at lines 103, 408 (registry deletion grep targets `crates/`, regen wall budget cites `cargo xtask regen --check`); no proc-macro façade for codegen output proposed.
- **Lock 7** — honoured (PASS-3.md:84, 273-299; `path` + `path-core` + `path-ts` triplet)
- **Lock 8** — **honoured at row level** (PASS-3.md:391-396; competitor + platform inline per row, closing the V3 row-level residue)
- **Lock 9** — honoured (PASS-3.md:30, 60-78)
- **Lock 10** — honoured via `BBNF-OPT001/002` diagnostics (lines 356-357) which announce auto-detection misfire; no grammar-level `@pratt`/`@simd` directive proposed
- **Lock 11** — silent at PASS-3 surface (path-deps for incubating sister crates is workspace-shape territory; PASS-3 §6 module trees match)
- **Lock 12** — silent at PASS-3 surface (ser/gorgeous archive ceremony is Tranche A.W0 territory; PASS-3 module trees do not include `ser/` or `gorgeous/`)
- **Lock 13** — honoured (PASS-3.md:194-208, 210-272, 273-318; every crate's `src/` tree is 4–10 children, sibling-API uniform; visitor cookbook routing reinforces `bbnf` aggregator's `visitor/` + `diagnostics/` cohesion)
- **Lock 14** — honoured (PASS-3.md:320-325 fixture separation; 328-344 10-row feeder including the tightened yaml host-route cell at 342; 94-101 registry deletion close gate; 38 rewrite/Unicode/per-grammar-crate DISCARD)

Lane 1 verdict: **honoured**. KEEP 12 / REINVENT 0 / DISCARD 0. The Wave 4.1 amendment promotes Lock 8 from "partially honoured" (V3 narrative §3, line 70) to fully honoured at the row level.

## §5 Lane 2 — Sequencing Discipline

N/A. PASS-3 is single-pass; sequencing across waves is MASTER-PLAN territory. The cross-pass hand-off table at §8 (lines 411-426) and unresolved punch-list at §10 (lines 462-475) carry receiver/blocker/receiving-gate triples that bind any deferred consumer to a named gate, but the audit's lane-2 substrate-then-consumer check applies to wave-level plans.

## §6 Lane 3 — Cohesion (P3-3 closure-focused re-walk)

V3's single REINVENT (yaml-row host-route cell at line 342) closed by P3-3.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:60-78 | `Grammar` trait + `DocumentView` | typed-root + three-constructor + `root_value()` + diagnostics | precise lifetime parameterisation; matches W4 cookbook | none | the steelman "drop lifetime params from default API" defeated by Lock 9 mandate | KEEP |
| PASS-3.md:104-112 | three executable consumer-acceptance gates | `parse`/`parse_in`/`parse_owned` smoke; `DocumentView` metadata feeds visitors + selectors; materialisation cost table generated | resolves orphan-consumer risk | "materialisation_cost.toml or equivalent" is artefact-name flexibility | flexibility appropriate | KEEP |
| PASS-3.md:130-152 | tape illustrative shape | `Tape<'input>` with `tokens: Box<[TapeToken]>`; `TapeToken` with kind/flags/start/end/payload/sibling_skip; `ValueRef<'doc, 'input, K>` | precise typed shape | "user-surface contract, not PASS-1 mandate" disclaimed at 153 | the disclaimer is correct: PASS-1 owns ABI; PASS-3 commits to the semantic surface | KEEP |
| PASS-3.md:181-190 | fallback-rate gates by dataset | 4 corpora × 4 columns | dataset-level thresholds bind incremental contract | "BBNF self-edit corpus" target 70% reuse / 15% fallback may be loose | the looseness is calibrated for self-host bootstrap workload; defeats the steelman | KEEP |
| PASS-3.md:194-208 | bbnf canonical 8-children layout | matches Lock 13 + rationale at 208 | none | matches `restart/locks/LOCKS.md:58` standard | KEEP |
| PASS-3.md:210-251 | bbnf-cli + bbnf-language-server trees | clean concern-split | sibling-API uniform | language-server tree at 19 entries spans 7 sibling directories; child count 7 ∈ [4, 10] | matches Lock 13 | KEEP |
| PASS-3.md:252-272 | bbnf-bench tree | harness + datasets + competitors + report | child count 5 ∈ [4, 10] | matches Lock 13 | KEEP |
| PASS-3.md:273-318 | path/path-core/path-ts/test-fixtures trees | unprefixed names; concern-split | matches Lock 7 + Lock 13 | "fixtures/ has 4 children" — child count 4 is the lower bound of [4, 10] | matches Lock 13 boundary | KEEP |
| PASS-3.md:342 | §6a yaml row host-route cell **(P3-3 closure)** | "decomposed via `host::primitives` + `@host fn` chain in the metadata block per `restart/README.md:155`; no Rust per-grammar code emerges from the onboarding two surfaces" | documentary cell now enforceable; forward pointer to README:155 + Lock 14 onboarding two-surface mandate; reinforces "zero Rust edits" at line 322 + 344 | none | the steelman "cell is documentary; enforceability lives in the metadata schema" defeated by the cell now naming both the decomposition mechanism (`host::primitives` + `@host fn` chain) AND the no-Rust invariant — the cell carries enforcement intent on its face | KEEP |

Lane 3 verdict: **honoured**. KEEP 9 / REINVENT 0 / DISCARD 0. The V3 REINVENT closed by surgical replacement at line 342 with the verbatim text proposed in V3 punch item #3.

## §7 Lane 4 — SOTA-Anchoring (P3-1 closure-focused re-walk)

V3's REINVENT (bench-row competitor + platform attribution at lines 386-396) closed by P3-1.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:374-385 | Dataset baseline citation table | 5 rows × 3 columns | every row points to `restart/corpora/SOTA.md` line range | the "Baseline citation" cell points to a corpus path | the steelman "corpus paths are sufficient" — V3 flagged this; the §7 bench-row table now inlines competitor numbers per row, so the §6 dataset-baseline table no longer needs to carry the absolute number itself; the dataset-baseline table is the corpus index, the bench-row table is the gate ledger | KEEP |
| PASS-3.md:387-389 | Bench-row table preamble + columns **(P3-1 closure)** | preamble at 387 binds row-level attribution to `restart/locks/LOCKS.md:50` and `restart/README.md:328-334`; carry to SYNTHESIS H/J at §10 framed as insurance, not substitute | every row now carries Competitor floor + Platform columns | none | the steelman "cross-document carry suffices" defeated by per-row attribution at 391-396 | KEEP |
| PASS-3.md:391 | `json/twitter/borrowed` ≤ 380 µs **(P3-1)** | Competitor floor "sonic-rs 436 µs / simd-json 424 µs"; Platform "M1 Pro" | matches `restart/README.md:328` verbatim | none | challenge "single competitor floor" defeated by dual-citation (sonic-rs + simd-json) | KEEP |
| PASS-3.md:392 | `json/twitter/tape_cursor` ≤ borrowed + 10% **(P3-1)** | Competitor floor "(no Lock-8 claim; relative to bbnf borrowed row)"; explicit relative-internal framing | non-throughput-surface row honestly disclaims | none | matches HARDENING-CONSOLIDATED §4.33 non-Lock-8 row framing | KEEP |
| PASS-3.md:393 | `json/citm/pointer` ≤ 750 µs **(P3-1)** | Competitor floor "sonic-rs 854 µs / simd-json 831 µs"; Platform "M1 Pro" | matches `restart/README.md:328-334` | none | dual-citation matches the README-established pattern | KEEP |
| PASS-3.md:394 | `json/canada/array_scan` ≤ 2.8 ms **(P3-1)** | Competitor floor "sonic-rs 3.144 ms"; Platform "M1 Pro" | matches `restart/README.md:329` | none | single-competitor citation matches the README's lone citation for canada | KEEP |
| PASS-3.md:395 | `css/bootstrap/visitor` ≤ 3.0 ms **(P3-1)** | Competitor floor "lightning-css ~4.16 ms"; Platform "M1 Pro (PASS-3 §9 disclaim re: platform ratification per `restart/README.md:336`)" | matches `restart/README.md:330` + carries the platform-ratification disclaim verbatim | the disclaim makes the row's platform inheritance conditional | the conditional inheritance is the right shape: bbnf carries today's known number AND today's ratification disclaim | KEEP |
| PASS-3.md:396 | `css/animate/layout` ≤ 1.6 ms **(P3-1)** | Competitor floor "lightning-css 1.97 ms"; Platform "M1 Pro (same disclaim as bootstrap row)" | matches `restart/README.md:331` | same as bootstrap | matches the README pattern | KEEP |
| PASS-3.md:397-399 | non-throughput rows | self-host, edit_anchor, trace_overhead all carry "(no Lock-8 claim)" or equivalent | non-throughput engineering gates appropriately do NOT claim Lock 8 | none | challenge "mechanism gates need numbers" — defeated; dataset-level fallback gates at §5 carry the actual thresholds | KEEP |
| PASS-3.md:468 | §10 carry "Bench harness target numbers and machine profiles" | Receiver SYNTHESIS / Tranche H/J; Blocker bench rows become aspirational; Receiving gate "Master/Architecture inline competitor + dataset + platform + bbnf number for every row" | the carry is now insurance for post-PASS-3 platform ratification (especially the lightning-css local-M1 disclaim), not a substitute for present-document attribution | none | the carry remains correctly framed | KEEP |

Verbatim row-level attribution census against `restart/README.md:328-334`:

| Bench row (PASS-3.md) | bbnf target | Competitor floor (inlined) | Platform (inlined) | README:328-334 match |
|---|---|---|---|---|
| `json/twitter/borrowed` (391) | ≤ 380 µs | sonic-rs 436 µs / simd-json 424 µs | M1 Pro | exact |
| `json/twitter/tape_cursor` (392) | ≤ borrowed + 10% | (no Lock-8 claim; relative to bbnf) | M1 Pro | non-throughput honest disclaim |
| `json/citm/pointer` (393) | ≤ 750 µs | sonic-rs 854 µs / simd-json 831 µs | M1 Pro | exact |
| `json/canada/array_scan` (394) | ≤ 2.8 ms | sonic-rs 3.144 ms | M1 Pro | exact |
| `css/bootstrap/visitor` (395) | ≤ 3.0 ms | lightning-css ~4.16 ms | M1 Pro + ratification disclaim | exact |
| `css/animate/layout` (396) | ≤ 1.6 ms | lightning-css 1.97 ms | M1 Pro + same disclaim | exact |
| `bbnf/self_host/internal` (397) | ≤ 100 ms | (no SOTA peer claim attaches) | M1 Pro | non-Lock-8 internal gate |
| `incremental/edit_anchor` (398) | report fallback rate | (no Lock-8 claim; non-throughput) | M1 Pro | non-throughput |
| `debug/trace_overhead` (399) | report overhead | (no Lock-8 claim; non-throughput) | M1 Pro | non-throughput |

Every throughput row inlines a competitor floor. Every row carries a platform. Every non-throughput row honestly disclaims Lock-8 attachment. The simdjson sustained row noted in V3 (`restart/README.md:333`, ≥ 5 GB/s sustained M-series; ≥ 7 GB/s x86) remains absent from PASS-3 §7 — but on review, this is a scope decision (PASS-3 §7 covers per-grammar bench rows only; sustained throughput is MASTER-PLAN J/K territory). V3's note about this gap is preserved as informational; it is not a Lane-4 fault at PASS-3 scope.

Lane 4 verdict: **honoured**. KEEP 7 / REINVENT 0 / DISCARD 0. The V3 violation closed by P3-1 surgery. Per-row competitor + platform attribution honours Lock 8 at the present-document level; the §10 carry to SYNTHESIS remains as ratification insurance.

## §8 Lane 5 — Grammar-Authoritative Discipline (re-confirm)

V3 returned all-KEEP. P3-3 (yaml-row host-route cell) reinforces the no-Rust-per-grammar invariant at the per-X table level.

Verification:
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-3-runtime/PASS-3.md` — **zero matches** (re-verified V4).
- `rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math'` — every match classifies as ratified per the V3 §7 classification taxonomy; new V4 hits at the W3-baseline preamble (line 401: css_l4 ≈ 107,138 LOC, bbnf ≈ 21,503 LOC, …) classify as **per-X table cell** (the LOC anchors are per-grammar measurements, not match-arm dispatch).

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:94-101 | Registry deletion close gate | `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry\|hardcoded_grammar_registry'` returns zero outside generated data | resolves CENSUS §2 violation; binds the deletion to a verifiable grep | none | challenge "deferral, not deletion" — defeated by explicit "deletion item" + close-gate framing | KEEP |
| PASS-3.md:320-325 | Fixture separation Lock 14 onboarding | onboarding admits two surfaces; fixtures land in parity phase | resolves the V1 fixture-residue concern; binds verification grep at 325 | none | challenge "fixtures are part of onboarding" defeated by explicit two-phase gate | KEEP |
| PASS-3.md:328-344 | 10-row per-grammar feeder + tightened yaml row | 10 rows × 7 columns; yaml host-route cell at 342 binds `host::primitives` + `@host fn` chain (P3-3) | every "all grammars" claim resolves through this table; yaml row enforces no-Rust invariant on its face | none | matches Lock 14 per-X-table mandate AND defeats the documentary-vs-enforceable critique V3 raised | KEEP |
| PASS-3.md:90-91 | "Generated grammar metadata replaces fixture registries" | path schema validates compile-time | resolves the hardcoded-registry concern in the path/select DSL | none | matches Architecture §10.1 path inheritance | KEEP |
| PASS-3.md:300-318 | test-fixtures crate tree | data + manifests only; no per-grammar Rust | matches Lock 14 + Amendment 01 | "fixtures/ holds 4 grammar dirs" — these are post-onboarding parity surface, NOT onboarding allowance | the fixture-separation block at 320-325 enforces this distinction explicitly | KEEP |
| PASS-3.md:38, 451-456 | Per-grammar declaration crates DISCARD; rewrite-mode DISCARD; grammar-level Unicode algebra DISCARD | matches HARDENING-CONSOLIDATED §3 row 6 | settled across §1 + §8 + §9 | none | challenge "rare host adapter escape valve" addressed at §10 line 472 | KEEP |
| PASS-3.md:344 | yaml onboarding boundary | "every cell to the left of the parity-phase fixture manifest must be generated from `yaml.bbnf` plus the workspace-metadata block, with zero Rust edits and zero per-grammar match arms" | matches Lock 14 two-surface mandate | none | challenge "yaml needs a Rust file" — defeated by explicit "zero Rust edits" requirement, reinforced by the tightened cell at 342 | KEEP |

Lane 5 verdict: **honoured**. KEEP 7 / REINVENT 0 / DISCARD 0.

## §9 Lane 6 — Generated-Code + LOC Budget (P3-2 closure-focused re-walk)

V3's REINVENT (silent +2% baseline at lines 399-409) closed by P3-2.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:401 | Generated API budget preamble **(P3-2 closure)** | per-grammar W3 baseline LOC inlined: css_l4 ≈ 107,138; bbnf ≈ 21,503; google_sheets ≈ 14,088; css_pretty ≈ 9,021; ebnf ≈ 7,646; json ≈ 3,500; bnf ≈ 3,290; csv ≈ 1,693; math ≈ 871; total 168,750; yaml provisional ≤ 4,000; sourced from `restart/audit/pass-2-codegen/PASS-2.md:380-392` | the "+2 percent" delta now anchors against a named, present-document number | none | the steelman "anchor lives elsewhere" defeated by the in-line preamble — the gate is verifiable from PASS-3 alone | KEEP |
| PASS-3.md:403-404 | Generated API budget table header | "W3 baseline LOC" column added | every row carries a baseline cell | none | matches HARDENING.md §"Lane 6" baseline mandate | KEEP |
| PASS-3.md:405 | Visitor traits row **(P3-2)** | "css_l4 visitor ≤ 22 K LOC at W3 baseline (≈ 20 percent of 107 K); bbnf visitor ≤ 6 K LOC at W3 baseline (≈ 28 percent of 21 K); other-grammar visitor LOC scales with each grammar's `generated_loc` row in PASS-2.md §6"; gate "no handwritten visitor file over 500 LOC; per-grammar visitor LOC delta beyond the W3 baseline anchor carries a +2 percent ceiling per regen" | precise per-grammar percentage anchors; cross-grammar generalisation via PASS-2.md §6 carry | "≈ 20 percent" approximation may drift | the ratio-based framing is the right shape; the absolute numbers (22 K, 6 K) bind the gate | KEEP |
| PASS-3.md:406 | Path metadata (Rust) row | "Counted within each grammar's `generated_loc` PASS-2 row; no separate W3 anchor" + "≤ 32 KB per grammar" | precise byte budget; honest "no separate W3 anchor" framing | none | matches Lock 13 sibling discipline | KEEP |
| PASS-3.md:407 | Path metadata (sidecar) row | "Sidecar files are not Rust source; no W3 LOC anchor applies" + "≤ 64 KB per grammar; bench manifest sidecar ≤ 8 KB per grammar" | precise byte budgets; honest sidecar disclaim | none | matches Architecture §10.2 | KEEP |
| PASS-3.md:408 | Tape projections row **(P3-2)** | "css_l4 projection ≤ 35 K LOC at W3 baseline (≈ 33 percent of 107 K); bbnf projection ≤ 8 K LOC at W3 baseline (≈ 38 percent of 21 K)"; gate "+2 percent ceiling per regen" | precise per-grammar anchors; ratio + absolute dual citation | none | matches PASS-2 §6 budget; the +2% delta now has a named origin | KEEP |
| PASS-3.md:409 | Tape identity field/method delta row | "Field/method count, not LOC; the W3 anchor is the post-PASS-1 substrate definition (§4 above)" + "≤ 1 field plus 2 methods per regen; larger deltas open a named amendment" | honest "not LOC" framing + precise count rule + named-amendment escape valve | none | matches Lock 13 | KEEP |
| PASS-3.md:410 | Bench-report generation row | "Generated artefacts; no W3 LOC anchor applies" + "≤ 16 KB markdown; ≤ 8 KB JSON; aggregate ≤ 64 KB" | precise byte budgets; honest no-LOC-anchor framing | none | matches Lock 13 | KEEP |
| PASS-3.md:411 | Regen wall budget row | "Wall time, not LOC; PASS-2.md §6 carries observed-vs-provisional baselines" + "≤ 12 s on M1 Pro for the nine extant grammars; ≤ 14 s including yaml" | precise wall + platform + grammar count | none | matches HARDENING.md §"Lane 6" xtask regen-cycle wall budget mandate | KEEP |

Lane 6 verdict: **honoured**. KEEP 8 / REINVENT 0 / DISCARD 0. The V3 violation closed by P3-2 surgery. Per-grammar W3 baseline LOC anchors are now inline; the +2% delta gates against named values; visitor and tape-projection rows carry per-grammar absolute + ratio anchors; non-LOC budget rows (path metadata, sidecar, bench-report, regen wall) honestly disclaim "no W3 LOC anchor applies."

## §10 Lane 7 — Friction Forecast (P3-4 expansion re-walk)

V3 returned all-KEEP across 9 rows covering 11 friction surfaces. P3-4 expands the diagnostic ledger from 13 codes (V3) to 16 codes (V4) with three new BBNF-VISIT* rows.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:115 | §3 visitor commitments — cookbook routing **(P3-4 closure)** | trailing sentence: "Visitor diagnostics emitted by the runtime carry `BBNF-VISIT*` codes (rows in §6b); the visitor cookbook table-of-contents indexes each code so authors land on the relevant chapter from the diagnostic alone" | binds runtime emission to cookbook discoverability; closes the V3 §9 REINVENT | none | the steelman "cookbook + ledger live in separate documents" defeated by the explicit code → chapter index commitment | KEEP |
| PASS-3.md:346-371 | Compiler diagnostic ledger | 16 rows × 5 columns (code / verbatim text / target user / mental model / confusion point / artefact) | every row committed verbatim; coverage expands to BBNF-VISIT* family | none | challenge "strings will drift" — defeated by §8 carry to "Every code in §6b appears in cookbook table-of-contents and runtime emit tests" | KEEP |
| PASS-3.md:352-353 | `BBNF-LIFE001/002` | borrowed-escape + arena mismatch | committed with help messages | none | matches Lock 9 friction surface | KEEP |
| PASS-3.md:354-355 | `BBNF-LAYOUT001/002` | warning + error pair on `@layout` | committed strings | none | matches Lane-7 mandate "layout lowering errors" | KEEP |
| PASS-3.md:356-357 | `BBNF-OPT001/002` | Pratt + SIMD informational notes | committed strings; "did not apply" tone is informational | "informational" tone may seem soft | the diagnostics are auto-detection notices, not user errors; informational tone is correct per Lock 10 | KEEP |
| PASS-3.md:358 | `BBNF-GRAMMAR001` | workspace-metadata-block missing | committed string with `{name}` placeholder; matches Lock 14 two-surface mandate | none | educates the new-grammar author at the exact friction point | KEEP |
| PASS-3.md:359-361 | `BBNF-POINTER001/002/003` | unknown segment + grammar inference + stale schema | committed strings; matches Lane-7 pointer! surface | none | matches W5 pointer-syntax-decision § 92-128 | KEEP |
| PASS-3.md:362 | `BBNF-VISIT001` **(P3-4)** | `warning[BBNF-VISIT001]: visitor declares no matching node kinds. help: add the desired kind to VisitTypes or remove the visitor.` — Visitor author / "Empty `VisitTypes` walks every node." / "Bitflag default vs. declared-kinds intent." / Visitor cookbook §pruning | matches `agent-3-visitor-surface-designer.md:60-68` declares-no-matching-kinds case | none | covers the empty-bitflag friction case | KEEP |
| PASS-3.md:363 | `BBNF-VISIT002` **(P3-4)** | `error[BBNF-VISIT002]: borrowed parse tree cannot be mutated in place. help: use parse_owned, parse_in with a mutable arena document, or emit an edit plan via the edit builder.` — Visitor author / "Visitors always mutate." / "Borrowed root is shared-immutable." / Visitor cookbook §mutation | matches borrowed-tree-cannot-mutate case; help message routes to all three Lock 9 escape hatches | none | covers the borrowed-immutable friction case | KEEP |
| PASS-3.md:364 | `BBNF-VISIT003` **(P3-4)** | `warning[BBNF-VISIT003]: recovery nodes skipped by this visitor. help: implement visit_error or enable VisitTypes::ERROR.` — Visitor author / "Default visitor sees every node." / "Recovery nodes opted out by default." / Visitor cookbook §recovery | matches recovery-nodes-skipped case | none | covers the visit-error opt-in friction case | KEEP |
| PASS-3.md:365 | `LookbehindWidth` (`BBNF-LIFE003`) | committed string with bounded-alternative help | matches PASS-1 ownership | "BBNF-LIFE003 vs `LookbehindWidth` code naming inconsistency" | the inconsistency is intentional: PASS-1 owns the formal code (`BBNF1004`), PASS-3 stages the user-facing string under `BBNF-LIFE003` to colocate lifetime/lookbehind family | KEEP |
| PASS-3.md:366-369 | `HostSignature` + `ChainStep` + `WasmHost` + `LowererImport` | committed strings; lowerer-import code mirrored from PASS-2 ownership | matches PASS-1 + PASS-2 ownership | none | cross-PASS string ownership is clear | KEEP |

Mental-model coverage census (V4 update — 16 codes total cover 12 of 12 friction surfaces; the V3 single-row note about `select!` shared validation now covers 12 surfaces with the BBNF-VISIT* family expansion):

| Friction surface | PASS-3 ledger code | Verbatim coverage |
|---|---|---|
| `pointer!` macro syntax | `BBNF-POINTER001/002/003` | three codes; field-name + grammar-disambiguation + regen-stale modes |
| `select!` macro syntax | covered indirectly via pointer-validation ledger | shared validation surface |
| `parse / parse_in / parse_owned` lifetime API | `BBNF-LIFE001/002` | borrowed-escape + arena-mismatch |
| `ParseStream` lazy materialisation | DISCARD; not a friction surface | resolved by Lock 1 amendment |
| Layout lowering errors | `BBNF-LAYOUT001/002` | warning + error pair |
| Pratt + SIMD auto-detection misfire | `BBNF-OPT001/002` | informational notes |
| Crate split migration | covered indirectly via §3 path crate naming + §10 carry at line 470 | naming carry is triple-complete |
| Adding-a-new-grammar onboarding | `BBNF-GRAMMAR001` | matches Lock 14 two-surface mandate |
| Lookbehind width unbounded | `LookbehindWidth` (`BBNF-LIFE003`) | bounded-alternative help |
| Host signature / chain / WASM mismatch | `HostSignature` / `ChainStep` / `WasmHost` (`BBNF-HOST001/002/003`) | type-flow + chain-composition + WASM disclaim |
| Lowerer Grammar IR import | `LowererImport` (`BBNF-CG001`) | mirroring PASS-2 ownership |
| Visitor declares-no-matching-kinds **(P3-4)** | `BBNF-VISIT001` | empty-bitflag warning |
| Visitor borrowed-tree-cannot-mutate **(P3-4)** | `BBNF-VISIT002` | error with three escape-hatch help |
| Visitor recovery-nodes-skipped **(P3-4)** | `BBNF-VISIT003` | warning with `visit_error` opt-in |

Coverage: 16 codes cover 12 of 12 friction surfaces. The `select!` surface shares pointer validation by design; the `ParseStream` surface is settled away.

Lane 7 verdict: **honoured**. KEEP 12 / REINVENT 0 / DISCARD 0. The Wave 4.1 amendment expands ledger coverage from 13 to 16 codes and adds the cookbook-routing commitment at line 115.

## §11 Lane 8 — Carry & Deferral Audit (re-confirm)

V3 returned all-KEEP across 11 rows. The Wave 4.1 amendment did not introduce or modify any carries; it filled in attribution + anchors that were previously deferred to SYNTHESIS-only. The §10 carry at line 468 (bench harness target numbers) remains as ratification insurance — not a substitute for present-document attribution.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:411-426 | Cross-pass hand-off table (§8) | 12 rows × 4 columns | every row carries the Receiver/Blocker/Receiving-gate triple | none | challenge "rename Gate to Receiving gate" defeated by Wave-2 amendment | KEEP |
| PASS-3.md:415 | Tape token packing carry | Receiver PASS-1 / Tranche B | matches PASS-1 §10 | none | clean cross-pass binding | KEEP |
| PASS-3.md:416 | `ParseStream` rejection carry | Receiver PASS-1 / SYNTHESIS | resolves Lock 1 ambiguity | none | matches HARDENING-CONSOLIDATED §3 conflict #2 | KEEP |
| PASS-3.md:418 | Consumer acceptance carry | Receiver PASS-2 / Tranche F + Tranche I | resolves the §3 close gate | none | matches PASS-2.md:336-347 producer-side carry | KEEP |
| PASS-3.md:419 | Per-grammar declaration crates negative carry | Receiver PASS-2 / SYNTHESIS | matches HARDENING-CONSOLIDATED §3 row 6 | none | combined API + parser fixture coverage | KEEP |
| PASS-3.md:420 | Final crate names carry | Receiver SYNTHESIS / Tranche A | resolves Lock 7 naming | none | matches MASTER-PLAN §12 + Architecture §3.4 | KEEP |
| PASS-3.md:421 | Hardcoded grammar registry deletion carry | Receiver SYNTHESIS / Tranche I close gate | resolves CENSUS §2 violation | none | matches HARDENING-CONSOLIDATED §3 conflict #11 | KEEP |
| PASS-3.md:423 | Performance rows carry | Receiver SYNTHESIS / Tranche H/J; now framed as ratification insurance post-P3-1 | matches Lock 8 | none | the carry survives P3-1 because platform ratification (especially lightning-css local-M1) lives in MASTER-PLAN J | KEEP |
| PASS-3.md:424 | Incremental fallback gates carry | Receiver PASS-1 / Tranche I | matches §5 fallback-rate gates | none | matches HARDENING-CONSOLIDATED §4.36 | KEEP |
| PASS-3.md:425 | Per-grammar feeder rows carry | Receiver SYNTHESIS / Architecture per-X table | matches Lock 14 per-X-table mandate | none | feeds Architecture §12.1 | KEEP |
| PASS-3.md:462-475 | Unresolved punch-list (§10) | 12 rows × 4 columns | every row carries the triple | none | matches Lane-8 standard | KEEP |

Lane 8 verdict: **honoured**. KEEP 11 / REINVENT 0 / DISCARD 0.

## §12 Lane 9 — Greenfield Discipline (P3-4 closure-focused re-walk)

V3's single REINVENT (visitor cookbook routing at line 115) closed by P3-4.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-3.md:32, 449-451 | `ParseStream` DISCARD | rebrand pressure abrogated, not patched | matches Lock 1 + HARDENING-CONSOLIDATED §3 conflict #2 | none | abrogate-before-patch posture honoured | KEEP |
| PASS-3.md:38, 451-456 | rewrite-mode + Unicode + per-grammar declaration crates DISCARD | settled across §1 + §8 + §9 | matches Lock 14 + Lock 8 | none | matches HARDENING-CONSOLIDATED §3 row 6 | KEEP |
| PASS-3.md:84-92 | path crate names corrected | restart names are `path`, `path-core`, `path-ts`; legacy `bbnf-path` cited as legacy evidence only | resolves Lock 7 + V1 punch item 17 | none | matches Architecture §3.4 + MASTER-PLAN §12 | KEEP |
| PASS-3.md:94-101 | Registry deletion close gate | "deletion item, not deferral" | binds the deletion to a verifiable grep | none | challenge "treat as deferral" defeated by close-gate framing | KEEP |
| PASS-3.md:160 | `@error(recover = ...)` consolidation | folds rewrite-mode-adjacent surface into `@error` | matches HARDENING-CONSOLIDATED §4.9 | none | matches Architecture §8.1 input-normalization-deletion table | KEEP |
| PASS-3.md:162-179, 190 | `DocumentSnapshot` + LSP user-facing silence policy | architectural transposition for incremental parsing | resolves diagnostic-noise risk; matches HARDENING-CONSOLIDATED §4.36 | "may fall back to full parse when anchors fail" | the fallback-rate gate at §5 + LSP silence policy at line 190 catch the fallback risk; env-var hatch is dev-vs-user calibration | KEEP |
| PASS-3.md:115 | Visitor cohort + cookbook routing **(P3-4 closure)** | trailing sentence binds runtime `BBNF-VISIT*` emission to cookbook table-of-contents | resolves the V3 documentation-fold opportunity; binds cookbook + ledger together | none | the steelman "cookbook + ledger live in separate documents" defeated by the explicit code → chapter index commitment | KEEP |
| PASS-3.md:342 | yaml host-route cell tightening **(P3-3)** | "decomposed via `host::primitives` + `@host fn` chain in the metadata block per `restart/README.md:155`; no Rust per-grammar code emerges from the onboarding two surfaces" | the documentary-vs-enforceable critique closed; the cell carries enforcement intent on its face | none | the steelman "cell is documentary" defeated by the explicit no-Rust invariant | KEEP |

Greenfield-discipline tally per the user-stated five-clause mandate:

| Discipline clause | PASS-3 surface evidence | Verdict |
|---|---|---|
| No quick solutions | `ParseStream` rebrand abrogated; hardcoded grammar registry deletion is a close gate, not "we'll deprecate later" | honoured |
| No workarounds | `@error(recover = ...)` folds `@recover` rewrite-mode-adjacent surface into the canonical declarative directive | honoured |
| No legacy code uncontested | Every `crates/bbnf-path/...` citation explicitly framed as "legacy evidence only"; every `crates/analysis/...` and `crates/lsp/...` reference framed as "useful entry shape, not the final design" | honoured |
| No contrivance / overengineering / overcomplication | The `bbnf` aggregator's 8-children layout deliberately keeps `tape/` and `value/` under `runtime/` instead of duplicating sibling directories; the `path` / `path-core` / `path-ts` triplet justified by Rust-toolchain limitation, not gold-plating | honoured |
| Idiomatic gestalt; architectural transpositions for elegance/simplicity/performance | `DocumentSnapshot` + `ReparsePlan` is a named transposition; the visitor-cookbook-fold (P3-4) and the yaml-row tightening (P3-3) are calibration transpositions; the bench-row + W3 baseline column extensions (P3-1, P3-2) are calibration transpositions | honoured |

Lane 9 verdict: **honoured**. KEEP 8 / REINVENT 0 / DISCARD 0.

## §13 Tightened gate-rerun

The §4 nine-command checklist re-walked end-to-end:

| # | Command | Expected | Observed | Result |
|---|---|---|---|---|
| 1 | `rg -n "bbnf-path\|bbnf-test-fixtures\|path!" PASS-3.md` | only legacy archaeology + deletion gate | line 84 (legacy evidence framing) + line 470 (deletion archaeology rg gate) | PASS |
| 2 | `rg -n "fixtures/yaml" PASS-3.md` | only post-onboarding parity references | lines 320-325 (fixture separation), 342 (parity-phase manifest), 477 (architecture proof gate) | PASS |
| 3 | `rg -n "@recover" PASS-3.md` | only as legacy alias | lines 35, 160 (legacy alias only during migration) | PASS |
| 4 | `rg -n "twitter\|canada\|citm\|bootstrap\|animate" PASS-3.md` | dataset rows + bench rows with attribution | lines 185-186 (incremental corpus), 379-382 (dataset baselines), 391-396 (bench rows with inline competitor + platform) | PASS |
| 5 | `rg -n "receiver\|blocker\|receiving gate" PASS-3.md` | every carry triple-complete | lines 113, 371, 429, 472, 475 (verbatim "receiver/blocker/receiving-gate" framings); §8 hand-off table at 411-426 + §10 punch-list at 462-475 carry the triple | PASS |
| 6 | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" PASS-3.md` | onboarding two-surface mandate | lines 320 (canonical citation), 322 (onboarding phase), 342 (per-X feeder yaml row), 344 (onboarding boundary) | PASS |
| 7 | `rg -n "generated_loc\|regen_wall\|xtask" PASS-3.md` | regen budget + W3 baseline cross-reference | line 103 (xtask-emitted descriptors), 361 (regen help in BBNF-POINTER003), 401 (W3 baseline preamble citing PASS-2 generated_loc), 405 (visitor row scales with PASS-2 generated_loc), 406, 411 (regen wall budget) | PASS |
| 8 | `rg -n "BBNF-VISIT\|BBNF-LIFE\|BBNF-LAYOUT\|BBNF-POINTER\|HostSignature" PASS-3.md` | ledger carries 16 codes | line 115 (cookbook routing), 352-365 (lifetime/layout/pointer/visit/lookbehind codes), 366 (HostSignature) | PASS |
| 9 | `rg -n "pointer!\|select!" PASS-3.md` | macros honoured; no `path!` reintroduction | 14 hits across §1, §3, §6, §6b, §8, §10; zero `path!` hits | PASS |

All nine gates resolve to expected post-amendment shape. No residue.

## §14 Punch list

Empty. The four V3 punch items closed by Wave 4.1 commit `11806d5d`. No new items surface from the V4 closure-focused re-audit.

The simdjson sustained throughput row absent from PASS-3 §7 (noted in V3 §6 line 162 and re-noted at V4 §7 lane 4) is preserved as informational scope-decision context, not a new punch item; sustained throughput is MASTER-PLAN J/K territory per the §10 carry at PASS-3.md:468.

## §15 Final readiness

> **Decision: READY.**
>
> PASS-3 V4 returns nine-lane all-honoured across an independent closure-focused walk after Wave 4.1 narrow amendment landed the four V3 punch items at commit `11806d5d`. KEEP fraction 100% (74/74 rows) with the V3 challenge surface preserved through per-row Pro/Con/Explication/Challenge rows; every KEEP defeats its steelman. Lanes 4 (SOTA-anchoring) and 6 (Generated-Code-Budget) advance from V3 violated-with-recommendation to V4 honoured: bench-row competitor + platform attribution now inlines per row at PASS-3.md:391-396; W3 baseline LOC anchors now inline at PASS-3.md:401-413 with PASS-2 §6 cross-reference. Lanes 3 (cohesion) and 9 (greenfield) close their non-blocking REINVENTs: yaml-row host-route cell binds `host::primitives` + `@host fn` chain (P3-3); visitor cookbook routing binds runtime `BBNF-VISIT*` emission to the cookbook table-of-contents (P3-4). Lane 7 grows from 13 to 16 diagnostic codes covering 12 of 12 friction surfaces.
>
> Hereupon PASS-3 advances to per-tranche full-spec drafting without further hardening. The Wave 4.1 amendment was calibration of existing tables and cells, not architectural surgery; no new architectural risk surfaces. The §10 carry to SYNTHESIS H/J remains as platform-ratification insurance (especially the lightning-css local-M1 disclaim per `restart/README.md:336`).

## §16 Provenance and methodology

Methodology per HARDENING.md §"Per-Item Discipline" + §"Methodology": Pro/Con/Explication/Challenge per row; KEEP requires defeating its steelman; REINVENT requires named redesign surviving its steelman; DISCARD requires named replacement surviving its steelman. Voice per `restart/README.md` §13 (calibrated, archaic-permissive, no metalanguage, path:line citations, tables liberal). Independent V4 audit; V3 read for the §12 punch list closure verification only; Wave 4.1 commit `11806d5d` independently inspected via `git show --stat` and per-line verification commands of §3 above.

## §17 V3 → V4 verdict-class delta summary

| Lane | V3 KEEP | V3 REINVENT | V4 KEEP | V4 REINVENT | Net delta |
|---|---:|---:|---:|---:|---|
| 1 Lock-Adherence | 12 | 0 | 12 | 0 | identical |
| 3 Cohesion | 8 | 1 | 9 | 0 | P3-3 closure |
| 4 SOTA-Anchoring | 5 | 1 | 7 | 0 | P3-1 closure (+1 preamble row) |
| 5 Grammar-Authoritative | 7 | 0 | 7 | 0 | identical |
| 6 Generated-Code-Budget | 7 | 1 | 8 | 0 | P3-2 closure |
| 7 Friction-Forecast | 9 | 0 | 12 | 0 | +3 BBNF-VISIT* rows |
| 8 Carry-Deferral | 11 | 0 | 11 | 0 | identical |
| 9 Greenfield-Discipline | 7 | 1 | 8 | 0 | P3-4 closure |
| **Totals** | **66** | **4** | **74** | **0** | **all V3 REINVENTs closed; +8 KEEP** |

V4 KEEP fraction: 100%. V3 REINVENT residue: 0. The decision class advances from AMENDMENT-REQUIRED to READY.

## §18 Closing posture

PASS-3 is architecturally sound, materially complete, and substantively ready. The Wave 4.1 narrow amendment closed all four V3 punch items at commit `11806d5d` through 55 lines of PASS-3.md surgical edits — calibration of existing tables and cells without architectural change. Per-tranche full-spec drafting begins with PASS-3 advanced.
