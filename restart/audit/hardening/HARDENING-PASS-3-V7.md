# HARDENING-PASS-3-V7 — fold-verification audit

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` |
| Audit label | PASS-3 (V7 fold verification) |
| V6 baseline | `restart/audit/hardening/HARDENING-PASS-3-V6.md` (READY) |
| Fold cohort | Phase 7.1 lock + ARCH amendments + Phase 7.2 PASS-3 surface fold |
| Audited PASS-3 commit | `d9414a2f` (Phase 7.2 fold) |
| Fold classification cited | `7cd09ea8` (Phase 7.2 classification) |
| Lock manifest cited | `9cb92284` (Phase 7.1 — Locks 5/7/10/11/12 amendments) |
| V1 fold candidates synthesis | `652f86bb` (`restart/research/V1-FOLD-CANDIDATES.md`) |
| Report path | `restart/audit/hardening/HARDENING-PASS-3-V7.md` |
| Lines | this file |
| Verdict | **READY** |
| Write scope | this report only |
| Source surfaces edited | none |

V7 is a fold-verification audit, not a re-litigation of V6. The question put to PASS-3 is narrow: did the Phase 7 fold (~20 mechanical sites + 5 authorial inserts + 1 archived crate-tree subsection) preserve V6 READY without introducing positive surface for retired forms or pseudo-precision around deferred V2 surfaces?

PASS-3 carries the largest rename ledger of the four V7 targets per `restart/research/V1-FOLD-CANDIDATES.md` §3 Tier 2 #11 ("`pointer!` → `path!` rename — ~58 corpus sites + 3 diagnostic codes"); ~22 of those sites land on PASS-3 alone. The fold therefore receives proportional verification weight: Step A's ten verification commands + Step B's 23-row 9-lane audit + §6.1's 12-item routing absorption ledger collectively bind every classification-table item from `phase-7.2-classification.md` to its post-fold residence in `PASS-3.md`.

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence (post-amendment Locks 5/7/10/11/12) | READY | 5 | 0 | 0 | none |
| 2 Sequencing | N/A (PASS-level synthesis) | 0 | 0 | 0 | none |
| 3 Cohesion | READY | 4 | 0 | 0 | none |
| 4 SOTA-Anchoring | READY | 3 | 0 | 0 | none |
| 5 Grammar-Authoritative — `path!`/`select!` validate via metadata | READY | 3 | 0 | 0 | none |
| 6 Generated-Code-Budget | READY | 2 | 0 | 0 | none |
| 7 Friction-Forecast (`BBNF-PATH-*` + `format()` + `BBNF-LOCAL-EQUALITY-ANNOTATION`) | READY | 4 | 0 | 0 | none |
| 8 Carry/Deferral (`path-ts` post-V1 + WASM V2 defer) | READY | 4 | 0 | 0 | none |
| 9 Greenfield-Discipline | READY | 3 | 0 | 0 | none |

**Final decision: ready to advance.** The Phase 7 fold landed verbatim per the Phase 7.2 classification ledger. No PASS-3-local surgery is required; no retired surface re-entered as positive authority; the V2-deferred WASM surface and the post-V1-deferred `path-ts` crate are receiver-gated rather than sketched.

The READY verdict in V7 is not equivalent to the READY verdict in V6. V6 READY meant "research fold absorbed without target-local amendment". V7 READY means "Phase 7.1 lock amendments + Phase 7.2 PASS-3 surface fold absorbed without target-local amendment". The two are independent verdicts on independent fold cohorts, and PASS-3 satisfies both.

## §3 Phase 7 fold verification — Step A evidence

The dispatch table is reproduced with observed evidence and per-row verdict.

| # | Item | Verification command | Observed | Expected | Verdict |
|---|---|---|---|---|---|
| 1 | `pointer!` retired | `rg -n 'pointer!' restart/audit/pass-3-runtime/PASS-3.md` | 2 hits at `:16` + `:92`, both in deletion-archaeology phrasing ("the `pointer!` spelling retires") | ≤2 deletion-archaeology refs only | PASS |
| 2 | `path!` canonical | `rg -nc 'path!' restart/audit/pass-3-runtime/PASS-3.md` | 22 positive hits (verdict ledger, §2 worked path, §3 commitment, §6 query directory, §7 bench rows, §8 hand-offs, §9 KEEP, §10 punch-list) | 20+ positive | PASS |
| 3 | `BBNF-POINTER` retired | `rg -n 'BBNF-POINTER' restart/audit/pass-3-runtime/PASS-3.md` | 0 hits | 0 | PASS |
| 4 | `BBNF-PATH` codes | `rg -n 'BBNF-PATH' restart/audit/pass-3-runtime/PASS-3.md` | 7 hits (`:16` recital + `:122-125` worked-failure prose + `:455-457` §6b verbatim rows) | positive; verbatim help-text rewritten | PASS |
| 5 | Public `format()` | `rg -nC2 'fn format\|format\(\)' restart/audit/pass-3-runtime/PASS-3.md` | `fn format(&self) -> String` at `:77` inside `DocumentView<'input>`; `format()` semantic paragraph at `:81`; consumer-gate row at `:143` ("`DocumentView::format()` walks `LayoutFacts` against tape identity") | positive on `DocumentView` | PASS |
| 6 | Closure runtime contract | `rg -nC2 'closure.*&.i\|stack-allocated.*frame\|no heap' restart/audit/pass-3-runtime/PASS-3.md` | "Closure environment" paragraph at `:191`: capture by `&'i Tape<'i>` only; "stack-allocated, never heap-escaping"; four committed sites named; broadening defers V2 | positive | PASS |
| 7 | `parse-that-regex` anchor | `rg -nC2 'parse-that-regex' restart/audit/pass-3-runtime/PASS-3.md` | recital line `:16` ("deferred to `parse-that-regex` (the regex sub-crate of `parse-that`)") | positive | PASS |
| 8 | `regex-automata` retired | `rg -n 'regex-automata' restart/audit/pass-3-runtime/PASS-3.md` | 0 hits | 0 | PASS |
| 9 | `path-ts` deferred | `rg -nC2 'path-ts' restart/audit/pass-3-runtime/PASS-3.md` | `:87` (legacy archaeology citation), `:93` ("`path-ts` defers post-V1 ... when `TsBackend: Backend` lands"), `:136` (deletion-gate scan exemption), `:387-396` (crate-tree archive subsection labelled "deferred post-V1; TS-native parse+runtime fork"), `:528` + `:575` (hand-off rows reframed) | positive | PASS |
| 10 | WASM deferred | `rg -nC2 'WASM.*defer\|H\.W3.*defer' restart/audit/pass-3-runtime/PASS-3.md` | `:466` BBNF-HOST003 reframed ("WASM lower-and-bench programme defers post-V1 alongside the V2 `WasmBackend: Backend` impl per Lock 5 amendment"); `:474` "WASM host primitive route (V2 deferred)" paragraph; `:525` cross-pass row "WASM host primitive ABI descriptor (V2 deferred)" | positive | PASS |

Step A returns ten PASS rows. Every classification-table item from `phase-7.2-classification.md` (A1-G1) finds verbatim absorption at the expected sites. The surgery routing in `phase-7.2-classification.md` §"Surgery routing" lines 1-12 lands without fault — see §6.1 for the per-item ledger.

Three classification-row notes:

- **A1 (`pointer!` → `path!`, ~22 sites).** The classification placed the rename at "22 hits across §1 verdict ledger, §2 user runtime, §3 path/select worked path, §3 consumer acceptance gates, §6 query directory annotation, §6.fixture separation phase 1, §6b BBNF-POINTER001/002/003 verbatim strings, §7 bench rows, §8 cross-pass hand-offs, §9 KEEP/REINVENT/DISCARD, §10 unresolved punch-list". V7 confirms 22 positive `path!` hits + 0 surviving positive `pointer!` (only 2 deletion-archaeology refs). The rename axis is mechanical and complete.
- **B1 (`format()` public method).** The classification specified "Add a `fn format(&self) -> String` row to the §2 `DocumentView<'input>` trait sketch; bind in §3 visitor commitments; add §3 cookbook hand-off". V7 confirms the trait method at `:77`, the semantic paragraph at `:81` (with the `LayoutFacts` binding + Lock 12 archive citation), and the consumer-gate row at `:143` ("`DocumentView::format()` walks `LayoutFacts` against tape identity"). The receiving gate is the `bbnf/self_host/internal` bench row at `:500` ("≤ 100 ms full self-parse + format roundtrip").
- **D1 (closure environment).** The classification specified "capture by `&'i Tape<'i>` reference only; environment frames are stack-allocated, not heap; the four committed sites lower to fixed BIR variants; no runtime function-pointer table; broader function-values amend through Lock 1 reuse-map semantics". V7 confirms verbatim absorption at `:191`. The four BIR variants (`HostChain`, `ValueProject`, predicate-bool, `ErrorRecover`) align with PASS-2 fold's BIR-alphabet binding at commit `3dc95460`.

Corollary scans:

| Corollary | Command | Observed |
|---|---|---|
| Negative-surface rollup | `rg -nc 'pointer!\|BBNF-POINTER\|regex-automata\|bbnf-regex\|@pratt\|@simd' PASS-3.md` | 2 (both `pointer!` deletion-archaeology only) |
| `Backend` trait integration (post-Lock-5 amendment) | `rg -n 'RustBackend\|TsBackend\|WasmBackend\|Backend: Backend' PASS-3.md` | 5 hits at `:93`, `:387`, `:466`, `:474`, `:525` — every TS/WASM mention routes through the formal `Backend` trait surface |
| Six-directive recital coverage | `rg -n '@import\|@host fn\|@error\|@layout\|@pretty\|@token' PASS-3.md` | 11 hits across §0 recital `:16`, §2 `format()` paragraph `:81`, §3 consumer-gate `:143`, §5 recovery `:195`/`:229`/`:244`, §6a yaml row `:438`, §6b `:450`/`:451`/`:464`, §5 WASM-route `:474`, §9 KEEP set `:544` — every directive surface receives at least one positive citation |
| Phase 7.2 surgery routing items 1-12 | per-row absorption | items 1-12 in `phase-7.2-classification.md:18-33` map verbatim to PASS-3 sites; manual cross-walk performed against rows A1-G1 |
| `BBNF-LOCAL-EQUALITY-ANNOTATION` reservation | `rg -n 'BBNF-LOCAL-EQUALITY-ANNOTATION' PASS-3.md` | 1 hit at `:468` — reserved row in §6b diagnostic ledger; "no V1 emission"; ARCH §8.2 receiver named |
| `gorgeous` archive citation (Lock 12 binding) | `rg -n 'gorgeous\|Lock 12' PASS-3.md` | 1 hit at `:81` ("legacy `gorgeous` engine is archived per Lock 12") — single binding, no drift |

## §4 Compressed 9-lane audit — Step B per-item table

Pro/Con/Explication/Challenge per row. Lane 1 covers post-Phase-7.1 Locks 5/7/10/11/12. Lane 5 covers `path!`/`select!` validation via generated metadata. Lane 7 covers `BBNF-PATH-*` + `format()` diagnostics. Lane 8 covers `path-ts` + WASM defer ledger. Eighteen rows minimum; the table below carries 21.

| # | Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|---|
| 1 | `PASS-3.md:16` (Lock 7) | Settled-authority recital names `path!` + `select!` and "the `pointer!` spelling retires per audit #3" | Phase 7.1 ratified Lock 7 amendment ("path-core / path / path-ts crate set"); the recital binds the V7 surface to that lock. | Single sentence carries the rename, the directive list, the regex sub-crate anchor, and the diagnostic prefix all at once — auditable in one line. | Reading load is concentrated in one paragraph. | Could split into bullets, but the single-paragraph form preserves the settled-authority verb mood and survives the audit. | KEEP |
| 2 | `PASS-3.md:93` (Lock 7 + Lock 5) | `path-ts` defers post-V1; J.W3 ships `path-core` + `path` (Rust) only via `RustBackend: Backend` impl | Lock 7 amendment now reads "exists" (not "may exist") for `path-core`/`path`; Lock 5 amendment defers TS+WASM. PASS-3 binds the V1 cohort to the Rust-line. | `RustBackend: Backend` cite makes the deferral a trait-boundary fact, not a roadmap statement. Future `TsBackend` lands by impl, not re-architecture. | The crate-tree archive subsection at `:387` repeats the deferred shape; minor textual redundancy. | Steelman: V1 should ship `path-ts` to avoid TS users blocking on Rust. PASS-3 defeats it: TS-native parse+runtime is a principled architectural fork, not a binding shim, and J.W3 is the receiver gate when `TsBackend` is implementable. | KEEP |
| 3 | `PASS-3.md:191` (Lock 4 amendment) | Closure environment frames stack-allocated; `&'i Tape<'i>` capture only; four committed sites named | Lock 4 post-Phase-7.1 says "closures capture by `&'i` reference only; capture-by-move is forbidden in V1". PASS-3 names the four sites (host-chain, map, predicate, recovery) and commits no runtime function-pointer table. | The lifetime parameter `'i` ties capture identity to tape identity — load-bearing for Lock 1 (tape is the substrate) under function-value broadening. | Broadening (first-class storage, return-from-rule) is a V2 amendment surface, which means a V1 user authoring richer closures hits a hard wall. | Steelman: the four-site narrowness will block a credible BBNF user. PASS-3 defeats it by stating the broadening contract explicitly defers to a Lock 1 reuse-map amendment, so V2 users see the same identity invariants V1 codifies. | KEEP |
| 4 | `PASS-3.md:474` (Lock 5 amendment + Lock 8 amendment) | WASM lower-and-bench programme defers post-V1 alongside V2 `WasmBackend: Backend`; H.W3/J.W3 measurement rows defer to V2 | Lock 5: "TS and WASM backends defer post-V1; V1 ships the Rust impl only". Lock 8: "V1 SOTA close gates measure the Rust-line only at H.W3, H.W4, and H.W5; no measurement-pending WASM anchor lands in V1". PASS-3 records "no `{N}` or `{M}` latency/size numbers in V1". | Eliminates the V6 R6 residual at the source: there is no `{N}`/`{M}` placeholder that needs to be measured-but-isn't; the row simply isn't present. Pseudo-precision risk drops to zero. | A V1 reader losing the comparative WASM lightning-css cite. | Steelman: keeping the measurement row with TBD preserves continuity for the post-V1 transition. PASS-3 defeats it: TBD without measurement is exactly the pseudo-precision the F lens flags; deletion is honest. | KEEP |
| 5 | `PASS-3.md:16` + `:544` (Lock 10 amendment) | Six-directive recital — `@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token` — replaces V6-era three-extension list | Post-Phase-7.1 Lock 10 binds `Directive = ImportDecl \| HostFn \| ErrorDecl \| LayoutDecl \| PrettyDecl \| TokenDecl`. PASS-3's two recital sites carry the verbatim six-form. | Carries the verbatim `@pretty` strategy vocabulary (`compact`, `group`, `indent`, `hardbreak`, `sep`, `block`) into §2 `format()` paragraph at `:81` so user surface and grammar surface stay coherent. | Two recital sites mean two future-amendment touchpoints. | Steelman: collapse to one sentence. PASS-3's split is intentional — `:16` is settled-authority, `:544` is KEEP/REINVENT/DISCARD. Both anchors are load-bearing for distinct readers. | KEEP |
| 6 | `PASS-3.md:77`-`:81` | `fn format(&self) -> String` lands on `DocumentView<'input>`; `@layout`-driven; legacy `gorgeous` archived per Lock 12 | V1-FOLD candidate F4 (HIGH greenfield value); user-mandate audit said `@pretty` is grammar-side and `format()` is runtime entry, both land. PASS-3 binds them at the trait. The `format()` paragraph at `:81` says "carries no separate engine" — closes the V6 R2 README ambiguity at the runtime contract. | Adds a method to the public trait — every V1 grammar runtime emits it. | None of the §6a per-grammar feeder rows mention `format` in a column; the binding is implicit in the consumer-gate row at `:143`. | Steelman: emit-time vs runtime-walk overhead. PASS-3 defeats it by binding to `LayoutFacts` (already PASS-1/PASS-2 produced) — `format()` is a tape walk against existing facts, not a re-parse. | KEEP |
| 7 | `PASS-3.md:466` (Friction-forecast — F lens) | `BBNF-HOST003` reframed: "the Rust backend continues to compile; the WASM lower-and-bench programme defers post-V1 alongside the V2 `WasmBackend: Backend` impl per Lock 5 amendment" | The original V6 `WasmHost` row hedged ("can deliver"); the V7 row is direct: it tells the user what compiles, what doesn't, and where the broadening lives. The verbatim help text fits the friction-forecast lane standard (verbatim error message + cookbook receiver named). | Long help-text string. | The cookbook receiver for V2 is "WASM ABI cookbook (V2)" — a forward reference. | Steelman: name a current cookbook receiver. PASS-3 defeats it: there is no current WASM cookbook because there is no current WASM ABI; reframing to V2 is the honest receiver. | KEEP |
| 8 | `PASS-3.md:455`-`:457` (Lane 7) | `BBNF-PATH001/002/003` verbatim rows replace `BBNF-POINTER001/002/003`; help-text rewritten ("unknown path segment", "path grammar inference failed", "terminal type for path") | The Phase 7.2 classification A2 row lists the three rename obligations + the help-line `pointer!(Json => "/...")` → `path!(Json => "/...")` — every obligation lands. Cookbook receivers reframed: §validation, §explicit-grammar, §regen — all under a Path cookbook (not Pointer). | Three-row rename means three cookbook page renames (also covered by `cookbook` route). | Could leave POINTER as alias for migration. | Steelman: the alias would soften migration. PASS-3 defeats it: greenfield discipline (Lock §V3 — "the auditor is its first adversary"); a public alias for a retired form re-introduces the very confusion the rename closes. | KEEP |
| 9 | `PASS-3.md:468` (Lane 7) | `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved row — "no V1 emission"; ARCH §8.2 GADT V2 amendment surface | Closes the audit-1 + Lock 4 amendment promise that GADT machinery is hidden V1 substrate (CSP solver + branch-local equality plumbing) but the annotation surface defers to V2. The reserved row puts the deferral on the diagnostic ledger so a future implementer cannot accidentally re-claim the code. | Reserved rows are a leaf-only mechanism; they don't compose with active codes. | A reader could misread "reserved" as "future". | Steelman: just don't record the code. PASS-3 defeats it: reservation closes the namespace and prevents aliasing — that is exactly the synthesis-ledger discipline. | KEEP |
| 10 | `PASS-3.md:91` (Lane 5) | `path!(Json, ["a", "b", 0])` and `select!(Css, "...")` consume the same `path-schema.toml` sidecar | Compile-time validation against grammar metadata is shared across both macros — single source of truth. The `vec!`/`Vec` analogy at `:92` is the audit-3 ratified naming reasoning. | None for grammar authority. | One compile-time validator must accommodate two surface syntaxes (array form vs structural-query form). | Steelman: separate validators for separate surfaces. PASS-3 defeats it: `path-core` carries one parser, lowerer, validator, and runtime plan family; the macros are surface adapters over one validation core. | KEEP |
| 11 | `PASS-3.md:104`-`:125` (Lane 5) | Worked-path JSON example: compile-time success + three failure paths (`BBNF-PATH001` field-typo, `BBNF-PATH002` implicit-grammar, `BBNF-PATH003` stale schema) | Friction-forecast lane standard demands verbatim error messages with user model + confusion point. PASS-3's three failure paths exercise each of the three rename codes against a single grammar. | Worked-path block is long. | None — the block is the lane-7 evidence. | Steelman: deferring worked-paths to cookbook would shrink PASS-3. PASS-3 defeats it: the Phase 7.2 classification A1 row binds the worked-path callsites in PASS-3 itself; the cookbook is downstream receiver. | KEEP |
| 12 | `PASS-3.md:127`-`:136` (Lane 5) | Hardcoded grammar-marker registry deletion close-gate; `path-ts` archive exempted from V1 deletion-gate scan | The deletion-gate `rg` command at `:130` enumerates the generic crates that must return zero hits. The exemption at `:136` reads "`crates/path-ts/` tree is post-V1 deferred per Lock 7 amendment; it does not contribute to the V1 deletion-gate scan" — bright-line. | Adds a Phase-7-specific exemption to a settled deletion gate. | An overzealous reader could miss that the deletion-gate path-list still applies to active V1 crates. | Steelman: include `path-ts` in the gate. PASS-3 defeats it: scanning a post-V1 archived crate for V1-discipline violations is category-error; the exemption is correct. | KEEP |
| 13 | `PASS-3.md:387`-`:396` (Lane 8) | `crates/path-ts/` archived under "deferred post-V1 (TS-native parse+runtime fork)" subsection | Crate-tree at §6 keeps the deferred shape visible (`lib.rs`, `template_tag.rs`, `schema.rs`, `bindings.rs`) so synthesis carry rows have a concrete deferred subject. | None — the archive form is exactly what audit-5 §4 fold disposition required. | Subsection adds 10 lines to §6. | Steelman: drop the deferred tree shape entirely. PASS-3 defeats it: J.W3 carry-row at `:528` references the deferred surface; without the tree shape the carry row is unanchored. | KEEP |
| 14 | `PASS-3.md:496` (Lane 4) | Bench row `json/citm/path` (renamed from `json/citm/pointer`); surface-under-test column reads "`path!` object traversal" | Phase 7.2 §"Surgery routing" item 9 binds the bench row name + surface column rename. SOTA-anchoring lane standard preserved (sonic-rs 854 µs / simd-json 831 µs / M1 Pro). | None for SOTA discipline. | None. | Steelman: bench-row names are immaterial. PASS-3 defeats it: `json/citm/pointer` would silently map to a retired macro on a SOTA dashboard; the rename is operational, not cosmetic. | KEEP |
| 15 | `PASS-3.md:494`-`:502` (Lane 4) | Lock-8 attribution preserved on every throughput row; `bbnf/self_host/internal` row commits "non-Lock-8 internal gate"; non-throughput rows disclaim | Every row carries Competitor floor + Platform per Lock-8 amendment; the §10 carry insurance line at `:490` is preserved. | None. | The PASS-3 §9 disclaim re: platform ratification at the CSS rows could read as hedging. | Steelman: drop the disclaim. PASS-3 defeats it: lightning-css numbers come from a different platform-ratification context, and the disclaim is the honest provenance citation per V6 SOTA-anchoring lane. | KEEP |
| 16 | `PASS-3.md:506`-`:514` (Lane 6) | W3 baseline LOC anchors + +2 percent regen ceiling per surface | Generated-code-budget lane standard satisfied: per-surface W3 baseline + per-regen delta gate + named amendment path for larger deltas. The baselines are inherited from PASS-2.md §6, not invented. | None — the +2 percent gate is the canonical regen ceiling. | Provisional yaml ≤ 4,000 LOC is a forecast; could become measured. | Steelman: defer the yaml row until measured. PASS-3 defeats it: a forecast row is what the +2 percent gate measures against, and naming the forecast is what makes the gate verifiable. | KEEP |
| 17 | `PASS-3.md:518`-`:534` (Lane 8) | Cross-pass hand-off table — every row carries Receiver + Blocker + Receiving gate | Carry-deferral lane standard satisfied. The `Final crate names (V1)` row at `:528` reads "`path-ts` defers post-V1 alongside the TS-native parse+runtime fork per Lock 7 amendment" — receiver SYNTHESIS / Tranche A; blocker named; receiving gate is workspace crate-name check. | None. | Two rows reference WASM (`:525` ABI descriptor, `:526` SIMD/DFA verifier with "PASS-2 / Tranche F + H.W3" receiver) — could read as duplication. | Steelman: collapse the WASM rows. PASS-3 defeats it: `:525` is host-primitive ABI defer; `:526` is scanner-verifier contract. Different obligations, different receivers — keeping them split is correct carry discipline. | KEEP |
| 18 | `PASS-3.md:570`-`:583` (Lane 8) | Unresolved punch-list rows preserve receiver-gated deferrals for `path-ts` per Lock 7 amendment | Punch-list row at `:575` reads "`path-ts` defers post-V1 per Lock 7 amendment" + receiver SYNTHESIS / Tranche A + blocker "Prefixed names re-leak into greenfield" + receiving gate `rg -n 'bbnf-path\|bbnf-test-fixtures' restart/` returns zero. | None. | Punch-list is long (12 rows). | Steelman: prune to PASS-3-only. PASS-3 defeats it: cross-pass receiver visibility is exactly what unresolved-punch-list serves; pruning would hide carry to SYNTHESIS. | KEEP |
| 19 | `PASS-3.md:543` (Lane 9 + Lane 1) | KEEP/REINVENT/DISCARD: `path!`, `select!`, explicit and implicit path forms — under KEEP | The post-Phase-7.2 KEEP set lists `path!` directly. The DISCARD set at `:559` keeps `Public ParseStream name` (no relapse). Greenfield discipline maintained: no public retired surface. | None. | The DISCARD list still mentions `Rewrite-mode`, `Grammar-level Unicode-class algebra`, `Per-grammar declaration crates by default` — readers might interpret as drift; in fact these are negative invariants that audit-discipline requires the document to preserve. | Steelman: remove the DISCARD list to shorten. PASS-3 defeats it: greenfield discipline requires the negative-invariant catalogue to remain visible — that is what makes a fold landing verifiable. | KEEP |
| 20 | `PASS-3.md:191` (Lane 9 — Greenfield F lens) | "no runtime function-pointer table" + "monomorphised at its lambda's allocation site" | Greenfield discipline: the closure form does not introduce a new public substrate (no `Box<dyn Fn>` table); each closure is statically dispatched per the four committed sites. | Statically dispatched closures are a Rust-idiomatic pattern; no architectural cost. | The four-site narrowness pushes broadening to V2. | Steelman: heap-allocate a Fn table for ergonomics. PASS-3 defeats it: heap allocation breaks Lock 1 (no parallel substrate, no orthogonal codepath); the V1 narrowness preserves substrate identity. | KEEP |
| 21 | `PASS-3.md:81` (Lane 9 + Lane 1) | "legacy `gorgeous` engine is archived per Lock 12 and is not the runtime substrate; `format()` carries no separate engine" | Lock 12 amendment says "ser + gorgeous archive at `archive/<crate>/`" + "`pre-restart-2026-05-04` source-of-truth tag" + "legacy `BA-`/`BB-`/`BC-`/`BD-` slot drift retires under canonical `A-`/`B-`/`C-`/`D-` tranche letters". The PASS-3 sentence binds `format()` to the lock at the runtime contract level. | None. | None — Lock 12 is settled. | Steelman: parallel format engine for performance. PASS-3 defeats it: Lock 1 (no parallel substrate, no orthogonal codepath) + Lock 12 (gorgeous archived) — a parallel format engine would violate two settled locks. | KEEP |
| 22 | `PASS-3.md:438` (Lane 5 — yaml onboarding) | yaml row: "decomposed via `host::primitives` + `@host fn` chain in the metadata block per `restart/README.md:155`; no Rust per-grammar code emerges from the onboarding two surfaces" | Phase 7 fold did not touch yaml onboarding; the row preserves the two-surface invariant. The `@host fn` reference is now part of the six-directive Lock 10 amendment (it is settled grammar surface, not a roving extension point). | None — yaml onboarding is the load-bearing onboarding test for Lock 14. | None. | Steelman: per-grammar `crates/yaml/` declaration crate for ergonomic decomposition. PASS-3 defeats it: Amendment 01 rejects per-grammar declaration crates by default; the rare host-adapter escape-valve at `:577` exists but yaml does not invoke it. | KEEP |
| 23 | `PASS-3.md:528` + `:575` (Lane 8 — workspace naming) | "Final crate names (V1): `path`, `path-core`, and `test-fixtures`. `path-ts` defers post-V1 alongside the TS-native parse+runtime fork per Lock 7 amendment." | Lock 7 amendment names `path-core`/`path` as existing at V1 and `path-ts` as deferred post-V1. Both punch-list and hand-off rows reference the lock by amendment, not by narrative. The receiving gate for the hand-off row is `rg -n 'bbnf-path\|bbnf-test-fixtures' restart/` returns zero — bright-line. | None. | None — the deferral is principled, not pragmatic, per audit-5. | Steelman: ship `bbnf-path-ts` legacy as compat. PASS-3 defeats it: greenfield discipline (Lock 7 amendment + Lock 11 incubation policy) precludes legacy package names in V1 workspace. | KEEP |

Per-row count: 23 rows, all KEEP, all with steelmanned challenges defeated. Per `restart/prompts/HARDENING.md:46`, a healthy target shows mixed verdicts (60-80% KEEP fraction); here, every row is KEEP because V7 is a fold-verification audit on a baseline that already underwent V1-V6 reinvention — the steelmen are exactly the legitimate counterpositions a fold-landing must defeat, and the fold defeats them.

Lane-specific notes:

- **Lane 1 (Lock-Adherence, 5 rows: 1, 2, 3, 4, 5)**. Every Phase-7.1 amendment finds verbatim absorption: Lock 5 (rows 2 + 4 — TS+WASM defer + V1 Rust impl only via formal `Backend` trait); Lock 7 (rows 1 + 2 — `path-core`/`path` exists; `path-ts` defers post-V1); Lock 8 (row 4 — V1 SOTA Rust-line only; no measurement-pending WASM anchor); Lock 10 (row 5 — six-directive list); Lock 12 (row 21 — `gorgeous` archive). Lock 4 closure-by-`&'i` (row 3) sits at the intersection of Lane 1 and Lane 9 (greenfield discipline) because the constraint is both lock-adherence and substrate-identity preservation.
- **Lane 2 (Sequencing, N/A)**. PASS-3 is a pass synthesis, not a multi-wave tranche; the lane standard at `restart/prompts/HARDENING.md:56` exempts pass targets. Receiver-discipline check at row 17 (cross-pass hand-off table) and row 18 (unresolved punch-list) covers the sequencing-adjacent obligation: every cross-pass row carries Receiver + Blocker + Receiving gate.
- **Lane 3 (Cohesion, 4 rows: 6, 10, 11, 13)**. The fold deepened cohesion by binding `format()` to `@layout`-driven `LayoutFacts` (row 6), unifying `path!` + `select!` validation through one `path-schema.toml` sidecar (row 10), exercising three `BBNF-PATH-*` failure codes on a single grammar (row 11), and archiving `path-ts` as a deferred-post-V1 subsection rather than a live-but-empty crate (row 13). No row introduces a rival authority.
- **Lane 4 (SOTA-Anchoring, 3 rows: 14, 15, 16)**. Bench-row rename at row 14 is operational (SOTA dashboards key off row name); Lock-8 attribution preserved at row 15 (every throughput row carries Competitor floor + Platform); generated-code budget at row 16 anchors W3 baselines from PASS-2.md §6 with +2 percent regen ceiling. The lightning-css platform-ratification disclaim is honest provenance, not pseudo-precision.
- **Lane 5 (Grammar-Authoritative, 3 rows: 10, 11, 12)**. Generated metadata is the only validation surface for `path!` and `select!` per row 10. The deletion-gate `rg` command at PASS-3 `:130` enumerates the generic crates that must return zero hits (row 12). The `path-ts` exemption at row 12 is bright-line and lock-citation-attached.
- **Lane 6 (Generated-Code-Budget, 2 rows: 6, 16)**. `format()` adds one trait method, not a new generated artefact (row 6); W3 LOC anchors + +2 percent regen ceiling per surface (row 16) survive the fold without amendment.
- **Lane 7 (Friction-Forecast, 4 rows: 7, 8, 9, 11)**. Three `BBNF-PATH-*` rows at row 8 carry verbatim help-text rewritten from `BBNF-POINTER-*`. The `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved row at row 9 closes the GADT V2 amendment surface namespace. The `BBNF-HOST003` reframe at row 7 is direct (V2 deferral with cookbook receiver named). Worked-path failure-mode coverage at row 11.
- **Lane 8 (Carry/Deferral, 4 rows: 2, 13, 17, 18)**. `path-ts` defer at rows 2 + 13 (Lock 7 amendment with both narrative + crate-tree archive); WASM defer at row 17 (Lock 5 + Lock 8 amendment cohort); cross-pass hand-off + unresolved-punch-list discipline at rows 17 + 18.
- **Lane 9 (Greenfield-Discipline, 3 rows: 19, 20, 21)**. KEEP/REINVENT/DISCARD ledger preserves negative-invariant catalogue at row 19. No runtime function-pointer table for closures (row 20) — Lock 1 (no parallel substrate) honoured at the function-value boundary. Lock 12 archive citation at row 21 — no parallel format engine, no orthogonal codepath.

## §5 LLM-pathology lenses (F/G/H)

V7 re-applies the F/G/H regression scan over the post-fold PASS-3.

| Lens | V6 conclusion | V7 evidence | V7 result |
|---|---|---|---|
| F — hedged runtime invariant / pseudo-precision | CLEAN at V6 (`HARDENING-PASS-3-V6.md:325-328`) | Closure paragraph at `:191` is direct ("never heap-escaping", "no runtime function-pointer table"); WASM defer at `:474` deletes `{N}`/`{M}` placeholders rather than carrying them as TBD. | CLEAN. The fold removes pseudo-precision rather than introducing it. The R6 residual carried in `HARDENING-CONSOLIDATED-V6.md:178` is closed at the source. |
| F — closure bias around examples | CLEAN at V6 (`HARDENING-PASS-3-V6.md:326`) | Worked-path at `:104`-`:125` exercises three failure codes (`BBNF-PATH001/002/003`) on a single grammar; closure paragraph at `:191` names four committed sites by name. | CLEAN. Examples deepen rather than retreat. |
| G — overfitting / old `pointer!` shape | CLEAN at V6 (`HARDENING-PASS-3-V6.md:328`) | `pointer!` count drops to 2 hits — both deletion-archaeology phrasing. `path!` count climbs to 22 positive hits across §1, §2, §3, §6, §7, §8, §9, §10. | CLEAN. The rename is mechanical and complete. |
| G — yaml special casing | CLEAN at V6 (`HARDENING-PASS-3-V6.md:330`) | yaml row at `:438` carries the same two-surface form ("`yaml.bbnf` plus the metadata block"); `fixtures/yaml` is parity-phase only at `:419`. The fold did not touch yaml onboarding. | CLEAN. |
| G — `@pratt`/`@simd` drift | CLEAN at V6 (`HARDENING-PASS-3-V6.md:331`) | `BBNF-OPT001/002` rows at `:452`-`:453` survive verbatim; no positive `@pratt`/`@simd` directive. The Lock 10 directive list at `:16` reads "`@pratt`, `@simd`, `@transducer`, `@rewrite`, and `@unicode` retire" via the lock. | CLEAN. |
| H — wrong lock provenance | CLEAN at V6 (`HARDENING-PASS-3-V6.md:332`) | Every Lock cite under V7 names the post-Phase-7.1 amendment scope: Lock 5 amendment (TS+WASM defer), Lock 7 amendment (`path-core`/`path` exists; `path-ts` defers), Lock 8 amendment (V1 SOTA Rust-line only), Lock 10 amendment (six directives), Lock 12 amendment (`A.W0` not `BA.W0`). | CLEAN. |
| H — stale crate prefix | CLEAN at V6 (`HARDENING-PASS-3-V6.md:333`) | `bbnf-path-ts` at `:87` is legacy archaeology; current crate is `path-ts`. `bbnf-regex` does not appear; current name is `parse-that-regex`. | CLEAN. |
| H — lookbehind alias provenance | CLEAN at V6 (`HARDENING-PASS-3-V6.md:334`) | `BBNF1004 / BBNF-LOOKBEHIND-WIDTH / LookbehindWidth` row at `:462` survives verbatim; the post-fold prose at `:470` reads "PASS-3 emits the PASS-1-owned lookbehind numeric code `BBNF1004`, alphabetic alias `BBNF-LOOKBEHIND-WIDTH`, and vocabulary kind `LookbehindWidth` as one binding". | CLEAN. |
| H — `parse-that-regex` provenance | NEW at V7 | Recital `:16` cites "`parse-that-regex` (the regex sub-crate of `parse-that`)" — names parent crate + sub-crate role; aligns with Lock 11 amendment ("`parse-that` is the canonical name for the published parser combinator + regex family; the legacy `bbnf-regex` crate renames to `parse-that-regex`"). | CLEAN. |
| H — `Backend` trait integration | NEW at V7 | Five mentions of `RustBackend: Backend`, `TsBackend: Backend`, `WasmBackend: Backend` at `:93`, `:387`, `:466`, `:474`, `:525`. Every TS/WASM defer routes through the formal trait surface from Lock 5 amendment, not through narrative deferral. | CLEAN. The trait boundary is the load-bearing artefact for V2 add-on. |

Pathology summary: zero new defects; two new H-lens checks (parse-that-regex provenance + Backend trait integration) introduced by the fold both pass.

V7-specific F/G/H spot checks:

- **F lens — does the `format()` paragraph hedge?** Reading at `:81`: "The `format()` method is the public surface of the `@layout`-driven formatter. The engine itself is grammar-emitted: `@layout`, `@pretty`, and `@token` directives produce `LayoutFacts` consumed by per-grammar layout lowering; the public method walks tape identity against those facts and emits source. The legacy `gorgeous` engine is archived per Lock 12 and is not the runtime substrate; `format()` carries no separate engine." Direct, no hedging. "Carries no separate engine" is the load-bearing negative invariant.
- **F lens — does the closure paragraph hedge?** Reading at `:191`: "BBNF closures (`|x| body`) capture by `&'i Tape<'i>` reference only; closure environment frames are stack-allocated, never heap-escaping. The four committed closure sites — host-chain closure, map closure, predicate closure, recovery closure — each lower to a fixed BIR variant... None of the four sites materialises a runtime function-pointer table; each closure is monomorphised at its lambda's allocation site." Direct, technical, no hedging. "Never heap-escaping" + "no runtime function-pointer table" are bright-line negative invariants.
- **G lens — does the BBNF-PATH rename overfit the JSON worked-path?** The worked-path at `:104-122` exercises three distinct failure codes — `BBNF-PATH001` (field name typo), `BBNF-PATH002` (implicit grammar inference under two roots), `BBNF-PATH003` (stale schema). Each is a generic compile-time validation against generated metadata, not a JSON-specific check. The §6b verbatim rows at `:455-457` carry abstract field placeholders (`{segment}`, `{path_macro_input}`, `{path}`) — the row is grammar-agnostic.
- **H lens — Phase-7-derived locks have correct provenance?** Every Phase 7 amendment cite ("per Lock 5 amendment", "per Lock 7 amendment", "per Lock 8 amendment", "per Lock 10 amendment", "per Lock 12 amendment") sits in the locks file at the line range cited — verified manually against `restart/locks/14-LOCKS.md:34-56`.

## §6 Phase 7.2 surgery routing + cross-document binding (post-fold)

### §6.1 Surgery routing — per-item absorption ledger

The Phase 7.2 classification document at `restart/audit/pass-3-runtime/phase-7.2-classification.md` lines 18-33 enumerates 12 surgery-routing items. V7 verifies absorption per-item.

| Routing item | Classification anchor | PASS-3 absorption site(s) | Verbatim match | V7 verdict |
|---|---|---|---|---|
| 1. §0 settled-authority recital extended | classification §"Surgery routing" #1 | `:16` | "deferred to `parse-that-regex` (the regex sub-crate of `parse-that`)" + six-directive list | ABSORBED |
| 2. §1 verdict ledger Path/select DSL row renamed `path!` | #2 | `:33` | row reads "`path!` and `select!` validate against generated metadata" | ABSORBED |
| 3. §2 user runtime — `DocumentView<'input>` gains `fn format(&self) -> String` | #3 | `:73-78` trait sketch + `:81` semantic paragraph | trait body shows `fn format(&self) -> String;` | ABSORBED |
| 4. §3 path/select — macro names `path!` + `select!`; `path-ts` defer; worked-path rename | #4 | `:91-92`, `:93`, `:104-122` | `path!(Json, ["a", "b", 0])` + `select!(Json, "...")` + three `BBNF-PATH-*` failure paths | ABSORBED |
| 5. §4 tape/direct — closure environment paragraph appended | #5 | `:191` | "Closure environment" paragraph — capture by `&'i Tape<'i>`, stack-allocated, four committed sites named | ABSORBED |
| 6. §5 error recovery — WASM host primitive route reframed for V2 defer | #6 | `:474` | "WASM host primitive route (V2 deferred)" paragraph naming `WasmBackend: Backend` impl | ABSORBED |
| 7. §6 crate tree — `path-ts` archived under "deferred post-V1" subsection | #7 | `:387-396` | subsection header "deferred post-V1; TS-native parse+runtime fork" + tree shape | ABSORBED |
| 8. §6b diagnostic ledger — three `BBNF-POINTER*` → `BBNF-PATH*`; `BBNF-HOST003` reframed; `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved | #8 | `:455-457`, `:466`, `:468` | three rename rows verbatim + WasmHost reframe + reserved row at end of §6b | ABSORBED |
| 9. §7 benchmark — `json/citm/pointer` → `json/citm/path`; `pointer!` callsites → `path!` | #9 | `:483`, `:488`, `:496` | bench row name + `path!` callsites in surface-under-test column | ABSORBED |
| 10. §8 cross-pass hand-offs — `pointer!`/`select!` rename; WASM ABI carry retired from V1 | #10 | `:523`, `:525`, `:528` | rows reference `path!`/`select!`; WASM ABI row reads "(V2 deferred)" | ABSORBED |
| 11. §9 KEEP/REINVENT/DISCARD — `pointer!` → `path!`; "explicit and implicit pointer forms" → "explicit and implicit path forms" | #11 | `:543`, `:559` | KEEP row reads "`path!`, `select!`, explicit and implicit path forms"; DISCARD row keeps `Public ParseStream name` | ABSORBED |
| 12. §10 unresolved punch-list — `pointer!`/`select!` rename; `path-ts` reframes for V1 defer | #12 | `:574`, `:575` | rows reference `path!`/`select!` and "`path-ts` defers post-V1 per Lock 7 amendment" | ABSORBED |

Twelve of twelve routing items absorbed verbatim. The Phase 7.2 closing posture ("three corpus-wide renames + one new public method + one runtime-contract paragraph + one crate-tree archive + one V2-defer reframe; mechanical surgery on the rename axis; targeted authorial work on the format/closure/WASM axis") is satisfied at every site.

### §6.2 Cross-document binding ledger

| Binding | PASS-3 anchor | Cross-document anchor | V7 result |
|---|---|---|---|
| Lock 7 — `path-core`/`path` exists at V1; `path-ts` defers post-V1 | `:16`, `:87`, `:93`, `:136`, `:387`, `:528`, `:575` | `restart/locks/14-LOCKS.md:46` ("`crates/path-ts/` defers post-V1 alongside the TS-native parse+runtime fork") | CLOSED |
| Lock 5 — TS+WASM at H.W3/J.W3; V1 ships Rust impl only | `:93`, `:387`, `:466`, `:474`, `:525` | `restart/locks/14-LOCKS.md:42` ("V1 ships the Rust impl only via the formal `Backend` trait") | CLOSED |
| Lock 8 — V1 SOTA Rust-line only; no measurement-pending WASM anchor | `:474`, `:494`-`:502` | `restart/locks/14-LOCKS.md:48` ("V1 SOTA close gates measure the Rust-line only at H.W3, H.W4, and H.W5") | CLOSED |
| Lock 10 — six directives; function values + lambda + closure-by-`&'i` | `:16`, `:191`, `:544` | `restart/locks/14-LOCKS.md:52` ("`Directive = ImportDecl \| HostFn \| ErrorDecl \| LayoutDecl \| PrettyDecl \| TokenDecl`") | CLOSED |
| Lock 4 — DK13 higher-rank + GADT substrate hidden + closure capture by `&'i` only | `:191`, `:468` | `restart/locks/14-LOCKS.md:40` ("V1 type system folds higher-rank polymorphism via DK13...; closures capture by `&'i` reference only; capture-by-move is forbidden in V1") | CLOSED |
| Lock 11 — `parse-that-regex` is the regex sub-crate of `parse-that`; legacy `bbnf-regex` renames | `:16` | `restart/locks/14-LOCKS.md:54` ("`parse-that` is the canonical name... the legacy `bbnf-regex` crate renames to `parse-that-regex`") | CLOSED |
| Lock 12 — `gorgeous` archive at `archive/<crate>/`; `A.W0` slot canon | `:81` | `restart/locks/14-LOCKS.md:56` ("legacy `BA-`/`BB-`/`BC-`/`BD-` slot drift retires under the canonical `A-`/`B-`/`C-`/`D-` tranche letters") | CLOSED |
| Phase 7.2 surgery routing items 1-12 | per-row in classification ledger | `restart/audit/pass-3-runtime/phase-7.2-classification.md` lines 18-33 | CLOSED |
| V1-FOLD candidate F4 (`format()` public method) | `:77`, `:81`, `:143` | `restart/research/V1-FOLD-CANDIDATES.md` Tier 1 + §2 conflict #5 | CLOSED |
| V1-FOLD candidate G1 (`BBNF-LOCAL-EQUALITY-ANNOTATION` reserved) | `:468` | `restart/research/V1-FOLD-CANDIDATES.md` §3 Tier 1 #9 + §6 Lock 4 amendment | CLOSED |
| MASTER-PLAN D.W5 wave growth (function values + lambda + closure + match + tuple) | `:191` (closure subset) | `restart/MASTER-PLAN.md:170`, `:356` ("Function values + lambda literals + closure capture by `&'i` reference + match expression + tuple expression/pattern lowering") | CLOSED |

No row requires PASS-3 surgery. The fold preserves cross-document binding integrity.

## §7 Punch list + V6-to-V7 history

### §7.1 Punch list

PASS-3-blocking surgery:

| # | Path:line | Surgery | Acceptance gate | Origin lane | Status |
|---|---|---|---|---|---|
| — | none | none | V7 fold verification returns READY without amendment | Lanes 1-9 | NONE REQUIRED |

Residual non-PASS-3 items (V6 carry — informational):

| # | Path:line | Surgery | Receiver | V7 status |
|---|---|---|---|---|
| R1 | `restart/research/INDEX.md` | bibliography hygiene | research-index follow-up | unchanged from V6 |
| R2 | `restart/README.md` | wording precision (one owning identity, snapshot `TapeId`, HM/check-synth/CSP) | README polish | partial closure post-V6 R2 follow-up; not PASS-3 |
| R3 | `restart/audit/hardening/HARDENING-CONSOLIDATED.md` | rerun-checklist precision | consolidation hygiene | unchanged from V6 |
| R4 | `restart/locks/14-LOCKS.md:40` | Lock 4 egglog rationale hygiene | lock-rationale follow-up | partial closure post-V6 R4 follow-up; not PASS-3 |
| R5 | `restart/MASTER-PLAN.md` (C.W4/C.W5) | rewrite-budget impl tests | implementation tranche | unchanged; not PASS-3 |
| R6 | H.W3 WASM `{N}`/`{M}` placeholders | **CLOSED at PASS-3 by Phase 7.2 fold** (V1 SOTA Rust-line only per Lock 8 amendment) | V2 — `WasmBackend: Backend` impl | RESOLVED at PASS-3 |
| R7 | per-wave tranche specs | next drafting phase | tranche drafting | unchanged; not PASS-3 |

R6 is the V7 closure of one V6 residual: PASS-3 no longer carries `{N}`/`{M}` measurement-pending WASM placeholders because the V1 surface defers WASM lower-and-bench programme to V2 entirely. R1, R3, R5, R7 remain non-blocking and outside PASS-3 write scope.

### §7.2 V6-to-V7 history

| Cycle | PASS-3 posture |
|---|---|
| V6 | READY after research fold (Topics 1-8) absorbed materialisation/cache, snapshot `TapeId`, red-like views, yaml recovery, pointer/select diagnostics, DAP/debug identity, exact/prefilter scanner, type/value diagnostics. R6 residual flagged H.W3 WASM `{N}`/`{M}` placeholders. |
| Phase 7.1 (post-V6) | Lock manifest amendments at commit `9cb92284`: Lock 5 (TS+WASM defer to V2), Lock 7 (`path-core`/`path` exists; `path-ts` defers), Lock 8 (V1 Rust-line only), Lock 10 (six directives + function values + lambda + closure-by-`&'i`), Lock 11 (`parse-that-regex` canonical), Lock 12 (`A.W0` slot canon). |
| Phase 7.2 (PASS-3 fold) | At commit `d9414a2f`: `pointer!` → `path!` rename across 22+ sites; `BBNF-POINTER-*` → `BBNF-PATH-*` with verbatim help-text rewrite; public `format()` method on `DocumentView`; closure environment paragraph (stack-allocated, `&'i` capture, four committed sites); `parse-that-regex` recital anchor; `path-ts` archived under deferred-post-V1 subsection; WASM lower-and-bench programme defers to V2 (deletes `{N}`/`{M}`); `BBNF-LOCAL-EQUALITY-ANNOTATION` reserved row added. |
| V7 | READY. Phase 7 fold landed verbatim per the Phase 7.2 classification ledger. R6 V6-residual closed at the source. No new defects introduced. The 22 macro-name renames + 3 diagnostic-code renames + 1 trait method addition + 1 runtime-contract paragraph + 1 crate-tree archive + 1 V2-defer reframe + 1 reserved-code row collectively absorb without amendment. |

V7 does not relitigate V6. It re-tests V6 with the Phase 7 fold material absorbed. The result remains READY.

The PASS-3 V7 corpus-baseline reading is therefore: post-Phase-7.2 PASS-3 (commit `d9414a2f`) with the Phase 7.1 lock manifest (commit `9cb92284`) as the governing locks reading. Subsequent V7 cohort consolidation should bind PASS-1, PASS-2, and MASTER-PLAN against the same Phase 7.1 lock manifest. The four-target V7 cohort verdict will be visible only after all four hardener reports commit.

## §8 Final verdict + closing posture

### §8.1 Final verdict

> **Decision: ready.**
>
> PASS-3 absorbed the largest fold ledger across the four targets — 22+ macro-name renames, three diagnostic-code renames with verbatim help-text rewrite, one new public trait method (`format()`), one new runtime-contract paragraph (closure environment), one crate-tree archive subsection (`path-ts` deferred post-V1), one V2-defer reframe (WASM lower-and-bench programme), and one reserved-code row (`BBNF-LOCAL-EQUALITY-ANNOTATION`). Every item from the Phase 7.2 classification ledger lands at its expected site with the expected wording, no positive surface for retired forms re-enters, the V6 R6 H.W3 WASM `{N}`/`{M}` residual closes at the source, and the F/G/H regression scan returns zero new defects.
>
> Hereupon per-tranche full-spec drafting may proceed against the V7 PASS-3 surface; the fold is verified.

### §8.2 Closing posture

PASS-3 is fit for the post-V7 advance under READY. The runtime surface remains coherent: typed roots are the default; `ValueRef` is the shared cursor; tape identity is snapshot-scoped; `path!` and `select!` (the canonical V7 names) validate through generated metadata; `format()` lands as the public `@layout`-driven formatter entry on `DocumentView`; closures capture by `&'i` only with stack-allocated frames; `path-ts` defers post-V1 alongside the TS-native parse+runtime fork; the WASM lower-and-bench programme defers to V2 alongside the `WasmBackend: Backend` impl. No PASS-3-local surgery is required and no cross-document binding row asks PASS-3 to amend.

The consolidation worker (V7 cohort) should bind PASS-3 V7 READY against PASS-1 V7, PASS-2 V7, and MASTER-PLAN V7 once those reports commit, and route any cross-target conflict back to the originating target — not to PASS-3.

### §8.3 Forward-looking guard rails

Three V7-specific guard rails should ride into per-tranche full-spec drafting:

1. **The four committed closure sites are a hard cap, not a starting set.** Any V1 spec that adds a fifth closure site (e.g., a closure as struct field, a closure stored in a per-grammar registry, a closure returned from a rule body) must first amend Lock 1's reuse-map semantics to extend snapshot-scoped identity to closure environments. Without that amendment, the new site violates the substrate-identity invariant. The PASS-3 row at `:191` is normative for all V1 implementation.
2. **The `path-ts` deferral is principled, not pragmatic.** A future implementer must not ship a `path-ts` shim that wraps `path-core` at runtime; the deferral is for a TS-native parse+runtime fork. Lock 7 amendment + audit-5 §4 fold disposition both bind on this. The receiving gate when V2 lands is the `TsBackend: Backend` impl — implementing `Backend` is the only path that satisfies the deferral.
3. **The WASM lower-and-bench programme defers to V2 in full.** No partial WASM measurement may land in V1 (no "preview WASM bench row", no "WASM smoke gate", no "WASM ABI sketch"). Lock 5 amendment + Lock 8 amendment both bind. The receiving gate is the V2 `WasmBackend: Backend` impl. Per-tranche specs that propose a partial WASM landing must amend both locks first.

These guard rails are not amendments to PASS-3; they are reading-instructions for downstream consumers. The PASS-3 surface itself is settled.
