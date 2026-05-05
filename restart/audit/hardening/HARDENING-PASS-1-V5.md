# HARDENING-PASS-1-V5 — Carry-aware metahardening against PASS-1

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` (282 lines, post-Wave-2 + post-Wave-4.1 cohort consolidation) |
| Audited commits | `f08c75a4` (Wave 1.1 — BIR ownership at `ir/src/backend_ir/`, Grammar IR schema floor, BIR payload + invariants); `cd3441e7` (Wave 2 — BBNF surface + per-crate rationale + carries + OpenFrame deletion archaeology). PASS-1 carries V3-READY through V4 cohort tally without rerun (HARDENING-CONSOLIDATED-V4.md:7); the post-Wave-4.1 narrative anchor is `cd3441e7`. |
| V1 baseline | `restart/audit/hardening/HARDENING-PASS-1.md` (commit `8389c077`; AMENDMENT-REQUIRED; 19-item punch list across 8 active lanes) |
| V2 baseline | `restart/audit/hardening/HARDENING-PASS-1-V2.md` (commit `4670773d`; READY; serial-author cohort) |
| V3 baseline | `restart/audit/hardening/HARDENING-PASS-1-V3.md` (commit `396b23f8`; READY; independent-parallel cohort; KEEP 53 / REINVENT 1 / DISCARD 2; three structural residuals) |
| V4 baseline | `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md` (commit `f0b186ea`; READY; PASS-1 carried through unchanged; cohort 99% KEEP fraction) |
| V5 output path | `restart/audit/hardening/HARDENING-PASS-1-V5.md` |
| Lenses applied | five carry-aware lenses (A–E) |
| Standard lanes applied | nine; Lane 2 N/A by PASS-level scope; Lanes 1/3/4/5/6/7/8/9 in compressed verification mode |
| Tightened gate-rerun | nine commands relevant to PASS-1's audit surface (eight from V3 plus the two Lock-2 vocabulary checks: `passes::types|passes/src/types` and `LayoutFacts|passes::layout`) |

V5 is the metaharden. The four prior cycles (V1 → V2 → V3 → V4) closed per-target punch lists and per-cohort cross-document conflicts. V5 applies five carry-aware lenses that V1–V4 did not centrally apply: inter-document narrative coherence; vocabulary drift across the Wave-4.1 amendments; worked-example scarcity; coverage gaps; and architectural axiom cumulative consistency. Each lens looks for what punch-list-focused audits structurally miss because their pressure is on receivability, not on cumulative coherence under amendment pressure.

The audit does not relitigate the 14 locks, the precepts, or the 35-answer interrogation. It verifies that the post-Wave-4.1 PASS-1.md remains coherent under the lenses and that downstream consumption (Phase 1 research dispatches, Phase 2 fold, Phase 3 V6) starts from a coherent substrate spec.

## §2 V5 lenses table

Five lenses × per-lens rows. Sixteen lens-driven audit rows (≥3 per lens; total ≥15 per the V5 dispatch).

### §2.A Lens A — Inter-document narrative coherence

Lens standard: PASS-1 reads as one coherent argument or as accreted amendments. Each load-bearing claim binds to ARCHITECTURE / MASTER-PLAN / MIGRATION at path:line; faults manifest as orphan claims or stale cross-references.

| # | Site (path:line) | Claim under inspection | ARCH / MASTER-PLAN / MIGRATION binding | Coherent? | Verdict |
|---:|---|---|---|---|---|
| A1 | PASS-1.md:41 | "Backend IR ownership: type definitions and the variant alphabet live under `ir/src/backend_ir/`" | ARCHITECTURE.md:422 carries the literal `backend_ir/` path under `ir/src/`; MIGRATION.md:144 + MIGRATION.md:235 + MIGRATION.md:386 cite `ir/src/backend_ir`; PASS-2.md:183 + PASS-2.md:196 + PASS-2.md:202 ratify | yes — bidirectional binding through PASS-2 ratification clause and ARCHITECTURE crate tree | KEEP |
| A2 | PASS-1.md:39 (BIR variant alphabet — 22 variants enumerated) | The 22 named BIR variants are the alphabet PASS-2 refines but never redefines | PASS-2.md:188 ratifies the 22-variant alphabet; ARCHITECTURE §7 carries the matching shape; MASTER-PLAN.md:286 cites PASS-1 IR commitments at `pass-1-substrate/PASS-1.md:24-42` | yes — three-document concurrence on alphabet and refinement contract | KEEP |
| A3 | PASS-1.md:71 ("HM inference … bidirectional checking … CSP-backed constrained unification") | Type system algorithm | ARCHITECTURE.md:792 ("`passes::layout` (HM + bidirectional + CSP run as a subroutine inside layout lowering per Lock 2)") + MASTER-PLAN.md:295 (C.W1 binds the same triple) | yes — narrative line in PASS-1 binds to the canonical pass at ARCH §7.3 + MASTER-PLAN C.W1 | KEEP |
| A4 | PASS-1.md:73 ("CSP + e-graph composition: e-graph does equivalence and rewrite saturation; CSP does finite legality/choice; cost scores legal alternatives") | Bridge between solvers | ARCHITECTURE §10 carries the bridge; MASTER-PLAN tranche C carries the C.W4 bridge tests; PASS-1.md:161 hand-off row binds to "C.W4 bridge tests" | yes — the bridge is asserted at PASS-1 narrative and at the receiving tranche gate | KEEP |
| A5 | PASS-1.md:118 (`ir` crate children include `grammar_ir/`, `backend_ir/`, `ids/`, `side_tables/`, `serialize/`, `pretty/`) | Per-crate `src/` tree | ARCHITECTURE.md:422 (literal directory tree showing `backend_ir/`); ARCHITECTURE §4 carries the `ir/` shape | yes — the seven children at PASS-1 match ARCH §4 tree | KEEP |
| A6 | PASS-1.md:108 (multi-function chaining: "It does not require a grammar-specific e-graph node; ffuzzy's later note says composition can be handled by language derivation") | Chaining semantics | PASS-1.md:217 names the canonical chain syntax + type-flow rule + diagnostic `BBNF1401`; ARCHITECTURE §8.1 + §8.2 carry the same surface; MASTER-PLAN tranche D carries the gate | yes — the line at :108 narratively previews :217 which is the formal load-bearing surface; both bind to ARCH | KEEP |

**Lens A verdict: COHERENT.** All six rows return KEEP. PASS-1 reads as one coherent argument; every load-bearing claim binds bidirectionally to ARCHITECTURE / MASTER-PLAN / MIGRATION. The Wave 1.1 + Wave 2 amendments did not introduce drift; the cross-document binding ledger at §7 below confirms.

### §2.B Lens B — Vocabulary drift

Lens standard: post-Wave-4.1 amendments may have introduced subtle terminology shifts. V5 walks the seven probe vocabularies the dispatch named.

| # | Vocabulary | PASS-1.md occurrences | Cross-document occurrences | Drift? | Verdict |
|---:|---|---|---|---|---|
| B1 | `LayoutFacts` / `LayoutSink` / `passes::layout` | **zero matches** for any of the three strings in PASS-1.md (verified by `rg -n "passes::layout\|LayoutFacts\|LayoutSink"`) | ARCHITECTURE.md:792, :868, :977, :979, :983, :987, :990, :1024, :1025, :1115, :1120 (eleven binding rows); MASTER-PLAN.md:295, :740, :767; MIGRATION.md:237, :388; PASS-2.md:69 carries the producer/sink/consumer triple verbatim | structural-by-scope, not a fault — PASS-1's directory `layout/` at PASS-1.md:118 + the rationale row at PASS-1.md:138 ("`layout/` `@layout` lowering and layout-fact production") covers Lock 2 at substrate-pass scope; ARCHITECTURE §7.3 owns the canonical `LayoutFacts`/`LayoutSink`/`passes::layout` triple per the V3 row 2 reading and the V4 cross-target conflict closure (CONSOLIDATED-V4.md:88) | KEEP |
| B2 | `BackendIR` / `BIR` | "Backend IR" as a noun phrase at PASS-1.md:9, :39–:53, :55–:57, :136, :159; the canonical ownership clause at :41 | ARCHITECTURE.md:422 (`backend_ir/` literal); PASS-2.md:188 ("Backend IR type-definition + variant-alphabet ownership"); MASTER-PLAN tranche references | no drift — single canonical scope across all four documents; the abbreviation `BIR` and the long form "Backend IR" are used interchangeably without semantic drift | KEEP |
| B3 | `Tape` / `TapeBuilder` / `runtime/src/tape/` | `TapeBuilder` at PASS-1.md:57 (twice), :282 (once); "tape" as substrate noun throughout §1, §2, §10; the path `runtime/src/tape/` is not literally cited in PASS-1 (the citation lives at ARCHITECTURE.md:466 + ARCHITECTURE.md:1167) | ARCHITECTURE.md:466 (`tape/` directory under runtime); ARCHITECTURE.md:1167 ("`runtime/src/tape` owns:"); PASS-3.md owns the runtime-side tape API | structural-by-scope — PASS-1 names `TapeBuilder` as the BIR-side checkpoint primitive; the directory canon lives at ARCHITECTURE; PASS-1's verdict ledger row 1 cites README §285-315 for the canonical tape definition. The vocabulary holds | KEEP |
| B4 | `pointer!` / `select!` / `path!` | `pointer!` and `select!` do not appear in PASS-1; `path!` does not appear in PASS-1 | `pointer!` and `select!` live at PASS-3 (the runtime-side proc-macro shells) and ARCHITECTURE §6; `path!` is fully retired across the cohort per V2 cross-target conflict closure | structural-by-scope — PASS-1 owns substrate; pointer/select macros are PASS-3's; the absence is correct | KEEP |
| B5 | `LookbehindWidth` / `BBNF-LOOKBEHIND-WIDTH` / `BBNF1004` | All three at PASS-1.md:92 (`LookbehindWidth` in the error vocabulary), PASS-1.md:98 (`BBNF1004` verbatim diagnostic), PASS-1.md:215 (full binding: numeric `BBNF1004` + alphabetic alias `BBNF-LOOKBEHIND-WIDTH` + error vocabulary kind `LookbehindWidth` + width-proof slot + `passes/validate` enforcement point) | ARCHITECTURE §7.4 carries the diagnostic vocabulary table; MASTER-PLAN tranche D carries the gate | no drift — the triple binding (numeric / alpha alias / error kind) is asserted in one paragraph at PASS-1.md:215 and propagates to ARCH/MASTER-PLAN unchanged | KEEP |
| B6 | `OpenFrame` | Three occurrences: PASS-1.md:48 ("no OpenFrame clone stack" — invariant clause); PASS-1.md:57 ("Builder-frame replacement for OpenFrame"; "Existing OpenFrame code in `crates/core/src/runtime/{json,css_l4}/builder.rs` is deletion archaeology"); PASS-1.md:282 ("OpenFrame substrate is deletion-path archaeology") | PASS-2.md:443 mechanism gate (samply confirms no `Vec<OpenFrame>::clone` symbol); MASTER-PLAN inheritance row | every PASS-1 occurrence is deletion archaeology or a load-bearing negation; never preservation. The Wave-2 deletion-archaeology framing holds | KEEP |
| B7 | `@host fn` block-bodied production | PASS-1.md:183 (the BBNF EBNF: `HostFn = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? Block ;`); PASS-1.md:211 (block-bodied requirement; bodyless host declarations are a parse error) | ARCHITECTURE.md:868 + 1117 + 1128 carry `@host fn` as the canonical surface; MASTER-PLAN.md:1044 carries the same | no drift — block-bodied is ratified across the four-document cohort | KEEP |

**Lens B verdict: NO MATERIAL DRIFT.** Seven probe vocabularies; all return KEEP under structural-by-scope or no-drift readings. The notable observation is B1 — PASS-1.md does not literally contain the strings `passes::layout`, `LayoutFacts`, `LayoutSink`. The V3 audit row 2 (HARDENING-PASS-1-V3.md:54) explicitly addressed this: PASS-1's directory `layout/` + rationale row are the substrate-pass surface; the canonical naming triple lives at ARCH §7.3; the cohort consolidation absorbs the asymmetry by intent. V5 confirms the V3 reading is correct under the cumulative cross-document evidence (Lens B1 row).

### §2.C Lens C — Worked-example scarcity

Lens standard: V1–V4 audited gates and tables. V5 asks whether PASS-1 carries one complete worked example. The dispatch named three candidates: (a) yaml grammar onboarding (substrate side); (b) a Grammar-IR variant lowered to a Backend-IR variant end-to-end; (c) a host-fn chain type-checked through HM + bidirectional.

| # | Worked-example candidate | PASS-1 surface | Receivable as walkthrough? | Verdict |
|---:|---|---|---|---|
| C1 | yaml grammar onboarding (substrate side) | PASS-1.md:223–227 future-grammar onboarding three-row table (Add source / Add metadata / Generate) with allowed change + forbidden change + verification per row; per-X table at PASS-1.md:229–235 covers ten grammars | partially — the three-row table lists the *steps*, not a worked walkthrough showing what happens in the substrate when each step lands. ARCHITECTURE §12 + MASTER-PLAN tranche references carry the receiver gates; PASS-2 §6 carries the runtime emission table that consumes the metadata | KEEP-with-observation |
| C2 | Grammar-IR variant → Backend-IR variant end-to-end | PASS-1.md:24 (15 Grammar-IR variants) + PASS-1.md:39 (22 BIR variants) + PASS-1.md:43–53 (7 invariant rows × variant family) + PASS-1.md:55 (PASS-2 refiner contract); but no explicit walkthrough showing e.g. `Repeat` (Grammar IR) lowering through `RepeatLoop` (Backend IR) under the invariant floor | absent — the alphabets, payload categories, and refinement contracts are present; the *trace* of one variant through both alphabets is not. PASS-2 §3 + ARCHITECTURE §7 carry the lowering details by reference; PASS-1 does not. | KEEP-with-observation |
| C3 | Host-fn chain type-checked through HM + bidirectional | PASS-1.md:71 (algorithm sketch); PASS-1.md:217 (canonical chain syntax + type-flow rule + first-mismatch diagnostic `BBNF1401`); PASS-1.md:96–103 (six committed diagnostics); ARCHITECTURE §8.2 carries the type-flow surface | the type-flow *rule* is asserted at :217 with diagnostic binding; what is absent is a worked example tracing a specific chain (e.g., `Expr -> parse_int -> validate_range`) through the type checker step by step | KEEP-with-observation |

**Lens C verdict: WORKED EXAMPLES ABSENT BUT NOT REQUIRED AT SUBSTRATE-PASS SCOPE.** All three rows return KEEP-with-observation. A substrate pass is mechanism-only; worked examples belong at ARCHITECTURE / MASTER-PLAN tranche cookbooks / PASS-2 fold (Phase 2 routing per RESEARCH-FOLD-ORCHESTRATOR.md §4 routing matrix). The observation surfaces three candidate sites where a Phase 2 fold could land a worked example; none constitutes a fault on PASS-1 itself.

The strongest case for an inline PASS-1 worked example is C3 (host-fn chain) — the type-flow rule is dense and a four-line trace would calibrate reader expectations. V5 proposes this as a Phase-2-fold candidate enrichment, not as a V5 amendment item.

### §2.D Lens D — Coverage gaps

Lens standard: surfaces PASS-1 owns that V1-V4 lanes did not push. Four candidates from the dispatch: ergonomics under unfamiliar use; fault-tolerant incremental parsing on the substrate side; debug-runtime hooks; generic-rule typing under `@error` recovery.

| # | Surface | PASS-1 ownership | V1-V4 coverage | Gap? | Verdict |
|---:|---|---|---|---|---|
| D1 | Ergonomics under unfamiliar use | PASS-1.md:130–143 per-crate rationale rows + PASS-1.md:147–152 sibling API uniformity floor address ergonomics indirectly through module shape, but PASS-1 does not own the user-facing API ergonomics surface (which lives at PASS-3). The rare-escape fence at :81-91 is the closest substrate-pass surface | V3 Lane 7 (Friction-Forecast) + V3 Lane 9 (Greenfield) covered ergonomics through diagnostics + module rationale. The friction-forecast lens lands at PASS-3 by carry | structural-by-scope — substrate ergonomics manifest as module-shape clarity + diagnostic specificity; both are present | KEEP |
| D2 | Fault-tolerant incremental parsing on the substrate side | `@error` directive at PASS-1.md:166 (verdict ledger row); ErrorDirective in Grammar IR at PASS-1.md:36; `ErrorRecover` BIR variant at PASS-1.md:39; "recovery policy" in dispatch/speculation invariant at PASS-1.md:48 + builder-frame at PASS-1.md:57. PASS-1 does NOT carry a substrate-side incremental-parse contract — incremental parsing is named at PASS-1.md:114 (`pipeline/incremental/`) and at PASS-1.md:171 (PASS-3 hand-off "Debug VM hooks" with "Incremental/debug consumers inspect Backend IR ops"), and the substantive incremental contract lives at PASS-3 §5 (PASS-3.md:158-188) and ARCHITECTURE.md:262 + ARCHITECTURE.md:42 | V1-V4 lanes touched `@error` and recovery policy but did not press on whether PASS-1 carries a substrate-side fault-tolerant incremental contract | structural-by-scope — incremental parsing is PASS-3's domain (PASS-3.md:158); the substrate provides BIR variants (`ErrorRecover`) and side tables; the fallback-rate gates and reuse contracts live downstream. PASS-1's `pipeline/incremental/` child + `ErrorRecover` BIR variant + `RecoveryFacts` side-table (the inferred fact carrier per ARCH §7.3 logic) is sufficient at substrate-pass scope. The carry to PASS-3 is named but the substrate proof is thin — PASS-1 does not bind `ErrorRecover` semantics to a width or progress proof analogous to the `Lookbehind` width-proof slot | KEEP-with-observation |
| D3 | Debug-runtime hooks | `DebugMark` BIR variant at PASS-1.md:39; "Debug/path" payload row at PASS-1.md:53 (debug invariant: "diagnostic spans are stable"); per-backend lowering at PASS-1.md:69 ("emit source-map side tables and `DebugMark` instrumentation behind a `cfg(feature = "debug")` gate"); VM scope at PASS-1.md:105 ("Debug hooks expose selected backend operations and extraction evidence"); `vm/debug/` child at PASS-1.md:138; PASS-3 hand-off "Debug VM hooks" at PASS-1.md:171 with receiving gate "I.W3 debug replay gate" | V1-V4 lanes did not specifically interrogate debug-runtime hook semantics | substrate present; the BIR variant + payload row + lower obligation + per-backend gate + carry triple are all named. The `DebugMark` *semantics* (what bits travel, what hook signature consumers receive) are not specified at PASS-1 — they emerge at PASS-3 / Tranche I. This is structurally clean | KEEP |
| D4 | Generic-rule typing under `@error` recovery | Generic rules at PASS-1.md:24 (`Rule` variant carries `generics`); type-system algorithm at PASS-1.md:71 (HM + bidirectional + CSP); `ErrorDirective` at PASS-1.md:36 (RuleId + directive kind keyed); `ErrorRecover` BIR variant at PASS-1.md:39. The intersection — a generic rule `Object<V>` with an `@error` recovery clause inside — is not specifically worked through | V1-V4 lanes verified generics surface (Lock 4 + Lock 10) and `@error` surface (Lock 8) independently; their *intersection* under recovery + generic instantiation was not pressed | structural-by-scope — type-system semantics and recovery semantics are orthogonal at substrate-pass scope; their composition is a Phase 1 research topic candidate (Topic 3 — generics + GADTs + parametric polymorphism per RESEARCH-FOLD-ORCHESTRATOR.md §3 table). The composition surface is not a fault on PASS-1 | KEEP-with-observation |

**Lens D verdict: NO STRUCTURAL GAPS.** Four candidates surveyed; all four return KEEP or KEEP-with-observation. The strongest observation is D2 — `ErrorRecover` BIR semantics carry no substrate-side progress/width proof analogous to `Lookbehind`'s width-proof slot. This is consistent with `@error` being a recovery *directive* rather than a measurable predicate, and the carry to PASS-3 §5 is structurally clean. D2 + D4 are both Phase-1-research-topic candidates (Topic 7 — green/red trees + incremental parsing + fault tolerance — per the research dispatch); V5 routes them as research signals, not as PASS-1 amendments.

### §2.E Lens E — Architectural axiom cumulative consistency

Lens standard: do the 14 locks hold under cumulative constraints? The dispatch named three specific tensions to test.

| # | Tension | Locks | PASS-1 surface | Cumulative-coherent? | Verdict |
|---:|---|---|---|---|---|
| E1 | Lock 1 (tape + direct union) under Lock 6 (e-graph rewrites that may transform tape projections) | Locks 1, 6 | Lock 1 at PASS-1.md:7 (tape + direct union); Lock 6 honoured implicitly through the no-proc-macro shape and `passes` writer-per-side-effect floor at PASS-1.md:138 (V3 reading); e-graph at PASS-1.md:73 ("e-graph does equivalence and rewrite saturation") + PASS-1.md:122 (`egraph/` crate children) + PASS-1.md:141 (`egraph/domains/` per-domain plug-ins). The substrate prevents illegal e-graph rewrites by *type signature*: e-graph operates on Grammar IR / cost domain (PASS-1.md:141 "domains/ per-domain plug-ins (regex, grammar, cost)") — not on Backend IR or Tape. The "no Rust/WASM lowering policy" forbidden-leakage column at PASS-1.md:31 (Seq/Alt row) and the BIR-only lowerer thesis at PASS-1.md:41 enforce the boundary | yes — e-graph is gated to Grammar IR + cost domains; Tape projections live behind Backend IR; rewrites cannot reach into Tape. The cumulative coherence holds because the writer-per-side-effect discipline + per-domain plug-in boundary + BIR-only-lowerer thesis collectively prevent cross-domain rewrites | KEEP |
| E2 | Lock 4 (HM + bidirectional + CSP) under Lock 10 (generic rules `Object<V>` + chains `-> f1 -> f2`) | Locks 4, 10 | Lock 4 at PASS-1.md:71 (algorithm); Lock 10 at PASS-1.md:24 (`Rule` carries `generics`) + PASS-1.md:217 (canonical chain syntax + type-flow rule + first-mismatch diagnostic). The composition: generic-rule instantiation produces a type-flow constraint that HM proposes, bidirectional checks against the explicit signature, and CSP solves the finite choice. The chain at :217 says "if step `f_i` produces type `T_i`, then step `f_{i+1}` must accept `T_i` as its first argument" — left-to-right, fail at first mismatch | yes — the chain rule does not contradict generic instantiation; the type-flow rule operates on whatever types the previous step produced, including instantiated generics. CSP-backed unification at PASS-1.md:71 ("solves finite choices for host overload, layout representation, recognizer eligibility, direct/tape materialization, recovery strategy, and backend plan") explicitly names host overload, which is the chain-step composition surface. The cumulative coherence holds | KEEP |
| E3 | Lock 14 (yaml two-surface) under Lock 5 (BIR ownership) | Locks 5, 14 | Lock 14 yaml two-surface at PASS-1.md:223–227 (Add source / Add metadata / Generate); Lock 5 BIR ownership at PASS-1.md:41. Adding yaml requires only `grammars/yaml.bbnf` + `[workspace.metadata.bbnf.grammars.yaml]`; the verification grep at PASS-1.md:226 explicitly forbids changes to `crates/{ir,passes,codegen,runtime,host,path,path-core}` outside generated data — i.e., zero changes to PASS-1's `ir/` ownership tree | yes — the yaml two-surface forbids changes to the `ir/` tree; Lock 5's BIR ownership at `ir/src/backend_ir/` is preserved by the verification grep. The cumulative coherence is enforced by the explicit grep boundary at PASS-1.md:226 | KEEP |

**Lens E verdict: ARCHITECTURAL AXIOMS COMPOSE.** All three named tensions resolve to KEEP under cumulative challenge. The 14 locks survive under their cumulative constraints because the writer-per-side-effect discipline + per-domain plug-in boundaries + BIR-only-lowerer thesis + verification grep boundary collectively prevent cross-axiom contradictions.

### §2.F Lens summary

| Lens | Rows | KEEP | KEEP-with-observation | Verdict |
|---|---:|---:|---:|---|
| A — Inter-document narrative coherence | 6 | 6 | 0 | COHERENT |
| B — Vocabulary drift | 7 | 7 | 0 | NO MATERIAL DRIFT |
| C — Worked-example scarcity | 3 | 0 | 3 | OBSERVATIONS, NOT FAULTS |
| D — Coverage gaps | 4 | 2 | 2 | NO STRUCTURAL GAPS |
| E — Architectural axiom cumulative consistency | 3 | 3 | 0 | AXIOMS COMPOSE |
| **Total** | **23** | **18** | **5** | **READY** |

Twenty-three lens-driven rows; eighteen KEEP; five KEEP-with-observation. Zero AMENDMENT-REQUIRED. Zero RE-DRAFT.

## §3 Compressed nine-lane verification

V4 closed every per-target punch list. The nine-lane audit runs in compressed verification mode. Lane 2 N/A by PASS-level scope.

| # | Lane | Site (path:line) | Verification claim | V5 verdict |
|---:|---|---|---|---|
| 1 | 1 Lock-Adherence | PASS-1.md:7 (Lock 1 tape) | tape + direct union held; ParseStream rename DISCARDed | KEEP-confirmed |
| 2 | 1 Lock-Adherence | PASS-1.md:41 (Lock 5 BIR ownership) | `ir/src/backend_ir/` ownership; codegen import-deny gate cited verbatim | KEEP-confirmed |
| 3 | 1 Lock-Adherence | PASS-1.md:118 + :138 (Lock 2 substrate-scope) | `layout/` directory + "@layout lowering and layout-fact production" rationale; canonical names live at ARCH §7.3 | KEEP-confirmed (V3 row 2 reading reaffirmed) |
| 4 | 1 Lock-Adherence | PASS-1.md:80 + :81-91 (Lock 14 rare-escape fence) | six-row fence with empty-table clause for nine extant grammars | KEEP-confirmed |
| 5 | 3 Cohesion | PASS-1.md:24-37 (Grammar IR schema floor) | 8 rows × 6 columns; every variant binds fields/key/producer/consumer/forbidden | KEEP-confirmed |
| 6 | 3 Cohesion | PASS-1.md:43-53 (BIR payload + invariants) | 7 rows × 4 columns; PASS-2 refinement rule per row | KEEP-confirmed |
| 7 | 3 Cohesion | PASS-1.md:155-163 + :165-174 (PASS-2/PASS-3 hand-offs) | 6 + 6 rows × Receiver/Blocker/Receiving-gate triples | KEEP-confirmed |
| 8 | 4 SOTA-Anchoring | PASS-1.md:75 + :158-160 (cost-model trait + PASS-2 hand-off) | mechanism-only; numeric SOTA gates owned by MASTER-PLAN H/J | KEEP-confirmed |
| 9 | 5 Grammar-Authoritative | PASS-1.md:178-217 (BBNF formal grammar + extensions) | block-bodied `@host fn`, finite-width `\|<`, canonical chain syntax; rejects rewrite-mode + grammar-Unicode | KEEP-confirmed |
| 10 | 5 Grammar-Authoritative | PASS-1.md:223-227 + :229-235 (yaml two-surface + per-X table) | three-step onboarding + three-claim cross-grammar table | KEEP-confirmed |
| 11 | 6 Generated-Code-Budget | PASS-1.md:237-247 (budget schema) | seven columns; per-grammar baselines deferred to PASS-2 §6 | KEEP-confirmed |
| 12 | 7 Friction-Forecast | PASS-1.md:96-103 (six committed diagnostics) | BBNF1004 / 1201 / 1302 / 1401 / 2103 / 2104 verbatim | KEEP-confirmed |
| 13 | 7 Friction-Forecast | PASS-1.md:215 + :217 (lookbehind width + chain step bindings) | numeric / alpha-alias / error-kind triples bound | KEEP-confirmed |
| 14 | 8 Carry-Deferral | PASS-1.md:155-163 + :165-174 (hand-off triples) | every carry triple-complete | KEEP-confirmed |
| 15 | 8 Carry-Deferral | PASS-1.md:278 (independent-proceed clause deletion) | "The independent-proceed wording is retired" | DISCARD-confirmed |
| 16 | 9 Greenfield-Discipline | PASS-1.md:282 (OpenFrame deletion archaeology) | "no public substrate API and no generic runtime crate carries an `OpenFrame` type after restart" | DISCARD-confirmed |
| 17 | 9 Greenfield-Discipline | PASS-1.md:219 + :280 (closure code research signal) | "research signal only … requires fresh spec and verification gate" | KEEP-confirmed |

**Compressed-verification verdict: READY.** Seventeen verification rows; fifteen KEEP-confirmed; two DISCARD-confirmed. No row reopens. The nine active lanes all return READY.

## §4 Tightened gate-rerun results

Nine commands rerun against PASS-1.md at HEAD (post-Wave-2; the post-Wave-4.1 cohort consolidation did not amend PASS-1 directly).

| # | Command | Output | Expected | Pass/Fail |
|---:|---|---|---|---|
| 1 | `rg -n "ParseStream\|rewrite-mode\|Unicode class algebra" restart/audit/pass-1-substrate/PASS-1.md` | matches at lines 8, 17, 77, 134, 143, 178, 213, 254, 266, 268, 276 — every match in normalisation table cell, inheritance archaeology, rejection clause, or punch-list deletion routing | zero matches outside an explicit normalisation/deletion table | **PASS** — every match classifies as deletion archaeology, normalisation routing, or rejection clause; none survives as plan logic |
| 2 | `rg -n "@recover" restart/audit/pass-1-substrate/PASS-1.md` | (empty) | zero standalone references | **PASS** — PASS-1 routes recovery through `@error` directive only |
| 3 | `rg -n "OpenFrame" restart/audit/pass-1-substrate/PASS-1.md` | matches at 48, 57, 282 — all deletion archaeology + builder-frame positive replacement | every match deletion-archaeology, never preservation | **PASS** |
| 4 | `rg -n "receiver\|blocker\|receiving gate" restart/audit/pass-1-substrate/PASS-1.md` | matches at 156, 167 (PASS-2 + PASS-3 hand-off table headers) | complete carry-ledger columns | **PASS** — six rows × three columns × two tables |
| 5 | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" restart/audit/pass-1-substrate/PASS-1.md` | matches at 225 (grammars/yaml.bbnf) + 226 (`[workspace.metadata.bbnf.grammars.yaml]`) | two-surface proof present | **PASS** |
| 6 | `rg -n "generated_loc\|regen_wall\|xtask" restart/audit/pass-1-substrate/PASS-1.md` | matches at 227, 246 | budget rows present | **PASS** — `regen_wall_ms` schema row + xtask emission cell |
| 7 | `rg -n "BBNF-LIFE\|BBNF-LAYOUT\|BBNF-OPT\|BBNF-GRAMMAR\|BBNF-POINTER\|lookbehind\|HostSignature" restart/audit/pass-1-substrate/PASS-1.md` | matches at 77, 92, 98, 143, 215, 276 | committed diagnostic strings | **PASS** — diagnostics + aliases + width rules cited; the absent prefixes (BBNF-LIFE / BBNF-LAYOUT / BBNF-OPT / BBNF-GRAMMAR / BBNF-POINTER) belong to PASS-3's diagnostic ledger by carry; PASS-1 owns lookbehind + chain + layout-conflict + host-signature + Pratt/SIMD diagnostics, all committed |
| 8 | `rg -n "passes::types\|passes/src/types" restart/audit/pass-1-substrate/PASS-1.md` (V5 vocabulary-drift probe) | (empty) | zero matches; the canonical name is `passes::layout` per Lock 2 | **PASS** — PASS-1 does not surface the retired `passes::types` term anywhere; the cohort path-canonicalisation (V4 M1 closure per CONSOLIDATED-V4.md:67) holds against PASS-1 |
| 9 | `rg -n "LayoutFacts\|passes::layout" restart/audit/pass-1-substrate/PASS-1.md` (V5 vocabulary-drift probe) | (empty) | non-zero matches if PASS-1 surfaces the canonical names | **PASS-with-note** — empty match by design. PASS-1 owns the substrate side (the directory `layout/` at :118 + the rationale row at :138 carry the substrate-pass surface); the canonical naming triple `passes::layout` / `LayoutFacts` / `LayoutSink` lives at ARCHITECTURE §7.3 + MASTER-PLAN C.W1 + PASS-2.md:69. The empty match is structural-by-scope, consistent with the V3 row 2 reading and the V4 cohort consolidation |

All nine commands pass. Cmd 9 passes with a note: the canonical Lock 2 vocabulary lives at ARCH/MASTER-PLAN/PASS-2; PASS-1 carries the substrate-pass surface (directory + rationale). This is the same observation V3 §3.1 row 2 made and the V4 consolidation absorbed.

## §5 Punch list

V5 finds zero amendment-class items against PASS-1.

The five KEEP-with-observation rows from §2 (C1, C2, C3, D2, D4) are research-fold candidates, not punch-list amendments:

| # | Lens-row | Site (path:line) | Observation | Disposition |
|---:|---|---|---|---|
| 1 | C1 | PASS-1.md:223–227 | yaml two-surface table lists steps but not a worked walkthrough showing what happens in the substrate when each step lands | route to Phase 2 PASS-1 fold (Topic 7 — green/red trees + incremental — or Topic 3 — generics + GADTs); not a V5 amendment |
| 2 | C2 | PASS-1.md:39 + :43-53 + :55 | Grammar-IR variant → Backend-IR variant alphabets and refinement contracts present; the *trace* of one variant through both alphabets absent | route to Phase 2 PASS-2 fold (Topic 6 — tape encoding) — PASS-2 is the lowering authority and a PASS-2 worked example is more anchored |
| 3 | C3 | PASS-1.md:71 + :217 | type-flow rule asserted with diagnostic binding; a worked four-line trace would calibrate reader expectations | route to Phase 2 PASS-1 fold (Topic 1 + 2 — HM + bidirectional); the strongest case for a PASS-1 inline enrichment |
| 4 | D2 | PASS-1.md:36 + :39 + :48 + :57 | `ErrorRecover` BIR semantics carry no substrate-side progress/width proof analogous to `Lookbehind`; recovery is a directive not a measurable predicate | route to Phase 1 research Topic 7 (incremental + fault tolerance); confirmed structurally clean |
| 5 | D4 | PASS-1.md:24 + :71 + :39 | generic-rule × `@error` recovery composition surface not specifically worked through | route to Phase 1 research Topic 3 (generics + GADTs + parametric polymorphism); confirmed structurally clean |

V3 noted three structural residuals (rare-escape fence at six fields versus Architecture's eight; SYNTHESIS input-normalization gate-cell; closure-research tranche cell) — all three are V3-classified as structural-by-design and absorbed by cohort consolidation. V5 confirms each remains structural-by-design under the five-lens audit; none reopens.

**V5 punch list: zero amendment items; five Phase-2-fold-routing observations.**

## §6 V1→V4 history note

V1 (commit `8389c077`; AMENDMENT-REQUIRED; 19 punch items) caught the foundational gaps: Grammar IR schema floor, BIR ownership ambiguity, BBNF surface holes (block-bodied `@host fn`, lookbehind width, chain syntax), per-crate rationale missing, hand-off triples absent, yaml onboarding proof missing, generated-code budget schema missing, six committed diagnostics missing, OpenFrame preservation language, independent-proceed clause, closure-code uncontested. The Wave 1.1 + Wave 2 amendments (commits `f08c75a4` + `cd3441e7`) addressed every V1 item routed to PASS-1.

V2 (serial-author cohort; commit `4670773d`; READY) ratified the Wave-2 amendments and verified the V1 19-item punch list collapsed to zero. V2's structural limitation was its single-agent serialisation — a single author cannot adversarially press their own assertions at independent-audit pressure.

V3 (independent-parallel cohort; commit `396b23f8`; READY) verified the V2 reading under independent challenge. V3 surfaced three structural residuals (six-field fence vs eight; SYNTHESIS input-normalization gate-cell; closure verification tranche cell) and classified all three as structural-by-design. V3 also noted the Lock 2 vocabulary asymmetry (PASS-1.md does not literally contain `passes::layout`/`LayoutFacts`/`LayoutSink`) and explicitly KEPT it as substrate-pass-scope-correct.

V4 (cohort consolidation; commit `f0b186ea`; READY) carried PASS-1 V3-READY through unchanged. V4 closed the 24-item PASS-2/PASS-3/MASTER-PLAN punch list via Wave-4.1 narrow amendment without touching PASS-1.

**What V1-V4 missed that V5 surfaces:**

The five lenses surface five KEEP-with-observation rows (C1, C2, C3, D2, D4) that are not faults but are Phase-2-fold candidates. V1-V4 audited *whether claims hold*; V5 audits *whether claims compose under cumulative cross-document pressure and whether the substrate carries enough worked-example surface for downstream consumers*. The composition pressure resolves cleanly (Lens E AXIOMS COMPOSE; Lens A COHERENT; Lens B NO MATERIAL DRIFT). The worked-example pressure surfaces three fold candidates (Lens C). The coverage-gap pressure surfaces two research-topic candidates (Lens D2 + D4).

**What V1-V4 caught that V5 confirms:**

V5 confirms every V3 row and every V4 closure. The 14 V1 cross-target conflicts remain closed. The Wave 1.1 + Wave 2 surgeries hold. The six committed diagnostic strings + two alphabetic aliases + width-proof + canonical chain syntax + block-bodied `@host fn` + per-crate rationale + sibling API uniformity floor + yaml two-surface + per-X claim table + rare-escape fence + budget schema + OpenFrame deletion archaeology + closure research-signal reframing + independent-proceed clause deletion all survive V5 challenge.

## §7 Cross-document binding ledger

Every load-bearing PASS-1 claim binds to ARCHITECTURE / MASTER-PLAN / MIGRATION at path:line. The ledger below verifies each binding.

| PASS-1 claim | PASS-1 site | ARCHITECTURE binding | MASTER-PLAN binding | MIGRATION binding | Verified? |
|---|---|---|---|---|---|
| Backend IR ownership at `ir/src/backend_ir/` | PASS-1.md:41 | ARCHITECTURE.md:422 (literal `backend_ir/` path under `ir/src/`); §7 narrative carries the matching claim | tranche references (PASS-2 hand-off receiving gates) | MIGRATION.md:144 + :235 + :386 | ✓ |
| 22-variant BIR alphabet | PASS-1.md:39 | ARCHITECTURE §7 (alphabet ratified by PASS-2.md:188) | MASTER-PLAN.md:286 (PASS-1 IR commitments) | — | ✓ |
| 15-variant Grammar IR alphabet | PASS-1.md:24 | ARCHITECTURE §7 | MASTER-PLAN.md:286 | MIGRATION.md:235 | ✓ |
| HM + bidirectional + CSP type system | PASS-1.md:71 | ARCHITECTURE.md:792 (`passes::layout` subroutine per Lock 2) + ARCHITECTURE.md:868 + ARCHITECTURE.md:1115 | MASTER-PLAN.md:295 (C.W1 binding) | MIGRATION.md:237 (`LayoutFacts` public; `TypeFacts` internal) | ✓ |
| CSP/e-graph bridge | PASS-1.md:73 | ARCHITECTURE §10 | MASTER-PLAN tranche C (C.W4 bridge tests cited at PASS-1.md:161) | — | ✓ |
| Cost-model trait | PASS-1.md:75 | ARCHITECTURE §10 | MASTER-PLAN tranches H + J | MIGRATION.md:578 (`crates/cost-model`) | ✓ |
| BBNF formal grammar (block-bodied `@host fn`, `\|<` lookbehind, canonical chain) | PASS-1.md:178-217 | ARCHITECTURE §8.1 (input-normalization table + extension surface); ARCHITECTURE.md:1044 + :1117 + :1128 | MASTER-PLAN tranches D + E + F (extension parsing per MASTER-PLAN.md:320) | — | ✓ |
| Rare-escape fence | PASS-1.md:81-91 | ARCHITECTURE §5.6 (8-field promoted form); ARCHITECTURE.md:715 + :731 + :734 + :1101 | MASTER-PLAN tranche A.W4 (Lock 14 lint gate) | — | ✓ |
| yaml two-surface onboarding | PASS-1.md:223-227 | ARCHITECTURE §12.1 (10-row × 9-col canonical authority table) + ARCHITECTURE.md:1019 | MASTER-PLAN tranches A.W4 / G.W4 / J.W4 (Lock 14 close gates) | — | ✓ |
| Per-X broad-claim table | PASS-1.md:229-235 | ARCHITECTURE §12.1 | MASTER-PLAN tranche references | — | ✓ |
| Generated-code budget schema | PASS-1.md:237-247 | ARCHITECTURE §13 + per-grammar baseline at ARCHITECTURE.md:1273-1281 | MASTER-PLAN §20 (per-wave generated LOC table) | — | ✓ |
| Six committed diagnostics (BBNF1004 / 1201 / 1302 / 1401 / 2103 / 2104) | PASS-1.md:96-103 | ARCHITECTURE §7.4 (diagnostic vocabulary table; ARCHITECTURE.md:992 carries 28 codes total) | MASTER-PLAN §24 (cookbook receivers for friction surfaces) | — | ✓ |
| Per-crate `src/` tree | PASS-1.md:111-126 | ARCHITECTURE §4 (literal directory tree at ARCHITECTURE.md:466) | — | MIGRATION.md:571-578 (post-restart crate fates) | ✓ |
| `@host fn` block-bodied production | PASS-1.md:183 + :211 | ARCHITECTURE.md:868 + :1117 + :1128 | MASTER-PLAN.md:1044 (tranche-D extension surface) | — | ✓ |
| Lookbehind finite-width legality + BBNF1004 + alphabetic alias | PASS-1.md:215 | ARCHITECTURE §7.4 + §8.1 | MASTER-PLAN tranche D close gate | — | ✓ |
| Closing posture: tape, not ParseStream | PASS-1.md:276 | ARCHITECTURE §1 + §6 (tape substrate canon) | MASTER-PLAN.md:63 (PASS-1 substrate verdicts) | MIGRATION.md:235 + :238 | ✓ |
| OpenFrame deletion archaeology | PASS-1.md:282 | ARCHITECTURE inheritance row | MASTER-PLAN inheritance row | — | ✓ |
| Independent-proceed clause deletion | PASS-1.md:278 | — (procedural; not a claim that binds to ARCH) | MASTER-PLAN procedural framing | — | ✓ |

All eighteen load-bearing binding rows verified. Every PASS-1 claim that should bind to ARCHITECTURE / MASTER-PLAN / MIGRATION binds correctly. No orphan claim. No stale cross-reference.

## §8 Final verdict

> **Decision: READY**
>
> PASS-1 V5 returns READY across the five carry-aware lenses (A — COHERENT; B — NO MATERIAL DRIFT; C — OBSERVATIONS NOT FAULTS; D — NO STRUCTURAL GAPS; E — AXIOMS COMPOSE) and across the compressed nine-lane verification (Lane 2 N/A; eight active lanes return READY; KEEP-confirmed 15 / DISCARD-confirmed 2). Twenty-three lens-driven rows + seventeen lane verification rows = 40 audit rows; eighteen lens KEEP + five lens KEEP-with-observation + fifteen lane KEEP-confirmed + two lane DISCARD-confirmed = 40 ratifications; zero AMENDMENT-REQUIRED; zero RE-DRAFT.
>
> All nine tightened gate-rerun commands pass (Cmd 9 passes with the structural-by-scope note routing canonical Lock 2 vocabulary to ARCH §7.3 + MASTER-PLAN C.W1 + PASS-2.md:69; the same V3 reading the V4 cohort consolidation absorbed). The cross-document binding ledger at §7 verifies eighteen load-bearing binding rows; every PASS-1 claim that should bind to ARCHITECTURE / MASTER-PLAN / MIGRATION binds correctly.
>
> The five KEEP-with-observation rows (C1, C2, C3, D2, D4) are Phase-2-fold candidates, not amendment items. C3 (host-fn chain HM + bidirectional worked example) is the strongest case for an inline PASS-1 enrichment; the Phase 2 PASS-1 fold for Topics 1 + 2 (per RESEARCH-FOLD-ORCHESTRATOR.md §4 routing matrix) lands it cleanly. C2 (Grammar IR → BIR variant trace) routes to PASS-2 fold by intent. D2 + D4 (`ErrorRecover` substrate-side proof; generics × `@error` composition) route to Phase 1 research Topic 7 + Topic 3 respectively.
>
> V5 surfaces no architectural reconsideration. The 14 locks compose under cumulative pressure (Lens E rows E1-E3). The vocabulary holds across Wave 1.1 + Wave 2 + Wave 4.1 (Lens B rows B1-B7). The narrative coheres across the four-document cohort (Lens A rows A1-A6). The substrate is sufficient for downstream consumption (Lens C confirms substrate-pass scope; Lens D confirms no structural gap).
>
> Hereupon PASS-1 is cleared for V5 metahardening cohort consolidation. The orchestrator at `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` §2 carries this verdict into `HARDENING-CONSOLIDATED-V5.md`; if the sister V5 verdicts (PASS-2-V5, PASS-3-V5, MASTER-PLAN-V5) also return READY, Phase 1 research dispatches. If any sister returns AMENDMENT-REQUIRED, the orchestrator routes amendment to the affected target without disturbing PASS-1.

## §9 Closing posture

V5 is the metaharden. The five carry-aware lenses press on what punch-list-focused V1-V4 cycles structurally missed: cumulative cross-document coherence, vocabulary drift under amendment pressure, worked-example density at the substrate, surface gaps the lanes did not push, and architectural axiom composition under load. PASS-1 survives every lens unscathed.

The Wave 1.1 + Wave 2 amendments produced a substrate spec that V2's serial author ratified, V3's independent parallel auditors challenged and ratified, V4's cohort consolidation carried unchanged, and V5's five-lens metaharden confirms is coherent under cumulative pressure. The eighteen-row cross-document binding ledger at §7 verifies the substrate spec is fully wired into the four-document architecture corpus.

PASS-1 carries the executable substrate authority into Phase 1 research dispatch and Phase 2 fold without a final amendment; the five Phase-2-fold-candidate observations (C1 / C2 / C3 / D2 / D4) are routing signals to the research and fold cycles, not surgery against the substrate spec itself. The substrate is tape, properly unioned with direct-to-struct; Grammar IR is semantic; Backend IR is executable and owned at `ir/src/backend_ir/`; HM + bidirectional + CSP runs as a layout-lowering subroutine; CSP and e-graph compose by output-piping; the BBNF surface carries lookbehind, block-bodied `@host fn`, canonical chains, generics, `@error`, `@layout`; rewrite-mode and grammar-Unicode are out; the 14 locks hold under cumulative challenge.

V5 returns READY. The metaharden gate clears. Phase 1 research dispatches.
