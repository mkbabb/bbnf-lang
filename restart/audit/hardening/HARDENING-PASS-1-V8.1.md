# HARDENING-PASS-1-V8.1 — Fold Verification Audit

## §1 Target Identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` |
| Audit role | independent V8.1 verification auditor — PASS-1 (post-Phase-8.4 fold) |
| V8 carry-baseline | `restart/audit/hardening/HARDENING-PASS-1-V8.md` (SIMPLIFY-AVAILABLE; 14 punch items routed to Phase 8.4 fold) |
| V8 cohort baseline | `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` (SIMPLIFY-AVAILABLE; 41 candidates across 4 targets) |
| Phase 8.3.1 commit | `a74cdc52` (corpus cleanup — GADT V1 + CHR V1 + composition delete + V5.1 prune + orchestrator hoist + README + HANDOFF) |
| Phase 8.4 PASS-1 fold commits | `4c69b848` (classification) + `23311ff8` (Grammar-IR merge + generic validation collapse + diagnostic numeric retire + host-leverage delegation + tranche-body routing) |
| Phase 8.4 SYNTHESIS commit | `e5cb1e4b` (Backend trait 5→2 + type-system 7→5 + BIR 22→19 + rewrite-budget 4→3 + V2-amendment retire) |
| Working tree at audit start | clean |
| Write surface | this report only |
| Cohort siblings (parallel) | V8.1-PASS-2, V8.1-PASS-3, V8.1-MASTER-PLAN |

V8.1 question: did Phase 8.3.1 corpus cleanup (Q1-Q8 user adjudication) and Phase 8.4 fold (14 PASS-1 punch items) land coherently in PASS-1? Did the fold close every V8 surgery, retire active V2-amendment language, and preserve cohort cross-reference integrity (BIR alphabet 19 / Backend trait 2 / 6-directive grammar)? Verdict (anticipated by §7): **READY** — every surgery closed; zero active V2-deferral residue; cohort cross-references match the post-fold state.

## §2 Phase 8.3.1 Corpus Cleanup Closure (Q1-Q3 PASS-1 surfaces)

Per `restart/research/CORPUS-AUDIT-SYNTHESIS.md:208-218` §9 user adjudication: Q1 (GADT V1 fold), Q2 (CHR V1 fold), Q3 (function composition library deletion). The PASS-1 surface must reflect the user's FOLD (Q1, Q2) + DELETE (Q3) decisions.

| Q | Adjudication | PASS-1 surface | Path:line evidence | Status |
|---|---|---|---|---|
| Q1 | FOLD GADT V1 | User-facing GADT branch-local equality at match-arm; `BBNF-LOCAL-EQUALITY-ANNOTATION` is V1 active diagnostic; OutsideIn(X) implication V1-active; `Refinement` + `Equality` non-terminals in §6 grammar; Lock 4 cross-reference cited | `PASS-1.md:73` ("GADT V1 user-facing surface" + "OutsideIn(X)-style implication constraints carry into the solver"); `:81` (CSP `Implication { givens, wanted }` propagation; `BBNF-LOCAL-EQUALITY-ANNOTATION` emission); `:85` ("OutsideIn(X)-style implication ... active"); `:129` (catalogue row); `:244-246` (`Arm`/`Refinement`/`Equality` productions); `:265` (match expressions admit `Pattern @ where T = U => Expr` form) | CLOSED |
| Q1 | RETIRE V2 amendment phrasing | Zero active "GADT V2 amendment" / "reserved for V2" / "hidden substrate"; only V1-affirmation negations ("not a future amendment", "is not reserved for V2 amendment") and ledger archaeology in §11 | `PASS-1.md:73` ("Higher-rank polymorphism is a V1 surface, not a future amendment"); `:73` ("the layer ships V1 and is not reserved for V2 amendment"); `:357` ledger row "in lieu of 'V2 amendment' phrasing"; zero active deferral commitments | CLOSED |
| Q2 | FOLD CHR V1 | CHR-improvement layer is V1-load-bearing; CHR-shaped rewrite rule fires before finite CSP search; lives inside `csp-solver` per Lock 11 publication coupling; type-system algorithm-layer table marks the CHR-improvement row "active" | `PASS-1.md:73` ("CHR-shaped, before finite CSP selection — the CHR-improvement layer is V1-load-bearing and lives inside `csp-solver`"); `:84` table row ("CHR-improvement layer ... active"); the prior "CHR where applicable" hedging is absent | CLOSED |
| Q3 | DELETE function composition library | Zero references; not part of the PASS-1 substrate surface | `rg -n 'function composition library' restart/audit/pass-1-substrate/PASS-1.md` returns zero | CLOSED |

§11 ledger at `PASS-1.md:340-360` records the simplification fold trail without resurrecting any V2-amendment commitment; the disposition column phrases routed deferrals as "tranche D body" (named receiver), not "V2 amendment" (open-ended).

## §3 Phase 8.4 PASS-1 Fold Closure (10 V8 items routed to PASS-1)

V8 surfaced 14 punch items at `HARDENING-PASS-1-V8.md:98-118`; 10 land in PASS-1 substrate (the remaining 4 land in PASS-2/PASS-3/MASTER-PLAN per V8 cohort `HARDENING-CONSOLIDATED-V8.md` §3). Each PASS-1-routed item must close at the path:line cited in the V8 punch list.

| V8 # | Tier | Surgery | PASS-1 surface | Path:line evidence | Status |
|---|---|---|---|---|---|
| α3 | I | Backend IR alphabet 22 → 19 (three pair-collapses) | `Alt` carries `mode: Dispatch \| Speculative`; `LayoutScope` carries `kind: Push \| Pop`; `CallHost` chains express as `Seq`-of-`CallHost`; PASS-1 cross-references ARCH §7.2 as authoritative | `PASS-1.md:41` ("19-variant alphabet ... three semantically-redundant pair-collapses"); `:346` ledger ("PASS-1 cross-references ARCH §7.2 as authoritative") | CLOSED |
| α4 | I | Grammar-IR `Map` + `HostCall` merge into `Call` with `kind: Map \| Host` | Single `Call` IR variant with discriminator field; schema-floor table merged; multi-function chaining semantics rewritten | `PASS-1.md:24` ("Call ... carries a kind: Map \| Host discriminator"); `:35` schema-floor row (`Call (kind: Map \| Host)`); `:141` (multi-function chaining: "each node carries kind: Map \| Host"); `:271` (chain typing: "Call { kind: Map }" / "Call { kind: Host }"); `:347` ledger | CLOSED |
| α6 | I | Generic validation 3-path → 2-path | "annotation OR rejection" two-path validation; structural-decreasing-argument detector routed to tranche D body | `PASS-1.md:93` ("recursive generic cycles require an explicit return annotation, or rejection ... The two-path validation (annotation OR rejection) is the V1 contract — structural-decreasing-argument detection ... is a tranche-D-body ergonomic refinement"); `:348` ledger | CLOSED |
| β1 | I | Numeric diagnostic aliases retired | Single-namespace alphabetic catalogue; rendering-layer rationale aligned to alphabetic-only; the prior numeric tags appear only in the §11 ledger row as deletion archaeology | `PASS-1.md:119` ("Codes are single-namespace alphabetic / mnemonic; the prior numeric-alias track retires"); `:166` (`error/codes/` rationale: "binds the single-namespace alphabetic codes (e.g. BBNF-LOOKBEHIND-WIDTH)"); catalogue at `:121-133` is alphabetic-only; `:349` ledger row lists `BBNF1004 / BBNF1201 / BBNF1302 / BBNF1401 / BBNF2103 / BBNF2104` as retired | CLOSED |
| γ1 | J | Closure capture leverage rustc borrow checker | rustc's borrow checker is the V1 Rust correctness gate; bbnf's `BBNF-CLOSURE-CAPTURE-BY-MOVE` is the grammar-author UX surface | `PASS-1.md:87` ("For V1 Rust the closure-capture-by-`&'i` rule is enforced by rustc's borrow checker on the generated source ... the parse-time `BBNF-CLOSURE-CAPTURE-BY-MOVE` diagnostic is the better-error UX surface"); `:263` ("rustc's borrow checker enforces the `&'i` invariant on the generated source"); `:350` ledger | CLOSED |
| γ2 | J | Match exhaustiveness leverage rustc | rustc's match-exhaustiveness check is the load-bearing correctness gate; bbnf's check localises against grammar-aware variant names | `PASS-1.md:89` ("The check is a layered cover: bbnf's codegen-time check fires against grammar-aware variant names (the better-error UX surface), and rustc's match-exhaustiveness check on the generated Rust source is the load-bearing correctness gate"); `:351` ledger | CLOSED |
| γ3 | J | thiserror + miette host-leverage | `error` crate binds `thiserror`-derived enum + `miette` span-attached pretty rendering; bbnf does not invent display machinery | `PASS-1.md:119` ("the `error` crate binds them to `thiserror`-derived enum variants ... routes them through `miette` for span-attached pretty rendering, so bbnf does not invent its own error-display machinery"); `:166` (`error` rationale row built atop `thiserror` + `miette`); `:352` ledger | CLOSED |
| γ8 | J | Generic monomorphisation leverage rustc | rustc monomorphises; bbnf's `(RuleId, TypeArgs)` validation surfaces `BBNF-GENERIC-CYCLE` as a better-error ahead of rustc's `recursion_limit` | `PASS-1.md:93` ("For V1 Rust the `(RuleId, TypeArgs)` instance set is the input to rustc's monomorphisation; bbnf's codegen-time validation surfaces `BBNF-GENERIC-CYCLE` as a better-error layer ahead of rustc's `recursion_limit` exhaustion, and rustc's monomorphisation termination is the load-bearing correctness gate"); `:353` ledger | CLOSED |
| γ9 | J | Function-arrow-unification leverage rustc HM | rustc's standard HM unification on the generated source is the load-bearing correctness gate; bbnf's pre-emission arrow unification fires for diagnostic localisation only | `PASS-1.md:73` ("for V1 Rust the bbnf-side arrow unification fires pre-emission for diagnostic localisation only, and rustc's standard HM unification on the generated source is the load-bearing correctness gate"); `:354` ledger | CLOSED |
| δ1 | δ | DK13 rank-N body → tranche D | rank-1 inference body is V1-load-bearing now; rank-N body lands at tranche D (D.W3 / D.W6 per MASTER-PLAN) with explicit `forall` annotation V1-day-one | `PASS-1.md:73` ("the rank-N inference body lands at tranche D (D.W3 / D.W6 per MASTER-PLAN), since no V1 seed grammar exercises rank-N — the V1 surface admits the explicit annotation on day one"); `:81` algorithm-layer table row ("active V1; rank-N body at tranche D"); `:355` ledger | CLOSED |
| δ2 | δ | Schema-miner telemetry → tranche D | HM-driven named-schema synthesis is V1-active; telemetry-driven refinement lands at tranche D body | `PASS-1.md:91` ("V1 named-schema synthesis is HM-shape-driven ... Telemetry-driven *refinement* of inferred schemas ... lands at tranche D body, since V1 has no runtime trace producer to feed the miner"); `:356` ledger | CLOSED |

10 of 10 PASS-1-routed V8 items closed at the path:line cited. Item I-1 (per-layer V1-active vs substrate-only legibility table) lands inline at `PASS-1.md:75-86` as a 7-row table; item I-4 (Leijen row-poly internal collapse) is preserved with explicit tranche-D-body deferral at `:93`; item I-5 (three-path collapse) closes at α6 above; item I-7 closes at α4 above; item I-8 closes at α3 above.

V8 punch-list cross-walk:

| V8 punch # | V8 surgery | V8.1 disposition | Status |
|---:|---|---|---|
| 1 | Insert per-layer V1-active vs substrate-only table after `:73` | landed at `:75-86` as 7-row table covering HM equality + Algorithm-W principal schemes / Pierce-Turner local check/synth / DK13 application judgment / finite first-order unification / finite CSP / CHR-improvement layer / OutsideIn(X)-style implication | CLOSED |
| 2 | I-2 + K-3: SIMPLIFY GADT substrate; route to V2 | OVERRIDDEN by user FOLD adjudication Q1; GADT V1 user-facing surface kept; `BBNF-LOCAL-EQUALITY-ANNOTATION` retained at `:129`; OutsideIn(X) implication V1-active at `:85` | CLOSED (FOLD overrides V8 SIMPLIFY) |
| 3 | I-3 + K-4: schema-mining miner CONSOLIDATE; telemetry to V2 | landed at `:91` as "HM-shape-driven" V1 named-schema synthesis; telemetry refinement routes to tranche D body | CLOSED |
| 4 | I-4: drop Leijen row-poly internal collapse | preserved as `passes::layout` subroutine at `:93` with user-facing surface routed to tranche D body when shapes are not both known | CLOSED (modified — substrate kept; user-facing only routed) |
| 5 | I-5: three-path generic-cycle validation → two-path | landed at `:93` ("two-path validation (annotation OR rejection) is the V1 contract") | CLOSED |
| 6 | I-6: drop numeric diagnostic codes | landed at `:119`, catalogue at `:121-133` alphabetic-only | CLOSED |
| 7 | I-7: Map + HostCall → Call (kind: Map \| Host) | landed at `:24`, `:35`, `:141`, `:271` | CLOSED |
| 8 | I-8: BIR pair-collapses → 19 variants | landed at `:41` referencing ARCH §7.2 as authoritative | CLOSED |
| 9 | J-1: closure-capture better-error framing | landed at `:87` + `:263` | CLOSED |
| 10 | J-2: generic-rule monomorphisation better-error framing | landed at `:93` | CLOSED |
| 11 | J-3: match-exhaustiveness better-error framing | landed at `:89` | CLOSED |
| 12 | J-4: FnType first-order unification better-error framing | landed at `:73` ("rustc's standard HM unification on the generated source is the load-bearing correctness gate") | CLOSED |
| 13 | J-6: Fn/FnMut/FnOnce collapse Rust-aware origin cite | absorbed at `:87` ("the `Fn`/`FnMut`/`FnOnce` discrimination Rust exposes is collapsed at the BBNF surface") | CLOSED |
| 14 | J-7: thiserror as V1 Rust error-vocabulary host facility | landed at `:119` + `:166` | CLOSED |

14 of 14 V8 punch items dispositioned. Item 2 carries the user FOLD override (Q1 adjudication takes precedence over V8 SIMPLIFY recommendation, per `restart/research/CORPUS-AUDIT-SYNTHESIS.md:210`). Item 4 carries a modified-fold disposition (V1 substrate kept; user-facing surface routed). All other items absorbed verbatim per V8 surgery direction.

## §4 Compressed Nine-Lane Verification (V8 carry forward)

V7.1 → V8 SIMPLIFY-AVAILABLE → V8.1 verification rerun. Each lane verifies V8 SIMPLIFY-AVAILABLE survived the Phase 8.4 fold without regression.

| # | Lane | V8 verdict | V8.1 verification | V8.1 verdict |
|---:|---|---|---|---|
| 1 | Lock-Adherence (Lock 4 — DK13 + GADT V1 surface + closure-by-`&'i`) | READY (with simplification opportunity I-2) | Phase 8.3.1 retained Lock 4's GADT V1 user-facing surface + `Pattern @ where T = U` refinement at `restart/locks/LOCKS.md:40`; PASS-1 surfaces `BBNF-LOCAL-EQUALITY-ANNOTATION` at `:129` + `Refinement`/`Equality` non-terminals at `:244-246`; the I-2 substrate-deletion alternative was overridden by user FOLD adjudication (Q1) | **READY** |
| 2 | Lock-Adherence (Lock 5 — Backend trait + per-backend lowering) | READY | PASS-1 references ARCH §7.5 RustBackend impl at `:61`; per-backend obligations table at `:63-71` preserved with both Rust V1 + WASM V1 columns; cohort SYNTHESIS landed Backend trait 5→2 method collapse | **READY** |
| 3 | Lock-Adherence (Lock 10 — 6-directive + function values + lambda + closure-by-`&'i`) | READY | PASS-1 §6 production at `:216-222` carries the six directives (`ImportDecl \| HostFn \| ErrorDecl \| LayoutDecl \| PrettyDecl \| TokenDecl`); function values + `FnType` at `:255`; `LambdaExpr` at `:242`; `:261` ("six-directive `Directive` production above is the complete V1 surface") | **READY** |
| 4 | Lock-Adherence (Lock 11 — parse-that + parse-that-regex naming canon) | READY | PASS-1 cites `parse-that` at `:158` per-crate tree row; `parse-that-regex` not surfaced (regex sub-crate is a follow-up commit per Lock 11 amendment); naming canon honoured | **READY** |
| 5 | Sequencing-Discipline | N/A | PASS-1 is a substrate synthesis, not a multi-wave plan | **N/A** |
| 6 | Cohesion (HM + DK13 + CSP composition + IR alphabet) | READY (cohesion strictly improves under I-1) | Per-layer V1-active/substrate-only table landed at `:75-86`; algorithmic frame at `:73` preserved; Grammar-IR merge α4 collapses redundant variant pair into one with discriminator (cohesion improves); BIR pair-collapse α3 likewise | **READY** |
| 7 | SOTA-Anchoring | READY | PASS-1 carries no parse-throughput numerics; SOTA-anchoring is master-plan / tranche-H concern; unchanged | **READY** |
| 8 | Grammar-Authoritative-Discipline (Lock 14) | READY | Rare escape-valve fence at `:104-115` preserved; "Extant grammars" table empty; the verification command at `:114` preserved; per-grammar declaration crates remain not-default | **READY** |
| 9 | Generated-Code-Budget | READY (improves under I-7 + I-8) | Grammar-IR variant 15→14 (α4 collapse); BIR variant alphabet 22→19 (α3 collapse); generated match-arm cardinality reduces in `passes/extract/`, lowerers, vm/program/ | **READY** |
| 10 | Friction-Forecast | READY (improves under I-6 + γ-cohort) | Diagnostic catalogue single-namespace alphabetic-only (β1 / I-6); host-leverage framing makes the parse-time check explicitly the "better-error UX layer" (γ1, γ2, γ3, γ8, γ9); friction strictly improves | **READY** |
| 11 | Carry-Deferral (V2 / tranche-D receivers) | READY (new carries fully named) | δ1 rank-N body → tranche D (D.W3/D.W6); δ2 telemetry refinement → tranche D body; δ-residue (capture-by-move + Fn* trait split + or-patterns + match guards) → tranche D body; **zero open-ended "V2 amendment"** routes; every deferral names a tranche receiver | **READY** |
| 12 | Greenfield-Discipline | READY (sharpens under V8 simplifications) | The V8 cohort excised contrivance; Phase 8.4 absorbed the punch list without retracting locks; PASS-1 §11 ledger records the trail without metalanguage; honours grammar-authoritative + meta-grammar discipline | **READY** |
| 13 | LLM-pathology lenses F + G + H | READY | Phase 8.4 fold preserved calibrated, direct prose; path:line citations on every concrete claim; no hedging, no reference-stuffing, no hallucinated cites; §11 ledger names dispositions concretely | **READY** |

12 of 13 lanes READY (1 N/A); zero regressions. The four lanes that V8 marked as "improves under simplification" (6, 9, 10, 12) confirm the improvement landed in PASS-1.

V8 lens-cohort verification (Lens I + Lens J + Lens K rows must survive Phase 8.4 fold without verdict regression):

| V8 lens | V8 row count | V8 verdict distribution | V8.1 verification | V8.1 verdict |
|---|---:|---|---|---|
| Lens I (Contrivance) | 8 rows (I-1 ... I-8) | 0 KEEP / 2 SIMPLIFY / 6 CONSOLIDATE | I-1 table landed; I-2 OVERRIDDEN by Q1 FOLD; I-3 schema-mining consolidated to HM-driven; I-4 modified-fold (substrate kept; user surface routed); I-5/I-6/I-7/I-8 absorbed verbatim | survived; one user override (I-2) preserves GADT V1 user surface |
| Lens J (Host-leverage) | 7 rows (J-1 ... J-7) | 1 LEVERAGE / 5 HYBRID / 1 KEEP | J-1 closure-capture better-error landed `:87`; J-2 generic-rule monomorphisation `:93`; J-3 match-exhaustiveness `:89`; J-4 FnType first-order unification `:73`; J-5 obligations table preserved `:63-71`; J-6 Fn*/FnMut/FnOnce collapse cited `:87`; J-7 thiserror + miette `:119`, `:166` | survived; every HYBRID row absorbed; J-5 KEEP preserved |
| Lens K (Meta-grammar) | 9 rows (K-1 ... K-9) | 5 LOAD-BEARING / 3 ASPIRATIONAL / 1 SPECULATIVE | K-1 (HM equality) load-bearing; K-2 (DK13 rank-N) ASPIRATIONAL → tranche D D.W3/D.W6; K-3 (GADT) Q1 FOLD overrides SPECULATIVE → V1 user-facing surface; K-4 (schema-miner telemetry) ASPIRATIONAL → tranche D body; K-5 (CHR-improvement) Q2 FOLD overrides ASPIRATIONAL → V1-load-bearing; K-6/K-7/K-8/K-9 load-bearing preserved | survived; two user overrides (K-3 + K-5) elevate ASPIRATIONAL/SPECULATIVE to V1-active per Q1/Q2 |

V8 lens-cohort: 24 rows; 24 dispositioned; 0 regressions. The two user overrides (K-3 GADT + K-5 CHR) elevate ASPIRATIONAL/SPECULATIVE to V1-active per the Phase 8.3.1 user adjudications and are reflected accurately in PASS-1's V1-active layer table at `:75-86`.

## §5 V2-Amendment Retirement Ledger

The user mandate at `restart/research/CORPUS-AUDIT-SYNTHESIS.md` §4 retires open-ended "V2 amendment" / "deferred to V2" / "post-V1 amendment" phrasing in favour of named tranche receivers. PASS-1 must contain zero active V2-deferral commitments; only V1-affirmation negations and §11 ledger archaeology are admissible.

`rg -n 'V2 amendment\|V2 deferral\|deferred to V2\|post-V1 amendment' restart/audit/pass-1-substrate/PASS-1.md` audit:

| Match site | Phrasing | Class |
|---|---|---|
| `PASS-1.md:73` | "Higher-rank polymorphism is a V1 surface, not a future amendment" | V1-affirmation negation |
| `PASS-1.md:73` | "the layer ships V1 and is not reserved for V2 amendment" | V1-affirmation negation |
| `PASS-1.md:357` | ledger row "in lieu of 'V2 amendment' phrasing" | §11 deletion archaeology |

Three matches; zero active V2-deferral commitments. Every deferral in PASS-1 names a concrete receiver:

| Deferred item | Receiver | Path:line |
|---|---|---|
| Rank-N inference body | tranche D (D.W3 / D.W6) | `PASS-1.md:73`, `:81`, `:355` |
| Schema-miner telemetry refinement | tranche D body | `PASS-1.md:91`, `:356` |
| Or-patterns + match guards | tranche D body | `PASS-1.md:89`, `:265` |
| Capture-by-move + `Fn*` trait split | tranche D body | `PASS-1.md:263`, `:357` |
| Structural-decreasing-argument detection | tranche D body | `PASS-1.md:93`, `:348` |
| User-facing row-poly surface (Leijen) | tranche D body | `PASS-1.md:93` |

V2-amendment retirement: **CLOSED**. Every deferral routes to a tranche-named receiver; no open-ended "V2" lives.

Comparison with V8 baseline: V8 PASS-1 carried `BBNF-LOCAL-EQUALITY-ANNOTATION` substrate language at `HARDENING-PASS-1-V8.md:81` cited as "V1 surface absent" + Phase 7.5B reservation-only. Phase 8.3.1 Q1 user adjudication FOLDED GADT V1 user-facing surface; PASS-1 now carries the full V1-active surface (match-arm refinement + OutsideIn(X) implication propagation + diagnostic emission). V8.1 verifies the FOLD eliminated the prior "substrate-without-surface" contrivance signal: every V1 algorithmic-stack layer at `:75-86` now has a named V1 user surface (HM equality / Algorithm-W principal schemes / Pierce-Turner / DK13 / first-order unification / CSP / CHR-improvement / OutsideIn(X) implication — eight-row table; column 2 names V1 user surface for each row).

The receiver-naming discipline holds across PASS-1's deferred items:

- **D.W3 / D.W6** (rank-N inference body): named in MASTER-PLAN tranche D wave structure; PASS-1 cites the wave specifically per `:73`.
- **tranche D body** (telemetry refinement / or-patterns / match guards / capture-by-move / structural-decreasing-argument detection / row-poly user-facing surface): named tranche; PASS-1 routes to body-execution within tranche D's type-system tranche.

No "future tranche", no "later", no "post-V1" without a named receiver. The HARDENING.md Lane 8 (Carry-Deferral) discipline holds.

## §6 Cross-Target Cohort Coherence

PASS-1 cross-references the sibling targets (PASS-2, PASS-3, ARCHITECTURE, MASTER-PLAN). Phase 8.4 cohort SYNTHESIS at commit `e5cb1e4b` landed three architectural cardinality reductions: Backend trait 5 → 2 methods (α1); type-system stack 7 → 5 mechanisms (α2 — collapses Algorithm-W + HM equality + first-order unification into one algorithm); BIR alphabet 22 → 19 (α3); rewrite-budget categories 4 → 3 (α5). PASS-1 must reference the post-fold state.

| Cross-reference | Authoritative source | PASS-1 surface | Path:line | Coherent? |
|---|---|---|---|---|
| BIR alphabet 22 → 19 | ARCH §7.2 | PASS-1 carries the listing for substrate-side cross-reference; names the 19-variant alphabet with three pair-collapses (Alt-mode, LayoutScope-kind, CallHost-chain-as-Seq); explicitly cites "ARCH §7.2 owns the authoritative variant set" | `PASS-1.md:41` | YES |
| Backend trait V1 impl | ARCH §7.5 | PASS-1's per-backend obligations table is consumed by the V1 `RustBackend: Backend` impl per ARCH §7.5; cites `restart/ARCHITECTURE.md:1067-1144` | `PASS-1.md:61` | YES |
| 6-directive grammar | Lock 10 (`14-LOCKS.md:52`) | PASS-1 §6 production carries `ImportDecl \| HostFn \| ErrorDecl \| LayoutDecl \| PrettyDecl \| TokenDecl`; `:261` cites Lock 10 amendment + `directive-canon` lint at ARCH §13.1 | `PASS-1.md:216-222`, `:261` | YES |
| Type-system algorithm layer | PASS-1 §3 (own surface) | Per-layer V1-active vs substrate-only legibility table at `:75-86`; references DK13 (Dunfield-Krishnaswami 2013), Pottier-Rémy first-order unification (Milner 1978), Vytiniotis et al. 2011 OutsideIn(X); Lock 4 + Lock 11 cross-references intact | `PASS-1.md:73`, `:75-86` | YES |
| Function values + `FnType` + lambda | Lock 10 (`14-LOCKS.md:52`) | PASS-1 `:87` cites Lock 10 amendment Phase 7.1; `Type` admits `FnType = "fn" "(" TypeList? ")" "->" Type` at §6 grammar `:255` | `PASS-1.md:87`, `:255` | YES |
| Closure capture by `&'i` | Lock 4 (`14-LOCKS.md:40`) | PASS-1 `:87` cites Lock 4 amendment Phase 7.1; `BBNF-CLOSURE-CAPTURE-BY-MOVE` diagnostic at `:131`; `:263` reaffirms | `PASS-1.md:87`, `:131`, `:263` | YES |
| `csp-solver` Lock 11 publication coupling | Lock 11 (`14-LOCKS.md:54`) | PASS-1 `:73` cites: "the published-once-stable sister crate per Lock 11" couples CHR-improvement layer publication state | `PASS-1.md:73` | YES |
| Wave routing D.W3 / D.W6 | MASTER-PLAN | PASS-1 `:73` cites "tranche D (D.W3 / D.W6 per MASTER-PLAN)" | `PASS-1.md:73` | YES |
| `directive-canon` lint at ARCH §13.1 | ARCHITECTURE | PASS-1 `:261` cites "The `directive-canon` lint at ARCHITECTURE §13.1 enforces this set" | `PASS-1.md:261` | YES |

All cross-references coherent with the post-fold state. PASS-1 names ARCH §7.2 + ARCH §7.5 + ARCH §13.1 + Lock 4 + Lock 10 + Lock 11 + MASTER-PLAN as authoritative-elsewhere; the substrate substrate-side claims do not contradict the SYNTHESIS-fold edits at `e5cb1e4b`.

PASS-1's substrate-side claims (Grammar IR shape, type-system algorithm, schema-floor table, per-backend obligations, error vocabulary, BBNF formal grammar) cohere with the post-fold authoritative surfaces:

| PASS-1 own claim | Cited authority | Post-fold consistency |
|---|---|---|
| Grammar IR variant alphabet (14 variants — `Rule`, `Seq`, `Alt`, `Repeat`, `Optional`, `Literal`, `Regex`, `Ref`, `Predicate`, `Lookbehind`, `Call`, `LayoutDirective`, `ErrorDirective`, `Annotation`) | PASS-1 substrate-side cross-reference; ARCH §7.1 owns alphabet | consistent — α4 Map+HostCall merge collapses prior 15 to 14 |
| Backend IR variant alphabet (19 variants) | ARCH §7.2 | consistent — α3 BIR pair-collapse collapses prior 22 to 19; PASS-1 explicitly cites ARCH §7.2 as authoritative |
| Type-system algorithm composition (HM equality + Algorithm-W + Pierce-Turner check/synth + DK13 + finite first-order unification + finite CSP + CHR-improvement + OutsideIn(X) implication) | PASS-1 substrate-side; ARCH §8.2 owns layered description | consistent — α2 SYNTHESIS-side stack-collapse 7→5 acknowledges Algorithm-W + HM equality + first-order unification co-mention as one algorithm; PASS-1's prose preserves all three terms because each cites a distinct primary source (Damas-Milner 1982; Pierce 2002; Pottier-Rémy first-order); the V1-active vs substrate-only table at `:75-86` clarifies the substantive composition |
| 6-directive grammar surface (`@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`) | Lock 10 (`14-LOCKS.md:52`) | consistent — every directive named in §6 EBNF productions at `:217-223`; retired directives `@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode` enumerated as retired at `:261` |
| `Refinement = "@" "where" Equality { "," Equality }` non-terminal | PASS-1 §6 grammar (own surface); Lock 4 GADT V1 user-facing | consistent — Phase 8.3.1 Q1 FOLD landed; production at `:245`; cross-references the §3 type-system algorithm at `:93` and `:265` |
| `LambdaExpr = "\|" Params? "\|" ( Expr \| Block )` non-terminal | PASS-1 §6 grammar; Lock 10 function-values amendment | consistent — production at `:242`; cross-references closure-by-`&'i` rule at `:263` |
| Diagnostic catalogue 11 alphabetic codes | PASS-1 §2 own surface; β1 retires numeric aliases | consistent — catalogue at `:121-133`; rendering rationale at `:166` references alphabetic codes only |
| Recovery facts producer-side | PASS-1 §2; PASS-3 consumer-side at G/I tranches | consistent — `RecoveryFacts` keyed by `RuleId`/`NodeId` at `:137`; PASS-3 consumer named in §5 hand-off at `:204` |
| Per-grammar declaration crate fence | Lock 14 (`14-LOCKS.md:60`) | consistent — eight-field rare-fence at `:104-115`; Extant grammars row empty; verification command at `:114` enforces zero per-grammar imports |

The substrate-side claims do not regress the SYNTHESIS-side edits; PASS-1 is the substrate that the SYNTHESIS targets cross-reference.

## §7 Final Verdict

**Decision: READY.**

V8.1 verifies that:

1. Phase 8.3.1 corpus cleanup (Q1 GADT V1 fold; Q2 CHR V1 fold; Q3 function composition library deletion) landed coherently in PASS-1 with zero active V2-amendment residue.
2. Phase 8.4 PASS-1 fold closed all 10 PASS-1-routed V8 punch items at the path:line cited (α3 / α4 / α6 / β1 / γ1 / γ2 / γ3 / γ8 / γ9 / δ1 / δ2 — 10 items closed; the residual punch items 7, 11, 13 fold inline as I-1 layer table / J-5 obligations table preservation / J-6 framing-only at `:75-86`, `:63-71`, `:87`).
3. The 13 compressed-mode lanes carry V8 SIMPLIFY-AVAILABLE forward without regression; lanes 6 / 9 / 10 / 12 strictly improve under the fold.
4. V2-amendment retirement is total: zero active V2-deferral commitments; six deferred items each name a tranche-D-body receiver.
5. Cross-target cohort coherence holds: BIR 22→19, Backend trait via ARCH §7.5, 6-directive grammar via Lock 10, function values + closures via Lock 4 + Lock 10 — all named, cross-referenced, coherent.

The corpus is **READY for Wave 9 per-tranche full-spec drafting**. PASS-1 is the substrate; the substrate now carries one canonical algorithmic frame with the V1-active vs substrate-only boundary explicit, one merged Grammar-IR `Call` variant, one alphabetic diagnostic catalogue, and one set of host-leverage framings that route to rustc as the V1 Rust correctness gate. No re-draft. No amendment. No further punch list to absorb against PASS-1.

V8 SIMPLIFY-AVAILABLE → Phase 8.4 fold landed → V8.1 READY.

## §8 Closing Posture

V8.1 is the verification audit. V8 surfaced the 14 simplification candidates; Phase 8.3.1 + Phase 8.4 absorbed every PASS-1-routed candidate (Q1-Q3 cleanup; α3, α4, α6, β1, γ1, γ2, γ3, γ8, γ9, δ1, δ2 fold). The substrate stands at 360 lines (the V8 baseline was substantively unchanged in line-count by the fold; Phase 8.4 added §11 ledger + per-layer table + tranche-routing clauses while consolidating six other clauses, net-neutral). The 14 architectural locks hold. The 6-directive grammar holds. The Backend trait per Lock 5 holds. DK13 + GADT V1 user-facing surface + closure-by-`&'i` per Lock 4 holds. parse-that + parse-that-regex naming canon per Lock 11 holds.

The substrate is calibrated, direct, host-leverage-aware, and tranche-routed. Per-tranche full-spec drafting (Wave 9+) unblocks at PASS-1's V8.1 READY substrate. The next sharpening lives at the wave level, not the substrate level.

Hereupon Wave 9 proceeds with PASS-1 V8.1 READY as the substrate-side baseline; V8.1-PASS-2, V8.1-PASS-3, V8.1-MASTER-PLAN run in parallel and the consolidated cohort verdict synthesises at the V8.1-CONSOLIDATED level.
