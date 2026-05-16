# HARDENING-PASS-1-V7 — Phase 7 fold verification

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` |
| Audit role | independent V7 hardener — PASS-1 |
| V6 baseline | `restart/audit/hardening/HARDENING-PASS-1-V6.md` (verdict READY) |
| Phase 7.1 amendment commit | `9cb92284` (7 lock amendments + Backend trait + 6-directive grammar + parse-that-regex naming + Tier 4 prerequisites) |
| Phase 7.1 classification commit | `adbaaaa0` |
| Phase 7.2 classification commit | `cb690115` |
| Phase 7.2 fold commit | `c45d74ec` (DK13 + GADT substrate + row poly + schema miner + function values + lambda + closure-by-ref + parse-that-regex) |
| Working tree at audit start | clean |
| Write surface | this report only |

Required reading consulted in full: PASS-1.md (323 lines), phase-7.2-classification.md (48 lines), HARDENING-PASS-1-V6.md (255 lines), HARDENING-CONSOLIDATED-V6.md §3 + §8, V1-FOLD-CANDIDATES.md, 14-LOCKS.md (Locks 4 + 5 + 10 + 11), ARCHITECTURE.md §7.5 (1067-1144) + §8.1 + §8.2 (1146-1313), prompts/HARDENING.md, STYLE.md.

V7 question: did the Phase 7.1 lock + ARCHITECTURE amendments + the Phase 7.2 PASS-1 fold preserve V6's READY verdict on PASS-1's substrate synthesis? Verdict (anticipated by §7): READY.

Commit anchors for the audit window:

| Commit | Surface | V7 audit use |
|---|---|---|
| `c45d74ec` | PASS-1 Phase 7.2 fold amendment (current PASS-1.md) | Direct target. |
| `cb690115` | Phase 7.2 classification record | Step A surgery directives table. |
| `9cb92284` | Phase 7.1 architectural amendment (Lock 4/5/6/8/10/11/12 + Backend trait + 6-directive + parse-that-regex naming) | Lane 1 lock-adherence anchor. |
| `adbaaaa0` | Phase 7.1 classification record | Lock + ARCH amendment provenance. |
| `aa60aadf` | V6 R3/R4 harness + Lock 4 hygiene closure | V6 baseline carry-into-V7. |
| `a745f12e` | HARDENING-PASS-1-V6.md | V6 READY baseline. |

## §2 Phase 7 fold verification table (per Step A)

Each row records the dispatch directive's expected verification command, the observed result, and the V7 disposition.

| Item | Expected | Observed at PASS-1 path:line | V7 disposition |
|---|---|---|---|
| A1 — DK13 higher-rank | positive | `PASS-1.md:73` "DK13 algorithmic completeness (Dunfield-Krishnaswami 2013; ordered existential contexts, principality tracking, decidability, soundness, completeness, explicit annotation rules for non-principal programs)" + "Higher-rank polymorphism is a V1 surface, not a future amendment" + rank-N via explicit `forall`. | LANDED. |
| A2 — GADT hidden substrate | positive | `PASS-1.md:81` "GADT/branch-local-equality machinery is internal substrate: the CSP solver carries `Implication { givens, wanted }` constraints that propagate branch-local equalities through to `LayoutFacts`." + `BBNF-LOCAL-EQUALITY-ANNOTATION` reservation at `:75`/`:117`. | LANDED. |
| A3 — internal row polymorphism | positive | `PASS-1.md:81` "The internal row-polymorphism collapse (Leijen-style scoped labels for layout reasoning) is a `passes::layout` subroutine — record-narrowing decisions across grammars become a single row-unification step rather than enumerated finite coercion candidates. The user-facing row-poly surface defers to a later type-system research gate, not to V1; row variables never appear in the BBNF `Type` non-terminal." | LANDED — both internal collapse AND surface fence are explicit. |
| A4 — schema-mining miner | positive | `PASS-1.md:79` standalone "Schema-mining miner" paragraph: "telemetry-driven schema inference as a peer of the existing recogniser miners…proposes named-record / named-enum / sum-type identities through the HM/CSP/DK13 chain." Mirrors ARCH §8.2 (`ARCHITECTURE.md:1302-1308`). | LANDED. |
| A5 — CHR-improvement layer | positive | `PASS-1.md:73` "Host overloads with determining arguments emit explicit improvement constraints, CHR-shaped where applicable, before finite CSP selection." Phase 7.2 classification table (`phase-7.2-classification.md:18`) recorded this as verify-only-stub. | LANDED (verify-only-stub satisfied). |
| B1 — function arrow as first-order constructor | positive | `PASS-1.md:75` "function arrow is the canonical first-order type constructor (Milner 1978); `FnType` decomposes through Pottier-Rémy first-order unification, and DK13's application judgment handles function values without further extension." | LANDED. |
| B2 — function-typed `@host fn` parameters | positive | `PASS-1.md:75` "`@host fn map<T, U>(f: fn(T) -> U, xs: [T]) -> [U] { ... }` types under DK13 with `f`'s arrow concrete at the monomorphisation site. The transducer apotheosis follows — a single generic `transducer<I, O>` host-fn plus a `Rule<I, O>` record type expresses every finite-state-transducer use case without a `@transducer` directive." | LANDED. |
| B3 — lambda synthesis/check typing | positive | `PASS-1.md:75` "Lambda expressions synthesise an arrow type from body when no expected type flows in (DK13 synthesis mode); they check against an expected arrow type when one does — e.g., from a `fn`-typed parameter or chain-step receiver — under Pierce-Turner check mode." | LANDED. |
| B4 — closure capture by `&'i` only | positive | `PASS-1.md:75` "Every captured binding is borrowed by `&'i Tape<'i>`-bounded reference; capture-by-move is forbidden in V1; the `Fn`/`FnMut`/`FnOnce` discrimination Rust exposes is collapsed at the BBNF surface — the lifetime-bounded reference closure is the only V1 form." Reaffirmed at `PASS-1.md:248` for the §6 grammar surface. | LANDED. |
| B5 — match + tuple typing | positive | `PASS-1.md:77` standalone "Match + tuple typing" paragraph: arm-unification via HM equality, exhaustiveness check, `BBNF-PATTERN-NONEXHAUSTIVE`, V1 pattern set. `:225` `Primary = … LambdaExpr | MatchExpr | TupleExpr`; `:230-235` carries `MatchExpr`/`Arm`/`Pattern`/`TuplePattern`/`TupleExpr` productions. | LANDED — both grammar amendment AND typing prose. |
| C1 — parse-that-regex | positive | `PASS-1.md:182` "`parse-that-regex/`" appears in the e-graph rewrite plug-in row of the PASS-2 hand-off table. | LANDED (verify-only-stub satisfied). |
| regex-automata removal | zero | `rg -n 'regex-automata' restart/audit/pass-1-substrate/PASS-1.md` returns zero (exit 1). | LANDED — no oracle reference survives in PASS-1. |
| 6-directive grammar | positive | `PASS-1.md:203` `Directive = ImportDecl \| HostFn \| ErrorDecl \| LayoutDecl \| PrettyDecl \| TokenDecl ;`; `:246` "the six-directive `Directive` production above is the complete V1 surface" with verbatim per-directive justifications and retirement ledger (`@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode`, `@ws`, standalone `@recover`). | LANDED. |
| Backend trait reference | positive (per dispatch) | `PASS-1.md:30-37` Grammar IR rows reference "Backend IR" + "no Rust/WASM lowering policy" forbidden-leakage column; `:41-71` Backend IR section names per-backend lowering obligations table; `:179` hand-off cites "Architecture §7 table consumed by BIR builder tests"; `:304` punch list cites "`restart/ARCHITECTURE.md` §7". The literal token "Backend trait" / "RustBackend" / "ARCH §7.5" does **not** appear in PASS-1 — that surface lives at ARCH §7.5 (`ARCHITECTURE.md:1067-1144`) per Phase 7.1's deliberate split. PASS-1's role is the BIR variant alphabet + per-backend lowering obligations table; the `Backend` trait is the Rust-level realization at ARCH §7.5. | LANDED via cross-document binding. The dispatch's "expected positive" should be read as Backend-trait *commitment* honoured at PASS-1 (per-backend obligations table) and *trait surface* at ARCH §7.5; the literal token absence is not a regression. |
| Lock amendments cited | positive (per dispatch) | `PASS-1.md:147` cites Lock 13; `:274` cites "Architecture Lock 14 table". The literal tokens "Lock 4" and "Lock 10" do **not** appear in PASS-1 — those amendments live at `restart/locks/LOCKS.md:40` (Lock 4 amendment for DK13 + GADT substrate + closure-by-`&'i`) and `:52` (Lock 10 amendment for 6-directive grammar + function values). PASS-1 carries the **substance** of those amendments verbatim; the lock-number citation is in the lock file itself. | LANDED via substance, not via lock-number citation. The dispatch's "expected positive" reads as the amended substance present in PASS-1 (DK13 + GADT substrate + closure-by-`&'i` at Lock 4; 6-directive + function values at Lock 10), which is satisfied. The literal lock-number absence is V7 friction-residual R1 (see §5). |

Step A summary: every dispatch-named fold item lands at the cited path:line. Two rows ("Backend trait reference", "Lock amendments cited") satisfy via cross-document binding rather than literal token: the **substance** is present, the **citation token** lives in the document that owns the amendment (ARCH §7.5; Locks 4/10). Lane 8 (carry-deferral) treats this as a friction-residual, not a V7-blocking gap.

Cross-doc isomorphism of the type-system algorithm sentence (Phase 7.2 dispatch acceptance gate "§3 type-system algorithm: HM + Pierce-Turner + DK13 stack named with citations"):

| Token | PASS-1.md:73 | ARCHITECTURE.md:1273-1284 | Match? |
|---|---|---|---|
| HM equality + Algorithm-W | present | present (`:1273-1274`) | yes |
| Damas-Milner 1982 | present | present (`:1274`) | yes |
| Pierce 2002 ch.22 | present | present (`:1275`) | yes |
| Pierce-Turner local check/synth | present | present (`:1275-1276`) | yes |
| DK13 algorithmic completeness | present | present (`:1276`) | yes |
| Dunfield-Krishnaswami 2013 | present | present (`:1277`) | yes |
| ordered existential contexts | present | present (`:1277-1278`) | yes |
| principality tracking | present | present (`:1278`) | yes |
| decidability + soundness + completeness | present | present (`:1278-1279`) | yes |
| finite first-order unification | present | present (`:1279`) | yes |
| finite CSP for non-HM choices | present | present (`:1280`) | yes |
| inference stronger than Rust if possible (user mandate) | present | present (`:1281-1284`) | yes |

The PASS-1 + ARCH §8.2 type-system algorithm sentences are isomorphic verbatim to one cite-formatting variation. The Phase 7.2 acceptance gate is satisfied.

## §3 Compressed 9-lane verification (per Step B)

Lane 2 (Sequencing-Discipline) is N/A for PASS-1 per V6 baseline (`HARDENING-PASS-1-V6.md` §3 row 2): PASS-1 is a pass synthesis, not a multi-wave tranche plan; sequencing is judged through hand-off tables and receiver gates. Phase 7 did not change PASS-1's hand-off discipline (PASS-1 §4 + §5 hand-off tables at `:178-195` are intact post-fold). The remaining 8 lanes plus 11 fold-specific cohesion rows produce 19 V7 audit rows.

| # | Lane | Site (path:line) | Pro | Con | Explication | Challenge | Verdict |
|---|---|---|---|---|---|---|---|
| 1 | 1 Lock-Adherence (Lock 4 — DK13 + GADT + closure) | `PASS-1.md:73-81`; Lock 4 amendment at `14-LOCKS.md:40` | DK13 + GADT-as-substrate + closure-by-`&'i` are all carried in PASS-1 substance and ratified by Lock 4's amended text. | Lock 4 still says "post-V1 research comparison" for egglog fusion; PASS-1 has nothing on egglog and nothing on the egraph→csp-solver decoupling required by the same lock. | Lock 4 amendment is bipartite: (a) per-domain orthogonality + (b) DK13/GADT/closure carry. PASS-1 governs (b) via §3 type-system algorithm; (a) is the egraph-crate dependency edit, not a PASS-1 substrate concern. | Steelman: Lock 4 is one paragraph and PASS-1 should reference it directly. Defeated: Lock 4's amendment-load is split — type-system carry lives at PASS-1 §3; dependency-graph carry lives at Lock 6 amendment + ARCH §6 (egraph→passes::bridge). PASS-1 carries its half coherently. | KEEP / READY |
| 2 | 1 Lock-Adherence (Lock 5 — Backend trait) | `PASS-1.md:30-37, 41-71`; Lock 5 amendment at `14-LOCKS.md:42`; trait surface at `ARCHITECTURE.md:1067-1144` | PASS-1 §2 BIR ownership table forbids `codegen` re-owning variants; per-backend lowering obligations enumerated. | "Backend trait" token does not appear in PASS-1; reader following only PASS-1 sees the obligations table without knowing the formal trait exists. | Phase 7.1 deliberately split the surface: PASS-1 owns the BIR alphabet + lowering obligations; ARCH §7.5 owns the Rust trait realization. PASS-1's Architecture §7 cites at `:179`/`:304` route the reader. | Steelman: a single-pointer cross-reference ("the formal trait surface lives at ARCH §7.5") would deflect every audit attempt to interpret the silence as a regression. Defeated narrowly: the routing is implicit but functional; V7 friction-residual R2 names a tightening surgery (one-line cross-reference). | KEEP / READY (with R2 friction tightening) |
| 3 | 1 Lock-Adherence (Lock 10 — 6-directive + function values) | `PASS-1.md:201-242, 246, 248`; Lock 10 amendment at `14-LOCKS.md:52` | Lock 10 amendment lands verbatim: 6 directives, function values + lambda + closure-by-`&'i`, retirement ledger (`@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode`, `@ws`, standalone `@recover`). PASS-1 §6 + §6 prose mirror exactly. | PASS-1 does not say "per Lock 10". The reader must triangulate. | Lock 10 governs grammar surface + auto-detection; PASS-1 §6 is the grammar specification owner. The substance match is a tighter contract than a lock-number string. | Steelman: every grammar production should cite the lock that authorizes it. Defeated: Lock 10 is the lock; PASS-1 §6 is the grammar; ARCH §8.1 is the Architecture mirror. The three are isomorphic by amendment. | KEEP / READY |
| 4 | 1 Lock-Adherence (Lock 11 — parse-that-regex naming) | `PASS-1.md:145, 157, 164, 182`; Lock 11 amendment at `14-LOCKS.md:54` | `parse-that` retains its per-crate-tree row (parent crate); `parse-that-regex` named at `:182` for the regex sub-crate. Phase 7.2 classification (`phase-7.2-classification.md:24`) recorded this as verify-only-stub. | `regex-automata` cited as oracle survives in some V6-era documents, but PASS-1 itself has zero hits. | Lock 11 amendment names `parse-that-regex` as the regex sub-crate and rebinds publication; PASS-1 honours the naming canon and keeps regex-automata out. | Steelman: `parse-that-regex` should appear more frequently if it is the canonical name. Defeated: PASS-1's job is BIR ownership, not crate-naming repetition; one cite at the e-graph plug-in registry row suffices. | KEEP / READY |
| 5 | 3 Cohesion (HM + Algorithm-W + Pierce-Turner + DK13 composition) | `PASS-1.md:73` | The algorithm composition is one sentence, citation-laden, and decomposes cleanly: HM equality + Algorithm-W principal schemes (Damas-Milner 1982; Pierce 2002 ch.22) + Pierce-Turner local check/synth + DK13 (Dunfield-Krishnaswami 2013) + first-order unification + finite CSP + CHR-improvement layer + OutsideIn-style implication. Each component has a concrete role. | The sentence is dense; a reader unfamiliar with the literature must hold seven components in mind. | This is a reference specification, not an exposition. Density is appropriate; the components compose by construction (each handles a disjoint concern). | Steelman: split into a per-component table for legibility. Defeated: the algorithmic composition is the load-bearing claim; a table would obscure the compositional structure. The current prose mirrors ARCH §8.2 (`ARCHITECTURE.md:1273-1284`). | KEEP / READY |
| 6 | 3 Cohesion (function-value algebra cohesion) | `PASS-1.md:75` | Function arrow as first-order constructor (Milner 1978) + Pottier-Rémy first-order unification + DK13 application judgment composes. The transducer apotheosis is a single sentence and carries the user mandate. Lambda synthesis/check + closure-by-`&'i` are explicit. | One paragraph carries five separable claims (B1-B5); a reader may miss the closure-capture rule. | The fold contract bound B1-B5 to a single function-value paragraph; that contract is honoured. The §6 grammar surface (`:248`) reasserts each claim. | Steelman: split B4 (closure capture) into its own paragraph because it is a parse-error rule, not a typing rule. Defeated: closure capture is *both* a parse rule (§6) and a typing rule (§3); the §3 paragraph correctly names it as a typing concern. | KEEP / READY |
| 7 | 3 Cohesion (match + tuple typing) | `PASS-1.md:77`, `:230-235`, `:250` | Match-arm unification through HM equality + exhaustiveness check against grammar-derived variant set + `BBNF-PATTERN-NONEXHAUSTIVE` reserved at `:118`. Tuples synthesize product type from components. | V1 patterns admit literal/wildcard/identifier/constructor/tuple; or-patterns and guards defer to V2. The deferral is named but the receiver is not (no "V2 amendment" pointer). | The deferral is closed-by-construction: V1 = these five pattern forms; V2 amendment opens or-patterns and guards. The classification is sufficient at the substrate level. | Steelman: any deferral needs receiver + blocker + gate per Lane 8. Defeated: the receiver is "V2 amendment"; the blocker is "or-patterns/guards do not compose with single-pass exhaustiveness check"; the gate is the future V2 type-system research gate. PASS-1 is concise but not silent. | KEEP / READY |
| 8 | 4 SOTA-Anchoring (DK13 cite) | `PASS-1.md:73` | "DK13 algorithmic completeness (Dunfield-Krishnaswami 2013…)" cites the right paper and authors; the algorithmic-completeness frame is correct (DK13 is the standard reference for sound + complete bidirectional higher-rank). | The citation does not include the paper title; "DK13" is the field's shorthand but a reader needs the full reference. | Citations in synthesis documents bind the *frame* (algorithmic completeness, ordered contexts, principality), not the bibliography. The bibliography lives at `restart/research/topic-2-bidirectional.md`. | Steelman: every academic cite should carry the full reference. Defeated: PASS-1 is a synthesis spec, not a literature survey; the topic-2 research file carries the full reference per V6 fold. | KEEP / READY |
| 9 | 4 SOTA-Anchoring (HM cite) | `PASS-1.md:73` | "Damas-Milner 1982; Pierce 2002 ch.22" — Damas-Milner 1982 is the POPL paper "Principal type-schemes for functional programs" (correct); Pierce 2002 ch.22 is *Types and Programming Languages* chapter 22 "Type Reconstruction" (correct). | Pierce 2002 has 32 chapters; ch.22 specifically covers HM Algorithm-W with constraint-based formulation, which matches the cite's intent. The chapter number is verifiable from the book TOC. | The pairing (Damas-Milner foundational + Pierce 2002 ch.22 modern textbook treatment) is the standard pedagogical cite for HM Algorithm-W in 2026. | Steelman (lens H): Pierce 2002 ch.22 might be ch.21 or ch.23 — verify. Result: Pierce TAPL 2002 ch.22 is "Type Reconstruction" (HM with W); ch.20 is "Recursive Types"; ch.21 is "Metatheory of Recursive Types"; ch.23 is "Universal Types". The cite is correct. | KEEP / READY |
| 10 | 4 SOTA-Anchoring (CSP cite — Pottier-Rémy) | `PASS-1.md:75` | "Pottier-Rémy first-order unification" cites the standard reference for first-order unification under HM(X). | Pottier-Rémy is the more typical cite for HM(X), not pure first-order unification (Robinson 1965 is the canonical first-order unification reference). | The cite is for first-order unification *as deployed in HM-style type systems*; Pottier-Rémy 2005 ("The Essence of ML Type Inference") is the modern reference for this composition. The cite is contextually correct, though Robinson 1965 would be more precise for unification *qua unification*. | Steelman (lens H): is this a hallucinated cite? Result: François Pottier and Didier Rémy authored "The Essence of ML Type Inference" in *Advanced Topics in Types and Programming Languages* (MIT Press, 2005). The first-order unification frame is correct. | KEEP / READY |
| 11 | 5 Grammar-Authoritative (yaml two-surface) | `PASS-1.md:262-268` | Future grammar onboarding proof table preserved verbatim from V6: grammar source + workspace metadata + xtask-emitted artifacts; "manual fixture as onboarding requirement" forbidden; verification command intact. | Phase 7 fold did not touch the yaml table; could the directive surface expansion (`@import`, `@pretty`, `@token`) silently violate two-surface? | The 6-directive expansion adds *grammar-side* directives, all of which live in `<grammar>.bbnf` source. None of them moves authority to a third surface; per-grammar declaration crates remain rare-fenced (`PASS-1.md:91-103`). | Steelman: `@import` is cross-file composition — does it count as a third surface? Defeated: `@import` is a *within-grammar-source* construct that names another grammar source, not a separate author surface. The two-surface contract holds. | KEEP / READY |
| 12 | 5 Grammar-Authoritative (rare-fence) | `PASS-1.md:91-103` | Eight-field rare fence preserved verbatim from V6; extant grammar table empty; verification command preserved; canonical review form synced to Architecture. | Phase 7 fold opened function-value surface; could a `@host fn` carrying a function-typed parameter constitute a covert per-grammar declaration crate? | Function-value surface is *generic* — `@host fn map<I, O>(f: fn(I) -> O, xs: [I]) -> [O]` works for any `I`, `O`; nothing in the surface admits grammar-named code. | Steelman: a sufficiently complex `@host fn` chain could embed grammar-specific behavior. Defeated: Lock 14 enforces zero grammar-named match arms in generic crates; the rare-fence covers any escape valve; PASS-1's seed-grammar table at `:274` lists rare fence as "table empty". | KEEP / READY |
| 13 | 5 Grammar-Authoritative (6-directive set) | `PASS-1.md:203, 246` | The six directives are each justified by extant grammar use (`@import` 22+ files; `@pretty` 30+ sites; `@token` cited at `grammar/css/pretty.bbnf:17-19`). Retirement ledger excludes `@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode`, `@ws`, standalone `@recover`. | The retirement set is large; could a future grammar need one back? | Lock 10 amendment binds the six-directive set; future expansion would require a Lock 10 re-amendment + ARCH §8.1 + PASS-1 §6 sync. The amendment surface exists; the retirement is not irreversible. | Steelman: `@token` is a new directive — was it justified or hallucinated? Result: cite at `grammar/css/pretty.bbnf:17-19` confirms extant use. The retirement set is principled, not arbitrary. | KEEP / READY |
| 14 | 6 Generated-Code-Budget (finite generic instance validation) | `PASS-1.md:81` | "Validation materializes a finite `(RuleId, TypeArgs)` instance set reachable from a concrete entry or metadata-declared export; recursive generic cycles require a decreasing structural argument, an explicit return annotation, or rejection with `BBNF-GENERIC-CYCLE`." | DK13 higher-rank fold opens a wider monomorphisation surface; could a rank-N generic explode? | Higher-rank polymorphism in DK13 is *type-checked* at instantiation but *monomorphisation* still requires a finite `(RuleId, TypeArgs)` set. The finiteness gate is upstream of any rank concern. | Steelman: `forall a. a -> a` admits infinite type instances. Defeated: `BBNF-GENERIC-CYCLE` rejects unbounded sets; DK13's principality + finite reachability bounds the set. The budget gate is preserved. | KEEP / READY |
| 15 | 7 Friction-Forecast (BBNF-LOCAL-EQUALITY-ANNOTATION reservation) | `PASS-1.md:117` | Verbatim diagnostic message reserved: "rule {rule} introduces branch-local type equality {equality}; add an explicit return annotation because the inferred type is not principal." | The diagnostic is reserved but cannot fire in V1 (no surface emits the equality); could it confuse a user reading the diagnostic catalogue? | The reservation is intentional: V1 surface is closed against GADT, but the substrate carries the implication-constraint plumbing (`PASS-1.md:81`). When the V2 amendment opens the surface, the diagnostic activates without re-architect. The catalogue entry is forward-discipline, not friction. | Steelman: an unreachable diagnostic is dead code in spec form. Defeated: the diagnostic catalogue is a stable contract; V2 surface activation requires the diagnostic to pre-exist; pre-reservation is the correct discipline. | KEEP / READY |
| 16 | 7 Friction-Forecast (BBNF-PATTERN-NONEXHAUSTIVE new diagnostic) | `PASS-1.md:118` | Verbatim diagnostic added: "match expression in {rule} does not cover variant {variant}; add an arm or a wildcard." | New diagnostic increases the friction surface for users learning match. | Match expressions are a new V1 surface (B5 fold); the diagnostic is mandatory friction infrastructure for that surface. The wording is direct and actionable. | Steelman: the diagnostic should also name the *missing variant set* in detail, not just one variant. Defeated: `{variant}` is a placeholder; the diagnostic-rendering layer (PASS-3) decides whether to enumerate one missing variant or all. PASS-1 owns the message *template*, not the rendering. | KEEP / READY |
| 17 | 8 Carry-Deferral (V2 GADT amendment route) | `PASS-1.md:81, 117` | Receiver: V2 amendment with annotation rules. Blocker: V1 has no user-facing GADT branch-local equality surface. Gate: `BBNF-LOCAL-EQUALITY-ANNOTATION` activation. | The receiver is "V2 amendment" (open-ended); the gate is the diagnostic, not a tranche letter. | V2 is post-V1 by definition; tranche letters at this scope (A-J) are V1-only. The diagnostic + annotation-rule contract is the gate. | Steelman: per Lane 8 contract, "no future tranche". Defeated: V2 is an architecture-cycle, not a tranche; the post-V1 amendment receiver is the canonical post-V1 carry receiver per V6 baseline (`HARDENING-PASS-1-V6.md` §6 "Future GADT/higher-rank gate"). | KEEP / READY |
| 18 | 8 Carry-Deferral (V2 closure-capture-by-move + Fn* trait split) | `PASS-1.md:248` | "Closure-capture-by-move and the `Fn*` trait split defer to V2 amendment." | Receiver named, but no blocker or gate. | Implicit blocker: V1 lifetime model is `&'i Tape<'i>`; capture-by-move requires owned-tape-fragment lifetime extension. Gate: future V2 amendment with closure-environment lowering. | Steelman: the deferral should name a `BBNF-CLOSURE-CAPTURE` diagnostic. Defeated: V1 simply rejects capture-by-move at parse time; no diagnostic-catalogue entry needed because the surface is closed. The deferral is closed-by-grammar, not closed-by-diagnostic. | KEEP / READY (V7 R3 friction: name the parse-error message verbatim, see §5) |
| 19 | 9 Greenfield-Discipline (no legacy code uncontested) | `PASS-1.md:75, 248`; V6 baseline `HARDENING-PASS-1-V6.md` §3 row 9 | DK13 + closure-by-`&'i` + 6-directive replace prior conservative posture (V6 had "rank-1 only", "no closure ratification", "3-directive grammar"). Phase 7 inverts the polarity per user mandate. | The reversal is large — V6 said rank-1 was V1-final; V7 says higher-rank is V1. Could this be a regression of greenfield discipline? | The reversal is by user adjudication (Q1+Q2 in `V1-FOLD-CANDIDATES.md:204-205`); it is the *strengthening* of greenfield discipline, not a retreat. Conservative V6 deferred work that V7 folds in. | Steelman: a target that flips its substrate-claim mid-cycle is unstable. Defeated: V6 was research-fold-aware; V7 is fold-V1-aware; the user's "audacious + SOTA" mandate is the discriminator. The greenfield posture sharpened, not regressed. | KEEP / READY |

Cohort count for V7 PASS-1:

| Class | Count |
|---:|---:|
| KEEP / READY rows | 19 |
| AMENDMENT-REQUIRED rows | 0 |
| RE-DRAFT rows | 0 |
| Friction-residual rows (non-blocking) | 3 (R1, R2, R3) |

Lane-rollup verdict: every active lane returns READY post-fold. No blocking amendment row.

## §4 LLM-pathology audit (lenses F/G/H)

### Lens F — LLM bias (hedging, reference-stuffing, pseudo-precise numerics)

| Probe | Site | Finding |
|---|---|---|
| Hedging in DK13 fold language | `PASS-1.md:73-75` | None. The text says "Higher-rank polymorphism is a V1 surface, not a future amendment" — direct claim, no hedging. The user-mandate clause "inference stronger than Rust if possible" is quoted from `V1-FOLD-CANDIDATES.md:5`, not invented. |
| Reference-stuffing of Dunfield-Krishnaswami citations | `PASS-1.md:73-75, 81` | "Dunfield-Krishnaswami 2013" appears once; "DK13" as shorthand four times. No padding. The Phase 7.2 classification (`phase-7.2-classification.md:14`) explicitly directed naming + citation, not stuffing. |
| Pseudo-precise numerics | PASS-1 globally | None. PASS-1 is a substrate spec, not a perf gate; it carries no numeric claims. The closest numerics are "12-15 variant IR" (`:9`) and "about 22 executable variants" (`:10`), both inherited from V1-V6 and consistent. |

Lens F result: no pathology. The Phase 7 fold introduced no hedging, no reference-stuffing, no pseudo-precise numerics. The user-mandate quote is verbatim from the synthesis baseline.

### Lens G — Overfitting (pattern-lift from external systems)

| Probe | Site | Finding |
|---|---|---|
| Pattern-lift from GHC OutsideIn(X) | `PASS-1.md:73, 81` | "OutsideIn-style implication constraints" is named with the qualifier "-style"; the substrate is "internal" and uses BBNF's own `Implication { givens, wanted }` shape. The cite is inspirational, not adopted-in-full. |
| Mimetic convergence with rust-analyzer | PASS-1 globally | None. PASS-1 specifies `passes::layout` + `passes::types` decomposition; rust-analyzer uses HIR + chalk. The trait Backend at ARCH §7.5 is Rust-idiomatic but not mimetic of rust-analyzer's compiler interface. |
| Pattern-lift from GHC's Implication constraints | `PASS-1.md:81` | "the CSP solver carries `Implication { givens, wanted }` constraints" — this is GHC-derived terminology applied to BBNF's CSP solver. The pattern transfer is principled (CHR + OutsideIn(X) work for GHC; PASS-1 cites the same lineage at `:73`). Not overfitting; explicit lineage. |

Lens G result: no pathology. External patterns are cited with provenance and adapted to BBNF's substrate, not lifted wholesale. The "OutsideIn-style" qualifier and the "internal substrate" framing prevent overfitting.

### Lens G addendum — Phase 7.2 acceptance-gate cross-check

The Phase 7.2 classification (`phase-7.2-classification.md:35-44`) declared eight acceptance gates. V7 verifies each:

| Acceptance gate | Phase 7.2 directive | V7 verification |
|---|---|---|
| §3 type-system algorithm: HM + Pierce-Turner + DK13 stack named with citations | full naming + Damas-Milner 1982 + Pierce 2002 ch.22 + Dunfield-Krishnaswami 2013 | LANDED (cross-doc isomorphism table in §2 confirms). |
| §3 GADT substrate: branch-local-equality plumbing as internal CSP machinery | `Implication { givens, wanted }` constraints feeding `LayoutFacts` | LANDED at `PASS-1.md:81`. |
| §3 row polymorphism: internal collapse named; surface fence preserved | Leijen-style scoped labels + `passes::layout` subroutine + surface defers | LANDED at `PASS-1.md:81`; surface fence ratified ("row variables never appear in the BBNF `Type` non-terminal"). |
| §3 schema-mining miner: peer-of-recogniser-miners paragraph mirrors ARCH §8.2 | telemetry + `(rule_shape, layout_decision, value_shape)` triples + HM/CSP/DK13 chain | LANDED at `PASS-1.md:79`; ARCH §8.2 mirror at `:1302-1308` confirmed. |
| §3 function values: arrow as first-order; DK13 application; transducer apotheosis worked through; lambda synthesis/check; closure-by-`&'i` | Milner 1978 + Pottier-Rémy + DK13 application judgment + worked example + synthesis/check + capture rule | LANDED at `PASS-1.md:75` — five separable claims in one paragraph, each verifiable. |
| §6 grammar: `Match`, `Tuple`, `Arm`, `Pattern` productions added; `Primary` extended | EBNF productions for new non-terminals | LANDED at `PASS-1.md:225, 230-235`. |
| §3 match + tuple typing: arm-unification + exhaustiveness + product-type synthesis | typing rule paragraph | LANDED at `PASS-1.md:77`. |
| C1 parse-that-regex: no `regex-automata` references in PASS-1; `parse-that-regex` named only where the regex engine is the subject | scan zero `regex-automata`; positive `parse-that-regex` at e-graph plug-in row | LANDED — `rg -n 'regex-automata' PASS-1.md` exits 1 (zero hits); `parse-that-regex` at `:182`. |

Eight of eight acceptance gates satisfied. No Phase 7.2 acceptance gate is partial.

### Lens H — Hallucination (citation accuracy, code accuracy)

| Probe | Verification | Finding |
|---|---|---|
| Pierce 2002 ch.22 — is the chapter number right? | TAPL 2002 TOC: ch.22 = "Type Reconstruction" (Algorithm W with constraint-based formulation); ch.21 = "Metatheory of Recursive Types"; ch.23 = "Universal Types". | Correct. ch.22 is the canonical chapter for HM Algorithm-W. |
| Damas-Milner 1982 — is the venue right? | Damas + Milner, "Principal type-schemes for functional programs", POPL 1982 (9th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, January 1982, Albuquerque, NM). | Correct. PASS-1 cites year only ("Damas-Milner 1982"); year matches the POPL'82 publication. |
| Dunfield-Krishnaswami 2013 — is the cite real? | Dunfield + Krishnaswami, "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism", ICFP 2013. The 2019 follow-up extended it ("Sound and Complete Bidirectional Typechecking for Higher-Rank Polymorphism with Existentials and Indexed Types", POPL 2019). | Correct. The fold candidates dispatch (`phase-7.2-classification.md:14`) cited "Dunfield-Krishnaswami 2013/2019"; PASS-1 cites 2013 only. The 2013 paper is the primary cite; the 2019 paper is the existential/indexed extension. PASS-1's choice is principled. |
| Milner 1978 — is the cite real? | Milner, "A Theory of Type Polymorphism in Programming", JCSS 1978. This is the foundational HM paper; the function arrow as first-order constructor frame is consistent with Milner's contribution. | Correct. |
| Pottier-Rémy first-order unification | "The Essence of ML Type Inference", François Pottier + Didier Rémy, *Advanced Topics in Types and Programming Languages* (MIT Press, 2005). | Correct. The cite supports the HM(X)/first-order unification frame. |
| `BBNF-PATTERN-NONEXHAUSTIVE` — is this code committed? | Verified at `PASS-1.md:118` (verbatim message) + `:77` + `:250` (text references). New in Phase 7.2 fold (B5). Mirrored at ARCH §7.4 catalogue? | The new code is committed in PASS-1 §2; it does not yet appear in `ARCHITECTURE.md` §7.4 catalogue (which lists `BBNF-LOCAL-EQUALITY-ANNOTATION` at `:1052` but not `BBNF-PATTERN-NONEXHAUSTIVE`). V7 friction-residual R4: ARCH §7.4 should ratify the new code. **Not blocking PASS-1**; ARCH owns the catalogue, and the diagnostic-string ownership fence at `PASS-1.md:107` says "Diagnostic strings owned by PASS-1". |
| `BBNF-LOCAL-EQUALITY-ANNOTATION` — is this code committed? | Verified at `PASS-1.md:117` + `ARCHITECTURE.md:1052`. Pre-existed V6; not new in Phase 7. | Correct. |

Lens H result: every Phase 7 citation is verifiable. One residual (R4): `BBNF-PATTERN-NONEXHAUSTIVE` not yet in ARCH §7.4 catalogue — non-blocking for PASS-1 (PASS-1 owns the diagnostic surface; ARCH catalogue sync is a synthesis amendment).

LLM-pathology overall: zero pathology found. The Phase 7 fold preserved citation accuracy, did not hedge, did not stuff references, did not invent numerics, and did not overfit to external systems. The single residual (ARCH §7.4 catalogue sync for the new pattern-nonexhaustive code) is V7 friction, not V7 pathology.

## §5 Punch list

V7-blocking punch list:

| # | Path:line | Surgery | Acceptance gate | Origin | V7 status |
|---|---|---|---|---|---|
| 1 | none | none | no V7 PASS-1 amendment required | V7 9-lane audit + Step A fold verification + lens F/G/H audit | READY |

V7 friction-residual ledger (non-blocking):

| # | Path:line | Surgery | Acceptance gate | Origin | PASS-1 blocking? |
|---|---|---|---|---|---|
| R1 | `PASS-1.md` (any §3 anchor) | Add a one-sentence cross-reference: "These amendments mirror Lock 4 (`14-LOCKS.md:40`) and Lock 10 (`14-LOCKS.md:52`)." | The literal lock-number citation appears at least once in PASS-1, so a single-document reader can triangulate to the amendment provenance. | Lane 1 + Step A "Lock amendments cited" row | No. PASS-1 carries the substance verbatim; lock-number citation is hygiene. |
| R2 | `PASS-1.md` §2 (around `:41-71`) | Add a one-sentence cross-reference: "The formal Rust trait that realizes this contract is `Backend` at ARCH §7.5 (`ARCHITECTURE.md:1067-1144`)." | The Backend trait is reachable from PASS-1 with one click. | Lane 1 + Step A "Backend trait reference" row | No. The PASS-1 obligations table is the substantive contract; ARCH §7.5 is the Rust realization; cross-document binding is functional. |
| R3 | `PASS-1.md:248` | Append the verbatim parse-error message for closure-capture-by-move: e.g., "`closure in {rule} captures {binding} by move; V1 closures capture by &'i Tape<'i> reference only.`" | Lane 8 carry-deferral row 18: receiver + blocker + gate fully named via verbatim diagnostic. | Lane 8 row 18 (V2 closure-capture-by-move + `Fn*` trait split) | No. V1 simply rejects capture-by-move at parse time; the diagnostic catalogue need not pre-reserve a code, but a verbatim parse-error message tightens friction. |
| R4 | `ARCHITECTURE.md` §7.4 (the diagnostic catalogue around `:1017-1060`) | Add a row for `BBNF-PATTERN-NONEXHAUSTIVE` mirroring `PASS-1.md:118`. | The catalogue ratifies every PASS-1-owned diagnostic. | Lens H lens-check; PASS-1.md `:107` says "Diagnostic strings owned by PASS-1" + `ARCHITECTURE.md:1063` says "The catalogue here binds identifiers and producer sites". | No. PASS-1 owns the string; ARCH owns the catalogue; sync is a synthesis-level amendment, not a PASS-1-local one. |

R1-R4 are routed to consolidation, not to a V7 PASS-1 amendment cycle. They are visibility/hygiene tightenings; the load-bearing substance lands in PASS-1 verbatim.

## §6 V6 vs V7 comparison

V6 closed the research-fold absorption (Topics 1-8) on a conservative substrate posture: rank-1 HM core, no V1 GADT surface (substrate not yet specified as branch-local-equality-bearing), no closure ratification, three-directive grammar, scalar miner + Pratt + recogniser miners. V6 returned READY because the conservative substrate honoured every lock and every research-fold pressure was routed.

V7 verifies that Phase 7 inverted the conservative posture per the user's "audacious + SOTA + functional + Rust-like + inference stronger than Rust if possible" mandate from `V1-FOLD-CANDIDATES.md:5`. The inversions are: DK13 algorithmic completeness folded as V1 surface (rank-N via explicit `forall`); GADT machinery exposed as internal substrate (CSP `Implication` constraints feeding `LayoutFacts`); internal row polymorphism exposed as `passes::layout` subroutine; schema-mining miner added as peer of recogniser miners; function values + `FnType` + lambda + closure-by-`&'i` admitted at the `Type` non-terminal and `Primary` site; match + tuple expressions admitted with `BBNF-PATTERN-NONEXHAUSTIVE`; six-directive grammar (`@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`) replacing the three-directive form; `Backend` trait at ARCH §7.5; `parse-that-regex` naming canon. Each inversion is locked by an amendment (Lock 4 + Lock 5 + Lock 6 + Lock 8 + Lock 10 + Lock 11 + Lock 12 + 7-amendment composite at `9cb92284`); each inversion lands at PASS-1 path:line; each inversion's substance is verifiable against the dispatch's classification table (`phase-7.2-classification.md`).

V6 → V7 delta table:

| Surface | V6 posture | V7 posture | PASS-1 anchor |
|---|---|---|---|
| Higher-rank polymorphism | Future amendment via Dunfield-Krishnaswami or OutsideIn(X); rank-1 only in V1 | DK13 algorithmic completeness folded as V1; rank-N via explicit `forall` annotation | `:73` |
| GADT / branch-local equality | Surface fence preserved (V1 has no GADT); substrate not specified | Surface fence preserved AND internal substrate exposed (`Implication { givens, wanted }` constraints feeding `LayoutFacts`); GADT-ready by construction | `:81` |
| Row polymorphism | "Out of V1" (surface fence only) | Surface fence preserved AND internal collapse exposed (Leijen-style scoped labels + `passes::layout` subroutine); record-narrowing as single row-unification step | `:81` |
| Schema-mining miner | Not specified at PASS-1 | Telemetry-driven schema inference as peer of recogniser miners; `(rule_shape, layout_decision, value_shape)` triples; honors user mandate "without explicit annotations" | `:79` |
| CHR-improvement layer | Not specified at PASS-1 | Host overloads with determining arguments emit explicit improvement constraints, CHR-shaped where applicable, before finite CSP selection | `:73` |
| Function values + types (`fn(T) -> U`) | Not in `Type` non-terminal; transducer required `@transducer` directive | First-class member of `Type`; `FnType = "fn" "(" TypeList? ")" "->" Type`; transducer apotheosis without directive | `:240`, `:75`, `:248` |
| Lambda literal | `Closure` non-terminal; semantics under-specified | `LambdaExpr = "|" Params? "|" ( Expr | Block )`; DK13 synthesis + Pierce-Turner check | `:229`, `:75` |
| Closure capture | Closure beta-reduction "research signal only"; no V1 ratification | Capture by `&'i Tape<'i>` reference only; capture-by-move forbidden in V1; `Fn`/`FnMut`/`FnOnce` collapsed | `:75`, `:248` |
| Match + tuple expressions | Absent from `Primary` | `MatchExpr` and `TupleExpr` join `Primary`; arm-unification under HM equality; `BBNF-PATTERN-NONEXHAUSTIVE` reserved | `:225`, `:230-235`, `:77`, `:118` |
| Grammar directive set | Three (`HostFn`, `ErrorDecl`, `LayoutDecl`) | Six (`ImportDecl`, `HostFn`, `ErrorDecl`, `LayoutDecl`, `PrettyDecl`, `TokenDecl`); retirement ledger for `@pratt`/`@simd`/`@transducer`/`@rewrite`/`@unicode`/`@ws`/standalone `@recover` | `:203`, `:246` |
| Backend per-backend lowering | Per-backend obligations table only | Per-backend obligations table preserved AND formal `Backend` trait at ARCH §7.5 (`RustBackend` V1; `WasmBackend`/`TsBackend` V2 deferred) | `:61-71` (PASS-1) + `ARCHITECTURE.md:1067-1144` (trait surface) |
| Regex engine naming | `parse-that` (parent crate) only | `parse-that` parent + `parse-that-regex` regex sub-crate; `regex-automata` oracle role retired | `:182` |

The new V7 findings are exclusively friction-residuals (R1-R4), not blocking amendments: lock-number citations missing from PASS-1 prose (R1); Backend trait cross-reference missing from PASS-1 §2 (R2); closure-capture-by-move parse-error verbatim message uncommitted (R3); ARCH §7.4 catalogue sync for `BBNF-PATTERN-NONEXHAUSTIVE` outstanding (R4). All four are hygiene tightenings; none invalidates the V6 READY verdict; none requires a V7 PASS-1 amendment cycle.

V6 readiness criteria preserved post-fold:

| V6 criterion | V7 status |
|---|---|
| Research-fold coherence (Topics 1-8) | PASS — Topics 1-3 type-system folds expanded into DK13 + GADT substrate + row poly + schema miner; Topic 4 bridge stable-id contract intact; Topic 5 cost evidence intact; Topics 6-8 still routed. |
| Nine-lane audit | PASS — 19 KEEP/READY rows post-fold; zero amendment rows. |
| Retired-surface scan (`path!`, `@pratt`, `@simd`, rewrite-mode, grammar Unicode algebra, OpenFrame substrate, ParseStream runtime, default declaration-crate) | PASS — Phase 7 fold did not revive any retired surface; the six-directive grammar explicitly retires `@pratt`/`@simd`/`@transducer`/`@rewrite`/`@unicode`. |
| Layout/type vocabulary | PASS — `LayoutFacts` public; `TypeFacts` internal; `passes::layout` ownership intact; `passes::types` internal child. |
| Rare declaration-crate fence | PASS — eight-field fence preserved verbatim; extant grammar table empty. |
| YAML two-surface onboarding | PASS — grammar source + workspace metadata only; six-directive expansion does not breach. |
| Gate rerun | PASS — every Phase 7.2 acceptance gate verified (Lens G addendum). |
| Punch list | PASS — zero V7-blocking entries; four V7 friction-residuals routed to consolidation. |

## §7 Final verdict

**Decision: READY.**

Phase 7 expanded V1's type-system surface (DK13 + GADT-as-substrate + row-poly internal + schema miner + CHR-improvement) and function-value surface (`FnType` + lambda + closure-by-`&'i` + match + tuple) per the user's audacious mandate. Each fold item lands at PASS-1 at the path:line cited by the Phase 7.2 classification record. Every academic citation (Damas-Milner 1982; Pierce 2002 ch.22; Dunfield-Krishnaswami 2013; Milner 1978; Pottier-Rémy) is verifiable. No LLM pathology was introduced (zero hedging, zero reference-stuffing, zero pseudo-precise numerics, zero hallucinated cites, zero pattern-overfit). The 9-lane verification produces 19 KEEP/READY rows, 0 amendment rows, 0 re-draft rows. Friction-residuals R1-R4 are routed to consolidation; none blocks PASS-1.

V6's READY verdict on PASS-1 is preserved by V7. The substrate sharpened, the surface widened, and the locks held.

### §7.1 Verdict-criteria evidence

| Criterion | Result | Evidence |
|---|---|---|
| Phase 7.2 acceptance gates (8) | PASS | §2 cross-doc isomorphism table + Lens G addendum verify all 8. |
| 9-lane audit row count ≥ 18 | PASS | 19 audit rows produced (§3). |
| LLM-pathology lenses F + G + H clean | PASS | Zero hedging, zero reference-stuffing, zero pseudo-precise numerics, zero hallucinated cites, zero pattern-overfit (§4). |
| Retired-surface scan zero | PASS | `regex-automata` zero hits in PASS-1; `@pratt`/`@simd`/`@transducer`/`@rewrite`/`@unicode` only in retirement ledger (`:246`). |
| V6 baseline preserved | PASS | All V6 lock-adherence + sequencing + cohesion + SOTA + grammar-authoritative + budget + friction + carry + greenfield rows still READY post-fold. |
| Friction-residual routing | PASS | R1-R4 routed to consolidation; none blocks PASS-1. |

Re-draft thresholds met: zero. Amendment-required threshold for PASS-1 met: zero.

## §8 Closing posture

PASS-1 is fit for V7 consolidation as READY.

The Phase 7 fold honoured the user mandate: function values + lambda + closure-by-`&'i` + match + tuple now express the transducer apotheosis without a `@transducer` directive; DK13's principality tracking + algorithmic completeness deliver "inference stronger than Rust if possible"; the schema-mining miner consumes telemetry to "generate semantic schemas without explicit annotations in most cases"; the GADT/branch-local-equality substrate is greenfield-ready though no V1 surface exposes it. The audacious centre of the fold (DK13 + GADT substrate + row poly + schema miner) lands without sacrificing the substrate-first contract or the lock fence.

Hereupon the consolidator should treat PASS-1 as READY, route R1-R4 to the synthesis hygiene pass, and avoid a V7 PASS-1 amendment cycle. The remaining V7 work is the parallel V7-PASS-2, V7-PASS-3, V7-MASTER-PLAN audits and the V7 consolidation that binds them.
