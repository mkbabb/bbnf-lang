# PASS-1 Synthesis: Substrate

## §1 PASS-1 Verdict Ledger

| Concern | Agent | Evidence | Decision |
|---|---|---|---|
| Tape substrate | 1, 4, 6 | `restart/README.md:285`-`restart/README.md:315`; `restart/locks/14-LOCKS.md:34` | KEEP tape as substrate and union it with direct-to-struct. |
| ParseStream naming | 1, 6 | `restart/inheritance/INDEX.md:66`; stale README cells at `restart/README.md:391`, `restart/README.md:473` | DISCARD rename; retain source normalization/span-map work under source modules. |
| Grammar IR | 1 | `restart/README.md:104`-`restart/README.md:118`; current node mix at `crates/ir/src/types/node.rs:30`-`crates/ir/src/types/node.rs:98` | REINVENT as semantic 12-15 variant IR. |
| Backend IR | 1, 3, 4 | BC table at `docs/tranches/BC/audit/W0-typed-ir-variant-table.md:160`-`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:254` | KEEP about 22 executable variants. |
| Optimized IR | 1, 3 | `restart/README.md:114`-`restart/README.md:118` | KEEP as side tables, not AST. |
| Type system | 2 | `restart/README.md:258`-`restart/README.md:268` | KEEP HM core + expected-type check/synth + finite CSP choice; §2 binds the sharpened solver contract. |
| CSP/e-graph | 3 | `restart/README.md:219`-`restart/README.md:228`; `restart/locks/14-LOCKS.md:40` | KEEP bridged, per-domain composition. |
| Cost model | 4 | `restart/README.md:211`-`restart/README.md:218`; `restart/locks/14-LOCKS.md:48` | KEEP `CostModel` / `CostDecision` scoring with SOTA gates, objective evidence, and extraction evidence. |
| Lookbehind | 5 | `restart/README.md:124`-`restart/README.md:132` | KEEP. |
| Rewrite-mode | 5 | Prompt asks at `restart/prompts/PASS-1-SUBSTRATE.md:3`, `restart/prompts/PASS-1-SUBSTRATE.md:31`, `restart/prompts/PASS-1-SUBSTRATE.md:66`; README rejects at `restart/README.md:134`-`restart/README.md:137` | DISCARD. |
| Unicode class algebra | 5 | README defers at `restart/README.md:139`-`restart/README.md:143` | DEFER to regex layer. |
| Host fn/chains/generics | 2, 5 | `restart/README.md:145`-`restart/README.md:164`; `docs/ffuzzy.md:648`-`docs/ffuzzy.md:672` | KEEP. |
| `@error` / `@layout` | 2, 5 | `restart/README.md:166`-`restart/README.md:174` | KEEP as typed directives. |
| Per-grammar declaration crates | 2, 5 | `AMENDMENT-01`: `restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:7`-`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:24` | DISCARD as default; rare escape only. |

## §2 Substrate Architectural Commitments

Grammar IR shape: semantic nodes only. Proposed variants are `Rule`, `Seq`, `Alt`, `Repeat`, `Optional`, `Literal`, `Regex`, `Ref`, `Predicate`, `Lookbehind`, `Map`, `HostCall`, `LayoutDirective`, `ErrorDirective`, and `Annotation`.

Grammar IR schema floor:

| Variant | Required fields | Stable key | Producer | Consumer | Forbidden leakage |
|---|---|---|---|---|---|
| `Rule` | name, generics, body, attrs, source span | `RuleId` | `grammar/desugar` | `passes/validate`, `passes/layout` | no backend emission data |
| `Seq` / `Alt` | child ids, separator/progress facts, source span | `NodeId` | `grammar/desugar` | recognizers, e-graph, cost model | no Rust/WASM lowering policy |
| `Repeat` / `Optional` | body id, bounds, progress proof | `NodeId` | `grammar/desugar` | recognizers, error recovery, cost model | no loop code shape |
| `Literal` / `Regex` | literal bytes or regex body, flags, source span | `NodeId` | parser | regex compiler, scanner miner | no scanner kernel choice |
| `Lookbehind` | predicate id, body id, width proof slot | `NodeId` | parser | width checker, Backend IR builder | no unbounded predicate |
| `Map` / `HostCall` | chain steps, argument ids, expected type | `NodeId` | parser + type checker | host inference, layout lowering | no per-grammar host crate reference |
| `LayoutDirective` / `ErrorDirective` | directive value, owner rule, override reason | `RuleId` + directive kind | parser | layout/error fact builders, PASS-3 recovery consumers | no runtime recovery code |
| `Annotation` | key, value, source span | `NodeId` | parser | diagnostics and metadata lowering | no backend-specific key |

Grammar IR producer note: `RuleId` and `NodeId` are the stable keys for type, layout, recovery, e-graph, CSP, and cost side tables. E-graph and cost consumers may record facts keyed by those ids, but Grammar IR never stores e-node representatives, regex internals, scanner choices, lowerer policy, or Backend IR payload refinements.

Backend IR shape: executable plan nodes. Proposed variants are `Entry`, `Seq`, `DispatchAlt`, `SpeculativeAlt`, `RepeatLoop`, `OptionalBranch`, `ByteLiteral`, `RegexProgram`, `SimdScan`, `PrattSpine`, `CallRule`, `CallHost`, `HostChain`, `LayoutPush`, `LayoutPop`, `ErrorRecover`, `SpanMark`, `TapeEmit`, `DirectBuild`, `ValueProject`, `PathEval`, and `DebugMark`.

Backend IR ownership: type definitions and the variant alphabet live under `ir/src/backend_ir/`. PASS-1 (Grammar IR producer) and the lowering passes (consumer) both depend on `ir`; neither owns BIR types itself. The `codegen` crate is limited to lowerers, adapters, generated-source snapshots, and emission tests; it imports `ir::backend_ir` and never re-defines or extends the BIR node alphabet. The lowerer import-deny gate `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template` returns zero — only the BIR producer pass under `passes` may import Grammar IR; lowerers walk Backend IR alone.

Backend IR ownership and invariant floor:

| Variant family | Payload category | Lower-time invariant | PASS-2 refinement rule |
|---|---|---|---|
| Entry/control | rule id, node id, control successors | every edge targets valid BIR node | PASS-2 may split files, not redefine nodes |
| Dispatch/speculation | dispatch keys, checkpoint policy, progress proof | no OpenFrame clone stack; rollback is bounded | PASS-2 picks branch shape from cost evidence |
| Terminal/scanner | literal bytes, regex program, scanner alphabet | regex owns Unicode semantics; scanner is data-driven | PASS-2 chooses scalar/SIMD backend |
| Pratt/SIMD | operator table or structural alphabet | auto-detected only; no grammar directives | PASS-2 emits tables and diagnostics |
| Host/layout/error | typed host chain, layout facts, recovery policy | no default declaration crate; `@error` owns recovery | PASS-2 emits generic host/runtime hooks |
| Tape/direct/value | tape kind, span, payload slot, direct fields | direct values borrow from tape identity | PASS-3 consumes emitted view metadata |
| Debug/path | source map, selected alternative, path plan hook | diagnostic spans are stable | PASS-3 consumes for LSP/path/debug |

PASS-2's role is payload refiner, not BIR re-owner. PASS-2 may sharpen field types, add lower-time evidence (cost weights, scanner alphabets, dispatch tables), and split BIR submodules for cohesion; PASS-2 may not introduce new variants, retire variants, or redefine the alphabet. New variants and alphabet changes return to PASS-1 + Architecture §7 and rerun the hardening gate before they land.

Builder-frame replacement for OpenFrame: dispatch, speculation, and `Repeat`/`Optional` lowering use generated Backend IR builder frames keyed by `RuleId` and `NodeId` plus `TapeBuilder` checkpoints. A builder frame is a stack-allocated record of (a) the entered BIR node, (b) the `TapeBuilder` checkpoint marking the tape position at frame entry, (c) the layout-fact cursor, and (d) the recovery policy reference. Rollback restores the tape position from the checkpoint without cloning frames. Existing OpenFrame code in `crates/core/src/runtime/{json,css_l4}/builder.rs` is deletion archaeology; no public substrate API and no generic runtime crate carries an `OpenFrame` type, and the BIR producer never emits a clone-stack frame variant. The "no OpenFrame clone stack" invariant on the Dispatch/speculation row above is the load-bearing rule; the generated builder-frame design is its positive surface.

Per-backend lowering obligations, by variant family — Rust V1 and WASM V1 are in scope; TS production is deferred per Q28 but the BIR shape supports TS lower without retrofit when scope opens:

| Variant family | Rust V1 lowering obligation | WASM V1 lowering obligation |
|---|---|---|
| Entry/control | emit `fn parse_<rule>` plus per-node match arms; preserve control successors as basic blocks | emit guest-resident control-flow instructions; basic blocks reuse Rust shape via shared structural snapshot |
| Dispatch/speculation | emit `match` over dispatch keys with bounded rollback checkpoints (no clone-stack) | emit guest-side jump table backed by linear memory; rollback uses tape position alone |
| Terminal/scanner | emit `&[u8]` slice compares plus generated regex programs; SIMD scanners route through `simd-scan` cfg-gated kernels | emit `wasm-simd128` scanner kernels with portable scalar fallback; regex programs cross-compile via shared automata |
| Pratt/SIMD | emit Pratt LUT and operator-spine state machine; SIMD selection driven by cost-model evidence | emit Pratt LUT into WASM data segment; SIMD selection limited to `simd128` with portable scalar fallback |
| Host/layout/error | emit `host::call_<name>` trait dispatch backed by metadata-resolved primitives; `@error` recovery emits typed recovery shells | emit guest-side host-fn imports plus host-chain marshalling; `@error` recovery routes through host boundary diagnostics |
| Tape/direct/value | emit `TapeEmit` into runtime tape buffer; `DirectBuild` projects typed records borrowing tape spans | emit tape buffer into linear memory; direct projections cross the WASM boundary as host-side typed views |
| Debug/path | emit source-map side tables and `DebugMark` instrumentation behind a `cfg(feature = "debug")` gate | emit source maps into a sidecar segment; `DebugMark` is opt-in via host policy |

Type system algorithm: the V1 type system composes HM equality + Algorithm-W principal schemes (Damas-Milner 1982; Pierce 2002 ch.22) + Pierce-Turner local check/synth (the bidirectional expected-type interface that handles explicit signatures, annotations, directives, chain steps, and directed subsumption edges) + DK13 algorithmic completeness (Dunfield-Krishnaswami 2013; ordered existential contexts, principality tracking, decidability, soundness, completeness, explicit annotation rules for non-principal programs) + finite first-order unification + finite CSP for non-HM choices. Synthesis recovers a type from an expression's structure when no expected type flows in; check verifies an expression against an expected type pushed down from the surrounding context; subsumption mediates between the two at coercion edges. Higher-rank polymorphism is a V1 surface, not a future amendment: rank-1 stays the default for inferred programs (every grammar in the seed set parses as rank-1), and rank-N becomes available where the user writes an explicit `forall` annotation on a `@host fn` or a generic rule signature. The user mandate's "inference stronger than Rust if possible" is honoured by DK13's principality tracking, which admits annotation-elidable polymorphism that Rust requires the programmer to write out (call-site type-arg inference for generic rules eliminates the turbofish equivalent). CSP solves finite non-HM choices for host overload, layout representation, recognizer eligibility, direct/tape materialization, recovery strategy, backend plan, and extraction legality. Host overloads with determining arguments emit explicit improvement constraints, CHR-shaped where applicable, before finite CSP selection. OutsideIn-style implication constraints carry into the solver for branch-local equality plumbing as internal substrate.

Function values + types: function arrow is the canonical first-order type constructor (Milner 1978); `FnType` decomposes through Pottier-Rémy first-order unification, and DK13's application judgment handles function values without further extension. The `Type` non-terminal admits `fn(T) -> U` (see §6), so a `@host fn` parameter may carry a function type: `@host fn map<T, U>(f: fn(T) -> U, xs: [T]) -> [U] { ... }` types under DK13 with `f`'s arrow concrete at the monomorphisation site. The transducer apotheosis follows — a single generic `transducer<I, O>` host-fn plus a `Rule<I, O>` record type expresses every finite-state-transducer use case without a `@transducer` directive. Lambda expressions synthesise an arrow type from body when no expected type flows in (DK13 synthesis mode); they check against an expected arrow type when one does — e.g., from a `fn`-typed parameter or chain-step receiver — under Pierce-Turner check mode. Every captured binding is borrowed by `&'i Tape<'i>`-bounded reference; capture-by-move is forbidden in V1; the `Fn`/`FnMut`/`FnOnce` discrimination Rust exposes is collapsed at the BBNF surface — the lifetime-bounded reference closure is the only V1 form. Match and tuple expressions admit product-type construction and variant dispatch inside `@host fn` bodies (see §6 productions).

Match + tuple typing: a match expression checks each arm's pattern against the scrutinee's type; arm-result types unify across arms under HM equality; exhaustiveness is checked at compile time against the grammar-derived variant set (every `Alt` rule emits an enum per `restart/README.md:115`) and emits `BBNF-PATTERN-NONEXHAUSTIVE` when the cover is incomplete. V1 patterns admit literal, wildcard `_`, identifier binding, constructor-with-payload, and tuple patterns; or-patterns and guards defer to V2. Tuples synthesise the product type from component types and pattern-destructure through the same `Pattern` non-terminal. Tuple typing reuses the existing `TupleType` in `Type` without further extension.

Schema-mining miner: the type system runs telemetry-driven schema inference as a peer of the existing recogniser miners. The miner observes `(rule_shape, layout_decision, value_shape)` triples and proposes named-record / named-enum / sum-type identities through the HM/CSP/DK13 chain — analogous to how Pratt miners propose operator tables. The proposal is a hint, not a constraint; the checker accepts when consistent with HM/DK13 facts and rejects candidates that fail principality or finite-CSP legality. The user mandate's "type algebra + telemetry to generate semantic schemas without explicit annotations in most cases" lands here: BBNF emits named, queryable schemas — `JsonObject` rather than `{string: Json}` — without requiring annotations.

Generic rule typing: V1 generic rules such as `Object<V>` are rank-1 HM parametric schemes generalised at definition and instantiated at each `Ref` / call site. DK13's application judgment infers the call-site type argument from the call context, eliminating the explicit `<Json>` turbofish for inferable cases (the `<V>` declaration at the rule head remains, since the parser must know the rule is generic). Validation materializes a finite `(RuleId, TypeArgs)` instance set reachable from a concrete entry or metadata-declared export; recursive generic cycles require a decreasing structural argument, an explicit return annotation, or rejection with `BBNF-GENERIC-CYCLE`. V1 has no user-facing GADT branch-local equality surface. A later indexed/local-equality feature must arrive by amendment with annotation rules and `BBNF-LOCAL-EQUALITY-ANNOTATION`. GADT/branch-local-equality machinery is internal substrate: the CSP solver carries `Implication { givens, wanted }` constraints that propagate branch-local equalities through to `LayoutFacts`. The substrate is GADT-ready though no grammar production introduces a GADT today; when the surface lands post-V1, the algorithm absorbs it without re-architect. Record narrowing in V1 is finite generated-shape coercion at the surface where source and target shapes are both known. The internal row-polymorphism collapse (Leijen-style scoped labels for layout reasoning) is a `passes::layout` subroutine — record-narrowing decisions across grammars become a single row-unification step rather than enumerated finite coercion candidates. The user-facing row-poly surface defers to a later type-system research gate, not to V1; row variables never appear in the BBNF `Type` non-terminal.

CSP + e-graph composition: e-graph does equivalence and rewrite saturation; CSP does finite legality/choice; cost scores legal alternatives. The bridge exchanges monotone facts keyed by stable Grammar IR node ids, e-class ids, and CSP variables; it does not commit to an e-node representative. Extraction consults solved assignments and emits bridge justifications with selected `CostFacts`.

Cost model API: `Analysis`-style e-class facts, `CostModel` objective scoring, and optional solver-backed extraction are separate layers. They share `CostDecision` evidence across terminal, sequence, alternation, repetition, host call, layout, materialization, SIMD, Pratt, recovery, regex, and generated-code pressure. Extraction records objective vectors, legality, child links, selected alternatives, rejected alternatives, dominated alternatives, scalarization profile, and bridge justification.

BBNF grammar specification: lookbehind, `@host fn`, chains, generics, `@error`, and `@layout` are first-class. Rewrite-mode is not. Unicode class algebra is referenced through regex syntax and owned by regex.

Host-fn primitive library: normal grammars use generic primitives, workspace metadata, and explicit `@host fn` composition. Per-grammar declaration crates are not default; any escape valve is named and audited.

Rare escape-valve fence:

| Field | Requirement |
|---|---|
| Approval owner | SYNTHESIS amendment plus tranche D owner. |
| Failure proof | The amendment states why workspace metadata, generic primitives, and `@host fn` cannot express the adapter. |
| Location | A fenced declaration crate may exist only after Architecture records the exception. |
| Import rule | Generic crates must not import the declaration crate. Generated host tables call through a trait or metadata-dispatched adapter. |
| Deletion path | The exception records the stability condition that deletes the declaration crate or folds it back into metadata/generic primitives. |
| Reviewer | The exception names the reviewer who accepted the failure proof, deletion path, and non-import rule. |
| Extant grammars | Exception table is empty for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, and math. |
| Verification | `rg -n "crates/(json|css|bbnf|sheets|math|csv|bnf|ebnf)" crates/{ir,passes,codegen,runtime,host,path,path-core}` returns zero outside generated data. |
| Canonical review form | Architecture owns the full eight-field declaration-crate review form; PASS-1 keeps this substrate fence in sync and does not admit a declaration crate by itself. |

Error vocabulary: at minimum `Syntax`, `TypeMismatch`, `HostSignature`, `HostFailure`, `LayoutConflict`, `LookbehindWidth`, `RegexClass`, `Recovery`, `BackendUnsupported`, and `InternalInvariant`.

Diagnostic strings owned by PASS-1:

| Code | Verbatim message |
|---|---|
| `BBNF1004` | `lookbehind in rule {rule} must have finite maximum width; {expr} is unbounded after {operator}.` |
| `BBNF1201` | `host function {name} cannot satisfy signature {expected}; argument {index} inferred {actual} at {span}.` |
| `BBNF1302` | `@layout({wanted}) on rule {rule} conflicts with inferred {inferred}; remove the hint or change {field}.` |
| `BBNF1401` | `chain step {step} in rule {rule} expects {expected} but previous step produced {actual}.` |
| `BBNF-HOST004` / `BBNF-SUBSUMPTION-EDGE` | `chain step {step} requires coercion {from} -> {to}, but no directed subsumption rule is registered at this edge.` |
| `BBNF-GENERIC-CYCLE` | `generic rule {rule} produces an unbounded monomorphization set for type arguments {args}; add a return annotation, break the recursive type argument, or route the recursion through a concrete rule.` |
| `BBNF-LOCAL-EQUALITY-ANNOTATION` | `rule {rule} introduces branch-local type equality {equality}; add an explicit return annotation because the inferred type is not principal.` |
| `BBNF-PATTERN-NONEXHAUSTIVE` | `match expression in {rule} does not cover variant {variant}; add an arm or a wildcard.` |
| `BBNF2103` | `rule {rule} was not lowered as Pratt; candidate operator {op} lacks stable precedence metadata.` |
| `BBNF2104` | `rule {rule} stayed scalar because SIMD setup cost {simd_cost} exceeds scalar cost {scalar_cost} for expected length {n}.` |

Each type diagnostic records `expected_from`, `actual_from`, `obligation_id`, and `solver_stage` (`hm-unify`, `check`, `coerce`, `local-equality`, or `csp-choice`). The obligation log is diagnostic evidence retained until `LayoutFacts` and `RecoveryFacts` are emitted; it is not a public pass artefact.

Recovery fact floor: `ErrorDirective` lowering emits `RecoveryFacts` keyed by `RuleId` / `NodeId` with recovery kind, diagnostic code, sync token, typed-placeholder policy, and source span. PASS-3 consumes those facts for recovery nodes, visitors, LSP, and path/value diagnostics. PASS-1 does not create a second parse substrate or runtime recovery API.

VM scope: VM consumes Backend IR and side tables; it does not inspect Grammar IR. Debug hooks expose selected backend operations and extraction evidence.

Multi-function chaining semantics: `a.f(x).g(y)` desugars to nested typed host/map calls with preserved spans and chain-step metadata. It does not require a grammar-specific e-graph node; ffuzzy’s later note says composition can be handled by language derivation (`docs/ffuzzy.md:648`-`docs/ffuzzy.md:672`).

## §3 Per-Crate `src/` Tree

| Crate | Proposed `src/` children |
|---|---|
| `error` | `lib.rs`, `diagnostic/`, `span/`, `kind/`, `render/`, `codes/`, `source_map/` |
| `pipeline` | `lib.rs`, `driver/`, `session/`, `stages/`, `artifacts/`, `incremental/`, `trace/` |
| `source` | `lib.rs`, `input/`, `encoding/`, `span/`, `interner/`, `normalize/`, `diagnostic/` |
| `grammar` | `lib.rs`, `ast/`, `parse/`, `directives/`, `desugar/`, `validate/`, `source_map/` |
| `ir` | `lib.rs`, `grammar_ir/`, `backend_ir/`, `ids/`, `side_tables/`, `serialize/`, `pretty/` |
| `passes` | `lib.rs`, `normalize/`, `types/`, `layout/`, `facts/`, `recognizers/`, `extract/`, `validate/` |
| `vm` | `lib.rs`, `program/`, `ops/`, `runner/`, `stack/`, `debug/`, `profile/` |
| `host` | `lib.rs`, `signature/`, `metadata/`, `registry/`, `chain/`, `primitives/`, `backend/` |
| `cost-model` | `lib.rs`, `weights/`, `score/`, `frontier/`, `solve/`, `evidence/`, `profiles/`, `sota/`, `tiebreak/` |
| `egraph` | `lib.rs`, `arena/`, `class/`, `analysis/`, `rewrite/`, `extract/`, `domains/` |
| `csp-solver` | `lib.rs`, `domain/`, `variable/`, `constraint/`, `propagate/`, `search/`, `optimize/` |
| `parse-that` | `lib.rs`, `regex/`, `automata/`, `unicode/`, `lookaround/`, `compile/`, `diagnostic/` |

This layout follows the README’s greenfield workspace direction (`restart/README.md:29`-`restart/README.md:63`) and Lock 13’s no-god-directory rule (`restart/locks/14-LOCKS.md:58`).

Per-crate rationale:

| Crate | Rationale for these children |
|---|---|
| `error` | `diagnostic/` carries renderable diagnostic records; `span/` owns byte/char/line span types reused by every other crate; `kind/` enumerates error kinds (the §2 error vocabulary); `render/` formats diagnostics for humans and machines; `codes/` binds numeric codes (e.g. `BBNF1004`) and alphabetic aliases (e.g. `BBNF-LOOKBEHIND-WIDTH`) to kinds; `source_map/` maps generated spans back to grammar source. The split keeps render policy out of kind ownership and code binding out of render policy. |
| `pipeline` | `driver/` runs the orchestrator; `session/` owns mutable per-run state; `stages/` carries declarative stage descriptions consumed by the driver; `artifacts/` typed inter-stage results; `incremental/` change tracking; `trace/` structured logging for stage timings and failures. The split keeps mutability out of stage descriptions and tracing out of artifact types. |
| `source` | `input/` reads bytes; `encoding/` normalizes UTF-8 / line endings / BOM; `span/` byte/char position math; `interner/` symbol table; `normalize/` whitespace and comment normalization decisions; `diagnostic/` source-attached diagnostic helpers. The ParseStream rename is dissolved here: source normalization lives under `normalize/` without a substrate-level rename. |
| `grammar` | `ast/` parsed grammar AST; `parse/` BBNF parser; `directives/` `@host`/`@error`/`@layout` parsers and validators; `desugar/` lowers AST to Grammar IR; `validate/` width-proofs and structural invariants run before desugar; `source_map/` maps AST nodes to source spans. The split lets directives evolve without disturbing parse/desugar and keeps validation out of the parser. |
| `ir` | `grammar_ir/` and `backend_ir/` are the two-IR contract from §2; `ids/` carries `RuleId`, `NodeId`, and stable numbering; `side_tables/` holds optimized-IR facts as side tables, never AST; `serialize/` typed serialization for snapshot tests and inter-stage transport; `pretty/` printable forms. Backend IR ownership lives here so `codegen` can never re-own the variant alphabet. |
| `passes` | `normalize/` desugar and canonicalization; `types/` is an internal child invoked by `layout/`, owning HM schemes, expected-type checking, type obligations, directed coercion candidates, and diagnostic provenance; `layout/` is the public pass boundary for `@layout` lowering and `LayoutFacts`; `facts/` fact tables consumed by recognizers and cost model; `recognizers/` recognizer eligibility; `extract/` cost-model extraction evidence; `validate/` cross-pass invariant checks. `TypeFacts` and type-obligation logs remain internal and do not cross as public artefacts. The split assigns one writer per side effect (`docs/precepts/instructions/LESSONS-LEARNED.md:65`-`docs/precepts/instructions/LESSONS-LEARNED.md:72`). |
| `vm` | `program/` typed VM program built from Backend IR; `ops/` operation alphabet; `runner/` execution loop; `stack/` runtime stack/frame management; `debug/` source-map and breakpoint hooks; `profile/` per-op counters. The VM never inspects Grammar IR. |
| `host` | `signature/` typed `@host fn` signatures; `metadata/` workspace metadata host adapter; `registry/` the runtime host table; `chain/` chain-step type flow and dispatch; `primitives/` the generic primitive library; `backend/` backend-specific marshalling. Per-grammar declaration crates live nowhere here; metadata-driven dispatch is the path. |
| `cost-model` | `weights/` configurable scalar weights; `score/` `CostModel` implementations and objective scalarizers; `frontier/` Pareto and lexicographic filtering; `solve/` SMT/ILP-backed composition adapters; `evidence/` selected, rejected, dominated, and bridge-justified alternative logs; `profiles/` named cost profiles (parse-throughput, codegen-size, debug); `sota/` competitor target rows; `tiebreak/` deterministic tiebreaker rules. |
| `egraph` | `arena/` hash-cons arena; `class/` e-class and analysis fact carriers; `analysis/` the `egg::Analysis`-style trait; `rewrite/` rewrite rule plumbing; `extract/` cost-driven extraction; `domains/` per-domain plug-ins (regex, grammar, cost). The `domains/` split is what keeps the e-graph generic and lets PASS-2 add domains without reopening the core. |
| `csp-solver` | `domain/` finite-domain types; `variable/` variable types; `constraint/` constraint definitions and propagators; `propagate/` the propagation engine; `search/` backtracking and ordering; `optimize/` objective-driven optimization. The CSP carries no bbnf grammar-name dispatch. |
| `parse-that` | `regex/` parsed regex AST; `automata/` NFA/DFA construction and Unicode handling; `unicode/` Unicode class algebra (sole owner); `lookaround/` regex-internal lookahead/lookbehind including `(?<=...)`; `compile/` regex-to-program compilation; `diagnostic/` regex-layer diagnostics. Grammar-level `|<` is owned by `grammar` and `passes`, not `parse-that`. |

Sibling API uniformity floor:

| Crate family | Uniform sibling contract |
|---|---|
| `error`, `source`, `grammar` | each child exposes `mod.rs`, a public data type, parse/validate helpers where relevant, and diagnostic spans. |
| `ir`, `passes`, `cost-model` | each child exposes typed ids/facts plus producer and consumer tests; no child emits backend source. |
| `vm`, `host` | each child exposes an execution-facing trait plus error conversion through `error`. |
| `egraph`, `csp-solver`, `parse-that` | each child remains generic and carries no bbnf grammar-name dispatch. |

## §4 Hand-Offs To PASS-2

| Contract | PASS-2 receiver | Blocker | Receiving gate |
|---|---|---|---|
| Grammar IR variant list and field schema | `ir/grammar_ir/`, `grammar/desugar/`, `passes/normalize/` | parser/desugar does not have stable ids | Architecture §7 table consumed by BIR builder tests |
| Backend IR variant list and consumer interface | `ir/backend_ir/`, `vm/program/`, `passes/extract/` | lowerers must not own BIR types | PASS-2 BIR import-deny gate |
| Cost-model trait public API | `cost-model/score/`, `cost-model/frontier/`, `cost-model/solve/`, `cost-model/evidence/`, `passes/extract/` | generated budget, SOTA scores, and dominated alternatives need common evidence | PASS-2 perf/budget table consumes `CostDecision` evidence |
| E-graph rewrite plug-in registry | `egraph/domains/`, `passes/normalize/`, `parse-that-regex/` | bridge facts need stable e-class names | C.W4 bridge tests |
| Host metadata schema | `host/signature/`, `host/metadata/`, `grammar/directives/` | host chains need typed signatures | D.W2 and F.W2 host gates |
| Tape/direct value contract | PASS-2 runtime template and PASS-3 value API | tape ABI must be named | F.W1 runtime template and G.W2 value API gates |

## §5 Hand-Offs To PASS-3

| Contract | PASS-3 receiver | Blocker | Receiving gate |
|---|---|---|---|
| Host-fn dispatch from grammar | Backend host tables consume typed metadata and `HostChain`. | host signature inference must be complete | PASS-3 host docs and WASM host diagnostics |
| Error vocabulary and recovery facts | User-facing surfaces consume typed error kinds, source spans, `RecoveryFacts`, and type-obligation provenance. | diagnostic codes and recovery placeholders need stable source spans | PASS-3 friction ledger |
| Debug VM hooks | Incremental/debug consumers inspect Backend IR ops, side tables, and extraction evidence. | VM must replay BIR | I.W3 debug replay gate |
| Path/value API | Runtime consumers query tape/direct values through one API. | runtime template must emit metadata | G.W1/G.W2 path and value gates |
| Rust/WASM parity | Rust and WASM V1 share Backend IR semantics. | lowerers and runtime template must exist | H/J parity gates |
| TS deferred parity | TS production is deferred by Q28 scope. | public package and path-ts surfaces not yet active | J parity/publication gate |

## §6 BBNF Grammar Formal Specification

The canonical grammar surface excludes rewrite-mode and grammar-level Unicode algebra even though the stale PASS prompt asks about them. Unicode algebra is a regex-layer term.

```ebnf
Grammar        = { Directive | Rule } ;
Directive      = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl ;
ImportDecl     = "@import" "{" Ident { "," Ident } "}" "from" StringLit ";" ;
HostFn         = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? Block ;
ErrorDecl     = "@error" Ident ErrorBody ;
LayoutDecl    = "@layout" Ident LayoutBody ;
PrettyDecl    = "@pretty" Ident PrettyStrategy { PrettyStrategy } ";" ;
PrettyStrategy = "compact" | "group" | "indent" | "hardbreak" | "sep" "(" StringLit ")" | "block" ;
TokenDecl     = "@token" Ident ";" ;
Rule           = "rule"? Ident GenericParams? RuleParams? ReturnType? "=" Expr MapTail? ";" ;
GenericParams  = "<" Ident { "," Ident } ">" ;
RuleParams     = "(" Params? ")" ;
Params         = Param { "," Param } ;
Param          = Ident ":" Type ;
ReturnType     = "->" Type ;
Expr           = Alt ;
Alt            = Seq { "|" Seq } ;
Seq            = Prefix { Prefix } ;
Prefix         = Lookbehind | Predicate | Postfix ;
Lookbehind     = Expr "|<" Expr | Expr "|<!" Expr ;
Predicate      = "&" Postfix | "!" Postfix ;
Postfix        = Primary { "?" | "*" | "+" | RepeatRange } ;
RepeatRange    = "{" Number ( "," Number? )? "}" ;
Primary        = Literal | Regex | Ref | Group | HostCall | LambdaExpr | MatchExpr | TupleExpr ;
Group          = "(" Expr ")" ;
Ref            = Ident GenericArgs? ;
HostCall       = "@" Ident "(" Args? ")" ;
LambdaExpr     = "|" Params? "|" ( Expr | Block ) ;
MatchExpr      = "match" Expr "{" Arm { "," Arm } ","? "}" ;
Arm            = Pattern "=>" Expr ;
Pattern        = Literal | Ident | "_" | Constructor | TuplePattern ;
Constructor    = Ident ( "(" Pattern { "," Pattern } ")" )? ;
TuplePattern   = "(" Pattern "," Pattern { "," Pattern } ")" ;
TupleExpr      = "(" Expr "," Expr { "," Expr } ")" ;
MapTail        = "->" ChainExpr ;
ChainExpr      = Ident { "->" Ident } ;
Regex          = "/" RegexBody "/" RegexFlags? ;
Type           = Ident GenericArgs? | TupleType | RecordType | BorrowType | FnType ;
FnType         = "fn" "(" TypeList? ")" "->" Type ;
TypeList       = Type { "," Type } ;
```

`HostFn` is block-bodied. The production above carries `Block` as the trailing non-terminal; the declaration-only form `HostFn = ... ";"` is rejected and never appears in BBNF source. The block is the sole owner of host-function semantics: it carries the typed body, the optional method-chain expressions, and the closure captures. Bodyless host declarations do not exist; a `@host fn` without a body is a parse error before validation runs.

V1 BBNF directive set: the six-directive `Directive` production above is the complete V1 surface. `@import` carries cross-file grammar composition and is extant in 22+ grammar files (cite: `grammar/bbnf/bbnf.bbnf:4-5`). `@pretty` carries pretty-printing strategy with the verbatim vocabulary `compact`, `group`, `indent`, `hardbreak`, `sep(...)`, `block` preserved across the 30+ extant sites (cite: `grammar/json/json.bbnf:18-20`). `@token` carries atomic-token markers binding to the BIR scanner (cite: `grammar/css/pretty.bbnf:17-19`). `@ws` folds into `@layout(ws = ...)`; `@debug` is a host primitive routed through the host registry, not a directive; standalone `@recover` retires (folded into `@error(recover = ...)`); `@pratt`, `@simd`, `@transducer`, `@rewrite`, and `@unicode` retire entirely. The `directive-canon` lint at ARCHITECTURE §13.1 enforces this set.

Function values + types: V1 BBNF promotes function values to first-class. `Type` admits `FnType = "fn" "(" TypeList? ")" "->" Type`, so a user may write `@host fn map<I, O>(f: fn(I) -> O, xs: [I]) -> [O] { ... }` without a `@transducer` directive — the transducer apotheosis. `LambdaExpr` replaces the prior `Closure` production at the `Primary` site; the surface form is `|x| body` (or `|x| Block`). Lambda captures by `&'i Tape<'i>` reference only; capture-by-move is a parse error in V1; the `Fn` / `FnMut` / `FnOnce` discrimination Rust exposes is absent at the BBNF surface. The lifetime-bounded reference closure is the only V1 form. Closure-capture-by-move and the `Fn*` trait split defer to V2 amendment.

Match + tuple expressions: `MatchExpr` and `TupleExpr` join `Primary`. A match expression has shape `match Expr { Pattern => Expr, ... }`; arms admit literal, wildcard `_`, identifier-binding, constructor-with-payload, and tuple patterns. Or-patterns and guards defer to V2. Tuples are at-least-two-element parenthesised expression lists; one-element parens remain `Group`. The match-expression form is the canonical variant-dispatch surface inside `@host fn` bodies; without it, every Alt-derived enum requires a host-side helper or a chain of `if-let`-style probes. Exhaustiveness is checked against the scrutinee's grammar-derived variant set; non-exhaustive matches emit `BBNF-PATTERN-NONEXHAUSTIVE`. Tuple typing reuses `TupleType`; constructor patterns destructure grammar-derived enums through their declared payload tuples or records.

Regex-style `(?<=...)` belongs only inside regex literals. `RewriteMode` has no production. Unicode class algebra has no BBNF production; regex may accept Unicode class expressions inside `RegexBody`.

Lookbehind finite-width legality: the predicate expression on the left of `|<` (and `|<!`) must have a statically computable finite maximum width in bytes. Constant-width literals, fixed-bounded `RepeatRange` such as `{n}` or `{m,n}`, and alternations whose arms are each finite-width are legal; unbounded operators (`*`, `+`, `?` over a non-finite body, recursive `Ref`) are not. The width is computed during `passes/validate` and stored on the `Lookbehind` node's width-proof slot before lowering. A lookbehind whose predicate is unbounded fails compilation with diagnostic code `BBNF1004`, alphabetic alias `BBNF-LOOKBEHIND-WIDTH`, and error vocabulary kind `LookbehindWidth` (see the §2 error vocabulary and diagnostic strings table). The codegen path never sees an unbounded `Lookbehind`.

Canonical chain syntax and type flow: the rule-level chain form is `Expr -> f1 -> f2 -> f3` where every `fi` is a typed function reference resolvable as either a grammar `Map` step or a `@host fn`. The grammar production at `MapTail = "->" ChainExpr` and `ChainExpr = Ident { "->" Ident }` is the sole form for rule-level chains. Type flow threads the previous step's value type into the next: if step `f_i` produces type `T_i`, then step `f_{i+1}` must accept `T_i` as its first argument, and the chain's final type is the last step's return type. Type checking runs left-to-right and may pass through directed subsumption only when a registered `CoercionCandidate` exists at that chain edge; otherwise it fails at the first mismatch and emits diagnostic `BBNF1401` (alphabetic alias `BBNF-CHAIN-STEP`) plus `BBNF-SUBSUMPTION-EDGE` when the missing edge is a coercion. The method-chain form `a.f(x).g(y)` is permitted only inside `@host fn` bodies, where it desugars (per §2 multi-function chaining semantics) to nested typed host/map calls and carries the same chain-step diagnostic; method-chain syntax is not a grammar-rule surface and must not appear outside a host-fn body.

Closure semantics: compile-time grammar closures beta-reduce where all arguments are grammar values. Current closure beta-reduction code is research signal only (`crates/core/src/lower/expression/closures.rs:19`-`crates/core/src/lower/expression/closures.rs:77`); greenfield reuse requires a fresh spec and verification gate. Host calls remain runtime/typed host expressions rather than grammar macros.

Future grammar onboarding proof:

| Step | Allowed change | Forbidden change | Verification |
|---|---|---|---|
| Add source | `grammars/yaml.bbnf` | `crates/yaml/` or handwritten runtime file | `git diff --name-only` shows the grammar source. |
| Add metadata | `[workspace.metadata.bbnf.grammars.yaml]` | generic-crate match arm or registry edit | `rg -n "Yaml|yaml" crates/{ir,passes,codegen,runtime,host,path,path-core}` returns zero outside generated data. |
| Generate | xtask-emitted runtime/path/visitor metadata | manual fixture as onboarding requirement | generated output is committed and budgeted. |

The onboarding proof counts only the two author inputs: grammar source and workspace metadata. Generated runtime, path, visitor, host, diagnostic, and budget files may appear as xtask outputs; they are not a third onboarding surface and must not require a manual Rust registry edit.

Per-X broad-claim table:

| Claim | Applies to | Proof owner |
|---|---|---|
| normal grammars need no declaration crate | bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, yaml smoke | Architecture Lock 14 table |
| all backends consume Backend IR | Rust V1, WASM V1, TS scaffold | PASS-2 import-deny and parity gates |
| all grammar variation is data or generated code | nine extant grammars plus yaml smoke | PASS-2 runtime emission table |

Generated-code budget schema:

| Column | Meaning |
|---|---|
| `grammar` | metadata grammar ident |
| `baseline_loc` | current generated LOC before tranche |
| `projected_loc` | generated LOC after tranche |
| `allowed_delta` | default ceiling, initially `1.02` unless overridden |
| `pressure_source` | BIR constructs or value/visitor/path feature adding output |
| `regen_wall_ms` | xtask regen/check wall ceiling |
| `evidence` | command output or committed budget report |

## §7 Inheritance Ledger

| Legacy substance | Carries forward | Dissolves | Re-anchor |
|---|---|---|---|
| BA.W2 layout/god-module discipline | Cohesive splits and consumer-coupled substrate work (`docs/tranches/BA/waves/W2.md:1`-`docs/tranches/BA/waves/W2.md:17`). | Exact old rename plan. | PASS-1 type/layout/materialization separation. |
| BA.W4 cursor/unification | Consumer-coupled parse surface pressure. | ParseStream naming. | Source normalization + tape value substrate. |
| BB generality/optimizer | Per-domain optimizers, output piping, Pratt/SIMD auto-detect (`docs/tranches/BB/BB.md:5`-`docs/tranches/BB/BB.md:9`). | Direct-only/tape-dead assumptions. | CSP/e-graph/cost bridge. |
| BB path/visitor | Lazy value/path and visitor pressure (`docs/tranches/BB/BB.md:7`). | Old path-crate exact mechanics. | Single path/value API over tape/direct. |
| BC backend ABI | Backend-agnostic typed IR and multi-backend lowerer pressure (`docs/tranches/BC/BC.md:5`-`docs/tranches/BC/BC.md:24`). | Old one-typed-IR framing. | Grammar IR + Backend IR split. |
| BC regex endpoint | One regex owner pressure. | Grammar-level Unicode algebra. | Regex-layer Unicode algebra in `parse-that`. |
| BD activation | TS/WASM/Rust backend parity and host-fn dispatch pressure (`docs/tranches/BD/BD.md:31`-`docs/tranches/BD/BD.md:47`). | Publication details. | Backend-neutral IR, host metadata, and diagnostics. |

## §8 PASS-1 Punch List

1. Keep Grammar IR and Backend IR schemas inline here and in `restart/ARCHITECTURE.md` §7 rather than routing them to free-floating specs.
2. Keep type-system and host-fn metadata gates inline in Architecture §8 and Tranche D/F gates.
3. Keep CSP/egraph bridge and cost-model evidence in Architecture §7 and Master Tranche C/H gates.
4. Keep rewrite-mode exclusion and regex Unicode delegation in the formal BBNF section.
5. Route tape ABI to PASS-1/Architecture and value/path API to PASS-3/Tranche G.
6. SYNTHESIS must include an input-normalization table for stale ParseStream, rewrite-mode, and grammar-Unicode clauses before any target advances.

## §9 Voice + Discipline Locks

PASS-1 must keep assertions concrete and cited, matching the prompt’s citation discipline (`restart/prompts/PASS-1-SUBSTRATE.md:74`). It must keep substrate work attached to consumers per `docs/precepts/instructions/LESSONS-LEARNED.md:17`-`docs/precepts/instructions/LESSONS-LEARNED.md:26`, use one writer per side effect per `docs/precepts/instructions/LESSONS-LEARNED.md:65`-`docs/precepts/instructions/LESSONS-LEARNED.md:72`, and preserve output-pipe contracts per `docs/precepts/instructions/LESSONS-LEARNED.md:74`-`docs/precepts/instructions/LESSONS-LEARNED.md:80`.

## §10 Closing Posture

The substrate is tape, not ParseStream. Direct-to-struct is a peer materialization, not a replacement. Grammar IR is semantic; Backend IR is executable; optimized IR is side-table evidence. CSP and e-graph are bridged and domain-scoped. BBNF keeps lookbehind, host functions, chaining, generics, `@error`, and `@layout`; it rejects rewrite-mode and sends Unicode class algebra to regex. SYNTHESIS must reconcile conflicting sister-pass outputs before any target is treated as settled.

The independent-proceed wording is retired: any prior text framing PASS-2 and PASS-3 as free to advance independently is dissolved by the reconcile-first sentence above. Sister-pass outputs flow through SYNTHESIS for conflict reconciliation before they govern tranche execution.

No legacy closure code is inherited by default. The closure beta-reduction implementation cited in §6 is research signal: it documents prior failure modes and ratifies the compile-time-only beta-reduction rule, but the greenfield reuse path is a fresh spec plus a verification gate, not a port. Greenfield grammar/closure work begins from the §6 closure-semantics rule and the BBNF formal grammar; the existing module is contestable until ratified.

OpenFrame substrate is deletion-path archaeology. The §2 Backend IR ownership table forbids clone-stack OpenFrames and the §2 builder-frame replacement names the positive surface (generated BIR builder frames keyed by `RuleId`/`NodeId` plus `TapeBuilder` checkpoints). No public substrate API and no generic runtime crate carries an `OpenFrame` type after restart.
