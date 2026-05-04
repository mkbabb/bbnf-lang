# PASS-1 Synthesis: Substrate

## §1 PASS-1 Verdict Ledger

| Concern | Agent | Evidence | Decision |
|---|---|---|---|
| Tape substrate | 1, 4, 6 | `restart/README.md:285`-`restart/README.md:315`; `restart/locks/14-LOCKS.md:34` | KEEP tape as substrate and union it with direct-to-struct. |
| ParseStream naming | 1, 6 | `restart/inheritance/INDEX.md:66`; stale README cells at `restart/README.md:391`, `restart/README.md:473` | DISCARD rename; retain source normalization/span-map work under source modules. |
| Grammar IR | 1 | `restart/README.md:104`-`restart/README.md:118`; current node mix at `crates/ir/src/types/node.rs:30`-`crates/ir/src/types/node.rs:98` | REINVENT as semantic 12-15 variant IR. |
| Backend IR | 1, 3, 4 | BC table at `docs/tranches/BC/audit/W0-typed-ir-variant-table.md:160`-`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:254` | KEEP about 22 executable variants. |
| Optimized IR | 1, 3 | `restart/README.md:114`-`restart/README.md:118` | KEEP as side tables, not AST. |
| Type system | 2 | `restart/README.md:258`-`restart/README.md:268` | KEEP HM + bidirectional + CSP-backed constrained unification. |
| CSP/e-graph | 3 | `restart/README.md:219`-`restart/README.md:228`; `restart/locks/14-LOCKS.md:40` | KEEP bridged, per-domain composition. |
| Cost model | 4 | `restart/README.md:211`-`restart/README.md:218`; `restart/locks/14-LOCKS.md:48` | KEEP trait-based scoring with SOTA gates and extraction evidence. |
| Lookbehind | 5 | `restart/README.md:124`-`restart/README.md:132` | KEEP. |
| Rewrite-mode | 5 | Prompt asks at `restart/prompts/PASS-1-SUBSTRATE.md:3`, `restart/prompts/PASS-1-SUBSTRATE.md:31`, `restart/prompts/PASS-1-SUBSTRATE.md:66`; README rejects at `restart/README.md:134`-`restart/README.md:137` | DISCARD. |
| Unicode class algebra | 5 | README defers at `restart/README.md:139`-`restart/README.md:143` | DEFER to regex layer. |
| Host fn/chains/generics | 2, 5 | `restart/README.md:145`-`restart/README.md:164`; `docs/ffuzzy.md:648`-`docs/ffuzzy.md:672` | KEEP. |
| `@error` / `@layout` | 2, 5 | `restart/README.md:166`-`restart/README.md:174` | KEEP as typed directives. |
| Per-grammar declaration crates | 2, 5 | `AMENDMENT-01`: `restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:7`-`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:24` | DISCARD as default; rare escape only. |

## §2 Substrate Architectural Commitments

Grammar IR shape: semantic nodes only. Proposed variants are `Rule`, `Seq`, `Alt`, `Repeat`, `Optional`, `Literal`, `Regex`, `Ref`, `Predicate`, `Lookbehind`, `Map`, `HostCall`, `LayoutDirective`, `ErrorDirective`, and `Annotation`.

Backend IR shape: executable plan nodes. Proposed variants are `Entry`, `Seq`, `DispatchAlt`, `SpeculativeAlt`, `RepeatLoop`, `OptionalBranch`, `ByteLiteral`, `RegexProgram`, `SimdScan`, `PrattSpine`, `CallRule`, `CallHost`, `HostChain`, `LayoutPush`, `LayoutPop`, `ErrorRecover`, `SpanMark`, `TapeEmit`, `DirectBuild`, `ValueProject`, `PathEval`, and `DebugMark`.

Type system algorithm: HM inference generates core constraints; bidirectional checking handles explicit signatures/directives; CSP-backed constrained unification solves finite choices for host overload, layout representation, recognizer eligibility, direct/tape materialization, recovery strategy, and backend plan.

CSP + e-graph composition: e-graph does equivalence and rewrite saturation; CSP does finite legality/choice; cost scores legal alternatives. The bridge is facts from e-class analysis into CSP and decisions from CSP into extraction.

Cost model trait: an `AnalysisCost`-style interface scores terminal, sequence, alternation, repetition, host call, layout, materialization, SIMD, Pratt, recovery, and generated-code pressure. Extraction records selected and rejected alternatives.

BBNF grammar specification: lookbehind, `@host fn`, chains, generics, `@error`, and `@layout` are first-class. Rewrite-mode is not. Unicode class algebra is referenced through regex syntax and owned by regex.

Host-fn primitive library: normal grammars use generic primitives, workspace metadata, and explicit `@host fn` composition. Per-grammar declaration crates are not default; any escape valve is named and audited.

Error vocabulary: at minimum `Syntax`, `TypeMismatch`, `HostSignature`, `HostFailure`, `LayoutConflict`, `LookbehindWidth`, `RegexClass`, `Recovery`, `BackendUnsupported`, and `InternalInvariant`.

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
| `cost-model` | `lib.rs`, `weights/`, `score/`, `evidence/`, `profiles/`, `sota/`, `tiebreak/` |
| `egraph` | `lib.rs`, `arena/`, `class/`, `analysis/`, `rewrite/`, `extract/`, `domains/` |
| `csp-solver` | `lib.rs`, `domain/`, `variable/`, `constraint/`, `propagate/`, `search/`, `optimize/` |
| `parse-that` | `lib.rs`, `regex/`, `automata/`, `unicode/`, `lookaround/`, `compile/`, `diagnostic/` |

This layout follows the README’s greenfield workspace direction (`restart/README.md:29`-`restart/README.md:63`) and Lock 13’s no-god-directory rule (`restart/locks/14-LOCKS.md:58`).

## §4 Hand-Offs To PASS-2

| Contract | PASS-2 receiver |
|---|---|
| Grammar IR variant list and field schema | `ir/grammar_ir/`, `grammar/desugar/`, `passes/normalize/` |
| Backend IR variant list and consumer interface | `ir/backend_ir/`, `vm/program/`, `passes/extract/` |
| Cost-model trait public API | `cost-model/score/`, `cost-model/evidence/`, `passes/extract/` |
| E-graph rewrite plug-in registry | `egraph/domains/`, `passes/normalize/`, `parse-that/regex/` |
| Host metadata schema | `host/signature/`, `host/metadata/`, `grammar/directives/` |
| Tape/direct value contract | `ir/side_tables/`, later value/runtime modules, `vm/runner/` |

## §5 Hand-Offs To PASS-3

| Contract | PASS-3 receiver |
|---|---|
| Host-fn dispatch from grammar | Backend host tables consume typed metadata and `HostChain`. |
| Error vocabulary | User-facing surfaces consume typed error kinds and source spans. |
| Debug VM hooks | Incremental/debug consumers inspect Backend IR ops, side tables, and extraction evidence. |
| Path/value API | Runtime consumers query tape/direct values through one API. |
| Cross-backend parity | Rust/TS/WASM later share Backend IR semantics; BD parity pressure is inherited (`docs/tranches/BD/BD.md:31`-`docs/tranches/BD/BD.md:36`). |

## §6 BBNF Grammar Formal Specification

The canonical grammar surface excludes rewrite-mode and grammar-level Unicode algebra even though the stale PASS prompt asks about them. Unicode algebra is a regex-layer term.

```ebnf
Grammar        = { Directive | Rule } ;
Directive      = HostFn | ErrorDecl | LayoutDecl ;
HostFn         = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? ";" ;
ErrorDecl      = "@error" Ident ErrorBody ;
LayoutDecl     = "@layout" Ident LayoutBody ;
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
Lookbehind     = "(?<=" Expr ")" | "(?<!" Expr ")" ;
Predicate      = "&" Postfix | "!" Postfix ;
Postfix        = Primary { "?" | "*" | "+" | RepeatRange } ;
RepeatRange    = "{" Number ( "," Number? )? "}" ;
Primary        = Literal | Regex | Ref | Group | HostCall | Closure ;
Group          = "(" Expr ")" ;
Ref            = Ident GenericArgs? ;
HostCall       = "@" Ident "(" Args? ")" ;
Closure        = "|" Params? "|" Expr ;
MapTail        = "=>" ChainExpr ;
ChainExpr      = Atom { "." Ident "(" Args? ")" } ;
Regex          = "/" RegexBody "/" RegexFlags? ;
Type           = Ident GenericArgs? | TupleType | RecordType | BorrowType ;
```

`RewriteMode` has no production. Unicode class algebra has no BBNF production; regex may accept Unicode class expressions inside `RegexBody`.

Closure semantics: compile-time grammar closures beta-reduce where all arguments are grammar values; current source already has closure beta-reduction machinery (`crates/core/src/lower/expression/closures.rs:19`-`crates/core/src/lower/expression/closures.rs:77`). Host calls remain runtime/typed host expressions rather than grammar macros.

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

1. Draft `restart/specs/pass-1/grammar-ir.md` and `backend-ir.md`.
2. Draft `restart/specs/pass-1/type-system.md` and `host-fn-metadata.md`.
3. Draft `restart/specs/pass-1/csp-egraph-bridge.md` and `cost-model.md`.
4. Draft `restart/specs/pass-1/bbnf-grammar.md` with rewrite-mode exclusion and regex Unicode delegation.
5. Draft `restart/specs/pass-1/tape-value-api.md` and `path-api.md`.
6. Audit stale ParseStream/rewrite-mode/Unicode clauses during SYNTHESIS reconciliation.

## §9 Voice + Discipline Locks

PASS-1 must keep assertions concrete and cited, matching the prompt’s citation discipline (`restart/prompts/PASS-1-SUBSTRATE.md:74`). It must keep substrate work attached to consumers per `docs/precepts/instructions/LESSONS-LEARNED.md:17`-`docs/precepts/instructions/LESSONS-LEARNED.md:26`, use one writer per side effect per `docs/precepts/instructions/LESSONS-LEARNED.md:65`-`docs/precepts/instructions/LESSONS-LEARNED.md:72`, and preserve output-pipe contracts per `docs/precepts/instructions/LESSONS-LEARNED.md:74`-`docs/precepts/instructions/LESSONS-LEARNED.md:80`.

## §10 Closing Posture

The substrate is tape, not ParseStream. Direct-to-struct is a peer materialization, not a replacement. Grammar IR is semantic; Backend IR is executable; optimized IR is side-table evidence. CSP and e-graph are bridged and domain-scoped. BBNF keeps lookbehind, host functions, chaining, generics, `@error`, and `@layout`; it rejects rewrite-mode and sends Unicode class algebra to regex. PASS-2 and PASS-3 may proceed independently, but SYNTHESIS must reconcile any conflicting ParseStream, rewrite-mode, Unicode-algebra, direct-only, or per-grammar-crate proposals against this PASS-1 ledger.
