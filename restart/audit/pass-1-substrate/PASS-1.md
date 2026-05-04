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

Grammar IR schema floor:

| Variant | Required fields | Stable key | Producer | Consumer | Forbidden leakage |
|---|---|---|---|---|---|
| `Rule` | name, generics, body, attrs, source span | `RuleId` | `grammar/desugar` | `passes/validate`, `passes/layout` | no backend emission data |
| `Seq` / `Alt` | child ids, separator/progress facts, source span | `NodeId` | `grammar/desugar` | recognizers, e-graph, cost model | no Rust/WASM lowering policy |
| `Repeat` / `Optional` | body id, bounds, progress proof | `NodeId` | `grammar/desugar` | recognizers, error recovery, cost model | no loop code shape |
| `Literal` / `Regex` | literal bytes or regex body, flags, source span | `NodeId` | parser | regex compiler, scanner miner | no scanner kernel choice |
| `Lookbehind` | predicate id, body id, width proof slot | `NodeId` | parser | width checker, Backend IR builder | no unbounded predicate |
| `Map` / `HostCall` | chain steps, argument ids, expected type | `NodeId` | parser + type checker | host inference, layout lowering | no per-grammar host crate reference |
| `LayoutDirective` / `ErrorDirective` | directive value, owner rule, override reason | `RuleId` + directive kind | parser | layout/error fact builders | no runtime recovery code |
| `Annotation` | key, value, source span | `NodeId` | parser | diagnostics and metadata lowering | no backend-specific key |

Backend IR shape: executable plan nodes. Proposed variants are `Entry`, `Seq`, `DispatchAlt`, `SpeculativeAlt`, `RepeatLoop`, `OptionalBranch`, `ByteLiteral`, `RegexProgram`, `SimdScan`, `PrattSpine`, `CallRule`, `CallHost`, `HostChain`, `LayoutPush`, `LayoutPop`, `ErrorRecover`, `SpanMark`, `TapeEmit`, `DirectBuild`, `ValueProject`, `PathEval`, and `DebugMark`.

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

Type system algorithm: HM inference generates core constraints; bidirectional checking handles explicit signatures/directives; CSP-backed constrained unification solves finite choices for host overload, layout representation, recognizer eligibility, direct/tape materialization, recovery strategy, and backend plan.

CSP + e-graph composition: e-graph does equivalence and rewrite saturation; CSP does finite legality/choice; cost scores legal alternatives. The bridge is facts from e-class analysis into CSP and decisions from CSP into extraction.

Cost model trait: an `AnalysisCost`-style interface scores terminal, sequence, alternation, repetition, host call, layout, materialization, SIMD, Pratt, recovery, and generated-code pressure. Extraction records selected and rejected alternatives.

BBNF grammar specification: lookbehind, `@host fn`, chains, generics, `@error`, and `@layout` are first-class. Rewrite-mode is not. Unicode class algebra is referenced through regex syntax and owned by regex.

Host-fn primitive library: normal grammars use generic primitives, workspace metadata, and explicit `@host fn` composition. Per-grammar declaration crates are not default; any escape valve is named and audited.

Rare escape-valve fence:

| Field | Requirement |
|---|---|
| Approval owner | SYNTHESIS amendment plus tranche D owner. |
| Failure proof | The amendment states why workspace metadata, generic primitives, and `@host fn` cannot express the adapter. |
| Location | A fenced declaration crate may exist only after Architecture records the exception. |
| Import rule | Generic crates must not import the declaration crate. Generated host tables call through a trait or metadata-dispatched adapter. |
| Extant grammars | Exception table is empty for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, and math. |
| Verification | `rg -n "crates/(json|css|bbnf|sheets|math|csv|bnf|ebnf)" crates/{ir,passes,codegen,runtime,host,path,path-core}` returns zero outside generated data. |

Error vocabulary: at minimum `Syntax`, `TypeMismatch`, `HostSignature`, `HostFailure`, `LayoutConflict`, `LookbehindWidth`, `RegexClass`, `Recovery`, `BackendUnsupported`, and `InternalInvariant`.

Diagnostic strings owned by PASS-1:

| Code | Verbatim message |
|---|---|
| `BBNF1004` | `lookbehind in rule {rule} must have finite maximum width; {expr} is unbounded after {operator}.` |
| `BBNF1201` | `host function {name} cannot satisfy signature {expected}; argument {index} inferred {actual} at {span}.` |
| `BBNF1302` | `@layout({wanted}) on rule {rule} conflicts with inferred {inferred}; remove the hint or change {field}.` |
| `BBNF1401` | `chain step {step} in rule {rule} expects {expected} but previous step produced {actual}.` |
| `BBNF2103` | `rule {rule} was not lowered as Pratt; candidate operator {op} lacks stable precedence metadata.` |
| `BBNF2104` | `rule {rule} stayed scalar because SIMD setup cost {simd_cost} exceeds scalar cost {scalar_cost} for expected length {n}.` |

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
| Cost-model trait public API | `cost-model/score/`, `cost-model/evidence/`, `passes/extract/` | generated budget and SOTA scores need common evidence | PASS-2 perf/budget table consumes cost evidence |
| E-graph rewrite plug-in registry | `egraph/domains/`, `passes/normalize/`, `parse-that/regex/` | bridge facts need stable e-class names | C.W4 bridge tests |
| Host metadata schema | `host/signature/`, `host/metadata/`, `grammar/directives/` | host chains need typed signatures | D.W2 and F.W2 host gates |
| Tape/direct value contract | PASS-2 runtime template and PASS-3 value API | tape ABI must be named | F.W1 runtime template and G.W2 value API gates |

## §5 Hand-Offs To PASS-3

| Contract | PASS-3 receiver | Blocker | Receiving gate |
|---|---|---|---|
| Host-fn dispatch from grammar | Backend host tables consume typed metadata and `HostChain`. | host signature inference must be complete | PASS-3 host docs and WASM host diagnostics |
| Error vocabulary | User-facing surfaces consume typed error kinds and source spans. | diagnostic codes need stable source spans | PASS-3 friction ledger |
| Debug VM hooks | Incremental/debug consumers inspect Backend IR ops, side tables, and extraction evidence. | VM must replay BIR | I.W3 debug replay gate |
| Path/value API | Runtime consumers query tape/direct values through one API. | runtime template must emit metadata | G.W1/G.W2 path and value gates |
| Rust/WASM parity | Rust and WASM V1 share Backend IR semantics. | lowerers and runtime template must exist | H/J parity gates |
| TS deferred parity | TS production is deferred by Q28 scope. | public package and path-ts surfaces not yet active | J parity/publication gate |

## §6 BBNF Grammar Formal Specification

The canonical grammar surface excludes rewrite-mode and grammar-level Unicode algebra even though the stale PASS prompt asks about them. Unicode algebra is a regex-layer term.

```ebnf
Grammar        = { Directive | Rule } ;
Directive      = HostFn | ErrorDecl | LayoutDecl ;
HostFn         = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? Block ;
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
Lookbehind     = Expr "|<" Expr | Expr "|<!" Expr ;
Predicate      = "&" Postfix | "!" Postfix ;
Postfix        = Primary { "?" | "*" | "+" | RepeatRange } ;
RepeatRange    = "{" Number ( "," Number? )? "}" ;
Primary        = Literal | Regex | Ref | Group | HostCall | Closure ;
Group          = "(" Expr ")" ;
Ref            = Ident GenericArgs? ;
HostCall       = "@" Ident "(" Args? ")" ;
Closure        = "|" Params? "|" Expr ;
MapTail        = "->" ChainExpr ;
ChainExpr      = Ident { "->" Ident } ;
Regex          = "/" RegexBody "/" RegexFlags? ;
Type           = Ident GenericArgs? | TupleType | RecordType | BorrowType ;
```

Regex-style `(?<=...)` belongs only inside regex literals. `RewriteMode` has no production. Unicode class algebra has no BBNF production; regex may accept Unicode class expressions inside `RegexBody`.

Closure semantics: compile-time grammar closures beta-reduce where all arguments are grammar values. Current closure beta-reduction code is research signal only (`crates/core/src/lower/expression/closures.rs:19`-`crates/core/src/lower/expression/closures.rs:77`); greenfield reuse requires a fresh spec and verification gate. Host calls remain runtime/typed host expressions rather than grammar macros.

Future grammar onboarding proof:

| Step | Allowed change | Forbidden change | Verification |
|---|---|---|---|
| Add source | `grammars/yaml.bbnf` | `crates/yaml/` or handwritten runtime file | `git diff --name-only` shows the grammar source. |
| Add metadata | `[workspace.metadata.bbnf.grammars.yaml]` | generic-crate match arm or registry edit | `rg -n "Yaml|yaml" crates/{ir,passes,codegen,runtime,host,path,path-core}` returns zero outside generated data. |
| Generate | xtask-emitted runtime/path/visitor metadata | manual fixture as onboarding requirement | generated output is committed and budgeted. |

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
