# Deferral Audit 3 — BBNF Surface, @directives, and Naming

Greenfield-mandate audit of the BBNF V1 source surface: what grammar productions land, what `@directives` are admitted, and what corpus-wide rename ledger the user's `pointer! -> path!` directive begets. The audit reads V6-READY corpus (`restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md`), the three pass syntheses, the master trio (`restart/{ARCHITECTURE,MASTER-PLAN,MIGRATION}.md`), the 14 locks, and the ffuzzy three-primitive provenance.

---

## §1 — Scope and Corpus References

### §1.1 Required reading walked

| Source | Anchor | Use |
|---|---|---|
| `restart/README.md:121-183` | §5 BBNF Extensions | settled extension surface; `@host fn`, `@error`, `@layout`, lookbehind `\|<`, generics, multi-function chaining, rich regex routed through parse-that |
| `restart/README.md:182` | "no `@pratt` / `@simd` / `@phf` directives" | retired-directive list per Lock 10 |
| `restart/audit/pass-1-substrate/PASS-1.md:81-83`, `:194-227` | §2 substrate commitments + §6 formal grammar | canonical EBNF; rule-level chain via `MapTail`; `HostFn` block-bodied; rewrite-mode and grammar Unicode algebra rejected |
| `restart/audit/pass-3-runtime/PASS-3.md:190-191`, `:438-460` | §5 error/recovery surface + §6b diagnostic ledger | `@error(recover = ...)` accepted; legacy bare `@recover` is migration-alias only; verbatim diagnostic strings own the `BBNF-POINTER001/002/003` codes |
| `restart/ARCHITECTURE.md:1067-1135` | §8 BBNF Language Surface | grammar sketch + directive ledger + input-normalization deletions table |
| `restart/ARCHITECTURE.md:1015-1058` | §7.4 Diagnostic Vocabulary | consolidated `BBNF-*` code catalogue (binds the rename surface) |
| `restart/audit/pass-3-runtime/PASS-3.md:84-122` | §3 Path / select commitments | `pointer!` and `select!` validation surface; canonical macro spelling |
| `docs/ffuzzy.md:597-645` | three-primitive walkback | `@transducer` proposal retracted; runtime walker is ~30 LOC; user kept lookbehind, dropped rewrite-mode and grammar-level Unicode |
| `restart/locks/LOCKS.md:46`, `:52`, `:60` | Lock 7, Lock 10, Lock 14 | path crate consolidation; auto-detect Pratt/SIMD; full grammar generalisation |

### §1.2 Audit posture

- **Lean, small, robust** is the standard for the V1 surface. A directive earns survival by (a) carrying intent the type system or shape miner cannot infer, (b) decomposing into a single, well-typed lowering edge, and (c) failing closed when the underlying contract is not reachable.
- The user's two corpus-wide renames stand:
  1. `pointer!` → `path!` (macro rename; better matches path semantics; no collision with `path` / `path-core` / `path-ts` crates by the `vec!` / `Vec` analogy).
  2. `BBNF-POINTER*` → `BBNF-PATH*` (diagnostic-code rename; consistent with the macro rename).
- `regex-automata` retirement is audit-#4 territory; this audit treats parse-that's regex layer as the regex owner and notes only the surface impact (rich regex within grammar literals).
- Transducer-without-directive: the ffuzzy `@transducer` proposal stays retired. The runtime walker pattern lives at runtime/host, not at the grammar surface. Lookbehind `\|<` plus typed `@host fn` chaining plus generics expresses every rewriter the user cited as an MVP target.

---

## §2 — Grammar Surface Deferrals (table)

The greenfield BBNF EBNF lives at `restart/audit/pass-1-substrate/PASS-1.md:194-227` and is mirrored at `restart/ARCHITECTURE.md:1077-1112`. Per-row disposition for syntactic features that could open against V1:

| Item | Path:line | Status in V1 | Disposition | Rationale |
|---|---|---|---|---|
| Function-type productions in `Type` | `PASS-1.md:222`, `ARCHITECTURE.md:1102` | `Type ::= Ident GenericArgs? \| TupleType \| RecordType \| BorrowType` — no arrow type | **DEFER** | Function types appear only as host-fn signatures; HM owns those at the `@host fn` `(Params) -> Type` boundary. A grammar-level `T -> U` arrow opens currying surface that V1's rank-1 HM does not need. Receiver: post-V1 amendment if higher-rank surface lands. Cross-ref audit #2 (function/value). |
| Lambda / anonymous-function literal | `PASS-1.md:218` carries `Closure ::= "\|" Params? "\|" Expr` | **PRESENT** at expression position; full first-class lambda is **DEFER** | **KEEP-NARROW** | Closures exist for host chains, map closures, predicate closures, recovery closures — `ARCHITECTURE.md:1187-1208` enumerates the four closure forms. Forbidden behaviours (`ARCHITECTURE.md:1200-1207`) keep BBNF from becoming a general programming language. No expansion required at V1. |
| Type-application / generic call-site syntax | `PASS-1.md:216` `Ref ::= Ident GenericArgs?`; `RuleParams ::= "(" Params? ")"` | **PRESENT** for generic rules (`Object<V>`); no first-class generic-fn-call form | **KEEP** | `Object<V>` works (rank-1 HM scheme; V1 first-class per `restart/README.md:168-170`). Function-call generic application is folded into `Ref` + `HostCall`. No widening needed. |
| `match` expression / pattern matching literal | absent from EBNF | **DEFER** | **DEFER** | Pattern dispatch lives in BIR `DispatchAlt` / `SpeculativeAlt` — selected by cost model, not authored. Surface-level `match` would shadow `\|` alternatives without earning expressiveness. Receiver: post-V1 amendment if the visitor surface proves insufficient. |
| Record / struct literal in expression position | absent | **DEFER** | **DEFER** | Record values are produced by typed `Map` projections (`->` chain) over rule captures; `DirectBuild` lowers them. Authoring records inside expressions duplicates what layout lowering already infers. |
| Variant / enum literal in expression position | absent | **DEFER** | **DEFER** | Enums emerge from `Alt` shape mining; variant tags belong to the codegen target, not the source. Authoring tag construction would re-couple BBNF to backend identity, violating Lock 14's grammar-generalisation posture. |
| Pipe operators (`\|>`, `>>`) beyond `->` | absent | **DISCARD** | **DISCARD** | The `MapTail = "->" ChainExpr` (`PASS-1.md:219-220`) form is canonical and threads HM types left-to-right. Adding `\|>` / `>>` invents synonyms with no semantic gain and confuses the diagnostic surface (`BBNF1401` chain-step codes already cite `->`). |
| `let` bindings in expression context | absent | **DEFER** | **DEFER** | Captures are introduced by `RuleParams`, `Closure` params, and chain-step bindings; an expression-level `let` would force HM to track block-scoped existentials, opening the GADT/local-equality gate the type system explicitly closes (`ARCHITECTURE.md:1161-1166`, diagnostic `BBNF-LOCAL-EQUALITY-ANNOTATION`). |
| `if` / `else` as expressions | absent | **DEFER** | **DEFER** | Branching is owned by `Alt` (parser branching) and by `@host fn` block bodies (host computational branching). A grammar-surface `if` blurs the parser/host boundary the architecture is at pains to keep separate. |
| Annotation surface `e : T` (ascription) | `Ref ::= Ident GenericArgs?` and `MapTail`/`ReturnType` carry typed annotation; no infix `:` | **PRESENT** as `ReturnType ::= "->" Type` and `Param ::= Ident ":" Type`; **no infix expression-level `:`** | **KEEP** | The hybrid annotation surface (`restart/README.md:264-266`) is honoured at the rule, generic-param, host-fn-param, and return-type sites. An expression-level `e : T` would multiply diagnostic provenance without adding inference power; HM principal schemes already converge without it. |
| Currently deferred V1 grammar items per Lock 10 | `restart/locks/LOCKS.md:52` | retired `@pratt` / `@simd` directives; auto-detection only | **DISCARD** | Honoured by `restart/README.md:180-183`; mentioned as forbidden output at `restart/MASTER-PLAN.md:204` and HARDENING-CONSOLIDATED-V6.md:151. No surface; no diagnostic teaches them. |
| GADT / higher-rank / row-polymorphism | `ARCHITECTURE.md:1161-1171`, `PASS-1.md:74-75` | **DEFER** with named diagnostic | **DEFER** | `BBNF-LOCAL-EQUALITY-ANNOTATION` is the named amendment gate. V1 is rank-1 HM; opening this surface requires reopening the type-system proof (Dunfield-Krishnaswami / OutsideIn(X)). |
| Open-row / structural record subtyping | `PASS-1.md:75`, `ARCHITECTURE.md:1168-1171` | **DEFER** | **DEFER** | Record narrowing in V1 is finite generated-shape coercion only; row polymorphism routes to a later type-system gate. |

### §2.1 Grammar-surface verdict

The V1 grammar surface as `PASS-1.md:194-227` carries it is the **lean, small, robust** floor. Deferrals above are not waiting on tranches — they are post-V1 amendment gates. None opens here. The grammar onboarding test (`yaml.bbnf` plus one metadata block, `ARCHITECTURE.md:1331-1376`) is the verification surface; if a grammar onboarding cannot be expressed inside the §2 row set, that grammar opens an amendment, not a hidden V1 fold.

---

## §3 — @directive Lean Inventory (per-directive disposition)

### §3.1 Surviving directives

| Directive | Surface form | Path:line | Minimal V1 form | Justification | Acceptance gate |
|---|---|---|---|---|---|
| `@host fn` | block-bodied, generic params, params, return type, attrs, body | `PASS-1.md:197`, `ARCHITECTURE.md:1081-1082` | `HostFn ::= "@host" "fn" Ident GenericParams? "(" ParamList? ")" "->" Type HostAttrs? Block` (block mandatory) | The single bridge between grammar-derived parsing and host computation. Decomposes through generic `host::primitives` plus typed chain composition; HM unifies signature + body. Block is mandatory (`PASS-1.md:225` rejects bodyless). | D.W2 (`MASTER-PLAN.md:348`); host-call compiles without declaration crate. |
| `@error(recover = ...)` | per-rule recovery directive | `PASS-1.md:198`, `ARCHITECTURE.md:1086`, `MASTER-PLAN.md:413` | `ErrorDecl ::= "@error" Ident ErrorBody` where `ErrorBody` carries `recover = ...` | Recovery vocabulary the cost model cannot infer (treesitter-class MISSING/ERROR + sync sets). Lowers to `RecoveryFacts` + BIR `ErrorRecover`. | D.W4 (`MASTER-PLAN.md:350`); standalone `@recover` must fail; F.W2 host/layout/error closes per `MASTER-PLAN.md:413`. |
| `@layout` | per-rule layout-policy override | `PASS-1.md:199`, `ARCHITECTURE.md:1085`, `MASTER-PLAN.md:413,802` | `LayoutDecl ::= "@layout" Ident LayoutBody` | Override surface for layout lowering when inference disagrees with author intent (`restart/README.md:176-178`). Lowers to `LayoutFacts` + BIR `LayoutPush` / `LayoutPop`. | D.W4 + F.W2; `BBNF-LAYOUT-CONFLICT` and `BBNF-LAYOUT-UNCLOSED` codes catalogued at `ARCHITECTURE.md:1036-1037`. |
| `@host fn` attribute set (`HostAttrs`) | optional attributes on host-fn signature | `PASS-1.md:197`, `ARCHITECTURE.md:1082` | `HostAttrs?` admit a small named set; not a freeform metadata escape | Declarative shape — no host-side execution at attribute time. The set is closed; new attributes enter through SYNTHESIS amendment. | D.W2 closes attribute set; metadata + signature unification gates. |

### §3.2 Retired directives (verification of zero positive surface)

| Directive | Path:line | Status | Verification |
|---|---|---|---|
| `@pratt` | `restart/README.md:182`, `restart/locks/LOCKS.md:52`, `MASTER-PLAN.md:204` | **RETIRED** | `rg "@pratt" restart/{README,ARCHITECTURE,MASTER-PLAN,MIGRATION}.md restart/audit/pass-*` returns only Lock 10 prohibition rows and forbidden-output rows. `BBNF-OPT001` (`ARCHITECTURE.md:1040`) explains rejection by cost evidence; the diagnostic body does not teach `@pratt` (`restart/audit/hardening/HARDENING-PASS-2-V5.md:222`). |
| `@simd` | as above; plus `restart/research/topic-8-simd-dfa.md:511`, `:861` | **RETIRED** | `BBNF-OPT002` (`ARCHITECTURE.md:1041`) explains rejection without `@simd hint may force` wording. PASS-2 V5.1 amendment closed the leak. |
| `@phf` | `restart/README.md:182` | **RETIRED** | Auto-detected via `passes::recognizers` keyword-set detection. |
| `@recover` (standalone, top-level) | `ARCHITECTURE.md:1134`, `PASS-3.md:190-191` | **FOLDED** into `@error(recover = ...)` | "PASS-3 amendment; no production for `Recover ::= ...` outside `@error` body." Migration-alias only if SYNTHESIS keeps a parser hook (`PASS-3.md:190`). The greenfield grammar parser rejects bare `@recover`. |
| `@transducer` | `docs/ffuzzy.md:25-50`, `:594-645` | **REJECTED** | The user's own walkback at `docs/ffuzzy.md:614-624` ratifies it — the runtime walker is ~30 LOC and does not need to be a BBNF surface. The transducer is a runtime/host concern, not a grammar directive. |
| `@rewrite` / rewrite-mode | `ARCHITECTURE.md:1131`, `PASS-1.md:79-80`, `restart/README.md:139-148` | **REJECTED** | "Rewrite-mode is rejected; Visitor covers transformations." Closing gate: `rg "rewrite-mode\|RewriteMode\|@rewrite"` returns zero outside the deletion table. |
| `@unicode` / class-algebra surface | `PASS-1.md:80-81`, `ARCHITECTURE.md:1132` | **DEFERRED** to regex layer | Unicode lives in regex literals via `parse-that`; BBNF does not expose `[:L:]`, `A & B`, or `\p{...}--\p{...}` at the grammar level. |
| `@pretty` (proposed) | only in this audit prompt | **NOT PRESENT** | The user's prompt names `@pretty` as accepted "if present"; corpus inspection finds no `@pretty` directive in any of `restart/{README,ARCHITECTURE,MASTER-PLAN,MIGRATION}.md`, the three pass syntheses, or the locks. No KEEP record is owed because the directive does not appear. **Verdict: NEVER-LANDED; no acceptance gate needed.** Pretty-printing is generator-side, owned by the per-grammar runtime (`css_pretty.bbnf` is a grammar, not a directive). |

### §3.3 Inventory verdict — directive surface is settled

The minimal V1 directive set is **three**: `@host fn`, `@error`, `@layout`. Every other directive cited in the corpus is either retired (verified zero positive surface), folded (standalone `@recover` into `@error(recover)`), rejected (rewrite-mode, transducer), deferred (Unicode class algebra to regex), or never-landed (`@pretty`).

The trichotomy maps cleanly onto the lowering substrate:

| Directive | Producer | Side-table | BIR consumer |
|---|---|---|---|
| `@host fn` | `grammar/directives/` + `passes::types` | `HostSignature` facts | `CallHost`, `HostChain` |
| `@error` | `grammar/directives/` + `passes::recognizers` | `RecoveryFacts` | `ErrorRecover` |
| `@layout` | `grammar/directives/` + `passes::layout` | `LayoutFacts` | `LayoutPush`, `LayoutPop` |

One writer per side effect; one consumer per BIR family; no overlap.

---

## §4 — `pointer!` → `path!` Rename Ledger

The user's directive: rename `pointer!` to `path!` corpus-wide. Below is the full citation table per file, with disposition. The `vec!` macro / `Vec` type analogy resolves the apparent collision with the `path` / `path-core` / `path-ts` crates: macros and crate names occupy disjoint namespaces in Rust, and `path!` better matches the path semantics carried by those crates.

### §4.1 `restart/README.md`

| Line | Verbatim context | Rename target |
|---|---|---|
| 35 | `bbnf` aggregator re-exports list — `pointer!, select!` | `path!, select!` |
| 50 | `path \| Rust pointer! + select! proc-macro shells` | `path \| Rust path! + select! proc-macro shells` |
| 284 | `\| pointer!(Json, ["a", "b", 0]) \| sonic-rs idiom \| ...` | `path!(Json, ["a", "b", 0])` |
| 287 | "two surfaces (`path` ships the proc-macros for both `pointer!` and `select!`)" | `... for both path! and select!` |
| 363 | "**sonic-rs** \| LazyValue<'a> idiom; pointer! macro; ..." | "... `path!` macro (sonic-rs's `pointer!` is the influence)" |

Note on §4.1 row 363: SOTA influence rows naming sonic-rs's `pointer!` should preserve the influence attribution but rename our adopted spelling. Recommended phrasing: "LazyValue<'a> idiom; pointer-macro influence (greenfield spelling: `path!`); ..."

### §4.2 `restart/ARCHITECTURE.md`

| Line | Verbatim context | Rename target |
|---|---|---|
| 61 | "`path` \| Public \| Rust macro/front-facing path DSL: `pointer!`, `select!`, visitor selectors." | `path!, select!` |
| 277 | `pointer!(Bbnf => "/rules/0/name")` | `path!(Bbnf => "/rules/0/name")` |
| 293 | `\| pointer!, select! \| path \| Rust compile-time syntax. \|` | `path!, select!` |
| 296 | "README keeps `pointer!`, `select!`, JSONPath-style selection, ..." | `path!, select!` |
| 322 | "`path` \| `pointer!`, `select!`, typed path wrappers, ..." | `path!, select!` |
| 1407 | "tape-cursor view ... that backs `pointer!`, `select!`, visitors, ..." | `path!, select!` |
| 1410 | "Path schema \| The generated path-schema sidecar consumed by `pointer!`/`select!` typing ..." | `path!`/`select!` |

### §4.3 `restart/MASTER-PLAN.md`

| Line | Verbatim context | Rename target |
|---|---|---|
| 168 | "G \| Path, Value, Visitor \| 5 \| `pointer!`, `select!`, ..." | `path!, select!` |
| 221 | "G \| `pointer!` and `select!` validate against ..." | `path!` and `select!` |
| 436 | "README path API. \| `pointer!`, `select!`, visitor mutation ..." | `path!, select!` |
| 445 | "G.W1 \| Rust `pointer!` and `select!`. \| Compile-time path diagnostics work." | `path!` and `select!` |
| 784 | "PASS-3 API docs \| ... `pointer!`, `select!`, visitor, language-server ..." | `path!, select!` |
| 799 | "`pointer!` and `select!` \| Library consumer building queries ... canonical Rust spelling uses an explicit grammar prefix such as `pointer!(Json => "/...")` and `select!(Json => "...")`. ... `BBNF-POINTER-UNKNOWN-SEGMENT` and `BBNF-POINTER-GRAMMAR-MISMATCH`." | `path!` and `select!`; canonical spellings `path!(Json => "/...")` and `select!(Json => "...")`; codes rename per §5 |

### §4.4 `restart/MIGRATION.md`

No `pointer!` macro hits in MIGRATION.md proper; the only `pointer` line at `:359` references "host shims" and is unrelated (host primitive abrogation, not the path macro). MIGRATION.md should still record the macro rename in its public-API migration table once §6.4 (other naming) lands; Tranche A.W1 (the path-crate consolidation wave per `MASTER-PLAN.md:444-445`) is the receiver.

### §4.5 `restart/audit/pass-1-substrate/PASS-1.md`

No first-class `pointer!` macro citations in PASS-1; the substrate pass does not own the macro surface. Indirect references (e.g., the diagnostic ledger does not list `BBNF-POINTER` codes in PASS-1) are clean.

### §4.6 `restart/audit/pass-2-codegen/PASS-2.md`

| Line | Verbatim context | Rename target |
|---|---|---|
| 360 | "Path-schema metadata reaches `path` and `path-core` \| `cargo test -p path-core --test grammar_schema_load` — every emitted runtime exposes the path schema descriptor consumed by `pointer!` compilation." | `path!` compilation |

### §4.7 `restart/audit/pass-3-runtime/PASS-3.md`

PASS-3 carries the densest macro-surface prose; every `pointer!` site renames to `path!`.

| Line | Verbatim context | Rename target |
|---|---|---|
| 33 | "Path/select DSL \| `pointer!` and `select!` validate against generated metadata ..." | `path!` and `select!` |
| 80 | "Tape-backed `ValueRef` is the shared cursor for `pointer!`, `select!`, ..." | `path!, select!` |
| 84 | "`pointer!` and `select!` survive, but their implementation is rebuilt." | `path!` and `select!` |
| 89 | "`path` owns Rust proc macros: `pointer!` and `select!`." | `path!` and `select!` |
| 94 | "**Pointer/select worked path.**" header | "**Path/select worked path.**" |
| 101 | `let sku_path = pointer!(Json, ["orders", 0, "sku"]);` | `let sku_path = path!(Json, ["orders", 0, "sku"]);` |
| 108 | "Compile time: `pointer!` validates the `orders -> [0] -> sku` ..." | `path!` validates ... |
| 114 | "The `pointer!` path yields one `ValueRef<_, _, JsonString>`; ..." | `path!` path yields ... |
| 118 | "`pointer!(Json, ["orders", 0, "sku_code"])` emits ..." | `path!(Json, ["orders", 0, "sku_code"])` |
| 133 | "Generated metadata ... is the only validation surface for `pointer!` and `select!`." | `path!` and `select!` |
| 140 | "DocumentView metadata feeds visitors and selectors \| ... `pointer!`, `select!` ..." | `path!, select!` |
| 295 | "`query/` (`pointer!`/`select!` adapters)" | `path!`/`select!` adapters |
| 407 | "... after a grammar's runtime emission, visitor, `pointer!`/`select!` adapters, and host route are already proven." | `path!`/`select!` |
| 409 | "Generated value API, `pointer!`, `select!`, visitor, host route, ..." | `path!, select!` |
| 481 | "`citm_catalog.json` \| ... \| object traversal and `pointer!`" | `path!` |
| 486 | "... `pointer!` and `select!` traversal timings, ..." | `path!` and `select!` |
| 494 | "`json/citm/pointer` \| ... \| `pointer!` object traversal." | `json/citm/path` (row-name rename); `path!` object traversal |
| 519 | "Runtime identity tests over direct root, `ValueRef`, `pointer!`, `select!`, visitor traversal, ..." | `path!, select!` |
| 541 | "- `pointer!`, `select!`, explicit and implicit pointer forms." | `path!, select!`, explicit and implicit path forms |
| 572 | "PASS-3 cannot validate `pointer!`/`select!` or visitors at compile time." | `path!`/`select!` |
| 446-448 | `BBNF-POINTER001/002/003` verbatim diagnostic strings: `error[BBNF-POINTER001]: unknown pointer segment ...`, `error[BBNF-POINTER002]: pointer grammar inference failed; help: add an explicit grammar prefix like \`pointer!(Json => "/...")\`.`, `error[BBNF-POINTER003]: terminal type for pointer ...` | per §5: rename codes to `BBNF-PATH001/002/003`; rewrite "unknown pointer segment" → "unknown path segment"; "pointer grammar inference failed" → "path grammar inference failed"; help line `pointer!(Json => "/...")` → `path!(Json => "/...")`; "terminal type for pointer" → "terminal type for path" |
| 119 | `BBNF-POINTER001` because `sku_code` is not a generated field. | `BBNF-PATH001` |
| 120 | `BBNF-POINTER002` until the caller supplies the explicit grammar marker. | `BBNF-PATH002` |
| 122 | `BBNF-POINTER003` and routes the user to `cargo xtask regen`. | `BBNF-PATH003` |

### §4.8 `restart/research/`

| File:line | Context | Rename target |
|---|---|---|
| `topic-6-tape.md:52` | "Settled claim 38: PASS-3 says tape-backed `ValueRef` is shared by `pointer!`, `select!`, ..." | `path!, select!` |
| `topic-6-tape.md:53` | "Settled claim 39: ... only validation surface for `pointer!` and `select!`." | `path!` and `select!` |
| `topic-6-tape.md:113` | "`pointer!`, `select!`, visitors, debug, LSP, ..." | `path!, select!` |
| `topic-6-tape.md:256` | "adopting On-Demand semantics under the word "lazy" would break `pointer!` and visitor correctness." | `path!` |
| `topic-6-tape.md:388` | "`pointer!`, `select!`, visitors, debugger, LSP, ..." | `path!, select!` |
| `topic-6-tape.md:477` | "`doc.root()`, `doc.root_value()`, `pointer!`, `select!`, visitor traversal, ..." | `path!, select!` |
| `fold-pass-3.md:146` | rg-search line including `pointer!` token | `path!` |
| `fold-pass-2.md:186` | rg-search line including `pointer!` / `path!` tokens | rg pattern updates to `path!` (current `path!` mention is "must not appear" zero-surface gate; the gate inverts: now `path!` SHOULD appear) |
| `fold-pass-1.md:211` | rg-search line including `path!` | gate inverts: `path!` should now match positive surface |

The research-fold rg gates need amendment: today they assert `path!` returns zero hits as evidence the deprecated alias is gone. After the rename, the gate inverts — `pointer!` returns zero hits as evidence the rename completed. SYNTHESIS Wave-2 owns this gate inversion.

### §4.9 `restart/audit/hardening/`

The hardening corpus contains many `pointer!` citations that survive as **deletion archaeology** rather than positive surface — e.g., HARDENING-CONSOLIDATED.md §4 rows that say "`pointer!` is the authored macro name." (`HARDENING-CONSOLIDATED.md:524`) and HARDENING-CONSOLIDATED-V6.md:342 saying "`path!` positive surface count must be zero." Both flip:

| File:line | Today's wording | Post-rename wording |
|---|---|---|
| `HARDENING-CONSOLIDATED.md:524` | "`pointer!` is the authored macro name." | "`path!` is the authored macro name." |
| `HARDENING-CONSOLIDATED.md:273-276` | "Public `pointer!` surface. Surgery: Replace public `path!` wording with `pointer!`; ..." | inverts: surgery now reads "Replace public `pointer!` wording with `path!`" |
| `HARDENING-CONSOLIDATED-V4.md:86,112` | "Public path macro name (`pointer!`) \| CLOSED" | "Public path macro name (`path!`) \| CLOSED-AMENDED" |
| `HARDENING-CONSOLIDATED-V6.md:151` | "`path!`, `@pratt`, `@simd`, rewrite-mode, and grammar Unicode algebra are not positive surfaces." | "`pointer!`, `@pratt`, `@simd`, rewrite-mode, and grammar Unicode algebra are not positive surfaces." |
| `HARDENING-CONSOLIDATED-V6.md:342` | "`path!` positive surface count must be zero." | "`pointer!` positive surface count must be zero." |
| `HARDENING-CONSOLIDATED-V6.md:382` | "`path!`, `@pratt`, `@simd`, rewrite-mode, ... survive only as deletion/prohibition/archaeology contexts." | "`pointer!`, `@pratt`, `@simd`, rewrite-mode, ... survive only as deletion/prohibition/archaeology contexts." |
| `HARDENING-PASS-1-V5.md:127, 310` | "`path!` survives only as legacy citation in PASS-3; canonical macros are `pointer!` and `select!`." | inverted: "`pointer!` survives only as legacy citation; canonical macros are `path!` and `select!`." |
| `HARDENING-PASS-3-V5.md:58` | "PASS-3 retires `path!` and preserves `pointer!` plus `select!` ..." | "PASS-3 retires `pointer!` and preserves `path!` plus `select!` ..." |

Older hardening rounds (V2-V5) record the path the corpus walked; they are **historical** in shape but their grep gates are referenced by the V6 close. The rename should propagate cleanly because the prior verdict ("`path!` is the deprecated alias") is being inverted, not erased.

### §4.10 `restart/locks/LOCKS.md`

| Line | Verbatim | Rename target |
|---|---|---|
| 46 | "The Rust `pointer!` proc-macro lives here." | "The Rust `path!` proc-macro lives here." |
| 136 | "`pointer!["a", "b", 1]` syntax (compile-time path AST)" | "`path!["a", "b", 1]` syntax (compile-time path AST)" |

Lock 7 (line 46) is the path-crate consolidation lock; the rename is editorial within the lock body, no semantic shift.

### §4.11 Crate-name collision audit

The user's prompt names the parallel: `vec!` macro vs `Vec` type in Rust. The `path!` macro vs `path` crate (`path-core`, `path-ts`) parallel holds. Rust's macro namespace and item namespace are disjoint; a macro and a module/crate may share a base identifier without collision. Concrete proof:

- `std::path::Path` (type) coexists with the `path!` macro as a perfectly legal Rust construct.
- The crate carries `pub use path_core::*;` re-exports plus the `#[proc_macro]` definitions; the macro callsite is `path!(...)` while the type/value callsites are `path::PathExpr` etc.
- The `vec!` / `Vec` precedent is canonical Rust idiom; users do not confuse them. The same holds for `path!` / `path`.

The rename is therefore safe; the surface analogy makes the macro's typed-path semantics legible. **Verdict: rename proceeds.**

---

## §5 — `BBNF-POINTER-*` → `BBNF-PATH-*` Code Rename Ledger

The diagnostic vocabulary at `ARCHITECTURE.md:1015-1058` is the consolidated catalogue. Three pointer-named codes live in V6:

| Current code | Current alias | Site | Verbatim message (today) | Renamed code | Renamed alias | Renamed verbatim message |
|---|---|---|---|---|---|---|
| `BBNF-POINTER-UNKNOWN-SEGMENT` | `BBNF-POINTER001` | `path` macro | `error[BBNF-POINTER001]: unknown pointer segment {segment} in {pointer_macro_input}; rule has no field with that name.` | `BBNF-PATH-UNKNOWN-SEGMENT` | `BBNF-PATH001` | `error[BBNF-PATH001]: unknown path segment {segment} in {path_macro_input}; rule has no field with that name.` |
| `BBNF-POINTER-GRAMMAR-MISMATCH` | `BBNF-POINTER002` | `path` macro | `error[BBNF-POINTER002]: pointer grammar inference failed; help: add an explicit grammar prefix like \`pointer!(Json => "/...")\`.` | `BBNF-PATH-GRAMMAR-MISMATCH` | `BBNF-PATH002` | `error[BBNF-PATH002]: path grammar inference failed; help: add an explicit grammar prefix like \`path!(Json => "/...")\`.` |
| `BBNF-POINTER003` | (none, numeric only) | `path` macro | `error[BBNF-POINTER003]: terminal type for pointer {path} is not yet known to the macro; help: regenerate with \`cargo xtask regen\` so the schema is in sync.` | `BBNF-PATH003` | — | `error[BBNF-PATH003]: terminal type for path {path} is not yet known to the macro; help: regenerate with \`cargo xtask regen\` so the schema is in sync.` |

### §5.1 Site enumeration

Where each code appears verbatim:

| Code | File:line |
|---|---|
| `BBNF-POINTER-UNKNOWN-SEGMENT` | `ARCHITECTURE.md:1044`, `MASTER-PLAN.md:799`, `HARDENING-MASTER-PLAN-V4.md:238`, `HARDENING-MASTER-PLAN-V2.md:140`, `HARDENING-MASTER-PLAN-V3.md:182, 348`, `HARDENING-CONSOLIDATED-V5.md` (multiple) |
| `BBNF-POINTER-GRAMMAR-MISMATCH` | `ARCHITECTURE.md:1045`, `MASTER-PLAN.md:799`, `HARDENING-MASTER-PLAN-V4.md:238`, `HARDENING-MASTER-PLAN-V3.md:182, 349`, `HARDENING-CONSOLIDATED-V5.md` (multiple) |
| `BBNF-POINTER001/002/003` | `PASS-3.md:119, 120, 122, 446, 447, 448`; `agent-2-path-select-dsl-designer.md:78`; `HARDENING-PASS-3-V3.md:237, 245`; `HARDENING-PASS-3-V4.md:198, 209`; `HARDENING-PASS-3-V5.1.md:59`; `HARDENING-MASTER-PLAN-V4.md:323`; `HARDENING-CONSOLIDATED-V5.md:162, 456` |

### §5.2 Cookbook routing rename

Per `PASS-3.md:446-448`, the cookbook receivers are:

| Today | Rename |
|---|---|
| Pointer cookbook §validation | Path cookbook §validation |
| Pointer cookbook §explicit-grammar | Path cookbook §explicit-grammar |
| Pointer cookbook §regen | Path cookbook §regen |

The cookbook itself is post-V1 doc work; the receiver names rename now so the diagnostic-text-to-cookbook chain stays consistent at code-rename time.

### §5.3 Grep-gate inversion

Existing grep gates that use `BBNF-POINTER` as evidence (e.g., `HARDENING-MASTER-PLAN-V4.md:323`, `REVIEW-D-PUNCH-LIST-EXECUTABILITY.md:370`) must update to `BBNF-PATH`. The hardening grep set is:

```
rg -n "BBNF-LIFE|BBNF-LAYOUT|BBNF-OPT|BBNF-GRAMMAR|BBNF-PATH|lookbehind|HostSignature" \
    restart/ARCHITECTURE.md restart/audit/pass-*
```

(was `BBNF-POINTER`; now `BBNF-PATH`).

---

## §6 — Other Naming Cleanups

While auditing, the following naming inconsistencies surfaced as candidates for greenfield cleanup. Each is offered with verdict; not all warrant V1 churn.

| Item | Path:line | Observation | Verdict |
|---|---|---|---|
| `RegexDfa` BIR variant spelling | `PASS-2.md:34, 65, 81`; `ARCHITECTURE.md:903, 935` | "compatibility name for the regex payload slot; it does not require full-DFA codegen for every regex" — the name implies a specific automaton when the payload is in fact a regex-program contract carrying VM/lazy-DFA/full-DFA/prefilter alternatives. | **RENAME** to `RegexProgram` (already used as alias at `ARCHITECTURE.md:903`; PASS-2 §2 explicitly notes the "compatibility name" character). The dual spelling is the naming artefact; settling on `RegexProgram` retires the implicit-automaton-choice misread. Receiver: PASS-1/PASS-2 SYNTHESIS amendment. **Cross-ref audit #4 (parse-that internals)**. |
| `pointer-syntax` cookbook anchors and bench row names | `PASS-3.md:494` (`json/citm/pointer`), various cookbook cells | bench-row name `pointer` and cookbook-anchor `pointer-...` track the macro name | **RENAME** to `json/citm/path` and `path-...` cookbook anchors at the same wave the macro renames. Tranche A (path-crate consolidation) or G (path/value/visitor) is the receiver. |
| `Pointer cookbook` doc-section titles | `PASS-3.md:446-448` cookbook receivers | "Pointer cookbook §validation" titles | **RENAME** to "Path cookbook §..." per §5.2. |
| "explicit and implicit pointer forms" | `PASS-3.md:541` | prose-level naming attached to the macro | **RENAME** to "explicit and implicit path forms". |
| `bbnf-path` legacy crate names | `PASS-3.md:84` (deletion archaeology), `MIGRATION.md` migration table | survive only as deletion-archaeology references | **KEEP-AS-ARCHAEOLOGY**; the rename is to the new crates `path`, `path-core`, `path-ts` (already settled). The macro inside `path` is what `path!` lives at after the rename. No additional crate renames begotten. |
| "JSON-pointer" / "json pointer" naming in path-DSL prose | `MASTER-PLAN.md:799` (cookbook title `cookbook/path-pointer.md`), various explainer prose | the path-DSL absorbed JSON-pointer ergonomics from sonic-rs; cookbook page named `path-pointer.md` | **RENAME** cookbook page to `cookbook/path-dsl.md`; preserve "JSON-pointer-style" as influence-attribution prose only. |
| `sib_skip` / `sibling_skip` | `PASS-3.md:174` (`sibling_skip: u32`) and `topic-6-tape.md:102, 180` (`sib_skip`) | one document uses long form, another uses short | **STANDARDISE** to `sibling_skip` per the canonical PASS-3 token shape. Cosmetic; receiver Tranche B.W2 (tape token packing). |
| `RecoveryKind` variant names | `PASS-3.md:248, 261-263` (`{Error, Missing, Substituted}`) | three-variant set tracks treesitter (`Error` + `Missing`) plus a `Substituted` for `@error(recover = ...)` substitution semantics | **KEEP**; the names are precise. |
| `__EAGER_EMPTY_PATH` magic-name | `ARCHITECTURE.md:823`, `PASS-2.md:180`, `restart/locks/LOCKS.md:38` | underscore-prefixed magic name for Lock 3 elision sentinel | **KEEP**; it is a runtime-internal const, not a public surface. |

### §6.1 Other naming verdict

The greenfield posture is conservative on rename churn beyond the `pointer!`/`BBNF-POINTER` directive. The `RegexDfa` → `RegexProgram` consolidation is the one substantive parallel rename worth folding into the same SYNTHESIS amendment that closes `path!` / `BBNF-PATH*`, because both are user-facing terminology where the today-spelling encodes a substrate decision the architecture explicitly does not commit to.

---

## §7 — Cross-cutting Concerns (with audit #2)

Audit #2 covers "function/value" — the boundary between grammar productions that build typed values and host-fn surface that performs computation. This audit observes:

| Concern | Audit #3 surface | Audit #2 receiver |
|---|---|---|
| Function-type productions in `Type` | §2 row 1: deferred; arrow types absent; `@host fn (Params) -> Type` is the typed-function declaration form | Audit #2 confirms the V1 type alphabet (`Type ::= Ident GenericArgs? \| TupleType \| RecordType \| BorrowType` per `PASS-1.md:222`, `ARCHITECTURE.md:1102`) and ratifies that function-call-as-expression lives only inside `@host fn` bodies via method-chain syntax (`a.f(x).g(y)`, `PASS-1.md:231`), not at grammar surface. |
| Closure form scope | §2 row 2: `Closure ::= "\|" Params? "\|" Expr` (`PASS-1.md:218`) is the four-form admit list per `ARCHITECTURE.md:1193-1199` | Audit #2 binds the host-chain-closure / map-closure / predicate-closure / recovery-closure quartet to its lowering targets (`HostChain`, `ValueProject`/`DirectBuild`, predicate guard BIR, `ErrorRecover`). |
| Multi-function chaining | `MapTail = "->" ChainExpr` rule-level; `a.f(x).g(y)` host-fn-body method-chain | Audit #2 ratifies type flow at chain edges and the `BBNF1401` / `BBNF-CHAIN-STEP` diagnostic + `BBNF-SUBSUMPTION-EDGE` coercion-edge surface. |
| Generic rules | `GenericParams ::= "<" Ident ("," Ident)* ">"`; first-class V1 | Audit #2 binds rank-1 monomorphisation at `(RuleId, TypeArgs)` instances and the `BBNF-GENERIC-CYCLE` rejection diagnostic. |
| Annotation surface | `e : T` ascription absent; `ReturnType ::= "->" Type` plus `Param ::= Ident ":" Type` is the typed-control surface | Audit #2 owns the Pierce-Turner check/synth distribution, hybrid annotation default per `restart/README.md:266`. |

The two audits are complementary: audit #3 binds the lexical surface (what tokens may appear), audit #2 binds the semantic surface (how types flow through them).

---

## §8 — Recommended V1 Folds (sorted by greenfield value)

| Rank | Fold | Why | Receiver |
|---|---|---|---|
| 1 | **`pointer!` → `path!` macro rename** corpus-wide; verbatim diagnostic strings, cookbook anchors, bench-row names, and README/ARCHITECTURE/MASTER-PLAN/MIGRATION/PASS-3 prose. | The user's directive: better matches path semantics; removes "pointer" connotation that misreads as raw-pointer/JSON-pointer-only; aligns with the `path` / `path-core` / `path-ts` crate set; `vec!`/`Vec` precedent confirms zero collision. | SYNTHESIS Wave-2 amendment; landing wave is Tranche A (path-crate consolidation per `MASTER-PLAN.md:444-445`) for the macro and Tranche G.W1 for the diagnostic strings. |
| 2 | **`BBNF-POINTER*` → `BBNF-PATH*` diagnostic-code rename**; both numeric and alphabetic aliases; verbatim help-text rewrite per §5; grep-gate inversion (`HARDENING-MASTER-PLAN-V4.md:323` set). | Diagnostic codes are user-facing API; they must track the macro spelling. Cookbook-receiver titles rename in lockstep. | SYNTHESIS Wave-2 amendment; same wave as fold #1. |
| 3 | **`RegexDfa` → `RegexProgram` BIR variant rename** (PASS-1/PASS-2 §2 variant table + ARCHITECTURE §7.2). | Today's `RegexDfa` is "a compatibility name for the regex payload slot" (PASS-2.md:34); the variant payload carries VM/lazy-DFA/full-DFA/prefilter alternatives (`PASS-2.md:81`), so the spelling implies a substrate decision the architecture does not commit to. The audit-prompt directive against `regex-automata` plus the bespoke `parse-that` regex owner sharpens the case: the V1 BIR should not signal a specific automaton at the variant level. | PASS-1 + PASS-2 SYNTHESIS amendment; landing wave Tranche C.W3 (BIR ratification) per `MASTER-PLAN.md:348-350` adjacency. Cross-ref audit #4. |
| 4 | **Grep-gate inversion**: every `rg -n "path!\|@pratt\|@simd"` zero-surface gate that today asserts `path!` returns zero hits inverts to assert `pointer!` returns zero hits. The hardening corpus carries ~12 such gates. | Without gate inversion the rename's CI evidence breaks. | SYNTHESIS Wave-2; landing concurrently with fold #1. |
| 5 | **Cookbook-anchor rename** (`cookbook/path-pointer.md` → `cookbook/path-dsl.md`; `Pointer cookbook §validation` → `Path cookbook §validation`; et al). | Doc-side consistency. | Tranche I (LSP/CLI/cookbook) per `MASTER-PLAN.md:445`. |
| 6 | **MASTER-PLAN forbidden-output row #204** ("`@pratt` or `@simd` grammar directives") **may stay as-is**; it is the prohibition row Lock 10 invokes. No rename touches it. | Prohibition rows are negative surface; their citation is the lock anchor. | No action. |
| 7 | **"JSON-pointer-style" influence prose** at `restart/README.md:283-287, 363` keeps the influence attribution but explicitly names the greenfield spelling as `path!`. | Influence is real; spelling is greenfield. | Same wave as fold #1. |
| 8 | **Per-grammar onboarding test** at `ARCHITECTURE.md:1331-1376` may stay grammar-prefix unchanged (`Yaml`, etc.); the macro-name change is internal to the cell at line 1407 ("`path!, select!`"). | Onboarding test exercises the cell, not the cell name. | No additional action. |

---

## §9 — Voice and Discipline Lock

Calibrated, direct. Every claim cites path:line. Where corpus prose carries the deprecated wording, the rename target is given verbatim, not paraphrased. The greenfield posture binds: no quick solutions, no workarounds, no legacy code uncontested. The directive set is **three** (`@host fn`, `@error`, `@layout`); the macro name is **`path!`**; the diagnostic codes are **`BBNF-PATH-*`**. Every other directive cited in the corpus is verified zero-surface, retired, folded, rejected, or never-landed. No hidden V1 expansion lives behind this audit; the deferral list above is honest.

---

## §10 — Closing posture

The BBNF V1 surface is settled: a small EBNF (PASS-1.md §6), three directives, one macro family (`path!` + `select!`), one diagnostic-code family per family-of-failure. The `pointer!` → `path!` rename is the single load-bearing edit this audit begets, and it threads through:

- `restart/README.md` (5 sites)
- `restart/ARCHITECTURE.md` (7 sites + diagnostic catalogue)
- `restart/MASTER-PLAN.md` (6 sites)
- `restart/audit/pass-2-codegen/PASS-2.md` (1 site)
- `restart/audit/pass-3-runtime/PASS-3.md` (~22 sites including verbatim diagnostic strings)
- `restart/locks/LOCKS.md` (2 sites)
- `restart/research/` (5 sites)
- `restart/audit/hardening/` (~10 inversion sites carrying gate evidence)

Total surface: ~58 verbatim citations across the open corpus; plus the diagnostic-code rename across the same set. SYNTHESIS Wave-2 owns the amendment; Tranche A and G own the implementation waves. The `@directives` audit closes with three survivors and zero open gates.

The `@transducer` rejection survives. Every transducer the user has written or cited (ICU rules, ffuzzy phonetic walker, trigraph mapping, vendor-prefix stripping) decomposes through lookbehind `\|<` plus `@host fn` chain plus generics over the existing `Object<V>`-class surface. The runtime walker is the ~30-LOC consumer of the dispatch tables BBNF already produces (`docs/ffuzzy.md:611-614`); no grammar directive carries it.

Lean. Small. Robust.
