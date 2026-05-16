# Deferral audit #5 — Runtime / PASS-3 / public API

## §1 — Scope and corpus

This audit walks the runtime surface — incremental parsing, fault tolerance,
debug runtime, LSP, TS bindings, WASM ABI, public macros, visitor / pointer /
select / value APIs — and asks for every item: is it V1 ready, V1 partial,
properly carried, or carry-ledger-orphaned? The greenfield mandate is the
prevailing pressure: items that the cohort has parked through inertia rather
than principled receiver routing should be dragged forward.

Corpus consulted, in priority order:

- `restart/audit/pass-3-runtime/PASS-3.md` (585 lines).
- `restart/research/topic-7-green-red-incremental.md`.
- `restart/research/fold-pass-3.md`.
- `restart/ARCHITECTURE.md` §3 (public APIs), §4 (private internals), §8
  (formal grammar + closures), §12 (yaml proof).
- `restart/MASTER-PLAN.md` §11–§16 (G/H/I/J), §24 (carry ledger + cookbook
  friction).
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` §5 (residue ledger),
  §8 (topic ownership matrix), §9 (verification checklist).
- `restart/locks/LOCKS.md` Locks 7 (path split), 11 (incubating sister
  crates), 12 (ser/gorgeous archive).

Verdict legend used in §2-§5 tables:

| Disposition | Meaning |
|---|---|
| **V1-READY** | Item is committed in PASS-3 / Architecture / Master with receiver, blocker, gate, and acceptance. |
| **V1-PARTIAL** | Item has a contract sketch but receiver is loose, gate is report-only, or acceptance is verbal. |
| **CARRY-OK** | Item is post-V1, receiver and blocker named, gate routed (e.g. `{N}`/`{M}`). |
| **CARRY-LOOSE** | Item is deferred but receiver vague or acceptance gate absent. |
| **FOLD** | Greenfield candidate to drag into V1 / V1.1; user-facing benefit dominates. |
| **DEFER-OK** | Principled deferral; pulling forward forks architecture. |

## §2 — Incremental parsing and fault tolerance

| Item | Path:line | Status | Disposition | Notes |
|---|---|---|---|---|
| `DocumentSnapshot { id, text, tape, diagnostics, semantic }` shape | `restart/audit/pass-3-runtime/PASS-3.md:194-201` | Sketch only; field types like `Rope` and `SemanticIndex` are placeholder types | V1-PARTIAL | Field types belong on I.W1 implementation, but the public surface (`bbnf-language-server` exports it per `restart/ARCHITECTURE.md:263`) needs the lifetime story committed before generated runtime crates compile against it. |
| `ReparsePlan { Reuse, Reparse }` with `reuse_map`, `fallback_reason`, `invalidated_queries` | `restart/audit/pass-3-runtime/PASS-3.md:203-216` | Full sketch with R5 fold landed | V1-READY | The variant carrying `invalidated_queries: QueryInvalidationSet` is a contract; producer side is `RecoveryFacts` from PASS-1, consumer side is I.W1 from `restart/MASTER-PLAN.md:523`. |
| Snapshot-scoped `TapeId` + cross-snapshot reuse map | `restart/audit/pass-3-runtime/PASS-3.md:184` | Settled by R4 fold | V1-READY | Lock 1 spirit preserved; reuse map is the only cross-snapshot identity bridge. |
| `RecoveryKind::{Error, Missing, Substituted}` typed recovery nodes | `restart/audit/pass-3-runtime/PASS-3.md:227-248` | Committed | V1-READY | Diagnostic, sync token, typed placeholder policy, and `VisitTypes::ERROR` behavior are bound; producer is I.W0 (`restart/MASTER-PLAN.md:522`). |
| Dataset-level fallback ceilings (JSON / CSS / BBNF / large-paste) | `restart/audit/pass-3-runtime/PASS-3.md:268-275` | Thresholds committed | V1-READY | `incremental/edit_anchor` bench is the receiver; harness is H.W4 / I.W1. |
| LSP user-facing silence policy | `restart/audit/pass-3-runtime/PASS-3.md:277` | Committed | V1-READY | Default LSP silent; `BBNF_LSP_DEBUG=1` debug channel; never `showMessage`. Test exists at `restart/MASTER-PLAN.md:758`. |
| `BBNF-RECOVERY*` diagnostic family | `restart/audit/pass-3-runtime/PASS-3.md:452` | Single committed code (`BBNF-RECOVERY001`) | V1-PARTIAL | Family is referenced in `restart/ARCHITECTURE.md:1053` as "`BBNF-RECOVERY*` Error pass" but only `001` exists in the verbatim ledger. Need `002`/`003` for missing-token vs substituted-token, or fold into one general code with parameters. **Recommend FOLD**: commit at least three concrete recovery codes before I.W0 closes, otherwise §6b becomes a pretend ledger. |
| Anchor algorithm (how `anchors: Vec<TapeId>` are picked) | `restart/audit/pass-3-runtime/PASS-3.md:209-216` | Sketch only | V1-PARTIAL | The data structure is committed; the algorithm (sync-token windows, scope-balance proofs, etc.) is not. Implementation gate at I.W1 (`restart/MASTER-PLAN.md:523`) but the spec needs receiver-blocker-gate. **Recommend** routing to I.W1 with explicit acceptance criterion: "anchor algorithm proves balanced-scope reuse on JSON edit corpus or names `anchor_miss_unbalanced_scope` fallback". |
| Streaming parsing (chunked input, incremental over network/IO) | absent | Not in V1 | DEFER-OK | No PASS-3 row claims it; `parse(&str)`, `parse_in`, `parse_owned` are slice-only; streaming forks the lifetime story (Lock 9). Carry to post-V1 if user-mandated. |
| DAP / breakpoints / source maps | `restart/audit/pass-3-runtime/PASS-3.md:186` | Identity contract committed; protocol/server is consolidated into `bbnf-language-server` per `restart/MASTER-PLAN.md:525` | V1-READY | I.W3 owns "Debug/replay and playground hooks. VM trace displayed through server/debug API." Acceptance: `cargo test -p vm debug_replay`. |
| Compiled trace / VM `DebugMark` | `restart/ARCHITECTURE.md:917,949` | BIR variant committed | V1-READY | `vm` crate at `restart/ARCHITECTURE.md:56` carries trace/replay; gate is I.W3. |
| Interpreter step (single-step debugger surface) | `restart/audit/pass-3-runtime/PASS-3.md:186` (mentions stepping) | Identity carries; step semantics are protocol-level | V1-PARTIAL | Stepping is mentioned but the public protocol contract is folded into `bbnf-language-server`. Receiver = I.W3, blocker = DAP server impl, gate = `cargo test -p vm debug_replay` plus a DAP fixture. **Recommend** add explicit acceptance: "single-step over a yaml fixture lands on tape-identified nodes with `(SnapshotId, TapeId, span)`". |
| Trace overhead measurement | `restart/audit/pass-3-runtime/PASS-3.md:500` | Report-only row | DEFER-OK | `debug/trace_overhead` is non-Lock-8; no SOTA peer claim. Report at H.W4 / J.W1 is sufficient. |
| `RecoveryFacts` side-table | `restart/ARCHITECTURE.md:1011` | Producer = error pass; consumer = `ErrorRecover`, LSP diagnostics | V1-READY | Public side-table per Lock 2 boundary. |
| `BBNF-VISIT*` family for recovery node visitors | `restart/audit/pass-3-runtime/PASS-3.md:449-451` | Three codes committed | V1-READY | `BBNF-VISIT003` warns when a visitor opts out of `VisitTypes::ERROR`. |

**Fold carry**: `BBNF-RECOVERY*` family expansion into concrete codes; anchor
algorithm acceptance criterion at I.W1; interpreter step DAP fixture.

## §3 — Public API deferrals

| Item | Path:line | Status | Disposition | Notes |
|---|---|---|---|---|
| `pointer!` macro (Rust proc-macro) | `restart/audit/pass-3-runtime/PASS-3.md:88-92` | Crate split owned by Lock 7; G.W1 close gate at `restart/MASTER-PLAN.md:445` | V1-READY | Validates against generated metadata; explicit `pointer!(Json => "...")` and implicit forms supported. |
| `select!` macro | same row | V1-READY | Same posture as `pointer!`. |
| **Rename `pointer!` to `path!`** (user mandate) | `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:151,342,382` explicitly forbid `path!` | Conflict | **CARRY-LOOSE → ESCALATE** | The V6 verification checklist row #2 demands "`path!` positive surface count must be zero". The user mandate to rename `pointer!` to `path!` overturns that lock-adjacent invariant. This is an architectural decision, not a fold; either V6 row #2 is amended to permit `path!` (and `pointer!` becomes deletion archaeology) or the rename is rejected. **Recommend**: synthesis-level escalation; if folded, rename across PASS-3 §3, ARCHITECTURE §3.4, MASTER-PLAN §24 cookbook + friction rows; deletion archaeology for `pointer!` retained. The semantic argument for `path!` is real — `pointer!` connotes a JSON-Pointer-only model, while `path!` correctly generalizes over both pointer (single-result) and select (multi-result) traversals. |
| `select!` rename to `query!` or absorption into `path!` | not currently surfaced | Not in V6 | DEFER-OK pending the rename decision above | If `path!` becomes the canonical macro, `select!` survives as the multi-result variant; alternatively `path!` could be a single keyword carrying both shapes by syntax. Cookbook impact is non-trivial. |
| `ValueRef<'doc, 'input, K>` cursor | `restart/audit/pass-3-runtime/PASS-3.md:177-182` | Public surface committed | V1-READY | Backs `pointer!`, `select!`, visitors, debugger, CLI projections, LSP, playground. |
| `ValueOwned` typed-owned root | `restart/MASTER-PLAN.md:446` (G.W2) | Implementation committed; type sketch absent in PASS-3 | V1-PARTIAL | PASS-3 §2 commits `parse_owned` returning `Self::OwnedRoot`; `ValueOwned` is mentioned by name only at G.W2. **Recommend** add a row to PASS-3 §2 or Architecture §3.5 binding `ValueOwned` shape. |
| Typed-root API (`Json::parse → JsonRoot`) | `restart/audit/pass-3-runtime/PASS-3.md:43-71` | Trait sketch with `Grammar` + `DocumentView` | V1-READY | Three constructors + view trait; lifetime parameters documented. |
| `DocumentView<'input>` | `restart/audit/pass-3-runtime/PASS-3.md:73-77` | Sketch | V1-READY | Members: `source`, `root_value`, `diagnostics`. Acceptance gate at PASS-3 §3 consumer table. |
| `VisitTypes` bitflag pruning | `restart/audit/pass-3-runtime/PASS-3.md:145` | Committed | V1-READY | W5 visitor-bitflag spec inherited; cookbook receivers indexed in §6b. |
| Visitor cookbook (collection / pruning / mutation / warnings / recovery) | `restart/MASTER-PLAN.md:801` | One friction row + cookbook page name only | V1-PARTIAL | Cookbook page `cookbook/visitor-mutation.md` is named, content is deferred to "next drafting phase" per `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:179` R7. **Recommend FOLD** at least the four codes' worked examples (`BBNF-VISIT001`/`002`/`003` + `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY`) into V1.1; without them G.W3 mutation gate is documentation-blind. |
| Pointer/select diagnostics — committed strings | `restart/audit/pass-3-runtime/PASS-3.md:447-449` | `BBNF-POINTER001`/`002`/`003` verbatim | V1-READY | All three carry mental model, confusion point, cookbook receiver. |
| Friction-row ledger | `restart/MASTER-PLAN.md:799-806` | Eight rows committed | V1-READY | Each binds target user, mental model, confusion, artefact, diagnostic. |
| Generic over-the-grammar APIs (`GrammarHandle<G>`, `Grammar` trait) | `restart/ARCHITECTURE.md:307` lists `Grammar`, `GrammarHandle` | Public exports committed | V1-PARTIAL | The trait is named but its method set is sketch. PASS-3 §2 gives the constructor surface; per-grammar generated code consuming `impl Grammar for Json` is implicit. **Recommend** binding the full trait method list (associated types `Root<'arena, 'input>`, `OwnedRoot`, `RootKind`, `VisitorRoot`) before F.W3 emits per-grammar runtime crates, otherwise generated code lacks a target. |
| `Grammar::RootKind` for `pointer!` typing | absent | Not committed | V1-PARTIAL | The `pointer!` worked path uses `JsonRoot` (`restart/audit/pass-3-runtime/PASS-3.md:99`); the type-level discriminator that links `Json` (the marker) to `JsonRoot` (the typed root) is not committed. **Recommend FOLD**: commit `Grammar::RootKind` as an associated type. |
| `prelude.rs` re-exports | `restart/audit/pass-3-runtime/PASS-3.md:295` | Mentioned as 8-child top-level | V1-PARTIAL | Concrete re-export list is absent. **Recommend** binding the prelude surface in Architecture §3.5: `parse`, `parse_in`, `parse_owned`, `Grammar`, `DocumentView`, `ValueRef`, `ValueOwned`, `Visitor`, `VisitTypes`, `Diagnostic`, `pointer!`/`select!` (or `path!`). |
| WASM exposed surface (Rust → wasm32 binding) | `restart/MASTER-PLAN.md:479,556` (J.W3) | Stable surface includes `bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `path-ts` | V1-READY | The wasm32 binding is the same Rust public API; no separate hand-crafted ABI surface. |

**Fold carry**: `Grammar::RootKind` associated type; full `Grammar` trait spec;
`prelude.rs` concrete export list; visitor cookbook V1.1 worked examples; the
`pointer!`-vs-`path!` rename decision (escalation, not fold).

## §4 — TS bindings (`path-ts`)

| Item | Path:line | Status | Disposition | Notes |
|---|---|---|---|---|
| `path-ts` schema generation from `path-core` | `restart/MASTER-PLAN.md:448` (G.W4) | "yaml enters through grammar source plus metadata; generated runtime is derivative" | V1-READY | The TS template tag and schema bindings live in `path-ts/src/{template_tag.rs, schema.rs, bindings.rs}` per `restart/audit/pass-3-runtime/PASS-3.md:381-385`. |
| TS publication | `restart/MASTER-PLAN.md:790` | "J.W3 dry-run records `path-ts` only after J.W0 parity matrix passes" | V1-READY | Lock 11 incubation policy applies; failure slips one J cycle. |
| Full TS path runtime (TypeScript-side parse + traversal) | absent from PASS-3 | Not committed | DEFER-OK | The architecture is "TS schema dump + TS API definitions" only (`restart/ARCHITECTURE.md:324`); the TS runtime that *executes* paths against TS-side parsed documents would require either (a) a TS port of `parse-that` + tape, or (b) WASM-loaded Rust runtime. The user prompt asks: is this deferral architectural or pragmatic? Answer: pragmatic — Lock 11 puts `path-ts` in the stable cohort (`restart/MASTER-PLAN.md:556`) but only as schema + API definitions. A full TS runtime is architecturally a different lifetime story (TS has GC; Rust borrow surface does not translate); the current `path-ts` posture is a schema-typed binding over WASM-loaded Rust runtime, not a TS-native one. |
| TS-native parse (no WASM) | absent | Not committed | DEFER-OK | Forks the lifetime + tape stories; not a V1 fold candidate. |
| `path-ts` semantics drift from `path-core` | `restart/MASTER-PLAN.md:787` | Carry: "`path-ts` schema does not derive from the same `path-core` semantics" | V1-READY | Gate: "`path-ts` and `path` consume identical `path-core` AST; schema dump round-trips". |

**TS bindings disposition**: V1-READY *as schema-binding crate*. Pulling
TS-native runtime into V1 is architectural fork, not fold. The user's
greenfield instinct is correct that TS *publication* should not be deferred —
and Lock 11 doesn't defer it; `path-ts` is in the unconditional-publish cohort
at J.W3. The narrative that "TS is deferred" is loose corpus reading; the
authoritative statement is that `path-ts` ships as schema + API in V1, and TS
runtime parity with Rust is post-V1 architectural work.

**Fold candidate (V1.1, not V1)**: a thin TS validator that checks parsed
JSON against grammar metadata at TS-runtime, separate from the proc-macro
schema. This is a small surface, has clear user value (browser-side path
checks without WASM bring-up), and would not fork tape semantics. Carry to a
post-J cycle.

## §5 — WASM ABI residues

| Item | Path:line | Status | Disposition | Notes |
|---|---|---|---|---|
| H.W3 measurement placeholders `{N}` and `{M}` | `restart/MASTER-PLAN.md:479` | Owner = H.W3 lead, blocker = lightning-css/WASM build available | CARRY-OK | Per V6 §5 R6 these are measured placeholders, not invented numbers. Acceptance gate at H.W3 records measured values, host/browser/runtime, fixture hash, ABI matrix, scalar/SIMD parity, competitor baseline. |
| WASM host primitive ABI matrix | `restart/MASTER-PLAN.md:483-491` | Five-row receiving matrix committed | V1-READY | Exported function names, host-call shape, marshalling rule, primitive coverage, scalar/SIMD parity. Owner = H.W3. |
| `BBNF-HOST003` ("host chain cannot lower to WASM") | `restart/audit/pass-3-runtime/PASS-3.md:457` | Verbatim string committed | V1-READY | Routes missing primitives to H.W3 ABI descriptor expansion. |
| WASM publication readiness (J.W3) | `restart/MASTER-PLAN.md:556` | Two-gate Lock 11 split (stable / incubation-cleared) | V1-READY | wasm32 build of `bbnf` + `path` + `path-ts` ships unconditionally; sister crates need stability gate. |
| `wasm-simd128` parity gate | `restart/MASTER-PLAN.md:491` | "wasm-simd128 and scalar outputs produce identical tape/value hashes before any H.W3 latency or size number is accepted" | V1-READY | Hard gate; no measurement accepted without parity. |
| Browser host runtime selection (Safari/Chromium/Firefox) | `restart/MASTER-PLAN.md:479` | "M1 Pro Safari WASM runtime within `{N}`ms" | CARRY-LOOSE | Single-browser receiver is narrow; lightning-css comparison is host-dependent. **Recommend** broadening H.W3 acceptance to record at least Chromium parity numbers, otherwise the SOTA claim is single-host. Not a V1 fold; H.W3 measurement scope. |

**Fold carry**: H.W3 cross-browser measurement scope; otherwise the WASM ABI
contract is V1-READY in shape, V1-CARRY in measurement.

## §6 — Pretty / formatting story

The `@pretty` directive is **not a positive surface in V1**. The corpus
search is unambiguous:

- `@pretty` does not appear as a directive anywhere in PASS-1, PASS-2, PASS-3,
  ARCHITECTURE, MASTER-PLAN, or 14-LOCKS.
- The accepted directive set is fixed at six: lookbehind, `@host fn`,
  multi-function chaining, generics, `@error(recover = ...)`, `@layout`
  (`restart/audit/pass-3-runtime/PASS-3.md:16`,
  `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:151`).
- `gorgeous` (the historic pretty-printer crate) is **archive-only** before
  A.W0 per Lock 12 (`restart/locks/LOCKS.md:56`); MASTER-PLAN A.W0
  enforces archive (`restart/MASTER-PLAN.md:244`).
- `css_pretty` is a grammar — not a formatting engine. It generates
  `CssPrettyRoot` over `&'i Tape<'i>` like every other grammar
  (`restart/ARCHITECTURE.md:1395`). Its host route is "metadata + format host
  fns from `host::primitives`" — the formatter is grammar-driven, not a
  separate engine.
- `@layout` is the closest thing V1 has to a pretty-printer surface
  (`restart/ARCHITECTURE.md:1159`): "Layout and error directives are typed
  side effects. They produce `LayoutFacts` and `RecoveryFacts`, not ad hoc
  codegen flags." `BBNF-LAYOUT001` warns when `@layout` is unused by the
  generated formatter (`restart/audit/pass-3-runtime/PASS-3.md:441`).

| Item | Path:line | Status | Disposition |
|---|---|---|---|
| `@pretty` as a directive | absent | Not committed | DEFER-OK / silent in corpus |
| `gorgeous`-like pretty-print engine in V1 runtime | `restart/locks/LOCKS.md:56` | Archived before A.W0 | DEFER-OK |
| `@layout` as the formatting surface | `restart/ARCHITECTURE.md:1159` | Committed | V1-READY |
| Generated formatter via `@layout` + grammar | `restart/audit/pass-3-runtime/PASS-3.md:441` | `BBNF-LAYOUT001` warns when unused | V1-READY |
| `bbnf/self_host/internal` parse + format roundtrip | `restart/audit/pass-3-runtime/PASS-3.md:498` | <= 100ms internal gate | V1-READY |
| Pretty-printing as a public runtime API | absent | Not committed | DEFER-LOOSE |

The user invoked `@pretty` favorably. **Recommendation**: if the user wants
`@pretty` in V1 it must be added as a seventh directive in PASS-1 §6 and the
formal grammar at `restart/ARCHITECTURE.md:1103-1108`, and the codebase posture
that formatting is `@layout` + grammar must be reconsidered. This is
**escalation**, not fold — adding a directive opens the formal grammar surface
which is currently locked against new positive surfaces (V6 §9 row #1).

A weaker fold candidate: surface a public `format()` / `pretty()` method on
generated typed roots that walks the grammar with `@layout` evidence and emits
pretty source. This is V1-PARTIAL today (the engine exists generically through
`@layout`; the public method does not) and could fold without grammar change.
Receiver = G.W2 (`ValueRef`, `ValueOwned`, shape-backed projection).

**Bench evidence the user's intuition is right**: `bbnf/self_host/internal`
already commits to a parse+format roundtrip gate at <= 100ms. There is a
formatter; it just lacks a clean public surface.

## §7 — Cross-cutting (function-value runtime impact)

Audits #1 + #2 are surfacing function-value typing. The runtime obligations
this introduces, traced through the corpus:

**Closure presence in the formal grammar**:
`restart/ARCHITECTURE.md:1103` —
`Primary ::= Literal | Regex | Ref | Group | HostCall | Closure`
and `restart/ARCHITECTURE.md:1108` —
`Closure ::= "|" ParamList? "|" Expr`.

Closures exist as a positive grammar surface. Their semantics at
`restart/ARCHITECTURE.md:1187-1198` are intentionally narrow — four shapes:
host-chain closure, map closure, predicate closure, recovery closure. Each
lowers to a fixed BIR variant (`HostChain`, `ValueProject`, predicate-bool,
`ErrorRecover`); none demands a runtime function-pointer table.

**Function-value runtime obligations the audits may surface**:

| Obligation | Currently committed? | V1 disposition |
|---|---|---|
| First-class function values (storage, return, parameter passing of `\|x\| ...` outside the four committed sites) | No — only the four sites at `restart/ARCHITECTURE.md:1193-1198` | DEFER-OK; first-class function values would require a runtime closure environment (capture record, lifetime story across snapshots). Not a fold; opens a new lifetime contract. |
| Function-pointer dispatch table (host registry) | Yes — `host` crate at `restart/ARCHITECTURE.md:59`, `host/src/registry/` at `restart/ARCHITECTURE.md:483-489`, "Host registry, primitive signatures, chain typing, dispatch handles" | V1-READY but private internals (`restart/ARCHITECTURE.md:592`: "Raw function-pointer table, metadata normalization maps. \| Registry and typed dispatch handles are enough."). |
| Closure environment frames (captures live across parse) | No — closures only capture "previous host result and explicit args" (`restart/ARCHITECTURE.md:1195`) or "matched value, named captures, explicit annotations" (`restart/ARCHITECTURE.md:1196`); captures are lexical, not heap-escaping | V1-READY; lifetime story stays slice-borrow primary (Lock 9). |
| `dyn` / vtable layout for visitor traits | `runtime/visitor/` carries `Visitor` traits per `restart/ARCHITECTURE.md:479`; G.W3 mutation goes through visitor | V1-READY; trait dispatch is the existing Rust-language solution. |
| HRT (higher-rank types) for visitors crossing snapshots | DK-style higher-rank is explicitly post-V1 per `restart/ARCHITECTURE.md:1161-1166` | DEFER-OK. |
| Callable host primitive vs `@host fn` body uniformity | Yes — `restart/ARCHITECTURE.md:1156`: "Block-bodied `@host fn` definitions and generic primitives share the same checker" | V1-READY. |

**Receiver-blocker-gate routing for any audit-#1/#2 function-value pressure
that lands**:

- If function-values stay narrow (the four closure sites): V1-READY; runtime
  needs no new public surface beyond `host` registry, visitor traits, and
  generated `ErrorRecover`/`ValueProject` BIR variants.
- If function-values broaden (first-class storage, return-from-rule,
  parameter-pass): runtime *must* commit a closure-environment frame and a
  cross-snapshot identity story for captures over edits. This is a Lock 1
  amendment because reuse-map semantics must extend to closure environments.
  **Receiver** = SYNTHESIS (Lock 1 amendment) → PASS-1 (closure capture
  facts) → PASS-3 (public closure surface, runtime identity). **Blocker** =
  closure capture facts cannot live across `ReparsePlan::Reparse` without an
  identity bridge. **Gate** = closure environment frame in `runtime/builder/`
  with snapshot-scoped reuse map; visitor mutation tests over closure-bearing
  documents.

The audit's posture: **runtime is conservative**. The current PASS-3 surface
absorbs narrow closures (the four committed shapes) without architectural
change. Broadening to first-class function values is escalation, not fold.

## §8 — Recommended V1 folds (sorted by greenfield value)

Each fold names target file:line, the surgical edit, and the receiving gate.
Ordered descending by user-visible value.

| # | Fold | Target | Surgery | Gate |
|---|---|---|---|---|
| F1 | **`Grammar::RootKind` associated type** committed in PASS-3 §2 trait sketch. | `restart/audit/pass-3-runtime/PASS-3.md:61-71` | Add `type RootKind;` to `Grammar` trait; bind `pointer!`/`select!` typing to it; reflect in Architecture §3.5 public exports. | F.W3 generated runtime crate compiles when `<Json as Grammar>::RootKind = JsonRoot`; G.W1 macro test asserts the link. |
| F2 | **`prelude.rs` concrete export list**. | `restart/ARCHITECTURE.md:307` (currently lists abstract families) | Bind the prelude to: `parse`, `parse_in`, `parse_owned`, `Grammar`, `DocumentView`, `ValueRef`, `ValueOwned`, `Visitor`, `VisitTypes`, `Diagnostic`, `pointer!`/`select!` (or `path!` after rename decision). | A.W4 lock-no-hardcoded-grammars gate; B-W consumer smoke. |
| F3 | **`BBNF-RECOVERY*` family expansion** (at least `BBNF-RECOVERY002` and `BBNF-RECOVERY003`). | `restart/audit/pass-3-runtime/PASS-3.md:452` | Commit one code per `RecoveryKind` variant: 001 `Substituted`, 002 `Missing`, 003 `Error`. Verbatim strings + cookbook receivers. | I.W0 close: "Error directive fixtures produce stable diagnostics" — fixtures must hit each code. |
| F4 | **Public `format()` / `pretty()` method on typed roots** as the `@layout`-driven formatter surface. | `restart/audit/pass-3-runtime/PASS-3.md:73-77` `DocumentView` trait | Add `fn format(&self) -> String` (or `fn pretty(&self, options: PrettyOptions) -> String`) consuming `LayoutFacts`. The engine exists; the public method does not. | G.W2 `ValueRef`/`ValueOwned` close: round-trip parse+format on `bbnf/self_host/internal` <= 100ms. |
| F5 | **Visitor cookbook V1.1 worked examples**, four codes. | `restart/MASTER-PLAN.md:801` cookbook page name only | Draft `cookbook/visitor-mutation.md` with worked examples for `BBNF-VISIT001`/`002`/`003` + `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY`. | G.W3 close: mutation API tests reference cookbook examples by anchor. |
| F6 | **Anchor algorithm acceptance criterion at I.W1** (snapshot reuse map). | `restart/MASTER-PLAN.md:523` (close gate exists; acceptance is loose) | Add: "anchor algorithm proves balanced-scope reuse on JSON edit corpus or names `anchor_miss_unbalanced_scope` fallback; algorithm is documented as `incremental/anchors.rs` per `restart/audit/pass-3-runtime/PASS-3.md:329`". | I.W1 close adds an anchor-algorithm contract test. |
| F7 | **DAP single-step fixture** at I.W3. | `restart/MASTER-PLAN.md:525` | Add: "single-step over a yaml fixture lands on tape-identified nodes with `(SnapshotId, TapeId, span)`". | I.W3 close runs against yaml fixture. |

Escalation items (not folds; require lock or formal-grammar amendment):

| # | Escalation | Decision blocker |
|---|---|---|
| E1 | **`pointer!` → `path!` rename**. | V6 §9 row #2 explicitly forbids `path!` as positive surface. Either V6 amends row #2 (and `pointer!` becomes deletion archaeology) or the rename is rejected. User mandate present in audit prompt. |
| E2 | **`@pretty` as a directive**. | Adding to PASS-1 §6 + Architecture §8.1 `Closure ::=` line opens the formal grammar surface. V6 §9 row #1 holds the surface closed. The `@layout` + generated-formatter + public `format()` method (fold F4) gives the user-visible benefit without amending the grammar; recommend F4 over E2. |
| E3 | **First-class function values** beyond the four closure sites. | Lock 1 amendment for cross-snapshot closure capture identity; PASS-1 closure capture facts; PASS-3 public closure surface. Forks runtime contract; not greenfield-cheap. |
| E4 | **Cross-browser H.W3 measurement** beyond Safari. | H.W3 measurement scope; not a V1 fold but a hardening of carry row R6. |

## §9 — Final posture

Runtime is the user-facing surface. PASS-3 has carried the V6 fold cleanly:
snapshot-scoped tape identity, reuse-mapped reparse, typed recovery, dataset
fallback gates, LSP silence policy, debug/DAP identity, and committed
diagnostic strings. The seven folds (F1–F7) close gaps that are user-visible
within V1's existing locks; the four escalations (E1–E4) are decisions the
user must make at the architecture / lock layer, not the audit layer.

The TS bindings narrative — that they are "deferred" — is loose corpus
reading. `path-ts` ships in the V1 stable cohort at J.W3. TS-native parse +
runtime is post-V1 architectural fork, not pragmatic deferral.

The pretty / formatting narrative requires care. `@pretty` is not a directive;
`gorgeous` is archived; the formatter is `@layout`-driven and grammar-emitted.
The user-visible gap is a public `format()` method on typed roots — fold F4.
Adding `@pretty` as a directive (E2) is grammar-surface escalation and is
dominated by F4 in greenfield value.

Function-value impact on runtime is conservative if the four committed
closure sites hold. If audits #1 / #2 surface broader function-values, the
runtime contract amends through Lock 1 reuse-map semantics, not through
PASS-3 public surface. PASS-3 is positioned to receive that pressure without
rewriting the user surface.
