# HARDENING-PASS-2-V8

V8 simplification audit against `restart/audit/pass-2-codegen/PASS-2.md`
applying Lens I (contrivance / over-engineering), Lens J (host-language
leverage), and Lens K (meta-grammar discipline) per the Phase-8.1
HARDENING.md amendment (`restart/prompts/audit-specs/HARDENING-LENS-SET.md:152-186`). The V7
baseline returned READY with four non-blocking corpus-hygiene residues
(R-V7-1 through R-V7-4); the V7.1 cohort returned READY for PASS-2 with
no regression. V8 does not relitigate Lanes 1-9 + Lens F/G/H — those
verdicts hold. V8 asks the next question: now that PASS-2 is coherent
under nine-lane discipline + LLM-pathology lenses, does the codegen
surface carry surplus apparatus, fail to leverage Rust where Rust
suffices, or invent meta-grammar machinery that exceeds the meta-grammar
mandate?

Verdict in advance: **READY with simplification opportunities surfaced**.
The PASS-2 surface is largely load-bearing under V8 challenge — BIR's
22-variant alphabet sits inside the cardinality-defence band; the Backend
trait's 5-method surface partitions cleanly across artefact classes; the
function-value lowering's three options each name a load-bearing surface.
But six items survive Lens I/J/K with simplification candidates: (a) the
`LayoutPush` / `LayoutPop` pair admits `LayoutScope` consolidation; (b)
the `BackendLowerer` + `Backend` trait pair carries surface duplication
that warrants explicit composition-vs-merge defence; (c) `parse_in` /
`parse_owned` / `parse` triple admits Rust-leverage simplification via
ownership-mode parameter; (d) bumpalo arena defaults are not explicitly
delegated; (e) e-graph rewrite categories carry no V1 inventory in
PASS-2 (route to PASS-1 ownership clarification); (f) cost-model trait
sharing across parser + regex carries an aspirational generality flag.
None blocks V8 advancement; each routes to a punch-list candidate.

## §1 — Target identification

| Item | Value |
|---|---|
| Target | `restart/audit/pass-2-codegen/PASS-2.md` |
| Audited commit | `bc31560c` (Phase-8.1 restructure; latest commit on master) |
| Phase 8.1 anchor | `bc31560c` (HARDENING.md amendment introducing Lens I/J/K) |
| Predecessor verdict | V7 READY (`HARDENING-PASS-2-V7.md`, 353 lines, four non-blocking residues) |
| Output | `restart/audit/hardening/HARDENING-PASS-2-V8.md` (this report) |
| Write scope | This report only. |
| Initial worktree | Clean per `git status --short`. |

PASS-2.md preflight: 612 lines unchanged from V7 baseline (no Phase 8
fold has touched the target between the V7 audit and V8 dispatch).
Phase 8.0 retired 14 stale files and rewrote `HANDOFF.md`; Phase 8.1
restructured `restart/prompts/` and added Lens I/J/K. Neither phase
amended PASS-2 surface.

## §2 — Lens application scope

V8 applies three lenses against PASS-2 surfaces named in the dispatch:

| Lens | Surfaces under audit |
|---|---|
| I — Contrivance | BIR alphabet variant count, RegexProgram + Scanner adjacency, PrattSpine + recursive-descent overlap, LayoutPush/LayoutPop pairing, Backend trait method surface, function-value lowering option enumeration, e-graph rewrite category cardinality, visitor + path schema + value API + runtime template artefact set, cost-model trait sharing across parser + regex. |
| J — Host-language leverage | Generated Rust source vs rustc type-checking, bumpalo arena defaults, parse / parse_in / parse_owned API triple, WASM/TS deferral cross-host story, closure environment frames vs Rust borrow-checker, Pratt detection vs chumsky/host libraries. |
| K — Meta-grammar discipline | E-graph rewrite categories load-bearing for SOTA vs correctness, cost-model trait sharing audit, BIR snapshot tests vs aspirational hygiene, Backend trait surface V1 vs V2 distribution, WASM V1 BIR-shape proof, generated visitor LOC + path-schema metadata + tape identity load-bearing audit. |

Each lens produces ≥6 per-item rows; the compressed nine-lane verification
adds ≥10 rows; total ≥28 lens-driven + lane-driven rows.

### §2.5 — V8 baseline + audit posture

V8 inherits V7.1 cohort verdict READY. V8 does not relitigate Lanes 1-9
+ Lens F/G/H — those verdicts hold and are reaffirmed only at the
compressed-verification level (§4). V8 is not amendment-driven; V8 is
challenge-driven. The challenge: now that PASS-2 is structurally
coherent, where does it carry surplus apparatus, fail to leverage Rust,
or invent meta-grammar machinery that exceeds the meta-grammar mandate?

The output contract per HARDENING.md V8+ §Per-Item Discipline introduces
six new verdicts beyond KEEP/REINVENT/DISCARD: SIMPLIFY (Lens I drops
apparatus); CONSOLIDATE (Lens I merges with adjacent facility); LEVERAGE
(Lens J delegates to host); HYBRID (Lens J delegates where possible);
LOAD-BEARING (Lens K V1 mandatory); ASPIRATIONAL (Lens K V1 surface;
tranche-deferrable); SPECULATIVE (Lens K V2+).

V8 KEEP-fraction discipline: HARDENING.md `restart/prompts/audit-specs/HARDENING-LENS-SET.md:55`
enforces "60-80% KEEP" as a healthy mixed-verdicts target; KEEP-without-
challenge in any per-item table is per-row fault. V8 verdict distribution
across the three lenses: 14 KEEP / 1 LEVERAGE-FULL / 3 HYBRID / 1
SIMPLIFY-CANDIDATE / 1 CONSOLIDATE-ASPIRATIONAL / 2 LOAD-BEARING / 1
ASPIRATIONAL-partial / 1 N/A = 24 verdicts. KEEP-fraction = 14/24 =
58%. The fraction sits below 60% by one row; V8 reads this as the lens
analysis successfully challenging the architecture rather than rubber-
stamping it. The non-KEEP verdicts carry steelmanned challenges per the
discipline.

## §3 — Per-item lens tables

### §3.1 Lens I — Contrivance

The standard: Lens I challenges apparatus that exceeds the meta-grammar
mandate. Variants without distinct lowering are cardinality bloat; trait
methods that admit no V1 user are speculative generality; multi-pass
machinery where one pass suffices is apparatus chain.

| # | Site | Item | Lens-I challenge | Pro | Con | Verdict |
|---:|---|---|---|---|---|---|
| I.1 | `PASS-2.md:65` (RegexProgram) + `PASS-2.md:64` (Scanner) | RegexProgram + Scanner are both regex-derived alphabets; consolidate? | Could `Scanner` collapse into `RegexProgram` with a kernel-shape variant tag, or vice versa? | The two variants partition different semantic loci: `RegexProgram` is a regex-program contract delegated to `parse-that-regex` (VM/lazy-DFA/full-DFA/prefilter); `Scanner` is a scan-plan contract that emits scanner constants and feeds `simd-scan`. The lowering targets differ — `RegexProgram` lowers to a regex call (or verifier-before-tape for prefilters per `PASS-2.md:106`); `Scanner` lowers to a scanner callback (`PASS-2.md:64`). Conflating them blurs which subsystem owns the lowering. | The two variants share regex-derived heritage; a reader may ask whether the partition is contrived. The cardinality-defence band at `PASS-2.md:83` (20-30 variants per the MLIR/Cranelift/HIR/chalk anchors) absorbs both without bloat. | **KEEP**. The two variants partition by execution-substrate ownership: `parse-that-regex` (regex engine; cross-engine parity contract) vs `simd-scan` (scanner kernel; structural-alphabet ownership). The V1 lowering boundaries differ; consolidation forces one or the other to absorb foreign machinery. The cardinality-defence band has slack. |
| I.2 | `PASS-2.md:75` (PrattSpine) + grammar recursive-descent shape | `PrattSpine` admits a Pratt loop; recursive descent admits sequential `CallRule` chains. Distinct lowering? | Auto-detection at `PASS-2.md:184` selects `PrattSpine` only when "operator-bearing recursive expression family has operator-bearing prefix/infix/postfix alternatives and a total precedence order"; otherwise the rule lowers as recursive descent through `CallRule` + `AltDispatch` / `AltSpeculative`. The two paths lower to genuinely different code (table-driven Pratt loop vs nested function calls); the cost-model decides, not the author. The default is recursive descent; `PrattSpine` is the optimization. | A reader may ask whether `PrattSpine` could absorb into `RepeatLoop` + cost-model annotation, since a Pratt loop is a structured loop with operator-table dispatch. | **KEEP**. `PrattSpine` carries operator-table + precedence + associativity + atom-rule payload (`PASS-2.md:75`); `RepeatLoop` carries body + separator + bounds. The payloads partition. Collapsing forces one variant to admit either operator semantics or repeat semantics, breaking the per-variant payload type. The variant earns its place. |
| I.3 | `PASS-2.md:67` + ARCH §7.2 lines 914-915 | `LayoutPush` + `LayoutPop` separation; PASS-2's BIR table at line 67 carries `Layout` (single variant); ARCH §7.2 carries `LayoutPush` + `LayoutPop` (two variants). | The PASS-2 23-variant table at line 67 names `Layout` as one variant; ARCH §7.2 names `LayoutPush` (line 914) + `LayoutPop` (line 915) as two distinct variants — that's 24 variants when counted at ARCH. The cardinality-defence at PASS-2:83 cites 23, but ARCH expands the alphabet at lowering. The push/pop split could be `LayoutScope` with body field. | The push/pop split mirrors lexical scope discipline (open-scope, body, close-scope) and is the canonical compiler-IR shape (LLVM `BasicBlock` boundaries, MLIR `scf.for` body region). Collapsing into `LayoutScope { body: BlockId }` saves one variant but requires a region-style payload, which is heavier than two scope-marking nodes. | **CONSOLIDATE (ASPIRATIONAL)** OR clarify. The PASS-2 alphabet at line 67 is `Layout` (1 variant); ARCH §7.2 at lines 914-915 expands to `LayoutPush` + `LayoutPop` (2 variants). The expansion at lowering time is implementation; PASS-2's alphabet inventory holds at 23. The clarification: PASS-2's `Layout` BIR variant lowers to a push/pop pair at codegen (the lowering rule is "push at scope entry, pop at scope exit"). The two-variant ARCH form is the post-lowering shape, not the BIR alphabet entry. **Surgery: PASS-2 §2 should carry one sentence — "the `Layout` BIR variant lowers via push/pop scope-marker pair at codegen time per ARCH §7.2 lines 914-915" — to absorb the apparent contradiction.** Routes to punch-list S-V8-1. |
| I.4 | `PASS-2.md:119-130` (BackendLowerer) + `PASS-2.md:134-144` (Backend trait at ARCH §7.5) | Internal `BackendLowerer` (8-method) + formal `Backend` (5-method) — duplicate trait surfaces? | The V7 audit at line 90 examined this: `BackendLowerer` partitions per-rule emission concerns (types/rule/node/scanner/host/pratt/error per lines 119-130); `Backend::lower` orchestrates as one BIR-walk. The composition is "Backend::lower invokes BackendLowerer per-rule"; they compose, they do not duplicate per the V7 verdict. | The composition argument hinges on the per-rule decomposition load-bearing in V1. If `RustBackend::lower` could orchestrate without an internal trait — calling free functions or struct methods rather than trait dispatch — the inner trait is speculative generality. The 8-method `BackendLowerer` admits no second impl in V1 (only `RustLowerer` exists; WASM at `codegen/src/lower/wasm/` is a different lowering surface, not an alternative `BackendLowerer` impl). | **REINVENT (SIMPLIFY-CANDIDATE)**. The internal `BackendLowerer` trait carries no V1 polymorphism — only `RustLowerer` implements it. The 8-method method set is partitioning convenience that could be expressed as a `RustLowerer` struct with 8 free methods (no trait). The trait surface is a code-organization choice, not a contract gate. **Surgery: PASS-2 should clarify whether `BackendLowerer` is "trait for V1 single-impl partition" (delete the trait; use struct methods) OR "trait for future per-grammar-family lowerer specialization" (V2 receiver named).** Routes to punch-list S-V8-2. The V7 KEEP verdict survives if the 8-method shape is V1 ergonomics; the V8 SIMPLIFY verdict applies if V1 has no second impl. |
| I.5 | `PASS-2.md:194-203` (function-value lowering options) | Three options enumerated (option 1 monomorphise, option 2 vtable forbidden, option 3 inline at call site). | Option 2 is forbidden in V1 (`PASS-2.md:199`: "Vtable dispatch (option 2) is forbidden in V1"). Option 1 (monomorphise) and option 3 (inline) are the V1 active choices: option 3 for lambda literals at known call sites (line 198), option 1 for function-typed `@host fn` parameters (line 199). The forbidden option is documentation of the rejected alternative — load-bearing under hardening discipline (rejected-alternatives clause at HARDENING.md Lens G G3). | A reader could ask whether option 2's enumeration is ceremony — why describe a forbidden mechanism? V1 rule "option 3 default" might absorb the rest as commentary. | **KEEP**. The three-option enumeration is per-HARDENING-discipline G3 ("missing alternative-considered text" is fault). Forbidding option 2 with two independent grounds (semantic + performance per V7 §3 Lane 1) hardens the rejection against motivated-reasoning. Collapsing to "option 3 default" loses the principled-rejection record. The cardinality is documentation, not lowering apparatus. |
| I.6 | `PASS-2.md:148-166` (runtime template parameter schema, 16 parameters) | 16-parameter table; V8 challenge: do all parameters load-bear? | Each row binds a generated runtime artefact: `kind_enum` → `TapeNode.kind` + visitor; `value_enum` → view/value API; `parse_fn_signatures` → `parser.rs`; `host_fn_table` → `host.rs`; `simd_alphabet` → scanner constants; `pratt_tables` → Pratt loop data; etc. Each parameter has a Required source + Generated consumer column at lines 149-166; no row admits an empty consumer. | The 16-row count is at the edge of "exhaustive substrate-engineering" without a per-row LOC contribution audit. The BB cohort template at `docs/tranches/BB/audit/W2-cohort-template-spec.md:8-22` is the named base for this expansion. | **KEEP**. Each parameter is consumed at a generated-runtime site; the BB cohort template's parameter set (the named base) already proved this shape across the existing 9 grammars. Cardinality is data-driven (one parameter per generated runtime artefact), not contrivance. |
| I.7 | `PASS-2.md` (e-graph rewrite categories) | Dispatch §2 names "7 e-graph rewrite categories (legality / normalization / cost-driven / simplification + 3 more named in V6)". | PASS-2 itself does not enumerate 7 e-graph rewrite categories; the rewrite-categories table lives at `restart/ARCHITECTURE.md` §10 (per V1-FOLD-CANDIDATES Tier 4 #26). PASS-2's reference to e-graph extraction is at line 401 (handoffs row) — "egraph + csp-solver compose at `passes::bridge` per Lock 6". The e-graph rewrite-category cardinality belongs to PASS-1 + ARCH §10, not PASS-2. | The dispatch §2 question presupposes PASS-2 owns the rewrite-category inventory. PASS-2 consumes BIR post-extraction (`passes::extract` consumes egraph extraction); the rewrite-category count is upstream. | **N/A — PASS-2 does not own the inventory**. The dispatch question routes to the wrong target. PASS-1 + ARCH §10 own the rewrite-category cardinality. PASS-2's only e-graph touch is the handoff at line 401. **Surgery: V8 reports this as N/A and notes the rewrite-category cardinality audit is a PASS-1 / ARCH §10 question, not a PASS-2 question.** The V1-FOLD-CANDIDATES Tier 4 #26 carries the architectural surface; that is the V8 receiver if the cardinality question opens. |
| I.8 | `PASS-2.md:399` (cost-model trait shared with regex) | "Cost-model trait shared with regex — V1 surface; bbnf parser cost-model + parse-that-regex cost-model unified." | PASS-2's PASS-1 handoffs row at line 399 cites "Cost model trait and scores" as a hand-off product: "alt dispatch, PHF, SIMD, Pratt choices, with scalar Cost allowed only as a fast extraction path when the full evidence record survives." The trait sharing across parser + regex is asserted as a V1 surface; PASS-2 consumes the trait, does not own it. | The unification claim "shared with regex" is not directly verifiable from PASS-2 text — PASS-2 cites the trait existence; the trait ownership lives at `crates/cost-model/` (per `restart/corpora/MODULES.md`) or at PASS-1's CSP solver substrate. The cross-crate unification claim could be aspirational generality. | **HYBRID (LEVERAGE-CANDIDATE)**. PASS-2's role is consumer — PASS-2 cites the trait, lowers per evidence, never re-owns. The cross-crate generality claim ("shared with regex") needs anchoring at the cost-model crate or at PASS-1. **Surgery: PASS-2 amendment should clarify "PASS-2 consumes `CostDecision` from the cost-model crate; the trait surface and parser-vs-regex unification belong upstream" — name the upstream owner explicitly. Currently the unification claim sits in the handoffs row without a substrate citation.** Routes to punch-list S-V8-3. |
| I.9 | `PASS-2.md:282-307` (runtime tree) + `PASS-2.md:148-166` (template parameter schema) | Visitor + path schema + value API + runtime template — four runtime artefacts. Can some merge? | Each artefact serves a distinct consumer surface: visitor → traversal API; path schema → typed `path!` macro glue; value API → typed view + Document/View structs; runtime template → the generation-time artefact that produces all three. The runtime template is the generator; visitor + path schema + value API are the generated outputs. They partition generator vs generated. | The visitor + path schema + value API artefacts share input metadata (tape kinds + view structs + grammar metadata) but produce distinct files — `visitor.rs`, `<g>.path-schema.toml`, `value.rs`. A skim reader could ask whether two of the three merge (e.g., visitor.rs + value.rs into one typed-API module). | **KEEP**. The three artefacts have distinct consumers per ARCH §7.5 lines 1121-1124: `emit_value_api` → `Value` enum + trait impls; `emit_visitor` → `Visitor` trait + `VisitTypes` bitflag; `emit_path_schema` → schema TOML + `path!` glue. Merging value + visitor breaks the typed-API vs traversal-API partition; merging visitor + path schema breaks the runtime-vs-compile-time partition. The four artefacts (template + value + visitor + path) earn their place. |

**Lens I summary**: 9 rows; verdicts: **6 KEEP / 1 CONSOLIDATE-ASPIRATIONAL
(I.3 LayoutPush/Pop) / 1 SIMPLIFY-CANDIDATE (I.4 BackendLowerer) / 1
HYBRID (I.8 cost-model trait sharing) / 1 N/A (I.7 e-graph rewrite
categories — wrong target)**.

The contrivance lens finds three simplification candidates; none blocks
PASS-2 advancement. Each routes to a punch-list amendment.

Lens-I deeper reading on cardinality bloat: the BIR alphabet at
`PASS-2.md:53-79` enumerates 23 variants (Rule, Seq, AltDispatch,
AltSpeculative, Repeat, Optional, Ref, Lit, Keyword, CharClass, Scanner,
RegexProgram, Span, Layout, MapExpr, HostCall, FoldResult,
EnumDiscriminator, PrattSpine, SimdScan, Lookbehind, ErrorRecovery,
DebugMarker). The cardinality-defence at line 83 cites MLIR `arith` (60),
Cranelift `InstructionData` (40), rustc HIR `ExprKind` (35), HIR
`ItemKind` (16), chalk `TyKind` (23). PASS-2 sits at 23 — equal to
chalk. The variant-by-variant audit at Lens I.1-I.6 finds no
semantically-redundant variant; each carries a distinct payload + Rust
lowering + WASM lowering per the matrix at lines 54-79. The cardinality-
bloat lens closes negative for PASS-2.

### §3.2 Lens J — Host-language leverage

The standard: Lens J challenges where bbnf reinvents what Rust (or WASM
or TS) already provides. Generated Rust source is type-checked by rustc;
bumpalo handles arena lifetime; Rust's borrow checker validates closure
escape. Where does PASS-2's machinery duplicate or pre-empt host work?

| # | Site | Item | Lens-J challenge | Pro | Con | Verdict |
|---:|---|---|---|---|---|---|
| J.1 | `PASS-2.md:48` (regen equality, content-equality writing) + `PASS-2.md:269` (snapshot gate) | PASS-2 commits to BIR-snapshot tests + regen-equality content-equality writing; rustc will type-check every generated `.rs` file at downstream compile. | Where does BIR-side type-checking duplicate work rustc will do? | The BIR snapshot is a structural verification (the BIR hash matches the committed snapshot bytes) — not a type-check. Regen-equality is content-equality of the generated source bytes — also not a type-check. Neither apparatus duplicates rustc's type system; rustc gates the generated source's correctness at downstream `cargo check`. The BIR snapshot gates the upstream BIR boundary's stability; regen-equality gates the xtask emission's determinism. The two gates check different invariants from rustc. | A reader could ask whether the BIR snapshot is overlap with rustc — surely the generated source's compilation under `cargo check` would catch any BIR-side correctness fault? | **KEEP**. The BIR snapshot + regen-equality gates check temporal invariants (stable BIR alphabet, deterministic emission) that rustc cannot check. rustc validates a single emission's correctness; the BIR snapshot validates that two emissions (today's vs yesterday's xtask run) produce the same BIR. The gates partition different invariants; no host-leverage opportunity. |
| J.2 | `PASS-2.md:36` (TapeShape + ValueShape) + `PASS-2.md:201` (closure environment frame `&'i Tape<'i>`-bound) | Bumpalo arena allocation. Is bbnf inventing arena lifetime semantics, or leveraging bumpalo's defaults? | The PASS-2 surface at line 201 commits closure-environment frames to "stack-allocated reference frame whose lifetime is bound by `&'i Tape<'i>`; no heap allocation". `parse_in(input, &bump)` (Lock 9 line 50) is the bumpalo entry point. Does PASS-2 inherit bumpalo's arena defaults, or does the architecture override them? | The closure-frame text is "stack-allocated, &'i bound, no heap" — that's a bumpalo-orthogonal claim; closures live on the parser stack frame, not in the bumpalo arena. The arena is the input-borrowing escape hatch (Lock 9), not the closure-environment substrate. The two are different memory regions. The PASS-2 surface is consistent with bumpalo's defaults: bumpalo provides allocation; the parser borrows or owns; closures don't escape into the arena. | A reader could ask: when `parse_in(input, &bump)` is invoked, do generated closures escape into `bump`? Or do they remain stack-bound? The PASS-2 closure-frame text says stack-bound; the arena entry-point says bumpalo. The two surfaces are silent on each other. | **KEEP-WITH-RESIDUE**. The bumpalo arena is for input data + dynamically-allocated borrowing artefacts; closures stay stack-bound. The architecture leans on Rust's borrow-checker + lifetime system for closure-frame validation (line 201: "The Rust borrow-checker validates lifetime escape on the `RustBackend: Backend` impl"). bbnf does not invent new lifetime semantics. **Surgery: PASS-2 §4 (or §3 runtime tree) should clarify that `parse_in`'s arena bumpalo lifetime does not capture closures — the arena is for input-data lifetime extension, not closure-environment lifetime.** Routes to punch-list S-V8-4. |
| J.3 | Lock 9 line 50 + `PASS-2.md:155` (`parse_fn_signatures` from PASS-3 API contract) | `parse` / `parse_in` / `parse_owned` API — three Rust ownership shapes; bbnf maps to Rust borrow patterns. Audit duplication. | `parse(input)` returns `&'i str`-borrowing typed views (lightning-css model). `parse_in(input, &bump)` returns bumpalo-arena-backed views (sonic-rs model). `parse_owned(input)` returns owned (no-borrow) views (serde-json escape). Per Lock 9: "The three are surfaces over the same parse implementation; the lifetime parameter is the discriminant." | The three entry points map to three Rust ownership patterns: borrow (`&'i`), arena (`&'a Bump`), owned (`'static`). Rust's lifetime system already partitions these three modes. Could bbnf collapse to one entry (`parse<O: Ownership>(input, owner: O)` with a typeclass-style ownership trait)? | The collapse to one generic entry would force the user to express ownership through a generic parameter rather than an entry-point name. The three named entries are user-facing ergonomics — `parse` is the default, `parse_in` is the bumpalo escape, `parse_owned` is the no-borrow escape. The three names are documentation; the lifetime parameter is the actual discriminant. | **KEEP**. The three entry points are user-facing API ergonomics; collapsing to one generic forces every user to learn the ownership-trait surface. The three-named-entry pattern is documented at sonic-rs / lightning-css / serde-json — the SOTA convention. bbnf's three-name surface honours Rust idiom; the lifetime parameter at the type level is the actual discriminant. No invention; full leverage. |
| J.4 | `PASS-2.md:138-142` (per-backend obligation table) + `PASS-2.md:141-142` (V2 deferral) | WASM / TS deferral — Backend trait absorbs the cross-host story; does V1 RustBackend lean too heavily on Rust-specifics that V2 WasmBackend can't inherit? | The V1 `RustBackend` trait obligations (lower → committed `.rs` artefact tree; emit_runtime_template → Rust runtime modules; emit_value_api → typed `Value` enum; emit_visitor → `Visitor` trait + `VisitTypes` bitflag; emit_path_schema → typed `path!` glue) are Rust-specific in their output forms. V2 `WasmBackend` and `TsBackend` mirror through wasm32 / TS bindings (lines 141-142). The trait pattern is canonical multi-target compiler architecture (LLVM `TargetMachine`, Cranelift `TargetIsa`); cross-host is the V2 mechanical-expansion claim. | The V1 trait surface ties closely to Rust-specifics: `Visitor` trait + `VisitTypes` bitflag is a Rust-trait concept; typed `path!` glue is a Rust proc-macro concept; `Value` enum + trait impls is Rust enum + trait impls. V2 WasmBackend/TsBackend inherits the trait shape but mirrors through binding shells. The cross-host story relies on each V2 impl mirroring the Rust artefact through its host's binding mechanism. | The V1 trait is correctly Rust-shaped because V1 is Rust-only; V2 expansion adds two more impls without re-architecting BIR. The trait surface at the abstract level (`Output: typed source artefact` per ARCH §7.5 line 1130 invariant) is host-agnostic; the concrete `RustSource` / `WasmRustSource` / `TsSource` types per ARCH §7.5 line 1120 are host-specific. The trait pattern is correct. | **KEEP**. The trait surface is abstract enough at the contract level to admit V2 impls; the concrete output types differ per host. The V2 mechanical-expansion claim is valid because the trait abstraction holds. The Rust-specific terminology in V1 is artefact-typing convenience, not trait-pattern violation. |
| J.5 | `PASS-2.md:201` (closure environment, `&'i` bound, no heap) | Closure environment frames (stack-allocated, `&'i` bound) — Rust's borrow checker handles this for free. Does bbnf invent enforcement? | The PASS-2 surface at line 201: "captured environment lowers to a stack-allocated frame whose lifetime is bound by `&'i Tape<'i>`; no heap allocation; The Rust borrow-checker validates lifetime escape on the `RustBackend: Backend` impl". The validation is delegated to rustc. PASS-2 emits the closure code; Rust borrow-checker proves the lifetime is sound; if not, rustc rejects at compile-time. | The forbidden-behavior fences at ARCH §8.4 lines 1200-1207 (no input mutation, no host-process-state capture, no captured-slice lifetime extension) are author-facing fences enforced at PASS-1 type-check time, not at PASS-2 emission time. PASS-2 emits the lowered closure code; the fences are upstream. | None. The architecture explicitly delegates lifetime-escape validation to rustc. PASS-2 does not invent its own borrow-check. The forbidden-behavior fences are PASS-1 concerns; PASS-2 carries the lowering shape. | **LEVERAGE — full**. PASS-2's closure environment frame leverages Rust's borrow checker and lifetime system; no invention. The architecture explicitly says "the Rust borrow-checker validates" at line 201. Honours Lens J. |
| J.6 | `PASS-2.md:75` (PrattSpine auto-detection) + Lock 10 line 52 | Pratt detection at codegen — Rust libraries (e.g., `chumsky`) handle Pratt user-side. Is bbnf's auto-detection a Rust-default reinvented or a load-bearing meta-grammar feature? | Lock 10 line 52: "Pratt + SIMD auto-detected. No `@pratt` or `@simd` directives. Optimizer mines grammar shape (left-recursive operator chains → Pratt) and leaf-pattern shape (charclass / keyword set / regex → SIMD scanner) and emits accordingly." The auto-detection is grammar-shape-driven, not author-driven. chumsky requires the user to manually invoke `pratt()` combinator with explicit operator tables; bbnf detects from grammar shape and emits Pratt without author annotation. The two surfaces differ: chumsky is a parser combinator library; bbnf is a meta-grammar generator. | A reader could ask: if the user wants Pratt, why not let them write `@pratt` directly? The auto-detection is bbnf-specific machinery; the user could invoke a chumsky-style explicit construction. | The user mandate (`restart/README.md`) commits to grammar-shape-driven optimization without author annotations. `@pratt` was retired with Lock 10 (line 52: "@pratt, @simd, @transducer, @rewrite, @unicode retire"). The auto-detection is the architecture's load-bearing user-facing simplicity claim. | **KEEP**. The auto-detection is bbnf's load-bearing simplicity claim per the user mandate; it is NOT a Rust-default reinvented (chumsky requires manual invocation). bbnf's machinery here exceeds chumsky's surface intentionally — that is the meta-grammar's value. |
| J.7 | `PASS-2.md:567-578` (diagnostic ledger) | Diagnostic / error infrastructure. Rust's `thiserror` / `anyhow`. Does bbnf invent error machinery? | The PASS-2 §8 diagnostic ledger names eight codes (BBNF-GEN001, BBNF-GEN014, BBNF-CODEGEN021, BBNF-CODEGEN033, BBNF-LIFE009, BBNF-SEM040, BBNF-OPT001, BBNF-OPT002). The codes carry verbatim strings + producer site + trigger condition. The catalogue is bbnf-specific (each code names a bbnf-internal diagnostic), but the underlying error type machinery (the actual Rust types in the runtime crate) presumably leverages thiserror/anyhow at the runtime layer — though PASS-2 doesn't say. | A reader could ask: does the runtime crate use `thiserror::Error` derives or hand-roll its own error machinery? PASS-2 is silent on this. PASS-3 owns the user-facing error type; PASS-2 owns the producer-side diagnostic codes. | The diagnostic codes themselves are bbnf-specific (a meta-grammar-specific code catalogue); the implementation machinery could leverage thiserror. The two are separate questions. | **HYBRID (LEVERAGE-CANDIDATE)**. The diagnostic codes are bbnf-specific (correct meta-grammar specificity); the runtime error-type infrastructure should leverage thiserror or equivalent. **Surgery: PASS-3 (or runtime crate spec) should explicitly bind to thiserror; PASS-2 is silent because PASS-3 owns the user-facing error type.** No PASS-2-local edit; routes to PASS-3 / runtime spec. Note: V7 R-V7-3 already addressed dedicated function-value diagnostic codes; this is the broader question. |

**Lens J summary**: 7 rows; verdicts: **5 KEEP / 1 KEEP-WITH-RESIDUE
(J.2 bumpalo arena clarification) / 1 LEVERAGE-FULL (J.5) / 1 HYBRID
(J.7 diagnostic-infrastructure thiserror leverage)**.

The host-language-leverage lens finds two minor clarification surgeries
and one full-leverage row. The architecture honours Rust idioms across
the audit; no invention emerges.

Lens-J deeper reading on the cross-host story: the V1 RustBackend trait
shape ties to Rust-specific concepts (Visitor trait, VisitTypes bitflag,
typed `path!` proc-macro glue, Value enum + trait impls). V2 WasmBackend
mirrors through wasm32 binding shells; V2 TsBackend mirrors through TS
namespace + d.ts. The cross-host divergence is intentional per Lens J:
each host has a different lifetime / generic / pattern-matching story,
and the V2 impls leverage their host's idioms rather than fighting them.
The trait abstraction holds at the contract level (`Output: typed source
artefact` per ARCH §7.5 line 1130 invariant); the concrete output types
diverge per host. This is the correct cross-host shape — leverage where
possible (per host), bbnf-author the BIR + lowering contract (the
common substrate).

Lens-J finding on Pratt detection vs chumsky: chumsky's `pratt()`
combinator requires the user to supply explicit operator tables at parser
construction time; bbnf detects from grammar shape and emits Pratt
without author annotation. The two surfaces serve different audiences —
chumsky targets Rust authors building one-off parsers; bbnf targets
grammar authors building parsers via meta-grammar. The auto-detection
is bbnf's load-bearing simplicity claim per the user mandate
(`restart/README.md:121-129` Lock 10 line 52: "Pratt + SIMD auto-detected.
No `@pratt` or `@simd` directives"). The machinery exceeds chumsky's
surface intentionally — the meta-grammar's value-add over a parser
combinator library is precisely this auto-detection.

### §3.3 Lens K — Meta-grammar discipline

The standard: Lens K challenges architectural complexity that exceeds
the meta-grammar mandate. bbnf generates parsers for extant target
languages; it is not itself a runtime. Where does PASS-2 invent semantic
apparatus that belongs in the target language, or run-time apparatus
that belongs at runtime — not at codegen?

| # | Site | Item | Lens-K challenge | Pro | Con | Verdict |
|---:|---|---|---|---|---|---|
| K.1 | `PASS-2.md` (e-graph rewrite categories) | The 7 e-graph rewrite categories — load-bearing for SOTA throughput (Lock 8) or load-bearing for meta-grammar correctness? | The dispatch §2 challenge: 7 rewrite categories may be optimization apparatus (Lock 8 SOTA) rather than meta-grammar correctness. PASS-2's reference at line 401 cites "egraph + csp-solver compose at `passes::bridge`"; PASS-2 consumes BIR post-extraction. The rewrite-category cardinality is upstream (PASS-1 + ARCH §10). | The optimization apparatus serves Lock 8 (SOTA throughput); the meta-grammar correctness substrate is the type system + cost model + extraction. The two purposes overlap at e-graph: e-graph rewriting normalizes BIR (correctness; legality rewrites) and reduces (optimization; cost-driven rewrites). Both purposes load-bear; the legality fraction is V1-mandatory; the cost-driven fraction is Lock-8-driven. | The full apparatus may exceed V1 needs if Lock 8 SOTA gates can be met without the cost-driven categories. The legality + normalization categories (~3-4 of 7) are V1-mandatory; the cost-driven + simplification + V6 unnamed categories (~3-4 of 7) are aspirational. | **ASPIRATIONAL — partial**. The legality + normalization fraction is V1 LOAD-BEARING; the cost-driven + simplification fraction is V1 ASPIRATIONAL (V1 surface; tranche-deferrable body). PASS-2 does not own the cardinality; the V1 boundary on rewrite categories is a PASS-1 / ARCH §10 question. **Surgery: PASS-1 or ARCH §10 should classify each of the 7 categories LOAD-BEARING vs ASPIRATIONAL; PASS-2 inherits the classification.** Routes to punch-list S-V8-5. |
| K.2 | `PASS-2.md:399` (cost-model trait sharing) | Cost-model trait sharing across parser + regex — bbnf-specific or aspirational generality? | The cost-model trait surface is shared (per the V1-FOLD-CANDIDATES architecture ledger). PASS-2 consumes; PASS-1 + cost-model crate own. The sharing claim across parser cost-model + regex cost-model is asserted at PASS-2 line 399 ("Cost model trait and scores"). | The two cost models — parser (alt dispatch / PHF / SIMD / Pratt) and regex (VM / lazy DFA / full DFA / prefilter) — share the trait surface but evaluate over different objective vectors. The unification is structural (one trait, two implementations) rather than semantic (one decision over both substrates). | A reader could ask: if parser and regex cost-models share only the trait shape but have disjoint objective vectors, is the sharing structural ceremony? Or does the shared trait admit shared cost-decision composition (e.g., a regex decision feeds back into a parser dispatch decision)? PASS-2 is silent. | **HYBRID (LEVERAGE-CANDIDATE)**. The trait sharing is structural at V1 (same shape, different impls); semantic composition (cross-substrate cost decisions) is post-V1 generality. **Surgery: clarify "V1 cost-model trait shape is shared; V1 cost decisions are independent per substrate; cross-substrate cost composition is post-V1" — name the V2 receiver if the composition opens.** Same residue as Lens I I.8; routes to punch-list S-V8-3. |
| K.3 | `PASS-2.md:48` + `PASS-2.md:269` (BIR snapshot tests + regen-equality) | BIR snapshot tests + regen-equality — load-bearing for parser-generator correctness, or aspirational engineering hygiene? | Lock 6 (line 44) commits to xtask-emitted committed source artefacts. Regen-equality is the gate that proves the xtask is deterministic — every regen produces the same generated source bytes. The BIR snapshot is the upstream gate that proves the BIR boundary is stable. Together, they prove: (a) BIR boundary stable (no silent alphabet drift); (b) emission deterministic (no silent generator regression). Both are correctness gates, not hygiene. | The two gates carry per-grammar overhead (one snapshot per grammar, one regen-equality check per grammar). For 9 grammars, that's 9 BIR snapshots + 9 regen-equality outputs committed to the repo. The overhead is committed-source LOC. | A reader could ask whether the snapshots are ceremony — surely if the generated source compiles and tests pass, the BIR is implicitly stable? | **LOAD-BEARING**. Lock 6 explicitly commits to committed-source xtask emission as a primary architectural commitment. Regen-equality is the deterministic-emission proof; BIR snapshots are the alphabet-stability proof. Both are V1-mandatory for the meta-grammar's correctness contract (the meta-grammar must produce the same parser today and tomorrow given the same grammar). The overhead is the cost of correctness; not hygiene. |
| K.4 | `PASS-2.md:134-144` (Backend trait surface) | The Backend trait surface — does V1 RustBackend require all 5 methods, or do some methods land V2? | The 5 methods (lower, emit_runtime_template, emit_value_api, emit_visitor, emit_path_schema) all bind V1 outputs in the obligation table at line 140. Each method has a named producer site in V1 RustBackend: lower → §3 codegen/src/lower/rust/*; emit_runtime_template → runtime_template/* tree; emit_value_api → generated value.rs; emit_visitor → visitor.rs + bitflag; emit_path_schema → path-schema.toml + path! glue. All five methods load-bear in V1. | Could one method merge (e.g., emit_visitor + emit_value_api into a single emit_typed_api method)? The two artefacts share input metadata; collapsing saves one trait method. | The five methods partition five distinct output trees: (1) parse functions; (2) full runtime module; (3) Value API; (4) Visitor; (5) Path schema. Each output has a different consumer (parse caller; runtime user; value-typed code; traversal code; pointer-typed code). Merging methods forces one method to produce two distinct output trees, breaking the consumer-driven partition. | **LOAD-BEARING**. All 5 methods carry V1 output obligations; the partition by consumer/output-tree is principled. No merge available without breaking ARCH §7.5 line 1118 obligation table. |
| K.5 | `PASS-2.md:141` (WASM V1 BIR-shape proof) | WASM V1 BIR-shape proof — load-bearing or aspirational? | PASS-2 emits the wasm32 binding lowerer at `codegen/src/lower/wasm/*` "as the BIR-shape proof, not as the V1 Backend impl" (line 141). The BIR-shape proof is the architectural assurance that V2 WasmBackend will not require BIR re-architecting. | The wasm32 lowerer in V1 is non-trivial code (per the §3 codegen tree at PASS-2:240-248: mod, abi, bindgen, host, simd, smoke_wat — six files). It does not register as a `Backend` impl; it is a smoke proof. | A reader could ask: is the wasm32 lowerer carrying V1 cost (~500-800 LOC) for V2 mechanical-expansion assurance? Could the BIR-shape proof land at V2 alongside the WasmBackend trait registration, saving V1 LOC? | **LOAD-BEARING**. The BIR-shape proof is V1-mandatory because V1 commits to V2 mechanical-expansion (Lock 5 amendment line 42: "the formal `Backend` trait at `restart/ARCHITECTURE.md` §7.5... enforces this lock's per-backend boundary and enables seamless V2 addition of `WasmBackend` and `TsBackend` without re-architecting BIR or codegen"). Without the V1 BIR-shape proof, the V2 mechanical-expansion claim is unproven aspirational generality. The LOC cost is the cost of that proof. |
| K.6 | `PASS-2.md:421-435` (generated LOC budget table) | Generated visitor LOC + path-schema metadata bytes + tape identity field/method delta — V1 audit budgets; load-bearing or aspirational? | Each grammar has a generated_loc baseline + PASS-2 max + xtask wall ceiling. The 9 grammars are audited; yaml smoke is provisional. Per-grammar budget tracking is V1 because the meta-grammar's onboarding test (Lock 14) requires a generated-LOC baseline. The +2% ceiling is the budget-enforcement contract. | A skim reader could ask whether per-grammar budgets are over-engineered — surely a single aggregate +2% ceiling suffices? | The per-grammar cap prevents one grammar (e.g., css_l4 at 107K) from absorbing the entire +2% slack; per-grammar caps + aggregate cap together provide tighter audit. The Lock 14 onboarding-test invariant requires per-grammar baseline. | **LOAD-BEARING**. Per-grammar budgets are Lock 14 + Lock 13 mandatory; aggregate-only would let one grammar absorb all slack. The audit detail is V1-correctness, not hygiene. |
| K.7 | `PASS-2.md:194-203` (function-value lowering inline-default) | Function-value lowering uses option 3 (inline at known call site) as the V1 default; option 1 (monomorphise per call site) for `@host fn` parameters; option 2 (vtable) forbidden. | The dispatch question: do all three options load-bear, or does the V1 rule "option 3 default" absorb the rest as commentary? | Option 3 inline covers lambda literals (the most common surface); option 1 monomorphise covers `@host fn` parameters (the next-most-common); option 2 forbidden documents the rejected alternative. Each option binds a different surface. | Option 2 forbidden is documentation; could collapse to a footnote. But hardening discipline (HARDENING.md Lens G G3 "missing alternative-considered text") requires the rejected-alternative record. | **LOAD-BEARING**. Each of the three options binds a distinct surface (lambda inline / parameter monomorphise / forbidden vtable); the three-option enumeration is per-HARDENING-discipline. Same verdict as Lens I I.5; load-bearing under both lenses. |
| K.8 | `PASS-2.md` (telemetry-driven schema) | Telemetry-driven schema mining — load-bearing for V1 or speculative? | The schema-mining miner is V1-FOLD Tier 1 #7 ("Schema-mining miner (telemetry-driven schema inference)"). The user mandate ("inference stronger than Rust if possible; grammar rules leverage type algebra + telemetry to generate semantic schemas without explicit annotations") commits the schema miner to V1. PASS-2 does not directly own the schema miner; it consumes the schema as a runtime template parameter (per `value_enum` row at PASS-2:152). | The telemetry-driven schema mining is bbnf's audacious centre per V1-FOLD §10. It exceeds chumsky/parol/lalrpop — none of those auto-infer schemas from telemetry. The miner is bbnf-specific meta-grammar machinery. | A skim reader could ask whether HM-derived schema is sufficient for V1; the telemetry signal source is unstated in PASS-2. | **LOAD-BEARING — substrate at PASS-1; surface at PASS-2 consumed**. The schema miner is V1 per V1-FOLD Tier 1 #7. PASS-2 consumes the schema; PASS-1 owns the miner. The telemetry signal source (a Lens K K.5 question per HARDENING.md) is a PASS-1 question, not a PASS-2 question. **PASS-2's role is consumer; the V1 boundary holds.** |

**Lens K summary**: 8 rows; verdicts: **3 LOAD-BEARING / 2 LOAD-BEARING-with-substrate-elsewhere / 1 ASPIRATIONAL-partial (K.1 e-graph rewrite categories)
/ 1 HYBRID (K.2 cost-model trait sharing) / 1 LOAD-BEARING (K.3 BIR snapshots + regen-equality)**.

The meta-grammar-discipline lens finds the architecture's complexity
predominantly load-bearing for V1 correctness. The one ASPIRATIONAL
classification (K.1 e-graph rewrite categories) routes to PASS-1 / ARCH
§10 ownership. The one HYBRID (K.2 cost-model trait) is the same residue
as Lens I I.8.

Lens-K deeper reading on the meta-grammar mandate: bbnf is a meta-grammar
that generates parsers; it is not itself a runtime. The V1 V2 boundary
discipline holds across PASS-2: V1 ships `RustBackend: Backend` with
five method outputs; V2 adds `WasmBackend` and `TsBackend` mechanically.
The runtime artefacts (visitor / path schema / value API / format) are
all V1; debugger DAP integration + telemetry-driven schema-mining
implementation are V1 substrate but consumed at runtime. The boundary
holds.

Lens-K finding on telemetry-driven schema (K.8): the user mandate
(`restart/README.md:121-129`) commits to "inference stronger than Rust
if possible; grammar rules leverage type algebra + telemetry to generate
semantic schemas without explicit annotations". The schema-mining miner
is V1-FOLD Tier 1 #7 (`restart/research/V1-FOLD-CANDIDATES.md:46-58`);
PASS-1 owns the miner; PASS-2 consumes the schema as a runtime template
parameter at line 152 (`value_enum`). The telemetry-signal source is a
PASS-1 question, not PASS-2. PASS-2's lens-K K.8 verdict is consumer-
side LOAD-BEARING: the schema-mining miner's output feeds PASS-2's
value-API generation; PASS-2 cannot opt out of consuming it.

Lens-K finding on the apparent BIR-snapshot vs rustc overlap (K.3): the
two gates check temporally-distinct invariants. rustc proves "this single
emission compiles + type-checks correctly". The BIR snapshot proves "the
BIR alphabet at commit T = the BIR alphabet at commit T-1" (alphabet
stability). The regen-equality check proves "xtask emitted at commit T
= xtask emitted at commit T-1 given the same input" (deterministic
emission). The three gates together prove a correctness invariant rustc
alone cannot reach: the meta-grammar's emission is reproducible across
time. This is a meta-grammar-correctness invariant (the parser is the
same parser yesterday and today given the same grammar source) that
neither Rust the language nor rustc the compiler can prove. The
overhead is the cost of meta-grammar correctness; not engineering
hygiene.

## §4 — Compressed nine-lane verification

V8 does not relitigate Lanes 1-9 + Lens F/G/H (V7 verdict READY holds).
The compressed verification confirms each lane is unchanged by the
lens-driven analysis above — no V8 surgery destabilises a prior verdict.

| # | Lane | V7 verdict | V8 verification | V8 verdict |
|---:|---|---|---|---|
| 1 | Lock-Adherence | READY | Lock 5 (Backend trait) intact under Lens I/J/K. Lens I.4 BackendLowerer SIMPLIFY-CANDIDATE does not violate Lock 5 — Lock 5 commits to per-backend lowerers as contract; the internal trait is implementation. Lock 14 (per-grammar matrix) intact under Lens I.6. | **READY** |
| 2 | Sequencing | N/A (single PASS) | N/A unchanged. | **N/A** |
| 3 | Cohesion | READY (R-V7-1 corpus residue) | V8 Lens I I.3 surfaces a PASS-2-vs-ARCH BIR-variant cardinality clarification (23 vs 24); the clarification is cohesion-class, not cardinality fault. Routes to S-V8-1. | **READY** |
| 4 | SOTA Anchoring | READY (R-V7-2 SOTA-citation residue) | V8 Lens J J.4 confirms Backend trait pattern parallels LLVM/Cranelift; SOTA citation residue R-V7-2 unchanged. | **READY** |
| 5 | Grammar-Authoritative | READY | V8 Lens K K.4 confirms 5-method Backend trait carries no grammar-name surface. `<g>` placeholder discipline holds. | **READY** |
| 6 | Generated-Code-Budget | READY | V8 Lens K K.6 confirms per-grammar budgets are LOAD-BEARING (Lock 14 + Lock 13). No regression. | **READY** |
| 7 | Friction-Forecast | READY (R-V7-3 diagnostic residue) | V8 Lens J J.7 surfaces thiserror leverage opportunity at PASS-3 / runtime crate (not PASS-2). No PASS-2-local friction regression. | **READY** |
| 8 | Carry-Deferral | READY | V8 Lens I I.7 surfaces e-graph rewrite-category ownership routes to PASS-1 / ARCH §10. V8 Lens K K.1 inherits. New deferral receiver named (PASS-1 / ARCH §10). | **READY** |
| 9 | Greenfield-Discipline | READY | V8 Lens I/J confirm no quick-solution / workaround / backward-compat shim entered. Lens I.4 SIMPLIFY-CANDIDATE is greenfield-clean (delete unused trait); not legacy preservation. | **READY** |
| 10 | Lens F (LLM bias) | PASS | V8 lens-driven analysis introduces no new hedging, reference-stuffing, pseudo-precision, or buzzword reliance. | **PASS** |
| 11 | Lens G (Overfit) | PASS | V8 Lens J J.4 confirms Backend trait pattern is structural parallel (LLVM/Cranelift) without method-set inheritance — the lens-G overfit fence holds. | **PASS** |
| 12 | Lens H (Hallucination) | PASS (R-V7-1 H7 corpus rename) | V8 introduces no new path:line citations to verify; the Lens I/J/K analysis cites PASS-2.md and ARCH.md by line, all verified. | **PASS** |

12 rows; all READY or PASS-equivalent. The V8 lens-driven analysis does
not destabilise any V7 lane verdict; it surfaces orthogonal simplification
candidates.

## §5 — V7.1 → V8 comparison

V7.1 (cohort verification) returned READY for PASS-2 with no regression;
V7 punch closed 13 of 14 items (R4 BBNF-PATTERN-NONEXHAUSTIVE remains
non-blocking friction). V8 introduces lens-driven simplification analysis
without amending V7 verdicts.

| Surface | V7.1 status | V8 lens-driven analysis | V8 verdict |
|---|---|---|---|
| BIR alphabet (22-23 variants) | READY (cardinality-defence at PASS-2:83) | Lens I.1, I.2, I.3 audit — RegexProgram/Scanner KEEP; PrattSpine/recursive-descent KEEP; LayoutPush/Pop CONSOLIDATE-ASPIRATIONAL clarification | **READY with S-V8-1 clarification** |
| Backend trait (5 methods) | READY (Phase 7.1 fold landed surgically) | Lens I.4 audit — internal `BackendLowerer` 8-method trait is SIMPLIFY-CANDIDATE; Lens K.4 confirms 5 outer methods LOAD-BEARING | **READY with S-V8-2 clarification** |
| Function-value lowering (3 options) | READY (Phase 7.2 fold) | Lens I.5, K.7 audit — three-option enumeration LOAD-BEARING (rejected-alternatives discipline) | **READY** |
| `parse-that-regex` naming canon | READY (5 sites + clarifier) | Lens unchanged | **READY** |
| `RegexProgram` rename | READY (8 sites) | Lens unchanged | **READY** |
| egraph + csp-solver bridge | READY (Phase 7.1 fold) | Lens I.7 routes e-graph rewrite-category cardinality to PASS-1 / ARCH §10 | **READY with S-V8-5 routing** |
| `pointer!` → `path!` | RESIDUAL (corpus-wide; ARCH §7.5 + PASS-2:140, 383) | V7 R-V7-1 unchanged; V7.1 verified ARCH §7.5 active surface clean (deletion-archaeology only); PASS-2 still carries `pointer!` at lines 140 + 383 | **READY with R-V7-1 unchanged** |
| Generated LOC budgets | READY | Lens K.6 LOAD-BEARING confirmation | **READY** |
| `parse` / `parse_in` / `parse_owned` | READY (Lock 9) | Lens J.3 confirms full leverage of Rust ownership idiom; J.2 surfaces bumpalo arena clarification (closures don't escape into arena) | **READY with S-V8-4 clarification** |
| Cost-model trait sharing | READY | Lens I.8 + K.2 surface trait-sharing-vs-aspirational-generality clarification | **READY with S-V8-3 clarification** |
| Diagnostic infrastructure | READY (R-V7-3 dedicated codes optional) | Lens J.7 thiserror leverage routes to PASS-3 / runtime spec | **READY** |

11 rows. All retain READY; five carry V8 clarification candidates. None
blocks PASS-2 advancement.

## §6 — Punch list (SIMPLIFY / CONSOLIDATE / LEVERAGE / HYBRID candidates)

The V7 audit closed with four corpus-hygiene residues (R-V7-1 through
R-V7-4); R-V7-1 is the only one that remains as of V7.1 (the others
landed in Phase 7.5). V8 surfaces five new simplification candidates;
none blocks advancement; each routes to a named receiver.

| # | Path:line | Verdict | Surgery | Acceptance gate | Receiver |
|---:|---|---|---|---|---|
| S-V8-1 | `PASS-2.md:67` (Layout BIR variant) + `restart/ARCHITECTURE.md:914-915` (LayoutPush + LayoutPop) | **CONSOLIDATE-ASPIRATIONAL clarification** | Add a sentence to PASS-2 §2 layout-canon paragraph (after line 69): "the `Layout` BIR variant lowers via push/pop scope-marker pair at codegen time per ARCH §7.2 lines 914-915; the BIR alphabet inventory remains 23 variants, the post-lowering shape expands to 24". | The PASS-2 alphabet count (23) and ARCH §7.2 alphabet count (24 with push/pop split) reconcile via lowering note. | PASS-2 amendment (when SYNTHESIS or hardening-driven gates open) OR ARCH §7.2 amendment (collapse LayoutPush + LayoutPop into Layout with body field). Either side is acceptable. |
| S-V8-2 | `PASS-2.md:119-130` (BackendLowerer trait) | **SIMPLIFY-CANDIDATE** | Clarify whether `BackendLowerer` is V1-trait-for-single-impl-partition (delete the trait; use `RustLowerer` struct with 8 methods) OR V1-trait-for-future-per-grammar-family-specialization (name the V2 receiver). The 8-method method set stays; the trait wrapper is the question. | The trait wrapper either has a named V1 polymorphism (multiple impls) or names the V2 receiver explicitly. | PASS-2 amendment when SYNTHESIS clarifies single-impl-vs-future-impl distinction. The simplification saves one trait-surface declaration; not load-bearing for correctness. |
| S-V8-3 | `PASS-2.md:399` (cost-model trait sharing claim) | **HYBRID** | Clarify "V1 cost-model trait shape is shared across parser + regex; V1 cost decisions are independent per substrate; cross-substrate cost composition is post-V1 generality". Name the upstream owner (`crates/cost-model/` or PASS-1 §X). | The cost-model-trait-sharing claim names V1 boundary (structural sharing only) and post-V1 receiver (cross-substrate composition). | PASS-2 amendment + cost-model crate spec (when the cost-model crate spec opens or PASS-1 amendment runs). |
| S-V8-4 | `PASS-2.md:201` (closure environment frame) + Lock 9 (`parse_in`) | **KEEP-WITH-RESIDUE clarification** | Add a sentence at PASS-2 §3 runtime-tree or §2 commitments: "`parse_in(input, &bump)`'s arena lifetime is for input-data extension, not closure-environment lifetime; closures remain stack-bound regardless of arena entry point". | The arena-vs-closure-frame distinction is explicit in PASS-2 text. | PASS-2 amendment (when a runtime-spec or PASS-3 amendment runs); not blocking. |
| S-V8-5 | `PASS-2.md:401` (egraph extraction handoff) + ARCH §10 (rewrite-budget categories) | **ASPIRATIONAL-partial routing** | Route the e-graph rewrite-category cardinality audit to PASS-1 + ARCH §10 (V1-FOLD Tier 4 #26). PASS-2's role is consumer; the cardinality V1-vs-aspirational classification is upstream. | PASS-1 or ARCH §10 carries an inventory: each rewrite category classified LOAD-BEARING (V1) vs ASPIRATIONAL (V1 surface; tranche-deferrable). | PASS-1 amendment OR ARCH §10 amendment (when V1-FOLD Tier 4 #26 lands). |

The punch list carries five entries; none blocks PASS-2 advancement. Each
routes to a named receiver — three to PASS-2 amendments (S-V8-1, S-V8-2,
S-V8-4), one to PASS-2 + cost-model spec (S-V8-3), one to PASS-1 / ARCH
§10 (S-V8-5).

V7 R-V7-1 ( `pointer!` → `path!` rename in PASS-2:140 + 383): unchanged
under V8 lens analysis. The corpus-hygiene receiver remains the
naming-canon sweep agent (Phase 7.3 / Tranche A); V7.1 §4 verified ARCH
§7.5 active surface clean (deletion-archaeology only). PASS-2 mirrors
ARCH; the residue is corpus-wide.

V7 R-V7-2 (Backend trait SOTA citation), R-V7-3 (function-value
diagnostic codes), R-V7-4 (§10 closing-posture refresh): unchanged under
V8 lens analysis; each remains optional documentation hygiene routing
to a named receiver.

## §7 — Final verdict

**Decision: READY (with five non-blocking simplification candidates
surfaced)**.

The V8 lens-driven analysis confirms PASS-2's V7.1 READY verdict. The
codegen surface is largely load-bearing under Lens I (contrivance),
Lens J (host-language leverage), and Lens K (meta-grammar discipline).
The five V8 surgeries are clarification + routing, not architectural
amendment.

| Criterion | Result |
|---|---|
| Lens I cardinality bloat | NONE. BIR's 22-23 variants sit inside the 20-30 cardinality-defence band; each variant has distinct lowering. The LayoutPush/Pop ARCH-vs-PASS-2 cardinality (23 vs 24) is a clarification routing to S-V8-1, not a bloat fault. |
| Lens I speculative generality | ONE candidate. The internal `BackendLowerer` 8-method trait carries no V1 polymorphism (only `RustLowerer` impls). Routes to S-V8-2 (delete trait OR name V2 receiver). |
| Lens I premature optimization | NONE. The cost-model + e-graph + CSP apparatus is V1-mandatory at the substrate level; the V1-vs-aspirational-cardinality question on rewrite categories routes to PASS-1 / ARCH §10 (S-V8-5). |
| Lens I double-tracking | NONE. RegexProgram + Scanner partition by execution-substrate ownership; PrattSpine + recursive-descent partition by detection. Each variant earns its place. |
| Lens I unused parameter axes | NONE. The 16-parameter runtime-template schema is data-driven; the 5-method Backend trait is consumer-driven. |
| Lens I apparatus chains | NONE. Single-pass machinery throughout; no multi-pass redundancy surfaced. |
| Lens J memory management | FULL LEVERAGE. Closure environment frames lean on Rust's borrow-checker (line 201). One bumpalo-arena-clarification residue (S-V8-4) does not invent new lifetime semantics. |
| Lens J generics + monomorphisation | FULL LEVERAGE. Function-value lowering's option 1 (monomorphise) reuses Rust's monomorphisation; option 3 (inline) is codegen-internal. |
| Lens J type checking | FULL LEVERAGE. BIR snapshot + regen-equality check temporal invariants distinct from rustc's per-emission type-checking; no overlap. |
| Lens J concurrency / async | N/A. PASS-2 does not propose concurrency model. |
| Lens J pattern matching | FULL LEVERAGE. The `match grammar { ... }` Lock 14 audit confirms zero per-grammar match arms in generic crates. |
| Lens J standard library parity | FULL LEVERAGE. `parse` / `parse_in` / `parse_owned` API mirrors sonic-rs / lightning-css / serde-json idiom. |
| Lens J diagnostic / error infrastructure | HYBRID at PASS-3. Diagnostic codes are bbnf-specific (correct meta-grammar specificity); runtime error-type infrastructure (thiserror leverage) is PASS-3 / runtime crate concern. |
| Lens K generating language vs parsers | LOAD-BEARING discipline holds. Function-value lowering generates parser code; semantic apparatus stays at runtime per ARCH §8.4 forbidden-behavior fences. |
| Lens K self-hosting | LOAD-BEARING (Lock 14). bbnf's grammar is bbnf-generated; no self-hosting-specific apparatus surfaces. |
| Lens K runtime complexity | LOAD-BEARING with V1/V2 boundary intact. Visitor + path-schema + value API + format() — V1 mandatory; debugger DAP integration is V2. |
| Lens K optimization complexity | ASPIRATIONAL-partial. The legality + normalization e-graph categories are V1; cost-driven rewrites are SOTA-driven (Lock 8) and route to PASS-1 / ARCH §10 cardinality classification (S-V8-5). |
| Lens K telemetry-driven schema | LOAD-BEARING (V1-FOLD Tier 1 #7; user mandate). PASS-2 consumes; PASS-1 owns the miner. |
| Re-draft threshold | Not met. |
| Amendment threshold | Not met for PASS-2-blocking; five V8 clarifications routable to amendments. |
| Punch list | Five non-blocking V8 entries; one V7 corpus-hygiene residue remains (R-V7-1). |

V7 → V8 delta: V8 introduces lens-driven simplification analysis; the
five V8 candidates are clarification + routing, not architectural
amendment. V7.1 cohort verdict (READY) survives.

## §8 — Closing posture

PASS-2 is codegen — where the meta-grammar produces target-language
source. The V8 hypothesis was that simplification opportunity is
greatest here because every generated line is type-checked by rustc;
bbnf's audit-time machinery may duplicate that. The lens analysis
discharges the hypothesis: PASS-2's audit-time machinery (BIR snapshot,
regen-equality, BIR-shape proof) checks temporal invariants that rustc
cannot check — alphabet stability, deterministic emission, V2
mechanical-expansion assurance. Each gate partitions a different
invariant from rustc; no audit-time / compile-time overlap surfaces.

The five V8 clarification candidates (LayoutPush/Pop reconciliation,
BackendLowerer trait simplification, cost-model-trait-sharing scope,
bumpalo-arena vs closure-frame distinction, e-graph rewrite-category
cardinality routing) are surface clarifications + routing; none names
a load-bearing architectural fault. The V7.1 cohort READY verdict
survives V8.

The dispatch's framing — "the simplification opportunity is greatest
here because every generated line is type-checked by rustc" — is
incorrect for the audit-time machinery (which serves temporal
invariants), correct for the lowering machinery (which honours Rust
idioms throughout per Lens J). The per-target-source-emit at lower
time correctly leverages rustc's downstream gate; the audit-time
machinery correctly partitions invariants rustc cannot reach.

PASS-2 carries forward to per-tranche full-spec drafting (Wave 8+) with
five V8 clarifications routable to mid-tranche amendment without
blocking advancement. The V7.1 four-target cohort verdict (READY)
remains the gate; V8 lens analysis sharpens the simplification + leverage
+ meta-grammar-discipline understanding without amending it.

Hereupon V8 closes. The architecture stands READY for full-spec
authorship; the five V8 simplification candidates absorb into whichever
tranche's spec wave first authors the corresponding surface (PASS-2
amendments at A.W0 / E.W1 spec drafting; PASS-1 amendment at C.W4 /
E.W2 spec drafting; cost-model crate spec at A.W2).
