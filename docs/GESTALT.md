# GESTALT — bbnf-lang, Universal Synthesis

> **Refreshed at master `40092b28` (post-AZ-IV close, post-DEEP-SYNTHESIS canonical ordering).** This file is the project synthesis: what bbnf-lang is, how it composes the SOTA literature into a grammar-derived parser fleet, what the value-API generalization is, and what the cross-repo shape looks like. Plan items, runway sequencing, decision records, and tranche schedules live in the per-tranche docs that own them (`docs/tranches/<LETTER>/<LETTER>.md`); they no longer live here. This is canon for project direction in totality.
>
> **Canonical post-AZ-IV ordering (per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`): AZ → BA (direct-projection codegen) → BB (rule-discovery) → BC (cleanup) → BD+ (TS/WASM re-engineering or shared-ABI; future).** The earlier "recycled BA / subsumed BB / closed-orchestration BC" lettering was reconciled at the DEEP-SYNTHESIS commit; archives are at `docs/tranches/{BA,BB}/historical/` and `docs/tranches/BC/orchestration-archive-2026-04-30/`. The fictional "AZ-V" is removed from all close-state docs.

## 1. Abstract

bbnf-lang is a grammar-derived compiler fleet. A BBNF grammar, typed by `->` annotations on rules, lowers through an IR-pass substrate (`crates/ir`) into backend emitters that project directly into grammar-derived structs and typed value APIs. The IR is optimised by a pluggable CSP solver (`crates/csp-solver`) and a pluggable e-graph (`crates/egraph`), both grammar-agnostic; grammar semantics flow in through `IrNode` plus persisted projection facts.

The historical tape runtime is gone: `crates/tape/` was deleted at AZ-II.cutover.O5 + AZ-III.W1; `Parsed<R>` and `TapeDirect` deleted at AZ-II.cutover.O4; generated view helpers deleted at AZ-II.cutover.O3. AZ-III.W4 structural audits confirm static no-legacy GREEN. A parse-that substrate (`../parse-that`) carries the parser-combinator surface and bespoke regex HIR/NFA/DFA engine. A pprint substrate (`../pprint`) carries the gorgeous auto-formatter.

AZ-IV closed `complete_with_misses` at master `cb14970f` (2026-05-02; see `docs/tranches/AZ-IV/FINAL.md`). The union tranche absorbed the AZ-III carry burn-down, every overfit-elimination and substrate-activation item the third hardening pass surfaced, the typed compile-time `path!` macro and lazy bail-out parse, the per-grammar value-API consolidation, the TS template-literal-tag binding, and a complete failing-test redress. Post-close audits (DEEP-A through DEEP-D, synthesised at `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`) named the chronic perf gap's mechanism precisely: 86.07% of inclusive samples on `bbnf_value_twitter` are `Vec<OpenFrame>::clone` from `JsonStructBuilder::checkpoint`, the speculative-branch deep-clone discipline that was needed because compile-time-resolved direct projection isn't emitted. The post-AZ-IV residual is sequenced under the canonical AZ → BA → BB → BC ordering: BA opens with **direct-projection codegen** (closes the perf carries through mechanism), BB opens after BA with **rule-discovery** (un-subsumed; the originally-planned BB scope returns to its canonical letter), BC opens after BB with **cleanup pass** (the precepts orchestration tranche is archived unmodified at `docs/tranches/BC/orchestration-archive-2026-04-30/`).

The synthesis thesis: bbnf is a *compositional* SOTA of pieces from simdjson, sonic-rs, lightningcss, Ruler, egg, parse-that, and yyjson — where composition is mediated by grammar-derived semantics. Every technique is applied at grammar abstraction level; per-grammar hand-tuning is rejected at plan time.

## 2. What bbnf-lang is, from first principles

A BBNF grammar describes both a recogniser and the type of its accepted value. Every rule carries an optional `-> T` annotation. Type inference composes these — `feedback_typed-materialization-invariant`: *every `->` in the grammar must reach the emitter; inference composes types, never loses them; parity = full typed-AST equivalence*. The grammar is the single source of truth. Hand-written `bbnf::json::Value` or `bbnf::css::StyleSheet` containers do not exist at HEAD; the AX.W1.A/B experiments that produced them landed and reverted at −6,128 LOC under `feedback_grammar-authoritative-status`.

The phrase "grammar-derived" does real work. It is not decoration. A typical parser-generator treats the grammar as input to code generation and the generated code as the authoritative artefact; bbnf-lang treats the grammar as the authoritative artefact and the generated code as its projection. When the CSS L4 grammar declares `length -> Length`, the emitter has no latitude to project `length` as anything but a typed `Length`; if the lightningcss `Length` and the bbnf-derived `Length` disagree in shape, the grammar is edited to match, not the emitter. When a payload cannot be derived from `->`, the grammar is extended (e.g., hybrid-grammar-host's Phase 3 adds host-function annotations for context-dependent semantics); the emitter does not compensate for missing grammar information. The discipline is what makes `feedback_no-backward-compat` viable: all dev products migrate fully, because the grammar mediates change and the emitter is a pure projection.

### Four interlocking invariants

**Typed materialisation.** Every `->` in every grammar reaches `push_leaf_with_*`, `begin_compound`, or `end_compound`, and an IR audit pass enforces 100 % coverage and fails the build otherwise. The emitter never re-derives what the grammar already declares. The `project_types` IR pass writes into a `StructRegistry`; the emitter reads the registry; there is no third party that opines on shape.

**No orthogonal codepaths.** Arena allocation is a singular collection strategy; no conditional Vec-vs-scratch branching; no combinator fallback alongside the monolithic codegen; one regex system (HIR); KISS DRY. The Tranche AQ.5 deletion of the `EmissionTier` lattice (`MustTape`/`MustFn`/`MayInline`) and structural dispatch was the concretisation: two orthogonal decision surfaces collapsed into a single `PayloadKind → TypeDesc` projection in one commit.

**Direct-to-struct.** Generalize regex-to-value conversion; no hard-coded pattern lists; every `->` in the grammar projects directly to a typed record at emission time, without an intermediate untyped phase. `project_types` is the single projection pass; there is no parallel shape-derivation pipeline.

**Grammar-authoritative.** The grammar owns leaf semantics through `->`; host functions cover context-dependent and recursive computations that the grammar cannot express. Hybrid-grammar-host is the current migration posture (Phase 1+2 done, Phase 3 host-fns pending). Backends see `TypeDesc::Named` as abstract names; each backend resolves to native types via its own registry. The CSP and e-graph do not know which language they target; the emitter does.

The four invariants interlock. Typed materialisation requires direct-to-struct, because anything else re-derives shape after inference has already composed it. Direct-to-struct requires `no-orthogonal-codepaths`, because a second projection surface would inevitably drift from the first. `no-orthogonal-codepaths` requires grammar-authoritative, because only a single source of truth can be canonical. Grammar-authoritative requires typed materialisation, because without `->` reaching the emitter the grammar's authority ends at the parse boundary and the runtime re-asserts its own types. The cycle is the core of the architecture.

### Two parse modes, one parser

After AZ-IV.W3 closes, every generated parser has two parse modes that share generated code:

1. **Eager** — `parse(input) -> Result<Document, ParseErr>` materializes the full document tree. All parse errors surface.
2. **Lazy bail-out** — `parse_with(input, &path) -> Option<T>` consumes a `TypedPath<G, T>` and drives the recognizer to satisfy the path while skipping subtrees the path does not visit. Lazy mode silently elides parse errors past the path's reach (documented contract).

The two modes share the same generated parse functions; the entry-point dispatch is the only divergence. The recognizer plan (per-rule `(rule, segment_kind) -> {ParseFully, ParseUntil(child_index), Skip}` decision table) is a static array per grammar emitted at codegen from the IR's `path_check` pass output. The plan is grammar-general; no rule-name match arms in the emitter.

## 3. The SOTA union — grammar-derived everything

The fleet is a compositional SOTA of parser literature pieces, mediated by grammar-derived semantics. Each piece is taken for a specific capability; each composes with bbnf's IR / CSP / e-graph substrate through a grammar-side hook rather than a side-channel runtime API.

**simdjson's tape, then the post-tape inversion.** 16-byte fixed records, compound open/close record pairing, and opaque string scratch storage were the Era IV stepping stone. bbnf adopted the shape for every grammar, not only JSON, to make typed payload materialisation measurable. AZ-I/AZ-II inverted the lesson: simdjson's tape was a proof of shape regularity, not the final surface. After `StructRegistry` closed, the same facts wrote directly into grammar-derived structs and `crates/tape/` deleted.

**simdjson OnDemand's lazy iteration.** The path-driven lazy parse mode (AZ-IV.W3) applies the same skip discipline simdjson uses for input bytes — but applied at the grammar abstraction. The path acts as a schema; the recognizer drives forward, skipping subtrees the path does not visit. Same parser; same generated code; mode-driven entry-point dispatch.

**sonic-rs's StructRegistry + `pointer!`.** Type-safe field access through compile-time registration. bbnf adopts the registry as `project_types` output — but populates the registry from grammar `->` annotations, not from user `#[derive(StaticType)]` macros on host-language structs. `pointer!` ergonomics become `path!` (AZ-IV.W2) with **stronger compile-time validation**: invalid paths fail to compile with grammar-aware diagnostics; sonic-rs fails at runtime on bad paths.

**lightningcss's typed-value parity.** Parse `<length>` into a typed `Length`, not a string; parse `<color>` into a typed `Color`; every CSS L4 property rule returns the typed shape that lightningcss produces from its hand-written Rust implementation. bbnf derives the same shapes from the CSS L4 grammar — node-for-node parity gated at AZ-I.W3, then refreshed at AZ-IV.W1 from regenerated tempdir output.

**Ruler's CVC rule enumeration.** Given a term algebra and an equivalence oracle, enumerate terms up to a size bound, group by equivalence, extract cross-class equivalences as rewrite rules. bbnf uses `IrNode` as the algebra, the VM as the oracle on residue, and the existing CSP cost model as the scheduler. **Tranche BB's thesis** (post-BA, per the canonical post-AZ-IV ordering; previously stated as "recycled BA's thesis" before DEEP-SYNTHESIS reconciled the lettering).

**egg's e-graph substrate.** `crates/egraph` is the workspace member; `crates/egraph-derive` derives the `Language` impl from existing `IrNode` enum variants; `crates/bbnf-regex` (a sub-crate of parse-that, eventually) uses the same optimisation architecture internally. The cost model is CSP-modelled; the rewrite rules (factor, merge_regex_alts, inline_acyclic) were hand-coded in Tranche H and become e-graph-inferred in BB (post-BA, per canonical ordering).

**parse-that's combinator substrate.** The runtime parser surface. A modern recursive-descent combinator layer with bespoke HIR for the regex engine — explicit negated flag, hand-written parser, no dependency on `regex-syntax`. The `regex` crate does not appear in the emission path; `bbnf-regex` replaces it through `[patch.crates-io]`. The path-lexer for the `path!` macro lives at `crates/bbnf-regex/src/path_lexer.rs` (custom HIR API per AZ-IV.W2).

**yyjson's dispatch-and-allocation frontier.** yyjson observes that SIMD is not where the next 10 % lives past a certain point; key dispatch and in-place payload placement are. bbnf already ships AP.4 key dispatch (the Tranche AP structural-dispatch substrate that survived AQ.5's rescope as a `PayloadKind → TypeDesc` projection) and AP.5 NibbleLut. AZ-I/AZ-II's direct-to-struct activation is the in-place-payload piece; the grammar-derived StructRegistry tells the emitter exactly which field receives each scalar payload, so the emitter writes in-place without a two-stage "materialize-then-project" pass. yyjson reading applied at grammar abstraction level.

### The synthesis principle

bbnf is *compositional* SOTA, where composition is mediated by grammar-derived semantics. simdjson's tape shape, sonic-rs's type registry, lightningcss's typed values, Ruler's rule enumeration, egg's e-graph, parse-that's combinators, yyjson's in-place payload — each contributes a specific capability; each is wired into bbnf through the grammar's `->` annotations, not through a per-feature side channel. The IR is what makes the composition coherent.

The grammar-derived mediation is what makes the composition defensible at scale. A JSON-only speed-up that ships a JSON-specific codepath is rejected at plan time (`feedback_preserve-rich-ast`: never flatten typed grammar rules for speed; rich AST parity with lightningcss is non-negotiable). A per-grammar parser hand-tuned to beat a specific fixture is rejected at plan time. Every technique bbnf adopts from the literature is applied at grammar abstraction level. When AZ-I/AZ-II activate scalar payload directly to struct, they do so for *every* grammar with a scalar `->` annotation, not for JSON's `value -> f64` alone. When AZ-IV's `path!` macro compiles, it works for *any* grammar, not for JSON's well-known structure alone. When BA emits direct-projection codegen, it emits for *every* grammar's typed records — annotated or `->`-less — uniformly through the same `StructRegistry` consumer. When BB infers rewrite rules, it infers over `IrNode` — the grammar-agnostic IR — producing rules that apply to any grammar by construction. *The grammar is the only distinguishing input, and everything downstream is uniform across grammars*.

## 4. Generalization and Value-API Vision

The value API is grammar-derived end-to-end. After AZ-IV closes, every production grammar has:

1. **A typed value enum** (`JsonValue`, `CssTypedValue`, `SheetsValue`, `BbnfValue`, etc.) — sum of every alternation branch in the grammar's value rules, with payloads typed per the rule's `->` annotation. Semantic richness is preserved (the typed enums are NOT touched by the per-grammar arena/builder dedup at AZ-IV.W5).
2. **A typed document** (`JsonDocument`, `CssDocument`, etc.) holding an arena, a root, and a borrowed input slice; arena handles resolve through `Document::array(id)` / `Document::object(id)` or through the `*View<'a, 'p>` ergonomic surface.
3. **A typed compile-time path** (`TypedPath<G, T>`) — the grammar marker `G` and terminal type `T` are extracted by the `path!` macro from the `StructRegistry` populated by `project_types`. Invalid paths fail to compile with grammar-aware diagnostics naming the segment, the resolved struct type, and valid alternatives.
4. **A wildcard step** that returns lazy `Iter<Item = T>` (sonic-rs / simdjson idiom). `.with_anchors()` adapter yields `Iter<Item = (Path<'_>, T)>`. `.collect()` materializes when callers want.
5. **A variant-selection step** for typed-enum sums. `path!(CssL4, "rules", 0, "declarations", 0, "value", "color")` returns `Option<&CssColor>` from `CssTypedValue::Color(_)` — grammar-derived from the `->` annotation on the `color` rule.
6. **Two parse modes** — eager (full materialization) and lazy (path-driven bail-out, ≤ 5x sonic-rs `get_*` same-harness) — sharing generated parse functions. Per `feedback_no-orthogonal-codepaths`: one parser, two entry points.
7. **Per-grammar arena/builder pairs** generated from one shared template parameterised by `StructRegistry` + `TypeDesc` (AZ-IV.W5). Each per-grammar `arena.rs` is ≤ 30 LOC of instantiation; each `builder.rs` is ≤ 50 LOC. The structural skeleton is dedup; the typed leaves are preserved untouched.

### What this generalization replaces

- **`from_rule_name(&str) -> Kind` impls** (one per non-JSON grammar) — eliminated at AZ-IV.W1; replaced by `StructRegistry::compound_kind_for_layout(layout) -> CompoundKindId` (registry-projected discriminator).
- **`(layout.kind, layout.rule_name.as_str())` builder dispatches** — eliminated at AZ-IV.W1; replaced by `OpenFrame::from_layout(layout, &registry)` (typed selection).
- **`leak_static_str` rule-name allowlist** — deleted at AZ-IV.W1; rule names that need static lifetimes go through one canonical interner.
- **`EmitStrategy::for_grammar` 9-arm allowlist** — eliminated at AZ-IV.W1; replaced by manifest-driven binding registry. A synthetic grammar registered only via `[package.metadata.bbnf-grammars.<ident>]` round-trips codegen without adding a Rust arm.
- **`substrate_path` JSON-builder fallback** — replaced at AZ-IV.W1 with `panic!` on invalid binding string.
- **Hand-coded normalizers, rule-name projection tables, synthetic payload defaults** — none can close a parity gate; semantic parity is type-inference driven.
- **2953x sonic-rs lazy gap** — closed at AZ-IV.W3 to ≤ 5x same-harness on `bbnf_get_twitter` (target ≤ 1.0x routes only with profile evidence per AZ-IV §Hard Gates 16).
- **`backend/rust/view/color` shim** (290 LOC zero production consumers) — deleted at AZ-IV.W1; CSS uses `runtime::css_l4::CssColor`; legacy decoder migrates to test-support only.
- **`recover_modifier`/`recover_binary_op`/`wrap.rs:89-99` BBNF source-byte scanners** — deleted at AZ-IV.W1; replaced by alt_dispatch typed-leaf push activation.

The generalization principle is simple: **the grammar is the only distinguishing input; the value API is uniform across grammars.** Sonic-rs gives JSON-only ergonomics. lightningcss gives CSS-only ergonomics. simdjson gives JSON-only speed. bbnf gives **all of the above for any grammar** — derived from the grammar's `->` annotations, not from per-grammar hand-tuning.

## 5. The fleet — cross-repo shape

The fleet spans three sibling repos and one workspace.

**bbnf-lang** owns the IR, the workspace-internal crates (`crates/core`, `crates/ir`, `crates/analysis`, `crates/lsp`, `crates/ser`, `crates/gorgeous`, `crates/bootstrap`, `crates/egraph`, `crates/egraph-derive`, `crates/csp-solver`, `crates/simd-scan`), the grammars, the benchmark surface, and the CLI. After AZ-IV adds: `crates/bbnf-path` (proc-macro for `path!`), `crates/bbnf-path-ts` (cdylib + wasm-bindgen).

**parse-that** (`/Users/mkbabb/Programming/parse-that`) owns the combinator substrate (`parse_that`), the bespoke regex engine (`bbnf-regex`), and `regex-bootstrap`; path-patched into the bbnf-lang workspace through `.cargo/config.toml`. `bbnf-regex` exposes a custom path-lexer HIR API at `crates/bbnf-regex/src/path_lexer.rs` after AZ-IV.W2 (≤ 200 LOC; no `regex-syntax` dependency).

**pprint** (`/Users/mkbabb/Programming/pprint`) owns the auto-formatter runtime consumed by `crates/gorgeous`. The sibling `gorgeous` repo retired during the appurtenant assay (`~/.Trash/gorgeous-retired-2026-04-23`); the workspace `crates/gorgeous` is the only canonical gorgeous.

**csp-solver** (`/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver`) is the algorithm-evolution authoritative source for the general CSP solver. The workspace member `crates/csp-solver` is bench-authoritative; AZ-IV.W0 enforces canonical-source policy (diff-clean between the two trees on shared files; `[patch.crates-io]` in bbnf-lang's `.cargo/config.toml` pins the sibling commit).

The choice to host general-infra crates outside bbnf-lang is `feedback_general-infra-crates`: general-purpose constructs (e-graphs, cost models) in their own crate(s), not stuffed into domain crates. The e-graph is in `crates/egraph`; the CSP is split bbnf-lang ↔ csc411; the regex engine is in `../parse-that/`. Each of these has its own optimisation architecture internally, so the bespoke regex crate benefits from the same egraph-based rewriting that bbnf-lang uses at grammar level. WASM bindings for general-infra crates live as sub-crates inside the parent (`feedback_wasm-subcrate-pattern`): a cargo workspace member, a cdylib, a path-dep.

### Future-work cross-repo motion (out of AZ-IV scope)

Future tranches will move bench/optimization sub-crates into their own repositories or relocate them. Recorded for plan continuity:

- `crates/csp-solver` → its own repo (canonical-source policy already declared between bbnf-lang and csc411 sibling).
- `crates/egraph` → its own repo (general-purpose infra crate per `feedback_general-infra-crates`).
- `crates/simd-scan` → its own repo or into parse-that.
- `xtask` → relocated within `crates/` repo as `crates/xtask`.
- `bbnf-regex` → sub-crate of parse-that (regex source-of-truth lives in one place).

These are out of AZ-IV scope. The path-lexer API exposed from `bbnf-regex` (AZ-IV.W2 D1) is designed to survive a future relocation cleanly.

## 6. The measurement discipline

Measurement gates substrate. AX invariant 13 codifies it: a ledger-only wave — imports green, tests compile, substrate in place but no runtime consumer exercising it — is a re-plan trigger, not a close. Era V violated this five waves running; every post-Era V tranche plan cites invariant 13 explicitly. The third hardening pass (Babbage) added the permanent `substrate_audit.rs` test landing at AZ-IV.W5 — the substrate-with-consumer rule becomes machine-checkable: the test enumerates every `pub` substrate at compile time and fails the build on zero-caller substrate.

### Bench schema and matrix authority

`docs/benchmarks/SPEC.md` is the canonical schema for bench matrices. Per-tranche close matrices live at `docs/benchmarks/post-{TAG}.json`. Wave-evidence files live at `docs/tranches/{LETTER}/audit/`. Pre-existing evidence lives in `docs/benchmarks/archive/`. `docs/benchmarks/iai-baselines/` carries iai-callgrind canonical baselines. `docs/benchmarks/profiles/` carries samply / instruments / perf artefacts.

Every bench matrix carries: `tag`, `tranche`, `kind` (tranche-close | wave-mid | wave-close | prototype | spot), `commit`, `arch`, `profile`, `profile_definition`, `description`, `bench_matrix_note`, `harness_carves`, `fixtures`, `benches`, `competitors`, `floors`. Tranche-close matrices REQUIRE a `floors` block comparing row-by-row against at least one prior `post-{X}.json`. Cross-profile comparison requires explicit conversion notes.

Status vocabulary per row: `MEASURED` (canonical), `WATCHDOG_HALT` (per-iter wall-clock guard halted measurement), `NAMED-BLOCKER` (carved out with routed cause). A close that lands with any `NAMED-BLOCKER` row in a non-routable carry context is invalid.

### Profiling discipline

`docs/instructions/PROFILING.md` is the canonical profiling contract. Samply is the canonical runtime profiler. Divan is the canonical bench harness. iai-callgrind gates instruction-count regressions on Linux CI. All runtime perf claims cite a samply or divan artefact; CI-side claims cite iai-callgrind output.

The seven required artefacts per profiled entry: `bench.txt`, `build.txt`, `record.txt`, `load.txt`, `profile.json.gz`, `profile.json.syms.json`, `syms-proof.txt`. Every entry under `docs/benchmarks/profiles/post-{TAG}/<harness>/<entry>/` carries all seven.

Multi-agent profiling waves run one sub-agent per bench harness (5 canonical harnesses today). The orchestrator runs `scripts/prepare-profile-wave.sh` once; sub-agents consume `wave.tsv` rows verbatim; sub-agents do NOT rerun `cargo expand` or `cargo bench` after prepare finishes; sub-agents write retained artefacts only under `.profiles/`.

Samply invocation rules: `--unstable-presymbolicate` always; `--save-only` never; ports preflighted; bench cwd `crates/core`; `--bench <filter>` substring caveat (avoid name prefixes).

Every claim in a profiling sub-agent's report cites a saved file. Inference-only conclusions are forbidden.

### Cold-only benchmarks; sequential runs

`feedback_no-warm-benches`: every measurement is cold per-parse; warm/cached benchmarks are disingenuous. `feedback_bench-sequential-regression`: benchmarks run sequentially, never interleaved.

## 7. The instruction-layer discipline

The orchestrator and sub-agents share a rule set carved from real incidents. These are not decorative; they are the protocol that made the meta-audit archaeology possible and the parallel waves tractable. The canonical edicts live in `docs/precepts/instructions/{README,ORCHESTRATION,LESSONS-LEARNED,tranche/SPEC,tranche/WAVE_SPEC,tranche/AGENT_DISPATCH_TEMPLATE}.md`. A summary of the load-bearing items:

**Hard-cap on every dispatch.** `HARD CAP: N min. At 0.9N commit, at N halt`. Defaults: research 20, plan 15, redress 30, audit 25. HARD CAP expands on overrun (no pre-allocated triumvirate budget; expand cap and record reason in PROGRESS.md).

**Triumvirate auto-trigger.** When a JSONL has been quiet for >15 min, a first-pass sub-agent returns without a commit, three diagnostic-loop iterations fail to isolate root cause, or scope reveal invalidates file bounds / hard gates / substrate-with-consumer wiring — the orchestrator dispatches a three-agent triumvirate (research / plan / redress) without prompting.

**Worktree isolation.** Every parallel agent unit runs in a sibling worktree with its own `CARGO_TARGET_DIR`. The wave spec lists the worktree plan and asserts per-unit modify-path disjointness before dispatch. `git worktree list` runs before dispatch; waves whose units overlap on `modify` paths are rejected.

**Empty-return redispatch.** An empty sub-agent return is a failed dispatch. Redispatch verbatim once with the prior worktree pointer; a second empty return triggers mandatory triumvirate.

**Six-agent ceiling.** Hard ceiling is six agents in a wave. Waves that appear to need more decompose into sequential mini-waves; agent count never substitutes for plan quality.

**Cherry-pick preserves wave provenance.** When agents commit in sibling worktrees, the orchestrator's integration step is `git cherry-pick` of named commits, not `git merge`. Linear history per wave is non-negotiable.

**Single cargo per CARGO_TARGET_DIR.** At most one cargo invocation per `CARGO_TARGET_DIR`. Sibling worktrees set per-agent `CARGO_TARGET_DIR=<worktree>/target/<agent>`.

**Iter-profile always.** Every iteration-loop `cargo check` / `cargo test` carries `--profile ax-iter` explicitly; bare forms are heavy-surface.

**Read-size preflight.** `wc -l` before Read on any file > 2K lines; grep + offset for generated.rs, transcripts, large audits.

**Generated-size budget.** Generated code has a per-tranche line-count budget; overflow blocks the wave until the O(N) generator regression is traced.

**Bodyless-large-commits prohibition.** Implementation commits use concrete mechanism or surface scopes; broad, generated, deletion, benchmark, profiling, gate, and status commits carry bodies with why, what landed, evidence, and any routed remainder. Templated bodies are rejected.

**Close-honesty checklist.** Before declaring close, the orchestrator runs the checklist — every claim grounded in PROGRESS.md or a cited artefact, every gate marked MET with a resolving evidence path, every status word matching the latest gate run, every cross-tranche debt entry naming a destination. Mismatches reconcile before close, not after.

**Hardening pass.** After redress lands, the orchestrator may dispatch hardening agents in parallel to verify carry-over completeness, reduce friction, and absorb late-arriving findings. Hardening is read-mostly; it produces audit documents and surgical patches, not large new features. Hardening is distinct from triumvirate (which fires on stalls).

**No ledger-only close.** A wave is not closed until status docs reflect what actually landed, missed, or changed. Implementation can advance while the execution record drifts; the docs-update task at every wave close prevents that.

**Substrate with consumer.** Every substrate change must land with a same-wave consumer or an explicitly declared brittleness window and restoration wave. The permanent `substrate_audit.rs` test (AZ-IV.W5) makes this machine-checkable: zero-caller `pub` substrate fails the build.

**No grammar overfitting.** Production runtime/builder/dispatch paths derive discriminants and selection from `StructRegistry`, `TypeDesc`, `FactAuthority`, manifest metadata, or generated projection tables. Literal grammar parser-struct idents (`JsonParser`, `BbnfParser`, etc.) appear only at registry-binding entry points; literal rule-name match arms appear nowhere outside `#[cfg(test)]`. A static AST scan (`crates/core/tests/no_grammar_name_branch.rs`) enforces this.

**No silent fallback.** No production code path swallows a malformed substrate path, missing rule, unrecognised parser ident, or unknown grammar by routing into a default builder, default discriminant, or per-rule allowlist. Failure is a `panic!` with a named binding string at construction time.

**Failing-test discipline.** Workspace nextest is 100 % pass. Every `#[ignore]` carries an owner + deadline-commit + reason + ticket. Tests deleted in a wave carry per-test commit-body justification (test name, file:line, why, replacement).

**Non-routable carries.** The chronic-deferral pattern ends at AZ-IV: items that have been deferred ≥ 3 tranches are non-routable in AZ-IV. AZ-IV cannot close by routing them to a successor letter. A non-routable item that cannot land inside AZ-IV without changing the AZ-IV thesis triggers a triumvirate scope-reveal review of the thesis itself, not a new tranche letter.
