# AZ-IV AUDIT-F — Generalized-Grammar-Optimum Architectural Critique

**Date**: 2026-05-02
**Auditor**: AUDIT-F (read-only architectural lane)
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-audit-f-optimum`
**Read-first**: `STYLE.md`, `GESTALT.md` §2 + §4, `AZ-IV.md` §Thesis + §Invariants + §Deletion Bias, `HARDENING-2026-05-01-fermat.md`, `HARDENING-2026-05-01-babbage.md`, `AUDIT-2026-05-02-mid-tranche.md`. Spot-read: `crates/ir/src/registry/strategy.rs`, `crates/ir/src/egraph/cost.rs`, `crates/core/src/pipeline/compile.rs`, `crates/core/src/path/mod.rs`, `crates/bbnf-path/src/path_macro.rs`, `crates/core/src/backend/driver/**`.

The mandate: read the system's posture toward the synthesis principle in GESTALT §3, *the grammar is the only distinguishing input, and everything downstream is uniform across grammars*, and name where the architecture lives up to that principle, where it falls short, what transpositions would close the gap, and which of those transpositions are worth doing inside AZ-IV's remaining wave budget.

The thesis holds. None of the recommendations below would invalidate it. The architecture has the right bones. What's missing is mostly the elision of decision surfaces that no longer earn their keep, plus a small handful of de-duplications that the post-W1 carry-burn has merely pushed forward.

## §1 Where bbnf Generalizes Cleanly Today

The four interlocking invariants are honoured at the following surfaces. This is the ledger of *what works* — the concrete substrate that justifies the synthesis claim.

**Typed materialisation reaches the emitter end-to-end through `project_types` → `StructRegistry`**. `crates/ir/src/passes/types/obligation.rs` and `crates/ir/src/registry/struct.rs:33-100` form the authoritative shape projection: every `->` annotation flows through `TypeDesc` into a `StructLayout`, and the `LayoutKind` discriminator is data, not behaviour. Consumers query `StructLayout::is_tagged_enum` / `is_newtype` / `field_count` rather than re-pattern-matching on `LayoutKind` itself. The CSS L4 named_color path is the canonical witness — `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:227-298` walks `IrNode::Map { fn_id }` chains, inspects `FnDescriptor::Expr`, and constant-folds `MapExpr::IntLit` into `push_leaf_with_u64`, all without a single rule-name compare.

**Direct-to-struct is real for typed leaves**. `runtime::view` carries `CssColor`, payload bytes, and the typed-leaf reader surface; the emitter writes in-place via `push_leaf_with_*` per the `StructLayout`'s field projection. The post-Tape inversion landed: `crates/tape/` deleted at AZ-II.cutover.O5 + AZ-III.W1, no parallel intermediate untyped phase exists, and `project_types` is the single projection pass.

**Grammar-authoritative is enforced statically for the runtime layer**. `crates/core/tests/no_grammar_name_branch.rs` is the AST-scan gate — production code under `crates/core/src/runtime/**` and `crates/core/src/backend/rust/emitter/shapes/**` carries zero literal-rule-name match arms outside `#[cfg(test)]`. W1 closed this gate at commit `c56bda3f`. The seven `from_rule_name(&str) -> Kind` impls are deleted; `EmitStrategy::for_grammar` is manifest-driven through `PRODUCTION_MANIFEST_TABLE` (`crates/ir/src/registry/strategy.rs:134-189`); `substrate.rs:70-78`'s JSON-builder fallback is replaced by `panic!` on invalid binding; `recover_modifier`/`recover_binary_op`/`wrap.rs:89-99` byte-recovery deleted. The synthetic-grammar test (`crates/core/tests/synthetic_grammar_strategy.rs`) holds the manifest-only round-trip gate.

**No-orthogonal-codepaths is honoured for the structural layer**. One regex system (HIR through `bbnf-regex`); one arena allocation strategy; one `EmitStrategy::StructDirect` target; one `compile_grammar` driver in `crates/core/src/backend/driver/mod.rs:261-347` walking `GrammarIR` once per grammar.

**FactAuthority is real where consumed**. `crates/ir/src/passes/facts/authority.rs` is consumed at `alt_dispatch.rs:82` for branch-admissibility decisions and is the canonical fact-resolver for that surface.

**The Path subsystem (W2) is grammar-derived end-to-end**. `crates/bbnf-path/src/path_macro.rs:1-80` documents a five-stage pipeline (parse → lex → lower → validate → emit) where validation reads the grammar's `StructRegistry` and emits compile-time-anchored diagnostics naming segments and valid alternatives. The `path_check` IR pass (`crates/ir/src/passes/path_check.rs`, plus the `inline_trace.rs` sidecar) re-resolves source rule names through `fuse_single_use` substitutions; the `fixture_grammar_fused_rule_still_resolves` golden test guards the closure.

**Backend-shared decisions live at the right layer**. The driver (`crates/core/src/backend/driver/mod.rs`) emits target-agnostic structural decisions (dispatch strategy, span compression, sep_by detection, inlining, operator-chain detection) and threads them through the `Emitter` trait (`crates/core/src/backend/emitter.rs:31-566`). `compile_grammar` does not branch on backend identity; the `Emitter` impl decides the syntax. This is the substrate the W3 + W5 multi-backend story rests on.

These surfaces are the ones the synthesis claim grounds itself in. They are the shape that justifies "the grammar is the only distinguishing input" in the present tense, not as an aspiration.

## §2 Where bbnf Still Overfits or Has Residual Special-Casing

Beyond Fermat's enumeration (which W1 closed for the OVERFIT-HARD class — the `from_rule_name` strings, the `EmitStrategy::for_grammar` arm-list, the `JsonStructBuilder` silent fallback, the BBNF byte-recovery functions), four classes of residual special-casing survive at HEAD. Each is a decision surface that still encodes a per-grammar fact in a place the registry could carry as data. Each is named here with file:line so successor waves have a precise target.

**S1 — `from_rule_id(u32) -> CompoundKind` impls relocate the lookup but preserve its shape**. The W1.1 redress chose Path B: rename `from_rule_name(&str)` to `from_rule_id(u32)`, replace string literals with integer literals matching the rule-id allocation in `crates/core/src/grammar/generated/<grammar>.rs`. The literal-name match arm is gone. The literal-rule-id match arm remains, with comments naming the rule the integer corresponds to:

```rust
// crates/core/src/runtime/bbnf/arena.rs:148-191
pub fn from_rule_id(rule_id: RuleId) -> Self {
    match rule_id {
        16 => Self::ImportPath,
        21 => Self::ImportItems,
        // ... 30+ arms ...
        _ => Self::Other,
    }
}
```

Eight grammars carry one of these match expressions (`bbnf, bnf, csv, css_pretty, ebnf, google_sheets, math, css_l4`). The static AST gate passes because the gate scans for *string* literals; integer literals are not the regex it tests. The W2 `StructRegistry` already projects the layout, including the `LayoutKind` discriminator. *The runtime could read `registry.compound_kind_for_layout(layout)` directly* — the per-grammar enum is a typed wrapper around a value the registry knows. This is the architectural transposition T1 in §3.

**S2 — `(layout.kind, layout.rule_id)` builder dispatches preserve the same shape one layer up**. JSON's `crates/core/src/runtime/json/builder.rs:270-289` keeps the dispatch:

```rust
let frame = match (layout.kind, layout.rule_id) {
    (LayoutKind::Struct, 5) | (LayoutKind::TaggedEnum, 5) | (LayoutKind::UntaggedEnum, 5)
        => OpenFrame::Array { items: Vec::new() },
    // ...
};
```

The discriminator is now `(LayoutKind, RuleId)` integer literals instead of `(LayoutKind, &str)`. The deeper invariant, *the registry projects `OpenFrame` selection from layout*, is not yet realised. CSS L4's builder carries the same shape at higher arm count.

**S3 — CSP authority is partial; the consumer still re-overrides via sidecar lookup**. `crates/core/src/backend/strategy/alt_strategy.rs:160-184, 193-197` reads `ir.recognizer_decisions[id].alt_mode` (the CSP's pin) but then re-consults `ir.key_dispatch_configs.contains_key(id)` and elevates the strategy to `KeyDispatch` when the structural detector disagrees with CSP. The doc-comment cites the rationale: "the structural detector has higher coverage than the CSP recognizer shapes." That rationale is exactly the seam Babbage's Hard Finding 7 names: CSP authority is not yet authoritative. *The W3b dispatch installer's commit body states that the override should be retired*. The retirement remains a W4 carry. Until it lands, two decision surfaces co-exist and the "consumer-authoritative" gate (AZ-IV §Hard Gate 12) is open.

**S4 — Inline-trace `_with_trace` wrappers are layered additively over the bare passes**. `crates/ir/src/passes/transform/inline.rs:38-40` exposes `inline_acyclic_with_trace(ir, trace)` next to the bare `inline_acyclic(ir)`; `fuse_single_use_with_trace` is the sibling. The `path_check` IR pass requires the trace; `pipeline/compile.rs:729-732` calls the `_with_trace` variants in production. This is the same shape S1 takes at the other end of the pipeline: the bare API is preserved, the trace-recording API is bolted on. The `_with_trace` variants ought to be the canonical pass form; the trace recorder ought to be a pluggable `&mut dyn TraceSink` parameter that defaults to a null sink. AUDIT-2026-05-02-mid-tranche §R2 names this as the recording-coupling risk.

**S5 — `bbnf-path` consumes a synthetic registry fixture, not the production const**. `crates/bbnf-path/src/registry.rs` carries hand-authored `GrammarFixture` rows for the four canonical grammars; the `path!` macro validates against the fixture, not against the const projected by `cargo xtask regen`. AUDIT-2026-05-02-mid-tranche §R1 names this as a 1-day W4/W5 task. The fixture *is* the production-shape today (the four grammars are the four supported markers), but the synthetic surface means a grammar shape change requires editing two trees: the generated module and the fixture. This is a substrate-without-real-consumer pattern at the macro layer; the macro's correctness depends on the fixture mirror staying tight.

**S6 — Audit-tag aliasing carries a small per-grammar string table**. `crates/core/src/pipeline/compile.rs:219-224` (per Fermat F6) maps entry-rule names (`"value" | "json"` → `Json`, `"stylesheet" | "css_l4" | "cssL4"` → `CssL4`, `"spreadsheet" | "sheets" | "google_sheets"` → `Sheets`) to a debug-only `GrammarAuditTag`. Diagnostic, not parity-bearing, yet a residue of grammar-name aliasing in production code. Fermat's recommended fix (`Custom(entry_name)` for every grammar) preserves the artefact key without the alias table.

**S7 — Per-grammar runtime modules duplicate the structural skeleton**. Every grammar has its own `arena.rs`, `builder.rs`, `value.rs`, `document.rs`, `view.rs`: nine sets, each rewriting the same surface independently. AZ-IV §Hard Gate 21 + W5.3 names the structural-skeleton dedup target: one `Arena<G>` + one `Builder<G>` template parameterised by `StructRegistry` + `TypeDesc`; per-grammar typed `*Value` enums survive untouched. This is genuinely scoped to W5 and is the right architectural shape; it is enumerated here because *until W5 lands*, the system continues to ship eight near-twin builder files, and the lowering-quartet's structural-detection seam (AUDIT mid-tranche §R4) carries part of the cost.

The seam-load ranking, by parity-bearing weight: S1 + S2 (production discriminator surface) > S3 (consumer-authority gate) > S7 (W5 in-flight) > S4 (recording channel) > S5 (proc-macro fixture) > S6 (debug aliasing).

S1 + S2 are the seams that AZ-IV's "grammar generality" closure has *moved* but not *eliminated*. The seams are smaller (rule-id integers vs rule-name strings) and the static gate catches the gross case (string literals), but the deeper invariant, *the registry projects discriminators; consumers do not*, is not yet realised at runtime.



## §3 The Architectural Transposition Queue

Each transposition below is a *re-shaping* of an existing decision surface, not a bug fix. Each names the orthogonal codepath / shim / special-case it eliminates. Wave-fit notes are bounded by AZ-IV's remaining waves (W3 / W4 / W5 / W6) plus recycled BA and post-AZ-IV.

### T1 — Registry-projected compound discriminator (eliminates S1 + S2)

**Mechanism**. Extend `StructRegistry` with `compound_kind_for_layout(layout: &StructLayout) -> u8` (or per-grammar associated discriminator type via a generic). Generate the per-grammar enum + `from_rule_id` body from the registry's projection during `cargo xtask regen`; runtime arena/builder modules read `registry.compound_kind_for_layout(layout)` directly. The `match (LayoutKind, RuleId)` form vanishes: the discriminator is the registry's value, not the consumer's switch.

**Why it's a transposition, not a fix**. Today the runtime carries the kind enum and the layout→kind mapping in hand-coded form; the registry knows everything it needs to project both. The transposition collapses the redundant decision surface (per-grammar arena.rs holding a copy of grammar facts the registry already holds) into a single source. It eliminates eight `from_rule_id` impls and two `(layout.kind, layout.rule_id)` builder dispatches.

**Cost**. Codegen: ~150 LOC added to `xtask/src/regen.rs` + the registry projection. Per-grammar arena.rs: ~30 LOC each removed (× 8 = ~240 LOC). Net delta: roughly neutral, but the consumer surface is *one* location instead of nine. Integration risk: medium — the discriminator type must round-trip through codegen and the registry must be available at runtime as a const (resolves S5 simultaneously when W4/W5 wires the production const).

**Wave fit**. W4 (Optimization Substrate Activation) — the W4 file bounds already include `crates/ir/src/passes/recognizers/shape_dict.rs` and the registry-touching surfaces; T1 fits the wave's "consume-or-retire substrate" frame. The static no-grammar-name-branch gate still passes; T1 would extend it to `no_layout_rule_id_branch.rs` to make integer-literal arms a regression target too.

### T2 — `EmitStrategy` becomes one trait carrying Rust/TS/WASM as instances (eliminates the `SubstrateBinding { rust, ts, wasm }` triplet)

**Mechanism**. Replace `enum EmitStrategy::StructDirect { rust, ts: Option, wasm: Option }` with `trait EmitStrategy<B: Backend>` parameterised by the backend instance. The `PRODUCTION_MANIFEST_TABLE` rows already carry per-backend bindings as parallel string fields; rotate them to `impl EmitStrategy<Rust> for ManifestEntry` + `impl EmitStrategy<Ts> for ManifestEntry` + `impl EmitStrategy<Wasm> for ManifestEntry`. The driver becomes `compile_grammar<B: Backend, E: Emitter<B>>` and instantiates the strategy at the seam, not at the enum.

**Why it's a transposition, not a fix**. The current shape, three sibling `Option<SubstrateBinding>` fields, encodes the per-backend axis as data and forces every consumer to handle the absence case. Backends are not data; backends are decision surfaces. Modeling them as trait instances localises each backend's binding to its own impl block; adding WASM is `impl Backend for Wasm + impl EmitStrategy<Wasm> for ManifestEntry`, no enum changes. It also models the eventual native-Rust deferred backend (per `project_multi_backend`) as a fourth instance without disturbing the existing three.

**Cost**. ~200 LOC moved + 50 LOC added to express the trait. Integration risk: medium-high — the strategy resolver is read by `pipeline/compile.rs::resolve_emit_strategy` and `backend::rust::emitter::shapes::emit_shapes_for_grammar`; both call sites need to thread the backend type parameter. The deferral risk is real: this is the kind of refactor that *seems* clean but turns up half a dozen `where B: Backend + Default + Copy` bounds at the leaf. Bounded scope of W4 makes this risky to land mid-tranche.

**Wave fit**. Post-AZ-IV recycled BA — recycled BA opens for rule-discovery work, and the multi-backend trait shape supports the rule-discovery substrate cleanly. Premature for W3-W6; the immediate per-backend cost is too small (the `Option` checks happen at one site) for the refactor surface.

### T3 — Inline-trace recording lifts from `_with_trace` wrappers into a `&mut dyn TraceSink` parameter (eliminates S4)

**Mechanism**. Change `pub fn inline_acyclic(ir: &mut GrammarIR)` to `pub fn inline_acyclic(ir: &mut GrammarIR, trace: &mut dyn TraceSink)` with a default `NullTraceSink` for callers that don't need recording. Delete `inline_acyclic_with_trace` as a separate symbol. `fuse_single_use_with_trace` follows. The pass's *behaviour* never changes; only the recording hook becomes canonical instead of additive.

**Why it's a transposition, not a fix**. The `_with_trace` shape is two functions doing the same work, separated by which channel they write to. The `&mut dyn TraceSink` shape says: every pass takes a trace sink; production calls pass a null sink; `path_check` callers pass an `InlineTrace` sink. The decision surface ("does this pass record?") becomes data, not API.

**Cost**. ~20 LOC across `crates/ir/src/passes/transform/{inline,fuse}.rs` and the four call sites in `pipeline/compile.rs`. Integration risk: low — the `dyn` sink is monomorphisation-friendly when callers pass concrete types; the `NullTraceSink` is a unit struct.

**Wave fit**. W4 modify-carve — the W4 file bounds already touch `pipeline/compile.rs` and the IR passes. T3 is small enough to absorb without scope creep; it closes AUDIT mid-tranche §R2 directly.

### T4 — `bbnf-path` synthetic registry fixture → production const (closes S5)

**Mechanism**. `cargo xtask regen` emits `pub const REGISTRY: StructRegistry = StructRegistry::from_layouts(...)` per generated grammar. `bbnf-path` reads through a `GrammarMarker → registry const` resolver instead of through the hand-authored `GrammarFixture`. The proc-macro's lex / lower / validate stages stay unchanged; only the source of truth swaps.

**Why it's a transposition, not a fix**. The synthetic fixture is a substrate-without-real-consumer in miniature: the fixture mirrors the const, but the const is the truth. Until the swap lands, the fixture is the ostensible truth and the macro is correct only insofar as the fixture stays tight. The transposition routes the macro through the registry the rest of the system already trusts.

**Cost**. ~80 LOC in `xtask/src/regen.rs` (registry-const emission) + ~40 LOC in `crates/bbnf-path/src/registry.rs` (fixture → const-resolver). Integration risk: low — the fixture's shape mirrors the const; the swap is mechanical.

**Wave fit**. W4 or W5 — AUDIT mid-tranche names this as a "1-day task in W4." It pairs naturally with T1 because the production const T1 needs is the same const T4 needs.

### T5 — Pluggable cost model (currently hardcoded in `egraph/cost.rs`)

**Mechanism**. The `GrammarCostModel` struct already implements `egraph::CostModel<GrammarENode>` and reads weights from `crate::CostConfig::default()`. The hardcoded `MAP_PRESERVE_BONUS: f64 = 1.0e6` (`cost.rs:150`) and the per-node-class costs (`literal_cost`, `regex_cost`, `ref_cost`, `seq_per_child`, `weights.alt_per_branch`, `weights.dispatch_bonus`) live partly in `CostConfig` and partly inline. Lift the inline constants into `CostConfig`; expose `CostConfig` as a field on `CompileOptions`; admit per-grammar cost overrides from the manifest.

**Why it's a transposition, not a fix**. Per `feedback_pluggable-components`, decision points must be pluggable, not hardcoded branches. The cost model is a decision point with one knob (the inline constant). Pluggability is a small lift but it converts a hidden axis (cost shape) into a visible one (config field), which is the exact shape recycled BA's rule-discovery substrate needs to evaluate cost-config alternatives during ranker training.

**Cost**. ~30 LOC of struct-field plumbing + ~10 LOC of manifest-read code. Integration risk: very low — the inline constant moves to a config field and the pass reads through `cfg.map_preserve_bonus` instead of the inline.

**Wave fit**. W4 modify-carve — fits the wave's substrate-activation frame. Recycled BA can then admit per-grammar cost overrides cleanly.

### T6 — Generated module-split output: per-concern files instead of monolithic dumps

**Mechanism**. `crates/core/src/grammar/generated/<grammar>.rs` is a single file per grammar today, ranging 624 LOC (math) to 91,185 LOC (css_l4). Extend the regen step to emit `<grammar>/{parse,types,registry,recognizer_plan}.rs` modules. The css_l4 file becomes a directory; the parse functions move to `parse.rs`, the type definitions to `types.rs`, the const tables (registry, recognizer plan after W3) to `registry.rs`, et cetera. `mod.rs` re-exports the public surface so consumers see the same import paths.

**Why it's a transposition, not a fix**. Monolithic generated dumps fight every IDE, every diff tool, and every compile-cache heuristic. They are the kind of thing that survives because nothing strictly *requires* them to be split — until they do, and by then the generator has accumulated O(N) growth that's painful to refactor under fire. The 91 KLOC css_l4 file is already at the threshold where `wc -l` and Read-tool offsets are required ceremony. Splitting now, while the generator is small, is cheaper than splitting after recycled BA's rule-discovery doubles the rule count.

**Cost**. ~150 LOC in `xtask/src/regen.rs` (per-concern file emission + mod.rs synthesis). Integration risk: low — the public import paths are preserved; the change is internal layout.

**Wave fit**. W4 or W5 — fits the wave's "generated-size budget" frame (`feedback_generated-size-budget`). The css_l4 91 KLOC artefact and bbnf 17 KLOC artefact are the two that benefit immediately; the others gain readability.

### T7 — Folding `crates/csp-solver`, `crates/egraph`, `crates/simd-scan` into a single `crates/optimizer` umbrella vs the GESTALT §5 split-out plan

**Mechanism (one direction)**. Combine the three optimisation crates into `crates/optimizer/{csp,egraph,scan}/`. Internally, the public APIs stay; the workspace gains one fewer member.

**Mechanism (the other direction)**. Per GESTALT §5, ship each as its own repo: `csp-solver` joins csc411 (canonical-source policy already in place); `egraph` joins parse-that or stands alone; `simd-scan` joins parse-that or stands alone.

**Why it's a transposition, not a fix**. The three crates serve as general-purpose infrastructure (`feedback_general-infra-crates`); they are independent algorithms that bbnf-lang composes. The current shape (three sibling workspace members) is fine but *transitional* — neither the umbrella nor the split-out shape, just unstable equilibrium. The umbrella reduces top-level workspace surface; the split-out aligns with the general-infra precedent.

**Cost**. Umbrella: ~50 LOC of `Cargo.toml` and re-export plumbing; very low integration risk. Split-out: per-crate, ~100-200 LOC of cross-repo plumbing plus the canonical-source policy machinery (already exemplified by csp-solver / csc411).

**Wave fit**. Post-AZ-IV — explicitly out of AZ-IV scope per `AZ-IV.md` §Cross-Repo Future Work. The umbrella is *also* post-AZ-IV; the right answer is the split-out, but only after AZ-IV closes and the API surface is locked.

### T8 — Tagless-Final / GADT-style backend trait so emit_rust + emit_ts share a single typed traversal

**Mechanism**. Today `Emitter::Output` is `Default`-bound; the Rust backend overrides only `emit_rule_function` / `emit_type_definitions` / `emit_grammar` and discards per-rule bodies (the comment at `emitter.rs:19-30` documents this — the Rust backend routes through `dta_run` wholesale). TS + WASM override every method. The two backends use the same trait but in fundamentally different ways: Rust treats the per-rule emit calls as no-ops, TS treats them as the canonical surface.

A tagless-final shape would split the trait into two: `EmissionBackend<Output>` for the per-rule shape (TS, WASM) and `MonolithicBackend<Output>` for the whole-grammar shape (Rust). The driver dispatches to whichever shape the backend implements, not to a Default::default() that gets discarded.

**Why it's a transposition, not a fix**. The current shape is one trait carrying two emission models. The model the Rust backend uses is *invisible* in the trait surface (the no-ops happen at every per-rule call), making the trait's contract weakly-typed. The tagless split makes each backend's emission shape explicit and statically-checked.

**Cost**. ~300 LOC of trait surface change + ~100 LOC across the three Emitter impls. Integration risk: high — touches `compile_grammar`, every per-shape `emit_*` call site, and the backend impls. The benefit is mostly clarity, not capability.

**Wave fit**. Post-AZ-IV — too disruptive for the remaining waves. *Maybe* recycled BA when the rule-discovery substrate stabilises; more likely a successor letter.

### T9 — Consolidate `with_trace` and the recognizer-decisions sidecar into a single decision-stream

**Mechanism**. `ir.recognizer_decisions: HashMap<NodeId, RecognizerDecision>` and `ir.key_dispatch_configs: HashMap<NodeId, KeyDispatchMatch>` are two parallel sidecars holding adjacent facts about the same NodeId. Combine them into one `ir.decisions: HashMap<NodeId, Decisions>` where `Decisions` carries every fact the consumer needs in one struct. The CSP installer writes one entry per NodeId; consumers read one entry; the override seam in alt_strategy.rs disappears because there is no second sidecar to consult.

**Why it's a transposition, not a fix**. The override at alt_strategy.rs (S3) exists *because* the sidecar is structurally separate from the CSP decision; the consumer can read both and pick. Combining them eliminates the second-source temptation. CSP authority becomes a structural property: the only place a consumer can read the decision is the unified stream. This is a behaviour-preserving merge with a strong invariant payoff.

**Cost**. ~80 LOC of struct definition + ~50 LOC across the installers and consumers. Integration risk: medium — touches the CSP installers, the strategy consumers, and the recognizer pass; needs careful sequencing.

**Wave fit**. W4 — the wave already owns the CSP authority globalisation and the sidecar consumers. T9 is the structural form of "make CSP authoritative."

## §4 The Minimum Architectural Surface

The smallest architecture that fulfils GESTALT §3 is a four-layer stack with no orthogonal seams between layers:

1. **Grammar source + manifest**: the BBNF text plus `[workspace.metadata.bbnf-strategy]` row per grammar.
2. **IR + registry**: `GrammarIR` plus `StructRegistry` plus the canonical decision stream (per T9). One pass populates each fact; one consumer reads each fact; no fact has two readers and no consumer has two readers.
3. **Driver + emitter trait**: one `compile_grammar` walking IR once, one `Emitter` trait per backend instance.
4. **Per-backend codegen**: one emitter impl per backend; the impl is *the only place* a backend identity is named in production code.

Compared to current (post-W2 HEAD), what's surplus:

- **The per-grammar arena/builder pairs duplicate the structural skeleton** (T1 + S7): nine sets of files where one parameterised template suffices.
- **The CSP override seam at alt_strategy.rs:160-184** (S3, dissolved by T9): a second decision surface that should be the only one.
- **The `_with_trace` wrapper duplication** (S4, dissolved by T3): a recording channel layered as a separate API.
- **The `bbnf-path` synthetic fixture** (S5, dissolved by T4): a registry-mirror that should be the registry.
- **The hardcoded `MAP_PRESERVE_BONUS` constant** (T5): a cost-config knob that should be in `CostConfig`.
- **The monolithic generated.rs files** (T6): one file holding multiple concerns.
- **The `EmitStrategy` enum's parallel `Option<SubstrateBinding>` triplet** (T2, deferred): three optional fields modeling what trait instances naturally express.

What's missing:

- **A registry-projected discriminator API** (T1): `compound_kind_for_layout` is the missing primitive.
- **A unified decision stream** (T9): `ir.decisions` is the missing aggregate.
- **A backend-instance trait** (T2): only relevant when the WASM/native backends mature; deferred.

The minimum surface is roughly 60-70% the current line count *for the runtime+driver layers* (T1 + S7 close most of the duplication) with no loss of capability. The IR + emitter trait layers are already at minimum for the present feature set; the driver can lose ~80 LOC through T9 + T3 consolidation. The proc-macro is at minimum after T4.

The synthesis principle's structural cost, the cost of being grammar-derived end-to-end, is *currently* about 40% per-grammar duplication overhead. The minimum-surface architecture above reduces this to <10%; the rest is essential per-grammar typed-leaf API surface (e.g., CSS's typed-color family) which `feedback_preserve-rich-ast` requires to survive untouched.

## §5 Performance Posture Toward the Optimum

AZ-IV §Hard Gate 16 mandates the lazy lane (`bbnf_get_twitter`) closes ≤ 5x sonic-rs same-harness; ≤ 1.0x is the stretch target with profile evidence. The W3 path-driven recognizer is the load-bearing piece — the eager-then-walk lane is 2953× sonic-rs at AZ-III close because the parser materialises the full tree before path resolution. W3's `parse_with(input, &path)` consumes the typed `TypedPath<G, T>` at parse time and emits a per-rule decision table mapping `(rule, segment_kind) → {ParseFully, ParseUntil(child_index), Skip}`. The decision table is generated at codegen from the IR's `path_check` pass output; no rule-name match arms in the emitter; grammar-general by construction.

The piece adjacent to W3 that *also* moves the needle:

**SIMD-scan broadening** (W4). `crates/simd-scan/` is wired into 5/9 generated grammars (`json, bbnf, google_sheets, csv, ebnf`); CSS L4, CSS Pretty, math, bnf miss the structural-scan adapter even though `compute_structural_alphabet` runs unconditionally (Babbage row 11). CSS L4 is a perf-load grammar — the tailwind regex_scan timeout (AZ-III.C5) routes here. Broadening simd-scan to all 9 grammars is housekeeping in the sense that the substrate already exists; it is *not* housekeeping in the sense that CSS L4 perf doesn't close without it.

**Regex engine consumption** (W4). `cost_model.rs:175` reads `ir.regex_engine_decisions[sid]` (CONSUMED), but `crates/core/tests/regex_engine_authoritative.rs:54-87` proves the chosen variant is *not always emitted as the exact chosen engine path* (Babbage row 12). This is the W4 hard-gate 5 frame: regex engine decisions select concrete emitted scanner classes. Proven necessary by the tailwind timeout.

**Map fn_id extraction preservation** (already landed at W0.4, commit `4373a49d`). `cost.rs:149-152` carries the `MAP_PRESERVE_BONUS = 1.0e6` pin; the named regression test holds. This is necessary infrastructure but not perf-bearing in itself; it unblocks named_color emission which is a *correctness* gate, not a perf gate.

**RuleSet load + ruler family activation** (W4). `crates/ir/src/rewrites/` and `crates/egraph/src/ruler/{enumerate,oracle,residue}` are WIRED-NOT-CONSUMED (Babbage rows 7-8); `pipeline/compile.rs:560-573` is an eprintln-only sink. The W4 hard-gate is "every non-empty loaded rewrite/ruler ruleset proves the full production chain" — but per the W4 plan, the wave's actual disposition is *delete* the unused substrate and let recycled BA recreate it. This is housekeeping for AZ-IV; the perf payoff is post-AZ-IV.

The candid ranking, by perf needle-movement:

1. **W3 lazy parse**: closes the 2953× gap; the only piece large enough to flip the bench.
2. **Simd-scan broadening to 9/9 + regex engine emission**: closes the tailwind timeout class and CSS L4 leftover.
3. **T1 (registry-projected discriminator)**: eliminates the per-grammar dispatch hash-map indirection; samply on the post-W3 fixture should pin this.
4. **T6 (generated module split)**: not perf in the runtime sense, but compile-time perf for the iteration loop, which compounds.
5. **The rest (T3, T4, T5, T9)**: clarity / pluggability, not throughput.

What blocks the perf trajectory:

- **CSP override seam (S3) blocks regex-engine emission consistency**. The `key_dispatch_configs.contains_key` re-override means the chosen scanner is sometimes the structural detector's pick, not the CSP's pick — which means the regex_engine_decisions are the *available* truth, not the *consumed* truth. T9 dissolves this seam; W4 needs T9 to close hard-gate 5 cleanly.
- **The `_with_trace` wrapper duplication (S4)** is not perf-bearing but *is* refactor-blocking. Future passes that need to record intermediate state pay the same shim cost.
- **Per-grammar runtime modules (S7)** are perf-neutral until W5's templated dedup; the dedup itself is structural, not throughput-bearing. The performance question is whether the templated form preserves inlining; the answer is yes if the template is monomorphised per grammar (which is the natural Rust shape).

## §6 Deferred-Item Architectural Lens

Boole's hardening pass surfaced the chronic-deferral pattern: 13/15 carries chronic ≥ 3 tranches. The lens here is per-deferral: is this a chronic deferral because the item is hard, or because the system isn't yet shaped to absorb it?

**Sheets parity (133/133)**. Closed at W1 (commit `c56bda3f` lineage). Was chronic across AZ-II → AZ-III → AZ-IV.W0; closed *because* W1 landed the registry-based discriminator (Path B via `from_rule_id`). Diagnosis: was system-shape blocked, not problem-hard. Architectural transposition that absorbed it: registry-projected discriminator (the partial form of T1, one step away from full closure).

**Egraph `Map { fn_id }` preservation** — closed at W0.4 with `MAP_PRESERVE_BONUS = 1.0e6` pin. Chronic because the cost model strip was discovered late and no tranche before AZ-IV had file-bounds touching `cost.rs`. Diagnosis: file-bounds-blocked, not problem-hard.

**TS backend executable (Node-execute)** — open; routed to W5. Chronic across AZ-I → AZ-II → AZ-III → AZ-IV.W1 because the `crates/bbnf-path-ts` cdylib never landed. Diagnosis: substrate-missing, not problem-hard. The shape (`feedback_wasm-subcrate-pattern`: cargo workspace member, cdylib, path-dep) is well-understood; the *substrate* (cdylib infrastructure and template-tag binding) was not built.

**Tailwind regex_scan perf timeout**. Open; routed to W4. Chronic across AZ-II → AZ-III → AZ-IV. Diagnosis: half problem-hard (large alphabet + last-byte narrowing tradeoffs), half system-shape blocked (the regex_engine_decisions consumer at `cost_model.rs:175` reads the decision but the emitter doesn't always emit the exact chosen engine; see Babbage row 12). The system-shape part dissolves with T9; the problem-hard part is a real perf engineering problem.

**Cross-profile watchdog rows** — open; routed to W6. Chronic across AZ-II → AZ-III → AZ-IV. Diagnosis: pure measurement / harness work; not architectural. Profile coverage matrix needs filling.

**`RuleSet` field unconsumed** — routed to W4 *for deletion*. Was chronic-by-rebuild: the substrate kept getting rebuilt on each tranche under the assumption that consumers would land. Diagnosis: the shape was right (substrate-with-eventual-consumer) but the consumer never came. The deletion is the right call; recycled BA recreates clean from the rule-discovery side. Architectural transposition: substrate-with-consumer enforcement (the W5 permanent `substrate_audit.rs` test) closes this pattern at the meta-level.

**`type_obligations` Vec un-drained** — open; routed to W4. Chronic since AZ-III. Diagnosis: missing diagnostic surface. The shape is right (the field carries audit-bearing facts) but no consumer drains it. Either codegen surfaces the obligations through a diagnostic emit path, or the field deletes. T9 (decision-stream consolidation) is adjacent: a unified decision stream would naturally carry the obligation as one of its facts.

**`shape_dict_templates` / `shape_dict_selection` un-read** — open; routed to W4. Chronic since AZ-II. Diagnosis: substrate landed; emitter consumer never wired. Same pattern as `RuleSet`; the W5 substrate-audit test will catch this if W4 doesn't land or delete it.

**26 `#[ignore]` triplet enumeration** — open; routed to W6 close-honesty. Diagnosis: documentation discipline; not architectural.

**5 csc411-only files in csp-solver dead-code** — open; W4 or post-AZ-IV. Diagnosis: cross-repo coordination; not architectural per se, but a real cost-of-canonical-source-split tax.

The pattern: chronic deferrals divide roughly half-and-half between "system isn't shaped to absorb it" (Sheets parity, TS executable, half of tailwind) and "the work is just hard / measurement-bound" (cross-profile rows, perf engineering). The system-shape half closes when the corresponding architectural transposition lands (T1 absorbed Sheets; T4 closes the bbnf-path mirror; T9 closes the CSP override; T8 closes the dual-trait emitter shape). The hard-work half stays hard.

The taxonomy supports the AZ-IV thesis claim that *non-routable carries either close inside the tranche or AZ-IV does not close*. The W4 wave is where most of the architectural transpositions land; the wave's success is the proxy for the half-and-half ratio inverting.

## §7 Recommendations — Top-5 Architectural Transpositions

Ranked by `(elegance × simplicity × performance) / (integration cost)`. Each ranked item carries: mechanism summary, wave fit, expected close-honesty impact.

### Rank 1 — T1: Registry-projected compound discriminator

**Mechanism**. `StructRegistry::compound_kind_for_layout(layout) -> CompoundKindId`; codegen emits the per-grammar discriminator consts; runtime arena/builder modules read through the registry. Eliminates eight `from_rule_id` impls and two `(LayoutKind, RuleId)` dispatches.
**Wave fit**. W4 (modify-carve on registry + xtask + runtime).
**Close-honesty impact**. Closes S1 + S2; extends the no-grammar-name-branch CI gate to no-rule-id-branch; the static gate becomes a tighter envelope around the synthesis claim. *The* transposition that converts AZ-IV's W1 closure from "moved the seam" to "eliminated the seam."

### Rank 2 — T9: Unified decision stream (`ir.decisions: HashMap<NodeId, Decisions>`)

**Mechanism**. Combine `ir.recognizer_decisions` + `ir.key_dispatch_configs` (and the surrounding sidecars) into one struct per NodeId. CSP installers write one entry; consumers read one entry; the override seam at `alt_strategy.rs:160-184` disappears because there is no second source.
**Wave fit**. W4 (the wave's CSP authority globalisation is exactly this transposition's structural form).
**Close-honesty impact**. Closes S3 + AZ-IV §Hard Gate 12 by construction; eliminates Babbage Hard Finding 7. CSP authority becomes a structural property, not a discipline.

### Rank 3 — T3: Inline-trace canonicalisation (`&mut dyn TraceSink` parameter)

**Mechanism**. `inline_acyclic(ir, trace)` becomes the canonical signature; `inline_acyclic_with_trace` deletes; production calls pass `NullTraceSink`; `path_check` callers pass `InlineTrace`. Same for `fuse_single_use`.
**Wave fit**. W4 modify-carve (small surface, fits cleanly).
**Close-honesty impact**. Closes S4 + AUDIT mid-tranche §R2; removes the additive-recording-shape risk. Cheap, clean, low-risk.

### Rank 4 — T4: `bbnf-path` synthetic fixture → production const

**Mechanism**. `cargo xtask regen` emits per-grammar `pub const REGISTRY: StructRegistry`; `bbnf-path` reads through the registry resolver; the hand-authored `GrammarFixture` deletes.
**Wave fit**. W4 or W5 (1-day scope).
**Close-honesty impact**. Closes S5 + AUDIT mid-tranche §R1. Removes the proc-macro-mirror substrate-without-real-consumer pattern.

### Rank 5 — T6: Generated module-split output (per-concern files)

**Mechanism**. `crates/core/src/grammar/generated/<grammar>.rs` becomes `<grammar>/{parse,types,registry,recognizer_plan}.rs`. Public import paths preserved through `mod.rs`.
**Wave fit**. W4 or W5 modify-carve (xtask change; consumer-transparent).
**Close-honesty impact**. Closes the implicit cost of the 91 KLOC css_l4 monolith; aligns with `feedback_generated-size-budget`. Compounds for every successor wave that touches generated code.

The five together close S1–S5 and the related risks (R1, R2, R7) without disturbing the AZ-IV thesis. They fit the W4 + W5 envelope; total LOC delta is roughly +200/-300 (T6's split offsets T1's removals). Integration risk is low-to-medium across the five; T9 carries the highest integration cost but also the highest payoff.

T2 (backend trait) and T8 (tagless-final) defer to post-AZ-IV; their cost outruns the tranche's remaining budget. T7 (umbrella vs split-out) is GESTALT §5's already-declared post-AZ-IV plan.

The mid-tranche audit's verdict — "the thesis is intact; carries are tractable" — survives this lens. The transpositions above are how AZ-IV's W4 + W5 + W6 close *cleanly* rather than *technically*: the seams the W1 closure moved get eliminated, not relocated; the substrates land their consumers, not their wrappers; the path subsystem reads from the truth, not from its mirror. The architecture is one wave away from honouring the synthesis principle structurally instead of by discipline.
