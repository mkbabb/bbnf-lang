# Tranche BB — Egraph Rule Inference + Ruler + VM Oracle + Ranker

> **Letter status — un-subsumed at master `40092b28` (post-AZ-IV close).**
> The previous BB tranche (subsumption banner pointing at AZ-IV + recycled-BA) is **archived** at `docs/tranches/BB/historical/subsumed/`. Per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`, the canonical post-AZ-IV letter sequence is **AZ → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) → BD+ (TS/WASM re-engineering)**. BB takes the original rule-discovery scope (same as the recycled-BA plan that BA was holding while the canonical ordering was being adjudicated). The BB letter is the rightful home of rule-discovery work; the recycled-BA archive at `docs/tranches/BA/historical/recycled-rule-discovery/` is its predecessor reading.
>
> BB opens after BA close.

## Thesis

BB closes the loop on `feedback_pluggable-components`, `feedback_csp-always-optimize`, and `feedback_general-infra-crates` by letting the e-graph *discover* grammar-level rewrite rules rather than only apply a fixed set. Ruler-style CVC enumeration over `IrNode` produces candidate pairs `(L, R)`; the e-graph itself is the fast-path equivalence check; the surviving VM interpreter serves as the non-circular ground-truth oracle on the *residue*. An automatic ranker scores every surviving candidate; a tiered review pipeline auto-accepts the trivial class, fast-tracks the structural class, and reserves full human review for the novel class only. Rules live outside `crates/core`: fleet-wide rules in a new `crates/ir/src/rewrites/` module, grammar-specific rules colocated with each grammar under `grammar/<name>/rewrites/*.ron` via a standardised schema `cargo xtask regen` scans at IR-pipeline time and compiles into that grammar's cost-config.

## Architectural Theses (preserved from recycled-BA plan)

1. **Rule inference over `IrNode` is an e-graph enumeration problem.** Ruler (Nandi et al. 2021) demonstrates the CVC-style approach: generate candidate terms over the grammar's alphabet up to bounded size, group them by equivalence under an oracle, extract rules as cross-class equivalences. BB applies the same shape to bbnf's `IrNode`.
2. **The e-graph is the fast path; the VM is the residue oracle.** An e-graph that already contains both `L` and `R` in the same class proves their equivalence without any external call. The VM runs only when the e-graph is silent — `L` and `R` belong to different classes under the current rewrite set.
3. **Rules are grammar-derived, not hand-coded.** Tranche H's `factor` / `merge_regex_alts` / `inline_acyclic` were hand-written. BB does not hand-code; BB discovers. Every rule that persists was produced by enumeration, survived oracle validation, and cleared the ranker tiering.
4. **Storage is extensible and out-of-core.** Fleet-wide rules live in `crates/ir/src/rewrites/`; grammar-specific rules colocate with their grammar directory. `crates/core` never accumulates a hand-curated rule list. Adding a grammar does not require editing core.
5. **Ranking + tiering is first-class.** Class 1 (trivial / algebraic / rediscovered) auto-accepts with audit log only. Class 2 (structural resemblance to hand-coded patterns) fast-tracks. Class 3 (novel) is the only class that consumes human review time.

## BA Dependency (hard opening gate)

BB opens after BA close. The opening contract:

1. **Direct-projection codegen GREEN.** BA closes the value-API direct-projection thesis; BB consumes the typed StructRegistry output for IR-rewrite candidate enumeration.
2. **`crates/ir/src/rewrites/` populated only with `path_seed.rs` + W3.0 seed bag** (or empty, if BA.W0 deleted them). BB.W0 recreates the rewrites tree clean from CVC enumeration; it does NOT inherit a hand-curated set.
3. **Workspace nextest 100% pass** at BA close. BB inherits the discipline.
4. **`cargo xtask regen --check` 9/9 green** at BA close. BB's grammar-rewrite-dir discovery integrates with the regen pipeline.
5. **TypedPath<G, T> + `path!` macro live + lazy parse on 4 grammars** — BA hardens these into the canonical value-API path; BB consumes them for path-rewrite enumeration.
6. **Substrate-audit test GREEN** at BA close. BB's new substrates (rewrites/, ruler/, ranker, schema) must pass it at every wave close.

If any of these is not true at BA close, BB does not open. The carry routes back to BA per the non-routable-carries discipline.

## Wave Table (preserved skeleton from recycled-BA; full body in `historical/recycled-rule-discovery/BA-rule-discovery.md`)

The recycled-BA wave table covered: W0 substrate preflight (regen drift + cost extractor + RuleSet load); W1 Ruler CVC enumerator over IrNode alphabet; W2 VM oracle on egraph residue; W3 ranker + Class-1/2/3 tiering; W4 grammar-colocated rewrite dir discovery + regen integration; W5 review ledger + CI status; W6 measurement + close.

The full wave shape is preserved at `docs/tranches/BA/historical/recycled-rule-discovery/BA-rule-discovery.md`. BB.W0 dispatch reconciles that plan with the post-BA codebase state (direct-projection landed; checkpoint discipline retired; `parse_with` is the value-API hot path) and produces an active BB plan that lives at this top-level path.

## Cross-Tranche Ordering

BB blocks BC. BC blocks any subsequent letter (BD+).
