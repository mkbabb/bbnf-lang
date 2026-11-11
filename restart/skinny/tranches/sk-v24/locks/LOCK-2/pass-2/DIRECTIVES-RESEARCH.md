# LOCK-2 — THE GRAPH ENGINE — PASS-2 RESEARCH DIRECTIVES

*Steering questions for pass-2 research — e-graph canonicalization/extraction, CSP selection, profile-fed cost, deterministic regen. Answer with concrete evidence, not position.*

## Binding frame (cross-lock, conveyed as fact)
- **Tree of record (XL-A, settled):** the greenfield loom (GN-1) is the tree-of-record-to-be; **skinny@HEAD is the MEASUREMENT SUBSTRATE** (only living implementation). Designs target greenfield structure; **ports scope organ-level, never tree-level**; legacy `crates/` archives at GN-0. Do not re-audit which tree hosts what — settled.
- **Tower ownership (XL-B, working, final at GESTALT-B):** graph-opt engines operate **WITHIN** the middle-IR tower at **named decision points**; the tower's shape is not ours to redraw. Frame every question as "at which decision point," not "what is the IR."
- **Regex axis (XL-C, ratified):** grammar tier keeps `Regex(StringId)` opaque; engine tier keeps rich `Hir` with a cheap bridge; canonical engine home is parse-that (vendored copy = substrate). Any regex-canon question lives at the engine tier.
- **Supersession (XL-F):** the GN-2.W1 scan-lowering predicate is superseded-in-part; its relation (**replaces / wraps / waits-on**) must be characterized with evidence, not assumed.

## Research questions (answer with evidence; no conclusion presumed)
1. **Saturated multiplicity.** On the substrate, what is the average/max e-class size after saturation across the fleet? How often does extraction face a genuine choice among ≥2 survivors, and at which node kinds (child-bearing vs childless)? What follows for extraction-as-decision vs plain cost-ranked canonicalization where multiplicity ≈ 1?
2. **Widening rewrites.** For a rewrite that *manufactures/widens* rather than narrows, what establishes (a) confluence with an all-narrowing rule set, (b) termination, (c) semantics preservation? Survey precedent for widening + narrowing coexisting in shipping e-graph engines.
3. **Organ port budget.** Does a grammar e-graph organ (build+saturate+extract+**teardown**) hold the per-parse compile budget (~230 µs order) and preserve `regen∘regen` byte-identity through cost-guided extraction? What total-order tie-break makes extraction deterministic (HashMap iteration inadmissible)?
4. **Context-sensitive extraction.** Does the fleet contain a caller-context conflict — a sub-rule reached under both a sink-only and an offset-tape caller — where one per-class shape cannot serve both? Audit `admits_sink_only`. If none: what follows for context-free per-class extraction; if so: complexity of the identity-preserving repair?
5. **IR-modeled competing sites.** Does any fleet grammar present ≥2 scan/self-loop sites modeled IN the IR (not merely in grammar source) with **real, binding** static-size + cyc/B costs? What IR extension models `SelfLoop`/`Prefilter`, and which lock owns building it?
6. **Solver shape.** For a near-tree decision graph (independent sites + budget coupling + few coherence edges), is **PBQP** (per-node discrete choice, pairwise cost matrices; RegAllocPBQP, Scholz & Eckstein 2002) the right solver vs generic B&B or e-graph extraction? Give optimality/complexity guarantees on this graph class and solve-cost evidence separating them.
7. **Budget knapsack as constraint.** A `bounded_sum` budget knapsack folded as a constraint at named sites — does it preserve **selection-equivalence** with the priority cascade it would replace? What coordinates ONE solver home across locks sharing the constraint store?
8. **Profile-fed cost.** Can a coefficient fitted from measured cyc/B (a) flip a real per-site decision, (b) win cold-paired at real value-start positions, (c) drive a freshness gate RED on mutation? How is attribution identified when whole-grammar cyc/B underdetermines per-feature coefficients (A/B-differential)? Which knobs would a fit supersede; how is env-source fenced from regen?

## Evidence standard (XL-H + refutation laws)
HEAD named on every measurement; worktree prototypes base-pin (`baseCommit`). Warm/cached benches inadmissible — cold per-parse only. Ratios certified with CIs; absolutes directional. Concrete artifact or SPEC-ONLY, stated as such.
