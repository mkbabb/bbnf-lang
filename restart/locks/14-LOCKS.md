# Hardening pass — plan set

You are auditing a freshly drafted plan set for the bbnf-lang BA-restart. The plan composes BA / BB / BC (and optionally BD+) tranches that re-architect the parser fleet from first principles toward grammar-agnostic, multi-backend, sonic-class direct-to-struct parsing.

The plan exists. Your task is to challenge and harden it: ratify what is sound, surface what is unsound, identify what is missing, and recommend what must change before execution begins.

You are not implementing. You are auditing.

## Subjects

The plan documents under audit:

- `docs/tranches/BA/BA.md` — the restart tranche
- `docs/tranches/BA/waves/W*.md` — wave-level specifications
- `docs/tranches/BB/BB.md` — successor tranche
- `docs/tranches/BC/BC.md` — successor-of-successor tranche
- Optionally: `docs/tranches/BD/BD.md` if drafted (TS/WASM emergence)

Read each end-to-end before producing any audit output.

Read also for context (do not audit, but use as ground truth):

- `audit/HARDENING-SYNTHESIS-2026-05-03.md` — codebase audit synthesis from the prior pass
- `audit/SOTA-2026-05-03.md` — sonic-rs / simdjson / lightning-css research
- `audit/CENSUS-2026-05-03.md` — kill-list of grammar-specific code, tape residue, dupes, god modules
- `audit/MODULES-2026-05-03.md` — per-file fates and 17-step pipeline ordering
- `audit/RESTART-SKETCH-2026-05-03.md` — JSON parse trace + post-restart pipeline sketch
- `docs/HARDENING-AUDIT-PROMPT.md` — codebase-audit prompt (for methodological symmetry)

## Gestalt — fourteen locks

The plan must reflect these fourteen architectural commitments faithfully. Any wave that violates one is a fault.

1. **Tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead.** Tape is the greenfield's contiguous parsed-token-stream-with-payload-arena, unioned with direct-to-struct typed values that borrow into it (`&'i Tape<'i>` + index). The 2,000-commit prior failure was implementation, not concept: orthogonal codepaths (the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology); type ambivalence (tape and OpenFrame and direct-to-struct competing for the same role); substrate-first/consumer-later (Era V failure mode); columnar SoA designed in AV.04 archaeology but never activated. The greenfield's tape lives at `runtime/src/tape/`; typed-value records borrow into it; per-grammar runtime modules (template-emitted at `runtime/src/grammars/<name>/`) emit accessors; one materialisation surface; one Visitor pattern; no parallel substrate. Columnar SoA stays buried. Plans that resurrect parallel substrates (OpenFrame ladders; columnar SoA; type-ambivalent dual representations) or implement tape with consumer-later sequencing are faults; plans that implement tape properly with same-wave consumer wiring + direct-to-struct union are honoured. **2026-05-04 reframe**: the prior restart's wholesale retirement of the tape name was an over-correction against the implementation failure; the user has confirmed tape is the right substrate when implemented properly. Lock 1's spirit (no parallel substrate; no orthogonal codepath; no Vec<OpenFrame>::clone pathology) holds; the no-rename clause is amended.

2. **Layout lowering is the canonical IR pass name**. The term replaces *type projection / type collapsing / type inference / type elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis* everywhere. Old terms appear only in archived docs. The IR module is `bbnf-ir/src/passes/layout/`; the IR record is `Layout`; the trait that consumes it is `LayoutSink`. Any plan section referring to a retired term is a fault.

3. **Cursor-parse + byte-skip unified, with cursor branch elided when path is empty**. One parse implementation. Cursor consultation generates byte-skip when consult returns `Skip`. The empty-path case (`__EAGER_EMPTY_PATH`) elides cursor calls entirely so the eager fast path pays no consultation cost. Any plan that bifurcates byte-skip and cursor-parse into two implementations is a fault.

4. **Per-domain orthogonal optimization**. CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, and cost model compose by output-piping. No unified hypergraph. Each lives in its own crate (egraph + csp-solver path-deps until stable). Any plan that fuses CSP and e-graph into one solver is a fault.

5. **IR + per-backend lower**. Codegen emits a backend-agnostic typed IR; per-backend lowerers (Rust now, TS+WASM at BD+) produce native source. There is no source-emit-per-backend duplication; there is no trait-based emitter walking grammar directly. The IR is the contract. Any plan whose Rust codegen and TS codegen do not share an IR is a fault.

6. **xtask emits committed source artefacts**. No proc-macro façade. css_l4.rs at 107 K LOC is greppable on disk. Build is fast incremental because expansion is not at compile time. Any plan that proposes proc-macro for codegen output (other than the bbnf-path / bbnf-path-ts proc-macro shells, which are different) is a fault.

7. **`crates/path/` is the consolidated path crate**. The runtime cursor engine merges INTO it; the existing `crates/core/src/path/` directory empties. The Rust `pointer!` proc-macro lives here. The TS proc-macro lives at `crates/path-ts/` because Rust toolchain forbids proc-macro path-dep sharing — this is a Rust limitation, not a boundary failure. A `crates/path-core/` (non-proc-macro) crate may exist to share the path-AST + compile logic between the two proc-macro shells; if so, it is the only deduplication mechanism allowed. Any plan that names `crates/bbnf-path/` (with prefix) is stale; any plan with three proc-macro shells is a fault.

8. **Surpass sonic-rs / simdjson / lightning-css**. AU is never mentioned. Every perf gate names a specific competitor's number on a specific dataset on a specific platform. simdjson On-Demand 7 GB/s (JSON parse). sonic-rs M1 Pro twitter 436 µs (parse-to-typed-struct). lightning-css 4.16 ms Bootstrap (CSS). Plans that reference AU's bench numbers are stale.

9. **Slice-borrow primary; bumpalo + owned escape hatches**. Default API is `&'i str` slices + `Cow<'i, str>` for transformations (lightning-css model). Bumpalo arena is opt-in via `parse_in(input, &bump)` (sonic-rs model). Owned (no-borrow) is opt-in via `parse_owned(input)` (serde-json escape). The three are surfaces over the same parse implementation; the lifetime parameter is the discriminant. Any plan that allocates eagerly into bumpalo without justifying why borrowing won't suffice is a fault.

10. **Pratt + SIMD auto-detected**. No `@pratt` or `@simd` directives. Optimizer mines grammar shape (left-recursive operator chains → Pratt) and leaf-pattern shape (charclass / keyword set / regex → SIMD scanner) and emits accordingly. Cost model decides when SIMD overhead is worth the dispatch cost. Any plan that requires grammar authors to annotate Pratt or SIMD is a fault.

11. **Path-deps for incubating sister crates**. egraph + egraph-derive + csp-solver + bbnf-regex + parse-that as path-deps in workspace until each API stabilises; promote to registry once stable. simd-scan + bootstrap + analysis + lsp stay workspace-internal. ser + gorgeous archive at `archive/<crate>/`, removed from workspace, source preserved.

12. **ser + gorgeous archive BEFORE BA.W0**. Clean slate is the precondition for the BA tranche to begin. Any plan that interleaves the archive ceremony with BA waves is a fault.

13. **No god directories; cohesive encapsulation at every level**. Every directory partitions one cohesive concern; siblings are peer partitions of that concern; sub-modules express finer partitions. Per-level surface APIs are uniform across siblings. The standard is set by sonic-rs (`src/{parser, value, serde, util, lazyvalue, ...}`), lightning-css (`src/{rules, properties, selector, declaration, traits, ...}`), and simdjson (`{dom, ondemand, generic, ...}`): each top-level directory names a concern, expresses it through 4–10 children at the next level, and each child carries a uniform sub-API (e.g., every property module exports `Property` enum + `parse` + `print` + `Visit` impls). bbnf must match this discipline. A 16-sibling directory mixing per-grammar subdirs with generic mechanism files (e.g., today's `crates/core/src/runtime/`) is a god directory and is a fault. Files >500 LOC outside `generated/` are forbidden; directories with >10 immediate children mixing concerns are forbidden; sibling-API divergence (one module exports `parse` + `emit`, the next exports `compile` + `walk`) is forbidden.

14. **Full grammar generalisation; zero overfitting**. The substrate carries ZERO grammar-specific code. Every grammar plugs into the fleet via three declarative surfaces only: (a) a grammar source file (`<name>.bbnf`), (b) workspace metadata declaring its strategy (recognisers, host fns, output-dir, pratt eligibility, simd eligibility, etc., per Lock 5's IR contract), and (c) optionally a per-grammar declaration crate (`crates/<grammar>/`) carrying host-fn implementations. Generic crates — `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`, `bbnf-ir`, `path`, `path-core`, `egraph`, `csp-solver`, `bbnf-regex`, `parse-that`, `simd-scan`, `analysis`, `lsp` — carry ZERO `match grammar { Json => ..., CssL4 => ..., ... }` arms; ZERO grammar-named modules; ZERO grammar-specific types in their public APIs; ZERO per-grammar feature flags. Per-grammar runtime modules (value, document, view, kind) are emitted from a single grammar-agnostic generator template that consumes (grammar source + workspace metadata) and produces typed Rust; hand-written per-grammar runtime files are forbidden. Per-grammar deviations (CSS L4 colour-function emit; BBNF Pratt operators; Sheets array literals) are encoded in the grammar metadata + source, NOT in branching code in any other crate. Adding a new grammar is a config + grammar-source change with NO code change in any generic or other-grammar crate. The current overfitting mess — CSS L4 14-variant `OpenFrame`; BBNF aggregator `pub use bbnf::*`; Sheets arena fallbacks; per-grammar registry arms in `bbnf-ir`; `shape_dict_bbnf.rs`; `crates/core/src/css_types.rs`; per-grammar runtime/<g>/ hand-written modules — is the failure mode this lock prevents from recurring. Any plan, tranche, wave, or commit that introduces grammar-specific code in a generic crate, or any new hand-written per-grammar runtime file, is a fault regardless of its other merits. Verification commands: `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,bbnf-regex,parse-that,simd-scan,analysis,lsp}/src/` returns ZERO; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO per-grammar dirs (all generated from template); `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' crates/` returns ZERO matches in non-generated source under generic-crate paths.

## Lanes

Produce one document per lane plus a synthesis. Each lane has scaffolded items the auditor must produce.

### Lane 1 — Lock-Adherence

Walk every plan document. For each of the twelve locks, cite path:line in the plan where the lock is honored, and path:line where it is violated (or absent). For violations, recommend the surgical edit that closes the violation.

For each lock, end with a verdict: **honored / violated-with-recommendation / silent (must add)**.

### Lane 2 — Sequencing Discipline

The Era V failure mode (substrate-then-substrate-then-ship, never substrate-then-consumer-then-ship) was the genesis of seven dead substrate crates between AV and AX. The new plan must not repeat it.

For every wave in BA / BB / BC:
- What does this wave produce?
- Who consumes it, and when?
- If the consumer arrives in a later wave, is the substrate compileable + tested + benchable in this wave's gate?
- If the consumer never arrives in the plan, why is the substrate landing?

Flag any wave whose deliverable lands without a same-wave or next-wave consumer. Recommend either: (a) add the consumer to the wave, (b) merge the wave into the consuming wave, (c) cut the substrate from the plan.

### Lane 3 — Cohesion

Every wave's exit-criteria must be achievable from prior waves' outputs. Walk the wave dependency graph:
- W0 produces X. W1 consumes X via Y mechanism. Is Y specified?
- W1's exit-criteria reference Z. Is Z produced by W0 or W1's body?
- Are any wave's gates impossible to verify from artifacts the wave creates?

Identify orphan exit-criteria (gates that test invariants no wave produces) and orphan deliverables (wave outputs no later wave consumes).

### Lane 4 — SOTA Anchoring

For every perf gate in the plan, verify it cites a specific SOTA number with platform + dataset:

- ✓ "≤ 500 µs to parse twitter.json on M1 Pro, beating sonic-rs's 436 µs"
- ✗ "≥ AU bench parity"
- ✗ "≥ baseline"
- ✗ "≥ pre-W3"

Flag any gate that does not name a competitor's number. Recommend the specific number to substitute, sourced from `audit/SOTA-2026-05-03.md`.

### Lane 5 — Grammar-Authoritative Discipline

Per-grammar code in supposedly-generic crates is the GESTALT § grammar-authoritative violation. Walk the plan for any wave deliverable that:
- Hardcodes grammar idents in `bbnf-ir`
- Adds per-grammar match arms in non-codegen files
- Adds per-grammar feature flags
- Names a module after a grammar

Also: walk the plan for any wave that does NOT excise the existing violations enumerated in `audit/CENSUS-2026-05-03.md` §2 (css_types.rs, ir/registry/strategy.rs:130-185, ir/passes/audit/payload_coverage.rs:69, ir/passes/recognizers/shape_dict_bbnf.rs).

Recommend per-violation surgery; recommend tranche-and-wave for each excision.

### Lane 6 — Generated-Code Budget

Per-tranche LOC budget for `crates/core/src/grammar/generated/`. The current 168 K LOC across 9 grammars is the starting point. Layout lowering may grow some files (typed-enum variants explode) and shrink others (dispatch indirection retires).

For each wave:
- Does it grow generated LOC? Estimate.
- Is the growth justified (typed payloads carrying real data) or accidental (generator regression)?
- Is there a per-wave budget check in the gate?

Flag any wave that is silent on generated-code impact. Recommend a budget check (e.g., "css_l4.rs ≤ 110 K LOC; bbnf.rs ≤ 22 K; net delta ≤ +5%").

### Lane 7 — Friction Forecast

Forecast where users and grammar authors hit the new API and do not understand it. For each:
- The API surface (signature + docstring as planned)
- The user mental model required
- The point of greatest confusion
- The educational artefact the plan should produce (cookbook entry, doc page, error message hint)

Specifically forecast friction at:
- `pointer!["a", "b", 1]` syntax (compile-time path AST)
- `parse(input)` vs `parse_in(input, bump)` vs `parse_owned(input)` (lifetime escape hatches)
- Layout lowering errors (rule X has no resolvable layout because Y)
- Pratt auto-detection misfiring on a grammar shape the optimizer should not have classified as Pratt

Recommend at least three error messages (verbatim) the plan should commit to.

### Lane 8 — Carry & Deferral Audit

Every plan item deferred to a later tranche must:
- Name the receiving tranche (no "future tranche", no "AZ-V" fictional successors)
- State what blocks it from this tranche (specific dependency)
- Have a corresponding gate in the receiving tranche

Walk every "deferred to BB", "carry from BA.W3", "see BC.W1" in the plan. Verify each:
- Names a real, drafted tranche
- States the blocker concretely
- Lands in the receiving tranche's gate list

Flag every dangling carry. Recommend either: (a) move forward into current tranche, (b) explicit landing in receiving tranche's W?.M? gate, (c) cut entirely.

## Invariants of the audit

§1. **No metalanguage in audit docs**. Reference plan content by path:section (e.g., "BA.W2.M3 fails because…"); never reference commits, conversation history, or the plan's draft history.

§2. **Audit voice is direct, archaic-permissive**. Match the project's voice ("hereupon", "begotten", "thereof"). Avoid corporate hedging ("might want to consider"). State faults directly.

§3. **Citations are path:line, not paraphrase**. Every claim about the plan cites where it lives. Every claim about the codebase cites where it lives.

§4. **Recommendations are surgical**. Not "improve cohesion"; instead "merge BA.W4 into BA.W3 because W4 has no consumer of W3's output that BA itself uses; relocate W5 ↑ to fill the slot".

§5. **Verdicts are ratifiable or actionable**. Not "this is concerning"; instead "honored", "violated-with-rec-X", or "silent-must-add-Y".

## Execution discipline

§ED1. Each lane is one document. Filename: `audit/HARDENING-PLAN-2026-MM-DD-NN-<lane-slug>.md`.

§ED2. Plus one synthesis: `audit/HARDENING-PLAN-SYNTHESIS-2026-MM-DD.md` referencing each lane and tabulating cross-lane verdicts.

§ED3. Lanes can dispatch in parallel; synthesis lands after.

§ED4. HARD CAP per lane: 25 minutes. Synthesis: 15 minutes. At 0.9N commit progress, at N halt and report.

§ED5. The synthesis ends with a punch list: ordered, surgical, ready to act on. Each item names its plan-doc target and the specific edit.

§ED6. No hedges. No "consider". No "might". The plan is either right or wrong on each lane.

## Voice locks

§V1. Archaic diction is welcome.

§V2. State the fault. State the surgery. Move on.

§V3. The auditor is not a collaborator on the plan; the auditor is its first adversary. Ratify what survives; cut what doesn't.

§V4. No restating of the plan back to the user. Cite path:line and proceed.

## Failure modes to avoid

D1. **Restating the plan as audit**. The audit document recapitulates the plan in its own voice instead of identifying faults. Symptom: "BA.W2 plans to do X; this is good because Y." Audit is not summary.

D2. **Soft verdicts**. "Could be tightened", "may benefit from review". Either it's a fault (with surgery) or it's not.

D3. **Paragraph-level critiques**. "The optimization layering section needs more depth." Cite the line; specify the addition.

D4. **Ignoring locks**. The twelve locks above are settled. The audit does not relitigate them; it verifies the plan honors them.

D5. **Carry-blindness**. Treating every "deferred to BB" as legitimate without auditing whether BB has the gate. Era V's failure mode replicated.

D6. **Friction-vagueness**. "Users may find this confusing." Specify the user, the mental model, the point of confusion, the verbatim error message.

D7. **SOTA-erasure**. Accepting "≥ baseline" as a perf gate. Every gate names a competitor's number.

D8. **Genericity-erasure**. Accepting per-grammar code in generic crates because "the plan says we'll fix it later". Cite the planned fix or flag the deferral.

## Reading list (in order)

1. `docs/tranches/BA/BA.md`
2. `docs/tranches/BA/waves/*.md`
3. `docs/tranches/BB/BB.md`
4. `docs/tranches/BC/BC.md`
5. `docs/tranches/BD/BD.md` if drafted
6. `audit/HARDENING-SYNTHESIS-2026-05-03.md`
7. `audit/SOTA-2026-05-03.md`
8. `audit/CENSUS-2026-05-03.md`
9. `audit/MODULES-2026-05-03.md`
10. `audit/RESTART-SKETCH-2026-05-03.md`
11. `docs/tranches/meta-audit/archaeology/era-IV-tape-first.md` — tape arc archaeology (peak)
12. `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` — substrate-first/consumer-later failure-mode anatomy
13. `docs/tranches/AV/research/04-columnar-soa.md` — kind-partitioned columnar SoA spec (designed, never activated; cited so the auditor can verify Lock 1 is honoured)

## Methodology

Per lane:

1. Open the plan documents (subjects).
2. Walk the lane's question with the plan in front of you.
3. For every claim, cite path:line.
4. For every fault, recommend the surgical edit.
5. End with a per-lane verdict tabulation: items honored, violated, silent.
6. Commit the lane document.

Synthesis:

1. Read all eight lane documents.
2. Tabulate cross-lane verdicts (the same plan section may be honored on Lane 4 and violated on Lane 6).
3. Produce a punch list — one entry per surgery, in execution order, with target path:line and verbatim edit.
4. Recommend whether the plan is ready to execute, ready after surgery, or requires re-draft.

## Provenance

This prompt is for hardening the BA-restart plan set drafted after the 2026-05-03 codebase audit. The twelve locks are user verdicts on the architectural commitments that govern BA-restart. Do not relitigate the locks; verify the plan honors them.

The codebase-side companion prompt is `docs/HARDENING-AUDIT-PROMPT.md` (audits the codebase, not the plan).
