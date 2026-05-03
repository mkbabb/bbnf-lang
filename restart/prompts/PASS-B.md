# Pass B — Codegen + Runtime + Optimisers (Greenfield Restart)

You are the orchestrator for Pass B. You cover ~1/3 of the project — the codegen / runtime / optimiser middle of the pipeline: the codegen substrate, the runtime substrate, the pipeline coordinator, the generated grammar output tree, the e-graph + CSP solver + SIMD scanner sister crates, and xtask. The other 2/3 are covered by Pass A (parse front) and Pass C (periphery + tooling + docs + commit chain).

You dispatch six agents in parallel, each applying the **keep outright / keep but modify / abrogate (delete / move / replace)** rubric through a distinct lens. You synthesise.

## Required reading (mandatory; in order)

**Suite directives:**
1. `/Users/mkbabb/Programming/bbnf-lang/docs/restart/README.md`
2. `/Users/mkbabb/Programming/bbnf-lang/docs/HARDENING-PLAN-PROMPT.md` — the 14 locks
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/CONSUMING.md`
5. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md`

**Prior audit corpora (ground truth):**
6. `/Users/mkbabb/Programming/bbnf-lang/audit/CENSUS-2026-05-03.md`
7. `/Users/mkbabb/Programming/bbnf-lang/audit/MODULES-2026-05-03.md`
8. `/Users/mkbabb/Programming/bbnf-lang/audit/RESTART-SKETCH-2026-05-03.md`
9. `/Users/mkbabb/Programming/bbnf-lang/audit/SOTA-2026-05-03.md`
10. `/Users/mkbabb/Programming/bbnf-lang/audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md`
11. `/Users/mkbabb/Programming/bbnf-lang/audit/PHASE-4-SYNTHESIS-2026-05-03.md` (partial; 577 lines authoritative)
12. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-IV-tape-first.md`
13. `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`

## Pass B Scope

### B.1 — Codegen substrate
- `crates/core/src/codegen/` — entire subtree (every emitter; rust/, ts/, wasm/ if extant)
- `crates/core/src/codegen/rust/` — including struct_direct, dispatcher, alt_dispatch, pratt, regex
- `crates/core/src/codegen/<other-backends>/` — TS scaffold, WASM scaffold, anything else extant

### B.2 — Runtime substrate
- `crates/core/src/runtime/` — every per-grammar runtime dir (the Lock-13 god-directory under audit)
- `crates/core/src/runtime/<g>/{builder, document, view, kind, value, mod}.rs` per grammar — currently 9 grammars duplicating shape
- `crates/core/src/runtime/error.rs` (or wherever runtime errors live)
- `crates/core/src/runtime/<shared>/` — generic mechanism files; this is the cross-cut to identify

### B.3 — Pipeline + grammar generated output
- `crates/core/src/pipeline/` (or `pipeline.rs` + `pipeline/` collision per Phase-4 W0 surgery)
- `crates/core/src/grammar/generated/` — 168,750 LOC across 9 grammar files; this is the output of the entire codegen path
- `crates/core/src/grammar/<source-side>/` if any non-source / non-generated files live here

### B.4 — Optimiser sister crates
- `crates/egraph/` + `crates/egraph-derive/` — entire crates
- `crates/csp-solver/` — entire crate
- `crates/simd-scan/` — entire crate (the byte-disjoint Alt + structural-alphabet scanner)

### B.5 — xtask
- `xtask/` — entire crate; the regen / check / test / bench harness
- `xtask/src/regen.rs` — codegen invocation
- `xtask/src/bench.rs` (or wherever benches live within xtask)
- `xtask/Cargo.toml`

### B.6 — Generated-output budget evidence
- `find crates/core/src/grammar/generated -name '*.rs' | xargs wc -l` baseline
- The pre-Phase-4 168,750 LOC distribution

**Out of Pass B scope:** parse, lower, IR, source, host, path, bootstrap, parse-that, bbnf-regex, grammar/ source tree (these are Pass A); analysis, lsp, ser, gorgeous, docs, audit, scripts, sibling repos, commit chain (these are Pass C).

## Methodology — Six Agents in Parallel

You dispatch the six agents below. Each writes to `audit/restart/per-agent/pass-b-agent-{N}-{lens}.md`. Each: 30-min hard cap.

### Agent B.1 — Inventory

**Lens.** Exhaustive catalogue of every file in Pass B scope.

Critical sub-tables:
- Per-emitter shape inventory (struct_direct, dispatcher, alt_dispatch, pratt, regex, ts, wasm) — current LOC, public API, internal sub-modules
- Per-grammar runtime inventory — LOC per `<g>/{builder, document, view, kind, value, mod}.rs` × 9 grammars = 54-72 files
- Generated-output inventory — per-grammar LOC: bbnf.rs, css_l4.rs, json.rs, etc.
- Optimiser-crate inventory — egraph public API, egraph-derive macro surface, csp-solver public API, simd-scan public API
- xtask command inventory — every subcommand the xtask exposes

**Output**: `audit/restart/per-agent/pass-b-agent-1-inventory.md`, ~600-1000 lines.

### Agent B.2 — Idiomaticity (precepts adherence)

**Lens.** Apply every precept under `docs/precepts/` to every Pass-B file. Per file, identify violations. Critical foci:

- **direct-to-struct** — every emit shape MUST produce typed enums + parse fns; Tape / OpenFrame / variant-strip residue is fault
- **no-orthogonal-codepaths** — multiple code paths for "emit an Alt" / "emit a Repeat" are fault; the cost-model decides between paths but they share substrate
- **system-cohesion** — codegen + runtime + optimisers must share types; multiple `Layout` definitions are fault
- **gestalt approach** — workarounds, fallbacks, "scalar fallback for SIMD", "Vec fallback for arena" are fault unless gated by the cost-model
- **single-codegen-path** — ONE codegen path (Lock 1: direct-to-struct only); combinator fallback / tape fallback / EmissionTier residue are fault

**Output**: `audit/restart/per-agent/pass-b-agent-2-idiomaticity.md`, ~600-1000 lines.

### Agent B.3 — Lock-adherence

**Lens.** Apply the 14 locks per Pass-B file. Critical foci:

- **Lock 1** (tape + columnar dead) — the MOST consequential lock for Pass B. Search every Pass-B file for `TapeRec`, `TapeCursor`, `payload_idx`, `OpenFrame`, `FusedBuilder`, `payload_arena`, `column_*`, `TapeKind`. Every match is fault unless the file is `archive/`-bound.
- **Lock 4** (per-domain orthogonal optimisation) — egraph + csp-solver + miners + cost-model must compose by output-piping; no unified hypergraph. Read each crate's public API; verify no fused entry-point.
- **Lock 5** (IR + per-backend lower) — codegen must consume typed IR (per Phase-4 BC.W0 spec at `docs/tranches/BC/audit/W0-typed-ir-variant-table.md`), NOT walk grammar IR directly. If any emitter calls `grammar.rules.iter()`, fault.
- **Lock 6** (xtask emits committed source artefacts) — proc-macro façades for codegen output are fault. Verify xtask's regen output is committed.
- **Lock 9** (slice-borrow primary) — the runtime's `<G>Value<'i>` types must default to `&'i str` slices; bumpalo arena must be opt-in via `parse_in`; eager arena allocation in default `parse(input)` is fault.
- **Lock 10** (Pratt + SIMD auto-detected) — no `@pratt` / `@simd` directives; cost-model decides. Verify any "force-Pratt" / "force-SIMD" code paths are absent.
- **Lock 13** (no god directories) — `crates/core/src/runtime/` with 9+ per-grammar children mixing concerns is the archetype god-directory. Identify every Pass-B god-directory.
- **Lock 14** (full grammar generalisation) — ZERO grammar-specific code in generic crates. EVERY per-grammar runtime dir is a Lock-14 violation; every match-arm over grammar idents in codegen / pipeline is fault.

**Output**: `audit/restart/per-agent/pass-b-agent-3-lock-adherence.md`, ~700-1100 lines.

### Agent B.4 — Architectural Transposition

**Lens.** Macro-level restructuring proposals.

Concrete questions:
- The `crates/core/src/codegen/` + `runtime/` + `pipeline/` together carry ~10,000+ LOC. Should `codegen/` become a `crates/bbnf-codegen/` crate per Phase-4 BC.W3? Should `runtime/` become a `crates/bbnf-runtime/` crate?
- Per-grammar runtime modules (`runtime/<g>/`) violate Lock 14. Should EVERY per-grammar runtime module retire and emit from a single `bbnf-runtime-template`? Where does the template live? How is it consumed (proc-macro / xtask)?
- The optimiser sister crates — egraph, csp-solver — currently are workspace-internal. Should they become crates.io-ready libraries (Lock 11 promotion)? What is the API-freeze checklist?
- simd-scan is workspace-internal. Lock 11 keeps it internal; is that right, or should it promote?
- xtask currently bundles regen + bench + check + test. Should it split (xtask-regen, xtask-bench, xtask-check)? Or stay monolithic?
- The generated-output tree at `crates/core/src/grammar/generated/` — should it relocate to `crates/bbnf-parse/src/parse/generated/` per Phase-4 surgery 22? Or to per-grammar declaration crates `crates/<grammar>/generated/`?

For each macro-proposal: current state (path:line) | proposed shape | locks honoured | migration cost | carry to synthesizer.

**Output**: `audit/restart/per-agent/pass-b-agent-4-architectural-transposition.md`, ~600-1000 lines.

### Agent B.5 — Replacement Design

**Lens.** For every ABROGATE-REPLACE item, design the new facility. Plus: identify brand-new items needed.

Brand-new items the agent should consider:
- A unified **`bbnf-runtime-template`** mechanism (per Lock 14): one template generates per-grammar runtime modules from (grammar source + workspace metadata). Where does it live? Proc-macro / xtask / build script?
- A **`bbnf-codegen-IR`** crate carrying the typed IR shared across Rust + TS + WASM emitters (per Phase-4 BC.W0 22-variant table)
- An **`Emitter` trait** with `RustEmitter` / `TsEmitter` / `WasmEmitter` impls (per Lock 5)
- The **cohort template generator** (per Phase-4 gap D) — proc-macro2 + quote sketch
- A **`bbnf-bench`** harness separate from xtask, with vitest-style `bench()` API (per `feedback_vitest-bench`)
- A **`bbnf-cost-model`** crate carrying the cost-model type + extraction logic (post-egraph)
- A **`bbnf-pratt`** crate carrying the Pratt LUT propagation + auto-detection (currently embedded in the emitter)
- A **`bbnf-simd-detect`** crate or pass that auto-detects SIMD-eligible leaves from grammar shape

For each: name | location | API sketch | locks honoured | migration sequence.

**Output**: `audit/restart/per-agent/pass-b-agent-5-replacement-design.md`, ~600-1000 lines.

### Agent B.6 — Cross-cut Analysis

**Lens.** Cross-file concerns. Critical foci:

- **Codegen-runtime coupling**: which codegen-emitted symbols depend on runtime-defined types? If `bbnf-codegen` and `bbnf-runtime` split, what is the contract?
- **Optimiser fan-in / fan-out**: which optimiser-crate functions consume codegen-IR types? Which produce types that codegen consumes?
- **Tape residue across crates**: per CENSUS, `TapeRec` references are scattered across runtime, codegen, and possibly bbnf-ir. Surface every match.
- **OpenFrame migration completeness**: post-Phase-4 BA option-(a), all 9 grammars retire OpenFrame. Verify no residue remains in Pass-B scope.
- **Cost-model-egraph coupling**: should the cost model live in egraph (extraction-side) or as a separate crate?
- **Generated-output as substrate**: 168,750 LOC of generated/ files have implicit invariants (trait conformance, visibility, span resolution). Surface the invariants the codegen path silently relies on.
- **xtask cross-cut**: xtask depends on `bbnf-codegen` (to regen) and `bbnf-bench` (to bench). What is the dependency arrow? Is it acyclic?

**Output**: `audit/restart/per-agent/pass-b-agent-6-cross-cut.md`, ~600-1000 lines.

## Synthesis (orchestrator's output)

After all six agents commit, you produce `audit/restart/PASS-B-2026-MM-DD.md`, ~1500-2500 lines, structured §1-§8 per the Pass-A skeleton (verdict ledger / architectural transpositions / new facilities / cross-cuts / pass-residues / lock+precept verdicts / punch list / greenfield commitments).

Key Pass-B-specific items in the verdict ledger:
- Per-grammar runtime dirs (9 grammars) — bucket likely ABROGATE-REPLACE → consolidated runtime template
- 168,750 LOC generated tree — bucket KEEP-MODIFY (relocate to new crate per architectural transposition)
- Per-emitter shape modules — bucket varies; KEEP-OUTRIGHT for shapes that survive direct-to-struct unification, ABROGATE for tape-coupled shapes
- egraph / csp-solver / simd-scan crates — bucket likely KEEP-MODIFY (API freeze + crates.io readiness per Lock 11)
- xtask — bucket likely KEEP-MODIFY (split or refactor per architectural-transposition)

## Voice + discipline locks (universal)

(Per `docs/restart/README.md` §Voice. Calibrated, archaic-permissive, no metalanguage, path:line citations, no quick solutions, no workarounds, idiomatic gestalt.)

## Hard cap

You: 60 min. Each agent: 30 min. Cumulative wall: ~90 min.

## Output contract

Per-agent: `docs(audit/restart/pass-b/agent-{N}): {lens} of Pass-B scope`.
Orchestrator: `docs(audit/restart/pass-b): synthesise Pass-B — codegen + runtime + optimisers`.

## Cross-tranche scope boundary

Touch ONLY `audit/restart/`. Do NOT modify `crates/`, `docs/tranches/`, `docs/precepts/`, `docs/restart/`. Do NOT modify Phase-4 directive.

## Background

Pass B is mid-pipeline. Pass A produces IR; Pass B consumes IR + emits Rust source + carries the runtime substrate; Pass C audits everything else. The synthesizer reconciles cross-pass.

The greenfield mandate is the substrate identity: codegen + runtime + optimisers should be entirely re-shaped if the audit demands it. Lock 1 (tape dead) and Lock 14 (full grammar generalisation) are the most consequential here. The 168,750 LOC generated/ tree is largely Lock-14-questionable today — every per-grammar file is a candidate ABROGATE-REPLACE-via-template.
