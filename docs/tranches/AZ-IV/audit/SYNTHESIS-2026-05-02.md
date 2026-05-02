# AZ-IV Mid-Tranche Synthesis — Recap, Path Forward, Generalized-Grammar Optimum

**Date**: 2026-05-02
**Synthesis from**: 6-agent audit cohort (A/B/C surgical + D/E/F analytical)
**Inputs**:
- `audit/AUDIT-2026-05-02-mid-tranche.md` — orchestrator's W0+W1+W2 close audit (10-section)
- `audit/AUDIT-A-legacy-2026-05-02.md` — legacy / fallback / workaround excision
- `audit/AUDIT-B-arch-2026-05-02.md` — encapsulation / DI / god-module split
- `audit/AUDIT-C-kiss-dry-2026-05-02.md` — KISS / DRY / antipattern excision
- `audit/AUDIT-D-recap-2026-05-02.md` — comprehensive recap of plan vs reality
- `audit/AUDIT-E-pathforward-2026-05-02.md` — W3-W6 + post-AZ-IV path forward
- `audit/AUDIT-F-optimum-2026-05-02.md` — generalized-grammar-optimum architectural critique

## §0 Verdict

The AZ-IV thesis holds. None of the 33 non-routable carries require thesis amendment. The 13 chronic deferrals (Boole's hardening pass) divide cleanly into "the system isn't shaped to absorb it" (W3-W6 architectural transpositions) and "the work is just hard" (measurement + perf engineering). All current carries have a defensible AZ-IV-internal path.

W3 dispatches next from base `1ae8e9e2`, taking on (a) the W2 path-egraph-seed carry, (b) the load-bearing PathSchema unification + lazy bail-out parse, and (c) wave-bridging bench snapshots adopted by AUDIT-E §5.

## §1 Recapitulation — Original Plan + Precepts

(Detail at `audit/AUDIT-D-recap-2026-05-02.md` §1.)

**AZ-IV thesis** (paraphrased): the system already chose the right architecture (grammar-derived StructDirect + canonical regen + CSP/egraph/Pratt/regex/SIMD substrate); AZ-IV is the union tranche that consumes or deletes every existing substrate, eliminates every overfit, lands the typed compile-time `path!` macro and the path-driven lazy recognizer, executes the TS binding to parity, and redresses every failing test. *No parallel parser, no shadow path system, no unconsumed substrate, no chronic deferral.*

**Four interlocking invariants** (GESTALT §2):
1. **Typed materialization** — every `->` reaches the emitter
2. **No orthogonal codepaths** — singular collection strategy; one regex system; no combinator fallback
3. **Direct-to-struct** — no intermediate untyped phase
4. **Grammar-authoritative** — `->` annotations own leaf semantics; host fns cover context-dependent

**Load-bearing user-facing precepts** (from precepts/instructions + memory):
- KISS / DRY / no-workarounds / no-fallbacks / no-special-cases
- No grammar overfitting (no rule-name string matching in production runtime)
- No silent fallback (panic on misuse with named binding string)
- No god modules (>500 LOC files broken into cohesive sub-modules)
- No inline tests in src/ (move to tests/)
- One codegen path (`feedback_one-codegen-path`)
- Substrate with consumer (zero-caller `pub` substrate fails the build at W5 audit)
- Failing-test census 100 % pass; every `#[ignore]` carries owner+deadline+reason+ticket

## §2 Status Hitherto — What Landed in W0+W1+W2

(Detail at `audit/AUDIT-2026-05-02-mid-tranche.md` §1-§4 + `audit/AUDIT-D-recap-2026-05-02.md` §3.)

**Numerics**:
- 91 commits since tranche open (`2678ed44` → mid-tranche audit at `10ac5448`)
- +33,490 / -3,161 LOC across 245 files
- Workspace nextest: 1527 → 1582 passed (+55 new tests); 78 → 0 failures
- regen --check: 9/9 GREEN at every wave close

**Closures**:
- W0: regen 9/9, manifest binding scaffold, `Map { fn_id }` extractor pin, csp-solver canonicalised, derive eradicated, dev-baseline + generated-size budget MET, REGEN triumvirate (research + plan + 2 redress passes)
- W1: 7 `from_rule_name` impls retired (Path B `from_rule_id(u32)`), `view/color` shim deleted (290 LOC), substrate panic + manifest strategy live, `recover_*`/byte-recovery deleted, alt_dispatch typed-leaf push active, no-grammar-name-branch CI scan landed; W1-CLOSE triumvirate drove 13 → 0 residual failures
- W2: Path IR + path lexer (186 LOC, in parse-that path-patched) + `path_check` IR pass + InlineTrace sidecar + `path!` proc-macro + AscentStrategy (3 impls; HybridSidecar default per micro-bench) + Wildcard lazy-iter + variant-select resolver

**Surgical sweeps (A/B/C this cohort)**:
- Inline tests relocated from src/ to tests/ (substrate_path_resolve.rs, substrate_path_tokens.rs)
- `recognize_*_legacy` → `*_pattern` (W4-territory work pre-completed)
- Dead code excised: `is_term_kind`, `is_grouped_term`, dead bindings + getter discards
- DRY: `DIAGNOSTIC_SOURCE` constant extracted across 11 files (18 inline `"bbnf".into()` callsites)
- God-module split: `pipeline/compile.rs` (1049 → 6 sub-files), `google_sheets/document.rs` (732 → 4 sub-files)
- Legacy excision audit roster: 8 substrate-without-consumer items + 13 production `eprintln!` sites + 935 walker/tape doc-scrubs routed to W4

## §3 Generalized-Grammar Optimum — Where We Are vs Where We Aim

(Detail at `audit/AUDIT-F-optimum-2026-05-02.md`.)

### What generalizes cleanly today
- Path subsystem: `TypedPath<G, T>` + `path!` macro + IR `path_check` after `project_types` + InlineTrace — the Path IR consumes only `StructRegistry` and grammar markers; per-grammar paths derive from `->` annotations alone
- AscentStrategy as reversal seam: 3 impls (Root/InStruct/HybridSidecar) plug-in via type alias; default chosen by micro-bench artefact, not arm-list
- alt_dispatch typed-leaf push: pushes modifier+operator tokens as typed Span leaves; lower deletes its 3 source-byte recovery functions; mechanism is grammar-general (no rule-name compares)
- `EmitStrategy::for_grammar` post-W1.8: manifest-driven binding registry; synthetic-grammar test proves no Rust arm needed
- W0.4 cost-extractor: `Map { fn_id }` preserved through saturation via 1e6 preserve-bonus; no rewrite-language change; cost-side discipline only

### Where the seam was moved, not eliminated (AUDIT-F finding T1)
W1's Path B replaced `from_rule_name(&str)` with `from_rule_id(u32)` per-grammar match expressions. The integer-literal form satisfies the static `no_grammar_name_branch.rs` CI scan (which greps for *string* literals in match arms) but **8 grammars still carry per-grammar discriminator match expressions**. **T1**: lift to `StructRegistry::compound_kind_for_layout(layout) -> CompoundKindId` so the discriminator is registry-projected, eliminating the seam. Lands W4.

### Where overfitting still bites
- CSP authority remains partial: `alt_strategy.rs:160-184` still re-overrides the CSP-chosen `AltMode` via `key_dispatch_configs.contains_key(id)` sidecar lookup. Babbage Hard Finding 7 names this. **T9** (unified decision stream `ir.decisions`) collapses sidecars into one stream, dissolving the override seam.
- Inline-trace recording: layered as `_with_trace` wrappers over bare passes. **T3** (canonicalise as `&mut dyn TraceSink`) deletes the duplication.
- bbnf-path synthetic registry fixture: `crates/bbnf-path/src/registry.rs` carries hand-authored fixtures mirroring 4 grammars. **T4** (codegen emits `pub const REGISTRY: StructRegistry`) closes mid-tranche §R1.
- Generated code monolithic dumps: css_l4 = 85 KLOC, bbnf = 17 KLOC. **T6** (module-split output: per-concern files) aligns with `feedback_generated_size_budget` and compounds for every successor wave.

### Top-5 architectural transpositions (F §7)

1. **T1 — Registry-projected compound discriminator** (W4) — eliminates the moved-seam from W1
2. **T9 — Unified decision stream `ir.decisions`** (W4) — collapses CSP-authority override sidecars
3. **T3 — Inline-trace canonicalisation as `&mut dyn TraceSink`** (W4) — deletes `_with_trace` wrapper duplication
4. **T4 — `bbnf-path` synthetic fixture → production const** (W4/W5) — closes §R1
5. **T6 — Generated module-split output** (W4/W5) — eliminates the monolithic-dump generated tree

## §4 Path Forward — Refined W3-W6 + Post-AZ-IV

(Detail at `audit/AUDIT-E-pathforward-2026-05-02.md`.)

### W3 — Lazy Bail-Out Parse (next)

**Critical-path**: PathSchema unification (single trait abstracting `TypedPath<G, T>` and future schemas) + per-rule `(rule, segment_kind) -> {ParseFully, ParseUntil(child_index), Skip}` decision table at codegen + lazy entry-point `parse_with(input, &path)` on JSON / CSS L4 / Sheets / BBNF.

**Carries absorbed**: W2.md Hard Gate 9 (path-egraph-seed at `crates/ir/src/rewrites/path_seed.rs`) — small absorption at W3 opening; 3 hand-authored rewrites (duplicate-prefix elimination, redundant downcast removal, adjacent-accessor fusion).

**Refinement adopted (AUDIT-E §5)**: wave-bridging bench snapshot at `docs/benchmarks/wave-{tag}-{wave}.json` (small, ≤ 5 rows) so trends visible before W6's full close-matrix. **Default: ADOPT.**

**Hard gate**: same-harness `bbnf_get_twitter` ≤ 5x sonic-rs `sonic_get_twitter` (target ≤ 1.0x routes only with profile evidence per AZ-IV §Hard Gates 16); the W3.5 profiling sub-cohort produces the 7-artefact contract per `PROFILING.md`.

### W4 — Optimization Substrate Activation

**Critical-path** (with absorbed transpositions):
- **T1**: registry-projected compound discriminator (eliminates 8 per-grammar `from_rule_id` matches)
- **T9**: unified decision stream `ir.decisions` (collapses `alt_strategy.rs:160-184` override seam)
- **T3**: inline-trace canonicalisation as `&mut dyn TraceSink`
- **AUDIT-E D2 — RuleSet wire-or-DELETE**: default **DELETE** per `feedback_no-workarounds`. The half-broken substrate is the chronic-defer pattern Boole §b.2 row 14 names; BA W0 recreates clean.
- Substrate-without-consumer roster (8 items): consume or delete each (per §Hard Gate 14).
- Tailwind regex_scan perf timeout (carry from W1 close).
- DTA cleanup: `dfa_codegen.rs` rename to `regex_scan_adapter.rs`; `dta.rs` rename + module-doc rewrite; `emit_dfa_inline_body` deletion.
- 13 production `eprintln!` sites → `tracing` (per `feedback_clean_instrumentation`).
- AUDIT-B routed splits: `dta.rs` (1565 LOC), `csp_strategy/mod.rs` (1316 LOC), `analysis/inline.rs` (673 LOC).

### W5 — TS Binding + Value-API Dedup + Substrate Audit

**Critical-path** (with absorbed transpositions):
- **T4**: `bbnf-path` synthetic fixture → production const wired by codegen
- `crates/bbnf-path-ts/` cdylib + wasm-bindgen + template-literal tag + Node-execute proof
- Per-grammar value-API dedup (structural skeleton; typed `*Value` enums survive untouched)
- Permanent `substrate_audit.rs` test consuming the W4 census tool (AUDIT-E D6 split: W4 lands tool, W5 lands test)
- AUDIT-B routed splits: `css_l4/builder.rs` (1014 LOC), `types/mod.rs` (786 LOC), `css_l4/value.rs` (852 LOC review)

### W6 — Measurement + Close

**Critical-path**:
- post-AZ-IV.json fat-LTO close matrix per `docs/benchmarks/SPEC.md`
- Samply 7-artefact contract per `PROFILING.md` (5 sub-agents on 5 canonical bench harnesses)
- Close-honesty checklist
- 26 `#[ignore]` triplet enumeration (W1 hard gate 3 carry; per-test owner+deadline+reason+ticket)
- 2 timeouts addressed (tailwind perf via W4; LSP completion test root-cause)
- FINAL.md with cited commits and artefacts for every gate

### Post-AZ-IV — Recycled BA + cross-repo motion

**BA (rule discovery)**: opens after AZ-IV close. Ruler CVC enumerator + VM oracle on residue + ranker/tiering + Class-1/2/3 + grammar-colocated rewrite dirs. The W4.1 rewrite-chain delivery seeds this; the substrate W4 may delete (RuleSet) gets recreated clean by BA.

**Cross-repo motion priority** (AUDIT-E §6):
1. **csp-solver repo split** (highest priority — canonical-source policy already operationally complete; the in-tree byte-mirror is mechanically ready)
2. egraph repo split (general-infra crate per `feedback_general-infra-crates`)
3. simd-scan repo split or fold into parse-that
4. bbnf-regex sub-crate of parse-that (already de-facto via path-patch)
5. **xtask rename** — defer indefinitely (AUDIT-E §6 verdict: lowest priority)

## §5 Bench/Test Readiness — Concrete Answer

(Detail at `audit/AUDIT-E-pathforward-2026-05-02.md` §4.)

**Benching**: divan workspace alive at HEAD; `cargo bench-iter-json` works; bench harnesses for json_monolithic / css_l4 / google_sheets_monolithic / bbnf_monolithic / json_value all exist. Wave-bridging bench snapshots can land starting **W3 close** (focused 5-row JSON paths matrix). Full close-matrix is W6.

**Testing**: workspace nextest is at **1582 / 0 / 2 timeouts / 26 skipped** at HEAD. The 2 timeouts (tailwind perf + LSP completion) are W4 carries. The 26 ignored need triplet enumeration at W6. Continuous testing is the operating mode; every wave runs nextest pre-cherry-pick.

## §6 Decision Queue — Items Requiring User Direction

(Detail at `audit/AUDIT-E-pathforward-2026-05-02.md` §8.)

**Top-3 (AUDIT-E surface)**:

| # | Decision | Default | Rationale |
|---|---|---|---|
| D2 | RuleSet wire-or-delete (W4.1 floor) | **DELETE** | Wiring half-broken substrate is the chronic-defer pattern Boole §b.2 row 14 names; BA W0 recreates clean per `feedback_no-workarounds` |
| D5 | Wave-bridging bench snapshots (`docs/benchmarks/wave-{tag}-{wave}.json`) | **ADOPT** | ~30 min/wave × 3 = 1.5 hrs; trend visibility prevents the chronic-defer pattern on watchdog rows |
| D6 | Substrate-audit infrastructure split (T-N from F) | **W4 lands census tool; W5 lands test** | Eliminates the manual-census drift pattern at the source |

Five additional items in AUDIT-E §8 with defaults set; surface only if you want to discuss.

## §7 Risks Carried Forward (mid-tranche audit §7 confirmed)

- **R1** — synthetic registry fixture in `bbnf-path`: closes via T4 in W4/W5
- **R2** — inline-trace pipeline-ordering coupling: closes via T3 in W4
- **R3** — `Color = unknown` TS preamble: closes via W5 cdylib binding
- **R4** — lowering-quartet structural-detection seam: codified at AZ-IV.md §Orchestration Rules item 14; protected by `no_grammar_name_branch.rs` CI scan
- **R5** — bbnf-regex sibling-repo path-patch: lock-pinned via `cargo metadata --locked`; documented in W2-path-lexer.txt

All five have routing or test-harness gates; none block W3 dispatch.

## §8 Synthesis Verdict + Next-Step

**Thesis intact.** The four invariants (typed materialization / no-orthogonal-codepaths / direct-to-struct / grammar-authoritative) hold across W0+W1+W2 work. The transpositions T1, T9, T3, T4, T6 are the load-bearing simplifications still owed; they fit cleanly into W4 + W5 without thesis amendment. Chronic deferrals all have AZ-IV-internal paths.

**Next dispatch**: W3 — Lazy Bail-Out Parse, 5 sub-units parallel:
1. Path-egraph-seed (W2 carry absorption — `crates/ir/src/rewrites/path_seed.rs`)
2. PathSchema executor + cursor + schema trait
3. Per-grammar `parse_with` entry points (JSON / CSS L4 / Sheets / BBNF)
4. Codegen path-plan emitter
5. parse-with test suite + bench harness extensions + profiling

Wave-bridging bench snapshot adopted; produces `docs/benchmarks/wave-AZ-IV-W3.json` at W3 close.

**No surprises. No undocumented gaps. No silent debt.**
