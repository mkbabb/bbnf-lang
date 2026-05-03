# Phase 4 Directive — Specification-Depth Re-Draft

Date: 2026-05-03
Audience: BA / BB / BC / BD tranche re-draft agents + synthesis agent.
Status: settled. The 13 architectural locks ratify the substrate; the audit synthesis at `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md` ratifies the punch-list. This directive composes both with three operational rules and a research mandate, then specifies the re-draft mode.

The plan-set under re-draft is `docs/tranches/{BA,BB,BC,BD}/`. Phase 2 produced the prior draft (4669 lines, 24 files); Phase 3 produced the 9-lane audit (507 lines, 9 files). Phase 4 produces the granular re-draft.

---

## §1 — The Three Operational Rules

These rules govern every wave document in every tranche. Violation is fault.

1. **No "all-X" claims without per-X tables.** Every "all eight remaining grammars" / "every grammar" / "all backends" / "all tests" decomposes into a markdown table with one row per X. Each row carries a per-X gate (cargo command + expected duration + pass condition + LOC delta where relevant).

2. **No "user adjudicates" / "future BD" / "TBD" / "investigate later".** Every deferral has (a) a named receiving tranche + wave, (b) a named blocker (concrete dependency, not "complexity"), and (c) a named receiving gate (the cargo / rg / shell command that proves the deferred work landed). If any of (a) / (b) / (c) cannot be named, the deferral is decided **in-plan** before execution.

3. **Every wave's spec is independently executable.** The wave document must contain enough information that an agent (or a human implementer) can begin work without consulting the audit synthesis, the directive, or the user. If the wave needs an external decision, it is not yet plannable.

---

## §2 — The 35 Surgeries (Mandatory)

The synthesis at `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:39-75` enumerates 35 ordered surgical edits. Each must land. Per-tranche distribution:

### BA (surgeries 1-12, 15-19, 24, 27)

1. `docs/tranches/BA/waves/W5.md:35`, `:99-104` — delete CSS L4 / BBNF / Sheets `OpenFrame` preservation; replace W5.M6 with `rg -n 'enum OpenFrame' crates/core/src/runtime/` returning 0, OR move full migration to BA.W6 with gates.
2. `docs/tranches/BA/BA.md:78`, `docs/tranches/BA/waves/W5.md:146`, `docs/tranches/BB/BB.md:32` — if all-grammar migration stays in BB, BA must mark Lock 1 as **deferred-with-receiver**, not honoured. Preferred: move BB.W1 up.
3. `docs/tranches/BA/waves/W2.md:9-11`, `:56`, `:134` — remove transitional `TypeDesc`/`StructLayout` aliases. W2 close grep returns zero retired terms. No `pub use` exception.
6. `docs/tranches/BA/waves/W3.md:3`, `:7`, `:27-32`, `:36`, `:83-84`, `:96` — remove every stale directory spelling. Use `crates/path`, `crates/path-core`, `crates/path-ts`. State whether package name remains `bbnf-path` or is renamed; update every `cargo -p` gate accordingly.
7. After BA.W3.M4 — move `crates/core/src/path/` into `crates/path/src/runtime/`. W3 close leaves `crates/core/src/path/` empty or deleted.
9. Move `docs/tranches/BA/waves/W3.md:62-67` (parse_with deletion) into BA.W4 before `docs/tranches/BA/waves/W4.md:34-39` (parse_with reintroduction); remove the W3 `test(parse_with)` gate at `:148`.
10. Split BA.W4 into **W4a** (private parse core + eager empty-path cursor elision) and **W4b** (reroute `parse` and `Document::get<T>`, run API tests). No runtime path argument may remain on the eager fast path.
11. `docs/tranches/BA/BA.md:11`, `docs/tranches/BC/BC.md:11` — "Every **parse-throughput** gate cites...". Move BA-G3 and BC-G4..G10 under non-SOTA engineering gates.
12. `docs/tranches/BA/BA.md:23`, `docs/tranches/BA/waves/W4.md:41-46` — add a sonic-rs `get`/`get_unchecked` twitter measurement to SOTA with primary-source citation, OR mark BA-G9 as internal ratio only (non-SOTA).
15. `docs/tranches/BA/waves/W0.md:26-31`, `docs/tranches/BB/waves/W1.md:25` — move CSS and Sheets host fns to per-grammar host namespaces (`grammar/host/css_l4.rs`, `grammar/host/google_sheets.rs`); update generated path examples accordingly.
16. `docs/tranches/BA/waves/W1.md:21`, `:57-62` — define recogniser plugin schema fields (`name`, `crate`, `entrypoint`, `output_kind`) so generic IR never hardcodes miner names.
17. Add BA.W2 or BA.W5 gate for **inverse-layout-audit**: every compound-typed rule, including `->`-less rules, has `Layout` and reaches emitted fields.
18. Add BA.W2.M4 fail-explicit table from `audit/CENSUS-2026-05-03.md:571-581`; every fallback / asymmetry / shim row has a grep / test gate. (No row may close by "investigate later".)
19. Add BA.W0 or BA.W2 gate deleting the BBNF aggregator `pub use bbnf::*`; BBNF uses namespaced generated access like every other grammar.
20. Add generated-LOC gates to `docs/tranches/BA/waves/W1.md`, `W3.md`, `W4.md`. W4 windows: `json.rs ≤ 3,700`, `bbnf.rs ≤ 22,000`, `css_l4.rs ≤ 110,000`, aggregate ≤ +5% from W2.
24. `docs/tranches/BA/BA.md:59` — replace "BB.W0/W1" with "BB.W1 for CSS L4/BBNF/Sheets; BB.W2 for the five-grammar cohort."
27. `docs/tranches/BA/waves/W1.md:23`, `:105` — delete "slow-burn carry" unless a receiving wave, blocker, and close gate are named.

### BB (surgeries 4-5, 13-14, 21, 25, 30, 32, 34-35)

5. `docs/tranches/BB/BB.md:34` — replace `crates/ir/src/passes/types/` with `bbnf-ir/src/passes/layout/`; replace `crates/ir/src/egraph/` with `crates/egraph/` path-dep crate.
13. `docs/tranches/BB/BB.md:15-16`, `docs/tranches/BC/BC.md:16` — gate the exact lightningcss benchmark operation named by SOTA, OR add M1 Pro parse-only lightningcss measurements before treating CSS gates as SOTA.
14. `docs/tranches/BB/BB.md:141-142` — remove BBNF / Sheets perf rows without external SOTA, OR amend SOTA with concrete competitor numbers.
21. Add BB wave LOC windows to `docs/tranches/BB/BB.md:31-36`: W0 unchanged, W1 specialised-grammar windows, W2 generated-parser and runtime-template budgets separated, W3 Pratt/SIMD delta rows, W4 wrapper delta ≤ +2%, W5 visitor delta bounded by record count.
25. `docs/tranches/BB/BB.md:32-33` — resolve "all eight remaining grammars" vs cohort ownership. Split into more waves; each grammar class gets its own gate.
30. `docs/tranches/BB/BB.md:158` — replace BC.W5 with BC.W4 as Visitor consumer.
32. `docs/tranches/BB/waves/W3.md:57`, `:73` — make BB-G2 hard-fail OR add a concrete BC receiving perf gate; no unresolved Tailwind carry to BC.W5 without a gate.

### BC (surgeries 4, 8, 22-23, 26, 28-29, 31, 33)

4. `docs/tranches/BC/BC.md:30`, `:141`, `:152` — replace `TypeDesc` with `Layout` vocabulary. If a type descriptor remains, define it as a field of `Layout`, not as a separate canonical IR term.
8. `docs/tranches/BC/waves/W3.md:49-53` — `bbnf-runtime` depends on `crates/path`; it does not absorb `path/`.
22. Add BC.W3 generated-output relocation budget: path moves to `crates/bbnf-parse/src/parse/generated/`, bytes unchanged; delete stale `crates/core/src/grammar/generated/` post-W3 references.
23. `docs/tranches/BC/BC.md:24`, `:110` — make BC-G10 aggregate-only ≤ +2% and per-file ≤ +2.5%, OR reduce JSON's +2.3% row to ≤ +2%.
26. `docs/tranches/BA/waves/W0.md:142`, `docs/tranches/BB/waves/W0.md:52`, `docs/tranches/BC/BC.md:35` — normalise the fleet-wide fixture receiver to one wave (preferably BC.W5), OR add real gates earlier.
28. `docs/tranches/BA/waves/W3.md:133` — replace BC.W4 with BC.W5 for `bbnf-regex` endpoint reconciliation.
29. Add BC.W0 gates to `docs/tranches/BC/BC.md:30`: `docs/tranches/BC/audit/W0-sibling-baseline.txt`, `docs/tranches/BC/audit/W0-ascent-strategy-disposition.md`.
31. `docs/tranches/BC/BC.md:35` — remove "user adjudicates at hardening time"; choose one `bbnf-regex` endpoint in the plan and gate it.
33. **Draft `docs/tranches/BD/BD.md` with BD.W0 gates for BC→BD.C1..C3, OR delete `docs/tranches/BC/BC.md:56-62` and all BD carry promises.** (Phase 4 chooses: draft BD.)

### BB.W5 cookbook gates (surgery 34-35)

34. Add `docs/cookbook/path-macro.md`, `docs/cookbook/lifetime-surfaces.md`, `docs/cookbook/visitors.md`, `docs/errors/layout-lowering.md`, `docs/optimizer/pratt-simd-detection.md`, `docs/migration/bc-core-split.md` as gates in BA.W2, BB.W3, BB.W4, BB.W5, BC.W3, BC.W4.
35. Add BB.W5 gate: `pointer!` produces typed terminal paths without turbofish on unambiguous paths; wildcard returns typed iterators; invalid paths include grammar-aware diagnostics.

---

## §3 — The Nine Spec-Depth Gaps

Each gap names a directive from the user's review. Every tranche addresses the gaps in its scope.

### A — Lock 1 substrate identity (BA + BB)

Direct-to-struct is **both** API and memory layout. `OpenFrame` at any wave is a substrate violation, not a deferral candidate.

**Required**: BA tranche must commit to one of:
- (a) **BA migrates all 9 grammars** in W5/W5b/W5c/W5d (sub-waves per grammar class), and Lock 1 is honoured at BA close.
- (b) **BA migrates JSON only**, and Lock 1 is explicitly **deferred-with-receiver** at BA close, with BB.W1a/W1b/W1c per-grammar receivers.

No third option. The current draft straddles both and the audit caught it. Pick (a) or (b) and **defend the choice with: (i) iter-time impact, (ii) regression-risk mitigation, (iii) per-grammar test coverage)**.

### B — Lock 5 IR contract variant cardinality (BC)

BC.W0 names 7 `TypedIRNode` variants. MLIR / HIR / Cranelift / Salsa-derived IRs carry 30-100 variants. Either bbnf's IR is genuinely complete at 7, or 7 is a placeholder.

**Required deep research before drafting**:
- MLIR Op definition: how many op categories does the standard dialect carry? (`mlir.llvm`, `mlir.func`, `mlir.scf`, `mlir.tensor`)
- Cranelift `Inst` enum cardinality and grouping (arithmetic / memory / control)
- rustc HIR: `hir::ExprKind` cardinality (~30); `hir::ItemKind` cardinality (~12)
- Salsa-style query crate IRs (e.g., `chalk_ir::TyKind`, ~10-15 variants)
- chumsky + lalrpop generated types (no public IR)

**Output**: BC.W0 must commit to a `TypedIRNode` variant table with **at minimum 15-20 variants** covering: rule, alt, seq, repeat, optional, charclass, keyword, lit, scanner, hostcall, mapexpr, layout, span, ref, fold-result, tape-op (or its absence), pratt-spine, simd-scan, error-recovery, debug-marker. Each variant has: name, payload type, lower-time invariants, all-backend lowering rules, generation-site (in IR / in rule lowering / in optimisation pass).

### C — Lock 13 post-split file-size distribution (BA)

Plan claims 23 → 0 files >500 LOC. But: how many post-split files land at 100-499 LOC vs <100 LOC? The latter case is a god-directory of micro-files (also Lock 13 violation).

**Required**: BA.W2 produces a **post-split file-size histogram artefact** at `docs/tranches/BA/audit/W2-file-size-distribution.md`:
- Bucket counts: <50 LOC, 50-99, 100-249, 250-499, ≥500.
- ≥500 must be 0 (claim).
- <50 must be ≤ ⌈total_files * 0.05⌉ (avoid micro-fragmentation).
- 100-249 should be the majority (cohesive, navigable).
- Each post-split directory has 4-10 children (no >10, no 1-2).

### D — Cohort template parameterisation (BB)

BB.W2 templates 5 grammars. The template's actual parameterisation is undefined.

**Required deep research**:
- serde-rs derive-macro template surface
- proc-macro2 / quote idioms for templated codegen
- lalrpop's grammar-driven codegen template

**Output**: BB.W2 commits to a **cohort template specification** at `docs/tranches/BB/audit/W2-cohort-template-spec.md`:
- Template parameter table: `{ grammar_ident, kinds_enum, value_enum, document_struct, view_struct, parse_fn_signatures, leaf_kinds, host_fn_table, simd_alphabet }`
- Per-cohort instantiation table (BNF, CSV, EBNF, CSS Pretty, Math): values for each parameter.
- Hash-of-template artefact: `xtask regen --check` re-emits byte-identical output if template + parameters unchanged.
- Migration evidence: byte-equal diff vs hand-written cohort modules at first commit.

### E — `pointer!` grammar inference (BB)

`pointer!["a","b",1]` lacks grammar context. Three valid designs; plan picks none.

**Required deep research**:
- sonic-rs `pointer!` source (`sonic-rs/src/pointer.rs`) — how grammar / type context resolves
- jsonpath_rust + jsonpath-rs (the path-traversal crates' API)
- chumsky's typed `Parser<I, O, E>` — how `O` resolves at call site
- jq's `--type` option vs implicit traversal

**Output**: BB.W5 commits to a `pointer!` syntax decision at `docs/tranches/BB/audit/W5-pointer-syntax-decision.md`. Choose one of:
- (i) `pointer!(Json, ["a","b",1])` — explicit grammar marker mandatory; macro returns `JsonPath<...>` with terminal type.
- (ii) `pointer!["a","b",1]` with implicit grammar inferred from call-site type ascription (`let p: JsonPath<...> = pointer!["a","b",1];`).
- (iii) Both — `pointer!(Json, [...])` always works; `pointer![...]` works in typed-context positions.

Each option has: macro implementation sketch (Rust pseudocode), error message for ambiguity, friction examples, recommended default.

### F — Workspace metadata TOML schema (BA)

BA.W1 prescribes per-grammar `[workspace.metadata.bbnf-strategy]` but the schema is undefined.

**Required deep research**:
- cargo workspace metadata patterns (cargo-msrv, cargo-features, cargo-deny)
- proc-macro consumption of workspace metadata (cargo_metadata crate)

**Output**: BA.W1 commits to a TOML schema at `docs/tranches/BA/audit/W1-workspace-metadata-schema.md`:
```toml
[workspace.metadata.bbnf-strategy]
# Per-grammar entries
[workspace.metadata.bbnf-strategy.grammars.<ident>]
  source_path = "..."
  bootstrap = true | false
  recognisers = [
    { name = "...", crate = "...", entrypoint = "...", output_kind = "..." },
    ...
  ]
  host_fns = [
    { name = "...", crate = "...", path = "..." },
    ...
  ]
  output_dir = "..."
  pratt_eligibility = "auto" | "force" | "skip"
  simd_eligibility = "auto" | "force" | "skip"
```
Plus: validation rules (which fields are required / optional; which combinations are illegal); migration plan from current static `GRAMMARS` array; breaking-change notice strategy.

### G — Generated parser shape (BA + BB)

The actual codegen contract is the most consequential surface in the plan and the most under-specified.

**Required deep research**:
- sonic-rs source: how it generates / hand-writes value-construction inner loop
- simdjson On-Demand: how its document model emits typed accessors
- chumsky generated `Parser` impls (no codegen, but typed combinator shape)
- lalrpop generated parser tables (per-rule fn shape)
- pest derive-macro output

**Output**: BA.W5 + BB.W1 jointly commit to a **generated-parser-shape specification** at `docs/tranches/BA/audit/W5-generated-parser-shape.md`. For each grammar construct, specify:
- **Alt**: emission shape (when byte-disjoint → match; when speculative → ordered try; when char-class → SIMD scan; cost-model boundary).
- **Seq**: emission shape (linear push; field-binding to typed-enum variant).
- **Repeat**: emission shape (loop with break condition; CharClass-driven SIMD scan; cursor consultation).
- **Optional**: emission shape (peek byte; commit-or-skip).
- **CharClass**: emission shape (lookup table; SIMD shuffle; scalar fallback).
- **Keyword**: emission shape (PHF; small-string compare; suffix elide).
- **Scanner**: emission shape (regex DFA; bespoke NFA; inline byte-test).
- **Pratt operator chain**: emission shape (lookup table on operator token; recursive descent for operands).
- **HostCall**: emission shape (resolved at codegen via workspace metadata; backend-specific dispatch).
- **MapExpr**: emission shape (typed-enum constructor with bound field positions).

Each shape has: sample emission (Rust pseudocode), invariants, cost-model decision points.

### H — bbnf-parse / bbnf-codegen / bbnf-runtime split dependency arrows (BC)

BC.W3 names the crate split. The dependency DAG is unspecified.

**Required**: BC.W3 commits to a dependency-arrow specification at `docs/tranches/BC/audit/W3-crate-dependency-dag.md`:
- ASCII diagram of crate dependencies (no cycles; explicit arrows).
- For each crate: public API surface (exported types, traits, functions); private internals (must not appear in `pub use`).
- For each dependency: justification (why bbnf-codegen depends on bbnf-parse, not vice versa).
- For `bbnf-ir`: where it lives (crate of its own? folded into bbnf-parse? folded into bbnf-codegen?).
- Re-export sunset rules: which `pub use` lines exist temporarily, which are permanent, which retire by BC.W6.

### I — `parse-that` promotion path (BC)

Lock 11 names `parse-that` as path-dep "until API stabilises". BC.W5 freezes other sister crates but the audit catches `parse-that` omission. The deeper question: should `parse-that` ever leave incubation, or stay path-dep indefinitely as a private dependency?

**Required**: BC.W5 commits to a promotion-path decision at `docs/tranches/BC/audit/W5-parse-that-disposition.md`. Choose:
- (i) `parse-that` is a permanent private path-dep; never published. (Justification: it carries grammar-coupling.)
- (ii) `parse-that` is a future publication candidate at BD.W?; named gate.
- (iii) `parse-that` is stabilised + frozen + published in BC.W5 alongside other sister crates; named publish gate.

Each option has: API-freeze checklist, semver impact, downstream user friction.

---

## §4 — Voice Locks

§V1. **Archaic-permissive voice** ("hereupon", "begotten", "thereof", "appurtenant", "extant", "in fine", "thereafter"). Per `feedback_archaic-diction-is-voice`. Not corporate.

§V2. **No metalanguage**. Documents do NOT reference commits, conversation history, the plan's draft history, the audit, the synthesis, or "the user said". Per `feedback_no-metalanguage-docs`. Cite path:line for every concrete claim.

§V3. **Path:line citations on every concrete claim**. Read the file before quoting line numbers. Re-Read after surgery to verify the line number resolves to what you cite.

§V4. **State the deliverable. State the gate. Move on.** Per `feedback_no-workarounds`. Do NOT hedge. Do NOT use "consider", "may", "might", "perhaps". The plan is decided in-document.

§V5. **Tables liberal**. Markdown tables for every multi-row enumeration. No prose lists when a table conveys the same information.

§V6. **Per-X tables for every "all-X" claim**. Per Operational Rule 1.

---

## §5 — Wave-Numbering Convention

The current 7-wave structure (W0-W6) is a backbone. Sub-waves use letter suffixes:

- **W3a** (split from W3): private parse core
- **W3b** (split from W3): public wrappers
- **W3c** (added): parse_with deletion
- **W4a** (split from W4): generated parse_with for all grammars
- **W4b** (added): cursor empty-path elision verification
- etc.

Sub-waves get their own files: `docs/tranches/BA/waves/W3a.md`, `W3b.md`, etc. Each carries the same per-wave structure (Header / §1 Deliverable / §2 Milestones / §3 Closer gate / §4 Invariants / §5 Risks / §6 Cross-references / §7 Iter-time check / §8 Verification artefacts / §9 Audit lane forecast).

The wave summary table in the top-level tranche document (`docs/tranches/{X}/{X}.md`) lists sub-waves explicitly (e.g., `BA.W3a`, `BA.W3b`, `BA.W3c` as three rows).

Carry-tags use sub-wave granularity where the source/destination is sub-waved (e.g., `BA→BB.C1` becomes `BA.W5d→BB.W1a` if both sides are sub-waved). The synthesis pass verifies cross-tranche carry-tag receivers.

---

## §6 — Research Mandate

Every tranche agent MUST research before drafting. Research is **deep**: read primary sources, not just summaries.

### Universal research (all tranches)

- `audit/SOTA-2026-05-03.md` — re-read for parse-throughput anchors
- `audit/CENSUS-2026-05-03.md` — kill-list with path:line for excisions
- `audit/MODULES-2026-05-03.md` — per-file fates
- `audit/RESTART-SKETCH-2026-05-03.md` — JSON parse trace
- `audit/HARDENING-SYNTHESIS-2026-05-03.md` — synthesis verdict + 35 surgeries
- `audit/HARDENING-PLAN-2026-05-03-{01..08}-*.md` — 8 audit lanes
- `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` — failure mode
- `docs/HARDENING-PLAN-PROMPT.md` — 13 locks (settled)
- The current `docs/tranches/{X}/{X}.md` + waves (the file you are re-drafting)

### Per-tranche deep research

- **BA**: workspace metadata patterns (cargo_metadata crate, cargo-deny.toml schema); 8 inline test migration patterns; 9-directory layered re-org references (lightning-css `src/` layout, sonic-rs `src/` layout).
- **BB**: sonic-rs `pointer!` source; chumsky / lalrpop / pest codegen patterns; lightning-css visitor source (`Visit` derive); bumpalo + parse_in idiom; serde-rs DeserializeOwned escape.
- **BC**: MLIR Op cardinality; rustc HIR variant cardinality; Cranelift Inst grouping; Salsa / chalk_ir / chumsky type IRs; cargo-workspace crate split idioms (e.g., the cargo source tree's own structure).
- **BD**: TS proc-macro patterns (NAPI, swc, quickjs); WASM compilation pipelines (wasm-bindgen, wasm-pack, wit-bindgen); cargo-release / semver-checks / docs.rs publication ergonomics; Bun + Deno + Node.js native module ABIs.

Research output goes into the per-tranche audit/ subdirectory as `docs/tranches/{X}/audit/research-anchors.md` (one document, ≤ 200 lines, naming primary sources + key findings).

---

## §7 — Cross-Tranche Invariants (Synthesis Verifies)

The synthesis agent verifies these AFTER all four tranche re-drafts land:

| Invariant | Verification |
|---|---|
| Every carry-tag has a receiving wave + receiving gate | grep all `BA→BB.C\d`, `BB→BC.C\d`, `BC→BD.C\d`, `BA→BC.C\d`; for each, find the receiving wave's §6 cross-reference table; verify the receiving wave's §3 closer-gate names the close-criterion. |
| Every "all-X" claim has a per-X table | grep "all .* grammars", "every grammar", "all backends"; for each, the immediately following text is a markdown table. |
| Every deferral has receiver + blocker + receiving gate | grep "deferred", "carries to", "future", "TBD"; reject any without all three. |
| Every parse-throughput gate cites competitor + dataset + platform | grep "G\d.*sonic-rs|simdjson|lightning|cssparser|chumsky|serde_json"; verify each gate. Non-throughput gates must NOT claim Lock 8 honour. |
| 13-lock honoured table is exhaustive across tranches | every lock has at least one wave-cell; locks not addressed in BA/BB are addressed in BC/BD; locks deferred are explicitly deferred-with-receiver. |
| File-size distribution honours Lock 13 | BA.W2's file-size-distribution.md exists; ≥500 LOC bucket has 0 files; <50 LOC bucket is bounded. |
| Generated-LOC budgets are wave-level (not just tranche-level) | every wave §1 / §3 / §7 names its generated-LOC delta from prior wave. |
| TS/WASM scaffold (BC.W2) has BD activation receiver | BD.W0 / BD.W1 / BD.W2 carry TS/WASM activation gates; otherwise BC.W2 is "scaffold-only" with explicit non-Lock-5 disposition. |
| `parse-that` disposition decided in-plan | BC.W5's parse-that-disposition.md exists; one of (i)/(ii)/(iii) is chosen. |
| `pointer!` syntax decided in-plan | BB.W5's pointer-syntax-decision.md exists; one of (i)/(ii)/(iii) is chosen. |

---

## §8 — Output Cardinality Targets

| Tranche | Pre-draft waves | Post-draft waves (target) | Post-draft total LOC (target) |
|---|---|---|---|
| BA | 7 (W0-W6) | 10-13 (sub-wave splits at W3, W4, W5; possibly add W6 sub-wave) | ~2500-3500 |
| BB | 7 (W0-W6) | 10-15 (sub-wave splits at W1, W2, W3, W4, W5) | ~3000-4000 |
| BC | 7 (W0-W6) | 10-12 (sub-wave splits at W0/W1, W3, W5) | ~2500-3500 |
| BD | 0 (drafted from scratch) | 5-7 (W0-W4 or W0-W6) | ~1500-2500 |

Total Phase-4 plan output: ~9500-13500 lines, against the ~4669 of Phase 2.

The 9 spec-depth gap audit/ documents (A-I) add ~1500-2500 additional lines.

Total Phase-4 deliverable (waves + audit/): ~11000-16000 lines.

---

## §9 — Sequencing Discipline

Every wave (sub-waves included) must satisfy the Era V invariant (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:7-10`):

> No substrate lands without a same-wave or next-wave consumer.

In the per-wave §6 cross-reference table, every wave must name:

- Producing-wave outputs (what this wave makes)
- Same-wave or next-wave consumers (who reads what this wave makes)
- Closer-gate that proves the consumer reads the substrate (cargo command + assertion)

If a wave produces substrate without a consumer, fault. Either move the substrate forward, merge the wave with its consumer, or cut the substrate.

---

## §10 — Honesty Discipline

Per the user's third operational rule + `feedback_no-workarounds`:

- **No claim of lock-honoured if substrate is preserved**. If `OpenFrame` is alive at BA close, Lock 1 is **deferred-with-receiver**, not honoured. Mark it explicitly. The 13-lock table cell says "deferred to BB.W1a" not "honoured at W5".
- **No "carries to BB" without naming the wave**. Carries name `BA→BB.W1a.M3`, not "carries to BB".
- **No "user adjudicates"**. Decide in-plan.
- **No "future BD" with BD undrafted**. BD is drafted in Phase 4.
- **No "investigate later"**. Every milestone has an exit-criterion that is a cargo / rg / shell command, not "investigate".

---

## §11 — Fastidiousness Discipline

Per the user's emphasis on fastidiousness:

- Every section header in every wave file is filled. No `## §X — TBD`.
- Every cell in every markdown table is non-empty. No `—` or `TBD`.
- Every milestone has Surface / Action / Gate / Exit-criteria. No three-of-four.
- Every cargo command is verbatim and executable, not paraphrased.
- Every path:line citation resolves. Re-Read the cited file after surgery.

The synthesis agent runs grep / Read on every wave file and rejects empty cells.

---

## §12 — Phase-4 Execution Mode

1. Four parallel tranche agents (BA, BB, BC, BD), 45-min hard cap each.
2. Each agent: research → surgery → spec-depth additions → wave splits → audit/ artefact production.
3. Each agent commits its own work as `docs(tranches/{X}): phase-4 spec-depth re-draft` at completion. Per-wave files are committed atomically; cross-wave amendments are split commits with named surgery IDs.
4. Synthesis agent runs after all four complete (15-min cap). Output: `audit/PHASE-4-SYNTHESIS-2026-05-03.md` covering §7 cross-tranche invariants.
5. If synthesis surfaces faults: orchestrator dispatches narrow-scope amendment agents to close.
6. Phase 5 (re-invocation of `docs/HARDENING-PLAN-PROMPT.md`) ratifies before BA execution can begin.

---

## §13 — Closing Posture

Hereupon Phase 4 opens. The substrate is the 13 locks; the audit is the gate; this directive is the contract. The plan-set under re-draft must, at completion, be **independently executable** in the sense of Operational Rule 3: any agent reading any wave can begin work without consulting external state. The phase succeeds when the synthesis pass returns zero faults and the cookbook + audit artefacts are committed.

Until then, the plan remains a draft. Execution awaits.
