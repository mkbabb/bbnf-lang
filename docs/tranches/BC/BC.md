# BC — Backend ABI + Multi-Backend Foundation

## Gestalt

BC is the backend ABI tranche: hereupon the codegen surface formalises into a backend-agnostic typed IR + per-backend lowerer per Lock 5; the existing Rust source emitter refactors to consume the IR contract instead of walking the grammar IR directly; the IR contract specifies input grammar IR + Layout, output backend-agnostic typed IR; TS and WASM emitter scaffolds land but their production activation EXPLICITLY DEFERS to BD+. The core crate splits into `bbnf-parse` (source, parse, lower, host) + `bbnf-codegen` (codegen IR + per-backend lowerer + Rust emitter) + `bbnf-runtime` (runtime, path, handle) per `audit/MODULES-2026-05-03.md:1158-1167`. The sister crates — `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` — freeze their public APIs and become candidates for crates.io publication; the two-candidate `bbnf-regex` endpoint reconciles to one per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175`.

The thesis is contract-first formalisation begotten of refactor: the Rust emitter already walks an IR-shape (BB.W1 generalised the direct-to-struct emit across all nine grammars); BC names the contract, refactors the consumer to consume it through the formal trait surface, and proves the contract supports TS + WASM by compiling scaffold emitters that produce non-empty syntactically-valid output for at-least-one trivial grammar. Final perf gates surpass sonic-rs / simdjson / lightning-css per Lock 8 — `JsonParser::parse(twitter.json)` ≤ 380 µs beating sonic-rs's 436 µs (`audit/SOTA-2026-05-03.md:50-58`); `parse(bootstrap.css)` ≤ 3.0 ms beating lightningcss's 4.16 ms (`audit/SOTA-2026-05-03.md:131-136`); `parse(canada.json)` ≤ 2.8 ms beating sonic-rs's 3.144 ms (same). BC stabilises generated/* LOC; the contract is the boundary, not the regen surface.

## Hard gates

Every gate cites a specific competitor + dataset + platform per Lock 8. Zero "AU baseline" or "≥ pre-W3" gates appear in any cell.

| ID | Gate | Anchor |
|---|---|---|
| BC-G1 | `JsonParser::parse(twitter.json)` ≤ 380 µs on M1 Pro, beating sonic-rs's 436 µs by ≥ 13% | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs benchmark_aarch64 twitter row); tighter than BB-G1 |
| BC-G2 | `parse(bootstrap.css)` ≤ 3.0 ms on M1 Pro, beating lightningcss's 4.16 ms by ≥ 27% | `audit/SOTA-2026-05-03.md:131-136` (lightningcss bench table) |
| BC-G3 | `parse(canada.json)` ≤ 2.8 ms on M1 Pro, beating sonic-rs's 3.144 ms by ≥ 11% | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs canada row) |
| BC-G4 | The IR contract is documented at `docs/codegen-IR-CONTRACT.md` — input grammar IR + Layout, output backend-agnostic typed IR; the Rust emitter consumes only the typed IR; no direct grammar-IR access in `bbnf-codegen`'s emitter | Lock 5 |
| BC-G5 | Core crate splits into `bbnf-parse` / `bbnf-codegen` / `bbnf-runtime`; each compiles independently with explicit dependency arrows; `bbnf-parse` does not depend on `bbnf-codegen` | `audit/MODULES-2026-05-03.md:1158-1167` |
| BC-G6 | TS + WASM emitter scaffolds exist at `bbnf-codegen/src/ts/` and `bbnf-codegen/src/wasm/` (post-split paths) but are NOT activated; the IR shape supports them via the `Emitter` trait; the same-wave consumer is the trivial-grammar smoke test (BNF or CSV) | Lock 5 |
| BC-G7 | `crates/path-core/` final API stable; `bbnf-regex` endpoint reconciliation per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175` chooses one of the two candidates and documents the choice with rationale | Lock 11 |
| BC-G8 | `crates/path/`, `crates/path-core/`, `crates/path-ts/` API surface frozen and documented; sister crates (egraph, egraph-derive, csp-solver, bbnf-regex) candidates for crates.io publication; `cargo publish --dry-run` produces clean output for each | Lock 7 + Lock 11 |
| BC-G9 | Sonic-class get-by-pointer surface complete: `JsonValue::pointer(input, &path) -> Option<LazyValue<'a>>` with the `pointer!` macro from BB.W5; lightning-css-class visit-by-type surface complete: `visit_<Name>` per record with `VisitTypes` bitflag pruning subtree traversal across all nine grammars | Lock 9 + `audit/SOTA-2026-05-03.md:103-118` |
| BC-G10 | Generated-file LOC stable: post-BC `crates/core/src/grammar/generated/` net delta from BB ≤ +2%; the IR contract is the boundary, not the regen surface | Lane 06 generated-code budget |

## Wave summary

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BC.W0 | IR contract specification: `docs/codegen-IR-CONTRACT.md` documents the input (grammar IR + Layout output from `bbnf-parse`'s lowering) and the output (backend-agnostic typed IR consumed by per-backend lowerer). Names every IR node kind: `TypedIRNode`, `TypedRule`, `TypedAlt`, `TypedSeq`, `TypedRepeat`, `TypedCharClass`, `TypedKeyword`. Specifies Lifetime / Layout / TypeDesc resolution; specifies what is per-backend (only leaf source emission) and what is shared (decision dispatch, IR walk, strategy selection). Existing Rust source emitter refactors here from `crates/core/src/codegen/rust/emitter.rs` to `crates/core/src/codegen/rust/lower.rs` consuming `TypedIRNode`. | Lock 5 (IR + per-backend lower); the contract emerges from refactoring the existing trait surface, not forward design. | `docs/codegen-IR-CONTRACT.md` lands; the Rust emitter consumes typed IR exclusively; `cargo nextest run -p bbnf` 100% pass; samply trace shows zero grammar-IR access from emitter site. |
| BC.W1 | Rust emitter refactor to IR contract: `crates/core/src/codegen/rust/emitter.rs` becomes `crates/core/src/codegen/rust/lower.rs`; existing emitter shapes — struct_direct, dispatcher, alt_dispatch — refactor to consume typed IR. Codegen surface preserves Lock 1 (direct-to-struct visible-and-internal across all nine grammars per BB.W1 generalisation) and Lock 9 (slice-borrow primary). | The refactor is a rename + interface narrowing, not a behaviour change; regen-equality between pre- and post-refactor xtask output is the gate. | `cargo xtask regen --check` produces byte-identical output to BB close artefact for all nine grammars; per-grammar parity tests against sonic-rs / lightningcss / simdjson / cssparser remain green. |
| BC.W2 | TS + WASM emitter scaffolds: `bbnf-codegen/src/ts/` and `bbnf-codegen/src/wasm/` (or pre-split `crates/core/src/codegen/{ts,wasm}/`); each implements the `Emitter` trait; each takes typed IR and emits target-language source/bytes; NOT activated for production — `emit()` returns `unimplemented!()` for non-trivial cases; the same-wave consumer is the trivial-grammar smoke test. | Lock 5 (IR shape supports TS + WASM via the Emitter trait + scaffold compilation, NOT runtime exercise); Era V failure mode mitigated because the smoke-test consumer is in the same wave. | TS emitter produces a `parseObject(ctx)` skeleton for JSON's `object` rule per `audit/RESTART-SKETCH-2026-05-03.md:559-577` AND a parse fn for the trivial BNF/CSV cohort grammar; WASM emitter produces a WAT skeleton for the same; both fail gracefully with `unimplemented!()` on rules requiring host-fn shims. |
| BC.W3 | Core crate split per `audit/MODULES-2026-05-03.md:1158-1167`: `bbnf-parse` carries `source/`, `parse/`, `lower/`, `host/`. `bbnf-codegen` carries `codegen/` and per-backend emitters. `bbnf-runtime` carries `runtime/`, `path/`, `handle.rs`. The split is structural; tests pass independently per sub-crate. | Lock 13 (no god directories at the crate level either); `audit/MODULES-2026-05-03.md:1149-1156` already verified zero circular dependencies in the proposed split. | `cargo check -p bbnf-parse -p bbnf-codegen -p bbnf-runtime` green independently; `cargo nextest run -p bbnf-parse -p bbnf-codegen -p bbnf-runtime` 100% pass; `bbnf-parse` does NOT depend on `bbnf-codegen`. |
| BC.W4 | Visitor surface formalisation: `Visitor<'i, T>` trait + `VisitTypes` bitflag becomes the IR contract's traversal API; per-backend lowerer emits `visit_<Name>` methods consuming typed IR; CSS L4 + JSON visitors tested cross-backend — Rust emits + executes; TS emits but does not execute since BC scaffold defers activation. | Lock 9 + `audit/SOTA-2026-05-03.md:103-118` (lightningcss visitor reference). | BC-G9 met (CSS L4 exposes `visit_color`, `visit_length`, `visit_url`, `visit_property`; JSON exposes `visit_string`, `visit_number`, `visit_object`, `visit_array`); cross-backend TS-emit produces compileable interfaces. |
| BC.W5 | Sister crate API freeze + `bbnf-regex` endpoint reconciliation per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175`: the two candidate endpoints reconcile to one; choice documented with rationale. Sister crates — `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` — candidates for crates.io publication. The wave presents the two options + selection criteria + a default recommendation; user adjudicates at hardening time. Worktree fixture symlink contract closure per `audit/HARDENING-SYNTHESIS-2026-05-03.md:158-164`. | Lock 11 (sister crate stabilisation); Lock 7 (path crate API freeze). | `cargo doc -p egraph -p egraph-derive -p csp-solver -p bbnf-regex` clean; `cargo publish --dry-run` clean for each; endpoint reconciliation pre-flight commands run cleanly; `xtask worktree-init` materialises every grammar's data + rewrite fixtures. |
| BC.W6 | BC close: final perf gates BC-G1..G3 met (sub-sonic JSON, sub-lightning CSS); PROGRESS / FINAL; IR contract published at `docs/codegen-IR-CONTRACT.md`; carry ledger to BD.W0 named explicitly (TS + WASM activation, host-fn per-backend resolution, sister crate publication). | Lock-honoured at every gate; all 13 locks closed in §13-Lock cross-reference table. | `cargo nextest run -p bbnf-parse -p bbnf-codegen -p bbnf-runtime -p bbnf-ir -p bbnf-analysis -p bbnf-path` 100% pass; bench harness produces post-BC.json archetype; competitor parity tests against sonic-rs / lightningcss / simdjson / cssparser / serde_json all pass. |

## Carry-tags FROM BA (skip-BB)

The §1.1 BA→BC carry-tags route directly to BC, bypassing BB. They land in BC's W0 + W2.

| Tag | Owner wave | Description | Receiving BC wave |
|---|---|---|---|
| BA→BC.C1 | BA.W2 | Layout-lowering canon supports the IR contract spec BC.W0 codifies. The renamed pass surface — `Layout`/`LayoutSink` — is the canonical input to the typed IR contract. | BC.W0 |
| BA→BC.C2 | BA.W5 | Direct-to-struct emitter pattern (one IR walker, leaf emission per backend) is the precursor to BC's `Emitter` trait formalisation across Rust/TS/WASM. The pattern arrives in BA.W5 (JSON only); BB.W1 generalises across nine grammars; BC.W0 codifies. | BC.W0, BC.W2 |

## Carry-tags FROM BB

| Tag | Owner wave | Description | Receiving BC wave |
|---|---|---|---|
| BB→BC.C1 | BB.W3 | Optimiser composition (CSP → e-graph → miners → cost model) is output-piped; BC's IR contract specifies the contract between optimiser stages and the per-backend lowerer. The contract names what the optimiser consumes and what the lowerer consumes; the boundary is the typed IR. | BC.W0 |
| BB→BC.C2 | BB.W1 | Direct-to-struct emit shape is grammar-agnostic (BA.W5 + BB.W1 generalised across nine grammars); BC formalises this as the IR-input/typed-IR-output contract for the per-backend lowerer. | BC.W0, BC.W1 |
| BB→BC.C3 | BB.W5 | Visitor + `VisitTypes` bitflag pattern is the per-backend lowerer's traversal API; BC's TS + WASM emitter scaffolds consume this contract; BC.W4 formalises the cross-backend visitor surface. | BC.W4 |
| BB→BC.C4 | BB.W0 | Sister crates (egraph, egraph-derive, csp-solver, bbnf-regex, parse-that) are path-deps; BC may promote any to crates.io once API stabilises. BC.W5 freezes APIs + publishes candidates. | BC.W5 |

## Carry-tags TO BD (the eventual TS/WASM emergence)

| Tag | Owner wave | Description |
|---|---|---|
| BC→BD.C1 | BC.W2 | TS + WASM emitter scaffolds exist; BD activates them in production; host-fn per-backend resolution (TS: `runtime.parseHexColor`; WASM: indexed extern import) is BD scope. The scaffolds compile, produce trivial-grammar output, and fail gracefully with `unimplemented!()` on host-fn shim sites; BD lands the host-fn resolution table per backend. |
| BC→BD.C2 | BC.W5 | Sister crates frozen; BD may promote any to crates.io. The endpoint reconciliation lands one canonical `bbnf-regex` path; BD operates against the published crate. |
| BC→BD.C3 | BC.W5 | Worktree fixture contract closure supports parallel-agent dispatch infrastructure for BD execution; `xtask worktree-init` materialises `data/{json,css,bbnf,sheets}` + `grammar/<name>/rewrites/*.ron` for every grammar; BD's parallel agent layout consumes this contract. |

## 13-Lock honoured

Every cell names the wave that addresses the lock; empty cells are faults. The Notes column flags weak adherence, prior-tranche closure, or carry-deferral.

| Lock | Wave | Notes |
|---|---|---|
| L1. Tape + columnar dead | (closed in BA.W0 + BA.W5 + BB.W1) | Era V columnar (`docs/tranches/AV/research/04-columnar-soa.md`) explicitly rejected; OpenFrame retiral spans BA + BB; BC carries no L1 work. |
| L2. Layout lowering canon | W0 (IR contract uses canonical `Layout`/`LayoutSink` terms) | Old terms (`type_projection`, `TypeMap`, `StructLayout`, `TypeDesc`, `schema_synthesis`) survive only in archived docs. The contract spec is the final canon-fixing point. |
| L3. Cursor + byte-skip unified | (closed in BA.W4 + BB.W2) | `__EAGER_EMPTY_PATH` LazyLock at BA.W4; BB.W2 generalised; BC carries no L3 work. |
| L4. Per-domain orthogonal optimisation | (closed in BB.W3) | CSP → e-graph → miners → cost-model output-piped; no unified hypergraph; BC.W5 freezes the sister-crate boundaries. |
| L5. IR + per-backend lower | W0 (IR contract codified); W1 (Rust emitter consumes typed IR); W2 (TS + WASM scaffolds compile against the contract); W4 (Visitor formalisation as IR contract traversal API) | The contract is the BC tranche's central deliverable. |
| L6. xtask emits committed source | W1 (regen-equality gate; xtask emits the refactored Rust output) | `bbnf-path`, `bbnf-path-ts` proc-macro shells are SEPARATE per Lock 7; not the codegen substrate. |
| L7. `crates/path/` consolidation | W5 (path-core API freeze; sister-crate publication candidates) | Three crate names (path, path-core, path-ts) only; no fourth proc-macro shell. |
| L8. Surpass sonic-rs / simdjson / lightning-css | G1 (≤ 380 µs twitter beating sonic-rs 436 µs by 13%); G2 (≤ 3.0 ms bootstrap.css beating lightningcss 4.16 ms by 27%); G3 (≤ 2.8 ms canada.json beating sonic-rs 3.144 ms by 11%) | Zero AU references; every gate names competitor + dataset + platform. BC tightens the gates further than BA + BB. |
| L9. Slice-borrow primary; bumpalo + owned escape hatches | W4 (Visitor surface formalisation; `LazyValue<'a>` borrowed views stable; `JsonValue::pointer(input, &path) -> Option<LazyValue<'a>>` complete) | Default surface is `&'i str` slice + `Cow<'i, str>` per `audit/SOTA-2026-05-03.md:122-123`. |
| L10. Pratt + SIMD auto-detected | (closed in BB.W3) | No grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. BC carries no L10 work. |
| L11. Path-deps for incubating sister crates | W5 (sister crate API freeze; crates.io candidates; `bbnf-regex` endpoint reconciliation) | egraph + egraph-derive + csp-solver + bbnf-regex frozen; simd-scan + bootstrap + analysis + lsp stay workspace-internal. |
| L12. ser + gorgeous archive BEFORE BA.W0 | (closed in pre-BA ceremony) | Verification: `archive/{ser,gorgeous}/` exist; `Cargo.toml` workspace members reduced by 2. Not a tranche; closed before BA opened. |
| L13. No god directories; cohesive encapsulation at every level | W3 (core crate splits into 3 cohesive sub-crates) | sonic-rs / lightningcss / simdjson cohesion is the standard. The crate-level split honours Lock 13 at the same level the directory split honours it within each sub-crate. |

## Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BC.W0 IR contract specification is over-engineered (over-specifies what's per-backend) | Medium | The IR contract emerges from refactoring the existing `Emitter` trait, not from forward design. The trait's current shape names what's shared; BC.W0 documents the existing contract, doesn't invent a new one. The refactor lands in the same wave as the spec. |
| BC.W1 Rust emitter refactor breaks regen-equality with BB close artefact | Medium | Regen-equality is the closer gate; the refactor is a rename + interface narrowing, not a behaviour change; `cargo xtask regen --check` produces byte-identical output to BB close artefact for all nine grammars. |
| BC.W2 TS + WASM emitter scaffolds introduce substrate-without-consumer per Era V failure mode | Medium | The same-wave consumer is the trivial-grammar smoke test (BNF/CSV); the production consumer is BD; per Lock 5, the IR contract requires the scaffolds to compile and produce non-empty output (not just exist). The scaffold-without-runtime-exercise is the correct ratification of Lock 5 IR shape support. |
| BC.W3 core split creates circular dependencies | Low | `audit/MODULES-2026-05-03.md:1149-1156` already verified zero circular dependencies in the proposed split; lower/ reads `runtime::bbnf::BbnfView`, but post-split `bbnf-parse` re-exports the BBNF runtime types from `bbnf-runtime`; the dep arrow is `bbnf-parse → bbnf-runtime`. |
| BC.W5 `bbnf-regex` endpoint flip breaks downstream consumers | Low | The rename is a one-time cargo-config update; current consumers reference `bbnf-regex` package name, not path; `[patch.crates-io]` block resolves transparently. The wave presents two options + criteria + a default recommendation; user adjudicates. |
| BC.W5 perf gates miss because BB.W3 optimiser path was gated on consumer demand that BC.W5 surfaces | Low | BC.W5's gates depend on BB.W3's CSP/e-graph/miner output; if BB.W3 doesn't deliver the expected speedup, BC.W5 gates become uncloseable; mitigation is per-wave samply checkpoints in BB.W3 + BC.W5 entry preflight. |
| BC.W4 cross-backend visitor surface drifts between Rust + TS emit | Medium | Both backends consume the same typed IR; the visitor methods derive from the IR's record set (one `visit_<Name>` per typed compound); cross-backend test asserts that both emit the same method-name set per grammar. |
| BD has not been drafted at BC close; carry-tags BC→BD.C1..C3 forward-commit without a receiving doc | Low (expected) | Per Lane 8 (Carry-Audit) and the user's TS/WASM punt at `audit/HARDENING-SYNTHESIS-2026-05-03.md:42-43, 56-58`, BD's drafting status is named explicitly; BC.W6's carry ledger names BD.W0 as the receiving wave; BD.W0's preflight consumes the carry. The forward commitment is structurally honest: BC stops; BD opens with the named carries. |
| The visitor surface formalisation at BC.W4 grows generated/* LOC beyond the +2% BC budget per BC-G10 | Medium | Per the §Build/iter time gate's per-grammar LOC budget table, the visitor surface is bounded by per-grammar record alphabet; CSS L4 grows +1.9% (largest), JSON grows +2.3%, cohort grammars grow +1.1-1.7%; aggregate is +1.9%, within budget. |

## Build/iter time gate

BC stabilises generated/* LOC. The IR contract spec (BC.W0) regenerates nothing; the Rust emitter refactor (BC.W1) requires regen-equality with BB close; the crate split (BC.W3) relocates files without regenerating. TS + WASM scaffold output lands at separate paths (`bbnf-codegen/src/ts/`, `bbnf-codegen/src/wasm/`), not added to existing `crates/core/src/grammar/generated/*.rs`. Net delta to `crates/core/src/grammar/generated/` from BB close: **0% to +2%** (visitor methods may add modest LOC per record; bounded by the per-grammar record count).

xtask iteration-time gate (BC-G3 reformulated as iter time): `cargo xtask regen --check` ≤ 22 s on M1 Pro at BC close. (BA close: ≤ 30 s; BB close: ≤ 25 s; BC close: ≤ 22 s.) The downward trajectory continues because the IR contract narrows the codegen surface and the scaffold-only TS/WASM emitters add minimal regen cost.

The post-W3 split additionally introduces per-sub-crate iter-loops: `cargo check -p bbnf-runtime` ~8 s (smallest; runtime-only); `cargo check -p bbnf-codegen` ~14 s (codegen-only); `cargo check -p bbnf-parse` ~14 s (parse-only); `cargo check -p bbnf` (umbrella) ~24 s. Per-sub-crate iter-loops benefit edits scoped to a single concern; the runtime-only iter-loop is a 65% improvement over the pre-split umbrella check.

### Generated-LOC budget table (BB close → BC close)

| Grammar | BB close LOC | BC close LOC | Net Delta | Source |
|---|---:|---:|---:|---|
| `json.rs` | ~2,200 | ~2,250 | +2.3% (visitor methods) | BB close artefact + BC.W4 |
| `bbnf.rs` | ~19,800 | ~20,200 | +2.0% (visitor methods) | same |
| `css_l4.rs` | ~93,000 | ~94,800 | +1.9% (visitor methods over many record types) | same |
| `google_sheets.rs` | ~13,200 | ~13,460 | +2.0% (visitor methods) | same |
| `css_pretty.rs` (cohort) | ~1,800 | ~1,820 | +1.1% (visitor methods, fewer records) | same |
| `ebnf.rs` (cohort) | ~1,500 | ~1,520 | +1.3% | same |
| `bnf.rs` (cohort) | ~600 | ~610 | +1.7% | same |
| `csv.rs` (cohort) | ~330 | ~335 | +1.5% | same |
| `math.rs` (cohort) | ~170 | ~172 | +1.2% | same |
| **TOTAL** | **~132,600** | **~135,167** | **+1.9%** | aggregate |

Each wave's commit body MUST include a per-file `## Generated-LOC Budget` table. Overflow without justification blocks the wave; the +2% net delta is the BC ceiling per BC-G10.

## Voice locks

§V1. Voice is archaic-permissive ("hereupon", "begotten", "thereof", "appurtenant", "extant"). Not corporate. Per `feedback_archaic_diction_is_voice`.

§V2. No metalanguage. Documents do NOT reference commits, conversation history, or the plan's draft history. Cite path:line. Per `feedback_no_metalanguage_docs`.

§V3. State the deliverable. State the gate. Move on.

§V4. Citations are path:line, not paraphrase. `audit/MODULES-2026-05-03.md:1158-1167` not "the audit recommends a core split".

§V5. Tables are liberal; markdown tables for every multi-row enumeration.

## IR contract — typed IR alphabet sketch

The IR contract documented at `docs/codegen-IR-CONTRACT.md` (landed at BC.W0) specifies the typed IR node alphabet derived from `audit/RESTART-SKETCH-2026-05-03.md:404-413`. The alphabet is the boundary between optimiser-output and per-backend-lowerer-input:

| Node kind | Carries | Per-backend resolves to |
|---|---|---|
| `TypedRule { rule_id, body, layout, type_desc }` | a top-level grammar rule + its resolved Layout + TypeDesc | Rust: `pub fn parse_<rule>` + `<G>Value::<Variant>` ; TS: `function parse<Rule>` + discriminated union variant ; WASM: function index + struct layout |
| `TypedAlt { branches, dispatch, layout }` | a typed alternation + dispatch strategy | Rust: `match first { ... }` byte-disjoint or speculative checkpoint ; TS: `switch (ctx.bytes[ctx.pos]) { ... }` ; WASM: br_table |
| `TypedSeq { children, layout }` | a typed sequence | Rust: typed-record field writes ; TS: object-literal field writes ; WASM: linear-memory struct writes |
| `TypedRepeat { body, kind, layout }` | a typed repeat + repeat kind | Rust: `SmallVec<[T; N]>` push loop ; TS: `T[]` push loop ; WASM: stack-allocated array |
| `TypedCharClass { class, layout }` | a character class predicate | Rust: bitmap or regex DFA ; TS: regex-equivalent ; WASM: bitmap predicate |
| `TypedKeyword { keyword, layout }` | a keyword literal | Rust: byte-comparison ; TS: substring match ; WASM: byte-comparison |
| `TypedRef { rule_id, layout }` | a reference to another rule | Rust: `parse_<rule>(...)?` call ; TS: `parse<Rule>(ctx)` call ; WASM: indirect function call |
| `TypedRegex { regex_id, layout }` | a regex predicate | Rust: bbnf-regex DFA ; TS: regex-equivalent ; WASM: bytecode walk |
| `TypedMap { inner, fn_id, layout }` | a host-fn invocation on a typed inner | Rust: `host::<fn>(span)` ; TS: `runtime.<fn>(span)` ; WASM: indexed extern import |
| `TypedHost { inner, fn_id, layout }` | a host-fn shim site | same as TypedMap; the BC scaffolds emit `unimplemented!()` for non-trivial backends |

Every node carries pre-resolved `Layout` (per BA→BC.C1 carry) and `TypeDesc`; per-backend lowerers consume these resolved fields without re-deriving structural decisions. The contract narrows the codegen surface — what is shared between backends is the IR walk + decision dispatch + strategy selection; what is per-backend is leaf source emission only.

## Wave-by-wave deliverable summary

| Wave | Primary deliverable | BC-G gates closed | Carry-tags consumed | Carry-tags produced |
|---|---|---|---|---|
| W0 | IR contract spec at `docs/codegen-IR-CONTRACT.md`; typed IR alphabet at `crates/ir/src/typed_ir/`; Rust emitter begins consuming typed IR | BC-G4 | BA→BC.C1, BA→BC.C2, BB→BC.C1, BB→BC.C2 | W0→W1, W0→W2, W0→W3 |
| W1 | Rust emitter refactor closer; per-shape consumers (struct_direct, dispatcher, alt_dispatch, pratt) consume typed IR; regen-equality with BB close | BC-G4 (behavioural verification) | W0→W1, BB→BC.C2 | W1→W2, W1→W3, W1→W4 |
| W2 | TS + WASM emitter scaffolds at `crates/core/src/codegen/{ts,wasm}/`; trivial-grammar smoke tests; JSON `object` reference emit; host-fn graceful failure | BC-G6 | W0→W2, W1→W2 | W2→W3, BC→BD.C1 |
| W3 | Core crate split into `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`; per-crate compile + test passing; umbrella `core` re-exports | BC-G5 | W0→W3, W2→W3 | W3→W4, W3→W5, W3→W6 |
| W4 | Visitor surface at `bbnf-runtime/src/visitor.rs`; per-backend `visit_<Name>` emit; CSS L4 + JSON visitor surfaces; cross-backend isomorphism | BC-G9 | BB→BC.C3, W3→W4 | W4→W5, W4→W6, BC→BD.C1 |
| W5 | Sister crate API freeze (egraph, egraph-derive, csp-solver, bbnf-regex); endpoint reconciliation; worktree fixture closure | BC-G7, BC-G8 | BB→BC.C4, W3→W5, W4→W5 | BC→BD.C2, BC→BD.C3 |
| W6 | BC close: final perf gates BC-G1..G3 met; PROGRESS / FINAL committed; carry ledger to BD.W0 named | BC-G1, BC-G2, BC-G3, BC-G10 | All preceding waves | BC→BD.C1, BC→BD.C2, BC→BD.C3 |

## SOTA anchors used in BC gates

| Anchor | Library | Dataset / Surface | Source |
|---|---|---|---|
| sonic-rs M1 Pro twitter parse 436 µs | sonic-rs (cloudwego) | twitter.json | `audit/SOTA-2026-05-03.md:50-58` |
| sonic-rs M1 Pro canada parse 3.144 ms | sonic-rs | canada.json | same |
| lightningcss bootstrap-4 4.16 ms | parcel-bundler/lightningcss | bootstrap.css | `audit/SOTA-2026-05-03.md:131-136` |
| lightningcss `Visitor<'i, T>` | parcel-bundler/lightningcss | `src/visitor.rs` | `audit/SOTA-2026-05-03.md:103-118` |
| sonic-rs `pointer!["a","b",1]` | sonic-rs | `src/pointer/` | `audit/SOTA-2026-05-03.md:33-42` |
| chumsky `.as_<T>()` | zesterer/chumsky | combinator return | `audit/SOTA-2026-05-03.md:174-182` |

Every BC perf gate names a competitor + dataset + platform per Lock 8. Zero "AU baseline" or "≥ pre-W3" gates appear in any cell.

## Friction forecast for BC's exposed surfaces

Per Lane 7 (Friction-Forecast), BC's three new user-facing surfaces require named educational artefacts. Each forecast names: the API surface, the user mental model required, the point of greatest confusion, the verbatim error message the lowerer/runtime should emit.

| Surface | User mental model | Point of confusion | Error message |
|---|---|---|---|
| Typed IR consumption (BC.W0) | typed IR is post-Layout-resolution; grammar IR is pre-resolution; the lowerer reads typed IR exclusively | "why does my emitter not see `IrNode::Repeat`?" — answer: it sees `TypedIRNode::TypedRepeat` with pre-resolved `Layout` | `error: rule '<name>' has no resolvable Layout because <reason>; layout-lowering pass at crates/ir/src/passes/layout/ records the dispatch decision` |
| Visitor surface (BC.W4) | `Visitor<'i, T>` walks the typed tree post-parse; `VisitTypes` bitflag prunes subtree traversal | "why isn't my `visit_color` called?" — answer: `visit_types() & CssColor::CHILD_TYPES` doesn't intersect; set the bitflag | `warning: visitor for record '<Name>' was set but visit_types() does not include the record's CHILD_TYPES bit; the framework will skip the subtree` |
| Core crate split (BC.W3) | three sub-crates: `bbnf-parse` (parse), `bbnf-codegen` (codegen), `bbnf-runtime` (runtime); umbrella `core` re-exports | "why can't I import `bbnf::backend::*`?" — answer: backend module moved to `bbnf-codegen`; use `bbnf::codegen::*` (re-exported from umbrella) | (compile-time): `error[E0433]: failed to resolve: use of undeclared crate or module 'backend'; help: 'bbnf::codegen' is the new path` |

## Closing posture

Hereupon BC closes the foundation arc. The IR contract is the boundary; the per-backend lowerer is the consumer; the Rust lowerer ships in production; the TS + WASM lowerers ship as scaffolds compiled against the contract. The core crate fractures into `bbnf-parse` / `bbnf-codegen` / `bbnf-runtime` as cohesive sub-crates; the sister crates — egraph, egraph-derive, csp-solver, bbnf-regex — freeze and publish; the `bbnf-regex` endpoint reconciliation lands one canonical path. The carry to BD is the TS/WASM activation, sister-crate publication, and worktree fixture infrastructure for parallel-agent dispatch. The 13 locks are settled; BC verifies them in seven waves and hands BD a ratified backend ABI.
