# Skinny Implementation Spec — Index

The skinny exists to validate the V1 architectural premise — specifically the SOTA-viability claim — before tranches A-J commit. **One grammar (JSON) end-to-end through 10 partial crates plus `xtask`, dual-track measured against sonic-rs / simd-json. Buildable in 2-4 weeks; ~31,400 handwritten LOC + ≤4,000 generated LOC.**

The full V1 spec lives at `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/MIGRATION.md`, plus the PASS surfaces. This skinny spec carves out a minimum-viable subset of that V1 contract.

## Four quadrants

| Slice | File | Owns |
|---|---|---|
| Substrate | [SUBSTRATE.md](SUBSTRATE.md) | `Tape`, `TapeToken` (16-byte packed), `ValueRef<'doc, 'input, K>`, `DocumentView`, payload arena policy, `simd-scan` integration contract, snapshot identity invariant, hand-coded JSON parity contract |
| Compiler | [COMPILER.md](COMPILER.md) | `json.bbnf` source sketch, Grammar IR subset (9 of 14 variants), BIR subset (14 of 20 variants), HM-only type checker, single-plan extraction, `codegen::rust` per-BIR-variant lowering, emitted runtime files (~1,185 LOC for JSON) |
| Bench | [BENCH.md](BENCH.md) | Dual-track contract (Track 1 = generated parser; Track 2 = hand-coded against same substrate), three competitor baselines (sonic-rs, simd-json, serde_json), three corpora (twitter/citm/canada), reproducibility schema, go/no-go threshold matrix, criterion harness layout, RESULTS.md template |
| Workspace | [WORKSPACE.md](WORKSPACE.md) | 10-crate set + `xtask`, per-crate LOC budgets (31,400 total handwritten), Cargo.toml skeleton with profiles (samply-resolvable), directory layout (Lock 13 honored), build/test commands, stub policy for skipped V1 crates, migration parity matrix |
| Hardening | [HARDENING.md](HARDENING.md) | Per-target audit specification for the skinny corpus. Composes with V1 `restart/prompts/HARDENING.md` (lenses A-K) by reference; adds three skinny-specific lenses — L (premise fidelity), M (falsifiability), N (graduation mechanicality) — plus skinny-specific verdict classes (FAITHFUL/MASKING, MECHANICAL/ANTI-MECHANICAL). Cycle namespace SK-V1, SK-V2, etc.; outputs land at `restart/skinny/audit/HARDENING-{TARGET}-SK-V{N}.md` |

## What the skinny is testing

**The SOTA-viability premise**: if a JSON parser generated through the V1 substrate (tape + direct-to-struct + structural SIMD scan) lands within or beats the sonic-rs / simd-json envelope on twitter / citm / canada, the V1 architectural premise is validated for JSON-class grammars. The dual-track measurement (generated vs hand-coded against the same substrate) separates **substrate ceiling** from **codegen overhead** as independent levers.

Threshold preview notation: `BEAT_BOUND = min(S × 0.95, T_README)`, where `T_README` is the README spec target (380 µs / 750 µs / 2.8 ms for twitter / citm / canada). For all three skinny corpora, `T_README` is the binding bound.

| Outcome | Meaning | Action |
|---|---|---|
| Track 2 ≤ BEAT_BOUND AND Track 1 ≤ Track 2 × 1.10 | Substrate viable, codegen viable | Dispatch tranches A-J; SOTA-beat at V1 likely |
| Track 2 ≤ S × 1.05 AND 1.15 < Track 1 / Track 2 ≤ 1.50 | Substrate parity, codegen gap | GO with codegen focus; if ratio exceeds 1.50, conditional hold per BENCH.md |
| Track 2 > S × 1.10 OR structural scan misses floor | Substrate gap | NO-GO; reopen Lock 1 amendment |
| Parity oracle fail, SIMD parity hash fail, schema fail, or peak RSS > 3× competitor on canada | Correctness / instrumentation / memory failure | NO-GO or INVALID per BENCH.md §6; do not dispatch from throughput rows |

`S = min(sonic_rs_anchor_time, simd_json_borrowed_time, simd_json_owned_time)` for the corpus row, using the pinned API/mode recorded in BENCH.md. The classifier may compute from elapsed nanoseconds, but the published skinny report renders parse and scan throughput in Mbps plus Track 1 / sonic and Track 2 / sonic ratios. Full matrix in `BENCH.md` §6.

## What the skinny is NOT testing

| Not tested | Reason | V1 owner |
|---|---|---|
| Multi-grammar generation (CSS L4, Sheets, BBNF-self) | Skinny is JSON-only | Tranches D, F, H |
| LSP / DAP / incremental parse | Editor surface, not throughput | Tranche I |
| GADT / DK13 / OutsideIn / CSP | JSON's grammar is monomorphic | Tranche D |
| Cost-model + e-graph rewrites | Skinny pre-selects one canonical plan and bounds that cut with non-egraph alternate-plan stubs. The `alternate_dispatch_table_plan` candidate was invalidated empirically per `skinny/REDRESS.md` item 17 (duplicate probe + measured function-pointer table regression). The remaining alternate `scalar_plan` confirmed canonical wins by 38-52% on M1 Pro per `skinny/RESULTS.md`. | H.W2/H.W3 cost-driven rewrite and recognizer tuning body; legality/normalization rewrites remain V1 correctness work in `passes::normalize` |
| Pratt auto-detection | JSON has no operator precedence | Tranche H |
| Recovery / `@error` directives | SOTA inputs are valid | Tranche I |
| WASM / TS backends | V2 territory per Lock 5 amendment | V2 |
| `path!` / `select!` macros | User-facing query, not parse-throughput | Tranche G |
| Generated LOC enforcement at 9-grammar scale | One grammar in skinny | Skinny enforces JSON ≤4,000 generated LOC; nine-grammar scale routes to Tranche F.W3 |

## Cross-quadrant invariants

The four quadrants share these invariants. A change that breaks one breaks all four; a contradiction here is a scope signal.

1. **One grammar.** JSON only. Every quadrant assumes JSON's structural alphabet, byte-disjoint alts, monomorphic types, and a deliberate host-fn-free skinny grammar. Because V1 JSON has numeric/string host fns, BENCH must bound the direct-decode vs `CallHost` registry dispatch delta before RESULTS can claim FAITHFUL.
2. **One Backend.** `RustBackend: Backend` per ARCH §7.5. WASM/TS deferred to V2.
3. **Tape + direct-to-struct as one substrate.** Per Lock 1. No parallel substrate. No OpenFrame clone. SUBSTRATE.md §1; BENCH.md §1.1.
4. **Single-plan extraction.** No CSP, no e-graph, no cost-model selection. COMPILER.md §5.3. BENCH carries alternate-plan probes (BENCH §7.8.2: scalar — reported, dispatch-table — invalidated per `skinny/REDRESS.md` item 17, x86_64 PEXT — plausibly-better; aarch64 measurement currently runs scalar only).
5. **Samply-resolvable profiles.** `debug = true`, `strip = false` in `release` and `bench`. WORKSPACE.md §3.1.
6. **Dual-track measurement.** Two bbnf-side parsers (generated + hand-coded) against the same substrate. The delta diagnoses substrate vs codegen. BENCH.md §1.
7. **Onboarding contract.** Two surfaces — `grammars/json.bbnf` (grammar source) plus the workspace metadata sketch — comprise the user-authored skinny input. Lock 14's §5.6 declaration-crate fence is empty for the skinny per Lock 14 (no declaration crate at skinny scope; declaration-crate enforcement returns at V1 Tranche F).

## Open contradictions and skinny-specific deviations from V1

These are deliberate scope cuts; the V1 graduation closes them.

| Contradiction | Source | Skinny resolution | V1 closure |
|---|---|---|---|
| ARCH §8.2 + Lock 2 say HM runs as a `passes::layout` subroutine. The skinny inverts this — HM is top-level, layout is pass-through. | COMPILER.md §9.1 | Skinny carries no `@layout` so layout has nothing to do; HM-as-top-level produces the same `LayoutFacts` shape. | Tranche D re-inverts when `@layout` arrives. |
| ARCH §12.2 says JSON has metadata + numeric/string host fns from `host::primitives`. The skinny is host-fn-free. | COMPILER.md §1.3 | The decode-string call moves into a SUBSTRATE-provided path; saves the `host` + `csp-solver` crates. | Tranche D adds `@host fn` surface; decode moves back. |
| Lock 13 demands 4-10 children per `src/` directory. `parse-that-regex/src/regex/{hir,nfa,dfa,vm}` nesting gives only 3 children. | WORKSPACE.md §4.7 | Promote `regex/*` sub-trees to top-level siblings: `hir/`, `nfa/`, `dfa/`, `vm/`, `literal/`. | V1 inherits the same shape. |
| `passes` budget at 6,000 LOC requires HM-only + observational-shapes-only + hand-curated-recognizers. | WORKSPACE.md §2.1 | If any of the three constraints is relaxed, skinny scope is wrong. **Treated as a binding signal**, not a budget overrun to absorb. | V1 grows `passes` to ~25,000 LOC across multiple sub-modules. |
| `workspace.metadata.bbnf.grammars.json.codegen.wasm = false` exists in the skinny metadata sketch while V1 rejects `wasm = true`. | WORKSPACE.md §3 | `false` is an explicit V1 Rust-line-only marker and must be accepted by the metadata validator. | V2 flips/adds backend metadata when `WasmBackend: Backend` lands. |
| `Tape<'input>` uses private-Vec semantic sealing for parse throughput. V1 I tranche's incremental reuse map (`ReparsePlan`) requires append-after-parse before committed snapshots. | SUBSTRATE.md §1.2 | The skinny's sealed view is the committed-snapshot projection of the future V1 `TapeBuilder<'input>` (private Vec, boxed slice, or chunked storage). | I tranche adds the mutable/reusable builder upstream; the read-side `Tape<'input>` and `ValueRef` shapes do not change. **MECHANICAL with named inversion** under Lens N. |
| HM-as-top-level vs HM-as-`passes::layout`-subroutine. The skinny inverts the boundary ARCH §7.3 documents (where `passes::layout` is the *producer* of `LayoutFacts`); skinny `passes::layout` is a trivial pass-through and `passes::layout::types` runs Algorithm-W as the actual fact-source. | COMPILER.md §4.4, §9.1 | Producer name and `LayoutFacts` shape are preserved at the public boundary; only the internal subroutine direction inverts. | Tranche D adds `@layout` lowering inside `passes::layout`, restoring the original direction. The HM module relocates from sibling to subroutine via wrapper, not rewrite. **MECHANICAL with named inversion** under Lens N. |
| Eager-tape substrate ceiling empirically pinned at ~1.6× sonic-rs across three corpora; per-token write bandwidth is the bottleneck. | skinny/REDRESS.md Sonic Closeness analysis (lines 158-215); skinny/RESULTS.md NO-GO outcome G across all three corpora | Eager-tape mode remains the canonical skinny substrate for SK-V2 commit. The lazy-offset tape path is dispatched separately as a V1 Lock 1 amendment surface (see `restart/skinny/audit/LAZY-TAPE-DESIGN.md`). | Lock 1 amendment ratifies dual-mode tape (Eager \| Lazy) via per-grammar metadata. V9.2 V1-corpus hardening cycle dispatches the lazy-tape proposal against PASS-1/2/3 + MASTER-PLAN trio. **MECHANICAL with V1 Lock 1 amendment surface** under Lens N. |

## Decision protocol

The skinny is the prior-validation step. Dispatch order:

1. Build the 10 crates per WORKSPACE.md.
2. Author `grammars/json.bbnf` per COMPILER.md §1.1.
3. Implement the substrate per SUBSTRATE.md (this is the longest single piece of work; ~4,000 LOC).
4. Implement the compiler pipeline subset per COMPILER.md (~4,400 LOC).
5. Hand-code the JSON parallel against the same substrate per BENCH.md §1.2 (≤500 LOC, counted inside `bbnf-bench`, not a separate runtime crate).
6. Run the parity matrix per BENCH.md §6.
7. Write `skinny/RESULTS.md` recording the verdict, Mbps table, reproducibility schema rows, arena counters, and tape-materialization notes. The `restart/skinny/` tree remains the spec authority.
8. If GO → tranche A.W0 dispatches with the skinny crates as the V1 starting state.
9. If CONDITIONAL → full A-J dispatch is blocked; only the explicitly named BENCH.md work may proceed while F/H waits on the failing ratio or probe.
10. If INVALID → re-run instrumentation; do not dispatch from that bench.
11. If NO-GO → reopen Lock 1 (substrate) or COMPILER §3 (extraction) per which delta failed.

The skinny is buildable, measurable, and falsifiable. It exists to update the SOTA-beat probability with measurement evidence before the V1 plan commits 6-12 months of tranche execution.
