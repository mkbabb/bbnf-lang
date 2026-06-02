# Skinny Implementation Spec — Index

The skinny exists to validate the V1 architectural premise — specifically the SOTA-viability claim — before tranches A-J commit. **One grammar (JSON) end-to-end through 10 partial crates plus `xtask`, dual-track measured against sonic-rs / simd-json. Buildable in 2-4 weeks; ~32,000 handwritten LOC + ≤4,000 generated LOC.**

**Pass Omega V10 / SK-V18 generalization authority (2026-06-01).** SK-V15's
PRUNE-then-REBUILD W0-W11 narrative is historical. SK-V18 — the GENERALIZATION
cycle, the inflection backtrack — is the active certified contract: the two
hand-written/forked parsers (JSON + CSS) collapse into ONE grammar-driven
generator emitting JSON + CSS + Sheets from `.bbnf` (`generator_grammar_count
== 3`), preserving >SOTA honestly (CSS beats lightningcss 1.66-3.38x via
`track1_rich`; JSON beats sonic-rs strict), x86 DELETED (aarch64-only), net LOC
≈ −10800. The active implementation contract is
`restart/skinny/tranches/sk-v18/SPEC.md` — the certified
W-PRUNE→G1..G6→PROVE→H1 12-wave manifest. W-PRUNE (P1-P5) is the only
dispatch-now-eligible cluster after G-Omega; every G1..G6/PROVE/H1 wave is
gated on its predecessor's exit gate AND its entry-gate predicate AND an
explicit wave-triumvirate dispatch.

Historical SK-V5/SK-V6/SK-V13/SK-V14/SK-V15 cohorts remain evidence for
rejected routes, primitive-admission discipline, and strict same-plane
comparator language; they are not the active dispatch anchor. SK-V16/SK-V17 are
the immediate antecedents — the SK-V17 tape-fold predictions (the ONE
BackendRule-walking projection generator, the shared NEON classifier) are now
SK-V18-CERTIFIED.

JSON parse_only / direct_to_struct / real_typed_struct remain 51/51 strict
same-plane guard rows (maintained at G1). CSS is no longer demoted: the
certified close gate is the SAME-RUN `track1_rich/lightningcss > 1.0x` ∧ no
same-run regression vs the parser's OWN pre-G2 baseline
(`track1_rich_over_lcss_ratio_pre_g2`, captured at G2 entry). `cssparser` is
the 9-field EXACT CORRECTNESS oracle (gate-before-speed), structurally distinct
from `track1_rich` — NOT a speed comparator; `lightningcss` IS the CSS >SOTA
speed bar. The 16-lock count and 5-shape `BackendShape` canon are preserved by
addition (no sixth shape, no renumber, no production FNV route, no new public
syntax).

**Pass Omega V10 / SK-V18 fold-adoption state (2026-06-01).** The SK-V17 T-P3
tape-fold is CERTIFIED-ADOPTED by SK-V18: the flat lazy-offset SoA
`Tape<'input>` + lazy `ValueRef<G>` projection is the single post-fold
substrate the 5 `BackendShape` shapes project from, and the ONE
BackendRule-walking projection generator now emits JSON byte-equal AND CSS +
Sheets lazy from one walk. The durable skinny REJECTIONS (AZ-IV eager value
tree 118x, per-leaf indirection, CSS fact-stream String as admission plane,
x86/AVX/SVE) are locks-strengthening fences — x86 is now DELETED, not merely
fenced.

The full V1 spec lives at `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/MIGRATION.md`, plus the PASS surfaces. This skinny spec carves out a minimum-viable subset of that V1 contract.

## Four quadrants

| Slice | File | Owns |
|---|---|---|
| Substrate | [SUBSTRATE.md](SUBSTRATE.md) | `Tape`, lazy structural-offset tape / `TapeToken` history, `ValueRef<'doc, 'input, K>`, `DocumentView`, payload arena policy, `bbnf-simd` integration contract, snapshot identity invariant, hand-coded JSON parity contract |
| Compiler | [COMPILER.md](COMPILER.md) | `json.bbnf` source sketch, Grammar IR subset (9 of 14 variants), BIR subset (14 of 20 variants), HM-only type checker, single-plan extraction, `codegen::rust` per-BIR-variant lowering, emitted runtime files (~1,185 LOC for JSON) |
| Bench | [BENCH.md](BENCH.md) | Dual-track contract (Track 1 = generated parser; Track 2 = hand-coded against same substrate), three competitor baselines (sonic-rs, simd-json, serde_json), three corpora (twitter/citm/canada), reproducibility schema, go/no-go threshold matrix, criterion harness layout, RESULTS.md template |
| Workspace | [WORKSPACE.md](WORKSPACE.md) | 10-crate set + `xtask`, per-crate LOC budgets (32,000 total handwritten), Cargo.toml skeleton with profiles (samply-resolvable), directory layout (Lock 13 honored), build/test commands, stub policy for skipped V1 crates, migration parity matrix |
| Hardening | [HARDENING.md](HARDENING.md) | Per-target audit specification for the skinny corpus. Composes with V1 `restart/prompts/audit-specs/HARDENING-LENS-SET.md` (lenses A-K) by reference; adds three skinny-specific lenses — L (premise fidelity), M (falsifiability), N (graduation mechanicality) — plus skinny-specific verdict classes (FAITHFUL/MASKING, MECHANICAL/ANTI-MECHANICAL). Cycle namespace SK-V1, SK-V2, etc.; outputs land at `restart/skinny/tranches/HARDENING-{TARGET}-SK-V{N}.md` |

## What the skinny is testing

**The SOTA-viability premise**: if a JSON parser generated through the V1 substrate (tape + direct-to-struct + structural SIMD scan) lands within or beats the sonic-rs / simd-json envelope on twitter / citm / canada, the V1 architectural premise is validated for JSON-class grammars. The dual-track measurement (generated vs hand-coded against the same substrate) separates **substrate ceiling** from **codegen overhead** as independent levers.

**SK-V6 same-plane rule.** The expanded gate now has five planes, not one
undifferentiated "JSON speed" number: retained parse, structural scan,
`semantic_full_digest_stressor`, generated `real_typed_struct`, and native
sidecar/flaw-probe comparators. SOTA-BEAT can be declared only on same-plane
rows: strictness, output shape, ownership, hardware, feature mask, corpus, and
freshness must match. asmjson's permissive Apple Silicon/SWAR rows and any
lossy UTF-8 competitor row can inform architecture, but they cannot ratify a
strict bbnf beat.

**Current measured split, 2026-05-14.** The original twitter / citm_catalog /
canada triad remains useful historical evidence after lazy offset tape and
local hot-path work, but it is not the dispatch gate. The current authority is
the expanded corpus in `skinny/RESULTS.md`, which records **overall outcome
N-direct / NoGo**. The retained parse plane has 13 G rows and four A rows
(`canada`, `mesh`, `marine_ik`, `numbers`). Canada structural scan is now
green in the full report at 69075 Mbps against the 40000 Mbps NEON floor,
folding SK-V5 redress item 56 into the official matrix. Direct-to-struct
correctness is green. The `semantic_full_digest_stressor` pass rows are
`citm_catalog`, `apache_builds`, `github_events`, and `instruments`; the other
13 digest rows remain red. The representative `real_typed_struct` rows for
`twitter` and
`update_center` pass under the host/API output-schema plane. Tranche dispatch
must treat the expanded corpus plus both direct workload planes as the current
SOTA-BEAT block.
SK-V15 is the active dispatch anchor via
`restart/skinny/tranches/sk-v15/SPEC.md` and
`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`. Older SK-V6/SK-V13/SK-V14
prompt text is legacy evidence only. REDRESS 50-55, 72, 96/97/98, 119, 120,
183, 184, 209-212, and PASS-IMPL V1 remain negative-route evidence that a new
wave must cite with material differential; they are not close waivers under
SK-V15.
Generated direct string lowering now preserves raw spans to the sink boundary
through `JsonSink::*_source`; the first no-allocation decoded visitor consumer
and a later exact decoded-stats sink were measured and rejected, so the current
receiver is a measured field-layout materializer or same-loop SinkOnly plan
rather than another generic visitor, sink-local decoded hash helper, or
parser-side eager decode. A quote-source one-pass streaming hasher was also
measured and rejected. SK-V5 redress item 57 admits inlinable direct receivers
plus a bounded direct-only tiny-plain-string source fast path as useful
source-shape work, not as a retained parse-G or Unicode close.

Threshold preview notation: `BEAT_BOUND = min(S × 0.95, T_README)`, where `T_README` is the README spec target (380 µs / 750 µs / 2.8 ms for twitter / citm / canada). For all three skinny corpora, `T_README` is the binding bound.

| Outcome | Meaning | Action |
|---|---|---|
| Track 2 ≤ BEAT_BOUND AND Track 1 ≤ Track 2 × 1.10 | Substrate viable, codegen viable | Dispatch tranches A-J; SOTA-beat at V1 likely |
| Track 2 ≤ S × 1.05 AND 1.15 < Track 1 / Track 2 ≤ 1.50 | Substrate parity, codegen gap | GO with codegen focus; if ratio exceeds 1.50, conditional hold per BENCH.md |
| Track 2 > S × 1.10 OR structural scan misses floor | Substrate gap | NO-GO; reopen Lock 1 amendment |
| Any direct-to-struct row slower than sonic-rs × 1.10 in time | Direct typed-emission gap | NO-GO; route to generated `SinkOnly` exact float/string/Unicode materialization |
| Parity oracle fail, SIMD parity hash fail, schema fail, or peak RSS > 3× competitor on canada | Correctness / instrumentation / memory failure | NO-GO or INVALID per BENCH.md §6; do not dispatch from throughput rows |

`S = min(sonic_rs_anchor_time, simd_json_borrowed_time, simd_json_owned_time)` for the corpus row, using the pinned API/mode recorded in BENCH.md. The classifier may compute from elapsed nanoseconds, but the published skinny report renders parse and scan throughput in Mbps plus Track 1 / sonic and Track 2 / sonic ratios. Full matrix in `BENCH.md` §6.

## What the skinny is NOT testing

| Not tested | Reason | V1 owner |
|---|---|---|
| Multi-grammar generation (CSS L4, Sheets, BBNF-self) | Skinny is JSON-only | Tranches D, F, H |
| LSP / DAP / incremental parse | Editor surface, not throughput | Tranche I |
| GADT / DK13 / OutsideIn / CSP | JSON's grammar is monomorphic | Tranche D |
| Cost-model + e-graph rewrites | Skinny pre-selects one canonical plan and bounds that cut with non-egraph alternate-plan stubs. The `alternate_dispatch_table_plan` candidate was invalidated empirically per `skinny/REDRESS.md` item 17 (duplicate probe + measured function-pointer table regression). The remaining alternate `scalar_plan` confirmed canonical wins by 38-52% on M1 Pro per `skinny/RESULTS.md`. PASS-IMPL V1 classifies the current Decision Engine as scaffold: zero executable e-graph rewrites, tautological CSP, grammar-named facts, and label-string lowerers cannot admit. | SK-V15 W7-W9 own executable Decision Engine activation and real or gate-rejected lowerers for `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`; legality/normalization rewrites remain V1 correctness work in `passes::normalize` |
| Pratt auto-detection | JSON has no operator precedence | Tranche H |
| Recovery / `@error` directives | SOTA inputs are valid | Tranche I |
| WASM / TS backends | V2 territory per Lock 5 amendment | V2 |
| `path!` / `select!` macros | User-facing query, not parse-throughput | Tranche G |
| Generated LOC enforcement at 9-grammar scale | One grammar in skinny | Skinny enforces JSON ≤4,000 generated LOC; nine-grammar scale routes to Tranche F.W3 |

## Cross-quadrant invariants

The four quadrants share these invariants. A change that breaks one breaks all four; a contradiction here is a scope signal.

1. **One grammar.** JSON only. Every quadrant assumes JSON's structural alphabet, byte-disjoint alts, monomorphic types, and a deliberate host-fn-free skinny grammar. Because V1 JSON has numeric/string host fns, BENCH must bound the direct-decode vs `CallHost` registry dispatch delta before RESULTS can claim FAITHFUL.
2. **One Backend.** `RustBackend: Backend` per ARCH §7.5. WASM/TS deferred to V2.
3. **Tape + direct-to-struct as one substrate.** Per Lock 1. No parallel substrate. No OpenFrame clone. SUBSTRATE.md §1; BENCH.md §1.1. SK-V6 refinement: tape and structural projection are a union; the retained offset projection is the tape. Mask streams are transient producers and never become a second retained substrate.
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
| Lazy-offset tape plus local hot-path specialization is the measured JSON substrate for the original triad, while expanded parse and direct rows remain open. | `skinny/RESULTS.md` current table: expanded retained parse has 13 G rows and four A rows; the `semantic_full_digest_stressor` direct workload is correctness-green but `N-direct / NoGo` with four passing rows (`citm_catalog`, `apache_builds`, `github_events`, `instruments`) and 13 failing rows. `real_typed_struct` passes for `twitter` and `update_center`. `skinny/REDRESS.md` items 20-72. | Lazy-offset tape is retained because it reduces materialization bytes and validates the tape/direct union on the original triad. SK-V5 redress item 56 restores the Canada structural scan floor in the full matrix; item 71 admits host/API schema-sourced DirectBuild for representative typed output; item 72 admits a generated-retained-only cap-16 tiny string probe while rejecting global/direct/Track 2 widening. The remaining expanded parse misses route to single-substrate event/tape consumption, Track 2 substrate-shape parity, and string/Unicode projection; digest misses route to generated source-hook `SinkOnly`, field-layout decoded string delivery, and exact float/string/Unicode materialization rather than eager-token revival, a generic decoded visitor, sink-local exact decoded-stats helpers, quote-source streaming hash helpers, parse-time retained projection aux side tables, byte-class whitespace cursor wrappers, parser-local structural-mask cursor rescans, or unconditional global string-threshold changes. | Lock 1 unchanged. V1 carries this as triad substrate validation plus expanded-corpus and direct-typed-emission implementation debt. **MECHANICAL-FROM-MEASURED-SPLIT** under Lens N. |
| Rejected alternates cluster remains binding. | `skinny/REDRESS.md` items 16-18, 25, and 49-55. | Structural-index typed parser prepass, NEON no-escape string matcher, separator elision, generic SWAR whitespace skipper, 12-byte/width churn, dispatch-table/function-pointer alternates, no-allocation generic decoded visitors, sink-local exact decoded-stats helpers, quote-source streaming hash helpers, parse-time aux projection columns, byte-class whitespace EventCursor wrappers, and parser-local structural-mask cursors are non-canonical because they failed to produce a better gate row or duplicated an invalid signal. | Future use requires a new before/after bench row that overturns the recorded rejection; Lock 1 remains unchanged. **MECHANICAL-FROM-NEGATIVE-EVIDENCE** under Lens N. |
| Hot-leaf/profile asymmetry improved through local generated-parser shape changes, but expanded corpus still exposes primitive gaps. | six-agent comparative-profile cohort 2026-05-12; `skinny/REDRESS.md` items 24-26; `skinny/RESULTS.md` expanded rows. | The accepted lowering changes are local: `parse_value_at`, short plain-string fast path, fused comma/close delimiter consumption, newline-indent space-run skipper, and SWAR digit/plain-string runs. The old structural-index typed parser prepass was rejected; the new plan is typed tape projection plus streaming mask-consume primitives, gated per corpus. | COMPILER/SUBSTRATE retain the cursor/offset contract. V1 must add grammar-neutral `bbnf-simd` and `parse-that` primitive contracts before claiming SOTA-BEAT. **ADDITIVE-MECHANICAL** under Lens N. |
| Per-target SIMD primitive layer is necessary but not sufficient. | `skinny/RESULTS.md` current full report: Canada structural scan = 69075 Mbps against a 40000 Mbps floor after SK-V5 redress item 56. Fresh profiles still show parse-driver/string/direct-materialization costs above the scanner on other rows. | `crates/bbnf-simd/` has replaced the old scanner surface for runtime and bench dependencies, with per-target submodules (`aarch64/`, `x86_64/{avx512_vbmi2,avx2}/`, `scalar/swar_8byte`) and parity tests. The full gate remains `N-direct / NoGo`; scanner-floor restoration is no longer the report-level blocker. | Lock 14 generalisation preserved (no grammar-specific code in `bbnf-simd`); Lock 16 governs admissible primitives + handwritten `asm!` blocks. V1 carries `bbnf-simd` plus typed-event consumption, parse-that primitive closure, and generated `SinkOnly`. **ADDITIVE-MECHANICAL with new Lock 16** under Lens N. |
| Build profile must declare `lto = true codegen-units = 1` for every runtime release build. Sonic-rs's INLINED hot-leaf count = 1 is empirically caused by this discipline; NOINLINE wall-clock falls 2.1-3.2× without it. The codegen template inversion yields only half its gain absent LTO fusion; the two are co-load-bearing. | `skinny/profile/sonic-rs-v2/PROFILE-REPORT.md` §(e) NOINLINE wall-clock ratios | Skinny `Cargo.toml [profile.release]` adds `lto=true`, `codegen-units=1`, `panic="abort"`, `debug=true`. WORKSPACE.md §3.1 binds. | Lock 15 new (build-profile discipline). Every bbnf-generated runtime crate carries this profile shape; deviations require documented measurement justification at `[workspace.metadata.bbnf.grammars.<name>.profile]`. **ADDITIVE-MECHANICAL with new Lock 15** under Lens N. |
| `lto=thin` regression: skinny-expanded profile (2026-05-12) shows the release binary built with `lto=thin` (not `lto=fat`/`lto=true`). Twitter throughput regressed 11780 → 5521 Mbps between SK-V2 and skinny-v3 (M1 Pro → M5 Max host change does not explain). Lock 15 enforcement gap. | `skinny/profile/skinny-expanded/PROFILE-REPORT.md` header line `lto=thin, codegen-units=1, debug=true`; `skinny/RESULTS.md:5-7` prior baseline. | Workspace `[profile.release] lto = "fat"` (explicit); verification command `cargo build --release -v 2>&1 \| grep -E '\-C lto=(fat\|true)'` returns ≥1 per workspace member. | Lock 15 enforcement amendment 2026-05-12 (verification command added). **ADDITIVE-MECHANICAL** under Lens N. |
| yyjson achieves SOTA-class throughput **without SIMD**: 3687 MiB/s twitter / 0.91 c/B on M5 Max, beating simdjson DOM (2923 MiB/s / 1.142 c/B) and sonic-rs Value-DOM (2438 MiB/s / ~2.3 c/B). Mechanism: `always_inline` everywhere + ~18 KiB hot function i-cache resident + `repeat16` macro unrolling + single-pass forward `read_num` (no two-stage scan-then-walk). | `skinny/profile/yyjson/PROFILE-REPORT.md` — twitter 3687 / canada 1549 / citm 2497 MiB/s; hot-leaf count = 1 (`yyjson_read_opts` 80-95% self); NO SIMD anywhere in `yyjson.c`. | Lock 15 extension: codegen template emits `#[inline(always)]` on Grammar IR's hot-path rules (cost-model-derived `LayoutFacts.hot_call_graph`); target ≤20 KiB post-LTO hot function size. Force-inline is auto, not directive. | Lock 15 amendment 2026-05-12 (force-inline + i-cache budget). Diagnostics `BBNF-FORCE-INLINE-MISSED` + `BBNF-ICACHE-BUDGET-EXCEEDED` per ARCH §7.4. **ADDITIVE-MECHANICAL** under Lens N. |
| Three parallel offset-Vec runtime artefact (the tape-union pathology): `ParserState.structural_offsets` (scan-emitted) + `TapeAssembler.offsets` (codegen-copied) + `Tape.offsets` (sealed) — three Vec<u32> at runtime; only simdjson among comparators keeps two buffers post-parse; asmjson/yyjson/RapidJSON/sonic-rs-LazyValue all use one. Implementation drift, not Lock 1 defect. | tape-union research agent 2026-05-12 (`skinny/crates/runtime/src/grammars/json/parser.rs:11+17`, `tape/mod.rs:157-292`); legacy string/unicode profile evidence is folded into SK-V5/SK-V6 authority. | DELETE `ParserState.structural_offsets`; rename `TapeAssembler` → `TapeBuilder`; scan emits write-through into `Tape::offsets`; fold three `Box<[u32]>` (offsets + string_escape_offsets + string_control_offsets) into one + one packed `flags: Box<[u8]>`. Net -180 LOC delete + 30 LOC flags fold + 20 LOC write-through. | Lock 1 verbatim CLARIFICATION (one sentence appended): "the structural projection IS the tape, not a sidecar; no parallel offset stream." No Lock 1 amendment; no spec-level new construct. **CLARIFICATION + MECHANICAL MIGRATION** under Lens N. |
| Escape-heavy corpus pathology affects ALL parsers, not just skinny: simdjson `unicode_escapes` at 4.97 c/B = 2× worse than canada (2.44) and 6.3× worse than github_events (0.71); simdjson stage1/stage2 ratio inverts from 55/33 (twitter) to 9/61/30 (unicode_escapes); sonic-rs LazyValue at 364 Mbps on unicode_escapes (5× worse than its own Value-DOM at 1839 Mbps). | `skinny/profile/simdjson-expanded/PROFILE-REPORT.md` per-corpus c/B + stage-inversion finding; `skinny/profile/sonic-rs-expanded/PROFILE-REPORT.md` LazyValue collapse on unicode_escapes; `skinny/profile/skinny-expanded/PROFILE-REPORT.md` unicode_escapes 5429 Mbps + `match_json_string` 62.7% self; fresh `profile/reprofile-2026-05-12/unicode_escapes` shows current parse-only is dominated by `parse_value_at`. | Corpus expansion forces honesty: no parser overfit to escape-heavy can be hidden behind twitter/citm/canada averages. BENCH.md §3.1 expanded corpus + §7.9 correctness gates (UTF-8 validation at scan + non-character codepoint admission per RFC 8259). | Bench corpus expansion + UTF-8 correctness gate; no Lock amendment but a `BBNF-UTF8-INVALID-AT-PARSE` + `BBNF-UNICODE-NONCHAR-CODEPOINT` diagnostic vocabulary addition per ARCH §7.4. **ADDITIVE-MECHANICAL** under Lens N. |
| Two UTF-8 correctness gaps in current skinny: (1) `view.rs:203, 229` panics on invalid-UTF-8 input rather than rejecting at parse time; (2) `parse-that-regex/src/lib.rs:352` `char::from_u32` over-rejects non-character codepoints (U+FDD0, U+FFFE, U+10FFFE — JSONTestSuite marks `y_` must-accept). | JSON corpora research agent 2026-05-12 (the `i_string_iso_latin_1.json`, `i_string_invalid_utf-8.json` test bundle); string/unicode profile agent ("no `simdutf8` dependency; UTF-8 validation absent from the hot path"). | Move UTF-8 validation to scan stage via `simdutf8` crate wrap (Lock 11 + Lock 16 algorithm-class citation Keiser-Lemire 2020); admit non-character codepoints per RFC 8259. New BENCH §7.9 conformance gate enforces. | Lock 9 (slice-borrow primary; Cow escape hatches) UNCHANGED; the fix lands at scan boundary, not view boundary. Diagnostic vocabulary additions `BBNF-UTF8-INVALID-AT-PARSE` + `BBNF-UNICODE-NONCHAR-CODEPOINT` per ARCH §7.4. **ADDITIVE-MECHANICAL** under Lens N. |
| 5-shape `BackendShape` taxonomy: `LayoutFacts.backend_shape[rule_id] ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` per `restart/ARCHITECTURE.md` §7.3. Per-rule selection is cost-model-derived (no directive, no workspace metadata) from existing Grammar IR facts: first-set disjointness, `@error(recover)` presence, `@host fn` decoded-at-parse presence, `@layout` scope presence, target-feature admissibility, retained-document need. JSON skinny derives `OffsetTape` for every rule per the algorithm at ARCH §7.3 step 1–8. | ARCH §7.3 LayoutFacts.backend_shape field; SK-V5 landed the Rust state in `603308b3`; SUBSTRATE.md §1.5 + §1.6; COMPILER.md §3.3 lowering matrix. | Lowering matrix at COMPILER §3.3 emits one access pattern per `backend_shape` against existing BIR variants (`Alt { Dispatch }`, `Seq`, `RepeatLoop`, `OptionalBranch`). No new BIR variant; no new directive; no parallel substrate. | V1 inherits the 5-shape taxonomy verbatim; the cost-model derivation runs in `passes::recognizers::derive_backend_shape` at every grammar's layout pass. **ADDITIVE-MECHANICAL** under Lens N. |
| Two-pathology-class fix prescription (`tiny_string_loop` + `hex_decode`) within `G-fusion-quality`. | Wave 2 Agent 2 finding 2026-05-12; BENCH.md §6.1.1; SOTA-BEAT-DESIGN.md §6 falsifiability matrix. | This remains a diagnostic, not the current gate story. Current parse-G rows are `twitter`, `random`, `unicode_mixed`, and `unicode_basic`, and the full blocker is `N-direct / NoGo`. Keep pathology labels in the bench output, but route current direct misses to generated `SinkOnly` exact float/string/Unicode materialization and route current parse misses to no-inline/PC-level `parse_value_at` attribution before prescribing another kernel. | NEON kernels remain admissible under Lock 16 only when the same wave consumes them and the affected corpus crosses S. **ADDITIVE-MECHANICAL, DIAGNOSTIC-ONLY** under Lens N. |
| `checkasm` differential harness as Lock 16 admission gate. Every SIMD primitive admitted under Lock 16 requires (a) a scalar reference implementation in `crates/bbnf-simd/src/scalar/`, (b) a target-feature CPUID dispatch table, (c) a `checkasm`-style parity + bench harness at `crates/bbnf-simd/tests/` per `feedback_no_inline_tests`, (d) corpus-parity against the expanded 17-row throughput corpus. The harness is the admission gate; primitives that fail parity against the scalar reference cannot land. | Lock 16 verbatim text (`restart/locks/LOCKS.md:69-94`); dav1d/FFmpeg checkasm lineage now routes through SK-V5/SK-V6 authority; FOSDEM 2023 VLC/FFmpeg slides. | `xtask primitive-checkasm` runs the per-primitive parity + bench harness; primitives are gated on PASS before they appear in the CPUID dispatch table. Handwritten `asm!` blocks are admissible only when the equivalent intrinsic is absent from `core::arch::*`. | V1 inherits the harness as the SIMD admission protocol. **ADDITIVE-MECHANICAL with Lock 16** under Lens N. |
| Tape capacity Plan D adoption. Capacity policy was a measured substrate dimension, but it is not the current blocker. | `skinny/profile/wave2-capacity/CAPACITY-REPORT.md`; BENCH.md §7.8.2 `alternate_capacity_plan` row. | `TapeBuilder` initial-capacity policy = `Plan D`: `Vec::with_capacity(256)` plus geometric grow. Sampled prefix, exact prepass, and one-shot SIMD pre-scan are rejected-route probes unless a later event-cursor/codegen change reopens allocation as a top profile leaf. Bench continues reporting logical + allocated tape bytes per corpus to expose regressions. | V1 inherits grow-only Plan D as the default capacity policy for `OffsetTape` / `EventTape` builders, with probes retained for future falsification. **ADDITIVE-MECHANICAL** under Lens N. |
| `escape_mask_64` NEON correctness bug (Wave 2 Agent 5 finding, 2026-05-12). The current NEON `escape_mask_64` kernel produces incorrect masks on certain backslash-run boundary cases; blocks SOTA-BEAT bench claims until corrected. Falsifier: `checkasm` parity row against scalar reference. | Wave 2 Agent 5 finding 2026-05-12; `bbnf-simd/tests/escape_mask_parity.rs` (pending). | Kernel correction lands BEFORE any SOTA-BEAT bench claim is honest; the `K — SIMD parity hash fail` outcome class fires on this kernel today. Diagnostic: scalar reference passes, NEON kernel fails on adversarial backslash-run corpora. | V1 inherits the correction; Lock 16 admission gate (checkasm) prevents future regressions. **CORRECTNESS-BLOCKER** under Lens M. |
| Lock 15 i-cache budget already met (Wave 2 Agent 3 evidence, 2026-05-12). The current skinny generated parser post-LTO measures under the 20 KiB hot-function budget per Lock 15; the `BBNF-ICACHE-BUDGET-EXCEEDED` diagnostic does not fire on the current skinny build. | Wave 2 Agent 3 cargo-asm measurement 2026-05-12; Lock 15 verbatim text. | No remediation required; the budget is met. Continued enforcement via the Lock 15 verification command at every build. | V1 inherits the budget; codegen template continues to emit `#[inline(always)]` on cost-model-derived hot rules. **HONORED-WITH-EVIDENCE** under Lens M. |

## Decision protocol

The skinny is the prior-validation step. Dispatch order:

1. Build the 10 crates per WORKSPACE.md.
2. Author `grammars/json.bbnf` per COMPILER.md §1.1.
3. Implement the substrate per SUBSTRATE.md (this is the longest single piece of work; ~4,000 LOC).
4. Implement the compiler pipeline subset per COMPILER.md (~4,400 LOC).
5. Hand-code the JSON parallel against the same substrate per BENCH.md §1.2 (substrate-API correspondence-gated inside `bbnf-bench`, not a separate runtime crate).
6. Run the parity matrix per BENCH.md §6.
7. Write `skinny/RESULTS.md` recording the verdict, Mbps table, reproducibility schema rows, arena counters, and tape-materialization notes. The `restart/skinny/` tree remains the spec authority.
8. Dispatch SK-V15 W0 first after G-Omega V9 authorization; continue W1 through W11 in strict SPEC order.
9. If any SK-V15 dependency row remains implementation-blocked, W11 records row-level intrinsic-block proof, revert/REDRESS, or fail-closed status; do not claim tranche close by deferral or SK-V16 routing.
10. If instrumentation is INVALID, re-run instrumentation; do not dispatch from that bench.
11. If a row fails twice in-tranche, surface the round-trip rule with architectural-block evidence or a fresh material differential, then continue under the user pin.

The skinny is buildable, measurable, and falsifiable. It exists to update the SOTA-beat probability with measurement evidence before the V1 plan commits 6-12 months of tranche execution.

## Authority cross-references

The four sibling slices ([SUBSTRATE.md](SUBSTRATE.md), [COMPILER.md](COMPILER.md), [BENCH.md](BENCH.md), [WORKSPACE.md](WORKSPACE.md), [HARDENING.md](HARDENING.md)) compose with the current SK-V15 tranche authority and keep older SK-V5/SK-V6/SK-V13/SK-V14 packets as historical evidence:

- [`tranches/sk-v15/SPEC.md`](tranches/sk-v15/SPEC.md) and [`tranches/sk-v15/DISPATCH-PROMPT.md`](tranches/sk-v15/DISPATCH-PROMPT.md) - active skinny implementation authority after G-Omega V9 CRUD alignment, W0-W11 strict order.
- [`tranches/sk-v14/SPEC.md`](tranches/sk-v14/SPEC.md) and the SK-V14 12-wave plan (W0..W11) - historical row-ledger evidence after the V8 W5B-FRONTENDR close; JSON remains a guard baseline, while CSS and generic infrastructure are audit-demoted by PASS-IMPL V1.
- [`tranches/sk-v13/SYNTHESIS.md`](tranches/sk-v13/SYNTHESIS.md), [`tranches/sk-v13/SPEC.md`](tranches/sk-v13/SPEC.md), [`tranches/sk-v13/DISPATCH-PROMPT.md`](tranches/sk-v13/DISPATCH-PROMPT.md), [`tranches/sk-v13/HANDOFF.md`](tranches/sk-v13/HANDOFF.md) — historical SK-V13 dispatch packet (now evidence; superseded by SK-V14 and SK-V15).
- [`audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md`](audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md) — historical SK-V6 evidence (2026-05-14): legacy purge, profile-first regression recovery, direct-string close discipline, strict workload matrix, and negative-route evidence.
- [`audit/SOTA-BEAT-DESIGN.md`](audit/SOTA-BEAT-DESIGN.md) §6 — falsifiability matrix per pathology class; `checkasm` admission gate; Wave 2 re-baseline against S-anchors.
- [`audit/GRAND-SYNTHESIS-SK-V5.md`](audit/GRAND-SYNTHESIS-SK-V5.md) — SK-V5 authority (2026-05-13; 417 LOC): corrected diagnoses (parse-attribution dishonesty, bench-private dishonesty, strictness honesty gap, BackendShape wiring gap, Lock 14 leak status, tape-union verdict) plus wave structure feeding the SK-V5 implementation packet.
- [`audit/IMPLEMENTATION-PACKET-SK-V5.md`](audit/IMPLEMENTATION-PACKET-SK-V5.md) — SK-V5 receiver packet (771 LOC; 7 waves): Wave 0 strictness columns + `parse-attribution` feature flag + nuke audit; Wave 1 BackendShape + LayoutFacts + `derive_backend_shape`; Wave 2 number lever + generated `SinkOnly`; Wave 3 source-hook/string work with UTF-8 fusion refuted as a close; Wave 4 partial Lock 14 remediation; Wave 5 consumed bbnf.asm primitive admission; Wave 6 strict workload matrix; Wave 7 optional x86 `CollapsedStage` successor.
- [`audit/NUKE-PLAN-SK-V5.md`](audit/NUKE-PLAN-SK-V5.md) — SK-V5 nuke catalogue (476 LOC; 16 sections): decisions recorded in Wave 0, deletions land in Wave 4 alongside bbnf-simd Lock 14 remediation so related changes commit together.
- [`audit/HANDOFF-SK-V5.md`](audit/HANDOFF-SK-V5.md) — SK-V5 packet-internal handoff (211 LOC): per-wave entry/exit gates, owner paths, falsifier commands, and the close-condition restatement.
- [`audit/SK-V5-COHORT/`](audit/SK-V5-COHORT/) — 15 audit reports, 5,559 LOC: A1-A6 deep research (comparative reassay, dav1d process, parse-that-regex gaps, tape-union audit, grammar generalization, research ledger); B1-B3 profile honesty (parse-attribution, direct attribution, native sidecar strictness); D1-D6 novelty challenge (Eisel-Lemire, UTF-8 pipeline, derive shape, SIMD split, sink-only, class A/B primitives).

SK-V5 close condition (`audit/GRAND-SYNTHESIS-SK-V5.md` §12): `skinny/RESULTS.md` shows zero parse-G rows, zero parse-L rows, zero N-direct rows, and strictness/output-plane columns disclosed per row; Track 1 calls generated runtime `SinkOnly`, Track 2 calls a structurally-different hand-coded path, and direct source is emitted by the real BIR lowerer rather than the static JSON template. The previous Track 1 ≡ Track 2 ≡ bench-private dishonesty is corrected; the remaining close blockers are measured runtime/materialization gaps (`N-direct`, retained G rows, decoded strings, single-substrate event/tape consumption), not codegen template authority or the now-cleared Canada scan floor. Parse-time retained projection aux side tables, byte-class whitespace cursor wrappers, and parser-local structural-mask cursors are rejected by measurement and do not count as the event-consumption close. x86 `CollapsedStage` successor remains out of scope for SK-V5 close.

External authority: [`../ARCHITECTURE.md`](../ARCHITECTURE.md) §7.3 (LayoutFacts.backend_shape 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` + cost-model derivation algorithm); [`../locks/LOCKS.md`](../locks/LOCKS.md) (779 lines post-CRUD-3) Lock 1 verbatim + clarification (structural projection IS the tape) + LAC-2F-V5-02 substrate-union v+1 ELEVATED ban on cross-call retained classifier state + LAC-1E-14 FactStream 5th SUBSTRATE-manifest category (NOT 6th BackendShape), Lock 10 5-shape canon (preserved verbatim; G-Omega-gated extension only), Lock 14 v+1 generated-output allowance + Pattern H 67 per-tranche census mandate + `byte_class_from_range_64` sibling, Lock 15 (build-profile + force-inline + i-cache budget), Lock 16 (SIMD/ASM admissibility allowlist + checkasm gate + bbnf-regex::Dfa admissibility row).
