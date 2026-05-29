# Alpha-D — Validated / Invalidated Ledger — SK-V17 (cycle V4)

Pass: Pass Alpha (skinny astral synthesis). Cycle: SK-V16 -> SK-V17.
Date: 2026-05-29.
Scope: durable SK-V16 wins carrying forward (with commit SHAs), invalidated /
superseded claims (with measurement evidence), and still-open items becoming
SK-V17 candidates. Discipline: aarch64 Apple M5 Max only; every claim cites
`path:line` / commit SHA / RESULTS row / measured number.

Bracket HEAD: master `1c5bd7a25` (`feat(sk-v16-W6-tape): add shared flat-tape
runtime substrate`).

Architecture authority cited throughout:
`restart/audit/skinny-impl-overfit/sk-v16-css-sota-tape-architecture.md`
(the unified tape / lazy projection / NEON hot-leaf union synthesis).

---

## 0. Benched-surface disambiguation (load-bearing; binds every §-row below)

The architecture-doc authority repeatedly cites the **TOTALITY (core) tree** —
`crates/core/src/runtime/...`, `bbnf_ir::registry::struct.rs` `StructLayout`,
`OpenFrame`, `CssArena`, `TapeStructBuilder`/`TapeCursor`, `css_l4/builder.rs:274`.
**None of these exist in the benched skinny tree.** Grep-verified clean across
`skinny/crates/`: `StructLayout`, `OpenFrame`, `CssArena`, `TapeStructBuilder`,
`begin_compound`, `TapeCursor` all return zero. This ledger adopts α-E's
translation correction (`alphaE-candidate-shortlist.md:37-51`) verbatim and binds
every win/candidate to the **measured skinny surface**:

| Doc (core-tree) symbol | Benched skinny equivalent | Verified path |
|---|---|---|
| flat tape substrate (`crates/core/src/runtime/tape/{record,arena,cursor}.rs`) | `Tape` / `TapeBuilder` / `ValueRef` / `PayloadArena` | `skinny/crates/runtime/src/tape/{mod.rs:94,175,38, assembler.rs:42, offsets.rs}` |
| `StructLayout` / `OpenFrame` projection | `BackendRule` + tape-plan lowering (`TapeFlavor`, `render_rule`, `TapeEmit`, `SpanMark`) | `skinny/crates/codegen/src/lower/{tape_plan.rs:5, offset_tape.rs:16, event_tape.rs:16, eager_tape.rs, collapsed_stage.rs, sink_only.rs:89-92}` |
| `TapeStructBuilder` consumer trait | `EventGrammar` + `DocumentView` traits | `skinny/crates/runtime/src/tape/{event_grammar.rs:4, mod.rs:227}` |
| typed-value generator (core projection) | `RuntimeEmitterKind { CompiledLowering, RequestFacts }` | `skinny/crates/codegen/src/grammar_provider.rs:40-42` |
| CSS builder routing (`css_l4/builder.rs` arms) | hand-coded `W5C_REQUEST_FACT_PROFILES` profile array | `skinny/crates/codegen/src/lib.rs:336` |
| CSS regen target | skinny `regen_css.rs` -> `crates/runtime/src/grammars/css_l4_*` `*_fact_stream` plane | `skinny/xtask/src/regen_css.rs:41,51,59,69,77,87,95,105,113,122` |
| benched CSS "Track 1" | `track1_facts(input) -> Result<String, String>` | `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596-620` |
| 594-line hand-curated `css_l4.toml` | TOTALITY-tree only — `xtask/runtime-projections/css_l4.toml:1-594` (repo-root xtask, NOT consumed by skinny xtask; no `css_l4.toml` reference in `skinny/xtask/src/`) | `xtask/runtime-projections/css_l4.toml` |

The core paths are the **TOTALITY fold target**, not SK-V17 owner paths. Any
goalset citing a core-tree path as the benched surface is CH1-rejectable; the row
could read "met" in core while the benched skinny CSS path (`track1_facts ->
String`) is untouched.

**Benched corpus set** (the only corpora SK-V17 close can gate on):
`{bootstrap, tailwindcss, material-components-web, animate}`
(`skinny/crates/bbnf-bench/src/css_l4_corpus.rs:23,31,39,47`). `normalize` is
**NOT** in the benched set; `animate` is the structurally-regular corpus. Any
close condition naming `normalize` is unmeasurable until `normalize.css` is added
to the corpus and re-baselined.

**Grammar-witness reality** (binds Lock-14 "generalizes to JSON/Sheets/BBNF"
claims): only `json` and `sheets_witness` carry an `EventGrammar` witness
(`skinny/crates/runtime/src/grammars/{json,sheets_witness}/event_grammar_witness.rs`).
The Sheets witness is a stub — a 24-line `EventGrammar` trait impl with NO `.bbnf`,
NO parser, and NO `BackendRule` to project from (`sheets_witness/event_grammar_witness.rs`
24 LOC; codegen treats `google_sheets`/`bbnf` as fail-closed negative controls,
`skinny/crates/codegen/src/lib.rs:1075-1090`); BBNF-self carries no tape
witness. So tape-generality demonstrated **today** is JSON-witnessed only;
Sheets/BBNF generality is by-construction, not by-exercise. A `ValueRef`-cursor
projection over `sheets_witness` is therefore structurally non-dischargeable
(it has no shape to lower); the only exercised projection riders are JSON + CSS,
and a non-CSS-non-JSON rider (a real sheets value-grammar with `.bbnf` + `BackendRule`)
is an explicit **SK-V18 fold target**, not an SK-V17 exit gate.

---

## 1. Validated — load-bearing wins carrying forward into SK-V17

These are measured, scrutineer-ACCEPTed, parity-preserving wins landed on master
at or before `1c5bd7a25`. They are SK-V17 guard state and must not be reopened
without a stricter same-plane falsification.

| # | Win | Commit SHA | Evidence (measured / cited) |
|---|---|---|---|
| V1 | **JSON 51/51 strict baseline sustained** | inherited (SK-V14/V15 close) | `skinny/RESULTS.md:6-54` (json `direct_to_struct` GO rows, Track 1 > strict sonic, per-iter equality PASS); SK-V15 ledger `restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:10-12`. JSON tape path is the already->SOTA reference (`sk-v16-css-sota-tape-architecture.md:80-83`). |
| V2 | **CSS L4 grammar-derived typed provider** (no string-literal generated parser) | `ea8138056` (`feat(sk-v16-W5): structural CSS typed-summary parity with cssparser`) | Drives grammar-derived `CssL4Parser::parse` typed document, not the retired `CSS_GENERATED_RS` string literal; the W8R 646-line embedded tokeniser is gone (contrast SK-V15 invalidation `…sk-v15/…alpha-D…:26-31`). |
| V3 | **8-field structural equality with cssparser** (typed CSSOM, not summary) | `ea8138056`, refreshed `4de419f5e` (`test(sk-v16-W6): equality over structural fields; refresh substrate ids`) | `css_l4_w6_typed_retime`: `shared_summary_equal=true`, rules=10136, style=9561, sel=9561, decls=20043, track1_errors=0, cssparser_errors=0; track1 == cssparser exactly on all 4 SK-V14 corpora. Independently re-run cold: `sk-v16-w6tape-report.md:30`, `sk-v16-w6p2-o1-checkpoint-report.md:54-60`, `sk-v16-w6-speed-report.md:102`. preserve-rich-ast holds (dimensions=2963, colors=1169, functions=883, lists=6754 — not flattened: `sk-v16-w6tape-report.md:34`). |
| V4 | **Cross-grammar PEG codegen fix** (preserve PEG branch order in wrap byte-dispatch) | `2a85bf240` (`fix(sk-v16-W5): preserve PEG branch order in wrap byte-dispatch`) | StructDirect Wrap-Alt emitter hoisted a bounded-first-byte branch ahead of an earlier linear-routed branch, inverting PEG precedence; `atRule = mediaRule \| keyframesRule \| genericAtRule` was the canonical victim (`@media`/`@keyframes` misrouted to `genericAtRule`). Generic emitter fix, all 9 grammars regenerated, `regen --check` clean 9/9. |
| V5 | **O(1)-amortized speculative checkpoint** (scratch-stack hoist; ~14-16x, generic) | `8153236e8` (`perf(sk-v16-W6): O(1) speculative-checkpoint via scratch-stack hoist`) | O(N^2)->O(N): every growing per-frame container hoisted to builder-owned append-only scratch stacks; frames become `Copy` so checkpoint `stack.clone()` collapses to O(stack-depth) memcpy. Measured cold: `data/css/bootstrap.css` 0.617 -> 8.741 Mbps = **14.2x**; `bootstrap-5.3.3.min.css` 0.509 -> 7.957 Mbps = **15.6x** (`sk-v16-w6p2-o1-checkpoint-report.md:83-84`). Scrutineer ACCEPT with rollback-soundness proof (`:98-114`). **Generic** (all grammars, no CSS special-case). Note: implementer diverged from the design's watermark/deferral-journal scheme — proven unsound because checkpoints are routinely dropped without commit (commit 916 << checkpoint 1452 in generated CSS: `:44-47`). |
| V6 | **Flat-tape SUBSTRATE landed** (shared, grammar-agnostic, clean, additive) — but UNWIRED | `1c5bd7a25` (`feat(sk-v16-W6-tape): add shared flat-tape runtime substrate`) | **Benched skinny substrate** (verified module names, NOT the doc's core-tree `record/arena/cursor` siblings): `skinny/crates/runtime/src/tape/` = `mod.rs` (`PayloadArena` `:38`, `Tape<'input>` `:94`, `ValueRef<…>` `:175`, `trait DocumentView` `:227`) + `assembler.rs` (`TapeBuilder<'input>` `:42`, `push_plain_offset` append) + `offsets.rs` + `event_grammar.rs` (`trait EventGrammar` `:4`). O(1)-truncate `PayloadArena`, lazy `ValueRef` cursor, `Copy` checkpoint = `offsets.len()` marker. **No-StructRegistry guard asserts on the measured tree:** grep over `skinny/crates/` for `StructRegistry`/`Arena<G>`/`Builder<G>`/`StructLayout`/`OpenFrame`/`TapeStructBuilder` returns zero (AZ-IV failure mode absent on the benched surface). **Caveat (carries to §3 still-open):** confirmed UNWIRED — zero usage of `Tape`/`TapeBuilder`/`ValueRef` in any benched CSS parse path; the benched CSS "Track 1" still rides `track1_facts -> Result<String,String>` (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596`); CSS `generated.rs` emits a fact-stream String (`emit_fact_stream` / `emit_full_parse`, `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5,61`), zero SIMD. |

**Locks reaffirmed (constraint, not new win):** Lock 1 substrate-union and Lock
14 grammar-neutrality remain load-bearing. The flat tape (V6) is admissible only
as the single substrate, not a parallel/second tape or public `UnionTape`
(`restart/locks/LOCKS.md:585`). Generated files require line-1 provenance +
byte-equivalent regen/check + same-wave replacement provider before any
retirement (`restart/locks/LOCKS.md:593`).

---

## 2. Invalidated / Superseded — claims refuted by SK-V16 measurement

These are routes empirically falsified during SK-V16. They must NOT carry
forward as SK-V17 hypotheses (CH3 regression guard).

| # | Refuted claim | Evidence (measured) | Disposition |
|---|---|---|---|
| I1 | **Per-lever micro-optimization on the eager OpenFrame path moves CSS throughput.** (The eager path is the TOTALITY css_l4 builder; the benched skinny analogue is the fact-stream `track1_facts` String path — both refute micro-tuning.) | The W6 hypothesis named "the 7-way speculative dimension dispatch" as the dominant lever; lever-1 (`85b4edd88`, dimension suffix-class dispatch — grammar-expressed, parity-clean) removed exactly that cost and throughput did **not** move: track1 3.093 -> 3.178 Mbps (~1.02x, cold-run noise). `sk-v16-w6p1-dimension-dispatch-report.md:80-102`. | **SUPERSEDED.** The cost lives in typed-value materialization + arena/builder indirection (core) / fact-stream String serialization (skinny), not dimension dispatch. Refutes its own W6 prediction. |
| I2 | **The flat StructLayout LazyLock hoist is a CSS speedup.** | `f4b6f757c` removed a real dead `String::from`+`Vec::new` per speculative entry but is throughput-neutral within cold-run noise; profiler attributed >68% self-time to the inlined per-branch number re-parse, NOT the layout alloc. `sk-v16-w6-speed-report.md:40-46`. | **VALID cleanup, INVALID as speedup.** Keep the hoist (dead alloc removal); do not credit it with throughput. |
| I3 | **The W5 "2.46x over lightningcss" summary margin is the typed result.** | The 2331 Mbps figure was the String-summary lane that **retains nothing**; the real rich-typed CSSOM is 3.09 Mbps (~0.37% of lightningcss, ~269x slower). `sk-v16-w6-speed-report.md:84-87`. | **INVALIDATED.** Summary margins do not transfer to the typed lane. Honest typed figure is the only admissible number. |
| I4 | **Lever-2 (arena-offset / stack-length-snapshot checkpoint) is unblocked by lever-1.** | Lever-1 was supposed to make deposits append-only-safe; it did not — the inner `unitSuffix` Alt still speculates with per-branch checkpoint/rollback (unit-class first-byte sets overlap: `d`=>deg\|dpi\|dvw, `r`=>rad\|rem, `m`=>ms\|mm, `s`=>s\|svw). `sk-v16-w6p1-dimension-dispatch-report.md:136-157`. | **BLOCKED.** Not attemptable as scoped; superseded by the commit-by-construction spine route (§3). |
| I5 | **AZ-IV eager value-substrate / StructRegistry+Arena<G>+Builder<G>.** | Pre-block carry-forward. 118x regression (eager per-leaf payload materialization) + 28-65x registry indirection (983x css bootstrap, 10583x tailwind WATCHDOG). `sk-v16-css-sota-tape-architecture.md:46-66`. | **PRE-BLOCKED (no re-open).** The tape was right; the AZ-IV value substrate was wrong. Never eager-by-default; never a registry lookup in the per-leaf hot path. |
| I6 | **The "28-118x regression was caused by the restart."** | Timeline error: `post-AZ-IV.json` (`cb14970f`, 2026-05-02) measures intra-A-series self-regression; the restart began `b5eb4651c`/`a5145a0bb` on 2026-05-03, *after*. The restart inherited and deleted the cause. `sk-v16-css-sota-tape-architecture.md:21-26`. | **CORRECTED RECORD.** Not a candidate; a provenance correction so SK-V17 does not mis-attribute the floor. |
| I7 | **W6 single-sample telemetry is statistically adequate.** | The W6 harness `W6_SAMPLE_COUNT=1` single cold sample produced ~3.1 (build) vs ~70 (scrutineer) Mbps for the same path — run-to-run variance of ~20x. `sk-v16-w6tape-report.md:40-47`, `sk-v16-w6-speed-report.md:13`. | **INVALIDATED as a telemetry basis.** SK-V17 telemetry MUST use N>=50 cold samples + median (ground-truth mandate). |

**Pre-block families carried forward verbatim** (must NOT re-open, per
`sk-v16/SYNTHESIS.md:71-87`): CSS fact-stream String serialization /
`emit_fact_stream` / `CSS_GENERATED_RS` as admission proof; the hand-coded
`W5C_REQUEST_FACT_PROFILES` CSS profile array (`skinny/crates/codegen/src/lib.rs:336`,
itself a Lock-14 phrase-#1 construct — see O5); the 24-row broadcast measurement;
brace-counter summary vs lightningcss CSSOM; fixture/FNV contrivances; x86/AVX
paths; the eager `Vec<Vec<T>>`+split_off arena as a destination (TOTALITY
`css_l4/arena.rs`); sidecars / second tapes / public `UnionTape` / Track1==Track2
dishonesty.

---

## 3. Still-Open — SK-V17 candidate routes (with framing constraints)

These are the unmet items from the SK-V16 close. Each is a candidate for SK-V17;
each carries the grammar-neutrality (Lock 14) and substrate-union (Lock 1)
framing it must satisfy, AND every owner path is bound to the **benched skinny
surface** per §0 — NOT the architecture-doc core-tree paths.

The single governing fact: **there are zero ADMITTED typed CSS rows in
`skinny/RESULTS.md`; the only CSS rows present are 24 `css_l4/*/direct_to_struct/main`
W8R broadcast diagnostics, classified `not_admitted:SK-V15-W0-broadcast-diagnostic`
/ `AUDIT-FALSIFIED`** (`skinny/RESULTS.md:112-135`, grep-verified count = 24; tuple
`track1_mbps=2319.041; cssparser_mbps=2362.037; lightningcss_mbps=929.281` —
a full-parse-summary plane the spec forbids for admission). The retimed typed CSS
figure (the honest number) is **CSS rich-typed track1 ~70 Mbps cold** (canonical
N=100 master `1c5bd7a25`; the typed retime is 3.093 Mbps at the
`css_l4_w6_typed_retime` plane, `sk-v16-w6-speed-report.md:164`), **~14x slower
than lightningcss** (~974 Mbps full-CSSOM canonical; the W6 BUILD plane measured
809.977 / 833, `sk-v16-w6-speed-report.md:58,164` — the same-run median is the
SK-V17 close comparator, not a fixed literal); cssparser token-scan ~2539 Mbps.
The gap is the materialization/scan architecture, not micro-tuning (proven by
I1/I2).

**Falsifiable NO-GO thresholds (per α-E C1-C4, bound here so still-open
candidates are not estimate-closed downstream):** O1 ≥30 Mbps PASS / <20 NO-GO;
O3-NEON ≥80 Mbps regular-corpus PASS / <60 NO-GO; O4 ≥300 Mbps PASS with
>same-run-lightningcss cross plausible on regular corpora / <200 NO-GO; tailwind
closeout = cross-or-honest-profiled-residual.

| # | Open candidate | Why open / current state | SK-V17 framing constraint (benched skinny surface) |
|---|---|---|---|
| O1 | **Tape WIRING** (connect V6 substrate to the benched parse path) | The substrate is landed but unwired dead code (V6 caveat). The benched CSS "Track 1" still rides `track1_facts -> Result<String,String>` (`nonjson_css_l4.rs:596`); CSS `generated.rs` emits a fact-stream String (`css_l4_declaration_values/generated.rs:5,61`). Wiring the tape into the benched path is unstarted. | **Owner paths (skinny):** `skinny/xtask/src/regen_css.rs` (emits to `crates/runtime/src/grammars/css_l4_*` `*_fact_stream` plane today, lines 41-122 — flip to tape-record emission); `skinny/crates/runtime/src/grammars/css_l4_*/` (replace eager String emission); `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}` (consume `TapeBuilder`/`ValueRef`, NOT core `TapeStructBuilder`/`TapeCursor`); `skinny/crates/codegen/src/lower/{offset_tape.rs,tape_plan.rs}` (emit CSS tape ops as JSON does). Grammar-neutral by construction: the tape build dispatches on the `BackendRule`/tape-plan shape, no route strings. **Generality is JSON-WITNESSED only** — `json` carries an `EventGrammar` witness; `sheets_witness` is a stub; bbnf-self has no tape witness (§0). The "JSON/sheets/bbnf already implement the same builder trait" claim is downgraded to "JSON exercises the tape; Sheets/BBNF are by-construction-not-by-exercise." **Anti-relabel pruning gate (Lock 14, `LOCKS.md:349`):** any CSS-specific routing the eager builder encodes (declaration/selector/aggregate/numeric/function/color rule-id sets, 0/1/N value-list collapse, hex packing, color component order) must be threaded as grammar-derived DATA recoverable from the `.bbnf` rule + `BackendRule` shape — NOT per-rule-id match arms; **wave FAILS if CSS needs match arms / hand-curated packing constants JSON does not, OR if the CSS regen profile array does not trend toward the JSON shape.** **Falsifiable gate:** ≥30 Mbps PASS (alloc-floor removal, arch-doc Wave 2 estimate `:373`) / <20 NO-GO (REJECT, record in REDRESS, re-frame as "speculative floor is the wall" -> escalate O4). 8-field equality EXACT (10136/9561/9561/20043) must hold on the new typed benched row. |
| O2 | **Lazy view + cursor API** (typed projection over tape, not eager tree) | Today the benched CSS typed value is reconstructed eagerly; the lazy `ValueRef` cursor (`skinny/crates/runtime/src/tape/mod.rs:175`) is unused for CSS. `CssView` navigates retained eager values, not tape offsets. | Layout-driven projection generated from the `BackendRule`/tape-plan shape (skinny `lower/tape_plan.rs` `TapeEmit`/`SpanMark`), emitting a lazy `ValueRef`-cursor typed accessor set **isomorphic to JSON's `value_from_ref`** (`skinny/crates/runtime/src/grammars/json/value.rs:143`) over the EXISTING `Tape`/`ValueRef` — **no new cursor/builder type is introduced** (a second cursor type would be a Lock-1 type-ambivalence violation). preserve-rich-ast holds because the typed CSSOM is reconstructed on demand, not flattened. **Generality is exercised on CSS+JSON only — sheets_witness is NOT a projection target.** `sheets_witness` is a 24-line `EventGrammar` trait impl (`skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs`, 24 LOC; `mod.rs` 1 LOC) with NO `.bbnf`, NO parser, and NO `BackendRule` to project from; codegen treats `google_sheets`/`bbnf` as fail-closed negative controls (`skinny/crates/codegen/src/lib.rs:1075-1090`). A `ValueRef`-cursor lazy view over `sheets_witness` is therefore structurally non-dischargeable, NOT a live disjunction. The riders that emit lazy views in SK-V17 are **JSON + CSS only**; non-CSS-non-JSON projection (authoring a real sheets value-grammar with a `.bbnf` + `BackendRule`) is an explicit **SK-V18 fold target**, not an SK-V17 exit gate. Same-wave coupled with O1 (the lazy view IS the typed Track-1 consumer). |
| O3 | **NEON aarch64 pre-scan hot leaf** (dav1d-style, no x86/SVE) | CSS uses ZERO SIMD — `css_l4_declaration_values/generated.rs` has 0 `simd`/`neon`/`vqtbl`/`core::arch` (grep clean). Hot leaves (core-tree profile): `find_component_delim` ~56% self-time, `consume_balanced_at` ~10%, scalar `regex_scan` ~3% (`sk-v16-css-sota-tape-architecture.md:254-260`) — **these %% are from the core-tree profile and MUST be re-confirmed on the benched skinny path (S-P1 re-profile; feedback `actual-profiling`) before any kernel lands.** Orphan kernel present in skinny: `digit_mac` udot/sdot (`asm!` at `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:39-40` udot, `:62-63` sdot; `parse_4_digits_dotprod` `:27` never called in prod). i8mm: **grep-clean absent from skinny.** | aarch64-only (M5 Max: NEON+dotprod+fp16, NO SVE — SVE paths would be dead code). **Owner surface (skinny):** `skinny/crates/bbnf-simd/src/dispatch.rs` (`select_classifier` / `PrimitiveKernels` OnceLock fn-table, the grammar-general entry — extend, do not add a subsystem); `skinny/crates/runtime/src/grammars/css_l4_*/` (add `scan.rs` isomorphic to `json/scan.rs`). Vectorize a **grammar-general** leaf set keyed on the grammar's delimiter/alphabet sets, never CSS-specific (`byte_class_index_64` via `vqtbl4q_u8` lo6-table; neutrality vehicle is `select_classifier(alphabet)` + `lo6_table_admissible`, `dispatch.rs:42,101`). NEON produces only a `Vec<u32>` structural index; the tape consumes it — speed from the scan, never from dropping structure. **Split per α-E C4a/C4b:** (a) udot-orphan wiring is LOW risk (scalar + checkasm present) and admits unconditionally; (b) any NET-NEW i8mm kernel is MEDIUM-HIGH and GATED behind the Wave-5 re-profile proving the digit leaf is top-N tailwind self-time — else it does NOT land (no orphan kernel). Per primitive: scalar reference + checkasm parity + same-wave consumer (CH4). PREMATURE before O1/O2 land (no tape decode path to pre-scan into). **Falsifiable gate:** ≥80 Mbps regular-corpus (`animate`/`bootstrap`) PASS / <60 NO-GO. |
| O4 | **Commit-by-construction structural spine** (no speculative rollback on the backbone) | The speculative-descent architecture is itself the residual wall after the alloc floor: ~31% own-compute is speculative branch machinery + scalar scan + utf8 revalidation (`sk-v16-css-sota-tape-architecture.md:320-328`). Lever-2 micro-route is BLOCKED (I4); the structural route supersedes it. | **Owner paths (skinny):** `skinny/crates/codegen/src/lower/tape_plan.rs` (`AltMode` — emit commit-mode for non-depositing Alts); `skinny/crates/codegen/src/lower/{offset_tape.rs,event_tape.rs}` (no-checkpoint spine emission); `skinny/crates/runtime/src/tape/assembler.rs` (the O(1) `offsets.len()` checkpoint marker, banked V5 — this candidate REMOVES checkpoints where provably unneeded, does not add a mechanism). Emitter emits NO checkpoint for pure-lexical keyword-dispatch Alts that deposit nothing structural; backtracking survives only on true ambiguous leaves. Grammar-general (the emitter, not a CSS patch). Builds on V5's O(1) checkpoint + the sound-rollback proof. **Falsifiable gate:** ≥300 Mbps PASS, first cross of the same-run-measured lightningcss median PLAUSIBLE on regular corpora (`animate`/`bootstrap`) / <200 NO-GO. tailwind hardest (cross-or-honest-residual). |
| O5 | **Codegen unification + overfit removal** (typed-value emitter for every grammar) | skinny `RuntimeEmitterKind` has only `{CompiledLowering, RequestFacts}` (`skinny/crates/codegen/src/grammar_provider.rs:40-42`) — no generalized typed-value generator; JSON value/view via JSON-specific templates; CSS routes through `RequestFacts` driven by the hand-coded `W5C_REQUEST_FACT_PROFILES` array (`skinny/crates/codegen/src/lib.rs:336`). `generated_real_typed.rs` (4941 lines, **148** `fn parse_` — grep-counted, NOT 187) carries hand-tuned per-corpus capacity constants — textbook overfit. The 594-line hand-curated `css_l4.toml` is a **TOTALITY-tree artifact** (`xtask/runtime-projections/css_l4.toml:1-594`, repo-root xtask; no `css_l4.toml` reference in `skinny/xtask/src/`) — it is the fold target, not a skinny owner path. | Wave-1 substrate for everything (no perf yet): one projection-driven generator emitting document/value/view/visitor for every grammar from the `BackendRule`/tape-plan shape; retire the JSON-specific template special-case; quarantine `emit_fact_stream` to diagnostic-only. **Retire-list (Lock-14 phrase-#1 constructs that must be deleted):** the hand-coded `W5C_REQUEST_FACT_PROFILES` CSS profile array (`skinny/crates/codegen/src/lib.rs:336`); the fixture-named `generated_real_typed.rs` parse fns + per-corpus capacity constants; and (TOTALITY fold) the 594-line `css_l4.toml` catalogue. **The skinny-greppable exit gate is grammar-derivation, NOT TOML-LOC count:** every residual CSS routing entry must name the `.bbnf` rule it derives from, and the CSS regen profile array (`regen_css.rs:45-153`) must trend toward the JSON emitter shape. The 594-line `css_l4.toml` LOC convergence is a TOTALITY metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17 close/exit gate (SYNTHESIS §0.1). Pre-block: dirty generated files as close proof; string-literal generated proof. |

---

## 4. Demoted / Closed-Out (informational)

- **Pattern H collapse** — SK-V16 close required 67-file count held + provenance
  advance to generator-owned collapse (`sk-v16/SYNTHESIS.md:38`). Provenance
  discipline closed in SK-V15; full generator-owned collapse not proven. Folds
  into O5 (codegen unification) rather than standing alone.
- **FNV** — remains bench-only quarantine; production migration stays blocked
  without a new typed-semantic contract (`sk-v16/SYNTHESIS.md:41`). Not an
  SK-V17 throughput candidate.
- **Decision Engine / 5 BackendShape lowerers** — validated SK-V15 close
  evidence, grammar-neutral, preserved as canon (`sk-v16/SYNTHESIS.md:39,53`).
  No SK-V17 action; guard only.

---

## 5. Ledger Text — the SK-V17 starting posture

SK-V16 delivered the *substrate and the honest diagnosis*, not the CSS >SOTA
beat. The durable wins are grammar-derived CSS (`ea8138056`), 8-field structural
equality with cssparser (`ea8138056`/`4de419f5e`), the cross-grammar PEG
branch-order fix (`2a85bf240`), the generic O(1) checkpoint (`8153236e8`, 14-16x
on bootstrap, sound), and the landed-but-unwired flat-tape substrate
(`1c5bd7a25`, at `skinny/crates/runtime/src/tape/` — `TapeBuilder`/`Tape`/
`ValueRef`/`PayloadArena`, NOT the doc's core-tree `TapeStructBuilder`). The
measured truth at bracket HEAD: **zero admitted typed CSS rows in RESULTS.md** —
only 24 falsified W8R broadcast diagnostics
(first row `track1 2319.041 / cssparser 2362.037 / lightningcss 929.281`,
`AUDIT-FALSIFIED`, `skinny/RESULTS.md:112-135`, grep-verified count = 24); the honest retimed typed figure is
CSS rich-typed track1 ~70 Mbps cold, ~14x under lightningcss's ~974 Mbps
full-CSSOM (same-run median is the SK-V17 close comparator, not a fixed literal).

The decisive SK-V16 lesson — banked, not to be relitigated — is that
**per-lever micro-optimization on the eager path does not move the CSS floor**
(lever-1 and the LazyLock hoist were both refuted by measurement: I1, I2). The
gap is architectural. SK-V17 is therefore the **tape-wiring + lazy-projection +
NEON-prescan + commit-by-construction-spine** tranche, executed as the unified
tape/layout/projection model generalized across ALL grammars
(`sk-v16-css-sota-tape-architecture.md`), **but with every owner path bound to
the benched skinny tree** (`skinny/crates/runtime/src/tape/`,
`skinny/crates/codegen/src/lower/`, `skinny/xtask/src/regen_css.rs`,
`skinny/crates/runtime/src/grammars/css_l4_*/`) — the core paths are the TOTALITY
fold target, not SK-V17 owner paths. Telemetry is rebound to N>=50 cold samples +
median (I7), benched against the corpus set `{bootstrap, tailwindcss,
material-components-web, animate}` (`normalize` is absent — `animate` is the
regular corpus to bench first; do NOT gate close on `normalize` until it is added
to `css_l4_corpus.rs` and re-baselined).

The ordered candidate spine is O5 (codegen unification, substrate) -> O1 (tape
wiring) + O2 (lazy view, coupled) -> O3 (NEON pre-scan, after a tape decode path
exists; udot-orphan unconditional, i8mm gated on re-profile) -> O4
(commit-by-construction spine, the lightningcss-cross lever). Every candidate
must generalize beyond CSS to JSON (witnessed today) / Sheets / BBNF-self
(by-construction, witnessed only when the generator emits a non-CSS-non-JSON
rider — Lock 14), ride the single `Tape`/`ValueRef` substrate (Lock 1, no second
cursor/builder type), preserve-rich-ast, and carry scalar-ref + checkasm +
same-wave-consumer per primitive (CH4). No contrivance, no overfit, no x86.
