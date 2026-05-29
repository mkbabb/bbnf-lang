# SK-V17 Grand Synthesis

Date: 2026-05-29.

Status: Pass Alpha (cycle V4, alphaF) contract for SK-V17, folding the V1 + V2 + V3
CHALLENGE dispositions (`research/alpha-hardening/V1,V2,V3/CH1..CH7` +
`CONSOLIDATED`). The V3 substantive folds are carried (the V2 CH2-V2-F1 (b′)
sheets_witness-non-dischargeable repair, the 6→24 broadcast-count reconciliation, the
`nonjson_css_l4.rs:776` definition citation, the `regen_css.rs:45-153` seam-flip site,
and the `css_l4.toml`-is-totality SK-V18-fold demotion). V4 folds the two residual V3
count-correction REVISEs (V3-CH1-a stale meta-note rewritten to "all cohort artefacts
state 24 as of V3; V2 undercount resolved"; V3-CH1-b grep-substring mislabel corrected
to "25 substring matches, of which 24 are `^| css_l4/` table rows and the 25th :154 is
a prose REDRESS-127 companion reference, not a row") and the F1 orphan (alphaD:154 O5
relabel to grammar-derivation-not-TOML-LOC). CHALLENGE V3 = 59/61 ACCEPT (96.7%),
above the §3Z ≥95% bar.
SK-V16 closes at master HEAD `1c5bd7a25` with the CSS grammar-derived provider,
8-field structural equality with cssparser, the cross-grammar PEG codegen fix, the
O(1) generic checkpoint, and a shared flat-tape SUBSTRATE that is LANDED BUT
UNWIRED. The W6 speed report (`restart/audit/skinny-impl-overfit/sk-v16-w6tape-report.md`)
records the close honestly: the >SOTA bar is NOT met; CSS typed parsing remains
~14x slower than lightningcss; the tape decodes no grammar yet.

SK-V17 is the tape-activation + projection-generalization + NEON hot-leaf tranche.
Its subject: **CSS L4 typed parsing must reach >SOTA — BEAT lightningcss (the fair
full-CSSOM-materializing comparator) on regular corpora, with honest tailwind
handling — via the UNIFIED TAPE / LAYOUT / PROJECTION model generalized across ALL
grammars + dav1d-style aarch64 NEON hot leaves. No x86. preserve-rich-ast. No
contrivance / overfit. Fully generalized for SKINNY, foldable into TOTALITY.**

## Benched-surface note (load-bearing; binds every surface citation below)

This contract gates the **benched skinny tree** (`skinny/crates/`), NOT the totality
tree (`crates/core/`, `crates/ir/`). The distinction is verified-binding (CH1-R1):

- `StructLayout`, `OpenFrame`, `CssArena`, `TapeStructBuilder`, `begin_compound`,
  `bbnf_ir::registry::struct.rs`, `css_l4/builder.rs:274` are **grep-clean-absent
  from `skinny/crates/`** (verified). They exist only in the totality tree. Any
  close-condition gate keyed on them could be "met" in `crates/core/` while the
  benched CSS path is untouched — that is wrong-tree dishonesty and is REJECTed.
- The benched flat-tape substrate is `skinny/crates/runtime/src/tape/`:
  `mod.rs` (`Tape` :94, `ValueRef` :175, `PayloadArena` :38, `DocumentView` trait
  :227), `assembler.rs` (`TapeBuilder` :42, `push_plain_offset` :71),
  `event_grammar.rs` (`EventGrammar`/`AnyGrammar` traits, `mod.rs:11`), `offsets.rs`.
  This is the LIVE benched tape (JSON rides it); `crates/core/src/runtime/tape/`
  (`record/arena/cursor/mod`) is the TOTALITY fold target, NOT an SK-V17 owner path.
- The benched CSS "Track 1" is today a String:
  `track1_facts(input) -> Result<String,String>` (`nonjson_css_l4.rs:596`, calling
  `track1::parser::parse`), produced by `emit_fact_stream`
  (`css_l4_*/generated.rs:5`). The skinny layout-equivalent of "StructLayout-driven
  projection" is the codegen lowering: `BackendRule` + `skinny/crates/codegen/src/
  lower/tape_plan.rs` (`TapeFlavor`, `render_rule`, `TapeEmit`/`SpanMark`) +
  `lower/{offset_tape,event_tape,eager_tape}.rs`. CSS rides
  `RuntimeEmitterKind::RequestFacts` (`grammar_provider.rs:40`, selected
  `lib.rs:291`); JSON rides `RuntimeEmitterKind::CompiledLowering` (`lib.rs:282`,
  tape-emitting). The CSS-routing live today is the hand-coded
  `W5C_REQUEST_FACT_PROFILES` array (`codegen/src/lib.rs:336`) — itself a Lock-14
  phrase-#1 construct that SK-V17 must retire, not extend.
- The NEON entry is `skinny/crates/bbnf-simd/src/dispatch.rs` (`select_classifier`
  :42, `lo6_table_admissible` :101, `PrimitiveKernels`). The udot orphan is
  `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs` (`parse_4_digits_dotprod` :27,
  `udot` :40 — scalar fallback + asm present, never called in prod). No i8mm kernel
  exists in skinny source (i8mm appears only as a target CPU feature, not a kernel).

The totality-tree symbols (`StructLayout`, `OpenFrame`, `CssArena`,
`TapeStructBuilder`) are the **design-intent fold target**, expressed in skinny as
`BackendRule` + `tape_plan.rs` + the skinny `Tape`/`ValueRef`. The TOTALITY adoption
of this model is SK-V18 work, not SK-V17 owner paths.

## Authority

- `restart/audit/skinny-impl-overfit/sk-v16-css-sota-tape-architecture.md`
  (the five-investigation synthesis: A-series flat-tape archaeology, unified
  tape/layout/projection design, value-API audit, aarch64 NEON plan, honest
  feasibility band). Its core-tree path citations are translated to the skinny
  benched tree per the benched-surface note above.
- `restart/audit/skinny-impl-overfit/sk-v16-w6tape-report.md`
  (the honest W6 close: substrate landed, unwired, no speedup).
- `restart/audit/skinny-impl-overfit/sk-v16-w6tape-conversion-report.md`,
  `sk-v16-w6-speed-report.md`, `sk-v16-w6p1-dimension-dispatch-report.md`,
  `sk-v16-w6p2-o1-checkpoint-report.md`.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md` +
  `restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md`.
- `restart/skinny/tranches/sk-v16/SYNTHESIS.md`,
  `restart/skinny/tranches/sk-v16/HANDOFF.md`,
  `restart/skinny/tranches/sk-v16/SPEC.md`.
- `restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` +
  `research/alpha-hardening/V1/{CH1..CH7,CONSOLIDATED}.md` (the V2 fold source).
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/HANDOFF.md`,
  `restart/locks/LOCKS.md` (Lock 1 substrate-union + Lock 14 grammar-neutrality
  load-bearing; Lock 6/14 generated-output deletion clause; Lock 16 SIMD parity).
- `restart/prompts/pass-contracts/PASS-ALPHA.md`,
  `restart/prompts/ORCHESTRATOR.md`.

The active user pin controls gate conflicts: only G-Omega is mandatory during this
execution; G-Alpha auto-passes. The detailed wave plan (PASS-ALPHA §4.4) is deferred
to skinny pass S-P3 in `sk-v17/SPEC.md`, which consumes the goalset set here.

## Section 0 - Close Condition And Goalset

### 0.1 Close condition

SK-V17 closes only when the following are all true. Every surface citation is the
benched skinny tree (per the benched-surface note); each tape/layout gate is written
so it is verifiable by grepping `skinny/crates/`, not `crates/core/`.

| Gate | Close condition |
|---|---|
| JSON guard | 51 / 51 JSON rows remain admitted, strict, same-plane on Apple M5 Max / aarch64; touched rows are re-run cold; the tape activation moves no JSON row out of A/GO. JSON already rides the lazy-offset tape (`skinny/crates/runtime/src/tape/`); it is the proof the model is >SOTA and the regression tripwire. |
| Tape activation (not dead code) | The shared flat-tape substrate at `skinny/crates/runtime/src/tape/` (`Tape`/`ValueRef`/`TapeBuilder`/`PayloadArena`, landed `1c5bd7a25`, today UNWIRED for CSS) becomes the LIVE parse substrate for the benched CSS Track 1 (`track1::parser::parse`, reached via `nonjson_css_l4.rs:596`). PROOF: `Tape`/`ValueRef`/`TapeBuilder` appear in the benched CSS parse path and in `skinny/crates/runtime/src/grammars/css_l4_*/`; a grep over those files returns non-zero; `PayloadArena` write/alloc counters (per alphaC §1) confirm the parse emits into the tape rather than into a fact-stream String. The benched Track 1 stops returning `String` (retires `emit_fact_stream`, `generated.rs:5`) and the W6 "unwired dead code" finding is retired with measurement. No new cursor/builder type is introduced — the EXISTING skinny `Tape`/`ValueRef`/`TapeBuilder` is the only substrate (Lock 1, no second tape). |
| Layout-driven projection | The lazy-view accessor generator exists in `skinny/crates/codegen/` and emits `document/value/view/visitor` for the CSS grammars by walking the SAME `BackendRule` shape the parser emits, lowered via `skinny/crates/codegen/src/lower/{tape_plan.rs,offset_tape.rs,event_tape.rs}` (the skinny equivalent of "StructLayout-driven projection"; there is no `StructLayout`/`OpenFrame` in skinny). The generated lazy accessors are `ValueRef`-cursor reads isomorphic to JSON's `value_from_ref` (`json/value.rs:143`) over the existing skinny `Tape`/`ValueRef`. The CSS routing the fact-stream encodes today — the hand-coded `W5C_REQUEST_FACT_PROFILES` array (`codegen/src/lib.rs:336`) and the per-rule routing (declaration/selector/aggregate/numeric/function/color rule sets, 0/1/N value-list collapse, selector span-vs-single, dir-pseudo synthesis, hex packing, color-component order) — is RETIRED as hand-coded branching and DERIVED from the `.bbnf` grammar / `BackendRule` shape, preserved as DATA in the tape-plan lowering, NOT lost and NOT re-hardcoded (Lock 14). The SK-V17 close gate for this is keyed strictly to skinny-greppable facts: `W5C_REQUEST_FACT_PROFILES` retired (`codegen/src/lib.rs:336`), no per-rule-id match arms in the skinny generic crates that JSON does not need, every residual CSS routing entry names the `.bbnf` rule it derives from, and the CSS regen profile array (`regen_css.rs:45-153`) trends toward the JSON emitter shape. (The 594-vs-34-line `css_l4.toml`-vs-`json.toml` asymmetry is a TOTALITY artefact — `css_l4.toml` is grep-clean-absent from `skinny/`, it lives only at the repo-root totality tree — so its LOC convergence is an SK-V18 totality-fold metric, INFORMATIONAL only, NOT an SK-V17 close gate; gating an SK-V17 close on a non-benched totality file would be the wrong-tree dishonesty this contract REJECTs.) Generality is exercised on JSON (the existing generated `value_from_ref` rider) and CSS (the new rich rider) — those are the two projection riders SK-V17 exercises. `sheets_witness` is a 24-line `EventGrammar` byte-classification witness with NO `.bbnf` source / parser / `BackendRule` shape to walk, so it CANNOT serve as a projection-generator exercise; non-CSS-non-JSON projection generality is asserted-by-construction with proof deferred to SK-V18 (see §0.4 generality clause). |
| CSS typed equality (gate before speed) | Track 1 typed CSS summary equals cssparser same-workload typed summary BEFORE any speed counts — EXACT 8-field structural equality (`rules=10136, style=9561, sel=9561, decls=20043`, `track1_errors=0`, `cssparser_errors=0`, 4/4 corpora, as banked at `1c5bd7a25`). Equality is re-proven after the tape conversion on the NEW typed benched path, not assumed. |
| preserve-rich-ast | The typed CSSOM (`CssColor`, `CssDimension`, `CssLength`, `CssFunction`, `Selector`, `CssRule`, `CssTypedValue`) is produced by lazy `ValueRef`-view projection over the tape, NOT flattened to spans for speed and NOT materialized eagerly (no per-leaf `Box::new`, no eager value tree). Value-plane population parity holds (dimensions, colors, functions, lists counts match the eager-tree baseline). preserve-rich-ast is non-negotiable. |
| CSS >SOTA on regular corpora | On a per-corpus basis with N>=50 cold samples + median, CSS L4 typed Track 1 BEATS lightningcss full-CSSOM on the structurally-regular benched corpora (animate, bootstrap — the two regular corpora in the SK-V14 benched set; `normalize` is NOT in the benched set and is not gated). Per-corpus median-Mbps thresholds are specified in §0.5. lightningcss is the materializing comparator (full CSSOM build, the fair bar), re-baselined same-run, NOT cssparser token-scan (which materializes nothing and is a flaw probe). |
| Honest tailwind handling | tailwindcss (deeply nested, many short rules — the hardest corpus) is benched cold adversarially with N>=50 + median. If it crosses the lightningcss bar, it admits; if it lands short, the residual gap is REPORTED honestly with the hot-leaf attribution and recorded in REDRESS, NOT paper-closed and NOT hidden behind a corpus average. No corpus-average claim substitutes for per-corpus medians. |
| Telemetry honesty (N>=50 fix) | The W6 harness single-sample (`W6_SAMPLE_COUNT=1`) is statistically inadequate and is retired. The SK-V17 bench harness takes N>=50 cold samples per corpus per workload and reports the MEDIAN Mbps (not mean, not single-sample, not warm/cached). lightningcss is wired as a same-run, same-plane, full-CSSOM-materializing comparator re-baselined this run — NOT against a fact-stream. (`assert_lightningcss_strict_equality`, defined `nonjson_css_l4.rs:776` in the bench src, call sites `:1057,:3460` + bench harness `benches/nonjson_css_l4.rs:8`, asserts against a fact stream today and is retired; the comparator must build CSSOM.) |
| NEON hot-leaf union | Any SIMD is profile-first (re-confirmed on the benched tape path, not inherited from the core-tree profile — the architecture profile's `find_component_delim` ~56% / `consume_balanced_at` ~10% must be RE-PROFILED on the benched skinny path before any kernel lands, per actual-profiling), scalar-referenced, checkasm/parity verified, same-wave consumed, aarch64-only (NEON + optional dotprod/i8mm; NO x86, NO SVE — Apple cores have no SVE). The grammar-general leaf set routes through `skinny/crates/bbnf-simd/src/dispatch.rs` `select_classifier`/`lo6_table_admissible` single entry (the neutrality vehicle, alphaE C2), produces only a `Vec<u32>` structural index, and the tape consumes it. The NEON leaf must exercise at least one non-JSON grammar (Lock 14). NEON is gated behind tape activation — there is no structural index to pre-scan into until the tape decodes CSS. |
| Generated-state cleanliness | The dirty generated CSS / real-typed files (8 git-dirty at bracket: `bbnf-bench/src/generated_real_typed.rs`, 7 `css_l4_*/generated.rs`) are cleanly regenerated as fresh generator output (`cargo xtask regen --check` 9/9, exit 0). Generated files are output of fresh regen, never hand-patched (Lock 6/14). |
| Foldable into TOTALITY | The unified tape/layout/projection model + NEON leaf set are structured so the TOTALITY tree (`crates/core/src/runtime/tape/`, the `StructLayout`/`OpenFrame` design-intent target) can adopt them in SK-V18. Generality is demonstrated by-construction only where exercised: JSON is the existing witness, CSS is the first-mover hardest-routing rider; the non-CSS-non-JSON projection rider (Sheets/BBNF-self) is the SK-V18 generality proof (`sheets_witness` cannot serve as an SK-V17 projection exercise — it has no `BackendRule` shape; §0.4). |
| PASS-IMPL close audit | The close audit accepts every axis or records row-level intrinsic-block proof with measurement. |

### 0.2 Starting state (SK-V16 close, HEAD `1c5bd7a25`)

The lightningcss full-CSSOM figure is run-dependent across the committed evidence:
`sk-v16-w6-speed-report.md:59` records 833.199 (scrutineer) / 809.977 (build);
`sk-v16-w6tape-report.md:42-47` records 793.326 / 60.96; the RESULTS.md W8R
broadcast diagnostic row carries 929.281; the contract ground-truth canonical
(N=100) is ~974. **No single committed measurement equals 974.** Per §0.5, the gate
is NOT any prior fixed number — it is the same-run re-baselined lightningcss median
on the SK-V17 N>=50 harness (Wave 0). The figures below are prior-run references
only.

| Surface | SK-V16 close | SK-V17 bracket |
|---|---:|---|
| JSON parse_only / direct_to_struct / real_typed_struct | 51 / 51 admitted, strict | guard baseline; tape-activation tripwire |
| CSS L4 grammar-derived provider | landed | preserve; ride the tape |
| CSS 8-field structural equality vs cssparser | EXACT (`rules=10136/style=9561/sel=9561/decls=20043`) | re-prove post-conversion |
| CSS typed Track 1 vs lightningcss (cold, run-dependent prior numbers) | typed retime ~3.09 Mbps (`w6-speed-report.md:164`); fact-stream profile plane ~64-70 Mbps; vs lightningcss ~793/833 scrutineer or ~61 build; ~0.004-0.22x depending on plane | primary >SOTA target; re-baselined same-run at N>=50 |
| Benched flat-tape substrate (`skinny/crates/runtime/src/tape/`) | LANDED, UNWIRED for CSS (JSON uses it; zero CSS parse-path callers) | activate for CSS |
| Lazy-view accessor generator (skinny codegen) | does not exist | build (the gating artefact) |
| `W5C_REQUEST_FACT_PROFILES` hand-coded CSS routing (`lib.rs:336`) | live; CSS rides `RequestFacts` | RETIRE; derive routing from `BackendRule`/grammar shape |
| NEON on CSS | zero SIMD on any `css_l4_*/generated.rs` | union leaf set, gated behind tape |
| Generated CSS / real-typed state | 8 git-dirty files | clean regen |

The 24 `css_l4/*/direct_to_struct/main` rows present in `skinny/RESULTS.md` (lines
112-135, grep-verified `grep -c '^| css_l4/.*/direct_to_struct/main '` = 24) are the
W8R broadcast diagnostics — all `not_admitted:SK-V15-W0-broadcast-diagnostic`
/ `AUDIT-FALSIFIED`, carrying the single broadcast tuple
`track1_mbps=2319.041; cssparser_mbps=2362.037; lightningcss_mbps=929.281`
projected across all 24 rows (one timing tuple → 24 conceptual rows — the broadcast
the §0.4 pre-block forbids). There are **zero ADMITTED TYPED CSS rows**; the only CSS
rows are these 24 falsified broadcast diagnostics, which carry no per-corpus typed
throughput. There is no SK-V16 per-corpus admitted typed-CSS row to delta against.

Cross-artefact reconciliation note: all cohort artefacts state the broadcast row
count as 24 / lines 112-135 as of V3 (alphaA §results, alphaC §4/§7, alphaD §0/§5,
this SYNTHESIS §0.2, HANDOFF Current State); the V2 "6" undercount is resolved across
the cohort. The grep-verified ground truth is 24 table rows
(`grep -c '^| css_l4/.*/direct_to_struct/main ' skinny/RESULTS.md` = 24, lines
112-135; the broader `grep -c 'css_l4/' skinny/RESULTS.md` = 25, whose 25th match
:154 is a prose REDRESS-127 companion reference, not a row, with
`grep 'W6.*css|tape.*direct_to_struct'` EMPTY — there is NO admitted/distinct W6
typed CSS row). The substantive conclusion (zero ADMITTED typed CSS rows; the
broadcast is pre-blocked) is unchanged by the count correction.

### 0.3 Receiver goalset

Every owner path below is the benched skinny tree (incl. `skinny/xtask/src/regen_css.rs`
— the seven `RequestFactsProfile` literals that the seam-flip edits, see the Tape
activation row). The totality paths (`crates/core/src/backend/rust/emitter/`, the
totality `emit_builder` fn + `OpenFrame` template, `css_l4/builder.rs:274`) are the
SK-V18 fold target, NOT SK-V17 owner paths; a receiver editing them would burn LOC on
an un-benched tree
(CH1-R1).

| Receiver | Obligation |
|---|---|
| Lazy-view projection generator | Write the layout-walk accessor generator in `skinny/crates/codegen/` (`grammar_provider.rs` + `lower/{tape_plan.rs,offset_tape.rs,event_tape.rs}`, the skinny lowering seam) that reconstructs each `CssRule/StyleRule/Selector/Declaration/CssTypedValue` from the skinny `(Tape, ValueRef)` cursor over the `BackendRule` shape: child-position → `ValueRef` child, branch tag → meta dispatch, typed leaf → decode by type, rule reference → child + recurse. Emit `document/value/view/visitor` for the CSS grammars from this one generator, isomorphic to JSON's `value_from_ref` (`json/value.rs:143`). State explicitly that NO new cursor/builder type is introduced — the existing skinny `Tape`/`ValueRef` is reused. The architecture-doc `StructLayout`/`begin_compound` design intent maps to the skinny `BackendRule` + `tape_plan.rs` equivalents. |
| Tape activation + builder seam flip | Flip the benched CSS Track 1 from `emit_fact_stream` String emission (`generated.rs:5`) to skinny `TapeBuilder` append (`assembler.rs:42,71`, `push_plain_offset` = one branchless u32 write) via `skinny/crates/codegen/src/lower/` + `runtime_generator.rs:17-25` (route CSS off `RuntimeEmitterKind::RequestFacts`; extend or re-select the emitter). DELETE the hand-coded `W5C_REQUEST_FACT_PROFILES` CSS profile array (`codegen/src/lib.rs:336`, iterated `:567,:611`, selected `:299`) — the Lock-14 phrase-#1 construct — replacing it with grammar-derived routing. The concrete seam-flip site is the seven per-grammar `RequestFactsProfile` literals carrying `emitter: RuntimeEmitterKind::RequestFacts` in `skinny/xtask/src/regen_css.rs:45,63,81,99,117,135,153`, flipped off the fact-stream and regenerated via the `regen_css` fn (`regen_css.rs:164`); the consumers are the two `for profile in …` loops at `codegen/src/lib.rs:567,611`. checkpoint = `offsets.len()` marker O(1); rollback = truncate. No `split_off`, no `Vec<Vec>` arena, no per-leaf eager payload. The CSS parser becomes generic over the skinny tape sink (the conversion report `:55` notes parse fns are monomorphized to a named builder today; the seam must accept the tape sink without re-introducing a second substrate). |
| CSS typed equality re-proof | Re-prove EXACT 8-field structural equality against cssparser after the tape conversion before any speed counts. Equality is the gate; speed is meaningless until equality holds. |
| N>=50 cold telemetry + full-CSSOM lightningcss comparator | Retire `W6_SAMPLE_COUNT=1`. Take N>=50 cold samples + median per corpus per workload. Wire lightningcss as a same-run, same-plane, full-CSSOM-materializing comparator re-baselined this run (NOT a fact-stream). Emit per-corpus median-Mbps + the SK-V17 schema (§Section 2). |
| NEON grammar-general hot-leaf union | RE-PROFILE on the benched tape path first (do not inherit the core-tree `find_component_delim ~56%` figure). Then land `to_bitmask64`, then `byte_class_index_64`, route CSS + JSON through `skinny/crates/bbnf-simd/src/dispatch.rs` `select_classifier`. Scalar reference + checkasm/parity per leaf, same-wave consumer, aarch64-only, non-JSON exercise. udot orphan wiring (C4a) admits unconditionally; the i8mm kernel (C4b) lands ONLY if the re-profile proves the digit leaf is a top-N tailwind self-time leaf (no orphan kernel). |
| Generated-state clean regen | Cleanly regenerate the 8 dirty files; `regen --check` 9/9 exit 0. |

### 0.4 Pre-blocks (carried from alphaC + the CONTEXT REDRESS pre-block)

SK-V17 must NOT reopen any of the following. The CONTEXT pre-block is binding verbatim:

- **AZ-IV eager-value-tree materialization** (the 118x regression: `json_monolithic`
  parsing into a value tree by default, eager per-leaf payload materialization /
  f64-alloc-per-number / per-color `Box<CssColor>`). Materialization stays
  lazy-by-default. The tape appends offsets; typed values are reconstructed on
  demand via `ValueRef`. This is the load-bearing architectural lesson of the
  A-series: the tape was right, the eager value substrate was wrong.
- **StructRegistry / Arena<G> / Builder<G> hot-path indirection** (the 28-65x
  regression on bbnf/sheets, 983x on css bootstrap, 10583x WATCHDOG on tailwind).
  No registry lookup in the per-leaf hot path. The skinny `TapeBuilder` is a single
  non-generic layout-driven sink; it stays that way.
- **CSS fact-stream String serialization** as a live admission output plane
  (`emit_fact_stream`/`emit_full_parse`/`CSS_GENERATED_RS`/`CssFullParseSummary`,
  `generated.rs:5`): diagnostic-only, never an admission surface. The dominant
  benched-CSS-track1 cost is this String building; the tape append replaces it.
- **The hand-coded `W5C_REQUEST_FACT_PROFILES` CSS profile array**
  (`codegen/src/lib.rs:336`): the Lock-14 phrase-#1 construct that hand-curates CSS
  routing. SK-V17 must RETIRE it (derive routing from the grammar/`BackendRule`
  shape), not extend it; relocating its per-rule branching into projection DATA is
  the overfit re-entry seam and is forbidden — every residual CSS routing entry must
  name the `.bbnf` rule it derives from.
- **The 24-row broadcast measurement** (one CSS timing tuple projected into N
  conceptual admits, the source of the 24 falsified `css_l4/*/direct_to_struct/main`
  RESULTS rows, lines 112-135): pre-blocked.
- **Fixture / FNV contrivances**: per-corpus hand-coded `real_typed.rs` fixture
  parse fns, hand-tuned per-corpus capacity constants, FNV production
  selector/arbiter/correctness proof, FNV closed-enum production migration.
  FNV stays bench-only.
- **x86 / AVX paths**: Apple M5 Max / aarch64 only. No x86, no AVX-512, no SVE
  (Apple cores have no SVE; SVE paths would be dead code).
- **brace-counter proof** as CSS admission; **lightningcss CSSOM comparison before
  Track 1 emits comparable CSSOM/value output**; **deleting legacy CSS
  generated/runtime shims before replacement proof lands**; **full-codegen close
  claims while dirty generated CSS files remain**.

Inherited REDRESS pre-block families (semantics carried, not just ids):
`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247,
FNV closed-enum production migration`.

Hidden-coupling escapes are pre-blocked unless routed through Pass Omega + G-Omega
(Lock 1 substrate-union): retained sidecars, retained sidecar tables, sidecar event
vectors, retained cursor/list, cursor streams, aux density/projection tables,
parser-owned structural projections or streams, parallel source passes, second
tapes, public `UnionTape`, new substrate APIs, sixth `BackendShape`, production FNV
arbiters, production hash-correctness proof, Track 1 == Track 2 sidecars, wrong-plane
comparator admission. A SIMD mask stream is a transient producer, not a retained
sidecar; if structural offsets are retained, the structural projection IS the tape
(Lock 1, `LOCKS.md:75`). Cross-call classifier-state retention is REJECT under
Lock 1 v+1. **No second substrate**: if the implementor introduces a skinny
`StructLayout`/`TapeStructBuilder`/`TapeCursor`, those would become a SECOND
substrate alongside the landed `Tape`/`ValueRef` (Lock 1 type-ambivalence) and are
REJECTed; the projection generator emits accessors over the EXISTING `Tape`/`ValueRef`.

**Generality clause (Lock 14, witness-honest):** generality is exercised, not
asserted, and the exercised projection riders are **JSON + CSS only**. JSON is the
existing tape-wired witness (`json/scan.rs`, `value.rs:143` — generated
`value_from_ref` walked from the grammar shape, not from `->` arms); CSS is the
SK-V17 first-mover rich rider. The four-grammar (JSON/CSS/Sheets/BBNF-self) claim is
NOT proven by-construction in SK-V17. `sheets_witness` is NOT a viable third
exercise: it is a 24-line `EventGrammar` byte-classification trait impl
(`runtime/src/grammars/sheets_witness/` = 2 files, 25 LOC) with NO `.bbnf` source, NO
parser, NO `BackendRule` shape, and codegen treats `sheets`/`bbnf` as fail-closed
negative controls (`codegen/src/lib.rs:1075-1090`) — the projection generator walks a
`BackendRule` shape and has nothing to walk for `sheets_witness`. BBNF-self is absent
entirely. Therefore: the SK-V17 exercised projection riders are JSON (real generated
`value_from_ref`) + CSS (the new rich rider); the projection generator's
non-CSS-non-JSON generality is asserted-by-construction, with the Sheets/BBNF-self
proof deferred to SK-V18. Per Lock 14 phrase #2 (`LOCKS.md:386-387`: "with only one
of Sheets or BBNF-self, the claim is scoped to the witnessed grammars"), and since
the SK-V17 witness is JSON+CSS (NOT Sheets), the contract does NOT claim the Lock 14
CSS+Sheets minimum is *met* in SK-V17 — that minimum is the SK-V18 close-out. No
candidate's generality is admitted "by construction" without a named exercised
grammar; the named exercised grammars are JSON and CSS. (Separately, the NEON SIMD
leaf's non-JSON exercise IS `css_l4` — a real rider sharing the
`select_classifier(alphabet)` kernel — which is genuinely dischargeable, §0.1 NEON
gate / Section 2 `simd_non_json_exercise`; that is sound and distinct from the
projection-generator generality bound here.)

### 0.5 Per-corpus close conditions (PASS-ALPHA §4.1)

The >SOTA bar is per-corpus, against lightningcss full-CSSOM, with N>=50 cold
samples reporting the MEDIAN. The benched CSS corpus set is fixed in
`skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54`:
**`{bootstrap, tailwindcss, material-components-web, animate}`**. `normalize` is NOT
in this set; every gate below names only benched corpora. The thresholds are set
against the architecture doc's honest feasibility band (300-600 Mbps after the four
levers; first cross of the lightningcss bar plausible on regular corpora; tailwind
hardest). Each corpus states current state, target, expected intervention, and
fallback. The lightningcss bar is the per-corpus measured median lightningcss
full-CSSOM Mbps re-baselined in Wave 0 — the prior numbers (793/833/929/974,
run-dependent, §0.2) are NOT the gate; the gate is the same-run measured lightningcss
median on the SK-V17 harness.

The per-corpus endpoint character below (animate "regular", tailwind "deeply nested,
many short rules") is the corpus's structural description, NOT a per-corpus lightningcss
Mbps split — the W8R broadcast carried ONE tuple across all corpora, so no per-corpus
lightningcss endpoint is measured. **All per-corpus endpoints are UNMEASURED-PENDING:
no wave exit-gate may key on an inferred per-corpus endpoint until the N>=50 harness
emits the per-corpus split** (alphaB §2/§3 endpoints animate↔164, tailwind↔51,
material↔60 are INFERRED from corpus character, self-flagged, not cited measurements).

| Corpus | Current (benched, fact-stream path) | Target close state | Expected intervention | Fallback if not moved |
|---|---|---|---|---|
| animate (regular) | tape decodes no CSS; benched Track 1 is a fact-stream String | median Track 1 typed > median lightningcss full-CSSOM on same run (>1.0x), N>=50 | tape activation + layout projection (W1/W2) + NEON structural index (W3) + commit-by-construction spine (W4) | if < 1.0x lightningcss after W4: REJECT row, record residual gap + hot leaf in REDRESS; do NOT paper-close |
| bootstrap (regular) | tape decodes no CSS; A-series recognition-only hit 454 Mbps (`3b8b757d`) | median Track 1 typed > median lightningcss full-CSSOM on same run (>1.0x), N>=50 | same four-lever stack | same as animate |
| tailwindcss (hardest: deeply nested, many short rules) | A-series recognition-only hit 496 Mbps; eager path WATCHDOG'd 10583x under AZ-IV | benched cold N>=50; ADMIT if > lightningcss; else REPORT honest residual gap + hot leaf | adversarial delimiter-table tuning + digit udot wiring (C4a) + i8mm kernel (C4b, GATED behind re-profile) | tailwind is explicitly allowed to land short on first pass; record gap honestly in REDRESS; NOT a tranche-blocking failure provided ≥1 regular corpus crosses |
| material-components-web (full corpus, 979638 B) | benched Track 1 is a fact-stream String; prior broadcast tuple ~2319 (NOT typed, falsified) | per-corpus median reported; cross-bar admit OR honest residual | same four-lever stack | report median delta; the full-corpus row is the integration check, not a single-corpus gate |

Tranche-level success criterion: **at least one regular corpus (animate OR
bootstrap) crosses the lightningcss full-CSSOM bar at N>=50 median**, with
preserve-rich-ast intact and EXACT cssparser typed equality re-proven, while JSON
51/51 holds. tailwindcss crossing is a stretch; its honest residual gap is
acceptable and recorded. If NO regular corpus crosses after the four-lever stack,
the tranche records the honest residual and escalates per PASS-ALPHA §8 (`WARN`).

### 0.6 Strict comparator gate (PASS-ALPHA §4.2, CSS-domain framing)

For CSS, the strict-vs-strict comparator gate maps to the materializing-comparator
gate. lightningcss full-CSSOM is the fair bar (it materializes a typed CSSOM, the
same plane Track 1 produces), re-baselined same-run at N>=50. cssparser token-scan
(~2476 Mbps, `w6-speed-report.md:58`) materializes nothing and is a flaw probe ONLY
— beating cssparser is not a >SOTA claim. Every CSS row discloses, per comparator:
Mbps median, % delta, materialization plane (full-CSSOM / typed-direct / token-scan
/ none), and hot leaf. The W6 fact-stream comparator
(`assert_lightningcss_strict_equality` against a fact stream) is retired; the
comparator must build CSSOM on the same run.

| Comparator | Plane | Role |
|---|---|---|
| lightningcss full-CSSOM | full CSSOM build | THE fair >SOTA bar (re-baselined same-run) |
| cssparser token-scan | none (tokens only) | flaw probe; beating it is NOT a SOTA claim |
| Track 1 typed (bbnf, post-tape) | typed direct via lazy `ValueRef` projection | the subject |
| Track 2 / oracle | independent reference | equality anchor, structurally distinct from Track 1 (Lock 1, CH5) |

## Section 1 - Validated And Invalidated Ledger

**Validated (SK-V16 close, carry forward):** JSON 51/51 strict same-plane riding the
skinny tape; CSS grammar-derived provider; CSS 8-field structural equality with
cssparser (EXACT counts, banked `1c5bd7a25`); cross-grammar PEG codegen branch-order
fix; O(1) generic checkpoint (20x sound, generic, `8153236e8`); the benched
flat-tape substrate as correct, clean, additive scaffolding (`skinny/crates/runtime/
src/tape/`: single non-generic `TapeBuilder`, no `Arena<G>` indirection, green
correctness tests); Lock gates; the five BackendShape lowerers; FNV quarantine;
Pattern H provenance.

**Invalidated / still open (SK-V17 candidates):** CSS >SOTA (open — ~14x gap on the
build plane); tape activation for CSS (substrate is JSON-wired but UNWIRED for CSS;
zero CSS parse-path callers); the lazy-view accessor generator (does not exist — the
gating artefact); layout-driven projection (CSS still rides `W5C_REQUEST_FACT_PROFILES`
hand-coded routing + fact-stream String); NEON on CSS (zero SIMD on the CSS path);
generated-state cleanliness (8 dirty files); N>=50 cold telemetry + full-CSSOM
lightningcss comparator (W6 used single-sample + fact-stream comparator).

The A-series proof-of-concept (`3b8b757d`, `crates/bbnf-tape`) reached CSS
recognition-only bootstrap 454 / normalize 735 / tailwind 496 Mbps — proving the
tape substrate is not the bottleneck. SK-V17 reconstitutes that *shape* generally
(typed lazy projection over the skinny `Tape`/`ValueRef`, not recognition-only),
explicitly NOT recovering the AZ-IV overfit (StructRegistry / eager value tree).

## Section 2 - Telemetry Binding (PASS-ALPHA §4.3)

SK-V17 inherits the SK-V15/16 JSON telemetry schema and adds the CSS >SOTA close
columns. The harness MUST emit N>=50 cold samples and report the MEDIAN per row.
The `W6_SAMPLE_COUNT=1` single-sample harness is retired; the gate rejects any CSS
row whose `sample_count < 50` or whose `sample_statistic != median`.

| Column | Type | Required |
|---|---|---|
| `css_corpus` | string (bootstrap / tailwindcss / material-components-web / animate — the benched set, `css_l4_corpus.rs:22-54`) | yes for CSS |
| `css_sample_count` | integer (>=50) | yes for CSS |
| `css_sample_statistic` | enum (median) | yes for CSS |
| `css_sample_mode` | enum (cold / warm) — must be cold | yes for CSS |
| `css_track1_typed_median_mbps` | number | yes for CSS |
| `css_lightningcss_full_cssom_median_mbps` | number (same-run re-baseline; the materializing bar) | yes for CSS |
| `css_cssparser_tokenscan_median_mbps` | number | optional (flaw probe only) |
| `css_comparator_plane` | enum (full-cssom / typed-direct / token-scan / none) | yes for CSS |
| `delta_vs_lightningcss` | number (% or ratio, per-corpus, against same-run median) | yes for CSS |
| `css_track1_typed_passes` | integer | yes for CSS |
| `css_cssparser_typed_passes` | integer | yes for CSS |
| `css_typed_summary_equal` | boolean (EXACT 8-field, gate before speed) | yes for CSS admission |
| `css_rich_ast_preserved` | boolean (CSSOM via lazy `ValueRef` projection, not flattened, not eager) | yes for CSS |
| `css_provider_source` | string (grammar source path) | yes for CSS |
| `tape_activated` | boolean (benched `track1::parser::parse` emits into skinny `Tape`, read via `ValueRef`; proven by `PayloadArena` write/alloc counters; NOT satisfiable by a grep in `crates/core/`) | yes for CSS |
| `lazy_view_generated` | boolean (skinny accessor generator emits document/value/view/visitor over `BackendRule`) | yes for CSS |
| `projection_generality_exercise` | string (named projection rider exercised: `json` or `css_l4` — the two riders with a real `BackendRule` shape; `sheets_witness` is NOT a valid value here, it has no `BackendRule` to project from; non-CSS-non-JSON projection is SK-V18) | yes for projection-generality claims |
| `w5c_profile_array_retired` | boolean (`W5C_REQUEST_FACT_PROFILES` deleted; CSS routing grammar-derived) | yes for CSS |
| `dirty_generated_state` | enum (clean / retired / routed-intrinsic-block) | yes for generated checks |
| `native_simd_status` | enum (scalar / parity-pass / checkasm-pass / not-applicable) | yes for SIMD claims |
| `simd_non_json_exercise` | string (named grammar that exercises the SIMD leaf via the shared `select_classifier(alphabet)` kernel: `css_l4` — a real rider sharing the kernel; this is the dischargeable non-JSON SIMD exercise, distinct from the projection-generality column above) | yes for SIMD claims |

The retained JSON schema (PASS-ALPHA §4.3: Track 1 Mbps, Track 2 Mbps, sonic-rs
strict, simdjson DOM, yyjson, serde_json, Δ columns, Hot leaf, Signal) stays in
force for the JSON guard rows.

S-P3 must bind executable gate consumers for:
`(cd skinny && cargo xtask gate-json --check-results --skv17-css-sota-report <path>)`
(consumes per-corpus median, N>=50, full-CSSOM comparator, equality boolean,
preserve-rich-ast boolean, tape-activated boolean, `w5c_profile_array_retired`
boolean, per-corpus delta-vs-lightningcss, hot leaf, admitted-row count); and re-uses
the SK-V16 dirty-generated + native-simd report consumers. The gate rejects any CSS
row missing `css_sample_count >= 50`, `css_sample_statistic == median`,
`css_sample_mode == cold`, `css_comparator_plane == full-cssom` for the lightningcss
bar, `css_typed_summary_equal == true` before any speed admission, or
`css_rich_ast_preserved == true`. The gate also rejects any CSS row whose
`css_corpus` is not in the benched set (no phantom `normalize`), and any
single-tuple broadcast (`sample_count == 1` or one tuple across multiple corpus
rows — the W8R regression tripwire).

## Section 3 - Trajectory

SK-V17 is the tape-activation + layout-driven-projection + NEON-hot-leaf tranche.
The architecture doc's four-lever stack is the route, translated to the benched
skinny tree: (1) kill fact-stream String serialization (`emit_fact_stream`,
`generated.rs:5`) via skinny `TapeBuilder` append + retire `W5C_REQUEST_FACT_PROFILES`;
(2) alloc removal via O(1) tape checkpoint (`offsets.len()` marker + truncate), no
`split_off`, no `Vec<Vec>`, no eager per-leaf payload; (3) NEON structural pre-scan
(`byte_class_index_64` + movemask cascade via `skinny/crates/bbnf-simd/src/dispatch.rs`),
RE-PROFILED on the benched path first — the architecture profile's ~56% / ~10% scalar
delimiter/balance figures are inherited from the core-tree profile and must be
re-confirmed on the benched skinny path before any kernel lands; (4)
commit-by-construction spine removing speculative rollback on the structural backbone.
The honest expected ceiling is the 300-600 Mbps band, crossing the lightningcss bar
plausibly on regular corpora (animate/bootstrap), with tailwindcss the adversarial
hold-out.

If SK-V17 closes with at least one regular corpus (animate OR bootstrap) crossing the
lightningcss full-CSSOM bar at N>=50 median, preserve-rich-ast intact, EXACT cssparser
equality re-proven, and JSON 51/51 held, then the unified tape/layout/projection model
is proven generalizable (JSON+CSS witnessed) and SK-V18 becomes the
Sheets/BBNF-self tape-conversion + TOTALITY-fold tranche. If no regular corpus crosses
after the four-lever stack, SK-V17 records the honest residual and Pass Alpha V3
revises the candidate shortlist (per PASS-ALPHA §5/§8).

The detailed wave-by-wave falsifiability gates (PASS-ALPHA §4.4: owner paths, entry
gate, exit gate, hard cap, revert protocol, same-wave consumer, pre-blocked routes)
are authored downstream by skinny pass S-P3 in `sk-v17/SPEC.md`, consuming this
goalset. Alpha supplies only the measurable goalset (§0.1/§0.5), the strict
comparator gate (§0.6), the telemetry binding (Section 2), and the pre-blocked
routes (§0.4). Revert protocol, hard caps, and per-wave triumvirate discipline are
sanctioned-deferred to S-P3 per PASS-ALPHA §4.4.
