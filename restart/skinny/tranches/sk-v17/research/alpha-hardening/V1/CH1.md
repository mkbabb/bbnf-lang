# CH1 CORRECTNESS — Pass Alpha SK-V17 (cycle V1)

Lens: CH1 Correctness (PASS-ALPHA §3 + ORCHESTRATOR §3W). Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md`
+ `SYNTHESIS.md` + `HANDOFF.md`.

CH1 focus: (a) every claim cites a RESULTS row / REDRESS entry / commit SHA / measurement
file; (b) falsifiability gates are measurable; (c) competitor deltas computed on the correct
(materializing) plane vs lightningcss. CH1/CH6 REJECT any uncited claim.

Host discipline verified: HEAD `1c5bd7a25` (`git log --oneline -1` confirmed). Reviewer
verified path/line claims directly against the worktree where load-bearing.

---

## Verification performed (reviewer evidence)

1. `git log --oneline -1` → `1c5bd7a25 feat(sk-v16-W6-tape): add shared flat-tape runtime substrate`. ✓ All artefact HEAD anchors correct.
2. `grep -rl "StructLayout" skinny/crates/` → **0 files**; `grep -rl "OpenFrame" skinny/crates/` → **0 files**. The benched skinny tree has NO `StructLayout`, NO `OpenFrame`, NO `CssArena`. ✓ Confirms alphaE §0's translation correction; **falsifies** the SYNTHESIS/HANDOFF benched-surface citations (see CH1-R1).
3. `ls skinny/crates/runtime/src/tape/` → `assembler.rs event_grammar.rs mod.rs offsets.rs`; `ls crates/core/src/runtime/tape/` → `arena.rs cursor.rs mod.rs record.rs`. **Two distinct tape trees.** The benched substrate is `skinny/crates/runtime/src/tape/` (`TapeBuilder`, `push_plain_offset` at `assembler.rs:42,71`); `crates/core/src/runtime/tape/` is the TOTALITY tree.
4. `grep RuntimeEmitterKind skinny/crates/` → enum at `grammar_provider.rs:40` with `{CompiledLowering, RequestFacts}`; CSS rides `RequestFacts` (`lib.rs:291`), JSON rides `CompiledLowering` (`lib.rs:282`). ✓ Confirms alphaE C0/C1.
5. `track1_facts(input) -> Result<String,String>` at `nonjson_css_l4.rs:596`. ✓ Confirms the benched CSS Track 1 is a String, not typed (alphaE C0).
6. `nonjson_css_l4.rs` `sample_count: track1_measure.iterations` (line ~1134). ✓ Confirms single-sample harness inadequacy (alphaA/B/D/E).
7. RESULTS.md: `grep -c GO|NO-GO` → 51 admitted JSON rows; twitter/parse_only Track 1 `8349.290` > sonic `4913.095` (+69.9%), citm/real_typed `20512.601` > sonic `12662.292` (+62.0%) — ✓ alphaB §5 JSON guard rows EXACT.
8. RESULTS.md **DOES contain CSS rows**: 6 `css_l4/*/direct_to_struct/main` rows, all `not_admitted:SK-V15-W0-broadcast-diagnostic` / `AUDIT-FALSIFIED`, carrying the broadcast tuple `track1=2319.041;cssparser=2362.037;lightningcss=929.281`. This **falsifies** alphaA's "zero CSS rows" claim (see CH1-R2) and **confirms** alphaC §4's broadcast figures EXACT.
9. lightningcss figure dispersion verified: `sk-v16-w6-speed-report.md:59` = 833.199/809.977; `sk-v16-w6tape-report.md:42-47` = 793.326 (scrutineer)/60.96 (build); RESULTS W8R = 929.281; contract canonical = ~974. All run-dependent; no single committed measurement equals 974.

---

## §1 — alphaA (results extraction)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 standing | ACCEPT | 0/24 admitted, banked wins, ~70 Mbps ~14x slower — all cited and verified. |
| §1 canonical bench table | ACCEPT | Honestly discloses that ~974/~2539 are the contract-supplied N=100 canonical, distinct from the committed single-sample 793/2529 (`w6tape-report.md:42-47`). The N≥50 binding is correctly stated. The 793-vs-974 gap is flagged, not papered. |
| §2 per-corpus structure | **REVISE** | **CH1-R2.** Line 96-98: "SK-V16 produced NO per-corpus typed CSS rows in `skinny/RESULTS.md` (the file holds JSON rows only — verified, 51 JSON rows, zero CSS rows)." This is **false as written**: RESULTS.md contains 6 `css_l4/*/direct_to_struct/main` rows (the W8R broadcast diagnostics, `not_admitted`/`AUDIT-FALSIFIED`, tuple `2319.041/2362.037/929.281`). Fix: replace "zero CSS rows" with "zero *admitted typed* CSS rows; the only CSS rows present are the 6 falsified W8R broadcast diagnostics (`not_admitted:SK-V15-W0-broadcast-diagnostic`), which carry no per-corpus typed throughput." The downstream conclusion (no SK-V16 per-corpus typed-CSS row to delta against) survives the correction. |
| §3 8-field equality | ACCEPT | 10136/9561/9561/20043, errors=0, cited to three reports + test path. Verified consistent. |
| §4 20x checkpoint | ACCEPT | 14.2x/15.6x/20x all cited to `:83-89`; SHA `8153236e8`; soundness argument cited. The "20x = full-corpus 63.9 vs design 3.1 fragment baseline" distinction is correctly disclosed (not conflated with the 14-16x single-sheet). |
| §5 sub-wave ledger | ACCEPT | Each row carries commit SHA + report line. The "all single-sample cold" caveat is honest. |
| §6 banked wins | ACCEPT | Provenance per row. |
| §7 goalset seed | **REVISE** | **CH1-R3.** Line 254-255 cites `sk-v16-css-sota-tape-architecture.md:347-355` for the 300-600 band — citation valid, BUT the §7 lever sequence inherits the architecture-doc's core-tree framing without alphaE's correction. The "wire the flat-tape lazy-view generator" lever does not name WHICH tape tree (`crates/core/` vs `skinny/crates/runtime/`). Fix: add the alphaE §0 translation note (benched surface = `skinny/crates/runtime/src/tape/` + `skinny/crates/codegen/src/lower/`, NOT `crates/core/`). Otherwise αF inherits the wrong-tree citation (CH1-R1). |
| §8 citation ledger | ACCEPT | Comprehensive; each claim mapped to a source line. |

alphaA: 6 ACCEPT, 2 REVISE, 0 REJECT.

---

## §2 — alphaB (competitor deltas)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 plane taxonomy | ACCEPT | The materializing-vs-token-scan distinction is the correct CSS analogue of the sonic-rs `utf8_lossy` finding. lightningcss = fair bar; cssparser = flaw probe. Cited to `w6-speed-report.md:55-60`. This is exactly the CH1 "correct plane" requirement, satisfied. |
| §1 canonical baseline | ACCEPT | HEAD verified; corpus bytes cited to manifest; the measurement-honesty caveat (3.09/70/13-15 dispersion = harness inadequacy, not architecture contradiction) is correct and cited to `nonjson_css_l4.rs:1134`. |
| §2 per-corpus delta vs lightningcss | **REVISE** | **CH1-R4.** The per-corpus endpoint mapping (animate↔164, tailwind↔51) is INFERENTIAL — alphaB itself flags this at lines 116-119 + 242-245, which is commendable honesty. But the table at lines 96-102 presents inferred per-corpus numbers (animate 16.8%, material 6.2%, etc.) in the SAME visual format as cited numbers, with only a prose disclaimer. CH1 requires inferred values be visually marked. Fix: tag each inferred cell with a superscript/marker (e.g. `~164†`) and a table footnote `† inferred from 51-164 range + corpus character; not a published per-corpus number; SK-V17 N≥50 must confirm`. The material-components-web `~60 (est. mid-low)` is doubly inferred (not even an endpoint) — mark it explicitly. |
| §3 per-corpus delta vs cssparser | **REVISE** | Same CH1-R4 inferential-marking defect carries here (the same inferred Track 1 endpoints feed the cssparser ratios). Apply the same footnote. Otherwise the §3 reasoning (cssparser plane-mismatched, ~36x, not a SOTA bar) is correct and cited (`w6-speed-report.md:102`). |
| §4 inter-comparator relation | ACCEPT | lightningcss ~38% of cssparser (~2.6x) = the materialization tax — correct arithmetic from the cited aggregates, correctly framed as why lightningcss is the fair bar. |
| §5 JSON guard | ACCEPT | twitter/parse_only +69.9% and citm/real_typed +62.0% verified EXACT against RESULTS.md rows 5 + 10. |
| §6 findings feed | ACCEPT | Each finding restates a cited §1-§5 result; the N≥50 + per-corpus-lightningcss-split mandate is correct. |
| Verification ledger | ACCEPT | The INFERENTIAL flag at lines 242-245 is the model CH1 wants; it is what makes §2/§3 REVISE (mark in-table) rather than REJECT (uncited). |

alphaB: 5 ACCEPT, 2 REVISE, 0 REJECT.

---

## §3 — alphaC (REDRESS digest)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 framing + two-bucket classification | ACCEPT | The PERMANENT-PRE-BLOCK vs ADMIT-UNDER-DIFFERENT-FRAMING taxonomy is the correct CH3 instrument; each carries an explicit re-open test. |
| §1 AZ-IV eager value tree | ACCEPT | 118x (canada 1.83ms→215.7ms) cited to `cb14970f` + `sk-v16-arch:46-66`; timeline correction cited to `:21-26`; re-open test measurable (per-leaf typed/f64 alloc on hot path). |
| §2 StructRegistry/Arena/Builder | ACCEPT | 28-65x/983x/10583x WATCHDOG cited to `sk-v16-arch:46-66`. The SPLIT (indirection = permanent; StructLayout = admit-under-framing) is correct. **Caveat for αF (not a defect here):** §2 cites `bbnf_ir::registry::struct.rs` + `css_l4/builder.rs:274` — these are TOTALITY-tree paths. alphaC is digesting the architecture doc faithfully, so this is in-scope for a redress digest; the wrong-tree problem is αF's (CH1-R1), not alphaC's. |
| §3 fact-stream String | ACCEPT | `emit_fact_stream` at `css_l4_declaration_values/generated.rs:5,61`; ~34% self-time; `track1` is `Result<String,String>` — verified at `nonjson_css_l4.rs:596`. The 2331-summary-margin-does-not-transfer correction cited. |
| §4 24-row broadcast | ACCEPT | Reviewer independently confirmed the broadcast tuple in RESULTS.md (`track1=2319.041;cssparser=2362.037;lightningcss=929.281` across 6 `css_l4/*` rows, `not_admitted:SK-V15-W0-broadcast-diagnostic`). EXACT match to alphaC's `2319.041/929.281/2362.037`. Cited to `css_l4_w8.rs:206-228`, `W8_SELECTED_CSS_ROWS=24`, Lock 8. PERMANENT PRE-BLOCK correct. |
| §5 FNV/fixture | ACCEPT | Both contrivances cited (V1-AUDIT, V2 AGENT-5, `generated_real_typed.rs` 4941 lines/187 fns). Classification correct. |
| §6 x86/AVX | ACCEPT | aarch64-only; no-SVE-on-Apple cited `:265-266`; x86 sites cited `REDRESS.md:465-468`. Diagnostic-only correct. |
| §7 consolidated ledger | ACCEPT | Each row maps to its §; measured refutation column populated. |
| §8 single distinction | ACCEPT | The admit/pre-block line is the correct CH3 anchor. |

alphaC: 9 ACCEPT, 0 REVISE, 0 REJECT. Strongest artefact; every measured refutation cited and one independently re-verified.

---

## §4 — alphaD (validated/invalidated ledger)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §1 validated (V1-V6) | ACCEPT | Each carries a commit SHA (`ea8138056`, `4de419f5e`, `2a85bf240`, `8153236e8`, `1c5bd7a25`) + measured evidence. V6 honestly flags UNWIRED with the grep evidence. |
| §2 invalidated (I1-I7) | ACCEPT | Each refuted claim cites measured evidence (I1 3.093→3.178; I3 2331-summary-does-not-transfer; I5 118x; I7 ~3.1-vs-70 variance). I6 timeline correction cited. |
| §3 still-open (O1-O5) | **REVISE** | **CH1-R5.** O1-O5 inherit the architecture-doc core-tree paths verbatim (`css_l4/builder.rs`, `CssArena`, `StructLayout`, `regen_css.rs`, `bbnf_ir::registry`) WITHOUT alphaE's correction that these do not exist in the benched skinny tree. O1 line 79 says "thread the CSS-specific routing the eager builder encodes" — referencing `css_l4/builder.rs` (core tree). For a candidate-feeding ledger this is a citation-tree error that propagates into αF. Fix: add a one-line note per O-row (or a §3 preamble) that the benched implementation surface is `skinny/crates/{runtime/src/tape,codegen/src/lower,bbnf-bench}`, per alphaE §0; the core-tree paths are the architecture-doc's totality framing. |
| §4 demoted | ACCEPT | Pattern H / FNV / Decision Engine each cited to `sk-v16/SYNTHESIS.md`. |
| §5 ledger text | ACCEPT | The ordered spine O5→O1+O2→O3→O4 and the I1/I2 "micro-opt does not move the floor" lesson are cited and correct. |

alphaD: 4 ACCEPT, 1 REVISE, 0 REJECT.

---

## §5 — alphaE (candidate shortlist)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 ground-truth anchors | ACCEPT | Every anchor cited. **This is the artefact that gets the benched-surface tree RIGHT** (lines 37-51): explicitly states no `StructLayout`/`OpenFrame`/`CssArena` in skinny, names the real surfaces (`BackendRule`, `lower/tape_plan.rs`, `RuntimeEmitterKind`), and pre-declares "CH1 will reject any goalset citing core-tree paths as the benched surface." Reviewer independently verified all of this (grep = 0; `RuntimeEmitterKind` enum confirmed). This anchor is load-bearing for CH1-R1. |
| §0 lightningcss anchor (~833) | ACCEPT | Cites `w6-speed-report.md:58,164` = 833.199. Honest; distinct from the contract's ~974 canonical (which alphaE does not adopt because no committed measurement equals 974). Consistent with alphaA's disclosure. |
| C0 de-fact-stream | ACCEPT | File paths are SKINNY paths, verified (`grammar_provider.rs`, `runtime_generator.rs:17-25`, `nonjson_css_l4.rs:596-624`). Falsifiability gate measurable (8-field EXACT + typed-not-String + N≥50). Scalar-ref N/A correctly justified (codegen, not primitive). |
| C1 tape wiring | ACCEPT | Skinny paths (`runtime/src/tape/{mod,assembler}.rs`, `codegen/src/lower/{offset_tape,tape_plan}.rs`) verified to exist. Gate ≥30 Mbps with <20 fallback = measurable. Entry-gate (borrowed-slice-vs-lazy decision before dispatch) correctly cited to `w6tape-conversion-report.md:67`. |
| C2 NEON pre-scan | ACCEPT | `select_classifier`/`PrimitiveKernels` (dispatch.rs:42,50,58), checkasm tests named, scalar-ref present (`scalar::classify_chunk`, `scan_structurals_scalar`). lo6-admissibility honesty (fall back to scalar if alphabet collides) is the correct CH2/CH1 posture. Gate ≥80 measurable. |
| C3 commit-by-construction | ACCEPT | Skinny paths; gate ≥300 with >833 plausible, <200 NO-GO, 150-200 PARTIAL — measurable. The "non-deposition must be PROVEN at codegen, not heuristic" gate is the correct CH5-aware framing. |
| C4 tailwind/udot/i8mm | ACCEPT | `digit_mac.rs:38-40,62-63` orphan + scalar twin cited; checkasm REQUIRED for new i8mm kernel; honest-residual close (no fabricated cross) satisfies CH6. i8mm-detection-clean (zero in skinny) verified by alphaE grep. |
| §1-§4 order/discipline | ACCEPT | Dependency DAG measurable; the escalation note (C0 unmeasurable → §8 BLOCKED) is correct PASS-ALPHA §8 application. |

alphaE: 9 ACCEPT, 0 REVISE, 0 REJECT. The candidate shortlist is correctly bracketed and, uniquely, gets the benched surface right.

---

## §6 — SYNTHESIS.md (αF contract)

| Section | Disposition | Rationale + fix |
|---|---|---|
| header + Authority | ACCEPT | HEAD + authority files cited. |
| §0.1 close condition | **REJECT** | **CH1-R1 (the central CH1 defect).** The benched-surface citations are WRONG-TREE and directly contradict alphaE §0 + reviewer grep (0 occurrences in skinny). Specifically: row "Tape activation" cites `crates/core/src/runtime/tape/` + `TapeStructBuilder`/`TapeCursor` + `tests/tape_substrate.rs`; row "Layout-driven projection" cites `bbnf_ir::registry::struct.rs LayoutKind+FieldSource`, `css_l4/builder.rs:274 ~40-arm match`, `StructLayout`; row "preserve-rich-ast" cites core-tree value types. **None of these exist in the benched `skinny/crates/` tree** (`grep -rl StructLayout skinny/crates/` = 0; `OpenFrame` = 0; the benched tape is `skinny/crates/runtime/src/tape/` with `TapeBuilder`, not `TapeStructBuilder`). The close condition therefore gates on artefacts in the WRONG tree — a row could be "met" in `crates/core/` while the benched CSS path (`RequestFacts` → `track1_facts` String) is untouched. alphaE explicitly warned (`alphaE:50`): "CH1 will reject any goalset citing core-tree paths as the benched surface." **Fix:** rewrite every close-condition surface citation to the skinny benched tree per alphaE §0 — Tape activation: `skinny/crates/runtime/src/tape/{assembler,mod}.rs` (`TapeBuilder`/`ValueRef`/`push_plain_offset`) live in the CSS parse path, grep over `skinny/crates/runtime/src/grammars/css_l4_*/` non-empty; Layout-driven projection: `RuntimeEmitterKind` extended/CSS routed to `CompiledLowering`, `skinny/crates/codegen/src/lower/{offset_tape,tape_plan}.rs` emit CSS tape ops, the benched-fn String (`nonjson_css_l4.rs:596`) replaced by a typed-summary consumer. Keep the architecture-doc core-tree paths only as cross-reference, never as the gate surface. |
| §0.1 equality/rich-ast/>SOTA/tailwind/telemetry gates | ACCEPT (content) | The equality (8-field EXACT), preserve-rich-ast, per-corpus >SOTA, honest-tailwind, and N≥50-median gates are individually measurable and correctly framed. They are NOT rejected — only the §0.1 *surface paths* (CH1-R1) are. |
| §0.2 starting state | **REVISE** | Inherits CH1-R1: rows cite `runtime/tape/` and core-tree builders. Also the lightningcss column (line 74) reads "~793 / ~61" while §0.5 line 154 reads "~793 / ~61" but §0.1/§0.6 invoke ~974/~2529 — pick ONE disclosed baseline. Fix: state the W6 committed range (793 scrutineer / 61 build / 833 build-alt / 929 W8R) AND the contract canonical ~974, and bind the gate to "the same-run measured lightningcss median on the SK-V17 N≥50 harness" (which §0.5 line 146 already does correctly — propagate that resolution up to §0.2). |
| §0.3 receiver goalset | **REJECT** | **CH1-R1 again, load-bearing.** "Write the layout-walk accessor generator in `crates/core/src/backend/rust/emitter/`" and "Rewrite `regen_css.rs emit_builder` to select `TapeStructBuilder` (DELETE the OpenFrame template + match rule_id)" — these direct the implementer to edit the CORE tree and delete an `OpenFrame` template that **does not exist in skinny** (grep = 0). This would either be a no-op on the benched path or an edit to the un-benched totality tree. Fix: retarget to `skinny/crates/codegen/src/{grammar_provider.rs,runtime_generator.rs,lower/}` + the skinny `TapeBuilder` seam, per alphaE C0/C1. |
| §0.4 pre-blocks | ACCEPT | Faithful to alphaC; all six families + hidden-coupling escapes carried with measured refutations. |
| §0.5 per-corpus close | **REVISE** | **CH1-R6.** Corpus naming is INCONSISTENT with the actual corpus. §0.5 names "normalize" as a regular corpus, but the SK-V14 corpus (`css_l4_corpus.rs:22-54`, confirmed by alphaA §2 + alphaB §1) is {animate, bootstrap, tailwindcss, material-components-web} — there is **no `normalize` corpus**. "normalize" appears to be imported from the A-series archaeology (`sk-v16-arch` normalize 735 Mbps) which used a DIFFERENT corpus set. Fix: replace "normalize" with "animate" (the actual regular/easiest corpus per alphaB §1) throughout §0.5 + §0.1 + Section 3 + HANDOFF, OR explicitly add normalize.css to the corpus and re-baseline. As written, the tranche-success criterion ("normalize OR bootstrap crosses") names a corpus that is not benched — unmeasurable as stated. Otherwise the per-corpus table structure (current/target/intervention/fallback) is correct PASS-ALPHA §4.1 form. |
| §0.6 strict comparator gate | ACCEPT | Materializing-comparator mapping correct; lightningcss=fair, cssparser=flaw-probe; per-row plane disclosure required. The `assert_lightningcss_strict_equality`-against-fact-stream retirement is correctly mandated. |
| Section 1 ledger | ACCEPT | Consistent with alphaD; A-series 454/735/496 cited to `3b8b757d`. (Note: 735 is "normalize" from the A-series corpus — same CH1-R6 corpus-name provenance; acceptable here as historical recognition-only marks, but do not let "normalize" leak into the SK-V14 per-corpus gate.) |
| Section 2 telemetry | ACCEPT | The CSS schema columns (sample_count≥50, median, cold, full-cssom plane, equality-before-speed, rich-ast, tape_activated, simd_non_json_exercise) are measurable and gate-bindable. The xtask `--skv17-css-sota-report` consumer is specified. |
| Section 3 trajectory | **REVISE** | Inherits CH1-R6 ("normalize/bootstrap") corpus-name defect. Fix with §0.5. Otherwise the four-lever route + escalation are correct. |

SYNTHESIS: 5 ACCEPT, 4 REVISE, 2 REJECT.

---

## §7 — HANDOFF.md (αF packet)

| Section | Disposition | Rationale + fix |
|---|---|---|
| Current State | **REVISE** | Line 13 cites `crates/core/src/runtime/tape/` + `TapeStructBuilder` as the landed substrate, and line 18 cites the "eager `OpenFrame` tree" — same CH1-R1 wrong-tree provenance. As a *narrative of what SK-V16 landed in totality* this is defensible, but it must DISCLOSE that the benched skinny substrate is `skinny/crates/runtime/src/tape/` (`TapeBuilder`), not `crates/core/`. Fix: add the alphaE §0 tree-distinction sentence. |
| What SK-V17 Opens | ACCEPT (with CH1-R1 carry) | The four-lever route is correct; the "lazy-view accessor generator does not exist" gating-artefact framing is correct. But it must name the SKINNY emitter surface, not the core-tree generator (folds into CH1-R1). |
| Authority / Gate Posture | ACCEPT | Files cited; G-Omega-only gate posture stated per the active pin. |
| Pre-Blocked Routes | ACCEPT | Faithful to alphaC + SYNTHESIS §0.4. |
| Next Move | **REVISE** | Step 3 names waves "(W1) lazy-view accessor generator + codegen unification → (W2) tape activation + builder seam flip" without naming the skinny surface; step 2 profile leaf `find_component_delim ~56%` is cited to the architecture profile but that profile is over the core-tree/eager path — confirm it reproduces on the benched skinny path (S-P1 must re-profile, not inherit; this is the `actual-profiling` discipline). Fix: tag the hot-leaf percentages as "architecture-doc profile; S-P1 must re-confirm on the benched skinny CSS path before nominating." Also propagate the CH1-R6 corpus-name fix (close criterion line 137-138 says "normalize OR bootstrap"). |
| Close criterion | **REVISE** | Same CH1-R6 "normalize" defect (line 137). Fix with §0.5. |

HANDOFF: 3 ACCEPT, 3 REVISE, 0 REJECT.

---

## Consolidated CH1 disposition

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| alphaA | 6 | 2 | 0 |
| alphaB | 5 | 2 | 0 |
| alphaC | 9 | 0 | 0 |
| alphaD | 4 | 1 | 0 |
| alphaE | 9 | 0 | 0 |
| SYNTHESIS | 5 | 4 | 2 |
| HANDOFF | 3 | 3 | 0 |
| **TOTAL** | **41** | **12** | **2** |

ACCEPT rate = 41/55 = **74.5%**. Below the §3Z ≥95% convergence bar. Pass Alpha V2 required.

## The load-bearing CH1 findings (orphan-REVISE blockers)

- **CH1-R1 (REJECT × 2, SYNTHESIS §0.1 + §0.3):** the αF contract gates and directs the implementer against the TOTALITY tree (`crates/core/src/runtime/tape/`, `StructLayout`, `bbnf_ir::registry`, `css_l4/builder.rs:274`, `OpenFrame`) which **does not exist in the benched `skinny/crates/` tree** (grep-verified 0). alphaE §0 already supplies the correct skinny surfaces and pre-warned this exact rejection. **αF must adopt alphaE §0's translation wholesale.** This is the single most important CH1 defect: as written, the goalset is unmeasurable on the benched plane.
- **CH1-R6 (REVISE, SYNTHESIS §0.5/§0.1/Sec3 + HANDOFF):** the close-condition names a `normalize` corpus that is not in the SK-V14 benched corpus {animate, bootstrap, tailwindcss, material-components-web}. The tranche-success criterion ("normalize OR bootstrap crosses") is therefore unmeasurable. Replace with `animate` or add the corpus.
- **CH1-R2 (REVISE, alphaA §2):** "zero CSS rows in RESULTS.md" is false; 6 falsified W8R broadcast diagnostic CSS rows exist. Reframe to "zero admitted typed CSS rows."
- **CH1-R3/R5 (REVISE, alphaA §7 / alphaD §3):** propagate alphaE §0 benched-surface correction so the candidate-feeding ledgers do not hand αF the wrong tree.
- **CH1-R4 (REVISE, alphaB §2/§3):** mark the inferred per-corpus Track 1 endpoints in-table (footnote), not only in prose.

Competitor-plane verdict (CH1's third mandate): **PASS.** Every artefact computes the >SOTA delta against lightningcss full-CSSOM (the materializing comparator) and explicitly demotes cssparser to plane-disclosed flaw-probe. alphaB §0/§4 and SYNTHESIS §0.6 get the plane exactly right. The only plane-adjacent issue is the lightningcss baseline NUMBER dispersion (793/833/929/974), which is disclosed everywhere and correctly resolved by binding the gate to the same-run N≥50 median (SYNTHESIS §0.5:146) rather than any frozen figure.
