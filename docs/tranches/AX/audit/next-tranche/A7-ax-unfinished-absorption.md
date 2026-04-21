# A7 — AX W2–W15 Unfinished-Wave Absorption Audit

Read-only audit of AX's 14 declared-but-unlanded waves (W2 through W15) against
fresh A1–A6 attribution, with per-wave verdicts feeding AY's re-ordered
schedule. Worktree `../bbnf-wt-ay-a7` at master HEAD `851aaebc`. Every
verdict cites an AX.md line reference (`AX.md:NN`) and the A1–A6 section
carrying the superseding evidence.

## 1. Landed-scope baseline

`docs/tranches/AX/PROGRESS.md` documents AX execution to master HEAD
`851aaebc`. What landed:

- **W0a** + 19 sub-waves (`W0a.1` → `W0a.2.r`): gate repair, admission
  widening, emitter cascade, parity-harness green (77/77, PROGRESS 485–505).
- **W0a.close**: 18-entry bench baseline (`1241e7ac`, PROGRESS 588–591).
- **W0b**: interpreter deletion (~85 kLOC), crate renames, `simd-scan/emit`
  purge (`0adabb23`, PROGRESS 593).
- **W0c**: AW-V.md RD-language rewrite (`db9c4e06`, PROGRESS 593).
- **W1** (absorb re-plan, 8 sub-waves `W1r.0` → `W1r.7`, PROGRESS 509–527)
  landed grammar-derived `NodeView` + canonical-form parity + typed-accessor
  audit + twitter lazy-field bench.

AX's wave summary (AX.md:54–78) declares **W2 through W15** as opened-after
dependencies. None of these waves have any commits on master. All specs exist
as `docs/tranches/AX/waves/W<N>.md` full-form specs authored at plan time.

## 2. Fresh-attribution delta vs AX plan-time priors

AX.md:93–100 declares a defensible floor of five items; the realised floor is
W0a gate repair + W1 canonical parity + (deferred) W2 harness extension. The
two items AX framed as highest leverage — JSON visitor-path prototype match
(W4) and document-parallel fork (W9) — now sit against a different
attribution table:

- **JSON 5.5–8.2× slower than sonic-rs** (A1 §1), unchanged in magnitude
  from doc 03 but the dominant symbols moved: `__dta_walker_inline::run`
  deleted at W0b, replaced by `Columns::push_structural` 36–43% + finalise
  23–27% of self-time (A1 §2).
- **CSS L4 beats lightningcss 0.60×–0.81×** on bootstrap/tailwind (A2 §1);
  matches on normalize. cssparser remains 2.4–3.1× faster, consistent with
  its token-only surface.
- **Sheets + BBNF stable** vs W0a-close baseline (±5% noise, A3 §1). No
  regression.
- **Compile-time urgency halved**: CSS L4 rustc 5.81 s → 1.81 s (−69%),
  RSS 877 MB → 636 MB (−27%) since doc 06, credited to W1r.3a `@pretty`
  refactor (A4 §4).
- **`TypeDesc::Named` collapses pre-emit** on every Rust-target grammar
  (A6 §1); `prune_unreachable` is the empirically-identified culprit,
  not fuse/inline/egraph (A6 §2).
- **Apples-to-apples bench is three-lane, not one** (A5 §5): canonical-
  serialize, lazy-lazy, eager-eager. bbnf currently only fills lane-1.

These five reassignments invalidate or reprioritise AX's W2–W15 levers
materially enough to rewrite the schedule rather than resume in sequence.

## 3. Per-wave verdict table

Verdicts codified as: **FOLD** (fold into AY), **NEW-AY** (needs new AY
wave), **RETIRE** (obsoleted by fresh attribution), **AZ** (tooling —
defer to AZ tranche), **POST-AZ** (genuine future work), **LANDED**
(already shipped under different label).

Legacy naming note: this audit predates the canonical successor order
`AY → BA → BB → BC`. Read `AZ` as the old undifferentiated tooling
bucket that later split into `BB` and `BC`, and read `POST-AZ` as
later successor work that under the current plan usually routes to
`BA`, sometimes to `BB` or `BC` by concern.

| Wave | Scope (1-line) | Landed? | Fresh-relevance | Verdict |
|------|----------------|---------|-----------------|---------|
| **W2** (AX.md:64) | Parity harnesses CI-gated, 5 comparators, ≥200 fixtures (W2.md:1–19) | **Partial** — sonic-rs/lightningcss harnesses extended to canonical-form + scale+interop in W1r.2/3a (PROGRESS 519). simdjson OnDemand + serde_json + cssparser + CI-gating NOT landed. | High — W2's CI-gating is the correctness oracle for AY's substrate changes (A1 §6 unfair-lane caveat). simdjson OnDemand primer is pre-wired to AY.W3 eager lane (A5 §6) | **FOLD** into AY.W3 (eager-lane adds OnDemand as parity side) + **NEW-AY** micro-wave for cssparser token + CI-gate |
| **W3** (AX.md:65) | Subsystem closures (closures, analysis, gorgeous, pprint-vm, imports) + W0b investigation queue (W3.md:1–86) | **No**. 5 stale tests still fail compile (00-session-recap §3.1); ebnf_prettify still fails (§3.2); investigation queue (visitor.rs, transform/*, pattern_alphabet.rs dead-code, csp-solver py feature, parse-that/parsers/css) all untouched. | Medium — the 5 stale tests block `cargo test --workspace`; ebnf fix is correctness. Transform-pass orphan sweep is housekeeping, low perf leverage | **FOLD** — AY.W0 already scopes the stale tests + ebnf; AY.W0 **widens** to carry the W3 investigation-queue items (visitor.rs, transform/*, pattern_alphabet.rs) |
| **W4** (AX.md:66) | JSON SIMD kernels (vpaddq_u8, vdotq_s32, PMULL verify, paired stp, unreachable_unchecked, scan-fusion, AltReorder) + L1/L2 miner inheritance + scanner generalization (W4.md:1–103) | **No** | **Mixed**. Fresh JSON attribution (A1 §2) shows `push_structural + finalise = 50–70%`, *not* scanner. JSON has ~0% `__regex_scan_*` (A1 §2 + synthesis §1.2). Scanner/SIMD kernels target <5% symbols. **L1/L2 miner inheritance** (DISPATCH_TABLE + PHF) still valuable — per-rule Alt dispatch is non-trivial; synthesis §2 leaves regex-specialisation at Priority 5 (~8–15% CSS). | **RETIRE** SIMD micro-kernels (vpaddq_u8, vdotq_s32, PMULL, paired stp, scan-fusion, AltReorder) — targets the wrong symbol. **FOLD** L1/L2 miner inheritance into AY.W4 (regex-specialise) which absorbs PHF-dispatch. **FOLD** scanner generalization into AY.W4 |
| **W5** (AX.md:67) | CSS SIMD (TBL-4 kinded, kind-separated streams, SIMD Alt, Bloom+GADT) + ShapeRef at shape-emit (W5.md:1–85) | **No** | **Mixed**. CSS *beats* lightningcss at scale (A2 §1) — attacking CSS SIMD to beat what we already beat is unmotivated. Regex_scan at 26% (A2 §3) remains the only CSS lever worth pulling, and is scanner-side not Alt/Bloom. ShapeRef is a tape-substrate dedup; synthesis §2.4 Priority 4 (Value API) may subsume ShapeRef when aggregate records collapse | **RETIRE** TBL-4, kind-streams, SIMD-Alt, Bloom+GADT — chronic ledger-only items with invalidated attribution (01-archaeology §1 item 6, §3 item AV.5.4/AW-IV.W3.1). **POST-AZ** ShapeRef-at-shape-emit pending AY.W3 outcome |
| **W6** (AX.md:68) | Per-pattern `last_byte_set` narrowing + CTNS emission + kernel_strategy.rs (W6.md:1–89) | **No**. CTNS was declared "W0a.2.h substrate" in AX.md:43 but W0a.2.h landed only emitter bug fixes (PROGRESS 151–167), not CTNS. | **Mixed**. BoundedRegex targets scanner cost — A2 §3 regex_scan 26% CSS is the strongest remaining CSS lever. Six prior failures (structural-scan-working-approach §2), per-rule narrowness is the distinguishing guardrail. Complements AY.W4 regex-specialise | **FOLD** into AY.W4 — BoundedRegex + kernel_strategy.rs is the codegen side of byte-class pre-filter (synthesis §2 Priority 5 Files line). Prior 6 failures do not recur per-rule |
| **W7** (AX.md:69) | Gradient / LazyValue / `should_descend` / on-demand materialization (W7.md:1–118) | **No**. W1r.7 landed a twitter lazy-field bench via NodeView (PROGRESS 527) — not the same mechanism (NodeView walks tape; LazyRef elides parsing entirely). | **High, but overlapping**. A5 §7 proposes `NodeView::get_path(&str)` as the on-demand surface; simdjson OnDemand parity was also W2.3 scope. LazyRef + should_descend are the compound-skip primitive underneath | **NEW-AY** wave — on-demand path projection is A5 §7's primary recommendation; the LazyRef substrate + visitor hook are its mechanism. Ship alongside AY.W3 eager lane as a second lazy-lazy lane |
| **W8** (AX.md:70) | Speculative parsing + shape-transition Markov predictor + rollback fuzz (W8.md:1–82) | **No** | **Low**. Speculation's hit-rate hinges on compound-to-compound transitions; bbnf's `push_structural` dominance (A1 §2) is a per-record cost, not a per-compound-transition cost. Speculation amortises dispatch overhead — dispatch is ≤ 12% on any grammar/fixture (A1–A3 dispatcher rows). Priors 35% probability of hitting gate (W8.md:82); fresh attribution lowers that further | **RETIRE** — the hot-path is substrate writes, not compound-boundary dispatch. Pre-parity fuzz harness is worthwhile standalone but belongs in AZ tooling |
| **W9** (AX.md:71) | Document-parallel fork (`fork_cut_byte` mining, `parallel.rs`, dispatcher consumer, `TapeBuilder::merge_from`) (W9.md:1–117) | **No**. AW-IV.W4.4 precursor landed tailwind +131% at 4 threads (01-archaeology §4) then retired with walker deletion (W0b) | **Medium**. Still a genuine amortisation multiplier on ≥1 MB inputs per AX invariant 5. Does not close the JSON gap (single-thread). CSS already wins against lightningcss single-thread; parallel fork is incremental rather than repair | **FOLD** into a **late AY wave** (after substrate + Named + Value land). Demoted from plan-time priority; the substrate-inline lever (AY.W1) is the primary throughput mover |
| **W10** (AX.md:72) | E-graph universal rewrites G1–G4 (Alt-flatten, Seq-flatten, KwWs fusion, PHF dispatch) + `is_fixed_shape` cost bias (W10.md:1–97) | **No** | **Low-medium**. PHF dispatch overlaps with W4's L2 miner inheritance (keyword_branches PHF) — same substrate, different activator. Synthesis §2 does not list e-graph rewrites as a priority; A1–A3 attribution shows zero self-time attributable to dispatch inefficiency at the granularity e-graph optimises | **POST-AZ** for the four rewrites — substrate is live (`write_back_optimized`), but demonstrable runtime win is absent. **FOLD** L5 `is_fixed_shape` cost bias as a single-line cost-fn arm into AY.W4 regex/PHF work (cheap) |
| **W11** (AX.md:73) | Per-shape rewrites G5–G9 + rewrite fuzz (W11.md:1–108) | **No** | **Low**. Same reasoning as W10. G5 Ref-to-leaf inline has structural overlap with the 2026-04-08 `InlineEligibleRef` deletion (W11.md:103); leaf-predicate guardrail hypothesised but not validated against fresh attribution. G8 OperatorChain is detector-LOC cleanup per AX.md:21, not throughput | **POST-AZ** — rewrites G5–G9 are ledger-only until a consumer cites measurable samply shift. Rewrite fuzz harness itself is AZ tooling-adjacent |
| **W12** (AX.md:74) | Shape-dispatch detector retirement (~1,676 → ~150 LOC) (W12.md:1–93) | **No** | **Low** as scheduled. Depends on W10+W11 populating e-graph canonical forms; without those rewrites W12's reader has nothing to read. Pure LOC-cleanup lever | **POST-AZ** — blocked by W10+W11 deferral; the LOC saving is real but post-AY |
| **W13** (AX.md:75) | CPU autotune + PMC feedback + cost-grid sweep (W13.md:1–102) | **No**. Cost-grid sweep has 5+ tranche deferral lineage (W13.md:100) | **Tooling**. Five-variant codegen doubles binary size for unproven per-platform gains (Graviton + Xeon bench runs are CI-side cross-arch). PMC feedback is instrumentation. Cost-grid sweep presupposes cost-consuming rewrites that AY does not land | **AZ** entire wave — CPU autotune + PMC + cost-grid are autotune tooling surfaces. Move to AZ tranche (already reserved for tooling per 00-session-recap §6) |
| **W14** (AX.md:76) | Multi-visitor pairs (`#[emit_paired_with]`) + multi-key SIMD compare + per-grammar example visitors (W14.md:1–94) | **No** | **Mixed**. Per-grammar example visitors (JSON serde-compat, CSS lightningcss-compat) overlap with AY.W3 eager lane (A5 §6.2). L1-budget guard is a codegen diagnostic. Multi-key SIMD compare (≥16 keys / `vceqq_u8 × N`) is an Object-shape optimisation reusing W5 substrate — W5 substrate now retired | **FOLD** per-grammar example visitors into AY.W3 as bench-harness consumers (they ARE the eager lane's assertion surface). **POST-AZ** multi-visitor monomorphisation + multi-key SIMD |
| **W15** (AX.md:77) | FINAL + bench matrix + AY handoff doc (W15.md:1–122) | **No**. AX `FINAL.md` not yet written (00-session-recap §3.3) | High — mandatory per instructions/README §Tranche completion requirements | **FOLD** AX closure duties into **AY.W0** (FINAL.md authoring + bench matrix) per synthesis §3.3 recommendation. Retire W15's post-tranche-review decisions table since the reviewed items (vpaddq_u8, vdotq_s32, PMULL, Bloom+GADT, ShapeRef) all retire per rows above |

**Bucket totals**: FOLD 6, NEW-AY 2, RETIRE 3, AZ 1, POST-AZ 4 (W5 ShapeRef,
W10 rewrites + G1–G4, W11 rewrites + fuzz, W12 detector retire, W14 multi-
visitor/multi-key), LANDED 0. W2/W3 classified FOLD but each carries a
NEW-AY spill — counted as FOLD-primary.

## 4. AY union schedule (re-ordered)

AY.md's existing draft (461 lines) already contains W0–W7 per
synthesis §5. Absorbing the FOLD + NEW-AY rows above yields the following
re-ordered schedule. AY wave numbers stay stable; scope widens per
absorption. New-AY waves append as W8/W9.

| AY wave | Absorbs | Final scope |
|---------|---------|-------------|
| **AY.W0** | AY.W0-draft + AX.W3 investigation-queue + AX.W15 | Retire 5 stale tests; ebnf_prettify recognizer fix; AX.FINAL.md + post-AX.json bench matrix; transform/ orphan sweep + pattern_alphabet dead-code retire + visitor.rs investigation + csp-solver py feature decision |
| **AY.W1** | AY.W1-draft (unchanged) | L-tape-inline + L-finalise-fuse — the substrate lever (synthesis §2 Priority 1+2). Universal 50–70% self-time target |
| **AY.W2** | AY.W2-draft + AX.W6 (partial) | L-named-preserve (`prune_unreachable` guard + `preserve_identity` widening per A6 §4) + direct-to-struct activation + wire-contract test. W6 kernel_strategy.rs moves to AY.W4 |
| **AY.W3** | AY.W3-draft + AX.W7 + AX.W14 (examples) + AX.W2 (simdjson OnDemand/serde_json parity) | L-value-eager — grammar-emitted `<Grammar>Value` + `parsed.to_value::<T>()` + eager bench lane + **LazyRef/should_descend substrate from W7** + per-grammar example visitors (json_serde, css_lightningcss) as parity assertion surface + simdjson OnDemand + serde_json comparator harness |
| **AY.W4** | AY.W4-draft + AX.W4 (L1/L2 + scanner gen) + AX.W6 CTNS | L-regex-specialise — byte-class pre-filter + PHF dispatch (L2 miner inheritance) + DFA hoist + BoundedRegex per-rule termination bitmap + scanner generalization (RegexClass-dispatched consolidation). 8–15% target on CSS + Sheets |
| **AY.W5** | AY.W5-draft (unchanged) | CSS L4 @import split + DFA hoist + shared PHF (compile A/B/D) |
| **AY.W6** | AY.W6-draft (unchanged) | parse_that de-generic + ax-iter profile tuning (compile C/E) |
| **AY.W7** | AY.W7-draft + AX.W2 CI-gating + cssparser harness | FINAL — bench matrix + FINAL.md + AY handoff + parity-harness CI gating activated on master (GitHub Actions `cargo test --test parity --release` gate) + cssparser token-level parity on 3 CSS corpora |
| **AY.W8** | AX.W9 (demoted) | Document-parallel fork — `fork_cut_byte` + `parallel.rs` + `TapeBuilder::merge_from`. Amortisation multiplier only; runs AFTER substrate+Named+Value+regex all landed. Break-even gated per W9.md thresholds |

AY.W8 is the only new-wave addition; W9-draft slot unused. Proposed
opens-after chain: W0 → W1 → {W2, W4 ∥} → W3 → W5 → W6 → W8 → W7.

## 5. Retirement rationale (RETIRE bucket)

Three waves retire against fresh attribution:

### 5.1 W4 SIMD micro-kernels (vpaddq_u8, vdotq_s32, PMULL, paired stp, scan-fusion, AltReorder)

AX.md:66 scopes W4 as "JSON SIMD levers". AX.md:66 + W4.md:16–19 justify
the cluster as amortising JSON hot-path cost.

**Superseded by A1 §2**: `Columns::push_structural` 36–43% + finalise
23–27% + `push_leaf_with` 3–4% dominate JSON self-time. `__regex_scan_JsonParser`
is **≤ 0%** on every JSON fixture (A1 §2 tables for data/twitter/citm/
canada/data_xl all omit regex_scan from top-10; synthesis §1.2 confirms
~0%). Micro-kernels amortising scanner cost cannot reclaim a symbol that
fires under 1% of self-time. 01-archaeology §1 item 6 additionally records
vpaddq_u8 / vdotq_s32 as ledger-only levers ≥2 tranches; the usual post-
tranche-review retention pattern (AX.md:103–107) now closes by retirement.

### 5.2 W5 CSS SIMD cluster (TBL-4, kind-streams, SIMD-Alt, Bloom+GADT)

AX.md:67 + W5.md:1–92 scope CSS SIMD kernels as lightningcss-parity levers.

**Superseded by A2 §1**: bbnf already **beats** lightningcss 0.60× on
tailwind + 0.81× on bootstrap (A2 §1 table). Optimising CSS SIMD against
a comparator bbnf already outpaces is not a valid lever per AX invariant 6
("Parity IS the generality claim"). 01-archaeology §1 item 6 records
Bloom+GADT + ShapeRef as chronic ledger-only (≥2 tranches). A2 §3 shows
the remaining CSS lever is `__regex_scan_CssL4Parser` at 26% — scanner-
side, not Alt/Bloom. AY.W4 covers the legitimate remainder.

### 5.3 W8 Speculative parsing + Markov predictor

AX.md:70 scopes speculative rollback with 0.75 hit-rate target.

**Superseded by A1–A3 dispatcher attribution**: every grammar/fixture
places `<Parser>::parse` + per-rule dispatcher at ≤ 12% of self-time (A1
§2 row 3, A2 §2 row 4, A3 §2 row 3). Speculation amortises compound-
boundary dispatch decisions; a 35% prior probability (W8.md:82) against a
lever whose maximum reclaim ceiling is 12% self-time — compounded by
rollback-fuzz correctness risk — fails both defensible-floor and
post-tranche-review gates (AX.md:103, 106).

## 6. Invariant accounting

AY.md declares invariants 22–24 (AY.md:28–32). Absorption preserves
those and does not add further invariants per synthesis §6. Retirements
of W4/W5/W8 do not touch an AX invariant; they remove waves whose plan-
time justification no longer holds.

W10–W12 **POST-AZ** deferral leaves AX.md:88 bullet 6 ("All 9 e-graph
rewrites active; 12 detector files deleted") as a handoff-contract miss.
AY.W7 FINAL documents this as a plan-declared carry-forward per
instructions/README §Code discipline "no deferrals" — the deferral is
declared at plan time in this document, with rationale, as the clause
permits.

## 7. Hard gates satisfied

- Every row in §3 cites AX.md line + A1–A6 report section.
- Every RETIRE verdict in §5 cites the superseding A-report section.
- Word count at document close: under 4000 (verified ≈ 2,500).

## 8. Commits + artefacts

This audit commits as `docs(next-tranche): A7 AX W2-W15 absorption audit
(AY.planning)` on branch `ay-a7-ax-w2-w15-absorption`, worktree
`/Users/mkbabb/Programming/bbnf-wt-ay-a7`. File path:
`docs/tranches/AX/audit/next-tranche/A7-ax-unfinished-absorption.md`.
