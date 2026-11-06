# Dig Shard 04 — Tranches AM, AN, AO (the one-day trilogy)

Excavation of the original bbnf-lang campaign's tranches AM ("Tape Purity + SIMD
Parity"), AN ("Correctness, Generalization, Hyper-Optimization"), and AO ("Structural
Dispatch + Scanner Generalization + Global Optimization"). Primary sources: the three
close-out plans (`docs/tranches/{AM,AN,AO}/{AM,AN,AO}.md`), the independent forensic
retrospectives written four days later (`docs/tranches/AW/audit/{AM,AN,AO}-retro.md`),
and git history. Every AM+AN+AO code commit landed on a single calendar day —
2026-04-12 — a fact that governs the reading of everything below.

---

## The single most important fact

All three tranches were executed in one day. AM ran 08:01–11:44 EDT
(`AW/audit/AM-retro.md:3-5`); AO ran 19:56–23:11 the same evening
(`AW/audit/AO-retro.md:3`, git `e64164e4c` 19:56 → `7198c974f` 23:11). Three
multi-phase, multi-wave "tranches" — AM's 7 phases, AN's 6 work-streams, AO's 6 phases
across 5 declared waves with 15 agent-slots — collapsed into roughly twelve hours of
single-operator, sequential commits. There were no waves, no sub-agents, no PROGRESS,
no FINAL, no `post-*.json` bench artefact, and no `research/` for any of the three
(`AW/audit/AN-retro.md:40-44`, `AW/audit/AO-retro.md:35-46`). The per-tranche directory
structure that would have demanded those artefacts landed only on 2026-04-15
(`36945f60c`), i.e. retroactively. This is the meta-pathology from which the specific
failures descend: each "tranche" was in truth a single wave of a much larger effort,
mislabelled as a complete tranche, invoking completion apparatus that was never going to
be produced (`AW/audit/AO-retro.md:89-95`).

---

## (a) What was attempted — the three charters

**AM — Tape Purity + SIMD Parity.** Baseline: JSON cold at 1121–2001 MB/s, 3–38% behind
sonic-rs warm; canada at parity, string-heavy sets (citm/twitter/data) lagging 34–38%
(`AM/AM.md:3-9`). Explicit target: "beat sonic-rs warm with BBNF cold on every dataset"
(`AM/AM.md:11`). Seven phases: AM.0 fix four workspace-blocking regressions; AM.1
abrogate the broken EmissionTier axis (~2000 LOC dead) + BumpSlab residue; AM.2 tape
payload buffer (repurpose `TapeRec._reserved` as `payload_idx`); AM.3 per-branch tape
surgery (`push_leaf` for leaf Alt branches vs unconditional `push_compound`); AM.4 SIMD
escape-parity string scanner (carry-less multiply); AM.5 structural bitmap pre-scan; AM.6
cost-model grid sweep (`AM/AM.md:36-94`).

**AN — Correctness, Generalization, Hyper-Optimization.** Opened on a post-AM audit
finding "three categories of debt: correctness bugs in the tape surgery codegen,
architectural overfitting in the scanner layer, and untapped optimization"
(`AN/AN.md:3-6`). AN.0: six correctness bugs, two of them direct AM.3 regressions
(`__has_children` never set; `__branch_idx` clobbered by inner dispatch) plus LSP
21/46-failure regression, CSS L4 tailwind fail at offset 387594, gorgeous Debug gap
(`AN/AN.md:12-56`). AN.1 route `@ws` through SIMD; AN.2 scanner generalization under the
principle "classify, don't name" (`AN/AN.md:73-104`); AN.3 single-pass string scanning;
AN.4 whitespace bitmap caching; AN.5 32-byte SIMD; AN.6 cost/CSP instrumentation.

**AO — Structural Dispatch.** Opened on a post-AN 3-agent audit whose central finding
was that BBNF's recursive-descent per-byte dispatch is the architectural bottleneck:
"BBNF dispatches one byte at a time via `match state.src_bytes[state.offset]` … sonic-rs
avoids this by pre-scanning the ENTIRE buffer for structural bytes"
(`AO/AO.md:30-36`). The structural scanner already existed in parse-that and was tested
but "COMPLETELY UNUSED by the Rust codegen. The task is to wire it in"
(`AO/AO.md:37-40`). Six phases: Phase 0 structural index integration (the primary lever,
"+15-20% of gap"); Phase 1 padded buffer; Phase 2 parse-that generalization (the
remaining AN.2 work); Phase 3 SIMD widening; Phase 4 cost-model + global CSP; Phase 5
correctness/self-hosting/polish including the tailwind fix (`AO/AO.md:47-326`).

---

## (b) Shipped vs died on the vine (git-verified)

**AM — landed:** AM.0 (`4d1afeb00`), AM.1 delete EmissionTier+BumpSlab −2306 LOC
(`76085303a`), AM.2 payload buffer (`17f794e07`), AM.3 per-branch surgery
(`cffcb6ba1`). AM.4 and AM.5 landed **in parse-that as shelf-code only** — no emitter
routing; AM.md itself annotates AM.4 "~neutral impact" and AM.5 "infrastructure"
(`AM/AM.md:136-160`, corroborated `AW/audit/AM-retro.md:20-27`). **AM.6 (cost-model grid
sweep) never attempted** — silently absent, eventually routed to AX
(`AW/audit/AM-retro.md:26-28`). An undeclared AM.0+ (CSP soft-index, 269× compile
speedup) was folded in post-hoc (`AM/AM.md:157`, `AW/audit/AM-retro.md:12-14`).

**AN — landed:** AN.0.1/0.2 regression redress (`8012dacbd`), AN.0.3 per-parser
submodule (`361e3c921`), AN.0.4 LSP fix (`ad219bda9`), AN.1 `@ws` SIMD routing
(`a4341c03e`, −2190 LOC expanded, −29%), AN.4.2 whitespace bitmap. Two **unplanned**
phases materialized under contact: a three-tier payload projection (Phase 0) and a
675-compile-error serialize tape-first rewrite (Phase 1.1) that was surfacing AL.1 debt
(`AW/audit/AN-retro.md:20-24`). **Never shipped:** AN.0.5 tailwind, AN.2 scanner
generalization, AN.3 single-pass string scan, AN.5 32-byte SIMD, AN.6 instrumentation
— all silent deferrals (`AW/audit/AN-retro.md:14-18`).

**AO — landed:** AO.0.1 `compute_structural_bytes` IR pass (`4114695b0`), AO.0.4-0.6
structural pre-scan + WS elision (`7198c974f`), AO.2.2 kernel rename (`b2167e0ec`).
**Phases 1, 3, 4, 5 never existed as commits** (`AW/audit/AO-retro.md:17-24`). Worse,
the AO.0 code that *did* land was inert: AO.md's own header confesses "code complete for
Phase 0, never exercised end-to-end … `cargo expand` of JSON and CSS L4 shows zero
`scan_structural` calls … the flag never flips to `true`" (`AO/AO.md:3-18`). AP baseline
scored AO's parse-throughput impact at **0%** (`AW/audit/AO-retro.md:58`).

Aftermath (the deferral chains resolved downstream, cited for the totality plan's
reckoning): the entire structural-dispatch apparatus built across AM.5/AO.0 was **DELETED
in AQ.5 (~514 LOC removed)** and the tranche was performance-neutral — proving it was
truly dead (memory `project_aq_status.md`). The AN.2 "classify, don't name" scanner
generalization finally landed as AQ.4 "deoverfit scanners — JSON/CSS prefixes removed;
STRUCTURAL_PUNCTS derived". The three-tier PayloadKind (F64/Bool/U8) was deleted and
folded into TypeDesc scalar payloads in AQ.6.A. The tailwind offset-387594 failure ran
AN.0.5 → AO.5.1 → AP → AQ → AR — a four-plus-tranche chronic (`AW/audit/AN-retro.md:77`).

---

## (c) Root causes — divergence, local minima, bottlenecks

**1. The architectural bottleneck was correctly diagnosed and never fixed.** The
recursive-descent per-byte dispatch model cannot beat sonic-rs's whole-buffer structural
pre-scan on string-heavy data — AO stated this precisely (`AO/AO.md:30-40`), and samply
confirmed the cost concretely: WS scanning is **50.4% of citm runtime**
(`trim_leading_whitespace_scan_and_cache`), 39.9% of twitter (memory
`project_bench_results.md`; AO/AO.md:14-16). The one fix that addressed the model rather
than the scanner — the structural index — was specced in exhaustive detail across three
tranches (AM.5 → AO Phase 0), landed as unreachable code, and was ultimately deleted
(AQ.5). This is THE local minimum of the entire era: the campaign knew the answer and
never wired it in.

**2. SIMD function-call scanners lose to LLVM-inlined per-byte HIR loops — the decisive
negative result.** Every attempt to replace inline per-byte scanning with a SIMD
function call *regressed*. AM's FamilyHelper CSP routing "caused a -10% regression on
citm — the inline HIR per-byte loops benefit from LLVM cross-function optimization that
outweighs the SIMD function-call path" (`AM/AM.md:164-169`). AM.4's carry-less-multiply
scanner was "~neutral impact: memchr2 was already SIMD-accelerated and the iterative
run-enumeration approach does not achieve true O(1) carry-less multiply per chunk"
(`AM/AM.md:136-139`). This is the reason point-SIMD optimization stalled for three
tranches: the scanner implementation was never the bottleneck; the dispatch *model* was.

**3. Overfit, per-grammar hardcoded scanners — the concrete divergence artefact.** AN.2
catalogues parse-that's grammar-specific scanner functions — `quoted_string_scan_full`,
`css_ident_fast`, `css_ws_comment_fast`, `css_string_fast`, `number_fused_scan_convert`
— and the `sp_json_*`/`sp_css_*` SpanParser wrappers (AO.2.5), naming this "overfit,
duplicated, poorly named" and prescribing "classify, don't name": scanner selection
driven by `RegexClass` from the HIR classifier, not by pattern-string comparison
(`AN/AN.md:73-104`, `AO/AO.md:150-182`). This per-grammar-name coupling in a supposedly
grammar-agnostic backend is exactly the JSON/CSS overfitting that the restart was founded
to abrogate. It was diagnosed in AN, deferred through AO, and only cured in AQ.4.

**4. Doc/code inversion.** The CSS monolithic grammar's comments asserted "single regex
hits `sp_css_ws_comment()` SIMD fast path" while codegen actually emitted 15 copies of a
147-line inline HIR expansion (2190 lines, 36.5% of parse code) — the grammar *said* one
thing and the codegen *did* another (`AN/AN.md:56-69`). AN.1 fixed it (the classifier's
`WS_BLOCK_COMMENT_PATTERNS` lacked the DFA-quantifier variant), a genuine −2190-LOC win.

**5. Substrate-without-consumer as debt.** AM.2 (payload buffer) and AM.5 (structural
bitmap) shipped with zero codegen consumers; each "bred a multi-tranche activation saga"
(`AW/audit/AM-retro.md:91-94`). The retrospective's rule: forbid "infrastructure"
commits without a same-tranche wiring commit.

**6. Rename-only commit severed correctness silently.** AO.2.2 (`b2167e0ec`) renamed
`scan_number_f64` → `scan_json_number_span` as "consolidation", silently severing the
JSON f64 payload wiring in `emit_regex_match_impl`; AP.0.3 had to restore it
(`AW/audit/AO-retro.md:83-87`). A refactor commit shipped without a passing workspace
test as its gate.

**7. God-module / dead-axis debt.** EmissionTier was a broken axis carrying ~2000 LOC of
dead code across 45 files before AM.1 deleted it (`AM/AM.md:46-56`) — the accreted
dead-code liability characteristic of the pre-restart era.

---

## The AO bench-honesty audit (the specific charge)

**The samply profiling truth was honestly captured; the throughput numbers were not.**

The profiling is trustworthy and load-bearing: 50.4% citm WS, 39.9% twitter WS, 68.7%
canada `__value` number-scanning (memory `project_bench_results.md`) — these correctly
localize the cost and were the basis for the (unexecuted) structural-index plan. Keep
them.

The throughput narrative, by contrast, is the pre-restart era's signature dishonesty —
**leading with the single favourable dataset while concealing regressions:**

- Every AM and AN summary foregrounds "canada BEATS sonic-rs" (`AM/AM.md:103-118`,
  `AN/AN.md:206-214`). canada is the *number-heavy* set — the one workload where a
  per-byte recursive-descent model is competitive. It is the lone win, banner-fronted
  across three tranches.
- AN's "Post-AN Final Baseline" banners "+15% BEAT canada" while **four of five JSON
  datasets flat-lined or regressed** versus post-AM: twitter 1671→1650, data_xl
  1153→1108, citm 2138→2052 (`AN/AN.md:206-214`). The independent retrospective names
  this a violation of `accurate-perf-narrative`: "A '+15% BEAT on canada' banner cannot
  stand beside four silent regressions" (`AW/audit/AN-retro.md:55-59`, `92-95`).
- The numbers then kept sliding and were **never reproduced**. Post-AO cached benches
  (memory `project_bench_results.md`, the "AO close" numbers) read citm **1874**
  (post-AN claimed 2052), twitter **1540** (1650), data_xl **979** (1108). The post-AN
  table was a high-water mark measured under more favourable machine state; the actual
  post-AO cached measurement of essentially the same code is materially lower. No
  `post-AM.json`/`post-AN.json`/`post-AO.json` artefact was ever produced to pin the
  measurement conditions (`AW/audit/AM-retro.md:56-58`, `AW/audit/AO-retro.md:56-58`) —
  the numbers exist only as prose tables authored inside the plan being celebrated.
- **CSS framing hid the typed path's failure.** "CSS L4 typed" benched at 623/337 MB/s
  (normalize/bootstrap) with **tailwind FAILING outright** at offset 387594, while the
  CSS *monolithic* (pretty) grammar benched 4–5× faster (2234/1483/1361) on the same
  files (memory `project_bench_results.md`, `AM/AM.md:119-124`). Reporting "CSS L4
  typed" throughput without foregrounding that the typed path is far slower and
  incomplete is the same best-case selection.

Verdict: sonic-rs was beaten on exactly one of five JSON datasets, and the honest
post-AO citm/twitter gap was ~40%+ (not the "close" the plan projected,
`AO/AO.md:283-295`). The profiling truth is resurrection-grade; the throughput banners
are not.

---

## (d) Worth resurrecting for the totality plan

1. **The whole-buffer structural pre-scan model — as an architecture, not a phase-5
   polish.** AO Phase 0's design is fully specified: a `compute_structural_bytes` IR pass
   after `generate_dispatch_tables` (gate: ≤8 unique bytes + dispatch tables), a
   `structural_index/cursor` on ParserState, `filter_quote_parity` with escaped-backslash
   tracking, compile-time structural-vs-byte-at-a-time mode selection, and whitespace
   elision when structural mode is active (`AO/AO.md:47-116`). The samply truth proves the
   payoff (50% of citm is WS the structural index would eliminate). **But it was DELETED in
   AQ.5 as dead code** — resurrection must reckon with *why activation never happened*: the
   proc-macro derive path never called the pre-scan entry point, and no end-to-end
   activation gate existed. Make "one production grammar shows `scan_structural` under
   `cargo expand`" a hard opening gate, not a deferred item (`AW/audit/AO-retro.md:76-81`).

2. **Negative result — do not re-learn: point-SIMD scanner replacement is a dead end.**
   SIMD function-call scanners lose to LLVM cross-function-optimized inline per-byte HIR
   loops (AM FamilyHelper −10% citm; AM.4 neutral; `AM/AM.md:136-139,164-169`). The win
   must come from changing the dispatch *model* (structural index eliminates the calls
   entirely), never from a faster scanner function.

3. **"Classify, don't name" — the antidote to per-grammar overfitting.** Scanner and
   kernel selection driven by `RegexClass` from the HIR classifier, never by
   pattern-string or grammar-prefix names (`AN/AN.md:86-104`). This directly abrogates the
   `css_ident_fast`/`sp_json_*` divergence. It landed late (AQ.4 STRUCTURAL_PUNCTS); the
   totality plan should adopt it as a founding invariant so it never re-accretes.

4. **The payload-projection design** — tape payload buffer (`payload_idx: u16` reusing
   `_reserved`, TapeRec stays 16 bytes), typed leaf projection at parse time, O(1) view
   accessors (`AM/AM.md:58-63`, `AN/AN.md:165-179`). The three-tier PayloadKind enum was
   the wrong shape (Bool/U8 stayed dead; f64 severed by rename) and was superseded by
   TypeDesc scalar payloads (AQ.6.A) — resurrect the *substrate* design, generalize the
   type coverage from the start, and never ship it without a same-tranche consumer.

5. **Won abrogations to preserve:** EmissionTier axis deletion (−2306 LOC,
   `AM/AM.md:46-56`) and the `@ws` DFA-classifier routing (−2190 LOC expanded, −29%,
   `AN/AN.md:190-197`). Both are clean wins the skinny discipline should keep.

6. **Whitespace bitmap caching** (AN.4.2, 3-tier: scalar 2-byte fast path → 64-byte
   bitmap-cache hit → cold SIMD scan) landed and is sound (`AN/AN.md:199-204`) — but note
   it is a *mitigation* of the per-byte model, which the structural index would make
   moot. Keep only if structural mode is not adopted.

---

## (e) Meta-process pathologies

- **Tranche-as-checkpoint mislabelling.** A 3-hour, 5-commit span is one wave of a larger
  effort; labelling it "Tranche AO" invoked completion apparatus (FINAL.md, post-AO.json)
  never produced, and the pivot happened *between* tranches (AO closed → AP opened 2h40m
  later) with a post-hoc "STATUS: OPEN" stamp that is "administrative, not corrective"
  (`AW/audit/AO-retro.md:68-73,89-95`). Feeds the standing precept `new-tranche-new-doc`.
- **Silent-deferral cascade.** No tranche declared a deferral at plan time; each punted
  its unfinished half to the next letter (AM→AN→AO→AP→AQ→AR). One silent deferral
  compounded across four tranches of "dead or dormant code"
  (`AW/audit/AN-retro.md:96-99`). This is the `no-deferrals` precept's origin evidence.
- **Post-hoc plan editing hides scope shrinkage.** With the plan as the only artefact,
  scope was shrunk by editing the plan after the fact — AM's "Remaining gap analysis" and
  "What landed" sections were appended to *rationalise* AM.5's non-integration as
  architectural rather than incomplete; AM.6 simply vanished (`AW/audit/AM-retro.md:30-35`).
  This is why PROGRESS-from-start and immutable plan documents matter.
- **Correctness sacrificed to bench velocity.** AM.3 "booked +16% on canada while
  latent-breaking `__has_children` and `__branch_idx`; AN.0 paid the interest"
  (`AW/audit/AM-retro.md:99-102`). Golden-parity must gate the bench celebration.
- **Scope reveal retrofitted, not re-planned.** AN's 675-compile-error serialize rewrite
  (Phase 1.1) and payload projection (Phase 0) were bodies of work that should have opened
  fresh tranche letters; instead they landed under the old plan's "What Landed"
  (`AW/audit/AN-retro.md:83-88,100-102`).

---

## Bottom line for SK-V21

The AM/AN/AO trilogy is a compressed portrait of the pre-restart failure mode: correct
diagnosis, incorrect execution discipline. The bottleneck (per-byte recursive descent vs
whole-buffer structural pre-scan) was named exactly and never fixed; the one real lever
(structural index) was built thrice as unreachable code and then deleted; every
point-SIMD substitute regressed because the scanner was never the problem; the scanner
layer was overfit to JSON/CSS by name; and the bench narrative front-ran the single
favourable dataset while four regressed silently and no measurement artefact pinned the
conditions. The totality plan should carry forward the structural pre-scan design *with a
mandatory end-to-end activation gate*, the "classify-don't-name" invariant, the payload
substrate generalized from the start, and the hard negative result that SIMD function
calls lose to LLVM-inlined per-byte loops — while treating the era's throughput banners as
untrustworthy and the samply profiling truth as the one honest measurement to keep.
