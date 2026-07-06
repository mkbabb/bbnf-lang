# SK-V21 SPEC-DRAFT-P1 — The Convergence-Loop Re-Founding (pass-2 amended)

Date: 2026-07-05. HEAD: `734bfec51` (charter commit; no code has landed since SK-V20's close at
`05302109d` — cutover commit 0 was never executed, per PASS-ALPHA.md:3-4 and report 35:6-9).
Evidence base: pass-0 reports `research/01-32`, gap-fills `research/34-37`, adversarially verified
by 30/31 (13/14 historical and 13/15 now-state claims CONFIRMED; two corrections consumed below).
This draft amends the SK-V20 TW0-TW6 manifest wave by wave; where it supersedes, it says so and why.

Accounting note, updated at pass-1 close: report 33 (`33-cold-truth-profiling.md`) now EXISTS on
disk — the recovered cold-per-parse JSON+CSS capture (json_canon_bench, N=200, loadavg 8.3-10.2).
It joins report 21 as the profiling truth of record and supersedes 21's warm JSON numbers. Both
captures ran above the charter's loadavg<2: every performance ranking in this draft remains
DIRECTIONAL until the TW0 quiet re-capture runs (32 Part IV, U1). Where PROTO-P2's private harness
(loadavg 16-17) and report 33 (loadavg 8-10) disagree, the quieter capture governs (CRIT-P2-v2).

Accounting note, updated at pass-2 close: the pass-2 instruments (PROTO-A1 bench adjudication,
PROTO-A2 kernel PMU, PROTO-A3 TW-S pricing, PROTO-B1 TOML-real, PROTO-B3 width fix, research/38
seam disposition) are folded throughout. The environmental adjudication is explicit and binding:
NO pass-2 session met the loadavg<2 quiet mandate (A1 at 8.6-9.1; A2 at 8-16; A3 at 7-25; B1 at
~24). Every pass-2 DECISION is certified at the ratio level — paired per-sample interleaving with
95% CIs (A1 D1), same-binary min-of-7 PMU ratios (A2), same-window cross-harness reproduction
(A1 reproduces report-33's 1.559 cyc/B at 1.5654; A3 divan-vs-criterion 4-11%), and byte-equality
parity incl. fingerprint on all three corpora (A1, stronger than 33's XOR-under-black_box). Every
ABSOLUTE remains DIRECTIONAL. The quiet (loadavg<2) re-certification is a TW0 EXECUTION gate — a
scheduled tranche deliverable, no longer an open research question. One pass-2 ratio is explicitly
NOT decision-grade: the A3 nightly-vs-stable channel comparison is load-confounded (§3) and its
clean capture also rides TW0.

---

## §0 — The charter, restated against the evidence

A fully generalized parser generator that bests every SOTA parser for its targeted structured-data
languages, by: (1) backend-agnostic core, zero grammar-specific code in generic crates; (2) a
specializing emitter in the dav1d discipline — reference Rust, cost-model-selected grammar-agnostic
aarch64 kernels, checkasm bound to liveness — with the honest asm bound now SETTLED: hand asm
buys 0-5% over well-shaped intrinsics on Apple silicon, and the pass-2 door pricing retired the
one candidate exception — P5's "~10% instruction-selection miss the intrinsic cannot survive" is
REFUTED by A2: door (a), the pure-intrinsic `vshrn` movemask, beats the hand asm by ~20-28% on the
same classify kernel (+34.6/36.5% over baseline vs asm's +6.7/10.4%), because the binding resource
is the 8 SIMD→GPR crossings, not the classify ALU. Zero measured asm exceptions remain in the
kernel census; the classify kernel is now the case that PROVES an intrinsic obviates the asm lane
(25:263-271; P5; A2); (3) full type resolution — a genuine rank-1 HM core over the two open-ended surfaces,
structural projection for the rest (28 §6); (4) a first-class Value API — a lazy cursor over the
ONE tape plus a DISTINCT eager single-pass fused sink (the dual-path shape sonic-rs ships, matched
knowingly: the one-substrate fold-over-cursor was benched at pass 2 and LOST, §4), one
grammar-derived path alphabet, every comparison asymmetry disclosed at the claim site.
NO workarounds, NO stubs, NO legacy code; aarch64/Apple-M only; cold benchmarks only; ONE codegen
path. The "hand-rolled ASM" half of the aim is scoped explicitly (per 32's standing-aim gap): the
emitter's default emission is NEON intrinsics; an `asm!` emission path is admitted per-kernel only
on a checkasm-proven compiler failure. The doors were priced at pass 2 (§5.4): no kernel currently
qualifies; the per-kernel gate remains standing law, presently unexercised.

## §1 — The master pathology and its cure (binding on every wave)

The dig is unanimous on the single failure mode: **substrate-without-consumer**. Fourteen instances
across eighteen tranches (09:190-206); the #1 chronic anti-pattern of the AK→AV retrospective
(07:174-192); AI built six waves on infrastructure its own plan documented as dead-on-arrival
(30 claim 4); the AO structural-dispatch chain landed at 0% parse impact and was deleted wholesale
at AQ.5 (04, 30 claim 3). The one executed cure in the whole corpus is the W→X→Y→Z arc
(git-tagged, 30 claim 14): the **Y.13 consumer-invariant test** — an exhaustive-match gate over
every emitted decision variant that fails to compile until each variant has an observable consumer
(15:17-24; 35 N21).

**The cure, as standing law for SK-V21:**

1. Every **substrate/kernel/value wave (TW1-TW4)** closes on a **consumer-invariant runtime gate in
   `crates/`** — never on a document, never on a source grep. The gate template is Y.13's: exhaustive
   over the wave's emitted decision surface (kernel selection, tape-layout shape, value-plane
   variant), plus runtime evidence of activation — and, per P1's lesson, liveness alone is
   INSUFFICIENT: the gate is liveness AND a measured ≥3% win (a scan proven live under `cargo expand`
   still lost). Hygiene/stack/type waves (TW0, TW-S, TW5, TW6) close on their own named gate class
   (build+bench parity, oracle green, falsifier-fires); structural LOCKS may be path greps — that is
   a lock class, not a wave-close class, and the two are deliberately distinct (07 §5.3).
2. **No substrate lands without its consumer in the same wave.** The four-hop pipeline — mined IR
   fact → pluggable cost model → emitter selects body shape → consumer reads the emitted const —
   closes all four hops in one wave or the wave does not open (07:189-192).
3. **Profile-driven over spec-driven**: the corpus scores profile-driven tranches 2/2
   (BEAT-or-peak, zero dead substrate) against spec-driven 12/14 dead-substrate instances
   (09 §C4). Every wave benches every grammar (the CSS/Sheets panic that JSON-only closes hid —
   09 §C5 — is the standing counterexample).
4. "Complete with recorded misses" is not a close state (09:204-206). A wave that cannot close is
   re-planned under a new letter, never absorbed (memory: new-tranche-new-doc; 08 meta-lessons).

## §2 — The performance thesis, rebuilt on dig truth

The pre-restart campaign's own artifacts settle the architecture question:

- **fn-per-rule recursive descent won.** The AW-V.W2.1 prototype (`bbnf-json-prototype`, 2,246 LOC:
  RD + inline SIMD kernels + one monomorphic visitor, no interpreter) beat sonic-rs 5/5 on every
  JSON entry at 0.89-0.94× sonic ns/iter, 91.15% self-time in one monomorphized symbol (07 §3, §5.1).
  The DTA/tape-automaton family is settled-negative: non-uniform state bodies (40-400 LOC) cannot
  compile into fast code (07 §4.1). The generalization mechanism is shape classification from IR
  (12-variant ShapeTag; 10/15 hand-tunings IR-derivable — 07 §5.2).
- **The historical peak is AU**: twitter 1967 MB/s, 1.63 cyc/byte (09:144 table; report 09's prose
  "0.615" is bytes/cycle mislabeled as its inverse — the arithmetic 3.2e9/1967e6 ≈ 1.63 settles it),
  0.91× sonic — and its primitive set is recorded: flat AoS `Vec<TapeRec>` (one 16-byte store per
  push), unified `push_leaf_with(kind, PayloadData)`, `.map(|_| ())` elimination, per-grammar
  capacity fingerprint (09 §C1, §D1). Every tranche after AU regressed from it. AU was measured
  under the warm harness regime, was never re-derived by the verifiers, and is contamination-
  adjacent (30 claim 6) — it is a DIRECTIONAL aspiration, never a gate. **The single governing JSON
  floor is the TW0 fresh cold baseline**; every no-regression clause in this spec binds to it and
  to nothing else. An AU-relative cold capture (fused ns/B vs AU's recorded coordinates) is taken
  at TW0 for the ledger, as context, not as a close condition.
- **The BEAT-sonic lever is now cold-corroborated, per-corpus, with one honest loss.** The old warm
  0.99×-geomean provenance is retired: P2 measured the actual fused single-pass cold, and report 33
  independently measured the identical entry point (`direct_struct::track1_digest`) on the mandated
  `json_canon_bench`. The cold facts, now TRIPLY sourced: fused single-pass beats the tape-then-walk
  reconstruction path by ~3.2× (P2's construct; the spec's former fold-over-cursor construct pays
  3.9-4.2× vs the sink — the two constructs are distinct, A1 D3), and beats sonic-rs cold on twitter
  and citm (~0.5-0.6× ratios). **canada LOSES to sonic cold — the sign is SETTLED**: A1 D2
  re-measured it on the same instrument (fused/sonic 1.252× wall, 1.137× cyc/B), confirming report
  33 (9936 vs 11794 Mbps) and refuting P2's private 0.79× (divergence attributed to
  mimalloc+thin-LTO+per-sample-clone under loadavg 16). The loss is the eager `materialize_f64`
  float tax, the sharpest optimisation signal in the corpus, pending TW4.1's Eisel-Lemire/SWAR
  decode. The lever is stated per-corpus, never as a sweep or a geomean. **The fused parse+value
  single-pass SINK is the default materialization lane** (09 §D2) — and it is now benched AS the
  default: the fold-over-lazy-cursor construct §4 formerly named is benched NEGATIVE (loses sonic
  2.3-4.9×, loses its own sink 3.9-4.2× — A1 D3); that sub-gate is CLOSED, not open. At the AU
  anchor (A1 D4): structural `parse_tape` twitter 1.5654 cyc/B BEATS the 1.63 floor (reproducing
  33's 1.559); the fused sink meets it on citm (1.354), runs +23% on twitter, 2.04× on canada
  (float tax localized).
- **The structural pre-scan presize is resolved NEGATIVE: DELETE.** Six attempts at a universal
  stage-1 pre-pass, six losses (07 §5.4); eager whole-input scan regressed −64% on twitter (09 §D7);
  and P1 is the seventh and terminal loss: `OneShotSimd` — proven LIVE by three witnesses (runtime
  backend, `cargo expand`, `nm`) — pays a full stage-1 pass plus a throwaway positions Vec purely to
  size a tape, regressing 1.6-2.4× directional; and the pass-2 interleaved-paired re-run (A1 D1,
  N=200, 95% CIs) settles the `Sampled` question with one nuance: P1's canada +9.3% is OVERTURNED
  to +1.25% CI[-0.43,+2.94] TIE (inter-invocation drift, as CRIT-P1 predicted), citm ties, but
  twitter shows a statistically real small win (+3.45%, CI[+1.07,+5.83]). `OneShotSimd`/`Exact`
  are refuted overwhelmingly (they pay 30-170% to reclaim ≤3.5%); `Sampled` is deleted for KISS —
  it buys ≤3.5% on small inputs and nothing on large — NOT for a flat null. The decision is closed,
  not carried to TW4: **delete `Sampled`/`Exact`/`OneShotSimd`, keep `GrowOnly` only** — the
  deletion rides TW1's N24 reclaim sweep (note: the family has ZERO occurrences in `core`; the
  delete closes in `crates/simd-scan`/`crates/ir` — 38 §1). The interleaved CI re-run has now RUN;
  only the quiet-absolute leg rides TW0, and it does not reopen the decision. A
  CONSUMED stage1→stage2 index is a different design and was never refuted (P1's scope boundary);
  it may only return via the §5 cost table with a measured win.
- **Where the time actually goes** (21, directional): the live JSON parse is entirely scalar
  single-pass; number decode is hand-rolled, NOT serde_json (the 19-vs-21 conflict is adjudicated
  for 21 — 31 item 11; 34 headline). Number materialization + digest costs ~0.6 cyc/byte, +45%
  over parse_only — that ~30% of the typed figure is the legitimate target (SIMD/SWAR + Eisel-Lemire
  via the already-vendored lexical-core path — 23 §3). This matches the external frontier exactly:
  simdjson's stage 2 / materialization dominates on Apple silicon (canada: 1.847 c/B stage-2 vs
  0.547 stage-1 — 22 §2); On-Demand's margin comes entirely from unread substructure and does not
  transfer to full materialization (22 §1) — bbnf's full-typed-AST mandate means the win lives in
  number decode and per-value store cost, not structural skipping.
- **The benchmark truth-floor**: the cold-per-parse JSON lacuna (the committed 8349 Mbps vs 25,448
  warm ~3× harness gap — 21:50-77) is CLOSED in kind: `json_canon_bench` (mirror of
  `css_canon_bench`, N=200, `proc_pid_rusage` PMU) exists and produced report 33 — and at pass 2
  A1 rebuilt it with the two named gaps CLOSED: the `assert_eq!` digest-parity gate is folded and
  PASSING (full byte-equality incl. fingerprint, fused==foldcursor==sonic, all three corpora —
  strictly stronger than 33's XOR-under-black_box), and the canada sign is adjudicated on it (D2).
  What remains for TW0 is the quiet leg ONLY: loadavg < 2 re-capture plus an interactive-samply
  flame — the prerequisite for *ranking* any lever (32 U1). Per-corpus-shape reporting
  (numeric/string/nesting-dense) replaces aggregate
  single numbers (23 §7). CSS holds under cold discipline even at load (~2.0-3.6× lightningcss
  full-CSSOM — 21:32-48) but the eager-depth asymmetry stands until TW3 closes it (§4).

## §3 — The stack abrogation (new wave TW-S; no SK-V20 antecedent — 35 N26)

Report 19's headline, verified by 31 items 9/15: **every nightly feature the toolchain pin cites is
dormant** (`-Zthreads`/`-Zshare-generics` removed after a measured 28× warm regression; cranelift
commented out; `rust-toolchain.toml:6-8` vs `.cargo/config.toml:227-291`). The pin is ~12 weeks
stale. The abrogation:

1. **Edition 2024 unification**: skinny `edition = "2021"` → `"2024"` (one workspace line +
   FOUR `unsafe extern` wraps (P3's corrected count; sites enumerated in PROTO-P3:47-48) +
   `cargo fix --edition`; 166 files/~68.6K LOC; the `crates/` migration is the in-repo precedent —
   19 §a). MSRV 1.78 → 1.88 (simd-json 0.17/criterion floor). P3 proved this leg and A3 PRICED and
   APPLIED it: five edits total (edition 2024, rust-version 1.88, four unsafe-extern wraps),
   applied and verified in-worktree — the divan port builds and runs EXIT 0 on BOTH stable 1.96.1
   and nightly-2026-04-11 under edition 2024. The migration is executed-mechanical, not estimated.
2. **Pinned stable** (Path A, 19 §b): `channel = "1.96.1"` (then-current stable; 31 item 15).
   Retires the AttrId-ICE bisect treadmill; drop the cranelift component. Nightly returns only
   under a measured cold-build win. Prototype P3 executes and prices this before freeze.
3. **Lock-15 release profile** (OWNED by TW0, landed early so every later bench is honest; listed
   here for stack coherence only): root `[profile.release]` lto thin→fat, add `panic = "abort"`
   (the thin config is documented as halving twitter throughput — 19 §d; 31 item 10); add
   `panic = "abort"` to bench; **add an Apple-M `target-cpu` to the bench profile** — currently set
   nowhere, "the largest untapped zero-risk throughput lever visible in the profile surface" (19 §d).
4. **Dependency modernization** (19 §c), now PRICED (A3 D4: per-dep from→to+risk table with
   TW-S.1-5 wave assignment): delete criterion from skinny (divan is the frozen harness;
   half-migration finished), thiserror 1→2, toml 0.8→1.x, cssparser 0.34→0.37, simd-json →0.17
   single-sourced, nom 7→8, tree-sitter →0.26, wasmtime →current, once_cell deleted (std
   `LazyLock`), `ron = "0.12"` pin confirmed. **Preserve the regex-syntax excision** — the bespoke
   HIR is already SOTA-clean; no action (19 §c.1). Two A3 scope corrections: (a) the criterion
   deletion is NOT a dep swap — gate.rs/report.rs carry 60+ `criterion-estimates.json` ingestion
   references that must be re-pathed first; that re-path is its own sub-wave (TW-S.2). (b) divan's
   scope is disclosed: it is the WARM/iterated kernel-microbench tier only (median-of-samples,
   optimistic 4-11% vs criterion's regression slope, same-window cross-check); the cold-single-
   parse discipline stays in the `harness = false` mains.
5. `std::simd` is NOT adopted at any emitter layer; `core::arch` intrinsics remain the emission
   target (29:71-72).

**Scope decision (resolves P3's "MIXED" verdict):** the stable-build requirement covers **what
survives the cutover** — the skinny workspace and every root crate the totality plan retains. The
legacy root `crates/` tree's stable failure dies at one leaf: `parse_that` → `smallbox` with
`features = ["nightly"]` (`../parse-that/rust/parse_that/Cargo.toml:97`, E0554 genuine). That leaf
is retired-with-the-legacy-tree, not fixed: the cutover abrogates the pre-restart consumers of
`parse_that`, so its nightly surface is moot, not a blocker. NO "de-nightly parse-that first"
precondition is added — CRIT-P3 shows the coupling claim conflated two crates: §7.3 routes skinny
onto the canonical **regex** engine (`../parse-that/rust/regex`), whose dependency set carries no
smallbox and no `parse_that`; the regex unification does not re-import the nightly surface. The
residual `cargo tree` verification is DISCHARGED at pass 2 (A3 D2): smallbox count = 0 through the
canonical parse-that/rust/regex subtree including egraph/csp-solver — §7.3 is DECOUPLED from the
abrogation on the graph, PROTO-P3's coupling recommendation is falsified, and the sole blocker is
the legacy root `crates/` tree via `parse_that 0.4.0 → smallbox[nightly]`, which the cutover
retires (moot) or a parse_that de-nightly fixes.

Close gate: skinny + all surviving `crates/` members build on the pinned stable, edition 2024;
bench parity re-proven cold against the TW0 baseline. TW-S is now PRICED (A3: migration applied,
five edits; divan built+ran both channels; dep table with TW-S.1-5 assignment; the criterion-
ingestion re-path scoped as TW-S.2). One honest hole remains: the nightly-vs-stable parity RATIO
is LOAD-CONFOUNDED (the nightly window ran sustained loadavg 19-25 vs stable's lighter window; the
raw 2.37×/1.82× gap is oversubscription, not channel — same 1.96-generation LLVM, no mechanism);
the clean channel ratio is captured at TW0 quiet, and the cold parity measurement remains a TW-S
close deliverable, not a discharged fact.

## §4 — The value plane and the one-tape substrate (TW1 + TW3, amended)

**The C1 reversal, resolved.** SK-V20's TW1 title "AoS→SoA" collides head-on with the dig's
hardest-won negative result: a 7-column SoA tape split was measured to LOSE to flat AoS
`Vec<TapeRec>` + parallel `sib_skip` on cache locality — "the most important architectural reversal
of Era VI"; the SoA 7-column pivot cost −60-76% (35 N19; 09 §C1; 14). This SPEC resolves the
tension in the dig's favor: **TW1 lands the ONE tape as AoS hardening** — single-writer
`frame_depth` stamped once by one owner (13 D1), atomic multi-column rollback, `sib_skip`
side-plane, the AU primitive set (§2) restored as the floor. Lock 1's exactly-one-encoding goal is
met by deleting the dual-encoding trap (`TapeStructBuilder`, 7 hits, all hand-written) and the
three eager-builder families (~1,080 hits), not by a columnar pivot. **The hit-counts are regen
artifacts, not hand blast radii** (38 §1, binding amendment A2): `core` is 81.6% xtask output
(176,887 of 216,818 src LOC); `StructBuilder` is 95.1% generated, `StructLayout` 88.5% — the true
hand-edit surface is ~40-60 emitter-template/runtime-type sites followed by regen. A SoA layout
may only return via a fresh measurement that
names why its coupling differs from AY-I.W1's failure (35 §1 C1). Read-side column caches (the
4-lane reordered-unrolling reducer, 6.57× on canada — 09 §D6) are admitted as read-optimizations
that leave the write path AoS.

**The value plane (TW3)** stands on three externally-corroborated design facts:

- **Lazy-escape/HasEsc is confirmed SOTA shape**: JustJson independently converged on borrow-when-
  clean, decode-escapes-on-touch (23 §6). Escape density becomes a controlled bench axis.
- **Eager is a DISTINCT single-pass fused sink; the one-substrate fold-over-cursor is benched and
  RESOLVED NEGATIVE.** The pass-1 thesis ("eager is a generic fold over the lazy cursor — one
  substrate," the stronger claim over sonic-rs's dual path) was run exactly as this section
  demanded (A1 D3: fold-over-cursor vs sonic dual-path direct-to-struct) and LOST on all three
  corpora — 2.32×/4.85×/2.27× slower than sonic (twitter/canada/citm), 3.87-4.23× slower than its
  own fused sink, 5.3-8.4× the AU 1.63 floor. The hedge this section carried ("a fold-over-cursor
  could plausibly lose" — 23 §1, §8) resolved TRUE. The value plane is therefore two lanes over
  the one tape-side substrate: **lazy = the cursor over the ONE tape; eager = the tapeless fused
  single-pass sink** — the dual-path shape sonic-rs ships, MATCHED knowingly, not bested; the
  "stronger claim" is retracted and the elegance argument concedes to the measurement. One path
  alphabet is retained across both lanes (sonic-rs's `pointer!` proves
  one alphabet suffices — the four JSON-pointer-shaped implementations collapse); the serde
  `Deserialize` bridge rides the cursor; runtime reflection (facet) is explicitly rejected (23 §5).
- **The lightningcss comparison requires eager-depth parity, honestly scoped** (24): lightningcss
  materializes a ~100+-variant typed `Property` CSSOM eagerly while bbnf's typed catalog is a
  27-unit hand-curated TOML; AND lightningcss itself lazily defers `var()`-bearing and custom
  properties to `TokenList` — so full fidelity means (a) a comparably deep typed catalog, ideally
  derived from W3C/MDN machine-readable property data (a genuine novelty over lightningcss's
  hand-authored variants), and (b) an explicit Unparsed/Custom deferred-token escape hatch in the
  typed model. Minify/browserslist/nesting-transform parity is named OUT of the parse mandate
  (24 §Minification). The full-tree-touch bench plane (N15) and the co-located depth-asymmetry
  disclosures make the ">SOTA" sentence honest until TW3+TW4 make it true.
- The vendored lightningcss (1.0.0-alpha.71, cssparser 0.33) is several releases stale; the TW-S
  bump re-pins the comparator before any re-measurement (24 §Versions).

**TW3's headline close condition**: the fused parse+value SINK (§2) becomes the DEFAULT
materialization path fleet-wide; every `->` reaches the emitter (`typed_materialization_invariant`
flips to built); JSON value_from_ref byte-equal; full-depth CSS oracle green. Two riders, updated
at pass 2: (a) the CRIT-P2-v2 fold-over-cursor sub-gate is DISCHARGED NEGATIVE (A1 D3, above) —
TW3 records the resolution, it does not re-bench it; the default is the sink and the lazy cursor
is its own lane; (b) TW3's per-corpus floor binds to the TW0 cold baseline (§2), and **TW3's
canada leg depends on TW4.1's Eisel-Lemire decode** — the coupling is carried in §11, not hidden
behind a parallel bar.

## §5 — The decision engine, right-sized (TW4, amended per 27)

The frontier and the repo's own sweep deliver the same verdict from opposite directions: Cranelift
ships production eqsat at average e-class size 1.13 with tree-cost greedy DP; the repo's 54-config
cost-weight sweep returned a documented null (singleton e-classes) (27 headline). And 13/14 e-graph
rewrite rules registered zero fires behind the converging normalizer (09 §C7). Therefore:

1. **The greedy tree-cost `Extractor` is the PERMANENT default engine**, not a placeholder
   (35 §1 C3). The one net-new artifact is the **measured `KernelCostTable`** —
   `(KernelId, Uarch) → cycles/64B`, populated by a checkasm/PMU harness, checked in as versioned
   data, never estimated (27 §e.1). P4 refuted the null against proxies; **P4-v2 (A2 D2) re-ran it
   against the LIVE kernels** (verbatim source-cited `classify_stripe_nibble/wide/multi` sharing
   the live `movemask_u8x16`; parity byte-exact both runs): the live crossover is **N\*=3** (moved
   from the proxy N\*≈2 — nibble-LUT 13.8/13.3 vs wide-LUT 18.2/17.2 cyc/64B), the ladder
   mis-selects only at |S|∈{1,2}, and **NO current-fleet decision flips** — JSON (N≈6-7) and CSS
   sit at N≥3. The flip band |S|≤2 is exactly the third language (CSV), so the TW6 consumer
   binding below is now measured fact, not precaution. Binding corrections, per CRIT-P4: **the
   table's consumer is the LIVE `crates/simd-scan` `KernelShape::select` ladder**
   (`alphabet.rs:103,118-124`, byte-identical `1..=8 / 9..=16 / _`) and the live
   `scan_nibble/scan_wide/scan_multi` kernels — never the tests-only `crates/ir`
   `for_singleton_count` mirror, which is retired into the same source of truth. **The table's
   flip-consumer is bound to TW6's third language** — §11 orders TW4's table landing so its first
   outcome-changing flip is exercised by a fleet grammar in the same campaign, or the table is the
   §1 pathology it exists to cure; the table lands with its TW6 consumer named in the same commit
   message (§13.5, now answered). The single
   production `perf_cost: 0` site in the retained engine (`skinny/crates/passes/src/lib.rs:590` —
   16 item 2; two further sites at `decision_csp.rs:242,254` are deleted WITH the CSP stage, per
   §5.2) is a one-line fix the table feeds. `cfg!(target_arch)` and every hardcoded threshold
   (NIBBLE_LUT_MAX=8, WIDE_LUT_MAX=16, alt_branch≥8, tiny_string_cap) demote to admissibility
   filters / table cells (27 §e.2). P4's FLIP 3 is RE-SCOPED by A2: the live
   `crates/simd-scan/src/neon.rs:690` `movemask_u8x16` ALREADY uses the cheaper `vaddv`; the
   `vshrn`/`vaddv` split lives skinny-side (`movemask.rs:4-5` vs
   `byte_class_from_eq_set_64.rs:79-84`), and the movemask spelling is genuinely
   kernel-shape-dependent (`vshrn` cheaper for the no-space kernel's crossing profile, `vaddv` for
   multi/nibble) — it is a pluggable cost-model CELL, not a TW0 global unification; the TW0 sweep
   item is withdrawn and the cell rides the KernelCostTable.
2. **The tautological CSP second stage is retired, not ported** (decision_csp solves to recover its
   input — 31 item 5; no frontier design hands an extractor's answer to a downstream CSP — 27 §d).
   One engine over `(BackendShape × KernelStrategy)`; skinny's engine and core's `csp_strategy`
   unify (27 §e.5).
3. **Escalation is gated behind a measured ablation**: treewidth-FPT → warm-start ILP → CP-SAT only
   if greedy provably differs from the optimum on the real corpus, with Y.5's connected-components
   decomposition as the separability instrument (35 N23; 27 §e.4, the anti-theatre gate). The
   re-vendored csp-solver's `node_budget` freezing guard is verified present before any escalation
   is even considered (35 N22).
4. **The dav1d chain of consequence** (25): function-pointer kernel table resolved once at init
   (rav1d measured fn-pointers beating enum dispatch in Rust — 25 §1.3); checkasm co-emitted per
   kernel, iterating EVERY registered variant (the liveness property — 25 §4) so dead kernels
   (JSON `neon::scan` behind GrowOnly, `unescape_uxxxx`, the two dead `asm!` blocks, the lying
   `ScanBackend::Scalar` label) are wired-or-deleted, never carried. Intrinsics by default; hand asm
   admitted per-kernel only with a cargo-asm/checkasm proof of a mis-schedule, missing intrinsic,
   spill failure, or an instruction-selection miss no intrinsic spelling survives. **The doors are
   PRICED (A2 D1) and the doors-first order is vindicated outright: the asm lane is OBVIATED for
   the classify kernel.** Door (a), the pure-intrinsic `vshrn_n_u16::<4>` movemask (in-file at
   `json.rs:928`), lands +34.6/36.5% over baseline and beats the hand asm by ~20-28%; door (b),
   the opaque-barrier ORR-tree coercion, lands +11.5/15.6%; the hand asm only +6.7/10.4%. The
   binding resource is established: the 8 SIMD→GPR crossings bind, not the classify ALU — the
   crossing lever (door a, 8→4 crossings) is worth ~2.6× the classify-ALU lever (door b, 40→32
   ops). P5's "one measured exception" is thereby retired (§0): an intrinsic spelling survives it
   and WINS. No TW4 asm path is needed for this kernel; the per-kernel gate remains standing law
   for future kernels, presently unexercised. The TW0-quiet item narrows to the PMU absolute
   re-capture on the pinned toolchain (ratios are settled). checkasm is right-sized to the actual
   kernel census (~4-6 kernels, one
   ISA): the per-kernel differential-parity + bench harness P5 built IS the liveness instrument;
   no dav1d-scale machinery is imported beyond it.
5. **Apple-NEON invariants live in the cost model as data** (25 §2): the `shrn`-4 movemask is
   4-bits-per-byte (nibble-domain arithmetic — the #1 porting trap); one SIMD↔GPR crossing per
   64-byte block (~12-cycle bubble each); classification LUTs ≤32 bytes (`vqtbl2q` at 2c/4-per-cyc,
   never `vqtbl4q` without proof); saturate four 128-bit pipes via independent chains, not width.
   Host-tax discipline by construction: `MaybeUninit` scratch, panic-free hot paths, bounds-elision
   shapes (rav1d's largest real gains — 25 §1.3, §5).
6. **The SOTA-gap kernels**: grammar-emittable Eisel-Lemire number decode (lexical-core is already
   in the lockfile via simd-json — near-zero integration cost, 23 §3; **this kernel is TW3's canada
   dependency, §11**); NEON bulk unescape keyed on HasEsc; tokenize-once CSS declaration bodies.
   The structural pre-scan presize is DELETED per §2/P1 — no TW4 decision remains.

## §6 — The type plane (TW5, per 28)

Honesty first: the README/ARCHITECTURE "rank-1 HM / AC-3 / bidirectional" claim is struck — what
exists is a monotone lattice fold, independently confirmed (28 §1; 34 item 18). Then the build, in
28 §6's order: (1) `MapExpr::synth` — bottom-up over the closed operator set (~80 lines), killing
`unwrap_or(Span)` at `generate.rs:371/:399` so annotations CHECK, never replace; (2) `HostSigTable`
from `@host` declarations + a first-order unifier (~150 lines) for chains and host edges —
`BBNF-HOST-SIGNATURE-MISMATCH` becomes a real unification failure; (3) `TypeDesc::Record(Row)`
(closed rows — productions never extend records) and `TypeDesc::Sum` (typed tagged unions retiring
`HeterogeneousAltJoin`/`BoxedEnum` erasure), resolved during projection; (4) rank-1 HM (Algorithm J,
union-find + Rémy levels) for generic rules — `List : ∀a. Elem(a) → Vec<a>` inferred once,
instantiated per call, un-monomorphizing the closure at the type layer; (5) the structural fold
re-homed off the csp-solver crate onto a plain worklist (the "AC-3" naming is a category error —
28 §a5); (6) a Salsa-style incremental shell, later. Algebraic subtyping (MLsub/Simple-sub) is
explicitly rejected for the core; egglog is rejected as the type engine (e-class analyses propagate
bottom-up; typing needs top-down contexts — 28 §3). Steps 1-3 are load-bearing honesty+capability;
4-6 are frontier and carry an explicit disposition: DEFERRED until a fleet grammar demands a
generic rule or a row — never carried as silent committed scope. P6's actual de-risk accounting
(CRIT-P6): **step 1 is de-risked over two grammars** (33/33 descriptors, zero Span defaults, three
negative injections fire); step 2 is validated synthetically only (the corpus has no `@host fn` and
lowering drops call args — the unifier's param edge is unreachable today); step 3 is absent from
the corpus entirely. Three binding consequences:

- **Step 0, PROVEN and scheduled**: the upstream width destruction P6 exposed (22 of 33
  descriptors land `I64` where the grammar wrote `U8`) is root-caused CORRECTLY by B3 — the
  destroyed datum is the typed numeric leaf's SOURCE SPAN (`runtime/bbnf/view.rs:141` returns
  `None` for every scalar-payload leaf, so the width suffix is unrecoverable downstream);
  `value_expr/literals.rs:7,17` is a real width-drop but on a fallback path these grammars never
  take. The fix is BUILT and verified (PROTO-B3): a 12-code-line helper in `expression/wrap.rs`
  recovering the suffix from `parent_source` (wrap.rs:260), +45/−15 in one file, zero
  grammar-named code — all 22 `Nu8` tags land `U8`, i64=0, span_default=0, checked=33/synth=0
  (inverted from P6's checked=11). TW5 re-lands the proven patch FIRST as a mechanical step;
  without it the type plane types the wrong facts. The dropped `FnCall` args remain the @host-leg
  residue (below).
- **The TW5 close gate asserts correct WIDTH, not merely zero `unwrap_or(Span)`** — P6 proved the
  Span-free condition satisfiable while two-thirds of discriminant tags land at the wrong width;
  the gate adds a per-descriptor width assertion against the grammar's written annotation.
- **HostSigTable and its `@host` consumer land in the SAME wave** (§1's law applied to ourselves):
  the `@host` declaration surface moves INTO TW5 alongside the table and unifier, with a falsifier
  grammar exercising the signature-mismatch error; the land-or-delete decision on the wider
  closure surface stays in TW6.

The inference completes BEFORE emission so neither backend's type system does inference work — the
chumsky/LALRPOP portability trap is structurally avoided and the backend-agnostic type manifest is
the Rust/TS emission interface (28 §5).

## §7 — Coordination seams (dated, outbound, and folded)

1. **parse-that LR/packrat: NO-GO, dated 2026-07-05** (37 §6, closing critic gap G3). bbnf
   eliminates left-recursion at compile time (`crates/ir/src/passes/lr.rs`, wired unconditionally at
   `pipeline.rs:95-99`); the production fleet uses the PEG repetition idiom throughout; the sole
   left-recursive grammar is a named ambiguity fixture. parse-that is released from the bbnf keep
   and may retire the WDM tier, retaining the packrat memoization core for value.js. TW0 records
   the outbound letter; the GO-conditional packrat-design fold-in drops to a note.
2. **csp-solver**: the imported P0 does NOT transfer — `ac3.rs:128` is unreachable from every bbnf
   call path (zero `Pruning::Ac3` selections; both search sites use ForwardChecking; worst-case
   blast radius is a String telemetry field — 36). It gates nothing. The 0.2.0 API break is
   pre-staged mechanically: remove `decision_csp.rs:89` `backjumping: false`, add
   `..Default::default()`, sweep three Cargo.toml pins to `"0.2"` (36 §4). The Lock-4 fix
   (egraph→csp direct dep, `crates/egraph/Cargo.toml:11`) re-vendors against csc411 upstream, never
   forks (35 N27); a diff-against-upstream checkpoint precedes it.
3. **The two-`bbnf-regex` collision (N7)**: skinny's 322-line facade collides by name with the
   canonical parse-that engine (31 item 8). TW2 deletes the skinny crate and the ad-hoc FIRST
   scanner; ONE regex home — the canonical engine via the same path patch totality uses. The
   parse-that 1.0.0 cut is TypeScript-only; the Rust tree bbnf consumes is unaffected (35 N7).
   The canonical regex crate's dependency set carries no smallbox and no `parse_that` — this
   unification does NOT re-import the nightly surface P3 found in the `parse_that` parser (§3).

## §8 — Crate topology: decomposition-first (TW1 lead, per 20)

The restart authored ~600K lines of markdown describing a 24-crate greenfield it never executed;
the live tree is 13 crates dominated by a 247,428-LOC `core` god-crate whose `grammar/` subtree
alone is 171K LOC; the doc-to-code ratio is 3.74:1 by line on a frozen codebase (20 §0, §2). The
TW manifest buried the split as a TW1 footnote — the inverse of the correct order, because TW3/TW4
otherwise land value and kernel planes INTO the god-crate (20 §2c, §4a). Amendments:

1. **A TW0 doc commit marks ARCHITECTURE.md's 24-crate design and MIGRATION.md §3-14 SUPERSEDED**,
   naming the real target: the `core` split along the already-half-designed seams — frontend/
   grammar, codegen, runtime (tape+value), kernels (bbnf-simd), passes, host (20 §5.1). Silence is
   the worst option.
2. **The split is TW1's opening, gated deliverable** — decomposition precedes the substrate
   landing, so TW1's tape and TW3/TW4's planes land into clean crates (20 §5.2). MIGRATION.md's
   per-file disposition discipline (KEEP/ABROGATE tables) is the asset the wave manifest inherits
   for the split's file-level accounting (20 §3.3).
3. **The meta-lock**: a lock whose verification command matches zero files is a failing lock
   (20 §1e). TW0 runs the five-lock falsifier audit — Lock 14 (both artifacts: the live two-crate
   test gate AND the near-vacuous LOCKS.md:349 command with 8/13 phantom paths — kept distinct per
   32 C2/31 item 12), Lock 12 (ser/gorgeous precondition never met), Locks 7/11 (crates never
   created), Lock 13 (god-directories, flagrantly violated). Until the split lands, the widened
   Lock-14 falsifier keys on directory paths inside `core`, not on crates that do not exist
   (35 §2 W6 row). The generated-file marker is unified to one string (`css_l4.rs` uses
   `//! AUTO-GENERATED`, invisible to a `// @generated`-keyed gate — 20 §1a).
4. **Husks deleted**: `bootstrap` (re-export → core), x86/wasm bodies out of `simd-scan` (952 LOC +
   92 emitter-stamped AVX2 hits — 16 item 6), the ser/gorgeous Lock-12 archive decision executed;
   the N24 dead-substrate `rg` sweep (dta.rs residue, tape::visitor family) rides the same commits.
5. **The doc/code ratio gate**: a standing cap on markdown lines added per code line landed
   (20 §5.5); research passes are capped at three per cycle (SK-V20 process surgery, carried).

## §9 — Grammar generalization, certified empirically

Lock-14 is green-by-exclusion (16 item 1) and the constraint's current status has no affirmative
owner (32 Part I). Gate-widening (§8.3) makes the negative check honest; empirical certification
makes the positive claim: **a third serious target language is required** to break the JSON/CSS
duopoly (32 G4 — every SOTA researcher targeted only JSON/CSS; the "…" in the aim is unbacked).

**The adjudicated generalization statement (CRIT-P7-adjudication, superseding both PROTO-P7 and
CRIT-P7):** the generator at HEAD is MORE general than P7 claimed and less finished than either
document located. P7's BREAK 1 is FALSE — `->` projections ARE carried and materialized for
non-JSON grammars via `general_descent` (Sheets: 31 projections, `GoogleSheetsKind` emitted);
dispatch is un-forked and structural (`SinkOnly`/`Scan` by shape-harvest; the `RuntimeEmitterKind`
fork P7 tested is nonexistent at HEAD — P7 ran against a phantom base and is disqualified as
measurement, retained as a break-census instrument). The TRUE walls are three SHARED
generator defects in `general_descent`, not per-grammar gaps — which is the good news: **TW6's TOML
is certifiable WITHOUT per-grammar emitter files.** The three fixes: (WALL A) whitespace policy
derived from the grammar's `?w` declaration, replacing the hardcoded skip-whitespace between Seq
elements (`emit.rs:126-135,272,303`) that makes newline-significant structure unrecognizable;
(WALL B) a real runtime leaf regex matcher replacing the `match_leaf` delimiter-scan approximation
(`emit.rs:157-228`, `regex_first.rs`) — RFC3339 datetimes, escaped/multiline strings, `[\s\S]*?`
all break on the approximation; (WART C) — widened by B1 — a TWO-SITE name-coupling
(`runtime_generator.rs:98` AND `json_sink_direct/mod.rs:98`) that misroutes any grammar owning a
rule named `value` onto verbatim JSON templates; its neutralization is a JSON-into-
`general_descent` FOLD plus JSON-template retirement, not a one-line sniffer deletion — nominal
residue, structurally forbidden either way. Lock-14 widens to a POSITIVE gate:
**the emitted TOML parser round-trips against the `toml` crate**, not directory-path greps.

**Pass-2 state (PROTO-B1): both walls are proven DISSOLVABLE by grammar-neutral, Lock-14-clean
primitives, runnable.** Coordinate pin: `general_descent` exists only at skinny submodule
`efc6b875f` (17 commits past `d2b32f6e3`); every §9 coordinate binds to that commit. WALL A: the
one-bit `WsClass::inline()` derivation yields 7/7; the falsifier fires on cue (the current
`all_whitespace` behavior of emit.rs:126-135 hard-misparses at byte 21, swallowing the newline
after `count=42`). WALL B: the bounded regex interpreter matches all five hard leaves full-length
(RFC3339 offset datetime, escaped basic string, non-greedy triple-quote multiline, `0xDEAD_BEEF`,
`1e10`); `bbnf-regex` has zero matcher symbols — the interpreter is the ONLY path, not a fallback.
An emitter-shaped TOML descent using ONLY those primitives round-trips **7/7 against the `toml`
crate** (scalars, escaped/multiline strings, datetimes, nested arrays, dotted keys,
std+inline+array tables, quoted keys/comments). What remains for the TW6 certification is the
in-tree REGEN wiring — thread `WsPolicy` through `lower_program`, swap the interpreter into the
`RegexProgram` arm, execute the WART-C JSON fold — a mechanical gate, not a wall.

TW6 lands TOML as a fleet member with that independent oracle (field-for-field), alongside the N4
oracle completion (CSV vs the `csv` crate first; five grammars today have zero correctness
oracle). The novelty seam this defends is confirmed still empty as of
July 2026: no system holds even the (declarative grammar × emitted block-SIMD classification) pair;
the Parabix lineage is alive but pointed at regex/GPU; XGrammar/llguidance are named and dispatched
as Axis-G constrained generation, not parsing (26 §0, §6). The claim stays narrow: "first GENERATOR
to emit grammar-derived block-wide SIMD structural classification with a typed value plane" —
never "first to vectorize parsing" (26 §7).

## §10 — The folded ledger and the precepts scorecard (normative companions)

Report 35 is the opening census of record: all six chronic items re-verified OPEN (C6 D-G reconcile
still gates everything; C1 revised per §4; C3 right-sized per §5), N1-N18 re-dispositioned, and
nine new rows N19-N27 folded (the AoS negative result, the pre-scan resurrection condition, the
Y.13 gate template, the node-budget verification, Y.5 decomposition, the reclaim sweep, the
idempotent-on-disk codegen rule — any generator writing `include!`-consumed source skips unchanged
writes or poisons cargo's fingerprint (13/35 N25) — the stack axis, and the csc411 coordination
checkpoint). Report 34 is the precepts scorecard of record: 20 of 23 directives unchanged, three
with corrected evidence that resizes work without flipping status — perf_cost is ONE production
site not ~8; inline tests are 7 core + 21+ skinny, worse than carried (TW0 relocation scoped to the
corrected count); the JSON hot-path decode is hand-rolled, so the 51/51 gate figure needs
re-narrating, not re-measuring. The pre-fix 1810 MB/s contamination magnitude was never
independently re-derived and anchors no regression budget (32 U3).

## §11 — The amended wave manifest

Order: **TW0 → TW-S → TW1a → TW1b → TW1c → TW2 → TW4.1 → {TW3 ∥ TW4.2} → TW5 → TW6**, two
couplings carried openly. The D11 coupling: TW2 may land structure-un-forked with hot leaves
behind static dispatch; TW4 replaces that dispatch with the measured table; the cutover-gate runs
at TW2 close and again at TW4 close. **The TW3↔TW4 number-decode coupling (CRIT-spec-architecture
B2): TW3's canada close leg requires TW4's Eisel-Lemire decode kernel — so TW4 is split: TW4.1
(Eisel-Lemire + the KernelCostTable substrate) lands BEFORE TW3 closes; TW4.2 (the remaining
kernel plane + dispatch-table replacement) may run parallel to TW3.** The parallel bar binds only
where no dependency crosses it.

- **TW0 — Gates, hygiene, honesty, truth-floor.** **First commit — the D-G reconcile (C6),
  defined so it is executable:** a doc-only commit that places the SK-V16/V17 REDRESS pre-block
  reconcile onto the committed ledger. Concretely: REDRESS 51/53/246/247 are entries in the
  SK-V16 pre-block route families (SK-V16 SYNTHESIS §0.4; SK-V19 SPEC D-G, CH3-V1-R2) — 51/53 from
  the 50-55 retained-shortcut family (53 is the canonical instance: a structural index retained as
  a parallel vector alongside the ONE tape; 51 its retained-parse-shortcut sibling) and 246/247
  from the 242-247 fixed-shape/Unicode-retry family (one-quartet Unicode/object-pair promotion and
  fixed-shape retry under old framing). The commit contains: (a) a DEFERRED-LEDGER entry naming
  the four ids AND their carried family meanings, so no substrate wave can reopen them by renaming;
  (b) the skinny freeze tag `sk-v19-frozen` — skinny becomes architecture-read-only, retained
  solely as differential oracle + checkasm/microbench harness; (c) the supersession pointer to
  this SPEC as the governing manifest. Zero behavior LOC; it unblocks every TW wave. Then: the
  24-crate supersession doc commit (§8.1). The five-lock falsifier audit + meta-lock (§8.3). The
  Y.13 consumer-invariant gate template adopted as standing wave-close law (§1). Lock-15 +
  target-cpu (§3.3; TW0 is the SOLE owner — TW-S inherits, never re-lands). Lock-4 fix with the
  csc411 re-vendor checkpoint (§7.2). W5d x86/wasm deletion. Inline-test relocation (7+21
  corrected, including the four `#[cfg(test)]` perf_cost sites CRIT-spec-architecture R7 flags).
  MASTER-PLAN §13.6 reconcile (16 item 9). Oracle hygiene (N5, W3C rekey). (The P4 FLIP-3 movemask
  item is WITHDRAWN from this sweep — A2 shows the live path already uses `vaddv` and the spelling
  is a kernel-shape-dependent cost-model cell, §5.1.) **The truth-floor bench session, narrowed by
  pass 2 to the QUIET leg only**: the digest-parity assert is folded and passing, the canada sign
  is settled, the P1 interleaved CI re-run has run, and the P5 doors are priced (A1/A2) — all at
  loadavg 8-16, ratios authoritative. TW0 executes the loadavg < 2 re-certification of those
  directional absolutes: cold re-capture of CSS + JSON both sides on `json_canon_bench`; the P5
  PMU absolute re-capture on the pinned toolchain (§5.4); the A3 nightly-vs-stable channel ratio
  un-confounded (§3); interactive-samply flame; per-corpus-shape reporting; the full-tree-touch
  plane (N15). No pass-2 DECISION reopens at TW0 — the quiet session certifies magnitudes. The
  parse-that NO-GO letter recorded (§7.1). Playground junk ignored (16 tail). *Close gate:*
  locks/tests green; the quiet baseline ledgered — no later wave's no-regression clause is
  meaningful without it (20 §4c).
- **TW-S — Stack abrogation** (§3), priced per A3 into sub-waves TW-S.1-5 (TW-S.2 is the
  criterion-ingestion re-path of gate.rs/report.rs — 60+ references — preceding the criterion
  deletion). *Close gate:* skinny + surviving crates build on pinned
  stable, edition 2024 everywhere (scope per §3); cold bench parity vs the TW0 baseline. (The
  smallbox transitive check is DISCHARGED at pass 2 — A3 D2, count=0.)
- **TW1a — The topology split** (§8.2), scope-bound per 38 amendment A1: the split partitions the
  **39,931 hand-written LOC** across the six seams; the 176,887-LOC generated tree relocates as a
  bulk `git mv` payload per grammar, never hand-curated. Structural-lock gate class — no
  performance clause; a pure move must not move a number. *Close gate:* the split's crate
  boundaries enforced by the
  widened Lock-14 keyed on the NEW crate directories; **9-grammar regen byte-IDENTICAL post-move**
  (the relocation-only proof); build green both workspaces; zero behavior change
  (differential oracle green).
- **TW1b — The one-tape AoS substrate** (§4). The AoS-hardened ONE tape + the eager-builder
  retirement (~1,080 hits, 95.1% regen-owned — true hand surface ~40-60 template/type sites, 38
  A2) in one fenced parity-gated commit — the substrate and its consumer
  together, per §1. Gate relaxation, carried openly (38 §4 / CRIT NEW-R7): TW1b RESTORES the AU
  AoS floor, so its gate is liveness-AND-no-regression, not the §1 ≥3%-win clause — the win is
  recovering ground SoA lost. *Close gate:* JSON+Sheets oracles field-for-field; Lock 1 green,
  falsified two ways (grep-zero `TapeStructBuilder` AND a runtime assertion that exactly one owner
  stamps `frame_depth`); no cold regression vs the TW0 baseline.
- **TW1c — Mechanical tail.** StructLayout→Layout rename (960 hits, 88.5% regen-owned); the N24
  reclaim sweep. The §2 presize deletion (`Sampled`/`Exact`/`OneShotSimd`, plus the dead
  `attach_structural_index` no-op) is re-homed to a `crates/simd-scan`/`crates/ir`-scoped gate —
  the family has zero `core` occurrences (38 §1). *Close gate:* regen byte-clean vs the new
  golden; grep-zero on the retired names; no cold regression.
- **TW2 — The one generator.** As SK-V20 wrote it (BackendExpr → THE IR; real Emitter trait; xtask
  un-fork; couriers derived-or-owned; structural-predicate dispatch; grammar-derived alphabets),
  plus: ONE regex home (§7.3); the css_l4 108K-line regression trace attacking line count itself —
  the B2 lesson: relocation is not reduction (35 N6); `generated_loc_budget` enforced in
  `regen --check`; the idempotent-on-disk rule asserted (N25). *Close gate:* ONE emit path serves
  all 9 grammars; cutover-gate green for the slice; budgets met; widened Lock-14 green over codegen.
- **TW4.1 — Eisel-Lemire + the cost-table substrate** (§5). The Eisel-Lemire/SWAR number-decode
  kernel (TW3's canada dependency — lands FIRST); the `KernelCostTable` bound to the LIVE
  `crates/simd-scan::KernelShape::select` ladder and kernels (§5.1); the tests-only `crates/ir`
  selector retired into it. *Close gate:* canada eager cold beats sonic on `json_canon_bench`
  (the float-tax signal closed); the table feeds the live selector with a `cargo expand` witness.
- **TW3 — The value plane** (§4; opens after TW4.1). The fused single-pass SINK as the default
  eager lane; the lazy ValueRef/cursor over the ONE tape as its own lane (the dual-path shape,
  matched knowingly — §4); one path alphabet across both; serde bridge; CSS depth closure
  with the derived-catalog + Unparsed escape hatch; StructRegistry projection fence;
  projection-totality invariant (`PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers`
  — 09 §D4). *Close gate:* every `->` reaches the emitter; the fused SINK IS the default with no
  cold regression vs the TW0 baseline on any corpus, per-corpus reported (twitter/citm/canada
  each named — no geomean); the fold-over-lazy-cursor sub-gate RECORDED as resolved-negative
  (A1 D3 — no re-bench; any future one-substrate revival requires a fresh measurement that names
  why it differs); full-depth CSS oracle green; the AU-relative
  cold capture ledgered as context; asymmetries co-located.
- **TW4.2 — The decision engine + kernel plane** (§5; parallel to TW3). One engine; ablation gate
  before any escalation; per-kernel differential parity + bench (checkasm right-sized, §5.4) over
  the whole dispatch table; NEON bulk unescape + CSS tokenize-once; scanner unification (C4/C5:
  lo/hi nibble two-LUT, `vshrn` single-pack, Lock-16 co-gate); the asm door only per §5.4's
  doors-first order. *Close gate:* cutover-gate green (second leg); Lock-14 re-proven full-system;
  the compound >SOTA sentence true unqualified on the gated planes — cold, quiet, live SIMD on
  the JSON path. The cost table's fleet-exercising flip is certified at TW6 when the third
  language lands (§5.1's consumer binding).
- **TW5 — Type resolution** (§6). Step 0 first — re-land the PROVEN B3 width patch (wrap.rs
  helper, +45/−15; view.rs:141 span recovery) plus the FnCall-args preservation;
  then steps 1-3 with the `@host` declaration surface in-wave (§6). *Close gate:* 9 grammars type
  end-to-end with zero `unwrap_or(Span)` AND correct width per the grammar's annotation (the P6
  falsifier); the host-signature gate fires on a falsifier grammar; annotations check.
- **TW6 — Language + fleet completeness + the third language.** MapExpr byte-fold primitives;
  `type_annotation` extension; `@assoc`; land-or-delete the remaining closure surface; oracle
  completion (N4/N13); **TOML landed as fleet member ten via the three SHARED `general_descent`
  fixes (§9: ?w-derived whitespace policy, real leaf regex interpreter, the two-site WART-C JSON
  fold) — no per-grammar emitter files; both walls are pass-2-proven dissolvable (B1, 7/7
  round-trip), so the TW6 work is the in-tree regen wiring: WsPolicy through `lower_program`, the
  interpreter into the `RegexProgram` arm, the JSON fold at submodule `efc6b875f`**; the
  KernelCostTable's single-delimiter flip exercised live (CSV, |S|≤2 — the measured flip band,
  §5.1).
  *Close gate:* the emitted TOML parser round-trips the `toml` crate field-for-field (the widened
  positive Lock-14); zero grammars without an oracle; zero live host hatches not expressible as
  grammar; the generalization census names ten grammars, three of them serious targets.

## §12 — Prototype dispositions (pass-1 closed; instruments retained)

All seven ran; their verdicts are folded above. P1 (presize): DELETE decided, §2 — instrument
retired, magnitude certification rides TW0. P2 (fused lane): core thesis cold-corroborated, canada
refuted, fold-over-cursor then-open (closed NEGATIVE at pass 2) — §2/§4/TW3. P3 (stack):
skinny-on-stable settled; scope
decision taken in §3; bench leg deferred to TW-S close. P4 (cost table): null refuted; consumer
re-bound to the live selector and TW6's third language — §5.1. P5 (asm): instrument sound;
decision deferred behind the doors-first order (resolved at pass 2: doors priced, asm obviated) —
§5.4; the checkasm-shaped parity harness is a
TW4 asset. P6 (HM spike): step 1 de-risked; step 0 lowering fix and the width gate scheduled —
§6. P7 (TOML): disqualified as measurement (phantom fork), superseded by the adjudication — §9;
the break census (walls A/B, wart C) is the TW6 work-list. The `json_canon_bench` harness and the
P5 parity harness are the two instruments that graduate to `crates/`.

**Pass-2 dispositions (all five instruments ran; verdicts folded above):** A1 (bench
adjudication): canada sign SETTLED, fold-over-cursor REFUTED, presize CIs certified at ratio
level, AU-relative capture taken — §2/§4/TW3; the rebuilt `json_canon_bench` (parity assert +
fold-cursor lane) is the TW0 instrument of record. A2 (kernel PMU): doors priced, asm lane
OBVIATED, binding resource established (crossings, ~2.6× the ALU lever); P4-v2 live crossover
N\*=3, no current-fleet flip — §0/§5. A3 (TW-S pricing): migration applied (5 edits), divan port
proven both channels, smallbox check CLOSED (falsifying PROTO-P3's coupling), dep table priced
into TW-S.1-5; the channel ratio is load-confounded and rides TW0 — §3. B1 (TOML-real): both
walls dissolved by generic primitives, 7/7 toml-crate round-trip, WART C widened to a two-site
fold; the in-tree regen wiring is the TW6 gate — §9. B3 (width fix): built and verified, 22/22
Nu8→U8; re-lands as TW5 step 0 — §6. research/38 (seam disposition): split-first survives;
amendments A1/A2 bound into TW1a/b/c — §4/§8/§11.

## §13 — Open questions, re-stated at pass-2 close

Pass-1 questions 1, 2, 3, 5, and 6 are CLOSED: (1) the asm door — both doors priced, door (a)
obviates the lane, zero qualifying kernels (§5.4); (2) the fold-over-cursor — benched NEGATIVE,
default is the fused sink (§4); (3) the TW1a/TW1b seam — split-first adjudicated sound
(research/38, §11); (5) the live-selector rebind — no current-fleet flip, the table lands with
its TW6 CSV consumer named in the same commit message (§5.1); (6) the smallbox transitive check —
discharged clean (§3). What remains:

1. The severity-weight of CSS SIMD tokenization (24 §SIMD: a confirmed-by-absence open frontier
   with real comment/url() correctness risk) — deliberately NOT scheduled in TW4; it enters only
   via a costed per-grammar decision once the TW0 measurement loop is live. (Carried unchanged —
   a scheduled decision point, not a design unknown.)
2. TW0's within-wave breadth (~14 workstreams; CRIT R3, twice flagged) — tolerated as a
   gate/hygiene wave with a clean first-commit anchor; the implementation tranche sequences TW0
   internally (D-G reconcile → supersession docs → lock audit → Lock-15 → truth-floor session)
   rather than re-lettering.

Everything else outstanding is an EXECUTION deliverable owned by a named wave, not an open
design question: the TW0 quiet (loadavg<2) re-certification of the pass-2 directional absolutes
(including the un-confounded channel ratio); TW-S's cold parity measurement; TW5's step-0
re-land; TW6's regen wiring + the CSV flip exercise.
