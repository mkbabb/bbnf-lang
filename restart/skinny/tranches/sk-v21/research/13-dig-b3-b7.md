# Archaeological Dig — B3 through B7 (the late-campaign mud)

Excavation of `docs/tranches/B3` … `docs/tranches/B7`, the final five tranches
of the original bbnf-lang campaign before the restart. These five are not the
divergence's origin — they are its terminal symptom: a run of five consecutive
tranches in which not a single one advances the stated goal (best every SOTA
structured-data parser). Every one is firefighting the substrate the campaign
built for itself. B3 chases a parser that stopped returning; B4 fixes codegen
that emits invalid Rust; B5 spends eight waves un-welding and de-god-moduling
the substrate; B6 discovers its own dev loop was measured against fabricated
baselines; B7 modernizes build tooling across three repos. The corpus is a
textbook local minimum: enormous procedural ceremony orbiting maintenance of
accumulated debt, with the frontier never touched.

The physical scale of the divergence is measurable. The nine per-grammar
generated backend files total **169,956 lines** (`crates/core/src/grammar/generated/`):
`css_l4.rs` alone is **108,406 lines**, `bbnf.rs` 21,557, `google_sheets.rs`
13,861. Nine hardcoded grammars, nine registry JSONs, ~170K lines of generated
Rust the campaign regenerated and maintained on every change — this is precisely
the "hardcoded grammars, per-grammar backend files" the restart charter names as
the failure mode.

---

## (a) What each tranche attempted

**B3 — "AY-II.W0' Revert + Parser-Baseline Restoration."** Thesis: a cluster of
recent commits ("W0'" — the FusedBuilder collapse, materializer re-routing,
scan-policy splice) caused `BbnfBootstrap::parse` to hang (>5 min on bbnf, >39 s
on the 537-byte json grammar). Two waves. W0 was a *proof-first* protocol:
apply 14 candidate reverts on a disposable worktree, measure the parse-phase
wall, and only land canonical reverts on master if the probe confirmed the
thesis. Escape clauses widened the revert scope to W0-fix, then W0-base, if the
narrow revert disproved. (`B3/B3.md:1-11,186-192`; rationale in
`B3/audit/W0p-revert-rationale.md`.)

**B4 — "Codegen-Emission Correctness + W0' Re-land Hardening."** The bbnf
self-host regen reached codegen then failed `syn::parse2` with "expected loop or
block expression." W0 traces the offending emitter and fixes at source; W1 does
consumer-fixture polish (the `FusedOutput<R>` migration close). (`B4/B4.md:1-16`.)

**B5 — "Gestalt Substrate Restoration."** Six (grew to eight) waves of pure
cleanup. Five named smells: (1) the welded `FusedBuilder` two-substrate — delete
it, promote value-side state into `Columns`; (2) rollback triplication → one
primitive; (3) the `columns_mut()` boundary leak (1061 callsites in CSS L4
alone) → a `Tape::position()` accessor; (4) `extern crate self as bbnf` →
removed; (5) six god modules (>1200 LOC each) → decomposed. Net-negative LOC.
(`B5/B5.md:1-60`.)

**B6 — "Dev-Loop Expedite."** A bounded prelude annex. Three waves to remove
build/bench/test drag: halve the bootstrap regen wall (W0), carve an
`iter-check-az` iteration alias (W1), partition the JSON test surface (W2).
(`B6/B6.md:1-48`.)

**B7 — "Cross-Repo Modernization Annex."** Ten agents, three repo-isolated
waves: consolidate every bench onto divan and every CI onto nextest across
`bbnf-lang`, `parse-that`, and `pprint`, and collapse the `bbnf-ser` patch to a
single source of truth. (`B7/B7.md:1-16`.)

---

## (b) Shipped vs died on the vine (git-cross-checked)

Every one of the five *closed*, and every FINAL.md is honest about what actually
happened — often at odds with what the plan predicted. Git confirms the commit
trails.

- **B3 shipped the OPPOSITE of its plan.** Zero of the 14 planned reverts landed.
  The git trail is a sequence of disproofs: `ef52c35c` "probe #1 DISPROVED",
  `c325c13f` "probe #2 INDETERMINATE", `e5f77689` "probe #3 DISPROVED-PARSE —
  hang INSIDE BbnfBootstrap::parse (relinquish to user)", `a5dfdf02` "probe #4
  DISPROVED-GENERIC — hangs on json (537B) too", then `eec1731b` "β samply
  localization", then `9167dac4` the actual fix. What shipped was five
  architectural fixes to the tape finaliser, none of them a revert
  (`B3/FINAL.md:10-63`). **The entire planned thesis was false.**

- **B4 shipped, but W1 uncovered 327 hidden failures.** W0's actual fix was a
  one-shape correction (a SIMD kernel emitting `break 'label Some(...)`;
  `B4/FINAL.md:13-22`). W1, billed as "consumer-fixture polish," instead traced
  **327 workspace failures** all signed `FusedBuilder::finish called with N open
  value frames remaining` — the rollback path truncated the tape column but never
  unwound the paired value substrate. Test gate climbed 1163 → 1480 on one
  landing (`B4/FINAL.md:24-50,88-94`). Commits `a5fdd...`, `bb52d...` on master.

- **B5 shipped fully — eight waves, honest.** W2 hit a substrate barrier; the
  W2b audit re-routed it (see (c)); W6/W6b landed the correct fix. Workspace
  1477/1477 green, `compile_bbnf` 2.806 ms (−0.9% vs B4). Git shows the wave
  authoring + the `12f4265c` W2b diagnosis commit (`B5/FINAL.md:356-364`).

- **B6 shipped ONE source commit; two waves closed vacuous.** Only W0's 35-LOC
  content-equality skip touched source (`5967d37b`); W1 (`f685a9a6`) and W2
  (`b75ff57c`) are docs-only "plan-time miscalibration" closes. Git confirms: no
  source commits for W1/W2. The one real win was a **192× speedup** (88.26 s →
  0.46 s) at a bottleneck the plan never named (`B6/FINAL.md:1-13,99-130`).

- **B7 shipped 20 commits across three repos in ~15 min wall** (`B7/FINAL.md:1-11`).
  Warm nextest wall 22.353 s → 10.832 s. Two items forwarded, not closed: the
  parse-that bootstrap crate's dead `bbnf_derive 0.2` dependency, and pprint's
  `-Zthreads=8 -Zshare-generics=y` rustflags measured as a **28× warm iter-check
  regression** left in place as "future cross-repo refinement"
  (`B7/FINAL.md:247-265`).

---

## (c) Root causes, local minima, bottlenecks — with evidence

**1. The proc-macro cold-wall accretion is the taproot of B3–B6.** The four
2026-04-25 deep audits attributed an >80-min cold `iter-check-full` wall to the
IR pipeline running inside rustc's expand phase, and prescribed retiring the
proc-macro for a build-time `xtask regen` step (`B3/audit/W0p-revert-rationale.md:13-27`).
That retirement is what forced 170K lines of on-disk generated code, which in
turn produced B6's self-invalidation cycle. The whole B-series is downstream of
one architectural decision to move a slow pipeline out of the compiler rather
than to make the pipeline fast.

**2. B3: a latent pre-order/post-order contract bug, mis-diagnosed as a
migration regression.** The real cause (`B3/PROGRESS.md:421-513`, fix `9167dac4`):
`tape::finaliser::derive_frame_depth` did a post-hoc reverse-walk deriving each
record's `frame_depth` from the `child_off` graph, presupposing canonical
post-order (`child_off < self_idx`). Pratt "Option C inline" emission introduced
*pre-order* compounds (`child_off = open_offset + 1`). When a pre-order compound
sat as a direct child of a post-order parent (BBNF's `value_path`), the leap
`pos = child_off` went *upward*, and `while pos > start` spun forever — samply
credited **93.1% of runtime to two adjacent PCs in a 28-byte machine-code window**
(`B3/PROGRESS.md:395-414`). The two offending commits (`f603f549`
`derive_frame_depth`, `49d468f2` Pratt pre-order) landed **the same day, without
each other's awareness** (`B3/PROGRESS.md:532-537`). The local minimum: the plan
committed to a revert hypothesis and burned four disposable-worktree probes plus
a user relinquish before anyone ran a profiler. The memory feedback
`actual-profiling` ("run the actual profiler, don't guess from static analysis")
is this episode's scar tissue.

**3. B4: speculative-parse rollback that unwinds one substrate, not both.** The
327-failure signature is a two-substrate weld leaking under retry: shape
emitters wrapped retries in IIFE closures that could open compounds before
failing; `columns_mut().rollback_to(X)` truncated only the tape column family,
leaving `value_open_stack`/`value_frames`/`value_payloads_*` opens dangling
(`B4/FINAL.md:24-45`). The `FusedBuilder::finish` open-frames assertion is the
canary for the entire fused-substrate design being fragile under backtracking.

**4. B5: the substrate was two things pretending to be one.** `FusedBuilder`
owned a `Tape<R>` *plus* four value-side fields; the naming "admitted the weld"
(`B5/B5.md:22-27`). The weld required `extern crate self as bbnf` mediation
(`B5/B5.md:41-44`). Three rollback methods each claimed to be "the canonical"
(`B5/FINAL.md:38-52`). The `columns_mut()` escape hatch had **1061 callsites in
CSS L4 alone** and had forced `frame_depth`/`current_depth` to migrate into
`Columns` as a leak-consequence (`B5/B5.md:37-40`). God modules: `expression.rs`
2140 LOC, `dispatcher.rs` 1969, `inline.rs` 1687, `flat.rs` 1342, `columns.rs`
1287, `emitter/grammar.rs` 1286 (`B5/FINAL.md:91-97`). W3+W3b split **thirteen**
files. This is the concrete shape of "god modules in gestation" the memory
feedback warns against — accreted over a campaign, retired in one tranche.

**5. B5.W2b: the plan's prescribed fix composed badly with the substrate.** The
sharpest single audit in the corpus (`B5/audit/W2b-architecture-diagnosis.md`).
The α audit correctly named the post-order depth cascade as "bookkeeping
correcting bookkeeping" but the wave plan prescribed adding
`ValueCheckpoint::first_child_off`, which fails two ways: post-order shapes have
no checkpoint for the closing compound at child-push time (Wrap), and an offset
field cannot be saturating-decremented on rollback the way a count can
(`W2b:82-126`). The correct fix *inverts the depth-stamp invariant*: bump
`current_depth` *before* the body emits (`enter_post_order_children`), so every
`frame_depth` byte is written exactly once at push time — a **single-writer
invariant** — deleting the cascade, the forward scan, and
`leftmost_descendant_offset` entirely (`W2b:128-184`). The lesson: wave plans
authored a level above the substrate prescribe the wrong refactor target;
contact reveals the architecture.

**6. B6: numeric gates authored against fabricated baselines.** The purest
process failure in the corpus. W1's plan asserted a 660 s `iter-check-full`
baseline; direct measurement was **17.02 s — stale by 38.8×** (`B6/PROGRESS.md:333-334`).
Worse, the prescribed lever (egraph lazy-pass deferral) was *structurally
incapable* of moving the metric: it defers runtime saturation work that a
typecheck-only wall never executes (`B6/FINAL.md:53-63`). W2 was the same shape:
the JSON bench partition operated on `[[bench]]` targets that `nextest` does not
even run (`B6/FINAL.md:68-92`). The actual W0 win — a **self-invalidation
fingerprint cycle** where `regen` unconditionally `fs::write`s, advancing the
generated file's mtime, so cargo rebuilds the `bbnf` crate (~85 s) on the *next*
regen — was found only by measuring, not planning (`B6/FINAL.md:26-44`). "The act
of regen guaranteed the next regen paid the rebuild cost." A 35-LOC
content-equality skip cured it.

---

## (d) Worth resurrecting for the totality plan

**D1 — The single-writer `frame_depth` invariant (B5.W2b + B3 fix) is
foundational design, not incidental cleanup.** Any tape/CST substrate that emits
compounds in both pre-order and post-order MUST stamp structural depth once at
push time via an enter/exit-children bracket discipline
(`enter_post_order_children` saves+bumps `current_depth`; `end_compound_post`
stamps at the saved outer depth; a matching `exit` restores on retry failure).
The post-hoc reverse-walk (`derive_frame_depth`) is a proven infinite-loop trap
and must never be reintroduced. Worked example and full API in
`B5/audit/W2b-architecture-diagnosis.md:161-184`. Adopt this from day one so the
totality substrate never rediscovers it.

**D2 — Atomic multi-column rollback contract (B4.W1).** Speculative-parse retry
must roll every substrate column back atomically — structural AND value — keyed
on the open offset, never truncating one family. Keep a finish-time open-frames
assertion as the canary. `B4/FINAL.md:38-50`.

**D3 — Idempotent-on-disk codegen (B6.W0) as a hard rule.** Any generator that
writes source files consumed via `include!` must skip the write when bytes are
unchanged; otherwise it poisons cargo's fingerprint and self-inflicts a rebuild
cascade. This is a 35-LOC pattern with a 192× payoff and belongs in the
generator's contract, not rediscovered per campaign. `B6/FINAL.md:26-44`.

**D4 — Negative result: do not build tranches on unverified root-cause
hypotheses.** B3 is the canonical anti-pattern — a 347-line rationale, R1/R2/R3
alternatives, escape clauses, and a "probabilistic thesis status" section
(`B3/audit/W0p-revert-rationale.md:282-327`), all orbiting a guess that four
probes falsified. The cheap, decisive instrument (samply) should run *first* for
any performance-shaped problem. Mandate profile-before-plan.

**D5 — Negative result: never author a numeric gate against an inherited or
estimated baseline (B6).** Measure the current substrate before writing a
threshold. Two of B6's three waves closed vacuous because their baselines were
38.8× and ~2.4× stale and their levers targeted the wrong subsystem. Any
"expedite X" tranche must open with a Phase-0 measurement, not an asserted
baseline.

**D6 — Negative results to not re-learn (cheap facts):** `break 'label
<path-expr>` is rejected by `syn` — codegen must wrap break-values in braces
(`B4/FINAL.md:13-22`). Bench-class and fuzz-class tests must not sit on the
routine `[[test]]`/nextest surface — route them to `[[bench]]` and sample-cap
proptest fuzzers (`B6/FINAL.md:68-92`, `B7/FINAL.md:26-42`). pprint's
`-Zthreads=8 -Zshare-generics=y` rustflags are a **28× warm iter-check
regression** — do not inherit them (`B7/FINAL.md:261-265`).

**D7 — divan-everywhere / nextest-everywhere / single-source patch graph (B7)
already landed** and should be preserved as the fleet-wide build discipline
rather than re-derived. `B7/FINAL.md:300-318`.

---

## (e) Meta-process pathologies

**P1 — Ceremony-to-work inversion.** Every tranche carries B*.md + FINAL.md +
PROGRESS.md + AGENT_DISPATCH.md + waves/*.md + audit/*.md, each with
"Invariants," "Hard gates summary," "Hard-gate measurement methodology," "Floor
checks," "Defensible floor," "Escape clause," and "Indefatigability" sections.
B6's W1 and W2 produced **zero source lines** and hundreds of lines of
close-ceremony prose apiece. The documentation mass vastly exceeds the code
delta; the process optimizes for defensible-sounding closes over shipped
capability. The near-identical boilerplate ("no stretch goals, no if-time-permits
carve-outs… the triumvirate stands by… the response is to widen the synthesis,
not to retreat") appears verbatim in B5, B6, and B7 — ritual, not thought.

**P2 — Plan-as-prophecy, disproved-under-contact.** In three of five tranches the
plan's central claim was wrong and the tranche succeeded by *abandoning* it: B3's
revert thesis (fully disproved), B5.W2's prescribed refactor target (re-routed by
W2b), B6's W1/W2 levers (structurally incapable). The tranche machinery is good
at recovering — the FINALs are refreshingly honest — but the up-front planning
repeatedly manufactures elaborate structure around unverified premises. The
honesty of the closes (B6 naming its own baselines "stale by 38.8×") is the
corpus's redeeming trait and should be preserved; the fictional up-front
precision should not.

**P3 — Yak-shaving lock-in / goal displacement.** Five consecutive tranches, not
one of which touches the actual mission (beat SOTA parsers). B3 restores a
parser to *not hang*; B4 makes codegen *emit valid Rust*; B5 *un-welds its own
substrate*; B6 *unbreaks its own dev loop*; B7 *modernizes build tooling*. Each
is legitimate maintenance, but the aggregate is a campaign servicing its own
accumulated complexity (170K generated LOC, a fused two-substrate, six god
modules, a self-invalidating regen). This is the local minimum the restart
exists to escape: the substrate grew heavy enough that all energy went to
holding it up. The parity/throughput benches that would measure the *mission*
are explicitly deferred every time ("peer comparisons become load-bearing at
AY-II.W1+"; `B5/FINAL.md:258-261`) — always next tranche, never this one.

**P4 — Substrate cleverness ahead of substrate correctness.** The recurring
technical villain across B3/B4/B5 is one idea: fuse the structural tape and the
value/materialization columns into a single builder for speed, then mediate the
weld with a self-alias, a reverse-walk depth derivation, and a partial rollback.
Each optimization spawned a bug class (infinite loop, open-frame leak, boundary
leak) that a later tranche paid to unwind. The KISS lesson: get the substrate's
invariants single-writer and its rollback atomic *before* fusing for
performance.

---

## Load-bearing takeaways for the totality plan

The B3–B7 corpus is the divergence's terminal state made legible. Its concrete
gifts are three reusable design invariants (single-writer depth, atomic
multi-column rollback, idempotent-on-disk codegen) and five negative results
(don't plan on unverified hypotheses; don't gate on unmeasured baselines; profile
first; don't fuse before correctness; specific codegen/test-routing/rustflags
traps). Its concrete warnings are the process pathologies: ceremony-to-work
inversion, plan-as-prophecy, and a five-tranche goal-displacement spiral that
never once ran the mission's own benchmark. The totality plan should carry the
invariants forward, hard-wire profile-and-measure-before-plan, and — above all —
keep the SOTA parity/throughput bench as a *standing* gate every tranche must
touch, so the campaign can never again spend five tranches maintaining its own
scaffolding while the frontier goes unmeasured.
