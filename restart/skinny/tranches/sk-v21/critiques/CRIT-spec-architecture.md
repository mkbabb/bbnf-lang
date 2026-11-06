# CRIT — SPEC-DRAFT-P1 architecture coherence & right-sizing

Target: `restart/skinny/tranches/sk-v21/SPEC-DRAFT-P1.md` (382 lines), read end-to-end. Spot-checks
grounded against `research/35-deferred-fold.md`, `skinny/crates/passes/src/lib.rs`,
`skinny/crates/passes/src/decision_csp.rs`. This critique judges the whole: wave-graph right-sizing,
consumer-invariant gating, crate-topology reality, seam dating, and substrate-first recurrence.

## What is settled and strong (credited, so the score is honest)

The draft does real pruning, which is the opposite of the corpus pathology. It retires the
tautological CSP second stage (§5.2), keeps greedy `Extractor` as the permanent engine not a
placeholder (§5.1), rejects e-graph-as-type-engine and algebraic subtyping (§6), holds AoS over the
measured-losing SoA pivot (§4, corroborated `35:83` N19), keeps `core::arch` over `std::simd`
(§3.5), and costs the pre-scan per-grammar rather than universalising it (§2). The parse-that NO-GO
is dated 2026-07-05 (§7.1). The Y.13 consumer-invariant gate is elevated to standing wave-close law
(§1, `35:85` N21). Performance is disclosed DIRECTIONAL until the TW0 quiet recapture (lines 13, 87).
The 24-crate design is explicitly superseded and a real 6-way `core` split named (§8.1). This is a
coherent, evidence-anchored draft — not a mess. The findings below are what keeps it from plan-ready.

## Blocking — must resolve before the plan freezes

**B1. The TW1 topology-split ordering is deferred to pass-2, and it is the axis that decides whether
substrate-first recurs.** §13.3 self-flags: "Whether the TW1 topology-split-first ordering survives
contact… the split may reveal that tape must move first; pass-2 critique adjudicates." §8.2 asserts
split-first as gospel ("decomposition precedes the substrate landing"), but the crate-topology
decision is only real in WHAT (the six seams), not in WHEN. Splitting a 247,428-LOC god-crate into
six crates AND landing the AoS one-tape AND retiring ~1,080 eager-builder hits AND a 960-hit rename
in one wave (§11 TW1) is not a right-sized wave; it is four waves wearing one letter. The ordering
that is deferred is exactly the one that governs pathology recurrence.

**B2. {TW3 ∥ TW4} parallelism hides a number-decode dependency; the parallel split may be false.**
TW3's headline close gate is "the fused lane IS the default and meets the AU-relative floor cold"
(line 348). But §2 (lines 88-94) states number materialization is ~30% of the typed figure and the
legitimate target is Eisel-Lemire/SWAR decode — which lands in TW4 (§5.6, line 208). TW3 cannot meet
an AU-relative materialization floor without TW4's decode kernel. Only the TW2↔TW4 dispatch coupling
is carried (D11, lines 316-318); the TW3↔TW4 number-decode coupling is unnamed. The parallel is
likely a hidden sequence.

**B3. Two performance floors are in tension, one of unknown thermal provenance.** §2 anchors the JSON
floor as "AU-relative" (line 72: twitter 1967 MB/s, 0.615 cyc/byte) while §2 also concedes JSON has
NO cold harness at all and the committed figure carries a ~3× warm gap (lines 95-98). The AU peak was
measured under that pre-cold regime. TW3/TW1 close gates say "no cold regression vs the TW0 baseline"
(good) yet also "AU-relative floor cold" (line 348) — a near-oxymoron if AU was warm. The spec must
pick the TW0 cold baseline as the sole governing floor and demote AU to directional, or the
no-regression gate is unfalsifiable.

**B4. The plan's highest-ROI lever rests on an unfalsified number.** §2 names fused parse+value as
"the highest-ROI unfinished lever" (line 78); the 0.99× geomean was measured under the old harness
(§13 Q2). TW3 commits "fused lane IS the default" as a close condition (line 348) before P2 confirms
0.99× survives cold. If P2 falsifies it, TW3's headline is unachievable and the wave re-plans. This
is disclosed but unresolved — the central bet is not yet load-bearing.

**B5. The first commit "gates everything" and is never explained.** The D-G reconcile (C6) is "the
single fact gating the entire SK-V21 opening" (`35:109`; spec line 301, 320) and the literal first
commit — yet the spec nowhere states what D-G IS or what reconciling it entails. A reader cannot
execute the opening act. An opaque, everything-gating first commit caps convergence by itself.

## Refinements — resize, do not re-found

**R1. The §1.1 law is overstated relative to the waves it binds.** "Every wave closes on a
consumer-invariant runtime gate in `crates/` — never on a document, never on a source grep" (line 45)
is not uniformly satisfiable: TW0 closes on "locks/tests green; the quiet baseline ledgered" (line
329), TW-S on "both workspaces build… cold bench parity" (line 331), TW5 on a host-signature-gate
firing. None of those is an emitted-decision-surface consumer. Either scope the law to
substrate/kernel/value waves (TW1-TW4) or admit two gate classes.

**R2. §1.1 forbids grep-gates while §8.3 relies on one.** The widened Lock-14 falsifier "keys on
directory paths inside `core`" (line 275) — a path grep — as the topology invariant, exactly the
gate shape §1.1 bans for wave-close. Different categories (structural lock vs wave-close), but the
spec should say so explicitly rather than leave the contradiction latent.

**R3. Within-wave sprawl in TW0/TW1/TW6.** The wave COUNT (8) is defensible for the aim, but TW0
bundles ~14 workstreams (D-G reconcile, supersession doc, five-lock audit, meta-lock, Y.13 template,
Lock-15+target-cpu, Lock-4+csc411 checkpoint, x86/wasm deletion, inline-test relocation, MASTER-PLAN
reconcile, oracle hygiene, a full json_canon_bench + samply + full-tree-touch bench subsystem,
NO-GO letter, playground ignore). TW6 bundles six. This is the god-module anti-pattern at wave
granularity — right-sized at the graph, over-stuffed at the contents.

**R4. Lock-15 / target-cpu ownership is ambiguous.** §3.3 assigns Lock-15 + `target-cpu` to TW-S;
the manifest pulls them into TW0 "landed early" (line 328). One owner, please.

**R5. checkasm-per-kernel may be dav1d cargo-culting for this target.** §5.4 mandates checkasm
co-emitted per kernel iterating every variant (the liveness property). checkasm earns its keep in
dav1d across hundreds of kernels and many ISAs; bbnf targets ~4-6 kernels on ONE ISA (aarch64/Apple-M
only, line 27). A per-kernel differential test + bench may deliver the same liveness guarantee at a
fraction of the machinery. Price the harness against its kernel count before committing it.

**R6. TW5 steps 4-6 are frontier stuffed into a parse-speed-first manifest.** §6 concedes steps 1-3
are "load-bearing honesty+capability; 4-6 are frontier"; only 1-3 are de-risked (P6). Step 4
(rank-1 HM, Algorithm J, Rémy levels) "un-monomorphizes the closure at the type layer" — an elegance
goal, not the parse-speed first-order aim (PASS-ALPHA:11). Give 4-6 an explicit
defer-or-commit disposition rather than carrying them as committed manifest scope.

**R7. perf_cost count is slightly optimistic at HEAD.** Spec line 184 ("single production site…
NOT ~8"): confirmed `lib.rs:590` is the sole production site in the KEPT engine (2023/2035/2062/2083
sit under `#[cfg(test)]` at 1674/2012/2052 — themselves inline-test-precept violations TW0 must
relocate). But `decision_csp.rs:242,254` are two further production sites; "one" holds only AFTER the
CSP second stage is retired. State it as "one in the retained engine, two more deleted with the CSP
stage."

## Does anything rebuild the substrate-first pathology?

Largely no — §1 is the cure made binding, Y.13 is the gate, no-substrate-without-consumer is law.
The residual risk is concentrated in B1 (a huge crate split whose only same-wave consumer is a tape
whose own consumers are pre-existing regen/oracles — thin but present) and B2 (TW3's value plane
landing ahead of the TW4 decode kernel it needs to hit its floor). Both are ordering questions, not
architecture questions; the substrate discipline itself is sound.

## Convergence

64/100. Strong, coherent, well-pruned draft that is explicitly a pass-1 synthesis: perf rankings are
directional pending TW0 recapture, the central fused-lane bet is unfalsified pending P2, TW1 ordering
is self-deferred to pass-2, TW3∥TW4 hides a coupling, and the everything-gating first commit is
unspecified. Six of seven prototypes have not run. The prototype fleet is precisely the instrument
designed to close B2-B4 and R5; this item needs another loop pass, and is on track for one.
