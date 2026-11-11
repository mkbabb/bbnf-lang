# CRIT-codex-honesty — the honesty + evidence audit of the SK-V23 codex

Adversarial critique agent, numerically conservative. Scope: does `SPEC-DRAFT.md` quote every
settled SK-V21/22 fact correctly, cite every SOTA/perf number cold-and-labeled, triage the
feature fold honestly (no kitchen sink), and refrain from over-reading its prototype verdicts?
Verdict up front: the honesty spine is SOUND — no fabricated number, no misquoted settled fact,
no kitchen-sink triage. The gaps are (a) the six prototypes ran AFTER this PASS-1 synthesis and
are not yet folded into the tranche bodies, (b) two disclosure sharpenings the profiling law
already demands, and (c) one citation-hygiene hazard. All are next-synthesis- or wave-ownable.
**Convergence: 87%.**

---

## PART 1 — The settled-fact quotations: VERIFIED CORRECT

Every SK-V21/22 fact the codex leans on was checked against the frozen source. All quote true.

- **vshrn obviates hand asm.** SPEC-DRAFT:7-8 cites `sk-v21/SPEC.md:44-48`. Source confirms:
  "door (a), the pure-intrinsic `vshrn` movemask, beats the hand asm by ~20-28%... +36.5/34.6%
  over baseline vs asm's +6.7/10.4%... the classify kernel is now the case that PROVES an
  intrinsic obviates the asm lane" (sk-v21/SPEC.md:44-47). research/02:24 restates it "+35% over
  the alternatives" — an honest mean of 36.5/34.6. TB5.W2 carries the non-optional correctness
  trap intact (nibble-domain, "compaction contract moves to the nibble domain", SPEC-DRAFT:355;
  sk-v21 25 §2.4). No over-claim: TB5.W2 never promises the +35% primitive win as a whole-kernel
  win, and research/02:44-47 explicitly separates "+35% primitive" from "mid-single→teens %
  kernel". CLEAN.

- **canada = Eisel-Lemire float tax.** SPEC-DRAFT:334,344,356,383 quote "+1.48 cyc/B canada tax
  localized to `materialize_f64`". Source: sk-v22/research/04:149,156 "+1.481 / +1.48 cyc/B on
  canada — the float tax"; :222-224 "canada eager float — fused/sonic 1.20× SLOWER... +1.48 cyc/B
  materialise tax localised to `materialize_f64`. Eisel-Lemire/SWAR... the single measured win".
  Both the "+1.48" and the charter's "1.20× loss owned by Eisel-Lemire" quote true.

- **2.10×/1.41×/2.06× baseline.** sk-v22/research/04:112,219 "parse_tape beats sonic-rs
  2.10× / 1.41× / 2.06× vs sonic-rs (cyc/byte)". The codex anchors to it (§0:52-54; §5:623) and
  never restates the triple unlabeled.

- **twitter reconciliation.** SPEC-DRAFT:222,625 "adjudicates 2.10× vs 1.59× with the +36%
  sonic-side drift disclosed; 33:59 never cited for a 2.10× claim" reproduces sk-v22/SPEC.md:444
  verbatim in substance (report-33 1.59× vs sonic ~2.48; 04 re-measured sonic 3.371 = +36%
  undisclosed; PRECONDITION on GW0.e). CLEAN.

- **N\*=3 / |S|≤2 flip band.** SPEC-DRAFT:9,399 cite sk-v21/SPEC.md:607-609,635-646. Source:
  "live crossover N\*=3, flip band |S|≤2, JSON/CSS sit at N≥3" (sk-v21:607-608) and the CSV
  |S|≤2 flip (sk-v21:642). Correct.

- **fused sink default; AoS; TOML walls dissolvable.** sk-v21:126-128 "The fused parse+value
  single-pass SINK is the default materialization lane"; AoS carried as settled (SPEC-DRAFT:97;
  sk-v22/04:445 NEGATIVE-RESULTS "AoS-beats-SoA"); TOML both walls "pass-2-proven dissolvable
  (B1, 7/7 round-trip)" (sk-v21:637-643). All three quote true.

- **CSS depth-asymmetry honestly capped.** N4 is scoped "catalog breadth only, never
  minify/browserslist/nesting" (SPEC-DRAFT:317,643; research/04:30-33,99-100). The 2.07-3.47×
  vs lightningcss figure is disclosed as parse-only. The profiling law's bans hold: the codex
  forbids geomean explicitly ("per-corpus, never geomean", SPEC-DRAFT:330,410) and quotes no warm
  figure. No honesty defect in the number surface.

## PART 2 — The feature-fold triage: HONEST, NOT KITCHEN-SINK

research/04 is a model of restraint. §5's "ruthless cut" folds exactly FIVE near-term items
(P1, P2, N4, Q1, Q3, N5, Q5, N6 — all riding waves that already exist), ELEVATES two to a named
post-codex shopwindow (N1 DAP, Q2 LSP lints), and CUTS four with concrete reasons (N2 fights
fail-fast, N3 no consumer, Q4 no stable API, P4 fights singular-arena). The shopwindow honesty
boundary is stated at every touch: "debugs the grammar semantics, not the emitted kernel"
(research/04:57,275; SPEC-DRAFT:406,664). This is the OPPOSITE of a kitchen sink. No finding.

## PART 3 — The honesty refinements (the profiling law demands these AT the claim site)

**R1 (moderate) — the canada lane-attribution asymmetry is disclosed only implicitly.** The
anchor's 2.06× canada win is the LAZY `parse_tape` lane (sk-v22/04:127,136 "The lazy `parse_tape`
product beats sonic on canada 1.38×"). The DEFAULT SHIPPING lane is the eager fused sink
(sk-v21:126), and it LOSES canada 0.83× / 1.20× slower (sk-v22/04:128,137,222 "only the EAGER
fused sink loses there"). The codex owns the tax in TB5 but never states, at the anchor/claim
site, that the headline 2.06× is the lazy lane while the default lane loses pending Eisel-Lemire.
sk-v21:52,126 make disclosure-at-the-claim-site a standing requirement ("every comparison
asymmetry disclosed at the claim site"). FIX: one clause at §5:623-626 and §1:97 naming the lane
split.

**R2 (minor) — "canada eager cold BEATS sonic" reads declarative where it is a target.**
SPEC-DRAFT:344 (TB5.W0) states the win in the present tense; sk-v22/04:224 has it as the pending
"single measured win", not yet achieved. It is correctly a CLOSE GATE at :374, but the wave-body
prose should read "must beat / targets" to avoid reading as a settled result.

## PART 4 — The prototypes ran AFTER PASS-1 and are NOT yet folded (the real convergence gap)

The SPEC-DRAFT is dated 2026-07-09; PROTO-PB-1/4 are dated 2026-07-10. All six returned
favorable verdicts (PB-1 GO; PB-2 PROVEN@92%; PB-3 compiles+byte-identical; PB-4 PASS 4/4;
PB-5 verdict-vs-criteria; PB-6 GO), so none breaks the plan — but their refinements are not yet
in the tranche bodies. Two are load-bearing:

**B1 (blocking) — PB-1's mandatory fused-trigger is absent from PT-R2.W2.** PB-1 proves the
ungated accel form REGRESSES 1.1-1.9× on dense-match inputs and that "the fused trigger... is
therefore a REQUIRED design element, not optional" (PROTO-PB-1:82-90,98 "consume via the
fused-trigger loop only — never the ungated form"). SPEC-DRAFT PT-R2.W2:478-480 wires
`StateAccel` into the search loop with no mention of the mandatory gate, yet its own §7 success
criterion is "no regression on dense-match" (:697). The next synthesis MUST encode
fused-trigger-mandatory / ungated-forbidden into PT-R2.W2, plus PB-1's open GATE=16 tuning note.
(Not over-read: the codex uses research/02's conservative 4-10× — PB-1's 18-67× exceeds it.)

**B2 (blocking) — the HM-core-without-consumer tension in TB4.W1 is unresolved.** PB-4 proves the
two HM surfaces (`@host` unify, rank-1 HM) are 0-hole / dormant on the whole corpus
(PROTO-PB-4:25,36-37,117-139 "the greenfield can land steps 1-3-5 now and gate the unifier behind
first real demand"). TB4.W1:306 reads as committing to BUILD the HM core in-wave. Under the Y.13
consumer-invariant law (§0:43-46; sk-v21:72-83) a substrate with zero corpus consumer cannot
close a substrate wave — building the HM core now is precisely the "substrate-without-consumer"
master pathology (sk-v21 §1). TB4.W1's close gate (:322-324) only gates the STRUCTURAL projection
(steps 1/3/5, which PB-4 validated), so the HM core is implicitly unrequired — but the wave prose
does not say so. FIX: explicitly scaffold-dormant the HM core (holes not silent defaults) and
land steps 2/4/6 behind first real demand, matching PB-4 + Y.13.

## PART 5 — Evidence hygiene

**H1 (minor) — bare "04:" citations are ambiguous.** The codex uses bare `04:149-159` for
sk-v22 research/04 (SPEC-DRAFT:344,626) but `research/03:…` for sk-v23 research. sk-v23 ALSO has
a `research/04-bbnf-features.md`; `04:149-159` followed into it lands on Q2/Q3 diagnostics, wrong
content. Disambiguate bare `04:` as "sk-v22 research/04" once, or prefix all cross-tranche cites.

**H2 (minor) — PB-2's residue.** The x86-purge probe is "PROVEN" at CONFIDENCE 92% with 5 test
failures (attributed 100% to missing-corpus file reads, zero parity failures — PROTO-PB-2). Not
over-read, but TB1/TB2 should carry a named execution checkpoint that the 5 corpus-file failures
are genuinely corpus-absence, not hidden x86 coupling, before the guard reads green.

**H3 (residual, low) — external SOTA citations unverified.** research/04 names "TOSEM 2025",
"Speculate (Mozilla)", "Grammarinator", "tree-crasher", "mdn/data css/properties.json";
research/02 names Vectorscan/Hyperscan/simdjson/Lemire/Mula idioms. All are named, none fabricated
in tone, but none independently web-verified in this pass (no cargo/web budget). Flag for a
citation-verification lane, not a blocker.

## CONVERGENCE: 87%

The honesty audit passes: settled facts quote true, the triage is ruthless, no warm/geomean
figure, prototypes came back GO. Short of 100 by: two blocking prototype-fold gaps (B1
fused-trigger-mandatory, B2 HM-core-consumer), two disclosure sharpenings the profiling law
already mandates (R1 canada lane, R2 target-not-claim), and citation hygiene (H1). None require
re-architecting; all are next-synthesis / wave-ownable.
