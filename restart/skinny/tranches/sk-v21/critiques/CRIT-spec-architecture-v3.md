# CRIT — SPEC-DRAFT-P1 architecture, pass-3 verification re-score (the freeze re-score)

Target: `restart/skinny/tranches/sk-v21/SPEC-DRAFT-P1.md` (680 lines, pass-2-amended), read
end-to-end against its predecessor `critiques/CRIT-spec-architecture-v2.md` (82/100, NEW-B1 +
NEW-R1..R7 + carried R3). Charter: verify each pass-2 fold-in against its mandate — correctly
folded, coherently sequenced, consumer-invariant gate named per wave, no new contradiction. Every
folded number cross-checked against its source instrument (`research/38`, `PROTO-A1/A2/A3/B1/B3`).

## Verdict: every v2 item is folded, and the numbers are faithful

**NEW-B1 (BLOCKER — the value-plane default contradiction): CLOSED, cleanly.** The pass-2 rewrite
lands the winning fallback v2 named. The fused single-pass SINK is the default *uniformly* now —
§0:44-47 (charter aim restated as "a lazy cursor over the ONE tape plus a DISTINCT eager
single-pass fused sink… the dual-path shape sonic-rs ships, matched knowingly"), §2:108-122,
§4:243-252 ("the 'stronger claim' is retracted and the elegance argument concedes to the
measurement"), TW3 close §11:594-604. A grep confirms the refuted phrase "generic fold over the
lazy cursor — one substrate" survives **only** as an explicitly-retracted quote (§4:244, "…was run
exactly as this section demanded and LOST") — zero affirmative residue. The A1 D3 numbers are
transcribed exactly: 2.32×/4.85×/2.27× vs sonic, 3.87–4.23× vs its own sink, 5.3–8.4× the AU floor
(A1:124-145 → SPEC:246-248). D2 canada 1.252× wall / 1.137× cyc, D4 parse_tape 1.5654 < 1.63 and
fused citm 1.354 / twitter +23% / canada 2.04× — all match A1 line-for-line. The internal
contradiction v2 diagnosed is gone.

**NEW-R1 (strike the ~10% asm exception): FOLDED.** §0:36-42 restates it as "P5's '~10%
instruction-selection miss the intrinsic cannot survive' is REFUTED by A2… Zero measured asm
exceptions remain"; §5.4:328-335 retires "one measured exception." Door numbers exact
(+34.6/36.5% baseline, ~20-28% over asm, door-b +11.5/15.6%, asm +6.7/10.4%, crossing lever ~2.6×
the ALU lever — A2:47-68). Grep confirms the "~10%" phrase appears only inside the refuted quote.

**NEW-R2 (Sampled nuance, not a flat null): FOLDED.** §2:130-134 carries the exact A1 D1 CI —
canada +1.25% CI[−0.43,+2.94] TIE, twitter +3.45% CI[+1.07,+5.83] real — and states "deleted for
KISS… NOT for a flat null." The "buys nothing" framing v2 flagged is gone.

**NEW-R3 (FLIP-3 misdirected): FOLDED.** §5.1:306-312 re-scopes it to a pluggable cost-model cell
(live `neon.rs:690` already `vaddv`); §11 TW0:546 explicitly WITHDRAWS the item from the sweep.
Matches A2 D2-point-2 verbatim.

**NEW-R4 (Step-0 loci): FOLDED.** §6:376-386 corrects to `view.rs:141` (span destruction) with
`literals.rs:7,17` demoted to the untaken fallback, and the 12-line `wrap.rs:260` helper
(+45/−15, 22/22 Nu8→U8) — matches PROTO-B3:20-49 exactly. Minor: SPEC writes "parent_source"
where B3's field is `parent_span_for_type`/`node.span_text()`; the coordinate is right, the name a
loose paraphrase.

**NEW-R5 (WART-C two-site fold + submodule pin): FOLDED, and the SPEC is the correct copy.** §9
names both sites (`runtime_generator.rs:98` AND `json_sink_direct/mod.rs:98`) and frames it as a
JSON-into-`general_descent` FOLD, not a sniffer deletion (§9:472-475) — the stronger reading B1
settled on (B1:128-130,170). The submodule pin `efc6b875f` (§9:479, TW6:624) is **correct** per B1
(confirmed at B1:3,6,7,9,161). *Out-of-scope note:* `CONVERGENCE-LEDGER.md:113` mis-transcribes it
as `efb6c875f` — a ledger typo, not a SPEC defect.

**NEW-R6 (hit-counts regen-owned): FOLDED.** §4:230-233 and TW1b/TW1c (§11:570-578) annotate the
~1,080 / 960 counts as 95.1%/88.5% regen-owned, true hand surface ~40-60 sites — matches
`research/38`:35-48,140-143 and the 39,931-hand / 176,887-generated split (§11 TW1a:562-569 ==
38:26,139).

**NEW-R7 (TW1b gate cannot meet ≥3%): FOLDED, by name.** §11 TW1b:573-577 states the relaxation
explicitly ("liveness-AND-no-regression, not the §1 ≥3%-win clause — the win is recovering ground
SoA lost"), citing CRIT NEW-R7 and 38 §4. This is the model treatment for a substrate whose §1.1
win-gate is structurally unmeetable at its landing wave.

**R3 (TW0 sprawl): carried, unchanged.** §13.2:672-675 tolerates the ~14 workstreams with internal
sequencing, no re-lettering — the same disposition v2 accepted.

Sequencing is coherent: TW0→TW-S→TW1a→TW1b→TW1c→TW2→TW4.1→{TW3∥TW4.2}→TW5→TW6, with TW4.1
(Eisel-Lemire) before TW3's canada close, the D11 dispatch coupling and the TW3↔TW4.1 decode
coupling both carried openly (§11:518-525). The split-first order is adjudicated sound
(`research/38` §3c). No fabrication surfaced; the pass-2 instruments corroborate every folded
figure at the ratio level, with the absolutes honestly flagged DIRECTIONAL pending TW0-quiet.

## The one refinement pass-2 opened without a matching gate statement

**NEW-R8. TW4.1's `KernelCostTable` close gate is liveness-only, and §1.1's ≥3%-win law is
unmeetable there by the SPEC's own measured fact — mirror the NEW-R7 relaxation.** §1.1:71-72 makes
every substrate/kernel wave close on "liveness AND a measured ≥3% win," and cites the P1 lesson
that liveness under `cargo expand` *alone* is exactly the failure mode (a live scan that still
lost). The pass-2 amendment PROVED (A2/P4-v2, now folded as fact at §5.1:293) that **no
current-fleet decision flips** — the table's first outcome-changing win is the TW6/CSV |S|≤2 band.
So the `KernelCostTable` substrate landing at TW4.1 *cannot* exhibit a ≥3% win at its own wave. Yet
the TW4.1 close gate (§11:592-593) reads liveness-only: "the table feeds the live selector with a
`cargo expand` witness" — no win clause, no relaxation. §5.1:299-301 reconciles the *intent*
(commit-message naming of the TW6 consumer, same-campaign flip, "or the table is the §1 pathology
it exists to cure"), but that reconciliation is not restated as a gate-class relaxation *at the
TW4.1 gate*, so a literal read of §11 against §1.1 leaves the table's gate either unmeetable or
falsely-met-by-liveness — the exact P1 hazard §1.1 warns against. This is the identical shape v2
caught for TW1b and the SPEC handled with an explicit one-sentence relaxation (NEW-R7); TW4.1
deserves the parallel sentence: "the cost-table's win is deferred to the TW6/CSV flip; its TW4.1
gate is liveness + selector-fed + TW6-consumer-named, not ≥3%." The Eisel-Lemire half of TW4.1 DOES
carry a real measured-win gate (canada-beats-sonic), so the *wave* is not dead substrate — only the
cost-table sub-landing needs the gate-class carve-out named. Bounded, one sentence, template in
hand.

**NEW-R9 (cosmetic, carry at freeze).** §4:250 phrases the value plane as "two lanes over the one
tape-side substrate" then, in the same clause, "eager = the tapeless fused single-pass sink." The
charter §0:44-47 phrasing ("a lazy cursor over the ONE tape PLUS a DISTINCT eager sink") is cleaner
and non-self-tensioning. Not a contradiction (the tapeless disclosure is co-clause), but tighten
§4:250 to match §0 so no reader re-reads the eager lane as tape-backed.

## Convergence

**94/100.** The pass-2 agglomerator closed the NEW-B1 blocker completely and folded all seven
refinements faithfully — every folded number matches its source instrument (A1/A2/A3/B1/B3,
research/38) to honest rounding, no stale affirmative claim survives the grep, sequencing and
couplings are coherent, and each wave names a consumer-invariant gate of the correct class. The
architecture is freeze-ready in substance. Two residuals cap it below 100, both spec-edits rather
than execution items: the primary (NEW-R8) is a single-sentence gate-class relaxation the TW4.1
`KernelCostTable` needs to match the treatment TW1b already received — a real §1.1/§11 asymmetry
that the pass-2 "no-fleet-flip-is-fact" finding sharpened without a corresponding gate line; the
secondary (NEW-R9) is a phrasing tightening. Neither is a measurement or a rebuild. One short
editorial loop over the TW4.1 gate clears the freeze.
