# SK-V18 S-P2 CHALLENGE — CH3 SOTA-PRESERVATION (cycle V2)

Lens: does any RECOMMENDED candidate risk REGRESSING the >SOTA hot leaves (CSS find_component_delim
94.1%, JSON parse_object_value_at_direct ~91.5%) without an honest named-gated primitive; is
preserve-rich-ast honored; is the gate sufficient to catch regression BEFORE landing?
Reviewer: orchestrator (infra dropped the sub-agent dispatch twice; applied on-disk, transparent).
Read: SYNTHESIS-RESEARCH.md (post-fold, §1/§3/§4/§5) + rB-css-lowering.md + the S-P1 profile.

## Claims

### C1 [ACCEPT] — R-B correctly refuses the naive grammar-walk that would regress the 94.1% scan
SYNTHESIS §4 names the PRIMARY §6 finding precisely: a naive grammar-IR tree-walk lowering of the
CSS scan produces "the combinator-shaped recursive descent (lightningcss's own architecture) that
categorically regresses >SOTA." The recommended R-B (B⊃A) instead lands the balanced delimiter scan
as a grammar-parameterized NAMED primitive, preserving the flat scan. This is the correct
SOTA-preservation posture — it does NOT pretend the scan is freely grammar-derivable. ACCEPT.

### C2 [ACCEPT] — R-C keeps the JSON hot-leaf micro-opts byte-stable as gated primitives
SYNTHESIS §1 R-C + §4 SECONDARY finding keep the 91.5% hot leaf's inner kernels (digit fast-path,
tiny-string inline) byte-stable as (a)-(b)-(c)-gated primitives; the structural skeleton is
walk-derived. The G1 byte-equivalence gate (against json_templates/ oracle) catches a JSON
structural regression. ACCEPT.

### C3 [ACCEPT] — preserve-rich-ast honored
§5-risk-3 forbids LCD-unifying the value/object/array dispatch triple (regresses the monomorphized
sink leaf); R-D preserves JSON rich tree navigation by construction. No typed rule is flattened for
speed. ACCEPT.

### C4 [REVISE] — G2 exit binds parity but NOT an explicit >SOTA RE-MEASUREMENT; "no >SOTA regression" is parenthetical, not a gate
SYNTHESIS §3 G2 Exit lists `verbatim_blob_present == false`, the named-primitive (a)-(c) falsifier,
and "9-field cssparser oracle parity held (no >SOTA regression)". The "(no >SOTA regression)" is a
parenthetical to the CORRECTNESS-parity claim — there is no explicit performance gate. The CSS
lowering wave (G2) is where the 94.1%-scan regression risk is HIGHEST (it re-derives the scan), yet
nothing requires a re-measurement of `track1_rich/lightningcss` on css_canon_bench at the
S-P1-established ratio. A grammar-derived scan could pass oracle parity (correct output) while
regressing throughput below the lightningcss bar — admitted under a green gate. This mirrors CH2's
V10 finding for G6. EDIT (SYNTHESIS-RESEARCH §3, G2 Exit): add an explicit conjunct —
`track1_rich/lightningcss >= S-P1 ratio on css_canon_bench (cold, corpus-in-timer)` — as the
binding >SOTA-regression falsifier, distinct from oracle parity; absolute figures inherit §5-risk-7's
QUIET-recapture caveat. REVISE (gate-completeness, not candidate-selection).

### C5 [ACCEPT] — the §6 named-primitive seam IS the SOTA-preservation mechanism, correctly co-located with G6
The balanced-scan primitive doubles as the G6 NEON-retarget call site (§2 coupling 3): the one seam
that preserves AND accelerates the 94.1% leaf. This is the architecturally-correct place to defend
>SOTA. ACCEPT.

## Net
No candidate regresses >SOTA silently; the highest-risk path (R-B CSS lowering) is the explicitly
gated §6 primitive. One gate-completeness gap: G2 must bind an explicit cold corpus-in-timer >SOTA
re-measurement, not fold it into oracle parity (C4). preserve-rich-ast intact.

TALLY accept=4 revise=1 reject=0
