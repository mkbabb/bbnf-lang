# SK-V18 S-P2 CHALLENGE — CH6 OVERFIT-PRUNE (spine) (cycle V2)

Lens: does any recommended candidate overfit to JSON/CSS specifics or smuggle a hand-written
contrivance under a grammar-driven banner; is the ONE-generator framing preserved end-to-end; would
the architecture generalize to a 4th/5th grammar? Reviewer: orchestrator (infra dropped the
sub-agent dispatch). Read: SYNTHESIS-RESEARCH.md (all, post-fold) + rA-emitter-unify.md + rB-css-lowering.md.

## Claims

### C1 [ACCEPT] — the discriminator is grammar-NEUTRAL, not a JSON/CSS fork
R-A dispatches on the 5-shape BackendShape {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}
(lower/mod.rs:18), a cost-model output over rule shapes carrying zero grammar tokens. A 4th grammar
maps to one of these shapes by its cost profile, not by a name. RuntimeEmitterKind (the JSON-vs-CSS
fork) is DELETED. The one-generator framing is structurally preserved. ACCEPT.

### C2 [ACCEPT] — Sheets + BBNF-self are the generality controls beyond JSON/CSS
PROVE (R-E) emits a structurally-distinct THIRD grammar through the un-forked generator; S-P0 §7 tees
BBNF-self as the SK-V19 fourth-grammar litmus. The pass does not stop at JSON+CSS — it builds the
negative control that proves generality. ACCEPT.

### C3 [ACCEPT] — the JSON leaf kernels are rule-parameterized, not JSON-hardcoded
R-C's leaf primitives are invoked by the `.bbnf` string/number rules and (post-CH2-V6 fold) vary
under the rule's byte-set/class mutation — so a 4th grammar with a `number` rule reuses the same
parameterized kernel. Not a JSON-specific blob. ACCEPT.

### C4 [REVISE] — balanced_component_scan's NEUTRALITY is ASSERTED for R-B but only PROVEN for the R-F sub-kernel
R-F candidate A retargets the already-neutral alphabet-data kernel (bbnf-simd::find_ascii_set_member64)
— genuinely grammar-neutral (caller supplies the byte set). But the HIGHER-LEVEL R-B named primitive
`balanced_component_scan` (the recursive balanced-delimiter recognizer) is described as
grammar-parameterized by its delimiter byte-set, yet its NEUTRALITY (that it serves grammars other
than CSS) is asserted, not demonstrated: §1 R-F claims "the same primitive serves CSS AND JSON" but
the balanced-recognizer SHELL (not just the inner scan) is only exercised by CSS in this pass. A
primitive named neutrally but exercised only by CSS is an overfit-in-waiting. EDIT
(SYNTHESIS-RESEARCH §4 R-B bullet + §5-risk-2): require that `balanced_component_scan`'s neutrality be
PROVEN by a non-CSS invocation (the JSON object/array balanced `{}`/`[]` nesting, OR the Sheets
`paren_expr` balancing) — i.e. at least one non-CSS grammar must invoke the SAME primitive in this
campaign, else it is demoted to a CSS-scoped primitive with an honest CSS-specific name (not a false
neutral). REVISE (generality-proof gap, not a present overfit).

### C5 [ACCEPT] — the §6 escape is bounded to PROFILE-PROVEN-hot leaves, not arbitrary blobs
The named primitives are confined to the measured hot leaves (the 94.1% scan, the 91.5% JSON leaf
kernels); the structural skeleton is walk-derived. The pass does not relabel large hand-written
bodies as "primitives". (CH7 examines the §6-honesty gate in depth.) ACCEPT.

## Net
The one-generator framing is preserved; the discriminator is neutral; Sheets+BBNF-self prove
generality. One generality-proof gap: the R-B balanced-recognizer must be PROVEN neutral via a
non-CSS invocation this campaign, or honestly named CSS-scoped — else a neutrally-named CSS-only
primitive is an overfit-in-waiting (C4).

TALLY accept=4 revise=1 reject=0
