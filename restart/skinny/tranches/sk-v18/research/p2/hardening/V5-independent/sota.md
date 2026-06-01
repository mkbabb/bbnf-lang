# SK-V18 S-P3 — V5-independent GROUND/sota verdict (clean re-validation)

Independent re-challenge of the COMMITTED S-P2 synthesis
(`restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md`) under ONE lens:
**SOTA-PRESERVATION**. Question per claim: does any recommended candidate (R-A..R-F) risk
regressing the >SOTA hot leaves (CSS `find_component_delim`+`consume_balanced_at` 94.1%, JSON
`parse_object_value_at_direct`+`parse_array_element_at_direct` 91.5%) WITHOUT an honest
(a)-(d)-gated named primitive? Is the G2 explicit >SOTA-regression gate sound? Is
preserve-rich-ast honored?

Method: read the synthesis + the S-P1 profile + the S-P0 audit; spot-checked the cited hot-leaf
code on disk at the live tree
(`css_l4_declaration_values/generated.rs:657-713` for the CSS scan,
`json/generated.rs:823-899` for the JSON byte-dispatch,
`css_l4_declaration_values/generated.rs:296-320` for the lazy-rich projection). Each claim is
judged ACCEPT / REVISE (file + exact edit) / REJECT (falsifying evidence). Only claims bearing on
SOTA-preservation are enumerated; the orthogonal structural claims (R-A row-collapse mechanics,
R-D phantom DELETE, R16 numbering) are out of this lens except where they touch a hot leaf.

Ground-truth re-confirmed on disk this pass:

- CSS hot leaf (`generated.rs:657-680`/`693-713`): `find_component_delim(&self, mut pos, delimiters: &[u8])`
  is a flat scalar scan whose stop set is a **caller-data `&[u8]` argument** (call sites pass
  `b";{}"`, `b"{};"`, `b":{};"`, `b";}"` at `:493`/`:512`/`:550`/`:585`/`:687`), with structural-byte
  dispatch on `' " / ( [ {` recursing through `consume_balanced_at`. This is EXACTLY the shape R-B
  names: a narrow flat recognizer parameterized by a byte set, NOT a combinator descent. The S-P2
  characterization is faithful to the code.
- JSON hot leaf (`json/generated.rs:823-899`): `parse_object_value_at_direct`/`parse_array_element_at_direct`
  are `match byte { b'{'=>… b'['=>… b'"'=>… b'-'|b'0'..=b'9'=>… b't'/b'f'/b'n'=>… }` byte-dispatch with
  monomorphized `sink.object_*`/`sink.array_*` call sites and per-arm inline cfg
  (`inline(never)` under `parse-attribution`, else `inline(always)`, `:861-862`). This is EXACTLY
  the "same `match byte` + `sink.*` call sites the profile rewards" R-C C1 claims to re-emit.
- Lazy-rich projection (`generated.rs:296-320`): `rich_summary` re-derives every field "from
  (source, offset) via lazy spans, writing nothing to the payload arena" — preserve-rich-ast holds
  at the source the G2 `track1_rich` gate measures.

---

## Claims under the SOTA-PRESERVATION lens

### C1 — R-B recommends balanced-scan as a grammar-parameterized NAMED PRIMITIVE, refusing the generic IR tree-walk because "a tree-walk descent = lightningcss's own architecture, regresses >SOTA" (§1 R-B; §4 PRIMARY finding; §5-risk-2)

**ACCEPT.** This is the load-bearing SOTA-preservation decision and it is correct. The disk
confirms the 94.1% leaf is a flat byte-at-a-time scan over a caller-supplied stop set — a naive
grammar-walk lowering produces the recursive combinator descent the flat scan was purpose-built to
avoid, which categorically regresses the >SOTA (the synthesis's §4 "genuine §4 tension" and its
outright REJECT of R-B/R-C Candidate C are sound). The honest path (name it, parameterize by
grammar-derived byte-set ARGS, invoke from the emitted scan) is the only one that structurally
preserves the hot body. The (a)-(d) gating discipline is present (§4 four-conjunct gate). No revise.

### C2 — R-C C1 (`SinkOnlyExpr` AST-walk emitter) is the ONLY candidate that "structurally preserves the 91.5% hot leaf … the walk emits the same `match byte` + `sink.*` call sites the profile rewards" (§1 R-C; §4 SECONDARY finding)

**ACCEPT.** The disk byte-dispatch (`:832-857`) is a finite per-byte `match` with monomorphized
sink calls — a `SinkOnlyExpr` Alt{Dispatch}-over-leading-byte walk re-emits this shape directly
(the house `tape_plan.rs`/`json_typed_direct.rs` pattern). C1 keeps the structural skeleton
walk-derived while the proven-hot inner kernels (string/number leaf scanners) stay byte-stable as
named primitives. The competing candidate (R-C tree-walk) is correctly REJECTED as regressing the
monomorphized-sink leaf. The claim is grounded and SOTA-safe.

### C3 — The G2 EXPLICIT >SOTA-regression gate: `track1_rich/lightningcss >= the S-P1 ratio` on `css_canon_bench` (cold, corpus-in-timer), DISTINCT from the 9-field cssparser oracle parity gate (§3 G2 exit; §1 R-B; §4)

**ACCEPT — and this is the single most important SOTA-preservation finding of the pass.** The gate
is SOUND precisely because it is decoupled from parity. G2 re-derives the 94.1% scan; oracle parity
proves only that the OUTPUT is correct, never that THROUGHPUT survived (a combinator descent
produces identical output at a fraction of the speed). The synthesis explicitly states this
("oracle parity alone … does NOT prove throughput preservation — the bench re-measurement is the
binding regression falsifier", §3 G2). Grounding the threshold in the S-P1 *ratio* rather than an
absolute Mbps is the correct choice under the documented load depression (S-P1 §0/§5-risk-7:
loadavg 4.35, absolute DIRECTIONAL). The per-corpus S-P1 ratios this gate must hold are concrete
and load-robust: bootstrap 2.190, tailwindcss 3.375, **material-components-web 1.658 (the binding
min)**, animate 2.101 (S-P1 §1). The gate text already carries the QUIET-recapture caveat. Sound.

### C4 — R-F "inner-skip vectorize" retargets the EXISTING checkasm-gated kernel onto the SCALAR RECURSIVE SHELL, vectorizing ONLY the inert-run skip with the set as caller data; error positions come from the shell (§1 R-F; §3 G5/G6 exit; §5-risk-6)

**ACCEPT.** This is the SOTA-safe retarget shape. The disk shell (`:662-678`) does string/comment
skipping and `()[]{}` recursion the dead flat kernel cannot — so the kernel MUST be retargeted to
vectorize only the inert run between structural bytes, stopping at `([{'"/`, leaving recursion and
error positions to the scalar shell. The synthesis names exactly this constraint (§5-risk-6: "the
vector skip must stop AT `([{'"/` so the scalar shell still handles recursion/strings; error
positions must come from the shell"). This ACCELERATES the 94.1% leaf without altering its semantics
— a strict >SOTA improvement, not a regression risk. The honest "speedup bounded by inert-run
length, a MEASUREMENT to confirm post-wire" framing is correct.

### C5 — R-F neutrality claim: the same eq-set kernel "JSON's `scan_structurals` already rides", JSON neutrality "honest, NOT fabricated (JSON product path is scan-free)" (§1 R-F)

**ACCEPT.** Consistent with S-P1 §2 (the direct `track1_digest` JSON product path is scan-free;
`json/scan.rs` samples ZERO). The synthesis does NOT fabricate a JSON hot leaf to justify the
kernel — it correctly grounds neutrality in the SHARED eq-set sub-kernel (caller-supplied byte set)
while admitting the JSON *product* path does not exercise it. This honors the profile-first mandate
(S-P1 §5: "do not author a JSON classifier"). G5 = neutralize the zero-sampled `json/scan.rs`, no
JSON classifier authored. SOTA-neutral and honest.

### C6 — The balanced-recognizer SHELL neutrality-proof obligation (CH6): `balanced_component_scan` must be invoked by at least one NON-CSS grammar (JSON `{}`/`[]` OR Sheets `paren_expr`) ELSE demote to `css_balanced_component_scan` (§4 NEUTRALITY-PROOF obligation)

**ACCEPT, with a SOTA-preservation caveat that is already correctly bounded.** The obligation is the
right discipline against a neutrally-named CSS-only overfit. The SOTA-relevant risk: forcing the
JSON `{}`/`[]` nesting through the SAME balanced-shell to satisfy neutrality could regress the JSON
91.5% leaf IF the JSON byte-dispatch were re-routed through a generic recognizer. The synthesis does
NOT mandate that — the JSON hot path's `match byte` dispatch (`:832-857`) descends via
`parse_object_direct`/`parse_array_direct`, not via a stop-set `find_component_delim`; the shared
sub-kernel is the inner alphabet-scan, not the dispatch. The §4 escape valve (demote to a
CSS-scoped name rather than force a regressing non-CSS invocation) is the correct SOTA-safe
fallback: an honestly-CSS-scoped name beats a JSON regression to chase a false neutral. No revise —
the obligation as written already permits the demotion that protects the JSON leaf.

### C7 — R-C SECONDARY finding: the JSON string/number leaf kernels (`b'-' | b'0'..=b'9'` array fast-path, `match_tiny_plain_string_direct`) stay byte-stable as named primitives; the (b) falsifier is the BYTE-SET/numeric-class mutation (§4 SECONDARY; §5-risk-3)

**ACCEPT.** Correct and well-bounded. The byte-exact micro-opts are real (the `b'-' | b'0'..=b'9'`
arm is live at `:841`/`:881`). The synthesis's insistence that the (b) falsifier be the
class-mutation ("widen the `number` rule's digit class → the `b'0'..=b'9'` literal widens"), NOT
merely byte-equivalence, is exactly what distinguishes a derived leaf from a relabeled courier (the
byte-equivalence gate alone is satisfiable by routing the same literal through the new walk). The
§5-risk-3 caution "do NOT LCD-unify the value/object/array dispatch triple (regresses the
monomorphized-sink leaf)" directly protects the 91.5% leaf. SOTA-safe.

### C8 — G1 exit gate re-emits `parse_object_value_at_direct` "with identical inline cfg + `sink.object_*` call sites (91.5% MUST-preserve)" and requires byte-equivalence vs the `json_templates/` oracle BEFORE oracle deletion (§3 G1 exit)

**ACCEPT.** Grounded: the disk shows the per-arm inline cfg (`inline(never)`/`inline(always)`,
`:861-862`) that the gate names, and the monomorphized `sink.object_*` calls. Byte-equivalence
against the oracle before deletion is the binding proof the projection is real (S-P0 §2.1.1). The
±5% line delta is correctly a SOFT tripwire only. The 91.5% MUST-preserve is named explicitly. The
gate is sound; it preserves the hot body by byte-equivalence and proves derivation by the
`.bbnf`-mutation falsifier. SOTA-safe.

### C9 — G3 un-fork reads output-shape ONLY from `program.policy_summary.backend_shape`, NEVER from a `RuntimeTarget` field; the fourth conjunct `emit_shape_source == lowered_program` (§3 G3 exit; §5-risk-1)

**ACCEPT (SOTA-tangential, but it does protect a hot-leaf seam).** The relocated-seam risk is
primarily a generalization-honesty concern, but it bears on SOTA because a per-grammar branch
surviving in a neutral data table is precisely the mechanism by which the CSS scan or JSON dispatch
could be re-specialized (and thus diverge or regress) under a green arm-census grep. The fourth
conjunct (grep the `render(program)` body for any read of `target.profile`/`target.emitter`/
`target.output_labels`/`target.profile_contract` == 0) is the right structural guard. It does not
itself touch the hot bodies — those are preserved by G1/G2 — but it prevents the un-fork from
re-forking the shape that carries them. Sound as written.

### C10 — preserve-rich-ast: R-D's `Cursor` micro-trait "shares only the cursor/laziness contract, NEVER navigation", JSON's rich tree preserved by construction (`json_rich_navigation_preserved == true`); the CSS `track1_rich` is lazy `ValueRef` projection writing nothing to the arena (§1 R-D; §5-risk-4; S-P1 §0)

**ACCEPT.** preserve-rich-ast is honored on BOTH the trait axis and the bench axis. (1) The R-D
trait is deliberately narrow (cursor/laziness, not a forced common value shape) — the synthesis
correctly rebuts the "too thin" critique by binding to `json_rich_navigation_preserved == true` and
noting that ANY trait wide enough to satisfy the critic LCD-flattens JSON (§5-risk-4); a forced
common `Value` shape is REJECTED (Candidate B). (2) The `track1_rich` product the G2 gate measures
is genuinely lazy-rich on disk (`:296-304`: "writing nothing to the payload arena: rich, lazy, not
eager, not flattened") — so the >SOTA-regression gate measures the REAL preserve-rich-ast product,
not a count-only structural probe. This is the full-value-materialization-vs-eager-CSSOM honest
framing (S-P1 §0; H1). preserve-rich-ast is intact.

### C11 — R-E-2 (precedence-tower core) lowers to the EXISTING `SinkOnlyExpr` vocabulary needing NO new IR primitive; the stress is on G3's GENERALITY, and Sheets does NOT use the CSS NEON (§1 R-E; §3 PROVE; §3 sequencing note)

**ACCEPT (SOTA-neutral by construction).** Sheets carries no >SOTA bar in this campaign (it is the
generalization litmus), so it poses no SOTA-regression risk to JSON/CSS. The synthesis correctly
routes PROVE PARALLEL to G5/G6 with the explicit note "Sheets does not use the CSS NEON" (§3
diagram) — so the Sheets proof cannot perturb the CSS hot-leaf retarget. The §4 R-E CANDIDATE
framing (a precedence primitive surfaces ONLY if G3 cannot render `CallRule`/`RepeatLoop` chains
from grammar structure) is honest and does not touch the JSON/CSS >SOTA. No SOTA-preservation
exposure.

### C12 — The G6 timed-plane binding: checkasm differential is a CORRECTNESS gate only; any Mbps/speedup FIGURE comes from the corpus-in-timer symmetric harness, deferring the speedup CLAIM to the H1 symmetric timer (§3 G5/G6 exit; addendum 5; §5-risk-7)

**ACCEPT.** This is the correct SOTA-MEASUREMENT honesty discipline. Separating the correctness gate
(checkasm differential, always valid) from the throughput CLAIM (deferred to the H1 quiet symmetric
timer, same plane both sides, corpus-in-timer) prevents a load-depressed or asymmetric speedup claim
from masquerading as a >SOTA win. The G6 acceleration must hit the LIVE generated caller
(`acceleration_at_admission == admission` via the generated-`generated.rs` caller census, NOT a
`#[cfg(test)]` caller) — directly addressing R7 (the dead-at-admission NEON, S-P0 R7). This both
preserves and honestly proves the >SOTA. Sound.

### C13 — (d) PROFILE-PROVEN-NARROW-LEAF: every admitted primitive covers a SINGLE hot leaf attributable to a named S-P1 profile leaf; a "primitive" spanning a rule's whole body or an unprofiled region is REJECT regardless of (a)-(b)-(c), machine-checkable as primitive LOC vs profiled hot-leaf extent (§4 four-conjunct gate)

**ACCEPT — and this is the strongest SOTA-PRESERVATION safeguard in the contract.** The (d)
conjunct is what prevents the §6 named-primitive escape from admitting an arbitrarily large
relabeled blob that merely varies under mutation (the paper-close R-A0-3 / S-P0 R-A0-3 names as "the
single largest paper-close surface"). Binding the primitive size to a named S-P1 hot leaf
(`find_component_delim` 79.5%, `consume_balanced_at` 14.6%, JSON dispatch 91.5%) and forcing the
surrounding SKELETON to be walk-derived is the correct mechanism: it preserves the proven-hot leaf
verbatim while forbidding the whole body from being smuggled in as one "primitive". This is the
honest reconciliation of "preserve >SOTA" with "no hand-written blob". Fully sound.

---

## Cross-cutting SOTA-preservation judgement

The synthesis does NOT risk regressing either >SOTA hot leaf without an honest (a)-(d)-gated named
primitive. Every place a fully grammar-derived parser CANNOT preserve the >SOTA (the §4 surfaces)
is named as a gated primitive with a per-primitive mutate-falsifier AND the size-bounding (d)
conjunct. The two architectural REJECTs that would have regressed >SOTA (R-B/R-C full tree-walk;
R-D forced-common `Value` shape) are correctly excluded. The G2 explicit >SOTA-regression gate is
SOUND — it is decoupled from parity, ratio-grounded (load-robust), and names the bench
re-measurement as the binding falsifier with the concrete per-corpus S-P1 ratios (min 1.658
material-components-web). preserve-rich-ast is honored on both the trait axis (narrow cursor
contract, no LCD-flatten) and the bench axis (lazy `ValueRef` projection, nothing to the arena,
verified on disk). The R-F retarget ACCELERATES the CSS 94.1% leaf without semantic change and does
not fabricate a JSON hot leaf. The timed-plane binding defers the speedup CLAIM to the quiet
symmetric H1 timer.

ZERO REVISE, ZERO REJECT under this lens. The orchestrator-applied S-P2 V2 changes hold under clean
independent re-validation.

One non-blocking observation (NOT a revise — it is already covered): the P5 metalang leak (R15) is
live in the JSON hot leaf itself (`parse_w11_1_number_object_direct`/`parse_w11_1_number_array_direct`
at `json/generated.rs:841`/`:881`). G1's byte-equivalence-then-rename and P5's source-level rename
both touch this hot-leaf call site; the SPEC author should ensure the P5 rename
(`parse_w11_1_number_*` -> `parse_number_*`) and the G1 byte-equivalence gate are sequenced so the
rename does not spuriously trip G1's identical-call-site check — but this is a SEQUENCING note for
S-P3 already implied by P5-before-G1 in the standing order, not a SOTA-preservation defect in the
S-P2 synthesis. The synthesis is clean under the SOTA lens.

TALLY accept=13 revise=0 reject=0
