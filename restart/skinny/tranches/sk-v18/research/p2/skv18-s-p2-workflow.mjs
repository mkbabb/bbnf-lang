export const meta = {
  name: 'skv18-s-p2-research',
  description: 'SK-V18 S-P2 research pass: candidate architecture classes for the un-forked grammar-driven generator (emitter unify / CSS lowering / JSON projection / shared value-API / Sheets proof / G6 neutral scan), grounded in real code, schema-free 3Z CHALLENGE',
  phases: [
    { title: 'Research', detail: 'R-A..R-F candidate classes, each grounded in actual generator/runtime code' },
    { title: 'Synthesize', detail: 'candidate shortlist + sequencing for S-P3' },
    { title: 'Harden', detail: '7-lens schema-free CHALLENGE to 3Z convergence' },
  ],
}

const P2 = 'restart/skinny/tranches/sk-v18/research/p2'
const P1 = 'restart/skinny/tranches/sk-v18/research/p1'
const AUDIT = 'restart/skinny/tranches/sk-v18/audit-overfit'

const COMMON =
  'You are a SK-V18 S-P2 (RESEARCH pass) candidate-architecture agent. SK-V18 is the GENERALIZATION cycle: backtrack TWO hand-written/forked parsers (JSON + CSS) into ONE grammar-driven generator that emits JSON + CSS + Sheets from .bbnf, preserving >SOTA (CSS beats lightningcss 1.66-3.38x; JSON beats sonic-rs strict). aarch64 / Apple M5 Max ONLY. '
  + 'You must GROUND every claim in the ACTUAL code (read the files; cite path:line). Do NOT run cargo. Do NOT write code — this is research: propose 2-3 CANDIDATE approaches per class with concrete trade-offs, name the RECOMMENDED candidate, and state risks + the prune/sequencing dependency. '
  + 'BINDING CONSTRAINTS (the 6 S-P0 addenda — read ' + AUDIT + '/SYNTHESIS-AUDIT-OVERFIT.md and ' + AUDIT + '/a1-six-addenda-lens-registry.md): (1) NO verbatim &str courier may masquerade as @generated — output must be grammar-DERIVED; (2) N grammars must yield N NON-identical generated.rs (md5-distinct AND zero grammar-named branches AND collapsed RuntimeTarget rows); (3) ONE grammar-agnostic emitter, no grammar-family fork; (4) no phantom generic <G> uninstantiated outside #[cfg(test)] — instantiate-or-delete, and the shared trait must NOT LCD-flatten JSON rich tree navigation; (5) timed-plane symmetry + corpus-in-timer; (6) any NEON/ASM primitive must reach the hot path at admission (no orphan kernel). '
  + 'PROFILE GROUND-TRUTH (read ' + P1 + '/SYNTHESIS-PROFILE.md): CSS hot leaf = CssFullParser::find_component_delim scalar scan (94.1% of parser self-time) -> G6 WIRE retarget after P3 collapse; JSON hot leaf = generated parse_object_value_at_direct direct-to-struct (~70%), json/scan.rs zero-sampled. '
  + 'KEY REAL FILES: skinny/crates/codegen/src/runtime_generator.rs (emit_from_request matches RuntimeEmitterKind; CSS const courier CSS_GENERATED_RS ~line 701; emit_compiled pushes JSON_PARSE_ONLY_GENERATED_RS const + json_sink_direct::render); skinny/crates/codegen/src/grammar_provider.rs:40-42 (enum RuntimeEmitterKind{CompiledLowering,RequestFacts}); skinny/crates/codegen/src/json_sink_direct.rs (JSON projection render + push_str blocks); skinny/crates/codegen/src/lower/ (the lowering module); skinny/crates/codegen/src/json_templates/ (the JSON byte-oracle); skinny/xtask/src/regen.rs (RuntimeTarget 12-field struct, the per-grammar config rows); skinny/crates/runtime/src/.../tape/mod.rs (the Tape substrate + phantom <G>); grammar/json/json.bbnf, grammar/css/l4/stylesheet.bbnf (+ l4 modules), grammar/google-sheets/google-sheets.bbnf; skinny/crates/runtime/src/grammars/sheets_witness/ (the existing Sheets witness scaffold). '

const CLASSES = [
  {
    key: 'R-A', label: 'R-A-emitter-unify', file: P2 + '/rA-emitter-unify.md',
    task: 'CLASS R-A — UN-FORK THE EMITTER (addendum 3; wave G3). Read runtime_generator.rs (emit_from_request / emit_compiled / emit_request_facts) and grammar_provider.rs (RuntimeEmitterKind). The fork is ASYMMETRIC: CompiledLowering (JSON) renders from a sink_only program via json_sink_direct::render, while RequestFacts (CSS) couriers a verbatim const. Propose 2-3 candidate architectures for ONE grammar-agnostic emit path that subsumes both, parameterized only by grammar metadata (NOT a grammar-family enum). Address: what becomes of RuntimeEmitterKind (delete? collapse to a data-driven projection spec?); how the single driver decides JSON-vs-CSS-vs-Sheets behaviour from the .bbnf/profile WITHOUT a grammar-named branch (addendum 2 row-collapse); the entry-gate (G1+G2 must close first). Recommend one; state the risk of a relocated-seam (a grammar-named branch hidden in a data table).',
  },
  {
    key: 'R-B', label: 'R-B-css-lowering', file: P2 + '/rB-css-lowering.md',
    task: 'CLASS R-B — CSS LOWERING FROM .bbnf (addendum 1; waves G2, the LARGEST). The CSS_GENERATED_RS const (~910 LOC verbatim recursive-descent CssFullParser, runtime_generator.rs ~line 701) must become GENERATED from grammar/css/l4/stylesheet.bbnf (+ l4 modules). Read the const courier body, the lower/ module, and json_sink_direct::render (the JSON analog that DOES derive). Propose 2-3 candidates for lowering the CssFullParser (find_component_delim scan, consume_balanced_at, parse_block/declaration/at_rule) from the grammar IR. Address: does the existing lower/ infrastructure reach CSS or is it JSON-only; how the 94.1%-hot scalar scan is preserved/improved (it must stay >=SOTA, NOT regress — preserve-rich-ast); how the generated CSS stays md5-distinct from JSON (addendum 2). Recommend one; flag if a grammar-derived parser CANNOT preserve >SOTA without hand-shaping (a genuine section-6 finding -> a named grammar-parameterized primitive, gated a-b-c).',
  },
  {
    key: 'R-C', label: 'R-C-json-projection', file: P2 + '/rC-json-projection.md',
    task: 'CLASS R-C — JSON PROJECTION FROM .bbnf (addendum 1; wave G1). Read json_sink_direct.rs (the render path + every push_str) and json_templates/ (the byte oracle). S-P0 R2 flags 7x push_str fixed-literal renders. Determine which push_str blocks are GRAMMAR-DERIVED vs HAND-WRITTEN fixed literals, and propose 2-3 candidates to make the projection fully grammar-derived (the SinkOnly program -> struct projection driven by grammar metadata). Address: the diff-control gate (the same-wave regen must byte-match json_templates/ BEFORE the oracle is deleted — that diff-match, NOT a +/-5% line delta, is the binding proof); preserve the ~70% hot parse_object_value_at_direct leaf (no regression). Recommend one.',
  },
  {
    key: 'R-D', label: 'R-D-value-api-trait', file: P2 + '/rD-value-api-trait.md',
    task: 'CLASS R-D — SHARED LAZY VALUE-API TRAIT (addendum 4; wave G4). Read the tape substrate (tape/mod.rs, incl. the phantom G: EventGrammar = AnyGrammar ~line 175), the JSON value navigation, and the CSS CssDocument/CssNode/CssTypedNode projection. JSON navigates a RICH tree; CSS projects a FLAT tape. Propose 2-3 candidates for a shared Value/Document/Cursor trait both implement OVER the existing single Tape, staying LAZY, with NO second substrate and NO LCD-flatten of JSON rich nav (>=2 real impls so the trait cannot collapse to the lesser). Address: instantiate-or-delete the phantom <G> (default DELETE; what real type instantiates it if kept); how laziness is preserved (cursor over offsets, not eager materialization); the preserve-rich-ast invariant. Recommend one.',
  },
  {
    key: 'R-E', label: 'R-E-sheets-proof', file: P2 + '/rE-sheets-proof.md',
    task: 'CLASS R-E — SHEETS THIRD-GRAMMAR PROOF (addendum 2; wave PROVE). Sheets = grammar/google-sheets/google-sheets.bbnf; a witness scaffold exists at skinny/crates/runtime/src/grammars/sheets_witness/. Read the grammar + the witness + the w5a_sheets test in codegen/src/lib.rs. The generalization is PROVEN only if a THIRD, structurally-distinct grammar emits a working parser THROUGH the un-forked generator (not JSON, not CSS). Propose 2-3 candidates for what Sheets must exercise to prove generality (what grammar constructs does google-sheets.bbnf have that JSON+CSS lack — formulas? cell refs? ranges?), and how its generated.rs is held md5-distinct + branch-free. Address: the proof is a NEGATIVE control — if Sheets needs a hand-written shim, the generator is NOT general. Recommend the minimal Sheets proof that is honest (no contrived constructs).',
  },
  {
    key: 'R-F', label: 'R-F-neutral-scan', file: P2 + '/rF-neutral-scan.md',
    task: 'CLASS R-F — G6 GRAMMAR-NEUTRAL SCAN PRIMITIVE (addendum 6; waves G5/G6). S-P1 mandates WIRE: retarget NEON to the live find_component_delim scan (94.1% CSS self-time) AFTER P3 collapse, as a SHARED grammar-neutral primitive (caller supplies the delimiter/significant set; the kernel is alphabet-data, not grammar-coupled). The dead kernels find_css_significant/find_comment_close (S-P0 R7) were written for DIFFERENT functions. Read the existing NEON kernels + the scalar find_component_delim + the JSON scan path. Propose 2-3 candidates for the grammar-neutral scan primitive the generator emits a call to, under dav1d discipline (scalar-reference FIRST + checkasm differential parity + aarch64 NEON/dotprod only, NO x86). Address: how the same primitive serves CSS (delimiter scan) AND JSON (structural/string scan) so it is genuinely neutral; the no-orphan-kernel law (land WITH the hot consumer same commit); the P1->P3->G6 sequencing. Recommend one.',
  },
]

phase('Research')
const digests = await parallel(
  CLASSES.map((c) => () =>
    agent(COMMON + c.task + ' Write your research digest to ' + c.file + '. Return a 12-line summary: the candidates, the recommendation, the key risk, and the prune/sequencing dependency.',
      { label: c.label, phase: 'Research' })
  )
)

phase('Synthesize')
const SYN =
  COMMON
  + 'TASK SYNTHESIS — the CANDIDATE SHORTLIST S-P3 turns into the wave manifest. Read all six digests: ' + CLASSES.map((c) => c.file).join(', ') + '. '
  + 'Consolidate into one research synthesis: (1) the recommended candidate per class (R-A..R-F) with its one-line justification; (2) the cross-class couplings (e.g. R-A un-fork depends on R-B+R-C closing; R-F depends on P3 collapse + R-B; R-D resolves the phantom); (3) the binding sequencing PRUNE(P1..P5) -> G1(JSON proj) -> G2(CSS lowering) -> G3(un-fork) -> G4(shared trait) -> G5/G6(neutral scan) -> PROVE(Sheets) -> H1(honesty), with each entry-gate; (4) any section-6 finding (a grammar-derived parser that cannot preserve >SOTA without a named, gated, grammar-parameterized primitive); (5) the residual research RISKS S-P3 must plan around. '
  + 'End with a one-line next-move: ready-for-S-P3. Write to ' + P2 + '/SYNTHESIS-RESEARCH.md. Return a 12-line executive summary.\n\n'
  + digests.map((d, i) => '--- ' + CLASSES[i].key + ' ---\n' + (d || '(missing)')).join('\n\n')
const syn = await agent(SYN, { label: 'synthesis-research', phase: 'Synthesize' })

// ---- HARDEN: schema-free 3Z CHALLENGE ----
function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}

const LENSES = [
  'CH1 GROUNDEDNESS: is every candidate grounded in actual cited path:line code, not abstract hand-waving? Flag any claim not traceable to a read file.',
  'CH2 ADDENDA-COMPLIANCE: does any recommended candidate violate the 6 addenda (verbatim courier, identical output, grammar-family fork, phantom/ LCD-flatten, timed-plane, orphan kernel)?',
  'CH3 SOTA-PRESERVATION: does any candidate risk regressing the >SOTA hot leaves (CSS find_component_delim 94.1%, JSON parse_object_value_at_direct ~70%) without a named gated primitive? Is preserve-rich-ast honored?',
  'CH4 SEQUENCING-SOUNDNESS: is the PRUNE->GENERALIZE->PROVE->HONESTY order correct, with P4-before-G2/G3 and P3-before-G6 and G1-blocks-downstream respected; no candidate dispatchable over an unclosed predecessor?',
  'CH5 SHEETS-PROOF-RIGOR: is the Sheets third-grammar a genuine NEGATIVE control (structurally distinct, no contrived constructs, no hand-written shim), proving generality rather than decorating it?',
  'CH6 OVERFIT-PRUNE (spine): does any candidate overfit to JSON or CSS specifics or smuggle a hand-written contrivance under a grammar-driven banner; is the ONE-generator framing preserved end to end?',
  'CH7 SECTION-6-HONESTY: if any class finds a grammar-derived parser cannot preserve >SOTA without hand-shaping, is that recorded as an honest named-primitive finding (gated a-INVOKED b-DERIVED c-no-verbatim-blob), never silently smuggled?',
]

function chPrompt(lens, v) {
  return 'You are a SK-V18 S-P2 CHALLENGE lens (cycle V' + v + '). Adversarially review the S-P2 research artifacts on disk: '
    + CLASSES.map((c) => c.file).join(', ') + ', ' + P2 + '/SYNTHESIS-RESEARCH.md — cross-checked against the ACTUAL code they cite and the S-P0 addenda (' + AUDIT + '/SYNTHESIS-AUDIT-OVERFIT.md) and the S-P1 profile (' + P1 + '/SYNTHESIS-PROFILE.md). '
    + 'YOUR LENS: ' + lens + ' '
    + 'Read the artifacts and spot-verify the cited code. Enumerate every recommended-candidate / sequencing / proof claim under your lens and judge each ACCEPT (sound + grounded), REVISE (fixable precision/grounding gap), or REJECT (false / addenda-violating / contrived). A REVISE names the file + exact single edit; a REJECT cites the falsifying code or addendum. '
    + 'End with EXACTLY one line (no backticks): TALLY accept=N revise=N reject=N'
}

phase('Harden')
let consec = 0, v = 0, voids = 0
while (consec < 2 && v < 5 && voids < 3) {
  v++
  const verdicts = await parallel(
    LENSES.map((L, i) => () => agent(chPrompt(L, v), { label: 'CH' + (i + 1) + '-v' + v, phase: 'Harden' }))
  )
  const tallies = verdicts.filter(Boolean).map(parseTally).filter(Boolean)
  if (tallies.length < 4) { voids++; log('S-P2 CHALLENGE v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('S-P2 CHALLENGE v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0) && v < 5) {
    const FOLD =
      COMMON
      + 'TASK FOLD (cycle V' + v + '). The S-P2 CHALLENGE raised ' + revise + ' REVISE and ' + reject + ' REJECT items. Re-read the seven research artifacts (rA..rF + SYNTHESIS-RESEARCH) against the cited code and the addenda through the SEVEN lenses, then APPLY each fix in place (edit the markdown). Fixes must be single-edit, mechanism-correct, grounded in real code; a REJECT may require replacing an unsound candidate recommendation with a compliant one (cite why). Do not invent code that does not exist. Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Harden' })
  }
}

const CONS =
  'You are the S-P2 consolidation agent. The schema-free CHALLENGE ran ' + v + ' cycle(s); converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read the seven S-P2 artifacts under ' + P2 + ' and write ' + P2 + '/HARDENING-S-P2-CONSOLIDATED.md recording: per-cycle convergence posture (r), the disposition of every REVISE/REJECT folded, the FINAL recommended candidate per class R-A..R-F, the binding sequencing, any section-6 finding, and the next-move ready-for-S-P3. Return a 10-line summary.'
const cons = await agent(CONS, { label: 'consolidate', phase: 'Harden' })

return { converged: consec >= 2, cycles: v, voids, consolidated: cons }
