export const meta = {
  name: 'skv18-s-p3-plan',
  description: 'SK-V18 S-P3 synthesis-plan: independent re-validation of S-P2 + build the wave manifest SPEC.md from the candidate shortlist + schema-free 3Z CHALLENGE. Stall-resilient: every agent writes its deliverable to disk.',
  phases: [
    { title: 'Ground', detail: 'independent re-validation of the S-P2 recommendations (clean provenance)' },
    { title: 'Plan', detail: 'wave-manifest sections PA..PE + SPEC.md assembly' },
    { title: 'Harden', detail: '7-lens schema-free CHALLENGE on the SPEC to 3Z' },
  ],
}

const T = 'restart/skinny/tranches/sk-v18'
const P3 = T + '/research/p3'
const P2 = T + '/research/p2'
const P1 = T + '/research/p1'
const AUDIT = T + '/audit-overfit'

const COMMON =
  'You are a SK-V18 S-P3 (synthesis-PLAN pass) agent. SK-V18 is the GENERALIZATION cycle: backtrack two hand-written/forked parsers (JSON+CSS) into ONE grammar-driven generator emitting JSON+CSS+Sheets from .bbnf, preserving >SOTA (CSS beats lightningcss 1.66-3.38x; JSON beats sonic-rs strict). aarch64 / Apple M5 Max ONLY. '
  + 'S-P3 turns the CONVERGED S-P2 candidate shortlist into the binding wave manifest (SPEC.md) — entry/exit gates, falsifiers, telemetry, caps. This is NOT an implementation; it is the executable plan. '
  + 'BINDING INPUTS (read what your task needs): ' + P2 + '/SYNTHESIS-RESEARCH.md (the candidate shortlist — recommended R-A..R-F, the cross-class lattice, the PRUNE->G1..G6->PROVE->H1 sequencing with per-wave exit-gates, the §6 findings, the residual risks); ' + AUDIT + '/SYNTHESIS-AUDIT-OVERFIT.md (the 6 addenda + the PRUNE-list P1..P5 + R16); ' + P1 + '/SYNTHESIS-PROFILE.md (the hot leaves: CSS find_component_delim 94.1%, JSON parse_object_value_at_direct ~91.5%, G6=WIRE); restart/skinny/tranches/sk-v17/SPEC.md (the STRUCTURE TEMPLATE — Section 0 close-condition/comparators/outcome-enum/telemetry, Section 1 non-negotiables, Section 2 wave manifest + 2.1 Lock-14 gate, then per-wave sections). '
  + 'Do NOT run cargo. GROUND every gate in the S-P2 sequencing. Write your deliverable to the named file (that file IS your primary output; resume depends on it). '

phase('Ground')
// Independent re-validation of the S-P2 recommendations that were orchestrator-applied in V2.
const GROUND_LENSES = [
  { key: 'sota', label: 'reval-sota', q: 'SOTA-PRESERVATION: does any recommended candidate (R-A..R-F) risk regressing the >SOTA hot leaves (CSS find_component_delim 94.1%, JSON ~91.5%) without an honest (a)-(d)-gated named primitive? Is the G2 explicit >SOTA-regression gate (track1_rich/lightningcss >= S-P1 ratio) sound? Is preserve-rich-ast honored?' },
  { key: 'seq', label: 'reval-seq', q: 'SEQUENCING-SOUNDNESS: is the PRUNE->G1..G6->PROVE->H1 lattice acyclic and complete (P4-before-G2/G3, P3-before-G6/G2, G1-blocks-downstream, PROVE on G3∧G4 parallel to G5/G6)? Any circular dependency or a wave dispatchable before its predecessor closes? Are the per-wave falsifiers RED-able?' },
  { key: 's6', label: 'reval-s6', q: 'SECTION-6-HONESTY: are the §6 named primitives (CSS balanced scan, JSON leaf kernels) gated (a)-(b)-(c)-(d) — grammar-invoked, varies-under-mutation, no verbatim blob, AND profile-proven-narrow-leaf with walk-derived skeleton — so the escape cannot admit a relabeled blob? Is the balanced_component_scan neutrality-proof obligation present?' },
]
const groundVerdicts = await parallel(
  GROUND_LENSES.map((L) => () =>
    agent(
      COMMON + 'TASK GROUND/' + L.key + ' — independently re-challenge the COMMITTED S-P2 synthesis (' + P2 + '/SYNTHESIS-RESEARCH.md) under this lens (these were orchestrator-applied in S-P2 V2 under infra load; this is the clean independent re-validation). LENS: ' + L.q + ' '
      + 'Read the synthesis (and spot-check the cited code only if a defect is suspected). Enumerate the claims under your lens; judge each ACCEPT / REVISE (file + exact edit) / REJECT (falsifying evidence). '
      + 'Write your verdict to ' + P2 + '/hardening/V5-independent/' + L.key + '.md. End your reply and the file with EXACTLY one line, no backticks: TALLY accept=N revise=N reject=N',
      { label: L.label, phase: 'Ground' }
    )
  )
)
const groundText = groundVerdicts.map((v, i) => '--- ' + GROUND_LENSES[i].key + ' ---\n' + (v || '(dropped)')).join('\n\n')
log('S-P3 GROUND re-validation complete (' + groundVerdicts.filter(Boolean).length + '/3 lenses landed)')

phase('Plan')
const WAVES = [
  { key: 'PA', label: 'plan-prune', file: P3 + '/pa-prune-waves.md',
    task: 'PLAN PA — the PRUNE cluster (P1..P5) wave specs. For EACH prune (P1 delete x86 crate-wide incl. checkasm decouple; P2 delete warm micro-fixture CSS bench; P3 collapse 7 css_l4 replicas + RuntimeTarget row-collapse; P4 fix Lock-14 green-by-exclusion BEFORE G2/G3; P5 purge metalang leak), specify: entry-gate, exit-gate falsifier (the concrete grep/test that turns RED), LOC delta estimate, impl/redress cap, and the binding sequencing note. Ground each in AUDIT/SYNTHESIS-AUDIT-OVERFIT.md §4 + a2-prune-sequencing.md. P-cluster lands FIRST (zero generalization risk).' },
  { key: 'PB', label: 'plan-g1g2', file: P3 + '/pb-g1-g2-waves.md',
    task: 'PLAN PB — G1 (JSON projection) + G2 (CSS lowering) wave specs. G1: SinkOnlyExpr AST-walk emitter (R-C C1); exit = byte-equivalence of regenerated generated.rs vs json_templates/ oracle BEFORE deletion + the .bbnf-mutation falsifier + verbatim_blob_present==false; the leaf kernels as (a)-(d)-gated primitives (the (b) = byte-set/class mutation). G2: balanced_component_scan as the §6 named primitive (R-B); exit = CSS generated.rs grammar-DERIVED (CSS_GENERATED_RS grep==0) + the per-primitive (a)-(c) mutate-falsifier + cssparser oracle parity + the EXPLICIT >SOTA gate (track1_rich/lightningcss >= S-P1 ratio, corpus-in-timer) + the balanced_component_scan neutrality-proof obligation. Entry-gates per the lattice (G1: P-cluster; G2: G1∧P3∧P4). Specify falsifiers, caps, the named-primitive contracts. Ground in P2/SYNTHESIS-RESEARCH §3/§4 + rB/rC.' },
  { key: 'PC', label: 'plan-g3g4', file: P3 + '/pc-g3-g4-waves.md',
    task: 'PLAN PC — G3 (un-fork emitter) + G4 (shared trait + phantom) wave specs. G3: DELETE RuntimeEmitterKind, dispatch on lowered BackendShape (R-A A); exit = emitter_fork_present==false ∧ generator_grammar_branch_count==0 ∧ generator_grammar_type_count==0 ∧ runtime_target_rows_collapsed==true ∧ emit_shape_source==lowered_program (render(program) reads NO target.* field). Bind the R16 recipe: RuntimeTarget: PartialEq full-row derive (+1 line; recurses into BOTH nested structs frontend_requirements field#11 AND output_labels field#12). G4: thin Cursor micro-trait over ValueRef<K> + DELETE the phantom <G>; exit = phantom_generic_resolved==deleted ∧ json_rich_navigation_preserved==true ∧ >=2 real impls that cannot LCD-collapse. Entry-gates (G3: G1∧G2∧P4∧P3; G4: G1∧G2∧G3). Specify falsifiers, caps. Ground in P2/SYNTHESIS-RESEARCH §1/§2/§3 + rA/rD.' },
  { key: 'PD', label: 'plan-g5g6-prove-h1', file: P3 + '/pd-g5g6-prove-h1-waves.md',
    task: 'PLAN PD — G5/G6 (neutral scan) + PROVE (Sheets) + H1 (honesty) wave specs. G5/G6: retarget the checkasm-gated bbnf-simd kernel onto find_component_delim (R-F A, WIRE per S-P1) under dav1d discipline (scalar-reference FIRST + checkasm differential parity + aarch64 NEON/dotprod ONLY, NO x86); exit = acceleration_at_admission==admission (generated-caller census non-empty, NOT #[cfg(test)]) + the neon_significant_skip_matches_scalar guard over the REAL 71KB-495KB corpora + the timed-plane binding (checkasm PASS/FAIL pre-H1, Mbps from corpus-in-timer); G5 = neutralize the zero-sampled json/scan.rs. PROVE: Sheets (google-sheets.bbnf) emits THROUGH the un-forked generator (R-E-2 precedence-tower core, the SOLE distinctive construct; Nu8 is shared); exit = md5-distinct Sheets generated.rs + no Sheets _RS blob + sheets_grammar_shape==pratt-operator + the Sheets value type instantiates the G4 trait + the import-closure data-relaxation (NOT a match-grammar arm); the binding fallback (if Sheets needs a shim, generalization is NOT real — surface honestly). H1: CSS framing honesty (lazy-rich-vs-eager-cssom disclosed) + corpus_in_timer==true + regen --check clean. Entry-gates (G5/G6: P1∧P3∧G3∧profile; PROVE: G3∧G4, PARALLEL to G5/G6; H1: G5/G6∧PROVE). Specify falsifiers, caps. Ground in P2/SYNTHESIS-RESEARCH §3 + rE/rF.' },
  { key: 'PE', label: 'plan-gate-telemetry', file: P3 + '/pe-gate-telemetry-close.md',
    task: 'PLAN PE — the cross-cutting Section 0/1/2.1 of the SPEC: (0) the GLOBAL close condition (one generator emits JSON+CSS+Sheets from .bbnf; shared value-API trait both instantiate; phantom <G> resolved; >SOTA preserved honestly; x86 gone; Lock-14 gate meaningful; net ~-10800 LOC), the comparator classes (CSS lazy-rich-vs-eager-cssom honesty, JSON strict-vs-sonic), the OUTCOME enum, the REQUIRED telemetry columns the executable --skv18-generalization-report gate-consumer reports (verbatim_blob_present, generator_grammar_branch_count, generator_grammar_type_count, runtime_target_rows_collapsed, emit_shape_source, emitter_fork_present, phantom_generic_resolved, json_rich_navigation_preserved, acceleration_at_admission, sheets_grammar_shape, generator_grammar_count, corpus_in_timer, materialization_framing); (1) the NON-NEGOTIABLES (the 6 addenda as standing law + the §6 (a)-(d) gate + dav1d discipline + aarch64-only + preserve-rich-ast + clean-regen); (2.1) the Generality + Lock-14 gate EVERY wave carries. Ground in AUDIT/SYNTHESIS-AUDIT-OVERFIT + P2/SYNTHESIS-RESEARCH + the SK-V17 SPEC Section 0/1/2.1 template.' },
]
const planParts = await parallel(
  WAVES.map((w) => () =>
    agent(COMMON + w.task + ' Write the section to ' + w.file + '. Return a 8-line summary of the wave specs you produced.',
      { label: w.label, phase: 'Plan' })
  )
)

const ASM =
  COMMON
  + 'TASK SPEC-ASSEMBLER — assemble the binding SK-V18 wave manifest SPEC.md from the five section files: ' + WAVES.map((w) => w.file).join(', ') + '. '
  + 'Produce ' + T + '/SPEC.md modeled on the SK-V17 SPEC structure: Section 0 (close condition, comparators, outcome enum, required telemetry) from PE; Section 1 (non-negotiables) from PE; Section 2 (the wave manifest TABLE: wave | name | dispatch status | LOC budget | cap, then the entry/exit-gate per wave) + Section 2.1 (Generality + Lock-14 gate) from PA-PE; then per-wave Sections (W-PRUNE P1..P5, then G1, G2, G3, G4, G5/G6, PROVE, H1) drawing the specs from PA-PD. '
  + 'The wave count + caps must respect the <=12 skinny ceiling. Each wave row cites its entry-gate, exit-gate falsifier, LOC budget, cap. End SPEC.md with the close condition restated + next-move (ready-for-T-P1 totality fold OR ready-for-wave-implementation). '
  + 'ALSO incorporate the GROUND re-validation findings below (fold any REVISE/REJECT into the relevant wave spec):\n\n' + groundText + '\n\n'
  + 'Write ' + T + '/SPEC.md. Return a 12-line executive summary: the wave list, the close condition, and whether the GROUND re-validation surfaced anything.'
const specSummary = await agent(ASM, { label: 'spec-assembler', phase: 'Plan' })

// ---- HARDEN: schema-free 3Z CHALLENGE on the SPEC ----
function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}
const LENSES = [
  'CH1 GATE-FALSIFIABILITY: does every wave exit-gate name a CONCRETE falsifier (a grep/test/bench that turns RED), not a prose assertion? Flag any unfalsifiable gate.',
  'CH2 SEQUENCING: is the wave manifest order + entry-gates consistent with the S-P2 lattice (P4-before-G2/G3, P3-before-G6/G2, G1-blocks-downstream, PROVE on G3∧G4 parallel to G5/G6)? Any wave dispatchable before its predecessor closes?',
  'CH3 ADDENDA-LAW: does every wave preserve the 6 addenda + the §6 (a)-(d) gate + the >SOTA gate + dav1d/aarch64-only + preserve-rich-ast? Any wave that could admit a courier/fork/phantom/orphan-kernel?',
  'CH4 TELEMETRY-COMPLETENESS: do the required telemetry columns + the gate-consumer cover every close-condition predicate, with each column produced by a named wave? Any close predicate with no telemetry?',
  'CH5 SOTA-PRESERVATION: is the >SOTA-regression gate bound at G2 AND G6 (corpus-in-timer), with the hot leaves preserved as gated primitives; no wave regresses the 94.1%/91.5% leaves silently?',
  'CH6 OVERFIT-PRUNE (spine): does the SPEC preserve the ONE-generator framing end to end; the named primitives grammar-parameterized + neutrality-proven; Sheets a genuine negative control; nothing overfit to JSON/CSS?',
  'CH7 CLOSE-CONDITION-HONESTY: is the close condition the honest generalization goalset (one generator, >SOTA preserved with the lazy-rich framing disclosed, x86 gone, net LOC negative); the §6 fallback binding (if a parser cannot be grammar-derived without a shim, surface honestly)?',
]
function chPrompt(lens, v) {
  return 'You are a SK-V18 S-P3 CHALLENGE lens (cycle V' + v + '). Adversarially review the wave manifest ' + T + '/SPEC.md against the S-P2 sequencing (' + P2 + '/SYNTHESIS-RESEARCH.md §3) and the addenda (' + AUDIT + '/SYNTHESIS-AUDIT-OVERFIT.md). '
    + 'YOUR LENS: ' + lens + ' '
    + 'Read SPEC.md (and the section files under ' + P3 + ' if needed). Enumerate every wave-gate/telemetry/close claim under your lens; judge each ACCEPT / REVISE (name the SPEC section + exact edit) / REJECT (the unfalsifiable gate / broken sequence / addenda violation + evidence). '
    + 'End with EXACTLY one line, no backticks: TALLY accept=N revise=N reject=N'
}

phase('Harden')
let consec = 0, v = 0, voids = 0
while (consec < 2 && v < 5 && voids < 3) {
  v++
  const verdicts = await parallel(
    LENSES.map((L, i) => () => agent(chPrompt(L, v), { label: 'CH' + (i + 1) + '-v' + v, phase: 'Harden' }))
  )
  const tallies = verdicts.filter(Boolean).map(parseTally).filter(Boolean)
  if (tallies.length < 4) { voids++; log('S-P3 CHALLENGE v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('S-P3 CHALLENGE v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0) && v < 5) {
    const FOLD =
      COMMON
      + 'TASK FOLD (cycle V' + v + '). The S-P3 CHALLENGE raised ' + revise + ' REVISE and ' + reject + ' REJECT items against ' + T + '/SPEC.md. Re-derive the gaps by re-reading SPEC.md through the SEVEN lenses (gate-falsifiability, sequencing, addenda-law, telemetry-completeness, sota-preservation, overfit-prune, close-condition-honesty), then APPLY each fix in place (edit SPEC.md). Fixes must be single-edit, mechanism-correct, grounded in the S-P2 sequencing; a REJECT may require replacing an unfalsifiable gate with a concrete falsifier. Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Harden' })
  }
}

const CONS =
  'You are the S-P3 consolidation agent. The schema-free CHALLENGE ran ' + v + ' cycle(s); converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read ' + T + '/SPEC.md + the section files under ' + P3 + ' and write ' + P3 + '/HARDENING-S-P3-CONSOLIDATED.md: the per-cycle convergence posture, the disposition of every REVISE/REJECT folded, the GROUND re-validation outcome, the final wave manifest summary, and the next-move (ready-for-T-P1 totality fold). Return a 10-line summary.'
const cons = await agent(CONS, { label: 'consolidate', phase: 'Harden' })

return { converged: consec >= 2, cycles: v, voids, groundLanded: groundVerdicts.filter(Boolean).length, spec: specSummary, consolidated: cons }
