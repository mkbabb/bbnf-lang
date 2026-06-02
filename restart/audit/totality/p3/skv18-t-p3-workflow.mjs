export const meta = {
  name: 'skv18-t-p3-synthesis',
  description: 'SK-V18 T-P3 totality synthesis: distil T-P1 + T-P2 into proposed V1-spec amendments — 3A architecture deltas / 3B master-plan reconciliation / 3C LOCKS v+1 crystallisation (21 candidates) / 3D skinny-fold / 3E grammar-generalisation / 3F migration+handoff — then 6-lens CHALLENGE to 3Z. Write-to-disk.',
  phases: [
    { title: 'Synthesize', detail: '3A..3F proposed-diff artefacts distilling T-P1+T-P2' },
    { title: 'Harden', detail: '6-lens schema-free CHALLENGE to 3Z' },
  ],
}

const OUT = 'restart/audit/totality/p3'
const P1 = 'restart/audit/totality/p1'
const P2 = 'restart/audit/totality/p2'
const SK = 'restart/skinny/tranches/sk-v18'

const COMMON =
  'You are a SK-V18 T-P3 (TOTALITY SYNTHESIS pass) agent. T-P3 distils the converged T-P1 evidence base (' + P1 + '/1A..1F + 8 LOCKS-AMENDMENTS-CANDIDATEs) and the converged T-P2 research dossiers (' + P2 + '/2A..2F + 13 candidates + 13 REFUTED architectural assertions) into PROPOSED V1-spec amendments. '
  + 'CRITICAL BOUNDARY: T-P3 PROPOSES — it writes proposed-diff documents under ' + OUT + ', NEVER the V1 governance surfaces themselves (restart/ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md, MIGRATION.md, HANDOFF.md are touched ONLY by Pass Omega CRUD, post-G-Omega). Writing a governance surface directly is a cross-scope violation. '
  + 'CONTEXT: SK-V18 is the GENERALIZATION cycle — ONE grammar-driven generator emitting JSON+CSS+Sheets (ultimately the 9 totality grammars) from .bbnf, preserving >SOTA, aarch64-only. The certified plan: ' + SK + '/SPEC.md (12-wave manifest) + ' + SK + '/research/p2/SYNTHESIS-RESEARCH.md. '
  + 'KEY T-P2 REFUTATIONS (constrain your amendments — the literature REFUTED these, so do NOT synthesise a delta that revives them): tree-walk does NOT preserve the 94.1% scan; find_css_significant wire-as-is; neutral-name-on-one-grammar proves neutrality; checkasm-PASS is a speedup; x86/AVX-512 closes an M5 Max row; eq-set dual-consumer neutrality; md5-distinctness alone proves the un-fork; bracket_depth_mask_64 replaces the scalar shell (REDRESS-fenced). '
  + 'GROUND every proposed delta in a cited T-P1 finding-id / T-P2 dossier / V1-surface path:line — an uncited delta is a REJECT. The skinny->totality fold is MONOTONIC (skinny informs totality, never reverse). No new directive / BIR variant / substrate is synthesised silently — disposition it explicitly. A prior totality cycle wrote your artefact; EXTEND it absorbing the SK-V18 findings. '
  + 'Emit the output-schema frontmatter (agent, pass, cycle, t_p1_inventories_consumed, t_p2_dossiers_consumed, v1_surface_targeted, proposed_deltas_count, delta_summary{carried/removed/answered/newly_added}) + body sections: Executive Summary (<=200 words), V{N} Delta Summary, Proposed Delta Table (delta | source T-P1/T-P2 finding-id | affected V1-surface section | rationale), Consequences (positive/cost/propagation), Open Questions (tagged to a CHALLENGE lens). '

const AGENTS = [
  { key: '3A', label: '3A-architecture', file: OUT + '/3A-architecture-synthesis.md',
    task: 'SCOPE 3A — ARCHITECTURE.md surface synthesis. EXTEND ' + OUT + '/3A-architecture-synthesis.md. Distil T-P1\'s substrate/codegen/runtime divergences (the 56-unimplemented generalization surface: un-forked generator absent, CSS_GENERATED_RS courier + 7 replicas, phantom <G>, marker-string lowerers, BIR 13/20) + T-P2\'s grounded techniques into a proposed-delta document for restart/ARCHITECTURE.md, section by section. Carry the 13 refuted-technique CONSEQUENCES into the surface (e.g. the §6 named-primitive (a)-(d) discipline is now ARCHITECTURE-authoritative because the literature refutes the naive tree-walk). The 5-shape BackendShape canon (§7.3) must stay coherent with 3B/3E. Every delta cites the T-P1 divergence-id or T-P2 grounding it answers.' },
  { key: '3B', label: '3B-master-plan', file: OUT + '/3B-master-plan-reconciliation.md',
    task: 'SCOPE 3B — MASTER-PLAN.md wave reconciliation. EXTEND ' + OUT + '/3B-master-plan-reconciliation.md. Audit restart/MASTER-PLAN.md §5 tranche set + §H waves against T-P1\'s implemented-vs-unimplemented census + the skinny REDRESS ledger. Classify every wave landed/refuted/pending/new. Propose wave allocations + any NEW wave implied by T-P1/T-P2 — in particular reconcile the SK-V18 12-wave generalization manifest (W-PRUNE P1-P5 -> G1..G6 -> PROVE -> H1, ' + SK + '/SPEC.md) into the master plan, and tee up SK-V19 (the totality-fold tranche: crates/core/ adoption + BBNF-self as the 4th-grammar litmus). A NEW wave carries a same-wave consumer.' },
  { key: '3C', label: '3C-locks', file: OUT + '/3C-locks-crystallisation.md',
    task: 'SCOPE 3C — LOCKS crystallisation (the G3 gate object, the singularity). EXTEND ' + OUT + '/3C-locks-crystallisation.md AND write ' + OUT + '/3C-locks-v+1-diff.md (the line-level LOCKS diff). Consolidate EVERY T-P1 1E candidate (8: LAC-1E-V5-01..07 + the 1A companion) AND every T-P2 2X candidate (13: 2C=3, 2D=4, 2E=3, 2F=3) into ONE v+1 LOCKS diff. Per candidate, EXACTLY one disposition ACCEPT/REJECT/MODIFY/DEFER — with the proposing agent, the affected LOCKS.md section, the supporting path:line evidence, and the rationale. SILENT DROPS FORBIDDEN (a dropped candidate is a CH1+CH6 REJECT). Load-bearing candidates to disposition first: the named-primitive (a)-(b)-(c)-(d) machine-checked gate (LAC-1E-V5-01); the relocated-seam firewall emit_shape_source==lowered_program (LAC-1E-V5-02). NEVER renumber a lock; the 16-lock count amends only by addition/retirement (G-Omega-gated). A DEFER names its re-entry trigger.' },
  { key: '3D', label: '3D-skinny-fold', file: OUT + '/3D-skinny-fold.md',
    task: 'SCOPE 3D — skinny->totality fold synthesis. EXTEND ' + OUT + '/3D-skinny-fold.md. Distil T-P1\'s 1D skinny-lessons digest into the durable totality fold: which SK-V1..V18 WINS become V1-spec-authoritative (the unified tape/ValueRef substrate, the canonical cold bench, the >SOTA cold proofs, the one-generator inflection finding); which SK rejections become locks-strengthening evidence (cross-ref 3C — the REDRESS-fenced routes: eager value-tree 118x, StructRegistry, fact-stream-as-output, bracket_depth_mask, x86); which non-JSON generalization gaps the totality spec must absorb (cross-ref 3E). MONOTONIC — skinny informs totality, never reverse.' },
  { key: '3E', label: '3E-grammar-general', file: OUT + '/3E-grammar-generalisation.md',
    task: 'SCOPE 3E — grammar-generalisation synthesis. EXTEND ' + OUT + '/3E-grammar-generalisation.md. Distil T-P2\'s 2C grammar-neutrality research into the non-JSON generality story: the per-grammar BackendShape matrix across the 9 totality grammars (bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math); the primitive-vocabulary transfer to CSS L4 / Sheets / BBNF-self; the future-grammar onboarding test. Surface every Lock 14 hardening clause the T-P1/T-P2 evidence warrants (cross-ref 3C): the §6 named-primitive (a)-(d) gate as the generality-preserving discipline; the css_balanced_component_scan FORCED CSS-scoped demotion (the literature refuted neutral-name-on-one-grammar); the Sheets precedence-tower negative control.' },
  { key: '3F', label: '3F-migration-handoff', file: OUT + '/3F-migration-handoff.md',
    task: 'SCOPE 3F — MIGRATION + HANDOFF + next-cycle dispatch. EXTEND ' + OUT + '/3F-migration-handoff.md. Synthesise the rename/abrogate/refactor decisions T-P1/T-P2 surfaced into a proposed restart/MIGRATION.md delta (the x86 crate-wide deletion, the CSS courier retirement, the 7-replica collapse, the phantom <G> delete, the css_types.rs generic-core relocation); synthesise the proposed restart/HANDOFF.md top-level state delta (SK-V18 generalization cycle: skinny S-P0..S-P3 certified + totality T-P1..T-P3 converged, ready for Pass Omega + wave implementation); author the next-cycle dispatch directive with concrete measurable entry conditions (W-PRUNE P1-P5 dispatch-eligible FIRST per the SPEC; SK-V19 totality-fold tee-up).' },
]

phase('Synthesize')
const arts = await parallel(
  AGENTS.map((a) => () =>
    agent(COMMON + a.task + ' Write the artefact to ' + a.file + '. Return a 10-line summary: proposed_deltas_count + the top 3 deltas + (3C only) the ACCEPT/REJECT/MODIFY/DEFER tally over the 21 candidates.',
      { label: a.label, phase: 'Synthesize' })
  )
)
log('T-P3 synthesis complete (' + arts.filter(Boolean).length + '/6 artefacts landed)')

function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}
const LENSES = [
  'CH1 CORRECTNESS: every proposed delta cites a real T-P1 finding-id or T-P2 grounding; every cited V1-surface section resolves at path:line; 3C\'s disposition matrix references real amendment candidates and dispositions ALL 21 (8 T-P1 + 13 T-P2) with no silent drop; the 3C-locks-v+1-diff applies cleanly to the current LOCKS.md (16-lock count, no renumber).',
  'CH2 GENERALITY: Lock 14 holds — 3A surface deltas + 3B waves generalise to non-JSON; 3E\'s story is concrete for CSS L4 / Sheets / BBNF-self / the 9-grammar fleet; 3C accepts no JSON/CSS-narrowing amendment; the future-grammar onboarding test survives.',
  'CH3 REGRESSION: no delta re-opens a skinny/REDRESS.md route; 3B revives no refuted wave; 3D promotes no rejected route; 3C weakens no lock that REDRESS strengthened; no delta revives one of the 13 T-P2-refuted assertions.',
  'CH4 COST: every delta states a LOC budget, a propagation cost (surfaces touched), a risk class, a wave alignment; 3B NEW waves carry a same-wave consumer; 3C dispositions are realistic.',
  'CH5 HIDDEN-COUPLING: no delta implies a parallel substrate / sidecar producer / renamed-scanner Lock-1 violation / Track1≡Track2 dishonesty; the substrate union holds across every 3A delta; 3C\'s accepted amendments introduce no coupling.',
  'CH6 ANTI-PAPER-CLOSE: no artefact claims a delta "validated" without the T-P1/T-P2 evidence chain; no delta deferred to "a future cycle" without a named receiver + blocker + receiving gate; 3C DEFER dispositions name the re-entry trigger; 3F next-cycle directive specifies concrete measurable entry conditions.',
]
function chPrompt(lens, v) {
  return 'You are a SK-V18 T-P3 (totality synthesis) CHALLENGE lens (cycle V' + v + '). Adversarially review the 6 synthesis artefacts under ' + OUT + ' (3A-architecture, 3B-master-plan, 3C-locks-crystallisation + 3C-locks-v+1-diff, 3D-skinny-fold, 3E-grammar-generalisation, 3F-migration-handoff) against the T-P1 evidence (' + P1 + '), the T-P2 dossiers (' + P2 + '), and the V1 surfaces (restart/ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md, MIGRATION.md). '
    + 'YOUR LENS: ' + lens + ' '
    + 'Spot-verify the most load-bearing deltas (a cited finding-id resolves; a cited LOCKS section exists; the v+1 diff applies). Enumerate every proposed delta / disposition under your lens; judge each ACCEPT / REVISE (name the artefact + the exact correction) / REJECT (an uncited delta / a revived refuted-route / a silent-dropped candidate / a cross-scope violation + the evidence). Cycle V1 expects >=30% REVISE. '
    + 'Write your verdict to ' + OUT + '/hardening/V' + v + '/CH' + (LENSES.indexOf(lens) + 1) + '.md. End your reply and the file with EXACTLY one line, no backticks: TALLY accept=N revise=N reject=N'
}

phase('Harden')
let consec = 0, v = 0, voids = 0
while (consec < 2 && v < 5 && voids < 3) {
  v++
  const verdicts = await parallel(
    LENSES.map((L) => () => agent(chPrompt(L, v), { label: 'CH' + (LENSES.indexOf(L) + 1) + '-v' + v, phase: 'Harden' }))
  )
  const tallies = verdicts.filter(Boolean).map(parseTally).filter(Boolean)
  if (tallies.length < 4) { voids++; log('T-P3 CHALLENGE v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('T-P3 CHALLENGE v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0)) {
    const FOLD =
      COMMON
      + 'TASK FOLD (cycle V' + v + '). The T-P3 CHALLENGE lenses wrote verdicts to ' + OUT + '/hardening/V' + v + '/CH1..CH6.md against the 6 artefacts. Read all verdict files; for every REJECT correct the uncited/revived/cross-scope delta or add the missing candidate disposition, for every REVISE add the missing evidence/cost/cross-ref; APPLY each fix IN PLACE editing the named artefact(s). Every delta must cite its evidence chain; 3C must disposition all 21 candidates. Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Harden' })
  }
}

const CONS =
  'You are the T-P3 aggregator. The schema-free CHALLENGE ran ' + v + ' cycle(s); converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read the 6 artefacts under ' + OUT + ' + the per-cycle verdicts under ' + OUT + '/hardening and write ' + OUT + '/hardening/HARDENING-T-P3-CONSOLIDATED.md: the per-cycle r, the proposed-deltas census (per surface), the 3C disposition tally (ACCEPT/REJECT/MODIFY/DEFER over the 21 candidates) + the load-bearing accepted lock amendments, the disposition of every REVISE/REJECT folded, and the G3-gate readiness (the 3C-locks-v+1-diff is the gate object the user authorizes at G-Omega). Set next-move = ready-for-G3/Pass-Omega. Return a 12-line summary incl. the 3C disposition tally + the accepted lock amendments + the next-cycle (SK-V19) tee-up.'
const cons = await agent(CONS, { label: 'aggregate', phase: 'Harden' })

return { converged: consec >= 2, cycles: v, voids, landed: arts.filter(Boolean).length, consolidated: cons }
