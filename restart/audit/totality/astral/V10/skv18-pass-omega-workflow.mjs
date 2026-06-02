export const meta = {
  name: 'skv18-pass-omega-v10',
  description: 'SK-V18 Pass Omega V10 astral synthesis: consume the converged T-P1/T-P2/T-P3 + SK-V18 skinny lessons into the corpus-cohesion synthesis + STAGED CRUD diffs (locks-diff, master-plan-diff, CRUD-LOG plan) for the G-Omega gate. 6 Ω-agents -> 6-lens CHALLENGE to 3Z. NO governance-surface merge (post-G-Omega). Write-to-disk.',
  phases: [
    { title: 'Astral', detail: 'Ω-A..Ω-F corpus-cohesion synthesis + staged CRUD diffs' },
    { title: 'Harden', detail: '6-lens schema-free CHALLENGE to 3Z' },
  ],
}

const V = 'restart/audit/totality/astral/V10'
const P3 = 'restart/audit/totality/p3'
const P1 = 'restart/audit/totality/p1'
const P2 = 'restart/audit/totality/p2'
const SK = 'restart/skinny/tranches/sk-v18'

const COMMON =
  'You are a SK-V18 PASS OMEGA (astral synthesis, cycle V10) agent. Pass Omega is the astral layer ABOVE the three converged totality passes: it consumes T-P1 excavation (' + P1 + '), T-P2 research (' + P2 + '), T-P3 synthesis (' + P3 + '/3A..3F — the 93 proposed deltas + the 3C-locks-v+1-diff) AND the SK-V18 skinny lessons, and produces the corpus-cohesion synthesis + the STAGED CRUD diffs that the user authorizes at the MANDATORY G-Omega gate. '
  + 'CRITICAL BOUNDARY: this convergence pass PRODUCES STAGED DIFFS ONLY — it does NOT merge any governance surface (restart/ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md, HANDOFF.md, MIGRATION.md). The actual CRUD merge executes POST-G-Omega, after user sign-off. Write your artefact + any staged diff under ' + V + '/; never edit a live governance surface. '
  + 'CONTEXT: SK-V18 is the GENERALIZATION cycle — skinny S-P0..S-P3 CERTIFIED (one grammar-driven generator emitting JSON+CSS+Sheets from .bbnf, >SOTA preserved honestly, x86 deleted, net ~-10800 LOC, the §6 named-primitive (a)-(d) discipline literature-validated by T-P2) + totality T-P1/T-P2/T-P3 CONVERGED. The certified plan: ' + SK + '/SPEC.md (12-wave manifest). '
  + 'T-P3 already disposed all 21 amendment candidates (9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER) into the 3C-locks-v+1-diff (git apply --check exit 0 against live LOCKS.md). DO NOT re-derive T-P3 — CONSUME it; your job is corpus-cohesion + final CRUD staging. The 16-lock count + 5 BackendShape variants are preserved (amendment by addition, no renumber). GROUND every claim at path:line. '

const AGENTS = [
  { key: 'OA', label: 'OmegaA-coherence', file: V + '/ΩA-coherence-audit.md',
    task: 'SCOPE Ω-A — V1 SPEC COHERENCE AUDIT. Cross-document audit: does every claim in ARCHITECTURE.md / MASTER-PLAN.md / HANDOFF.md / MIGRATION.md cite file:line or SHA; does every LOCKS reference cite the lock at file:line; is every spec<->implementation pairing verified? Cross-check the T-P3 3A architecture deltas + 3B master-plan deltas for coherence against the LIVE spec surfaces — flag any delta that would create a cross-document inconsistency when merged. Verify the 5-shape BackendShape canon stays coherent across ARCHITECTURE §7.3 + MASTER-PLAN §H + skinny COMPILER.md after the proposed deltas. Output the coherence audit + the list of cohesion fixes the CRUD must apply.' },
  { key: 'OB', label: 'OmegaB-skinny-lessons', file: V + '/ΩB-skinny-lessons.md',
    task: 'SCOPE Ω-B — SKINNY LESSONS DIGEST. Walk SK-V1..SK-V18 master docs + REDRESS + RESULTS. Extract the load-bearing wins + rejections per iteration + the longitudinal trajectory for the ARCHITECTURE §implementation-status update. SK-V18 focus: the inflection-point finding (impl was hand-written/forked, now generalized); the one-generator architecture; the §6 named-primitive discipline; the G6=WIRE profile (find_component_delim 94.1%); the prune findings. Consume the T-P3 3D skinny-fold. Produce the digest the ARCHITECTURE implementation-status CRUD consumes.' },
  { key: 'OC', label: 'OmegaC-locks', file: V + '/ΩC-locks-amendments.md',
    task: 'SCOPE Ω-C — LOCKS AMENDMENTS (the G-Omega gate object). CONSOLIDATE the T-P3 3C-locks-v+1-diff (the converged 11-clause SK-V18 addendum disposing all 21 candidates) into the FINAL staged locks diff at ' + V + '/locks-diff.md (copy/refine the T-P3 3C-locks-v+1-diff, re-verify git apply --check exit 0 against live LOCKS.md, confirm the 16-lock count + 5 BackendShape variants preserved + no renumber + the 2 PLANNED co-gate symbols written as planned not live). Write ' + V + '/ΩC-locks-amendments.md summarizing the disposition (9A/11M/0R/1D), the load-bearing accepted clauses (the named-primitive (a)-(d) gate, the relocated-seam firewall), and the DEFER re-entry trigger. This is the diff the user reads at G-Omega before authorizing the LOCKS merge. STAGED ONLY — do not touch live LOCKS.md.' },
  { key: 'OD', label: 'OmegaD-master-plan', file: V + '/ΩD-master-plan-reconciliation.md',
    task: 'SCOPE Ω-D — MASTER-PLAN RECONCILIATION. Consume the T-P3 3B master-plan reconciliation. Audit MASTER-PLAN.md §H tranche against landed work (SK-V14..V18 commit SHAs) — waves landed/refuted/pending/new. Produce the STAGED master-plan diff at ' + V + '/master-plan-diff.md: reconcile the SK-V18 12-wave generalization manifest into §H, mark SK-V17 waves landed, tee up SK-V19 (the 9-grammar crates/core/ totality-fold + BBNF-self litmus). A NEW wave carries a same-wave consumer. STAGED ONLY.' },
  { key: 'OE', label: 'OmegaE-skinny-corpus', file: V + '/ΩE-skinny-corpus.md',
    task: 'SCOPE Ω-E — SKINNY CORPUS ALIGNMENT. Audit restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md for outdated references, stale cohort citations, missing SK-V18 anchors, drift from V1 terminology after the generalization. Produce the per-surface STAGED diff (what the CRUD-5 will apply): sync to the SK-V18 anchors (the certified SPEC, the one-generator architecture, the §6 discipline, the bench bit-rot fix track1_rich, the G6=WIRE finding). STAGED ONLY.' },
  { key: 'OF', label: 'OmegaF-migration-handoff', file: V + '/ΩF-migration-handoff.md',
    task: 'SCOPE Ω-F — MIGRATION + HANDOFF + next-cycle directive. Consume the T-P3 3F. Produce the STAGED MIGRATION.md delta (x86 crate-wide delete, CSS courier retirement, 7-replica collapse, phantom <G> delete, css_types.rs relocation) + the STAGED HANDOFF.md top-level state delta (SK-V18 generalization: skinny S-P0..S-P3 certified + totality T-P1..T-P3 converged + Pass Omega V10 staged, G-Omega pending; on G-Omega close -> W-PRUNE implementation) + the next-cycle dispatch directive with concrete measurable entry conditions (W-PRUNE P1-P5 dispatch-eligible FIRST; SK-V19 totality-fold tee-up). Write the staged diffs into ' + V + '/. STAGED ONLY.' },
]

phase('Astral')
const arts = await parallel(
  AGENTS.map((a) => () =>
    agent(COMMON + a.task + ' Write to ' + a.file + ' (+ the named staged diff file under ' + V + '/ where specified). Return a 10-line summary.',
      { label: a.label, phase: 'Astral' })
  )
)
log('Pass Omega V10 astral synthesis complete (' + arts.filter(Boolean).length + '/6 artefacts landed)')

function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}
const LENSES = [
  'CH1 CORRECTNESS: does every cited file:line/SHA resolve; does every REDRESS reference match content; does the staged locks-diff apply cleanly (git apply --check exit 0) to live LOCKS.md; does the master-plan-diff cite real §H waves + real SHAs?',
  'CH2 GENERALITY: does the Ω-C locks amendment respect Lock 14 across JSON/CSS L4/Sheets/BBNF-self; does the Ω-D master-plan reconciliation generalise to non-JSON; no JSON/CSS-narrowing amendment?',
  'CH3 REGRESSION: does any staged amendment reintroduce a skinny/REDRESS route or a T-P2-refuted assertion; cross-check Ω-C+Ω-D+Ω-E against the REDRESS ledger?',
  'CH4 COST: does every staged amendment carry a LOC budget + propagation cost (files touched); are the CRUD operations realistic + bounded?',
  'CH5 HIDDEN-COUPLING: does any Ω-C lock amendment imply a parallel substrate / renamed sidecar / Lock-1 violation / Track1≡Track2 dishonesty; the substrate union holds across every staged delta?',
  'CH6 NEXT-TRANCHE-IMPACT: does Ω-F next-cycle directive specify concrete measurable entry conditions; are the G-Omega sign-off items concretely measurable (the locks-diff, the master-plan-diff, the CRUD plan, the SK-V18 close summary)?',
]
function chPrompt(lens, v) {
  return 'You are a SK-V18 Pass Omega (astral V10) CHALLENGE lens (cycle V' + v + '). Adversarially review the 6 Ω artefacts + the staged diffs under ' + V + ' (ΩA-coherence, ΩB-skinny-lessons, ΩC-locks + locks-diff.md, ΩD-master-plan + master-plan-diff.md, ΩE-skinny-corpus, ΩF-migration-handoff) against the live V1 surfaces (restart/ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md, MIGRATION.md, HANDOFF.md) and the converged T-P1/T-P2/T-P3 evidence. '
    + 'YOUR LENS: ' + lens + ' '
    + 'Spot-verify the most load-bearing items (run git apply --check on the staged locks-diff; resolve a cited §H wave; check a REDRESS reference). Enumerate every staged amendment / CRUD operation under your lens; judge ACCEPT / REVISE (name the artefact + the exact correction) / REJECT (a non-applying diff / a revived REDRESS route / a Lock-14 narrowing / a coupling / an uncited claim + the evidence). Cycle V1 expects >=30% REVISE. '
    + 'Write your verdict to ' + V + '/hardening/V' + v + '/CH' + (LENSES.indexOf(lens) + 1) + '.md. End your reply and the file with EXACTLY one line, no backticks: TALLY accept=N revise=N reject=N'
}

phase('Harden')
let consec = 0, v = 0, voids = 0
while (consec < 2 && v < 5 && voids < 3) {
  v++
  const verdicts = await parallel(
    LENSES.map((L) => () => agent(chPrompt(L, v), { label: 'CH' + (LENSES.indexOf(L) + 1) + '-v' + v, phase: 'Harden' }))
  )
  const tallies = verdicts.filter(Boolean).map(parseTally).filter(Boolean)
  if (tallies.length < 4) { voids++; log('Pass Omega CHALLENGE v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('Pass Omega CHALLENGE v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0)) {
    const FOLD =
      COMMON
      + 'TASK FOLD (cycle V' + v + '). The Pass Omega CHALLENGE lenses wrote verdicts to ' + V + '/hardening/V' + v + '/CH1..CH6.md. Read all verdict files; for every REJECT correct the non-applying diff / revived route / coupling, for every REVISE add the missing citation/cost/cohesion-fix; APPLY each fix IN PLACE editing the named Ω artefact + staged diff under ' + V + '. The staged locks-diff must keep git apply --check exit 0. Do NOT touch a live governance surface. Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Harden' })
  }
}

const CONS =
  'You are the Pass Omega V10 aggregator. The schema-free CHALLENGE ran ' + v + ' cycle(s); converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read the 6 Ω artefacts + staged diffs under ' + V + ' + the per-cycle verdicts under ' + V + '/hardening and write ' + V + '/hardening/CONSOLIDATED.md AND the G-Omega package summary ' + V + '/G-OMEGA-PACKAGE.md: (1) the cycle summary (T-P1/T-P2/T-P3 consumed + SK-V18 lessons + corpus-coherence delta); (2) the CHALLENGE CONSOLIDATED verdict (per-cycle r); (3) the staged locks-diff status (git apply --check exit code, the 9A/11M/0R/1D disposition, the load-bearing accepted clauses); (4) the staged master-plan-diff summary; (5) the planned CRUD operations CRUD-1..CRUD-6 (what each will merge on G-Omega-close); (6) the SK-V18 close one-paragraph summary; (7) the post-G-Omega next-move (CRUD merge -> W-PRUNE P1-P5 dispatch-eligible first -> SK-V19 totality-fold tee-up). Return a 14-line summary that IS the G-Omega presentation the orchestrator shows the user.'
const cons = await agent(CONS, { label: 'aggregate-g-omega', phase: 'Harden' })

return { converged: consec >= 2, cycles: v, voids, landed: arts.filter(Boolean).length, consolidated: cons }
