export const meta = {
  name: 'skv18-t-p1-certify',
  description: 'SK-V18 T-P1 convergence continuation: drive the 6 evidence inventories to the 2-consecutive-clean §3Z streak the V1-V5 loop did not reach. Corrects the CH4 reject-convention false-positive (a self-falsified suspicion is ACCEPT, not reject). Write-to-disk, fold each cycle.',
  phases: [{ title: 'Certify', detail: '6-lens schema-free CHALLENGE V6+ on the inventories, fold-each-cycle, to 3Z' }],
}

const OUT = 'restart/audit/totality/p1'
const HARD = OUT + '/hardening'
const SK = 'restart/skinny/tranches/sk-v18'

const INVENTORIES = '1A-substrate-evidence, 1B-codegen-evidence, 1C-runtime-evidence, 1D-skinny-lessons, 1E-locks-evidence, 1F-coherence-scan (+1F-anti-pattern, 1F-past-corpora)'

const REJECT_RULE =
  'CRITICAL REJECT CONVENTION (corrects a V1-V5 false-positive): a REJECT is admissible ONLY when an inventory STATES SOMETHING FALSE ON DISK and you cite the live falsifying path:line proving the inventory wrong. A self-falsified suspicion — e.g. "I checked whether any LOC is fabricated and found NONE, every figure matches disk" — is an ACCEPT (the inventory is CORRECT), NEVER a reject=1. Do NOT record reject=N to mean "I rejected my own hypothesis"; that inverts the gate. If your lens finds the inventories sound, the honest tally is reject=0.'

const LENSES = [
  { k: 'CH1', q: 'CORRECTNESS: every spec-claim<->impl row resolves — the spec path:line carries the claimed text, the impl path:line carries the claimed symbol, the verdict matches the evidence; 1D RESULTS/REDRESS citations resolve to real entries. No recalled LOC/symbol. A row whose citations all verify on disk is ACCEPT.' },
  { k: 'CH2', q: 'GENERALITY: Lock 14 holds — no divergence catalogued JSON/CSS-only when it is a grammar-neutral substrate fact; 1C flags every grammar-named module in a generic crate; 1D separates JSON/CSS-empirical from grammar-neutral. No grammar-name leak uncited.' },
  { k: 'CH3', q: 'REGRESSION: no inventory re-opens a route in skinny/REDRESS.md; the rejected-route pre-block list is correctly identified by 1D/1E; no admitted REDRESS row mis-catalogued as unimplemented.' },
  { k: 'CH4', q: 'COST: every divergence carries a realistic LOC-delta + risk class; 1E amendment candidates carry a wave-alignment hint + path:line. A cost cell whose figure matches disk is ACCEPT — finding that the LOC figures are NOT fabricated is an ACCEPT, not a reject. A candidate with no supporting path:line is REVISE.' },
  { k: 'CH5', q: 'HIDDEN-COUPLING: the substrate inventory (1A) honours the Lock-1 union — no catalogued state implies a parallel substrate / sidecar producer / renamed-scanner; 1F caught the live couplings; the 1F auxiliaries (anti-pattern, past-corpora) are correctly cited as live where they are regenerated this cycle.' },
  { k: 'CH6', q: 'ANTI-PAPER-CLOSE: no inventory self-reports a divergence "resolved/wired" without a live-evidence citation; no divergence deferred to "a later inventory"; every UNKNOWN carries a verify_action.' },
]

function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}

function chPrompt(lens, v) {
  return 'You are a SK-V18 T-P1 (totality excavation) CHALLENGE lens ' + lens.k + ' (cycle V' + v + '), adversarial reviewer of the evidence inventories under ' + OUT + ' (' + INVENTORIES + '). These catalogue V1-spec<->impl divergences absorbing the SK-V18 generalization (one grammar-driven generator from .bbnf; the SK-V18 plan is at ' + SK + '/SPEC.md). The inventories were hardened through 5 prior cycles; citation discipline is near-exhaustive. Your job: drive out the RESIDUAL precision REVISEs (single-locus path:line / cost-carrier nits) to a 2-consecutive-clean fixed point, and catch any GENUINE reject. Be PROPORTIONATE — a nit is a REVISE only if it would mislead a T-P2 reader. '
    + REJECT_RULE + ' '
    + 'YOUR LENS: ' + lens.q + ' '
    + 'Read the inventory/inventories your lens targets; spot-verify the most load-bearing cited path:line rows against the V1 spec (restart/ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md) and the live code. Enumerate every finding; judge each ACCEPT / REVISE (name the inventory file + the EXACT one-line correction) / REJECT (per the convention above — an inventory stating something FALSE on disk + the live falsifying citation). '
    + 'Write your verdict to ' + HARD + '/V' + v + '/' + lens.k + '.md. End your reply and the file with EXACTLY one line, no backticks: TALLY accept=N revise=N reject=N'
}

phase('Certify')
let consec = 0, v = 5, voids = 0
while (consec < 2 && v < 8 && voids < 3) {
  v++
  const verdicts = await parallel(
    LENSES.map((L) => () => agent(chPrompt(L, v), { label: L.k + '-v' + v, phase: 'Certify' }))
  )
  const tallies = verdicts.filter(Boolean).map(parseTally).filter(Boolean)
  if (tallies.length < 4) { voids++; log('T-P1 certify v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('T-P1 certify v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0)) {
    const FOLD =
      'You are the SK-V18 T-P1 FOLD agent (cycle V' + v + '). The CHALLENGE lenses wrote verdicts to ' + HARD + '/V' + v + '/CH1..CH6.md against the inventories under ' + OUT + '. '
      + 'Read all 6 verdict files; collect every REVISE (and any genuine REJECT) with its named inventory file + exact correction, and APPLY each fix IN PLACE editing the inventory file(s). Every fix must cite a live path:line; do not recall, do not invent. '
      + 'IMPORTANT: a CH4/CH-style "reject" that merely records a self-falsified fabrication-suspicion (the LOC figures all MATCH disk) is NOT a defect — there is nothing to fix; note it as already-correct and move on. '
      + 'Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Certify' })
  }
}

const CONS =
  'You are the T-P1 aggregator. The convergence continuation ran cycles V6..V' + v + '; converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read the six inventories under ' + OUT + ' + the per-cycle verdicts under ' + HARD + ' (V1..V' + v + ') and write ' + HARD + '/HARDENING-T-P1-CONSOLIDATED.md: the per-cycle r (V1..V' + v + ', noting the V1-V5 in-workflow run + the V6+ continuation), the divergence census (spec_claims_implemented vs unimplemented vs impl_exceeds_spec, summed across 1A-1F), the 1E LOCKS-AMENDMENTS-CANDIDATE table summary (count + the load-bearing candidates), the disposition of every REVISE/REJECT folded, the note that the V1-V5 CH4 "reject" was a self-falsified-suspicion convention artifact (not a real defect, corrected in the continuation), and the next-move (ready-for-T-P2). If the streak was reached, record CONVERGED; else record the honest NOT-FULLY-CERTIFIED close (sound inventories, no real standing reject, bounded precision-churn). Return a 12-line summary incl. the divergence census + the amendment-candidate count.'
const cons = await agent(CONS, { label: 'aggregate-certify', phase: 'Certify' })

return { converged: consec >= 2, lastCycle: v, voids, consolidated: cons }
