export const meta = {
  name: 'skv18-s-p3-certify',
  description: 'SK-V18 S-P3 certification continuation: drive the SPEC.md §3Z CHALLENGE to the 2-consecutive-clean streak the V1-V5 loop did not reach (V5 ceiling). Write-to-disk verdicts, fold EVERY cycle incl. last.',
  phases: [{ title: 'Certify', detail: '7-lens schema-free CHALLENGE V6+ on SPEC.md, fold-each-cycle, to 3Z' }],
}

const T = 'restart/skinny/tranches/sk-v18'
const P3 = T + '/research/p3'
const P2 = T + '/research/p2'
const AUDIT = T + '/audit-overfit'
const HARD = P3 + '/hardening'

const LENSES = [
  { k: 'CH1', q: 'GATE-FALSIFIABILITY: does every wave exit-gate in SPEC.md name a CONCRETE falsifier (a grep/test/bench that turns RED), not a prose assertion? Flag any unfalsifiable gate.' },
  { k: 'CH2', q: 'SEQUENCING: is the wave manifest order + entry-gates consistent with the S-P2 lattice (P5-before-G1, P4-before-G2/G3, P3-before-G6/G2, G1-blocks-downstream, PROVE on G3∧G4 PARALLEL to G5/G6)? Any wave dispatchable before its predecessor closes?' },
  { k: 'CH3', q: 'ADDENDA-LAW: does every wave preserve the 6 addenda + the §6 (a)-(d) gate + the >SOTA gate + dav1d/aarch64-only + preserve-rich-ast? Any wave that could admit a courier/fork/phantom/orphan-kernel?' },
  { k: 'CH4', q: 'TELEMETRY-COMPLETENESS: do the required telemetry columns + the gate-consumer cover every close-condition predicate, each column produced by a named wave? Any close predicate with no telemetry, or column with no producing wave?' },
  { k: 'CH5', q: 'SOTA-PRESERVATION: is the >SOTA-regression gate bound at G2 AND G6 (corpus-in-timer, the per-corpus floors), the hot leaves preserved as gated primitives; no wave regresses the 94.1%/91.5% leaves silently?' },
  { k: 'CH6', q: 'OVERFIT-PRUNE (spine): does the SPEC preserve the ONE-generator framing end to end; the named primitives grammar-parameterized + (css_balanced_component_scan) neutrality-discharged; Sheets a genuine negative control; nothing overfit to JSON/CSS?' },
  { k: 'CH7', q: 'CLOSE-CONDITION-HONESTY: is the close condition the honest generalization goalset (one generator, >SOTA preserved with the lazy-rich framing disclosed, x86 gone, net LOC negative); the §6 fallback binding (if a parser cannot be grammar-derived without a shim, surface honestly, no _RS blob)?' },
]

function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}

function chPrompt(lens, v) {
  return 'You are a SK-V18 S-P3 CHALLENGE lens ' + lens.k + ' (cycle V' + v + '), adversarial reviewer of the wave-manifest SPEC. The SPEC has already been hardened through 5 prior cycles (V1-V5, all reject=0); your job is to drive out the RESIDUAL precision REVISEs so the doc reaches a 2-consecutive-clean fixed point — and to catch any genuine REJECT. Be PROPORTIONATE: a wording nit on a 1642-line doc is a REVISE only if it would mislead an implementer; do not invent churn. '
    + 'Read ' + T + '/SPEC.md (the section your lens targets) against the S-P2 sequencing (' + P2 + '/SYNTHESIS-RESEARCH.md §3) and the addenda (' + AUDIT + '/SYNTHESIS-AUDIT-OVERFIT.md). '
    + 'YOUR LENS: ' + lens.q + ' '
    + 'Enumerate every wave-gate/telemetry/close claim under your lens; judge each ACCEPT (sound) / REVISE (names the SPEC section + the EXACT one-line edit; only if materially clarifying) / REJECT (an unfalsifiable gate / broken sequence / addenda violation + the evidence). '
    + 'Write your verdict to ' + HARD + '/V' + v + '/' + lens.k + '.md. End your reply and the file with EXACTLY one line, no backticks: TALLY accept=N revise=N reject=N'
}

phase('Certify')
let consec = 0, v = 5, voids = 0
while (consec < 2 && v < 9 && voids < 3) {
  v++
  const verdicts = await parallel(
    LENSES.map((L) => () => agent(chPrompt(L, v), { label: L.k + '-v' + v, phase: 'Certify' }))
  )
  const tallies = verdicts.filter(Boolean).map(parseTally).filter(Boolean)
  if (tallies.length < 4) { voids++; log('S-P3 certify v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('S-P3 certify v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  // fold EVERY non-converged cycle, including the last (the V1-V5 loop skipped its last fold)
  if (!converged && (revise > 0 || reject > 0)) {
    const FOLD =
      'You are the SK-V18 S-P3 FOLD agent (cycle V' + v + '). The CHALLENGE lenses wrote verdicts to ' + HARD + '/V' + v + '/CH1..CH7.md against ' + T + '/SPEC.md. '
      + 'Read all 7 verdict files, collect every REVISE (and REJECT) with its named SPEC section + exact edit, and APPLY each fix IN PLACE editing ' + T + '/SPEC.md. Fixes must be single-edit, mechanism-correct, grounded in the S-P2 sequencing (' + P2 + '/SYNTHESIS-RESEARCH.md); a REJECT may require replacing an unfalsifiable gate with a concrete falsifier. Do NOT introduce new claims beyond the folds. '
      + 'Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Certify' })
  }
}

const CONS =
  'You are the SK-V18 S-P3 certification consolidation agent. The continuation CHALLENGE ran cycles V6..V' + v + '; converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read ' + T + '/SPEC.md + the per-cycle verdicts under ' + HARD + ' and APPEND to ' + P3 + '/HARDENING-S-P3-CONSOLIDATED.md a "Certification continuation" section: the per-cycle r (V6..V' + v + '), the residual REVISEs driven out, whether the 2-consecutive-clean streak was reached, and the final next-move (ready-for-T-P1 totality fold). If reject stayed 0 throughout but the streak was not reached, record the honest NOT-FULLY-CERTIFIED close (sound, no standing reject, residual precision-churn) rather than paper-closing. Return an 8-line summary.'
const cons = await agent(CONS, { label: 'consolidate-certify', phase: 'Certify' })

return { converged: consec >= 2, lastCycle: v, voids, consolidated: cons }
