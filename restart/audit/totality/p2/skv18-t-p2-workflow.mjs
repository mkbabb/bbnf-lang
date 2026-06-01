export const meta = {
  name: 'skv18-t-p2-research',
  description: 'SK-V18 T-P2 totality research: EXTEND the 6 literature dossiers (2A SOTA / 2B primitive-vocab / 2C grammar-neutrality / 2D cost-model / 2E host-arch esoterica / 2F parse-that gaps) to ground the SK-V18 generalization assertions in primary literature, then 6-lens CHALLENGE to 3Z. Write-to-disk.',
  phases: [
    { title: 'Research', detail: '2A..2F dossiers extended with SK-V18 literature grounding' },
    { title: 'Harden', detail: '6-lens schema-free CHALLENGE to 3Z' },
  ],
}

const OUT = 'restart/audit/totality/p2'
const P1 = 'restart/audit/totality/p1'
const SK = 'restart/skinny/tranches/sk-v18'

const COMMON =
  'You are a SK-V18 T-P2 (TOTALITY RESEARCH pass) literature-grounding agent. T-P2 grounds every SOTA + architectural assertion the V1 greater spec depends upon in PRIMARY LITERATURE — papers (title + venue/year), named-technique posts (URL + technique), or library source (path:line). It converts T-P1 divergences + amendment candidates from "the spec says X" into "X is defensible against the published architecture, here cited" — or REFUTES it. A confabulated citation is a hard REJECT. '
  + 'CONTEXT: this cycle absorbs the just-certified SK-V18 GENERALIZATION — ONE grammar-driven generator emitting JSON+CSS+Sheets (ultimately the 9 totality grammars) from .bbnf, preserving >SOTA, aarch64-PRIMARY (M5 Max; x86 SECONDARY/deleted in skinny). The SK-V18 plan: ' + SK + '/SPEC.md (certified wave manifest) + ' + SK + '/research/p2/SYNTHESIS-RESEARCH.md (the §6 named-primitive architecture) + ' + SK + '/research/p1/SYNTHESIS-PROFILE.md (G6=WIRE: find_component_delim 94.1% scalar scan retarget). '
  + 'T-P1 evidence base (CONVERGED, read what you need): ' + P1 + '/1A..1F + the 1E LOCKS-AMENDMENTS-CANDIDATE table (26 candidates). '
  + 'A prior totality cycle already wrote your dossier; EXTEND it (do not re-derive the converged content) with the SK-V18-specific grounding. aarch64 is PRIMARY; x86 esoterica are the secondary >SOTA path, hardware-gated, never the M5 Max close route. Lock 16 admissibility: every primitive carries a published citation + an abstract-primitive name + a scalar reference + a checkasm-parity plan + a same-wave consumer; an undocumented hand-tuned intrinsic loop is inadmissible. Refutation is a first-class output. You MAY use WebSearch/WebFetch to verify a citation, but every cited source must be REAL and verifiable. '
  + 'Emit the output-schema frontmatter (agent, pass, cycle, t_p1_inventories_consumed, primary_sources_cited, techniques_grounded, techniques_refuted, locks_amendment_candidates) + body sections: Executive Summary (<=200 words), Technique Grounding Table (spec-claim/T-P1-divergence-id | published source | grounded/refuted/partial | bbnf-specific note), Architectural Assertions Defended, Architectural Assertions Refuted, Open Research Questions (UNKNOWN->verify_action), LOCKS-AMENDMENTS-CANDIDATE table (or explicit no-candidates row). '

const AGENTS = [
  { key: '2A', label: '2A-sota', file: OUT + '/2A-sota-landscape.md',
    task: 'SCOPE 2A — SOTA parsing landscape. EXTEND ' + OUT + '/2A-sota-landscape.md. Ground the SOTA-parsing assertions: simdjson (stage1/stage2, On-Demand), sonic-rs (lazy-value), yyjson, asmjson; the DAV1D/FFmpeg/VLC PROCESS discipline (scalar oracle + checkasm differential + same-wave consumer — the process, NOT the pixel kernels). SK-V18 lens: ground the >SOTA comparison framing for the SK-V18 lazy-rich-vs-eager-cssom CSS bar (vs lightningcss) and the JSON strict-vs-sonic-rs bar; ground the dav1d-discipline behind the G6 NEON retarget of find_component_delim (scalar-reference-first + checkasm parity). Cite papers + library source per claim.' },
  { key: '2B', label: '2B-primitive-vocab', file: OUT + '/2B-primitive-vocabulary.md',
    task: 'SCOPE 2B — primitive-vocabulary. EXTEND ' + OUT + '/2B-primitive-vocabulary.md. The two-layer reusable SIMD/ASM primitive layer (Layer 0 vendored dav1d/x86inc macro corpus + Layer 1 bbnf-authored grammar-neutral vocabulary), the Lock-16 admission discipline. SK-V18 lens: ground the css_balanced_component_scan named §6 primitive and its inner alphabet-scan sub-kernel (the bbnf-simd eq-set member scan) as admissible primitives — published citation + abstract-primitive name + scalar reference + checkasm-parity + same-wave consumer. Ground the find_component_delim NEON retarget primitive (the ≤13-byte significant-set scan exceeding the 8-byte eq-set cap → the two-fan OR-reduce). aarch64 NEON/dotprod only.' },
  { key: '2C', label: '2C-grammar-neutral', file: OUT + '/2C-grammar-neutrality.md',
    task: 'SCOPE 2C — grammar-neutrality / generalisation. EXTEND ' + OUT + '/2C-grammar-neutrality.md. How the primitive vocabulary + the 5-shape BackendShape generalise beyond JSON to CSS L4 / Sheets / BBNF-self / the 9 totality grammars / arbitrary user grammars (Lock 14). SK-V18 lens: ground the one-generator generalization thesis — the §6 named-primitive (a)-(b)-(c)-(d) gate as the discipline that keeps a hand-shaped hot kernel grammar-PARAMETERIZED (works for any grammar with that construct) rather than grammar-specific; the css_balanced_component_scan neutrality-proof obligation (proven by a non-CSS invocation OR honestly CSS-scoped); the Sheets precedence-tower as the negative control. The future-grammar onboarding test against the 9-grammar fleet.' },
  { key: '2D', label: '2D-cost-model', file: OUT + '/2D-cost-model.md',
    task: 'SCOPE 2D — cost-model + 5-shape BackendShape. EXTEND ' + OUT + '/2D-cost-model.md. Ground the cost model (Lock 10) + the 5-shape BackendShape derivation in literature: how published parser generators / optimizers derive materialization strategy; the derive_backend_shape algorithm; the CollapsedStage FSM design. SK-V18 lens: ground R-A — DELETE RuntimeEmitterKind and dispatch on the lowered BackendShape (the cost-model-derived discriminator) as defensible against the published generator-architecture record; ground the emit_shape_source==lowered_program relocated-seam firewall.' },
  { key: '2E', label: '2E-host-arch', file: OUT + '/2E-host-arch-esoterica.md',
    task: 'SCOPE 2E — host-arch ASM/SIMD esoterica. EXTEND ' + OUT + '/2E-host-arch-esoterica.md. aarch64 PRIMARY (M5 Max): PMULL/VPCLMUL lineage, CSSC, UDOT/DotProd, LD4-interleaved classify, BCAX/EOR3, NEON svmatch_u8 port. x86 SECONDARY: AVX2/AVX-512 (VBMI2, GFNI, VPCLMUL), k-mask, VNNI, BITALG. SK-V18 lens: ground the specific aarch64 NEON kernels behind the G6 find_component_delim retarget (the inert-run vector skip stopping at the ([{\'"/ significant set; the two-fan OR-reduce for >8-byte sets); each entry: published citation + abstract-primitive name + hardware gate. NOTE the SK-V18 prune DELETES the x86 skinny tree — x86 esoterica here are the secondary totality >SOTA path only, never M5 Max close.' },
  { key: '2F', label: '2F-parse-that-gaps', file: OUT + '/2F-parse-that-gaps.md',
    task: 'SCOPE 2F — parse-that primitive gaps. EXTEND ' + OUT + '/2F-parse-that-gaps.md. Audit the parse-that crate family (/Users/mkbabb/Programming/parse-that/rust/{parse_that,regex}) for primitives the V1 spec + the SK-V18 plan depend on but the crate lacks: regex/HIR primitives, SIMD scan primitives, string primitives, float parsing (Eisel-Lemire/Clinger). SK-V18 lens: does parse-that carry the eq-set member scan / the balanced-delimiter scan substrate the css_balanced_component_scan + the find_component_delim retarget need, or is it a gap? Per gap: the published primitive, the upstream-or-vendor decision, the bbnf-specific need.' },
]

phase('Research')
const dossiers = await parallel(
  AGENTS.map((a) => () =>
    agent(COMMON + a.task + ' Write the extended dossier to ' + a.file + '. Return a 10-line summary: primary_sources_cited + techniques_grounded + techniques_refuted + the top 3 SK-V18 groundings + (any) amendment-candidate count.',
      { label: a.label, phase: 'Research' })
  )
)
log('T-P2 research complete (' + dossiers.filter(Boolean).length + '/6 dossiers landed)')

function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}
const LENSES = [
  'CH1 CORRECTNESS: every cited paper EXISTS and carries the claimed finding; every library-source citation resolves to the claimed path:line; every benchmark number traces to a named corpus+platform. A confabulated/unverifiable citation is a REJECT. Refuted-technique rows match the literature\'s actual position. (A REJECT requires citing the specific unverifiable source, not a blanket suspicion.)',
  'CH2 GENERALITY: Lock 14 holds — every primitive/technique grounded grammar-NEUTRALLY; 2C shows the technique transferring to CSS L4 / Sheets / BBNF-self / the 9-grammar fleet, not JSON-only. A technique grounded JSON-only but used fleet-wide is a REVISE.',
  'CH3 REGRESSION: no dossier grounds a route already refuted in skinny/REDRESS.md as viable; the rejected-route ledger is honoured; a "promising" direction REDRESS already falsified is a REJECT.',
  'CH4 COST: every grounded primitive carries an admission cost — scalar reference + checkasm parity per Lock 16; a same-wave consumer named; realistic LOC/risk; no orphan-kernel research.',
  'CH5 HIDDEN-COUPLING: no grounded design implies a parallel substrate / sidecar producer / Lock-1 violation; the FSM/CollapsedStage research keeps the mask stream transient; Layer-0/Layer-1 stays a clean one-directional dependency.',
  'CH6 ANTI-PAPER-CLOSE: no dossier claims a technique "validated" on citation-density alone; reference-stuffing (N sources cited, none integrated) is flagged; every grounded technique states the bbnf-specific reason it transfers; no deferral to "a later pass".',
]
function chPrompt(lens, v) {
  return 'You are a SK-V18 T-P2 (totality research) CHALLENGE lens (cycle V' + v + '). Adversarially review the 6 literature dossiers under ' + OUT + ' (2A-sota, 2B-primitive-vocab, 2C-grammar-neutral, 2D-cost-model, 2E-host-arch, 2F-parse-that-gaps) — the SK-V18 generalization grounding. '
    + 'YOUR LENS: ' + lens + ' '
    + 'Spot-verify the most load-bearing citations (a paper title/venue, a library path:line, a benchmark provenance) — use WebSearch/WebFetch to confirm a suspicious citation exists. Enumerate every grounding/refutation under your lens; judge each ACCEPT / REVISE (name the dossier + the exact correction or the missing citation) / REJECT (a confabulated/unverifiable citation or a refuted-route grounding + the falsifying evidence). Cycle V1 expects >=30% REVISE. '
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
  if (tallies.length < 4) { voids++; log('T-P2 CHALLENGE v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('T-P2 CHALLENGE v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0)) {
    const FOLD =
      COMMON
      + 'TASK FOLD (cycle V' + v + '). The T-P2 CHALLENGE lenses wrote verdicts to ' + OUT + '/hardening/V' + v + '/CH1..CH6.md against the 6 dossiers. Read all verdict files; for every REJECT correct or REMOVE the confabulated/refuted grounding, for every REVISE add the missing citation/neutralisation; APPLY each fix IN PLACE editing the named dossier(s). Every citation must be real + verifiable. Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Harden' })
  }
}

const CONS =
  'You are the T-P2 aggregator. The schema-free CHALLENGE ran ' + v + ' cycle(s); converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read the 6 dossiers under ' + OUT + ' + the per-cycle verdicts under ' + OUT + '/hardening and write ' + OUT + '/hardening/HARDENING-T-P2-CONSOLIDATED.md: the per-cycle r, the technique-grounding census (grounded vs refuted vs partial, summed), the Architectural Assertions REFUTED (these constrain T-P3 hardest), the LOCKS-AMENDMENTS-CANDIDATE summary, the disposition of every REVISE/REJECT folded, and the next-move (ready-for-T-P3). Return a 12-line summary incl. the grounding census + the refuted-assertions list + the amendment-candidate count.'
const cons = await agent(CONS, { label: 'aggregate', phase: 'Harden' })

return { converged: consec >= 2, cycles: v, voids, landed: dossiers.filter(Boolean).length, consolidated: cons }
