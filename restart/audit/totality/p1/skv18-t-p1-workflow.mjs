export const meta = {
  name: 'skv18-t-p1-excavation',
  description: 'SK-V18 T-P1 totality excavation: 6 evidence inventories (1A substrate / 1B codegen / 1C runtime / 1D skinny-lessons / 1E locks+amendments / 1F coherence) cataloguing V1-spec<->impl divergences absorbing the SK-V18 generalization, then 6-lens CHALLENGE to 3Z. Write-to-disk.',
  phases: [
    { title: 'Excavate', detail: '1A..1F evidence inventories vs the V1 spec + the SK-V18 generalization' },
    { title: 'Harden', detail: '6-lens schema-free CHALLENGE to 3Z' },
  ],
}

const OUT = 'restart/audit/totality/p1'
const SK = 'restart/skinny/tranches/sk-v18'

const COMMON =
  'You are a SK-V18 T-P1 (TOTALITY EXCAVATION pass) evidence agent. T-P1 catalogues current-state evidence against the V1 greater spec (restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md) and the skinny empirical corpus: per layer, what the V1 spec CLAIMS vs what is IMPLEMENTED vs where they DIVERGE — every divergence cited at path:line, no claim on recall. '
  + 'CONTEXT: this totality cycle absorbs the just-certified SK-V18 GENERALIZATION plan — backtrack two hand-written/forked parsers (JSON+CSS) into ONE grammar-driven generator emitting JSON+CSS+Sheets (and ultimately the 9 totality grammars: bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math) from .bbnf, preserving >SOTA, aarch64-only. The SK-V18 plan is at ' + SK + '/SPEC.md (the certified wave manifest) + ' + SK + '/research/p2/SYNTHESIS-RESEARCH.md (the architecture shortlist) + ' + SK + '/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md (the 6 addenda + prune list). '
  + 'The totality tree is crates/core/ (9 generated grammars at crates/core/src/grammar/generated/); the skinny tree is skinny/crates/ (JSON+CSS+Sheets-witness). T-P1 holds NO amendment authority — it catalogues evidence + surfaces LOCKS-amendment CANDIDATES (1E only); disposition is T-P3, ratification is Pass Omega. '
  + 'GROUND every row at path:line (wc -l + rg + read). Do NOT run cargo. Emit the output-schema frontmatter block (agent, pass, cycle, spec_surfaces_audited, files_audited_count, divergence_count{implemented,unimplemented,impl_exceeds_spec,unknown}, locks_amendment_candidates) + body sections: Executive Summary (<=200 words), Spec-Claim<->Implementation Table (claim path:line | impl path:line | verdict | note), Divergences Catalogued, Gaps/Missing Primitives, Open Questions (UNKNOWN->verify_action). '

const AGENTS = [
  { key: '1A', label: '1A-substrate', file: OUT + '/1A-substrate-evidence.md',
    task: 'SCOPE 1A — SUBSTRATE-LAYER evidence. What ARCHITECTURE.md §1/§7.1/§9 + Lock 1 claim about the substrate (tape ∪ direct-to-struct union, the BIR alphabet, the &\'i Tape<\'i> borrow shape) vs what is implemented in skinny/crates/runtime/src/tape/, crates/ir/src/ (or skinny/crates/ir), skinny/crates/runtime/src/grammars/. Cite spec-claim path:line <-> impl path:line per row; flag every divergence. SK-V18 lens: does the SK-V18 shared Cursor value-API trait + the phantom <G> DELETE (R-D) align with or diverge from the Lock-1 substrate-union claim? Is the tape substrate already grammar-neutral enough to serve the un-forked generator? The Lock 1 substrate-union audit is your SPINE.' },
  { key: '1B', label: '1B-codegen', file: OUT + '/1B-codegen-evidence.md',
    task: 'SCOPE 1B — CODEGEN-LAYER evidence. What the spec claims about codegen, the BackendShape 5-shape canon (ARCHITECTURE.md §7.3), derive_backend_shape, the cost model (Lock 10), the lowerer hierarchy (ARCHITECTURE.md §10, Lock 5) vs skinny/crates/codegen/src/lower/, codegen/src/lib.rs, skinny/crates/passes/src/ (or wherever passes live). Cite each shape\'s claimed derivation step <-> landed Rust state. SK-V18 lens: the SK-V18 R-A recommends DELETE RuntimeEmitterKind and dispatch on the lowered BackendShape — does the spec already canonicalize this, or is the RuntimeEmitterKind fork a spec-vs-impl divergence the totality spec must absorb? Is the 5-shape canon whole in codegen/src/lower/?' },
  { key: '1C', label: '1C-runtime', file: OUT + '/1C-runtime-evidence.md',
    task: 'SCOPE 1C — RUNTIME-LAYER evidence. The runtime crate, generated parsers, runtime/src/grammars/<g>/ (BOTH skinny/crates/runtime AND crates/core). What ARCHITECTURE.md §9 + §4.3 claim vs the generated SinkOnly/OffsetTape/EventTape consumption code actually emitted. Per-grammar runtime module census (the 9 totality grammars + the skinny JSON/CSS/Sheets-witness); hand-written-vs-generated audit (Lock 14). SK-V18 lens: the SK-V18 audit found CSS_GENERATED_RS is a verbatim ~910-LOC const courier (the generator does NOT exist for CSS) and 7 byte-identical css_l4 replicas — catalogue this divergence precisely; census which of the 9 totality grammars are genuinely generated vs hand-written/couriered. Flag every grammar-named module in a generic crate.' },
  { key: '1D', label: '1D-skinny-lessons', file: OUT + '/1D-skinny-lessons.md',
    task: 'SCOPE 1D — SKINNY-TRACK LESSONS digest. What SK-V1..SK-V18 empirically proved or disproved that the totality spec must absorb. Read skinny/REDRESS.md (the rejected-route + admitted-win ledger, 6465 lines) and skinny/RESULTS.md (the measured gate) as evidence; cite every entry. Produce a PROVED / DISPROVED / PENDING table the V1 spec must reflect, SEPARATING JSON/CSS-empirical findings from grammar-NEUTRAL findings (Lock 14). SK-V18 lens: digest the SK-V18 inflection-point finding (we are AT the inflection point; the impl is hand-written/forked), the one-generator architecture, the §6 named-primitive discipline (a naive grammar-walk regresses to lightningcss\'s architecture), the prune list (x86 delete, replica collapse), the G6=WIRE profile finding (find_component_delim 94.1%). These are the durable lessons T-P3 folds.' },
  { key: '1E', label: '1E-locks', file: OUT + '/1E-locks-evidence.md',
    task: 'SCOPE 1E — LOCKS evidence + amendment CANDIDATES. Audit the 16 locks (restart/locks/LOCKS.md) against current code + the skinny REDRESS ledger. Per lock: honoured / drifted / over-stated / silent-must-add, with path:line. Produce a LOCKS-AMENDMENTS-CANDIDATE table (additions / refinements / removals) with supporting evidence. CANDIDATES ONLY — never amend LOCKS.md, never re-number. SK-V18 lens: does the SK-V18 generalization imply lock-amendment candidates — e.g. the §6 named-primitive (a)-(b)-(c)-(d) gate as a discipline; the emit_shape_source==lowered_program rule (the relocated-seam firewall); the css_balanced_component_scan neutrality-proof obligation; the aarch64-only/x86-deleted standing; the verbatim-blob-courier prohibition? Each candidate carries a wave-alignment hint + path:line evidence. If none, emit an explicit no-candidates row naming scanned axes.' },
  { key: '1F', label: '1F-coherence', file: OUT + '/1F-coherence-scan.md',
    task: 'SCOPE 1F — CROSS-CORPUS COHERENCE + ANTI-PATTERN + PAST-CORPORA scan. (1) Cross-document coherence audit (spec-surface <-> spec-surface drift across ARCHITECTURE.md / MASTER-PLAN.md / LOCKS.md / HANDOFF.md). (2) Anti-pattern scan of the live code (god modules, parallel substrates, grammar-name leaks in generic crates) — write to ' + OUT + '/1F-anti-pattern.md. (3) Past-corpora scan (restart/skinny/tranches/sk-v{1..18}/research/, prior audit reports) for findings THIS cycle must not re-derive — write to ' + OUT + '/1F-past-corpora.md. SK-V18 lens: does the SK-V18 generalization plan cohere with the V1 spec surfaces, or does it surface spec-drift the totality fold must reconcile (e.g. the spec may already claim a un-forked generator the impl forks)? Multi-output permitted.' },
]

phase('Excavate')
const inventories = await parallel(
  AGENTS.map((a) => () =>
    agent(COMMON + a.task + ' Write your evidence inventory to ' + a.file + '. Return a 10-line summary: divergence_count by class + the top 3 divergences + (1E only) the amendment-candidate count.',
      { label: a.label, phase: 'Excavate' })
  )
)
const invText = inventories.map((v, i) => '--- ' + AGENTS[i].key + ' ---\n' + (v || '(dropped)')).join('\n\n')
log('T-P1 excavation complete (' + inventories.filter(Boolean).length + '/6 inventories landed)')

// ---- HARDEN: 6-lens schema-free CHALLENGE ----
function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}
const LENSES = [
  'CH1 CORRECTNESS: every spec-claim<->impl row resolves — the spec path:line carries the claimed text, the impl path:line carries the claimed symbol, the verdict matches the evidence; 1D RESULTS/REDRESS citations resolve to real entries. No recalled LOC/symbol.',
  'CH2 GENERALITY: Lock 14 holds — no divergence catalogued JSON/CSS-only when it is a grammar-neutral substrate fact; 1C flags every grammar-named module in a generic crate; 1D separates JSON/CSS-empirical from grammar-neutral. No grammar-name leak uncited.',
  'CH3 REGRESSION: no inventory re-opens a route in skinny/REDRESS.md; the rejected-route pre-block list is correctly identified by 1D/1E; no admitted REDRESS row mis-catalogued as unimplemented.',
  'CH4 COST: every divergence carries a realistic LOC-delta + risk class; 1E amendment candidates carry a wave-alignment hint + path:line; a candidate without supporting evidence is REVISE.',
  'CH5 HIDDEN-COUPLING: the substrate inventory (1A) honours the Lock-1 union — no catalogued state implies a parallel substrate / sidecar producer / renamed-scanner; 1F caught the live couplings; Track-1≡Track-2 dishonesty surfaces if present.',
  'CH6 ANTI-PAPER-CLOSE: no inventory self-reports a divergence "resolved/wired" without a live-evidence citation; no divergence deferred to "a later inventory"; every UNKNOWN carries a verify_action.',
]
function chPrompt(lens, v) {
  return 'You are a SK-V18 T-P1 CHALLENGE lens (cycle V' + v + '). Adversarially review the T-P1 evidence inventories under ' + OUT + ' (1A-substrate, 1B-codegen, 1C-runtime, 1D-skinny-lessons, 1E-locks, 1F-coherence/anti-pattern/past-corpora) against the V1 spec surfaces (restart/ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md) and the live code. '
    + 'YOUR LENS: ' + lens + ' '
    + 'Spot-verify the most load-bearing cited path:line rows. Enumerate every finding under your lens; judge each ACCEPT / REVISE (name the inventory file + the exact correction) / REJECT (a recalled/false/uncited claim + the falsifying evidence). Cycle V1 expects >=30% REVISE (an all-ACCEPT wave without close reading is itself paper-close). '
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
  if (tallies.length < 4) { voids++; log('T-P1 CHALLENGE v' + v + ' VOID (' + tallies.length + ' valid)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('T-P1 CHALLENGE v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0)) {
    const FOLD =
      COMMON
      + 'TASK FOLD (cycle V' + v + '). The T-P1 CHALLENGE lenses wrote verdicts to ' + OUT + '/hardening/V' + v + '/CH1..CH6.md against the six inventories. Read all verdict files; for every REJECT correct the inventory entry, for every REVISE add the missing evidence; APPLY each fix IN PLACE editing the named inventory file(s) under ' + OUT + '. Fixes must cite path:line; do not recall. Return one line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Harden' })
  }
}

const CONS =
  'You are the T-P1 aggregator. The schema-free CHALLENGE ran ' + v + ' cycle(s); converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read the six inventories under ' + OUT + ' + the per-cycle verdicts under ' + OUT + '/hardening and write ' + OUT + '/hardening/HARDENING-T-P1-CONSOLIDATED.md: the per-cycle r, the divergence census (spec_claims_implemented vs unimplemented vs impl_exceeds_spec), the 1E LOCKS-AMENDMENTS-CANDIDATE table summary, the disposition of every REVISE/REJECT folded, and the next-move (ready-for-T-P2). Return a 12-line summary incl. the divergence census + the amendment-candidate count.'
const cons = await agent(CONS, { label: 'aggregate', phase: 'Harden' })

return { converged: consec >= 2, cycles: v, voids, landed: inventories.filter(Boolean).length, consolidated: cons }
