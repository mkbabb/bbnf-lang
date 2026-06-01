export const meta = {
  name: 'skv18-s-p1-profile',
  description: 'SK-V18 S-P1 profile pass: attribute JSON+CSS hot leaves from real captures + re-baseline >SOTA directionally + G6 wire-or-retire evidence + schema-free 3Z CHALLENGE',
  phases: [
    { title: 'Attribute', detail: 'A0 re-baseline / A1 JSON leaves / A2 CSS leaves+G6 gate / A3 synthesis' },
    { title: 'Harden', detail: '7-lens schema-free CHALLENGE to 3Z convergence' },
  ],
}

const P1 = 'restart/skinny/tranches/sk-v18/research/p1'
const RAW = P1 + '/raw'

// W0-LOCKED lightningcss >SOTA bars (sk-v17 W0 ledger; ABSOLUTE, captured quiet)
const W0 = 'bootstrap 1112.393 | tailwindcss 841.332 | material-components-web 1292.260 | animate 1218.685 Mbps'

const COMMON =
  'You are a SK-V18 S-P1 (profile pass) analysis agent. The campaign is generalizing two hand-written/forked CSS+JSON parsers into ONE grammar-driven generator while preserving >SOTA (CSS beats lightningcss; JSON beats sonic-rs strict). aarch64 / Apple M5 Max ONLY (x86 is a prune target). '
  + 'Your inputs are REAL captures already on disk under ' + RAW + ' (do NOT run cargo; the bench+sample were already captured). '
  + 'css_canon_n200.txt = N=200 cold per-parse medians (ROW lines: corpus / workload / bytes / median_mbps / min / max / stddev) for workloads {track1_full_parse (recognizer), track1_rich (the rich typed CSSOM projection materializing 9 fields — the >SOTA product), lightningcss (full CSSOM = the fair bar), cssparser (token-scan flaw probe)}. '
  + 'css_sample.txt + json_sample.txt = macOS `sample` call-tree captures (each frame line carries a leading sample count; deeper indented frames are callees; the heaviest LEAF is the function with high self count and few/no deeper frames). capture.log carries host_loadavg. '
  + 'IMPORTANT HONESTY: this capture ran under concurrent-session machine load (cite the loadavg from capture.log). Treat ABSOLUTE Mbps as DIRECTIONAL/depressed vs the W0 baseline; the LOAD-ROBUST load-bearing outputs are (1) the same-run >SOTA RATIO track1_rich/lightningcss and track1_full_parse/lightningcss, and (2) the relative hot-leaf RANK. Never present today absolute Mbps as a re-locked baseline. '
  + 'W0-LOCKED lightningcss bars (ABSOLUTE, quiet, for reference only): ' + W0 + '. '

const A0 =
  COMMON
  + 'TASK A0 — RE-BASELINE LEDGER. Read ' + RAW + '/css_canon_n200.txt and ' + RAW + '/capture.log. Build a per-corpus table: for each of the 4 corpora list median_mbps for all 4 workloads, then compute and show ratios track1_rich/lightningcss and track1_full_parse/lightningcss. '
  + 'Verdict (the load-robust finding): does track1_rich still BEAT lightningcss (ratio > 1.0) on the regular corpora (animate, bootstrap), and is the recognizer (track1_full_parse) still ahead? State each ratio and PASS/FAIL honestly. '
  + 'Disclose the load caveat explicitly with the loadavg. Record that THIS PASS fixed a canonical-bench bit-rot: the old track1_fact_stream workload referenced the W1-pruned fact-stream String and no longer compiled; it was replaced by track1_rich (parser::rich_summary, summing 9 materialized fields = eager-vs-eager vs lightningcss CSSOM), already committed. This closes residual R14/H1 (lazy-vs-eager framing) for the canonical harness. '
  + 'Write the ledger to ' + P1 + '/a0-rebaseline-ledger.md. Return a 6-line summary + the four track1_rich/lcss ratios.'

const A1 =
  COMMON
  + 'TASK A1 — JSON HOT-LEAF ATTRIBUTION. First read skinny/crates/bbnf-bench/src/bin/profile_direct.rs to confirm WHICH JSON workload the sample drove (the direct-to-struct JsonDigestSink path). Then read ' + RAW + '/json_sample.txt — the "Sort by top of stack, same collapsed" section is the per-leaf self-sample ranking; demangle the Rust symbols (e.g. ..generated_json9generated28parse_object_value_at_direct.. = generated_json::generated::parse_object_value_at_direct). Extract the TOP self-time leaf functions (rank by self sample count) and compute each leaf self-share as count / sum-of-all-leaf-counts. For each, name the function + its module/role (scan/tokenize, SinkOnlyProgram projection, number parse, string/escape handling, struct materialization). '
  + 'Then map each hot leaf to the SK-V18 generalization obligation: which leaves must G1 (the grammar-driven JSON projection) preserve, and which must G5 (the grammar-NEUTRAL NEON classifier) cover? Flag whether the bespoke json/scan.rs (S-P0 R12 non-neutral holdout) appears as a hot leaf — if so it is a G5 generalization target, if not it is cheap-to-neutralize. '
  + 'Write to ' + P1 + '/a1-json-hot-leaves.md. Return the top-8 leaves with self-share + the G1/G5 mapping.'

const A2 =
  COMMON
  + 'TASK A2 — CSS HOT-LEAF ATTRIBUTION + THE G6 WIRE-OR-RETIRE GATE (the load-bearing S-P1 deliverable). First read skinny/crates/bbnf-bench/src/bin/css_track1_profile.rs to confirm WHICH CSS workload the sample drove (the CssFullParser structural recognizer that underlies BOTH the full_parse and rich_summary paths — rich_summary runs this scan then projects typed values atop it). Then read ' + RAW + '/css_sample.txt — the "Sort by top of stack, same collapsed" section is the per-leaf self-sample ranking; demangle the symbols (e.g. ..CssFullParser20find_component_delim = CssFullParser::find_component_delim). Extract the TOP self-time leaves and compute each leaf self-share as count / sum-of-all-PARSER-leaf-counts (exclude the `main` corpus-loop frame from the parser-share denominator, but report it). '
  + 'CRITICAL: S-P0 finding R7 says the CSS NEON kernels written are find_css_significant + find_comment_close, DEAD at admission (only #[cfg(test)] callers), and the HOT scan is SCALAR. Determine from the sample WHICH scalar scanning leaf actually dominates (find_component_delim and/or consume_balanced_at) and QUANTIFY its self-share. NOTE EXPLICITLY whether the dominant hot scalar leaf is the SAME function the dead NEON targets or a DIFFERENT one (if the NEON was written for find_css_significant but the hot leaf is find_component_delim, the existing NEON does not even cover the hot path — a sharper G6 finding). '
  + 'G6 EVIDENCE VERDICT: if the scalar scan is a TOP-N leaf with meaningful self-share (>~8%), NEON wiring has real headroom -> recommend WIRE under dav1d discipline (scalar-reference FIRST + checkasm differential parity + aarch64 NEON only). If it is a MINOR leaf, recommend RETIRE (honestly delete the dead NEON kernels rather than wire a cold path). Ground the recommendation in the measured share, not assertion (actual-profiling). '
  + 'Also list the rich-projection cost leaves (rich_summary / classify / typed_value / nodes iteration) that S-P2 must ground the grammar-driven projection in. '
  + 'Write to ' + P1 + '/a2-css-hot-leaves.md. Return the top-8 leaves with self-share, the scalar-scan share, and the WIRE-or-RETIRE verdict with its numeric basis.'

phase('Attribute')
const [a0, a1, a2] = await parallel([
  () => agent(A0, { label: 'A0-rebaseline', phase: 'Attribute' }),
  () => agent(A1, { label: 'A1-json-leaves', phase: 'Attribute' }),
  () => agent(A2, { label: 'A2-css-leaves-G6', phase: 'Attribute' }),
])

const A3 =
  COMMON
  + 'TASK A3 — SYNTHESIS-PROFILE (the binding S-P1 input S-P2 consumes). Read ' + P1 + '/a0-rebaseline-ledger.md, ' + P1 + '/a1-json-hot-leaves.md, ' + P1 + '/a2-css-hot-leaves.md. '
  + 'Consolidate into one profile ledger: (1) the directional >SOTA re-confirmation (ratios, load caveat); (2) the JSON hot leaves + G1/G5 mapping; (3) the CSS hot leaves + the G6 WIRE-or-RETIRE recommendation grounded in the measured scalar-scan share; (4) what S-P2 (research) must ground the grammar-driven generator projection in (the JSON SinkOnly projection leaves, the CSS rich-projection leaves, the scalar-scan disposition); (5) sequencing reminder: S-P1 is a hard dependency of G5/G6 (no orphan kernel; profile-first). '
  + 'End with a one-line next-move: ready-for-S-P2. '
  + 'Here are the three upstream returns for cross-check.\n\n--- A0 ---\n' + (a0 || '(missing)') + '\n\n--- A1 ---\n' + (a1 || '(missing)') + '\n\n--- A2 ---\n' + (a2 || '(missing)') + '\n'
  + 'Write to ' + P1 + '/SYNTHESIS-PROFILE.md. Return a 10-line executive summary.'

const a3 = await agent(A3, { label: 'A3-synthesis', phase: 'Attribute' })

// ---- HARDEN: schema-free 3Z CHALLENGE ----
function parseTally(txt) {
  const m = (typeof txt === 'string') ? txt.match(/TALLY\s+accept=(\d+)\s+revise=(\d+)\s+reject=(\d+)/i) : null
  return m ? { accept: +m[1], revise: +m[2], reject: +m[3] } : null
}

const LENSES = [
  'CH1 MEASUREMENT-INTEGRITY: are the re-baseline ratios computed correctly from the ROW medians, the load caveat disclosed, no absolute number presented as a re-locked baseline?',
  'CH2 ADMIT-MECHANISM: is the >SOTA re-confirmation same-plane (track1_rich materializes 9 fields vs lightningcss full CSSOM), not a relabel; is the bit-rot fix honest?',
  'CH3 ACTUAL-PROFILING: is every hot-leaf claim grounded in a real self-count from the sample capture, never guessed from static analysis?',
  'CH4 G6-GATE-RIGOR: is the WIRE-or-RETIRE verdict grounded in the MEASURED scalar-scan self-share with a stated numeric threshold, and does it honor dav1d discipline (scalar-ref + checkasm + aarch64-only) / honest-retire?',
  'CH5 S-P2-HANDOFF-COMPLETENESS: does the synthesis name the concrete hot leaves S-P2 must ground the JSON+CSS generator projection in, with the sequencing (profile-first, no orphan kernel)?',
  'CH6 OVERFIT-PRUNE (CH7-spine): does any finding overfit to JSON or CSS specifics, or smuggle a hand-written contrivance; is the generalization framing preserved (one generator, grammar-derived)?',
  'CH7 HONESTY-CLOSURE: are R7/R12/R14 dispositions stated truthfully (scalar hot scan, non-neutral JSON holdout, eager-vs-eager fix), with no embellishment of the load-contended capture?',
]

function chPrompt(lens, v) {
  return 'You are a SK-V18 S-P1 CHALLENGE lens (cycle V' + v + '). Adversarially review the S-P1 profile artifacts on disk: '
    + P1 + '/a0-rebaseline-ledger.md, ' + P1 + '/a1-json-hot-leaves.md, ' + P1 + '/a2-css-hot-leaves.md, ' + P1 + '/SYNTHESIS-PROFILE.md, against the raw captures under ' + RAW + '. '
    + 'YOUR LENS: ' + lens + ' '
    + 'Read the artifacts and the relevant raw capture. Enumerate every claim under your lens and judge each ACCEPT (sound + grounded), REVISE (fixable wording/precision/grounding gap), or REJECT (false/contrived/fabricated). '
    + 'A REVISE must name the file + the exact single edit. A REJECT must cite the falsifying evidence in the raw capture. '
    + 'End your reply with EXACTLY one line in this format (no backticks): TALLY accept=N revise=N reject=N  -- where N are integer counts of claims you judged in each bucket under your lens.'
}

phase('Harden')
let consec = 0, v = 0, voids = 0
while (consec < 2 && v < 5 && voids < 3) {
  v++
  const verdicts = await parallel(
    LENSES.map((L, i) => () => agent(chPrompt(L, v), { label: 'CH' + (i + 1) + '-v' + v, phase: 'Harden' }))
  )
  const tallies = verdicts.filter(Boolean).map(parseTally).filter(Boolean)
  if (tallies.length < 4) { voids++; log('S-P1 CHALLENGE v' + v + ' VOID (' + tallies.length + ' valid lenses)'); continue }
  const accept = tallies.reduce((s, t) => s + t.accept, 0)
  const all = tallies.reduce((s, t) => s + t.accept + t.revise + t.reject, 0)
  const reject = tallies.reduce((s, t) => s + t.reject, 0)
  const revise = tallies.reduce((s, t) => s + t.revise, 0)
  const r = all > 0 ? accept / all : 0
  log('S-P1 CHALLENGE v' + v + ' r=' + r.toFixed(3) + ' (A=' + accept + ' R=' + revise + ' X=' + reject + ', ' + tallies.length + ' lenses)')
  const converged = r >= 0.95 && reject === 0
  if (converged) consec++; else consec = 0
  if (!converged && (revise > 0 || reject > 0) && v < 5) {
    // fold the REVISE/REJECT notes into the artifacts before the next cycle
    const FOLD =
      COMMON
      + 'TASK FOLD (cycle V' + v + '). The S-P1 CHALLENGE lenses raised ' + revise + ' REVISE and ' + reject + ' REJECT items against the S-P1 artifacts. '
      + 'Read every ' + P1 + '/audit-overfit-style CHALLENGE return is NOT on disk — instead re-derive the gaps by re-reading the four artifacts (a0/a1/a2/SYNTHESIS) against the raw captures under ' + RAW + ' through the SEVEN lenses, then APPLY each fix in place (edit the markdown files). '
      + 'Fixes must be single-edit, mechanism-correct, non-architectural, grounded in the raw capture. Do not invent numbers. After applying, return a one-line: FOLDED revise=' + revise + ' reject=' + reject + '.'
    await agent(FOLD, { label: 'fold-v' + v, phase: 'Harden' })
  }
}

const CONS =
  'You are the S-P1 consolidation agent. The schema-free CHALLENGE ran ' + v + ' cycle(s); converged=' + (consec >= 2) + ' (consec=' + consec + ', voids=' + voids + '). '
  + 'Read the four S-P1 artifacts under ' + P1 + ' (a0/a1/a2/SYNTHESIS-PROFILE) and write ' + P1 + '/HARDENING-S-P1-CONSOLIDATED.md recording: the convergence posture (per-cycle r), the disposition of every REVISE folded, the final G6 WIRE-or-RETIRE verdict, and the next-move ready-for-S-P2. Return a 8-line summary.'
const cons = await agent(CONS, { label: 'consolidate', phase: 'Harden' })

return { converged: consec >= 2, cycles: v, voids, consolidated: cons }
