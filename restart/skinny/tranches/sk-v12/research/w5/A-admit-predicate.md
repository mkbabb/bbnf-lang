# SK-V12 W5 Research A: ADMIT Predicate Audit

## Verdict

The SK-V12 CSS L4 technical ADMIT candidate passes the USER PIN numeric and
strictness predicate, but W5 is still required because close documents disagree.
`skinny/RESULTS.md` still records `Overall outcome N-direct / NoGo`, and the
SK-V12 tranche documents remain in open-dispatch language.

## Evidence

- SPEC Section 0.1 defines campaign close by `ADMIT` when a generated CSS L4
  row beats `lightningcss_mbps + 1` on the same corpus, same output plane,
  strict equality, same-host independent oracle/Track 2, gate-consumed
  provenance, Lock 14 clean, Lock 16 admission clean, and JSON guards held or
  honestly demoted.
- SPEC Section 10 makes W5 responsible for `G-W5-CLOSE`: reconcile
  `RESULTS.md`, `REDRESS.md`, `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, and
  `DISPATCH-PROMPT.md` after W0..W4 dispositions.
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`
  records row `css_l4/declaration_values/direct_to_struct/main`, output plane
  `css_l4_declaration_value_fact_stream`, runtime path
  `runtime::generated_css_l4_declaration_values::parser::parse`, run id
  `sk-v12-w1b-2b:criterion-fnv64-27240148e5780a54`, gate status `pass`,
  outcome `A`, verdict `GO`, and admission `PASS-ADMIT-CANDIDATE`.
- The same report records Track 1 `429.34420791225705 Mbps`, cssparser oracle
  `217.42665242186035 Mbps`, lightningcss same-plane strict comparator
  `168.92962215656692 Mbps`, threshold `169.92962215656692 Mbps`, and margin
  over threshold `259.41458575569015 Mbps`.
- Strict equality is gate-consumed: Track 1, cssparser, and lightningcss fact
  streams share SHA-256
  `caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`.
- REDRESS-125 records the W1b-2b CSS L4 SOTA report gate and why it stopped at
  `PASS-ADMIT-CANDIDATE`: final campaign close and `RESULTS.md` movement belong
  to W5.
- REDRESS-121 records the refreshed native JSON guard capture and
  `gate-json` / AWK checks that held the JSON guard floors.
- REDRESS-122 records the `escape_mask_64` Lock 16 prerequisite closure before
  any later SIMD admission.
- REDRESS-126 records W4's routed ASM-gen attempt and zero final orphan count.

## Blocker

The evidence is sufficient for W5 to promote the candidate to campaign
`PASS-ADMIT`, but not until the close documents agree. The minimal W5 redress
slice is status/document reconciliation plus the `RESULTS.md` CSS row and
`REDRESS-127`, with no behavior/source/gate-code edits.
