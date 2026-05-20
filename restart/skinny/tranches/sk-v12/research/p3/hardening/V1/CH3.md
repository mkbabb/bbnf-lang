# SK-V12 S-P3 CHALLENGE V1 - CH3 Regression

Disposition: REVISE

## Lens

CH3 asks whether the P3-E pre-blocked-route ledger enumerates every
REDRESS route SK-V12 waves must not reopen, whether P3-B silently gives any
wave authority to reopen those routes, and whether the SPEC carries the full
blocked-route surface required by PASS-3
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:122`-`:126`).

## Findings

1. REDRESS 28+33 are not consistently carried into the W2 bounded-string /
   tiny-string pre-block map. REDRESS 28 rejected active 16-byte TBL
   tiny-string dispatch after a roughly 25% `twitter` regression and kept the
   active parser on the 8-byte scalar recognizer (`skinny/REDRESS.md:324`-`:337`).
   REDRESS 33 refined the same finding: the parity-green
   `match_tiny_plain_string` NEON kernel targets the wrong boundary and is not
   the parse-G fix (`skinny/REDRESS.md:394`-`:418`). REDRESS 72 later admitted
   only the scalar cap-16 retained probe and explicitly says it does not wire the
   rejected NEON kernel from REDRESS 28/33 (`skinny/REDRESS.md:1996`-`:2004`).
   The SPEC global block does name REDRESS 28/33
   (`restart/skinny/tranches/sk-v12/SPEC.md:600`-`:603`), but P3-E's
   bounded-string family lists REDRESS 54/55/60-69/72/82/83/116/117/119 and omits
   28/33 (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:90`-`:92`).
   P3-B's W2 per-wave map has the same omission while W2 is exactly where
   bounded string span, string-block, and tiny-string routes can re-enter
   (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:100`-`:101`,
   `:204`-`:209`). This is a regression risk because a W2 plan could treat
   REDRESS 72 as the only tiny-string history and miss the REDRESS 28/33 NEON/TBL
   active-dispatch rejection.

2. REDRESS 70/71 typed-output history is not enumerated in the ledger/SPEC even
   though W1 allows a typed-equivalent non-JSON baseline. REDRESS 70 rejects a
   hand-authored typed sink / measurement-surface proof as a SOTA close and says
   a conforming route needs an explicit host/API schema source, not a hidden BBNF
   directive or benchmark-private parser (`skinny/REDRESS.md:1890`-`:1940`).
   REDRESS 71 admits generated typed `DirectBuild` only through grammar-neutral
   schema-source facts consumed by codegen, with no new BIR variant, directive,
   retained side table, or benchmark-private Track 1 parser
   (`skinny/REDRESS.md:1944`-`:1965`). W1's target may be
   `css_l4`, `sheets`, or `bbnf_self` "or typed equivalent"
   (`restart/skinny/tranches/sk-v12/SPEC.md:171`-`:180`,
   `:399`-`:408`), and P3-E has a generic direct-as-typed block
   (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:67`),
   but neither P3-E's specific REDRESS list nor SPEC Section 8 names the
   REDRESS 70/71 typed-output boundary
   (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:120`-`:151`;
   `restart/skinny/tranches/sk-v12/SPEC.md:600`-`:622`). That leaves W1 typed
   baseline plans under-specified: they forbid direct digest as typed proof, but
   do not carry the older "no hand-authored typed sink / host-schema facts only"
   regression lesson by REDRESS number.

## Accepted Surfaces

The rest of the CH3 regression surface is sound.

- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate
  routes are hard-retired in P3-E, P3-B, SPEC, and dispatch, with REDRESS 96/97/98
  and REDRESS 102 cited (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:62`;
  `restart/skinny/tranches/sk-v12/SPEC.md:221`-`:229`;
  `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:123`-`:124`).
- JSON direct residuals are protected by REDRESS 119/120, non-JSON priority
  ordering, fresh-material-evidence burden, Track 1/Track 2 floors, and CHALLENGE
  acceptance (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:64`,
  `:107`; `restart/skinny/tranches/sk-v12/SPEC.md:499`-`:524`).
- W0-clamped rows and docs-only admissions are blocked in P3-E and in the W0/W3
  wave text (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:66`;
  `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:206`;
  `restart/skinny/tranches/sk-v12/SPEC.md:529`-`:533`).
- JSON guard rows are protected by P3-C/P3-F/SPEC: 4 direct guards and 7 typed
  guards have explicit floors, and every behavior wave that touches runtime,
  generated parser, SIMD, parse-that, bench, gate, or report code must maintain
  them or record measured demotion
  (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:65`-`:104`;
  `restart/skinny/tranches/sk-v12/SPEC.md:182`-`:201`,
  `:407`, `:460`, `:523`).
- REDRESS 50/51/53, 54/55, 60-69/72, 80, 82-84, 88/89, and 96-120 are otherwise
  represented in either the P3-E candidate-family table or SPEC Section 8, with
  the two revision gaps above limited to the omitted 28/33 and 70/71 enumeration
  bindings (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:90`-`:97`;
  `restart/skinny/tranches/sk-v12/SPEC.md:600`-`:622`).

## Fold Revisions

1. Update P3-E's bounded-string / special-byte family and §4 hard-preblock list
   to name REDRESS 28+33 explicitly: no active TBL/NEON
   `match_tiny_plain_string` dispatch, no "tiny-string/TBL proof-only" admission,
   and no treating REDRESS 72 scalar cap widening as authority to wire the
   rejected NEON/TBL active-dispatch route.

2. Update P3-B's W2 pre-block map and SPEC Section 5 W2 pre-block text to carry
   REDRESS 28+33 next to the bounded string span / StringBlock16 / tiny-string
   route family. The W2 plan must cite those entries if it selects any
   bounded-string, string-block, or tiny-string intervention.

3. Update P3-E and SPEC Section 8 to enumerate REDRESS 70/71 as the typed-output
   route boundary: typed-equivalent W1 baselines may admit only through generated
   DirectBuild/schema-source facts with independent oracle equality and gate
   consumption; they may not use a hand-authored typed sink, direct digest proof,
   hidden directive/BIR extension, or benchmark-private Track 1 parser.

4. Mirror the two additions into P3-F and the dispatch prompt's load-bearing
   pre-block summary so implementation agents see the same REDRESS set without
   relying on the SPEC global list alone.
