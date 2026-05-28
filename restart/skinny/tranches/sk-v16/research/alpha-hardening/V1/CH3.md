# SK-V16 Alpha V1 CH3 - Regression

Disposition: REVISE-FOLDED.

## Findings

1. Alpha-C carried inherited REDRESS ids but did not expand all SK-V15
   pre-block meanings.
2. Native SIMD could be read as reopening REDRESS 88/89 or REDRESS 247.

## Fold

Alpha-C and SYNTHESIS now carry the REDRESS family meanings, including:
tiny-string/StringBlock replay, retained parse shortcuts, retained class
columns, Track 1 == Track 2 sidecars, global cap changes, numeric/digit routes
without fresh P1 BBNF hot-leaf evidence, one-quartet promotion, PMULL/CSSC
promotion from ISA/checkasm alone, and old decoded-string/structural-stream/
string64/fixed-shape framing. Native SIMD is conditional profile discovery only.
