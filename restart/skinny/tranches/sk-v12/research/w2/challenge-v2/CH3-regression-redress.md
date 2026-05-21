# SK-V12 W2 CH3 V2 - Regression / REDRESS

Disposition: ACCEPT.

V2 clears the V1 regression blockers. It preserves REDRESS 28/33 tiny-string
preblocks, REDRESS 88 PMULL default-body rejection, and REDRESS 89 CTZ/bulk
rewrite rejection. REDRESS 121 is carried: W1a closed the legality gate and
left W2 blocked only on the escape-mask prerequisite.

Runtime source-fix ownership is explicit and narrow. Proof-only W2 records no
behavior change and proves `RESULTS.md` untouched; any non-test scanner
behavior change triggers `check-json`, conformance, native bench/gate,
cost-facts, and SK-V12 floor verification. The rejected-patch-before-revert
protocol is sufficient.

Execution constraint: if production `bbnf-simd` or non-test `scan.rs` changes,
treat it as JSON-producing behavior and run the expanded guard path.
