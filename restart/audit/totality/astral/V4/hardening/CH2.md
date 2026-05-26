# Pass Omega V4 CH2 Generality

Verdict: ACCEPT.

W4R does not special-case CSS in a generic crate. It prevents the current
CSS-specific provider deletion from happening before the generic replacement
exists. Moving deletion into W5 strengthens the general Lock 14 trajectory:
per-grammar providers are removed only when the grammar-agnostic provider path
is load-bearing.

No non-JSON generality regression found. Sheets and BBNF-self witnesses remain
W5/W7/W8 downstream work as already specified.
