# S-P2 V1 CH2 Generality

Disposition: ACCEPT.

`p2f-grammar-neutral.md` assigns a grammar-neutral verdict to every candidate
family from P2-A through P2-E. Accepted shapes are byte-set, class table,
mask/carry, mask-to-position, hex/digit atom, or generated tape-cursor
operations. JSON-only semantic shapes are reframed or quarantined.

The surviving candidate vocabulary has CSS L4, Sheets, and BBNF-self witnesses
where relevant:

- delimiter/class scans generalize to CSS punctuation, Sheets operators, and
  BBNF grammar punctuation;
- string-special scans generalize when terminator/escape/control policy is
  generated;
- digit blocks generalize only under caller-owned numeric policy;
- tape cursor operations generalize through generated view/tape tables.

No generic crate branch or hard-coded JSON/CSS profile is authorized.
