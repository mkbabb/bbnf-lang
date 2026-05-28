# S-P2 V1 CH3 Regression

Disposition: ACCEPT.

The artifacts carry forward the SK-V16 pre-block families:
28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215,
242-247, and FNV closed-enum production migration.

The high-risk candidates are quarantined rather than admitted:

- PMULL prefix XOR and CSSC/CTZ next-bit routes remain blocked from production
  promotion by prior REDRESS unless fresh P1/S-P3 evidence and a same-wave
  consumer exist.
- Unicode hex decode is accepted only as a hex decoder; JSON decoded-string,
  codepoint-product, and fixed-shape rows remain pre-blocked under old framing.
- Digit MAC remains candidate-only and cannot reopen numeric routes without
  fresh BBNF-side consumer evidence.
- StringBlock/tiny/string64 replay is rejected except for the already admitted
  W11W memchr split floor.

No candidate requires a LOCKS change or a G-Omega amendment.
