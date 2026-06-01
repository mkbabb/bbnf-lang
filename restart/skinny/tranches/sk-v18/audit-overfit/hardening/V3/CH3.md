# S-P0 audit-overfit hardening V3 — CH3 Regression (2nd confirm, independent re-grep)

Independent re-grep at HEAD `83b66db42`. V3 confirms the regression folds held through V2.

- **R1-CH3 (dependency-graph prose) holds in BOTH halves.** (i) SYNTHESIS §5 fact 3 carries the
  G2 dual entry-gate annotation ("G2 entry-gates on BOTH G1 AND P3 — a P3 failure also blocks G2,
  independent of G1; the dependency-graph diagram above is authoritative"). (ii) a2 §4 title is
  "G1/G3 co-derive; G3-failure blocks PROVE"; re-grepped a2 → **zero** "un-fork gates G1/G2"
  assertions outside the explicit quote-and-correct fold notes (the §4 disposition, §7 addenda
  table, §8 summary all corrected). The revert chain prose is unambiguous; the chain itself never
  inverts.
- **No §0.4 pre-block re-opened.** AZ-IV eager (G4 LAZY); StructRegistry; fact-stream-output (G2
  retires toward lowering); 24-broadcast; FNV-runtime (bench-only); x86 (P1 ENFORCES); second
  substrate (G3/G4 over existing tape); verbatim-blob/phantom/distinct-output (L1/L4/L2 police).
- **Prune list deletes no >SOTA-bearing code.** P1 dormant x86 (0 reachable intrinsics,
  `build.rs:38-40` early-returns); P2 not-the-headline warm path (KEEPS `css_canon_bench`); P3
  byte-identical 7→1; P4/P5 gate/symbol.
- **αD V1–V8 VALIDATED facts preserved as KEEP**; checkasm count is disk-true **14**, not stale 18.
- **R16 (now both nested structs) sharpens the recipe, does not change the close-gate.** The
  by-exclusion gate stays correctly RED pre-P3 (the 7 css_l4 rows differ in `profile` + `output_dir`
  + nested `output_labels`); widening the recipe to inline `frontend_requirements` too does not move
  the gate definition.

No new regression.

## Tally
ACCEPT 6 · REVISE 0 · REJECT 0 — **100%**.

TALLY accept=6 revise=0 reject=0
