# BB — Progress Log

**Status**: planned (gated on AZ close + AY-II close; grammar-colocated rule
storage per `crates/ir/src/rewrites/` + `grammar/<name>/rewrites/`).

**Date**: 2026-04-23

Dated execution log for tranche BB. Execution begins after BB opens.

BB ships Ruler-style e-graph rule inference with the e-graph as the
fast-path equivalence check and the surviving VM interpreter as the
non-circular ground-truth oracle on enumeration residue only. An
automatic ranker plus tiered review keeps human attention on novel
rules. Fleet-wide rules live in `crates/ir/src/rewrites/`; grammar-specific
rules colocate with each grammar under `grammar/<name>/rewrites/*.ron`
and are compiled at build time by `bbnf_derive` into the grammar's
cost-config.

Wave plan: W0 scaffold + Tranche H soundness rediscovery → W1 JSON +
Sheets enumeration → W2 CSS L4 + BBNF wide alphabet → W3 grammar-
specific rule authoring → W4 FINAL.
