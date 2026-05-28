# Alpha-C — REDRESS Digest — SK-V15 V1

Pass: Pass Alpha. Cycle: SK-V14 -> SK-V15.
Date: 2026-05-27.
Scope: admitted, rejected, contrived, and pre-blocked route digest.
Output: this file.

## Admitted Classes

JSON is the clean admitted class. W11W admits the final six parse_only
rows with `memchr2` trusted-string splitting and same-run strict floor
evidence (`skinny/REDRESS.md:6254`). W11A admits strict product direct
rows without relying on digest re-admission (`skinny/REDRESS.md:5853`).
W11L, W11N, and W11O admit JSON direct/typed residual rows, but their
closed-token and FNV components remain bench-only and require a SK-V15
guard (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60`).

W5, W6, and W7 are ledger-closed as governance/wave-graph work, not as
proof that the implementation is clean. PASS-IMPL reclassifies the CSS
generator outcome as provider-content relocation into `CSS_GENERATED_RS`,
not grammar-derived generation
(`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:31`).

## Refuted Classes

The wave-graph cycles remain pre-blocked:

| REDRESS | Refuted route |
|---|---|
| REDRESS-183 | W2 dual-tree emission before core CSS runtime regeneration existed |
| REDRESS-184 | W4 provider/template deletion before replacement generation |
| REDRESS-209..212 | monolithic/split W5 receivers that tried to delete before a valid provider-free generator existed |
| REDRESS-213 | W6.0 destructive CSS L4 root-runtime regeneration required `regen-css` capability the root workspace did not have (`skinny/REDRESS.md:5276-5293`) |

The JSON negative W10/W11 routes remain pre-blocked unless a wave proves a
fresh material differential. This includes W10X, W10Y/W10Z, W10AA, and
W11D through W11V rejected parse_only/product routes recorded in
`restart/skinny/tranches/sk-v14/research/skv14-W11-close.md:95`.

## Contrived Classes

| Class | SK-V15 treatment |
|---|---|
| CSS W8R 24-row admit | audit-demote; cannot re-admit by broadcast or brace-counter |
| W5 generator relocation | cosmetic until `CSS_GENERATED_RS` is retired and emission is grammar-derived |
| Lock 14 / Lock 16 clean gates | incomplete while scan roots or checkasm/report gates hide known leak files or exclusions |
| Decision Engine | scaffold while e-graph has zero rewrites, CSP is tautological, and four lowerers are stubs |
| FNV closed enum products | bench-only; cannot migrate into production runtime |

## Pre-Blocked Route List

SK-V15 waves must not re-open these patterns:

- one CSS timing tuple projected into N conceptual admits;
- lightningcss CSSOM compared against Track 1 summary or fact-stream output;
- string-literal CSS runtime bodies moved between files and called generated;
- Lock 14 or Lock 16 gates with silent exclusion lists;
- Pattern H collapse without generated headers and generator ownership;
- Decision Engine evidence with zero e-graph rules, non-driving CSP, or
  `format!("rule {} -> shape", ...)` lowerer stubs;
- W11L/W11N/W11O FNV closed-enum strict products outside `bbnf-bench` and
  `xtask` scaffolding.

Disposition: SK-V15 is PRUNE-then-REBUILD. PRUNE-WAVE-A through D retire
CSS contrivance, Lock 14 / Lock 16 gate holes, codegen leaks, and Pattern
H drift; REBUILD-WAVE-E through G build CSS Value, activate the Decision
Engine, and quarantine bench-only FNV products. Any deletion/retirement
wave remains blocked until its rebuild provider is proven no later than
that wave.
