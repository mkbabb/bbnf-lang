# Pass Omega V2 CH2 Generality Lock 14

| Field | Value |
|---|---|
| Pass | Pass Omega |
| Cycle | V2 CHALLENGE |
| Date | 2026-05-21 |
| Lens | CH2 Generality / Lock 14 |
| Output | `restart/audit/totality/astral/V1/hardening/V2/CH2.md` |

## Verdict

ACCEPT.

The V1 CH2 blocker is resolved. The folded Omega packet no longer leaves
Lock 14 witness cardinality as an unresolved G-Omega choice: fleet-wide
grammar-neutral transfer now requires one strict CSS L4 positive row plus both
Sheets and BBNF-self fail-closed negative-control witnesses or admitted
generated-role fact rows. A single negative-control witness is explicitly
scoped to the witnessed grammars only.

## Evidence Table

| Check | Disposition | Evidence | Finding |
|---|---|---|---|
| Governing CH2 scope | ACCEPT | Pass Omega defines CH2 as Lock 14 coverage across JSON, CSS L4, Sheets, and BBNF-self (`restart/prompts/pass-contracts/PASS-OMEGA.md:41`-`46`). The common CH2 lens requires no grammar-name leak and interventions that work for CSS L4, Sheets, and BBNF-self, not only JSON (`restart/prompts/ORCHESTRATOR.md:81`-`85`). | The review scope still requires all four grammar families. |
| V1 blocker restated | ACCEPT | V1 CH2 required the fold to settle fleet-wide witness cardinality, require both Sheets and BBNF-self for fleet-wide claims, scope single-control evidence, and make JSON/CSS comparator names row metadata (`restart/audit/totality/astral/V1/hardening/CH2.md:34`-`48`). The consolidated V1 challenge carried the same required fold (`restart/audit/totality/astral/V1/hardening/CONSOLIDATED.md:42`-`52`). | The acceptance check is against the recorded V1 blocker, not a weaker ad hoc standard. |
| Lock 14 hunk cardinality | ACCEPT | `locks-diff.md` Hunk 10 now requires generated provider registry, grammar-shape census, generated sink/fact/value/flag ownership, primitive policy source, one strict CSS L4 positive row, and both Sheets and BBNF-self controls before fleet-wide transfer; it explicitly scopes one-control evidence to witnessed grammars (`restart/audit/totality/astral/V1/locks-diff.md:301`-`314`). | The ambiguous V1 "Sheets/BBNF-self witness or negative control" wording is gone from the proposed lock text. |
| Omega-C disposition | ACCEPT | Omega-C's Lock 14 disposition names both Sheets and BBNF-self fail-closed/generated-role controls before fleet-wide grammar-neutral claims (`restart/audit/totality/astral/V1/ΩC-locks-amendments.md:58`). The family rationale repeats that one of those witnesses scopes claims to witnessed grammars only (`restart/audit/totality/astral/V1/ΩC-locks-amendments.md:75`). | The locks-amendment synthesis and the proposed diff agree. |
| Omega-D and master-plan diff | ACCEPT | Omega-D marks H.W4.LOCK14 partial until CSS plus both Sheets and BBNF-self witnesses pass (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:44`). MP.NW6 and MP.NW11 carry the same fleet-wide requirement, while one witnessed grammar remains scoped (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:74`-`79`). The explicit blocker row now says witness cardinality is resolved by CH2 and forbids MASTER, HANDOFF, BENCH, or S-P3 from using fleet-wide wording without both controls (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:104`). The companion master-plan diff mirrors this in H.W4.LOCK14, MP.NW6, and MP.NW11 (`restart/audit/totality/astral/V1/master-plan-diff.md:35`, `restart/audit/totality/astral/V1/master-plan-diff.md:61`-`66`). | The previous unresolved G-Omega decision has been converted into a concrete precondition. |
| CSS one-row scope | ACCEPT | The locks diff says the SK-V12 CSS declaration-values row is not full CSS parity, universal grammar closure, or SK-V13 close authority (`restart/audit/totality/astral/V1/locks-diff.md:159`-`166`). Omega-B records the same scoped CSS truth and names the remaining full parity target (`restart/audit/totality/astral/V1/ΩB-skinny-lessons.md:47`-`49`). | The packet does not overclaim from the single admitted CSS row. |
| Row-specific comparator metadata | ACCEPT | Omega-E's BENCH receiver defines universal telemetry columns as grammar id, output plane, strictness, oracle/comparator id, witness kind, generated policy source, row verdict, run id, host, and REDRESS/wave provenance; JSON `sonic-rs` and CSS `lightningcss`/`cssparser` are row-specific anchors, not universal columns (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:19`). The WORKSPACE receiver repeats that `sonic-rs`, `lightningcss`, and `cssparser` are row-specific comparator anchors (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:27`). | The telemetry wording no longer bakes JSON or CSS comparators into the grammar-neutral schema. |
| Generic-crate leakage | ACCEPT | The proposed Lock 14 hunk confines grammar names to generated `runtime/src/grammars/<name>/` output and excludes hand-coded provider enums, root aliases, generic branches, grammar-named public generic APIs, generic-root fixtures, and grammar-shaped policy mining (`restart/audit/totality/astral/V1/locks-diff.md:283`-`290`). Generic crates may consume generated manifests and facts, but may not hand-code JSON/CSS provider branches or punctuation/role policy (`restart/audit/totality/astral/V1/locks-diff.md:292`-`299`). Primitive policy must come from generated config or caller data (`restart/audit/totality/astral/V1/locks-diff.md:316`-`324`). | The folded packet preserves Lock 14 across generic crates and shared primitive APIs. |

## Required Fold Items

None for CH2.

## Residual Risks

The accepted Omega packet is still proposal-only. CRUD, governance-surface
edits, lock-text merges, source edits, gate output, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, and SK-V13 W0 remain blocked until Pass Omega challenge
converges and G-Omega closes (`restart/prompts/pass-contracts/PASS-OMEGA.md:86`-`110`,
`restart/audit/totality/p3/G3-PRESENTATION.md:62`-`68`).

## Verification

`git diff --check -- restart/audit/totality/astral/V1/hardening/V2/CH2.md`
passed with no output.
