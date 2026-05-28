# CH5 Hidden Coupling - T-P3 V3

Verdict: ACCEPT.

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Context commit: `5b85f7d5d`.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | `e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`; 7 files changed, 170 insertions, 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | exit 0, no output. |
| Extracted `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v3.diff`; `git apply --check /tmp/tp3-locks-v3.diff` | exit 0, no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| Required stale-pattern `rg` scan from `CHALLENGE-CONTEXT.md` | no matches, exit 1. |

The invariant outputs match the V3 challenge context: 16 numbered locks, 67
Pattern H runtime files, and no stale-pattern matches.

## Hidden Coupling Audit

| coupling route | verdict | target packet evidence | controlling evidence |
|---|---|---|---|
| Parallel substrate / retained sidecar | ACCEPT | 3A preserves no public substrate APIs, retained sidecars, or sixth shape (`restart/audit/totality/p3/3A-architecture-synthesis.md:32`-`34`), keeps `StructuralIndex`/sidecar-like fields open or rejected under Lock 1 (`restart/audit/totality/p3/3A-architecture-synthesis.md:74`), and 3D blocks retained sidecars, cursor/list state, class columns, public `UnionTape`, and sixth substrate routes (`restart/audit/totality/p3/3D-skinny-fold.md:66`). | Lock 1 rejects parallel substrates and retained sidecars (`restart/locks/LOCKS.md:75`-`81`, `restart/locks/LOCKS.md:118`-`126`); 2A allows only transient same-loop masks consumed into the existing substrate (`restart/audit/totality/p2/2A-sota-landscape.md:110`). |
| Sidecar producer / CSS fact-stream producer | ACCEPT | 3E keeps `CSS_GENERATED_RS`, `CssFullParseSummary`, and current fact-stream `parse()` out of close evidence until typed provider and same-workload retime land (`restart/audit/totality/p3/3E-grammar-generalisation.md:69`); 3F routes CSS fact-stream blockers through W5/W6 (`restart/audit/totality/p3/3F-migration-handoff.md:70`). | 2C refutes `CSS_GENERATED_RS` as a generator sidecar and blocks deletion before typed provider proof (`restart/audit/totality/p2/2C-grammar-neutrality.md:69`, `restart/audit/totality/p2/2C-grammar-neutrality.md:145`). |
| Renamed-scanner Lock 1 violation | ACCEPT | 3A names canonical `parse-that-regex`, treats `skinny/crates/bbnf-regex` as a temporary legacy path, and makes it non-admissible as a future owner (`restart/audit/totality/p3/3A-architecture-synthesis.md:72`). 3C carries the same repair in both crystallisation and proposed Lock 16 text (`restart/audit/totality/p3/3C-locks-crystallisation.md:57`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). | Lock 11 states `parse-that`/`parse-that-regex` is canonical and legacy `bbnf-regex` renames to it (`restart/locks/LOCKS.md:319`); 2F requires parse-that-family owner taxonomy and blocks runtime regex engines without review (`restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`120`). |
| Track 1 == Track 2 dishonesty | ACCEPT | 3B explicitly keeps `SinkOnly` scoped and says no Track 1 == Track 2 sidecar (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:91`); 3E rejects repeated CSS throughput tuples without independent row identity (`restart/audit/totality/p3/3E-grammar-generalisation.md:70`). | Lock 1 says Track 2 is a substrate-ceiling probe, not a second substrate (`restart/locks/LOCKS.md:77`-`81`); 2A refutes the CSS 24-row broadcast as one aggregate measurement (`restart/audit/totality/p2/2A-sota-landscape.md:59`-`61`). |
| FactStream as `BackendShape` | ACCEPT | 3A classifies `admitted_fact_output` as output-plane / `SubstrateTarget`, never a `BackendShape` (`restart/audit/totality/p3/3A-architecture-synthesis.md:65`); 3C keeps FactStream out of `BackendShape` (`restart/audit/totality/p3/3C-locks-crystallisation.md:46`, `restart/audit/totality/p3/3C-locks-crystallisation.md:82`); 3E forbids adding FactStream as a shape (`restart/audit/totality/p3/3E-grammar-generalisation.md:72`, `restart/audit/totality/p3/3E-grammar-generalisation.md:86`). | 1E verifies the five-shape canon and says FactStream is not a sixth `BackendShape` (`restart/audit/totality/p1/1E-locks-evidence.md:64`-`68`, `restart/audit/totality/p1/1E-locks-evidence.md:82`-`84`); Lock 1 repeats FactStream is not a sixth `BackendShape` (`restart/locks/LOCKS.md:100`-`109`). |
| Runtime regex/DFA substrate | ACCEPT | 3A states runtime regex/DFA remains rejected as runtime substrate unless prior G-Omega amends Lock 1, and manifest plus consumer proof is necessary but never sufficient (`restart/audit/totality/p3/3A-architecture-synthesis.md:72`, `restart/audit/totality/p3/3A-architecture-synthesis.md:89`). 3C repeats that rule in D-L01, D-L16, and the proposed Lock 1/16 hunk (`restart/audit/totality/p3/3C-locks-crystallisation.md:46`, `restart/audit/totality/p3/3C-locks-crystallisation.md:57`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:42`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). | 2F classifies runtime regex/DFA matcher work as blocked with no named runtime consumer (`restart/audit/totality/p2/2F-parse-that-gaps.md:73`-`74`) and requires a named consumer plus CH3/CH5 review before any runtime DFA plan (`restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`120`). Lock 1 rejects second substrate/public `UnionTape`/retained stream routes without G-Omega amendment (`restart/locks/LOCKS.md:118`-`126`). |
| x86 diagnostic evidence as aarch64 close evidence | ACCEPT | 3A requires Apple M5 Max/aarch64 gates and rejects source-present-only primitive admission (`restart/audit/totality/p3/3A-architecture-synthesis.md:71`, `restart/audit/totality/p3/3A-architecture-synthesis.md:108`). 3B keeps CollapsedStage diagnostic unless 2E supplies aarch64 proof (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:133`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:163`). 3C folds wrong-host evidence into diagnostic x86 and aarch64 admission rules (`restart/audit/totality/p3/3C-locks-crystallisation.md:53`, `restart/audit/totality/p3/3C-locks-crystallisation.md:116`-`117`). 3E states no CollapsedStage admission for current SK-V15 grammars on M5 Max/aarch64, with x86 diagnostic only (`restart/audit/totality/p3/3E-grammar-generalisation.md:99`). | 2D says AVX-512 CollapsedStage is diagnostic only and aarch64 admission needs scalar/parity/hardware/consumer proof (`restart/audit/totality/p2/2D-cost-model.md:65`, `restart/audit/totality/p2/2D-cost-model.md:118`). 2E makes Apple M5 Max/aarch64 the only close route and x86 diagnostic-only (`restart/audit/totality/p2/2E-host-arch-esoterica.md:25`-`32`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:67`-`69`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:81`). |

## Findings

None.

V3 folds the prior CH5 V1 defects: canonical `parse-that-regex` ownership is
used instead of peer legacy `bbnf-regex`, and runtime regex/DFA manifest plus
consumer proof is explicitly necessary but never sufficient without prior
G-Omega Lock 1 amendment. The V3 changes are CH4 field-coverage additions only
and do not reopen the hidden-coupling routes that were accepted in V2
(`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:31`).
