# CH5 Hidden Coupling - T-P3 V4

Verdict: ACCEPT.

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Context HEAD during confirmation: `40528179e`.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | `e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`; 7 files changed, 170 insertions, 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | exit 0, no output. |
| Extracted `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v4.diff`; `git apply --check /tmp/tp3-locks-v4.diff` | exit 0, no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| Required stale-pattern `rg` scan from `CHALLENGE-CONTEXT.md` over the seven T-P3 artifacts | no matches, exit 1. |
| `git diff --name-status e6c1c2a84 --` the seven T-P3 proposal artifacts | no output; current proposal artifacts match the target packet. |

The invariant outputs match the V4 challenge context: 16 numbered locks, 67
Pattern H runtime files, and no stale-pattern matches.

## Hidden Coupling Audit

| coupling route | verdict | target packet evidence | independent controlling evidence |
|---|---|---|---|
| Parallel substrate / retained sidecar | ACCEPT | 3A forbids public substrate APIs, retained sidecars, and a sixth shape (`restart/audit/totality/p3/3A-architecture-synthesis.md:32`-`34`), keeps root `StructuralIndex`/sidecar-like fields routed through Lock 1 classification (`restart/audit/totality/p3/3A-architecture-synthesis.md:74`, `:121`), and 3D blocks retained sidecars, cursor/list state, class columns, public `UnionTape`, and sixth substrate routes (`restart/audit/totality/p3/3D-skinny-fold.md:66`). | Lock 1 rejects parallel substrates, retained sidecars, public `UnionTape`, and second tapes (`restart/locks/LOCKS.md:75`-`81`, `:118`-`:126`); 2A allows only transient same-loop masks consumed into the existing substrate (`restart/audit/totality/p2/2A-sota-landscape.md:51`, `:110`). |
| Sidecar producer / fact-stream producer | ACCEPT | 3E keeps `CSS_GENERATED_RS`, `CssFullParseSummary`, and fact-stream-only CSS `parse()` out of close evidence until typed provider and same-workload retime land (`restart/audit/totality/p3/3E-grammar-generalisation.md:69`), and 3F routes CSS fact-stream blockers through W5/W6 (`restart/audit/totality/p3/3F-migration-handoff.md:70`). | SK-V15 SPEC blocks fact-stream-only CSS proof before W6 provider/retime gates (`restart/skinny/tranches/sk-v15/SPEC.md:56`, `:196`); 2C refutes CSS fact-stream/generator-sidecar closure and requires typed-provider proof (`restart/audit/totality/p2/2C-grammar-neutrality.md:64`, `:145`). |
| Renamed-scanner Lock 1 violation | ACCEPT | 3A names canonical `parse-that-regex`, treats `skinny/crates/bbnf-regex` as a temporary legacy path, and makes it non-admissible as a future owner (`restart/audit/totality/p3/3A-architecture-synthesis.md:72`). 3C repeats the same owner split in crystallisation and proposed Lock 16 text (`restart/audit/totality/p3/3C-locks-crystallisation.md:57`, `:120`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). | Lock 11 states `parse-that` / `parse-that-regex` is canonical and legacy `bbnf-regex` renames to it (`restart/locks/LOCKS.md:319`); 2F requires parse-that-family owner taxonomy and blocks runtime regex import without consumer plus CH3/CH5 review (`restart/audit/totality/p2/2F-parse-that-gaps.md:74`, `:119`-`:120`). |
| Track 1 == Track 2 dishonesty | ACCEPT | 3B explicitly says `SinkOnly` remains scoped and there is no Track 1 == Track 2 sidecar (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:91`), while 3E rejects repeated CSS throughput tuples without independent row identity (`restart/audit/totality/p3/3E-grammar-generalisation.md:70`). | Lock 1 says Track 2 is a substrate-ceiling probe, not a second substrate or hidden runtime identity (`restart/locks/LOCKS.md:77`-`:81`); 2A refutes CSS 24-row broadcast as one aggregate measurement, not independent row movement (`restart/audit/totality/p2/2A-sota-landscape.md:59`-`:61`). |
| Fact-stream shape leak | ACCEPT | 3A classifies `admitted_fact_output` as output-plane / `SubstrateTarget`, never a `BackendShape` value (`restart/audit/totality/p3/3A-architecture-synthesis.md:65`); 3C keeps `FactStream` out of `BackendShape` (`restart/audit/totality/p3/3C-locks-crystallisation.md:46`, `:82`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:42`); 3E preserves the five-shape canon and forbids adding FactStream as a shape (`restart/audit/totality/p3/3E-grammar-generalisation.md:72`, `:86`). | 1E verifies the five-shape canon and says FactStream is not a sixth `BackendShape` (`restart/audit/totality/p1/1E-locks-evidence.md:83`, `:130`); Lock 1 repeats the same boundary (`restart/locks/LOCKS.md:100`-`:109`). |
| Runtime regex/DFA substrate | ACCEPT | 3A states runtime regex/DFA remains rejected as runtime substrate unless prior G-Omega amends Lock 1, and manifest plus consumer proof is necessary but never sufficient (`restart/audit/totality/p3/3A-architecture-synthesis.md:72`, `:89`). 3C repeats that rule in D-L01, D-L16, and the proposed Lock 1/16 hunk (`restart/audit/totality/p3/3C-locks-crystallisation.md:46`, `:57`, `:121`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:42`, `:64`). | 2F classifies runtime regex/DFA matcher work as blocked without a named generated-runtime consumer (`restart/audit/totality/p2/2F-parse-that-gaps.md:74`) and requires CH3/CH5 hidden-substrate review before any runtime import (`restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`:120`). Lock 1 rejects second substrate/public API/retained stream routes without G-Omega amendment (`restart/locks/LOCKS.md:118`-`:126`). |
| x86 close evidence | ACCEPT | 3A requires Apple M5 Max/aarch64 gates and rejects source-present-only primitive admission (`restart/audit/totality/p3/3A-architecture-synthesis.md:71`, `:108`); 3B keeps `CollapsedStage` diagnostic unless aarch64 proof exists (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:133`, `:163`); 3C folds wrong-host evidence into diagnostic x86 and aarch64 admission rules (`restart/audit/totality/p3/3C-locks-crystallisation.md:53`, `:116`-`:117`); 3E states no `CollapsedStage` admission for current SK-V15 grammars on M5 Max/aarch64, with x86 diagnostic only (`restart/audit/totality/p3/3E-grammar-generalisation.md:99`). | 2D says AVX-512 `CollapsedStage` is diagnostic and aarch64 admission needs scalar/parity/hardware/consumer proof (`restart/audit/totality/p2/2D-cost-model.md:65`, `:118`); 2E makes Apple M5 Max/aarch64 the only close route and x86 diagnostic-only (`restart/audit/totality/p2/2E-host-arch-esoterica.md:68`, `:81`, `:123`). |

## Findings

None.

V4 independently rechecked the seven T-P3 target artifacts and the controlling
T-P1/T-P2/LOCKS evidence. The target packet does not introduce a parallel
substrate, sidecar producer, renamed-scanner Lock 1 violation, Track 1/Track 2
dishonesty, fact-stream shape leak, runtime regex/DFA substrate, or x86 close
evidence. Routed open items, such as root `StructuralIndex` classification and
transient primitive-mask lifetime proof, remain gated by Lock 1/Lock 16 and do
not authorize hidden substrate expansion.
