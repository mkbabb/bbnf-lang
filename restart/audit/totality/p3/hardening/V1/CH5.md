# CH5 Hidden Coupling - T-P3 V1

## Verdict

REVISE.

The packet blocks the major hidden-coupling routes: no parallel tape, retained
sidecar producer, Track 1 == Track 2 close, FactStream-as-BackendShape, FNV
runtime arbiter, x86-as-aarch64 close, or source-present primitive admission is
accepted. However, the regex/scanner boundary needs a V2 repair. 3A and 3C
reintroduce `bbnf-regex` as an active owner despite the current naming canon, and
3C's Lock 16 clause can be read as admitting runtime regex/DFA by manifest plus
consumer proof without restating the Lock 1/G-Omega substrate gate.

## Evidence Commands And Outputs

```sh
git show --stat --oneline 0a0508acd -- restart/audit/totality/p3
```

```text
0a0508acd docs(sk-v15-t-p3): add V1 synthesis packet
 .../audit/totality/p3/3A-architecture-synthesis.md |  92 ++++++++++++
 .../totality/p3/3B-master-plan-reconciliation.md   | 167 +++++++++++++++++++++
 .../audit/totality/p3/3C-locks-crystallisation.md  | 114 ++++++++++++++
 restart/audit/totality/p3/3C-locks-v+1-diff.md     |  76 ++++++++++
 restart/audit/totality/p3/3D-skinny-fold.md        |  93 ++++++++++++
 .../audit/totality/p3/3E-grammar-generalisation.md | 145 ++++++++++++++++++
 restart/audit/totality/p3/3F-migration-handoff.md  | 120 +++++++++++++++
 7 files changed, 807 insertions(+)
```

```sh
git diff --check 0a0508acd^ 0a0508acd -- restart/audit/totality/p3
```

```text
<no output>
```

```sh
awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v1.diff
git apply --check /tmp/tp3-locks-v1.diff
```

```text
<no output>
```

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

```text
16
      67
```

```sh
rg -n "bbnf-regex|runtime regex/DFA|FactStream|FNV|source-present|x86|aarch64" \
  restart/audit/totality/p3/3A-architecture-synthesis.md \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3C-locks-crystallisation.md \
  restart/audit/totality/p3/3C-locks-v+1-diff.md \
  restart/audit/totality/p3/3D-skinny-fold.md \
  restart/audit/totality/p3/3E-grammar-generalisation.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

```text
restart/audit/totality/p3/3A-architecture-synthesis.md:54:...`admitted_fact_output` is a `SubstrateTarget` / output-plane classification, never a `BackendShape` value...
restart/audit/totality/p3/3A-architecture-synthesis.md:60:...Apple M5 Max/aarch64 hardware gate...source-present disposition...
restart/audit/totality/p3/3A-architecture-synthesis.md:61:...`bbnf-regex` for compile-time regex/HIR facts...runtime regex engines remain inadmissible without a named generated-runtime consumer and CH3/CH5 review.
restart/audit/totality/p3/3A-architecture-synthesis.md:62:...FNV...bench/telemetry only; they cannot become runtime selectors, production arbiters...
restart/audit/totality/p3/3C-locks-v+1-diff.md:42:...runtime regex/DFA substrate...remain rejected unless a later G-Omega explicitly amends Lock 1.
restart/audit/totality/p3/3C-locks-v+1-diff.md:64:...owner (`bbnf-regex`, `bbnf-simd`, generated provider, or `parse-that-regex`)...runtime regex/DFA...require the same manifest and consumer proof before admission.
restart/audit/totality/p3/3E-grammar-generalisation.md:95:...x86 diagnostic only unless future aarch64 strategy lands...
```

## Findings

| id | severity | target evidence | conflicting evidence | hidden-coupling failure | repair directive |
|---|---|---|---|---|---|
| CH5-V1-01 | High | 3A names `bbnf-regex` as the active compile-time regex/HIR owner (`restart/audit/totality/p3/3A-architecture-synthesis.md:61`); 3C's proposed Lock 16 owner list includes both `bbnf-regex` and `parse-that-regex` (`restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). | Lock 11 says `parse-that` is canonical, legacy `bbnf-regex` renames to `parse-that-regex`, and documentation uses the new name now (`restart/locks/LOCKS.md:319`). ARCH's naming lint rejects `bbnf-regex` references because the canonical name is `parse-that-regex` (`restart/ARCHITECTURE.md:2266`). | The packet creates two active regex/scanner authorities. That is a renamed-scanner coupling path across Lock 14 naming discipline and Lock 16 primitive ownership, and it weakens Lock 1 scanner-plan ownership by allowing future rows to cite the legacy owner as current. | Owner: 3A + 3C. Replace active owner wording with canonical `parse-that-regex` wording, or explicitly mark `skinny/crates/bbnf-regex` as a temporary legacy path awaiting the Lock 11 rename and not an admissible future owner. The v+1 Lock 16 owner list must not list both names as peers. |
| CH5-V1-02 | High | 3A says runtime regex engines are inadmissible only absent a named generated-runtime consumer and CH3/CH5 review (`restart/audit/totality/p3/3A-architecture-synthesis.md:61`). 3C accepts the same framing in the disposition matrix (`restart/audit/totality/p3/3C-locks-crystallisation.md:96`). 3C's Lock 16 clause then says runtime regex/DFA requires manifest and consumer proof before admission (`restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). | The same 3C diff correctly says runtime regex/DFA substrate remains rejected unless a later G-Omega explicitly amends Lock 1 (`restart/audit/totality/p3/3C-locks-v+1-diff.md:42`). Lock 1 rejects retained scanner/sidecar/second-substrate routes unless G-Omega amends Lock 1 (`restart/locks/LOCKS.md:118`-`126`). 2F's source dossier classifies runtime regex/DFA as blocked with no admitted owner or same-wave consumer (`restart/audit/totality/p2/2F-parse-that-gaps.md:73`-`80`, `restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`120`). | The Lock 16 primitive-manifest clause can be misread as a bypass: provide manifest + consumer and a runtime regex/DFA substrate becomes admissible. That contradicts the Lock 1/G-Omega substrate gate and leaves a runtime-regex substrate slip in the proposal set. | Owner: 3A + 3C. Restate the gate everywhere runtime regex/DFA appears: manifest and consumer proof are necessary but never sufficient for a runtime substrate; any runtime regex/DFA substrate requires prior G-Omega amendment to Lock 1. Keep compile-time regex/HIR facts separate from runtime matcher admission. |

## Lock Interaction Audit

| interaction | disposition | evidence |
|---|---|---|
| Lock 1 substrate union | REVISE only for regex wording. The tape/direct/fact-output union is otherwise preserved: Lock 1 forbids parallel substrates and retained class/mask/sidecar streams (`restart/locks/LOCKS.md:75`, `restart/locks/LOCKS.md:118`-`126`); 3A D12 keeps cursor sidecars open/rejected (`restart/audit/totality/p3/3A-architecture-synthesis.md:63`); 3D D08 blocks retained structural sidecars, public `UnionTape`, and sixth-substrate routes (`restart/audit/totality/p3/3D-skinny-fold.md:64`). |
| Lock 8 row plane | ACCEPT. CSS broadcast laundering is blocked: 3B demotes MP.NW2..4 as live CSS close (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:93`), 3C requires `measurement_row_id` and `broadcast_group_id` (`restart/audit/totality/p3/3C-locks-v+1-diff.md:52`), and 3E makes repeated CSS tuples non-admit (`restart/audit/totality/p3/3E-grammar-generalisation.md:66`). |
| Lock 10 five shapes | ACCEPT. 3A keeps exactly five variants and requires real lowerer output or gate rejection (`restart/audit/totality/p3/3A-architecture-synthesis.md:59`); 3C's all-five gate is exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` (`restart/audit/totality/p3/3C-locks-v+1-diff.md:56`); 3E prevents FactStream from becoming a shape (`restart/audit/totality/p3/3E-grammar-generalisation.md:68`). |
| Lock 14 generalisation | REVISE only for regex naming. The broader non-JSON boundary is sound: generated manifests replace grammar switches (`restart/audit/totality/p3/3E-grammar-generalisation.md:67`), CSS plus Sheets/BBNF-self is required for fleet claims (`restart/audit/totality/p3/3E-grammar-generalisation.md:64`), and full-surface scans must report exclusions (`restart/audit/totality/p3/3E-grammar-generalisation.md:73`). |
| Lock 16 primitive admission | REVISE for owner/runtime-regex clauses. The other primitive gates are sound: aarch64 close is required (`restart/audit/totality/p3/3A-architecture-synthesis.md:60`; `restart/audit/totality/p3/3E-grammar-generalisation.md:95`), source inventory is not admission (`restart/audit/totality/p3/3C-locks-crystallisation.md:79`), and PMU/x86 rows remain diagnostic unless row-local/aarch64 gates land (`restart/audit/totality/p3/3C-locks-crystallisation.md:91`-`94`). |

## Non-Finding Notes

- FactStream boundary: ACCEPT. 3A defines `admitted_fact_output` as output-plane/SubstrateTarget language, not a `BackendShape` or CSS Value API proof (`restart/audit/totality/p3/3A-architecture-synthesis.md:54`). 3C keeps FactStream out of `BackendShape` (`restart/audit/totality/p3/3C-locks-crystallisation.md:57`) and the proposed Lock 1 addendum says fact streams are not retained internal sidecars (`restart/audit/totality/p3/3C-locks-v+1-diff.md:42`). 3E keeps fact streams diagnostic until typed CSS provider and same-workload retime land (`restart/audit/totality/p3/3E-grammar-generalisation.md:65`, `restart/audit/totality/p3/3E-grammar-generalisation.md:81`).
- Track 1 == Track 2 dishonesty: ACCEPT. Current Lock 1 says Track 2 is a substrate-ceiling probe, not a second substrate (`restart/locks/LOCKS.md:77`-`81`), and 3B preserves the SinkOnly/direct gate without Track 1 == Track 2 sidecar close (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:81`).
- FNV arbiter: ACCEPT. 3A, 3B, 3D, and 3F all route FNV to bench/quarantine only and reject production arbiters or correctness proof (`restart/audit/totality/p3/3A-architecture-synthesis.md:62`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`; `restart/audit/totality/p3/3D-skinny-fold.md:63`; `restart/audit/totality/p3/3F-migration-handoff.md:72`).
- x86 diagnostic as aarch64 close: ACCEPT. 3E states x86 is diagnostic only unless future aarch64 strategy lands (`restart/audit/totality/p3/3E-grammar-generalisation.md:95`); 3C folds wrong-host evidence into aarch64 admission and diagnostic x86 rules (`restart/audit/totality/p3/3C-locks-crystallisation.md:91`-`92`).
- Source-present primitive admission: ACCEPT. 3B reclassifies source-present primitive work through W2 manifest and same-wave consumers (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:78`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:139`), and 3C says source inventory is not admission (`restart/audit/totality/p3/3C-locks-crystallisation.md:79`).

## Residual Risk

Residual risk is medium after repair. The substrate union and FactStream boundary
are explicit enough for Pass Omega if the regex/scanner wording is corrected.
Without that correction, downstream W2/W7 owners can accidentally treat a legacy
regex crate name or a runtime DFA manifest as an admissible scanner path, which
is exactly the hidden-coupling route CH5 is meant to block.
