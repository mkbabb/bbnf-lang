---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V1
disposition: REVISE
reviewed_artifacts:
  - restart/prompts/ORCHESTRATOR.md
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/skinny/tranches/sk-v15/SYNTHESIS.md
  - restart/locks/LOCKS.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# T-P1 V1 CH2 Generality / Lock 14

## Verdict

REVISE. The core V1 inventories now satisfy most of the CH2 Lock 14
generality bar: 1D separates JSON empirical findings, CSS audit-demoted
findings, and grammar-neutral findings; 1B/1C/1E/current 1F inventory the
runtime and generic grammar-name leaks; and the current divergences are not
promoted as fleet-wide proof when they are only JSON or CSS evidence.

The blocker is cross-inventory staleness. The auxiliary 1F files still carry
`cycle: V6`, SK-V14-only claims, and stale live-code facts that contradict the
current SK-V15 V1 inventories. That prevents ACCEPT because CH2 requires no
grammar-name leak to pass uncited and requires clean JSON-empirical vs
grammar-neutral separation across the inventories, not only inside 1D
(`restart/prompts/totality/PASS-1-EXCAVATION.md:110-114`).

REJECT is not warranted: the load-bearing V1 inventories correctly name the
Lock 14 failures and do not smuggle CSS or JSON evidence into grammar-neutral
closure.

## Governing Evidence

| Source | CH2 requirement |
|---|---|
| `restart/prompts/ORCHESTRATOR.md:74-85` | CH2 is GENERALITY: Lock 14 holds, no grammar-name leak, and interventions work for CSS L4 / Sheets / BBNF-self, not only JSON. |
| `restart/prompts/ORCHESTRATOR.md:104-126` | REVISE findings fold into V2 before the pass can converge. |
| `restart/prompts/totality/PASS-1-EXCAVATION.md:110-114` | For T-P1, no divergence may be miscatalogued as JSON-only when it is grammar-neutral; 1C must flag grammar-named runtime modules; 1D must separate JSON-empirical from grammar-neutral findings. |
| `restart/skinny/tranches/sk-v15/SYNTHESIS.md:38-46` | SK-V15 preserves JSON as a 51-row guard, demotes CSS admission, and requires Lock 14/codegen neutrality cleanup. |
| `restart/skinny/tranches/sk-v15/SYNTHESIS.md:98-110` | SK-V15 adds gate-exclusion detection; Lock 14 gates must scan and report their own exclusion lists. |
| `restart/locks/LOCKS.md:349-400` | Lock 14 forbids grammar-specific generic code, grammar-named public APIs, hardcoded JSON/CSS policy, and hand-written per-grammar runtime files. |
| `restart/locks/LOCKS.md:402-420` | Pattern H 67-file recurrence is a category-scale Lock 14 failure requiring per-tranche census and generated provenance. |

## Findings

| ID | Disposition | Finding | Evidence |
|---|---|---|---|
| CH2-001 | ACCEPT | 1D separates JSON empirical lessons from grammar-neutral findings. JSON rows are labelled JSON-empirical; CSS rows are separately audit-demoted; substrate, gate, Pattern H, Decision Engine, SIMD/process, and parse-that rows are grammar-neutral. | JSON split at `restart/audit/totality/p1/1D-skinny-lessons.md:92-97` and `:113-123`; grammar-neutral split at `restart/audit/totality/p1/1D-skinny-lessons.md:102-109` and `:125-137`; CSS audit-demoted split at `restart/audit/totality/p1/1D-skinny-lessons.md:98-101` and `:139-148`. |
| CH2-002 | ACCEPT | Current 1A/1B/1C/1E classify JSON/CSS evidence without promoting it to grammar-neutral closure. JSON direct scan/sink is useful but outside generated-runtime closure; CSS fact stream is output-plane evidence, not a sixth backend shape or typed CSS API; pass/codegen JSON-byte and JSON-role mining are generic grammar-shape leaks. | `restart/audit/totality/p1/1A-substrate-evidence.md:73-76`, `restart/audit/totality/p1/1A-substrate-evidence.md:123-130`; `restart/audit/totality/p1/1B-codegen-evidence.md:47-48`, `restart/audit/totality/p1/1B-codegen-evidence.md:85-87`; `restart/audit/totality/p1/1C-runtime-evidence.md:56-62`; `restart/audit/totality/p1/1E-locks-evidence.md:103-110`. |
| CH2-003 | ACCEPT | Runtime and generic grammar-name leaks are inventoried in the current V1 path. 1C flags the skinny runtime root aliases, generated-vs-hand runtime files, and main Pattern H; 1B flags hardcoded profile/runtime generation and pass-layer role mining; 1E and current 1F route gate exclusions and root-token misses. | `restart/audit/totality/p1/1C-runtime-evidence.md:57`, `restart/audit/totality/p1/1C-runtime-evidence.md:86-90`, `restart/audit/totality/p1/1C-runtime-evidence.md:106-111`; `restart/audit/totality/p1/1B-codegen-evidence.md:85-87`; `restart/audit/totality/p1/1E-locks-evidence.md:95`, `restart/audit/totality/p1/1E-locks-evidence.md:128-133`; `restart/audit/totality/p1/1F-coherence-scan.md:69-75`, `restart/audit/totality/p1/1F-coherence-scan.md:102`. |
| CH2-004 | REVISE | `1F-anti-pattern.md` is stale and contradicts current Lock 14 leak evidence. It declares `cycle: V6`, says `grammar_profile.rs:17-25` defines an eight-variant `RuntimeProvider`, and says live `find *_provider.rs` returns eight per-grammar provider files. Current 1B and live code identify the current leak as an eight-profile static roster plus renderer branches, with only `grammar_provider.rs` under `*_provider.rs`. | Stale frontmatter at `restart/audit/totality/p1/1F-anti-pattern.md:1-5`; stale enum/provider claims at `restart/audit/totality/p1/1F-anti-pattern.md:63` and `:72`; current 1B correction at `restart/audit/totality/p1/1B-codegen-evidence.md:26`, `restart/audit/totality/p1/1B-codegen-evidence.md:47`, `restart/audit/totality/p1/1B-codegen-evidence.md:113`; live static roster at `skinny/crates/codegen/src/grammar_profile.rs:89-99`; current request provider at `skinny/crates/codegen/src/grammar_provider.rs:1-13`. |
| CH2-005 | REVISE | `1F-past-corpora.md` is stale against SK-V15's JSON/CSS split. It still presents the SK-V14 audit-zero baseline as binding for JSON and CSS, while SK-V15 and 1D now preserve JSON as the validated guard baseline and demote CSS only. It also contains a stale instruction that "1D row 113" must cross-cite PC-008; current 1D row 113 is only a section heading. | Stale frontmatter and SK-V14 scope at `restart/audit/totality/p1/1F-past-corpora.md:1-5`, `restart/audit/totality/p1/1F-past-corpora.md:21-27`; stale audit-zero claim at `restart/audit/totality/p1/1F-past-corpora.md:61`, `restart/audit/totality/p1/1F-past-corpora.md:82`, `restart/audit/totality/p1/1F-past-corpora.md:119`; SK-V15 split at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:57-68`; current 1D JSON/CSS split at `restart/audit/totality/p1/1D-skinny-lessons.md:117-148`; stale row-number instruction at `restart/audit/totality/p1/1F-past-corpora.md:74` versus current heading at `restart/audit/totality/p1/1D-skinny-lessons.md:113`. |
| CH2-006 | REVISE | 1D should explicitly carry the current pass-layer JSON-shape leak from 1B. The separation is structurally correct, but the grammar-neutral matrix should name `P1-1B-D9` and `P1-1B-D10` directly so recognizer JSON punctuation and materialization JSON-role mining cannot be mistaken for JSON-only empirical lessons. | 1B pass-layer leaks at `restart/audit/totality/p1/1B-codegen-evidence.md:48`, `restart/audit/totality/p1/1B-codegen-evidence.md:86-87`; current 1D G-6 names codegen neutrality broadly at `restart/audit/totality/p1/1D-skinny-lessons.md:134`, but does not cite the pass-layer role-mining rows. |

## Fold Directives

| Fold | Required V2 action |
|---|---|
| CH2-FOLD-001 | Rebase or replace `restart/audit/totality/p1/1F-anti-pattern.md` as T-P1 V1/SK-V15 output. Remove stale `RuntimeProvider` and eight-provider-file claims; replace them with the current `grammar_profile.rs` static roster, `runtime_generator.rs` JSON/CSS branches, and single `grammar_provider.rs` request provider. |
| CH2-FOLD-002 | Rebase or replace `restart/audit/totality/p1/1F-past-corpora.md` as T-P1 V1/SK-V15 output. Preserve historical SK-V14 audit-zero as history only; current classification must be JSON guard baseline plus CSS audit-demoted/reopened, matching SK-V15 and 1D. |
| CH2-FOLD-003 | Add a compact cross-inventory Lock 14 leak map, preferably in 1F coherence or 1C, with columns: surface, current leak, classification (`JSON-empirical`, `CSS audit-demoted`, `grammar-neutral`), owner inventory, and receiver. Include runtime root aliases, `grammar_profile.rs`, `runtime_generator.rs`, `passes` recognizer/materialization mining, Pattern H 67, gate exclusions, and Decision Engine grammar-named facts. |
| CH2-FOLD-004 | Expand 1D's grammar-neutral section to cite `P1-1B-D9` and `P1-1B-D10` directly. These are generic pass-layer leaks caused by JSON-shaped mining, not JSON-only empirical lessons. |

No source, lock, prompt, or inventory file is changed by this CH2 verdict.
