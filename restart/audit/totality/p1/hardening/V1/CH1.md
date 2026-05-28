---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V1
disposition: REVISE
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH1 Correctness Audit - T-P1 V1

## Verdict

REVISE. The fresh SK-V15 inventories mostly cite real surfaces and their major
verdicts match sampled evidence, but the cycle cannot ACCEPT under CH1:
`1F-anti-pattern.md` and `1F-past-corpora.md` are stale V6/SK-V14 artifacts in a
V1/SK-V15 wave, several rows cite paths that do not resolve from the repo root,
and `1A` has an uncounted table row. No whole inventory is rejected, but the
malformed/stale rows must be refreshed before consolidation.

## Findings

| ID | Disposition | Evidence |
|---|---|---|
| CH1-V1-F1 | REVISE | `restart/audit/totality/p1/1F-anti-pattern.md:4` and `restart/audit/totality/p1/1F-past-corpora.md:4` declare `cycle: V6`; both carry SK-V14-era scopes (`restart/audit/totality/p1/1F-anti-pattern.md:13-17`, `restart/audit/totality/p1/1F-past-corpora.md:21-24`). T-P1 V1 challenge files must judge V1 SK-V15 inventories, not carry forward unrefreshed V6 rows. |
| CH1-V1-F2 | REVISE | `restart/audit/totality/p1/1F-anti-pattern.md:55` reports stale LOC counts and old codegen structure. Live `wc -l` gives `report.rs=10564`, `gate.rs=5949`, `generated_real_typed.rs=4941`, `lock14_baseline.rs=4796`, `runtime/grammars/json/generated.rs=1235`, not the row's `8403`, `5698`, `3056`, `2119`, `842`. |
| CH1-V1-F3 | REVISE | `restart/audit/totality/p1/1F-anti-pattern.md:63` claims `RuntimeProvider` at `skinny/crates/codegen/src/grammar_profile.rs:17-25`; live lines 11-15 define `RuntimeGenerationMode`, and `rg "RuntimeProvider|json_provider|css_l4_.*_provider" skinny/crates/codegen/src` returns no hits. `restart/audit/totality/p1/1F-anti-pattern.md:72-74` repeats the obsolete 8-provider/`json_provider.rs`/CSS-template layout; live `find skinny/crates/codegen/src -maxdepth 1 -name '*_provider.rs'` returns only `grammar_provider.rs`. |
| CH1-V1-F4 | REVISE | `restart/audit/totality/p1/1A-substrate-evidence.md:13-18` counts/list additions through `1A-SUB-021`, but the table contains `1A-SUB-022` at `restart/audit/totality/p1/1A-substrate-evidence.md:78`. The row itself is evidence-bearing, but the frontmatter count is stale and the row is unaccounted. |
| CH1-V1-F5 | REVISE | Multiple rows use non-root-resolving shorthand citations: examples include `restart/audit/totality/p1/1A-substrate-evidence.md:75` (`json/scan.rs:1`), `restart/audit/totality/p1/1B-codegen-evidence.md:84` (`offset_tape.rs:15-17` siblings), `restart/audit/totality/p1/1C-runtime-evidence.md:72-87` (`json/generated.rs:1`, `parser.rs:5`, CSS config shorthands), `restart/audit/totality/p1/1E-locks-evidence.md:106` (bare lowerer files), `restart/audit/totality/p1/1F-anti-pattern.md:79`, `restart/audit/totality/p1/1F-anti-pattern.md:104`, `restart/audit/totality/p1/1F-anti-pattern.md:115` (`arena_template.rs:1-31`), `restart/audit/totality/p1/1F-past-corpora.md:83`, `restart/audit/totality/p1/1F-past-corpora.md:120`, and `restart/audit/totality/p1/1F-past-corpora.md:140` (`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`). These may be contextually clear, but CH1 requires cited path:line claims to resolve directly. |

## Evidence Checked

- Dispatch rules read: `ORCHESTRATOR.md` CH1 requires resolving citations at
  `restart/prompts/ORCHESTRATOR.md:81-88`; convergence/governance at
  `restart/prompts/ORCHESTRATOR.md:104-126`; T-P1 CH1 scope at
  `restart/prompts/totality/PASS-1-EXCAVATION.md:91-108`; SK-V15 addenda at
  `restart/skinny/tranches/sk-v15/SYNTHESIS.md:98-110`.
- Major code claims spot-checked: `BackendShape` is exactly five variants at
  `skinny/crates/ir/src/lib.rs:339-346`; `BackendExpr` is 13 variants plus
  `Recognizer::SimdScan` at `skinny/crates/ir/src/lib.rs:354-398`; CSS config
  writes `W7_POLICY_BACKEND_SHAPE = "admitted_fact_output"` at
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:5`;
  `runtime_profiles()` is the live 8-profile roster at
  `skinny/crates/codegen/src/grammar_profile.rs:89-100`.
- Runtime census spot-checked: `find crates/core/src/runtime -mindepth 2 -type f
  -name '*.rs' | wc -l` returns 67; generated-header scan under that tree returns
  0; `find skinny/crates/runtime/src/grammars -type f -name '*.rs' | wc -l`
  returns 48 with JSON=11, seven CSS dirs=5 each, Sheets witness=2.
- RESULTS/REDRESS citations sampled as real: `skinny/RESULTS.md:139-149` carries
  JSON 17/17 + CSS 24/24 notes and plane wording; `skinny/REDRESS.md:6254-6284`
  carries the W11W parse-only memchr admit; `skinny/REDRESS.md:126-132` carries
  the tape/direct projection wording; `skinny/REDRESS.md:216-234` carries the
  dispatch-table and 12-byte-token historical rejects.

## Artifact Dispositions

| Artifact | CH1 disposition | Notes |
|---|---|---|
| `1A-substrate-evidence.md` | REVISE | Evidence mostly resolves; frontmatter misses `1A-SUB-022`; a few context shorthands need full paths. |
| `1B-codegen-evidence.md` | REVISE | Major lowerer/shape verdicts checked; sibling-file shorthand citations must be expanded. |
| `1C-runtime-evidence.md` | REVISE | Census and runtime claims checked; table uses many relative path shorthands. |
| `1D-skinny-lessons.md` | ACCEPT | Sampled RESULTS/REDRESS/SK-V15 citations resolve and verdict buckets match evidence. |
| `1E-locks-evidence.md` | REVISE | Major lock verdicts checked; row `D-1E-V1-04` uses bare sibling lowerer paths. |
| `1F-coherence-scan.md` | ACCEPT | Fresh V1/SK-V15 artifact; sampled authority, gate, Pattern H, Decision Engine, and sidecar cites resolve. |
| `1F-anti-pattern.md` | REVISE | Stale V6/SK-V14 artifact; current LOC/symbol/provider claims are false at HEAD. |
| `1F-past-corpora.md` | REVISE | Stale V6/SK-V14 artifact; historical findings mostly useful, but cycle/scope and bare relative cites are malformed for V1 CH1. |

## Required V2 Fold

1. Refresh or replace `1F-anti-pattern.md` and `1F-past-corpora.md` as V1/SK-V15
   inventories; remove obsolete `RuntimeProvider`, `json_provider.rs`, old LOC,
   and eight-provider-module claims.
2. Expand every shorthand citation to a repo-root path:line or explicitly define
   a local citation base before using shorthands.
3. Reconcile `1A` frontmatter/counts with the actual `1A-SUB-022` row.
