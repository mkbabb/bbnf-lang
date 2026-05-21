---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V4
generated_at: 2026-05-21
dispositions_used: [ACCEPT, REVISE, REJECT]
source_artifacts_reviewed:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/audit/totality/p1/hardening/V3/CH2.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md
live_truth_method: "nl -ba path:line reads over T-P1 prompt, ORCHESTRATOR, V4 folded inventories, V3 accepted CH2 posture, and V3 consolidation entry rule; no source edits and no cargo tests run"
---

## Verdict

Disposition: ACCEPT.

The V4 inventory set is a metadata/provenance fold, not a substantive CH2 rewrite. It preserves the accepted V3 generality posture: grammar-name leaks and grammar-shape leaks remain distinct, CSS L4 remains admitted non-JSON row evidence without becoming universal closure, Sheets and BBNF-self implications remain explicit, `GrammarConfig` remains partial row-level repair evidence, and generated per-grammar names remain bounded by generated/rostered criteria. I found no V4 JSON-only overclaim and no weakening of CSS, Sheets, BBNF-self, or GrammarConfig partiality.

## Findings

| ID | Disposition | Target | Finding |
|---|---|---|---|
| CH2-V4-001 | ACCEPT | Governing CH2 standard | The audit uses the correct CH2 scope. ORCHESTRATOR defines totality as grammar-neutral across JSON, CSS L4, BBNF-self, Sheets, and arbitrary user grammars at `restart/prompts/ORCHESTRATOR.md:48-55`, and CH2 requires no grammar-name leak plus CSS L4 / Sheets / BBNF-self applicability at `restart/prompts/ORCHESTRATOR.md:81-85`. PASS-1 requires Lock 14 checks, 1C runtime grammar-name census, 1D JSON-vs-grammar-neutral separation, and uncited grammar-name leak rejection at `restart/prompts/totality/PASS-1-EXCAVATION.md:110-114`; the zero grammar-named generic-crate axis is restated at `restart/prompts/totality/PASS-1-EXCAVATION.md:210-212`. |
| CH2-V4-002 | ACCEPT | V4 metadata-only fold | V3 consolidation required only active-cycle/provenance repair for 1A and 1B and explicitly said V4 must not alter accepted CH2-CH6 evidence at `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:29-38`. Current 1A and 1B now show `cycle: V4` plus metadata-only provenance at `restart/audit/totality/p1/1A-substrate-evidence.md:4-9` and `restart/audit/totality/p1/1B-codegen-evidence.md:4-10`; 1C, 1D, 1E, and 1F likewise identify V4 as metadata-only at `restart/audit/totality/p1/1C-runtime-evidence.md:4-10`, `restart/audit/totality/p1/1D-skinny-lessons.md:4-10`, `restart/audit/totality/p1/1E-locks-evidence.md:4-10`, `restart/audit/totality/p1/1F-anti-pattern.md:4-10`, `restart/audit/totality/p1/1F-coherence-scan.md:4-10`, and `restart/audit/totality/p1/1F-past-corpora.md:4-10`. |
| CH2-V4-003 | ACCEPT | V3 accepted posture preserved | V3 CH2 accepted exactly the posture now being rechecked: grammar-name versus grammar-shape split, CSS L4 boundary, Sheets/BBNF-self implications, partial `GrammarConfig`, generated per-grammar allowance boundaries, and no JSON-only overclaim at `restart/audit/totality/p1/hardening/V3/CH2.md:25-42`. V3 consolidation records CH2 as ACCEPT with those load-bearing results at `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:22-24`. |
| CH2-V4-004 | ACCEPT | Grammar-name leaks still flagged | V4 does not paper-close Lock 14 grammar-name leaks. 1B still calls hardcoded `RuntimeProvider::{Json, CssL4DeclarationValues}` and renderer branching a grammar-name leak at `restart/audit/totality/p1/1B-codegen-evidence.md:47-58`; 1C still flags runtime root hardcoded grammar names and hand-written per-grammar runtime files at `restart/audit/totality/p1/1C-runtime-evidence.md:58-60` and `restart/audit/totality/p1/1C-runtime-evidence.md:79-85`; 1E keeps Lock 14 drifted at `restart/audit/totality/p1/1E-locks-evidence.md:76` and `restart/audit/totality/p1/1E-locks-evidence.md:92`. |
| CH2-V4-005 | ACCEPT | Grammar-shape leaks still separate | V4 keeps grammar-shape policy leaks separate from literal grammar-name leaks. 1B distinguishes recognizer JSON punctuation and materialization role mining at `restart/audit/totality/p1/1B-codegen-evidence.md:49-50` and `restart/audit/totality/p1/1B-codegen-evidence.md:58-60`. 1D requires Sheets and BBNF-self proof beyond JSON role mining at `restart/audit/totality/p1/1D-skinny-lessons.md:80-81`. 1E defines the name/shape/generated-name taxonomy at `restart/audit/totality/p1/1E-locks-evidence.md:112-119`, and 1F coherence retains the same Lock 14 drift at `restart/audit/totality/p1/1F-coherence-scan.md:25` and `restart/audit/totality/p1/1F-coherence-scan.md:55-56`. |
| CH2-V4-006 | ACCEPT | CSS L4 not weakened | V4 preserves CSS declaration-values as admitted same-plane non-JSON evidence while refusing universal CSS or totality closure. 1A keeps the CSS fact-stream row as an admitted evidence/category gap at `restart/audit/totality/p1/1A-substrate-evidence.md:45-57`; 1C repeats that CSS is admitted row evidence with a runtime substrate/telemetry classification gap at `restart/audit/totality/p1/1C-runtime-evidence.md:70-72` and `restart/audit/totality/p1/1C-runtime-evidence.md:101-102`; 1D says one CSS row remains accepted while the full CSS parity matrix is pending at `restart/audit/totality/p1/1D-skinny-lessons.md:87-95`; 1E preserves CSS admission but keeps full SOTA gated at `restart/audit/totality/p1/1E-locks-evidence.md:54-57`. |
| CH2-V4-007 | ACCEPT | Sheets and BBNF-self not weakened | V4 still names Sheets and BBNF-self as first-class generality obligations. 1B says Sheets formula/function/array/reference/error atoms and BBNF-self directive/operator/Pratt classes are not covered by JSON role mining at `restart/audit/totality/p1/1B-codegen-evidence.md:64-69`. 1D adds pending Sheets and BBNF-self rows with generated fixture consumers at `restart/audit/totality/p1/1D-skinny-lessons.md:80-81` and repeats the synthesis obligation at `restart/audit/totality/p1/1D-skinny-lessons.md:109`. 1F coherence routes CSS/Sheets/BBNF-self recognizer fixtures as the Lock 14 consumer at `restart/audit/totality/p1/1F-coherence-scan.md:69`. |
| CH2-V4-008 | ACCEPT | `GrammarConfig` partiality preserved | V4 does not promote `GrammarConfig` to full generic-crate closure. 1D's hardening fold says the status was downgraded to partial row-level Lock 14 repair evidence at `restart/audit/totality/p1/1D-skinny-lessons.md:31-39`, and the main row repeats "proved as direction; partial as generic repair" at `restart/audit/totality/p1/1D-skinny-lessons.md:52` and "partial row-level repair" at `restart/audit/totality/p1/1D-skinny-lessons.md:74`. 1B independently keeps GrammarConfig/provider direction partial at `restart/audit/totality/p1/1B-codegen-evidence.md:47`. |
| CH2-V4-009 | ACCEPT | Generated per-grammar allowance bounded | V4 preserves the generated-name allowance boundary rather than making a blanket exception. 1C allows generated per-grammar type names only if generated from metadata/source at `restart/audit/totality/p1/1C-runtime-evidence.md:85`; 1E's Lock 14 candidate allows generated runtime under `runtime/src/grammars/<name>/` only when produced by the rostered generator and guarded by `lock14_baseline::validate`, while generic crates must still reject grammar-name branches and grammar-shape policy leaks at `restart/audit/totality/p1/1E-locks-evidence.md:107`; the taxonomy repeats that generated grammar-owned names are admitted row evidence, not blanket generic-crate exception, at `restart/audit/totality/p1/1E-locks-evidence.md:116-119`. |
| CH2-V4-010 | ACCEPT | No JSON-only overclaim introduced | V4 inventories keep JSON evidence scoped. 1D explicitly says single-substrate evidence is "proved for JSON; grammar-neutral rule candidate" at `restart/audit/totality/p1/1D-skinny-lessons.md:39` and `restart/audit/totality/p1/1D-skinny-lessons.md:45`; it also says JSON-only changes cannot prove grammar-general behavior at `restart/audit/totality/p1/1D-skinny-lessons.md:105`. 1E narrows Lock 1 to scoped JSON lazy-offset evidence only at `restart/audit/totality/p1/1E-locks-evidence.md:48` and `restart/audit/totality/p1/1E-locks-evidence.md:63`. 1C keeps focused runtime test status UNKNOWN without a committed transcript at `restart/audit/totality/p1/1C-runtime-evidence.md:114-128`. |

## Residual Notes

No CH2-blocking REVISE remains. The residual risks are already correctly classified as future synthesis or implementation work: Lock 14 census must include grammar-shape leaks, generated-name exceptions need rostered criteria, and CSS/Sheets/BBNF-self fixtures remain required consumers before grammar-general closure.
