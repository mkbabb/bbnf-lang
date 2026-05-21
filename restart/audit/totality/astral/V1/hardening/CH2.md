# Pass Omega V1 CH2 Generality Lock 14

| Field | Value |
|---|---|
| Pass | Pass Omega |
| Cycle | V1 CHALLENGE |
| Date | 2026-05-21 |
| Lens | CH2 Generality / Lock 14 |
| Output | `restart/audit/totality/astral/V1/hardening/CH2.md` |

## Verdict

REVISE.

Omega V1 mostly preserves Lock 14: the proposed locks diff fences generated output, forbids hand-coded grammar branches in generic crates, treats fact streams as output planes rather than retained sidecars, and routes CSS/JSON work through row-plane telemetry. The packet is not REJECT-level because the proposed direction is grammar-neutral and gated.

The fold blocker is narrower: Ω-D leaves witness cardinality unresolved, and Ω-C's Lock 14 hunk allows ambiguous "Sheets/BBNF-self witness or negative control" wording. CH2 was dispatched to check generality across JSON, CSS L4, Sheets, and BBNF-self; V1 cannot leave it to G-Omega to decide whether both negative controls are required. Fleet-wide grammar-neutral claims must require a strict CSS positive lane plus both Sheets and BBNF-self fail-closed or generated-role witnesses. With only one negative control, the claim must stay scoped to the witnessed grammars.

## Evidence Table

| Check | Disposition | Evidence | CH2 finding |
|---|---|---|---|
| Governing CH2 scope | ACCEPT | PASS-OMEGA defines CH2 as Lock 14 coverage across JSON, CSS L4, Sheets, and BBNF-self, and asks whether Ω-D generalizes to non-JSON (`restart/prompts/pass-contracts/PASS-OMEGA.md:41`-`46`). ORCHESTRATOR defines CH2 as no grammar-name leak and interventions that work for CSS L4, Sheets, and BBNF-self, not only JSON (`restart/prompts/ORCHESTRATOR.md:81`-`85`). | The lens requires explicit non-JSON witness coverage, not a JSON/CSS-only pass. |
| Live Lock 14 baseline | ACCEPT | Lock 14 forbids grammar-specific code, public grammar-specific types, feature flags, and grammar branches in generic crates; new grammars must be grammar source plus workspace metadata plus optional declaration crate only (`restart/locks/LOCKS.md:78`). | The baseline is strict enough; the proposed amendments must preserve it. |
| Ω-C generated-output fence | ACCEPT | Ω-C keeps the 16-lock count fixed and authorizes no directive, BIR variant, BackendShape, public substrate API, or sidecar (`restart/audit/totality/astral/V1/ΩC-locks-amendments.md:9`-`15`). The proposed Lock 14 hunk allows grammar names only in generated `runtime/src/grammars/<name>/` output and excludes hand-coded provider enums, root aliases, generic branches, public grammar-named types, generic-root fixtures, and grammar-shaped policy mining (`restart/audit/totality/astral/V1/locks-diff.md:257`-`270`). | Generated per-grammar output is fenced correctly. |
| Generic-crate policy | ACCEPT | The Lock 14 hunk says generic crates consume generated provider manifests, generated sink/fact/value/flag surfaces, and generated grammar facts, and may not hand-code `RuntimeProvider::{Json, CssL4DeclarationValues}`, JSON/CSS renderer branches, JSON alphabets, role mining, sink callback names, or grammar-specific feature flags (`restart/audit/totality/astral/V1/locks-diff.md:272`-`279`). | This avoids JSON-only or CSS-only policy in generic crates. |
| Fact and primitive surface | ACCEPT | Fact streams are output-plane contracts, not retained sidecars (`restart/audit/totality/astral/V1/locks-diff.md:77`-`82`). Shared `bbnf-simd`, parse-that, and future regex APIs expose grammar-neutral facts and primitives only; quote, escape, control, delimiter, number, string, and no-string/no-number policy must come from generated grammar config or caller data (`restart/audit/totality/astral/V1/locks-diff.md:293`-`301`). | The proposed fact/primitive surface is grammar-neutral. |
| Telemetry surface | ACCEPT | The Lock 8 hunk requires row-plane accounting for JSON parse/direct/typed and CSS fact-stream rows (`restart/audit/totality/astral/V1/locks-diff.md:139`-`146`) and says non-JSON telemetry must feed the bench gate or a gate-consumed companion report, not prose (`restart/audit/totality/astral/V1/locks-diff.md:157`-`164`). Ω-E proposes a common telemetry schema with row/corpus/workload, verdict, strictness, output plane, Track 1/2, deltas, REDRESS id, wave id, run id, and host (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:19`). | The telemetry proposal is usable, but the fold must ensure JSON/CSS comparator names are row metadata, not universal columns, when Sheets and BBNF-self witnesses are added. |
| Ω-D non-JSON MASTER routing | ACCEPT | Ω-D marks GrammarConfig legality as partial until generated non-JSON plus negative controls pass (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:44`). It adds MP.NW6 for Lock 14 generated provider/config/sink/fact/flag/schema repair with CSS plus Sheets/BBNF-self negative controls, and MP.NW11 for Sheets and BBNF-self witnesses (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:69`-`80`). The proposed master-plan diff carries the same MP.NW6 and MP.NW11 rows (`restart/audit/totality/astral/V1/master-plan-diff.md:51`-`69`). | Ω-D is not JSON-only, but its open cardinality row must be folded. |
| CSS one-row scope | ACCEPT | Ω-B states the SK-V12 CSS declaration-values row is not full CSS parity, universal grammar closure, or SK-V13 close authority (`restart/audit/totality/astral/V1/ΩB-skinny-lessons.md:45`-`49`). Lock 8 repeats that CSS declaration-values is not full CSS parity or universal grammar closure (`restart/audit/totality/astral/V1/locks-diff.md:139`-`146`). | The packet does not over-promote the CSS row. |
| Witness cardinality | REVISE | Ω-D says G-Omega must decide whether Lock 14 negative controls require both Sheets and BBNF-self or one negative-control witness plus CSS (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:96`-`104`). T-P3 carried the same open CH2 question (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:164`-`173`, `restart/audit/totality/p3/3D-skinny-fold.md:119`-`128`). Meanwhile T-P3 3E requires CSS L4 plus Sheets/BBNF-self negative controls before fleet-wide grammar-neutral wording (`restart/audit/totality/p3/3E-grammar-generalisation.md:132`-`143`). | V1 must resolve this before convergence: both Sheets and BBNF-self are required for fleet-wide grammar-neutral claims. |
| Governance boundary | ACCEPT | The locks diff is proposed-only and requires Pass Omega CHALLENGE convergence plus G-Omega (`restart/audit/totality/astral/V1/locks-diff.md:3`-`10`). Its footer forbids using proposed v+1 text as implementation permission before G-Omega (`restart/audit/totality/astral/V1/locks-diff.md:385`-`401`). | The defect is fold wording, not premature merge authority. |

## Required Fold Actions

1. In `restart/audit/totality/astral/V1/locks-diff.md`, revise Hunk 10 so the per-wave Lock 14 gate says: "At minimum, the gate checks generated provider registry, grammar-shape role mining, generated sink/fact/value/flag ownership, primitive policy source, one strict CSS L4 positive row, both Sheets and BBNF-self fail-closed negative-control witnesses or admitted generated-role fact rows when claiming fleet-wide transfer, and decision-engine generated facts." Also add: "With only one of Sheets or BBNF-self, the claim is scoped to the witnessed grammars and may not use fleet-wide grammar-neutral wording."

2. In `restart/audit/totality/astral/V1/ΩC-locks-amendments.md`, update the Lock 14 disposition and `3C-L14-generated-output-and-per-wave-gate` rationale to name both Sheets and BBNF-self controls for fleet-wide claims. Keep the generated-output allowance and generic-crate prohibitions intact.

3. In `restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md`, remove the "Witness cardinality unresolved" blocker as an unresolved G-Omega decision. Replace it with a resolved CH2 requirement: MP.NW6/MP.NW11 require both Sheets and BBNF-self fail-closed or generated-role witnesses before MASTER, HANDOFF, BENCH, or S-P3 may call the result fleet-wide grammar-neutral.

4. In `restart/audit/totality/astral/V1/master-plan-diff.md`, update H.W4.LOCK14, MP.NW6, and MP.NW11 so they require both Sheets and BBNF-self witnesses for fleet-wide Lock 14 closure. If implementation proceeds with only one negative control, the diff must label the result "scoped non-JSON witness" rather than "fleet-wide" or "grammar-neutral closure."

5. In `restart/audit/totality/astral/V1/ΩE-skinny-corpus.md`, adjust the BENCH/COMPILER receiver wording so JSON and CSS comparator names are row metadata, not universal telemetry columns. The common telemetry schema should be grammar-neutral: grammar id, output plane, strictness, oracle/comparator id, witness kind, generated policy source, row verdict, run id, host, and REDRESS/wave provenance. JSON sonic-rs and CSS lightningcss/cssparser stay row-specific anchors.

Affected files: `restart/audit/totality/astral/V1/locks-diff.md`, `restart/audit/totality/astral/V1/ΩC-locks-amendments.md`, `restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md`, `restart/audit/totality/astral/V1/master-plan-diff.md`, and `restart/audit/totality/astral/V1/ΩE-skinny-corpus.md`.

No source, governance surface, generated runtime, `skinny/RESULTS.md`, or `skinny/REDRESS.md` edit is required by this CH2 fold.

## G-Omega Presentation

This lens blocks G-Omega presentation. Pass Omega cannot converge with an unresolved REVISE, and the CH2 fold is required before a consolidated V1 packet can present the locks diff or master-plan diff for G-Omega sign-off.
