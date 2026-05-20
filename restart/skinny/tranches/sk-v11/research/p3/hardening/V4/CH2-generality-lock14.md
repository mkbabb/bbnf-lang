# SK-V11 S-P3 V4 CH2: Generality / Lock 14

Pass: S-P3 Synthesis-Plan.
Cycle: V4.
Lens: CH2 GENERALITY / Lock 14.
Date: 2026-05-20.
Output: this file.
Scope: stability check that V4 preserved the V3 CH2 semantics: W1a/W1b/W2
non-JSON measured-proof sequencing and generic JSON-policy blocks.
Disposition: ACCEPT.

## Verdict

ACCEPT.

V4 preserves the V3 CH2 contract. The governing S-P3 lens still requires every
shortlisted candidate to carry grammar-neutral generality, requires non-JSON
CSS L4 / Sheets / BBNF-self proof for generic-crate edits, and fails any wave
that lets JSON policy into generic crates
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:116-120`). Orchestrator
convergence also requires folds before advancement and two clean cycles, so this
V4 stability pass is supposed to preserve the V3 accepted semantics rather than
invent new source authority (`restart/prompts/ORCHESTRATOR.md:112-121`;
`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`).

I found no CH2 regression in V4.

## Evidence

| Check | Assessment | Evidence |
|---|---|---|
| V4 is still a draft packet with no behavior-source authority | ACCEPT | `SPEC.md` is `Cycle: V4 draft`, says S-P3 CHALLENGE must converge before wave dispatch authority, and authorizes no behavior source change until convergence and the selected wave entry gate pass (`restart/skinny/tranches/sk-v11/SPEC.md:3-12`). The dispatch prompt repeats that S-P3 V4 has not converged and authorizes no behavior source work yet (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:8-14`). |
| The W1a -> W1b -> W2 measured-proof sequence is preserved | ACCEPT | P3-B keeps W1a as the non-JSON gate/report lane, W1b as exactly one generated non-JSON baseline plus independent oracle, and W2 as the CSS L4 generated intervention that consumes the W1b baseline and cannot create the first measurable non-JSON row (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:67-69`). Its topology summary repeats that W1a blocks W1b, W1b blocks W2, and W2 blocks generic C1-C7 behavior waves because SK-V11 requires exercised non-JSON generality, not prose (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:78-86`). The dispatch prompt mirrors the same dependency order (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:65-72`). |
| W1a remains gate/report only and cannot claim baseline authority | ACCEPT | SPEC W1a is C9 accounting plus Lock 14 gate/report infrastructure with no parser row movement (`restart/skinny/tranches/sk-v11/SPEC.md:283-286`). Its tasks add gate fixtures for grammar id, domain, output plane, comparator/oracle, Track 2/oracle independence, run id, host, feature mask, same-wave consumer class, and producer-only telemetry rejection (`restart/skinny/tranches/sk-v11/SPEC.md:299-307`). Its exit gate rejects missing non-JSON fields and producer-only telemetry, keeps JSON `gate-json` green, moves no JSON row, and claims no generated non-JSON baseline authority (`restart/skinny/tranches/sk-v11/SPEC.md:308-315`). |
| W1b creates exactly one measured baseline and independent oracle before intervention | ACCEPT | SPEC W1b entry requires W1a closed and CHALLENGE selection of exactly one non-JSON target, preferring CSS L4 then Sheets then BBNF-self, with the independent oracle/Track 2 path named (`restart/skinny/tranches/sk-v11/SPEC.md:345-347`). Its tasks stand up one generated non-JSON direct or typed parser baseline row, prove strict output equality and gate consumption, and prove `json_provider` does not leak JSON policy into the selected generated parser (`restart/skinny/tranches/sk-v11/SPEC.md:349-356`). Its exit gate requires generated Track 1 baseline, independent oracle/Track 2, strict equality, baseline throughput with provenance fields, no JSON policy leak, and no behavior row admission (`restart/skinny/tranches/sk-v11/SPEC.md:357-367`). |
| W2 consumes W1b and owns the first non-JSON intervention admit | ACCEPT | SPEC W2 entry requires W1b closed and names the generated non-JSON intervention, scalar oracle, independent Track 2/oracle, baseline Mbps, target threshold, and Lock 14 proof (`restart/skinny/tranches/sk-v11/SPEC.md:397-400`). Its tasks consume the W1b baseline and state that W2 may not create the first measurable non-JSON row (`restart/skinny/tranches/sk-v11/SPEC.md:402-409`). Its exit gate requires generated non-JSON Track 1 plus independent Track 2/oracle, strict output equality, Track 1 at least `ceil(W1b_css_baseline_mbps * 1.01)`, SIMD scalar differential/checkasm when applicable, JSON guard preservation if refreshed, and no JSON policy in generic crates or runtime outside generated per-grammar code (`restart/skinny/tranches/sk-v11/SPEC.md:411-421`). |
| The non-JSON proof is measured and gate-consumed, not prose | ACCEPT | P3-A C6 makes generated FIRST/prefix/lookahead dispatch the mandatory non-JSON carrier, with CSS L4 declaration values first, Sheets second, BBNF-self third; it requires a generated non-JSON direct or typed benchmark row with independent oracle and names the W1b baseline / W2 1% improvement rule (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:332-357`). P3-C rejects any non-JSON claim that lacks a generated row, independent oracle on the same output plane, before/after Mbps, and gate-consumed grammar id (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:50-57`). P3-D requires non-JSON grammar rows to extend existing gate-consumed fields in the same wave and rejects non-JSON admission unless comparator/oracle identity, source artifact, output plane, and independence proof are gate-consumed (`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:154-172`, `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:197-214`). |
| Generic JSON-policy blocks are preserved | ACCEPT | SPEC close/non-negotiables prohibit generic JSON policy, require every generic/codegen/runtime-outside-JSON edit to carry same-wave CSS L4 / Sheets / BBNF-self proof, and restrict generated output to named inputs (`restart/skinny/tranches/sk-v11/SPEC.md:54-57`, `restart/skinny/tranches/sk-v11/SPEC.md:172-179`). SPEC 2.2 then makes this an every-wave exit gate: no generic branch selects JSON/corpus/object/array/field/layout roles, grammar facts are generated metadata, the non-JSON proof is run and consumed in the same wave when generic behavior changes, `json_provider` must be replaced/bypassed/proven untouched before a non-JSON generality claim, and generic/codegen/runtime edits revert together on proof failure (`restart/skinny/tranches/sk-v11/SPEC.md:229-245`). |
| P3-E/P3-F carry the same generic policy block | ACCEPT | P3-E hard-blocks generic-crate JSON policy leakage and states Lock 14 requires generated per-grammar policy plus non-JSON proof (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:72-73`). Its W1a/W1b/W2 pre-blocks forbid JSON-provider claims without non-JSON benchmark, generic JSON policy, prose-only Lock 14 proof, renamed JSON helpers, and first-baseline creation in W2 (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:154-170`). P3-F says Lock 14 must be measured, not asserted; before generic/codegen/runtime-outside-JSON edits claim generality, a wave must stand up a generated non-JSON benchmark and consume its telemetry in the same wave gate (`restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md:34-38`). |
| V3 acceptance criteria were carried forward | ACCEPT | V3 CH2 accepted the same generic-crate/codegen JSON-policy blocks, executable CSS/Sheets/BBNF-self proof requirements, W1a/W1b/W2 sequencing, and no-paper-close rule (`restart/skinny/tranches/sk-v11/research/p3/hardening/V3/CH2-generality-lock14.md:35-43`). The V3 consolidation records CH2 ACCEPT and specifically says V4 should be a stability cycle that bumps the packet to V4 and preserves V3 semantics (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:29-30`, `restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`). |

## Residual Watch

The V3 watch item still applies: W4 says W2's non-JSON proof remains valid for
generic edits (`restart/skinny/tranches/sk-v11/SPEC.md:506-509`). This is not a
V4 CH2 REVISE because SPEC 2.2 independently requires same-wave
CSS/Sheets/BBNF-self proof whenever generic behavior changes
(`restart/skinny/tranches/sk-v11/SPEC.md:229-245`). A later W4 CH2 review should
reject any plan that treats W2's prior proof as a reusable substitute for that
same-wave proof.

No source edits were made.
