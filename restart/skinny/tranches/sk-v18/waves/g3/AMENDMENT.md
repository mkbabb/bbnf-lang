# G3 Gate Amendment — emit_shape_source widens to lowered-artifact (recorded 2026-06-02)

## Finding (G3 research, waves/g3/research.md)
After G1+G2, JSON lowers to a SinkOnlyProgram (policy_summary.backend_shape == SinkOnly) but CSS
does NOT lower (the ratified G2 LOWERING GAP — first_unsupported rejects @token/@pretty/->), carrying
a CssScanProgram with NO BackendShape. SPEC §6 conjunct-5's literal "shape from
program.policy_summary.backend_shape" is structurally IMPOSSIBLE for CSS (no policy_summary). The
genuinely-neutral un-fork dispatches on the lowered-ARTIFACT TYPE
(LoweredArtifact{ SinkOnly(SinkOnlyProgram) | Scan(CssScanProgram) }), the variant CHOSEN by the
construct-kind predicate first_unsupported().is_none() — grammar-DERIVED, not a grammar tag, not a
config field. (Forcing CSS to cost-model to a synthetic BackendShape would push it through
lower_to_rust, which first_unsupported forbids = a fabricated-shape relocated seam = L1 REJECT;
correctly rejected.)

## Amendment (recorded; same root cause + pattern as G1 split + G2 lowering-gap)
SPEC §6 conjunct-5 `emit_shape_source == lowered_program` WIDENS: the un-forked render reads its
output-shape from the LOWERED ARTIFACT (the LoweredArtifact variant for the scan path ∨
policy_summary.backend_shape for the sink path), NEVER from a target.*/contract.* field. The BINDING
firewall is UNCHANGED (no target.* read); only the over-specific single-field sub-clause widens to
admit the variant discriminator. The binding falsifier: grep the un-forked render + lower_for_runtime
body for any target.*/contract.* read == 0 (strictly STRONGER than the literal), AND the variant is
COMPUTED (first_unsupported predicate) not CONFIGURED (no RuntimeEmitterKind, no emitter field).

## Disposition
Wave-level amendment; folds into SPEC §6 + LOCKS at the next Pass Omega. Does NOT relax the un-fork
(RuntimeEmitterKind DELETED, ONE render path) or the firewall (no target.* read). It records that the
neutral discriminator is the artifact TYPE (construct-derived), not a BackendShape value — the only
shape the un-fork can take given CSS provably never lowers.
