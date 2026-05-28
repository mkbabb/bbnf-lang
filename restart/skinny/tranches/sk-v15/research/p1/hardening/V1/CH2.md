# CH2 Generality - SK-V15 S-P1 V1

Disposition: ACCEPT after fold.

Initial result: REVISE. CH2 objected that generated JSON wrappers, schema-specific `parse_type_*` leaves, and sidecar/parser file drift could be read as primitive antecedents.

Folded evidence:

- `p1e-normalized-attribution.tsv` maps every row to a primitive boundary and S-P2 antecedent status.
- `runtime::generated_json::scan::neon::scan` rows are marked generated JSON wrappers over grammar-neutral scanner primitives, allowed only after the generic boundary is cited.
- Schema-shaped product builders such as `parse_type_mesh`, `parse_type_plugin_ordered`, and `parse_type_unicode_mixed_document` are marked `schema-wrapper` and blocked as primitives.
- `profile_direct::parse_only_checksum` sidecar drift is marked `unknown/harness/sidecar-symbolization` and blocked as parser proof.

Coverage is 17/17 corpora across P1-A/P1-B/P1-C/P1-D, so the float-row overfit concern is cleared.
