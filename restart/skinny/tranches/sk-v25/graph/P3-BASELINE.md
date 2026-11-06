# Architecture exporter P3 — gate-first P2 baseline

Status: **FROZEN RED baseline captured before collector replacement.**

Declared implementation model: `gpt-5.6-sol`; reasoning effort: `xhigh`.
The implementing agent did not delegate. This record is an implementation
baseline, not a review or an authorization for graph-directed movement.

## Source snapshot

The P2 implementation was hashed before any P2 collector or structural module
was replaced. `sha256sum` over the sorted per-file checksum ledger was:

```text
c349aefa215cf9c9a7a79e46e0ccfe62458592b9d346ceb0a9fd456ee400d602  p2-source-files.sha256
```

The ledger entries were:

```text
bc2430ba6cbb96422e65512a2a46d485bef17e6884b2ed207c8f82031f571785  xtask/architecture/frontend.mjs
c0218ebf373d436f89ffb8fa6302dfdf6df38ae3caae829be5132ecaf0d577fc  xtask/src/architecture/cargo.rs
891e38d10a2cac6ef530beed68876485aff18e6200496d69461d212e44992b0f  xtask/src/architecture/frontend.rs
8b84205cf6d7e544968ed3beaaf3cc1c7f1e0d2095d1ac97a745e7c645f3ccf4  xtask/src/architecture/graph.rs
2d6d007253a5693a72cbca5dfd57639e81bb11fd65e61da7cdbe147f6b111aec  xtask/src/architecture/inventory.rs
0c551ada71777e138e1efdeaff9c789b2854393c3fc72b1d284830a7df71f9f3  xtask/src/architecture/mod.rs
370549e521ac066ed12053258643bcc5c8c540aeded1f56aa66f39d738b06e33  xtask/src/architecture/output.rs
2ca54bc89a3efea24bbf30637f2391f88a8495c5544ffba1945975603f841c85  xtask/src/architecture/rust.rs
28933d468aebecbb93123fcf33514c9b73be6f353ea296705a6a56a0aa9eff0d  xtask/Cargo.toml
4ee004857ff3d02556bd5bf84edc3c34b34f60d6b20e11048df22fcb2170a910  xtask/src/lib.rs
6b23a8309edbf6182352d0dd8c2a663a7e9d0ad7944d4b2b6ef8ceb13b735e23  xtask/src/main.rs
```

The promoted bank source hash was:

```text
dd228c890bbbc60d60c97f053971a86ed3a3180074590e469998418107498faf  xtask/tests/architecture_p3_counterexamples.rs
```

The workspace `Cargo.lock` hash before and after the baseline command was
identical:

```text
a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908  Cargo.lock
```

## Exact P2 command and result

```text
cargo test -p xtask --test architecture_p3_counterexamples -- --test-threads=1
```

Result: exit 101; **0 passed, 10 failed, 0 ignored, 0 measured, 0 filtered
out**. The captured combined-output SHA-256 was:

```text
bbfc985a84a04d2b92a4890753e8df1d51b4e415a19cf37affd1255cfa83ca30
```

Exact failing tests and reproduced P2 failure:

1. `p3_manifest_declares_the_complete_typed_constellation` —
   `architecture/constellation-v3.toml` was absent.
2. `p3_inventory_rejects_two_semantic_mappings_for_one_physical_path` — two
   mappings for `fixture:src/lib.rs` canonicalized as `Ok(())`.
3. `p3_rust_inline_cfg_extern_prefix_test_and_span_counterexamples_fail_closed`
   — `/tmp` canonicalized to `/private/tmp` while the supplied root did not,
   aborting analysis before sound attribution; P2 therefore proved neither the
   exact-positive nor exact-negative bank.
4. `p3_unknown_is_required_red_for_every_applicable_projection` — an explicit
   `ResolutionConfidence::Unknown` produced no required projection failure.
5. `p3_projection_hash_binds_complete_edge_semantics` — changing only resolver
   identity retained `012f7ed11be11845`; the value was non-SHA-256.
6. `p3_shard_hash_binds_typed_cut_records` — adding/removing the outgoing cut
   retained `089bc907b544c769`.
7. `p3_component_identity_is_projection_qualified_and_witnesses_are_edge_typed`
   — the SCC ID omitted `rust_module`; P2 witnesses were node strings rather
   than typed edge identities.
8. `p3_privacy_validation_covers_every_serialized_string_field` — absolute ID,
   label, attribute, obligation subject, and reason canonicalized as `Ok(())`.
9. `p3_frontend_false_exact_cases_are_quarantined_as_site_specific_unknowns` —
   `VITE_ALIAS_RESOLUTION_UNSOUND` and the other site-specific frontend Unknowns
   were absent while the heuristic collector remained active.
10. `p3_early_node_failure_still_publishes_one_complete_failure_generation` —
    removing `node` from `PATH` exited nonzero with no `CURRENT`, generation,
    binding, or diagnostic evidence.

These failures remain RED evidence. No P2 false edge, silence, short hash, or
missing artifact is reclassified as passing.
