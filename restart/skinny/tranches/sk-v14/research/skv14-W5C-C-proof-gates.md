# SK-V14 W5C-C: Provider-Free Proof Gates

Date: 2026-05-26.
Scope: W5C-GEN falsifiability commands and proof-carry.
Output: this file.

## §1 — Findings

W5C must preserve the W5A/W5B proof carry while removing live provider dispatch:
`regen-css`, all seven `check-css-l4-*` commands, `check-json`, and the Lock 14
parent-diff test remain same-wave consumers.

The provider/template residue counts are non-zero before W5D by design. The W5C
proof is reachability, not deletion.

## §2 — Recommendations

Run these W5C exact checks after implementation:

```sh
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w5c_gen_owner_paths_admit --profile ax-iter -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen_rejects_profile_only_css_emission --profile ax-iter -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen_css_runtime_output_depends_on_frontend_source_hash --profile ax-iter -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_json_request_matches_emit_from_source --profile ax-iter -- --exact
```

Then run `cargo xtask regen-css`, the seven CSS L4 companion checks,
`cargo xtask check-json`, and `cargo xtask gate-json --check-results
--skv14-existing-results-capture`.

## §3 — Risks

The W5C reachability grep must allow provider/template files to exist but must
reject live production references from `lib.rs` and `grammar_provider.rs`.
Provider/template deletion belongs only to W5D.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 8C.
- W5A/W5B close packets under `restart/skinny/tranches/sk-v14/research/`.
