# SK-V14 W5B.0 B: Provider/Template Topology

Date: 2026-05-26.
Scope: W5B.0 provider/template mutation guard and topology census.
Output: this file.

## §1 — Findings

`skinny/crates/codegen/src/grammar_provider.rs` is the W5A grammar-neutral
request-boundary module, not old provider residue.

The codegen provider layout currently has nine `*_provider.rs` files: seven
CSS L4 providers, `json_provider.rs`, and `grammar_provider.rs`. The existing
Lock 14 topology excludes `grammar_provider.rs`, so the protected legacy
provider count is eight
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1187`).

The codegen template layout currently has eight `*_templates` directories:
seven `css_l4_*_templates` directories plus `json_templates`. The existing W5A
template count checks only seven CSS template dirs
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1209`).

The W5A status guard rejects untracked, added, deleted, and renamed protected
provider/template paths but permits modified provider/template files
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1255`). The current test
`w5a_provider_template_status_allows_modify_and_grammar_provider` proves that
modified CSS providers are still allowed under W5A
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2093`).

## §2 — Recommendations

W5B.0 must tighten the topology guard for this wave:

- reject modified, added, deleted, renamed, and untracked
  `crates/codegen/src/*_provider.rs` except `crates/codegen/src/grammar_provider.rs`;
- reject modified, added, deleted, renamed, and untracked paths containing
  `_templates`, including `json_templates` and all seven CSS L4 template dirs;
- preserve `grammar_provider.rs` as the explicit neutral exception.

The required exact tests are:

- `w5b_lock14_frontend_rejects_modified_provider`
- `w5b_lock14_frontend_rejects_modified_template`
- `w5b_lock14_frontend_all_templates_guard_counts_8`
- `w5b_lock14_frontend_allows_grammar_provider_exception`

## §3 — Risks

A CSS-only template guard would miss `json_templates`, reopening the hidden
coupling caught by V8 CH5
(`restart/audit/totality/astral/V8/hardening/V2/CH5.md:34`). Allowing modified
providers/templates in W5B.0 would let frontend work smuggle provider/template
changes before W5C-GEN and W5D-DELETE.

## §4 — Sources

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `restart/audit/totality/astral/V8/hardening/V2/CH5.md`
- Read-only agent `019e65b7-6207-7882-9054-89c3b8bf33ad`.
