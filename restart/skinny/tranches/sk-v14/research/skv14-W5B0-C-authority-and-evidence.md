# SK-V14 W5B.0 C: Authority And Evidence

Date: 2026-05-26.
Scope: W5B.0 active authority, evidence requirements, and wording traps.
Output: this file.

## §1 — Findings

HEAD `48888d2f9` closed V8 CRUD. The active handoff says implementation resumes
at W5B.0 LOCK14-GATE
(`restart/HANDOFF.md:5`). V8 CRUD-LOG says W5B.0 is the next sequenced step and
describes the exact Lock 14 tests
(`restart/audit/totality/astral/V8/CRUD-LOG.md:57`,
`restart/audit/totality/astral/V8/CRUD-LOG.md:67`).

W5B.0 is Lock14-only. Acceptance requires `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
to add the W5B-FRONTEND owner roster, parent-diff subject routing, and exact
tests before any grammar/codegen/xtask frontend source edits
(`restart/skinny/tranches/sk-v14/SPEC.md:719`,
`restart/skinny/tranches/sk-v14/SPEC.md:736`,
`restart/MIGRATION.md:140`).

Every exact W5B test must write its own `/tmp/skv14-w5b-<test-name>.log` and
must have its own nonzero `rg "test result: ok\\. [1-9][0-9]* passed"` proof.
Wildcard aggregate greps are rejected
(`restart/skinny/tranches/sk-v14/SPEC.md:764`).

SPEC/DISPATCH/HANDOFF agree on sequencing: W5B.0 next; W5B.1 through W5B.4
blocked until W5B.0 admits; W5C-GEN blocked until aggregate W5B closes
(`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:54`,
`restart/skinny/tranches/sk-v14/HANDOFF.md:178`,
`restart/HANDOFF.md:149`).

## §2 — Recommendations

The redress phase must run and record these exact tests:

- `w5b_lock14_frontend_owner_paths_admit`
- `w5b_lock14_frontend_rejects_w5c_subject`
- `w5b_lock14_frontend_rejects_w5d_subject`
- `w5b_lock14_frontend_rejects_modified_provider`
- `w5b_lock14_frontend_rejects_modified_template`
- `w5b_lock14_frontend_all_templates_guard_counts_8`
- `w5b_lock14_frontend_allows_grammar_provider_exception`
- `w5b_lock14_frontend_generic_owner_leak_census`

Each command needs a dedicated tee log and dedicated nonzero grep.

## §3 — Risks

`restart/skinny/tranches/sk-v14/SPEC.md:728` says the aggregate entry gate
already "has" the W5B.0 contents. Read literally, that cycles W5B.0 behind
itself. The controlling V8 CRUD/HANDOFF flow clarifies that W5B.0 adds those
contents, then W5B.1..W5B.4 may touch frontend source paths
(`restart/audit/totality/astral/V8/CRUD-LOG.md:60`,
`restart/skinny/tranches/sk-v14/HANDOFF.md:195`).

## §4 — Sources

- `restart/HANDOFF.md`
- `restart/audit/totality/astral/V8/CRUD-LOG.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`
- Read-only agent `019e65b7-4cda-7030-8682-73a059a2b966`.
