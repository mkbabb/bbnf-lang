# SK-V14 W5B-GEN-D: Verification Gates

Date: 2026-05-26.
Scope: Read-only Lock 14, grep, RESULTS, and rejection-routing audit for W5B-GEN.
Output: this file.

## §1 — Findings

Lock 14 runs before the JSON gate. `skinny/crates/bbnf-bench/src/bin/gate.rs:50`
calls `lock14_baseline::validate`, and
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:599` checks allowlists,
generated headers, provider/template topology, git freeze, BackendShape, and
generic neutrality.

The Lock 14 provider topology is still W5A-shaped and correct for W5B-GEN:
provider/template deletion remains W5C-DELETE scope. `lock14_baseline.rs:1187`
requires the eight legacy providers and seven CSS template dirs, and
`lock14_baseline.rs:1255` rejects provider/template add, delete, or rename.

W5B-GEN currently has no dedicated Lock 14 owner-path subject branch. Existing
SK-V14 owner routing stops at W5A via `lock14_baseline.rs:1105` and
`lock14_baseline.rs:1611`. If redress adds a neutral generator module or changes
Lock 14 guards, it must add W5B-GEN owner-path and parent-diff allowances or
`gate-json` will reject the commit.

`skinny/RESULTS.md` has no W5B-GEN infrastructure row. CSS rows remain
`AUDIT-FALSIFIED`, and W5B-GEN must not promote those rows unless a genuine
gate output changes. The W5A precedent left RESULTS and the rolling delta
unchanged in `restart/skinny/tranches/sk-v14/research/skv14-W5A-redress.md:16`.

## §2 — Recommendations

Use fail-closed wrappers for the amended greps so logs preserve offending
lines before exit:

```sh
cd skinny && if rg -n '\b(render_runtime_profile|RuntimeProvider|GrammarProfile|json_provider|css_l4_.*provider)\b' crates/codegen/src/{lib.rs,grammar_provider.rs}; then exit 1; fi
cd skinny && if rg -nU 'match\s+[^{]+\{[^}]*\b(Json|CssL4\w*|Bbnf\w*|GoogleSheets\w*)\b\s*=>' crates/{codegen,runtime,passes,bbnf,grammar}/src --glob '!**/tests/**'; then exit 1; fi
```

On W5B-GEN rejection, append the next REDRESS item after 210, save the attempted
patch to `/tmp/skv14-waveW5B-GEN-rejected.patch` if any source edits exist,
revert source edits, and prove `skinny/RESULTS.md` plus
`restart/skinny/ROLLING-SOTA-DELTA.md` remain unchanged.

## §3 — Risks

Changing Lock 14 without owner-path routing creates a false implementation
failure. Changing RESULTS without a new measured gate output creates a paper
admit. Both are pre-blocked by W5B-GEN discipline.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:732` through `SPEC.md:746`
- `skinny/crates/bbnf-bench/src/bin/gate.rs:50`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:599`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1105`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1187`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1255`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:1611`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-redress.md:16`
- `skinny/REDRESS.md:5197`
