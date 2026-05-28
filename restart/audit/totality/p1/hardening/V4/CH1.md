---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V4
disposition: REVISE
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
historical_auxiliaries_not_live:
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH1 - CORRECTNESS (SK-V15 T-P1 V4)

## Verdict

REVISE.

V4 folded the specific V3 brace-path and directory-local CSS `config.rs`
citation defects, and the 1A / 1F frontmatter counts still reconcile with their
tables. CH1 cannot ACCEPT because the V4 packet still contains residual
non-root-resolving shorthand citations in 1A and 1F, and 1A / 1B retain two
stale prose claims that call the current V4 inventories V3.

Scope was the six live inventories named by the V4 dispatch context at
`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:9-17`.
The two 1F auxiliaries are historical only per
`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:19-22`.

## Evidence

CH1 authority requires resolving file:line evidence, not recalled LOC, per
`restart/prompts/ORCHESTRATOR.md:81-84` and
`restart/prompts/totality/PASS-1-EXCAVATION.md:104-108`. V4 specifically asks
CH1 to verify all six live inventories are V4, root-resolving citation greps
return zero residual shorthand or brace-path cites, and 1A / 1F count tables
match frontmatter at
`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:53-57`.

All six live inventories have V4 frontmatter:

```text
restart/audit/totality/p1/1E-locks-evidence.md:4:cycle: V4
restart/audit/totality/p1/1D-skinny-lessons.md:4:cycle: V4
restart/audit/totality/p1/1F-coherence-scan.md:4:cycle: V4
restart/audit/totality/p1/1B-codegen-evidence.md:4:cycle: V4
restart/audit/totality/p1/1C-runtime-evidence.md:4:cycle: V4
restart/audit/totality/p1/1A-substrate-evidence.md:4:cycle: V4
```

But two V4 inventories still self-describe as V3:

```text
restart/audit/totality/p1/1B-codegen-evidence.md:37:... | Cycle is V3; V1 names are retained only inside finding IDs and prior PASS-IMPL evidence anchors. |
restart/audit/totality/p1/1A-substrate-evidence.md:56:Dispatch context: this V3 inventory treats SK-V15 as PRUNE-then-REBUILD after
```

The exact V3 F01 citation greps now return zero output:

```sh
rg -n '`[^`]*\.rs:[0-9][^`]*`' restart/audit/totality/p1/1*.md \
  | rg -v '`(restart|skinny|crates|xtask)/|`Cargo\.toml:|`skinny/Cargo\.toml:'
```

```sh
rg -n '`[^`]*\{[^`]*\}[^`]*\.(rs|md|toml):[0-9][^`]*`' \
  restart/audit/totality/p1/1*.md
```

However, a token-level shorthand scan still finds non-root-resolving cites:

```sh
rg -n -o '`[^`]*(?:\.md|\.rs|\.toml):[0-9][^`]*`' \
  restart/audit/totality/p1/1*.md \
  | rg -v ':`(restart|skinny|crates|xtask)/|:`Cargo\.toml:|:`skinny/Cargo\.toml:'
```

Output:

```text
restart/audit/totality/p1/1F-coherence-scan.md:89:`generated.rs:25`
restart/audit/totality/p1/1F-coherence-scan.md:89:`generated.rs:71`
restart/audit/totality/p1/1F-coherence-scan.md:89:`generated.rs:619`
restart/audit/totality/p1/1A-substrate-evidence.md:83:`json/scan.rs:1`
restart/audit/totality/p1/1A-substrate-evidence.md:83:`json/sink.rs:1`
```

The colon-only shorthand scan also fails:

```sh
rg -n -o '`:[0-9][0-9]*(?:-[0-9][0-9]*)?`' \
  restart/audit/totality/p1/1*.md
```

Output:

```text
restart/audit/totality/p1/1F-coherence-scan.md:89:`:71`
restart/audit/totality/p1/1F-coherence-scan.md:89:`:619`
restart/audit/totality/p1/1F-coherence-scan.md:89:`:783`
restart/audit/totality/p1/1F-coherence-scan.md:89:`:1331`
restart/audit/totality/p1/1F-coherence-scan.md:165:`:71`
restart/audit/totality/p1/1F-coherence-scan.md:165:`:619`
restart/audit/totality/p1/1F-coherence-scan.md:165:`:783`
restart/audit/totality/p1/1F-coherence-scan.md:165:`:1331`
```

Those shorthand paths do not resolve from repo root: `json/scan.rs`,
`json/sink.rs`, and `generated.rs` are absent at the repository root. The
corresponding claims are at
`restart/audit/totality/p1/1A-substrate-evidence.md:83`,
`restart/audit/totality/p1/1F-coherence-scan.md:89`, and
`restart/audit/totality/p1/1F-coherence-scan.md:165`.

1A count reconciliation passes. Frontmatter says `6 / 7 / 4 / 5` at
`restart/audit/totality/p1/1A-substrate-evidence.md:22-26`. Table counting
rows `1A-SUB-001` through `1A-SUB-022` gives one
`partial / JSON-example implemented` plus five `implemented`, seven
`unimplemented`, four `impl_exceeds_spec`, and three `unknown` plus two
`partial / UNKNOWN routed`.

1F count reconciliation passes. Frontmatter says `3 / 11 / 0 / 2` at
`restart/audit/totality/p1/1F-coherence-scan.md:58-62`. Counting COH rows in
the spec-claim table at
`restart/audit/totality/p1/1F-coherence-scan.md:74-89` gives three
`implemented`, eleven `unimplemented`, and two `unknown` rows.

## Findings

| id | disposition | severity | finding | evidence | required fold |
|---|---|---|---|---|---|
| CH1-V4-001 | REVISE | high | Residual shorthand citations remain. The V3 F01 brace and CSS `config.rs` defects are gone, but V4 introduces or leaves shorthand cites that do not resolve from repo root. | `restart/audit/totality/p1/1A-substrate-evidence.md:83`; `restart/audit/totality/p1/1F-coherence-scan.md:89`; `restart/audit/totality/p1/1F-coherence-scan.md:165`; failing token-level greps above. | Expand every `json/scan.rs:1`, `json/sink.rs:1`, `generated.rs:<line>`, and `:<line>` citation to full repo-root path:line form. For COH-016, either list every CSS runtime root path needed to support the "each generated CSS runtime" claim or cite a root-resolving transcript that proves all seven profiles share those line positions. |
| CH1-V4-002 | REVISE | medium | Two inventories have V4 frontmatter but stale V3 self-description in live prose. | 1A frontmatter is V4 at `restart/audit/totality/p1/1A-substrate-evidence.md:4`, but line 56 says "this V3 inventory"; 1B frontmatter is V4 at `restart/audit/totality/p1/1B-codegen-evidence.md:4`, but line 37 says "Cycle is V3". | Replace those two stale cycle claims with `V4` or cycle-neutral wording. Re-run `rg -n 'Cycle is V3|this V3 inventory'` over the six live inventories and require zero output. |
| CH1-V4-003 | ACCEPT | none | All six live inventories carry `cycle: V4` frontmatter. | `restart/audit/totality/p1/1A-substrate-evidence.md:4`; `restart/audit/totality/p1/1B-codegen-evidence.md:4`; `restart/audit/totality/p1/1C-runtime-evidence.md:4`; `restart/audit/totality/p1/1D-skinny-lessons.md:4`; `restart/audit/totality/p1/1E-locks-evidence.md:4`; `restart/audit/totality/p1/1F-coherence-scan.md:4`. | None. |
| CH1-V4-004 | ACCEPT | none | 1A frontmatter counts still match the 1A table after the V4 carry-forward. | Frontmatter `6 / 7 / 4 / 5` at `restart/audit/totality/p1/1A-substrate-evidence.md:22-26`; row counts over `restart/audit/totality/p1/1A-substrate-evidence.md:65-86`. | None. |
| CH1-V4-005 | ACCEPT | none | 1F frontmatter counts include the V4 COH-016 addition and match the table. | Frontmatter `3 / 11 / 0 / 2` at `restart/audit/totality/p1/1F-coherence-scan.md:58-62`; `COH-016` is listed in first-cycle additions at `restart/audit/totality/p1/1F-coherence-scan.md:57`; row counts over `restart/audit/totality/p1/1F-coherence-scan.md:74-89`. | None. |

## Required Fold

V5 must fold `CH1-V4-001` and `CH1-V4-002` before CH1 can ACCEPT:

1. Replace the non-root-resolving 1A note citations at
   `restart/audit/totality/p1/1A-substrate-evidence.md:83` with
   `skinny/crates/runtime/src/grammars/json/scan.rs:1` and
   `skinny/crates/runtime/src/grammars/json/sink.rs:1`.
2. Rewrite the COH-016 FNV row at
   `restart/audit/totality/p1/1F-coherence-scan.md:89` and the matching gap row
   at `restart/audit/totality/p1/1F-coherence-scan.md:165` so every CSS runtime
   and generator-template line citation is root-resolving. No `generated.rs:<n>`
   or `:<n>` shorthand may remain.
3. Replace stale V3 self-descriptions at
   `restart/audit/totality/p1/1A-substrate-evidence.md:56` and
   `restart/audit/totality/p1/1B-codegen-evidence.md:37`.
4. Re-run these checks and require zero output:

```sh
rg -n -o '`[^`]*(?:\.md|\.rs|\.toml):[0-9][^`]*`' \
  restart/audit/totality/p1/1*.md \
  | rg -v ':`(restart|skinny|crates|xtask)/|:`Cargo\.toml:|:`skinny/Cargo\.toml:'

rg -n -o '`:[0-9][0-9]*(?:-[0-9][0-9]*)?`' \
  restart/audit/totality/p1/1*.md

rg -n '`[^`]*\{[^`]*\}[^`]*\.(rs|md|toml):[0-9][^`]*`' \
  restart/audit/totality/p1/1*.md

rg -n 'Cycle is V3|this V3 inventory' \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
```

No REJECT is warranted. The open CH1 defects are bounded to citation
resolvability and stale cycle prose; V4 count reconciliation passes.
