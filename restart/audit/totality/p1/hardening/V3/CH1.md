# CH1 - CORRECTNESS (SK-V15 T-P1 V3)

Verdict: REVISE

Score: 4 / 5 CH1 focus checks pass. The packet cannot ACCEPT while
root-resolving citation repair still has unresolved shorthand and brace-path
citations.

Scope checked: `restart/prompts/totality/PASS-1-EXCAVATION.md`,
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z, V3 challenge context, V2
consolidated hardening, V2 CH1-CH7, current inventories `1A` through `1F`, and
the superseded 1F auxiliary files at HEAD `5a9ef7f43`.

Static challenge audit only. No source files, inventories, staging, or commits
were changed.

## Evidence

Root-resolving citation check failed:

```sh
rg -n '`[^`]*\.rs:[0-9][^`]*`' restart/audit/totality/p1/1*.md \
  | rg -v '`(restart|skinny|crates|xtask)/|`Cargo\.toml:|`skinny/Cargo\.toml:'
```

Output:

```text
restart/audit/totality/p1/1C-runtime-evidence.md:78:| `css_l4_declaration_values_extended` ... `css_l4_declaration_values_extended/config.rs:1`-`9`. |
restart/audit/totality/p1/1C-runtime-evidence.md:79:| `css_l4_stylesheet_selectors` ... `css_l4_stylesheet_selectors/config.rs:1`-`9`. |
restart/audit/totality/p1/1C-runtime-evidence.md:80:| `css_l4_visual_functions` ... `css_l4_visual_functions/config.rs:1`-`9`. |
restart/audit/totality/p1/1C-runtime-evidence.md:81:| `css_l4_at_rules_and_media` ... `css_l4_at_rules_and_media/config.rs:1`-`9`. |
restart/audit/totality/p1/1C-runtime-evidence.md:82:| `css_l4_vendor_and_custom_atrules` ... `css_l4_vendor_and_custom_atrules/config.rs:1`-`9`. |
restart/audit/totality/p1/1C-runtime-evidence.md:83:| `css_l4_nested_layout` ... `css_l4_nested_layout/config.rs:1`-`9`. |
```

Those six paths do not resolve from repo root:

```sh
for p in css_l4_declaration_values_extended/config.rs \
  css_l4_stylesheet_selectors/config.rs css_l4_visual_functions/config.rs \
  css_l4_at_rules_and_media/config.rs css_l4_vendor_and_custom_atrules/config.rs \
  css_l4_nested_layout/config.rs; do test -f "$p" && echo FOUND "$p" || echo MISSING "$p"; done
```

Output: all six printed `MISSING`. The corresponding root paths do exist under
`skinny/crates/runtime/src/grammars/<profile>/config.rs`.

Brace citation check also failed:

```sh
rg -n '`[^`]*\{[^`]*\}[^`]*\.(rs|md|toml):[0-9][^`]*`' restart/audit/totality/p1/1*.md
```

Output:

```text
restart/audit/totality/p1/1B-codegen-evidence.md:71:... `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15`-`17`. |
restart/audit/totality/p1/1B-codegen-evidence.md:114:... `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15`-`17`. |
```

`test -f 'skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs'`
prints `MISSING`; the four individual lowerer files exist and must be cited
separately.

Count reconciliation passed for the V2 CH1 target files:

```sh
awk -F'|' 'NR>=64 && NR<=85 && /\| 1A-SUB-[0-9][0-9][0-9] / {v=$5; gsub(/^ +| +$/,"",v); c[v]++} END {for (v in c) print c[v] "\t" v}' restart/audit/totality/p1/1A-substrate-evidence.md | sort
```

Output:

```text
1	partial / JSON-example implemented
2	partial / UNKNOWN routed
3	unknown
4	impl_exceeds_spec
5	implemented
7	unimplemented
```

This mechanically supports 1A frontmatter `6 / 7 / 4 / 5`: one JSON-scoped
implemented row plus five implemented rows, seven unimplemented rows, four
implementation-exceeds rows, and five unknown/partial-unknown rows.

```sh
awk -F'|' 'NR>=72 && NR<=86 && /\| COH-[0-9][0-9][0-9] / {v=$6; gsub(/^ +| +$/,"",v); c[v]++} END {for (v in c) print c[v] "\t" v}' restart/audit/totality/p1/1F-coherence-scan.md | sort
```

Output:

```text
1	unknown
11	unimplemented
3	implemented
```

This matches 1F frontmatter `3 / 11 / 0 / 1`.

Stale V1 self-description removal passed for the V2 CH1 target files:

```sh
rg -n 'this V1 inventory|Cycle is V1 per user dispatch|cycle: V1' \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md
```

Output: no matches. 1A and 1B both carry `cycle: V3`; 1A records the V3 fold at
`restart/audit/totality/p1/1A-substrate-evidence.md:96-102`, and 1B records
the V3 fold at `restart/audit/totality/p1/1B-codegen-evidence.md:73-78`.

Superseded 1F auxiliary status passed:

```sh
rg -n 'status: superseded-historical-auxiliary|authoritative_live_inventory|Use `restart/audit/totality/p1/1F-coherence-scan.md`|only live 1F inventory|may not be used for current LOC' \
  restart/audit/totality/p1/1F-anti-pattern.md \
  restart/audit/totality/p1/1F-past-corpora.md \
  restart/audit/totality/p1/1F-coherence-scan.md
```

Material output:

```text
restart/audit/totality/p1/1F-anti-pattern.md:6:status: superseded-historical-auxiliary
restart/audit/totality/p1/1F-anti-pattern.md:7:authoritative_live_inventory: restart/audit/totality/p1/1F-coherence-scan.md
restart/audit/totality/p1/1F-anti-pattern.md:14:Use `restart/audit/totality/p1/1F-coherence-scan.md` for current SK-V15 live coherence and anti-pattern evidence.
restart/audit/totality/p1/1F-past-corpora.md:6:status: superseded-historical-auxiliary
restart/audit/totality/p1/1F-past-corpora.md:7:authoritative_live_inventory: restart/audit/totality/p1/1F-coherence-scan.md
restart/audit/totality/p1/1F-coherence-scan.md:90:For SK-V15 T-P1 V2, `1F-coherence-scan.md` is the only live 1F inventory. The two auxiliary files are retained as historical ledgers only:
restart/audit/totality/p1/1F-coherence-scan.md:92:- `restart/audit/totality/p1/1F-anti-pattern.md` is superseded by COH-013/014/015 and may not be used for current LOC or provider/module claims.
```

Spot checks of material citations passed: `skinny/RESULTS.md:139-149`
records JSON 17/17 + 17/17 + 17/17 and CSS 24/24 state; `skinny/REDRESS.md:6254-6284`
resolves W11W parse-only admission; `skinny/crates/ir/src/lib.rs:339-346`
contains the five `BackendShape` variants; `skinny/crates/ir/src/cost.rs:333-340`
returns the same five variants; and `skinny/crates/runtime/src/tape/mod.rs:94-101`
plus `skinny/crates/runtime/src/tape/mod.rs:175-191` resolve the tape and
`ValueRef` borrow shape used by 1A.

## Findings

| id | disposition | severity | finding | evidence | required fold |
|---|---|---|---|---|---|
| CH1-V3-001 | REVISE | high | Root-resolving citation repair is incomplete. V3 still contains six directory-local CSS runtime citations and two brace-path lowerer citations. These are not independently falsifiable from repo root, so CH1 cannot ACCEPT the packet. | `restart/audit/totality/p1/1C-runtime-evidence.md:78-83`; `restart/audit/totality/p1/1B-codegen-evidence.md:71`; `restart/audit/totality/p1/1B-codegen-evidence.md:114`; failing `rg` and `test -f` commands above. | Expand every CSS census citation to `skinny/crates/runtime/src/grammars/<profile>/config.rs:1-9`. Replace brace paths with four individual lowerer citations: `skinny/crates/codegen/src/lower/eager_tape.rs:15-17`, `skinny/crates/codegen/src/lower/offset_tape.rs:15-17`, `skinny/crates/codegen/src/lower/event_tape.rs:15-17`, and `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17`. Re-run both citation greps and require zero output. |
| CH1-V3-002 | ACCEPT | none | 1A frontmatter count reconciliation is now mechanically auditable. | 1A frontmatter `6 / 7 / 4 / 5` at `restart/audit/totality/p1/1A-substrate-evidence.md:21-25`; table-count command above. | None. |
| CH1-V3-003 | ACCEPT | none | 1F frontmatter count reconciliation is now mechanically auditable. | 1F frontmatter `3 / 11 / 0 / 1` at `restart/audit/totality/p1/1F-coherence-scan.md:56-60`; table-count command above. | None. |
| CH1-V3-004 | ACCEPT | none | Stale V1 self-description targeted by V2 CH1 is removed from 1A and 1B. | No matches for the old V2 failure strings; current V3 fold rows at `restart/audit/totality/p1/1A-substrate-evidence.md:96-102` and `restart/audit/totality/p1/1B-codegen-evidence.md:73-78`. | None. |
| CH1-V3-005 | ACCEPT | none | The 1F auxiliary files are substantively demoted to superseded historical status and do not carry live LOC/provider claims. | Auxiliary frontmatter at `restart/audit/totality/p1/1F-anti-pattern.md:6-7` and `restart/audit/totality/p1/1F-past-corpora.md:6-7`; current 1F authority statement at `restart/audit/totality/p1/1F-coherence-scan.md:90-93`. | None. |

## Required Fold

V3 must fold CH1-V3-001 before CH1 can accept:

1. Rewrite the six `1C-runtime-evidence.md` CSS census citations at lines
   78-83 as full repo-root paths under
   `skinny/crates/runtime/src/grammars/<profile>/config.rs:1-9`.
2. Rewrite the two `1B-codegen-evidence.md` brace citations at lines 71 and
   114 as four explicit repo-root citations, one per lowerer file.
3. Re-run the two `rg` commands in this report and record zero output.

No REJECT is warranted. The packet is current SK-V15 V3 evidence, and the
count, stale-V1-wording, and 1F auxiliary demotion folds are accepted. The open
CH1 issue is bounded to citation resolvability.
