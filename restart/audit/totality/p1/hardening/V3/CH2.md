# T-P1 V3 CH2 Generality

Verdict: ACCEPT.

Score: 4 / 4 CH2 checks pass. The V2 CH2 blockers are folded: `P1-1B-D9` /
`P1-1B-D10` are now grammar-neutral Lock 14 failures with non-JSON proof
receivers, and 1F carries a compact Lock 14 owner/receiver map.

## Evidence

Commands run:

```sh
rg -n "P1-1B-D9|P1-1B-D10|Sheets/BBNF-self|BBNF-self|non-JSON|JSON-only|grammar-neutral Lock 14|owner/receiver|V3 Lock 14" \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1F-coherence-scan.md

nl -ba restart/audit/totality/p1/1B-codegen-evidence.md | sed -n '45,80p;98,120p'
nl -ba restart/audit/totality/p1/1D-skinny-lessons.md | sed -n '136,147p;168,186p'
nl -ba restart/audit/totality/p1/1F-coherence-scan.md | sed -n '120,145p;170,180p'
nl -ba restart/audit/totality/p1/1C-runtime-evidence.md | sed -n '35,70p;85,105p;120,135p'
nl -ba restart/audit/totality/p1/1E-locks-evidence.md | sed -n '96,105p;193,200p'
```

Material grep evidence:

- `1B-codegen-evidence.md:52` ties `P1-1B-D9` and `P1-1B-D10` to JSON-shaped
  recognizer and materialization facts in generic pass logic, and
  `1B-codegen-evidence.md:105`-`106` assigns non-JSON proof: a
  Sheets/BBNF-self/CSS recognizer fixture and non-JSON role facts from generated
  metadata.
- `1B-codegen-evidence.md:78` explicitly preserves the V2 fold requirement:
  D9/D10 remain mandatory 1D grammar-neutral Lock 14 findings with
  Sheets/BBNF-self proof receivers rather than JSON-only routing.
- `1D-skinny-lessons.md:145` now has `G-10` in the Grammar-Neutral Findings
  table: pass-layer recognizer mining and materialization role mining are
  "JSON-shaped generic pass logic, not JSON-only empirical lessons"; Sheets and
  BBNF-self are named proof receivers.
- `1D-skinny-lessons.md:172` confirms the fold: D9/D10 are carried as
  grammar-neutral Lock 14 failures, not JSON-only empirical lessons.
- `1F-coherence-scan.md:129`-`139` adds the V3 Lock 14 owner/receiver map. It
  covers runtime root leaks, codegen profile leaks, pass recognizer mining, pass
  materialization mining, Pattern H root runtime, CSS `CSS_GENERATED_RS`, and
  Lock 14 gate exclusions.
- `1F-coherence-scan.md:135`-`136` route the D9/D10 surfaces to owners `1B / 1D`
  and downstream receivers `REBUILD-WAVE-F` or a primitive shortlist receiver,
  with proof expected from Sheets/BBNF-self/CSS or other non-JSON generated
  metadata.
- `1C-runtime-evidence.md:61` and `1C-runtime-evidence.md:90`-`94` keep runtime
  grammar-name leaks and Pattern H as Lock 14 failures, while
  `1E-locks-evidence.md:102` and `1E-locks-evidence.md:197` preserve the
  broader gate-exclusion receiver discipline.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH2-V3-001 | ACCEPT | `P1-1B-D9` / `P1-1B-D10` are no longer only self-reported or JSON-empirical. They are carried as grammar-neutral Lock 14 failures in 1D. | `1B-codegen-evidence.md:52`, `1B-codegen-evidence.md:78`, `1D-skinny-lessons.md:145`, `1D-skinny-lessons.md:172`. |
| CH2-V3-002 | ACCEPT | The proof receivers are non-JSON and generality-bearing: Sheets/BBNF-self/CSS recognizer fixtures and non-JSON role facts from generated metadata. | `1B-codegen-evidence.md:105`-`106`, `1D-skinny-lessons.md:145`, `1F-coherence-scan.md:135`-`136`. |
| CH2-V3-003 | ACCEPT | The compact Lock 14 owner/receiver map exists and is general enough: it maps pass-layer leaks, runtime-root leaks, profile roster leaks, Pattern H, CSS generated-output contrivance, and gate exclusions to owners, downstream receivers, and proof expectations. | `1F-coherence-scan.md:129`-`139`, `1F-coherence-scan.md:177`. |
| CH2-V3-004 | ACCEPT | No reviewed V3 inventory recasts D9/D10 as a JSON-only lesson. The only "JSON-only" occurrences in the relevant grep assert the opposite: not JSON-only, non-JSON receiver required, or JSON guard scoped away from generality. | `rg -n "P1-1B-D9|P1-1B-D10|JSON-only|non-JSON|BBNF-self" ...` returned only the positive routing lines cited above. |

## Required Fold

None for CH2. Preserve the 1D `G-10` grammar-neutral row and the 1F Lock 14
owner/receiver map in the next cycle; do not demote D9/D10 back into JSON-only
empirical lessons.
