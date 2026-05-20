# SK-V12 S-P1 PIN-V2 CH2 - Generality / Lock 14

Verdict: ACCEPT
Score: 96%

## Scope

Reviewed current head `d4ef80b21769` after the PIN-V1 fold, with focus on
grammar generality, Lock 14 integrity, CSS L4 admission discipline, and whether
JSON profile facts remain bounded to profile observation.

Primary inputs:

- `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V1/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- P1-A through P1-F profile artifacts.

## Blocking Findings

None.

The PIN-V1 CH2 finding survives the fold. The current packet does not claim CSS
L4, Sheets, or BBNF-self before generated skinny rows exist; it does not
substitute root CSS/report fixtures for admission; the seven Lock 14 leaks and
`GrammarConfig` prerequisite remain routed; generated-size/O(N) guard routing
appears in the handoff; and JSON hot-leaf claims remain profile observations.

## Evidence

### Non-JSON rows are not claimed early

- The user pin makes CSS L4 the authoritative first target and keeps Sheets plus
  BBNF-self fallback-only after a CSS L4 redress attempt fails:
  `USER-PIN-W1-CSS-L4-SOTA.md:18-24`, `:80-89`.
- `HANDOFF.md` records the live state as CSS L4 generated parser `no admitted
  row`, Sheets/BBNF-self `no admitted row`, and requires generated CSS L4 Track
  1 `> lightningcss_mbps + 1`: `HANDOFF.md:37-38`, `:53-65`, `:74-85`.
- `SYNTHESIS.md` carries the same boundary: CSS L4 has no admitted row,
  Sheets/BBNF-self have no admitted row, and FIXPOINT cannot use them before a
  measured CSS attempt: `SYNTHESIS.md:72-94`, `:101-108`, `:173-178`.
- The manifest makes the pin root explicit and says CSS L4 remains unprofiled
  because there is no generated CSS L4 Track 1 runtime, lightningcss
  same-plane comparator row, or strict equality oracle row:
  `skv12-p1-capture-manifest.md:8-27`, `:152-157`.
- Local replay inspection found `skv12-p1-pin-replay.tsv` has 458 command rows
  and zero `css`/`sheets`/`bbnf`/`lightningcss` hits in the family, plane,
  corpus, or mode fields. Its rows are parse/direct/typed JSON capture/export
  rows, not non-JSON admissions.

### Root CSS artifacts are not substituted

- P1-A explicitly rejects `nonjson-pass-css-l4.json`, report-schema helpers,
  root CSS snippets, report fixtures, and lightningcss-only runs as profile or
  admission authority: `p1a-samply-mode-1.md:145-158`.
- P1-F states the only current CSS artifacts are non-admitting schema/report
  fixtures and historical REDRESS evidence; there is no RESULTS row or
  generated runtime module admitting CSS L4: `p1f-results-delta.md:22-36`,
  `:157-169`, `:177-194`, `:205-210`.
- Runtime inventory still contains generated JSON plus `sheets_witness`; there
  is no generated `css_l4`, `css_l4_declaration_values`, `sheets`, or
  `bbnf_self` runtime module: `p1f-results-delta.md:180-185`.

### Lock 14 leaks remain routed

- The user pin keeps the seven Lock 14 leaks from `skv12-value-api-audit.md`
  open until W1's `GrammarConfig` surface resolves them before CSS L4 emission:
  `USER-PIN-W1-CSS-L4-SOTA.md:97-103`, `:141-150`.
- The value API audit names the inventory as 5 major plus 2 embedded leaks and
  requires grammar-derived config/metadata before non-JSON emission:
  `skv12-value-api-audit.md:63-108`, `:160-207`.
- P1-C preserves the blocker instead of paper-closing it: codegen accepts only
  JSON runtime emission, JSON template policy remains embedded, and
  `GrammarConfig` is not landed: `p1c-samply-mode-3.md:96-110`.
- The current code still enforces that prerequisite: `json_provider` returns OK
  only for `backend.grammar_name == "json"`, and both direct and typed emit
  paths call `ensure_runtime_profile` before runtime emission.

### Generated-size and O(N) routing appears

- PIN-V1 required the fold to route generated CSS runtime size, module byte
  size, regen/check command, and O(N) grammar-size guard into `HANDOFF.md`:
  `PIN-V1/CONSOLIDATED.md:34-35`.
- The current handoff now requires S-P3 to record generated CSS runtime size
  before redress, including generated LOC, module byte size, regen/check
  command, and an O(N) grammar-size guard; overflow blocks W1b until traced:
  `HANDOFF.md:119-123`.
- The telemetry binding also includes generated LOC, generated module byte
  size, and O(N) grammar-size status in the CSS gate/companion report:
  `HANDOFF.md:144-151`.

### JSON hot-leaf claims stay observational

- P1-B splits Track 1 and Track 2/oracle families and says JSON product rows are
  guard/diagnostic evidence, not CSS L4 admission:
  `p1b-samply-mode-2.md:32-49`, `:97-112`, `:186-194`.
- P1-D states PMU values are profile evidence only and do not move rows or
  create the missing CSS L4 row: `p1d-pmu-cycles.md:65-78`, `:226-259`.
- P1-E records hot-leaf tables as pin-root JSON profile antecedents only, keeps
  Track 2/oracle families out of generated Track 1 antecedents, and marks CSS L4
  unavailable until W1 creates generated Track 1, lightningcss comparator,
  equality oracle, and row telemetry: `p1e-hot-leaf-attribution.md:17-50`,
  `:114-178`, `:195-201`.
- `HARDENING-S-P1-CONVERGED.md` keeps the key CH2 boundary: JSON-only profile
  telemetry may nominate primitive families for S-P2, but does not prove CSS L4,
  Sheets, or BBNF-self behavior: `HARDENING-S-P1-CONVERGED.md:58-63`.

## Nonblocking Notes

1. `HARDENING-S-P1-CONVERGED.md` still contains pre-pin profile authority
   paths (`/tmp/skv12-p1`, `skv12-p1-replay.tsv`, and
   `/tmp/skv12-profile-target-50bd1648`). That is provenance drift rather than a
   CH2 blocker because the pin manifest, P1 artifacts, and handoff supersede it,
   but CH1/CH4/CH6 should keep owning that cleanup.
2. `SYNTHESIS.md` carries the CSS/GrammarConfig/admission boundaries, while the
   generated-size/O(N) fields are spelled out in `HANDOFF.md`. If S-P3 treats the
   synthesis telemetry list as a gate schema, mirror the generated LOC/module
   byte/O(N) fields there to avoid future drift.

## Exact Fold Edits Required

None for CH2. No REVISE fold edits are required.
