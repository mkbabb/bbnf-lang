# SK-V12 S-P1 PIN-V1 CH2 - Generality / Lock 14

Verdict: ACCEPT
Score: 94%

## Scope

Reviewed the pinned S-P1 profile fold committed as `b1043383`:

- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`

Reviewed against `restart/prompts/ORCHESTRATOR.md` Section 3W/3Z,
`restart/prompts/skinny/PASS-1-PROFILE.md`, and
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.

## Blocking Findings

None.

The pinned S-P1 fold satisfies CH2 for this rerun. It records JSON profile
evidence as profile evidence only, keeps CSS L4/Sheets/BBNF-self unadmitted
until generated skinny rows exist, and routes Lock 14/GrammarConfig work forward
instead of paper-closing it in S-P1.

## Evidence

### CSS L4 and Sheets are not claimed before generated skinny rows exist

- The user pin makes CSS L4 authoritative and Sheets/BBNF-self fallback-only
  after a CSS L4 redress attempt fails:
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-24`,
  `:80-89`.
- The capture manifest records the pin root as JSON-only for parse/direct/typed
  capture and explicitly says CSS L4 remains unprofiled because no generated CSS
  L4 Track 1 runtime or lightningcss same-plane comparator row exists:
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:22-37`.
- P1-A records CSS L4 parse artifacts as `0/0`, states CSS L4 is unprofiled
  until W1 creates generated Track 1, and refuses report fixtures or
  lightningcss-only runs as substitute admission:
  `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:16-21`,
  `:146-159`.
- P1-C is the strongest boundary statement: no CSS/non-JSON/sheets commands are
  present under `/tmp/skv12-pin-p1`, no generated CSS runtime exists, and W1 must
  first create the generated CSS parser plus strict lightningcss comparator
  before CSS hot-leaf, Mode III, or SOTA claims become measurable:
  `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:59-62`,
  `:96-110`.
- P1-F confirms current `skinny/RESULTS.md` has zero generated CSS L4 rows and
  zero generated Sheets/BBNF-self rows, and that JSON sonic/serde rows cannot
  fill the CSS L4 `lightningcss_mbps + 1` close bar:
  `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:22-36`,
  `:80-93`, `:171-193`.

### Grammar-neutral claims remain profile observations

- `PASS-1-PROFILE.md` requires hot-leaf attribution to name primitives so S-P2
  can ask whether they generalize to CSS L4, Sheets, and BBNF-self:
  `restart/prompts/skinny/PASS-1-PROFILE.md:129-135`.
- P1-B limits JSON product PMU rows to guard/diagnostic evidence and states they
  cannot substitute for CSS L4 generated baseline or lightningcss admission:
  `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:42-43`,
  `:153-161`, `:203-205`.
- P1-D treats JSON PMU data only as nomination evidence. It does not propose a
  route from outlier rows and says CSS L4 still requires its own measured row
  before admission:
  `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:76-78`,
  `:241-259`.
- P1-E keeps raw symbol evidence before family labels and states comparator or
  oracle symbols never prove generated Track 1 hot leaves:
  `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:136-149`.

### Root-workspace CSS artifacts are not substituted for skinny CSS admission

- P1-A rejects `nonjson-pass-css-l4.json`, report-schema helpers, root-workspace
  CSS snippets, report fixtures, and lightningcss-only runs as admission or
  profile authority:
  `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:154-159`.
- P1-F characterizes current CSS L4 artifacts as non-admitting schema/report
  fixtures and historical REDRESS evidence, not `skinny/RESULTS.md` rows or
  generated runtime modules:
  `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:26-29`,
  `:157-169`.
- Filesystem evidence matches the docs: `skinny/crates/runtime/src/grammars/`
  contains `json` and `sheets_witness`, with no generated `css_l4` or
  `css_l4_declaration_values` module.

### Lock 14 leaks and GrammarConfig remain routed forward

- The user pin carries the requirement that the seven Lock 14 leaks from
  `skv12-value-api-audit.md` must be resolved by W1's `GrammarConfig` surface
  before CSS L4 emission is legal:
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97-103`,
  `:141-150`.
- The audit names the leak inventory as 5 major plus 2 embedded Lock 14 leaks
  and requires W1/W2 resolution, including `GrammarConfig` and per-grammar
  metadata:
  `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-108`,
  `:160-207`.
- P1-C preserves that blocker rather than closing it: codegen still rejects
  non-JSON runtime emission, JSON template policy remains embedded, and
  `GrammarConfig` is not landed:
  `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:100-104`.
- Source confirms the prerequisite remains real: `json_provider` accepts only
  `backend.grammar_name == "json"` and both runtime emit paths call
  `ensure_runtime_profile`:
  `skinny/crates/codegen/src/json_provider.rs:4-12`,
  `skinny/crates/codegen/src/lib.rs:102-146`.

## Nonblocking Notes

1. P1-A, P1-B, and P1-E retain superseded partial-capture blocker sections below
   their final fold blocks. The final fold language is clear enough for CH2
   because it explicitly supersedes the stale sections, but future readers could
   confuse old "unavailable" text with the final JSON hot-leaf capture state:
   `p1a-samply-mode-1.md:23-27`, `:137-144`;
   `p1b-samply-mode-2.md:18-21`, `:163-190`;
   `p1e-hot-leaf-attribution.md:17-20`, `:75-118`, `:165-193`.
2. The manifest still carries the pre-pin `/tmp/skv12-p1` run identity after the
   pin addendum. This is not a CH2 blocker because the addendum is explicit and
   the six pinned docs cite `/tmp/skv12-pin-p1`, but a later cleanup should mark
   the older manifest sections as historical pre-pin context:
   `skv12-p1-capture-manifest.md:9-37`, `:39-47`.

## Exact Fold Edits Required

None for CH2. Optional cleanup only: label retained partial-capture sections as
historical/superseded or move them under an appendix so they cannot be mistaken
for the final pin-root authority.
