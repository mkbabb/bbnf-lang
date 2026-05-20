# SK-V12 S-P1 PIN-V3 CH2 - Generality / Lock 14

Verdict: ACCEPT
Score: 97%

## Scope

Reviewed current head `9559a2c4b480` after the PIN-V2 fold. This CH2 pass owns
only grammar generality and Lock 14: whether S-P1 claims CSS L4, Sheets, or
BBNF-self before generated rows exist; whether root CSS artifacts are being
substituted for a skinny generated parser; whether Lock 14 leaks and
`GrammarConfig` remain routed; whether generated-size/O(N) remains routed; and
whether JSON hot leaves stay profile observations.

Inputs read:

- `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V2/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- P1-A through P1-F current S-P1 artifacts
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- Relevant audits: `skv12-value-api-audit.md`,
  `skv12-W1-A1-css-l4-preflight.md`, `skv12-W1-A4-codegen-runtime-seam.md`,
  and `skv12-W1-A5-bench-oracle-gate.md`.

## Blocking Findings

None.

PIN-V3 preserves the PIN-V2 CH2 result. The reviewed packet does not admit or
profile CSS L4, Sheets, or BBNF-self before generated skinny rows exist; it
does not substitute root CSS/report/lightningcss artifacts; it keeps the seven
Lock 14 leaks and `GrammarConfig` work as W1 prerequisites; it keeps
generated-size/O(N) in the handoff gate; and it keeps JSON hot leaves as
observational profile evidence.

## Evidence

### No early CSS L4, Sheets, or BBNF-self claim

- The user pin makes CSS L4 authoritative and Sheets/BBNF-self fallback-only
  after a CSS L4 redress attempt, while raising the close bar to generated CSS
  L4 `> lightningcss` on the same corpus/output plane with strict equality:
  `USER-PIN-W1-CSS-L4-SOTA.md:18-34`, `:80-103`.
- `HANDOFF.md` records CSS L4 generated parser as `no admitted row` and
  Sheets/BBNF-self as `no admitted row`; it also requires generated CSS L4
  Track 1 `> lightningcss_mbps + 1` and keeps Sheets/BBNF-self fallback-only:
  `HANDOFF.md:30-38`, `:53-65`, `:74-85`.
- The current manifest says the pin root is profile evidence only and moves no
  rows; CSS L4 remains unprofiled because there is no generated CSS L4 Track 1
  runtime, lightningcss same-plane comparator row, or strict equality oracle:
  `skv12-p1-capture-manifest.md:8-29`, `:168-173`.
- P1-F extracts 41 JSON rows only and records generated CSS L4 as 0 admitted
  rows and generated Sheets/BBNF-self as 0 admitted rows. It also states JSON
  rows cannot fill the CSS L4 `lightningcss_mbps + 1` close bar:
  `p1f-results-delta.md:24-33`, `:80-93`, `:202-210`.
- Local replay check over `skv12-p1-pin-replay.tsv` found 458 data rows and 0
  `css`/`lightningcss`/`sheets`/`bbnf` hits in the semantic
  `family/plane/corpus/mode` fields. The lane split remains JSON
  parse/direct/typed profile replay: parse 170, direct 204, typed 84.

### Root CSS artifacts are not substituted

- P1-A states there is no generated CSS L4 runtime, no
  `css_l4/generated.rs`, no `css_l4_declaration_values/generated.rs`, and that
  `nonjson-pass-css-l4.json`, report helpers, root CSS snippets, report
  fixtures, and lightningcss-only runs are not profile or admission authority:
  `p1a-samply-mode-1.md:145-158`.
- P1-C records `/tmp/skv12-pin-p1/*css*` as absent and says the command
  manifest contains no CSS, nonjson, or Sheets command:
  `p1c-samply-mode-3.md:47-62`.
- P1-F records that runtime inventory contains generated JSON plus
  `sheets_witness` only, with no generated `css_l4`,
  `css_l4_declaration_values`, `sheets`, or `bbnf_self` runtime module:
  `p1f-results-delta.md:177-185`.
- The CSS preflight audit independently blocks report fixtures, hand-only CSS,
  JSON-provider clones, and source-only CSS grammar claims without measured
  Mbps and strict equality. It also says root CSS grammar is not a small direct
  import for skinny W1: `skv12-W1-A1-css-l4-preflight.md:28-47`.

### Lock 14 leaks and GrammarConfig remain routed

- The user pin carries Lock 14 grammar-neutrality and requires the seven
  Lock-14 leaks to be resolved by W1's `GrammarConfig` surface before CSS L4
  emission is legal: `USER-PIN-W1-CSS-L4-SOTA.md:97-103`, `:141-150`.
- The value API audit still names the inventory as 5 major plus 2 embedded
  leaks: structural alphabet, value dispatch, string escape/quote policy,
  number policy, key quoting, OffsetFlags semantics, and JsonSink hardcoding:
  `skv12-value-api-audit.md:63-107`.
- The same audit routes the minimal generic surface through
  `GrammarConfig`, per-grammar metadata modules, and parametrized view
  generation; it still blocks non-JSON baselines until codegen can emit them:
  `skv12-value-api-audit.md:160-207`.
- P1-C preserves this as an open blocker: no generated CSS runtime exists,
  codegen still rejects non-JSON runtime emission, JSON template policy remains
  embedded, and `GrammarConfig` is not landed:
  `p1c-samply-mode-3.md:96-110`.
- The codegen/runtime seam audit rejects simply deleting
  `ensure_runtime_profile` and reusing JSON templates for non-JSON grammars:
  `skv12-W1-A4-codegen-runtime-seam.md:6-27`, `:40-44`.

### Generated-size/O(N) guard remains routed

- PIN-V2 consolidation records that the previous fold routed generated CSS
  runtime size, module byte size, regen/check command, and O(N) grammar-size
  guard into `HANDOFF.md`: `PIN-V2/CONSOLIDATED.md:23-35`.
- The current handoff requires S-P3 to record generated CSS runtime size before
  redress, including generated LOC, module byte size, regen/check command, and
  an O(N) grammar-size guard; overflow blocks W1b until traced:
  `HANDOFF.md:119-123`.
- The CSS gate/companion report telemetry list still consumes generated LOC,
  generated module byte size, and O(N) grammar-size status alongside Lock 14,
  Lock 16, JSON guard state, gate status, wave id, and REDRESS id:
  `HANDOFF.md:144-151`.

### JSON hot leaves remain observations

- P1-B splits Track 1 from Track 2/oracle families and says JSON product rows
  are guard/diagnostic evidence, not CSS L4 admission:
  `p1b-samply-mode-2.md:33-50`, `:98-113`, `:187-195`.
- P1-D states PMU values are profile evidence only; they do not move any row,
  admit direct/typed rows, or create the missing CSS L4 row:
  `p1d-pmu-cycles.md:67-80`, `:228-231`, `:258-261`.
- P1-E keeps Track 2/oracle-only families as guard/comparator context, marks
  CSS L4 absent from the hot-leaf ledger, and says CSS L4 deltas remain
  unavailable until W1 creates generated Track 1 CSS, lightningcss comparator,
  equality oracle, and row telemetry:
  `p1e-hot-leaf-attribution.md:19-50`, `:114-116`, `:165-201`.
- The manifest mirrors that split: Track 2/oracle-only families are guard or
  comparator context and are not generated Track 1 optimization antecedents:
  `skv12-p1-capture-manifest.md:189-201`.
- `HARDENING-S-P1-CONVERGED.md` still states the core CH2 boundary: JSON-only
  profile telemetry may nominate primitive families for S-P2, but does not
  prove CSS L4, Sheets, or BBNF-self behavior:
  `HARDENING-S-P1-CONVERGED.md:56-63`.

## Nonblocking Notes

1. `HARDENING-S-P1-CONVERGED.md` still carries the older pre-pin profile
   authority paths (`50bd1648`, `/tmp/skv12-p1`,
   `/tmp/skv12-profile-target-50bd1648`, and `skv12-p1-replay.tsv`):
   `HARDENING-S-P1-CONVERGED.md:23-37`. This is not a CH2 blocker because the
   pin manifest and P1-A through P1-F are the current pin-era evidence, but the
   stale authority block remains easy to misread.
2. `SYNTHESIS.md` carries the CSS/GrammarConfig/admission boundaries, while the
   generated LOC/module byte/O(N) fields are clearest in `HANDOFF.md`. If a
   future S-P3 gate schema is copied from synthesis alone, mirror the handoff
   generated-size fields there to prevent drift.
3. `skv12-W1-A5-bench-oracle-gate.md` includes a Sheets example. Under the user
   pin and current handoff, that example is fallback-only after a measured CSS
   redress attempt; it is not authority to skip CSS.

## Exact Fold Edits Required

None for CH2. No REVISE fold edits are required.
