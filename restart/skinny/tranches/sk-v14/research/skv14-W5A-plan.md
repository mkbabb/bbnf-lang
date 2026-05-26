# SK-V14 Wave W5A Plan: Source-Consuming Runtime Generation Request

Date: 2026-05-26.
Wave: W5A.
Phase: plan.
Disposition: PROCEED TO CHALLENGE.

Inputs:

- `restart/skinny/tranches/sk-v14/SPEC.md:637` binds W5A to the source-consuming runtime generator contract; `SPEC.md:654`-`698` defines entry, tasks, exit, same-wave consumer, pre-blocked routes, revert protocol, and downstream block.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md:84`-`124` proposes the minimal honest request boundary and rejects digest-only source handling.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:23`-`224` inventories the CSS L4 constructs W5A must parse or preserve as runtime-generation facts without grammar-id branches.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md:211`-`225` names the all-seven CSS companion migration gates.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md:60`-`151` defines the JSON byte-equivalence proof and the pre-existing full-codegen-test risk.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:1`-`11` binds Sheets/BBNF-self to the same parser/contract; `skv14-W5A-E-sheets-bbnf-witness.md:77`-`128` defines named fail-closed witness gates.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md:72`-`84` defines the temporary W5A owner-path, no-new-provider/template, source-consuming positive-check, and cap guards.

Intervention: Introduce one grammar-neutral `RuntimeGenerationRequest` path that carries grammar source plus workspace metadata into codegen, parses required V1 runtime-generation constructs into source facts, routes `regen-css` and JSON checks through that request, and leaves CSS provider/template deletion to W5B.

Owner paths:

- `skinny/crates/grammar/src/lib.rs` or a new module under `skinny/crates/grammar/src/` for runtime-generation source parsing and named unsupported construct reporting.
- `skinny/crates/codegen/src/lib.rs` for the public request entrypoint and existing JSON/CSS runtime emission routing.
- `skinny/crates/codegen/src/grammar_provider.rs` as the single source-consuming contract module.
- `skinny/xtask/src/regen.rs` and `skinny/xtask/src/regen_css.rs` for request construction and all-seven CSS consumer wiring.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` for the temporary W5A no-deletion/no-new-provider guard.
- `skinny/RESULTS.md` and `skinny/REDRESS.md` only for final W5A admit/reject attribution.

Explicit non-owner paths:

- `skinny/crates/codegen/src/css_l4_*_provider.rs`.
- `skinny/crates/codegen/src/css_l4_*_templates/`.
- `skinny/crates/runtime/src/grammars/css_l4_*/` except generated output from `cargo xtask regen-css`.
- `crates/core/src/runtime/css_l4/`.
- `grammar/css/l4/`.

Falsifiability gate:

- `regen-css` and every `check-css-l4-*` companion call the new request path and no longer call `codegen::emit_runtime_profile(target.profile)` at the `regen.rs` call boundary.
- The parser/contract accepts the CSS L4 source constructs needed by runtime generation as source facts, not full CSS semantic generation: import graph metadata, `@token`, `@ws`, `@pretty`, comma sequence, `?w`, `>>`, `<<`, `->` projection metadata, typed projection metadata, raw host/value-expression spans, and `@{...}` span capture. These must parse without `grammar_id == "css_l4"` or equivalent profile-specific generic-branch behavior.
- JSON unchanged-output is captured by `cargo xtask check-json`, a before/after whole-directory hash or `git diff --exit-code -- skinny/crates/runtime/src/grammars/json`, and an in-code equality test comparing the new request path to current `emit_from_source("json", source)`.
- Sheets and BBNF-self use the same request path and default to fail-closed witnesses with named source-located unsupported constructs such as `BBNF-UNSUPPORTED-PROJECTION`, `BBNF-UNSUPPORTED-WHITESPACE-MODIFIER`, `BBNF-UNSUPPORTED-IMPORT-RESOLUTION`, `BBNF-UNSUPPORTED-DIRECTIVE`, or `BBNF-UNSUPPORTED-HOST-CAPTURE`. Generated-role witnesses are allowed only if they reuse source-fact parser work already needed for CSS and remain inside the component LOC ledger.
- Provider/template count does not increase, no CSS provider/template path is deleted or renamed in W5A, and `find skinny/crates/codegen/src -name '*_provider.rs' \! -name 'grammar_provider.rs' | wc -l` may remain `8` while `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l` remains `7`.
- Full-table maintain is enforced by `cargo xtask gate-json --check-results --skv14-existing-results-capture`; any refreshed W5A result must preserve every non-target row within +/-1.0% and must not downgrade correctness or audit overlay verdicts.
- W5A source/test LOC remains within the <=1.0k C-1 part-A cap and does not borrow from W5B or W6.

W5A cost ledger:

- Grammar runtime-source parser facts and parser tests: <=300 source/test LOC.
- Codegen request contract, metadata consumption, and JSON equivalence test: <=300 source/test LOC.
- `regen.rs` / `regen_css.rs` request construction and all-seven CSS routing: <=150 source/test LOC.
- Temporary Lock 14 W5A guard: <=120 source/test LOC.
- Sheets/BBNF-self named fail-closed tests: <=100 source/test LOC.
- W5A admit/reject attribution edits counted as hand-written source/test-equivalent budget if they alter gate behavior: <=30 LOC.

Generated runtime output is outside the C-1 source/test LOC count only when produced by `cargo xtask regen-css`, named in the redress log, byte-diff audited, and included in the revert slice. If the estimated implementation cannot fit the ledger or the 90-minute ceiling, return REVISE before source edits rather than borrowing W5B or W6 budget.

Verification commands:

```sh
cd skinny && cargo test -p grammar w5a_css_l4_constructs_parse_as_source_facts -- --exact --nocapture 2>&1 | tee /tmp/skv14-w5a-grammar-css.log
rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5a-grammar-css.log
cd skinny && cargo test -p grammar w5a_named_unsupported_constructs_are_source_located -- --exact --nocapture 2>&1 | tee /tmp/skv14-w5a-grammar-unsupported.log
rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5a-grammar-unsupported.log
cd skinny && cargo test -p codegen w5a_runtime_contract_consumes_source_and_metadata -- --exact --nocapture 2>&1 | tee /tmp/skv14-w5a-codegen-contract.log
rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5a-codegen-contract.log
cd skinny && cargo test -p codegen w5a_json_request_matches_emit_from_source -- --exact --nocapture 2>&1 | tee /tmp/skv14-w5a-codegen-json.log
rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5a-codegen-json.log
cd skinny && cargo test -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract -- --exact --nocapture 2>&1 | tee /tmp/skv14-w5a-codegen-nonjson.log
rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5a-codegen-nonjson.log
cd skinny && cargo xtask check-json
cd skinny && cargo xtask regen-css
cd skinny && cargo xtask check-css-l4-at-rules-and-media
cd skinny && cargo xtask check-css-l4-declaration-values
cd skinny && cargo xtask check-css-l4-declaration-values-extended
cd skinny && cargo xtask check-css-l4-nested-layout
cd skinny && cargo xtask check-css-l4-stylesheet-selectors
cd skinny && cargo xtask check-css-l4-vendor-and-custom-atrules
cd skinny && cargo xtask check-css-l4-visual-functions
cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture
```

Additional grep/count gates:

```sh
if rg -n "emit_runtime_profile\\(target\\.profile\\)" skinny/xtask/src/regen.rs; then exit 1; fi
rg -n "RuntimeGenerationRequest|emit_runtime_from_request" \
  skinny/xtask/src/regen.rs \
  skinny/crates/codegen/src/lib.rs \
  skinny/crates/codegen/src/grammar_provider.rs
test "$(find skinny/crates/codegen/src -name '*_provider.rs' \! -name 'grammar_provider.rs' | wc -l | tr -d ' ')" = "8"
test "$(find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' ')" = "7"
if git diff --name-status -- skinny/crates/codegen/src | rg '^(A|D|R[0-9]*)\\s+.*(_provider\\.rs|_templates)'; then exit 1; fi
W5A_LOC="$(git diff --numstat HEAD -- \
  skinny/crates/grammar/src \
  skinny/crates/codegen/src/lib.rs \
  skinny/crates/codegen/src/grammar_provider.rs \
  skinny/xtask/src/regen.rs \
  skinny/xtask/src/regen_css.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  | awk '$1 != "-" && $2 != "-" { total += $1 + $2 } END { print total + 0 }')"
printf 'W5A source/test LOC delta=%s\n' "$W5A_LOC"
test "$W5A_LOC" -le 1000
git diff --exit-code -- crates/core/src/runtime/css_l4 grammar/css/l4
```

Hard cap: 75 minutes redress wall time with a 90-minute ceiling; commit or reject at cap. Source/test delta must remain <=1.0k C-1 part-A.

Revert protocol: save the rejected patch at `/tmp/skv14-waveW5A-rejected.patch`; revert `grammar_provider.rs`, parser/runtime-generation construct support, `lib.rs` request entrypoint edits, `regen.rs`/`regen_css.rs` routing, and the W5A Lock 14 guard as one slice; retain the existing provider/template mesh; write a new REDRESS entry naming the failed parser construct, source-consumption proof, metadata field, JSON equivalence proof, Sheets/BBNF witness, or Lock 14 guard.

Same-wave consumer: `cargo xtask regen-css` and the seven `check-css-l4-*` companions exercise the source-consuming request in the W5A redress commit. JSON `check-json` and the Sheets/BBNF-self tests exercise the same request path as non-CSS proof consumers.

Downstream route: W5A ADMIT unlocks W5B only. W5A REJECT blocks W5B, W6, W7, W8, W9, and W10 until the PRUNE chain is rerouted through a new plan or Pass Omega amendment.

Pre-blocked routes:

- Static centralization of CSS runtime bodies into a single file.
- Hash-only or provenance-only source handling.
- Grammar-name branches in generic crates.
- Deleting or renaming CSS providers/templates before W5B.
- Editing `crates/core/src/runtime/css_l4/` before W6.
- JSON policy leakage into generic CSS/source routing.
- Reusing `sheets_witness` or SK-V13 witness JSON instead of producing same-contract Sheets/BBNF-self evidence.
- Treating generic parser errors such as `unexpected token '-'` as sufficient fail-closed evidence.

Challenge directive:

Run the mandatory seven-lens W5A CHALLENGE before redress. CH1 must verify measurable gates and source citations. CH2 must verify non-JSON generality and Lock 14. CH3 must verify REDRESS-184/209 are not reopened and apply NEW-CH3-V4-01 deletion/rebuild ordering. CH4 must verify the <=1.0k cap and W5A/W5B/W6 budget separation. CH5 must verify there is no sidecar provider substrate and apply NEW-CH5-V4-01 deletion/consumer coupling. CH6 must verify same-wave consumers, revert protocol, and non-paper-close evidence. CH7 must verify no P-1..P-7 recurrence, fake generated header, fixture lookup, or gate relabeling.
