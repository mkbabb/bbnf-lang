# SK-V14 W5A CHALLENGE V1 CH5 - Hidden Coupling

Date: 2026-05-26.

Scope: CH5 Hidden Coupling review of `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` against SPEC Section 8, DISPATCH §4.3 including NEW-CH5-V4-01, REDRESS-184/209, and the W5A research packet. Lens: no parallel substrate, no sidecar provider, no Track 1/Track 2 dishonesty, no deletion/consumer decoupling, and no static-provider dependency at the W5A call boundary.

Disposition: ACCEPT.

## §1 Findings

1. ACCEPT - the plan introduces one source-consuming request path, not a parallel substrate or sidecar provider substrate. The selected intervention is a single grammar-neutral `RuntimeGenerationRequest` that carries grammar source plus workspace metadata into codegen (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`). The owner paths are the existing grammar/codegen/xtask/Lock14 surfaces plus a `grammar_provider.rs` successor contract module (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:20`-`27`), while existing CSS provider/template paths and root runtime paths are explicit non-owner paths (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29`-`35`). This matches SPEC's W5A owner/task envelope for a source-consuming parser/contract and temporary guard (`restart/skinny/tranches/sk-v14/SPEC.md:641`-`668`) and does not relax SPEC's global ban on new substrate surfaces, parser-owned facts/sidecars, parallel substrates, or sidecar substrates (`restart/skinny/tranches/sk-v14/SPEC.md:205`-`213`).

2. ACCEPT - the static-provider dependency at the W5A call boundary is explicitly blocked. The current HEAD risk is real: `RuntimeTarget` carries source and metadata fields, but `write_targets` and `check_target` still call `codegen::emit_runtime_profile(target.profile)` (`skinny/xtask/src/regen.rs:5`-`18`, `skinny/xtask/src/regen.rs:30`-`32`), and that route selects static `RuntimeProvider` variants (`skinny/crates/codegen/src/lib.rs:117`-`120`, `skinny/crates/codegen/src/lib.rs:162`-`210`). REDRESS-209 rejected that exact shape because source/metadata were freshness-only and CSS still emitted through the static provider/template mesh (`skinny/REDRESS.md:5173`-`5183`). The W5A plan's falsifiability gate requires `regen-css` and every CSS companion to call the new request path and no longer call `emit_runtime_profile(target.profile)` at the `regen.rs` call boundary (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`43`), matching SPEC's no-static-provider/template call-boundary requirement (`restart/skinny/tranches/sk-v14/SPEC.md:662`-`675`).

3. ACCEPT - NEW-CH5-V4-01 deletion/consumer coupling is preserved. DISPATCH §4.3 requires CH5 to treat provider/template/runtime deletion as coupled to the code path compiling the same-wave consumer (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:158`-`180`). REDRESS-184 proves why: deleting CSS provider modules before replacement made `cargo xtask regen-css` fail with missing provider modules (`skinny/REDRESS.md:5105`-`5118`). The W5A plan keeps provider/template deletion out of scope, requires counts to remain at 8 providers and 7 CSS template dirs, and makes `regen-css` plus all seven CSS companions the same-wave consumers of the new source-consuming path (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:75`-`88`). W5B, not W5A, owns deletion after W5A admits (`restart/skinny/tranches/sk-v14/SPEC.md:701`-`724`).

4. ACCEPT - Track 1/Track 2 dishonesty is not introduced. W5A is a generator-contract and verification wave, not a row-admission wave: its gates are source consumption, parser construct coverage, JSON unchanged output, Sheets/BBNF-self same-contract proof, provider/template no-deletion counts, and `gate-json --check-results` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`44`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:46`-`71`). The plan contains no Track 1/Track 2 measurement or relabeling path, and SPEC's separate telemetry gate still requires `track2_entry_point` and `track2_independence_status` for any row claiming Track 1 + Track 2 admission (`restart/skinny/tranches/sk-v14/SPEC.md:134`-`158`).

5. ACCEPT - REDRESS-184 and REDRESS-209 remain closed rather than reopened. The plan's pre-blocked routes forbid static centralization, hash-only/provenance-only handling, grammar-name branches, provider/template deletion before W5B, root runtime edits before W6, JSON policy leakage, stale witness reuse, and generic parser errors as sufficient evidence (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:79`-`88`). That is aligned with REDRESS-184's deletion-before-consumer failure (`skinny/REDRESS.md:5105`-`5118`) and REDRESS-209's static-provider/freshness-only rejection plus W5A/W5B supersession (`skinny/REDRESS.md:5173`-`5193`).

## §2 Required Plan Edits

None.

## §3 Executable/read-only evidence

Commands run read-only at HEAD `1dd390065`:

```sh
rg -n "Track 1|Track 2|track1|track2" restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md
# no output

find skinny/crates/codegen/src -maxdepth 1 -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '
# 8
find skinny/crates/codegen/src -maxdepth 1 -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '
# 7

rg -n "emit_runtime_profile\\(target\\.profile\\)" skinny/xtask/src/regen.rs
# 18:        let emitted = codegen::emit_runtime_profile(target.profile)?;
# 32:    let emitted = codegen::emit_runtime_profile(target.profile)?;

git diff --name-status -- skinny/crates/codegen/src | rg '(_provider\.rs|_templates)' || true
# no output
```

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md`
- `skinny/REDRESS.md`
- `skinny/xtask/src/regen.rs`
- `skinny/crates/codegen/src/lib.rs`
