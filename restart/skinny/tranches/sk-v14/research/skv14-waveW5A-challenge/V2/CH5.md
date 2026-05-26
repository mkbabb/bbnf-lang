# SK-V14 W5A CH5 V2: Hidden Coupling

Date: 2026-05-26.
Scope: CH5 Hidden Coupling review of the revised `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` after V1 folds, against V1 CH5, V1 consolidated packet, DISPATCH NEW-CH5-V4-01, SPEC Section 8, and REDRESS-184/209. Lens: no sidecar provider substrate, no deletion/consumer decoupling, no profile-only call-boundary escape, and no Track 1/Track 2 dishonesty.
Disposition: ACCEPT.

## §1 — Findings

1. ACCEPT - the folded plan still introduces one source-consuming contract, not a sidecar provider substrate. The intervention is one grammar-neutral `RuntimeGenerationRequest` carrying grammar source plus workspace metadata into codegen (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`). Owner paths are limited to grammar/codegen/xtask/Lock14 surfaces plus `grammar_provider.rs` as the single source-consuming contract module (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:20`-`27`), while existing CSS provider/template paths and root runtime paths remain explicit non-owners (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29`-`35`). That matches SPEC Section 8's one grammar-neutral W5A generator-contract module (`restart/skinny/tranches/sk-v14/SPEC.md:641`-`645`) without relaxing the global ban on new substrate surfaces, parser-owned sidecars/facts, or parallel/sidecar substrate (`restart/skinny/tranches/sk-v14/SPEC.md:205`-`213`). The folded fail-closed provider/template count and A/D/R diff gates also block a new or renamed provider substrate (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:91`-`93`).

2. ACCEPT - provider/template deletion remains coupled to the same-wave consumer instead of being decoupled. DISPATCH NEW-CH5-V4-01 requires CH5 to treat provider/template/runtime deletion as coupled to the code path compiling the same-wave consumer (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:175`-`179`), and the V5 lesson states the wave is REJECT if the consumer still imports, includes, or profile-dispatches through the deletion target (`restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:49`-`52`). REDRESS-184 is the proof case: provider deletion before replacement left `cargo xtask regen-css` compiling through deleted provider modules and failed before regeneration could run (`skinny/REDRESS.md:5105`-`5118`). The folded plan keeps CSS provider/template deletion out of W5A, requires the old counts to remain `8` and `7`, fails A/D/R provider/template diffs, and pre-blocks deleting or renaming CSS providers/templates before W5B (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:91`-`93`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:120`). The same-wave consumers are `cargo xtask regen-css`, all seven `check-css-l4-*` companions, `check-json`, and Sheets/BBNF-self tests through the same request path (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:111`), while W5B owns provider/template deletion only after W5A admits (`restart/skinny/tranches/sk-v14/SPEC.md:715`-`724`, `restart/skinny/tranches/sk-v14/SPEC.md:740`-`750`).

3. ACCEPT - the folded plan has no profile-only call-boundary escape. Current HEAD still has the REDRESS-209 shape: `RuntimeTarget` carries source and metadata fields, but `write_targets` and `check_target` call `codegen::emit_runtime_profile(target.profile)` (`skinny/xtask/src/regen.rs:5`-`18`, `skinny/xtask/src/regen.rs:30`-`32`), and `emit_runtime_profile` selects a static profile/provider route (`skinny/crates/codegen/src/lib.rs:117`-`120`, `skinny/crates/codegen/src/lib.rs:162`-`210`). REDRESS-209 rejected that exact profile-only/static-provider/freshness-only shape (`skinny/REDRESS.md:5173`-`5183`) and superseded it into W5A source-consuming capability followed by W5B deletion (`skinny/REDRESS.md:5189`-`5193`). The folded plan requires `regen-css` and every CSS companion to call the new request path and no longer call `emit_runtime_profile(target.profile)` at the `regen.rs` boundary (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39`), makes that forbidden-call check fail closed (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:86`), requires positive `RuntimeGenerationRequest|emit_runtime_from_request` evidence in `regen.rs`, `lib.rs`, and `grammar_provider.rs` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:87`-`90`), and includes source/metadata contract tests (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:65`-`70`). This satisfies SPEC's W5A requirement that source and metadata not remain freshness-only inputs and that all seven CSS profiles have no static provider/template dependency at the call boundary (`restart/skinny/tranches/sk-v14/SPEC.md:662`-`675`).

4. ACCEPT - the folded plan does not introduce Track 1/Track 2 dishonesty. W5A remains a generator-contract and verification wave: source-consuming parser/contract tests, JSON unchanged-output proof, CSS companion checks, provider/template no-deletion checks, LOC cap, and full-table maintain gate (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`45`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:58`-`80`). It contains no Track 1/Track 2 measurement, relabeling, or shared-entrypoint admission path. The separate SK-V14 telemetry gate still requires `track2_entry_point` and `track2_independence_status` for any row claiming Track 1 + Track 2 admission (`restart/skinny/tranches/sk-v14/SPEC.md:134`-`158`), and SPEC's P-7 guard keeps Track 1 generated structurally distinct from Track 2 oracle work (`restart/skinny/tranches/sk-v14/SPEC.md:1152`-`1153`).

5. ACCEPT - the V1 fold strengthened CH5 rather than weakening it. V1 CH5 already accepted the no-sidecar-provider, no-static-boundary, no-deletion-decoupling, and no-Track1/Track2 conclusions (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH5.md:11`-`19`). The V1 consolidated packet kept CH5 ACCEPT and folded other-lens fixes that make the forbidden profile call, provider/template counts, and provider/template A/D/R diffs fail closed (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:11`-`16`, `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:23`-`32`). Those folds are present in the revised plan (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:86`-`93`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:109`-`113`).

## §2 — Remaining Required Edits

None for CH5 V2.

## §3 — Evidence

Read-only commands:

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

git diff --name-status -- skinny/crates/codegen/src
# no output

rg -n "RuntimeGenerationRequest|emit_runtime_profile\\(target\\.profile\\)|Provider/template count|Same-wave consumer|Deleting or renaming CSS providers/templates|CH5" restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md
```

The current code still shows the REDRESS-209 profile-only risk, and the folded plan now makes that exact boundary fail closed while preserving the required W5A no-deletion state.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH5.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/audit/totality/astral/V5/ΩB-skinny-lessons.md`
- `skinny/REDRESS.md`
- `skinny/xtask/src/regen.rs`
- `skinny/crates/codegen/src/lib.rs`
