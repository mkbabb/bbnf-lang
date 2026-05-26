# SK-V14 W5A CH6 V2: Anti-Paper-Close / Next-Tranche Impact

Date: 2026-05-26.
Scope: CH6 review of revised `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` after V1 folds, against V1 CH6, V1 consolidated, SPEC Section 8, and SKINNY-TRIUMVIRATE Sections 4/8/9. Lens: exact test gates, fail-closed grep/count gates, rejected-patch escrow, same-wave consumers, and downstream ADMIT/REJECT routing.
Disposition: ACCEPT.

## §1 Findings

1. ACCEPT - Exact test gates now block the zero-test paper-close shape from V1. V1 CH6 required replacing broad `w5a_` filters with exact tests or nonzero-test assertions (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH6.md:21`), and the V1 consolidated fold records that change as applied (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:23`). The revised plan names five exact grammar/codegen tests and follows each with a log assertion requiring at least one passing test (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:61`-`70`). Those tests map to SPEC Section 8's required parser facts, JSON unchanged-output proof, and Sheets/BBNF-self witnesses (`restart/skinny/tranches/sk-v14/SPEC.md:657`, `restart/skinny/tranches/sk-v14/SPEC.md:672`-`678`), and satisfy the CH6 challenge question for revert/consumer/pre-blocked evidence (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:116`-`125`).

2. ACCEPT - The grep/count gates are now fail-closed rather than observational. V1 CH6 identified the inverted forbidden-call grep and provider/template diff as paper-close risks (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH6.md:13`), and the V1 consolidated fold records fail-closed grep/count and LOC gates as applied (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:24`-`27`). The revised plan now fails if `emit_runtime_profile(target.profile)` remains at the `regen.rs` boundary, asserts the new request symbols, locks provider/template counts to 8 and 7, fails on add/delete/rename provider/template diffs, enforces a <=1000 LOC delta, and blocks non-owner CSS core/grammar edits (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:86`-`104`). This matters because current HEAD still has the forbidden profile-only calls in `regen.rs` (`skinny/xtask/src/regen.rs:14`-`18`, `skinny/xtask/src/regen.rs:30`-`33`) and `emit_runtime_profile` still selects a static runtime profile (`skinny/crates/codegen/src/lib.rs:117`-`120`).

3. ACCEPT - Same-wave consumers are named in the plan and exist as real xtask dispatch surfaces. SPEC requires `cargo xtask regen-css` plus all seven exact `check-css-l4-*` companions to consume the new source-consuming generator contract in W5A's commit (`restart/skinny/tranches/sk-v14/SPEC.md:682`-`684`), and SKINNY-TRIUMVIRATE makes omitted consumer wire-up an automatic REJECT (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177`-`186`). The revised plan runs `check-json`, `regen-css`, all seven CSS companions, and the full-table gate, then states that CSS, JSON, Sheets, and BBNF-self exercise the same request path (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:71`-`80`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:111`). Current xtask dispatch exposes the expected `regen-css` and seven companion commands (`skinny/xtask/src/main.rs:20`-`33`), and `regen_css.rs` carries seven `RuntimeTarget` entries plus companion check functions (`skinny/xtask/src/regen_css.rs:25`-`75`, `skinny/xtask/src/regen_css.rs:77`-`118`).

4. ACCEPT - Rejected-patch escrow and downstream routing are explicit. SKINNY-TRIUMVIRATE requires a failure REDRESS entry with measurement evidence and the reverted patch saved at `/tmp/skv{N}-wave{W}-rejected.patch` (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:73`-`75`), and SK-V14 dispatch repeats the `/tmp/skv14-wave{W}-rejected.patch` requirement (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:65`-`70`). The revised plan now saves `/tmp/skv14-waveW5A-rejected.patch`, reverts the W5A source-consuming contract slice together, retains the provider/template mesh, and writes REDRESS (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:107`-`109`). It also states the downstream route: W5A ADMIT unlocks W5B only, while W5A REJECT blocks W5B, W6, W7, W8, W9, and W10 until reroute (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:113`), matching SPEC's revert/downstream effect (`restart/skinny/tranches/sk-v14/SPEC.md:693`-`699`) and SKINNY role separation for plan versus redress work (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190`-`200`).

## §2 Remaining Required Edits If Any

None for CH6 V2.

## §3 Evidence

Read-only commands executed:

```sh
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md | sed -n '1,150p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH6.md | sed -n '1,120p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md | sed -n '1,90p'
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '637,701p'
nl -ba restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md | sed -n '100,130p'
nl -ba restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md | sed -n '177,210p'
nl -ba restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md | sed -n '55,75p;180,215p'
nl -ba skinny/xtask/src/main.rs | sed -n '1,55p;145,180p'
nl -ba skinny/xtask/src/regen_css.rs | sed -n '1,140p'
nl -ba skinny/xtask/src/regen.rs | sed -n '1,70p'
nl -ba skinny/crates/codegen/src/lib.rs | sed -n '100,135p'
find skinny/crates/codegen/src -name '*_provider.rs' \! -name 'grammar_provider.rs' | wc -l | tr -d ' '
find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '
```

Evidence result: V1 CH6's broad-test, observational-grep, missing-escrow, and missing-route objections are folded in the revised plan. The current code still contains the profile-only boundary that W5A redress must remove, so the revised fail-closed grep gate has a real target. No source, generated output, RESULTS, REDRESS, or existing challenge files were modified by this review.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH6.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/regen_css.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/crates/codegen/src/lib.rs`
