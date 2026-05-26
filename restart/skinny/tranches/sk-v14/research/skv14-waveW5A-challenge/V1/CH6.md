# SK-V14 W5A CH6 Anti-Paper-Close / Next-Tranche Impact

Date: 2026-05-26.

Scope: CH6 review of `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` against SPEC Section 8, DISPATCH Section 4.3 CH6 language, SKINNY-TRIUMVIRATE Sections 4/8/9, and W5A research artifacts A-F. Lens: same-wave consumer reality, revert sufficiency, present-tense executable proof, and downstream routing for W5A admission/rejection.

Disposition: REVISE.

## §1 Findings

1. REVISE - The same-wave consumers are real commands, but the plan's test proof can still paper-close. `cargo xtask regen-css` and all seven CSS companion commands are present in the xtask dispatcher (`skinny/xtask/src/main.rs:20`-`33`) and map to seven `RuntimeTarget` entries plus companion check functions (`skinny/xtask/src/regen_css.rs:25`-`75`, `skinny/xtask/src/regen_css.rs:77`-`118`). SPEC Section 8 makes those exact commands the W5A production consumers (`restart/skinny/tranches/sk-v14/SPEC.md:682`-`684`), and the plan names them as same-wave consumers (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:77`). However, the verification commands use broad filters, `cargo test -p grammar w5a_` and `cargo test -p codegen w5a_` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:49`-`50`). Rust test filters can exit successfully with zero matching tests, so CH6 needs exact test names or a nonzero-test-count assertion. W5A-E already proposed exact gate names for this purpose (`restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:119`-`125`).

2. REVISE - The call-boundary proof is stated, but the grep/count gates are not fail-closed as written. The plan correctly requires `regen-css` and all companions to stop calling `codegen::emit_runtime_profile(target.profile)` at the `regen.rs` boundary (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39`), and current HEAD proves why that matters: both write and check still call `emit_runtime_profile(target.profile)` (`skinny/xtask/src/regen.rs:14`-`18`, `skinny/xtask/src/regen.rs:30`-`33`), which selects profile-only static rendering (`skinny/crates/codegen/src/lib.rs:117`-`120`). But the additional gate only runs `rg -n "emit_runtime_profile\\(target\\.profile\\)" skinny/xtask/src/regen.rs` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:63`-`67`), which returns success when the forbidden call is present. The provider/template diff command is also observational because it ends in `|| true` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:69`). These must become explicit fail-closed checks, otherwise W5A can close on displayed evidence instead of enforced evidence.

3. REVISE - The revert protocol is directionally correct but omits the required rejected-patch escrow and downstream disposition. The plan says to revert `grammar_provider.rs`, parser support, request entrypoint edits, regen routing, and the W5A Lock 14 guard as one slice, retain the provider/template mesh, and write REDRESS (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:73`-`75`). That matches SPEC's slice-level revert language (`restart/skinny/tranches/sk-v14/SPEC.md:693`-`696`), but SKINNY-TRIUMVIRATE requires failure commits to include measurement evidence plus the reverted patch saved at `/tmp/skv{N}-wave{W}-rejected.patch` (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:73`-`75`), and DISPATCH repeats the SK-V14 failure path (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:67`-`70`). The plan also does not explicitly state the downstream route: W5A rejection blocks W5B, W6, W7, and all new-admit waves (`restart/skinny/tranches/sk-v14/SPEC.md:698`-`699`); W5A admission unlocks W5B only, with W6 still blocked until W5B and W8/W9/W10 blocked until PRUNE-1..PRUNE-5 close (`restart/audit/totality/astral/V5/G-OMEGA-SIGNOFF.md:61`-`64`).

4. ACCEPT - The plan avoids the main CH6 tense failure, but only if the edits above make the proof executable. SPEC forbids closing on "wired", "advisory", "future consumer", "integrated", or "paper close" language without measured evidence (`restart/skinny/tranches/sk-v14/SPEC.md:219`-`220`) and flags past-perfect claims on not-present path:line as paper-close (`restart/skinny/tranches/sk-v14/SPEC.md:226`-`227`). The plan frames the intervention prospectively as "Introduce" (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`) and names concrete commands (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:46`-`61`), so the issue is not false past-tense. The issue is that two proof commands and two grep gates need fail-closed semantics before they can satisfy present-tense executable proof.

## §2 Required Plan Edits

1. Replace `cargo test -p grammar w5a_` and `cargo test -p codegen w5a_` with exact named tests, or add a harness assertion that at least one W5A test ran in each crate. Minimum names should cover source+metadata materiality, named unsupported constructs, and Sheets/BBNF-self same-contract proof, using the W5A-E gate names as the seed.

2. Make call-boundary and provider/template gates fail closed. The `emit_runtime_profile(target.profile)` check should fail if any hit remains on the W5A `regen-css` / companion boundary; provider/template diff should fail on unplanned add/delete/rename and must not be hidden behind `|| true`.

3. Amend the revert protocol to save the rejected patch at `/tmp/skv14-waveW5A-rejected.patch`, then record REDRESS with the failed construct/path/proof. Add an explicit downstream clause: ADMIT unlocks W5B only; REJECT blocks W5B, W6, W7, and W8/W9/W10 until the PRUNE chain is rerouted.

## §3 Executable/Read-Only Evidence

Read-only commands executed:

```sh
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '637,735p'
nl -ba restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md | sed -n '1,230p'
nl -ba restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md | sed -n '34,215p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-{A,B,C,D,E,F}-*.md
nl -ba skinny/xtask/src/main.rs | sed -n '1,45p;145,175p'
nl -ba skinny/xtask/src/regen_css.rs | sed -n '1,150p'
nl -ba skinny/xtask/src/regen.rs | sed -n '1,90p'
nl -ba skinny/crates/codegen/src/lib.rs | sed -n '90,230p'
nl -ba skinny/REDRESS.md | sed -n '5168,5195p'
```

Evidence result: the W5A consumer commands exist; current HEAD still routes CSS runtime emission through profile-only `emit_runtime_profile`; REDRESS-209 remains the controlling rejected shape; SPEC/G-Omega route W5A admission to W5B and W5A rejection to a PRUNE-chain block. No source, generated, RESULTS, REDRESS, or existing challenge files were modified by this review.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md`
- `restart/audit/totality/astral/V5/G-OMEGA-SIGNOFF.md`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/regen_css.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/REDRESS.md`
