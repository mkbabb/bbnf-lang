# SK-V14 W4R Corrective Packet: PRUNE-2 Provider-Deletion Cycle

Status: proposed Pass Omega V4 input.
Date: 2026-05-26.

## 1. Finding

W4 PRUNE-2 has a wave-graph cycle after W2R and W3 close.

The amended W2 `regen-css` command is skinny-side only, but it still delegates
runtime emission to `codegen::emit_runtime_profile`. That call path compiles
through the seven CSS L4 provider modules and their template directories. W4
requires those provider modules and template directories to be deleted before
running `cargo xtask regen-css`. The provider replacement is W5's
grammar-agnostic generator collapse, but W5 requires W4 admission.

This makes W4 impossible to close without preserving the deleted providers,
breaking the exit gate, or smuggling W5 work into W4.

## 2. Evidence

SPEC clauses:

- `restart/skinny/tranches/sk-v14/SPEC.md:572-574` names the seven CSS
  template dirs, seven provider modules, and seven runtime twins as W4 owner
  deletion paths.
- `restart/skinny/tranches/sk-v14/SPEC.md:593-596` orders deletion before
  `cargo xtask regen-css`.
- `restart/skinny/tranches/sk-v14/SPEC.md:603-605` makes a clean
  `regen-css` rerun the W4 exit gate.
- `restart/skinny/tranches/sk-v14/SPEC.md:633-658` assigns provider collapse
  and `regen_css.rs` migration to W5.
- `restart/skinny/tranches/sk-v14/SPEC.md:646-648` makes W5 conditional on
  W4 admission.

Source clauses:

- `skinny/xtask/src/regen.rs:18` calls
  `codegen::emit_runtime_profile(target.profile)`.
- `skinny/crates/codegen/src/lib.rs:1-7` imports the seven CSS provider
  modules.
- `skinny/crates/codegen/src/lib.rs:166-208` dispatches CSS runtime profiles
  to those modules.
- `skinny/crates/codegen/src/grammar_profile.rs:100-110` registers CSS
  runtime profiles by calling those providers.

Executable probe:

```sh
git worktree add --detach /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe HEAD
rm -rf /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny/crates/codegen/src/css_l4_*_templates \
  /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny/crates/codegen/src/css_l4_*_provider.rs \
  /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny/crates/runtime/src/grammars/css_l4_*
(cd /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny && cargo xtask regen-css)
git worktree remove --force /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe
```

Observed failure: `error[E0583]` for all seven `css_l4_*_provider` modules.

## 3. Proposed Amendment

Pass Omega V4 should amend the SK-V14 wave graph locally.

### W4 after amendment

W4 becomes the CSS L4 admit-ledger PRUNE wave:

- Keep the W2 and W3 entry gates.
- Do not delete codegen provider modules or template directories in W4.
- Revert `restart/skinny/ROLLING-SOTA-DELTA.md` so the 24 CSS L4 rows show
  `AUDIT-FALSIFIED` / `OPEN` / `0/24` instead of `ADMITTED`.
- Add 24 row-keyed `skinny/REDRESS.md` entries citing
  `restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md §1-6`.
- Keep `skinny/RESULTS.md` as already audit-falsified unless the gate needs a
  narrow status-text correction.
- Preserve the runtime/template/provider deletion as a hard downstream gate,
  not a W4 action.

### W5 after amendment

W5 absorbs the skinny CSS provider/template deletion into its existing
Lock-14 provider-collapse scope:

- Stand up the grammar-agnostic provider path.
- Migrate `regen_css.rs` to the new provider path.
- Delete the seven CSS L4 provider modules and seven template directories in
  the same commit as their replacement.
- Run `cargo xtask regen-css` and all seven `check-css-l4-*` companions after
  deletion.
- Update the Lock 14 baseline intentionally for removed provider/header paths.

### W6 after amendment

No change from V3 W2R: W6.0 remains CSS L4 root-runtime collapse, W6.1-W6.8
remain the other Pattern H dirs.

## 4. Rejected Alternatives

- **Force W4 deletion now.** This breaks `cargo xtask regen-css` before the
  same-wave consumer can run.
- **Keep providers while claiming W4 deleted them.** This violates the W4
  exit gate and repeats fake cleanup.
- **Move all W5 work into W4.** This is a global resequence of the Lock 14
  refactor and hides a high-risk architectural wave inside a medium-low
  deletion wave.

## 5. Risk Class

Risk: MEDIUM-LOW.

Reason: the amendment changes wave ownership and exit-gate text. It does not
change Lock 1, Lock 10, Lock 14, Lock 16, BackendShape, or substrate shape.
It reduces implementation risk by moving provider deletion to the wave that
owns the replacement generator.

## 6. Pass Omega V4 Routing

Inputs:

- REDRESS-184.
- `restart/skinny/tranches/sk-v14/research/skv14-W4-redress.md`.
- This corrective packet.
- V3 W2R close packet, because W4R builds on the amended W2/W6 split.

Primary Omega outputs:

- Omega-A: verify the amended W4/W5/W6 wave graph is acyclic.
- Omega-C: verify no locks amendment is required.
- Omega-D: produce SPEC / MASTER-PLAN amendment diff text.
- Omega-E: align skinny corpus wording for W4 ledger-only and W5 provider
  deletion.
- Omega-F: update HANDOFF / MIGRATION next-dispatch wording.

Until G-Omega V4 closes, do not patch SPEC or dispatch W5. W4 is rejected as
currently written.
