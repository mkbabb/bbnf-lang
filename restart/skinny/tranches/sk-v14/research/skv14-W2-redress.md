# SK-V14 Wave W2 Redress: Regen-CSS Round-Trip Rejected

Status: `REJECTED`.

Gate: `G-W2-FULL-ROUNDTRIP` from `restart/skinny/tranches/sk-v14/SPEC.md:489-495`.

Rejected patch: `/tmp/skv14-waveW2-rejected-regen-css.patch`.

## Finding

W2 cannot honestly close at current HEAD. The skinny-side `regen-css`
surface can be built as a rostered command for the seven existing CSS L4
runtime profiles, but the required destructive round-trip also deletes
`crates/core/src/runtime/css_l4/`. No current generator restores that tree.

The contradiction is structural, not a missing edit in the attempted patch:

- SPEC W2 requires `regen-css` to emit both
  `skinny/crates/runtime/src/grammars/css_l4_*` and
  `crates/core/src/runtime/css_l4/`, then pass the destructive dual-tree
  diff gate.
- `restart/ARCHITECTURE.md` records live HEAD as having zero generated
  runtime files and `crates/core/src/runtime/css_l4/` as 7 hand-written
  Pattern H files.
- `restart/MIGRATION.md` routes the 67 hand-written per-grammar runtime
  files, including CSS L4, to SK-V14 W6 PRUNE-4 after the W5 generic
  provider refactor.
- Root `cargo xtask regen --grammar css_l4` regenerates
  `crates/core/src/grammar/generated/css_l4.{rs,registry.json}`, not
  `crates/core/src/runtime/css_l4/`.

## Evidence

The reverted W2 source attempt added the narrow skinny commands only:

```sh
cd skinny && cargo xtask regen-css
cd skinny && cargo xtask check-css-l4-declaration-values
cd skinny && cargo xtask check-css-l4-declaration-values-extended
cd skinny && cargo xtask check-css-l4-stylesheet-selectors
cd skinny && cargo xtask check-css-l4-visual-functions
cd skinny && cargo xtask check-css-l4-at-rules-and-media
cd skinny && cargo xtask check-css-l4-vendor-and-custom-atrules
cd skinny && cargo xtask check-css-l4-nested-layout
cd skinny && cargo test -p codegen css_l4_ -- --nocapture
```

Those checks passed, but they only prove the skinny rostered emission loop.
They do not satisfy W2 because the full gate is dual-tree.

Root grammar generation is reproducible and separate:

```sh
rm -rf /tmp/skv14-w2-root-css
cargo xtask regen --grammar css_l4 --output /tmp/skv14-w2-root-css
diff -q crates/core/src/grammar/generated/css_l4.rs /tmp/skv14-w2-root-css/css_l4.rs
diff -q crates/core/src/grammar/generated/css_l4.registry.json /tmp/skv14-w2-root-css/css_l4.registry.json
git diff --exit-code -- crates/core/src/grammar/generated/css_l4.rs crates/core/src/grammar/generated/css_l4.registry.json
```

All root grammar diffs were clean. That confirms the available root
generator targets grammar parser output, not core runtime output.

The destructive W2 gate failed:

```sh
rm -rf skinny/crates/runtime/src/grammars/css_l4_* crates/core/src/runtime/css_l4/
(cd skinny && cargo xtask regen-css)
git diff --exit-code --name-status -- skinny/crates/runtime/src/grammars crates/core/src/runtime/css_l4
```

Failure:

```text
error: couldn't read `crates/runtime/src/grammars/css_l4_declaration_values/mod.rs`: No such file or directory (os error 2)
 --> crates/runtime/src/lib.rs:7:1
```

The command failed before it could regenerate because the skinny runtime
crate imports the deleted checked-in modules during xtask compilation. Even
if that compile-order issue were repaired, the attempted W2 patch had no
authority or mechanism to emit `crates/core/src/runtime/css_l4/`; generating
that tree is the Pattern H collapse routed to W6.

The post-revert JSON gate remained clean:

```sh
cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture
```

It exited 0 and retained the SK-V14-open `NO-GO`/audit-falsified baseline.

## Disposition

W2 is rejected rather than partially admitted. A skinny-only `regen-css`
would make later W4/W5/W6 dispatches rest on a false generated-runtime
claim. Copying `crates/core/src/runtime/css_l4/` into templates, replaying
git content, or adding generated headers to hand-written code would repeat
the P-1 fake-generated recurrence.

Per `restart/skinny/tranches/sk-v14/SPEC.md:508-510`, the source attempt was
reverted and the rejected patch was retained under `/tmp`. Because W3 and
W4 hard-entry gates require W2 admitted, and W5/W6/W7 depend on W4/W5/W6 in
sequence, no later SK-V14 implementation wave can legally dispatch from
this rejected W2 state.
