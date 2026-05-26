# SK-V14 W6.0 Close Packet

Date: 2026-05-26.

Disposition: REJECTED.

## Commits

- Research packet: `4fee87129`
- Plan: `95a52d981`
- Lock 14 route gate: `e3c8c8706`
- Redress: this close packet, `skv14-W6.0-redress.md`,
  `skv14-W6.0R-corrective-packet.md`, `skinny/REDRESS.md`, and the SK-V14
  handoff update.

## Admitted Work

The narrow W6.0 Lock 14 route is admitted. It freezes the root CSS L4 runtime
and exact root xtask files, admits only W6.0 subjects for those paths, rejects
broad W6/W6.1/W5D subjects, and rejects sibling runtime/xtask surfaces.

## Rejected Work

The CSS L4 root-runtime collapse is rejected. There is no root
`cargo xtask regen-css` command, and root `cargo xtask regen --grammar css_l4`
emits only parser/registry outputs. No current root generator has access to the
CSS runtime projection semantics needed to regenerate `CssDocument`,
`CssStructBuilder`, the arena families, typed CSS values, document walkers, or
runtime-view traversal.

## Evidence

- `CARGO_TARGET_DIR=/tmp/bbnf-w6-route-target cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline -- --nocapture`
  passed before the route-gate commit.
- `CARGO_TARGET_DIR=/tmp/bbnf-w6-proof-target cargo xtask regen-css` exited `2`
  with `error: unrecognized subcommand 'regen-css'`.
- `CARGO_TARGET_DIR=/tmp/bbnf-w6-proof-parser-target cargo xtask regen --grammar css_l4 --output /tmp/w6-css-parser-proof`
  exited `0`.
- `find /tmp/w6-css-parser-proof -maxdepth 1 -type f -print` showed only
  `css_l4.rs` and `css_l4.registry.json`.
- Read-only subagent probes independently confirmed that the current registry is
  structural and lacks CSS domain projection semantics.

## Routed Remainder

`restart/skinny/tranches/sk-v14/research/skv14-W6.0R-corrective-packet.md`
defines the next admissible route: add a real runtime projection source and
emitter, then retry the destructive CSS root-runtime gate.

W6.1..W6.8, W7, and all new-admit waves remain blocked until W6.0 admits.
