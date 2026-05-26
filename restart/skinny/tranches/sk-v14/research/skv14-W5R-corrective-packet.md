# SK-V14 W5R Corrective Packet: Lock 14 Generator Capability Split

Status: proposed Pass Omega V5 input.
Date: 2026-05-26.

## 1. Finding

W5 PRUNE-3 has a generator-capability gap after W4R close.

The amended W5 wave is required to replace the current per-grammar provider
mesh with one grammar-agnostic generator template consuming grammar source plus
workspace metadata, then delete the CSS provider/template clusters in the same
replacement slice. Current HEAD has only static runtime-profile dispatch.
`regen-css` validates and hashes grammar-source inputs, but codegen receives
only a profile name.

This makes W5 impossible to close without either:

- moving static provider/template bodies into a new file and calling that a
  generic generator; or
- implementing a full source-consuming CSS L4 grammar parser/generator inside
  the W5 cap.

The first route is a workaround. The second route is larger than the W5
replacement/delete gate and needs an explicit wave split.

## 2. Evidence

SPEC clauses:

- `restart/skinny/tranches/sk-v14/SPEC.md:635-647` names W5 owner paths and the
  provider-collapse/delete surface.
- `restart/skinny/tranches/sk-v14/SPEC.md:654-668` requires a generic generator
  template consuming grammar source plus workspace metadata and `regen_css.rs`
  migration.
- `restart/skinny/tranches/sk-v14/SPEC.md:670-680` makes provider count zero,
  CSS template count zero, `regen-css`, companion checks, and Lock 14 baseline
  the exit gate.
- `restart/skinny/tranches/sk-v14/SPEC.md:699-703` correctly blocks W6/W7 on W5
  rejection, but incorrectly leaves W8-W10 independent despite the active
  prune-before-new-admit chain.

Source clauses:

- `skinny/crates/codegen/src/lib.rs:1-10` imports the current provider modules.
- `skinny/crates/codegen/src/lib.rs:117-120` exposes profile-name-only runtime
  emission.
- `skinny/crates/codegen/src/lib.rs:162-210` matches the static
  `RuntimeProvider` variants.
- `skinny/crates/codegen/src/grammar_profile.rs:16-26` defines the provider enum,
  and `grammar_profile.rs:100-110` registers the eight current provider-backed
  profiles.
- `skinny/xtask/src/regen.rs:14-33` calls runtime-profile emission after input
  validation; `regen.rs:61-74` hashes source/metadata but does not pass them to
  codegen.
- `skinny/crates/grammar/src/lib.rs:80-99` accepts only `@import` and `@token`;
  `skinny/crates/grammar/src/lib.rs:196-231` has no `->` or `@{...}` atom.
- `grammar/css/l4/values.bbnf:37` and `grammar/css/l4/values.bbnf:67-69`
  require that missing syntax.

Executable evidence is archived in `skv14-W5-plan.md` and
`skv14-W5-redress.md`.

## 3. Proposed Amendment

Pass Omega V5 should amend the SK-V14 wave graph locally.

### W5 after amendment

W5 becomes **PRUNE-3A: Runtime Generator Contract And CSS Source Parser**.

Required scope:

- Introduce the source-consuming runtime emission request contract in codegen:
  grammar id, target id, grammar-source bundle, workspace metadata digest, output
  roster, and runtime contract.
- Extend the skinny grammar parser or a dedicated runtime-generation parser so
  the CSS L4 source surface required by `grammar/css/l4/*.bbnf` is parseable,
  including `->` value projections and `@{...}` span capture.
- Migrate `regen-css` so source and metadata are codegen inputs, not freshness
  inputs only.
- Keep CSS provider/template deletion out of W5 until the replacement generator
  is load-bearing.
- Preserve `crates/core/src/runtime/css_l4/` as W6.0 work.

Exit gate:

- Source/metadata are passed into codegen and consumed by the runtime generator.
- At least one CSS L4 profile is emitted through the source-consuming path with
  no static provider/template dependency.
- The seven existing CSS provider modules and template directories may remain
  in W5 only as non-consuming legacy inputs with an explicit W5B deletion owner.
- `cargo xtask regen-css` and all seven companions pass through the migrated
  source-consuming path.
- Lock 14 baseline gains a temporary W5A state: no new provider modules or
  template directories may be added.

### New W5B after amendment

W5B becomes **PRUNE-3B: Provider/Template Deletion And Lock 14 Baseline Close**.

Required scope:

- Delete the seven CSS provider modules and seven CSS template directories.
- Delete or retire the old `RuntimeProvider` provider mesh once no profile
  consumes it.
- Update `lock14_baseline.rs` to the post-W5 forward invariant.
- Run `cargo xtask regen-css`, all seven `check-css-l4-*` companions, provider
  count zero, CSS template count zero, and Lock 14 grep gates.

### W6 and later after amendment

W6 remains W6.0 CSS L4 root-runtime collapse followed by W6.1-W6.8 remaining
Pattern H collapses, but its entry gate changes from "W5 admitted" to "W5B
admitted". W7 remains conditional on W6 close. W8/W9/W10 remain globally blocked
until PRUNE-1 through PRUNE-5 close.

## 4. Rejected Alternatives

- **Static centralization**: moving all CSS template bodies into one
  `grammar_provider.rs` deletes names but preserves the hand-written
  per-profile implementation. This repeats the Lock 14 recurrence in a smaller
  namespace.
- **Delete providers now and fix regen later**: repeats REDRESS-184 because the
  same-wave consumer is severed before replacement exists.
- **Fold W6 into W5**: violates W5's no-runtime-edit gate and breaks the W6.0
  W2R root-runtime ownership.
- **Advance W8/W9/W10**: violates prune-before-new-admit and leaves PRUNE-3/4/5
  open.

## 5. Risk Class

Risk: MEDIUM.

Reason: the amendment changes wave ownership, exit-gate text, and the count of
PRUNE-3 sub-waves. It does not change Lock 1, Lock 10, Lock 14, Lock 16,
BackendShape, substrate shape, or any admitted row. It prevents a cosmetic
provider rename from masquerading as a generic generator.

## 6. Pass Omega V5 Routing

Inputs:

- REDRESS-209.
- `restart/skinny/tranches/sk-v14/research/skv14-W5-plan.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5-redress.md`.
- This corrective packet.
- Pass Omega V4 W4R close packet, because W5R builds on the amended W4/W5 split.

Primary Omega outputs:

- Omega-A: verify the amended W5/W5B/W6/W7/W8-W10 wave graph is acyclic.
- Omega-B: carry forward the W2R/W4R/W5R lesson that wave-graph cycles hide at
  delete-target/rebuild-capability boundaries.
- Omega-C: verify no locks amendment is required.
- Omega-D: produce SPEC / MASTER-PLAN amendment diff text.
- Omega-E: align skinny corpus wording for W5A/W5B and provider deletion.
- Omega-F: update HANDOFF / MIGRATION next-dispatch wording.

Until G-Omega V5 closes, do not patch SPEC or dispatch an implementation
replacement for W5. W5 is rejected as currently written.
