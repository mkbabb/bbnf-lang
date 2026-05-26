# SK-V14 W5 Redress: Lock 14 Generator Capability Gap

Date: 2026-05-26.
Wave: W5.
Phase: redress.
Status: REJECTED.

## Finding

W5 cannot honestly close under the current SPEC wording. The wave requires
collapsing the eight per-grammar provider modules into one grammar-agnostic
generator template that consumes grammar source plus workspace metadata, then
deleting the seven CSS provider modules and seven CSS template directories in
the same replacement slice.

The current implementation has no such generator path. The live `regen-css`
command can reproduce the checked-in skinny CSS runtime twins, but it still
emits through the static provider/template mesh that W5 must remove.

The blocker is intrinsic:

- W5's replacement must be source-consuming, not a renamed static table.
- `regen-css` currently hashes CSS source and metadata as freshness evidence
  only; it does not pass those bytes into codegen.
- The skinny grammar parser cannot parse the CSS L4 source surface that the
  proposed generator would have to consume.
- W6 owns root runtime Pattern H collapse, so W5 cannot smuggle runtime collapse
  into the provider replacement.

## Evidence

Current W5 gates remain pre-W5:

```sh
find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar' | wc -l
# 5

find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l
# 8

find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l
# 7

find grammar/css/l4 -type f -name '*.bbnf' | wc -l
# 15
```

`cargo xtask regen-css` exits 0, but the output path is still
`skinny/xtask/src/regen.rs:18-33 -> codegen::emit_runtime_profile(target.profile)`,
which reaches the provider modules imported at
`skinny/crates/codegen/src/lib.rs:1-10` and matched at
`skinny/crates/codegen/src/lib.rs:162-210`.

The parser gap is executable:

```sh
cd skinny && cargo test -p grammar rejects_non_skinny_directives -- --nocapture
# 1 passed
```

A temporary parse probe against `grammar/css/l4/values.bbnf` returned:

```text
ERR BBNF-PARSE: unexpected token `-` at byte 1362
```

The failure aligns with `grammar/css/l4/values.bbnf:37` (`->` value projection)
and `grammar/css/l4/values.bbnf:67-69` (`@{...}` span capture), neither of
which exists in the current skinny parser's atom/directive surface.

## REDRESS

Record REDRESS-209 in `skinny/REDRESS.md`:

- Gate: `G-SK-V14-W5-PRUNE-3`.
- Decision: `REJECTED`.
- Root cause: W5 requires a grammar-source-consuming generic generator, but the
  current codegen path is static provider dispatch and the current skinny
  grammar parser cannot parse CSS L4 source.
- Corrective route: Pass Omega V5 W5R.

## Blocked State

W6 remains blocked because it requires the W5 generic generator template as the
collapse target. W7 remains blocked because PRUNE-5 wires policy/union consumers
after PRUNE-3 and PRUNE-4. W8/W9/W10 remain globally blocked until PRUNE-1
through PRUNE-5 close.

No CSS row is admitted or newly measured by W5. No provider, template, runtime,
or generated output path is deleted by this redress.
