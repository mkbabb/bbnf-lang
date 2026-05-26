# Pass Omega V6 Master Plan / SPEC Diff

Status: proposed patch text.
Date: 2026-05-26.

## MASTER-PLAN §13.3

Replace the current W5B/W6 sequence with:

```text
W5A: closed. Source-consuming runtime request boundary.
W5B-GEN: provider-free runtime generator body. Emits CSS L4 + JSON runtime
bytes from grammar source + workspace metadata through the W5A request contract.
No provider/template deletion. Cap: <=1.0k C-1 part-A source/test LOC.
W5C-DELETE: delete legacy providers/templates and close post-W5 Lock 14.
Cap: <=400 C-1 part-A deletion/baseline LOC.
W6: opens after W5C-DELETE, unchanged in substance and cap.
```

## SK-V14 SPEC

Patch summary:

- Rename current Section 8B to W5B-GEN.
- Add Section 8C for W5C-DELETE.
- Move all provider/template deletion gates from W5B-GEN to W5C-DELETE.
- Move post-W5 Lock 14 zero-provider/zero-template baseline close to W5C-DELETE.
- W5B-GEN exit requires all same-wave consumers to pass while proving no
  provider-backed renderer, per-grammar template, generated-output fixture
  lookup, or grammar-name branch remains in the generation path.
- W5C-DELETE exit requires provider count 0 excluding `grammar_provider.rs`,
  CSS template dir count 0, retired JSON template residue removed or proved
  non-provider/non-template, and Lock 14 baseline passing.
- W6 entry gate becomes W5C-DELETE admitted.

## Cap Amendment

V6 explicitly expands the C-1 part-A envelope because REDRESS-210 proves the
missing provider-free generator body cannot be hidden inside the old W5B <=400
deletion cap:

- W5A closed at 921 augmented source/test LOC.
- W5B-GEN receives <=1.0k C-1 part-A source/test LOC.
- W5C-DELETE receives <=400 C-1 part-A deletion/baseline LOC.
- W6 remains <=2.0k C-1 part-B aggregate, <=90 min per sub-wave.

## Grep Repair

Replace the current `rg -nE ... crates/` command with ripgrep-correct gates
scoped to skinny generic production code. Candidate text:

```sh
cd skinny && ! rg -n '\b(render_runtime_profile|RuntimeProvider|GrammarProfile|json_provider|css_l4_.*provider)\b' crates/codegen/src/{lib.rs,grammar_provider.rs}
cd skinny && ! rg -nU 'match\s+[^{]+\{[^}]*\b(Json|CssL4\w*|Bbnf\w*|GoogleSheets\w*)\b\s*=>' crates/{codegen,runtime,passes,bbnf,grammar}/src --glob '!**/tests/**'
```

The CRUD patch may refine the production/test split, but must not use `rg -E`
as grep-style extended regex.

## Dispatch Prompt

Add a W5BR guard:

```text
Before W5B-GEN, verify Pass Omega V6 G-Omega closed and REDRESS-210 is routed.
Before W5C-DELETE, verify W5B-GEN admitted provider-free generation. Provider
or template deletion is forbidden before W5C-DELETE.
```
