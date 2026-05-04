# PASS-2 Agent 4: Runtime Template Architect

## §1 Scope + Framing

Lens: specify the grammar-agnostic runtime template that emits per-grammar modules. PASS-2 prompt names the output target as `runtime/src/grammars/<name>/{generated.rs, parser.rs, host.rs}` and says it consumes grammar source plus workspace metadata at xtask regen time (`restart/prompts/PASS-2-CODEGEN.md:35`). README names `runtime` as the runtime substrate plus template-emitted grammar subdirs (`restart/README.md:47`).

The current runtime is the opposite shape. `crates/core/src/runtime/mod.rs` manually declares nine grammar modules and re-exports many grammar-specific symbols (`crates/core/src/runtime/mod.rs:8-23`, `crates/core/src/runtime/mod.rs:25-72`). CENSUS calls the nine runtime directories out as duplicated per-grammar inventory and gives the simple cohort duplication count (`restart/corpora/CENSUS.md:435-527`). Lock 13 calls a 16-sibling runtime directory mixing grammar subdirs and mechanisms a god directory (`restart/locks/14-LOCKS.md:58`). Lock 14 forbids hand-written per-grammar runtime files and requires template emission from grammar source plus metadata (`restart/locks/14-LOCKS.md:60`).

## §2 Per-Item Table

| Template Element | Pro | Con | Explication | Challenge | Disposition |
|---|---|---|---|---|---|
| `runtime/src/tape/` | Lock 1 explicitly places tape there (`restart/locks/14-LOCKS.md:34`). | Current runtime uses builder stacks and grammar modules. | New runtime substrate owns `Tape`, `TapeNode`, `PayloadArena`, checkpoints, and visitors. | No OpenFrame compatibility layer. | KEEP-REINVENT. |
| `runtime/src/grammars/<name>/generated.rs` | Prompt names template-emitted per-grammar modules (`restart/prompts/PASS-2-CODEGEN.md:35`). | Generated code can become too large; Lock 13 caps non-generated files at 500 LOC (`restart/locks/14-LOCKS.md:58`). | Holds generated parser tables, rule functions, and typed view impls. | Must be generated and budgeted. | KEEP-MODIFY. |
| `parser.rs` | Stable parse signatures are needed by PASS-3. | If hand-written, it violates Lock 14. | Template-emitted wrapper around generated parse core. | Header marks generator and metadata hash. | KEEP-MODIFY. |
| `host.rs` | Prompt names it; host table is per grammar as data. | Hand-authored host code is not allowed for the 9 grammars (`restart/README.md:13-25`). | Template-emitted resolver glue from metadata and `@host fn`; calls `host` generic primitives. | Rare external host escape must be explicit. | REINVENT. |
| `kind.rs` / `value.rs` / `document.rs` | Typed views need named surfaces. | Hand-written per-grammar files are old failure. | Generated inside `generated.rs` or split as generated sibling files under one template. | Split only if generated line count demands it. | KEEP-MODIFY. |
| Builder template | Current template already captures simple cohort concepts (`crates/core/src/runtime/builder_template.rs:1-14`). | It retains OpenFrame-like frames and clone checkpoints (`crates/core/src/runtime/builder_template.rs:92-103`, `crates/core/src/runtime/builder_template.rs:203-210`). | Convert to TapeBuilder. | No compatibility shim. | REINVENT. |
| Arena template | Current arena template is generic and small (`crates/core/src/runtime/arena_template.rs:1-10`, `crates/core/src/runtime/arena_template.rs:80-134`). | It currently exists beside grammar-specific runtime dirs. | Keep the slab idea inside `runtime/src/tape/payload.rs`. | Payload arena must borrow input slices. | KEEP-MODIFY. |

## §3 Architectural Commitments Ratified

1. **Two onboarding surfaces.** A grammar enters the fleet through `<name>.bbnf` plus `[workspace.metadata.bbnf.grammars.<name>]` and nothing else. README states this as the anthem and rejects Rust crate or per-grammar match arm additions (`restart/README.md:13-25`).

2. **Generated subdirs are artefacts, not hand-maintained modules.** Amendment 01 keeps template-emitted subdirectories and rejects per-grammar declaration crates (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-32`). Current README updates internal crate naming and keeps zero per-grammar crates (`restart/README.md:31-58`).

3. **Tape and typed values are one runtime surface.** The template emits views borrowing `&'i Tape<'i>` and node ids. This follows Lock 1 and avoids the old direct-only/pure-builder split (`restart/locks/14-LOCKS.md:34`).

4. **Generated output is committed and byte-identical.** Lock 6 requires xtask-generated committed source artefacts (`restart/locks/14-LOCKS.md:44`). The BB cohort template spec already gives hash, xtask regen, and byte-equality precedent (`docs/tranches/BB/audit/W2-cohort-template-spec.md:40-61`).

5. **No runtime god directory.** Mechanism modules live under `runtime/src/{tape,error,visitor,layout,owned,grammars}`. Per-grammar output lives only under `runtime/src/grammars/<name>/` and every subdir has the same generated shape.

## §4 New Facilities Proposed

Template parameter table:

| Parameter | Source | Consumer |
|---|---|---|
| `grammar_ident` | workspace metadata | file names, parse names, diagnostics |
| `kind_enum` | Backend IR `TapeKind` table | `TapeNode.kind`, visitor, debug |
| `value_enum` | Backend IR `ValueShape` table | typed document/views |
| `document_struct` | metadata naming policy | PASS-3 API wrappers |
| `view_structs` | rule result shapes | field accessors |
| `parse_fn_signatures` | PASS-3 contract plus BIR input mode | `parser.rs` |
| `leaf_kinds` | BIR scanner/literal/regex nodes | leaf payloads |
| `host_fn_table` | metadata plus `@host fn` | `host.rs` glue |
| `simd_alphabet` | PASS-1 scan analysis | generated scanner constants |
| `layout_policy` | `@layout` analysis | layout skipping edges |
| `error_policy` | `@error` analysis | diagnostics and recovery nodes |
| `pratt_tables` | PASS-1 Pratt detection | generated Pratt loop data |
| `budget` | xtask registry | generated LOC gate |

`proc_macro2 + quote` remains a good generation mechanism because the BB template spec already used that sketch (`docs/tranches/BB/audit/W2-cohort-template-spec.md:75-162`). PASS-2 should move it into `codegen/src/runtime_template/` and make it consume Backend IR plus metadata instead of current runtime source.

Output tree:

```text
runtime/src/
  tape/
    mod.rs
    node.rs
    payload.rs
    checkpoint.rs
  visitor/
    mod.rs
  error/
    mod.rs
  layout/
    mod.rs
  grammars/
    json/
      mod.rs
      generated.rs
      parser.rs
      host.rs
```

Every file under `runtime/src/grammars/<name>/` is generated. Generic runtime modules carry no grammar names.

## §5 Cross-Cuts To PASS-1 / PASS-3

PASS-1 provides: type layouts, generic substitutions, host signatures, error policies, layout policies, Pratt and SIMD plans, and field names. `@layout`, `@error`, generics, host fn, and chaining are all V1 README extensions (`restart/README.md:145-178`).

PASS-3 consumes: parse function names, document/view types, visitor trait, selectors, and owned escape hatches. README names parse signatures with slice-borrow primary and owned escape hatches in the locks table, though line 391 still says "ParseStream"; PASS-2 resolves that word to Tape (`restart/README.md:391`, `restart/locks/14-LOCKS.md:34`).

## §6 Risk + Mitigation Table

| Risk | Impact | Mitigation |
|---|---|---|
| `host.rs` becomes hand-edited per grammar | Lock 14 break | Generated header plus `cargo xtask regen --check` compares bytes and fails edits. |
| Runtime `mod.rs` re-exports grammar-specific names | God module returns | Aggregator belongs to PASS-3/user-facing crate; runtime keeps generic registry. |
| Generated files hide unbounded growth | Compile time and review burden | Per-grammar LOC budget, with generated exempt from file cap but not from budget (`restart/locks/14-LOCKS.md:118-125`). |
| Template parameters come from ad hoc metadata | Future grammar onboarding breaks | Metadata schema validation before generation; no missing optional behavior. |
| Tape payload arena loses borrow discipline | Owned allocations become default | `parse(&'i str)` emits borrowed payloads; owned parse is explicit escape. |

## §7 Inheritance Ledger

| Source | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| BB cohort template spec | Parameterization and hash/regen checks (`docs/tranches/BB/audit/W2-cohort-template-spec.md:8-22`, `docs/tranches/BB/audit/W2-cohort-template-spec.md:40-61`). | Expand from cohort to all 9 grammars and Tape-backed views. | `cohort=true` metadata as a special mode. |
| Current arena template | Compact slab arena concept (`crates/core/src/runtime/arena_template.rs:80-134`). | Payload arena under Tape. | Grammar opt-out comments and outlier split. |
| Current runtime tree | Names of existing grammars for migration inventory (`crates/core/src/runtime/mod.rs:8-23`). | Generated per-grammar subdirs. | Manual module list and re-export fanout. |
| Amendment 01 | Zero per-grammar crates and metadata-driven onboarding (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-24`). | Re-anchor to current crate names. | MASTER-PLAN per-grammar crate skeletons. |

## Wave 2 correction note

This agent's runtime template emission (agent-4 §1, line 23 cited by HARDENING-PASS-2 punch items 3 and 4; agent-4 §2/§3, line 82 cited by punch item 7) is augmented by PASS-2's per-grammar runtime emission table (PASS-2.md §6) whose columns are `generated.rs`, `parser.rs`, `host.rs`, host source, layout source, error source, Pratt/SIMD source. Hand-written runtime files are forbidden; every cell is template-emitted or data-only, with the prohibition enforced by `rg -n "// hand-written" crates/runtime/src/grammars/` returning zero. The yaml smoke row reaches the table at the same column shape; this agent's template parameter schema feeds every column.
