# AZ-II.cutover.O3a-A1 - Analysis, LSP, json-prototype, and Bootstrap Bridge Disposition
**Opens after**: AZ-II.cutover.O3a baseline capture and six-agent audit synthesis
**Agents**: up to 10 parallel
**Hard gate**: live analysis/LSP failures are repaired or owned, historical prototype surfaces are deleted/archived without shims, and `bootstrap_parser.rs` has an explicit close disposition before tape deletion.
**Status**: planned

## Scope

1. Split live product regressions from historical prototype failures.
2. Decide whether `json-prototype` is deleted, archived, or repaired as
   a live crate; no tape-era compatibility shim is allowed.
3. Decide whether `crates/gorgeous/src/jit.rs` is rewritten onto
   `cargo xtask regen` or deleted as non-product substrate.
4. Add an explicit `bootstrap_parser.rs` disposition: bounded bridge,
   generated self-host replacement gate, or close-blocking repair.
5. Feed archive/delete decisions into O5 and final status into O7.

## Failure Assignment

| Lane | Failed tests |
|---|---|
| Live analysis | `bbnf-analysis::directives import_directive_has_semantic_tokens` |
| Live LSP | `bbnf-lsp::integration test_hover_recover_keyword` |
| Historical json-prototype | `json-prototype::corpus parses_citm`; `json-prototype::corpus parses_data_s`; `json-prototype::corpus parses_data_xl`; `json-prototype::corpus parses_canada`; `json-prototype::corpus parses_twitter`; `json-prototype::corpus tape_visitor_twitter`; `json-prototype::corpus tape_visitor_data_s` |

## File Bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/O3a-A1-research.md` | create |
| `docs/tranches/AZ-II/audit/O3a-A1-plan.md` | create |
| `docs/tranches/AZ-II/waves/cutover/O5.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O6.md` | modify if bootstrap proof is a hard gate |
| `docs/tranches/AZ-II/waves/cutover/O7.md` | modify |
| `crates/analysis/**` | future redress |
| `crates/lsp/**` | future redress |
| `crates/core/src/grammar/bootstrap_parser.rs` | future proof/retirement work |
| `crates/gorgeous/src/jit.rs` | future rewrite/delete |
| `crates/core/benches/json-prototype/**` | future archive/delete |
| workspace manifests | future archive/delete only |

**Do NOT touch**: tape crate deletion directly in A1. O5 owns the
actual crate/workspace severance after A1 supplies the disposition.

## Triumvirate Dispatch

| Lane | Agents | Deliverable |
|---|---:|---|
| Research | 3 | analysis/LSP root cause; json-prototype disposition; bootstrap/JIT disposition |
| Plan + wave creation | 1 | `O3a-A1-plan.md` plus O5/O6/O7 amendments |
| Redress | up to 4 | Repair live surfaces or delete/archive prototype surfaces inside amended owner wave |
| Orchestrator | 1 | Integrate archive decision and close-gate wording |

## Hard Gate

1. `docs/tranches/AZ-II/audit/O3a-A1-research.md` distinguishes live
   product failures from historical prototype failures.
2. `docs/tranches/AZ-II/audit/O3a-A1-plan.md` names delete/archive/repair
   for `json-prototype` and rewrite/delete for the legacy JIT surface.
3. `bootstrap_parser.rs` has an explicit O6/O7 close disposition; it is
   not hidden as ambient legacy code.
4. O5 consumes A1 before deleting `crates/tape`.
5. O7 cites A1 before declaring terminal close.

## Dependencies

- **Depends on**: AZ-II.cutover.O3a
- **Blocks**: AZ-II.cutover.O5 and AZ-II.cutover.O7
