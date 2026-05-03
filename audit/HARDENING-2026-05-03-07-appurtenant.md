# Hardening 07 — Appurtenant Posture

Date: 2026-05-03  
Repo: `/Users/mkbabb/Programming/bbnf-lang`  
Baseline observed: `baf7df2d07cd130a5ad2b8f81fc339418406a3b3`  
Brief baseline claimed: `c5a6fab9` (stale against observed HEAD)

## Sources

- `docs/HARDENING-AUDIT-PROMPT.md` §Appurtenant-Posture.
- `docs/tranches/meta-audit/07-appurtenant-assay.md`.
- `Cargo.toml:1-3`, `.cargo/config.toml:44-59`.
- `docs/GESTALT.md:98-120`.
- `docs/tranches/BC/BC.md:44-56`, `docs/tranches/BC/waves/W5.md:6-14`.
- Siblings inspected: `/Users/mkbabb/Programming/parse-that`, `/Users/mkbabb/Programming/pprint`, `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver`, `/Users/mkbabb/Programming/bbnf-buddy`, `/Users/mkbabb/Programming/ffuzzy`.

## Cross-Repo Snapshot

| Repo / ring member | Observed state | Audit-subject dependency | Verdict |
|---|---|---|---|
| bbnf-lang | workspace members include `crates/csp-solver`, `crates/bbnf-path`, `crates/bbnf-path-ts`, and exclude `wasm` (`Cargo.toml:1-3`) | BA owns Rust direct projection; BB owns rewrites; BC owns cleanup and cross-repo refresh | READY with baseline caveat |
| parse-that | exists at `051a6d6`; dirty/untracked docs, precepts, `.cargo`, rustc ICE artefact | `.cargo/config.toml:51-52` path-patches `parse_that` and `bbnf-regex` to `../parse-that/rust/parse_that` and `../parse-that/rust/regex` | PRE-CYCLE TOUCH |
| pprint | exists at `2b4d2d4`; untracked `pprint` and `rust/.cargo` | path-patched by `.cargo/config.toml:49-50`; consumed by gorgeous | READY, with dirty-state warning |
| csp-solver sibling | exists at `b700986`; `git status` from the csp-solver path reports many dirty sibling web `node_modules` deletions because the repo root is higher | BC.W5 requires bbnf-lang in-tree vs csc411 sibling diff-clean (`docs/tranches/BC/waves/W5.md:11`, `:92`) | PRE-CYCLE TOUCH |
| wasm/ | tracked workspace-excluded directory (`Cargo.toml:3`) | BA/BC punt TS/WASM to BD (`docs/tranches/BA/BA.md:189-199`, `docs/tranches/BC/BC.md:29`) | OUT-OF-SCOPE-FOREVER for this cycle |
| gorgeous sibling | missing; `docs/GESTALT.md:104` says sibling gorgeous retired and workspace `crates/gorgeous` is canonical | no sibling touch required | OUT-OF-SCOPE-FOREVER |
| bbnf-buddy | exists at `53badf0`; dirty Vue/package files | not referenced by BA/BB/BC mechanisms | OUT-OF-SCOPE-FOREVER |
| precepts submodule | tracked gitlink at `93a24ea` | BC.W3 may update `docs/precepts/instructions/PROFILING.md` as submodule (`docs/tranches/BC/waves/W3.md:35`, `:70-71`) | POST-CYCLE TOUCH |
| ffuzzy | exists at `98f85f8`; dirty and unrelated | no BA/BB/BC dependency found | OUT-OF-SCOPE-FOREVER |

## Findings

### A1. `bbnf-regex` relocation target disagrees with the current path-patch

BC.W5 says relocation target is `parse-that/rust/bbnf-regex/` and that bbnf-lang will consume it by path-dep (`docs/tranches/BC/waves/W5.md:6`, `:12`, `:66-76`). Current bbnf-lang consumes `bbnf-regex` from `../parse-that/rust/regex` (`.cargo/config.toml:52`), and the sibling actually contains `/Users/mkbabb/Programming/parse-that/rust/regex`, including `src/path_lexer.rs`; `/Users/mkbabb/Programming/parse-that/rust/bbnf-regex` does not exist.

Mechanism-level fix: amend BC.W5 before dispatch to choose one canonical endpoint. If the desired name is `bbnf-regex`, make W5.2 explicitly rename `parse-that/rust/regex` to `parse-that/rust/bbnf-regex` and update parse-that workspace membership plus `.cargo/config.toml`. If the desired endpoint is the existing crate path, change every W5 reference from `parse-that/rust/bbnf-regex/` to `parse-that/rust/regex/`.

Paste-ready amendment:

```md
### BC.W5 preflight amendment — bbnf-regex endpoint

Before W5 dispatch, run:

`test -d /Users/mkbabb/Programming/parse-that/rust/regex`
`test ! -d /Users/mkbabb/Programming/parse-that/rust/bbnf-regex`
`rg -n "bbnf-regex\\s*=.*parse-that/rust/regex|parse-that/rust/bbnf-regex" .cargo/config.toml docs/tranches/BC docs/GESTALT.md`

W5 must then choose and document one endpoint. Either:

1. Rename `parse-that/rust/regex` to `parse-that/rust/bbnf-regex`, update parse-that workspace membership, and update bbnf-lang's path patch; or
2. Preserve the existing endpoint `parse-that/rust/regex` and amend BC.W5 / GESTALT references accordingly.

No W5 close may cite relocation complete while bbnf-lang path-patches one endpoint and the sibling filesystem provides another.
```

### A2. Sibling dirty states will make BC.W5 evidence ambiguous

The csc411 sibling exists, but `git status --short` from the csp-solver directory surfaces many dirty/deleted files outside the csp-solver crate. parse-that and pprint also have untracked local state. BC.W5's diff-clean and cross-repo bench parity claims (`docs/tranches/BC/BC.md:116-118`) need a pre-cycle sibling-status capture, otherwise W5 will not distinguish pre-existing sibling dirt from BA/BB/BC output.

Mechanism-level fix: BC.W0 should write a sibling baseline artefact before any BC.W5 cross-repo edit. This is audit-only; the future execution pass can decide whether to isolate sibling worktrees or clean unrelated dirt.

Paste-ready amendment:

```md
### BC.W0 sibling-baseline artefact

Add to BC.W0 verification artefacts:

- `docs/tranches/BC/audit/W0-sibling-baseline.txt` — captures `git rev-parse --short HEAD` and `git status --short` for parse-that, pprint, csc411/csp-solver, bbnf-buddy, and ffuzzy before BC edits. BC.W5 uses this baseline to separate pre-existing sibling dirt from W5 output.
```

### A3. parse-that is already the combinator and regex owner; BA direct projection should not cross-pollute it

`docs/GESTALT.md:102` names parse-that as the combinator and regex owner, while BA's file bounds are entirely bbnf-lang Rust emitter/runtime paths (`docs/tranches/BA/BA.md:100-115`). No BA wave needs parse-that edits. This is correct and should remain enforced: direct-projection codegen consumes `StructRegistry` in bbnf-lang; it must not alter parse-that's combinator API to make BA pass.

Mechanism-level fix: add a BA.W0 or BA.W2 guard line: parse-that is read-only for BA unless a triumvirate proves a path-lexer API defect blocks `path!` diagnostics.

## Cross-Repo Synthesis

The appurtenant ring is broadly sequenced correctly: BA stays in bbnf-lang, BB consumes workspace IR/egraph substrate, and BC owns cross-repo cleanup. The one concrete pre-cycle blocker is naming and path consistency for `bbnf-regex`; the one process blocker is dirty sibling state before BC.W5. Neither blocks BA.W0 directly, but both should be paste-amended before BC reaches W5.
