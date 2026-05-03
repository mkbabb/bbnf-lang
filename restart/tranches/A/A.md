# Tranche A — Workspace Genesis

## Gestalt

Tranche A is the precondition. Hereupon the workspace as inherited from the prior tranche set sheds its accumulated residue and reshapes around the 14 locks. The Lock 12 archive ceremony fires first — `crates/ser/` and `crates/gorgeous/` retire to `archive/`, freeing the workspace `members` table from substrates whose contracts the restart no longer honours. The commit-chain disposition executes per Pass C §7.5 (Option 3: keep verbatim + branch reset): the `pre-restart-2026-05-03` tag preserves provenance; the `master-greenfield-2026-05-03` branch carries the prelude. The empty crate skeletons land — bbnf-error, bbnf-pipeline, bbnf-grammar, bbnf-parse, bbnf-ir, bbnf-passes, bbnf-vm, bbnf-codegen-ir, bbnf-codegen, bbnf-runtime-template, bbnf-runtime, bbnf-host, path-core, path, path-ts, bbnf, bbnf-test-fixtures, bbnf-bench, bbnf-language-server, plus the 9 per-grammar declaration crates — each with `Cargo.toml`, an empty `src/lib.rs` carrying the module skeleton, and the workspace path-dep registration. The Lock 14 retirement (the seven sites identified by Pass A §7 W1 + Pass B §7.1.3) lands here because they block the IR fracture in tranche C. The narrative scrub eliminates ~50 tape-residue sites across the prior corpus. Tranche A closes when `cargo check --workspace` passes green on the new crate set with skeletal contents and the eight prelude commits sit on `master-greenfield-2026-05-03`.

This tranche is the operational shape of the restart's first breath: not yet building the substrate proper, but clearing the ground so the substrate may rise.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| Lock 12 archive ceremony complete | A.W0 | `find archive/{ser,gorgeous}/Cargo.toml` exists; `find crates/{ser,gorgeous}/Cargo.toml` returns nothing; `rg 'crates/ser\|crates/gorgeous\|bbnf-ser\|bbnf-gorgeous' crates/ -l` returns 0 |
| Commit-chain disposition executed | A.W0 | `git tag --list pre-restart-2026-05-03` returns the tag; `git log origin/master..HEAD --oneline \| wc -l` returns 0 (master pushed); `git branch --show-current` returns `master-greenfield-2026-05-03` |
| Sister-crate path-deps registered | A.W2 | `cargo tree -p bbnf-passes` shows `parse-that = path = "crates/parse-that"`; same for `bbnf-regex` and `csp-solver`; submodules pinned |
| Lock 14 retirement (7 sites) complete | A.W3 | `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|Bbnf\w*\s*=>\|GoogleSheets\w*\s*=>' crates/{bbnf-parse,bbnf-ir,bbnf-passes,path-core,path,path-ts,bbnf-codegen-ir,bbnf-codegen,bbnf-runtime,bbnf-runtime-template,bbnf-host,bbnf-error,bbnf-pipeline,bbnf-vm,bbnf-grammar,bbnf-test-fixtures,bbnf-bench,bbnf-language-server}/src/` returns 0 |
| Tape narrative residue scrubbed | A.W4 | `rg -nE 'TapeRec\|TapeCursor\|payload_idx\|OpenFrame\|FusedBuilder\|tape-?first\|columnar' crates/ docs/` returns 0 outside `archive/` and `docs/precepts/` |
| Workspace skeleton compiles | A.W5 | `cargo check --workspace` exits 0 with no warnings (excluding `unused_imports` for skeletal lib.rs files) |
| README + GESTALT rewrite | A.W6 | `rg 'rust/\|typescript/\|prettier-plugin-bbnf/\|AY-\|BA\.W\|BB\.W\|BC\.W' README.md docs/GESTALT.md` returns 0 |
| `crates/{analysis, lsp}` consolidate to `crates/bbnf-language-server/` | A.W1 | `find crates/analysis crates/lsp` returns nothing; `find crates/bbnf-language-server/src/{analysis,lsp}` returns directories; `cargo check -p bbnf-language-server` passes |
| `data/` relocates to `crates/bbnf-test-fixtures/data/` | A.W5 | `find data/` returns nothing; `find crates/bbnf-test-fixtures/data/` returns the fixture tree |
| Build artefacts cleaned + .gitignore updated | A.W5 | `git status` shows clean working tree; `rg '^server/$\|^wasm/pkg' .gitignore` matches |
| Legacy tranche tree relocates to `docs/tranches/archive/legacy-Y-BD/` | A.W3 | `find docs/tranches -maxdepth 1 -mindepth 1 -type d` lists only `archive`, `meta-audit`, `A`, `B`, ..., `J` |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| A.W0 — Lock 12 ceremony + commit-chain branch reset | The Lock 12 archive ceremony fires; tag + branch reset; pre-restart provenance preserved | 1 (orchestrator-driven; mechanical) | archive/ exists; tag exists; new branch checked out |
| A.W1 — analysis + lsp consolidation; sister-crate submodule registration | Merge `crates/{analysis, lsp}` into `crates/bbnf-language-server/`; register parse-that + bbnf-regex as submodules + workspace-members | 2 parallel | bbnf-language-server compiles; submodules pinned |
| A.W2 — Empty crate skeletons land | All 23 substrate crates appear with Cargo.toml + src/lib.rs (module skeleton); `[workspace.metadata.bbnf]` populated per §6.1 of master plan | 4 parallel (skeleton groups) | `cargo check --workspace` passes on skeletal crates; metadata validate gate runs |
| A.W3 — Lock 14 retirement (7 sites) + docs/ tree relocate | Seven Lock 14 violation sites retire (per Pass A §7 W1 + Pass B §7.1.3); docs tree relocates per Pass C §3.3 + §8 of master plan; legacy tranche tree archives | 3 parallel | Lock 14 verification gate passes; docs structure matches §8.1 of master plan |
| A.W4 — Narrative scrub + crates/core/ partial dismantle | ~50 tape narrative sites scrub during regen; partial movement of `crates/core/src/path/` content to `crates/path-core/` skeleton | 2 parallel | tape grep returns 0; path-core skeleton compiles |
| A.W5 — `data/` relocation + build-artefact cleanup + workspace check-green | `data/` → `crates/bbnf-test-fixtures/data/`; `server/`, `*.vsix`, `wasm/pkg*/` deleted; `.gitignore` augmented | 1 | clean working tree; `cargo check --workspace` green |
| A.W6 — README + GESTALT rewrite + close ceremony | README rewrites per restart vocabulary; GESTALT reflects post-restart shape; tranche close docs land | 1 | metalanguage scan passes; FINAL.md cites every gate's evidence |

## Carry-tags FROM

(none — A is the precondition tranche)

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| Skeletal `bbnf-error/` ready for substantive impl | B | B.W0 |
| Skeletal `bbnf-pipeline/` ready for substantive impl | B | B.W2 |
| Skeletal `bbnf-grammar/`, `bbnf-parse/`, `bbnf-ir/`, `bbnf-passes/`, `bbnf-vm/` ready for content migration | C | C.W0 |
| Skeletal `bbnf-codegen-ir/` ready for typed-IR landing | D | D.W0 |
| Skeletal `bbnf-codegen/`, `bbnf-runtime-template/`, `bbnf-runtime/`, `bbnf-host/` + 9 per-grammar declaration crates ready for substantive impl | E | E.W0 |
| Sister-crate submodules registered (parse-that, bbnf-regex, csp-solver path-deps) | I | I.W0 (publication prep) |
| Cleared workspace top-level (Cargo.toml, README, GESTALT, .gitignore) | All subsequent tranches | (continuous) |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | partial-honoured | A.W4 (narrative scrub); substantive completion at C.W7 |
| 2 — Layout canon | silent | (deferred to B.W3 directory rename + C.W2 substantive fold) |
| 3 — Cursor + byte-skip | silent | (deferred to C.W6 + E.W4) |
| 4 — Per-domain orthogonal | silent | (deferred to F.W2) |
| 5 — IR + per-backend | silent | (deferred to C.W4 + D.W3) |
| 6 — xtask source emit | partial-honoured | A.W2 (xtask Cargo.toml registers `bbnf-codegen` + `bbnf-runtime-template` path-deps); substantive at E.W6 |
| 7 — `crates/path/` consolidated | partial-honoured | A.W2 (skeleton lands) + A.W4 (partial content movement); substantive completion at C.W6 |
| 8 — Surpass SOTA | n/a | (Tranche A is pre-codegen; perf gates begin at F) |
| 9 — Slice-borrow primary | silent | (deferred to G.W2) |
| 10 — Pratt + SIMD auto-detected | silent | (deferred to F.W3) |
| 11 — Path-deps for sister crates | honoured | A.W2 (path-dep registration) |
| 12 — ser + gorgeous archive | honoured | A.W0 (THE blocking precondition; lands first commit) |
| 13 — No god directories | partial-honoured | A.W2 (skeletal cohesion); substantive completion at C.W6 |
| 14 — Full grammar generalisation | partial-honoured | A.W3 (7 site retirement); substantive completion at E.W6 |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| Lock 12 ceremony silently breaks workspace `members` consumers | A.W0 explicit pre-flight: `rg 'bbnf-ser\|bbnf-gorgeous' crates/` confirms zero internal references; `cargo check --workspace` post-ceremony confirms green; per master plan §13 R1 |
| Commit-chain disposition (Option 3) breaks user's GitHub fork integration | A.W0 verifies the prior chain reachable via `pre-restart-2026-05-03` tag; user retains rollback capability via `git reset --hard pre-restart-2026-05-03`; per §13 R2 |
| `crates/{analysis, lsp}` consolidation breaks editor extension | A.W1 prelude commit verifies the LSP binary builds + extension loads; per §13 R17 |
| docs/ tree restructure breaks cross-references | A.W3 mechanical relocate + grep-based cross-reference fix; per §13 R18 |
| Lock 14 retirement (7 sites) misses one site, allowing grammar-named code to persist | A.W3 final gate runs verification command per-crate; CI gate post-A.close fires the same command on every PR; per §13 R4 |
| Sister-crate submodule operational complexity | A.W1 ratified disposition: submodule-as-workspace-member per `docs/precepts/CONSUMING.md` precedent; operational sequence documented; per §13 R16 |
| `data/` relocation breaks existing tests | A.W5 staged: pre-relocate audit confirms test fixture references; relocate + import-rewrite + `cargo nextest run --workspace` post-relocate |
| Empty crate skeletons compile-fail due to dependency cycle | A.W2 dependency-DAG audit: `cargo tree --workspace` confirms acyclic; skeletal lib.rs uses `pub mod foo;` not `pub use foo::*;` to avoid premature glob imports |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| Workspace clean rebuild after Lock 12 ceremony | ≤ 60s on M1 Pro | `cargo build --workspace --timings`; per `feedback_build-infra-first` |
| Per-crate skeletal `cargo check` | ≤ 5s per crate | `cargo check -p <crate>` per crate; mean across 23 substrate crates |
| Generated-LOC budget | unchanged from baseline (168,750 LOC) | A doesn't touch generated; per master plan §12.2 |
| Cross-reference grep across docs | ≤ 2s | `rg` smoke per master plan §8.2.4 |

## Voice locks

Per `docs/precepts/instructions/STYLE.md` and master plan §14. Calibrated, archaic-permissive register. No metalanguage. Every gate cites a concrete artefact path. The wave specifications drafted by the per-tranche execution agents inherit these locks.

## Closing posture

Hereupon tranche A opens the restart's operational sequence. The substrate's accumulated residue sheds in waves; the workspace reshapes around the locks; the commit-chain provenance preserves; the tranche closes with a workspace skeleton ready to receive the substrate proper. Tranche B's bbnf-error + bbnf-pipeline foundation lands next.

The greenfield mandate carries through tranche A: no quick solutions, no workarounds, no carry-forward of legacy substrates except by explicit ratification per file. Lock 12 fires; Locks 11 + 14 partial-honour; the rest defer with named receivers.
