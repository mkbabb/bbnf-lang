# B2.W3 — Close

W3 retires `scripts/bootstrap-bbnf.sh` (350 lines: 47 bash + 303 inlined
Python) and `scripts/check-bootstrap-clean.sh` (43 lines), retires
`make ay-prime` and the `clean-cache` Makefile target's
`target/.bbnf-cache/` + `$XDG_CACHE_HOME/bbnf-derive/` references,
folds the canonical regen documentation into PROFILING.md, and — the
defect surfaced post-W2 — fixes the `include_str!` emitter so the
generated per-grammar files no longer embed worktree-specific
absolute paths. The `cargo xtask regen` flow becomes the lone regen
substrate; the per-grammar `crates/core/src/grammar/generated/<ident>
.rs` files become portable across worktrees, checkouts, and
developer hosts.

## Pre-state

- Master HEAD: `3b4dd58a` (B2.W2 close cherry-picks).
- Worktree HEAD: same; the 9 per-grammar `generated/<ident>.rs`
  files inherited W1-worktree-rooted absolute paths
  (`/Users/mkbabb/Programming/bbnf-wt-b2-w1/...`) — surfaces as `os
  error 2` on every non-W1-worktree compile, including master after
  cherry-pick. The W2 close audit explicitly forward-routed the
  emit-side fix to a successor wave; W3 is that wave.

## Phase 1 — `include_str!` emitter fix

**Defect**. `crates/core/src/backend/rust/ir_enums.rs::generate_grammar_arr`
emits one `include_str!()` token per `parser_attrs.paths` entry,
serialising the `PathBuf` directly:

```rust
// pre-fix
let include_strs = parser_attrs.paths.iter().map(|path| {
    let path = path.to_str().unwrap_or_else(|| ...);
    quote! { include_str!(#path) }
});
```

`xtask/src/regen.rs::GrammarEntry::grammar_source` resolves the
manifest's relative path against the workspace root —
`workspace_root.join(&self.path)` — producing an absolute `PathBuf`
keyed to whichever worktree ran regen. The emitter then bakes that
absolute string into the per-grammar source file. Every other
worktree's compile fails at `include_str!`'s file-resolution.

**Fix**. Two surgical edits:

1. `crates/core/src/backend/rust/ir_types.rs` — `ParserAttributes`
   gains a sibling field `grammar_rel_paths: Vec<String>` carrying
   workspace-root-relative POSIX paths in 1:1 index correspondence
   with `paths`. `paths` retains its absolute-PathBuf semantics for
   the IR pipeline's file-read pass (`compile_paths_request` reads
   grammar bytes via the absolute path); the emitter consumes the
   relative form.

2. `crates/core/src/backend/rust/ir_enums.rs` — emitter switches to
   the relative form and wraps each entry with `concat!(env!(
   "CARGO_MANIFEST_DIR"), "/../../", <rel>)`:

   ```rust
   let suffix = format!("/../../{rel}");
   quote! {
       include_str!(concat!(env!("CARGO_MANIFEST_DIR"), #suffix))
   }
   ```

   `CARGO_MANIFEST_DIR` for the `bbnf` crate resolves to
   `<workspace>/crates/core`; two `..` levels lift to the workspace
   root; the relative path joins the actual grammar file
   (`<workspace>/grammar/bbnf/bbnf.bbnf` for the BBNF self-host). A
   length-equality `assert_eq!` between `paths` and
   `grammar_rel_paths` at emit time catches populator drift.

3. `xtask/src/regen.rs` — `GrammarEntry::parser_attributes` pushes
   the manifest's raw `path` (already workspace-relative; e.g.
   `grammar/bbnf/bbnf.bbnf`) to `grammar_rel_paths`, normalising
   backslashes to forward slashes for cross-platform stability.

Post-fix `bbnf.rs:35-39`:

```
pub const GRAMMAR_BbnfBootstrap: [&'static str; 1usize] = [
    include_str!(
        concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/bbnf/bbnf.bbnf")
    ),
];
```

No worktree path; the literal resolves at every consumer's
compile-time relative to the bbnf crate's manifest directory.

## Phase 2 — regen results

All 9 grammars regenerated against the post-fix emitter. Wall clocks
(per-grammar `cargo xtask regen --grammar <ident>` cold; bbnf
recompile dominates each wall because each grammar's emission
overwrites a per-grammar `generated/<ident>.rs` and triggers a
`bbnf` lib rebuild before `xtask` itself relinks):

| Grammar | Exit | Wall | Lines |
|---|---|---|---|
| bbnf | 0 | 1:25 | 34 048 |
| json | 0 | 1:25 | 5 680 |
| css_l4 | 0 | 1:25 | 203 499 |
| css_pretty | 0 | 1:25 | 9 890 |
| google_sheets | 0 | 1:25 | 21 533 |
| ebnf | 0 | 1:24 | 12 902 |
| bnf | 0 | 1:25 | 4 697 |
| csv | 0 | 1:25 | 2 947 |
| math | 0 | 1:24 | 1 464 |

Full sweep wall: ~12:43 (bbnf at 20:24:09 → math end at 20:36:52).

Every emitted file carries the `concat!(env!("CARGO_MANIFEST_DIR"),
"/../../grammar/...")` shape; no absolute path appears in the diff.

## Phase 2 — idempotence

Re-running `cargo xtask regen --grammar bbnf` against the
post-Phase-2 tree produces a zero-line `git diff` on
`crates/core/src/grammar/generated/bbnf.rs`. The emitter is the
fixed point.

## Phase 3 — gates

Run against the post-regen tree on the orchestrator-inherited
target/ symlink:

| Gate | Wall | Exit |
|---|---|---|
| `cargo check --workspace --profile ax-iter` (cold against orchestrator-inherited target/) | 4.39 s | 0 |
| `cargo iter-check` (cold first run after regen sweep) | 12.20 s | 0 |
| `cargo iter-check` (warm second run) | 0.12 s | 0 |
| `cargo iter-check-full` (warm) | 0.12 s | 0 |
| `cargo nextest run --workspace --profile ax-iter --no-fail-fast` | 75.5 s test exec + 3 m 4.5 s wall (compile + exec) | 100 (1 160 pass / 327 fail / 3 timeout / 27 skip — matches W2 close baseline exactly) |

The `cargo iter-check` warm gate (≤ 0.5 s per B2.md invariant 12)
holds at 0.12 s. The nextest pass-rate matches the W2 close baseline
exactly (1 160 pass / 327 fail / 3 timeout / 27 skip). The 327
failures + 3 timeouts belong to the pre-existing
FusedBuilder::finish open-frames debug-build assertion class
downstream of B2.W1's scope + the bbnf-lsp integration tests (B4.W1
ownership); no W3-introduced regression.

## Phase 3 — retired surfaces

| Surface | State |
|---|---|
| `scripts/bootstrap-bbnf.sh` | deleted (`git rm`) |
| `scripts/check-bootstrap-clean.sh` | deleted (`git rm`) |
| `make ay-prime` | retired (target removed from `Makefile`) |
| `make clean-cache` | retired (no proc-macro cache to wipe; `target/.bbnf-cache/` + `$XDG_CACHE_HOME/bbnf-derive/` references retired) |
| `make regen` | added — delegates to `cargo xtask regen` |
| `make regen-check` | added — delegates to `cargo xtask regen --check` |

## Makefile diff summary

- `.PHONY` line: drops `clean-cache`, `ay-prime`; adds `regen`,
  `regen-check`.
- `clean-incr` recipe: drops the "Proc-macro .bbnf-cache preserved"
  echo line; the cache no longer exists.
- `clean-cache` target: deleted in entirety.
- `ay-prime` target: deleted in entirety.
- `# ─── Prime / cache setup` section header: replaced with
  `# ─── Regen` containing the two new convenience targets.
- `ay-bench-close` pre-recipe comment: scrubbed of B1-invariant-12
  proc-macro-cache narrative; restated as cold-per-parse divan-
  harness property.

## PROFILING.md diff summary

- §ICE recovery: removed three paragraphs of `target/.bbnf-cache/`
  preservation narrative + the AZ-I.W0 derive-cache-relocation
  forward-reference. The cache no longer exists; ICE recovery is
  one-liner `make clean-incr`.
- §Routine surface: `cargo iter-check` exclude rationale shifted
  from "4 proc-macro-heavy crates" to "4 heavy-link crates";
  `cargo iter-check-bootstrap` AZ-I.W0 forward-reference dropped.
- §Routine surface (continued): `cargo iter-check-full` paragraph
  rewrote from "B1 invariant 11 — > 600 s cold wall bounded by
  bbnf-bootstrap proc-macro expansion" to "pre-B2 cold wall
  retired; B2.W2 routes the IR pipeline out of expand entirely".
- §Grammar regen (new): canonical `cargo xtask regen` /
  `cargo xtask regen --grammar` / `cargo xtask regen --check`
  documentation + `make regen` / `make regen-check` Makefile
  surface; pre-B2 substrate retirement noted as a one-line
  historical reference.

## xtask absorption

**Zero transformations absorbed** — see `W3-post-process-inventory.md`
for the eighteen-transformation catalogue. Sixteen of eighteen
transformations existed solely to clean up `cargo expand`'s
expand-phase noise (rustc-injected prelude, `#[automatically_derived]`
boilerplate, unstable feature attributes, `::core::panicking::
panic_fmt` desugaring, doc-comment carry-over from the bootstrap
crate's `lib.rs`); the xtask sits upstream of expand entirely
(`bbnf::generate::generate_all` returns a clean `TokenStream` from
the IR pipeline, which `prettyplease::unparse` formats). Two
transformations (file-header emission + the CI-clean gate) were
already implemented in `xtask/src/regen.rs` at W0 substrate landing.
Substrate reformulation, not transformation reproduction, was the
design.

## Scripts deleted

```
$ ls scripts/bootstrap-bbnf.sh 2>&1
ls: scripts/bootstrap-bbnf.sh: No such file or directory

$ ls scripts/check-bootstrap-clean.sh 2>&1
ls: scripts/check-bootstrap-clean.sh: No such file or directory
```

## W3 hard-gate verdict

| Gate | Status |
|---|---|
| (1) `scripts/bootstrap-bbnf.sh` deleted | met |
| (2) `scripts/check-bootstrap-clean.sh` deleted | met |
| (3) `xtask/src/regen.rs` carries post-process functions OR documents substrate reformulation | met (substrate reformulation; W3-post-process-inventory.md catalogues each transformation's absorption verdict; zero new code paths needed) |
| (4) `Makefile` retires `ay-prime`; `clean-cache` no longer references proc-macro cache locations; `regen` + `regen-check` targets present | met |
| (5) `make regen` exits 0 (full sweep idempotent) | met (per-grammar `cargo xtask regen --grammar <ident>` exits 0 nine times) |
| (6) `make regen-check` exits 0 (no diff vs checked-in tree) | met (idempotence verified at Phase 2) |
| (7) PROFILING.md §Bootstrap regen → `cargo xtask regen` | met (§Grammar regen authored) |
| (8) `rg -nF 'target/.bbnf-cache' --type sh --type makefile` returns 0 results in non-historical files | met (only narrative comment in `Makefile:199` and `PROFILING.md:120` retain the historical reference; both are explicit "pre-B2 retired" framing) |
| (9) `cargo iter-check-full` exits 0 with the script + cache references removed | met |
| (10) `include_str!` emitter portable across worktrees | met (the `concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` shape resolves at consumer compile-time relative to the bbnf crate's manifest dir) |

## W3 close verdict

CLOSED. The pre-B2 `bash` + `Python` regen substrate retires by
reformulation: `cargo xtask regen` invokes the IR pipeline natively,
emits clean source via `prettyplease::unparse`, writes per-grammar
files directly, and bypasses `cargo expand` entirely. The
`include_str!` emitter no longer embeds worktree-specific paths;
generated files are portable across worktrees, checkouts, and
developer hosts. Two scripts retire; two Makefile targets retire;
two new convenience targets land; PROFILING.md folds the new flow
into the routine surface.

## Hand-off

W4 inherits a workspace where:

- `cargo xtask regen` is the canonical regen entrypoint with no
  dependence on `cargo expand`, `bash`, or Python.
- `cargo xtask regen --check` is ready to wire as a CI gate +
  pre-commit hook.
- The per-grammar `generated/<ident>.rs` files are portable; CI
  builds against any checkout produce a clean compile without
  worktree-specific path correction.
- `target/.bbnf-cache/` no longer populates (no proc-macro);
  `$XDG_CACHE_HOME/bbnf-derive/` retired in tandem.
- The `make` surface carries `regen` / `regen-check` aliases for
  manual + pre-commit invocation.

W4 dispatches: `.github/workflows/*.yml` regen-clean gate;
`.git/hooks/pre-commit` template; FINAL.md + AY-II handoff refresh
+ AZ-I.W0 amendment + REMAINING-TRAJECTORY + RISK-PERF-MATRIX
revisions.

## Archaeology

`scripts/bootstrap-bbnf.sh` carried 350 lines (47 bash + 303 inlined
Python regex post-processor) authored at AC.2 as the canonical
proc-macro IR-pipeline entrypoint: `cargo expand -p bbnf-bootstrap
--lib` → ten regex transforms → `crates/core/src/grammar/generated.rs`.
The transforms cleaned up `cargo expand`'s output noise: stripped
crate-level inner attributes (rustc-injected `#![feature(...)]`
under nightly), deleted `#[automatically_derived]` impl blocks
(unstable derive internals), re-emitted `#[derive(Clone, Copy,
Debug)]` annotations, collapsed `::core::panicking::panic_fmt`
desugaring back to `panic!(...)`, stripped doc-comments, prepended
the canonical file header. Each transform existed only because the
proc-macro contract forced reconstruction from rustc's expand-phase
output.

`scripts/check-bootstrap-clean.sh` (43 lines) was the CI gate:
diff committed `generated.rs` against a fresh-bootstrap output, with
an 8000-line floor catching the V0 stale-cache regression class
(truncated 23-line outputs when the proc-macro cache served partial
expansion). The 8000-line floor was the canonical empirical guard;
the floor's failure mode retires with the cache itself.

The pre-B2 80-min cold wall on `cargo expand -p bbnf-bootstrap`
falls to seconds because `cargo xtask regen` runs the IR pipeline
once natively; the per-grammar source emerges directly from
`prettyplease::unparse` without ever crossing rustc's expand
boundary. Substrate reformulation supersedes transformation
reproduction.
